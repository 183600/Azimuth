// Azimuth Distributed Tracing Consistency Enhanced Test Suite
// This file contains tests for distributed tracing consistency across services

// Test 1: Trace Context Propagation
test "trace context propagation across services" {
  // Trace context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    flags: Int
  }
  
  // Service span structure
  type ServiceSpan = {
    service_name: String,
    operation_name: String,
    trace_context: TraceContext,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // Create trace context
  let create_trace_context = fn(trace_id: String, span_id: String, parent_span_id: Option[String]) {
    {
      trace_id,
      span_id,
      parent_span_id,
      baggage: [],
      flags: 0
    }
  }
  
  // Extract trace context from headers
  let extract_trace_context = fn(headers: Array[(String, String)]) {
    let mut trace_id = ""
    let mut span_id = ""
    let mut parent_span_id = None
    let mut baggage = []
    
    for (key, value) in headers {
      match key {
        "x-trace-id" => trace_id = value
        "x-span-id" => span_id = value
        "x-parent-span-id" => parent_span_id = Some(value)
        "x-baggage" => {
          // Parse baggage items (key=value,key2=value2)
          let items = value.split(",")
          for item in items {
            let parts = item.split("=")
            if parts.length() == 2 {
              baggage = baggage.push((parts[0], parts[1]))
            }
          }
        }
        _ => {}
      }
    }
    
    if trace_id != "" and span_id != "" {
      Some({
        trace_id,
        span_id,
        parent_span_id,
        baggage,
        flags: 0
      })
    } else {
      None
    }
  }
  
  // Inject trace context into headers
  let inject_trace_context = fn(context: TraceContext) {
    let mut headers = []
    
    headers = headers.push(("x-trace-id", context.trace_id))
    headers = headers.push(("x-span-id", context.span_id))
    
    match context.parent_span_id {
      Some(parent_id) => headers = headers.push(("x-parent-span-id", parent_id))
      None => {}
    }
    
    if context.baggage.length() > 0 {
      let baggage_str = context.baggage
        .map(fn((k, v)) { k + "=" + v })
        .reduce(fn(acc, item) { acc + "," + item })
      headers = headers.push(("x-baggage", baggage_str))
    }
    
    headers
  }
  
  // Create service span
  let create_service_span = fn(service_name: String, operation_name: String, context: TraceContext, start_time: Int) {
    {
      service_name,
      operation_name,
      trace_context: context,
      start_time,
      end_time: 0,
      status: "running",
      tags: []
    }
  }
  
  // Complete service span
  let complete_span = fn(span: ServiceSpan, end_time: Int, status: String) {
    {
      span |
      end_time,
      status
    }
  }
  
  // Test trace context creation
  let root_context = create_trace_context("trace-12345", "span-1001", None)
  assert_eq(root_context.trace_id, "trace-12345")
  assert_eq(root_context.span_id, "span-1001")
  assert_eq(root_context.parent_span_id, None)
  
  let child_context = create_trace_context("trace-12345", "span-1002", Some("span-1001"))
  assert_eq(child_context.trace_id, "trace-12345")
  assert_eq(child_context.span_id, "span-1002")
  assert_eq(child_context.parent_span_id, Some("span-1001"))
  
  // Test context injection and extraction
  let headers = inject_trace_context(root_context)
  assert_true(headers.contains(("x-trace-id", "trace-12345")))
  assert_true(headers.contains(("x-span-id", "span-1001")))
  
  let extracted_context = extract_trace_context(headers)
  assert_true(extracted_context.is_some())
  
  let context = extracted_context.unwrap()
  assert_eq(context.trace_id, "trace-12345")
  assert_eq(context.span_id, "span-1001")
  assert_eq(context.parent_span_id, None)
  
  // Test with baggage
  let context_with_baggage = {
    root_context |
    baggage: [("user.id", "user-123"), ("request.id", "req-456")]
  }
  
  let headers_with_baggage = inject_trace_context(context_with_baggage)
  let extracted_with_baggage = extract_trace_context(headers_with_baggage).unwrap()
  
  assert_true(extracted_with_baggage.baggage.contains(("user.id", "user-123")))
  assert_true(extracted_with_baggage.baggage.contains(("request.id", "req-456")))
  
  // Test trace propagation across services
  let api_span = create_service_span("api-service", "process-request", root_context, 1640995200)
  let completed_api_span = complete_span(api_span, 1640995250, "success")
  
  let db_context = create_trace_context("trace-12345", "span-1003", Some("span-1001"))
  let db_span = create_service_span("database-service", "query", db_context, 1640995220)
  let completed_db_span = complete_span(db_span, 1640995240, "success")
  
  let cache_context = create_trace_context("trace-12345", "span-1004", Some("span-1001"))
  let cache_span = create_service_span("cache-service", "get", cache_context, 1640995210)
  let completed_cache_span = complete_span(cache_span, 1640995215, "hit")
  
  // Verify trace consistency
  assert_eq(completed_api_span.trace_context.trace_id, "trace-12345")
  assert_eq(completed_db_span.trace_context.trace_id, "trace-12345")
  assert_eq(completed_cache_span.trace_context.trace_id, "trace-12345")
  
  // Verify parent-child relationships
  assert_eq(completed_api_span.trace_context.parent_span_id, None)
  assert_eq(completed_db_span.trace_context.parent_span_id, Some("span-1001"))
  assert_eq(completed_cache_span.trace_context.parent_span_id, Some("span-1001"))
  
  // Verify timing consistency
  assert_true(completed_api_span.start_time <= completed_db_span.start_time)
  assert_true(completed_api_span.start_time <= completed_cache_span.start_time)
  assert_true(completed_db_span.end_time <= completed_api_span.end_time)
  assert_true(completed_cache_span.end_time <= completed_api_span.end_time)
}

// Test 2: Trace Sampling Strategy
test "trace sampling strategy consistency" {
  // Sampling decision structure
  type SamplingDecision = {
    sampled: Bool,
    decision_reason: String,
    sample_rate: Float
  }
  
  // Sampling strategies
  let always_sample = fn(trace_id: String) {
    {
      sampled: true,
      decision_reason: "always_on",
      sample_rate: 1.0
    }
  }
  
  let never_sample = fn(trace_id: String) {
    {
      sampled: false,
      decision_reason: "always_off",
      sample_rate: 0.0
    }
  }
  
  let probabilistic_sample = fn(trace_id: String, sample_rate: Float) {
    // Simple hash-based sampling for deterministic results
    let hash = trace_id.length() % 100
    let sampled = hash.to_float() < (sample_rate * 100.0)
    
    {
      sampled,
      decision_reason: "probabilistic",
      sample_rate
    }
  }
  
  let rate_limited_sample = fn(trace_id: String, max_traces_per_second: Int, current_count: Int) {
    let sampled = current_count < max_traces_per_second
    
    {
      sampled,
      decision_reason: "rate_limited",
      sample_rate: if sampled { 1.0 } else { 0.0 }
    }
  }
  
  // Test always sampling
  let always_decision = always_sample("trace-12345")
  assert_true(always_decision.sampled)
  assert_eq(always_decision.decision_reason, "always_on")
  assert_eq(always_decision.sample_rate, 1.0)
  
  // Test never sampling
  let never_decision = never_sample("trace-12345")
  assert_false(never_decision.sampled)
  assert_eq(never_decision.decision_reason, "always_off")
  assert_eq(never_decision.sample_rate, 0.0)
  
  // Test probabilistic sampling
  let high_rate_decision = probabilistic_sample("trace-12345", 0.8)
  assert_eq(high_rate_decision.decision_reason, "probabilistic")
  assert_eq(high_rate_decision.sample_rate, 0.8)
  
  let low_rate_decision = probabilistic_sample("trace-12345", 0.1)
  assert_eq(low_rate_decision.decision_reason, "probabilistic")
  assert_eq(low_rate_decision.sample_rate, 0.1)
  
  // Test rate-limited sampling
  let under_limit_decision = rate_limited_sample("trace-12345", 10, 5)
  assert_true(under_limit_decision.sampled)
  assert_eq(under_limit_decision.decision_reason, "rate_limited")
  
  let over_limit_decision = rate_limited_sample("trace-12345", 10, 15)
  assert_false(over_limit_decision.sampled)
  assert_eq(over_limit_decision.decision_reason, "rate_limited")
  
  // Test sampling consistency across services
  let trace_id = "trace-67890"
  let sample_rate = 0.5
  
  // All services should make the same decision for the same trace
  let api_decision = probabilistic_sample(trace_id, sample_rate)
  let db_decision = probabilistic_sample(trace_id, sample_rate)
  let cache_decision = probabilistic_sample(trace_id, sample_rate)
  
  assert_eq(api_decision.sampled, db_decision.sampled)
  assert_eq(db_decision.sampled, cache_decision.sampled)
  assert_eq(api_decision.decision_reason, "probabilistic")
  assert_eq(db_decision.decision_reason, "probabilistic")
  assert_eq(cache_decision.decision_reason, "probabilistic")
  
  // Test with different trace IDs
  let trace_id_1 = "trace-abcde"
  let trace_id_2 = "trace-fghij"
  
  let decision_1 = probabilistic_sample(trace_id_1, 0.5)
  let decision_2 = probabilistic_sample(trace_id_2, 0.5)
  
  // Decisions may be different for different traces
  // (but deterministic for the same trace)
  let decision_1_repeat = probabilistic_sample(trace_id_1, 0.5)
  assert_eq(decision_1.sampled, decision_1_repeat.sampled)
}

// Test 3: Trace Timeline Consistency
test "trace timeline consistency" {
  // Span event structure
  type SpanEvent = {
    timestamp: Int,
    event_name: String,
    attributes: Array[(String, String)]
  }
  
  // Service span structure
  type ServiceSpan = {
    service_name: String,
    operation_name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Int,
    events: Array[SpanEvent],
    status: String
  }
  
  // Validate timeline consistency
  let validate_timeline = fn(spans: Array[ServiceSpan]) {
    let mut issues = []
    
    for span in spans {
      // Check start time <= end time
      if span.start_time > span.end_time {
        issues = issues.push({
          span_id: span.span_id,
          issue: "start_time_after_end_time",
          message: "Start time is after end time"
        })
      }
      
      // Check event timestamps are within span duration
      for event in span.events {
        if event.timestamp < span.start_time or event.timestamp > span.end_time {
          issues = issues.push({
            span_id: span.span_id,
            issue: "event_outside_span",
            message: "Event timestamp is outside span duration"
          })
        }
      }
      
      // Check parent span timeline
      match span.parent_span_id {
        Some(parent_id) => {
          let mut parent_span = None
          for potential_parent in spans {
            if potential_parent.span_id == parent_id {
              parent_span = Some(potential_parent)
              break
            }
          }
          
          match parent_span {
            Some(parent) => {
              // Child span should start after parent starts
              if span.start_time < parent.start_time {
                issues = issues.push({
                  span_id: span.span_id,
                  issue: "child_starts_before_parent",
                  message: "Child span starts before parent span"
                })
              }
              
              // Child span should end before parent ends (for synchronous operations)
              if span.end_time > parent.end_time {
                // This might be OK for async operations, but we'll note it
                issues = issues.push({
                  span_id: span.span_id,
                  issue: "child_ends_after_parent",
                  message: "Child span ends after parent span"
                })
              }
            }
            None => {
              issues = issues.push({
                span_id: span.span_id,
                issue: "parent_not_found",
                message: "Parent span not found"
              })
            }
          }
        }
        None => {}  // Root span, no parent to check
      }
    }
    
    issues
  }
  
  // Create test spans with consistent timeline
  let root_span = {
    service_name: "api-service",
    operation_name: "process-request",
    trace_id: "trace-12345",
    span_id: "span-1001",
    parent_span_id: None,
    start_time: 1640995200,
    end_time: 1640995300,
    events: [
      { timestamp: 1640995210, event_name: "auth-start", attributes: [] },
      { timestamp: 1640995220, event_name: "auth-complete", attributes: [] },
      { timestamp: 1640995250, event_name: "db-query-start", attributes: [] },
      { timestamp: 1640995270, event_name: "db-query-complete", attributes: [] }
    ],
    status: "success"
  }
  
  let child_span = {
    service_name: "database-service",
    operation_name: "query",
    trace_id: "trace-12345",
    span_id: "span-1002",
    parent_span_id: Some("span-1001"),
    start_time: 1640995250,
    end_time: 1640995270,
    events: [
      { timestamp: 1640995255, event_name: "query-exec", attributes: [] },
      { timestamp: 1640995265, event_name: "result-fetch", attributes: [] }
    ],
    status: "success"
  }
  
  let async_span = {
    service_name: "notification-service",
    operation_name: "send-email",
    trace_id: "trace-12345",
    span_id: "span-1003",
    parent_span_id: Some("span-1001"),
    start_time: 1640995280,
    end_time: 1640995350,  // Ends after parent (async operation)
    events: [
      { timestamp: 1640995285, event_name: "email-queued", attributes: [] },
      { timestamp: 1640995340, event_name: "email-sent", attributes: [] }
    ],
    status: "success"
  }
  
  // Test with consistent timeline
  let consistent_spans = [root_span, child_span, async_span]
  let consistent_issues = validate_timeline(consistent_spans)
  
  // Should have one issue: async_span ends after parent
  assert_eq(consistent_issues.length(), 1)
  assert_eq(consistent_issues[0].span_id, "span-1003")
  assert_eq(consistent_issues[0].issue, "child_ends_after_parent")
  
  // Test with inconsistent timeline
  let invalid_root = {
    root_span |
    start_time: 1640995300,
    end_time: 1640995200  // Start after end
  }
  
  let invalid_child = {
    child_span |
    start_time: 1640995100  // Starts before parent
  }
  
  let invalid_event = {
    child_span |
    events: [
      { timestamp: 1640996000, event_name: "future-event", attributes: [] }  // Event after span ends
    ]
  }
  
  let inconsistent_spans = [invalid_root, invalid_child, invalid_event]
  let inconsistent_issues = validate_timeline(inconsistent_spans)
  
  // Should have multiple issues
  assert_true(inconsistent_issues.length() >= 3)
  
  let has_start_after_end = inconsistent_issues.any(fn(issue) { issue.issue == "start_time_after_end_time" })
  let has_child_before_parent = inconsistent_issues.any(fn(issue) { issue.issue == "child_starts_before_parent" })
  let has_event_outside = inconsistent_issues.any(fn(issue) { issue.issue == "event_outside_span" })
  
  assert_true(has_start_after_end)
  assert_true(has_child_before_parent)
  assert_true(has_event_outside)
}

// Test 4: Cross-Service Trace Correlation
test "cross-service trace correlation" {
  // Correlation context
  type CorrelationContext = {
    correlation_id: String,
    user_id: Option[String],
    session_id: Option[String],
    request_id: String,
    business_transaction: String
  }
  
  // Service span with correlation
  type CorrelatedSpan = {
    service_name: String,
    operation_name: String,
    trace_id: String,
    span_id: String,
    correlation_context: CorrelationContext,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // Create correlation context
  let create_correlation_context = fn(correlation_id: String, request_id: String, business_transaction: String) {
    {
      correlation_id,
      user_id: None,
      session_id: None,
      request_id,
      business_transaction
    }
  }
  
  // Add user context
  let with_user_context = fn(context: CorrelationContext, user_id: String, session_id: String) {
    {
      context |
      user_id: Some(user_id),
      session_id: Some(session_id)
    }
  }
  
  // Extract correlation from headers
  let extract_correlation = fn(headers: Array[(String, String)]) {
    let mut correlation_id = ""
    let mut user_id = None
    let mut session_id = None
    let mut request_id = ""
    let mut business_transaction = ""
    
    for (key, value) in headers {
      match key {
        "x-correlation-id" => correlation_id = value
        "x-user-id" => user_id = Some(value)
        "x-session-id" => session_id = Some(value)
        "x-request-id" => request_id = value
        "x-business-transaction" => business_transaction = value
        _ => {}
      }
    }
    
    if correlation_id != "" and request_id != "" {
      Some({
        correlation_id,
        user_id,
        session_id,
        request_id,
        business_transaction
      })
    } else {
      None
    }
  }
  
  // Inject correlation into headers
  let inject_correlation = fn(context: CorrelationContext) {
    let mut headers = []
    
    headers = headers.push(("x-correlation-id", context.correlation_id))
    headers = headers.push(("x-request-id", context.request_id))
    
    if context.business_transaction != "" {
      headers = headers.push(("x-business-transaction", context.business_transaction))
    }
    
    match context.user_id {
      Some(user_id) => headers = headers.push(("x-user-id", user_id))
      None => {}
    }
    
    match context.session_id {
      Some(session_id) => headers = headers.push(("x-session-id", session_id))
      None => {}
    }
    
    headers
  }
  
  // Test correlation context creation
  let base_context = create_correlation_context("corr-12345", "req-67890", "order-processing")
  assert_eq(base_context.correlation_id, "corr-12345")
  assert_eq(base_context.request_id, "req-67890")
  assert_eq(base_context.business_transaction, "order-processing")
  assert_eq(base_context.user_id, None)
  assert_eq(base_context.session_id, None)
  
  let user_context = with_user_context(base_context, "user-111", "session-222")
  assert_eq(user_context.user_id, Some("user-111"))
  assert_eq(user_context.session_id, Some("session-222"))
  
  // Test correlation injection and extraction
  let headers = inject_correlation(user_context)
  assert_true(headers.contains(("x-correlation-id", "corr-12345")))
  assert_true(headers.contains(("x-request-id", "req-67890")))
  assert_true(headers.contains(("x-business-transaction", "order-processing")))
  assert_true(headers.contains(("x-user-id", "user-111")))
  assert_true(headers.contains(("x-session-id", "session-222")))
  
  let extracted_context = extract_correlation(headers)
  assert_true(extracted_context.is_some())
  
  let context = extracted_context.unwrap()
  assert_eq(context.correlation_id, "corr-12345")
  assert_eq(context.request_id, "req-67890")
  assert_eq(context.business_transaction, "order-processing")
  assert_eq(context.user_id, Some("user-111"))
  assert_eq(context.session_id, Some("session-222"))
  
  // Test cross-service correlation consistency
  let api_span = {
    service_name: "api-service",
    operation_name: "create-order",
    trace_id: "trace-12345",
    span_id: "span-1001",
    correlation_context: user_context,
    start_time: 1640995200,
    end_time: 1640995250,
    status: "success"
  }
  
  let order_span = {
    service_name: "order-service",
    operation_name: "process-order",
    trace_id: "trace-12345",
    span_id: "span-1002",
    correlation_context: user_context,
    start_time: 1640995220,
    end_time: 1640995240,
    status: "success"
  }
  
  let payment_span = {
    service_name: "payment-service",
    operation_name: "process-payment",
    trace_id: "trace-12345",
    span_id: "span-1003",
    correlation_context: user_context,
    start_time: 1640995230,
    end_time: 1640995235,
    status: "success"
  }
  
  let spans = [api_span, order_span, payment_span]
  
  // Verify correlation consistency across services
  for span in spans {
    assert_eq(span.correlation_context.correlation_id, "corr-12345")
    assert_eq(span.correlation_context.request_id, "req-67890")
    assert_eq(span.correlation_context.business_transaction, "order-processing")
    assert_eq(span.correlation_context.user_id, Some("user-111"))
    assert_eq(span.correlation_context.session_id, Some("session-222"))
  }
  
  // Test correlation with missing fields
  let partial_context = create_correlation_context("corr-54321", "req-09876", "")
  let partial_headers = inject_correlation(partial_context)
  let extracted_partial = extract_correlation(partial_headers).unwrap()
  
  assert_eq(extracted_partial.correlation_id, "corr-54321")
  assert_eq(extracted_partial.request_id, "req-09876")
  assert_eq(extracted_partial.business_transaction, "")
  assert_eq(extracted_partial.user_id, None)
  assert_eq(extracted_partial.session_id, None)
}

// Test 5: Trace Consistency Validation
test "trace consistency validation" {
  // Validation issue type
  type ValidationIssue = {
    severity: String,
    code: String,
    message: String,
    affected_span_ids: Array[String]
  }
  
  // Service span structure
  type ServiceSpan = {
    service_name: String,
    operation_name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // Validate trace consistency
  let validate_trace_consistency = fn(spans: Array[ServiceSpan]) {
    let mut issues = []
    
    // Check 1: All spans should have the same trace_id
    let trace_ids = spans.map(fn(s) { s.trace_id }).unique()
    if trace_ids.length() > 1 {
      issues = issues.push({
        severity: "error",
        code: "multiple_trace_ids",
        message: "Trace contains multiple trace IDs: " + trace_ids.join(", "),
        affected_span_ids: spans.map(fn(s) { s.span_id })
      })
    }
    
    // Check 2: Parent-child relationships should be valid
    for span in spans {
      match span.parent_span_id {
        Some(parent_id) => {
          let parent_exists = spans.any(fn(s) { s.span_id == parent_id })
          if not(parent_exists) {
            issues = issues.push({
              severity: "warning",
              code: "missing_parent",
              message: "Span references non-existent parent: " + parent_id,
              affected_span_ids: [span.span_id]
            })
          }
        }
        None => {}  // Root span
      }
    }
    
    // Check 3: There should be exactly one root span
    let root_spans = spans.filter(fn(s) { s.parent_span_id.is_none() })
    if root_spans.length() == 0 {
      issues = issues.push({
        severity: "error",
        code: "no_root_span",
        message: "Trace has no root span",
        affected_span_ids: []
      })
    } else if root_spans.length() > 1 {
      issues = issues.push({
        severity: "warning",
        code: "multiple_root_spans",
        message: "Trace has multiple root spans",
        affected_span_ids: root_spans.map(fn(s) { s.span_id })
      })
    }
    
    // Check 4: No circular dependencies
    let has_circular_dependency = fn(span_id: String, visited: Array[String]) {
      if visited.contains(span_id) {
        return true
      }
      
      let span = spans.find(fn(s) { s.span_id == span_id })
      match span {
        Some(s) => {
          match s.parent_span_id {
            Some(parent_id) => has_circular_dependency(parent_id, visited.push(span_id))
            None => false
          }
        }
        None => false
      }
    }
    
    for span in spans {
      if has_circular_dependency(span.span_id, []) {
        issues = issues.push({
          severity: "error",
          code: "circular_dependency",
          message: "Circular dependency detected in span hierarchy",
          affected_span_ids: [span.span_id]
        })
        break
      }
    }
    
    // Check 5: Timing consistency
    for span in spans {
      if span.start_time > span.end_time {
        issues = issues.push({
          severity: "error",
          code: "invalid_timing",
          message: "Span start time is after end time",
          affected_span_ids: [span.span_id]
        })
      }
      
      // Check parent-child timing
      match span.parent_span_id {
        Some(parent_id) => {
          let parent_span = spans.find(fn(s) { s.span_id == parent_id })
          match parent_span {
            Some(parent) => {
              if span.start_time < parent.start_time {
                issues = issues.push({
                  severity: "warning",
                  code: "child_before_parent",
                  message: "Child span starts before parent span",
                  affected_span_ids: [span.span_id, parent.span_id]
                })
              }
            }
            None => {}  // Already caught by missing parent check
          }
        }
        None => {}
      }
    }
    
    issues
  }
  
  // Create a consistent trace
  let consistent_spans = [
    {
      service_name: "api-service",
      operation_name: "process-request",
      trace_id: "trace-12345",
      span_id: "span-1001",
      parent_span_id: None,
      start_time: 1640995200,
      end_time: 1640995300,
      status: "success",
      tags: []
    },
    {
      service_name: "database-service",
      operation_name: "query",
      trace_id: "trace-12345",
      span_id: "span-1002",
      parent_span_id: Some("span-1001"),
      start_time: 1640995220,
      end_time: 1640995240,
      status: "success",
      tags: []
    },
    {
      service_name: "cache-service",
      operation_name: "get",
      trace_id: "trace-12345",
      span_id: "span-1003",
      parent_span_id: Some("span-1001"),
      start_time: 1640995210,
      end_time: 1640995215,
      status: "hit",
      tags: []
    }
  ]
  
  // Test validation of consistent trace
  let consistent_issues = validate_trace_consistency(consistent_spans)
  assert_eq(consistent_issues.length(), 0)
  
  // Create an inconsistent trace with multiple issues
  let inconsistent_spans = [
    {
      service_name: "api-service",
      operation_name: "process-request",
      trace_id: "trace-12345",
      span_id: "span-1001",
      parent_span_id: None,
      start_time: 1640995300,  // Start after end
      end_time: 1640995200,
      status: "success",
      tags: []
    },
    {
      service_name: "database-service",
      operation_name: "query",
      trace_id: "trace-67890",  // Different trace ID
      span_id: "span-1002",
      parent_span_id: Some("span-1001"),
      start_time: 1640995220,
      end_time: 1640995240,
      status: "success",
      tags: []
    },
    {
      service_name: "cache-service",
      operation_name: "get",
      trace_id: "trace-12345",
      span_id: "span-1003",
      parent_span_id: Some("span-9999"),  // Non-existent parent
      start_time: 1640995210,
      end_time: 1640995215,
      status: "hit",
      tags: []
    }
  ]
  
  // Test validation of inconsistent trace
  let inconsistent_issues = validate_trace_consistency(inconsistent_spans)
  
  // Should have multiple issues
  assert_true(inconsistent_issues.length() >= 3)
  
  let has_multiple_trace_ids = inconsistent_issues.any(fn(issue) { issue.code == "multiple_trace_ids" })
  let has_missing_parent = inconsistent_issues.any(fn(issue) { issue.code == "missing_parent" })
  let has_invalid_timing = inconsistent_issues.any(fn(issue) { issue.code == "invalid_timing" })
  
  assert_true(has_multiple_trace_ids)
  assert_true(has_missing_parent)
  assert_true(has_invalid_timing)
  
  // Test with empty trace
  let empty_issues = validate_trace_consistency([])
  assert_eq(empty_issues.length(), 1)
  assert_eq(empty_issues[0].code, "no_root_span")
  
  // Test with multiple root spans
  let multiple_root_spans = [
    {
      service_name: "api-service",
      operation_name: "process-request",
      trace_id: "trace-12345",
      span_id: "span-1001",
      parent_span_id: None,
      start_time: 1640995200,
      end_time: 1640995300,
      status: "success",
      tags: []
    },
    {
      service_name: "web-service",
      operation_name: "handle-request",
      trace_id: "trace-12345",
      span_id: "span-1002",
      parent_span_id: None,
      start_time: 1640995250,
      end_time: 1640995350,
      status: "success",
      tags: []
    }
  ]
  
  let multiple_root_issues = validate_trace_consistency(multiple_root_spans)
  assert_eq(multiple_root_issues.length(), 1)
  assert_eq(multiple_root_issues[0].code, "multiple_root_spans")
}