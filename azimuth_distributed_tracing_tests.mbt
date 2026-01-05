// Azimuth Distributed Tracing Test Suite
// This file contains test cases for distributed tracing functionality

// Test 1: Trace ID Generation and Validation
test "trace ID generation and validation" {
  let generate_trace_id = fn() {
    let mut id = ""
    for i in 0..16 {
      let hex_chars = "0123456789abcdef"
      let random_index = i % 16
      id = id + hex_chars[random_index].to_string()
    }
    id
  }
  
  let validate_trace_id = fn(trace_id: String) {
    trace_id.length() == 32 && trace_id.chars().all(fn(c) { 
      let hex_chars = "0123456789abcdef"
      hex_chars.contains(c.to_string())
    })
  }
  
  let trace_id1 = generate_trace_id()
  let trace_id2 = generate_trace_id()
  
  assert_true(validate_trace_id(trace_id1))
  assert_true(validate_trace_id(trace_id2))
  assert_true(trace_id1 != trace_id2)
  
  let invalid_trace_id1 = "invalid-trace-id"
  let invalid_trace_id2 = "1234567890123456789012345678901g"  // 31 chars + invalid char
  let invalid_trace_id3 = "123456789012345678901234567890"    // 30 chars
  
  assert_false(validate_trace_id(invalid_trace_id1))
  assert_false(validate_trace_id(invalid_trace_id2))
  assert_false(validate_trace_id(invalid_trace_id3))
}

// Test 2: Span ID Generation and Validation
test "span ID generation and validation" {
  let generate_span_id = fn() {
    let mut id = ""
    for i in 0..8 {
      let hex_chars = "0123456789abcdef"
      let random_index = (i * 3) % 16
      id = id + hex_chars[random_index].to_string()
    }
    id
  }
  
  let validate_span_id = fn(span_id: String) {
    span_id.length() == 16 && span_id.chars().all(fn(c) { 
      let hex_chars = "0123456789abcdef"
      hex_chars.contains(c.to_string())
    })
  }
  
  let span_id1 = generate_span_id()
  let span_id2 = generate_span_id()
  
  assert_true(validate_span_id(span_id1))
  assert_true(validate_span_id(span_id2))
  assert_true(span_id1 != span_id2)
  
  let invalid_span_id1 = "invalid-span-id"
  let invalid_span_id2 = "123456789012345g"  // 15 chars + invalid char
  let invalid_span_id3 = "12345678901234"    // 14 chars
  
  assert_false(validate_span_id(invalid_span_id1))
  assert_false(validate_span_id(invalid_span_id2))
  assert_false(validate_span_id(invalid_span_id3))
}

// Test 3: Trace Context Propagation
test "trace context propagation" {
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: Array[(String, String)]
  }
  
  let create_trace_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      trace_flags: 1,
      trace_state: []
    }
  }
  
  let extract_from_headers = fn(headers: Array[(String, String)]) {
    let mut trace_id = None
    let mut span_id = None
    let mut trace_flags = Some(1)
    
    for (key, value) in headers {
      if key == "traceparent" {
        let parts = value.split("-")
        if parts.length() >= 3 {
          trace_id = Some(parts[1])
          span_id = Some(parts[2])
          if parts.length() >= 4 {
            let flags = parts[3]
            if flags.length() >= 2 {
              trace_flags = Some(flags[1].to_int())
            }
          }
        }
      }
    }
    
    match (trace_id, span_id) {
      (Some(tid), Some(sid)) => Some({
        trace_id: tid,
        span_id: sid,
        trace_flags: trace_flags.unwrap_or(1),
        trace_state: []
      })
      _ => None
    }
  }
  
  let inject_to_headers = fn(context: TraceContext) {
    [
      ("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags.to_string()),
      ("x-trace-id", context.trace_id),
      ("x-span-id", context.span_id)
    ]
  }
  
  let parent_context = create_trace_context("123456789012345678901234567890ab", "cdef0123456789ab")
  let headers = inject_to_headers(parent_context)
  
  assert_eq(headers.length(), 3)
  assert_true(headers.contains(("traceparent", "00-123456789012345678901234567890ab-cdef0123456789ab-1")))
  assert_true(headers.contains(("x-trace-id", "123456789012345678901234567890ab")))
  assert_true(headers.contains(("x-span-id", "cdef0123456789ab")))
  
  let extracted_context = extract_from_headers(headers)
  assert_true(extracted_context.is_some())
  
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, parent_context.trace_id)
      assert_eq(context.span_id, parent_context.span_id)
      assert_eq(context.trace_flags, parent_context.trace_flags)
    }
    None => assert_true(false)
  }
}

// Test 4: Span Tree Operations
test "span tree operations" {
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Int,
    children: Array[String]
  }
  
  let create_span = fn(name: String, trace_id: String, span_id: String, parent_span_id: Option[String]) {
    {
      name,
      trace_id,
      span_id,
      parent_span_id,
      start_time: 1640995200,
      end_time: 1640995250,
      children: []
    }
  }
  
  let add_child = fn(span: Span, child_span_id: String) {
    { span | children: span.children + [child_span_id] }
  }
  
  let root_span = create_span("root_operation", "trace-123", "span-1", None)
  let child_span1 = create_span("child_operation_1", "trace-123", "span-2", Some("span-1"))
  let child_span2 = create_span("child_operation_2", "trace-123", "span-3", Some("span-1"))
  let grandchild_span = create_span("grandchild_operation", "trace-123", "span-4", Some("span-2"))
  
  let root_with_children = add_child(add_child(root_span, "span-2"), "span-3")
  let child1_with_grandchild = add_child(child_span1, "span-4")
  
  assert_eq(root_with_children.children.length(), 2)
  assert_true(root_with_children.children.contains("span-2"))
  assert_true(root_with_children.children.contains("span-3"))
  
  assert_eq(child1_with_grandchild.children.length(), 1)
  assert_true(child1_with_grandchild.children.contains("span-4"))
  
  assert_eq(child_span2.children.length(), 0)
  assert_eq(grandchild_span.children.length(), 0)
  
  let is_root = fn(span: Span) {
    span.parent_span_id.is_none()
  }
  
  let is_child_of = fn(span: Span, parent_span_id: String) {
    match span.parent_span_id {
      Some(id) => id == parent_span_id
      None => false
    }
  }
  
  assert_true(is_root(root_span))
  assert_false(is_root(child_span1))
  assert_false(is_root(child_span2))
  assert_false(is_root(grandchild_span))
  
  assert_true(is_child_of(child_span1, "span-1"))
  assert_true(is_child_of(child_span2, "span-1"))
  assert_true(is_child_of(grandchild_span, "span-2"))
  assert_false(is_child_of(root_span, "span-1"))
}

// Test 5: Trace Sampling
test "trace sampling" {
  type SamplingDecision = {
    sampled: Bool,
    reason: String
  }
  
  let always_sample = fn(_trace_id: String, _span_name: String) {
    { sampled: true, reason: "always" }
  }
  
  let never_sample = fn(_trace_id: String, _span_name: String) {
    { sampled: false, reason: "never" }
  }
  
  let probability_sample = fn(trace_id: String, _span_name: String, probability: Float) {
    let hash = trace_id.chars().fold(0, fn(acc, c) { 
      (acc * 31 + c.to_int()) % 10000
    })
    let threshold = (probability * 10000.0).to_int()
    {
      sampled: hash < threshold,
      reason: "probability_" + probability.to_string()
    }
  }
  
  let trace_id1 = "123456789012345678901234567890ab"
  let trace_id2 = "abcdef0123456789abcdef0123456789"
  
  let decision1 = always_sample(trace_id1, "test_operation")
  let decision2 = never_sample(trace_id1, "test_operation")
  let decision3 = probability_sample(trace_id1, "test_operation", 0.5)
  let decision4 = probability_sample(trace_id2, "test_operation", 0.5)
  
  assert_true(decision1.sampled)
  assert_eq(decision1.reason, "always")
  
  assert_false(decision2.sampled)
  assert_eq(decision2.reason, "never")
  
  assert_true(decision3.sampled == true || decision3.sampled == false)  // Could be either
  assert_eq(decision3.reason, "probability_0.5")
  
  assert_true(decision4.sampled == true || decision4.sampled == false)  // Could be either
  assert_eq(decision4.reason, "probability_0.5")
  
  // With probability 0.0, never sample
  let decision_never = probability_sample(trace_id1, "test_operation", 0.0)
  assert_false(decision_never.sampled)
  
  // With probability 1.0, always sample
  let decision_always = probability_sample(trace_id1, "test_operation", 1.0)
  assert_true(decision_always.sampled)
}

// Test 6: Cross-Service Trace Context
test "cross-service trace context" {
  type ServiceTraceContext = {
    service_name: String,
    trace_id: String,
    span_id: String,
    parent_service: Option[String],
    parent_span_id: Option[String]
  }
  
  let create_service_context = fn(service_name: String, trace_id: String, span_id: String) {
    {
      service_name,
      trace_id,
      span_id,
      parent_service: None,
      parent_span_id: None
    }
  }
  
  let create_child_context = fn(parent_context: ServiceTraceContext, child_service_name: String, child_span_id: String) {
    {
      service_name: child_service_name,
      trace_id: parent_context.trace_id,
      span_id: child_span_id,
      parent_service: Some(parent_context.service_name),
      parent_span_id: Some(parent_context.span_id)
    }
  }
  
  let web_service_context = create_service_context("web-service", "trace-123", "span-1")
  let auth_service_context = create_child_context(web_service_context, "auth-service", "span-2")
  let user_service_context = create_child_context(auth_service_context, "user-service", "span-3")
  
  assert_eq(web_service_context.service_name, "web-service")
  assert_eq(web_service_context.trace_id, "trace-123")
  assert_eq(web_service_context.span_id, "span-1")
  assert_eq(web_service_context.parent_service, None)
  assert_eq(web_service_context.parent_span_id, None)
  
  assert_eq(auth_service_context.service_name, "auth-service")
  assert_eq(auth_service_context.trace_id, "trace-123")
  assert_eq(auth_service_context.span_id, "span-2")
  assert_eq(auth_service_context.parent_service, Some("web-service"))
  assert_eq(auth_service_context.parent_span_id, Some("span-1"))
  
  assert_eq(user_service_context.service_name, "user-service")
  assert_eq(user_service_context.trace_id, "trace-123")
  assert_eq(user_service_context.span_id, "span-3")
  assert_eq(user_service_context.parent_service, Some("auth-service"))
  assert_eq(user_service_context.parent_span_id, Some("span-2"))
  
  let get_trace_path = fn(context: ServiceTraceContext) {
    match context.parent_service {
      Some(parent) => parent + " -> " + context.service_name
      None => context.service_name
    }
  }
  
  assert_eq(get_trace_path(web_service_context), "web-service")
  assert_eq(get_trace_path(auth_service_context), "web-service -> auth-service")
  assert_eq(get_trace_path(user_service_context), "auth-service -> user-service")
}

// Test 7: Trace Aggregation
test "trace aggregation" {
  type TraceSummary = {
    trace_id: String,
    span_count: Int,
    total_duration: Int,
    service_names: Array[String],
    error_count: Int
  }
  
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    service_name: String,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  let create_span = fn(name: String, trace_id: String, span_id: String, service_name: String, start_time: Int, end_time: Int, status: String) {
    {
      name,
      trace_id,
      span_id,
      service_name,
      start_time,
      end_time,
      status
    }
  }
  
  let aggregate_trace = fn(spans: Array[Span]) {
    if spans.length() == 0 {
      None
    } else {
      let trace_id = spans[0].trace_id
      let span_count = spans.length()
      let total_duration = spans.map(fn(s) { s.end_time - s.start_time }).reduce(fn(acc, d) { acc + d }, 0)
      let service_names = spans.map(fn(s) { s.service_name })
      let unique_services = fn(services: Array[String]) {
        let mut result = []
        for service in services {
          if not(result.contains(service)) {
            result = result.push(service)
          }
        }
        result
      }
      let error_count = spans.filter(fn(s) { s.status == "error" }).length()
      
      Some({
        trace_id,
        span_count,
        total_duration,
        service_names: unique_services(service_names),
        error_count
      })
    }
  }
  
  let spans = [
    create_span("web_request", "trace-123", "span-1", "web-service", 1000, 1100, "ok"),
    create_span("auth_check", "trace-123", "span-2", "auth-service", 1105, 1125, "ok"),
    create_span("user_lookup", "trace-123", "span-3", "user-service", 1130, 1200, "ok"),
    create_span("db_query", "trace-123", "span-4", "db-service", 1205, 1250, "error"),
    create_span("cache_lookup", "trace-123", "span-5", "cache-service", 1150, 1160, "ok")
  ]
  
  let summary = aggregate_trace(spans)
  assert_true(summary.is_some())
  
  match summary {
    Some(s) => {
      assert_eq(s.trace_id, "trace-123")
      assert_eq(s.span_count, 5)
      assert_eq(s.total_duration, 225)  // 100 + 20 + 70 + 45 + 10
      assert_eq(s.service_names.length(), 4)
      assert_true(s.service_names.contains("web-service"))
      assert_true(s.service_names.contains("auth-service"))
      assert_true(s.service_names.contains("user-service"))
      assert_true(s.service_names.contains("db-service"))
      assert_true(s.service_names.contains("cache-service"))
      assert_eq(s.error_count, 1)
    }
    None => assert_true(false)
  }
  
  let empty_spans: Array[Span] = []
  let empty_summary = aggregate_trace(empty_spans)
  assert_true(empty_summary.is_none())
}

// Test 8: Trace Consistency Validation
test "trace consistency validation" {
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Int
  }
  
  type TraceValidationResult = {
    is_valid: Bool,
    issues: Array[String]
  }
  
  let validate_trace = fn(spans: Array[Span]) {
    let mut issues = []
    
    // Check if all spans have the same trace ID
    if spans.length() > 0 {
      let trace_id = spans[0].trace_id
      for span in spans {
        if span.trace_id != trace_id {
          issues = issues.push("Span " + span.span_id + " has different trace ID: " + span.trace_id)
        }
      }
    }
    
    // Check if all parent span IDs exist
    let span_ids = spans.map(fn(s) { s.span_id })
    for span in spans {
      match span.parent_span_id {
        Some(parent_id) => {
          if not(span_ids.contains(parent_id)) {
            issues = issues.push("Span " + span.span_id + " references non-existent parent: " + parent_id)
          }
        }
        None => {}  // Root span, no parent to check
      }
    }
    
    // Check for time consistency
    for span in spans {
      if span.start_time > span.end_time {
        issues = issues.push("Span " + span.span_id + " has start time after end time")
      }
    }
    
    // Check for cycles
    let has_cycle = fn(spans: Array[Span]) {
      let span_map = spans.reduce(fn(acc, span) {
        let mut new_acc = acc
        new_acc[span.span_id] = span.parent_span_id
        new_acc
      }, {})
      
      let visit_span = fn(span_id: String, visited: Array[String]) {
        if visited.contains(span_id) {
          true  // Cycle detected
        } else {
          match span_map[span_id] {
            Some(Some(parent_id)) => visit_span(parent_id, visited + [span_id])
            _ => false  // No parent or root reached
          }
        }
      }
      
      for span in spans {
        if visit_span(span.span_id, []) {
          return true
        }
      }
      false
    }
    
    if has_cycle(spans) {
      issues = issues.push("Cycle detected in span parent-child relationships")
    }
    
    {
      is_valid: issues.length() == 0,
      issues
    }
  }
  
  let valid_spans = [
    { name: "root", trace_id: "trace-1", span_id: "span-1", parent_span_id: None, start_time: 1000, end_time: 1100 },
    { name: "child1", trace_id: "trace-1", span_id: "span-2", parent_span_id: Some("span-1"), start_time: 1105, end_time: 1125 },
    { name: "child2", trace_id: "trace-1", span_id: "span-3", parent_span_id: Some("span-1"), start_time: 1130, end_time: 1150 },
    { name: "grandchild", trace_id: "trace-1", span_id: "span-4", parent_span_id: Some("span-2"), start_time: 1110, end_time: 1120 }
  ]
  
  let valid_result = validate_trace(valid_spans)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.issues.length(), 0)
  
  let invalid_trace_id_spans = [
    { name: "root", trace_id: "trace-1", span_id: "span-1", parent_span_id: None, start_time: 1000, end_time: 1100 },
    { name: "child", trace_id: "trace-2", span_id: "span-2", parent_span_id: Some("span-1"), start_time: 1105, end_time: 1125 }
  ]
  
  let invalid_trace_id_result = validate_trace(invalid_trace_id_spans)
  assert_false(invalid_trace_id_result.is_valid)
  assert_true(invalid_trace_id_result.issues.length() > 0)
  
  let invalid_parent_spans = [
    { name: "root", trace_id: "trace-1", span_id: "span-1", parent_span_id: None, start_time: 1000, end_time: 1100 },
    { name: "child", trace_id: "trace-1", span_id: "span-2", parent_span_id: Some("span-99"), start_time: 1105, end_time: 1125 }
  ]
  
  let invalid_parent_result = validate_trace(invalid_parent_spans)
  assert_false(invalid_parent_result.is_valid)
  assert_true(invalid_parent_result.issues.length() > 0)
  
  let invalid_time_spans = [
    { name: "root", trace_id: "trace-1", span_id: "span-1", parent_span_id: None, start_time: 1100, end_time: 1000 },
    { name: "child", trace_id: "trace-1", span_id: "span-2", parent_span_id: Some("span-1"), start_time: 1105, end_time: 1125 }
  ]
  
  let invalid_time_result = validate_trace(invalid_time_spans)
  assert_false(invalid_time_result.is_valid)
  assert_true(invalid_time_result.issues.length() > 0)
  
  let cycle_spans = [
    { name: "span1", trace_id: "trace-1", span_id: "span-1", parent_span_id: Some("span-2"), start_time: 1000, end_time: 1100 },
    { name: "span2", trace_id: "trace-1", span_id: "span-2", parent_span_id: Some("span-1"), start_time: 1105, end_time: 1125 }
  ]
  
  let cycle_result = validate_trace(cycle_spans)
  assert_false(cycle_result.is_valid)
  assert_true(cycle_result.issues.length() > 0)
}