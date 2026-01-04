// Azimuth Premium Distributed Tracing Consistency Tests
// This file contains comprehensive test cases for distributed tracing consistency across services

// Test 1: Trace Context Propagation
test "trace context propagation across services" {
  // Define trace context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String,
    baggage: Array[(String, String)]
  }
  
  // Define service span structure
  type ServiceSpan = {
    service_name: String,
    operation_name: String,
    trace_context: TraceContext,
    start_time: Int,
    end_time: Int,
    status: String,
    events: Array[(String, Int, Array[(String, String)])]
  }
  
  // Create initial trace context
  let initial_context = {
    trace_id: "a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6",
    span_id: "1111111111111111",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: "rojo=00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",
    baggage: [("user.id", "12345"), ("request.id", "req-67890")]
  }
  
  // Service A creates a span
  let service_a_span = {
    service_name: "service-a",
    operation_name: "process-request",
    trace_context: initial_context,
    start_time: 1640995200000,
    end_time: 1640995200200,
    status: "ok",
    events: []
  }
  
  // Service B receives the trace context and creates a child span
  let service_b_context = {
    trace_id: initial_context.trace_id,
    span_id: "2222222222222222",
    parent_span_id: Some(initial_context.span_id),
    trace_flags: initial_context.trace_flags,
    trace_state: initial_context.trace_state,
    baggage: initial_context.baggage
  }
  
  let service_b_span = {
    service_name: "service-b",
    operation_name: "validate-data",
    trace_context: service_b_context,
    start_time: 1640995200100,
    end_time: 1640995200150,
    status: "ok",
    events: [
      ("validation.start", 1640995200100, []),
      ("validation.complete", 1640995200150, [("records.validated", "42")])
    ]
  }
  
  // Service C receives the trace context and creates another child span
  let service_c_context = {
    trace_id: initial_context.trace_id,
    span_id: "3333333333333333",
    parent_span_id: Some(service_b_context.span_id),
    trace_flags: initial_context.trace_flags,
    trace_state: initial_context.trace_state,
    baggage: initial_context.baggage.push(("processing.mode", "batch"))
  }
  
  let service_c_span = {
    service_name: "service-c",
    operation_name: "transform-data",
    trace_context: service_c_context,
    start_time: 1640995200120,
    end_time: 1640995200180,
    status: "ok",
    events: [
      ("transformation.start", 1640995200120, []),
      ("transformation.complete", 1640995200180, [("records.transformed", "42")])
    ]
  }
  
  // Verify trace consistency
  assert_eq(service_a_span.trace_context.trace_id, service_b_span.trace_context.trace_id)
  assert_eq(service_b_span.trace_context.trace_id, service_c_span.trace_context.trace_id)
  
  // Verify parent-child relationships
  match service_b_span.trace_context.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, service_a_span.trace_context.span_id)
    None => assert_true(false)
  }
  
  match service_c_span.trace_context.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, service_b_span.trace_context.span_id)
    None => assert_true(false)
  }
  
  // Verify trace flags consistency
  assert_eq(service_a_span.trace_context.trace_flags, service_b_span.trace_context.trace_flags)
  assert_eq(service_b_span.trace_context.trace_flags, service_c_span.trace_context.trace_flags)
  
  // Verify trace state consistency
  assert_eq(service_a_span.trace_context.trace_state, service_b_span.trace_context.trace_state)
  assert_eq(service_b_span.trace_context.trace_state, service_c_span.trace_context.trace_state)
  
  // Verify baggage propagation
  assert_true(service_b_span.trace_context.baggage.contains(("user.id", "12345")))
  assert_true(service_c_span.trace_context.baggage.contains(("user.id", "12345")))
  assert_true(service_c_span.trace_context.baggage.contains(("processing.mode", "batch")))
  
  // Verify temporal consistency
  assert_true(service_a_span.start_time <= service_b_span.start_time)
  assert_true(service_b_span.start_time <= service_c_span.start_time)
  assert_true(service_b_span.end_time <= service_c_span.end_time)
  assert_true(service_c_span.end_time <= service_a_span.end_time)
}

// Test 2: Cross-Service Trace Correlation
test "cross-service trace correlation" {
  // Define trace correlation structure
  type TraceCorrelation = {
    trace_id: String,
    correlation_id: String,
    service_spans: Array[ServiceSpan],
    root_span: String,
    span_tree: Array[(String, Option[String])]
  }
  
  // Create multiple service spans for the same trace
  let trace_id = "c1d2e3f4g5h6i7j8k9l0m1n2o3p4q5r6"
  let correlation_id = "corr-12345-abcd"
  
  let root_span_id = "aaaaaaaaaaaaaaaa"
  let service_a_span_id = "bbbbbbbbbbbbbbbb"
  let service_b_span_id = "cccccccccccccccc"
  let service_c_span_id = "dddddddddddddddd"
  let service_d_span_id = "eeeeeeeeeeeeeeee"
  
  let service_spans = [
    {
      service_name: "api-gateway",
      operation_name: "handle-request",
      trace_context: {
        trace_id,
        span_id: root_span_id,
        parent_span_id: None,
        trace_flags: 1,
        trace_state: "",
        baggage: [("correlation.id", correlation_id)]
      },
      start_time: 1640995200000,
      end_time: 1640995200500,
      status: "ok",
      events: []
    },
    {
      service_name: "auth-service",
      operation_name: "authenticate",
      trace_context: {
        trace_id,
        span_id: service_a_span_id,
        parent_span_id: Some(root_span_id),
        trace_flags: 1,
        trace_state: "",
        baggage: [("correlation.id", correlation_id)]
      },
      start_time: 1640995200100,
      end_time: 1640995200200,
      status: "ok",
      events: []
    },
    {
      service_name: "user-service",
      operation_name: "get-user-profile",
      trace_context: {
        trace_id,
        span_id: service_b_span_id,
        parent_span_id: Some(root_span_id),
        trace_flags: 1,
        trace_state: "",
        baggage: [("correlation.id", correlation_id)]
      },
      start_time: 1640995200150,
      end_time: 1640995200250,
      status: "ok",
      events: []
    },
    {
      service_name: "order-service",
      operation_name: "process-order",
      trace_context: {
        trace_id,
        span_id: service_c_span_id,
        parent_span_id: Some(root_span_id),
        trace_flags: 1,
        trace_state: "",
        baggage: [("correlation.id", correlation_id)]
      },
      start_time: 1640995200200,
      end_time: 1640995200400,
      status: "ok",
      events: []
    },
    {
      service_name: "payment-service",
      operation_name: "process-payment",
      trace_context: {
        trace_id,
        span_id: service_d_span_id,
        parent_span_id: Some(service_c_span_id),
        trace_flags: 1,
        trace_state: "",
        baggage: [("correlation.id", correlation_id)]
      },
      start_time: 1640995200250,
      end_time: 1640995200350,
      status: "ok",
      events: []
    }
  ]
  
  // Build span tree
  let span_tree = [
    (root_span_id, None),
    (service_a_span_id, Some(root_span_id)),
    (service_b_span_id, Some(root_span_id)),
    (service_c_span_id, Some(root_span_id)),
    (service_d_span_id, Some(service_c_span_id))
  ]
  
  let trace_correlation = {
    trace_id,
    correlation_id,
    service_spans,
    root_span: root_span_id,
    span_tree
  }
  
  // Verify trace correlation
  assert_eq(trace_correlation.trace_id, trace_id)
  assert_eq(trace_correlation.correlation_id, correlation_id)
  assert_eq(trace_correlation.root_span, root_span_id)
  
  // Verify all spans have the same trace ID
  for span in trace_correlation.service_spans {
    assert_eq(span.trace_context.trace_id, trace_id)
  }
  
  // Verify all spans have the same correlation ID in baggage
  for span in trace_correlation.service_spans {
    assert_true(span.trace_context.baggage.contains(("correlation.id", correlation_id)))
  }
  
  // Verify span tree structure
  let find_parent = fn(span_id: String) {
    trace_correlation.span_tree.find(fn(pair) { pair.0 == span_id })
  }
  
  // Root span should have no parent
  match find_parent(root_span_id) {
    Some((_, parent)) => match parent {
      None => assert_true(true)
      Some(_) => assert_true(false)
    }
    None => assert_true(false)
  }
  
  // Child spans should have correct parents
  match find_parent(service_a_span_id) {
    Some((_, parent)) => match parent {
      Some(p) => assert_eq(p, root_span_id)
      None => assert_true(false)
    }
    None => assert_true(false)
  }
  
  match find_parent(service_d_span_id) {
    Some((_, parent)) => match parent {
      Some(p) => assert_eq(p, service_c_span_id)
      None => assert_true(false)
    }
    None => assert_true(false)
  }
  
  // Verify temporal consistency across services
  let sorted_spans = trace_correlation.service_spans.sort_by(fn(a, b) { 
    if a.start_time < b.start_time { -1 } else if a.start_time > b.start_time { 1 } else { 0 }
  })
  
  for i in 1..sorted_spans.length() {
    let prev_span = sorted_spans[i - 1]
    let curr_span = sorted_spans[i]
    assert_true(prev_span.start_time <= curr_span.start_time)
  }
}

// Test 3: Trace Consistency Under Failure
test "trace consistency under failure conditions" {
  // Define error handling structure
  type ErrorInfo = {
    error_code: String,
    error_message: String,
    error_time: Int,
    recovered: Bool
  }
  
  // Define service span with error handling
  type ServiceSpanWithError = {
    service_name: String,
    operation_name: String,
    trace_context: TraceContext,
    start_time: Int,
    end_time: Int,
    status: String,
    events: Array[(String, Int, Array[(String, String)])],
    error_info: Option[ErrorInfo]
  }
  
  // Create a trace with failure scenarios
  let trace_id = "f1a2i3l4u5r6e7t8e9s0t1c2a3s4e5"
  let root_span_id = "1111111111111111"
  let service_a_span_id = "2222222222222222"
  let service_b_span_id = "3333333333333333"
  let service_c_span_id = "4444444444444444"
  
  // Root span (API Gateway) - succeeds
  let root_span = {
    service_name: "api-gateway",
    operation_name: "process-request",
    trace_context: {
      trace_id,
      span_id: root_span_id,
      parent_span_id: None,
      trace_flags: 1,
      trace_state: "",
      baggage: [("request.id", "req-failure-123")]
    },
    start_time: 1640995200000,
    end_time: 1640995200800,
    status: "ok",
    events: [
      ("request.received", 1640995200000, []),
      ("routing.to.service.b", 1640995200100, []),
      ("routing.to.service.c", 1640995200200, []),
      ("response.from.service.b", 1640995200600, [("status", "error")]),
      ("response.from.service.c", 1640995200700, [("status", "ok")]),
      ("request.completed", 1640995200800, [])
    ],
    error_info: None
  }
  
  // Service B - fails with timeout
  let service_b_span = {
    service_name: "database-service",
    operation_name: "query-database",
    trace_context: {
      trace_id,
      span_id: service_b_span_id,
      parent_span_id: Some(root_span_id),
      trace_flags: 1,
      trace_state: "",
      baggage: [("request.id", "req-failure-123")]
    },
    start_time: 1640995200150,
    end_time: 1640995200550,
    status: "error",
    events: [
      ("query.start", 1640995200150, []),
      ("database.connection.attempt", 1640995200200, []),
      ("query.timeout", 1640995200500, [("timeout.ms", "3000")]),
      ("query.failed", 1640995200550, [])
    ],
    error_info: Some({
      error_code: "DB_TIMEOUT",
      error_message: "Database query timed out after 3000ms",
      error_time: 1640995200500,
      recovered: false
    })
  }
  
  // Service C - succeeds
  let service_c_span = {
    service_name: "cache-service",
    operation_name: "get-cache",
    trace_context: {
      trace_id,
      span_id: service_c_span_id,
      parent_span_id: Some(root_span_id),
      trace_flags: 1,
      trace_state: "",
      baggage: [("request.id", "req-failure-123")]
    },
    start_time: 1640995200250,
    end_time: 1640995200650,
    status: "ok",
    events: [
      ("cache.lookup", 1640995200250, []),
      ("cache.hit", 1640995200300, []),
      ("data.retrieved", 1640995200650, [("bytes", "1024")])
    ],
    error_info: None
  }
  
  let spans_with_errors = [root_span, service_b_span, service_c_span]
  
  // Verify trace consistency despite failures
  for span in spans_with_errors {
    assert_eq(span.trace_context.trace_id, trace_id)
    assert_true(span.trace_context.baggage.contains(("request.id", "req-failure-123")))
  }
  
  // Verify error propagation
  match service_b_span.error_info {
    Some(error) => {
      assert_eq(error.error_code, "DB_TIMEOUT")
      assert_eq(error.error_message, "Database query timed out after 3000ms")
      assert_false(error.recovered)
    }
    None => assert_true(false)
  }
  
  // Verify error events are recorded
  let timeout_events = service_b_span.events.filter(fn(event) {
    event.0 == "query.timeout"
  })
  assert_eq(timeout_events.length(), 1)
  
  let failed_events = service_b_span.events.filter(fn(event) {
    event.0 == "query.failed"
  })
  assert_eq(failed_events.length(), 1)
  
  // Verify root span records child service failures
  let error_response_events = root_span.events.filter(fn(event) {
    event.0 == "response.from.service.b" && 
    event.2.contains(("status", "error"))
  })
  assert_eq(error_response_events.length(), 1)
  
  // Verify temporal consistency even with failures
  assert_true(root_span.start_time <= service_b_span.start_time)
  assert_true(root_span.start_time <= service_c_span.start_time)
  assert_true(service_b_span.end_time <= root_span.end_time)
  assert_true(service_c_span.end_time <= root_span.end_time)
  
  // Verify error doesn't break trace context propagation
  assert_eq(service_b_span.trace_context.parent_span_id, Some(root_span_id))
  assert_eq(service_c_span.trace_context.parent_span_id, Some(root_span_id))
}

// Test 4: Distributed Trace Sampling Consistency
test "distributed trace sampling consistency" {
  // Define sampling decision structure
  type SamplingDecision = {
    sampled: Bool,
    decision_reason: String,
    decision_time: Int,
    sample_rate: Float
  }
  
  // Define enhanced trace context with sampling
  type EnhancedTraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String,
    baggage: Array[(String, String)],
    sampling_decision: SamplingDecision
  }
  
  // Create sampling decision
  let sampling_decision = {
    sampled: true,
    decision_reason: "probabilistic",
    decision_time: 1640995200000,
    sample_rate: 0.1
  }
  
  // Create root span with sampling decision
  let root_trace_id = "s1a2m3p4l5i6n7g8t9e0s1t2c3a4s5e6"
  let root_span_id = "aaaaaaaaaaaaaaaa"
  
  let root_context = {
    trace_id: root_trace_id,
    span_id: root_span_id,
    parent_span_id: None,
    trace_flags: 1,  // Sampled flag
    trace_state: "",
    baggage: [("sampling.rate", "0.1")],
    sampling_decision
  }
  
  // Service A respects sampling decision
  let service_a_context = {
    trace_id: root_trace_id,
    span_id: "bbbbbbbbbbbbbbbb",
    parent_span_id: Some(root_span_id),
    trace_flags: 1,  // Sampled flag preserved
    trace_state: "",
    baggage: [("sampling.rate", "0.1")],
    sampling_decision
  }
  
  // Service B respects sampling decision
  let service_b_context = {
    trace_id: root_trace_id,
    span_id: "cccccccccccccccc",
    parent_span_id: Some(root_span_id),
    trace_flags: 1,  // Sampled flag preserved
    trace_state: "",
    baggage: [("sampling.rate", "0.1")],
    sampling_decision
  }
  
  // Create a non-sampled trace for comparison
  let non_sampled_decision = {
    sampled: false,
    decision_reason: "probabilistic",
    decision_time: 1640995300000,
    sample_rate: 0.1
  }
  
  let non_sampled_trace_id = "n1o2n3s4a5m6p7l8e9d0t1r2a3c4e5i6d"
  let non_sampled_root_id = "dddddddddddddddd"
  
  let non_sampled_context = {
    trace_id: non_sampled_trace_id,
    span_id: non_sampled_root_id,
    parent_span_id: None,
    trace_flags: 0,  // Not sampled
    trace_state: "",
    baggage: [("sampling.rate", "0.1")],
    sampling_decision: non_sampled_decision
  }
  
  // Verify sampling consistency
  assert_true(root_context.sampling_decision.sampled)
  assert_true(service_a_context.sampling_decision.sampled)
  assert_true(service_b_context.sampling_decision.sampled)
  
  assert_false(non_sampled_context.sampling_decision.sampled)
  
  // Verify trace flags reflect sampling decision
  assert_eq(root_context.trace_flags & 1, 1)  // Sampled bit set
  assert_eq(service_a_context.trace_flags & 1, 1)  // Sampled bit preserved
  assert_eq(service_b_context.trace_flags & 1, 1)  // Sampled bit preserved
  assert_eq(non_sampled_context.trace_flags & 1, 0)  // Sampled bit not set
  
  // Verify sampling decision is propagated
  assert_eq(root_context.sampling_decision.decision_reason, service_a_context.sampling_decision.decision_reason)
  assert_eq(service_a_context.sampling_decision.decision_reason, service_b_context.sampling_decision.decision_reason)
  
  assert_eq(root_context.sampling_decision.sample_rate, service_a_context.sampling_decision.sample_rate)
  assert_eq(service_a_context.sampling_decision.sample_rate, service_b_context.sampling_decision.sample_rate)
  
  // Verify baggage contains sampling information
  assert_true(root_context.baggage.contains(("sampling.rate", "0.1")))
  assert_true(service_a_context.baggage.contains(("sampling.rate", "0.1")))
  assert_true(service_b_context.baggage.contains(("sampling.rate", "0.1")))
  assert_true(non_sampled_context.baggage.contains(("sampling.rate", "0.1")))
  
  // Verify sampling decision time consistency
  assert_eq(root_context.sampling_decision.decision_time, service_a_context.sampling_decision.decision_time)
  assert_eq(service_a_context.sampling_decision.decision_time, service_b_context.sampling_decision.decision_time)
}

// Test 5: Trace State Management Across Services
test "trace state management across services" {
  // Define trace state entry
  type TraceStateEntry = {
    vendor: String,
    key: String,
    value: String
  }
  
  // Define enhanced trace context with trace state
  type TraceStateContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state_entries: Array[TraceStateEntry],
    baggage: Array[(String, String)]
  }
  
  // Create initial trace state
  let initial_trace_state = [
    { vendor: "otel", key: "rojo", value: "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" },
    { vendor: "vendor1", key: "custom1", value: "value1" },
    { vendor: "vendor2", key: "custom2", value: "value2" }
  ]
  
  // Create root span context
  let root_context = {
    trace_id: "t1r2a3c4e5s6t7a8t9e0m1a2n3a4g5e6",
    span_id: "1111111111111111",
    parent_span_id: None,
    trace_flags: 1,
    trace_state_entries: initial_trace_state,
    baggage: [("request.id", "req-trace-state-123")]
  }
  
  // Service A adds to trace state
  let service_a_trace_state = root_context.trace_state_entries.push({
    vendor: "service-a",
    key: "processing.stage",
    value: "validation"
  })
  
  let service_a_context = {
    trace_id: root_context.trace_id,
    span_id: "2222222222222222",
    parent_span_id: Some(root_context.span_id),
    trace_flags: root_context.trace_flags,
    trace_state_entries: service_a_trace_state,
    baggage: root_context.baggage
  }
  
  // Service B modifies trace state
  let service_b_trace_state = service_a_context.trace_state_entries.map(fn(entry) {
    if entry.vendor == "vendor1" && entry.key == "custom1" {
      { entry | value: "modified_value1" }
    } else {
      entry
    }
  }).push({
    vendor: "service-b",
    key: "processing.result",
    value: "success"
  })
  
  let service_b_context = {
    trace_id: root_context.trace_id,
    span_id: "3333333333333333",
    parent_span_id: Some(service_a_context.span_id),
    trace_flags: root_context.trace_flags,
    trace_state_entries: service_b_trace_state,
    baggage: root_context.baggage
  }
  
  // Service C reads trace state
  let service_c_context = {
    trace_id: root_context.trace_id,
    span_id: "4444444444444444",
    parent_span_id: Some(service_b_context.span_id),
    trace_flags: root_context.trace_flags,
    trace_state_entries: service_b_context.trace_state_entries,
    baggage: root_context.baggage
  }
  
  // Verify trace state propagation
  assert_eq(service_a_context.trace_state_entries.length(), 4)
  assert_eq(service_b_context.trace_state_entries.length(), 5)
  assert_eq(service_c_context.trace_state_entries.length(), 5)
  
  // Verify original trace state entries are preserved
  let find_trace_state_entry = fn(entries: Array[TraceStateEntry], vendor: String, key: String) {
    entries.find(fn(entry) { entry.vendor == vendor && entry.key == key })
  }
  
  // Original entries should be present in all contexts
  match find_trace_state_entry(root_context.trace_state_entries, "otel", "rojo") {
    Some(entry) => {
      match find_trace_state_entry(service_a_context.trace_state_entries, "otel", "rojo") {
        Some(a_entry) => assert_eq(entry.value, a_entry.value)
        None => assert_true(false)
      }
      match find_trace_state_entry(service_b_context.trace_state_entries, "otel", "rojo") {
        Some(b_entry) => assert_eq(entry.value, b_entry.value)
        None => assert_true(false)
      }
      match find_trace_state_entry(service_c_context.trace_state_entries, "otel", "rojo") {
        Some(c_entry) => assert_eq(entry.value, c_entry.value)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Verify Service A's addition
  match find_trace_state_entry(service_a_context.trace_state_entries, "service-a", "processing.stage") {
    Some(entry) => assert_eq(entry.value, "validation")
    None => assert_true(false)
  }
  
  // Verify Service B's modification
  match find_trace_state_entry(service_b_context.trace_state_entries, "vendor1", "custom1") {
    Some(entry) => assert_eq(entry.value, "modified_value1")
    None => assert_true(false)
  }
  
  // Verify Service B's addition
  match find_trace_state_entry(service_b_context.trace_state_entries, "service-b", "processing.result") {
    Some(entry) => assert_eq(entry.value, "success")
    None => assert_true(false)
  }
  
  // Verify Service C sees all modifications
  match find_trace_state_entry(service_c_context.trace_state_entries, "vendor1", "custom1") {
    Some(entry) => assert_eq(entry.value, "modified_value1")
    None => assert_true(false)
  }
  
  match find_trace_state_entry(service_c_context.trace_state_entries, "service-b", "processing.result") {
    Some(entry) => assert_eq(entry.value, "success")
    None => assert_true(false)
  }
}