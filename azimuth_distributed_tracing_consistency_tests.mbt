// Azimuth Distributed Tracing Consistency Tests
// This file contains test cases for distributed tracing consistency validation

// Test 1: Trace Context Propagation Consistency
test "trace context propagation consistency" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: Array[(String, String)],
    baggage: Array[(String, String)]
  }
  
  // Define propagation format
  enum PropagationFormat {
    HTTPHeaders
    GRPCMetadata
    MessageProperties
    Custom(String)
  }
  
  // Define propagation result
  type PropagationResult = {
    is_valid: Bool,
    extracted_context: Option[TraceContext],
    missing_fields: Array[String],
    invalid_fields: Array[String]
  }
  
  // Serialize trace context to HTTP headers
  let serialize_to_http_headers = fn(context: TraceContext) {
    let headers = [
      ("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags.to_string(16)),
      ("tracestate", context.trace_state.map_fn(item) { item.0 + "=" + item.1 }.join(",")),
      ("baggage", context.baggage.map_fn(item) { item.0 + "=" + item.1 }.join(","))
    ]
    
    // Filter out empty headers
    headers.filter_fn(header) { 
      let (_, value) = header
      value.length() > 0
    }
  }
  
  // Extract trace context from HTTP headers
  let extract_from_http_headers = fn(headers: Array[(String, String)]) {
    let mut trace_id = None
    let mut span_id = None
    let mut parent_span_id = None
    let mut trace_flags = 0
    let mut trace_state = []
    let mut baggage = []
    let mut missing_fields = []
    let mut invalid_fields = []
    
    for (key, value) in headers {
      match key {
        "traceparent" => {
          let parts = value.split("-")
          if parts.length() == 4 {
            trace_id = Some(parts[1])
            span_id = Some(parts[2])
            match parts[3].parse_int(16) {
              Some(flags) => trace_flags = flags
              None => invalid_fields = invalid_fields.push("trace_flags")
            }
          } else {
            invalid_fields = invalid_fields.push("traceparent_format")
          }
        }
        "tracestate" => {
          if value.length() > 0 {
            trace_state = value.split(",").map_fn(item) {
              let kv = item.split("=")
              if kv.length() == 2 {
                (kv[0], kv[1])
              } else {
                ("", "")
              }
            }
          }
        }
        "baggage" => {
          if value.length() > 0 {
            baggage = value.split(",").map_fn(item) {
              let kv = item.split("=")
              if kv.length() == 2 {
                (kv[0], kv[1])
              } else {
                ("", "")
              }
            }
          }
        }
        _ => {}  // Ignore other headers
      }
    }
    
    // Check for required fields
    if trace_id.is_none() { missing_fields = missing_fields.push("trace_id") }
    if span_id.is_none() { missing_fields = missing_fields.push("span_id") }
    
    let context = if missing_fields.length() == 0 && invalid_fields.length() == 0 {
      Some({
        trace_id: trace_id.unwrap(),
        span_id: span_id.unwrap(),
        parent_span_id: parent_span_id,
        trace_flags: trace_flags,
        trace_state: trace_state,
        baggage: baggage
      })
    } else {
      None
    }
    
    {
      is_valid: missing_fields.length() == 0 && invalid_fields.length() == 0,
      extracted_context: context,
      missing_fields: missing_fields,
      invalid_fields: invalid_fields
    }
  }
  
  // Test context propagation round-trip
  let original_context = {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("1234567890abcdef"),
    trace_flags: 1,
    trace_state: [("rojo", "00f067aa0ba902b7"), ("congo", "t61rcWkgMzE")],
    baggage: [("user_id", "12345"), ("session_id", "abcdef")]
  }
  
  // Test HTTP header serialization and extraction
  let http_headers = serialize_to_http_headers(original_context)
  assert_eq(http_headers.length(), 3)
  assert_true(http_headers.some_fn(h) { h.0 == "traceparent" })
  assert_true(http_headers.some_fn(h) { h.0 == "tracestate" })
  assert_true(http_headers.some_fn(h) { h.0 == "baggage" })
  
  let http_result = extract_from_http_headers(http_headers)
  assert_true(http_result.is_valid)
  assert_eq(http_result.missing_fields.length(), 0)
  assert_eq(http_result.invalid_fields.length(), 0)
  
  match http_result.extracted_context {
    Some(extracted) => {
      assert_eq(extracted.trace_id, original_context.trace_id)
      assert_eq(extracted.span_id, original_context.span_id)
      assert_eq(extracted.parent_span_id, original_context.parent_span_id)
      assert_eq(extracted.trace_flags, original_context.trace_flags)
      assert_eq(extracted.trace_state.length(), 2)
      assert_eq(extracted.baggage.length(), 2)
    }
    None => assert_true(false)
  }
  
  // Test with missing traceparent header
  let incomplete_headers = [
    ("tracestate", "rojo=00f067aa0ba902b7"),
    ("baggage", "user_id=12345")
  ]
  
  let incomplete_result = extract_from_http_headers(incomplete_headers)
  assert_false(incomplete_result.is_valid)
  assert_eq(incomplete_result.missing_fields.length(), 2)
  assert_true(incomplete_result.missing_fields.contains("trace_id"))
  assert_true(incomplete_result.missing_fields.contains("span_id"))
  
  // Test with invalid traceparent format
  let invalid_headers = [
    ("traceparent", "invalid-format"),
    ("tracestate", "rojo=00f067aa0ba902b7"),
    ("baggage", "user_id=12345")
  ]
  
  let invalid_result = extract_from_http_headers(invalid_headers)
  assert_false(invalid_result.is_valid)
  assert_eq(incomplete_result.invalid_fields.length(), 1)
  assert_true(incomplete_result.invalid_fields.contains("traceparent_format"))
  
  // Test cross-service propagation consistency
  let propagate_across_services = fn(context: TraceContext, service_chain: Array[String>) {
    let mut current_context = context
    let mut propagation_history = []
    
    for service_name in service_chain {
      // Simulate propagation to next service
      let headers = serialize_to_http_headers(current_context)
      let result = extract_from_http_headers(headers)
      
      match result.extracted_context {
        Some(extracted) => {
          // Create new span in this service
          let new_span_id = "span-" + service_name + "-" + "12345678"
          current_context = {
            trace_id: extracted.trace_id,
            span_id: new_span_id,
            parent_span_id: Some(extracted.span_id),
            trace_flags: extracted.trace_flags,
            trace_state: extracted.trace_state,
            baggage: extracted.baggage
          }
          
          propagation_history = propagation_history.push({
            service: service_name,
            trace_id: extracted.trace_id,
            span_id: new_span_id,
            parent_span_id: extracted.span_id,
            propagation_valid: result.is_valid
          })
        }
        None => {
          propagation_history = propagation_history.push({
            service: service_name,
            trace_id: "",
            span_id: "",
            parent_span_id: "",
            propagation_valid: false
          })
        }
      }
    }
    
    propagation_history
  }
  
  let service_chain = ["api-gateway", "user-service", "payment-service", "notification-service"]
  let propagation_history = propagate_across_services(original_context, service_chain)
  
  assert_eq(propagation_history.length(), 4)
  
  // Verify trace ID consistency across all services
  for service_record in propagation_history {
    assert_true(service_record.propagation_valid)
    assert_eq(service_record.trace_id, original_context.trace_id)
  }
  
  // Verify parent-child relationship
  assert_eq(propagation_history[0].parent_span_id, original_context.span_id)
  assert_eq(propagation_history[1].parent_span_id, propagation_history[0].span_id)
  assert_eq(propagation_history[2].parent_span_id, propagation_history[1].span_id)
  assert_eq(propagation_history[3].parent_span_id, propagation_history[2].span_id)
}

// Test 2: Distributed Trace Timeline Consistency
test "distributed trace timeline consistency" {
  // Define span timeline
  type SpanTimeline = {
    span_id: String,
    service_name: String,
    start_time: Int,    // Unix timestamp in microseconds
    end_time: Int,      // Unix timestamp in microseconds
    duration: Int       // Duration in microseconds
  }
  
  // Define trace timeline
  type TraceTimeline = {
    trace_id: String,
    spans: Array[SpanTimeline],
    root_span: String,
    total_duration: Int
  }
  
  // Define timeline consistency result
  type TimelineConsistencyResult = {
    is_consistent: Bool,
    violations: Array[String],
    orphaned_spans: Array[String>,
    circular_dependencies: Array[(String, String)]
  }
  
  // Validate timeline consistency
  let validate_timeline_consistency = fn(timeline: TraceTimeline, parent_child_map: Map[String, String>) {
    let mut violations = []
    let mut orphaned_spans = []
    let mut circular_dependencies = []
    
    // Check for negative or zero durations
    for span in timeline.spans {
      if span.duration <= 0 {
        violations = violations.push("invalid_duration_" + span.span_id)
      }
      
      if span.end_time < span.start_time {
        violations = violations.push("end_before_start_" + span.span_id)
      }
    }
    
    // Check for orphaned spans (spans without valid parents)
    for span in timeline.spans {
      if span.span_id != timeline.root_span {
        match Map::get(parent_child_map, span.span_id) {
          Some(parent_id) => {
            // Check if parent exists
            let parent_exists = timeline.spans.some_fn(s) { s.span_id == parent_id }
            if not(parent_exists) {
              orphaned_spans = orphaned_spans.push(span.span_id)
            }
          }
          None => {
            orphaned_spans = orphaned_spans.push(span.span_id)
          }
        }
      }
    }
    
    // Check for timeline consistency (child should be within parent duration)
    for span in timeline.spans {
      if span.span_id != timeline.root_span {
        match Map::get(parent_child_map, span.span_id) {
          Some(parent_id) => {
            match timeline.spans.find_fn(s) { s.span_id == parent_id } {
              Some(parent_span) => {
                if span.start_time < parent_span.start_time || span.end_time > parent_span.end_time {
                  violations = violations.push("child_outside_parent_" + span.span_id)
                }
              }
              None => {}  // Parent not found (already caught as orphaned)
            }
          }
          None => {}  // No parent (already caught as orphaned)
        }
      }
    }
    
    // Check for circular dependencies
    for (child, parent) in parent_child_map {
      let mut current = parent
      let mut visited = []
      let mut found_cycle = false
      
      while Map::contains(parent_child_map, current) && not(found_cycle) {
        if visited.contains(current) {
          circular_dependencies = circular_dependencies.push((child, current))
          found_cycle = true
        } else {
          visited = visited.push(current)
          match Map::get(parent_child_map, current) {
            Some(next_parent) => current = next_parent
            None => break
          }
        }
      }
    }
    
    {
      is_consistent: violations.length() == 0 && orphaned_spans.length() == 0 && circular_dependencies.length() == 0,
      violations: violations,
      orphaned_spans: orphaned_spans,
      circular_dependencies: circular_dependencies
    }
  }
  
  // Test timeline consistency validation
  let parent_child_map = Map::from_array([
    ("span-2", "span-1"),    // span-2's parent is span-1
    ("span-3", "span-1"),    // span-3's parent is span-1
    ("span-4", "span-2"),    // span-4's parent is span-2
    ("span-5", "span-3")     // span-5's parent is span-3
  ])
  
  let consistent_timeline = {
    trace_id: "trace-consistent-123",
    spans: [
      {
        span_id: "span-1",
        service_name: "api-gateway",
        start_time: 1640995200000000,  // 2022-01-01 00:00:00 UTC
        end_time: 1640995205000000,    // 5 seconds later
        duration: 5000000              // 5 seconds
      },
      {
        span_id: "span-2",
        service_name: "user-service",
        start_time: 1640995201000000,  // 1 second after root start
        end_time: 1640995203000000,    // 3 seconds after root start
        duration: 2000000              // 2 seconds
      },
      {
        span_id: "span-3",
        service_name: "payment-service",
        start_time: 1640995202000000,  // 2 seconds after root start
        end_time: 1640995204500000,    // 4.5 seconds after root start
        duration: 2500000              // 2.5 seconds
      },
      {
        span_id: "span-4",
        service_name: "database",
        start_time: 1640995201500000,  // 1.5 seconds after root start
        end_time: 1640995202500000,    // 2.5 seconds after root start
        duration: 1000000              // 1 second
      },
      {
        span_id: "span-5",
        service_name: "notification-service",
        start_time: 1640995204000000,  // 4 seconds after root start
        end_time: 1640995204800000,    // 4.8 seconds after root start
        duration: 800000               // 0.8 seconds
      }
    ],
    root_span: "span-1",
    total_duration: 5000000
  }
  
  let consistent_result = validate_timeline_consistency(consistent_timeline, parent_child_map)
  assert_true(consistent_result.is_consistent)
  assert_eq(consistent_result.violations.length(), 0)
  assert_eq(consistent_result.orphaned_spans.length(), 0)
  assert_eq(consistent_result.circular_dependencies.length(), 0)
  
  // Test with timeline violations
  let inconsistent_timeline = {
    trace_id: "trace-inconsistent-456",
    spans: [
      {
        span_id: "span-1",
        service_name: "api-gateway",
        start_time: 1640995200000000,
        end_time: 1640995205000000,
        duration: 5000000
      },
      {
        span_id: "span-2",
        service_name: "user-service",
        start_time: 1640995201000000,
        end_time: 1640995203000000,
        duration: -1000000  // Negative duration
      },
      {
        span_id: "span-3",
        service_name: "payment-service",
        start_time: 1640995206000000,  // Starts after parent ends
        end_time: 1640995207000000,
        duration: 1000000
      },
      {
        span_id: "span-4",
        service_name: "orphan-service",
        start_time: 1640995202000000,
        end_time: 1640995203000000,
        duration: 1000000
      }
    ],
    root_span: "span-1",
    total_duration: 5000000
  }
  
  let inconsistent_parent_child = Map::from_array([
    ("span-2", "span-1"),
    ("span-3", "span-1"),
    ("span-4", "span-nonexistent")  // Orphaned span
  ])
  
  let inconsistent_result = validate_timeline_consistency(inconsistent_timeline, inconsistent_parent_child)
  assert_false(inconsistent_result.is_consistent)
  assert_true(inconsistent_result.violations.length() > 0)
  assert_true(inconsistent_result.orphaned_spans.length() > 0)
  assert_true(inconsistent_result.violations.some_fn(v) { v.contains("invalid_duration") })
  assert_true(inconsistent_result.violations.some_fn(v) { v.contains("child_outside_parent") })
  
  // Test circular dependency detection
  let circular_map = Map::from_array([
    ("span-2", "span-3"),
    ("span-3", "span-4"),
    ("span-4", "span-2")  // Circular dependency
  ])
  
  let circular_timeline = {
    trace_id: "trace-circular-789",
    spans: [
      {
        span_id: "span-1",
        service_name: "api-gateway",
        start_time: 1640995200000000,
        end_time: 1640995205000000,
        duration: 5000000
      },
      {
        span_id: "span-2",
        service_name: "service-2",
        start_time: 1640995201000000,
        end_time: 1640995202000000,
        duration: 1000000
      },
      {
        span_id: "span-3",
        service_name: "service-3",
        start_time: 1640995201500000,
        end_time: 1640995202500000,
        duration: 1000000
      },
      {
        span_id: "span-4",
        service_name: "service-4",
        start_time: 1640995202000000,
        end_time: 1640995203000000,
        duration: 1000000
      }
    ],
    root_span: "span-1",
    total_duration: 5000000
  }
  
  let circular_result = validate_timeline_consistency(circular_timeline, circular_map)
  assert_false(circular_result.is_consistent)
  assert_eq(circular_result.circular_dependencies.length(), 3)  // All spans in the cycle
}

// Test 3: Cross-Service Trace Consistency
test "cross-service trace consistency" {
  // Define service trace point
  type ServiceTracePoint = {
    service_name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    status: String,
    start_time: Int,
    end_time: Int,
    tags: Array[(String, String)]
  }
  
  // Define cross-service consistency validator
  type CrossServiceValidator = {
    required_services: Array[String],
    required_operations: Map[String, Array[String]>,  // service -> required operations
    consistency_rules: Array[String]  // Names of consistency rules to apply
  }
  
  // Define cross-service consistency result
  type CrossServiceResult = {
    is_consistent: Bool,
    missing_services: Array[String],
    missing_operations: Array[(String, String)],  // service, operation
    inconsistent_trace_ids: Array[String],
    timeline_violations: Array[String]
  }
  
  // Validate cross-service trace consistency
  let validate_cross_service_consistency = fn(trace_points: Array[ServiceTracePoint], validator: CrossServiceValidator) {
    let mut missing_services = []
    let mut missing_operations = []
    let mut inconsistent_trace_ids = []
    let mut timeline_violations = []
    
    // Group trace points by service
    let mut service_points = Map::empty()
    for point in trace_points {
      let current_points = match Map::get(service_points, point.service_name) {
        Some(points) => points
        None => []
      }
      let _ = Map::insert(service_points, point.service_name, current_points.push(point))
    }
    
    // Check for missing services
    for service in validator.required_services {
      if not(Map::contains(service_points, service)) {
        missing_services = missing_services.push(service)
      }
    }
    
    // Check for missing operations
    for (service, required_ops) in validator.required_operations {
      match Map::get(service_points, service) {
        Some(points) => {
          let service_operations = points.map_fn(p) { p.operation_name }
          for required_op in required_ops {
            if not(service_operations.contains(required_op)) {
              missing_operations = missing_operations.push((service, required_op))
            }
          }
        }
        None => {
          // Service missing, already recorded above
        }
      }
    }
    
    // Check for consistent trace IDs across all services
    let mut trace_ids = []
    for point in trace_points {
      if not(trace_ids.contains(point.trace_id)) {
        trace_ids = trace_ids.push(point.trace_id)
      }
    }
    
    if trace_ids.length() > 1 {
      inconsistent_trace_ids = trace_ids
    }
    
    // Check timeline consistency (simplified)
    if trace_points.length() > 1 {
      let sorted_points = trace_points.sort_by(fn(a, b) { 
        if a.start_time < b.start_time { -1 } 
        else if a.start_time > b.start_time { 1 } 
        else { 0 } 
      })
      
      // Check for reasonable timeline gaps
      for i in 1..sorted_points.length() {
        let time_diff = sorted_points[i].start_time - sorted_points[i-1].end_time
        if time_diff > 30000000 {  // More than 30 seconds gap
          timeline_violations = timeline_violations.push("large_gap_" + sorted_points[i].service_name)
        }
      }
    }
    
    {
      is_consistent: missing_services.length() == 0 && 
                     missing_operations.length() == 0 && 
                     inconsistent_trace_ids.length() <= 1 &&
                     timeline_violations.length() == 0,
      missing_services: missing_services,
      missing_operations: missing_operations,
      inconsistent_trace_ids: inconsistent_trace_ids,
      timeline_violations: timeline_violations
    }
  }
  
  // Test cross-service consistency
  let validator = {
    required_services: ["api-gateway", "user-service", "payment-service", "notification-service"],
    required_operations: Map::from_array([
      ("api-gateway", ["authenticate", "authorize"]),
      ("user-service", ["get_user", "validate_user"]),
      ("payment-service", ["process_payment", "validate_payment"]),
      ("notification-service", ["send_notification"])
    ]),
    consistency_rules: ["trace_id_consistency", "timeline_consistency", "operation_completeness"]
  }
  
  let consistent_trace_points = [
    {
      service_name: "api-gateway",
      trace_id: "trace-consistent-123",
      span_id: "span-1",
      parent_span_id: None,
      operation_name: "authenticate",
      status: "success",
      start_time: 1640995200000000,
      end_time: 1640995201000000,
      tags: [("user_id", "12345"), ("ip", "192.168.1.1")]
    },
    {
      service_name: "user-service",
      trace_id: "trace-consistent-123",
      span_id: "span-2",
      parent_span_id: Some("span-1"),
      operation_name: "get_user",
      status: "success",
      start_time: 1640995201000000,
      end_time: 1640995202000000,
      tags: [("user_id", "12345")]
    },
    {
      service_name: "payment-service",
      trace_id: "trace-consistent-123",
      span_id: "span-3",
      parent_span_id: Some("span-2"),
      operation_name: "process_payment",
      status: "success",
      start_time: 1640995202000000,
      end_time: 1640995204000000,
      tags: [("amount", "100.00"), ("currency", "USD")]
    },
    {
      service_name: "notification-service",
      trace_id: "trace-consistent-123",
      span_id: "span-4",
      parent_span_id: Some("span-3"),
      operation_name: "send_notification",
      status: "success",
      start_time: 1640995204000000,
      end_time: 1640995204500000,
      tags: [("type", "email"), ("recipient", "user@example.com")]
    }
  ]
  
  let consistent_result = validate_cross_service_consistency(consistent_trace_points, validator)
  assert_true(consistent_result.is_consistent)
  assert_eq(consistent_result.missing_services.length(), 0)
  assert_eq(consistent_result.missing_operations.length(), 0)
  assert_eq(consistent_result.inconsistent_trace_ids.length(), 1)
  assert_eq(consistent_result.timeline_violations.length(), 0)
  
  // Test with missing service
  let incomplete_trace_points = [
    consistent_trace_points[0],
    consistent_trace_points[1],
    consistent_trace_points[2]  // Missing notification-service
  ]
  
  let incomplete_result = validate_cross_service_consistency(incomplete_trace_points, validator)
  assert_false(incomplete_result.is_consistent)
  assert_eq(incomplete_result.missing_services.length(), 1)
  assert_eq(incomplete_result.missing_services[0], "notification-service")
  
  // Test with inconsistent trace IDs
  let inconsistent_trace_points = [
    consistent_trace_points[0],
    { consistent_trace_points[1] | trace_id: "trace-different-456" },
    consistent_trace_points[2],
    consistent_trace_points[3]
  ]
  
  let inconsistent_result = validate_cross_service_consistency(inconsistent_trace_points, validator)
  assert_false(inconsistent_result.is_consistent)
  assert_eq(inconsistent_result.inconsistent_trace_ids.length(), 2)
  
  // Test with timeline violations
  let timeline_violation_points = [
    consistent_trace_points[0],
    consistent_trace_points[1],
    consistent_trace_points[2],
    { 
      consistent_trace_points[3] | 
      start_time: 1640995300000000,  // 100 seconds later
      end_time: 1640995300500000
    }
  ]
  
  let timeline_result = validate_cross_service_consistency(timeline_violation_points, validator)
  assert_false(timeline_result.is_consistent)
  assert_eq(timeline_result.timeline_violations.length(), 1)
  assert_true(timeline_result.timeline_violations[0].contains("large_gap"))
}

// Test 4: Distributed Trace Sampling Consistency
test "distributed trace sampling consistency" {
  // Define sampling decision
  type SamplingDecision = {
    trace_id: String,
    sampled: Bool,
    sample_rate: Float,
    decision_point: String,
    timestamp: Int
  }
  
  // Define sampling consistency validator
  type SamplingConsistencyValidator = {
    min_sample_rate: Float,
    max_sample_rate: Float,
    consistency_window: Int,  // Time window in seconds
    required_decision_points: Array[String>
  }
  
  // Define sampling consistency result
  type SamplingConsistencyResult = {
    is_consistent: Bool,
    inconsistent_decisions: Array[(String, Bool, Bool)],  // trace_id, decision1, decision2
    rate_violations: Array[String],
    missing_decision_points: Array[String]
  }
  
  // Validate sampling consistency
  let validate_sampling_consistency = fn(decisions: Array[SamplingDecision], validator: SamplingConsistencyValidator) {
    let mut inconsistent_decisions = []
    let mut rate_violations = []
    let mut missing_decision_points = []
    
    // Group decisions by trace ID
    let mut trace_decisions = Map::empty()
    for decision in decisions {
      let current_decisions = match Map::get(trace_decisions, decision.trace_id) {
        Some(decisions) => decisions
        None => []
      }
      let _ = Map::insert(trace_decisions, decision.trace_id, current_decisions.push(decision))
    }
    
    // Check for inconsistent decisions for the same trace
    for (trace_id, trace_decision_list) in trace_decisions {
      if trace_decision_list.length() > 1 {
        let first_decision = trace_decision_list[0].sampled
        for decision in trace_decision_list.slice(1, trace_decision_list.length()) {
          if decision.sampled != first_decision {
            inconsistent_decisions = inconsistent_decisions.push((trace_id, first_decision, decision.sampled))
          }
        }
      }
    }
    
    // Check sample rate bounds
    let total_decisions = decisions.length()
    if total_decisions > 0 {
      let sampled_count = decisions.filter_fn(d) { d.sampled }.length()
      let actual_sample_rate = (sampled_count as Float) / (total_decisions as Float)
      
      if actual_sample_rate < validator.min_sample_rate {
        rate_violations = rate_violations.push("below_min_rate")
      }
      if actual_sample_rate > validator.max_sample_rate {
        rate_violations = rate_violations.push("above_max_rate")
      }
    }
    
    // Check for required decision points
    let decision_points = decisions.map_fn(d) { d.decision_point }
    for required_point in validator.required_decision_points {
      if not(decision_points.contains(required_point)) {
        missing_decision_points = missing_decision_points.push(required_point)
      }
    }
    
    {
      is_consistent: inconsistent_decisions.length() == 0 && 
                     rate_violations.length() == 0 &&
                     missing_decision_points.length() == 0,
      inconsistent_decisions: inconsistent_decisions,
      rate_violations: rate_violations,
      missing_decision_points: missing_decision_points
    }
  }
  
  // Test sampling consistency validation
  let validator = {
    min_sample_rate: 0.1,
    max_sample_rate: 0.3,
    consistency_window: 60,
    required_decision_points: ["api-gateway", "user-service", "payment-service"]
  }
  
  let consistent_decisions = [
    {
      trace_id: "trace-1",
      sampled: true,
      sample_rate: 0.2,
      decision_point: "api-gateway",
      timestamp: 1640995200
    },
    {
      trace_id: "trace-1",
      sampled: true,
      sample_rate: 0.2,
      decision_point: "user-service",
      timestamp: 1640995201
    },
    {
      trace_id: "trace-1",
      sampled: true,
      sample_rate: 0.2,
      decision_point: "payment-service",
      timestamp: 1640995202
    },
    {
      trace_id: "trace-2",
      sampled: false,
      sample_rate: 0.2,
      decision_point: "api-gateway",
      timestamp: 1640995203
    },
    {
      trace_id: "trace-2",
      sampled: false,
      sample_rate: 0.2,
      decision_point: "user-service",
      timestamp: 1640995204
    },
    {
      trace_id: "trace-2",
      sampled: false,
      sample_rate: 0.2,
      decision_point: "payment-service",
      timestamp: 1640995205
    },
    {
      trace_id: "trace-3",
      sampled: true,
      sample_rate: 0.2,
      decision_point: "api-gateway",
      timestamp: 1640995206
    },
    {
      trace_id: "trace-3",
      sampled: true,
      sample_rate: 0.2,
      decision_point: "user-service",
      timestamp: 1640995207
    },
    {
      trace_id: "trace-3",
      sampled: true,
      sample_rate: 0.2,
      decision_point: "payment-service",
      timestamp: 1640995208
    }
  ]
  
  let consistent_result = validate_sampling_consistency(consistent_decisions, validator)
  assert_true(consistent_result.is_consistent)
  assert_eq(consistent_result.inconsistent_decisions.length(), 0)
  assert_eq(consistent_result.rate_violations.length(), 0)
  assert_eq(consistent_result.missing_decision_points.length(), 0)
  
  // Test with inconsistent decisions
  let inconsistent_decisions = [
    consistent_decisions[0],
    { consistent_decisions[1] | sampled: false },  // Inconsistent decision
    consistent_decisions[2]
  ]
  
  let inconsistent_result = validate_sampling_consistency(inconsistent_decisions, validator)
  assert_false(inconsistent_result.is_consistent)
  assert_eq(inconsistent_result.inconsistent_decisions.length(), 1)
  assert_eq(inconsistent_result.inconsistent_decisions[0], ("trace-1", true, false))
  
  // Test with rate violations
  let rate_violation_decisions = [
    {
      trace_id: "trace-1",
      sampled: true,
      sample_rate: 0.5,  // Above max rate
      decision_point: "api-gateway",
      timestamp: 1640995200
    },
    {
      trace_id: "trace-2",
      sampled: true,
      sample_rate: 0.5,  // Above max rate
      decision_point: "user-service",
      timestamp: 1640995201
    },
    {
      trace_id: "trace-3",
      sampled: true,
      sample_rate: 0.5,  // Above max rate
      decision_point: "payment-service",
      timestamp: 1640995202
    }
  ]
  
  let rate_result = validate_sampling_consistency(rate_violation_decisions, validator)
  assert_false(rate_result.is_consistent)
  assert_eq(rate_result.rate_violations.length(), 1)
  assert_eq(rate_result.rate_violations[0], "above_max_rate")
  
  // Test with missing decision points
  let missing_points_decisions = [
    {
      trace_id: "trace-1",
      sampled: true,
      sample_rate: 0.2,
      decision_point: "api-gateway",
      timestamp: 1640995200
    },
    {
      trace_id: "trace-1",
      sampled: true,
      sample_rate: 0.2,
      decision_point: "user-service",
      timestamp: 1640995201
    }
    // Missing payment-service decision point
  ]
  
  let missing_points_result = validate_sampling_consistency(missing_points_decisions, validator)
  assert_false(missing_points_result.is_consistent)
  assert_eq(missing_points_result.missing_decision_points.length(), 1)
  assert_eq(missing_points_result.missing_decision_points[0], "payment-service")
}