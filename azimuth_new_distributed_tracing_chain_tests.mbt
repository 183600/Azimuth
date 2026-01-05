// Azimuth Telemetry System - New Distributed Tracing Chain Tests
// This file contains test cases for distributed tracing chain functionality

// Test 1: Trace Chain Creation
test "trace chain creation operations" {
  // Create a trace chain with multiple spans
  let trace_id = "trace-12345"
  
  // Create root span
  let root_span = {
    trace_id: trace_id,
    span_id: "span-001",
    parent_span_id: None,
    operation_name: "api_gateway_request",
    service_name: "api-gateway",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "success",
    tags: [("http.method", "GET"), ("http.url", "/api/orders")]
  }
  
  // Create child spans
  let auth_span = {
    trace_id: trace_id,
    span_id: "span-002",
    parent_span_id: Some("span-001"),
    operation_name: "authenticate_user",
    service_name: "auth-service",
    start_time: 1640995205,
    end_time: 1640995220,
    status: "success",
    tags: [("user.id", "user-123")]
  }
  
  let order_span = {
    trace_id: trace_id,
    span_id: "span-003",
    parent_span_id: Some("span-001"),
    operation_name: "get_orders",
    service_name: "order-service",
    start_time: 1640995225,
    end_time: 1640995245,
    status: "success",
    tags: [("order.count", "5")]
  }
  
  // Create trace chain
  let trace_chain = [root_span, auth_span, order_span]
  
  // Verify trace chain structure
  assert_eq(trace_chain.length(), 3)
  
  // Verify root span
  assert_eq(trace_chain[0].trace_id, trace_id)
  assert_eq(trace_chain[0].span_id, "span-001")
  assert_eq(trace_chain[0].parent_span_id, None)
  assert_eq(trace_chain[0].operation_name, "api_gateway_request")
  
  // Verify child spans
  assert_eq(trace_chain[1].trace_id, trace_id)
  assert_eq(trace_chain[1].span_id, "span-002")
  assert_eq(trace_chain[1].parent_span_id, Some("span-001"))
  
  assert_eq(trace_chain[2].trace_id, trace_id)
  assert_eq(trace_chain[2].span_id, "span-003")
  assert_eq(trace_chain[2].parent_span_id, Some("span-001"))
}

// Test 2: Trace Chain Traversal
test "trace chain traversal operations" {
  // Create a complex trace chain
  let trace_id = "trace-67890"
  
  let trace_spans = [
    {
      trace_id: trace_id,
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "web_request",
      service_name: "web-frontend",
      start_time: 1640995200,
      end_time: 1640995300
    },
    {
      trace_id: trace_id,
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "api_call",
      service_name: "api-gateway",
      start_time: 1640995210,
      end_time: 1640995280
    },
    {
      trace_id: trace_id,
      span_id: "span-003",
      parent_span_id: Some("span-002"),
      operation_name: "user_lookup",
      service_name: "user-service",
      start_time: 1640995220,
      end_time: 1640995240
    },
    {
      trace_id: trace_id,
      span_id: "span-004",
      parent_span_id: Some("span-002"),
      operation_name: "order_lookup",
      service_name: "order-service",
      start_time: 1640995250,
      end_time: 1640995270
    }
  ]
  
  // Find root span function
  let find_root_span = fn(spans: Array[Dynamic]) {
    spans.find_fn(s) { s.parent_span_id == None }
  }
  
  // Find child spans function
  let find_child_spans = fn(spans: Array[Dynamic], parent_span_id: String) {
    spans.filter_fn(s) { s.parent_span_id == Some(parent_span_id) }
  }
  
  // Build trace tree function
  let build_trace_tree = fn(spans: Array[Dynamic], span_id: String) {
    let span = spans.find_fn(s) { s.span_id == span_id }
    let children = find_child_spans(spans, span_id)
    
    {
      span: span,
      children: children.map_fn(child) { build_trace_tree(spans, child.span_id) }
    }
  }
  
  // Test finding root span
  let root_span = find_root_span(trace_spans)
  assert_eq(root_span.span_id, "span-001")
  assert_eq(root_span.operation_name, "web_request")
  
  // Test finding child spans
  let api_children = find_child_spans(trace_spans, "span-002")
  assert_eq(api_children.length(), 2)
  assert_true(api_children.map_fn(c) { c.span_id }.contains("span-003"))
  assert_true(api_children.map_fn(c) { c.span_id }.contains("span-004"))
  
  // Test building trace tree
  let trace_tree = build_trace_tree(trace_spans, "span-001")
  assert_eq(trace_tree.span.span_id, "span-001")
  assert_eq(trace_tree.children.length(), 1)
  assert_eq(trace_tree.children[0].span.span_id, "span-002")
  assert_eq(trace_tree.children[0].children.length(), 2)
}

// Test 3: Trace Chain Analysis
test "trace chain analysis operations" {
  // Create trace chain with various performance characteristics
  let trace_spans = [
    {
      trace_id: "trace-analysis",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "request_handler",
      service_name: "api-gateway",
      start_time: 1640995200,
      end_time: 1640995300,
      status: "success"
    },
    {
      trace_id: "trace-analysis",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "database_query",
      service_name: "user-service",
      start_time: 1640995210,
      end_time: 1640995270,
      status: "success"
    },
    {
      trace_id: "trace-analysis",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      operation_name: "cache_lookup",
      service_name: "cache-service",
      start_time: 1640995280,
      end_time: 1640995290,
      status: "success"
    },
    {
      trace_id: "trace-analysis",
      span_id: "span-004",
      parent_span_id: Some("span-002"),
      operation_name: "slow_operation",
      service_name: "user-service",
      start_time: 1640995220,
      end_time: 1640995260,
      status: "success"
    }
  ]
  
  // Calculate span duration
  let span_duration = fn(span: Dynamic) {
    span.end_time - span.start_time
  }
  
  // Find slowest span
  let find_slowest_span = fn(spans: Array[Dynamic]) {
    spans.reduce(fn(slowest, current) {
      let slowest_duration = span_duration(slowest)
      let current_duration = span_duration(current)
      if current_duration > slowest_duration {
        current
      } else {
        slowest
      }
    }, spans[0])
  }
  
  // Calculate trace duration
  let calculate_trace_duration = fn(spans: Array[Dynamic]) {
    let start_times = spans.map_fn(s) { s.start_time }
    let end_times = spans.map_fn(s) { s.end_time }
    
    let trace_start = start_times.reduce(fn(min, time) { 
      if time < min { time } else { min } 
    }, start_times[0])
    
    let trace_end = end_times.reduce(fn(max, time) { 
      if time > max { time } else { max } 
    }, end_times[0])
    
    trace_end - trace_start
  }
  
  // Find service bottlenecks
  let find_service_bottlenecks = fn(spans: Array[Dynamic]) {
    let mut services = []
    let mut processed_services = []
    
    for span in spans {
      let service_name = span.service_name
      
      if not(processed_services.contains(service_name)) {
        let service_spans = spans.filter_fn(s) { s.service_name == service_name }
        let total_duration = service_spans.reduce(fn(total, s) { 
          total + span_duration(s) 
        }, 0)
        
        services = services.push({
          service: service_name,
          span_count: service_spans.length(),
          total_duration: total_duration,
          avg_duration: total_duration / service_spans.length()
        })
        
        processed_services = processed_services.push(service_name)
      }
    }
    
    services.sort(fn(a, b) { b.total_duration - a.total_duration })
  }
  
  // Test span duration calculation
  let root_duration = span_duration(trace_spans[0])
  assert_eq(root_duration, 100) // 1640995300 - 1640995200
  
  let db_duration = span_duration(trace_spans[1])
  assert_eq(db_duration, 60) // 1640995270 - 1640995210
  
  // Test finding slowest span
  let slowest_span = find_slowest_span(trace_spans)
  assert_eq(slowest_span.span_id, "span-001") // Root span with 100ms duration
  
  // Test trace duration calculation
  let trace_duration = calculate_trace_duration(trace_spans)
  assert_eq(trace_duration, 100) // Same as root span duration
  
  // Test service bottleneck analysis
  let bottlenecks = find_service_bottlenecks(trace_spans)
  assert_eq(bottlenecks.length(), 3) // api-gateway, user-service, cache-service
  
  // user-service should be the bottleneck
  assert_eq(bottlenecks[0].service, "user-service")
  assert_eq(bottlenecks[0].span_count, 2)
  assert_eq(bottlenecks[0].total_duration, 80) // 60 + 20
  assert_eq(bottlenecks[0].avg_duration, 40) // 80 / 2
}

// Test 4: Trace Chain Error Propagation
test "trace chain error propagation operations" {
  // Create trace chain with errors
  let error_trace_spans = [
    {
      trace_id: "trace-error",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "api_request",
      service_name: "api-gateway",
      start_time: 1640995200,
      end_time: 1640995300,
      status: "error",
      error_message: "Service unavailable"
    },
    {
      trace_id: "trace-error",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "database_call",
      service_name: "user-service",
      start_time: 1640995210,
      end_time: 1640995250,
      status: "error",
      error_message: "Connection timeout"
    },
    {
      trace_id: "trace-error",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      operation_name: "cache_lookup",
      service_name: "cache-service",
      start_time: 1640995260,
      end_time: 1640995270,
      status: "success"
    }
  ]
  
  // Find error spans
  let find_error_spans = fn(spans: Array[Dynamic]) {
    spans.filter_fn(s) { s.status == "error" }
  }
  
  // Trace error path
  let trace_error_path = fn(spans: Array[Dynamic], error_span_id: String) {
    let mut path = []
    let mut current_span_id = error_span_id
    
    while true {
      let current_span = spans.find_fn(s) { s.span_id == current_span_id }
      
      match current_span {
        Some(span) => {
          path = path.push(span)
          
          match span.parent_span_id {
            Some(parent_id) => {
              current_span_id = parent_id
            }
            None => {
              break
            }
          }
        }
        None => {
          break
        }
      }
    }
    
    path.reverse()
  }
  
  // Analyze error impact
  let analyze_error_impact = fn(spans: Array[Dynamic]) {
    let error_spans = find_error_spans(spans)
    let total_spans = spans.length()
    let error_rate = (error_spans.length() * 100) / total_spans
    
    let mut affected_services = []
    for error_span in error_spans {
      if not(affected_services.contains(error_span.service_name)) {
        affected_services = affected_services.push(error_span.service_name)
      }
    }
    
    {
      total_spans: total_spans,
      error_spans: error_spans.length(),
      error_rate: error_rate,
      affected_services: affected_services,
      error_paths: error_spans.map_fn(e) { trace_error_path(spans, e.span_id) }
    }
  }
  
  // Test finding error spans
  let errors = find_error_spans(error_trace_spans)
  assert_eq(errors.length(), 2)
  assert_true(errors.map_fn(e) { e.span_id }.contains("span-001"))
  assert_true(errors.map_fn(e) { e.span_id }.contains("span-002"))
  
  // Test error path tracing
  let db_error_path = trace_error_path(error_trace_spans, "span-002")
  assert_eq(db_error_path.length(), 2)
  assert_eq(db_error_path[0].span_id, "span-001") // Root
  assert_eq(db_error_path[1].span_id, "span-002") // Error span
  
  // Test error impact analysis
  let impact = analyze_error_impact(error_trace_spans)
  assert_eq(impact.total_spans, 3)
  assert_eq(impact.error_spans, 2)
  assert_eq(impact.error_rate, 66) // floor(2 * 100 / 3)
  assert_eq(impact.affected_services.length(), 2)
  assert_true(impact.affected_services.contains("api-gateway"))
  assert_true(impact.affected_services.contains("user-service"))
}

// Test 5: Trace Chain Context Propagation
test "trace chain context propagation operations" {
  // Create trace context
  let trace_context = {
    trace_id: "trace-context",
    baggage: [
      ("user.id", "user-123"),
      ("request.id", "req-456"),
      ("session.id", "session-789")
    ],
    sampling_decision: true
  }
  
  // Create span with context propagation
  let create_span_with_context = fn(parent_context: Dynamic, span_id: String, operation_name: String, service_name: String) {
    {
      trace_id: parent_context.trace_id,
      span_id: span_id,
      operation_name: operation_name,
      service_name: service_name,
      baggage: parent_context.baggage,
      sampling_decision: parent_context.sampling_decision
    }
  }
  
  // Add baggage item
  let add_baggage_item = fn(context: Dynamic, key: String, value: String) {
    let existing_item = context.baggage.find_fn(item) { item.0 == key }
    
    match existing_item {
      Some(_) => {
        // Update existing item
        context.baggage.map_fn(item) {
          if item.0 == key {
            (key, value)
          } else {
            item
          }
        }
      }
      None => {
        // Add new item
        context.baggage.push((key, value))
      }
    }
  }
  
  // Create trace chain with context propagation
  let root_span = create_span_with_context(trace_context, "span-001", "web_request", "web-frontend")
  let auth_span = create_span_with_context(root_span, "span-002", "authenticate", "auth-service")
  
  // Add baggage during processing
  let updated_auth_context = { auth_span | 
    baggage: add_baggage_item(auth_span, "auth.method", "oauth2") 
  }
  
  let order_span = create_span_with_context(updated_auth_context, "span-003", "get_orders", "order-service")
  
  // Verify context propagation
  assert_eq(root_span.trace_id, "trace-context")
  assert_eq(auth_span.trace_id, "trace-context")
  assert_eq(order_span.trace_id, "trace-context")
  
  // Verify baggage propagation
  assert_eq(root_span.baggage.length(), 3)
  assert_eq(auth_span.baggage.length(), 3)
  assert_eq(order_span.baggage.length(), 4) // Added auth.method
  
  // Verify specific baggage items
  let get_baggage_value = fn(baggage: Array[(String, String)], key: String) {
    let item = baggage.find_fn(i) { i.0 == key }
    match item {
      Some((_, value)) => Some(value)
      None => None
    }
  }
  
  assert_eq(get_baggage_value(root_span.baggage, "user.id"), Some("user-123"))
  assert_eq(get_baggage_value(auth_span.baggage, "user.id"), Some("user-123"))
  assert_eq(get_baggage_value(order_span.baggage, "user.id"), Some("user-123"))
  
  assert_eq(get_baggage_value(order_span.baggage, "auth.method"), Some("oauth2"))
  assert_eq(get_baggage_value(root_span.baggage, "auth.method"), None)
}