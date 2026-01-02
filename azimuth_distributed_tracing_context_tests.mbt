// Azimuth Distributed Tracing Context Tests
// This file contains test cases for distributed tracing context operations

// Test 1: Trace Context Creation and Validation
test "trace context creation and validation" {
  // Create trace identifiers
  let trace_id = "a1b2c3d4e5f678901234567890123456"
  let span_id = "12345678"
  let parent_span_id = "87654321"
  
  // Validate trace ID format (32 hex characters)
  assert_eq(trace_id.length(), 32)
  assert_true(trace_id.chars().all(fn(c) { 
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') 
  }))
  
  // Validate span ID format (8 hex characters)
  assert_eq(span_id.length(), 8)
  assert_true(span_id.chars().all(fn(c) { 
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') 
  }))
  
  // Validate parent span ID format
  assert_eq(parent_span_id.length(), 8)
  assert_true(parent_span_id.chars().all(fn(c) { 
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') 
  }))
}

// Test 2: Span Hierarchy Management
test "span hierarchy management" {
  // Create span hierarchy
  let root_span = ("trace-001", "span-001", "")
  let child_span1 = ("trace-001", "span-002", "span-001")
  let child_span2 = ("trace-001", "span-003", "span-001")
  let grandchild_span = ("trace-001", "span-004", "span-002")
  
  // Build hierarchy structure
  let spans = [root_span, child_span1, child_span2, grandchild_span]
  
  // Find root spans (no parent)
  let root_spans = spans.filter(fn(span) { span.2 == "" })
  assert_eq(root_spans.length(), 1)
  assert_eq(root_spans[0].1, "span-001")
  
  // Find children of root span
  let root_children = spans.filter(fn(span) { span.2 == "span-001" })
  assert_eq(root_children.length(), 2)
  assert_true(root_children.map(fn(s) { s.1 }).contains("span-002"))
  assert_true(root_children.map(fn(s) { s.1 }).contains("span-003"))
  
  // Find grandchildren
  let grandchildren = spans.filter(fn(span) { 
    span.2 == "span-002" || span.2 == "span-003" 
  })
  assert_eq(grandchildren.length(), 1)
  assert_eq(grandchildren[0].1, "span-004")
}

// Test 3: Context Propagation
test "context propagation across services" {
  // Initial context from service A
  let service_a_context = {
    "trace_id": "trace-123456",
    "span_id": "span-a1",
    "baggage": [
      ("user_id", "user-789"),
      ("request_id", "req-456"),
      ("service_version", "1.2.3")
    ]
  }
  
  // Service B receives and extends context
  let service_b_context = {
    "trace_id": service_a_context.trace_id,
    "span_id": "span-b1",
    "parent_span_id": service_a_context.span_id,
    "baggage": service_a_context.baggage.push(("service_b_latency", "150ms"))
  }
  
  // Service C receives and extends context
  let service_c_context = {
    "trace_id": service_b_context.trace_id,
    "span_id": "span-c1",
    "parent_span_id": service_b_context.span_id,
    "baggage": service_b_context.baggage.push(("service_c_result", "success"))
  }
  
  // Verify trace ID consistency
  assert_eq(service_a_context.trace_id, service_b_context.trace_id)
  assert_eq(service_b_context.trace_id, service_c_context.trace_id)
  
  // Verify parent-child relationships
  assert_eq(service_b_context.parent_span_id, service_a_context.span_id)
  assert_eq(service_c_context.parent_span_id, service_b_context.span_id)
  
  // Verify baggage propagation
  assert_eq(service_c_context.baggage.length(), 5)
  assert_true(service_c_context.baggage.map(fn(item) { item.0 }).contains("user_id"))
  assert_true(service_c_context.baggage.map(fn(item) { item.0 }).contains("request_id"))
  assert_true(service_c_context.baggage.map(fn(item) { item.0 }).contains("service_version"))
  assert_true(service_c_context.baggage.map(fn(item) { item.0 }).contains("service_b_latency"))
  assert_true(service_c_context.baggage.map(fn(item) { item.0 }).contains("service_c_result"))
}

// Test 4: Trace Sampling
test "trace sampling strategies" {
  let trace_ids = [
    "trace-000001",
    "trace-000002", 
    "trace-000003",
    "trace-000004",
    "trace-000005",
    "trace-000006",
    "trace-000007",
    "trace-000008",
    "trace-000009",
    "trace-000010"
  ]
  
  // Sample based on trace ID (deterministic sampling)
  let sampling_rate = 0.2 // 20% sampling rate
  let sampled_traces = []
  
  for trace_id in trace_ids {
    // Simple hash-based sampling (using last character for demo)
    let last_char = trace_id[trace_id.length() - 1]
    let hash_value = if last_char >= '0' && last_char <= '9' {
      Int::from_char(last_char) - Int::from_char('0')
    } else {
      0
    }
    
    let should_sample = Int::to_float(hash_value) / 10.0 < sampling_rate
    if should_sample {
      sampled_traces = sampled_traces.push(trace_id)
    }
  }
  
  // With 20% sampling rate and our simple hash, we expect 2 traces
  assert_eq(sampled_traces.length(), 2)
  
  // Verify specific traces are sampled (0 and 1 based on our hash)
  assert_true(sampled_traces.contains("trace-000001"))
  assert_true(sampled_traces.contains("trace-000002"))
}

// Test 5: Span Event Management
test "span event management" {
  let span_events = []
  
  // Add events to span
  span_events = span_events.push({
    "timestamp": 1640995200,
    "name": "db.query.start",
    "attributes": [
      ("db.statement", "SELECT * FROM users"),
      ("db.type", "postgresql")
    ]
  })
  
  span_events = span_events.push({
    "timestamp": 1640995210,
    "name": "db.query.end",
    "attributes": [
      ("db.rows_affected", "25"),
      ("db.duration_ms", "10")
    ]
  })
  
  span_events = span_events.push({
    "timestamp": 1640995225,
    "name": "cache.hit",
    "attributes": [
      ("cache.key", "user:123"),
      ("cache.size", "1024")
    ]
  })
  
  assert_eq(span_events.length(), 3)
  
  // Find query events
  let query_events = span_events.filter(fn(event) { 
    event.name.contains("db.query") 
  })
  assert_eq(query_events.length(), 2)
  
  // Calculate query duration
  if query_events.length() == 2 {
    let start_event = query_events[0]
    let end_event = query_events[1]
    let duration = end_event.timestamp - start_event.timestamp
    assert_eq(duration, 10)
  }
  
  // Find cache events
  let cache_events = span_events.filter(fn(event) { 
    event.name.contains("cache") 
  })
  assert_eq(cache_events.length(), 1)
  assert_eq(cache_events[0].attributes.filter(fn(attr) { attr.0 == "cache.key" })[0].1, "user:123")
}

// Test 6: Span Attribute Management
test "span attribute management" {
  let span_attributes = []
  
  // Add standard attributes
  span_attributes = span_attributes.push(("service.name", "user-service"))
  span_attributes = span_attributes.push(("service.version", "1.2.3"))
  span_attributes = span_attributes.push(("service.instance.id", "instance-001"))
  
  // Add custom attributes
  span_attributes = span_attributes.push(("user.id", "user-12345"))
  span_attributes = span_attributes.push(("user.role", "admin"))
  span_attributes = span_attributes.push(("request.path", "/api/users"))
  span_attributes = span_attributes.push(("request.method", "GET"))
  span_attributes = span_attributes.push(("response.status_code", "200"))
  
  assert_eq(span_attributes.length(), 8)
  
  // Find attributes by prefix
  let service_attributes = span_attributes.filter(fn(attr) { 
    attr.0.starts_with("service.") 
  })
  assert_eq(service_attributes.length(), 3)
  
  let user_attributes = span_attributes.filter(fn(attr) { 
    attr.0.starts_with("user.") 
  })
  assert_eq(user_attributes.length(), 2)
  
  let request_attributes = span_attributes.filter(fn(attr) { 
    attr.0.starts_with("request.") 
  })
  assert_eq(request_attributes.length(), 2)
  
  // Check specific attribute values
  let service_name = span_attributes.filter(fn(attr) { attr.0 == "service.name" })[0].1
  let user_role = span_attributes.filter(fn(attr) { attr.0 == "user.role" })[0].1
  let response_status = span_attributes.filter(fn(attr) { attr.0 == "response.status_code" })[0].1
  
  assert_eq(service_name, "user-service")
  assert_eq(user_role, "admin")
  assert_eq(response_status, "200")
}

// Test 7: Cross-Service Trace Correlation
test "cross-service trace correlation" {
  // Trace data from multiple services
  let service_a_spans = [
    ("trace-001", "span-a1", "", 1640995200, 1640995250), // 50ms
    ("trace-001", "span-a2", "span-a1", 1640995210, 1640995230) // 20ms
  ]
  
  let service_b_spans = [
    ("trace-001", "span-b1", "", 1640995220, 1640995280), // 60ms
    ("trace-001", "span-b2", "span-b1", 1640995235, 1640995255) // 20ms
  ]
  
  let service_c_spans = [
    ("trace-001", "span-c1", "", 1640995240, 1640995290), // 50ms
    ("trace-001", "span-c2", "span-c1", 1640995250, 1640995270) // 20ms
  ]
  
  // Combine all spans
  let all_spans = []
  all_spans = all_spans.concat(service_a_spans.map(fn(s) { ("service-a", s) }))
  all_spans = all_spans.concat(service_b_spans.map(fn(s) { ("service-b", s) }))
  all_spans = all_spans.concat(service_c_spans.map(fn(s) { ("service-c", s) }))
  
  assert_eq(all_spans.length(), 6)
  
  // Verify all spans have same trace ID
  let trace_ids = all_spans.map(fn(span) { span.1.0 })
  let unique_trace_ids = trace_ids.fold([], fn(acc, id) {
    if acc.contains(id) { acc } else { acc.push(id) }
  })
  assert_eq(unique_trace_ids.length(), 1)
  assert_eq(unique_trace_ids[0], "trace-001")
  
  // Calculate service durations
  let service_durations = []
  let services = ["service-a", "service-b", "service-c"]
  
  for service in services {
    let service_spans = all_spans.filter(fn(span) { span.0 == service })
    let total_duration = service_spans.reduce(fn(acc, span) { 
      acc + (span.1.4 - span.1.3) 
    }, 0)
    service_durations = service_durations.push((service, total_duration))
  }
  
  assert_eq(service_durations.length(), 3)
  
  // Find total trace duration
  let all_timestamps = []
  for span in all_spans {
    all_timestamps = all_timestamps.push(span.1.3)
    all_timestamps = all_timestamps.push(span.1.4)
  }
  
  let trace_start = all_timestamps.reduce(fn(acc, ts) { if ts < acc { ts } else { acc } }, all_timestamps[0])
  let trace_end = all_timestamps.reduce(fn(acc, ts) { if ts > acc { ts } else { acc } }, all_timestamps[0])
  let trace_duration = trace_end - trace_start
  
  assert_eq(trace_duration, 90) // From 1640995200 to 1640995290
}

// Test 8: Trace Context Error Handling
test "trace context error handling" {
  // Test malformed trace ID
  let malformed_trace_id = "invalid-trace-id"
  let valid_trace_id = "a1b2c3d4e5f678901234567890123456"
  
  let validate_trace_id = fn(trace_id) {
    trace_id.length() == 32 && trace_id.chars().all(fn(c) { 
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') 
    })
  }
  
  assert_false(validate_trace_id(malformed_trace_id))
  assert_true(validate_trace_id(valid_trace_id))
  
  // Test context extraction failure
  let extract_context_from_headers = fn(headers) {
    let trace_header = headers.filter(fn(h) { h.0 == "traceparent" })
    if trace_header.length() == 0 {
      None
    } else {
      let header_value = trace_header[0].1
      let parts = header_value.split("-")
      if parts.length() >= 3 {
        Some((parts[1], parts[2])) // (trace_id, span_id)
      } else {
        None
      }
    }
  }
  
  let valid_headers = [
    ("traceparent", "00-a1b2c3d4e5f678901234567890123456-12345678-01"),
    ("baggage", "user_id=user123")
  ]
  
  let invalid_headers = [
    ("content-type", "application/json"),
    ("authorization", "Bearer token123")
  ]
  
  let malformed_headers = [
    ("traceparent", "invalid-format"),
    ("baggage", "user_id=user123")
  ]
  
  let valid_context = extract_context_from_headers(valid_headers)
  let invalid_context = extract_context_from_headers(invalid_headers)
  let malformed_context = extract_context_from_headers(malformed_headers)
  
  match valid_context {
    Some((trace_id, span_id)) => {
      assert_eq(trace_id, "a1b2c3d4e5f678901234567890123456")
      assert_eq(span_id, "12345678")
    }
    None => assert_true(false)
  }
  
  match invalid_context {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match malformed_context {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}