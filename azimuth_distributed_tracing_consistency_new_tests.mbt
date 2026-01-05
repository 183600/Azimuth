// Azimuth Distributed Tracing Consistency Tests
// This file contains test cases for distributed tracing consistency across services

// Test 1: Trace Context Propagation
test "trace context propagation across services" {
  // Create initial trace context
  let trace_id = "trace_1234567890abcdef"
  let parent_span_id = "span_1111111111111111"
  let initial_context = SpanContext::new(trace_id, parent_span_id, true, "sampled")
  
  // Simulate service A receiving request
  let service_a_context = SpanContext::new(
    SpanContext::trace_id(initial_context),
    "span_aaaaaaaaaaaaaaaa",
    SpanContext::is_sampled(initial_context),
    "service_a_state"
  )
  
  // Verify trace ID is preserved
  assert_eq(SpanContext::trace_id(service_a_context), trace_id)
  assert_eq(SpanContext::span_id(service_a_context), "span_aaaaaaaaaaaaaaaa")
  assert_true(SpanContext::is_sampled(service_a_context))
  
  // Simulate service A calling service B
  let service_b_context = SpanContext::new(
    SpanContext::trace_id(service_a_context),
    "span_bbbbbbbbbbbbbbbb",
    SpanContext::is_sampled(service_a_context),
    "service_b_state"
  )
  
  // Verify trace ID is still preserved
  assert_eq(SpanContext::trace_id(service_b_context), trace_id)
  assert_eq(SpanContext::span_id(service_b_context), "span_bbbbbbbbbbbbbbbb")
  assert_true(SpanContext::is_sampled(service_b_context))
  
  // Simulate service B calling service C
  let service_c_context = SpanContext::new(
    SpanContext::trace_id(service_b_context),
    "span_cccccccccccccccc",
    SpanContext::is_sampled(service_b_context),
    "service_c_state"
  )
  
  // Verify trace ID consistency across all services
  assert_eq(SpanContext::trace_id(service_c_context), trace_id)
  assert_eq(SpanContext::span_id(service_c_context), "span_cccccccccccccccc")
  assert_true(SpanContext::is_sampled(service_c_context))
  
  // All contexts should have the same trace ID
  assert_eq(
    SpanContext::trace_id(initial_context),
    SpanContext::trace_id(service_a_context)
  )
  assert_eq(
    SpanContext::trace_id(service_a_context),
    SpanContext::trace_id(service_b_context)
  )
  assert_eq(
    SpanContext::trace_id(service_b_context),
    SpanContext::trace_id(service_c_context)
  )
}

// Test 2: Span Relationship Consistency
test "span relationship consistency" {
  // Create a trace with multiple services
  let trace_id = "trace_fedcba0987654321"
  
  // Service A: Root span
  let service_a_root = {
    trace_id: trace_id,
    span_id: "span_root_aaaaaaaaaa",
    parent_span_id: None,
    service_name: "service-a",
    operation_name: "api_gateway_request",
    start_time: 1640995200,
    end_time: 1640995350,
    status: "ok"
  }
  
  // Service A: Child span for auth
  let service_a_auth = {
    trace_id: trace_id,
    span_id: "span_auth_aaaaaaaaaa",
    parent_span_id: Some("span_root_aaaaaaaaaa"),
    service_name: "service-a",
    operation_name: "authenticate_user",
    start_time: 1640995210,
    end_time: 1640995250,
    status: "ok"
  }
  
  // Service B: Child span called by Service A
  let service_b_main = {
    trace_id: trace_id,
    span_id: "span_main_bbbbbbbbbb",
    parent_span_id: Some("span_root_aaaaaaaaaa"),
    service_name: "service-b",
    operation_name: "process_user_data",
    start_time: 1640995260,
    end_time: 1640995300,
    status: "ok"
  }
  
  // Service B: Database operation
  let service_b_db = {
    trace_id: trace_id,
    span_id: "span_db_bbbbbbbbbbb",
    parent_span_id: Some("span_main_bbbbbbbbbb"),
    service_name: "service-b",
    operation_name: "database_query",
    start_time: 1640995270,
    end_time: 1640995290,
    status: "ok"
  }
  
  // Service C: Called by Service B
  let service_c_notification = {
    trace_id: trace_id,
    span_id: "span_notif_cccccccc",
    parent_span_id: Some("span_main_bbbbbbbbbb"),
    service_name: "service-c",
    operation_name: "send_notification",
    start_time: 1640995310,
    end_time: 1640995340,
    status: "ok"
  }
  
  // Collect all spans
  let all_spans = [
    service_a_root,
    service_a_auth,
    service_b_main,
    service_b_db,
    service_c_notification
  ]
  
  // Verify all spans have the same trace ID
  for span in all_spans {
    assert_eq(span.trace_id, trace_id)
  }
  
  // Verify parent-child relationships
  let find_span = fn(span_id: String) {
    let mut found = None
    for span in all_spans {
      if span.span_id == span_id {
        found = Some(span)
      }
    }
    found
  }
  
  // Check Service A auth span parent
  let auth_span = find_span("span_auth_aaaaaaaaaa").unwrap()
  match auth_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "span_root_aaaaaaaaaa")
    None => assert_true(false)
  }
  
  // Check Service B main span parent
  let main_span = find_span("span_main_bbbbbbbbbb").unwrap()
  match main_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "span_root_aaaaaaaaaa")
    None => assert_true(false)
  }
  
  // Check Service B DB span parent
  let db_span = find_span("span_db_bbbbbbbbbbb").unwrap()
  match db_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "span_main_bbbbbbbbbb")
    None => assert_true(false)
  }
  
  // Check Service C notification span parent
  let notif_span = find_span("span_notif_cccccccc").unwrap()
  match notif_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "span_main_bbbbbbbbbb")
    None => assert_true(false)
  }
  
  // Verify root span has no parent
  let root_span = find_span("span_root_aaaaaaaaaa").unwrap()
  match root_span.parent_span_id {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify timeline consistency
  assert_true(service_a_root.start_time <= service_a_auth.start_time)
  assert_true(service_a_auth.end_time <= service_b_main.start_time)
  assert_true(service_b_main.start_time <= service_b_db.start_time)
  assert_true(service_b_db.end_time <= service_c_notification.start_time)
  assert_true(service_c_notification.end_time <= service_a_root.end_time)
}

// Test 3: Baggage Propagation Consistency
test "baggage propagation consistency across services" {
  // Initialize baggage in service A
  let initial_baggage = Baggage::new()
  let service_a_baggage = Baggage::set_entry(initial_baggage, "user.id", "user_12345")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "request.id", "req_abcdef")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "tenant.id", "tenant_67890")
  
  // Service B receives and adds to baggage
  let service_b_baggage = Baggage::set_entry(service_a_baggage, "service.b.version", "1.2.3")
  let service_b_baggage = Baggage::set_entry(service_b_baggage, "service.b.region", "us-west-2")
  
  // Service C receives and adds to baggage
  let service_c_baggage = Baggage::set_entry(service_b_baggage, "service.c.version", "2.1.0")
  let service_c_baggage = Baggage::set_entry(service_c_baggage, "service.c.zone", "us-west-2a")
  
  // Verify original baggage entries are preserved
  let user_id = Baggage::get_entry(service_c_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user_12345")
    None => assert_true(false)
  }
  
  let request_id = Baggage::get_entry(service_c_baggage, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req_abcdef")
    None => assert_true(false)
  }
  
  let tenant_id = Baggage::get_entry(service_c_baggage, "tenant.id")
  match tenant_id {
    Some(value) => assert_eq(value, "tenant_67890")
    None => assert_true(false)
  }
  
  // Verify service B additions are preserved
  let service_b_version = Baggage::get_entry(service_c_baggage, "service.b.version")
  match service_b_version {
    Some(value) => assert_eq(value, "1.2.3")
    None => assert_true(false)
  }
  
  let service_b_region = Baggage::get_entry(service_c_baggage, "service.b.region")
  match service_b_region {
    Some(value) => assert_eq(value, "us-west-2")
    None => assert_true(false)
  }
  
  // Verify service C additions are present
  let service_c_version = Baggage::get_entry(service_c_baggage, "service.c.version")
  match service_c_version {
    Some(value) => assert_eq(value, "2.1.0")
    None => assert_true(false)
  }
  
  let service_c_zone = Baggage::get_entry(service_c_baggage, "service.c.zone")
  match service_c_zone {
    Some(value) => assert_eq(value, "us-west-2a")
    None => assert_true(false)
  }
  
  // Test baggage immutability (previous versions unchanged)
  let service_b_user_id = Baggage::get_entry(service_b_baggage, "user.id")
  match service_b_user_id {
    Some(value) => assert_eq(value, "user_12345")
    None => assert_true(false)
  }
  
  // Service C specific entries should not be in service B baggage
  let service_c_zone_in_b = Baggage::get_entry(service_b_baggage, "service.c.zone")
  match service_c_zone_in_b {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Cross-Service Sampling Consistency
test "cross-service sampling consistency" {
  // Test with sampling decision made at entry point
  let trace_id_sampled = "trace_sampled_123456"
  let trace_id_not_sampled = "trace_not_sampled_789012"
  
  // Sampled trace
  let sampled_context = SpanContext::new(trace_id_sampled, "span_1111111111111111", true, "sampled")
  
  // Propagate through services
  let service_a_sampled = SpanContext::new(
    SpanContext::trace_id(sampled_context),
    "span_aaaaaaaaaaaaaaaa",
    SpanContext::is_sampled(sampled_context),
    "service_a"
  )
  
  let service_b_sampled = SpanContext::new(
    SpanContext::trace_id(service_a_sampled),
    "span_bbbbbbbbbbbbbbbb",
    SpanContext::is_sampled(service_a_sampled),
    "service_b"
  )
  
  let service_c_sampled = SpanContext::new(
    SpanContext::trace_id(service_b_sampled),
    "span_cccccccccccccccc",
    SpanContext::is_sampled(service_b_sampled),
    "service_c"
  )
  
  // All services should respect the original sampling decision
  assert_true(SpanContext::is_sampled(sampled_context))
  assert_true(SpanContext::is_sampled(service_a_sampled))
  assert_true(SpanContext::is_sampled(service_b_sampled))
  assert_true(SpanContext::is_sampled(service_c_sampled))
  
  // Not sampled trace
  let not_sampled_context = SpanContext::new(trace_id_not_sampled, "span_2222222222222222", false, "not_sampled")
  
  // Propagate through services
  let service_a_not_sampled = SpanContext::new(
    SpanContext::trace_id(not_sampled_context),
    "span_aaaaaaaaaaaaaaaa",
    SpanContext::is_sampled(not_sampled_context),
    "service_a"
  )
  
  let service_b_not_sampled = SpanContext::new(
    SpanContext::trace_id(service_a_not_sampled),
    "span_bbbbbbbbbbbbbbbb",
    SpanContext::is_sampled(service_a_not_sampled),
    "service_b"
  )
  
  let service_c_not_sampled = SpanContext::new(
    SpanContext::trace_id(service_b_not_sampled),
    "span_cccccccccccccccc",
    SpanContext::is_sampled(service_b_not_sampled),
    "service_c"
  )
  
  // All services should respect the original sampling decision
  assert_false(SpanContext::is_sampled(not_sampled_context))
  assert_false(SpanContext::is_sampled(service_a_not_sampled))
  assert_false(SpanContext::is_sampled(service_b_not_sampled))
  assert_false(SpanContext::is_sampled(service_c_not_sampled))
  
  // Verify trace IDs are different
  assert_neq(trace_id_sampled, trace_id_not_sampled)
  
  // Verify sampling decisions are consistent within each trace
  assert_eq(
    SpanContext::is_sampled(service_a_sampled),
    SpanContext::is_sampled(service_b_sampled)
  )
  assert_eq(
    SpanContext::is_sampled(service_b_sampled),
    SpanContext::is_sampled(service_c_sampled)
  )
  
  assert_eq(
    SpanContext::is_sampled(service_a_not_sampled),
    SpanContext::is_sampled(service_b_not_sampled)
  )
  assert_eq(
    SpanContext::is_sampled(service_b_not_sampled),
    SpanContext::is_sampled(service_c_not_sampled)
  )
}

// Test 5: Distributed Trace Reconstruction
test "distributed trace reconstruction" {
  // Simulate spans collected from different services
  let service_a_spans = [
    {
      trace_id: "trace_reconstruction_123",
      span_id: "span_root_aaaaaa",
      parent_span_id: None,
      service_name: "api-gateway",
      operation_name: "handle_request",
      start_time: 1640995200,
      end_time: 1640995280,
      status: "ok",
      tags: [("http.method", "GET"), ("http.url", "/api/users")]
    },
    {
      trace_id: "trace_reconstruction_123",
      span_id: "span_auth_aaaaaa",
      parent_span_id: Some("span_root_aaaaaa"),
      service_name: "api-gateway",
      operation_name: "authenticate",
      start_time: 1640995210,
      end_time: 1640995240,
      status: "ok",
      tags: [("auth.method", "jwt"), ("auth.user_id", "user_123")]
    }
  ]
  
  let service_b_spans = [
    {
      trace_id: "trace_reconstruction_123",
      span_id: "span_main_bbbbbb",
      parent_span_id: Some("span_root_aaaaaa"),
      service_name: "user-service",
      operation_name: "get_user_profile",
      start_time: 1640995250,
      end_time: 1640995270,
      status: "ok",
      tags: [("user.id", "user_123"), ("cache.hit", "false")]
    },
    {
      trace_id: "trace_reconstruction_123",
      span_id: "span_db_bbbbbbb",
      parent_span_id: Some("span_main_bbbbbb"),
      service_name: "user-service",
      operation_name: "database_query",
      start_time: 1640995255,
      end_time: 1640995265,
      status: "ok",
      tags: [("db.table", "users"), ("db.operation", "select")]
    }
  ]
  
  let service_c_spans = [
    {
      trace_id: "trace_reconstruction_123",
      span_id: "span_notif_ccccc",
      parent_span_id: Some("span_main_bbbbbb"),
      service_name: "notification-service",
      operation_name: "send_welcome_email",
      start_time: 1640995271,
      end_time: 1640995278,
      status: "ok",
      tags: [("notification.type", "email"), ("notification.template", "welcome")]
    }
  ]
  
  // Combine all spans
  let all_spans = service_a_spans + service_b_spans + service_c_spans
  
  // Verify all spans belong to the same trace
  for span in all_spans {
    assert_eq(span.trace_id, "trace_reconstruction_123")
  }
  
  // Find root span
  let root_spans = all_spans.filter(fn(span) { span.parent_span_id == None })
  assert_eq(root_spans.length(), 1)
  assert_eq(root_spans[0].operation_name, "handle_request")
  assert_eq(root_spans[0].service_name, "api-gateway")
  
  // Build span tree
  let find_children = fn(parent_id: String, spans: Array[{trace_id: String, span_id: String, parent_span_id: Option[String], service_name: String, operation_name: String, start_time: Int, end_time: Int, status: String, tags: Array[(String, String)}]) {
    spans.filter(fn(span) {
      match span.parent_span_id {
        Some(id) => id == parent_id
        None => false
      }
    })
  }
  
  // Find children of root span
  let root_children = find_children("span_root_aaaaaa", all_spans)
  assert_eq(root_children.length(), 2)
  
  let auth_span = root_children.filter(fn(s) { s.operation_name == "authenticate" })[0]
  let main_span = root_children.filter(fn(s) { s.operation_name == "get_user_profile" })[0]
  
  // Verify auth span properties
  assert_eq(auth_span.service_name, "api-gateway")
  assert_eq(auth_span.span_id, "span_auth_aaaaaa")
  
  // Verify main span properties
  assert_eq(main_span.service_name, "user-service")
  assert_eq(main_span.span_id, "span_main_bbbbbb")
  
  // Find children of main span
  let main_children = find_children("span_main_bbbbbb", all_spans)
  assert_eq(main_children.length(), 2)
  
  let db_span = main_children.filter(fn(s) { s.operation_name == "database_query" })[0]
  let notif_span = main_children.filter(fn(s) { s.operation_name == "send_welcome_email" })[0]
  
  // Verify database span properties
  assert_eq(db_span.service_name, "user-service")
  assert_eq(db_span.span_id, "span_db_bbbbbbb")
  
  // Verify notification span properties
  assert_eq(notif_span.service_name, "notification-service")
  assert_eq(notif_span.span_id, "span_notif_ccccc")
  
  // Verify timeline consistency
  assert_true(root_spans[0].start_time <= auth_span.start_time)
  assert_true(auth_span.end_time <= main_span.start_time)
  assert_true(main_span.start_time <= db_span.start_time)
  assert_true(db_span.end_time <= notif_span.start_time)
  assert_true(notif_span.end_time <= root_spans[0].end_time)
  
  // Calculate trace statistics
  let calculate_trace_stats = fn(spans: Array[{trace_id: String, span_id: String, parent_span_id: Option[String], service_name: String, operation_name: String, start_time: Int, end_time: Int, status: String, tags: Array[(String, String)}]) {
    let total_spans = spans.length()
    let services = spans.map(fn(s) { s.service_name })
    let unique_services = {
      let mut result = []
      for service in services {
        if not(result.contains(service)) {
          result = result.push(service)
        }
      }
      result
    }
    
    let durations = spans.map(fn(s) { s.end_time - s.start_time })
    let total_duration = durations.reduce(fn(acc, x) { acc + x }, 0)
    let trace_duration = {
      let start_times = spans.map(fn(s) { s.start_time })
      let end_times = spans.map(fn(s) { s.end_time })
      let earliest = start_times.reduce(fn(acc, x) { if x < acc { x } else { acc } }, start_times[0])
      let latest = end_times.reduce(fn(acc, x) { if x > acc { x } else { acc } }, end_times[0])
      latest - earliest
    }
    
    let error_spans = spans.filter(fn(s) { s.status != "ok" })
    let error_count = error_spans.length()
    
    {
      trace_id: spans[0].trace_id,
      total_spans,
      service_count: unique_services.length(),
      services: unique_services,
      total_span_duration: total_duration,
      trace_duration,
      error_count
    }
  }
  
  let trace_stats = calculate_trace_stats(all_spans)
  assert_eq(trace_stats.trace_id, "trace_reconstruction_123")
  assert_eq(trace_stats.total_spans, 5)
  assert_eq(trace_stats.service_count, 3)
  assert_true(trace_stats.services.contains("api-gateway"))
  assert_true(trace_stats.services.contains("user-service"))
  assert_true(trace_stats.services.contains("notification-service"))
  assert_eq(trace_stats.error_count, 0)
  assert_eq(trace_stats.trace_duration, 80)  // 1640995280 - 1640995200
}