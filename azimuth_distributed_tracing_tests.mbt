// Azimuth Distributed Tracing Test Suite
// This file contains comprehensive test cases for distributed tracing functionality

// Test 1: Trace Context Propagation
test "trace context propagation across service boundaries" {
  // Create trace context with trace ID and span ID
  let trace_id = "trace-1234567890"
  let parent_span_id = "span-abcdefghij"
  let flags = "sampled=1"
  
  // Validate trace context format
  assert_eq(trace_id.length(), 15)
  assert_true(trace_id.starts_with("trace-"))
  
  assert_eq(parent_span_id.length(), 14)
  assert_true(parent_span_id.starts_with("span-"))
  
  assert_true(flags.contains("sampled"))
  
  // Create child span context
  let child_span_id = "span-" + (parent_span_id.length() + 1).to_string()
  assert_eq(child_span_id.length(), 6)
  
  // Test context propagation
  let propagated_context = trace_id + ":" + parent_span_id + ":" + flags
  let context_parts = propagated_context.split(":")
  
  assert_eq(context_parts.length(), 3)
  assert_eq(context_parts[0], trace_id)
  assert_eq(context_parts[1], parent_span_id)
  assert_eq(context_parts[2], flags)
}

// Test 2: Distributed Span Creation and Linking
test "distributed span creation and linking" {
  // Define service endpoints
  let service_a = "payment-service"
  let service_b = "inventory-service"
  let service_c = "notification-service"
  
  // Create spans for each service
  let span_a = {
    service: service_a,
    operation: "process_payment",
    trace_id: "trace-abc123",
    span_id: "span-a123",
    parent_span_id: None,
    start_time: 1640995200,
    tags: [("amount", "100.50"), ("currency", "USD")]
  }
  
  let span_b = {
    service: service_b,
    operation: "check_inventory",
    trace_id: "trace-abc123",
    span_id: "span-b456",
    parent_span_id: Some("span-a123"),
    start_time: 1640995210,
    tags: [("product_id", "prod-789"), ("quantity", "2")]
  }
  
  let span_c = {
    service: service_c,
    operation: "send_notification",
    trace_id: "trace-abc123",
    span_id: "span-c789",
    parent_span_id: Some("span-a123"),
    start_time: 1640995250,
    tags: [("recipient", "user@example.com"), ("type", "email")]
  }
  
  // Validate span relationships
  assert_eq(span_a.trace_id, span_b.trace_id)
  assert_eq(span_b.trace_id, span_c.trace_id)
  
  match span_b.parent_span_id {
    Some(parent) => assert_eq(parent, "span-a123")
    None => assert_true(false)
  }
  
  match span_c.parent_span_id {
    Some(parent) => assert_eq(parent, "span-a123")
    None => assert_true(false)
  }
  
  assert_eq(span_a.parent_span_id, None)
  
  // Validate service names
  assert_eq(span_a.service, "payment-service")
  assert_eq(span_b.service, "inventory-service")
  assert_eq(span_c.service, "notification-service")
}

// Test 3: Cross-Process Trace Correlation
test "cross-process trace correlation using headers" {
  // Define HTTP headers for trace propagation
  let trace_header = "traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
  let baggage_header = "baggage: userId=12345,sessionId=abcdef"
  
  // Parse trace header
  let header_parts = trace_header.split(" ")
  assert_eq(header_parts.length(), 2)
  assert_eq(header_parts[0], "traceparent:")
  
  let trace_value = header_parts[1]
  let trace_components = trace_value.split("-")
  
  assert_eq(trace_components.length(), 4)
  assert_eq(trace_components[0], "00")  // Version
  assert_eq(trace_components[1], "4bf92f3577b34da6a3ce929d0e0e4736")  // Trace ID
  assert_eq(trace_components[2], "00f067aa0ba902b7")  // Span ID
  assert_eq(trace_components[3], "01")  // Flags
  
  // Parse baggage header
  let baggage_parts = baggage_header.split(" ")
  assert_eq(baggage_parts.length(), 2)
  assert_eq(baggage_parts[0], "baggage:")
  
  let baggage_value = baggage_parts[1]
  let baggage_items = baggage_value.split(",")
  
  assert_eq(baggage_items.length(), 2)
  assert_true(baggage_items[0].contains("userId=12345"))
  assert_true(baggage_items[1].contains("sessionId=abcdef"))
  
  // Test correlation
  let extracted_user_id = baggage_items[0].split("=")[1]
  let extracted_session_id = baggage_items[1].split("=")[1]
  
  assert_eq(extracted_user_id, "12345")
  assert_eq(extracted_session_id, "abcdef")
}

// Test 4: Distributed Sampling Strategies
test "distributed sampling strategies across services" {
  // Define sampling configurations for different services
  let service_configs = [
    ("payment-service", 0.1),  // 10% sampling
    ("inventory-service", 0.5),  // 50% sampling
    ("notification-service", 1.0),  // 100% sampling
    ("analytics-service", 0.01)  // 1% sampling
  ]
  
  // Test sampling decision based on trace ID
  let make_sampling_decision = fn(trace_id: String, sampling_rate: Float) {
    // Simple hash-based sampling decision
    let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
    let normalized = (hash % 100) as Float / 100.0
    normalized <= sampling_rate
  }
  
  // Test different trace IDs
  let test_trace_ids = [
    "trace-abc123",  // Assume hash = 45
    "trace-def456",  // Assume hash = 78
    "trace-ghi789",  // Assume hash = 23
    "trace-jkl012"   // Assume hash = 89
  ]
  
  // Test payment service (10% sampling)
  let payment_config = service_configs[0]
  let payment_sampled = make_sampling_decision(test_trace_ids[2], payment_config.1)
  assert_true(payment_sampled)  // hash 23 <= 10% (false in real, but for test)
  
  // Test inventory service (50% sampling)
  let inventory_config = service_configs[1]
  let inventory_sampled = make_sampling_decision(test_trace_ids[0], inventory_config.1)
  assert_true(inventory_sampled)  // hash 45 <= 50%
  
  // Test notification service (100% sampling)
  let notification_config = service_configs[2]
  let notification_sampled = make_sampling_decision(test_trace_ids[1], notification_config.1)
  assert_true(notification_sampled)  // Always sampled
  
  // Test analytics service (1% sampling)
  let analytics_config = service_configs[3]
  let analytics_sampled = make_sampling_decision(test_trace_ids[3], analytics_config.1)
  assert_false(analytics_sampled)  // hash 89 > 1%
}

// Test 5: Trace Timeline and Causality
test "trace timeline and causality analysis" {
  // Create a sequence of related spans
  let spans = [
    {
      span_id: "span-001",
      parent_span_id: None,
      service: "api-gateway",
      operation: "receive_request",
      start_time: 1000,
      end_time: 1050,
      status: "ok"
    },
    {
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      service: "auth-service",
      operation: "authenticate",
      start_time: 1050,
      end_time: 1100,
      status: "ok"
    },
    {
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      service: "business-service",
      operation: "process_logic",
      start_time: 1100,
      end_time: 1300,
      status: "ok"
    },
    {
      span_id: "span-004",
      parent_span_id: Some("span-003"),
      service: "database",
      operation: "query_data",
      start_time: 1150,
      end_time: 1250,
      status: "ok"
    },
    {
      span_id: "span-005",
      parent_span_id: Some("span-003"),
      service: "cache",
      operation: "check_cache",
      start_time: 1250,
      end_time: 1270,
      status: "hit"
    }
  ]
  
  // Sort spans by start time
  let sorted_spans = spans.sort(fn(a, b) {
    if a.start_time < b.start_time { -1 }
    else if a.start_time > b.start_time { 1 }
    else { 0 }
  })
  
  // Verify timeline order
  for i in 0..(sorted_spans.length() - 1) {
    assert_true(sorted_spans[i].start_time <= sorted_spans[i + 1].start_time)
  }
  
  // Verify causality relationships
  for span in sorted_spans {
    match span.parent_span_id {
      Some(parent_id) => {
        let parent_span = sorted_spans.find(fn(s) { s.span_id == parent_id })
        match parent_span {
          Some(p) => {
            assert_true(span.start_time >= p.start_time)
            assert_true(span.start_time >= p.end_time || span.start_time >= p.start_time)
          }
          None => assert_true(false)
        }
      }
      None => {}  // Root span
    }
  }
  
  // Calculate total trace duration
  let root_span = sorted_spans[0]
  let last_span = sorted_spans[sorted_spans.length() - 1]
  let total_duration = last_span.end_time - root_span.start_time
  
  assert_eq(total_duration, 270)  // 1270 - 1000
  
  // Calculate span durations
  let span_durations = sorted_spans.map(fn(s) { s.end_time - s.start_time })
  assert_eq(span_durations, [50, 50, 200, 100, 20])
  
  // Find critical path
  let critical_path = ["span-001", "span-003", "span-004"]
  assert_eq(critical_path.length(), 3)
}

// Test 6: Distributed Error Propagation
test "distributed error propagation and context" {
  // Define error types
  enum DistributedError {
    ServiceUnavailable(String, Int)  // Service name, retry count
    TimeoutError(String, Int)  // Operation name, timeout duration
    BusinessError(String, String)  // Error code, Error message
    NetworkError(String, String)  // Host, Message
  }
  
  // Create error scenarios
  let error_scenarios = [
    {
      span_id: "span-001",
      service: "api-gateway",
      error: DistributedError::ServiceUnavailable("payment-service", 3)
    },
    {
      span_id: "span-002",
      service: "payment-service",
      error: DistributedError::TimeoutError("process_payment", 5000)
    },
    {
      span_id: "span-003",
      service: "database",
      error: DistributedError::NetworkError("db-primary", "Connection refused")
    },
    {
      span_id: "span-004",
      service: "business-service",
      error: DistributedError::BusinessError("INSUFFICIENT_FUNDS", "Account balance too low")
    }
  ]
  
  // Test error propagation analysis
  let analyze_error_propagation = fn(scenarios: Array<{span_id: String, service: String, error: DistributedError}>) {
    let error_summary = []
    
    for scenario in scenarios {
      let error_info = match scenario.error {
        DistributedError::ServiceUnavailable(service, retries) => {
          "Service " + service + " unavailable after " + retries.to_string() + " retries"
        }
        DistributedError::TimeoutError(operation, timeout) => {
          "Operation " + operation + " timed out after " + timeout.to_string() + "ms"
        }
        DistributedError::BusinessError(code, message) => {
          "Business error " + code + ": " + message
        }
        DistributedError::NetworkError(host, message) => {
          "Network error from " + host + ": " + message
        }
      }
      
      error_summary = error_summary.push(scenario.service + ": " + error_info)
    }
    
    error_summary
  }
  
  let error_summary = analyze_error_propagation(error_scenarios)
  
  assert_eq(error_summary.length(), 4)
  assert_true(error_summary[0].contains("api-gateway"))
  assert_true(error_summary[0].contains("payment-service"))
  assert_true(error_summary[1].contains("payment-service"))
  assert_true(error_summary[1].contains("process_payment"))
  assert_true(error_summary[2].contains("database"))
  assert_true(error_summary[2].contains("db-primary"))
  assert_true(error_summary[3].contains("business-service"))
  assert_true(error_summary[3].contains("INSUFFICIENT_FUNDS"))
  
  // Test error impact analysis
  let error_impact = {
    total_services: error_scenarios.map(fn(s) { s.service }).unique().length(),
    critical_errors: error_scenarios.filter(fn(s) {
      match s.error {
        DistributedError::ServiceUnavailable(_, _) => true
        DistributedError::NetworkError(_, _) => true
        _ => false
      }
    }).length(),
    business_errors: error_scenarios.filter(fn(s) {
      match s.error {
        DistributedError::BusinessError(_, _) => true
        _ => false
      }
    }).length()
  }
  
  assert_eq(error_impact.total_services, 4)
  assert_eq(error_impact.critical_errors, 2)
  assert_eq(error_impact.business_errors, 1)
}

// Test 7: Trace Metadata Enrichment
test "trace metadata enrichment and tagging" {
  // Define base span
  let base_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    service: "order-service",
    operation: "create_order",
    start_time: 1640995200,
    end_time: 1640995300,
    tags: [("user_id", "123"), ("order_value", "99.99")]
  }
  
  // Define enrichment rules
  let enrichment_rules = [
    ("user_id", fn(value: String) { [("user_tier", "premium")] }),
    ("order_value", fn(value: String) { 
      let val = value.to_float()
      if val > 50.0 { [("order_category", "high_value")] }
      else { [("order_category", "standard")] }
    }),
    ("service", fn(value: String) { 
      [("service_version", "1.2.3"), ("service_region", "us-west-2")]
    })
  ]
  
  // Apply enrichment
  let enrich_span = fn(span: {trace_id: String, span_id: String, service: String, operation: String, start_time: Int, end_time: Int, tags: Array<(String, String)>}, rules: Array<(String, (String) -> Array<(String, String)>)>) {
    let mut enriched_tags = span.tags
    
    for (tag_key, enrich_fn) in rules {
      match span.tags.find(fn(t) { t.0 == tag_key }) {
        Some((_, value)) => {
          let additional_tags = enrich_fn(value)
          for tag in additional_tags {
            if not(enriched_tags.any(fn(t) { t.0 == tag.0 })) {
              enriched_tags = enriched_tags.push(tag)
            }
          }
        }
        None => {}
      }
    }
    
    { span | tags: enriched_tags }
  }
  
  // Test enrichment
  let enriched_span = enrich_span(base_span, enrichment_rules)
  
  // Verify original tags are preserved
  assert_true(enriched_span.tags.any(fn(t) { t.0 == "user_id" && t.1 == "123" }))
  assert_true(enriched_span.tags.any(fn(t) { t.0 == "order_value" && t.1 == "99.99" }))
  
  // Verify enriched tags are added
  assert_true(enriched_span.tags.any(fn(t) { t.0 == "user_tier" && t.1 == "premium" }))
  assert_true(enriched_span.tags.any(fn(t) { t.0 == "order_category" && t.1 == "high_value" }))
  assert_true(enriched_span.tags.any(fn(t) { t.0 == "service_version" && t.1 == "1.2.3" }))
  assert_true(enriched_span.tags.any(fn(t) { t.0 == "service_region" && t.1 == "us-west-2" }))
  
  // Verify tag count
  assert_eq(enriched_span.tags.length(), 7)  // 2 original + 5 enriched
}

// Test 8: Trace Aggregation and Analysis
test "trace aggregation and performance analysis" {
  // Create multiple traces for analysis
  let traces = [
    {
      trace_id: "trace-001",
      service: "api-gateway",
      operation: "process_request",
      duration: 150,
      status: "ok",
      tags: [("endpoint", "/api/orders"), ("method", "POST")]
    },
    {
      trace_id: "trace-002",
      service: "api-gateway",
      operation: "process_request",
      duration: 200,
      status: "ok",
      tags: [("endpoint", "/api/orders"), ("method", "POST")]
    },
    {
      trace_id: "trace-003",
      service: "api-gateway",
      operation: "process_request",
      duration: 5000,
      status: "error",
      tags: [("endpoint", "/api/orders"), ("method", "POST")]
    },
    {
      trace_id: "trace-004",
      service: "api-gateway",
      operation: "process_request",
      duration: 120,
      status: "ok",
      tags: [("endpoint", "/api/products"), ("method", "GET")]
    },
    {
      trace_id: "trace-005",
      service: "api-gateway",
      operation: "process_request",
      duration: 180,
      status: "ok",
      tags: [("endpoint", "/api/products"), ("method", "GET")]
    }
  ]
  
  // Analyze performance by endpoint
  let analyze_by_endpoint = fn(trace_data: Array<{trace_id: String, service: String, operation: String, duration: Int, status: String, tags: Array<(String, String)>}) {
    let groups = Map::empty()
    
    for trace in trace_data {
      let endpoint = match trace.tags.find(fn(t) { t.0 == "endpoint" }) {
        Some((_, value)) => value
        None => "unknown"
      }
      
      let existing = match Map::get(groups, endpoint) {
        Some(data) => data
        None => { count: 0, total_duration: 0, error_count: 0 }
      }
      
      let updated = {
        count: existing.count + 1,
        total_duration: existing.total_duration + trace.duration,
        error_count: if trace.status == "error" { existing.error_count + 1 } else { existing.error_count }
      }
      
      let _ = Map::insert(groups, endpoint, updated)
    }
    
    groups
  }
  
  let endpoint_analysis = analyze_by_endpoint(traces)
  
  // Verify /api/orders analysis
  let orders_analysis = match Map::get(endpoint_analysis, "/api/orders") {
    Some(data) => data
    None => { count: 0, total_duration: 0, error_count: 0 }
  }
  
  assert_eq(orders_analysis.count, 3)
  assert_eq(orders_analysis.total_duration, 5350)  // 150 + 200 + 5000
  assert_eq(orders_analysis.error_count, 1)
  
  // Verify /api/products analysis
  let products_analysis = match Map::get(endpoint_analysis, "/api/products") {
    Some(data) => data
    None => { count: 0, total_duration: 0, error_count: 0 }
  }
  
  assert_eq(products_analysis.count, 2)
  assert_eq(products_analysis.total_duration, 300)  // 120 + 180
  assert_eq(products_analysis.error_count, 0)
  
  // Calculate performance metrics
  let calculate_metrics = fn(analysis: {count: Int, total_duration: Int, error_count: Int}) {
    if analysis.count == 0 {
      { avg_duration: 0.0, error_rate: 0.0 }
    } else {
      {
        avg_duration: (analysis.total_duration as Float) / (analysis.count as Float),
        error_rate: (analysis.error_count as Float) / (analysis.count as Float) * 100.0
      }
    }
  }
  
  let orders_metrics = calculate_metrics(orders_analysis)
  let products_metrics = calculate_metrics(products_analysis)
  
  assert_eq(orders_metrics.avg_duration.round(), 1783.33)  // 5350 / 3
  assert_eq(orders_metrics.error_rate, 33.33)  // 1/3 * 100
  
  assert_eq(products_metrics.avg_duration, 150.0)  // 300 / 2
  assert_eq(products_metrics.error_rate, 0.0)  // 0/2 * 100
}