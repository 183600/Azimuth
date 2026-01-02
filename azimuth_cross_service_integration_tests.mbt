// Azimuth Telemetry System - Cross-Service Integration Test Suite
// This file contains comprehensive test cases for cross-service integration

// Test 1: End-to-End Trace Propagation
test "end to end trace propagation" {
  // Simulate a multi-service request flow
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Service A: API Gateway
  let gateway_span = [
    ("trace.id", trace_id),
    ("span.id", "b7ad6b7169203331"),
    ("parent.span.id", ""),
    ("service.name", "api-gateway"),
    ("operation.name", "HTTP GET /api/orders"),
    ("span.kind", "server"),
    ("start.time", "1640995200000"),
    ("end.time", "1640995200100"),
    ("status.code", "200"),
    ("http.method", "GET"),
    ("http.url", "/api/orders"),
    ("http.status_code", "200")
  ]
  
  // Service B: Order Service
  let order_span = [
    ("trace.id", trace_id),
    ("span.id", "c8be7c827a314442"),
    ("parent.span.id", "b7ad6b7169203331"),
    ("service.name", "order-service"),
    ("operation.name", "processOrderRequest"),
    ("span.kind", "server"),
    ("start.time", "1640995200050"),
    ("end.time", "1640995200300"),
    ("status.code", "200"),
    ("db.system", "postgresql"),
    ("db.statement", "SELECT * FROM orders WHERE user_id = ?")
  ]
  
  // Service C: Inventory Service (called by Order Service)
  let inventory_span = [
    ("trace.id", trace_id),
    ("span.id", "d9cf8d938b425553"),
    ("parent.span.id", "c8be7c827a314442"),
    ("service.name", "inventory-service"),
    ("operation.name", "checkInventory"),
    ("span.kind", "client"),
    ("start.time", "1640995200100"),
    ("end.time", "1640995200200"),
    ("status.code", "200"),
    ("rpc.system", "grpc"),
    ("rpc.service", "InventoryService"),
    ("rpc.method", "CheckStock")
  ]
  
  // Service D: Payment Service
  let payment_span = [
    ("trace.id", trace_id),
    ("span.id", "e0d9ea449c536664"),
    ("parent.span.id", "c8be7c827a314442"),
    ("service.name", "payment-service"),
    ("operation.name", "processPayment"),
    ("span.kind", "server"),
    ("start.time", "1640995200200"),
    ("end.time", "1640995200280"),
    ("status.code", "200"),
    ("payment.method", "credit_card"),
    ("payment.amount", "99.99"),
    ("payment.currency", "USD")
  ]
  
  // Validate trace consistency
  let spans = [gateway_span, order_span, inventory_span, payment_span]
  
  for span in spans {
    let span_trace_id = span.filter(|(k, _)| k == "trace.id")[0][1]
    assert_eq(span_trace_id, trace_id)
  }
  
  // Validate parent-child relationships
  let gateway_span_id = gateway_span.filter(|(k, _)| k == "span.id")[0][1]
  let order_parent_id = order_span.filter(|(k, _)| k == "parent.span.id")[0][1]
  assert_eq(order_parent_id, gateway_span_id)
  
  let order_span_id = order_span.filter(|(k, _)| k == "span.id")[0][1]
  let inventory_parent_id = inventory_span.filter(|(k, _)| k == "parent.span.id")[0][1]
  let payment_parent_id = payment_span.filter(|(k, _)| k == "parent.span.id")[0][1]
  
  assert_eq(inventory_parent_id, order_span_id)
  assert_eq(payment_parent_id, order_span_id)
  
  // Validate chronological ordering
  let span_start_times = spans.map(|span| {
    span.filter(|(k, _)| k == "start.time")[0][1].to_int()
  })
  
  let sorted_start_times = span_start_times.sort(|a, b| a <= b)
  for i = 1; i < sorted_start_times.length(); i = i + 1 {
    assert_true(sorted_start_times[i] >= sorted_start_times[i-1])
  }
  
  // Validate service names are unique
  let service_names = spans.map(|span| {
    span.filter(|(k, _)| k == "service.name")[0][1]
  })
  
  let unique_service_names = []
  for name in service_names {
    let is_unique = !unique_service_names.any(|existing| existing == name)
    if is_unique {
      unique_service_names = unique_service_names.concat([name])
    }
  }
  
  assert_eq(unique_service_names.length(), 4)
}

// Test 2: Cross-Service Context Propagation
test "cross service context propagation" {
  // Simulate context propagation through multiple service calls
  let base_trace_id = "1234567890abcdef1234567890abcdef"
  
  // Initial context creation
  let initial_context = [
    ("traceparent", "00-" + base_trace_id + "-1111111111111111-01"),
    ("tracestate", "rojo=00f067aa0ba902b7"),
    ("baggage.user.id", "12345"),
    ("baggage.session.id", "abcdef123456"),
    ("baggage.request.id", "req-789012"),
    ("baggage.source", "web-ui")
  ]
  
  // Service A context extraction
  let service_a_context = []
  for (key, value) in initial_context {
    if key == "traceparent" {
      let parts = value.split("-")
      if parts.length() == 4 {
        service_a_context = service_a_context.concat([
          ("trace.id", parts[1]),
          ("parent.span.id", parts[2]),
          ("trace.flags", parts[3])
        ])
      }
    } else if key.starts_with("baggage.") {
      let baggage_key = key.substring(8)  // Remove "baggage." prefix
      service_a_context = service_a_context.concat([(baggage_key, value)])
    }
  }
  
  // Service A adds its own baggage
  let service_a_enhanced = service_a_context.concat([
    ("service.a.version", "1.2.3"),
    ("service.a.region", "us-west-2")
  ])
  
  // Service B receives context from Service A
  let service_b_context = service_a_enhanced.concat([
    ("service.b.version", "2.1.0"),
    ("service.b.instance", "instance-42")
  ])
  
  // Service C receives context from Service B
  let service_c_context = service_b_context.concat([
    ("service.c.version", "3.0.1"),
    ("service.c.zone", "availability-zone-1b")
  ])
  
  // Validate baggage propagation
  let original_baggage_items = ["user.id", "session.id", "request.id", "source"]
  
  for item in original_baggage_items {
    let service_a_has = service_a_enhanced.any(|(k, _)| k == item)
    let service_b_has = service_b_context.any(|(k, _)| k == item)
    let service_c_has = service_c_context.any(|(k, _)| k == item)
    
    assert_true(service_a_has)
    assert_true(service_b_has)
    assert_true(service_c_has)
  }
  
  // Validate service-specific baggage
  assert_true(service_a_enhanced.any(|(k, _)| k == "service.a.version"))
  assert_true(service_b_context.any(|(k, _)| k == "service.b.version"))
  assert_true(service_c_context.any(|(k, _)| k == "service.c.version"))
  
  // Validate trace ID consistency
  let service_a_trace_id = service_a_context.filter(|(k, _)| k == "trace.id")[0][1]
  let service_b_trace_id = service_b_context.filter(|(k, _)| k == "trace.id")[0][1]
  let service_c_trace_id = service_c_context.filter(|(k, _)| k == "trace.id")[0][1]
  
  assert_eq(service_a_trace_id, base_trace_id)
  assert_eq(service_b_trace_id, base_trace_id)
  assert_eq(service_c_trace_id, base_trace_id)
}

// Test 3: Distributed Metrics Aggregation
test "distributed metrics aggregation" {
  // Simulate metrics from multiple services
  let services = ["api-gateway", "user-service", "order-service", "inventory-service", "payment-service"]
  
  // Generate metrics for each service
  let mut service_metrics = []
  
  for i = 0; i < services.length(); i = i + 1 {
    let service_name = services[i]
    
    // HTTP request metrics
    let http_metrics = [
      ("http.requests.total", (100 + i * 20).to_string()),
      ("http.requests.duration", (50.0 + i * 10.0).to_string()),
      ("http.errors.total", (5 + i).to_string()),
      ("http.status.2xx", (90 + i * 18).to_string()),
      ("http.status.4xx", (8 + i).to_string()),
      ("http.status.5xx", (2 + i).to_string())
    ]
    
    // Resource metrics
    let resource_metrics = [
      ("cpu.usage", (0.3 + i * 0.1).to_string()),
      ("memory.usage", (0.4 + i * 0.05).to_string()),
      ("disk.usage", (0.2 + i * 0.02).to_string()),
      ("network.bytes.sent", (1000000 + i * 100000).to_string()),
      ("network.bytes.received", (2000000 + i * 200000).to_string())
    ]
    
    // Business metrics
    let business_metrics = [
      ("orders.processed", (50 + i * 10).to_string()),
      ("payments.completed", (45 + i * 9).to_string()),
      ("inventory.checked", (60 + i * 12).to_string()),
      ("users.active", (1000 + i * 200).to_string())
    ]
    
    let all_metrics = http_metrics.concat(resource_metrics).concat(business_metrics)
    
    // Add service name to each metric
    let service_specific_metrics = all_metrics.map(|(name, value)| {
      (service_name + "." + name, value)
    })
    
    service_metrics = service_metrics.concat([service_specific_metrics])
  }
  
  assert_eq(service_metrics.length(), 5)  // 5 services
  
  // Aggregate metrics across all services
  let mut aggregated_metrics = []
  
  // Find all unique metric names (without service prefix)
  let mut metric_names = []
  for metrics in service_metrics {
    for (name, _) in metrics {
      let parts = name.split(".")
      if parts.length() >= 2 {
        let base_name = []
        for i = 1; i < parts.length(); i = i + 1 {
          base_name = base_name.concat([parts[i]])
        }
        let full_base_name = base_name.reduce(|acc, part| acc + "." + part, "")
        let metric_name = full_base_name.substring(1)  // Remove leading "."
        
        let is_unique = !metric_names.any(|existing| existing == metric_name)
        if is_unique {
          metric_names = metric_names.concat([metric_name])
        }
      }
    }
  }
  
  // Aggregate each metric type
  for metric_name in metric_names {
    let mut total_value = 0.0
    let mut service_count = 0
    
    for metrics in service_metrics {
      for (name, value) in metrics {
        let parts = name.split(".")
        if parts.length() >= 2 {
          let base_name = []
          for i = 1; i < parts.length(); i = i + 1 {
            base_name = base_name.concat([parts[i]])
          }
          let full_base_name = base_name.reduce(|acc, part| acc + "." + part, "")
          let current_metric_name = full_base_name.substring(1)
          
          if current_metric_name == metric_name {
            total_value = total_value + value.to_float()
            service_count = service_count + 1
          }
        }
      }
    }
    
    let avg_value = if service_count > 0 { total_value / service_count.to_float() } else { 0.0 }
    
    aggregated_metrics = aggregated_metrics.concat([
      (metric_name + ".total", total_value.to_string()),
      (metric_name + ".average", avg_value.to_string()),
      (metric_name + ".service_count", service_count.to_string())
    ])
  }
  
  // Validate aggregation results
  let http_requests_total = aggregated_metrics.filter(|(name, _)| name == "http.requests.total.total")[0][1]
  assert_eq(http_requests_total, "700.0")  // Sum of 100+120+140+160+180
  
  let http_requests_avg = aggregated_metrics.filter(|(name, _)| name == "http.requests.total.average")[0][1]
  assert_eq(http_requests_avg, "140.0")  // Average of 700/5
  
  let orders_processed_total = aggregated_metrics.filter(|(name, _)| name == "orders.processed.total")[0][1]
  assert_eq(orders_processed_total, "300.0")  // Sum of 50+60+70+80+90
}

// Test 4: Cross-Service Log Correlation
test "cross service log correlation" {
  // Simulate correlated logs from multiple services
  let trace_id = "abcdef0123456789abcdef0123456789"
  let request_id = "req-123456789"
  
  // Service A logs
  let service_a_logs = [
    ("timestamp", "1640995200000"),
    ("level", "info"),
    ("service", "api-gateway"),
    ("message", "Incoming request received"),
    ("trace.id", trace_id),
    ("request.id", request_id),
    ("http.method", "GET"),
    ("http.path", "/api/users/123")
  ]
  
  let service_a_logs_2 = [
    ("timestamp", "1640995200050"),
    ("level", "debug"),
    ("service", "api-gateway"),
    ("message", "Authentication successful"),
    ("trace.id", trace_id),
    ("request.id", request_id),
    ("user.id", "123")
  ]
  
  // Service B logs
  let service_b_logs = [
    ("timestamp", "1640995200100"),
    ("level", "info"),
    ("service", "user-service"),
    ("message", "Fetching user profile"),
    ("trace.id", trace_id),
    ("request.id", request_id),
    ("user.id", "123")
  ]
  
  let service_b_logs_2 = [
    ("timestamp", "1640995200150"),
    ("level", "warn"),
    ("service", "user-service"),
    ("message", "User profile cache miss"),
    ("trace.id", trace_id),
    ("request.id", request_id),
    ("cache.key", "user:123")
  ]
  
  // Service C logs
  let service_c_logs = [
    ("timestamp", "1640995200200"),
    ("level", "info"),
    ("service", "database"),
    ("message", "Database query executed"),
    ("trace.id", trace_id),
    ("request.id", request_id),
    ("db.query", "SELECT * FROM users WHERE id = ?"),
    ("db.duration", "25")
  ]
  
  let service_c_logs_2 = [
    ("timestamp", "1640995200250"),
    ("level", "error"),
    ("service", "database"),
    ("message", "Connection timeout"),
    ("trace.id", trace_id),
    ("request.id", request_id),
    ("error.code", "CONN_TIMEOUT")
  ]
  
  // All logs
  let all_logs = [
    service_a_logs, service_a_logs_2,
    service_b_logs, service_b_logs_2,
    service_c_logs, service_c_logs_2
  ]
  
  // Validate trace correlation
  for log in all_logs {
    let log_trace_id = log.filter(|(k, _)| k == "trace.id")[0][1]
    assert_eq(log_trace_id, trace_id)
    
    let log_request_id = log.filter(|(k, _)| k == "request.id")[0][1]
    assert_eq(log_request_id, request_id)
  }
  
  // Validate chronological ordering
  let log_timestamps = all_logs.map(|log| {
    log.filter(|(k, _)| k == "timestamp")[0][1].to_int()
  })
  
  let sorted_timestamps = log_timestamps.sort(|a, b| a <= b)
  for i = 1; i < sorted_timestamps.length(); i = i + 1 {
    assert_true(sorted_timestamps[i] >= sorted_timestamps[i-1])
  }
  
  // Group logs by service
  let mut service_groups = []
  
  for log in all_logs {
    let service_name = log.filter(|(k, _)| k == "service")[0][1]
    
    let mut found_group = false
    for i = 0; i < service_groups.length(); i = i + 1 {
      if service_groups[i][0] == service_name {
        service_groups[i] = (service_groups[i][0], service_groups[i][1].concat([log]))
        found_group = true
        break
      }
    }
    
    if !found_group {
      service_groups = service_groups.concat([(service_name, [log])])
    }
  }
  
  // Should have 3 service groups
  assert_eq(service_groups.length(), 3)
  
  // Validate each service has logs
  for group in service_groups {
    let service_name = group[0]
    let logs = group[1]
    assert_true(logs.length() >= 1)
    
    // All logs in group should have same service
    for log in logs {
      let log_service = log.filter(|(k, _)| k == "service")[0][1]
      assert_eq(log_service, service_name)
    }
  }
  
  // Validate log levels distribution
  let mut level_counts = []
  
  for log in all_logs {
    let level = log.filter(|(k, _)| k == "level")[0][1]
    
    let mut found_count = false
    for i = 0; i < level_counts.length(); i = i + 1 {
      if level_counts[i][0] == level {
        level_counts[i] = (level_counts[i][0], level_counts[i][1] + 1)
        found_count = true
        break
      }
    }
    
    if !found_count {
      level_counts = level_counts.concat([(level, 1)])
    }
  }
  
  // Should have info, debug, warn, error levels
  assert_true(level_counts.any(|(level, _)| level == "info"))
  assert_true(level_counts.any(|(level, _)| level == "debug"))
  assert_true(level_counts.any(|(level, _)| level == "warn"))
  assert_true(level_counts.any(|(level, _)| level == "error"))
}

// Test 5: Service Dependency Mapping
test "service dependency mapping" {
  // Simulate service dependencies based on trace data
  let traces = [
    // Trace 1: API Gateway -> User Service -> Database
    [
      ("trace.id", "trace001"),
      ("span.id", "span001"),
      ("service.name", "api-gateway"),
      ("parent.span.id", ""),
      ("operation.name", "GET /api/users")
    ],
    [
      ("trace.id", "trace001"),
      ("span.id", "span002"),
      ("service.name", "user-service"),
      ("parent.span.id", "span001"),
      ("operation.name", "getUserProfile")
    ],
    [
      ("trace.id", "trace001"),
      ("span.id", "span003"),
      ("service.name", "database"),
      ("parent.span.id", "span002"),
      ("operation.name", "SELECT * FROM users")
    ],
    
    // Trace 2: API Gateway -> Order Service -> Inventory Service -> Database
    [
      ("trace.id", "trace002"),
      ("span.id", "span004"),
      ("service.name", "api-gateway"),
      ("parent.span.id", ""),
      ("operation.name", "POST /api/orders")
    ],
    [
      ("trace.id", "trace002"),
      ("span.id", "span005"),
      ("service.name", "order-service"),
      ("parent.span.id", "span004"),
      ("operation.name", "createOrder")
    ],
    [
      ("trace.id", "trace002"),
      ("span.id", "span006"),
      ("service.name", "inventory-service"),
      ("parent.span.id", "span005"),
      ("operation.name", "checkInventory")
    ],
    [
      ("trace.id", "trace002"),
      ("span.id", "span007"),
      ("service.name", "database"),
      ("parent.span.id", "span006"),
      ("operation.name", "SELECT * FROM inventory")
    ],
    
    // Trace 3: API Gateway -> Order Service -> Payment Service
    [
      ("trace.id", "trace003"),
      ("span.id", "span008"),
      ("service.name", "api-gateway"),
      ("parent.span.id", ""),
      ("operation.name", "POST /api/orders")
    ],
    [
      ("trace.id", "trace003"),
      ("span.id", "span009"),
      ("service.name", "order-service"),
      ("parent.span.id", "span008"),
      ("operation.name", "createOrder")
    ],
    [
      ("trace.id", "trace003"),
      ("span.id", "span010"),
      ("service.name", "payment-service"),
      ("parent.span.id", "span009"),
      ("operation.name", "processPayment")
    ]
  ]
  
  // Build service dependency map
  let mut dependencies = []
  
  for span in traces {
    let service_name = span.filter(|(k, _)| k == "service.name")[0][1]
    let parent_span_id = span.filter(|(k, _)| k == "parent.span.id")[0][1]
    let span_id = span.filter(|(k, _)| k == "span.id")[0][1]
    
    // Find parent service
    if parent_span_id != "" {
      let mut parent_service = ""
      
      for parent_span in traces {
        let parent_span_id_check = parent_span.filter(|(k, _)| k == "span.id")[0][1]
        if parent_span_id_check == parent_span_id {
          parent_service = parent_span.filter(|(k, _)| k == "service.name")[0][1]
          break
        }
      }
      
      if parent_service != "" {
        let dependency = (parent_service, service_name)
        
        // Check if dependency already exists
        let exists = dependencies.any(|(from, to)| from == parent_service && to == service_name)
        if !exists {
          dependencies = dependencies.concat([dependency])
        }
      }
    }
  }
  
  // Should have 5 unique dependencies
  assert_eq(dependencies.length(), 5)
  
  // Verify specific dependencies
  assert_true(dependencies.any(|(from, to)| from == "api-gateway" && to == "user-service"))
  assert_true(dependencies.any(|(from, to)| from == "user-service" && to == "database"))
  assert_true(dependencies.any(|(from, to)| from == "api-gateway" && to == "order-service"))
  assert_true(dependencies.any(|(from, to)| from == "order-service" && to == "inventory-service"))
  assert_true(dependencies.any(|(from, to)| from == "inventory-service" && to == "database"))
  assert_true(dependencies.any(|(from, to)| from == "order-service" && to == "payment-service"))
  
  // Build service topology
  let mut services = []
  
  for span in traces {
    let service_name = span.filter(|(k, _)| k == "service.name")[0][1]
    
    let is_unique = !services.any(|existing| existing == service_name)
    if is_unique {
      services = services.concat([service_name])
    }
  }
  
  // Should have 5 services
  assert_eq(services.length(), 5)
  
  // Build adjacency matrix for service topology
  let mut adjacency_matrix = []
  for i = 0; i < services.length(); i = i + 1 {
    let mut row = []
    for j = 0; j < services.length(); j = j + 1 {
      let from_service = services[i]
      let to_service = services[j]
      
      let has_dependency = dependencies.any(|(from, to)| from == from_service && to == to_service)
      row = row.concat([if has_dependency { 1 } else { 0 }])
    }
    adjacency_matrix = adjacency_matrix.concat([row])
  }
  
  // Validate adjacency matrix dimensions
  assert_eq(adjacency_matrix.length(), 5)
  for row in adjacency_matrix {
    assert_eq(row.length(), 5)
  }
  
  // Calculate service degrees (incoming and outgoing dependencies)
  let mut service_degrees = []
  
  for i = 0; i < services.length(); i = i + 1 {
    let service_name = services[i]
    
    let mut outgoing_count = 0
    let mut incoming_count = 0
    
    for j = 0; j < services.length(); j = j + 1 {
      if adjacency_matrix[i][j] == 1 {
        outgoing_count = outgoing_count + 1
      }
      if adjacency_matrix[j][i] == 1 {
        incoming_count = incoming_count + 1
      }
    }
    
    service_degrees = service_degrees.concat([(
      service_name,
      ("outgoing", outgoing_count),
      ("incoming", incoming_count)
    )])
  }
  
  // API Gateway should have only outgoing dependencies
  let api_gateway_degrees = service_degrees.filter(|(service, _)| service == "api-gateway")[0]
  assert_eq(api_gateway_degrees[1][1], 2)  // 2 outgoing
  assert_eq(api_gateway_degrees[2][1], 0)  // 0 incoming
  
  // Database should have only incoming dependencies
  let database_degrees = service_degrees.filter(|(service, _)| service == "database")[0]
  assert_eq(database_degrees[1][1], 0)  // 0 outgoing
  assert_eq(database_degrees[2][1], 2)  // 2 incoming
  
  // Order Service should have both incoming and outgoing
  let order_service_degrees = service_degrees.filter(|(service, _)| service == "order-service")[0]
  assert_eq(order_service_degrees[1][1], 2)  // 2 outgoing
  assert_eq(order_service_degrees[2][1], 1)  // 1 incoming
}

// Test 6: Cross-Service Error Propagation
test "cross service error propagation" {
  // Simulate error propagation through service chain
  let trace_id = "error-trace-001"
  
  // Service A: Initial error
  let service_a_error = [
    ("trace.id", trace_id),
    ("span.id", "error-span-001"),
    ("service.name", "api-gateway"),
    ("operation.name", "POST /api/orders"),
    ("status.code", "400"),
    ("status.message", "Bad Request"),
    ("error.type", "ValidationError"),
    ("error.message", "Invalid order data"),
    ("error.stack", "api-gateway:validateOrder"),
    ("timestamp", "1640995200000")
  ]
  
  // Service B: Error propagated from Service A
  let service_b_error = [
    ("trace.id", trace_id),
    ("span.id", "error-span-002"),
    ("parent.span.id", "error-span-001"),
    ("service.name", "order-service"),
    ("operation.name", "processOrder"),
    ("status.code", "400"),
    ("status.message", "Bad Request"),
    ("error.type", "ValidationError"),
    ("error.message", "Invalid order data"),
    ("error.stack", "order-service:processOrder -> api-gateway:validateOrder"),
    ("error.cause", "api-gateway:ValidationError"),
    ("timestamp", "1640995200050")
  ]
  
  // Service C: Error transformed
  let service_c_error = [
    ("trace.id", trace_id),
    ("span.id", "error-span-003"),
    ("parent.span.id", "error-span-002"),
    ("service.name", "notification-service"),
    ("operation.name", "sendOrderConfirmation"),
    ("status.code", "500"),
    ("status.message", "Internal Server Error"),
    ("error.type", "NotificationError"),
    ("error.message", "Failed to send order confirmation"),
    ("error.stack", "notification-service:sendConfirmation -> order-service:processOrder -> api-gateway:validateOrder"),
    ("error.cause", "order-service:ValidationError"),
    ("error.original.type", "ValidationError"),
    ("timestamp", "1640995200100")
  ]
  
  let error_chain = [service_a_error, service_b_error, service_c_error]
  
  // Validate error chain consistency
  for error in error_chain {
    let error_trace_id = error.filter(|(k, _)| k == "trace.id")[0][1]
    assert_eq(error_trace_id, trace_id)
    
    // All should have error information
    assert_true(error.any(|(k, _)| k == "error.type"))
    assert_true(error.any(|(k, _)| k == "error.message"))
    assert_true(error.any(|(k, _)| k == "error.stack"))
  }
  
  // Validate parent-child relationships
  let service_a_span_id = service_a_error.filter(|(k, _)| k == "span.id")[0][1]
  let service_b_parent_id = service_b_error.filter(|(k, _)| k == "parent.span.id")[0][1]
  assert_eq(service_b_parent_id, service_a_span_id)
  
  let service_b_span_id = service_b_error.filter(|(k, _)| k == "span.id")[0][1]
  let service_c_parent_id = service_c_error.filter(|(k, _)| k == "parent.span.id")[0][1]
  assert_eq(service_c_parent_id, service_b_span_id)
  
  // Validate error propagation
  let service_a_error_type = service_a_error.filter(|(k, _)| k == "error.type")[0][1]
  let service_b_error_cause = service_b_error.filter(|(k, _)| k == "error.cause")[0][1]
  assert_eq(service_b_error_cause, "api-gateway:" + service_a_error_type)
  
  let service_b_error_type = service_b_error.filter(|(k, _)| k == "error.type")[0][1]
  let service_c_error_cause = service_c_error.filter(|(k, _)| k == "error.cause")[0][1]
  assert_eq(service_c_error_cause, "order-service:" + service_b_error_type)
  
  // Validate error transformation
  let service_c_original_error_type = service_c_error.filter(|(k, _)| k == "error.original.type")[0][1]
  assert_eq(service_c_original_error_type, service_a_error_type)
  
  // Validate error stack trace
  let service_c_stack = service_c_error.filter(|(k, _)| k == "error.stack")[0][1]
  assert_true(service_c_stack.contains("notification-service"))
  assert_true(service_c_stack.contains("order-service"))
  assert_true(service_c_stack.contains("api-gateway"))
  
  // Validate chronological ordering
  let error_timestamps = error_chain.map(|error| {
    error.filter(|(k, _)| k == "timestamp")[0][1].to_int()
  })
  
  let sorted_timestamps = error_timestamps.sort(|a, b| a <= b)
  for i = 1; i < sorted_timestamps.length(); i = i + 1 {
    assert_true(sorted_timestamps[i] >= sorted_timestamps[i-1])
  }
  
  // Analyze error impact across services
  let mut error_analysis = []
  
  for error in error_chain {
    let service_name = error.filter(|(k, _)| k == "service.name")[0][1]
    let error_type = error.filter(|(k, _)| k == "error.type")[0][1]
    let status_code = error.filter(|(k, _)| k == "status.code")[0][1]
    
    error_analysis = error_analysis.concat([(
      service_name,
      ("error.type", error_type),
      ("status.code", status_code)
    )])
  }
  
  // Should have 3 services in error analysis
  assert_eq(error_analysis.length(), 3)
  
  // Validate error types
  let api_gateway_error = error_analysis.filter(|(service, _)| service == "api-gateway")[0]
  let order_service_error = error_analysis.filter(|(service, _)| service == "order-service")[0]
  let notification_service_error = error_analysis.filter(|(service, _)| service == "notification-service")[0]
  
  assert_eq(api_gateway_error[1][1], "ValidationError")
  assert_eq(api_gateway_error[2][1], "400")
  
  assert_eq(order_service_error[1][1], "ValidationError")
  assert_eq(order_service_error[2][1], "400")
  
  assert_eq(notification_service_error[1][1], "NotificationError")
  assert_eq(notification_service_error[2][1], "500")
}

// Test 7: Cross-Service Performance Analysis
test "cross service performance analysis" {
  // Simulate performance data from multiple services
  let trace_id = "perf-trace-001"
  
  // Service A: API Gateway
  let service_a_perf = [
    ("trace.id", trace_id),
    ("span.id", "perf-span-001"),
    ("service.name", "api-gateway"),
    ("operation.name", "GET /api/products"),
    ("start.time", "1640995200000"),
    ("end.time", "1640995200100"),
    ("duration", "100"),
    ("cpu.time", "15"),
    ("memory.allocated", "1024"),
    ("network.bytes", "2048")
  ]
  
  // Service B: Product Service
  let service_b_perf = [
    ("trace.id", trace_id),
    ("span.id", "perf-span-002"),
    ("parent.span.id", "perf-span-001"),
    ("service.name", "product-service"),
    ("operation.name", "getProducts"),
    ("start.time", "1640995200020"),
    ("end.time", "1640995200080"),
    ("duration", "60"),
    ("cpu.time", "25"),
    ("memory.allocated", "2048"),
    ("database.queries", "3"),
    ("database.time", "30")
  ]
  
  // Service C: Cache Service
  let service_c_perf = [
    ("trace.id", trace_id),
    ("span.id", "perf-span-003"),
    ("parent.span.id", "perf-span-002"),
    ("service.name", "cache-service"),
    ("operation.name", "getProductsCache"),
    ("start.time", "1640995200030"),
    ("end.time", "1640995200045"),
    ("duration", "15"),
    ("cpu.time", "5"),
    ("memory.allocated", "512"),
    ("cache.hit", "true")
  ]
  
  // Service D: Recommendation Service
  let service_d_perf = [
    ("trace.id", trace_id),
    ("span.id", "perf-span-004"),
    ("parent.span.id", "perf-span-001"),
    ("service.name", "recommendation-service"),
    ("operation.name", "getRecommendations"),
    ("start.time", "1640995200050"),
    ("end.time", "1640995200090"),
    ("duration", "40"),
    ("cpu.time", "20"),
    ("memory.allocated", "1536"),
    ("ml.model.inference.time", "25")
  ]
  
  let performance_data = [service_a_perf, service_b_perf, service_c_perf, service_d_perf]
  
  // Calculate overall trace performance
  let trace_start = performance_data.map(|span| span.filter(|(k, _)| k == "start.time")[0][1].to_int()).sort(|a, b| a <= b)[0]
  let trace_end = performance_data.map(|span| span.filter(|(k, _)| k == "end.time")[0][1].to_int()).sort(|a, b| a >= b)[0]
  let trace_duration = trace_end - trace_start
  
  assert_eq(trace_duration, 100)  // Should match the longest span (API Gateway)
  
  // Calculate service-specific performance metrics
  let mut service_performance = []
  
  for span in performance_data {
    let service_name = span.filter(|(k, _)| k == "service.name")[0][1]
    let duration = span.filter(|(k, _)| k == "duration")[0][1].to_int()
    let cpu_time = span.filter(|(k, _)| k == "cpu.time")[0][1].to_int()
    let memory_allocated = span.filter(|(k, _)| k == "memory.allocated")[0][1].to_int()
    
    service_performance = service_performance.concat([(
      service_name,
      ("duration", duration),
      ("cpu.time", cpu_time),
      ("memory.allocated", memory_allocated)
    )])
  }
  
  // Should have 4 services
  assert_eq(service_performance.length(), 4)
  
  // Calculate performance statistics
  let durations = service_performance.map(|(_, perf)| perf.filter(|(k, _)| k == "duration")[0][1])
  let cpu_times = service_performance.map(|(_, perf)| perf.filter(|(k, _)| k == "cpu.time")[0][1])
  let memory_allocations = service_performance.map(|(_, perf)| perf.filter(|(k, _)| k == "memory.allocated")[0][1])
  
  let total_duration = durations.reduce(|acc, val| acc + val, 0)
  let total_cpu_time = cpu_times.reduce(|acc, val| acc + val, 0)
  let total_memory = memory_allocations.reduce(|acc, val| acc + val, 0)
  
  let avg_duration = total_duration / durations.length()
  let avg_cpu_time = total_cpu_time / cpu_times.length()
  let avg_memory = total_memory / memory_allocations.length()
  
  assert_eq(avg_duration, 53)  // (100 + 60 + 15 + 40) / 4
  assert_eq(avg_cpu_time, 16)  // (15 + 25 + 5 + 20) / 4
  assert_eq(avg_memory, 1280)  // (1024 + 2048 + 512 + 1536) / 4
  
  // Identify performance bottlenecks
  let slowest_service = service_performance.reduce(|slowest, current| {
    let slowest_duration = slowest[1].filter(|(k, _)| k == "duration")[0][1]
    let current_duration = current[1].filter(|(k, _)| k == "duration")[0][1]
    
    if current_duration > slowest_duration {
      current
    } else {
      slowest
    }
  })
  
  assert_eq(slowest_service[0], "api-gateway")
  
  // Calculate CPU usage percentage
  let cpu_usage_percentages = []
  for perf in service_performance {
    let duration = perf[1].filter(|(k, _)| k == "duration")[0][1]
    let cpu_time = perf[1].filter(|(k, _)| k == "cpu.time")[0][1]
    let cpu_percentage = (cpu_time.to_float() / duration.to_float()) * 100.0
    
    cpu_usage_percentages = cpu_usage_percentages.concat([(
      perf[0],
      cpu_percentage
    )])
  }
  
  // Validate CPU usage percentages
  for (service, percentage) in cpu_usage_percentages {
    assert_true(percentage >= 0.0 && percentage <= 100.0)
  }
  
  // API Gateway should have lowest CPU percentage (15/100 = 15%)
  let api_gateway_cpu = cpu_usage_percentages.filter(|(service, _)| service == "api-gateway")[0][1]
  assert_eq(api_gateway_cpu, 15.0)
  
  // Product Service should have highest CPU percentage (25/60 ≈ 41.7%)
  let product_service_cpu = cpu_usage_percentages.filter(|(service, _)| service == "product-service")[0][1]
  assert_true(product_service_cpu > 40.0)
  
  // Analyze concurrent operations
  let mut concurrent_operations = []
  
  for i = 0; i < performance_data.length(); i = i + 1 {
    let span_i = performance_data[i]
    let start_i = span_i.filter(|(k, _)| k == "start.time")[0][1].to_int()
    let end_i = span_i.filter(|(k, _)| k == "end.time")[0][1].to_int()
    
    for j = i + 1; j < performance_data.length(); j = j + 1 {
      let span_j = performance_data[j]
      let start_j = span_j.filter(|(k, _)| k == "start.time")[0][1].to_int()
      let end_j = span_j.filter(|(k, _)| k == "end.time")[0][1].to_int()
      
      // Check if spans overlap
      let overlap = (start_i <= end_j) && (start_j <= end_i)
      if overlap {
        concurrent_operations = concurrent_operations.concat([(
          span_i.filter(|(k, _)| k == "service.name")[0][1],
          span_j.filter(|(k, _)| k == "service.name")[0][1]
        )])
      }
    }
  }
  
  // Should have concurrent operations
  assert_true(concurrent_operations.length() > 0)
  
  // Product Service and Cache Service should be concurrent
  assert_true(concurrent_operations.any(|(service_a, service_b)| 
    (service_a == "product-service" && service_b == "cache-service") ||
    (service_a == "cache-service" && service_b == "product-service")
  ))
  
  // Recommendation Service and Product Service should be concurrent
  assert_true(concurrent_operations.any(|(service_a, service_b)| 
    (service_a == "recommendation-service" && service_b == "product-service") ||
    (service_a == "product-service" && service_b == "recommendation-service")
  ))
}

// Test 8: Cross-Service Configuration Synchronization
test "cross service configuration synchronization" {
  // Simulate configuration from different services
  let base_config = [
    ("telemetry.enabled", "true"),
    ("telemetry.sampling.rate", "0.1"),
    ("telemetry.batch.size", "512"),
    ("telemetry.exporter.endpoint", "http://otel-collector:4318"),
    ("telemetry.resource.attributes", "service.name,service.version,deployment.environment")
  ]
  
  // Service A specific configuration
  let service_a_config = [
    ("service.name", "api-gateway"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "production"),
    ("service.port", "8080"),
    ("service.timeout", "30000"),
    ("telemetry.sampling.rate", "0.05"),  // Override
    ("telemetry.service.attributes", "service.name,service.version,deployment.environment,service.port")
  ]
  
  // Service B specific configuration
  let service_b_config = [
    ("service.name", "user-service"),
    ("service.version", "2.1.0"),
    ("deployment.environment", "production"),
    ("service.port", "8081"),
    ("service.timeout", "15000"),
    ("database.connection.pool", "10"),
    ("telemetry.service.attributes", "service.name,service.version,deployment.environment,service.port")
  ]
  
  // Service C specific configuration
  let service_c_config = [
    ("service.name", "order-service"),
    ("service.version", "3.0.1"),
    ("deployment.environment", "production"),
    ("service.port", "8082"),
    ("service.timeout", "45000"),
    ("database.connection.pool", "20"),
    ("cache.enabled", "true"),
    ("telemetry.sampling.rate", "0.2"),  // Override
    ("telemetry.service.attributes", "service.name,service.version,deployment.environment,service.port,cache.enabled")
  ]
  
  // Merge configurations for each service
  let service_configs = [
    ("api-gateway", base_config.concat(service_a_config)),
    ("user-service", base_config.concat(service_b_config)),
    ("order-service", base_config.concat(service_c_config))
  ]
  
  // Validate configuration merging
  for (service_name, config) in service_configs {
    // Base telemetry config should be present
    assert_true(config.any(|(k, _)| k == "telemetry.enabled"))
    assert_true(config.any(|(k, _)| k == "telemetry.batch.size"))
    assert_true(config.any(|(k, _)| k == "telemetry.exporter.endpoint"))
    
    // Service-specific config should be present
    assert_true(config.any(|(k, v)| k == "service.name" && v == service_name))
    assert_true(config.any(|(k, _)| k == "service.port"))
    assert_true(config.any(|(k, _)| k == "service.timeout"))
  }
  
  // Validate configuration overrides
  let api_gateway_config = service_configs.filter(|(name, _)| name == "api-gateway")[0][1]
  let api_gateway_sampling = api_gateway_config.filter(|(k, v)| k == "telemetry.sampling.rate")[0][1]
  assert_eq(api_gateway_sampling, "0.05")  // Should be overridden
  
  let order_service_config = service_configs.filter(|(name, _)| name == "order-service")[0][1]
  let order_service_sampling = order_service_config.filter(|(k, v)| k == "telemetry.sampling.rate")[0][1]
  assert_eq(order_service_sampling, "0.2")  // Should be overridden
  
  let user_service_config = service_configs.filter(|(name, _)| name == "user-service")[0][1]
  let user_service_sampling = user_service_config.filter(|(k, v)| k == "telemetry.sampling.rate")[0][1]
  assert_eq(user_service_sampling, "0.1")  // Should use base config
  
  // Validate configuration consistency
  let common_config_keys = [
    "telemetry.enabled",
    "telemetry.batch.size",
    "telemetry.exporter.endpoint",
    "deployment.environment"
  ]
  
  for key in common_config_keys {
    let mut values = []
    
    for (_, config) in service_configs {
      for (config_key, config_value) in config {
        if config_key == key {
          values = values.concat([config_value])
          break
        }
      }
    }
    
    // All services should have same value for common config
    for i = 1; i < values.length(); i = i + 1 {
      assert_eq(values[0], values[i])
    }
  }
  
  // Simulate configuration update propagation
  let config_update = [
    ("telemetry.batch.size", "1024"),  // Update
    ("telemetry.sampling.rate", "0.15"),  // Update
    ("new.global.setting", "enabled")  // New
  ]
  
  // Apply update to all services
  let mut updated_service_configs = []
  
  for (service_name, config) in service_configs {
    let mut updated_config = config
    
    // Remove old values for updated keys
    updated_config = updated_config.filter(|(key, _)| {
      !config_update.any(|(update_key, _)| update_key == key)
    })
    
    // Add new values
    updated_config = updated_config.concat(config_update)
    
    updated_service_configs = updated_service_configs.concat([(service_name, updated_config)])
  }
  
  // Validate update propagation
  for (_, config) in updated_service_configs {
    let batch_size = config.filter(|(k, v)| k == "telemetry.batch.size")[0][1]
    assert_eq(batch_size, "1024")
    
    let sampling_rate = config.filter(|(k, v)| k == "telemetry.sampling.rate")[0][1]
    assert_eq(sampling_rate, "0.15")
    
    let new_setting = config.filter(|(k, v)| k == "new.global.setting")[0][1]
    assert_eq(new_setting, "enabled")
  }
  
  // Validate configuration validation
  let validation_rules = [
    ("telemetry.batch.size", "1-10000"),
    ("telemetry.sampling.rate", "0.0-1.0"),
    ("service.timeout", "1000-300000"),
    ("service.port", "1-65535")
  ]
  
  for (service_name, config) in updated_service_configs {
    for (key, value) in config {
      for (rule_key, rule_range) in validation_rules {
        if key == rule_key {
          let range_parts = rule_range.split("-")
          if range_parts.length() == 2 {
            let min_val = range_parts[0].to_float()
            let max_val = range_parts[1].to_float()
            let config_val = value.to_float()
            
            assert_true(config_val >= min_val && config_val <= max_val)
          }
        }
      }
    }
  }
}