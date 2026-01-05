// Azimuth Telemetry System - Cross-Service Communication Enhanced Tests
// This file contains comprehensive test cases for cross-service communication functionality

// Test 1: Service Discovery and Registration
test "service discovery and registration" {
  let service_registry = ServiceRegistry::new()
  
  // Test service registration
  let service_a = ServiceInfo::new("service-a", "1.0.0", "http://localhost:8001")
  let service_b = ServiceInfo::new("service-b", "1.2.0", "http://localhost:8002")
  let service_c = ServiceInfo::new("service-c", "2.0.0", "http://localhost:8003")
  
  let reg_result_a = ServiceRegistry::register(service_registry, service_a)
  let reg_result_b = ServiceRegistry::register(service_registry, service_b)
  let reg_result_c = ServiceRegistry::register(service_registry, service_c)
  
  match (reg_result_a, reg_result_b, reg_result_c) {
    (Ok(_), Ok(_), Ok(_)) => assert_true(true)
    _ => assert_true(false) // All should succeed
  }
  
  // Test service discovery
  let discovery_result_a = ServiceRegistry::discover(service_registry, "service-a")
  let discovery_result_b = ServiceRegistry::discover(service_registry, "service-b")
  let discovery_result_c = ServiceRegistry::discover(service_registry, "service-c")
  
  match discovery_result_a {
    Some(service) => {
      assert_eq(service.name, "service-a")
      assert_eq(service.version, "1.0.0")
      assert_eq(service.endpoint, "http://localhost:8001")
    }
    None => assert_true(false) // Should find service
  }
  
  match discovery_result_b {
    Some(service) => {
      assert_eq(service.name, "service-b")
      assert_eq(service.version, "1.2.0")
      assert_eq(service.endpoint, "http://localhost:8002")
    }
    None => assert_true(false) // Should find service
  }
  
  match discovery_result_c {
    Some(service) => {
      assert_eq(service.name, "service-c")
      assert_eq(service.version, "2.0.0")
      assert_eq(service.endpoint, "http://localhost:8003")
    }
    None => assert_true(false) // Should find service
  }
  
  // Test discovery of non-existent service
  let non_existent_result = ServiceRegistry::discover(service_registry, "non-existent")
  match non_existent_result {
    Some(_) => assert_true(false) // Should not find service
    None => assert_true(true)
  }
  
  // Test service deregistration
  let dereg_result = ServiceRegistry::deregister(service_registry, "service-b")
  match dereg_result {
    Ok(_) => assert_true(true)
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Verify service is no longer discoverable
  let post_dereg_result = ServiceRegistry::discover(service_registry, "service-b")
  match post_dereg_result {
    Some(_) => assert_true(false) // Should not find service
    None => assert_true(true)
  }
  
  // Test service health check
  let health_result_a = ServiceRegistry::health_check(service_registry, "service-a")
  let health_result_c = ServiceRegistry::health_check(service_registry, "service-c")
  
  match (health_result_a, health_result_c) {
    (Healthy, Healthy) => assert_true(true)
    _ => assert_true(false) // Both should be healthy
  }
}

// Test 2: Service-to-Service Communication
test "service to service communication" {
  let service_communicator = ServiceCommunicator::new()
  
  // Test synchronous communication
  let request = ServiceRequest::new("GET", "/api/telemetry", [], None)
  let sync_result = ServiceCommunicator::send_sync(service_communicator, "telemetry-service", request)
  
  match sync_result {
    Ok(response) => {
      assert_eq(response.status_code, 200)
      match response.body {
        Some(body) => assert_true(body.length() > 0)
        None => assert_true(false)
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test asynchronous communication
  let async_request = ServiceRequest::new("POST", "/api/telemetry", [("Content-Type", "application/json")], Some("{\"metric\":\"cpu.usage\",\"value\":75.5}"))
  let async_result = ServiceCommunicator::send_async(service_communicator, "telemetry-service", async_request)
  
  match async_result {
    Ok(future) => {
      let response = Future::await(future, 5000) // Wait 5 seconds
      match response {
        Some(resp) => {
          assert_eq(resp.status_code, 201)
          match resp.body {
            Some(body) => assert_true(String::contains(body, "created"))
            None => assert_true(false)
          }
        }
        None => assert_true(false) // Should get response
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test communication with retry
  let retry_config = RetryConfig::new(3, 500, ExponentialBackoff)
  let unreliable_request = ServiceRequest::new("GET", "/api/unreliable", [], None)
  let retry_result = ServiceCommunicator::send_with_retry(service_communicator, "unreliable-service", unreliable_request, retry_config)
  
  match retry_result {
    Ok(response) => {
      assert_eq(response.status_code, 200)
    }
    Error(err) => {
      match err {
        MaxRetriesExceeded => assert_true(true) // Expected for unreliable service
        _ => assert_true(false)
      }
    }
  }
  
  // Test batch communication
  let batch_requests = [
    ServiceRequest::new("GET", "/api/metrics/cpu", [], None),
    ServiceRequest::new("GET", "/api/metrics/memory", [], None),
    ServiceRequest::new("GET", "/api/metrics/disk", [], None)
  ]
  
  let batch_result = ServiceCommunicator::send_batch(service_communicator, "metrics-service", batch_requests)
  
  match batch_result {
    Ok(responses) => {
      assert_eq(responses.length(), 3)
      for response in responses {
        assert_eq(response.status_code, 200)
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
}

// Test 3: Load Balancing
test "load balancing" {
  let load_balancer = LoadBalancer::new()
  
  // Register multiple instances of the same service
  let instance1 = ServiceInstance::new("telemetry-service", "instance-1", "http://localhost:8001")
  let instance2 = ServiceInstance::new("telemetry-service", "instance-2", "http://localhost:8002")
  let instance3 = ServiceInstance::new("telemetry-service", "instance-3", "http://localhost:8003")
  
  LoadBalancer::register_instance(load_balancer, instance1)
  LoadBalancer::register_instance(load_balancer, instance2)
  LoadBalancer::register_instance(load_balancer, instance3)
  
  // Test round-robin load balancing
  LoadBalancer::set_strategy(load_balancer, RoundRobin)
  
  let rr1 = LoadBalancer::get_instance(load_balancer, "telemetry-service")
  let rr2 = LoadBalancer::get_instance(load_balancer, "telemetry-service")
  let rr3 = LoadBalancer::get_instance(load_balancer, "telemetry-service")
  let rr4 = LoadBalancer::get_instance(load_balancer, "telemetry-service")
  
  match (rr1, rr2, rr3, rr4) {
    (Some(inst1), Some(inst2), Some(inst3), Some(inst4)) => {
      assert_eq(inst1.instance_id, "instance-1")
      assert_eq(inst2.instance_id, "instance-2")
      assert_eq(inst3.instance_id, "instance-3")
      assert_eq(inst4.instance_id, "instance-1") // Should cycle back
    }
    _ => assert_true(false) // All should succeed
  }
  
  // Test weighted load balancing
  LoadBalancer::set_strategy(load_balancer, Weighted)
  LoadBalancer::set_weight(load_balancer, "instance-1", 1)
  LoadBalancer::set_weight(load_balancer, "instance-2", 3)
  LoadBalancer::set_weight(load_balancer, "instance-3", 6)
  
  let mut weight_counts = Map::new()
  
  for i in 0..=100 {
    let instance = LoadBalancer::get_instance(load_balancer, "telemetry-service")
    match instance {
      Some(inst) => {
        let count = Map::get(weight_counts, inst.instance_id)
        match count {
          Some(c) => Map::set(weight_counts, inst.instance_id, c + 1)
          None => Map::set(weight_counts, inst.instance_id, 1)
        }
      }
      None => assert_true(false) // Should get instance
    }
  }
  
  // Verify distribution follows weights (approximately)
  let count1 = Map::get(weight_counts, "instance-1")
  let count2 = Map::get(weight_counts, "instance-2")
  let count3 = Map::get(weight_counts, "instance-3")
  
  match (count1, count2, count3) {
    (Some(c1), Some(c2), Some(c3)) => {
      assert_true(c1 < c2) // instance-2 should get more than instance-1
      assert_true(c2 < c3) // instance-3 should get more than instance-2
    }
    _ => assert_true(false) // All should have counts
  }
  
  // Test least connections load balancing
  LoadBalancer::set_strategy(load_balancer, LeastConnections)
  
  // Simulate connections
  LoadBalancer::increment_connections(load_balancer, "instance-1")
  LoadBalancer::increment_connections(load_balancer, "instance-1")
  LoadBalancer::increment_connections(load_balancer, "instance-2")
  
  let lc_instance = LoadBalancer::get_instance(load_balancer, "telemetry-service")
  match lc_instance {
    Some(instance) => {
      assert_eq(instance.instance_id, "instance-3") // Should have least connections
    }
    None => assert_true(false) // Should get instance
  }
  
  // Test health-aware load balancing
  LoadBalancer::set_strategy(load_balancer, HealthAware)
  
  // Mark instance-2 as unhealthy
  LoadBalancer::mark_unhealthy(load_balancer, "instance-2")
  
  let ha_instance = LoadBalancer::get_instance(load_balancer, "telemetry-service")
  match ha_instance {
    Some(instance) => {
      assert_not_eq(instance.instance_id, "instance-2") // Should not be instance-2
    }
    None => assert_true(false) // Should get instance
  }
}

// Test 4: Circuit Breaker Pattern
test "circuit breaker pattern" {
  let circuit_breaker = CircuitBreaker::new("telemetry-service", 5, 10000) // 5 failures, 10 second timeout
  
  // Test circuit breaker in closed state
  assert_eq(CircuitBreaker::get_state(circuit_breaker), Closed)
  
  // Test successful calls
  for i in 0..=3 {
    let result = CircuitBreaker::call(circuit_breaker, func() {
      // Simulate successful call
      Ok("success")
    })
    
    match result {
      Ok(value) => assert_eq(value, "success")
      Error(_) => assert_true(false) // Should succeed
    }
  }
  
  // Verify circuit breaker is still closed
  assert_eq(CircuitBreaker::get_state(circuit_breaker), Closed)
  
  // Test failed calls to trigger circuit breaker
  for i in 0..=5 {
    let result = CircuitBreaker::call(circuit_breaker, func() {
      // Simulate failed call
      Error(NetworkError)
    })
    
    if i < 4 {
      match result {
        Ok(_) => assert_true(false)
        Error(NetworkError) => assert_true(true)
        Error(_) => assert_true(false)
      }
    } else {
      match result {
        Ok(_) => assert_true(false)
        Error(CircuitBreakerOpen) => assert_true(true)
        Error(_) => assert_true(false)
      }
    }
  }
  
  // Verify circuit breaker is now open
  assert_eq(CircuitBreaker::get_state(circuit_breaker), Open)
  
  // Test calls are immediately rejected when circuit is open
  let open_result = CircuitBreaker::call(circuit_breaker, func() {
    Ok("should not be called")
  })
  
  match open_result {
    Ok(_) => assert_true(false)
    Error(CircuitBreakerOpen) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Test circuit breaker transitions to half-open after timeout
  Time::sleep(11000) // Wait longer than timeout
  
  // Test call in half-open state
  let half_open_result = CircuitBreaker::call(circuit_breaker, func() {
    Ok("success after timeout")
  })
  
  match half_open_result {
    Ok(value) => assert_eq(value, "success after timeout")
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Verify circuit breaker is closed again after successful call
  assert_eq(CircuitBreaker::get_state(circuit_breaker), Closed)
  
  // Test circuit breaker statistics
  let stats = CircuitBreaker::get_stats(circuit_breaker)
  assert_eq(stats.total_calls, 9)
  assert_eq(stats.successful_calls, 4)
  assert_eq(stats.failed_calls, 5)
  assert_eq(stats.circuit_breaker_opens, 1)
}

// Test 5: Distributed Tracing
test "distributed tracing" {
  let tracer = DistributedTracer::new()
  
  // Create a root span
  let root_span = Tracer::start_span(tracer, "root_operation")
  Span::set_attribute(root_span, "service.name", "telemetry-service")
  Span::set_attribute(root_span, "operation.type", "processing")
  
  // Create child spans
  let child_span1 = Tracer::start_span_with_parent(tracer, "database_query", root_span)
  Span::set_attribute(child_span1, "db.type", "postgresql")
  Span::set_attribute(child_span1, "db.statement", "SELECT * FROM metrics")
  
  let child_span2 = Tracer::start_span_with_parent(tracer, "api_call", root_span)
  Span::set_attribute(child_span2, "http.method", "GET")
  Span::set_attribute(child_span2, "http.url", "https://api.example.com/data")
  
  // Create grandchild span
  let grandchild_span = Tracer::start_span_with_parent(tracer, "data_processing", child_span2)
  Span::set_attribute(grandchild_span, "processing.type", "aggregation")
  
  // End spans in reverse order
  Span::end(grandchild_span)
  Span::end(child_span2)
  Span::end(child_span1)
  Span::end(root_span)
  
  // Verify trace context propagation
  let trace_context = Span::get_context(root_span)
  assert_not_eq(trace_context.trace_id, "")
  assert_not_eq(trace_context.span_id, "")
  
  // Verify parent-child relationships
  assert_eq(Span::get_parent_id(child_span1), Span::get_span_id(root_span))
  assert_eq(Span::get_parent_id(child_span2), Span::get_span_id(root_span))
  assert_eq(Span::get_parent_id(grandchild_span), Span::get_span_id(child_span2))
  
  // Test trace context injection and extraction
  let headers = []
  let injected_headers = Tracer::inject_context(tracer, trace_context, headers)
  
  assert_true(contains_header(injected_headers, "traceparent"))
  assert_true(contains_header(injected_headers, "tracestate"))
  
  let extracted_context = Tracer::extract_context(tracer, injected_headers)
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, trace_context.trace_id)
      assert_eq(context.span_id, trace_context.span_id)
    }
    None => assert_true(false) // Should extract context
  }
  
  // Test trace collection
  let trace = Tracer::get_trace(tracer, trace_context.trace_id)
  match trace {
    Some(collected_trace) => {
      assert_eq(collected_trace.spans.length(), 4)
      assert_eq(collected_trace.spans[0].operation_name, "root_operation")
      assert_eq(collected_trace.spans[1].operation_name, "database_query")
      assert_eq(collected_trace.spans[2].operation_name, "api_call")
      assert_eq(collected_trace.spans[3].operation_name, "data_processing")
    }
    None => assert_true(false) // Should find trace
  }
}

// Test 6: Message Queue Communication
test "message queue communication" {
  let message_queue = MessageQueue::new("telemetry-events")
  
  // Test message publishing
  let telemetry_event = TelemetryEvent::new("metric_update", [
    ("metric_name", "cpu.usage"),
    ("metric_value", "75.5"),
    ("timestamp", "1634567890")
  ])
  
  let publish_result = MessageQueue::publish(message_queue, telemetry_event)
  match publish_result {
    Ok(message_id) => assert_not_eq(message_id, "")
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test message subscription
  let subscription = MessageQueue::subscribe(message_queue, "metric-updates")
  
  // Test message consumption
  let consume_result = MessageQueue::consume(subscription, 5000) // Wait 5 seconds
  match consume_result {
    Some(message) => {
      assert_eq(message.event_type, "metric_update")
      assert_eq(message.attributes["metric_name"], "cpu.usage")
      assert_eq(message.attributes["metric_value"], "75.5")
      assert_eq(message.attributes["timestamp"], "1634567890")
    }
    None => assert_true(false) // Should receive message
  }
  
  // Test message acknowledgment
  let ack_result = MessageQueue::acknowledge(subscription, consume_result.unwrap().message_id)
  match ack_result {
    Ok(_) => assert_true(true)
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test batch publishing
  let batch_events = [
    TelemetryEvent::new("metric_update", [
      ("metric_name", "memory.usage"),
      ("metric_value", "68.2"),
      ("timestamp", "1634567891")
    ]),
    TelemetryEvent::new("metric_update", [
      ("metric_name", "disk.usage"),
      ("metric_value", "45.7"),
      ("timestamp", "1634567892")
    ])
  ]
  
  let batch_publish_result = MessageQueue::publish_batch(message_queue, batch_events)
  match batch_publish_result {
    Ok(message_ids) => assert_eq(message_ids.length(), 2)
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test batch consumption
  let batch_consume_result = MessageQueue::consume_batch(subscription, 10, 5000)
  match batch_consume_result {
    Some(messages) => {
      assert_eq(messages.length(), 2)
      assert_eq(messages[0].attributes["metric_name"], "memory.usage")
      assert_eq(messages[1].attributes["metric_name"], "disk.usage")
    }
    None => assert_true(false) // Should receive messages
  }
  
  // Test message filtering
  let filtered_subscription = MessageQueue::subscribe_with_filter(message_queue, "cpu-metrics", func(event) {
    event.attributes["metric_name"] == "cpu.usage"
  })
  
  let cpu_event = TelemetryEvent::new("metric_update", [
    ("metric_name", "cpu.usage"),
    ("metric_value", "80.1"),
    ("timestamp", "1634567893")
  ])
  
  let memory_event = TelemetryEvent::new("metric_update", [
    ("metric_name", "memory.usage"),
    ("metric_value", "70.3"),
    ("timestamp", "1634567894")
  ])
  
  MessageQueue::publish(message_queue, cpu_event)
  MessageQueue::publish(message_queue, memory_event)
  
  let filtered_result = MessageQueue::consume(filtered_subscription, 5000)
  match filtered_result {
    Some(message) => {
      assert_eq(message.attributes["metric_name"], "cpu.usage")
      assert_eq(message.attributes["metric_value"], "80.1")
    }
    None => assert_true(false) // Should receive CPU message
  }
}

// Test 7: API Gateway Communication
test "api gateway communication" {
  let api_gateway = ApiGateway::new()
  
  // Register routes
  let telemetry_route = Route::new("/api/telemetry", "GET", "telemetry-service")
  let metrics_route = Route::new("/api/metrics", "GET", "metrics-service")
  let events_route = Route::new("/api/events", "POST", "event-service")
  
  ApiGateway::register_route(api_gateway, telemetry_route)
  ApiGateway::register_route(api_gateway, metrics_route)
  ApiGateway::register_route(api_gateway, events_route)
  
  // Test request routing
  let telemetry_request = ApiRequest::new("GET", "/api/telemetry", [], None)
  let telemetry_result = ApiGateway::route_request(api_gateway, telemetry_request)
  
  match telemetry_result {
    Ok(route) => assert_eq(route.service_name, "telemetry-service")
    Error(_) => assert_true(false) // Should succeed
  }
  
  let metrics_request = ApiRequest::new("GET", "/api/metrics", [], None)
  let metrics_result = ApiGateway::route_request(api_gateway, metrics_request)
  
  match metrics_result {
    Ok(route) => assert_eq(route.service_name, "metrics-service")
    Error(_) => assert_true(false) // Should succeed
  }
  
  let events_request = ApiRequest::new("POST", "/api/events", [], None)
  let events_result = ApiGateway::route_request(api_gateway, events_request)
  
  match events_result {
    Ok(route) => assert_eq(route.service_name, "event-service")
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test request transformation
  let transformer = RequestTransformer::new()
  RequestTransformer::add_header(transformer, "X-Request-ID", "req-12345")
  RequestTransformer::add_header(transformer, "X-Forwarded-For", "client-ip")
  
  let original_request = ApiRequest::new("GET", "/api/telemetry", [], None)
  let transformed_request = ApiGateway::transform_request(api_gateway, original_request, transformer)
  
  assert_true(contains_header(transformed_request.headers, "X-Request-ID"))
  assert_true(contains_header(transformed_request.headers, "X-Forwarded-For"))
  
  // Test response transformation
  let response_transformer = ResponseTransformer::new()
  ResponseTransformer::add_header(response_transformer, "X-Response-Time", "150ms")
  ResponseTransformer::add_header(response_transformer, "X-Cache", "MISS")
  
  let original_response = ApiResponse::new(200, [("Content-Type", "application/json")], Some("{\"data\":\"value\"}"))
  let transformed_response = ApiGateway::transform_response(api_gateway, original_response, response_transformer)
  
  assert_true(contains_header(transformed_response.headers, "X-Response-Time"))
  assert_true(contains_header(transformed_response.headers, "X-Cache"))
  
  // Test request aggregation
  let aggregation_config = AggregationConfig::new()
  AggregationConfig::add_service(aggregation_config, "telemetry-service", "/api/telemetry")
  AggregationConfig::add_service(aggregation_config, "metrics-service", "/api/metrics")
  
  let aggregation_request = ApiRequest::new("GET", "/api/aggregated", [], None)
  let aggregation_result = ApiGateway::aggregate_requests(api_gateway, aggregation_request, aggregation_config)
  
  match aggregation_result {
    Ok(responses) => {
      assert_eq(responses.length(), 2)
      assert_eq(responses["telemetry-service"].status_code, 200)
      assert_eq(responses["metrics-service"].status_code, 200)
    }
    Error(_) => assert_true(false) // Should succeed
  }
}

// Test 8: Service Mesh Communication
test "service mesh communication" {
  let service_mesh = ServiceMesh::new()
  
  // Register services in the mesh
  let telemetry_service = MeshService::new("telemetry-service", "v1", ["sidecar-telemetry"])
  let metrics_service = MeshService::new("metrics-service", "v1", ["sidecar-metrics"])
  let event_service = MeshService::new("event-service", "v1", ["sidecar-event"])
  
  ServiceMesh::register_service(service_mesh, telemetry_service)
  ServiceMesh::register_service(service_mesh, metrics_service)
  ServiceMesh::register_service(service_mesh, event_service)
  
  // Test service mesh routing
  let mesh_request = MeshRequest::new("telemetry-service", "/api/data", "GET", [], None)
  let mesh_result = ServiceMesh::route(service_mesh, mesh_request)
  
  match mesh_result {
    Ok(route) => {
      assert_eq(route.destination_service, "telemetry-service")
      assert_eq(route.destination_sidecar, "sidecar-telemetry")
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test traffic splitting
  let traffic_split = TrafficSplit::new("telemetry-service", [
    ("telemetry-service-v1", 80),
    ("telemetry-service-v2", 20)
  ])
  
  ServiceMesh::configure_traffic_split(service_mesh, traffic_split)
  
  let mut v1_count = 0
  let mut v2_count = 0
  
  for i in 0..=100 {
    let split_request = MeshRequest::new("telemetry-service", "/api/data", "GET", [], None)
    let split_result = ServiceMesh::route_with_split(service_mesh, split_request)
    
    match split_result {
      Ok(route) => {
        if route.destination_service == "telemetry-service-v1" {
          v1_count = v1_count + 1
        } else if route.destination_service == "telemetry-service-v2" {
          v2_count = v2_count + 1
        }
      }
      Error(_) => assert_true(false) // Should succeed
    }
  }
  
  // Verify traffic split (approximately)
  let v1_percentage = v1_count.to_float() / 100.0
  let v2_percentage = v2_count.to_float() / 100.0
  
  assert_true(v1_percentage > 0.7 && v1_percentage < 0.9) // Around 80%
  assert_true(v2_percentage > 0.1 && v2_percentage < 0.3) // Around 20%
  
  // Test fault injection
  let fault_injection = FaultInjection::new("metrics-service", [
    LatencyFault(1000), // 1 second delay
    ErrorFault(500, 0.1) // 10% chance of 500 error
  ])
  
  ServiceMesh::configure_fault_injection(service_mesh, fault_injection)
  
  let fault_request = MeshRequest::new("metrics-service", "/api/data", "GET", [], None)
  let fault_result = ServiceMesh::route_with_fault(service_mesh, fault_request)
  
  match fault_result {
    Ok(route) => {
      // Should route successfully but with injected latency
      assert_eq(route.destination_service, "metrics-service")
    }
    Error(err) => {
      // Might get injected error
      match err {
        InjectedError => assert_true(true)
        _ => assert_true(false)
      }
    }
  }
  
  // Test mutual TLS
  let mtls_config = MutualTLSConfig::new()
  MutualTLSConfig::set_certificate(mtls_config, "telemetry-service", "cert-telemetry.pem")
  MutualTLSConfig::set_certificate(mtls_config, "metrics-service", "cert-metrics.pem")
  MutualTLSConfig::set_certificate(mtls_config, "event-service", "cert-event.pem")
  
  ServiceMesh::configure_mutual_tls(service_mesh, mtls_config)
  
  let mtls_request = MeshRequest::new("telemetry-service", "/api/data", "GET", [], None)
  let mtls_result = ServiceMesh::route_with_mtls(service_mesh, mtls_request)
  
  match mtls_result {
    Ok(route) => {
      assert_true(route.secure)
      assert_eq(route.destination_service, "telemetry-service")
    }
    Error(_) => assert_true(false) // Should succeed
  }
}

// Test 9: Cross-Service Authentication
test "cross service authentication" {
  let auth_manager = AuthenticationManager::new()
  
  // Test JWT token generation
  let claims = Claims::new("telemetry-service", ["read", "write"], 3600)
  let jwt_result = AuthenticationManager::generate_jwt(auth_manager, claims)
  
  match jwt_result {
    Ok(token) => {
      assert_true(token.length() > 0)
      
      // Test JWT token validation
      let validation_result = AuthenticationManager::validate_jwt(auth_manager, token)
      
      match validation_result {
        Ok(validated_claims) => {
          assert_eq(validated_claims.subject, "telemetry-service")
          assert_true(validated_claims.scope.contains("read"))
          assert_true(validated_claims.scope.contains("write"))
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test OAuth2 token exchange
  let oauth_config = OAuth2Config::new("client-id", "client-secret", "https://auth.example.com")
  AuthenticationManager::configure_oauth2(auth_manager, oauth_config)
  
  let oauth_result = AuthenticationManager::exchange_client_credentials(auth_manager)
  match oauth_result {
    Ok(token_info) => {
      assert_not_eq(token_info.access_token, "")
      assert_true(token_info.expires_in > 0)
      
      // Test OAuth2 token refresh
      let refresh_result = AuthenticationManager::refresh_token(auth_manager, token_info.refresh_token)
      
      match refresh_result {
        Ok(refreshed_token) => {
          assert_not_eq(refreshed_token.access_token, token_info.access_token)
        }
        Error(_) => assert_true(false) // Should succeed
      }
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test service-to-service authentication
  let service_credentials = ServiceCredentials::new("telemetry-service", "secret-key")
  AuthenticationManager::register_service_credentials(auth_manager, service_credentials)
  
  let auth_request = AuthenticatedRequest::new("GET", "/api/data", [], None, "telemetry-service")
  let auth_result = AuthenticationManager::authenticate_request(auth_manager, auth_request)
  
  match auth_result {
    Ok(authenticated_request) => {
      assert_true(contains_header(authenticated_request.headers, "Authorization"))
      assert_true(contains_header(authenticated_request.headers, "X-Service-Identity"))
    }
    Error(_) => assert_true(false) // Should succeed
  }
  
  // Test request with invalid credentials
  let invalid_request = AuthenticatedRequest::new("GET", "/api/data", [], None, "unknown-service")
  let invalid_result = AuthenticationManager::authenticate_request(auth_manager, invalid_request)
  
  match invalid_result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(err) => {
      match err {
        AuthenticationFailed => assert_true(true)
        InvalidCredentials => assert_true(true)
        _ => assert_true(false)
      }
    }
  }
}

// Test 10: Service Observability
test "service observability" {
  let observability_manager = ObservabilityManager::new()
  
  // Test metrics collection
  let metrics_collector = MetricsCollector::new()
  ObservabilityManager::register_metrics_collector(observability_manager, metrics_collector)
  
  // Record some metrics
  MetricsCollector::record_counter(metrics_collector, "http.requests.total", 1.0, [
    ("service", "telemetry-service"),
    ("method", "GET"),
    ("status", "200")
  ])
  
  MetricsCollector::record_histogram(metrics_collector, "http.request.duration", 150.5, [
    ("service", "telemetry-service"),
    ("method", "GET")
  ])
  
  MetricsCollector::record_gauge(metrics_collector, "service.memory.usage", 512.0, [
    ("service", "telemetry-service")
  ])
  
  // Test metrics retrieval
  let metrics = ObservabilityManager::get_metrics(observability_manager, "telemetry-service")
  assert_true(metrics.length() >= 3)
  
  // Test distributed tracing
  let tracer = DistributedTracer::new()
  ObservabilityManager::register_tracer(observability_manager, tracer)
  
  let span = Tracer::start_span(tracer, "service-operation")
  Span::set_attribute(span, "service.name", "telemetry-service")
  Span::set_attribute(span, "operation.name", "process-telemetry")
  Span::end(span)
  
  // Test trace retrieval
  let traces = ObservabilityManager::get_traces(observability_manager, "telemetry-service")
  assert_true(traces.length() >= 1)
  
  // Test logging
  let logger = StructuredLogger::new()
  ObservabilityManager::register_logger(observability_manager, logger)
  
  let log_entry = LogEntry::new(Info, "Processing telemetry data", [
    ("service", "telemetry-service"),
    ("operation", "process-telemetry"),
    ("trace_id", Span::get_trace_id(span))
  ])
  
  StructuredLogger::log(logger, log_entry)
  
  // Test log retrieval
  let logs = ObservabilityManager::get_logs(observability_manager, "telemetry-service")
  assert_true(logs.length() >= 1)
  
  // Test health checks
  let health_checker = HealthChecker::new()
  ObservabilityManager::register_health_checker(observability_manager, health_checker)
  
  HealthChecker::add_check(health_checker, "database", func() {
    // Simulate database health check
    Healthy
  })
  
  HealthChecker::add_check(health_checker, "cache", func() {
    // Simulate cache health check
    Healthy
  })
  
  HealthChecker::add_check(health_checker, "external-api", func() {
    // Simulate external API health check
    Degraded("High latency")
  })
  
  let health_status = ObservabilityManager::get_health_status(observability_manager, "telemetry-service")
  match health_status {
    Degraded(reasons) => {
      assert_true(reasons.contains("external-api"))
    }
    _ => assert_true(false) // Should be degraded due to external API
  }
  
  // Test service dashboard data
  let dashboard_data = ObservabilityManager::get_dashboard_data(observability_manager, "telemetry-service")
  
  assert_true(dashboard_data.metrics.length() >= 3)
  assert_true(dashboard_data.traces.length() >= 1)
  assert_true(dashboard_data.logs.length() >= 1)
  match dashboard_data.health_status {
    Degraded(_) => assert_true(true)
    _ => assert_true(false) // Should be degraded
  }
}

// Helper function to check if headers contain a specific header
func contains_header(headers : Array[(String, String)], header_name : String) -> Bool {
  for (name, _) in headers {
    if name == header_name {
      return true
    }
  }
  return false
}