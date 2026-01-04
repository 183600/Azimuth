// Azimuth High-Quality Cross-Service Communication Tests
// This file contains comprehensive test cases for cross-service communication

// Test 1: Basic Service Discovery and Registration
test "basic service discovery and registration" {
  let service_registry = ServiceRegistry::new()
  
  // Test service registration
  let service_info = ServiceInfo::new()
    .with_id("user-service-1")
    .with_name("user-service")
    .with_version("1.0.0")
    .with_host("localhost")
    .with_port(8080)
    .with_protocol("http")
    .with_health_check_url("http://localhost:8080/health")
    .with_metadata({"team": "auth", "environment": "development"})
  
  let registration_result = service_registry.register(service_info)
  assert_true(registration_result.is_success())
  
  // Test service discovery by name
  let discovery_result = service_registry.discover_by_name("user-service")
  assert_true(discovery_result.is_success())
  
  let services = discovery_result.unwrap()
  assert_eq(services.length(), 1)
  assert_eq(services[0].id, "user-service-1")
  assert_eq(services[0].name, "user-service")
  assert_eq(services[0].version, "1.0.0")
  
  // Test service discovery by ID
  let discovery_by_id_result = service_registry.discover_by_id("user-service-1")
  assert_true(discovery_by_id_result.is_success())
  
  let service = discovery_by_id_result.unwrap()
  assert_eq(service.id, "user-service-1")
  assert_eq(service.name, "user-service")
  
  // Test service discovery with filters
  let filter = ServiceFilter::new()
    .with_name("user-service")
    .with_version("1.0.0")
    .with_metadata({"team": "auth"})
  
  let filtered_result = service_registry.discover_with_filter(filter)
  assert_true(filtered_result.is_success())
  
  let filtered_services = filtered_result.unwrap()
  assert_eq(filtered_services.length(), 1)
  
  // Test service discovery with tags
  let tagged_service_info = ServiceInfo::new()
    .with_id("order-service-1")
    .with_name("order-service")
    .with_version("1.2.0")
    .with_host("localhost")
    .with_port(8081)
    .with_protocol("http")
    .with_tags(["payment", "processing", "critical"])
  
  let tagged_registration = service_registry.register(tagged_service_info)
  assert_true(tagged_registration.is_success())
  
  let tag_filter = ServiceFilter::new()
    .with_tags(["payment"])
  
  let tagged_result = service_registry.discover_with_filter(tag_filter)
  assert_true(tagged_result.is_success())
  
  let tagged_services = tagged_result.unwrap()
  assert_eq(tagged_services.length(), 1)
  assert_eq(tagged_services[0].id, "order-service-1")
  
  // Test service deregistration
  let deregistration_result = service_registry.deregister("user-service-1")
  assert_true(deregistration_result.is_success())
  
  let discovery_after_dereg = service_registry.discover_by_name("user-service")
  assert_true(discovery_after_dereg.is_success())
  assert_eq(discovery_after_dereg.unwrap().length(), 0)
  
  // Test service health status
  let health_check_result = service_registry.check_health("order-service-1")
  assert_true(health_check_result.is_success())
  
  let health_status = health_check_result.unwrap()
  assert_true(health_status.is_healthy)
  assert_true(health_status.response_time_ms > 0)
  
  // Test service registry statistics
  let registry_stats = service_registry.get_statistics()
  assert_eq(registry_stats.total_services, 1)
  assert_eq(registry_stats.healthy_services, 1)
  assert_eq(registry_stats.unhealthy_services, 0)
}

// Test 2: Load Balancing Strategies
test "load balancing strategies" {
  let service_registry = ServiceRegistry::new()
  
  // Register multiple instances of the same service
  for i in 1..=5 {
    let service_info = ServiceInfo::new()
      .with_id(format!("user-service-{}", i))
      .with_name("user-service")
      .with_version("1.0.0")
      .with_host("localhost")
      .with_port(8080 + i)
      .with_protocol("http")
      .with_weight(i) // Different weights
    
    service_registry.register(service_info)
  }
  
  // Test round-robin load balancing
  let round_robin_balancer = LoadBalancer::new()
    .with_strategy(LoadBalancingStrategy::RoundRobin)
  
  let round_robin_requests = []
  for i in 1..=10 {
    let service = round_robin_balancer.select_service("user-service", service_registry)
    round_robin_requests.push(service.id)
  }
  
  // Should distribute evenly across all instances
  let mut service_counts = {}
  for service_id in round_robin_requests {
    service_counts[service_id] = service_counts.get(service_id).unwrap_or(0) + 1
  }
  
  for i in 1..=5 {
    let service_id = format!("user-service-{}", i)
    assert_eq(service_counts.get(service_id).unwrap_or(0), 2) // Each service should get 2 requests
  }
  
  // Test weighted load balancing
  let weighted_balancer = LoadBalancer::new()
    .with_strategy(LoadBalancingStrategy::Weighted)
  
  let weighted_requests = []
  for i in 1..=100 {
    let service = weighted_balancer.select_service("user-service", service_registry)
    weighted_requests.push(service.id)
  }
  
  // Should distribute based on weights
  let mut weighted_counts = {}
  for service_id in weighted_requests {
    weighted_counts[service_id] = weighted_counts.get(service_id).unwrap_or(0) + 1
  }
  
  // Higher weight services should get more requests
  assert_true(weighted_counts.get("user-service-5").unwrap_or(0) > weighted_counts.get("user-service-1").unwrap_or(0))
  
  // Test least connections load balancing
  let least_connections_balancer = LoadBalancer::new()
    .with_strategy(LoadBalancingStrategy::LeastConnections)
  
  // Simulate active connections
  let connection_tracker = ConnectionTracker::new()
  connection_tracker.set_active_connections("user-service-1", 10)
  connection_tracker.set_active_connections("user-service-2", 5)
  connection_tracker.set_active_connections("user-service-3", 2)
  connection_tracker.set_active_connections("user-service-4", 8)
  connection_tracker.set_active_connections("user-service-5", 1)
  
  least_connections_balancer.set_connection_tracker(connection_tracker)
  
  let selected_service = least_connections_balancer.select_service("user-service", service_registry)
  assert_eq(selected_service.id, "user-service-5") // Should select service with least connections
  
  // Test random load balancing
  let random_balancer = LoadBalancer::new()
    .with_strategy(LoadBalancingStrategy::Random)
  
  let random_requests = []
  for i in 1..=100 {
    let service = random_balancer.select_service("user-service", service_registry)
    random_requests.push(service.id)
  }
  
  // Should distribute randomly (not evenly necessarily)
  let mut random_counts = {}
  for service_id in random_requests {
    random_counts[service_id] = random_counts.get(service_id).unwrap_or(0) + 1
  }
  
  // All services should get some requests
  for i in 1..=5 {
    let service_id = format!("user-service-{}", i)
    assert_true(random_counts.get(service_id).unwrap_or(0) > 0)
  }
  
  // Test IP hash load balancing
  let ip_hash_balancer = LoadBalancer::new()
    .with_strategy(LoadBalancingStrategy::IPHash)
  
  let client_ips = [
    "192.168.1.100",
    "192.168.1.101",
    "192.168.1.102",
    "192.168.1.103"
  ]
  
  let ip_hash_results = {}
  for ip in client_ips {
    let service = ip_hash_balancer.select_service_for_client("user-service", service_registry, ip)
    ip_hash_results[ip] = service.id
  }
  
  // Same client IP should always map to the same service
  for ip in client_ips {
    let service1 = ip_hash_balancer.select_service_for_client("user-service", service_registry, ip)
    let service2 = ip_hash_balancer.select_service_for_client("user-service", service_registry, ip)
    assert_eq(service1.id, service2.id)
  }
  
  // Test health-aware load balancing
  let health_aware_balancer = LoadBalancer::new()
    .with_strategy(LoadBalancingStrategy::HealthAware)
  
  // Mark some services as unhealthy
  service_registry.set_health_status("user-service-1", false)
  service_registry.set_health_status("user-service-3", false)
  
  let healthy_service = health_aware_balancer.select_service("user-service", service_registry)
  assert_ne(healthy_service.id, "user-service-1") // Should not select unhealthy service
  assert_ne(healthy_service.id, "user-service-3") // Should not select unhealthy service
  
  // Test load balancer statistics
  let balancer_stats = round_robin_balancer.get_statistics()
  assert_true(balancer_stats.total_requests > 0)
  assert_true(balancer_stats.service_selections.length() > 0)
}

// Test 3: Service Mesh Communication
test "service mesh communication" {
  let service_mesh = ServiceMesh::new()
  
  // Configure service mesh
  service_mesh.add_service("user-service", ServiceConfig::new()
    .with_host("localhost")
    .with_port(8080)
    .with_protocol("http")
    .with_endpoints(["/users", "/users/{id}", "/users/search"]))
  
  service_mesh.add_service("order-service", ServiceConfig::new()
    .with_host("localhost")
    .with_port(8081)
    .with_protocol("http")
    .with_endpoints(["/orders", "/orders/{id}", "/orders/create"]))
  
  service_mesh.add_service("payment-service", ServiceConfig::new()
    .with_host("localhost")
    .with_port(8082)
    .with_protocol("http")
    .with_endpoints(["/payments", "/payments/{id}", "/payments/process"]))
  
  // Test service-to-service communication
  let communication_channel = service_mesh.create_channel("user-service", "order-service")
  
  let request = ServiceRequest::new()
    .with_method("GET")
    .with_path("/orders")
    .with_headers({"Content-Type": "application/json"})
    .with_query_params({"user_id": "123"})
  
  let response = communication_channel.send(request)
  assert_true(response.is_success())
  
  let response_data = response.unwrap()
  assert_eq(response_data.status_code, 200)
  assert_true(response_data.body.length() > 0)
  
  // Test request routing
  let router = service_mesh.get_router()
  
  let route_config = RouteConfig::new()
    .with_source("user-service")
    .with_destination("order-service")
    .with_path_prefix("/orders")
    .with_rewrite("/api/orders")
    .with_timeout(Duration::from_seconds(30))
    .with_retry_policy(RetryPolicy::new()
      .with_max_attempts(3)
      .with_backoff(BackoffStrategy::Exponential))
  
  router.add_route(route_config)
  
  let routed_request = ServiceRequest::new()
    .with_method("GET")
    .with_path("/orders/123")
    .with_headers({"Authorization": "Bearer token123"})
  
  let routed_response = router.route(routed_request)
  assert_true(routed_response.is_success())
  
  // Test circuit breaker pattern
  let circuit_breaker = CircuitBreaker::new()
    .with_failure_threshold(5)
    .with_timeout(Duration::from_seconds(60))
    .with_half_open_max_calls(3)
  
  service_mesh.add_circuit_breaker("payment-service", circuit_breaker)
  
  // Simulate failures to trigger circuit breaker
  for i in 1..=6 {
    let failing_request = ServiceRequest::new()
      .with_method("POST")
      .with_path("/payments/process")
      .with_body("{\"amount\": 100.00, \"currency\": \"USD\"}")
    
    let failing_response = service_mesh.send_request("user-service", "payment-service", failing_request)
    
    if i <= 5 {
      assert_true(failing_response.is_error()) // Simulate failures
    }
  }
  
  // Circuit breaker should be open now
  let circuit_breaker_request = ServiceRequest::new()
    .with_method("POST")
    .with_path("/payments/process")
    .with_body("{\"amount\": 50.00, \"currency\": \"USD\"}")
  
  let circuit_breaker_response = service_mesh.send_request("user-service", "payment-service", circuit_breaker_request)
  assert_true(circuit_breaker_response.is_error())
  assert_eq(circuit_breaker_response.unwrap_error().error_type, ServiceErrorType::CircuitBreakerOpen)
  
  // Test service mesh observability
  let observability = service_mesh.get_observability()
  
  let metrics = observability.get_metrics()
  assert_true(metrics.request_count > 0)
  assert_true(metrics.response_time_avg > 0)
  assert_true(metrics.error_rate >= 0.0)
  
  let traces = observability.get_traces()
  assert_true(traces.length() > 0)
  
  for trace in traces {
    assert_true(trace.trace_id.length() > 0)
    assert_true(trace.span_id.length() > 0)
    assert_true(trace.service_name.length() > 0)
    assert_true(trace.operation_name.length() > 0)
    assert_true(trace.start_time > 0)
    assert_true(trace.end_time > 0)
  }
  
  // Test service mesh security
  let security = service_mesh.get_security()
  
  // Enable mTLS
  security.enable_mutual_tls(true)
  
  // Add service identities
  security.add_service_identity("user-service", "user-service-cert")
  security.add_service_identity("order-service", "order-service-cert")
  security.add_service_identity("payment-service", "payment-service-cert")
  
  // Test secure communication
  let secure_request = ServiceRequest::new()
    .with_method("GET")
    .with_path("/orders")
    .with_headers({"Authorization": "Bearer token123"})
  
  let secure_response = service_mesh.send_secure_request("user-service", "order-service", secure_request)
  assert_true(secure_response.is_success())
  
  // Test service mesh configuration management
  let config_manager = service_mesh.get_config_manager()
  
  // Add configuration
  config_manager.add_config("user-service", Config::new()
    .with_key("database.url")
    .with_value("postgresql://localhost:5432/users")
    .with_version("1.0.0"))
  
  config_manager.add_config("order-service", Config::new()
    .with_key("payment.timeout")
    .with_value("30s")
    .with_version("1.0.0"))
  
  // Get configuration
  let user_service_config = config_manager.get_config("user-service", "database.url")
  assert_true(user_service_config.is_some())
  assert_eq(user_service_config.unwrap().value, "postgresql://localhost:5432/users")
  
  // Test configuration updates
  config_manager.update_config("user-service", Config::new()
    .with_key("database.url")
    .with_value("postgresql://localhost:5432/users_v2")
    .with_version("1.1.0"))
  
  let updated_config = config_manager.get_config("user-service", "database.url")
  assert_true(updated_config.is_some())
  assert_eq(updated_config.unwrap().value, "postgresql://localhost:5432/users_v2")
  assert_eq(updated_config.unwrap().version, "1.1.0")
}

// Test 4: Message Queuing and Event Streaming
test "message queuing and event streaming" {
  let message_queue = MessageQueue::new()
  
  // Create topics
  let user_topic = message_queue.create_topic("user-events", TopicConfig::new()
    .with_partitions(3)
    .with_replication_factor(2)
    .with_retention(Duration::from_days(7)))
  
  let order_topic = message_queue.create_topic("order-events", TopicConfig::new()
    .with_partitions(5)
    .with_replication_factor(3)
    .with_retention(Duration::from_days(30)))
  
  let payment_topic = message_queue.create_topic("payment-events", TopicConfig::new()
    .with_partitions(2)
    .with_replication_factor(2)
    .with_retention(Duration::from_days(14)))
  
  // Test message publishing
  let user_event = Event::new()
    .with_type("UserCreated")
    .with_source("user-service")
    .with_data({"user_id": "123", "email": "user@example.com", "name": "John Doe"})
    .with_timestamp(DateTime::now())
    .with_id(generate_uuid())
  
  let publish_result = message_queue.publish("user-events", user_event)
  assert_true(publish_result.is_success())
  
  // Test batch publishing
  let batch_events = []
  for i in 1..=100 {
    let order_event = Event::new()
      .with_type("OrderCreated")
      .with_source("order-service")
      .with_data({
        "order_id": format!("{}", i),
        "user_id": format!("{}", i % 10 + 1),
        "amount": format!("{}", i * 10.0),
        "currency": "USD"
      })
      .with_timestamp(DateTime::now())
      .with_id(generate_uuid())
    
    batch_events.push(order_event)
  }
  
  let batch_publish_result = message_queue.publish_batch("order-events", batch_events)
  assert_true(batch_publish_result.is_success())
  
  // Test message subscription
  let subscription = message_queue.subscribe("user-events", SubscriptionConfig::new()
    .with_consumer_group("user-analytics")
    .with_start_position(StartPosition::Earliest)
    .with_auto_commit(true))
  
  // Consume messages
  let consume_result = subscription.consume(Duration::from_seconds(5))
  assert_true(consume_result.is_success())
  
  let messages = consume_result.unwrap()
  assert_true(messages.length() > 0)
  
  let user_message = messages[0]
  assert_eq(user_message.event.type, "UserCreated")
  assert_eq(user_message.event.source, "user-service")
  assert_eq(user_message.event.data.get("user_id"), "123")
  
  // Test message filtering
  let filter_subscription = message_queue.subscribe("order-events", SubscriptionConfig::new()
    .with_consumer_group("order-filtering")
    .with_filter(|event| {
      event.type == "OrderCreated" && 
      event.data.get("amount").to_float() > 500.0
    }))
  
  let filter_consume_result = filter_subscription.consume(Duration::from_seconds(5))
  assert_true(filter_consume_result.is_success())
  
  let filtered_messages = filter_consume_result.unwrap()
  
  for message in filtered_messages {
    assert_eq(message.event.type, "OrderCreated")
    assert_true(message.event.data.get("amount").to_float() > 500.0)
  }
  
  // Test message transformation
  let transformer = MessageTransformer::new()
  
  transformer.add_transformation("order-events", |event| {
    if event.type == "OrderCreated" {
      let transformed_data = event.data.clone()
      transformed_data["status"] = "pending"
      transformed_data["created_at"] = event.timestamp.to_string()
      
      Event::new()
        .with_type("OrderProcessed")
        .with_source("order-processor")
        .with_data(transformed_data)
        .with_timestamp(DateTime::now())
        .with_id(generate_uuid())
        .with_correlation_id(event.id)
    } else {
      event
    }
  })
  
  let transformed_subscription = message_queue.subscribe_with_transformer(
    "order-events", 
    SubscriptionConfig::new().with_consumer_group("order-transforming"),
    transformer
  )
  
  let transform_consume_result = transformed_subscription.consume(Duration::from_seconds(5))
  assert_true(transform_consume_result.is_success())
  
  let transformed_messages = transform_consume_result.unwrap()
  
  for message in transformed_messages {
    assert_eq(message.event.type, "OrderProcessed")
    assert_eq(message.event.source, "order-processor")
    assert_eq(message.event.data.get("status"), "pending")
  }
  
  // Test dead letter queue
  let dlq_subscription = message_queue.subscribe_with_dlq(
    "payment-events",
    SubscriptionConfig::new().with_consumer_group("payment-processing"),
    DeadLetterQueueConfig::new()
      .with_topic("payment-events-dlq")
      .with_max_retries(3)
      .with_retry_delay(Duration::from_seconds(5))
  )
  
  // Simulate message processing failure
  let failing_processor = MessageProcessor::new()
  failing_processor.set_processor(|message| {
    if message.event.data.get("amount").to_float() > 1000.0 {
      ProcessingResult::Failure("Amount too high")
    } else {
      ProcessingResult::Success
    }
  })
  
  let processing_result = dlq_subscription.process_with_processor(failing_processor, Duration::from_seconds(10))
  assert_true(processing_result.processed_count > 0)
  assert_true(processing_result.failed_count > 0)
  
  // Check dead letter queue
  let dlq_messages = message_queue.get_dlq_messages("payment-events-dlq")
  assert_true(dlq_messages.length() > 0)
  
  for dlq_message in dlq_messages {
    assert_true(dlq_message.event.data.get("amount").to_float() > 1000.0)
    assert_true(dlq_message.retry_count >= 3)
  }
  
  // Test message queue monitoring
  let queue_monitor = message_queue.get_monitor()
  
  let topic_metrics = queue_monitor.get_topic_metrics("order-events")
  assert_true(topic_metrics.message_count > 0)
  assert_true(topic_metrics.producer_count > 0)
  assert_true(topic_metrics.consumer_count > 0)
  assert_true(topic_metrics.throughput > 0)
  
  let consumer_metrics = queue_monitor.get_consumer_metrics("order-analytics")
  assert_true(consumer_metrics.messages_consumed > 0)
  assert_true(consumer_metrics.lag >= 0)
  assert_true(consumer_metrics.processing_rate > 0)
  
  // Test event sourcing
  let event_store = EventStore::new()
  
  // Store events
  for event in batch_events {
    let store_result = event_store.store_event("order-123", event)
    assert_true(store_result.is_success())
  }
  
  // Retrieve events
  let retrieved_events = event_store.get_events("order-123")
  assert_eq(retrieved_events.length(), 100)
  
  // Create snapshot
  let snapshot = OrderSnapshot::new()
    .with_order_id("order-123")
    .with_status("completed")
    .with_total_amount(1000.0)
    .with_items(10)
    .with_created_at(DateTime::now())
    .with_updated_at(DateTime::now())
  
  let snapshot_result = event_store.create_snapshot("order-123", snapshot)
  assert_true(snapshot_result.is_success())
  
  // Test event replay
  let replay_result = event_store.replay_events("order-123", |event| {
    println!("Replaying event: {}", event.type)
  })
  assert_true(replay_result.is_success())
}

// Test 5: API Gateway and Edge Services
test "api gateway and edge services" {
  let api_gateway = APIGateway::new()
  
  // Configure upstream services
  api_gateway.add_upstream("user-service", UpstreamConfig::new()
    .with_hosts(["localhost:8080", "localhost:8081"])
    .with_protocol("http")
    .with_load_balancing(LoadBalancingStrategy::RoundRobin)
    .with_health_check("/health")
    .with_timeout(Duration::from_seconds(30)))
  
  api_gateway.add_upstream("order-service", UpstreamConfig::new()
    .with_hosts(["localhost:8082"])
    .with_protocol("http")
    .with_load_balancing(LoadBalancingStrategy::LeastConnections)
    .with_health_check("/health")
    .with_timeout(Duration::from_seconds(30)))
  
  api_gateway.add_upstream("payment-service", UpstreamConfig::new()
    .with_hosts(["localhost:8083", "localhost:8084"])
    .with_protocol("http")
    .with_load_balancing(LoadBalancingStrategy::Weighted)
    .with_weights([1, 2]) // Second host has double the weight
    .with_health_check("/health")
    .with_timeout(Duration::from_seconds(30)))
  
  // Define routes
  api_gateway.add_route(RouteConfig::new()
    .with_path("/api/users/*")
    .with_method("GET,POST,PUT,DELETE")
    .with_upstream("user-service")
    .with_rewrite("^/api/users/(.*)", "/$1")
    .with_rate_limit(RateLimit::new()
      .with_requests_per_minute(100)
      .with_burst_size(20))
    .with_authentication(true)
    .with_authorization("users:read,users:write"))
  
  api_gateway.add_route(RouteConfig::new()
    .with_path("/api/orders/*")
    .with_method("GET,POST")
    .with_upstream("order-service")
    .with_rewrite("^/api/orders/(.*)", "/$1")
    .with_rate_limit(RateLimit::new()
      .with_requests_per_minute(200)
      .with_burst_size(50))
    .with_authentication(true)
    .with_authorization("orders:read,orders:write"))
  
  api_gateway.add_route(RouteConfig::new()
    .with_path("/api/payments/*")
    .with_method("POST")
    .with_upstream("payment-service")
    .with_rewrite("^/api/payments/(.*)", "/$1")
    .with_rate_limit(RateLimit::new()
      .with_requests_per_minute(50)
      .with_burst_size(10))
    .with_authentication(true)
    .with_authorization("payments:process")
    .with_request_validation(true)
    .with_response_validation(true))
  
  // Test request routing
  let user_request = APIRequest::new()
    .with_method("GET")
    .with_path("/api/users/123")
    .with_headers({"Authorization": "Bearer valid_token"})
    .with_query_params({"include": "orders"})
  
  let user_response = api_gateway.route_request(user_request)
  assert_true(user_response.is_success())
  
  let user_data = user_response.unwrap()
  assert_eq(user_data.status_code, 200)
  assert_true(user_data.body.length() > 0)
  
  // Test request transformation
  let transformer = RequestTransformer::new()
  
  transformer.add_transformer("/api/orders/*", |request| {
    // Add user ID from JWT token to request headers
    let mut transformed = request.clone()
    transformed.headers.insert("X-User-ID", "123")
    transformed
  })
  
  api_gateway.set_request_transformer(transformer)
  
  let order_request = APIRequest::new()
    .with_method("POST")
    .with_path("/api/orders")
    .with_headers({"Authorization": "Bearer valid_token"})
    .with_body("{\"product_id\": \"prod-123\", \"quantity\": 2}")
  
  let order_response = api_gateway.route_request(order_request)
  assert_true(order_response.is_success())
  
  // Test response transformation
  let response_transformer = ResponseTransformer::new()
  
  response_transformer.add_transformer("/api/users/*", |response| {
    // Remove sensitive fields from user response
    let mut transformed = response.clone()
    if let Some(body) = &mut transformed.body {
      // Remove password field from JSON
      *body = body.replace("\"password\":\"hidden\"", "\"password\":\"[REDACTED]\"")
    }
    transformed
  })
  
  api_gateway.set_response_transformer(response_transformer)
  
  // Test rate limiting
  let rate_limiter = api_gateway.get_rate_limiter()
  
  // Send requests rapidly to trigger rate limit
  let mut rate_limit_triggered = false
  for i in 1..=25 { // Exceed burst size of 20
    let rapid_request = APIRequest::new()
      .with_method("GET")
      .with_path("/api/users/list")
      .with_headers({"Authorization": "Bearer valid_token", "X-Client-ID": "test-client"})
    
    let rapid_response = api_gateway.route_request(rapid_request)
    
    if rapid_response.is_error() && 
       rapid_response.unwrap_error().error_type == APIErrorType::RateLimited {
      rate_limit_triggered = true
      break
    }
  }
  
  assert_true(rate_limit_triggered)
  
  // Test authentication
  let auth_service = api_gateway.get_auth_service()
  
  // Test valid token
  let valid_request = APIRequest::new()
    .with_method("GET")
    .with_path("/api/users/123")
    .with_headers({"Authorization": "Bearer valid_token"})
  
  let valid_auth_result = auth_service.authenticate(valid_request)
  assert_true(valid_auth_result.is_success())
  
  let auth_context = valid_auth_result.unwrap()
  assert_eq(auth_context.user_id, "123")
  assert_eq(auth_context.username, "testuser")
  assert_true(auth_context.permissions.contains("users:read"))
  
  // Test invalid token
  let invalid_request = APIRequest::new()
    .with_method("GET")
    .with_path("/api/users/123")
    .with_headers({"Authorization": "Bearer invalid_token"})
  
  let invalid_auth_result = auth_service.authenticate(invalid_request)
  assert_true(invalid_auth_result.is_error())
  
  // Test authorization
  let authz_service = api_gateway.get_authz_service()
  
  // Test permitted action
  let permitted_context = auth_context.clone()
  let permitted_result = authz_service.authorize(permitted_context, "users:read")
  assert_true(permitted_result.is_success())
  
  // Test forbidden action
  let forbidden_result = authz_service.authorize(auth_context, "admin:delete")
  assert_true(forbidden_result.is_error())
  
  // Test request validation
  let validator = RequestValidator::new()
  
  validator.add_schema("/api/payments/process", JSONSchema::new()
    .with_property("amount", PropertyType::Number)
    .with_property("currency", PropertyType::String)
    .with_required_fields(["amount", "currency"]))
  
  api_gateway.set_request_validator(validator)
  
  let invalid_payment_request = APIRequest::new()
    .with_method("POST")
    .with_path("/api/payments/process")
    .with_headers({"Authorization": "Bearer valid_token"})
    .with_body("{\"amount\": 100.00}") // Missing required currency field
  
  let invalid_payment_response = api_gateway.route_request(invalid_payment_request)
  assert_true(invalid_payment_response.is_error())
  assert_eq(invalid_payment_response.unwrap_error().error_type, APIErrorType::ValidationError)
  
  // Test API gateway monitoring
  let gateway_monitor = api_gateway.get_monitor()
  
  let gateway_metrics = gateway_monitor.get_metrics()
  assert_true(gateway_metrics.total_requests > 0)
  assert_true(gateway_metrics.successful_requests > 0)
  assert_true(gateway_metrics.error_requests >= 0)
  assert_true(gateway_metrics.average_response_time > 0)
  
  let route_metrics = gateway_monitor.get_route_metrics("/api/users/*")
  assert_true(route_metrics.request_count > 0)
  assert_true(route_metrics.average_response_time > 0)
  
  // Test API gateway configuration
  let config_manager = api_gateway.get_config_manager()
  
  // Update rate limit
  config_manager.update_rate_limit("/api/users/*", RateLimit::new()
    .with_requests_per_minute(150)
    .with_burst_size(30))
  
  // Add new route
  config_manager.add_route(RouteConfig::new()
    .with_path("/api/products/*")
    .with_method("GET")
    .with_upstream("product-service")
    .with_rewrite("^/api/products/(.*)", "/$1")
    .with_rate_limit(RateLimit::new()
      .with_requests_per_minute(300)
      .with_burst_size(50)))
  
  // Verify configuration was updated
  let updated_route_metrics = gateway_monitor.get_route_metrics("/api/users/*")
  assert_true(updated_route_metrics.rate_limit.requests_per_minute == 150)
}

// Test 6: Distributed Transactions and Saga Pattern
test "distributed transactions and saga pattern" {
  let saga_manager = SagaManager::new()
  
  // Define order processing saga
  let order_saga = Saga::new("order-processing")
    .add_step("validate-user", SagaStep::new()
      .with_action(|context| {
        let user_id = context.get("user_id")
        // Validate user exists and is active
        if validate_user(user_id) {
          SagaActionResult::Success
        } else {
          SagaActionResult::Failure("User not found or inactive")
        }
      })
      .with_compensation(|context| {
        // No compensation needed for validation
        SagaActionResult::Success
      }))
    
    .add_step("reserve-inventory", SagaStep::new()
      .with_action(|context| {
        let product_id = context.get("product_id")
        let quantity = context.get("quantity")
        // Reserve inventory
        if reserve_inventory(product_id, quantity) {
          context.set("reservation_id", generate_uuid())
          SagaActionResult::Success
        } else {
          SagaActionResult::Failure("Insufficient inventory")
        }
      })
      .with_compensation(|context| {
        let reservation_id = context.get("reservation_id")
        // Release inventory reservation
        release_inventory(reservation_id)
        SagaActionResult::Success
      }))
    
    .add_step("process-payment", SagaStep::new()
      .with_action(|context| {
        let user_id = context.get("user_id")
        let amount = context.get("amount")
        // Process payment
        match process_payment(user_id, amount) {
          Ok(payment_id) => {
            context.set("payment_id", payment_id)
            SagaActionResult::Success
          }
          Err(error) => SagaActionResult::Failure(error)
        }
      })
      .with_compensation(|context| {
        let payment_id = context.get("payment_id")
        // Refund payment
        refund_payment(payment_id)
        SagaActionResult::Success
      }))
    
    .add_step("create-order", SagaStep::new()
      .with_action(|context| {
        let user_id = context.get("user_id")
        let product_id = context.get("product_id")
        let quantity = context.get("quantity")
        let payment_id = context.get("payment_id")
        // Create order
        match create_order(user_id, product_id, quantity, payment_id) {
          Ok(order_id) => {
            context.set("order_id", order_id)
            SagaActionResult::Success
          }
          Err(error) => SagaActionResult::Failure(error)
        }
      })
      .with_compensation(|context| {
        let order_id = context.get("order_id")
        // Cancel order
        cancel_order(order_id)
        SagaActionResult::Success
      }))
  
  saga_manager.register_saga(order_saga)
  
  // Test successful saga execution
  let saga_context = SagaContext::new()
    .with_data({
      "user_id": "123",
      "product_id": "prod-456",
      "quantity": "2",
      "amount": "99.99"
    })
  
  let saga_execution = saga_manager.execute("order-processing", saga_context)
  assert_true(saga_execution.is_success())
  
  let result = saga_execution.unwrap()
  assert_true(result.is_completed)
  assert_true(result.compensations_executed == 0) // No compensations needed for success
  
  // Verify order was created
  let order_id = result.context.get("order_id")
  assert_true(order_exists(order_id))
  
  // Test saga failure and compensation
  let failing_saga_context = SagaContext::new()
    .with_data({
      "user_id": "999", // Non-existent user
      "product_id": "prod-456",
      "quantity": "2",
      "amount": "99.99"
    })
  
  let failing_saga_execution = saga_manager.execute("order-processing", failing_saga_context)
  assert_true(failing_saga_execution.is_success()) // Saga execution itself succeeds
  
  let failing_result = failing_saga_execution.unwrap()
  assert_true(failing_result.is_failed)
  assert_true(failing_result.failure_step == "validate-user")
  assert_true(failing_result.compensations_executed == 0) // No compensations needed for first step failure
  
  // Test saga with compensation
  let payment_failing_context = SagaContext::new()
    .with_data({
      "user_id": "123",
      "product_id": "prod-456",
      "quantity": "2",
      "amount": "99999.99" // Amount that will fail payment
    })
  
  let payment_failing_execution = saga_manager.execute("order-processing", payment_failing_context)
  assert_true(payment_failing_execution.is_success())
  
  let payment_failing_result = payment_failing_execution.unwrap()
  assert_true(payment_failing_result.is_failed)
  assert_true(payment_failing_result.failure_step == "process-payment")
  assert_true(payment_failing_result.compensations_executed == 1) // Should execute compensation for reserve-inventory
  
  // Verify inventory was released
  let reservation_id = payment_failing_result.context.get("reservation_id")
  assert_false(inventory_is_reserved(reservation_id))
  
  // Test distributed transaction coordinator
  let transaction_coordinator = TransactionCoordinator::new()
  
  // Define transaction participants
  let user_service = TransactionParticipant::new("user-service", "localhost:8080")
  let inventory_service = TransactionParticipant::new("inventory-service", "localhost:8081")
  let payment_service = TransactionParticipant::new("payment-service", "localhost:8082")
  let order_service = TransactionParticipant::new("order-service", "localhost:8083")
  
  transaction_coordinator.add_participant(user_service)
  transaction_coordinator.add_participant(inventory_service)
  transaction_coordinator.add_participant(payment_service)
  transaction_coordinator.add_participant(order_service)
  
  // Begin distributed transaction
  let transaction = transaction_coordinator.begin_transaction()
  let transaction_id = transaction.id
  
  // Execute transaction steps
  let validate_result = transaction.execute_step("user-service", "validate", {"user_id": "123"})
  assert_true(validate_result.is_success())
  
  let reserve_result = transaction.execute_step("inventory-service", "reserve", {"product_id": "prod-456", "quantity": "2"})
  assert_true(reserve_result.is_success())
  
  let payment_result = transaction.execute_step("payment-service", "process", {"user_id": "123", "amount": "99.99"})
  assert_true(payment_result.is_success())
  
  let create_result = transaction.execute_step("order-service", "create", {"user_id": "123", "product_id": "prod-456", "quantity": "2"})
  assert_true(create_result.is_success())
  
  // Commit transaction
  let commit_result = transaction_coordinator.commit_transaction(transaction_id)
  assert_true(commit_result.is_success())
  
  // Verify all participants committed
  let transaction_status = transaction_coordinator.get_transaction_status(transaction_id)
  assert_eq(transaction_status, TransactionStatus::Committed)
  
  // Test transaction rollback
  let rollback_transaction = transaction_coordinator.begin_transaction()
  let rollback_transaction_id = rollback_transaction.id
  
  // Execute some steps
  let validate_rollback_result = rollback_transaction.execute_step("user-service", "validate", {"user_id": "123"})
  assert_true(validate_rollback_result.is_success())
  
  let reserve_rollback_result = rollback_transaction.execute_step("inventory-service", "reserve", {"product_id": "prod-789", "quantity": "5"})
  assert_true(reserve_rollback_result.is_success())
  
  // Simulate payment failure
  let payment_rollback_result = rollback_transaction.execute_step("payment-service", "process", {"user_id": "123", "amount": "99999.99"})
  assert_true(payment_rollback_result.is_error())
  
  // Rollback transaction
  let rollback_result = transaction_coordinator.rollback_transaction(rollback_transaction_id)
  assert_true(rollback_result.is_success())
  
  // Verify all participants rolled back
  let rollback_status = transaction_coordinator.get_transaction_status(rollback_transaction_id)
  assert_eq(rollback_status, TransactionStatus::RolledBack)
  
  // Test transaction timeout handling
  let timeout_transaction = transaction_coordinator.begin_transaction()
    .with_timeout(Duration::from_seconds(1)) // Very short timeout
  
  let timeout_transaction_id = timeout_transaction.id
  
  // Execute step
  let slow_result = timeout_transaction.execute_step("payment-service", "slow_process", {"amount": "100.00"})
  
  // Wait for timeout
  sleep(Duration::from_seconds(2))
  
  // Check transaction status
  let timeout_status = transaction_coordinator.get_transaction_status(timeout_transaction_id)
  assert_eq(timeout_status, TransactionStatus::TimedOut)
  
  // Test transaction monitoring
  let transaction_monitor = transaction_coordinator.get_monitor()
  
  let transaction_metrics = transaction_monitor.get_metrics()
  assert_true(transaction_metrics.total_transactions > 0)
  assert_true(transaction_metrics.successful_transactions > 0)
  assert_true(transaction_metrics.failed_transactions >= 0)
  assert_true(transaction_metrics.average_duration > 0)
  
  let active_transactions = transaction_monitor.get_active_transactions()
  assert_true(active_transactions.length() >= 0)
}

// Test 7: Resilience and Fault Tolerance
test "resilience and fault tolerance" {
  let resilience_manager = ResilienceManager::new()
  
  // Test retry mechanism
  let retry_policy = RetryPolicy::new()
    .with_max_attempts(3)
    .with_backoff(BackoffStrategy::Exponential)
    .with_initial_delay(Duration::from_millis(100))
    .with_max_delay(Duration::from_seconds(10))
    .with_multiplier(2.0)
    .with_jitter(0.1)
  
  let retry_service = FaultyService::new(0.7) // 70% failure rate
  
  let retry_result = resilience_manager.execute_with_retry(|| {
    retry_service.call()
  }, retry_policy)
  
  assert_true(retry_result.is_success()) // Should eventually succeed with retries
  
  // Test circuit breaker
  let circuit_breaker_config = CircuitBreakerConfig::new()
    .with_failure_threshold(5)
    .with_success_threshold(3)
    .with_timeout(Duration::from_seconds(30))
    .with_half_open_max_calls(3)
  
  let circuit_breaker = resilience_manager.create_circuit_breaker("test-service", circuit_breaker_config)
  
  let failing_service = FaultyService::new(1.0) // 100% failure rate
  
  // Trigger circuit breaker
  for i in 1..=6 {
    let result = circuit_breaker.call(|| {
      failing_service.call()
    })
    
    if i <= 5 {
      assert_true(result.is_error()) // First 5 should fail
    } else {
      assert_true(result.is_error()) // 6th should fail due to open circuit breaker
      assert_eq(result.unwrap_error().error_type, ServiceErrorType::CircuitBreakerOpen)
    }
  }
  
  // Verify circuit breaker state
  assert_eq(circuit_breaker.get_state(), CircuitBreakerState::Open)
  
  // Test bulkhead pattern
  let bulkhead_config = BulkheadConfig::new()
    .with_max_concurrent_calls(5)
    .with_max_wait_time(Duration::from_seconds(2))
  
  let bulkhead = resilience_manager.create_bulkhead("test-bulkhead", bulkhead_config)
  
  let slow_service = SlowService::new(Duration::from_seconds(1)) // 1 second response time
  
  // Execute concurrent calls
  let handles = []
  for i in 1..=10 {
    let handle = spawn_thread(|| {
      bulkhead.call(|| {
        slow_service.call()
      })
    })
    handles.push(handle)
  }
  
  let mut success_count = 0
  let mut rejected_count = 0
  
  for handle in handles {
    let result = handle.join()
    if result.is_success() {
      success_count = success_count + 1
    } else if result.unwrap_error().error_type == ServiceErrorType::BulkheadRejected {
      rejected_count = rejected_count + 1
    }
  }
  
  assert_true(success_count <= 5) // Should not exceed max concurrent calls
  assert_true(rejected_count > 0) // Some should be rejected
  
  // Test timeout mechanism
  let timeout_duration = Duration::from_millis(500)
  let very_slow_service = SlowService::new(Duration::from_seconds(2)) // 2 second response time
  
  let timeout_result = resilience_manager.execute_with_timeout(|| {
    very_slow_service.call()
  }, timeout_duration)
  
  assert_true(timeout_result.is_error())
  assert_eq(timeout_result.unwrap_error().error_type, ServiceErrorType::Timeout)
  
  // Test fallback mechanism
  let primary_service = FaultyService::new(0.8) // 80% failure rate
  let fallback_service = ReliableService::new()
  
  let fallback_result = resilience_manager.execute_with_fallback(|| {
    primary_service.call()
  }, || {
    fallback_service.call()
  })
  
  assert_true(fallback_result.is_success())
  
  // Test rate limiting
  let rate_limiter_config = RateLimiterConfig::new()
    .with_requests_per_second(10)
    .with_burst_size(20)
  
  let rate_limiter = resilience_manager.create_rate_limiter("test-rate-limiter", rate_limiter_config)
  
  // Send requests rapidly
  let mut rate_limited_count = 0
  for i in 1..=25 {
    let result = rate_limiter.acquire()
    if result.is_error() && result.unwrap_error().error_type == ServiceErrorType::RateLimited {
      rate_limited_count = rate_limited_count + 1
    }
  }
  
  assert_true(rate_limited_count > 0) // Some should be rate limited
  
  // Test cache-aside pattern with resilience
  let cache = Cache::with_capacity(1000)
  let cache_service = CacheService::new(cache)
  
  let resilient_cache = resilience_manager.create_resilient_cache(cache_service)
    .with_retry_policy(RetryPolicy::new().with_max_attempts(2))
    .with_circuit_breaker(CircuitBreakerConfig::new().with_failure_threshold(3))
    .with_fallback(|| "default_value")
  
  // Test cache hit
  resilient_cache.put("test_key", "test_value")
  let cache_result = resilient_cache.get("test_key")
  assert_true(cache_result.is_success())
  assert_eq(cache_result.unwrap(), "test_value")
  
  // Test cache miss with fallback
  let cache_miss_result = resilient_cache.get("non_existent_key")
  assert_true(cache_miss_result.is_success())
  assert_eq(cache_miss_result.unwrap(), "default_value")
  
  // Test resilience patterns combination
  let combined_service = FaultyService::new(0.5) // 50% failure rate
  
  let combined_result = resilience_manager.execute_with_combined_strategies(|| {
    combined_service.call()
  }, CombinedStrategy::new()
    .with_retry(RetryPolicy::new().with_max_attempts(3))
    .with_circuit_breaker(CircuitBreakerConfig::new().with_failure_threshold(5))
    .with_timeout(Duration::from_seconds(2))
    .with_fallback(|| "fallback_value"))
  
  assert_true(combined_result.is_success())
  
  // Test resilience monitoring
  let resilience_monitor = resilience_manager.get_monitor()
  
  let circuit_breaker_metrics = resilience_monitor.get_circuit_breaker_metrics("test-service")
  assert_true(circuit_breaker_metrics.failure_count > 0)
  assert_eq(circuit_breaker_metrics.state, CircuitBreakerState::Open)
  
  let bulkhead_metrics = resilience_monitor.get_bulkhead_metrics("test-bulkhead")
  assert_true(bulkhead_metrics.active_calls >= 0)
  assert_true(bulkhead_metrics.rejected_calls > 0)
  
  let rate_limiter_metrics = resilience_monitor.get_rate_limiter_metrics("test-rate-limiter")
  assert_true(rate_limiter_metrics.permits_acquired > 0)
  assert_true(rate_limiter_metrics.permits_denied > 0)
  
  // Test chaos engineering
  let chaos_engineer = ChaosEngineer::new()
  
  // Inject latency fault
  chaos_engineer.inject_latency_fault("test-service", Duration::from_millis(500), 0.5) // 50% of calls
  
  let latency_result = resilience_manager.execute_with_retry(|| {
    // This call should be affected by chaos
    call_service("test-service")
  }, RetryPolicy::new().with_max_attempts(3))
  
  // Should still succeed despite chaos
  assert_true(latency_result.is_success())
  
  // Inject failure fault
  chaos_engineer.inject_failure_fault("test-service", 0.3) // 30% of calls
  
  let failure_result = resilience_manager.execute_with_retry(|| {
    call_service("test-service")
  }, RetryPolicy::new().with_max_attempts(5))
  
  // Should still succeed with retries
  assert_true(failure_result.is_success())
  
  // Remove chaos faults
  chaos_engineer.remove_all_faults("test-service")
}

// Test 8: Service Mesh Security
test "service mesh security" {
  let security_manager = SecurityManager::new()
  
  // Test mTLS configuration
  let mtls_config = MTLSConfig::new()
    .with_ca_certificate_path("/path/to/ca.crt")
    .with_server_certificate_path("/path/to/server.crt")
    .with_server_key_path("/path/to/server.key")
    .with_client_certificate_path("/path/to/client.crt")
    .with_client_key_path("/path/to/client.key")
    .with_verify_peer(true)
    .with_verify_hostname(true)
  
  security_manager.enable_mutual_tls(mtls_config)
  
  // Test certificate generation
  let cert_generator = CertificateGenerator::new()
  
  let service_cert = cert_generator.generate_service_certificate("user-service", ["localhost", "user-service.local"])
  assert_true(service_cert.is_valid())
  
  let client_cert = cert_generator.generate_client_certificate("user-service-client")
  assert_true(client_cert.is_valid())
  
  // Test certificate validation
  let cert_validator = CertificateValidator::new()
  cert_validator.add_trusted_ca("/path/to/ca.crt")
  
  let validation_result = cert_validator.validate(service_cert)
  assert_true(validation_result.is_valid)
  
  // Test JWT authentication
  let jwt_auth = JWTAuth::new()
    .with_secret("my-secret-key")
    .with_algorithm("HS256")
    .with_issuer("azimuth-auth")
    .with_audience("azimuth-services")
  
  // Generate JWT token
  let claims = JWTClaims::new()
    .with_subject("user-123")
    .with_name("John Doe")
    .with_email("john@example.com")
    .with_roles(["user", "admin"])
    .with_expires_at(DateTime::now() + Duration::from_hours(1))
  
  let token = jwt_auth.generate_token(claims)
  assert_true(token.length() > 0)
  
  // Validate JWT token
  let validation_result = jwt_auth.validate_token(token)
  assert_true(validation_result.is_valid)
  
  let validated_claims = validation_result.claims
  assert_eq(validated_claims.subject, "user-123")
  assert_eq(validated_claims.name, "John Doe")
  assert_eq(validated_claims.email, "john@example.com")
  assert_true(validated_claims.roles.contains("user"))
  assert_true(validated_claims.roles.contains("admin"))
  
  // Test token refresh
  let refresh_token = jwt_auth.generate_refresh_token("user-123")
  assert_true(refresh_token.length() > 0)
  
  let refreshed_token = jwt_auth.refresh_token(refresh_token)
  assert_true(refreshed_token.length() > 0)
  assert_ne(refreshed_token, token) // Should be different
  
  // Test API key authentication
  let api_key_auth = APIKeyAuth::new()
  
  // Generate API key
  let api_key = api_key_auth.generate_key("user-service", ["read", "write"])
  assert_true(api_key.key.length() > 0)
  assert_true(api_key.key_id.length() > 0)
  
  // Validate API key
  let api_key_validation = api_key_auth.validate_key(api_key.key)
  assert_true(api_key_validation.is_valid)
  assert_eq(api_key_validation.service_id, "user-service")
  assert_true(api_key_validation.permissions.contains("read"))
  assert_true(api_key_validation.permissions.contains("write"))
  
  // Test OAuth2 authentication
  let oauth2_auth = OAuth2Auth::new()
    .with_client_id("azimuth-client")
    .with_client_secret("client-secret")
    .with_auth_url("https://auth.example.com/oauth/authorize")
    .with_token_url("https://auth.example.com/oauth/token")
    .with_redirect_uri("https://service.example.com/callback")
    .with_scopes(["read", "write"])
  
  // Generate authorization URL
  let auth_url = oauth2_auth.get_authorization_url()
  assert_true(auth_url.contains("response_type=code"))
  assert_true(auth_url.contains("client_id=azimuth-client"))
  
  // Exchange authorization code for access token
  let token_result = oauth2_auth.exchange_code_for_token("authorization_code")
  assert_true(token_result.is_success())
  
  let oauth_token = token_result.unwrap()
  assert_true(oauth_token.access_token.length() > 0)
  assert_true(oauth_token.token_type == "Bearer")
  assert_true(oauth_token.expires_in > 0)
  
  // Test role-based access control (RBAC)
  let rbac = RBAC::new()
  
  // Define roles
  rbac.add_role(Role::new("user")
    .with_permissions(["profile:read", "profile:write", "orders:read"]))
  
  rbac.add_role(Role::new("admin")
    .with_permissions(["profile:read", "profile:write", "orders:read", "orders:write", "users:read", "users:write"]))
  
  rbac.add_role(Role::new("readonly")
    .with_permissions(["profile:read", "orders:read"]))
  
  // Assign roles to users
  rbac.assign_role("user-123", "user")
  rbac.assign_role("user-456", "admin")
  rbac.assign_role("user-789", "readonly")
  
  // Test permission checks
  assert_true(rbac.has_permission("user-123", "profile:read"))
  assert_true(rbac.has_permission("user-123", "orders:read"))
  assert_false(rbac.has_permission("user-123", "users:read")) // User doesn't have this permission
  
  assert_true(rbac.has_permission("user-456", "users:write")) // Admin has all permissions
  assert_true(rbac.has_permission("user-789", "profile:read"))
  assert_false(rbac.has_permission("user-789", "profile:write")) // Readonly can't write
  
  // Test attribute-based access control (ABAC)
  let abac = ABAC::new()
  
  // Define policies
  abac.add_policy(Policy::new("order-read-policy")
    .with_target("order:read")
    .with_condition(|context| {
      // User can read orders if they own the order or are admin
      let user_id = context.get("user_id");
      let order_owner = context.get("order_owner");
      let user_roles = context.get("user_roles");
      
      user_id == order_owner || user_roles.contains("admin")
    }))
  
  abac.add_policy(Policy::new("payment-process-policy")
    .with_target("payment:process")
    .with_condition(|context| {
      // User can process payments if they have payment:process permission
      // and payment amount is less than their daily limit
      let user_permissions = context.get("user_permissions");
      let payment_amount = context.get("payment_amount").to_float();
      let daily_limit = context.get("daily_limit").to_float();
      
      user_permissions.contains("payment:process") && payment_amount <= daily_limit
    }))
  
  // Test ABAC evaluation
  let order_context = ABACContext::new()
    .with_data({
      "user_id": "user-123",
      "order_owner": "user-123",
      "user_roles": ["user"]
    })
  
  let order_permission = abac.evaluate("order:read", order_context)
  assert_true(order_permission.is_allowed) // User owns the order
  
  let admin_order_context = ABACContext::new()
    .with_data({
      "user_id": "user-456",
      "order_owner": "user-123",
      "user_roles": ["admin"]
    })
  
  let admin_order_permission = abac.evaluate("order:read", admin_order_context)
  assert_true(admin_order_permission.is_allowed) // Admin can read any order
  
  let payment_context = ABACContext::new()
    .with_data({
      "user_permissions": ["payment:process"],
      "payment_amount": "500.00",
      "daily_limit": "1000.00"
    })
  
  let payment_permission = abac.evaluate("payment:process", payment_context)
  assert_true(payment_permission.is_allowed) // Within daily limit
  
  let high_payment_context = ABACContext::new()
    .with_data({
      "user_permissions": ["payment:process"],
      "payment_amount": "1500.00",
      "daily_limit": "1000.00"
    })
  
  let high_payment_permission = abac.evaluate("payment:process", high_payment_context)
  assert_false(high_payment_permission.is_allowed) // Exceeds daily limit
  
  // Test service-to-service authentication
  let service_auth = ServiceAuth::new()
  
  // Register services
  service_auth.register_service("user-service", "user-service-secret")
  service_auth.register_service("order-service", "order-service-secret")
  service_auth.register_service("payment-service", "payment-service-secret")
  
  // Generate service token
  let service_token = service_auth.generate_token("user-service")
  assert_true(service_token.length() > 0)
  
  // Validate service token
  let service_validation = service_auth.validate_token(service_token)
  assert_true(service_validation.is_valid)
  assert_eq(service_validation.service_id, "user-service")
  
  // Test token exchange between services
  let exchanged_token = service_auth.exchange_token(service_token, "order-service")
  assert_true(exchanged_token.length() > 0)
  
  let exchanged_validation = service_auth.validate_token(exchanged_token)
  assert_true(exchanged_validation.is_valid)
  assert_eq(exchanged_validation.service_id, "order-service")
  
  // Test security monitoring
  let security_monitor = security_manager.get_monitor()
  
  let auth_metrics = security_monitor.get_authentication_metrics()
  assert_true(auth_metrics.total_attempts > 0)
  assert_true(auth_metrics.successful_attempts > 0)
  assert_true(auth_metrics.failed_attempts >= 0)
  
  let access_metrics = security_monitor.get_access_metrics()
  assert_true(access_metrics.total_checks > 0)
  assert_true(access_metrics.allowed_checks > 0)
  assert_true(access_metrics.denied_checks >= 0)
  
  // Test security audit logging
  let audit_logger = security_manager.get_audit_logger()
  
  // Log security events
  audit_logger.log_authentication("user-123", "success", "jwt")
  audit_logger.log_access("user-123", "order:read", "allowed", "order-123")
  audit_logger.log_access("user-123", "users:write", "denied", "insufficient_permissions")
  
  // Retrieve audit logs
  let audit_logs = audit_logger.get_logs()
  assert_true(audit_logs.length() >= 3)
  
  let auth_log = audit_logs.find(|log| log.event_type == "authentication")
  assert_true(auth_log.is_some())
  assert_eq(auth_log.unwrap().user_id, "user-123")
  assert_eq(auth_log.unwrap().result, "success")
  
  let access_log = audit_logs.find(|log| log.event_type == "access" && log.result == "denied")
  assert_true(access_log.is_some())
  assert_eq(access_log.unwrap().resource, "users:write")
}

// Test 9: Cross-Service Observability
test "cross service observability" {
  let observability_manager = ObservabilityManager::new()
  
  // Test distributed tracing
  let tracer = observability_manager.get_tracer()
  
  // Create root span
  let root_span = tracer.start_span("user-order-flow")
    .with_tag("user_id", "123")
    .with_tag("service", "order-service")
  
  // Create child spans
  let user_validation_span = tracer.start_span_with_parent("user-validation", root_span)
    .with_tag("user_id", "123")
    .with_tag("validation_result", "success")
  
  sleep(Duration::from_millis(50)) // Simulate work
  tracer.finish_span(user_validation_span)
  
  let inventory_check_span = tracer.start_span_with_parent("inventory-check", root_span)
    .with_tag("product_id", "prod-456")
    .with_tag("quantity", "2")
    .with_tag("availability", "in_stock")
  
  sleep(Duration::from_millis(100)) // Simulate work
  tracer.finish_span(inventory_check_span)
  
  let payment_processing_span = tracer.start_span_with_parent("payment-processing", root_span)
    .with_tag("amount", "99.99")
    .with_tag("currency", "USD")
    .with_tag("payment_method", "credit_card")
  
  sleep(Duration::from_millis(200)) // Simulate work
  tracer.finish_span(payment_processing_span)
  
  tracer.finish_span(root_span)
  
  // Get trace
  let trace = tracer.get_trace(root_span.trace_id)
  assert_true(trace.is_some())
  
  let trace_data = trace.unwrap()
  assert_eq(trace_data.trace_id, root_span.trace_id)
  assert_eq(trace_data.spans.length(), 4) // Root + 3 child spans
  
  // Verify span relationships
  let root_span_data = trace_data.spans.find(|span| span.span_id == root_span.span_id)
  assert_true(root_span_data.is_some())
  assert_eq(root_span_data.unwrap().parent_span_id, "") // Root span has no parent
  
  let child_spans = trace_data.spans.filter(|span| span.parent_span_id == root_span.span_id)
  assert_eq(child_spans.length(), 3)
  
  // Test metrics collection
  let metrics_collector = observability_manager.get_metrics_collector()
  
  // Record metrics
  metrics_collector.record_counter("order_requests", 1, {"service": "order-service", "status": "success"})
  metrics_collector.record_counter("order_requests", 1, {"service": "order-service", "status": "error"})
  metrics_collector.record_counter("order_requests", 1, {"service": "order-service", "status": "success"})
  
  metrics_collector.record_histogram("order_processing_time", 150.5, {"service": "order-service"})
  metrics_collector.record_histogram("order_processing_time", 200.2, {"service": "order-service"})
  metrics_collector.record_histogram("order_processing_time", 120.8, {"service": "order-service"})
  
  metrics_collector.record_gauge("active_connections", 25, {"service": "order-service"})
  metrics_collector.record_gauge("active_connections", 30, {"service": "order-service"})
  metrics_collector.record_gauge("active_connections", 28, {"service": "order-service"})
  
  // Get metrics
  let order_requests_metrics = metrics_collector.get_counter("order_requests")
  assert_true(order_requests_metrics.length() > 0)
  
  let success_count = order_requests_metrics
    .find(|metric| metric.labels.get("status") == "success")
    .map(|metric| metric.value)
    .unwrap_or(0)
  
  assert_eq(success_count, 2)
  
  let processing_time_metrics = metrics_collector.get_histogram("order_processing_time")
  assert_true(processing_time_metrics.length() > 0)
  
  let processing_time_histogram = processing_time_metrics[0]
  assert_eq(processing_time_histogram.count, 3)
  assert_true(processing_time_histogram.sum > 0)
  assert_true(processing_time_histogram.avg > 0)
  
  // Test logging
  let logger = observability_manager.get_logger()
  
  // Log structured events
  logger.info("Order processed", {
    "order_id": "order-123",
    "user_id": "123",
    "amount": "99.99",
    "processing_time_ms": "150"
  })
  
  logger.warn("Inventory low", {
    "product_id": "prod-456",
    "current_stock": "5",
    "reorder_point": "10"
  })
  
  logger.error("Payment failed", {
    "order_id": "order-456",
    "user_id": "789",
    "error_code": "CARD_DECLINED",
    "error_message": "Insufficient funds"
  })
  
  // Get logs
  let logs = logger.get_logs()
  assert_true(logs.length() >= 3)
  
  let info_log = logs.find(|log| log.level == "info" && log.message.contains("Order processed"))
  assert_true(info_log.is_some())
  assert_eq(info_log.unwrap().fields.get("order_id"), "order-123")
  
  let error_log = logs.find(|log| log.level == "error" && log.message.contains("Payment failed"))
  assert_true(error_log.is_some())
  assert_eq(error_log.unwrap().fields.get("error_code"), "CARD_DECLINED")
  
  // Test service health checks
  let health_checker = observability_manager.get_health_checker()
  
  // Register health checks
  health_checker.register_check("database", DatabaseHealthCheck::new())
  health_checker.register_check("redis", RedisHealthCheck::new())
  health_checker.register_check("external_api", ExternalAPIHealthCheck::new("https://api.example.com/health"))
  
  // Run health checks
  let health_results = health_checker.run_checks()
  assert_eq(health_results.length(), 3)
  
  for result in health_results {
    assert_true(result.check_name.length() > 0)
    assert_true(result.response_time_ms > 0)
    assert_true(result.status == "healthy" || result.status == "unhealthy")
  }
  
  // Test correlation across services
  let correlation_manager = observability_manager.get_correlation_manager()
  
  // Start correlated flow
  let correlation_id = correlation_manager.start_correlation("user-order-flow")
    .with_data({"user_id": "123", "session_id": "sess-456"})
  
  // Propagate correlation to other services
  let user_service_context = correlation_manager.propagate_to_service(correlation_id, "user-service")
  let order_service_context = correlation_manager.propagate_to_service(correlation_id, "order-service")
  let payment_service_context = correlation_manager.propagate_to_service(correlation_id, "payment-service")
  
  // Verify correlation context
  assert_eq(user_service_context.correlation_id, correlation_id)
  assert_eq(user_service_context.service_name, "user-service")
  assert_eq(user_service_context.data.get("user_id"), "123")
  
  // Complete correlation
  correlation_manager.complete_correlation(correlation_id)
  
  // Get correlation data
  let correlation_data = correlation_manager.get_correlation(correlation_id)
  assert_true(correlation_data.is_some())
  
  let data = correlation_data.unwrap()
  assert_eq(data.correlation_id, correlation_id)
  assert_eq(data.flow_name, "user-order-flow")
  assert_true(data.service_contexts.length() >= 3)
  assert_eq(data.status, "completed")
  
  // Test cross-service alerting
  let alerting_manager = observability_manager.get_alerting_manager()
  
  // Define alert rules
  alerting_manager.add_rule(AlertRule::new("high_error_rate")
    .with_condition("error_rate > 0.1")
    .with_duration(Duration::from_minutes(5))
    .with_severity(AlertSeverity::Warning)
    .with_services(["order-service", "payment-service"]))
  
  alerting_manager.add_rule(AlertRule::new("high_response_time")
    .with_condition("avg_response_time > 1000")
    .with_duration(Duration::from_minutes(2))
    .with_severity(AlertSeverity::Critical)
    .with_services(["user-service"]))
  
  // Simulate metrics that trigger alerts
  metrics_collector.record_counter("request_count", 100, {"service": "order-service"})
  metrics_collector.record_counter("error_count", 15, {"service": "order-service"}) // 15% error rate
  
  metrics_collector.record_histogram("response_time", 1200.0, {"service": "user-service"}) // High response time
  
  // Check for alerts
  let alerts = alerting_manager.check_alerts()
  assert_true(alerts.length() >= 1)
  
  let error_rate_alert = alerts.find(|alert| alert.rule_name == "high_error_rate")
  assert_true(error_rate_alert.is_some())
  assert_eq(error_rate_alert.unwrap().severity, AlertSeverity::Warning)
  
  let response_time_alert = alerts.find(|alert| alert.rule_name == "high_response_time")
  assert_true(response_time_alert.is_some())
  assert_eq(response_time_alert.unwrap().severity, AlertSeverity::Critical)
  
  // Test observability dashboard
  let dashboard = observability_manager.get_dashboard()
  
  // Generate dashboard data
  let dashboard_data = dashboard.generate_data()
  assert_true(dashboard_data.contains("service_metrics"))
  assert_true(dashboard_data.contains("traces"))
  assert_true(dashboard_data.contains("logs"))
  assert_true(dashboard_data.contains("health_checks"))
  assert_true(dashboard_data.contains("alerts"))
  
  // Test distributed context propagation
  let context_propagator = observability_manager.get_context_propagator()
  
  // Create context
  let context = DistributedContext::new()
    .with_trace_id("trace-123")
    .with_span_id("span-456")
    .with_baggage_items({"user_id": "123", "session_id": "sess-789"})
  
  // Inject context into carrier
  let mut headers = {}
  context_propagator.inject(context, &mut headers)
  
  assert_true(headers.contains("traceparent"))
  assert_true(headers.contains("baggage"))
  
  // Extract context from carrier
  let extracted_context = context_propagator.extract(&headers)
  assert_eq(extracted_context.trace_id, "trace-123")
  assert_eq(extracted_context.span_id, "span-456")
  assert_eq(extracted_context.baggage_items.get("user_id"), "123")
}

// Test 10: Service Mesh Advanced Features
test "service mesh advanced features" {
  let service_mesh = ServiceMesh::new()
  
  // Test traffic splitting and canary deployments
  let traffic_splitter = service_mesh.get_traffic_splitter()
  
  // Configure traffic split
  traffic_splitter.configure_split("user-service", TrafficSplitConfig::new()
    .with_version("v1", 80) // 80% to v1
    .with_version("v2", 20) // 20% to v2 (canary)
    .with_headers({"User-Agent": "azimuth-client"})) // Only for specific client
  
  // Send requests and verify distribution
  let mut v1_count = 0
  let mut v2_count = 0
  
  for i in 1..=100 {
    let request = ServiceRequest::new()
      .with_method("GET")
      .with_path("/users/123")
      .with_headers({"User-Agent": "azimuth-client"})
    
    let response = service_mesh.send_request("client-service", "user-service", request)
    
    if response.is_success() {
      let version = response.unwrap().headers.get("X-Service-Version")
      if version == "v1" {
        v1_count = v1_count + 1
      } else if version == "v2" {
        v2_count = v2_count + 1
      }
    }
  }
  
  // Verify traffic distribution (approximately)
  let v1_percentage = v1_count as Float / (v1_count + v2_count) as Float
  assert_true(v1_percentage > 0.7 && v1_percentage < 0.9) // Around 80%
  
  // Test fault injection
  let fault_injector = service_mesh.get_fault_injector()
  
  // Configure fault injection
  fault_injector.configure_fault("payment-service", FaultConfig::new()
    .with_delay(Duration::from_millis(500), 0.2) // 20% of requests delayed by 500ms
    .with_error(503, "Service Unavailable", 0.1) // 10% of requests return 503
    .with_rate_limit(100) // Limit to 100 requests per minute
    .with_headers({"X-Test-Mode": "enabled"})) // Only for requests with this header
  
  // Test fault injection
  let mut delayed_count = 0
  let mut error_count = 0
  let mut normal_count = 0
  
  for i in 1..=50 {
    let request = ServiceRequest::new()
      .with_method("POST")
      .with_path("/payments/process")
      .with_headers({"X-Test-Mode": "enabled"})
      .with_body("{\"amount\": 100.00}")
    
    let start_time = get_current_timestamp()
    let response = service_mesh.send_request("order-service", "payment-service", request)
    let end_time = get_current_timestamp()
    let response_time = end_time - start_time
    
    if response.is_error() {
      error_count = error_count + 1
    } else if response_time.to_millis() > 400 {
      delayed_count = delayed_count + 1
    } else {
      normal_count = normal_count + 1
    }
  }
  
  // Verify fault injection effects
  assert_true(error_count > 0) // Some should receive errors
  assert_true(delayed_count > 0) // Some should be delayed
  assert_true(normal_count > 0) // Some should be normal
  
  // Test request/response transformation
  let transformer = service_mesh.get_transformer()
  
  // Configure request transformation
  transformer.add_request_transformer("order-service", "user-service", |request| {
    let mut transformed = request.clone()
    
    // Add authentication header
    transformed.headers.insert("Authorization", "Bearer injected-token")
    
    // Add request ID
    transformed.headers.insert("X-Request-ID", generate_uuid())
    
    // Transform path
    if request.path.starts_with("/api/") {
      transformed.path = request.path.replace("/api/", "/internal/")
    }
    
    transformed
  })
  
  // Configure response transformation
  transformer.add_response_transformer("user-service", "order-service", |response| {
    let mut transformed = response.clone()
    
    // Add response header
    transformed.headers.insert("X-Transformed-By", "service-mesh")
    
    // Remove sensitive headers
    transformed.headers.remove("X-Internal-Debug")
    
    // Transform response body
    if let Some(body) = &mut transformed.body {
      *body = body.replace("\"password\":\"[REDACTED]\"", "\"password\":\"*****\"")
    }
    
    transformed
  })
  
  // Test transformation
  let transform_request = ServiceRequest::new()
    .with_method("GET")
    .with_path("/api/users/123")
    .with_headers({"Content-Type": "application/json"})
  
  let transform_response = service_mesh.send_request("order-service", "user-service", transform_request)
  assert_true(transform_response.is_success())
  
  let response_data = transform_response.unwrap()
  assert_true(response_data.headers.contains("Authorization"))
  assert_true(response_data.headers.contains("X-Request-ID"))
  assert_true(response_data.headers.contains("X-Transformed-By"))
  assert_false(response_data.headers.contains("X-Internal-Debug"))
  
  // Test retry with exponential backoff
  let retry_policy = RetryPolicy::new()
    .with_max_attempts(5)
    .with_backoff(BackoffStrategy::Exponential)
    .with_initial_delay(Duration::from_millis(100))
    .with_max_delay(Duration::from_seconds(10))
    .with_multiplier(2.0)
    .with_jitter(0.1)
    .with_retryable_errors([502, 503, 504]) // Retry on server errors
  
  service_mesh.set_retry_policy("unreliable-service", retry_policy)
  
  // Test retry
  let retry_request = ServiceRequest::new()
    .with_method("GET")
    .with_path("/data")
    .with_headers({"X-Retry-Test": "enabled"})
  
  let retry_response = service_mesh.send_request("client-service", "unreliable-service", retry_request)
  assert_true(retry_response.is_success()) // Should eventually succeed
  
  // Test timeout and deadline propagation
  let timeout_config = TimeoutConfig::new()
    .with_request_timeout(Duration::from_seconds(5))
    .with_global_deadline(Duration::from_seconds(30))
  
  service_mesh.set_timeout_config("slow-service", timeout_config)
  
  // Test timeout
  let timeout_request = ServiceRequest::new()
    .with_method("GET")
    .with_path("/slow-data")
    .with_headers({"X-Slow-Test": "enabled"})
  
  let timeout_response = service_mesh.send_request("client-service", "slow-service", timeout_request)
  
  // Should either succeed or timeout
  if timeout_response.is_error() {
    assert_eq(timeout_response.unwrap_error().error_type, ServiceErrorType::Timeout)
  }
  
  // Test service mesh telemetry
  let telemetry = service_mesh.get_telemetry()
  
  // Get telemetry data
  let telemetry_data = telemetry.get_data()
  assert_true(telemetry_data.contains("request_metrics"))
  assert_true(telemetry_data.contains("latency_histograms"))
  assert_true(telemetry_data.contains("error_rates"))
  assert_true(telemetry_data.contains("circuit_breaker_states"))
  assert_true(telemetry_data.contains("retry_statistics"))
  
  // Test service mesh configuration management
  let config_manager = service_mesh.get_config_manager()
  
  // Update configuration
  config_manager.update_service_config("user-service", ServiceConfig::new()
    .with_timeout(Duration::from_seconds(15))
    .with_retry_policy(RetryPolicy::new().with_max_attempts(3))
    .with_circuit_breaker(CircuitBreakerConfig::new().with_failure_threshold(10)))
  
  // Verify configuration was updated
  let updated_config = config_manager.get_service_config("user-service")
  assert_eq(updated_config.timeout, Duration::from_seconds(15))
  assert_eq(updated_config.retry_policy.max_attempts, 3)
  assert_eq(updated_config.circuit_breaker.failure_threshold, 10)
  
  // Test service mesh plugin system
  let plugin_manager = service_mesh.get_plugin_manager()
  
  // Load plugins
  plugin_manager.load_plugin("auth-plugin", AuthPlugin::new())
  plugin_manager.load_plugin("metrics-plugin", MetricsPlugin::new())
  plugin_manager.load_plugin("tracing-plugin", TracingPlugin::new())
  
  // Test plugin execution
  let plugin_request = ServiceRequest::new()
    .with_method("GET")
    .with_path("/protected-data")
    .with_headers({"Authorization": "Bearer valid-token"})
  
  let plugin_response = service_mesh.send_request_with_plugins("client-service", "data-service", plugin_request)
  assert_true(plugin_response.is_success())
  
  // Verify plugins were executed
  let auth_headers = plugin_response.unwrap().headers
  assert_true(auth_headers.contains("X-Auth-Plugin-Executed"))
  assert_true(auth_headers.contains("X-Metrics-Plugin-Executed"))
  assert_true(auth_headers.contains("X-Tracing-Plugin-Executed"))
  
  // Test service mesh debugging
  let debugger = service_mesh.get_debugger()
  
  // Enable debugging
  debugger.enable_debug_mode("user-service")
  
  // Send request with debugging
  let debug_request = ServiceRequest::new()
    .with_method("GET")
    .with_path("/users/123")
    .with_headers({"X-Debug-Mode": "enabled"})
  
  let debug_response = service_mesh.send_request("client-service", "user-service", debug_request)
  assert_true(debug_response.is_success())
  
  // Get debug information
  let debug_info = debugger.get_debug_info("user-service")
  assert_true(debug_info.contains("request_flow"))
  assert_true(debug_info.contains("transformations"))
  assert_true(debug_info.contains("timing"))
  assert_true(debug_info.contains("headers"))
}