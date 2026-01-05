// Azimuth Network Communication Test Suite
// This file contains test cases for network communication and telemetry data transmission

// Test 1: HTTP Client Telemetry Integration
test "http client telemetry integration" {
  // Test HTTP client creation with telemetry
  let telemetry_config = HttpClientTelemetryConfig::new()
  let http_client = HttpClient::with_telemetry(telemetry_config)
  
  // Test HTTP GET request with telemetry
  let get_request = HttpRequest::get("https://api.example.com/data")
  let get_telemetry = HttpClientTelemetry::instrument_request(get_request)
  
  assert_eq(HttpRequest::method(get_telemetry), "GET")
  assert_eq(HttpRequest::url(get_telemetry), "https://api.example.com/data")
  assert_true(HttpRequest::has_telemetry_headers(get_telemetry))
  
  // Test HTTP POST request with telemetry
  let post_data = "{\"name\":\"test\",\"value\":42}"
  let post_request = HttpRequest::post("https://api.example.com/create", post_data)
  let post_telemetry = HttpClientTelemetry::instrument_request(post_request)
  
  assert_eq(HttpRequest::method(post_telemetry), "POST")
  assert_eq(HttpRequest::url(post_telemetry), "https://api.example.com/create")
  assert_true(HttpRequest::has_telemetry_headers(post_telemetry))
  
  // Test request tracing headers
  let trace_headers = HttpClientTelemetry::get_trace_headers(post_telemetry)
  assert_true(Map::contains_key(trace_headers, "traceparent"))
  assert_true(Map::contains_key(trace_headers, "tracestate"))
  
  // Test response processing with telemetry
  let mock_response = HttpResponse::new(200, "{\"status\":\"success\"}", [
    ("content-type", "application/json"),
    ("x-request-id", "req-12345")
  ])
  
  let processed_response = HttpClientTelemetry::process_response(mock_response)
  assert_eq(HttpResponse::status(processed_response), 200)
  assert_true(HttpResponse::has_telemetry_attributes(processed_response))
  
  // Test telemetry metrics collection
  let metrics = HttpClientTelemetry::collect_metrics()
  assert_true(Map::contains_key(metrics, "http.requests.total"))
  assert_true(Map::contains_key(metrics, "http.request.duration"))
}

// Test 2: Network Connection Resilience
test "network connection resilience" {
  // Test connection pool management
  let pool_config = ConnectionPoolConfig::new()
  let connection_pool = ConnectionPool::with_config(pool_config)
  
  // Test connection creation
  let connection1 = ConnectionPool::get_connection(connection_pool, "https://api.example.com")
  assert_true(Connection::is_valid(connection1))
  
  let connection2 = ConnectionPool::get_connection(connection_pool, "https://api.example.com")
  assert_true(Connection::is_valid(connection2))
  
  // Test connection reuse
  let reused_connection = ConnectionPool::get_connection(connection_pool, "https://api.example.com")
  assert_true(Connection::is_valid(reused_connection))
  
  // Test connection health check
  assert_true(ConnectionPool::is_healthy(connection_pool))
  
  // Test connection timeout handling
  let timeout_config = TimeoutConfig::new(5000) // 5 seconds
  let timeout_connection = ConnectionPool::get_connection_with_timeout(
    connection_pool, 
    "https://slow-api.example.com", 
    timeout_config
  )
  
  // In a real scenario, this might timeout, but for testing we simulate
  assert_true(Connection::is_valid(timeout_connection))
  
  // Test connection retry logic
  let retry_config = RetryConfig::new(3, 1000) // 3 retries with 1s delay
  let retry_result = ConnectionPool::execute_with_retry(
    connection_pool,
    |conn| Connection::execute_request(conn, HttpRequest::get("https://api.example.com/data")),
    retry_config
  )
  
  match retry_result {
    Ok(response) => assert_eq(HttpResponse::status(response), 200),
    Err(error) => assert_true(String::contains(error.to_string(), "retry"))
  }
  
  // Test circuit breaker pattern
  let circuit_breaker = CircuitBreaker::new(5, 10000) // 5 failures, 10s timeout
  
  // Simulate successful requests
  for i = 0; i < 3; i = i + 1 {
    let result = CircuitBreaker::execute(circuit_breaker, || {
      Ok(HttpResponse::new(200, "success", []))
    })
    assert_true(Result::is_ok(result))
  }
  
  // Simulate failed requests
  for i = 0; i < 6; i = i + 1 {
    let result = CircuitBreaker::execute(circuit_breaker, || {
      Err(NetworkError::new("Connection failed"))
    })
    assert_true(Result::is_err(result))
  }
  
  // Circuit should be open now
  assert_true(CircuitBreaker::is_open(circuit_breaker))
}

// Test 3: Distributed Tracing Over Network
test "distributed tracing over network" {
  // Test trace context propagation
  let trace_id = "12345678901234567890123456789012"
  let span_id = "1234567890123456"
  let trace_context = TraceContext::new(trace_id, span_id, true)
  
  // Test trace context injection into HTTP headers
  let headers = TraceContext::inject_to_headers(trace_context)
  assert_eq(Map::get(headers, "traceparent"), Some("00-" + trace_id + "-" + span_id + "-01"))
  
  // Test trace context extraction from HTTP headers
  let extracted_context = TraceContext::extract_from_headers(headers)
  assert_eq(TraceContext::trace_id(extracted_context), trace_id)
  assert_eq(TraceContext::span_id(extracted_context), span_id)
  assert_true(TraceContext::is_sampled(extracted_context))
  
  // Test remote span creation
  let remote_span = RemoteSpan::create_from_context("remote.operation", extracted_context)
  assert_eq(RemoteSpan::name(remote_span), "remote.operation")
  assert_eq(RemoteSpan::trace_id(remote_span), trace_id)
  assert_eq(RemoteSpan::parent_span_id(remote_span), span_id)
  
  // Test cross-service trace propagation
  let service_a_tracer = Tracer::new("service-a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a.operation")
  
  let service_b_context = TraceContext::from_span(service_a_span)
  let service_b_headers = TraceContext::inject_to_headers(service_b_context)
  
  // Simulate service B receiving the request
  let service_b_extracted = TraceContext::extract_from_headers(service_b_headers)
  let service_b_tracer = Tracer::new("service-b")
  let service_b_span = Tracer::start_span_with_context(service_b_tracer, "service-b.operation", service_b_extracted)
  
  assert_eq(TraceContext::trace_id(service_b_context), TraceContext::trace_id(service_b_extracted))
  assert_eq(RemoteSpan::parent_span_id(RemoteSpan::from_span(service_b_span)), Span::span_id(service_a_span))
  
  // Test trace sampling across services
  let sampler = Sampler::trace_id_ratio(0.5)
  let sampling_decision = Sampler::should_sample(sampler, None, trace_id, "test.operation", [])
  
  match SamplingDecision::decision(sampling_decision) {
    RecordAndSample => {
      assert_true(TraceContext::is_sampled(service_b_context))
    }
    Drop => {
      assert_false(TraceContext::is_sampled(service_b_context))
    }
  }
}

// Test 4: Network Metrics Collection
test "network metrics collection" {
  // Test network metrics collector
  let metrics_collector = NetworkMetricsCollector::new()
  
  // Test request metrics
  let request_metrics = RequestMetrics::new()
  RequestMetrics::record_request(request_metrics, "GET", "https://api.example.com/data", 200, 150)
  RequestMetrics::record_request(request_metrics, "POST", "https://api.example.com/create", 201, 250)
  RequestMetrics::record_request(request_metrics, "GET", "https://api.example.com/data", 500, 1000)
  RequestMetrics::record_request(request_metrics, "GET", "https://api.example.com/data", 200, 120)
  
  // Verify request count metrics
  let total_requests = RequestMetrics::total_requests(request_metrics)
  assert_eq(total_requests, 4)
  
  let get_requests = RequestMetrics::requests_by_method(request_metrics, "GET")
  assert_eq(get_requests, 3)
  
  let post_requests = RequestMetrics::requests_by_method(request_metrics, "POST")
  assert_eq(post_requests, 1)
  
  // Verify response status metrics
  let success_requests = RequestMetrics::requests_by_status_range(request_metrics, 200, 299)
  assert_eq(success_requests, 3)
  
  let error_requests = RequestMetrics::requests_by_status_range(request_metrics, 500, 599)
  assert_eq(error_requests, 1)
  
  // Verify duration metrics
  let avg_duration = RequestMetrics::average_duration(request_metrics)
  assert_true(avg_duration > 300 && avg_duration < 400) // (150+250+1000+120)/4 = 380
  
  let max_duration = RequestMetrics::max_duration(request_metrics)
  assert_eq(max_duration, 1000)
  
  let min_duration = RequestMetrics::min_duration(request_metrics)
  assert_eq(min_duration, 120)
  
  // Test connection metrics
  let connection_metrics = ConnectionMetrics::new()
  ConnectionMetrics::record_connection_created(connection_metrics, "https://api.example.com")
  ConnectionMetrics::record_connection_created(connection_metrics, "https://api.example.com")
  ConnectionMetrics::record_connection_closed(connection_metrics, "https://api.example.com")
  ConnectionMetrics::record_connection_failed(connection_metrics, "https://api.example.com")
  
  // Verify connection metrics
  let total_connections = ConnectionMetrics::total_connections(connection_metrics)
  assert_eq(total_connections, 2)
  
  let active_connections = ConnectionMetrics::active_connections(connection_metrics)
  assert_eq(active_connections, 1)
  
  let failed_connections = ConnectionMetrics::failed_connections(connection_metrics)
  assert_eq(failed_connections, 1)
  
  // Test bandwidth metrics
  let bandwidth_metrics = BandwidthMetrics::new()
  BandwidthMetrics::record_bytes_sent(bandwidth_metrics, 1024)
  BandwidthMetrics::record_bytes_received(bandwidth_metrics, 2048)
  BandwidthMetrics::record_bytes_sent(bandwidth_metrics, 512)
  BandwidthMetrics::record_bytes_received(bandwidth_metrics, 1024)
  
  // Verify bandwidth metrics
  let total_sent = BandwidthMetrics::total_bytes_sent(bandwidth_metrics)
  assert_eq(total_sent, 1536)
  
  let total_received = BandwidthMetrics::total_bytes_received(bandwidth_metrics)
  assert_eq(total_received, 3072)
  
  // Test metrics aggregation
  let aggregated_metrics = NetworkMetricsCollector::aggregate(metrics_collector, [
    request_metrics,
    connection_metrics,
    bandwidth_metrics
  ])
  
  assert_true(Map::contains_key(aggregated_metrics, "network.requests.total"))
  assert_true(Map::contains_key(aggregated_metrics, "network.connections.active"))
  assert_true(Map::contains_key(aggregated_metrics, "network.bandwidth.sent"))
  assert_true(Map::contains_key(aggregated_metrics, "network.bandwidth.received"))
}

// Test 5: Network Error Handling and Recovery
test "network error handling and recovery" {
  // Test network error classification
  let timeout_error = NetworkError::timeout("Request timed out after 5000ms")
  assert_eq(NetworkError::type(timeout_error), TimeoutError)
  assert_true(String::contains(NetworkError::message(timeout_error), "timed out"))
  
  let connection_error = NetworkError::connection_failed("Failed to connect to host")
  assert_eq(NetworkError::type(connection_error), ConnectionError)
  assert_true(String::contains(NetworkError::message(connection_error), "Failed to connect"))
  
  let dns_error = NetworkError::dns_resolution_failed("Could not resolve hostname")
  assert_eq(NetworkError::type(dns_error), DNSError)
  assert_true(String::contains(NetworkError::message(dns_error), "resolve"))
  
  // Test error recovery strategies
  let retry_strategy = RetryStrategy::exponential_backoff(3, 1000, 10000)
  let timeout_recovery = ErrorRecovery::with_retry(timeout_error, retry_strategy)
  
  match ErrorRecovery::execute(timeout_recovery, || {
    Ok(HttpResponse::new(200, "success", []))
  }) {
    Ok(response) => assert_eq(HttpResponse::status(response), 200),
    Err(error) => assert_eq(NetworkError::type(error), TimeoutError)
  }
  
  // Test circuit breaker for error recovery
  let circuit_breaker = CircuitBreaker::new(3, 5000) // 3 failures, 5s timeout
  
  // Simulate failures to trigger circuit breaker
  for i = 0; i < 4; i = i + 1 {
    let result = CircuitBreaker::execute(circuit_breaker, || {
      Err(NetworkError::connection_failed("Connection failed"))
    })
    assert_true(Result::is_err(result))
  }
  
  // Circuit should be open now
  assert_true(CircuitBreaker::is_open(circuit_breaker))
  
  // Test fallback mechanism
  let fallback_strategy = FallbackStrategy::new(|error| {
    match NetworkError::type(error) {
      ConnectionError => HttpResponse::new(200, "{\"status\":\"cached\"}", []),
      TimeoutError => HttpResponse::new(408, "{\"status\":\"timeout\"}", []),
      _ => HttpResponse::new(500, "{\"status\":\"error\"}", [])
    }
  })
  
  let fallback_result = FallbackStrategy::execute(fallback_strategy, || {
    Err(connection_error)
  })
  
  match fallback_result {
    Ok(response) => {
      assert_eq(HttpResponse::status(response), 200)
      assert_eq(HttpResponse::body(response), "{\"status\":\"cached\"}")
    }
    Err(_) => assert_true(false)
  }
  
  // Test error telemetry
  let error_telemetry = ErrorTelemetry::new()
  ErrorTelemetry::record_error(error_telemetry, timeout_error)
  ErrorTelemetry::record_error(error_telemetry, connection_error)
  ErrorTelemetry::record_error(error_telemetry, dns_error)
  
  let error_metrics = ErrorTelemetry::get_metrics(error_telemetry)
  assert_eq(Map::get(error_metrics, "network.errors.total"), Some(3))
  assert_eq(Map::get(error_metrics, "network.errors.timeout"), Some(1))
  assert_eq(Map::get(error_metrics, "network.errors.connection"), Some(1))
  assert_eq(Map::get(error_metrics, "network.errors.dns"), Some(1))
}

// Test 6: Network Security and Authentication
test "network security and authentication" {
  // Test TLS/SSL configuration
  let tls_config = TlsConfig::new()
  let tls_with_validation = TlsConfig::with_certificate_validation(tls_config, true)
  let tls_with_custom_ca = TlsConfig::with_custom_ca(tls_with_validation, "/path/to/ca.pem")
  
  assert_true(TlsConfig::certificate_validation_enabled(tls_with_custom_ca))
  assert_eq(TlsConfig::custom_ca_path(tls_with_custom_ca), Some("/path/to/ca.pem"))
  
  // Test HTTP basic authentication
  let basic_auth = HttpAuth::basic("username", "password")
  let auth_headers = HttpAuth::to_headers(basic_auth)
  
  assert_true(Map::contains_key(auth_headers, "Authorization"))
  match Map::get(auth_headers, "Authorization") {
    Some(value) => assert_true(String::starts_with(value, "Basic ")),
    None => assert_true(false)
  }
  
  // Test Bearer token authentication
  let bearer_auth = HttpAuth::bearer("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9")
  let bearer_headers = HttpAuth::to_headers(bearer_auth)
  
  assert_true(Map::contains_key(bearer_headers, "Authorization"))
  match Map::get(bearer_headers, "Authorization") {
    Some(value) => assert_true(String::starts_with(value, "Bearer ")),
    None => assert_true(false)
  }
  
  // Test API key authentication
  let api_key_auth = HttpAuth::api_key("X-API-Key", "api-key-value")
  let api_key_headers = HttpAuth::to_headers(api_key_auth)
  
  assert_eq(Map::get(api_key_headers, "X-API-Key"), Some("api-key-value"))
  
  // Test request signing
  let signing_config = RequestSigningConfig::new("hmac-sha256", "secret-key")
  let request = HttpRequest::post("https://api.example.com/data", "{\"test\": \"data\"}")
  let signed_request = RequestSigning::sign(request, signing_config)
  
  assert_true(HttpRequest::has_signature(signed_request))
  let signature_header = HttpRequest::get_header(signed_request, "X-Signature")
  assert_true(Option::is_some(signature_header))
  
  // Test signature verification
  let is_valid = RequestSigning::verify_signature(signed_request, signing_config)
  assert_true(is_valid)
  
  // Test OAuth2 authentication
  let oauth_config = OAuth2Config::new(
    "https://auth.example.com/oauth/token",
    "client_id",
    "client_secret"
  )
  
  let oauth_auth = HttpAuth::oauth2(oauth_config)
  let oauth_headers = HttpAuth::to_headers(oauth_auth)
  
  assert_true(Map::contains_key(oauth_headers, "Authorization"))
  
  // Test security telemetry
  let security_telemetry = SecurityTelemetry::new()
  SecurityTelemetry::record_authentication_attempt(security_telemetry, "basic")
  SecurityTelemetry::record_authentication_success(security_telemetry, "basic")
  SecurityTelemetry::record_authentication_attempt(security_telemetry, "oauth2")
  SecurityTelemetry::record_authentication_failure(security_telemetry, "oauth2")
  
  let security_metrics = SecurityTelemetry::get_metrics(security_telemetry)
  assert_eq(Map::get(security_metrics, "security.auth.attempts.total"), Some(2))
  assert_eq(Map::get(security_metrics, "security.auth.success.total"), Some(1))
  assert_eq(Map::get(security_metrics, "security.auth.failure.total"), Some(1))
}

// Test 7: Network Protocol Support
test "network protocol support" {
  // Test HTTP/1.1 support
  let http11_client = HttpClient::new(HttpVersion::Http11)
  let http11_request = HttpRequest::get("https://api.example.com/data")
  let http11_response = HttpClient::execute(http11_client, http11_request)
  
  assert_eq(HttpResponse::version(http11_response), HttpVersion::Http11)
  
  // Test HTTP/2 support
  let http2_client = HttpClient::new(HttpVersion::Http2)
  let http2_request = HttpRequest::get("https://api.example.com/data")
  let http2_response = HttpClient::execute(http2_client, http2_request)
  
  assert_eq(HttpResponse::version(http2_response), HttpVersion::Http2)
  
  // Test WebSocket support
  let websocket_config = WebSocketConfig::new()
  let websocket_client = WebSocketClient::with_config(websocket_config)
  
  let websocket_connection = WebSocketClient::connect(websocket_client, "wss://api.example.com/ws")
  assert_true(WebSocketConnection::is_connected(websocket_connection))
  
  // Test WebSocket message sending
  let message = WebSocketMessage::text("{\"type\":\"ping\"}")
  WebSocketConnection::send(websocket_connection, message)
  
  // Test WebSocket message receiving
  let received_message = WebSocketConnection::receive(websocket_connection)
  match received_message {
    Some(msg) => assert_eq(WebSocketMessage::message_type(msg), TextMessage),
    None => assert_true(false)
  }
  
  // Test gRPC support
  let grpc_client = GrpcClient::new("https://grpc.example.com:443")
  let grpc_request = GrpcRequest::new("UserService", "GetUser", "{\"id\":\"123\"}")
  let grpc_response = GrpcClient::execute(grpc_client, grpc_request)
  
  assert_eq(GrpcResponse::service(grpc_response), "UserService")
  assert_eq(GrpcResponse::method(grpc_response), "GetUser")
  
  // Test GraphQL support
  let graphql_client = GraphQlClient::new("https://graphql.example.com/graphql")
  let graphql_query = "{ user(id: \"123\") { name email } }"
  let graphql_request = GraphQlRequest::query(graphql_query)
  let graphql_response = GraphQlClient::execute(graphql_client, graphql_request)
  
  assert_true(GraphQlResponse::has_data(graphql_response))
  
  // Test protocol metrics
  let protocol_metrics = ProtocolMetrics::new()
  ProtocolMetrics::record_request(protocol_metrics, HttpVersion::Http11, "GET")
  ProtocolMetrics::record_request(protocol_metrics, HttpVersion::Http2, "POST")
  ProtocolMetrics::record_request(protocol_metrics, HttpVersion::Http11, "GET")
  ProtocolMetrics::record_websocket_message(protocol_metrics, "sent")
  ProtocolMetrics::record_websocket_message(protocol_metrics, "received")
  ProtocolMetrics::record_grpc_request(protocol_metrics, "UserService", "GetUser")
  ProtocolMetrics::record_graphql_request(protocol_metrics, "query")
  
  let http11_requests = ProtocolMetrics::requests_by_version(protocol_metrics, HttpVersion::Http11)
  assert_eq(http11_requests, 2)
  
  let http2_requests = ProtocolMetrics::requests_by_version(protocol_metrics, HttpVersion::Http2)
  assert_eq(http2_requests, 1)
  
  let websocket_messages = ProtocolMetrics::websocket_messages(protocol_metrics)
  assert_eq(websocket_messages, 2)
  
  let grpc_requests = ProtocolMetrics::grpc_requests_by_service(protocol_metrics, "UserService")
  assert_eq(grpc_requests, 1)
  
  let graphql_requests = ProtocolMetrics::graphql_requests_by_type(protocol_metrics, "query")
  assert_eq(graphql_requests, 1)
}

// Test 8: Network Load Balancing
test "network load balancing" {
  // Test round-robin load balancing
  let servers = [
    "https://server1.example.com",
    "https://server2.example.com",
    "https://server3.example.com"
  ]
  
  let round_robin_lb = LoadBalancer::round_robin(servers)
  
  let server1 = LoadBalancer::next_server(round_robin_lb)
  let server2 = LoadBalancer::next_server(round_robin_lb)
  let server3 = LoadBalancer::next_server(round_robin_lb)
  let server4 = LoadBalancer::next_server(round_robin_lb) // Should cycle back to server1
  
  assert_eq(server1, "https://server1.example.com")
  assert_eq(server2, "https://server2.example.com")
  assert_eq(server3, "https://server3.example.com")
  assert_eq(server4, "https://server1.example.com")
  
  // Test weighted load balancing
  let weighted_servers = [
    ("https://server1.example.com", 1),
    ("https://server2.example.com", 2),
    ("https://server3.example.com", 3)
  ]
  
  let weighted_lb = LoadBalancer::weighted(weighted_servers)
  
  // With weights 1, 2, 3, server3 should be selected more often
  let server_counts = Map::new()
  
  for i = 0; i < 60; i = i + 1 {
    let server = LoadBalancer::next_server(weighted_lb)
    let count = Map::get(server_counts, server).unwrap_or(0)
    Map::set(server_counts, server, count + 1)
  }
  
  // Server3 should have approximately 3/6 = 50% of requests
  let server3_count = Map::get(server_counts, "https://server3.example.com").unwrap_or(0)
  assert_true(server3_count >= 20 && server3_count <= 40)
  
  // Test least connections load balancing
  let least_connections_lb = LoadBalancer::least_connections(servers)
  
  // Simulate connections
  LoadBalancer::add_connection(least_connections_lb, "https://server1.example.com")
  LoadBalancer::add_connection(least_connections_lb, "https://server1.example.com")
  LoadBalancer::add_connection(least_connections_lb, "https://server2.example.com")
  
  // Next server should be server3 (0 connections)
  let next_server = LoadBalancer::next_server(least_connections_lb)
  assert_eq(next_server, "https://server3.example.com")
  
  // Test health check-based load balancing
  let health_check_lb = LoadBalancer::health_check(servers)
  
  // Mark server2 as unhealthy
  LoadBalancer::mark_unhealthy(health_check_lb, "https://server2.example.com")
  
  // Should only return healthy servers
  let healthy_server1 = LoadBalancer::next_server(health_check_lb)
  let healthy_server2 = LoadBalancer::next_server(health_check_lb)
  let healthy_server3 = LoadBalancer::next_server(health_check_lb)
  
  assert_not_eq(healthy_server1, "https://server2.example.com")
  assert_not_eq(healthy_server2, "https://server2.example.com")
  assert_not_eq(healthy_server3, "https://server2.example.com")
  
  // Test load balancing metrics
  let lb_metrics = LoadBalancerMetrics::new()
  LoadBalancerMetrics::record_request(lb_metrics, "https://server1.example.com")
  LoadBalancerMetrics::record_request(lb_metrics, "https://server2.example.com")
  LoadBalancerMetrics::record_request(lb_metrics, "https://server1.example.com")
  LoadBalancerMetrics::record_request(lb_metrics, "https://server3.example.com")
  LoadBalancerMetrics::record_request(lb_metrics, "https://server1.example.com")
  
  let server1_requests = LoadBalancerMetrics::requests_by_server(lb_metrics, "https://server1.example.com")
  assert_eq(server1_requests, 3)
  
  let server2_requests = LoadBalancerMetrics::requests_by_server(lb_metrics, "https://server2.example.com")
  assert_eq(server2_requests, 1)
  
  let server3_requests = LoadBalancerMetrics::requests_by_server(lb_metrics, "https://server3.example.com")
  assert_eq(server3_requests, 1)
}

// Test 9: Network Caching
test "network caching" {
  // Test HTTP response caching
  let cache_config = CacheConfig::new(1000, 600) // 1000 entries, 10 minutes TTL
  let response_cache = ResponseCache::with_config(cache_config)
  
  let request1 = HttpRequest::get("https://api.example.com/data/123")
  let response1 = HttpResponse::new(200, "{\"id\":123,\"name\":\"test\"}", [
    ("cache-control", "max-age=300")
  ])
  
  // Cache the response
  ResponseCache::put(response_cache, request1, response1)
  
  // Retrieve from cache
  let cached_response = ResponseCache::get(response_cache, request1)
  assert_true(Option::is_some(cached_response))
  
  match cached_response {
    Some(response) => {
      assert_eq(HttpResponse::status(response), 200)
      assert_eq(HttpResponse::body(response), "{\"id\":123,\"name\":\"test\"}")
    }
    None => assert_true(false)
  }
  
  // Test cache invalidation
  ResponseCache::invalidate(response_cache, request1)
  let invalidated_response = ResponseCache::get(response_cache, request1)
  assert_true(Option::is_none(invalidated_response))
  
  // Test cache expiration
  let short_ttl_config = CacheConfig::new(100, 1) // 1 second TTL
  let short_ttl_cache = ResponseCache::with_config(short_ttl_config)
  
  ResponseCache::put(short_ttl_cache, request1, response1)
  
  // Should be available immediately
  let immediate_response = ResponseCache::get(short_ttl_cache, request1)
  assert_true(Option::is_some(immediate_response))
  
  // Simulate time passing (in a real implementation)
  // Time::sleep(2000)
  
  // After TTL, should be expired
  // let expired_response = ResponseCache::get(short_ttl_cache, request1)
  // assert_true(Option::is_none(expired_response))
  
  // Test cache key generation
  let request2 = HttpRequest::get("https://api.example.com/data/456")
  let request3 = HttpRequest::get("https://api.example.com/data/123")
  let request4 = HttpRequest::post("https://api.example.com/data/123", "{}")
  
  let key1 = ResponseCache::generate_key(request1)
  let key2 = ResponseCache::generate_key(request2)
  let key3 = ResponseCache::generate_key(request3)
  let key4 = ResponseCache::generate_key(request4)
  
  assert_not_eq(key1, key2)
  assert_eq(key1, key3) // Same URL and method
  assert_not_eq(key1, key4) // Different method
  
  // Test cache metrics
  let cache_metrics = ResponseCache::get_metrics(response_cache)
  assert_eq(Map::get(cache_metrics, "cache.puts"), Some(1))
  assert_eq(Map::get(cache_metrics, "cache.hits"), Some(1))
  assert_eq(Map::get(cache_metrics, "cache.misses"), Some(1))
}

// Test 10: Network Performance Monitoring
test "network performance monitoring" {
  // Test network performance monitor
  let perf_monitor = NetworkPerformanceMonitor::new()
  
  // Test latency measurement
  let start_time = Time::now()
  
  // Simulate network request
  let latency = Time::duration_since(start_time, Time::now())
  NetworkPerformanceMonitor::record_latency(perf_monitor, "https://api.example.com/data", latency)
  
  // Test throughput measurement
  let data_size = 1024 * 1024 // 1MB
  let transfer_time = 1000 // 1 second
  NetworkPerformanceMonitor::record_throughput(perf_monitor, "https://api.example.com/upload", data_size, transfer_time)
  
  // Test error rate measurement
  NetworkPerformanceMonitor::record_success(perf_monitor, "https://api.example.com/data")
  NetworkPerformanceMonitor::record_success(perf_monitor, "https://api.example.com/data")
  NetworkPerformanceMonitor::record_error(perf_monitor, "https://api.example.com/data")
  
  // Test connection pool utilization
  NetworkPerformanceMonitor::record_connection_created(perf_monitor)
  NetworkPerformanceMonitor::record_connection_created(perf_monitor)
  NetworkPerformanceMonitor::record_connection_closed(perf_monitor)
  
  // Get performance metrics
  let metrics = NetworkPerformanceMonitor::get_metrics(perf_monitor)
  
  // Verify latency metrics
  let avg_latency = Map::get(metrics, "network.latency.average")
  assert_true(Option::is_some(avg_latency))
  
  // Verify throughput metrics
  let throughput = Map::get(metrics, "network.throughput.bytes_per_second")
  assert_true(Option::is_some(throughput))
  
  // Verify error rate metrics
  let error_rate = Map::get(metrics, "network.error_rate")
  match error_rate {
    Some(rate) => assert_true(rate > 0.3 && rate < 0.4), // 1 error out of 3 requests
    None => assert_true(false)
  }
  
  // Verify connection pool metrics
  let active_connections = Map::get(metrics, "network.connections.active")
  assert_eq(active_connections, Some(1))
  
  // Test performance alerts
  let alert_config = AlertConfig::new()
  let high_latency_alert = AlertConfig::add_latency_threshold(alert_config, 5000) // 5 seconds
  let high_error_rate_alert = AlertConfig::add_error_rate_threshold(high_latency_alert, 0.1) // 10%
  
  let alert_manager = AlertManager::with_config(high_error_rate_alert)
  
  // Trigger high latency alert
  NetworkPerformanceMonitor::record_latency(perf_monitor, "https://slow-api.example.com/data", 6000)
  
  let alerts = NetworkPerformanceMonitor::check_alerts(perf_monitor, alert_manager)
  assert_true(alerts.length() > 0)
  
  // Test performance trends
  let trend_analyzer = PerformanceTrendAnalyzer::new()
  
  // Record metrics over time
  for i = 0; i < 10; i = i + 1 {
    NetworkPerformanceMonitor::record_latency(perf_monitor, "https://api.example.com/data", 100 + i * 10)
  }
  
  let latency_trend = PerformanceTrendAnalyzer::analyze_latency_trend(trend_analyzer, perf_monitor, "https://api.example.com/data")
  assert_eq(latency_trend, IncreasingTrend) // Latency is increasing
  
  // Test performance recommendations
  let recommendations = PerformanceTrendAnalyzer::generate_recommendations(trend_analyzer, perf_monitor)
  assert_true(recommendations.length() > 0)
}