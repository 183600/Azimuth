// Azimuth Telemetry System - Enhanced Network Communication Tests
// This file contains comprehensive test cases for enhanced network communication functionality

// Test 1: HTTP Client Advanced Features
test "http client advanced features" {
  // Test HTTP client creation with custom configuration
  let config = HttpClientConfig::new()
    .with_timeout(5000)  // 5 seconds timeout
    .with_max_retries(3)  // Max 3 retries
    .with_retry_delay(1000)  // 1 second between retries
    .with_user_agent("Azimuth-Telemetry/1.0")
  
  let http_client = HttpClient::new(config)
  assert_true(HttpClient::is_valid(http_client))
  
  // Test GET request with headers
  let headers = [
    ("Accept", "application/json"),
    ("Authorization", "Bearer test_token")
  ]
  
  let request = HttpRequest::new("GET", "https://httpbin.org/get", headers, None)
  let response = HttpClient::execute(http_client, request)
  
  assert_eq(response.status_code, 200)
  assert_true(response.headers.contains("content-type"))
  assert_true(response.body.contains("\"url\""))
  
  // Test POST request with JSON body
  let post_headers = [("Content-Type", "application/json")]
  let json_body = "{\"key\": \"value\", \"number\": 42}"
  let post_request = HttpRequest::new("POST", "https://httpbin.org/post", post_headers, Some(json_body))
  let post_response = HttpClient::execute(http_client, post_request)
  
  assert_eq(post_response.status_code, 200)
  assert_true(post_response.body.contains("\"key\": \"value\""))
  
  // Test request with query parameters
  let query_params = [("param1", "value1"), ("param2", "value2")]
  let query_request = HttpRequest::new_with_params("GET", "https://httpbin.org/get", [], query_params, None)
  let query_response = HttpClient::execute(http_client, query_request)
  
  assert_eq(query_response.status_code, 200)
  assert_true(query_response.body.contains("\"param1\": \"value1\""))
  assert_true(query_response.body.contains("\"param2\": \"value2\""))
}

// Test 2: Connection Pooling
test "connection pooling" {
  // Test connection pool creation
  let pool_config = ConnectionPoolConfig::new()
    .with_max_connections(10)
    .with_max_idle_time(60000)  // 60 seconds
    .with_connection_timeout(5000)  // 5 seconds
  
  let connection_pool = ConnectionPool::new(pool_config)
  assert_true(ConnectionPool::is_valid(connection_pool))
  
  // Test connection acquisition
  let conn1 = ConnectionPool::acquire(connection_pool, "https://httpbin.org")
  assert_true(ConnectionPool::is_valid_connection(conn1))
  
  let conn2 = ConnectionPool::acquire(connection_pool, "https://httpbin.org")
  assert_true(ConnectionPool::is_valid_connection(conn2))
  
  // Test connection reuse
  ConnectionPool::release(connection_pool, conn1)
  let reused_conn = ConnectionPool::acquire(connection_pool, "https://httpbin.org")
  assert_true(ConnectionPool::is_reused(reused_conn))
  
  // Test pool statistics
  let stats = ConnectionPool::get_statistics(connection_pool)
  assert_eq(stats.total_connections, 2)
  assert_eq(stats.active_connections, 1)
  assert_eq(stats.idle_connections, 1)
  assert_true(stats.total_acquisitions >= 3)
  
  // Test connection timeout
  ConnectionPool::release(connection_pool, conn2)
  ConnectionPool::release(connection_pool, reused_conn)
  
  // Test pool cleanup
  ConnectionPool::cleanup_idle_connections(connection_pool)
  let cleanup_stats = ConnectionPool::get_statistics(connection_pool)
  assert_eq(cleanup_stats.idle_connections, 0)
}

// Test 3: Retry and Circuit Breaker
test "retry and circuit breaker" {
  // Test retry mechanism
  let retry_config = RetryConfig::new()
    .with_max_attempts(3)
    .with_initial_delay(100)  // 100ms
    .with_max_delay(1000)  // 1 second
    .with_backoff_multiplier(2.0)
  
  let retry_client = HttpClient::with_retry(retry_config)
  
  // Test successful request
  let success_request = HttpRequest::new("GET", "https://httpbin.org/status/200", [], None)
  let success_response = HttpClient::execute(retry_client, success_request)
  assert_eq(success_response.status_code, 200)
  
  // Test request that fails initially but succeeds after retry
  let flaky_request = HttpRequest::new("GET", "https://httpbin.org/status/500", [], None)
  let flaky_response = HttpClient::execute(retry_client, flaky_request)
  assert_eq(flaky_response.status_code, 500)  // Should still fail after all retries
  
  // Test circuit breaker
  let circuit_breaker_config = CircuitBreakerConfig::new()
    .with_failure_threshold(3)
    .with_timeout(5000)  // 5 seconds
    .with_success_threshold(2)
  
  let circuit_breaker = CircuitBreaker::new(circuit_breaker_config)
  
  // Test circuit breaker state transitions
  assert_true(CircuitBreaker::is_closed(circuit_breaker))
  
  // Simulate failures
  for i = 1; i <= 3; i = i + 1 {
    let failure_request = HttpRequest::new("GET", "https://httpbin.org/status/500", [], None)
    let response = CircuitBreaker::execute(circuit_breaker, || {
      HttpClient::execute(retry_client, failure_request)
    })
    assert_eq(response.status_code, 500)
  }
  
  // Circuit should be open now
  assert_true(CircuitBreaker::is_open(circuit_breaker))
  
  // Test request rejection when circuit is open
  let rejected_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  try {
    CircuitBreaker::execute(circuit_breaker, || {
      HttpClient::execute(retry_client, rejected_request)
    })
    assert_true(false)  // Should not reach here
  } catch {
    CircuitBreakerOpenError => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 4: WebSocket Communication
test "websocket communication" {
  // Test WebSocket client creation
  let ws_config = WebSocketConfig::new()
    .with_timeout(5000)  // 5 seconds
    .with_max_message_size(1024 * 1024)  // 1MB
    .with_heartbeat_interval(30000)  // 30 seconds
  
  let ws_client = WebSocketClient::new(ws_config)
  assert_true(WebSocketClient::is_valid(ws_client))
  
  // Test WebSocket connection
  let connection = WebSocketClient::connect(ws_client, "wss://echo.websocket.org")
  assert_true(WebSocketConnection::is_connected(connection))
  
  // Test sending text message
  let text_message = "Hello WebSocket!"
  WebSocketConnection::send_text(connection, text_message)
  
  // Test receiving text message
  let received_message = WebSocketConnection::receive_text(connection, 5000)  // 5 seconds timeout
  assert_eq(received_message, text_message)
  
  // Test sending binary message
  let binary_data = [1, 2, 3, 4, 5]
  WebSocketConnection::send_binary(connection, binary_data)
  
  // Test receiving binary message
  let received_binary = WebSocketConnection::receive_binary(connection, 5000)  // 5 seconds timeout
  assert_eq(received_binary, binary_data)
  
  // Test ping/pong
  WebSocketConnection::ping(connection)
  let pong_received = WebSocketConnection::wait_for_pong(connection, 5000)  // 5 seconds timeout
  assert_true(pong_received)
  
  // Test connection statistics
  let stats = WebSocketConnection::get_statistics(connection)
  assert_true(stats.messages_sent >= 3)
  assert_true(stats.messages_received >= 3)
  assert_true(stats.bytes_sent > 0)
  assert_true(stats.bytes_received > 0)
  
  // Test graceful close
  WebSocketConnection::close(connection, 1000, "Normal closure")
  assert_false(WebSocketConnection::is_connected(connection))
}

// Test 5: Load Balancing
test "load balancing" {
  // Test load balancer creation
  let servers = [
    "https://httpbin.org",
    "https://httpbin.org",
    "https://httpbin.org"
  ]
  
  let load_balancer = LoadBalancer::new(servers)
  assert_true(LoadBalancer::is_valid(load_balancer))
  
  // Test round-robin strategy
  LoadBalancer::set_strategy(load_balancer, RoundRobin)
  
  let server1 = LoadBalancer::get_server(load_balancer)
  let server2 = LoadBalancer::get_server(load_balancer)
  let server3 = LoadBalancer::get_server(load_balancer)
  let server4 = LoadBalancer::get_server(load_balancer)  // Should wrap around
  
  assert_true(servers.contains(server1))
  assert_true(servers.contains(server2))
  assert_true(servers.contains(server3))
  assert_eq(server1, server4)  // Should wrap around to first server
  
  // Test least connections strategy
  LoadBalancer::set_strategy(load_balancer, LeastConnections)
  
  // Simulate connections
  LoadBalancer::increment_connections(load_balancer, servers[0])
  LoadBalancer::increment_connections(load_balancer, servers[0])
  LoadBalancer::increment_connections(load_balancer, servers[1])
  
  let least_conn_server = LoadBalancer::get_server(load_balancer)
  assert_eq(least_conn_server, servers[2])  // Should have least connections
  
  // Test health check
  LoadBalancer::health_check(load_balancer)
  let healthy_servers = LoadBalancer::get_healthy_servers(load_balancer)
  assert_true(healthy_servers.length() >= 1)
  
  // Test load balancer statistics
  let stats = LoadBalancer::get_statistics(load_balancer)
  assert_true(stats.total_requests >= 4)
  assert_eq(stats.server_connections.get(servers[0]), Some(2))
  assert_eq(stats.server_connections.get(servers[1]), Some(1))
  assert_eq(stats.server_connections.get(servers[2]), Some(1))
}

// Test 6: Rate Limiting
test "rate limiting" {
  // Test rate limiter creation
  let rate_limiter = RateLimiter::new(10, 60)  // 10 requests per minute
  assert_true(RateLimiter::is_valid(rate_limiter))
  
  // Test rate limiting
  let client_id = "test_client"
  
  // First 10 requests should succeed
  for i = 1; i <= 10; i = i + 1 {
    assert_true(RateLimiter::allow_request(rate_limiter, client_id))
  }
  
  // 11th request should be denied
  assert_false(RateLimiter::allow_request(rate_limiter, client_id))
  
  // Test different clients have separate limits
  let other_client_id = "other_client"
  assert_true(RateLimiter::allow_request(rate_limiter, other_client_id))
  
  // Test rate limiter reset
  RateLimiter::reset_client(rate_limiter, client_id)
  assert_true(RateLimiter::allow_request(rate_limiter, client_id))
  
  // Test sliding window rate limiter
  let sliding_limiter = SlidingWindowRateLimiter::new(5, 10)  // 5 requests per 10 seconds
  
  // Make requests at 1-second intervals
  for i = 1; i <= 5; i = i + 1 {
    assert_true(SlidingWindowRateLimiter::allow_request(sliding_limiter, client_id))
    Thread::sleep(1000)  // Wait 1 second
  }
  
  // 6th request should be denied
  assert_false(SlidingWindowRateLimiter::allow_request(sliding_limiter, client_id))
  
  // Wait for window to slide
  Thread::sleep(6000)  // Wait 6 seconds
  
  // Should allow requests again as window slides
  assert_true(SlidingWindowRateLimiter::allow_request(sliding_limiter, client_id))
}

// Test 7: Request/Response Interceptors
test "request/response interceptors" {
  // Test HTTP client with interceptors
  let http_client = HttpClient::new()
  
  // Add request interceptor
  HttpClient::add_request_interceptor(http_client, |request| {
    request.headers.set("X-Custom-Header", "custom_value")
    request.headers.set("X-Request-ID", UUID::generate().to_string())
    request
  })
  
  // Add response interceptor
  HttpClient::add_response_interceptor(http_client, |response| {
    if response.status_code >= 400 {
      log_error("HTTP error: " + response.status_code.to_string())
    }
    response
  })
  
  // Test request interceptor
  let request = HttpRequest::new("GET", "https://httpbin.org/headers", [], None)
  let response = HttpClient::execute(http_client, request)
  
  assert_eq(response.status_code, 200)
  assert_true(response.body.contains("X-Custom-Header"))
  assert_true(response.body.contains("custom_value"))
  assert_true(response.body.contains("X-Request-ID"))
  
  // Test response interceptor with error
  let error_request = HttpRequest::new("GET", "https://httpbin.org/status/404", [], None)
  let error_response = HttpClient::execute(http_client, error_request)
  
  assert_eq(error_response.status_code, 404)
  // Response interceptor should log the error
  
  // Test logging interceptor
  let log_entries = []
  HttpClient::add_request_interceptor(http_client, |request| {
    log_entries.push("Request: " + request.method + " " + request.url)
    request
  })
  
  HttpClient::add_response_interceptor(http_client, |response| {
    log_entries.push("Response: " + response.status_code.to_string())
    response
  })
  
  let test_request = HttpRequest::new("GET", "https://httpbin.org/status/200", [], None)
  HttpClient::execute(http_client, test_request)
  
  assert_true(log_entries.length() >= 2)
  assert_true(log_entries.some(|entry| entry.contains("Request: GET")))
  assert_true(log_entries.some(|entry| entry.contains("Response: 200")))
}

// Test 8: Request Caching
test "request caching" {
  // Test cache configuration
  let cache_config = RequestCacheConfig::new()
    .with_max_size(100)
    .with_ttl(60000)  // 60 seconds
    .with_cacheable_status_codes([200, 301, 302])
  
  let request_cache = RequestCache::new(cache_config)
  
  // Test GET request caching
  let get_request = HttpRequest::new("GET", "https://httpbin.org/cache", [], None)
  
  // First request should not be cached
  let response1 = RequestCache::execute(request_cache, get_request)
  assert_eq(response1.status_code, 200)
  assert_false(response1.from_cache)
  
  // Second request should be cached
  let response2 = RequestCache::execute(request_cache, get_request)
  assert_eq(response2.status_code, 200)
  assert_true(response2.from_cache)
  
  // Test POST request should not be cached
  let post_request = HttpRequest::new("POST", "https://httpbin.org/post", [], Some("test data"))
  let post_response1 = RequestCache::execute(request_cache, post_request)
  assert_eq(post_response1.status_code, 200)
  assert_false(post_response1.from_cache)
  
  let post_response2 = RequestCache::execute(request_cache, post_request)
  assert_eq(post_response2.status_code, 200)
  assert_false(post_response2.from_cache)
  
  // Test cache expiration
  let short_ttl_cache = RequestCache::new(RequestCacheConfig::new().with_ttl(100))  // 100ms
  let expire_request = HttpRequest::new("GET", "https://httpbin.org/cache", [], None)
  
  let expire_response1 = RequestCache::execute(short_ttl_cache, expire_request)
  assert_false(expire_response1.from_cache)
  
  Thread::sleep(150)  // Wait for cache to expire
  
  let expire_response2 = RequestCache::execute(short_ttl_cache, expire_request)
  assert_false(expire_response2.from_cache)  // Should make new request
  
  // Test cache statistics
  let stats = RequestCache::get_statistics(request_cache)
  assert_true(stats.cache_hits >= 1)
  assert_true(stats.cache_misses >= 1)
  assert_true(stats.hit_ratio >= 0.0)
}

// Test 9: Network Resilience
test "network resilience" {
  // Test timeout handling
  let timeout_config = HttpClientConfig::new().with_timeout(1000)  // 1 second timeout
  let timeout_client = HttpClient::new(timeout_config)
  
  let slow_request = HttpRequest::new("GET", "https://httpbin.org/delay/5", [], None)
  
  try {
    HttpClient::execute(timeout_client, slow_request)
    assert_true(false)  // Should not reach here due to timeout
  } catch {
    TimeoutError => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test DNS resolution failure handling
  let invalid_request = HttpRequest::new("GET", "https://nonexistent.domain.example", [], None)
  
  try {
    HttpClient::execute(timeout_client, invalid_request)
    assert_true(false)  // Should not reach here due to DNS failure
  } catch {
    DnsResolutionError => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test connection refused handling
  let connection_refused_request = HttpRequest::new("GET", "http://localhost:99999", [], None)
  
  try {
    HttpClient::execute(timeout_client, connection_refused_request)
    assert_true(false)  // Should not reach here due to connection refused
  } catch {
    ConnectionRefusedError => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test network resilience with fallback
  let fallback_config = ResilienceConfig::new()
    .with_primary_server("https://httpbin.org")
    .with_fallback_server("https://postman-echo.com")
    .with_max_retries(2)
    .with_retry_delay(500)
  
  let resilient_client = HttpClient::with_resilience(fallback_config)
  
  let resilient_request = HttpRequest::new("GET", "/get", [], None)
  let resilient_response = HttpClient::execute(resilient_client, resilient_request)
  
  assert_true(resilient_response.status_code == 200)
  assert_true(resilient_response.from_primary || resilient_response.from_fallback)
}

// Test 10: Network Metrics and Monitoring
test "network metrics and monitoring" {
  // Test network monitor
  let network_monitor = NetworkMonitor::new()
  NetworkMonitor::start(network_monitor)
  
  // Execute some network requests
  let http_client = HttpClient::new()
  
  for i = 1; i <= 10; i = i + 1 {
    let request = HttpRequest::new("GET", "https://httpbin.org/status/200", [], None)
    HttpClient::execute(http_client, request)
  }
  
  // Get network metrics
  let metrics = NetworkMonitor::get_metrics(network_monitor)
  
  assert_true(metrics.total_requests >= 10)
  assert_true(metrics.successful_requests >= 10)
  assert_eq(metrics.failed_requests, 0)
  assert_true(metrics.avg_response_time > 0)
  assert_true(metrics.min_response_time > 0)
  assert_true(metrics.max_response_time >= metrics.min_response_time)
  assert_true(metrics.total_bytes_sent > 0)
  assert_true(metrics.total_bytes_received > 0)
  
  // Test per-host metrics
  let host_metrics = NetworkMonitor::get_host_metrics(network_monitor, "httpbin.org")
  assert_true(host_metrics.total_requests >= 10)
  assert_true(host_metrics.successful_requests >= 10)
  
  // Test error metrics with some failed requests
  for i = 1; i <= 5; i = i + 1 {
    let error_request = HttpRequest::new("GET", "https://httpbin.org/status/404", [], None)
    HttpClient::execute(http_client, error_request)
  }
  
  let updated_metrics = NetworkMonitor::get_metrics(network_monitor)
  assert_true(updated_metrics.total_requests >= 15)
  assert_true(updated_metrics.failed_requests >= 5)
  assert_true(updated_metrics.error_rate > 0.0)
  
  // Test network health check
  let health_status = NetworkMonitor::check_health(network_monitor)
  assert_true(health_status.overall_health == "healthy" || health_status.overall_health == "degraded")
  assert_true(health_status.hosts_status.contains("httpbin.org"))
  
  // Stop monitoring
  NetworkMonitor::stop(network_monitor)
  
  // Test metrics export
  let metrics_json = NetworkMonitor::export_metrics(network_monitor, "json")
  assert_true(metrics_json.contains("total_requests"))
  assert_true(metrics_json.contains("successful_requests"))
  assert_true(metrics_json.contains("avg_response_time"))
}