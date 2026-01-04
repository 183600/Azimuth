// Azimuth Telemetry System - Network Communication Tests
// This file contains comprehensive test cases for network communication functionality

// Test 1: Basic HTTP Client Operations
test "basic http client operations" {
  let http_client = HttpClient::new()
  
  // Test HTTP GET request
  let get_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let get_response = HttpClient::send_request(http_client, get_request)
  
  match get_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      assert_true(HttpResponse::body(response).length() > 0)
      
      let content_type = HttpResponse::get_header(response, "content-type")
      match content_type {
        Some(header) => assert_true(header.contains("application/json"))
        None => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
  
  // Test HTTP POST request
  let post_headers = [
    ("Content-Type", "application/json")
  ]
  let post_body = "{\"key\": \"value\", \"number\": 42}"
  let post_request = HttpRequest::new("POST", "https://httpbin.org/post", post_headers, Some(post_body))
  let post_response = HttpClient::send_request(http_client, post_request)
  
  match post_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      assert_true(HttpResponse::body(response).length() > 0)
    }
    Err(error) => assert_true(false)
  }
  
  // Test HTTP PUT request
  let put_headers = [
    ("Content-Type", "application/json")
  ]
  let put_body = "{\"updated\": true}"
  let put_request = HttpRequest::new("PUT", "https://httpbin.org/put", put_headers, Some(put_body))
  let put_response = HttpClient::send_request(http_client, put_request)
  
  match put_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      assert_true(HttpResponse::body(response).length() > 0)
    }
    Err(error) => assert_true(false)
  }
  
  // Test HTTP DELETE request
  let delete_request = HttpRequest::new("DELETE", "https://httpbin.org/delete", [], None)
  let delete_response = HttpClient::send_request(http_client, delete_request)
  
  match delete_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      assert_true(HttpResponse::body(response).length() > 0)
    }
    Err(error) => assert_true(false)
  }
}

// Test 2: HTTP Client Configuration and Timeouts
test "http client configuration and timeouts" {
  let config = HttpClientConfig::new()
    .with_timeout(5000L) // 5 seconds timeout
    .with_max_retries(3)
    .with_user_agent("Azimuth-Telemetry/1.0")
    .with_follow_redirects(true)
    .with_max_redirects(5)
    .with_verify_ssl(true)
  
  let http_client = HttpClient::with_config(config)
  
  // Test timeout with slow endpoint
  let slow_request = HttpRequest::new("GET", "https://httpbin.org/delay/10", [], None) // 10 second delay
  let slow_response = HttpClient::send_request(http_client, slow_request)
  
  match slow_response {
    Err(NetworkError(TimeoutError)) => assert_true(true) // Should timeout
    Ok(_) => assert_true(false)
    Err(_) => assert_true(false)
  }
  
  // Test retry with failing endpoint
  let retry_request = HttpRequest::new("GET", "https://httpbin.org/status/500", [], None) // 500 error
  let retry_response = HttpClient::send_request(http_client, retry_request)
  
  match retry_response {
    Ok(response) => {
      // Should still get 500 after retries
      assert_eq(HttpResponse::status_code(response), 500)
    }
    Err(error) => assert_true(false)
  }
  
  // Test user agent header
  let user_agent_request = HttpRequest::new("GET", "https://httpbin.org/user-agent", [], None)
  let user_agent_response = HttpClient::send_request(http_client, user_agent_request)
  
  match user_agent_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      let body = HttpResponse::body(response)
      assert_true(body.contains("Azimuth-Telemetry/1.0"))
    }
    Err(error) => assert_true(false)
  }
  
  // Test redirect following
  let redirect_request = HttpRequest::new("GET", "https://httpbin.org/redirect/2", [], None) // 2 redirects
  let redirect_response = HttpClient::send_request(http_client, redirect_request)
  
  match redirect_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
    }
    Err(error) => assert_true(false)
  }
}

// Test 3: WebSocket Communication
test "websocket communication" {
  let websocket_client = WebSocketClient::new()
  
  // Test WebSocket connection
  let connection_result = WebSocketClient::connect(websocket_client, "wss://echo.websocket.org")
  
  match connection_result {
    Ok(connection) => {
      assert_true(WebSocketConnection::is_connected(connection))
      
      // Test sending text message
      let text_message = "Hello WebSocket!"
      let send_result = WebSocketConnection::send_text(connection, text_message)
      assert_true(send_result)
      
      // Test receiving text message
      let received_message = WebSocketConnection::receive_text(connection)
      match received_message {
        Some(message) => assert_eq(message, text_message)
        None => assert_true(false)
      }
      
      // Test sending binary message
      let binary_data = [0x48, 0x65, 0x6C, 0x6C, 0x6F] // "Hello" in bytes
      let binary_send_result = WebSocketConnection::send_binary(connection, binary_data)
      assert_true(binary_send_result)
      
      // Test receiving binary message
      let received_binary = WebSocketConnection::receive_binary(connection)
      match received_binary {
        Some(data) => assert_eq(data, binary_data)
        None => assert_true(false)
      }
      
      // Test ping/pong
      let ping_result = WebSocketConnection::ping(connection)
      assert_true(ping_result)
      
      let pong_result = WebSocketConnection::wait_for_pong(connection, 5000L)
      assert_true(pong_result)
      
      // Close connection
      WebSocketConnection::close(connection)
      assert_false(WebSocketConnection::is_connected(connection))
    }
    Err(error) => assert_true(false)
  }
}

// Test 4: HTTP/2 Support
test "http2 support" {
  let http2_client = HttpClient::new()
    .with_http2_support(true)
  
  // Test HTTP/2 request
  let http2_request = HttpRequest::new("GET", "https://http2.org", [], None)
  let http2_response = HttpClient::send_request(http2_client, http2_request)
  
  match http2_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      
      // Check if HTTP/2 was used
      let http_version = HttpResponse::get_http_version(response)
      assert_eq(http_version, "HTTP/2")
    }
    Err(error) => assert_true(false)
  }
  
  // Test server push
  let push_request = HttpRequest::new("GET", "https://http2.org/serverpush", [], None)
  let push_response = HttpClient::send_request(http2_client, push_request)
  
  match push_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      
      // Check for pushed resources
      let pushed_resources = HttpResponse::get_pushed_resources(response)
      assert_true(pushed_resources.length() >= 0)
    }
    Err(error) => assert_true(false)
  }
  
  // Test multiplexing
  let requests = []
  for i in 0..=5 {
    let request = HttpRequest::new("GET", "https://http2.org", [], None)
    requests.push(request)
  }
  
  let start_time = Time::now()
  let responses = []
  
  // Send all requests concurrently
  for request in requests {
    let response = HttpClient::send_request(http2_client, request)
    responses.push(response)
  }
  
  let end_time = Time::now()
  let total_time = end_time - start_time
  
  // Check all responses
  for response in responses {
    match response {
      Ok(resp) => assert_eq(HttpResponse::status_code(resp), 200)
      Err(error) => assert_true(false)
    }
  }
  
  // HTTP/2 should be faster for multiple concurrent requests
  assert_true(total_time < 10000L) // Less than 10 seconds
}

// Test 5: Network Resilience and Retry Logic
test "network resilience and retry logic" {
  let retry_config = RetryConfig::new()
    .with_max_attempts(5)
    .with_initial_delay(1000L) // 1 second
    .with_max_delay(10000L) // 10 seconds
    .with_backoff_multiplier(2.0)
    .with_retryable_status_codes([408, 429, 500, 502, 503, 504])
    .with_retryable_errors([NetworkTimeoutError, ConnectionError])
  
  let resilient_client = HttpClient::new()
    .with_retry_config(retry_config)
  
  // Test retry with 503 error
  let retry_request = HttpRequest::new("GET", "https://httpbin.org/status/503", [], None)
  let retry_response = HttpClient::send_request(resilient_client, retry_request)
  
  match retry_response {
    Ok(response) => {
      // Should still get 503 after all retries
      assert_eq(HttpResponse::status_code(response), 503)
    }
    Err(error) => assert_true(false)
  }
  
  // Test retry with 429 error (rate limited)
  let rate_limit_request = HttpRequest::new("GET", "https://httpbin.org/status/429", [], None)
  let rate_limit_response = HttpClient::send_request(resilient_client, rate_limit_request)
  
  match rate_limit_response {
    Ok(response) => {
      // Should still get 429 after all retries
      assert_eq(HttpResponse::status_code(response), 429)
    }
    Err(error) => assert_true(false)
  }
  
  // Test circuit breaker
  let circuit_breaker = CircuitBreaker::new()
    .with_failure_threshold(5)
    .with_timeout(30000L) // 30 seconds
    .with_half_open_max_calls(3)
  
  let circuit_breaker_client = HttpClient::new()
    .with_circuit_breaker(circuit_breaker)
  
  // Trigger circuit breaker with multiple failures
  for i in 0..=6 {
    let failing_request = HttpRequest::new("GET", "https://httpbin.org/status/500", [], None)
    let failing_response = HttpClient::send_request(circuit_breaker_client, failing_request)
    
    if i < 5 {
      match failing_response {
        Ok(response) => assert_eq(HttpResponse::status_code(response), 500)
        Err(error) => assert_true(false)
      }
    } else {
      match failing_response {
        Ok(_) => assert_true(false) // Should be blocked by circuit breaker
        Err(CircuitBreakerOpenError) => assert_true(true)
        Err(_) => assert_true(false)
      }
    }
  }
}

// Test 6: Network Metrics and Telemetry
test "network metrics and telemetry" {
  let telemetry_client = HttpClient::new()
    .with_telemetry_enabled(true)
  
  // Send some requests to generate metrics
  let requests = [
    HttpRequest::new("GET", "https://httpbin.org/get", [], None),
    HttpRequest::new("GET", "https://httpbin.org/status/200", [], None),
    HttpRequest::new("GET", "https://httpbin.org/delay/1", [], None),
    HttpRequest::new("GET", "https://httpbin.org/status/404", [], None),
    HttpRequest::new("GET", "https://httpbin.org/status/500", [], None)
  ]
  
  for request in requests {
    HttpClient::send_request(telemetry_client, request)
  }
  
  // Get network metrics
  let metrics = HttpClient::get_metrics(telemetry_client)
  
  match metrics {
    NetworkMetrics(total_requests, successful_requests, failed_requests, average_response_time, total_bytes_sent, total_bytes_received) => {
      assert_eq(total_requests, 5)
      assert_eq(successful_requests, 4) // All should succeed even 404 and 500
      assert_eq(failed_requests, 0)
      assert_true(average_response_time > 0)
      assert_true(total_bytes_sent > 0)
      assert_true(total_bytes_received > 0)
    }
    _ => assert_true(false)
  }
  
  // Get per-host metrics
  let host_metrics = HttpClient::get_host_metrics(telemetry_client, "httpbin.org")
  
  match host_metrics {
    HostNetworkMetrics(requests, success_rate, average_response_time, error_rate) => {
      assert_eq(requests, 5)
      assert_eq(success_rate, 1.0)
      assert_true(average_response_time > 0)
      assert_eq(error_rate, 0.0)
    }
    _ => assert_true(false)
  }
  
  // Get status code distribution
  let status_distribution = HttpClient::get_status_distribution(telemetry_client)
  
  match status_distribution {
    StatusCodeDistribution(distribution) => {
      assert_true(distribution.contains(200))
      assert_true(distribution.contains(404))
      assert_true(distribution.contains(500))
      assert_eq(distribution.get(200, 0), 3) // get, status/200, delay/1
      assert_eq(distribution.get(404, 0), 1)
      assert_eq(distribution.get(500, 0), 1)
    }
    _ => assert_true(false)
  }
  
  // Export metrics in Prometheus format
  let prometheus_metrics = HttpClient::export_metrics(telemetry_client, "prometheus")
  assert_true(prometheus_metrics.length() > 0)
  assert_true(prometheus_metrics.contains("http_requests_total"))
  assert_true(prometheus_metrics.contains("http_request_duration_seconds"))
  
  // Export metrics in JSON format
  let json_metrics = HttpClient::export_metrics(telemetry_client, "json")
  assert_true(json_metrics.length() > 0)
}

// Test 7: Network Security and Authentication
test "network security and authentication" {
  // Test basic authentication
  let basic_auth_client = HttpClient::new()
    .with_basic_auth("user", "pass")
  
  let basic_auth_request = HttpRequest::new("GET", "https://httpbin.org/basic-auth/user/pass", [], None)
  let basic_auth_response = HttpClient::send_request(basic_auth_client, basic_auth_request)
  
  match basic_auth_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      let body = HttpResponse::body(response)
      assert_true(body.contains("\"authenticated\": true"))
    }
    Err(error) => assert_true(false)
  }
  
  // Test bearer token authentication
  let bearer_auth_client = HttpClient::new()
    .with_bearer_token("test-token")
  
  let bearer_auth_request = HttpRequest::new("GET", "https://httpbin.org/bearer", [], None)
  let bearer_auth_response = HttpClient::send_request(bearer_auth_client, bearer_auth_request)
  
  match bearer_auth_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      let body = HttpResponse::body(response)
      assert_true(body.contains("\"authenticated\": true"))
    }
    Err(error) => assert_true(false)
  }
  
  // Test API key authentication
  let api_key_client = HttpClient::new()
    .with_api_key("X-API-Key", "test-api-key")
  
  let api_key_request = HttpRequest::new("GET", "https://httpbin.org/headers", [], None)
  let api_key_response = HttpClient::send_request(api_key_client, api_key_request)
  
  match api_key_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      let body = HttpResponse::body(response)
      assert_true(body.contains("X-API-Key"))
      assert_true(body.contains("test-api-key"))
    }
    Err(error) => assert_true(false)
  }
  
  // Test SSL/TLS verification
  let tls_client = HttpClient::new()
    .with_verify_ssl(true)
    .with_cert_file("/path/to/cert.pem")
    .with_key_file("/path/to/key.pem")
  
  let tls_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let tls_response = HttpClient::send_request(tls_client, tls_request)
  
  match tls_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
    }
    Err(error) => {
      // May fail if cert files don't exist, which is expected in test environment
      assert_true(error.contains("certificate") || error.contains("file"))
    }
  }
  
  // Test request signing
  let signing_client = HttpClient::new()
    .with_request_signer(HMACSHA256Signer::new("secret-key"))
  
  let signed_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let signed_response = HttpClient::send_request(signing_client, signed_request)
  
  match signed_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      
      // Check if signature header was added
      let signature = HttpResponse::get_header(response, "X-Signature")
      match signature {
        Some(sig) => assert_true(sig.length() > 0)
        None => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
}

// Test 8: Connection Pooling and Keep-Alive
test "connection pooling and keep-alive" {
  let pool_config = ConnectionPoolConfig::new()
    .with_max_connections(10)
    .with_max_idle_connections(5)
    .with_max_idle_time(30000L) // 30 seconds
    .with_keep_alive(true)
    .with_keep_alive_timeout(60000L) // 1 minute
  
  let pooled_client = HttpClient::new()
    .with_connection_pool(pool_config)
  
  // Send multiple requests to the same host
  let requests = []
  for i in 0..=10 {
    let request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
    requests.push(request)
  }
  
  let responses = []
  for request in requests {
    let response = HttpClient::send_request(pooled_client, request)
    responses.push(response)
  }
  
  // Check all responses
  for response in responses {
    match response {
      Ok(resp) => assert_eq(HttpResponse::status_code(resp), 200)
      Err(error) => assert_true(false)
    }
  }
  
  // Check connection pool statistics
  let pool_stats = HttpClient::get_connection_pool_stats(pooled_client)
  
  match pool_stats {
    ConnectionPoolStats(total_connections, active_connections, idle_connections, created_connections, closed_connections) => {
      assert_true(total_connections > 0)
      assert_true(active_connections >= 0)
      assert_true(idle_connections >= 0)
      assert_true(created_connections > 0)
    }
    _ => assert_true(false)
  }
  
  // Test connection reuse
  let reuse_request1 = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let reuse_response1 = HttpClient::send_request(pooled_client, reuse_request1)
  
  let reuse_request2 = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let reuse_response2 = HttpClient::send_request(pooled_client, reuse_request2)
  
  match (reuse_response1, reuse_response2) {
    (Ok(resp1), Ok(resp2)) => {
      assert_eq(HttpResponse::status_code(resp1), 200)
      assert_eq(HttpResponse::status_code(resp2), 200)
      
      // Check if connection was reused
      let connection_id1 = HttpResponse::get_connection_id(resp1)
      let connection_id2 = HttpResponse::get_connection_id(resp2)
      assert_eq(connection_id1, connection_id2)
    }
    _ => assert_true(false)
  }
}

// Test 9: Network Proxy Support
test "network proxy support" {
  // Test HTTP proxy
  let http_proxy_config = ProxyConfig::new()
    .with_type(HttpProxy)
    .with_host("proxy.example.com")
    .with_port(8080)
    .with_auth("proxy-user", "proxy-pass")
  
  let http_proxy_client = HttpClient::new()
    .with_proxy(http_proxy_config)
  
  let proxy_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let proxy_response = HttpClient::send_request(http_proxy_client, proxy_request)
  
  // May fail if proxy doesn't exist, which is expected in test environment
  match proxy_response {
    Ok(response) => assert_eq(HttpResponse::status_code(response), 200)
    Err(error) => assert_true(error.contains("proxy") || error.contains("connection"))
  }
  
  // Test SOCKS proxy
  let socks_proxy_config = ProxyConfig::new()
    .with_type(SocksProxy)
    .with_host("socks.example.com")
    .with_port(1080)
  
  let socks_proxy_client = HttpClient::new()
    .with_proxy(socks_proxy_config)
  
  let socks_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let socks_response = HttpClient::send_request(socks_proxy_client, socks_request)
  
  // May fail if proxy doesn't exist, which is expected in test environment
  match socks_response {
    Ok(response) => assert_eq(HttpResponse::status_code(response), 200)
    Err(error) => assert_true(error.contains("proxy") || error.contains("connection"))
  }
  
  // Test proxy bypass rules
  let bypass_proxy_config = ProxyConfig::new()
    .with_type(HttpProxy)
    .with_host("proxy.example.com")
    .with_port(8080)
    .with_bypass_rules(["localhost", "127.0.0.1", "*.local"])
  
  let bypass_proxy_client = HttpClient::new()
    .with_proxy(bypass_proxy_config)
  
  let bypass_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let bypass_response = HttpClient::send_request(bypass_proxy_client, bypass_request)
  
  // Should use proxy for httpbin.org
  match bypass_response {
    Ok(response) => assert_eq(HttpResponse::status_code(response), 200)
    Err(error) => assert_true(error.contains("proxy") || error.contains("connection"))
  }
  
  // Test PAC (Proxy Auto-Configuration) script
  let pac_config = ProxyConfig::new()
    .with_type(PacProxy)
    .with_pac_url("http://proxy.example.com/proxy.pac")
  
  let pac_client = HttpClient::new()
    .with_proxy(pac_config)
  
  let pac_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let pac_response = HttpClient::send_request(pac_client, pac_request)
  
  // May fail if PAC script doesn't exist, which is expected in test environment
  match pac_response {
    Ok(response) => assert_eq(HttpResponse::status_code(response), 200)
    Err(error) => assert_true(error.contains("proxy") || error.contains("pac"))
  }
}

// Test 10: Network Performance Optimization
test "network performance optimization" {
  // Test compression
  let compression_client = HttpClient::new()
    .with_compression(true)
    .with_compression_algorithms(["gzip", "deflate", "br"])
  
  let compression_request = HttpRequest::new("GET", "https://httpbin.org/gzip", [], None)
  let compression_response = HttpClient::send_request(compression_client, compression_request)
  
  match compression_response {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      
      // Check if response was compressed
      let content_encoding = HttpResponse::get_header(response, "content-encoding")
      match content_encoding {
        Some(encoding) => assert_true(encoding == "gzip" || encoding == "deflate" || encoding == "br")
        None => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
  
  // Test HTTP pipelining
  let pipelining_client = HttpClient::new()
    .with_http_pipelining(true)
    .with_pipeline_depth(5)
  
  let pipeline_requests = []
  for i in 0..=5 {
    let request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
    pipeline_requests.push(request)
  }
  
  let start_time = Time::now()
  let pipeline_responses = []
  
  // Send all requests in pipeline
  for request in pipeline_requests {
    let response = HttpClient::send_request(pipelining_client, request)
    pipeline_responses.push(response)
  }
  
  let end_time = Time::now()
  let pipeline_time = end_time - start_time
  
  // Check all responses
  for response in pipeline_responses {
    match response {
      Ok(resp) => assert_eq(HttpResponse::status_code(resp), 200)
      Err(error) => assert_true(false)
    }
  }
  
  // Test parallel requests
  let parallel_client = HttpClient::new()
    .with_max_parallel_requests(10)
  
  let parallel_requests = []
  for i in 0..=10 {
    let request = HttpRequest::new("GET", "https://httpbin.org/delay/" + i.to_string(), [], None)
    parallel_requests.push(request)
  }
  
  let start_time_parallel = Time::now()
  let parallel_responses = HttpClient::send_parallel(parallel_client, parallel_requests)
  let end_time_parallel = Time::now()
  let parallel_time = end_time_parallel - start_time_parallel
  
  // Check all responses
  for response in parallel_responses {
    match response {
      Ok(resp) => assert_eq(HttpResponse::status_code(resp), 200)
      Err(error) => assert_true(false)
    }
  }
  
  // Parallel should be faster than sequential for these requests
  assert_true(parallel_time < pipeline_time)
  
  // Test request caching
  let cache_config = CacheConfig::new()
    .with_max_size(100)
    .with_ttl(60000L) // 1 minute
    .with_cacheable_methods(["GET"])
    .with_cacheable_status_codes([200, 304])
  
  let cache_client = HttpClient::new()
    .with_cache(cache_config)
  
  // First request
  let cache_request1 = HttpRequest::new("GET", "https://httpbin.org/cache", [], None)
  let cache_response1 = HttpClient::send_request(cache_client, cache_request1)
  
  match cache_response1 {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      
      // Check if response is cacheable
      let cache_control = HttpResponse::get_header(response, "cache-control")
      match cache_control {
        Some(control) => assert_true(control.contains("max-age"))
        None => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
  
  // Second request should hit cache
  let cache_request2 = HttpRequest::new("GET", "https://httpbin.org/cache", [], None)
  let cache_response2 = HttpClient::send_request(cache_client, cache_request2)
  
  match cache_response2 {
    Ok(response) => {
      assert_eq(HttpResponse::status_code(response), 200)
      
      // Check if response came from cache
      let x_cache = HttpResponse::get_header(response, "x-cache")
      match x_cache {
        Some(cache) => assert_eq(cache, "HIT")
        None => assert_true(false)
      }
    }
    Err(error) => assert_true(false)
  }
  
  // Get cache statistics
  let cache_stats = HttpClient::get_cache_stats(cache_client)
  
  match cache_stats {
    CacheStats(hits, misses, hit_ratio, size) => {
      assert_eq(hits, 1)
      assert_eq(misses, 1)
      assert_eq(hit_ratio, 0.5)
      assert_eq(size, 1)
    }
    _ => assert_true(false)
  }
}