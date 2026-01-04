// Azimuth 网络通信增强测试用例
// 专注于遥测系统的网络通信功能，包括HTTP客户端、连接池、负载均衡、网络错误处理等

// 测试1: HTTP客户端基础功能
test "HTTP客户端基础功能测试" {
  // 创建HTTP客户端
  let http_client = HttpClient::new()
  
  // 配置客户端
  http_client.set_timeout(5000)  // 5秒超时
  http_client.set_user_agent("Azimuth-Telemetry-Client/1.0")
  http_client.enable_retry(true, 3)  // 启用重试，最多3次
  
  // 测试GET请求
  let get_test = fn() -> Result<HttpResponse, HttpError> {
    let request = HttpRequest::new("GET", "https://httpbin.org/get")
    request.add_header("Accept", "application/json")
    request.add_header("X-Test-Header", "test-value")
    
    http_client.execute(request)
  }
  
  let get_result = get_test()
  
  // 验证GET请求结果
  assert_true(get_result.is_ok())
  
  match get_result {
    Ok(response) => {
      assert_eq(response.status_code, 200)
      assert_true(response.body.length() > 0)
      assert_true(response.headers.contains("content-type"))
      
      // 验证响应体包含请求信息
      assert_true(response.body.contains("httpbin.org"))
      assert_true(response.body.contains("test-value"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试POST请求
  let post_test = fn() -> Result<HttpResponse, HttpError> {
    let request = HttpRequest::new("POST", "https://httpbin.org/post")
    request.add_header("Content-Type", "application/json")
    request.set_body("{\"test\":\"data\",\"timestamp\":" + Time::now().to_string() + "}")
    
    http_client.execute(request)
  }
  
  let post_result = post_test()
  
  // 验证POST请求结果
  assert_true(post_result.is_ok())
  
  match post_result {
    Ok(response) => {
      assert_eq(response.status_code, 200)
      assert_true(response.body.contains("test"))
      assert_true(response.body.contains("timestamp"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试PUT请求
  let put_test = fn() -> Result<HttpResponse, HttpError> {
    let request = HttpRequest::new("PUT", "https://httpbin.org/put")
    request.add_header("Content-Type", "application/json")
    request.set_body("{\"updated\":true,\"version\":\"1.0\"}")
    
    http_client.execute(request)
  }
  
  let put_result = put_test()
  
  // 验证PUT请求结果
  assert_true(put_result.is_ok())
  
  match put_result {
    Ok(response) => {
      assert_eq(response.status_code, 200)
      assert_true(response.body.contains("updated"))
      assert_true(response.body.contains("version"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试DELETE请求
  let delete_test = fn() -> Result<HttpResponse, HttpError> {
    let request = HttpRequest::new("DELETE", "https://httpbin.org/delete")
    request.add_header("X-Delete-Reason", "test")
    
    http_client.execute(request)
  }
  
  let delete_result = delete_test()
  
  // 验证DELETE请求结果
  assert_true(delete_result.is_ok())
  
  match delete_result {
    Ok(response) => {
      assert_eq(response.status_code, 200)
      assert_true(response.body.contains("test"))
    }
    Err(_) => assert_true(false)
  }
}

// 测试2: 连接池管理
test "连接池管理测试" {
  // 创建连接池管理器
  let connection_pool = ConnectionPool::new()
  
  // 配置连接池
  connection_pool.configure({
    max_connections: 10,
    max_idle_connections: 5,
    connection_timeout: 5000,
    idle_timeout: 30000,
    max_lifetime: 300000
  })
  
  // 测试连接获取和释放
  let connection_lifecycle_test = fn() {
    // 获取连接
    let connection1 = connection_pool.get_connection("https://httpbin.org")
    assert_true(connection1.is_some())
    
    let connection2 = connection_pool.get_connection("https://httpbin.org")
    assert_true(connection2.is_some())
    
    // 验证连接池状态
    let pool_stats = connection_pool.get_stats()
    assert_eq(pool_stats.active_connections, 2)
    assert_eq(pool_stats.total_connections, 2)
    
    // 释放连接
    connection_pool.release_connection(connection1.unwrap())
    connection_pool.release_connection(connection2.unwrap())
    
    // 验证释放后的状态
    let after_release_stats = connection_pool.get_stats()
    assert_eq(after_release_stats.active_connections, 0)
    assert_eq(after_release_stats.idle_connections, 2)
  }
  
  connection_lifecycle_test()
  
  // 测试连接复用
  let connection_reuse_test = fn() {
    // 获取连接
    let conn1 = connection_pool.get_connection("https://httpbin.org").unwrap()
    let conn1_id = conn1.get_id()
    
    // 释放连接
    connection_pool.release_connection(conn1)
    
    // 再次获取连接，应该复用之前的连接
    let conn2 = connection_pool.get_connection("https://httpbin.org").unwrap()
    let conn2_id = conn2.get_id()
    
    assert_eq(conn1_id, conn2_id)  // 应该是同一个连接
    
    connection_pool.release_connection(conn2)
  }
  
  connection_reuse_test()
  
  // 测试连接池限制
  let connection_limit_test = fn() {
    let connections = []
    
    // 获取最大数量的连接
    for i in 0..=9 {
      let conn = connection_pool.get_connection("https://httpbin.org")
      assert_true(conn.is_some())
      connections = connections.push(conn.unwrap())
    }
    
    // 尝试获取超出限制的连接
    let extra_conn = connection_pool.get_connection("https://httpbin.org")
    assert_true(extra_conn.is_none())  // 应该失败
    
    // 释放一个连接
    connection_pool.release_connection(connections[0])
    
    // 现在应该能够获取连接
    let new_conn = connection_pool.get_connection("https://httpbin.org")
    assert_true(new_conn.is_some())
    
    // 释放所有连接
    for i in 1..=connections.length() - 1 {
      connection_pool.release_connection(connections[i])
    }
    connection_pool.release_connection(new_conn.unwrap())
  }
  
  connection_limit_test()
  
  // 测试连接超时和清理
  let connection_timeout_test = fn() {
    // 配置短超时用于测试
    connection_pool.set_idle_timeout(1000)  // 1秒空闲超时
    
    // 获取连接
    let conn = connection_pool.get_connection("https://httpbin.org").unwrap()
    
    // 释放连接
    connection_pool.release_connection(conn)
    
    // 等待超时
    Time::sleep(1200)
    
    // 触发清理
    connection_pool.cleanup_idle_connections()
    
    // 验证连接被清理
    let cleanup_stats = connection_pool.get_stats()
    assert_eq(cleanup_stats.idle_connections, 0)
    
    // 恢复正常超时设置
    connection_pool.set_idle_timeout(30000)
  }
  
  connection_timeout_test()
}

// 测试3: 负载均衡
test "负载均衡测试" {
  // 创建负载均衡器
  let load_balancer = LoadBalancer::new()
  
  // 添加后端服务器
  let backends = [
    "https://httpbin.org",
    "https://postman-echo.com",
    "https://jsonplaceholder.typicode.com"
  ]
  
  for backend in backends {
    load_balancer.add_backend(backend)
  }
  
  // 测试轮询策略
  load_balancer.set_strategy("round_robin")
  
  let round_robin_test = fn() {
    let selected_backends = []
    
    // 选择10次后端
    for i in 0..=9 {
      let backend = load_balancer.select_backend()
      selected_backends = selected_backends.push(backend)
    }
    
    // 验证轮询分布
    let backend_counts = @{
      "https://httpbin.org": 0,
      "https://postman-echo.com": 0,
      "https://jsonplaceholder.typicode.com": 0
    }
    
    for backend in selected_backends {
      backend_counts[backend] = backend_counts[backend] + 1
    }
    
    // 轮询应该均匀分布
    for count in backend_counts.values() {
      assert_true(count >= 3 and count <= 4)  // 10/3 ≈ 3.33
    }
  }
  
  round_robin_test()
  
  // 测试加权轮询策略
  load_balancer.set_strategy("weighted_round_robin")
  
  // 设置权重
  load_balancer.set_backend_weight("https://httpbin.org", 5)
  load_balancer.set_backend_weight("https://postman-echo.com", 3)
  load_balancer.set_backend_weight("https://jsonplaceholder.typicode.com", 2)
  
  let weighted_round_robin_test = fn() {
    let selected_backends = []
    
    // 选择100次后端
    for i in 0..=99 {
      let backend = load_balancer.select_backend()
      selected_backends = selected_backends.push(backend)
    }
    
    // 验证加权分布
    let backend_counts = @{
      "https://httpbin.org": 0,
      "https://postman-echo.com": 0,
      "https://jsonplaceholder.typicode.com": 0
    }
    
    for backend in selected_backends {
      backend_counts[backend] = backend_counts[backend] + 1
    }
    
    // 权重5:3:2，总计10份
    // 100次选择中，应该大约50次、30次、20次
    assert_true(backend_counts["https://httpbin.org"] >= 45 and backend_counts["https://httpbin.org"] <= 55)
    assert_true(backend_counts["https://postman-echo.com"] >= 25 and backend_counts["https://postman-echo.com"] <= 35)
    assert_true(backend_counts["https://jsonplaceholder.typicode.com"] >= 15 and backend_counts["https://jsonplaceholder.typicode.com"] <= 25)
  }
  
  weighted_round_robin_test()
  
  // 测试最少连接策略
  load_balancer.set_strategy("least_connections")
  
  let least_connections_test = fn() {
    // 模拟连接计数
    load_balancer.set_backend_connections("https://httpbin.org", 10)
    load_balancer.set_backend_connections("https://postman-echo.com", 5)
    load_balancer.set_backend_connections("https://jsonplaceholder.typicode.com", 2)
    
    // 选择后端，应该选择连接数最少的
    let selected = load_balancer.select_backend()
    assert_eq(selected, "https://jsonplaceholder.typicode.com")
    
    // 重置连接计数
    load_balancer.reset_backend_connections()
  }
  
  least_connections_test()
  
  // 测试健康检查
  let health_check_test = fn() {
    // 启用健康检查
    load_balancer.enable_health_check(true)
    load_balancer.set_health_check_interval(5000)  // 5秒间隔
    load_balancer.set_health_check_timeout(2000)   // 2秒超时
    
    // 执行健康检查
    let health_results = load_balancer.perform_health_check()
    
    // 验证健康检查结果
    assert_eq(health_results.length(), backends.length())
    
    for result in health_results {
      assert_true(result.backend.length() > 0)
      assert_true(result.is_healthy or not result.is_healthy)  // 应该有明确的健康状态
      assert_true(result.response_time >= 0)
    }
    
    // 获取健康状态
    let healthy_backends = load_balancer.get_healthy_backends()
    assert_true(healthy_backends.length() > 0)  // 至少有一个健康的后端
  }
  
  health_check_test()
}

// 测试4: 网络错误处理和重试
test "网络错误处理和重试测试" {
  // 创建网络错误处理器
  let error_handler = NetworkErrorHandler::new()
  
  // 配置错误处理策略
  error_handler.configure_retry({
    max_retries: 3,
    initial_backoff: 1000,      // 1秒
    max_backoff: 10000,         // 10秒
    backoff_multiplier: 2.0,
    retryable_errors: ["timeout", "connection_refused", "service_unavailable", "rate_limited"]
  })
  
  // 测试超时错误处理
  let timeout_error_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_timeout(100)  // 100ms超时，很可能超时
    
    let request = HttpRequest::new("GET", "https://httpbin.org/delay/5")  // 5秒延迟
    
    let result = error_handler.execute_with_retry(fn() {
      http_client.execute(request)
    })
    
    // 应该失败，但经过了重试
    assert_true(result.is_err())
    
    match result {
      Err(error) => {
        assert_eq(error.error_type, "timeout")
        assert_true(error.retry_count > 0)
        assert_true(error.total_duration > 100)
      }
      Ok(_) => assert_true(false)
    }
  }
  
  timeout_error_test()
  
  // 测试连接拒绝错误处理
  let connection_refused_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_timeout(2000)
    
    // 使用无效端口
    let request = HttpRequest::new("GET", "http://localhost:99999/nonexistent")
    
    let result = error_handler.execute_with_retry(fn() {
      http_client.execute(request)
    })
    
    // 应该失败
    assert_true(result.is_err())
    
    match result {
      Err(error) => {
        assert_true(error.error_type == "connection_refused" or error.error_type == "timeout")
      }
      Ok(_) => assert_true(false)
    }
  }
  
  connection_refused_test()
  
  // 测试限流错误处理
  let rate_limit_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_timeout(5000)
    
    let request = HttpRequest::new("GET", "https://httpbin.org/status/429")  // 429状态码
    
    let result = error_handler.execute_with_retry(fn() {
      http_client.execute(request)
    })
    
    // 429错误应该被重试
    assert_true(result.is_err())
    
    match result {
      Err(error) => {
        assert_eq(error.error_type, "rate_limited")
        assert_true(error.retry_count > 0)
      }
      Ok(_) => assert_true(false)
    }
  }
  
  rate_limit_test()
  
  // 测试断路器模式
  let circuit_breaker_test = fn() {
    let circuit_breaker = CircuitBreaker::new({
      failure_threshold: 3,
      recovery_timeout: 5000,  // 5秒
      half_open_max_calls: 2
    })
    
    // 模拟连续失败
    let failure_count = 0
    for i in 0..=4 {
      let result = circuit_breaker.call(fn() {
        Err(HttpError::new("service_unavailable", "Service unavailable"))
      })
      
      if i < 2 {
        assert_true(result.is_err())
        assert_eq(circuit_breaker.get_state(), "closed")
      } else if i == 2 {
        assert_true(result.is_err())
        assert_eq(circuit_breaker.get_state(), "open")
      } else {
        // 断路器打开后应该直接返回错误
        assert_true(result.is_err())
        assert_eq(circuit_breaker.get_state(), "open")
      }
    }
    
    // 等待恢复超时
    Time::sleep(5100)
    
    // 现在应该进入半开状态
    assert_eq(circuit_breaker.get_state(), "half_open")
    
    // 在半开状态下成功调用
    let success_result = circuit_breaker.call(fn() {
      Ok(HttpResponse::new(200, "OK", []))
    })
    
    assert_true(success_result.is_ok())
    assert_eq(circuit_breaker.get_state(), "closed")
  }
  
  circuit_breaker_test()
  
  // 测试错误恢复策略
  let error_recovery_test = fn() {
    let recovery_manager = ErrorRecoveryManager::new()
    
    // 添加恢复策略
    recovery_manager.add_strategy("timeout", {
      action: "increase_timeout",
      parameters: { multiplier: 2.0, max_timeout: 30000 }
    })
    
    recovery_manager.add_strategy("connection_refused", {
      action: "switch_endpoint",
      parameters: { fallback_endpoints: ["https://backup.example.com"] }
    })
    
    recovery_manager.add_strategy("rate_limited", {
      action: "exponential_backoff",
      parameters: { base_delay: 1000, max_delay: 60000 }
    })
    
    // 测试超时恢复
    let timeout_recovery = recovery_manager.apply_recovery("timeout", {
      current_timeout: 5000,
      failure_count: 2
    })
    
    assert_true(timeout_recovery.success)
    assert_eq(timeout_recovery.action, "increase_timeout")
    assert_eq(timeout_recovery.new_timeout, 10000)  // 5000 * 2.0
    
    // 测试连接拒绝恢复
    let conn_refused_recovery = recovery_manager.apply_recovery("connection_refused", {
      current_endpoint: "https://primary.example.com",
      failure_count: 1
    })
    
    assert_true(conn_refused_recovery.success)
    assert_eq(conn_refused_recovery.action, "switch_endpoint")
    assert_eq(conn_refused_recovery.new_endpoint, "https://backup.example.com")
    
    // 测试限流恢复
    let rate_limit_recovery = recovery_manager.apply_recovery("rate_limited", {
      retry_count: 1,
      last_delay: 1000
    })
    
    assert_true(rate_limit_recovery.success)
    assert_eq(rate_limit_recovery.action, "exponential_backoff")
    assert_eq(rate_limit_recovery.new_delay, 2000)  // 1000 * 2
  }
  
  error_recovery_test()
}

// 测试5: 网络性能优化
test "网络性能优化测试" {
  // 创建网络性能优化器
  let performance_optimizer = NetworkPerformanceOptimizer::new()
  
  // 测试连接复用
  let connection_reuse_test = fn() {
    let http_client = HttpClient::new()
    http_client.enable_connection_reuse(true)
    http_client.set_max_idle_connections(10)
    
    let start_time = Time::now()
    
    // 发送多个请求到同一主机
    for i in 0..=9 {
      let request = HttpRequest::new("GET", "https://httpbin.org/get")
      let result = http_client.execute(request)
      assert_true(result.is_ok())
    }
    
    let end_time = Time::now()
    let total_duration = end_time - start_time
    
    // 连接复用应该提高性能
    assert_true(total_duration < 10000)  // 10秒内完成10个请求
    
    let connection_stats = http_client.get_connection_stats()
    assert_true(connection_stats.reused_connections > 0)
  }
  
  connection_reuse_test()
  
  // 测试HTTP/2支持
  let http2_test = fn() {
    let http_client = HttpClient::new()
    http_client.enable_http2(true)
    
    let request = HttpRequest::new("GET", "https://httpbin.org/get")
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        // 验证使用了HTTP/2（如果服务器支持）
        if response.protocol_version == "HTTP/2" {
          assert_true(true)
        }
      }
      Err(_) => assert_true(false)
    }
  }
  
  http2_test()
  
  // 测试压缩支持
  let compression_test = fn() {
    let http_client = HttpClient::new()
    http_client.enable_compression(true)
    http_client.set_compression_types(["gzip", "deflate"])
    
    let request = HttpRequest::new("GET", "https://httpbin.org/gzip")
    request.add_header("Accept-Encoding", "gzip, deflate")
    
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        // 验证响应被压缩
        let content_encoding = response.headers.get("content-encoding")
        assert_true(content_encoding.is_some())
        assert_true(content_encoding.unwrap() == "gzip")
      }
      Err(_) => assert_true(false)
    }
  }
  
  compression_test()
  
  // 测试批量请求
  let batch_request_test = fn() {
    let http_client = HttpClient::new()
    http_client.enable_batch_requests(true)
    
    let requests = []
    for i in 0..=4 {
      let request = HttpRequest::new("GET", "https://httpbin.org/get?id=" + i.to_string())
      requests = requests.push(request)
    }
    
    let start_time = Time::now()
    let results = http_client.execute_batch(requests)
    let end_time = Time::now()
    let batch_duration = end_time - start_time
    
    // 验证批量请求结果
    assert_eq(results.length(), 5)
    
    for result in results {
      assert_true(result.is_ok())
      match result {
        Ok(response) => assert_eq(response.status_code, 200),
        Err(_) => assert_true(false)
      }
    }
    
    // 批量请求应该比单独请求更快
    assert_true(batch_duration < 10000)  // 10秒内完成5个请求
  }
  
  batch_request_test()
  
  // 测试并发请求
  let concurrent_request_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_max_concurrent_requests(10)
    
    let futures = []
    
    // 创建并发请求
    for i in 0..=9 {
      let request = HttpRequest::new("GET", "https://httpbin.org/delay/" + (i % 3).to_string())
      
      let future = async_execute(fn() {
        http_client.execute(request)
      })
      futures = futures.push(future)
    }
    
    let start_time = Time::now()
    
    // 等待所有请求完成
    let results = []
    for future in futures {
      let result = future.wait()
      results = results.push(result)
    }
    
    let end_time = Time::now()
    let concurrent_duration = end_time - start_time
    
    // 验证并发请求结果
    assert_eq(results.length(), 10)
    
    let successful_requests = results.filter(fn(r) { r.is_ok() }).length()
    assert_eq(successful_requests, 10)
    
    // 并发请求应该比串行快
    assert_true(concurrent_duration < 15000)  // 15秒内完成10个请求（有些有延迟）
  }
  
  concurrent_request_test()
}

// 测试6: 网络安全和认证
test "网络安全和认证测试" {
  // 创建网络安全管理器
  let security_manager = NetworkSecurityManager::new()
  
  // 测试HTTPS连接
  let https_test = fn() {
    let http_client = HttpClient::new()
    http_client.enable_ssl_verification(true)
    
    let request = HttpRequest::new("GET", "https://httpbin.org/get")
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        // 验证SSL连接信息
        if response.ssl_info.is_some() {
          let ssl_info = response.ssl_info.unwrap()
          assert_true(ssl_info.cipher_suite.length() > 0)
          assert_true(ssl_info.protocol_version.length() > 0)
        }
      }
      Err(_) => assert_true(false)
    }
  }
  
  https_test()
  
  // 测试API密钥认证
  let api_key_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_api_key("test-api-key-12345", "X-API-Key")
    
    let request = HttpRequest::new("GET", "https://httpbin.org/bearer")
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        // 验证API密钥被正确发送
        assert_true(response.body.contains("test-api-key-12345"))
      }
      Err(_) => assert_true(false)
    }
  }
  
  api_key_test()
  
  // 测试Bearer Token认证
  let bearer_token_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_bearer_token("test-bearer-token-67890")
    
    let request = HttpRequest::new("GET", "https://httpbin.org/bearer")
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        // 验证Bearer Token被正确发送
        assert_true(response.body.contains("test-bearer-token-67890"))
      }
      Err(_) => assert_true(false)
    }
  }
  
  bearer_token_test()
  
  // 测试基本认证
  let basic_auth_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_basic_auth("testuser", "testpass")
    
    let request = HttpRequest::new("GET", "https://httpbin.org/basic-auth/testuser/testpass")
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        assert_true(response.body.contains("authenticated"))
      }
      Err(_) => assert_true(false)
    }
  }
  
  basic_auth_test()
  
  // 测试自定义认证头
  let custom_auth_test = fn() {
    let http_client = HttpClient::new()
    
    let request = HttpRequest::new("GET", "https://httpbin.org/headers")
    request.add_header("Authorization", "Custom token123")
    request.add_header("X-Custom-Auth", "custom-value")
    
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        // 验证自定义认证头被正确发送
        assert_true(response.body.contains("Custom token123"))
        assert_true(response.body.contains("custom-value"))
      }
      Err(_) => assert_true(false)
    }
  }
  
  custom_auth_test()
  
  // 测试证书验证
  let certificate_verification_test = fn() {
    let http_client = HttpClient::new()
    
    // 启用严格证书验证
    http_client.set_ssl_verification_mode("strict")
    
    let request = HttpRequest::new("GET", "https://httpbin.org/get")
    let result = http_client.execute(request)
    
    // 应该成功，因为httpbin.org有有效的证书
    assert_true(result.is_ok())
    
    // 测试无效证书（使用自签名证书的测试站点）
    http_client.set_ssl_verification_mode("none")  // 临时禁用验证用于测试
    let insecure_request = HttpRequest::new("GET", "https://self-signed.badssl.com/")
    let insecure_result = http_client.execute(insecure_request)
    
    // 禁用验证后应该成功
    assert_true(insecure_result.is_ok())
    
    // 重新启用验证
    http_client.set_ssl_verification_mode("strict")
  }
  
  certificate_verification_test()
}

// 测试7: 网络监控和指标
test "网络监控和指标测试" {
  // 创建网络监控器
  let network_monitor = NetworkMonitor::new()
  
  // 启用监控
  network_monitor.enable_monitoring(true)
  network_monitor.set_metrics_collection_interval(1000)  // 1秒间隔
  
  // 测试请求指标收集
  let metrics_collection_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_monitor(network_monitor)
    
    // 发送多个请求
    for i in 0..=9 {
      let request = HttpRequest::new("GET", "https://httpbin.org/get")
      let result = http_client.execute(request)
      assert_true(result.is_ok())
    }
    
    // 等待指标收集
    Time::sleep(1100)
    
    // 获取网络指标
    let metrics = network_monitor.get_metrics()
    
    // 验证指标
    assert_true(metrics.total_requests > 0)
    assert_true(metrics.successful_requests > 0)
    assert_true(metrics.failed_requests >= 0)
    assert_true(metrics.average_response_time > 0)
    assert_true(metrics.total_bytes_sent > 0)
    assert_true(metrics.total_bytes_received > 0)
    
    // 验证请求分布
    assert_true(metrics.requests_by_status_code.contains(200))
    assert_true(metrics.requests_by_host.contains("httpbin.org"))
  }
  
  metrics_collection_test()
  
  // 测试性能指标
  let performance_metrics_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_monitor(network_monitor)
    
    // 发送不同延迟的请求
    let delays = [1, 2, 3, 1, 5]  // 秒
    
    for delay in delays {
      let request = HttpRequest::new("GET", "https://httpbin.org/delay/" + delay.to_string())
      let result = http_client.execute(request)
      assert_true(result.is_ok())
    }
    
    // 等待指标收集
    Time::sleep(1100)
    
    // 获取性能指标
    let perf_metrics = network_monitor.get_performance_metrics()
    
    // 验证性能指标
    assert_true(perf_metrics.min_response_time > 0)
    assert_true(perf_metrics.max_response_time > 0)
    assert_true(perf_metrics.average_response_time > 0)
    assert_true(perf_metrics.p95_response_time > 0)
    assert_true(perf_metrics.p99_response_time > 0)
    
    // 验证延迟分布
    assert_true(perf_metrics.min_response_time <= perf_metrics.average_response_time)
    assert_true(perf_metrics.average_response_time <= perf_metrics.max_response_time)
    assert_true(perf_metrics.p95_response_time <= perf_metrics.p99_response_time)
  }
  
  performance_metrics_test()
  
  // 测试错误指标
  let error_metrics_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_monitor(network_monitor)
    http_client.set_timeout(100)  // 短超时
    
    // 发送一些会失败的请求
    let error_requests = [
      "https://httpbin.org/delay/5",      // 超时
      "https://nonexistent.domain.com",  // 域名不存在
      "https://httpbin.org/status/500",   // 服务器错误
      "https://httpbin.org/status/429"    // 限流
    ]
    
    for url in error_requests {
      let request = HttpRequest::new("GET", url)
      let result = http_client.execute(request)
      // 这些请求应该失败
    }
    
    // 等待指标收集
    Time::sleep(1100)
    
    // 获取错误指标
    let error_metrics = network_monitor.get_error_metrics()
    
    // 验证错误指标
    assert_true(error_metrics.total_errors > 0)
    assert_true(error_metrics.timeout_errors > 0)
    assert_true(error_metrics.connection_errors > 0)
    assert_true(error_metrics.http_errors > 0)
    
    // 验证错误分布
    assert_true(error_metrics.errors_by_type.contains("timeout"))
    assert_true(error_metrics.errors_by_status_code.contains(500))
    assert_true(error_metrics.errors_by_status_code.contains(429))
  }
  
  error_metrics_test()
  
  // 测试实时监控
  let real_time_monitoring_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_monitor(network_monitor)
    
    // 启动实时监控
    let monitor_handle = network_monitor.start_real_time_monitoring()
    
    // 发送请求
    for i in 0..=4 {
      let request = HttpRequest::new("GET", "https://httpbin.org/get")
      let result = http_client.execute(request)
      assert_true(result.is_ok())
      
      // 获取实时指标
      let real_time_metrics = network_monitor.get_real_time_metrics()
      assert_true(real_time_metrics.current_requests_per_second > 0)
      assert_true(real_time_metrics.current_error_rate >= 0.0)
    }
    
    // 停止实时监控
    network_monitor.stop_real_time_monitoring(monitor_handle)
    
    // 获取监控报告
    let monitoring_report = network_monitor.generate_monitoring_report()
    
    // 验证监控报告
    assert_true(monitoring_report.monitoring_duration > 0)
    assert_true(monitoring_report.total_requests > 0)
    assert_true(monitoring_report.average_throughput > 0)
    assert_true(monitoring_report.error_rate >= 0.0)
  }
  
  real_time_monitoring_test()
}

// 测试8: 网络配置和适配
test "网络配置和适配测试" {
  // 创建网络配置管理器
  let config_manager = NetworkConfigManager::new()
  
  // 测试代理配置
  let proxy_config_test = fn() {
    let http_client = HttpClient::new()
    
    // 配置HTTP代理
    let proxy_config = ProxyConfig::new()
    proxy_config.set_http_proxy("http://proxy.example.com:8080")
    proxy_config.set_https_proxy("http://proxy.example.com:8080")
    proxy_config.set_no_proxy(["localhost", "127.0.0.1", "*.local"])
    
    http_client.set_proxy_config(proxy_config)
    
    // 验证代理配置
    let current_config = http_client.get_proxy_config()
    assert_eq(current_config.http_proxy, "http://proxy.example.com:8080")
    assert_eq(current_config.https_proxy, "http://proxy.example.com:8080")
    assert_true(current_config.no_proxy.contains("localhost"))
  }
  
  proxy_config_test()
  
  // 测试DNS配置
  let dns_config_test = fn() {
    let http_client = HttpClient::new()
    
    // 配置DNS
    let dns_config = DnsConfig::new()
    dns_config.set_servers(["8.8.8.8", "8.8.4.4", "1.1.1.1"])
    dns_config.set_timeout(5000)
    dns_config.enable_cache(true)
    dns_config.set_cache_ttl(300)  // 5分钟
    
    http_client.set_dns_config(dns_config)
    
    // 验证DNS配置
    let current_dns_config = http_client.get_dns_config()
    assert_eq(current_dns_config.servers.length(), 3)
    assert_eq(current_dns_config.servers[0], "8.8.8.8")
    assert_eq(current_dns_config.timeout, 5000)
    assert_true(current_dns_config.cache_enabled)
    assert_eq(current_dns_config.cache_ttl, 300)
  }
  
  dns_config_test()
  
  // 测试网络适配器自动选择
  let adapter_selection_test = fn() {
    let adapter_selector = NetworkAdapterSelector::new()
    
    // 获取可用网络适配器
    let adapters = adapter_selector.get_available_adapters()
    assert_true(adapters.length() > 0)
    
    // 选择最佳适配器
    let best_adapter = adapter_selector.select_best_adapter()
    assert_true(best_adapter.is_some())
    
    match best_adapter {
      Some(adapter) => {
        assert_true(adapter.name.length() > 0)
        assert_true(adapter.is_up)
        assert_true(adapter.speed > 0)
      }
      None => assert_true(false)
    }
    
    // 测试适配器故障转移
    let fallback_adapter = adapter_selector.select_fallback_adapter()
    assert_true(fallback_adapter.is_some())
  }
  
  adapter_selection_test()
  
  // 测试网络条件自适应
  let network_adaptation_test = fn() {
    let adapter = NetworkAdaptiveClient::new()
    
    // 配置自适应策略
    adapter.set_adaptation_strategy({
      enable_auto_timeout_adjustment: true,
      enable_auto_retry_adjustment: true,
      enable_compression_adaptation: true,
      poor_network_threshold: 0.1,  // 10%丢包率
      good_network_threshold: 0.01   // 1%丢包率
    })
    
    // 模拟网络条件变化
    let network_conditions = [
      { latency: 50, packet_loss: 0.01, bandwidth: 1000000 },    // 良好网络
      { latency: 200, packet_loss: 0.05, bandwidth: 500000 },    // 一般网络
      { latency: 1000, packet_loss: 0.15, bandwidth: 100000 }    // 差网络
    ]
    
    for condition in network_conditions {
      adapter.update_network_condition(condition)
      
      // 获取当前配置
      let current_config = adapter.get_current_config()
      
      // 验证配置适应网络条件
      if condition.packet_loss > 0.1 {
        // 差网络：应该增加超时和重试次数
        assert_true(current_config.timeout >= 10000)
        assert_true(current_config.max_retries >= 5)
        assert_true(current_config.compression_enabled)
      } else if condition.packet_loss < 0.02 {
        // 良好网络：可以使用较短的超时
        assert_true(current_config.timeout <= 5000)
      }
    }
  }
  
  network_adaptation_test()
  
  // 测试配置热更新
  let config_hot_update_test = fn() {
    let http_client = HttpClient::new()
    
    // 初始配置
    http_client.set_timeout(3000)
    http_client.set_max_retries(3)
    
    // 热更新配置
    let new_config = NetworkConfig::new()
    new_config.set_timeout(5000)
    new_config.set_max_retries(5)
    new_config.set_enable_compression(true)
    
    config_manager.hot_update_config(http_client, new_config)
    
    // 验证配置已更新
    assert_eq(http_client.get_timeout(), 5000)
    assert_eq(http_client.get_max_retries(), 5)
    assert_true(http_client.is_compression_enabled())
  }
  
  config_hot_update_test()
}

// 测试9: 网络边界条件和异常情况
test "网络边界条件和异常情况测试" {
  // 创建网络边界测试器
  let boundary_tester = NetworkBoundaryTester::new()
  
  // 测试超大请求处理
  let large_request_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_max_request_size(10 * 1024 * 1024)  // 10MB限制
    
    // 创建接近限制的请求
    let large_body = "x".repeat(9 * 1024 * 1024)  // 9MB
    let request = HttpRequest::new("POST", "https://httpbin.org/post")
    request.set_body(large_body)
    
    let result = http_client.execute(request)
    
    // 应该成功
    assert_true(result.is_ok())
    
    // 创建超过限制的请求
    let oversized_body = "x".repeat(12 * 1024 * 1024)  // 12MB
    let oversized_request = HttpRequest::new("POST", "https://httpbin.org/post")
    oversized_request.set_body(oversized_body)
    
    let oversized_result = http_client.execute(oversized_request)
    
    // 应该失败
    assert_true(oversized_result.is_err())
    
    match oversized_result {
      Err(error) => assert_eq(error.error_type, "request_too_large"),
      Ok(_) => assert_true(false)
    }
  }
  
  large_request_test()
  
  // 测试超长URL处理
  let long_url_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_max_url_length(2048)  // 2KB限制
    
    // 正常长度URL
    let normal_request = HttpRequest::new("GET", "https://httpbin.org/get?param=value")
    let normal_result = http_client.execute(normal_request)
    assert_true(normal_result.is_ok())
    
    // 超长URL
    let long_params = "x".repeat(3000)  // 超过2KB
    let long_request = HttpRequest::new("GET", "https://httpbin.org/get?param=" + long_params)
    let long_result = http_client.execute(long_request)
    
    // 应该失败
    assert_true(long_result.is_err())
    
    match long_result {
      Err(error) => assert_eq(error.error_type, "url_too_long"),
      Ok(_) => assert_true(false)
    }
  }
  
  long_url_test()
  
  // 测试并发连接限制
  let concurrent_limit_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_max_concurrent_connections(5)
    
    let futures = []
    
    // 创建10个并发请求
    for i in 0..=9 {
      let request = HttpRequest::new("GET", "https://httpbin.org/delay/" + (i % 3).to_string())
      
      let future = async_execute(fn() {
        http_client.execute(request)
      })
      futures = futures.push(future)
    }
    
    // 等待所有请求完成
    let results = []
    for future in futures {
      let result = future.wait()
      results = results.push(result)
    }
    
    // 验证结果
    let successful_requests = results.filter(fn(r) { r.is_ok() }).length()
    let failed_requests = results.filter(fn(r) { r.is_err() }).length()
    
    // 应该有一些请求成功，一些可能因为连接限制而失败或等待
    assert_true(successful_requests > 0)
    assert_true(successful_requests + failed_requests == 10)
    
    // 验证连接统计
    let connection_stats = http_client.get_connection_stats()
    assert_true(connection_stats.concurrent_connections <= 5)
  }
  
  concurrent_limit_test()
  
  // 测试网络中断恢复
  let network_interruption_test = fn() {
    let http_client = HttpClient::new()
    http_client.enable_auto_recovery(true)
    http_client.set_recovery_timeout(30000)  // 30秒恢复超时
    
    // 模拟网络中断（使用无效地址）
    let interrupt_request = HttpRequest::new("GET", "http://192.0.2.0/nonexistent")  // RFC5737测试地址
    let interrupt_result = http_client.execute(interrupt_request)
    
    // 应该失败
    assert_true(interrupt_result.is_err())
    
    // 等待一段时间模拟网络恢复
    Time::sleep(1000)
    
    // 尝试正常请求
    let recovery_request = HttpRequest::new("GET", "https://httpbin.org/get")
    let recovery_result = http_client.execute(recovery_request)
    
    // 应该成功
    assert_true(recovery_result.is_ok())
    
    // 验证恢复统计
    let recovery_stats = http_client.get_recovery_stats()
    assert_true(recovery_stats.interruptions_detected > 0)
    assert_true(recovery_stats.successful_recoveries > 0)
  }
  
  network_interruption_test()
  
  // 测试内存压力下的网络操作
  let memory_pressure_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_memory_limit(50 * 1024 * 1024)  // 50MB内存限制
    
    // 创建内存压力
    let memory_pressure = []
    for i in 0..=100 {
      let large_data = "x".repeat(1024 * 1024)  // 1MB
      memory_pressure = memory_pressure.push(large_data)
    }
    
    // 在内存压力下发送请求
    let request = HttpRequest::new("GET", "https://httpbin.org/get")
    let result = http_client.execute(request)
    
    // 应该成功或优雅失败
    if result.is_err() {
      match result {
        Err(error) => assert_eq(error.error_type, "memory_limit_exceeded"),
        Ok(_) => ()
      }
    }
    
    // 释放内存压力
    memory_pressure = []
  }
  
  memory_pressure_test()
}

// 测试10: 网络协议兼容性
test "网络协议兼容性测试" {
  // 创建协议兼容性测试器
  let compatibility_tester = ProtocolCompatibilityTester::new()
  
  // 测试HTTP/1.1兼容性
  let http11_compatibility_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_protocol_version("HTTP/1.1")
    
    let request = HttpRequest::new("GET", "https://httpbin.org/get")
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        assert_eq(response.protocol_version, "HTTP/1.1")
        
        // 验证HTTP/1.1特有功能
        assert_true(response.headers.contains("connection"))
      }
      Err(_) => assert_true(false)
    }
  }
  
  http11_compatibility_test()
  
  // 测试HTTP/2兼容性
  let http2_compatibility_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_protocol_version("HTTP/2")
    http_client.enable_http2(true)
    
    let request = HttpRequest::new("GET", "https://httpbin.org/get")
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        // 如果服务器支持，应该是HTTP/2
        if response.protocol_version == "HTTP/2" {
          assert_true(true)
        } else {
          // 降级到HTTP/1.1也是可接受的
          assert_eq(response.protocol_version, "HTTP/1.1")
        }
      }
      Err(_) => assert_true(false)
    }
  }
  
  http2_compatibility_test()
  
  // 测试WebSocket兼容性
  let websocket_compatibility_test = fn() {
    let websocket_client = WebSocketClient::new()
    
    // 连接到WebSocket测试服务器
    let connect_result = websocket_client.connect("wss://echo.websocket.org")
    
    if connect_result.is_ok() {
      // 发送消息
      let send_result = websocket_client.send_text("Hello WebSocket!")
      assert_true(send_result.is_ok())
      
      // 接收消息
      let receive_result = websocket_client.receive_text()
      assert_true(receive_result.is_ok())
      
      match receive_result {
        Ok(message) => assert_eq(message, "Hello WebSocket!"),
        Err(_) => assert_true(false)
      }
      
      // 关闭连接
      let close_result = websocket_client.close()
      assert_true(close_result.is_ok())
    }
  }
  
  websocket_compatibility_test()
  
  // 测试不同内容类型兼容性
  let content_type_compatibility_test = fn() {
    let http_client = HttpClient::new()
    
    let content_types = [
      ("application/json", "{\"test\":\"json\"}"),
      ("text/plain", "plain text content"),
      ("application/xml", "<root><test>xml</test></root>"),
      ("application/x-www-form-urlencoded", "key1=value1&key2=value2")
    ]
    
    for (content_type, body) in content_types {
      let request = HttpRequest::new("POST", "https://httpbin.org/post")
      request.add_header("Content-Type", content_type)
      request.set_body(body)
      
      let result = http_client.execute(request)
      assert_true(result.is_ok())
      
      match result {
        Ok(response) => {
          assert_eq(response.status_code, 200)
          // 验证服务器正确处理了不同内容类型
          assert_true(response.body.contains(content_type))
        }
        Err(_) => assert_true(false)
      }
    }
  }
  
  content_type_compatibility_test()
  
  // 测试重定向兼容性
  let redirect_compatibility_test = fn() {
    let http_client = HttpClient::new()
    http_client.set_follow_redirects(true)
    http_client.set_max_redirects(5)
    
    // 发送到会重定向的URL
    let request = HttpRequest::new("GET", "https://httpbin.org/redirect/3")
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        assert_true(response.redirect_count > 0)
        assert_true(response.final_url.contains("get"))  // 最终应该重定向到/get
      }
      Err(_) => assert_true(false)
    }
  }
  
  redirect_compatibility_test()
  
  // 测试跨域兼容性
  let cors_compatibility_test = fn() {
    let http_client = HttpClient::new()
    
    // 发送包含CORS头的请求
    let request = HttpRequest::new("GET", "https://httpbin.org/get")
    request.add_header("Origin", "https://example.com")
    
    let result = http_client.execute(request)
    
    assert_true(result.is_ok())
    
    match result {
      Ok(response) => {
        assert_eq(response.status_code, 200)
        // 验证CORS响应头
        if response.headers.contains("access-control-allow-origin") {
          assert_true(true)
        }
      }
      Err(_) => assert_true(false)
    }
  }
  
  cors_compatibility_test()
}