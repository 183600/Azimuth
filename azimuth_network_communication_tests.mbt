// Azimuth 网络通信和连接池测试用例
// 专注于网络通信功能和连接池管理的测试

// 测试1: 连接池基本功能
test "连接池基本功能测试" {
  // 创建连接池管理器
  let connection_pool = ConnectionPool::new({
    max_connections: 10,
    min_connections: 2,
    connection_timeout: 5000,
    idle_timeout: 30000,
    max_lifetime: 300000
  })
  
  // 验证连接池初始状态
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 0)
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 0)
  assert_eq(ConnectionPool::get_total_connections(connection_pool), 0)
  
  // 获取连接
  let connection1 = ConnectionPool::get_connection(connection_pool, "telemetry-server:8080")
  assert_true(connection1 != None)
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 1)
  assert_eq(ConnectionPool::get_total_connections(connection_pool), 1)
  
  // 获取第二个连接
  let connection2 = ConnectionPool::get_connection(connection_pool, "telemetry-server:8080")
  assert_true(connection2 != None)
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 2)
  assert_eq(ConnectionPool::get_total_connections(connection_pool), 2)
  
  // 释放连接
  ConnectionPool::release_connection(connection_pool, connection1)
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 1)
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 1)
  assert_eq(ConnectionPool::get_total_connections(connection_pool), 2)
  
  // 再次获取连接，应该重用已释放的连接
  let connection3 = ConnectionPool::get_connection(connection_pool, "telemetry-server:8080")
  assert_true(connection3 != None)
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 2)
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 0)
  assert_eq(ConnectionPool::get_total_connections(connection_pool), 2)
  
  // 释放所有连接
  ConnectionPool::release_connection(connection_pool, connection2)
  ConnectionPool::release_connection(connection_pool, connection3)
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 0)
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 2)
}

// 测试2: 连接池容量限制
test "连接池容量限制测试" {
  // 创建小型连接池
  let small_pool = ConnectionPool::new({
    max_connections: 3,
    min_connections: 1,
    connection_timeout: 1000,
    idle_timeout: 60000,
    max_lifetime: 300000
  })
  
  // 获取最大数量的连接
  let conn1 = ConnectionPool::get_connection(small_pool, "server1:8080")
  let conn2 = ConnectionPool::get_connection(small_pool, "server1:8080")
  let conn3 = ConnectionPool::get_connection(small_pool, "server1:8080")
  
  assert_true(conn1 != None)
  assert_true(conn2 != None)
  assert_true(conn3 != None)
  assert_eq(ConnectionPool::get_total_connections(small_pool), 3)
  
  // 尝试获取超出限制的连接
  let conn4 = ConnectionPool::get_connection(small_pool, "server1:8080")
  assert_true(conn4 == None)  // 应该返回None，因为达到最大连接数
  
  // 释放一个连接
  ConnectionPool::release_connection(small_pool, conn1)
  
  // 现在应该能够获取连接
  let conn5 = ConnectionPool::get_connection(small_pool, "server1:8080")
  assert_true(conn5 != None)
  assert_eq(ConnectionPool::get_total_connections(small_pool), 3)
  
  // 清理
  ConnectionPool::release_connection(small_pool, conn2)
  ConnectionPool::release_connection(small_pool, conn3)
  ConnectionPool::release_connection(small_pool, conn5)
}

// 测试3: 多服务器连接池
test "多服务器连接池测试" {
  // 创建连接池
  let multi_server_pool = ConnectionPool::new({
    max_connections: 10,
    min_connections: 1,
    connection_timeout: 2000,
    idle_timeout: 60000,
    max_lifetime: 300000
  })
  
  // 连接到不同服务器
  let server1_conn1 = ConnectionPool::get_connection(multi_server_pool, "server1.example.com:8080")
  let server1_conn2 = ConnectionPool::get_connection(multi_server_pool, "server1.example.com:8080")
  let server2_conn1 = ConnectionPool::get_connection(multi_server_pool, "server2.example.com:8080")
  let server3_conn1 = ConnectionPool::get_connection(multi_server_pool, "server3.example.com:8080")
  
  // 验证连接状态
  assert_true(server1_conn1 != None)
  assert_true(server1_conn2 != None)
  assert_true(server2_conn1 != None)
  assert_true(server3_conn1 != None)
  assert_eq(ConnectionPool::get_total_connections(multi_server_pool), 4)
  
  // 检查每个服务器的连接数
  assert_eq(ConnectionPool::get_server_connections(multi_server_pool, "server1.example.com:8080"), 2)
  assert_eq(ConnectionPool::get_server_connections(multi_server_pool, "server2.example.com:8080"), 1)
  assert_eq(ConnectionPool::get_server_connections(multi_server_pool, "server3.example.com:8080"), 1)
  
  // 释放连接
  ConnectionPool::release_connection(multi_server_pool, server1_conn1)
  ConnectionPool::release_connection(multi_server_pool, server2_conn1)
  
  // 验证连接状态更新
  assert_eq(ConnectionPool::get_server_connections(multi_server_pool, "server1.example.com:8080"), 1)
  assert_eq(ConnectionPool::get_server_connections(multi_server_pool, "server2.example.com:8080"), 0)
  assert_eq(ConnectionPool::get_server_connections(multi_server_pool, "server3.example.com:8080"), 1)
  
  // 清理剩余连接
  ConnectionPool::release_connection(multi_server_pool, server1_conn2)
  ConnectionPool::release_connection(multi_server_pool, server3_conn1)
}

// 测试4: 连接健康检查
test "连接健康检查测试" {
  // 创建带健康检查的连接池
  let health_check_pool = ConnectionPool::new({
    max_connections: 5,
    min_connections: 1,
    connection_timeout: 2000,
    idle_timeout: 30000,
    max_lifetime: 300000,
    health_check_interval: 10000,
    health_check_timeout: 5000
  })
  
  // 获取连接
  let healthy_conn = ConnectionPool::get_connection(health_check_pool, "healthy-server:8080")
  assert_true(healthy_conn != None)
  
  // 模拟健康检查
  let health_status1 = ConnectionPool::check_connection_health(health_check_pool, healthy_conn)
  assert_true(health_status1)
  
  // 模拟连接故障
  ConnectionPool::simulate_connection_failure(health_check_pool, healthy_conn)
  
  // 健康检查应该失败
  let health_status2 = ConnectionPool::check_connection_health(health_check_pool, healthy_conn)
  assert_false(health_status2)
  
  // 释放不健康的连接
  ConnectionPool::release_connection(health_check_pool, healthy_conn)
  
  // 连接池应该清理不健康的连接
  let cleaned_up = ConnectionPool::cleanup_unhealthy_connections(health_check_pool)
  assert_true(cleaned_up)
  
  // 获取新连接应该替换不健康的连接
  let new_conn = ConnectionPool::get_connection(health_check_pool, "healthy-server:8080")
  assert_true(new_conn != None)
  assert_true(new_conn != healthy_conn)  // 应该是新的连接
  
  // 新连接应该是健康的
  let health_status3 = ConnectionPool::check_connection_health(health_check_pool, new_conn)
  assert_true(health_status3)
  
  // 清理
  ConnectionPool::release_connection(health_check_pool, new_conn)
}

// 测试5: 网络重试机制
test "网络重试机制测试" {
  // 创建重试配置
  let retry_config = {
    max_retries: 3,
    initial_backoff: 1000,
    max_backoff: 10000,
    backoff_multiplier: 2.0,
    retryable_errors: ["timeout", "connection_refused", "network_unreachable"]
  }
  
  // 创建网络客户端
  let network_client = NetworkClient::new(retry_config)
  
  // 测试成功请求（不需要重试）
  let success_result = NetworkClient::send_request(network_client, {
    url: "http://reliable-server:8080/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"cpu.usage\", \"value\": 75.5}",
    timeout: 5000
  })
  
  assert_true(success_result.success)
  assert_eq(success_result.attempts, 1)
  
  // 测试可重试错误
  let retry_result = NetworkClient::send_request_with_failure_simulation(network_client, {
    url: "http://flaky-server:8080/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"memory.usage\", \"value\": 60.2}",
    timeout: 5000
  }, {
    failure_type: "timeout",
    failure_count: 2,  // 前两次失败
    succeed_on_attempt: 3  // 第三次成功
  })
  
  assert_true(retry_result.success)
  assert_eq(retry_result.attempts, 3)
  assert_true(retry_result.total_delay > 1000)  // 至少有初始退避延迟
  
  // 测试不可重试错误
  let non_retryable_result = NetworkClient::send_request_with_failure_simulation(network_client, {
    url: "http://server:8080/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"disk.usage\", \"value\": 85.0}",
    timeout: 5000
  }, {
    failure_type: "authentication_error",  // 不可重试错误
    failure_count: 1,
    succeed_on_attempt: 0
  })
  
  assert_false(non_retryable_result.success)
  assert_eq(non_retryable_result.attempts, 1)  // 不应该重试
  
  // 测试超过最大重试次数
  let max_retry_result = NetworkClient::send_request_with_failure_simulation(network_client, {
    url: "http://unavailable-server:8080/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"network.usage\", \"value\": 95.0}",
    timeout: 5000
  }, {
    failure_type: "connection_refused",
    failure_count: 5,  // 超过最大重试次数
    succeed_on_attempt: 0
  })
  
  assert_false(max_retry_result.success)
  assert_eq(max_retry_result.attempts, 3)  // 达到最大重试次数
}

// 测试6: 网络超时处理
test "网络超时处理测试" {
  // 创建带超时配置的客户端
  let timeout_client = NetworkClient::new({
    max_retries: 2,
    initial_backoff: 500,
    max_backoff: 5000,
    backoff_multiplier: 2.0,
    retryable_errors: ["timeout"]
  })
  
  // 测试连接超时
  let connect_timeout_result = timeout_client.send_request_with_timeout_simulation({
    url: "http://slow-server:8080/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"response.time\", \"value\": 200.0}",
    timeout: 1000  // 1秒超时
  }, {
    timeout_type: "connect",
    delay_ms: 2000  // 2秒延迟，超过超时时间
  })
  
  assert_false(connect_timeout_result.success)
  assert_eq(connect_timeout_result.error_type, "connect_timeout")
  
  // 测试读取超时
  let read_timeout_result = timeout_client.send_request_with_timeout_simulation({
    url: "http://slow-response-server:8080/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"processing.time\", \"value\": 150.0}",
    timeout: 1500  // 1.5秒超时
  }, {
    timeout_type: "read",
    delay_ms: 3000  // 3秒延迟，超过超时时间
  })
  
  assert_false(read_timeout_result.success)
  assert_eq(read_timeout_result.error_type, "read_timeout")
  
  // 测试部分超时恢复
  let partial_timeout_result = timeout_client.send_request_with_timeout_simulation({
    url: "http://inconsistent-server:8080/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"availability\", \"value\": 99.9}",
    timeout: 2000  // 2秒超时
  }, {
    timeout_type: "intermittent",
    failure_pattern: [true, false, true, false, true],  // 成功、失败交替
    delay_ms: 3000
  })
  
  assert_true(partial_timeout_result.success)
  assert_true(partial_timeout_result.attempts > 1)
}

// 测试7: 网络负载均衡
test "网络负载均衡测试" {
  // 创建负载均衡器
  let load_balancer = LoadBalancer::new({
    algorithm: "round_robin",  // 轮询算法
    servers: [
      "server1.example.com:8080",
      "server2.example.com:8080",
      "server3.example.com:8080"
    ],
    health_check_interval: 30000,
    failure_threshold: 3
  })
  
  // 测试轮询分配
  let request1 = LoadBalancer::select_server(load_balancer)
  let request2 = LoadBalancer::select_server(load_balancer)
  let request3 = LoadBalancer::select_server(load_balancer)
  let request4 = LoadBalancer::select_server(load_balancer)
  
  // 验证轮询顺序
  assert_eq(request1, "server1.example.com:8080")
  assert_eq(request2, "server2.example.com:8080")
  assert_eq(request3, "server3.example.com:8080")
  assert_eq(request4, "server1.example.com:8080")  // 回到第一个服务器
  
  // 测试加权轮询
  let weighted_lb = LoadBalancer::new({
    algorithm: "weighted_round_robin",
    servers: [
      ("server1.example.com:8080", 1),  // 权重1
      ("server2.example.com:8080", 3),  // 权重3
      ("server3.example.com:8080", 1)   // 权重1
    ],
    health_check_interval: 30000,
    failure_threshold: 3
  })
  
  // 收集服务器选择结果
  let server_selections = []
  for i in 0..=9 {
    let selected = LoadBalancer::select_server(weighted_lb)
    server_selections = server_selections.push(selected)
  }
  
  // 计算每个服务器被选中的次数
  let server1_count = server_selections.filter(fn(s) { s == "server1.example.com:8080" }).length()
  let server2_count = server_selections.filter(fn(s) { s == "server2.example.com:8080" }).length()
  let server3_count = server_selections.filter(fn(s) { s == "server3.example.com:8080" }).length()
  
  // 验证权重分配（server2应该被选中更多次）
  assert_true(server2_count > server1_count)
  assert_true(server2_count > server3_count)
  
  // 测试最少连接算法
  let least_conn_lb = LoadBalancer::new({
    algorithm: "least_connections",
    servers: [
      "server1.example.com:8080",
      "server2.example.com:8080",
      "server3.example.com:8080"
    ],
    health_check_interval: 30000,
    failure_threshold: 3
  })
  
  // 模拟不同服务器的连接数
  LoadBalancer::set_server_connections(least_conn_lb, "server1.example.com:8080", 5)
  LoadBalancer::set_server_connections(least_conn_lb, "server2.example.com:8080", 2)
  LoadBalancer::set_server_connections(least_conn_lb, "server3.example.com:8080", 8)
  
  // 选择服务器应该选择连接数最少的
  let least_conn_selected = LoadBalancer::select_server(least_conn_lb)
  assert_eq(least_conn_selected, "server2.example.com:8080")
  
  // 测试健康检查和故障转移
  LoadBalancer::mark_server_unhealthy(load_balancer, "server2.example.com:8080")
  
  // 健康检查应该排除不健康的服务器
  let healthy_servers = LoadBalancer::get_healthy_servers(load_balancer)
  assert_eq(healthy_servers.length(), 2)
  assert_false(healthy_servers.contains("server2.example.com:8080"))
  
  // 服务器选择应该只在健康服务器之间
  let healthy_selection1 = LoadBalancer::select_server(load_balancer)
  let healthy_selection2 = LoadBalancer::select_server(load_balancer)
  assert_true(healthy_selection1 != "server2.example.com:8080")
  assert_true(healthy_selection2 != "server2.example.com:8080")
  
  // 恢复服务器健康状态
  LoadBalancer::mark_server_healthy(load_balancer, "server2.example.com:8080")
  
  let all_servers = LoadBalancer::get_healthy_servers(load_balancer)
  assert_eq(all_servers.length(), 3)
}

// 测试8: 网络数据压缩传输
test "网络数据压缩传输测试" {
  // 创建支持压缩的网络客户端
  let compression_client = CompressionNetworkClient::new({
    compression_algorithms: ["gzip", "lz4", "deflate"],
    compression_threshold: 1024,  // 1KB以上的数据压缩
    max_compression_level: 6
  })
  
  // 创建小型数据（不应压缩）
  let small_data = "{\"metric\": \"temperature\", \"value\": 25.5}"
  let small_result = compression_client.send_with_compression({
    url: "http://api.example.com/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: small_data,
    timeout: 5000
  })
  
  assert_true(small_result.success)
  assert_eq(small_result.compression_used, None)  // 小数据不应压缩
  
  // 创建大型数据（应该压缩）
  let large_data = create_large_telemetry_data(500)  // 创建500条记录的大型数据
  let large_result = compression_client.send_with_compression({
    url: "http://api.example.com/telemetry/batch",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: large_data,
    timeout: 10000
  })
  
  assert_true(large_result.success)
  assert_true(large_result.compression_used != None)  // 大数据应该压缩
  
  match large_result.compression_used {
    Some(algorithm) => {
      assert_true(["gzip", "lz4", "deflate"].contains(algorithm))
    }
    None => assert_true(false)
  }
  
  // 验证压缩效果
  assert_true(large_result.compressed_size < large_result.original_size)
  let compression_ratio = 1.0 - (large_result.compressed_size.to_float() / large_result.original_size.to_float())
  assert_true(compression_ratio > 0.3)  // 至少30%压缩率
  
  // 测试不同压缩算法
  let gzip_result = compression_client.send_with_specific_compression({
    url: "http://api.example.com/telemetry/batch",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: large_data,
    timeout: 10000
  }, "gzip")
  
  let lz4_result = compression_client.send_with_specific_compression({
    url: "http://api.example.com/telemetry/batch",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: large_data,
    timeout: 10000
  }, "lz4")
  
  assert_true(gzip_result.success)
  assert_true(lz4_result.success)
  assert_eq(gzip_result.compression_used, Some("gzip"))
  assert_eq(lz4_result.compression_used, Some("lz4"))
  
  // 比较压缩效果
  let gzip_ratio = 1.0 - (gzip_result.compressed_size.to_float() / gzip_result.original_size.to_float())
  let lz4_ratio = 1.0 - (lz4_result.compressed_size.to_float() / lz4_result.original_size.to_float())
  
  // 通常gzip压缩率更高，但lz4速度更快
  assert_true(gzip_ratio >= lz4_ratio)
  
  // 测试压缩性能
  let gzip_start_time = Time::now()
  for i in 0..=10 {
    compression_client.compress_data(large_data, "gzip")
  }
  let gzip_duration = Time::now() - gzip_start_time
  
  let lz4_start_time = Time::now()
  for i in 0..=10 {
    compression_client.compress_data(large_data, "lz4")
  }
  let lz4_duration = Time::now() - lz4_start_time
  
  // lz4应该比gzip更快
  assert_true(lz4_duration < gzip_duration)
}

// 测试9: 网络安全连接
test "网络安全连接测试" {
  // 创建安全网络客户端
  let secure_client = SecureNetworkClient::new({
    tls_version: "1.3",
    cipher_suites: ["TLS_AES_256_GCM_SHA384", "TLS_CHACHA20_POLY1305_SHA256"],
    certificate_validation: true,
    hostname_verification: true,
    client_cert: None,
    client_key: None
  })
  
  // 测试HTTPS连接
  let https_result = secure_client.send_secure_request({
    url: "https://secure-api.example.com/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"security.event\", \"level\": \"info\"}",
    timeout: 5000
  })
  
  assert_true(https_result.success)
  assert_true(https_result.tls_version_used == "1.3")
  assert_true(https_result.cipher_suite_used != None)
  
  // 测试证书验证
  let cert_validation_result = secure_client.validate_certificate("invalid-cert.example.com")
  assert_false(cert_validation_result.valid)
  assert_true(cert_validation_result.errors.length() > 0)
  
  // 测试有效证书
  let valid_cert_result = secure_client.validate_certificate("valid-cert.example.com")
  assert_true(valid_cert_result.valid)
  assert_eq(valid_cert_result.errors.length(), 0)
  
  // 测试客户端证书认证
  let client_auth_client = SecureNetworkClient::new({
    tls_version: "1.3",
    cipher_suites: ["TLS_AES_256_GCM_SHA384"],
    certificate_validation: true,
    hostname_verification: true,
    client_cert: Some("client.crt"),
    client_key: Some("client.key")
  })
  
  let client_auth_result = client_auth_client.send_secure_request({
    url: "https://client-auth.example.com/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"auth.test\", \"success\": true}",
    timeout: 5000
  })
  
  assert_true(client_auth_result.success)
  assert_true(client_auth_result.client_auth_used)
  
  // 测试连接重用和安全会话恢复
  let session_client = SecureNetworkClient::new({
    tls_version: "1.3",
    cipher_suites: ["TLS_AES_256_GCM_SHA384"],
    certificate_validation: true,
    hostname_verification: true,
    session_resumption: true,
    session_timeout: 300000
  })
  
  // 第一次连接
  let first_session_result = session_client.send_secure_request({
    url: "https://session-api.example.com/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"session.test\", \"attempt\": 1}",
    timeout: 5000
  })
  
  assert_true(first_session_result.success)
  assert_false(first_session_result.session_resumed)  // 第一次连接不能恢复会话
  
  // 第二次连接应该恢复会话
  let second_session_result = session_client.send_secure_request({
    url: "https://session-api.example.com/telemetry",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    body: "{\"metric\": \"session.test\", \"attempt\": 2}",
    timeout: 5000
  })
  
  assert_true(second_session_result.success)
  assert_true(second_session_result.session_resumed)  // 应该恢复会话
  assert_true(second_session_result.handshake_time < first_session_result.handshake_time)
}

// 测试10: 网络流量控制和节流
test "网络流量控制和节流测试" {
  // 创建流量控制器
  let rate_limiter = NetworkRateLimiter::new({
    requests_per_second: 10,
    burst_size: 20,
    algorithm: "token_bucket"
  })
  
  // 测试正常请求速率
  let mut successful_requests = 0
  let mut rejected_requests = 0
  
  for i in 0..=15 {
    let request_result = rate_limiter.allow_request()
    if request_result.allowed {
      successful_requests = successful_requests + 1
    } else {
      rejected_requests = rejected_requests + 1
    }
  }
  
  // 验证请求限制
  assert_true(successful_requests <= 20)  // 不应超过突发大小
  assert_true(rejected_requests >= 0)
  
  // 等待令牌桶补充
  Time::sleep(2000)  // 等待2秒
  
  // 再次测试请求
  let after_wait_result = rate_limiter.allow_request()
  assert_true(after_wait_result.allowed)  // 应该有新的令牌
  
  // 测试不同服务器的独立限制
  let per_server_limiter = PerServerRateLimiter::new({
    default_requests_per_second: 5,
    server_limits: [
      ("high-priority-server.example.com", 20),
      ("medium-priority-server.example.com", 10),
      ("low-priority-server.example.com", 2)
    ],
    algorithm: "token_bucket"
  })
  
  // 测试高优先级服务器
  let mut high_priority_success = 0
  for i in 0..=15 {
    let result = per_server_limiter.allow_request("high-priority-server.example.com")
    if result.allowed {
      high_priority_success = high_priority_success + 1
    }
  }
  assert_true(high_priority_success > 5)  // 应该比默认限制更多
  
  // 测试低优先级服务器
  let mut low_priority_success = 0
  for i in 0..=10 {
    let result = per_server_limiter.allow_request("low-priority-server.example.com")
    if result.allowed {
      low_priority_success = low_priority_success + 1
    }
  }
  assert_true(low_priority_success <= 2)  // 应该受到严格限制
  
  // 测试自适应流量控制
  let adaptive_limiter = AdaptiveRateLimiter::new({
    initial_requests_per_second: 10,
    min_requests_per_second: 1,
    max_requests_per_second: 50,
    adjustment_factor: 0.1,
    success_threshold: 0.95,
    error_threshold: 0.1
  })
  
  // 模拟成功请求
  for i in 0..=20 {
    adaptive_limiter.record_request_result(true)
  }
  
  let adjusted_rate_up = adaptive_limiter.get_current_rate()
  assert_true(adjusted_rate_up > 10)  // 成功率高应该提高限制
  
  // 模拟失败请求
  for i in 0..=20 {
    adaptive_limiter.record_request_result(false)
  }
  
  let adjusted_rate_down = adaptive_limiter.get_current_rate()
  assert_true(adjusted_rate_down < adjusted_rate_up)  // 失败率高应该降低限制
  
  // 测试网络带宽限制
  let bandwidth_limiter = BandwidthLimiter::new({
    bytes_per_second: 1024 * 1024,  // 1MB/s
    burst_size: 5 * 1024 * 1024     // 5MB突发
  })
  
  let large_data_size = 2 * 1024 * 1024  // 2MB数据
  let bandwidth_result = bandwidth_limiter.allow_transfer(large_data_size)
  
  if bandwidth_result.allowed {
    assert_true(bandwidth_result.wait_time == 0)
  } else {
    assert_true(bandwidth_result.wait_time > 0)
  }
  
  // 测试优先级队列
  let priority_queue = PriorityNetworkQueue::new({
    max_size: 100,
    priority_levels: 3,
    processing_rate: 20  // 每秒处理20个请求
  })
  
  // 添加不同优先级的请求
  priority_queue.enqueue({
    id: "req1",
    priority: 1,  // 高优先级
    data: "high priority data",
    timestamp: Time::now()
  })
  
  priority_queue.enqueue({
    id: "req2",
    priority: 3,  // 低优先级
    data: "low priority data",
    timestamp: Time::now()
  })
  
  priority_queue.enqueue({
    id: "req3",
    priority: 1,  // 高优先级
    data: "another high priority data",
    timestamp: Time::now()
  })
  
  // 处理队列应该按优先级顺序处理
  let processed1 = priority_queue.dequeue()
  let processed2 = priority_queue.dequeue()
  let processed3 = priority_queue.dequeue()
  
  assert_eq(processed1.id, "req1")  // 第一个高优先级
  assert_eq(processed2.id, "req3")  // 第二个高优先级
  assert_eq(processed3.id, "req2")  // 低优先级
}

// 辅助函数：创建大型遥测数据
fn create_large_telemetry_data(record_count: Int) -> String {
  let mut data = "{\"telemetry_data\": ["
  for i in 0..=record_count - 1 {
    if i > 0 {
      data = data + ","
    }
    data = data + "{"
    data = data + "\"timestamp\": " + (1640995200 + i).to_string() + ","
    data = data + "\"metric\": \"metric." + i.to_string() + ","
    data = data + "\"value\": " + (i * 1.5).to_string() + ","
    data = data + "\"tags\": {"
    data = data + "\"host\": \"host" + (i % 10).to_string() + "\","
    data = data + "\"service\": \"service" + (i % 5).to_string() + "\""
    data = data + "}"
    data = data + "}"
  }
  data = data + "]}"
  data
}