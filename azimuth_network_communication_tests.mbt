// Azimuth Network Communication Test Suite
// 网络通信测试套件 - 专注于网络协议、连接管理和数据传输

// 测试1: HTTP/HTTPS客户端通信
test "HTTP/HTTPS客户端通信测试" {
  // 创建HTTP客户端
  let http_client = HttpClient::new()
  HttpClient::set_timeout(http_client, 5000) // 5秒超时
  HttpClient::set_user_agent(http_client, "Azimuth-Telemetry/1.0")
  HttpClient::set_max_redirects(http_client, 3)
  
  // 配置请求头
  let headers = [
    ("Content-Type", "application/json"),
    ("Accept", "application/json"),
    ("X-Trace-ID", "trace-123456"),
    ("X-Request-ID", "req-789012")
  ]
  
  // 创建遥测数据负载
  let telemetry_payload = {
    "trace_id": "trace-123456",
    "span_id": "span-789012",
    "service_name": "azimuth-telemetry",
    "operation_name": "http.request",
    "start_time": 1640995200000,
    "end_time": 1640995200500,
    "status": "ok",
    "attributes": {
      "http.method": "GET",
      "http.url": "https://api.example.com/telemetry",
      "http.status_code": 200,
      "response_size": 1024
    }
  }
  
  // 测试HTTPS GET请求
  let get_request = HttpRequest::new("GET", "https://api.example.com/telemetry")
  HttpRequest::set_headers(get_request, headers)
  
  let get_start_time = Clock::now_unix_nanos(Clock::system())
  let get_response = HttpClient::execute(http_client, get_request)
  let get_end_time = Clock::now_unix_nanos(Clock::system())
  
  // 验证GET请求
  match get_response {
    HttpResponse(status_code: 200, headers: response_headers, body: response_body) => {
      assert_true(response_headers.contains("Content-Type"))
      assert_true(response_body.length() > 0)
      
      // 验证响应时间
      let response_time = get_end_time - get_start_time
      assert_true(response_time < 5000000000) // 小于5秒
    }
    _ => assert_true(false)
  }
  
  // 测试HTTPS POST请求
  let post_request = HttpRequest::new("POST", "https://api.example.com/telemetry")
  HttpRequest::set_headers(post_request, headers)
  HttpRequest::set_body(post_request, Json::serialize(telemetry_payload))
  
  let post_start_time = Clock::now_unix_nanos(Clock::system())
  let post_response = HttpClient::execute(http_client, post_request)
  let post_end_time = Clock::now_unix_nanos(Clock::system())
  
  // 验证POST请求
  match post_response {
    HttpResponse(status_code: 201, headers: response_headers, body: response_body) => {
      assert_true(response_headers.contains("Location"))
      assert_true(response_body.contains("success"))
      
      // 验证响应时间
      let response_time = post_end_time - post_start_time
      assert_true(response_time < 5000000000) // 小于5秒
    }
    _ => assert_true(false)
  }
  
  // 测试连接池复用
  let pooled_request = HttpRequest::new("GET", "https://api.example.com/telemetry/pool")
  let pooled_response = HttpClient::execute(http_client, pooled_request)
  
  match pooled_response {
    HttpResponse(status_code: 200, _, _) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证连接池统计
  let pool_stats = HttpClient::get_connection_pool_stats(http_client)
  assert_true(pool_stats.active_connections > 0)
  assert_true(pool_stats.total_requests >= 3)
}

// 测试2: WebSocket通信
test "WebSocket通信测试" {
  // 创建WebSocket客户端
  let ws_client = WebSocketClient::new()
  WebSocketClient::set_ping_interval(ws_client, 30000) // 30秒ping间隔
  WebSocketClient::set_max_message_size(ws_client, 1024 * 1024) // 1MB最大消息
  
  // 连接到WebSocket服务器
  let ws_url = "wss://telemetry.example.com/ws"
  let connect_result = WebSocketClient::connect(ws_client, ws_url)
  
  assert_true(connect_result.success)
  assert_true(connect_result.connection_id.length() > 0)
  
  let connection_id = connect_result.connection_id
  
  // 测试发送文本消息
  let text_message = {
    "type": "telemetry_data",
    "trace_id": "trace-123456",
    "span_id": "span-789012",
    "timestamp": 1640995200000,
    "metrics": [
      {"name": "cpu_usage", "value": 75.5, "unit": "percent"},
      {"name": "memory_usage", "value": 512.0, "unit": "mb"},
      {"name": "request_count", "value": 1000, "unit": "count"}
    ]
  }
  
  let send_result = WebSocketClient::send_text(ws_client, connection_id, Json::serialize(text_message))
  assert_true(send_result.success)
  
  // 测试发送二进制消息
  let binary_data = BinaryData::from_string("binary_telemetry_data")
  let binary_send_result = WebSocketClient::send_binary(ws_client, connection_id, binary_data)
  assert_true(binary_send_result.success)
  
  // 测试接收消息
  let receive_start = Clock::now_unix_nanos(Clock::system())
  let receive_result = WebSocketClient::receive_message(ws_client, connection_id, 5000) // 5秒超时
  let receive_end = Clock::now_unix_nanos(Clock::system())
  
  match receive_result {
    WebSocketMessage::Text(text) => {
      assert_true(text.contains("telemetry"))
      assert_true(text.contains("acknowledged"))
    }
    WebSocketMessage::Binary(data) => {
      assert_true(data.length() > 0)
    }
    WebSocketMessage::Close(_) => assert_true(false)
  }
  
  // 验证接收延迟
  let receive_latency = receive_end - receive_start
  assert_true(receive_latency < 1000000000) // 小于1秒
  
  // 测试心跳机制
  let ping_result = WebSocketClient::ping(ws_client, connection_id)
  assert_true(ping_result.success)
  
  let pong_result = WebSocketClient::wait_for_pong(ws_client, connection_id, 3000)
  assert_true(pong_result.success)
  
  // 测试连接状态
  let connection_status = WebSocketClient::get_connection_status(ws_client, connection_id)
  assert_eq(connection_status, WebSocketStatus::Connected)
  
  // 测试优雅关闭
  let close_result = WebSocketClient::close(ws_client, connection_id, 1000, "Normal closure")
  assert_true(close_result.success)
  
  // 验证连接已关闭
  let closed_status = WebSocketClient::get_connection_status(ws_client, connection_id)
  assert_eq(closed_status, WebSocketStatus::Closed)
}

// 测试3: TCP/UDP套接字通信
test "TCP/UDP套接字通信测试" {
  // 创建TCP客户端
  let tcp_client = TcpClient::new()
  TcpClient::set_connect_timeout(tcp_client, 5000) // 5秒连接超时
  TcpClient::set_read_timeout(tcp_client, 3000) // 3秒读取超时
  TcpClient::set_write_timeout(tcp_client, 3000) // 3秒写入超时
  
  // 连接到TCP服务器
  let tcp_address = SocketAddress::new("tcp.telemetry.example.com", 8080)
  let tcp_connect_result = TcpClient::connect(tcp_client, tcp_address)
  
  assert_true(tcp_connect_result.success)
  let tcp_connection = tcp_connect_result.connection
  
  // 测试TCP数据发送
  let tcp_data = {
    "protocol": "telemetry",
    "version": "1.0",
    "data": {
      "trace_id": "trace-123456",
      "metrics": [
        {"name": "latency", "value": 150.5, "unit": "ms"},
        {"name": "throughput", "value": 1000.0, "unit": "ops/s"}
      ]
    }
  }
  
  let tcp_send_result = TcpClient::send(tcp_connection, Json::serialize(tcp_data))
  assert_true(tcp_send_result.bytes_sent > 0)
  
  // 测试TCP数据接收
  let tcp_receive_result = TcpClient::receive(tcp_connection, 4096) // 最多4KB
  assert_true(tcp_receive_result.bytes_received > 0)
  assert_true(tcp_receive_result.data.length() > 0)
  
  // 测试TCP连接统计
  let tcp_stats = TcpClient::get_connection_stats(tcp_connection)
  assert_true(tcp_stats.bytes_sent > 0)
  assert_true(tcp_stats.bytes_received > 0)
  assert_true(tcp_stats.connection_duration > 0)
  
  // 关闭TCP连接
  let tcp_close_result = TcpClient::close(tcp_connection)
  assert_true(tcp_close_result.success)
  
  // 创建UDP客户端
  let udp_client = UdpClient::new()
  UdpClient::set_send_timeout(udp_client, 1000) // 1秒发送超时
  UdpClient::set_receive_timeout(udp_client, 1000) // 1秒接收超时
  
  // 绑定UDP套接字
  let udp_bind_result = UdpClient::bind(udp_client, SocketAddress::new("0.0.0.0", 0)) // 随机端口
  assert_true(udp_bind_result.success)
  let udp_socket = udp_bind_result.socket
  
  // 测试UDP数据发送
  let udp_target = SocketAddress::new("udp.telemetry.example.com", 9090)
  let udp_data = "udp_telemetry_packet"
  let udp_send_result = UdpClient::send_to(udp_socket, udp_data, udp_target)
  assert_true(udp_send_result.bytes_sent > 0)
  
  // 测试UDP数据接收
  let udp_receive_result = UdpClient::receive_from(udp_socket, 1024) // 最多1KB
  match udp_receive_result {
    UdpReceiveResult(data: received_data, from: sender) => {
      assert_true(received_data.length() > 0)
      assert_true(sender.address.length() > 0)
    }
    UdpReceiveTimeout => assert_true(false) // 不应该超时
  }
  
  // 关闭UDP套接字
  let udp_close_result = UdpClient::close(udp_socket)
  assert_true(udp_close_result.success)
}

// 测试4: 网络连接池管理
test "网络连接池管理测试" {
  // 创建连接池
  let connection_pool = ConnectionPool::new()
  ConnectionPool::set_max_connections(connection_pool, 10) // 最大10个连接
  ConnectionPool::set_max_idle_time(connection_pool, 30000) // 30秒空闲超时
  ConnectionPool::set_connection_timeout(connection_pool, 5000) // 5秒连接超时
  
  // 配置连接池目标
  let pool_targets = [
    ("https://api1.example.com", 5), // 最多5个连接
    ("https://api2.example.com", 3), // 最多3个连接
    ("https://api3.example.com", 2)  // 最多2个连接
  ]
  
  for (target, max_connections) in pool_targets {
    ConnectionPool::add_target(connection_pool, target, max_connections)
  }
  
  // 测试连接获取
  let connection1 = ConnectionPool::get_connection(connection_pool, "https://api1.example.com")
  assert_true(connection1.is_some())
  
  let connection2 = ConnectionPool::get_connection(connection_pool, "https://api1.example.com")
  assert_true(connection2.is_some())
  
  let connection3 = ConnectionPool::get_connection(connection_pool, "https://api2.example.com")
  assert_true(connection3.is_some())
  
  // 测试连接池统计
  let pool_stats = ConnectionPool::get_stats(connection_pool)
  assert_true(pool_stats.active_connections >= 3)
  assert_true(pool_stats.total_connections_created >= 3)
  
  // 测试连接归还
  ConnectionPool::return_connection(connection_pool, connection1.unwrap())
  
  let returned_stats = ConnectionPool::get_stats(connection_pool)
  assert_true(returned_stats.idle_connections > 0)
  
  // 测试连接复用
  let reused_connection = ConnectionPool::get_connection(connection_pool, "https://api1.example.com")
  assert_true(reused_connection.is_some())
  
  // 验证连接复用统计
  let reuse_stats = ConnectionPool::get_stats(connection_pool)
  assert_true(reuse_stats.connections_reused > 0)
  
  // 测试连接清理
  ConnectionPool::cleanup_idle_connections(connection_pool)
  
  let cleanup_stats = ConnectionPool::get_stats(connection_pool)
  assert_true(cleanup_stats.connections_cleaned >= 0)
  
  // 归还所有连接
  if connection2.is_some() {
    ConnectionPool::return_connection(connection_pool, connection2.unwrap())
  }
  if connection3.is_some() {
    ConnectionPool::return_connection(connection_pool, connection3.unwrap())
  }
  if reused_connection.is_some() {
    ConnectionPool::return_connection(connection_pool, reused_connection.unwrap())
  }
  
  // 关闭连接池
  ConnectionPool::close(connection_pool)
  
  let final_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(final_stats.active_connections, 0)
}

// 测试5: 网络错误处理和重试机制
test "网络错误处理和重试机制测试" {
  // 创建带重试机制的HTTP客户端
  let retry_client = HttpClient::new()
  
  // 配置重试策略
  let retry_policy = RetryPolicy::new()
  RetryPolicy::set_max_attempts(retry_policy, 3) // 最多3次尝试
  RetryPolicy::set_initial_delay(retry_policy, 1000) // 初始延迟1秒
  RetryPolicy::set_max_delay(retry_policy, 10000) // 最大延迟10秒
  RetryPolicy::set_backoff_multiplier(retry_policy, 2.0) // 指数退避
  RetryPolicy::set_retryable_status_codes(retry_policy, [500, 502, 503, 504]) // 可重试状态码
  
  HttpClient::set_retry_policy(retry_client, retry_policy)
  
  // 测试网络超时错误
  let timeout_request = HttpRequest::new("GET", "https://timeout.example.com/timeout")
  HttpClient::set_timeout(retry_client, 1000) // 1秒超时
  
  let timeout_start = Clock::now_unix_nanos(Clock::system())
  let timeout_response = HttpClient::execute(retry_client, timeout_request)
  let timeout_end = Clock::now_unix_nanos(Clock::system())
  
  // 验证超时处理
  match timeout_response {
    HttpError(error_type: NetworkTimeout, message: _) => {
      assert_true(true)
      
      // 验证重试时间
      let total_time = timeout_end - timeout_start
      assert_true(total_time >= 3000000000) // 至少3秒（1秒超时 × 3次尝试）
    }
    _ => assert_true(false)
  }
  
  // 测试连接拒绝错误
  let refused_request = HttpRequest::new("GET", "https://refused.example.com/refused")
  let refused_response = HttpClient::execute(retry_client, refused_request)
  
  match refused_response {
    HttpError(error_type: ConnectionRefused, message: _) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试DNS解析错误
  let dns_request = HttpRequest::new("GET", "https://nonexistent.domain.example.com")
  let dns_response = HttpClient::execute(retry_client, dns_request)
  
  match dns_response {
    HttpError(error_type: DnsResolutionFailed, message: _) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试服务器错误重试
  let server_error_request = HttpRequest::new("GET", "https://error.example.com/server-error")
  let server_error_response = HttpClient::execute(retry_client, server_error_request)
  
  // 根据服务器配置，这可能成功或失败，但应该尝试重试
  match server_error_response {
    HttpResponse(status_code: 200, _, _) => assert_true(true) // 重试后成功
    HttpError(error_type: MaxRetriesExceeded, message: _) => assert_true(true) // 重试次数耗尽
    _ => assert_true(false)
  }
  
  // 验证重试统计
  let retry_stats = HttpClient::get_retry_stats(retry_client)
  assert_true(retry_stats.total_retries > 0)
  assert_true(retry_stats.failed_retries > 0)
  
  // 测试断路器模式
  let circuit_breaker = CircuitBreaker::new()
  CircuitBreaker::set_failure_threshold(circuit_breaker, 5) // 5次失败后打开
  CircuitBreaker::set_recovery_timeout(circuit_breaker, 30000) // 30秒后尝试恢复
  
  // 模拟多次失败
  for i in 1..=6 {
    let failing_request = HttpRequest::new("GET", "https://failing.example.com/fail")
    let failing_response = HttpClient::execute_with_circuit_breaker(retry_client, failing_request, circuit_breaker)
    
    if i <= 5 {
      match failing_response {
        HttpError(error_type: ConnectionRefused, message: _) => assert_true(true)
        _ => assert_true(false)
      }
    } else {
      match failing_response {
        HttpError(error_type: CircuitBreakerOpen, message: _) => assert_true(true)
        _ => assert_true(false)
      }
    }
  }
  
  // 验证断路器状态
  let breaker_state = CircuitBreaker::get_state(circuit_breaker)
  assert_eq(breaker_state, CircuitBreakerState::Open)
}

// 测试6: 网络性能和吞吐量测试
test "网络性能和吞吐量测试" {
  // 创建性能测试客户端
  let perf_client = HttpClient::new()
  HttpClient::set_timeout(perf_client, 30000) // 30秒超时
  HttpClient::set_keep_alive(perf_client, true)
  
  // 准备测试数据
  let small_payload = Json::serialize({
    "type": "small_telemetry",
    "data": "small_data_payload"
  })
  
  let medium_payload = Json::serialize({
    "type": "medium_telemetry",
    "data": "x".repeat(1024) // 1KB数据
  })
  
  let large_payload = Json::serialize({
    "type": "large_telemetry",
    "data": "x".repeat(10240) // 10KB数据
  })
  
  // 测试小数据包吞吐量
  let small_start_time = Clock::now_unix_nanos(Clock::system())
  let mut small_success_count = 0
  
  for i in 1..=100 {
    let small_request = HttpRequest::new("POST", "https://perf.example.com/telemetry")
    HttpRequest::set_body(small_request, small_payload)
    
    let small_response = HttpClient::execute(perf_client, small_request)
    match small_response {
      HttpResponse(status_code: 200, _, _) => small_success_count = small_success_count + 1
      _ => () // 忽略错误
    }
  }
  
  let small_end_time = Clock::now_unix_nanos(Clock::system())
  let small_duration = (small_end_time - small_start_time) / 1000000 // 转换为毫秒
  let small_throughput = (small_success_count.to_float() / small_duration.to_float()) * 1000 // 请求/秒
  
  assert_true(small_success_count >= 90) // 至少90%成功
  assert_true(small_throughput > 10.0) // 至少10请求/秒
  
  // 测试中等数据包吞吐量
  let medium_start_time = Clock::now_unix_nanos(Clock::system())
  let mut medium_success_count = 0
  
  for i in 1..=50 {
    let medium_request = HttpRequest::new("POST", "https://perf.example.com/telemetry")
    HttpRequest::set_body(medium_request, medium_payload)
    
    let medium_response = HttpClient::execute(perf_client, medium_request)
    match medium_response {
      HttpResponse(status_code: 200, _, _) => medium_success_count = medium_success_count + 1
      _ => () // 忽略错误
    }
  }
  
  let medium_end_time = Clock::now_unix_nanos(Clock::system())
  let medium_duration = (medium_end_time - medium_start_time) / 1000000 // 转换为毫秒
  let medium_throughput = (medium_success_count.to_float() / medium_duration.to_float()) * 1000 // 请求/秒
  
  assert_true(medium_success_count >= 45) // 至少90%成功
  assert_true(medium_throughput > 5.0) // 至少5请求/秒
  
  // 测试大数据包吞吐量
  let large_start_time = Clock::now_unix_nanos(Clock::system())
  let mut large_success_count = 0
  
  for i in 1..=20 {
    let large_request = HttpRequest::new("POST", "https://perf.example.com/telemetry")
    HttpRequest::set_body(large_request, large_payload)
    
    let large_response = HttpClient::execute(perf_client, large_request)
    match large_response {
      HttpResponse(status_code: 200, _, _) => large_success_count = large_success_count + 1
      _ => () // 忽略错误
    }
  }
  
  let large_end_time = Clock::now_unix_nanos(Clock::system())
  let large_duration = (large_end_time - large_start_time) / 1000000 // 转换为毫秒
  let large_throughput = (large_success_count.to_float() / large_duration.to_float()) * 1000 // 请求/秒
  
  assert_true(large_success_count >= 18) // 至少90%成功
  assert_true(large_throughput > 1.0) // 至少1请求/秒
  
  // 测试并发连接性能
  let concurrent_start_time = Clock::now_unix_nanos(Clock::system())
  let mut concurrent_success_count = 0
  
  // 创建10个并发请求
  let concurrent_results = []
  for i in 1..=10 {
    let concurrent_request = HttpRequest::new("GET", "https://perf.example.com/concurrent")
    let concurrent_response = HttpClient::execute_async(perf_client, concurrent_request)
    concurrent_results.push(concurrent_response)
  }
  
  // 等待所有请求完成
  for result in concurrent_results {
    let response = HttpClient::wait_for_result(result)
    match response {
      HttpResponse(status_code: 200, _, _) => concurrent_success_count = concurrent_success_count + 1
      _ => () // 忽略错误
    }
  }
  
  let concurrent_end_time = Clock::now_unix_nanos(Clock::system())
  let concurrent_duration = (concurrent_end_time - concurrent_start_time) / 1000000 // 转换为毫秒
  
  assert_true(concurrent_success_count >= 8) // 至少80%成功
  assert_true(concurrent_duration < 10000) // 小于10秒
  
  // 验证性能统计
  let perf_stats = HttpClient::get_performance_stats(perf_client)
  assert_true(perf_stats.total_requests >= 170)
  assert_true(perf_stats.successful_requests >= 153)
  assert_true(perf_stats.average_response_time > 0)
  assert_true(perf_stats.p95_response_time > perf_stats.average_response_time)
}

// 测试7: 网络安全和TLS配置
test "网络安全和TLS配置测试" {
  // 创建安全HTTP客户端
  let secure_client = HttpClient::new()
  
  // 配置TLS
  let tls_config = TlsConfig::new()
  TlsConfig::set_verify_mode(tls_config, TlsVerifyMode::Strict) // 严格验证
  TlsConfig::set_min_version(tls_config, TlsVersion::TLS1_2) // 最低TLS 1.2
  TlsConfig::set_max_version(tls_config, TlsVersion::TLS1_3) // 最高TLS 1.3
  TlsConfig::set_cipher_suites(tls_config, [
    "TLS_AES_256_GCM_SHA384",
    "TLS_CHACHA20_POLY1305_SHA256",
    "TLS_AES_128_GCM_SHA256"
  ])
  
  // 添加CA证书
  let ca_cert_path = "/etc/ssl/certs/ca-certificates.crt"
  TlsConfig::add_ca_certificate(tls_config, ca_cert_path)
  
  // 添加客户端证书
  let client_cert_path = "/path/to/client.crt"
  let client_key_path = "/path/to/client.key"
  TlsConfig::set_client_certificate(tls_config, client_cert_path, client_key_path)
  
  HttpClient::set_tls_config(secure_client, tls_config)
  
  // 测试HTTPS连接
  let secure_request = HttpRequest::new("GET", "https://secure.example.com/api")
  let secure_response = HttpClient::execute(secure_client, secure_request)
  
  match secure_response {
    HttpResponse(status_code: 200, headers: response_headers, body: _) => {
      assert_true(true)
      
      // 验证安全头
      assert_true(response_headers.contains("Strict-Transport-Security"))
      assert_true(response_headers.contains("X-Content-Type-Options"))
    }
    _ => assert_true(false)
  }
  
  // 测试证书验证
  let cert_info = HttpClient::get_peer_certificate_info(secure_client, "secure.example.com")
  assert_true(cert_info.is_valid)
  assert_true(cert_info.subject.contains("secure.example.com"))
  assert_true(cert_info.issuer.contains("trusted_ca"))
  assert_true(cert_info.not_after > Clock::now_unix_nanos(Clock::system()))
  
  // 测试证书固定
  let pinning_client = HttpClient::new()
  let pinning_config = CertificatePinningConfig::new()
  CertificatePinningConfig::add_public_key_hash(pinning_config, "sha256/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=")
  
  HttpClient::set_certificate_pinning(pinning_client, pinning_config)
  
  let pinning_request = HttpRequest::new("GET", "https://pinned.example.com/api")
  let pinning_response = HttpClient::execute(pinning_client, pinning_request)
  
  match pinning_response {
    HttpResponse(status_code: 200, _, _) => assert_true(true) // 证书匹配
    HttpError(error_type: CertificatePinningFailed, message: _) => assert_true(true) // 证书不匹配
    _ => assert_true(false)
  }
  
  // 测试OCSP装订
  let ocsp_client = HttpClient::new()
  let ocsp_config = TlsConfig::new()
  TlsConfig::enable_ocsp_stapling(ocsp_config, true)
  TlsConfig::set_ocsp_verify_mode(ocsp_config, OcspVerifyMode::Require)
  
  HttpClient::set_tls_config(ocsp_client, ocsp_config)
  
  let ocsp_request = HttpRequest::new("GET", "https://ocsp.example.com/api")
  let ocsp_response = HttpClient::execute(ocsp_client, ocsp_request)
  
  match ocsp_response {
    HttpResponse(status_code: 200, _, _) => assert_true(true)
    HttpError(error_type: OcspVerificationFailed, message: _) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试安全连接重用
  let reuse_request1 = HttpRequest::new("GET", "https://secure.example.com/api1")
  let reuse_response1 = HttpClient::execute(secure_client, reuse_request1)
  
  let reuse_request2 = HttpRequest::new("GET", "https://secure.example.com/api2")
  let reuse_response2 = HttpClient::execute(secure_client, reuse_request2)
  
  match (reuse_response1, reuse_response2) {
    (HttpResponse(status_code: 200, _, _), HttpResponse(status_code: 200, _, _)) => {
      assert_true(true)
      
      // 验证连接重用统计
      let reuse_stats = HttpClient::get_connection_stats(secure_client)
      assert_true(reuse_stats.connections_reused > 0)
    }
    _ => assert_true(false)
  }
}

// 测试8: 网络代理和负载均衡
test "网络代理和负载均衡测试" {
  // 创建带代理配置的HTTP客户端
  let proxy_client = HttpClient::new()
  
  // 配置HTTP代理
  let http_proxy = ProxyConfig::new()
  ProxyConfig::set_type(http_proxy, ProxyType::Http)
  ProxyConfig::set_host(http_proxy, "proxy.example.com")
  ProxyConfig::set_port(http_proxy, 8080)
  ProxyConfig::set_username(http_proxy, "proxy_user")
  ProxyConfig::set_password(http_proxy, "proxy_pass")
  
  // 配置代理绕过规则
  ProxyConfig::add_bypass_rule(http_proxy, "*.local")
  ProxyConfig::add_bypass_rule(http_proxy, "192.168.*")
  
  HttpClient::set_proxy_config(proxy_client, http_proxy)
  
  // 测试通过代理的请求
  let proxy_request = HttpRequest::new("GET", "https://api.example.com/through-proxy")
  let proxy_response = HttpClient::execute(proxy_client, proxy_request)
  
  match proxy_response {
    HttpResponse(status_code: 200, _, _) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证代理使用统计
  let proxy_stats = HttpClient::get_proxy_stats(proxy_client)
  assert_true(proxy_stats.requests_through_proxy > 0)
  
  // 创建负载均衡客户端
  let lb_client = HttpClient::new()
  
  // 配置负载均衡
  let lb_config = LoadBalancerConfig::new()
  LoadBalancerConfig::set_algorithm(lb_config, LoadBalancerAlgorithm::RoundRobin)
  LoadBalancerConfig::add_target(lb_config, "https://api1.example.com")
  LoadBalancerConfig::add_target(lb_config, "https://api2.example.com")
  LoadBalancerConfig::add_target(lb_config, "https://api3.example.com")
  LoadBalancerConfig::set_health_check_path(lb_config, "/health")
  LoadBalancerConfig::set_health_check_interval(lb_config, 30000) // 30秒健康检查
  
  HttpClient::set_load_balancer_config(lb_client, lb_config)
  
  // 测试负载均衡
  let mut responses = []
  for i in 1..=9 {
    let lb_request = HttpRequest::new("GET", "https://lb.example.com/api")
    let lb_response = HttpClient::execute(lb_client, lb_request)
    
    match lb_response {
      HttpResponse(status_code: 200, headers: response_headers, body: _) => {
        // 从响应头获取实际处理请求的服务器
        match response_headers.get("X-Served-By") {
          Some(server) => responses.push(server),
          None => responses.push("unknown")
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 验证负载分布
  let server_counts = responses.reduce(fn(acc, server) {
    let count = acc.get(server).or_else(Some(0))
    acc.set(server, count + 1)
    acc
  }, {})
  
  // 每个服务器应该处理大约相同数量的请求
  assert_true(server_counts.size() >= 2) // 至少使用2个服务器
  for (_, count) in server_counts {
    assert_true(count >= 2) // 每个服务器至少处理2个请求
    assert_true(count <= 5) // 每个服务器最多处理5个请求
  }
  
  // 测试故障转移
  let failover_client = HttpClient::new()
  let failover_config = LoadBalancerConfig::new()
  LoadBalancerConfig::set_algorithm(failover_config, LoadBalancerAlgorithm::Failover)
  LoadBalancerConfig::add_target(failover_config, "https://primary.example.com")
  LoadBalancerConfig::add_target(failover_config, "https://backup.example.com")
  
  HttpClient::set_load_balancer_config(failover_client, failover_config)
  
  let failover_request = HttpRequest::new("GET", "https://failover.example.com/api")
  let failover_response = HttpClient::execute(failover_client, failover_request)
  
  match failover_response {
    HttpResponse(status_code: 200, headers: response_headers, _) => {
      // 验证使用了哪个服务器
      match response_headers.get("X-Served-By") {
        Some(server) => {
          assert_true(server == "primary.example.com" || server == "backup.example.com")
        }
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证负载均衡统计
  let lb_stats = HttpClient::get_load_balancer_stats(lb_client)
  assert_true(lb_stats.total_requests >= 9)
  assert_true(lb_stats.healthy_targets >= 2)
  assert_true(lb_stats.failed_targets >= 0)
}