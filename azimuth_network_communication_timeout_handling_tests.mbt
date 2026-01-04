// Azimuth 网络通信和超时处理测试用例
// 专注于测试网络连接、超时机制、重试策略和错误恢复

// 测试1: 基础网络连接和超时
test "基础网络连接和超时" {
  // 1. 创建网络连接管理器
  let network_manager = NetworkManager::new()
  
  // 2. 配置连接参数
  let connection_config = ConnectionConfig({
    host: "telemetry.example.com",
    port: 8080,
    protocol: "http",
    timeout_ms: 5000,
    retry_attempts: 3,
    retry_delay_ms: 1000
  })
  
  // 3. 测试成功连接
  let connection_result = NetworkManager::connect(network_manager, connection_config)
  assert_true(connection_result.success)
  assert_not_nil(connection_result.connection_id)
  
  // 4. 测试连接状态
  let connection_id = connection_result.connection_id!
  assert_true(NetworkManager::is_connected(network_manager, connection_id))
  assert_eq(NetworkManager::get_connection_info(network_manager, connection_id).host, "telemetry.example.com")
  assert_eq(NetworkManager::get_connection_info(network_manager, connection_id).port, 8080)
  
  // 5. 测试数据发送
  let telemetry_data = TelemetryPacket({
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    metrics: [("cpu_usage", 75.5), ("memory_usage", 60.2)],
    timestamp: 1735689600000000000L
  })
  
  let send_result = NetworkManager::send_data(network_manager, connection_id, telemetry_data)
  assert_true(send_result.success)
  assert_eq(send_result.bytes_sent, telemetry_data.size())
  
  // 6. 测试连接关闭
  let close_result = NetworkManager::close_connection(network_manager, connection_id)
  assert_true(close_result.success)
  assert_false(NetworkManager::is_connected(network_manager, connection_id))
}

// 测试2: 连接超时处理
test "连接超时处理" {
  // 1. 创建网络连接管理器
  let network_manager = NetworkManager::new()
  
  // 2. 配置短超时连接（模拟超时场景）
  let timeout_config = ConnectionConfig({
    host: "slow-server.example.com",
    port: 8080,
    protocol: "http",
    timeout_ms: 100, // 非常短的超时
    retry_attempts: 2,
    retry_delay_ms: 50
  })
  
  // 3. 测试连接超时
  let timeout_result = NetworkManager::connect(network_manager, timeout_config)
  assert_false(timeout_result.success)
  assert_eq(timeout_result.error.type, "connection_timeout")
  assert_true(timeout_result.error.message.contains("timeout"))
  
  // 4. 测试连接超时统计
  let timeout_stats = NetworkManager::get_timeout_stats(network_manager)
  assert_eq(timeout_stats.connection_timeouts, 1)
  assert_eq(timeout_stats.total_connection_attempts, 1)
  
  // 5. 测试不同超时设置
  let configs = [
    (50, "very_short"),
    (100, "short"),
    (500, "medium"),
    (1000, "long"),
    (5000, "very_long")
  ]
  
  for (timeout_ms, config_name) in configs {
    let test_config = ConnectionConfig({
      host: "test-server-" + config_name + ".example.com",
      port: 8080,
      protocol: "http",
      timeout_ms: timeout_ms,
      retry_attempts: 1,
      retry_delay_ms: 10
    })
    
    let start_time = Time::now()
    let result = NetworkManager::connect(network_manager, test_config)
    let end_time = Time::now()
    let actual_duration = end_time - start_time
    
    if not(result.success) and result.error.type == "connection_timeout" {
      // 验证实际超时时间接近配置的超时时间（允许一定误差）
      assert_true(actual_duration >= timeout_ms)
      assert_true(actual_duration <= timeout_ms + 500) // 允许500ms误差
    }
  }
  
  // 6. 测试超时事件记录
  let timeout_events = NetworkManager::get_timeout_events(network_manager)
  assert_true(timeout_events.length() > 0)
  
  for event in timeout_events {
    assert_eq(event.event_type, "connection_timeout")
    assert_not_nil(event.timestamp)
    assert_not_nil(event.config.host)
    assert_true(event.actual_timeout_ms >= event.config.timeout_ms)
  }
}

// 测试3: 数据传输超时处理
test "数据传输超时处理" {
  // 1. 创建网络连接管理器
  let network_manager = NetworkManager::new()
  
  // 2. 建立连接
  let connection_config = ConnectionConfig({
    host: "telemetry.example.com",
    port: 8080,
    protocol: "http",
    timeout_ms: 5000,
    retry_attempts: 3,
    retry_delay_ms: 1000
  })
  
  let connection_result = NetworkManager::connect(network_manager, connection_config)
  assert_true(connection_result.success)
  let connection_id = connection_result.connection_id!
  
  // 3. 创建大量数据（模拟传输超时）
  let large_data = TelemetryPacket({
    trace_id: "trace-" + "x".repeat(1000),
    span_id: "span-" + "y".repeat(1000),
    metrics: [
      for i in 0..=1000 {
        ("metric_" + i.to_string(), i as Float * 0.1)
      }
    ],
    timestamp: 1735689600000000000L
  })
  
  // 4. 配置短传输超时
  let transfer_config = TransferConfig({
    connection_id: connection_id,
    timeout_ms: 100, // 非常短的传输超时
    chunk_size: 1024,
    max_retries: 1
  })
  
  // 5. 测试传输超时
  let transfer_result = NetworkManager::send_data_with_config(network_manager, large_data, transfer_config)
  assert_false(transfer_result.success)
  assert_eq(transfer_result.error.type, "transfer_timeout")
  
  // 6. 测试部分传输统计
  assert_true(transfer_result.bytes_transferred < large_data.size())
  assert_true(transfer_result.bytes_transferred > 0)
  
  // 7. 测试传输超时统计
  let transfer_stats = NetworkManager::get_transfer_stats(network_manager)
  assert_eq(transfer_stats.transfer_timeouts, 1)
  assert_eq(transfer_stats.partial_transfers, 1)
  assert_true(transfer_stats.total_bytes_attempted > transfer_stats.total_bytes_successful)
  
  // 8. 测试传输恢复
  let resume_config = TransferConfig({
    connection_id: connection_id,
    timeout_ms: 5000, // 更长的超时
    chunk_size: 512,
    max_retries: 3,
    resume_from_byte: transfer_result.bytes_transferred
  })
  
  let resume_result = NetworkManager::resume_transfer(network_manager, large_data, resume_config)
  assert_true(resume_result.success)
  assert_eq(resume_result.bytes_transferred, large_data.size())
  
  // 9. 清理连接
  NetworkManager::close_connection(network_manager, connection_id)
}

// 测试4: 自动重试机制
test "自动重试机制" {
  // 1. 创建网络连接管理器
  let network_manager = NetworkManager::new()
  
  // 2. 模拟不稳定的连接
  let unstable_config = ConnectionConfig({
    host: "unstable.example.com",
    port: 8080,
    protocol: "http",
    timeout_ms: 1000,
    retry_attempts: 5,
    retry_delay_ms: 500,
    backoff_multiplier: 2.0
  })
  
  // 3. 配置失败模拟器
  let failure_simulator = FailureSimulator::new()
  FailureSimulator::set_failure_pattern(failure_simulator, [
    (1, true),   // 第一次尝试失败
    (2, true),   // 第二次尝试失败
    (3, false),  // 第三次尝试成功
    (4, false),  // 后续尝试成功
    (5, false)
  ])
  
  // 4. 测试带重试的连接
  let retry_result = NetworkManager::connect_with_retry(network_manager, unstable_config, failure_simulator)
  assert_true(retry_result.success)
  assert_eq(retry_result.attempts, 3)
  
  // 5. 验证重试延迟
  assert_eq(retry_result.retry_delays[0], 500)   // 第一次重试延迟
  assert_eq(retry_result.retry_delays[1], 1000)  // 第二次重试延迟（指数退避）
  
  // 6. 测试重试统计
  let retry_stats = NetworkManager::get_retry_stats(network_manager)
  assert_eq(retry_stats.total_retry_attempts, 2)
  assert_eq(retry_stats.successful_retries, 1)
  assert_eq(retry_stats.retry_success_rate, 0.5)
  
  // 7. 测试所有尝试都失败的情况
  let always_fail_config = ConnectionConfig({
    host: "always-fail.example.com",
    port: 8080,
    protocol: "http",
    timeout_ms: 1000,
    retry_attempts: 3,
    retry_delay_ms: 200,
    backoff_multiplier: 1.5
  })
  
  FailureSimulator::set_failure_pattern(failure_simulator, [
    (1, true),
    (2, true),
    (3, true)
  ])
  
  let fail_result = NetworkManager::connect_with_retry(network_manager, always_fail_config, failure_simulator)
  assert_false(fail_result.success)
  assert_eq(fail_result.attempts, 3)
  assert_eq(fail_result.error.type, "all_attempts_failed")
  
  // 8. 测试重试事件记录
  let retry_events = NetworkManager::get_retry_events(network_manager)
  assert_true(retry_events.length() >= 3)
  
  let fail_events = retry_events.filter(fn(event) { event.config.host == "always-fail.example.com" })
  assert_eq(fail_events.length(), 3)
  
  for i in 0..fail_events.length() {
    assert_eq(fail_events[i].attempt_number, i + 1)
    assert_true(fail_events[i].failed)
  }
}

// 测试5: 连接池管理
test "连接池管理" {
  // 1. 创建连接池管理器
  let connection_pool = ConnectionPool::new()
  
  // 2. 配置连接池
  ConnectionPool::configure(connection_pool, PoolConfig({
    max_connections: 10,
    min_connections: 2,
    connection_timeout_ms: 5000,
    idle_timeout_ms: 30000,
    health_check_interval_ms: 10000
  }))
  
  // 3. 测试连接池初始化
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 0)
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 0)
  
  // 4. 测试连接获取
  let connections = []
  for i in 0..=5 {
    let connection = ConnectionPool::acquire_connection(connection_pool, "telemetry.example.com", 8080)
    assert_not_nil(connection)
    connections.push(connection)
  }
  
  // 5. 验证连接池状态
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 5)
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 0)
  
  // 6. 测试连接释放
  for connection in connections {
    ConnectionPool::release_connection(connection_pool, connection)
  }
  
  // 7. 验证连接释放后的状态
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 0)
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 5)
  
  // 8. 测试连接重用
  let reused_connection = ConnectionPool::acquire_connection(connection_pool, "telemetry.example.com", 8080)
  assert_not_nil(reused_connection)
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 1)
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 4)
  
  // 9. 测试连接超时清理
  ConnectionPool::advance_time(connection_pool, 35000) // 超过空闲超时时间
  ConnectionPool::cleanup_idle_connections(connection_pool)
  
  // 10. 验证超时清理结果
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 0)
  
  // 11. 测试连接健康检查
  let unhealthy_connection = ConnectionPool::acquire_connection(connection_pool, "unhealthy.example.com", 8080)
  ConnectionPool::simulate_connection_failure(unhealthy_connection)
  ConnectionPool::release_connection(connection_pool, unhealthy_connection)
  
  ConnectionPool::perform_health_check(connection_pool)
  assert_eq(ConnectionPool::get_idle_connections(connection_pool), 0) // 不健康连接被移除
  
  // 12. 测试连接池统计
  let pool_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(pool_stats.total_acquired, 7)
  assert_eq(pool_stats.total_released, 7)
  assert_eq(pool_stats.connections_created, 6)
  assert_eq(pool_stats.connections_reused, 1)
  assert_eq(pool_stats.connections_timeout, 4)
  assert_eq(pool_stats.connections_failed_health_check, 1)
}

// 测试6: 网络错误分类和处理
test "网络错误分类和处理" {
  // 1. 创建网络错误处理器
  let error_handler = NetworkErrorHandler::new()
  
  // 2. 配置错误处理策略
  NetworkErrorHandler::configure_error_strategy(error_handler, "connection_refused", ErrorStrategy::RetryWithBackoff)
  NetworkErrorHandler::configure_error_strategy(error_handler, "connection_timeout", ErrorStrategy::RetryWithBackoff)
  NetworkErrorHandler::configure_error_strategy(error_handler, "dns_resolution_failed", ErrorStrategy::FailImmediately)
  NetworkErrorHandler::configure_error_strategy(error_handler, "ssl_handshake_failed", ErrorStrategy::FailImmediately)
  NetworkErrorHandler::configure_error_strategy(error_handler, "transfer_timeout", ErrorStrategy::ResumeTransfer)
  NetworkErrorHandler::configure_error_strategy(error_handler, "server_error_5xx", ErrorStrategy::RetryWithBackoff)
  NetworkErrorHandler::configure_error_strategy(error_handler, "client_error_4xx", ErrorStrategy::FailImmediately)
  
  // 3. 测试错误分类
  let test_errors = [
    ("Connection refused", "connection_refused", true),
    ("DNS resolution failed", "dns_resolution_failed", false),
    ("SSL handshake failed", "ssl_handshake_failed", false),
    ("Transfer timeout", "transfer_timeout", true),
    ("Server error 500", "server_error_5xx", true),
    ("Client error 404", "client_error_4xx", false)
  ]
  
  for (message, error_type, should_retry) in test_errors {
    let error = NetworkError::new(error_type, message)
    let classification = NetworkErrorHandler::classify_error(error_handler, error)
    let strategy = NetworkErrorHandler::get_error_strategy(error_handler, error_type)
    
    assert_eq(classification.type, error_type)
    assert_eq(strategy.should_retry, should_retry)
  }
  
  // 4. 测试错误恢复策略
  let recoverable_errors = [
    NetworkError::new("connection_timeout", "Connection timeout after 5000ms"),
    NetworkError::new("server_error_5xx", "Internal server error 500"),
    NetworkError::new("transfer_timeout", "Transfer timeout after 10000ms")
  ]
  
  for error in recoverable_errors {
    let recovery_action = NetworkErrorHandler::get_recovery_action(error_handler, error)
    assert_not_nil(recovery_action)
    assert_true(recovery_action.type == "retry" || recovery_action.type == "resume")
  }
  
  // 5. 测试不可恢复错误
  let non_recoverable_errors = [
    NetworkError::new("dns_resolution_failed", "DNS resolution failed"),
    NetworkError::new("ssl_handshake_failed", "SSL handshake failed"),
    NetworkError::new("client_error_4xx", "Not found 404")
  ]
  
  for error in non_recoverable_errors {
    let recovery_action = NetworkErrorHandler::get_recovery_action(error_handler, error)
    assert_not_nil(recovery_action)
    assert_eq(recovery_action.type, "fail")
  }
  
  // 6. 测试错误统计
  for error in recoverable_errors + non_recoverable_errors {
    NetworkErrorHandler::record_error(error_handler, error)
  }
  
  let error_stats = NetworkErrorHandler::get_error_stats(error_handler)
  assert_eq(error_stats.total_errors, 6)
  assert_eq(error_stats.recoverable_errors, 3)
  assert_eq(error_stats.non_recoverable_errors, 3)
  
  // 7. 测试错误率监控
  let error_rate = NetworkErrorHandler::get_error_rate(error_handler, 60000) // 最近1分钟
  assert_true(error_rate > 0.0)
  assert_true(error_rate <= 1.0)
  
  // 8. 测试错误阈值告警
  NetworkErrorHandler::set_error_threshold(error_handler, 0.5) // 50%错误率阈值
  let alert_triggered = NetworkErrorHandler::check_error_threshold(error_handler)
  assert_true(alert_triggered) // 6个错误中有3个是不可恢复的，错误率50%
}

// 测试7: 网络性能监控
test "网络性能监控" {
  // 1. 创建网络性能监控器
  let performance_monitor = NetworkPerformanceMonitor::new()
  
  // 2. 模拟网络操作
  let operations = [
    ("small_data", 1024, 50),      // 1KB数据，50ms延迟
    ("medium_data", 10240, 200),   // 10KB数据，200ms延迟
    ("large_data", 102400, 800),   // 100KB数据，800ms延迟
    ("huge_data", 1024000, 3000)   // 1MB数据，3000ms延迟
  ]
  
  for (name, size, latency) in operations {
    // 模拟连接建立
    let connection_start = Time::now()
    Time::sleep(10) // 模拟连接时间
    let connection_end = Time::now()
    
    // 模拟数据传输
    let transfer_start = Time::now()
    Time::sleep(latency)
    let transfer_end = Time::now()
    
    // 记录性能指标
    NetworkPerformanceMonitor::record_connection_time(
      performance_monitor,
      name,
      connection_end - connection_start
    )
    
    NetworkPerformanceMonitor::record_transfer_performance(
      performance_monitor,
      name,
      size,
      transfer_end - transfer_start
    )
  }
  
  // 3. 测试性能统计
  let connection_stats = NetworkPerformanceMonitor::get_connection_stats(performance_monitor)
  assert_eq(connection_stats.total_connections, 4)
  assert_true(connection_stats.avg_connection_time > 0)
  assert_true(connection_stats.min_connection_time > 0)
  assert_true(connection_stats.max_connection_time >= connection_stats.min_connection_time)
  
  let transfer_stats = NetworkPerformanceMonitor::get_transfer_stats(performance_monitor)
  assert_eq(transfer_stats.total_transfers, 4)
  assert_eq(transfer_stats.total_bytes, 1034494) // 1KB + 10KB + 100KB + 1MB
  assert_true(transfer_stats.avg_throughput > 0)
  assert_true(transfer_stats.min_throughput > 0)
  assert_true(transfer_stats.max_throughput >= transfer_stats.min_throughput)
  
  // 4. 测试性能趋势分析
  let performance_trend = NetworkPerformanceMonitor::analyze_performance_trend(performance_monitor, 300000) // 最近5分钟
  assert_not_nil(performance_trend)
  
  // 验证趋势数据
  assert_true(performance_trend.connection_time_trend.length() > 0)
  assert_true(performance_trend.throughput_trend.length() > 0)
  assert_true(performance_trend.latency_trend.length() > 0)
  
  // 5. 测试性能异常检测
  // 添加一个异常慢的操作
  NetworkPerformanceMonitor::record_transfer_performance(
    performance_monitor,
    "anomaly_slow",
    1024,
    5000 // 异常高的延迟
  )
  
  let anomalies = NetworkPerformanceMonitor::detect_performance_anomalies(performance_monitor)
  assert_true(anomalies.length() > 0)
  
  // 验证异常检测结果
  let slow_anomalies = anomalies.filter(fn(anomaly) { anomaly.type == "high_latency" })
  assert_true(slow_anomalies.length() > 0)
  
  // 6. 测试性能报告生成
  let performance_report = NetworkPerformanceMonitor::generate_performance_report(performance_monitor)
  assert_not_nil(performance_report)
  
  // 验证报告内容
  assert_true(performance_report.contains("Connection Statistics"))
  assert_true(performance_report.contains("Transfer Statistics"))
  assert_true(performance_report.contains("Performance Trends"))
  assert_true(performance_report.contains("Detected Anomalies"))
  
  // 7. 测试性能阈值告警
  NetworkPerformanceMonitor::set_latency_threshold(performance_monitor, 1000) // 1秒延迟阈值
  NetworkPerformanceMonitor::set_throughput_threshold(performance_monitor, 10000) // 10KB/s吞吐量阈值
  
  let threshold_alerts = NetworkPerformanceMonitor::check_performance_thresholds(performance_monitor)
  assert_true(threshold_alerts.length() > 0)
  
  // 验证告警内容
  let latency_alerts = threshold_alerts.filter(fn(alert) { alert.type == "high_latency" })
  let throughput_alerts = threshold_alerts.filter(fn(alert) { alert.type == "low_throughput" })
  
  assert_true(latency_alerts.length() > 0) // 应该有高延迟告警
  // 吞吐量告警取决于实际测试环境
}

// 测试8: 网络负载均衡
test "网络负载均衡" {
  // 1. 创建负载均衡器
  let load_balancer = LoadBalancer::new()
  
  // 2. 配置服务器池
  let servers = [
    ServerConfig({ host: "server1.example.com", port: 8080, weight: 1, health: "healthy" }),
    ServerConfig({ host: "server2.example.com", port: 8080, weight: 2, health: "healthy" }),
    ServerConfig({ host: "server3.example.com", port: 8080, weight: 1, health: "healthy" })
  ]
  
  for server in servers {
    LoadBalancer::add_server(load_balancer, server)
  }
  
  // 3. 测试轮询策略
  LoadBalancer::set_strategy(load_balancer, LoadBalanceStrategy::RoundRobin)
  
  let round_robin_selections = []
  for i in 0..=9 {
    let selected_server = LoadBalancer::select_server(load_balancer)
    round_robin_selections.push(selected_server.host)
  }
  
  // 验证轮询分布
  assert_eq(round_robin_selections[0], "server1.example.com")
  assert_eq(round_robin_selections[1], "server2.example.com")
  assert_eq(round_robin_selections[2], "server3.example.com")
  assert_eq(round_robin_selections[3], "server1.example.com")
  assert_eq(round_robin_selections[4], "server2.example.com")
  
  // 4. 测试加权轮询策略
  LoadBalancer::set_strategy(load_balancer, LoadBalanceStrategy::WeightedRoundRobin)
  
  let weighted_selections = []
  for i in 0..=9 {
    let selected_server = LoadBalancer::select_server(load_balancer)
    weighted_selections.push(selected_server.host)
  }
  
  // 验证加权分布（server2应该被选中更多次）
  let server1_count = weighted_selections.filter(fn(host) { host == "server1.example.com" }).length()
  let server2_count = weighted_selections.filter(fn(host) { host == "server2.example.com" }).length()
  let server3_count = weighted_selections.filter(fn(host) { host == "server3.example.com" }).length()
  
  assert_eq(server1_count, 3)
  assert_eq(server2_count, 4) // 权重为2，应该被选中更多次
  assert_eq(server3_count, 3)
  
  // 5. 测试最少连接策略
  LoadBalancer::set_strategy(load_balancer, LoadBalanceStrategy::LeastConnections)
  
  // 模拟连接数
  LoadBalancer::set_server_connections(load_balancer, "server1.example.com", 5)
  LoadBalancer::set_server_connections(load_balancer, "server2.example.com", 2)
  LoadBalancer::set_server_connections(load_balancer, "server3.example.com", 8)
  
  let least_conn_server = LoadBalancer::select_server(load_balancer)
  assert_eq(least_conn_server.host, "server2.example.com") // 连接数最少
  
  // 6. 测试健康检查和故障转移
  // 标记server2为不健康
  LoadBalancer::set_server_health(load_balancer, "server2.example.com", "unhealthy")
  
  let healthy_servers = LoadBalancer::get_healthy_servers(load_balancer)
  assert_eq(healthy_servers.length(), 2)
  assert_true(healthy_servers.any(fn(server) { server.host == "server1.example.com" }))
  assert_true(healthy_servers.any(fn(server) { server.host == "server3.example.com" }))
  assert_false(healthy_servers.any(fn(server) { server.host == "server2.example.com" }))
  
  // 验证故障转移
  let failover_server = LoadBalancer::select_server(load_balancer)
  assert_true(failover_server.host == "server1.example.com" || failover_server.host == "server3.example.com")
  
  // 7. 测试负载均衡统计
  let lb_stats = LoadBalancer::get_stats(load_balancer)
  assert_eq(lb_stats.total_requests, 10 + 10 + 1 + 1) // 轮询 + 加权 + 最少连接 + 故障转移
  assert_eq(lb_stats.server_stats["server1.example.com"].requests, 4) // 轮询2 + 加权3 + 最少连接0 + 故障转移1
  assert_eq(lb_stats.server_stats["server2.example.com"].requests, 4) // 轮询2 + 加权4 + 最少连接1 + 故障转移0
  assert_eq(lb_stats.server_stats["server3.example.com"].requests, 4) // 轮询2 + 加权3 + 最少连接0 + 故障转移1
  
  // 8. 测试动态权重调整
  LoadBalancer::adjust_server_weight(load_balancer, "server1.example.com", 3)
  LoadBalancer::adjust_server_weight(load_balancer, "server3.example.com", 2)
  
  // 恢复server2健康状态
  LoadBalancer::set_server_health(load_balancer, "server2.example.com", "healthy")
  
  let adjusted_selections = []
  for i in 0..=5 {
    let selected_server = LoadBalancer::select_server(load_balancer)
    adjusted_selections.push(selected_server.host)
  }
  
  // 验证权重调整后的分布
  let adj_server1_count = adjusted_selections.filter(fn(host) { host == "server1.example.com" }).length()
  let adj_server2_count = adjusted_selections.filter(fn(host) { host == "server2.example.com" }).length()
  let adj_server3_count = adjusted_selections.filter(fn(host) { host == "server3.example.com" }).length()
  
  assert_eq(adj_server1_count, 2)
  assert_eq(adj_server2_count, 1)
  assert_eq(adj_server3_count, 3)
}