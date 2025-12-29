// 网络层健壮性测试用例
// 测试网络延迟、超时、连接池管理、重试策略等高级网络功能

test "network_latency_handling" {
  // 测试网络延迟处理机制
  
  struct NetworkLatencyProfile {
    min_latency_ms: Int
    max_latency_ms: Int
    avg_latency_ms: Int
    p95_latency_ms: Int
    p99_latency_ms: Int
    jitter_ms: Int
  }
  
  struct TimeoutConfig {
    connection_timeout_ms: Int
    read_timeout_ms: Int
    write_timeout_ms: Int
    total_timeout_ms: Int
  }
  
  struct LatencyThresholds {
    warning_threshold_ms: Int
    critical_threshold_ms: Int
    acceptable_p99_ms: Int
  }
  
  // 创建网络延迟配置
  let latency_profile = NetworkLatencyProfile{
    min_latency_ms: 10,
    max_latency_ms: 5000,
    avg_latency_ms: 150,
    p95_latency_ms: 800,
    p99_latency_ms: 2000,
    jitter_ms: 50
  }
  
  let timeout_config = TimeoutConfig{
    connection_timeout_ms: 5000,
    read_timeout_ms: 10000,
    write_timeout_ms: 10000,
    total_timeout_ms: 30000
  }
  
  let latency_thresholds = LatencyThresholds{
    warning_threshold_ms: 1000,
    critical_threshold_ms: 3000,
    acceptable_p99_ms: 2500
  }
  
  // 验证延迟配置的合理性
  assert_eq(latency_profile.min_latency_ms <= latency_profile.avg_latency_ms, true)
  assert_eq(latency_profile.avg_latency_ms <= latency_profile.p95_latency_ms, true)
  assert_eq(latency_profile.p95_latency_ms <= latency_profile.p99_latency_ms, true)
  assert_eq(latency_profile.p99_latency_ms <= latency_profile.max_latency_ms, true)
  
  assert_eq(timeout_config.connection_timeout_ms <= timeout_config.read_timeout_ms, true)
  assert_eq(timeout_config.read_timeout_ms <= timeout_config.total_timeout_ms, true)
  assert_eq(timeout_config.write_timeout_ms <= timeout_config.total_timeout_ms, true)
  
  // 延迟分类函数
  let classify_latency = fn(latency_ms: Int, thresholds: LatencyThresholds) -> String {
    if latency_ms >= thresholds.critical_threshold_ms {
      "critical"
    } else if latency_ms >= thresholds.warning_threshold_ms {
      "warning"
    } else {
      "normal"
    }
  }
  
  // 测试延迟分类
  assert_eq(classify_latency(500, latency_thresholds), "normal")
  assert_eq(classify_latency(1500, latency_thresholds), "warning")
  assert_eq(classify_latency(4000, latency_thresholds), "critical")
  
  // 测试延迟是否在可接受范围内
  let is_latency_acceptable = fn(profile: NetworkLatencyProfile, thresholds: LatencyThresholds) -> Bool {
    profile.p99_latency_ms <= thresholds.acceptable_p99_ms &&
    profile.avg_latency_ms <= thresholds.warning_threshold_ms / 2
  }
  
  assert_eq(is_latency_acceptable(latency_profile, latency_thresholds), true)
  
  // 测试高延迟场景
  let high_latency_profile = NetworkLatencyProfile{
    min_latency_ms: 100,
    max_latency_ms: 10000,
    avg_latency_ms: 2000,
    p95_latency_ms: 5000,
    p99_latency_ms: 8000,
    jitter_ms: 500
  }
  
  assert_eq(is_latency_acceptable(high_latency_profile, latency_thresholds), false)
  assert_eq(classify_latency(high_latency_profile.avg_latency_ms, latency_thresholds), "critical")
}

test "connection_timeout_management" {
  // 测试连接超时管理
  
  enum ConnectionState {
    Connecting
    Connected
    Timeout
    Failed
    Disconnected
  }
  
  struct ConnectionMetrics {
    state: ConnectionState
    connect_time_ms: Int
    last_activity_time: Int64
    bytes_sent: Int
    bytes_received: Int
    timeout_count: Int
  }
  
  struct ConnectionPool {
    active_connections: Array<ConnectionMetrics>
    idle_connections: Array<ConnectionMetrics>
    max_connections: Int
    connection_timeout_ms: Int
    idle_timeout_ms: Int
  }
  
  // 创建连接池
  let connection_pool = ConnectionPool{
    active_connections: Array::empty(),
    idle_connections: Array::empty(),
    max_connections: 100,
    connection_timeout_ms: 5000,
    idle_timeout_ms: 30000
  }
  
  // 模拟创建连接
  let create_connection = fn(current_time: Int64) -> ConnectionMetrics {
    ConnectionMetrics{
      state: Connected,
      connect_time_ms: 150,
      last_activity_time: current_time,
      bytes_sent: 0,
      bytes_received: 0,
      timeout_count: 0
    }
  }
  
  // 检查连接超时
  let check_connection_timeout = fn(
    connection: ConnectionMetrics, 
    current_time: Int64, 
    timeout_ms: Int
  ) -> ConnectionMetrics {
    let idle_time = current_time - connection.last_activity_time
    if idle_time > timeout_ms.to_int64() {
      { connection | state: Timeout, timeout_count: connection.timeout_count + 1 }
    } else {
      connection
    }
  }
  
  // 测试连接创建
  let current_time = 1640995200000000L
  let new_connection = create_connection(current_time)
  
  assert_eq(new_connection.state, Connected)
  assert_eq(new_connection.connect_time_ms, 150)
  assert_eq(new_connection.last_activity_time, current_time)
  assert_eq(new_connection.timeout_count, 0)
  
  // 测试连接超时检测
  let timeout_time = current_time + 40000L  // 40秒后
  let timed_out_connection = check_connection_timeout(
    new_connection, 
    timeout_time, 
    connection_pool.idle_timeout_ms
  )
  
  assert_eq(timed_out_connection.state, Timeout)
  assert_eq(timed_out_connection.timeout_count, 1)
  
  // 测试未超时的连接
  let active_time = current_time + 20000L  // 20秒后
  let active_connection = check_connection_timeout(
    new_connection, 
    active_time, 
    connection_pool.idle_timeout_ms
  )
  
  assert_eq(active_connection.state, Connected)
  assert_eq(active_connection.timeout_count, 0)
  
  // 测试连接活动更新
  let update_connection_activity = fn(
    connection: ConnectionMetrics, 
    current_time: Int64, 
    bytes_sent: Int, 
    bytes_received: Int
  ) -> ConnectionMetrics {
    { connection |
      last_activity_time: current_time,
      bytes_sent: connection.bytes_sent + bytes_sent,
      bytes_received: connection.bytes_received + bytes_received
    }
  }
  
  let updated_connection = update_connection_activity(
    new_connection,
    current_time + 10000L,
    1024,
    2048
  )
  
  assert_eq(updated_connection.last_activity_time, current_time + 10000L)
  assert_eq(updated_connection.bytes_sent, 1024)
  assert_eq(updated_connection.bytes_received, 2048)
}

test "exponential_backoff_retry_strategy" {
  // 测试指数退避重试策略
  
  struct RetryConfig {
    max_retries: Int
    base_delay_ms: Int
    max_delay_ms: Int
    backoff_multiplier: Float
    jitter_factor: Float
  }
  
  struct RetryAttempt {
    attempt_number: Int
    delay_ms: Int
    should_retry: Bool
    retry_reason: String
  }
  
  // 创建重试配置
  let retry_config = RetryConfig{
    max_retries: 5,
    base_delay_ms: 1000,
    max_delay_ms: 30000,
    backoff_multiplier: 2.0,
    jitter_factor: 0.1
  }
  
  // 计算退避延迟
  let calculate_backoff_delay = fn(attempt: Int, config: RetryConfig) -> Int {
    if attempt <= 1 {
      return 0  // 第一次尝试不延迟
    }
    
    let exponential_delay = config.base_delay_ms.to_float() * 
                           config.backoff_multiplier.pow((attempt - 1).to_float())
    let capped_delay = exponential_delay.min(config.max_delay_ms.to_float())
    
    // 添加抖动
    let jitter_range = capped_delay * config.jitter_factor
    let jitter = jitter_range * 0.5  // 简化：使用50%的抖动范围
    
    (capped_delay + jitter).to_int()
  }
  
  // 判断是否应该重试
  let should_retry = fn(attempt: Int, error_type: String, config: RetryConfig) -> (Bool, String) {
    if attempt > config.max_retries {
      return (false, "max_retries_exceeded")
    }
    
    match error_type {
      "network_timeout" => (attempt < config.max_retries, "network_timeout_retry")
      "connection_refused" => (attempt < config.max_retries, "connection_refused_retry")
      "server_error" => (attempt < 3, "server_error_retry")  // 服务器错误最多重试2次
      "rate_limited" => (attempt < config.max_retries, "rate_limited_retry")
      "client_error" => (false, "client_error_no_retry")  // 客户端错误不重试
      _ => (attempt < config.max_retries, "unknown_error_retry")
    }
  }
  
  // 测试退避延迟计算
  let delay_1 = calculate_backoff_delay(1, retry_config)
  let delay_2 = calculate_backoff_delay(2, retry_config)
  let delay_3 = calculate_backoff_delay(3, retry_config)
  let delay_4 = calculate_backoff_delay(4, retry_config)
  let delay_5 = calculate_backoff_delay(5, retry_config)
  
  assert_eq(delay_1, 0)
  assert_eq(delay_2, 1000 + 50)  // 1000ms + 10%抖动
  assert_eq(delay_3, 2000 + 100)  // 2000ms + 10%抖动
  assert_eq(delay_4, 4000 + 200)  // 4000ms + 10%抖动
  assert_eq(delay_5, 8000 + 400)  // 8000ms + 10%抖动
  
  // 验证延迟呈指数增长
  assert_eq(delay_2 < delay_3, true)
  assert_eq(delay_3 < delay_4, true)
  assert_eq(delay_4 < delay_5, true)
  
  // 测试最大延迟限制
  let large_attempt_delay = calculate_backoff_delay(10, retry_config)
  assert_eq(large_attempt_delay <= retry_config.max_delay_ms, true)
  
  // 测试重试决策
  let (retry_network, reason_network) = should_retry(1, "network_timeout", retry_config)
  assert_eq(retry_network, true)
  assert_eq(reason_network, "network_timeout_retry")
  
  let (retry_server, reason_server) = should_retry(2, "server_error", retry_config)
  assert_eq(retry_server, true)
  assert_eq(reason_server, "server_error_retry")
  
  let (retry_server_max, reason_server_max) = should_retry(3, "server_error", retry_config)
  assert_eq(retry_server_max, false)
  assert_eq(reason_server_max, "max_retries_exceeded")
  
  let (retry_client, reason_client) = should_retry(1, "client_error", retry_config)
  assert_eq(retry_client, false)
  assert_eq(reason_client, "client_error_no_retry")
  
  // 测试最大重试次数限制
  let (retry_max, reason_max) = should_retry(6, "network_timeout", retry_config)
  assert_eq(retry_max, false)
  assert_eq(reason_max, "max_retries_exceeded")
}

test "connection_pool_lifecycle" {
  // 测试连接池生命周期管理
  
  enum PoolState {
    Normal
    Draining
    Shutdown
  }
  
  struct ConnectionInfo {
    id: String
    created_time: Int64
    last_used_time: Int64
    is_active: Bool
    usage_count: Int
    max_uses: Int
  }
  
  struct ConnectionPoolManager {
    connections: Array<ConnectionInfo>
    state: PoolState
    max_connections: Int
    min_connections: Int
    connection_ttl_ms: Int
    max_idle_time_ms: Int
  }
  
  // 创建连接池管理器
  let pool_manager = ConnectionPoolManager{
    connections: Array::empty(),
    state: Normal,
    max_connections: 50,
    min_connections: 5,
    connection_ttl_ms: 300000,  // 5分钟
    max_idle_time_ms: 60000    // 1分钟
  }
  
  // 创建新连接
  let create_connection = fn(pool: ConnectionPoolManager, current_time: Int64) -> ConnectionPoolManager {
    if pool.connections.length() >= pool.max_connections {
      return pool  // 达到最大连接数
    }
    
    let new_connection = ConnectionInfo{
      id: "conn-" + pool.connections.length().to_string(),
      created_time: current_time,
      last_used_time: current_time,
      is_active: true,
      usage_count: 0,
      max_uses: 1000
    }
    
    let mut new_connections = pool.connections.to_array()
    new_connections.push(new_connection)
    
    { pool | connections: new_connections }
  }
  
  // 获取连接
  let acquire_connection = fn(pool: ConnectionPoolManager, current_time: Int64) -> (ConnectionPoolManager, Option[String]) {
    if pool.state != Normal {
      return (pool, None)
    }
    
    // 查找可用的连接
    let mut available_connection_index = -1
    let mut i = 0
    while i < pool.connections.length() {
      let conn = pool.connections[i]
      if conn.is_active && conn.usage_count < conn.max_uses {
        available_connection_index = i
        break
      }
      i = i + 1
    }
    
    if available_connection_index >= 0 {
      // 更新连接使用信息
      let mut updated_connections = pool.connections.to_array()
      let conn = updated_connections[available_connection_index]
      updated_connections[available_connection_index] = { conn |
        last_used_time: current_time,
        usage_count: conn.usage_count + 1
      }
      
      ({ pool | connections: updated_connections }, Some(conn.id))
    } else {
      // 没有可用连接，尝试创建新连接
      let expanded_pool = create_connection(pool, current_time)
      if expanded_pool.connections.length() > pool.connections.length() {
        // 成功创建新连接
        let new_conn = expanded_pool.connections[expanded_pool.connections.length() - 1]
        (expanded_pool, Some(new_conn.id))
      } else {
        // 无法创建新连接
        (pool, None)
      }
    }
  }
  
  // 清理过期连接
  let cleanup_expired_connections = fn(pool: ConnectionPoolManager, current_time: Int64) -> ConnectionPoolManager {
    let mut active_connections = Array::empty<ConnectionInfo>()
    
    let mut i = 0
    while i < pool.connections.length() {
      let conn = pool.connections[i]
      let age = current_time - conn.created_time
      let idle_time = current_time - conn.last_used_time
      
      let is_expired = age > pool.connection_ttl_ms.to_int64() || 
                       idle_time > pool.max_idle_time_ms.to_int64() ||
                       conn.usage_count >= conn.max_uses
      
      if !is_expired {
        active_connections.push(conn)
      }
      i = i + 1
    }
    
    // 确保至少保留最小连接数
    while active_connections.length() < pool.min_connections && pool.state == Normal {
      let new_conn = ConnectionInfo{
        id: "conn-min-" + active_connections.length().to_string(),
        created_time: current_time,
        last_used_time: current_time,
        is_active: true,
        usage_count: 0,
        max_uses: 1000
      }
      active_connections.push(new_conn)
    }
    
    { pool | connections: active_connections }
  }
  
  // 测试连接创建
  let current_time = 1640995200000000L
  let pool_with_connections = create_connection(pool_manager, current_time)
  
  assert_eq(pool_with_connections.connections.length(), 1)
  assert_eq(pool_with_connections.connections[0].is_active, true)
  assert_eq(pool_with_connections.connections[0].usage_count, 0)
  
  // 测试连接获取
  let (pool_after_acquire, conn_id) = acquire_connection(pool_with_connections, current_time + 1000L)
  
  match conn_id {
    Some(id) => {
      assert_eq(id, "conn-0")
      let conn = pool_after_acquire.connections.find(fn(c) { c.id == id })
      match conn {
        Some(c) => {
          assert_eq(c.usage_count, 1)
          assert_eq(c.last_used_time, current_time + 1000L)
        }
        None => assert_eq(false, true)
      }
    }
    None => assert_eq(false, true)
  }
  
  // 测试连接清理
  let old_time = current_time - 400000L  // 连接创建时间很早
  let old_connection = ConnectionInfo{
    id: "old-conn",
    created_time: old_time,
    last_used_time: old_time,
    is_active: true,
    usage_count: 500,
    max_uses: 1000
  }
  
  let pool_with_old_connection = { pool_after_acquire | connections: pool_after_acquire.connections.concat([old_connection]) }
  let cleaned_pool = cleanup_expired_connections(pool_with_old_connection, current_time)
  
  // 旧连接应该被清理（因为超过最大空闲时间）
  let remaining_old_conn = cleaned_pool.connections.find(fn(c) { c.id == "old-conn" })
  match remaining_old_conn {
    Some(_) => assert_eq(false, true)
    None => assert_eq(true, true)
  }
  
  // 测试最小连接数保证
  let empty_pool = { pool_manager | connections: Array::empty() }
  let pool_with_min_connections = cleanup_expired_connections(empty_pool, current_time)
  
  assert_eq(pool_with_min_connections.connections.length(), pool_manager.min_connections)
}

test "network_bandwidth_throttling" {
  // 测试网络带宽限制
  
  struct BandwidthConfig {
    max_bytes_per_second: Int
    burst_bytes: Int
    refill_rate_per_second: Int
    window_size_ms: Int
  }
  
  struct BandwidthState {
    available_bytes: Int
    last_refill_time: Int64
    total_bytes_sent: Int
    total_bytes_dropped: Int
  }
  
  struct ThrottlingResult {
    bytes_allowed: Int
    bytes_dropped: Int
    wait_time_ms: Int
    state: BandwidthState
  }
  
  // 创建带宽配置
  let bandwidth_config = BandwidthConfig{
    max_bytes_per_second: 1024 * 1024,  // 1MB/s
    burst_bytes: 2 * 1024 * 1024,      // 2MB 突发
    refill_rate_per_second: 1024 * 1024, // 1MB/s 补充速率
    window_size_ms: 1000                // 1秒窗口
  }
  
  // 初始化带宽状态
  let initial_bandwidth_state = BandwidthState{
    available_bytes: bandwidth_config.burst_bytes,
    last_refill_time: 1640995200000000L,
    total_bytes_sent: 0,
    total_bytes_dropped: 0
  }
  
  // 补充带宽令牌
  let refill_bandwidth = fn(state: BandwidthState, current_time: Int64, config: BandwidthConfig) -> BandwidthState {
    let time_diff_ms = (current_time - state.last_refill_time) / 1000000L
    let time_diff_seconds = time_diff_ms.to_int() / 1000
    
    if time_diff_seconds > 0 {
      let refill_bytes = time_diff_seconds * config.refill_rate_per_second
      let new_available = (state.available_bytes + refill_bytes).min(config.burst_bytes)
      
      { state |
        available_bytes: new_available,
        last_refill_time: current_time
      }
    } else {
      state
    }
  }
  
  // 尝试发送数据
  let try_send_data = fn(
    state: BandwidthState, 
    requested_bytes: Int, 
    current_time: Int64, 
    config: BandwidthConfig
  ) -> ThrottlingResult {
    let refilled_state = refill_bandwidth(state, current_time, config)
    
    if requested_bytes <= refilled_state.available_bytes {
      // 允许发送
      let new_state = { refilled_state |
        available_bytes: refilled_state.available_bytes - requested_bytes,
        total_bytes_sent: refilled_state.total_bytes_sent + requested_bytes
      }
      
      ThrottlingResult{
        bytes_allowed: requested_bytes,
        bytes_dropped: 0,
        wait_time_ms: 0,
        state: new_state
      }
    } else {
      // 部分或全部丢弃
      let allowed_bytes = refilled_state.available_bytes
      let dropped_bytes = requested_bytes - allowed_bytes
      
      // 计算等待时间
      let wait_time_ms = if dropped_bytes > 0 {
        (dropped_bytes.to_float() / config.refill_rate_per_second.to_float() * 1000.0).to_int()
      } else {
        0
      }
      
      let new_state = { refilled_state |
        available_bytes: 0,
        total_bytes_sent: refilled_state.total_bytes_sent + allowed_bytes,
        total_bytes_dropped: refilled_state.total_bytes_dropped + dropped_bytes
      }
      
      ThrottlingResult{
        bytes_allowed: allowed_bytes,
        bytes_dropped: dropped_bytes,
        wait_time_ms: wait_time_ms,
        state: new_state
      }
    }
  }
  
  // 测试初始状态
  assert_eq(initial_bandwidth_state.available_bytes, 2 * 1024 * 1024)
  assert_eq(initial_bandwidth_state.total_bytes_sent, 0)
  assert_eq(initial_bandwidth_state.total_bytes_dropped, 0)
  
  // 测试正常发送
  let current_time = 1640995200000000L
  let result1 = try_send_data(initial_bandwidth_state, 512 * 1024, current_time, bandwidth_config)  // 512KB
  
  assert_eq(result1.bytes_allowed, 512 * 1024)
  assert_eq(result1.bytes_dropped, 0)
  assert_eq(result1.wait_time_ms, 0)
  assert_eq(result1.state.available_bytes, 2 * 1024 * 1024 - 512 * 1024)
  assert_eq(result1.state.total_bytes_sent, 512 * 1024)
  
  // 测试突发发送
  let result2 = try_send_data(result1.state, 2 * 1024 * 1024, current_time + 1000L, bandwidth_config)  // 2MB
  
  assert_eq(result2.bytes_allowed, 1536 * 1024)  // 剩余可用字节
  assert_eq(result2.bytes_dropped, 512 * 1024)   // 超出部分被丢弃
  assert_eq(result2.wait_time_ms > 0, true)      // 需要等待
  assert_eq(result2.state.available_bytes, 0)
  assert_eq(result2.state.total_bytes_sent, 512 * 1024 + 1536 * 1024)
  assert_eq(result2.state.total_bytes_dropped, 512 * 1024)
  
  // 测试带宽补充
  let after_refill_time = current_time + 2000L  // 2秒后
  let refilled_state = refill_bandwidth(result2.state, after_refill_time, bandwidth_config)
  
  assert_eq(refilled_state.available_bytes, 2 * 1024 * 1024)  // 补充到最大突发值
  assert_eq(refilled_state.last_refill_time, after_refill_time)
  
  // 测试补充后发送
  let result3 = try_send_data(refilled_state, 1024 * 1024, after_refill_time + 1000L, bandwidth_config)  // 1MB
  
  assert_eq(result3.bytes_allowed, 1024 * 1024)
  assert_eq(result3.bytes_dropped, 0)
  assert_eq(result3.wait_time_ms, 0)
  assert_eq(result3.state.available_bytes, 1024 * 1024)
  
  // 测试等待时间计算
  let large_request = 3 * 1024 * 1024  // 3MB请求
  let result4 = try_send_data(result3.state, large_request, after_refill_time + 2000L, bandwidth_config)
  
  assert_eq(result4.bytes_allowed, 1024 * 1024)  // 只能发送剩余的1MB
  assert_eq(result4.bytes_dropped, 2 * 1024 * 1024)  // 2MB被丢弃
  assert_eq(result4.wait_time_ms > 0, true)  // 需要等待
  
  // 验证等待时间合理性（2MB / 1MB/s = 2秒 = 2000ms）
  assert_eq(result4.wait_time_ms >= 1900 && result4.wait_time_ms <= 2100, true)
}

test "network_compression_optimization" {
  // 测试网络压缩优化
  
  enum CompressionType {
    None
    Gzip
    Lz4
    Zstd
  }
  
  struct CompressionConfig {
    compression_type: CompressionType
    compression_level: Int
    min_size_to_compress: Int
    max_compression_time_ms: Int
  }
  
  struct CompressionResult {
    original_size: Int
    compressed_size: Int
    compression_ratio: Float
    compression_time_ms: Int
    should_compress: Bool
  }
  
  // 创建压缩配置
  let compression_configs = [
    CompressionConfig{
      compression_type: None,
      compression_level: 0,
      min_size_to_compress: 0,
      max_compression_time_ms: 0
    },
    CompressionConfig{
      compression_type: Gzip,
      compression_level: 6,
      min_size_to_compress: 1024,  // 1KB以上才压缩
      max_compression_time_ms: 100
    },
    CompressionConfig{
      compression_type: Lz4,
      compression_level: 1,
      min_size_to_compress: 512,   // 512B以上才压缩
      max_compression_time_ms: 50
    },
    CompressionConfig{
      compression_type: Zstd,
      compression_level: 3,
      min_size_to_compress: 2048,  // 2KB以上才压缩
      max_compression_time_ms: 200
    }
  ]
  
  // 模拟压缩函数
  let simulate_compression = fn(data_size: Int, config: CompressionConfig) -> CompressionResult {
    let start_time = 1640995200000000L
    
    if data_size < config.min_size_to_compress {
      return CompressionResult{
        original_size: data_size,
        compressed_size: data_size,
        compression_ratio: 1.0,
        compression_time_ms: 0,
        should_compress: false
      }
    }
    
    // 模拟不同压缩算法的压缩比和时间
    let (ratio, time) = match config.compression_type {
      None => (1.0, 0)
      Gzip => (0.3, config.compression_level * 10)
      Lz4 => (0.5, config.compression_level * 5)
      Zstd => (0.25, config.compression_level * 15)
    }
    
    let compressed_size = (data_size.to_float() * ratio).to_int()
    let compression_time_ms = time + (data_size / 10240)  // 基础时间 + 数据大小相关时间
    
    let should_compress = compressed_size < data_size && 
                         compression_time_ms <= config.max_compression_time_ms
    
    CompressionResult{
      original_size: data_size,
      compressed_size: compressed_size,
      compression_ratio: ratio,
      compression_time_ms: compression_time_ms,
      should_compress: should_compress
    }
  }
  
  // 测试不同大小的数据压缩
  let data_sizes = [256, 512, 1024, 4096, 16384]  // 256B, 512B, 1KB, 4KB, 16KB
  
  let mut i = 0
  while i < data_sizes.length() {
    let data_size = data_sizes[i]
    
    let mut j = 0
    while j < compression_configs.length() {
      let config = compression_configs[j]
      let result = simulate_compression(data_size, config)
      
      // 验证压缩结果
      assert_eq(result.original_size, data_size)
      assert_eq(result.compression_ratio > 0.0, true)
      assert_eq(result.compression_time_ms >= 0, true)
      
      // 验证压缩决策
      if data_size < config.min_size_to_compress {
        assert_eq(result.should_compress, false)
        assert_eq(result.compressed_size, data_size)
      } else if config.compression_type == None {
        assert_eq(result.should_compress, false)
        assert_eq(result.compressed_size, data_size)
      } else {
        // 对于有效压缩，压缩后大小应该小于原始大小
        if result.should_compress {
          assert_eq(result.compressed_size < data_size, true)
        }
      }
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 测试最佳压缩算法选择
  let select_best_compression = fn(data_size: Int, configs: Array<CompressionConfig>) -> (CompressionConfig, CompressionResult) {
    let mut best_config = configs[0]
    let mut best_result = simulate_compression(data_size, configs[0])
    let mut best_score = -1.0
    
    let mut i = 0
    while i < configs.length() {
      let config = configs[i]
      let result = simulate_compression(data_size, config)
      
      if result.should_compress {
        // 计算压缩评分：压缩比 / 压缩时间
        let score = (1.0 - result.compression_ratio) / result.compression_time_ms.to_float()
        if score > best_score {
          best_score = score
          best_config = config
          best_result = result
        }
      }
      
      i = i + 1
    }
    
    (best_config, best_result)
  }
  
  // 测试不同数据大小的最佳压缩选择
  let (best_config_1kb, best_result_1kb) = select_best_compression(1024, compression_configs)
  let (best_config_16kb, best_result_16kb) = select_best_compression(16384, compression_configs)
  
  // 验证最佳压缩选择
  assert_eq(best_result_1kb.should_compress, true)
  assert_eq(best_result_1kb.compressed_size < 1024, true)
  
  assert_eq(best_result_16kb.should_compress, true)
  assert_eq(best_result_16kb.compressed_size < 16384, true)
  
  // 较大的数据应该有更好的压缩比
  assert_eq(best_result_16kb.compression_ratio <= best_result_1kb.compression_ratio, true)
}