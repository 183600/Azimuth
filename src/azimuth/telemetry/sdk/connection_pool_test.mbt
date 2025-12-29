// 连接池管理测试用例
// 测试连接池的生命周期管理、负载均衡、故障恢复、性能优化等高级连接池功能

test "connection_pool_lifecycle_management" {
  // 测试连接池的生命周期管理
  
  enum ConnectionState {
    Idle
    Active
    Busy
    Closing
    Closed
    Error
  }
  
  struct Connection {
    id: String
    state: ConnectionState
    created_time: Int64
    last_used_time: Int64
    usage_count: Int
    max_uses: Int
    error_count: Int
    max_errors: Int
  }
  
  struct ConnectionPoolConfig {
    min_connections: Int
    max_connections: Int
    connection_timeout_ms: Int
    idle_timeout_ms: Int
    max_lifetime_ms: Int
    health_check_interval_ms: Int
  }
  
  struct ConnectionPool {
    connections: Array<Connection>
    config: ConnectionPoolConfig
    active_connections: Int
    total_created: Int
    total_destroyed: Int
    last_health_check: Int64
  }
  
  // 创建连接池配置
  let pool_config = ConnectionPoolConfig{
    min_connections: 5,
    max_connections: 50,
    connection_timeout_ms: 5000,
    idle_timeout_ms: 30000,
    max_lifetime_ms: 300000,  // 5分钟
    health_check_interval_ms: 10000
  }
  
  // 创建连接池
  let connection_pool = ConnectionPool{
    connections: Array::empty(),
    config: pool_config,
    active_connections: 0,
    total_created: 0,
    total_destroyed: 0,
    last_health_check: 0L
  }
  
  // 创建新连接
  let create_connection = fn(pool: ConnectionPool, current_time: Int64) -> ConnectionPool {
    if pool.connections.length() >= pool.config.max_connections {
      return pool  // 达到最大连接数
    }
    
    let new_connection = Connection{
      id: "conn-" + pool.total_created.to_string(),
      state: Idle,
      created_time: current_time,
      last_used_time: current_time,
      usage_count: 0,
      max_uses: 1000,
      error_count: 0,
      max_errors: 10
    }
    
    let mut new_connections = pool.connections.to_array()
    new_connections.push(new_connection)
    
    { pool |
      connections: new_connections,
      total_created: pool.total_created + 1
    }
  }
  
  // 获取连接
  let acquire_connection = fn(pool: ConnectionPool, current_time: Int64) -> (ConnectionPool, Option[String]) {
    let mut available_connection_index = -1
    let mut i = 0
    while i < pool.connections.length() {
      let conn = pool.connections[i]
      if conn.state == Idle || conn.state == Active {
        available_connection_index = i
        break
      }
      i = i + 1
    }
    
    if available_connection_index >= 0 {
      // 更新连接状态
      let mut new_connections = pool.connections.to_array()
      let conn = new_connections[available_connection_index]
      new_connections[available_connection_index] = { conn |
        state: Busy,
        last_used_time: current_time,
        usage_count: conn.usage_count + 1
      }
      
      let updated_pool = { pool |
        connections: new_connections,
        active_connections: pool.active_connections + 1
      }
      
      (updated_pool, Some(conn.id))
    } else {
      // 没有可用连接，尝试创建新连接
      let expanded_pool = create_connection(pool, current_time)
      if expanded_pool.total_created > pool.total_created {
        // 成功创建新连接
        let new_conn = expanded_pool.connections[expanded_pool.connections.length() - 1]
        let mut final_connections = expanded_pool.connections.to_array()
        final_connections[final_connections.length() - 1] = { new_conn | state: Busy }
        
        let final_pool = { expanded_pool |
          connections: final_connections,
          active_connections: expanded_pool.active_connections + 1
        }
        
        (final_pool, Some(new_conn.id))
      } else {
        // 无法创建新连接
        (pool, None)
      }
    }
  }
  
  // 释放连接
  let release_connection = fn(pool: ConnectionPool, connection_id: String, current_time: Int64) -> ConnectionPool {
    let mut new_connections = Array::empty<Connection>()
    let mut found = false
    
    let mut i = 0
    while i < pool.connections.length() {
      let conn = pool.connections[i]
      if conn.id == connection_id {
        found = true
        let updated_conn = { conn |
          state: Active,
          last_used_time: current_time
        }
        new_connections.push(updated_conn)
      } else {
        new_connections.push(conn)
      }
      i = i + 1
    }
    
    if found {
      { pool |
        connections: new_connections,
        active_connections: pool.active_connections - 1
      }
    } else {
      pool
    }
  }
  
  // 测试连接池初始化
  let current_time = 1640995200000000L
  let mut initialized_pool = connection_pool
  
  // 创建最小连接数
  let mut i = 0
  while i < pool_config.min_connections {
    initialized_pool = create_connection(initialized_pool, current_time + i.to_int64() * 1000L)
    i = i + 1
  }
  
  assert_eq(initialized_pool.connections.length(), pool_config.min_connections)
  assert_eq(initialized_pool.total_created, pool_config.min_connections)
  
  // 测试获取连接
  let (pool_after_acquire, conn_id_1) = acquire_connection(initialized_pool, current_time + 10000L)
  assert_eq(conn_id_1.is_some(), true)
  assert_eq(pool_after_acquire.active_connections, 1)
  
  let acquired_conn = pool_after_acquire.connections.find(fn(c) { c.id == conn_id_1.unwrap() })
  match acquired_conn {
    Some(conn) => {
      assert_eq(conn.state, Busy)
      assert_eq(conn.usage_count, 1)
    }
    None => assert_eq(false, true)
  }
  
  // 测试释放连接
  let pool_after_release = release_connection(pool_after_acquire, conn_id_1.unwrap(), current_time + 20000L)
  assert_eq(pool_after_release.active_connections, 0)
  
  let released_conn = pool_after_release.connections.find(fn(c) { c.id == conn_id_1.unwrap() })
  match released_conn {
    Some(conn) => {
      assert_eq(conn.state, Active)
      assert_eq(conn.last_used_time, current_time + 20000L)
    }
    None => assert_eq(false, true)
  }
  
  // 测试连接池扩容
  let mut expanded_pool = pool_after_release
  let mut conn_ids = Array::empty<String>()
  
  // 获取超过现有连接数的连接
  let mut j = 0
  while j < pool_config.min_connections + 5 {
    let (temp_pool, conn_id) = acquire_connection(expanded_pool, current_time + 30000L + j.to_int64() * 1000L)
    match conn_id {
      Some(id) => conn_ids.push(id)
      None => {}
    }
    expanded_pool = temp_pool
    j = j + 1
  }
  
  assert_eq(expanded_pool.connections.length() > pool_config.min_connections, true)
  assert_eq(expanded_pool.total_created > pool_config.min_connections, true)
}

test "connection_pool_health_checking" {
  // 测试连接池健康检查
  
  struct HealthCheckResult {
    connection_id: String
    is_healthy: Bool
    response_time_ms: Int
    error_message: Option<String>
  }
  
  struct HealthCheckConfig {
    enabled: Bool
    interval_ms: Int
    timeout_ms: Int
    max_failures: Int
    check_on_acquire: Bool
    check_on_release: Bool
  }
  
  // 模拟健康检查
  let perform_health_check = fn(connection: Connection, config: HealthCheckConfig) -> HealthCheckResult {
    // 简化的健康检查逻辑
    let is_healthy = connection.state != Error && 
                     connection.error_count < connection.max_errors &&
                     connection.usage_count < connection.max_uses
    
    let response_time_ms = if is_healthy {
      50 + connection.usage_count * 2  // 响应时间随使用次数增加
    } else {
      5000  // 错误连接的响应时间
    }
    
    let error_message = if !is_healthy {
      Some("Connection unhealthy")
    } else {
      None
    }
    
    HealthCheckResult{
      connection_id: connection.id,
      is_healthy: is_healthy,
      response_time_ms: response_time_ms,
      error_message: error_message
    }
  }
  
  // 批量健康检查
  let batch_health_check = fn(
    pool: ConnectionPool, 
    config: HealthCheckConfig,
    current_time: Int64
  ) -> (ConnectionPool, Array<HealthCheckResult]) {
    let mut results = Array::empty<HealthCheckResult>()
    let mut unhealthy_connections = Array::empty<String>()
    
    let mut i = 0
    while i < pool.connections.length() {
      let conn = pool.connections[i]
      let result = perform_health_check(conn, config)
      results.push(result)
      
      if !result.is_healthy {
        unhealthy_connections.push(conn.id)
      }
      
      i = i + 1
    }
    
    // 移除不健康的连接
    let mut healthy_connections = Array::empty<Connection>()
    let mut j = 0
    while j < pool.connections.length() {
      let conn = pool.connections[j]
      if !unhealthy_connections.contains(conn.id) {
        healthy_connections.push(conn)
      }
      j = j + 1
    }
    
    let updated_pool = { pool |
      connections: healthy_connections,
      total_destroyed: pool.total_destroyed + unhealthy_connections.length()
    }
    
    (updated_pool, results)
  }
  
  // 创建测试连接池
  let pool_config = ConnectionPoolConfig{
    min_connections: 3,
    max_connections: 10,
    connection_timeout_ms: 5000,
    idle_timeout_ms: 30000,
    max_lifetime_ms: 300000,
    health_check_interval_ms: 10000
  }
  
  let health_config = HealthCheckConfig{
    enabled: true,
    interval_ms: 10000,
    timeout_ms: 5000,
    max_failures: 3,
    check_on_acquire: true,
    check_on_release: false
  }
  
  let current_time = 1640995200000000L
  
  // 创建不同健康状态的连接
  let healthy_conn = Connection{
    id: "healthy-conn",
    state: Active,
    created_time: current_time - 1000000L,
    last_used_time: current_time - 100000L,
    usage_count: 10,
    max_uses: 1000,
    error_count: 0,
    max_errors: 10
  }
  
  let unhealthy_conn_1 = Connection{
    id: "unhealthy-conn-1",
    state: Error,
    created_time: current_time - 2000000L,
    last_used_time: current_time - 500000L,
    usage_count: 100,
    max_uses: 1000,
    error_count: 5,
    max_errors: 10
  }
  
  let unhealthy_conn_2 = Connection{
    id: "unhealthy-conn-2",
    state: Active,
    created_time: current_time - 3000000L,
    last_used_time: current_time - 200000L,
    usage_count: 999,  // 接近最大使用次数
    max_uses: 1000,
    error_count: 0,
    max_errors: 10
  }
  
  let test_pool = ConnectionPool{
    connections: [healthy_conn, unhealthy_conn_1, unhealthy_conn_2],
    config: pool_config,
    active_connections: 0,
    total_created: 3,
    total_destroyed: 0,
    last_health_check: current_time - 15000L
  }
  
  // 执行健康检查
  let (pool_after_health_check, health_results) = batch_health_check(test_pool, health_config, current_time)
  
  // 验证健康检查结果
  assert_eq(health_results.length(), 3)
  
  let healthy_result = health_results.find(fn(r) { r.connection_id == "healthy-conn" })
  match healthy_result {
    Some(result) => {
      assert_eq(result.is_healthy, true)
      assert_eq(result.response_time_ms < 100, true)
      assert_eq(result.error_message, None)
    }
    None => assert_eq(false, true)
  }
  
  let unhealthy_result_1 = health_results.find(fn(r) { r.connection_id == "unhealthy-conn-1" })
  match unhealthy_result_1 {
    Some(result) => {
      assert_eq(result.is_healthy, false)
      assert_eq(result.error_message.is_some(), true)
    }
    None => assert_eq(false, true)
  }
  
  let unhealthy_result_2 = health_results.find(fn(r) { r.connection_id == "unhealthy-conn-2" })
  match unhealthy_result_2 {
    Some(result) => {
      assert_eq(result.is_healthy, false)  // 使用次数过多
      assert_eq(result.error_message.is_some(), true)
    }
    None => assert_eq(false, true)
  }
  
  // 验证不健康连接被移除
  assert_eq(pool_after_health_check.connections.length(), 1)
  assert_eq(pool_after_health_check.connections[0].id, "healthy-conn")
  assert_eq(pool_after_health_check.total_destroyed, 2)
}

test "connection_pool_load_balancing" {
  // 测试连接池负载均衡
  
  enum LoadBalancingStrategy {
    RoundRobin
    LeastConnections
    WeightedRoundRobin
    Random
    ResponseTime
  }
  
  struct LoadBalancer {
    strategy: LoadBalancingStrategy
    round_robin_index: Int
    connection_weights: Array<(String, Int)>  // (connection_id, weight)
  }
  
  struct ConnectionMetrics {
    connection_id: String
    active_requests: Int
    total_requests: Int
    average_response_time_ms: Float
    error_rate: Float
  }
  
  // 创建负载均衡器
  let create_load_balancer = fn(strategy: LoadBalancingStrategy) -> LoadBalancer {
    LoadBalancer{
      strategy: strategy,
      round_robin_index: 0,
      connection_weights: Array::empty()
    }
  }
  
  // 选择连接
  let select_connection = fn(
    balancer: LoadBalancer,
    available_connections: Array<Connection>,
    metrics: Array<ConnectionMetrics>
  ) -> (LoadBalancer, Option<String>) {
    if available_connections.length() == 0 {
      return (balancer, None)
    }
    
    match balancer.strategy {
      RoundRobin => {
        let index = balancer.round_robin_index % available_connections.length()
        let selected_conn = available_connections[index]
        let updated_balancer = { balancer | round_robin_index: balancer.round_robin_index + 1 }
        (updated_balancer, Some(selected_conn.id))
      }
      LeastConnections => {
        let mut min_requests = 999999
        let mut selected_index = 0
        
        let mut i = 0
        while i < available_connections.length() {
          let conn = available_connections[i]
          let conn_metrics = metrics.find(fn(m) { m.connection_id == conn.id })
          
          let active_requests = match conn_metrics {
            Some(m) => m.active_requests
            None => 0
          }
          
          if active_requests < min_requests {
            min_requests = active_requests
            selected_index = i
          }
          
          i = i + 1
        }
        
        (balancer, Some(available_connections[selected_index].id))
      }
      WeightedRoundRobin => {
        // 简化的加权轮询
        let total_weight = balancer.connection_weights.fold(0, fn(acc, w) { acc + w.1 })
        if total_weight == 0 {
          // 如果没有权重，回退到普通轮询
          let index = balancer.round_robin_index % available_connections.length()
          let selected_conn = available_connections[index]
          let updated_balancer = { balancer | round_robin_index: balancer.round_robin_index + 1 }
          (updated_balancer, Some(selected_conn.id))
        } else {
          // 简化实现：选择第一个可用连接
          (balancer, Some(available_connections[0].id))
        }
      }
      Random => {
        let random_index = (available_connections.length() * 7) % available_connections.length()  // 简化随机
        (balancer, Some(available_connections[random_index].id))
      }
      ResponseTime => {
        let mut best_index = 0
        let mut best_response_time = 999999.0
        
        let mut i = 0
        while i < available_connections.length() {
          let conn = available_connections[i]
          let conn_metrics = metrics.find(fn(m) { m.connection_id == conn.id })
          
          let response_time = match conn_metrics {
            Some(m) => m.average_response_time_ms
            None => 100.0
          }
          
          if response_time < best_response_time {
            best_response_time = response_time
            best_index = i
          }
          
          i = i + 1
        }
        
        (balancer, Some(available_connections[best_index].id))
      }
    }
  }
  
  // 创建测试连接
  let current_time = 1640995200000000L
  let test_connections = [
    Connection{
      id: "conn-1",
      state: Active,
      created_time: current_time - 1000000L,
      last_used_time: current_time - 100000L,
      usage_count: 10,
      max_uses: 1000,
      error_count: 0,
      max_errors: 10
    },
    Connection{
      id: "conn-2",
      state: Active,
      created_time: current_time - 2000000L,
      last_used_time: current_time - 200000L,
      usage_count: 20,
      max_uses: 1000,
      error_count: 1,
      max_errors: 10
    },
    Connection{
      id: "conn-3",
      state: Active,
      created_time: current_time - 3000000L,
      last_used_time: current_time - 300000L,
      usage_count: 30,
      max_uses: 1000,
      error_count: 0,
      max_errors: 10
    }
  ]
  
  // 创建连接指标
  let connection_metrics = [
    ConnectionMetrics{
      connection_id: "conn-1",
      active_requests: 5,
      total_requests: 100,
      average_response_time_ms: 50.0,
      error_rate: 0.01
    },
    ConnectionMetrics{
      connection_id: "conn-2",
      active_requests: 2,
      total_requests: 80,
      average_response_time_ms: 30.0,
      error_rate: 0.02
    },
    ConnectionMetrics{
      connection_id: "conn-3",
      active_requests: 8,
      total_requests: 120,
      average_response_time_ms: 80.0,
      error_rate: 0.00
    }
  ]
  
  // 测试不同负载均衡策略
  let strategies = [RoundRobin, LeastConnections, WeightedRoundRobin, Random, ResponseTime]
  
  let mut i = 0
  while i < strategies.length() {
    let strategy = strategies[i]
    let balancer = create_load_balancer(strategy)
    
    // 执行多次选择以验证策略
    let mut selections = Array::empty<String>()
    let mut current_balancer = balancer
    
    let mut j = 0
    while j < 10 {
      let (updated_balancer, selected_conn) = select_connection(
        current_balancer, 
        test_connections, 
        connection_metrics
      )
      
      match selected_conn {
        Some(conn_id) => selections.push(conn_id)
        None => {}
      }
      
      current_balancer = updated_balancer
      j = j + 1
    }
    
    // 验证选择结果
    assert_eq(selections.length(), 10)
    
    // 验证策略特定的行为
    match strategy {
      RoundRobin => {
        // 轮询策略应该相对均匀分布
        let conn1_count = selections.filter(fn(s) { s == "conn-1" }).length()
        let conn2_count = selections.filter(fn(s) { s == "conn-2" }).length()
        let conn3_count = selections.filter(fn(s) { s == "conn-3" }).length()
        
        assert_eq(conn1_count >= 2 && conn1_count <= 5, true)
        assert_eq(conn2_count >= 2 && conn2_count <= 5, true)
        assert_eq(conn3_count >= 2 && conn3_count <= 5, true)
      }
      LeastConnections => {
        // 最少连接策略应该优先选择conn-2（只有2个活跃请求）
        let first_selection = selections[0]
        assert_eq(first_selection == "conn-2", true)
      }
      ResponseTime => {
        // 响应时间策略应该优先选择conn-2（30ms响应时间最快）
        let first_selection = selections[0]
        assert_eq(first_selection == "conn-2", true)
      }
      _ => {
        // 其他策略应该选择有效的连接
        let valid_selections = selections.filter(fn(s) { 
          s == "conn-1" || s == "conn-2" || s == "conn-3" 
        })
        assert_eq(valid_selections.length(), 10)
      }
    }
    
    i = i + 1
  }
}

test "connection_pool_fault_tolerance" {
  // 测试连接池容错机制
  
  enum FailureType {
    ConnectionTimeout
    ConnectionRefused
    NetworkError
    ProtocolError
    ResourceExhausted
  }
  
  struct FailureScenario {
    failure_type: FailureType
    failure_rate: Float
    recovery_time_ms: Int
    affected_connections: Array<String>
  }
  
  struct FaultToleranceConfig {
    max_retry_attempts: Int
    retry_delay_ms: Int
    circuit_breaker_threshold: Int
    failure_detection_window_ms: Int
    automatic_recovery_enabled: Bool
  }
  
  struct CircuitBreakerState {
    is_open: Bool
    failure_count: Int
    last_failure_time: Int64
    next_attempt_time: Int64
  }
  
  // 创建容错配置
  let fault_config = FaultToleranceConfig{
    max_retry_attempts: 3,
    retry_delay_ms: 1000,
    circuit_breaker_threshold: 5,
    failure_detection_window_ms: 10000,
    automatic_recovery_enabled: true
  }
  
  // 模拟连接故障
  let simulate_connection_failure = fn(
    connection: Connection, 
    scenario: FailureScenario,
    current_time: Int64
  ) -> Connection {
    if scenario.affected_connections.contains(connection.id) {
      // 简化的随机故障模拟
      let random_factor = (connection.id.to_int() % 100).to_float() / 100.0
      let should_fail = random_factor < scenario.failure_rate
      
      if should_fail {
        { connection |
          state: Error,
          error_count: connection.error_count + 1,
          last_used_time: current_time
        }
      } else {
        connection
      }
    } else {
      connection
    }
  }
  
  // 检查熔断器状态
  let check_circuit_breaker = fn(
    breaker: CircuitBreakerState,
    failure_count: Int,
    current_time: Int64,
    config: FaultToleranceConfig
  ) -> CircuitBreakerState {
    if breaker.is_open {
      // 检查是否可以尝试恢复
      if current_time >= breaker.next_attempt_time {
        // 半开状态，允许一次尝试
        { breaker |
          is_open: false,
          next_attempt_time: current_time + config.retry_delay_ms.to_int64() * 1000000L
        }
      } else {
        breaker
      }
    } else {
      // 检查是否应该打开熔断器
      if failure_count >= config.circuit_breaker_threshold {
        { breaker |
          is_open: true,
          failure_count: failure_count,
          last_failure_time: current_time,
          next_attempt_time: current_time + config.retry_delay_ms.to_int64() * 1000000L
        }
      } else {
        { breaker | failure_count: failure_count }
      }
    }
  }
  
  // 故障恢复
  let attempt_connection_recovery = fn(
    connection: Connection,
    current_time: Int64,
    config: FaultToleranceConfig
  ) -> Connection {
    if connection.state == Error && config.automatic_recovery_enabled {
      // 检查是否可以尝试恢复
      let time_since_last_failure = current_time - connection.last_used_time
      let recovery_time_ms = config.retry_delay_ms * (connection.error_count + 1)
      
      if time_since_last_failure >= recovery_time_ms.to_int64() * 1000000L {
        // 尝试恢复连接
        { connection |
          state: Active,
          error_count: 0,
          last_used_time: current_time
        }
      } else {
        connection
      }
    } else {
      connection
    }
  }
  
  // 创建测试场景
  let current_time = 1640995200000000L
  
  let test_connections = [
    Connection{
      id: "conn-1",
      state: Active,
      created_time: current_time - 1000000L,
      last_used_time: current_time - 100000L,
      usage_count: 10,
      max_uses: 1000,
      error_count: 0,
      max_errors: 10
    },
    Connection{
      id: "conn-2",
      state: Active,
      created_time: current_time - 2000000L,
      last_used_time: current_time - 200000L,
      usage_count: 20,
      max_uses: 1000,
      error_count: 2,
      max_errors: 10
    },
    Connection{
      id: "conn-3",
      state: Active,
      created_time: current_time - 3000000L,
      last_used_time: current_time - 300000L,
      usage_count: 30,
      max_uses: 1000,
      error_count: 0,
      max_errors: 10
    }
  ]
  
  // 场景1：网络错误
  let network_failure_scenario = FailureScenario{
    failure_type: NetworkError,
    failure_rate: 0.8,  // 80%失败率
    recovery_time_ms: 5000,
    affected_connections: ["conn-1", "conn-2"]
  }
  
  let mut failed_connections = Array::empty<Connection>()
  let mut i = 0
  while i < test_connections.length() {
    let failed_conn = simulate_connection_failure(
      test_connections[i], 
      network_failure_scenario, 
      current_time
    )
    failed_connections.push(failed_conn)
    i = i + 1
  }
  
  // 验证故障模拟
  let conn1_after_failure = failed_connections.find(fn(c) { c.id == "conn-1" })
  let conn2_after_failure = failed_connections.find(fn(c) { c.id == "conn-2" })
  let conn3_after_failure = failed_connections.find(fn(c) { c.id == "conn-3" })
  
  // conn-1和conn-2应该有故障（取决于随机因子）
  // conn-3不应该有故障（不在受影响列表中）
  match conn3_after_failure {
    Some(conn) => assert_eq(conn.state, Active)
    None => assert_eq(false, true)
  }
  
  // 场景2：熔断器测试
  let circuit_breaker = CircuitBreakerState{
    is_open: false,
    failure_count: 0,
    last_failure_time: 0L,
    next_attempt_time: 0L
  }
  
  // 模拟连续失败
  let mut current_breaker = circuit_breaker
  let mut failure_count = 0
  
  let mut j = 0
  while j < 6 {  // 超过阈值
    current_breaker = check_circuit_breaker(
      current_breaker,
      failure_count,
      current_time + j.to_int64() * 1000000L,
      fault_config
    )
    failure_count = failure_count + 1
    j = j + 1
  }
  
  // 验证熔断器打开
  assert_eq(current_breaker.is_open, true)
  assert_eq(current_breaker.failure_count, 5)
  
  // 场景3：故障恢复
  let error_connection = Connection{
    id: "conn-error",
    state: Error,
    created_time: current_time - 5000000L,
    last_used_time: current_time - 2000000L,
    usage_count: 50,
    max_uses: 1000,
    error_count: 3,
    max_errors: 10
  }
  
  // 尝试恢复（时间不够）
  let not_recovered = attempt_connection_recovery(error_connection, current_time, fault_config)
  assert_eq(not_recovered.state, Error)
  
  // 尝试恢复（时间足够）
  let recovery_time = current_time + fault_config.retry_delay_ms.to_int64() * 4 * 1000000L
  let recovered = attempt_connection_recovery(error_connection, recovery_time, fault_config)
  assert_eq(recovered.state, Active)
  assert_eq(recovered.error_count, 0)
}

test "connection_pool_performance_optimization" {
  // 测试连接池性能优化
  
  struct PerformanceMetrics {
    total_connections: Int
    active_connections: Int
    idle_connections: Int
    connection_creation_rate: Float
    connection_destruction_rate: Float
    average_wait_time_ms: Float
    pool_utilization: Float
    hit_rate: Float
    miss_rate: Float
  }
  
  struct OptimizationConfig {
    enable_connection_reuse: Bool
    enable_pre warming: Bool
    enable_idle_cleanup: Bool
    max_idle_time_ms: Int
    min_idle_connections: Int
    connection_reuse_threshold: Int
  }
  
  // 计算性能指标
  let calculate_performance_metrics = fn(pool: ConnectionPool) -> PerformanceMetrics {
    let idle_connections = pool.connections.filter(fn(c) { c.state == Idle || c.state == Active }).length()
    let busy_connections = pool.connections.filter(fn(c) { c.state == Busy }).length()
    
    let pool_utilization = if pool.connections.length() > 0 {
      busy_connections.to_float() / pool.connections.length().to_float()
    } else {
      0.0
    }
    
    let hit_rate = if pool.total_created > 0 {
      (pool.total_created - pool.connections.length()).to_float() / pool.total_created.to_float()
    } else {
      0.0
    }
    
    let miss_rate = 1.0 - hit_rate
    
    PerformanceMetrics{
      total_connections: pool.connections.length(),
      active_connections: busy_connections,
      idle_connections: idle_connections,
      connection_creation_rate: pool.total_created.to_float(),  // 简化
      connection_destruction_rate: pool.total_destroyed.to_float(),  // 简化
      average_wait_time_ms: 5.0,  // 简化
      pool_utilization: pool_utilization,
      hit_rate: hit_rate,
      miss_rate: miss_rate
    }
  }
  
  // 连接预热
  let pre_warm_connections = fn(pool: ConnectionPool, target_count: Int, current_time: Int64) -> ConnectionPool {
    let mut warmed_pool = pool
    let current_count = warmed_pool.connections.length()
    
    if current_count < target_count {
      let mut i = 0
      while i < target_count - current_count {
        let new_connection = Connection{
          id: "pre-warm-" + i.to_string(),
          state: Idle,
          created_time: current_time,
          last_used_time: current_time,
          usage_count: 0,
          max_uses: 1000,
          error_count: 0,
          max_errors: 10
        }
        
        let mut new_connections = warmed_pool.connections.to_array()
        new_connections.push(new_connection)
        
        warmed_pool = { warmed_pool |
          connections: new_connections,
          total_created: warmed_pool.total_created + 1
        }
        
        i = i + 1
      }
    }
    
    warmed_pool
  }
  
  // 清理空闲连接
  let cleanup_idle_connections = fn(pool: ConnectionPool, max_idle_time_ms: Int, current_time: Int64) -> ConnectionPool {
    let mut active_connections = Array::empty<Connection>()
    let mut removed_count = 0
    
    let mut i = 0
    while i < pool.connections.length() {
      let conn = pool.connections[i]
      let idle_time = current_time - conn.last_used_time
      
      let should_remove = (conn.state == Idle || conn.state == Active) && 
                         idle_time > max_idle_time_ms.to_int64() * 1000000L &&
                         pool.connections.length() - removed_count > pool.config.min_connections
      
      if should_remove {
        removed_count = removed_count + 1
      } else {
        active_connections.push(conn)
      }
      
      i = i + 1
    }
    
    { pool |
      connections: active_connections,
      total_destroyed: pool.total_destroyed + removed_count
    }
  }
  
  // 创建优化配置
  let optimization_config = OptimizationConfig{
    enable_connection_reuse: true,
    enable_pre_warming: true,
    enable_idle_cleanup: true,
    max_idle_time_ms: 60000,  // 1分钟
    min_idle_connections: 5,
    connection_reuse_threshold: 100
  }
  
  // 创建测试连接池
  let pool_config = ConnectionPoolConfig{
    min_connections: 5,
    max_connections: 20,
    connection_timeout_ms: 5000,
    idle_timeout_ms: 30000,
    max_lifetime_ms: 300000,
    health_check_interval_ms: 10000
  }
  
  let current_time = 1640995200000000L
  let test_pool = ConnectionPool{
    connections: Array::empty(),
    config: pool_config,
    active_connections: 0,
    total_created: 0,
    total_destroyed: 0,
    last_health_check: current_time - 15000L
  }
  
  // 场景1：连接预热
  let pre_warmed_pool = if optimization_config.enable_pre_warming {
    pre_warm_connections(test_pool, 10, current_time)
  } else {
    test_pool
  }
  
  assert_eq(pre_warmed_pool.connections.length(), 10)
  assert_eq(pre_warmed_pool.total_created, 10)
  
  // 场景2：计算性能指标
  let metrics = calculate_performance_metrics(pre_warmed_pool)
  
  assert_eq(metrics.total_connections, 10)
  assert_eq(metrics.active_connections, 0)
  assert_eq(metrics.idle_connections, 10)
  assert_eq(metrics.pool_utilization, 0.0)
  
  // 场景3：模拟使用连接
  let mut used_pool = pre_warmed_pool
  let mut i = 0
  while i < 8 {  // 使用8个连接
    let mut updated_connections = Array::empty<Connection>()
    let mut j = 0
    while j < used_pool.connections.length() {
      let conn = used_pool.connections[j]
      if j < i && conn.state == Idle {
        updated_connections.push({ conn | state: Busy })
      } else {
        updated_connections.push(conn)
      }
      j = j + 1
    }
    
    used_pool = { used_pool |
      connections: updated_connections,
      active_connections = i
    }
    
    i = i + 1
  }
  
  let used_metrics = calculate_performance_metrics(used_pool)
  assert_eq(used_metrics.active_connections, 8)
  assert_eq(used_metrics.idle_connections, 2)
  assert_eq(used_metrics.pool_utilization, 0.8)
  
  // 场景4：清理空闲连接
  let old_time = current_time - 120000000000L  // 2分钟前
  
  let mut old_connections = Array::empty<Connection>()
  let mut k = 0
  while k < 5 {
    old_connections.push(Connection{
      id: "old-conn-" + k.to_string(),
      state: Idle,
      created_time: old_time,
      last_used_time: old_time,
      usage_count: 0,
      max_uses: 1000,
      error_count: 0,
      max_errors: 10
    })
    k = k + 1
  }
  
  let pool_with_old_connections = { used_pool |
    connections: used_pool.connections.concat(old_connections)
  }
  
  let cleaned_pool = if optimization_config.enable_idle_cleanup {
    cleanup_idle_connections(pool_with_old_connections, optimization_config.max_idle_time_ms, current_time)
  } else {
    pool_with_old_connections
  }
  
  // 验证清理效果
  assert_eq(cleaned_pool.connections.length() < pool_with_old_connections.connections.length(), true)
  assert_eq(cleaned_pool.connections.length() >= pool_config.min_connections, true)
  
  let final_metrics = calculate_performance_metrics(cleaned_pool)
  assert_eq(final_metrics.total_connections >= pool_config.min_connections, true)
  
  // 场景5：性能优化验证
  let hit_rate_acceptable = final_metrics.hit_rate >= 0.5  // 至少50%的连接被重用
  let utilization_acceptable = final_metrics.pool_utilization <= 0.9  // 利用率不超过90%
  
  assert_eq(hit_rate_acceptable, true)
  assert_eq(utilization_acceptable, true)
}