// Azimuth 遥测系统容错性测试
// 测试遥测系统的容错性和故障恢复能力

// 测试1: 网络故障容错测试
test "网络故障容错测试" {
  // 创建容错测试管理器
  let fault_tolerance_manager = FaultToleranceManager::new()
  
  // 配置网络故障模拟器
  let network_simulator = NetworkSimulator::new()
  
  // 测试连接超时处理
  NetworkSimulator::configure_timeout(network_simulator, {
    host: "telemetry-collector.example.com",
    port: 4317,
    timeout_ms: 5000,  // 5秒超时
    failure_rate: 0.3  // 30%的请求超时
  })
  
  // 创建遥测客户端
  let telemetry_client = TelemetryClient::new({
    endpoint: "http://telemetry-collector.example.com:4317",
    retry_policy: {
      max_attempts: 3,
      backoff_strategy: "exponential",
      initial_delay_ms: 100,
      max_delay_ms: 5000,
      retry_on: ["timeout", "connection_error", "server_error"]
    },
    circuit_breaker: {
      failure_threshold: 5,
      recovery_timeout_ms: 30000,
      half_open_max_calls: 3
    }
  })
  
  // 创建测试数据
  let test_spans = []
  
  for i in 0..20 {
    let span = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "test.operation",
      service_name: "test-service",
      start_time: 1640995200 + i * 1000,
      end_time: 1640995250 + i * 1000,
      status: "ok",
      attributes: []
    }
    
    test_spans = test_spans.push(span)
  }
  
  // 发送遥测数据
  let send_result = TelemetryClient::send_spans(telemetry_client, test_spans)
  
  // 验证发送结果
  assert_true(send_result.attempted_count == 20)
  assert_true(send_result.successful_count > 0)  // 至少有一些成功
  assert_true(send_result.failed_count < 20)    // 不是全部失败
  
  // 验证重试机制
  assert_true(send_result.retry_attempts > 0)  // 应该有重试尝试
  
  // 验证断路器状态
  let circuit_breaker_state = TelemetryClient::get_circuit_breaker_state(telemetry_client)
  
  // 由于有失败，断路器可能处于打开或半开状态
  assert_true(circuit_breaker_state.state == "closed" or 
              circuit_breaker_state.state == "open" or 
              circuit_breaker_state.state == "half_open")
  
  // 测试网络分区处理
  NetworkSimulator::simulate_partition(network_simulator, {
    duration_ms: 10000,  // 10秒分区
    affected_hosts: ["telemetry-collector.example.com"]
  })
  
  // 在网络分区期间发送数据
  let partition_send_result = TelemetryClient::send_spans(telemetry_client, test_spans.slice(0, 5))
  
  // 验证分区期间的结果
  assert_eq(partition_send_result.attempted_count, 5)
  assert_eq(partition_send_result.successful_count, 0)  // 分区期间应该全部失败
  assert_eq(partition_send_result.failed_count, 5)
  
  // 验证断路器状态
  let partition_circuit_state = TelemetryClient::get_circuit_breaker_state(telemetry_client)
  assert_eq(partition_circuit_state.state, "open")  // 断路器应该打开
  
  // 等待网络分区恢复
  NetworkSimulator::recover_partition(network_simulator)
  
  // 等待断路器恢复
  Time::sleep(31000)  // 等待超过恢复超时时间
  
  // 再次发送数据
  let recovery_send_result = TelemetryClient::send_spans(telemetry_client, test_spans.slice(5, 10))
  
  // 验证恢复后的结果
  assert_eq(recovery_send_result.attempted_count, 5)
  assert_true(recovery_send_result.successful_count > 0)  // 应该有一些成功
  
  // 测试连接池故障处理
  let connection_pool = ConnectionPool::new({
    max_connections: 10,
    idle_timeout_ms: 30000,
    health_check_interval_ms: 5000
  })
  
  // 模拟连接池故障
  ConnectionPool::simulate_failure(connection_pool, {
    failure_type: "connection_exhaustion",
    affected_connections: 8  // 8个连接故障
  })
  
  // 使用连接池发送数据
  let pool_send_result = TelemetryClient::send_spans_with_pool(telemetry_client, test_spans.slice(10, 15), connection_pool)
  
  // 验证连接池故障处理
  assert_eq(pool_send_result.attempted_count, 5)
  assert_true(pool_send_result.successful_count > 0)  // 应该有一些成功，使用剩余连接
  
  // 测试负载均衡故障处理
  let load_balancer = LoadBalancer::new({
    strategy: "round_robin",
    endpoints: [
      "http://collector1.example.com:4317",
      "http://collector2.example.com:4317",
      "http://collector3.example.com:4317"
    ],
    health_check: {
      enabled: true,
      interval_ms: 5000,
      timeout_ms: 1000
    }
  })
  
  // 模拟端点故障
  LoadBalancer::mark_endpoint_unhealthy(load_balancer, "http://collector2.example.com:4317")
  
  // 使用负载均衡器发送数据
  let lb_send_result = TelemetryClient::send_spans_with_lb(telemetry_client, test_spans.slice(15, 20), load_balancer)
  
  // 验证负载均衡故障处理
  assert_eq(lb_send_result.attempted_count, 5)
  assert_true(lb_send_result.successful_count > 0)  // 应该有一些成功，使用健康端点
  
  // 验证请求分布
  assert_false(lb_send_result.endpoint_counts.contains_key("http://collector2.example.com:4317"))  // 故障端点不应有请求
}

// 测试2: 存储故障容错测试
test "存储故障容错测试" {
  // 创建存储容错测试器
  let storage_fault_tolerance = StorageFaultTolerance::new()
  
  // 配置多级存储
  let tiered_storage = TieredStorage::new({
    tiers: [
      {
        name: "hot",
        type: "memory",
        size_limit: 1073741824,  // 1GB
        retention_policy: "1h"
      },
      {
        name: "warm",
        type: "ssd",
        size_limit: 10737418240,  // 10GB
        retention_policy: "24h"
      },
      {
        name: "cold",
        type: "hdd",
        size_limit: 107374182400,  // 100GB
        retention_policy: "30d"
      }
    ],
    promotion_policy: "access_frequency",
    demotion_policy: "lru"
  })
  
  // 创建测试数据
  let test_data = []
  
  for i in 0..1000 {
    let data = {
      id: "data-" + i.to_string(),
      timestamp: 1640995200 + i * 60,
      size: 1024 + (i % 10) * 512,  // 1KB-6KB
      access_frequency: 1000 - i  // 递减访问频率
    }
    
    test_data = test_data.push(data)
  }
  
  // 存储测试数据
  let storage_result = TieredStorage::store_batch(tiered_storage, test_data)
  
  // 验证存储结果
  assert_eq(storage_result.stored_count, 1000)
  assert_eq(storage_result.failed_count, 0)
  
  // 验证数据分布
  let tier_distribution = TieredStorage::get_distribution(tiered_storage)
  assert_eq(tier_distribution.total_items, 1000)
  assert_true(tier_distribution.hot_items > 0)  // 热层应该有数据
  assert_true(tier_distribution.warm_items > 0)  // 温层应该有数据
  assert_true(tier_distribution.cold_items > 0)  // 冷层应该有数据
  
  // 模拟热层故障
  TieredStorage::simulate_tier_failure(tiered_storage, "hot", {
    failure_type: "corruption",
    affected_percentage: 50  // 50%的数据损坏
  })
  
  // 测试热层故障时的读取
  let hot_read_result = TieredStorage::read_batch(tiered_storage, test_data.slice(0, 100).map(fn(d) { d.id }))
  
  // 验证热层故障处理
  assert_eq(hot_read_result.requested_count, 100)
  assert_true(hot_read_result.successful_count > 50)  // 至少50%成功（未损坏的部分）
  assert_true(hot_read_result.fallback_count > 0)     // 应该有从其他层恢复的数据
  
  // 测试热层故障时的写入
  let hot_write_data = test_data.slice(1000, 1100).map(fn(d, i) { 
    { d | id: "new-" + i.to_string() } 
  })
  
  let hot_write_result = TieredStorage::store_batch(tiered_storage, hot_write_data)
  
  // 验证热层故障时的写入
  assert_eq(hot_write_result.stored_count, 100)
  assert_true(hot_write_result.fallback_tier != "hot")  // 应该回退到其他层
  
  // 模拟温层故障
  TieredStorage::simulate_tier_failure(tiered_storage, "warm", {
    failure_type: "unavailable",
    duration_ms: 10000  // 10秒不可用
  })
  
  // 测试温层故障时的数据提升
  let promotion_result = TieredStorage::promote_hot_data(tiered_storage, {
    criteria: "access_frequency",
    threshold: 100,
    count: 50
  })
  
  // 验证温层故障时的提升
  assert_true(promotion_result.successful_promotions > 0)
  assert_true(promotion_result.failed_promotions > 0)  // 一些提升可能失败
  
  // 测试存储复制容错
  let replicated_storage = ReplicatedStorage::new({
    primary: {
      type: "local",
      path: "/data/primary"
    },
    replicas: [
      {
        type: "local",
        path: "/data/replica1"
      },
      {
        type: "remote",
        endpoint: "http://backup-server.example.com:8080"
      }
    ],
    replication_factor: 2,
    consistency_level: "eventual"
  })
  
  // 存储数据到复制存储
  let replication_result = ReplicatedStorage::store_batch(replicated_storage, test_data.slice(0, 100))
  
  // 验证复制结果
  assert_eq(replication_result.stored_count, 100)
  assert_true(replication_result.replication_success_count >= 1)  // 至少一个副本成功
  
  // 模拟主存储故障
  ReplicatedStorage::simulate_failure(replicated_storage, "primary", {
    failure_type: "unavailable"
  })
  
  // 测试主存储故障时的读取
  let replica_read_result = ReplicatedStorage::read_batch(replicated_storage, test_data.slice(0, 50).map(fn(d) { d.id }))
  
  // 验证副本读取
  assert_eq(replica_read_result.requested_count, 50)
  assert_true(replica_read_result.successful_count > 0)  // 应该能从副本读取
  
  // 测试主存储故障时的写入
  let replica_write_data = test_data.slice(100, 150).map(fn(d, i) { 
    { d | id: "replica-" + i.to_string() } 
  })
  
  let replica_write_result = ReplicatedStorage::store_batch(replicated_storage, replica_write_data)
  
  // 验证副本写入
  assert_eq(replica_write_result.stored_count, 50)
  assert_true(replica_write_result.successful_count > 0)  // 应该能写入副本
  
  // 测试存储压缩容错
  let compressed_storage = CompressedStorage::new({
    compression_algorithm: "gzip",
    compression_level: 6,
    verify_after_compress: true
  })
  
  // 存储压缩数据
  let compression_result = CompressedStorage::store_batch(compressed_storage, test_data.slice(0, 100))
  
  // 验证压缩存储
  assert_eq(compression_result.stored_count, 100)
  assert_true(compression_result.compression_ratio < 1.0)  // 应该有压缩效果
  
  // 模拟解压缩故障
  CompressedStorage::simulate_decompression_failure(compressed_storage, {
    failure_rate: 0.2  // 20%的解压缩失败
  })
  
  // 测试解压缩故障时的读取
  let decompress_read_result = CompressedStorage::read_batch(compressed_storage, test_data.slice(0, 50).map(fn(d) { d.id }))
  
  // 验证解压缩故障处理
  assert_eq(decompress_read_result.requested_count, 50)
  assert_true(decompress_read_result.successful_count > 40)  // 至少80%成功
  assert_true(decompress_read_result.failed_count > 0)      // 一些应该失败
}

// 测试3: 数据处理故障容错测试
test "数据处理故障容错测试" {
  // 创建数据处理容错测试器
  let processing_fault_tolerance = ProcessingFaultTolerance::new()
  
  // 配置容错处理管道
  let fault_tolerant_pipeline = FaultTolerantPipeline::new({
    stages: [
      {
        name: "validation",
        retry_policy: {
          max_attempts: 3,
          backoff_strategy: "fixed",
          delay_ms: 100
        },
        error_handling: "skip_and_continue"
      },
      {
        name: "transformation",
        retry_policy: {
          max_attempts: 2,
          backoff_strategy: "exponential",
          initial_delay_ms: 200,
          max_delay_ms: 1000
        },
        error_handling: "fallback_to_original"
      },
      {
        name: "aggregation",
        retry_policy: {
          max_attempts: 1,
          backoff_strategy: "none"
        },
        error_handling: "abort_on_error"
      }
    ],
    dead_letter_queue: {
      enabled: true,
      max_size: 10000,
      retention_hours: 24
    }
  })
  
  // 创建测试数据
  let test_data = []
  
  for i in 0..100 {
    let data = {
      id: "data-" + i.to_string(),
      value: i,
      timestamp: 1640995200 + i * 60,
      valid: i % 10 != 0,  // 每10个中有1个无效
      transformable: i % 15 != 0,  // 每15个中有1个不可转换
      aggregatable: i % 20 != 0   // 每20个中有1个不可聚合
    }
    
    test_data = test_data.push(data)
  }
  
  // 处理测试数据
  let processing_result = FaultTolerantPipeline::process(fault_tolerant_pipeline, test_data)
  
  // 验证处理结果
  assert_eq(processing_result.input_count, 100)
  assert_true(processing_result.successful_count > 0)
  assert_true(processing_result.failed_count > 0)
  assert_true(processing_result.skipped_count > 0)
  
  // 验证死信队列
  let dlq_items = FaultTolerantPipeline::get_dead_letter_items(fault_tolerant_pipeline)
  assert_true(dlq_items.length() > 0)
  
  // 验证错误统计
  let error_stats = FaultTolerantPipeline::get_error_statistics(fault_tolerant_pipeline)
  assert_true(error_stats.stage_errors.length() > 0)
  
  // 测试批处理容错
  let batch_processor = FaultTolerantBatchProcessor::new({
    batch_size: 10,
    max_parallel_batches: 5,
    error_handling: "isolate_failed_batches",
    partial_success_handling: "continue_processing"
  })
  
  // 模拟批处理故障
  BatchProcessor::simulate_failure(batch_processor, {
    failure_rate: 0.2,  // 20%的批处理失败
    affected_batches: "random"
  })
  
  // 批处理测试数据
  let batch_result = BatchProcessor::process_batches(batch_processor, test_data)
  
  // 验证批处理结果
  assert_eq(batch_result.total_batches, 10)  // 100个项目，每批10个
  assert_true(batch_result.successful_batches > 0)
  assert_true(batch_result.failed_batches > 0)
  assert_true(batch_result.processed_items > 0)
  
  // 测试流处理容错
  let stream_processor = FaultTolerantStreamProcessor::new({
    buffer_size: 1000,
    checkpoint_interval_ms: 5000,
    error_handling: "skip_and_log",
    max_consecutive_failures: 5
  })
  
  // 模拟流处理故障
  StreamProcessor::simulate_failure(stream_processor, {
    failure_type: "intermittent",
    failure_rate: 0.1,  // 10%的消息处理失败
    recovery_time_ms: 1000
  })
  
  // 流处理测试数据
  let stream_data = test_data.slice(0, 50)
  let stream_result = StreamProcessor::process_stream(stream_processor, stream_data)
  
  // 验证流处理结果
  assert_eq(stream_result.input_count, 50)
  assert_true(stream_result.processed_count > 40)  // 至少80%成功
  assert_true(stream_result.failed_count > 0)      // 一些应该失败
  assert_true(stream_result.checkpoint_count > 0)   // 应该有检查点
  
  // 测试状态机容错
  let state_machine = FaultTolerantStateMachine::new({
    states: ["initialized", "processing", "completed", "failed"],
    transitions: [
      { from: "initialized", to: "processing" },
      { from: "processing", to: "completed" },
      { from: "processing", to: "failed" },
      { from: "failed", to: "processing" }
    ],
    error_handling: "rollback_on_failure",
    state_persistence: true
  })
  
  // 模拟状态机故障
  StateMachine::simulate_failure(state_machine, {
    failure_point: "processing",
    failure_rate: 0.3,  // 30%的状态转换失败
    recovery_action: "rollback_to_previous_state"
  })
  
  // 处理状态机
  let state_machine_items = test_data.slice(0, 20)
  let state_result = StateMachine::process_items(state_machine, state_machine_items)
  
  // 验证状态机结果
  assert_eq(state_result.input_count, 20)
  assert_true(state_result.completed_count > 0)
  assert_true(state_result.failed_count > 0)
  assert_true(state_result.rollback_count > 0)
  
  // 测试事务处理容错
  let transaction_processor = FaultTolerantTransactionProcessor::new({
    isolation_level: "read_committed",
    retry_policy: {
      max_attempts: 3,
      backoff_strategy: "exponential",
      initial_delay_ms: 100
    },
    deadlock_detection: true,
    timeout_ms: 5000
  })
  
  // 模拟事务冲突
  TransactionProcessor::simulate_conflicts(transaction_processor, {
    conflict_rate: 0.2,  // 20%的事务冲突
    conflict_types: ["deadlock", "write_write_conflict"]
  })
  
  // 处理事务
  let transaction_items = test_data.slice(0, 30)
  let transaction_result = TransactionProcessor::process_transactions(transaction_processor, transaction_items)
  
  // 验证事务结果
  assert_eq(transaction_result.attempted_count, 30)
  assert_true(transaction_result.committed_count > 0)
  assert_true(transaction_result.aborted_count > 0)
  assert_true(transaction_result.retry_count > 0)
  
  // 测试并发处理容错
  let concurrent_processor = FaultTolerantConcurrentProcessor::new({
    max_workers: 10,
    queue_size: 1000,
    worker_timeout_ms: 5000,
    error_handling: "isolate_failed_workers"
  })
  
  // 模拟工作线程故障
  ConcurrentProcessor::simulate_worker_failure(concurrent_processor, {
    failure_rate: 0.1,  // 10%的工作线程故障
    failure_types: ["exception", "timeout", "memory_error"]
  })
  
  // 并发处理
  let concurrent_items = test_data.slice(0, 100)
  let concurrent_result = ConcurrentProcessor::process_concurrent(concurrent_processor, concurrent_items)
  
  // 验证并发处理结果
  assert_eq(concurrent_result.input_count, 100)
  assert_true(concurrent_result.processed_count > 80)  // 至少80%成功
  assert_true(concurrent_result.failed_workers > 0)   // 一些工作线程应该失败
  assert_true(concurrent_result.restarted_workers > 0) // 一些工作线程应该重启
}

// 测试4: 系统级故障容错测试
test "系统级故障容错测试" {
  // 创建系统级容错测试器
  let system_fault_tolerance = SystemFaultTolerance::new()
  
  // 配置健康检查
  let health_checker = HealthChecker::new({
    checks: [
      {
        name: "database",
        type: "connection",
        endpoint: "postgresql://localhost:5432/telemetry",
        interval_ms: 5000,
        timeout_ms: 1000,
        failure_threshold: 3
      },
      {
        name: "message_queue",
        type: "connection",
        endpoint: "redis://localhost:6379",
        interval_ms: 3000,
        timeout_ms: 500,
        failure_threshold: 2
      },
      {
        name: "disk_space",
        type: "resource",
        threshold: 0.8,  // 80%使用率
        interval_ms: 10000,
        failure_threshold: 1
      }
    ]
  })
  
  // 模拟系统故障
  HealthChecker::simulate_failure(health_checker, "database", {
    failure_type: "connection_refused",
    duration_ms: 15000  // 15秒故障
  })
  
  // 检查系统健康状态
  let health_status = HealthChecker::check_health(health_checker)
  
  // 验证健康状态
  assert_false(health_status.overall_healthy)  // 系统应该不健康
  assert_false(health_status.checks.get("database").healthy)  // 数据库检查应该失败
  assert_true(health_status.checks.get("message_queue").healthy)  // 消息队列应该健康
  
  // 测试故障转移
  let failover_manager = FailoverManager::new({
    strategy: "active_passive",
    health_check_interval_ms: 5000,
    failover_timeout_ms: 10000,
    auto_failback: true,
    failback_delay_ms: 30000
  })
  
  // 配置主备节点
  FailoverManager::add_nodes(failover_manager, [
    {
      id: "primary",
      endpoint: "http://primary-telemetry.example.com:8080",
      role: "primary",
      priority: 1
    },
    {
      id: "secondary",
      endpoint: "http://secondary-telemetry.example.com:8080",
      role: "secondary",
      priority: 2
    }
  ])
  
  // 模拟主节点故障
  FailoverManager::simulate_node_failure(failover_manager, "primary", {
    failure_type: "unreachable",
    detection_delay_ms: 2000
  })
  
  // 检查故障转移状态
  let failover_status = FailoverManager::get_status(failover_manager)
  
  // 验证故障转移
  assert_eq(failover_status.active_node, "secondary")  // 应该转移到备节点
  assert_eq(failover_status.failed_nodes.length(), 1)   // 应该有一个失败节点
  assert_eq(failover_status.failed_nodes[0], "primary")
  
  // 测试负载均衡容错
  let load_balancer = FaultTolerantLoadBalancer::new({
    strategy: "weighted_round_robin",
    health_check_interval_ms: 5000,
    unhealthy_node_removal: true,
    recovery_check_interval_ms: 10000
  })
  
  // 配置负载均衡节点
  LoadBalancer::add_nodes(load_balancer, [
    {
      endpoint: "http://node1.example.com:8080",
      weight: 3,
      healthy: true
    },
    {
      endpoint: "http://node2.example.com:8080",
      weight: 2,
      healthy: true
    },
    {
      endpoint: "http://node3.example.com:8080",
      weight: 1,
      healthy: true
    }
  ])
  
  // 模拟节点故障
  LoadBalancer::simulate_node_failure(load_balancer, "http://node2.example.com:8080", {
    failure_type: "timeout",
    duration_ms: 20000
  })
  
  // 测试负载分配
  let request_count = 100
  let distribution = LoadBalancer::distribute_requests(load_balancer, request_count)
  
  // 验证负载分配
  assert_true(distribution.get("http://node1.example.com:8080") > 0)  // 节点1应该处理请求
  assert_eq(distribution.get("http://node2.example.com:8080"), 0)    // 节点2应该不处理请求（故障）
  assert_true(distribution.get("http://node3.example.com:8080") > 0)  // 节点3应该处理请求
  
  // 测试熔断器容错
  let circuit_breaker = FaultTolerantCircuitBreaker::new({
    failure_threshold: 5,
    recovery_timeout_ms: 30000,
    half_open_max_calls: 3,
    success_threshold: 3
  })
  
  // 模拟服务调用失败
  for i in 0..6 {
    let result = CircuitBreaker::call(circuit_breaker, fn() {
      // 模拟服务调用
      if i < 5 {
        Error("Service unavailable")
      } else {
        Ok("Success")
      }
    })
    
    if i < 5 {
      assert_true(result.is_error())  // 前5次调用应该失败
    } else {
      // 第6次调用可能不会执行，因为熔断器应该已经打开
      assert_true(result.is_error() or circuit_breaker.get_state() == "open")
    }
  }
  
  // 验证熔断器状态
  assert_eq(circuit_breaker.get_state(), "open")  // 熔断器应该打开
  
  // 等待恢复超时
  Time::sleep(31000)
  
  // 测试半开状态
  let half_open_result = CircuitBreaker::call(circuit_breaker, fn() {
    Ok("Success")
  })
  
  // 验证半开状态
  assert_true(half_open_result.is_ok())  // 调用应该成功
  assert_eq(circuit_breaker.get_state(), "half_open")  // 状态应该是半开
  
  // 再进行几次成功调用
  for i in 0..3 {
    CircuitBreaker::call(circuit_breaker, fn() {
      Ok("Success")
    })
  }
  
  // 验证熔断器关闭
  assert_eq(circuit_breaker.get_state(), "closed")  // 熔断器应该关闭
  
  // 测试限流容错
  let rate_limiter = FaultTolerantRateLimiter::new({
    strategy: "token_bucket",
    rate: 100,  // 每秒100个请求
    burst: 200,  // 突发200个请求
    window_size_ms: 1000
  })
  
  // 测试限流
  let limiter_results = []
  
  for i in 0..250 {
    let result = RateLimiter::try_acquire(rate_limiter)
    limiter_results = limiter_results.push(result)
  }
  
  // 验证限流结果
  let allowed_count = limiter_results.filter(fn(r) { r }).length()
  let denied_count = limiter_results.filter(fn(r) { not(r) }).length()
  
  assert_true(allowed_count <= 200)  // 最多允许200个请求（突发）
  assert_true(denied_count > 0)      // 一些请求应该被拒绝
  
  // 测试缓存容错
  let cache = FaultTolerantCache::new({
    backend: "redis",
    fallback_backend: "memory",
    ttl_ms: 300000,  // 5分钟
    max_size: 10000,
    eviction_policy: "lru"
  })
  
  // 模拟缓存故障
  Cache::simulate_backend_failure(cache, "redis", {
    failure_type: "connection_error",
    duration_ms: 10000
  })
  
  // 测试缓存操作
  let cache_operations = []
  
  for i in 0..20 {
    let key = "key-" + i.to_string()
    let value = "value-" + i.to_string()
    
    // 写入缓存
    let write_result = Cache::set(cache, key, value)
    
    // 读取缓存
    let read_result = Cache::get(cache, key)
    
    cache_operations = cache_operations.push({
      key: key,
      write_success: write_result,
      read_success: read_result.is_some(),
      read_value: read_result
    })
  }
  
  // 验证缓存容错
  let successful_writes = cache_operations.filter(fn(op) { op.write_success }).length()
  let successful_reads = cache_operations.filter(fn(op) { op.read_success }).length()
  
  assert_true(successful_writes > 0)  // 一些写入应该成功
  assert_true(successful_reads > 0)  // 一些读取应该成功
  
  // 验证回退到内存缓存
  let memory_cache_hits = cache_operations.filter(fn(op) { 
    op.read_success and op.read_value == Some("value-" + op.key.split("-")[1])
  }).length()
  
  assert_true(memory_cache_hits > 0)  // 应该有内存缓存命中
}

// 测试5: 故障恢复和自愈测试
test "故障恢复和自愈测试" {
  // 创建自愈管理器
  let self_healing_manager = SelfHealingManager::new()
  
  // 配置自愈策略
  SelfHealingManager::add_healing_strategy(self_healing_manager, {
    name: "service_restart",
    condition: "service_unhealthy",
    actions: [
      {
        type: "check_logs",
        parameters: { lines: 100 }
      },
      {
        type: "restart_service",
        parameters: { graceful: true, timeout_ms: 30000 }
      },
      {
        type: "verify_health",
        parameters: { max_attempts: 3, interval_ms: 5000 }
      }
    ],
    max_attempts_per_hour: 3,
    cooldown_period_ms: 300000  // 5分钟冷却期
  })
  
  SelfHealingManager::add_healing_strategy(self_healing_manager, {
    name: "connection_pool_reset",
    condition: "connection_pool_exhausted",
    actions: [
      {
        type: "drain_connections",
        parameters: { timeout_ms: 10000 }
      },
      {
        type: "rebuild_pool",
        parameters: { initial_size: 5, max_size: 20 }
      }
    ],
    max_attempts_per_hour: 5,
    cooldown_period_ms: 60000  // 1分钟冷却期
  })
  
  SelfHealingManager::add_healing_strategy(self_healing_manager, {
    name: "memory_cleanup",
    condition: "memory_pressure_high",
    actions: [
      {
        type: "garbage_collect",
        parameters: { aggressive: true }
      },
      {
        type: "clear_caches",
        parameters: { older_than_ms: 300000 }
      },
      {
        type: "reduce_batch_sizes",
        parameters: { reduction_factor: 0.5 }
      }
    ],
    max_attempts_per_hour: 10,
    cooldown_period_ms: 30000  // 30秒冷却期
  })
  
  // 模拟服务故障
  let service_monitor = ServiceMonitor::new({
    services: [
      {
        name: "telemetry-collector",
        endpoint: "http://localhost:8080/health",
        health_check_interval_ms: 5000,
        failure_threshold: 3
      },
      {
        name: "telemetry-processor",
        endpoint: "http://localhost:8081/health",
        health_check_interval_ms: 5000,
        failure_threshold: 3
      }
    ]
  })
  
  // 模拟telemetry-collector服务故障
  ServiceMonitor::simulate_service_failure(service_monitor, "telemetry-collector", {
    failure_type: "process_crash",
    auto_recovery: false  // 禁用自动恢复，测试自愈
  })
  
  // 检查服务状态
  let service_status = ServiceMonitor::check_services(service_monitor)
  
  // 验证服务状态
  assert_false(service_status.services.get("telemetry-collector").healthy)
  assert_true(service_status.services.get("telemetry-processor").healthy)
  
  // 触发自愈
  let healing_result = SelfHealingManager::trigger_healing(self_healing_manager, {
    service: "telemetry-collector",
    condition: "service_unhealthy"
  })
  
  // 验证自愈结果
  assert_true(healing_result.triggered)
  assert_eq(healing_result.strategy, "service_restart")
  assert_true(healing_result.actions.length() > 0)
  
  // 检查自愈操作
  let healing_actions = healing_result.actions
  assert_true(healing_actions.any(fn(a) { a.type == "check_logs" }))
  assert_true(healing_actions.any(fn(a) { a.type == "restart_service" }))
  assert_true(healing_actions.any(fn(a) { a.type == "verify_health" }))
  
  // 模拟连接池故障
  let connection_pool = ConnectionPool::new({
    max_connections: 10,
    idle_timeout_ms: 30000
  })
  
  // 模拟连接池耗尽
  ConnectionPool::simulate_exhaustion(connection_pool, {
    exhausted_connections: 10,
    new_connection_failures: true
  })
  
  // 检查连接池状态
  let pool_status = ConnectionPool::get_status(connection_pool)
  assert_true(pool_status.exhausted)
  
  // 触发连接池自愈
  let pool_healing_result = SelfHealingManager::trigger_healing(self_healing_manager, {
    component: "connection_pool",
    condition: "connection_pool_exhausted"
  })
  
  // 验证连接池自愈结果
  assert_true(pool_healing_result.triggered)
  assert_eq(pool_healing_result.strategy, "connection_pool_reset")
  
  // 检查连接池恢复状态
  let recovered_pool_status = ConnectionPool::get_status(connection_pool)
  assert_false(recovered_pool_status.exhausted)
  assert_true(recovered_pool_status.active_connections > 0)
  
  // 模拟内存压力
  let memory_monitor = MemoryMonitor::new({
    thresholds: {
      warning: 0.7,   // 70%
      critical: 0.85, // 85%
      emergency: 0.95 // 95%
    },
    check_interval_ms: 5000
  })
  
  // 模拟高内存使用
  MemoryMonitor::simulate_memory_pressure(memory_monitor, {
    usage_percentage: 0.9,  // 90%内存使用
    sustained_duration_ms: 10000  // 持续10秒
  })
  
  // 检查内存状态
  let memory_status = MemoryMonitor::get_status(memory_monitor)
  assert_true(memory_status.pressure_level == "critical" or memory_status.pressure_level == "emergency")
  
  // 触发内存自愈
  let memory_healing_result = SelfHealingManager::trigger_healing(self_healing_manager, {
    component: "system",
    condition: "memory_pressure_high"
  })
  
  // 验证内存自愈结果
  assert_true(memory_healing_result.triggered)
  assert_eq(memory_healing_result.strategy, "memory_cleanup")
  
  // 检查内存恢复状态
  let recovered_memory_status = MemoryMonitor::get_status(memory_monitor)
  assert_true(recovered_memory_status.pressure_level == "normal" or 
              recovered_memory_status.pressure_level == "warning")
  
  // 测试自愈限制
  let healing_stats = SelfHealingManager::get_healing_statistics(self_healing_manager)
  
  // 验证自愈统计
  assert_true(healing_stats.total_healings > 0)
  assert_true(healing_stats.successful_healings > 0)
  assert_true(healing_stats.strategies_applied.contains("service_restart"))
  assert_true(healing_stats.strategies_applied.contains("connection_pool_reset"))
  assert_true(healing_stats.strategies_applied.contains("memory_cleanup"))
  
  // 测试自愈冷却期
  let immediate_healing = SelfHealingManager::trigger_healing(self_healing_manager, {
    service: "telemetry-collector",
    condition: "service_unhealthy"
  })
  
  // 验证冷却期
  assert_false(immediate_healing.triggered)  // 由于冷却期，不应该触发
  
  // 测试自愈历史
  let healing_history = SelfHealingManager::get_healing_history(self_healing_manager, {
    limit: 10,
    service_filter: "telemetry-collector"
  })
  
  // 验证自愈历史
  assert_true(healing_history.length() > 0)
  
  let collector_healings = healing_history.filter(fn(h) { h.service == "telemetry-collector" })
  assert_true(collector_healings.length() > 0)
  
  // 验证自愈历史详情
  let last_healing = collector_healings[0]
  assert_eq(last_healing.strategy, "service_restart")
  assert_true(last_healing.timestamp > 0)
  assert_true(last_healing.actions.length() > 0)
  
  // 测试自愈报告
  let healing_report = SelfHealingManager::generate_healing_report(self_healing_manager, {
    period_hours: 24,
    include_recommendations: true
  })
  
  // 验证自愈报告
  assert_true(healing_report.period_hours == 24)
  assert_true(healing_report.total_healings > 0)
  assert_true(healing_report.success_rate > 0)
  assert_true(healing_report.recommendations.length() > 0)
}