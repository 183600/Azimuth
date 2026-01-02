// Azimuth 遥测分布式事务测试用例
// 专注于遥测系统中的分布式事务处理和一致性保证

// 测试1: 两阶段提交协议
test "两阶段提交协议测试" {
  let transaction_manager = DistributedTransactionManager::new("分布式事务管理器")
  
  // 创建参与者
  let participant1 = TelemetryParticipant::new("metrics.store", "localhost:8001")
  let participant2 = TelemetryParticipant::new("traces.store", "localhost:8002")
  let participant3 = TelemetryParticipant::new("logs.store", "localhost:8003")
  
  // 配置参与者
  TelemetryParticipant::set_timeout(participant1, 5000)  // 5秒超时
  TelemetryParticipant::set_timeout(participant2, 5000)
  TelemetryParticipant::set_timeout(participant3, 5000)
  
  // 添加参与者到事务管理器
  DistributedTransactionManager::add_participant(transaction_manager, participant1)
  DistributedTransactionManager::add_participant(transaction_manager, participant2)
  DistributedTransactionManager::add_participant(transaction_manager, participant3)
  
  // 创建分布式事务
  let transaction = DistributedTransactionManager::begin_transaction(transaction_manager)
  let transaction_id = DistributedTransaction::get_id(transaction)
  
  // 准备阶段操作
  let metric_operation = MetricOperation::new("CREATE", "cpu.usage", 75.5)
  let trace_operation = TraceOperation::new("CREATE", "trace-123", "api.request")
  let log_operation = LogOperation::new("CREATE", "error", "Database connection failed")
  
  // 添加操作到事务
  DistributedTransaction::add_operation(transaction, participant1, metric_operation)
  DistributedTransaction::add_operation(transaction, participant2, trace_operation)
  DistributedTransaction::add_operation(transaction, participant3, log_operation)
  
  // 执行两阶段提交
  let prepare_result = DistributedTransactionManager::prepare(transaction_manager, transaction)
  
  // 验证准备阶段
  assert_eq(prepare_result.status, Prepared)
  assert_eq(prepare_result.prepared_participants, 3)
  assert_eq(prepare_result.aborted_participants, 0)
  
  // 提交阶段
  let commit_result = DistributedTransactionManager::commit(transaction_manager, transaction)
  
  // 验证提交阶段
  assert_eq(commit_result.status, Committed)
  assert_eq(commit_result.committed_participants, 3)
  assert_eq(commit_result.failed_participants, 0)
  
  // 验证事务状态
  let final_status = DistributedTransactionManager::get_transaction_status(transaction_manager, transaction_id)
  assert_eq(final_status, Committed)
  
  // 验证参与者状态
  let participant1_status = TelemetryParticipant::get_transaction_status(participant1, transaction_id)
  let participant2_status = TelemetryParticipant::get_transaction_status(participant2, transaction_id)
  let participant3_status = TelemetryParticipant::get_transaction_status(participant3, transaction_id)
  
  assert_eq(participant1_status, Committed)
  assert_eq(participant2_status, Committed)
  assert_eq(participant3_status, Committed)
}

// 测试2: 三阶段提交协议
test "三阶段提交协议测试" {
  let transaction_manager = DistributedTransactionManager::new("三阶段事务管理器")
  
  // 配置三阶段提交
  DistributedTransactionManager::set_protocol(transaction_manager, ThreePhaseCommit)
  
  // 创建参与者
  let participants = [
    TelemetryParticipant::new("cache.service", "localhost:9001"),
    TelemetryParticipant::new("database.service", "localhost:9002"),
    TelemetryParticipant::new("message.queue", "localhost:9003"),
    TelemetryParticipant::new("search.index", "localhost:9004")
  ]
  
  // 添加参与者
  for participant in participants {
    DistributedTransactionManager::add_participant(transaction_manager, participant)
  }
  
  // 创建分布式事务
  let transaction = DistributedTransactionManager::begin_transaction(transaction_manager)
  
  // 添加操作
  let operations = [
    CacheOperation::new("SET", "user:123", "profile_data"),
    DatabaseOperation::new("INSERT", "users", "{'id': 123, 'name': 'John'}"),
    MessageOperation::new("PUBLISH", "user.events", "user.created"),
    SearchOperation::new("INDEX", "users", "{'id': 123, 'name': 'John'}")
  ]
  
  for i in 0..=3 {
    DistributedTransaction::add_operation(transaction, participants[i], operations[i])
  }
  
  // 执行三阶段提交
  // 阶段1: CanCommit
  let can_commit_result = DistributedTransactionManager::can_commit(transaction_manager, transaction)
  assert_eq(can_commit_result.status, CanCommit)
  assert_eq(can_commit_result.voting_participants, 4)
  
  // 阶段2: PreCommit
  let pre_commit_result = DistributedTransactionManager::pre_commit(transaction_manager, transaction)
  assert_eq(pre_commit_result.status, PreCommit)
  assert_eq(pre_commit_result.pre_committed_participants, 4)
  
  // 阶段3: DoCommit
  let do_commit_result = DistributedTransactionManager::do_commit(transaction_manager, transaction)
  assert_eq(do_commit_result.status, Committed)
  assert_eq(do_commit_result.committed_participants, 4)
  
  // 验证最终状态
  let transaction_id = DistributedTransaction::get_id(transaction)
  let final_status = DistributedTransactionManager::get_transaction_status(transaction_manager, transaction_id)
  assert_eq(final_status, Committed)
  
  // 验证三阶段提交统计
  let protocol_stats = DistributedTransactionManager::get_protocol_stats(transaction_manager)
  assert_eq(protocol_stats.three_phase_transactions, 1)
  assert_eq(protocol_stats.can_commit_rounds, 1)
  assert_eq(protocol_stats.pre_commit_rounds, 1)
  assert_eq(protocol_stats.do_commit_rounds, 1)
}

// 测试3: 分布式事务补偿机制
test "分布式事务补偿机制测试" {
  let transaction_manager = DistributedTransactionManager::new("补偿事务管理器")
  
  // 创建参与者
  let payment_service = TelemetryParticipant::new("payment.service", "localhost:7001")
  let inventory_service = TelemetryParticipant::new("inventory.service", "localhost:7002")
  let shipping_service = TelemetryParticipant::new("shipping.service", "localhost:7003")
  
  // 配置补偿操作
  TelemetryParticipant::set_compensation_operation(payment_service, 
    CompensationOperation::new("REFUND", "payment.refund"))
  TelemetryParticipant::set_compensation_operation(inventory_service, 
    CompensationOperation::new("RESTORE", "inventory.restore"))
  TelemetryParticipant::set_compensation_operation(shipping_service, 
    CompensationOperation::new("CANCEL", "shipping.cancel"))
  
  // 添加参与者
  DistributedTransactionManager::add_participant(transaction_manager, payment_service)
  DistributedTransactionManager::add_participant(transaction_manager, inventory_service)
  DistributedTransactionManager::add_participant(transaction_manager, shipping_service)
  
  // 启用补偿机制
  DistributedTransactionManager::enable_compensation(transaction_manager)
  
  // 创建Saga事务
  let saga = SagaTransaction::new("order.processing")
  
  // 添加Saga步骤
  SagaTransaction::add_step(saga, payment_service, 
    TransactionOperation::new("CHARGE", "payment.charge", 100.0),
    CompensationOperation::new("REFUND", "payment.refund", 100.0))
  
  SagaTransaction::add_step(saga, inventory_service,
    TransactionOperation::new("RESERVE", "inventory.reserve", "item-123", 2),
    CompensationOperation::new("RESTORE", "inventory.restore", "item-123", 2))
  
  SagaTransaction::add_step(saga, shipping_service,
    TransactionOperation::new("SCHEDULE", "shipping.schedule", "order-456"),
    CompensationOperation::new("CANCEL", "shipping.cancel", "order-456"))
  
  // 执行Saga事务（模拟失败场景）
  let saga_result = DistributedTransactionManager::execute_saga(transaction_manager, saga)
  
  // 模拟第三步失败
  let failure_simulation = FailureSimulation::new(shipping_service, "shipping.schedule")
  DistributedTransactionManager::simulate_failure(transaction_manager, failure_simulation)
  
  // 验证补偿执行
  assert_eq(saga_result.status, Compensated)
  assert_eq(saga_result.completed_steps, 2)  // 前两步完成
  assert_eq(saga_result.failed_step, 3)    // 第三步失败
  assert_eq(saga_result.compensated_steps, 2)  // 前两步被补偿
  
  // 验证补偿状态
  let payment_compensated = TelemetryParticipant::is_compensated(payment_service, saga.transaction_id)
  let inventory_compensated = TelemetryParticipant::is_compensated(inventory_service, saga.transaction_id)
  
  assert_true(payment_compensated)
  assert_true(inventory_compensated)
  
  // 验证补偿统计
  let compensation_stats = DistributedTransactionManager::get_compensation_stats(transaction_manager)
  assert_eq(compensation_stats.total_sagas, 1)
  assert_eq(compensation_stats.failed_sagas, 1)
  assert_eq(compensation_stats.compensated_operations, 2)
}

// 测试4: 分布式事务超时和恢复
test "分布式事务超时和恢复测试" {
  let transaction_manager = DistributedTransactionManager::new("超时恢复事务管理器")
  
  // 创建参与者（模拟慢响应）
  let slow_participant1 = TelemetryParticipant::new("slow.service1", "localhost:6001")
  let slow_participant2 = TelemetryParticipant::new("slow.service2", "localhost:6002")
  let normal_participant = TelemetryParticipant::new("normal.service", "localhost:6003")
  
  // 配置超时
  TelemetryParticipant::set_timeout(slow_participant1, 2000)  // 2秒超时
  TelemetryParticipant::set_timeout(slow_participant2, 2000)
  TelemetryParticipant::set_timeout(normal_participant, 5000)  // 5秒超时
  
  // 模拟慢响应
  TelemetryParticipant::simulate_slow_response(slow_participant1, 3000)  // 3秒响应
  TelemetryParticipant::simulate_slow_response(slow_participant2, 3000)
  
  // 添加参与者
  DistributedTransactionManager::add_participant(transaction_manager, slow_participant1)
  DistributedTransactionManager::add_participant(transaction_manager, slow_participant2)
  DistributedTransactionManager::add_participant(transaction_manager, normal_participant)
  
  // 配置恢复策略
  let recovery_strategy = TimeoutRecoveryStrategy::new()
  TimeoutRecoveryStrategy::set_max_retries(recovery_strategy, 3)
  TimeoutRecoveryStrategy::set_retry_interval(recovery_strategy, 1000)
  
  DistributedTransactionManager::set_recovery_strategy(transaction_manager, recovery_strategy)
  
  // 创建事务
  let transaction = DistributedTransactionManager::begin_transaction(transaction_manager)
  
  // 添加操作
  DistributedTransaction::add_operation(transaction, slow_participant1, 
    MetricOperation::new("CREATE", "slow.metric1", 100.0))
  DistributedTransaction::add_operation(transaction, slow_participant2,
    MetricOperation::new("CREATE", "slow.metric2", 200.0))
  DistributedTransaction::add_operation(transaction, normal_participant,
    MetricOperation::new("CREATE", "normal.metric", 300.0))
  
  // 执行事务（会遇到超时）
  let transaction_result = DistributedTransactionManager::execute_transaction(transaction_manager, transaction)
  
  // 验证超时处理
  assert_eq(transaction_result.status, Timeout)
  assert_eq(transaction_result.timed_out_participants, 2)
  assert_eq(transaction_result.successful_participants, 1)
  
  // 等待恢复
  Time::sleep(5000)
  
  // 检查恢复状态
  let recovery_stats = DistributedTransactionManager::get_recovery_stats(transaction_manager)
  assert_true(recovery_stats.recovery_attempts > 0)
  assert_true(recovery_stats.recovered_transactions >= 0)
  
  // 验证事务日志
  let transaction_logs = DistributedTransactionManager::get_transaction_logs(transaction_manager)
  assert_true(transaction_logs.length() > 0)
  
  // 验证超时统计
  let timeout_stats = DistributedTransactionManager::get_timeout_stats(transaction_manager)
  assert_eq(timeout_stats.total_timeouts, 2)
  assert_true(timeout_stats.avg_timeout_ms > 2000)
}

// 测试5: 分布式事务并发控制
test "分布式事务并发控制测试" {
  let transaction_manager = DistributedTransactionManager::new("并发控制事务管理器")
  
  // 创建共享资源参与者
  let shared_resource = TelemetryParticipant::new("shared.resource", "localhost:5001")
  
  // 配置并发控制
  let concurrency_controller = ConcurrencyController::new()
  ConcurrencyController::set_lock_timeout(concurrency_controller, 5000)  // 5秒锁超时
  ConcurrencyController::set_deadlock_detection(concurrency_controller, true)
  ConcurrencyController::set_isolation_level(concurrency_controller, ReadCommitted)
  
  TelemetryParticipant::set_concurrency_controller(shared_resource, concurrency_controller)
  DistributedTransactionManager::add_participant(transaction_manager, shared_resource)
  
  // 创建并发事务
  let transaction1 = DistributedTransactionManager::begin_transaction(transaction_manager)
  let transaction2 = DistributedTransactionManager::begin_transaction(transaction_manager)
  let transaction3 = DistributedTransactionManager::begin_transaction(transaction_manager)
  
  // 添加冲突操作（访问同一资源）
  let resource_key = "shared.counter"
  
  DistributedTransaction::add_operation(transaction1, shared_resource,
    MetricOperation::new("INCREMENT", resource_key, 1.0))
  
  DistributedTransaction::add_operation(transaction2, shared_resource,
    MetricOperation::new("INCREMENT", resource_key, 2.0))
  
  DistributedTransaction::add_operation(transaction3, shared_resource,
    MetricOperation::new("INCREMENT", resource_key, 3.0))
  
  // 并发执行事务
  let executor = ConcurrentTransactionExecutor::new(3)  // 3个并发线程
  let results = executor.execute_transactions([transaction1, transaction2, transaction3])
  
  // 验证并发控制结果
  assert_eq(results.length(), 3)
  
  // 应该有一个事务成功，其他等待或失败
  let successful_transactions = results.filter(|r| r.status == Committed)
  let waiting_transactions = results.filter(|r| r.status == Waiting)
  let aborted_transactions = results.filter(|r| r.status == Aborted)
  
  assert_eq(successful_transactions.length(), 1)
  assert_true(waiting_transactions.length() + aborted_transactions.length() == 2)
  
  // 验证资源状态一致性
  let final_value = TelemetryParticipant::get_metric_value(shared_resource, resource_key)
  assert_true(final_value.is_some())
  
  // 验证锁状态
  let lock_stats = ConcurrencyController::get_lock_stats(concurrency_controller)
  assert_true(lock_stats.total_locks_acquired > 0)
  assert_true(lock_stats.lock_contentions > 0)
  
  // 验证死锁检测
  let deadlock_stats = ConcurrencyController::get_deadlock_stats(concurrency_controller)
  assert_true(deadlock_stats.deadlock_detections >= 0)
}

// 测试6: 分布式事务监控和指标
test "分布式事务监控和指标测试" {
  let transaction_manager = DistributedTransactionManager::new("监控事务管理器")
  let transaction_monitor = TransactionMonitor::new()
  
  // 配置监控指标
  TransactionMonitor::enable_throughput_monitoring(transaction_monitor)
  TransactionMonitor::enable_latency_monitoring(transaction_monitor)
  TransactionMonitor::enable_success_rate_monitoring(transaction_monitor)
  TransactionMonitor::enable_resource_usage_monitoring(transaction_monitor)
  
  // 创建参与者
  let participants = [
    TelemetryParticipant::new("metrics.service", "localhost:4001"),
    TelemetryParticipant::new("traces.service", "localhost:4002"),
    TelemetryParticipant::new("logs.service", "localhost:4003"),
    TelemetryParticipant::new("alerts.service", "localhost:4004")
  ]
  
  for participant in participants {
    DistributedTransactionManager::add_participant(transaction_manager, participant)
  }
  
  // 启动监控
  TransactionMonitor::start_monitoring(transaction_monitor, transaction_manager)
  
  // 执行大量事务
  for i in 0..=49 {
    let transaction = DistributedTransactionManager::begin_transaction(transaction_manager)
    
    // 添加操作
    for j in 0..=3 {
      let operation = MetricOperation::new("CREATE", "metric." + j.to_string(), i.to_float())
      DistributedTransaction::add_operation(transaction, participants[j], operation)
    }
    
    // 模拟一些失败
    if i % 10 == 9 {
      TelemetryParticipant::simulate_failure(participants[0], "CREATE")
    }
    
    DistributedTransactionManager::execute_transaction(transaction_manager, transaction)
  }
  
  // 等待所有事务完成
  Time::sleep(5000)
  
  // 获取监控指标
  let monitoring_metrics = TransactionMonitor::get_metrics(transaction_monitor)
  
  // 验证吞吐量指标
  assert_true(monitoring_metrics.transactions_per_second > 0)
  assert_true(monitoring_metrics.peak_throughput > 0)
  assert_eq(monitoring_metrics.total_transactions, 50)
  
  // 验证延迟指标
  assert_true(monitoring_metrics.avg_transaction_latency_ms > 0)
  assert_true(monitoring_metrics.p95_transaction_latency_ms > 0)
  assert_true(monitoring_metrics.p99_transaction_latency_ms > 0)
  
  // 验证成功率指标
  assert_true(monitoring_metrics.success_rate >= 0.0 && monitoring_metrics.success_rate <= 1.0)
  assert_true(monitoring_metrics.successful_transactions >= 40)  // 至少40个成功（5个失败）
  assert_eq(monitoring_metrics.failed_transactions, 5)  // 5个失败
  
  // 验证资源使用指标
  assert_true(monitoring_metrics.memory_usage_mb > 0)
  assert_true(monitoring_metrics.cpu_usage_percent >= 0.0)
  assert_true(monitoring_metrics.network_io_mb > 0)
  
  // 验证参与者指标
  let participant_metrics = TransactionMonitor::get_participant_metrics(transaction_monitor)
  assert_eq(participant_metrics.length(), 4)
  
  for metrics in participant_metrics {
    assert_true(metrics.total_operations > 0)
    assert_true(metrics.avg_operation_latency_ms > 0)
  }
  
  // 验证监控历史
  let monitoring_history = TransactionMonitor::get_history(transaction_monitor, 3600)  // 最近1小时
  assert_true(monitoring_history.length() > 0)
}

// 测试7: 分布式事务跨数据中心复制
test "分布式事务跨数据中心复制测试" {
  // 创建多数据中心事务管理器
  let multi_dc_manager = MultiDataCenterTransactionManager::new("多数据中心事务管理器")
  
  // 配置数据中心
  let dc1 = DataCenter::new("dc1", "us-east-1", ["dc1-node1:8001", "dc1-node2:8002"])
  let dc2 = DataCenter::new("dc2", "us-west-1", ["dc2-node1:8001", "dc2-node2:8002"])
  let dc3 = DataCenter::new("dc3", "eu-west-1", ["dc3-node1:8001", "dc3-node2:8002"])
  
  // 配置数据中心拓扑
  MultiDataCenterTransactionManager::add_data_center(multi_dc_manager, dc1)
  MultiDataCenterTransactionManager::add_data_center(multi_dc_manager, dc2)
  MultiDataCenterTransactionManager::add_data_center(multi_dc_manager, dc3)
  
  // 配置复制策略
  let replication_strategy = MultiDCReplicationStrategy::new()
  MultiDCReplicationStrategy::set_replication_factor(replication_strategy, 2)  // 2副本
  MultiDCReplicationStrategy::set_consistency_level(replication_strategy, EventualConsistency)
  MultiDCReplicationStrategy::set_preferred_data_centers(replication_strategy, ["dc1", "dc2"])
  
  MultiDataCenterTransactionManager::set_replication_strategy(multi_dc_manager, replication_strategy)
  
  // 创建跨数据中心事务
  let transaction = MultiDataCenterTransactionManager::begin_transaction(multi_dc_manager)
  
  // 添加跨数据中心操作
  let dc1_operation = MetricOperation::new("CREATE", "dc1.metric", 100.0)
  let dc2_operation = TraceOperation::new("CREATE", "dc2.trace", "cross.dc.operation")
  let dc3_operation = LogOperation::new("CREATE", "info", "Cross data center log")
  
  MultiDataCenterTransaction::add_operation(transaction, "dc1", dc1_operation)
  MultiDataCenterTransaction::add_operation(transaction, "dc2", dc2_operation)
  MultiDataCenterTransaction::add_operation(transaction, "dc3", dc3_operation)
  
  // 执行跨数据中心事务
  let transaction_result = MultiDataCenterTransactionManager::execute_transaction(multi_dc_manager, transaction)
  
  // 验证事务结果
  assert_eq(transaction_result.status, Committed)
  assert_eq(transaction_result.involved_data_centers, 3)
  assert_eq(transaction_result.replicated_data_centers, 2)  // 2副本
  
  // 验证复制状态
  let replication_status = MultiDataCenterTransactionManager::get_replication_status(multi_dc_manager, transaction.transaction_id)
  assert_eq(replication_status.total_replicas, 6)  // 3个数据中心 × 2副本
  assert_true(replication_status.successful_replicas >= 4)  // 至少4个成功复制
  
  // 模拟数据中心故障
  MultiDataCenterTransactionManager::simulate_data_center_failure(multi_dc_manager, "dc2")
  
  // 继续执行事务
  let recovery_transaction = MultiDataCenterTransactionManager::begin_transaction(multi_dc_manager)
  let recovery_operation = MetricOperation::new("CREATE", "recovery.metric", 200.0)
  MultiDataCenterTransaction::add_operation(recovery_transaction, "dc1", recovery_operation)
  
  let recovery_result = MultiDataCenterTransactionManager::execute_transaction(multi_dc_manager, recovery_transaction)
  
  // 验证故障恢复
  assert_eq(recovery_result.status, Committed)
  assert_eq(recovery_result.involved_data_centers, 2)  // 只有dc1和dc3可用
  
  // 验证多数据中心统计
  let multi_dc_stats = MultiDataCenterTransactionManager::get_stats(multi_dc_manager)
  assert_eq(multi_dc_stats.total_cross_dc_transactions, 2)
  assert_true(multi_dc_stats.avg_replication_latency_ms > 0)
  assert_true(multi_dc_stats.data_center_failures > 0)
}

// 测试8: 分布式事务性能优化
test "分布式事务性能优化测试" {
  let transaction_manager = DistributedTransactionManager::new("性能优化事务管理器")
  let performance_optimizer = TransactionPerformanceOptimizer::new()
  
  // 配置性能优化策略
  let batch_processing = BatchTransactionStrategy::new(10)  // 批量处理10个事务
  let parallel_execution = ParallelExecutionStrategy::new(4)  // 4个并行线程
  let connection_pooling = ConnectionPoolingStrategy::new(20)  // 20个连接池
  let caching = TransactionCachingStrategy::new(1000)  // 缓存1000个事务状态
  
  TransactionPerformanceOptimizer::add_strategy(performance_optimizer, batch_processing)
  TransactionPerformanceOptimizer::add_strategy(performance_optimizer, parallel_execution)
  TransactionPerformanceOptimizer::add_strategy(performance_optimizer, connection_pooling)
  TransactionPerformanceOptimizer::add_strategy(performance_optimizer, caching)
  
  // 应用性能优化
  DistributedTransactionManager::set_performance_optimizer(transaction_manager, performance_optimizer)
  
  // 创建参与者
  let participants = []
  for i in 0..=7 {
    let participant = TelemetryParticipant::new("service." + i.to_string(), "localhost:300" + i.to_string())
    DistributedTransactionManager::add_participant(transaction_manager, participant)
    participants = participants.push(participant)
  }
  
  // 性能基准测试
  let start_time = Time::now()
  
  // 执行大量事务
  for i in 0..=99 {
    let transaction = DistributedTransactionManager::begin_transaction(transaction_manager)
    
    // 每个事务涉及4个随机参与者
    let selected_participants = participants.shuffle().take(4)
    for j in 0..=3 {
      let operation = MetricOperation::new("CREATE", "metric." + j.to_string(), i.to_float())
      DistributedTransaction::add_operation(transaction, selected_participants[j], operation)
    }
    
    DistributedTransactionManager::execute_transaction(transaction_manager, transaction)
  }
  
  let end_time = Time::now()
  let total_time = end_time - start_time
  
  // 等待所有事务完成
  Time::sleep(10000)
  
  // 验证性能指标
  let performance_stats = TransactionPerformanceOptimizer::get_stats(performance_optimizer)
  
  // 验证批处理效果
  assert_true(performance_stats.batch_transactions > 0)
  assert_true(performance_stats.avg_batch_size >= 5.0)
  
  // 验证并行执行效果
  assert_true(performance_stats.parallel_efficiency > 0.5)
  assert_true(performance_stats.max_concurrent_transactions > 0)
  
  // 验证连接池效果
  assert_true(performance_stats.connection_pool_hit_rate > 0.8)
  assert_true(performance_stats.avg_connection_wait_time_ms < 10)
  
  // 验证缓存效果
  assert_true(performance_stats.cache_hit_rate > 0.0)
  assert_true(performance_stats.avg_cache_lookup_time_ms < 1)
  
  // 验证总体性能
  assert_true(total_time < 30000)  // 应该在30秒内完成
  assert_true(performance_stats.transactions_per_second > 3)
  
  // 验证资源使用
  assert_true(performance_stats.memory_usage_mb < 100)
  assert_true(performance_stats.cpu_usage_percent < 80)
}

// 测试9: 分布式事务安全性和加密
test "分布式事务安全性和加密测试" {
  let transaction_manager = DistributedTransactionManager::new("安全事务管理器")
  let security_manager = TransactionSecurityManager::new()
  
  // 配置安全策略
  let encryption_strategy = TransactionEncryptionStrategy::new("aes-256-gcm", "secure-key-123")
  let authentication_strategy = MutualTLSAuthenticationStrategy::new()
  let authorization_strategy = RoleBasedAuthorizationStrategy::new()
  
  // 配置基于角色的授权
  RoleBasedAuthorizationStrategy::add_role(authorization_strategy, "coordinator", ["begin", "commit", "rollback"])
  RoleBasedAuthorizationStrategy::add_role(authorization_strategy, "participant", ["prepare", "commit", "abort"])
  RoleBasedAuthorizationStrategy::add_role(authorization_strategy, "observer", ["read", "monitor"])
  
  // 添加安全策略
  TransactionSecurityManager::add_encryption_strategy(security_manager, encryption_strategy)
  TransactionSecurityManager::add_authentication_strategy(security_manager, authentication_strategy)
  TransactionSecurityManager::add_authorization_strategy(security_manager, authorization_strategy)
  
  // 启用安全
  DistributedTransactionManager::enable_security(transaction_manager, security_manager)
  
  // 创建安全参与者
  let secure_participants = []
  for i in 0..=2 {
    let participant = SecureTelemetryParticipant::new("secure.service." + i.to_string(), "localhost:200" + i.to_string())
    
    // 配置证书
    SecureTelemetryParticipant::set_certificate(participant, "cert-" + i.to_string() + ".pem")
    SecureTelemetryParticipant::set_role(participant, "participant")
    
    DistributedTransactionManager::add_participant(transaction_manager, participant)
    secure_participants = secure_participants.push(participant)
  }
  
  // 创建安全事务
  let coordinator = SecureTransactionCoordinator::new("secure.coordinator", "coordinator")
  SecureTransactionCoordinator::set_certificate(coordinator, "coordinator-cert.pem")
  
  DistributedTransactionManager::set_coordinator(transaction_manager, coordinator)
  
  let transaction = DistributedTransactionManager::begin_secure_transaction(transaction_manager)
  
  // 添加敏感操作
  let sensitive_operations = [
    MetricOperation::new("CREATE", "user.profile", "sensitive_data"),
    TraceOperation::new("CREATE", "financial.transaction", "high_value"),
    LogOperation::new("CREATE", "audit", "compliance_required")
  ]
  
  for i in 0..=2 {
    DistributedTransaction::add_operation(transaction, secure_participants[i], sensitive_operations[i])
  }
  
  // 执行安全事务
  let transaction_result = DistributedTransactionManager::execute_secure_transaction(transaction_manager, transaction)
  
  // 验证安全事务结果
  assert_eq(transaction_result.status, Committed)
  
  // 验证加密效果
  let encryption_stats = TransactionSecurityManager::get_encryption_stats(security_manager)
  assert_eq(encryption_stats.encrypted_transactions, 1)
  assert_eq(encryption_stats.encrypted_operations, 3)
  assert_true(encryption_stats.encryption_overhead_ms > 0)
  
  // 验证认证效果
  let authentication_stats = TransactionSecurityManager::get_authentication_stats(security_manager)
  assert_eq(authentication_stats.total_authentications, 4)  // 1 coordinator + 3 participants
  assert_eq(authentication_stats.successful_authentications, 4)
  assert_eq(authentication_stats.failed_authentications, 0)
  
  // 验证授权效果
  let authorization_stats = TransactionSecurityManager::get_authorization_stats(security_manager)
  assert_true(authorization_stats.authorization_checks > 0)
  assert_eq(authorization_stats.denied_operations, 0)
  
  // 验证事务日志安全性
  let transaction_logs = DistributedTransactionManager::get_secure_transaction_logs(transaction_manager, transaction.transaction_id)
  assert_true(transaction_logs.length() > 0)
  
  // 验证日志内容已加密
  for log in transaction_logs {
    assert_true(log.is_encrypted)
    assert_false(log.content.contains("sensitive_data"))
  }
}

// 测试10: 分布式事务混合一致性模型
test "分布式事务混合一致性模型测试" {
  let transaction_manager = DistributedTransactionManager::new("混合一致性事务管理器")
  
  // 创建不同一致性要求的参与者
  let strong_consistency_participant = TelemetryParticipant::new("financial.service", "localhost:1001")
  let eventual_consistency_participant = TelemetryParticipant::new("analytics.service", "localhost:1002")
  let causal_consistency_participant = TelemetryParticipant::new("social.service", "localhost:1003")
  let weak_consistency_participant = TelemetryParticipant::new("cache.service", "localhost:1004")
  
  // 配置一致性模型
  TelemetryParticipant::set_consistency_model(strong_consistency_participant, StrongConsistency)
  TelemetryParticipant::set_consistency_model(eventual_consistency_participant, EventualConsistency)
  TelemetryParticipant::set_consistency_model(causal_consistency_participant, CausalConsistency)
  TelemetryParticipant::set_consistency_model(weak_consistency_participant, WeakConsistency)
  
  // 添加参与者
  DistributedTransactionManager::add_participant(transaction_manager, strong_consistency_participant)
  DistributedTransactionManager::add_participant(transaction_manager, eventual_consistency_participant)
  DistributedTransactionManager::add_participant(transaction_manager, causal_consistency_participant)
  DistributedTransactionManager::add_participant(transaction_manager, weak_consistency_participant)
  
  // 配置混合一致性管理器
  let consistency_manager = HybridConsistencyManager::new()
  HybridConsistencyManager::set_global_consistency_level(consistency_manager, ReadCommitted)
  HybridConsistencyManager::enable_adaptive_consistency(consistency_manager)
  
  DistributedTransactionManager::set_consistency_manager(transaction_manager, consistency_manager)
  
  // 创建混合一致性事务
  let transaction = DistributedTransactionManager::begin_hybrid_transaction(transaction_manager)
  
  // 添加不同一致性要求的操作
  let strong_operation = FinancialOperation::new("TRANSFER", "accountA", "accountB", 1000.0)
  let eventual_operation = AnalyticsOperation::new("UPDATE", "user.stats", "profile.views")
  let causal_operation = SocialOperation::new("POST", "user.timeline", "new.post")
  let weak_operation = CacheOperation::new("INVALIDATE", "user.profile", "user-123")
  
  DistributedTransaction::add_operation(transaction, strong_consistency_participant, strong_operation)
  DistributedTransaction::add_operation(transaction, eventual_consistency_participant, eventual_operation)
  DistributedTransaction::add_operation(transaction, causal_consistency_participant, causal_operation)
  DistributedTransaction::add_operation(transaction, weak_consistency_participant, weak_operation)
  
  // 执行混合一致性事务
  let transaction_result = DistributedTransactionManager::execute_hybrid_transaction(transaction_manager, transaction)
  
  // 验证事务结果
  assert_eq(transaction_result.status, Committed)
  
  // 验证一致性级别
  let strong_consistency_result = TelemetryParticipant::get_consistency_result(strong_consistency_participant, transaction.transaction_id)
  let eventual_consistency_result = TelemetryParticipant::get_consistency_result(eventual_consistency_participant, transaction.transaction_id)
  let causal_consistency_result = TelemetryParticipant::get_consistency_result(causal_consistency_participant, transaction.transaction_id)
  let weak_consistency_result = TelemetryParticipant::get_consistency_result(weak_consistency_participant, transaction.transaction_id)
  
  assert_eq(strong_consistency_result.consistency_level, StrongConsistency)
  assert_eq(strong_consistency_result.is_consistent, true)
  
  assert_eq(eventual_consistency_result.consistency_level, EventualConsistency)
  assert_true(eventual_consistency_result.time_to_consistency_ms > 0)
  
  assert_eq(causal_consistency_result.consistency_level, CausalConsistency)
  assert_true(causal_consistency_result.causal_dependencies_preserved)
  
  assert_eq(weak_consistency_result.consistency_level, WeakConsistency)
  assert_true(weak_consistency_result.is_accessible)
  
  // 验证混合一致性统计
  let hybrid_stats = HybridConsistencyManager::get_stats(consistency_manager)
  assert_eq(hybrid_stats.total_hybrid_transactions, 1)
  assert_eq(hybrid_stats.strong_consistency_operations, 1)
  assert_eq(hybrid_stats.eventual_consistency_operations, 1)
  assert_eq(hybrid_stats.causal_consistency_operations, 1)
  assert_eq(hybrid_stats.weak_consistency_operations, 1)
  
  // 验证自适应一致性
  let adaptive_stats = HybridConsistencyManager::get_adaptive_stats(consistency_manager)
  assert_true(adaptive_stats.consistency_adjustments >= 0)
  assert_true(adaptive_stats.performance_optimizations >= 0)
}