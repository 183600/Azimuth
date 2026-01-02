// Azimuth 遥测批量处理测试用例
// 专注于遥测数据的批量处理和优化功能

// 测试1: 基础批量处理
test "基础批量处理测试" {
  let batch_processor = BatchProcessor::new("基础批量处理器")
  
  // 配置批量处理参数
  BatchProcessor::set_batch_size(batch_processor, 100)
  BatchProcessor::set_flush_interval(batch_processor, 5000)  // 5秒
  BatchProcessor::set_max_wait_time(batch_processor, 10000)  // 最大等待10秒
  
  // 创建数据源和接收器
  let source = MockDataSource::new()
  let sink = MemorySink::new()
  
  // 添加测试数据
  for i in 0..=150 {
    let metric = Metric::new("batch.metric", i.to_float())
    MockDataSource::add_metric(source, metric)
  }
  
  // 配置处理管道
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_sink(batch_processor, sink)
  
  // 启动批量处理
  BatchProcessor::start(batch_processor)
  
  // 等待处理完成
  BatchProcessor::wait_for_completion(batch_processor, 15000)
  
  // 验证批量处理结果
  let processed_metrics = MemorySink::get_metrics(sink)
  assert_eq(processed_metrics.length(), 151)
  
  // 验证批处理统计
  let batch_stats = BatchProcessor::get_batch_stats(batch_processor)
  assert_eq(batch_stats.total_items_processed, 151)
  assert_eq(batch_stats.total_batches_processed, 2)  // 151条记录，每批100条，应该有2批
  assert_true(batch_stats.avg_batch_size >= 75.0 && batch_stats.avg_batch_size <= 100.0)
}

// 测试2: 动态批量大小调整
test "动态批量大小调整测试" {
  let batch_processor = BatchProcessor::new("动态批量处理器")
  let adaptive_strategy = AdaptiveBatchStrategy::new()
  
  // 配置自适应策略
  AdaptiveBatchStrategy::set_min_batch_size(adaptive_strategy, 10)
  AdaptiveBatchStrategy::set_max_batch_size(adaptive_strategy, 1000)
  AdaptiveBatchStrategy::set_target_latency(adaptive_strategy, 100)  // 目标延迟100ms
  AdaptiveBatchStrategy::set_adjustment_factor(adaptive_strategy, 1.5)
  
  BatchProcessor::set_adaptive_strategy(batch_processor, adaptive_strategy)
  
  // 创建数据源和接收器
  let source = MockDataSource::new()
  let sink = MemorySink::new()
  
  // 模拟不同负载场景
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_sink(batch_processor, sink)
  
  BatchProcessor::start(batch_processor)
  
  // 低负载场景
  for i in 0..=50 {
    let metric = Metric::new("low.load.metric", i.to_float())
    MockDataSource::add_metric(source, metric)
  }
  
  Time::sleep(2000)  // 等待处理
  
  let low_load_stats = BatchProcessor::get_batch_stats(batch_processor)
  let low_load_batch_size = low_load_stats.current_batch_size
  
  // 高负载场景
  for i in 0..=500 {
    let metric = Metric::new("high.load.metric", i.to_float())
    MockDataSource::add_metric(source, metric)
  }
  
  Time::sleep(5000)  // 等待处理
  
  let high_load_stats = BatchProcessor::get_batch_stats(batch_processor)
  let high_load_batch_size = high_load_stats.current_batch_size
  
  // 验证动态调整
  assert_true(high_load_batch_size >= low_load_batch_size)
  assert_true(low_load_batch_size >= 10 && low_load_batch_size <= 1000)
  assert_true(high_load_batch_size >= 10 && high_load_batch_size <= 1000)
  
  BatchProcessor::stop(batch_processor)
}

// 测试3: 批量数据转换和聚合
test "批量数据转换和聚合测试" {
  let batch_processor = BatchProcessor::new("转换聚合批量处理器")
  
  // 配置转换器
  let metric_transformer = MetricTransformer::new()
  let trace_transformer = TraceTransformer::new()
  let log_transformer = LogTransformer::new()
  
  // 配置聚合器
  let sum_aggregator = SumAggregator::new("batch.sum")
  let avg_aggregator = AvgAggregator::new("batch.avg")
  let count_aggregator = CountAggregator::new("batch.count")
  
  BatchProcessor::add_transformer(batch_processor, metric_transformer)
  BatchProcessor::add_transformer(batch_processor, trace_transformer)
  BatchProcessor::add_transformer(batch_processor, log_transformer)
  
  BatchProcessor::add_aggregator(batch_processor, sum_aggregator)
  BatchProcessor::add_aggregator(batch_processor, avg_aggregator)
  BatchProcessor::add_aggregator(batch_processor, count_aggregator)
  
  // 创建测试数据
  let source = MockDataSource::new()
  let sink = MemorySink::new()
  
  // 添加指标数据
  for i in 0..=99 {
    let metric = Metric::new("test.metric", (i + 1).to_float())
    Metric::add_attribute(metric, "batch.id", "batch-1")
    MockDataSource::add_metric(source, metric)
  }
  
  // 添加追踪数据
  for i in 0..=49 {
    let trace = Trace::new("trace-" + i.to_string(), "test.operation", Ok, 100 + i)
    Trace::add_attribute(trace, "batch.id", "batch-1")
    MockDataSource::add_trace(source, trace)
  }
  
  // 添加日志数据
  for i in 0..=74 {
    let log = LogEntry::new(Info, "Test message " + i.to_string(), "test.service")
    LogEntry::add_attribute(log, "batch.id", "batch-1")
    MockDataSource::add_log(source, log)
  }
  
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_sink(batch_processor, sink)
  BatchProcessor::set_batch_size(batch_processor, 50)
  
  BatchProcessor::start(batch_processor)
  BatchProcessor::wait_for_completion(batch_processor, 10000)
  
  // 验证转换和聚合结果
  let processed_data = MemorySink::get_all_data(sink)
  
  // 验证聚合结果
  let sum_results = processed_data.filter(|d| d.name.contains("batch.sum"))
  let avg_results = processed_data.filter(|d| d.name.contains("batch.avg"))
  let count_results = processed_data.filter(|d| d.name.contains("batch.count"))
  
  assert_true(sum_results.length() > 0)
  assert_true(avg_results.length() > 0)
  assert_true(count_results.length() > 0)
  
  // 验证聚合计算
  let metric_sum = sum_results.find(|d| d.name.contains("test.metric"))
  assert_true(metric_sum.is_some())
  assert_eq(metric_sum.unwrap().value, 5050.0)  // 1+2+...+100 = 5050
  
  let metric_avg = avg_results.find(|d| d.name.contains("test.metric"))
  assert_true(metric_avg.is_some())
  assert_eq(metric_avg.unwrap().value, 50.5)  // 5050/100 = 50.5
  
  let metric_count = count_results.find(|d| d.name.contains("test.metric"))
  assert_true(metric_count.is_some())
  assert_eq(metric_count.unwrap().value, 100.0)
}

// 测试4: 批量错误处理和恢复
test "批量错误处理和恢复测试" {
  let batch_processor = BatchProcessor::new("容错批量处理器")
  
  // 配置错误处理策略
  let retry_policy = RetryPolicy::exponential_backoff(3, 100, 2000)
  let error_filter = ErrorFilter::new()
  let dead_letter_queue = DeadLetterQueue::new()
  
  BatchProcessor::set_retry_policy(batch_processor, retry_policy)
  BatchProcessor::set_error_filter(batch_processor, error_filter)
  BatchProcessor::set_dead_letter_queue(batch_processor, dead_letter_queue)
  
  // 创建会失败的接收器
  let failing_sink = FailingSink::new("failing", 0.3)  // 30%失败率
  let recovery_sink = MemorySink::new()
  
  // 配置故障转移
  let failover_strategy = FailoverStrategy::new()
  FailoverStrategy::add_sink(failover_strategy, failing_sink)
  FailoverStrategy::add_sink(failover_strategy, recovery_sink)
  
  BatchProcessor::set_failover_strategy(batch_processor, failover_strategy)
  
  // 创建测试数据
  let source = MockDataSource::new()
  for i in 0..=99 {
    let metric = Metric::new("error.test.metric", i.to_float())
    MockDataSource::add_metric(source, metric)
  }
  
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_batch_size(batch_processor, 20)
  
  BatchProcessor::start(batch_processor)
  BatchProcessor::wait_for_completion(batch_processor, 15000)
  
  // 验证错误处理结果
  let successful_metrics = MemorySink::get_metrics(recovery_sink)
  let dead_letter_items = DeadLetterQueue::get_items(dead_letter_queue)
  let error_stats = BatchProcessor::get_error_stats(batch_processor)
  
  // 应该有部分成功处理的指标
  assert_true(successful_metrics.length() > 0)
  
  // 应该有部分进入死信队列的指标
  assert_true(dead_letter_items.length() > 0)
  
  // 验证错误统计
  assert_true(error_stats.total_attempts > 0)
  assert_true(error_stats.successful_attempts > 0)
  assert_true(error_stats.failed_attempts > 0)
  assert_true(error_stats.retry_attempts > 0)
  
  // 总处理量应该等于输入量
  assert_eq(successful_metrics.length() + dead_letter_items.length(), 100)
}

// 测试5: 批量数据压缩和序列化
test "批量数据压缩和序列化测试" {
  let batch_processor = BatchProcessor::new("压缩序列化批量处理器")
  
  // 配置压缩策略
  let gzip_compression = GzipCompressionStrategy::new(6)
  let lz4_compression = LZ4CompressionStrategy::new()
  let adaptive_compression = AdaptiveCompressionStrategy::new()
  
  // 配置序列化策略
  let json_serialization = JsonSerializationStrategy::new()
  let binary_serialization = BinarySerializationStrategy::new()
  let protobuf_serialization = ProtobufSerializationStrategy::new()
  
  BatchProcessor::set_compression_strategy(batch_processor, adaptive_compression)
  BatchProcessor::set_serialization_strategy(batch_processor, binary_serialization)
  
  // 创建测试数据
  let source = MockDataSource::new()
  let sink = MemorySink::new()
  
  // 添加大量数据
  for i in 0..=999 {
    let trace = Trace::new("trace-" + i.to_string(), "large.operation", Ok, 200 + i)
    
    // 添加大量属性
    for j in 0..=20 {
      Trace::add_attribute(trace, "attr." + j.to_string(), "value." + j.to_string())
    }
    
    // 添加大量span
    for k in 0..=10 {
      let span = Span::new("span-" + k.to_string(), "sub.operation", Server)
      Span::add_attribute(span, "data", "x".repeat(100))  // 100字符数据
      Trace::add_span(trace, span)
    }
    
    MockDataSource::add_trace(source, trace)
  }
  
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_sink(batch_processor, sink)
  BatchProcessor::set_batch_size(batch_processor, 100)
  
  // 记录原始大小
  let original_total_size = calculate_total_data_size(source)
  
  BatchProcessor::start(batch_processor)
  BatchProcessor::wait_for_completion(batch_processor, 30000)
  
  // 验证压缩和序列化结果
  let processed_batches = MemorySink::get_batches(sink)
  let compression_stats = BatchProcessor::get_compression_stats(batch_processor)
  let serialization_stats = BatchProcessor::get_serialization_stats(batch_processor)
  
  // 验证批处理
  assert_eq(processed_batches.length(), 10)  // 1000条记录，每批100条
  
  // 验证压缩效果
  assert_true(compression_stats.compression_ratio < 1.0)
  assert_true(compression_stats.space_saved_mb > 0)
  
  // 验证序列化效果
  assert_true(serialization_stats.serialization_time_ms > 0)
  assert_true(serialization_stats.deserialization_time_ms > 0)
  assert_true(serialization_stats.avg_serialization_time_ms < 100)
  
  // 验证数据完整性
  let deserialized_count = processed_batches.map(|batch| batch.item_count).reduce(0, +)
  assert_eq(deserialized_count, 1000)
}

// 测试6: 批量数据处理优先级
test "批量数据处理优先级测试" {
  let batch_processor = BatchProcessor::new("优先级批量处理器")
  
  // 配置优先级策略
  let priority_strategy = PriorityStrategy::new()
  PriorityStrategy::add_priority_level(priority_strategy, "critical", 10)
  PriorityStrategy::add_priority_level(priority_strategy, "high", 7)
  PriorityStrategy::add_priority_level(priority_strategy, "normal", 5)
  PriorityStrategy::add_priority_level(priority_strategy, "low", 2)
  
  BatchProcessor::set_priority_strategy(batch_processor, priority_strategy)
  
  // 创建测试数据
  let source = MockDataSource::new()
  let sink = MemorySink::new()
  
  // 添加不同优先级的数据
  for i in 0..=24 {
    let critical_metric = Metric::new("critical.metric", i.to_float())
    Metric::add_attribute(critical_metric, "priority", "critical")
    MockDataSource::add_metric(source, critical_metric)
    
    let high_metric = Metric::new("high.metric", i.to_float())
    Metric::add_attribute(high_metric, "priority", "high")
    MockDataSource::add_metric(source, high_metric)
    
    let normal_metric = Metric::new("normal.metric", i.to_float())
    Metric::add_attribute(normal_metric, "priority", "normal")
    MockDataSource::add_metric(source, normal_metric)
    
    let low_metric = Metric::new("low.metric", i.to_float())
    Metric::add_attribute(low_metric, "priority", "low")
    MockDataSource::add_metric(source, low_metric)
  }
  
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_sink(batch_processor, sink)
  BatchProcessor::set_batch_size(batch_processor, 20)
  
  BatchProcessor::start(batch_processor)
  BatchProcessor::wait_for_completion(batch_processor, 15000)
  
  // 验证优先级处理结果
  let processed_metrics = MemorySink::get_metrics(sink)
  
  // 按优先级分组
  let critical_metrics = processed_metrics.filter(|m| m.name == "critical.metric")
  let high_metrics = processed_metrics.filter(|m| m.name == "high.metric")
  let normal_metrics = processed_metrics.filter(|m| m.name == "normal.metric")
  let low_metrics = processed_metrics.filter(|m| m.name == "low.metric")
  
  // 验证处理顺序（高优先级应该先处理）
  let processing_order = BatchProcessor::get_processing_order(batch_processor)
  
  // 检查critical是否在high之前处理
  let critical_first_batch = processing_order.find_index(|batch| batch.contains_critical)
  let high_first_batch = processing_order.find_index(|batch| batch.contains_high)
  
  if critical_first_batch.is_some() && high_first_batch.is_some() {
    assert_true(critical_first_batch.unwrap() <= high_first_batch.unwrap())
  }
  
  // 验证所有数据都被处理
  assert_eq(critical_metrics.length(), 25)
  assert_eq(high_metrics.length(), 25)
  assert_eq(normal_metrics.length(), 25)
  assert_eq(low_metrics.length(), 25)
  
  // 验证优先级统计
  let priority_stats = BatchProcessor::get_priority_stats(batch_processor)
  assert_eq(priority_stats.critical_processed, 25)
  assert_eq(priority_stats.high_processed, 25)
  assert_eq(priority_stats.normal_processed, 25)
  assert_eq(priority_stats.low_processed, 25)
}

// 测试7: 批量数据分区和分片
test "批量数据分区和分片测试" {
  let batch_processor = BatchProcessor::new("分区分片批量处理器")
  
  // 配置分区策略
  let hash_partitioner = HashPartitioner::new("service.name")
  let range_partitioner = RangePartitioner::new("timestamp", [0, 1000, 2000, 3000])
  let round_robin_partitioner = RoundRobinPartitioner::new(4)
  
  // 配置分片策略
  let consistent_hash_sharding = ConsistentHashSharding::new(8)
  let mod_sharding = ModSharding::new(4)
  
  BatchProcessor::set_partition_strategy(batch_processor, hash_partitioner)
  BatchProcessor::set_sharding_strategy(batch_processor, consistent_hash_sharding)
  
  // 创建多个接收器
  let sinks = [
    MemorySink::new(),
    MemorySink::new(),
    MemorySink::new(),
    MemorySink::new()
  ]
  
  // 创建测试数据
  let source = MockDataSource::new()
  let services = ["service-a", "service-b", "service-c", "service-d"]
  
  for i in 0..=399 {
    let service = services[i % 4]
    let metric = Metric::new("partitioned.metric", i.to_float())
    Metric::add_attribute(metric, "service.name", service)
    Metric::add_attribute(metric, "timestamp", (i * 10).to_int())
    MockDataSource::add_metric(source, metric)
  }
  
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_batch_size(batch_processor, 50)
  
  // 配置多接收器
  for i in 0..=3 {
    BatchProcessor::add_sink(batch_processor, sinks[i])
  }
  
  BatchProcessor::start(batch_processor)
  BatchProcessor::wait_for_completion(batch_processor, 20000)
  
  // 验证分区和分片结果
  let partition_stats = BatchProcessor::get_partition_stats(batch_processor)
  let sharding_stats = BatchProcessor::get_sharding_stats(batch_processor)
  
  // 验证分区
  assert_eq(partition_stats.total_partitions, 4)  // 4个服务
  assert_true(partition_stats.partition_distribution.all(|count| count > 0))
  
  // 验证分片
  assert_eq(sharding_stats.total_shards, 8)
  assert_true(sharding_stats.shard_distribution.all(|count| count > 0))
  
  // 验证每个接收器都有数据
  for sink in sinks {
    let metrics = MemorySink::get_metrics(sink)
    assert_true(metrics.length() > 0)
  }
  
  // 验证数据完整性
  let total_processed = sinks.map(|sink| MemorySink::get_metrics(sink).length()).reduce(0, +)
  assert_eq(total_processed, 400)
}

// 测试8: 批量数据监控和指标
test "批量数据监控和指标测试" {
  let batch_processor = BatchProcessor::new("监控批量处理器")
  let monitor = BatchMonitor::new()
  
  // 配置监控指标
  BatchMonitor::enable_throughput_monitoring(monitor)
  BatchMonitor::enable_latency_monitoring(monitor)
  BatchMonitor::enable_error_rate_monitoring(monitor)
  BatchMonitor::enable_resource_usage_monitoring(monitor)
  
  // 创建测试数据
  let source = MockDataSource::new()
  let sink = MemorySink::new()
  
  // 添加不同类型和负载的数据
  for i in 0..=499 {
    let metric = Metric::new("monitored.metric", i.to_float())
    MockDataSource::add_metric(source, metric)
    
    if i % 10 == 0 {
      let trace = Trace::new("trace-" + i.to_string(), "monitored.operation", Ok, 100 + i)
      MockDataSource::add_trace(source, trace)
    }
    
    if i % 20 == 0 {
      let log = LogEntry::new(Info, "Monitored log " + i.to_string(), "test.service")
      MockDataSource::add_log(source, log)
    }
  }
  
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_sink(batch_processor, sink)
  BatchProcessor::set_batch_size(batch_processor, 50)
  
  // 启动监控
  BatchMonitor::start_monitoring(monitor, batch_processor)
  
  BatchProcessor::start(batch_processor)
  BatchProcessor::wait_for_completion(batch_processor, 25000)
  
  // 获取监控指标
  let monitoring_metrics = BatchMonitor::get_metrics(monitor)
  
  // 验证吞吐量指标
  assert_true(monitoring_metrics.throughput_items_per_second > 0)
  assert_true(monitoring_metrics.throughput_batches_per_second > 0)
  assert_true(monitoring_metrics.peak_throughput > 0)
  
  // 验证延迟指标
  assert_true(monitoring_metrics.avg_batch_latency_ms > 0)
  assert_true(monitoring_metrics.p95_batch_latency_ms > 0)
  assert_true(monitoring_metrics.p99_batch_latency_ms > 0)
  
  // 验证错误率指标
  assert_true(monitoring_metrics.error_rate >= 0.0 && monitoring_metrics.error_rate <= 1.0)
  
  // 验证资源使用指标
  assert_true(monitoring_metrics.memory_usage_mb > 0)
  assert_true(monitoring_metrics.cpu_usage_percent >= 0.0)
  
  // 验证处理统计
  assert_eq(monitoring_metrics.total_items_processed, 575)  // 500 metrics + 50 traces + 25 logs
  assert_eq(monitoring_metrics.total_batches_processed, 10)  // 575 items / 50 batch size ≈ 11.5, 向下取整为11批
  
  // 验证监控历史
  let monitoring_history = BatchMonitor::get_history(monitor, 3600)  // 最近1小时
  assert_true(monitoring_history.length() > 0)
}

// 测试9: 批量数据持久化和恢复
test "批量数据持久化和恢复测试" {
  let batch_processor = BatchProcessor::new("持久化批量处理器")
  let persistence_manager = BatchPersistenceManager::new()
  
  // 配置持久化策略
  let file_persistence = FilePersistenceStrategy::new("/tmp/batch_backup")
  let database_persistence = DatabasePersistenceStrategy::new("postgresql://localhost/telemetry")
  
  BatchPersistenceManager::add_strategy(persistence_manager, file_persistence)
  BatchPersistenceManager::add_strategy(persistence_manager, database_persistence)
  
  // 配置检查点策略
  let checkpoint_strategy = CheckpointStrategy::new()
  CheckpointStrategy::set_interval(checkpoint_strategy, 5000)  // 每5秒检查点
  CheckpointStrategy::set_batch_threshold(checkpoint_strategy, 100)  // 每100批检查点
  
  BatchPersistenceManager::set_checkpoint_strategy(persistence_manager, checkpoint_strategy)
  
  // 创建测试数据
  let source = MockDataSource::new()
  let sink = MemorySink::new()
  
  for i in 0..=199 {
    let metric = Metric::new("persistent.metric", i.to_float())
    Metric::add_attribute(metric, "batch.id", "batch-" + (i / 50).to_string())
    MockDataSource::add_metric(source, metric)
  }
  
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_sink(batch_processor, sink)
  BatchProcessor::set_batch_size(batch_processor, 25)
  
  // 启用持久化
  BatchProcessor::set_persistence_manager(batch_processor, persistence_manager)
  
  BatchProcessor::start(batch_processor)
  
  // 等待第一部分处理完成
  Time::sleep(6000)
  
  // 创建检查点
  let checkpoint_id = BatchPersistenceManager::create_checkpoint(persistence_manager, batch_processor)
  
  // 等待剩余处理完成
  BatchProcessor::wait_for_completion(batch_processor, 15000)
  
  // 验证处理结果
  let processed_metrics = MemorySink::get_metrics(sink)
  assert_true(processed_metrics.length() > 0)
  
  // 模拟故障并恢复
  let recovered_processor = BatchProcessor::new("恢复的批量处理器")
  let recovered_sink = MemorySink::new()
  
  BatchProcessor::set_sink(recovered_processor, recovered_sink)
  BatchProcessor::set_persistence_manager(recovered_processor, persistence_manager)
  
  // 从检查点恢复
  BatchPersistenceManager::restore_from_checkpoint(persistence_manager, recovered_processor, checkpoint_id)
  
  // 验证恢复结果
  let recovered_metrics = MemorySink::get_metrics(recovered_sink)
  let persistence_stats = BatchPersistenceManager::get_stats(persistence_manager)
  
  // 验证持久化统计
  assert_true(persistence_stats.checkpoints_created > 0)
  assert_true(persistence_stats.checkpoints_restored > 0)
  assert_true(persistence_stats.bytes_persisted > 0)
  
  // 验证恢复完整性
  assert_true(recovered_metrics.length() > 0)
}

// 测试10: 批量数据流控制
test "批量数据流控制测试" {
  let batch_processor = BatchProcessor::new("流控制批量处理器")
  let flow_controller = FlowController::new()
  
  // 配置流控制策略
  let rate_limiter = RateLimiter::new(100)  // 每秒100个项目
  let backpressure_controller = BackpressureController::new()
  let throttle_controller = ThrottleController::new()
  
  FlowController::add_strategy(flow_controller, rate_limiter)
  FlowController::add_strategy(flow_controller, backpressure_controller)
  FlowController::add_strategy(flow_controller, throttle_controller)
  
  // 配置背压参数
  BackpressureController::set_buffer_size(backpressure_controller, 200)
  BackpressureController::set_high_watermark(backpressure_controller, 150)
  BackpressureController::set_low_watermark(backpressure_controller, 50)
  
  // 配置限流参数
  ThrottleController::set_burst_size(throttle_controller, 50)
  ThrottleController::set_refill_rate(throttle_controller, 10)
  
  BatchProcessor::set_flow_controller(batch_processor, flow_controller)
  
  // 创建测试数据
  let source = MockDataSource::new()
  let sink = SlowSink::new(20)  // 每个处理20ms延迟
  
  // 快速添加大量数据
  for i in 0..=299 {
    let metric = Metric::new("flow.control.metric", i.to_float())
    MockDataSource::add_metric(source, metric)
  }
  
  BatchProcessor::set_source(batch_processor, source)
  BatchProcessor::set_sink(batch_processor, sink)
  BatchProcessor::set_batch_size(batch_processor, 30)
  
  // 记录开始时间
  let start_time = Time::now()
  
  BatchProcessor::start(batch_processor)
  BatchProcessor::wait_for_completion(batch_processor, 30000)
  
  let end_time = Time::now()
  let total_time = end_time - start_time
  
  // 验证流控制结果
  let processed_metrics = SlowSink::get_metrics(sink)
  let flow_control_stats = FlowController::get_stats(flow_controller)
  
  // 验证所有数据都被处理
  assert_eq(processed_metrics.length(), 300)
  
  // 验证流控制生效
  assert_true(flow_control_stats.rate_limited_count > 0)
  assert_true(flow_control_stats.backpressure_applied_count > 0)
  assert_true(flow_control_stats.throttled_count > 0)
  
  // 验证处理时间受流控制影响
  // 如果没有流控制，300个指标应该很快处理完成
  // 有流控制的情况下，处理时间应该更长
  assert_true(total_time > 5000)  // 至少5秒
  
  // 验证缓冲区管理
  let buffer_stats = BackpressureController::get_buffer_stats(backpressure_controller)
  assert_true(buffer_stats.max_buffer_usage <= 200)
  assert_true(buffer_stats.overflow_count >= 0)
}

// 辅助函数：计算数据总大小
fn calculate_total_data_size(source : MockDataSource) -> Int {
  let metrics = MockDataSource::get_metrics(source)
  let traces = MockDataSource::get_traces(source)
  let logs = MockDataSource::get_logs(source)
  
  let metrics_size = metrics.map(|m| m.size).reduce(0, +)
  let traces_size = traces.map(|t| t.size).reduce(0, +)
  let logs_size = logs.map(|l| l.size).reduce(0, +)
  
  metrics_size + traces_size + logs_size
}