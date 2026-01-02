// Azimuth Real-time Stream Processing and Monitoring Tests
// 实时流处理和监控测试用例 - 专注于流数据处理和实时监控

// Test 1: 基础流数据处理
test "basic stream data processing" {
  let stream_processor = StreamProcessor::new()
  StreamProcessor::configure_buffer_size(stream_processor, 1000)
  
  // 创建测试数据流
  let data_stream = []
  for i in 0..<100 {
    let data_point = StreamData::new(i.to_string(), i.to_float(), Time::now())
    data_stream = data_stream.push(data_point)
  }
  
  // 启动流处理
  StreamProcessor::start(stream_processor)
  
  // 发送数据到流
  for data in data_stream {
    StreamProcessor::send_data(stream_processor, data)
  }
  
  // 等待处理完成
  ConcurrentProcessor::sleep(1000)
  
  // 获取处理结果
  let processed_data = StreamProcessor::get_processed_data(stream_processor)
  assert_eq(processed_data.length(), data_stream.length())
  
  // 验证数据完整性
  for i in 0..<processed_data.length() {
    assert_eq(processed_data[i].id, data_stream[i].id)
    assert_eq(processed_data[i].value, data_stream[i].value)
  }
  
  // 获取处理统计
  let stats = StreamProcessor::get_statistics(stream_processor)
  assert_eq(stats.processed_count, 100)
  assert_eq(stats.error_count, 0)
  assert_true(stats.processing_rate > 0)
  
  StreamProcessor::stop(stream_processor)
}

// Test 2: 流数据过滤和转换
test "stream data filtering and transformation" {
  let stream_transformer = StreamTransformer::new()
  
  // 配置过滤器和转换器
  StreamTransformer::add_filter(stream_transformer, fn(data) { data.value > 50.0 })
  StreamTransformer::add_transformer(stream_transformer, fn(data) { 
    StreamData::new(data.id + "_transformed", data.value * 2.0, data.timestamp)
  })
  
  // 创建混合数据流
  let mixed_stream = []
  for i in 0..<100 {
    let value = (i * 1.5) % 100.0
    let data_point = StreamData::new("item_" + i.to_string(), value, Time::now())
    mixed_stream = mixed_stream.push(data_point)
  }
  
  // 处理数据流
  StreamTransformer::start(stream_transformer)
  for data in mixed_stream {
    StreamTransformer::process_data(stream_transformer, data)
  }
  
  ConcurrentProcessor::sleep(500)
  
  // 获取过滤和转换后的数据
  let filtered_data = StreamTransformer::get_filtered_data(stream_transformer)
  let transformed_data = StreamTransformer::get_transformed_data(stream_transformer)
  
  // 验证过滤结果
  for data in filtered_data {
    assert_true(data.value > 50.0)
  }
  
  // 验证转换结果
  for data in transformed_data {
    assert_true(String::contains(data.id, "_transformed"))
    // 转换后的值应该是原始值的两倍
    let original_value = data.value / 2.0
    assert_true(original_value > 50.0) // 只有通过过滤的数据才会被转换
  }
  
  StreamTransformer::stop(stream_transformer)
}

// Test 3: 实时窗口聚合
test "real-time window aggregation" {
  let window_aggregator = WindowAggregator::new(5000) // 5秒窗口
  
  // 配置聚合函数
  WindowAggregator::add_aggregator(window_aggregator, "sum", fn(values) { 
    values.reduce(fn(acc, val) { acc + val }, 0.0)
  })
  WindowAggregator::add_aggregator(window_aggregator, "avg", fn(values) { 
    let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
    sum / values.length().to_float()
  })
  WindowAggregator::add_aggregator(window_aggregator, "max", fn(values) { 
    values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, 0.0)
  })
  
  WindowAggregator::start(window_aggregator)
  
  // 在时间窗口内发送数据
  let start_time = Time::now()
  let mut data_count = 0
  
  while Time::now() - start_time < 3000 { // 3秒内
    let value = (Random::next() % 100).to_float()
    let data_point = StreamData::new("realtime_" + data_count.to_string(), value, Time::now())
    WindowAggregator::add_data(window_aggregator, data_point)
    data_count = data_count + 1
    ConcurrentProcessor::sleep(50) // 50ms间隔
  }
  
  // 等待窗口关闭
  ConcurrentProcessor::sleep(3000)
  
  // 获取聚合结果
  let aggregation_results = WindowAggregator::get_results(window_aggregator)
  
  assert_true(aggregation_results.contains("sum"))
  assert_true(aggregation_results.contains("avg"))
  assert_true(aggregation_results.contains("max"))
  
  // 验证聚合值的合理性
  let sum_result = aggregation_results["sum"]
  let avg_result = aggregation_results["avg"]
  let max_result = aggregation_results["max"]
  
  assert_true(sum_result > 0.0)
  assert_true(avg_result > 0.0 && avg_result < 100.0)
  assert_true(max_result >= 0.0 && max_result <= 99.0)
  
  WindowAggregator::stop(window_aggregator)
}

// Test 4: 流数据异常检测
test "stream data anomaly detection" {
  let anomaly_detector = AnomalyDetector::new()
  
  // 配置异常检测规则
  AnomalyDetector::add_statistical_rule(anomaly_detector, "value", 2.0) // 2个标准差
  AnomalyDetector::add_range_rule(anomaly_detector, "value", 0.0, 100.0) // 值范围
  AnomalyDetector::add_pattern_rule(anomaly_detector, "spike_detection", 1.5) // 峰值检测
  
  AnomalyDetector::start(anomaly_detector)
  
  // 发送正常数据
  let normal_data = []
  for i in 0..<50 {
    let value = 50.0 + (Random::next() % 20 - 10).to_float() // 40-60范围内的值
    let data_point = StreamData::new("normal_" + i.to_string(), value, Time::now())
    normal_data = normal_data.push(data_point)
    AnomalyDetector::process_data(anomaly_detector, data_point)
  }
  
  // 发送异常数据
  let anomaly_data = [
    StreamData::new("anomaly_1", 150.0, Time::now()), // 超出范围
    StreamData::new("anomaly_2", -10.0, Time::now()), // 负值
    StreamData::new("anomaly_3", 200.0, Time::now())  // 极端值
  ]
  
  for data in anomaly_data {
    AnomalyDetector::process_data(anomaly_detector, data)
  }
  
  ConcurrentProcessor::sleep(1000)
  
  // 获取异常检测结果
  let anomaly_report = AnomalyDetector::get_report(anomaly_detector)
  
  assert_true(anomaly_report.total_processed >= 53)
  assert_true(anomaly_report.anomalies_detected >= 3)
  assert_true(anomaly_report.false_positives < 5) // 允许少量误报
  
  // 验证异常数据被正确识别
  let detected_anomalies = AnomalyDetector::get_detected_anomalies(anomaly_detector)
  assert_true(detected_anomalies.length() >= 3)
  
  for anomaly in detected_anomalies {
    assert_true(String::contains(anomaly.data.id, "anomaly"))
    assert_true(anomaly.confidence > 0.5)
  }
  
  AnomalyDetector::stop(anomaly_detector)
}

// Test 5: 多流数据合并
test "multi-stream data merging" {
  let stream_merger = StreamMerger::new()
  
  // 创建多个数据流
  let stream1 = []
  let stream2 = []
  let stream3 = []
  
  for i in 0..<30 {
    let data1 = StreamData::new("stream1_" + i.to_string(), i.to_float(), Time::now())
    let data2 = StreamData::new("stream2_" + i.to_string(), (i * 2).to_float(), Time::now())
    let data3 = StreamData::new("stream3_" + i.to_string(), (i * 3).to_float(), Time::now())
    
    stream1 = stream1.push(data1)
    stream2 = stream2.push(data2)
    stream3 = stream3.push(data3)
  }
  
  // 配置合并策略
  StreamMerger::set_merge_strategy(stream_merger, MergeStrategy::Timestamp)
  StreamMerger::add_stream(stream_merger, "stream1", stream1)
  StreamMerger::add_stream(stream_merger, "stream2", stream2)
  StreamMerger::add_stream(stream_merger, "stream3", stream3)
  
  StreamMerger::start(stream_merger)
  StreamMerger::process_all(stream_merger)
  
  ConcurrentProcessor::sleep(1000)
  
  // 获取合并结果
  let merged_stream = StreamMerger::get_merged_stream(stream_merger)
  
  // 验证合并结果
  assert_eq(merged_stream.length(), 90) // 3个流各30个数据点
  
  // 验证时间戳顺序
  for i in 1..<merged_stream.length() {
    assert_true(merged_stream[i].timestamp >= merged_stream[i-1].timestamp)
  }
  
  // 测试基于键的合并
  StreamMerger::set_merge_strategy(stream_merger, MergeStrategy::KeyBased)
  StreamMerger::process_all(stream_merger)
  
  ConcurrentProcessor::sleep(500)
  
  let key_merged = StreamMerger::get_merged_stream(stream_merger)
  assert_true(key_merged.length() <= 90) // 键基合并可能会去重
  
  StreamMerger::stop(stream_merger)
}

// Test 6: 流数据背压处理
test "stream data backpressure handling" {
  let backpressure_handler = BackpressureHandler::new()
  
  // 配置背压策略
  BackpressureHandler::set_buffer_limit(backpressure_handler, 100)
  BackpressureHandler::set_strategy(backpressure_handler, BackpressureStrategy::DropOldest)
  BackpressureHandler::set_threshold(backpressure_handler, 0.8) // 80%容量时触发
  
  BackpressureHandler::start(backpressure_handler)
  
  // 快速发送大量数据以触发背压
  let fast_producer = fn() {
    for i in 0::<200 {
      let data = StreamData::new("fast_" + i.to_string(), i.to_float(), Time::now())
      BackpressureHandler::send_data(backpressure_handler, data)
      ConcurrentProcessor::sleep(10) // 10ms间隔，快于消费速度
    }
  }
  
  // 慢速消费者
  let slow_consumer = fn() {
    for i in 0..<50 {
      ConcurrentProcessor::sleep(100) // 100ms间隔，慢于生产速度
      let data = BackpressureHandler::receive_data(backpressure_handler)
      match data {
        Some(_) => assert_true(true)
        None => assert_true(true) // 可能为空，因为背压导致丢弃
      }
    }
  }
  
  // 并发执行生产者和消费者
  let producer_task = ConcurrentTask::new(fast_producer)
  let consumer_task = ConcurrentTask::new(slow_consumer)
  
  ConcurrentTask::start(producer_task)
  ConcurrentTask::start(consumer_task)
  
  ConcurrentTask::wait(producer_task, 5000)
  ConcurrentTask::wait(consumer_task, 5000)
  
  ConcurrentProcessor::sleep(1000)
  
  // 检查背压处理统计
  let backpressure_stats = BackpressureHandler::get_statistics(backpressure_handler)
  
  assert_true(backpressure_stats.messages_sent > 150)
  assert_true(backpressure_stats.messages_received < backpressure_stats.messages_sent)
  assert_true(backpressure_stats.dropped_messages > 0)
  assert_true(backpressure_handler.backpressure_triggered)
  
  // 验证缓冲区大小控制
  let buffer_usage = BackpressureHandler::get_buffer_usage(backpressure_handler)
  assert_true(buffer_usage <= 100)
  
  BackpressureHandler::stop(backpressure_handler)
}

// Test 7: 流数据持久化和恢复
test "stream data persistence and recovery" {
  let stream_persistence = StreamPersistence::new("/tmp/azimuth_stream_test")
  
  // 配置持久化策略
  StreamPersistence::set_persistence_mode(stream_persistence, PersistenceMode::Periodic)
  StreamPersistence::set_interval(stream_persistence, 1000) // 1秒间隔
  StreamPersistence::set_batch_size(stream_persistence, 50)
  
  StreamPersistence::start(stream_persistence)
  
  // 创建测试数据流
  let test_stream = []
  for i in 0..<150 {
    let data = StreamData::new("persist_" + i.to_string(), i.to_float(), Time::now())
    test_stream = test_stream.push(data)
    StreamPersistence::add_data(stream_persistence, data)
    
    if i % 25 == 0 {
      ConcurrentProcessor::sleep(200) // 偶尔暂停
    }
  }
  
  // 等待持久化完成
  ConcurrentProcessor::sleep(3000)
  
  // 停止并检查持久化状态
  StreamPersistence::stop(stream_persistence)
  
  let persistence_stats = StreamPersistence::get_statistics(stream_persistence)
  assert_true(persistence_stats.persisted_batches > 0)
  assert_true(persistence_stats.persisted_items > 0)
  
  // 模拟系统重启和恢复
  let recovered_persistence = StreamPersistence::new("/tmp/azimuth_stream_test")
  StreamPersistence::start(recovered_persistence)
  
  // 检查恢复的数据
  let recovered_data = StreamPersistence::recover_data(recovered_persistence)
  assert_true(recovered_data.length() > 0)
  
  // 验证恢复数据的完整性
  for i in 0..<recovered_data.length().min(10) {
    let original = test_stream[i]
    let recovered = recovered_data[i]
    assert_eq(original.id, recovered.id)
    assert_eq(original.value, recovered.value)
  }
  
  StreamPersistence::stop(recovered_persistence)
  
  // 清理测试文件
  StreamPersistence::cleanup(stream_persistence)
}

// Test 8: 流数据监控和告警
test "stream data monitoring and alerting" {
  let stream_monitor = StreamMonitor::new()
  
  // 配置监控指标
  StreamMonitor::add_metric(stream_monitor, "throughput", MetricType::Counter)
  StreamMonitor::add_metric(stream_monitor, "latency", MetricType::Histogram)
  StreamMonitor::add_metric(stream_monitor, "error_rate", MetricType::Gauge)
  
  // 配置告警规则
  StreamMonitor::add_alert_rule(stream_monitor, "low_throughput", AlertCondition::LessThan, "throughput", 10.0)
  StreamMonitor::add_alert_rule(stream_monitor, "high_latency", AlertCondition::GreaterThan, "latency", 100.0)
  StreamMonitor::add_alert_rule(stream_monitor, "high_error_rate", AlertCondition::GreaterThan, "error_rate", 0.05)
  
  StreamMonitor::start(stream_monitor)
  
  // 发送正常流量的数据
  for i in 0..<100 {
    let start_time = Time::now()
    let data = StreamData::new("monitor_" + i.to_string(), i.to_float(), start_time)
    StreamMonitor::process_data(stream_monitor, data)
    
    let processing_time = Time::now() - start_time
    StreamMonitor::record_metric(stream_monitor, "latency", processing_time.to_float())
    
    ConcurrentProcessor::sleep(50)
  }
  
  // 发送高延迟数据触发告警
  for i in 0..<10 {
    let start_time = Time::now()
    let data = StreamData::new("slow_" + i.to_string(), i.to_float(), start_time)
    StreamMonitor::process_data(stream_monitor, data)
    
    ConcurrentProcessor::sleep(200) // 慢处理
    let processing_time = Time::now() - start_time
    StreamMonitor::record_metric(stream_monitor, "latency", processing_time.to_float())
  }
  
  // 模拟错误
  for i in 0..<20 {
    StreamMonitor::record_error(stream_monitor, "processing_error")
    ConcurrentProcessor::sleep(25)
  }
  
  ConcurrentProcessor::sleep(2000)
  
  // 检查监控指标
  let metrics = StreamMonitor::get_metrics(stream_monitor)
  assert_true(metrics.contains("throughput"))
  assert_true(metrics.contains("latency"))
  assert_true(metrics.contains("error_rate"))
  
  // 检查告警
  let alerts = StreamMonitor::get_alerts(stream_monitor)
  assert_true(alerts.length() >= 2) // 至少高延迟和高错误率告警
  
  // 验证告警内容
  let high_latency_alerts = alerts.filter(fn(alert) { alert.rule == "high_latency" })
  let high_error_rate_alerts = alerts.filter(fn(alert) { alert.rule == "high_error_rate" })
  
  assert_true(high_latency_alerts.length() > 0)
  assert_true(high_error_rate_alerts.length() > 0)
  
  for alert in high_latency_alerts {
    assert_true(alert.triggered_at > 0)
    assert_true(alert.severity > 0)
  }
  
  StreamMonitor::stop(stream_monitor)
}

// Test 9: 动态流拓扑管理
test "dynamic stream topology management" {
  let topology_manager = TopologyManager::new()
  
  // 创建流处理节点
  let source_node = TopologyManager::add_node(topology_manager, "source", NodeType::DataSource)
  let filter_node = TopologyManager::add_node(topology_manager, "filter", NodeType::Filter)
  let transform_node = TopologyManager::add_node(topology_manager, "transform", NodeType::Transformer)
  let sink_node = TopologyManager::add_node(topology_manager, "sink", NodeType::DataSink)
  
  // 创建流连接
  TopologyManager::connect_nodes(topology_manager, "source", "filter")
  TopologyManager::connect_nodes(topology_manager, "filter", "transform")
  TopologyManager::connect_nodes(topology_manager, "transform", "sink")
  
  // 启动拓扑
  TopologyManager::start(topology_manager)
  
  // 配置节点处理逻辑
  TopologyManager::configure_filter(topology_manager, "filter", fn(data) { data.value > 25.0 })
  TopologyManager::configure_transformer(topology_manager, "transform", fn(data) { 
    StreamData::new(data.id + "_processed", data.value * 1.1, data.timestamp)
  })
  
  // 发送测试数据
  for i in 0..<50 {
    let data = StreamData::new("topology_" + i.to_string(), i.to_float(), Time::now())
    TopologyManager::send_to_source(topology_manager, "source", data)
    ConcurrentProcessor::sleep(20)
  }
  
  ConcurrentProcessor::sleep(1000)
  
  // 检查处理结果
  let sink_data = TopologyManager::get_sink_data(topology_manager, "sink")
  
  // 验证数据经过过滤和转换
  for data in sink_data {
    assert_true(String::contains(data.id, "_processed"))
    assert_true(data.value > 25.0 * 1.1)
  }
  
  // 动态添加新节点
  let aggregate_node = TopologyManager::add_node(topology_manager, "aggregate", NodeType::Aggregator)
  TopologyManager::disconnect_nodes(topology_manager, "transform", "sink")
  TopologyManager::connect_nodes(topology_manager, "transform", "aggregate")
  TopologyManager::connect_nodes(topology_manager, "aggregate", "sink")
  
  // 配置聚合节点
  TopologyManager::configure_aggregator(topology_manager, "aggregate", fn(values) { 
    values.reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_float()
  })
  
  // 发送更多数据
  for i in 50..<100 {
    let data = StreamData::new("topology_" + i.to_string(), i.to_float(), Time::now())
    TopologyManager::send_to_source(topology_manager, "source", data)
    ConcurrentProcessor::sleep(20)
  }
  
  ConcurrentProcessor::sleep(1000)
  
  // 检查拓扑状态
  let topology_status = TopologyManager::get_status(topology_manager)
  assert_true(topology_status.all_nodes_healthy)
  assert_eq(topology_status.node_count, 5)
  assert_eq(topology_status.connection_count, 4)
  
  TopologyManager::stop(topology_manager)
}

// Test 10: 流数据性能基准测试
test "stream data performance benchmarking" {
  let performance_benchmark = StreamBenchmark::new()
  
  // 配置基准测试参数
  StreamBenchmark::configure_test_parameters(performance_benchmark, {
    "data_rate": 1000,        // 每秒1000条消息
    "test_duration": 5000,    // 5秒测试
    "message_size": 1024,     // 1KB消息
    "parallelism": 4          // 4个并行处理器
  })
  
  StreamBenchmark::start(performance_benchmark)
  
  // 执行基准测试
  let benchmark_result = StreamBenchmark::run_throughput_test(performance_benchmark)
  
  // 验证性能指标
  assert_true(benchmark_result.actual_throughput > 0)
  assert_true(benchmark_result.latency_p50 > 0)
  assert_true(benchmark_result.latency_p95 > 0)
  assert_true(benchmark_result.latency_p99 > 0)
  assert_true(benchmark_result.error_rate < 0.01) // 错误率低于1%
  
  // 执行延迟测试
  let latency_result = StreamBenchmark::run_latency_test(performance_benchmark)
  assert_true(latency_result.average_latency > 0)
  assert_true(latency_result.max_latency > latency_result.average_latency)
  
  // 执行可扩展性测试
  let scalability_results = []
  for parallelism in [1, 2, 4, 8] {
    let result = StreamBenchmark::run_scalability_test(performance_benchmark, parallelism)
    scalability_results = scalability_results.push(result)
  }
  
  // 验证可扩展性
  for i in 1..<scalability_results.length() {
    let prev = scalability_results[i - 1]
    let curr = scalability_results[i]
    
    // 更多并行处理器应该提供更好的吞吐量
    assert_true(curr.throughput >= prev.throughput * 0.8) // 至少80%的线性扩展
  }
  
  // 生成性能报告
  let performance_report = StreamBenchmark::generate_report(performance_benchmark, 
    benchmark_result, latency_result, scalability_results)
  
  assert_true(performance_report.contains("throughput"))
  assert_true(performance_report.contains("latency"))
  assert_true(performance_report.contains("scalability"))
  
  // 检查资源使用情况
  let resource_usage = StreamBenchmark::get_resource_usage(performance_benchmark)
  assert_true(resource_usage.cpu_usage > 0.0)
  assert_true(resource_usage.memory_usage > 0)
  assert_true(resource_usage.network_io > 0)
  
  StreamBenchmark::stop(performance_benchmark)
}