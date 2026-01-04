// Azimuth 高级实时流处理测试用例
// 专注于验证遥测系统的实时流处理能力

// 测试1: 高吞吐量实时数据流处理
test "高吞吐量实时数据流处理测试" {
  // 创建实时流处理器
  let stream_processor = RealtimeStreamProcessor::new()
  stream_processor.set_parallelism(4) // 4个并行处理线程
  stream_processor.set_buffer_size(10000) // 10K缓冲区
  
  // 创建数据源
  let telemetry_source = TelemetryDataSource::new("high.volume.source")
  telemetry_source.set_generation_rate(10000) // 每秒10K事件
  
  // 创建流处理拓扑
  let processing_topology = StreamTopology::new()
  
  // 添加处理阶段
  StreamTopology::add_source(processing_topology, telemetry_source)
  
  let filtering_stage = StreamTopology::add_stage(processing_topology, "filtering", fn(event) {
    // 过滤掉无效事件
    event.has_attribute("service.name") && 
    event.has_attribute("operation.name") &&
    event.timestamp > 0
  })
  
  let enrichment_stage = StreamTopology::add_stage(processing_topology, "enrichment", fn(event) {
    // 丰富事件数据
    let enriched_event = event.clone()
    enriched_event.set_attribute("processed.timestamp", Timestamp::now().to_string())
    enriched_event.set_attribute("processing.node", StringValue("node-" + ThreadId::current().to_string()))
    enriched_event
  })
  
  let aggregation_stage = StreamTopology::add_stage(processing_topology, "aggregation", fn(events) {
    // 聚合事件
    let service_metrics = Map::new()
    
    for event in events {
      let service_name = event.get_attribute("service.name")
      let current_count = service_metrics.get(service_name).or(0)
      service_metrics.put(service_name, current_count + 1)
    }
    
    return service_metrics
  })
  
  // 创建数据汇
  let telemetry_sink = TelemetryDataSink::new("high.volume.sink")
  StreamTopology::add_sink(processing_topology, telemetry_sink)
  
  // 启动流处理
  let processing_job = stream_processor.start_processing(processing_topology)
  
  // 生成测试数据
  let data_generator = StreamTestDataGenerator::new()
  data_generator.set_services(["api.gateway", "user.service", "payment.service", "notification.service"])
  data_generator.set_operations(["request.processed", "database.query", "cache.hit", "authentication.success"])
  data_generator.set_event_count(50000) // 50K事件
  
  let test_start_time = Timestamp::now()
  
  // 生成并流式传输数据
  let stream_results = data_generator.generate_and_stream(telemetry_source)
  
  let test_end_time = Timestamp::now()
  let total_duration_ms = test_end_time.to_millis() - test_start_time.to_millis()
  
  // 等待处理完成
  stream_processor.wait_for_completion(processing_job, Duration::from_seconds(30))
  
  // 验证处理结果
  let processing_metrics = stream_processor.get_processing_metrics(processing_job)
  
  assert_true(processing_metrics.events_received >= 50000)
  assert_true(processing_metrics.events_processed >= 45000) // 允许一些过滤损失
  assert_true(processing_metrics.processing_rate_events_per_sec > 1000) // 处理速率应大于1K events/sec
  assert_true(processing_metrics.average_latency_ms < 100) // 平均延迟应小于100ms
  assert_true(processing_metrics.p99_latency_ms < 500) // P99延迟应小于500ms
  
  // 验证数据完整性
  let sink_data = telemetry_sink.get_all_data()
  assert_true(sink_data.length() >= 45000)
  
  // 验证聚合结果
  let aggregated_metrics = telemetry_sink.get_aggregated_metrics()
  assert_true(aggregated_metrics.size() > 0)
  
  for (service_name, count) in aggregated_metrics {
    assert_true(count > 0)
    assert_true(service_name.length() > 0)
  }
  
  // 测试背压处理
  let backpressure_tester = StreamBackpressureTester::new(stream_processor)
  let backpressure_results = backpressure_tester.test_backpressure_handling(processing_topology, 100000) // 100K事件
  
  assert_true(backpressure_results.backpressure_applied)
  assert_true(backpressure_results.no_data_loss)
  assert_true(backpressure_results.system_remained_stable)
}

// 测试2: 实时窗口聚合和计算
test "实时窗口聚合和计算测试" {
  // 创建窗口化流处理器
  let window_processor = WindowedStreamProcessor::new()
  
  // 定义时间窗口
  let tumbling_window = TumblingWindow::new(Duration::from_seconds(5)) // 5秒滚动窗口
  let sliding_window = SlidingWindow::new(Duration::from_seconds(10), Duration::from_seconds(2)) // 10秒窗口，2秒滑动
  let session_window = SessionWindow::new(Duration::from_minutes(5)) // 5秒超时会话窗口
  
  // 创建数据源
  let event_source = TelemetryEventSource::new("window.test.source")
  
  // 创建窗口聚合器
  let count_aggregator = CountAggregator::new()
  let sum_aggregator = SumAggregator::new("response.time")
  let average_aggregator = AverageAggregator::new("cpu.usage")
  let percentile_aggregator = PercentileAggregator::new("latency", [50.0, 95.0, 99.0])
  
  // 设置窗口处理
  window_processor.add_window(tumbling_window, count_aggregator)
  window_processor.add_window(sliding_window, sum_aggregator)
  window_processor.add_window(session_window, average_aggregator)
  window_processor.add_window(tumbling_window, percentile_aggregator)
  
  // 启动窗口处理
  let window_job = window_processor.start_processing(event_source)
  
  // 生成带有时间戳的测试数据
  let timestamped_generator = TimestampedEventGenerator::new()
  timestamped_generator.set_event_rate(100) // 每秒100事件
  timestamped_generator.set_duration(Duration::from_seconds(30)) // 30秒测试
  timestamped_generator.set_value_range("response.time", 10.0, 1000.0)
  timestamped_generator.set_value_range("cpu.usage", 0.0, 100.0)
  timestamped_generator.set_value_range("latency", 1.0, 500.0)
  
  // 生成时间序列数据
  let generation_start = Timestamp::now()
  timestamped_generator.generate_events(event_source)
  let generation_end = Timestamp::now()
  
  // 等待窗口处理完成
  Thread::sleep(Duration::from_seconds(10)) // 等待窗口关闭
  
  // 获取窗口结果
  let tumbling_results = window_processor.get_window_results(tumbling_window)
  let sliding_results = window_processor.get_window_results(sliding_window)
  let session_results = window_processor.get_window_results(session_window)
  
  // 验证滚动窗口结果
  assert_true(tumbling_results.length() >= 5) // 至少5个5秒窗口
  for window_result in tumbling_results {
    assert_true(window_result.start_time < window_result.end_time)
    assert_true(window_result.event_count > 0)
    assert_true(window_result.window_duration_ms >= 4900 && window_result.window_duration_ms <= 5100) // 允许误差
    
    // 验证百分位数聚合
    if window_result.has_aggregation("latency.percentiles") {
      let percentiles = window_result.get_aggregation("latency.percentiles")
      assert_true(percentiles.contains("p50"))
      assert_true(percentiles.contains("p95"))
      assert_true(percentiles.contains("p99"))
      
      let p50 = percentiles.get("p50")
      let p95 = percentiles.get("p95")
      let p99 = percentiles.get("p99")
      
      assert_true(p50 <= p95 && p95 <= p99) // 百分位数应该递增
    }
  }
  
  // 验证滑动窗口结果
  assert_true(sliding_results.length() >= 10) // 至少10个滑动窗口
  for window_result in sliding_results {
    assert_true(window_result.start_time < window_result.end_time)
    assert_true(window_result.event_count > 0)
    assert_true(window_result.window_duration_ms >= 9800 && window_result.window_duration_ms <= 10200) // 允许误差
    
    // 验证求和聚合
    if window_result.has_aggregation("response.time.sum") {
      let sum = window_result.get_aggregation("response.time.sum")
      assert_true(sum > 0.0)
    }
  }
  
  // 验证会话窗口结果
  assert_true(session_results.length() >= 1) // 至少1个会话
  for window_result in session_results {
    assert_true(window_result.start_time < window_result.end_time)
    assert_true(window_result.event_count > 0)
    
    // 验证平均值聚合
    if window_result.has_aggregation("cpu.usage.average")) {
      let average = window_result.get_aggregation("cpu.usage.average")
      assert_true(average >= 0.0 && average <= 100.0)
    }
  }
  
  // 测试延迟数据处理
  let late_data_handler = LateDataHandler::new(window_processor)
  late_data_handler.set_allowed_lateness(Duration::from_seconds(10)) // 允许10秒延迟
  
  // 生成延迟数据
  let delayed_generator = DelayedEventGenerator::new()
  delayed_generator.set_base_time(generation_start)
  delayed_generator.set_delays([0, 2, 5, 8, 12, 15]) // 不同延迟的秒数
  delayed_generator.generate_delayed_events(event_source)
  
  // 等待延迟数据处理
  Thread::sleep(Duration::from_seconds(20))
  
  // 验证延迟数据处理
  let late_data_results = late_data_handler.get_late_data_results()
  assert_true(late_data_results.total_late_events > 0)
  assert_true(late_data_results.processed_late_events > 0)
  assert_true(late_data_results.dropped_late_events >= 0)
  
  // 测试水印机制
  let watermark_generator = WatermarkGenerator::new(window_processor)
  watermark_generator.set_watermark_strategy("event_time")
  watermark_generator.set_max_out_of_orderness(Duration::from_seconds(5))
  
  let watermark_results = watermark_generator.get_watermark_progression()
  assert_true(watermark_results.watermarks_generated > 0)
  assert_true(watermark_results.watermark_monotonic) // 水印应该单调递增
}

// 测试3: 实时异常检测和告警
test "实时异常检测和告警测试" {
  // 创建异常检测系统
  let anomaly_detector = RealtimeAnomalyDetector::new()
  
  // 配置异常检测规则
  let latency_anomaly_rule = StatisticalAnomalyRule::new("response.time", "zscore")
  StatisticalAnomalyRule::set_threshold(latency_anomaly_rule, 3.0) // 3个标准差
  StatisticalAnomalyRule::set_window_size(latency_anomaly_rule, 100) // 100个样本窗口
  
  let error_rate_rule = ThresholdAnomalyRule::new("error.rate", "percentage")
  ThresholdAnomalyRule::set_threshold(error_rate_rule, 5.0) // 5%错误率阈值
  ThresholdAnomalyRule::set_evaluation_window(error_rate_rule, Duration::from_minutes(1))
  
  let pattern_anomaly_rule = PatternAnomalyRule::new("access.pattern", "sequence")
  PatternAnomalyRule::set_normal_pattern(pattern_anomaly_rule, ["login", "browse", "add_to_cart", "checkout"])
  PatternAnomalyRule::set_anomaly_threshold(pattern_anomaly_rule, 0.7) // 70%偏离阈值
  
  // 注册检测规则
  anomaly_detector.add_rule(latency_anomaly_rule)
  anomaly_detector.add_rule(error_rate_rule)
  anomaly_detector.add_rule(pattern_anomaly_rule)
  
  // 创建告警管理器
  let alert_manager = RealtimeAlertManager::new()
  
  // 配置告警策略
  let critical_alert_policy = AlertPolicy::new("critical")
  AlertPolicy::set_severity_threshold(critical_alert_policy, "critical")
  AlertPolicy::set_notification_channels(critical_alert_policy, ["email", "slack", "pagerduty"])
  AlertPolicy::set_cooldown_period(critical_alert_policy, Duration::from_minutes(5))
  
  let warning_alert_policy = AlertPolicy::new("warning")
  AlertPolicy::set_severity_threshold(warning_alert_policy, "warning")
  AlertPolicy::set_notification_channels(warning_alert_policy, ["email", "slack"])
  AlertPolicy::set_cooldown_period(warning_alert_policy, Duration::from_minutes(15))
  
  alert_manager.add_policy(critical_alert_policy)
  alert_manager.add_policy(warning_alert_policy)
  
  // 创建实时数据流
  let anomaly_source = AnomalyTestDataSource::new()
  
  // 启动异常检测
  let detection_job = anomaly_detector.start_detection(anomaly_source, alert_manager)
  
  // 生成正常数据
  let normal_generator = NormalEventGenerator::new()
  normal_generator.set_metrics([
    ("response.time", 50.0, 10.0), // 均值50ms，标准差10ms
    ("error.rate", 1.0, 0.5),     // 均值1%，标准差0.5%
    ("cpu.usage", 45.0, 15.0)     // 均值45%，标准差15%
  ])
  normal_generator.set_event_rate(50) // 每秒50事件
  normal_generator.set_duration(Duration::from_seconds(30))
  
  normal_generator.generate_events(anomaly_source)
  
  // 验证正常期间无异常
  let normal_period_anomalies = anomaly_detector.get_detected_anomalies(
    Timestamp::now() - Duration::from_seconds(30),
    Timestamp::now()
  )
  assert_true(normal_period_anomalies.length() == 0)
  
  // 生成异常数据 - 高延迟
  let high_latency_generator = AnomalyEventGenerator::new()
  high_latency_generator.set_anomaly_type("latency_spike")
  high_latency_generator.set_anomaly_value("response.time", 500.0) // 500ms延迟
  high_latency_generator.set_anomaly_duration(Duration::from_seconds(10))
  high_latency_generator.set_event_rate(50)
  
  high_latency_generator.generate_events(anomaly_source)
  
  // 生成异常数据 - 高错误率
  let high_error_rate_generator = AnomalyEventGenerator::new()
  high_error_rate_generator.set_anomaly_type("error_rate_spike")
  high_error_rate_generator.set_anomaly_value("error.rate", 15.0) // 15%错误率
  high_error_rate_generator.set_anomaly_duration(Duration::from_seconds(10))
  high_error_rate_generator.set_event_rate(50)
  
  high_error_rate_generator.generate_events(anomaly_source)
  
  // 生成异常数据 - 异常访问模式
  let pattern_anomaly_generator = AnomalyEventGenerator::new()
  pattern_anomaly_generator.set_anomaly_type("pattern_deviation")
  pattern_anomaly_generator.set_anomaly_sequence(["login", "login", "login", "admin_access"])
  pattern_anomaly_generator.set_anomaly_duration(Duration::from_seconds(10))
  pattern_anomaly_generator.set_event_rate(20)
  
  pattern_anomaly_generator.generate_events(anomaly_source)
  
  // 等待异常检测
  Thread::sleep(Duration::from_seconds(15))
  
  // 验证异常检测结果
  let detected_anomalies = anomaly_detector.get_detected_anomalies(
    Timestamp::now() - Duration::from_seconds(60),
    Timestamp::now()
  )
  
  assert_true(detected_anomalies.length() >= 3) // 应该检测到至少3个异常
  
  // 验证特定异常类型
  let latency_anomalies = detected_anomalies.filter(fn(a) { a.anomaly_type == "latency_spike" })
  let error_rate_anomalies = detected_anomalies.filter(fn(a) { a.anomaly_type == "error_rate_spike" })
  let pattern_anomalies = detected_anomalies.filter(fn(a) { a.anomaly_type == "pattern_deviation" })
  
  assert_true(latency_anomalies.length() >= 1)
  assert_true(error_rate_anomalies.length() >= 1)
  assert_true(pattern_anomalies.length() >= 1)
  
  // 验证异常详情
  for anomaly in detected_anomalies {
    assert_true(anomaly.timestamp > 0)
    assert_true(anomaly.severity == "warning" || anomaly.severity == "critical")
    assert_true(anomaly.confidence_score >= 0.0 && anomaly.confidence_score <= 1.0)
    assert_true(anomaly.description.length() > 0)
  }
  
  // 验证告警生成
  let generated_alerts = alert_manager.get_generated_alerts(
    Timestamp::now() - Duration::from_seconds(60),
    Timestamp::now()
  )
  
  assert_true(generated_alerts.length() >= 3) // 应该生成至少3个告警
  
  // 验证告警详情
  for alert in generated_alerts {
    assert_true(alert.id.length() > 0)
    assert_true(alert.severity == "warning" || alert.severity == "critical")
    assert_true(alert.timestamp > 0)
    assert_true(alert.title.length() > 0)
    assert_true(alert.description.length() > 0)
    assert_true(alert.notification_channels.length() > 0)
  }
  
  // 测试告警去重
  let deduplicator = AlertDeduplicator::new(alert_manager)
  deduplicator.set_deduplication_window(Duration::from_minutes(10))
  deduplicator.set_deduplication_keys(["anomaly_type", "service_name"])
  
  // 生成重复异常
  let duplicate_generator = AnomalyEventGenerator::new()
  duplicate_generator.set_anomaly_type("latency_spike")
  duplicate_generator.set_anomaly_value("response.time", 500.0)
  duplicate_generator.set_anomaly_duration(Duration::from_seconds(5))
  duplicate_generator.set_event_rate(50)
  
  duplicate_generator.generate_events(anomaly_source)
  Thread::sleep(Duration::from_seconds(10))
  duplicate_generator.generate_events(anomaly_source) // 重复异常
  
  Thread::sleep(Duration::from_seconds(10))
  
  // 验证去重效果
  let deduplicated_alerts = alert_manager.get_generated_alerts(
    Timestamp::now() - Duration::from_seconds(30),
    Timestamp::now()
  )
  
  let latency_alerts = deduplicated_alerts.filter(fn(a) { a.title.contains("latency") })
  assert_true(latency_alerts.length() <= 2) // 去重后应该减少告警数量
  
  // 测试异常检测性能
  let performance_analyzer = AnomalyDetectionPerformanceAnalyzer::new(anomaly_detector)
  let performance_metrics = performance_analyzer.analyze_performance(
    Timestamp::now() - Duration::from_minutes(5),
    Timestamp::now()
  )
  
  assert_true(performance_metrics.detection_latency_ms < 1000) // 检测延迟应小于1秒
  assert_true(performance_metrics.processing_rate_events_per_sec > 40) // 处理速率应大于40 events/sec
  assert_true(performance_metrics.memory_usage_mb < 100) // 内存使用应小于100MB
}

// 测试4: 实时流状态管理和恢复
test "实时流状态管理和恢复测试" {
  // 创建状态管理器
  let state_manager = StreamStateManager::new()
  state_manager.set_state_backend(StateBackend::RocksDB)
  state_manager.set_checkpoint_interval(Duration::from_seconds(10))
  state_manager.set_checkpoint_path("/tmp/stream_checkpoints")
  
  // 创建有状态的流处理拓扑
  let stateful_topology = StatefulStreamTopology::new()
  
  // 创建状态源
  let stateful_source = StatefulDataSource::new("stateful.test.source")
  
  // 添加有状态操作
  let counting_state = CountingState::new("event_counter")
  let counting_stage = StatefulStreamTopology::add_stateful_stage(
    stateful_topology, 
    "counting", 
    counting_state,
    fn(event, state) {
      state.increment_counter(event.get_service_name())
      return event
    }
  )
  
  let window_state = WindowState::new("time_windows", Duration::from_minutes(1))
  let window_stage = StatefulStreamTopology::add_stateful_stage(
    stateful_topology,
    "windowing",
    window_state,
    fn(event, state) {
      state.add_to_window(event.timestamp, event.get_metric_value("response.time"))
      return event
    }
  )
  
  let session_state = SessionState::new("user_sessions", Duration::from_minutes(5))
  let session_stage = StatefulStreamTopology::add_stateful_stage(
    stateful_topology,
    "sessionization",
    session_state,
    fn(event, state) {
      let user_id = event.get_attribute("user.id")
      state.update_session(user_id, event.timestamp, event.get_operation_name())
      return event
    }
  )
  
  // 添加状态汇
  let stateful_sink = StatefulDataSink::new("stateful.test.sink")
  StatefulStreamTopology::add_sink(stateful_topology, stateful_sink)
  
  // 启动状态流处理
  let stateful_job = state_manager.start_processing(stateful_topology)
  
  // 生成测试数据
  let stateful_generator = StatefulEventGenerator::new()
  stateful_generator.set_services(["api.gateway", "user.service", "order.service"])
  stateful_generator.set_users(["user-001", "user-002", "user-003"])
  stateful_generator.set_operations(["login", "browse", "add_to_cart", "checkout"])
  stateful_generator.set_event_rate(100)
  stateful_generator.set_duration(Duration::from_minutes(2))
  
  stateful_generator.generate_events(stateful_source)
  
  // 等待初始处理和检查点
  Thread::sleep(Duration::from_seconds(15))
  
  // 验证状态创建
  let counting_snapshot = state_manager.get_state_snapshot(counting_state)
  assert_true(counting_snapshot.size() > 0)
  
  let window_snapshot = state_manager.get_state_snapshot(window_state)
  assert_true(window_snapshot.size() > 0)
  
  let session_snapshot = state_manager.get_state_snapshot(session_state)
  assert_true(session_snapshot.size() > 0)
  
  // 验证检查点创建
  let checkpoints = state_manager.get_checkpoints(stateful_job)
  assert_true(checkpoints.length() >= 1)
  
  let latest_checkpoint = checkpoints[checkpoints.length() - 1]
  assert_true(latest_checkpoint.timestamp > 0)
  assert_true(latest_checkpoint.size_bytes > 0)
  assert_true(latest_checkpoint.completed)
  
  // 模拟故障和恢复
  let failure_simulator = StreamFailureSimulator::new(state_manager)
  
  // 注入故障
  let failure_result = failure_simulator.inject_failure(stateful_job, "process_crash")
  assert_true(failure_result.failure_injected)
  
  // 等待故障检测
  Thread::sleep(Duration::from_seconds(5))
  
  // 验证故障检测
  let failure_status = state_manager.get_job_status(stateful_job)
  assert_true(failure_status.failed)
  assert_true(failure_status.failure_reason.length() > 0)
  
  // 执行恢复
  let recovery_result = state_manager.recover_job(stateful_job, latest_checkpoint)
  assert_true(recovery_result.recovery_started)
  
  // 等待恢复完成
  Thread::sleep(Duration::from_seconds(10))
  
  // 验证恢复状态
  let recovery_status = state_manager.get_job_status(stateful_job)
  assert_true(recovery_status.running)
  assert_true(recovery_status.recovery_completed)
  
  // 验证状态恢复
  let recovered_counting_snapshot = state_manager.get_state_snapshot(counting_state)
  assert_eq(recovered_counting_snapshot.size(), counting_snapshot.size())
  
  // 验证数据一致性
  let pre_failure_events = stateful_sink.get_event_count_before(Timestamp::now() - Duration::from_seconds(20))
  let post_recovery_events = stateful_sink.get_event_count_after(Timestamp::now() - Duration::from_seconds(10))
  
  assert_true(post_recovery_events >= pre_failure_events) // 恢复后应该继续处理
  
  // 继续生成数据
  stateful_generator.set_duration(Duration::from_minutes(1))
  stateful_generator.generate_events(stateful_source)
  
  // 等待处理完成
  Thread::sleep(Duration::from_seconds(15))
  
  // 验证状态更新
  let final_counting_snapshot = state_manager.get_state_snapshot(counting_state)
  assert_true(final_counting_snapshot.size() > recovered_counting_snapshot.size())
  
  // 测试状态迁移
  let state_migrator = StateMigrator::new(state_manager)
  
  // 创建新作业
  let new_job_id = "migrated-job-001"
  let migration_result = state_migrator.migrate_state(stateful_job, new_job_id, [
    counting_state, window_state, session_state
  ])
  
  assert_true(migration_result.success)
  assert_true(migration_result.migrated_states.contains(counting_state))
  assert_true(migration_result.migrated_states.contains(window_state))
  assert_true(migration_result.migrated_states.contains(session_state))
  
  // 验证迁移后的状态
  let migrated_counting_snapshot = state_manager.get_state_snapshot_for_job(counting_state, new_job_id)
  assert_eq(migrated_counting_snapshot.size(), final_counting_snapshot.size())
  
  // 测试状态清理
  let state_cleaner = StateCleaner::new(state_manager)
  state_cleaner.set_retention_policy(Duration::from_hours(24)) // 保留24小时的状态
  
  // 清理旧状态
  let cleanup_result = state_cleaner.cleanup_old_state()
  assert_true(cleanup_result.states_cleaned >= 0)
  assert_true(cleanup_result.space_freed_bytes >= 0)
  
  // 测试状态监控
  let state_monitor = StateMonitor::new(state_manager)
  let state_metrics = state_monitor.get_state_metrics()
  
  assert_true(state_metrics.total_states >= 3)
  assert_true(state_metrics.total_state_size_bytes > 0)
  assert_true(state_metrics.checkpoint_count >= 1)
  assert_true(state_metrics.last_checkpoint_timestamp > 0)
}

// 测试5: 实时流可扩展性和弹性
test "实时流可扩展性和弹性测试" {
  // 创建弹性流处理器
  let elastic_processor = ElasticStreamProcessor::new()
  elastic_processor.set_auto_scaling(true)
  elastic_processor.set_min_parallelism(2)
  elastic_processor.set_max_parallelism(10)
  elastic_processor.set_scale_up_threshold(0.8) // 80% CPU使用率触发扩容
  elastic_processor.set_scale_down_threshold(0.3) // 30% CPU使用率触发缩容
  elastic_processor.set_scale_evaluation_interval(Duration::from_seconds(30))
  
  // 创建可扩展拓扑
  let elastic_topology = ElasticStreamTopology::new()
  
  // 创建数据源
  let elastic_source = ElasticDataSource::new("elastic.test.source")
  
  // 添加可扩展处理阶段
  let elastic_stage = ElasticStreamTopology::add_scalable_stage(
    elastic_topology,
    "elastic.processing",
    fn(event) {
      // CPU密集型处理
      let result = 0
      for i in 1..=1000 {
        result = result + (i * event.get_metric_value("cpu.intensity"))
      }
      event.set_attribute("processing.result", IntValue(result))
      return event
    }
  )
  
  // 添加数据汇
  let elastic_sink = ElasticDataSink::new("elastic.test.sink")
  ElasticStreamTopology::add_sink(elastic_topology, elastic_sink)
  
  // 启动弹性处理
  let elastic_job = elastic_processor.start_processing(elastic_topology)
  
  // 初始状态验证
  let initial_metrics = elastic_processor.get_scaling_metrics(elastic_job)
  assert_eq(initial_metrics.current_parallelism, 2) // 应该从最小并行度开始
  
  // 生成低负载
  let low_load_generator = ElasticLoadGenerator::new()
  low_load_generator.set_event_rate(50) // 低负载
  low_load_generator.set_cpu_intensity(1.0) // 低CPU强度
  low_load_generator.set_duration(Duration::from_minutes(2))
  
  low_load_generator.generate_events(elastic_source)
  
  // 等待负载处理
  Thread::sleep(Duration::from_minutes(1))
  
  // 验证低负载期间无扩容
  let low_load_metrics = elastic_processor.get_scaling_metrics(elastic_job)
  assert_eq(low_load_metrics.current_parallelism, 2) // 应该保持最小并行度
  assert_true(low_load_metrics.cpu_utilization < 0.5) // CPU使用率应该较低
  
  // 生成高负载
  let high_load_generator = ElasticLoadGenerator::new()
  high_load_generator.set_event_rate(500) // 高负载
  high_load_generator.set_cpu_intensity(10.0) // 高CPU强度
  high_load_generator.set_duration(Duration::from_minutes(3))
  
  high_load_generator.generate_events(elastic_source)
  
  // 等待扩容触发
  Thread::sleep(Duration::from_seconds(45))
  
  // 验证扩容
  let scaling_metrics = elastic_processor.get_scaling_metrics(elastic_job)
  assert_true(scaling_metrics.current_parallelism > 2) // 应该已经扩容
  assert_true(scaling_metrics.scaling_events.length() > 0)
  
  let last_scaling_event = scaling_metrics.scaling_events[scaling_metrics.scaling_events.length() - 1]
  assert_eq(last_scaling_event.direction, "scale_up")
  assert_true(last_scaling_event.reason.contains("cpu_threshold"))
  
  // 继续高负载
  Thread::sleep(Duration::from_minutes(2))
  
  // 验证最大并行度限制
  let max_parallelism_metrics = elastic_processor.get_scaling_metrics(elastic_job)
  assert_true(max_parallelism_metrics.current_parallelism <= 10) // 不应超过最大并行度
  
  // 停止高负载
  high_load_generator.stop_generation()
  
  // 生成中等负载
  let medium_load_generator = ElasticLoadGenerator::new()
  medium_load_generator.set_event_rate(200) // 中等负载
  medium_load_generator.set_cpu_intensity(5.0) // 中等CPU强度
  medium_load_generator.set_duration(Duration::from_minutes(3))
  
  medium_load_generator.generate_events(elastic_source)
  
  // 等待缩容触发
  Thread::sleep(Duration::from_minutes(4))
  
  // 验证缩容
  let scale_down_metrics = elastic_processor.get_scaling_metrics(elastic_job)
  assert_true(scale_down_metrics.current_parallelism < max_parallelism_metrics.current_parallelism) // 应该已经缩容
  
  let last_scale_down_event = scale_down_metrics.scaling_events[scale_down_metrics.scaling_events.length() - 1]
  assert_eq(last_scale_down_event.direction, "scale_down")
  assert_true(last_scale_down_event.reason.contains("cpu_threshold"))
  
  // 测试手动扩缩容
  let manual_scaling_result = elastic_processor.manual_scale(elastic_job, 5)
  assert_true(manual_scaling_result.success)
  assert_eq(manual_scaling_result.new_parallelism, 5)
  
  let manual_scaling_metrics = elastic_processor.get_scaling_metrics(elastic_job)
  assert_eq(manual_scaling_metrics.current_parallelism, 5)
  
  // 测试分区重平衡
  let rebalancer = PartitionRebalancer::new(elastic_processor)
  
  // 添加新分区
  let new_partitions = ["partition-3", "partition-4", "partition-5"]
  let rebalance_result = rebalancer.rebalance_partitions(elastic_job, new_partitions)
  
  assert_true(rebalance_result.success)
  assert_true(rebalance_result.rebalanced_partitions.contains("partition-3"))
  assert_true(rebalance_result.rebalanced_partitions.contains("partition-4"))
  assert_true(rebalance_result.rebalanced_partitions.contains("partition-5"))
  
  // 验证重平衡后的一致性
  let rebalance_metrics = elastic_processor.get_scaling_metrics(elastic_job)
  assert_true(rebalance_metrics.partition_count >= 5)
  assert_true(rebalance_metrics.rebalance_events.length() > 0)
  
  // 测试故障恢复和弹性
  let fault_tolerance_tester = FaultToleranceTester::new(elastic_processor)
  
  // 模拟工作节点故障
  let node_failure_result = fault_tolerance_tester.simulate_node_failure(elastic_job, "worker-node-3")
  assert_true(node_failure_result.failure_simulated)
  
  // 等待故障恢复
  Thread::sleep(Duration::from_seconds(30))
  
  // 验证故障恢复
  let recovery_metrics = elastic_processor.get_scaling_metrics(elastic_job)
  assert_true(recovery_metrics.failed_nodes.contains("worker-node-3"))
  assert_true(recovery_metrics.replacement_nodes.length() > 0)
  assert_true(recovery_metrics.current_parallelism >= 4) // 应该保持足够的并行度
  
  // 测试弹性性能
  let elasticity_analyzer = ElasticityAnalyzer::new(elastic_processor)
  let elasticity_report = elasticity_analyzer.analyze_elasticity(
    elastic_job,
    Timestamp::now() - Duration::from_minutes(10),
    Timestamp::now()
  )
  
  assert_true(elasticity_report.scale_up_events > 0)
  assert_true(elasticity_report.scale_down_events > 0)
  assert_true(elasticity_report.average_scale_up_time_seconds < 60) // 扩容时间应小于60秒
  assert_true(elasticity_report.average_scale_down_time_seconds < 120) // 缩容时间应小于120秒
  assert_true(elasticity_report.elasticity_efficiency_score > 0.7) // 弹性效率应大于70%
  
  // 测试资源利用率
  let resource_analyzer = ResourceUtilizationAnalyzer::new(elastic_processor)
  let utilization_report = resource_analyzer.analyze_utilization(elastic_job)
  
  assert_true(utilization_report.average_cpu_utilization > 0.2)
  assert_true(utilization_report.average_cpu_utilization < 0.9)
  assert_true(utilization_report.average_memory_utilization > 0.2)
  assert_true(utilization_report.average_memory_utilization < 0.9)
  assert_true(utilization_report.resource_efficiency_score > 0.6) // 资源效率应大于60%
}