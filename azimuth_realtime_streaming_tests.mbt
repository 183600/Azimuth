// Azimuth Realtime Streaming Tests
// 实时流处理测试用例 - 专注于遥测数据的实时流处理和分析能力

// Test 1: 基础流数据处理测试
test "basic stream data processing" {
  // 创建流处理器
  let stream_processor = StreamProcessor::new()
  
  // 创建测试数据源
  let data_source = MockDataSource::new()
  
  // 配置数据源
  let source_config = DataSourceConfig::new()
  DataSourceConfig::set_data_rate(source_config, 100) // 每秒100个事件
  DataSourceConfig::set_event_pattern(source_config, "telemetry")
  
  MockDataSource::configure(data_source, source_config)
  
  // 创建流处理管道
  let pipeline = StreamPipeline::new()
  
  // 添加处理阶段
  StreamPipeline::add_stage(pipeline, "validation", ValidationStage::new())
  StreamPipeline::add_stage(pipeline, "enrichment", EnrichmentStage::new())
  StreamPipeline::add_stage(pipeline, "aggregation", AggregationStage::new())
  
  // 连接数据源到处理器
  StreamProcessor::connect_source(stream_processor, data_source)
  StreamProcessor::set_pipeline(stream_processor, pipeline)
  
  // 启动流处理
  let start_result = StreamProcessor::start(stream_processor)
  match start_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 处理一段时间
  Time::sleep(2000) // 处理2秒
  
  // 停止流处理
  let stop_result = StreamProcessor::stop(stream_processor)
  match stop_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 获取处理统计
  let stats = StreamProcessor::get_stats(stream_processor)
  
  // 验证处理结果
  assert_true(stats.events_received > 0)
  assert_true(stats.events_processed > 0)
  assert_true(stats.events_dropped >= 0)
  assert_true(stats.processing_time > 0)
  
  // 验证处理效率
  let processing_rate = stats.events_processed.to_float() / (stats.processing_time / 1000.0)
  assert_true(processing_rate > 0) // 应该有正的处理速率
  
  StreamProcessor::cleanup(stream_processor)
  MockDataSource::cleanup(data_source)
  StreamPipeline::cleanup(pipeline)
}

// Test 2: 流数据窗口聚合测试
test "stream data window aggregation" {
  // 创建窗口聚合器
  let window_aggregator = WindowAggregator::new()
  
  // 配置窗口策略
  let window_config = WindowConfig::new()
  WindowConfig::set_window_size(window_config, 5000) // 5秒窗口
  WindowConfig::set_slide_interval(window_config, 1000) // 1秒滑动
  WindowConfig::set_aggregation_type(window_config, "time_based")
  
  WindowAggregator::configure(window_aggregator, window_config)
  
  // 创建测试数据流
  let data_stream = MockDataStream::new()
  
  // 生成时间序列数据
  let base_time = Time::now()
  for i in 0..<100 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, base_time + i * 100) // 每100ms一个事件
    
    // 添加指标
    StreamEvent::add_metric(event, "latency", 50.0 + (i % 100).to_float())
    StreamEvent::add_metric(event, "cpu_usage", 30.0 + (i % 70).to_float())
    StreamEvent::add_metric(event, "memory_usage", 40.0 + (i % 60).to_float())
    
    // 添加维度
    StreamEvent::add_dimension(event, "service", "test_service")
    StreamEvent::add_dimension(event, "region", "test_region")
    
    MockDataStream::add_event(data_stream, event)
  }
  
  // 处理数据流
  let aggregation_results = WindowAggregator::process_stream(window_aggregator, data_stream)
  
  // 验证窗口聚合结果
  assert_true(aggregation_results.length() > 0)
  
  for window_result in aggregation_results {
    assert_true(window_result.window_start > 0)
    assert_true(window_result.window_end > window_result.window_start)
    assert_true(window_result.events_count > 0)
    
    // 验证聚合统计
    assert_true(window_result.aggregates.contains("latency"))
    assert_true(window_result.aggregates.contains("cpu_usage"))
    assert_true(window_result.aggregates.contains("memory_usage"))
    
    let latency_stats = window_result.aggregates["latency"]
    assert_true(latency_stats.count > 0)
    assert_true(latency_stats.average >= 50.0)
    assert_true(latency_stats.min >= 50.0)
    assert_true(latency_stats.max <= 149.0)
  }
  
  // 测试不同窗口类型
  let count_window_config = WindowConfig::new()
  WindowConfig::set_window_size(count_window_config, 10) // 10个事件窗口
  WindowConfig::set_aggregation_type(count_window_config, "count_based")
  
  WindowAggregator::reconfigure(window_aggregator, count_window_config)
  let count_results = WindowAggregator::process_stream(window_aggregator, data_stream)
  
  // 验证计数窗口结果
  assert_true(count_results.length() > 0)
  
  for count_result in count_results {
    assert_true(count_result.events_count <= 10) // 不应超过窗口大小
  }
  
  WindowAggregator::cleanup(window_aggregator)
  MockDataStream::cleanup(data_stream)
}

// Test 3: 流数据异常检测测试
test "stream data anomaly detection" {
  // 创建流异常检测器
  let anomaly_detector = StreamAnomalyDetector::new()
  
  // 配置异常检测算法
  let detector_config = AnomalyDetectorConfig::new()
  AnomalyDetectorConfig::set_algorithm(detector_config, "statistical")
  AnomalyDetectorConfig::set_sensitivity(detector_config, 0.8) // 80%敏感度
  AnomalyDetectorConfig::set_training_window(detector_config, 50) // 50个事件训练窗口
  
  StreamAnomalyDetector::configure(anomaly_detector, detector_config)
  
  // 创建正常数据流
  let normal_stream = MockDataStream::new()
  let base_time = Time::now()
  
  // 生成正常数据
  for i in 0..<100 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, base_time + i * 100)
    
    // 正态分布指标
    let latency = 100.0 + NormalDistribution::sample(0.0, 15.0)
    let cpu_usage = 50.0 + NormalDistribution::sample(0.0, 10.0)
    let memory_usage = 60.0 + NormalDistribution::sample(0.0, 12.0)
    
    StreamEvent::add_metric(event, "latency", Math::max(0.0, latency))
    StreamEvent::add_metric(event, "cpu_usage", Math::max(0.0, Math::min(100.0, cpu_usage)))
    StreamEvent::add_metric(event, "memory_usage", Math::max(0.0, Math::min(100.0, memory_usage)))
    
    StreamEvent::add_dimension(event, "service", "web")
    StreamEvent::add_dimension(event, "environment", "prod")
    
    MockDataStream::add_event(normal_stream, event)
  }
  
  // 训练异常检测器
  let training_result = StreamAnomalyDetector::train(anomaly_detector, normal_stream)
  match training_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 创建包含异常的数据流
  let anomaly_stream = MockDataStream::new()
  
  // 添加正常数据
  for i in 0..<50 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, base_time + i * 100)
    
    let latency = 100.0 + NormalDistribution::sample(0.0, 15.0)
    let cpu_usage = 50.0 + NormalDistribution::sample(0.0, 10.0)
    
    StreamEvent::add_metric(event, "latency", Math::max(0.0, latency))
    StreamEvent::add_metric(event, "cpu_usage", Math::max(0.0, Math::min(100.0, cpu_usage)))
    
    StreamEvent::add_dimension(event, "service", "web")
    StreamEvent::add_dimension(event, "environment", "prod")
    
    MockDataStream::add_event(anomaly_stream, event)
  }
  
  // 添加异常数据
  for i in 0..<10 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, base_time + (50 + i) * 100)
    
    // 异常指标
    let anomaly_type = i % 3
    match anomaly_type {
      0 => { // 延迟异常
        StreamEvent::add_metric(event, "latency", 500.0 + (Random::generate() % 100).to_float())
        StreamEvent::add_metric(event, "cpu_usage", 50.0 + NormalDistribution::sample(0.0, 10.0))
      }
      1 => { // CPU异常
        StreamEvent::add_metric(event, "latency", 100.0 + NormalDistribution::sample(0.0, 15.0))
        StreamEvent::add_metric(event, "cpu_usage", 95.0 + (Random::generate() % 5).to_float())
      }
      _ => { // 内存异常
        StreamEvent::add_metric(event, "latency", 100.0 + NormalDistribution::sample(0.0, 15.0))
        StreamEvent::add_metric(event, "cpu_usage", 50.0 + NormalDistribution::sample(0.0, 10.0))
        StreamEvent::add_metric(event, "memory_usage", 95.0 + (Random::generate() % 5).to_float())
      }
    }
    
    StreamEvent::add_dimension(event, "service", "web")
    StreamEvent::add_dimension(event, "environment", "prod")
    
    MockDataStream::add_event(anomaly_stream, event)
  }
  
  // 检测异常
  let detection_results = StreamAnomalyDetector::detect(anomaly_detector, anomaly_stream)
  
  // 验证异常检测结果
  assert_true(detection_results.total_events == 60)
  assert_true(detection_results.anomalies.length() > 0)
  assert_true(detection_results.anomalies.length() <= 10) // 不应超过添加的异常数量
  
  // 验证异常详情
  for anomaly in detection_results.anomalies {
    assert_true(anomaly.anomaly_score > 0.5) // 异常分数应该较高
    assert_true(anomaly.timestamp > 0)
    assert_true(anomaly.metrics.length() > 0)
    assert_true(anomaly.reason.length() > 0)
  }
  
  // 测试实时异常检测
  let realtime_detector = StreamAnomalyDetector::new()
  StreamAnomalyDetector::configure(realtime_detector, detector_config)
  StreamAnomalyDetector::train(realtime_detector, normal_stream)
  
  let realtime_results = []
  
  // 模拟实时数据流
  for i in 0..<20 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, Time::now())
    
    if i >= 15 {
      // 最后5个事件是异常
      StreamEvent::add_metric(event, "latency", 500.0)
    } else {
      StreamEvent::add_metric(event, "latency", 100.0 + NormalDistribution::sample(0.0, 15.0))
    }
    
    StreamEvent::add_dimension(event, "service", "web")
    
    let result = StreamAnomalyDetector::detect_event(realtime_detector, event)
    realtime_results = realtime_results.push(result)
  }
  
  // 验证实时检测结果
  let detected_anomalies = realtime_results.filter(fn(r) { r.is_anomaly }).length()
  assert_true(detected_anomalies >= 3) // 至少应该检测到一些异常
  
  StreamAnomalyDetector::cleanup(anomaly_detector)
  StreamAnomalyDetector::cleanup(realtime_detector)
  MockDataStream::cleanup(normal_stream)
  MockDataStream::cleanup(anomaly_stream)
}

// Test 4: 流数据模式识别测试
test "stream data pattern recognition" {
  // 创建流模式识别器
  let pattern_recognizer = StreamPatternRecognizer::new()
  
  // 配置模式识别算法
  let recognizer_config = PatternRecognizerConfig::new()
  PatternRecognizerConfig::set_pattern_types(recognizer_config, ["seasonal", "trend", "spike"])
  PatternRecognizerConfig::set_min_pattern_length(recognizer_config, 5) // 最小模式长度5
  PatternRecognizerConfig::set_confidence_threshold(recognizer_config, 0.7) // 70%置信度阈值
  
  StreamPatternRecognizer::configure(pattern_recognizer, recognizer_config)
  
  // 创建包含模式的数据流
  let pattern_stream = MockDataStream::new()
  let base_time = Time::now()
  
  // 生成季节性模式数据
  for i in 0..<100 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, base_time + i * 1000) // 每秒一个事件
    
    // 季节性模式（24小时周期）
    let hour_of_day = (i % 24).to_float()
    let seasonal_factor = Math::sin((hour_of_day / 24.0) * 2.0 * 3.14159)
    
    // 趋势模式
    let trend_factor = i.to_float() / 100.0
    
    // 基础值
    let base_value = 100.0 + seasonal_factor * 20.0 + trend_factor * 10.0
    
    // 偶尔添加尖峰
    let spike_factor = if i % 20 == 0 { 50.0 } else { 0.0 }
    
    let metric_value = base_value + spike_factor + NormalDistribution::sample(0.0, 5.0)
    
    StreamEvent::add_metric(event, "request_count", Math::max(0.0, metric_value))
    StreamEvent::add_dimension(event, "service", "web")
    StreamEvent::add_dimension(event, "environment", "prod")
    
    MockDataStream::add_event(pattern_stream, event)
  }
  
  // 识别模式
  let recognition_results = StreamPatternRecognizer::recognize(pattern_recognizer, pattern_stream)
  
  // 验证模式识别结果
  assert_true(recognition_results.patterns.length() > 0)
  
  // 验证季节性模式
  let seasonal_patterns = recognition_results.patterns.filter(fn(p) { p.pattern_type == "seasonal" })
  assert_true(seasonal_patterns.length() > 0)
  
  for pattern in seasonal_patterns {
    assert_true(pattern.confidence >= 0.7)
    assert_true(pattern.start_time > 0)
    assert_true(pattern.end_time > pattern.start_time)
    assert_true(pattern.periodicity > 0)
  }
  
  // 验证趋势模式
  let trend_patterns = recognition_results.patterns.filter(fn(p) { p.pattern_type == "trend" })
  assert_true(trend_patterns.length() > 0)
  
  for pattern in trend_patterns {
    assert_true(pattern.confidence >= 0.7)
    assert_true(pattern.direction == "increasing" || pattern.direction == "decreasing" || pattern.direction == "stable")
  }
  
  // 验证尖峰模式
  let spike_patterns = recognition_results.patterns.filter(fn(p) { p.pattern_type == "spike" })
  assert_true(spike_patterns.length() > 0)
  
  for pattern in spike_patterns {
    assert_true(pattern.confidence >= 0.7)
    assert_true(pattern.spike_magnitude > 0)
  }
  
  // 测试实时模式识别
  let realtime_recognizer = StreamPatternRecognizer::new()
  StreamPatternRecognizer::configure(realtime_recognizer, recognizer_config)
  
  let realtime_patterns = []
  
  // 模拟实时数据流
  for i in 0..<50 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, Time::now())
    
    // 简单的周期性模式
    let periodic_value = 100.0 + 20.0 * Math::sin((i / 10.0) * 2.0 * 3.14159)
    
    StreamEvent::add_metric(event, "metric", periodic_value)
    StreamEvent::add_dimension(event, "service", "test")
    
    let result = StreamPatternRecognizer::recognize_event(realtime_recognizer, event)
    if result.patterns.length() > 0 {
      realtime_patterns = realtime_patterns.concat(result.patterns)
    }
  }
  
  // 验证实时模式识别
  assert_true(realtime_patterns.length() > 0)
  
  StreamPatternRecognizer::cleanup(pattern_recognizer)
  StreamPatternRecognizer::cleanup(realtime_recognizer)
  MockDataStream::cleanup(pattern_stream)
}

// Test 5: 流数据复杂事件处理测试
test "stream data complex event processing" {
  // 创建复杂事件处理器
  let cep_engine = ComplexEventProcessor::new()
  
  // 定义事件模式
  let event_patterns = [
    {
      "name": "high_latency_followed_by_error",
      "description": "高延迟后跟随错误",
      "conditions": [
        {"type": "event", "metric": "latency", "operator": ">", "value": 200.0},
        {"type": "followed_by", "time_window": 5000}, // 5秒内
        {"type": "event", "dimension": "error_type", "operator": "!=", "value": ""}
      ]
    },
    {
      "name": "resource_exhaustion",
      "description": "资源耗尽模式",
      "conditions": [
        {"type": "event", "metric": "cpu_usage", "operator": ">", "value": 90.0},
        {"type": "and"},
        {"type": "event", "metric": "memory_usage", "operator": ">", "value": 90.0},
        {"type": "within", "time_window": 10000} // 10秒内
      ]
    }
  ]
  
  // 注册事件模式
  for pattern in event_patterns {
    let pattern_result = ComplexEventProcessor::register_pattern(cep_engine, pattern)
    match pattern_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // 创建测试事件流
  let event_stream = MockEventStream::new()
  let base_time = Time::now()
  
  // 生成匹配模式的事件序列
  
  // 高延迟后跟随错误的事件序列
  let high_latency_event = StreamEvent::new()
  StreamEvent::set_timestamp(high_latency_event, base_time)
  StreamEvent::add_metric(high_latency_event, "latency", 250.0)
  StreamEvent::add_dimension(high_latency_event, "service", "web")
  MockEventStream::add_event(event_stream, high_latency_event)
  
  // 3秒后的错误事件
  let error_event = StreamEvent::new()
  StreamEvent::set_timestamp(error_event, base_time + 3000)
  StreamEvent::add_metric(error_event, "latency", 150.0)
  StreamEvent::add_dimension(error_event, "service", "web")
  StreamEvent::add_dimension(error_event, "error_type", "timeout")
  MockEventStream::add_event(event_stream, error_event)
  
  // 资源耗尽事件序列
  let high_cpu_event = StreamEvent::new()
  StreamEvent::set_timestamp(high_cpu_event, base_time + 5000)
  StreamEvent::add_metric(high_cpu_event, "cpu_usage", 95.0)
  StreamEvent::add_dimension(high_cpu_event, "service", "api")
  MockEventStream::add_event(event_stream, high_cpu_event)
  
  // 5秒后的高内存事件
  let high_memory_event = StreamEvent::new()
  StreamEvent::set_timestamp(high_memory_event, base_time + 10000)
  StreamEvent::add_metric(high_memory_event, "memory_usage", 92.0)
  StreamEvent::add_dimension(high_memory_event, "service", "api")
  MockEventStream::add_event(event_stream, high_memory_event)
  
  // 添加一些不匹配模式的普通事件
  for i in 0..<10 {
    let normal_event = StreamEvent::new()
    StreamEvent::set_timestamp(normal_event, base_time + 15000 + i * 1000)
    StreamEvent::add_metric(normal_event, "latency", 50.0 + (i % 50).to_float())
    StreamEvent::add_metric(normal_event, "cpu_usage", 30.0 + (i % 40).to_float())
    StreamEvent::add_dimension(normal_event, "service", "cache")
    MockEventStream::add_event(event_stream, normal_event)
  }
  
  // 处理事件流
  let processing_results = ComplexEventProcessor::process_stream(cep_engine, event_stream)
  
  // 验证复杂事件处理结果
  assert_true(processing_results.total_events == 13) // 2个模式事件序列 + 10个普通事件 + 1个额外事件
  assert_true(processing_results.complex_events.length() >= 2) // 至少应该检测到2个复杂事件
  
  // 验证检测到的复杂事件
  let high_latency_error_events = processing_results.complex_events.filter(fn(e) { e.pattern_name == "high_latency_followed_by_error" })
  assert_true(high_latency_error_events.length() >= 1)
  
  for event in high_latency_error_events {
    assert_true(event.matched_events.length() == 2)
    assert_true(event.confidence > 0.0)
    assert_true(event.timestamp > 0)
  }
  
  let resource_exhaustion_events = processing_results.complex_events.filter(fn(e) { e.pattern_name == "resource_exhaustion" })
  assert_true(resource_exhaustion_events.length() >= 1)
  
  for event in resource_exhaustion_events {
    assert_true(event.matched_events.length() == 2)
    assert_true(event.confidence > 0.0)
  }
  
  // 测试实时复杂事件处理
  let realtime_cep = ComplexEventProcessor::new()
  
  // 注册简单模式
  let simple_pattern = {
    "name": "simple_threshold",
    "description": "简单阈值模式",
    "conditions": [
      {"type": "event", "metric": "latency", "operator": ">", "value": 100.0}
    ]
  }
  
  ComplexEventProcessor::register_pattern(realtime_cep, simple_pattern)
  
  let realtime_results = []
  
  // 模拟实时事件流
  for i in 0..<20 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, Time::now())
    
    if i >= 15 {
      // 最后5个事件是高延迟
      StreamEvent::add_metric(event, "latency", 150.0)
    } else {
      StreamEvent::add_metric(event, "latency", 50.0)
    }
    
    StreamEvent::add_dimension(event, "service", "test")
    
    let result = ComplexEventProcessor::process_event(realtime_cep, event)
    if result.complex_events.length() > 0 {
      realtime_results = realtime_results.concat(result.complex_events)
    }
  }
  
  // 验证实时处理结果
  assert_true(realtime_results.length() >= 3) // 至少应该检测到3个高延迟事件
  
  ComplexEventProcessor::cleanup(cep_engine)
  ComplexEventProcessor::cleanup(realtime_cep)
  MockEventStream::cleanup(event_stream)
}

// Test 6: 流数据状态管理测试
test "stream data state management" {
  // 创建流状态管理器
  let state_manager = StreamStateManager::new()
  
  // 配置状态存储
  let state_config = StateManagerConfig::new()
  StateManagerConfig::set_storage_backend(state_config, "memory") // 内存存储
  StateManagerConfig::set_checkpoint_interval(state_config, 10000) // 10秒检查点
  StateManagerConfig::set_state_ttl(state_config, 60000) // 1分钟TTL
  
  StreamStateManager::configure(state_manager, state_config)
  
  // 创建带状态的处理函数
  let stateful_processor = StatefulStreamProcessor::new(state_manager)
  
  // 定义状态更新函数
  let update_fn = fn(event, current_state) {
    match current_state {
      Some(state) => {
        // 更新现有状态
        let new_count = state["event_count"] + 1
        let new_sum = state["metric_sum"] + event.metrics["latency"]
        let new_avg = new_sum / new_count.to_float()
        
        {
          "event_count": new_count,
          "metric_sum": new_sum,
          "metric_avg": new_avg,
          "last_updated": event.timestamp
        }
      }
      None => {
        // 初始化状态
        {
          "event_count": 1,
          "metric_sum": event.metrics["latency"],
          "metric_avg": event.metrics["latency"],
          "last_updated": event.timestamp
        }
      }
    }
  }
  
  // 创建测试数据流
  let stateful_stream = MockDataStream::new()
  let base_time = Time::now()
  
  // 生成测试事件
  for i in 0..<50 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, base_time + i * 1000)
    
    StreamEvent::add_metric(event, "latency", 50.0 + (i % 100).to_float())
    StreamEvent::add_dimension(event, "service", "web")
    StreamEvent::add_dimension(event, "region", "us-east")
    
    MockDataStream::add_event(stateful_stream, event)
  }
  
  // 处理带状态的数据流
  let stateful_results = StatefulStreamProcessor::process_with_state(
    stateful_processor, 
    stateful_stream, 
    "web:us-east", // 状态键
    update_fn
  )
  
  // 验证状态管理结果
  assert_true(stateful_results.processed_events == 50)
  
  // 获取最终状态
  let final_state = StreamStateManager::get_state(state_manager, "web:us-east")
  match final_state {
    Some(state) => {
      assert_eq(state["event_count"], 50)
      assert_true(state["metric_sum"] > 0)
      assert_true(state["metric_avg"] > 0)
      assert_true(state["last_updated"] > 0)
    }
    None => assert_true(false)
  }
  
  // 测试状态恢复
  let checkpoint_result = StreamStateManager::create_checkpoint(state_manager, "test_checkpoint")
  match checkpoint_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 清除状态
  StreamStateManager::clear_state(state_manager, "web:us-east")
  
  // 验证状态已清除
  let cleared_state = StreamStateManager::get_state(state_manager, "web:us-east")
  match cleared_state {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 恢复状态
  let restore_result = StreamStateManager::restore_from_checkpoint(state_manager, "test_checkpoint")
  match restore_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证状态已恢复
  let restored_state = StreamStateManager::get_state(state_manager, "web:us-east")
  match restored_state {
    Some(state) => {
      assert_eq(state["event_count"], 50)
    }
    None => assert_true(false)
  }
  
  // 测试状态过期
  StreamStateManager::set_state_ttl(state_manager, 100) // 100ms TTL
  Time::sleep(200) // 等待TTL过期
  
  let expired_state = StreamStateManager::get_state(state_manager, "web:us-east")
  match expired_state {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  StreamStateManager::cleanup(state_manager)
  StatefulStreamProcessor::cleanup(stateful_processor)
  MockDataStream::cleanup(stateful_stream)
}

// Test 7: 流数据背压处理测试
test "stream data backpressure handling" {
  // 创建带背压处理的流处理器
  let backpressure_processor = BackpressureStreamProcessor::new()
  
  // 配置背压策略
  let backpressure_config = BackpressureConfig::new()
  BackpressureConfig::set_strategy(backpressure_config, "buffer") // 缓冲策略
  BackpressureConfig::set_buffer_size(backpressure_config, 1000) // 1000个事件缓冲
  BackpressureConfig::set_threshold(backpressure_config, 0.8) // 80%阈值触发背压
  
  BackpressureStreamProcessor::configure(backpressure_processor, backpressure_config)
  
  // 创建快速数据源
  let fast_source = MockFastDataSource::new()
  MockFastDataSource::set_rate(fast_source, 1000) // 每秒1000个事件
  
  // 创建慢速处理器
  let slow_processor = MockSlowProcessor::new()
  MockSlowProcessor::set_processing_time(slow_processor, 10) // 每个事件处理10ms
  
  // 连接数据源和处理器
  BackpressureStreamProcessor::connect_source(backpressure_processor, fast_source)
  BackpressureStreamProcessor::set_processor(backpressure_processor, slow_processor)
  
  // 启动处理
  let start_result = BackpressureStreamProcessor::start(backpressure_processor)
  match start_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 处理一段时间
  Time::sleep(3000) // 处理3秒
  
  // 停止处理
  let stop_result = BackpressureStreamProcessor::stop(backpressure_processor)
  match stop_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 获取处理统计
  let stats = BackpressureStreamProcessor::get_stats(backpressure_processor)
  
  // 验证背压处理结果
  assert_true(stats.events_produced > 0)
  assert_true(stats.events_consumed > 0)
  assert_true(stats.buffer_utilization >= 0.0 && stats.buffer_utilization <= 1.0)
  assert_true(stats.backpressure_triggered >= 0)
  
  // 如果触发了背压，验证背压指标
  if stats.backpressure_triggered > 0 {
    assert_true(stats.backpressure_duration > 0)
    assert_true(stats.events_dropped >= 0)
  }
  
  // 测试不同背压策略
  let drop_strategy_config = BackpressureConfig::new()
  BackpressureConfig::set_strategy(drop_strategy_config, "drop") // 丢弃策略
  BackpressureConfig::set_drop_ratio(drop_strategy_config, 0.1) // 丢弃10%事件
  
  BackpressureStreamProcessor::reconfigure(backpressure_processor, drop_strategy_config)
  
  // 重置统计
  BackpressureStreamProcessor::reset_stats(backpressure_processor)
  
  // 再次启动处理
  BackpressureStreamProcessor::start(backpressure_processor)
  Time::sleep(2000)
  BackpressureStreamProcessor::stop(backpressure_processor)
  
  // 获取新统计
  let drop_stats = BackpressureStreamProcessor::get_stats(backpressure_processor)
  
  // 验证丢弃策略
  if drop_stats.backpressure_triggered > 0 {
    assert_true(drop_stats.events_dropped > 0)
    let drop_ratio = drop_stats.events_dropped.to_float() / drop_stats.events_produced.to_float()
    assert_true(drop_ratio > 0.05 && drop_ratio < 0.15) // 应该接近10%
  }
  
  BackpressureStreamProcessor::cleanup(backpressure_processor)
  MockFastDataSource::cleanup(fast_source)
  MockSlowProcessor::cleanup(slow_processor)
}

// Test 8: 流数据动态路由测试
test "stream data dynamic routing" {
  // 创建动态路由器
  let dynamic_router = DynamicStreamRouter::new()
  
  // 定义路由规则
  let routing_rules = [
    {
      "name": "high_latency_route",
      "condition": {"metric": "latency", "operator": ">", "value": 100.0},
      "destination": "high_latency_processor"
    },
    {
      "name": "error_route",
      "condition": {"dimension": "error_type", "operator": "!=", "value": ""},
      "destination": "error_processor"
    },
    {
      "name": "critical_service_route",
      "condition": {"dimension": "service", "operator": "==", "value": "payment"},
      "destination": "critical_processor"
    },
    {
      "name": "default_route",
      "condition": {"metric": "any", "operator": "==", "value": "any"},
      "destination": "default_processor"
    }
  ]
  
  // 注册路由规则
  for rule in routing_rules {
    let rule_result = DynamicStreamRouter::add_rule(dynamic_router, rule)
    match rule_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // 创建处理器
  let processors = {
    "high_latency_processor": MockStreamProcessor::new(),
    "error_processor": MockStreamProcessor::new(),
    "critical_processor": MockStreamProcessor::new(),
    "default_processor": MockStreamProcessor::new()
  }
  
  // 连接处理器到路由器
  for (name, processor) in processors {
    DynamicStreamRouter::connect_processor(dynamic_router, name, processor)
  }
  
  // 创建测试数据流
  let routing_stream = MockDataStream::new()
  let base_time = Time::now()
  
  // 生成不同类型的事件
  let test_events = [
    // 高延迟事件
    {
      "latency": 150.0,
      "service": "web",
      "error_type": "",
      "expected_destination": "high_latency_processor"
    },
    // 错误事件
    {
      "latency": 50.0,
      "service": "api",
      "error_type": "timeout",
      "expected_destination": "error_processor"
    },
    // 关键服务事件
    {
      "latency": 75.0,
      "service": "payment",
      "error_type": "",
      "expected_destination": "critical_processor"
    },
    // 普通事件
    {
      "latency": 60.0,
      "service": "cache",
      "error_type": "",
      "expected_destination": "default_processor"
    },
    // 高延迟错误事件（应该匹配第一个规则）
    {
      "latency": 200.0,
      "service": "db",
      "error_type": "connection_error",
      "expected_destination": "high_latency_processor"
    }
  ]
  
  for i in 0..<test_events.length() {
    let test_event = test_events[i]
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, base_time + i * 1000)
    
    StreamEvent::add_metric(event, "latency", test_event.latency)
    StreamEvent::add_dimension(event, "service", test_event.service)
    StreamEvent::add_dimension(event, "error_type", test_event.error_type)
    
    MockDataStream::add_event(routing_stream, event)
  }
  
  // 处理路由
  let routing_results = DynamicStreamRouter::process_stream(dynamic_router, routing_stream)
  
  // 验证路由结果
  assert_true(routing_results.total_events == test_events.length())
  
  // 验证每个处理器接收到正确的事件
  for (processor_name, processor) in processors {
    let received_events = MockStreamProcessor::get_received_events(processor)
    let expected_events = test_events.filter(fn(e) { e.expected_destination == processor_name })
    
    assert_eq(received_events.length(), expected_events.length())
    
    for i in 0..<received_events.length() {
      let received_event = received_events[i]
      let expected_event = expected_events[i]
      
      assert_eq(received_event.metrics["latency"], expected_event.latency)
      assert_eq(received_event.dimensions["service"], expected_event.service)
      assert_eq(received_event.dimensions["error_type"], expected_event.error_type)
    }
  }
  
  // 测试动态规则添加
  let new_rule = {
    "name": "memory_pressure_route",
    "condition": {"metric": "memory_usage", "operator": ">", "value": 90.0},
    "destination": "memory_pressure_processor"
  }
  
  let add_rule_result = DynamicStreamRouter::add_rule(dynamic_router, new_rule)
  match add_rule_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证规则已添加
  let rules = DynamicStreamRouter::get_rules(dynamic_router)
  assert_true(rules.length() == routing_rules.length() + 1)
  
  // 测试规则移除
  let remove_rule_result = DynamicStreamRouter::remove_rule(dynamic_router, "memory_pressure_route")
  match remove_rule_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证规则已移除
  let updated_rules = DynamicStreamRouter::get_rules(dynamic_router)
  assert_true(updated_rules.length() == routing_rules.length())
  
  DynamicStreamRouter::cleanup(dynamic_router)
  for (_, processor) in processors {
    MockStreamProcessor::cleanup(processor)
  }
  MockDataStream::cleanup(routing_stream)
}

// Test 9: 流数据容错和恢复测试
test "stream data fault tolerance and recovery" {
  // 创建容错流处理器
  let fault_tolerant_processor = FaultTolerantStreamProcessor::new()
  
  // 配置容错策略
  let fault_tolerance_config = FaultToleranceConfig::new()
  FaultToleranceConfig::set_checkpointing_strategy(fault_tolerance_config, "periodic")
  FaultToleranceConfig::set_checkpoint_interval(fault_tolerance_config, 5000) // 5秒
  FaultToleranceConfig::set_recovery_strategy(fault_tolerance_config, "replay_from_checkpoint")
  
  FaultTolerantStreamProcessor::configure(fault_tolerant_processor, fault_tolerance_config)
  
  // 创建测试数据流
  let fault_tolerance_stream = MockDataStream::new()
  let base_time = Time::now()
  
  // 生成测试事件
  for i in 0..<100 {
    let event = StreamEvent::new()
    StreamEvent::set_timestamp(event, base_time + i * 1000)
    
    StreamEvent::add_metric(event, "value", i.to_float())
    StreamEvent::add_dimension(event, "id", i.to_string())
    
    MockDataStream::add_event(fault_tolerance_stream, event)
  }
  
  // 启动处理
  FaultTolerantStreamProcessor::start(fault_tolerant_processor)
  
  // 处理一部分事件
  let partial_results = FaultTolerantStreamProcessor::process_partial(
    fault_tolerant_processor, 
    fault_tolerance_stream, 
    50 // 处理前50个事件
  )
  
  // 验证部分处理结果
  assert_eq(partial_results.processed_events, 50)
  
  // 模拟处理器故障
  FaultTolerantStreamProcessor::simulate_failure(fault_tolerant_processor)
  
  // 验证故障状态
  let failure_status = FaultTolerantStreamProcessor::get_status(fault_tolerant_processor)
  assert_true(failure_status.is_failed)
  
  // 尝试恢复
  let recovery_result = FaultTolerantStreamProcessor::recover(fault_tolerant_processor)
  match recovery_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证恢复状态
  let recovery_status = FaultTolerantStreamProcessor::get_status(fault_tolerant_processor)
  assert_true(recovery_status.is_operational)
  
  // 继续处理剩余事件
  let remaining_results = FaultTolerantStreamProcessor::process_remaining(
    fault_tolerant_processor, 
    fault_tolerance_stream
  )
  
  // 验证完整处理结果
  assert_eq(remaining_results.processed_events, 50) // 应该处理剩余的50个事件
  
  // 获取总处理统计
  let total_stats = FaultTolerantStreamProcessor::get_total_stats(fault_tolerant_processor)
  assert_eq(total_stats.total_processed, 100) // 总共应该处理100个事件
  assert_true(total_stats.failure_count > 0)
  assert_true(total_stats.recovery_count > 0)
  
  // 测试检查点恢复
  let checkpoint_result = FaultTolerantStreamProcessor::create_checkpoint(fault_tolerant_processor, "test_checkpoint")
  match checkpoint_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 模拟更严重的故障（需要从检查点恢复）
  FaultTolerantStreamProcessor::simulate_catastrophic_failure(fault_tolerant_processor)
  
  // 从检查点恢复
  let checkpoint_recovery_result = FaultTolerantStreamProcessor::recover_from_checkpoint(
    fault_tolerant_processor, 
    "test_checkpoint"
  )
  match checkpoint_recovery_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证检查点恢复状态
  let checkpoint_recovery_status = FaultTolerantStreamProcessor::get_status(fault_tolerant_processor)
  assert_true(checkpoint_recovery_status.is_operational)
  
  FaultTolerantStreamProcessor::cleanup(fault_tolerant_processor)
  MockDataStream::cleanup(fault_tolerance_stream)
}

// Test 10: 流数据性能基准测试
test "stream data performance benchmarking" {
  // 创建性能基准测试器
  let performance_benchmark = StreamPerformanceBenchmark::new()
  
  // 配置基准测试参数
  let benchmark_config = BenchmarkConfig::new()
  BenchmarkConfig::set_event_count(benchmark_config, 10000) // 10000个事件
  BenchmarkConfig::set_event_rate(benchmark_config, 1000) // 每秒1000个事件
  BenchmarkConfig::set_duration(benchmark_config, 15000) // 15秒测试
  
  StreamPerformanceBenchmark::configure(performance_benchmark, benchmark_config)
  
  // 定义测试场景
  let test_scenarios = [
    {
      "name": "simple_processing",
      "description": "简单处理场景",
      "processor": SimpleStreamProcessor::new(),
      "expected_throughput": 500.0 // 每秒500个事件
    },
    {
      "name": "complex_processing",
      "description": "复杂处理场景",
      "processor": ComplexStreamProcessor::new(),
      "expected_throughput": 200.0 // 每秒200个事件
    },
    {
      "name": "windowed_aggregation",
      "description": "窗口聚合场景",
      "processor": WindowedAggregationProcessor::new(),
      "expected_throughput": 300.0 // 每秒300个事件
    }
  ]
  
  let benchmark_results = []
  
  // 运行基准测试
  for scenario in test_scenarios {
    let result = StreamPerformanceBenchmark::run_scenario(
      performance_benchmark, 
      scenario.name, 
      scenario.processor
    )
    
    benchmark_results = benchmark_results.push({
      "scenario": scenario.name,
      "throughput": result.throughput,
      "latency_p50": result.latency_p50,
      "latency_p95": result.latency_p95,
      "latency_p99": result.latency_p99,
      "cpu_usage": result.cpu_usage,
      "memory_usage": result.memory_usage,
      "error_rate": result.error_rate
    })
    
    // 清理处理器
    match scenario.name {
      "simple_processing" => SimpleStreamProcessor::cleanup(scenario.processor),
      "complex_processing" => ComplexStreamProcessor::cleanup(scenario.processor),
      "windowed_aggregation" => WindowedAggregationProcessor::cleanup(scenario.processor),
      _ => ()
    }
  }
  
  // 验证基准测试结果
  assert_true(benchmark_results.length() == test_scenarios.length())
  
  for i in 0..<benchmark_results.length() {
    let result = benchmark_results[i]
    let scenario = test_scenarios[i]
    
    // 验证吞吐量
    assert_true(result.throughput > 0)
    assert_true(result.throughput >= scenario.expected_throughput * 0.8) // 至少达到期望吞吐量的80%
    
    // 验证延迟
    assert_true(result.latency_p50 > 0)
    assert_true(result.latency_p95 >= result.latency_p50)
    assert_true(result.latency_p99 >= result.latency_p95)
    
    // 验证资源使用
    assert_true(result.cpu_usage >= 0.0 && result.cpu_usage <= 100.0)
    assert_true(result.memory_usage > 0)
    
    // 验证错误率
    assert_true(result.error_rate >= 0.0 && result.error_rate <= 1.0)
    assert_true(result.error_rate < 0.05) // 错误率应该低于5%
  }
  
  // 测试可扩展性
  let scalability_results = StreamPerformanceBenchmark::test_scalability(
    performance_benchmark, 
    SimpleStreamProcessor::new(),
    [100, 500, 1000, 2000] // 不同的事件率
  )
  
  // 验证可扩展性结果
  assert_true(scalability_results.length() == 4)
  
  for i in 0..<scalability_results.length() - 1 {
    let current = scalability_results[i]
    let next = scalability_results[i + 1]
    
    // 验证吞吐量随事件率增加而增加（至少在开始阶段）
    if current.event_rate < 1000 { // 在达到系统极限前
      assert_true(next.throughput >= current.throughput * 0.8) // 允许一些效率损失
    }
  }
  
  // 生成性能报告
  let report_result = StreamPerformanceBenchmark::generate_report(performance_benchmark)
  match report_result {
    Ok(report) => {
      assert_true(report.scenarios.length() == test_scenarios.length())
      assert_true(report.summary.average_throughput > 0)
      assert_true(report.summary.best_scenario.length() > 0)
      assert_true(report.summary.worst_scenario.length() > 0)
    }
    Err(_) => assert_true(false)
  }
  
  StreamPerformanceBenchmark::cleanup(performance_benchmark)
}