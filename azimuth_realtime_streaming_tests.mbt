// Azimuth Real-time Streaming Test Suite
// 实时流处理测试套件 - 测试遥测数据的实时流处理功能

// Test 1: 实时数据流处理
test "real-time data stream processing" {
  // 创建实时流处理器
  let stream_processor = @azimuth.StreamProcessor::new(
    @azimuth.StreamConfig {
      buffer_size: 1000,
      batch_size: 100,
      processing_interval_ms: 1000,
      max_lag_ms: 5000
    }
  )
  
  // 添加流处理阶段
  @azimuth.StreamProcessor::add_stage(stream_processor, @azimuth.ValidationStage::new())
  @azimuth.StreamProcessor::add_stage(stream_processor, @azimuth.EnrichmentStage::new())
  @azimuth.StreamProcessor::add_stage(stream_processor, @azimuth.FilteringStage::new())
  @azimuth.StreamProcessor::add_stage(stream_processor, @azimuth.AggregationStage::new())
  
  // 创建实时遥测数据流
  let telemetry_stream = @azimuth.TelemetryStream::new()
  
  // 模拟实时数据产生
  let base_timestamp = @azimuth.current_timestamp()
  let stream_data = []
  
  for i = 0; i < 500; i = i + 1 {
    let data_point = @azimuth.TelemetryDataPoint {
      trace_id: "realtime-trace-" + i.to_string(),
      span_id: "realtime-span-" + i.to_string(),
      timestamp: base_timestamp + i * 10,  // 每10ms一个数据点
      metric_name: if i % 3 == 0 { "response.time" } else if i % 3 == 1 { "error.rate" } else { "throughput" },
      metric_value: (50 + (i % 100)).to_double(),
      attributes: [
        ("service.name", @azimuth.StringValue("realtime-service")),
        ("instance.id", @azimuth.StringValue("instance-" + (i % 5).to_string())),
        ("operation.type", @azimuth.StringValue("api-request"))
      ]
    }
    stream_data = stream_data.push(data_point)
  }
  
  // 将数据添加到流中
  for data_point in stream_data {
    @azimuth.TelemetryStream::add_data_point(telemetry_stream, data_point)
  }
  
  // 启动流处理
  let processing_result = @azimuth.StreamProcessor::process_stream(stream_processor, telemetry_stream)
  
  // 验证流处理结果
  assert_true(processing_result.success)
  assert_eq(processing_result.processed_count, 500)
  assert_eq(processing_result.error_count, 0)
  
  // 验证处理延迟
  assert_true(processing_result.average_processing_latency_ms < 100)
  assert_true(processing_result.max_processing_latency_ms < 500)
  
  // 验证吞吐量
  assert_true(processing_result.throughput_per_second > 100)
  
  // 获取处理后的数据
  let processed_data = @azimuth.StreamProcessor::get_processed_data(stream_processor)
  assert_eq(processed_data.length(), 500)
  
  // 验证数据增强
  let first_processed = processed_data[0]
  assert_true(@azimuth.Attributes::contains_key(first_processed.attributes, "processing.timestamp"))
  assert_true(@azimuth.Attributes::contains_key(first_processed.attributes, "processing.stage"))
  
  // 验证数据过滤
  let filtered_data = processed_data.filter(fn(data) {
    match @azimuth.Attributes::get(data.attributes, "filtered") {
      Some(@azimuth.BoolValue(filtered)) => not(filtered)
      _ => true
    }
  })
  assert_true(filtered_data.length() <= processed_data.length())
}

// Test 2: 窗口化流聚合
test "windowed stream aggregation" {
  // 创建窗口化聚合器
  let window_aggregator = @azimuth.WindowAggregator::new(
    @azimuth.WindowConfig {
      window_type: "tumbling",  // 滚动窗口
      window_size_ms: 10000,    // 10秒窗口
      slide_interval_ms: 10000, // 滑动间隔等于窗口大小
      allowed_lateness_ms: 5000, // 允许5秒延迟
      aggregation_functions: ["avg", "min", "max", "count"]
    }
  )
  
  // 创建时间序列流数据
  let base_time = 1640995200000  // 基准时间
  let time_series_data = []
  
  // 创建跨越多个窗口的数据点
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_time + i * 200  // 每200ms一个数据点
    let window_index = i / 50  // 每50个数据点一个窗口
    
    let data_point = @azimuth.TimeSeriesDataPoint {
      timestamp,
      metric_name: "request.duration",
      value: (100 + (i % 200)).to_double(),
      attributes: [
        ("service.name", @azimuth.StringValue("window-service")),
        ("window.index", @azimuth.IntValue(window_index))
      ]
    }
    time_series_data = time_series_data.push(data_point)
  }
  
  // 添加数据到窗口聚合器
  for data_point in time_series_data {
    @azimuth.WindowAggregator::add_data_point(window_aggregator, data_point)
  }
  
  // 触发窗口计算
  let window_results = @azimuth.WindowAggregator::trigger_window_computation(window_aggregator)
  
  // 验证窗口结果
  assert_eq(window_results.length(), 2)  // 应该有2个窗口
  
  // 验证第一个窗口 (0-50个数据点)
  let first_window = window_results[0]
  assert_eq(first_window.window_start_ms, base_time)
  assert_eq(first_window.window_end_ms, base_time + 10000)
  assert_eq(first_window.data_points_count, 50)
  
  // 验证第一个窗口的聚合结果
  match @azimuth.WindowResult::get_aggregation_value(first_window, "avg") {
    Some(@azimuth.FloatValue(avg)) => {
      // 计算期望平均值
      let sum = (0..50).reduce(fn(acc, i) { acc + (100 + (i % 200)).to_double() }, 0.0)
      let expected_avg = sum / 50.0
      assert_eq(avg, expected_avg)
    }
    _ => assert_true(false)
  }
  
  match @azimuth.WindowResult::get_aggregation_value(first_window, "min") {
    Some(@azimuth.FloatValue(min)) => assert_eq(min, 100.0)
    _ => assert_true(false)
  }
  
  match @azimuth.WindowResult::get_aggregation_value(first_window, "max") {
    Some(@azimuth.FloatValue(max)) => assert_eq(max, 199.0)
    _ => assert_true(false)
  }
  
  match @azimuth.WindowResult::get_aggregation_value(first_window, "count") {
    Some(@azimuth.IntValue(count)) => assert_eq(count, 50)
    _ => assert_true(false)
  }
  
  // 验证第二个窗口 (50-100个数据点)
  let second_window = window_results[1]
  assert_eq(second_window.window_start_ms, base_time + 10000)
  assert_eq(second_window.window_end_ms, base_time + 20000)
  assert_eq(second_window.data_points_count, 50)
  
  // 验证窗口元数据
  assert_true(first_window.processing_timestamp_ms > first_window.window_end_ms)
  assert_true(second_window.processing_timestamp_ms > second_window.window_end_ms)
}

// Test 3: 流式异常检测
test "streaming anomaly detection" {
  // 创建异常检测器
  let anomaly_detector = @azimuth.StreamAnomalyDetector::new(
    @azimuth.AnomalyDetectionConfig {
      algorithm: "isolation_forest",
      training_window_size: 100,
      detection_window_size: 10,
      contamination_rate: 0.1,
      features: ["response_time", "error_rate", "cpu_usage"]
    }
  )
  
  // 创建正常训练数据
  let training_data = []
  let base_timestamp = @azimuth.current_timestamp()
  
  for i = 0; i < 100; i = i + 1 {
    let data_point = @azimuth.MultiMetricDataPoint {
      timestamp: base_timestamp + i * 1000,
      metrics: [
        ("response_time", (50 + (i % 50)).to_double()),
        ("error_rate", (0.01 + (i % 5) * 0.001).to_double()),
        ("cpu_usage", (20 + (i % 30)).to_double())
      ],
      attributes: [
        ("service.name", @azimuth.StringValue("anomaly-detection-service"))
      ]
    }
    training_data = training_data.push(data_point)
  }
  
  // 训练异常检测模型
  let training_result = @azimuth.StreamAnomalyDetector::train(anomaly_detector, training_data)
  assert_true(training_result.success)
  assert_eq(training_result.training_samples, 100)
  
  // 创建测试数据 (包含一些异常)
  let test_data = []
  
  // 添加正常数据点
  for i = 0; i < 20; i = i + 1 {
    let data_point = @azimuth.MultiMetricDataPoint {
      timestamp: base_timestamp + 100000 + i * 1000,
      metrics: [
        ("response_time", (50 + (i % 50)).to_double()),
        ("error_rate", (0.01 + (i % 5) * 0.001).to_double()),
        ("cpu_usage", (20 + (i % 30)).to_double())
      ],
      attributes: [
        ("service.name", @azimuth.StringValue("anomaly-detection-service"))
      ]
    }
    test_data = test_data.push(data_point)
  }
  
  // 添加异常数据点
  let anomaly_data_point = @azimuth.MultiMetricDataPoint {
    timestamp: base_timestamp + 120000,
    metrics: [
      ("response_time", 500.0),  // 异常高的响应时间
      ("error_rate", 0.5),       // 异常高的错误率
      ("cpu_usage", 95.0)        // 异常高的CPU使用率
    ],
    attributes: [
      ("service.name", @azimuth.StringValue("anomaly-detection-service"))
    ]
  }
  test_data = test_data.push(anomaly_data_point)
  
  // 添加更多正常数据点
  for i = 21; i < 30; i = i + 1 {
    let data_point = @azimuth.MultiMetricDataPoint {
      timestamp: base_timestamp + 100000 + i * 1000,
      metrics: [
        ("response_time", (50 + (i % 50)).to_double()),
        ("error_rate", (0.01 + (i % 5) * 0.001).to_double()),
        ("cpu_usage", (20 + (i % 30)).to_double())
      ],
      attributes: [
        ("service.name", @azimuth.StringValue("anomaly-detection-service"))
      ]
    }
    test_data = test_data.push(data_point)
  }
  
  // 执行异常检测
  let detection_result = @azimuth.StreamAnomalyDetector::detect(anomaly_detector, test_data)
  
  // 验证检测结果
  assert_eq(detection_result.total_samples, 31)
  assert_eq(detection_result.anomaly_count, 1)
  assert_eq(detection_result.normal_count, 30)
  
  // 验证异常详情
  assert_eq(detection_result.anomalies.length(), 1)
  let detected_anomaly = detection_result.anomalies[0]
  
  assert_eq(detected_anomaly.timestamp, base_timestamp + 120000)
  assert_true(detected_anomaly.anomaly_score > 0.5)
  assert_eq(detected_anomaly.reason, "multi_metric_deviation")
  
  // 验证异常指标值
  match detected_anomaly.anomalous_metrics.find(fn(m) { m.name == "response_time" }) {
    Some(metric) => assert_eq(metric.value, 500.0)
    None => assert_true(false)
  }
  
  match detected_anomaly.anomalous_metrics.find(fn(m) { m.name == "error_rate" }) {
    Some(metric) => assert_eq(metric.value, 0.5)
    None => assert_true(false)
  }
  
  match detected_anomaly.anomalous_metrics.find(fn(m) { m.name == "cpu_usage" }) {
    Some(metric) => assert_eq(metric.value, 95.0)
    None => assert_true(false)
  }
}

// Test 4: 流式模式检测
test "streaming pattern detection" {
  // 创建模式检测器
  let pattern_detector = @azimuth.StreamPatternDetector::new(
    @azimuth.PatternDetectionConfig {
      pattern_types: ["seasonal", "trend", "spike", "dip"],
      min_pattern_length: 5,
      max_pattern_length: 20,
      confidence_threshold: 0.8
    }
  )
  
  // 创建包含季节性模式的数据
  let seasonal_data = []
  let base_time = 1640995200000  // 基准时间
  
  // 创建24小时的季节性数据 (每小时一个数据点)
  for hour = 0; hour < 24; hour = hour + 1 {
    // 模拟日流量模式：工作时间高，夜间低
    let base_value = if hour >= 8 and hour <= 18 {
      100.0 + (10.0 * ((hour - 8) / 10.0).sin())  // 工作时间基线100，有波动
    } else {
      20.0 + (5.0 * ((hour - 18) / 6.0).sin())     // 夜间基线20，有波动
    }
    
    let data_point = @azimuth.TimeSeriesDataPoint {
      timestamp: base_time + hour * 3600000,  // 每小时
      metric_name: "hourly.traffic",
      value: base_value,
      attributes: [
        ("service.name", @azimuth.StringValue("pattern-detection-service")),
        ("time.hour", @azimuth.IntValue(hour))
      ]
    }
    seasonal_data = seasonal_data.push(data_point)
  }
  
  // 添加趋势数据
  let trend_data = []
  for day = 0; day < 30; day = day + 1 {
    // 模拟30天的增长趋势
    let trend_value = 50.0 + (day * 2.0)  // 每天增长2
    
    let data_point = @azimuth.TimeSeriesDataPoint {
      timestamp: base_time + day * 86400000,  // 每天
      metric_name: "daily.active.users",
      value: trend_value,
      attributes: [
        ("service.name", @azimuth.StringValue("pattern-detection-service")),
        ("time.day", @azimuth.IntValue(day))
      ]
    }
    trend_data = trend_data.push(data_point)
  }
  
  // 添加尖峰数据
  let spike_data = []
  for minute = 0; minute < 60; minute = minute + 1 {
    // 正常基线
    let base_value = 50.0
    
    // 在第30分钟添加一个尖峰
    let value = if minute == 30 {
      500.0  // 尖峰
    } else {
      base_value
    }
    
    let data_point = @azimuth.TimeSeriesDataPoint {
      timestamp: base_time + minute * 60000,  // 每分钟
      metric_name: "minute.requests",
      value,
      attributes: [
        ("service.name", @azimuth.StringValue("pattern-detection-service")),
        ("time.minute", @azimuth.IntValue(minute))
      ]
    }
    spike_data = spike_data.push(data_point)
  }
  
  // 检测季节性模式
  let seasonal_patterns = @azimuth.StreamPatternDetector::detect_patterns(pattern_detector, seasonal_data, "seasonal")
  assert_eq(seasonal_patterns.length(), 1)
  
  let seasonal_pattern = seasonal_patterns[0]
  assert_eq(seasonal_pattern.pattern_type, "seasonal")
  assert_eq(seasonal_pattern.period_hours, 24)
  assert_true(seasonal_pattern.confidence > 0.8)
  
  // 验证季节性模式的详细信息
  assert_eq(seasonal_pattern.peak_hours.length(), 11)  // 8-18点，共11小时
  assert_true(seasonal_pattern.peak_hours.contains(8))
  assert_true(seasonal_pattern.peak_hours.contains(12))
  assert_true(seasonal_pattern.peak_hours.contains(18))
  
  assert_eq(seasonal_pattern.low_hours.length(), 13)  // 其他13小时
  assert_true(seasonal_pattern.low_hours.contains(0))
  assert_true(seasonal_pattern.low_hours.contains(6))
  assert_true(seasonal_pattern.low_hours.contains(23))
  
  // 检测趋势模式
  let trend_patterns = @azimuth.StreamPatternDetector::detect_patterns(pattern_detector, trend_data, "trend")
  assert_eq(trend_patterns.length(), 1)
  
  let trend_pattern = trend_patterns[0]
  assert_eq(trend_pattern.pattern_type, "trend")
  assert_eq(trend_pattern.trend_direction, "increasing")
  assert_true(trend_pattern.trend_slope > 1.9)  // 接近2.0
  assert_true(trend_pattern.confidence > 0.9)
  
  // 验证趋势模式的详细信息
  assert_eq(trend_pattern.start_value, 50.0)   // 第1天
  assert_eq(trend_pattern.end_value, 108.0)    // 第30天：50 + 29*2
  assert_eq(trend_pattern.total_change, 58.0)  // 总变化量
  assert_eq(trend_pattern.percent_change, 116.0) // 变化百分比
  
  // 检测尖峰模式
  let spike_patterns = @azimuth.StreamPatternDetector::detect_patterns(pattern_detector, spike_data, "spike")
  assert_eq(spike_patterns.length(), 1)
  
  let spike_pattern = spike_patterns[0]
  assert_eq(spike_pattern.pattern_type, "spike")
  assert_eq(spike_pattern.spike_timestamp, base_time + 30 * 60000)  // 第30分钟
  assert_eq(spike_pattern.spike_value, 500.0)
  assert_eq(spike_pattern.baseline_value, 50.0)
  assert_eq(spike_pattern.spike_multiplier, 10.0)  // 500/50 = 10
  assert_true(spike_pattern.confidence > 0.9)
}

// Test 5: 流式状态管理
test "streaming state management" {
  // 创建流状态管理器
  let state_manager = @azimuth.StreamStateManager::new(
    @azimuth.StateConfig {
      state_store_type: "memory",
      checkpoint_interval_ms: 10000,
      max_state_size_mb: 100,
      state_ttl_ms: 300000  // 5分钟TTL
    }
  )
  
  // 定义状态键
  let counter_state_key = @azimuth.StateKey::new("request_counter")
  let window_state_key = @azimuth.StateKey::new("sliding_window")
  let alert_state_key = @azimuth.StateKey::new("alert_states")
  
  // 初始化状态
  @azimuth.StreamStateManager::initialize_state(state_manager, counter_state_key, @azimuth.IntValue(0))
  @azimuth.StreamStateManager::initialize_state(state_manager, window_state_key, @azimuth.ArrayValue([]))
  @azimuth.StreamStateManager::initialize_state(state_manager, alert_state_key, @azimuth.MapValue([]))
  
  // 模拟流处理并更新状态
  for i = 0; i < 100; i = i + 1 {
    // 更新计数器状态
    let current_counter = @azimuth.StreamStateManager::get_state(state_manager, counter_state_key)
    match current_counter {
      Some(@azimuth.IntValue(count)) => {
        let new_count = count + 1
        @azimuth.StreamStateManager::update_state(state_manager, counter_state_key, @azimuth.IntValue(new_count))
      }
      _ => assert_true(false)
    }
    
    // 更新滑动窗口状态 (保留最近10个值)
    let current_window = @azimuth.StreamStateManager::get_state(state_manager, window_state_key)
    match current_window {
      Some(@azimuth.ArrayValue(window)) => {
        let new_window = window.push(i.to_double())
        let trimmed_window = if new_window.length() > 10 {
          new_window.slice(new_window.length() - 10, new_window.length())
        } else {
          new_window
        }
        @azimuth.StreamStateManager::update_state(state_manager, window_state_key, @azimuth.ArrayValue(trimmed_window))
      }
      _ => assert_true(false)
    }
    
    // 每10个值更新一次警报状态
    if i % 10 == 0 and i > 0 {
      let current_alerts = @azimuth.StreamStateManager::get_state(state_manager, alert_state_key)
      match current_alerts {
        Some(@azimuth.MapValue(alerts)) => {
          let new_alerts = alerts.push(("alert-" + i.to_string(), @azimuth.StringValue("threshold_exceeded")))
          @azimuth.StreamStateManager::update_state(state_manager, alert_state_key, @azimuth.MapValue(new_alerts))
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 验证最终状态
  let final_counter = @azimuth.StreamStateManager::get_state(state_manager, counter_state_key)
  match final_counter {
    Some(@azimuth.IntValue(count)) => assert_eq(count, 100)
    _ => assert_true(false)
  }
  
  let final_window = @azimuth.StreamStateManager::get_state(state_manager, window_state_key)
  match final_window {
    Some(@azimuth.ArrayValue(window)) => {
      assert_eq(window.length(), 10)
      // 验证窗口包含最后10个值
      assert_eq(window[0], 90.0)
      assert_eq(window[9], 99.0)
    }
    _ => assert_true(false)
  }
  
  let final_alerts = @azimuth.StreamStateManager::get_state(state_manager, alert_state_key)
  match final_alerts {
    Some(@azimuth.MapValue(alerts)) => {
      assert_eq(alerts.length(), 9)  // 10, 20, ..., 90
      assert_true(alerts.contains(("alert-10", @azimuth.StringValue("threshold_exceeded"))))
      assert_true(alerts.contains(("alert-90", @azimuth.StringValue("threshold_exceeded"))))
    }
    _ => assert_true(false)
  }
  
  // 创建状态检查点
  let checkpoint_result = @azimuth.StreamStateManager::create_checkpoint(state_manager, "test-checkpoint")
  assert_true(checkpoint_result.success)
  assert_eq(checkpoint_result.checkpoint_id, "test-checkpoint")
  assert_true(checkpoint_result.state_size_bytes > 0)
  
  // 验证状态恢复
  let new_state_manager = @azimuth.StreamStateManager::new(
    @azimuth.StateConfig {
      state_store_type: "memory",
      checkpoint_interval_ms: 10000,
      max_state_size_mb: 100,
      state_ttl_ms: 300000
    }
  )
  
  let restore_result = @azimuth.StreamStateManager::restore_from_checkpoint(new_state_manager, "test-checkpoint")
  assert_true(restore_result.success)
  
  // 验证恢复的状态
  let restored_counter = @azimuth.StreamStateManager::get_state(new_state_manager, counter_state_key)
  match restored_counter {
    Some(@azimuth.IntValue(count)) => assert_eq(count, 100)
    _ => assert_true(false)
  }
  
  let restored_window = @azimuth.StreamStateManager::get_state(new_state_manager, window_state_key)
  match restored_window {
    Some(@azimuth.ArrayValue(window)) => assert_eq(window.length(), 10)
    _ => assert_true(false)
  }
  
  // 测试状态过期
  let expired_state_key = @azimuth.StateKey::new("expired_state")
  @azimuth.StreamStateManager::initialize_state(state_manager, expired_state_key, @azimuth.StringValue("test"))
  
  // 模拟时间流逝 (在实际环境中，这需要时间等待)
  // 这里我们直接测试状态清理功能
  let cleanup_result = @azimuth.StreamStateManager::cleanup_expired_states(state_manager)
  assert_true(cleanup_result.success)
  assert_eq(cleanup_result.cleaned_states_count, 0)  // 在这个简化的测试中，没有状态过期
}

// Test 6: 流式背压处理
test "streaming backpressure handling" {
  // 创建带背压处理的流处理器
  let backpressure_processor = @azimuth.BackpressureStreamProcessor::new(
    @azimuth.BackpressureConfig {
      buffer_size: 100,
      high_watermark_percent: 80,  // 80%时开始背压
      low_watermark_percent: 40,   // 40%时停止背压
      max_processing_delay_ms: 5000,
      backpressure_strategy: "buffer_drop_oldest"
    }
  )
  
  // 添加慢处理阶段 (模拟处理瓶颈)
  let slow_stage = @azimuth.SlowProcessingStage::new(100)  // 每个数据点处理100ms
  @azimuth.BackpressureStreamProcessor::add_stage(backpressure_processor, slow_stage)
  
  // 创建快速数据流
  let fast_stream = @azimuth.TelemetryStream::new()
  let base_timestamp = @azimuth.current_timestamp()
  
  // 快速添加数据点 (每10ms一个，但处理每个需要100ms)
  for i = 0; i < 50; i = i + 1 {
    let data_point = @azimuth.TelemetryDataPoint {
      trace_id: "backpressure-trace-" + i.to_string(),
      span_id: "backpressure-span-" + i.to_string(),
      timestamp: base_timestamp + i * 10,
      metric_name: "fast.metric",
      metric_value: i.to_double(),
      attributes: [
        ("service.name", @azimuth.StringValue("backpressure-service"))
      ]
    }
    @azimuth.TelemetryStream::add_data_point(fast_stream, data_point)
  }
  
  // 监控背压指标
  let backpressure_monitor = @azimuth.BackpressureMonitor::new()
  @azimuth.BackpressureStreamProcessor::set_monitor(backpressure_processor, backpressure_monitor)
  
  // 启动流处理
  let processing_start = @azimuth.current_timestamp()
  let processing_result = @azimuth.BackpressureStreamProcessor::process_stream(backpressure_processor, fast_stream)
  let processing_end = @azimuth.current_timestamp()
  
  // 验证处理结果
  assert_true(processing_result.success)
  
  // 由于处理速度慢于输入速度，一些数据可能会被丢弃
  assert_true(processing_result.processed_count <= 50)
  assert_true(processing_result.dropped_count >= 0)
  
  // 验证背压事件
  let backpressure_events = @azimuth.BackpressureMonitor::get_events(backpressure_monitor)
  assert_true(backpressure_events.length() > 0)
  
  // 验证背压触发事件
  let trigger_events = backpressure_events.filter(fn(event) { event.event_type == "backpressure_triggered" })
  assert_true(trigger_events.length() > 0)
  
  // 验证背压缓解事件
  let relieve_events = backpressure_events.filter(fn(event) { event.event_type == "backpressure_relieved" })
  assert_true(relieve_events.length() > 0)
  
  // 验证数据丢弃事件
  let drop_events = backpressure_events.filter(fn(event) { event.event_type == "data_dropped" })
  if processing_result.dropped_count > 0 {
    assert_true(drop_events.length() > 0)
  }
  
  // 验证缓冲区使用情况
  let buffer_metrics = @azimuth.BackpressureMonitor::get_buffer_metrics(backpressure_monitor)
  assert_true(buffer_metrics.max_buffer_usage > 0)
  assert_true(buffer_metrics.max_buffer_usage <= 100)
  assert_true(buffer_metrics.average_buffer_usage > 0)
  
  // 验证处理延迟
  let processing_metrics = @azimuth.BackpressureMonitor::get_processing_metrics(backpressure_monitor)
  assert_true(processing_metrics.average_processing_delay_ms >= 0)
  assert_true(processing_metrics.max_processing_delay_ms >= 100)  // 至少包含慢处理阶段的时间
  
  // 验证背压策略效果
  assert_true(processing_end - processing_start < 10000)  // 应该在10秒内完成，而不是50*100ms=5000ms
}