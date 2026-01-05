// 阿兹米克实时流处理测试用例
// 专注于遥测数据的实时流处理、分析和响应功能

// 测试1: 基本流数据处理
test "基本流数据处理测试" {
  let stream_processor = StreamProcessor::new()
  let telemetry_stream = TelemetryStream::new("realtime.telemetry")
  
  // 创建流数据源
  let data_source = DataSource::new("system.metrics")
  
  // 注册流处理器
  StreamProcessor::register_source(stream_processor, data_source, telemetry_stream)
  
  // 模拟实时数据流
  let metric_events = [
    MetricEvent::new("cpu.usage", 45.2, Timestamp::now()),
    MetricEvent::new("memory.usage", 67.8, Timestamp::now()),
    MetricEvent::new("disk.usage", 23.1, Timestamp::now()),
    MetricEvent::new("network.throughput", 1024.5, Timestamp::now()),
    MetricEvent::new("response.time", 120.3, Timestamp::now())
  ]
  
  // 处理流数据
  let processed_events = []
  for event in metric_events {
    let processed = StreamProcessor::process_event(stream_processor, event)
    processed_events = processed_events.push(processed)
  }
  
  // 验证处理结果
  assert_eq(processed_events.length(), 5)
  
  // 验证事件处理顺序
  assert_eq(MetricEvent::name(processed_events[0]), "cpu.usage")
  assert_eq(MetricEvent::name(processed_events[4]), "response.time")
  
  // 验证数据完整性
  for i in 0..processed_events.length() {
    assert_eq(MetricEvent::name(processed_events[i]), MetricEvent::name(metric_events[i]))
    assert_eq(MetricEvent::value(processed_events[i]), MetricEvent::value(metric_events[i]))
  }
}

// 测试2: 流数据窗口聚合
test "流数据窗口聚合测试" {
  let stream_processor = StreamProcessor::new()
  let telemetry_stream = TelemetryStream::new("windowed.telemetry")
  
  // 创建时间窗口聚合器
  let window_aggregator = TimeWindowAggregator::new(5000) // 5秒窗口
  
  // 注册聚合器
  StreamProcessor::register_aggregator(stream_processor, window_aggregator, telemetry_stream)
  
  // 模拟时间序列数据流
  let base_time = Timestamp::now()
  let time_series_events = [
    MetricEvent::with_timestamp("cpu.usage", 45.2, base_time),
    MetricEvent::with_timestamp("cpu.usage", 52.8, base_time + 1000),
    MetricEvent::with_timestamp("cpu.usage", 48.1, base_time + 2000),
    MetricEvent::with_timestamp("cpu.usage", 61.3, base_time + 3000),
    MetricEvent::with_timestamp("cpu.usage", 55.7, base_time + 4000),
    MetricEvent::with_timestamp("cpu.usage", 49.2, base_time + 6000), // 新窗口
    MetricEvent::with_timestamp("cpu.usage", 53.4, base_time + 7000)
  ]
  
  // 处理时间序列数据
  for event in time_series_events {
    StreamProcessor::process_event(stream_processor, event)
  }
  
  // 获取窗口聚合结果
  let windows = TimeWindowAggregator::get_completed_windows(window_aggregator)
  
  // 验证窗口聚合
  assert_eq(windows.length(), 2) // 应该有2个完成的窗口
  
  // 验证第一个窗口的聚合结果
  let first_window = windows[0]
  let first_window_avg = TimeWindow::average(first_window)
  let first_window_max = TimeWindow::maximum(first_window)
  let first_window_min = TimeWindow::minimum(first_window)
  
  assert_true(first_window_avg > 45.0 && first_window_avg < 60.0)
  assert_eq(first_window_max, 61.3)
  assert_eq(first_window_min, 45.2)
  
  // 验证第二个窗口的聚合结果
  let second_window = windows[1]
  let second_window_avg = TimeWindow::average(second_window)
  let second_window_max = TimeWindow::maximum(second_window)
  let second_window_min = TimeWindow::minimum(second_window)
  
  assert_true(second_window_avg > 49.0 && second_window_avg < 54.0)
  assert_eq(second_window_max, 53.4)
  assert_eq(second_window_min, 49.2)
}

// 测试3: 流数据异常检测
test "流数据异常检测测试" {
  let stream_processor = StreamProcessor::new()
  let anomaly_detector = AnomalyDetector::new()
  let telemetry_stream = TelemetryStream::new("anomaly.detection")
  
  // 配置异常检测器
  AnomalyDetector::set_threshold(anomaly_detector, "response.time", 2.0) // 2倍标准差
  AnomalyDetector::set_sensitivity(anomaly_detector, "error.rate", 0.1) // 10%阈值
  
  // 注册异常检测器
  StreamProcessor::register_anomaly_detector(stream_processor, anomaly_detector, telemetry_stream)
  
  // 模拟正常数据流
  let normal_events = [
    MetricEvent::new("response.time", 120.5, Timestamp::now()),
    MetricEvent::new("response.time", 115.2, Timestamp::now()),
    MetricEvent::new("response.time", 125.8, Timestamp::now()),
    MetricEvent::new("response.time", 118.3, Timestamp::now()),
    MetricEvent::new("response.time", 122.1, Timestamp::now())
  ]
  
  // 处理正常数据
  for event in normal_events {
    let result = StreamProcessor::process_event(stream_processor, event)
    // 正常数据不应该触发异常
    assert_false(ProcessEvent::is_anomaly(result))
  }
  
  // 模拟异常数据流
  let anomaly_events = [
    MetricEvent::new("response.time", 350.7, Timestamp::now()), // 异常高响应时间
    MetricEvent::new("error.rate", 0.15, Timestamp::now()),     // 异常高错误率
    MetricEvent::new("response.time", 95.2, Timestamp::now()),  // 正常数据
    MetricEvent::new("error.rate", 0.05, Timestamp::now())       // 正常数据
  ]
  
  // 处理异常数据
  let detected_anomalies = []
  for event in anomaly_events {
    let result = StreamProcessor::process_event(stream_processor, event)
    if ProcessEvent::is_anomaly(result) {
      detected_anomalies = detected_anomalies.push(result)
    }
  }
  
  // 验证异常检测结果
  assert_eq(detected_anomalies.length(), 2) // 应该检测到2个异常
  
  // 验证异常详情
  let first_anomaly = detected_anomalies[0]
  assert_eq(MetricEvent::name(ProcessEvent::original_event(first_anomaly)), "response.time")
  assert_eq(MetricEvent::value(ProcessEvent::original_event(first_anomaly)), 350.7)
  
  let second_anomaly = detected_anomalies[1]
  assert_eq(MetricEvent::name(ProcessEvent::original_event(second_anomaly)), "error.rate")
  assert_eq(MetricEvent::value(ProcessEvent::original_event(second_anomaly)), 0.15)
}

// 测试4: 流数据实时警报
test "流数据实时警报测试" {
  let stream_processor = StreamProcessor::new()
  let alert_manager = AlertManager::new()
  let telemetry_stream = TelemetryStream::new("realtime.alerts")
  
  // 配置警报规则
  let cpu_alert_rule = AlertRule::new("high.cpu.usage")
  AlertRule::set_condition(cpu_alert_rule, "cpu.usage", "gt", 80.0)
  AlertRule::set_severity(cpu_alert_rule, "warning")
  AlertRule::set_message(cpu_alert_rule, "CPU usage exceeded 80%")
  
  let memory_alert_rule = AlertRule::new("high.memory.usage")
  AlertRule::set_condition(memory_alert_rule, "memory.usage", "gt", 90.0)
  AlertRule::set_severity(memory_alert_rule, "critical")
  AlertRule::set_message(memory_alert_rule, "Memory usage exceeded 90%")
  
  // 注册警报规则
  AlertManager::register_rule(alert_manager, cpu_alert_rule)
  AlertManager::register_rule(alert_manager, memory_alert_rule)
  
  // 注册警报管理器
  StreamProcessor::register_alert_manager(stream_processor, alert_manager, telemetry_stream)
  
  // 模拟触发警报的数据流
  let alert_events = [
    MetricEvent::new("cpu.usage", 75.2, Timestamp::now()),      // 正常
    MetricEvent::new("memory.usage", 85.3, Timestamp::now()),    // 正常
    MetricEvent::new("cpu.usage", 82.5, Timestamp::now()),      // 触发CPU警报
    MetricEvent::new("memory.usage", 92.1, Timestamp::now()),    // 触发内存警报
    MetricEvent::new("cpu.usage", 78.9, Timestamp::now()),      // 恢复正常
    MetricEvent::new("memory.usage", 88.4, Timestamp::now())     // 恢复正常
  ]
  
  // 处理警报事件
  let triggered_alerts = []
  for event in alert_events {
    let result = StreamProcessor::process_event(stream_processor, event)
    let alerts = ProcessEvent::alerts(result)
    triggered_alerts = triggered_alerts.concat(alerts)
  }
  
  // 验证警报触发
  assert_eq(triggered_alerts.length(), 2) // 应该触发2个警报
  
  // 验证CPU警报
  let cpu_alert = triggered_alerts[0]
  assert_eq(Alert::rule_name(cpu_alert), "high.cpu.usage")
  assert_eq(Alert::severity(cpu_alert), "warning")
  assert_eq(Alert::metric_name(cpu_alert), "cpu.usage")
  assert_eq(Alert::metric_value(cpu_alert), 82.5)
  
  // 验证内存警报
  let memory_alert = triggered_alerts[1]
  assert_eq(Alert::rule_name(memory_alert), "high.memory.usage")
  assert_eq(Alert::severity(memory_alert), "critical")
  assert_eq(Alert::metric_name(memory_alert), "memory.usage")
  assert_eq(Alert::metric_value(memory_alert), 92.1)
}

// 测试5: 流数据过滤和转换
test "流数据过滤和转换测试" {
  let stream_processor = StreamProcessor::new()
  let telemetry_stream = TelemetryStream::new("filter.transform")
  
  // 创建数据过滤器
  let filter = StreamFilter::new()
  StreamFilter::add_condition(filter, "service", "web")          // 只处理web服务
  StreamFilter::add_condition(filter, "environment", "prod")     // 只处理生产环境
  StreamFilter::add_numeric_condition(filter, "response.time", "lt", 1000.0) // 只处理响应时间小于1秒的
  
  // 创建数据转换器
  let transformer = StreamTransformer::new()
  StreamTransformer::add_mapping(transformer, "response.time", "response.time.ms")
  StreamTransformer::add_unit_conversion(transformer, "cpu.usage", "percent", "ratio", 0.01)
  StreamTransformer::add_tag_enrichment(transformer, "region", "us-west-2")
  
  // 注册过滤器和转换器
  StreamProcessor::register_filter(stream_processor, filter, telemetry_stream)
  StreamProcessor::register_transformer(stream_processor, transformer, telemetry_stream)
  
  // 模拟混合数据流
  let mixed_events = [
    MetricEvent::with_attributes("response.time", 850.5, Timestamp::now(), [
      ("service", StringValue("web")),
      ("environment", StringValue("prod"))
    ]),
    MetricEvent::with_attributes("response.time", 1200.0, Timestamp::now(), [
      ("service", StringValue("web")),
      ("environment", StringValue("prod"))
    ]),
    MetricEvent::with_attributes("response.time", 450.2, Timestamp::now(), [
      ("service", StringValue("api")),
      ("environment", StringValue("prod"))
    ]),
    MetricEvent::with_attributes("cpu.usage", 75.5, Timestamp::now(), [
      ("service", StringValue("web")),
      ("environment", StringValue("prod"))
    ]),
    MetricEvent::with_attributes("response.time", 650.3, Timestamp::now(), [
      ("service", StringValue("web")),
      ("environment", StringValue("staging"))
    ])
  ]
  
  // 处理混合数据
  let processed_events = []
  for event in mixed_events {
    let result = StreamProcessor::process_event(stream_processor, event)
    if ProcessEvent::is_passed_filter(result) {
      processed_events = processed_events.push(ProcessEvent::transformed_event(result))
    }
  }
  
  // 验证过滤结果
  assert_eq(processed_events.length(), 2) // 只有2个事件通过所有过滤条件
  
  // 验证转换结果
  let first_event = processed_events[0]
  assert_eq(MetricEvent::name(first_event), "response.time.ms") // 名称已转换
  assert_eq(MetricEvent::value(first_event), 850.5) // 值保持不变
  
  let second_event = processed_events[1]
  assert_eq(MetricEvent::name(second_event), "cpu.usage") // 名称保持不变
  assert_eq(MetricEvent::value(second_event), 0.755) // 值已转换（75.5% -> 0.755 ratio）
  
  // 验证标签丰富
  let first_event_attrs = MetricEvent::attributes(first_event)
  let region_attr = Attributes::get(first_event_attrs, "region")
  match region_attr {
    Some(StringValue(region)) => assert_eq(region, "us-west-2")
    _ => assert_true(false)
  }
}

// 测试6: 流数据背压处理
test "流数据背压处理测试" {
  let stream_processor = StreamProcessor::new()
  let telemetry_stream = TelemetryStream::new("backpressure.handling")
  
  // 配置背压处理策略
  let backpressure_strategy = BackpressureStrategy::buffer(1000) // 缓冲1000个事件
  StreamProcessor::set_backpressure_strategy(stream_processor, backpressure_strategy)
  
  // 模拟高吞吐量数据流
  let high_volume_events = []
  for i in 1..=1500 {
    let event = MetricEvent::new("high.volume.metric", i.to_float(), Timestamp::now())
    high_volume_events = high_volume_events.push(event)
  }
  
  // 处理高吞吐量数据
  let processed_count = 0
  let dropped_count = 0
  
  for event in high_volume_events {
    let result = StreamProcessor::process_event(stream_processor, event)
    if ProcessEvent::is_processed(result) {
      processed_count = processed_count + 1
    } else if ProcessEvent::is_dropped(result) {
      dropped_count = dropped_count + 1
    }
  }
  
  // 验证背压处理结果
  assert_eq(processed_count, 1000) // 缓冲区大小
  assert_eq(dropped_count, 500)    // 超出缓冲区的事件被丢弃
  
  // 测试背压恢复
  let recovery_strategy = BackpressureStrategy::drop_oldest(100) // 丢弃最旧的100个事件
  StreamProcessor::set_backpressure_strategy(stream_processor, recovery_strategy)
  
  // 模拟背压恢复场景
  let recovery_events = []
  for i in 1..=200 {
    let event = MetricEvent::new("recovery.metric", i.to_float(), Timestamp::now())
    recovery_events = recovery_events.push(event)
  }
  
  // 处理恢复数据
  let recovery_processed_count = 0
  for event in recovery_events {
    let result = StreamProcessor::process_event(stream_processor, event)
    if ProcessEvent::is_processed(result) {
      recovery_processed_count = recovery_processed_count + 1
    }
  }
  
  // 验证背压恢复结果
  assert_eq(recovery_processed_count, 100) // 应该处理最新的100个事件
}

// 测试7: 流数据状态管理
test "流数据状态管理测试" {
  let stream_processor = StreamProcessor::new()
  let state_manager = StateManager::new()
  let telemetry_stream = TelemetryStream::new("state.management")
  
  // 注册状态管理器
  StreamProcessor::register_state_manager(stream_processor, state_manager, telemetry_stream)
  
  // 定义状态键
  let cpu_usage_key = StateKey::new("cpu.usage.history")
  let error_count_key = StateKey::new("error.count")
  let service_health_key = StateKey::new("service.health")
  
  // 初始化状态
  StateManager::set_state(state_manager, cpu_usage_key, [])
  StateManager::set_state(state_manager, error_count_key, 0)
  StateManager::set_state(state_manager, service_health_key, "healthy")
  
  // 模拟状态变化事件流
  let state_events = [
    MetricEvent::new("cpu.usage", 45.2, Timestamp::now()),
    MetricEvent::new("error.count", 1, Timestamp::now()),
    MetricEvent::new("cpu.usage", 52.8, Timestamp::now()),
    MetricEvent::new("error.count", 1, Timestamp::now()),
    MetricEvent::new("cpu.usage", 78.5, Timestamp::now()),
    MetricEvent::new("error.count", 3, Timestamp::now()),
    MetricEvent::new("cpu.usage", 35.1, Timestamp::now())
  ]
  
  // 处理状态变化事件
  for event in state_events {
    let result = StreamProcessor::process_event_with_state(stream_processor, event)
    
    // 更新状态
    match MetricEvent::name(event) {
      "cpu.usage" => {
        let history = StateManager::get_state(state_manager, cpu_usage_key)
        match history {
          Some(ArrayFloatValue(values)) => {
            let new_values = values.push(MetricEvent::value(event))
            StateManager::set_state(state_manager, cpu_usage_key, new_values)
          }
          _ => assert_true(false)
        }
      }
      "error.count" => {
        let count = StateManager::get_state(state_manager, error_count_key)
        match count {
          Some(IntValue(current)) => {
            let new_count = current + MetricEvent::value(event).to_int()
            StateManager::set_state(state_manager, error_count_key, new_count)
            
            // 根据错误数更新服务健康状态
            let new_health = if new_count > 5 { "unhealthy" } else if new_count > 2 { "degraded" } else { "healthy" }
            StateManager::set_state(state_manager, service_health_key, new_health)
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 验证最终状态
  let final_cpu_history = StateManager::get_state(state_manager, cpu_usage_key)
  match final_cpu_history {
    Some(ArrayFloatValue(values)) => {
      assert_eq(values.length(), 4)
      assert_eq(values[0], 45.2)
      assert_eq(values[3], 35.1)
    }
    _ => assert_true(false)
  }
  
  let final_error_count = StateManager::get_state(state_manager, error_count_key)
  match final_error_count {
    Some(IntValue(count)) => assert_eq(count, 5)
    _ => assert_true(false)
  }
  
  let final_service_health = StateManager::get_state(state_manager, service_health_key)
  match final_service_health {
    Some(StringValue(health)) => assert_eq(health, "degraded")
    _ => assert_true(false)
  }
}

// 测试8: 流数据多路复用和分发
test "流数据多路复用和分发测试" {
  let stream_processor = StreamProcessor::new()
  let telemetry_stream = TelemetryStream::new("multiplex.distribution")
  
  // 创建多个输出流
  let metrics_stream = TelemetryStream::new("metrics.output")
  let alerts_stream = TelemetryStream::new("alerts.output")
  let archive_stream = TelemetryStream::new("archive.output")
  
  // 配置流分发规则
  let distribution_rules = [
    DistributionRule::new("metrics", fn(event) {
      MetricEvent::name(event).starts_with("metric.")
    }, metrics_stream),
    DistributionRule::new("alerts", fn(event) {
      MetricEvent::name(event).starts_with("alert.") || MetricEvent::value(event) > 100.0
    }, alerts_stream),
    DistributionRule::new("archive", fn(event) {
      true // 所有事件都归档
    }, archive_stream)
  ]
  
  // 注册分发规则
  for rule in distribution_rules {
    StreamProcessor::register_distribution_rule(stream_processor, rule, telemetry_stream)
  }
  
  // 模拟混合事件流
  let mixed_events = [
    MetricEvent::new("metric.cpu.usage", 45.2, Timestamp::now()),
    MetricEvent::new("alert.high.memory", 95.5, Timestamp::now()),
    MetricEvent::new("metric.disk.io", 120.8, Timestamp::now()),
    MetricEvent::new("log.application.error", 1, Timestamp::now()),
    MetricEvent::new("metric.network.throughput", 1024.5, Timestamp::now()),
    MetricEvent::new("custom.event", 150.0, Timestamp::now())
  ]
  
  // 处理混合事件
  for event in mixed_events {
    StreamProcessor::process_event(stream_processor, event)
  }
  
  // 验证流分发结果
  let metrics_output = TelemetryStream::get_events(metrics_stream)
  let alerts_output = TelemetryStream::get_events(alerts_stream)
  let archive_output = TelemetryStream::get_events(archive_stream)
  
  // 验证metrics流
  assert_eq(metrics_output.length(), 3) // metric.cpu.usage, metric.disk.io, metric.network.throughput
  
  // 验证alerts流
  assert_eq(alerts_output.length(), 3) // alert.high.memory, metric.disk.io (value > 100), custom.event (value > 100)
  
  // 验证archive流
  assert_eq(archive_output.length(), 6) // 所有事件
  
  // 验证事件内容一致性
  for event in metrics_output {
    assert_true(MetricEvent::name(event).starts_with("metric."))
  }
  
  for event in alerts_output {
    let is_alert = MetricEvent::name(event).starts_with("alert.") || MetricEvent::value(event) > 100.0
    assert_true(is_alert)
  }
}