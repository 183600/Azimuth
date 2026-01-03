// Azimuth Telemetry System - High Quality Real-time Stream Processing Tests
// 实时遥测数据流处理测试

test "实时数据流处理" {
  // 创建实时数据流处理器
  let stream_processor = azimuth::StreamProcessor::new()
  
  // 创建测试数据流
  let mut telemetry_events = []
  let base_timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 生成1000个遥测事件
  for i in 0..1000 {
    let event = azimuth::TelemetryEvent::new(
      "event-" + i.to_string(),
      base_timestamp + (i * 100L), // 每100ms一个事件
      azimuth::Attributes::new()
    )
    
    // 添加事件属性
    let attrs = azimuth::TelemetryEvent::attributes(event)
    azimuth::Attributes::set(attrs, "event.id", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "event.type", azimuth::StringValue("telemetry"))
    azimuth::Attributes::set(attrs, "source.service", azimuth::StringValue("service-" + (i % 10).to_string()))
    azimuth::Attributes::set(attrs, "metric.value", azimuth::FloatValue(i * 1.5))
    
    telemetry_events = telemetry_events + [event]
  }
  
  // 验证事件数量
  assert_eq(telemetry_events.length(), 1000)
  
  // 处理数据流
  let processed_events = azimuth::StreamProcessor::process(stream_processor, telemetry_events)
  
  // 验证处理后的事件
  assert_eq(processed_events.length(), 1000)
  
  // 验证第一个事件
  let first_event = processed_events[0]
  assert_eq(azimuth::TelemetryEvent::name(first_event), "event-0")
  assert_eq(azimuth::TelemetryEvent::timestamp(first_event), base_timestamp)
  
  let first_attrs = azimuth::TelemetryEvent::attributes(first_event)
  let first_event_id = azimuth::Attributes::get(first_attrs, "event.id")
  match first_event_id {
    Some(azimuth::IntValue(id)) => assert_eq(id, 0)
    _ => assert_true(false)
  }
  
  // 验证最后一个事件
  let last_event = processed_events[999]
  assert_eq(azimuth::TelemetryEvent::name(last_event), "event-999")
  assert_eq(azimuth::TelemetryEvent::timestamp(last_event), base_timestamp + 99900L)
  
  let last_attrs = azimuth::TelemetryEvent::attributes(last_event)
  let last_event_id = azimuth::Attributes::get(last_attrs, "event.id")
  match last_event_id {
    Some(azimuth::IntValue(id)) => assert_eq(id, 999)
    _ => assert_true(false)
  }
}

test "实时窗口聚合" {
  // 创建窗口聚合器
  let window_aggregator = azimuth::WindowAggregator::new(azimuth::TimeWindow::Second)
  
  // 创建时间序列数据
  let mut metrics = []
  let base_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 生成10秒的数据，每秒100个数据点
  for second in 0..10 {
    for millisecond in 0..100 {
      let timestamp = base_time + (second * 1000L) + (millisecond * 10L)
      let metric = azimuth::Metric::new(
        "cpu.usage",
        azimuth::FloatValue(50.0 + millisecond * 0.5),
        timestamp,
        azimuth::Attributes::new()
      )
      
      let attrs = azimuth::Metric::attributes(metric)
      azimuth::Attributes::set(attrs, "host.name", azimuth::StringValue("host-" + (second % 3).to_string()))
      azimuth::Attributes::set(attrs, "service.name", azimuth::StringValue("web-server"))
      
      metrics = metrics + [metric]
    }
  }
  
  // 验证度量数量
  assert_eq(metrics.length(), 1000)
  
  // 执行窗口聚合
  let aggregated_windows = azimuth::WindowAggregator::aggregate(window_aggregator, metrics)
  
  // 验证聚合结果
  assert_eq(aggregated_windows.length(), 10) // 10个时间窗口
  
  // 验证第一个窗口的聚合结果
  let first_window = aggregated_windows[0]
  assert_eq(azimuth::Window::start_time(first_window), base_time)
  assert_eq(azimuth::Window::end_time(first_window), base_time + 1000L)
  assert_eq(azimuth::Window::count(first_window), 100)
  
  // 验证第一个窗口的平均值
  let first_avg = azimuth::Window::average(first_window)
  assert_true(first_avg > 50.0 && first_avg < 100.0)
  
  // 验证最后一个窗口的聚合结果
  let last_window = aggregated_windows[9]
  assert_eq(azimuth::Window::start_time(last_window), base_time + 9000L)
  assert_eq(azimuth::Window::end_time(last_window), base_time + 10000L)
  assert_eq(azimuth::Window::count(last_window), 100)
}

test "实时异常检测" {
  // 创建异常检测器
  let anomaly_detector = azimuth::AnomalyDetector::new()
  
  // 创建正常数据
  let mut normal_metrics = []
  let base_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 生成100个正常数据点（CPU使用率在20-80之间）
  for i in 0..100 {
    let metric = azimuth::Metric::new(
      "cpu.usage",
      azimuth::FloatValue(20.0 + (i % 60)), // 20-80之间的值
      base_time + (i * 1000L),
      azimuth::Attributes::new()
    )
    
    let attrs = azimuth::Metric::attributes(metric)
    azimuth::Attributes::set(attrs, "host.name", azimuth::StringValue("production-server"))
    azimuth::Attributes::set(attrs, "service.name", azimuth::StringValue("api-service"))
    
    normal_metrics = normal_metrics + [metric]
  }
  
  // 训练异常检测模型
  azimuth::AnomalyDetector::train(anomaly_detector, normal_metrics)
  
  // 创建测试数据（包含异常）
  let mut test_metrics = []
  
  // 添加正常数据
  for i in 0..50 {
    let metric = azimuth::Metric::new(
      "cpu.usage",
      azimuth::FloatValue(30.0 + (i % 40)), // 30-70之间的值
      base_time + 100000L + (i * 1000L),
      azimuth::Attributes::new()
    )
    
    let attrs = azimuth::Metric::attributes(metric)
    azimuth::Attributes::set(attrs, "host.name", azimuth::StringValue("production-server"))
    azimuth::Attributes::set(attrs, "service.name", azimuth::StringValue("api-service"))
    
    test_metrics = test_metrics + [metric]
  }
  
  // 添加异常数据（CPU使用率突然飙升到95）
  let anomaly_metric = azimuth::Metric::new(
    "cpu.usage",
    azimuth::FloatValue(95.0), // 异常高值
    base_time + 150000L,
    azimuth::Attributes::new()
  )
  
  let anomaly_attrs = azimuth::Metric::attributes(anomaly_metric)
  azimuth::Attributes::set(anomaly_attrs, "host.name", azimuth::StringValue("production-server"))
  azimuth::Attributes::set(anomaly_attrs, "service.name", azimuth::StringValue("api-service"))
  
  test_metrics = test_metrics + [anomaly_metric]
  
  // 检测异常
  let anomalies = azimuth::AnomalyDetector::detect(anomaly_detector, test_metrics)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 1)
  
  let detected_anomaly = anomalies[0]
  assert_eq(azimuth::Anomaly::metric_name(detected_anomaly), "cpu.usage")
  assert_eq(azimuth::Anomaly::timestamp(detected_anomaly), base_time + 150000L)
  assert_eq(azimuth::Anomaly::anomaly_score(detected_anomaly), 95.0)
  assert_true(azimuth::Anomaly::is_anomaly(detected_anomaly))
}

test "实时流式度量计算" {
  // 创建流式度量计算器
  let streaming_calculator = azimuth::StreamingCalculator::new()
  
  // 创建流式数据
  let mut stream_data = []
  let base_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 生成响应时间数据流
  for i in 0..500 {
    let response_time = 100.0 + (i % 200) + (i * 0.1) // 逐渐增加的响应时间
    let metric = azimuth::Metric::new(
      "http.request.duration",
      azimuth::FloatValue(response_time),
      base_time + (i * 200L),
      azimuth::Attributes::new()
    )
    
    let attrs = azimuth::Metric::attributes(metric)
    azimuth::Attributes::set(attrs, "endpoint", azimuth::StringValue("/api/users"))
    azimuth::Attributes::set(attrs, "method", azimuth::StringValue("GET"))
    azimuth::Attributes::set(attrs, "status.code", azimuth::IntValue(200))
    
    stream_data = stream_data + [metric]
  }
  
  // 验证数据流
  assert_eq(stream_data.length(), 500)
  
  // 计算流式统计
  let streaming_stats = azimuth::StreamingCalculator::calculate(streaming_calculator, stream_data)
  
  // 验证统计结果
  assert_eq(azimuth::StreamingStats::count(streaming_stats), 500)
  
  // 验证最小值
  let min_value = azimuth::StreamingStats::min(streaming_stats)
  assert_eq(min_value, 100.0)
  
  // 验证最大值
  let max_value = azimuth::StreamingStats::max(streaming_stats)
  assert_eq(max_value, 100.0 + 199 + 49.9) // 最后一个值
  
  // 验证平均值
  let avg_value = azimuth::StreamingStats::average(streaming_stats)
  assert_true(avg_value > 100.0 && avg_value < 300.0)
  
  // 验证百分位数
  let p50 = azimuth::StreamingStats::percentile(streaming_stats, 50.0)
  let p95 = azimuth::StreamingStats::percentile(streaming_stats, 95.0)
  let p99 = azimuth::StreamingStats::percentile(streaming_stats, 99.0)
  
  assert_true(p50 > p50 * 0.8 && p50 < p50 * 1.2)
  assert_true(p95 > p50 && p95 < max_value)
  assert_true(p99 > p95 && p99 <= max_value)
}

test "实时数据过滤和转换" {
  // 创建数据过滤器
  let data_filter = azimuth::DataFilter::new()
  let data_transformer = azimuth::DataTransformer::new()
  
  // 创建混合数据流
  let mut mixed_data = []
  let base_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 生成不同类型的数据
  for i in 0..200 {
    let service_type = if i % 3 == 0 { "web" } else if i % 3 == 1 { "database" } else { "cache" }
    let metric_value = if i % 5 == 0 { 0.0 } else { i * 2.5 }
    
    let metric = azimuth::Metric::new(
      "service.metric",
      azimuth::FloatValue(metric_value),
      base_time + (i * 500L),
      azimuth::Attributes::new()
    )
    
    let attrs = azimuth::Metric::attributes(metric)
    azimuth::Attributes::set(attrs, "service.type", azimuth::StringValue(service_type))
    azimuth::Attributes::set(attrs, "service.instance", azimuth::StringValue("instance-" + (i % 5).to_string()))
    azimuth::Attributes::set(attrs, "metric.value", azimuth::FloatValue(metric_value))
    
    mixed_data = mixed_data + [metric]
  }
  
  // 验证混合数据
  assert_eq(mixed_data.length(), 200)
  
  // 过滤数据：只保留web服务和非零值
  let filter_criteria = azimuth::FilterCriteria::new()
  azimuth::FilterCriteria::add_condition(filter_criteria, "service.type", azimuth::StringValue("web"))
  azimuth::FilterCriteria::add_range_condition(filter_criteria, "metric.value", 0.1, 1000.0)
  
  let filtered_data = azimuth::DataFilter::filter(data_filter, mixed_data, filter_criteria)
  
  // 验证过滤结果
  assert_true(filtered_data.length() < 200)
  assert_true(filtered_data.length() > 0)
  
  // 验证所有过滤结果都是web服务
  for metric in filtered_data {
    let attrs = azimuth::Metric::attributes(metric)
    let service_type = azimuth::Attributes::get(attrs, "service.type")
    match service_type {
      Some(azimuth::StringValue(service)) => assert_eq(service, "web")
      _ => assert_true(false)
    }
    
    // 验证所有值都大于0
    let metric_value = azimuth::Attributes::get(attrs, "metric.value")
    match metric_value {
      Some(azimuth::FloatValue(value)) => assert_true(value > 0.0)
      _ => assert_true(false)
    }
  }
  
  // 转换数据：将值标准化到0-1范围
  let transformed_data = azimuth::DataTransformer::normalize(data_transformer, filtered_data, "metric.value")
  
  // 验证转换结果
  assert_eq(transformed_data.length(), filtered_data.length())
  
  // 验证所有值都在0-1范围内
  for metric in transformed_data {
    let attrs = azimuth::Metric::attributes(metric)
    let normalized_value = azimuth::Attributes::get(attrs, "metric.value.normalized")
    match normalized_value {
      Some(azimuth::FloatValue(value)) => {
        assert_true(value >= 0.0)
        assert_true(value <= 1.0)
      }
      _ => assert_true(false)
    }
  }
}