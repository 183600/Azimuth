// 实时仪表板聚合测试
// 测试遥测系统的实时数据聚合和仪表板功能

import "azimuth/azimuth"

pub test "实时度量聚合基础测试" {
  let dashboard = azimuth::RealtimeDashboard::new("test-dashboard")
  
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "dashboard-meter")
  let counter = azimuth::Meter::create_counter(meter, "realtime.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "realtime.histogram")
  let gauge = azimuth::Meter::create_gauge(meter, "realtime.gauge")
  
  // 创建实时数据流
  let data_stream = azimuth::RealtimeDataStream::new()
  
  // 生成实时度量数据
  for i in 0..100 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double() * 0.1)
    azimuth::Gauge::set(gauge, (i % 20).to_double())
    
    // 添加到数据流
    azimuth::RealtimeDataStream::add_metric(data_stream, "counter", 1.0)
    azimuth::RealtimeDataStream::add_metric(data_stream, "histogram", i.to_double() * 0.1)
    azimuth::RealtimeDataStream::add_metric(data_stream, "gauge", (i % 20).to_double())
  }
  
  // 聚合数据
  let aggregated_data = azimuth::RealtimeDashboard::aggregate_metrics(dashboard, data_stream)
  
  // 验证聚合结果
  assert_eq(azimuth::AggregatedData::metric_count(aggregated_data, "counter"), 100)
  assert_eq(azimuth::AggregatedData::sum(aggregated_data, "counter"), 100.0)
  assert_eq(azimuth::AggregatedData::metric_count(aggregated_data, "histogram"), 100)
  assert_eq(azimuth::AggregatedData::average(aggregated_data, "gauge"), 9.5)  // 0-19的平均值
}

pub test "实时时间序列聚合测试" {
  let dashboard = azimuth::RealtimeDashboard::new("timeseries-dashboard")
  
  // 创建时间序列数据
  let timeseries = azimuth::TimeSeriesData::new()
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 生成时间序列数据点
  for i in 0..200 {
    let timestamp = start_time + (i * 1000000L)  // 每毫秒一个数据点
    let value = 50.0 + (10.0 * (i / 20.0).sin())  // 正弦波数据
    
    azimuth::TimeSeriesData::add_point(timeseries, "cpu.usage", timestamp, value)
    azimuth::TimeSeriesData::add_point(timeseries, "memory.usage", timestamp, 60.0 + (i % 40).to_double())
  }
  
  // 时间窗口聚合
  let windowed_data = azimuth::RealtimeDashboard::aggregate_by_time_window(
    dashboard, 
    timeseries, 
    10000000L  // 10秒窗口
  )
  
  // 验证时间窗口聚合
  let windows = azimuth::WindowedData::windows(windowed_data)
  assert_true(windows.length() > 0)
  
  // 验证每个窗口的数据
  for window in windows {
    let cpu_points = azimuth::TimeWindow::points(window, "cpu.usage")
    let memory_points = azimuth::TimeWindow::points(window, "memory.usage")
    
    assert_true(cpu_points.length() > 0)
    assert_true(memory_points.length() > 0)
    
    // 验证窗口统计
    let cpu_avg = azimuth::TimeWindow::average(window, "cpu.usage")
    let memory_avg = azimuth::TimeWindow::average(window, "memory.usage")
    
    assert_true(cpu_avg >= 40.0 && cpu_avg <= 60.0)  // 正弦波范围
    assert_true(memory_avg >= 60.0 && memory_avg <= 100.0)  // 内存使用范围
  }
}

pub test "实时事件聚合和过滤测试" {
  let dashboard = azimuth::RealtimeDashboard::new("events-dashboard")
  
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "events-tracer")
  
  // 创建事件流
  let event_stream = azimuth::EventStream::new()
  
  // 生成不同类型的事件
  let event_types = ["user.login", "user.logout", "api.request", "error.occurred", "system.startup"]
  let event_severities = [azimuth::Info, azimuth::Warn, azimuth::Error]
  
  for i in 0..500 {
    let event_type = event_types[i % event_types.length()]
    let severity = event_severities[i % event_severities.length()]
    
    let span = azimuth::Tracer::start_span(tracer, "event-span-" + i.to_string())
    
    // 添加事件
    azimuth::Span::add_event(span, event_type, Some([
      ("event.id", azimuth::IntValue(i)),
      ("event.type", azimuth::StringValue(event_type)),
      ("severity", azimuth::StringValue(severity.to_string()))
    ]))
    
    // 添加到事件流
    azimuth::EventStream::add_event(event_stream, event_type, severity, i)
    
    azimuth::Span::end(span)
  }
  
  // 按事件类型聚合
  let events_by_type = azimuth::RealtimeDashboard::aggregate_events_by_type(dashboard, event_stream)
  
  // 验证事件类型聚合
  for event_type in event_types {
    let count = azimuth::EventAggregation::event_count(events_by_type, event_type)
    assert_true(count > 0)
    assert_eq(count, 100)  // 500个事件，5种类型，每种100个
  }
  
  // 按严重程度聚合
  let events_by_severity = azimuth::RealtimeDashboard::aggregate_events_by_severity(dashboard, event_stream)
  
  // 验证严重程度聚合
  let info_count = azimuth::EventAggregation::severity_count(events_by_severity, azimuth::Info)
  let warn_count = azimuth::EventAggregation::severity_count(events_by_severity, azimuth::Warn)
  let error_count = azimuth::EventAggregation::severity_count(events_by_severity, azimuth::Error)
  
  assert_eq(info_count, 167)  // 500 / 3 = 166.67，向上取整
  assert_eq(warn_count, 167)
  assert_eq(error_count, 166)
  
  // 事件过滤测试
  let error_events = azimuth::EventStream::filter_by_severity(event_stream, azimuth::Error)
  let error_count_filtered = azimuth::EventStream::count(error_events)
  assert_eq(error_count_filtered, 166)
  
  let api_events = azimuth::EventStream::filter_by_type(event_stream, "api.request")
  let api_count = azimuth::EventStream::count(api_events)
  assert_eq(api_count, 100)
}

pub test "实时仪表板更新和刷新测试" {
  let dashboard = azimuth::RealtimeDashboard::new("refresh-dashboard")
  
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "refresh-meter")
  let counter = azimuth::Meter::create_counter(meter, "refresh.counter")
  
  // 模拟实时数据更新
  let update_count = 0
  let update_times = []
  
  for cycle in 0..20 {
    let cycle_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 生成新数据
    for i in 0..50 {
      azimuth::Counter::add(counter, 1.0)
      azimuth::RealtimeDashboard::update_metric(dashboard, "refresh.counter", 1.0)
    }
    
    // 刷新仪表板
    azimuth::RealtimeDashboard::refresh(dashboard)
    
    let cycle_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let update_time = cycle_end - cycle_start
    update_times.push(update_time)
    
    update_count = update_count + 1
    
    // 验证仪表板状态
    let current_value = azimuth::RealtimeDashboard::get_metric_value(dashboard, "refresh.counter")
    assert_eq(current_value, (cycle + 1) * 50.0)
  }
  
  // 验证更新性能
  let avg_update_time = update_times.reduce(0L, fn(acc, time) { acc + time }) / update_times.length()
  let max_update_time = update_times.reduce(0L, fn(acc, time) { if (time > acc) time else acc })
  
  assert_true(avg_update_time < 100000000L)    // 平均更新时间小于100ms
  assert_true(max_update_time < 500000000L)    // 最大更新时间小于500ms
  assert_eq(update_count, 20)                  // 验证更新次数
}

pub test "实时仪表板多数据源聚合测试" {
  let dashboard = azimuth::RealtimeDashboard::new("multi-source-dashboard")
  
  // 创建多个数据源
  let meter1 = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "source1-meter")
  let meter2 = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "source2-meter")
  let meter3 = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "source3-meter")
  
  let counter1 = azimuth::Meter::create_counter(meter1, "multi.counter")
  let counter2 = azimuth::Meter::create_counter(meter2, "multi.counter")
  let counter3 = azimuth::Meter::create_counter(meter3, "multi.counter")
  
  // 从多个数据源生成数据
  for i in 0..100 {
    azimuth::Counter::add(counter1, 1.0)
    azimuth::Counter::add(counter2, 2.0)
    azimuth::Counter::add(counter3, 3.0)
    
    // 添加到仪表板
    azimuth::RealtimeDashboard::add_data_source(dashboard, "source1", counter1)
    azimuth::RealtimeDashboard::add_data_source(dashboard, "source2", counter2)
    azimuth::RealtimeDashboard::add_data_source(dashboard, "source3", counter3)
  }
  
  // 跨数据源聚合
  let multi_source_aggregation = azimuth::RealtimeDashboard::aggregate_cross_sources(
    dashboard, 
    "multi.counter"
  )
  
  // 验证跨源聚合结果
  let source1_value = azimuth::MultiSourceAggregation::source_value(multi_source_aggregation, "source1")
  let source2_value = azimuth::MultiSourceAggregation::source_value(multi_source_aggregation, "source2")
  let source3_value = azimuth::MultiSourceAggregation::source_value(multi_source_aggregation, "source3")
  
  assert_eq(source1_value, 100.0)
  assert_eq(source2_value, 200.0)
  assert_eq(source3_value, 300.0)
  
  let total_value = azimuth::MultiSourceAggregation::total_value(multi_source_aggregation)
  assert_eq(total_value, 600.0)
  
  let average_value = azimuth::MultiSourceAggregation::average_value(multi_source_aggregation)
  assert_eq(average_value, 200.0)
}

pub test "实时仪表板异常检测测试" {
  let dashboard = azimuth::RealtimeDashboard::new("anomaly-dashboard")
  
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "anomaly-meter")
  let gauge = azimuth::Meter::create_gauge(meter, "anomaly.gauge")
  
  // 创建异常检测器
  let anomaly_detector = azimuth::AnomalyDetector::new()
  
  // 生成正常数据
  for i in 0..50 {
    let normal_value = 50.0 + (azimuth::Random::next_double() * 10.0)  // 50-60之间的正常值
    azimuth::Gauge::set(gauge, normal_value)
    azimuth::RealtimeDashboard::update_metric(dashboard, "anomaly.gauge", normal_value)
    
    // 训练异常检测器
    azimuth::AnomalyDetector::train(anomaly_detector, normal_value)
  }
  
  // 验证异常检测器训练
  assert_true(azimuth::AnomalyDetector::is_trained(anomaly_detector))
  
  // 生成异常数据
  let anomaly_detected = false
  for i in 0..20 {
    let value = if (i % 5 == 0) { 
      100.0 + (azimuth::Random::next_double() * 20.0)  // 异常值：100-120
    } else { 
      50.0 + (azimuth::Random::next_double() * 10.0)   // 正常值：50-60
    }
    
    azimuth::Gauge::set(gauge, value)
    azimuth::RealtimeDashboard::update_metric(dashboard, "anomaly.gauge", value)
    
    // 检测异常
    let is_anomaly = azimuth::AnomalyDetector::detect(anomaly_detector, value)
    if (is_anomaly) {
      anomaly_detected = true
      azimuth::RealtimeDashboard::add_anomaly_alert(dashboard, "anomaly.gauge", value)
    }
  }
  
  // 验证异常检测
  assert_true(anomaly_detected)
  
  // 验证异常警报
  let anomaly_alerts = azimuth::RealtimeDashboard::get_anomaly_alerts(dashboard)
  assert_true(anomaly_alerts.length() > 0)
  
  // 验证异常统计
  let anomaly_stats = azimuth::RealtimeDashboard::get_anomaly_statistics(dashboard)
  assert_true(azimuth::AnomalyStats::anomaly_count(anomaly_stats) > 0)
  assert_true(azimuth::AnomalyStats::anomaly_rate(anomaly_stats) > 0.0)
}

pub test "实时仪表板性能基准测试" {
  let dashboard = azimuth::RealtimeDashboard::new("performance-dashboard")
  
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "performance-meter")
  let counter = azimuth::Meter::create_counter(meter, "performance.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "performance.histogram")
  
  // 性能基准测试
  let benchmark_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 大量数据更新
  for i in 0..1000 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double())
    
    // 更新仪表板
    azimuth::RealtimeDashboard::update_metric(dashboard, "performance.counter", 1.0)
    azimuth::RealtimeDashboard::update_metric(dashboard, "performance.histogram", i.to_double())
    
    // 每100次刷新一次
    if (i % 100 == 0) {
      azimuth::RealtimeDashboard::refresh(dashboard)
    }
  }
  
  let benchmark_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let benchmark_duration = benchmark_end - benchmark_start
  
  // 验证性能基准
  assert_true(benchmark_duration < 5000000000L)  // 总时间小于5秒
  
  // 验证数据处理能力
  let throughput = 1000.0 / (benchmark_duration.to_double() / 1000000000.0)  // 操作/秒
  assert_true(throughput > 100.0)  // 至少100操作/秒
  
  // 验证内存使用
  let memory_usage = azimuth::RealtimeDashboard::memory_usage(dashboard)
  assert_true(memory_usage < 100 * 1024 * 1024)  // 内存使用小于100MB
  
  // 验证仪表板状态
  let final_counter_value = azimuth::RealtimeDashboard::get_metric_value(dashboard, "performance.counter")
  assert_eq(final_counter_value, 1000.0)
  
  let histogram_stats = azimuth::RealtimeDashboard::get_histogram_stats(dashboard, "performance.histogram")
  assert_eq(azimuth::HistogramStats::count(histogram_stats), 1000)
  assert_eq(azimuth::HistogramStats::sum(histogram_stats), 499500.0)  // 0+1+2+...+999
}