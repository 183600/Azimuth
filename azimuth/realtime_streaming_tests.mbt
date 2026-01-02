// Real-time Streaming Tests
// 验证遥测系统的实时流处理能力

test "real-time metrics streaming" {
  // 创建实时指标流处理器
  let stream_processor = azimuth::RealtimeStreamProcessor::new()
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "realtime-meter")
  let counter = azimuth::Meter::create_counter(meter, "realtime.requests")
  
  // 设置实时流处理
  let metrics_stream = azimuth::RealtimeStreamProcessor::create_metrics_stream(stream_processor, counter)
  
  // 模拟实时指标数据流
  let metrics_generated = 0
  let metrics_processed = 0
  let start_time = azimuth::Clock::now()
  let duration_ms = 1000 // 1秒的测试
  
  while (azimuth::Clock::now() - start_time < duration_ms * 1000000) {
    // 生成指标数据
    azimuth::Counter::add(counter, 1, [{"endpoint", azimuth::StringValue("/api/realtime")}])
    metrics_generated += 1
    
    // 处理流数据
    let processed = azimuth::RealtimeStreamProcessor::process_metrics_batch(stream_processor, metrics_stream)
    metrics_processed += processed
    
    // 模拟实时延迟
    azimuth::Clock::sleep(10) // 10ms延迟
  }
  
  // 验证实时流处理
  assert_true(metrics_generated > 0)
  assert_true(metrics_processed > 0)
  assert_true(metrics_processed <= metrics_generated)
  
  // 获取实时统计
  let stats = azimuth::RealtimeStreamProcessor::get_stream_stats(stream_processor)
  assert_true(azimuth::StreamStats::throughput(stats) > 0)
  assert_true(azimuth::StreamStats::latency_avg(stats) >= 0)
}

test "real-time log streaming" {
  // 创建实时日志流处理器
  let stream_processor = azimuth::RealtimeStreamProcessor::new()
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "realtime-logger")
  
  // 设置实时日志流处理
  let log_stream = azimuth::RealtimeStreamProcessor::create_log_stream(stream_processor, logger)
  
  // 模拟实时日志数据流
  let logs_generated = 0
  let logs_processed = 0
  let start_time = azimuth::Clock::now()
  let duration_ms = 1000 // 1秒的测试
  
  while (azimuth::Clock::now() - start_time < duration_ms * 1000000) {
    // 生成日志数据
    let log_record = azimuth::Logger::create_log_record(logger)
    azimuth::LogRecord::set_body(log_record, "Real-time log message " + logs_generated.to_string())
    azimuth::LogRecord::set_severity(log_record, azimuth::INFO)
    azimuth::LogRecord::set_attribute(log_record, "timestamp", azimuth::IntValue(azimuth::Clock::now()))
    logs_generated += 1
    
    // 处理流数据
    let processed = azimuth::RealtimeStreamProcessor::process_log_batch(stream_processor, log_stream)
    logs_processed += processed
    
    // 模拟实时延迟
    azimuth::Clock::sleep(5) // 5ms延迟
  }
  
  // 验证实时日志流处理
  assert_true(logs_generated > 0)
  assert_true(logs_processed > 0)
  assert_true(logs_processed <= logs_generated)
  
  // 获取实时统计
  let stats = azimuth::RealtimeStreamProcessor::get_log_stream_stats(stream_processor)
  assert_true(azimuth::LogStreamStats::messages_per_second(stats) > 0)
}

test "real-time trace streaming" {
  // 创建实时追踪流处理器
  let stream_processor = azimuth::RealtimeStreamProcessor::new()
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(provider, "realtime-tracer")
  
  // 设置实时追踪流处理
  let trace_stream = azimuth::RealtimeStreamProcessor::create_trace_stream(stream_processor, tracer)
  
  // 模拟实时追踪数据流
  let spans_generated = 0
  let spans_processed = 0
  let start_time = azimuth::Clock::now()
  let duration_ms = 1000 // 1秒的测试
  
  while (azimuth::Clock::now() - start_time < duration_ms * 1000000) {
    // 生成span数据
    let span_ctx = azimuth::SpanContext::new("realtime-trace", "span-" + spans_generated.to_string(), true, "")
    let span = azimuth::Tracer::start_span(tracer, "realtime-operation", span_ctx)
    azimuth::Span::set_attribute(span, "operation.id", azimuth::IntValue(spans_generated))
    azimuth::Span::set_attribute(span, "start.time", azimuth::IntValue(azimuth::Clock::now()))
    spans_generated += 1
    
    // 处理流数据
    let processed = azimuth::RealtimeStreamProcessor::process_trace_batch(stream_processor, trace_stream)
    spans_processed += processed
    
    // 模拟实时延迟
    azimuth::Clock::sleep(15) // 15ms延迟
  }
  
  // 验证实时追踪流处理
  assert_true(spans_generated > 0)
  assert_true(spans_processed > 0)
  assert_true(spans_processed <= spans_generated)
  
  // 获取实时统计
  let stats = azimuth::RealtimeStreamProcessor::get_trace_stream_stats(stream_processor)
  assert_true(azimuth::TraceStreamStats::spans_per_second(stats) > 0)
}

test "real-time data aggregation" {
  // 创建实时数据聚合器
  let aggregator = azimuth::RealtimeAggregator::new()
  
  // 设置聚合窗口（1秒窗口）
  azimuth::RealtimeAggregator::set_window_size(aggregator, 1000) // 1秒
  
  // 模拟实时数据流
  let data_points = []
  let start_time = azimuth::Clock::now()
  let duration_ms = 3000 // 3秒的测试
  
  while (azimuth::Clock::now() - start_time < duration_ms * 1000000) {
    // 生成数据点
    let timestamp = azimuth::Clock::now()
    let value = (timestamp % 1000).to_double() // 0-999的值
    data_points.push((timestamp, value))
    
    // 添加到聚合器
    azimuth::RealtimeAggregator::add_data_point(aggregator, timestamp, value)
    
    // 模拟实时延迟
    azimuth::Clock::sleep(50) // 50ms延迟
  }
  
  // 获取聚合结果
  let aggregations = azimuth::RealtimeAggregator::get_aggregations(aggregator)
  assert_true(aggregations.length > 0)
  
  // 验证聚合结果
  for aggregation in aggregations {
    let window_start = azimuth::Aggregation::window_start(aggregation)
    let window_end = azimuth::Aggregation::window_end(aggregation)
    let count = azimuth::Aggregation::count(aggregation)
    let sum = azimuth::Aggregation::sum(aggregation)
    let avg = azimuth::Aggregation::average(aggregation)
    let min = azimuth::Aggregation::min(aggregation)
    let max = azimuth::Aggregation::max(aggregation)
    
    assert_true(window_end > window_start)
    assert_true(count > 0)
    assert_true(sum >= 0.0)
    assert_true(avg >= 0.0)
    assert_true(min >= 0.0)
    assert_true(max >= min)
  }
}

test "real-time alerting" {
  // 创建实时警报系统
  let alerting_system = azimuth::RealtimeAlerting::new()
  
  // 设置警报规则
  let error_rate_rule = azimuth::AlertRule::new("error.rate", ">", 0.1) // 错误率超过10%
  let latency_rule = azimuth::AlertRule::new("latency.p95", ">", 1000) // P95延迟超过1秒
  let throughput_rule = azimuth::AlertRule::new("throughput", "<", 100) // 吞吐量低于100/秒
  
  azimuth::RealtimeAlerting::add_rule(alerting_system, error_rate_rule)
  azimuth::RealtimeAlerting::add_rule(alerting_system, latency_rule)
  azimuth::RealtimeAlerting::add_rule(alerting_system, throughput_rule)
  
  // 模拟实时监控数据
  let alerts_triggered = 0
  let start_time = azimuth::Clock::now()
  let duration_ms = 2000 // 2秒的测试
  
  while (azimuth::Clock::now() - start_time < duration_ms * 1000000) {
    // 生成监控指标
    let error_rate = 0.15 // 高错误率
    let latency_p95 = 1200.0 // 高延迟
    let throughput = 80 // 低吞吐量
    
    // 检查警报条件
    let alerts = azimuth::RealtimeAlerting::check_rules(alerting_system, [
      ("error.rate", error_rate),
      ("latency.p95", latency_p95),
      ("throughput", throughput)
    ])
    
    alerts_triggered += alerts.length
    
    // 模拟实时延迟
    azimuth::Clock::sleep(100) // 100ms延迟
  }
  
  // 验证警报触发
  assert_true(alerts_triggered > 0)
  
  // 获取警报统计
  let alert_stats = azimuth::RealtimeAlerting::get_stats(alerting_system)
  assert_true(azimuth::AlertStats::total_alerts(alert_stats) > 0)
  assert_true(azimuth::AlertStats::unique_rules_triggered(alert_stats) > 0)
}

test "real-time dashboard data" {
  // 创建实时仪表板数据提供者
  let dashboard_provider = azimuth::RealtimeDashboardProvider::new()
  
  // 设置仪表板组件
  azimuth::RealtimeDashboardProvider::add_timeseries_chart(dashboard_provider, "request.rate")
  azimuth::RealtimeDashboardProvider::add_gauge(dashboard_provider, "cpu.usage")
  azimuth::RealtimeDashboardProvider::add_heatmap(dashboard_provider, "response.latency")
  
  // 模拟实时数据更新
  let updates_generated = 0
  let start_time = azimuth::Clock::now()
  let duration_ms = 1500 // 1.5秒的测试
  
  while (azimuth::Clock::now() - start_time < duration_ms * 1000000) {
    let timestamp = azimuth::Clock::now()
    
    // 更新时间序列数据
    let request_rate = 100.0 + (timestamp % 1000).to_double() / 10.0
    azimuth::RealtimeDashboardProvider::update_timeseries(dashboard_provider, "request.rate", timestamp, request_rate)
    
    // 更新仪表数据
    let cpu_usage = 50.0 + (timestamp % 1000).to_double() / 20.0
    azimuth::RealtimeDashboardProvider::update_gauge(dashboard_provider, "cpu.usage", cpu_usage)
    
    // 更新热图数据
    let latency = 100.0 + (timestamp % 1000).to_double() / 5.0
    let endpoint = "/api/endpoint" + ((timestamp % 5) + 1).to_string()
    azimuth::RealtimeDashboardProvider::update_heatmap(dashboard_provider, "response.latency", endpoint, latency)
    
    updates_generated += 1
    
    // 模拟实时延迟
    azimuth::Clock::sleep(75) // 75ms延迟
  }
  
  // 验证仪表板数据
  let timeseries_data = azimuth::RealtimeDashboardProvider::get_timeseries_data(dashboard_provider, "request.rate")
  let gauge_data = azimuth::RealtimeDashboardProvider::get_gauge_data(dashboard_provider, "cpu.usage")
  let heatmap_data = azimuth::RealtimeDashboardProvider::get_heatmap_data(dashboard_provider, "response.latency")
  
  assert_true(timeseries_data.length > 0)
  assert_true(gauge_data.length > 0)
  assert_true(heatmap_data.length > 0)
  
  // 验证数据点数量
  assert_true(timeseries_data.length <= updates_generated)
  assert_true(gauge_data.length <= updates_generated)
  
  // 获取仪表板快照
  let snapshot = azimuth::RealtimeDashboardProvider::create_snapshot(dashboard_provider)
  assert_true(azimuth::DashboardSnapshot::component_count(snapshot) > 0)
  assert_true(azimuth::DashboardSnapshot::data_point_count(snapshot) > 0)
}