// 度量和数据聚合分析测试
// 测试Azimuth遥测系统的度量和数据聚合分析功能

test "计数器度量和聚合" {
  // 测试计数器度量和聚合功能
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "counter.test")
  
  // 创建计数器
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  
  // 记录不同类型的请求
  Counter::add_with_attributes(request_counter, 10.0, [("method", "GET"), ("status", "200")])
  Counter::add_with_attributes(request_counter, 5.0, [("method", "GET"), ("status", "404")])
  Counter::add_with_attributes(request_counter, 3.0, [("method", "POST"), ("status", "200")])
  Counter::add_with_attributes(request_counter, 2.0, [("method", "POST"), ("status", "400")])
  Counter::add_with_attributes(request_counter, 1.0, [("method", "PUT"), ("status", "200")])
  Counter::add_with_attributes(request_counter, 1.0, [("method", "DELETE"), ("status", "204")])
  
  // 创建聚合器
  let aggregator = MetricAggregator::new()
  
  // 按方法聚合
  let method_aggregation = Aggregator::group_by(aggregator, request_counter, "method")
  let get_count = Aggregation::get_value(method_aggregation, "GET")
  let post_count = Aggregation::get_value(method_aggregation, "POST")
  let put_count = Aggregation::get_value(method_aggregation, "PUT")
  let delete_count = Aggregation::get_value(method_aggregation, "DELETE")
  
  assert_eq(get_count, 15.0)
  assert_eq(post_count, 5.0)
  assert_eq(put_count, 1.0)
  assert_eq(delete_count, 1.0)
  
  // 按状态码聚合
  let status_aggregation = Aggregator::group_by(aggregator, request_counter, "status")
  let success_count = Aggregation::get_value(status_aggregation, "200")
  let not_found_count = Aggregation::get_value(status_aggregation, "404")
  let bad_request_count = Aggregation::get_value(status_aggregation, "400")
  let no_content_count = Aggregation::get_value(status_aggregation, "204")
  
  assert_eq(success_count, 14.0)
  assert_eq(not_found_count, 5.0)
  assert_eq(bad_request_count, 2.0)
  assert_eq(no_content_count, 1.0)
  
  // 多维度聚合
  let multi_dim_aggregation = Aggregator::group_by_multiple(aggregator, request_counter, ["method", "status"])
  let get_success = Aggregation::get_nested_value(multi_dim_aggregation, "GET", "200")
  let get_not_found = Aggregation::get_nested_value(multi_dim_aggregation, "GET", "404")
  let post_success = Aggregation::get_nested_value(multi_dim_aggregation, "POST", "200")
  let post_bad_request = Aggregation::get_nested_value(multi_dim_aggregation, "POST", "400")
  
  assert_eq(get_success, 10.0)
  assert_eq(get_not_found, 5.0)
  assert_eq(post_success, 3.0)
  assert_eq(post_bad_request, 2.0)
}

test "直方图度量和分布分析" {
  // 测试直方图度量和分布分析
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "histogram.test")
  
  // 创建响应时间直方图
  let response_time_histogram = Meter::create_histogram(
    meter, 
    "http.request.duration", 
    Some("HTTP request duration"), 
    Some("ms")
  )
  
  // 设置自定义桶边界
  Histogram::set_boundaries(response_time_histogram, [10.0, 50.0, 100.0, 200.0, 500.0, 1000.0, 2000.0])
  
  // 记录不同响应时间
  let response_times = [
    5.2, 15.7, 45.3, 78.9, 123.4, 234.5, 456.7, 789.1, 1234.5, 1567.8,
    8.9, 23.4, 67.8, 98.7, 145.6, 267.8, 523.4, 845.6, 1345.7, 1678.9,
    12.3, 34.5, 56.7, 89.0, 134.5, 256.7, 489.0, 712.3, 1123.4, 1456.7,
    7.8, 19.6, 41.2, 83.4, 116.7, 223.4, 445.6, 767.8, 1234.5, 1567.8
  ]
  
  for time in response_times {
    Histogram::record(response_time_histogram, time)
  }
  
  // 分析分布
  let distribution_analyzer = DistributionAnalyzer::new()
  let distribution = DistributionAnalyzer::analyze(distribution_analyzer, response_time_histogram)
  
  // 验证分布统计
  assert_true(Distribution::get_count(distribution) == 40)
  assert_true(Distribution::get_sum(distribution) > 0.0)
  assert_true(Distribution::get_min(distribution) > 0.0)
  assert_true(Distribution::get_max(distribution) > 0.0)
  assert_true(Distribution::get_mean(distribution) > 0.0)
  
  // 验证百分位数
  let p50 = Distribution::get_percentile(distribution, 50.0)
  let p90 = Distribution::get_percentile(distribution, 90.0)
  let p95 = Distribution::get_percentile(distribution, 95.0)
  let p99 = Distribution::get_percentile(distribution, 99.0)
  
  assert_true(p50 > 0.0 && p50 < p90)
  assert_true(p90 > p50 && p90 < p95)
  assert_true(p95 > p90 && p95 < p99)
  
  // 验证桶计数
  let bucket_counts = Distribution::get_bucket_counts(distribution)
  assert_eq(Array::length(bucket_counts), 8) // 7个边界 + 1个溢出桶
  
  // 计算每个桶的百分比
  let bucket_percentages = Distribution::get_bucket_percentages(distribution)
  for percentage in bucket_percentages {
    assert_true(percentage >= 0.0 && percentage <= 100.0)
  }
}

test "时间序列数据聚合" {
  // 测试时间序列数据聚合
  let time_series_store = TimeSeriesStore::new()
  
  // 创建时间序列数据
  let base_timestamp = 1704067200000L // 2024-01-01 00:00:00 UTC
  
  // CPU使用率时间序列
  for i in 0..1440 { // 24小时，每分钟一个数据点
    let timestamp = base_timestamp + (i * 60 * 1000).to_long()
    let value = 30.0 + (Math::sin(i.to_float() * 0.1) * 20.0) + (Math::random() * 10.0)
    
    TimeSeriesStore::add_point(time_series_store, "cpu.usage", timestamp, value, [
      ("host", "server-01"),
      ("datacenter", "us-west-2"),
      ("environment", "production")
    ])
  }
  
  // 内存使用率时间序列
  for i in 0..1440 {
    let timestamp = base_timestamp + (i * 60 * 1000).to_long()
    let value = 60.0 + (Math::cos(i.to_float() * 0.05) * 15.0) + (Math::random() * 8.0)
    
    TimeSeriesStore::add_point(time_series_store, "memory.usage", timestamp, value, [
      ("host", "server-01"),
      ("datacenter", "us-west-2"),
      ("environment", "production")
    ])
  }
  
  // 创建时间序列聚合器
  let ts_aggregator = TimeSeriesAggregator::new()
  
  // 按小时聚合
  let hourly_cpu = TimeSeriesAggregator::aggregate_by_interval(
    ts_aggregator, 
    "cpu.usage", 
    base_timestamp, 
    base_timestamp + (24 * 60 * 60 * 1000).to_long(), 
    OneHour,
    Average
  )
  
  assert_eq(Array::length(hourly_cpu), 24)
  
  // 按天聚合
  let daily_cpu = TimeSeriesAggregator::aggregate_by_interval(
    ts_aggregator, 
    "cpu.usage", 
    base_timestamp, 
    base_timestamp + (24 * 60 * 60 * 1000).to_long(), 
    OneDay,
    Average
  )
  
  assert_eq(Array::length(daily_cpu), 1)
  
  // 计算趋势
  let trend_analyzer = TrendAnalyzer::new()
  let cpu_trend = TrendAnalyzer::calculate_trend(trend_analyzer, hourly_cpu)
  
  assert_true(Trend::has_direction(cpu_trend))
  assert_true(Trend::get_slope(cpu_trend) != 0.0 || Trend::get_direction(cpu_trend) == Stable)
  
  // 检测异常
  let anomaly_detector = AnomalyDetector::new()
  let cpu_anomalies = AnomalyDetector::detect(anomaly_detector, hourly_cpu, ZScore, 2.0)
  
  assert_true(Array::length(cpu_anomalies) >= 0)
  
  // 预测未来值
  let forecaster = TimeSeriesForecaster::new()
  let cpu_forecast = TimeSeriesForecaster::forecast(forecaster, hourly_cpu, 6) // 预测未来6小时
  
  assert_eq(Array::length(cpu_forecast), 6)
  
  // 验证预测值的合理性
  for forecast_point in cpu_forecast {
    let value = TimeSeriesPoint::get_value(forecast_point)
    assert_true(value >= 0.0 && value <= 100.0) // CPU使用率应该在0-100%之间
  }
}

test "高级度量和统计分析" {
  // 测试高级度量和统计分析
  let stats_analyzer = StatsAnalyzer::new()
  
  // 创建测试数据集
  let data_set = []
  for i in 0..1000 {
    let value = Math::normal_random(50.0, 15.0) // 均值50，标准差15的正态分布
    Array::push(data_set, value)
  }
  
  // 计算基本统计量
  let basic_stats = StatsAnalyzer::calculate_basic_stats(stats_analyzer, data_set)
  
  assert_true(BasicStats::get_count(basic_stats) == 1000)
  assert_true(BasicStats::get_mean(basic_stats) > 45.0 && BasicStats::get_mean(basic_stats) < 55.0)
  assert_true(BasicStats::get_std_dev(basic_stats) > 10.0 && BasicStats::get_std_dev(basic_stats) < 20.0)
  assert_true(BasicStats::get_min(basic_stats) < BasicStats::get_mean(basic_stats))
  assert_true(BasicStats::get_max(basic_stats) > BasicStats::get_mean(basic_stats))
  
  // 计算分位数
  let quantiles = StatsAnalyzer::calculate_quantiles(stats_analyzer, data_set, [0.25, 0.5, 0.75, 0.9, 0.95, 0.99])
  
  let q25 = Quantiles::get_quantile(quantiles, 0.25)
  let q50 = Quantiles::get_quantile(quantiles, 0.5)
  let q75 = Quantiles::get_quantile(quantiles, 0.75)
  let q90 = Quantiles::get_quantile(quantiles, 0.9)
  let q95 = Quantiles::get_quantile(quantiles, 0.95)
  let q99 = Quantiles::get_quantile(quantiles, 0.99)
  
  assert_true(q25 < q50 && q50 < q75 && q75 < q90 && q90 < q95 && q95 < q99)
  assert_eq(q50, BasicStats::get_median(basic_stats))
  
  // 计算偏度和峰度
  let shape_stats = StatsAnalyzer::calculate_shape_stats(stats_analyzer, data_set)
  
  let skewness = ShapeStats::get_skewness(shape_stats)
  let kurtosis = ShapeStats::get_kurtosis(shape_stats)
  
  // 对于正态分布，偏度应接近0，峰度应接近3
  assert_true(skewness > -1.0 && skewness < 1.0)
  assert_true(kurtosis > 2.0 && kurtosis < 4.0)
  
  // 相关性分析
  let data_set_x = []
  let data_set_y = []
  
  for i in 0..1000 {
    let x = Math::normal_random(0.0, 1.0)
    let y = 2.0 * x + Math::normal_random(0.0, 0.5) // y与x线性相关
    Array::push(data_set_x, x)
    Array::push(data_set_y, y)
  }
  
  let correlation = StatsAnalyzer::calculate_correlation(stats_analyzer, data_set_x, data_set_y)
  
  // 验证相关性
  assert_true(correlation > 0.7) // 应该有较强的正相关
  
  // 回归分析
  let regression = StatsAnalyzer::linear_regression(stats_analyzer, data_set_x, data_set_y)
  
  let slope = Regression::get_slope(regression)
  let intercept = Regression::get_intercept(regression)
  let r_squared = Regression::get_r_squared(regression)
  
  // 验证回归结果
  assert_true(slope > 1.5 && slope < 2.5) // 斜率应接近2
  assert_true(r_squared > 0.5) // R²应较高
}

test "实时度量和流式聚合" {
  // 测试实时度量和流式聚合
  let realtime_processor = RealtimeProcessor::new()
  
  // 设置滑动窗口
  let sliding_window = SlidingWindow::new(60, 10) // 60秒窗口，10秒滑动
  
  // 创建流式度量
  let stream_counter = RealtimeProcessor::create_counter(realtime_processor, "events.per.second")
  let stream_gauge = RealtimeProcessor::create_gauge(realtime_processor, "active.connections")
  let stream_histogram = RealtimeProcessor::create_histogram(realtime_processor, "response.time")
  
  // 模拟实时数据流
  for i in 0..300 { // 5分钟的数据，每秒一个数据点
    let timestamp = 1704067200000L + i.to_long()
    
    // 模拟事件计数
    let event_count = Math::poisson_random(10.0) // 平均每秒10个事件
    RealtimeProcessor::record_counter(realtime_processor, stream_counter, event_count.to_float(), timestamp)
    
    // 模拟活跃连接数
    let active_connections = 50 + Math::sin(i.to_float() * 0.1) * 20.0
    RealtimeProcessor::record_gauge(realtime_processor, stream_gauge, active_connections, timestamp)
    
    // 模拟响应时间
    let response_time = Math::exponential_random(100.0) // 平均100ms
    RealtimeProcessor::record_histogram(realtime_processor, stream_histogram, response_time, timestamp)
    
    // 添加到滑动窗口
    SlidingWindow::add_point(sliding_window, timestamp, event_count.to_float())
  }
  
  // 计算实时统计
  let current_time = 1704067200000L + 300.to_long()
  let window_data = SlidingWindow::get_window_data(sliding_window, current_time)
  
  let realtime_stats = RealtimeProcessor::calculate_window_stats(realtime_processor, window_data)
  
  // 验证实时统计
  assert_true(RealtimeStats::get_event_rate(realtime_stats) > 0.0)
  assert_true(RealtimeStats::get_avg_response_time(realtime_stats) > 0.0)
  assert_true(RealtimeStats::get_p95_response_time(realtime_stats) > 0.0)
  assert_true(RealtimeStats::get_active_connections(realtime_stats) > 0.0)
  
  // 设置实时告警
  let alert_manager = AlertManager::new()
  
  // 创建告警规则
  AlertManager::add_rule(alert_manager, "high.error.rate", GreaterThan, 0.05, "error.rate")
  AlertManager::add_rule(alert_manager, "high.response.time", GreaterThan, 500.0, "response.time.p95")
  AlertManager::add_rule(alert_manager, "low.active.connections", LessThan, 10.0, "active.connections")
  
  // 检查告警
  let alerts = AlertManager::evaluate_rules(alert_manager, realtime_stats)
  
  // 验证告警系统
  assert_true(Array::length(alerts) >= 0)
  
  for alert in alerts {
    assert_true(Alert::has_name(alert))
    assert_true(Alert::has_severity(alert))
    assert_true(Alert::has_message(alert))
  }
}

test "度量和数据的可视化准备" {
  // 测试度量和数据的可视化准备
  let viz_preparer = VisualizationPreparer::new()
  
  // 创建多维度度量数据
  let multi_dim_data = []
  
  // 按服务和端点分组的请求数据
  let services = ["auth", "user", "order", "payment", "inventory"]
  let endpoints = ["login", "register", "profile", "create", "update", "delete"]
  
  for service in services {
    for endpoint in endpoints {
      for status_code in ["200", "400", "404", "500"] {
        let count = Math::poisson_random(100.0)
        let avg_duration = Math::normal_random(150.0, 50.0)
        
        let data_point = {
          "service": service,
          "endpoint": endpoint,
          "status_code": status_code,
          "count": count,
          "avg_duration": avg_duration,
          "timestamp": 1704067200000L
        }
        
        Array::push(multi_dim_data, data_point)
      }
    }
  }
  
  // 准备时间序列图表数据
  let time_series_chart = VisualizationPreparer::prepare_time_series_chart(
    viz_preparer, 
    multi_dim_data, 
    "timestamp", 
    "count", 
    "service"
  )
  
  // 验证时间序列图表数据
  assert_true(TimeSeriesChart::has_series(time_series_chart))
  assert_true(TimeSeriesChart::get_series_count(time_series_chart) > 0)
  
  // 准备柱状图数据
  let bar_chart = VisualizationPreparer::prepare_bar_chart(
    viz_preparer, 
    multi_dim_data, 
    "service", 
    "count"
  )
  
  // 验证柱状图数据
  assert_true(BarChart::has_categories(bar_chart))
  assert_true(BarChart::get_category_count(bar_chart) > 0)
  
  // 准备饼图数据
  let pie_chart = VisualizationPreparer::prepare_pie_chart(
    viz_preparer, 
    multi_dim_data, 
    "status_code", 
    "count"
  )
  
  // 验证饼图数据
  assert_true(PieChart::has_slices(pie_chart))
  assert_true(PieChart::get_slice_count(pie_chart) > 0)
  
  // 准备热力图数据
  let heatmap = VisualizationPreparer::prepare_heatmap(
    viz_preparer, 
    multi_dim_data, 
    "service", 
    "endpoint", 
    "avg_duration"
  )
  
  // 验证热力图数据
  assert_true(Heatmap::has_x_axis(heatmap))
  assert_true(Heatmap::has_y_axis(heatmap))
  assert_true(Heatmap::has_data_matrix(heatmap))
  
  // 准备仪表盘数据
  let dashboard = VisualizationPreparer::prepare_dashboard(
    viz_preparer, 
    [
      ("Request Trends", time_series_chart),
      ("Service Load", bar_chart),
      ("Status Distribution", pie_chart),
      ("Performance Heatmap", heatmap)
    ]
  )
  
  // 验证仪表盘数据
  assert_true(Dashboard::has_widgets(dashboard))
  assert_eq(Dashboard::get_widget_count(dashboard), 4)
  
  // 导出可视化数据
  let json_exporter = JsonExporter::new()
  let dashboard_json = JsonExporter::export_dashboard(json_exporter, dashboard)
  
  // 验证导出数据
  assert_true(String::length(dashboard_json) > 0)
  assert_true(JsonValidator::is_valid(dashboard_json))
}