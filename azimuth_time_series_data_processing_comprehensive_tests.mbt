// Azimuth Time Series Data Processing Test Suite
// 测试遥测系统的时间序列数据处理功能

test "时间序列数据收集和存储" {
  // 创建具有时间序列数据处理功能的遥测提供者
  let telemetry_provider = TelemetryProvider::with_time_series_processing()
  
  // 配置时间序列数据设置
  let time_series_config = TimeSeriesConfig {
    retention_period: 86400 * 30,  // 30天
    downsampling_enabled: true,
    downsampling_intervals: ["1m", "5m", "15m", "1h", "1d"],
    compression_enabled: true,
    aggregation_functions: ["avg", "min", "max", "sum", "count"]
  }
  
  TimeSeriesProcessing::configure(telemetry_provider, time_series_config)
  
  // 创建时间序列数据收集器
  let collector = TimeSeriesCollector::new("system.metrics")
  
  // 模拟时间序列数据收集
  let base_time = Timestamp::from_string("2025-01-02T10:00:00Z")
  let metrics = [
    ("cpu.usage", "cpu"),
    ("memory.usage", "memory"),
    ("disk.io", "disk"),
    ("network.throughput", "network"),
    ("response.time", "performance")
  ]
  
  // 生成24小时的数据，每分钟一个数据点
  for minute = 0; minute < 1440; minute = minute + 1 {
    let timestamp = base_time.add_minutes(minute)
    
    for (metric_name, metric_type) in metrics {
      // 生成模拟数据
      let value = generate_metric_value(metric_type, minute)
      
      let data_point = TimeSeriesDataPoint::new(
        metric_name,
        value,
        timestamp,
        [
          ("host", "server-01"),
          ("environment", "production"),
          ("service", "azimuth")
        ]
      )
      
      TimeSeriesCollector::add_data_point(collector, data_point)
    }
  }
  
  // 验证数据收集
  let total_points = TimeSeriesCollector::get_total_data_points(collector)
  assert_eq(total_points, 1440 * 5) // 1440分钟 × 5个指标
  
  // 按指标分组验证
  for (metric_name, _) in metrics {
    let metric_points = TimeSeriesCollector::get_data_points_by_metric(collector, metric_name)
    assert_eq(metric_points.length(), 1440)
  }
  
  // 测试数据存储
  let storage_result = TimeSeriesProcessing::store_data(telemetry_provider, collector)
  assert_true(storage_result.success)
  
  // 测试数据检索
  let query = TimeSeriesQuery::new()
    .metric("cpu.usage")
    .start_time(base_time)
    .end_time(base_time.add_hours(1))
    .aggregation("avg")
    .interval("1m")
  
  let query_result = TimeSeriesProcessing::query_data(telemetry_provider, query)
  assert_true(query_result.success)
  assert_eq(query_result.data_points.length(), 60) // 1小时=60分钟
  
  // 测试多指标查询
  let multi_metric_query = TimeSeriesQuery::new()
    .metrics(["cpu.usage", "memory.usage", "disk.io"])
    .start_time(base_time)
    .end_time(base_time.add_hours(1))
    .aggregation("avg")
    .interval("5m")
  
  let multi_metric_result = TimeSeriesProcessing::query_data(telemetry_provider, multi_metric_query)
  assert_true(multi_metric_result.success)
  assert_eq(multi_metric_result.data_points.length(), 12) // 1小时=12个5分钟间隔
  assert_eq(multi_metric_result.series.length(), 3) // 3个指标
  
  // 测试标签过滤
  let tag_filter_query = TimeSeriesQuery::new()
    .metric("cpu.usage")
    .start_time(base_time)
    .end_time(base_time.add_hours(1))
    .tag_filter("host", "server-01")
    .aggregation("max")
    .interval("10m")
  
  let tag_filter_result = TimeSeriesProcessing::query_data(telemetry_provider, tag_filter_query)
  assert_true(tag_filter_result.success)
  assert_eq(tag_filter_result.data_points.length(), 6) // 1小时=6个10分钟间隔
  
  assert_true(true)
}

test "时间序列数据聚合和下采样" {
  // 创建具有时间序列聚合功能的遥测提供者
  let telemetry_provider = TelemetryProvider::with_time_series_aggregation()
  
  // 配置聚合设置
  let aggregation_config = AggregationConfig {
    default_interval: "1m",
    supported_intervals: ["30s", "1m", "5m", "15m", "30m", "1h", "6h", "12h", "1d"],
    aggregation_functions: {
      "avg": TimeSeriesAggregation::average,
      "min": TimeSeriesAggregation::minimum,
      "max": TimeSeriesAggregation::maximum,
      "sum": TimeSeriesAggregation::sum,
      "count": TimeSeriesAggregation::count,
      "p50": TimeSeriesAggregation::percentile(50),
      "p95": TimeSeriesAggregation::percentile(95),
      "p99": TimeSeriesAggregation::percentile(99)
    },
    downsampling_policy: "auto"
  }
  
  TimeSeriesAggregation::configure(telemetry_provider, aggregation_config)
  
  // 创建高精度时间序列数据（每秒一个数据点）
  let collector = TimeSeriesCollector::new("high.frequency.metrics")
  let base_time = Timestamp::from_string("2025-01-02T10:00:00Z")
  
  // 生成1小时的高频数据
  for second = 0; second < 3600; second = second + 1 {
    let timestamp = base_time.add_seconds(second)
    
    // CPU使用率数据 - 模拟波动
    let cpu_value = 50.0 + 20.0 * Math::sin(second.to_double() / 300.0) + (Math::random() - 0.5) * 10.0
    let cpu_data_point = TimeSeriesDataPoint::new("cpu.usage", cpu_value, timestamp, [
      ("host", "server-01"),
      ("core", "0")
    ])
    TimeSeriesCollector::add_data_point(collector, cpu_data_point)
    
    // 内存使用率数据 - 模拟逐渐增长
    let memory_value = 60.0 + (second.to_double() / 3600.0) * 20.0 + (Math::random() - 0.5) * 5.0
    let memory_data_point = TimeSeriesDataPoint::new("memory.usage", memory_value, timestamp, [
      ("host", "server-01")
    ])
    TimeSeriesCollector::add_data_point(collector, memory_data_point)
    
    // 网络吞吐量数据 - 模拟突发流量
    let network_value = if second % 60 < 10 { 1000.0 } else { 100.0 } + (Math::random() - 0.5) * 50.0
    let network_data_point = TimeSeriesDataPoint::new("network.throughput", network_value, timestamp, [
      ("host", "server-01"),
      ("interface", "eth0")
    ])
    TimeSeriesCollector::add_data_point(collector, network_data_point)
  }
  
  // 验证高频数据收集
  let total_points = TimeSeriesCollector::get_total_data_points(collector)
  assert_eq(total_points, 3600 * 3) // 3600秒 × 3个指标
  
  // 测试1分钟聚合
  let one_minute_query = TimeSeriesQuery::new()
    .metrics(["cpu.usage", "memory.usage", "network.throughput"])
    .start_time(base_time)
    .end_time(base_time.add_hours(1))
    .aggregation("avg")
    .interval("1m")
  
  let one_minute_result = TimeSeriesAggregation::aggregate(telemetry_provider, collector, one_minute_query)
  assert_true(one_minute_result.success)
  assert_eq(one_minute_result.data_points.length(), 60) // 1小时=60个1分钟间隔
  
  // 验证CPU使用率1分钟聚合
  let cpu_series = one_minute_result.series.find(fn(s) { s.metric_name == "cpu.usage" })
  assert_not_eq(cpu_series, None)
  assert_eq(cpu_series.data_points.length(), 60)
  
  // 验证第一个聚合点（前60秒的平均值）
  let first_cpu_point = cpu_series.data_points[0]
  assert_eq(first_cpu_point.timestamp, base_time.add_minutes(1))
  assert_true(first_cpu_point.value >= 40.0 && first_cpu_point.value <= 60.0) // 合理范围内的平均值
  
  // 测试5分钟聚合
  let five_minute_query = TimeSeriesQuery::new()
    .metric("cpu.usage")
    .start_time(base_time)
    .end_time(base_time.add_hours(1))
    .aggregation("max")
    .interval("5m")
  
  let five_minute_result = TimeSeriesAggregation::aggregate(telemetry_provider, collector, five_minute_query)
  assert_true(five_minute_result.success)
  assert_eq(five_minute_result.data_points.length(), 12) // 1小时=12个5分钟间隔
  
  // 测试多函数聚合
  let multi_function_query = TimeSeriesQuery::new()
    .metric("memory.usage")
    .start_time(base_time)
    .end_time(base_time.add_hours(1))
    .aggregations(["avg", "min", "max", "p95"])
    .interval("15m")
  
  let multi_function_result = TimeSeriesAggregation::aggregate(telemetry_provider, collector, multi_function_query)
  assert_true(multi_function_result.success)
  assert_eq(multi_function_result.data_points.length(), 4) // 1小时=4个15分钟间隔
  
  // 验证多函数聚合结果
  let memory_series = multi_function_result.series.find(fn(s) { s.metric_name == "memory.usage" })
  assert_not_eq(memory_series, None)
  
  // 检查每个聚合函数的结果
  let avg_series = memory_series.aggregation_series.find(fn(s) { s.function == "avg" })
  let min_series = memory_series.aggregation_series.find(fn(s) { s.function == "min" })
  let max_series = memory_series.aggregation_series.find(fn(s) { s.function == "max" })
  let p95_series = memory_series.aggregation_series.find(fn(s) { s.function == "p95" })
  
  assert_not_eq(avg_series, None)
  assert_not_eq(min_series, None)
  assert_not_eq(max_series, None)
  assert_not_eq(p95_series, None)
  
  // 验证聚合函数的数学关系
  for i = 0; i < avg_series.data_points.length(); i = i + 1 {
    let avg_point = avg_series.data_points[i]
    let min_point = min_series.data_points[i]
    let max_point = max_series.data_points[i]
    let p95_point = p95_series.data_points[i]
    
    // 平均值应该在最小值和最大值之间
    assert_true(avg_point.value >= min_point.value)
    assert_true(avg_point.value <= max_point.value)
    
    // P95应该在最小值和最大值之间
    assert_true(p95_point.value >= min_point.value)
    assert_true(p95_point.value <= max_point.value)
    
    // P95应该大于或等于平均值
    assert_true(p95_point.value >= avg_point.value)
  }
  
  // 测试下采样
  let downsampling_result = TimeSeriesAggregation::downsample(
    telemetry_provider, 
    collector, 
    "1h",  // 下采样到1小时间隔
    ["avg", "min", "max"]
  )
  assert_true(downsampling_result.success)
  assert_eq(downsampling_result.data_points.length(), 1) // 1小时=1个1小时间隔
  
  // 验证下采样数据存储
  let store_result = TimeSeriesProcessing::store_downsampled_data(telemetry_provider, downsampling_result)
  assert_true(store_result.success)
  
  assert_true(true)
}

test "时间序列数据分析和趋势检测" {
  // 创建具有时间序列分析功能的遥测提供者
  let telemetry_provider = TelemetryProvider::with_time_series_analysis()
  
  // 配置分析设置
  let analysis_config = AnalysisConfig {
    trend_detection_enabled: true,
    anomaly_detection_enabled: true,
    seasonality_analysis_enabled: true,
    forecast_enabled: true,
    prediction_horizon: 3600,  // 1小时预测
    confidence_interval: 0.95
  }
  
  TimeSeriesAnalysis::configure(telemetry_provider, analysis_config)
  
  // 创建测试数据 - 包含趋势、季节性和异常值
  let collector = TimeSeriesCollector::new("analysis.test.data")
  let base_time = Timestamp::from_string("2025-01-02T00:00:00Z")
  
  // 生成7天的数据，每小时一个数据点
  for hour = 0; hour < 168; hour = hour + 1 { // 7天=168小时
    let timestamp = base_time.add_hours(hour)
    
    // 基础值 + 线性趋势 + 日季节性 + 周季节性 + 噪声
    let base_value = 50.0
    let trend = hour.to_double() * 0.1  // 每小时增加0.1的线性趋势
    let daily_seasonality = 10.0 * Math::sin((hour % 24).to_double() / 24.0 * 2 * Math::PI)  // 日季节性
    let weekly_seasonality = 5.0 * Math::sin((hour % 168).to_double() / 168.0 * 2 * Math::PI)  // 周季节性
    let noise = (Math::random() - 0.5) * 5.0  // 随机噪声
    
    let value = base_value + trend + daily_seasonality + weekly_seasonality + noise
    
    // 添加一些异常值
    let final_value = if (hour == 36 || hour == 72 || hour == 120) {
      value + 30.0  // 异常高峰
    } else if (hour == 48 || hour == 96) {
      value - 25.0  // 异常低谷
    } else {
      value
    }
    
    let data_point = TimeSeriesDataPoint::new("analysis.metric", final_value, timestamp, [
      ("host", "server-01"),
      ("environment", "production")
    ])
    
    TimeSeriesCollector::add_data_point(collector, data_point)
  }
  
  // 获取完整时间序列
  let time_series = TimeSeriesCollector::get_time_series_by_metric(collector, "analysis.metric")
  assert_eq(time_series.data_points.length(), 168)
  
  // 趋势分析
  let trend_analysis = TimeSeriesAnalysis::detect_trend(telemetry_provider, time_series)
  assert_true(trend_analysis.detected)
  assert_true(trend_analysis.slope > 0.05)  // 检测到上升趋势
  assert_true(trend_analysis.confidence > 0.8)  // 高置信度
  
  // 季节性分析
  let seasonality_analysis = TimeSeriesAnalysis::detect_seasonality(telemetry_provider, time_series)
  assert_true(seasonality_analysis.daily_detected)
  assert_true(seasonality_analysis.weekly_detected)
  assert_true(seasonality_analysis.daily_strength > 0.5)  // 中等季节性强度
  assert_true(seasonality_analysis.weekly_strength > 0.3)  // 较弱周季节性
  
  // 异常检测
  let anomaly_detection = TimeSeriesAnalysis::detect_anomalies(telemetry_provider, time_series)
  assert_true(anomaly_detection.anomalies.length() >= 5)  // 至少检测到5个异常
  
  // 验证检测到的异常点
  let expected_anomaly_hours = [36, 48, 72, 96, 120]
  for expected_hour in expected_anomaly_hours {
    let anomaly_found = anomaly_detection.anomalies.any(fn(anomaly) {
      let anomaly_timestamp = anomaly.timestamp
      let expected_timestamp = base_time.add_hours(expected_hour)
      Time::equals(anomaly_timestamp, expected_timestamp)
    })
    assert_true(anomaly_found)
  }
  
  // 变点检测
  let change_point_detection = TimeSeriesAnalysis::detect_change_points(telemetry_provider, time_series)
  assert_true(change_point_detection.change_points.length() >= 1)  // 至少检测到一个变点
  
  // 相关性分析 - 创建第二个时间序列
  for hour = 0; hour < 168; hour = hour + 1 {
    let timestamp = base_time.add_hours(hour)
    let base_data_point = time_series.data_points[hour]
    
    // 创建与第一个序列相关的第二个序列（相关系数约0.7）
    let correlated_value = base_data_point.value * 0.7 + (Math::random() - 0.5) * 20.0
    
    let correlated_data_point = TimeSeriesDataPoint::new("correlated.metric", correlated_value, timestamp, [
      ("host", "server-01"),
      ("environment", "production")
    ])
    
    TimeSeriesCollector::add_data_point(collector, correlated_data_point)
  }
  
  let correlated_series = TimeSeriesCollector::get_time_series_by_metric(collector, "correlated.metric")
  let correlation_analysis = TimeSeriesAnalysis::analyze_correlation(telemetry_provider, time_series, correlated_series)
  assert_true(correlation_analysis.correlation_coefficient > 0.5)  // 检测到正相关
  assert_true(correlation_analysis.p_value < 0.05)  // 显著相关
  
  // 预测分析
  let forecast_analysis = TimeSeriesAnalysis::forecast(telemetry_provider, time_series, 24)  // 预测24小时
  assert_true(forecast_analysis.forecast_points.length() == 24)
  
  // 验证预测点的置信区间
  for forecast_point in forecast_analysis.forecast_points {
    assert_true(forecast_point.lower_bound < forecast_point.predicted_value)
    assert_true(forecast_point.upper_bound > forecast_point.predicted_value)
  }
  
  // 验证预测趋势延续
  let last_actual_value = time_series.data_points[167].value
  let first_forecast_value = forecast_analysis.forecast_points[0].predicted_value
  assert_true(first_forecast_value > last_actual_value)  // 预测延续上升趋势
  
  // 生成分析报告
  let analysis_report = TimeSeriesAnalysis::generate_analysis_report(telemetry_provider, time_series)
  assert_true(AnalysisReport::has_trend(analysis_report))
  assert_true(AnalysisReport::has_seasonality(analysis_report))
  assert_true(AnalysisReport::anomaly_count(analysis_report) >= 5)
  assert_true(AnalysisReport::change_point_count(analysis_report) >= 1)
  assert_true(AnalysisReport::forecast_points(analysis_report) == 24)
  
  assert_true(true)
}

test "时间序列数据可视化和报告" {
  // 创建具有时间序列可视化功能的遥测提供者
  let telemetry_provider = TelemetryProvider::with_time_series_visualization()
  
  // 配置可视化设置
  let visualization_config = VisualizationConfig {
    chart_types: ["line", "area", "bar", "heatmap"],
    color_schemes: ["default", "viridis", "plasma", "warm", "cool"],
    output_formats: ["png", "svg", "html", "json"],
    resolution: 1920,  // 高分辨率
    interactive_charts: true
  }
  
  TimeSeriesVisualization::configure(telemetry_provider, visualization_config)
  
  // 创建多指标时间序列数据
  let collector = TimeSeriesCollector::new("visualization.data")
  let base_time = Timestamp::from_string("2025-01-02T00:00:00Z")
  let metrics = [
    ("cpu.usage", "CPU使用率", "%", [0, 100]),
    ("memory.usage", "内存使用率", "%", [0, 100]),
    ("disk.io", "磁盘I/O", "MB/s", [0, 500]),
    ("network.throughput", "网络吞吐量", "Mbps", [0, 1000])
  ]
  
  // 生成24小时的数据，每5分钟一个数据点
  for interval = 0; interval < 288; interval = interval + 1 { // 24小时=288个5分钟间隔
    let timestamp = base_time.add_minutes(interval * 5)
    
    for (metric_name, _, _, _) in metrics {
      // 生成模拟数据
      let value = generate_metric_value(metric_name.split(".")[0], interval)
      
      let data_point = TimeSeriesDataPoint::new(metric_name, value, timestamp, [
        ("host", "server-01"),
        ("environment", "production")
      ])
      
      TimeSeriesCollector::add_data_point(collector, data_point)
    }
  }
  
  // 获取时间序列数据
  let time_series_list = []
  for (metric_name, _, _, _) in metrics {
    let series = TimeSeriesCollector::get_time_series_by_metric(collector, metric_name)
    time_series_list.push(series)
  }
  
  // 创建线图
  let line_chart_config = ChartConfig::new("line")
    .title("系统性能指标 - 24小时")
    .time_range(base_time, base_time.add_hours(24))
    .y_axis_range(0, 100)
    .color_scheme("default")
    .resolution(1920, 1080)
  
  let line_chart = TimeSeriesVisualization::create_chart(telemetry_provider, time_series_list, line_chart_config)
  assert_true(line_chart.success)
  assert_true(line_chart.data.length() > 0)
  
  // 创建面积图
  let area_chart_config = ChartConfig::new("area")
    .title("资源使用率堆叠图")
    .time_range(base_time, base_time.add_hours(24))
    .stacked(true)
    .color_scheme("viridis")
    .resolution(1920, 1080)
  
  let area_chart = TimeSeriesVisualization::create_chart(telemetry_provider, time_series_list, area_chart_config)
  assert_true(area_chart.success)
  assert_true(area_chart.data.length() > 0)
  
  // 创建热力图
  let heatmap_config = ChartConfig::new("heatmap")
    .title("系统性能热力图")
    .time_range(base_time, base_time.add_hours(24))
    .color_scheme("plasma")
    .resolution(1920, 1080)
  
  let heatmap = TimeSeriesVisualization::create_chart(telemetry_provider, time_series_list, heatmap_config)
  assert_true(heatmap.success)
  assert_true(heatmap.data.length() > 0)
  
  // 创建交互式HTML图表
  let interactive_chart_config = ChartConfig::new("line")
    .title("交互式性能图表")
    .time_range(base_time, base_time.add_hours(24))
    .interactive(true)
    .zoom_enabled(true)
    .pan_enabled(true)
    .tooltip_enabled(true)
    .legend_enabled(true)
    .color_scheme("warm")
    .resolution(1920, 1080)
  
  let interactive_chart = TimeSeriesVisualization::create_interactive_chart(
    telemetry_provider, 
    time_series_list, 
    interactive_chart_config
  )
  assert_true(interactive_chart.success)
  assert_true(interactive_chart.html_content.length() > 0)
  
  // 创建仪表板
  let dashboard_config = DashboardConfig::new()
    .title("系统监控仪表板")
    .layout("grid")  // 网格布局
    .columns(2)
    .rows(2)
    .refresh_interval(300)  // 5分钟刷新
    .time_range(base_time, base_time.add_hours(24))
  
  // 添加图表到仪表板
  DashboardConfig::add_chart(dashboard_config, {
    "title": "CPU和内存使用率",
    "type": "line",
    "metrics": ["cpu.usage", "memory.usage"],
    "position": {"row": 0, "col": 0, "width": 1, "height": 1}
  })
  
  DashboardConfig::add_chart(dashboard_config, {
    "title": "磁盘和网络I/O",
    "type": "area",
    "metrics": ["disk.io", "network.throughput"],
    "position": {"row": 0, "col": 1, "width": 1, "height": 1}
  })
  
  DashboardConfig::add_chart(dashboard_config, {
    "title": "系统性能热力图",
    "type": "heatmap",
    "metrics": ["cpu.usage", "memory.usage", "disk.io", "network.throughput"],
    "position": {"row": 1, "col": 0, "width": 2, "height": 1}
  })
  
  let dashboard = TimeSeriesVisualization::create_dashboard(telemetry_provider, time_series_list, dashboard_config)
  assert_true(dashboard.success)
  assert_true(dashboard.html_content.length() > 0)
  
  // 生成报告
  let report_config = ReportConfig::new()
    .title("系统性能报告")
    .time_range(base_time, base_time.add_hours(24))
    .include_summary(true)
    .include_charts(true)
    .include_anomalies(true)
    .include_trends(true)
    .format("html")
  
  let report = TimeSeriesVisualization::generate_report(telemetry_provider, time_series_list, report_config)
  assert_true(report.success)
  assert_true(report.html_content.length() > 0)
  
  // 验证报告内容
  assert_true(report.html_content.contains("系统性能报告"))
  assert_true(report.html_content.contains("性能摘要"))
  assert_true(report.html_content.contains("趋势分析"))
  assert_true(report.html_content.contains("异常检测"))
  
  // 导出图表为不同格式
  let png_export = TimeSeriesVisualization::export_chart(telemetry_provider, line_chart, "png")
  assert_true(png_export.success)
  assert_true(png_export.data.length() > 0)
  
  let svg_export = TimeSeriesVisualization::export_chart(telemetry_provider, line_chart, "svg")
  assert_true(svg_export.success)
  assert_true(svg_export.data.length() > 0)
  
  let json_export = TimeSeriesVisualization::export_chart(telemetry_provider, line_chart, "json")
  assert_true(json_export.success)
  assert_true(json_export.data.length() > 0)
  
  // 验证导出的JSON包含图表数据
  let chart_data = JSON::parse(json_export.data)
  assert_true(chart_data.contains("title"))
  assert_true(chart_data.contains("data"))
  assert_true(chart_data.contains("config"))
  
  assert_true(true)
}

// 辅助函数：生成模拟指标值
fn generate_metric_value(metric_type : String, time_index : Int) -> Double {
  match metric_type {
    "cpu" => {
      // CPU使用率：基础值 + 正弦波动 + 随机噪声
      50.0 + 20.0 * Math::sin(time_index.to_double() / 60.0) + (Math::random() - 0.5) * 10.0
    }
    "memory" => {
      // 内存使用率：逐渐增长 + 随机波动
      40.0 + (time_index.to_double() / 288.0) * 30.0 + (Math::random() - 0.5) * 5.0
    }
    "disk" => {
      // 磁盘I/O：突发模式
      if time_index % 12 < 3 {  // 每12个间隔中有3个高I/O
        200.0 + (Math::random() - 0.5) * 50.0
      } else {
        50.0 + (Math::random() - 0.5) * 20.0
      }
    }
    "network" => {
      // 网络吞吐量：日周期性
      100.0 + 150.0 * Math::sin((time_index % 288).to_double() / 288.0 * 2 * Math::PI) + (Math::random() - 0.5) * 30.0
    }
    _ => {
      50.0 + (Math::random() - 0.5) * 20.0
    }
  }
}