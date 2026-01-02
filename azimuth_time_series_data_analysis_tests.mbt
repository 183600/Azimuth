// Azimuth 时间序列数据分析测试用例
// 专注于时间序列数据的收集、存储、分析和可视化

// 测试1: 时间序列数据收集和存储
test "时间序列数据收集和存储测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.collection")
  
  // 创建时间序列数据收集器
  let ts_collector = TimeSeriesCollector::new(meter)
  
  // 创建收集指标
  let collection_counter = Meter::create_counter(meter, "timeseries.points.collected")
  let storage_gauge = Meter::create_gauge(meter, "timeseries.storage.size")
  
  // 生成时间序列数据
  let mut time_series_data = []
  let base_time = Time::now()
  
  for i in 0..=239 {
    let timestamp = base_time + (i * 60) // 每分钟一个数据点，共4小时
    let value = 100.0 + (10.0 * (i / 60).to_float()) + (5.0 * (i % 60).to_float() / 60.0) + (Random::float() * 2.0)
    
    let data_point = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("cpu.usage")),
      ("metric.value", FloatValue(value)),
      ("host.name", StringValue("server-" + (i % 4).to_string())),
      ("region", StringValue("us-west-" + (i % 3).to_string()))
    ])
    time_series_data = time_series_data.push(data_point)
  }
  
  // 收集和存储时间序列数据
  for point in time_series_data {
    TimeSeriesCollector::collect_point(ts_collector, point)
    Counter::add(collection_counter, 1.0)
  }
  
  // 更新存储指标
  let storage_size = TimeSeriesCollector::get_storage_size(ts_collector)
  Gauge::set(storage_gauge, storage_size.to_float())
  
  // 验证收集结果
  let collection_results = TimeSeriesCollector::get_results(ts_collector)
  assert_eq(collection_results.total_points, 240)
  assert_eq(collection_results.unique_metrics, 1)
  assert_eq(collection_results.unique_hosts, 4)
  assert_eq(Counter::value(collection_counter), 240.0)
}

// 测试2: 时间序列数据聚合和降采样
test "时间序列数据聚合和降采样测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.aggregation")
  
  // 创建时间序列聚合器
  let ts_aggregator = TimeSeriesAggregator::new(meter)
  
  // 创建聚合指标
  let aggregation_counter = Meter::create_counter(meter, "timeseries.aggregations")
  let downsample_gauge = Meter::create_gauge(meter, "timeseries.downsampled.points")
  
  // 生成高频率时间序列数据
  let mut high_freq_data = []
  let base_time = Time::now()
  
  for i in 0..=1439 { // 24小时，每分钟一个点
    let timestamp = base_time + (i * 60)
    let base_value = 50.0 + 20.0 * (i / 720).to_float() // 12小时周期
    let noise = (Random::float() - 0.5) * 10.0
    let value = base_value + noise
    
    let data_point = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("memory.usage")),
      ("metric.value", FloatValue(value)),
      ("service", StringValue("api-gateway")),
      ("environment", StringValue("production"))
    ])
    high_freq_data = high_freq_data.push(data_point)
  }
  
  // 按小时聚合数据
  for hour in 0..=23 {
    let hour_start = base_time + (hour * 3600)
    let hour_end = hour_start + 3600
    
    let hour_data = high_freq_data.filter(fn(point) {
      let timestamp = TimeSeriesPoint::timestamp(point)
      timestamp >= hour_start && timestamp < hour_end
    })
    
    // 计算聚合值
    let aggregated_point = TimeSeriesAggregator::aggregate_hour(ts_aggregator, hour_data, hour_start, [
      ("metric.name", StringValue("memory.usage")),
      ("aggregation.type", StringValue("hourly")),
      ("service", StringValue("api-gateway"))
    ])
    
    Counter::add(aggregation_counter, 1.0)
  }
  
  // 验证聚合结果
  let aggregation_results = TimeSeriesAggregator::get_results(ts_aggregator)
  assert_eq(aggregation_results.hourly_points, 24)
  assert_eq(aggregation_results.original_points, 1440)
  assert_eq(Counter::value(aggregation_counter), 24.0)
}

// 测试3: 时间序列趋势分析
test "时间序列趋势分析测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.trend")
  
  // 创建时间序列趋势分析器
  let trend_analyzer = TimeSeriesTrendAnalyzer::new(meter)
  
  // 创建趋势分析指标
  let trend_counter = Meter::create_counter(meter, "timeseries.trends.analyzed")
  let anomaly_gauge = Meter::create_gauge(meter, "timeseries.anomalies.detected")
  
  // 生成带趋势的时间序列数据
  let mut trend_data = []
  let base_time = Time::now()
  let base_value = 100.0
  
  for i in 0..=335 { // 7天，每30分钟一个点
    let timestamp = base_time + (i * 1800) // 30分钟间隔
    let daily_trend = 5.0 * (i / 48).to_float() // 每天增长5
    let weekly_pattern = 10.0 * (2.0 * 3.14159 * (i % 48).to_float() / 48.0).sin() // 日内周期
    let noise = (Random::float() - 0.5) * 3.0
    let value = base_value + daily_trend + weekly_pattern + noise
    
    let data_point = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("request.rate")),
      ("metric.value", FloatValue(value)),
      ("service", StringValue("user-service")),
      ("period", StringValue("weekly"))
    ])
    trend_data = trend_data.push(data_point)
  }
  
  // 分析趋势
  let trend_analysis = TimeSeriesTrendAnalyzer::analyze_trend(trend_analyzer, trend_data)
  Counter::add(trend_counter, 1.0)
  
  // 检测异常
  let anomaly_count = TimeSeriesTrendAnalyzer::detect_anomalies(trend_analyzer, trend_data)
  Gauge::set(anomaly_gauge, anomaly_count.to_float())
  
  // 验证趋势分析结果
  assert_true(trend_analysis.slope > 0.0) // 应该有上升趋势
  assert_true(trend_analysis.correlation > 0.5) // 强相关性
  assert_true(anomaly_count >= 0)
  assert_eq(Counter::value(trend_counter), 1.0)
}

// 测试4: 时间序列季节性分析
test "时间序列季节性分析测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.seasonality")
  
  // 创建时间序列季节性分析器
  let seasonality_analyzer = TimeSeriesSeasonalityAnalyzer::new(meter)
  
  // 创建季节性分析指标
  let seasonality_counter = Meter::create_counter(meter, "timeseries.seasonality.analyzed")
  let pattern_gauge = Meter::create_gauge(meter, "timeseries.pattern.strength")
  
  // 生成带季节性的时间序列数据
  let mut seasonal_data = []
  let base_time = Time::now()
  
  for i in 0..=167 { // 7天，每小时一个点
    let timestamp = base_time + (i * 3600) // 1小时间隔
    let hour_of_day = i % 24
    let day_of_week = i / 24
    
    // 模拟日内模式（高峰在工作时间）
    let daily_pattern = if hour_of_day >= 9 && hour_of_day <= 17 {
      20.0 + 10.0 * (2.0 * 3.14159 * (hour_of_day - 9).to_float() / 8.0).sin()
    } else {
      5.0 + 2.0 * (2.0 * 3.14159 * hour_of_day.to_float() / 24.0).sin()
    }
    
    // 模拟周内模式（工作日vs周末）
    let weekly_pattern = if day_of_week < 5 { 1.5 } else { 0.7 }
    
    let value = daily_pattern * weekly_pattern + (Random::float() * 2.0)
    
    let data_point = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("traffic.volume")),
      ("metric.value", FloatValue(value)),
      ("service", StringValue("web-server")),
      ("granularity", StringValue("hourly"))
    ])
    seasonal_data = seasonal_data.push(data_point)
  }
  
  // 分析季节性
  let daily_seasonality = TimeSeriesSeasonalityAnalyzer::analyze_daily_pattern(seasonality_analyzer, seasonal_data)
  let weekly_seasonality = TimeSeriesSeasonalityAnalyzer::analyze_weekly_pattern(seasonality_analyzer, seasonal_data)
  
  Counter::add(seasonality_counter, 2.0)
  
  // 计算模式强度
  let pattern_strength = TimeSeriesSeasonalityAnalyzer::calculate_pattern_strength(seasonality_analyzer, daily_seasonality, weekly_seasonality)
  Gauge::set(pattern_gauge, pattern_strength)
  
  // 验证季节性分析结果
  assert_true(daily_seasonality.strength > 0.3) // 应该有明显的日内模式
  assert_true(weekly_seasonality.strength > 0.2) // 应该有周内模式
  assert_true(pattern_strength > 0.0)
  assert_eq(Counter::value(seasonality_counter), 2.0)
}

// 测试5: 时间序列预测
test "时间序列预测测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.forecasting")
  
  // 创建时间序列预测器
  let forecaster = TimeSeriesForecaster::new(meter)
  
  // 创建预测指标
  let forecast_counter = Meter::create_counter(meter, "timeseries.forecasts.generated")
  let accuracy_gauge = Meter::create_gauge(meter, "timeseries.forecast.accuracy")
  
  // 生成历史数据用于训练
  let mut historical_data = []
  let base_time = Time::now() - (7 * 24 * 3600) // 7天前
  
  for i in 0..=167 { // 7天历史数据，每小时一个点
    let timestamp = base_time + (i * 3600)
    let trend = 0.5 * i.to_float() // 线性趋势
    let seasonal = 10.0 * (2.0 * 3.14159 * (i % 24).to_float() / 24.0).sin() // 日内季节性
    let value = 50.0 + trend + seasonal + (Random::float() * 2.0)
    
    let data_point = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("cpu.load")),
      ("metric.value", FloatValue(value)),
      ("service", StringValue("database"))
    ])
    historical_data = historical_data.push(data_point)
  }
  
  // 训练预测模型
  TimeSeriesForecaster::train_model(forecaster, historical_data)
  
  // 生成未来24小时预测
  let forecast_start = base_time + (168 * 3600)
  let forecast_points = TimeSeriesForecaster::forecast(forecaster, forecast_start, 24)
  Counter::add(forecast_counter, 1.0)
  
  // 验证预测结果
  assert_eq(forecast_points.length(), 24)
  
  // 计算预测准确性（使用回测）
  let accuracy = TimeSeriesForecaster::calculate_backtest_accuracy(forecaster, historical_data, 24)
  Gauge::set(accuracy_gauge, accuracy)
  
  // 验证预测准确性
  assert_true(accuracy > 0.0)
  assert_true(accuracy <= 1.0)
  assert_eq(Counter::value(forecast_counter), 1.0)
}

// 测试6: 时间序列数据压缩
test "时间序列数据压缩测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.compression")
  
  // 创建时间序列数据压缩器
  let ts_compressor = TimeSeriesCompressor::new(meter)
  
  // 创建压缩指标
  let compression_counter = Meter::create_counter(meter, "timeseries.compressions")
  let ratio_gauge = Meter::create_gauge(meter, "timeseries.compression.ratio")
  
  // 生成大量时间序列数据
  let mut raw_data = []
  let base_time = Time::now()
  
  for i in 0..=999 { // 1000个数据点
    let timestamp = base_time + (i * 60) // 每分钟一个点
    let value = 100.0 + (i * 0.1) + (Random::float() * 5.0)
    
    let data_point = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("network.throughput")),
      ("metric.value", FloatValue(value)),
      ("interface", StringValue("eth0")),
      ("direction", StringValue(if i % 2 == 0 { "in" } else { "out" }))
    ])
    raw_data = raw_data.push(data_point)
  }
  
  // 计算原始数据大小
  let original_size = raw_data.map(fn(point) { TimeSeriesPoint::size(point) }).reduce(fn(acc, size) { acc + size }, 0)
  
  // 压缩时间序列数据
  let compressed_data = TimeSeriesCompressor::compress(ts_compressor, raw_data)
  Counter::add(compression_counter, 1.0)
  
  // 计算压缩后大小
  let compressed_size = TimeSeriesCompressor::get_compressed_size(ts_compressor)
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  Gauge::set(ratio_gauge, compression_ratio)
  
  // 验证压缩结果
  assert_true(compression_ratio < 1.0) // 压缩后应该更小
  assert_true(compression_ratio > 0.0)
  assert_eq(Counter::value(compression_counter), 1.0)
  
  // 验证解压缩准确性
  let decompressed_data = TimeSeriesCompressor::decompress(ts_compressor, compressed_data)
  assert_eq(decompressed_data.length(), raw_data.length())
}

// 测试7: 时间序列查询和检索
test "时间序列查询和检索测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.query")
  
  // 创建时间序列查询引擎
  let query_engine = TimeSeriesQueryEngine::new(meter)
  
  // 创建查询指标
  let query_counter = Meter::create_counter(meter, "timeseries.queries.executed")
  let latency_histogram = Meter::create_histogram(meter, "timeseries.query.latency")
  
  // 生成测试数据
  let mut test_data = []
  let base_time = Time::now() - (24 * 3600) // 24小时前
  
  for i in 0..=1439 { // 24小时，每分钟一个点
    let timestamp = base_time + (i * 60)
    let value = 50.0 + 20.0 * (i / 720).to_float() + (Random::float() * 10.0)
    
    let data_point = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("response.time")),
      ("metric.value", FloatValue(value)),
      ("endpoint", StringValue("/api/v" + ((i % 4) + 1).to_string() + "/users")),
      ("method", StringValue(if i % 3 == 0 { "GET" } else if i % 3 == 1 { "POST" } else { "PUT" })),
      ("status_code", IntValue(if value > 70.0 { 500 } else { 200 }))
    ])
    test_data = test_data.push(data_point)
  }
  
  // 索引数据
  TimeSeriesQueryEngine::index_data(query_engine, test_data)
  
  // 执行各种查询
  let query_start_time = Time::now()
  
  // 查询1: 时间范围查询
  let time_range_query = TimeRangeQuery::new(base_time + (12 * 3600), base_time + (18 * 3600))
  let time_range_results = TimeSeriesQueryEngine::execute_time_range_query(query_engine, time_range_query)
  
  // 查询2: 标签过滤查询
  let label_filter_query = LabelFilterQuery::new([("endpoint", "/api/v1/users"), ("method", "GET")])
  let label_filter_results = TimeSeriesQueryEngine::execute_label_filter_query(query_engine, label_filter_query)
  
  // 查询3: 值范围查询
  let value_range_query = ValueRangeQuery::new("metric.value", 60.0, 80.0)
  let value_range_results = TimeSeriesQueryEngine::execute_value_range_query(query_engine, value_range_query)
  
  // 查询4: 聚合查询
  let aggregation_query = AggregationQuery::new("metric.value", "avg", base_time, base_time + (24 * 3600), "1h")
  let aggregation_results = TimeSeriesQueryEngine::execute_aggregation_query(query_engine, aggregation_query)
  
  let query_end_time = Time::now()
  let query_latency = Time::duration_between(query_start_time, query_end_time)
  Histogram::record(latency_histogram, query_latency)
  
  Counter::add(query_counter, 4.0)
  
  // 验证查询结果
  assert_true(time_range_results.length() > 0)
  assert_true(label_filter_results.length() > 0)
  assert_true(value_range_results.length() > 0)
  assert_true(aggregation_results.length() > 0)
  assert_eq(Counter::value(query_counter), 4.0)
}

// 测试8: 时间序列异常检测
test "时间序列异常检测测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.anomaly")
  
  // 创建时间序列异常检测器
  let anomaly_detector = TimeSeriesAnomalyDetector::new(meter)
  
  // 创建异常检测指标
  let anomaly_counter = Meter::create_counter(meter, "timeseries.anomalies.detected")
  let severity_gauge = Meter::create_gauge(meter, "timeseries.anomaly.severity")
  
  // 生成包含异常的时间序列数据
  let mut data_with_anomalies = []
  let base_time = Time::now()
  
  for i in 0..=479 { // 8小时，每分钟一个点
    let timestamp = base_time + (i * 60)
    let base_value = 50.0 + 10.0 * (2.0 * 3.14159 * (i % 60).to_float() / 60.0).sin()
    
    // 插入异常值
    let value = if i == 120 || i == 240 || i == 360 {
      base_value + 50.0 // 突然增加50
    } else if i == 180 || i == 300 {
      base_value - 30.0 // 突然减少30
    } else {
      base_value + (Random::float() * 2.0)
    }
    
    let data_point = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("disk.usage")),
      ("metric.value", FloatValue(value)),
      ("mount.point", StringValue("/var"))
    ])
    data_with_anomalies = data_with_anomalies.push(data_point)
  }
  
  // 检测异常
  let anomalies = TimeSeriesAnomalyDetector::detect_anomalies(anomaly_detector, data_with_anomalies)
  Counter::add(anomaly_counter, anomalies.length().to_float())
  
  // 计算异常严重程度
  let max_severity = anomalies.map(fn(anomaly) { TimeSeriesAnomaly::severity(anomaly) }).reduce(fn(acc, severity) { if severity > acc { severity } else { acc } }, 0.0)
  Gauge::set(severity_gauge, max_severity)
  
  // 验证异常检测结果
  assert_true(anomalies.length() >= 5) // 至少应该检测到5个异常
  assert_true(max_severity > 0.0)
  assert_eq(Counter::value(anomaly_counter), anomalies.length().to_float())
}

// 测试9: 时间序列数据相关性分析
test "时间序列数据相关性分析测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.correlation")
  
  // 创建时间序列相关性分析器
  let correlation_analyzer = TimeSeriesCorrelationAnalyzer::new(meter)
  
  // 创建相关性分析指标
  let correlation_counter = Meter::create_counter(meter, "timeseries.correlations.analyzed")
  let correlation_gauge = Meter::create_gauge(meter, "timeseries.correlation.coefficient")
  
  // 生成相关的时间序列数据
  let mut series_a = []
  let mut series_b = []
  let base_time = Time::now()
  
  for i in 0..=239 { // 4小时，每分钟一个点
    let timestamp = base_time + (i * 60)
    let base_trend = i.to_float() * 0.1
    let seasonal = 5.0 * (2.0 * 3.14159 * (i % 60).to_float() / 60.0).sin()
    
    // 系列A：基础值 + 趋势 + 季节性 + 噪声
    let value_a = 100.0 + base_trend + seasonal + (Random::float() * 2.0)
    
    // 系列B：与A相关，但有延迟和不同的噪声
    let value_b = 80.0 + (base_trend * 0.8) + (seasonal * 1.2) + (Random::float() * 3.0)
    
    let point_a = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("cpu.usage")),
      ("metric.value", FloatValue(value_a)),
      ("host", StringValue("server-1"))
    ])
    
    let point_b = TimeSeriesPoint::new(timestamp, [
      ("metric.name", StringValue("memory.usage")),
      ("metric.value", FloatValue(value_b)),
      ("host", StringValue("server-1"))
    ])
    
    series_a = series_a.push(point_a)
    series_b = series_b.push(point_b)
  }
  
  // 分析相关性
  let correlation_result = TimeSeriesCorrelationAnalyzer::analyze_correlation(correlation_analyzer, series_a, series_b)
  Counter::add(correlation_counter, 1.0)
  Gauge::set(correlation_gauge, correlation_result.coefficient)
  
  // 验证相关性分析结果
  assert_true(correlation_result.coefficient > 0.0) // 应该有正相关
  assert_true(correlation_result.coefficient <= 1.0)
  assert_true(correlation_result.p_value < 0.05) // 应该是统计显著的
  assert_eq(Counter::value(correlation_counter), 1.0)
}

// 测试10: 时间序列数据可视化准备
test "时间序列数据可视化准备测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.visualization")
  
  // 创建时间序列可视化准备器
  let viz_preparer = TimeSeriesVisualizationPreparer::new(meter)
  
  // 创建可视化指标
  let viz_counter = Meter::create_counter(meter, "timeseries.visualizations.prepared")
  let chart_gauge = Meter::create_gauge(meter, "timeseries.chart.points")
  
  // 生成多维度时间序列数据
  let mut multi_dim_data = []
  let base_time = Time::now()
  
  for i in 0..=287 { // 24小时，每5分钟一个点
    let timestamp = base_time + (i * 300) // 5分钟间隔
    
    // 生成多个指标
    let cpu_usage = 30.0 + 20.0 * (2.0 * 3.14159 * (i % 96).to_float() / 96.0).sin() + (Random::float() * 5.0)
    let memory_usage = 50.0 + 15.0 * (2.0 * 3.14159 * (i % 96).to_string().to_float() / 96.0).cos() + (Random::float() * 3.0)
    let disk_io = 100.0 + 50.0 * (2.0 * 3.14159 * (i % 48).to_float() / 48.0).sin() + (Random::float() * 10.0)
    let network_io = 200.0 + 100.0 * (2.0 * 3.14159 * (i % 24).to_float() / 24.0).sin() + (Random::float() * 20.0)
    
    let data_point = TimeSeriesPoint::new(timestamp, [
      ("cpu.usage", FloatValue(cpu_usage)),
      ("memory.usage", FloatValue(memory_usage)),
      ("disk.io", FloatValue(disk_io)),
      ("network.io", FloatValue(network_io)),
      ("service", StringValue("web-app")),
      ("instance", StringValue("instance-1"))
    ])
    multi_dim_data = multi_dim_data.push(data_point)
  }
  
  // 准备不同类型的可视化数据
  let line_chart_data = TimeSeriesVisualizationPreparer::prepare_line_chart(viz_preparer, multi_dim_data, ["cpu.usage", "memory.usage"])
  let area_chart_data = TimeSeriesVisualizationPreparer::prepare_area_chart(viz_preparer, multi_dim_data, ["disk.io", "network.io"])
  let heatmap_data = TimeSeriesVisualizationPreparer::prepare_heatmap(viz_preparer, multi_dim_data, "cpu.usage", "hour", "day")
  let scatter_plot_data = TimeSeriesVisualizationPreparer::prepare_scatter_plot(viz_preparer, multi_dim_data, "cpu.usage", "memory.usage")
  
  Counter::add(viz_counter, 4.0)
  
  // 更新图表点数指标
  let total_chart_points = line_chart_data.length() + area_chart_data.length() + heatmap_data.length() + scatter_plot_data.length()
  Gauge::set(chart_gauge, total_chart_points.to_float())
  
  // 验证可视化准备结果
  assert_true(line_chart_data.length() > 0)
  assert_true(area_chart_data.length() > 0)
  assert_true(heatmap_data.length() > 0)
  assert_true(scatter_plot_data.length() > 0)
  assert_true(total_chart_points > 0)
  assert_eq(Counter::value(viz_counter), 4.0)
}