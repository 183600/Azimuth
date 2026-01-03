// 时间序列遥测数据处理测试
// 测试时间序列数据的收集、处理、分析和存储

test "时间序列数据的高效收集" {
  // 测试大规模时间序列数据的高效收集
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time-series-test")
  
  // 创建多种指标类型
  let counter = Meter::create_counter(meter, "requests.total")
  let histogram = Meter::create_histogram(meter, "response.time")
  let gauge = Meter::create_gauge(meter, "system.memory")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  
  let start_time = Clock::now_unix_nanos(Clock::system())
  let data_points = 10000
  
  // 生成时间序列数据
  for i in 0..data_points {
    let timestamp = start_time + (i * 1000000L) // 每毫秒一个数据点
    
    // 模拟不同类型的指标数据
    Counter::add(counter, 1.0, [
      ("endpoint", StringValue("/api/" + (i % 10).to_string())),
      ("method", StringValue(if i % 2 == 0 { "GET" } else { "POST" })),
      ("status.code", IntValue(if i % 100 == 0 { 500 } else { 200 }))
    ])
    
    Histogram::record(histogram, (i % 1000).to_double(), [
      ("service", StringValue("api-service")),
      ("version", StringValue("1.0." + (i % 5).to_string()))
    ])
    
    Gauge::record(gauge, (50.0 + (i % 50).to_double()), [
      ("host", StringValue("server-" + (i % 5).to_string())),
      ("region", StringValue("us-west-" + (i % 3).to_string()))
    ])
    
    UpDownCounter::add(updown_counter, if i % 3 == 0 { 1.0 } else { -1.0 }, [
      ("pool", StringValue("connection-pool-" + (i % 3).to_string()))
    ])
  }
  
  // 收集时间序列数据
  let time_series_data = TimeSeriesCollector::collect_all(provider, start_time, start_time + (data_points * 1000000L))
  
  // 验证数据收集完整性
  assert_eq(time_series_data.series_count, 4, "Should have 4 metric series")
  assert_true(time_series_data.total_points >= data_points * 4, "Should collect all data points")
  assert_true(time_series_data.collection_duration < 5000000000L, "Collection should complete within 5 seconds")
}

test "时间序列数据的聚合和降采样" {
  // 测试时间序列数据的聚合和降采样功能
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation-test")
  let counter = Meter::create_counter(meter, "events.count")
  
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // 生成高频数据（每秒1000个点）
  for i in 0..60000 { // 60秒的数据
    let timestamp = base_time + (i * 16666666L) // 约60Hz
    Counter::add(counter, 1.0, [
      ("event.type", StringValue("type-" + (i % 10).to_string())),
      ("source", StringValue("source-" + (i % 5).to_string()))
    ])
  }
  
  // 1分钟聚合
  let minute_aggregated = TimeSeriesAggregator::aggregate_by_interval(
    TimeSeriesCollector::collect(provider, base_time, base_time + 60000000000L),
    60000000000L // 1分钟
  )
  
  // 验证聚合结果
  assert_eq(minute_aggregated.series_count, 50, "Should have 50 series (10 types × 5 sources)")
  assert_eq(minute_aggregated.points_per_series, 1, "Each series should have 1 aggregated point")
  
  // 验证聚合值正确性
  for series in minute_aggregated.series {
    assert_eq(series.points[0].value, 1200.0, "Each series should have 1200 events (60s × 20 events/s)")
  }
  
  // 5分钟聚合
  let five_min_aggregated = TimeSeriesAggregator::aggregate_by_interval(
    TimeSeriesCollector::collect(provider, base_time, base_time + 60000000000L),
    300000000000L // 5分钟
  )
  
  assert_eq(five_min_aggregated.series_count, 50, "Should still have 50 series")
  assert_true(five_min_aggregated.points_per_series <= 12, "Should have at most 12 points (60s / 5s)")
}

test "时间序列数据的实时分析" {
  // 测试时间序列数据的实时分析功能
  let analyzer = TimeSeriesAnalyzer::new()
  
  // 设置实时分析规则
  TimeSeriesAnalyzer::add_rule(analyzer, "error.rate", AnalysisRule::threshold(0.05, "above"))
  TimeSeriesAnalyzer::add_rule(analyzer, "response.time", AnalysisRule::percentile(95.0, 1000.0, "above"))
  TimeSeriesAnalyzer::add_rule(analyzer, "memory.usage", AnalysisRule::trend("increasing", 5))
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "realtime-analysis")
  
  let error_counter = Meter::create_counter(meter, "errors.count")
  let total_counter = Meter::create_counter(meter, "requests.total")
  let response_histogram = Meter::create_histogram(meter, "response.time")
  let memory_gauge = Meter::create_gauge(meter, "system.memory")
  
  let start_time = Clock::now_unix_nanos(Clock::system())
  let alerts = []
  
  // 模拟实时数据流
  for i in 0..1000 {
    let timestamp = start_time + (i * 100000000L) // 每秒一个点
    
    // 模拟错误率变化
    let is_error = i > 500 && i % 10 < 2 // 后半段错误率增加
    if is_error {
      Counter::add(error_counter, 1.0)
    }
    Counter::add(total_counter, 1.0)
    
    // 模拟响应时间变化
    let response_time = if i > 700 { 1200.0 + (i % 200).to_double() } else { 200.0 + (i % 100).to_double() }
    Histogram::record(response_histogram, response_time)
    
    // 模拟内存使用增长
    let memory_usage = 50.0 + (i / 100.0)
    Gauge::record(memory_gauge, memory_usage)
    
    // 每100个点进行一次分析
    if i % 100 == 0 {
      let current_data = TimeSeriesCollector::collect(provider, start_time, timestamp)
      let analysis_result = TimeSeriesAnalyzer::analyze(analyzer, current_data)
      
      for alert in analysis_result.alerts {
        alerts.push(alert)
      }
    }
  }
  
  // 验证检测结果
  let error_rate_alerts = alerts.filter(fn(alert) { alert.metric == "error.rate" })
  let response_time_alerts = alerts.filter(fn(alert) { alert.metric == "response.time" })
  let memory_trend_alerts = alerts.filter(fn(alert) { alert.metric == "memory.usage" })
  
  assert_true(error_rate_alerts.length > 0, "Should detect error rate threshold breach")
  assert_true(response_time_alerts.length > 0, "Should detect response time percentile breach")
  assert_true(memory_trend_alerts.length > 0, "Should detect memory usage trend")
}

test "时间序列数据的异常检测" {
  // 测试时间序列数据的异常检测
  let anomaly_detector = TimeSeriesAnomalyDetector::new()
  
  // 配置异常检测算法
  TimeSeriesAnomalyDetector::set_algorithm(anomaly_detector, "statistical")
  TimeSeriesAnomalyDetector::set_sensitivity(anomaly_detector, 0.95)
  TimeSeriesAnomalyDetector::set_window_size(anomaly_detector, 100)
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "anomaly-detection")
  let gauge = Meter::create_gauge(meter, "metric.with.anomalies")
  
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // 生成正常数据（正态分布）
  for i in 0..500 {
    let timestamp = base_time + (i * 100000000L)
    let normal_value = 100.0 + (Random::next_normal() * 10.0) // 均值100，标准差10
    Gauge::record(gauge, normal_value)
  }
  
  // 插入异常值
  let anomaly_indices = [50, 150, 250, 350, 450]
  for anomaly_index in anomaly_indices {
    let timestamp = base_time + (anomaly_index * 100000000L)
    let anomaly_value = if anomaly_index % 2 == 0 { 200.0 } else { 0.0 }
    Gauge::record(gauge, anomaly_value, [("anomaly", StringValue("true"))])
  }
  
  // 继续生成正常数据
  for i in 500..1000 {
    let timestamp = base_time + (i * 100000000L)
    let normal_value = 100.0 + (Random::next_normal() * 10.0)
    Gauge::record(gauge, normal_value)
  }
  
  // 执行异常检测
  let time_series_data = TimeSeriesCollector::collect(provider, base_time, base_time + 100000000000L)
  let anomalies = TimeSeriesAnomalyDetector::detect(anomaly_detector, time_series_data)
  
  // 验证异常检测结果
  assert_true(anomalies.length >= 5, "Should detect at least 5 anomalies")
  
  // 验证检测到的异常位置
  let detected_positions = anomalies.map(fn(anomaly) { anomaly.index })
  for expected_index in anomaly_indices {
    assert_true(detected_positions.contains(expected_index), 
      "Should detect anomaly at index " + expected_index.to_string())
  }
  
  // 验证异常评分
  for anomaly in anomalies {
    assert_true(anomaly.score > 0.95, "Anomaly score should be above threshold")
    assert_true(anomaly.confidence > 0.8, "Anomaly confidence should be high")
  }
}

test "时间序列数据的预测和趋势分析" {
  // 测试时间序列数据的预测和趋势分析
  let forecaster = TimeSeriesForecaster::new()
  
  // 配置预测模型
  TimeSeriesForecaster::set_model(forecaster, "arima")
  TimeSeriesForecaster::set_horizon(forecaster, 50) // 预测50个点
  TimeSeriesForecaster::set_confidence_interval(forecaster, 0.95)
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "forecasting-test")
  let gauge = Meter::create_gauge(meter, "trend.metric")
  
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // 生成有趋势的数据
  for i in 0..500 {
    let timestamp = base_time + (i * 100000000L)
    // 线性增长 + 季节性 + 噪声
    let trend_value = 50.0 + (i * 0.5) + (10.0 * Math::sin(i * 0.1)) + (Random::next_normal() * 5.0)
    Gauge::record(gauge, trend_value)
  }
  
  // 执行预测
  let historical_data = TimeSeriesCollector::collect(provider, base_time, base_time + 50000000000L)
  let forecast_result = TimeSeriesForecaster::forecast(forecaster, historical_data)
  
  // 验证预测结果
  assert_eq(forecast_result.points.length, 50, "Should forecast 50 points")
  assert_true(forecast_result.confidence_intervals.length == 50, "Should have confidence intervals for each point")
  
  // 验证趋势持续性
  let last_actual_value = historical_data.series[0].points[historical_data.series[0].points.length - 1].value
  let first_predicted_value = forecast_result.points[0].value
  
  assert_true(first_predicted_value > last_actual_value, 
    "Predicted values should continue the upward trend")
  
  // 验证季节性模式
  let seasonal_pattern_detected = false
  for i in 1..forecast_result.points.length {
    let expected_seasonal = 10.0 * Math::sin((500 + i) * 0.1)
    let actual_seasonal = forecast_result.points[i].value - forecast_result.points[0].value - (i * 0.5)
    if Math::abs(actual_seasonal - expected_seasonal) < 5.0 {
      seasonal_pattern_detected = true
      break
    }
  }
  assert_true(seasonal_pattern_detected, "Should detect and continue seasonal patterns")
}

test "时间序列数据的压缩和存储优化" {
  // 测试时间序列数据的压缩和存储优化
  let compressor = TimeSeriesCompressor::new()
  
  // 配置压缩策略
  TimeSeriesCompressor::set_strategy(compressor, "adaptive")
  TimeSeriesCompressor::set_error_tolerance(compressor, 0.01) // 1%误差容忍度
  TimeSeriesCompressor::set_compression_target(compressor, 0.7) // 目标压缩率70%
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "compression-test")
  let gauge = Meter::create_gauge(meter, "compression.metric")
  
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // 生成不同特征的数据
  // 1. 稳定数据
  for i in 0..1000 {
    let timestamp = base_time + (i * 1000000L)
    Gauge::record(gauge, 100.0, [("type", StringValue("stable"))])
  }
  
  // 2. 线性增长数据
  for i in 0..1000 {
    let timestamp = base_time + 1000000000L + (i * 1000000L)
    Gauge::record(gauge, 100.0 + (i * 0.01), [("type", StringValue("linear"))])
  }
  
  // 3. 随机数据
  for i in 0..1000 {
    let timestamp = base_time + 2000000000L + (i * 1000000L)
    Gauge::record(gauge, 100.0 + (Random::next_normal() * 10.0), [("type", StringValue("random"))])
  }
  
  // 收集并压缩数据
  let raw_data = TimeSeriesCollector::collect(provider, base_time, base_time + 3000000000L)
  let compressed_data = TimeSeriesCompressor::compress(compressor, raw_data)
  
  // 验证压缩效果
  let compression_ratio = compressed_data.size.to_double() / raw_data.size.to_double()
  assert_true(compression_ratio < 0.5, "Compression ratio should be better than 50%")
  
  // 验证不同数据类型的压缩效果
  let stable_compression = compressed_data.series_compression_ratios.get("stable")
  let linear_compression = compressed_data.series_compression_ratios.get("linear")
  let random_compression = compressed_data.series_compression_ratios.get("random")
  
  assert_true(stable_compression.unwrap < 0.1, "Stable data should compress very well")
  assert_true(linear_compression.unwrap < 0.3, "Linear data should compress reasonably")
  assert_true(random_compression.unwrap < 0.8, "Random data should still compress somewhat")
  
  // 验证解压缩精度
  let decompressed_data = TimeSeriesCompressor::decompress(compressor, compressed_data)
  let reconstruction_error = TimeSeriesCompressor::calculate_error(raw_data, decompressed_data)
  
  assert_true(reconstruction_error < 0.01, "Reconstruction error should be within tolerance")
}

test "时间序列数据的实时查询和可视化" {
  // 测试时间序列数据的实时查询和可视化
  let query_engine = TimeSeriesQueryEngine::new()
  let visualizer = TimeSeriesVisualizer::new()
  
  // 设置索引
  TimeSeriesQueryEngine::create_index(query_engine, "metric.name")
  TimeSeriesQueryEngine::create_index(query_engine, "timestamp")
  TimeSeriesQueryEngine::create_index(query_engine, "tags")
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "query-test")
  
  // 创建多种指标
  let metrics = [
    ("cpu.usage", Meter::create_gauge(meter, "cpu.usage")),
    ("memory.usage", Meter::create_gauge(meter, "memory.usage")),
    ("network.bandwidth", Meter::create_counter(meter, "network.bandwidth")),
    ("disk.io", Meter::create_counter(meter, "disk.io"))
  ]
  
  let base_time = Clock::now_unix_nanos(Clock::system())
  
  // 生成带有多维度标签的数据
  for i in 0..10000 {
    let timestamp = base_time + (i * 1000000L)
    
    for (metric_name, metric) in metrics {
      let value = match metric_name {
        "cpu.usage" => 50.0 + (Math::sin(i * 0.01) * 20.0) + (Random::next_normal() * 5.0)
        "memory.usage" => 60.0 + (i * 0.001) + (Random::next_normal() * 3.0)
        "network.bandwidth" => (1000000.0 + (Random::next_normal() * 100000.0))
        "disk.io" => (500000.0 + (Random::next_normal() * 50000.0))
        _ => 0.0
      }
      
      let tags = [
        ("host", StringValue("server-" + (i % 10).to_string())),
        ("region", StringValue("us-west-" + (i % 3).to_string())),
        ("environment", StringValue(if i % 2 == 0 { "prod" } else { "staging" }))
      ]
      
      match metric {
        Gauge(g) => Gauge::record(g, value, tags)
        Counter(c) => Counter::add(c, value, tags)
        _ => ()
      }
    }
  }
  
  // 构建数据集
  let dataset = TimeSeriesQueryEngine::build_dataset(query_engine, provider, base_time, base_time + 10000000000L)
  
  // 执行复杂查询
  let query1 = TimeSeriesQuery::range("cpu.usage", base_time, base_time + 5000000000L)
    .filter("environment", "prod")
    .filter("region", "us-west-1")
    .aggregate("avg", 60000000000L) // 1分钟平均
  
  let result1 = TimeSeriesQueryEngine::execute(query_engine, dataset, query1)
  
  // 验证查询结果
  assert_true(result1.series.length > 0, "Query should return results")
  assert_true(result1.series[0].points.length <= 83, "Should have at most 83 points (5s / 1min)")
  
  // 执行多指标关联查询
  let query2 = TimeSeriesQuery::correlation(["cpu.usage", "memory.usage"])
    .filter("host", "server-1")
    .window(60000000000L) // 1分钟窗口
  
  let result2 = TimeSeriesQueryEngine::execute(query_engine, dataset, query2)
  
  // 验证关联查询结果
  assert_true(result2.correlation_coefficient > 0.5, "CPU and memory usage should be correlated")
  
  // 生成可视化数据
  let chart_data = TimeSeriesVisualizer::create_chart(visualizer, result1, ChartType::TimeSeries)
  
  // 验证可视化数据
  assert_true(chart_data.datasets.length > 0, "Should have chart datasets")
  assert_true(chart_data.datasets[0].data.length > 0, "Should have data points")
  assert_true(chart_data.options.time_range.start == base_time, "Chart should reflect query time range")
}