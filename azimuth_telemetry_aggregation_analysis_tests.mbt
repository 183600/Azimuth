// Azimuth Telemetry Aggregation and Analysis Tests
// 遥测数据聚合和分析测试用例 - 专注于数据聚合、统计分析和趋势检测

// Test 1: 多维度数据聚合
test "multi-dimensional data aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_meter")
  let counter = Meter::create_counter(meter, "request_count", Some("Request count"), Some("requests"))
  
  // 创建多维度属性数据
  let services = ["auth", "payment", "inventory", "notification"]
  let endpoints = ["login", "checkout", "update", "send"]
  let regions = ["us-east", "us-west", "eu-central", "ap-southeast"]
  
  // 生成测试数据
  for i in 0..<100 {
    let service_idx = i % services.length()
    let endpoint_idx = (i / 4) % endpoints.length()
    let region_idx = (i / 16) % regions.length()
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "service.name", StringValue(services[service_idx]))
    Attributes::set(attrs, "endpoint.name", StringValue(endpoints[endpoint_idx]))
    Attributes::set(attrs, "deployment.region", StringValue(regions[region_idx]))
    Attributes::set(attrs, "http.status_code", IntValue(200 + (i % 5) * 100))
    
    Counter::add(counter, 1.0, Some(attrs))
  }
  
  // 测试多维度聚合
  let aggregator = MultiDimensionalAggregator::new(counter)
  let dimensions = ["service.name", "endpoint.name"]
  let aggregated_data = Aggregator::group_by(aggregator, dimensions)
  
  assert_true(aggregated_data.length() > 0)
  assert_true(aggregated_data.length() <= services.length() * endpoints.length())
  
  // 验证聚合结果
  for (key, value) in aggregated_data {
    assert_true(value > 0.0)
    assert_true(key.contains("service.name"))
    assert_true(key.contains("endpoint.name"))
  }
}

// Test 2: 时间窗口统计分析
test "time window statistical analysis" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "stats_meter")
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time"), Some("ms"))
  
  // 生成时间序列数据
  let base_time = Time::now()
  let response_times = [120.0, 150.0, 180.0, 200.0, 90.0, 110.0, 160.0, 140.0, 130.0, 170.0]
  
  for i in 0..<response_times.length() {
    let attrs = Attributes::new()
    Attributes::set(attrs, "service.name", StringValue("api_service"))
    Attributes::set(attrs, "endpoint.name", StringValue("process_data"))
    
    // 模拟时间间隔
    let timestamp = base_time + i * 100
    Histogram::record_with_timestamp(histogram, response_times[i], Some(attrs), timestamp)
  }
  
  // 测试时间窗口统计
  let analyzer = TimeWindowAnalyzer::new(histogram)
  let window_size = 500 // 500ms窗口
  let stats = Analyzer::get_window_statistics(analyzer, base_time, base_time + window_size * 2)
  
  match stats {
    Some(window_stats) => {
      assert_true(window_stats.count > 0)
      assert_true(window_stats.min >= 90.0)
      assert_true(window_stats.max <= 200.0)
      assert_true(window_stats.average > 100.0 && window_stats.average < 200.0)
      assert_true(window_stats.percentile_95 > 150.0)
      assert_true(window_stats.percentile_99 >= 180.0)
    }
    None => assert_true(false)
  }
}

// Test 3: 趋势分析和异常检测
test "trend analysis and anomaly detection" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "trend_meter")
  let gauge = Meter::create_gauge(meter, "cpu_usage", Some("CPU usage"), Some("percent"))
  
  // 生成带有趋势和异常的数据
  let base_time = Time::now()
  let normal_values = [30.0, 32.0, 31.0, 33.0, 35.0, 34.0, 36.0, 38.0, 37.0, 39.0]
  let anomaly_values = [85.0, 90.0, 88.0] // 异常值
  
  // 记录正常值
  for i in 0..<normal_values.length() {
    let attrs = Attributes::new()
    Attributes::set(attrs, "host.name", StringValue("server-01"))
    Attributes::set(attrs, "metric.type", StringValue("system"))
    
    Gauge::set(gauge, normal_values[i], Some(attrs))
  }
  
  // 记录异常值
  for i in 0..<anomaly_values.length() {
    let attrs = Attributes::new()
    Attributes::set(attrs, "host.name", StringValue("server-01"))
    Attributes::set(attrs, "metric.type", StringValue("system"))
    
    let timestamp = base_time + (normal_values.length() + i) * 1000
    Gauge::set_with_timestamp(gauge, anomaly_values[i], Some(attrs), timestamp)
  }
  
  // 测试趋势分析
  let trend_analyzer = TrendAnalyzer::new(gauge)
  let trend = Analyzer::detect_trend(trend_analyzer, base_time, base_time + 20000)
  
  match trend {
    Some(trend_result) => {
      assert_true(trend_result.direction == "increasing")
      assert_true(trend_result.strength > 0.5)
      assert_true(trend_result.confidence > 0.8)
    }
    None => assert_true(false)
  }
  
  // 测试异常检测
  let anomaly_detector = AnomalyDetector::new(gauge)
  let anomalies = Detector::find_anomalies(anomaly_detector, base_time, base_time + 20000)
  
  assert_true(anomalies.length() >= 3)
  for anomaly in anomalies {
    assert_true(anomaly.value > 80.0)
    assert_true(anomaly.severity == "high")
    assert_true(anomaly.confidence > 0.9)
  }
}

// Test 4: 百分位数和直方图分析
test "percentile and histogram analysis" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "percentile_meter")
  let histogram = Meter::create_histogram(meter, "latency", Some("Latency"), Some("ms"))
  
  // 生成符合正态分布的测试数据
  let latencies = [
    10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0,
    60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0, 100.0, 105.0,
    110.0, 115.0, 120.0, 125.0, 130.0, 135.0, 140.0, 145.0, 150.0, 200.0,
    250.0, 300.0 // 一些异常高值
  ]
  
  for latency in latencies {
    let attrs = Attributes::new()
    Attributes::set(attrs, "service.name", StringValue("web_service"))
    Attributes::set(attrs, "operation.name", StringValue("database_query"))
    
    Histogram::record(histogram, latency, Some(attrs))
  }
  
  // 测试百分位数计算
  let percentile_calculator = PercentileCalculator::new(histogram)
  let p50 = Calculator::percentile(percentile_calculator, 50.0)
  let p90 = Calculator::percentile(percentile_calculator, 90.0)
  let p95 = Calculator::percentile(percentile_calculator, 95.0)
  let p99 = Calculator::percentile(percentile_calculator, 99.0)
  
  assert_true(p50 >= 70.0 && p50 <= 80.0)
  assert_true(p90 >= 140.0 && p90 <= 160.0)
  assert_true(p95 >= 180.0 && p95 <= 220.0)
  assert_true(p99 >= 250.0)
  
  // 测试直方图分布
  let distribution = Calculator::get_distribution(percentile_calculator, 10) // 10个桶
  assert_true(distribution.length() == 10)
  
  let mut total_count = 0
  for bucket in distribution {
    total_count = total_count + bucket.count
  }
  assert_true(total_count == latencies.length())
}

// Test 5: 相关性分析
test "correlation analysis between metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "correlation_meter")
  
  let request_counter = Meter::create_counter(meter, "requests", Some("Request count"), Some("requests"))
  let error_counter = Meter::create_counter(meter, "errors", Some("Error count"), Some("errors"))
  let response_time_histogram = Meter::create_histogram(meter, "response_time", Some("Response time"), Some("ms"))
  
  // 生成相关的测试数据
  let base_requests = 100
  let base_errors = 5
  let base_response_time = 50.0
  
  for i in 0..<50 {
    let request_factor = 1.0 + (i.to_float() / 50.0) // 请求量递增
    let error_factor = 1.0 + (i.to_float() / 25.0) // 错误率增长更快
    let response_time_factor = 1.0 + (i.to_float() / 40.0) // 响应时间也增长
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "service.name", StringValue("api_gateway"))
    Attributes::set(attrs, "load.level", StringValue("increasing"))
    
    Counter::add(request_counter, base_requests.to_float() * request_factor, Some(attrs))
    Counter::add(error_counter, base_errors.to_float() * error_factor, Some(attrs))
    Histogram::record(response_time_histogram, base_response_time * response_time_factor, Some(attrs))
  }
  
  // 测试相关性分析
  let correlation_analyzer = CorrelationAnalyzer::new()
  Analyzer::add_metric(correlation_analyzer, "requests", request_counter)
  Analyzer::add_metric(correlation_analyzer, "errors", error_counter)
  Analyzer::add_metric(correlation_analyzer, "response_time", response_time_histogram)
  
  let request_error_corr = Analyzer::calculate_correlation(correlation_analyzer, "requests", "errors")
  let request_time_corr = Analyzer::calculate_correlation(correlation_analyzer, "requests", "response_time")
  let error_time_corr = Analyzer::calculate_correlation(correlation_analyzer, "errors", "response_time")
  
  // 请求量和错误量应该有强正相关
  assert_true(request_error_corr > 0.8)
  // 请求量和响应时间应该有正相关
  assert_true(request_time_corr > 0.7)
  // 错误量和响应时间应该有强正相关
  assert_true(error_time_corr > 0.8)
}

// Test 6: 预测分析
test "predictive analysis and forecasting" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "prediction_meter")
  let gauge = Meter::create_gauge(meter, "memory_usage", Some("Memory usage"), Some("MB"))
  
  // 生成带有趋势的时间序列数据
  let base_time = Time::now()
  let base_usage = 1000.0
  let growth_rate = 10.0 // 每小时增长10MB
  
  for i in 0..<24 { // 24小时数据
    let usage = base_usage + (i.to_float() * growth_rate)
    // 添加一些随机波动
    let noise = (i % 3 - 1).to_float() * 5.0
    let final_usage = usage + noise
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "host.name", StringValue("app-server-01"))
    Attributes::set(attrs, "metric.type", StringValue("resource"))
    
    let timestamp = base_time + i * 3600000 // 每小时一个数据点
    Gauge::set_with_timestamp(gauge, final_usage, Some(attrs), timestamp)
  }
  
  // 测试预测分析
  let predictor = PredictiveAnalyzer::new(gauge)
  let forecast_hours = 6
  let forecast = Predictor::forecast(predictor, base_time + 24 * 3600000, forecast_hours)
  
  assert_true(forecast.length() == forecast_hours)
  
  // 验证预测趋势
  let mut last_value = 0.0
  for prediction in forecast {
    assert_true(prediction.value > base_usage)
    assert_true(prediction.confidence > 0.5)
    
    if last_value > 0.0 {
      assert_true(prediction.value >= last_value) // 预测值应该递增
    }
    last_value = prediction.value
  }
  
  // 验证预测准确性（使用历史数据）
  let accuracy = Predictor::backtest_accuracy(predictor, 12) // 使用12小时数据进行回测
  assert_true(accuracy > 0.7) // 准确率应该大于70%
}

// Test 7: 自适应阈值分析
test "adaptive threshold analysis" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "threshold_meter")
  let gauge = Meter::create_gauge(meter, "queue_size", Some("Queue size"), Some("items"))
  
  // 生成带有周期性模式的数据
  let base_time = Time::now()
  let base_size = 50
  let peak_size = 200
  let cycle_length = 24 // 24小时周期
  
  for i in 0..<72 { // 3天的数据
    let cycle_position = i % cycle_length
    let is_peak_time = cycle_position >= 8 && cycle_position <= 16 // 工作时间高峰
    
    let expected_size = if is_peak_time { peak_size } else { base_size }
    // 添加随机波动
    let noise = (i % 7 - 3).to_float() * 10.0
    let final_size = expected_size.to_float() + noise
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "queue.name", StringValue("task_queue"))
    Attributes::set(attrs, "environment", StringValue("production"))
    
    let timestamp = base_time + i * 3600000 // 每小时一个数据点
    Gauge::set_with_timestamp(gauge, final_size, Some(attrs), timestamp)
  }
  
  // 测试自适应阈值
  let threshold_analyzer = AdaptiveThresholdAnalyzer::new(gauge)
  let learning_period = 48 * 3600000 // 前48小时用于学习
  
  let thresholds = Analyzer::learn_thresholds(threshold_analyzer, base_time, base_time + learning_period)
  assert_true(thresholds.length() > 0)
  
  // 验证阈值合理性
  for threshold in thresholds {
    assert_true(threshold.lower > 0)
    assert_true(threshold.upper > threshold.lower)
    assert_true(threshold.confidence > 0.5)
  }
  
  // 测试异常检测
  let test_period_start = base_time + learning_period
  let test_period_end = test_period_start + 24 * 3600000
  let violations = Analyzer::detect_threshold_violations(threshold_analyzer, test_period_start, test_period_end)
  
  // 应该检测到一些违规（高峰期超过阈值）
  assert_true(violations.length() > 0)
  for violation in violations {
    assert_true(violation.severity >= "medium")
    assert_true(violation.confidence > 0.7)
  }
}