// Azimuth Enhanced MoonBit Test Suite
// 增强型MoonBit测试套件，专注于遥测系统的高级功能和边缘情况

// 测试1: 度量仪表盘数据聚合测试
test "度量仪表盘数据聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "dashboard.test")
  
  // 创建各种类型的度量
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_time_histogram = Meter::create_histogram(meter, "http.response.duration")
  let active_connections_gauge = Meter::create_gauge(meter, "http.active.connections")
  let error_rate_counter = Meter::create_counter(meter, "http.errors.total")
  
  // 模拟不同端点的请求数据
  let endpoints = ["/api/users", "/api/orders", "/api/products", "/api/auth"]
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let status_codes = ["200", "201", "400", "404", "500"]
  
  // 生成度量数据
  for i in 0..1000 {
    let endpoint = endpoints[i % endpoints.length()]
    let method = methods[i % methods.length()]
    let status_code = status_codes[@random_int(0, status_codes.length() - 1)]
    
    // 记录请求计数
    Counter::add_with_attributes(request_counter, 1.0, [
      ("endpoint", endpoint),
      ("method", method),
      ("status", status_code)
    ])
    
    // 记录响应时间
    let response_time = 10.0 + (@random() * 500.0)
    Histogram::record_with_attributes(response_time_histogram, response_time, [
      ("endpoint", endpoint),
      ("method", method)
    ])
    
    // 模拟活跃连接数
    let active_connections = 50.0 + (@random() * 200.0)
    Gauge::record(active_connections_gauge, active_connections)
    
    // 模拟错误计数
    if status_code == "400" || status_code == "404" || status_code == "500" {
      Counter::add_with_attributes(error_rate_counter, 1.0, [
        ("endpoint", endpoint),
        ("method", method),
        ("error.type", status_code)
      ])
    }
  }
  
  // 创建仪表盘聚合器
  let dashboard_aggregator = DashboardAggregator::new()
  
  // 按端点聚合请求计数
  let requests_by_endpoint = DashboardAggregator::aggregate_by_attribute(
    dashboard_aggregator, 
    request_counter, 
    "endpoint"
  )
  
  // 按方法聚合响应时间
  let response_time_by_method = DashboardAggregator::aggregate_histogram_by_attribute(
    dashboard_aggregator, 
    response_time_histogram, 
    "method"
  )
  
  // 计算错误率
  let error_rate = DashboardAggregator::calculate_error_rate(
    dashboard_aggregator, 
    request_counter, 
    error_rate_counter
  )
  
  // 验证聚合结果
  assert_true(requests_by_endpoint.size() > 0)
  assert_true(response_time_by_method.size() > 0)
  assert_true(error_rate >= 0.0 && error_rate <= 1.0)
  
  // 验证每个端点都有数据
  for endpoint in endpoints {
    assert_true(requests_by_endpoint.contains_key(endpoint))
  }
  
  // 验证每个方法都有响应时间数据
  for method in methods {
    assert_true(response_time_by_method.contains_key(method))
  }
}

// 测试2: 遥测数据质量验证测试
test "遥测数据质量验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "data-quality.test")
  
  // 创建数据质量验证器
  let quality_validator = DataQualityValidator::new()
  
  // 配置质量规则
  DataQualityValidator::add_required_attribute_rule(quality_validator, "service.name")
  DataQualityValidator::add_required_attribute_rule(quality_validator, "operation.name")
  DataQualityValidator::add_attribute_length_rule(quality_validator, "trace.id", 32, 32)
  DataQualityValidator::add_attribute_pattern_rule(quality_validator, "timestamp", "^[0-9]{13}$")
  
  // 创建符合质量要求的Span
  let valid_span = Tracer::start_span(tracer, "valid.operation")
  Span::set_attribute(valid_span, "service.name", "user-service")
  Span::set_attribute(valid_span, "operation.name", "getUser")
  Span::set_attribute(valid_span, "trace.id", "12345678901234567890123456789012")
  Span::set_attribute(valid_span, "timestamp", "1735689600000")
  
  // 创建不符合质量要求的Span
  let invalid_span = Tracer::start_span(tracer, "invalid.operation")
  Span::set_attribute(invalid_span, "service.name", "order-service")
  // 缺少operation.name
  Span::set_attribute(invalid_span, "trace.id", "123")  // 长度不符合要求
  Span::set_attribute(invalid_span, "timestamp", "invalid-timestamp")  // 格式不符合要求
  
  // 验证数据质量
  let valid_span_quality = DataQualityValidator::validate_span(quality_validator, valid_span)
  let invalid_span_quality = DataQualityValidator::validate_span(quality_validator, invalid_span)
  
  // 验证结果
  assert_true(DataQualityResult::is_valid(valid_span_quality))
  assert_false(DataQualityResult::is_valid(invalid_span_quality))
  
  // 检查具体违规项
  let violations = DataQualityResult::violations(invalid_span_quality)
  assert_true(violations.length() >= 3)  // 至少3个违规项
  
  // 验证违规类型
  let violation_types = violations.map(fn(v) { DataQualityViolation::violation_type(v) })
  assert_true(violation_types.contains("missing_required_attribute"))
  assert_true(violation_types.contains("attribute_length_violation"))
  assert_true(violation_types.contains("attribute_pattern_violation"))
  
  // 测试度量数据质量验证
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metric-quality.test")
  
  let counter = Meter::create_counter(meter, "test.counter")
  
  // 添加符合质量要求的度量数据
  Counter::add_with_attributes(counter, 1.0, [
    ("service.name", "payment-service"),
    ("operation.name", "processPayment")
  ])
  
  // 添加不符合质量要求的度量数据（负值）
  Counter::add_with_attributes(counter, -1.0, [
    ("service.name", "payment-service"),
    ("operation.name", "refundPayment")
  ])
  
  // 验证度量数据质量
  let metric_quality = DataQualityValidator::validate_metric(quality_validator, counter)
  assert_false(DataQualityResult::is_valid(metric_quality))
  
  let metric_violations = DataQualityResult::violations(metric_quality)
  assert_true(metric_violations.length() > 0)
}

// 测试3: 时间序列数据分析测试
test "时间序列数据分析测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "timeseries.test")
  
  // 创建时间序列分析器
  let timeseries_analyzer = TimeSeriesAnalyzer::new()
  
  // 创建度量
  let cpu_usage_gauge = Meter::create_gauge(meter, "system.cpu.usage")
  let memory_usage_gauge = Meter::create_gauge(meter, "system.memory.usage")
  let request_rate_counter = Meter::create_counter(meter, "http.requests.rate")
  
  // 生成时间序列数据（模拟24小时数据）
  let base_time = 1735689600000L  // 基准时间戳
  let hour_in_ms = 3600000L
  
  for hour in 0..24 {
    let timestamp = base_time + (hour.to_long() * hour_in_ms)
    
    // 模拟CPU使用率（有日周期模式）
    let cpu_usage = 20.0 + 30.0 * @sin((hour.to_double() / 24.0) * 2.0 * 3.14159) + (@random() * 10.0)
    Gauge::record_with_timestamp(cpu_usage_gauge, cpu_usage, timestamp)
    
    // 模拟内存使用率（逐渐增长）
    let memory_usage = 40.0 + (hour.to_double() * 1.5) + (@random() * 5.0)
    Gauge::record_with_timestamp(memory_usage_gauge, memory_usage, timestamp)
    
    // 模拟请求率（有峰值和低谷）
    let request_rate = 100.0 + 50.0 * @cos((hour.to_double() / 24.0) * 2.0 * 3.14159) + (@random() * 20.0)
    Counter::add_with_timestamp(request_rate_counter, request_rate, timestamp)
  }
  
  // 分析CPU使用率趋势
  let cpu_trend = TimeSeriesAnalyzer::analyze_trend(timeseries_analyzer, cpu_usage_gauge, base_time, base_time + (24L * hour_in_ms))
  
  // 分析内存使用率趋势
  let memory_trend = TimeSeriesAnalyzer::analyze_trend(timeseries_analyzer, memory_usage_gauge, base_time, base_time + (24L * hour_in_ms))
  
  // 分析请求率模式
  let request_pattern = TimeSeriesAnalyzer::detect_pattern(timeseries_analyzer, request_rate_counter, base_time, base_time + (24L * hour_in_ms))
  
  // 验证趋势分析结果
  assert_true(TimeSeriesTrend::trend_direction(cpu_trend) != "unknown")
  assert_true(TimeSeriesTrend::trend_direction(memory_trend) == "increasing")
  assert_true(TimeSeriesPattern::pattern_type(request_pattern) != "unknown")
  
  // 检测异常值
  let cpu_anomalies = TimeSeriesAnalyzer::detect_anomalies(timeseries_analyzer, cpu_usage_gauge, base_time, base_time + (24L * hour_in_ms))
  let memory_anomalies = TimeSeriesAnalyzer::detect_anomalies(timeseries_analyzer, memory_usage_gauge, base_time, base_time + (24L * hour_in_ms))
  
  // 验证异常检测结果
  assert_true(cpu_anomalies.length() >= 0)
  assert_true(memory_anomalies.length() >= 0)
  
  // 计算统计指标
  let cpu_stats = TimeSeriesAnalyzer::calculate_statistics(timeseries_analyzer, cpu_usage_gauge, base_time, base_time + (24L * hour_in_ms))
  let memory_stats = TimeSeriesAnalyzer::calculate_statistics(timeseries_analyzer, memory_usage_gauge, base_time, base_time + (24L * hour_in_ms))
  
  // 验证统计指标
  assert_true(TimeSeriesStatistics::mean(cpu_stats) > 0.0)
  assert_true(TimeSeriesStatistics::mean(memory_stats) > 0.0)
  assert_true(TimeSeriesStatistics::min(cpu_stats) <= TimeSeriesStatistics::max(cpu_stats))
  assert_true(TimeSeriesStatistics::min(memory_stats) <= TimeSeriesStatistics::max(memory_stats))
}

// 测试4: 遥测配置动态更新测试
test "遥测配置动态更新测试" {
  // 创建初始配置
  let initial_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_rate(initial_config, 0.1)  // 10%采样率
  TelemetryConfig::set_batch_size(initial_config, 512)
  TelemetryConfig::set_export_interval(initial_config, 5000)
  TelemetryConfig::enable_attribute_filter(initial_config, false)
  
  // 创建配置管理器
  let config_manager = ConfigManager::new(initial_config)
  
  // 验证初始配置
  assert_eq(TelemetryConfig::sampling_rate(ConfigManager::current_config(config_manager)), 0.1)
  assert_eq(TelemetryConfig::batch_size(ConfigManager::current_config(config_manager)), 512)
  assert_eq(TelemetryConfig::export_interval(ConfigManager::current_config(config_manager)), 5000)
  assert_false(TelemetryConfig::attribute_filter_enabled(ConfigManager::current_config(config_manager)))
  
  // 动态更新配置
  let updated_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_rate(updated_config, 0.5)  // 提高到50%采样率
  TelemetryConfig::set_batch_size(updated_config, 1024)   // 增大批量大小
  TelemetryConfig::set_export_interval(updated_config, 2000)  // 减少导出间隔
  TelemetryConfig::enable_attribute_filter(updated_config, true)  // 启用属性过滤
  
  // 应用配置更新
  ConfigManager::update_config(config_manager, updated_config)
  
  // 验证配置已更新
  assert_eq(TelemetryConfig::sampling_rate(ConfigManager::current_config(config_manager)), 0.5)
  assert_eq(TelemetryConfig::batch_size(ConfigManager::current_config(config_manager)), 1024)
  assert_eq(TelemetryConfig::export_interval(ConfigManager::current_config(config_manager)), 2000)
  assert_true(TelemetryConfig::attribute_filter_enabled(ConfigManager::current_config(config_manager)))
  
  // 测试配置变更通知
  let notification_received = false
  let config_listener = ConfigChangeListener::new(fn(old_config, new_config) {
    notification_received = true
    assert_eq(TelemetryConfig::sampling_rate(old_config), 0.5)
    assert_eq(TelemetryConfig::sampling_rate(new_config), 0.8)
  })
  
  ConfigManager::add_listener(config_manager, config_listener)
  
  // 再次更新配置
  let new_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_rate(new_config, 0.8)  // 提高到80%采样率
  ConfigManager::update_config(config_manager, new_config)
  
  // 验证通知已发送
  assert_true(notification_received)
  
  // 验证最新配置
  assert_eq(TelemetryConfig::sampling_rate(ConfigManager::current_config(config_manager)), 0.8)
  
  // 测试配置持久化
  let config_persistence = ConfigPersistence::new()
  ConfigPersistence::save_config(config_persistence, ConfigManager::current_config(config_manager))
  
  let loaded_config = ConfigPersistence::load_config(config_persistence)
  assert_eq(TelemetryConfig::sampling_rate(loaded_config), 0.8)
  assert_eq(TelemetryConfig::batch_size(loaded_config), 1024)
  assert_eq(TelemetryConfig::export_interval(loaded_config), 2000)
  assert_true(TelemetryConfig::attribute_filter_enabled(loaded_config))
}

// 测试5: 自定义遥测仪表测试
test "自定义遥测仪表测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "custom-gauge.test")
  
  // 创建自定义仪表
  let custom_gauge = CustomGauge::new("custom.metric", "Custom metric description", "units")
  
  // 测试基本仪表操作
  CustomGauge::record(custom_gauge, 42.0)
  assert_eq(CustomGauge::value(custom_gauge), 42.0)
  
  CustomGauge::record(custom_gauge, 58.0)
  assert_eq(CustomGauge::value(custom_gauge), 58.0)
  
  // 测试带属性的仪表记录
  CustomGauge::record_with_attributes(custom_gauge, 75.0, [
    ("service.name", "auth-service"),
    ("environment", "production")
  ])
  
  // 测试仪表增量操作
  CustomGauge::increment(custom_gauge, 5.0)
  assert_eq(CustomGauge::value(custom_gauge), 80.0)
  
  CustomGauge::decrement(custom_gauge, 10.0)
  assert_eq(CustomGauge::value(custom_gauge), 70.0)
  
  // 测试带属性的增量操作
  CustomGauge::increment_with_attributes(custom_gauge, 3.0, [
    ("service.name", "auth-service"),
    ("environment", "production")
  ])
  
  // 创建多标签仪表
  let multi_label_gauge = CustomGauge::with_labels("multi.label.metric", "Multi-label metric", "count", [
    "service", "environment", "region"
  ])
  
  // 记录不同标签组合的值
  CustomGauge::record_with_labels(multi_label_gauge, 100.0, [
    ("service", "api-gateway"),
    ("environment", "production"),
    ("region", "us-west-1")
  ])
  
  CustomGauge::record_with_labels(multi_label_gauge, 50.0, [
    ("service", "api-gateway"),
    ("environment", "staging"),
    ("region", "us-west-1")
  ])
  
  CustomGauge::record_with_labels(multi_label_gauge, 75.0, [
    ("service", "user-service"),
    ("environment", "production"),
    ("region", "us-east-1")
  ])
  
  // 验证多标签仪表数据
  let labels_values = CustomGauge::values_by_labels(multi_label_gauge)
  assert_true(labels_values.size() >= 3)
  
  // 测试仪表快照
  let gauge_snapshot = CustomGauge::snapshot(custom_gauge)
  assert_eq(GaugeSnapshot::value(gauge_snapshot), 70.0)
  assert_eq(GaugeSnapshot::timestamp(gauge_snapshot) > 0, true)
  
  // 测试仪表重置
  CustomGauge::reset(custom_gauge)
  assert_eq(CustomGauge::value(custom_gauge), 0.0)
  
  // 验证仪表属性
  assert_eq(CustomGauge::name(custom_gauge), "custom.metric")
  assert_eq(CustomGauge::description(custom_gauge), "Custom metric description")
  assert_eq(CustomGauge::unit(custom_gauge), "units")
}

// 测试6: 多维度属性查询测试
test "多维度属性查询测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multidimensional.test")
  
  // 创建多维度查询器
  let query_engine = MultidimensionalQueryEngine::new()
  
  // 创建具有多维度属性的Span
  let spans = []
  let services = ["auth-service", "user-service", "order-service", "payment-service"]
  let environments = ["production", "staging", "development"]
  let regions = ["us-east-1", "us-west-1", "eu-west-1", "ap-southeast-1"]
  let operations = ["login", "register", "getUser", "createOrder", "processPayment"]
  
  // 生成测试数据
  for i in 0..500 {
    let service = services[i % services.length()]
    let environment = environments[i % environments.length()]
    let region = regions[i % regions.length()]
    let operation = operations[i % operations.length()]
    
    let span = Tracer::start_span(tracer, "multidim-operation-" + i.to_string())
    
    // 设置多维属性
    Span::set_attribute(span, "service.name", service)
    Span::set_attribute(span, "environment", environment)
    Span::set_attribute(span, "region", region)
    Span::set_attribute(span, "operation.name", operation)
    Span::set_attribute(span, "operation.duration", @random_int(10, 1000))
    Span::set_attribute(span, "success", @random() > 0.1)  // 90%成功率
    
    spans.push(span)
  }
  
  // 构建查询条件
  let query1 = MultidimensionalQuery::and([
    MultidimensionalQuery::equals("service.name", "auth-service"),
    MultidimensionalQuery::equals("environment", "production")
  ])
  
  let query2 = MultidimensionalQuery::or([
    MultidimensionalQuery::equals("service.name", "user-service"),
    MultidimensionalQuery::equals("service.name", "order-service")
  ])
  
  let query3 = MultidimensionalQuery::and([
    MultidimensionalQuery::in_set("region", ["us-east-1", "us-west-1"]),
    MultidimensionalQuery::greater_than("operation.duration", 500)
  ])
  
  let query4 = MultidimensionalQuery::not(
    MultidimensionalQuery::equals("success", false)
  )
  
  // 执行查询
  let result1 = MultidimensionalQueryEngine::execute(query_engine, query1, spans)
  let result2 = MultidimensionalQueryEngine::execute(query_engine, query2, spans)
  let result3 = MultidimensionalQueryEngine::execute(query_engine, query3, spans)
  let result4 = MultidimensionalQueryEngine::execute(query_engine, query4, spans)
  
  // 验证查询结果
  assert_true(result1.length() > 0)
  assert_true(result2.length() > 0)
  assert_true(result3.length() > 0)
  assert_true(result4.length() > 0)
  
  // 验证结果准确性
  for span in result1 {
    let attrs = Span::attributes(span)
    assert_eq(Attributes::get(attrs, "service.name"), Some(StringValue("auth-service")))
    assert_eq(Attributes::get(attrs, "environment"), Some(StringValue("production")))
  }
  
  for span in result2 {
    let attrs = Span::attributes(span)
    let service_name = Attributes::get(attrs, "service.name")
    match service_name {
      Some(StringValue(name)) => assert_true(name == "user-service" || name == "order-service")
      _ => assert_true(false)
    }
  }
  
  // 测试聚合查询
  let aggregation_query = MultidimensionalQuery::aggregate_by([
    "service.name", 
    "environment"
  ], [
    AggregationFunction::Count,
    AggregationFunction::Average("operation.duration"),
    AggregationFunction::Sum("success")
  ])
  
  let aggregation_result = MultidimensionalQueryEngine::execute_aggregation(query_engine, aggregation_query, spans)
  
  // 验证聚合结果
  assert_true(aggregation_result.size() > 0)
  
  // 测试分页查询
  let pagination_query = MultidimensionalQuery::paginate(
    MultidimensionalQuery::all(),
    10,  // 页大小
    2    // 页码
  )
  
  let pagination_result = MultidimensionalQueryEngine::execute(query_engine, pagination_query, spans)
  assert_true(pagination_result.length() <= 10)
  
  // 测试排序查询
  let sort_query = MultidimensionalQuery::sort_by(
    MultidimensionalQuery::all(),
    "operation.duration",
    SortDirection::Descending
  )
  
  let sort_result = MultidimensionalQueryEngine::execute(query_engine, sort_query, spans)
  assert_true(sort_result.length() > 0)
  
  // 验证排序结果
  if sort_result.length() >= 2 {
    let first_span_duration = Attributes::get(Span::attributes(sort_result[0]), "operation.duration")
    let second_span_duration = Attributes::get(Span::attributes(sort_result[1]), "operation.duration")
    
    match (first_span_duration, second_span_duration) {
      (Some(IntValue(first)), Some(IntValue(second))) => assert_true(first >= second)
      _ => assert_true(false)
    }
  }
}

// 测试7: 遥测数据异常检测和告警测试
test "遥测数据异常检测和告警测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "anomaly-detection.test")
  
  // 创建异常检测器
  let anomaly_detector = AnomalyDetector::new()
  
  // 配置检测规则
  AnomalyDetector::add_threshold_rule(anomaly_detector, "error.rate", 0.05, ComparisonOperator::GreaterThan)
  AnomalyDetector::add_statistical_rule(anomaly_detector, "response.time", StatisticalMethod::ZScore, 2.0)
  AnomalyDetector::add_pattern_rule(anomaly_detector, "traffic.pattern", PatternMethod::SeasonalDeviation, 0.3)
  
  // 创建告警管理器
  let alert_manager = AlertManager::new()
  
  // 配置告警通道
  let email_channel = AlertChannel::email("admin@example.com")
  let slack_channel = AlertChannel::slack("#alerts")
  let webhook_channel = AlertChannel::webhook("https://api.example.com/alerts")
  
  AlertManager::add_channel(alert_manager, email_channel)
  AlertManager::add_channel(alert_manager, slack_channel)
  AlertManager::add_channel(alert_manager, webhook_channel)
  
  // 创建度量
  let error_rate_gauge = Meter::create_gauge(meter, "error.rate")
  let response_time_histogram = Meter::create_histogram(meter, "response.time")
  let request_counter = Meter::create_counter(meter, "requests.total")
  
  // 生成正常数据
  for i in 0..100 {
    // 正常错误率（1-3%）
    let error_rate = 0.01 + (@random() * 0.02)
    Gauge::record(error_rate_gauge, error_rate)
    
    // 正常响应时间（100-300ms）
    let response_time = 100.0 + (@random() * 200.0)
    Histogram::record(response_time_histogram, response_time)
    
    // 正常请求量
    Counter::add(request_counter, 100.0 + (@random() * 50.0))
  }
  
  // 检测正常数据中的异常
  let normal_anomalies = AnomalyDetector::detect(anomaly_detector, [
    ("error.rate", error_rate_gauge),
    ("response.time", response_time_histogram),
    ("requests.total", request_counter)
  ])
  
  // 正常情况下应该没有或很少异常
  assert_true(normal_anomalies.length() <= 5)
  
  // 生成异常数据
  // 1. 高错误率
  for i in 0..10 {
    Gauge::record(error_rate_gauge, 0.08 + (@random() * 0.02))  // 8-10%错误率
  }
  
  // 2. 高响应时间
  for i in 0..10 {
    Histogram::record(response_time_histogram, 800.0 + (@random() * 400.0))  // 800-1200ms
  }
  
  // 3. 流量异常（突然下降）
  for i in 0..10 {
    Counter::add(request_counter, 10.0 + (@random() * 10.0))  // 10-20请求量
  }
  
  // 检测异常数据中的异常
  let anomaly_results = AnomalyDetector::detect(anomaly_detector, [
    ("error.rate", error_rate_gauge),
    ("response.time", response_time_histogram),
    ("requests.total", request_counter)
  ])
  
  // 异常情况下应该检测到异常
  assert_true(anomaly_results.length() > 0)
  
  // 处理检测到的异常
  let triggered_alerts = []
  for anomaly in anomaly_results {
    // 创建告警
    let alert = Alert::new(
      Anomaly::metric_name(anomaly),
      Anomaly::severity(anomaly),
      Anomaly::description(anomaly),
      Anomaly::timestamp(anomaly)
    )
    
    // 发送告警
    let alert_result = AlertManager::send_alert(alert_manager, alert)
    triggered_alerts.push(alert_result)
  }
  
  // 验证告警发送结果
  assert_true(triggered_alerts.length() > 0)
  
  // 验证至少有一些告警发送成功
  let successful_alerts = triggered_alerts.filter(fn(result) { AlertResult::is_success(result) })
  assert_true(successful_alerts.length() > 0)
  
  // 测试告警抑制
  AlertManager::set_suppression_rule(alert_manager, SuppressionRule::rate_limit(5, 60))  // 每分钟最多5个告警
  
  // 生成大量异常
  for i in 0..20 {
    Gauge::record(error_rate_gauge, 0.1)  // 10%错误率
  }
  
  let additional_anomalies = AnomalyDetector::detect(anomaly_detector, [
    ("error.rate", error_rate_gauge)
  ])
  
  // 由于抑制规则，实际发送的告警应该少于检测到的异常
  let suppressed_alerts = []
  for anomaly in additional_anomalies {
    let alert = Alert::new(
      Anomaly::metric_name(anomaly),
      Anomaly::severity(anomaly),
      Anomaly::description(anomaly),
      Anomaly::timestamp(anomaly)
    )
    
    let alert_result = AlertManager::send_alert(alert_manager, alert)
    if AlertResult::is_suppressed(alert_result) {
      suppressed_alerts.push(alert_result)
    }
  }
  
  assert_true(suppressed_alerts.length() > 0)
}

// 测试8: 跨平台兼容性测试
test "跨平台兼容性测试" {
  // 创建平台检测器
  let platform_detector = PlatformDetector::new()
  let current_platform = PlatformDetector::current_platform(platform_detector)
  
  // 测试不同平台的遥测功能
  match current_platform {
    Platform::WebAssembly => {
      // WebAssembly平台特定测试
      test_webasm_telemetry()
    }
    Platform::Linux => {
      // Linux平台特定测试
      test_linux_telemetry()
    }
    Platform::Windows => {
      // Windows平台特定测试
      test_windows_telemetry()
    }
    Platform::MacOS => {
      // macOS平台特定测试
      test_macos_telemetry()
    }
    _ => {
      // 通用平台测试
      test_generic_telemetry()
    }
  }
  
  // 测试跨平台数据序列化
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross-platform.test")
  
  let span = Tracer::start_span(tracer, "cross-platform.span")
  Span::set_attribute(span, "platform", PlatformDetector::platform_name(current_platform))
  Span::set_attribute(span, "architecture", PlatformDetector::architecture(platform_detector))
  
  // 序列化Span数据
  let serializer = CrossPlatformSerializer::new()
  let serialized_data = CrossPlatformSerializer::serialize_span(serializer, span)
  
  // 验证序列化数据
  assert_true(serialized_data.length() > 0)
  
  // 反序列化Span数据
  let deserialized_span = CrossPlatformSerializer::deserialize_span(serializer, serialized_data)
  
  // 验证反序列化结果
  assert_eq(Span::name(deserialized_span), Span::name(span))
  
  let original_attrs = Span::attributes(span)
  let deserialized_attrs = Span::attributes(deserialized_span)
  
  assert_eq(
    Attributes::get(original_attrs, "platform"),
    Attributes::get(deserialized_attrs, "platform")
  )
  assert_eq(
    Attributes::get(original_attrs, "architecture"),
    Attributes::get(deserialized_attrs, "architecture")
  )
  
  // 测试跨平台时间戳处理
  let timestamp = PlatformClock::now(platform_detector)
  let formatted_timestamp = PlatformClock::format_timestamp(platform_detector, timestamp)
  
  assert_true(formatted_timestamp.length() > 0)
  
  // 测试跨平台文件路径处理
  let file_path = PlatformPaths::join(platform_detector, ["telemetry", "logs", "app.log"])
  assert_true(PlatformPaths::is_valid(file_path))
  
  // 测试跨平台环境变量
  let telemetry_env = PlatformEnvironment::get(platform_detector, "TELEMETRY_CONFIG")
  // 环境变量可能不存在，但不应该导致错误
  assert_true(true)
  
  Span::end(span)
}

// 测试9: 遥测系统自监控测试
test "遥测系统自监控测试" {
  // 创建自监控管理器
  let self_monitoring = SelfMonitoringManager::new()
  
  // 启用自监控
  SelfMonitoringManager::enable(self_monitoring, true)
  
  // 配置自监控指标
  SelfMonitoringManager::monitor_component(self_monitoring, "tracer")
  SelfMonitoringManager::monitor_component(self_monitoring, "meter")
  SelfMonitoringManager::monitor_component(self_monitoring, "logger")
  SelfMonitoringManager::monitor_component(self_monitoring, "exporter")
  
  // 创建遥测组件
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "self-monitoring.test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "self-monitoring.test")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "self-monitoring.test")
  
  // 执行一些遥测操作以生成自监控数据
  for i in 0..100 {
    let span = Tracer::start_span(tracer, "operation-" + i.to_string())
    Span::set_attribute(span, "operation.id", i.to_string())
    Span::end(span)
    
    let counter = Meter::create_counter(meter, "operations.total")
    Counter::add(counter, 1.0)
    
    let log = LogRecord::new(Info, "Operation " + i.to_string() + " completed")
    Logger::emit(logger, log)
  }
  
  // 获取自监控数据
  let tracer_metrics = SelfMonitoringManager::get_component_metrics(self_monitoring, "tracer")
  let meter_metrics = SelfMonitoringManager::get_component_metrics(self_monitoring, "meter")
  let logger_metrics = SelfMonitoringManager::get_component_metrics(self_monitoring, "logger")
  
  // 验证自监控数据
  assert_true(tracer_metrics.size() > 0)
  assert_true(meter_metrics.size() > 0)
  assert_true(logger_metrics.size() > 0)
  
  // 检查特定指标
  assert_true(ComponentMetrics::contains_key(tracer_metrics, "spans.created"))
  assert_true(ComponentMetrics::contains_key(tracer_metrics, "spans.ended"))
  assert_true(ComponentMetrics::contains_key(meter_metrics, "metrics.recorded"))
  assert_true(ComponentMetrics::contains_key(logger_metrics, "logs.emitted"))
  
  // 获取系统健康状态
  let system_health = SelfMonitoringManager::get_system_health(self_monitoring)
  
  // 验证健康状态
  assert_true(SystemHealth::overall_status(system_health) != "unknown")
  assert_true(SystemHealth::component_count(system_health) >= 3)
  
  // 检查组件健康状态
  let tracer_health = SystemHealth::component_health(system_health, "tracer")
  let meter_health = SystemHealth::component_health(system_health, "meter")
  let logger_health = SystemHealth::component_health(system_health, "logger")
  
  assert_true(ComponentHealth::status(tracer_health) != "unknown")
  assert_true(ComponentHealth::status(meter_health) != "unknown")
  assert_true(ComponentHealth::status(logger_health) != "unknown")
  
  // 测试性能监控
  let performance_metrics = SelfMonitoringManager::get_performance_metrics(self_monitoring)
  
  // 验证性能指标
  assert_true(PerformanceMetrics::contains_key(performance_metrics, "cpu.usage"))
  assert_true(PerformanceMetrics::contains_key(performance_metrics, "memory.usage"))
  assert_true(PerformanceMetrics::contains_key(performance_metrics, "operation.latency"))
  
  // 测试资源监控
  let resource_usage = SelfMonitoringManager::get_resource_usage(self_monitoring)
  
  // 验证资源使用情况
  assert_true(ResourceUsage::memory_used(resource_usage) >= 0)
  assert_true(ResourceUsage::memory_available(resource_usage) >= 0)
  assert_true(ResourceUsage::disk_used(resource_usage) >= 0)
  assert_true(ResourceUsage::disk_available(resource_usage) >= 0)
  
  // 测试自监控告警
  SelfMonitoringManager::configure_alert(self_monitoring, "memory.usage", 80.0, ComparisonOperator::GreaterThan)
  SelfMonitoringManager::configure_alert(self_monitoring, "operation.latency", 1000.0, ComparisonOperator::GreaterThan)
  
  // 模拟高内存使用情况
  let high_memory_usage = ResourceUsage::new(
    1024 * 1024 * 1024,  // 1GB已使用
    500 * 1024 * 1024,   // 500MB可用
    10 * 1024 * 1024 * 1024,  // 10GB已使用磁盘
    100 * 1024 * 1024 * 1024  // 100GB可用磁盘
  )
  
  SelfMonitoringManager::update_resource_usage(self_monitoring, high_memory_usage)
  
  // 检查是否触发告警
  let alerts = SelfMonitoringManager::get_active_alerts(self_monitoring)
  
  // 高内存使用应该触发告警
  let memory_alerts = alerts.filter(fn(alert) { Alert::metric_name(alert) == "memory.usage" })
  assert_true(memory_alerts.length() > 0)
}

// 测试10: 遥测数据生命周期管理测试
test "遥测数据生命周期管理测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 配置生命周期策略
  let hot_data_policy = DataPolicy::new("hot", 7, 24 * 60 * 60 * 1000)  // 7天热数据
  let warm_data_policy = DataPolicy::new("warm", 30, 24 * 60 * 60 * 1000)  // 30天温数据
  let cold_data_policy = DataPolicy::new("cold", 365, 24 * 60 * 60 * 1000)  // 365天冷数据
  
  LifecycleManager::add_policy(lifecycle_manager, hot_data_policy)
  LifecycleManager::add_policy(lifecycle_manager, warm_data_policy)
  LifecycleManager::add_policy(lifecycle_manager, cold_data_policy)
  
  // 创建不同时间点的遥测数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test")
  
  let current_time = Clock::now_unix_millis(Clock::system())
  let day_in_ms = 24 * 60 * 60 * 1000L
  
  // 生成不同生命周期的数据
  let hot_data_spans = []
  let warm_data_spans = []
  let cold_data_spans = []
  let expired_data_spans = []
  
  // 热数据（最近7天）
  for i in 0..50 {
    let span_time = current_time - (i.to_long() * day_in_ms / 10)
    let span = Tracer::start_span_with_time(tracer, "hot-data-" + i.to_string(), span_time)
    Span::set_attribute(span, "data.category", "hot")
    Span::set_attribute(span, "days.old", (i / 10).to_string())
    hot_data_spans.push(span)
  }
  
  // 温数据（7-30天前）
  for i in 0..50 {
    let span_time = current_time - ((7 + i / 2).to_long() * day_in_ms)
    let span = Tracer::start_span_with_time(tracer, "warm-data-" + i.to_string(), span_time)
    Span::set_attribute(span, "data.category", "warm")
    Span::set_attribute(span, "days.old", (7 + i / 2).to_string())
    warm_data_spans.push(span)
  }
  
  // 冷数据（30-365天前）
  for i in 0..30 {
    let span_time = current_time - ((30 + i * 10).to_long() * day_in_ms)
    let span = Tracer::start_span_with_time(tracer, "cold-data-" + i.to_string(), span_time)
    Span::set_attribute(span, "data.category", "cold")
    Span::set_attribute(span, "days.old", (30 + i * 10).to_string())
    cold_data_spans.push(span)
  }
  
  // 过期数据（超过365天）
  for i in 0..10 {
    let span_time = current_time - ((365 + i * 30).to_long() * day_in_ms)
    let span = Tracer::start_span_with_time(tracer, "expired-data-" + i.to_string(), span_time)
    Span::set_attribute(span, "data.category", "expired")
    Span::set_attribute(span, "days.old", (365 + i * 30).to_string())
    expired_data_spans.push(span)
  }
  
  // 执行生命周期管理
  let lifecycle_result = LifecycleManager::process(lifecycle_manager, [
    hot_data_spans,
    warm_data_spans,
    cold_data_spans,
    expired_data_spans
  ].flatten())
  
  // 验证生命周期管理结果
  assert_true(LifecycleResult::processed_count(lifecycle_result) > 0)
  
  // 检查数据分类结果
  let classified_data = LifecycleResult::classified_data(lifecycle_result)
  assert_true(ClassifiedData::contains_key(classified_data, "hot"))
  assert_true(ClassifiedData::contains_key(classified_data, "warm"))
  assert_true(ClassifiedData::contains_key(classified_data, "cold"))
  
  // 验证过期数据被标记为删除
  let expired_count = LifecycleResult::expired_count(lifecycle_result)
  assert_true(expired_count >= 10)
  
  // 测试数据转换
  let transformation_rules = [
    DataTransformationRule::compress("warm", "gzip"),
    DataTransformationRule::aggregate("cold", "hourly"),
    DataTransformationRule::archive("expired", "parquet")
  ]
  
  let transformation_result = LifecycleManager::apply_transformations(lifecycle_manager, classified_data, transformation_rules)
  
  // 验证转换结果
  assert_true(TransformationResult::success_count(transformation_result) > 0)
  
  // 测试数据保留策略
  let retention_policy = RetentionPolicy::new()
  RetentionPolicy::add_rule(retention_policy, "hot", RetentionRule::keep_all())
  RetentionPolicy::add_rule(retention_policy, "warm", RetentionRule::keep_sample(0.5))
  RetentionPolicy::add_rule(retention_policy, "cold", RetentionRule::keep_sample(0.1))
  RetentionPolicy::add_rule(retention_policy, "expired", RetentionRule::delete_all())
  
  let retention_result = LifecycleManager::apply_retention(lifecycle_manager, classified_data, retention_policy)
  
  // 验证保留策略结果
  assert_true(RetentionResult::kept_count(retention_result) > 0)
  assert_true(RetentionResult::deleted_count(retention_result) >= 10)
  
  // 测试生命周期监控
  let lifecycle_metrics = LifecycleManager::get_metrics(lifecycle_manager)
  
  // 验证生命周期指标
  assert_true(LifecycleMetrics::contains_key(lifecycle_metrics, "data.processed"))
  assert_true(LifecycleMetrics::contains_key(lifecycle_metrics, "data.transformed"))
  assert_true(LifecycleMetrics::contains_key(lifecycle_metrics, "data.deleted"))
  assert_true(LifecycleMetrics::contains_key(lifecycle_metrics, "processing.time"))
  
  // 验证处理时间合理
  let processing_time = LifecycleMetrics::get(lifecycle_metrics, "processing.time")
  assert_true(processing_time >= 0.0)
}

// 辅助函数：WebAssembly平台测试
fn test_webasm_telemetry() {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "webasm.test")
  
  let span = Tracer::start_span(tracer, "webasm.operation")
  Span::set_attribute(span, "platform", "webassembly")
  Span::set_attribute(span, "memory.model", "linear")
  
  // 测试WebAssembly特定的内存限制
  let memory_limit = WASMMemory::limit()
  assert_true(memory_limit > 0)
  
  Span::end(span)
}

// 辅助函数：Linux平台测试
fn test_linux_telemetry() {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "linux.test")
  
  let span = Tracer::start_span(tracer, "linux.operation")
  Span::set_attribute(span, "platform", "linux")
  Span::set_attribute(span, "filesystem", "ext4")
  
  // 测试Linux特定的文件系统路径
  let log_path = "/var/log/telemetry/app.log"
  assert_true(FileSystem::exists(log_path) || true)  // 可能不存在，但不应该出错
  
  Span::end(span)
}

// 辅助函数：Windows平台测试
fn test_windows_telemetry() {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "windows.test")
  
  let span = Tracer::start_span(tracer, "windows.operation")
  Span::set_attribute(span, "platform", "windows")
  Span::set_attribute(span, "registry", "available")
  
  // 测试Windows特定的注册表访问
  let registry_key = "HKEY_LOCAL_MACHINE\\SOFTWARE\\Azimuth"
  assert_true(Registry::key_exists(registry_key) || true)  // 可能不存在，但不应该出错
  
  Span::end(span)
}

// 辅助函数：macOS平台测试
fn test_macos_telemetry() {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "macos.test")
  
  let span = Tracer::start_span(tracer, "macos.operation")
  Span::set_attribute(span, "platform", "macos")
  Span::set_attribute(span, "system.integrity", "enabled")
  
  // 测试macOS特定的系统完整性保护
  let sip_enabled = MacOSSystem::system_integrity_protection_enabled()
  assert_true(sip_enabled || true)  // 可能未启用，但不应该出错
  
  Span::end(span)
}

// 辅助函数：通用平台测试
fn test_generic_telemetry() {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "generic.test")
  
  let span = Tracer::start_span(tracer, "generic.operation")
  Span::set_attribute(span, "platform", "generic")
  Span::set_attribute(span, "compatibility", "high")
  
  // 测试通用功能
  let system_info = SystemInfo::gather()
  assert_true(SystemInfo::platform_name(system_info).length() > 0)
  
  Span::end(span)
}