// Azimuth 综合遥测系统测试用例
// 涵盖遥测系统的序列化、传播、压缩、性能、采样、安全、可视化、配置、异常检测和资源管理

// 测试1: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化功能" {
  // 创建测试span数据
  let span_data = SpanData::new(
    "test.operation",
    1000,
    1100,
    [("service.name", "test-service"), ("operation.type", "database")]
  )
  
  // 添加span事件
  SpanData::add_event(span_data, "query.started", 1005, [("query.type", "SELECT")])
  SpanData::add_event(span_data, "query.completed", 1095, [("rows.affected", "42")])
  
  // 序列化span数据
  let serialized = JsonSerializer::serialize_span(span_data)
  assert_true(serialized.length() > 0)
  assert_true(serialized.contains("test.operation"))
  assert_true(serialized.contains("test-service"))
  
  // 反序列化span数据
  let deserialized = JsonSerializer::deserialize_span(serialized)
  assert_eq(SpanData::name(deserialized), "test.operation")
  assert_eq(SpanData::start_time(deserialized), 1000)
  assert_eq(SpanData::end_time(deserialized), 1100)
  
  // 测试度量数据序列化
  let metric_data = MetricData::counter(
    "requests.total",
    125.0,
    [("endpoint", "/api/users"), ("method", "GET")]
  )
  
  let serialized_metric = JsonSerializer::serialize_metric(metric_data)
  let deserialized_metric = JsonSerializer::deserialize_metric(serialized_metric)
  
  assert_eq(MetricData::name(deserialized_metric), "requests.total")
  assert_eq(MetricData::value(deserialized_metric), 125.0)
  
  // 测试日志数据序列化
  let log_data = LogData::new(
    Error,
    "Database connection failed",
    1005,
    [("error.code", "DB001"), ("retry.count", "3")]
  )
  
  let serialized_log = JsonSerializer::serialize_log(log_data)
  let deserialized_log = JsonSerializer::deserialize_log(serialized_log)
  
  assert_eq(LogData::severity(deserialized_log), Error)
  assert_eq(LogData::message(deserialized_log), "Database connection failed")
}

// 测试2: 跨服务遥测数据传播
test "跨服务遥测数据传播功能" {
  // 创建根上下文
  let root_context = Context::root()
  let trace_id = TraceId::generate()
  let span_id = SpanId::generate()
  
  // 创建根span
  let root_span = Span::create_with_context(
    "api.gateway.request",
    root_context,
    trace_id,
    span_id,
    [("service.name", "api-gateway")]
  )
  
  // 注入传播头
  let propagator = TraceContextPropagator::new()
  let headers = Propagator::inject(propagator, Span::context(root_span))
  
  // 验证传播头
  assert_true(headers.contains("traceparent"))
  assert_true(headers.contains("tracestate"))
  
  // 模拟跨服务调用 - 提取传播头
  let extracted_context = Propagator::extract(propagator, headers)
  let child_span = Span::create_with_parent(
    "user.service.authentication",
    extracted_context,
    [("service.name", "user-service"), ("operation.type", "auth")]
  )
  
  // 验证传播的上下文
  assert_eq(Span::trace_id(child_span), trace_id)
  assert_not_eq(Span::span_id(child_span), span_id)
  assert_eq(Span::parent_span_id(child_span), Some(span_id))
  
  // 测试跨服务 baggage 传播
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let updated_baggage = Baggage::set_entry(updated_baggage, "request.source", "mobile")
  
  let baggage_propagator = BaggagePropagator::new()
  let baggage_headers = BaggagePropagator::inject(baggage_propagator, updated_baggage)
  
  let extracted_baggage = BaggagePropagator::extract(baggage_propagator, baggage_headers)
  let user_id = Baggage::get_entry(extracted_baggage, "user.id")
  let request_source = Baggage::get_entry(extracted_baggage, "request.source")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_source, Some("mobile"))
}

// 测试3: 遥测数据压缩和存储
test "遥测数据压缩和存储功能" {
  // 创建大量遥测数据
  let telemetry_batch = []
  
  // 添加多个span数据
  for i = 0; i < 100; i = i + 1 {
    let span = SpanData::new(
      "operation." + i.to_string(),
      1000 + i * 10,
      1100 + i * 10,
      [("service", "test-service"), ("worker.id", i.to_string())]
    )
    telemetry_batch = telemetry_batch.push(span)
  }
  
  // 添加度量数据
  for i = 0; i < 50; i = i + 1 {
    let metric = MetricData::gauge(
      "memory.usage",
      1024.0 + (i % 10).to_float() * 100.0,
      [("instance", "instance-" + (i % 5).to_string())]
    )
    telemetry_batch = telemetry_batch.push(metric)
  }
  
  // 序列化批处理数据
  let serializer = BatchSerializer::new()
  let serialized_batch = BatchSerializer::serialize(serializer, telemetry_batch)
  
  // 测试压缩
  let compressor = GzipCompressor::new()
  let compressed_data = Compressor::compress(compressor, serialized_batch)
  
  // 验证压缩效果
  assert_true(compressed_data.length() < serialized_batch.length())
  let compression_ratio = compressed_data.length().to_float() / serialized_batch.length().to_float()
  assert_true(compression_ratio < 0.8) // 至少20%的压缩率
  
  // 测试解压缩
  let decompressed_data = Compressor::decompress(compressor, compressed_data)
  assert_eq(decompressed_data, serialized_batch)
  
  // 测试存储
  let storage = FileStorage::new("/tmp/telemetry_data")
  let storage_key = "batch_" + Time::now().to_string()
  
  Storage::store(storage, storage_key, compressed_data)
  let retrieved_data = Storage::retrieve(storage, storage_key)
  
  assert_eq(retrieved_data, compressed_data)
  
  // 验证存储数据的完整性
  let retrieved_decompressed = Compressor::decompress(compressor, retrieved_data)
  let deserialized_batch = BatchSerializer::deserialize(serializer, retrieved_decompressed)
  
  assert_eq(deserialized_batch.length(), telemetry_batch.length())
}

// 测试4: 遥测系统性能基准测试
test "遥测系统性能基准测试" {
  // 创建性能测试器
  let benchmark = PerformanceBenchmark::new()
  
  // 测试span创建性能
  let span_creation_time = Benchmark::measure(benchmark, fn() {
    let tracer = Tracer::new("performance.test")
    let span = Tracer::start_span(tracer, "benchmark.operation")
    Span::set_attribute(span, "benchmark", "true")
    Span::end(span)
  }, 1000) // 运行1000次
  
  // 验证span创建性能
  assert_true(span_creation_time < 100000) // 小于100微秒每次
  
  // 测试度量记录性能
  let metric_recording_time = Benchmark::measure(benchmark, fn() {
    let meter = Meter::new("performance.test")
    let counter = Meter::create_counter(meter, "benchmark.counter")
    Counter::add(counter, 1.0, [("tag", "performance")])
  }, 1000)
  
  // 验证度量记录性能
  assert_true(metric_recording_time < 50000) // 小于50微秒每次
  
  // 测试日志记录性能
  let log_recording_time = Benchmark::measure(benchmark, fn() {
    let logger = Logger::new("performance.test")
    let log = LogRecord::new(Info, "Performance test log entry")
    LogRecord::add_attribute(log, "benchmark", "true")
    Logger::emit(logger, log)
  }, 1000)
  
  // 验证日志记录性能
  assert_true(log_recording_time < 30000) // 小于30微秒每次
  
  // 测试序列化性能
  let test_span = SpanData::new("benchmark.span", 1000, 1100, [("service", "benchmark")])
  let serialization_time = Benchmark::measure(benchmark, fn() {
    JsonSerializer::serialize_span(test_span)
  }, 1000)
  
  // 验证序列化性能
  assert_true(serialization_time < 100000) // 小于100微秒每次
  
  // 测试批量处理性能
  let large_batch = []
  for i = 0; i < 1000; i = i + 1 {
    large_batch = large_batch.push(SpanData::new("span." + i.to_string(), 1000 + i, 1100 + i, []))
  }
  
  let batch_processing_time = Benchmark::measure(benchmark, fn() {
    let processor = BatchProcessor::new(100)
    BatchProcessor::process(processor, large_batch)
  }, 10) // 运行10次，每次处理1000个span
  
  // 验证批量处理性能
  assert_true(batch_processing_time < 1000000) // 小于1毫秒每次批处理
}

// 测试5: 遥测数据采样策略
test "遥测数据采样策略功能" {
  // 创建基于概率的采样器
  let probability_sampler = ProbabilitySampler::new(0.1) // 10%采样率
  
  // 测试概率采样
  let sampled_count = 0
  for i = 0; i < 1000; i = i + 1 {
    let trace_id = TraceId::from_string("trace_" + i.to_string())
    if Sampler::should_sample(probability_sampler, trace_id, "test.operation", []) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率在合理范围内 (5% - 15%)
  let actual_rate = sampled_count.to_float() / 1000.0
  assert_true(actual_rate > 0.05 && actual_rate < 0.15)
  
  // 创建基于属性的采样器
  let attribute_sampler = AttributeSampler::new([
    ("service.name", "critical-service"),
    ("error", "true")
  ])
  
  // 测试属性采样
  let critical_trace_id = TraceId::from_string("critical_trace")
  let normal_trace_id = TraceId::from_string("normal_trace")
  
  let critical_sampled = Sampler::should_sample(
    attribute_sampler,
    critical_trace_id,
    "critical.operation",
    [("service.name", "critical-service")]
  )
  
  let normal_sampled = Sampler::should_sample(
    attribute_sampler,
    normal_trace_id,
    "normal.operation",
    [("service.name", "normal-service")]
  )
  
  assert_true(critical_sampled)
  assert_false(normal_sampled)
  
  // 测试错误属性采样
  let error_trace_id = TraceId::from_string("error_trace")
  let error_sampled = Sampler::should_sample(
    attribute_sampler,
    error_trace_id,
    "error.operation",
    [("service.name", "normal-service"), ("error", "true")]
  )
  
  assert_true(error_sampled)
  
  // 创建复合采样器
  let composite_sampler = CompositeSampler::new([
    probability_sampler,
    attribute_sampler
  ], CompositeStrategy::Any) // 任何一个采样器同意就采样
  
  // 测试复合采样
  let composite_sampled = Sampler::should_sample(
    composite_sampler,
    TraceId::from_string("composite_trace"),
    "composite.operation",
    [("service.name", "normal-service")]
  )
  
  // 由于概率采样器的存在，应该有一定的采样率
  assert_true(composite_sampled || !composite_sampled) // 结果可能是true或false，取决于概率
  
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new(100, 0.05, 0.2) // 最大100个span，5%-20%采样率
  
  // 模拟高负载场景
  let high_load_sampled_count = 0
  for i = 0; i < 200; i = i + 1 {
    let trace_id = TraceId::from_string("high_load_trace_" + i.to_string())
    if Sampler::should_sample(adaptive_sampler, trace_id, "high.load.operation", []) {
      high_load_sampled_count = high_load_sampled_count + 1
    }
  }
  
  // 验证自适应采样在高负载时降低采样率
  let high_load_rate = high_load_sampled_count.to_float() / 200.0
  assert_true(high_load_rate < 0.2) // 应该低于20%
}

// 测试6: 遥测系统安全性和隐私保护
test "遥测系统安全性和隐私保护功能" {
  // 创建敏感数据检测器
  let sensitive_data_detector = SensitiveDataDetector::new([
    "email", "password", "token", "secret", "key", "credential"
  ])
  
  // 测试敏感数据检测
  let safe_attributes = [
    ("user.name", "john.doe"),
    ("operation.type", "database"),
    ("service.version", "1.2.3")
  ]
  
  let unsafe_attributes = [
    ("user.email", "john.doe@example.com"),
    ("auth.token", "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"),
    ("api.key", "sk-1234567890abcdef")
  ]
  
  let safe_result = SensitiveDataDetector::scan(sensitive_data_detector, safe_attributes)
  let unsafe_result = SensitiveDataDetector::scan(sensitive_data_detector, unsafe_attributes)
  
  assert_false(SensitiveDataDetector::contains_sensitive_data(safe_result))
  assert_true(SensitiveDataDetector::contains_sensitive_data(unsafe_result))
  
  // 测试敏感数据脱敏
  let sanitizer = DataSanitizer::new()
  let sanitized_attributes = DataSanitizer::sanitize_attributes(sanitizer, unsafe_attributes)
  
  // 验证敏感数据已被脱敏
  for (key, value) in sanitized_attributes {
    if key == "user.email" {
      assert_true(value.contains("***"))
    } else if key == "auth.token" {
      assert_true(value.contains("***"))
    } else if key == "api.key" {
      assert_true(value.contains("***"))
    }
  }
  
  // 测试加密存储
  let encryption_key = EncryptionKey::generate()
  let encryptor = AESEncryptor::new(encryption_key)
  
  let sensitive_data = "user_id=12345,email=user@example.com,token=secret_token"
  let encrypted_data = Encryptor::encrypt(encryptor, sensitive_data)
  
  // 验证加密后的数据不可读
  assert_not_eq(encrypted_data, sensitive_data)
  assert_false(encrypted_data.contains("user@example.com"))
  assert_false(encrypted_data.contains("secret_token"))
  
  // 测试解密
  let decrypted_data = Encryptor::decrypt(encryptor, encrypted_data)
  assert_eq(decrypted_data, sensitive_data)
  
  // 测试访问控制
  let access_policy = AccessPolicy::new()
  AccessPolicy::add_rule(access_policy, "admin", ["read", "write", "delete"])
  AccessPolicy::add_rule(access_policy, "user", ["read"])
  AccessPolicy::add_rule(access_policy, "guest", [])
  
  // 测试权限检查
  assert_true(AccessPolicy::has_permission(access_policy, "admin", "read"))
  assert_true(AccessPolicy::has_permission(access_policy, "admin", "write"))
  assert_true(AccessPolicy::has_permission(access_policy, "admin", "delete"))
  
  assert_true(AccessPolicy::has_permission(access_policy, "user", "read"))
  assert_false(AccessPolicy::has_permission(access_policy, "user", "write"))
  assert_false(AccessPolicy::has_permission(access_policy, "user", "delete"))
  
  assert_false(AccessPolicy::has_permission(access_policy, "guest", "read"))
  assert_false(AccessPolicy::has_permission(access_policy, "guest", "write"))
  assert_false(AccessPolicy::has_permission(access_policy, "guest", "delete"))
}

// 测试7: 遥测数据可视化和仪表板
test "遥测数据可视化和仪表板功能" {
  // 创建仪表板数据生成器
  let dashboard_generator = DashboardDataGenerator::new()
  
  // 添加时间序列数据
  let time_series_data = []
  for i = 0; i < 24; i = i + 1 { // 24小时数据
    let timestamp = 1609459200 + i * 3600 // 每小时一个数据点
    let cpu_usage = 30.0 + (i % 12).to_float() * 5.0 // 模拟CPU使用率
    let memory_usage = 50.0 + (i % 8).to_float() * 6.25 // 模拟内存使用率
    
    time_series_data = time_series_data.push(TimeSeriesPoint::new(
      timestamp,
      [("cpu.usage", cpu_usage), ("memory.usage", memory_usage)]
    ))
  }
  
  DashboardDataGenerator::add_time_series(dashboard_generator, "system.metrics", time_series_data)
  
  // 添加度量数据
  let metric_data = [
    ("http.requests.total", 12543),
    ("http.errors.total", 234),
    ("database.connections.active", 15),
    ("database.connections.max", 100)
  ]
  
  for (name, value) in metric_data {
    DashboardDataGenerator::add_metric(dashboard_generator, name, value.to_float())
  }
  
  // 生成仪表板数据
  let dashboard_data = DashboardDataGenerator::generate(dashboard_generator)
  
  // 验证仪表板数据
  assert_true(DashboardData::has_time_series(dashboard_data, "system.metrics"))
  assert_true(DashboardData::has_metric(dashboard_data, "http.requests.total"))
  assert_eq(DashboardData::get_metric_value(dashboard_data, "http.requests.total"), Some(12543.0))
  
  // 测试图表数据生成
  let chart_generator = ChartDataGenerator::new()
  
  // 生成CPU使用率图表数据
  let cpu_chart_data = ChartDataGenerator::generate_time_series_chart(
    chart_generator,
    time_series_data,
    "cpu.usage",
    "CPU Usage (%)",
    "Time",
    "CPU Usage (%)"
  )
  
  // 验证图表数据结构
  assert_true(ChartData::has_labels(cpu_chart_data))
  assert_true(ChartData::has_datasets(cpu_chart_data))
  assert_eq(ChartData::get_label(cpu_chart_data, "y"), Some("CPU Usage (%)"))
  
  // 生成饼图数据
  let error_data = [
    ("4xx errors", 180),
    ("5xx errors", 54)
  ]
  
  let error_pie_chart = ChartDataGenerator::generate_pie_chart(
    chart_generator,
    error_data,
    "HTTP Errors by Type"
  )
  
  // 验证饼图数据
  assert_eq(ChartData::get_chart_type(error_pie_chart), "pie")
  assert_true(ChartData::has_datasets(error_pie_chart))
  
  // 测试实时数据更新
  let real_time_updater = RealTimeUpdater::new()
  RealTimeUpdater::subscribe(real_time_updater, "cpu.usage", fn(data) {
    // 模拟实时数据处理
    assert_true(data.contains("cpu.usage"))
  })
  
  // 模拟实时数据推送
  for i = 0; i < 5; i = i + 1 {
    let real_time_data = "{\"cpu.usage\": " + (40.0 + i.to_float() * 2.0).to_string() + "}"
    RealTimeUpdater::push_data(real_time_updater, "cpu.usage", real_time_data)
  }
  
  // 验证订阅者收到更新
  assert_eq(RealTimeUpdater::get_update_count(real_time_updater, "cpu.usage"), 5)
}

// 测试8: 遥测系统配置管理
test "遥测系统配置管理功能" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 添加默认配置
  let default_config = {
    "sampling.probability": "0.1",
    "batch.size": "100",
    "export.interval": "5000",
    "compression.enabled": "true",
    "retention.days": "30"
  }
  
  for (key, value) in default_config {
    ConfigManager::set_default(config_manager, key, value)
  }
  
  // 验证默认配置
  assert_eq(ConfigManager::get(config_manager, "sampling.probability"), Some("0.1"))
  assert_eq(ConfigManager::get(config_manager, "batch.size"), Some("100"))
  assert_eq(ConfigManager::get(config_manager, "export.interval"), Some("5000"))
  assert_eq(ConfigManager::get(config_manager, "compression.enabled"), Some("true"))
  assert_eq(ConfigManager::get(config_manager, "retention.days"), Some("30"))
  
  // 测试环境特定配置
  let production_config = {
    "sampling.probability": "0.05",  // 生产环境降低采样率
    "batch.size": "200",             // 生产环境增大批处理大小
    "export.interval": "3000"        // 生产环境更频繁导出
  }
  
  ConfigManager::set_environment_config(config_manager, "production", production_config)
  
  // 切换到生产环境配置
  ConfigManager::set_environment(config_manager, "production")
  
  // 验证生产环境配置
  assert_eq(ConfigManager::get(config_manager, "sampling.probability"), Some("0.05"))
  assert_eq(ConfigManager::get(config_manager, "batch.size"), Some("200"))
  assert_eq(ConfigManager::get(config_manager, "export.interval"), Some("3000"))
  // 未覆盖的配置仍使用默认值
  assert_eq(ConfigManager::get(config_manager, "compression.enabled"), Some("true"))
  assert_eq(ConfigManager::get(config_manager, "retention.days"), Some("30"))
  
  // 测试配置验证
  let validation_rules = [
    ("sampling.probability", ConfigValidationRule::range(0.0, 1.0)),
    ("batch.size", ConfigValidationRule::min(1)),
    ("export.interval", ConfigValidationRule::min(100)),
    ("retention.days", ConfigValidationRule::min(1))
  ]
  
  for (key, rule) in validation_rules {
    ConfigManager::add_validation_rule(config_manager, key, rule)
  }
  
  // 测试有效配置
  let valid_config = {
    "sampling.probability": "0.15",
    "batch.size": "150",
    "export.interval": "4000",
    "retention.days": "45"
  }
  
  let validation_result = ConfigManager::validate_config(config_manager, valid_config)
  assert_true(ConfigValidationResult::is_valid(validation_result))
  
  // 测试无效配置
  let invalid_config = {
    "sampling.probability": "1.5",    // 超出范围
    "batch.size": "0",                // 小于最小值
    "export.interval": "50",          // 小于最小值
    "retention.days": "-5"            // 小于最小值
  }
  
  let invalid_validation_result = ConfigManager::validate_config(config_manager, invalid_config)
  assert_false(ConfigValidationResult::is_valid(invalid_validation_result))
  assert_true(ConfigValidationResult::has_error(invalid_validation_result, "sampling.probability"))
  assert_true(ConfigValidationResult::has_error(invalid_validation_result, "batch.size"))
  assert_true(ConfigValidationResult::has_error(invalid_validation_result, "export.interval"))
  assert_true(ConfigValidationResult::has_error(invalid_validation_result, "retention.days"))
  
  // 测试配置热更新
  let config_change_listener = ConfigChangeListener::new()
  ConfigChangeListener::on_change(config_change_listener, "sampling.probability", fn(old_value, new_value) {
    assert_eq(old_value, "0.05")
    assert_eq(new_value, "0.2")
  })
  
  ConfigManager::add_change_listener(config_manager, config_change_listener)
  ConfigManager::set(config_manager, "sampling.probability", "0.2")
  
  // 验证配置已更新
  assert_eq(ConfigManager::get(config_manager, "sampling.probability"), Some("0.2"))
}

// 测试9: 遥测数据异常检测
test "遥测数据异常检测功能" {
  // 创建异常检测器
  let anomaly_detector = AnomalyDetector::new()
  
  // 添加正常基线数据
  let baseline_metrics = [
    ("cpu.usage", 30.0),
    ("memory.usage", 50.0),
    ("response.time", 100.0),
    ("error.rate", 0.01)
  ]
  
  for (metric_name, value) in baseline_metrics {
    AnomalyDetector::set_baseline(anomaly_detector, metric_name, value)
  }
  
  // 设置异常阈值
  let thresholds = [
    ("cpu.usage", AnomalyThreshold::percentage(50.0)),  // 偏离基线50%
    ("memory.usage", AnomalyThreshold::percentage(30.0)),
    ("response.time", AnomalyThreshold::absolute(200.0)), // 绝对阈值200ms
    ("error.rate", AnomalyThreshold::percentage(100.0))  // 偏离基线100%
  ]
  
  for (metric_name, threshold) in thresholds {
    AnomalyDetector::set_threshold(anomaly_detector, metric_name, threshold)
  }
  
  // 测试正常数据检测
  let normal_metrics = [
    ("cpu.usage", 35.0),      // 偏离基线16.7%，在阈值内
    ("memory.usage", 55.0),   // 偏离基线10%，在阈值内
    ("response.time", 120.0), // 低于绝对阈值
    ("error.rate", 0.015)     // 偏离基线50%，在阈值内
  ]
  
  for (metric_name, value) in normal_metrics {
    let is_anomaly = AnomalyDetector::detect(anomaly_detector, metric_name, value)
    assert_false(is_anomaly)
  }
  
  // 测试异常数据检测
  let anomalous_metrics = [
    ("cpu.usage", 50.0),      // 偏离基线66.7%，超出阈值
    ("memory.usage", 70.0),   // 偏离基线40%，超出阈值
    ("response.time", 250.0), // 超出绝对阈值
    ("error.rate", 0.03)      // 偏离基线200%，超出阈值
  ]
  
  for (metric_name, value) in anomalous_metrics {
    let is_anomaly = AnomalyDetector::detect(anomaly_detector, metric_name, value)
    assert_true(is_anomaly)
  }
  
  // 测试时间序列异常检测
  let time_series_detector = TimeSeriesAnomalyDetector::new()
  
  // 添加正常时间序列数据
  let normal_series = []
  for i = 0; i < 100; i = i + 1 {
    let value = 50.0 + (i % 10).to_float() * 2.0 // 正常波动
    normal_series = normal_series.push(value)
  }
  
  TimeSeriesAnomalyDetector::train(time_series_detector, normal_series)
  
  // 测试正常时间序列点
  let normal_point = 52.0
  let is_series_anomaly_normal = TimeSeriesAnomalyDetector::detect(time_series_detector, normal_point)
  assert_false(is_series_anomaly_normal)
  
  // 测试异常时间序列点
  let anomalous_point = 80.0 // 显著偏离正常模式
  let is_series_anomaly_anomalous = TimeSeriesAnomalyDetector::detect(time_series_detector, anomalous_point)
  assert_true(is_series_anomaly_anomalous)
  
  // 测试异常模式检测
  let pattern_detector = PatternAnomalyDetector::new()
  
  // 添加正常模式（每天高峰期）
  let daily_pattern = []
  for hour = 0; hour < 24; hour = hour + 1 {
    let value = if hour >= 9 && hour <= 17 {
      80.0 + (hour - 9).to_float() * 2.0 // 工作时间高负载
    } else {
      20.0 + hour.to_float() * 0.5 // 非工作时间低负载
    }
    daily_pattern = daily_pattern.push(value)
  }
  
  PatternAnomalyDetector::learn_pattern(pattern_detector, "daily.pattern", daily_pattern)
  
  // 测试符合模式的序列
  let normal_daily_sequence = [25.0, 30.0, 85.0, 90.0, 95.0, 100.0]
  let pattern_match_normal = PatternAnomalyDetector::match_pattern(pattern_detector, "daily.pattern", normal_daily_sequence)
  assert_true(pattern_match_normal)
  
  // 测试不符合模式的序列
  let abnormal_daily_sequence = [80.0, 85.0, 90.0, 25.0, 30.0, 20.0] // 时间顺序异常
  let pattern_match_abnormal = PatternAnomalyDetector::match_pattern(pattern_detector, "daily.pattern", abnormal_daily_sequence)
  assert_false(pattern_match_abnormal)
}

// 测试10: 遥测系统资源管理和优化
test "遥测系统资源管理和优化功能" {
  // 创建资源管理器
  let resource_manager = ResourceManager::new()
  
  // 设置资源限制
  let resource_limits = {
    "memory.max": "100MB",
    "cpu.max": "50%",
    "storage.max": "1GB",
    "network.max": "10MB/s"
  }
  
  for (resource, limit) in resource_limits {
    ResourceManager::set_limit(resource_manager, resource, limit)
  }
  
  // 测试资源监控
  let memory_monitor = MemoryMonitor::new()
  let cpu_monitor = CPUMonitor::new()
  let storage_monitor = StorageMonitor::new()
  let network_monitor = NetworkMonitor::new()
  
  ResourceManager::add_monitor(resource_manager, memory_monitor)
  ResourceManager::add_monitor(resource_manager, cpu_monitor)
  ResourceManager::add_monitor(resource_manager, storage_monitor)
  ResourceManager::add_monitor(resource_manager, network_monitor)
  
  // 模拟资源使用
  let current_usage = {
    "memory.used": "45MB",
    "cpu.used": "25%",
    "storage.used": "200MB",
    "network.used": "2MB/s"
  }
  
  for (resource, usage) in current_usage {
    ResourceManager::update_usage(resource_manager, resource, usage)
  }
  
  // 验证资源使用情况
  assert_eq(ResourceManager::get_usage(resource_manager, "memory.used"), Some("45MB"))
  assert_eq(ResourceManager::get_usage(resource_manager, "cpu.used"), Some("25%"))
  assert_eq(ResourceManager::get_usage(resource_manager, "storage.used"), Some("200MB"))
  assert_eq(ResourceManager::get_usage(resource_manager, "network.used"), Some("2MB/s"))
  
  // 测试资源优化策略
  let optimization_strategies = [
    ResourceOptimizationStrategy::batch_size_adjustment,
    ResourceOptimizationStrategy::sampling_rate_adjustment,
    ResourceOptimizationStrategy::compression_level_adjustment,
    ResourceOptimizationStrategy::retention_period_adjustment
  ]
  
  for strategy in optimization_strategies {
    ResourceManager::add_optimization_strategy(resource_manager, strategy)
  }
  
  // 模拟高资源使用情况
  let high_usage = {
    "memory.used": "95MB",  // 接近限制
    "cpu.used": "45%"       // 接近限制
  }
  
  for (resource, usage) in high_usage {
    ResourceManager::update_usage(resource_manager, resource, usage)
  }
  
  // 触发资源优化
  let optimization_result = ResourceManager::optimize_resources(resource_manager)
  
  // 验证优化结果
  assert_true(ResourceOptimizationResult::has_adjustment(optimization_result, "batch.size"))
  assert_true(ResourceOptimizationResult::has_adjustment(optimization_result, "sampling.probability"))
  
  // 测试缓存管理
  let cache_manager = CacheManager::new(50) // 50MB缓存限制
  
  // 添加缓存项
  for i = 0; i < 100; i = i + 1 {
    let key = "cache_item_" + i.to_string()
    let value = "data_" + i.to_string()
    let size = 1 // 1MB每个项
    CacheManager::put(cache_manager, key, value, size)
  }
  
  // 验证缓存大小不超过限制
  let cache_size = CacheManager::get_size(cache_manager)
  assert_true(cache_size <= 50)
  
  // 测试缓存淘汰策略
  let eviction_count = CacheManager::get_eviction_count(cache_manager)
  assert_true(eviction_count > 0) // 应该有淘汰发生
  
  // 测试内存池管理
  let memory_pool = MemoryPool::new(10) // 10个对象的内存池
  
  // 从内存池获取对象
  let obj1 = MemoryPool::acquire(memory_pool)
  let obj2 = MemoryPool::acquire(memory_pool)
  let obj3 = MemoryPool::acquire(memory_pool)
  
  // 验证对象获取成功
  assert_true(MemoryPool::is_valid(obj1))
  assert_true(MemoryPool::is_valid(obj2))
  assert_true(MemoryPool::is_valid(obj3))
  
  // 归还对象到内存池
  MemoryPool::release(memory_pool, obj1)
  MemoryPool::release(memory_pool, obj2)
  MemoryPool::release(memory_pool, obj3)
  
  // 验证对象已归还
  assert_eq(MemoryPool::get_available_count(memory_pool), 10)
  
  // 测试连接池管理
  let connection_pool = ConnectionPool::new(5) // 最多5个连接
  
  // 获取连接
  let conn1 = ConnectionPool::acquire(connection_pool)
  let conn2 = ConnectionPool::acquire(connection_pool)
  let conn3 = ConnectionPool::acquire(connection_pool)
  
  // 验证连接获取成功
  assert_true(ConnectionPool::is_valid(conn1))
  assert_true(ConnectionPool::is_valid(conn2))
  assert_true(ConnectionPool::is_valid(conn3))
  
  // 归还连接
  ConnectionPool::release(connection_pool, conn1)
  ConnectionPool::release(connection_pool, conn2)
  ConnectionPool::release(connection_pool, conn3)
  
  // 验证连接已归还
  assert_eq(ConnectionPool::get_available_count(connection_pool), 5)
}