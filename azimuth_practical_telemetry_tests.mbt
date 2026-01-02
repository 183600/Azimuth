// 实用遥测测试用例
// 专注于实际应用场景的遥测功能测试

test "遥测数据采样策略测试" {
  // 测试基于概率的采样策略
  let sampling_config = SamplingConfig::new()
  let probability_sampler = SamplingConfig::with_probability(sampling_config, 0.1) // 10%采样率
  
  // 创建采样器
  let sampler = ProbabilitySampler::new(probability_sampler)
  
  // 测试采样决策
  let sampled_count = 0
  let total_requests = 1000
  
  for i in 0..total_requests {
    let decision = Sampler::should_sample(sampler, "trace-id-{i}", "test.operation")
    if decision == Sampled {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率大约为10%（允许一定的误差范围）
  let actual_rate = sampled_count * 100 / total_requests
  assert_true(actual_rate >= 8 && actual_rate <= 12)
  
  // 测试基于属性的采样策略
  let attribute_sampler = AttributeBasedSampler::new()
  Sampler::add_sampling_rule(attribute_sampler, "http.status_code", "5xx", Always)
  Sampler::add_sampling_rule(attribute_sampler, "user.premium", "true", Always)
  Sampler::add_sampling_rule(attribute_sampler, "operation.type", "critical", Always)
  
  // 测试错误状态码总是被采样
  let error_decision = Sampler::should_sample_with_attributes(
    attribute_sampler, 
    "error-trace", 
    "error.operation",
    [("http.status_code", "500")]
  )
  assert_eq(error_decision, Sampled)
  
  // 测试高级用户总是被采样
  let premium_decision = Sampler::should_sample_with_attributes(
    attribute_sampler, 
    "premium-trace", 
    "premium.operation",
    [("user.premium", "true")]
  )
  assert_eq(premium_decision, Sampled)
  
  // 测试关键操作总是被采样
  let critical_decision = Sampler::should_sample_with_attributes(
    attribute_sampler, 
    "critical-trace", 
    "critical.operation",
    [("operation.type", "critical")]
  )
  assert_eq(critical_decision, Sampled)
}

test "遥测数据批处理和导出测试" {
  // 测试遥测数据的批处理和导出功能
  let batch_processor = BatchProcessor::new()
  BatchProcessor::set_batch_size(batch_processor, 10)
  BatchProcessor::set_max_export_timeout(batch_processor, 5000)
  BatchProcessor::set_max_queue_size(batch_processor, 100)
  
  // 创建测试数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch.test")
  
  // 创建多个span
  let spans = []
  for i in 0..15 {
    let span = Tracer::start_span(tracer, "batch.operation.{i}")
    Span::set_attribute(span, "operation.id", "op-{i}")
    Span::set_attribute(span, "operation.type", if i % 2 == 0 { "even" } else { "odd" })
    spans = spans.push(span)
  }
  
  // 添加span到批处理器
  for span in spans {
    BatchProcessor::add_span(batch_processor, span)
  }
  
  // 验证批处理器状态
  let pending_count = BatchProcessor::get_pending_count(batch_processor)
  assert_true(pending_count >= 10) // 至少有一批数据
  
  // 手动触发导出
  let export_result = BatchProcessor::force_export(batch_processor)
  assert_true(ExportResult::is_success(export_result))
  
  // 验证导出后的状态
  let exported_count = ExportResult::get_exported_count(export_result)
  assert_true(exported_count >= 10)
  
  // 测试度量数据的批处理
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "batch.metrics.test")
  
  let counter = Meter::create_counter(meter, "batch.operations", Some("Batch operations"), Some("count"))
  
  // 添加度量数据
  for i in 0..20 {
    Counter::add_with_attributes(counter, 1.0, [
      ("operation.type", if i % 3 == 0 { "type-a" } else if i % 3 == 1 { "type-b" } else { "type-c" }),
      ("worker.id", "worker-{i % 3}")
    ])
  }
  
  // 批处理度量数据
  let metrics_batch = BatchProcessor::create_metrics_batch(batch_processor)
  BatchProcessor::add_metrics_to_batch(metrics_batch, counter)
  
  // 导出度量数据
  let metrics_export_result = BatchProcessor::export_metrics_batch(batch_processor, metrics_batch)
  assert_true(ExportResult::is_success(metrics_export_result))
}

test "遥测数据过滤和转换测试" {
  // 测试遥测数据的过滤和转换功能
  let filter_manager = FilterManager::new()
  
  // 创建基于属性的过滤器
  let sensitive_data_filter = AttributeFilter::new()
  FilterManager::add_attribute_filter(sensitive_data_filter, "password", Redact)
  FilterManager::add_attribute_filter(sensitive_data_filter, "token", Redact)
  FilterManager::add_attribute_filter(sensitive_data_filter, "api_key", Redact)
  
  // 创建基于值的过滤器
  let value_filter = ValueFilter::new()
  FilterManager::add_value_pattern(value_filter, "email", ".*@.*", Hash)
  FilterManager::add_value_pattern(value_filter, "phone", "\\d{3}-\\d{3}-\\d{4}", Mask)
  
  // 创建测试span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "filter.test")
  
  let span = Tracer::start_span(tracer, "filter.test.operation")
  
  // 添加敏感属性
  Span::set_attribute(span, "user.email", "test@example.com")
  Span::set_attribute(span, "user.phone", "123-456-7890")
  Span::set_attribute(span, "auth.token", "secret-token-123")
  Span::set_attribute(span, "session.id", "session-abc-123")
  Span::set_attribute(span, "operation.name", "user.login")
  
  // 应用过滤器
  let filtered_span = FilterManager::apply_filters(filter_manager, span)
  
  // 验证敏感数据被正确处理
  let filtered_email = Span::get_attribute(filtered_span, "user.email")
  let filtered_phone = Span::get_attribute(filtered_span, "user.phone")
  let filtered_token = Span::get_attribute(filtered_span, "auth.token")
  let normal_session = Span::get_attribute(filtered_span, "session.id")
  let normal_operation = Span::get_attribute(filtered_span, "operation.name")
  
  // 验证过滤结果（根据过滤器规则）
  match filtered_email {
    Some(value) => assert_true(value != "test@example.com") // 应该被哈希处理
    None => assert_true(false)
  }
  
  match filtered_phone {
    Some(value) => assert_true(value != "123-456-7890") // 应该被掩码处理
    None => assert_true(false)
  }
  
  match filtered_token {
    Some(value) => assert_true(value != "secret-token-123") // 应该被编辑处理
    None => assert_true(false)
  }
  
  // 非敏感数据应该保持不变
  match normal_session {
    Some(value) => assert_eq(value, "session-abc-123")
    None => assert_true(false)
  }
  
  match normal_operation {
    Some(value) => assert_eq(value, "user.login")
    None => assert_true(false)
  }
  
  // 测试数据转换器
  let transformer = DataTransformer::new()
  
  // 添加转换规则
  Transformer::add_rule(transformer, "timestamp", TimestampToISO)
  Transformer::add_rule(transformer, "duration_ms", DurationToHumanReadable)
  Transformer::add_rule(transformer, "bytes", BytesToHumanReadable)
  
  // 创建测试数据
  let test_data = [
    ("timestamp", "1672531200000"),
    ("duration_ms", "5432"),
    ("bytes", "1048576"),
    ("normal_field", "normal_value")
  ]
  
  // 应用转换
  let transformed_data = Transformer::apply(transformer, test_data)
  
  // 验证转换结果
  let transformed_timestamp = DataTransformer::get_value(transformed_data, "timestamp")
  let transformed_duration = DataTransformer::get_value(transformed_data, "duration_ms")
  let transformed_bytes = DataTransformer::get_value(transformed_data, "bytes")
  let normal_value = DataTransformer::get_value(transformed_data, "normal_field")
  
  match transformed_timestamp {
    Some(value) => assert_true(value.contains("T")) // ISO格式应该包含T
    None => assert_true(false)
  }
  
  match transformed_duration {
    Some(value) => assert_true(value.contains("s") || value.contains("sec")) // 人类可读格式
    None => assert_true(false)
  }
  
  match transformed_bytes {
    Some(value) => assert_true(value.contains("MB") || value.contains("MiB")) // 人类可读格式
    None => assert_true(false)
  }
  
  match normal_value {
    Some(value) => assert_eq(value, "normal_value") // 普通字段应该不变
    None => assert_true(false)
  }
}

test "遥测性能监控和自适应调整测试" {
  // 测试遥测系统自身的性能监控和自适应调整
  let performance_monitor = PerformanceMonitor::new()
  PerformanceMonitor::enable_monitoring(performance_monitor, true)
  
  // 设置性能阈值
  PerformanceMonitor::set_cpu_threshold(performance_monitor, 80.0) // 80% CPU使用率
  PerformanceMonitor::set_memory_threshold(performance_monitor, 85.0) // 85% 内存使用率
  PerformanceMonitor::set_latency_threshold(performance_monitor, 100.0) // 100ms延迟阈值
  
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new()
  AdaptiveSampler::set_base_sampling_rate(adaptive_sampler, 0.1) // 基础10%采样率
  AdaptiveSampler::set_min_sampling_rate(adaptive_sampler, 0.01) // 最小1%采样率
  AdaptiveSampler::set_max_sampling_rate(adaptive_sampler, 0.5) // 最大50%采样率
  
  // 模拟高负载情况
  PerformanceMonitor::simulate_high_load(performance_monitor, 90.0, 90.0) // 90% CPU, 90% 内存
  
  // 检查自适应调整
  let adjusted_sampling_rate = AdaptiveSampler::get_current_sampling_rate(adaptive_sampler)
  assert_true(adjusted_sampling_rate < 0.1) // 高负载时应该降低采样率
  
  // 模拟低负载情况
  PerformanceMonitor::simulate_low_load(performance_monitor, 30.0, 40.0) // 30% CPU, 40% 内存
  
  // 检查自适应调整
  let adjusted_sampling_rate_low = AdaptiveSampler::get_current_sampling_rate(adaptive_sampler)
  assert_true(adjusted_sampling_rate_low >= 0.1) // 低负载时应该恢复或提高采样率
  
  // 测试性能指标收集
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance.test")
  
  // 创建大量span来测试性能
  let start_time = Time::now()
  for i in 0..100 {
    let span = Tracer::start_span(tracer, "performance.operation.{i}")
    Span::set_attribute(span, "operation.id", "op-{i}")
    Span::set_attribute(span, "operation.type", "performance-test")
    Span::end(span)
  }
  let end_time = Time::now()
  let total_duration = Time::difference(end_time, start_time)
  
  // 获取性能指标
  let avg_span_creation_time = PerformanceMonitor::get_avg_span_creation_time(performance_monitor)
  let throughput = PerformanceMonitor::get_span_throughput(performance_monitor)
  let memory_usage = PerformanceMonitor::get_memory_usage(performance_monitor)
  
  // 验证性能指标
  assert_true(avg_span_creation_time < 10.0) // 每个span创建时间应该小于10ms
  assert_true(throughput > 50.0) // 吞吐量应该大于50 spans/sec
  assert_true(memory_usage < 100.0) // 内存使用应该小于100MB
  
  // 测试自适应批处理
  let adaptive_batch_processor = AdaptiveBatchProcessor::new()
  AdaptiveBatchProcessor::set_base_batch_size(adaptive_batch_processor, 50)
  AdaptiveBatchProcessor::set_min_batch_size(adaptive_batch_processor, 10)
  AdaptiveBatchProcessor::set_max_batch_size(adaptive_batch_processor, 200)
  
  // 在高负载情况下，批处理大小应该增加
  AdaptiveBatchProcessor::adjust_for_load(adaptive_batch_processor, 90.0) // 高负载
  let high_load_batch_size = AdaptiveBatchProcessor::get_current_batch_size(adaptive_batch_processor)
  assert_true(high_load_batch_size > 50) // 高负载时应该增加批处理大小
  
  // 在低负载情况下，批处理大小应该减少
  AdaptiveBatchProcessor::adjust_for_load(adaptive_batch_processor, 20.0) // 低负载
  let low_load_batch_size = AdaptiveBatchProcessor::get_current_batch_size(adaptive_batch_processor)
  assert_true(low_load_batch_size < 50) // 低负载时应该减少批处理大小
}

test "遥测数据安全和隐私保护测试" {
  // 测试遥测数据的安全性和隐私保护功能
  let security_manager = SecurityManager::new()
  
  // 配置加密设置
  SecurityManager::set_encryption_enabled(security_manager, true)
  SecurityManager::set_encryption_algorithm(security_manager, "AES-256-GCM")
  
  // 配置数据保留策略
  SecurityManager::set_data_retention_days(security_manager, 30) // 30天数据保留期
  SecurityManager::set_pii_detection_enabled(security_manager, true) // 启用PII检测
  
  // 测试PII（个人身份信息）检测
  let pii_detector = PIIDetector::new()
  PIIDetector::add_pattern(pii_detector, "email", "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b")
  PIIDetector::add_pattern(pii_detector, "phone", "\\b\\d{3}-\\d{3}-\\d{4}\\b")
  PIIDetector::add_pattern(pii_detector, "ssn", "\\b\\d{3}-\\d{2}-\\d{4}\\b")
  PIIDetector::add_pattern(pii_detector, "credit_card", "\\b\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}\\b")
  
  // 测试数据
  let test_data = [
    ("user.email", "john.doe@example.com"),
    ("user.phone", "123-456-7890"),
    ("user.ssn", "123-45-6789"),
    ("payment.card", "4111-1111-1111-1111"),
    ("user.name", "John Doe"),
    ("operation.name", "user.login")
  ]
  
  // 检测PII
  let detected_pii = PIIDetector::detect_pii(pii_detector, test_data)
  
  // 验证PII检测结果
  assert_true(PIIDetector::is_pii(detected_pii, "user.email"))
  assert_true(PIIDetector::is_pii(detected_pii, "user.phone"))
  assert_true(PIIDetector::is_pii(detected_pii, "user.ssn"))
  assert_true(PIIDetector::is_pii(detected_pii, "payment.card"))
  assert_false(PIIDetector::is_pii(detected_pii, "user.name")) // 姓名可能不在PII模式中
  assert_false(PIIDetector::is_pii(detected_pii, "operation.name"))
  
  // 测试数据脱敏
  let data_masker = DataMasker::new()
  DataMasker::set_masking_strategy(data_masker, "email", "partial") // 部分掩码
  DataMasker::set_masking_strategy(data_masker, "phone", "full") // 完全掩码
  DataMasker::set_masking_strategy(data_masker, "ssn", "hash") // 哈希
  DataMasker::set_masking_strategy(data_masker, "credit_card", "tokenize") // 令牌化
  
  // 应用脱敏
  let masked_data = DataMasker::apply_masking(data_masker, test_data)
  
  // 验证脱敏结果
  let masked_email = DataMasker::get_masked_value(masked_data, "user.email")
  let masked_phone = DataMasker::get_masked_value(masked_data, "user.phone")
  let masked_ssn = DataMasker::get_masked_value(masked_data, "user.ssn")
  let masked_card = DataMasker::get_masked_value(masked_data, "payment.card")
  
  match masked_email {
    Some(value) => assert_true(value.contains("***") || value != "john.doe@example.com")
    None => assert_true(false)
  }
  
  match masked_phone {
    Some(value) => assert_true(value == "***-***-****" || value != "123-456-7890")
    None => assert_true(false)
  }
  
  match masked_ssn {
    Some(value) => assert_true(value != "123-45-6789") // 应该被哈希
    None => assert_true(false)
  }
  
  match masked_card {
    Some(value) => assert_true(value != "4111-1111-1111-1111") // 应该被令牌化
    None => assert_true(false)
  }
  
  // 测试数据加密
  let encryption_service = EncryptionService::new()
  EncryptionService::set_key(encryption_service, "test-encryption-key-12345")
  
  // 创建敏感span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "security.test")
  
  let sensitive_span = Tracer::start_span(tracer, "sensitive.operation")
  Span::set_attribute(sensitive_span, "user.email", "john.doe@example.com")
  Span::set_attribute(sensitive_span, "auth.token", "secret-auth-token")
  Span::set_attribute(sensitive_span, "session.data", "sensitive-session-data")
  
  // 加密敏感属性
  let encrypted_span = EncryptionService::encrypt_sensitive_attributes(encryption_service, sensitive_span)
  
  // 验证加密结果
  let encrypted_email = Span::get_attribute(encrypted_span, "user.email")
  let encrypted_token = Span::get_attribute(encrypted_span, "auth.token")
  let encrypted_session = Span::get_attribute(encrypted_span, "session.data")
  
  match encrypted_email {
    Some(value) => assert_true(value != "john.doe@example.com") // 应该被加密
    None => assert_true(false)
  }
  
  match encrypted_token {
    Some(value) => assert_true(value != "secret-auth-token") // 应该被加密
    None => assert_true(false)
  }
  
  match encrypted_session {
    Some(value) => assert_true(value != "sensitive-session-data") // 应该被加密
    None => assert_true(false)
  }
  
  // 测试解密
  let decrypted_span = EncryptionService::decrypt_sensitive_attributes(encryption_service, encrypted_span)
  
  // 验证解密结果
  let decrypted_email = Span::get_attribute(decrypted_span, "user.email")
  let decrypted_token = Span::get_attribute(decrypted_span, "auth.token")
  let decrypted_session = Span::get_attribute(decrypted_span, "session.data")
  
  match decrypted_email {
    Some(value) => assert_eq(value, "john.doe@example.com") // 解密后应该恢复原值
    None => assert_true(false)
  }
  
  match decrypted_token {
    Some(value) => assert_eq(value, "secret-auth-token") // 解密后应该恢复原值
    None => assert_true(false)
  }
  
  match decrypted_session {
    Some(value) => assert_eq(value, "sensitive-session-data") // 解密后应该恢复原值
    None => assert_true(false)
  }
}