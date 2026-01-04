// Azimuth 高级遥测集成测试用例
// 专注于遥测系统的集成和高级功能测试

// 测试1: 遥测数据聚合和分析
test "遥测数据聚合和分析测试" {
  // 创建聚合器
  let aggregator = TelemetryAggregator::new()
  
  // 添加不同类型的遥测数据
  let metric_data = [
    ("cpu.usage", 75.5),
    ("memory.usage", 60.2),
    ("disk.usage", 45.8),
    ("network.throughput", 1024.0)
  ]
  
  for (name, value) in metric_data {
    TelemetryAggregator::add_metric(aggregator, name, value)
  }
  
  // 计算统计数据
  let stats = TelemetryAggregator::calculate_statistics(aggregator)
  
  // 验证聚合结果
  assert_eq(Statistics::count(stats), 4)
  assert_eq(Statistics::max(stats), 1024.0)
  assert_eq(Statistics::min(stats), 45.8)
  assert_true(Statistics::average(stats) > 300.0)
  
  // 测试时间窗口聚合
  let time_window = TimeWindow::new(3600) // 1小时窗口
  let windowed_stats = TelemetryAggregator::aggregate_by_time(aggregator, time_window)
  
  assert_true(Statistics::count(windowed_stats) >= 0)
}

// 测试2: 分布式追踪链路完整性
test "分布式追踪链路完整性测试" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::with_config(TracerConfig::new())
  let tracer = TracerProvider::get_tracer(tracer_provider, "chain.integrity")
  
  // 创建根span
  let root_span = Tracer::start_span(tracer, "api.request")
  let root_context = Span::context(root_span)
  
  // 创建子span链
  let auth_span = Tracer::start_span_with_context(tracer, "auth.verify", root_context)
  Span::set_attribute(auth_span, "auth.method", StringValue("jwt"))
  Span::set_status(auth_span, Ok)
  Span::end(auth_span)
  
  let business_span = Tracer::start_span_with_context(tracer, "business.logic", root_context)
  Span::set_attribute(business_span, "operation.type", StringValue("payment"))
  
  let db_span = Tracer::start_span_with_context(tracer, "database.transaction", Span::context(business_span))
  Span::set_attribute(db_span, "db.operation", StringValue("insert"))
  Span::set_status(db_span, Ok)
  Span::end(db_span)
  
  Span::set_status(business_span, Ok)
  Span::end(business_span)
  
  // 完成根span
  Span::set_status(root_span, Ok)
  Span::end(root_span)
  
  // 验证追踪链完整性
  let trace_id = SpanContext::trace_id(Span::context(root_span))
  assert_eq(SpanContext::trace_id(Span::context(auth_span)), trace_id)
  assert_eq(SpanContext::trace_id(Span::context(business_span)), trace_id)
  assert_eq(SpanContext::trace_id(Span::context(db_span)), trace_id)
  
  // 验证父子关系
  assert_eq(SpanContext::parent_span_id(Span::context(auth_span)), SpanContext::span_id(Span::context(root_span)))
  assert_eq(SpanContext::parent_span_id(Span::context(business_span)), SpanContext::span_id(Span::context(root_span)))
  assert_eq(SpanContext::parent_span_id(Span::context(db_span)), SpanContext::span_id(Span::context(business_span)))
}

// 测试3: 遥测数据采样策略
test "遥测数据采样策略测试" {
  // 创建采样器
  let sampling_config = SamplingConfig::new()
  SamplingConfig::set_sampling_rate(sampling_config, 0.1) // 10%采样率
  
  let sampler = Sampler::create(SamplingStrategy::Probabilistic, sampling_config)
  
  // 测试采样决策
  let mut sampled_count = 0
  let total_requests = 1000
  
  for i in 0..total_requests {
    let sampling_context = SamplingContext::new("trace-" + i.to_string())
    let decision = Sampler::should_sample(sampler, sampling_context)
    
    if decision {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率在合理范围内（5%-15%）
  let actual_rate = sampled_count.to_float() / total_requests.to_float()
  assert_true(actual_rate > 0.05 and actual_rate < 0.15)
  
  // 测试基于属性的采样
  let attribute_sampler = Sampler::create(SamplingStrategy::AttributeBased, sampling_config)
  SamplingConfig::add_sampling_attribute(sampling_config, "high.priority", BoolValue(true))
  
  let high_priority_context = SamplingContext::with_attributes("trace-high", [
    ("high.priority", BoolValue(true))
  ])
  let low_priority_context = SamplingContext::with_attributes("trace-low", [
    ("high.priority", BoolValue(false))
  ])
  
  assert_true(Sampler::should_sample(attribute_sampler, high_priority_context))
  assert_false(Sampler::should_sample(attribute_sampler, low_priority_context))
}

// 测试4: 遥测数据导出和格式化
test "遥测数据导出和格式化测试" {
  // 创建测试数据
  let test_span = Span::new("test.export", Server, SpanContext::new("trace-123", "span-456", true, ""))
  Span::set_attribute(test_span, "http.method", StringValue("POST"))
  Span::set_attribute(test_span, "http.status_code", IntValue(201))
  Span::set_attribute(test_span, "duration.ms", IntValue(150))
  
  // 测试JSON导出
  let json_exporter = JsonExporter::new()
  let json_data = Exporter::export_span(json_exporter, test_span)
  
  assert_true(json_data.contains("\"name\":\"test.export\""))
  assert_true(json_data.contains("\"trace_id\":\"trace-123\""))
  assert_true(json_data.contains("\"span_id\":\"span-456\""))
  
  // 测试Prometheus格式导出
  let prometheus_exporter = PrometheusExporter::new()
  let prometheus_data = Exporter::export_metrics(prometheus_exporter, [
    ("http_requests_total", 1000.0),
    ("request_duration_seconds", 0.150)
  ])
  
  assert_true(prometheus_data.contains("http_requests_total 1000.0"))
  assert_true(prometheus_data.contains("request_duration_seconds 0.150"))
  
  // 测试OTLP格式导出
  let otlp_exporter = OtlpExporter::new()
  let otlp_data = Exporter::export_span(otlp_exporter, test_span)
  
  assert_true(otlp_data.length() > 0)
  assert_true(otlp_data.contains("trace-123"))
}

// 测试5: 遥测数据缓存和性能优化
test "遥测数据缓存和性能优化测试" {
  // 创建缓存管理器
  let cache_config = CacheConfig::new()
  CacheConfig::set_max_size(cache_config, 1000)
  CacheConfig::set_ttl(cache_config, 300) // 5分钟TTL
  
  let cache = TelemetryCache::new(cache_config)
  
  // 测试缓存写入
  let cache_key = "span-data-123"
  let cache_value = SpanData::new("test.span", "trace-123", "span-456")
  
  TelemetryCache::set(cache, cache_key, cache_value)
  
  // 测试缓存读取
  let cached_value = TelemetryCache::get(cache, cache_key)
  assert_eq(cached_value, Some(cache_value))
  
  // 测试缓存未命中
  let missing_value = TelemetryCache::get(cache, "non-existent-key")
  assert_eq(missing_value, None)
  
  // 测试缓存过期
  let expired_cache = TelemetryCache::new(CacheConfig::with_ttl(0)) // 立即过期
  TelemetryCache::set(expired_cache, "temp-key", cache_value)
  let expired_value = TelemetryCache::get(expired_cache, "temp-key")
  assert_eq(expired_value, None)
  
  // 测试缓存容量限制
  let small_cache = TelemetryCache::new(CacheConfig::with_max_size(2))
  TelemetryCache::set(small_cache, "key1", cache_value)
  TelemetryCache::set(small_cache, "key2", cache_value)
  TelemetryCache::set(small_cache, "key3", cache_value) // 应该淘汰key1
  
  assert_eq(TelemetryCache::get(small_cache, "key1"), None)
  assert_eq(TelemetryCache::get(small_cache, "key2"), Some(cache_value))
  assert_eq(TelemetryCache::get(small_cache, "key3"), Some(cache_value))
}

// 测试6: 遥测数据安全性和加密
test "遥测数据安全性和加密测试" {
  // 创建加密配置
  let encryption_config = EncryptionConfig::new()
  EncryptionConfig::set_algorithm(encryption_config, "AES-256-GCM")
  EncryptionConfig::set_key(encryption_config, "secure-key-32-chars-long-123456")
  
  let encryptor = DataEncryptor::new(encryption_config)
  
  // 测试敏感数据加密
  let sensitive_data = "user-token-abc123"
  let encrypted_data = DataEncryptor::encrypt(encryptor, sensitive_data)
  
  assert_ne(encrypted_data, sensitive_data)
  assert_true(encrypted_data.length() > sensitive_data.length())
  
  // 测试数据解密
  let decrypted_data = DataEncryptor::decrypt(encryptor, encrypted_data)
  assert_eq(decrypted_data, sensitive_data)
  
  // 测试PII数据脱敏
  let pii_masker = PIIMasker::new()
  let email_data = "user@example.com"
  let masked_email = PIIMasker::mask_email(pii_masker, email_data)
  
  assert_eq(masked_email, "u***@example.com")
  
  let phone_data = "+1-555-123-4567"
  let masked_phone = PIIMasker::mask_phone(pii_masker, phone_data)
  
  assert_eq(masked_phone, "+1-555-***-4567")
  
  // 测试数据访问控制
  let access_control = AccessControl::new()
  AccessControl::add_role(access_control, "admin", ["read", "write", "delete"])
  AccessControl::add_role(access_control, "viewer", ["read"])
  
  assert_true(AccessControl::can_access(access_control, "admin", "write"))
  assert_false(AccessControl::can_access(access_control, "viewer", "write"))
}

// 测试7: 遥测数据实时流处理
test "遥测数据实时流处理测试" {
  // 创建流处理器
  let stream_config = StreamConfig::new()
  StreamConfig::set_buffer_size(stream_config, 1000)
  StreamConfig::set_batch_size(stream_config, 100)
  
  let stream_processor = StreamProcessor::new(stream_config)
  
  // 创建数据生产者
  let producer = StreamProducer::new(stream_processor)
  
  // 创建数据消费者
  let consumer = StreamConsumer::new(stream_processor)
  
  // 测试数据流
  let mut produced_count = 0
  let mut consumed_count = 0
  
  // 生产数据
  for i in 0..100 {
    let telemetry_event = TelemetryEvent::new("event-" + i.to_string(), i.to_float())
    StreamProducer::produce(producer, telemetry_event)
    produced_count = produced_count + 1
  }
  
  // 消费数据
  while consumed_count < produced_count {
    match StreamConsumer::consume(consumer) {
      Some(event) => {
        consumed_count = consumed_count + 1
        assert_true(TelemetryEvent::id(event).starts_with("event-"))
      }
      None => break
    }
  }
  
  assert_eq(consumed_count, produced_count)
  
  // 测试流过滤
  let filter_processor = StreamFilter::new(fn(event) {
    TelemetryEvent::value(event) > 50.0
  })
  
  let filtered_stream = StreamProcessor::add_filter(stream_processor, filter_processor)
  
  // 生产测试数据
  for i in 0..10 {
    let event = TelemetryEvent::new("filter-test", i.to_float())
    StreamProducer::produce(producer, event)
  }
  
  // 消费过滤后的数据
  let mut filtered_count = 0
  for i in 0..10 {
    match StreamConsumer::consume(consumer) {
      Some(event) => {
        if TelemetryEvent::value(event) > 50.0 {
          filtered_count = filtered_count + 1
        }
      }
      None => break
    }
  }
  
  assert_eq(filtered_count, 0) // 0-9都小于50
}

// 测试8: 遥测数据异常检测和告警
test "遥测数据异常检测和告警测试" {
  // 创建异常检测器
  let anomaly_detector = AnomalyDetector::new()
  
  // 配置检测规则
  AnomalyDetector::add_rule(anomaly_detector, AnomalyRule::new(
    "cpu.usage",
    ComparisonOperator::GreaterThan,
    90.0,
    Severity::Critical
  ))
  
  AnomalyDetector::add_rule(anomaly_detector, AnomalyRule::new(
    "error.rate",
    ComparisonOperator::GreaterThan,
    0.05,
    Severity::Warning
  ))
  
  // 测试正常数据
  let normal_metrics = [
    ("cpu.usage", 45.0),
    ("memory.usage", 60.0),
    ("error.rate", 0.01)
  ]
  
  for (name, value) in normal_metrics {
    let anomalies = AnomalyDetector::detect(anomaly_detector, name, value)
    assert_eq(anomalies.length(), 0)
  }
  
  // 测试异常数据
  let anomaly_metrics = [
    ("cpu.usage", 95.0), // 超过阈值
    ("error.rate", 0.1)  // 超过阈值
  ]
  
  for (name, value) in anomaly_metrics {
    let anomalies = AnomalyDetector::detect(anomaly_detector, name, value)
    assert_eq(anomalies.length(), 1)
    
    let anomaly = anomalies[0]
    assert_eq(Anomaly::metric_name(anomaly), name)
    assert_eq(Anomaly::severity(anomaly), if name == "cpu.usage" { Critical } else { Warning })
  }
  
  // 测试告警管理
  let alert_manager = AlertManager::new()
  
  // 创建告警
  let cpu_alert = Alert::new("High CPU Usage", "CPU usage is 95%", Critical)
  let error_alert = Alert::new("High Error Rate", "Error rate is 10%", Warning)
  
  AlertManager::send_alert(alert_manager, cpu_alert)
  AlertManager::send_alert(alert_manager, error_alert)
  
  // 验证告警状态
  let active_alerts = AlertManager::get_active_alerts(alert_manager)
  assert_eq(active_alerts.length(), 2)
  
  // 测试告警解决
  AlertManager::resolve_alert(alert_manager, Alert::id(cpu_alert))
  let remaining_alerts = AlertManager::get_active_alerts(alert_manager)
  assert_eq(remaining_alerts.length(), 1)
}

// 测试9: 遥测数据压缩和传输优化
test "遥测数据压缩和传输优化测试" {
  // 创建压缩器
  let compressor = DataCompressor::new(CompressionAlgorithm::Gzip)
  
  // 创建测试数据
  let large_dataset = []
  for i in 0..1000 {
    let telemetry_point = TelemetryPoint::new(
      "metric-" + i.to_string(),
      i.to_float(),
      1640995200 + i
    )
    large_dataset = large_dataset.push(telemetry_point)
  }
  
  // 序列化数据
  let serialized_data = TelemetryData::serialize(large_dataset)
  let original_size = serialized_data.length()
  
  // 压缩数据
  let compressed_data = DataCompressor::compress(compressor, serialized_data)
  let compressed_size = compressed_data.length()
  
  // 验证压缩效果
  assert_true(compressed_size < original_size)
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  assert_true(compression_ratio < 0.8) // 至少20%的压缩率
  
  // 解压缩数据
  let decompressed_data = DataCompressor::decompress(compressor, compressed_data)
  assert_eq(decompressed_data, serialized_data)
  
  // 测试批量传输优化
  let batch_config = BatchConfig::new()
  BatchConfig::set_max_batch_size(batch_config, 100)
  BatchConfig::set_max_batch_bytes(batch_config, 1024 * 1024) // 1MB
  
  let batch_processor = BatchProcessor::new(batch_config)
  
  // 分批处理数据
  let batches = BatchProcessor::process(batch_processor, large_dataset)
  assert_true(batches.length() >= 10) // 1000个数据点，每批最多100个
  
  // 验证每批大小
  for batch in batches {
    assert_true(batch.length() <= 100)
  }
}

// 测试10: 遥测系统高可用性和故障恢复
test "遥测系统高可用性和故障恢复测试" {
  // 创建健康检查管理器
  let health_checker = HealthChecker::new()
  
  // 注册健康检查
  HealthChecker::register_check(health_checker, "database", fn() {
    // 模拟数据库健康检查
    true
  })
  
  HealthChecker::register_check(health_checker, "message_queue", fn() {
    // 模拟消息队列健康检查
    false // 模拟故障
  })
  
  // 执行健康检查
  let health_status = HealthChecker::check_all(health_checker)
  
  assert_true(HealthStatus::is_healthy(health_status, "database"))
  assert_false(HealthStatus::is_healthy(health_status, "message_queue"))
  assert_false(HealthStatus::is_overall_healthy(health_status))
  
  // 测试故障恢复
  let recovery_manager = RecoveryManager::new()
  
  // 配置恢复策略
  RecoveryManager::add_strategy(recovery_manager, "message_queue", RecoveryStrategy::new(
    RetryPolicy::ExponentialBackoff,
    3, // 最大重试次数
    1000 // 初始延迟(ms)
  ))
  
  // 模拟恢复过程
  let mut attempt_count = 0
  let recovery_result = RecoveryManager::attempt_recovery(recovery_manager, "message_queue", fn() {
    attempt_count = attempt_count + 1
    // 第三次尝试成功
    attempt_count >= 3
  })
  
  assert_true(recovery_result)
  assert_eq(attempt_count, 3)
  
  // 测试熔断器
  let circuit_breaker = CircuitBreaker::new(CircuitBreakerConfig::new(
    5, // 失败阈值
    60000 // 1分钟超时
  ))
  
  // 模拟连续失败
  for i in 0..6 {
    let result = CircuitBreaker::execute(circuit_breaker, fn() {
      false // 模拟失败
    })
    assert_false(result)
  }
  
  // 验证熔断器打开
  assert_eq(CircuitBreaker::state(circuit_breaker), CircuitBreakerState::Open)
  
  // 测试熔断器拒绝请求
  let rejected_result = CircuitBreaker::execute(circuit_breaker, fn() {
    true // 即使返回成功，也应该被拒绝
  })
  assert_false(rejected_result)
  
  // 测试半开状态（模拟时间过去）
  CircuitBreaker::transition_to_half_open(circuit_breaker)
  assert_eq(CircuitBreaker::state(circuit_breaker), CircuitBreakerState::HalfOpen)
  
  // 测试成功执行后关闭熔断器
  let success_result = CircuitBreaker::execute(circuit_breaker, fn() {
    true // 成功
  })
  assert_true(success_result)
  assert_eq(CircuitBreaker::state(circuit_breaker), CircuitBreakerState::Closed)
}