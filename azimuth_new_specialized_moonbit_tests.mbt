// Azimuth New Specialized MoonBit Test Suite
// 新的专业化MoonBit测试用例，涵盖遥测系统的高级功能

// 测试1: 遥测数据采样策略
pub test "遥测数据采样策略测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "sampling-test")
  
  // 创建采样配置
  let sampling_config = azimuth::SamplingConfig::new()
  let probabilistic_sampler = azimuth::ProbabilisticSampler::new(0.1)  // 10%采样率
  let deterministic_sampler = azimuth::DeterministicSampler::new()
  let rate_limiter = azimuth::RateLimitingSampler::new(100)  // 每秒最多100个样本
  
  // 测试概率采样
  let sampled_spans = []
  let total_spans = 1000
  
  for i in 0..total_spans {
    let span = azimuth::Tracer::start_span(tracer, "sampling-test-" + i.to_string())
    let sampling_decision = azimuth::ProbabilisticSampler::should_sample(probabilistic_sampler, 
      azimuth::Span::span_context(span))
    
    if sampling_decision {
      sampled_spans.push(span)
    }
    
    azimuth::Span::end(span)
  }
  
  // 验证采样率在合理范围内（5%-15%）
  let actual_sampling_rate = sampled_spans.length().to_double() / total_spans.to_double()
  assert_true(actual_sampling_rate >= 0.05 && actual_sampling_rate <= 0.15)
  
  // 测试确定性采样
  let trace_ids = ["trace1", "trace2", "trace3", "trace1", "trace2"]
  let deterministic_results = []
  
  for trace_id in trace_ids {
    let span_ctx = azimuth::SpanContext::new(trace_id, "span-" + @random().to_string(), true, "")
    let decision = azimuth::DeterministicSampler::should_sample(deterministic_sampler, span_ctx)
    deterministic_results.push(decision)
  }
  
  // 验证相同trace ID产生相同的采样决策
  assert_true(deterministic_results[0] == deterministic_results[3])  // trace1
  assert_true(deterministic_results[1] == deterministic_results[4])  // trace2
}

// 测试2: 遥测数据聚合和统计
pub test "遥测数据聚合和统计测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建各种度量类型
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration")
  let error_gauge = azimuth::Meter::create_gauge(meter, "http.errors.current")
  
  // 模拟不同类型的请求数据
  let request_types = ["GET", "POST", "PUT", "DELETE"]
  let response_codes = [200, 201, 400, 404, 500]
  
  for i in 0..500 {
    let request_type = request_types[i % request_types.length()]
    let response_code = response_codes[@random().floor() % response_codes.length()]
    let response_time = 10.0 + (@random() * 200.0)
    
    // 记录请求计数
    azimuth::Counter::add(request_counter, 1.0)
    
    // 记录响应时间
    azimuth::Histogram::record(response_histogram, response_time)
    
    // 模拟错误情况
    if response_code >= 400 {
      azimuth::Gauge::set(error_gauge, @random() * 10.0)
    }
  }
  
  // 测试数据聚合功能
  let aggregation_config = azimuth::AggregationConfig::new()
  let time_window = 60000L  // 60秒窗口
  let aggregation_result = azimuth::MetricsAggregator::aggregate_in_window(
    [request_counter, response_histogram, error_gauge], time_window)
  
  // 验证聚合结果
  assert_true(azimuth::AggregationResult::has_metric(aggregation_result, "http.requests.total"))
  assert_true(azimuth::AggregationResult::has_metric(aggregation_result, "http.response.duration"))
  assert_true(azimuth::AggregationResult::has_metric(aggregation_result, "http.errors.current"))
  
  // 验证统计信息
  let request_stats = azimuth::AggregationResult::get_stats(aggregation_result, "http.requests.total")
  assert_true(azimuth::MetricStats::count(request_stats) == 500)
  assert_true(azimuth::MetricStats::sum(request_stats) == 500.0)
}

// 测试3: 分布式追踪的上下文传播
pub test "分布式追踪上下文传播测试" {
  // 创建跨服务的追踪场景
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "distributed-tracing")
  
  // 服务A：入口点
  let service_a_span = azimuth::Tracer::start_span(tracer, "service-a.entry")
  let root_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_a_span))
  
  // 创建传播器
  let propagator = azimuth::CompositePropagator::new([
    azimuth::TraceContextPropagator::new(),
    azimuth::BaggagePropagator::new(),
    azimuth::CorrelationContextPropagator::new()
  ])
  
  // 创建上下文
  let ctx = azimuth::Context::current()
  let ctx_with_span = azimuth::Context::with_span(ctx, service_a_span)
  
  // 添加Baggage
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  
  let final_ctx = azimuth::Context::with_baggage(ctx_with_span, baggage_with_session)
  
  // 模拟HTTP头传播
  let headers = azimuth::Headers::new()
  azimuth::CompositePropagator::inject(propagator, final_ctx, headers)
  
  // 服务B：提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(propagator, headers)
  let extracted_span = azimuth::Context::span(extracted_ctx)
  let extracted_baggage = azimuth::Context::baggage(extracted_ctx)
  
  // 验证Trace ID传播
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(extracted_span)), root_trace_id)
  
  // 验证Baggage传播
  assert_eq(azimuth::Baggage::get_entry(extracted_baggage, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(extracted_baggage, "session.id"), Some("session-abcdef"))
  
  // 服务C：继续追踪
  let service_c_span = azimuth::Tracer::start_span_with_context(tracer, "service-c.process", extracted_ctx)
  let service_c_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_c_span))
  
  // 验证Trace ID在整个调用链中保持一致
  assert_eq(service_c_trace_id, root_trace_id)
  
  // 结束所有Span
  azimuth::Span::end(service_a_span)
  azimuth::Span::end(service_c_span)
}

// 测试4: 遥测数据持久化和存储
pub test "遥测数据持久化和存储测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "persistence-test")
  
  // 创建存储配置
  let storage_config = azimuth::StorageConfig::new()
  let file_storage = azimuth::FileStorage::new("/tmp/telemetry.data")
  let memory_storage = azimuth::MemoryStorage::new(1000)  // 最多1000条记录
  
  // 创建批量处理器
  let batch_processor = azimuth::BatchSpanProcessor::new(file_storage, 100, 5000)  // 100个或5秒批量处理
  
  // 生成测试数据
  for i in 0..150 {
    let span = azimuth::Tracer::start_span(tracer, "persistence-test-" + i.to_string())
    
    // 添加属性
    azimuth::Span::set_attribute(span, "test.id", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "test.type", azimuth::StringValue("persistence"))
    
    // 添加事件
    azimuth::Span::add_event(span, "test.event", 
      Some([("event.id", azimuth::StringValue("event-" + i.to_string()))]))
    
    // 强制处理
    azimuth::BatchSpanProcessor::on_end(batch_processor, span)
  }
  
  // 强制刷新剩余数据
  azimuth::BatchSpanProcessor::force_flush(batch_processor)
  
  // 验证数据持久化
  let storage_stats = azimuth::FileStorage::get_stats(file_storage)
  assert_true(azimuth::StorageStats::record_count(storage_stats) >= 150)
  
  // 测试数据检索
  let retrieved_spans = azimuth::FileStorage::get_spans_by_attribute(file_storage, 
    "test.type", azimuth::StringValue("persistence"))
  assert_true(retrieved_spans.length() == 150)
  
  // 测试内存存储
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "memory-test-" + i.to_string())
    azimuth::MemoryStorage::store(memory_storage, span)
    azimuth::Span::end(span)
  }
  
  let memory_stats = azimuth::MemoryStorage::get_stats(memory_storage)
  assert_true(azimuth::StorageStats::record_count(memory_stats) == 50)
}

// 测试5: 遥测数据的实时监控和告警
pub test "遥测数据实时监控和告警测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "monitoring-test")
  
  // 创建度量
  let error_rate = azimuth::Meter::create_counter(meter, "errors.total")
  let response_time = azimuth::Meter::create_histogram(meter, "response.time")
  let active_connections = azimuth::Meter::create_gauge(meter, "active.connections")
  
  // 创建告警规则
  let alert_rules = [
    azimuth::AlertRule::new("high_error_rate", "errors.total", "rate", ">", 10.0, 60000),
    azimuth::AlertRule::new("slow_response", "response.time", "p95", ">", 200.0, 300000),
    azimuth::AlertRule::new("connection_overload", "active.connections", "current", ">", 100.0, 10000)
  ]
  
  // 创建告警管理器
  let alert_manager = azimuth::AlertManager::new(alert_rules)
  
  // 模拟正常操作
  for i in 0..50 {
    azimuth::Counter::add(error_rate, 1.0)
    azimuth::Histogram::record(response_time, 50.0 + (@random() * 50.0))
    azimuth::Gauge::set(active_connections, 10.0 + (@random() * 20.0))
  }
  
  // 检查告警状态
  let alert_status = azimuth::AlertManager::check_alerts(alert_manager)
  assert_true(azimuth::AlertStatus::active_alert_count(alert_status) == 0)
  
  // 模拟异常情况 - 高错误率
  for i in 0..20 {
    azimuth::Counter::add(error_rate, 1.0)
  }
  
  // 模拟慢响应
  for i in 0..30 {
    azimuth::Histogram::record(response_time, 250.0 + (@random() * 100.0))
  }
  
  // 模拟连接过载
  azimuth::Gauge::set(active_connections, 150.0)
  
  // 再次检查告警状态
  let updated_alert_status = azimuth::AlertManager::check_alerts(alert_manager)
  assert_true(azimuth::AlertStatus::active_alert_count(updated_alert_status) >= 2)
  
  // 验证具体告警
  assert_true(azimuth::AlertStatus::is_alert_active(updated_alert_status, "high_error_rate"))
  assert_true(azimuth::AlertStatus::is_alert_active(updated_alert_status, "connection_overload"))
}

// 测试6: 遥测数据的性能基准测试
pub test "遥测数据性能基准测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  // 性能测试参数
  let span_count = 10000
  let attribute_count = 20
  let event_count = 5
  
  // 测试Span创建性能
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let spans = []
  for i in 0..span_count {
    let span = azimuth::Tracer::start_span(tracer, "performance-span-" + i.to_string())
    
    // 添加多个属性
    for j in 0..attribute_count {
      azimuth::Span::set_attribute(span, "attr." + j.to_string(), 
        azimuth::StringValue("value-" + i.to_string() + "-" + j.to_string()))
    }
    
    // 添加多个事件
    for k in 0..event_count {
      azimuth::Span::add_event(span, "event-" + k.to_string(), 
        Some([("event.data", azimuth::StringValue("data-" + k.to_string()))]))
    }
    
    spans.push(span)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - start_time
  let creation_rate = span_count.to_double() / (creation_time.to_double() / 1000000000.0)
  
  // 测试Span结束性能
  start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for span in spans {
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - start_time
  let end_rate = span_count.to_double() / (end_time.to_double() / 1000000000.0)
  
  // 验证性能指标
  assert_true(creation_rate > 10000.0)  // 至少每秒10000个Span
  assert_true(end_rate > 50000.0)       // 至少每秒50000个Span结束
  
  // 测试内存使用
  let memory_before = azimuth::MemoryMonitor::get_used_memory()
  
  // 创建大量Span并保持引用
  let memory_test_spans = []
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "memory-test-" + i.to_string())
    azimuth::Span::set_attribute(span, "large.data", 
      azimuth::StringValue("x" * 1000))  // 1KB数据
    memory_test_spans.push(span)
  }
  
  let memory_after = azimuth::MemoryMonitor::get_used_memory()
  let memory_increase = memory_after - memory_before
  
  // 验证内存使用合理（每个Span不超过2KB）
  let memory_per_span = memory_increase.to_double() / 1000.0
  assert_true(memory_per_span < 2048.0)  // 小于2KB per Span
}

// 测试7: 遥测系统的配置管理
pub test "遥测系统配置管理测试" {
  // 创建配置管理器
  let config_manager = azimuth::ConfigurationManager::new()
  
  // 测试默认配置
  let default_config = azimuth::ConfigurationManager::get_default_config(config_manager)
  assert_true(azimuth::TelemetryConfig::sampling_rate(default_config) > 0.0)
  assert_true(azimuth::TelemetryConfig::batch_size(default_config) > 0)
  assert_true(azimuth::TelemetryConfig::export_timeout_ms(default_config) > 0)
  
  // 测试配置加载
  let config_data = {
    "sampling": {
      "rate": 0.1,
      "type": "probabilistic"
    },
    "batching": {
      "size": 500,
      "timeout": 30000
    },
    "exporters": [
      {
        "type": "file",
        "path": "/tmp/telemetry.log"
      },
      {
        "type": "http",
        "endpoint": "https://api.example.com/telemetry"
      }
    ],
    "resource": {
      "service.name": "test-service",
      "service.version": "1.0.0",
      "environment": "test"
    }
  }
  
  let loaded_config = azimuth::ConfigurationManager::load_from_json(config_manager, config_data)
  assert_eq(azimuth::TelemetryConfig::sampling_rate(loaded_config), 0.1)
  assert_eq(azimuth::TelemetryConfig::batch_size(loaded_config), 500)
  assert_eq(azimuth::TelemetryConfig::export_timeout_ms(loaded_config), 30000)
  
  // 测试配置验证
  let invalid_config_data = {
    "sampling": {
      "rate": -0.1,  // 无效：负数
      "type": "invalid_type"  // 无效：未知类型
    },
    "batching": {
      "size": 0,  // 无效：零
      "timeout": -1000  // 无效：负数
    }
  }
  
  let validation_result = azimuth::ConfigurationManager::validate_config(config_manager, invalid_config_data)
  assert_false(azimuth::ValidationResult::is_valid(validation_result))
  assert_true(azimuth::ValidationResult::error_count(validation_result) >= 3)
  
  // 测试配置热更新
  let tracer_provider = azimuth::TracerProvider::with_config(loaded_config)
  let initial_sampling_rate = azimuth::TracerProvider::get_sampling_rate(tracer_provider)
  
  let updated_config_data = {
    "sampling": {
      "rate": 0.2,  // 更新采样率
      "type": "probabilistic"
    }
  }
  
  azimuth::ConfigurationManager::hot_update(config_manager, updated_config_data)
  let updated_sampling_rate = azimuth::TracerProvider::get_sampling_rate(tracer_provider)
  
  assert_eq(updated_sampling_rate, 0.2)
  assert_true(updated_sampling_rate != initial_sampling_rate)
}

// 测试8: 遥测数据的多格式导出
pub test "遥测数据多格式导出测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "export-test")
  
  // 创建测试Span
  let test_span = azimuth::Tracer::start_span(tracer, "export-test-span")
  azimuth::Span::set_attribute(test_span, "service.name", azimuth::StringValue("test-service"))
  azimuth::Span::set_attribute(test_span, "operation.name", azimuth::StringValue("test-operation"))
  azimuth::Span::add_event(test_span, "test-event", 
    Some([("event.data", azimuth::StringValue("test-data"))]))
  
  // 测试JSON导出
  let json_exporter = azimuth::JsonExporter::new()
  let json_output = azimuth::JsonExporter::export_span(json_exporter, test_span)
  
  assert_true(azimuth::StringUtils::contains(json_output, "export-test-span"))
  assert_true(azimuth::StringUtils::contains(json_output, "test-service"))
  assert_true(azimuth::StringUtils::contains(json_output, "test-event"))
  
  // 测试XML导出
  let xml_exporter = azimuth::XmlExporter::new()
  let xml_output = azimuth::XmlExporter::export_span(xml_exporter, test_span)
  
  assert_true(azimuth::StringUtils::contains(xml_output, "<span"))
  assert_true(azimuth::StringUtils::contains(xml_output, "export-test-span"))
  assert_true(azimuth::StringUtils::contains(xml_output, "</span>"))
  
  // 测试CSV导出
  let csv_exporter = azimuth::CsvExporter::new()
  let csv_output = azimuth::CsvExporter::export_span(csv_exporter, test_span)
  
  assert_true(azimuth::StringUtils::contains(csv_output, "trace_id,span_id,parent_span_id"))
  assert_true(azimuth::StringUtils::contains(csv_output, "export-test-span"))
  
  // 测试Protobuf导出
  let protobuf_exporter = azimuth::ProtobufExporter::new()
  let protobuf_output = azimuth::ProtobufExporter::export_span(protobuf_exporter, test_span)
  
  assert_true(azimuth::ByteUtils::length(protobuf_output) > 0)
  
  // 测试OpenTelemetry格式导出
  let otlp_exporter = azimuth::OtlpExporter::new()
  let otlp_output = azimuth::OtlpExporter::export_span(otlp_exporter, test_span)
  
  assert_true(azimuth::ByteUtils::length(otlp_output) > 0)
  
  // 验证导出数据的一致性
  let parsed_json = azimuth::JsonUtils::parse(json_output)
  let parsed_xml = azimuth::XmlUtils::parse(xml_output)
  
  assert_eq(azimuth::JsonUtils::get_string(parsed_json, "name"), "export-test-span")
  assert_eq(azimuth::XmlUtils::get_string(parsed_xml, "span/name"), "export-test-span")
  
  azimuth::Span::end(test_span)
}

// 测试9: 遥测系统的安全性测试
pub test "遥测系统安全性测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "security-test")
  
  // 测试敏感数据过滤
  let security_config = azimuth::SecurityConfig::new()
  azimuth::SecurityConfig::add_sensitive_attribute(security_config, "password")
  azimuth::SecurityConfig::add_sensitive_attribute(security_config, "token")
  azimuth::SecurityConfig::add_sensitive_attribute(security_config, "api_key")
  azimuth::SecurityConfig::add_sensitive_pattern(security_config, "credit_card", "\d{16}")
  
  let secure_processor = azimuth::SecureSpanProcessor::new(security_config)
  
  // 创建包含敏感数据的Span
  let secure_span = azimuth::Tracer::start_span(tracer, "secure-test-span")
  
  // 添加敏感属性
  azimuth::Span::set_attribute(secure_span, "username", azimuth::StringValue("user123"))
  azimuth::Span::set_attribute(secure_span, "password", azimuth::StringValue("secret123"))
  azimuth::Span::set_attribute(secure_span, "token", azimuth::StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
  azimuth::Span::set_attribute(secure_span, "credit_card", azimuth::StringValue("1234567890123456"))
  azimuth::Span::set_attribute(secure_span, "normal_data", azimuth::StringValue("this is normal"))
  
  // 处理Span
  azimuth::SecureSpanProcessor::on_end(secure_processor, secure_span)
  
  // 获取处理后的Span数据
  let processed_data = azimuth::SecureSpanProcessor::get_processed_data(secure_processor)
  
  // 验证敏感数据被正确处理
  assert_eq(azimuth::SpanData::get_attribute(processed_data, "username"), 
    Some(azimuth::StringValue("user123")))  // 非敏感数据保持不变
  assert_eq(azimuth::SpanData::get_attribute(processed_data, "password"), 
    Some(azimuth::StringValue("***")))  // 敏感数据被遮蔽
  assert_eq(azimuth::SpanData::get_attribute(processed_data, "token"), 
    Some(azimuth::StringValue("***")))  // 敏感数据被遮蔽
  assert_eq(azimuth::SpanData::get_attribute(processed_data, "credit_card"), 
    Some(azimuth::StringValue("************3456")))  // 信用卡号部分遮蔽
  assert_eq(azimuth::SpanData::get_attribute(processed_data, "normal_data"), 
    Some(azimuth::StringValue("this is normal")))  // 非敏感数据保持不变
  
  // 测试数据加密
  let encryption_config = azimuth::EncryptionConfig::new()
  azimuth::EncryptionConfig::set_encryption_key(encryption_config, "test-key-12345")
  
  let encrypted_processor = azimuth::EncryptedSpanProcessor::new(encryption_config)
  
  // 创建需要加密的Span
  let encrypted_span = azimuth::Tracer::start_span(tracer, "encrypted-test-span")
  azimuth::Span::set_attribute(encrypted_span, "sensitive_info", azimuth::StringValue("highly sensitive data"))
  
  // 处理加密
  azimuth::EncryptedSpanProcessor::on_end(encrypted_processor, encrypted_span)
  
  // 获取加密数据
  let encrypted_data = azimuth::EncryptedSpanProcessor::get_encrypted_data(encrypted_processor)
  
  // 验证数据已加密
  let encrypted_value = azimuth::SpanData::get_attribute(encrypted_data, "sensitive_info")
  assert_true(azimuth::StringUtils::length(azimuth::AttributeValue::to_string(encrypted_value.unwrap())) > 
              azimuth::StringUtils::length("highly sensitive data"))
  
  // 验证可以解密
  let decrypted_data = azimuth::EncryptionUtils::decrypt(encryption_config, 
    azimuth::AttributeValue::to_string(encrypted_value.unwrap()))
  assert_eq(decrypted_data, "highly sensitive data")
  
  azimuth::Span::end(secure_span)
  azimuth::Span::end(encrypted_span)
}

// 测试10: 遥测系统的故障恢复和容错性
pub test "遥测系统故障恢复和容错性测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "fault-tolerance-test")
  
  // 创建故障注入器
  let fault_injector = azimuth::FaultInjector::new()
  
  // 测试导出器故障恢复
  let unreliable_exporter = azimuth::UnreliableExporter::new()
  azimuth::UnreliableExporter::set_failure_rate(unreliable_exporter, 0.3)  // 30%失败率
  azimuth::UnreliableExporter::set_failure_duration(unreliable_exporter, 5000)  // 失败持续5秒
  
  // 创建重试处理器
  let retry_config = azimuth::RetryConfig::new()
  azimuth::RetryConfig::set_max_attempts(retry_config, 3)
  azimuth::RetryConfig::set_backoff_strategy(retry_config, "exponential")
  
  let retry_processor = azimuth::RetrySpanProcessor::new(unreliable_exporter, retry_config)
  
  // 创建测试Span并尝试导出
  let test_spans = []
  for i in 0..20 {
    let span = azimuth::Tracer::start_span(tracer, "fault-test-" + i.to_string())
    azimuth::Span::set_attribute(span, "test.id", azimuth::IntValue(i))
    test_spans.push(span)
    
    // 尝试导出
    azimuth::RetrySpanProcessor::on_end(retry_processor, span)
  }
  
  // 等待重试完成
  azimuth::RetrySpanProcessor::wait_for_completion(retry_processor, 10000)
  
  // 验证最终导出成功
  let export_stats = azimuth::RetrySpanProcessor::get_export_stats(retry_processor)
  assert_true(azimuth::ExportStats::success_count(export_stats) >= 14)  // 至少70%成功
  
  // 测试内存压力下的稳定性
  let memory_monitor = azimuth::MemoryMonitor::new()
  azimuth::MemoryMonitor::set_pressure_threshold(memory_monitor, 0.8)  // 80%内存使用率
  
  let memory_aware_processor = azimuth::MemoryAwareSpanProcessor::new(memory_monitor)
  
  // 在内存压力下创建大量Span
  let memory_test_spans = []
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "memory-pressure-test-" + i.to_string())
    
    // 添加大量属性以增加内存使用
    for j in 0..10 {
      azimuth::Span::set_attribute(span, "large.attr." + j.to_string(), 
        azimuth::StringValue("x" * 100))  // 100字符
    }
    
    memory_test_spans.push(span)
    azimuth::MemoryAwareSpanProcessor::on_end(memory_aware_processor, span)
    
    // 检查内存压力
    if azimuth::MemoryMonitor::is_under_pressure(memory_monitor) {
      break
    }
  }
  
  // 验证内存压力下的行为
  let memory_stats = azimuth::MemoryAwareSpanProcessor::get_memory_stats(memory_aware_processor)
  assert_true(azimuth::MemoryStats::spans_dropped(memory_stats) >= 0)
  assert_true(azimuth::MemoryStats::spans_processed(memory_stats) > 0)
  
  // 测试网络分区恢复
  let network_partitioner = azimuth::NetworkPartitioner::new()
  azimuth::NetworkPartitioner::simulate_partition(network_partitioner, 3000)  // 3秒网络分区
  
  let network_aware_processor = azimuth::NetworkAwareSpanProcessor::new()
  
  // 在网络分区期间创建Span
  let network_test_spans = []
  for i in 0..10 {
    let span = azimuth::Tracer::start_span(tracer, "network-test-" + i.to_string())
    network_test_spans.push(span)
    azimuth::NetworkAwareSpanProcessor::on_end(network_aware_processor, span)
  }
  
  // 等待网络恢复
  azimuth::NetworkPartitioner::wait_for_recovery(network_partitioner, 5000)
  
  // 验证网络恢复后的数据同步
  let sync_stats = azimuth::NetworkAwareSpanProcessor::get_sync_stats(network_aware_processor)
  assert_true(azimuth::SyncStats::pending_count(sync_stats) >= 0)
  assert_true(azimuth::SyncStats::synced_count(sync_stats) > 0)
  
  // 清理
  for span in test_spans {
    if azimuth::Span::is_recording(span) {
      azimuth::Span::end(span)
    }
  }
  
  for span in memory_test_spans {
    if azimuth::Span::is_recording(span) {
      azimuth::Span::end(span)
    }
  }
  
  for span in network_test_spans {
    if azimuth::Span::is_recording(span) {
      azimuth::Span::end(span)
    }
  }
}