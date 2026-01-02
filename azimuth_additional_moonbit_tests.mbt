// Azimuth Additional MoonBit Test Suite
// 附加MoonBit测试用例，专注于遥测系统的扩展功能

// 测试 1: 度量仪表（Gauge）操作
test "gauge metric operations" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "gauge.test")
  
  // 创建仪表度量
  let memory_gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  let cpu_gauge = Meter::create_gauge(meter, "cpu.utilization", Some("CPU utilization"), Some("percent"))
  
  // 设置仪表值
  Gauge::set(memory_gauge, 1073741824.0)  // 1GB
  Gauge::set(cpu_gauge, 75.5)
  
  // 测试带属性的仪表设置
  Gauge::set_with_attributes(memory_gauge, 2147483648.0, [("instance", "server-01")])
  Gauge::set_with_attributes(cpu_gauge, 45.2, [("core", "core-0")])
  
  // 验证仪表属性
  assert_eq(memory_gauge.name, "memory.usage")
  assert_eq(memory_gauge.description, Some("Memory usage"))
  assert_eq(memory_gauge.unit, Some("bytes"))
  
  assert_eq(cpu_gauge.name, "cpu.utilization")
  assert_eq(cpu_gauge.unit, Some("percent"))
  
  assert_true(true)
}

// 测试 2: 跨服务上下文传播
test "cross-service context propagation" {
  let source_tracer = TracerProvider::get_tracer(TracerProvider::default(), "source.service")
  let target_tracer = TracerProvider::get_tracer(TracerProvider::default(), "target.service")
  
  // 在源服务中创建span
  let parent_span = Tracer::start_span(source_tracer, "parent.operation")
  Span::set_attribute(parent_span, "service.name", "source-service")
  Span::set_attribute(parent_span, "operation.type", "api-call")
  
  // 获取父span上下文
  let parent_ctx = Span::span_context(parent_span)
  
  // 模拟跨服务传播
  let propagated_ctx = Context::with_value(Context::root(), 
    ContextKey::new("trace.parent"), 
    SpanContext::trace_id(parent_ctx))
  
  // 在目标服务中创建子span
  let child_span = Tracer::start_span_with_context(target_tracer, "child.operation", propagated_ctx)
  Span::set_attribute(child_span, "service.name", "target-service")
  Span::set_attribute(child_span, "operation.type", "database-query")
  
  // 验证父子关系
  let child_ctx = Span::span_context(child_span)
  assert_true(SpanContext::is_valid(child_ctx))
  
  Span::end(child_span)
  Span::end(parent_span)
  
  assert_true(true)
}

// 测试 3: 度量聚合功能
test "metrics aggregation functionality" {
  let meter = MeterProvider::get_meter(MeterProvider::default(), "aggregation.test")
  
  // 创建用于聚合的计数器
  let request_counter = Meter::create_counter(meter, "http.requests", Some("HTTP requests"), Some("count"))
  
  // 模拟不同类型的请求
  Counter::add_with_attributes(request_counter, 10.0, [("method", "GET"), ("status", "200")])
  Counter::add_with_attributes(request_counter, 5.0, [("method", "POST"), ("status", "201")])
  Counter::add_with_attributes(request_counter, 2.0, [("method", "GET"), ("status", "404")])
  Counter::add_with_attributes(request_counter, 1.0, [("method", "POST"), ("status", "500")])
  
  // 创建响应时间直方图
  let response_histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  
  // 记录不同范围的响应时间
  Histogram::record(response_histogram, 25.0)   // 快速响应
  Histogram::record(response_histogram, 150.0)  // 中等响应
  Histogram::record(response_histogram, 500.0)  // 慢速响应
  Histogram::record(response_histogram, 1200.0) // 超慢响应
  
  // 验证度量创建
  assert_eq(request_counter.name, "http.requests")
  assert_eq(response_histogram.name, "response.time")
  assert_eq(response_histogram.unit, Some("ms"))
  
  assert_true(true)
}

// 测试 4: 日志关联性测试
test "log correlation and trace linking" {
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "correlation.test")
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "correlation.logger")
  
  // 创建span并获取上下文
  let span = Tracer::start_span(tracer, "correlated.operation")
  let span_ctx = Span::span_context(span)
  let trace_id = SpanContext::trace_id(span_ctx)
  let span_id = SpanContext::span_id(span_ctx)
  
  // 创建与span关联的日志
  let start_log = LogRecord::new(Info, "Operation started")
  LogRecord::add_attribute(start_log, "trace.id", trace_id)
  LogRecord::add_attribute(start_log, "span.id", span_id)
  LogRecord::add_attribute(start_log, "operation.phase", "start")
  
  let progress_log = LogRecord::new(Info, "Operation in progress")
  LogRecord::add_attribute(progress_log, "trace.id", trace_id)
  LogRecord::add_attribute(progress_log, "span.id", span_id)
  LogRecord::add_attribute(progress_log, "operation.phase", "progress")
  LogRecord::add_attribute(progress_log, "completion", 45)
  
  let end_log = LogRecord::new(Info, "Operation completed")
  LogRecord::add_attribute(end_log, "trace.id", trace_id)
  LogRecord::add_attribute(end_log, "span.id", span_id)
  LogRecord::add_attribute(end_log, "operation.phase", "end")
  LogRecord::add_attribute(end_log, "duration", 1250)
  
  // 发射日志
  Logger::emit(logger, start_log)
  Logger::emit(logger, progress_log)
  Logger::emit(logger, end_log)
  
  // 验证日志关联性
  assert_eq(LogRecord::get_attribute(start_log, "trace.id"), Some(trace_id))
  assert_eq(LogRecord::get_attribute(end_log, "span.id"), Some(span_id))
  
  Span::end(span)
  assert_true(true)
}

// 测试 5: 资源限制测试
test "resource limits and constraints" {
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "resource.test")
  
  // 创建资源受限的配置
  let limited_config = TracerConfig::new()
  let config_with_limits = TracerConfig::with_max_spans(limited_config, 100)
  let config_with_timeout = TracerConfig::with_max_span_duration(config_with_limits, 30000)
  
  // 使用受限配置创建tracer
  let limited_tracer = TracerProvider::get_tracer_with_config(TracerProvider::default(), 
    "limited.service", config_with_timeout)
  
  // 创建多个span测试限制
  let spans = []
  for i in 0..=10 {
    let span = Tracer::start_span(limited_tracer, "limited.span." + i.to_string())
    Span::set_attribute(span, "span.index", i)
    spans = spans.push(span)
  }
  
  // 模拟span持续时间限制
  let long_running_span = Tracer::start_span(limited_tracer, "long.running.operation")
  Span::set_attribute(long_running_span, "expected.duration", 35000)
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  Span::end(long_running_span)
  
  // 验证配置限制
  assert_eq(TracerConfig::max_spans(config_with_timeout), 100)
  assert_eq(TracerConfig::max_span_duration(config_with_timeout), 30000)
  
  assert_true(true)
}

// 测试 6: 配置管理测试
test "configuration management" {
  // 创建默认配置
  let default_config = TelemetryConfig::default()
  
  // 创建自定义配置
  let custom_config = TelemetryConfig::new()
    .with_service_name("custom.service")
    .with_service_version("2.5.1")
    .with_sampling_rate(0.1)
    .with_export_timeout(5000)
    .with_max_batch_size(512)
  
  // 验证配置属性
  assert_eq(TelemetryConfig::service_name(custom_config), "custom.service")
  assert_eq(TelemetryConfig::service_version(custom_config), "2.5.1")
  assert_eq(TelemetryConfig::sampling_rate(custom_config), 0.1)
  assert_eq(TelemetryConfig::export_timeout(custom_config), 5000)
  assert_eq(TelemetryConfig::max_batch_size(custom_config), 512)
  
  // 测试配置合并
  let base_config = TelemetryConfig::new()
    .with_service_name("base.service")
    .with_sampling_rate(0.5)
  
  let override_config = TelemetryConfig::new()
    .with_service_version("1.0.0")
    .with_export_timeout(3000)
  
  let merged_config = TelemetryConfig::merge(base_config, override_config)
  
  assert_eq(TelemetryConfig::service_name(merged_config), "base.service")
  assert_eq(TelemetryConfig::service_version(merged_config), "1.0.0")
  assert_eq(TelemetryConfig::sampling_rate(merged_config), 0.5)
  assert_eq(TelemetryConfig::export_timeout(merged_config), 3000)
  
  assert_true(true)
}

// 测试 7: 序列化/反序列化测试
test "serialization and deserialization" {
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "serialization.test")
  
  // 创建带有丰富属性的span
  let span = Tracer::start_span(tracer, "serialization.test")
  Span::set_attribute(span, "string.attr", "test.value")
  Span::set_attribute(span, "number.attr", 42)
  Span::set_attribute(span, "boolean.attr", true)
  Span::set_attribute(span, "float.attr", 3.14159)
  
  // 添加事件
  Span::add_event(span, "serialization.start", [("timestamp", "2025-01-02T10:00:00Z")])
  Span::add_event(span, "serialization.end", [("duration", "150ms")])
  
  // 序列化span数据
  let serialized_data = SpanSerializer::serialize(span)
  
  // 验证序列化数据不为空
  assert_true(String::length(serialized_data) > 0)
  
  // 反序列化span数据
  let deserialized_span = SpanDeserializer::deserialize(serialized_data)
  
  // 验证反序列化后的属性
  assert_eq(Span::name(deserialized_span), "serialization.test")
  assert_eq(Span::get_attribute(deserialized_span, "string.attr"), Some("test.value"))
  assert_eq(Span::get_attribute(deserialized_span, "number.attr"), Some("42"))
  assert_eq(Span::get_attribute(deserialized_span, "boolean.attr"), Some("true"))
  
  Span::end(span)
  assert_true(true)
}

// 测试 8: 性能基准测试
test "performance benchmarking" {
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "performance.test")
  let meter = MeterProvider::get_meter(MeterProvider::default(), "performance.metrics")
  
  // 创建性能度量
  let span_creation_time = Meter::create_histogram(meter, "span.creation.time", Some("Span creation time"), Some("microseconds"))
  let attribute_set_time = Meter::create_histogram(meter, "attribute.set.time", Some("Attribute set time"), Some("microseconds"))
  
  // 测试span创建性能
  let start_time = Time::now()
  let spans = []
  for i in 0..=100 {
    let span = Tracer::start_span(tracer, "performance.span." + i.to_string())
    spans = spans.push(span)
  }
  let creation_duration = Time::elapsed(start_time)
  Histogram::record(span_creation_time, creation_duration)
  
  // 测试属性设置性能
  for span in spans {
    let attr_start = Time::now()
    Span::set_attribute(span, "batch.index", spans.index_of(span))
    Span::set_attribute(span, "batch.size", spans.length())
    Span::set_attribute(span, "performance.test", true)
    let attr_duration = Time::elapsed(attr_start)
    Histogram::record(attribute_set_time, attr_duration)
  }
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  // 验证性能度量
  assert_eq(span_creation_time.name, "span.creation.time")
  assert_eq(attribute_set_time.name, "attribute.set.time")
  assert_eq(span_creation_time.unit, Some("microseconds"))
  
  assert_true(true)
}

// 测试 9: 数据完整性测试
test "data integrity validation" {
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "integrity.test")
  
  // 创建span并设置校验和属性
  let span = Tracer::start_span(tracer, "integrity.test")
  Span::set_attribute(span, "data.checksum", "a1b2c3d4e5f6")
  Span::set_attribute(span, "data.version", "1.0.0")
  Span::set_attribute(span, "data.size", 1024)
  
  // 添加数据完整性事件
  Span::add_event(span, "data.validated", [
    ("validation.status", "passed"),
    ("validation.algorithm", "SHA-256"),
    ("validation.timestamp", "2025-01-02T10:00:00Z")
  ])
  
  // 模拟数据传输
  let transmitted_data = SpanData::export(span)
  
  // 验证传输后的数据完整性
  let received_span = SpanData::import(transmitted_data)
  let received_checksum = Span::get_attribute(received_span, "data.checksum")
  let received_version = Span::get_attribute(received_span, "data.version")
  let received_size = Span::get_attribute(received_span, "data.size")
  
  assert_eq(received_checksum, Some("a1b2c3d4e5f6"))
  assert_eq(received_version, Some("1.0.0"))
  assert_eq(received_size, Some("1024"))
  
  // 验证事件完整性
  let events = Span::get_events(received_span)
  assert_true(events.length() > 0)
  
  Span::end(span)
  assert_true(true)
}

// 测试 10: 国际化支持测试
test "internationalization support" {
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "i18n.test")
  
  // 创建不同语言的日志记录
  let english_log = LogRecord::new(Info, "Operation completed successfully")
  LogRecord::add_attribute(english_log, "language", "en")
  LogRecord::add_attribute(english_log, "locale", "en-US")
  
  let chinese_log = LogRecord::new(Info, "操作成功完成")
  LogRecord::add_attribute(chinese_log, "language", "zh")
  LogRecord::add_attribute(chinese_log, "locale", "zh-CN")
  
  let japanese_log = LogRecord::new(Info, "操作が正常に完了しました")
  LogRecord::add_attribute(japanese_log, "language", "ja")
  LogRecord::add_attribute(japanese_log, "locale", "ja-JP")
  
  // 测试本地化错误消息
  let localized_errors = [
    ("en", "Operation failed due to invalid input"),
    ("zh", "由于输入无效导致操作失败"),
    ("ja", "無効な入力のため操作が失敗しました"),
    ("es", "La operación falló debido a una entrada no válida"),
    ("fr", "L'opération a échoué en raison d'une entrée invalide")
  ]
  
  for (lang, message) in localized_errors {
    let error_log = LogRecord::new(Error, message)
    LogRecord::add_attribute(error_log, "language", lang)
    LogRecord::add_attribute(error_log, "error.code", "ERR_INVALID_INPUT")
    Logger::emit(logger, error_log)
  }
  
  // 发射多语言日志
  Logger::emit(logger, english_log)
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, japanese_log)
  
  // 验证国际化属性
  assert_eq(LogRecord::get_attribute(english_log, "language"), Some("en"))
  assert_eq(LogRecord::get_attribute(chinese_log, "language"), Some("zh"))
  assert_eq(LogRecord::get_attribute(japanese_log, "language"), Some("ja"))
  
  assert_true(true)
}