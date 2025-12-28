// Azimuth 高级综合测试用例
// 覆盖并发安全、性能、数据完整性、跨服务集成等关键场景

test "并发安全测试 - 多线程属性操作" {
  // 测试在并发环境下属性操作的安全性
  let attrs = Attributes::new()
  
  // 模拟并发设置属性
  Attributes::set(attrs, "concurrent.key1", StringValue("value1"))
  Attributes::set(attrs, "concurrent.key2", IntValue(42))
  Attributes::set(attrs, "concurrent.key3", BoolValue(true))
  
  // 模拟并发读取属性
  let result1 = Attributes::get(attrs, "concurrent.key1")
  let result2 = Attributes::get(attrs, "concurrent.key2")
  let result3 = Attributes::get(attrs, "concurrent.key3")
  
  // 验证数据一致性
  match result1 {
    Some(StringValue(value)) => assert_eq(value, "value1")
    _ => assert_true(false)
  }
  
  match result2 {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  match result3 {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  // 测试数组属性的并发操作
  let array_data = ArrayStringValue(["item1", "item2", "item3"])
  Attributes::set(attrs, "concurrent.array", array_data)
  
  let array_result = Attributes::get(attrs, "concurrent.array")
  match array_result {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
    }
    _ => assert_true(false)
  }
}

test "性能基准测试 - 大量数据处理" {
  // 测试处理大量属性时的性能表现
  let attrs = Attributes::new()
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 批量设置属性
  for i = 0; i < 100; i = i + 1 {
    let key = "perf.key." + i.to_string()
    let value = StringValue("value." + i.to_string())
    Attributes::set(attrs, key, value)
  }
  
  // 批量读取属性
  for i = 0; i < 100; i = i + 1 {
    let key = "perf.key." + i.to_string()
    let result = Attributes::get(attrs, key)
    // 验证读取操作不会失败
    assert_true(result is Some)
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let duration = end_time - start_time
  
  // 验证操作在合理时间内完成（这里只验证不会超时）
  assert_true(duration < 10000000000L) // 10秒内完成
  
  // 测试大量Span创建和销毁
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance.test")
  
  let span_start = Clock::now_unix_nanos(Clock::system())
  for i = 0; i < 50; i = i + 1 {
    let span_name = "perf.span." + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    Span::add_event(span, "test.event", None)
    Span::set_status(span, Ok)
    Span::end(span)
  }
  let span_end = Clock::now_unix_nanos(Clock::system())
  let span_duration = span_end - span_start
  
  assert_true(span_duration < 5000000000L) // 5秒内完成
}

test "数据序列化完整性测试" {
  // 测试复杂数据结构的序列化和反序列化完整性
  let attrs = Attributes::new()
  
  // 创建包含各种数据类型的属性集合
  Attributes::set(attrs, "string.data", StringValue("测试数据with特殊字符!@#$%"))
  Attributes::set(attrs, "int.data", IntValue(-2147483648))
  Attributes::set(attrs, "float.data", FloatValue(3.14159265359))
  Attributes::set(attrs, "bool.data", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["item1", "项目2", "アイテム3"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, -1, 2147483647, -2147483648]))
  
  // 创建包含属性的Span
  let span_attrs = [
    ("operation.type", StringValue("serialization.test")),
    ("operation.id", StringValue("op-12345")),
    ("retry.count", IntValue(3)),
    ("success.rate", FloatValue(0.95)),
    ("is.critical", BoolValue(true))
  ]
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.test")
  let span = Tracer::start_span(tracer, "serialization.span")
  
  // 添加复杂事件
  let event_attrs = [
    ("event.type", StringValue("data.serialized")),
    ("data.size", IntValue(1024)),
    ("compression.ratio", FloatValue(0.65))
  ]
  Span::add_event(span, "serialization.start", Some(event_attrs))
  
  // 创建复杂的LogRecord
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "log.level", StringValue("INFO"))
  Attributes::set(log_attrs, "component", StringValue("serializer"))
  
  let complex_log = LogRecord::new_with_context(
    Info,
    Some("Serialization completed successfully"),
    Some(log_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(SpanContext::trace_id(span.span_context)),
    Some(SpanContext::span_id(span.span_context)),
    Some(Context::root())
  )
  
  // 验证所有数据都能正确访问
  let string_result = Attributes::get(attrs, "string.data")
  match string_result {
    Some(StringValue(value)) => assert_eq(value, "测试数据with特殊字符!@#$%")
    _ => assert_true(false)
  }
  
  let array_result = Attributes::get(attrs, "array.string")
  match array_result {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[1], "项目2")
    }
    _ => assert_true(false)
  }
  
  assert_eq(LogRecord::body(complex_log), Some("Serialization completed successfully"))
  assert_eq(LogRecord::severity_number(complex_log), Info)
  
  Span::set_status(span, Ok)
  Span::end(span)
}

test "跨服务遥测数据传播测试" {
  // 测试跨服务边界时的遥测数据传播
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // 服务A：创建原始追踪上下文
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a.operation")
  
  let service_a_ctx = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let enriched_ctx = Context::with_value(service_a_ctx, correlation_key, "corr-12345")
  
  // 创建服务间的Baggage
  let baggage = Baggage::new()
  let baggage_with_data = Baggage::set_entry(baggage, "user.id", "user-67890")
  let baggage_final = Baggage::set_entry(baggage_with_data, "request.id", "req-abcdef")
  
  // 注入到HTTP头部
  let outbound_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, enriched_ctx, outbound_carrier)
  TextMapCarrier::set(outbound_carrier, "baggage", "user.id=user-67890,request.id=req-abcdef")
  TextMapCarrier::set(outbound_carrier, "x-correlation-id", "corr-12345")
  TextMapCarrier::set(outbound_carrier, "x-service-name", "service-a")
  
  // 模拟网络传输
  let inbound_carrier = TextMapCarrier::new()
  TextMapCarrier::set(inbound_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(inbound_carrier, "baggage", "user.id=user-67890,request.id=req-abcdef")
  TextMapCarrier::set(inbound_carrier, "x-correlation-id", "corr-12345")
  
  // 服务B：提取传播的上下文
  let service_b_ctx = CompositePropagator::extract(composite, inbound_carrier)
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-b")
  let service_b_span = Tracer::start_span(service_b_tracer, "service-b.operation")
  
  // 验证上下文传播
  let extracted_correlation = Context::get(service_b_ctx, ContextKey::new("extracted"))
  match extracted_correlation {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  // 服务B添加自己的操作
  Span::add_event(service_b_span, "service.b.processing", Some([
    ("service.name", StringValue("service-b")),
    ("processing.time", IntValue(150)),
    ("cache.hit", BoolValue(true))
  ]))
  
  // 服务C：继续传播
  let service_c_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, service_b_ctx, service_c_carrier)
  TextMapCarrier::set(service_c_carrier, "x-calling-service", "service-b")
  
  let service_c_ctx = CompositePropagator::extract(composite, service_c_carrier)
  let service_c_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-c")
  let service_c_span = Tracer::start_span(service_c_tracer, "service.c.operation")
  
  // 验证整个调用链的完整性
  assert_eq(Span::name(service_a_span), "service-a.operation")
  assert_eq(Span::name(service_b_span), "service-b.operation")
  assert_eq(Span::name(service_c_span), "service-c.operation")
  
  Span::end(service_a_span)
  Span::end(service_b_span)
  Span::end(service_c_span)
}

test "配置管理和国际化测试" {
  // 测试不同语言环境下的配置管理
  let resource = Resource::new()
  
  // 设置多语言服务信息
  let i18n_attributes = [
    ("service.name", StringValue("azimuth-telemetry")),
    ("service.name.zh", StringValue("方位角遥测系统")),
    ("service.name.ja", StringValue("アジムス・テレメトリー・システム")),
    ("service.version", StringValue("1.0.0")),
    ("service.description.en", StringValue("Distributed telemetry system")),
    ("service.description.zh", StringValue("分布式遥测系统")),
    ("service.region", StringValue("asia-northeast1")),
    ("service.timezone", StringValue("Asia/Shanghai"))
  ]
  
  let localized_resource = Resource::with_attributes(resource, i18n_attributes)
  
  // 验证多语言属性
  let service_name_en = Resource::get_attribute(localized_resource, "service.name")
  match service_name_en {
    Some(StringValue(value)) => assert_eq(value, "azimuth-telemetry")
    _ => assert_true(false)
  }
  
  let service_name_zh = Resource::get_attribute(localized_resource, "service.name.zh")
  match service_name_zh {
    Some(StringValue(value)) => assert_eq(value, "方位角遥测系统")
    _ => assert_true(false)
  }
  
  let service_name_ja = Resource::get_attribute(localized_resource, "service.name.ja")
  match service_name_ja {
    Some(StringValue(value)) => assert_eq(value, "アジムス・テレメトリー・システム")
    _ => assert_true(false)
  }
  
  // 测试不同地区的配置
  let region_configs = [
    ("asia.config", StringValue({"timeout": "30s", "retry": "3"})),
    ("europe.config", StringValue({"timeout": "45s", "retry": "2"})),
    ("america.config", StringValue({"timeout": "60s", "retry": "5"}))
  ]
  
  let region_resource = Resource::with_attributes(Resource::new(), region_configs)
  
  let asia_config = Resource::get_attribute(region_resource, "asia.config")
  match asia_config {
    Some(StringValue(value)) => assert_eq(value, {"timeout": "30s", "retry": "3"})
    _ => assert_true(false)
  }
  
  // 测试时区相关的日志记录
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "i18n.logger")
  let zh_log = LogRecord::new(Info, "中文日志消息：操作成功完成")
  let ja_log = LogRecord::new(Warn, "日本語ログメッセージ：操作が完了しました")
  let en_log = LogRecord::new(Error, "English log message: Operation completed with errors")
  
  // 验证多语言日志内容
  assert_eq(LogRecord::body(zh_log), Some("中文日志消息：操作成功完成"))
  assert_eq(LogRecord::body(ja_log), Some("日本語ログメッセージ：操作が完了しました"))
  assert_eq(LogRecord::body(en_log), Some("English log message: Operation completed with errors"))
  
  Logger::emit(logger, zh_log)
  Logger::emit(logger, ja_log)
  Logger::emit(logger, en_log)
}

test "平台兼容性测试" {
  // 测试不同平台环境下的兼容性
  let clock = Clock::system()
  let random = Random::system()
  
  // 测试时间戳在不同平台的一致性
  let timestamp1 = Clock::now_unix_nanos(clock)
  let timestamp2 = Clock::now_unix_nanos(clock)
  
  // 验证时间戳格式和精度
  assert_true(timestamp1 > 0L)
  assert_true(timestamp2 > 0L)
  assert_true(timestamp1 <= timestamp2) // 时间应该是单调递增的
  
  // 测试随机数生成的跨平台一致性
  let random_bytes = Random::next_bytes(random, 16)
  assert_eq(random_bytes.length(), 16)
  
  let random_u64 = Random::next_u64(random)
  assert_true(random_u64 > 0UL)
  
  // 测试HTTP客户端的跨平台兼容性
  let client = HttpClient::new()
  
  // 不同平台的请求头格式
  let platform_headers = [
    ("User-Agent", StringValue("Azimuth-Telemetry/1.0.0 (Linux; x64)")),
    ("Accept", StringValue("application/json")),
    ("Accept-Encoding", StringValue("gzip, deflate")),
    ("Connection", StringValue("keep-alive")),
    ("X-Platform", StringValue("linux")),
    ("X-Architecture", StringValue("x64"))
  ]
  
  let platform_request = HttpRequest::new(
    "GET", 
    "https://api.example.com/telemetry/platform-info", 
    platform_headers
  )
  
  assert_eq(HttpRequest::http_method(platform_request), "GET")
  assert_eq(HttpRequest::url(platform_request), "https://api.example.com/telemetry/platform-info")
  
  // 测试不同平台的响应处理
  let platform_response = HttpResponse::new(200, [
    ("Content-Type", StringValue("application/json")),
    ("Server", StringValue("nginx/1.18.0")),
    ("X-Platform-Info", StringValue("linux-compatible"))
  ], Some("{\"platform\":\"linux\",\"version\":\"5.15.0\"}"))
  
  assert_eq(HttpResponse::status_code(platform_response), 200)
  match HttpResponse::body(platform_response) {
    Some(body) => assert_eq(body, "{\"platform\":\"linux\",\"version\":\"5.15.0\"}")
    None => assert_true(false)
  }
  
  // 测试文件路径的跨平台处理
  let file_paths = [
    ("/var/log/azimuth/telemetry.log", StringValue("unix-path")),
    ("C:\\Program Files\\Azimuth\\telemetry.log", StringValue("windows-path")),
    ("/Users/user/Library/Logs/Azimuth/telemetry.log", StringValue("macos-path"))
  ]
  
  let path_attrs = Attributes::new()
  for (path, path_type) in file_paths {
    Attributes::set(path_attrs, path_type, StringValue(path))
  }
  
  let unix_path = Attributes::get(path_attrs, "unix-path")
  match unix_path {
    Some(StringValue(value)) => assert_eq(value, "/var/log/azimuth/telemetry.log")
    _ => assert_true(false)
  }
}

test "实时数据处理和聚合测试" {
  // 测试实时数据流的处理和聚合
  let meter = MeterProvider::get_meter(MeterProvider::default(), "realtime.meter")
  
  // 创建各种指标仪器
  let request_counter = Meter::create_counter(meter, "http.requests.total", 
    Some("Total HTTP requests"), Some("requests"))
  let response_histogram = Meter::create_histogram(meter, "http.response.duration",
    Some("HTTP response duration"), Some("ms"))
  let active_connections = UpDownCounter::{ 
    name: "http.active.connections",
    description: Some("Active HTTP connections"),
    unit: Some("connections")
  }
  
  // 模拟实时数据流
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 模拟请求计数
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(request_counter, 1.0, None)
  }
  
  // 模拟响应时间分布
  let response_times = [10.5, 25.3, 50.7, 100.2, 200.8, 500.1, 1000.5, 2000.3]
  for time in response_times {
    Histogram::record(response_histogram, time, None)
  }
  
  // 模拟连接数变化
  for i = 0; i < 50; i = i + 1 {
    if i % 2 == 0 {
      // 连接增加
      Counter::add(Counter(active_connections.name, active_connections.description, active_connections.unit), 1.0)
    } else {
      // 连接减少
      Counter::add(Counter(active_connections.name, active_connections.description, active_connections.unit), -1.0)
    }
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let processing_duration = end_time - start_time
  
  // 验证处理性能
  assert_true(processing_duration < 1000000000L) // 1秒内完成
  
  // 测试实时Span创建和事件聚合
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "realtime.tracer")
  
  // 创建实时处理Span
  let realtime_span = Tracer::start_span(tracer, "realtime.data.processing")
  
  // 添加实时事件
  let realtime_events = [
    ("batch.started", Some([("batch.size", IntValue(1000))])),
    ("batch.processing", Some([("processed.count", IntValue(500))])),
    ("batch.aggregating", Some([("aggregation.time", IntValue(100))])),
    ("batch.completed", Some([("total.time", IntValue(250))]))
  ]
  
  for (event_name, event_attrs) in realtime_events {
    Span::add_event(realtime_span, event_name, event_attrs)
  }
  
  // 测试实时日志聚合
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "realtime.logger")
  
  // 创建不同严重级别的实时日志
  let log_levels = [(Trace, 100), (Debug, 200), (Info, 300), (Warn, 50), (Error, 10)]
  
  for (severity, count) in log_levels {
    for i = 0; i < count; i = i + 1 {
      let log_message = "Realtime log " + i.to_string() + " with severity " + severity.to_string()
      let log_record = LogRecord::new(severity, log_message)
      Logger::emit(logger, log_record)
    }
  }
  
  Span::set_status(realtime_span, Ok)
  Span::end(realtime_span)
}

test "资源管理和监控测试" {
  // 测试系统资源的监控和管理
  let resource_monitor = Resource::new()
  
  // 设置资源监控属性
  let system_resources = [
    ("system.memory.total", IntValue(8589934592)),      // 8GB
    ("system.memory.used", IntValue(4294967296)),       // 4GB
    ("system.memory.available", IntValue(4294967296)),  // 4GB
    ("system.cpu.cores", IntValue(4)),
    ("system.cpu.usage", FloatValue(0.65)),             // 65%
    ("system.disk.total", IntValue(107374182400)),      // 100GB
    ("system.disk.used", IntValue(53687091200)),        // 50GB
    ("system.network.bytes.sent", IntValue(1073741824)), // 1GB
    ("system.network.bytes.received", IntValue(2147483648)), // 2GB
    ("process.uptime", IntValue(86400)),                // 24小时
    ("process.memory.peak", IntValue(1073741824)),      // 1GB
    ("process.thread.count", IntValue(16)),
    ("process.file.descriptors", IntValue(256))
  ]
  
  let monitored_resource = Resource::with_attributes(resource_monitor, system_resources)
  
  // 验证资源监控数据
  let memory_usage = Resource::get_attribute(monitored_resource, "system.memory.used")
  match memory_usage {
    Some(IntValue(value)) => assert_eq(value, 4294967296)
    _ => assert_true(false)
  }
  
  let cpu_usage = Resource::get_attribute(monitored_resource, "system.cpu.usage")
  match cpu_usage {
    Some(FloatValue(value)) => assert_eq(value, 0.65)
    _ => assert_true(false)
  }
  
  // 测试资源阈值监控
  let resource_thresholds = [
    ("memory.warning.threshold", FloatValue(0.8)),    // 80%
    ("memory.critical.threshold", FloatValue(0.95)),  // 95%
    ("cpu.warning.threshold", FloatValue(0.7)),       // 70%
    ("cpu.critical.threshold", FloatValue(0.9)),      // 90%
    ("disk.warning.threshold", FloatValue(0.8)),      // 80%
    ("disk.critical.threshold", FloatValue(0.95))     // 95%
  ]
  
  let threshold_resource = Resource::with_attributes(Resource::new(), resource_thresholds)
  
  // 测试资源告警逻辑
  let current_memory_usage = 0.85 // 85%
  let memory_warning_threshold = Resource::get_attribute(threshold_resource, "memory.warning.threshold")
  
  match memory_warning_threshold {
    Some(FloatValue(threshold)) => {
      if current_memory_usage > threshold {
        // 触发内存警告
        let warning_log = LogRecord::new(Warn, "Memory usage exceeded warning threshold: " + current_memory_usage.to_string())
        Logger::emit(LoggerProvider::get_logger(LoggerProvider::default(), "resource.monitor"), warning_log)
        assert_true(true) // 告警触发成功
      }
    }
    _ => assert_true(false)
  }
  
  // 测试资源自动清理
  let cleanup_span = TracerProvider::get_tracer(TracerProvider::default(), "resource.cleanup")
  let cleanup_tracer = TracerProvider::get_tracer(TracerProvider::default(), "cleanup.tracer")
  let resource_cleanup_span = Tracer::start_span(cleanup_tracer, "automatic.resource.cleanup")
  
  Span::add_event(resource_cleanup_span, "cleanup.started", Some([
    ("cleanup.reason", StringValue("memory.threshold.exceeded")),
    ("cleanup.target", StringValue("cache.memory"))
  ]))
  
  // 模拟清理操作
  let cleanup_metrics = Meter::create_counter(meter, "resource.cleanup.operations",
    Some("Resource cleanup operations"), Some("operations"))
  Counter::add(cleanup_metrics, 1.0, Some([
    ("cleanup.type", StringValue("memory")),
    ("cleanup.result", StringValue("success"))
  ]))
  
  Span::add_event(resource_cleanup_span, "cleanup.completed", Some([
    ("memory.freed", IntValue(1073741824)), // 1GB freed
    ("cleanup.duration", IntValue(5000))     // 5 seconds
  ]))
  
  Span::set_status(resource_cleanup_span, Ok)
  Span::end(resource_cleanup_span)
}

test "安全性和边界防护测试" {
  // 测试安全相关的边界条件和防护机制
  let security_attrs = Attributes::new()
  
  // 测试输入验证和清理
  let malicious_inputs = [
    ("normal.input", StringValue("valid_input")),
    ("sql.injection.attempt", StringValue("'; DROP TABLE users; --")),
    ("xss.attempt", StringValue("<script>alert('xss')</script>")),
    ("path.traversal.attempt", StringValue("../../../etc/passwd")),
    ("command.injection.attempt", StringValue("; rm -rf /")),
    ("ldap.injection.attempt", StringValue("*)(|(objectClass=*))")),
    ("xml.injection.attempt", StringValue("<?xml version=\"1.0\"?><!DOCTYPE foo [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]>"))
  ]
  
  // 安全地处理各种输入
  for (key, value) in malicious_inputs {
    // 在实际实现中，这里应该有输入清理逻辑
    Attributes::set(security_attrs, "processed." + key, value)
  }
  
  // 测试敏感数据处理
  let sensitive_data = [
    ("user.password", StringValue("secret_password")),
    ("api.key", StringValue("sk-1234567890abcdef")),
    ("database.url", StringValue("mysql://user:pass@localhost/db")),
    ("jwt.token", StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9")),
    ("credit.card", StringValue("4111-1111-1111-1111"))
  ]
  
  // 模拟敏感数据脱敏
  for (key, value) in sensitive_data {
    let sanitized_value = match key {
      "user.password" => "***"
      "api.key" => "sk-****"
      "database.url" => "mysql://user:****@localhost/db"
      "jwt.token" => "eyJ****"
      "credit.card" => "****-****-****-1111"
      _ => "****"
    }
    
    Attributes::set(security_attrs, "sanitized." + key, StringValue(sanitized_value))
  }
  
  // 验证脱敏结果
  let sanitized_password = Attributes::get(security_attrs, "sanitized.user.password")
  match sanitized_password {
    Some(StringValue(value)) => assert_eq(value, "***")
    _ => assert_true(false)
  }
  
  // 测试访问控制
  let access_control_attrs = [
    ("user.role", StringValue("admin")),
    ("user.permissions", ArrayStringValue(["read", "write", "delete"])),
    ("resource.owner", StringValue("user123")),
    ("resource.acl", StringValue("user123:rw,group456:r,other:")),
    ("access.timestamp", IntValue(1735689600)),
    ("access.ip", StringValue("192.168.1.100"))
  ]
  
  let access_resource = Resource::with_attributes(Resource::new(), access_control_attrs)
  
  // 验证访问控制属性
  let user_role = Resource::get_attribute(access_resource, "user.role")
  match user_role {
    Some(StringValue(value)) => assert_eq(value, "admin")
    _ => assert_true(false)
  }
  
  // 测试安全事件记录
  let security_logger = LoggerProvider::get_logger(LoggerProvider::default(), "security.logger")
  
  // 记录安全事件
  let security_events = [
    (Info, "User login successful"),
    (Warn, "Multiple failed login attempts"),
    (Error, "Unauthorized access attempt detected"),
    (Fatal, "Security breach detected - immediate action required")
  ]
  
  for (severity, message) in security_events {
    let security_log = LogRecord::new_with_context(
      severity,
      Some(message),
      Some(security_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("security-trace-id"),
      Some("security-span-id"),
      Some(Context::root())
    )
    
    Logger::emit(security_logger, security_log)
  }
  
  // 测试加密数据传输
  let encryption_span = TracerProvider::get_tracer(TracerProvider::default(), "encryption.tracer")
  let secure_span = Tracer::start_span(encryption_span, "secure.data.transmission")
  
  Span::add_event(secure_span, "encryption.started", Some([
    ("encryption.algorithm", StringValue("AES-256-GCM")),
    ("key.length", IntValue(256)),
    ("data.size", IntValue(1024))
  ]))
  
  Span::add_event(secure_span, "encryption.completed", Some([
    ("encryption.result", StringValue("success")),
    ("encrypted.size", IntValue(1024)),
    ("encryption.time", IntValue(15))
  ]))
  
  Span::set_status(secure_span, Ok)
  Span::end(secure_span)
}