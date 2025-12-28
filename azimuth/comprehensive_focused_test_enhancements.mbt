// Comprehensive Focused Test Enhancements for Azimuth Telemetry System
// 专注于仪器类型、Baggage操作、Span生命周期、属性值类型、指标仪器、日志上下文、资源合并和传播器高级功能

// 测试1: 仪器类型详细操作和转换验证
test "仪器类型详细操作和转换验证测试" {
  // 创建各种仪器类型并测试其属性
  let counter_instrument = Counter("http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let histogram_instrument = Histogram("request.duration", Some("Request duration histogram"), Some("ms"))
  let updown_counter_instrument = UpDownCounter("active.connections", Some("Currently active connections"), Some("connections"))
  let gauge_instrument = Gauge("memory.usage", Some("Current memory usage"), Some("bytes"))
  
  // 测试仪器名称获取
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::name(histogram_instrument), "request.duration")
  assert_eq(Instrument::name(updown_counter_instrument), "active.connections")
  assert_eq(Instrument::name(gauge_instrument), "memory.usage")
  
  // 测试仪器描述获取
  assert_true(Instrument::description(counter_instrument) is Some)
  match Instrument::description(counter_instrument) {
    Some(desc) => assert_eq(desc, "Total HTTP requests")
    _ => assert_true(false)
  }
  
  assert_true(Instrument::description(histogram_instrument) is Some)
  match Instrument::description(histogram_instrument) {
    Some(desc) => assert_eq(desc, "Request duration histogram")
    _ => assert_true(false)
  }
  
  // 测试仪器单位获取
  assert_true(Instrument::unit(counter_instrument) is Some)
  match Instrument::unit(counter_instrument) {
    Some(unit) => assert_eq(unit, "requests")
    _ => assert_true(false)
  }
  
  assert_true(Instrument::unit(gauge_instrument) is Some)
  match Instrument::unit(gauge_instrument) {
    Some(unit) => assert_eq(unit, "bytes")
    _ => assert_true(false)
  }
  
  // 测试None描述和单位的仪器
  let no_desc_counter = Counter("simple.counter", None, None)
  assert_eq(Instrument::name(no_desc_counter), "simple.counter")
  assert_eq(Instrument::description(no_desc_counter), None)
  assert_eq(Instrument::unit(no_desc_counter), None)
  
  // 测试空字符串描述和单位
  let empty_desc_histogram = Histogram("empty.histogram", Some(""), Some(""))
  assert_true(Instrument::description(empty_desc_histogram) is Some)
  match Instrument::description(empty_desc_histogram) {
    Some(desc) => assert_eq(desc, "")
    _ => assert_true(false)
  }
}

// 测试2: Baggage操作复杂场景和边界条件
test "Baggage操作复杂场景和边界条件测试" {
  // 创建基础Baggage并测试多值操作
  let base_baggage = Baggage::new()
  
  // 测试连续设置多个条目
  let baggage1 = Baggage::set_entry(base_baggage, "user.id", "user-12345")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session-67890")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "request-abcde")
  
  // 验证所有条目都能正确获取
  let user_id = Baggage::get_entry(baggage3, "user.id")
  let session_id = Baggage::get_entry(baggage3, "session.id")
  let request_id = Baggage::get_entry(baggage3, "request.id")
  
  assert_true(user_id is Some)
  match user_id {
    Some(value) => assert_eq(value, "user-12345")
    _ => assert_true(false)
  }
  
  assert_true(session_id is Some)
  match session_id {
    Some(value) => assert_eq(value, "session-67890")
    _ => assert_true(false)
  }
  
  assert_true(request_id is Some)
  match request_id {
    Some(value) => assert_eq(value, "request-abcde")
    _ => assert_true(false)
  }
  
  // 测试获取不存在的条目
  let missing_entry = Baggage::get_entry(baggage3, "nonexistent.key")
  assert_eq(missing_entry, None)
  
  // 测试空键和空值处理
  let empty_key_baggage = Baggage::set_entry(baggage3, "", "empty.key.value")
  let empty_value_baggage = Baggage::set_entry(empty_key_baggage, "empty.value.key", "")
  
  // 测试特殊字符和Unicode键值
  let unicode_baggage = Baggage::set_entry(empty_value_baggage, "用户.标识", "用户-12345")
  let special_baggage = Baggage::set_entry(unicode_baggage, "special@key#with$symbols", "special-value-!@#$%^&*()")
  
  // 验证特殊字符键值获取
  let unicode_value = Baggage::get_entry(special_baggage, "用户.标识")
  assert_true(unicode_value is Some)
  match unicode_value {
    Some(value) => assert_eq(value, "用户-12345")
    _ => assert_true(false)
  }
  
  // 测试条目移除操作
  let removed_baggage = Baggage::remove_entry(special_baggage, "user.id")
  let removed_user_id = Baggage::get_entry(removed_baggage, "user.id")
  assert_eq(removed_user_id, None)
  
  // 验证其他条目仍然存在
  let remaining_session_id = Baggage::get_entry(removed_baggage, "session.id")
  assert_true(remaining_session_id is Some)
  match remaining_session_id {
    Some(value) => assert_eq(value, "session-67890")
    _ => assert_true(false)
  }
  
  // 测试移除不存在的条目
  let remove_nonexistent = Baggage::remove_entry(removed_baggage, "nonexistent.key")
  // 应该不影响其他条目
  let verify_session = Baggage::get_entry(remove_nonexistent, "session.id")
  assert_true(verify_session is Some)
}

// 测试3: Span生命周期详细测试和状态管理
test "Span生命周期详细测试和状态管理测试" {
  // 创建Tracer和多个Span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.tracer", Some("1.0.0"))
  
  // 测试不同类型的Span创建
  let internal_span = Tracer::start_span(tracer, "internal.operation")
  let server_span = Tracer::start_span(tracer, "server.request")
  let client_span = Tracer::start_span(tracer, "client.request")
  
  // 验证Span基本属性
  assert_eq(Span::name(internal_span), "internal.operation")
  assert_eq(Span::name(server_span), "server.request")
  assert_eq(Span::name(client_span), "client.request")
  
  // 验证Span初始状态
  assert_true(Span::is_recording(internal_span))
  assert_true(Span::is_recording(server_span))
  assert_true(Span::is_recording(client_span))
  
  // 测试Span状态设置和获取
  Span::set_status(internal_span, Ok, Some("Operation completed successfully"))
  Span::set_status(server_span, Error, Some("Server error occurred"))
  Span::set_status(client_span, Unset)
  
  // 测试Span事件添加
  Span::add_event(internal_span, "operation.started", Some([
    ("operation.type", StringValue("internal")),
    ("timestamp", IntValue(1735689600))
  ]))
  
  Span::add_event(server_span, "request.received", Some([
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("/api/users")),
    ("user.agent", StringValue("Azimuth-Client/1.0"))
  ]))
  
  Span::add_event(client_span, "response.received", Some([
    ("http.status_code", IntValue(200)),
    ("response.size", IntValue(1024))
  ]))
  
  // 测试Span上下文操作
  let internal_ctx = Span::span_context(internal_span)
  let server_ctx = Span::span_context(server_span)
  let client_ctx = Span::span_context(client_span)
  
  // 验证Span上下文有效性
  assert_true(SpanContext::is_valid(internal_ctx))
  assert_true(SpanContext::is_valid(server_ctx))
  assert_true(SpanContext::is_valid(client_ctx))
  
  // 验证Span上下文采样状态
  assert_true(SpanContext::is_sampled(internal_ctx))
  assert_true(SpanContext::is_sampled(server_ctx))
  assert_true(SpanContext::is_sampled(client_ctx))
  
  // 测试Span结束操作
  Span::end(internal_span)
  Span::end(server_span)
  Span::end(client_span)
  
  // 验证Tracer的仪器化范围
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "lifecycle.tracer")
  assert_true(scope.version is Some)
  match scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // 测试带属性的Span创建
  let span_with_attrs = Tracer::start_span(tracer, "span.with.attributes", Some([
    ("service.name", StringValue("azimuth.service")),
    ("operation.type", StringValue("custom.operation"))
  ]))
  
  assert_eq(Span::name(span_with_attrs), "span.with.attributes")
  Span::set_status(span_with_attrs, Ok, Some("Custom operation completed"))
  Span::add_event(span_with_attrs, "custom.event", None)
  Span::end(span_with_attrs)
}

// 测试4: 属性值类型深度操作和验证
test "属性值类型深度操作和验证测试" {
  // 创建包含各种属性值类型的Attributes
  let attrs = Attributes::new()
  
  // 设置各种类型的属性值
  let string_values = [
    ("simple.string", StringValue("simple.value")),
    ("empty.string", StringValue("")),
    ("unicode.string", StringValue("测试中文字符串 🚀")),
    ("special.chars", StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")),
    ("json.string", StringValue("{\"key\": \"value\", \"number\": 42}"))
  ]
  
  let int_values = [
    ("positive.int", IntValue(42)),
    ("zero.int", IntValue(0)),
    ("negative.int", IntValue(-42)),
    ("max.int", IntValue(2147483647)),
    ("min.int", IntValue(-2147483648))
  ]
  
  let float_values = [
    ("positive.float", FloatValue(3.14159)),
    ("zero.float", FloatValue(0.0)),
    ("negative.float", FloatValue(-3.14159)),
    ("large.float", FloatValue(1.23456789e+10)),
    ("small.float", FloatValue(1.23456789e-10))
  ]
  
  let bool_values = [
    ("true.bool", BoolValue(true)),
    ("false.bool", BoolValue(false))
  ]
  
  let array_string_values = [
    ("empty.string.array", ArrayStringValue([])),
    ("single.string.array", ArrayStringValue(["single"])),
    ("multi.string.array", ArrayStringValue(["item1", "item2", "item3"]))
  ]
  
  let array_int_values = [
    ("empty.int.array", ArrayIntValue([])),
    ("single.int.array", ArrayIntValue([42])),
    ("multi.int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  // 批量设置字符串属性
  for (key, value) in string_values {
    Attributes::set(attrs, key, value)
  }
  
  // 批量设置整数属性
  for (key, value) in int_values {
    Attributes::set(attrs, key, value)
  }
  
  // 批量设置浮点属性
  for (key, value) in float_values {
    Attributes::set(attrs, key, value)
  }
  
  // 批量设置布尔属性
  for (key, value) in bool_values {
    Attributes::set(attrs, key, value)
  }
  
  // 批量设置字符串数组属性
  for (key, value) in array_string_values {
    Attributes::set(attrs, key, value)
  }
  
  // 批量设置整数数组属性
  for (key, value) in array_int_values {
    Attributes::set(attrs, key, value)
  }
  
  // 验证字符串属性获取
  let simple_string = Attributes::get(attrs, "simple.string")
  assert_true(simple_string is Some)
  match simple_string {
    Some(StringValue(value)) => assert_eq(value, "simple.value")
    _ => assert_true(false)
  }
  
  let unicode_string = Attributes::get(attrs, "unicode.string")
  assert_true(unicode_string is Some)
  match unicode_string {
    Some(StringValue(value)) => assert_eq(value, "测试中文字符串 🚀")
    _ => assert_true(false)
  }
  
  // 验证整数属性获取
  let positive_int = Attributes::get(attrs, "positive.int")
  assert_true(positive_int is Some)
  match positive_int {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let max_int = Attributes::get(attrs, "max.int")
  assert_true(max_int is Some)
  match max_int {
    Some(IntValue(value)) => assert_eq(value, 2147483647)
    _ => assert_true(false)
  }
  
  // 验证浮点属性获取
  let pi_float = Attributes::get(attrs, "positive.float")
  assert_true(pi_float is Some)
  match pi_float {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false)
  }
  
  // 验证布尔属性获取
  let true_bool = Attributes::get(attrs, "true.bool")
  assert_true(true_bool is Some)
  match true_bool {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
  
  let false_bool = Attributes::get(attrs, "false.bool")
  assert_true(false_bool is Some)
  match false_bool {
    Some(BoolValue(value)) => assert_false(value)
    _ => assert_true(false)
  }
  
  // 测试不存在的属性
  let nonexistent_attr = Attributes::get(attrs, "nonexistent.attribute")
  assert_eq(nonexistent_attr, None)
}

// 测试5: 指标仪器具体操作和性能测试
test "指标仪器具体操作和性能测试测试" {
  // 创建MeterProvider和Meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance.test.meter", Some("1.0.0"))
  
  // 创建各种指标仪器
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_histogram = Meter::create_histogram(meter, "http.response.duration", Some("HTTP response duration"), Some("ms"))
  let active_connections = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  let memory_usage = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // 测试Counter操作
  assert_eq(request_counter.name, "http.requests.total")
  assert_true(request_counter.description is Some)
  match request_counter.description {
    Some(desc) => assert_eq(desc, "Total HTTP requests")
    _ => assert_true(false)
  }
  
  // 执行多次Counter操作
  Counter::add(request_counter, 1.0)
  Counter::add(request_counter, 5.0)
  Counter::add(request_counter, 10.5)
  Counter::add(request_counter, 0.5)
  
  // 测试Histogram操作
  assert_eq(response_histogram.name, "http.response.duration")
  assert_true(response_histogram.description is Some)
  match response_histogram.description {
    Some(desc) => assert_eq(desc, "HTTP response duration")
    _ => assert_true(false)
  }
  
  // 执行多次Histogram记录
  Histogram::record(response_histogram, 100.0)
  Histogram::record(response_histogram, 250.5)
  Histogram::record(response_histogram, 50.25)
  Histogram::record(response_histogram, 500.75)
  
  // 测试UpDownCounter操作
  assert_eq(active_connections.name, "active.connections")
  Counter::add(active_connections, 10.0)  // 增加10个连接
  Counter::add(active_connections, -3.0)  // 减少3个连接
  Counter::add(active_connections, 5.0)   // 增加5个连接
  
  // 测试Gauge操作
  assert_eq(memory_usage.name, "memory.usage")
  Counter::add(memory_usage, 1024.0 * 1024.0 * 100.0)  // 100MB
  
  // 测试带属性的指标操作
  let attrs = Attributes::new()
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  
  Counter::add(request_counter, 1.0, Some(attrs))
  Histogram::record(response_histogram, 150.0, Some(attrs))
  
  // 测试不同类型的属性值
  let string_attrs = Attributes::new()
  Attributes::set(string_attrs, "service.name", StringValue("azimuth.service"))
  
  let int_attrs = Attributes::new()
  Attributes::set(int_attrs, "port.number", IntValue(8080))
  
  let bool_attrs = Attributes::new()
  Attributes::set(bool_attrs, "debug.mode", BoolValue(true))
  
  // 使用不同类型的属性执行指标操作
  Counter::add(request_counter, 2.0, Some(string_attrs))
  Histogram::record(response_histogram, 200.0, Some(int_attrs))
  Counter::add(active_connections, 1.0, Some(bool_attrs))
  
  // 测试仪器转换为Instrument类型
  let counter_instrument = Histogram::as_instrument(response_histogram)
  assert_eq(Instrument::name(counter_instrument), "http.response.duration")
  assert_true(Instrument::description(counter_instrument) is Some)
  match Instrument::description(counter_instrument) {
    Some(desc) => assert_eq(desc, "HTTP response duration")
    _ => assert_true(false)
  }
  
  // 验证Meter的仪器化范围
  assert_eq(meter.scope.name, "performance.test.meter")
  assert_true(meter.scope.version is Some)
  match meter.scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
}

// 测试6: 日志记录上下文关联和追踪测试
test "日志记录上下文关联和追踪测试" {
  // 创建LoggerProvider和Logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "contextual.logger", Some("1.0.0"))
  
  // 创建Span上下文用于日志关联
  let trace_id = "trace-12345-abcde-67890-fghij"
  let span_id = "span-abcdef-123456"
  let span_context = SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  // 创建应用上下文
  let app_ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let enriched_ctx = Context::with_value(app_ctx, user_key, "user-12345")
  let final_ctx = Context::with_value(enriched_ctx, session_key, "session-67890")
  
  // 创建带完整上下文的日志记录
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "service.name", StringValue("azimuth.service"))
  Attributes::set(log_attrs, "operation.name", StringValue("user.authentication"))
  Attributes::set(log_attrs, "user.role", StringValue("admin"))
  Attributes::set(log_attrs, "request.id", StringValue("req-abcdef-123456"))
  
  let contextual_log = LogRecord::new_with_context(
    Info,
    Some("User authentication completed successfully"),
    Some(log_attrs),
    Some(1735689600000000000L),  // 2025-01-01 timestamp
    Some(1735689600000000100L),  // observed timestamp
    Some(trace_id),
    Some(span_id),
    Some(final_ctx)
  )
  
  // 验证日志记录的各个字段
  assert_eq(LogRecord::severity_number(contextual_log), Info)
  assert_true(LogRecord::body(contextual_log) is Some)
  match LogRecord::body(contextual_log) {
    Some(body) => assert_eq(body, "User authentication completed successfully")
    _ => assert_true(false)
  }
  
  assert_true(LogRecord::trace_id(contextual_log) is Some)
  match LogRecord::trace_id(contextual_log) {
    Some(log_trace_id) => assert_eq(log_trace_id, trace_id)
    _ => assert_true(false)
  }
  
  assert_true(LogRecord::span_id(contextual_log) is Some)
  match LogRecord::span_id(contextual_log) {
    Some(log_span_id) => assert_eq(log_span_id, span_id)
    _ => assert_true(false)
  }
  
  // 测试不同严重性级别的日志记录
  let severity_levels = [
    (Trace, "Detailed trace information for debugging"),
    (Debug, "Debug information about internal state"),
    (Info, "Normal operational information"),
    (Warn, "Warning condition that might need attention"),
    (Error, "Error condition that occurred"),
    (Fatal, "Critical error that caused system failure")
  ]
  
  for (severity, message) in severity_levels {
    let severity_log = LogRecord::new_with_context(
      severity,
      Some(message),
      Some(log_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some(trace_id),
      Some(span_id),
      Some(final_ctx)
    )
    
    assert_eq(LogRecord::severity_number(severity_log), severity)
    assert_true(LogRecord::body(severity_log) is Some)
    match LogRecord::body(severity_log) {
      Some(body) => assert_eq(body, message)
      _ => assert_true(false)
    }
    
    Logger::emit(logger, severity_log)
  }
  
  // 测试错误日志记录
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.type", StringValue("ValidationError"))
  Attributes::set(error_attrs, "error.code", StringValue("ERR_001"))
  Attributes::set(error_attrs, "error.message", StringValue("Invalid user credentials"))
  Attributes::set(error_attrs, "retry.count", IntValue(3))
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Authentication failed due to invalid credentials"),
    Some(error_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(final_ctx)
  )
  
  Logger::emit(logger, error_log)
  
  // 测试最简日志记录
  let simple_log = LogRecord::new(Info, "Simple log message")
  assert_eq(LogRecord::severity_number(simple_log), Info)
  assert_true(LogRecord::body(simple_log) is Some)
  match LogRecord::body(simple_log) {
    Some(body) => assert_eq(body, "Simple log message")
    _ => assert_true(false)
  }
  assert_eq(LogRecord::trace_id(simple_log), None)
  assert_eq(LogRecord::span_id(simple_log), None)
  
  // 验证Logger的仪器化范围
  assert_eq(logger.scope.name, "contextual.logger")
  assert_true(logger.scope.version is Some)
  match logger.scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // 发射所有日志记录
  Logger::emit(logger, contextual_log)
  Logger::emit(logger, simple_log)
}

// 测试7: 资源合并复杂场景和优先级处理测试
test "资源合并复杂场景和优先级处理测试" {
  // 创建多个具有不同属性的Resource
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("azimuth.base.service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development")),
    ("host.name", StringValue("localhost"))
  ])
  
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("azimuth.override.service")),  // 覆盖base中的service.name
    ("deployment.environment", StringValue("production")),      // 覆盖base中的deployment.environment
    ("region", StringValue("us-west-2")),
    ("availability.zone", StringValue("us-west-2a"))
  ])
  
  let additional_resource = Resource::with_attributes(Resource::new(), [
    ("service.instance.id", StringValue("instance-12345")),
    ("process.pid", IntValue(12345)),
    ("process.command", StringValue("azimuth-service")),
    ("process.runtime.version", StringValue("moonbit-1.0.0"))
  ])
  
  // 测试基础资源属性获取
  let base_service_name = Resource::get_attribute(base_resource, "service.name")
  assert_true(base_service_name is Some)
  match base_service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth.base.service")
    _ => assert_true(false)
  }
  
  let base_env = Resource::get_attribute(base_resource, "deployment.environment")
  assert_true(base_env is Some)
  match base_env {
    Some(StringValue(env)) => assert_eq(env, "development")
    _ => assert_true(false)
  }
  
  // 测试覆盖资源属性获取
  let override_service_name = Resource::get_attribute(override_resource, "service.name")
  assert_true(override_service_name is Some)
  match override_service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth.override.service")
    _ => assert_true(false)
  }
  
  // 测试资源合并 - base + override
  let merged_base_override = Resource::merge(base_resource, override_resource)
  let merged_service_name = Resource::get_attribute(merged_base_override, "service.name")
  assert_true(merged_service_name is Some)
  match merged_service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth.override.service")  // 应该是override的值
    _ => assert_true(false)
  }
  
  let merged_env = Resource::get_attribute(merged_base_override, "deployment.environment")
  assert_true(merged_env is Some)
  match merged_env {
    Some(StringValue(env)) => assert_eq(env, "production")  // 应该是override的值
    _ => assert_true(false)
  }
  
  // 测试三重资源合并
  let final_merged = Resource::merge(merged_base_override, additional_resource)
  
  // 验证来自不同资源的属性
  let final_service_name = Resource::get_attribute(final_merged, "service.name")
  assert_true(final_service_name is Some)
  match final_service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth.override.service")
    _ => assert_true(false)
  }
  
  let final_instance_id = Resource::get_attribute(final_merged, "service.instance.id")
  assert_true(final_instance_id is Some)
  match final_instance_id {
    Some(StringValue(id)) => assert_eq(id, "instance-12345")
    _ => assert_true(false)
  }
  
  let final_pid = Resource::get_attribute(final_merged, "process.pid")
  assert_true(final_pid is Some)
  match final_pid {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
  
  // 测试空资源合并
  let empty_resource = Resource::new()
  let merged_with_empty = Resource::merge(empty_resource, base_resource)
  let empty_merged_service = Resource::get_attribute(merged_with_empty, "service.name")
  assert_true(empty_merged_service is Some)
  match empty_merged_service {
    Some(StringValue(name)) => assert_eq(name, "azimuth.base.service")
    _ => assert_true(false)
  }
  
  // 测试复杂属性类型合并
  let complex_base = Resource::with_attributes(Resource::new(), [
    ("array.attribute", ArrayStringValue(["item1", "item2"])),
    ("int.attribute", IntValue(42)),
    ("float.attribute", FloatValue(3.14159)),
    ("bool.attribute", BoolValue(true))
  ])
  
  let complex_override = Resource::with_attributes(Resource::new(), [
    ("array.attribute", ArrayStringValue(["item3", "item4"])),
    ("int.attribute", IntValue(100)),
    ("new.attribute", StringValue("new.value"))
  ])
  
  let complex_merged = Resource::merge(complex_base, complex_override)
  let merged_array = Resource::get_attribute(complex_merged, "array.attribute")
  assert_true(merged_array is Some)
  match merged_array {
    Some(ArrayStringValue(arr)) => assert_eq(arr, ["item3", "item4"])  // 应该是override的值
    _ => assert_true(false)
  }
  
  let merged_int = Resource::get_attribute(complex_merged, "int.attribute")
  assert_true(merged_int is Some)
  match merged_int {
    Some(IntValue(value)) => assert_eq(value, 100)  // 应该是override的值
    _ => assert_true(false)
  }
  
  // 验证不存在的属性
  let nonexistent = Resource::get_attribute(final_merged, "nonexistent.attribute")
  assert_eq(nonexistent, None)
}

// 测试8: 复合传播器高级功能和错误处理测试
test "复合传播器高级功能和错误处理测试" {
  // 创建多个传播器
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let composite_propagator = CompositePropagator::new([trace_propagator1, trace_propagator2])
  
  // 创建丰富的上下文
  let base_ctx = Context::root()
  let trace_key = ContextKey::new("trace.id")
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let request_key = ContextKey::new("request.id")
  
  let enriched_ctx1 = Context::with_value(base_ctx, trace_key, "trace-12345-abcde")
  let enriched_ctx2 = Context::with_value(enriched_ctx1, user_key, "user-67890")
  let enriched_ctx3 = Context::with_value(enriched_ctx2, session_key, "session-fghij")
  let final_ctx = Context::with_value(enriched_ctx3, request_key, "request-klmno")
  
  // 创建TextMapCarrier并设置多个头部
  let carrier = TextMapCarrier::new()
  
  // 测试注入操作
  CompositePropagator::inject(composite_propagator, final_ctx, carrier)
  
  // 验证注入的头部
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_traceparent is Some)
  match injected_traceparent {
    Some(value) => assert_eq(value, "00-test-trace-id-test-span-id-01")
    _ => assert_true(false)
  }
  
  // 测试提取操作
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  
  assert_true(extracted_value is Some)
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_true(false)
  }
  
  // 测试带有预设置头部的载体
  let preset_carrier = TextMapCarrier::new()
  TextMapCarrier::set(preset_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(preset_carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(preset_carrier, "baggage", "user.id=user-12345,session.id=session-67890")
  TextMapCarrier::set(preset_carrier, "x-custom-header", "custom-value")
  
  // 从预设载体提取上下文
  let preset_extracted_ctx = CompositePropagator::extract(composite_propagator, preset_carrier)
  let preset_extracted_value = Context::get(preset_extracted_ctx, extracted_key)
  
  assert_true(preset_extracted_value is Some)
  match preset_extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_true(false)
  }
  
  // 测试空载体处理
  let empty_carrier = TextMapCarrier::new()
  let empty_extracted_ctx = CompositePropagator::extract(composite_propagator, empty_carrier)
  let empty_extracted_value = Context::get(empty_extracted_ctx, extracted_key)
  
  assert_true(empty_extracted_value is Some)
  match empty_extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_true(false)
  }
  
  // 测试单个传播器的复合传播器
  let single_propagator = CompositePropagator::new([trace_propagator1])
  let single_carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(single_propagator, final_ctx, single_carrier)
  let single_injected = TextMapCarrier::get(single_carrier, "traceparent")
  assert_true(single_injected is Some)
  
  let single_extracted = CompositePropagator::extract(single_propagator, single_carrier)
  let single_extracted_value = Context::get(single_extracted, extracted_key)
  assert_true(single_extracted_value is Some)
  
  // 测试多次注入和提取操作
  let multi_carrier = TextMapCarrier::new()
  
  // 第一次注入
  CompositePropagator::inject(composite_propagator, final_ctx, multi_carrier)
  let first_injection = TextMapCarrier::get(multi_carrier, "traceparent")
  assert_true(first_injection is Some)
  
  // 第二次注入（应该覆盖第一次）
  CompositePropagator::inject(composite_propagator, base_ctx, multi_carrier)
  let second_injection = TextMapCarrier::get(multi_carrier, "traceparent")
  assert_true(second_injection is Some)
  
  // 多次提取
  let extracted1 = CompositePropagator::extract(composite_propagator, multi_carrier)
  let extracted2 = CompositePropagator::extract(composite_propagator, multi_carrier)
  
  let value1 = Context::get(extracted1, extracted_key)
  let value2 = Context::get(extracted2, extracted_key)
  
  assert_true(value1 is Some)
  assert_true(value2 is Some)
  
  // 测试上下文值的保持
  let original_trace_value = Context::get(final_ctx, trace_key)
  let original_user_value = Context::get(final_ctx, user_key)
  let original_session_value = Context::get(final_ctx, session_key)
  let original_request_value = Context::get(final_ctx, request_key)
  
  assert_true(original_trace_value is Some)
  match original_trace_value {
    Some(value) => assert_eq(value, "trace-12345-abcde")
    _ => assert_true(false)
  }
  
  assert_true(original_user_value is Some)
  match original_user_value {
    Some(value) => assert_eq(value, "user-67890")
    _ => assert_true(false)
  }
  
  assert_true(original_session_value is Some)
  match original_session_value {
    Some(value) => assert_eq(value, "session-fghij")
    _ => assert_true(false)
  }
  
  assert_true(original_request_value is Some)
  match original_request_value {
    Some(value) => assert_eq(value, "request-klmno")
    _ => assert_true(false)
  }
}