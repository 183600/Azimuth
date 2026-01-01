// Focused MoonBit Test Suite for Azimuth Telemetry System
// This file contains 10 focused test cases covering key functionality areas

// Test 1: Span lifecycle management
pub test "span生命周期管理测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "lifecycle-test")
  
  // 创建Span并验证初始状态
  let span = azimuth::Tracer::start_span(tracer, "test-operation")
  assert_eq(azimuth::Span::name(span), "test-operation")
  assert_true(azimuth::Span::is_recording(span))
  
  // 添加事件和属性
  let event_attrs = [("event.type", azimuth::StringValue("test"))]
  azimuth::Span::add_event(span, "test-event", Some(event_attrs))
  
  let span_attrs = [("operation.type", azimuth::StringValue("test-operation"))]
  azimuth::Span::set_attributes(span, span_attrs)
  
  // 设置状态
  azimuth::Span::set_status(span, azimuth::Ok)
  
  // 结束Span
  azimuth::Span::end(span)
  assert_false(azimuth::Span::is_recording(span))
}

// Test 2: Metrics aggregation operations
pub test "度量聚合操作测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建各种度量类型
  let counter = azimuth::Meter::create_counter(meter, "test.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "test.histogram")
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "test.updown_counter")
  
  // 执行多次操作
  for i in 0..10 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double())
    azimuth::UpDownCounter::add(updown_counter, 1.0)
  }
  
  // 测试UpDownCounter的递减操作
  for i in 0..5 {
    azimuth::UpDownCounter::add(updown_counter, -1.0)
  }
  
  // 验证度量属性
  assert_eq(counter.name, "test.counter")
  assert_eq(histogram.name, "test.histogram")
  assert_eq(updown_counter.name, "test.updown_counter")
}

// Test 3: Context propagation with baggage
pub test "上下文传播和baggage测试" {
  let root_ctx = azimuth::Context::root()
  
  // 设置多个上下文值
  let user_key = azimuth::ContextKey::new("user.id")
  let session_key = azimuth::ContextKey::new("session.id")
  let trace_key = azimuth::ContextKey::new("trace.id")
  
  let ctx1 = azimuth::Context::with_value(root_ctx, user_key, "user-123")
  let ctx2 = azimuth::Context::with_value(ctx1, session_key, "session-456")
  let ctx3 = azimuth::Context::with_value(ctx2, trace_key, "trace-789")
  
  // 验证上下文传播
  assert_eq(azimuth::Context::get(ctx3, user_key), Some("user-123"))
  assert_eq(azimuth::Context::get(ctx3, session_key), Some("session-456"))
  assert_eq(azimuth::Context::get(ctx3, trace_key), Some("trace-789"))
  
  // 测试Baggage操作
  let baggage = azimuth::Baggage::new()
  let baggage1 = azimuth::Baggage::set_entry(baggage, "request.id", "req-123")
  let baggage2 = azimuth::Baggage::set_entry(baggage1, "correlation.id", "corr-456")
  
  assert_eq(azimuth::Baggage::get_entry(baggage2, "request.id"), Some("req-123"))
  assert_eq(azimuth::Baggage::get_entry(baggage2, "correlation.id"), Some("corr-456"))
}

// Test 4: LogRecord severity and formatting
pub test "日志记录严重性和格式化测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "severity-test")
  
  // 测试不同严重级别的日志
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error message")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal message")
  
  // 验证严重级别
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // 测试带有详细上下文的日志记录
  let detailed_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(detailed_attrs, "error.code", azimuth::IntValue(500))
  azimuth::Attributes::set(detailed_attrs, "error.message", azimuth::StringValue("Internal server error"))
  
  let detailed_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Error processing request"),
    Some(detailed_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-123"),
    Some("span-456"),
    Some(root_ctx)
  )
  
  assert_eq(azimuth::LogRecord::body(detailed_log), Some("Error processing request"))
  assert_eq(azimuth::LogRecord::severity_number(detailed_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::trace_id(detailed_log), Some("trace-123"))
  assert_eq(azimuth::LogRecord::span_id(detailed_log), Some("span-456"))
}

// Test 5: Resource attribute inheritance and override
pub test "资源属性继承和覆盖测试" {
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.namespace", azimuth::StringValue("default"))
  ])
  
  // 创建继承的资源（应该覆盖同名属性）
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("override-service")),
    ("service.instance.id", azimuth::StringValue("instance-456")),
    ("environment", azimuth::StringValue("production"))
  ])
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // 验证属性继承和覆盖规则
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("override-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.namespace"), Some(azimuth::StringValue("default")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.instance.id"), Some(azimuth::StringValue("instance-456")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "environment"), Some(azimuth::StringValue("production")))
}

// Test 6: Propagator injection and extraction
pub test "传播器注入和提取测试" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建带有trace context的上下文
  let span_ctx = azimuth::SpanContext::new("trace-1234567890", "span-1234567890", true, "")
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 注入trace context
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 验证注入的头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header.length() > 0)
  
  // 提取trace context
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  assert_true(extracted_ctx !== ctx)
  
  // 测试Baggage传播
  let baggage_carrier = azimuth::TextMapCarrier::new()
  let baggage = azimuth::Baggage::new()
  let baggage_with_entries = azimuth::Baggage::set_entry(baggage, "user.id", "12345")
  
  // 注入和提取baggage
  azimuth::W3CBaggagePropagator::inject(baggage_propagator, baggage_with_entries, baggage_carrier)
  let extracted_baggage = azimuth::W3CBaggagePropagator::extract(baggage_propagator, baggage_carrier)
  
  assert_eq(azimuth::Baggage::get_entry(extracted_baggage, "user.id"), Some("12345"))
}

// Test 7: HTTP client request/response cycle
pub test "HTTP客户端请求响应周期测试" {
  let client = azimuth::HttpClient::new()
  
  // 创建HTTP请求
  let request_headers = [
    ("Content-Type", azimuth::StringValue("application/json")),
    ("Authorization", azimuth::StringValue("Bearer token123")),
    ("X-Request-ID", azimuth::StringValue("req-456"))
  ]
  
  let request = azimuth::HttpRequest::new(
    "POST", 
    "https://api.example.com/data", 
    request_headers, 
    Some("{\"key\": \"value\"}")
  )
  
  // 验证请求属性
  assert_eq(azimuth::HttpRequest::http_method(request), "POST")
  assert_eq(azimuth::HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(azimuth::HttpRequest::body(request), Some("{\"key\": \"value\"}"))
  
  // 创建HTTP响应
  let response_headers = [
    ("Content-Type", azimuth::StringValue("application/json")),
    ("X-Response-ID", azimuth::StringValue("resp-789"))
  ]
  
  let response = azimuth::HttpResponse::new(
    200, 
    response_headers, 
    Some("{\"status\": \"success\", \"data\": {\"id\": 123}}")
  )
  
  // 验证响应属性
  assert_eq(azimuth::HttpResponse::status_code(response), 200)
  assert_eq(azimuth::HttpResponse::body(response), Some("{\"status\": \"success\", \"data\": {\"id\": 123}}"))
}

// Test 8: Instrument scope and versioning
pub test "仪器作用域和版本控制测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  // 创建带有版本信息的仪器
  let tracer_v1 = azimuth::TracerProvider::get_tracer(tracer_provider, "versioned-tracer", Some("1.0.0"))
  let tracer_v2 = azimuth::TracerProvider::get_tracer(tracer_provider, "versioned-tracer", Some("2.0.0"))
  
  let meter_v1 = azimuth::MeterProvider::get_meter(meter_provider, "versioned-meter", Some("1.0.0"))
  let meter_v2 = azimuth::MeterProvider::get_meter(meter_provider, "versioned-meter", Some("2.0.0"))
  
  let logger_v1 = azimuth::LoggerProvider::get_logger(logger_provider, "versioned-logger", Some("1.0.0"))
  let logger_v2 = azimuth::LoggerProvider::get_logger(logger_provider, "versioned-logger", Some("2.0.0"))
  
  // 验证仪器作用域
  let tracer_scope_v1 = azimuth::Tracer::instrumentation_scope(tracer_v1)
  let tracer_scope_v2 = azimuth::Tracer::instrumentation_scope(tracer_v2)
  
  assert_eq(tracer_scope_v1.name, "versioned-tracer")
  assert_eq(tracer_scope_v1.version, Some("1.0.0"))
  assert_eq(tracer_scope_v2.name, "versioned-tracer")
  assert_eq(tracer_scope_v2.version, Some("2.0.0"))
  
  // 验证不同版本的仪器是独立的
  assert_true(tracer_v1 !== tracer_v2)
  assert_true(meter_v1 !== meter_v2)
  assert_true(logger_v1 !== logger_v2)
  
  // 测试带有属性的仪器作用域
  let tracer_with_attrs = azimuth::TracerProvider::get_tracer(
    tracer_provider, 
    "attributed-tracer", 
    Some("1.0.0"),
    Some([("library.name", azimuth::StringValue("azimuth")), ("library.version", azimuth::StringValue("1.0.0"))])
  )
  
  let attributed_scope = azimuth::Tracer::instrumentation_scope(tracer_with_attrs)
  assert_eq(attributed_scope.name, "attributed-tracer")
  assert_eq(attributed_scope.version, Some("1.0.0"))
}

// Test 9: Clock and timestamp precision
pub test "时钟和时间戳精度测试" {
  let system_clock = azimuth::Clock::system()
  let monotonic_clock = azimuth::Clock::monotonic()
  
  // 测试系统时钟
  let system_time1 = azimuth::Clock::now_unix_nanos(system_clock)
  let system_time2 = azimuth::Clock::now_unix_nanos(system_clock)
  
  // 验证时间戳递增
  assert_true(system_time2 >= system_time1)
  assert_true(system_time1 > 0L)
  
  // 测试单调时钟
  let monotonic_time1 = azimuth::Clock::now_unix_nanos(monotonic_clock)
  let monotonic_time2 = azimuth::Clock::now_unix_nanos(monotonic_clock)
  
  // 验证单调时钟递增
  assert_true(monotonic_time2 >= monotonic_time1)
  assert_true(monotonic_time1 > 0L)
  
  // 测试时间戳精度（纳秒级）
  let high_precision_time = azimuth::Clock::now_unix_nanos(system_clock)
  let time_str = high_precision_time.toString()
  
  // 验证时间戳格式（应该有足够的精度）
  assert_true(time_str.length() >= 16)
  
  // 测试时间戳转换
  let timestamp = azimuth::Timestamp::from_unix_nanos(high_precision_time)
  let converted_back = azimuth::Timestamp::to_unix_nanos(timestamp)
  
  assert_eq(high_precision_time, converted_back)
}

// Test 10: Composite operations integration
pub test "复合操作集成测试" {
  // 创建完整的遥测工作流
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("integration-test")),
    ("service.version", azimuth::StringValue("1.0.0"))
  ])
  
  // 设置上下文传播
  let root_ctx = azimuth::Context::root()
  let user_key = azimuth::ContextKey::new("user.id")
  let ctx_with_user = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  
  // 设置Baggage
  let baggage = azimuth::Baggage::new()
  let baggage_with_session = azimuth::Baggage::set_entry(baggage, "session.id", "session-67890")
  
  // 创建追踪器并开始Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "integration-tracer", Some("1.0.0"))
  let span = azimuth::Tracer::start_span(tracer, "integration-operation")
  
  // 添加Span事件和属性
  let span_attrs = [
    ("operation.type", azimuth::StringValue("integration")),
    ("user.id", azimuth::StringValue("user-12345"))
  ]
  azimuth::Span::set_attributes(span, span_attrs)
  azimuth::Span::add_event(span, "operation.started", Some([("timestamp", azimuth::StringValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).toString()))]))
  
  // 创建度量并记录
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "integration-meter", Some("1.0.0"))
  let counter = azimuth::Meter::create_counter(meter, "operations.count", Some("Total operations count"), Some("operations"))
  let histogram = azimuth::Meter::create_histogram(meter, "operation.duration", Some("Operation duration"), Some("ms"))
  
  azimuth::Counter::add(counter, 1.0)
  azimuth::Histogram::record(histogram, 150.5)
  
  // 创建日志记录器并记录日志
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "integration-logger", Some("1.0.0"))
  
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "operation.result", azimuth::StringValue("success"))
  azimuth::Attributes::set(log_attrs, "user.id", azimuth::StringValue("user-12345"))
  
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Integration operation completed successfully"),
    Some(log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
    Some(ctx_with_user)
  )
  
  azimuth::Logger::emit(logger, log_record)
  
  // 结束Span
  azimuth::Span::set_status(span, azimuth::Ok)
  azimuth::Span::add_event(span, "operation.completed", Some([("duration", azimuth::StringValue("150.5ms"))]))
  azimuth::Span::end(span)
  
  // 验证所有组件都正确创建和配置
  assert_eq(azimuth::Span::name(span), "integration-operation")
  assert_eq(counter.name, "operations.count")
  assert_eq(histogram.name, "operation.duration")
  assert_eq(logger.scope.name, "integration-logger")
  assert_eq(azimuth::LogRecord::body(log_record), Some("Integration operation completed successfully"))
  
  // 验证资源属性
  assert_eq(azimuth::Resource::get_attribute(resource, "service.name"), Some(azimuth::StringValue("integration-test")))
  assert_eq(azimuth::Resource::get_attribute(resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  
  // 验证上下文传播
  assert_eq(azimuth::Context::get(ctx_with_user, user_key), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "session.id"), Some("session-67890"))
}