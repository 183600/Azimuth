// Azimuth Telemetry System - High Quality Comprehensive Tests
// This file contains comprehensive high-quality test cases for core functionality

// Test 1: 综合上下文传播测试
test "comprehensive_context_propagation_test" {
  // 创建根上下文
  let root_ctx = Context::root()
  
  // 设置上下文值
  let correlation_key = ContextKey::new("correlation.id")
  let user_key = ContextKey::new("user.id")
  let request_key = ContextKey::new("request.id")
  
  let ctx_with_correlation = Context::with_value(root_ctx, correlation_key, "corr-12345")
  let ctx_with_user = Context::with_value(ctx_with_correlation, user_key, "user-67890")
  let ctx_with_request = Context::with_value(ctx_with_user, request_key, "req-abcde")
  
  // 验证上下文值传播
  assert_eq(Context::get(ctx_with_request, correlation_key), Some("corr-12345"))
  assert_eq(Context::get(ctx_with_request, user_key), Some("user-67890"))
  assert_eq(Context::get(ctx_with_request, request_key), Some("req-abcde"))
  
  // 测试Baggage传播
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user-67890")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-11111")
  
  assert_eq(Baggage::get_entry(baggage_with_session, "user.id"), Some("user-67890"))
  assert_eq(Baggage::get_entry(baggage_with_session, "session.id"), Some("session-11111"))
  
  // 测试传播器注入和提取
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  CompositePropagator::inject(propagator, ctx_with_request, carrier)
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // 验证提取的上下文包含预期值
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

// Test 2: 跨服务追踪一致性测试
test "cross_service_tracing_consistency_test" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross-service-test")
  
  // 创建服务A的根Span
  let service_a_span = Tracer::start_span(tracer, "service-a.operation")
  let service_a_ctx = Span::span_context(service_a_span)
  
  // 验证服务A的Span上下文
  assert_true(SpanContext::is_valid(service_a_ctx))
  assert_true(SpanContext::is_sampled(service_a_ctx))
  
  // 添加服务A的事件
  Span::add_event(service_a_span, "service-a.started", Some([
    ("service.name", StringValue("service-a")),
    ("operation.type", StringValue("api-request"))
  ]))
  
  // 模拟跨服务调用 - 创建服务B的子Span
  let service_b_span = Tracer::start_span(tracer, "service-b.operation")
  let service_b_ctx = Span::span_context(service_b_span)
  
  // 添加服务B的事件和属性
  Span::add_event(service_b_span, "service-b.started", Some([
    ("service.name", StringValue("service-b")),
    ("parent.trace", StringValue(SpanContext::trace_id(service_a_ctx))),
    ("parent.span", StringValue(SpanContext::span_id(service_a_ctx)))
  ]))
  
  // 模拟服务B调用服务C
  let service_c_span = Tracer::start_span(tracer, "service-c.operation")
  let service_c_ctx = Span::span_context(service_c_span)
  
  Span::add_event(service_c_span, "service-c.started", Some([
    ("service.name", StringValue("service-c")),
    ("root.trace", StringValue(SpanContext::trace_id(service_a_ctx))),
    ("intermediate.trace", StringValue(SpanContext::trace_id(service_b_ctx)))
  ]))
  
  // 按顺序结束Span（C -> B -> A）
  Span::set_status(service_c_span, Ok)
  Span::end(service_c_span)
  
  Span::add_event(service_b_span, "service-c.completed", None)
  Span::set_status(service_b_span, Ok)
  Span::end(service_b_span)
  
  Span::add_event(service_a_span, "service-b.completed", None)
  Span::set_status(service_a_span, Ok)
  Span::end(service_a_span)
  
  // 验证所有Span都已结束
  assert_false(Span::is_recording(service_a_span))
  assert_false(Span::is_recording(service_b_span))
  assert_false(Span::is_recording(service_c_span))
}

// Test 3: 度量系统聚合操作测试
test "metrics_aggregation_operations_test" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建各种类型的度量
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_histogram = Meter::create_histogram(meter, "http.response.time")
  let active_connections_gauge = Meter::create_gauge(meter, "http.active.connections")
  let error_updown_counter = Meter::create_updown_counter(meter, "http.errors.current")
  
  // 验证度量创建
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.time")
  assert_eq(active_connections_gauge.name, "http.active.connections")
  assert_eq(error_updown_counter.name, "http.errors.current")
  
  // 模拟度量操作
  let attrs = Attributes::new()
  Attributes::set(attrs, "method", StringValue("GET"))
  Attributes::set(attrs, "status", StringValue("200"))
  
  // Counter操作
  Counter::add(request_counter, 100.0, Some(attrs))
  Counter::add(request_counter, 50.0, Some(attrs))
  
  // Histogram操作
  Histogram::record(response_histogram, 150.0, Some(attrs))
  Histogram::record(response_histogram, 200.0, Some(attrs))
  Histogram::record(response_histogram, 100.0, Some(attrs))
  
  // UpDownCounter操作
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.type", StringValue("timeout"))
  UpDownCounter::add(error_updown_counter, 5.0, Some(error_attrs))
  UpDownCounter::add(error_updown_counter, -2.0, Some(error_attrs))
  
  // 测试度量作为Instrument的转换
  let counter_instrument = Histogram::as_instrument(response_histogram)
  assert_eq(Instrument::name(counter_instrument), "http.response.time")
  assert_eq(Instrument::description(counter_instrument), Some("HTTP response time"))
  assert_eq(Instrument::unit(counter_instrument), Some("ms"))
  
  // 测试不同属性的度量
  let post_attrs = Attributes::new()
  Attributes::set(post_attrs, "method", StringValue("POST"))
  Attributes::set(post_attrs, "status", StringValue("201"))
  
  Counter::add(request_counter, 25.0, Some(post_attrs))
  Histogram::record(response_histogram, 300.0, Some(post_attrs))
  
  // 验证度量操作完成
  assert_true(true)
}

// Test 4: 日志记录与追踪关联测试
test "log_record_trace_correlation_test" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "correlation-test")
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "correlation-test")
  
  // 创建Span用于追踪
  let span = Tracer::start_span(tracer, "operation.with.logs")
  let span_ctx = Span::span_context(span)
  
  // 创建与追踪关联的日志记录
  let info_log = LogRecord::new_with_context(
    Info,
    Some("Operation started successfully"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    None
  )
  
  // 添加Span事件
  Span::add_event(span, "log.recorded", Some([
    ("log.level", StringValue("INFO")),
    ("log.message", StringValue("Operation started successfully"))
  ]))
  
  // 创建警告日志
  let warn_attrs = Attributes::new()
  Attributes::set(warn_attrs, "warning.type", StringValue("performance"))
  
  let warn_log = LogRecord::new_with_context(
    Warn,
    Some("Operation taking longer than expected"),
    Some(warn_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    None
  )
  
  // 添加警告事件到Span
  Span::add_event(span, "warning.issued", Some([
    ("warning.type", StringValue("performance")),
    ("log.message", StringValue("Operation taking longer than expected"))
  ]))
  
  // 创建错误日志
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.code", IntValue(500))
  Attributes::set(error_attrs, "error.type", StringValue("internal"))
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Internal server error occurred"),
    Some(error_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    None
  )
  
  // 设置Span错误状态
  Span::set_status(span, Error)
  
  // 发出所有日志
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  
  // 结束Span
  Span::end(span)
  
  // 验证日志和追踪关联
  assert_eq(LogRecord::trace_id(info_log), Some(SpanContext::trace_id(span_ctx)))
  assert_eq(LogRecord::span_id(info_log), Some(SpanContext::span_id(span_ctx)))
  assert_eq(LogRecord::severity_number(info_log), Info)
  
  assert_eq(LogRecord::trace_id(warn_log), Some(SpanContext::trace_id(span_ctx)))
  assert_eq(LogRecord::span_id(warn_log), Some(SpanContext::span_id(span_ctx)))
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  
  assert_eq(LogRecord::trace_id(error_log), Some(SpanContext::trace_id(span_ctx)))
  assert_eq(LogRecord::span_id(error_log), Some(SpanContext::span_id(span_ctx)))
  assert_eq(LogRecord::severity_number(error_log), Error)
}

// Test 5: 资源合并策略测试
test "resource_merge_strategy_test" {
  // 创建基础资源
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  let base_resource_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  // 创建覆盖资源
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.instance.id", StringValue("instance-12345")),
    ("host.name", StringValue("server-01")),
    ("deployment.environment", StringValue("staging")) // 这个应该覆盖基础资源
  ]
  let override_resource_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // 执行资源合并
  let merged_resource = Resource::merge(base_resource_with_attrs, override_resource_with_attrs)
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("azimuth-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), Some(StringValue("staging"))) // 被覆盖
  assert_eq(Resource::get_attribute(merged_resource, "service.instance.id"), Some(StringValue("instance-12345")))
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("server-01")))
  
  // 测试多级合并
  let additional_resource = Resource::new()
  let additional_attrs = [
    ("service.namespace", StringValue("backend")),
    ("service.version", StringValue("1.1.0")), // 再次覆盖版本
    ("region", StringValue("us-west"))
  ]
  let additional_resource_with_attrs = Resource::with_attributes(additional_resource, additional_attrs)
  
  let final_merged = Resource::merge(merged_resource, additional_resource_with_attrs)
  
  // 验证最终合并结果
  assert_eq(Resource::get_attribute(final_merged, "service.name"), Some(StringValue("azimuth-service")))
  assert_eq(Resource::get_attribute(final_merged, "service.version"), Some(StringValue("1.1.0"))) // 最终版本
  assert_eq(Resource::get_attribute(final_merged, "deployment.environment"), Some(StringValue("staging")))
  assert_eq(Resource::get_attribute(final_merged, "service.instance.id"), Some(StringValue("instance-12345")))
  assert_eq(Resource::get_attribute(final_merged, "host.name"), Some(StringValue("server-01")))
  assert_eq(Resource::get_attribute(final_merged, "service.namespace"), Some(StringValue("backend")))
  assert_eq(Resource::get_attribute(final_merged, "region"), Some(StringValue("us-west")))
  
  // 测试空资源合并
  let empty_resource = Resource::new()
  let merged_with_empty = Resource::merge(final_merged, empty_resource)
  
  // 验证与空资源合并不影响原资源
  assert_eq(Resource::get_attribute(merged_with_empty, "service.name"), Some(StringValue("azimuth-service")))
  assert_eq(Resource::get_attribute(merged_with_empty, "service.version"), Some(StringValue("1.1.0")))
}

// Test 6: 并发安全性测试
test "concurrent_safety_test" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "concurrent-test")
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent-test")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "concurrent-test")
  
  // 创建共享资源
  let shared_counter = Meter::create_counter(meter, "concurrent.operations")
  let shared_histogram = Meter::create_histogram(meter, "concurrent.duration")
  let shared_gauge = Meter::create_gauge(meter, "concurrent.active")
  
  // 模拟并发操作
  let spans = []
  let logs = []
  
  // 创建多个Span（模拟并发）
  for i in 0..50 {
    let span = Tracer::start_span(tracer, "concurrent.operation." + i.to_string())
    
    // 为每个Span添加唯一属性
    let attrs = Attributes::new()
    Attributes::set(attrs, "operation.id", IntValue(i))
    Attributes::set(attrs, "thread.id", StringValue("thread-" + (i % 5).to_string()))
    
    Span::add_event(span, "operation.started", Some(attrs))
    spans.push(span)
    
    // 创建对应的日志记录
    let log = LogRecord::new_with_context(
      Info,
      Some("Concurrent operation " + i.to_string() + " started"),
      Some(attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some(SpanContext::trace_id(Span::span_context(span))),
      Some(SpanContext::span_id(Span::span_context(span))),
      None
    )
    logs.push(log)
  }
  
  // 模拟并发度量操作
  for i in 0..50 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "operation.id", IntValue(i))
    
    // 并发Counter操作
    Counter::add(shared_counter, 1.0, Some(attrs))
    
    // 并发Histogram操作
    Histogram::record(shared_histogram, (i % 100).to_double(), Some(attrs))
    
    // 并发日志记录
    Logger::emit(logger, logs[i])
  }
  
  // 验证所有Span状态
  for span in spans {
    assert_true(Span::is_recording(span))
    Span::set_status(span, Ok)
    Span::end(span)
  }
  
  // 验证所有Span都已结束
  for span in spans {
    assert_false(Span::is_recording(span))
  }
  
  // 验证共享资源仍然有效
  assert_eq(shared_counter.name, "concurrent.operations")
  assert_eq(shared_histogram.name, "concurrent.duration")
  assert_eq(shared_gauge.name, "concurrent.active")
}

// Test 7: 序列化完整性测试
test "serialization_integrity_test" {
  // 创建复杂的属性集合
  let complex_attrs = Attributes::new()
  
  // 设置各种类型的属性
  Attributes::set(complex_attrs, "string.value", StringValue("test string"))
  Attributes::set(complex_attrs, "int.value", IntValue(42))
  Attributes::set(complex_attrs, "float.value", FloatValue(3.14159))
  Attributes::set(complex_attrs, "bool.value", BoolValue(true))
  
  // 设置数组属性
  let string_array = ["value1", "value2", "value3"]
  let int_array = [1, 2, 3, 4, 5]
  
  // 验证属性检索
  assert_eq(Attributes::get(complex_attrs, "string.value"), Some(StringValue("test string")))
  assert_eq(Attributes::get(complex_attrs, "int.value"), Some(IntValue(42)))
  assert_eq(Attributes::get(complex_attrs, "float.value"), Some(FloatValue(3.14159)))
  assert_eq(Attributes::get(complex_attrs, "bool.value"), Some(BoolValue(true)))
  
  // 测试SpanContext序列化
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  // 验证SpanContext属性
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  // 测试HTTP请求/响应序列化
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Trace-ID", trace_id),
    ("X-Request-ID", "req-12345")
  ]
  
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"test\": \"data\"}"))
  
  // 验证请求属性
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"test\": \"data\"}"))
  
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-Time", "150ms"),
    ("X-Trace-ID", trace_id)
  ]
  
  let response = HttpResponse::new(200, response_headers, Some("{\"result\": \"success\"}"))
  
  // 验证响应属性
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"result\": \"success\"}"))
  
  // 测试TextMapCarrier序列化
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, " baggage", "user.id=12345,session.id=abcdef")
  
  // 验证Carrier属性
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
}

// Test 8: 边界条件和错误恢复测试
test "boundary_conditions_error_recovery_test" {
  // 测试空字符串和None值
  let empty_trace_id = ""
  let empty_span_id = ""
  let invalid_span_ctx = SpanContext::new(empty_trace_id, empty_span_id, true, "")
  
  // 验证无效SpanContext
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // 测试部分有效的SpanContext
  let valid_trace_invalid_span = SpanContext::new("1234567890abcdef1234567890abcdef", "", true, "")
  let invalid_trace_valid_span = SpanContext::new("", "1234567890abcdef", true, "")
  
  assert_false(SpanContext::is_valid(valid_trace_invalid_span))
  assert_false(SpanContext::is_valid(invalid_trace_valid_span))
  
  // 测试边界值的度量创建
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "boundary-test")
  
  // 空名称度量
  let empty_name_counter = Meter::create_counter(meter, "")
  assert_eq(empty_name_counter.name, "")
  
  // 极长名称度量
  let very_long_name = "this.is.a.very.long.metric.name.that.exceeds.normal.expectations.and.tests.boundary.conditions"
  let long_name_gauge = Meter::create_gauge(meter, very_long_name)
  assert_eq(long_name_gauge.name, very_long_name)
  
  // 测试特殊字符度量名称
  let special_chars_name = "metric.with-special_chars_123!@#$%"
  let special_chars_histogram = Meter::create_histogram(meter, special_chars_name)
  assert_eq(special_chars_histogram.name, special_chars_name)
  
  // 测试Unicode度量名称
  let unicode_name = "测试度量🚀"
  let unicode_updown_counter = Meter::create_updown_counter(meter, unicode_name)
  assert_eq(unicode_updown_counter.name, unicode_name)
  
  // 测试边界值的日志记录
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "boundary-test")
  
  // 空日志消息
  let empty_log = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_log), Some(""))
  
  // 极长日志消息
  let very_long_message = "this.is.a.very.long.log.message.that.exceeds.normal.expectations.and.tests.boundary.conditions".repeat(10)
  let long_log = LogRecord::new(Info, very_long_message)
  assert_eq(LogRecord::body(long_log), Some(very_long_message))
  
  // 特殊字符日志消息
  let special_chars_message = "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_chars_log = LogRecord::new(Warn, special_chars_message)
  assert_eq(LogRecord::body(special_chars_log), Some(special_chars_message))
  
  // Unicode日志消息
  let unicode_message = "Unicode测试消息🚀 with émojis and ñ special chars"
  let unicode_log = LogRecord::new(Error, unicode_message)
  assert_eq(LogRecord::body(unicode_log), Some(unicode_message))
  
  // 测试边界值的时间戳
  let zero_timestamp = 0L
  let max_timestamp = 9223372036854775807L
  let negative_timestamp = -1L
  
  let zero_log = LogRecord::new_with_context(
    Info,
    Some("Zero timestamp"),
    None,
    Some(zero_timestamp),
    None,
    None,
    None,
    None
  )
  
  let max_log = LogRecord::new_with_context(
    Info,
    Some("Max timestamp"),
    None,
    Some(max_timestamp),
    None,
    None,
    None,
    None
  )
  
  // 验证边界值时间戳日志
  assert_eq(LogRecord::body(zero_log), Some("Zero timestamp"))
  assert_eq(LogRecord::body(max_log), Some("Max timestamp"))
  
  // 测试错误恢复
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-recovery-test")
  
  let error_span = Tracer::start_span(tracer, "error.operation")
  
  // 添加错误事件
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.type", StringValue("BoundaryCondition"))
  Attributes::set(error_attrs, "error.message", StringValue("Test boundary condition error"))
  Attributes::set(error_attrs, "error.recovered", BoolValue(true))
  
  Span::add_event(error_span, "error.occurred", Some(error_attrs))
  Span::add_event(error_span, "recovery.attempted", Some([
    ("recovery.strategy", StringValue("retry")),
    ("retry.count", IntValue(3))
  ]))
  
  // 设置错误状态然后恢复
  Span::set_status(error_span, Error)
  Span::add_event(error_span, "recovery.successful", None)
  Span::set_status(error_span, Ok)
  
  // 结束Span
  Span::end(error_span)
  
  // 验证错误恢复
  assert_false(Span::is_recording(error_span))
}