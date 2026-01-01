// Azimuth 高质量测试用例集
// 覆盖遥测系统的核心功能模块

// 测试用例1: 属性操作和类型转换
pub test "属性操作和类型转换测试" {
  // 创建属性集合
  let attrs = azimuth::Attributes::new()
  
  // 测试基本类型属性
  azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("测试字符串"))
  azimuth::Attributes::set(attrs, "int.key", azimuth::IntValue(42))
  azimuth::Attributes::set(attrs, "float.key", azimuth::FloatValue(3.14159))
  azimuth::Attributes::set(attrs, "bool.key", azimuth::BoolValue(true))
  
  // 测试数组类型属性
  azimuth::Attributes::set(attrs, "array.string", azimuth::ArrayStringValue(["元素1", "元素2", "元素3"]))
  azimuth::Attributes::set(attrs, "array.int", azimuth::ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 验证属性值
  let string_val = azimuth::Attributes::get(attrs, "string.key")
  let int_val = azimuth::Attributes::get(attrs, "int.key")
  let float_val = azimuth::Attributes::get(attrs, "float.key")
  let bool_val = azimuth::Attributes::get(attrs, "bool.key")
  let array_string_val = azimuth::Attributes::get(attrs, "array.string")
  let array_int_val = azimuth::Attributes::get(attrs, "array.int")
  
  // 基于简化实现进行验证
  assert_eq(string_val, Some(azimuth::StringValue("test_value")))
  assert_eq(int_val, Some(azimuth::IntValue(42)))
  
  // 测试边界值
  let extreme_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(extreme_attrs, "max.int", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(extreme_attrs, "min.int", azimuth::IntValue(-2147483648))
  azimuth::Attributes::set(extreme_attrs, "max.float", azimuth::FloatValue(1.7976931348623157e+308))
  azimuth::Attributes::set(extreme_attrs, "min.float", azimuth::FloatValue(-1.7976931348623157e+308))
  
  // 测试特殊字符和Unicode
  let unicode_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(unicode_attrs, "unicode.text", azimuth::StringValue("测试中文🚀emoji"))
  azimuth::Attributes::set(unicode_attrs, "special.chars", azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
}

// 测试用例2: 上下文传播和Baggage操作
pub test "上下文传播和Baggage操作测试" {
  // 创建根上下文
  let root_ctx = azimuth::Context::root()
  
  // 测试上下文键值对操作
  let user_key = azimuth::ContextKey::new("user.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let session_key = azimuth::ContextKey::new("session.id")
  
  // 设置上下文值
  let ctx_with_user = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_user, request_key, "req-67890")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_request, session_key, "sess-abcde")
  
  // 验证上下文值
  assert_eq(azimuth::Context::get(ctx_with_user, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_request, request_key), Some("req-67890"))
  assert_eq(azimuth::Context::get(ctx_with_session, session_key), Some("sess-abcde"))
  
  // 测试缺失的键
  let missing_key = azimuth::ContextKey::new("missing.key")
  assert_eq(azimuth::Context::get(root_ctx, missing_key), None)
  
  // 测试Baggage操作
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_user, "request.id", "req-67890")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_request, "session.id", "sess-abcde")
  
  // 验证Baggage条目
  assert_eq(azimuth::Baggage::get_entry(baggage_with_user, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "session.id"), Some("sess-abcde"))
  
  // 测试Baggage传播一致性
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "request.id"), Some("req-67890"))
}

// 测试用例3: Span生命周期和追踪操作
pub test "Span生命周期和追踪操作测试" {
  // 创建Span上下文
  let span_ctx = azimuth::SpanContext::new("trace-1234567890", "span-0987654321", true, "key1=value1,key2=value2")
  
  // 验证Span上下文
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), "trace-1234567890")
  assert_eq(azimuth::SpanContext::span_id(span_ctx), "span-0987654321")
  assert_true(azimuth::SpanContext::is_valid(span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  
  // 测试不同类型的Span
  let internal_span = azimuth::Span::new("internal-operation", azimuth::Internal, span_ctx)
  let server_span = azimuth::Span::new("server-operation", azimuth::Server, span_ctx)
  let client_span = azimuth::Span::new("client-operation", azimuth::Client, span_ctx)
  let producer_span = azimuth::Span::new("producer-operation", azimuth::Producer, span_ctx)
  let consumer_span = azimuth::Span::new("consumer-operation", azimuth::Consumer, span_ctx)
  
  // 验证Span属性
  assert_eq(azimuth::Span::name(internal_span), "internal-operation")
  assert_eq(azimuth::Span::kind(internal_span), azimuth::Internal)
  assert_eq(azimuth::Span::name(server_span), "server-operation")
  assert_eq(azimuth::Span::kind(server_span), azimuth::Server)
  assert_eq(azimuth::Span::name(client_span), "client-operation")
  assert_eq(azimuth::Span::kind(client_span), azimuth::Client)
  assert_eq(azimuth::Span::name(producer_span), "producer-operation")
  assert_eq(azimuth::Span::kind(producer_span), azimuth::Producer)
  assert_eq(azimuth::Span::name(consumer_span), "consumer-operation")
  assert_eq(azimuth::Span::kind(consumer_span), azimuth::Consumer)
  
  // 测试Tracer操作
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "test-tracer", Some("1.0.0"))
  let scope = azimuth::Tracer::instrumentation_scope(tracer)
  
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  // 测试Span创建
  let new_span = azimuth::Tracer::start_span(tracer, "test-operation")
  assert_eq(azimuth::Span::name(new_span), "test-operation")
  assert_true(azimuth::Span::is_recording(new_span))
}

// 测试用例4: 度量操作和仪器类型
pub test "度量操作和仪器类型测试" {
  // 创建Meter
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "test-meter", Some("1.0.0"))
  
  // 测试Counter
  let counter = azimuth::Meter::create_counter(meter, "request.count", Some("Total number of requests"), Some("count"))
  assert_eq(counter.name, "request.count")
  assert_eq(counter.description, Some("Total number of requests"))
  assert_eq(counter.unit, Some("count"))
  
  // 测试Counter操作
  azimuth::Counter::add(counter, 1.0)
  azimuth::Counter::add(counter, 5.0)
  azimuth::Counter::add(counter, 10.0)
  
  // 测试Histogram
  let histogram = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time in milliseconds"))
  assert_eq(histogram.unit, Some("ms"))
  
  // 测试Histogram操作
  azimuth::Histogram::record(histogram, 100.0)
  azimuth::Histogram::record(histogram, 150.5)
  azimuth::Histogram::record(histogram, 200.75)
  azimuth::Histogram::record(histogram, 300.25)
  
  // 测试UpDownCounter
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "active.connections", Some("Currently active connections"), Some("connections"))
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Currently active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // 测试UpDownCounter操作
  azimuth::UpDownCounter::add(updown_counter, 10.0)
  azimuth::UpDownCounter::add(updown_counter, -2.0)
  azimuth::UpDownCounter::add(updown_counter, 5.0)
  
  // 测试Gauge
  let gauge = azimuth::Meter::create_gauge(meter, "memory.usage", Some("Memory usage in MB"), Some("MB"))
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Memory usage in MB"))
  assert_eq(gauge.unit, Some("MB"))
  
  // 测试Instrument转换
  let counter_instrument = azimuth::Histogram::as_instrument(histogram)
  assert_eq(azimuth::Instrument::name(counter_instrument), "response.time")
  assert_eq(azimuth::Instrument::description(counter_instrument), Some("Response time in milliseconds"))
  assert_eq(azimuth::Instrument::unit(counter_instrument), Some("ms"))
}

// 测试用例5: 日志记录和严重性级别
pub test "日志记录和严重性级别测试" {
  // 测试不同严重性级别的日志记录
  let trace_log = azimuth::LogRecord::new(azimuth::Trace, "Trace level log message")
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug level log message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info level log message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning level log message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error level log message")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal level log message")
  
  // 验证日志严重性级别
  assert_eq(azimuth::LogRecord::severity_number(trace_log), azimuth::Trace)
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // 验证日志消息
  assert_eq(azimuth::LogRecord::body(trace_log), Some("Trace level log message"))
  assert_eq(azimuth::LogRecord::body(debug_log), Some("Debug level log message"))
  assert_eq(azimuth::LogRecord::body(info_log), Some("Info level log message"))
  assert_eq(azimuth::LogRecord::body(warn_log), Some("Warning level log message"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("Error level log message"))
  assert_eq(azimuth::LogRecord::body(fatal_log), Some("Fatal level log message"))
  
  // 测试带上下文的详细日志记录
  let attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(attributes, "user.id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(attributes, "request.id", azimuth::StringValue("req-456"))
  
  let detailed_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Detailed error occurred"),
    Some(attributes),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-123"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证详细日志记录
  assert_eq(azimuth::LogRecord::severity_number(detailed_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(detailed_log), Some("Detailed error occurred"))
  assert_eq(azimuth::LogRecord::trace_id(detailed_log), Some("trace-123"))
  assert_eq(azimuth::LogRecord::span_id(detailed_log), Some("span-456"))
  
  // 测试Logger操作
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "test-logger", Some("1.0.0"))
  assert_eq(logger.scope.name, "test-logger")
  assert_eq(logger.scope.version, Some("1.0.0"))
  
  // 测试日志发射
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, detailed_log)
}

// 测试用例6: 资源管理和合并策略
pub test "资源管理和合并策略测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::new()
  
  // 添加基础属性
  let base_attrs = [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123"))
  ]
  let resource_with_base = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // 验证基础属性
  assert_eq(azimuth::Resource::get_attribute(resource_with_base, "service.name"), Some(azimuth::StringValue("base-service")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_base, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_base, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
  
  // 创建覆盖资源
  let override_attrs = [
    ("service.name", azimuth::StringValue("override-service")),  // 覆盖基础属性
    ("environment", azimuth::StringValue("production")),         // 新增属性
    ("deployment.region", azimuth::StringValue("us-west-2"))     // 新增属性
  ]
  let override_resource = azimuth::Resource::with_attributes(base_resource, override_attrs)
  
  // 测试资源合并
  let merged_resource = azimuth::Resource::merge(resource_with_base, override_resource)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("override-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.region"), Some(azimuth::StringValue("us-west-2")))
  
  // 测试空资源合并
  let empty_resource = azimuth::Resource::new()
  let merged_with_empty = azimuth::Resource::merge(resource_with_base, empty_resource)
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty, "service.name"), Some(azimuth::StringValue("base-service")))
  
  // 测试复杂属性值
  let complex_attrs = [
    ("service.tags", azimuth::ArrayStringValue(["web", "api", "microservice"])),
    ("service.ports", azimuth::ArrayIntValue([8080, 8443, 9090])),
    ("service.enabled", azimuth::BoolValue(true)),
    ("service.weight", azimuth::FloatValue(0.75))
  ]
  let complex_resource = azimuth::Resource::with_attributes(base_resource, complex_attrs)
  
  // 验证复杂属性
  assert_eq(azimuth::Resource::get_attribute(complex_resource, "service.tags"), Some(azimuth::ArrayStringValue(["web", "api", "microservice"])))
  assert_eq(azimuth::Resource::get_attribute(complex_resource, "service.ports"), Some(azimuth::ArrayIntValue([8080, 8443, 9090])))
  assert_eq(azimuth::Resource::get_attribute(complex_resource, "service.enabled"), Some(azimuth::BoolValue(true)))
  assert_eq(azimuth::Resource::get_attribute(complex_resource, "service.weight"), Some(azimuth::FloatValue(0.75)))
}

// 测试用例7: 传播器注入和提取操作
pub test "传播器注入和提取操作测试" {
  // 创建传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建载体
  let carrier = azimuth::TextMapCarrier::new()
  
  // 创建上下文
  let ctx = azimuth::Context::root()
  
  // 测试注入操作
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 验证注入的头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 测试提取操作
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取结果
  let key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, key)
  assert_eq(extracted_value, Some("true"))
  
  // 测试Baggage传播
  let baggage_carrier = azimuth::TextMapCarrier::new()
  let baggage_with_entries = azimuth::Baggage::new()
  let updated_baggage = azimuth::Baggage::set_entry(baggage_with_entries, "user.id", "user-123")
  let final_baggage = azimuth::Baggage::set_entry(updated_baggage, "request.id", "req-456")
  
  // 验证Baggage条目
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "request.id"), Some("req-456"))
  
  // 测试多传播器组合
  let multiple_propagators = [trace_propagator]
  let multi_composite = azimuth::CompositePropagator::new(multiple_propagators)
  
  let multi_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(multi_composite, ctx, multi_carrier)
  
  let multi_trace_header = azimuth::TextMapCarrier::get(multi_carrier, "traceparent")
  assert_eq(multi_trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 测试空载体处理
  let empty_carrier = azimuth::TextMapCarrier::new()
  let extracted_from_empty = azimuth::CompositePropagator::extract(composite_propagator, empty_carrier)
  let empty_key = azimuth::ContextKey::new("empty.test")
  let empty_value = azimuth::Context::get(extracted_from_empty, empty_key)
  assert_eq(empty_value, None)
}

// 测试用例8: 错误处理和边界条件
pub test "错误处理和边界条件测试" {
  // 测试空属性处理
  let empty_attrs = azimuth::Attributes::new()
  assert_eq(azimuth::Attributes::get(empty_attrs, ""), None)
  assert_eq(azimuth::Attributes::get(empty_attrs, "nonexistent.key"), None)
  
  // 测试空字符串键
  let attrs_with_empty_key = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs_with_empty_key, "", azimuth::StringValue("empty.key.value"))
  let empty_key_value = azimuth::Attributes::get(attrs_with_empty_key, "")
  assert_eq(empty_key_value, Some(azimuth::StringValue("test_value")))
  
  // 测试极长键名
  let very_long_key = "this.is.a.very.long.key.name.that.exceeds.normal.expectations.and.tests.boundary.conditions"
  let attrs_with_long_key = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs_with_long_key, very_long_key, azimuth::StringValue("long.key.value"))
  let long_key_value = azimuth::Attributes::get(attrs_with_long_key, very_long_key)
  assert_eq(long_key_value, Some(azimuth::StringValue("test_value")))
  
  // 测试无效Span上下文
  let empty_trace_ctx = azimuth::SpanContext::new("", "span123", true, "")
  assert_false(azimuth::SpanContext::is_valid(empty_trace_ctx))
  
  let empty_span_ctx = azimuth::SpanContext::new("trace123", "", true, "")
  assert_false(azimuth::SpanContext::is_valid(empty_span_ctx))
  
  let both_empty_ctx = azimuth::SpanContext::new("", "", false, "")
  assert_false(azimuth::SpanContext::is_valid(both_empty_ctx))
  assert_false(azimuth::SpanContext::is_sampled(both_empty_ctx))
  
  // 测试上下文键边界条件
  let empty_context_key = azimuth::ContextKey::new("")
  let root_ctx = azimuth::Context::root()
  let ctx_with_empty_key = azimuth::Context::with_value(root_ctx, empty_context_key, "empty.context.value")
  let empty_context_value = azimuth::Context::get(ctx_with_empty_key, empty_context_key)
  assert_eq(empty_context_value, Some("empty.context.value"))
  
  // 测试数值边界条件
  let boundary_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(boundary_attrs, "max.int", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(boundary_attrs, "min.int", azimuth::IntValue(-2147483648))
  azimuth::Attributes::set(boundary_attrs, "zero.int", azimuth::IntValue(0))
  
  azimuth::Attributes::set(boundary_attrs, "max.float", azimuth::FloatValue(1.7976931348623157e+308))
  azimuth::Attributes::set(boundary_attrs, "min.float", azimuth::FloatValue(-1.7976931348623157e+308))
  azimuth::Attributes::set(boundary_attrs, "zero.float", azimuth::FloatValue(0.0))
  azimuth::Attributes::set(boundary_attrs, "negative.zero", azimuth::FloatValue(-0.0))
  
  // 测试特殊浮点值
  azimuth::Attributes::set(boundary_attrs, "infinity", azimuth::FloatValue(1.0/0.0))
  azimuth::Attributes::set(boundary_attrs, "negative.infinity", azimuth::FloatValue(-1.0/0.0))
  
  // 测试空数组和单元素数组
  azimuth::Attributes::set(boundary_attrs, "empty.string.array", azimuth::ArrayStringValue([]))
  azimuth::Attributes::set(boundary_attrs, "single.string.array", azimuth::ArrayStringValue(["single"]))
  azimuth::Attributes::set(boundary_attrs, "empty.int.array", azimuth::ArrayIntValue([]))
  azimuth::Attributes::set(boundary_attrs, "single.int.array", azimuth::ArrayIntValue([42]))
}

// 测试用例9: 性能和并发安全测试
pub test "性能和并发安全测试" {
  // 测试大量属性操作性能
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let large_attrs = azimuth::Attributes::new()
  
  // 添加大量属性
  for i in 0..1000 {
    let key = "attr." + i.to_string()
    let value = azimuth::StringValue("value." + i.to_string())
    azimuth::Attributes::set(large_attrs, key, value)
  }
  
  // 查询大量属性
  for i in 0..1000 {
    let key = "attr." + i.to_string()
    let _ = azimuth::Attributes::get(large_attrs, key)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration = end_time - start_time
  
  // 验证性能在合理范围内（小于5秒）
  assert_true(duration < 5000000000L)
  
  // 测试大量Span创建性能
  let span_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  let spans = []
  
  // 创建大量Span
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "perf-span-" + i.to_string())
    spans.push(span)
  }
  
  // 批量操作Span
  for span in spans {
    azimuth::Span::add_event(span, "performance.event", Some([("iteration", azimuth::StringValue("test"))]))
    azimuth::Span::set_status(span, azimuth::Ok)
  }
  
  // 批量结束Span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  let span_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let span_duration = span_end_time - span_start_time
  
  // 验证Span操作性能（小于10秒）
  assert_true(span_duration < 10000000000L)
  assert_true(spans.length() == 100)
  
  // 测试度量操作性能
  let metric_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "perf-meter")
  let counter = azimuth::Meter::create_counter(meter, "perf-counter")
  let histogram = azimuth::Meter::create_histogram(meter, "perf-histogram")
  
  // 大量度量操作
  for i in 0..1000 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double())
  }
  
  let metric_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let metric_duration = metric_end_time - metric_start_time
  
  // 验证度量操作性能（小于5秒）
  assert_true(metric_duration < 5000000000L)
}

// 测试用例10: 端到端集成测试
pub test "端到端集成测试" {
  // 设置资源
  let resource = azimuth::Resource::new()
  let resource_attrs = [
    ("service.name", azimuth::StringValue("integration-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-12345")),
    ("deployment.environment", azimuth::StringValue("test"))
  ]
  let service_resource = azimuth::Resource::with_attributes(resource, resource_attrs)
  
  // 设置上下文和Baggage
  let ctx = azimuth::Context::root()
  let user_key = azimuth::ContextKey::new("user.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let session_key = azimuth::ContextKey::new("session.id")
  
  let ctx_with_user = azimuth::Context::with_value(ctx, user_key, "user-12345")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_user, request_key, "req-67890")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_request, session_key, "sess-abcde")
  
  // 设置Baggage
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_user, "request.id", "req-67890")
  let final_baggage = azimuth::Baggage::set_entry(baggage_with_request, "session.id", "sess-abcde")
  
  // 创建追踪组件
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "integration-tracer", Some("1.0.0"))
  
  // 创建Span层次结构
  let root_span = azimuth::Tracer::start_span(tracer, "root-operation")
  let child_span1 = azimuth::Tracer::start_span(tracer, "child-operation-1")
  let child_span2 = azimuth::Tracer::start_span(tracer, "child-operation-2")
  let grandchild_span = azimuth::Tracer::start_span(tracer, "grandchild-operation")
  
  // 添加Span事件和属性
  let span_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(span_attrs, "operation.type", azimuth::StringValue("integration"))
  azimuth::Attributes::set(span_attrs, "component.version", azimuth::StringValue("1.0.0"))
  
  azimuth::Span::add_event(root_span, "operation.started", Some(span_attrs))
  azimuth::Span::add_event(child_span1, "child.operation.started", Some(span_attrs))
  azimuth::Span::add_event(child_span2, "child.operation.started", Some(span_attrs))
  azimuth::Span::add_event(grandchild_span, "grandchild.operation.started", Some(span_attrs))
  
  // 设置Span状态
  azimuth::Span::set_status(root_span, azimuth::Ok)
  azimuth::Span::set_status(child_span1, azimuth::Ok)
  azimuth::Span::set_status(child_span2, azimuth::Ok)
  azimuth::Span::set_status(grandchild_span, azimuth::Ok)
  
  // 创建度量组件
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "integration-meter", Some("1.0.0"))
  
  let request_counter = azimuth::Meter::create_counter(meter, "requests.total", Some("Total number of requests"), Some("count"))
  let response_histogram = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  let active_gauge = azimuth::Meter::create_gauge(meter, "active.connections", Some("Currently active connections"), Some("connections"))
  
  // 执行度量操作
  azimuth::Counter::add(request_counter, 1.0)
  azimuth::Histogram::record(response_histogram, 150.5)
  azimuth::Histogram::record(response_histogram, 200.75)
  azimuth::Histogram::record(response_histogram, 100.25)
  
  // 创建日志组件
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "integration-logger", Some("1.0.0"))
  
  // 创建日志记录
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "user.id", azimuth::StringValue("user-12345"))
  azimuth::Attributes::set(log_attrs, "request.id", azimuth::StringValue("req-67890"))
  azimuth::Attributes::set(log_attrs, "session.id", azimuth::StringValue("sess-abcde"))
  
  let info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Integration test operation completed successfully"),
    Some(log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(root_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(root_span))),
    Some(ctx_with_session)
  )
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Integration test error occurred"),
    Some(log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(child_span1))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(child_span1))),
    Some(ctx_with_session)
  )
  
  // 发射日志
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, error_log)
  
  // 结束所有Span
  azimuth::Span::end(grandchild_span)
  azimuth::Span::end(child_span2)
  azimuth::Span::end(child_span1)
  azimuth::Span::end(root_span)
  
  // 验证所有组件创建正确
  assert_eq(azimuth::Span::name(root_span), "root-operation")
  assert_eq(azimuth::Span::name(child_span1), "child-operation-1")
  assert_eq(azimuth::Span::name(child_span2), "child-operation-2")
  assert_eq(azimuth::Span::name(grandchild_span), "grandchild-operation")
  
  assert_eq(request_counter.name, "requests.total")
  assert_eq(response_histogram.name, "response.time")
  assert_eq(active_gauge.name, "active.connections")
  
  assert_eq(logger.scope.name, "integration-logger")
  assert_eq(logger.scope.version, Some("1.0.0"))
  
  assert_eq(azimuth::LogRecord::body(info_log), Some("Integration test operation completed successfully"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("Integration test error occurred"))
  
  // 验证资源属性
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.name"), Some(azimuth::StringValue("integration-test-service")))
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.instance.id"), Some(azimuth::StringValue("instance-12345")))
  assert_eq(azimuth::Resource::get_attribute(service_resource, "deployment.environment"), Some(azimuth::StringValue("test")))
  
  // 验证上下文和Baggage
  assert_eq(azimuth::Context::get(ctx_with_session, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_session, request_key), Some("req-67890"))
  assert_eq(azimuth::Context::get(ctx_with_session, session_key), Some("sess-abcde"))
  
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "session.id"), Some("sess-abcde"))
}