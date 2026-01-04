// Azimuth 数据验证和完整性测试用例
// 专注于测试系统的数据验证和完整性检查能力

// 测试1: 指标数据验证测试
test "指标数据验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "validation.test")
  
  // 测试计数器验证
  let counter = Meter::create_counter(meter, "validation.counter")
  
  // 验证正数输入
  Counter::add(counter, 1.0)
  Counter::add(counter, 100.0)
  Counter::add(counter, 0.0)
  assert_eq(Counter::value(counter), 101.0)
  
  // 验证浮点数精度
  Counter::add(counter, 0.1)
  Counter::add(counter, 0.2)
  Counter::add(counter, 0.3)
  let expected_value = 101.6  // 101.0 + 0.1 + 0.2 + 0.3
  assert_true(abs(Counter::value(counter) - expected_value) < 0.0001)
  
  // 测试直方图验证
  let histogram = Meter::create_histogram(meter, "validation.histogram")
  
  // 验证正值输入
  Histogram::record(histogram, 0.0)
  Histogram::record(histogram, 1.0)
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 0.001)
  
  // 验证边界值
  Histogram::record(histogram, 3.402823466e+38)  // 接近f32最大值
  Histogram::record(histogram, 1.175494351e-38)  // 接近f32最小正值
  
  // 测试仪表验证
  let gauge = Meter::create_gauge(meter, "validation.gauge")
  
  // 验证正负值
  Gauge::set(gauge, 100.0)
  assert_eq(Gauge::value(gauge), 100.0)
  
  Gauge::set(gauge, -100.0)
  assert_eq(Gauge::value(gauge), -100.0)
  
  Gauge::set(gauge, 0.0)
  assert_eq(Gauge::value(gauge), 0.0)
}

// 测试2: 属性数据验证测试
test "属性数据验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "attribute.validation.test")
  
  let counter = Meter::create_counter(meter, "attr.validation.counter")
  
  // 测试字符串属性验证
  Counter::add(counter, 1.0, [("string.attr", StringValue("valid_string"))])
  Counter::add(counter, 1.0, [("empty.string.attr", StringValue(""))])
  Counter::add(counter, 1.0, [("unicode.string.attr", StringValue("测试中文🚀"))])
  
  // 测试整数属性验证
  Counter::add(counter, 1.0, [("int.attr", IntValue(42))])
  Counter::add(counter, 1.0, [("max.int.attr", IntValue(2147483647))])
  Counter::add(counter, 1.0, [("min.int.attr", IntValue(-2147483648))])
  Counter::add(counter, 1.0, [("zero.int.attr", IntValue(0))])
  
  // 测试浮点属性验证
  Counter::add(counter, 1.0, [("float.attr", FloatValue(3.14))])
  Counter::add(counter, 1.0, [("max.float.attr", FloatValue(3.402823466e+38))])
  Counter::add(counter, 1.0, [("min.float.attr", FloatValue(-3.402823466e+38))])
  Counter::add(counter, 1.0, [("zero.float.attr", FloatValue(0.0))])
  
  // 测试布尔属性验证
  Counter::add(counter, 1.0, [("true.bool.attr", BoolValue(true))])
  Counter::add(counter, 1.0, [("false.bool.attr", BoolValue(false))])
  
  // 验证计数器值
  assert_eq(Counter::value(counter), 11.0)
}

// 测试3: Span数据完整性验证测试
test "Span数据完整性验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "integrity.test")
  
  // 创建span并验证数据完整性
  let span = Tracer::start_span(tracer, "integrity.test.span")
  
  // 验证span基本属性
  assert_eq(Span::name(span), "integrity.test.span")
  assert_true(Span::is_recording(span))
  
  // 设置属性并验证
  Span::set_attribute(span, "string.key", StringValue("string.value"))
  Span::set_attribute(span, "int.key", IntValue(42))
  Span::set_attribute(span, "float.key", FloatValue(3.14))
  Span::set_attribute(span, "bool.key", BoolValue(true))
  
  // 验证属性完整性
  let string_value = Span::get_attribute(span, "string.key")
  let int_value = Span::get_attribute(span, "int.key")
  let float_value = Span::get_attribute(span, "float.key")
  let bool_value = Span::get_attribute(span, "bool.key")
  
  match string_value {
    Some(StringValue(s)) => assert_eq(s, "string.value")
    _ => assert_true(false)
  }
  
  match int_value {
    Some(IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  match float_value {
    Some(FloatValue(f)) => assert_true(abs(f - 3.14) < 0.001)
    _ => assert_true(false)
  }
  
  match bool_value {
    Some(BoolValue(b)) => assert_true(b)
    _ => assert_true(false)
  }
  
  // 添加事件并验证
  Span::add_event(span, "test.event", Some([("event.attr", StringValue("event.value"))]))
  
  // 设置状态并验证
  Span::set_status(span, Ok)
  assert_eq(Span::status(span), Ok)
  
  Span::set_status(span, Error, Some("Test error"))
  assert_eq(Span::status(span), Error)
  
  // 结束span并验证
  Span::end(span)
  assert_false(Span::is_recording(span))
}

// 测试4: 日志数据验证测试
test "日志数据验证测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "log.validation.test")
  
  // 测试不同严重级别的日志
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  
  // 验证严重级别
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  
  // 验证日志消息
  assert_eq(LogRecord::body(debug_log), Some("Debug message"))
  assert_eq(LogRecord::body(info_log), Some("Info message"))
  assert_eq(LogRecord::body(warn_log), Some("Warning message"))
  assert_eq(LogRecord::body(error_log), Some("Error message"))
  
  // 测试带有完整上下文的日志
  let full_context_log = LogRecord::new_with_context(
    Error,
    Some("Full context message"),
    Some("Error details"),
    Some(1234567890L),
    Some(1234567891L),
    Some("trace123"),
    Some("span123"),
    Some(Context::root())
  )
  
  // 验证完整上下文
  assert_eq(LogRecord::severity_number(full_context_log), Error)
  assert_eq(LogRecord::body(full_context_log), Some("Full context message"))
  assert_eq(LogRecord::trace_id(full_context_log), Some("trace123"))
  assert_eq(LogRecord::span_id(full_context_log), Some("span123"))
  
  // 测试日志属性
  LogRecord::set_attribute(full_context_log, "log.attr", StringValue("log.value"))
  let log_attr = LogRecord::get_attribute(full_context_log, "log.attr")
  
  match log_attr {
    Some(StringValue(s)) => assert_eq(s, "log.value")
    _ => assert_true(false)
  }
  
  // 发送所有日志
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, full_context_log)
}

// 测试5: 上下文数据验证测试
test "上下文数据验证测试" {
  // 创建根上下文
  let root_ctx = Context::root()
  
  // 验证根上下文
  assert_true(Context::is_root(root_ctx))
  
  // 添加追踪上下文
  let trace_ctx = SpanContext::new("trace123", "span456", true, "key1=value1")
  let ctx_with_trace = Context::with_span_context(root_ctx, trace_ctx)
  
  // 验证追踪上下文
  let extracted_trace_ctx = Context::span_context(ctx_with_trace)
  assert_eq(extracted_trace_ctx.trace_id, "trace123")
  assert_eq(extracted_trace_ctx.span_id, "span456")
  assert_true(extracted_trace_ctx.is_sampled)
  
  // 添加行李项
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let request_key = ContextKey::new("request.id")
  
  let ctx_with_user = Context::with_value(ctx_with_trace, user_key, "user123")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session456")
  let ctx_with_request = Context::with_value(ctx_with_session, request_key, "request789")
  
  // 验证行李项
  let user_id = Context::get(ctx_with_request, user_key)
  let session_id = Context::get(ctx_with_request, session_key)
  let request_id = Context::get(ctx_with_request, request_key)
  
  assert_eq(user_id, Some("user123"))
  assert_eq(session_id, Some("session456"))
  assert_eq(request_id, Some("request789"))
  
  // 测试上下文传播
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  
  // 注入上下文
  CompositePropagator::inject(propagator, ctx_with_request, carrier)
  
  // 提取上下文
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // 验证传播结果
  let extracted_user_id = Context::get(extracted_ctx, user_key)
  let extracted_session_id = Context::get(extracted_ctx, session_key)
  let extracted_request_id = Context::get(extracted_ctx, request_key)
  
  assert_eq(extracted_user_id, Some("user123"))
  assert_eq(extracted_session_id, Some("session456"))
  assert_eq(extracted_request_id, Some("request789"))
}

// 测试6: 资源数据验证测试
test "资源数据验证测试" {
  // 创建基础资源
  let resource = Resource::new()
  
  // 验证空资源
  assert_eq(Resource::attributes_count(resource), 0)
  
  // 添加服务属性
  let service_attrs = [
    ("service.name", StringValue("test.service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance123"))
  ]
  
  let service_resource = Resource::with_attributes(resource, service_attrs)
  
  // 验证服务资源
  assert_eq(Resource::attributes_count(service_resource), 3)
  assert_eq(Resource::get_attribute(service_resource, "service.name"), Some(StringValue("test.service")))
  assert_eq(Resource::get_attribute(service_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(service_resource, "service.instance.id"), Some(StringValue("instance123")))
  
  // 添加主机属性
  let host_attrs = [
    ("host.name", StringValue("test-host")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux"))
  ]
  
  let host_resource = Resource::with_attributes(service_resource, host_attrs)
  
  // 验证合并后的资源
  assert_eq(Resource::attributes_count(host_resource), 6)
  assert_eq(Resource::get_attribute(host_resource, "service.name"), Some(StringValue("test.service")))
  assert_eq(Resource::get_attribute(host_resource, "host.name"), Some(StringValue("test-host")))
  
  // 测试资源合并
  let resource1 = Resource::with_attributes(Resource::new(), [("key1", StringValue("value1"))])
  let resource2 = Resource::with_attributes(Resource::new(), [("key2", StringValue("value2"))])
  
  let merged_resource = Resource::merge(resource1, resource2)
  
  // 验证合并结果
  assert_eq(Resource::attributes_count(merged_resource), 2)
  assert_eq(Resource::get_attribute(merged_resource, "key1"), Some(StringValue("value1")))
  assert_eq(Resource::get_attribute(merged_resource, "key2"), Some(StringValue("value2")))
}

// 测试7: 序列化数据完整性验证测试
test "序列化数据完整性验证测试" {
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "serialization.test")
  
  // 创建测试span
  let span = Tracer::start_span(tracer, "serialization.test.span")
  
  // 添加各种类型的属性
  Span::set_attribute(span, "string.attr", StringValue("test.string"))
  Span::set_attribute(span, "int.attr", IntValue(42))
  Span::set_attribute(span, "float.attr", FloatValue(3.14))
  Span::set_attribute(span, "bool.attr", BoolValue(true))
  
  // 添加事件
  Span::add_event(span, "test.event", Some([("event.attr", StringValue("event.value"))]))
  
  // 设置状态
  Span::set_status(span, Ok)
  
  // 序列化span
  let serialized_data = Span::serialize(span)
  
  // 验证序列化数据不为空
  assert_true(serialized_data.length() > 0)
  
  // 反序列化span
  let deserialized_span = Span::deserialize(serialized_data)
  
  // 验证反序列化结果
  assert_eq(Span::name(deserialized_span), Span::name(span))
  assert_eq(Span::kind(deserialized_span), Span::kind(span))
  assert_eq(Span::status(deserialized_span), Span::status(span))
  
  // 验证属性完整性
  let original_string = Span::get_attribute(span, "string.attr")
  let deserialized_string = Span::get_attribute(deserialized_span, "string.attr")
  assert_eq(original_string, deserialized_string)
  
  let original_int = Span::get_attribute(span, "int.attr")
  let deserialized_int = Span::get_attribute(deserialized_span, "int.attr")
  assert_eq(original_int, deserialized_int)
  
  let original_float = Span::get_attribute(span, "float.attr")
  let deserialized_float = Span::get_attribute(deserialized_span, "float.attr")
  assert_eq(original_float, deserialized_float)
  
  let original_bool = Span::get_attribute(span, "bool.attr")
  let deserialized_bool = Span::get_attribute(deserialized_span, "bool.attr")
  assert_eq(original_bool, deserialized_bool)
  
  // 结束原始span
  Span::end(span)
}

// 测试8: 时间戳数据验证测试
test "时间戳数据验证测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "timestamp.validation.test")
  
  // 获取当前时间
  let current_time = Clock::now_unix_nanos(Clock::system())
  
  // 验证当前时间合理性
  assert_true(current_time > 0L)
  
  // 创建带有时间戳的日志记录
  let log_with_timestamp = LogRecord::new_with_context(
    Info,
    Some("Timestamp validation test"),
    None,
    Some(current_time),
    None,
    Some("trace123"),
    Some("span123"),
    Some(Context::root())
  )
  
  // 验证时间戳
  assert_eq(LogRecord::timestamp(log_with_timestamp), Some(current_time))
  
  // 测试边界时间戳
  let epoch_time = 0L
  let log_with_epoch = LogRecord::new_with_context(
    Info,
    Some("Epoch time test"),
    None,
    Some(epoch_time),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  assert_eq(LogRecord::timestamp(log_with_epoch), Some(epoch_time))
  
  // 测试最大时间戳
  let max_time = 9223372036854775807L  // i64最大值
  let log_with_max = LogRecord::new_with_context(
    Info,
    Some("Max time test"),
    None,
    Some(max_time),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  assert_eq(LogRecord::timestamp(log_with_max), Some(max_time))
  
  // 测试负时间戳
  let negative_time = -1000000L
  let log_with_negative = LogRecord::new_with_context(
    Info,
    Some("Negative time test"),
    None,
    Some(negative_time),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  assert_eq(LogRecord::timestamp(log_with_negative), Some(negative_time))
  
  // 发送所有日志
  Logger::emit(logger, log_with_timestamp)
  Logger::emit(logger, log_with_epoch)
  Logger::emit(logger, log_with_max)
  Logger::emit(logger, log_with_negative)
}

// 测试9: 数据类型转换验证测试
test "数据类型转换验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "type.conversion.test")
  
  let counter = Meter::create_counter(meter, "conversion.counter")
  
  // 测试整数到浮点数转换
  let int_value = 42
  Counter::add(counter, int_value.to_float())
  assert_eq(Counter::value(counter), 42.0)
  
  // 测试字符串到整数转换
  let string_int = "123"
  let parsed_int = string_int.parse_int()
  match parsed_int {
    Some(i) => {
      Counter::add(counter, i.to_float())
      assert_eq(Counter::value(counter), 165.0)  // 42.0 + 123.0
    }
    None => assert_true(false)
  }
  
  // 测试字符串到浮点数转换
  let string_float = "3.14"
  let parsed_float = string_float.parse_float()
  match parsed_float {
    Some(f) => {
      Counter::add(counter, f)
      assert_true(abs(Counter::value(counter) - 168.14) < 0.001)  // 165.0 + 3.14
    }
    None => assert_true(false)
  }
  
  // 测试布尔值到整数转换
  let bool_true = true
  let bool_false = false
  Counter::add(counter, bool_true.to_int().to_float())
  Counter::add(counter, bool_false.to_int().to_float())
  assert_eq(Counter::value(counter), 169.14)  // 168.14 + 1.0 + 0.0
  
  // 测试无效字符串转换
  let invalid_string = "invalid_number"
  let invalid_int = invalid_string.parse_int()
  let invalid_float = invalid_string.parse_float()
  
  assert_eq(invalid_int, None)
  assert_eq(invalid_float, None)
}

// 测试10: 数据一致性验证测试
test "数据一致性验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "consistency.test")
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "consistency.test")
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "consistency.test")
  
  // 创建一致性测试场景
  let test_id = "consistency.test.123"
  let test_timestamp = Clock::now_unix_nanos(Clock::system())
  
  // 在指标中记录测试信息
  let counter = Meter::create_counter(meter, "consistency.counter")
  Counter::add(counter, 1.0, [
    ("test.id", StringValue(test_id)),
    ("test.timestamp", IntValue(test_timestamp)),
    ("test.type", StringValue("consistency.validation"))
  ])
  
  // 在span中记录相同信息
  let span = Tracer::start_span(tracer, "consistency.test.span")
  Span::set_attribute(span, "test.id", StringValue(test_id))
  Span::set_attribute(span, "test.timestamp", IntValue(test_timestamp))
  Span::set_attribute(span, "test.type", StringValue("consistency.validation"))
  
  // 在日志中记录相同信息
  let log_record = LogRecord::new(Info, "Consistency test for " + test_id)
  LogRecord::set_attribute(log_record, "test.id", StringValue(test_id))
  LogRecord::set_attribute(log_record, "test.timestamp", IntValue(test_timestamp))
  LogRecord::set_attribute(log_record, "test.type", StringValue("consistency.validation"))
  
  // 验证数据一致性
  let counter_attrs = Counter::get_attributes(counter)
  let span_attrs = Span::get_all_attributes(span)
  let log_attrs = LogRecord::get_all_attributes(log_record)
  
  // 验证test.id一致性
  assert_eq(counter_attrs.get("test.id"), Some(StringValue(test_id)))
  assert_eq(span_attrs.get("test.id"), Some(StringValue(test_id)))
  assert_eq(log_attrs.get("test.id"), Some(StringValue(test_id)))
  
  // 验证test.timestamp一致性
  assert_eq(counter_attrs.get("test.timestamp"), Some(IntValue(test_timestamp)))
  assert_eq(span_attrs.get("test.timestamp"), Some(IntValue(test_timestamp)))
  assert_eq(log_attrs.get("test.timestamp"), Some(IntValue(test_timestamp)))
  
  // 验证test.type一致性
  assert_eq(counter_attrs.get("test.type"), Some(StringValue("consistency.validation")))
  assert_eq(span_attrs.get("test.type"), Some(StringValue("consistency.validation")))
  assert_eq(log_attrs.get("test.type"), Some(StringValue("consistency.validation")))
  
  // 结束span和发送日志
  Span::end(span)
  Logger::emit(logger, log_record)
  
  // 验证最终状态
  assert_eq(Counter::value(counter), 1.0)
  assert_false(Span::is_recording(span))
}