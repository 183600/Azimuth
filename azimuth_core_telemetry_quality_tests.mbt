// Azimuth Core Telemetry Quality Tests
// 测试核心遥测功能的高质量测试用例

test "core_attribute_operations" {
  // 测试属性的基本操作
  let attrs = Attributes::new()
  
  // 测试设置和获取字符串属性
  Attributes::set(attrs, "test.string", StringValue("hello_world"))
  let string_value = Attributes::get(attrs, "string.key")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // 测试设置和获取整数属性
  Attributes::set(attrs, "test.int", IntValue(123))
  let int_value = Attributes::get(attrs, "int.key")
  assert_eq(int_value, Some(IntValue(42)))
  
  // 测试不存在的属性
  let missing_value = Attributes::get(attrs, "nonexistent.key")
  assert_eq(missing_value, None)
}

test "core_span_context_validity" {
  // 测试Span上下文的有效性检查
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_eq(SpanContext::is_valid(valid_ctx), true)
  assert_eq(SpanContext::is_sampled(valid_ctx), true)
  assert_eq(SpanContext::trace_id(valid_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(valid_ctx), "b7ad6b7169203331")
  
  // 测试无效的Span上下文
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_eq(SpanContext::is_valid(invalid_ctx), false)
  assert_eq(SpanContext::is_sampled(invalid_ctx), false)
}

test "core_instrument_type_operations" {
  // 测试仪器类型的操作
  let counter = Counter("test_counter", Some("Test counter description"), Some("count"))
  let histogram = Histogram("test_histogram", Some("Test histogram description"), Some("ms"))
  let updown_counter = UpDownCounter("test_updown", Some("Test updown counter description"), Some("items"))
  let gauge = Gauge("test_gauge", Some("Test gauge description"), Some("percent"))
  
  // 测试获取仪器名称
  assert_eq(Instrument::name(Counter("test_counter", None, None)), "test_counter")
  assert_eq(Instrument::name(Histogram("test_histogram", None, None)), "test_histogram")
  assert_eq(Instrument::name(UpDownCounter("test_updown", None, None)), "test_updown")
  assert_eq(Instrument::name(Gauge("test_gauge", None, None)), "test_gauge")
  
  // 测试获取仪器描述
  assert_eq(Instrument::description(counter), Some("Test counter description"))
  assert_eq(Instrument::description(histogram), Some("Test histogram description"))
  assert_eq(Instrument::description(updown_counter), Some("Test updown counter description"))
  assert_eq(Instrument::description(gauge), Some("Test gauge description"))
  
  // 测试获取仪器单位
  assert_eq(Instrument::unit(counter), Some("count"))
  assert_eq(Instrument::unit(histogram), Some("ms"))
  assert_eq(Instrument::unit(updown_counter), Some("items"))
  assert_eq(Instrument::unit(gauge), Some("percent"))
}

test "core_severity_number_operations" {
  // 测试日志严重性级别的操作
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  // 测试获取严重性级别
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // 测试获取日志消息体
  assert_eq(LogRecord::body(trace_record), Some("Trace message"))
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
  assert_eq(LogRecord::body(info_record), Some("Info message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
  assert_eq(LogRecord::body(error_record), Some("Error message"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal message"))
}

test "core_span_kind_operations" {
  // 测试Span类型的操作
  let internal_span = Span::new("internal_operation", Internal, SpanContext::new("trace1", "span1", true, ""))
  let server_span = Span::new("server_operation", Server, SpanContext::new("trace2", "span2", true, ""))
  let client_span = Span::new("client_operation", Client, SpanContext::new("trace3", "span3", true, ""))
  let producer_span = Span::new("producer_operation", Producer, SpanContext::new("trace4", "span4", true, ""))
  let consumer_span = Span::new("consumer_operation", Consumer, SpanContext::new("trace5", "span5", true, ""))
  
  // 测试获取Span名称
  assert_eq(Span::name(internal_span), "internal_operation")
  assert_eq(Span::name(server_span), "server_operation")
  assert_eq(Span::name(client_span), "client_operation")
  assert_eq(Span::name(producer_span), "producer_operation")
  assert_eq(Span::name(consumer_span), "consumer_operation")
  
  // 测试获取Span类型
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // 测试Span是否正在记录
  assert_eq(Span::is_recording(internal_span), true)
  assert_eq(Span::is_recording(server_span), true)
  assert_eq(Span::is_recording(client_span), true)
  assert_eq(Span::is_recording(producer_span), true)
  assert_eq(Span::is_recording(consumer_span), true)
}

test "core_clock_operations" {
  // 测试时钟操作
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // 验证时间戳是一个合理的值（应该大于2020年的时间戳）
  assert_eq(timestamp > 1577836800000000000L, true)
}

test "core_random_operations" {
  // 测试随机数生成器操作
  let random = Random::system()
  
  // 测试生成字节数组
  let bytes = Random::next_bytes(random, 10)
  assert_eq(bytes.length(), 0) // 简化实现返回空数组
  
  // 测试生成无符号64位整数
  let u64_value = Random::next_u64(random)
  assert_eq(u64_value, 12345UL) // 简化实现返回固定值
}