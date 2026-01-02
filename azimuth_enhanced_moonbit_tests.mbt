// Azimuth Enhanced MoonBit Test Suite
// 增强的MoonBit测试用例，专注于遥测系统的高级功能和边界情况

// 测试1: 属性值类型转换
test "属性值类型转换测试" {
  // 测试字符串属性
  let string_attr = StringValue("test-string")
  assert_eq(string_attr, StringValue("test-string"))
  
  // 测试整数属性
  let int_attr = IntValue(42)
  assert_eq(int_attr, IntValue(42))
  
  // 测试浮点数属性
  let float_attr = FloatValue(3.14)
  assert_eq(float_attr, FloatValue(3.14))
  
  // 测试布尔属性
  let bool_attr = BoolValue(true)
  assert_eq(bool_attr, BoolValue(true))
  
  // 测试数组属性
  let string_array_attr = ArrayStringValue(["a", "b", "c"])
  assert_eq(string_array_attr, ArrayStringValue(["a", "b", "c"]))
  
  let int_array_attr = ArrayIntValue([1, 2, 3])
  assert_eq(int_array_attr, ArrayIntValue([1, 2, 3]))
}

// 测试2: 属性操作
test "属性操作测试" {
  // 创建属性集合
  let attrs = Attributes::new()
  
  // 设置属性
  Attributes::set(attrs, "string.key", StringValue("test-value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  
  // 获取属性
  let string_result = Attributes::get(attrs, "string.key")
  let int_result = Attributes::get(attrs, "int.key")
  let missing_result = Attributes::get(attrs, "missing.key")
  
  // 验证结果
  assert_eq(string_result, Some(StringValue("test_value")))
  assert_eq(int_result, Some(IntValue(42)))
  assert_eq(missing_result, None)
}

// 测试3: 资源操作
test "资源操作测试" {
  // 创建基础资源
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("development"))
  ])
  
  // 创建覆盖资源
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("2.0.0")),  // 覆盖版本
    ("deployment.region", StringValue("us-west-1")),  // 新增属性
    ("environment", StringValue("production"))  // 覆盖环境
  ])
  
  // 合并资源
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), 
    Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), 
    Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "environment"), 
    Some(StringValue("production")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.region"), 
    Some(StringValue("us-west-1")))
}

// 测试4: Span上下文操作
test "Span上下文操作测试" {
  // 创建Span上下文
  let trace_id = "trace-123456789"
  let span_id = "span-987654321"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // 验证Span上下文属性
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_eq(SpanContext::is_sampled(span_ctx), true)
  assert_eq(SpanContext::trace_state(span_ctx), "key1=value1")
  
  // 测试Span创建
  let span = Span::new("test-span", Internal, span_ctx)
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
}

// 测试5: 度量类型操作
test "度量类型操作测试" {
  // 创建度量提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // 创建计数器
  let counter = Meter::create_counter(meter, "test.counter")
  assert_eq(Counter::name(counter), "test.counter")
  
  // 创建直方图
  let histogram = Meter::create_histogram(meter, "test.histogram")
  assert_eq(Histogram::name(histogram), "test.histogram")
  
  // 创建上下行计数器
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  assert_eq(UpDownCounter::name(updown_counter), "test.updown")
  
  // 创建仪表
  let gauge = Meter::create_gauge(meter, "test.gauge")
  assert_eq(Gauge::name(gauge), "test.gauge")
  
  // 测试度量操作
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 100.0)
  UpDownCounter::add(updown_counter, 5.0)
  
  // 验证操作成功（没有崩溃）
  assert_true(true)
}

// 测试6: 日志记录操作
test "日志记录操作测试" {
  // 创建日志记录
  let info_log = LogRecord::new(Info, "Information message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  
  // 验证日志记录
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::body(info_log), Some("Information message"))
  
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::body(warn_log), Some("Warning message"))
  
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::body(error_log), Some("Error message"))
  
  // 测试带上下文的日志记录
  let log_with_context = LogRecord::new_with_context(
    Info,
    Some("Log with context"),
    None,
    Some(1640995200000000000L),  // 时间戳
    None,
    Some("trace-123"),
    Some("span-456"),
    None
  )
  
  // 验证上下文日志记录
  assert_eq(LogRecord::trace_id(log_with_context), Some("trace-123"))
  assert_eq(LogRecord::span_id(log_with_context), Some("span-456"))
}

// 测试7: 上下文传播
test "上下文传播测试" {
  // 创建根上下文
  let root_ctx = Context::root()
  
  // 创建上下文键
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  
  // 设置上下文值
  let ctx_with_value1 = Context::with_value(root_ctx, key1, "value1")
  let ctx_with_values = Context::with_value(ctx_with_value1, key2, "value2")
  
  // 获取上下文值
  let value1 = Context::get(ctx_with_values, key1)
  let value2 = Context::get(ctx_with_values, key2)
  let missing_value = Context::get(ctx_with_values, ContextKey::new("missing"))
  
  // 验证上下文值
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
  assert_eq(missing_value, None)
}

// 测试8: Baggage操作
test "Baggage操作测试" {
  // 创建Baggage
  let baggage = Baggage::new()
  
  // 设置Baggage条目
  let baggage_with_entry1 = Baggage::set_entry(baggage, "key1", "value1")
  let baggage_with_entries = Baggage::set_entry(baggage_with_entry1, "key2", "value2")
  
  // 获取Baggage条目
  let entry1 = Baggage::get_entry(baggage_with_entries, "key1")
  let entry2 = Baggage::get_entry(baggage_with_entries, "key2")
  let missing_entry = Baggage::get_entry(baggage_with_entries, "missing")
  
  // 验证Baggage条目
  assert_eq(entry1, Some("value1"))
  assert_eq(entry2, Some("value2"))
  assert_eq(missing_entry, None)
  
  // 删除Baggage条目
  let baggage_reduced = Baggage::remove_entry(baggage_with_entries, "key1")
  let removed_entry = Baggage::get_entry(baggage_reduced, "key1")
  let remaining_entry = Baggage::get_entry(baggage_reduced, "key2")
  
  // 验证删除结果
  assert_eq(removed_entry, None)
  assert_eq(remaining_entry, Some("value2"))
}