// 边界条件错误处理高级测试
// 测试系统在边界条件、异常情况和错误输入下的行为

test "空值和None边界条件测试" {
  // 测试空字符串处理
  let empty_string = StringValue("")
  match empty_string {
    StringValue(s) => {
      assert_eq(s.length(), 0, "空字符串长度应该为0")
      assert_eq(s, "", "空字符串应该等于空字符串")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试空数组处理
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0, "空字符串数组长度应该为0")
    _ => assert_true(false, "期望字符串数组值")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0, "空整数数组长度应该为0")
    _ => assert_true(false, "期望整数数组值")
  }
  
  // 测试None值处理
  let none_option : Option[String] = None
  assert_eq(none_option, None, "None值应该等于None")
  
  // 测试空属性集合
  let empty_attributes = Attributes::new()
  let empty_result = Attributes::get(empty_attributes, "any.key")
  assert_eq(empty_result, None, "空属性集合应该返回None")
  
  // 测试空资源
  let empty_resource = Resource::new()
  assert_eq(empty_resource.attributes.length(), 0, "空资源属性长度应该为0")
  
  let empty_resource_result = Resource::get_attribute(empty_resource, "any.attribute")
  assert_eq(empty_resource_result, None, "空资源应该返回None")
  
  // 测试空上下文
  let empty_context = Context::root()
  let empty_context_result = Context::get(empty_context, ContextKey::new("any.key"))
  assert_eq(empty_context_result, None, "空上下文应该返回None")
  
  // 测试空载体
  let empty_carrier = TextMapCarrier::new()
  let empty_carrier_result = TextMapCarrier::get(empty_carrier, "any.header")
  assert_eq(empty_carrier_result, None, "空载体应该返回None")
}

test "极值边界条件测试" {
  // 测试整数极值
  let max_int = IntValue(2147483647)
  let min_int = IntValue(-2147483648)
  let zero_int = IntValue(0)
  
  match max_int {
    IntValue(i) => assert_eq(i, 2147483647, "最大整数应该正确")
    _ => assert_true(false, "期望整数值")
  }
  
  match min_int {
    IntValue(i) => assert_eq(i, -2147483648, "最小整数应该正确")
    _ => assert_true(false, "期望整数值")
  }
  
  match zero_int {
    IntValue(i) => assert_eq(i, 0, "零整数应该正确")
    _ => assert_true(false, "期望整数值")
  }
  
  // 测试浮点数极值
  let very_large_float = FloatValue(1.7976931348623157e+308)
  let very_small_float = FloatValue(2.2250738585072014e-308)
  let zero_float = FloatValue(0.0)
  let negative_float = FloatValue(-1.23456789)
  
  match very_large_float {
    FloatValue(f) => assert_true(f > 1.0e+307, "极大浮点数应该正确")
    _ => assert_true(false, "期望浮点数值")
  }
  
  match very_small_float {
    FloatValue(f) => assert_true(f > 0.0 && f < 1.0e-307, "极小浮点数应该正确")
    _ => assert_true(false, "期望浮点数值")
  }
  
  match zero_float {
    FloatValue(f) => assert_true(f == 0.0, "零浮点数应该正确")
    _ => assert_true(false, "期望浮点数值")
  }
  
  match negative_float {
    FloatValue(f) => assert_true(f < 0.0, "负浮点数应该正确")
    _ => assert_true(false, "期望浮点数值")
  }
  
  // 测试特殊浮点值
  let infinity_float = FloatValue(1.0 / 0.0) // 正无穷
  let neg_infinity_float = FloatValue(-1.0 / 0.0) // 负无穷
  let nan_float = FloatValue(0.0 / 0.0) // NaN
  
  match infinity_float {
    FloatValue(f) => assert_true(f > 1.0e+308, "正无穷应该被识别")
    _ => assert_true(false, "期望浮点数值")
  }
  
  match neg_infinity_float {
    FloatValue(f) => assert_true(f < -1.0e+308, "负无穷应该被识别")
    _ => assert_true(false, "期望浮点数值")
  }
  
  match nan_float {
    FloatValue(f) => assert_true(f != f, "NaN应该被识别（NaN != NaN）")
    _ => assert_true(false, "期望浮点数值")
  }
  
  // 测试极长字符串
  let very_long_string = StringValue("这是一个非常长的字符串，用于测试系统对长字符串的处理能力。" * 20)
  match very_long_string {
    StringValue(s) => assert_true(s.length() > 200, "极长字符串应该被正确处理")
    _ => assert_true(false, "期望字符串值")
  }
}

test "Span上下文边界条件测试" {
  // 测试无效的Span上下文
  let invalid_trace_id = SpanContext::new("", "1234567890abcdef", true, "")
  let invalid_span_id = SpanContext::new("1234567890abcdef1234567890abcdef", "", true, "")
  let both_invalid = SpanContext::new("", "", false, "")
  
  // 验证无效上下文
  assert_false(SpanContext::is_valid(invalid_trace_id), "空trace ID的上下文应该无效")
  assert_false(SpanContext::is_valid(invalid_span_id), "空span ID的上下文应该无效")
  assert_false(SpanContext::is_valid(both_invalid), "空ID的上下文应该无效")
  
  // 测试有效的Span上下文
  let valid_context = SpanContext::new("1234567890abcdef1234567890abcdef", "1234567890abcdef", true, "key1=value1")
  assert_true(SpanContext::is_valid(valid_context), "有效上下文应该被识别为有效")
  
  // 测试采样状态
  let sampled_context = SpanContext::new("1234567890abcdef1234567890abcdef", "1234567890abcdef", true, "")
  let not_sampled_context = SpanContext::new("1234567890abcdef1234567890abcdef", "1234567890abcdef", false, "")
  
  assert_true(SpanContext::is_sampled(sampled_context), "采样上下文应该被识别为已采样")
  assert_false(SpanContext::is_sampled(not_sampled_context), "未采样上下文应该被识别为未采样")
  
  // 测试trace state边界条件
  let empty_trace_state = SpanContext::new("1234567890abcdef1234567890abcdef", "1234567890abcdef", true, "")
  let long_trace_state = SpanContext::new("1234567890abcdef1234567890abcdef", "1234567890abcdef", true, "key1=value1,key2=value2,key3=value3,key4=value4,key5=value5")
  
  assert_eq(SpanContext::trace_state(empty_trace_state), "", "空trace state应该为空字符串")
  assert_true(SpanContext::trace_state(long_trace_state).length() > 20, "长trace state应该被正确处理")
}

test "错误恢复和容错测试" {
  // 测试损坏的traceparent头处理
  let corrupted_carrier = TextMapCarrier::new()
  TextMapCarrier::set(corrupted_carrier, "traceparent", "invalid-trace-data")
  
  let propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([propagator])
  
  // 尝试从损坏数据提取
  let ctx_from_corrupted = CompositePropagator::extract(composite_propagator, corrupted_carrier)
  let extracted_from_corrupted = Context::get(ctx_from_corrupted, ContextKey::new("extracted"))
  
  // 系统应该能从损坏数据恢复
  assert_eq(extracted_from_corrupted, Some("true"), "应该能从损坏数据恢复")
  
  // 测试部分缺失的头部
  let partial_carrier = TextMapCarrier::new()
  TextMapCarrier::set(partial_carrier, "baggage", "key1=value1")
  // 故意不设置traceparent
  
  let ctx_from_partial = CompositePropagator::extract(composite_propagator, partial_carrier)
  let extracted_from_partial = Context::get(ctx_from_partial, ContextKey::new("extracted"))
  
  assert_eq(extracted_from_partial, Some("true"), "应该能处理部分缺失的头部")
  
  // 测试空载体处理
  let empty_carrier = TextMapCarrier::new()
  let ctx_from_empty = CompositePropagator::extract(composite_propagator, empty_carrier)
  let extracted_from_empty = Context::get(ctx_from_empty, ContextKey::new("extracted"))
  
  assert_eq(extracted_from_empty, Some("true"), "应该能处理空载体")
  
  // 测试包含特殊字符的头部
  let special_char_carrier = TextMapCarrier::new()
  TextMapCarrier::set(special_char_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(special_char_carrier, "baggage", "key1=value-with-dashes,key2=value_with_underscores")
  TextMapCarrier::set(special_char_carrier, "x-custom-header", "special!@#$%^&*()chars")
  
  let ctx_from_special = CompositePropagator::extract(composite_propagator, special_char_carrier)
  let extracted_from_special = Context::get(ctx_from_special, ContextKey::new("extracted"))
  
  assert_eq(extracted_from_special, Some("true"), "应该能处理包含特殊字符的头部")
}

test "内存和资源边界测试" {
  // 测试大型数组处理
  let large_string_array = ArrayStringValue([
    "item1", "item2", "item3", "item4", "item5", "item6", "item7", "item8", "item9", "item10",
    "item11", "item12", "item13", "item14", "item15", "item16", "item17", "item18", "item19", "item20"
  ])
  
  let large_int_array = ArrayIntValue([
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
  ])
  
  match large_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 20, "大型字符串数组长度应该正确")
      assert_eq(arr[0], "item1", "第一个元素应该正确")
      assert_eq(arr[19], "item20", "最后一个元素应该正确")
    }
    _ => assert_true(false, "期望字符串数组值")
  }
  
  match large_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 20, "大型整数数组长度应该正确")
      assert_eq(arr[0], 1, "第一个元素应该正确")
      assert_eq(arr[19], 20, "最后一个元素应该正确")
    }
    _ => assert_true(false, "期望整数数组值")
  }
  
  // 测试大型属性集合
  let large_attributes = Attributes::new()
  
  // 设置大量属性
  Attributes::set(large_attributes, "attr1", StringValue("value1"))
  Attributes::set(large_attributes, "attr2", IntValue(2))
  Attributes::set(large_attributes, "attr3", FloatValue(3.14))
  Attributes::set(large_attributes, "attr4", BoolValue(true))
  Attributes::set(large_attributes, "attr5", ArrayStringValue(["a", "b", "c"]))
  
  // 验证大型属性集合
  let attr1_result = Attributes::get(large_attributes, "attr1")
  let attr2_result = Attributes::get(large_attributes, "attr2")
  let attr3_result = Attributes::get(large_attributes, "attr3")
  let attr4_result = Attributes::get(large_attributes, "attr4")
  let attr5_result = Attributes::get(large_attributes, "attr5")
  
  match attr1_result {
    Some(StringValue(value)) => assert_eq(value, "value1", "大型属性集合attr1应该正确")
    _ => assert_true(false, "期望字符串值")
  }
  
  match attr2_result {
    Some(IntValue(value)) => assert_eq(value, 2, "大型属性集合attr2应该正确")
    _ => assert_true(false, "期望整数值")
  }
  
  match attr3_result {
    Some(FloatValue(value)) => assert_true(value > 3.1 && value < 3.2, "大型属性集合attr3应该正确")
    _ => assert_true(false, "期望浮点数值")
  }
  
  match attr4_result {
    Some(BoolValue(value)) => assert_true(value, "大型属性集合attr4应该正确")
    _ => assert_true(false, "期望布尔值")
  }
  
  match attr5_result {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 3, "大型属性集合attr5数组长度应该正确")
      assert_eq(arr[0], "a", "大型属性集合attr5第一个元素应该正确")
    }
    _ => assert_true(false, "期望字符串数组值")
  }
  
  // 测试大型资源
  let large_resource = Resource::new()
  let large_resource_attributes = [
    ("service.name", StringValue("large-test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("host.name", StringValue("large-test-host")),
    ("environment", StringValue("test")),
    ("region", StringValue("us-west-2")),
    ("zone", StringValue("us-west-2a")),
    ("datacenter", StringValue("dc1")),
    ("rack", StringValue("rack-01")),
    ("server", StringValue("server-001"))
  ]
  let enriched_large_resource = Resource::with_attributes(large_resource, large_resource_attributes)
  
  // 验证大型资源
  let service_name = Resource::get_attribute(enriched_large_resource, "service.name")
  let server = Resource::get_attribute(enriched_large_resource, "server")
  
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "large-test-service", "大型资源服务名称应该正确")
    _ => assert_true(false, "期望字符串值")
  }
  
  match server {
    Some(StringValue(value)) => assert_eq(value, "server-001", "大型资源服务器名称应该正确")
    _ => assert_true(false, "期望字符串值")
  }
}

test "并发和竞争边界测试" {
  // 测试并发上下文操作
  let base_context = Context::root()
  let key1 = ContextKey::new("concurrent.key1")
  let key2 = ContextKey::new("concurrent.key2")
  let key3 = ContextKey::new("concurrent.key3")
  
  // 创建多个并发上下文
  let ctx1 = Context::with_value(base_context, key1, "value1")
  let ctx2 = Context::with_value(base_context, key2, "value2")
  let ctx3 = Context::with_value(base_context, key3, "value3")
  
  // 验证并发上下文独立性
  let value1 = Context::get(ctx1, key1)
  let value2 = Context::get(ctx2, key2)
  let value3 = Context::get(ctx3, key3)
  
  assert_eq(value1, Some("value1"), "并发上下文1应该独立")
  assert_eq(value2, Some("value2"), "并发上下文2应该独立")
  assert_eq(value3, Some("value3"), "并发上下文3应该独立")
  
  // 测试并发度量操作
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "concurrent-service-1")
  let meter2 = MeterProvider::get_meter(provider, "concurrent-service-2")
  let meter3 = MeterProvider::get_meter(provider, "concurrent-service-3")
  
  let counter1 = Meter::create_counter(meter1, "concurrent.requests")
  let counter2 = Meter::create_counter(meter2, "concurrent.requests")
  let counter3 = Meter::create_counter(meter3, "concurrent.requests")
  
  // 并发记录度量
  Counter::add(counter1, 10.0)
  Counter::add(counter2, 20.0)
  Counter::add(counter3, 30.0)
  
  // 验证并发度量独立性
  assert_eq(counter1.name, counter2.name, "并发计数器名称应该一致")
  assert_eq(counter2.name, counter3.name, "并发计数器名称应该一致")
  assert_eq(counter1.name, "concurrent.requests", "计数器名称应该正确")
  
  // 测试并发日志操作
  let logger_provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(logger_provider, "concurrent-logger-1")
  let logger2 = LoggerProvider::get_logger(logger_provider, "concurrent-logger-2")
  let logger3 = LoggerProvider::get_logger(logger_provider, "concurrent-logger-3")
  
  let timestamp = 1735689600000000000L
  
  let log1 = LogRecord::new_with_context(
    Info, Some("Concurrent log 1"), None,
    Some(timestamp), Some(timestamp),
    Some("trace-1111"), Some("span-1111"),
    Some(ctx1)
  )
  
  let log2 = LogRecord::new_with_context(
    Info, Some("Concurrent log 2"), None,
    Some(timestamp + 1000000L), Some(timestamp + 1000000L),
    Some("trace-2222"), Some("span-2222"),
    Some(ctx2)
  )
  
  let log3 = LogRecord::new_with_context(
    Info, Some("Concurrent log 3"), None,
    Some(timestamp + 2000000L), Some(timestamp + 2000000L),
    Some("trace-3333"), Some("span-3333"),
    Some(ctx3)
  )
  
  // 并发发送日志
  Logger::emit(logger1, log1)
  Logger::emit(logger2, log2)
  Logger::emit(logger3, log3)
  
  // 验证并发日志独立性
  assert_eq(LogRecord::trace_id(log1), Some("trace-1111"), "并发日志1追踪ID应该独立")
  assert_eq(LogRecord::trace_id(log2), Some("trace-2222"), "并发日志2追踪ID应该独立")
  assert_eq(LogRecord::trace_id(log3), Some("trace-3333"), "并发日志3追踪ID应该独立")
  
  assert_eq(LogRecord::span_id(log1), Some("span-1111"), "并发日志1 span ID应该独立")
  assert_eq(LogRecord::span_id(log2), Some("span-2222"), "并发日志2 span ID应该独立")
  assert_eq(LogRecord::span_id(log3), Some("span-3333"), "并发日志3 span ID应该独立")
}

test "输入验证和清理测试" {
  // 测试包含特殊字符的字符串输入
  let special_char_string = StringValue("Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  match special_char_string {
    StringValue(s) => {
      assert_true(s.contains("!@#$%^&*()"), "特殊字符字符串应该包含特殊字符")
      assert_eq(s.length(), 47, "特殊字符字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试Unicode字符串输入
  let unicode_string = StringValue("Unicode: 测试 🚀 العربية русский עברית")
  match unicode_string {
    StringValue(s) => {
      assert_true(s.contains("测试"), "Unicode字符串应该包含中文")
      assert_true(s.contains("🚀"), "Unicode字符串应该包含emoji")
      assert_true(s.contains("العربية"), "Unicode字符串应该包含阿拉伯文")
      assert_true(s.contains("русский"), "Unicode字符串应该包含俄文")
      assert_true(s.contains("עברית"), "Unicode字符串应该包含希伯来文")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试包含控制字符的字符串输入
  let control_char_string = StringValue("Control chars: 
	")
  match control_char_string {
    StringValue(s) => {
      assert_true(s.contains("
"), "控制字符字符串应该包含换行符")
      assert_true(s.contains("	"), "控制字符字符串应该包含制表符")
      assert_true(s.contains(""), "控制字符字符串应该包含回车符")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试包含空格的字符串输入
  let whitespace_string = StringValue("   leading and trailing spaces   ")
  match whitespace_string {
    StringValue(s) => {
      assert_true(s.starts_with("   "), "字符串应该以前导空格开始")
      assert_true(s.ends_with("   "), "字符串应该以尾随空格结束")
      assert_eq(s.length(), 35, "包含空格的字符串长度应该正确")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试包含HTML/XML特殊字符的字符串输入
  let html_string = StringValue("HTML chars: <script>alert('xss')</script> &amp; &lt; &gt; &quot;")
  match html_string {
    StringValue(s) => {
      assert_true(s.contains("<script>"), "HTML字符串应该包含script标签")
      assert_true(s.contains("&amp;"), "HTML字符串应该包含HTML实体")
      assert_true(s.contains("&lt;"), "HTML字符串应该包含小于号实体")
      assert_true(s.contains("&gt;"), "HTML字符串应该包含大于号实体")
      assert_true(s.contains("&quot;"), "HTML字符串应该包含引号实体")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试包含SQL注入尝试的字符串输入
  let sql_injection_string = StringValue("SQL injection: '; DROP TABLE users; --")
  match sql_injection_string {
    StringValue(s) => {
      assert_true(s.contains("DROP TABLE"), "SQL注入字符串应该包含恶意SQL")
      assert_true(s.contains("--"), "SQL注入字符串应该包含SQL注释")
    }
    _ => assert_true(false, "期望字符串值")
  }
  
  // 测试路径遍历攻击字符串输入
  let path_traversal_string = StringValue("Path traversal: ../../etc/passwd")
  match path_traversal_string {
    StringValue(s) => {
      assert_true(s.contains("../"), "路径遍历字符串应该包含相对路径")
      assert_true(s.contains("etc/passwd"), "路径遍历字符串应该包含系统文件路径")
    }
    _ => assert_true(false, "期望字符串值")
  }
}