// Azimuth Error Boundary Handling Tests
// 测试错误边界处理功能

test "attribute value error handling" {
  // 测试空字符串属性值
  let empty_string_attr = @azimuth.StringValue("")
  match empty_string_attr {
    @azimuth.StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
  
  // 测试极大整数值
  let max_int = 2147483647
  let max_int_attr = @azimuth.IntValue(max_int)
  match max_int_attr {
    @azimuth.IntValue(v) => assert_eq(v, 2147483647)
    _ => assert_true(false)
  }
  
  // 测试极小整数值
  let min_int = -2147483648
  let min_int_attr = @azimuth.IntValue(min_int)
  match min_int_attr {
    @azimuth.IntValue(v) => assert_eq(v, -2147483648)
    _ => assert_true(false)
  }
  
  // 测试极大浮点数值
  let max_float = 1.7976931348623157e+308
  let max_float_attr = @azimuth.FloatValue(max_float)
  match max_float_attr {
    @azimuth.FloatValue(v) => assert_eq(v, 1.7976931348623157e+308)
    _ => assert_true(false)
  }
  
  // 测试极小浮点数值
  let min_float = 2.2250738585072014e-308
  let min_float_attr = @azimuth.FloatValue(min_float)
  match min_float_attr {
    @azimuth.FloatValue(v) => assert_eq(v, 2.2250738585072014e-308)
    _ => assert_true(false)
  }
}

test "array boundary error handling" {
  // 测试空数组
  let empty_string_array = @azimuth.ArrayStringValue([])
  match empty_string_array {
    @azimuth.ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  let empty_int_array = @azimuth.ArrayIntValue([])
  match empty_int_array {
    @azimuth.ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  // 测试单元素数组
  let single_string_array = @azimuth.ArrayStringValue(["only_item"])
  match single_string_array {
    @azimuth.ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "only_item")
    }
    _ => assert_true(false)
  }
  
  let single_int_array = @azimuth.ArrayIntValue([42])
  match single_int_array {
    @azimuth.ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42)
    }
    _ => assert_true(false)
  }
  
  // 测试大数组
  let large_string_array = @azimuth.ArrayStringValue(["item1", "item2", "item3", "item4", "item5"])
  match large_string_array {
    @azimuth.ArrayStringValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "item1")
      assert_eq(arr[4], "item5")
    }
    _ => assert_true(false)
  }
  
  let large_int_array = @azimuth.ArrayIntValue([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  match large_int_array {
    @azimuth.ArrayIntValue(arr) => {
      assert_eq(arr.length(), 10)
      assert_eq(arr[0], 1)
      assert_eq(arr[9], 10)
    }
    _ => assert_true(false)
  }
}

test "span context validation error handling" {
  // 测试无效的trace_id长度
  let short_trace_id = "123"
  let invalid_span_context = @azimuth.SpanContext {
    trace_id : short_trace_id,
    span_id : "1234567890abcdef",
    sampled : true,
    trace_state : "test=value"
  }
  
  // 验证系统可以处理短trace_id（在实际实现中应该进行验证）
  assert_eq(invalid_span_context.trace_id, "123")
  
  // 测试无效的span_id长度
  let short_span_id = "123"
  let invalid_span_id_context = @azimuth.SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : short_span_id,
    sampled : true,
    trace_state : "test=value"
  }
  
  // 验证系统可以处理短span_id（在实际实现中应该进行验证）
  assert_eq(invalid_span_id_context.span_id, "123")
  
  // 测试空trace_state
  let empty_trace_state_context = @azimuth.SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1234567890abcdef",
    sampled : true,
    trace_state : ""
  }
  
  assert_eq(empty_trace_state_context.trace_state, "")
}

test "baggage error handling" {
  // 测试空baggage
  let empty_baggage = @azimuth.Baggage { entries : [] }
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 测试包含空键的baggage条目
  let empty_key_baggage = @azimuth.Baggage {
    entries : [("", "value")]
  }
  assert_eq(empty_key_baggage.entries.length(), 1)
  assert_eq(empty_key_baggage.entries[0].0, "")
  assert_eq(empty_key_baggage.entries[0].1, "value")
  
  // 测试包含空值的baggage条目
  let empty_value_baggage = @azimuth.Baggage {
    entries : [("key", "")]
  }
  assert_eq(empty_value_baggage.entries.length(), 1)
  assert_eq(empty_value_baggage.entries[0].0, "key")
  assert_eq(empty_value_baggage.entries[0].1, "")
  
  // 测试包含特殊字符的baggage条目
  let special_char_baggage = @azimuth.Baggage {
    entries : [
      ("key with spaces", "value with spaces"),
      ("key&with&symbols", "value&with&symbols"),
      ("key=with=equals", "value=with=equals"),
      ("key,with,commas", "value,with,commas")
    ]
  }
  
  assert_eq(special_char_baggage.entries.length(), 4)
  assert_eq(special_char_baggage.entries[0], ("key with spaces", "value with spaces"))
  assert_eq(special_char_baggage.entries[1], ("key&with&symbols", "value&with&symbols"))
  assert_eq(special_char_baggage.entries[2], ("key=with=equals", "value=with=equals"))
  assert_eq(special_char_baggage.entries[3], ("key,with,commas", "value,with,commas"))
}

test "text map carrier error handling" {
  // 测试空carrier
  let empty_carrier = @azimuth.TextMapCarrier { headers : [] }
  assert_eq(empty_carrier.headers.length(), 0)
  
  // 测试包含空header名称的carrier
  let empty_name_carrier = @azimuth.TextMapCarrier {
    headers : [("", "value")]
  }
  assert_eq(empty_name_carrier.headers.length(), 1)
  assert_eq(empty_name_carrier.headers[0].0, "")
  assert_eq(empty_name_carrier.headers[0].1, "value")
  
  // 测试包含空header值的carrier
  let empty_value_carrier = @azimuth.TextMapCarrier {
    headers : [("name", "")]
  }
  assert_eq(empty_value_carrier.headers.length(), 1)
  assert_eq(empty_value_carrier.headers[0].0, "name")
  assert_eq(empty_value_carrier.headers[0].1, "")
  
  // 测试包含特殊字符的headers
  let special_char_carrier = @azimuth.TextMapCarrier {
    headers : [
      ("name with spaces", "value with spaces"),
      ("name-with-dashes", "value-with-dashes"),
      ("name_with_underscores", "value_with_underscores"),
      ("name.with.dots", "value.with.dots")
    ]
  }
  
  assert_eq(special_char_carrier.headers.length(), 4)
  assert_eq(special_char_carrier.headers[0], ("name with spaces", "value with spaces"))
  assert_eq(special_char_carrier.headers[1], ("name-with-dashes", "value-with-dashes"))
  assert_eq(special_char_carrier.headers[2], ("name_with_underscores", "value_with_underscores"))
  assert_eq(special_char_carrier.headers[3], ("name.with.dots", "value.with.dots"))
}

test "resource error handling" {
  // 测试空资源
  let empty_resource = @azimuth.Resource { attributes : [] }
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 测试包含无效属性名称的资源
  let invalid_attr_name_resource = @azimuth.Resource {
    attributes : [("", @azimuth.StringValue("value"))]
  }
  assert_eq(invalid_attr_name_resource.attributes.length(), 1)
  assert_eq(invalid_attr_name_resource.attributes[0].0, "")
  
  // 测试包含null属性值的资源（在MoonBit中使用Option类型）
  let null_attr_resource = @azimuth.Resource {
    attributes : [
      ("valid.attr", @azimuth.StringValue("valid_value")),
      ("empty.attr", @azimuth.StringValue(""))
    ]
  }
  
  assert_eq(null_attr_resource.attributes.length(), 2)
  match null_attr_resource.attributes[0].1 {
    @azimuth.StringValue(v) => assert_eq(v, "valid_value")
    _ => assert_true(false)
  }
  
  match null_attr_resource.attributes[1].1 {
    @azimuth.StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
}

test "context error handling" {
  // 测试空上下文
  let empty_context = @azimuth.Context { data : None }
  match empty_context.data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试包含空键的上下文
  let empty_key_context = @azimuth.Context { 
    data : Some(("", "value")) 
  }
  match empty_key_context.data {
    Some((key, value)) => {
      assert_eq(key, "")
      assert_eq(value, "value")
    }
    None => assert_true(false)
  }
  
  // 测试包含空值的上下文
  let empty_value_context = @azimuth.Context { 
    data : Some(("key", "")) 
  }
  match empty_value_context.data {
    Some((key, value)) => {
      assert_eq(key, "key")
      assert_eq(value, "")
    }
    None => assert_true(false)
  }
  
  // 测试包含特殊字符的上下文
  let special_char_context = @azimuth.Context { 
    data : Some(("key with spaces", "value with spaces")) 
  }
  match special_char_context.data {
    Some((key, value)) => {
      assert_eq(key, "key with spaces")
      assert_eq(value, "value with spaces")
    }
    None => assert_true(false)
  }
}

test "instrumentation scope error handling" {
  // 测试空作用域名称
  let empty_name_scope = @azimuth.InstrumentationScope {
    name : "",
    version : Some("1.0.0"),
    schema_url : Some("https://example.com/schema")
  }
  assert_eq(empty_name_scope.name, "")
  
  // 测试None版本
  let none_version_scope = @azimuth.InstrumentationScope {
    name : "test-scope",
    version : None,
    schema_url : Some("https://example.com/schema")
  }
  match none_version_scope.version {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试None schema URL
  let none_schema_scope = @azimuth.InstrumentationScope {
    name : "test-scope",
    version : Some("1.0.0"),
    schema_url : None
  }
  match none_schema_scope.schema_url {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试空版本字符串
  let empty_version_scope = @azimuth.InstrumentationScope {
    name : "test-scope",
    version : Some(""),
    schema_url : Some("https://example.com/schema")
  }
  match empty_version_scope.version {
    Some(v) => assert_eq(v, "")
    None => assert_true(false)
  }
  
  // 测试空schema URL字符串
  let empty_schema_scope = @azimuth.InstrumentationScope {
    name : "test-scope",
    version : Some("1.0.0"),
    schema_url : Some("")
  }
  match empty_schema_scope.schema_url {
    Some(url) => assert_eq(url, "")
    None => assert_true(false)
  }
}

test "complex error scenarios" {
  // 测试混合错误条件的复杂场景
  let complex_resource = @azimuth.Resource {
    attributes : [
      ("", @azimuth.StringValue("")),  // 空键和空值
      ("valid.key", @azimuth.StringValue("valid.value")),
      ("", @azimuth.IntValue(42)),  // 空键但有有效值
      ("another.valid.key", @azimuth.StringValue(""))
    ]
  }
  
  assert_eq(complex_resource.attributes.length(), 4)
  
  // 验证每个条目
  assert_eq(complex_resource.attributes[0].0, "")
  match complex_resource.attributes[0].1 {
    @azimuth.StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
  
  assert_eq(complex_resource.attributes[1].0, "valid.key")
  match complex_resource.attributes[1].1 {
    @azimuth.StringValue(v) => assert_eq(v, "valid.value")
    _ => assert_true(false)
  }
  
  assert_eq(complex_resource.attributes[2].0, "")
  match complex_resource.attributes[2].1 {
    @azimuth.IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  assert_eq(complex_resource.attributes[3].0, "another.valid.key")
  match complex_resource.attributes[3].1 {
    @azimuth.StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
}