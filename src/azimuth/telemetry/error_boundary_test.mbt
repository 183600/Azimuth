// 错误处理和边界情况测试用例
test "attribute_value_extreme_values" {
  // 测试AttributeValue的极值处理
  
  // 字符串极值
  let empty_string = AttributeValue::string("")
  let very_long_string = AttributeValue::string("a" * 10000)  // 假设支持字符串重复
  let unicode_string = AttributeValue::string("Unicode测试🚀🌟💎🎯")
  let special_chars_string = AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  
  match empty_string {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected empty string")
  }
  
  match very_long_string {
    StringValue(s) => assert_eq(s.length(), 10000)
    _ => @test.fail("Expected very long string")
  }
  
  match unicode_string {
    StringValue(s) => assert_eq(s, "Unicode测试🚀🌟💎🎯")
    _ => @test.fail("Expected unicode string")
  }
  
  match special_chars_string {
    StringValue(s) => assert_eq(s, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
    _ => @test.fail("Expected special characters string")
  }
  
  // 整数极值
  let zero_int = AttributeValue::int(0L)
  let max_int = AttributeValue::int(9223372036854775807L)
  let min_int = AttributeValue::int(-9223372036854775808L)
  let negative_int = AttributeValue::int(-1L)
  
  match zero_int {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Expected zero integer")
  }
  
  match max_int {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected max integer")
  }
  
  match min_int {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Expected min integer")
  }
  
  match negative_int {
    IntValue(i) => assert_eq(i, -1L)
    _ => @test.fail("Expected negative integer")
  }
  
  // 浮点数极值
  let zero_float = AttributeValue::float(0.0)
  let positive_inf = AttributeValue::float(1.0/0.0)
  let negative_inf = AttributeValue::float(-1.0/0.0)
  let nan_float = AttributeValue::float(0.0/0.0)
  let very_small_float = AttributeValue::float(1e-308)
  let very_large_float = AttributeValue::float(1e+308)
  
  match zero_float {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => @test.fail("Expected zero float")
  }
  
  match positive_inf {
    FloatValue(f) => assert_eq(f, 1.0/0.0)  // Infinity
    _ => @test.fail("Expected positive infinity")
  }
  
  match negative_inf {
    FloatValue(f) => assert_eq(f, -1.0/0.0)  // -Infinity
    _ => @test.fail("Expected negative infinity")
  }
  
  match nan_float {
    FloatValue(f) => assert_eq(f, 0.0/0.0)  // NaN
    _ => @test.fail("Expected NaN")
  }
  
  // 布尔值
  let true_bool = AttributeValue::bool(true)
  let false_bool = AttributeValue::bool(false)
  
  match true_bool {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected true boolean")
  }
  
  match false_bool {
    BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected false boolean")
  }
}

test "array_attribute_edge_cases" {
  // 测试数组属性的边界情况
  
  // 空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty string array")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty int array")
  }
  
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty float array")
  }
  
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty bool array")
  }
  
  // 单元素数组
  let single_string_array = AttributeValue::array_string(["single"])
  let single_int_array = AttributeValue::array_int([42L])
  let single_float_array = AttributeValue::array_float([3.14])
  let single_bool_array = AttributeValue::array_bool([true])
  
  match single_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => @test.fail("Expected single string array")
  }
  
  match single_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42L)
    }
    _ => @test.fail("Expected single int array")
  }
  
  match single_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 3.14)
    }
    _ => @test.fail("Expected single float array")
  }
  
  match single_bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], true)
    }
    _ => @test.fail("Expected single bool array")
  }
  
  // 大数组
  let large_string_array = AttributeValue::array_string(["item" + i.to_string() | i <- 0..999])  // 1000个元素
  let large_int_array = AttributeValue::array_int([i | i <- 0..999])  // 1000个元素
  
  match large_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 1000)
    _ => @test.fail("Expected large string array")
  }
  
  match large_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 1000)
    _ => @test.fail("Expected large int array")
  }
  
  // 包含极值的数组
  let extreme_int_array = AttributeValue::array_int([
    0L, 1L, -1L, 9223372036854775807L, -9223372036854775808L
  ])
  let extreme_float_array = AttributeValue::array_float([
    0.0, 1.0, -1.0, 1.0/0.0, -1.0/0.0, 0.0/0.0, 1e-308, 1e+308
  ])
  
  match extreme_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 0L)
      assert_eq(arr[1], 1L)
      assert_eq(arr[2], -1L)
      assert_eq(arr[3], 9223372036854775807L)
      assert_eq(arr[4], -9223372036854775808L)
    }
    _ => @test.fail("Expected extreme int array")
  }
  
  match extreme_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 8)
      assert_eq(arr[0], 0.0)
      assert_eq(arr[1], 1.0)
      assert_eq(arr[2], -1.0)
      assert_eq(arr[3], 1.0/0.0)
      assert_eq(arr[4], -1.0/0.0)
      assert_eq(arr[5], 0.0/0.0)
      assert_eq(arr[6], 1e-308)
      assert_eq(arr[7], 1e+308)
    }
    _ => @test.fail("Expected extreme float array")
  }
}

test "context_edge_cases_and_error_handling" {
  // 测试Context的边界情况和错误处理
  
  // 空Context
  let empty_ctx = Context::empty()
  let nonexistent_key = create_key("nonexistent.key")
  
  match empty_ctx.get(nonexistent_key) {
    Some(_) => @test.fail("Expected None for nonexistent key in empty context")
    None => assert_eq(true, true)
  }
  
  // 大量键值对
  let ctx_with_many_values = { 
    let mut ctx = Context::empty()
    let mut i = 0
    while i < 1000 {
      ctx = ctx.with_value(create_key("key" + i.to_string()), "value" + i.to_string())
      i = i + 1
    }
    ctx
  }
  
  // 验证大量键值对
  match ctx_with_many_values.get(create_key("key0")) {
    Some(value) => assert_eq(value, "value0")
    None => @test.fail("Expected key0 to be found")
  }
  
  match ctx_with_many_values.get(create_key("key999")) {
    Some(value) => assert_eq(value, "value999")
    None => @test.fail("Expected key999 to be found")
  }
  
  match ctx_with_many_values.get(create_key("key1000")) {
    Some(_) => @test.fail("Expected None for key1000")
    None => assert_eq(true, true)
  }
  
  // 特殊键名
  let special_keys = [
    ("", "empty key"),
    (" ", "space key"),
    ("key.with.dots", "dotted key"),
    ("key/with/slashes", "slashed key"),
    ("key with spaces", "spaced key"),
    ("special!@#$%^&*()chars", "special chars key"),
    ("Unicode键测试🚀", "Unicode key"),
    ("key\nwith\nnewlines", "newline key"),
    ("key\twith\ttabs", "tab key")
  ]
  
  let ctx_with_special_keys = {
    let mut ctx = Context::empty()
    let mut i = 0
    while i < special_keys.length() {
      let (key_name, key_value) = special_keys[i]
      ctx = ctx.with_value(create_key(key_name), key_value)
      i = i + 1
    }
    ctx
  }
  
  // 验证特殊键名
  let mut i = 0
  while i < special_keys.length() {
    let (key_name, key_value) = special_keys[i]
    match ctx_with_special_keys.get(create_key(key_name)) {
      Some(value) => assert_eq(value, key_value)
      None => @test.fail("Expected special key to be found: " + key_name)
    }
    i = i + 1
  }
  
  // 相同键名的多次覆盖
  let same_key = create_key("same.key")
  let ctx_with_overrides = {
    let mut ctx = Context::empty()
    let mut i = 0
    while i < 100 {
      ctx = ctx.with_value(same_key, "value" + i.to_string())
      i = i + 1
    }
    ctx
  }
  
  // 验证最终值
  match ctx_with_overrides.get(same_key) {
    Some(value) => assert_eq(value, "value99")
    None => @test.fail("Expected final override value")
  }
}

test "span_context_edge_cases" {
  // 测试SpanContext的边界情况
  
  // 全零SpanContext
  let zero_span_context = SpanContext::{
    trace_id: Array::make(16, 0_byte),
    span_id: Array::make(8, 0_byte),
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  assert_eq(zero_span_context.trace_id.length(), 16)
  assert_eq(zero_span_context.span_id.length(), 8)
  assert_eq(zero_span_context.trace_flags, 0_byte)
  assert_eq(zero_span_context.trace_state, "")
  
  // 最大值SpanContext
  let max_span_context = SpanContext::{
    trace_id: Array::make(16, 255_byte),
    span_id: Array::make(8, 255_byte),
    trace_flags: 255_byte,
    trace_state: "key1=value1,key2=value2,key3=value3"
  }
  
  assert_eq(max_span_context.trace_id.length(), 16)
  assert_eq(max_span_context.span_id.length(), 8)
  assert_eq(max_span_context.trace_flags, 255_byte)
  assert_eq(max_span_context.trace_state, "key1=value1,key2=value2,key3=value3")
  
  // 复杂trace_state
  let complex_trace_state = "vendor1=value1,vendor2=value2 with spaces,vendor3=value3!@#$%^&*(),vendor4=Unicode值测试🚀"
  let complex_span_context = SpanContext::{
    trace_id: Array::make(16, 128_byte),
    span_id: Array::make(8, 128_byte),
    trace_flags: 1_byte,
    trace_state: complex_trace_state
  }
  
  assert_eq(complex_span_context.trace_state, complex_trace_state)
  
  // 验证trace_id和span_id的具体字节
  let mut i = 0
  while i < 16 {
    assert_eq(zero_span_context.trace_id[i], 0_byte)
    assert_eq(max_span_context.trace_id[i], 255_byte)
    assert_eq(complex_span_context.trace_id[i], 128_byte)
    i = i + 1
  }
  
  let mut i = 0
  while i < 8 {
    assert_eq(zero_span_context.span_id[i], 0_byte)
    assert_eq(max_span_context.span_id[i], 255_byte)
    assert_eq(complex_span_context.span_id[i], 128_byte)
    i = i + 1
  }
}

test "propagation_error_scenarios" {
  // 测试传播的错误场景
  
  // 无效的traceparent格式
  let invalid_trace_parents = [
    "",  // 空字符串
    "invalid-format",  // 无效格式
    "01-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331",  // 缺少trace_flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra",  // 额外部分
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-XX",  // 无效trace_flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b71692033",  // span_id太短
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",  // 正确格式（用于对比）
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",  // 重复的正确格式
    "ff-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"   // 不同版本
  ]
  
  let ctx = Context::empty()
  let propagator = W3CTraceContextPropagator::{}
  
  let mut i = 0
  while i < invalid_trace_parents.length() {
    let invalid_trace_parent = invalid_trace_parents[i]
    let carrier_data = [(TRACE_PARENT_HEADER, invalid_trace_parent)]
    let carrier = MapCarrier::from_map(carrier_data)
    
    // 即使格式无效，提取操作也不应该崩溃
    let extracted_ctx = propagator.extract(ctx, carrier)
    assert_eq(true, true)  // 如果没有崩溃就算通过
    
    i = i + 1
  }
  
  // 无效的baggage格式
  let invalid_baggages = [
    "",  // 空字符串
    "invalid",  // 无效格式（没有=）
    "key=",  // 空值
    "=value",  // 空键
    "key=value=extra",  // 多个等号
    "key=value,invalid",  // 混合格式
    "key=value,key2=",  // 混合有效和无效
    "key=value,key2=value2,invalid"  // 多个条目中有无效的
  ]
  
  let baggage_propagator = W3CBaggagePropagator::{}
  
  let mut i = 0
  while i < invalid_baggages.length() {
    let invalid_baggage = invalid_baggages[i]
    let carrier_data = [(BAGGAGE_HEADER, invalid_baggage)]
    let carrier = MapCarrier::from_map(carrier_data)
    
    // 即使格式无效，提取操作也不应该崩溃
    let extracted_ctx = baggage_propagator.extract(ctx, carrier)
    assert_eq(true, true)  // 如果没有崩溃就算通过
    
    i = i + 1
  }
  
  // MapCarrier错误场景
  let error_carrier_data = [
    ("", ""),  // 空键和值
    ("key", ""),  // 空值
    ("", "value"),  // 空键
    ("special/key/with/special.chars", "特殊值!@#$%^&*()"),  // 特殊字符
    ("Unicode键测试🚀", "Unicode值测试🌟"),  // Unicode字符
    ("key\nwith\nnewlines", "value\nwith\nnewlines"),  // 换行符
    ("key\twith\ttabs", "value\twith\ttabs")  // 制表符
  ]
  
  let error_carrier = MapCarrier::from_map(error_carrier_data)
  
  // 验证所有键都可以访问
  let mut i = 0
  while i < error_carrier_data.length() {
    let (key, expected_value) = error_carrier_data[i]
    match error_carrier.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Expected error carrier key to be found: " + key)
    }
    i = i + 1
  }
  
  // 验证keys操作
  let keys = error_carrier.keys()
  assert_eq(keys.length(), error_carrier_data.length())
}

test "resource_and_instrumentation_scope_edge_cases" {
  // 测试Resource和InstrumentationScope的边界情况
  
  // 极端服务名称
  let extreme_service_names = [
    "",  // 空字符串
    " ",  // 空格
    "a",  // 单字符
    "service.with.dots",  // 包含点
    "service/with/slashes",  // 包含斜杠
    "service with spaces",  // 包含空格
    "service!@#$%^&*()with+special-chars",  // 特殊字符
    "Unicode服务测试🚀",  // Unicode字符
    "service\nwith\nnewlines",  // 换行符
    "service\twith\ttabs",  // 制表符
    "a" * 1000  // 长字符串
  ]
  
  let mut i = 0
  while i < extreme_service_names.length() {
    let service_name = extreme_service_names[i]
    let resource = Resource::default(service_name)
    
    assert_eq(resource.service_name, service_name)
    assert_eq(resource.telemetry_sdk_name, "azimuth")
    assert_eq(resource.telemetry_sdk_version, "0.1.0")
    
    i = i + 1
  }
  
  // 复杂属性组合
  let complex_attrs : Attributes = [
    ("", AttributeValue::string("empty key")),  // 空键
    ("empty.value", AttributeValue::string("")),  // 空值
    ("special.chars.key", AttributeValue::string("!@#$%^&*()")),  // 特殊字符
    ("Unicode.key.测试", AttributeValue::string("Unicode值测试🚀")),  // Unicode
    ("key.with.dots", AttributeValue::string("dotted.value")),  // 点号
    ("key/with/slashes", AttributeValue::string("slashed.value")),  // 斜杠
    ("key with spaces", AttributeValue::string("spaced.value")),  // 空格
    ("multiline.key", AttributeValue::string("value\nwith\nnewlines")),  // 多行
    ("tab.key", AttributeValue::string("value\twith\ttabs")),  // 制表符
    ("very.long.key.name.that.exceeds.normal.expectations.and.tests.boundary.conditions", AttributeValue::string("long key")),  // 长键名
    ("array.value", AttributeValue::array_string(["item1", "item2", "item3"])),  // 数组值
    ("nested.value", AttributeValue::string("value.with.nested.structure")),  // 嵌套结构
    ("numeric.key.123", AttributeValue::string("numeric.key")),  // 数字键名
    ("mixed.case.KEY", AttributeValue::string("Mixed.Case.Value"))  // 大小写混合
  ]
  
  // 验证复杂属性
  assert_eq(complex_attrs.length(), 14)
  
  // 验证特定属性
  match complex_attrs[0].1 {
    AttributeValue::StringValue(value) => assert_eq(value, "empty key")
    _ => @test.fail("Expected StringValue for empty key")
  }
  
  match complex_attrs[1].1 {
    AttributeValue::StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Expected StringValue for empty value")
  }
  
  match complex_attrs[10].1 {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    _ => @test.fail("Expected ArrayStringValue for array.value")
  }
}