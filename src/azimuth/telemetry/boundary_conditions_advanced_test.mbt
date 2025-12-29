// 边界条件测试 - 测试极端输入情况下的行为

test "boundary conditions: empty and null string attributes" {
  // 测试空字符串和特殊字符串值
  let empty_string = AttributeValue::string("")
  let whitespace_string = AttributeValue::string("   ")
  let special_chars_string = AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  
  // 验证空字符串
  assert match (empty_string) {
    StringValue(s) => s == ""
    _ => false
  }
  
  // 验证空白字符串
  assert match (whitespace_string) {
    StringValue(s) => s == "   "
    _ => false
  }
  
  // 验证特殊字符字符串
  assert match (special_chars_string) {
    StringValue(s) => s == "!@#$%^&*()_+-=[]{}|;':\",./<>?"
    _ => false
  }
  
  // 创建包含边界条件的Resource
  let resource = Resource::{
    service_name: "",
    service_version: Some(""),
    telemetry_sdk_name: "   ",
    telemetry_sdk_version: "!@#$%",
    attributes: [
      ("empty", empty_string),
      ("whitespace", whitespace_string),
      ("special", special_chars_string)
    ]
  }
  
  // 验证Resource创建成功
  assert resource.service_name == ""
  assert match (resource.service_version) {
    Some(v) => v == ""
    None => false
  }
  assert resource.telemetry_sdk_name == "   "
  assert resource.telemetry_sdk_version == "!@#$%"
}

test "boundary conditions: extreme numeric values" {
  // 测试极值数值
  let max_int = AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int = AttributeValue::int(-9223372036854775808L)  // Int64最小值
  let zero_int = AttributeValue::int(0L)
  
  let max_float = AttributeValue::float(1.7976931348623157e+308)  // Double最大值
  let min_float = AttributeValue::float(-1.7976931348623157e+308)  // Double最小值
  let zero_float = AttributeValue::float(0.0)
  let inf_float = AttributeValue::float(1.0/0.0)  // 无穷大
  let neg_inf_float = AttributeValue::float(-1.0/0.0)  // 负无穷大
  let nan_float = AttributeValue::float(0.0/0.0)  // NaN
  
  // 验证极值整数
  assert match (max_int) {
    IntValue(i) => i == 9223372036854775807L
    _ => false
  }
  
  assert match (min_int) {
    IntValue(i) => i == -9223372036854775808L
    _ => false
  }
  
  assert match (zero_int) {
    IntValue(i) => i == 0L
    _ => false
  }
  
  // 验证极值浮点数
  assert match (max_float) {
    FloatValue(f) => f == 1.7976931348623157e+308
    _ => false
  }
  
  assert match (min_float) {
    FloatValue(f) => f == -1.7976931348623157e+308
    _ => false
  }
  
  assert match (zero_float) {
    FloatValue(f) => f == 0.0
    _ => false
  }
}

test "boundary conditions: empty and large arrays" {
  // 测试空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  // 验证空数组
  assert match (empty_string_array) {
    ArrayStringValue(arr) => arr.length == 0
    _ => false
  }
  
  assert match (empty_int_array) {
    ArrayIntValue(arr) => arr.length == 0
    _ => false
  }
  
  assert match (empty_float_array) {
    ArrayFloatValue(arr) => arr.length == 0
    _ => false
  }
  
  assert match (empty_bool_array) {
    ArrayBoolValue(arr) => arr.length == 0
    _ => false
  }
  
  // 测试单元素数组
  let single_string_array = AttributeValue::array_string(["single"])
  let single_int_array = AttributeValue::array_int([1L])
  let single_float_array = AttributeValue::array_float([1.0])
  let single_bool_array = AttributeValue::array_bool([true])
  
  // 验证单元素数组
  assert match (single_string_array) {
    ArrayStringValue(arr) => arr.length == 1 && arr[0] == "single"
    _ => false
  }
  
  assert match (single_int_array) {
    ArrayIntValue(arr) => arr.length == 1 && arr[0] == 1L
    _ => false
  }
  
  assert match (single_float_array) {
    ArrayFloatValue(arr) => arr.length == 1 && arr[0] == 1.0
    _ => false
  }
  
  assert match (single_bool_array) {
    ArrayBoolValue(arr) => arr.length == 1 && arr[0] == true
    _ => false
  }
}

test "boundary conditions: context with special keys" {
  // 测试特殊键名
  let empty_key = create_key("")
  let whitespace_key = create_key("   ")
  let long_key = create_key("this_is_a_very_long_key_name_that_exceeds_normal_expectations_and_tests_boundary_conditions")
  let special_chars_key = create_key("!@#$%^&*()")
  let unicode_key = create_key("测试键名_🚀_emoji")
  
  let context = Context::empty()
  
  // 使用特殊键名添加值
  let context1 = context.with_value(empty_key, "empty_key_value")
  let context2 = context1.with_value(whitespace_key, "whitespace_key_value")
  let context3 = context2.with_value(long_key, "long_key_value")
  let context4 = context3.with_value(special_chars_key, "special_chars_value")
  let context5 = context4.with_value(unicode_key, "unicode_value")
  
  // 验证特殊键名的值可以正确获取
  assert match (context5.get(empty_key)) {
    Some(value) => value == "empty_key_value"
    None => false
  }
  
  assert match (context5.get(whitespace_key)) {
    Some(value) => value == "whitespace_key_value"
    None => false
  }
  
  assert match (context5.get(long_key)) {
    Some(value) => value == "long_key_value"
    None => false
  }
  
  assert match (context5.get(special_chars_key)) {
    Some(value) => value == "special_chars_value"
    None => false
  }
  
  assert match (context5.get(unicode_key)) {
    Some(value) => value == "unicode_value"
    None => false
  }
}

test "boundary conditions: baggage with special entries" {
  // 测试特殊baggage条目
  let baggage = Baggage::empty()
  
  // 添加特殊条目
  let baggage1 = baggage.with_entry("", "empty_key_value")
  let baggage2 = baggage1.with_entry("   ", "whitespace_key_value")
  let baggage3 = baggage2.with_entry("special!@#$%", "special_key_value")
  let baggage4 = baggage3.with_entry("测试", "unicode_value")
  let baggage5 = baggage4.with_entry("empty_value", "")
  
  // 验证特殊条目的值可以正确获取
  assert match (baggage5.get("")) {
    Some(value) => value == "empty_key_value"
    None => false
  }
  
  assert match (baggage5.get("   ")) {
    Some(value) => value == "whitespace_key_value"
    None => false
  }
  
  assert match (baggage5.get("special!@#$%")) {
    Some(value) => value == "special_key_value"
    None => false
  }
  
  assert match (baggage5.get("测试")) {
    Some(value) => value == "unicode_value"
    None => false
  }
  
  assert match (baggage5.get("empty_value")) {
    Some(value) => value == ""
    None => false
  }
  
  // 验证不存在的键返回None
  assert match (baggage5.get("nonexistent_key")) {
    Some(_) => false
    None => true
  }
}

test "boundary conditions: instrumentation scope edge cases" {
  // 测试极端的InstrumentationScope配置
  let empty_scope = InstrumentationScope::{
    name: "",
    version: None,
    schema_url: None
  }
  
  let long_scope = InstrumentationScope::{
    name: "this_is_a_very_long_instrumentation_scope_name_that_tests_boundary_conditions",
    version: Some("this_is_a_very_long_version_string_that_tests_boundary_conditions_too"),
    schema_url: Some("this_is_a_very_long_schema_url_that_tests_boundary_conditions_as_well_and_should_be_handled_properly")
  }
  
  let special_scope = InstrumentationScope::{
    name: "!@#$%^&*()_+-=[]{}|;':\",./<>?",
    version: Some("!@#$%^&*()"),
    schema_url: Some("https://example.com/!@#$%^&*()")
  }
  
  let unicode_scope = InstrumentationScope::{
    name: "测试范围_🚀",
    version: Some("版本1.0_测试"),
    schema_url: Some("https://测试.example.com/模式")
  }
  
  // 验证各种极端情况
  assert empty_scope.name == ""
  assert empty_scope.version == None
  assert empty_scope.schema_url == None
  
  assert long_scope.name.length > 50
  assert match (long_scope.version) {
    Some(v) => v.length > 50
    None => false
  }
  assert match (long_scope.schema_url) {
    Some(url) => url.length > 50
    None => false
  }
  
  assert special_scope.name == "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert match (special_scope.version) {
    Some(v) => v == "!@#$%^&*()"
    None => false
  }
  assert match (special_scope.schema_url) {
    Some(url) => url == "https://example.com/!@#$%^&*()"
    None => false
  }
  
  assert unicode_scope.name == "测试范围_🚀"
  assert match (unicode_scope.version) {
    Some(v) => v == "版本1.0_测试"
    None => false
  }
  assert match (unicode_scope.schema_url) {
    Some(url) => url == "https://测试.example.com/模式"
    None => false
  }
}

test "boundary conditions: metrics with extreme values" {
  // 测试指标的极值
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("boundary_test_meter", None, None)
  
  let counter = meter.create_counter("boundary_counter", None, None)
  let histogram = meter.create_histogram("boundary_histogram", None, None)
  let up_down_counter = meter.create_up_down_counter("boundary_up_down", None, None)
  let gauge = meter.create_gauge("boundary_gauge", None, None)
  
  // 测试极值
  counter.add(9223372036854775807L, None)  // Int64最大值
  counter.add(-9223372036854775808L, None)  // Int64最小值
  counter.add(0L, None)  // 零值
  
  histogram.record(1.7976931348623157e+308, None)  // Double最大值
  histogram.record(-1.7976931348623157e+308, None)  // Double最小值
  histogram.record(0.0, None)  // 零值
  histogram.record(1.0/0.0, None)  // 无穷大
  histogram.record(-1.0/0.0, None)  // 负无穷大
  histogram.record(0.0/0.0, None)  // NaN
  
  up_down_counter.add(9223372036854775807L, None)  // Int64最大值
  up_down_counter.add(-9223372036854775808L, None)  // Int64最小值
  
  gauge.record(1.7976931348623157e+308, None)  // Double最大值
  gauge.record(-1.7976931348623157e+308, None)  // Double最小值
  
  // 测试带属性的极值
  let extreme_attributes = [
    ("", AttributeValue::string("empty_key")),
    ("   ", AttributeValue::string("whitespace_key")),
    ("!@#$%", AttributeValue::string("special_key")),
    ("测试", AttributeValue::string("unicode_key")),
    ("max_int", AttributeValue::int(9223372036854775807L)),
    ("max_float", AttributeValue::float(1.7976931348623157e+308)),
    ("empty_array", AttributeValue::array_string([])),
    ("single_element", AttributeValue::array_string(["single"]))
  ]
  
  counter.add(1L, Some(extreme_attributes))
  histogram.record(100.0, Some(extreme_attributes))
  up_down_counter.add(-1L, Some(extreme_attributes))
  gauge.record(50.0, Some(extreme_attributes))
}