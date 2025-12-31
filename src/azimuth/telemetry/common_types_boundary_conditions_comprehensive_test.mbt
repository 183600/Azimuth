// Azimuth Telemetry - Common Types Boundary Conditions Test
// 公共类型边界条件测试

test "attribute_value_string_types" {
  // 测试字符串属性值的边界条件
  let empty_string = common::AttributeValue::string("")
  match empty_string {
    common::StringValue(s) => assert_eq(s, "")
    _ => assert(false, "Expected string value")
  }
  
  let single_char = common::AttributeValue::string("a")
  match single_char {
    common::StringValue(s) => assert_eq(s, "a")
    _ => assert(false, "Expected string value")
  }
  
  let long_string = common::AttributeValue::string("x".repeat(10000))
  match long_string {
    common::StringValue(s) => assert_eq(s.length(), 10000)
    _ => assert(false, "Expected string value")
  }
  
  let special_chars = common::AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  match special_chars {
    common::StringValue(s) => assert_eq(s, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
    _ => assert(false, "Expected string value")
  }
  
  let unicode_string = common::AttributeValue::string("Unicode: 🚀 🌟 测试 العربية 中文")
  match unicode_string {
    common::StringValue(s) => assert_eq(s, "Unicode: 🚀 🌟 测试 العربية 中文")
    _ => assert(false, "Expected string value")
  }
  
  let whitespace_string = common::AttributeValue::string("   \t\n\r   ")
  match whitespace_string {
    common::StringValue(s) => assert_eq(s, "   \t\n\r   ")
    _ => assert(false, "Expected string value")
  }
}

test "attribute_value_int_types" {
  // 测试整数属性值的边界条件
  let zero_int = common::AttributeValue::int(0L)
  match zero_int {
    common::IntValue(i) => assert_eq(i, 0L)
    _ => assert(false, "Expected int value")
  }
  
  let positive_int = common::AttributeValue::int(42L)
  match positive_int {
    common::IntValue(i) => assert_eq(i, 42L)
    _ => assert(false, "Expected int value")
  }
  
  let negative_int = common::AttributeValue::int(-42L)
  match negative_int {
    common::IntValue(i) => assert_eq(i, -42L)
    _ => assert(false, "Expected int value")
  }
  
  let max_int = common::AttributeValue::int(Int64::max_value())
  match max_int {
    common::IntValue(i) => assert_eq(i, Int64::max_value())
    _ => assert(false, "Expected int value")
  }
  
  let min_int = common::AttributeValue::int(Int64::min_value())
  match min_int {
    common::IntValue(i) => assert_eq(i, Int64::min_value())
    _ => assert(false, "Expected int value")
  }
}

test "attribute_value_float_types" {
  // 测试浮点数属性值的边界条件
  let zero_float = common::AttributeValue::float(0.0)
  match zero_float {
    common::FloatValue(f) => assert_eq(f, 0.0)
    _ => assert(false, "Expected float value")
  }
  
  let positive_float = common::AttributeValue::float(3.14159)
  match positive_float {
    common::FloatValue(f) => assert_eq(f, 3.14159)
    _ => assert(false, "Expected float value")
  }
  
  let negative_float = common::AttributeValue::float(-3.14159)
  match negative_float {
    common::FloatValue(f) => assert_eq(f, -3.14159)
    _ => assert(false, "Expected float value")
  }
  
  let very_small_float = common::AttributeValue::float(0.0000001)
  match very_small_float {
    common::FloatValue(f) => assert_eq(f, 0.0000001)
    _ => assert(false, "Expected float value")
  }
  
  let very_large_float = common::AttributeValue::float(999999999.999999)
  match very_large_float {
    common::FloatValue(f) => assert_eq(f, 999999999.999999)
    _ => assert(false, "Expected float value")
  }
  
  let infinity_float = common::AttributeValue::float(Double::infinity)
  match infinity_float {
    common::FloatValue(f) => assert(f.is_infinity(), "Expected infinity")
    _ => assert(false, "Expected float value")
  }
  
  let neg_infinity_float = common::AttributeValue::float(Double::neg_infinity)
  match neg_infinity_float {
    common::FloatValue(f) => assert(f.is_infinity(), "Expected negative infinity")
    _ => assert(false, "Expected float value")
  }
  
  let nan_float = common::AttributeValue::float(Double::nan)
  match nan_float {
    common::FloatValue(f) => assert(f.is_nan(), "Expected NaN")
    _ => assert(false, "Expected float value")
  }
}

test "attribute_value_bool_types" {
  // 测试布尔属性值
  let true_value = common::AttributeValue::bool(true)
  match true_value {
    common::BoolValue(b) => assert_eq(b, true)
    _ => assert(false, "Expected bool value")
  }
  
  let false_value = common::AttributeValue::bool(false)
  match false_value {
    common::BoolValue(b) => assert_eq(b, false)
    _ => assert(false, "Expected bool value")
  }
}

test "attribute_value_array_string_types" {
  // 测试字符串数组属性值
  let empty_array = common::AttributeValue::array_string([])
  match empty_array {
    common::ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert(false, "Expected string array value")
  }
  
  let single_element_array = common::AttributeValue::array_string(["single"])
  match single_element_array {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => assert(false, "Expected string array value")
  }
  
  let multi_element_array = common::AttributeValue::array_string(["a", "b", "c", "d", "e"])
  match multi_element_array {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "a")
      assert_eq(arr[4], "e")
    }
    _ => assert(false, "Expected string array value")
  }
  
  let long_string_array = common::AttributeValue::array_string(["x".repeat(100), "y".repeat(200), "z".repeat(300)])
  match long_string_array {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0].length(), 100)
      assert_eq(arr[1].length(), 200)
      assert_eq(arr[2].length(), 300)
    }
    _ => assert(false, "Expected string array value")
  }
  
  let special_chars_array = common::AttributeValue::array_string(["!@#", "$%^", "&*()", "()_+"])
  match special_chars_array {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], "!@#")
      assert_eq(arr[3], "()_+")
    }
    _ => assert(false, "Expected string array value")
  }
  
  let unicode_array = common::AttributeValue::array_string(["🚀", "🌟", "测试", "العربية"])
  match unicode_array {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], "🚀")
      assert_eq(arr[3], "العربية")
    }
    _ => assert(false, "Expected string array value")
  }
}

test "attribute_value_array_int_types" {
  // 测试整数数组属性值
  let empty_array = common::AttributeValue::array_int([])
  match empty_array {
    common::ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => assert(false, "Expected int array value")
  }
  
  let single_element_array = common::AttributeValue::array_int([42L])
  match single_element_array {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42L)
    }
    _ => assert(false, "Expected int array value")
  }
  
  let mixed_sign_array = common::AttributeValue::array_int([-1L, 0L, 1L, -100L, 100L])
  match mixed_sign_array {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], -1L)
      assert_eq(arr[2], 1L)
      assert_eq(arr[4], 100L)
    }
    _ => assert(false, "Expected int array value")
  }
  
  let extreme_values_array = common::AttributeValue::array_int([Int64::min_value(), Int64::max_value()])
  match extreme_values_array {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], Int64::min_value())
      assert_eq(arr[1], Int64::max_value())
    }
    _ => assert(false, "Expected int array value")
  }
}

test "attribute_value_array_float_types" {
  // 测试浮点数数组属性值
  let empty_array = common::AttributeValue::array_float([])
  match empty_array {
    common::ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => assert(false, "Expected float array value")
  }
  
  let single_element_array = common::AttributeValue::array_float([3.14])
  match single_element_array {
    common::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 3.14)
    }
    _ => assert(false, "Expected float array value")
  }
  
  let mixed_values_array = common::AttributeValue::array_float([0.0, -1.5, 3.14159, 999.999])
  match mixed_values_array {
    common::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], 0.0)
      assert_eq(arr[1], -1.5)
      assert_eq(arr[3], 999.999)
    }
    _ => assert(false, "Expected float array value")
  }
  
  let special_values_array = common::AttributeValue::array_float([Double::infinity, Double::neg_infinity, Double::nan])
  match special_values_array {
    common::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert(arr[0].is_infinity(), "Expected infinity")
      assert(arr[1].is_infinity(), "Expected negative infinity")
      assert(arr[2].is_nan(), "Expected NaN")
    }
    _ => assert(false, "Expected float array value")
  }
}

test "attribute_value_array_bool_types" {
  // 测试布尔数组属性值
  let empty_array = common::AttributeValue::array_bool([])
  match empty_array {
    common::ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => assert(false, "Expected bool array value")
  }
  
  let single_true_array = common::AttributeValue::array_bool([true])
  match single_true_array {
    common::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], true)
    }
    _ => assert(false, "Expected bool array value")
  }
  
  let single_false_array = common::AttributeValue::array_bool([false])
  match single_false_array {
    common::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], false)
    }
    _ => assert(false, "Expected bool array value")
  }
  
  let mixed_array = common::AttributeValue::array_bool([true, false, true, false, true])
  match mixed_array {
    common::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[4], true)
    }
    _ => assert(false, "Expected bool array value")
  }
}

test "resource_creation_and_defaults" {
  // 测试Resource创建和默认值
  let basic_resource = common::Resource::default("test-service")
  assert_eq(basic_resource.service_name, "test-service")
  assert_eq(basic_resource.service_version, None)
  assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(basic_resource.attributes.length(), 0)
  
  // 测试带版本的服务
  let resource_with_version = common::Resource::default("test-service")
  // 在实际实现中，这里应该能设置版本
  assert_eq(resource_with_version.service_name, "test-service")
  
  // 测试空服务名
  let empty_service_resource = common::Resource::default("")
  assert_eq(empty_service_resource.service_name, "")
  
  // 测试长服务名
  let long_service_name = "service-".repeat(100)
  let long_service_resource = common::Resource::default(long_service_name)
  assert_eq(long_service_resource.service_name.length(), 800)
}

test "resource_with_attributes" {
  // 测试带属性的Resource
  let resource = common::Resource::default("test-service")
  
  // 在实际实现中，这里应该能添加属性
  // 现在我们验证默认属性
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
}

test "instrumentation_scope_creation" {
  // 测试InstrumentationScope创建
  let basic_scope = common::InstrumentationScope::{
    name: "test-instrumentation",
    version: None,
    schema_url: None
  }
  assert_eq(basic_scope.name, "test-instrumentation")
  assert_eq(basic_scope.version, None)
  assert_eq(basic_scope.schema_url, None)
  
  let full_scope = common::InstrumentationScope::{
    name: "full-instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/schema")
  }
  assert_eq(full_scope.name, "full-instrumentation")
  assert_eq(full_scope.version, Some("1.0.0"))
  assert_eq(full_scope.schema_url, Some("http://example.com/schema"))
  
  // 测试空名称
  let empty_name_scope = common::InstrumentationScope::{
    name: "",
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/schema")
  }
  assert_eq(empty_name_scope.name, "")
  
  // 测试长名称
  let long_name = "instrumentation-".repeat(50)
  let long_name_scope = common::InstrumentationScope::{
    name: long_name,
    version: None,
    schema_url: None
  }
  assert_eq(long_name_scope.name.length(), 600)
  
  // 测试特殊字符名称
  let special_name_scope = common::InstrumentationScope::{
    name: "instrumentation.special@chars#123",
    version: Some("2.0.0"),
    schema_url: Some("http://example.com/special-schema")
  }
  assert_eq(special_name_scope.name, "instrumentation.special@chars#123")
  assert_eq(special_name_scope.version, Some("2.0.0"))
  assert_eq(special_name_scope.schema_url, Some("http://example.com/special-schema"))
}

test "attributes_type_collection" {
  // 测试Attributes类型集合
  let empty_attributes : common::Attributes = []
  assert_eq(empty_attributes.length(), 0)
  
  let single_attribute : common::Attributes = [
    ("key1", common::AttributeValue::string("value1"))
  ]
  assert_eq(single_attribute.length(), 1)
  assert_eq(single_attribute[0].0, "key1")
  match single_attribute[0].1 {
    common::StringValue(v) => assert_eq(v, "value1")
    _ => assert(false, "Expected string value")
  }
  
  let mixed_attributes : common::Attributes = [
    ("string-key", common::AttributeValue::string("string-value")),
    ("int-key", common::AttributeValue::int(42L)),
    ("float-key", common::AttributeValue::float(3.14)),
    ("bool-key", common::AttributeValue::bool(true)),
    ("array-string-key", common::AttributeValue::array_string(["a", "b", "c"]))
  ]
  assert_eq(mixed_attributes.length(), 5)
  
  // 验证每个属性的类型
  match mixed_attributes[0].1 {
    common::StringValue(_) => ()
    _ => assert(false, "Expected string value")
  }
  
  match mixed_attributes[1].1 {
    common::IntValue(_) => ()
    _ => assert(false, "Expected int value")
  }
  
  match mixed_attributes[2].1 {
    common::FloatValue(_) => ()
    _ => assert(false, "Expected float value")
  }
  
  match mixed_attributes[3].1 {
    common::BoolValue(_) => ()
    _ => assert(false, "Expected bool value")
  }
  
  match mixed_attributes[4].1 {
    common::ArrayStringValue(_) => ()
    _ => assert(false, "Expected string array value")
  }
}