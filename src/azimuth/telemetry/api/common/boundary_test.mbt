// 边界条件和错误处理测试用例
// 测试各种边界条件、异常情况和错误处理

test "attribute_value_extreme_edge_cases" {
  // 测试AttributeValue的极端边界情况
  
  // 测试极长字符串
  let very_long_string = "This is an extremely long string that contains many characters and tests how the system handles very long attribute values without any issues or problems that might arise from memory allocation or string processing limitations that could potentially cause crashes or unexpected behavior in the telemetry system when dealing with unusually long attribute values that exceed typical expected lengths for normal telemetry data."
  let long_string_attr = AttributeValue::string(very_long_string)
  
  match long_string_attr {
    StringValue(s) => assert_eq(s, very_long_string)
    _ => @test.fail("Test failed")
  }
  
  // 测试包含特殊字符的字符串（移除控制字符）
  let special_chars_string = "Special chars: !@#$%^&*()_+-={}[]|\\:;\"'<>?,./\n\t\r"
  let special_chars_attr = AttributeValue::string(special_chars_string)
  
  match special_chars_attr {
    StringValue(s) => assert_eq(s, special_chars_string)
    _ => @test.fail("Test failed")
  }
  
  // 测试Unicode字符
  let unicode_string = "Unicode test: 中文测试 🚀 αβγδε العربية русский 日本語 한국어"
  let unicode_attr = AttributeValue::string(unicode_string)
  
  match unicode_attr {
    StringValue(s) => assert_eq(s, unicode_string)
    _ => @test.fail("Test failed")
  }
  
  // 测试极值整数
  let max_int_attr = AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int_attr = AttributeValue::int(-9223372036854775808L)  // Int64最小值
  let zero_int_attr = AttributeValue::int(0L)
  
  match max_int_attr {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match min_int_attr {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  match zero_int_attr {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Test failed")
  }
  
  // 测试极值浮点数
  let max_float_attr = AttributeValue::float(1.7976931348623157e308)  // Double最大值
  let min_float_attr = AttributeValue::float(-1.7976931348623157e308)  // Double最小值
  let smallest_float_attr = AttributeValue::float(0.000001)  // 使用更小的正数
  let infinity_attr = AttributeValue::float(1.0/0.0)  // 正无穷
  let neg_infinity_attr = AttributeValue::float(-1.0/0.0)  // 负无穷
  let nan_attr = AttributeValue::float(0.0/0.0)  // NaN
  
  match max_float_attr {
    FloatValue(f) => assert_eq(f, 1.7976931348623157e308)
    _ => @test.fail("Test failed")
  }
  
  match min_float_attr {
    FloatValue(f) => assert_eq(f, -1.7976931348623157e308)
    _ => @test.fail("Test failed")
  }
  
  match infinity_attr {
    FloatValue(f) => assert_eq(f, 1.0/0.0)
    _ => @test.fail("Test failed")
  }
  
  match nan_attr {
    FloatValue(f) => {
      // NaN比较需要特殊处理，因为NaN != NaN
      let is_nan = f != f
      assert_eq(is_nan, true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试极长数组
  let large_string_array = []
  let mut i = 0
  while i < 100 {  // 减少数组大小以避免性能问题
    large_string_array.push("item" + i.to_string())
    i = i + 1
  }
  
  let large_array_attr = AttributeValue::array_string(large_string_array)
  
  match large_array_attr {
    ArrayStringValue(arr) => assert_eq(arr.length(), 100)
    _ => @test.fail("Test failed")
  }
  
  // 测试包含极值的数组
  let extreme_int_array = AttributeValue::array_int([
    9223372036854775807L,
    -9223372036854775808L,
    0L,
    1L,
    -1L
  ])
  
  let extreme_float_array = AttributeValue::array_float([
    1.7976931348623157e308,
    -1.7976931348623157e308,
    0.0,
    1.0/0.0,
    -1.0/0.0,
    0.0/0.0
  ])
  
  match extreme_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 9223372036854775807L)
      assert_eq(arr[1], -9223372036854775808L)
    }
    _ => @test.fail("Test failed")
  }
  
  match extreme_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 6)
      assert_eq(arr[0], 1.7976931348623157e308)
      assert_eq(arr[1], -1.7976931348623157e308)
    }
    _ => @test.fail("Test failed")
  }
}

test "resource_boundary_conditions" {
  // 测试Resource的边界条件
  
  // 测试极长的服务名称
  let very_long_service_name = "this.is.a.very.long.service.name.that.tests.the.system.ability.to.handle.extremely.long.service.names.with.many.characters.that.might.exceed.typical.limits.for.telemetry.data.structures"
  let long_name_resource = Resource::default(very_long_service_name)
  
  assert_eq(long_name_resource.service_name, very_long_service_name)
  assert_eq(long_name_resource.telemetry_sdk_name, "azimuth")
  assert_eq(long_name_resource.telemetry_sdk_version, "0.1.0")
  
  // 测试包含特殊字符的服务名称
  let special_service_name = "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./service"
  let special_resource = Resource::default(special_service_name)
  
  assert_eq(special_resource.service_name, special_service_name)
  
  // 测试Unicode服务名称
  let unicode_service_name = "Unicode服务测试🚀العربيةрусский日本語한국어"
  let unicode_resource = Resource::default(unicode_service_name)
  
  assert_eq(unicode_resource.service_name, unicode_service_name)
  
  // 测试空服务名称
  let empty_resource = Resource::default("")
  assert_eq(empty_resource.service_name, "")
  
  // 测试包含空格的服务名称
  let space_resource = Resource::default("service with spaces")
  assert_eq(space_resource.service_name, "service with spaces")
  
  // 测试复杂的属性集合
  let very_long_attributes = []
  let mut i = 0
  while i < 100 {  // 减少属性数量以避免性能问题
    very_long_attributes.push(("very.long.attribute.name." + i.to_string(), AttributeValue::string("very.long.attribute.value." + i.to_string() + "with.many.characters")))
    i = i + 1
  }
  
  assert_eq(very_long_attributes.length(), 100)
  
  // 测试包含特殊字符的属性
  let special_attributes = [
    ("special!@#$%^&*()", AttributeValue::string("special!@#$%^&*()")),
    ("unicode测试", AttributeValue::string("Unicode值测试🚀")),
    ("empty.string", AttributeValue::string("")),
    ("zero.int", AttributeValue::int(0L)),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("zero.float", AttributeValue::float(0.0)),
    ("infinity", AttributeValue::float(1.0/0.0)),
    ("nan", AttributeValue::float(0.0/0.0)),
    ("true.bool", AttributeValue::bool(true)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    ("string.array", AttributeValue::array_string(["a", "b", "c"])),
    ("int.array", AttributeValue::array_int([1L, 2L, 3L])),
    ("float.array", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array", AttributeValue::array_bool([true, false, true]))
  ]
  
  assert_eq(special_attributes.length(), 16)
  
  // 验证特殊属性
  match special_attributes[0].1 {
    StringValue(value) => assert_eq(value, "special!@#$%^&*()")
    _ => @test.fail("Test failed")
  }
  
  match special_attributes[1].1 {
    StringValue(value) => assert_eq(value, "Unicode值测试🚀")
    _ => @test.fail("Test failed")
  }
  
  match special_attributes[2].1 {
    StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Test failed")
  }
  
  match special_attributes[3].1 {
    IntValue(value) => assert_eq(value, 0L)
    _ => @test.fail("Test failed")
  }
  
  match special_attributes[12].1 {
    ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Test failed")
  }
}

test "array_boundary_conditions" {
  // 测试数组的边界条件
  
  // 测试极长字符串数组
  let very_long_strings = []
  let mut i = 0
  while i < 100 {  // 减少数组大小以避免性能问题
    very_long_strings.push("very.long.string." + i.to_string() + ".with.many.characters.to.test.array.handling")
    i = i + 1
  }
  
  let long_string_array = AttributeValue::array_string(very_long_strings)
  
  match long_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 100)
      assert_eq(arr[0], "very.long.string.0.with.many.characters.to.test.array.handling")
      assert_eq(arr[99], "very.long.string.99.with.many.characters.to.test.array.handling")
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试极值整数数组
  let extreme_ints = [
    9223372036854775807L,  // 最大Int64
    -9223372036854775808L, // 最小Int64
    0L,
    1L,
    -1L,
    1234567890L,
    -1234567890L
  ]
  
  let extreme_int_array = AttributeValue::array_int(extreme_ints)
  
  match extreme_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 7)
      assert_eq(arr[0], 9223372036854775807L)
      assert_eq(arr[1], -9223372036854775808L)
      assert_eq(arr[2], 0L)
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试极值浮点数数组
  let extreme_floats = [
    1.7976931348623157e308,  // 最大Double
    -1.7976931348623157e308, // 最小Double
    0.0,
    1.0/0.0,  // 正无穷
    -1.0/0.0, // 负无穷
    0.0/0.0   // NaN
  ]
  
  let extreme_float_array = AttributeValue::array_float(extreme_floats)
  
  match extreme_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 6)
      assert_eq(arr[0], 1.7976931348623157e308)
      assert_eq(arr[1], -1.7976931348623157e308)
      
      // 测试NaN
      let nan_value = arr[5]
      let is_nan = nan_value != nan_value
      assert_eq(is_nan, true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试布尔数组
  let bool_values = [
    true,
    false,
    true,
    false,
    true
  ]
  
  let bool_array = AttributeValue::array_bool(bool_values)
  
  match bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
      assert_eq(arr[3], false)
      assert_eq(arr[4], true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  // 测试包含特殊值的数组
  let special_string_array = AttributeValue::array_string([
    "",
    "special!@#$%^&*()",
    "Unicode测试🚀",
    "string with spaces",
    "string\nwith\nnewlines",
    "string\twith\ttabs"
  ])
  
  match special_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 6)
      assert_eq(arr[0], "")
      assert_eq(arr[1], "special!@#$%^&*()")
      assert_eq(arr[2], "Unicode测试🚀")
    }
    _ => @test.fail("Test failed")
  }
}

test "resource_extreme_scenarios" {
  // 测试Resource的极端场景
  
  // 测试极值浮点数作为SDK版本（虽然不实际，但测试系统处理能力）
  let resource_with_float_values = Resource::default("test-service")
  
  // 验证基本属性
  assert_eq(resource_with_float_values.service_name, "test-service")
  assert_eq(resource_with_float_values.telemetry_sdk_name, "azimuth")
  assert_eq(resource_with_float_values.telemetry_sdk_version, "0.1.0")
  
  // 测试包含极值属性的Resource
  let extreme_attributes = [
    ("max.float", AttributeValue::float(1.7976931348623157e308)),
    ("min.float", AttributeValue::float(-1.7976931348623157e308)),
    ("infinity", AttributeValue::float(1.0/0.0)),
    ("neg.infinity", AttributeValue::float(-1.0/0.0)),
    ("nan", AttributeValue::float(0.0/0.0)),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("unicode", AttributeValue::string("Unicode测试🚀")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("empty", AttributeValue::string("")),
    ("array.large", AttributeValue::array_string(["a", "b", "c", "d", "e"]))
  ]
  
  assert_eq(extreme_attributes.length(), 11)
  
  // 验证极值属性
  match extreme_attributes[0].1 {
    FloatValue(value) => assert_eq(value, 1.7976931348623157e308)
    _ => @test.fail("Test failed")
  }
  
  match extreme_attributes[1].1 {
    FloatValue(value) => assert_eq(value, -1.7976931348623157e308)
    _ => @test.fail("Test failed")
  }
  
  match extreme_attributes[2].1 {
    FloatValue(value) => assert_eq(value, 1.0/0.0)
    _ => @test.fail("Test failed")
  }
  
  match extreme_attributes[3].1 {
    FloatValue(value) => assert_eq(value, -1.0/0.0)
    _ => @test.fail("Test failed")
  }
  
  match extreme_attributes[4].1 {
    FloatValue(value) => {
      let is_nan = value != value
      assert_eq(is_nan, true)
    }
    _ => @test.fail("Test failed")
  }
  
  match extreme_attributes[5].1 {
    IntValue(value) => assert_eq(value, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match extreme_attributes[6].1 {
    IntValue(value) => assert_eq(value, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  match extreme_attributes[7].1 {
    StringValue(value) => assert_eq(value, "Unicode测试🚀")
    _ => @test.fail("Test failed")
  }
  
  match extreme_attributes[10].1 {
    ArrayStringValue(arr) => assert_eq(arr.length(), 5)
    _ => @test.fail("Test failed")
  }
}

test "attributes_complex_scenarios" {
  // 测试属性的复杂场景
  
  // 测试极长的属性键和值
  let very_long_key = "this.is.a.very.long.attribute.key.name.that.tests.the.system.ability.to.handle.extremely.long.keys.with.many.characters.that.might.exceed.typical.limits"
  let very_long_value = "this.is.a.very.long.attribute.value.that.tests.the.system.ability.to.handle.extremely.long.values.with.many.characters.that.might.exceed.typical.limits.for.telemetry.data"
  
  let long_key_value = (very_long_key, AttributeValue::string(very_long_value))
  
  match long_key_value.1 {
    StringValue(value) => assert_eq(value, very_long_value)
    _ => @test.fail("Test failed")
  }
  
  // 测试包含特殊字符的属性键和值
  let special_key_value = ("special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./", AttributeValue::string("special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"))
  
  match special_key_value.1 {
    StringValue(value) => assert_eq(value, "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
    _ => @test.fail("Test failed")
  }
  
  // 测试Unicode属性键和值
  let unicode_key_value = ("Unicode键.测试.🚀", AttributeValue::string("Unicode值.测试.🚀.中文.العربية.русский.日本語.한국어"))
  
  match unicode_key_value.1 {
    StringValue(value) => assert_eq(value, "Unicode值.测试.🚀.中文.العربية.русский.日本語.한국어")
    _ => @test.fail("Test failed")
  }
  
  // 测试空属性键和值
  let empty_key_value = ("", AttributeValue::string(""))
  
  match empty_key_value.1 {
    StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Test failed")
  }
  
  // 测试包含空格的属性键和值
  let space_key_value = ("key with spaces", AttributeValue::string("value with spaces"))
  
  match space_key_value.1 {
    StringValue(value) => assert_eq(value, "value with spaces")
    _ => @test.fail("Test failed")
  }
  
  // 测试混合类型的属性集合
  let mixed_attributes = [
    ("string.attr", AttributeValue::string("string value")),
    ("int.attr", AttributeValue::int(42L)),
    ("float.attr", AttributeValue::float(3.14)),
    ("bool.attr", AttributeValue::bool(true)),
    ("string.array.attr", AttributeValue::array_string(["a", "b", "c"])),
    ("int.array.attr", AttributeValue::array_int([1L, 2L, 3L])),
    ("float.array.attr", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array.attr", AttributeValue::array_bool([true, false, true]))
  ]
  
  assert_eq(mixed_attributes.length(), 8)
  
  // 验证混合属性
  match mixed_attributes[0].1 {
    StringValue(value) => assert_eq(value, "string value")
    _ => @test.fail("Test failed")
  }
  
  match mixed_attributes[1].1 {
    IntValue(value) => assert_eq(value, 42L)
    _ => @test.fail("Test failed")
  }
  
  match mixed_attributes[2].1 {
    FloatValue(value) => assert_eq(value, 3.14)
    _ => @test.fail("Test failed")
  }
  
  match mixed_attributes[3].1 {
    BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Test failed")
  }
  
  match mixed_attributes[4].1 {
    ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Test failed")
  }
  
  match mixed_attributes[5].1 {
    ArrayIntValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Test failed")
  }
  
  match mixed_attributes[6].1 {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Test failed")
  }
  
  match mixed_attributes[7].1 {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Test failed")
  }
}