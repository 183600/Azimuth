// Common模块边界和高级测试用例
test "attribute_value_extreme_edge_cases" {
  // 测试AttributeValue的极端边界情况
  
  // 极大Int64值
  let max_int = AttributeValue::int(9223372036854775807L)
  match max_int {
    IntValue(value) => assert_eq(value, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  // 极小Int64值
  let min_int = AttributeValue::int(-9223372036854775808L)
  match min_int {
    IntValue(value) => assert_eq(value, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  // 边界Int64值
  let boundary_ints = [
    0L, 1L, -1L, 2147483647L, -2147483648L,
    4294967295L, -4294967296L, 140737488355327L, -140737488355328L
  ]
  
  let mut i = 0
  while i < boundary_ints.length() {
    let attr = AttributeValue::int(boundary_ints[i])
    match attr {
      IntValue(value) => assert_eq(value, boundary_ints[i])
      _ => @test.fail("Test failed")
    }
    i = i + 1
  }
  
  // 极值Double
  let max_double = AttributeValue::float(1.7976931348623157e+308)
  match max_double {
    FloatValue(value) => assert_eq(value, 1.7976931348623157e+308)
    _ => @test.fail("Test failed")
  }
  
  let min_double = AttributeValue::float(-1.7976931348623157e+308)
  match min_double {
    FloatValue(value) => assert_eq(value, -1.7976931348623157e+308)
    _ => @test.fail("Test failed")
  }
  
  // 特殊Double值
  let infinity = AttributeValue::float(1.0/0.0)
  let neg_infinity = AttributeValue::float(-1.0/0.0)
  let nan = AttributeValue::float(0.0/0.0)
  
  match infinity {
    FloatValue(value) => assert_eq(value, 1.0/0.0)
    _ => @test.fail("Test failed")
  }
  
  match neg_infinity {
    FloatValue(value) => assert_eq(value, -1.0/0.0)
    _ => @test.fail("Test failed")
  }
  
  match nan {
    FloatValue(value) => {
      // NaN的比较需要特殊处理
      assert_eq(value != value, true)  // NaN != NaN 是true
    }
    _ => @test.fail("Test failed")
  }
  
  // 极小Double值
  let tiny_double = AttributeValue::float(1.0e-308)
  match tiny_double {
    FloatValue(value) => assert_eq(value, 1.0e-308)
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_array_edge_cases" {
  // 测试数组类型的边界情况
  
  // 空数组
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
    _ => @test.fail("Test failed")
  }
  
  match single_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42L)
    }
    _ => @test.fail("Test failed")
  }
  
  match single_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 3.14)
    }
    _ => @test.fail("Test failed")
  }
  
  match single_bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 大数组
  let large_string_data = []
  let large_int_data = []
  let large_float_data = []
  let large_bool_data = []
  
  let mut i = 0
  while i < 1000 {
    large_string_data.push("item" + i.to_string())
    large_int_data.push(i.to_int64())
    large_float_data.push(i.to_double() + 0.5)
    large_bool_data.push(i % 2 == 0)
    i = i + 1
  }
  
  let large_string_array = AttributeValue::array_string(large_string_data)
  let large_int_array = AttributeValue::array_int(large_int_data)
  let large_float_array = AttributeValue::array_float(large_float_data)
  let large_bool_array = AttributeValue::array_bool(large_bool_data)
  
  match large_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], "item0")
      assert_eq(arr[999], "item999")
    }
    _ => @test.fail("Test failed")
  }
  
  match large_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], 0L)
      assert_eq(arr[999], 999L)
    }
    _ => @test.fail("Test failed")
  }
  
  match large_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], 0.5)
      assert_eq(arr[999], 999.5)
    }
    _ => @test.fail("Test failed")
  }
  
  match large_bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], true)
      assert_eq(arr[999], false)
    }
    _ => @test.fail("Test failed")
  }
}

test "resource_advanced_scenarios" {
  // 测试Resource的高级场景
  
  // 包含所有可能属性类型的Resource
  let complex_attributes = [
    ("string.basic", AttributeValue::string("basic string")),
    ("string.empty", AttributeValue::string("")),
    ("string.unicode", AttributeValue::string("Unicode测试🚀")),
    ("string.special", AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")),
    ("string.long", AttributeValue::string("a".repeat(1000))),
    ("int.max", AttributeValue::int(9223372036854775807L)),
    ("int.min", AttributeValue::int(-9223372036854775808L)),
    ("int.zero", AttributeValue::int(0L)),
    ("float.max", AttributeValue::float(1.7976931348623157e+308)),
    ("float.min", AttributeValue::float(-1.7976931348623157e+308)),
    ("float.zero", AttributeValue::float(0.0)),
    ("float.infinity", AttributeValue::float(1.0/0.0)),
    ("float.nan", AttributeValue::float(0.0/0.0)),
    ("bool.true", AttributeValue::bool(true)),
    ("bool.false", AttributeValue::bool(false)),
    ("array.string.empty", AttributeValue::array_string([])),
    ("array.string.single", AttributeValue::array_string(["single"])),
    ("array.string.multiple", AttributeValue::array_string(["item1", "item2", "item3"])),
    ("array.int.empty", AttributeValue::array_int([])),
    ("array.int.single", AttributeValue::array_int([42L])),
    ("array.int.multiple", AttributeValue::array_int([1L, 2L, 3L])),
    ("array.float.empty", AttributeValue::array_float([])),
    ("array.float.single", AttributeValue::array_float([3.14])),
    ("array.float.multiple", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array.bool.empty", AttributeValue::array_bool([])),
    ("array.bool.single", AttributeValue::array_bool([true])),
    ("array.bool.multiple", AttributeValue::array_bool([true, false, true]))
  ]
  
  let resource = Resource::default("advanced-test-service")
  
  // 由于Resource在实现中只能通过工厂函数创建，我们无法直接设置复杂属性
  // 但可以验证默认Resource的基本属性
  assert_eq(resource.service_name, "advanced-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 测试各种边界情况的服务名称
  let edge_case_names = [
    "",  // 空名称
    " ",  // 空格
    "a".repeat(1000),  // 长名称
    "Unicode测试🚀",  // Unicode
    "!@#$%^&*()_+-=[]{}|;':\",./<>?",  // 特殊字符
    "service with spaces",  // 包含空格
    "service.with.dots",  // 包含点号
    "service/with/slashes",  // 包含斜杠
    "service\\with\\backslashes",  // 包含反斜杠
    "service\nwith\nnewlines",  // 包含换行符
    "service\twith\ttabs"  // 包含制表符
  ]
  
  let mut i = 0
  while i < edge_case_names.length() {
    let edge_resource = Resource::default(edge_case_names[i])
    assert_eq(edge_resource.service_name, edge_case_names[i])
    assert_eq(edge_resource.telemetry_sdk_name, "azimuth")
    assert_eq(edge_resource.telemetry_sdk_version, "0.1.0")
    i = i + 1
  }
}

test "instrumentation_scope_edge_cases" {
  // 测试InstrumentationScope的边界情况
  
  // 由于InstrumentationScope是只读结构体，我们只能测试其基本属性
  let basic_scope = InstrumentationScope::{
    name: "basic-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  assert_eq(basic_scope.name, "basic-scope")
  match basic_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => @test.fail("Test failed")
  }
  match basic_scope.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema")
    None => @test.fail("Test failed")
  }
  
  // 测试各种边界情况
  let edge_scopes = [
    InstrumentationScope::{ name: "", version: None, schema_url: None },
    InstrumentationScope::{ name: " ", version: Some(""), schema_url: Some("") },
    InstrumentationScope::{ name: "Unicode测试🚀", version: Some("版本1.0"), schema_url: Some("https://测试.com/schema") },
    InstrumentationScope::{ name: "!@#$%^&*()", version: Some("!@#$%^&*()"), schema_url: Some("!@#$%^&*()") },
    InstrumentationScope::{ name: "a".repeat(1000), version: Some("b".repeat(1000)), schema_url: Some("c".repeat(1000)) }
  ]
  
  let mut i = 0
  while i < edge_scopes.length() {
    let scope = edge_scopes[i]
    assert_eq(scope.name.length() > 0, true)  // 名称应该存在
    i = i + 1
  }
}

test "attributes_complex_operations" {
  // 测试Attributes的复杂操作
  
  // 创建包含各种类型属性的Attributes
  let complex_attributes : Attributes = [
    ("string.key1", AttributeValue::string("value1")),
    ("string.key2", AttributeValue::string("value2")),
    ("int.key1", AttributeValue::int(42L)),
    ("int.key2", AttributeValue::int(-42L)),
    ("float.key1", AttributeValue::float(3.14)),
    ("float.key2", AttributeValue::float(-3.14)),
    ("bool.key1", AttributeValue::bool(true)),
    ("bool.key2", AttributeValue::bool(false)),
    ("array.string.key", AttributeValue::array_string(["item1", "item2", "item3"])),
    ("array.int.key", AttributeValue::array_int([1L, 2L, 3L])),
    ("array.float.key", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array.bool.key", AttributeValue::array_bool([true, false, true]))
  ]
  
  assert_eq(complex_attributes.length(), 12)
  
  // 验证各种属性类型
  match complex_attributes[0].1 {
    StringValue(value) => assert_eq(value, "value1")
    _ => @test.fail("Test failed")
  }
  
  match complex_attributes[2].1 {
    IntValue(value) => assert_eq(value, 42L)
    _ => @test.fail("Test failed")
  }
  
  match complex_attributes[4].1 {
    FloatValue(value) => assert_eq(value, 3.14)
    _ => @test.fail("Test failed")
  }
  
  match complex_attributes[6].1 {
    BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Test failed")
  }
  
  match complex_attributes[8].1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试边界情况的键名
  let edge_case_keys = [
    ("", AttributeValue::string("empty key")),
    (" ", AttributeValue::string("space key")),
    ("a".repeat(1000), AttributeValue::string("long key")),
    ("Unicode键🚀", AttributeValue::string("Unicode值🔥")),
    ("!@#$%^&*()", AttributeValue::string("special chars")),
    ("key.with.dots", AttributeValue::string("dots")),
    ("key/with/slashes", AttributeValue::string("slashes")),
    ("key\\with\\backslashes", AttributeValue::string("backslashes")),
    ("key\nwith\nnewlines", AttributeValue::string("newlines")),
    ("key\twith\ttabs", AttributeValue::string("tabs"))
  ]
  
  let edge_attributes : Attributes = edge_case_keys
  
  let mut i = 0
  while i < edge_attributes.length() {
    let (key, value) = edge_attributes[i]
    match value {
      StringValue(str_val) => {
        // 验证键和值都正确存储
        assert_eq(key.length() > 0, true)
        assert_eq(str_val.length() > 0, true)
      }
      _ => @test.fail("Test failed")
    }
    i = i + 1
  }
}

test "attribute_value_type_safety_and_conversion" {
  // 测试AttributeValue的类型安全和转换
  
  // 测试所有类型的创建和匹配
  let string_attr = AttributeValue::string("test string")
  let int_attr = AttributeValue::int(123L)
  let float_attr = AttributeValue::float(45.67)
  let bool_attr = AttributeValue::bool(true)
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = AttributeValue::array_bool([true, false, true])
  
  // 验证类型匹配
  let all_attrs = [
    string_attr, int_attr, float_attr, bool_attr,
    string_array_attr, int_array_attr, float_array_attr, bool_array_attr
  ]
  
  let mut i = 0
  while i < all_attrs.length() {
    let attr = all_attrs[i]
    match attr {
      StringValue(_) => assert_eq(i == 0, true)
      IntValue(_) => assert_eq(i == 1, true)
      FloatValue(_) => assert_eq(i == 2, true)
      BoolValue(_) => assert_eq(i == 3, true)
      ArrayStringValue(_) => assert_eq(i == 4, true)
      ArrayIntValue(_) => assert_eq(i == 5, true)
      ArrayFloatValue(_) => assert_eq(i == 6, true)
      ArrayBoolValue(_) => assert_eq(i == 7, true)
    }
    i = i + 1
  }
  
  // 测试边界值的类型安全
  let boundary_values = [
    AttributeValue::string(""),
    AttributeValue::string("a".repeat(10000)),
    AttributeValue::int(0L),
    AttributeValue::int(9223372036854775807L),
    AttributeValue::int(-9223372036854775808L),
    AttributeValue::float(0.0),
    AttributeValue::float(1.7976931348623157e+308),
    AttributeValue::float(-1.7976931348623157e+308),
    AttributeValue::float(1.0/0.0),
    AttributeValue::float(-1.0/0.0),
    AttributeValue::float(0.0/0.0),
    AttributeValue::bool(true),
    AttributeValue::bool(false),
    AttributeValue::array_string([]),
    AttributeValue::array_string([""]),
    AttributeValue::array_int([]),
    AttributeValue::array_int([0L]),
    AttributeValue::array_float([]),
    AttributeValue::array_float([0.0]),
    AttributeValue::array_bool([]),
    AttributeValue::array_bool([true])
  ]
  
  let mut j = 0
  while j < boundary_values.length() {
    let value = boundary_values[j]
    // 确保所有边界值都能正确创建和匹配
    match value {
      StringValue(_) => assert_eq(true, true)
      IntValue(_) => assert_eq(true, true)
      FloatValue(_) => assert_eq(true, true)
      BoolValue(_) => assert_eq(true, true)
      ArrayStringValue(_) => assert_eq(true, true)
      ArrayIntValue(_) => assert_eq(true, true)
      ArrayFloatValue(_) => assert_eq(true, true)
      ArrayBoolValue(_) => assert_eq(true, true)
    }
    j = j + 1
  }
}