// 通用API属性测试用例
// 测试AttributeValue的各种类型和操作

use azimuth.telemetry.api.common.{AttributeValue, Attributes, Resource, InstrumentationScope}

test "attribute_value_string_type" {
  // 测试字符串类型的属性值
  
  let string_value = AttributeValue::string("test_value")
  let string_attr = ("test_key", string_value)
  
  // 验证属性键
  assert_eq(string_attr.0, "test_key")
  assert_eq(string_attr.0.length(), 9)
  
  // 验证属性值类型
  match string_attr.1 {
    StringValue(v) => {
      assert_eq(v, "test_value")
      assert_eq(v.length(), 10)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "attribute_value_numeric_types" {
  // 测试数值类型的属性值
  
  let int_value = AttributeValue::int(42L)
  let float_value = AttributeValue::float(3.14)
  
  // 验证整数值
  match int_value {
    IntValue(v) => assert_eq(v, 42L)
    _ => assert_eq(false, true)
  }
  
  // 验证浮点数值
  match float_value {
    FloatValue(v) => assert_eq(v > 3.0 && v < 3.2, true)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_boolean_type" {
  // 测试布尔类型的属性值
  
  let bool_true = AttributeValue::bool(true)
  let bool_false = AttributeValue::bool(false)
  
  // 验证true值
  match bool_true {
    BoolValue(v) => assert_eq(v, true)
    _ => assert_eq(false, true)
  }
  
  // 验证false值
  match bool_false {
    BoolValue(v) => assert_eq(v, false)
    _ => assert_eq(false, true)
  }
}

test "attribute_value_array_types" {
  // 测试数组类型的属性值
  
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  let int_array = AttributeValue::array_int([1L, 2L, 3L])
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = AttributeValue::array_bool([true, false, true])
  
  // 验证字符串数组
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[2], "c")
    }
    _ => assert_eq(false, true)
  }
  
  // 验证整数数组
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[2], 3L)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证浮点数数组
  match float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0] > 1.0 && arr[0] < 1.2, true)
      assert_eq(arr[2] > 3.2 && arr[2] < 3.4, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证布尔数组
  match bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => assert_eq(false, true)
  }
}

test "resource_creation_and_properties" {
  // 测试资源创建和属性
  
  let resource = Resource::default("test-service")
  
  // 验证基本属性
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 验证服务名称长度和内容
  assert_eq(resource.service_name.length(), 12)
  assert_eq(resource.service_name.has_prefix("test"), true)
  assert_eq(resource.service_name.has_suffix("service"), true)
}

test "instrumentation_scope_creation" {
  // 测试检测范围创建
  
  let scope = InstrumentationScope::{
    name: "test-instrument",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  // 验证基本属性
  assert_eq(scope.name, "test-instrument")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, Some("https://example.com/schema"))
  
  // 验证名称长度和内容
  assert_eq(scope.name.length(), 16)
  assert_eq(scope.name.has_prefix("test"), true)
  assert_eq(scope.name.has_suffix("instrument"), true)
  
  // 验证版本
  match scope.version {
    Some(v) => {
      assert_eq(v, "1.0.0")
      assert_eq(v.length(), 5)
    }
    None => assert_eq(false, true)
  }
  
  // 验证schema URL
  match scope.schema_url {
    Some(url) => {
      assert_eq(url.has_prefix("https"), true)
      assert_eq(url.has_suffix("schema"), true)
    }
    None => assert_eq(false, true)
  }
}

test "attributes_collection_operations" {
  // 测试属性集合操作
  
  let attributes : Attributes = [
    ("string_attr", AttributeValue::string("value")),
    ("int_attr", AttributeValue::int(123L)),
    ("float_attr", AttributeValue::float(45.6)),
    ("bool_attr", AttributeValue::bool(true))
  ]
  
  // 验证属性数量
  assert_eq(attributes.length(), 4)
  
  // 验证第一个属性
  assert_eq(attributes[0].0, "string_attr")
  match attributes[0].1 {
    StringValue(v) => assert_eq(v, "value")
    _ => assert_eq(false, true)
  }
  
  // 验证第二个属性
  assert_eq(attributes[1].0, "int_attr")
  match attributes[1].1 {
    IntValue(v) => assert_eq(v, 123L)
    _ => assert_eq(false, true)
  }
  
  // 验证第三个属性
  assert_eq(attributes[2].0, "float_attr")
  match attributes[2].1 {
    FloatValue(v) => assert_eq(v > 45.0 && v < 46.0, true)
    _ => assert_eq(false, true)
  }
  
  // 验证第四个属性
  assert_eq(attributes[3].0, "bool_attr")
  match attributes[3].1 {
    BoolValue(v) => assert_eq(v, true)
    _ => assert_eq(false, true)
  }
}