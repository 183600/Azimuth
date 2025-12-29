// API Common模块的属性值转换测试

test "attribute_value_string_conversion" {
  // 测试字符串属性值的创建和转换
  
  let str_value = "http.request.method"
  let attr_value = common::AttributeValue::string(str_value)
  
  // 验证属性值类型
  match attr_value {
    StringValue(s) => assert_eq(s, str_value)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试空字符串
  let empty_value = ""
  let empty_attr = common::AttributeValue::string(empty_value)
  
  match empty_attr {
    StringValue(s) => assert_eq(s, "")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试特殊字符字符串
  let special_value = "test@#$%^&*()_+-={}[]|\\:;\"'<>?,./"
  let special_attr = common::AttributeValue::string(special_value)
  
  match special_attr {
    StringValue(s) => assert_eq(s, special_value)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "attribute_value_numeric_conversion" {
  // 测试数值属性值的创建和转换
  
  // 测试整数
  let int_val = 42L
  let int_attr = common::AttributeValue::int(int_val)
  
  match int_attr {
    IntValue(i) => assert_eq(i, int_val)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试负整数
  let neg_int_val = -123L
  let neg_int_attr = common::AttributeValue::int(neg_int_val)
  
  match neg_int_attr {
    IntValue(i) => assert_eq(i, neg_int_val)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试零值
  let zero_val = 0L
  let zero_attr = common::AttributeValue::int(zero_val)
  
  match zero_attr {
    IntValue(i) => assert_eq(i, zero_val)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试浮点数
  let float_val = 3.14159
  let float_attr = common::AttributeValue::float(float_val)
  
  match float_attr {
    FloatValue(f) => assert_eq(f, float_val)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试负浮点数
  let neg_float_val = -2.71828
  let neg_float_attr = common::AttributeValue::float(neg_float_val)
  
  match neg_float_attr {
    FloatValue(f) => assert_eq(f, neg_float_val)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试零浮点数
  let zero_float_val = 0.0
  let zero_float_attr = common::AttributeValue::float(zero_float_val)
  
  match zero_float_attr {
    FloatValue(f) => assert_eq(f, zero_float_val)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "attribute_value_boolean_conversion" {
  // 测试布尔属性值的创建和转换
  
  // 测试true
  let true_val = true
  let true_attr = common::AttributeValue::bool(true_val)
  
  match true_attr {
    BoolValue(b) => assert_eq(b, true_val)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试false
  let false_val = false
  let false_attr = common::AttributeValue::bool(false_val)
  
  match false_attr {
    BoolValue(b) => assert_eq(b, false_val)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "attribute_value_array_conversion" {
  // 测试数组属性值的创建和转换
  
  // 测试字符串数组
  let str_array = ["GET", "POST", "PUT", "DELETE"]
  let str_array_attr = common::AttributeValue::array_string(str_array)
  
  match str_array_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], "GET")
      assert_eq(arr[1], "POST")
      assert_eq(arr[2], "PUT")
      assert_eq(arr[3], "DELETE")
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试空字符串数组
  let empty_str_array = []  // Array[String]
  let empty_str_array_attr = common::AttributeValue::array_string(empty_str_array)
  
  match empty_str_array_attr {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试整数数组
  let int_array = [1L, 2L, 3L, 4L, 5L]
  let int_array_attr = common::AttributeValue::array_int(int_array)
  
  match int_array_attr {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1L)
      assert_eq(arr[2], 3L)
      assert_eq(arr[4], 5L)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试浮点数数组
  let float_array = [1.1, 2.2, 3.3]
  let float_array_attr = common::AttributeValue::array_float(float_array)
  
  match float_array_attr {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试布尔数组
  let bool_array = [true, false, true]
  let bool_array_attr = common::AttributeValue::array_bool(bool_array)
  
  match bool_array_attr {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}

test "resource_creation_and_default" {
  // 测试Resource创建和默认值
  
  let service_name = "user-service"
  let resource = common::Resource::default(service_name)
  
  // 验证基本字段
  assert_eq(resource.service_name, service_name)
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 测试带有版本的服务名
  let service_name2 = "payment-service"
  let resource_with_version = common::Resource::{
    service_name: service_name2,
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: []
  }
  
  assert_eq(resource_with_version.service_name, service_name2)
  match resource_with_version.service_version {
    Some(v) => assert_eq(v, "1.2.3")
    None => assert_eq(false, true)  // 不应该到达这里
  }
}

test "instrumentation_scope_creation" {
  // 测试InstrumentationScope创建
  
  let scope_name = "otel.instrumentation.http"
  let scope_version = Some("1.0.0")
  let schema_url = Some("https://opentelemetry.io/schemas/1.20.0")
  
  let scope = common::InstrumentationScope::{
    name: scope_name,
    version: scope_version,
    schema_url: schema_url
  }
  
  assert_eq(scope.name, scope_name)
  match scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  match scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试最小配置的scope
  let minimal_scope = common::InstrumentationScope::{
    name: "minimal.scope",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "minimal.scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

test "attributes_operations" {
  // 测试属性操作
  
  let attributes = [
    ("http.method", common::AttributeValue::string("GET")),
    ("http.status_code", common::AttributeValue::int(200L)),
    ("http.success", common::AttributeValue::bool(true)),
    ("http.duration", common::AttributeValue::float(123.45))
  ]
  
  // 验证属性数量
  assert_eq(attributes.length(), 4)
  
  // 验证各个属性
  assert_eq(attributes[0].0, "http.method")
  match attributes[0].1 {
    StringValue(s) => assert_eq(s, "GET")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(attributes[1].0, "http.status_code")
  match attributes[1].1 {
    IntValue(i) => assert_eq(i, 200L)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(attributes[2].0, "http.success")
  match attributes[2].1 {
    BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  assert_eq(attributes[3].0, "http.duration")
  match attributes[3].1 {
    FloatValue(f) => assert_eq(f, 123.45)
    _ => assert_eq(false, true)  // 不应该到达这里
  }
}