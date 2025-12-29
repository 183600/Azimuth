// Common API 类型转换和验证测试用例

test "attribute_value_type_conversions" {
  // 测试属性值的类型转换
  
  // 测试字符串转换
  let string_attr = AttributeValue::string("test value")
  match string_attr {
    StringValue(s) => {
      assert_eq(s, "test value")
      assert_eq(s.length(), 10)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试整数转换
  let int_attr = AttributeValue::int(42L)
  match int_attr {
    IntValue(i) => {
      assert_eq(i, 42L)
      assert_eq(i > 0L, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试浮点数转换
  let float_attr = AttributeValue::float(3.14159)
  match float_attr {
    FloatValue(f) => {
      assert_eq(f > 3.14 && f < 3.15, true)
      assert_eq(f.to_string().contains("."), true)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试布尔值转换
  let bool_attr = AttributeValue::bool(true)
  match bool_attr {
    BoolValue(b) => {
      assert_eq(b, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试数组转换
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  match string_array_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_eq(false, true)
  }
  
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])
  match int_array_attr {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1L)
      assert_eq(arr[4], 5L)
    }
    _ => assert_eq(false, true)
  }
  
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  match float_array_attr {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0] > 1.0 && arr[0] < 1.2, true)
      assert_eq(arr[2] > 3.2 && arr[2] < 3.4, true)
    }
    _ => assert_eq(false, true)
  }
  
  let bool_array_attr = AttributeValue::array_bool([true, false, true, true])
  match bool_array_attr {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
      assert_eq(arr[3], true)
    }
    _ => assert_eq(false, true)
  }
}

test "attribute_value_edge_cases" {
  // 测试属性值的边界情况
  
  // 测试空字符串
  let empty_string = AttributeValue::string("")
  match empty_string {
    StringValue(s) => {
      assert_eq(s.length(), 0)
      assert_eq(s, "")
    }
    _ => assert_eq(false, true)
  }
  
  // 测试极大整数
  let max_int = AttributeValue::int(9223372036854775807L)  // Int64.max
  match max_int {
    IntValue(i) => {
      assert_eq(i, 9223372036854775807L)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试极小整数
  let min_int = AttributeValue::int(-9223372036854775808L)  // Int64.min
  match min_int {
    IntValue(i) => {
      assert_eq(i, -9223372036854775808L)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试特殊浮点值
  let zero_float = AttributeValue::float(0.0)
  match zero_float {
    FloatValue(f) => {
      assert_eq(f, 0.0)
    }
    _ => assert_eq(false, true)
  }
  
  let negative_float = AttributeValue::float(-123.456)
  match negative_float {
    FloatValue(f) => {
      assert_eq(f < 0.0, true)
      assert_eq(f > -200.0, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试空数组
  let empty_string_array = AttributeValue::array_string([])
  match empty_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 0)
    }
    _ => assert_eq(false, true)
  }
  
  let empty_int_array = AttributeValue::array_int([])
  match empty_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 0)
    }
    _ => assert_eq(false, true)
  }
}

test "resource_creation_and_validation" {
  // 测试资源创建和验证
  
  // 测试默认资源创建
  let default_resource = Resource::default("test-service")
  assert_eq(default_resource.service_name, "test-service")
  assert_eq(default_resource.service_version, None)
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.attributes.length(), 0)
  
  // 测试带版本的资源创建
  let resource_with_version = Resource::default("payment-service")
  assert_eq(resource_with_version.service_name, "payment-service")
  
  // 测试资源属性验证
  let resource_with_attrs = Resource::{
    service_name: "order-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.namespace", AttributeValue::string("ecommerce")),
      ("service.instance.id", AttributeValue::string("instance-12345")),
      ("deployment.environment", AttributeValue::string("production"))
    ]
  }
  
  assert_eq(resource_with_attrs.service_name, "order-service")
  assert_eq(resource_with_attrs.service_version, Some("2.1.0"))
  assert_eq(resource_with_attrs.attributes.length(), 3)
  
  // 验证属性内容
  let mut found_namespace = false
  let mut found_instance = false
  let mut found_environment = false
  
  let mut i = 0
  while i < resource_with_attrs.attributes.length() {
    let (key, value) = resource_with_attrs.attributes[i]
    match (key, value) {
      ("service.namespace", StringValue(ns)) => {
        assert_eq(ns, "ecommerce")
        found_namespace = true
      }
      ("service.instance.id", StringValue(instance)) => {
        assert_eq(instance, "instance-12345")
        found_instance = true
      }
      ("deployment.environment", StringValue(env)) => {
        assert_eq(env, "production")
        found_environment = true
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(found_namespace, true)
  assert_eq(found_instance, true)
  assert_eq(found_environment, true)
}

test "instrumentation_scope_validation" {
  // 测试仪表化作用域验证
  
  // 测试最小仪表化作用域
  let minimal_scope = InstrumentationScope::{
    name: "test-instrumentation",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "test-instrumentation")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
  
  // 测试完整仪表化作用域
  let full_scope = InstrumentationScope::{
    name: "http-client",
    version: Some("1.2.3"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  assert_eq(full_scope.name, "http-client")
  assert_eq(full_scope.version, Some("1.2.3"))
  assert_eq(full_scope.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  // 测试作用域名称验证
  let scope_with_special_chars = InstrumentationScope::{
    name: "instrumentation.with.dots-and-hyphens",
    version: Some("v1.0-beta"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  assert_eq(scope_with_special_chars.name.contains("."), true)
  assert_eq(scope_with_special_chars.name.contains("-"), true)
  assert_eq(scope_with_special_chars.version, Some("v1.0-beta"))
}

test "attribute_value_string_representation" {
  // 测试属性值的字符串表示
  
  // 测试各种类型的字符串转换
  let string_attr = AttributeValue::string("hello")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  
  // 验证字符串表示包含预期内容
  match string_attr {
    StringValue(s) => {
      let string_repr = s.to_string()
      assert_eq(string_repr.contains("hello"), true)
    }
    _ => {}
  }
  
  match int_attr {
    IntValue(i) => {
      let int_repr = i.to_string()
      assert_eq(int_repr.contains("42"), true)
    }
    _ => {}
  }
  
  match float_attr {
    FloatValue(f) => {
      let float_repr = f.to_string()
      assert_eq(float_repr.contains("3.14"), true)
    }
    _ => {}
  }
  
  match bool_attr {
    BoolValue(b) => {
      let bool_repr = b.to_string()
      assert_eq(bool_repr.contains("true"), true)
    }
    _ => {}
  }
  
  // 测试数组的字符串表示
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0].to_string(), "a")
      assert_eq(arr[1].to_string(), "b")
      assert_eq(arr[2].to_string(), "c")
    }
    _ => {}
  }
}