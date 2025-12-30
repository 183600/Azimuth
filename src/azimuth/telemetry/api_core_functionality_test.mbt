// API核心功能测试用例
// 测试Azimuth Telemetry API的核心功能

test "attribute_value_creation_and_validation" {
  // 测试属性值的创建和验证
  
  // 测试字符串属性值
  let string_attr = @azimuth.telemetry.api.common.AttributeValue::string("test_value")
  match string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s, "test_value")
    _ => assert_eq(false, true, "Expected StringValue")
  }
  
  // 测试整数属性值
  let int_attr = @azimuth.telemetry.api.common.AttributeValue::int(42L)
  match int_attr {
    @azimuth.telemetry.api.common.AttributeValue::IntValue(i) => assert_eq(i, 42L)
    _ => assert_eq(false, true, "Expected IntValue")
  }
  
  // 测试浮点属性值
  let float_attr = common::AttributeValue::float(3.14)
  match float_attr {
    common::FloatValue(f) => assert_eq(f > 3.0 && f < 3.2, true)
    _ => assert_eq(false, true, "Expected FloatValue")
  }
  
  // 测试布尔属性值
  let bool_attr = common::AttributeValue::bool(true)
  match bool_attr {
    common::BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true, "Expected BoolValue")
  }
  
  // 测试字符串数组属性值
  let string_array = ["value1", "value2", "value3"]
  let array_attr = common::AttributeValue::array_string(string_array)
  match array_attr {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "value1")
      assert_eq(arr[2], "value3")
    }
    _ => assert_eq(false, true, "Expected ArrayStringValue")
  }
  
  // 测试整数数组属性值
  let int_array = [1L, 2L, 3L, 4L, 5L]
  let int_array_attr = common::AttributeValue::array_int(int_array)
  match int_array_attr {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1L)
      assert_eq(arr[4], 5L)
    }
    _ => assert_eq(false, true, "Expected ArrayIntValue")
  }
}

test "resource_creation_and_default_values" {
  // 测试资源创建和默认值
  
  // 测试默认资源创建
  let resource = common::Resource::default("test-service")
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 测试带版本的资源创建
  let resource_with_version = common::Resource::{
    service_name: "payment-service",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2"))
    ]
  }
  
  assert_eq(resource_with_version.service_name, "payment-service")
  match resource_with_version.service_version {
    Some(version) => assert_eq(version, "1.2.3")
    None => assert_eq(false, true, "Expected version to be Some")
  }
  assert_eq(resource_with_version.attributes.length(), 2)
  
  // 验证属性
  let (env_key, env_value) = resource_with_version.attributes[0]
  assert_eq(env_key, "environment")
  match env_value {
    common::StringValue(s) => assert_eq(s, "production")
    _ => assert_eq(false, true, "Expected StringValue for environment")
  }
}

test "instrumentation_scope_validation" {
  // 测试仪表范围验证
  
  // 测试基本仪表范围
  let scope = common::InstrumentationScope::{
    name: "database-client",
    version: Some("2.1.0"),
    schema_url: Some("https://opentelemetry.io/schema/1.0.0")
  }
  
  assert_eq(scope.name, "database-client")
  match scope.version {
    Some(v) => assert_eq(v, "2.1.0")
    None => assert_eq(false, true, "Expected version to be Some")
  }
  match scope.schema_url {
    Some(url) => assert_eq(url.contains("opentelemetry.io"), true)
    None => assert_eq(false, true, "Expected schema_url to be Some")
  }
  
  // 测试最小仪表范围
  let minimal_scope = common::InstrumentationScope::{
    name: "http-client",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "http-client")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

test "attributes_array_operations" {
  // 测试属性数组操作
  
  // 创建属性数组
  let attributes = [
    ("http.method", common::AttributeValue::string("GET")),
    ("http.status_code", common::AttributeValue::int(200L)),
    ("http.duration_ms", common::AttributeValue::float(125.5)),
    ("http.success", common::AttributeValue::bool(true))
  ]
  
  assert_eq(attributes.length(), 4)
  
  // 验证字符串属性
  let (method_key, method_value) = attributes[0]
  assert_eq(method_key, "http.method")
  match method_value {
    common::StringValue(method) => assert_eq(method, "GET")
    _ => assert_eq(false, true, "Expected StringValue for method")
  }
  
  // 验证整数属性
  let (status_key, status_value) = attributes[1]
  assert_eq(status_key, "http.status_code")
  match status_value {
    common::IntValue(status) => assert_eq(status, 200L)
    _ => assert_eq(false, true, "Expected IntValue for status")
  }
  
  // 验证浮点属性
  let (duration_key, duration_value) = attributes[2]
  assert_eq(duration_key, "http.duration_ms")
  match duration_value {
    common::FloatValue(duration) => assert_eq(duration > 100.0, true)
    _ => assert_eq(false, true, "Expected FloatValue for duration")
  }
  
  // 验证布尔属性
  let (success_key, success_value) = attributes[3]
  assert_eq(success_key, "http.success")
  match success_value {
    common::BoolValue(success) => assert_eq(success, true)
    _ => assert_eq(false, true, "Expected BoolValue for success")
  }
}

test "complex_attribute_structures" {
  // 测试复杂属性结构
  
  // 创建嵌套属性结构
  let complex_attributes = [
    ("service.name", common::AttributeValue::string("order-processing")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("service.instance.id", common::AttributeValue::string("instance-12345")),
    ("service.tags", common::AttributeValue::array_string(["critical", "payment", "v1"])),
    ("service.retries", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("service.latencies", common::AttributeValue::array_float([10.5, 25.3, 15.7])),
    ("service.features", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  assert_eq(complex_attributes.length(), 7)
  
  // 验证数组属性
  let (_, tags_value) = complex_attributes[3]
  match tags_value {
    common::ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags.contains("critical"), true)
      assert_eq(tags.contains("payment"), true)
      assert_eq(tags.contains("v1"), true)
    }
    _ => assert_eq(false, true, "Expected ArrayStringValue for tags")
  }
  
  let (_, latencies_value) = complex_attributes[5]
  match latencies_value {
    common::ArrayFloatValue(latencies) => {
      assert_eq(latencies.length(), 3)
      let mut total = 0.0
      let mut i = 0
      while i < latencies.length() {
        total = total + latencies[i]
        i = i + 1
      }
      let average = total / latencies.length().to_double()
      assert_eq(average > 10.0 && average < 30.0, true)
    }
    _ => assert_eq(false, true, "Expected ArrayFloatValue for latencies")
  }
}

test "attribute_type_conversion_edge_cases" {
  // 测试属性类型转换边界情况
  
  // 测试空字符串
  let empty_string = common::AttributeValue::string("")
  match empty_string {
    common::StringValue(s) => assert_eq(s.length(), 0)
    _ => assert_eq(false, true, "Expected StringValue for empty string")
  }
  
  // 测试零值
  let zero_int = common::AttributeValue::int(0L)
  match zero_int {
    common::IntValue(i) => assert_eq(i, 0L)
    _ => assert_eq(false, true, "Expected IntValue for zero")
  }
  
  let zero_float = common::AttributeValue::float(0.0)
  match zero_float {
    common::FloatValue(f) => assert_eq(f, 0.0)
    _ => assert_eq(false, true, "Expected FloatValue for zero")
  }
  
  // 测试负值
  let negative_int = common::AttributeValue::int(-100L)
  match negative_int {
    common::IntValue(i) => assert_eq(i, -100L)
    _ => assert_eq(false, true, "Expected IntValue for negative number")
  }
  
  let negative_float = common::AttributeValue::float(-3.14)
  match negative_float {
    common::FloatValue(f) => assert_eq(f < 0.0, true)
    _ => assert_eq(false, true, "Expected FloatValue for negative number")
  }
  
  // 测试空数组
  let empty_string_array = common::AttributeValue::array_string([])
  match empty_string_array {
    common::ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_eq(false, true, "Expected ArrayStringValue for empty array")
  }
  
  // 测试单元素数组
  let single_element_array = common::AttributeValue::array_int([42L])
  match single_element_array {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42L)
    }
    _ => assert_eq(false, true, "Expected ArrayIntValue for single element array")
  }
}