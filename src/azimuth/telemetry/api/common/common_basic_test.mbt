// Common API基础功能测试
test "AttributeValue string creation" {
  let value = AttributeValue::string("test")
  assert match (value) {
    StringValue(s) => s == "test"
    _ => false
  }
}

test "AttributeValue int creation" {
  let value = AttributeValue::int(42L)
  assert match (value) {
    IntValue(i) => i == 42L
    _ => false
  }
}

test "AttributeValue float creation" {
  let value = AttributeValue::float(3.14)
  assert match (value) {
    FloatValue(f) => f == 3.14
    _ => false
  }
}

test "AttributeValue bool creation" {
  let value = AttributeValue::bool(true)
  assert match (value) {
    BoolValue(b) => b == true
    _ => false
  }
}

test "AttributeValue array operations" {
  let str_array = ["a", "b", "c"]
  let int_array = [1L, 2L, 3L]
  let float_array = [1.1, 2.2, 3.3]
  let bool_array = [true, false, true]
  
  let str_value = AttributeValue::array_string(str_array)
  let int_value = AttributeValue::array_int(int_array)
  let float_value = AttributeValue::array_float(float_array)
  let bool_value = AttributeValue::array_bool(bool_array)
  
  assert match (str_value) {
    ArrayStringValue(arr) => arr.length == 3
    _ => false
  }
  
  assert match (int_value) {
    ArrayIntValue(arr) => arr.length == 3
    _ => false
  }
  
  assert match (float_value) {
    ArrayFloatValue(arr) => arr.length == 3
    _ => false
  }
  
  assert match (bool_value) {
    ArrayBoolValue(arr) => arr.length == 3
    _ => false
  }
}

test "Resource default creation" {
  let resource = Resource::default("test-service")
  assert resource.service_name == "test-service"
  assert resource.service_version == None
  assert resource.telemetry_sdk_name == "azimuth"
  assert resource.telemetry_sdk_version == "0.1.0"
  assert resource.attributes.length == 0
}

test "Resource with version" {
  let resource = Resource::{
    service_name: "test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [("env", AttributeValue::string("prod"))]
  }
  assert resource.service_name == "test-service"
  assert match (resource.service_version) {
    Some(v) => v == "1.0.0"
    None => false
  }
  assert resource.attributes.length == 1
}

test "InstrumentationScope creation" {
  let scope = InstrumentationScope::{
    name: "test-instrument",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  assert scope.name == "test-instrument"
  assert match (scope.version) {
    Some(v) => v == "1.0.0"
    None => false
  }
  assert match (scope.schema_url) {
    Some(url) => url == "https://example.com/schema"
    None => false
  }
}

test "InstrumentationScope minimal creation" {
  let scope = InstrumentationScope::{
    name: "minimal-instrument",
    version: None,
    schema_url: None
  }
  assert scope.name == "minimal-instrument"
  assert scope.version == None
  assert scope.schema_url == None
}

test "Attributes operations" {
  let attributes : Attributes = [
    ("string_attr", AttributeValue::string("value")),
    ("int_attr", AttributeValue::int(123L)),
    ("float_attr", AttributeValue::float(45.6)),
    ("bool_attr", AttributeValue::bool(true))
  ]
  assert attributes.length == 4
  
  // 验证属性值
  assert match (attributes[0].1) {
    StringValue(s) => s == "value"
    _ => false
  }
  
  assert match (attributes[1].1) {
    IntValue(i) => i == 123L
    _ => false
  }
  
  assert match (attributes[2].1) {
    FloatValue(f) => f == 45.6
    _ => false
  }
  
  assert match (attributes[3].1) {
    BoolValue(b) => b == true
    _ => false
  }
}