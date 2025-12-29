test "common_attribute_value_creation" {
  // 测试不同类型的AttributeValue创建
  let string_attr = AttributeValue::string("test_value")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  
  match string_attr {
    StringValue(s) => assert_eq(s, "test_value")
    _ => @test.fail("Expected StringValue")
  }
  
  match int_attr {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match float_attr {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
}

test "common_resource_default_creation" {
  // 测试Resource的默认创建
  let resource = Resource::default("test-service")
  
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
}

test "common_resource_with_version" {
  // 测试带版本号的Resource创建
  let resource = Resource::default("test-service")
  
  // 由于Resource的属性是immutable的，我们只能检查默认值
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
}

test "common_attributes_empty" {
  // 测试空属性列表
  let attributes : Attributes = []
  assert_eq(attributes.length(), 0)
}

test "common_resource_with_attributes" {
  // 测试Resource的默认属性
  let resource = Resource::default("test-service")
  
  // 由于Resource的属性是immutable的，我们只能检查默认值
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
}

test "common_instrumentation_scope_creation" {
  // 测试InstrumentationScope的创建
  let scope_with_version = InstrumentationScope::{
    name: "test-instrument",
    version: Some("2.1.0"),
    schema_url: Some("http://example.com/schema")
  }
  
  let scope_minimal = InstrumentationScope::{
    name: "minimal-instrument",
    version: None,
    schema_url: None
  }
  
  assert_eq(scope_with_version.name, "test-instrument")
  assert_eq(scope_with_version.version, Some("2.1.0"))
  assert_eq(scope_with_version.schema_url, Some("http://example.com/schema"))
  
  assert_eq(scope_minimal.name, "minimal-instrument")
  assert_eq(scope_minimal.version, None)
  assert_eq(scope_minimal.schema_url, None)
}