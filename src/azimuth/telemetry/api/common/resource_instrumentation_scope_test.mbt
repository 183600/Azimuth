// Resource和InstrumentationScope的测试用例
// 测试资源管理和仪器范围的基本功能

test "resource_default_creation" {
  let resource = Resource::default("test-service")
  
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
}

test "resource_with_attributes" {
  let attributes = [
    ("service.instance.id", AttributeValue::string("instance-123")),
    ("deployment.environment", AttributeValue::string("production")),
    ("host.name", AttributeValue::string("web-server-01"))
  ]
  
  let resource = Resource::{
    service_name: "api-service",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes
  }
  
  assert_eq(resource.service_name, "api-service")
  match resource.service_version {
    Some(version) => assert_eq(version, "1.2.3")
    None => @test.fail("Test failed")
  }
  assert_eq(resource.attributes.length(), 3)
  
  // 验证第一个属性
  match resource.attributes[0].1 {
    AttributeValue::StringValue(instance_id) => assert_eq(instance_id, "instance-123")
    _ => @test.fail("Test failed")
  }
}

test "instrumentation_scope_basic" {
  let scope = InstrumentationScope::{
    name: "test-instrumentation",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  assert_eq(scope.name, "test-instrumentation")
  match scope.version {
    Some(version) => assert_eq(version, "2.0.0")
    None => @test.fail("Test failed")
  }
  match scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://example.com/schema/v1")
    None => @test.fail("Test failed")
  }
}

test "instrumentation_scope_minimal" {
  let scope = InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  
  assert_eq(scope.name, "minimal-scope")
  assert_eq(scope.version, None)
  assert_eq(scope.schema_url, None)
}

test "resource_with_various_attribute_types" {
  let attributes = [
    ("string.attr", AttributeValue::string("value")),
    ("int.attr", AttributeValue::int(42L)),
    ("float.attr", AttributeValue::float(3.14)),
    ("bool.attr", AttributeValue::bool(true)),
    ("string.array.attr", AttributeValue::array_string(["a", "b", "c"])),
    ("int.array.attr", AttributeValue::array_int([1L, 2L, 3L]))
  ]
  
  let resource = Resource::{
    service_name: "multi-type-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes
  }
  
  assert_eq(resource.attributes.length(), 6)
  
  // 验证布尔属性
  match resource.attributes[3].1 {
    AttributeValue::BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Test failed")
  }
  
  // 验证字符串数组属性
  match resource.attributes[4].1 {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("Test failed")
  }
}