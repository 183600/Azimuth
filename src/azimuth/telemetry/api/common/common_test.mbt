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
  let mut resource = Resource::default("test-service")
  resource.service_version = Some("1.0.0")
  
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, Some("1.0.0"))
}

test "common_attributes_empty" {
  // 测试空属性列表
  let attributes : Attributes = []
  assert_eq(attributes.length(), 0)
}