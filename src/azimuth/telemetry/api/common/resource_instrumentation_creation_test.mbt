test "Resource and InstrumentationScope creation test" {
  // 测试Resource默认创建
  let resource = Resource::default("test-service")
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 测试带有属性的Resource创建
  let mut resource_with_attrs = Resource::default("service-with-attrs")
  resource_with_attrs.service_version = Some("1.0.0")
  resource_with_attrs.attributes = [
    ("env", AttributeValue::string("production")),
    ("region", AttributeValue::string("us-west")),
    ("instance_id", AttributeValue::int(12345L))
  ]
  
  assert_eq(resource_with_attrs.service_name, "service-with-attrs")
  match resource_with_attrs.service_version {
    Some(v) => assert_eq(v, "1.0.0")
    None => @test.fail("Expected service version to be Some")
  }
  assert_eq(resource_with_attrs.attributes.length(), 3)
  
  // 验证属性值
  let env_attr = resource_with_attrs.attributes[0]
  assert_eq(env_attr.0, "env")
  match env_attr.1 {
    StringValue(s) => assert_eq(s, "production")
    _ => @test.fail("Expected StringValue for env attribute")
  }
  
  let region_attr = resource_with_attrs.attributes[1]
  assert_eq(region_attr.0, "region")
  match region_attr.1 {
    StringValue(s) => assert_eq(s, "us-west")
    _ => @test.fail("Expected StringValue for region attribute")
  }
  
  let instance_attr = resource_with_attrs.attributes[2]
  assert_eq(instance_attr.0, "instance_id")
  match instance_attr.1 {
    IntValue(i) => assert_eq(i, 12345L)
    _ => @test.fail("Expected IntValue for instance_id attribute")
  }
  
  // 测试InstrumentationScope创建
  let scope_basic = InstrumentationScope{
    name: "test-scope",
    version: None,
    schema_url: None
  }
  assert_eq(scope_basic.name, "test-scope")
  assert_eq(scope_basic.version, None)
  assert_eq(scope_basic.schema_url, None)
  
  // 测试完整的InstrumentationScope
  let scope_full = InstrumentationScope{
    name: "full-scope",
    version: Some("2.1.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  assert_eq(scope_full.name, "full-scope")
  match scope_full.version {
    Some(v) => assert_eq(v, "2.1.0")
    None => @test.fail("Expected version to be Some")
  }
  match scope_full.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema/v1")
    None => @test.fail("Expected schema_url to be Some")
  }
  
  // 测试边界情况
  let empty_service_name = Resource::default("")
  assert_eq(empty_service_name.service_name, "")
  
  let long_service_name = Resource::default("this-is-a-very-long-service-name-that-might-be-used-in-testing-boundary-conditions")
  assert_eq(long_service_name.service_name.length(), 78)
  
  let scope_empty_name = InstrumentationScope{
    name: "",
    version: None,
    schema_url: None
  }
  assert_eq(scope_empty_name.name, "")
  
  let scope_long_name = InstrumentationScope{
    name: "this-is-a-very-long-instrumentation-scope-name-that-tests-boundary-conditions",
    version: Some("version-with-special-chars-123!@#"),
    schema_url: Some("https://very-long-url-that-tests-boundary-conditions.example.com/very/long/path/to/schema/v1.0.0")
  }
  assert_eq(scope_long_name.name.length(), 81)
  match scope_long_name.version {
    Some(v) => assert_eq(v, "version-with-special-chars-123!@#")
    None => @test.fail("Expected version to be Some")
  }
  match scope_long_name.schema_url {
    Some(url) => assert_eq(url, "https://very-long-url-that-tests-boundary-conditions.example.com/very/long/path/to/schema/v1.0.0")
    None => @test.fail("Expected schema_url to be Some")
  }
  
  // 测试Resource复杂属性
  let mut complex_resource = Resource::default("complex-service")
  complex_resource.attributes = [
    ("string_attr", AttributeValue::string("test")),
    ("int_attr", AttributeValue::int(42L)),
    ("float_attr", AttributeValue::float(3.14)),
    ("bool_attr", AttributeValue::bool(true)),
    ("string_array_attr", AttributeValue::array_string(["a", "b", "c"])),
    ("int_array_attr", AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array_attr", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array_attr", AttributeValue::array_bool([true, false, true]))
  ]
  
  assert_eq(complex_resource.attributes.length(), 8)
  
  // 验证每种类型的属性
  for i = 0; i < complex_resource.attributes.length(); i = i + 1 {
    let attr = complex_resource.attributes[i]
    match attr.0 {
      "string_attr" => {
        match attr.1 {
              StringValue(s) => assert_eq(s, "test")
              _ => @test.fail("Expected StringValue for string_attr")
            }
          }
          
          "int_attr" => {
            match attr.1 {
              IntValue(n) => assert_eq(n, 42L)
              _ => @test.fail("Expected IntValue for int_attr")
            }
          }
          
          "float_attr" => {
            match attr.1 {
              FloatValue(f) => assert_eq(f, 3.14)
              _ => @test.fail("Expected FloatValue for float_attr")
            }
          }
          
          "bool_attr" => {
            match attr.1 {
              BoolValue(b) => assert_eq(b, true)
              _ => @test.fail("Expected BoolValue for bool_attr")
            }
          }
          
          "string_array_attr" => {
            match attr.1 {
              ArrayStringValue(arr) => {
                assert_eq(arr.length(), 3)
                assert_eq(arr[0], "a")
                assert_eq(arr[1], "b")
                assert_eq(arr[2], "c")
              }
              _ => @test.fail("Expected ArrayStringValue for string_array_attr")
            }
          }
          
          "int_array_attr" => {
            match attr.1 {
              ArrayIntValue(arr) => {
                assert_eq(arr.length(), 3)
                assert_eq(arr[0], 1L)
                assert_eq(arr[1], 2L)
                assert_eq(arr[2], 3L)
              }
              _ => @test.fail("Expected ArrayIntValue for int_array_attr")
            }
          }
          
          "float_array_attr" => {
            match attr.1 {
              ArrayFloatValue(arr) => {
                assert_eq(arr.length(), 3)
                assert_eq(arr[0], 1.1)
                assert_eq(arr[1], 2.2)
                assert_eq(arr[2], 3.3)
              }
              _ => @test.fail("Expected ArrayFloatValue for float_array_attr")
            }
          }
          
          "bool_array_attr" => {
            match attr.1 {
              ArrayBoolValue(arr) => {
                assert_eq(arr.length(), 3)
                assert_eq(arr[0], true)
                assert_eq(arr[1], false)
                assert_eq(arr[2], true)
              }
              _ => @test.fail("Expected ArrayBoolValue for bool_array_attr")
            }
          }
      _ => @test.fail("Unexpected attribute name")
    }
  }
}