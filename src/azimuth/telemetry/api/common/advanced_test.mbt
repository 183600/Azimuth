test "attribute_conversion_and_validation" {
  // 测试属性值的转换和验证
  
  // 测试字符串到其他类型的转换
  let string_attr = AttributeValue::string("123")
  let int_attr = AttributeValue::int(123L)
  let float_attr = AttributeValue::float(123.0)
  
  // 验证不同类型的属性值
  match string_attr {
    StringValue(s) => {
      assert_eq(s, "123")
      assert_eq(s.length(), 3)
    }
    _ => @test.fail("Expected StringValue")
  }
  
  match int_attr {
    IntValue(i) => {
      assert_eq(i, 123L)
      assert_eq(i > 0L, true)
    }
    _ => @test.fail("Expected IntValue")
  }
  
  match float_attr {
    FloatValue(f) => {
      assert_eq(f, 123.0)
      assert_eq(f > 0.0, true)
    }
    _ => @test.fail("Expected FloatValue")
  }
}

test "resource_attributes_complex_validation" {
  // 测试Resource属性的复杂验证
  
  // 测试复杂的服务名称
  let complex_names = [
    "simple-service",
    "service.with.dots",
    "service_with_underscores",
    "service-with-dashes",
    "service123",
    "123service",
    "service-with-numbers-123",
    "UPPERCASE-SERVICE",
    "Mixed-Case-Service",
    "service.with.special.chars!@#$%",
    "service.with.spaces ",
    " service.with.leading.spaces",
    "service.with.unicode.测试",
    "service.with.emoji🚀"
  ]
  
  // 验证各种服务名称
  for name in complex_names {
    let resource = Resource::default(name)
    assert_eq(resource.service_name, name)
    assert_eq(resource.telemetry_sdk_name, "azimuth")
    assert_eq(resource.telemetry_sdk_version, "0.1.0")
  }
  
  // 测试空字符串和单字符服务名称
  let empty_resource = Resource::default("")
  let single_char_resource = Resource::default("a")
  
  assert_eq(empty_resource.service_name, "")
  assert_eq(single_char_resource.service_name, "a")
  
  // 测试极长的服务名称
  let long_name = "a".repeat(100)
  let long_resource = Resource::default(long_name)
  assert_eq(long_resource.service_name.length(), 100)
}