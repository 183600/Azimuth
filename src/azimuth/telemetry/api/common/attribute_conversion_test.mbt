// Common模块属性值转换测试
// 测试AttributeValue的各种类型转换和Resource的默认创建

test "attribute_value_string_conversion" {
  let string_attr = AttributeValue::string("test_value")
  match string_attr {
    StringValue(value) => assert_eq(value, "test_value")
    _ => @test.fail("Expected StringValue")
  }
}

test "attribute_value_int_conversion" {
  let int_attr = AttributeValue::int(42L)
  match int_attr {
    IntValue(value) => assert_eq(value, 42L)
    _ => @test.fail("Expected IntValue")
  }
}

test "attribute_value_float_conversion" {
  let float_attr = AttributeValue::float(3.14)
  match float_attr {
    FloatValue(value) => assert_eq(value, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
}

test "attribute_value_bool_conversion" {
  let bool_attr = AttributeValue::bool(true)
  match bool_attr {
    BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Expected BoolValue")
  }
}

test "attribute_value_array_string_conversion" {
  let array_attr = AttributeValue::array_string(["a", "b", "c"])
  match array_attr {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[1], "b")
      assert_eq(values[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "attribute_value_array_int_conversion" {
  let array_attr = AttributeValue::array_int([1L, 2L, 3L])
  match array_attr {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1L)
      assert_eq(values[1], 2L)
      assert_eq(values[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
}

test "resource_default_creation" {
  let resource = Resource::default("test_service")
  assert_eq(resource.service_name, "test_service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
}

test "resource_with_attributes" {
  let resource = Resource::default("test_service")
  let attributes = [
    ("env", AttributeValue::string("production")),
    ("version", AttributeValue::string("1.0.0"))
  ]
  let resource_with_attrs = Resource::{ ..resource, attributes }
  assert_eq(resource_with_attrs.attributes.length(), 2)
  match resource_with_attrs.attributes[0] {
    (key, StringValue(value)) => {
      assert_eq(key, "env")
      assert_eq(value, "production")
    }
    _ => @test.fail("Expected string attribute")
  }
}