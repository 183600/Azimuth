// 基本测试用例
test "attribute_value_basic" {
  let string_attr = AttributeValue::string("test")
  let int_attr = AttributeValue::int(42)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  
  match string_attr {
    StringValue(s) => assert_eq(s, "test")
    _ => @test.fail("Expected StringValue")
  }
  
  match int_attr {
    IntValue(i) => assert_eq(i, 42)
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

test "attribute_value_array" {
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  let int_array = AttributeValue::array_int([1L, 2L, 3L])
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = AttributeValue::array_bool([true, false, true])
  
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "resource_creation" {
  let resource = Resource::default("test-service")
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
}

test "attributes_usage" {
  let attrs : Attributes = [
    ("string.key", AttributeValue::string("value")),
    ("int.key", AttributeValue::int(42L)),
    ("float.key", AttributeValue::float(3.14)),
    ("bool.key", AttributeValue::bool(true))
  ]
  assert_eq(attrs.length(), 4)
}

test "resource_with_complex_attributes" {
  let resource = Resource::default("complex-service")
  let complex_attrs : Attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-123")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["web", "api", "critical"])),
    ("ports", AttributeValue::array_int([8080L, 8443L, 9090L])),
    ("cpu.thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 使用默认资源工厂函数
  let resource_with_attrs = Resource::default("complex-service")
  
  assert_eq(resource_with_attrs.service_name, "complex-service")
  match resource_with_attrs.service_version {
    Some(_) => @test.fail("Expected None for default resource")
    None => assert_eq(true, true)
  }
  assert_eq(resource_with_attrs.telemetry_sdk_name, "azimuth")
  assert_eq(resource_with_attrs.telemetry_sdk_version, "0.1.0")
  
  // 测试复杂属性数组
  assert_eq(complex_attrs.length(), 12)
  match complex_attrs[8].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "web")
      assert_eq(tags[1], "api")
      assert_eq(tags[2], "critical")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "attribute_value_edge_cases" {
  // 测试边界情况和特殊值
  let empty_string = AttributeValue::string("")
  let zero_int = AttributeValue::int(0L)
  let negative_int = AttributeValue::int(-1L)
  let max_int = AttributeValue::int(9223372036854775807L)
  let zero_float = AttributeValue::float(0.0)
  let negative_float = AttributeValue::float(-1.0)
  let infinity_float = AttributeValue::float(1.0/0.0)
  let nan_float = AttributeValue::float(0.0/0.0)
  
  // 测试空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  match empty_string {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected StringValue")
  }
  
  match zero_int {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Expected IntValue")
  }
  
  match negative_int {
    IntValue(i) => assert_eq(i, -1L)
    _ => @test.fail("Expected IntValue")
  }
  
  match max_int {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected IntValue")
  }
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayBoolValue")
  }
}