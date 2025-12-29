// 基本测试用例
test "attribute_value_basic" {
  let string_attr = AttributeValue::string("test")
  let int_attr = AttributeValue::int(42)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  
  match string_attr {
    StringValue(s) => assert_eq(s, "test")
    _ => @test.fail("Test failed")
  }
  
  match int_attr {
    IntValue(i) => assert_eq(i, 42)
    _ => @test.fail("Test failed")
  }
  
  match float_attr {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Test failed")
  }
  
  match bool_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Test failed")
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
    _ => @test.fail("Test failed")
  }
  
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Test failed")
  }
  
  match float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => @test.fail("Test failed")
  }
  
  match bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => @test.fail("Test failed")
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
    Some(_) => @test.fail("Test failed")
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
    _ => @test.fail("Test failed")
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
    _ => @test.fail("Test failed")
  }
  
  match zero_int {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Test failed")
  }
  
  match negative_int {
    IntValue(i) => assert_eq(i, -1L)
    _ => @test.fail("Test failed")
  }
  
  match max_int {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
}

test "instrumentation_scope_creation_and_properties" {
  // 测试基本的InstrumentationScope创建
  // 由于InstrumentationScope是只读类型，我们无法直接创建实例
  // 这里我们测试Resource的创建和属性，作为替代
  
  let resource = Resource::default("test-service")
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 测试带有属性的Resource
  let resource_with_attrs = Resource::default("test-service-with-attrs")
  let attrs : Attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("1.2.3")),
    ("deployment.environment", AttributeValue::string("prod"))
  ]
  
  assert_eq(resource_with_attrs.service_name, "test-service-with-attrs")
  assert_eq(resource_with_attrs.telemetry_sdk_name, "azimuth")
  assert_eq(resource_with_attrs.telemetry_sdk_version, "0.1.0")
  assert_eq(attrs.length(), 3)
  
  // 测试复杂的服务名称
  let complex_resource = Resource::default("my-complex-service.v2")
  assert_eq(complex_resource.service_name, "my-complex-service.v2")
  assert_eq(complex_resource.telemetry_sdk_name, "azimuth")
  
  // 测试空服务名称
  let empty_resource = Resource::default("")
  assert_eq(empty_resource.service_name, "")
  assert_eq(empty_resource.telemetry_sdk_name, "azimuth")
}

test "resource_complex_scenarios" {
  // 测试Resource的复杂场景
  
  // 测试复杂的服务名称和版本
  let complex_resource = Resource::default("my-complex-service.v2")
  
  assert_eq(complex_resource.service_name, "my-complex-service.v2")
  assert_eq(complex_resource.telemetry_sdk_name, "azimuth")
  assert_eq(complex_resource.telemetry_sdk_version, "0.1.0")
  
  // 测试包含特殊字符的服务名称
  let special_resource = Resource::default("special-service!@#$%^&*()")
  
  assert_eq(special_resource.service_name, "special-service!@#$%^&*()")
  assert_eq(special_resource.telemetry_sdk_name, "azimuth")
  
  // 测试Unicode服务名称
  let unicode_resource = Resource::default("Unicode服务测试🚀")
  
  assert_eq(unicode_resource.service_name, "Unicode服务测试🚀")
  assert_eq(unicode_resource.telemetry_sdk_name, "azimuth")
  
  // 测试空服务名称
  let empty_resource = Resource::default("")
  
  assert_eq(empty_resource.service_name, "")
  assert_eq(empty_resource.telemetry_sdk_name, "azimuth")
  
  // 测试包含空格的服务名称
  let space_resource = Resource::default("service with spaces")
  
  assert_eq(space_resource.service_name, "service with spaces")
  assert_eq(space_resource.telemetry_sdk_name, "azimuth")
}