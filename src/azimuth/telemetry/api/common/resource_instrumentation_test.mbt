// Resource和InstrumentationScope详细测试用例
// 测试资源创建、属性管理和复杂场景

test "resource_default_creation_variants" {
  // 测试不同类型的默认Resource创建
  let simple_resource = Resource::default("simple-service")
  assert_eq(simple_resource.service_name, "simple-service")
  assert_eq(simple_resource.service_version, None)
  assert_eq(simple_resource.telemetry_sdk_name, "azimuth")
  assert_eq(simple_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(simple_resource.attributes.length(), 0)
  
  // 测试带有版本的服务名称
  let versioned_resource = Resource::default("service:v1.2.3")
  assert_eq(versioned_resource.service_name, "service:v1.2.3")
  assert_eq(versioned_resource.telemetry_sdk_name, "azimuth")
  assert_eq(versioned_resource.telemetry_sdk_version, "0.1.0")
  
  // 测试长服务名称
  let long_name_resource = Resource::default("this-is-a-very-long-service-name-with-many-components-and-subdomains")
  assert_eq(long_name_resource.service_name, "this-is-a-very-long-service-name-with-many-components-and-subdomains")
  assert_eq(long_name_resource.telemetry_sdk_name, "azimuth")
}

test "resource_with_comprehensive_attributes" {
  // 测试Resource与各种类型的属性组合
  let resource = Resource::default("comprehensive-test-service")
  
  // 字符串属性
  let string_attrs : Attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-123")),
    ("deployment.environment", AttributeValue::string("staging")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("k8s.pod.name", AttributeValue::string("my-pod-7d4b8c9f-abc")),
    ("k8s.namespace", AttributeValue::string("default"))
  ]
  
  // 数值属性
  let numeric_attrs : Attributes = [
    ("process.pid", AttributeValue::int(12345L)),
    ("process.start.time", AttributeValue::int(1640995200L)),
    ("cpu.usage", AttributeValue::float(75.5)),
    ("memory.usage", AttributeValue::float(1024.5)),
    ("network.bytes.in", AttributeValue::int(1048576L)),
    ("network.bytes.out", AttributeValue::int(2097152L))
  ]
  
  // 布尔属性
  let bool_attrs : Attributes = [
    ("service.healthy", AttributeValue::bool(true)),
    ("maintenance.mode", AttributeValue::bool(false)),
    ("debug.enabled", AttributeValue::bool(true)),
    ("ssl.enabled", AttributeValue::bool(true))
  ]
  
  // 数组属性
  let array_attrs : Attributes = [
    ("service.tags", AttributeValue::array_string(["web", "api", "critical", "production"])),
    ("service.ports", AttributeValue::array_int([8080L, 8443L, 9090L, 3000L])),
    ("cpu.thresholds", AttributeValue::array_float([60.0, 80.0, 95.0, 99.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true, false]))
  ]
  
  // 验证属性数量
  assert_eq(string_attrs.length(), 6)
  assert_eq(numeric_attrs.length(), 6)
  assert_eq(bool_attrs.length(), 4)
  assert_eq(array_attrs.length(), 4)
  
  // 验证具体属性值
  match string_attrs[0].1 {
    StringValue(s) => assert_eq(s, "production")
    _ => @test.fail("Expected StringValue")
  }
  
  match numeric_attrs[2].1 {
    FloatValue(f) => assert_eq(f, 75.5)
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_attrs[1].1 {
    BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected BoolValue")
  }
  
  match array_attrs[0].1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], "web")
      assert_eq(arr[3], "production")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "resource_edge_cases_and_special_values" {
  // 测试Resource的边界情况和特殊值
  
  // 空字符串服务名称
  let empty_name_resource = Resource::default("")
  assert_eq(empty_name_resource.service_name, "")
  assert_eq(empty_name_resource.telemetry_sdk_name, "azimuth")
  
  // 包含特殊字符的服务名称
  let special_chars_resource = Resource::default("service!@#$%^&*()_+-=[]{}|;':\",./<>?")
  assert_eq(special_chars_resource.service_name, "service!@#$%^&*()_+-=[]{}|;':\",./<>?")
  
  // Unicode和emoji服务名称
  let unicode_resource = Resource::default("测试服务🚀🌟💫")
  assert_eq(unicode_resource.service_name, "测试服务🚀🌟💫")
  
  // 仅包含空格的服务名称
  let spaces_resource = Resource::default("   ")
  assert_eq(spaces_resource.service_name, "   ")
  
  // 数字开头的服务名称
  let numeric_start_resource = Resource::default("123service")
  assert_eq(numeric_start_resource.service_name, "123service")
  
  // 非常长的服务名称
  let long_name = "a".repeat(1000)
  let very_long_resource = Resource::default(long_name)
  assert_eq(very_long_resource.service_name.length(), 1000)
}

test "resource_attributes_edge_cases" {
  // 测试属性的边界情况
  
  // 空属性数组
  let empty_attrs : Attributes = []
  assert_eq(empty_attrs.length(), 0)
  
  // 单个属性
  let single_attr : Attributes = [
    ("single.key", AttributeValue::string("single.value"))
  ]
  assert_eq(single_attr.length(), 1)
  
  // 包含特殊字符的属性键
  let special_key_attrs : Attributes = [
    ("key.with.dots", AttributeValue::string("value1")),
    ("key_with_underscores", AttributeValue::string("value2")),
    ("key-with-dashes", AttributeValue::string("value3")),
    ("key.with.all separators!@#$%", AttributeValue::string("value4"))
  ]
  assert_eq(special_key_attrs.length(), 4)
  
  // 空字符串属性键和值
  let empty_string_attrs : Attributes = [
    ("", AttributeValue::string("empty.key")),
    ("empty.value", AttributeValue::string("")),
    ("", AttributeValue::string(""))
  ]
  assert_eq(empty_string_attrs.length(), 3)
  
  // 极值数值属性
  let extreme_value_attrs : Attributes = [
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("zero.int", AttributeValue::int(0L)),
    ("max.float", AttributeValue::float(1.7976931348623157e+308)),
    ("min.float", AttributeValue::float(-1.7976931348623157e+308)),
    ("zero.float", AttributeValue::float(0.0)),
    ("inf.float", AttributeValue::float(1.0/0.0)),
    ("neg.inf.float", AttributeValue::float(-1.0/0.0)),
    ("nan.float", AttributeValue::float(0.0/0.0))
  ]
  assert_eq(extreme_value_attrs.length(), 9)
  
  // 空数组属性
  let empty_array_attrs : Attributes = [
    ("empty.string.array", AttributeValue::array_string([])),
    ("empty.int.array", AttributeValue::array_int([])),
    ("empty.float.array", AttributeValue::array_float([])),
    ("empty.bool.array", AttributeValue::array_bool([]))
  ]
  assert_eq(empty_array_attrs.length(), 4)
  
  // 单元素数组属性
  let single_element_array_attrs : Attributes = [
    ("single.string.array", AttributeValue::array_string(["single"])),
    ("single.int.array", AttributeValue::array_int([1L])),
    ("single.float.array", AttributeValue::array_float([1.0])),
    ("single.bool.array", AttributeValue::array_bool([true]))
  ]
  assert_eq(single_element_array_attrs.length(), 4)
}

test "instrumentation_scope_comprehensive_scenarios" {
  // 测试InstrumentationScope的复杂场景
  // 由于InstrumentationScope是只读类型，我们通过Resource测试相关功能
  
  // 模拟不同类型的InstrumentationScope使用场景
  let web_server_resource = Resource::default("web-server")
  let web_attrs : Attributes = [
    ("instrumentation.name", AttributeValue::string("web-server")),
    ("instrumentation.version", AttributeValue::string("1.2.3")),
    ("instrumentation.schema", AttributeValue::string("https://opentelemetry.io/schemas/1.20.0")),
    ("server.type", AttributeValue::string("http")),
    ("server.port", AttributeValue::int(8080L)),
    ("server.ssl", AttributeValue::bool(true))
  ]
  
  let database_resource = Resource::default("database-client")
  let db_attrs : Attributes = [
    ("instrumentation.name", AttributeValue::string("database-client")),
    ("instrumentation.version", AttributeValue::string("2.1.0")),
    ("db.system", AttributeValue::string("postgresql")),
    ("db.port", AttributeValue::int(5432L)),
    ("db.ssl", AttributeValue::bool(true)),
    ("db.pool.size", AttributeValue::int(10L))
  ]
  
  let messaging_resource = Resource::default("messaging-client")
  let messaging_attrs : Attributes = [
    ("instrumentation.name", AttributeValue::string("messaging-client")),
    ("instrumentation.version", AttributeValue::string("3.0.1")),
    ("messaging.system", AttributeValue::string("kafka")),
    ("messaging.broker", AttributeValue::string("broker-1:9092")),
    ("messaging.topic", AttributeValue::string("user-events"))
  ]
  
  // 验证不同场景的属性
  assert_eq(web_server_resource.service_name, "web-server")
  assert_eq(web_attrs.length(), 6)
  
  assert_eq(database_resource.service_name, "database-client")
  assert_eq(db_attrs.length(), 6)
  
  assert_eq(messaging_resource.service_name, "messaging-client")
  assert_eq(messaging_attrs.length(), 4)
  
  // 验证具体属性值
  match web_attrs[0].1 {
    StringValue(s) => assert_eq(s, "web-server")
    _ => @test.fail("Expected StringValue")
  }
  
  match db_attrs[2].1 {
    StringValue(s) => assert_eq(s, "postgresql")
    _ => @test.fail("Expected StringValue")
  }
  
  match messaging_attrs[2].1 {
    StringValue(s) => assert_eq(s, "kafka")
    _ => @test.fail("Expected StringValue")
  }
}

test "resource_and_attributes_performance_considerations" {
  // 测试大量属性的性能场景
  
  // 创建大量属性
  let mut large_attrs : Attributes = []
  let mut i = 0
  while i < 100 {
    let key = "attr." + i.to_string()
    let value = AttributeValue::string("value." + i.to_string())
    large_attrs.push((key, value))
    i = i + 1
  }
  
  assert_eq(large_attrs.length(), 100)
  
  // 验证特定索引的属性
  match large_attrs[0].1 {
    StringValue(s) => assert_eq(s, "value.0")
    _ => @test.fail("Expected StringValue")
  }
  
  match large_attrs[99].1 {
    StringValue(s) => assert_eq(s, "value.99")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试混合类型的大量属性
  let mut mixed_attrs : Attributes = []
  let mut j = 0
  while j < 50 {
    let string_key = "string.attr." + j.to_string()
    let int_key = "int.attr." + j.to_string()
    let float_key = "float.attr." + j.to_string()
    let bool_key = "bool.attr." + j.to_string()
    
    mixed_attrs.push((string_key, AttributeValue::string("value." + j.to_string())))
    mixed_attrs.push((int_key, AttributeValue::int(j.to_int64())))
    mixed_attrs.push((float_key, AttributeValue::float(j.to_double() * 1.1)))
    mixed_attrs.push((bool_key, AttributeValue::bool(j % 2 == 0)))
    
    j = j + 1
  }
  
  assert_eq(mixed_attrs.length(), 200)
  
  // 验证混合类型的属性
  match mixed_attrs[0].1 {
    StringValue(s) => assert_eq(s, "value.0")
    _ => @test.fail("Expected StringValue")
  }
  
  match mixed_attrs[1].1 {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Expected IntValue")
  }
  
  match mixed_attrs[2].1 {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  match mixed_attrs[3].1 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
}