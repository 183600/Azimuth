// Resource Advanced Operations Test
// 测试资源的高级操作，包括合并、属性覆盖等场景

test "resource creation with complex attributes" {
  // 测试创建包含复杂属性的Resource
  let resource = Resource::new()
  
  // 添加各种类型的属性
  let complex_attributes = [
    ("service.name", StringValue("azimuth-telemetry")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("host.name", StringValue("production-server-01")),
    ("host.cpu.count", IntValue(8)),
    ("host.memory.total", IntValue(16777216)),
    ("process.pid", IntValue(1234)),
    ("process.executable.name", StringValue("azimuth")),
    ("telemetry.sdk.name", StringValue("azimuth-sdk")),
    ("telemetry.sdk.language", StringValue("moonbit")),
    ("telemetry.sdk.version", StringValue("0.1.0"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, complex_attributes)
  
  // 验证属性能够正确获取
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let cpu_count = Resource::get_attribute(resource_with_attrs, "host.cpu.count")
  let sdk_name = Resource::get_attribute(resource_with_attrs, "telemetry.sdk.name")
  
  assert_eq(service_name, Some(StringValue("azimuth-telemetry")))
  assert_eq(cpu_count, Some(IntValue(8)))
  assert_eq(sdk_name, Some(StringValue("azimuth-sdk")))
  
  // 验证不存在的属性返回None
  let non_existent = Resource::get_attribute(resource_with_attrs, "non.existent.key")
  assert_eq(non_existent, None)
}

test "resource merge and override behavior" {
  // 测试资源合并和覆盖行为
  let base_resource = Resource::new()
  let base_attributes = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development")),
    ("feature.flag", BoolValue(false))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attributes)
  
  let override_resource = Resource::new()
  let override_attributes = [
    ("service.version", StringValue("2.0.0")),  // 覆盖版本
    ("deployment.environment", StringValue("production")),  // 覆盖环境
    ("new.feature", StringValue("enabled")),  // 新增属性
    ("feature.flag", BoolValue(true))  // 覆盖布尔值
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attributes)
  
  // 执行合并操作
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // 验证合并结果（简化实现返回override资源）
  let merged_version = Resource::get_attribute(merged_resource, "service.version")
  let merged_env = Resource::get_attribute(merged_resource, "deployment.environment")
  let merged_new_feature = Resource::get_attribute(merged_resource, "new.feature")
  let merged_flag = Resource::get_attribute(merged_resource, "feature.flag")
  
  assert_eq(merged_version, Some(StringValue("2.0.0")))
  assert_eq(merged_env, Some(StringValue("production")))
  assert_eq(merged_new_feature, Some(StringValue("enabled")))
  assert_eq(merged_flag, Some(BoolValue(true)))
}

test "resource attribute array types handling" {
  // 测试资源属性中的数组类型处理
  let resource = Resource::new()
  
  // 添加数组类型的属性
  let array_attributes = [
    ("service.tags", ArrayStringValue(["web", "api", "microservice"])),
    ("deployment.regions", ArrayStringValue(["us-east-1", "us-west-2", "eu-west-1"])),
    ("supported.protocols", ArrayStringValue(["http", "https", "grpc"])),
    ("cpu.cores", ArrayIntValue([0, 1, 2, 3, 4, 5, 6, 7])),
    ("memory.allocated", ArrayIntValue([1024, 2048, 4096]))
  ]
  
  let resource_with_arrays = Resource::with_attributes(resource, array_attributes)
  
  // 验证数组类型属性
  let service_tags = Resource::get_attribute(resource_with_arrays, "service.tags")
  let cpu_cores = Resource::get_attribute(resource_with_arrays, "cpu.cores")
  
  match service_tags {
    Some(ArrayStringValue(tags)) => {
      assert_eq(tags.length, 3)
      assert_eq(tags[0], "web")
      assert_eq(tags[1], "api")
      assert_eq(tags[2], "microservice")
    }
    _ => @test.fail("Expected ArrayStringValue for service.tags")?
  }
  
  match cpu_cores {
    Some(ArrayIntValue(cores)) => {
      assert_eq(cores.length, 8)
      assert_eq(cores[0], 0)
      assert_eq(cores[7], 7)
    }
    _ => @test.fail("Expected ArrayIntValue for cpu.cores")?
  }
}

test "resource empty and null attribute handling" {
  // 测试空值和null属性的处理
  let resource = Resource::new()
  
  // 添加包含空字符串和特殊值的属性
  let special_attributes = [
    ("empty.string", StringValue("")),
    ("zero.number", IntValue(0)),
    ("false.boolean", BoolValue(false)),
    ("zero.float", FloatValue(0.0)),
    ("normal.attribute", StringValue("normal-value"))
  ]
  
  let resource_with_special = Resource::with_attributes(resource, special_attributes)
  
  // 验证特殊值属性
  let empty_string = Resource::get_attribute(resource_with_special, "empty.string")
  let zero_number = Resource::get_attribute(resource_with_special, "zero.number")
  let false_boolean = Resource::get_attribute(resource_with_special, "false.boolean")
  let normal_attribute = Resource::get_attribute(resource_with_special, "normal.attribute")
  
  assert_eq(empty_string, Some(StringValue("")))
  assert_eq(zero_number, Some(IntValue(0)))
  assert_eq(false_boolean, Some(BoolValue(false)))
  assert_eq(normal_attribute, Some(StringValue("normal-value")))
}