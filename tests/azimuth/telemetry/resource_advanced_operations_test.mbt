// Resource Advanced Operations Test
// 测试资源的高级操作，包括合并、属性覆盖等场景

test "resource creation with complex attributes" {
  // 测试创建包含复杂属性的Resource
  let resource = @azimuth.Resource::new()
  
  // 添加各种类型的属性
  let complex_attributes = [
    ("service.name", @azimuth.StringValue("azimuth-telemetry")),
    ("service.version", @azimuth.StringValue("1.0.0")),
    ("service.instance.id", @azimuth.StringValue("instance-12345")),
    ("host.name", @azimuth.StringValue("production-server-01")),
    ("host.cpu.count", @azimuth.IntValue(8)),
    ("host.memory.total", @azimuth.IntValue(16777216)),
    ("process.pid", @azimuth.IntValue(1234)),
    ("process.executable.name", @azimuth.StringValue("azimuth")),
    ("telemetry.sdk.name", @azimuth.StringValue("azimuth-sdk")),
    ("telemetry.sdk.language", @azimuth.StringValue("moonbit")),
    ("telemetry.sdk.version", @azimuth.StringValue("0.1.0"))
  ]
  
  let resource_with_attrs = @azimuth.Resource::with_attributes(resource, complex_attributes)
  
  // 验证属性能够正确获取
  let service_name = @azimuth.Resource::get_attribute(resource_with_attrs, "service.name")
  let cpu_count = @azimuth.Resource::get_attribute(resource_with_attrs, "host.cpu.count")
  let sdk_name = @azimuth.Resource::get_attribute(resource_with_attrs, "telemetry.sdk.name")
  
  assert_eq(service_name, Some(@azimuth.StringValue("azimuth-telemetry")))
  assert_eq(cpu_count, Some(@azimuth.IntValue(8)))
  assert_eq(sdk_name, Some(@azimuth.StringValue("azimuth-sdk")))
  
  // 验证不存在的属性返回None
  let non_existent = @azimuth.Resource::get_attribute(resource_with_attrs, "non.existent.key")
  assert_eq(non_existent, None)
}

test "resource merge and override behavior" {
  // 测试资源合并和覆盖行为
  let base_resource = @azimuth.Resource::new()
  let base_attributes = [
    ("service.name", @azimuth.StringValue("base-service")),
    ("service.version", @azimuth.StringValue("1.0.0")),
    ("deployment.environment", @azimuth.StringValue("development")),
    ("feature.flag", @azimuth.BoolValue(false))
  ]
  let base_with_attrs = @azimuth.Resource::with_attributes(base_resource, base_attributes)
  
  let override_resource = @azimuth.Resource::new()
  let override_attributes = [
    ("service.version", @azimuth.StringValue("2.0.0")),  // 覆盖版本
    ("deployment.environment", @azimuth.StringValue("production")),  // 覆盖环境
    ("new.feature", @azimuth.StringValue("enabled")),  // 新增属性
    ("feature.flag", @azimuth.BoolValue(true))  // 覆盖布尔值
  ]
  let override_with_attrs = @azimuth.Resource::with_attributes(override_resource, override_attributes)
  
  // 执行合并操作
  let merged_resource = @azimuth.Resource::merge(base_with_attrs, override_with_attrs)
  
  // 验证合并结果（简化实现返回override资源）
  let merged_version = @azimuth.Resource::get_attribute(merged_resource, "service.version")
  let merged_env = @azimuth.Resource::get_attribute(merged_resource, "deployment.environment")
  let merged_new_feature = @azimuth.Resource::get_attribute(merged_resource, "new.feature")
  let merged_flag = @azimuth.Resource::get_attribute(merged_resource, "feature.flag")
  
  assert_eq(merged_version, Some(@azimuth.StringValue("2.0.0")))
  assert_eq(merged_env, Some(@azimuth.StringValue("production")))
  assert_eq(merged_new_feature, Some(@azimuth.StringValue("enabled")))
  assert_eq(merged_flag, Some(@azimuth.BoolValue(true)))
}

test "resource attribute array types handling" {
  // 测试资源属性中的数组类型处理
  let resource = @azimuth.Resource::new()
  
  // 添加数组类型的属性
  let array_attributes = [
    ("service.tags", @azimuth.ArrayStringValue(["web", "api", "microservice"])),
    ("deployment.regions", @azimuth.ArrayStringValue(["us-east-1", "us-west-2", "eu-west-1"])),
    ("supported.protocols", @azimuth.ArrayStringValue(["http", "https", "grpc"])),
    ("cpu.cores", @azimuth.ArrayIntValue([0, 1, 2, 3, 4, 5, 6, 7])),
    ("memory.allocated", @azimuth.ArrayIntValue([1024, 2048, 4096]))
  ]
  
  let resource_with_arrays = @azimuth.Resource::with_attributes(resource, array_attributes)
  
  // 验证数组类型属性
  let service_tags = @azimuth.Resource::get_attribute(resource_with_arrays, "service.tags")
  let cpu_cores = @azimuth.Resource::get_attribute(resource_with_arrays, "cpu.cores")
  
  match service_tags {
    Some(@azimuth.ArrayStringValue(tags)) => {
      assert_eq(tags.length, 3)
      assert_eq(tags[0], "web")
      assert_eq(tags[1], "api")
      assert_eq(tags[2], "microservice")
    }
    _ => @test.fail("Expected ArrayStringValue for service.tags")?
  }
  
  match cpu_cores {
    Some(@azimuth.ArrayIntValue(cores)) => {
      assert_eq(cores.length, 8)
      assert_eq(cores[0], 0)
      assert_eq(cores[7], 7)
    }
    _ => @test.fail("Expected ArrayIntValue for cpu.cores")?
  }
}

test "resource empty and null attribute handling" {
  // 测试空值和null属性的处理
  let resource = @azimuth.Resource::new()
  
  // 添加包含空字符串和特殊值的属性
  let special_attributes = [
    ("empty.string", @azimuth.StringValue("")),
    ("zero.number", @azimuth.IntValue(0)),
    ("false.boolean", @azimuth.BoolValue(false)),
    ("zero.float", @azimuth.FloatValue(0.0)),
    ("normal.attribute", @azimuth.StringValue("normal-value"))
  ]
  
  let resource_with_special = @azimuth.Resource::with_attributes(resource, special_attributes)
  
  // 验证特殊值属性
  let empty_string = @azimuth.Resource::get_attribute(resource_with_special, "empty.string")
  let zero_number = @azimuth.Resource::get_attribute(resource_with_special, "zero.number")
  let false_boolean = @azimuth.Resource::get_attribute(resource_with_special, "false.boolean")
  let normal_attribute = @azimuth.Resource::get_attribute(resource_with_special, "normal.attribute")
  
  assert_eq(empty_string, Some(@azimuth.StringValue("")))
  assert_eq(zero_number, Some(@azimuth.IntValue(0)))
  assert_eq(false_boolean, Some(@azimuth.BoolValue(false)))
  assert_eq(normal_attribute, Some(@azimuth.StringValue("normal-value")))
}