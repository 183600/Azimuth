// Advanced Resource Merge Strategy Tests
// 高级资源合并策略测试

test "基础资源合并策略测试" {
  // 测试基础的资源合并策略
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // 为第一个资源设置属性
  let attrs1 = [
    ("service.name", StringValue("service-a")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-a-001")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  
  // 为第二个资源设置属性
  let attrs2 = [
    ("host.name", StringValue("host-server-01")),
    ("host.ip", StringValue("192.168.1.100")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("ubuntu-20.04"))
  ]
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  // 合并资源
  let merged_resource = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  
  // 验证合并后的资源包含所有属性
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("service-a")))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "service.instance.id"), Some(StringValue("instance-a-001")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), Some(StringValue("production")))
  
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("host-server-01")))
  assert_eq(Resource::get_attribute(merged_resource, "host.ip"), Some(StringValue("192.168.1.100")))
  assert_eq(Resource::get_attribute(merged_resource, "os.type"), Some(StringValue("linux")))
  assert_eq(Resource::get_attribute(merged_resource, "os.version"), Some(StringValue("ubuntu-20.04")))
  
  assert_true(true)
}

test "资源属性覆盖策略测试" {
  // 测试资源属性覆盖策略
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // 为第一个资源设置属性
  let attrs1 = [
    ("service.name", StringValue("original-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development")),
    ("custom.attribute", StringValue("original-value"))
  ]
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  
  // 为第二个资源设置部分相同的属性（测试覆盖）
  let attrs2 = [
    ("service.name", StringValue("overridden-service")),
    ("deployment.environment", StringValue("production")),
    ("new.attribute", StringValue("new-value"))
  ]
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  // 合并资源（第二个资源应该覆盖第一个资源的相同属性）
  let merged_resource = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  
  // 验证覆盖策略：第二个资源的属性应该覆盖第一个资源的相同属性
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("overridden-service")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), Some(StringValue("production")))
  
  // 验证未覆盖的属性保持不变
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "custom.attribute"), Some(StringValue("original-value")))
  
  // 验证新增属性
  assert_eq(Resource::get_attribute(merged_resource, "new.attribute"), Some(StringValue("new-value")))
  
  assert_true(true)
}

test "多级资源合并策略测试" {
  // 测试多级资源合并策略
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  let resource3 = Resource::new()
  let resource4 = Resource::new()
  
  // 为每个资源设置不同的属性
  let attrs1 = [
    ("service.name", StringValue("multi-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  
  let attrs2 = [
    ("host.name", StringValue("multi-host")),
    ("host.ip", StringValue("10.0.0.1"))
  ]
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  let attrs3 = [
    ("deployment.environment", StringValue("staging")),
    ("deployment.region", StringValue("us-west-1"))
  ]
  let resource3_with_attrs = Resource::with_attributes(resource3, attrs3)
  
  let attrs4 = [
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("0.1.0"))
  ]
  let resource4_with_attrs = Resource::with_attributes(resource4, attrs4)
  
  // 多级合并
  let merged12 = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  let merged123 = Resource::merge(merged12, resource3_with_attrs)
  let final_merged = Resource::merge(merged123, resource4_with_attrs)
  
  // 验证最终合并结果包含所有属性
  assert_eq(Resource::get_attribute(final_merged, "service.name"), Some(StringValue("multi-service")))
  assert_eq(Resource::get_attribute(final_merged, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(final_merged, "host.name"), Some(StringValue("multi-host")))
  assert_eq(Resource::get_attribute(final_merged, "host.ip"), Some(StringValue("10.0.0.1")))
  assert_eq(Resource::get_attribute(final_merged, "deployment.environment"), Some(StringValue("staging")))
  assert_eq(Resource::get_attribute(final_merged, "deployment.region"), Some(StringValue("us-west-1")))
  assert_eq(Resource::get_attribute(final_merged, "telemetry.sdk.name"), Some(StringValue("azimuth")))
  assert_eq(Resource::get_attribute(final_merged, "telemetry.sdk.version"), Some(StringValue("0.1.0")))
  
  assert_true(true)
}

test "资源合并优先级策略测试" {
  // 测试资源合并优先级策略
  let base_resource = Resource::new()
  let env_resource = Resource::new()
  let host_resource = Resource::new()
  let service_resource = Resource::new()
  
  // 基础资源（最低优先级）
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("base-env")),
    ("host.name", StringValue("base-host"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  // 环境资源（较高优先级）
  let env_attrs = [
    ("deployment.environment", StringValue("production")),
    ("deployment.region", StringValue("us-east-1")),
    ("deployment.zone", StringValue("us-east-1a")
  ]
  let env_with_attrs = Resource::with_attributes(env_resource, env_attrs)
  
  // 主机资源（更高优先级）
  let host_attrs = [
    ("host.name", StringValue("prod-host-01")),
    ("host.ip", StringValue("10.0.1.100")),
    ("host.cpu.count", IntValue(8))
  ]
  let host_with_attrs = Resource::with_attributes(host_resource, host_attrs)
  
  // 服务资源（最高优先级）
  let service_attrs = [
    ("service.name", StringValue("priority-service")),
    ("service.version", StringValue("2.0.0")),
    ("service.instance.id", StringValue("instance-prod-001"))
  ]
  let service_with_attrs = Resource::with_attributes(service_resource, service_attrs)
  
  // 按优先级顺序合并
  let step1 = Resource::merge(base_with_attrs, env_with_attrs)
  let step2 = Resource::merge(step1, host_with_attrs)
  let final_resource = Resource::merge(step2, service_with_attrs)
  
  // 验证优先级策略
  // 最高优先级的属性应该保留
  assert_eq(Resource::get_attribute(final_resource, "service.name"), Some(StringValue("priority-service")))
  assert_eq(Resource::get_attribute(final_resource, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(final_resource, "host.name"), Some(StringValue("prod-host-01")))
  
  // 中等优先级的属性应该保留
  assert_eq(Resource::get_attribute(final_resource, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(final_resource, "host.ip"), StringValue("10.0.1.100"))
  
  // 最低优先级的属性只有在没有更高优先级覆盖时才保留
  assert_eq(Resource::get_attribute(final_resource, "deployment.region"), Some(StringValue("us-east-1")))
  assert_eq(Resource::get_attribute(final_resource, "host.cpu.count"), Some(IntValue(8)))
  
  assert_true(true)
}

test "资源合并类型兼容性测试" {
  // 测试不同类型属性值的合并兼容性
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // 为第一个资源设置各种类型的属性
  let attrs1 = [
    ("string.attr", StringValue("string.value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["item1", "item2"])),
    ("array.int", ArrayIntValue([1, 2, 3]))
  ]
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  
  // 为第二个资源设置相同键但不同类型的属性
  let attrs2 = [
    ("string.attr", StringValue("overridden.string")),
    ("int.attr", IntValue(100)),
    ("float.attr", FloatValue(2.71)),
    ("bool.attr", BoolValue(false)),
    ("array.string", ArrayStringValue(["newitem1", "newitem2"])),
    ("array.int", ArrayIntValue([4, 5, 6]))
  ]
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  // 合并资源
  let merged_resource = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  
  // 验证类型兼容性：相同键的属性应该保持类型一致
  let string_val = Resource::get_attribute(merged_resource, "string.attr")
  let int_val = Resource::get_attribute(merged_resource, "int.attr")
  let float_val = Resource::get_attribute(merged_resource, "float.attr")
  let bool_val = Resource::get_attribute(merged_resource, "bool.attr")
  let array_string_val = Resource::get_attribute(merged_resource, "array.string")
  let array_int_val = Resource::get_attribute(merged_resource, "array.int")
  
  // 验证类型保持不变
  match string_val {
    Some(StringValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match int_val {
    Some(IntValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match float_val {
    Some(FloatValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match bool_val {
    Some(BoolValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match array_string_val {
    Some(ArrayStringValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match array_int_val {
    Some(ArrayIntValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_true(true)
}

test "资源合并边界条件测试" {
  // 测试资源合并的边界条件
  let empty_resource1 = Resource::new()
  let empty_resource2 = Resource::new()
  
  // 测试空资源合并
  let empty_merged = Resource::merge(empty_resource1, empty_resource2)
  assert_eq(Resource::get_attribute(empty_merged, "any.key"), None)
  
  // 测试空资源与非空资源合并
  let non_empty_resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let non_empty_with_attrs = Resource::with_attributes(non_empty_resource, attrs)
  
  let empty_to_non_empty = Resource::merge(empty_resource1, non_empty_with_attrs)
  assert_eq(Resource::get_attribute(empty_to_non_empty, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(empty_to_non_empty, "service.version"), Some(StringValue("1.0.0")))
  
  let non_empty_to_empty = Resource::merge(non_empty_with_attrs, empty_resource1)
  assert_eq(Resource::get_attribute(non_empty_to_empty, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(non_empty_to_empty, "service.version"), Some(StringValue("1.0.0")))
  
  // 测试大量属性的合并
  let large_resource1 = Resource::new()
  let large_resource2 = Resource::new()
  
  let large_attrs1 = []
  for i in 0..100 {
    large_attrs1.push(("large.attr." + i.to_string(), StringValue("value." + i.to_string())))
  }
  let large_resource1_with_attrs = Resource::with_attributes(large_resource1, large_attrs1)
  
  let large_attrs2 = []
  for i in 100..200 {
    large_attrs2.push(("large.attr." + i.to_string(), StringValue("value." + i.to_string())))
  }
  let large_resource2_with_attrs = Resource::with_attributes(large_resource2, large_attrs2)
  
  let large_merged = Resource::merge(large_resource1_with_attrs, large_resource2_with_attrs)
  
  // 验证大量属性合并的正确性
  for i in 0..100 {
    let key = "large.attr." + i.to_string()
    let value = Resource::get_attribute(large_merged, key)
    assert_eq(value, Some(StringValue("value." + i.to_string())))
  }
  
  for i in 100..200 {
    let key = "large.attr." + i.to_string()
    let value = Resource::get_attribute(large_merged, key)
    assert_eq(value, Some(StringValue("value." + i.to_string())))
  }
  
  assert_true(true)
}

test "资源合并性能测试" {
  // 测试资源合并的性能
  let base_resource = Resource::new()
  let merge_resources = []
  
  // 创建多个资源用于合并
  for i in 0..20 {
    let resource = Resource::new()
    let attrs = []
    for j in 0..20 {
      attrs.push(("merge.attr." + i.to_string() + "." + j.to_string(), StringValue("value." + i.to_string() + "." + j.to_string())))
    }
    let resource_with_attrs = Resource::with_attributes(resource, attrs)
    merge_resources.push(resource_with_attrs)
  }
  
  // 测量合并性能
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  let merged_resource = base_resource
  for resource in merge_resources {
    merged_resource = Resource::merge(merged_resource, resource)
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let duration = end_time - start_time
  
  // 验证性能在合理范围内（设置为相对宽松的限制）
  assert_true(duration < 5000000000L)  // 小于5秒
  
  // 验证合并结果的正确性
  for i in 0..20 {
    for j in 0..20 {
      let key = "merge.attr." + i.to_string() + "." + j.to_string()
      let value = Resource::get_attribute(merged_resource, key)
      assert_eq(value, Some(StringValue("value." + i.to_string() + "." + j.to_string())))
    }
  }
  
  assert_true(true)
}