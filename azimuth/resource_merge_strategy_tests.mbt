// 资源合并策略测试用例
// 测试遥测资源的属性合并策略和优先级规则

test "resource_attribute_merge_basic" {
  // 测试基本资源属性合并
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource_with_base_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),  // 覆盖基资源的属性
    ("host.name", StringValue("override-host")),        // 新增属性
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_override_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // 执行合并
  let merged_resource = Resource::merge(resource_with_base_attrs, resource_with_override_attrs)
  
  // 验证合并结果
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  let environment = Resource::get_attribute(merged_resource, "deployment.environment")
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  let instance_id = Resource::get_attribute(merged_resource, "service.instance.id")
  
  assert_eq(service_name, Some(StringValue("override-service")))  // 被覆盖
  assert_eq(service_version, Some(StringValue("1.0.0")))         // 保留
  assert_eq(environment, Some(StringValue("production")))        // 保留
  assert_eq(host_name, Some(StringValue("override-host")))       // 新增
  assert_eq(instance_id, Some(StringValue("instance-123")))      // 新增
}

test "resource_attribute_merge_type_safety" {
  // 测试资源属性合并时的类型安全性
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.port", IntValue(8080)),
    ("service.ratio", FloatValue(0.95)),
    ("service.enabled", BoolValue(true))
  ]
  let resource_with_base_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.port", IntValue(9090)),           // 相同类型覆盖
    ("service.ratio", FloatValue(0.85)),        // 相同类型覆盖
    ("service.enabled", BoolValue(false))       // 相同类型覆盖
  ]
  let resource_with_override_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // 执行合并
  let merged_resource = Resource::merge(resource_with_base_attrs, resource_with_override_attrs)
  
  // 验证类型安全性和值
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let service_port = Resource::get_attribute(merged_resource, "service.port")
  let service_ratio = Resource::get_attribute(merged_resource, "service.ratio")
  let service_enabled = Resource::get_attribute(merged_resource, "service.enabled")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_false(true, "Expected StringValue for service.name")
  }
  
  match service_port {
    Some(IntValue(port)) => assert_eq(port, 9090)
    _ => assert_false(true, "Expected IntValue for service.port")
  }
  
  match service_ratio {
    Some(FloatValue(ratio)) => assert_eq(ratio, 0.85)
    _ => assert_false(true, "Expected FloatValue for service.ratio")
  }
  
  match service_enabled {
    Some(BoolValue(enabled)) => assert_false(enabled)
    _ => assert_false(true, "Expected BoolValue for service.enabled")
  }
}

test "resource_attribute_merge_array_types" {
  // 测试数组类型属性的合并
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.tags", ArrayStringValue(["web", "api", "v1"])),
    ("service.endpoints", ArrayStringValue(["/users", "/orders"])),
    ("service.ports", ArrayIntValue([8080, 8443]))
  ]
  let resource_with_base_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.tags", ArrayStringValue(["web", "api", "v2"])),  // 覆盖数组
    ("service.endpoints", ArrayStringValue(["/users", "/products", "/admin"])),  // 覆盖数组
    ("new.service.endpoints", ArrayIntValue([3000, 3001]))    // 新增数组属性
  ]
  let resource_with_override_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // 执行合并
  let merged_resource = Resource::merge(resource_with_base_attrs, resource_with_override_attrs)
  
  // 验证数组合并结果
  let service_tags = Resource::get_attribute(merged_resource, "service.tags")
  let service_endpoints = Resource::get_attribute(merged_resource, "service.endpoints")
  let service_ports = Resource::get_attribute(merged_resource, "service.ports")
  let new_endpoints = Resource::get_attribute(merged_resource, "new.service.endpoints")
  
  match service_tags {
    Some(ArrayStringValue(tags)) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "web")
      assert_eq(tags[1], "api")
      assert_eq(tags[2], "v2")  // 被覆盖
    }
    _ => assert_false(true, "Expected ArrayStringValue for service.tags")
  }
  
  match service_endpoints {
    Some(ArrayStringValue(endpoints)) => {
      assert_eq(endpoints.length(), 3)
      assert_eq(endpoints[0], "/users")
      assert_eq(endpoints[1], "/products")  // 被覆盖
      assert_eq(endpoints[2], "/admin")      // 新增
    }
    _ => assert_false(true, "Expected ArrayStringValue for service.endpoints")
  }
  
  match service_ports {
    Some(ArrayIntValue(ports)) => {
      assert_eq(ports.length(), 2)
      assert_eq(ports[0], 8080)
      assert_eq(ports[1], 8443)  // 保留
    }
    _ => assert_false(true, "Expected ArrayIntValue for service.ports")
  }
  
  match new_endpoints {
    Some(ArrayIntValue(endpoints)) => {
      assert_eq(endpoints.length(), 2)
      assert_eq(endpoints[0], 3000)
      assert_eq(endpoints[1], 3001)
    }
    _ => assert_false(true, "Expected ArrayIntValue for new.service.endpoints")
  }
}

test "resource_attribute_merge_empty_resources" {
  // 测试空资源的合并
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let resource_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let empty_resource = Resource::new()
  
  // 测试基资源与空资源合并
  let merged_1 = Resource::merge(resource_with_attrs, empty_resource)
  let service_name_1 = Resource::get_attribute(merged_1, "service.name")
  let service_version_1 = Resource::get_attribute(merged_1, "service.version")
  
  assert_eq(service_name_1, Some(StringValue("base-service")))
  assert_eq(service_version_1, Some(StringValue("1.0.0")))
  
  // 测试空资源与基资源合并
  let merged_2 = Resource::merge(empty_resource, resource_with_attrs)
  let service_name_2 = Resource::get_attribute(merged_2, "service.name")
  let service_version_2 = Resource::get_attribute(merged_2, "service.version")
  
  assert_eq(service_name_2, Some(StringValue("base-service")))
  assert_eq(service_version_2, Some(StringValue("1.0.0")))
}

test "resource_attribute_merge_complex_hierarchy" {
  // 测试复杂资源层次结构的合并
  let level1_resource = Resource::new()
  let level1_attrs = [
    ("service.name", StringValue("level1-service")),
    ("service.owner", StringValue("team-a")),
    ("service.tier", StringValue("frontend")),
    ("region", StringValue("us-west-1"))
  ]
  let resource_level1 = Resource::with_attributes(level1_resource, level1_attrs)
  
  let level2_resource = Resource::new()
  let level2_attrs = [
    ("service.name", StringValue("level2-service")),  // 覆盖level1
    ("service.environment", StringValue("staging")),  // 新增
    ("service.tier", StringValue("backend")),         // 覆盖level1
    ("zone", StringValue("us-west-1a"))              // 新增
  ]
  let resource_level2 = Resource::with_attributes(level2_resource, level2_attrs)
  
  let level3_resource = Resource::new()
  let level3_attrs = [
    ("service.name", StringValue("level3-service")),     // 再次覆盖
    ("service.version", StringValue("2.1.0")),           // 新增
    ("service.environment", StringValue("production")),  // 覆盖level2
    ("instance.id", StringValue("i-1234567890abcdef0"))  // 新增
  ]
  let resource_level3 = Resource::with_attributes(level3_resource, level3_attrs)
  
  // 逐级合并
  let merged_1_2 = Resource::merge(resource_level1, resource_level2)
  let final_merged = Resource::merge(merged_1_2, resource_level3)
  
  // 验证最终合并结果
  let service_name = Resource::get_attribute(final_merged, "service.name")
  let service_owner = Resource::get_attribute(final_merged, "service.owner")
  let service_tier = Resource::get_attribute(final_merged, "service.tier")
  let region = Resource::get_attribute(final_merged, "region")
  let service_environment = Resource::get_attribute(final_merged, "service.environment")
  let zone = Resource::get_attribute(final_merged, "zone")
  let service_version = Resource::get_attribute(final_merged, "service.version")
  let instance_id = Resource::get_attribute(final_merged, "instance.id")
  
  assert_eq(service_name, Some(StringValue("level3-service")))     // 最终覆盖
  assert_eq(service_owner, Some(StringValue("team-a")))            // 来自level1
  assert_eq(service_tier, Some(StringValue("backend")))            // 来自level2
  assert_eq(region, Some(StringValue("us-west-1")))                // 来自level1
  assert_eq(service_environment, Some(StringValue("production")))  // 来自level3
  assert_eq(zone, Some(StringValue("us-west-1a")))                 // 来自level2
  assert_eq(service_version, Some(StringValue("2.1.0")))           // 来自level3
  assert_eq(instance_id, Some(StringValue("i-1234567890abcdef0"))) // 来自level3
}