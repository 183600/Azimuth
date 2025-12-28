// 资源合并策略测试用例
// 测试Resource类型的合并逻辑和策略

test "resource_basic_merge" {
  // 测试基本资源合并
  let base_attrs = [("service.name", StringValue("base-service")), ("version", StringValue("1.0.0"))]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [("environment", StringValue("production")), ("version", StringValue("2.0.0"))]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // 验证合并结果
  let env_value = Resource::get_attribute(merged, "environment")
  match env_value {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => assert_false(true, "Expected environment attribute")
  }
  
  let version_value = Resource::get_attribute(merged, "version")
  match version_value {
    Some(StringValue(version)) => assert_eq(version, "2.0.0")
    _ => assert_false(true, "Expected version attribute")
  }
}

test "resource_empty_merge" {
  // 测试空资源合并
  let base_attrs = [("service.name", StringValue("test-service"))]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let empty_resource = Resource::new()
  
  let merged1 = Resource::merge(base_resource, empty_resource)
  let service_name = Resource::get_attribute(merged1, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_false(true, "Expected service.name attribute")
  }
  
  let merged2 = Resource::merge(empty_resource, base_resource)
  let service_name2 = Resource::get_attribute(merged2, "service.name")
  match service_name2 {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_false(true, "Expected service.name attribute")
  }
}

test "resource_attribute_priority_merge" {
  // 测试属性优先级合并
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.instance.id", StringValue("base-instance")),
    ("priority", StringValue("low"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.instance.id", StringValue("override-instance")),
    ("priority", StringValue("high")),
    ("new.attribute", StringValue("new-value"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // 验证基础属性保持不变
  let service_name = Resource::get_attribute(merged, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "base-service")
    _ => assert_false(true, "Expected base service.name")
  }
  
  // 验证覆盖属性被替换
  let instance_id = Resource::get_attribute(merged, "service.instance.id")
  match instance_id {
    Some(StringValue(id)) => assert_eq(id, "override-instance")
    _ => assert_false(true, "Expected override service.instance.id")
  }
  
  // 验证新属性被添加
  let new_attr = Resource::get_attribute(merged, "new.attribute")
  match new_attr {
    Some(StringValue(value)) => assert_eq(value, "new-value")
    _ => assert_false(true, "Expected new.attribute")
  }
}

test "resource_type_compatibility_merge" {
  // 测试不同类型属性的兼容性合并
  let base_attrs = [
    ("string.attr", StringValue("string-value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("string.attr", StringValue("new-string-value")),
    ("int.attr", IntValue(100)),
    ("float.attr", FloatValue(2.71)),
    ("bool.attr", BoolValue(false))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // 验证字符串类型
  let string_attr = Resource::get_attribute(merged, "string.attr")
  match string_attr {
    Some(StringValue(value)) => assert_eq(value, "new-string-value")
    _ => assert_false(true, "Expected StringValue")
  }
  
  // 验证整数类型
  let int_attr = Resource::get_attribute(merged, "int.attr")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 100)
    _ => assert_false(true, "Expected IntValue")
  }
  
  // 验证浮点类型
  let float_attr = Resource::get_attribute(merged, "float.attr")
  match float_attr {
    Some(FloatValue(value)) => assert_true(value > 2.70 && value < 2.72)
    _ => assert_false(true, "Expected FloatValue")
  }
  
  // 验证布尔类型
  let bool_attr = Resource::get_attribute(merged, "bool.attr")
  match bool_attr {
    Some(BoolValue(value)) => assert_false(value)
    _ => assert_false(true, "Expected BoolValue")
  }
}

test "resource_array_attribute_merge" {
  // 测试数组属性合并
  let base_attrs = [
    ("string.array", ArrayStringValue(["item1", "item2"])),
    ("int.array", ArrayIntValue([1, 2, 3]))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("string.array", ArrayStringValue(["item3", "item4"])),
    ("int.array", ArrayIntValue([4, 5, 6]))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // 验证字符串数组被替换
  let string_array = Resource::get_attribute(merged, "string.array")
  match string_array {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], "item3")
      assert_eq(arr[1], "item4")
    }
    _ => assert_false(true, "Expected ArrayStringValue")
  }
  
  // 验证整数数组被替换
  let int_array = Resource::get_attribute(merged, "int.array")
  match int_array {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 4)
      assert_eq(arr[2], 6)
    }
    _ => assert_false(true, "Expected ArrayIntValue")
  }
}

test "resource_complex_merge_scenario" {
  // 测试复杂合并场景
  let service_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.namespace", StringValue("ecommerce"))
  ]
  let service_resource = Resource::with_attributes(Resource::new(), service_attrs)
  
  let host_attrs = [
    ("host.name", StringValue("prod-server-01")),
    ("host.ip", StringValue("192.168.1.100")),
    ("service.version", StringValue("2.0.0"))  // 这应该覆盖service.version
  ]
  let host_resource = Resource::with_attributes(Resource::new(), host_attrs)
  
  let deployment_attrs = [
    ("deployment.environment", StringValue("production")),
    ("deployment.region", StringValue("us-west-2")),
    ("host.name", StringValue("prod-server-02"))  // 这应该覆盖host.name
  ]
  let deployment_resource = Resource::with_attributes(Resource::new(), deployment_attrs)
  
  // 逐步合并
  let step1 = Resource::merge(service_resource, host_resource)
  let final_resource = Resource::merge(step1, deployment_resource)
  
  // 验证最终结果
  let service_name = Resource::get_attribute(final_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "payment-service")
    _ => assert_false(true, "Expected service.name")
  }
  
  let service_version = Resource::get_attribute(final_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "2.0.0")  // 应该是host的版本
    _ => assert_false(true, "Expected service.version")
  }
  
  let host_name = Resource::get_attribute(final_resource, "host.name")
  match host_name {
    Some(StringValue(name)) => assert_eq(name, "prod-server-02")  // 应该是deployment的主机名
    _ => assert_false(true, "Expected host.name")
  }
  
  let deployment_env = Resource::get_attribute(final_resource, "deployment.environment")
  match deployment_env {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => assert_false(true, "Expected deployment.environment")
  }
}

test "resource_merge_null_safety" {
  // 测试资源合并的空值安全性
  let base_attrs = [("existing.attr", StringValue("value"))]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // 测试与空资源合并
  let empty_resource = Resource::new()
  let merged1 = Resource::merge(base_resource, empty_resource)
  let existing_attr = Resource::get_attribute(merged1, "existing.attr")
  match existing_attr {
    Some(StringValue(value)) => assert_eq(value, "value")
    _ => assert_false(true, "Expected existing.attr")
  }
  
  // 测试空资源与有属性资源合并
  let merged2 = Resource::merge(empty_resource, base_resource)
  let existing_attr2 = Resource::get_attribute(merged2, "existing.attr")
  match existing_attr2 {
    Some(StringValue(value)) => assert_eq(value, "value")
    _ => assert_false(true, "Expected existing.attr")
  }
}

test "resource_merge_attribute_count_preservation" {
  // 测试资源合并时属性数量的保持
  let base_attrs = [
    ("attr1", StringValue("value1")),
    ("attr2", StringValue("value2")),
    ("attr3", StringValue("value3"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("attr2", StringValue("new-value2")),  // 覆盖现有属性
    ("attr4", StringValue("value4"))       // 新增属性
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // 验证所有属性都存在
  let attr1 = Resource::get_attribute(merged, "attr1")
  match attr1 {
    Some(StringValue(value)) => assert_eq(value, "value1")
    _ => assert_false(true, "Expected attr1")
  }
  
  let attr2 = Resource::get_attribute(merged, "attr2")
  match attr2 {
    Some(StringValue(value)) => assert_eq(value, "new-value2")
    _ => assert_false(true, "Expected attr2")
  }
  
  let attr3 = Resource::get_attribute(merged, "attr3")
  match attr3 {
    Some(StringValue(value)) => assert_eq(value, "value3")
    _ => assert_false(true, "Expected attr3")
  }
  
  let attr4 = Resource::get_attribute(merged, "attr4")
  match attr4 {
    Some(StringValue(value)) => assert_eq(value, "value4")
    _ => assert_false(true, "Expected attr4")
  }
}