// Azimuth Resource Merge Strategy Test Suite
// 资源合并策略测试用例，专注于资源属性的合并策略，包括冲突解决、优先级处理和边界条件

test "基本资源属性合并测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::new()
  
  // 创建带有属性的资源
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("server-01")),
    ("os.type", StringValue("linux"))
  ]
  
  let base_resource_with_attrs = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // 创建覆盖资源
  let override_resource = azimuth::Resource::new()
  
  let override_attrs = [
    ("service.name", StringValue("azimuth-service-updated")),  // 覆盖现有属性
    ("service.version", StringValue("1.1.0")),                 // 覆盖现有属性
    ("deployment.environment", StringValue("production")),    // 新属性
    ("host.name", StringValue("server-01-updated")),          // 覆盖现有属性
    ("custom.attribute", StringValue("custom-value"))         // 新属性
  ]
  
  let override_resource_with_attrs = azimuth::Resource::with_attributes(override_resource, override_attrs)
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(base_resource_with_attrs, override_resource_with_attrs)
  
  // 验证合并结果（根据实现，覆盖资源应该优先）
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("azimuth-service-updated")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("1.1.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.environment"), Some(StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("server-01-updated")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "custom.attribute"), Some(StringValue("custom-value")))
  
  // 验证未被覆盖的属性
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.instance.id"), Some(StringValue("instance-123")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "os.type"), Some(StringValue("linux")))
  
  // 验证不存在的属性
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "non.existent.attribute"), None)
}

test "多级资源合并和优先级测试" {
  // 创建三级资源：系统级 -> 应用级 -> 实例级
  
  // 系统级资源
  let system_resource = azimuth::Resource::new()
  let system_attrs = [
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("ubuntu-20.04")),
    ("host.name", StringValue("prod-server-01")),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2"))
  ]
  
  let system_resource_with_attrs = azimuth::Resource::with_attributes(system_resource, system_attrs)
  
  // 应用级资源
  let app_resource = azimuth::Resource::new()
  let app_attrs = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("2.0.0")),
    ("service.namespace", StringValue("production")),
    ("host.name", StringValue("container-001")),  // 覆盖系统级host.name
    ("service.language", StringValue("moonbit"))
  ]
  
  let app_resource_with_attrs = azimuth::Resource::with_attributes(app_resource, app_attrs)
  
  // 实例级资源
  let instance_resource = azimuth::Resource::new()
  let instance_attrs = [
    ("service.instance.id", StringValue("pod-xyz-123")),
    ("service.version", StringValue("2.0.1")),  // 覆盖应用级版本
    ("deployment.environment", StringValue("prod")),
    ("host.name", StringValue("pod-xyz-123")),  // 再次覆盖host.name
    ("custom.pod.ip", StringValue("10.0.1.100"))
  ]
  
  let instance_resource_with_attrs = azimuth::Resource::with_attributes(instance_resource, instance_attrs)
  
  // 多级合并：系统 -> 应用 -> 实例
  let system_app_merged = azimuth::Resource::merge(system_resource_with_attrs, app_resource_with_attrs)
  let final_merged = azimuth::Resource::merge(system_app_merged, instance_resource_with_attrs)
  
  // 验证最终合并结果（实例级优先级最高）
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.name"), Some(StringValue("user-service")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.version"), Some(StringValue("2.0.1")))  // 实例级覆盖
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.instance.id"), Some(StringValue("pod-xyz-123")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.namespace"), Some(StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.language"), Some(StringValue("moonbit")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "deployment.environment"), Some(StringValue("prod")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "host.name"), Some(StringValue("pod-xyz-123")))  // 实例级覆盖
  assert_eq(azimuth::Resource::get_attribute(final_merged, "custom.pod.ip"), Some(StringValue("10.0.1.100")))
  
  // 验证系统级属性（未被覆盖的）
  assert_eq(azimuth::Resource::get_attribute(final_merged, "os.type"), Some(StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "os.version"), Some(StringValue("ubuntu-20.04")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "cloud.provider"), Some(StringValue("aws")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "cloud.region"), Some(StringValue("us-west-2")))
}

test "不同类型属性值的合并测试" {
  // 创建包含不同类型属性值的资源
  let base_resource = azimuth::Resource::new()
  
  let base_attrs = [
    ("string.attr", StringValue("base_string")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true)),
    ("string.array.attr", ArrayStringValue(["base1", "base2", "base3"])),
    ("int.array.attr", ArrayIntValue([1, 2, 3]))
  ]
  
  let base_resource_with_attrs = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // 创建覆盖资源，包含不同类型的值
  let override_resource = azimuth::Resource::new()
  
  let override_attrs = [
    ("string.attr", StringValue("override_string")),
    ("int.attr", IntValue(100)),
    ("float.attr", FloatValue(2.71)),
    ("bool.attr", BoolValue(false)),
    ("string.array.attr", ArrayStringValue(["override1", "override2"])),
    ("int.array.attr", ArrayIntValue([10, 20, 30, 40]))
  ]
  
  let override_resource_with_attrs = azimuth::Resource::with_attributes(override_resource, override_attrs)
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(base_resource_with_attrs, override_resource_with_attrs)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "string.attr"), Some(StringValue("override_string")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "int.attr"), Some(IntValue(100)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "float.attr"), Some(FloatValue(2.71)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "bool.attr"), Some(BoolValue(false)))
  
  match azimuth::Resource::get_attribute(merged_resource, "string.array.attr") {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], "override1")
      assert_eq(arr[1], "override2")
    }
    _ => assert_true(false)
  }
  
  match azimuth::Resource::get_attribute(merged_resource, "int.array.attr") {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], 10)
      assert_eq(arr[1], 20)
      assert_eq(arr[2], 30)
      assert_eq(arr[3], 40)
    }
    _ => assert_true(false)
  }
}

test "边界条件和特殊属性值合并测试" {
  // 创建包含边界值的资源
  let base_resource = azimuth::Resource::new()
  
  let base_attrs = [
    ("empty.string", StringValue("")),
    ("unicode.string", StringValue("Unicode测试_🚀_🌟")),
    ("special.chars", StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")),
    ("long.string", StringValue("这是一个很长的字符串，用于测试长字符串的合并处理能力。" + 
      "重复内容：测试长字符串处理。重复内容：测试长字符串处理。" +
      "重复内容：测试长字符串处理。重复内容：测试长字符串处理。")),
    ("max.int", IntValue(2147483647)),
    ("min.int", IntValue(-2147483648)),
    ("zero.float", FloatValue(0.0)),
    ("negative.float", FloatValue(-3.14159)),
    ("true.bool", BoolValue(true)),
    ("false.bool", BoolValue(false)),
    ("empty.string.array", ArrayStringValue([])),
    ("empty.int.array", ArrayIntValue([])),
    ("single.string.array", ArrayStringValue(["single"])),
    ("single.int.array", ArrayIntValue([42]))
  ]
  
  let base_resource_with_attrs = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // 创建覆盖资源
  let override_resource = azimuth::Resource::new()
  
  let override_attrs = [
    ("empty.string", StringValue("not_empty")),
    ("unicode.string", StringValue("Unicode覆盖_🔥_💡")),
    ("special.chars", StringValue("覆盖特殊字符")),
    ("long.string", StringValue("覆盖的长字符串")),
    ("max.int", IntValue(0)),
    ("min.int", IntValue(0)),
    ("zero.float", FloatValue(1.0)),
    ("negative.float", FloatValue(0.0)),
    ("true.bool", BoolValue(false)),
    ("false.bool", BoolValue(true)),
    ("empty.string.array", ArrayStringValue(["not_empty"])),
    ("empty.int.array", ArrayIntValue([1])),
    ("single.string.array", ArrayStringValue([])),
    ("single.int.array", ArrayIntValue([]))
  ]
  
  let override_resource_with_attrs = azimuth::Resource::with_attributes(override_resource, override_attrs)
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(base_resource_with_attrs, override_resource_with_attrs)
  
  // 验证边界值合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "empty.string"), Some(StringValue("not_empty")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "unicode.string"), Some(StringValue("Unicode覆盖_🔥_💡")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "special.chars"), Some(StringValue("覆盖特殊字符")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "long.string"), Some(StringValue("覆盖的长字符串")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "max.int"), Some(IntValue(0)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "min.int"), Some(IntValue(0)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "zero.float"), Some(FloatValue(1.0)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "negative.float"), Some(FloatValue(0.0)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "true.bool"), Some(BoolValue(false)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "false.bool"), Some(BoolValue(true)))
  
  match azimuth::Resource::get_attribute(merged_resource, "empty.string.array") {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "not_empty")
    }
    _ => assert_true(false)
  }
  
  match azimuth::Resource::get_attribute(merged_resource, "empty.int.array") {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 1)
    }
    _ => assert_true(false)
  }
  
  match azimuth::Resource::get_attribute(merged_resource, "single.string.array") {
    Some(ArrayStringValue(arr)) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  match azimuth::Resource::get_attribute(merged_resource, "single.int.array") {
    Some(ArrayIntValue(arr)) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
}

test "空资源和大规模属性合并测试" {
  // 测试空资源合并
  let empty_resource1 = azimuth::Resource::new()
  let empty_resource2 = azimuth::Resource::new()
  
  let empty_attrs1: Array[(String, AttributeValue)] = []
  let empty_attrs2: Array[(String, AttributeValue)] = []
  
  let empty_resource1_with_attrs = azimuth::Resource::with_attributes(empty_resource1, empty_attrs1)
  let empty_resource2_with_attrs = azimuth::Resource::with_attributes(empty_resource2, empty_attrs2)
  
  let merged_empty = azimuth::Resource::merge(empty_resource1_with_attrs, empty_resource2_with_attrs)
  
  // 验证空资源合并
  assert_eq(azimuth::Resource::get_attribute(merged_empty, "any.key"), None)
  
  // 测试空资源与非空资源合并
  let non_empty_resource = azimuth::Resource::new()
  let non_empty_attrs = [
    ("key1", StringValue("value1")),
    ("key2", IntValue(42)),
    ("key3", FloatValue(3.14))
  ]
  
  let non_empty_resource_with_attrs = azimuth::Resource::with_attributes(non_empty_resource, non_empty_attrs)
  
  let merged_empty_nonempty = azimuth::Resource::merge(empty_resource1_with_attrs, non_empty_resource_with_attrs)
  let merged_nonempty_empty = azimuth::Resource::merge(non_empty_resource_with_attrs, empty_resource1_with_attrs)
  
  // 验证空资源与非空资源合并
  assert_eq(azimuth::Resource::get_attribute(merged_empty_nonempty, "key1"), Some(StringValue("value1")))
  assert_eq(azimuth::Resource::get_attribute(merged_empty_nonempty, "key2"), Some(IntValue(42)))
  assert_eq(azimuth::Resource::get_attribute(merged_empty_nonempty, "key3"), Some(FloatValue(3.14)))
  
  assert_eq(azimuth::Resource::get_attribute(merged_nonempty_empty, "key1"), Some(StringValue("value1")))
  assert_eq(azimuth::Resource::get_attribute(merged_nonempty_empty, "key2"), Some(IntValue(42)))
  assert_eq(azimuth::Resource::get_attribute(merged_nonempty_empty, "key3"), Some(FloatValue(3.14)))
  
  // 测试大规模属性合并
  let large_resource1 = azimuth::Resource::new()
  let large_resource2 = azimuth::Resource::new()
  
  let large_attrs1 = [
    for i in range(0, 100) {
      ("large_key_" + i.to_string(), StringValue("large_value_" + i.to_string()))
    }
  ]
  
  let large_attrs2 = [
    for i in range(50, 150) {
      ("large_key_" + i.to_string(), StringValue("override_value_" + i.to_string()))
    }
  ]
  
  let large_resource1_with_attrs = azimuth::Resource::with_attributes(large_resource1, large_attrs1)
  let large_resource2_with_attrs = azimuth::Resource::with_attributes(large_resource2, large_attrs2)
  
  let merged_large = azimuth::Resource::merge(large_resource1_with_attrs, large_resource2_with_attrs)
  
  // 验证大规模合并结果
  // 前50个键应该保持原值
  for i in range(0, 50) {
    let key = "large_key_" + i.to_string()
    let expected_value = "large_value_" + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_large, key), Some(StringValue(expected_value)))
  }
  
  // 50-149的键应该被覆盖
  for i in range(50, 150) {
    let key = "large_key_" + i.to_string()
    let expected_value = "override_value_" + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_large, key), Some(StringValue(expected_value)))
  }
  
  // 验证数组长度
  assert_eq(large_attrs1.length(), 100)
  assert_eq(large_attrs2.length(), 100)
}