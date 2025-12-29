// Resource合并操作测试用例
test "resource_merge_basic" {
  // 测试基本的Resource合并操作
  let resource1 = Resource::default("service-1")
  let resource2 = Resource::default("service-2")
  
  // 模拟Resource合并逻辑 - 创建一个新的Resource，合并两个Resource的属性
  let merged_attrs : Attributes = [
    ("service.1.name", AttributeValue::string(resource1.service_name)),
    ("service.2.name", AttributeValue::string(resource2.service_name)),
    ("merged.timestamp", AttributeValue::int(1234567890L))
  ]
  
  assert_eq(merged_attrs.length(), 3)
  
  match merged_attrs[0].1 {
    StringValue(name) => assert_eq(name, "service-1")
    _ => @test.fail("Expected StringValue")
  }
  
  match merged_attrs[1].1 {
    StringValue(name) => assert_eq(name, "service-2")
    _ => @test.fail("Expected StringValue")
  }
  
  match merged_attrs[2].1 {
    IntValue(timestamp) => assert_eq(timestamp, 1234567890L)
    _ => @test.fail("Expected IntValue")
  }
}

test "resource_merge_with_attributes" {
  // 测试带有属性的Resource合并
  let resource1 = Resource::default("service-1")
  let resource2 = Resource::default("service-2")
  
  let attrs1 : Attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("deployment.region", AttributeValue::string("us-west"))
  ]
  
  let attrs2 : Attributes = [
    ("service.environment", AttributeValue::string("prod")),
    ("service.instance.id", AttributeValue::string("instance-123")),
    ("host.name", AttributeValue::string("web-server-01"))
  ]
  
  // 合并属性，模拟合并逻辑
  let merged_attrs : Attributes = [
    ("primary.service", AttributeValue::string(resource1.service_name)),
    ("secondary.service", AttributeValue::string(resource2.service_name))
  ]
  
  // 添加第一个Resource的属性
  let mut i = 0
  while i < attrs1.length() {
    merged_attrs.push(attrs1[i])
    i = i + 1
  }
  
  // 添加第二个Resource的属性
  i = 0
  while i < attrs2.length() {
    merged_attrs.push(attrs2[i])
    i = i + 1
  }
  
  // 添加合并元数据
  merged_attrs.push(("merged.count", AttributeValue::int(2L)))
  merged_attrs.push(("merged.timestamp", AttributeValue::int(1234567890L)))
  
  assert_eq(merged_attrs.length(), 10) // 2 + 3 + 3 + 2
  
  // 验证合并后的属性
  match merged_attrs[0].1 {
    StringValue(name) => assert_eq(name, "service-1")
    _ => @test.fail("Expected StringValue")
  }
  
  match merged_attrs[1].1 {
    StringValue(name) => assert_eq(name, "service-2")
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证第一个Resource的属性
  match merged_attrs[2].1 {
    StringValue(ns) => assert_eq(ns, "production")
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证第二个Resource的属性
  match merged_attrs[5].1 {
    StringValue(env) => assert_eq(env, "prod")
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证合并元数据
  match merged_attrs[8].1 {
    IntValue(count) => assert_eq(count, 2L)
    _ => @test.fail("Expected IntValue")
  }
}

test "resource_merge_conflict_resolution" {
  // 测试Resource合并时的冲突解决
  let resource1 = Resource::default("service-1")
  let resource2 = Resource::default("service-2")
  
  let attrs1 : Attributes = [
    ("service.version", AttributeValue::string("1.0.0")),
    ("deployment.environment", AttributeValue::string("staging")),
    ("service.priority", AttributeValue::int(1L))
  ]
  
  let attrs2 : Attributes = [
    ("service.version", AttributeValue::string("2.0.0")),  // 冲突的键
    ("deployment.environment", AttributeValue::string("production")),  // 冲突的键
    ("service.region", AttributeValue::string("us-east"))
  ]
  
  // 模拟冲突解决策略：使用前缀区分不同来源
  let merged_attrs : Attributes = []
  
  // 添加第一个Resource的属性，带前缀
  let mut i = 0
  while i < attrs1.length() {
    let (key, value) = attrs1[i]
    merged_attrs.push(("source1." + key, value))
    i = i + 1
  }
  
  // 添加第二个Resource的属性，带前缀
  i = 0
  while i < attrs2.length() {
    let (key, value) = attrs2[i]
    merged_attrs.push(("source2." + key, value))
    i = i + 1
  }
  
  assert_eq(merged_attrs.length(), 6)
  
  // 验证冲突解决
  match merged_attrs[0].1 {
    StringValue(version) => assert_eq(version, "1.0.0")
    _ => @test.fail("Expected StringValue")
  }
  
  match merged_attrs[3].1 {
    StringValue(version) => assert_eq(version, "2.0.0")
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证键名不同
  assert_eq(merged_attrs[0].0, "source1.service.version")
  assert_eq(merged_attrs[3].0, "source2.service.version")
}

test "resource_merge_complex_attributes" {
  // 测试复杂属性的合并
  let resource1 = Resource::default("complex-service-1")
  let resource2 = Resource::default("complex-service-2")
  
  let complex_attrs1 : Attributes = [
    ("service.tags", AttributeValue::array_string(["web", "api", "v1"])),
    ("service.ports", AttributeValue::array_int([8080L, 8443L])),
    ("service.thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
    ("service.features", AttributeValue::array_bool([true, false, true]))
  ]
  
  let complex_attrs2 : Attributes = [
    ("service.tags", AttributeValue::array_string(["backend", "database", "v2"])),
    ("service.ports", AttributeValue::array_int([9090L, 9091L])),
    ("service.metrics", AttributeValue::array_float([0.1, 0.5, 0.9])),
    ("service.capabilities", AttributeValue::array_bool([false, true, false, true]))
  ]
  
  // 合并复杂属性
  let merged_attrs : Attributes = [
    ("primary.service", AttributeValue::string(resource1.service_name)),
    ("secondary.service", AttributeValue::string(resource2.service_name))
  ]
  
  // 添加第一个Resource的复杂属性
  let mut i = 0
  while i < complex_attrs1.length() {
    let (key, value) = complex_attrs1[i]
    merged_attrs.push(("service1." + key, value))
    i = i + 1
  }
  
  // 添加第二个Resource的复杂属性
  i = 0
  while i < complex_attrs2.length() {
    let (key, value) = complex_attrs2[i]
    merged_attrs.push(("service2." + key, value))
    i = i + 1
  }
  
  assert_eq(merged_attrs.length(), 10) // 2 + 4 + 4
  
  // 验证数组属性
  match merged_attrs[2].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "web")
      assert_eq(tags[1], "api")
      assert_eq(tags[2], "v1")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match merged_attrs[3].1 {
    ArrayIntValue(ports) => {
      assert_eq(ports.length(), 2)
      assert_eq(ports[0], 8080L)
      assert_eq(ports[1], 8443L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match merged_attrs[6].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "backend")
      assert_eq(tags[1], "database")
      assert_eq(tags[2], "v2")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match merged_attrs[9].1 {
    ArrayBoolValue(capabilities) => {
      assert_eq(capabilities.length(), 4)
      assert_eq(capabilities[0], false)
      assert_eq(capabilities[1], true)
      assert_eq(capabilities[2], false)
      assert_eq(capabilities[3], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "resource_merge_empty_and_edge_cases" {
  // 测试空Resource和边界情况的合并
  let empty_resource1 = Resource::default("")
  let empty_resource2 = Resource::default("")
  
  let empty_attrs1 : Attributes = []
  let empty_attrs2 : Attributes = []
  
  // 合并空Resource
  let merged_empty_attrs : Attributes = [
    ("service1.name", AttributeValue::string(empty_resource1.service_name)),
    ("service2.name", AttributeValue::string(empty_resource2.service_name))
  ]
  
  assert_eq(merged_empty_attrs.length(), 2)
  
  match merged_empty_attrs[0].1 {
    StringValue(name) => assert_eq(name, "")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试一个有属性一个没有属性的情况
  let resource_with_attrs = Resource::default("service-with-attrs")
  let resource_without_attrs = Resource::default("service-without-attrs")
  
  let single_attrs : Attributes = [
    ("only.attr", AttributeValue::string("only.value"))
  ]
  
  let merged_mixed_attrs : Attributes = [
    ("service.with.attrs", AttributeValue::string(resource_with_attrs.service_name)),
    ("service.without.attrs", AttributeValue::string(resource_without_attrs.service_name))
  ]
  
  // 只添加有属性的Resource的属性
  let mut i = 0
  while i < single_attrs.length() {
    merged_mixed_attrs.push(single_attrs[i])
    i = i + 1
  }
  
  assert_eq(merged_mixed_attrs.length(), 3)
  
  match merged_mixed_attrs[2].1 {
    StringValue(value) => assert_eq(value, "only.value")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试特殊字符的服务名称
  let special_resource1 = Resource::default("special!@#$%^&*()")
  let special_resource2 = Resource::default("特殊字符🚀")
  
  let merged_special_attrs : Attributes = [
    ("special.service1", AttributeValue::string(special_resource1.service_name)),
    ("special.service2", AttributeValue::string(special_resource2.service_name))
  ]
  
  assert_eq(merged_special_attrs.length(), 2)
  
  match merged_special_attrs[0].1 {
    StringValue(name) => assert_eq(name, "special!@#$%^&*()")
    _ => @test.fail("Expected StringValue")
  }
  
  match merged_special_attrs[1].1 {
    StringValue(name) => assert_eq(name, "特殊字符🚀")
    _ => @test.fail("Expected StringValue")
  }
}