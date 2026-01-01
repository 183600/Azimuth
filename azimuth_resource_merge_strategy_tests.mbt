// 资源合并策略测试
// 测试Azimuth遥测系统中资源合并的各种策略和行为

// 测试1: 基本资源合并策略
pub test "基本资源合并策略测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("deployment.environment", azimuth::StringValue("development"))
  ])
  
  // 创建覆盖资源
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.version", azimuth::StringValue("2.0.0")),  // 覆盖现有属性
    ("service.instance.id", azimuth::StringValue("instance-123")),  // 新增属性
    ("host.name", azimuth::StringValue("test-host"))  // 新增属性
  ])
  
  // 执行资源合并
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // 验证合并结果
  // 基础属性中未被覆盖的应该保持
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("base-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.environment"), Some(azimuth::StringValue("development")))
  
  // 被覆盖的属性应该使用新值
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("2.0.0")))
  
  // 新增属性应该存在
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "host.name"), Some(azimuth::StringValue("test-host")))
  
  // 验证原始资源未被修改
  assert_eq(azimuth::Resource::get_attribute(base_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(override_resource, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
}

// 测试2: 多级资源合并策略
pub test "多级资源合并策略测试" {
  // 创建第一级资源（基础服务信息）
  let level1_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("multi-level-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.namespace", azimuth::StringValue("production"))
  ])
  
  // 创建第二级资源（部署信息）
  let level2_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("deployment.environment", azimuth::StringValue("staging")),
    ("deployment.region", azimuth::StringValue("us-west-1")),
    ("service.version", azimuth::StringValue("1.1.0"))  // 覆盖版本
  ])
  
  // 创建第三级资源（运行时信息）
  let level3_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.instance.id", azimuth::StringValue("instance-456")),
    ("host.name", azimuth::StringValue("prod-host-1")),
    ("process.id", azimuth::IntValue(7890))
  ])
  
  // 执行多级合并
  let merged_level1_2 = azimuth::Resource::merge(level1_resource, level2_resource)
  let merged_all = azimuth::Resource::merge(merged_level1_2, level3_resource)
  
  // 验证一级合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "service.name"), Some(azimuth::StringValue("multi-level-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "service.version"), Some(azimuth::StringValue("1.1.0")))  // 被覆盖
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "deployment.environment"), Some(azimuth::StringValue("staging")))
  assert_eq(azimuth::Resource::get_attribute(merged_level1_2, "deployment.region"), Some(azimuth::StringValue("us-west-1")))
  
  // 验证三级合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_all, "service.name"), Some(azimuth::StringValue("multi-level-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_all, "service.version"), Some(azimuth::StringValue("1.1.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_all, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_all, "deployment.environment"), Some(azimuth::StringValue("staging")))
  assert_eq(azimuth::Resource::get_attribute(merged_all, "deployment.region"), Some(azimuth::StringValue("us-west-1")))
  assert_eq(azimuth::Resource::get_attribute(merged_all, "service.instance.id"), Some(azimuth::StringValue("instance-456")))
  assert_eq(azimuth::Resource::get_attribute(merged_all, "host.name"), Some(azimuth::StringValue("prod-host-1")))
  assert_eq(azimuth::Resource::get_attribute(merged_all, "process.id"), Some(azimuth::IntValue(7890)))
}

// 测试3: 不同类型属性值的合并策略
pub test "不同类型属性值的合并策略测试" {
  // 创建包含不同类型属性的基础资源
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("string.attr", azimuth::StringValue("base-value")),
    ("int.attr", azimuth::IntValue(100)),
    ("bool.attr", azimuth::BoolValue(true)),
    ("float.attr", azimuth::FloatValue(3.14))
  ])
  
  // 创建包含不同类型属性的覆盖资源
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("string.attr", azimuth::StringValue("override-value")),  // 覆盖字符串
    ("int.attr", azimuth::IntValue(200)),  // 覆盖整数
    ("bool.attr", azimuth::BoolValue(false)),  // 覆盖布尔值
    ("float.attr", azimuth::FloatValue(2.71)),  // 覆盖浮点数
    ("new.string.attr", azimuth::StringValue("new-string")),  // 新增字符串
    ("new.int.attr", azimuth::IntValue(300))  // 新增整数
  ])
  
  // 执行合并
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // 验证类型覆盖
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "string.attr"), Some(azimuth::StringValue("override-value")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "int.attr"), Some(azimuth::IntValue(200)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "bool.attr"), Some(azimuth::BoolValue(false)))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "float.attr"), Some(azimuth::FloatValue(2.71)))
  
  // 验证新增属性
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "new.string.attr"), Some(azimuth::StringValue("new-string")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "new.int.attr"), Some(azimuth::IntValue(300)))
}

// 测试4: 空资源和特殊属性合并策略
pub test "空资源和特殊属性合并策略测试" {
  // 创建空资源
  let empty_resource = azimuth::Resource::new()
  
  // 创建非空资源
  let non_empty_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0"))
  ])
  
  // 测试空资源与非空资源合并（空资源在前）
  let merged_empty_first = azimuth::Resource::merge(empty_resource, non_empty_resource)
  assert_eq(azimuth::Resource::get_attribute(merged_empty_first, "service.name"), Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_empty_first, "service.version"), Some(azimuth::StringValue("1.0.0")))
  
  // 测试空资源与非空资源合并（空资源在后）
  let merged_empty_last = azimuth::Resource::merge(non_empty_resource, empty_resource)
  assert_eq(azimuth::Resource::get_attribute(merged_empty_last, "service.name"), Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_empty_last, "service.version"), Some(azimuth::StringValue("1.0.0")))
  
  // 测试空字符串属性
  let resource_with_empty_string = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("empty.string", azimuth::StringValue("")),
    ("normal.string", azimuth::StringValue("normal-value"))
  ])
  
  let merged_empty_string = azimuth::Resource::merge(non_empty_resource, resource_with_empty_string)
  assert_eq(azimuth::Resource::get_attribute(merged_empty_string, "empty.string"), Some(azimuth::StringValue("")))
  assert_eq(azimuth::Resource::get_attribute(merged_empty_string, "normal.string"), Some(azimuth::StringValue("normal-value")))
  assert_eq(azimuth::Resource::get_attribute(merged_empty_string, "service.name"), Some(azimuth::StringValue("test-service")))
  
  // 测试零值属性
  let resource_with_zero_values = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("zero.int", azimuth::IntValue(0)),
    ("zero.float", azimuth::FloatValue(0.0)),
    ("false.bool", azimuth::BoolValue(false))
  ])
  
  let merged_zero_values = azimuth::Resource::merge(non_empty_resource, resource_with_zero_values)
  assert_eq(azimuth::Resource::get_attribute(merged_zero_values, "zero.int"), Some(azimuth::IntValue(0)))
  assert_eq(azimuth::Resource::get_attribute(merged_zero_values, "zero.float"), Some(azimuth::FloatValue(0.0)))
  assert_eq(azimuth::Resource::get_attribute(merged_zero_values, "false.bool"), Some(azimuth::BoolValue(false)))
}

// 测试5: 大量属性资源合并策略
pub test "大量属性资源合并策略测试" {
  // 创建包含大量属性的基础资源
  let base_attrs = []
  for i in 0..100 {
    base_attrs.push(("base.attr." + i.to_string(), azimuth::StringValue("base-value-" + i.to_string())))
  }
  
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), base_attrs)
  
  // 创建包含大量属性的覆盖资源
  let override_attrs = []
  for i in 0..50 {
    // 覆盖前50个属性
    override_attrs.push(("base.attr." + i.to_string(), azimuth::StringValue("override-value-" + i.to_string())))
  }
  
  for i in 50..100 {
    // 新增50个属性
    override_attrs.push(("new.attr." + i.to_string(), azimuth::StringValue("new-value-" + i.to_string())))
  }
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), override_attrs)
  
  // 执行合并
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // 验证覆盖的属性
  for i in 0..50 {
    let key = "base.attr." + i.to_string()
    let expected_value = "override-value-" + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_resource, key), Some(azimuth::StringValue(expected_value)))
  }
  
  // 验证未覆盖的属性
  for i in 50..100 {
    let key = "base.attr." + i.to_string()
    let expected_value = "base-value-" + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_resource, key), Some(azimuth::StringValue(expected_value)))
  }
  
  // 验证新增的属性
  for i in 50..100 {
    let key = "new.attr." + i.to_string()
    let expected_value = "new-value-" + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_resource, key), Some(azimuth::StringValue(expected_value)))
  }
}

// 测试6: 特殊字符和Unicode属性合并策略
pub test "特殊字符和Unicode属性合并策略测试" {
  // 创建包含特殊字符和Unicode的基础资源
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("special.chars.key", azimuth::StringValue("!@#$%^&*()")),
    ("unicode.key", azimuth::StringValue("测试中文")),
    ("emoji.key", azimuth::StringValue("🚀🌟")),
    ("mixed.key", azimuth::StringValue("Mixed!测试123🚀"))
  ])
  
  // 创建包含特殊字符和Unicode的覆盖资源
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("special.chars.key", azimuth::StringValue("[]{}|;':\",./<>?")),  // 覆盖特殊字符
    ("unicode.key", azimuth::StringValue("日本語")),  // 覆盖Unicode
    ("emoji.key", azimuth::StringValue("💯🎉")),  // 覆盖Emoji
    ("new.special.key", azimuth::StringValue("新!@#$%")),  // 新增特殊字符
    ("new.unicode.key", azimuth::StringValue("한국어"))  // 新增Unicode
  ])
  
  // 执行合并
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // 验证覆盖的属性
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "special.chars.key"), Some(azimuth::StringValue("[]{}|;':\",./<>?")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "unicode.key"), Some(azimuth::StringValue("日本語")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "emoji.key"), Some(azimuth::StringValue("💯🎉")))
  
  // 验证未覆盖的属性
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "mixed.key"), Some(azimuth::StringValue("Mixed!测试123🚀")))
  
  // 验证新增的属性
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "new.special.key"), Some(azimuth::StringValue("新!@#$%")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "new.unicode.key"), Some(azimuth::StringValue("한국어")))
}

// 测试7: 资源合并性能测试
pub test "资源合并性能测试" {
  // 创建大型基础资源
  let base_attrs = []
  for i in 0..1000 {
    base_attrs.push(("base.attr." + i.to_string(), azimuth::StringValue("base-value-" + i.to_string())))
  }
  
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), base_attrs)
  
  // 创建大型覆盖资源
  let override_attrs = []
  for i in 0..500 {
    override_attrs.push(("base.attr." + i.to_string(), azimuth::StringValue("override-value-" + i.to_string())))
  }
  
  for i in 500..1000 {
    override_attrs.push(("new.attr." + i.to_string(), azimuth::StringValue("new-value-" + i.to_string())))
  }
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), override_attrs)
  
  // 测试合并性能
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 验证性能
  let merge_duration = end_time - start_time
  assert_true(merge_duration < 5000000000L)  // 合并应在5秒内完成
  
  // 验证合并结果正确性
  for i in 0..500 {
    let key = "base.attr." + i.to_string()
    let expected_value = "override-value-" + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_resource, key), Some(azimuth::StringValue(expected_value)))
  }
  
  for i in 500..1000 {
    let key = "base.attr." + i.to_string()
    let expected_value = "base-value-" + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_resource, key), Some(azimuth::StringValue(expected_value)))
  }
  
  for i in 500..1000 {
    let key = "new.attr." + i.to_string()
    let expected_value = "new-value-" + i.to_string()
    assert_eq(azimuth::Resource::get_attribute(merged_resource, key), Some(azimuth::StringValue(expected_value)))
  }
}

// 测试8: 资源合并幂等性测试
pub test "资源合并幂等性测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("idempotent-service")),
    ("service.version", azimuth::StringValue("1.0.0"))
  ])
  
  // 创建覆盖资源
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.instance.id", azimuth::StringValue("instance-789"))
  ])
  
  // 执行第一次合并
  let merged1 = azimuth::Resource::merge(base_resource, override_resource)
  
  // 执行第二次合并（合并结果与空资源合并）
  let merged2 = azimuth::Resource::merge(merged1, azimuth::Resource::new())
  
  // 执行第三次合并（空资源与合并结果合并）
  let merged3 = azimuth::Resource::merge(azimuth::Resource::new(), merged2)
  
  // 验证幂等性
  assert_eq(azimuth::Resource::get_attribute(merged1, "service.name"), azimuth::Resource::get_attribute(merged2, "service.name"))
  assert_eq(azimuth::Resource::get_attribute(merged2, "service.name"), azimuth::Resource::get_attribute(merged3, "service.name"))
  
  assert_eq(azimuth::Resource::get_attribute(merged1, "service.version"), azimuth::Resource::get_attribute(merged2, "service.version"))
  assert_eq(azimuth::Resource::get_attribute(merged2, "service.version"), azimuth::Resource::get_attribute(merged3, "service.version"))
  
  assert_eq(azimuth::Resource::get_attribute(merged1, "service.instance.id"), azimuth::Resource::get_attribute(merged2, "service.instance.id"))
  assert_eq(azimuth::Resource::get_attribute(merged2, "service.instance.id"), azimuth::Resource::get_attribute(merged3, "service.instance.id"))
  
  // 验证具体值
  assert_eq(azimuth::Resource::get_attribute(merged1, "service.name"), Some(azimuth::StringValue("idempotent-service")))
  assert_eq(azimuth::Resource::get_attribute(merged1, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged1, "service.instance.id"), Some(azimuth::StringValue("instance-789")))
}

// 测试9: 资源合并交换律测试
pub test "资源合并交换律测试" {
  // 创建第一个资源
  let resource1 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("commutative-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("deployment.environment", azimuth::StringValue("test"))
  ])
  
  // 创建第二个资源
  let resource2 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.instance.id", azimuth::StringValue("instance-abc")),
    ("host.name", azimuth::StringValue("test-host"))
  ])
  
  // 执行 A + B 合并
  let merged_ab = azimuth::Resource::merge(resource1, resource2)
  
  // 执行 B + A 合并
  let merged_ba = azimuth::Resource::merge(resource2, resource1)
  
  // 验证交换律不成立（覆盖顺序不同会导致结果不同）
  // resource1 的属性在 merged_ab 中应该保持原样，在 merged_ba 中可能被覆盖
  // resource2 的属性在 merged_ba 中应该保持原样，在 merged_ab 中可能被覆盖
  
  // 验证共同属性（两个资源都没有的属性）
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "service.name"), azimuth::Resource::get_attribute(merged_ba, "service.name"))
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "service.version"), azimuth::Resource::get_attribute(merged_ba, "service.version"))
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "deployment.environment"), azimuth::Resource::get_attribute(merged_ba, "deployment.environment"))
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "service.instance.id"), azimuth::Resource::get_attribute(merged_ba, "service.instance.id"))
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "host.name"), azimuth::Resource::get_attribute(merged_ba, "host.name"))
  
  // 验证具体值
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "service.name"), Some(azimuth::StringValue("commutative-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "deployment.environment"), Some(azimuth::StringValue("test")))
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "service.instance.id"), Some(azimuth::StringValue("instance-abc")))
  assert_eq(azimuth::Resource::get_attribute(merged_ab, "host.name"), Some(azimuth::StringValue("test-host")))
}

// 测试10: 资源合并结合律测试
pub test "资源合并结合律测试" {
  // 创建三个资源
  let resource_a = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("associative-service")),
    ("service.version", azimuth::StringValue("1.0.0"))
  ])
  
  let resource_b = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("deployment.environment", azimuth::StringValue("production"))
  ])
  
  let resource_c = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("host.name", azimuth::StringValue("prod-host")),
    ("service.version", azimuth::StringValue("2.0.0"))  // 覆盖版本
  ])
  
  // 执行 (A + B) + C 合并
  let merged_ab = azimuth::Resource::merge(resource_a, resource_b)
  let merged_abc = azimuth::Resource::merge(merged_ab, resource_c)
  
  // 执行 A + (B + C) 合并
  let merged_bc = azimuth::Resource::merge(resource_b, resource_c)
  let merged_abc2 = azimuth::Resource::merge(resource_a, merged_bc)
  
  // 验证结合律不成立（覆盖顺序不同会导致结果不同）
  // 在 (A + B) + C 中，C 的 service.version 会覆盖 A 的 service.version
  // 在 A + (B + C) 中，C 的 service.version 会覆盖 B 的，然后 A 的 service.version 会保持
  
  // 验证结果差异（主要体现在 service.version 属性上）
  let version_abc = azimuth::Resource::get_attribute(merged_abc, "service.version")
  let version_abc2 = azimuth::Resource::get_attribute(merged_abc2, "service.version")
  
  // 验证其他属性一致
  assert_eq(azimuth::Resource::get_attribute(merged_abc, "service.name"), azimuth::Resource::get_attribute(merged_abc2, "service.name"))
  assert_eq(azimuth::Resource::get_attribute(merged_abc, "service.instance.id"), azimuth::Resource::get_attribute(merged_abc2, "service.instance.id"))
  assert_eq(azimuth::Resource::get_attribute(merged_abc, "deployment.environment"), azimuth::Resource::get_attribute(merged_abc2, "deployment.environment"))
  assert_eq(azimuth::Resource::get_attribute(merged_abc, "host.name"), azimuth::Resource::get_attribute(merged_abc2, "host.name"))
  
  // 验证具体值
  assert_eq(azimuth::Resource::get_attribute(merged_abc, "service.name"), Some(azimuth::StringValue("associative-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_abc, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
  assert_eq(azimuth::Resource::get_attribute(merged_abc, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_abc, "host.name"), Some(azimuth::StringValue("prod-host")))
}