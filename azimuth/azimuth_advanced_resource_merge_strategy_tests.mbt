// 高级资源合并策略测试用例
// 测试复杂属性冲突解决机制和合并策略

test "资源合并优先级策略测试" {
  // 创建具有不同优先级的资源
  let high_priority_resource = azimuth::Resource::with_priority(azimuth::Resource::new(), 100)
  let medium_priority_resource = azimuth::Resource::with_priority(azimuth::Resource::new(), 50)
  let low_priority_resource = azimuth::Resource::with_priority(azimuth::Resource::new(), 10)
  
  // 设置高优先级资源属性
  let high_attrs = [
    ("service.name", azimuth::StringValue("high-priority-service")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("critical.setting", azimuth::StringValue("high-value")),
    ("shared.attr", azimuth::StringValue("from-high"))
  ]
  let high_resource_with_attrs = azimuth::Resource::with_attributes(high_priority_resource, high_attrs)
  
  // 设置中等优先级资源属性
  let medium_attrs = [
    ("service.name", azimuth::StringValue("medium-priority-service")),
    ("service.version", azimuth::StringValue("1.5.0")),
    ("deployment.environment", azimuth::StringValue("staging")),
    ("shared.attr", azimuth::StringValue("from-medium"))
  ]
  let medium_resource_with_attrs = azimuth::Resource::with_attributes(medium_priority_resource, medium_attrs)
  
  // 设置低优先级资源属性
  let low_attrs = [
    ("service.name", azimuth::StringValue("low-priority-service")),
    ("service.instance.id", azimuth::StringValue("instance-low-123")),
    ("shared.attr", azimuth::StringValue("from-low"))
  ]
  let low_resource_with_attrs = azimuth::Resource::with_attributes(low_priority_resource, low_attrs)
  
  // 测试优先级合并策略
  let priority_merge_strategy = azimuth::PriorityMergeStrategy::new()
  let merged_resource = azimuth::Resource::merge_with_strategy(
    [high_resource_with_attrs, medium_resource_with_attrs, low_resource_with_attrs],
    priority_merge_strategy
  )
  
  // 验证高优先级属性胜出
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "service.name"),
    Some(azimuth::StringValue("high-priority-service"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "deployment.environment"),
    Some(azimuth::StringValue("production"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "critical.setting"),
    Some(azimuth::StringValue("high-value"))
  )
  
  // 验证中等优先级的独有属性被保留
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "service.version"),
    Some(azimuth::StringValue("1.5.0"))
  )
  
  // 验证低优先级的独有属性被保留
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "service.instance.id"),
    Some(azimuth::StringValue("instance-low-123"))
  )
}

test "资源合并类型冲突解决测试" {
  // 创建具有不同类型值的资源
  let string_resource = azimuth::Resource::new()
  let string_attrs = [
    ("config.timeout", azimuth::StringValue("30")),
    ("config.retries", azimuth::StringValue("3")),
    ("config.enabled", azimuth::StringValue("true")),
    ("unique.string", azimuth::StringValue("string-only"))
  ]
  let string_resource_with_attrs = azimuth::Resource::with_attributes(string_resource, string_attrs)
  
  let int_resource = azimuth::Resource::new()
  let int_attrs = [
    ("config.timeout", azimuth::IntValue(30)),
    ("config.retries", azimuth::IntValue(3)),
    ("config.enabled", azimuth::IntValue(1)),
    ("unique.int", azimuth::IntValue(42))
  ]
  let int_resource_with_attrs = azimuth::Resource::with_attributes(int_resource, int_attrs)
  
  let float_resource = azimuth::Resource::new()
  let float_attrs = [
    ("config.timeout", azimuth::FloatValue(30.0)),
    ("config.retries", azimuth::FloatValue(3.0)),
    ("config.enabled", azimuth::FloatValue(1.0)),
    ("unique.float", azimuth::FloatValue(3.14))
  ]
  let float_resource_with_attrs = azimuth::Resource::with_attributes(float_resource, float_attrs)
  
  let bool_resource = azimuth::Resource::new()
  let bool_attrs = [
    ("config.enabled", azimuth::BoolValue(true)),
    ("config.debug", azimuth::BoolValue(false)),
    ("unique.bool", azimuth::BoolValue(true))
  ]
  let bool_resource_with_attrs = azimuth::Resource::with_attributes(bool_resource, bool_attrs)
  
  // 测试类型转换合并策略
  let type_conversion_strategy = azimuth::TypeConversionMergeStrategy::new([
    ("config.timeout", "int"),    // 优先转换为int
    ("config.retries", "int"),    // 优先转换为int
    ("config.enabled", "bool")    // 优先转换为bool
  ])
  
  let merged_resource = azimuth::Resource::merge_with_strategy(
    [string_resource_with_attrs, int_resource_with_attrs, float_resource_with_attrs, bool_resource_with_attrs],
    type_conversion_strategy
  )
  
  // 验证类型转换结果
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "config.timeout"),
    Some(azimuth::IntValue(30))
  )
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "config.retries"),
    Some(azimuth::IntValue(3))
  )
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "config.enabled"),
    Some(azimuth::BoolValue(true))
  )
  
  // 验证独有属性被保留
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "unique.string"),
    Some(azimuth::StringValue("string-only"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "unique.int"),
    Some(azimuth::IntValue(42))
  )
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "unique.float"),
    Some(azimuth::FloatValue(3.14))
  )
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "unique.bool"),
    Some(azimuth::BoolValue(true))
  )
}

test "资源合并条件策略测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::new()
  let base_attrs = [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("development")),
    ("region", azimuth::StringValue("us-east-1")),
    ("temp.setting", azimuth::StringValue("should-be-removed"))
  ]
  let base_resource_with_attrs = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // 创建覆盖资源
  let override_resource = azimuth::Resource::new()
  let override_attrs = [
    ("service.name", azimuth::StringValue("override-service")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("environment", azimuth::StringValue("production")),
    ("region", azimuth::StringValue("us-west-2"))
  ]
  let override_resource_with_attrs = azimuth::Resource::with_attributes(override_resource, override_attrs)
  
  // 创建条件合并策略
  let conditional_strategy = azimuth::ConditionalMergeStrategy::new([
    // 环境为production时，允许覆盖service.name
    ("service.name", azimuth::Condition::new("environment", "production")),
    // 版本号比较：只有新版本大于当前版本时才覆盖
    ("service.version", azimuth::VersionCondition::new()),
    // 临时属性总是被移除
    ("temp.setting", azimuth::RemoveCondition::new())
  ])
  
  let merged_resource = azimuth::Resource::merge_with_strategy(
    [base_resource_with_attrs, override_resource_with_attrs],
    conditional_strategy
  )
  
  // 验证条件合并结果
  // 由于environment是production，允许覆盖service.name
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "service.name"),
    Some(azimuth::StringValue("override-service"))
  )
  
  // 版本2.0.0 > 1.0.0，允许覆盖
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "service.version"),
    Some(azimuth::StringValue("2.0.0"))
  )
  
  // environment被覆盖为production
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "environment"),
    Some(azimuth::StringValue("production"))
  )
  
  // region被覆盖
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "region"),
    Some(azimuth::StringValue("us-west-2"))
  )
  
  // 临时属性应该被移除
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "temp.setting"),
    None
  )
}

test "资源合并数组属性处理测试" {
  // 创建包含数组属性的资源
  let resource1 = azimuth::Resource::new()
  let array_attrs1 = [
    ("service.tags", azimuth::ArrayValue([azimuth::StringValue("web"), azimuth::StringValue("api")])),
    ("dependencies", azimuth::ArrayValue([azimuth::StringValue("database"), azimuth::StringValue("cache")])),
    ("endpoints", azimuth::ArrayValue([azimuth::StringValue("http://localhost:8080")]))
  ]
  let resource1_with_attrs = azimuth::Resource::with_attributes(resource1, array_attrs1)
  
  let resource2 = azimuth::Resource::new()
  let array_attrs2 = [
    ("service.tags", azimuth::ArrayValue([azimuth::StringValue("backend"), azimuth::StringValue("microservice")])),
    ("dependencies", azimuth::ArrayValue([azimuth::StringValue("message-queue")])),
    ("endpoints", azimuth::ArrayValue([azimuth::StringValue("http://localhost:9090")]))
  ]
  let resource2_with_attrs = azimuth::Resource::with_attributes(resource2, array_attrs2)
  
  // 测试数组合并策略
  let array_merge_strategy = azimuth::ArrayMergeStrategy::new([
    ("service.tags", "union"),      // 合并去重
    ("dependencies", "concat"),     // 简单连接
    ("endpoints", "override")       // 覆盖
  ])
  
  let merged_resource = azimuth::Resource::merge_with_strategy(
    [resource1_with_attrs, resource2_with_attrs],
    array_merge_strategy
  )
  
  // 验证数组合并结果
  let tags = azimuth::Resource::get_attribute(merged_resource, "service.tags")
  assert_eq(tags, Some(azimuth::ArrayValue([
    azimuth::StringValue("web"),
    azimuth::StringValue("api"),
    azimuth::StringValue("backend"),
    azimuth::StringValue("microservice")
  ])))
  
  let dependencies = azimuth::Resource::get_attribute(merged_resource, "dependencies")
  assert_eq(dependencies, Some(azimuth::ArrayValue([
    azimuth::StringValue("database"),
    azimuth::StringValue("cache"),
    azimuth::StringValue("message-queue")
  ])))
  
  let endpoints = azimuth::Resource::get_attribute(merged_resource, "endpoints")
  assert_eq(endpoints, Some(azimuth::ArrayValue([
    azimuth::StringValue("http://localhost:9090")
  ])))
}

test "资源合并性能基准测试" {
  // 创建大量资源进行性能测试
  let resources = []
  for i = 0; i < 100; i++ {
    let resource = azimuth::Resource::new()
    let attrs = [
      ("service.name", azimuth::StringValue("service-" + i.to_string())),
      ("service.instance.id", azimuth::StringValue("instance-" + i.to_string())),
      ("service.version", azimuth::StringValue("1." + i.to_string() + ".0")),
      ("environment", azimuth::StringValue("test")),
      ("region", azimuth::StringValue("us-east-" + (i % 3 + 1).to_string())),
      ("metric.counter", azimuth::IntValue(i)),
      ("metric.ratio", azimuth::FloatValue(i * 0.1))
    ]
    let resource_with_attrs = azimuth::Resource::with_attributes(resource, attrs)
    resources.push(resource_with_attrs)
  }
  
  // 测试基本合并性能
  let start_time = azimuth::Clock::now()
  let basic_merged = azimuth::Resource::merge(resources[0], resources[1])
  let basic_duration = azimuth::Clock::now() - start_time
  
  // 测试策略合并性能
  let strategy = azimuth::PriorityMergeStrategy::new()
  let start_time = azimuth::Clock::now()
  let strategy_merged = azimuth::Resource::merge_with_strategy(resources, strategy)
  let strategy_duration = azimuth::Clock::now() - start_time
  
  // 性能断言
  assert_true(basic_duration < 1000)  // 基本合并应该在1ms内完成
  assert_true(strategy_duration < 10000)  // 策略合并应该在10ms内完成
  
  // 验证合并结果的有效性
  assert_eq(
    azimuth::Resource::get_attribute(strategy_merged, "service.name"),
    Some(azimuth::StringValue("service-0"))  // 第一个资源的service.name
  )
  assert_eq(
    azimuth::Resource::get_attribute(strategy_merged, "service.instance.id"),
    Some(azimuth::StringValue("instance-0"))
  )
}

test "资源合并冲突检测和报告测试" {
  // 创建具有冲突属性的资源
  let resource1 = azimuth::Resource::new()
  let attrs1 = [
    ("service.name", azimuth::StringValue("service-A")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("production")),
    ("conflict.attr", azimuth::StringValue("value-from-A"))
  ]
  let resource1_with_attrs = azimuth::Resource::with_attributes(resource1, attrs1)
  
  let resource2 = azimuth::Resource::new()
  let attrs2 = [
    ("service.name", azimuth::StringValue("service-B")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("environment", azimuth::StringValue("staging")),
    ("conflict.attr", azimuth::StringValue("value-from-B"))
  ]
  let resource2_with_attrs = azimuth::Resource::with_attributes(resource2, attrs2)
  
  // 创建冲突检测策略
  let conflict_detection_strategy = azimuth::ConflictDetectionStrategy::new()
  
  let merge_result = azimuth::Resource::merge_with_detection(
    [resource1_with_attrs, resource2_with_attrs],
    conflict_detection_strategy
  )
  
  // 验证合并结果
  let merged_resource = merge_result.merged_resource
  let conflicts = merge_result.conflicts
  
  // 验证冲突被检测到
  assert_true(conflicts.length() > 0)
  
  // 验证具体冲突
  let service_name_conflict = conflicts.find(fn(conflict) { conflict.attribute_key == "service.name" })
  assert_true(service_name_conflict != None)
  
  let version_conflict = conflicts.find(fn(conflict) { conflict.attribute_key == "service.version" })
  assert_true(version_conflict != None)
  
  let environment_conflict = conflicts.find(fn(conflict) { conflict.attribute_key == "environment" })
  assert_true(environment_conflict != None)
  
  // 验证冲突信息包含所有相关值
  let service_conflict = service_name_conflict.unwrap()
  assert_true(service_conflict.values.contains(azimuth::StringValue("service-A")))
  assert_true(service_conflict.values.contains(azimuth::StringValue("service-B")))
  
  // 验证合并结果使用了默认解决策略（后覆盖前）
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "service.name"),
    Some(azimuth::StringValue("service-B"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "service.version"),
    Some(azimuth::StringValue("2.0.0"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(merged_resource, "environment"),
    Some(azimuth::StringValue("staging"))
  )
}