// 高级资源合并策略测试用例
// 测试复杂的资源合并场景和边界情况

test "资源合并的深度优先策略" {
  // 创建具有嵌套属性的复杂资源
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.metadata.version", StringValue("1.0.0")),
    ("service.metadata.owner", StringValue("team-a")),
    ("service.config.endpoints", ArrayStringValue(["http://api1.example.com", "http://api2.example.com"])),
    ("service.config.retry.count", IntValue(3)),
    ("service.config.retry.delay", IntValue(1000)),
    ("environment", StringValue("production"))
  ]
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.metadata.owner", StringValue("team-b")),
    ("service.config.retry.count", IntValue(5)),
    ("service.config.timeout", IntValue(30000)),
    ("deployment.region", StringValue("us-west-2"))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // 使用深度优先合并策略
  let merged_resource = Resource::merge_deep(base_resource, override_resource)
  
  // 验证深度合并结果
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("override-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.metadata.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "service.metadata.owner"), Some(StringValue("team-b")))
  assert_eq(Resource::get_attribute(merged_resource, "service.config.retry.count"), Some(IntValue(5)))
  assert_eq(Resource::get_attribute(merged_resource, "service.config.retry.delay"), Some(IntValue(1000)))
  assert_eq(Resource::get_attribute(merged_resource, "service.config.timeout"), Some(IntValue(30000)))
  assert_eq(Resource::get_attribute(merged_resource, "environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.region"), Some(StringValue("us-west-2")))
}

test "资源合并的冲突解决策略" {
  // 测试不同类型的属性冲突解决
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.port", IntValue(8080)),
    ("service.enabled", BoolValue(true)),
    ("service.tags", ArrayStringValue(["web", "api"])),
    ("service.metrics", FloatValue(95.5))
  ]
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.port", StringValue("9090")), // 类型冲突：Int -> String
    ("service.enabled", StringValue("false")), // 类型冲突：Bool -> String
    ("service.tags", ArrayStringValue(["batch"])), // 数组内容不同
    ("service.metrics", IntValue(99)) // 类型冲突：Float -> Int
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // 使用类型安全的合并策略
  let merged_resource = Resource::merge_with_type_resolution(base_resource, override_resource)
  
  // 验证类型冲突解决
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("override-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.port"), Some(StringValue("9090")))
  assert_eq(Resource::get_attribute(merged_resource, "service.enabled"), Some(StringValue("false")))
  assert_eq(Resource::get_attribute(merged_resource, "service.tags"), Some(ArrayStringValue(["batch"])))
  assert_eq(Resource::get_attribute(merged_resource, "service.metrics"), Some(IntValue(99)))
}

test "资源合并的条件策略" {
  // 测试基于条件的合并策略
  let base_attrs = [
    ("service.name", StringValue("conditional-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.environment", StringValue("development")),
    ("service.debug.enabled", BoolValue(true)),
    ("service.performance.threshold", IntValue(100))
  ]
  
  let override_attrs = [
    ("service.name", StringValue("prod-service")),
    ("service.version", StringValue("2.0.0")),
    ("service.environment", StringValue("production")),
    ("service.debug.enabled", BoolValue(false)),
    ("service.performance.threshold", IntValue(500))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // 定义条件合并规则
  let merge_rules = [
    ("service.name", "override_if_prod"), // 如果环境是production，则覆盖
    ("service.version", "always_override"), // 总是覆盖
    ("service.debug.enabled", "keep_base"), // 保留基础值
    ("service.performance.threshold", "max") // 取最大值
  ]
  
  // 使用条件合并策略
  let merged_resource = Resource::merge_with_conditions(base_resource, override_resource, merge_rules)
  
  // 验证条件合并结果
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("prod-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "service.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(merged_resource, "service.debug.enabled"), Some(BoolValue(true)))
  assert_eq(Resource::get_attribute(merged_resource, "service.performance.threshold"), Some(IntValue(500)))
}

test "资源合并的循环引用检测" {
  // 测试循环引用检测和处理
  let resource_a = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-a")),
    ("service.depends_on", StringValue("service-b"))
  ])
  
  let resource_b = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-b")),
    ("service.depends_on", StringValue("service-c"))
  ])
  
  let resource_c = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-c")),
    ("service.depends_on", StringValue("service-a")) // 创建循环引用
  ])
  
  // 检测循环引用
  let cycle_detector = Resource::CycleDetector::new()
  let has_cycle = Resource::CycleDetector::detect(cycle_detector, [resource_a, resource_b, resource_c])
  
  assert_true(has_cycle, "Should detect circular dependency")
  
  // 使用安全的合并策略处理循环引用
  let merged_resource = Resource::merge_with_cycle_detection([resource_a, resource_b, resource_c])
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("service-c")))
  assert_eq(Resource::get_attribute(merged_resource, "service.depends_on"), Some(StringValue("service-a")))
}

test "资源合并的性能优化" {
  // 测试大规模资源合并的性能
  let base_resource = Resource::new()
  let override_resource = Resource::new()
  
  // 创建大量属性
  let base_attrs = []
  let override_attrs = []
  
  for i in 0..10000 {
    base_attrs.push(("base.attr." + i.to_string(), StringValue("base-value-" + i.to_string())))
    override_attrs.push(("override.attr." + i.to_string(), StringValue("override-value-" + i.to_string())))
  }
  
  let base_resource = Resource::with_attributes(base_resource, base_attrs)
  let override_resource = Resource::with_attributes(override_resource, override_attrs)
  
  // 测量合并性能
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  let merged_resource = Resource::merge_optimized(base_resource, override_resource)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let merge_duration = end_time - start_time
  
  // 验证性能要求
  assert_true(merge_duration < 1000000000L, "Merge should complete within 1 second")
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged_resource, "base.attr.0"), Some(StringValue("base-value-0")))
  assert_eq(Resource::get_attribute(merged_resource, "override.attr.0"), Some(StringValue("override-value-0")))
  assert_eq(Resource::get_attribute(merged_resource, "base.attr.9999"), Some(StringValue("base-value-9999")))
  assert_eq(Resource::get_attribute(merged_resource, "override.attr.9999"), Some(StringValue("override-value-9999")))
}

test "资源合并的元数据保留" {
  // 测试合并过程中元数据的保留
  let base_resource = Resource::with_metadata(Resource::new(), [
    ("source", StringValue("config-file")),
    ("timestamp", StringValue("2023-01-01T00:00:00Z")),
    ("version", StringValue("1.0.0"))
  ])
  
  let override_resource = Resource::with_metadata(Resource::new(), [
    ("source", StringValue("runtime-override")),
    ("priority", StringValue("high")),
    ("version", StringValue("2.0.0"))
  ])
  
  // 添加属性
  let base_resource = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("base-service"))
  ])
  
  let override_resource = Resource::with_attributes(override_resource, [
    ("service.name", StringValue("override-service"))
  ])
  
  // 使用保留元数据的合并策略
  let merged_resource = Resource::merge_with_metadata_preservation(base_resource, override_resource)
  
  // 验证属性合并
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("override-service")))
  
  // 验证元数据合并
  let metadata = Resource::get_metadata(merged_resource)
  assert_eq(Resource::get_attribute(metadata, "source"), Some(StringValue("runtime-override")))
  assert_eq(Resource::get_attribute(metadata, "priority"), Some(StringValue("high")))
  assert_eq(Resource::get_attribute(metadata, "version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(metadata, "timestamp"), Some(StringValue("2023-01-01T00:00:00Z")))
}

test "资源合并的回滚机制" {
  // 测试合并失败的回滚机制
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("override-service")),
    ("service.version", StringValue("2.0.0"))
  ])
  
  // 创建一个会在合并过程中失败的验证器
  let validator = Resource::Validator::new()
  Resource::Validator::add_rule(validator, "service.name", fn(value) {
    match value {
      StringValue(name) if name == "override-service" => false // 拒绝此值
      _ => true
    }
  })
  
  // 尝试合并（应该失败）
  let merge_result = Resource::merge_with_validation(base_resource, override_resource, validator)
  
  // 验证合并失败
  assert_true(merge_result.is_error, "Merge should fail validation")
  
  // 验证原始资源未受影响
  assert_eq(Resource::get_attribute(base_resource, "service.name"), Some(StringValue("base-service")))
  assert_eq(Resource::get_attribute(base_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(override_resource, "service.name"), Some(StringValue("override-service")))
  assert_eq(Resource::get_attribute(override_resource, "service.version"), Some(StringValue("2.0.0")))
  
  // 使用有效的值重试
  let valid_override_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("valid-service")),
    ("service.version", StringValue("2.0.0"))
  ])
  
  let valid_merge_result = Resource::merge_with_validation(base_resource, valid_override_resource, validator)
  
  // 验证合并成功
  assert_true(valid_merge_result.is_ok, "Merge should succeed with valid values")
  
  let merged_resource = valid_merge_result.unwrap
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("valid-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("2.0.0")))
}