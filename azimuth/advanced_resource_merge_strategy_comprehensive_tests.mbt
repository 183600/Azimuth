// Azimuth Advanced Resource Merge Strategy Comprehensive Tests - 高级资源合并策略综合测试
// 测试资源合并、优先级处理、冲突解决和动态资源配置管理

test "基础资源合并策略测试" {
  // 测试基础资源合并功能
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("base-instance-123")),
    ("environment", StringValue("development")),
    ("host.name", StringValue("base-host"))
  ])
  
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("2.0.0")),
    ("service.instance.id", StringValue("override-instance-456")),
    ("environment", StringValue("production")),
    ("deployment.region", StringValue("us-west-2")),
    ("host.name", StringValue("override-host"))
  ])
  
  // 执行资源合并
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // 验证合并结果（基于简化实现，override_resource覆盖base_resource）
  match Resource::get_attribute(merged_resource, "service.version") {
    Some(StringValue("2.0.0")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "service.instance.id") {
    Some(StringValue("override-instance-456")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "environment") {
    Some(StringValue("production")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "deployment.region") {
    Some(StringValue("us-west-2")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "host.name") {
    Some(StringValue("override-host")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试空资源合并
  let empty_resource = Resource::new()
  let non_empty_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("non-empty-service")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  let merged_with_empty = Resource::merge(empty_resource, non_empty_resource)
  
  match Resource::get_attribute(merged_with_empty, "service.name") {
    Some(StringValue("non-empty-service")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_with_empty, "service.version") {
    Some(StringValue("1.0.0")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试相同资源合并
  let same_resource_merged = Resource::merge(non_empty_resource, non_empty_resource)
  
  match Resource::get_attribute(same_resource_merged, "service.name") {
    Some(StringValue("non-empty-service")) => assert_true(true)
    _ => assert_true(false)
  }
}

test "多级资源合并链测试" {
  // 测试多级资源合并链
  let level1_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("level1-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("level1-env")),
    ("level1.attr", StringValue("level1-value"))
  ])
  
  let level2_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("2.0.0")),
    ("environment", StringValue("level2-env")),
    ("level2.attr", StringValue("level2-value")),
    ("level2.only", StringValue("level2-only-value"))
  ])
  
  let level3_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("3.0.0")),
    ("level3.attr", StringValue("level3-value")),
    ("level3.only", StringValue("level3-only-value"))
  ])
  
  // 执行多级合并
  let level1_2_merged = Resource::merge(level1_resource, level2_resource)
  let final_merged = Resource::merge(level1_2_merged, level3_resource)
  
  // 验证最终合并结果
  match Resource::get_attribute(final_merged, "service.name") {
    Some(StringValue("level1-service")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_merged, "service.version") {
    Some(StringValue("3.0.0")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_merged, "environment") {
    Some(StringValue("level2-env")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_merged, "level1.attr") {
    Some(StringValue("level1-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_merged, "level2.attr") {
    Some(StringValue("level2-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_merged, "level3.attr") {
    Some(StringValue("level3-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_merged, "level2.only") {
    Some(StringValue("level2-only-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_merged, "level3.only") {
    Some(StringValue("level3-only-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试更复杂的合并链
  let chain_resources = []
  for i in 0..=4 {
    let resource = Resource::with_attributes(Resource::new(), [
      ("chain.level", StringValue("level-" + i.to_string())),
      ("chain.value", StringValue("value-" + i.to_string())),
      ("unique.attr." + i.to_string(), StringValue("unique-" + i.to_string()))
    ])
    chain_resources.push(resource)
  }
  
  // 执行链式合并
  let chain_result = Resource::merge(chain_resources[0], chain_resources[1])
  chain_result = Resource::merge(chain_result, chain_resources[2])
  chain_result = Resource::merge(chain_result, chain_resources[3])
  chain_result = Resource::merge(chain_result, chain_resources[4])
  
  // 验证链式合并结果
  match Resource::get_attribute(chain_result, "chain.level") {
    Some(StringValue("level-4")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(chain_result, "chain.value") {
    Some(StringValue("value-4")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(chain_result, "unique.attr.4") {
    Some(StringValue("unique-4")) => assert_true(true)
    _ => assert_true(false)
  }
}

test "资源合并优先级和冲突解决测试" {
  // 测试资源合并的优先级处理
  let high_priority_resource = Resource::with_attributes(Resource::new(), [
    ("priority.level", StringValue("high")),
    ("service.name", StringValue("high-priority-service")),
    ("critical.attr", StringValue("high-critical")),
    ("high.only", StringValue("high-only-value"))
  ])
  
  let medium_priority_resource = Resource::with_attributes(Resource::new(), [
    ("priority.level", StringValue("medium")),
    ("service.name", StringValue("medium-priority-service")),
    ("critical.attr", StringValue("medium-critical")),
    ("medium.only", StringValue("medium-only-value"))
  ])
  
  let low_priority_resource = Resource::with_attributes(Resource::new(), [
    ("priority.level", StringValue("low")),
    ("service.name", StringValue("low-priority-service")),
    ("critical.attr", StringValue("low-critical")),
    ("low.only", StringValue("low-only-value"))
  ])
  
  // 按优先级顺序合并
  let medium_low_merged = Resource::merge(low_priority_resource, medium_priority_resource)
  let final_priority_merged = Resource::merge(medium_low_merged, high_priority_resource)
  
  // 验证高优先级资源获胜
  match Resource::get_attribute(final_priority_merged, "priority.level") {
    Some(StringValue("high")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_priority_merged, "service.name") {
    Some(StringValue("high-priority-service")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_priority_merged, "critical.attr") {
    Some(StringValue("high-critical")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(final_priority_merged, "high.only") {
    Some(StringValue("high-only-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试冲突解决策略
  let conflict_resource_a = Resource::with_attributes(Resource::new(), [
    ("conflict.test", StringValue("value-a")),
    ("shared.attr", StringValue("shared-a")),
    ("unique.a", StringValue("unique-a-value"))
  ])
  
  let conflict_resource_b = Resource::with_attributes(Resource::new(), [
    ("conflict.test", StringValue("value-b")),
    ("shared.attr", StringValue("shared-b")),
    ("unique.b", StringValue("unique-b-value"))
  ])
  
  // 测试A覆盖B的合并
  let a_over_b = Resource::merge(conflict_resource_b, conflict_resource_a)
  
  match Resource::get_attribute(a_over_b, "conflict.test") {
    Some(StringValue("value-a")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(a_over_b, "shared.attr") {
    Some(StringValue("shared-a")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试B覆盖A的合并
  let b_over_a = Resource::merge(conflict_resource_a, conflict_resource_b)
  
  match Resource::get_attribute(b_over_a, "conflict.test") {
    Some(StringValue("value-b")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(b_over_a, "shared.attr") {
    Some(StringValue("shared-b")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试部分冲突解决
  let partial_conflict_a = Resource::with_attributes(Resource::new(), [
    ("no.conflict", StringValue("safe-value")),
    ("partial.conflict", StringValue("partial-a")),
    ("full.conflict", StringValue("full-a"))
  ])
  
  let partial_conflict_b = Resource::with_attributes(Resource::new(), [
    ("no.conflict", StringValue("safe-value")),
    ("partial.conflict", StringValue("partial-b")),
    ("full.conflict", StringValue("full-b"))
  ])
  
  let partial_merged = Resource::merge(partial_conflict_a, partial_conflict_b)
  
  // 测试相同值不冲突的情况
  match Resource::get_attribute(partial_merged, "no.conflict") {
    Some(StringValue("safe-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试不同值冲突的解决
  match Resource::get_attribute(partial_merged, "partial.conflict") {
    Some(StringValue("partial-b")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(partial_merged, "full.conflict") {
    Some(StringValue("full-b")) => assert_true(true)
    _ => assert_true(false)
  }
}

test "动态资源配置和更新测试" {
  // 测试动态资源配置
  let initial_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("dynamic-service")),
    ("service.version", StringValue("1.0.0")),
    ("config.source", StringValue("static")),
    ("last.update", StringValue("2025-01-01T00:00:00Z"))
  ])
  
  // 模拟动态更新1
  let update1_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("1.1.0")),
    ("config.source", StringValue("dynamic")),
    ("last.update", StringValue("2025-06-15T12:00:00Z")),
    ("update.reason", StringValue("bug-fix"))
  ])
  
  let updated1 = Resource::merge(initial_resource, update1_resource)
  
  // 验证第一次更新
  match Resource::get_attribute(updated1, "service.name") {
    Some(StringValue("dynamic-service")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(updated1, "service.version") {
    Some(StringValue("1.1.0")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(updated1, "config.source") {
    Some(StringValue("dynamic")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(updated1, "last.update") {
    Some(StringValue("2025-06-15T12:00:00Z")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(updated1, "update.reason") {
    Some(StringValue("bug-fix")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 模拟动态更新2
  let update2_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("2.0.0")),
    ("last.update", StringValue("2025-12-28T00:00:00Z")),
    ("update.reason", StringValue("major-release")),
    ("breaking.change", StringValue("true"))
  ])
  
  let updated2 = Resource::merge(updated1, update2_resource)
  
  // 验证第二次更新
  match Resource::get_attribute(updated2, "service.version") {
    Some(StringValue("2.0.0")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(updated2, "last.update") {
    Some(StringValue("2025-12-28T00:00:00Z")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(updated2, "update.reason") {
    Some(StringValue("major-release")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(updated2, "breaking.change") {
    Some(StringValue("true")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试批量动态更新
  let batch_updates = [
    Resource::with_attributes(Resource::new(), [
      ("batch.attr.1", StringValue("batch-value-1")),
      ("batch.common", StringValue("common-1"))
    ]),
    Resource::with_attributes(Resource::new(), [
      ("batch.attr.2", StringValue("batch-value-2")),
      ("batch.common", StringValue("common-2"))
    ]),
    Resource::with_attributes(Resource::new(), [
      ("batch.attr.3", StringValue("batch-value-3")),
      ("batch.common", StringValue("common-3"))
    ])
  ]
  
  // 执行批量更新
  let batch_result = Resource::merge(initial_resource, batch_updates[0])
  batch_result = Resource::merge(batch_result, batch_updates[1])
  batch_result = Resource::merge(batch_result, batch_updates[2])
  
  // 验证批量更新结果
  match Resource::get_attribute(batch_result, "batch.attr.1") {
    Some(StringValue("batch-value-1")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(batch_result, "batch.attr.2") {
    Some(StringValue("batch-value-2")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(batch_result, "batch.attr.3") {
    Some(StringValue("batch-value-3")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(batch_result, "batch.common") {
    Some(StringValue("common-3")) => assert_true(true)
    _ => assert_true(false)
  }
}

test "资源合并性能和内存优化测试" {
  // 测试资源合并的性能
  let large_base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("large-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("test"))
  ])
  
  // 添加大量属性
  let base_with_attrs = Resource::with_attributes(large_base_resource, [
    ("large.attr.1", StringValue("value-1")),
    ("large.attr.2", StringValue("value-2")),
    ("large.attr.3", StringValue("value-3")),
    ("large.attr.4", StringValue("value-4")),
    ("large.attr.5", StringValue("value-5"))
  ])
  
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 执行大量合并操作
  let merged_resources = []
  for i in 0..=49 {
    let override_resource = Resource::with_attributes(Resource::new(), [
      ("override.attr." + i.to_string(), StringValue("override-value-" + i.to_string())),
      ("service.version", StringValue("1." + i.to_string() + ".0"))
    ])
    
    let merged = Resource::merge(base_with_attrs, override_resource)
    merged_resources.push(merged)
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let merge_duration = end_time - start_time
  
  // 验证性能
  assert_true(merge_duration < 10000000000L)  // 小于10秒
  assert_true(merged_resources.length() == 50)
  
  // 验证合并结果
  for i in 0..=merged_resources.length() - 1 {
    let merged = merged_resources[i]
    
    match Resource::get_attribute(merged, "service.name") {
      Some(StringValue("large-service")) => assert_true(true)
      _ => assert_true(false)
    }
    
    match Resource::get_attribute(merged, "service.version") {
      Some(StringValue("1." + i.to_string() + ".0")) => assert_true(true)
      _ => assert_true(false)
    }
    
    match Resource::get_attribute(merged, "override.attr." + i.to_string()) {
      Some(StringValue("override-value-" + i.to_string())) => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // 测试内存优化的资源合并
  let memory_start = Clock::now_unix_nanos(Clock::system())
  
  let memory_optimized_resources = []
  for i in 0..=19 {
    let resource_a = Resource::with_attributes(Resource::new(), [
      ("memory.test." + i.to_string(), StringValue("memory-value-" + i.to_string())),
      ("common.attr", StringValue("common-" + i.to_string()))
    ])
    
    let resource_b = Resource::with_attributes(Resource::new(), [
      ("memory.override." + i.to_string(), StringValue("override-" + i.to_string())),
      ("common.attr", StringValue("overridden-" + i.to_string()))
    ])
    
    let memory_merged = Resource::merge(resource_a, resource_b)
    memory_optimized_resources.push(memory_merged)
  }
  
  let memory_end = Clock::now_unix_nanos(Clock::system())
  let memory_duration = memory_end - memory_start
  
  // 验证内存优化性能
  assert_true(memory_duration < 5000000000L)  // 小于5秒
  assert_true(memory_optimized_resources.length() == 20)
  
  // 验证内存优化结果
  for i in 0..=memory_optimized_resources.length() - 1 {
    let optimized = memory_optimized_resources[i]
    
    match Resource::get_attribute(optimized, "memory.test." + i.to_string()) {
      Some(StringValue("memory-value-" + i.to_string())) => assert_true(true)
      _ => assert_true(false)
    }
    
    match Resource::get_attribute(optimized, "memory.override." + i.to_string()) {
      Some(StringValue("override-" + i.to_string())) => assert_true(true)
      _ => assert_true(false)
    }
    
    match Resource::get_attribute(optimized, "common.attr") {
      Some(StringValue("overridden-" + i.to_string())) => assert_true(true)
      _ => assert_true(false)
    }
  }
}

test "资源合并边界条件和错误处理测试" {
  // 测试资源合并的边界条件
  let minimal_resource_a = Resource::with_attributes(Resource::new(), [
    ("single.attr", StringValue("single-value"))
  ])
  
  let minimal_resource_b = Resource::with_attributes(Resource::new(), [
    ("another.attr", StringValue("another-value"))
  ])
  
  // 测试最小资源合并
  let minimal_merged = Resource::merge(minimal_resource_a, minimal_resource_b)
  
  match Resource::get_attribute(minimal_merged, "single.attr") {
    Some(StringValue("single-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(minimal_merged, "another.attr") {
    Some(StringValue("another-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试空字符串属性
  let empty_string_resource = Resource::with_attributes(Resource::new(), [
    ("empty.string", StringValue("")),
    ("normal.string", StringValue("normal-value"))
  ])
  
  let empty_merged = Resource::merge(minimal_resource_a, empty_string_resource)
  
  match Resource::get_attribute(empty_merged, "empty.string") {
    Some(StringValue("")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(empty_merged, "normal.string") {
    Some(StringValue("normal-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试特殊字符属性
  let special_chars_resource = Resource::with_attributes(Resource::new(), [
    ("special.chars", StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")),
    ("unicode.chars", StringValue("测试中文🚀emoji")),
    ("newline.chars", StringValue("line1\nline2\rline3"))
  ])
  
  let special_merged = Resource::merge(minimal_resource_a, special_chars_resource)
  
  match Resource::get_attribute(special_merged, "special.chars") {
    Some(StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(special_merged, "unicode.chars") {
    Some(StringValue("测试中文🚀emoji")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(special_merged, "newline.chars") {
    Some(StringValue("line1\nline2\rline3")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试极长属性名和值
  let very_long_key = "this.is.a.very.long.attribute.name.that.tests.the.boundary.conditions.of.the.resource.merge.system"
  let very_long_value = "this.is.a.very.long.attribute.value.that.tests.the.boundary.conditions.of.the.resource.merge.system".repeat(10)
  
  let long_resource = Resource::with_attributes(Resource::new(), [
    (very_long_key, StringValue(very_long_value))
  ])
  
  let long_merged = Resource::merge(minimal_resource_a, long_resource)
  
  match Resource::get_attribute(long_merged, very_long_key) {
    Some(StringValue(value)) => assert_eq(value, very_long_value)
    _ => assert_true(false)
  }
  
  // 测试相同资源重复合并
  let same_resource = Resource::with_attributes(Resource::new(), [
    ("same.attr", StringValue("same-value")),
    ("another.same", StringValue("another-same-value"))
  ])
  
  let same_merged_once = Resource::merge(same_resource, same_resource)
  let same_merged_twice = Resource::merge(same_merged_once, same_resource)
  
  match Resource::get_attribute(same_merged_twice, "same.attr") {
    Some(StringValue("same-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(same_merged_twice, "another.same") {
    Some(StringValue("another-same-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试循环合并（理论上不应该发生，但测试健壮性）
  let resource_x = Resource::with_attributes(Resource::new(), [
    ("x.attr", StringValue("x-value"))
  ])
  
  let resource_y = Resource::with_attributes(Resource::new(), [
    ("y.attr", StringValue("y-value"))
  ])
  
  let xy_merged = Resource::merge(resource_x, resource_y)
  let yx_merged = Resource::merge(resource_y, resource_x)
  let xy_yx_merged = Resource::merge(xy_merged, yx_merged)
  
  match Resource::get_attribute(xy_yx_merged, "x.attr") {
    Some(StringValue("x-value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(xy_yx_merged, "y.attr") {
    Some(StringValue("y-value")) => assert_true(true)
    _ => assert_true(false)
  }
}