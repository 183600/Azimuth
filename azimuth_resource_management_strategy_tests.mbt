// Azimuth 资源管理策略测试用例
// 专注于遥测系统的资源分配、合并和管理策略

// 测试1: 资源合并策略
test "资源合并策略测试" {
  // 创建基础资源
  let base_resource = Resource::new()
  
  // 添加服务层资源属性
  let service_attrs = [
    ("service.name", StringValue("user.service")),
    ("service.version", StringValue("2.1.0")),
    ("service.namespace", StringValue("production"))
  ]
  let service_resource = Resource::with_attributes(base_resource, service_attrs)
  
  // 添加部署层资源属性
  let deployment_attrs = [
    ("deployment.environment", StringValue("staging")),
    ("deployment.region", StringValue("us-west-2")),
    ("deployment.zone", StringValue("us-west-2a"))
  ]
  let deployment_resource = Resource::with_attributes(base_resource, deployment_attrs)
  
  // 添加运行时资源属性
  let runtime_attrs = [
    ("runtime.name", StringValue("moonbit")),
    ("runtime.version", StringValue("0.1.0")),
    ("runtime.os", StringValue("linux"))
  ]
  let runtime_resource = Resource::with_attributes(base_resource, runtime_attrs)
  
  // 测试不同的合并策略
  let merge_strategy = ResourceMergeStrategy::new()
  
  // 策略1: 后者覆盖前者
  ResourceMergeStrategy::set_strategy(merge_strategy, Override)
  let merged_override = ResourceMergeStrategy::merge(
    merge_strategy, 
    [service_resource, deployment_resource, runtime_resource]
  )
  
  // 验证合并结果
  assert_eq(
    Resource::get_attribute(merged_override, "service.name"), 
    Some(StringValue("user.service"))
  )
  assert_eq(
    Resource::get_attribute(merged_override, "deployment.environment"), 
    Some(StringValue("staging"))
  )
  assert_eq(
    Resource::get_attribute(merged_override, "runtime.name"), 
    Some(StringValue("moonbit"))
  )
  
  // 策略2: 保留冲突值
  ResourceMergeStrategy::set_strategy(merge_strategy, PreserveConflict)
  let service_conflict = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("conflict.service"))
  ])
  
  let merged_preserve = ResourceMergeStrategy::merge(
    merge_strategy, 
    [service_resource, service_conflict]
  )
  
  // 验证冲突保留策略
  let service_name = Resource::get_attribute(merged_preserve, "service.name")
  match service_name {
    Some(StringValue("user.service")) => assert_true(true) // 原值保留
    _ => assert_true(false)
  }
}
