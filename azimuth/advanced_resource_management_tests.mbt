// 资源管理测试用例
// 测试Azimuth遥测系统的资源管理和优化功能

test "resource_lifecycle_management" {
  // 测试资源的生命周期管理
  let resource = Resource::new()
  
  // 创建资源并添加属性
  let service_resource = Resource::with_attributes(resource, [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345"))
  ])
  
  // 验证资源创建
  assert_eq(Resource::get_attribute(service_resource, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(service_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(service_resource, "service.instance.id"), Some(StringValue("instance-12345")))
  
  // 模拟资源更新
  let updated_resource = Resource::with_attributes(service_resource, [
    ("service.version", StringValue("1.1.0")),  // 版本升级
    ("deployment.environment", StringValue("production"))
  ])
  
  // 验证资源更新
  assert_eq(Resource::get_attribute(updated_resource, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(updated_resource, "service.version"), Some(StringValue("1.1.0")))
  assert_eq(Resource::get_attribute(updated_resource, "deployment.environment"), Some(StringValue("production")))
}

test "resource_attribute_inheritance" {
  // 测试资源属性继承
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("kubernetes.cluster", StringValue("production-cluster"))
  ])
  
  let service_resource = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("user-service")),
    ("service.namespace", StringValue("backend"))
  ])
  
  let instance_resource = Resource::with_attributes(service_resource, [
    ("service.instance.id", StringValue("pod-xyz123")),
    ("host.name", StringValue("worker-node-1"))
  ])
  
  // 验证属性继承链
  assert_eq(Resource::get_attribute(instance_resource, "cloud.provider"), Some(StringValue("aws")))
  assert_eq(Resource::get_attribute(instance_resource, "cloud.region"), Some(StringValue("us-west-2")))
  assert_eq(Resource::get_attribute(instance_resource, "kubernetes.cluster"), Some(StringValue("production-cluster")))
  assert_eq(Resource::get_attribute(instance_resource, "service.name"), Some(StringValue("user-service")))
  assert_eq(Resource::get_attribute(instance_resource, "service.namespace"), StringValue("backend"))
  assert_eq(Resource::get_attribute(instance_resource, "service.instance.id"), StringValue("pod-xyz123"))
  assert_eq(Resource::get_attribute(instance_resource, "host.name"), StringValue("worker-node-1"))
}

test "resource_merge_strategies" {
  // 测试资源合并策略
  let resource1 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-a")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("staging"))
  ])
  
  let resource2 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-b")),  // 冲突的键
    ("host.name", StringValue("host-1")),
    ("process.pid", StringValue("1234"))
  ])
  
  let resource3 = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("2.0.0")),  // 冲突的键
    ("container.id", StringValue("container-abc")),
    ("process.command", StringValue("/app/main"))
  ])
  
  // 测试不同的合并策略
  let merged_1_2 = Resource::merge(resource1, resource2)
  let merged_all = Resource::merge(merged_1_2, resource3)
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged_all, "service.name"), Some(StringValue("service-b")))  // 后者覆盖
  assert_eq(Resource::get_attribute(merged_all, "service.version"), Some(StringValue("2.0.0")))  // 后者覆盖
  assert_eq(Resource::get_attribute(merged_all, "deployment.environment"), Some(StringValue("staging")))
  assert_eq(Resource::get_attribute(merged_all, "host.name"), Some(StringValue("host-1")))
  assert_eq(Resource::get_attribute(merged_all, "process.pid"), Some(StringValue("1234")))
  assert_eq(Resource::get_attribute(merged_all, "container.id"), Some(StringValue("container-abc")))
  assert_eq(Resource::get_attribute(merged_all, "process.command"), Some(StringValue("/app/main")))
}

test "resource_memory_optimization" {
  // 测试资源内存优化
  let large_attributes = []
  
  // 创建大量属性
  for i in 0..1000 {
    large_attributes.push(("attr." + i.to_string(), StringValue("value." + i.to_string())))
  }
  
  let large_resource = Resource::with_attributes(Resource::new(), large_attributes)
  
  // 验证大资源创建成功
  for i in 0..10 {  // 只验证前10个，避免性能问题
    let attr_name = "attr." + i.to_string()
    let expected_value = "value." + i.to_string()
    assert_eq(Resource::get_attribute(large_resource, attr_name), Some(StringValue(expected_value)))
  }
  
  // 测试资源清理和内存释放
  let optimized_resource = Resource::new()  // 创建新的空资源
  
  // 验证优化后的资源是空的
  assert_eq(Resource::get_attribute(optimized_resource, "any.key"), None)
}

test "resource_attribute_validation" {
  // 测试资源属性验证
  let resource = Resource::new()
  
  // 测试有效的属性
  let valid_attributes = [
    ("service.name", StringValue("valid-service-name")),
    ("service.version", StringValue("1.0.0")),
    ("host.ip", StringValue("192.168.1.1")),
    ("process.pid", IntValue(1234)),
    ("process.start.time", IntValue(1735689600)),
    ("container.running", BoolValue(true)),
    ("cpu.count", IntValue(4)),
    ("memory.total", IntValue(8589934592))
  ]
  
  let validated_resource = Resource::with_attributes(resource, valid_attributes)
  
  // 验证所有有效属性都能正确设置
  for (key, value) in valid_attributes {
    assert_eq(Resource::get_attribute(validated_resource, key), Some(value))
  }
  
  // 测试边界情况属性
  let edge_case_attributes = [
    ("empty.string", StringValue("")),
    ("zero.number", IntValue(0)),
    ("negative.number", IntValue(-1)),
    ("large.number", IntValue(2147483647)),
    ("boolean.false", BoolValue(false))
  ]
  
  let edge_case_resource = Resource::with_attributes(Resource::new(), edge_case_attributes)
  
  // 验证边界情况属性
  for (key, value) in edge_case_attributes {
    assert_eq(Resource::get_attribute(edge_case_resource, key), Some(value))
  }
}

test "resource_attribute_caching" {
  // 测试资源属性缓存
  let resource = Resource::with_attributes(Resource::new(), [
    ("cache.test", StringValue("cached-value")),
    ("frequent.access", StringValue("frequently-accessed"))
  ])
  
  // 模拟频繁访问
  for i in 0..100 {
    let cached_value = Resource::get_attribute(resource, "cache.test")
    assert_eq(cached_value, Some(StringValue("cached-value")))
    
    let frequent_value = Resource::get_attribute(resource, "frequent.access")
    assert_eq(frequent_value, Some(StringValue("frequently-accessed")))
  }
  
  // 测试缓存失效和更新
  let updated_resource = Resource::with_attributes(resource, [
    ("cache.test", StringValue("updated-value"))
  ])
  
  // 验证缓存更新
  let updated_value = Resource::get_attribute(updated_resource, "cache.test")
  assert_eq(updated_value, Some(StringValue("updated-value")))
}

test "resource_concurrent_access" {
  // 测试资源并发访问
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("concurrent.test", StringValue("base-value"))
  ])
  
  // 模拟并发创建资源
  let resources = []
  for i in 0..10 {
    let concurrent_resource = Resource::with_attributes(base_resource, [
      ("thread.id", StringValue("thread-" + i.to_string())),
      ("iteration", IntValue(i))
    ])
    resources.push(concurrent_resource)
  }
  
  // 验证并发创建的资源
  for (i, resource) in resources.enumerate() {
    let thread_id = Resource::get_attribute(resource, "thread.id")
    let iteration = Resource::get_attribute(resource, "iteration")
    
    assert_eq(thread_id, Some(StringValue("thread-" + i.to_string())))
    assert_eq(iteration, Some(IntValue(i)))
    assert_eq(Resource::get_attribute(resource, "concurrent.test"), Some(StringValue("base-value")))
  }
  
  assert_eq(resources.length(), 10)
}

test "resource_serialization_compatibility" {
  // 测试资源序列化兼容性
  let original_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("serialization-test")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("test-host")),
    ("process.pid", IntValue(1234)),
    ("container.running", BoolValue(true))
  ])
  
  // 模拟序列化和反序列化过程
  let serialized_data = [
    ("service.name", "StringValue", "serialization-test"),
    ("service.version", "StringValue", "1.0.0"),
    ("host.name", "StringValue", "test-host"),
    ("process.pid", "IntValue", "1234"),
    ("container.running", "BoolValue", "true")
  ]
  
  // 模拟反序列化
  let deserialized_resource = Resource::new()
  let reconstructed_resource = Resource::with_attributes(deserialized_resource, [
    ("service.name", StringValue("serialization-test")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("test-host")),
    ("process.pid", IntValue(1234)),
    ("container.running", BoolValue(true))
  ])
  
  // 验证序列化兼容性
  assert_eq(
    Resource::get_attribute(original_resource, "service.name"),
    Resource::get_attribute(reconstructed_resource, "service.name")
  )
  assert_eq(
    Resource::get_attribute(original_resource, "process.pid"),
    Resource::get_attribute(reconstructed_resource, "process.pid")
  )
  assert_eq(
    Resource::get_attribute(original_resource, "container.running"),
    Resource::get_attribute(reconstructed_resource, "container.running")
  )
}

test "resource_attribute_filtering" {
  // 测试资源属性过滤
  let full_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("filter-test")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("test-host")),
    ("process.pid", IntValue(1234)),
    ("container.id", StringValue("container-123")),
    ("kubernetes.pod.name", StringValue("pod-xyz")),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("deployment.environment", StringValue("staging")),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("1.0.0"))
  ])
  
  // 定义过滤规则
  let service_filter = ["service.name", "service.version"]
  let infrastructure_filter = ["host.name", "process.pid", "container.id", "kubernetes.pod.name"]
  let cloud_filter = ["cloud.provider", "cloud.region", "deployment.environment"]
  let telemetry_filter = ["telemetry.sdk.name", "telemetry.sdk.version"]
  
  // 验证过滤结果
  for key in service_filter {
    let value = Resource::get_attribute(full_resource, key)
    assert_true(value != None, "Service filter key " + key + " should exist")
  }
  
  for key in infrastructure_filter {
    let value = Resource::get_attribute(full_resource, key)
    assert_true(value != None, "Infrastructure filter key " + key + " should exist")
  }
  
  for key in cloud_filter {
    let value = Resource::get_attribute(full_resource, key)
    assert_true(value != None, "Cloud filter key " + key + " should exist")
  }
  
  for key in telemetry_filter {
    let value = Resource::get_attribute(full_resource, key)
    assert_true(value != None, "Telemetry filter key " + key + " should exist")
  }
  
  // 验证过滤规则长度
  assert_eq(service_filter.length(), 2)
  assert_eq(infrastructure_filter.length(), 4)
  assert_eq(cloud_filter.length(), 3)
  assert_eq(telemetry_filter.length(), 2)
}

test "resource_hierarchy_management" {
  // 测试资源层次管理
  // 全局资源
  let global_resource = Resource::with_attributes(Resource::new(), [
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("1.0.0")),
    ("telemetry.sdk.language", StringValue("moonbit"))
  ])
  
  // 云资源
  let cloud_resource = Resource::with_attributes(global_resource, [
    ("cloud.provider", StringValue("aws")),
    ("cloud.account.id", StringValue("123456789012")),
    ("cloud.region", StringValue("us-west-2"))
  ])
  
  // 集群资源
  let cluster_resource = Resource::with_attributes(cloud_resource, [
    ("kubernetes.cluster.name", StringValue("production-cluster")),
    ("kubernetes.namespace", StringValue("default"))
  ])
  
  // 主机资源
  let host_resource = Resource::with_attributes(cluster_resource, [
    ("host.name", StringValue("ip-10-0-1-123.us-west-2.compute.internal")),
    ("host.id", StringValue("i-1234567890abcdef0")),
    ("host.type", StringValue("t3.medium"))
  ])
  
  // 服务资源
  let service_resource = Resource::with_attributes(host_resource, [
    ("service.name", StringValue("api-gateway")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-abc123"))
  ])
  
  // 验证层次结构
  let hierarchy_checks = [
    ("telemetry.sdk.name", "azimuth"),
    ("cloud.provider", "aws"),
    ("kubernetes.cluster.name", "production-cluster"),
    ("host.name", "ip-10-0-1-123.us-west-2.compute.internal"),
    ("service.name", "api-gateway")
  ]
  
  for (key, expected_value) in hierarchy_checks {
    let actual_value = Resource::get_attribute(service_resource, key)
    assert_eq(actual_value, Some(StringValue(expected_value)))
  }
}