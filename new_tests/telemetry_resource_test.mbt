// 遥测资源管理测试用例

test "resource_basic_creation_and_properties" {
  // 测试资源的基本创建和属性
  
  // 创建空资源
  let empty_resource = azimuth::telemetry::api::common::Resource::empty()
  assert_eq(empty_resource.attributes.size(), 0)
  
  // 创建带属性的资源
  let resource1 = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("payment-service"))
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("1.2.3"))
    .build()
  
  assert_eq(resource1.attributes.size(), 2)
  assert_eq(resource1.attributes.get("service.name").unwrap().to_string(), "payment-service")
  assert_eq(resource1.attributes.get("service.version").unwrap().to_string(), "1.2.3")
  
  // 测试资源的不可变性
  let resource2 = resource1.with("service.instance", azimuth::telemetry::api::common::AttributeValue::String("instance-001"))
  assert_eq(resource1.attributes.size(), 2)  // 原资源不变
  assert_eq(resource2.attributes.size(), 3)  // 新资源增加属性
  assert_eq(resource2.attributes.get("service.instance").unwrap().to_string(), "instance-001")
}

test "resource_default_attributes" {
  // 测试资源的默认属性
  
  // 创建默认资源
  let default_resource = azimuth::telemetry::api::common::Resource::default()
  
  // 验证默认包含的属性
  assert_eq(default_resource.attributes.size() >= 3, true)
  assert_eq(default_resource.attributes.get("service.name").unwrap().to_string(), "unknown_service")
  assert_eq(default_resource.attributes.get("telemetry.sdk.name").unwrap().to_string(), "azimuth_telemetry")
  assert_eq(default_resource.attributes.get("telemetry.sdk.language").unwrap().to_string(), "moonbit")
  assert_eq(default_resource.attributes.get("telemetry.sdk.version").unwrap().to_string().length() > 0, true)
}

test "resource_merging_and_combination" {
  // 测试资源的合并和组合
  
  // 创建基础资源
  let base_resource = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("auth-service"))
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("2.0.0"))
    .with("deployment.environment", azimuth::telemetry::api::common::AttributeValue::String("production"))
    .build()
  
  // 创建覆盖资源
  let override_resource = azimuth::telemetry::api::common::Resource::builder()
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("2.1.0"))  // 覆盖版本
    .with("service.instance.id", azimuth::telemetry::api::common::AttributeValue::String("instance-prod-01"))
    .with("host.name", azimuth::telemetry::api::common::AttributeValue::String("prod-server-01"))
    .build()
  
  // 合并资源
  let merged_resource = base_resource.merge(override_resource)
  
  // 验证合并结果
  assert_eq(merged_resource.attributes.size(), 5)
  assert_eq(merged_resource.attributes.get("service.name").unwrap().to_string(), "auth-service")
  assert_eq(merged_resource.attributes.get("service.version").unwrap().to_string(), "2.1.0")  // 被覆盖
  assert_eq(merged_resource.attributes.get("deployment.environment").unwrap().to_string(), "production")
  assert_eq(merged_resource.attributes.get("service.instance.id").unwrap().to_string(), "instance-prod-01")
  assert_eq(merged_resource.attributes.get("host.name").unwrap().to_string(), "prod-server-01")
}

test "resource_builder_pattern" {
  // 测试资源构建器模式
  
  // 使用构建器创建复杂资源
  let resource = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("order-service"))
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("3.1.4"))
    .with("service.namespace", azimuth::telemetry::api::common::AttributeValue::String("ecommerce"))
    .with("service.instance.id", azimuth::telemetry::api::common::AttributeValue::String("order-service-001"))
    .with("deployment.environment", azimuth::telemetry::api::common::AttributeValue::String("staging"))
    .with("host.name", azimuth::telemetry::api::common::AttributeValue::String("staging-node-03"))
    .with("host.arch", azimuth::telemetry::api::common::AttributeValue::String("amd64"))
    .with("os.type", azimuth::telemetry::api::common::AttributeValue::String("linux"))
    .with("os.version", azimuth::telemetry::api::common::AttributeValue::String("5.15.0"))
    .with("process.pid", azimuth::telemetry::api::common::AttributeValue::Int64(12345))
    .with("process.executable.name", azimuth::telemetry::api::common::AttributeValue::String("order-service"))
    .with("process.command_args", azimuth::telemetry::api::common::AttributeValue::ArrayString(["./order-service", "--config=staging.yaml"]))
    .build()
  
  // 验证所有属性
  assert_eq(resource.attributes.size(), 12)
  assert_eq(resource.attributes.get("service.name").unwrap().to_string(), "order-service")
  assert_eq(resource.attributes.get("service.version").unwrap().to_string(), "3.1.4")
  assert_eq(resource.attributes.get("service.namespace").unwrap().to_string(), "ecommerce")
  assert_eq(resource.attributes.get("service.instance.id").unwrap().to_string(), "order-service-001")
  assert_eq(resource.attributes.get("deployment.environment").unwrap().to_string(), "staging")
  assert_eq(resource.attributes.get("host.name").unwrap().to_string(), "staging-node-03")
  assert_eq(resource.attributes.get("host.arch").unwrap().to_string(), "amd64")
  assert_eq(resource.attributes.get("os.type").unwrap().to_string(), "linux")
  assert_eq(resource.attributes.get("os.version").unwrap().to_string(), "5.15.0")
  assert_eq(resource.attributes.get("process.pid").unwrap().to_string(), "12345")
  assert_eq(resource.attributes.get("process.executable.name").unwrap().to_string(), "order-service")
  assert_eq(resource.attributes.get("process.command_args").unwrap().to_string(), "[./order-service, --config=staging.yaml]")
}

test "resource_environment_detection" {
  // 测试资源的环境检测
  
  // 创建自动检测环境信息的资源
  let auto_resource = azimuth::telemetry::api::common::Resource::auto_detect()
  
  // 验证自动检测的属性
  assert_eq(auto_resource.attributes.size() >= 5, true)
  
  // 应该包含服务相关信息
  assert_eq(auto_resource.attributes.get("telemetry.sdk.name").unwrap().to_string(), "azimuth_telemetry")
  assert_eq(auto_resource.attributes.get("telemetry.sdk.language").unwrap().to_string(), "moonbit")
  
  // 应该包含主机相关信息（如果可检测）
  let has_host_info = auto_resource.attributes.get("host.name").is_some() or 
                     auto_resource.attributes.get("host.arch").is_some()
  assert_eq(has_host_info, true)
  
  // 应该包含进程相关信息（如果可检测）
  let has_process_info = auto_resource.attributes.get("process.pid").is_some() or 
                        auto_resource.attributes.get("process.executable.name").is_some()
  assert_eq(has_process_info, true)
}

test "resource_validation_and_constraints" {
  // 测试资源的验证和约束
  
  // 测试空键和值的处理
  let resource1 = azimuth::telemetry::api::common::Resource::builder()
    .with("", azimuth::telemetry::api::common::AttributeValue::String("empty_key_value"))
    .with("empty_value", azimuth::telemetry::api::common::AttributeValue::String(""))
    .with("normal", azimuth::telemetry::api::common::AttributeValue::String("normal_value"))
    .build()
  
  assert_eq(resource1.attributes.size(), 3)
  assert_eq(resource1.attributes.get("").unwrap().to_string(), "empty_key_value")
  assert_eq(resource1.attributes.get("empty_value").unwrap().to_string(), "")
  
  // 测试特殊字符键
  let resource2 = resource1.with("key.with.special@chars", azimuth::telemetry::api::common::AttributeValue::String("special_value"))
  assert_eq(resource2.attributes.size(), 4)
  
  // 测试Unicode键和值
  let resource3 = resource2.with("中文键", azimuth::telemetry::api::common::AttributeValue::String("中文值"))
  assert_eq(resource3.attributes.size(), 5)
  
  // 测试长键和值
  let long_key = "this.is.a.very.long.key.name.that.exceeds.normal.expectations"
  let long_value = "this.is.a.very.long.value.that.contains.a.lot.of.information"
  let resource4 = resource3.with(long_key, azimuth::telemetry::api::common::AttributeValue::String(long_value))
  assert_eq(resource4.attributes.size(), 6)
}

test "resource_serialization_and_deserialization" {
  // 测试资源的序列化和反序列化
  
  // 创建复杂资源
  let original_resource = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("inventory-service"))
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("4.2.1"))
    .with("service.namespace", azimuth::telemetry::api::common::AttributeValue::String("warehouse"))
    .with("deployment.environment", azimuth::telemetry::api::common::AttributeValue::String("production"))
    .with("host.name", azimuth::telemetry::api::common::AttributeValue::String("warehouse-server-02"))
    .with("cloud.provider", azimuth::telemetry::api::common::AttributeValue::String("aws"))
    .with("cloud.region", azimuth::telemetry::api::common::AttributeValue::String("us-west-2"))
    .with("cloud.availability_zone", azimuth::telemetry::api::common::AttributeValue::String("us-west-2a"))
    .with("k8s.cluster.name", azimuth::telemetry::api::common::AttributeValue::String("production-cluster"))
    .with("k8s.namespace.name", azimuth::telemetry::api::common::AttributeValue::String("warehouse"))
    .with("k8s.pod.name", azimuth::telemetry::api::common::AttributeValue::String("inventory-service-7f8d9c2b"))
    .build()
  
  // 序列化为JSON
  let serialized = original_resource.to_json()
  assert_eq(serialized.length() > 0, true)
  
  // 反序列化
  let deserialized_resource = azimuth::telemetry::api::common::Resource::from_json(serialized)
  assert_eq(deserialized_resource.attributes.size(), original_resource.attributes.size())
  
  // 验证反序列化的值
  assert_eq(deserialized_resource.attributes.get("service.name").unwrap().to_string(), "inventory-service")
  assert_eq(deserialized_resource.attributes.get("service.version").unwrap().to_string(), "4.2.1")
  assert_eq(deserialized_resource.attributes.get("cloud.provider").unwrap().to_string(), "aws")
  assert_eq(deserialized_resource.attributes.get("k8s.pod.name").unwrap().to_string(), "inventory-service-7f8d9c2b")
}

test "resource_attribute_inheritance_and_override" {
  // 测试资源属性的继承和覆盖
  
  // 创建父资源
  let parent_resource = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("base-service"))
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("1.0.0"))
    .with("deployment.environment", azimuth::telemetry::api::common::AttributeValue::String("development"))
    .with("team.name", azimuth::telemetry::api::common::AttributeValue::String("platform"))
    .build()
  
  // 创建子资源（继承父资源并覆盖部分属性）
  let child_resource = azimuth::telemetry::api::common::Resource::builder()
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("1.1.0"))  // 覆盖版本
    .with("deployment.environment", azimuth::telemetry::api::common::AttributeValue::String("testing"))  // 覆盖环境
    .with("feature.flags", azimuth::telemetry::api::common::AttributeValue::String("new_ui,experimental"))
    .build()
  
  // 继承并合并
  let inherited_resource = parent_resource.inherit(child_resource)
  
  // 验证继承结果
  assert_eq(inherited_resource.attributes.size(), 5)
  assert_eq(inherited_resource.attributes.get("service.name").unwrap().to_string(), "base-service")  // 继承
  assert_eq(inherited_resource.attributes.get("service.version").unwrap().to_string(), "1.1.0")  // 覆盖
  assert_eq(inherited_resource.attributes.get("deployment.environment").unwrap().to_string(), "testing")  // 覆盖
  assert_eq(inherited_resource.attributes.get("team.name").unwrap().to_string(), "platform")  // 继承
  assert_eq(inherited_resource.attributes.get("feature.flags").unwrap().to_string(), "new_ui,experimental")  // 新增
}

test "resource_performance_considerations" {
  // 测试资源的性能考虑
  
  // 创建包含大量属性的资源
  let mut builder = azimuth::telemetry::api::common::Resource::builder()
  for i = 0; i < 100; i = i + 1 {
    let key = "attribute." + i.to_string()
    let value = "value." + i.to_string()
    builder = builder.with(key, azimuth::telemetry::api::common::AttributeValue::String(value))
  }
  let large_resource = builder.build()
  
  // 验证大小
  assert_eq(large_resource.attributes.size(), 100)
  
  // 验证可以正确检索属性
  for i = 0; i < 100; i = i + 1 {
    let key = "attribute." + i.to_string()
    let expected_value = "value." + i.to_string()
    assert_eq(large_resource.attributes.get(key).unwrap().to_string(), expected_value)
  }
  
  // 测试资源合并的性能
  let another_large_resource = azimuth::telemetry::api::common::Resource::builder()
    .with("additional.attr1", azimuth::telemetry::api::common::AttributeValue::String("additional1"))
    .with("additional.attr2", azimuth::telemetry::api::common::AttributeValue::String("additional2"))
    .build()
  
  let merged_resource = large_resource.merge(another_large_resource)
  assert_eq(merged_resource.attributes.size(), 102)
}