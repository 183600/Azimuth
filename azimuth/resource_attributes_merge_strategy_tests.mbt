// 资源属性合并策略测试用例
// 测试遥测资源属性的合并、覆盖和优先级策略

test "资源属性基本合并策略测试" {
  // 1. 创建基础资源
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("prod-server-01"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  // 验证基础资源属性
  let service_name = Resource::get_attribute(base_with_attrs, "service.name")
  let service_version = Resource::get_attribute(base_with_attrs, "service.version")
  let environment = Resource::get_attribute(base_with_attrs, "deployment.environment")
  let host_name = Resource::get_attribute(base_with_attrs, "host.name")
  
  assert_eq(service_name, Some(StringValue("user-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(environment, Some(StringValue("production")))
  assert_eq(host_name, Some(StringValue("prod-server-01")))
  
  // 2. 创建覆盖资源
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.version", StringValue("1.1.0")),  // 覆盖版本
    ("host.name", StringValue("prod-server-02")), // 覆盖主机名
    ("feature.flag", StringValue("new-auth")),   // 新增功能标志
    ("region", StringValue("us-west-2"))         // 新增区域信息
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // 3. 执行资源合并
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // 4. 验证合并结果
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_service_version = Resource::get_attribute(merged_resource, "service.version")
  let merged_environment = Resource::get_attribute(merged_resource, "deployment.environment")
  let merged_host_name = Resource::get_attribute(merged_resource, "host.name")
  let merged_feature_flag = Resource::get_attribute(merged_resource, "feature.flag")
  let merged_region = Resource::get_attribute(merged_resource, "region")
  
  // 基础属性应该被保留
  assert_eq(merged_service_name, Some(StringValue("user-service")))
  assert_eq(merged_environment, Some(StringValue("production")))
  
  // 覆盖属性应该使用新值
  assert_eq(merged_service_version, Some(StringValue("1.1.0")))
  assert_eq(merged_host_name, Some(StringValue("prod-server-02")))
  
  // 新增属性应该存在
  assert_eq(merged_feature_flag, Some(StringValue("new-auth")))
  assert_eq(merged_region, Some(StringValue("us-west-2")))
  
  assert_true(true)
}

test "资源属性类型兼容性测试" {
  // 1. 创建具有不同类型属性的资源
  let string_resource = Resource::new()
  let string_attrs = [
    ("service.name", StringValue("api-service")),
    ("description", StringValue("API服务"))
  ]
  let string_with_attrs = Resource::with_attributes(string_resource, string_attrs)
  
  // 2. 创建具有数值类型属性的资源
  let numeric_resource = Resource::new()
  let numeric_attrs = [
    ("service.name", IntValue(42)),  // 类型冲突：字符串 vs 整数
    ("port", IntValue(8080)),
    ("cpu.cores", IntValue(4))
  ]
  let numeric_with_attrs = Resource::with_attributes(numeric_resource, numeric_attrs)
  
  // 3. 创建具有浮点类型属性的资源
  let float_resource = Resource::new()
  let float_attrs = [
    ("cpu.usage", FloatValue(0.75)),
    ("memory.usage", FloatValue(0.62)),
    ("response.time", FloatValue(125.5))
  ]
  let float_with_attrs = Resource::with_attributes(float_resource, float_attrs)
  
  // 4. 创建具有布尔类型属性的资源
  let bool_resource = Resource::new()
  let bool_attrs = [
    ("service.enabled", BoolValue(true)),
    ("debug.mode", BoolValue(false)),
    ("ssl.enabled", BoolValue(true))
  ]
  let bool_with_attrs = Resource::with_attributes(bool_resource, bool_attrs)
  
  // 5. 创建具有数组类型属性的资源
  let array_resource = Resource::new()
  let array_attrs = [
    ("endpoints", ArrayStringValue(["/api/users", "/api/orders", "/api/products"])),
    ("supported.versions", ArrayStringValue(["v1", "v2"])),
    ("server.ports", ArrayIntValue([8080, 8443, 9000]))
  ]
  let array_with_attrs = Resource::with_attributes(array_resource, array_attrs)
  
  // 6. 合并不同类型的资源
  let merged_all = Resource::merge(
    Resource::merge(string_with_attrs, numeric_with_attrs),
    Resource::merge(float_with_attrs, bool_with_attrs)
  )
  let final_merged = Resource::merge(merged_all, array_with_attrs)
  
  // 7. 验证不同类型的属性
  let final_service_name = Resource::get_attribute(final_merged, "service.name")
  let final_port = Resource::get_attribute(final_merged, "port")
  let final_cpu_usage = Resource::get_attribute(final_merged, "cpu.usage")
  let final_service_enabled = Resource::get_attribute(final_merged, "service.enabled")
  let final_endpoints = Resource::get_attribute(final_merged, "endpoints")
  
  // 验证属性值
  assert_eq(final_port, Some(IntValue(8080)))
  assert_eq(final_cpu_usage, Some(FloatValue(0.75)))
  assert_eq(final_service_enabled, Some(BoolValue(true)))
  assert_eq(final_endpoints, Some(ArrayStringValue(["/api/users", "/api/orders", "/api/products"])))
  
  assert_true(true)
}

test "资源属性优先级策略测试" {
  // 1. 模拟不同层级的资源属性
  // 全局级别属性
  let global_resource = Resource::new()
  let global_attrs = [
    ("organization.name", StringValue("acme-corp")),
    ("region", StringValue("global")),
    ("cost.center", StringValue("engineering"))
  ]
  let global_with_attrs = Resource::with_attributes(global_resource, global_attrs)
  
  // 环境级别属性
  let env_resource = Resource::new()
  let env_attrs = [
    ("deployment.environment", StringValue("staging")),
    ("region", StringValue("us-east-1")),  // 覆盖全局region
    ("cluster.name", StringValue("staging-cluster"))
  ]
  let env_with_attrs = Resource::with_attributes(env_resource, env_attrs)
  
  // 服务级别属性
  let service_resource = Resource::new()
  let service_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.environment", StringValue("production")), // 覆盖环境级别
    ("team", StringValue("payments"))
  ]
  let service_with_attrs = Resource::with_attributes(service_resource, service_attrs)
  
  // 实例级别属性
  let instance_resource = Resource::new()
  let instance_attrs = [
    ("instance.id", StringValue("i-1234567890abcdef0")),
    ("host.name", StringValue("payment-prod-03")),
    ("service.version", StringValue("2.1.1")), // 覆盖服务级别版本
    ("restart.count", IntValue(3))
  ]
  let instance_with_attrs = Resource::with_attributes(instance_resource, instance_attrs)
  
  // 2. 按优先级顺序合并资源（实例 > 服务 > 环境 > 全局）
  let env_global_merged = Resource::merge(global_with_attrs, env_with_attrs)
  let service_env_global_merged = Resource::merge(env_global_merged, service_with_attrs)
  let final_resource = Resource::merge(service_env_global_merged, instance_with_attrs)
  
  // 3. 验证优先级策略
  // 全局级别属性应该保留（如果没有被覆盖）
  let org_name = Resource::get_attribute(final_resource, "organization.name")
  let cost_center = Resource::get_attribute(final_resource, "cost.center")
  assert_eq(org_name, Some(StringValue("acme-corp")))
  assert_eq(cost_center, Some(StringValue("engineering")))
  
  // 环境级别属性应该部分保留
  let cluster_name = Resource::get_attribute(final_resource, "cluster.name")
  assert_eq(cluster_name, Some(StringValue("staging-cluster")))
  
  // 服务级别属性应该保留（如果没有被覆盖）
  let service_name = Resource::get_attribute(final_resource, "service.name")
  let team = Resource::get_attribute(final_resource, "team")
  assert_eq(service_name, Some(StringValue("payment-service")))
  assert_eq(team, Some(StringValue("payments")))
  
  // 实例级别属性应该具有最高优先级
  let instance_id = Resource::get_attribute(final_resource, "instance.id")
  let host_name = Resource::get_attribute(final_resource, "host.name")
  let restart_count = Resource::get_attribute(final_resource, "restart.count")
  assert_eq(instance_id, Some(StringValue("i-1234567890abcdef0")))
  assert_eq(host_name, Some(StringValue("payment-prod-03")))
  assert_eq(restart_count, Some(IntValue(3)))
  
  // 验证覆盖策略
  // region应该被环境级别覆盖
  let final_region = Resource::get_attribute(final_resource, "region")
  assert_eq(final_region, Some(StringValue("us-east-1")))
  
  // deployment.environment应该被服务级别覆盖
  let final_env = Resource::get_attribute(final_resource, "deployment.environment")
  assert_eq(final_env, Some(StringValue("production")))
  
  // service.version应该被实例级别覆盖
  let final_version = Resource::get_attribute(final_resource, "service.version")
  assert_eq(final_version, Some(StringValue("2.1.1")))
  
  assert_true(true)
}

test "资源属性合并边界条件测试" {
  // 1. 测试空资源合并
  let empty_resource = Resource::new()
  let non_empty_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test-service"))
  ])
  
  // 空资源与非空资源合并
  let empty_with_non_empty = Resource::merge(empty_resource, non_empty_resource)
  let non_empty_with_empty = Resource::merge(non_empty_resource, empty_resource)
  
  let service_name_1 = Resource::get_attribute(empty_with_non_empty, "service.name")
  let service_name_2 = Resource::get_attribute(non_empty_with_empty, "service.name")
  
  assert_eq(service_name_1, Some(StringValue("test-service")))
  assert_eq(service_name_2, Some(StringValue("test-service")))
  
  // 2. 测试空属性值
  let resource_with_empty_values = Resource::with_attributes(Resource::new(), [
    ("empty.string", StringValue("")),
    ("empty.int", IntValue(0)),
    ("empty.float", FloatValue(0.0)),
    ("empty.bool", BoolValue(false))
  ])
  
  let empty_string = Resource::get_attribute(resource_with_empty_values, "empty.string")
  let empty_int = Resource::get_attribute(resource_with_empty_values, "empty.int")
  let empty_float = Resource::get_attribute(resource_with_empty_values, "empty.float")
  let empty_bool = Resource::get_attribute(resource_with_empty_values, "empty.bool")
  
  assert_eq(empty_string, Some(StringValue("")))
  assert_eq(empty_int, Some(IntValue(0)))
  assert_eq(empty_float, Some(FloatValue(0.0)))
  assert_eq(empty_bool, Some(BoolValue(false)))
  
  // 3. 测试特殊字符和Unicode
  let unicode_resource = Resource::with_attributes(Resource::new(), [
    ("unicode.name", StringValue("服务名称")),
    ("special.chars", StringValue("a.b-c_d/e")),
    ("emoji", StringValue("🚀📊")),
    ("spaces", StringValue("value with spaces"))
  ])
  
  let unicode_name = Resource::get_attribute(unicode_resource, "unicode.name")
  let special_chars = Resource::get_attribute(unicode_resource, "special.chars")
  let emoji = Resource::get_attribute(unicode_resource, "emoji")
  let spaces = Resource::get_attribute(unicode_resource, "spaces")
  
  assert_eq(unicode_name, Some(StringValue("服务名称")))
  assert_eq(special_chars, Some(StringValue("a.b-c_d/e")))
  assert_eq(emoji, Some(StringValue("🚀📊")))
  assert_eq(spaces, Some(StringValue("value with spaces")))
  
  // 4. 测试超长属性名和值
  let long_key = "very.long.attribute.name.that.exceeds.normal.limits.and.tests.boundary.conditions"
  let long_value = "very.long.attribute.value.that.exceeds.normal.limits.and.tests.boundary.conditions.for.attribute.values"
  
  let long_resource = Resource::with_attributes(Resource::new(), [
    (long_key, StringValue(long_value))
  ])
  
  let retrieved_long_value = Resource::get_attribute(long_resource, long_key)
  assert_eq(retrieved_long_value, Some(StringValue(long_value)))
  
  // 5. 测试大量属性合并
  let many_attrs_resource_1 = Resource::with_attributes(Resource::new(), [
    ("attr.1", StringValue("value.1")),
    ("attr.2", StringValue("value.2")),
    ("attr.3", StringValue("value.3")),
    ("attr.4", StringValue("value.4")),
    ("attr.5", StringValue("value.5"))
  ])
  
  let many_attrs_resource_2 = Resource::with_attributes(Resource::new(), [
    ("attr.6", StringValue("value.6")),
    ("attr.7", StringValue("value.7")),
    ("attr.8", StringValue("value.8")),
    ("attr.9", StringValue("value.9")),
    ("attr.10", StringValue("value.10"))
  ])
  
  let merged_many_attrs = Resource::merge(many_attrs_resource_1, many_attrs_resource_2)
  
  // 验证所有属性都存在
  for i in range(1, 11) {
    let key = "attr." + i.to_string()
    let value = Resource::get_attribute(merged_many_attrs, key)
    assert_eq(value, Some(StringValue("value." + i.to_string())))
  }
  
  assert_true(true)
}

test "资源属性合并性能测试" {
  // 1. 创建大型资源集合
  let large_attrs_1 = [
    ("service.name", StringValue("large-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("prod-server-01")),
    ("region", StringValue("us-west-2")),
    ("availability.zone", StringValue("us-west-2a")),
    ("instance.type", StringValue("m5.large")),
    ("team", StringValue("backend")),
    ("cost.center", StringValue("engineering")),
    ("project", StringValue("platform"))
  ]
  
  let large_attrs_2 = [
    ("service.version", StringValue("1.1.0")),
    ("host.name", StringValue("prod-server-02")),
    ("cpu.count", IntValue(2)),
    ("memory.gb", IntValue(8)),
    ("disk.gb", IntValue(100)),
    ("network.mbps", IntValue(1000)),
    ("os.version", StringValue("ubuntu-20.04")),
    ("kernel.version", StringValue("5.4.0")),
    ("docker.version", StringValue("20.10.7")),
    ("kubernetes.version", StringValue("1.21.0"))
  ]
  
  let large_resource_1 = Resource::with_attributes(Resource::new(), large_attrs_1)
  let large_resource_2 = Resource::with_attributes(Resource::new(), large_attrs_2)
  
  // 2. 测量合并性能
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 执行多次合并操作
  for _ in range(0, 100) {
    let _ = Resource::merge(large_resource_1, large_resource_2)
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let duration = end_time - start_time
  
  // 3. 验证性能要求
  // 100次合并操作应该在合理时间内完成（这里设置为1秒）
  assert_true(duration < 1000000000L) // 小于1秒
  
  // 4. 验证合并结果正确性
  let final_resource = Resource::merge(large_resource_1, large_resource_2)
  
  let service_name = Resource::get_attribute(final_resource, "service.name")
  let service_version = Resource::get_attribute(final_resource, "service.version")
  let host_name = Resource::get_attribute(final_resource, "host.name")
  let cpu_count = Resource::get_attribute(final_resource, "cpu.count")
  let memory_gb = Resource::get_attribute(final_resource, "memory.gb")
  
  assert_eq(service_name, Some(StringValue("large-service")))
  assert_eq(service_version, Some(StringValue("1.1.0"))) // 应该被覆盖
  assert_eq(host_name, Some(StringValue("prod-server-02"))) // 应该被覆盖
  assert_eq(cpu_count, Some(IntValue(2)))
  assert_eq(memory_gb, Some(IntValue(8)))
  
  assert_true(true)
}