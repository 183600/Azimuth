// 资源管理测试用例
// 测试 Azimuth 遥测系统中的资源管理功能

test "telemetry_resource_creation_and_attributes" {
  // 创建基础资源
  let resource = Resource::new()
  
  // 验证空资源
  assert_eq(resource.attributes.length(), 0)
  
  // 设置资源属性
  let attributes = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("prod-server-01")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("20.04")),
    ("process.id", IntValue(1234)),
    ("process.executable.name", StringValue("payment-service")),
    ("process.command_args", ArrayStringValue(["./payment-service", "--config=prod.yaml"]))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // 验证资源属性设置
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let service_instance_id = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  let environment = Resource::get_attribute(resource_with_attrs, "deployment.environment")
  let host_name = Resource::get_attribute(resource_with_attrs, "host.name")
  let host_arch = Resource::get_attribute(resource_with_attrs, "host.arch")
  let os_type = Resource::get_attribute(resource_with_attrs, "os.type")
  let os_version = Resource::get_attribute(resource_with_attrs, "os.version")
  let process_id = Resource::get_attribute(resource_with_attrs, "process.id")
  let executable_name = Resource::get_attribute(resource_with_attrs, "process.executable.name")
  let command_args = Resource::get_attribute(resource_with_attrs, "process.command_args")
  
  // 验证字符串属性
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "payment-service")
    _ => assert_true(false, "Expected service name")
  }
  
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "2.1.0")
    _ => assert_true(false, "Expected service version")
  }
  
  match environment {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false, "Expected environment")
  }
  
  // 验证整数属性
  match process_id {
    Some(IntValue(pid)) => assert_eq(pid, 1234)
    _ => assert_true(false, "Expected process ID")
  }
  
  // 验证数组属性
  match command_args {
    Some(ArrayStringValue(args)) => {
      assert_eq(args.length(), 2)
      assert_eq(args[0], "./payment-service")
      assert_eq(args[1], "--config=prod.yaml")
    }
    _ => assert_true(false, "Expected command args")
  }
}

test "telemetry_resource_merge_strategy" {
  // 创建第一个资源
  let resource1 = Resource::new()
  let attributes1 = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("1.5.0")),
    ("deployment.environment", StringValue("staging")),
    ("host.name", StringValue("staging-server-01"))
  ]
  
  let resource1_with_attrs = Resource::with_attributes(resource1, attributes1)
  
  // 创建第二个资源
  let resource2 = Resource::new()
  let attributes2 = [
    ("service.name", StringValue("payment-service")),  // 冲突属性
    ("service.instance.id", StringValue("instance-67890")),
    ("host.name", StringValue("staging-server-02")),  // 冲突属性
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2"))
  ]
  
  let resource2_with_attrs = Resource::with_attributes(resource2, attributes2)
  
  // 合并资源（默认策略：第二个资源覆盖第一个资源）
  let merged_resource = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  
  // 验证合并结果
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  let service_instance_id = Resource::get_attribute(merged_resource, "service.instance.id")
  let environment = Resource::get_attribute(merged_resource, "deployment.environment")
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  let cloud_provider = Resource::get_attribute(merged_resource, "cloud.provider")
  let cloud_region = Resource::get_attribute(merged_resource, "cloud.region")
  
  // 验证冲突属性的值来自第二个资源
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "payment-service")  // 来自resource2
    _ => assert_true(false, "Expected merged service name")
  }
  
  match host_name {
    Some(StringValue(host)) => assert_eq(host, "staging-server-02")  // 来自resource2
    _ => assert_true(false, "Expected merged host name")
  }
  
  // 验证非冲突属性保留
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.5.0")  // 来自resource1
    _ => assert_true(false, "Expected original service version")
  }
  
  match environment {
    Some(StringValue(env)) => assert_eq(env, "staging")  // 来自resource1
    _ => assert_true(false, "Expected original environment")
  }
  
  // 验证新属性添加
  match service_instance_id {
    Some(StringValue(id)) => assert_eq(id, "instance-67890")  // 来自resource2
    _ => assert_true(false, "Expected new service instance ID")
  }
  
  match cloud_provider {
    Some(StringValue(provider)) => assert_eq(provider, "aws")  // 来自resource2
    _ => assert_true(false, "Expected cloud provider")
  }
}

test "telemetry_resource_default_detection" {
  // 测试自动资源检测
  let auto_detected_resource = Resource::auto_detect()
  
  // 验证自动检测的属性
  let service_name = Resource::get_attribute(auto_detected_resource, "service.name")
  let host_name = Resource::get_attribute(auto_detected_resource, "host.name")
  let os_type = Resource::get_attribute(auto_detected_resource, "os.type")
  let process_id = Resource::get_attribute(auto_detected_resource, "process.id")
  
  // 验证某些属性应该被自动检测
  assert_true(process_id != None, "Process ID should be auto-detected")
  assert_true(os_type != None, "OS type should be auto-detected")
}

test "telemetry_resource_environment_variables" {
  // 模拟环境变量
  let env_attributes = [
    ("OTEL_SERVICE_NAME", StringValue("env-service")),
    ("OTEL_SERVICE_VERSION", StringValue("3.0.0")),
    ("OTEL_RESOURCE_ATTRIBUTES", StringValue("deployment.environment=development,team=backend")),
    ("OTEL_EXPORTER_OTLP_ENDPOINT", StringValue("http://localhost:4317"))
  ]
  
  // 从环境变量创建资源
  let env_resource = Resource::from_environment(env_attributes)
  
  // 验证环境变量解析
  let service_name = Resource::get_attribute(env_resource, "service.name")
  let service_version = Resource::get_attribute(env_resource, "service.version")
  let environment = Resource::get_attribute(env_resource, "deployment.environment")
  let team = Resource::get_attribute(env_resource, "team")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "env-service")
    _ => assert_true(false, "Expected service name from environment")
  }
  
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "3.0.0")
    _ => assert_true(false, "Expected service version from environment")
  }
  
  match environment {
    Some(StringValue(env)) => assert_eq(env, "development")
    _ => assert_true(false, "Expected environment from OTEL_RESOURCE_ATTRIBUTES")
  }
  
  match team {
    Some(StringValue(team_name)) => assert_eq(team_name, "backend")
    _ => assert_true(false, "Expected team from OTEL_RESOURCE_ATTRIBUTES")
  }
}

test "telemetry_resource_schema_validation" {
  // 测试资源属性模式验证
  let resource = Resource::new()
  
  // 测试有效的属性
  let valid_attributes = [
    ("service.name", StringValue("valid-service")),  // 必需属性
    ("service.version", StringValue("1.0.0")),  // 标准属性
    ("custom.attribute", StringValue("custom-value")),  // 自定义属性
    ("numeric.attribute", IntValue(42)),  // 数值属性
    ("boolean.attribute", BoolValue(true))  // 布尔属性
  ]
  
  let valid_resource = Resource::with_attributes(resource, valid_attributes)
  
  // 验证有效资源创建成功
  let service_name = Resource::get_attribute(valid_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "valid-service")
    _ => assert_true(false, "Expected valid service name")
  }
  
  // 测试无效的属性（空的服务名）
  let invalid_attributes = [
    ("service.name", StringValue("")),  // 无效：空字符串
    ("service.version", StringValue("1.0.0"))
  ]
  
  let invalid_resource = Resource::with_attributes(resource, invalid_attributes)
  
  // 验证无效属性的处理
  let invalid_service_name = Resource::get_attribute(invalid_resource, "service.name")
  match invalid_service_name {
    Some(StringValue(name)) => assert_eq(name, "")  // 仍然设置，但可能被标记为无效
    _ => assert_true(false, "Expected empty service name")
  }
}

test "telemetry_resource_serialization" {
  // 创建资源
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("serialization-test")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("test")),
    ("process.id", IntValue(1234)),
    ("feature.flags", ArrayStringValue(["feature-a", "feature-b"]))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // 序列化资源
  let serialized = Resource::serialize(resource_with_attrs)
  
  // 验证序列化结果不为空
  assert_true(serialized.length() > 0, "Serialized resource should not be empty")
  
  // 反序列化资源
  let deserialized_resource = Resource::deserialize(serialized)
  
  // 验证反序列化结果
  let service_name = Resource::get_attribute(deserialized_resource, "service.name")
  let service_version = Resource::get_attribute(deserialized_resource, "service.version")
  let environment = Resource::get_attribute(deserialized_resource, "deployment.environment")
  let process_id = Resource::get_attribute(deserialized_resource, "process.id")
  let feature_flags = Resource::get_attribute(deserialized_resource, "feature.flags")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "serialization-test")
    _ => assert_true(false, "Expected deserialized service name")
  }
  
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false, "Expected deserialized service version")
  }
  
  match process_id {
    Some(IntValue(pid)) => assert_eq(pid, 1234)
    _ => assert_true(false, "Expected deserialized process ID")
  }
  
  match feature_flags {
    Some(ArrayStringValue(flags)) => {
      assert_eq(flags.length(), 2)
      assert_eq(flags[0], "feature-a")
      assert_eq(flags[1], "feature-b")
    }
    _ => assert_true(false, "Expected deserialized feature flags")
  }
}