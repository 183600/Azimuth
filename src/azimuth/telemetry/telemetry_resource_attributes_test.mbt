// 遥测资源属性测试
// 测试资源属性的创建、验证和管理功能

test "service_resource_attributes_test" {
  // 测试服务资源属性
  
  let service_name = "user-authentication-service"
  let service_version = "2.1.3"
  let service_namespace = "security"
  let service_instance_id = "auth-service-pod-12345"
  
  // 验证服务名称属性
  assert_eq(service_name.has_prefix("user"), true)
  assert_eq(service_name.has_suffix("service"), true)
  assert_eq(service_name.contains("-"), true)
  assert_eq(service_name.length(), 26)
  
  // 验证服务版本属性
  assert_eq(service_version, "2.1.3")
  assert_eq(service_version.contains("."), true)
  let version_parts = service_version.split(".")
  assert_eq(version_parts.length(), 3)
  assert_eq(version_parts[0], "2")
  assert_eq(version_parts[1], "1")
  assert_eq(version_parts[2], "3")
  
  // 验证服务命名空间属性
  assert_eq(service_namespace, "security")
  assert_eq(service_namespace.length(), 8)
  
  // 验证服务实例ID属性
  assert_eq(service_instance_id.has_prefix("auth-service"), true)
  assert_eq(service_instance_id.contains("-pod-"), true)
  assert_eq(service_instance_id.has_suffix("12345"), true)
  
  // 创建服务资源属性集合
  let service_attributes = [
    ("service.name", service_name),
    ("service.version", service_version),
    ("service.namespace", service_namespace),
    ("service.instance.id", service_instance_id)
  ]
  
  // 验证服务资源属性集合
  assert_eq(service_attributes.length(), 4)
  assert_eq(service_attributes[0].0, "service.name")
  assert_eq(service_attributes[0].1, service_name)
  assert_eq(service_attributes[3].0, "service.instance.id")
  assert_eq(service_attributes[3].1, service_instance_id)
}

test "telemetry_sdk_resource_attributes_test" {
  // 测试遥测SDK资源属性
  
  let sdk_name = "azimuth"
  let sdk_version = "0.1.0"
  let sdk_language = "moonbit"
  let sdk_semconv = "1.21.0"
  
  // 验证SDK名称属性
  assert_eq(sdk_name, "azimuth")
  assert_eq(sdk_name.length(), 7)
  
  // 验证SDK版本属性
  assert_eq(sdk_version, "0.1.0")
  assert_eq(sdk_version.contains("."), true)
  
  // 验证SDK语言属性
  assert_eq(sdk_language, "moonbit")
  assert_eq(sdk_language.length(), 7)
  
  // 验证语义约定版本属性
  assert_eq(sdk_semconv, "1.21.0")
  assert_eq(sdk_semconv.contains("."), true)
  
  // 创建SDK资源属性集合
  let sdk_attributes = [
    ("telemetry.sdk.name", sdk_name),
    ("telemetry.sdk.version", sdk_version),
    ("telemetry.sdk.language", sdk_language),
    ("telemetry.auto.version", sdk_semconv)
  ]
  
  // 验证SDK资源属性集合
  assert_eq(sdk_attributes.length(), 4)
  for attr in sdk_attributes {
    assert_eq(attr.0.has_prefix("telemetry"), true)
    assert_eq(attr.0.contains("."), true)
  }
}

test "host_resource_attributes_test" {
  // 测试主机资源属性
  
  let host_name = "prod-web-server-01"
  let host_id = "host-abc123def456"
  let host_type = "virtual_machine"
  let host_arch = "x86_64"
  
  // 验证主机名称属性
  assert_eq(host_name.has_prefix("prod"), true)
  assert_eq(host_name.contains("-web-"), true)
  assert_eq(host_name.has_suffix("-01"), true)
  
  // 验证主机ID属性
  assert_eq(host_id.has_prefix("host-"), true)
  assert_eq(host_id.length(), 15)
  
  // 验证主机类型属性
  assert_eq(host_type, "virtual_machine")
  assert_eq(host_type.contains("_"), true)
  
  // 验证主机架构属性
  assert_eq(host_arch, "x86_64")
  assert_eq(host_arch.contains("_"), true)
  
  // 创建主机资源属性集合
  let host_attributes = [
    ("host.name", host_name),
    ("host.id", host_id),
    ("host.type", host_type),
    ("host.arch", host_arch)
  ]
  
  // 验证主机资源属性集合
  assert_eq(host_attributes.length(), 4)
  for attr in host_attributes {
    assert_eq(attr.0.has_prefix("host"), true)
  }
}

test "kubernetes_resource_attributes_test" {
  // 测试Kubernetes资源属性
  
  let k8s_pod_name = "payment-service-7d4f8c9b5-abc12"
  let k8s_namespace = "production"
  let k8s_deployment_name = "payment-service"
  let k8s_node_name = "worker-node-03"
  
  // 验证Pod名称属性
  assert_eq(k8s_pod_name.has_prefix("payment-service"), true)
  assert_eq(k8s_pod_name.contains("-"), true)
  assert_eq(k8s_pod_name.length(), 27)
  
  // 验证命名空间属性
  assert_eq(k8s_namespace, "production")
  assert_eq(k8s_namespace.length(), 10)
  
  // 验证部署名称属性
  assert_eq(k8s_deployment_name, "payment-service")
  assert_eq(k8s_deployment_name.contains("-"), true)
  
  // 验证节点名称属性
  assert_eq(k8s_node_name.has_prefix("worker-node"), true)
  assert_eq(k8s_node_name.has_suffix("-03"), true)
  
  // 创建Kubernetes资源属性集合
  let k8s_attributes = [
    ("k8s.pod.name", k8s_pod_name),
    ("k8s.namespace.name", k8s_namespace),
    ("k8s.deployment.name", k8s_deployment_name),
    ("k8s.node.name", k8s_node_name)
  ]
  
  // 验证Kubernetes资源属性集合
  assert_eq(k8s_attributes.length(), 4)
  for attr in k8s_attributes {
    assert_eq(attr.0.has_prefix("k8s"), true)
    assert_eq(attr.0.contains("."), true)
  }
}

test "cloud_resource_attributes_test" {
  // 测试云资源属性
  
  let cloud_provider = "aws"
  let cloud_account_id = "123456789012"
  let cloud_region = "us-west-2"
  let cloud_availability_zone = "us-west-2a"
  
  // 验证云提供商属性
  assert_eq(cloud_provider, "aws")
  assert_eq(cloud_provider.length(), 3)
  
  // 验证云账户ID属性
  assert_eq(cloud_account_id, "123456789012")
  assert_eq(cloud_account_id.length(), 12)
  assert_eq(cloud_account_id.contains_only_digits(), true)
  
  // 验证云区域属性
  assert_eq(cloud_region, "us-west-2")
  assert_eq(cloud_region.contains("-"), true)
  
  // 验证云可用区属性
  assert_eq(cloud_availability_zone, "us-west-2a")
  assert_eq(cloud_availability_zone.has_prefix(cloud_region), true)
  
  // 创建云资源属性集合
  let cloud_attributes = [
    ("cloud.provider", cloud_provider),
    ("cloud.account.id", cloud_account_id),
    ("cloud.region", cloud_region),
    ("cloud.availability_zone", cloud_availability_zone)
  ]
  
  // 验证云资源属性集合
  assert_eq(cloud_attributes.length(), 4)
  for attr in cloud_attributes {
    assert_eq(attr.0.has_prefix("cloud"), true)
    assert_eq(attr.0.contains("."), true)
  }
}

test "process_resource_attributes_test" {
  // 测试进程资源属性
  
  let process_pid = 12345
  let process_executable_name = "payment-service"
  let process_command = "/usr/local/bin/payment-service"
  let process_command_args = ["--config", "/etc/payment/config.yaml", "--port", "8080"]
  
  // 验证进程PID属性
  assert_eq(process_pid > 0, true)
  assert_eq(process_pid < 65536, true)
  
  // 验证进程可执行文件名属性
  assert_eq(process_executable_name, "payment-service")
  assert_eq(process_executable_name.contains("-"), true)
  
  // 验证进程命令属性
  assert_eq(process_command.has_prefix("/"), true)
  assert_eq(process_command.contains(process_executable_name), true)
  
  // 验证进程命令参数属性
  assert_eq(process_command_args.length(), 4)
  assert_eq(process_command_args[0], "--config")
  assert_eq(process_command_args[1], "/etc/payment/config.yaml")
  assert_eq(process_command_args[2], "--port")
  assert_eq(process_command_args[3], "8080")
  
  // 创建进程资源属性集合
  let process_attributes = [
    ("process.pid", process_pid.to_string()),
    ("process.executable.name", process_executable_name),
    ("process.command", process_command),
    ("process.command_args", process_command_args.join(" "))
  ]
  
  // 验证进程资源属性集合
  assert_eq(process_attributes.length(), 4)
  for attr in process_attributes {
    assert_eq(attr.0.has_prefix("process"), true)
    assert_eq(attr.0.contains("."), true)
  }
}

test "custom_resource_attributes_test" {
  // 测试自定义资源属性
  
  let application_name = "azimuth-telemetry-demo"
  let application_version = "1.0.0"
  let environment = "staging"
  let team = "platform-engineering"
  let cost_center = "engineering-123"
  
  // 验证应用名称属性
  assert_eq(application_name.has_prefix("azimuth"), true)
  assert_eq(application_name.contains("-"), true)
  
  // 验证应用版本属性
  assert_eq(application_version, "1.0.0")
  assert_eq(application_version.contains("."), true)
  
  // 验证环境属性
  assert_eq(environment, "staging")
  assert_eq(environment.length(), 7)
  
  // 验证团队属性
  assert_eq(team, "platform-engineering")
  assert_eq(team.contains("-"), true)
  
  // 验证成本中心属性
  assert_eq(cost_center, "engineering-123")
  assert_eq(cost_center.contains("-"), true)
  
  // 创建自定义资源属性集合
  let custom_attributes = [
    ("application.name", application_name),
    ("application.version", application_version),
    ("deployment.environment", environment),
    ("team.name", team),
    ("cost.center", cost_center)
  ]
  
  // 验证自定义资源属性集合
  assert_eq(custom_attributes.length(), 5)
  
  // 验证属性值格式
  for attr in custom_attributes {
    assert_eq(attr.0.contains("."), true)
    assert_eq(attr.1.length() > 0, true)
  }
}

test "resource_attributes_validation_test" {
  // 测试资源属性验证
  
  let valid_attributes = [
    ("service.name", "valid-service-name"),
    ("service.version", "1.0.0"),
    ("host.name", "valid-host-name"),
    ("cloud.provider", "aws")
  ]
  
  let invalid_attributes = [
    ("service.name", ""), // 空值
    ("service.version", "not.a.version"), // 无效版本格式
    ("host.name", "a".repeat(300)), // 过长的值
    ("cloud.provider", "invalid-provider-with-special-chars!") // 包含特殊字符
  ]
  
  // 验证有效属性
  for attr in valid_attributes {
    assert_eq(attr.1.length() > 0, true)
    assert_eq(attr.1.length() < 256, true)
  }
  
  // 验证无效属性
  assert_eq(invalid_attributes[0].1.length(), 0) // 空值
  assert_eq(invalid_attributes[1].1.contains("not.a.version"), true) // 无效版本
  assert_eq(invalid_attributes[2].1.length() > 255, true) // 过长
  assert_eq(invalid_attributes[3].1.contains("!"), true) // 特殊字符
  
  // 创建属性验证结果
  let mut validation_results = []
  
  for attr in valid_attributes {
    let result = attr.0 + ":valid"
    validation_results.push(result)
  }
  
  for attr in invalid_attributes {
    let result = attr.0 + ":invalid"
    validation_results.push(result)
  }
  
  // 验证验证结果
  assert_eq(validation_results.length(), 8)
  assert_eq(validation_results[0], "service.name:valid")
  assert_eq(validation_results[4], "service.name:invalid")
}