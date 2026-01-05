// Azimuth Premium Resource Management Tests
// 测试资源属性管理和资源合并策略

test "basic resource creation and attribute access" {
  // 测试基本资源创建和属性访问
  let empty_resource = Resource::new()
  
  // 验证空资源
  assert_eq(Resource::get_attribute(empty_resource, "any.key"), None)
  
  // 创建带有属性的资源
  let attributes = [
    ("service.name", StringValue("auth-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource = Resource::with_attributes(empty_resource, attributes)
  
  // 验证属性访问
  assert_eq(Resource::get_attribute(resource, "service.name"), Some(StringValue("auth-service")))
  assert_eq(Resource::get_attribute(resource, "service.version"), Some(StringValue("1.2.3")))
  assert_eq(Resource::get_attribute(resource, "service.instance.id"), Some(StringValue("instance-123")))
  assert_eq(Resource::get_attribute(resource, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(resource, "nonexistent.key"), None)
}

test "resource with different attribute types" {
  // 测试包含不同类型属性的资源
  let empty_resource = Resource::new()
  
  // 创建混合类型的属性
  let mixed_attributes = [
    ("string.attr", StringValue("string value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("string.array", ArrayStringValue(["item1", "item2", "item3"])),
    ("int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  let resource = Resource::with_attributes(empty_resource, mixed_attributes)
  
  // 验证不同类型的属性访问
  assert_eq(Resource::get_attribute(resource, "string.attr"), Some(StringValue("string value")))
  assert_eq(Resource::get_attribute(resource, "int.attr"), Some(IntValue(42)))
  assert_eq(Resource::get_attribute(resource, "float.attr"), Some(FloatValue(3.14159)))
  assert_eq(Resource::get_attribute(resource, "bool.attr"), Some(BoolValue(true)))
  assert_eq(Resource::get_attribute(resource, "string.array"), Some(ArrayStringValue(["item1", "item2", "item3"])))
  assert_eq(Resource::get_attribute(resource, "int.array"), Some(ArrayIntValue([1, 2, 3, 4, 5])))
}

test "resource merge strategy" {
  // 测试资源合并策略
  let base_resource = Resource::new()
  let base_attributes = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("common.attr", StringValue("base-value")),
    ("base.only", StringValue("base-only-value"))
  ]
  let base = Resource::with_attributes(base_resource, base_attributes)
  
  let override_resource = Resource::new()
  let override_attributes = [
    ("service.name", StringValue("override-service")),  // 覆盖基资源属性
    ("service.version", StringValue("2.0.0")),          // 覆盖基资源属性
    ("common.attr", StringValue("override-value")),     // 覆盖基资源属性
    ("override.only", StringValue("override-only-value"))
  ]
  let override_res = Resource::with_attributes(override_resource, override_attributes)
  
  // 合并资源（简化实现中直接返回override资源）
  let merged = Resource::merge(base, override_res)
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged, "service.name"), Some(StringValue("override-service")))
  assert_eq(Resource::get_attribute(merged, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged, "common.attr"), Some(StringValue("override-value")))
  assert_eq(Resource::get_attribute(merged, "override.only"), Some(StringValue("override-only-value")))
}

test "resource with host and process information" {
  // 测试包含主机和进程信息的资源
  let empty_resource = Resource::new()
  
  // 主机信息
  let host_attributes = [
    ("host.name", StringValue("web-server-01")),
    ("host.id", StringValue("host-abc-123")),
    ("host.type", StringValue("virtual_machine")),
    ("host.image.name", StringValue("ubuntu-20.04")),
    ("host.image.id", StringValue("ami-12345678")),
    ("host.arch", StringValue("x86_64"))
  ]
  
  // 进程信息
  let process_attributes = [
    ("process.pid", IntValue(1234)),
    ("process.executable.name", StringValue("auth-service")),
    ("process.executable.path", StringValue("/opt/services/auth-service")),
    ("process.command", StringValue("/opt/services/auth-service --port=8080")),
    ("process.command_line", ArrayStringValue(["/opt/services/auth-service", "--port=8080"])),
    ("process.runtime.name", StringValue("nodejs")),
    ("process.runtime.version", StringValue("18.17.0"))
  ]
  
  // 组合所有属性
  let all_attributes = host_attributes + process_attributes
  let resource = Resource::with_attributes(empty_resource, all_attributes)
  
  // 验证主机信息
  assert_eq(Resource::get_attribute(resource, "host.name"), Some(StringValue("web-server-01")))
  assert_eq(Resource::get_attribute(resource, "host.id"), Some(StringValue("host-abc-123")))
  assert_eq(Resource::get_attribute(resource, "host.type"), Some(StringValue("virtual_machine")))
  assert_eq(Resource::get_attribute(resource, "host.arch"), Some(StringValue("x86_64")))
  
  // 验证进程信息
  assert_eq(Resource::get_attribute(resource, "process.pid"), Some(IntValue(1234)))
  assert_eq(Resource::get_attribute(resource, "process.executable.name"), Some(StringValue("auth-service")))
  assert_eq(Resource::get_attribute(resource, "process.runtime.name"), Some(StringValue("nodejs")))
  assert_eq(Resource::get_attribute(resource, "process.runtime.version"), Some(StringValue("18.17.0")))
}

test "resource with cloud provider information" {
  // 测试包含云提供商信息的资源
  let empty_resource = Resource::new()
  
  // AWS云信息
  let aws_attributes = [
    ("cloud.provider", StringValue("aws")),
    ("cloud.account.id", StringValue("123456789012")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.availability_zone", StringValue("us-west-2a")),
    ("cloud.platform", StringValue("aws_ec2")),
    ("cloud.instance.id", StringValue("i-1234567890abcdef0")),
    ("cloud.instance.type", StringValue("t3.medium")),
    ("cloud.image.id", StringValue("ami-1234567890abcdef0"))
  ]
  
  let resource = Resource::with_attributes(empty_resource, aws_attributes)
  
  // 验证云信息
  assert_eq(Resource::get_attribute(resource, "cloud.provider"), Some(StringValue("aws")))
  assert_eq(Resource::get_attribute(resource, "cloud.account.id"), Some(StringValue("123456789012")))
  assert_eq(Resource::get_attribute(resource, "cloud.region"), Some(StringValue("us-west-2")))
  assert_eq(Resource::get_attribute(resource, "cloud.platform"), Some(StringValue("aws_ec2")))
  assert_eq(Resource::get_attribute(resource, "cloud.instance.id"), Some(StringValue("i-1234567890abcdef0")))
}

test "resource with container information" {
  // 测试包含容器信息的资源
  let empty_resource = Resource::new()
  
  // 容器信息
  let container_attributes = [
    ("container.name", StringValue("auth-service-container")),
    ("container.id", StringValue("container-abc-123")),
    ("container.image.name", StringValue("mycompany/auth-service")),
    ("container.image.tag", StringValue("v1.2.3")),
    ("container.image.id", StringValue("sha256:1234567890abcdef")),
    ("container.runtime", StringValue("docker"))
  ]
  
  // Kubernetes信息
  let k8s_attributes = [
    ("k8s.pod.name", StringValue("auth-service-7d4f8c9b5-xyz12")),
    ("k8s.pod.uid", StringValue("12345678-1234-1234-1234-123456789012")),
    ("k8s.namespace.name", StringValue("production")),
    ("k8s.deployment.name", StringValue("auth-service")),
    ("k8s.replicaset.name", StringValue("auth-service-7d4f8c9b5")),
    ("k8s.node.name", StringValue("worker-node-01")),
    ("k8s.service.name", StringValue("auth-service"))
  ]
  
  // 组合所有属性
  let all_attributes = container_attributes + k8s_attributes
  let resource = Resource::with_attributes(empty_resource, all_attributes)
  
  // 验证容器信息
  assert_eq(Resource::get_attribute(resource, "container.name"), Some(StringValue("auth-service-container")))
  assert_eq(Resource::get_attribute(resource, "container.id"), Some(StringValue("container-abc-123")))
  assert_eq(Resource::get_attribute(resource, "container.runtime"), Some(StringValue("docker")))
  
  // 验证Kubernetes信息
  assert_eq(Resource::get_attribute(resource, "k8s.pod.name"), Some(StringValue("auth-service-7d4f8c9b5-xyz12")))
  assert_eq(Resource::get_attribute(resource, "k8s.namespace.name"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(resource, "k8s.deployment.name"), Some(StringValue("auth-service")))
}

test "resource with telemetry sdk information" {
  // 测试包含遥测SDK信息的资源
  let empty_resource = Resource::new()
  
  // SDK信息
  let sdk_attributes = [
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.language", StringValue("moonbit")),
    ("telemetry.sdk.version", StringValue("0.1.0")),
    ("telemetry.auto.version", StringValue("0.1.0"))
  ]
  
  let resource = Resource::with_attributes(empty_resource, sdk_attributes)
  
  // 验证SDK信息
  assert_eq(Resource::get_attribute(resource, "telemetry.sdk.name"), Some(StringValue("azimuth")))
  assert_eq(Resource::get_attribute(resource, "telemetry.sdk.language"), Some(StringValue("moonbit")))
  assert_eq(Resource::get_attribute(resource, "telemetry.sdk.version"), Some(StringValue("0.1.0")))
}

test "resource with web engine information" {
  // 测试包含Web引擎信息的资源
  let empty_resource = Resource::new()
  
  // Web引擎信息
  let webengine_attributes = [
    ("webengine.name", StringValue("express")),
    ("webengine.version", StringValue("4.18.2")),
    ("webengine.description", StringValue("Express.js web framework"))
  ]
  
  let resource = Resource::with_attributes(empty_resource, webengine_attributes)
  
  // 验证Web引擎信息
  assert_eq(Resource::get_attribute(resource, "webengine.name"), Some(StringValue("express")))
  assert_eq(Resource::get_attribute(resource, "webengine.version"), Some(StringValue("4.18.2")))
  assert_eq(Resource::get_attribute(resource, "webengine.description"), Some(StringValue("Express.js web framework")))
}

test "resource with operating system information" {
  // 测试包含操作系统信息的资源
  let empty_resource = Resource::new()
  
  // 操作系统信息
  let os_attributes = [
    ("os.type", StringValue("linux")),
    ("os.description", StringValue("Ubuntu 20.04.3 LTS")),
    ("os.name", StringValue("Ubuntu")),
    ("os.version", StringValue("20.04.3")),
    ("os.kernel", StringValue("5.13.0-30-generic"))
  ]
  
  let resource = Resource::with_attributes(empty_resource, os_attributes)
  
  // 验证操作系统信息
  assert_eq(Resource::get_attribute(resource, "os.type"), Some(StringValue("linux")))
  assert_eq(Resource::get_attribute(resource, "os.description"), Some(StringValue("Ubuntu 20.04.3 LTS")))
  assert_eq(Resource::get_attribute(resource, "os.name"), Some(StringValue("Ubuntu")))
  assert_eq(Resource::get_attribute(resource, "os.version"), Some(StringValue("20.04.3")))
}

test "resource with device information" {
  // 测试包含设备信息的资源
  let empty_resource = Resource::new()
  
  // 设备信息
  let device_attributes = [
    ("device.id", StringValue("device-12345")),
    ("device.model.identifier", StringValue("iPhone13,4")),
    ("device.model.name", StringValue("iPhone 13 Pro Max")),
    ("device.manufacturer", StringValue("Apple"))
  ]
  
  let resource = Resource::with_attributes(empty_resource, device_attributes)
  
  // 验证设备信息
  assert_eq(Resource::get_attribute(resource, "device.id"), Some(StringValue("device-12345")))
  assert_eq(Resource::get_attribute(resource, "device.model.identifier"), Some(StringValue("iPhone13,4")))
  assert_eq(Resource::get_attribute(resource, "device.model.name"), Some(StringValue("iPhone 13 Pro Max")))
  assert_eq(Resource::get_attribute(resource, "device.manufacturer"), Some(StringValue("Apple")))
}

test "comprehensive resource with all attributes" {
  // 测试包含所有类型属性的综合资源
  let empty_resource = Resource::new()
  
  // 创建包含所有类型属性的数组
  let comprehensive_attributes = [
    // 服务信息
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("payment-789")),
    
    // 主机信息
    ("host.name", StringValue("payment-server-01")),
    ("host.arch", StringValue("x86_64")),
    
    // 进程信息
    ("process.pid", IntValue(5678)),
    ("process.runtime.name", StringValue("java")),
    ("process.runtime.version", StringValue("17.0.2")),
    
    // 云信息
    ("cloud.provider", StringValue("gcp")),
    ("cloud.region", StringValue("us-central1")),
    
    // 容器信息
    ("container.id", StringValue("container-payment-123")),
    ("container.runtime", StringValue("containerd")),
    
    // Kubernetes信息
    ("k8s.pod.name", StringValue("payment-service-abc123")),
    ("k8s.namespace.name", StringValue("production")),
    
    // SDK信息
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.language", StringValue("java")),
    
    // 自定义属性
    ("custom.attribute", StringValue("custom-value")),
    ("custom.number", IntValue(999)),
    ("custom.flag", BoolValue(true)),
    ("custom.tags", ArrayStringValue(["payment", "critical", "pci"]))
  ]
  
  let resource = Resource::with_attributes(empty_resource, comprehensive_attributes)
  
  // 验证各种类型的属性
  assert_eq(Resource::get_attribute(resource, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(resource, "host.name"), Some(StringValue("payment-server-01")))
  assert_eq(Resource::get_attribute(resource, "process.pid"), Some(IntValue(5678)))
  assert_eq(Resource::get_attribute(resource, "cloud.provider"), Some(StringValue("gcp")))
  assert_eq(Resource::get_attribute(resource, "container.id"), Some(StringValue("container-payment-123")))
  assert_eq(Resource::get_attribute(resource, "k8s.pod.name"), Some(StringValue("payment-service-abc123")))
  assert_eq(Resource::get_attribute(resource, "telemetry.sdk.name"), Some(StringValue("azimuth")))
  assert_eq(Resource::get_attribute(resource, "custom.attribute"), Some(StringValue("custom-value")))
  assert_eq(Resource::get_attribute(resource, "custom.number"), Some(IntValue(999)))
  assert_eq(Resource::get_attribute(resource, "custom.flag"), Some(BoolValue(true)))
  assert_eq(Resource::get_attribute(resource, "custom.tags"), Some(ArrayStringValue(["payment", "critical", "pci"])))
}