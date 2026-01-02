// Azimuth Resource Management Tests
// 测试资源管理功能

test "resource creation with basic attributes" {
  // 创建基础资源
  let basic_resource = @azimuth.Resource {
    attributes : [
      ("service.name", @azimuth.StringValue("payment-service")),
      ("service.version", @azimuth.StringValue("1.0.0"))
    ]
  }
  
  // 验证资源属性数量
  assert_eq(basic_resource.attributes.length(), 2)
  
  // 验证服务名称
  let service_name = basic_resource.attributes[0]
  assert_eq(service_name.0, "service.name")
  match service_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  // 验证服务版本
  let service_version = basic_resource.attributes[1]
  assert_eq(service_version.0, "service.version")
  match service_version.1 {
    @azimuth.StringValue(v) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
}

test "resource with host and process attributes" {
  // 创建包含主机和进程信息的资源
  let host_resource = @azimuth.Resource {
    attributes : [
      ("host.name", @azimuth.StringValue("prod-server-01")),
      ("host.arch", @azimuth.StringValue("amd64")),
      ("os.type", @azimuth.StringValue("linux")),
      ("os.version", @azimuth.StringValue("5.15.0-76-generic")),
      ("process.pid", @azimuth.IntValue(12345)),
      ("process.executable.name", @azimuth.StringValue("payment-service")),
      ("process.command_args", @azimuth.ArrayStringValue(["./payment-service", "--port=8080"]))
    ]
  }
  
  // 验证属性数量
  assert_eq(host_resource.attributes.length(), 7)
  
  // 验证主机名
  let host_name = host_resource.attributes[0]
  match host_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "prod-server-01")
    _ => assert_true(false)
  }
  
  // 验证进程ID
  let process_pid = host_resource.attributes[4]
  match process_pid.1 {
    @azimuth.IntValue(v) => assert_eq(v, 12345)
    _ => assert_true(false)
  }
  
  // 验证命令参数
  let command_args = host_resource.attributes[6]
  match command_args.1 {
    @azimuth.ArrayStringValue(arr) => {
      assert_eq(arr.length(), 2)
      assert_eq(arr[0], "./payment-service")
      assert_eq(arr[1], "--port=8080")
    }
    _ => assert_true(false)
  }
}

test "resource with cloud provider attributes" {
  // 创建云环境资源
  let cloud_resource = @azimuth.Resource {
    attributes : [
      ("cloud.provider", @azimuth.StringValue("aws")),
      ("cloud.account.id", @azimuth.StringValue("123456789012")),
      ("cloud.region", @azimuth.StringValue("us-west-2")),
      ("cloud.availability_zone", @azimuth.StringValue("us-west-2a")),
      ("cloud.platform", @azimuth.StringValue("aws_ec2")),
      ("aws.ec2.instance.id", @azimuth.StringValue("i-1234567890abcdef0")),
      ("aws.ec2.instance.type", @azimuth.StringValue("t3.medium"))
    ]
  }
  
  // 验证云提供商属性
  let cloud_provider = cloud_resource.attributes[0]
  match cloud_provider.1 {
    @azimuth.StringValue(v) => assert_eq(v, "aws")
    _ => assert_true(false)
  }
  
  // 验证云账户ID
  let cloud_account = cloud_resource.attributes[1]
  match cloud_account.1 {
    @azimuth.StringValue(v) => assert_eq(v, "123456789012")
    _ => assert_true(false)
  }
  
  // 验证区域
  let cloud_region = cloud_resource.attributes[2]
  match cloud_region.1 {
    @azimuth.StringValue(v) => assert_eq(v, "us-west-2")
    _ => assert_true(false)
  }
  
  // 验证实例ID
  let instance_id = cloud_resource.attributes[5]
  match instance_id.1 {
    @azimuth.StringValue(v) => assert_eq(v, "i-1234567890abcdef0")
    _ => assert_true(false)
  }
}

test "resource with container attributes" {
  // 创建容器环境资源
  let container_resource = @azimuth.Resource {
    attributes : [
      ("container.name", @azimuth.StringValue("payment-service-container")),
      ("container.id", @azimuth.StringValue("abcdef123456")),
      ("container.image.name", @azimuth.StringValue("payment-service:1.0.0")),
      ("container.image.tag", @azimuth.StringValue("1.0.0")),
      ("container.runtime", @azimuth.StringValue("docker")),
      ("k8s.pod.name", @azimuth.StringValue("payment-service-7d4f8c9b5-xyz12")),
      ("k8s.namespace.name", @azimuth.StringValue("production")),
      ("k8s.deployment.name", @azimuth.StringValue("payment-service"))
    ]
  }
  
  // 验证容器名称
  let container_name = container_resource.attributes[0]
  match container_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "payment-service-container")
    _ => assert_true(false)
  }
  
  // 验证容器ID
  let container_id = container_resource.attributes[1]
  match container_id.1 {
    @azimuth.StringValue(v) => assert_eq(v, "abcdef123456")
    _ => assert_true(false)
  }
  
  // 验证Kubernetes Pod名称
  let k8s_pod_name = container_resource.attributes[5]
  match k8s_pod_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "payment-service-7d4f8c9b5-xyz12")
    _ => assert_true(false)
  }
  
  // 验证Kubernetes命名空间
  let k8s_namespace = container_resource.attributes[6]
  match k8s_namespace.1 {
    @azimuth.StringValue(v) => assert_eq(v, "production")
    _ => assert_true(false)
  }
}

test "resource with application attributes" {
  // 创建应用程序特定资源
  let app_resource = @azimuth.Resource {
    attributes : [
      ("service.name", @azimuth.StringValue("order-service")),
      ("service.namespace", @azimuth.StringValue("ecommerce")),
      ("service.instance.id", @azimuth.StringValue("instance-abc123")),
      ("service.version", @azimuth.StringValue("2.1.0")),
      ("telemetry.sdk.name", @azimuth.StringValue("azimuth")),
      ("telemetry.sdk.language", @azimuth.StringValue("moonbit")),
      ("telemetry.sdk.version", @azimuth.StringValue("0.1.0")),
      ("telemetry.auto.version", @azimuth.StringValue("0.1.0"))
    ]
  }
  
  // 验证服务名称
  let service_name = app_resource.attributes[0]
  match service_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "order-service")
    _ => assert_true(false)
  }
  
  // 验证服务命名空间
  let service_namespace = app_resource.attributes[1]
  match service_namespace.1 {
    @azimuth.StringValue(v) => assert_eq(v, "ecommerce")
    _ => assert_true(false)
  }
  
  // 验证服务实例ID
  let service_instance_id = app_resource.attributes[2]
  match service_instance_id.1 {
    @azimuth.StringValue(v) => assert_eq(v, "instance-abc123")
    _ => assert_true(false)
  }
  
  // 验证遥测SDK名称
  let telemetry_sdk_name = app_resource.attributes[4]
  match telemetry_sdk_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "azimuth")
    _ => assert_true(false)
  }
  
  // 验证遥测SDK语言
  let telemetry_sdk_language = app_resource.attributes[5]
  match telemetry_sdk_language.1 {
    @azimuth.StringValue(v) => assert_eq(v, "moonbit")
    _ => assert_true(false)
  }
}

test "resource attribute merging" {
  // 创建基础资源
  let base_resource = @azimuth.Resource {
    attributes : [
      ("service.name", @azimuth.StringValue("base-service")),
      ("service.version", @azimuth.StringValue("1.0.0")),
      ("host.name", @azimuth.StringValue("localhost"))
    ]
  }
  
  // 创建扩展资源
  let extended_resource = @azimuth.Resource {
    attributes : base_resource.attributes + [
      ("service.instance.id", @azimuth.StringValue("instance-123")),
      ("process.pid", @azimuth.IntValue(5678)),
      ("cloud.provider", @azimuth.StringValue("aws")),
      ("cloud.region", @azimuth.StringValue("us-east-1"))
    ]
  }
  
  // 验证合并后的资源属性数量
  assert_eq(extended_resource.attributes.length(), 7)
  
  // 验证原有属性保持不变
  assert_eq(extended_resource.attributes[0], base_resource.attributes[0])
  assert_eq(extended_resource.attributes[1], base_resource.attributes[1])
  assert_eq(extended_resource.attributes[2], base_resource.attributes[2])
  
  // 验证新增属性
  let service_instance_id = extended_resource.attributes[3]
  match service_instance_id.1 {
    @azimuth.StringValue(v) => assert_eq(v, "instance-123")
    _ => assert_true(false)
  }
  
  let process_pid = extended_resource.attributes[4]
  match process_pid.1 {
    @azimuth.IntValue(v) => assert_eq(v, 5678)
    _ => assert_true(false)
  }
}

test "resource attribute override behavior" {
  // 创建原始资源
  let original_resource = @azimuth.Resource {
    attributes : [
      ("service.name", @azimuth.StringValue("original-service")),
      ("service.version", @azimuth.StringValue("1.0.0")),
      ("environment", @azimuth.StringValue("development"))
    ]
  }
  
  // 创建覆盖资源（相同键名，不同值）
  let override_resource = @azimuth.Resource {
    attributes : [
      ("service.name", @azimuth.StringValue("override-service")),
      ("service.version", @azimuth.StringValue("2.0.0")),
      ("environment", @azimuth.StringValue("production"))
    ]
  }
  
  // 验证覆盖后的值
  let override_service_name = override_resource.attributes[0]
  match override_service_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "override-service")
    _ => assert_true(false)
  }
  
  let override_service_version = override_resource.attributes[1]
  match override_service_version.1 {
    @azimuth.StringValue(v) => assert_eq(v, "2.0.0")
    _ => assert_true(false)
  }
  
  let override_environment = override_resource.attributes[2]
  match override_environment.1 {
    @azimuth.StringValue(v) => assert_eq(v, "production")
    _ => assert_true(false)
  }
  
  // 确保原始资源不受影响
  let original_service_name = original_resource.attributes[0]
  match original_service_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "original-service")
    _ => assert_true(false)
  }
}

test "empty resource handling" {
  // 创建空资源
  let empty_resource = @azimuth.Resource { attributes : [] }
  
  // 验证空资源属性数量
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 向空资源添加属性
  let populated_resource = @azimuth.Resource {
    attributes : [
      ("service.name", @azimuth.StringValue("new-service")),
      ("service.version", @azimuth.StringValue("1.0.0"))
    ]
  }
  
  // 验证添加后的属性
  assert_eq(populated_resource.attributes.length(), 2)
  assert_eq(populated_resource.attributes[0].0, "service.name")
  assert_eq(populated_resource.attributes[1].0, "service.version")
}

test "resource attribute validation" {
  // 创建包含各种属性类型的资源
  let validation_resource = @azimuth.Resource {
    attributes : [
      ("string.attr", @azimuth.StringValue("string value")),
      ("int.attr", @azimuth.IntValue(42)),
      ("float.attr", @azimuth.FloatValue(3.14)),
      ("bool.attr", @azimuth.BoolValue(true)),
      ("string.array.attr", @azimuth.ArrayStringValue(["item1", "item2"])),
      ("int.array.attr", @azimuth.ArrayIntValue([1, 2, 3]))
    ]
  }
  
  // 验证每种属性类型
  let mut string_count = 0
  let mut int_count = 0
  let mut float_count = 0
  let mut bool_count = 0
  let mut string_array_count = 0
  let mut int_array_count = 0
  
  for attr in validation_resource.attributes {
    match attr.1 {
      @azimuth.StringValue(_) => string_count = string_count + 1
      @azimuth.IntValue(_) => int_count = int_count + 1
      @azimuth.FloatValue(_) => float_count = float_count + 1
      @azimuth.BoolValue(_) => bool_count = bool_count + 1
      @azimuth.ArrayStringValue(_) => string_array_count = string_array_count + 1
      @azimuth.ArrayIntValue(_) => int_array_count = int_array_count + 1
    }
  }
  
  assert_eq(string_count, 1)
  assert_eq(int_count, 1)
  assert_eq(float_count, 1)
  assert_eq(bool_count, 1)
  assert_eq(string_array_count, 1)
  assert_eq(int_array_count, 1)
}