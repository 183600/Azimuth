// Azimuth Resource Management Test Suite
// 测试资源管理功能，包括资源创建、合并、清理和生命周期管理

test "基本资源创建和属性访问" {
  // 创建基本资源
  let resource = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("payment-service")),
      ("service.version", azimuth::AttributeValue::StringValue("2.1.0")),
      ("service.instance.id", azimuth::AttributeValue::StringValue("instance-001"))
    ]
  }
  
  // 验证资源属性数量
  assert_eq(resource.attributes.length(), 3)
  
  // 验证资源属性内容
  let mut found_service_name = false
  let mut found_service_version = false
  let mut found_instance_id = false
  
  for (key, value) in resource.attributes {
    match key {
      "service.name" => {
        found_service_name = true
        match value {
          azimuth::AttributeValue::StringValue(name) => assert_eq(name, "payment-service")
          _ => assert_true(false)
        }
      }
      "service.version" => {
        found_service_version = true
        match value {
          azimuth::AttributeValue::StringValue(version) => assert_eq(version, "2.1.0")
          _ => assert_true(false)
        }
      }
      "service.instance.id" => {
        found_instance_id = true
        match value {
          azimuth::AttributeValue::StringValue(id) => assert_eq(id, "instance-001")
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  assert_true(found_service_name)
  assert_true(found_service_version)
  assert_true(found_instance_id)
}

test "空资源和默认资源" {
  // 创建空资源
  let empty_resource = azimuth::Resource { attributes: [] }
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 创建默认服务资源
  let default_resource = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("unknown-service")),
      ("service.version", azimuth::AttributeValue::StringValue("0.0.0")),
      ("telemetry.sdk.name", azimuth::AttributeValue::StringValue("azimuth")),
      ("telemetry.sdk.version", azimuth::AttributeValue::StringValue("1.0.0")),
      ("telemetry.sdk.language", azimuth::AttributeValue::StringValue("moonbit"))
    ]
  }
  
  assert_eq(default_resource.attributes.length(), 5)
  
  // 验证SDK属性
  let mut found_sdk_name = false
  let mut found_sdk_version = false
  let mut found_sdk_language = false
  
  for (key, value) in default_resource.attributes {
    match key {
      "telemetry.sdk.name" => {
        found_sdk_name = true
        match value {
          azimuth::AttributeValue::StringValue(name) => assert_eq(name, "azimuth")
          _ => assert_true(false)
        }
      }
      "telemetry.sdk.version" => {
        found_sdk_version = true
        match value {
          azimuth::AttributeValue::StringValue(version) => assert_eq(version, "1.0.0")
          _ => assert_true(false)
        }
      }
      "telemetry.sdk.language" => {
        found_sdk_language = true
        match value {
          azimuth::AttributeValue::StringValue(lang) => assert_eq(lang, "moonbit")
          _ => assert_true(false)
        }
      }
      _ => () // 其他属性
    }
  }
  
  assert_true(found_sdk_name)
  assert_true(found_sdk_version)
  assert_true(found_sdk_language)
}

test "主机资源创建" {
  // 创建主机相关资源
  let host_resource = azimuth::Resource {
    attributes: [
      ("host.name", azimuth::AttributeValue::StringValue("web-server-01")),
      ("host.id", azimuth::AttributeValue::StringValue("host-abc123")),
      ("host.type", azimuth::AttributeValue::StringValue("virtual-machine")),
      ("host.arch", azimuth::AttributeValue::StringValue("x86_64")),
      ("host.image.name", azimuth::AttributeValue::StringValue("ubuntu-20.04")),
      ("host.image.id", azimuth::AttributeValue::StringValue("ami-12345678")),
      ("host.ip", azimuth::AttributeValue::StringValue("10.0.1.100")),
      ("host.mac", azimuth::AttributeValue::StringValue("00:1b:44:11:3a:b7"))
    ]
  }
  
  assert_eq(host_resource.attributes.length(), 8)
  
  // 验证主机属性
  let mut found_host_name = false
  let mut found_host_ip = false
  let mut found_host_arch = false
  
  for (key, value) in host_resource.attributes {
    match key {
      "host.name" => {
        found_host_name = true
        match value {
          azimuth::AttributeValue::StringValue(name) => assert_eq(name, "web-server-01")
          _ => assert_true(false)
        }
      }
      "host.ip" => {
        found_host_ip = true
        match value {
          azimuth::AttributeValue::StringValue(ip) => assert_eq(ip, "10.0.1.100")
          _ => assert_true(false)
        }
      }
      "host.arch" => {
        found_host_arch = true
        match value {
          azimuth::AttributeValue::StringValue(arch) => assert_eq(arch, "x86_64")
          _ => assert_true(false)
        }
      }
      _ => () // 其他属性
    }
  }
  
  assert_true(found_host_name)
  assert_true(found_host_ip)
  assert_true(found_host_arch)
}

test "容器资源创建" {
  // 创建容器相关资源
  let container_resource = azimuth::Resource {
    attributes: [
      ("container.name", azimuth::AttributeValue::StringValue("payment-api-container")),
      ("container.id", azimuth::AttributeValue::StringValue("container-123456789")),
      ("container.image.name", azimuth::AttributeValue::StringValue("payment-api:latest")),
      ("container.image.tag", azimuth::AttributeValue::StringValue("latest")),
      ("container.runtime", azimuth::AttributeValue::StringValue("docker")),
      ("container.command", azimuth::AttributeValue::StringValue("/usr/bin/payment-api")),
      ("container.args", azimuth::AttributeValue::ArrayStringValue(["--port", "8080", "--config", "/etc/config.yaml"]))
    ]
  }
  
  assert_eq(container_resource.attributes.length(), 7)
  
  // 验证容器属性
  let mut found_container_name = false
  let mut found_container_id = false
  let mut found_container_runtime = false
  let mut found_container_args = false
  
  for (key, value) in container_resource.attributes {
    match key {
      "container.name" => {
        found_container_name = true
        match value {
          azimuth::AttributeValue::StringValue(name) => assert_eq(name, "payment-api-container")
          _ => assert_true(false)
        }
      }
      "container.id" => {
        found_container_id = true
        match value {
          azimuth::AttributeValue::StringValue(id) => assert_eq(id, "container-123456789")
          _ => assert_true(false)
        }
      }
      "container.runtime" => {
        found_container_runtime = true
        match value {
          azimuth::AttributeValue::StringValue(runtime) => assert_eq(runtime, "docker")
          _ => assert_true(false)
        }
      }
      "container.args" => {
        found_container_args = true
        match value {
          azimuth::AttributeValue::ArrayStringValue(args) => {
            assert_eq(args.length(), 4)
            assert_eq(args[0], "--port")
            assert_eq(args[1], "8080")
            assert_eq(args[2], "--config")
            assert_eq(args[3], "/etc/config.yaml")
          }
          _ => assert_true(false)
        }
      }
      _ => () // 其他属性
    }
  }
  
  assert_true(found_container_name)
  assert_true(found_container_id)
  assert_true(found_container_runtime)
  assert_true(found_container_args)
}

test "Kubernetes资源创建" {
  // 创建Kubernetes相关资源
  let k8s_resource = azimuth::Resource {
    attributes: [
      ("k8s.namespace.name", azimuth::AttributeValue::StringValue("production")),
      ("k8s.pod.name", azimuth::AttributeValue::StringValue("payment-api-7d4f8c9b5-abcde")),
      ("k8s.pod.uid", azimuth::AttributeValue::StringValue("12345678-1234-1234-1234-123456789abc")),
      ("k8s.deployment.name", azimuth::AttributeValue::StringValue("payment-api")),
      ("k8s.replicaset.name", azimuth::AttributeValue::StringValue("payment-api-7d4f8c9b5")),
      ("k8s.node.name", azimuth::AttributeValue::StringValue("worker-node-01")),
      ("k8s.service.name", azimuth::AttributeValue::StringValue("payment-api-service")),
      ("k8s.cluster.name", azimuth::AttributeValue::StringValue("prod-cluster"))
    ]
  }
  
  assert_eq(k8s_resource.attributes.length(), 8)
  
  // 验证Kubernetes属性
  let mut found_namespace = false
  let mut found_pod_name = false
  let mut found_deployment = false
  let mut found_cluster = false
  
  for (key, value) in k8s_resource.attributes {
    match key {
      "k8s.namespace.name" => {
        found_namespace = true
        match value {
          azimuth::AttributeValue::StringValue(ns) => assert_eq(ns, "production")
          _ => assert_true(false)
        }
      }
      "k8s.pod.name" => {
        found_pod_name = true
        match value {
          azimuth::AttributeValue::StringValue(pod) => assert_eq(pod, "payment-api-7d4f8c9b5-abcde")
          _ => assert_true(false)
        }
      }
      "k8s.deployment.name" => {
        found_deployment = true
        match value {
          azimuth::AttributeValue::StringValue(deployment) => assert_eq(deployment, "payment-api")
          _ => assert_true(false)
        }
      }
      "k8s.cluster.name" => {
        found_cluster = true
        match value {
          azimuth::AttributeValue::StringValue(cluster) => assert_eq(cluster, "prod-cluster")
          _ => assert_true(false)
        }
      }
      _ => () // 其他属性
    }
  }
  
  assert_true(found_namespace)
  assert_true(found_pod_name)
  assert_true(found_deployment)
  assert_true(found_cluster)
}

test "云平台资源创建" {
  // 创建云平台相关资源
  let cloud_resource = azimuth::Resource {
    attributes: [
      ("cloud.provider", azimuth::AttributeValue::StringValue("aws")),
      ("cloud.account.id", azimuth::AttributeValue::StringValue("123456789012")),
      ("cloud.region", azimuth::AttributeValue::StringValue("us-west-2")),
      ("cloud.availability_zone", azimuth::AttributeValue::StringValue("us-west-2a")),
      ("cloud.platform", azimuth::AttributeValue::StringValue("aws_ec2")),
      ("aws.ec2.instance.id", azimuth::AttributeValue::StringValue("i-1234567890abcdef0")),
      ("aws.ec2.instance.type", azimuth::AttributeValue::StringValue("t3.medium")),
      ("aws.ec2.instance.life_cycle", azimuth::AttributeValue::StringValue("on-demand"))
    ]
  }
  
  assert_eq(cloud_resource.attributes.length(), 8)
  
  // 验证云平台属性
  let mut found_provider = false
  let mut found_region = false
  let mut found_instance_id = false
  let mut found_instance_type = false
  
  for (key, value) in cloud_resource.attributes {
    match key {
      "cloud.provider" => {
        found_provider = true
        match value {
          azimuth::AttributeValue::StringValue(provider) => assert_eq(provider, "aws")
          _ => assert_true(false)
        }
      }
      "cloud.region" => {
        found_region = true
        match value {
          azimuth::AttributeValue::StringValue(region) => assert_eq(region, "us-west-2")
          _ => assert_true(false)
        }
      }
      "aws.ec2.instance.id" => {
        found_instance_id = true
        match value {
          azimuth::AttributeValue::StringValue(id) => assert_eq(id, "i-1234567890abcdef0")
          _ => assert_true(false)
        }
      }
      "aws.ec2.instance.type" => {
        found_instance_type = true
        match value {
          azimuth::AttributeValue::StringValue(type) => assert_eq(type, "t3.medium")
          _ => assert_true(false)
        }
      }
      _ => () // 其他属性
    }
  }
  
  assert_true(found_provider)
  assert_true(found_region)
  assert_true(found_instance_id)
  assert_true(found_instance_type)
}

test "资源合并操作" {
  // 创建第一个资源
  let resource1 = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("user-service")),
      ("service.version", azimuth::AttributeValue::StringValue("1.0.0")),
      ("host.name", azimuth::AttributeValue::StringValue("host-001"))
    ]
  }
  
  // 创建第二个资源
  let resource2 = azimuth::Resource {
    attributes: [
      ("service.version", azimuth::AttributeValue::StringValue("1.1.0")), // 覆盖版本
      ("deployment.environment", azimuth::AttributeValue::StringValue("staging")),
      ("cloud.provider", azimuth::AttributeValue::StringValue("gcp"))
    ]
  }
  
  // 模拟合并操作：第一个资源的属性优先
  let mut merged_attributes = resource1.attributes
  
  // 添加第二个资源中不存在的属性
  for (key2, value2) in resource2.attributes {
    let mut found = false
    for (key1, _) in resource1.attributes {
      if key1 == key2 {
        found = true
        break
      }
    }
    
    if not found {
      merged_attributes = merged_attributes + [(key2, value2)]
    }
  }
  
  let merged_resource = azimuth::Resource { attributes: merged_attributes }
  
  // 验证合并结果
  assert_eq(merged_resource.attributes.length(), 5)
  
  // 验证service.version保持原值
  let mut found_version = false
  for (key, value) in merged_resource.attributes {
    if key == "service.version" {
      found_version = true
      match value {
        azimuth::AttributeValue::StringValue(v) => assert_eq(v, "1.0.0") // 保持原值
        _ => assert_true(false)
      }
    }
  }
  assert_true(found_version)
  
  // 验证新添加的属性
  let mut found_env = false
  let mut found_provider = false
  for (key, value) in merged_resource.attributes {
    if key == "deployment.environment" {
      found_env = true
      match value {
        azimuth::AttributeValue::StringValue(env) => assert_eq(env, "staging")
        _ => assert_true(false)
      }
    } else if key == "cloud.provider" {
      found_provider = true
      match value {
        azimuth::AttributeValue::StringValue(p) => assert_eq(p, "gcp")
        _ => assert_true(false)
      }
    }
  }
  assert_true(found_env)
  assert_true(found_provider)
}

test "资源属性过滤" {
  // 创建完整的资源
  let resource = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("order-service")),
      ("service.version", azimuth::AttributeValue::StringValue("2.1.0")),
      ("host.name", azimuth::AttributeValue::StringValue("order-host-01")),
      ("host.ip", azimuth::AttributeValue::StringValue("10.0.2.100")),
      ("deployment.environment", azimuth::AttributeValue::StringValue("production")),
      ("cloud.provider", azimuth::AttributeValue::StringValue("azure")),
      ("k8s.namespace.name", azimuth::AttributeValue::StringValue("orders")),
      ("k8s.pod.name", azimuth::AttributeValue::StringValue("order-service-7d4f8c9b5-abcde"))
    ]
  }
  
  // 过滤以"service."开头的属性
  let mut service_attrs = []
  for (key, value) in resource.attributes {
    if key.starts_with("service.") {
      service_attrs = service_attrs + [(key, value)]
    }
  }
  
  assert_eq(service_attrs.length(), 2)
  
  // 过滤以"host."开头的属性
  let mut host_attrs = []
  for (key, value) in resource.attributes {
    if key.starts_with("host.") {
      host_attrs = host_attrs + [(key, value)]
    }
  }
  
  assert_eq(host_attrs.length(), 2)
  
  // 过滤以"k8s."开头的属性
  let mut k8s_attrs = []
  for (key, value) in resource.attributes {
    if key.starts_with("k8s.") {
      k8s_attrs = k8s_attrs + [(key, value)]
    }
  }
  
  assert_eq(k8s_attrs.length(), 2)
}

test "资源属性更新" {
  // 创建初始资源
  let mut resource = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("auth-service")),
      ("service.version", azimuth::AttributeValue::StringValue("1.0.0")),
      ("host.name", azimuth::AttributeValue::StringValue("auth-host-01"))
    ]
  }
  
  // 更新service版本
  let mut updated_attributes = []
  for (key, value) in resource.attributes {
    if key == "service.version" {
      updated_attributes = updated_attributes + [("service.version", azimuth::AttributeValue::StringValue("1.1.0"))]
    } else {
      updated_attributes = updated_attributes + [(key, value)]
    }
  }
  
  // 添加新的云属性
  updated_attributes = updated_attributes + [
    ("cloud.provider", azimuth::AttributeValue::StringValue("aws")),
    ("cloud.region", azimuth::AttributeValue::StringValue("us-east-1"))
  ]
  
  resource.attributes = updated_attributes
  
  // 验证更新结果
  assert_eq(resource.attributes.length(), 5)
  
  // 验证版本已更新
  let mut found_updated_version = false
  let mut found_cloud_provider = false
  let mut found_cloud_region = false
  
  for (key, value) in resource.attributes {
    match key {
      "service.version" => {
        match value {
          azimuth::AttributeValue::StringValue(version) => {
            if version == "1.1.0" {
              found_updated_version = true
            }
          }
          _ => assert_true(false)
        }
      }
      "cloud.provider" => {
        found_cloud_provider = true
        match value {
          azimuth::AttributeValue::StringValue(provider) => assert_eq(provider, "aws")
          _ => assert_true(false)
        }
      }
      "cloud.region" => {
        found_cloud_region = true
        match value {
          azimuth::AttributeValue::StringValue(region) => assert_eq(region, "us-east-1")
          _ => assert_true(false)
        }
      }
      _ => () // 其他属性
    }
  }
  
  assert_true(found_updated_version)
  assert_true(found_cloud_provider)
  assert_true(found_cloud_region)
}

test "资源边界条件测试" {
  // 测试空资源
  let empty_resource = azimuth::Resource { attributes: [] }
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 测试包含空值的属性
  let nullish_resource = azimuth::Resource {
    attributes: [
      ("empty.string", azimuth::AttributeValue::StringValue("")),
      ("zero.int", azimuth::AttributeValue::IntValue(0)),
      ("zero.float", azimuth::AttributeValue::FloatValue(0.0)),
      ("false.bool", azimuth::AttributeValue::BoolValue(false))
    ]
  }
  
  // 验证空值属性
  for (key, value) in nullish_resource.attributes {
    match key {
      "empty.string" => {
        match value {
          azimuth::AttributeValue::StringValue(s) => assert_eq(s, "")
          _ => assert_true(false)
        }
      }
      "zero.int" => {
        match value {
          azimuth::AttributeValue::IntValue(i) => assert_eq(i, 0)
          _ => assert_true(false)
        }
      }
      "zero.float" => {
        match value {
          azimuth::AttributeValue::FloatValue(f) => assert_true(f == 0.0)
          _ => assert_true(false)
        }
      }
      "false.bool" => {
        match value {
          azimuth::AttributeValue::BoolValue(b) => assert_false(b)
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试特殊字符和长字符串
  let special_resource = azimuth::Resource {
    attributes: [
      ("special.chars", azimuth::AttributeValue::StringValue("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")),
      ("unicode.chars", azimuth::AttributeValue::StringValue("测试中文🚀emoji")),
      ("very.long.key.name.that.exceeds.normal.expectations.and.tests.boundary.conditions", 
       azimuth::AttributeValue::StringValue("long-key-value"))
    ]
  }
  
  // 验证特殊字符处理
  for (key, value) in special_resource.attributes {
    match value {
      azimuth::AttributeValue::StringValue(s) => {
        assert_true(s.length() > 0)
        // 验证字符串内容不为空
        assert_false(s == "")
      }
      _ => assert_true(false)
    }
  }
}