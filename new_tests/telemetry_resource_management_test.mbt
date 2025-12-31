// 遥测资源管理测试用例

test "resource_basic_creation_and_properties" {
  // 测试资源的基本创建和属性
  
  // 创建默认资源
  let default_resource = azimuth::telemetry::api::common::Resource::default("test-service")
  
  // 验证默认资源属性
  assert_eq(default_resource.service_name, "test-service")
  assert_eq(default_resource.service_version, None)
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.attributes.length(), 0)
  
  // 创建带版本的服务资源
  let versioned_resource = azimuth::telemetry::api::common::Resource::default("api-service")
  let with_version = azimuth::telemetry::api::common::Resource::{
    service_name: versioned_resource.service_name,
    service_version: Some("2.1.0"),
    telemetry_sdk_name: versioned_resource.telemetry_sdk_name,
    telemetry_sdk_version: versioned_resource.telemetry_sdk_version,
    attributes: versioned_resource.attributes
  }
  
  // 验证带版本的资源
  assert_eq(with_version.service_name, "api-service")
  assert_eq(with_version.service_version.unwrap(), "2.1.0")
  assert_eq(with_version.telemetry_sdk_name, "azimuth")
  assert_eq(with_version.telemetry_sdk_version, "0.1.0")
}

test "resource_with_attributes" {
  // 测试带属性的资源
  
  // 创建资源属性
  let resource_attributes = [
    ("service.instance.id", azimuth::telemetry::api::common::AttributeValue::string("instance-12345")),
    ("service.namespace", azimuth::telemetry::api::common::AttributeValue::string("production")),
    ("host.name", azimuth::telemetry::api::common::AttributeValue::string("web-server-01")),
    ("host.arch", azimuth::telemetry::api::common::AttributeValue::string("amd64")),
    ("os.type", azimuth::telemetry::api::common::AttributeValue::string("linux")),
    ("os.version", azimuth::telemetry::api::common::AttributeValue::string("5.15.0")),
    ("deployment.environment", azimuth::telemetry::api::common::AttributeValue::string("prod")),
    ("cloud.provider", azimuth::telemetry::api::common::AttributeValue::string("aws")),
    ("cloud.region", azimuth::telemetry::api::common::AttributeValue::string("us-west-2")),
    ("cloud.availability_zone", azimuth::telemetry::api::common::AttributeValue::string("us-west-2a"))
  ]
  
  // 创建带属性的完整资源
  let full_resource = azimuth::telemetry::api::common::Resource::{
    service_name: "payment-service",
    service_version: Some("1.5.2"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: resource_attributes
  }
  
  // 验证资源属性
  assert_eq(full_resource.service_name, "payment-service")
  assert_eq(full_resource.service_version.unwrap(), "1.5.2")
  assert_eq(full_resource.attributes.length(), 10)
  
  // 验证特定属性
  for attr in full_resource.attributes {
    let (key, value) = attr
    match key {
      "service.instance.id" => {
        match value {
          StringValue(id) => assert_eq(id, "instance-12345")
          _ => assert_eq(false, true)
        }
      }
      "service.namespace" => {
        match value {
          StringValue(ns) => assert_eq(ns, "production")
          _ => assert_eq(false, true)
        }
      }
      "host.name" => {
        match value {
          StringValue(host) => assert_eq(host, "web-server-01")
          _ => assert_eq(false, true)
        }
      }
      "cloud.region" => {
        match value {
          StringValue(region) => assert_eq(region, "us-west-2")
          _ => assert_eq(false, true)
        }
      }
      _ => assert_eq(true, true)  // 其他属性也是有效的
    }
  }
}

test "resource_merging_and_combination" {
  // 测试资源的合并和组合
  
  // 创建基础资源
  let base_resource = azimuth::telemetry::api::common::Resource::default("base-service")
  
  // 创建扩展属性
  let additional_attributes = [
    ("custom.attribute.1", azimuth::telemetry::api::common::AttributeValue::string("value1")),
    ("custom.attribute.2", azimuth::telemetry::api::common::AttributeValue::int(42L)),
    ("custom.attribute.3", azimuth::telemetry::api::common::AttributeValue::bool(true)),
    ("custom.attribute.4", azimuth::telemetry::api::common::AttributeValue::array_string(["a", "b", "c"]))
  ]
  
  // 创建合并后的资源（模拟合并逻辑）
  let merged_attributes = []
  
  // 添加基础资源属性（空）
  for attr in base_resource.attributes {
    merged_attributes.push(attr)
  }
  
  // 添加额外属性
  for attr in additional_attributes {
    merged_attributes.push(attr)
  }
  
  let merged_resource = azimuth::telemetry::api::common::Resource::{
    service_name: base_resource.service_name,
    service_version: Some("1.0.0"),
    telemetry_sdk_name: base_resource.telemetry_sdk_name,
    telemetry_sdk_version: base_resource.telemetry_sdk_version,
    attributes: merged_attributes
  }
  
  // 验证合并结果
  assert_eq(merged_resource.service_name, "base-service")
  assert_eq(merged_resource.service_version.unwrap(), "1.0.0")
  assert_eq(merged_resource.attributes.length(), 4)
  
  // 验证合并的属性
  assert_eq(merged_resource.attributes[0], ("custom.attribute.1", azimuth::telemetry::api::common::AttributeValue::string("value1")))
  assert_eq(merged_resource.attributes[1], ("custom.attribute.2", azimuth::telemetry::api::common::AttributeValue::int(42L)))
  assert_eq(merged_resource.attributes[2], ("custom.attribute.3", azimuth::telemetry::api::common::AttributeValue::bool(true)))
}

test "resource_detection_and_auto_configuration" {
  // 测试资源检测和自动配置
  
  // 模拟不同环境下的资源配置
  let environments = ["development", "staging", "production", "testing"]
  
  for env in environments {
    let env_specific_attributes = [
      ("deployment.environment", azimuth::telemetry::api::common::AttributeValue::string(env)),
      ("service.namespace", azimuth::telemetry::api::common::AttributeValue::string(env + "-namespace")),
      ("log.level", azimuth::telemetry::api::common::AttributeValue::string(if env == "production" { "WARN" } else { "DEBUG" })),
      ("metrics.enabled", azimuth::telemetry::api::common::AttributeValue::bool(true)),
      ("tracing.enabled", azimuth::telemetry::api::common::AttributeValue::bool(true))
    ]
    
    let env_resource = azimuth::telemetry::api::common::Resource::{
      service_name: "multi-env-service",
      service_version: Some("2.0.0"),
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: env_specific_attributes
    }
    
    // 验证环境特定配置
    assert_eq(env_resource.service_name, "multi-env-service")
    assert_eq(env_resource.attributes.length(), 5)
    
    // 查找环境属性
    let mut found_env = false
    for attr in env_resource.attributes {
      let (key, value) = attr
      if key == "deployment.environment" {
        match value {
          StringValue(env_value) => {
            assert_eq(env_value, env)
            found_env = true
          }
          _ => assert_eq(false, true)
        }
      }
    }
    assert_eq(found_env, true)
  }
}

test "resource_cloud_platform_detection" {
  // 测试云平台资源检测
  
  // 模拟不同云平台的资源配置
  let cloud_configs = [
    {
      provider: "aws",
      region: "us-east-1",
      availability_zone: "us-east-1a",
      instance_id: "i-1234567890abcdef0",
      instance_type: "t3.medium"
    },
    {
      provider: "gcp",
      region: "us-central1",
      availability_zone: "us-central1-a",
      instance_id: "1234567890123456789",
      instance_type: "n1-standard-1"
    },
    {
      provider: "azure",
      region: "eastus",
      availability_zone: "eastus-1",
      instance_id: "vm-12345",
      instance_type: "Standard_B2s"
    }
  ]
  
  for config in cloud_configs {
    let cloud_attributes = [
      ("cloud.provider", azimuth::telemetry::api::common::AttributeValue::string(config.provider)),
      ("cloud.region", azimuth::telemetry::api::common::AttributeValue::string(config.region)),
      ("cloud.availability_zone", azimuth::telemetry::api::common::AttributeValue::string(config.availability_zone)),
      ("cloud.instance.id", azimuth::telemetry::api::common::AttributeValue::string(config.instance_id)),
      ("cloud.instance.type", azimuth::telemetry::api::common::AttributeValue::string(config.instance_type)),
      ("host.name", azimuth::telemetry::api::common::AttributeValue::string(config.instance_id)),
      ("service.instance.id", azimuth::telemetry::api::common::AttributeValue::string(config.instance_id))
    ]
    
    let cloud_resource = azimuth::telemetry::api::common::Resource::{
      service_name: "cloud-service",
      service_version: Some("3.0.0"),
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: cloud_attributes
    }
    
    // 验证云平台配置
    assert_eq(cloud_resource.service_name, "cloud-service")
    assert_eq(cloud_resource.attributes.length(), 7)
    
    // 验证云提供商属性
    let mut found_provider = false
    for attr in cloud_resource.attributes {
      let (key, value) = attr
      if key == "cloud.provider" {
        match value {
          StringValue(provider) => {
            assert_eq(provider, config.provider)
            found_provider = true
          }
          _ => assert_eq(false, true)
        }
      }
    }
    assert_eq(found_provider, true)
  }
}

test "resource_container_environment" {
  // 测试容器环境下的资源配置
  
  // 模拟容器环境（Docker/Kubernetes）
  let container_attributes = [
    ("container.name", azimuth::telemetry::api::common::AttributeValue::string("app-container")),
    ("container.id", azimuth::telemetry::api::common::AttributeValue::string("abc123def456")),
    ("container.image.name", azimuth::telemetry::api::common::AttributeValue::string("myapp:1.2.3")),
    ("container.image.tag", azimuth::telemetry::api::common::AttributeValue::string("1.2.3")),
    ("k8s.pod.name", azimuth::telemetry::api::common::AttributeValue::string("myapp-pod-7d4b8c9f-1234")),
    ("k8s.namespace.name", azimuth::telemetry::api::common::AttributeValue::string("production")),
    ("k8s.deployment.name", azimuth::telemetry::api::common::AttributeValue::string("myapp-deployment")),
    ("k8s.node.name", azimuth::telemetry::api::common::AttributeValue::string("worker-node-3")),
    ("k8s.cluster.name", azimuth::telemetry::api::common::AttributeValue::string("prod-cluster"))
  ]
  
  let container_resource = azimuth::telemetry::api::common::Resource::{
    service_name: "containerized-app",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: container_attributes
  }
  
  // 验证容器资源配置
  assert_eq(container_resource.service_name, "containerized-app")
  assert_eq(container_resource.service_version.unwrap(), "1.2.3")
  assert_eq(container_resource.attributes.length(), 9)
  
  // 验证容器特定属性
  let mut found_container_id = false
  let mut found_pod_name = false
  
  for attr in container_resource.attributes {
    let (key, value) = attr
    if key == "container.id" {
      match value {
        StringValue(id) => {
          assert_eq(id, "abc123def456")
          found_container_id = true
        }
        _ => assert_eq(false, true)
      }
    } else if key == "k8s.pod.name" {
      match value {
        StringValue(pod) => {
          assert_eq(pod, "myapp-pod-7d4b8c9f-1234")
          found_pod_name = true
        }
        _ => assert_eq(false, true)
      }
    }
  }
  
  assert_eq(found_container_id, true)
  assert_eq(found_pod_name, true)
}

test "resource_resource_lifecycle_management" {
  // 测试资源生命周期管理
  
  // 模拟资源创建、更新和销毁
  
  // 1. 创建初始资源
  let initial_resource = azimuth::telemetry::api::common::Resource::default("lifecycle-service")
  assert_eq(initial_resource.service_name, "lifecycle-service")
  assert_eq(initial_resource.attributes.length(), 0)
  
  // 2. 更新资源（添加版本）
  let updated_resource_v1 = azimuth::telemetry::api::common::Resource::{
    service_name: initial_resource.service_name,
    service_version: Some("1.0.0"),
    telemetry_sdk_name: initial_resource.telemetry_sdk_name,
    telemetry_sdk_version: initial_resource.telemetry_sdk_version,
    attributes: initial_resource.attributes
  }
  
  // 3. 添加属性
  let with_attributes = azimuth::telemetry::api::common::Resource::{
    service_name: updated_resource_v1.service_name,
    service_version: updated_resource_v1.service_version,
    telemetry_sdk_name: updated_resource_v1.telemetry_sdk_name,
    telemetry_sdk_version: updated_resource_v1.telemetry_sdk_version,
    attributes: [
      ("initial.attribute", azimuth::telemetry::api::common::AttributeValue::string("initial_value"))
    ]
  }
  
  // 4. 版本升级
  let upgraded_resource = azimuth::telemetry::api::common::Resource::{
    service_name: with_attributes.service_name,
    service_version: Some("2.0.0"),
    telemetry_sdk_name: with_attributes.telemetry_sdk_name,
    telemetry_sdk_version: with_attributes.telemetry_sdk_version,
    attributes: with_attributes.attributes
  }
  
  // 5. 添加更多属性
  let final_resource = azimuth::telemetry::api::common::Resource::{
    service_name: upgraded_resource.service_name,
    service_version: upgraded_resource.service_version,
    telemetry_sdk_name: upgraded_resource.telemetry_sdk_name,
    telemetry_sdk_version: upgraded_resource.telemetry_sdk_version,
    attributes: [
      ("initial.attribute", azimuth::telemetry::api::common::AttributeValue::string("initial_value")),
      ("version.upgrade", azimuth::telemetry::api::common::AttributeValue::string("upgraded_to_v2")),
      ("lifecycle.status", azimuth::telemetry::api::common::AttributeValue::string("active"))
    ]
  }
  
  // 验证生命周期各个阶段
  assert_eq(initial_resource.service_version, None)
  assert_eq(updated_resource_v1.service_version.unwrap(), "1.0.0")
  assert_eq(with_attributes.attributes.length(), 1)
  assert_eq(upgraded_resource.service_version.unwrap(), "2.0.0")
  assert_eq(final_resource.attributes.length(), 3)
  assert_eq(final_resource.service_version.unwrap(), "2.0.0")
}

test "resource_validation_and_constraints" {
  // 测试资源验证和约束
  
  // 测试有效的服务名称
  let valid_service_names = [
    "my-service",
    "my_service",
    "myservice",
    "service123",
    "a.b.c.d",
    "service-with-dashes"
  ]
  
  for name in valid_service_names {
    let resource = azimuth::telemetry::api::common::Resource::default(name)
    assert_eq(resource.service_name, name)
    assert_eq(resource.service_name.length() > 0, true)
  }
  
  // 测试有效的版本字符串
  let valid_versions = [
    "1.0.0",
    "2.1.3",
    "10.20.30",
    "1.0.0-alpha",
    "1.0.0-beta.2",
    "1.0.0+build.123"
  ]
  
  for version in valid_versions {
    let resource_with_version = azimuth::telemetry::api::common::Resource::{
      service_name: "version-test-service",
      service_version: Some(version),
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: []
    }
    assert_eq(resource_with_version.service_version.unwrap(), version)
  }
  
  // 测试属性约束
  let constrained_attributes = [
    ("empty.string", azimuth::telemetry::api::common::AttributeValue::string("")),
    ("zero.int", azimuth::telemetry::api::common::AttributeValue::int(0L)),
    ("negative.int", azimuth::telemetry::api::common::AttributeValue::int(-1L)),
    ("zero.float", azimuth::telemetry::api::common::AttributeValue::float(0.0)),
    ("negative.float", azimuth::telemetry::api::common::AttributeValue::float(-1.0)),
    ("false.bool", azimuth::telemetry::api::common::AttributeValue::bool(false)),
    ("empty.array", azimuth::telemetry::api::common::AttributeValue::array_string([]))
  ]
  
  let constrained_resource = azimuth::telemetry::api::common::Resource::{
    service_name: "constraint-test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: constrained_attributes
  }
  
  // 验证约束属性
  assert_eq(constrained_resource.attributes.length(), 7)
  
  // 验证特定约束值
  for attr in constrained_resource.attributes {
    let (key, value) = attr
    match key {
      "empty.string" => {
        match value {
          StringValue(s) => assert_eq(s, "")
          _ => assert_eq(false, true)
        }
      }
      "zero.int" => {
        match value {
          IntValue(i) => assert_eq(i, 0L)
          _ => assert_eq(false, true)
        }
      }
      "false.bool" => {
        match value {
          BoolValue(b) => assert_eq(b, false)
          _ => assert_eq(false, true)
        }
      }
      "empty.array" => {
        match value {
          ArrayStringValue(arr) => assert_eq(arr.length(), 0)
          _ => assert_eq(false, true)
        }
      }
      _ => assert_eq(true, true)
    }
  }
}