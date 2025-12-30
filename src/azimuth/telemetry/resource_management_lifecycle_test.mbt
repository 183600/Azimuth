// 资源管理生命周期测试用例
// 测试Azimuth Telemetry中资源管理的完整生命周期

test "resource_creation_and_default_configuration" {
  // 测试资源创建和默认配置
  
  // 使用默认工厂方法创建资源
  let default_resource = common::Resource::default("test-service")
  
  // 验证默认值
  assert_eq(default_resource.service_name, "test-service")
  assert_eq(default_resource.service_version, None)
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.attributes.length(), 0)
  
  // 创建带版本的资源
  let resource_with_version = common::Resource::default("api-gateway")
  let resource_with_version_set = common::Resource::{
    ..resource_with_version,
    service_version: Some("2.1.0")
  }
  
  match resource_with_version_set.service_version {
    Some(version) => assert_eq(version, "2.1.0")
    None => assert_eq(false, true, "Expected version to be Some")
  }
  
  // 创建完全自定义的资源
  let custom_resource = common::Resource::{
    service_name: "payment-service",
    service_version: Some("1.5.2"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2")),
      ("zone", common::AttributeValue::string("us-west-2a"))
    ]
  }
  
  assert_eq(custom_resource.service_name, "payment-service")
  match custom_resource.service_version {
    Some(version) => assert_eq(version, "1.5.2")
    None => assert_eq(false, true, "Expected version to be Some")
  }
  assert_eq(custom_resource.attributes.length(), 3)
}

test "resource_attributes_management" {
  // 测试资源属性管理
  
  // 创建带基本属性的资源
  let basic_resource = common::Resource::default("web-service")
  
  // 添加服务相关属性
  let resource_with_service_attrs = common::Resource::{
    ..basic_resource,
    attributes: [
      ("service.name", common::AttributeValue::string("web-service")),
      ("service.version", common::AttributeValue::string("1.0.0")),
      ("service.namespace", common::AttributeValue::string("frontend")),
      ("service.instance.id", common::AttributeValue::string("instance-12345"))
    ]
  }
  
  assert_eq(resource_with_service_attrs.attributes.length(), 4)
  
  // 验证服务属性
  let mut found_service_name = false
  let mut found_service_version = false
  let mut found_service_namespace = false
  let mut found_service_instance_id = false
  
  let mut i = 0
  while i < resource_with_service_attrs.attributes.length() {
    let (key, value) = resource_with_service_attrs.attributes[i]
    
    if key == "service.name" {
      match value {
        common::StringValue(name) => {
          assert_eq(name, "web-service")
          found_service_name = true
        }
        _ => assert_eq(false, true, "Expected StringValue for service.name")
      }
    }
    
    if key == "service.version" {
      match value {
        common::StringValue(version) => {
          assert_eq(version, "1.0.0")
          found_service_version = true
        }
        _ => assert_eq(false, true, "Expected StringValue for service.version")
      }
    }
    
    if key == "service.namespace" {
      match value {
        common::StringValue(namespace) => {
          assert_eq(namespace, "frontend")
          found_service_namespace = true
        }
        _ => assert_eq(false, true, "Expected StringValue for service.namespace")
      }
    }
    
    if key == "service.instance.id" {
      match value {
        common::StringValue(instance_id) => {
          assert_eq(instance_id, "instance-12345")
          found_service_instance_id = true
        }
        _ => assert_eq(false, true, "Expected StringValue for service.instance.id")
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_service_name, true, "Should find service.name")
  assert_eq(found_service_version, true, "Should find service.version")
  assert_eq(found_service_namespace, true, "Should find service.namespace")
  assert_eq(found_service_instance_id, true, "Should find service.instance.id")
  
  // 创建带部署属性的资源
  let resource_with_deployment_attrs = common::Resource::{
    ..basic_resource,
    attributes: [
      ("deployment.environment", common::AttributeValue::string("production")),
      ("deployment.region", common::AttributeValue::string("us-east-1")),
      ("deployment.availability_zone", common::AttributeValue::string("us-east-1a")),
      ("deployment.cluster", common::AttributeValue::string("main-cluster")),
      ("deployment.pod", common::AttributeValue::string("web-service-7f8d9c2"))
    ]
  }
  
  assert_eq(resource_with_deployment_attrs.attributes.length(), 5)
  
  // 创建带主机属性的资源配置
  let resource_with_host_attrs = common::Resource::{
    ..basic_resource,
    attributes: [
      ("host.name", common::AttributeValue::string("web-server-01.example.com")),
      ("host.id", common::AttributeValue::string("host-abcdef123456")),
      ("host.type", common::AttributeValue::string("virtual_machine")),
      ("host.image.name", common::AttributeValue::string("ubuntu-20.04")),
      ("host.image.id", common::AttributeValue::string("ami-1234567890abcdef0")),
      ("host.cpu.count", common::AttributeValue::int(4L)),
      ("host.memory.available", common::AttributeValue::int(8589934592L)),
      ("host.arch", common::AttributeValue::string("x86_64"))
    ]
  }
  
  assert_eq(resource_with_host_attrs.attributes.length(), 8)
  
  // 验证数值属性
  let mut found_cpu_count = false
  let mut found_memory_available = false
  
  i = 0
  while i < resource_with_host_attrs.attributes.length() {
    let (key, value) = resource_with_host_attrs.attributes[i]
    
    if key == "host.cpu.count" {
      match value {
        common::IntValue(cpu_count) => {
          assert_eq(cpu_count, 4L)
          found_cpu_count = true
        }
        _ => assert_eq(false, true, "Expected IntValue for host.cpu.count")
      }
    }
    
    if key == "host.memory.available" {
      match value {
        common::IntValue(memory) => {
          assert_eq(memory, 8589934592L)  // 8GB in bytes
          found_memory_available = true
        }
        _ => assert_eq(false, true, "Expected IntValue for host.memory.available")
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_cpu_count, true, "Should find host.cpu.count")
  assert_eq(found_memory_available, true, "Should find host.memory.available")
}

test "resource_inheritance_and_merging" {
  // 测试资源继承和合并
  
  // 创建基础资源
  let base_resource = common::Resource::{
    service_name: "base-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("development")),
      ("team", common::AttributeValue::string("platform"))
    ]
  }
  
  // 创建继承资源（添加更多属性）
  let inherited_resource = common::Resource::{
    service_name: base_resource.service_name,
    service_version: base_resource.service_version,
    telemetry_sdk_name: base_resource.telemetry_sdk_name,
    telemetry_sdk_version: base_resource.telemetry_sdk_version,
    attributes: [
      // 继承的属性
      ("environment", common::AttributeValue::string("development")),
      ("team", common::AttributeValue::string("platform")),
      // 新增的属性
      ("service.namespace", common::AttributeValue::string("api")),
      ("service.instance.id", common::AttributeValue::string("instance-67890")),
      ("region", common::AttributeValue::string("us-west-2"))
    ]
  }
  
  assert_eq(inherited_resource.attributes.length(), 5)
  
  // 验证继承的属性存在
  let mut found_environment = false
  let mut found_team = false
  let mut found_namespace = false
  
  let mut i = 0
  while i < inherited_resource.attributes.length() {
    let (key, _) = inherited_resource.attributes[i]
    
    if key == "environment" {
      found_environment = true
    }
    
    if key == "team" {
      found_team = true
    }
    
    if key == "service.namespace" {
      found_namespace = true
    }
    
    i = i + 1
  }
  
  assert_eq(found_environment, true, "Should inherit environment attribute")
  assert_eq(found_team, true, "Should inherit team attribute")
  assert_eq(found_namespace, true, "Should have new namespace attribute")
  
  // 测试属性覆盖（在实际实现中可能有合并策略）
  let override_resource = common::Resource::{
    service_name: base_resource.service_name,
    service_version: base_resource.service_version,
    telemetry_sdk_name: base_resource.telemetry_sdk_name,
    telemetry_sdk_version: base_resource.telemetry_sdk_version,
    attributes: [
      // 覆盖的属性
      ("environment", common::AttributeValue::string("production")),
      // 保留的属性
      ("team", common::AttributeValue::string("platform")),
      // 新增的属性
      ("deployment.region", common::AttributeValue::string("us-east-1"))
    ]
  }
  
  // 验证覆盖后的属性
  let mut found_production_env = false
  let mut found_team_kept = false
  let mut found_deployment_region = false
  
  i = 0
  while i < override_resource.attributes.length() {
    let (key, value) = override_resource.attributes[i]
    
    if key == "environment" {
      match value {
        common::StringValue(env) => {
          assert_eq(env, "production")
          found_production_env = true
        }
        _ => {}
      }
    }
    
    if key == "team" {
      match value {
        common::StringValue(team) => {
          assert_eq(team, "platform")
          found_team_kept = true
        }
        _ => {}
      }
    }
    
    if key == "deployment.region" {
      found_deployment_region = true
    }
    
    i = i + 1
  }
  
  assert_eq(found_production_env, true, "Should have overridden environment")
  assert_eq(found_team_kept, true, "Should keep team attribute")
  assert_eq(found_deployment_region, true, "Should have new deployment.region")
}

test "resource_validation_and_constraints" {
  // 测试资源验证和约束
  
  // 测试空服务名称（在实际实现中应该被拒绝）
  let empty_name_resource = common::Resource::default("")
  assert_eq(empty_name_resource.service_name, "")
  
  // 测试长服务名称
  let long_name = "this.is.a.very.long.service.name.that.tests.the.systems.ability.to.handle.extended.service.names.without.issues"
  let long_name_resource = common::Resource::default(long_name)
  assert_eq(long_name_resource.service_name.length(), long_name.length())
  
  // 测试特殊字符服务名称
  let special_chars_name = "service.with-special_chars@123#$%^&*()"
  let special_chars_resource = common::Resource::default(special_chars_name)
  assert_eq(special_chars_resource.service_name, special_chars_name)
  
  // 测试Unicode服务名称
  let unicode_name = "服务名称.测试.🚀"
  let unicode_resource = common::Resource::default(unicode_name)
  assert_eq(unicode_resource.service_name, unicode_name)
  
  // 测试版本约束
  let resources_with_versions = [
    common::Resource::{ service_name: "service1", service_version: Some("1.0.0"), telemetry_sdk_name: "azimuth", telemetry_sdk_version: "0.1.0", attributes: [] },
    common::Resource::{ service_name: "service2", service_version: Some("v2.1.0"), telemetry_sdk_name: "azimuth", telemetry_sdk_version: "0.1.0", attributes: [] },
    common::Resource::{ service_name: "service3", service_version: Some("2023.01.15"), telemetry_sdk_name: "azimuth", telemetry_sdk_version: "0.1.0", attributes: [] },
    common::Resource::{ service_name: "service4", service_version: Some("latest"), telemetry_sdk_name: "azimuth", telemetry_sdk_version: "0.1.0", attributes: [] },
    common::Resource::{ service_name: "service5", service_version: Some(""), telemetry_sdk_name: "azimuth", telemetry_sdk_version: "0.1.0", attributes: [] }
  ]
  
  let mut i = 0
  while i < resources_with_versions.length() {
    let resource = resources_with_versions[i]
    match resource.service_version {
      Some(version) => {
        assert_eq(version.length() >= 0, true)
        // 验证各种版本格式
        if i == 0 { assert_eq(version, "1.0.0") }
        if i == 1 { assert_eq(version, "v2.1.0") }
        if i == 2 { assert_eq(version, "2023.01.15") }
        if i == 3 { assert_eq(version, "latest") }
        if i == 4 { assert_eq(version, "") }
      }
      None => assert_eq(false, true, "Expected version to be Some")
    }
    i = i + 1
  }
  
  // 测试属性数量限制（在实际实现中可能有限制）
  let many_attributes = []
  i = 0
  while i < 1000 {
    many_attributes.push(("attr." + i.to_string(), common::AttributeValue::string("value." + i.to_string())))
    i = i + 1
  }
  
  let resource_with_many_attrs = common::Resource::{
    service_name: "service-with-many-attrs",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: many_attributes
  }
  
  assert_eq(resource_with_many_attrs.attributes.length(), 1000)
}

test "resource_lifecycle_operations" {
  // 测试资源生命周期操作
  
  // 创建资源
  let initial_resource = common::Resource::default("lifecycle-service")
  
  // 模拟资源初始化阶段
  let init_resource = common::Resource::{
    ..initial_resource,
    attributes: [
      ("lifecycle.phase", common::AttributeValue::string("initializing")),
      ("startup.time", common::AttributeValue::int(1640995200L)),
      ("pid", common::AttributeValue::int(12345L))
    ]
  }
  
  assert_eq(init_resource.attributes.length(), 3)
  
  // 模拟资源运行阶段
  let running_resource = common::Resource::{
    ..init_resource,
    attributes: [
      // 更新生命周期阶段
      ("lifecycle.phase", common::AttributeValue::string("running")),
      // 保留启动信息
      ("startup.time", common::AttributeValue::int(1640995200L)),
      ("pid", common::AttributeValue::int(12345L)),
      // 添加运行时属性
      ("runtime.start", common::AttributeValue::int(1640995300L)),
      ("port", common::AttributeValue::int(8080L)),
      ("ready", common::AttributeValue::bool(true))
    ]
  }
  
  assert_eq(running_resource.attributes.length(), 6)
  
  // 验证运行时属性
  let mut found_running_phase = false
  let mut found_ready = false
  let mut found_port = false
  
  let mut i = 0
  while i < running_resource.attributes.length() {
    let (key, value) = running_resource.attributes[i]
    
    if key == "lifecycle.phase" {
      match value {
        common::StringValue(phase) => {
          assert_eq(phase, "running")
          found_running_phase = true
        }
        _ => {}
      }
    }
    
    if key == "ready" {
      match value {
        common::BoolValue(ready) => {
          assert_eq(ready, true)
          found_ready = true
        }
        _ => {}
      }
    }
    
    if key == "port" {
      match value {
        common::IntValue(port) => {
          assert_eq(port, 8080L)
          found_port = true
        }
        _ => {}
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_running_phase, true, "Should have running phase")
  assert_eq(found_ready, true, "Should have ready status")
  assert_eq(found_port, true, "Should have port")
  
  // 模拟资源关闭阶段
  let shutdown_resource = common::Resource::{
    ..running_resource,
    attributes: [
      // 更新生命周期阶段
      ("lifecycle.phase", common::AttributeValue::string("shutting_down")),
      // 保留关键运行时信息
      ("startup.time", common::AttributeValue::int(1640995200L)),
      ("runtime.start", common::AttributeValue::int(1640995300L)),
      ("pid", common::AttributeValue::int(12345L)),
      ("port", common::AttributeValue::int(8080L)),
      // 更新状态
      ("ready", common::AttributeValue::bool(false)),
      // 添加关闭信息
      ("shutdown.start", common::AttributeValue::int(1640995400L)),
      ("shutdown.reason", common::AttributeValue::string("graceful"))
    ]
  }
  
  assert_eq(shutdown_resource.attributes.length(), 8)
  
  // 验证关闭阶段属性
  let mut found_shutdown_phase = false
  let mut found_shutdown_reason = false
  let mut found_not_ready = false
  
  i = 0
  while i < shutdown_resource.attributes.length() {
    let (key, value) = shutdown_resource.attributes[i]
    
    if key == "lifecycle.phase" {
      match value {
        common::StringValue(phase) => {
          assert_eq(phase, "shutting_down")
          found_shutdown_phase = true
        }
        _ => {}
      }
    }
    
    if key == "shutdown.reason" {
      match value {
        common::StringValue(reason) => {
          assert_eq(reason, "graceful")
          found_shutdown_reason = true
        }
        _ => {}
      }
    }
    
    if key == "ready" {
      match value {
        common::BoolValue(ready) => {
          assert_eq(ready, false)
          found_not_ready = true
        }
        _ => {}
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_shutdown_phase, true, "Should have shutdown phase")
  assert_eq(found_shutdown_reason, true, "Should have shutdown reason")
  assert_eq(found_not_ready, true, "Should be not ready during shutdown")
}

test "resource_compatibility_and_standardization" {
  // 测试资源兼容性和标准化
  
  // 测试OpenTelemetry资源标准
  let otel_standard_resource = common::Resource::{
    service_name: "otel-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      // OpenTelemetry标准属性
      ("service.name", common::AttributeValue::string("otel-service")),
      ("service.version", common::AttributeValue::string("1.0.0")),
      ("service.namespace", common::AttributeValue::string("production")),
      ("service.instance.id", common::AttributeValue::string("instance-otel-123")),
      
      // 主机属性
      ("host.name", common::AttributeValue::string("host-otel.example.com")),
      ("host.id", common::AttributeValue::string("host-otel-456")),
      ("host.type", common::AttributeValue::string("container")),
      ("host.image.name", common::AttributeValue::string("otel/runtime:latest")),
      
      // Kubernetes属性
      ("k8s.pod.name", common::AttributeValue::string("otel-service-7f8d9c2")),
      ("k8s.namespace.name", common::AttributeValue::string("production")),
      ("k8s.deployment.name", common::AttributeValue::string("otel-service")),
      ("k8s.node.name", common::AttributeValue::string("node-otel-001")),
      
      // 进程属性
      ("process.pid", common::AttributeValue::int(12345L)),
      ("process.executable.name", common::AttributeValue::string("otel-service")),
      ("process.executable.path", common::AttributeValue::string("/app/otel-service")),
      ("process.command_args", common::AttributeValue::array_string(["/app/otel-service", "--config", "/etc/config.yaml"])),
      
      // 云属性
      ("cloud.provider", common::AttributeValue::string("aws")),
      ("cloud.account.id", common::AttributeValue::string("123456789012")),
      ("cloud.region", common::AttributeValue::string("us-west-2")),
      ("cloud.availability_zone", common::AttributeValue::string("us-west-2a"))
    ]
  }
  
  assert_eq(otel_standard_resource.attributes.length(), 18)
  
  // 验证标准属性存在
  let standard_attributes = [
    "service.name", "service.version", "service.namespace", "service.instance.id",
    "host.name", "host.id", "host.type", "host.image.name",
    "k8s.pod.name", "k8s.namespace.name", "k8s.deployment.name", "k8s.node.name",
    "process.pid", "process.executable.name", "process.executable.path", "process.command_args",
    "cloud.provider", "cloud.account.id", "cloud.region", "cloud.availability_zone"
  ]
  
  let mut found_standards = 0
  let mut i = 0
  while i < otel_standard_resource.attributes.length() {
    let (key, _) = otel_standard_resource.attributes[i]
    
    let mut j = 0
    while j < standard_attributes.length() {
      if key == standard_attributes[j] {
        found_standards = found_standards + 1
        break
      }
      j = j + 1
    }
    
    i = i + 1
  }
  
  assert_eq(found_standards, 18, "Should find all standard attributes")
  
  // 测试数组属性
  let mut found_command_args = false
  i = 0
  while i < otel_standard_resource.attributes.length() {
    let (key, value) = otel_standard_resource.attributes[i]
    
    if key == "process.command_args" {
      match value {
        common::ArrayStringValue(args) => {
          assert_eq(args.length(), 3)
          assert_eq(args[0], "/app/otel-service")
          assert_eq(args[1], "--config")
          assert_eq(args[2], "/etc/config.yaml")
          found_command_args = true
        }
        _ => {}
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_command_args, true, "Should find command args array")
  
  // 测试自定义资源属性
  let custom_resource = common::Resource::{
    service_name: "custom-service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      // 标准属性
      ("service.name", common::AttributeValue::string("custom-service")),
      ("service.version", common::AttributeValue::string("2.0.0")),
      
      // 自定义业务属性
      ("business.unit", common::AttributeValue::string("payments")),
      ("business.criticality", common::AttributeValue::string("high")),
      ("business.owner", common::AttributeValue::string("payments-team@example.com")),
      
      // 自定义技术属性
      ("tech.stack", common::AttributeValue::array_string(["nodejs", "express", "postgresql"])),
      ("tech.version.nodejs", common::AttributeValue::string("18.17.0")),
      ("tech.version.express", common::AttributeValue::string("4.18.2")),
      
      // 自定义监控属性
      ("monitoring.enabled", common::AttributeValue::bool(true)),
      ("monitoring.metrics.endpoints", common::AttributeValue::array_string(["/metrics", "/health"])),
      ("monitoring.alerts.enabled", common::AttributeValue::bool(true)),
      ("monitoring.alerts.channels", common::AttributeValue::array_string(["email", "slack", "pagerduty"]))
    ]
  }
  
  assert_eq(custom_resource.attributes.length(), 13)
  
  // 验证自定义属性
  let mut found_business_unit = false
  let mut found_tech_stack = false
  let mut found_monitoring_channels = false
  
  i = 0
  while i < custom_resource.attributes.length() {
    let (key, value) = custom_resource.attributes[i]
    
    if key == "business.unit" {
      match value {
        common::StringValue(unit) => {
          assert_eq(unit, "payments")
          found_business_unit = true
        }
        _ => {}
      }
    }
    
    if key == "tech.stack" {
      match value {
        common::ArrayStringValue(stack) => {
          assert_eq(stack.length(), 3)
          assert_eq(stack.contains("nodejs"), true)
          assert_eq(stack.contains("express"), true)
          assert_eq(stack.contains("postgresql"), true)
          found_tech_stack = true
        }
        _ => {}
      }
    }
    
    if key == "monitoring.alerts.channels" {
      match value {
        common::ArrayStringValue(channels) => {
          assert_eq(channels.length(), 3)
          assert_eq(channels.contains("email"), true)
          assert_eq(channels.contains("slack"), true)
          assert_eq(channels.contains("pagerduty"), true)
          found_monitoring_channels = true
        }
        _ => {}
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_business_unit, true, "Should find business.unit")
  assert_eq(found_tech_stack, true, "Should find tech.stack")
  assert_eq(found_monitoring_channels, true, "Should find monitoring.alerts.channels")
}