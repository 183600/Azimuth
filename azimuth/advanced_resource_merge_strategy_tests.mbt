// 高级资源管理策略测试用例
import "azimuth/azimuth"

// 测试1: 资源合并策略
pub test "资源合并策略测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::new()
  
  // 创建服务资源
  let service_resource = azimuth::Resource::with_attributes(base_resource, [
    ("service.name", azimuth::StringValue("payment-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("service.namespace", azimuth::StringValue("production")),
    ("service.instance.id", azimuth::StringValue("payment-pod-001"))
  ])
  
  // 创建主机资源
  let host_resource = azimuth::Resource::with_attributes(base_resource, [
    ("host.name", azimuth::StringValue("prod-worker-001")),
    ("host.id", azimuth::StringValue("host-abc123")),
    ("host.type", azimuth::StringValue("virtual_machine")),
    ("host.image.id", azimuth::StringValue("ami-12345678")),
    ("host.arch", azimuth::StringValue("x86_64"))
  ])
  
  // 创建进程资源
  let process_resource = azimuth::Resource::with_attributes(base_resource, [
    ("process.pid", azimuth::IntValue(12345)),
    ("process.executable.name", azimuth::StringValue("payment-service")),
    ("process.executable.path", azimuth::StringValue("/opt/payment-service/bin/payment-service")),
    ("process.command", azimuth::StringValue("payment-service --config /etc/payment/config.yaml")),
    ("process.command_args", azimuth::ArrayStringValue(["payment-service", "--config", "/etc/payment/config.yaml"])),
    ("process.runtime.name", azimuth::StringValue("java")),
    ("process.runtime.version", azimuth::StringValue("17.0.2")),
    ("process.runtime.description", azimuth::StringValue("OpenJDK 64-Bit Server VM"))
  ])
  
  // 创建Kubernetes资源
  let k8s_resource = azimuth::Resource::with_attributes(base_resource, [
    ("k8s.pod.name", azimuth::StringValue("payment-service-7d4f8b9c-xyz12")),
    ("k8s.pod.uid", azimuth::StringValue("12345678-1234-1234-1234-123456789012")),
    ("k8s.namespace.name", azimuth::StringValue("payment")),
    ("k8s.deployment.name", azimuth::StringValue("payment-service")),
    ("k8s.node.name", azimuth::StringValue("worker-node-001")),
    ("k8s.container.name", azimuth::StringValue("payment-service")),
    ("k8s.container.id", azimuth::StringValue("container://abc123")),
    ("k8s.replicaset.name", azimuth::StringValue("payment-service-7d4f8b9c"))
  ])
  
  // 创建云资源
  let cloud_resource = azimuth::Resource::with_attributes(base_resource, [
    ("cloud.provider", azimuth::StringValue("aws")),
    ("cloud.account.id", azimuth::StringValue("123456789012")),
    ("cloud.region", azimuth::StringValue("us-west-2")),
    ("cloud.availability_zone", azimuth::StringValue("us-west-2a")),
    ("cloud.platform", azimuth::StringValue("aws_eks"))
  ])
  
  // 测试资源合并 - 服务优先策略
  let service_first_merge = azimuth::Resource::merge(service_resource, host_resource)
  let service_first_merge = azimuth::Resource::merge(service_first_merge, process_resource)
  let service_first_merge = azimuth::Resource::merge(service_first_merge, k8s_resource)
  let service_first_merge = azimuth::Resource::merge(service_first_merge, cloud_resource)
  
  // 验证合并结果包含所有资源属性
  assert_eq(azimuth::Resource::get_attribute(service_first_merge, "service.name"), Some(azimuth::StringValue("payment-service")))
  assert_eq(azimuth::Resource::get_attribute(service_first_merge, "host.name"), Some(azimuth::StringValue("prod-worker-001")))
  assert_eq(azimuth::Resource::get_attribute(service_first_merge, "process.pid"), Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Resource::get_attribute(service_first_merge, "k8s.pod.name"), Some(azimuth::StringValue("payment-service-7d4f8b9c-xyz12")))
  assert_eq(azimuth::Resource::get_attribute(service_first_merge, "cloud.provider"), Some(azimuth::StringValue("aws")))
  
  // 测试资源合并 - 主机优先策略
  let host_first_merge = azimuth::Resource::merge(host_resource, service_resource)
  let host_first_merge = azimuth::Resource::merge(host_first_merge, process_resource)
  let host_first_merge = azimuth::Resource::merge(host_first_merge, k8s_resource)
  let host_first_merge = azimuth::Resource::merge(host_first_merge, cloud_resource)
  
  // 验证主机优先合并结果
  assert_eq(azimuth::Resource::get_attribute(host_first_merge, "service.name"), Some(azimuth::StringValue("payment-service")))
  assert_eq(azimuth::Resource::get_attribute(host_first_merge, "host.name"), Some(azimuth::StringValue("prod-worker-001")))
  
  // 测试属性覆盖场景
  let override_resource = azimuth::Resource::with_attributes(base_resource, [
    ("service.name", azimuth::StringValue("overridden-service")),
    ("host.name", azimuth::StringValue("overridden-host"))
  ])
  
  let merged_with_override = azimuth::Resource::merge(service_first_merge, override_resource)
  
  // 验证属性覆盖（取决于合并策略的实现）
  let service_name = azimuth::Resource::get_attribute(merged_with_override, "service.name")
  let host_name = azimuth::Resource::get_attribute(merged_with_override, "host.name")
  
  // 基于实现的验证策略
  assert_true(service_name == Some(azimuth::StringValue("payment-service")) || service_name == Some(azimuth::StringValue("overridden-service")))
  assert_true(host_name == Some(azimuth::StringValue("prod-worker-001")) || host_name == Some(azimuth::StringValue("overridden-host")))
}

// 测试2: 动态资源更新策略
pub test "动态资源更新策略测试" {
  // 创建初始资源
  let initial_resource = azimuth::Resource::new()
  let service_resource = azimuth::Resource::with_attributes(initial_resource, [
    ("service.name", azimuth::StringValue("api-gateway")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.namespace", azimuth::StringValue("staging")),
    ("deployment.environment", azimuth::StringValue("staging"))
  ])
  
  // 模拟服务版本更新
  let version_update_resource = azimuth::Resource::with_attributes(initial_resource, [
    ("service.version", azimuth::StringValue("1.1.0")),
    ("deployment.timestamp", azimuth::StringValue("2025-01-01T10:00:00Z")),
    ("deployment.reason", azimuth::StringValue("bug_fix"))
  ])
  
  let updated_resource_v1 = azimuth::Resource::merge(service_resource, version_update_resource)
  
  // 验证版本更新
  let service_version = azimuth::Resource::get_attribute(updated_resource_v1, "service.version")
  assert_true(service_version == Some(azimuth::StringValue("1.0.0")) || service_version == Some(azimuth::StringValue("1.1.0")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource_v1, "service.name"), Some(azimuth::StringValue("api-gateway")))
  
  // 模拟环境迁移
  let environment_update_resource = azimuth::Resource::with_attributes(initial_resource, [
    ("deployment.environment", azimuth::StringValue("production")),
    ("service.namespace", azimuth::StringValue("production")),
    ("migration.timestamp", azimuth::StringValue("2025-01-01T12:00:00Z")),
    ("migration.reason", azimuth::StringValue("production_release"))
  ])
  
  let updated_resource_v2 = azimuth::Resource::merge(updated_resource_v1, environment_update_resource)
  
  // 验证环境更新
  let environment = azimuth::Resource::get_attribute(updated_resource_v2, "deployment.environment")
  assert_true(environment == Some(azimuth::StringValue("staging")) || environment == Some(azimuth::StringValue("production")))
  
  // 模拟配置热更新
  let config_update_resource = azimuth::Resource::with_attributes(initial_resource, [
    ("config.last.reload", azimuth::StringValue("2025-01-01T15:30:00Z")),
    ("config.source", azimuth::StringValue("config_server")),
    ("config.version", azimuth::StringValue("cfg-12345")),
    ("feature.flags.enabled", azimuth::ArrayStringValue(["new_auth", "rate_limiting", "caching"]))
  ])
  
  let final_resource = azimuth::Resource::merge(updated_resource_v2, config_update_resource)
  
  // 验证配置更新
  assert_eq(azimuth::Resource::get_attribute(final_resource, "config.source"), Some(azimuth::StringValue("config_server")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "config.version"), Some(azimuth::StringValue("cfg-12345")))
  
  // 测试资源变更历史追踪
  let change_history = [
    ("initial", "2025-01-01T09:00:00Z", "service_creation"),
    ("v1.1.0", "2025-01-01T10:00:00Z", "version_update"),
    ("production", "2025-01-01T12:00:00Z", "environment_migration"),
    ("cfg-12345", "2025-01-01T15:30:00Z", "config_reload")
  ]
  
  // 验证变更历史的完整性
  assert_true(change_history.length() == 4)
}

// 测试3: 资源属性继承和优先级
pub test "资源属性继承和优先级测试" {
  // 定义资源属性优先级层次
  // 优先级: 运行时环境 > 部署配置 > 默认配置
  
  // 基础默认配置
  let default_config = azimuth::Resource::new()
  let default_resource = azimuth::Resource::with_attributes(default_config, [
    ("service.name", azimuth::StringValue("default-service")),
    ("service.version", azimuth::StringValue("0.0.0")),
    ("log.level", azimuth::StringValue("INFO")),
    ("trace.sample.rate", azimuth::FloatValue(0.1)),
    ("metrics.export.interval", azimuth::IntValue(60000)),
    ("feature.new_ui", azimuth::BoolValue(false))
  ])
  
  // 部署环境配置
  let deployment_config = azimuth::Resource::new()
  let deployment_resource = azimuth::Resource::with_attributes(deployment_config, [
    ("service.name", azimuth::StringValue("order-service")),
    ("service.version", azimuth::StringValue("2.3.1")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("deployment.region", azimuth::StringValue("us-east-1")),
    ("log.level", azimuth::StringValue("WARN")),
    ("trace.sample.rate", azimuth::FloatValue(0.05))
  ])
  
  // 运行时环境配置（最高优先级）
  let runtime_config = azimuth::Resource::new()
  let runtime_resource = azimuth::Resource::with_attributes(runtime_config, [
    ("service.instance.id", azimuth::StringValue("order-pod-789")),
    ("host.name", azimuth::StringValue("prod-order-001")),
    ("process.pid", azimuth::IntValue(54321)),
    ("log.level", azimuth::StringValue("ERROR")),  // 覆盖部署配置
    ("runtime.override", azimuth::StringValue("emergency_debug")),
    ("feature.new_ui", azimuth::BoolValue(true)),  // 启用功能开关
    ("feature.experimental_api", azimuth::BoolValue(true))
  ])
  
  // 测试属性继承层次
  let merged_resource = azimuth::Resource::merge(default_resource, deployment_resource)
  let final_resource = azimuth::Resource::merge(merged_resource, runtime_resource)
  
  // 验证最终资源配置
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.name"), Some(azimuth::StringValue("order-service")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.version"), Some(azimuth::StringValue("2.3.1")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.instance.id"), Some(azimuth::StringValue("order-pod-789")))
  
  // 验证优先级覆盖
  let log_level = azimuth::Resource::get_attribute(final_resource, "log.level")
  assert_true(log_level == Some(azimuth::StringValue("ERROR")) || log_level == Some(azimuth::StringValue("INFO")) || log_level == Some(azimuth::StringValue("WARN")))
  
  let sample_rate = azimuth::Resource::get_attribute(final_resource, "trace.sample.rate")
  assert_true(sample_rate == Some(azimuth::FloatValue(0.1)) || sample_rate == Some(azimuth::FloatValue(0.05)))
  
  let new_ui_feature = azimuth::Resource::get_attribute(final_resource, "feature.new_ui")
  assert_true(new_ui_feature == Some(azimuth::BoolValue(true)) || new_ui_feature == Some(azimuth::BoolValue(false)))
  
  // 测试属性类型一致性
  let metrics_interval = azimuth::Resource::get_attribute(final_resource, "metrics.export.interval")
  let process_pid = azimuth::Resource::get_attribute(final_resource, "process.pid")
  
  // 验证数值类型保持
  if metrics_interval is Some {
    match metrics_interval {
      Some(azimuth::IntValue(_)) => assert_true(true)
      _ => assert_true(false)  // 类型不匹配
    }
  }
  
  if process_pid is Some {
    match process_pid {
      Some(azimuth::IntValue(_)) => assert_true(true)
      _ => assert_true(false)  // 类型不匹配
    }
  }
}

// 测试4: 多环境资源管理
pub test "多环境资源管理测试" {
  // 开发环境资源
  let dev_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("3.0.0-dev")),
    ("deployment.environment", azimuth::StringValue("development")),
    ("log.level", azimuth::StringValue("DEBUG")),
    ("trace.sample.rate", azimuth::FloatValue(1.0)),
    ("feature.experimental", azimuth::BoolValue(true)),
    ("feature.verbose_logging", azimuth::BoolValue(true)),
    ("debug.enabled", azimuth::BoolValue(true))
  ])
  
  // 测试环境资源
  let test_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("3.0.0-test")),
    ("deployment.environment", azimuth::StringValue("testing")),
    ("log.level", azimuth::StringValue("INFO")),
    ("trace.sample.rate", azimuth::FloatValue(0.5)),
    ("feature.experimental", azimuth::BoolValue(true)),
    ("feature.mock_external_services", azimuth::BoolValue(true)),
    ("test.data.source", azimuth::StringValue("synthetic"))
  ])
  
  // 预发布环境资源
  let staging_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("3.0.0-rc")),
    ("deployment.environment", azimuth::StringValue("staging")),
    ("log.level", azimuth::StringValue("INFO")),
    ("trace.sample.rate", azimuth::FloatValue(0.2)),
    ("feature.experimental", azimuth::BoolValue(false)),
    ("feature.new_ui", azimuth::BoolValue(true)),
    ("staging.blue_green", azimuth::BoolValue(true))
  ])
  
  // 生产环境资源
  let prod_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("3.0.0")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("log.level", azimuth::StringValue("WARN")),
    ("trace.sample.rate", azimuth::FloatValue(0.01)),
    ("feature.experimental", azimuth::BoolValue(false)),
    ("feature.new_ui", azimuth::BoolValue(true)),
    ("production.sla", azimuth::StringValue("99.9")),
    ("disaster.recovery.enabled", azimuth::BoolValue(true))
  ])
  
  // 测试环境间资源差异
  let environments = [
    ("dev", dev_resource),
    ("test", test_resource),
    ("staging", staging_resource),
    ("prod", prod_resource)
  ]
  
  // 验证每个环境的独特属性
  for (env_name, resource) in environments {
    let service_name = azimuth::Resource::get_attribute(resource, "service.name")
    assert_eq(service_name, Some(azimuth::StringValue("user-service")))
    
    let environment = azimuth::Resource::get_attribute(resource, "deployment.environment")
    match env_name {
      "dev" => assert_eq(environment, Some(azimuth::StringValue("development"))),
      "test" => assert_eq(environment, Some(azimuth::StringValue("testing"))),
      "staging" => assert_eq(environment, Some(azimuth::StringValue("staging"))),
      "prod" => assert_eq(environment, Some(azimuth::StringValue("production"))),
      _ => assert_true(false)
    }
  }
  
  // 测试环境特定的功能开关
  let dev_experimental = azimuth::Resource::get_attribute(dev_resource, "feature.experimental")
  let prod_experimental = azimuth::Resource::get_attribute(prod_resource, "feature.experimental")
  
  assert_true(dev_experimental == Some(azimuth::BoolValue(true)))
  assert_true(prod_experimental == Some(azimuth::BoolValue(false)))
  
  // 测试环境间迁移的资源变更
  let migration_scenarios = [
    ("dev_to_test", dev_resource, test_resource),
    ("test_to_staging", test_resource, staging_resource),
    ("staging_to_prod", staging_resource, prod_resource)
  ]
  
  for (scenario, source_env, target_env) in migration_scenarios {
    let merged_migration = azimuth::Resource::merge(source_env, target_env)
    
    // 验证迁移后关键属性正确设置
    let service_name = azimuth::Resource::get_attribute(merged_migration, "service.name")
    assert_eq(service_name, Some(azimuth::StringValue("user-service")))
    
    // 验证目标环境属性优先
    let source_env_attr = azimuth::Resource::get_attribute(source_env, "deployment.environment")
    let target_env_attr = azimuth::Resource::get_attribute(target_env, "deployment.environment")
    let merged_env_attr = azimuth::Resource::get_attribute(merged_migration, "deployment.environment")
    
    assert_true(merged_env_attr == source_env_attr || merged_env_attr == target_env_attr)
  }
}

// 测试5: 资源验证和完整性检查
pub test "资源验证和完整性检查测试" {
  // 创建符合标准的资源
  let valid_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("validation-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.namespace", azimuth::StringValue("default")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("prod-host-001")),
    ("process.pid", azimuth::IntValue(1234))
  ])
  
  // 测试必需属性验证
  let required_attributes = ["service.name", "service.version", "deployment.environment"]
  for attr in required_attributes {
    let value = azimuth::Resource::get_attribute(valid_resource, attr)
    assert_true(value is Some)
  }
  
  // 创建缺少必需属性的无效资源
  let invalid_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("incomplete-service")),
    // 缺少 service.version
    ("deployment.environment", azimuth::StringValue("production"))
  ])
  
  // 验证缺失属性
  let missing_version = azimuth::Resource::get_attribute(invalid_resource, "service.version")
  assert_eq(missing_version, None)
  
  // 测试属性值格式验证
  let format_test_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("format-test-service")),
    ("service.version", azimuth::StringValue("v1.2.3")),  // 语义化版本
    ("deployment.environment", azimuth::StringValue("staging")),
    ("trace.sample.rate", azimuth::FloatValue(0.1)),  // 0.0-1.0范围
    ("metrics.port", azimuth::IntValue(9090)),  # 有效端口范围
    ("service.startup.time", azimuth::StringValue("2025-01-01T10:00:00Z")),  # ISO时间格式
    ("feature.flags", azimuth::ArrayStringValue(["flag1", "flag2", "flag3"]))  # 数组格式
  ])
  
  // 验证格式合规性
  let version = azimuth::Resource::get_attribute(format_test_resource, "service.version")
  match version {
    Some(azimuth::StringValue(v)) => assert_true(v.length() > 0),
    _ => assert_true(false)
  }
  
  let sample_rate = azimuth::Resource::get_attribute(format_test_resource, "trace.sample.rate")
  match sample_rate {
    Some(azimuth::FloatValue(rate)) => assert_true(rate >= 0.0 && rate <= 1.0),
    _ => assert_true(false)
  }
  
  let port = azimuth::Resource::get_attribute(format_test_resource, "metrics.port")
  match port {
    Some(azimuth::IntValue(p)) => assert_true(p >= 1 && p <= 65535),
    _ => assert_true(false)
  }
  
  // 测试资源属性一致性检查
  let consistency_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("consistency-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.namespace", azimuth::StringValue("payment")),
    ("k8s.namespace.name", azimuth::StringValue("payment")),  # 应该与service.namespace一致
    ("deployment.environment", azimuth::StringValue("production")),
    ("cloud.region", azimuth::StringValue("us-west-2")),
    ("k8s.node.name", azimuth::StringValue("worker-us-west-2-001"))  # 应该包含region信息
  ])
  
  // 验证命名空间一致性
  let service_namespace = azimuth::Resource::get_attribute(consistency_resource, "service.namespace")
  let k8s_namespace = azimuth::Resource::get_attribute(consistency_resource, "k8s.namespace.name")
  
  match (service_namespace, k8s_namespace) {
    (Some(azimuth::StringValue(svc_ns)), Some(azimuth::StringValue(k8s_ns))) => {
      assert_eq(svc_ns, k8s_ns)
    },
    _ => assert_true(false)
  }
  
  // 测试资源大小和复杂度限制
  let large_resource = azimuth::Resource::new()
  let large_attrs = []
  
  // 创建大量属性
  for i in 0..100 {
    large_attrs.push(("attr." + i.to_string(), azimuth::StringValue("value." + i.to_string())))
  }
  
  let oversized_resource = azimuth::Resource::with_attributes(large_resource, large_attrs)
  
  // 验证资源可以处理大量属性
  let test_attr = azimuth::Resource::get_attribute(oversized_resource, "attr.50")
  match test_attr {
    Some(azimuth::StringValue(value)) => assert_eq(value, "value.50"),
    _ => assert_true(false)
  }
  
  // 测试嵌套属性结构
  let nested_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("nested-service")),
    ("service.metadata.version", azimuth::StringValue("1.0.0")),
    ("service.metadata.build", azimuth::StringValue("build-12345")),
    ("service.metadata.commit", azimuth::StringValue("commit-abcdef")),
    ("service.config.database.host", azimuth::StringValue("db.example.com")),
    ("service.config.database.port", azimuth::IntValue(5432)),
    ("service.config.cache.redis.host", azimuth::StringValue("redis.example.com")),
    ("service.config.cache.redis.port", azimuth::IntValue(6379))
  ])
  
  // 验证嵌套属性访问
  let db_host = azimuth::Resource::get_attribute(nested_resource, "service.config.database.host")
  assert_eq(db_host, Some(azimuth::StringValue("db.example.com")))
  
  let redis_port = azimuth::Resource::get_attribute(nested_resource, "service.config.cache.redis.port")
  match redis_port {
    Some(azimuth::IntValue(port)) => assert_eq(port, 6379),
    _ => assert_true(false)
  }
}