// Azimuth 配置管理测试
// 测试遥测系统的配置管理功能

// 测试1: 基本配置管理
test "基本配置管理测试" {
  // 创建基本配置
  let basic_config = Configuration({
    service_name: "azimuth-service",
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 1.0,
    export_interval_ms: 5000,
    export_timeout_ms: 30000,
    max_batch_size: 512,
    max_queue_size: 2048,
    attributes: [
      ("environment", "production"),
      ("region", "us-west-2")
    ]
  })
  
  // 验证基本配置
  assert_eq(basic_config.service_name, "azimuth-service")
  assert_eq(basic_config.service_version, "1.0.0")
  assert_eq(basic_config.enabled, true)
  assert_eq(basic_config.sampling_rate, 1.0)
  assert_eq(basic_config.export_interval_ms, 5000)
  assert_eq(basic_config.export_timeout_ms, 30000)
  assert_eq(basic_config.max_batch_size, 512)
  assert_eq(basic_config.max_queue_size, 2048)
  
  // 验证配置属性
  assert_eq(basic_config.attributes.length(), 2)
  assert_true(basic_config.attributes.contains(("environment", "production")))
  assert_true(basic_config.attributes.contains(("region", "us-west-2")))
  
  // 创建默认配置
  let default_config = Configuration.default()
  
  // 验证默认配置
  assert_eq(default_config.service_name, "unknown-service")
  assert_eq(default_config.service_version, "unknown")
  assert_eq(default_config.enabled, false) // 默认禁用
  assert_eq(default_config.sampling_rate, 1.0)
  assert_eq(default_config.export_interval_ms, 60000) // 默认60秒
  assert_eq(default_config.export_timeout_ms, 30000)
  assert_eq(default_config.max_batch_size, 512)
  assert_eq(default_config.max_queue_size, 2048)
  
  // 验证默认配置属性
  assert_true(default_config.attributes.contains(("telemetry.sdk.name", "azimuth")))
  assert_true(default_config.attributes.contains(("telemetry.sdk.language", "moonbit")))
  assert_true(default_config.attributes.contains(("telemetry.sdk.version", "1.0.0")))
}

// 测试2: 配置优先级管理
test "配置优先级管理测试" {
  // 创建不同来源的配置
  let system_config = Configuration({
    service_name: "system-service",
    service_version: "system-version",
    enabled: true,
    sampling_rate: 0.5,
    export_interval_ms: 10000,
    export_timeout_ms: 30000,
    max_batch_size: 512,
    max_queue_size: 2048,
    attributes: [
      ("environment", "system"),
      ("region", "system-region")
    ]
  })
  
  let env_config = Configuration({
    service_name: "env-service",
    service_version: "env-version",
    enabled: true,
    sampling_rate: 0.8,
    export_interval_ms: 15000,
    export_timeout_ms: 30000,
    max_batch_size: 512,
    max_queue_size: 2048,
    attributes: [
      ("environment", "env"),
      ("region", "env-region")
    ]
  })
  
  let file_config = Configuration({
    service_name: "file-service",
    service_version: "file-version",
    enabled: true,
    sampling_rate: 0.9,
    export_interval_ms: 20000,
    export_timeout_ms: 30000,
    max_batch_size: 512,
    max_queue_size: 2048,
    attributes: [
      ("environment", "file"),
      ("region", "file-region")
    ]
  })
  
  let runtime_config = Configuration({
    service_name: "runtime-service",
    service_version: "runtime-version",
    enabled: true,
    sampling_rate: 1.0,
    export_interval_ms: 25000,
    export_timeout_ms: 30000,
    max_batch_size: 512,
    max_queue_size: 2048,
    attributes: [
      ("environment", "runtime"),
      ("region", "runtime-region")
    ]
  })
  
  // 合并配置（优先级：运行时 > 文件 > 环境变量 > 系统默认）
  let merged_config = Configuration.merge([
    ("system", system_config),
    ("env", env_config),
    ("file", file_config),
    ("runtime", runtime_config)
  ])
  
  // 验证合并结果
  assert_eq(merged_config.service_name, "runtime-service") // 运行时配置优先
  assert_eq(merged_config.service_version, "runtime-version")
  assert_eq(merged_config.enabled, true)
  assert_eq(merged_config.sampling_rate, 1.0) // 运行时配置优先
  assert_eq(merged_config.export_interval_ms, 25000) // 运行时配置优先
  assert_eq(merged_config.export_timeout_ms, 30000)
  assert_eq(merged_config.max_batch_size, 512)
  assert_eq(merged_config.max_queue_size, 2048)
  
  // 验证合并后的属性
  assert_eq(merged_config.attributes.length(), 2)
  assert_true(merged_config.attributes.contains(("environment", "runtime")))
  assert_true(merged_config.attributes.contains(("region", "runtime-region")))
  
  // 测试部分配置覆盖
  let partial_runtime_config = Configuration({
    service_name: "partial-runtime-service",
    service_version: "", // 空字符串表示不覆盖
    enabled: false,     // 覆盖
    sampling_rate: 0.0, // 0.0表示不覆盖
    export_interval_ms: 0, // 0表示不覆盖
    export_timeout_ms: 0,
    max_batch_size: 0,
    max_queue_size: 0,
    attributes: [] // 空数组表示不覆盖
  })
  
  let partial_merged_config = Configuration.merge([
    ("system", system_config),
    ("runtime", partial_runtime_config)
  ])
  
  // 验证部分合并结果
  assert_eq(partial_merged_config.service_name, "partial-runtime-service") // 覆盖
  assert_eq(partial_merged_config.service_version, "system-version") // 不覆盖
  assert_eq(partial_merged_config.enabled, false) // 覆盖
  assert_eq(partial_merged_config.sampling_rate, 0.5) // 不覆盖
  assert_eq(partial_merged_config.export_interval_ms, 10000) // 不覆盖
  assert_eq(partial_merged_config.export_timeout_ms, 30000) // 不覆盖
  assert_eq(partial_merged_config.max_batch_size, 512) // 不覆盖
  assert_eq(partial_merged_config.max_queue_size, 2048) // 不覆盖
  
  // 验证部分合并后的属性
  assert_eq(partial_merged_config.attributes.length(), 2)
  assert_true(partial_merged_config.attributes.contains(("environment", "system")))
  assert_true(partial_merged_config.attributes.contains(("region", "system-region")))
}

// 测试3: 配置验证
test "配置验证测试" {
  // 创建有效配置
  let valid_config = Configuration({
    service_name: "valid-service",
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 0.5,
    export_interval_ms: 10000,
    export_timeout_ms: 30000,
    max_batch_size: 512,
    max_queue_size: 2048,
    attributes: [
      ("environment", "production"),
      ("region", "us-west-2")
    ]
  })
  
  // 验证有效配置
  let validation_result = valid_config.validate()
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // 创建无效配置
  let invalid_config = Configuration({
    service_name: "", // 空服务名
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 1.5, // 超出范围
    export_interval_ms: -1000, // 负数
    export_timeout_ms: 0, // 零
    max_batch_size: 0, // 零
    max_queue_size: 0, // 零
    attributes: [
      ("", "value"), // 空键名
      ("key", "")   // 空值
    ]
  })
  
  // 验证无效配置
  let invalid_validation_result = invalid_config.validate()
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // 验证具体错误
  let error_messages = invalid_validation_result.errors
  assert_true(error_messages.any(fn(msg) { msg.contains("service_name") }))
  assert_true(error_messages.any(fn(msg) { msg.contains("sampling_rate") }))
  assert_true(error_messages.any(fn(msg) { msg.contains("export_interval_ms") }))
  assert_true(error_messages.any(fn(msg) { msg.contains("export_timeout_ms") }))
  assert_true(error_messages.any(fn(msg) { msg.contains("max_batch_size") }))
  assert_true(error_messages.any(fn(msg) { msg.contains("max_queue_size") }))
  assert_true(error_messages.any(fn(msg) { msg.contains("attribute") }))
  
  // 测试边界值
  let boundary_config = Configuration({
    service_name: "boundary-service",
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 0.0, // 最小值
    export_interval_ms: 1000, // 最小值
    export_timeout_ms: 1000, // 最小值
    max_batch_size: 1, // 最小值
    max_queue_size: 1, // 最小值
    attributes: [
      ("key", "value")
    ]
  })
  
  // 验证边界值配置
  let boundary_validation_result = boundary_config.validate()
  assert_true(boundary_validation_result.is_valid)
  
  // 测试最大边界值
  let max_boundary_config = Configuration({
    service_name: "max-boundary-service",
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 1.0, // 最大值
    export_interval_ms: 600000, // 最大值：10分钟
    export_timeout_ms: 300000, // 最大值：5分钟
    max_batch_size: 10000, // 最大值
    max_queue_size: 100000, // 最大值
    attributes: [
      ("key", "value")
    ]
  })
  
  // 验证最大边界值配置
  let max_boundary_validation_result = max_boundary_config.validate()
  assert_true(max_boundary_validation_result.is_valid)
}

// 测试4: 动态配置更新
test "动态配置更新测试" {
  // 创建初始配置
  let initial_config = Configuration({
    service_name: "initial-service",
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 0.5,
    export_interval_ms: 10000,
    export_timeout_ms: 30000,
    max_batch_size: 512,
    max_queue_size: 2048,
    attributes: [
      ("environment", "development"),
      ("region", "us-east-1")
    ]
  })
  
  // 创建配置监听器
  let config_changes = [] : Array[ConfigurationChange]
  
  let listener = fn(change : ConfigurationChange) {
    config_changes = config_changes.push(change)
  }
  
  // 创建配置管理器
  let config_manager = ConfigurationManager.new(initial_config, listener)
  
  // 验证初始配置
  assert_eq(config_manager.get_config().service_name, "initial-service")
  assert_eq(config_manager.get_config().sampling_rate, 0.5)
  assert_eq(config_manager.get_config().export_interval_ms, 10000)
  assert_eq(config_manager.get_config().attributes.find(fn(a) { a[0] == "environment" })[1], "development")
  
  // 更新配置
  let update1 = ConfigurationUpdate({
    service_name: Some("updated-service"),
    service_version: None, // 不更新
    enabled: Some(false), // 更新
    sampling_rate: Some(0.8), // 更新
    export_interval_ms: None, // 不更新
    export_timeout_ms: Some(60000), // 更新
    max_batch_size: None, // 不更新
    max_queue_size: None, // 不更新
    attribute_updates: [
      ("environment", Some("production")), // 更新
      ("region", None), // 删除
      ("version", Some("1.1.0")) // 添加
    ]
  })
  
  config_manager.update_config(update1)
  
  // 验证更新后的配置
  let updated_config1 = config_manager.get_config()
  assert_eq(updated_config1.service_name, "updated-service")
  assert_eq(updated_config1.service_version, "1.0.0") // 未更新
  assert_eq(updated_config1.enabled, false) // 已更新
  assert_eq(updated_config1.sampling_rate, 0.8) // 已更新
  assert_eq(updated_config1.export_interval_ms, 10000) // 未更新
  assert_eq(updated_config1.export_timeout_ms, 60000) // 已更新
  assert_eq(updated_config1.max_batch_size, 512) // 未更新
  assert_eq(updated_config1.max_queue_size, 2048) // 未更新
  
  // 验证属性更新
  assert_eq(updated_config1.attributes.length(), 2)
  assert_true(updated_config1.attributes.contains(("environment", "production"))) // 已更新
  assert_false(updated_config1.attributes.any(fn(a) { a[0] == "region" })) // 已删除
  assert_true(updated_config1.attributes.contains(("version", "1.1.0"))) // 已添加
  
  // 验证配置变更通知
  assert_eq(config_changes.length(), 1)
  assert_eq(config_changes[0].field, "service_name")
  assert_eq(config_changes[0].old_value, "initial-service")
  assert_eq(config_changes[0].new_value, "updated-service")
  
  // 再次更新配置
  let update2 = ConfigurationUpdate({
    service_name: None, // 不更新
    service_version: Some("2.0.0"), // 更新
    enabled: Some(true), // 更新
    sampling_rate: None, // 不更新
    export_interval_ms: Some(20000), // 更新
    export_timeout_ms: None, // 不更新
    max_batch_size: Some(1024), // 更新
    max_queue_size: Some(4096), // 更新
    attribute_updates: [
      ("environment", Some("staging")), // 更新
      ("cluster", Some("cluster-1")) // 添加
    ]
  })
  
  config_manager.update_config(update2)
  
  // 验证第二次更新后的配置
  let updated_config2 = config_manager.get_config()
  assert_eq(updated_config2.service_name, "updated-service") // 未更新
  assert_eq(updated_config2.service_version, "2.0.0") // 已更新
  assert_eq(updated_config2.enabled, true) // 已更新
  assert_eq(updated_config2.sampling_rate, 0.8) // 未更新
  assert_eq(updated_config2.export_interval_ms, 20000) // 已更新
  assert_eq(updated_config2.export_timeout_ms, 60000) // 未更新
  assert_eq(updated_config2.max_batch_size, 1024) // 已更新
  assert_eq(updated_config2.max_queue_size, 4096) // 已更新
  
  // 验证属性更新
  assert_eq(updated_config2.attributes.length(), 3)
  assert_true(updated_config2.attributes.contains(("environment", "staging"))) // 已更新
  assert_true(updated_config2.attributes.contains(("version", "1.1.0"))) // 保留
  assert_true(updated_config2.attributes.contains(("cluster", "cluster-1"))) // 已添加
  
  // 重置配置
  config_manager.reset_config()
  
  // 验证重置后的配置
  let reset_config = config_manager.get_config()
  assert_eq(reset_config.service_name, "initial-service")
  assert_eq(reset_config.service_version, "1.0.0")
  assert_eq(reset_config.enabled, true)
  assert_eq(reset_config.sampling_rate, 0.5)
  assert_eq(reset_config.export_interval_ms, 10000)
  assert_eq(reset_config.export_timeout_ms, 30000)
  assert_eq(reset_config.max_batch_size, 512)
  assert_eq(reset_config.max_queue_size, 2048)
  assert_eq(reset_config.attributes.length(), 2)
  assert_true(reset_config.attributes.contains(("environment", "development")))
  assert_true(reset_config.attributes.contains(("region", "us-east-1")))
}

// 测试5: 配置序列化和反序列化
test "配置序列化和反序列化测试" {
  // 创建测试配置
  let test_config = Configuration({
    service_name: "test-service",
    service_version: "1.2.3",
    enabled: true,
    sampling_rate: 0.75,
    export_interval_ms: 15000,
    export_timeout_ms: 45000,
    max_batch_size: 1024,
    max_queue_size: 4096,
    attributes: [
      ("environment", "test"),
      ("region", "us-central-1"),
      ("zone", "us-central-1a"),
      ("instance_type", "t3.medium")
    ]
  })
  
  // 序列化配置为JSON
  let json_config = test_config.to_json()
  
  // 验证JSON格式
  assert_true(json_config.contains("\"service_name\":\"test-service\""))
  assert_true(json_config.contains("\"service_version\":\"1.2.3\""))
  assert_true(json_config.contains("\"enabled\":true"))
  assert_true(json_config.contains("\"sampling_rate\":0.75"))
  assert_true(json_config.contains("\"export_interval_ms\":15000"))
  assert_true(json_config.contains("\"export_timeout_ms\":45000"))
  assert_true(json_config.contains("\"max_batch_size\":1024"))
  assert_true(json_config.contains("\"max_queue_size\":4096"))
  assert_true(json_config.contains("\"environment\":\"test\""))
  assert_true(json_config.contains("\"region\":\"us-central-1\""))
  assert_true(json_config.contains("\"zone\":\"us-central-1a\""))
  assert_true(json_config.contains("\"instance_type\":\"t3.medium\""))
  
  // 从JSON反序列化配置
  let deserialized_config = Configuration.from_json(json_config)
  
  // 验证反序列化结果
  assert_eq(deserialized_config.service_name, test_config.service_name)
  assert_eq(deserialized_config.service_version, test_config.service_version)
  assert_eq(deserialized_config.enabled, test_config.enabled)
  assert_eq(deserialized_config.sampling_rate, test_config.sampling_rate)
  assert_eq(deserialized_config.export_interval_ms, test_config.export_interval_ms)
  assert_eq(deserialized_config.export_timeout_ms, test_config.export_timeout_ms)
  assert_eq(deserialized_config.max_batch_size, test_config.max_batch_size)
  assert_eq(deserialized_config.max_queue_size, test_config.max_queue_size)
  assert_eq(deserialized_config.attributes.length(), test_config.attributes.length())
  
  for attr in test_config.attributes {
    assert_true(deserialized_config.attributes.contains(attr))
  }
  
  // 序列化配置为YAML
  let yaml_config = test_config.to_yaml()
  
  // 验证YAML格式
  assert_true(yaml_config.contains("service_name: test-service"))
  assert_true(yaml_config.contains("service_version: 1.2.3"))
  assert_true(yaml_config.contains("enabled: true"))
  assert_true(yaml_config.contains("sampling_rate: 0.75"))
  assert_true(yaml_config.contains("export_interval_ms: 15000"))
  assert_true(yaml_config.contains("export_timeout_ms: 45000"))
  assert_true(yaml_config.contains("max_batch_size: 1024"))
  assert_true(yaml_config.contains("max_queue_size: 4096"))
  assert_true(yaml_config.contains("environment: test"))
  assert_true(yaml_config.contains("region: us-central-1"))
  assert_true(yaml_config.contains("zone: us-central-1a"))
  assert_true(yaml_config.contains("instance_type: t3.medium"))
  
  // 从YAML反序列化配置
  let deserialized_yaml_config = Configuration.from_yaml(yaml_config)
  
  // 验证YAML反序列化结果
  assert_eq(deserialized_yaml_config.service_name, test_config.service_name)
  assert_eq(deserialized_yaml_config.service_version, test_config.service_version)
  assert_eq(deserialized_yaml_config.enabled, test_config.enabled)
  assert_eq(deserialized_yaml_config.sampling_rate, test_config.sampling_rate)
  assert_eq(deserialized_yaml_config.export_interval_ms, test_config.export_interval_ms)
  assert_eq(deserialized_yaml_config.export_timeout_ms, test_config.export_timeout_ms)
  assert_eq(deserialized_yaml_config.max_batch_size, test_config.max_batch_size)
  assert_eq(deserialized_yaml_config.max_queue_size, test_config.max_queue_size)
  assert_eq(deserialized_yaml_config.attributes.length(), test_config.attributes.length())
  
  for attr in test_config.attributes {
    assert_true(deserialized_yaml_config.attributes.contains(attr))
  }
  
  // 测试无效JSON反序列化
  let invalid_json = "{ invalid json }"
  let invalid_json_result = Configuration.from_json(invalid_json)
  assert_eq(invalid_json_result.service_name, "unknown-service") // 回退到默认值
  
  // 测试无效YAML反序列化
  let invalid_yaml = "invalid: yaml: content:"
  let invalid_yaml_result = Configuration.from_yaml(invalid_yaml)
  assert_eq(invalid_yaml_result.service_name, "unknown-service") // 回退到默认值
}

// 测试6: 配置模板和继承
test "配置模板和继承测试" {
  // 创建基础配置模板
  let base_template = ConfigurationTemplate({
    name: "base-template",
    description: "Base configuration template for all services",
    config: Configuration({
      service_name: "", // 占位符，由子模板填充
      service_version: "1.0.0",
      enabled: true,
      sampling_rate: 1.0,
      export_interval_ms: 60000,
      export_timeout_ms: 30000,
      max_batch_size: 512,
      max_queue_size: 2048,
      attributes: [
        ("telemetry.sdk.name", "azimuth"),
        ("telemetry.sdk.language", "moonbit"),
        ("telemetry.sdk.version", "1.0.0")
      ]
    }),
    variables: [
      ("service_name", "Service name", true, ""),
      ("environment", "Environment", true, "development"),
      ("region", "Region", true, "us-west-2")
    ]
  })
  
  // 创建生产环境模板
  let production_template = ConfigurationTemplate({
    name: "production-template",
    description: "Production environment configuration template",
    config: Configuration({
      service_name: "", // 占位符，由实例填充
      service_version: "1.0.0",
      enabled: true,
      sampling_rate: 0.1, // 生产环境采样率较低
      export_interval_ms: 30000, // 生产环境导出间隔较短
      export_timeout_ms: 30000,
      max_batch_size: 1024, // 生产环境批处理大小较大
      max_queue_size: 4096, // 生产环境队列大小较大
      attributes: [
        ("environment", "production"),
        ("telemetry.sdk.name", "azimuth"),
        ("telemetry.sdk.language", "moonbit"),
        ("telemetry.sdk.version", "1.0.0")
      ]
    }),
    variables: [
      ("service_name", "Service name", true, ""),
      ("region", "Region", true, "us-west-2"),
      ("cluster", "Cluster name", false, "")
    ]
  })
  
  // 从模板创建配置
  let template_values = [
    ("service_name", "user-service"),
    ("environment", "production"),
    ("region", "us-east-1"),
    ("cluster", "prod-cluster-1")
  ]
  
  let config_from_template = Configuration.from_template(production_template, template_values)
  
  // 验证从模板创建的配置
  assert_eq(config_from_template.service_name, "user-service")
  assert_eq(config_from_template.service_version, "1.0.0")
  assert_eq(config_from_template.enabled, true)
  assert_eq(config_from_template.sampling_rate, 0.1)
  assert_eq(config_from_template.export_interval_ms, 30000)
  assert_eq(config_from_template.export_timeout_ms, 30000)
  assert_eq(config_from_template.max_batch_size, 1024)
  assert_eq(config_from_template.max_queue_size, 4096)
  
  // 验证模板变量替换
  assert_eq(config_from_template.attributes.length(), 4)
  assert_true(config_from_template.attributes.contains(("environment", "production")))
  assert_true(config_from_template.attributes.contains(("region", "us-east-1")))
  assert_true(config_from_template.attributes.contains(("cluster", "prod-cluster-1")))
  assert_true(config_from_template.attributes.contains(("telemetry.sdk.name", "azimuth")))
  
  // 测试模板继承
  let inherited_template = ConfigurationTemplate.inherit(
    base_template,
    "inherited-template",
    "Inherited configuration template",
    Configuration({
      service_name: "",
      service_version: "2.0.0", // 覆盖基模板
      enabled: true,
      sampling_rate: 0.5, // 覆盖基模板
      export_interval_ms: 60000, // 继承基模板
      export_timeout_ms: 60000, // 覆盖基模板
      max_batch_size: 512, // 继承基模板
      max_queue_size: 2048, // 继承基模板
      attributes: [
        ("environment", "staging"), // 覆盖基模板
        ("telemetry.sdk.name", "azimuth"), // 继承基模板
        ("telemetry.sdk.language", "moonbit"), // 继承基模板
        ("telemetry.sdk.version", "2.0.0") // 覆盖基模板
      ]
    }),
    [
      ("service_name", "Service name", true, ""),
      ("environment", "Environment", true, "staging"),
      ("region", "Region", true, "us-west-2"),
      ("feature_flag", "Feature flag", false, "")
    ]
  )
  
  // 验证继承的模板
  assert_eq(inherited_template.name, "inherited-template")
  assert_eq(inherited_template.config.service_version, "2.0.0") // 覆盖的值
  assert_eq(inherited_template.config.sampling_rate, 0.5) // 覆盖的值
  assert_eq(inherited_template.config.export_interval_ms, 60000) // 继承的值
  assert_eq(inherited_template.config.export_timeout_ms, 60000) // 覆盖的值
  assert_eq(inherited_template.config.max_batch_size, 512) // 继承的值
  assert_eq(inherited_template.config.max_queue_size, 2048) // 继承的值
  
  // 验证继承的属性
  assert_true(inherited_template.config.attributes.contains(("environment", "staging"))) // 覆盖的值
  assert_true(inherited_template.config.attributes.contains(("telemetry.sdk.name", "azimuth"))) // 继承的值
  assert_true(inherited_template.config.attributes.contains(("telemetry.sdk.language", "moonbit"))) // 继承的值
  assert_true(inherited_template.config.attributes.contains(("telemetry.sdk.version", "2.0.0"))) // 覆盖的值
  
  // 验证继承的变量
  assert_eq(inherited_template.variables.length(), 4)
  assert_true(inherited_template.variables.any(fn(v) { v[0] == "service_name" }))
  assert_true(inherited_template.variables.any(fn(v) { v[0] == "environment" }))
  assert_true(inherited_template.variables.any(fn(v) { v[0] == "region" }))
  assert_true(inherited_template.variables.any(fn(v) { v[0] == "feature_flag" }))
}