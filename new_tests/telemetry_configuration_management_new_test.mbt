// 遥测配置管理测试用例

test "telemetry_configuration_creation_and_loading" {
  // 测试遥测配置的创建和加载
  
  // 创建默认配置
  let default_config = azimuth::telemetry::api::config::TelemetryConfig::default()
  assert_eq(default_config.service_name, "unknown-service")
  assert_eq(default_config.sampling_rate, 1.0)
  assert_eq(default_config.export_interval, 60000)  // 60秒
  assert_eq(default_config.batch_size, 512)
  
  // 创建自定义配置
  let custom_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("payment-service")
    .with_service_version("2.1.0")
    .with_sampling_rate(0.1)
    .with_export_interval(30000)  // 30秒
    .with_batch_size(1024)
    .with_endpoint("https://otel-collector.example.com:4317")
  
  assert_eq(custom_config.service_name, "payment-service")
  assert_eq(custom_config.service_version, "2.1.0")
  assert_eq(custom_config.sampling_rate, 0.1)
  assert_eq(custom_config.export_interval, 30000)
  assert_eq(custom_config.batch_size, 1024)
  assert_eq(custom_config.endpoint, "https://otel-collector.example.com:4317")
}

test "telemetry_configuration_serialization_and_deserialization" {
  // 测试遥测配置的序列化和反序列化
  
  // 创建复杂配置
  let original_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("user-service")
    .with_service_version("1.5.2")
    .with_environment("production")
    .with_sampling_rate(0.05)
    .with_export_interval(15000)
    .with_batch_size(2048)
    .with_endpoint("https://otel.example.com:4317")
    .with_headers(["Authorization: Bearer token123", "X-Service: user-api"])
    .with_resource_attributes({
      "service.instance.id": "instance-abc-123",
      "deployment.environment": "prod",
      "host.name": "prod-server-01"
    })
    .with_enabled_signals(["traces", "metrics", "logs"])
  
  // 序列化为JSON
  let json_config = original_config.to_json()
  assert_eq(json_config.length() > 0, true)
  assert_eq(json_config.contains("user-service"), true)
  assert_eq(json_config.contains("1.5.2"), true)
  
  // 从JSON反序列化
  let deserialized_config = azimuth::telemetry::api::config::TelemetryConfig::from_json(json_config)
  assert_eq(deserialized_config.service_name, original_config.service_name)
  assert_eq(deserialized_config.service_version, original_config.service_version)
  assert_eq(deserialized_config.environment, original_config.environment)
  assert_eq(deserialized_config.sampling_rate, original_config.sampling_rate)
  assert_eq(deserialized_config.export_interval, original_config.export_interval)
  assert_eq(deserialized_config.batch_size, original_config.batch_size)
  assert_eq(deserialized_config.endpoint, original_config.endpoint)
}

test "telemetry_configuration_validation" {
  // 测试遥测配置的验证
  
  // 测试有效配置
  let valid_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("valid-service")
    .with_sampling_rate(0.5)
    .with_export_interval(10000)
    .with_batch_size(100)
  
  let validation_result = valid_config.validate()
  assert_eq(validation_result.is_valid, true)
  assert_eq(validation_result.errors.length(), 0)
  
  // 测试无效采样率
  let invalid_sampling_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("invalid-sampling-service")
    .with_sampling_rate(1.5)  // 超出范围 [0.0, 1.0]
  
  let sampling_validation = invalid_sampling_config.validate()
  assert_eq(sampling_validation.is_valid, false)
  assert_eq(sampling_validation.errors.length() > 0, true)
  assert_eq(sampling_validation.errors.any(|err| err.contains("sampling_rate")), true)
  
  // 测试无效导出间隔
  let invalid_interval_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("invalid-interval-service")
    .with_export_interval(-1000)  // 负数
  
  let interval_validation = invalid_interval_config.validate()
  assert_eq(interval_validation.is_valid, false)
  assert_eq(interval_validation.errors.any(|err| err.contains("export_interval")), true)
  
  // 测试无效批量大小
  let invalid_batch_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("invalid-batch-service")
    .with_batch_size(0)  // 零
  
  let batch_validation = invalid_batch_config.validate()
  assert_eq(batch_validation.is_valid, false)
  assert_eq(batch_validation.errors.any(|err| err.contains("batch_size")), true)
}

test "telemetry_configuration_inheritance_and_overrides" {
  // 测试遥测配置的继承和覆盖
  
  // 创建基础配置
  let base_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("base-service")
    .with_service_version("1.0.0")
    .with_environment("development")
    .with_sampling_rate(1.0)
    .with_export_interval(60000)
    .with_batch_size(512)
    .with_endpoint("http://localhost:4317")
  
  // 创建覆盖配置
  let override_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("override-service")  // 覆盖服务名
    .with_environment("production")          // 覆盖环境
    .with_sampling_rate(0.1)                 // 覆盖采样率
  
  // 合并配置
  let merged_config = base_config.merge(override_config)
  
  // 验证合并结果
  assert_eq(merged_config.service_name, "override-service")  // 被覆盖
  assert_eq(merged_config.service_version, "1.0.0")          // 继承自基础配置
  assert_eq(merged_config.environment, "production")         // 被覆盖
  assert_eq(merged_config.sampling_rate, 0.1)                 // 被覆盖
  assert_eq(merged_config.export_interval, 60000)            // 继承自基础配置
  assert_eq(merged_config.batch_size, 512)                   // 继承自基础配置
  assert_eq(merged_config.endpoint, "http://localhost:4317") // 继承自基础配置
}

test "telemetry_configuration_hot_reload" {
  // 测试遥测配置的热重载
  
  // 创建初始配置
  let initial_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("hot-reload-service")
    .with_sampling_rate(1.0)
    .with_export_interval(60000)
    .with_batch_size(512)
  
  // 创建配置管理器
  let config_manager = azimuth::telemetry::api::config::ConfigManager::new(initial_config)
  
  // 验证初始配置
  let current_config = config_manager.get_current_config()
  assert_eq(current_config.sampling_rate, 1.0)
  assert_eq(current_config.export_interval, 60000)
  assert_eq(current_config.batch_size, 512)
  
  // 创建更新配置
  let updated_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("hot-reload-service")
    .with_sampling_rate(0.2)
    .with_export_interval(30000)
    .with_batch_size(1024)
  
  // 热重载配置
  let reload_result = config_manager.reload_config(updated_config)
  assert_eq(reload_result.success, true)
  
  // 验证配置已更新
  let new_current_config = config_manager.get_current_config()
  assert_eq(new_current_config.sampling_rate, 0.2)
  assert_eq(new_current_config.export_interval, 30000)
  assert_eq(new_current_config.batch_size, 1024)
  
  // 验证配置变更历史
  let config_history = config_manager.get_config_history()
  assert_eq(config_history.length(), 2)
  assert_eq(config_history[0].sampling_rate, 1.0)
  assert_eq(config_history[1].sampling_rate, 0.2)
}

test "telemetry_configuration_environment_specific" {
  // 测试环境特定的遥测配置
  
  // 创建开发环境配置
  let dev_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("multi-env-service")
    .with_environment("development")
    .with_sampling_rate(1.0)  // 开发环境全采样
    .with_export_interval(10000)  // 频繁导出
    .with_endpoint("http://dev-collector:4317")
    .with_debug_enabled(true)
  
  // 创建生产环境配置
  let prod_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("multi-env-service")
    .with_environment("production")
    .with_sampling_rate(0.01)  // 生产环境低采样率
    .with_export_interval(60000)  // 较少导出频率
    .with_endpoint("https://prod-collector.example.com:4317")
    .with_debug_enabled(false)
  
  // 创建环境配置管理器
  let env_config_manager = azimuth::telemetry::api::config::EnvironmentConfigManager::new()
    .with_environment_config("development", dev_config)
    .with_environment_config("production", prod_config)
  
  // 获取开发环境配置
  let dev_active_config = env_config_manager.get_config_for_environment("development")
  assert_eq(dev_active_config.sampling_rate, 1.0)
  assert_eq(dev_active_config.export_interval, 10000)
  assert_eq(dev_active_config.endpoint, "http://dev-collector:4317")
  assert_eq(dev_active_config.debug_enabled, true)
  
  // 获取生产环境配置
  let prod_active_config = env_config_manager.get_config_for_environment("production")
  assert_eq(prod_active_config.sampling_rate, 0.01)
  assert_eq(prod_active_config.export_interval, 60000)
  assert_eq(prod_active_config.endpoint, "https://prod-collector.example.com:4317")
  assert_eq(prod_active_config.debug_enabled, false)
}

test "telemetry_configuration_feature_flags" {
  // 测试遥测配置的功能开关
  
  // 创建带功能开关的配置
  let feature_config = azimuth::telemetry::api::config::TelemetryConfig::new()
    .with_service_name("feature-flag-service")
    .with_feature_flag("experimental_metrics", true)
    .with_feature_flag("advanced_tracing", false)
    .with_feature_flag("real_time_dashboard", true)
    .with_feature_flag("debug_logging", false)
  
  // 创建功能开关管理器
  let feature_manager = azimuth::telemetry::api::config::FeatureFlagManager::new(feature_config)
  
  // 测试功能开关状态
  assert_eq(feature_manager.is_enabled("experimental_metrics"), true)
  assert_eq(feature_manager.is_enabled("advanced_tracing"), false)
  assert_eq(feature_manager.is_enabled("real_time_dashboard"), true)
  assert_eq(feature_manager.is_enabled("debug_logging"), false)
  assert_eq(feature_manager.is_enabled("non_existent_feature"), false)  // 默认false
  
  // 动态更新功能开关
  let updated_manager = feature_manager
    .set_feature_flag("advanced_tracing", true)  // 启用
    .set_feature_flag("debug_logging", true)     // 启用
    .set_feature_flag("experimental_metrics", false)  // 禁用
  
  // 验证更新后的状态
  assert_eq(updated_manager.is_enabled("experimental_metrics"), false)
  assert_eq(updated_manager.is_enabled("advanced_tracing"), true)
  assert_eq(updated_manager.is_enabled("real_time_dashboard"), true)  // 保持不变
  assert_eq(updated_manager.is_enabled("debug_logging"), true)
  
  // 获取所有启用的功能
  let enabled_features = updated_manager.get_enabled_features()
  assert_eq(enabled_features.contains("advanced_tracing"), true)
  assert_eq(enabled_features.contains("real_time_dashboard"), true)
  assert_eq(enabled_features.contains("debug_logging"), true)
  assert_eq(enabled_features.contains("experimental_metrics"), false)
}