// 遥测配置管理测试
// 测试Azimuth遥测系统的配置管理功能

test "基本配置创建和验证" {
  // 创建基本遥测配置
  let config = TelemetryConfig::new()
    .with_service_name("test-service")
    .with_service_version("1.0.0")
    .with_environment("test")
    .with_sampling_rate(0.1)
    .with_export_interval(60000)
    .with_batch_size(100)
    .with_max_attributes(32)
    .with_max_events(128)
    .with_max_links(32)
    .with_enabled_metrics(true)
    .with_enabled_tracing(true)
    .with_enabled_logging(true)
  
  // 验证基本配置属性
  assert_eq(config.get_service_name(), "test-service")
  assert_eq(config.get_service_version(), "1.0.0")
  assert_eq(config.get_environment(), "test")
  assert_eq(config.get_sampling_rate(), 0.1)
  assert_eq(config.get_export_interval(), 60000)
  assert_eq(config.get_batch_size(), 100)
  assert_eq(config.get_max_attributes(), 32)
  assert_eq(config.get_max_events(), 128)
  assert_eq(config.get_max_links(), 32)
  assert_true(config.get_enabled_metrics())
  assert_true(config.get_enabled_tracing())
  assert_true(config.get_enabled_logging())
  
  // 测试配置验证
  let validation_result = config.validate()
  assert_true(validation_result.is_valid())
  assert_eq(validation_result.get_errors().length(), 0)
  
  // 测试无效配置
  let invalid_config = TelemetryConfig::new()
    .with_sampling_rate(-0.1)  // 无效的负采样率
    .with_export_interval(0)    // 无效的导出间隔
    .with_batch_size(0)         // 无效的批量大小
    .with_max_attributes(-1)    // 无效的最大属性数
  
  let invalid_validation = invalid_config.validate()
  assert_false(invalid_validation.is_valid())
  assert_true(invalid_validation.get_errors().length() >= 4)
  
  // 验证具体错误信息
  let errors = invalid_validation.get_errors()
  assert_true(errors.any(fn(err) { err.contains("sampling_rate") }))
  assert_true(errors.any(fn(err) { err.contains("export_interval") }))
  assert_true(errors.any(fn(err) { err.contains("batch_size") }))
  assert_true(errors.any(fn(err) { err.contains("max_attributes") }))
}

test "配置文件加载和解析" {
  // 测试JSON配置文件加载
  let json_config_content = "{
    \"service_name\": \"json-test-service\",
    \"service_version\": \"2.1.0\",
    \"environment\": \"production\",
    \"sampling_rate\": 0.15,
    \"export_interval\": 30000,
    \"batch_size\": 200,
    \"max_attributes\": 64,
    \"max_events\": 256,
    \"max_links\": 64,
    \"enabled_metrics\": true,
    \"enabled_tracing\": true,
    \"enabled_logging\": false,
    \"exporters\": {
      \"otlp\": {
        \"endpoint\": \"https://otlp.example.com:4317\",
        \"headers\": {
          \"Authorization\": \"Bearer token123\"
        },
        \"timeout\": 10000
      },
      \"prometheus\": {
        \"port\": 9090,
        \"path\": \"/metrics\"
      }
    },
    \"processors\": {
      \"batch\": {
        \"max_export_batch_size\": 512,
        \"export_timeout_millis\": 30000,
        \"max_export_buffer_size\": 2048
      },
      \"resource\": {
        \"attributes\": {
          \"deployment.region\": \"us-west-2\",
          \"deployment.zone\": \"us-west-2a\"
        }
      }
    }
  }"
  
  // 创建临时配置文件
  let config_file = "/tmp/telemetry_config.json"
  let write_result = File::write_string(config_file, json_config_content)
  assert_true(write_result.is_ok())
  
  // 加载JSON配置
  let config_loader = ConfigLoader::new()
  let loaded_config = config_loader.load_from_file(config_file)
  
  assert_true(loaded_config.is_ok())
  
  let config = loaded_config.unwrap()
  assert_eq(config.get_service_name(), "json-test-service")
  assert_eq(config.get_service_version(), "2.1.0")
  assert_eq(config.get_environment(), "production")
  assert_eq(config.get_sampling_rate(), 0.15)
  assert_eq(config.get_export_interval(), 30000)
  assert_eq(config.get_batch_size(), 200)
  assert_eq(config.get_max_attributes(), 64)
  assert_eq(config.get_max_events(), 256)
  assert_eq(config.get_max_links(), 64)
  assert_true(config.get_enabled_metrics())
  assert_true(config.get_enabled_tracing())
  assert_false(config.get_enabled_logging())
  
  // 验证导出器配置
  let exporters = config.get_exporters()
  assert_true(exporters.contains_key("otlp"))
  assert_true(exporters.contains_key("prometheus"))
  
  let otlp_config = exporters.get("otlp")
  assert_eq(otp_config.get("endpoint"), "https://otlp.example.com:4317")
  assert_eq(otlp_config.get("headers").get("Authorization"), "Bearer token123")
  assert_eq(otlp_config.get("timeout"), "10000")
  
  // 验证处理器配置
  let processors = config.get_processors()
  assert_true(processors.contains_key("batch"))
  assert_true(processors.contains_key("resource"))
  
  let batch_config = processors.get("batch")
  assert_eq(batch_config.get("max_export_batch_size"), "512")
  assert_eq(batch_config.get("export_timeout_millis"), "30000")
  assert_eq(batch_config.get("max_export_buffer_size"), "2048")
  
  // 清理临时文件
  let cleanup_result = File::remove(config_file)
  assert_true(cleanup_result.is_ok())
}

test "动态配置更新和热重载" {
  // 创建初始配置
  let initial_config = TelemetryConfig::new()
    .with_service_name("dynamic-test-service")
    .with_sampling_rate(0.1)
    .with_export_interval(60000)
    .with_enabled_metrics(true)
    .with_enabled_tracing(true)
    .with_enabled_logging(true)
  
  // 创建配置管理器
  let config_manager = ConfigManager::new(initial_config)
  
  // 验证初始配置
  let current_config = config_manager.get_current_config()
  assert_eq(current_config.get_sampling_rate(), 0.1)
  assert_eq(current_config.get_export_interval(), 60000)
  assert_true(current_config.get_enabled_metrics())
  
  // 测试动态更新采样率
  let update_result1 = config_manager.update_config("sampling_rate", "0.2")
  assert_true(update_result1.is_success())
  
  let updated_config1 = config_manager.get_current_config()
  assert_eq(updated_config1.get_sampling_rate(), 0.2)
  assert_eq(updated_config1.get_export_interval(), 60000)  // 其他配置保持不变
  
  // 测试批量更新
  let batch_updates = [
    ("export_interval", "30000"),
    ("batch_size", "200"),
    ("enabled_logging", "false")
  ]
  
  let batch_result = config_manager.batch_update_config(batch_updates)
  assert_true(batch_result.is_success())
  
  let updated_config2 = config_manager.get_current_config()
  assert_eq(updated_config2.get_sampling_rate(), 0.2)  // 保持之前更新
  assert_eq(updated_config2.get_export_interval(), 30000)
  assert_eq(updated_config2.get_batch_size(), 200)
  assert_false(updated_config2.get_enabled_logging())
  
  // 测试配置变更监听器
  let mut received_updates = []
  
  let listener = ConfigChangeListener::new("test-listener", fn(key, old_value, new_value) {
    received_updates = received_updates.push((key, old_value, new_value))
  })
  
  let listener_id = config_manager.add_listener(listener)
  assert_true(listener_id.is_some())
  
  // 触发配置更新
  let update_result2 = config_manager.update_config("max_attributes", "64")
  assert_true(update_result2.is_success())
  
  // 验证监听器收到通知
  assert_eq(received_updates.length(), 1)
  let (key, old_value, new_value) = received_updates.get(0)
  assert_eq(key, "max_attributes")
  assert_eq(old_value, "32")  // 默认值
  assert_eq(new_value, "64")
  
  // 移除监听器
  let remove_result = config_manager.remove_listener(listener_id.some())
  assert_true(remove_result.is_success())
  
  // 再次更新配置，监听器不应该收到通知
  let update_result3 = config_manager.update_config("max_events", "256")
  assert_true(update_result3.is_success())
  
  assert_eq(received_updates.length(), 1)  // 仍然是1，没有新的通知
}

test "配置环境变量覆盖" {
  // 设置环境变量
  let env_vars = [
    ("AZIMUTH_SERVICE_NAME", "env-override-service"),
    ("AZIMUTH_SERVICE_VERSION", "3.2.1"),
    ("AZIMUTH_ENVIRONMENT", "staging"),
    ("AZIMUTH_SAMPLING_RATE", "0.25"),
    ("AZIMUTH_EXPORT_INTERVAL", "45000"),
    ("AZIMUTH_ENABLED_METRICS", "false"),
    ("AZIMUTH_ENABLED_TRACING", "true"),
    ("AZIMUTH_ENABLED_LOGGING", "false")
  ]
  
  for (key, value) in env_vars {
    let set_result = Env::set_var(key, value)
    assert_true(set_result.is_ok())
  }
  
  // 创建基础配置
  let base_config = TelemetryConfig::new()
    .with_service_name("base-service")
    .with_service_version("1.0.0")
    .with_environment("development")
    .with_sampling_rate(0.1)
    .with_export_interval(60000)
    .with_enabled_metrics(true)
    .with_enabled_tracing(true)
    .with_enabled_logging(true)
  
  // 应用环境变量覆盖
  let config_with_env_overrides = ConfigEnvironmentOverrider::apply_overrides(base_config)
  
  // 验证环境变量覆盖生效
  assert_eq(config_with_env_overrides.get_service_name(), "env-override-service")
  assert_eq(config_with_env_overrides.get_service_version(), "3.2.1")
  assert_eq(config_with_env_overrides.get_environment(), "staging")
  assert_eq(config_with_env_overrides.get_sampling_rate(), 0.25)
  assert_eq(config_with_env_overrides.get_export_interval(), 45000)
  assert_false(config_with_env_overrides.get_enabled_metrics())
  assert_true(config_with_env_overrides.get_enabled_tracing())
  assert_false(config_with_env_overrides.get_enabled_logging())
  
  // 测试环境变量优先级（环境变量 > 配置文件 > 默认值）
  let json_config_content = "{
    \"service_name\": \"file-service\",
    \"service_version\": \"2.0.0\",
    \"environment\": \"production\",
    \"sampling_rate\": 0.15,
    \"enabled_metrics\": false
  }"
  
  let config_file = "/tmp/priority_test_config.json"
  let write_result = File::write_string(config_file, json_config_content)
  assert_true(write_result.is_ok())
  
  let config_loader = ConfigLoader::new()
  let file_config = config_loader.load_from_file(config_file)
  assert_true(file_config.is_ok())
  
  let config_with_env_and_file = ConfigEnvironmentOverrider::apply_overrides(file_config.unwrap())
  
  // 验证环境变量覆盖了文件配置
  assert_eq(config_with_env_and_file.get_service_name(), "env-override-service")  // 来自环境变量
  assert_eq(config_with_env_and_file.get_service_version(), "3.2.1")              // 来自环境变量
  assert_eq(config_with_env_and_file.get_environment(), "staging")               // 来自环境变量
  assert_eq(config_with_env_and_file.get_sampling_rate(), 0.25)                  // 来自环境变量
  assert_false(config_with_env_and_file.get_enabled_metrics())                    // 来自环境变量
  
  // 清理
  let cleanup_result = File::remove(config_file)
  assert_true(cleanup_result.is_ok())
  
  for (key, _) in env_vars {
    let unset_result = Env::remove_var(key)
    assert_true(unset_result.is_ok())
  }
}

test "配置模板和继承" {
  // 创建基础配置模板
  let base_template = ConfigTemplate::new("base")
    .with_field("sampling_rate", "0.1")
    .with_field("export_interval", "60000")
    .with_field("batch_size", "100")
    .with_field("max_attributes", "32")
    .with_field("enabled_metrics", "true")
    .with_field("enabled_tracing", "true")
    .with_field("enabled_logging", "true")
  
  // 创建生产环境模板
  let production_template = ConfigTemplate::new("production")
    .with_parent("base")
    .with_field("environment", "production")
    .with_field("sampling_rate", "0.05")  // 生产环境降低采样率
    .with_field("export_interval", "30000")  // 更频繁的导出
    .with_field("batch_size", "200")  // 更大的批量
  
  // 创建开发环境模板
  let development_template = ConfigTemplate::new("development")
    .with_parent("base")
    .with_field("environment", "development")
    .with_field("sampling_rate", "1.0")  // 开发环境全采样
    .with_field("enabled_logging", "true")
  
  // 创建测试环境模板
  let testing_template = ConfigTemplate::new("testing")
    .with_parent("development")
    .with_field("environment", "testing")
    .with_field("export_interval", "15000")  // 测试环境更频繁导出
  
  // 注册模板
  let template_manager = ConfigTemplateManager::new()
  template_manager.register_template(base_template)
  template_manager.register_template(production_template)
  template_manager.register_template(development_template)
  template_manager.register_template(testing_template)
  
  // 验证模板继承关系
  let prod_inheritance = template_manager.get_inheritance_chain("production")
  assert_eq(prod_inheritance, ["production", "base"])
  
  let test_inheritance = template_manager.get_inheritance_chain("testing")
  assert_eq(test_inheritance, ["testing", "development", "base"])
  
  // 从模板生成配置
  let prod_config = template_manager.generate_config("production")
    .with_service_name("prod-service")
    .with_service_version("1.0.0")
  
  assert_eq(prod_config.get_environment(), "production")
  assert_eq(prod_config.get_sampling_rate(), 0.05)  // 来自生产模板
  assert_eq(prod_config.get_export_interval(), 30000)  // 来自生产模板
  assert_eq(prod_config.get_batch_size(), 200)  // 来自生产模板
  assert_eq(prod_config.get_max_attributes(), 32)  // 来自基础模板
  assert_true(prod_config.get_enabled_metrics())  // 来自基础模板
  assert_true(prod_config.get_enabled_tracing())  // 来自基础模板
  assert_true(prod_config.get_enabled_logging())  // 来自基础模板
  
  let test_config = template_manager.generate_config("testing")
    .with_service_name("test-service")
    .with_service_version("1.0.0")
  
  assert_eq(test_config.get_environment(), "testing")  // 来自测试模板
  assert_eq(test_config.get_sampling_rate(), 1.0)  // 来自开发模板（父模板）
  assert_eq(test_config.get_export_interval(), 15000)  // 来自测试模板
  assert_eq(test_config.get_batch_size(), 100)  // 来自基础模板
  assert_true(test_config.get_enabled_logging())  // 来自开发模板
  
  // 测试模板更新和重新生成
  let update_result = template_manager.update_template_field("base", "batch_size", "150")
  assert_true(update_result.is_success())
  
  let regenerated_prod_config = template_manager.regenerate_config("production", prod_config)
  assert_eq(regenerated_prod_config.get_batch_size(), 150)  // 应该反映基础模板的更新
  
  // 测试模板验证
  let validation_result = template_manager.validate_all_templates()
  assert_true(validation_result.is_valid())
  assert_eq(validation_result.get_errors().length(), 0)
}

test "配置持久化和恢复" {
  // 创建配置管理器并设置配置
  let initial_config = TelemetryConfig::new()
    .with_service_name("persistence-test-service")
    .with_service_version("1.5.0")
    .with_environment("test")
    .with_sampling_rate(0.12)
    .with_export_interval(45000)
    .with_batch_size(150)
    .with_max_attributes(48)
    .with_enabled_metrics(true)
    .with_enabled_tracing(false)
    .with_enabled_logging(true)
  
  let config_manager = ConfigManager::new(initial_config)
  
  // 测试配置持久化
  let persist_result = config_manager.persist_config("/tmp/test_telemetry_config.json")
  assert_true(persist_result.is_success())
  
  // 验证配置文件存在
  let file_exists = File::exists("/tmp/test_telemetry_config.json")
  assert_true(file_exists)
  
  // 修改配置
  let update_result = config_manager.update_config("sampling_rate", "0.25")
  assert_true(update_result.is_success())
  
  let modified_config = config_manager.get_current_config()
  assert_eq(modified_config.get_sampling_rate(), 0.25)
  
  // 测试配置恢复
  let restore_result = config_manager.restore_config("/tmp/test_telemetry_config.json")
  assert_true(restore_result.is_success())
  
  let restored_config = config_manager.get_current_config()
  assert_eq(restored_config.get_sampling_rate(), 0.12)  // 恢复为持久化的值
  assert_eq(restored_config.get_service_name(), "persistence-test-service")
  assert_eq(restored_config.get_service_version(), "1.5.0")
  assert_eq(restored_config.get_environment(), "test")
  assert_eq(restored_config.get_export_interval(), 45000)
  assert_eq(restored_config.get_batch_size(), 150)
  assert_eq(restored_config.get_max_attributes(), 48)
  assert_true(restored_config.get_enabled_metrics())
  assert_false(restored_config.get_enabled_tracing())
  assert_true(restored_config.get_enabled_logging())
  
  // 测试自动持久化功能
  let auto_persist_result = config_manager.enable_auto_persist("/tmp/auto_telemetry_config.json", 5000)  // 每5秒自动持久化
  assert_true(auto_persist_result.is_success())
  
  // 修改配置触发自动持久化
  let auto_update_result = config_manager.update_config("max_events", "300")
  assert_true(auto_update_result.is_success())
  
  // 等待自动持久化（在实际环境中可能需要等待）
  let wait_result = Thread::sleep(6000)  // 等待6秒确保持久化完成
  
  // 验证自动持久化文件存在
  let auto_file_exists = File::exists("/tmp/auto_telemetry_config.json")
  assert_true(auto_file_exists)
  
  // 创建新的配置管理器并加载自动持久化的配置
  let new_config_manager = ConfigManager::new_default()
  let auto_restore_result = new_config_manager.restore_config("/tmp/auto_telemetry_config.json")
  assert_true(auto_restore_result.is_success())
  
  let auto_restored_config = new_config_manager.get_current_config()
  assert_eq(auto_restored_config.get_max_events(), 300)  // 应该包含最新的更改
  
  // 测试配置备份和恢复
  let backup_result = config_manager.create_backup("/tmp/telemetry_config_backup.json")
  assert_true(backup_result.is_success())
  
  // 进行多次配置更改
  config_manager.update_config("sampling_rate", "0.3")
  config_manager.update_config("export_interval", "60000")
  config_manager.update_config("enabled_tracing", "true")
  
  // 从备份恢复
  let backup_restore_result = config_manager.restore_from_backup("/tmp/telemetry_config_backup.json")
  assert_true(backup_restore_result.is_success())
  
  let backup_restored_config = config_manager.get_current_config()
  assert_eq(backup_restored_config.get_sampling_rate(), 0.12)  // 应该恢复到备份时的状态
  assert_eq(backup_restored_config.get_export_interval(), 45000)
  assert_false(backup_restored_config.get_enabled_tracing())
  
  // 清理测试文件
  let cleanup_results = [
    File::remove("/tmp/test_telemetry_config.json"),
    File::remove("/tmp/auto_telemetry_config.json"),
    File::remove("/tmp/telemetry_config_backup.json")
  ]
  
  for result in cleanup_results {
    assert_true(result.is_ok())
  }
}