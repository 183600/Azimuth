// Azimuth 配置管理动态测试用例
// 测试配置管理功能，包括动态配置更新、配置验证和配置持久化

// 测试1: 基本配置管理
test "基本配置管理功能" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new("telemetry.config")
  
  // 添加配置项
  ConfigManager::add_string_config(config_manager, "service.name", "azimuth.telemetry")
  ConfigManager::add_int_config(config_manager, "service.port", 8080)
  ConfigManager::add_bool_config(config_manager, "service.enabled", true)
  ConfigManager::add_float_config(config_manager, "service.timeout", 30.5)
  
  ConfigManager::add_string_list_config(config_manager, "service.endpoints", ["http://localhost:8080", "http://localhost:8081"])
  ConfigManager::add_string_map_config(config_manager, "service.tags", {"env": "production", "version": "1.0.0"})
  
  // 读取配置项
  let service_name = ConfigManager::get_string(config_manager, "service.name")
  let service_port = ConfigManager::get_int(config_manager, "service.port")
  let service_enabled = ConfigManager::get_bool(config_manager, "service.enabled")
  let service_timeout = ConfigManager::get_float(config_manager, "service.timeout")
  
  assert_eq(service_name, "azimuth.telemetry")
  assert_eq(service_port, 8080)
  assert_eq(service_enabled, true)
  assert_eq(service_timeout, 30.5)
  
  let service_endpoints = ConfigManager::get_string_list(config_manager, "service.endpoints")
  let service_tags = ConfigManager::get_string_map(config_manager, "service.tags")
  
  assert_eq(service_endpoints.length(), 2)
  assert_eq(service_endpoints[0], "http://localhost:8080")
  assert_eq(service_endpoints[1], "http://localhost:8081")
  assert_eq(service_tags.get("env"), "production")
  assert_eq(service_tags.get("version"), "1.0.0")
  
  // 测试默认值
  let non_existent_string = ConfigManager::get_string_with_default(config_manager, "non.existent", "default.value")
  let non_existent_int = ConfigManager::get_int_with_default(config_manager, "non.existent", 42)
  let non_existent_bool = ConfigManager::get_bool_with_default(config_manager, "non.existent", false)
  
  assert_eq(non_existent_string, "default.value")
  assert_eq(non_existent_int, 42)
  assert_eq(non_existent_bool, false)
  
  // 测试配置存在性检查
  assert_true(ConfigManager::has_config(config_manager, "service.name"))
  assert_false(ConfigManager::has_config(config_manager, "non.existent"))
  
  // 测试配置类型检查
  assert_eq(ConfigManager::get_config_type(config_manager, "service.name"), ConfigType::String)
  assert_eq(ConfigManager::get_config_type(config_manager, "service.port"), ConfigType::Int)
  assert_eq(ConfigManager::get_config_type(config_manager, "service.enabled"), ConfigType::Bool)
  assert_eq(ConfigManager::get_config_type(config_manager, "service.timeout"), ConfigType::Float)
  assert_eq(ConfigManager::get_config_type(config_manager, "service.endpoints"), ConfigType::StringList)
  assert_eq(ConfigManager::get_config_type(config_manager, "service.tags"), ConfigType::StringMap)
}

// 测试2: 动态配置更新
test "动态配置更新功能" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new("dynamic.config")
  
  // 添加配置项
  ConfigManager::add_string_config(config_manager, "log.level", "INFO")
  ConfigManager::add_int_config(config_manager, "metrics.batch.size", 100)
  ConfigManager::add_bool_config(config_manager, "tracing.enabled", true)
  
  // 设置配置变更监听器
  let change_events = []
  ConfigManager::add_change_listener(config_manager, fn(key, old_value, new_value) {
    change_events.push((key, old_value, new_value))
  })
  
  // 更新配置项
  ConfigManager::set_string(config_manager, "log.level", "DEBUG")
  ConfigManager::set_int(config_manager, "metrics.batch.size", 200)
  ConfigManager::set_bool(config_manager, "tracing.enabled", false)
  
  // 验证配置更新
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "DEBUG")
  assert_eq(ConfigManager::get_int(config_manager, "metrics.batch.size"), 200)
  assert_eq(ConfigManager::get_bool(config_manager, "tracing.enabled"), false)
  
  // 验证变更事件
  assert_eq(change_events.length(), 3)
  assert_eq(change_events[0], ("log.level", "INFO", "DEBUG"))
  assert_eq(change_events[1], ("metrics.batch.size", "100", "200"))
  assert_eq(change_events[2], ("tracing.enabled", "true", "false"))
  
  // 测试批量更新
  let batch_updates = [
    ("log.level", "WARN"),
    ("metrics.batch.size", "300"),
    ("tracing.enabled", "true")
  ]
  
  ConfigManager::batch_update(config_manager, batch_updates)
  
  // 验证批量更新
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "WARN")
  assert_eq(ConfigManager::get_int(config_manager, "metrics.batch.size"), 300)
  assert_eq(ConfigManager::get_bool(config_manager, "tracing.enabled"), true)
  
  // 验证批量更新事件
  assert_eq(change_events.length(), 6) // 3个单独更新 + 3个批量更新
  
  // 测试条件更新
  let update_success = ConfigManager::conditional_update(config_manager, "log.level", "ERROR", fn(current) {
    return current == "WARN"
  })
  
  assert_true(update_success)
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "ERROR")
  
  let update_failure = ConfigManager::conditional_update(config_manager, "log.level", "FATAL", fn(current) {
    return current == "WARN" // 条件不满足
  })
  
  assert_false(update_failure)
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "ERROR") // 值应该保持不变
}

// 测试3: 配置验证
test "配置验证功能" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new("validation.config")
  
  // 添加带验证器的配置项
  ConfigManager::add_string_config_with_validator(config_manager, "service.name", "default.service", fn(value) {
    return value.length() > 0 && value.length() <= 50
  })
  
  ConfigManager::add_int_config_with_validator(config_manager, "service.port", 8080, fn(value) {
    return value >= 1 && value <= 65535
  })
  
  ConfigManager::add_float_config_with_validator(config_manager, "service.timeout", 30.0, fn(value) {
    return value >= 0.1 && value <= 3600.0
  })
  
  ConfigManager::add_string_list_config_with_validator(config_manager, "service.endpoints", [], fn(list) {
    return list.length() >= 1 && list.length() <= 10
  })
  
  // 测试有效配置值
  assert_true(ConfigManager::set_string(config_manager, "service.name", "valid.service.name"))
  assert_true(ConfigManager::set_int(config_manager, "service.port", 9090))
  assert_true(ConfigManager::set_float(config_manager, "service.timeout", 60.5))
  assert_true(ConfigManager::set_string_list(config_manager, "service.endpoints", ["http://localhost:9090"]))
  
  // 测试无效配置值
  assert_false(ConfigManager::set_string(config_manager, "service.name", "")) // 空字符串
  assert_false(ConfigManager::set_string(config_manager, "service.name", "x".repeat(100))) // 太长
  
  assert_false(ConfigManager::set_int(config_manager, "service.port", 0)) // 太小
  assert_false(ConfigManager::set_int(config_manager, "service.port", 70000)) // 太大
  
  assert_false(ConfigManager::set_float(config_manager, "service.timeout", 0.0)) // 太小
  assert_false(ConfigManager::set_float(config_manager, "service.timeout", 4000.0)) // 太大
  
  assert_false(ConfigManager::set_string_list(config_manager, "service.endpoints", [])) // 空列表
  assert_false(ConfigManager::set_string_list(config_manager, "service.endpoints", ["x".repeat(100)])) // 元素太长
  
  // 测试自定义验证错误消息
  ConfigManager::add_string_config_with_validator(config_manager, "api.key", "", fn(value) {
    if value.length() < 10 {
      return ValidationResult::Invalid("API密钥长度必须至少为10个字符")
    }
    if !value.contains("-") {
      return ValidationResult::Invalid("API密钥必须包含连字符")
    }
    return ValidationResult::Valid
  })
  
  let validation_result = ConfigManager::validate_and_set(config_manager, "api.key", "shortkey")
  assert_true(validation_result.is_error())
  assert_true(validation_result.error_message().contains("API密钥长度必须至少为10个字符"))
  
  let validation_result2 = ConfigManager::validate_and_set(config_manager, "api.key", "longkeywithnothyphen")
  assert_true(validation_result2.is_error())
  assert_true(validation_result2.error_message().contains("API密钥必须包含连字符"))
  
  let validation_result3 = ConfigManager::validate_and_set(config_manager, "api.key", "valid-api-key-123")
  assert_true(validation_result3.is_success())
  
  // 测试配置依赖验证
  ConfigManager::add_config_dependency(config_manager, "service.ssl.enabled", fn() {
    let ssl_enabled = ConfigManager::get_bool(config_manager, "service.ssl.enabled")
    if ssl_enabled {
      let ssl_port = ConfigManager::get_int(config_manager, "service.ssl.port")
      let ssl_cert = ConfigManager::get_string(config_manager, "service.ssl.certificate")
      
      if ssl_port <= 0 || ssl_port > 65535 {
        return ValidationResult::Invalid("SSL端口必须在1-65535范围内")
      }
      if ssl_cert == "" {
        return ValidationResult::Invalid("启用SSL时必须提供证书路径")
      }
    }
    return ValidationResult::Valid
  })
  
  ConfigManager::add_bool_config(config_manager, "service.ssl.enabled", false)
  ConfigManager::add_int_config(config_manager, "service.ssl.port", 8443)
  ConfigManager::add_string_config(config_manager, "service.ssl.certificate", "")
  
  // 启用SSL但不提供证书应该失败
  let ssl_validation_result = ConfigManager::validate_and_set(config_manager, "service.ssl.enabled", true)
  assert_true(ssl_validation_result.is_error())
  assert_true(ssl_validation_result.error_message().contains("启用SSL时必须提供证书路径"))
  
  // 提供证书后再启用SSL应该成功
  ConfigManager::set_string(config_manager, "service.ssl.certificate", "/path/to/cert.pem")
  let ssl_validation_result2 = ConfigManager::validate_and_set(config_manager, "service.ssl.enabled", true)
  assert_true(ssl_validation_result2.is_success())
}

// 测试4: 配置持久化
test "配置持久化功能" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new("persistence.config")
  
  // 添加配置项
  ConfigManager::add_string_config(config_manager, "database.host", "localhost")
  ConfigManager::add_int_config(config_manager, "database.port", 5432)
  ConfigManager::add_string_config(config_manager, "database.name", "telemetry")
  ConfigManager::add_bool_config(config_manager, "database.ssl", false)
  
  // 保存配置到文件
  let save_result = ConfigManager::save_to_file(config_manager, "/tmp/telemetry_config.json")
  assert_true(save_result)
  
  // 修改配置
  ConfigManager::set_string(config_manager, "database.host", "remote.db.server")
  ConfigManager::set_int(config_manager, "database.port", 5433)
  ConfigManager::set_bool(config_manager, "database.ssl", true)
  
  // 验证配置已修改
  assert_eq(ConfigManager::get_string(config_manager, "database.host"), "remote.db.server")
  assert_eq(ConfigManager::get_int(config_manager, "database.port"), 5433)
  assert_eq(ConfigManager::get_bool(config_manager, "database.ssl"), true)
  
  // 从文件加载配置
  let load_result = ConfigManager::load_from_file(config_manager, "/tmp/telemetry_config.json")
  assert_true(load_result)
  
  // 验证配置已恢复
  assert_eq(ConfigManager::get_string(config_manager, "database.host"), "localhost")
  assert_eq(ConfigManager::get_int(config_manager, "database.port"), 5432)
  assert_eq(ConfigManager::get_string(config_manager, "database.name"), "telemetry")
  assert_eq(ConfigManager::get_bool(config_manager, "database.ssl"), false)
  
  // 测试配置导出
  let exported_config = ConfigManager::export_to_json(config_manager)
  assert_true(exported_config.contains("database.host"))
  assert_true(exported_config.contains("localhost"))
  assert_true(exported_config.contains("5432"))
  assert_true(exported_config.contains("telemetry"))
  
  // 测试配置导入
  let new_config_manager = ConfigurationManager::new("imported.config")
  let import_result = ConfigManager::import_from_json(new_config_manager, exported_config)
  assert_true(import_result)
  
  // 验证导入的配置
  assert_eq(ConfigManager::get_string(new_config_manager, "database.host"), "localhost")
  assert_eq(ConfigManager::get_int(new_config_manager, "database.port"), 5432)
  assert_eq(ConfigManager::get_string(new_config_manager, "database.name"), "telemetry")
  assert_eq(ConfigManager::get_bool(new_config_manager, "database.ssl"), false)
  
  // 测试配置备份和恢复
  let backup_result = ConfigManager::create_backup(config_manager, "/tmp/telemetry_config_backup.json")
  assert_true(backup_result)
  
  // 修改配置
  ConfigManager::set_string(config_manager, "database.host", "modified.host")
  
  // 从备份恢复
  let restore_result = ConfigManager::restore_from_backup(config_manager, "/tmp/telemetry_config_backup.json")
  assert_true(restore_result)
  
  // 验证配置已恢复
  assert_eq(ConfigManager::get_string(config_manager, "database.host"), "localhost")
  
  // 测试配置版本管理
  ConfigManager::enable_versioning(config_manager, "/tmp/config_versions")
  
  // 保存多个版本
  ConfigManager::set_string(config_manager, "database.host", "version1.host")
  ConfigManager::save_version(config_manager, "Initial setup")
  
  ConfigManager::set_string(config_manager, "database.host", "version2.host")
  ConfigManager::save_version(config_manager, "Updated host")
  
  // 列出版本
  let versions = ConfigManager::list_versions(config_manager)
  assert_eq(versions.length(), 2)
  assert_true(versions[0].contains("Initial setup"))
  assert_true(versions[1].contains("Updated host"))
  
  // 恢复到特定版本
  let restore_version_result = ConfigManager::restore_to_version(config_manager, versions[0])
  assert_true(restore_version_result)
  
  // 验证已恢复到第一个版本
  assert_eq(ConfigManager::get_string(config_manager, "database.host"), "version1.host")
}

// 测试5: 环境特定配置
test "环境特定配置功能" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new("environment.config")
  
  // 设置当前环境
  ConfigManager::set_environment(config_manager, "development")
  
  // 添加基础配置
  ConfigManager::add_string_config(config_manager, "service.name", "azimuth.telemetry")
  ConfigManager::add_int_config(config_manager, "service.port", 8080)
  ConfigManager::add_string_config(config_manager, "database.host", "localhost")
  
  // 添加开发环境特定配置
  ConfigManager::add_environment_config(config_manager, "development", "service.port", "8081")
  ConfigManager::add_environment_config(config_manager, "development", "database.host", "dev.db.server")
  ConfigManager::add_environment_config(config_manager, "development", "debug.enabled", "true")
  
  // 添加生产环境特定配置
  ConfigManager::add_environment_config(config_manager, "production", "service.port", "80")
  ConfigManager::add_environment_config(config_manager, "production", "database.host", "prod.db.server")
  ConfigManager::add_environment_config(config_manager, "production", "debug.enabled", "false")
  
  // 验证开发环境配置
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth.telemetry") // 基础配置
  assert_eq(ConfigManager::get_int(config_manager, "service.port"), 8081) // 开发环境覆盖
  assert_eq(ConfigManager::get_string(config_manager, "database.host"), "dev.db.server") // 开发环境覆盖
  assert_eq(ConfigManager::get_bool(config_manager, "debug.enabled"), true) // 仅开发环境
  
  // 切换到生产环境
  ConfigManager::set_environment(config_manager, "production")
  
  // 验证生产环境配置
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth.telemetry") // 基础配置
  assert_eq(ConfigManager::get_int(config_manager, "service.port"), 80) // 生产环境覆盖
  assert_eq(ConfigManager::get_string(config_manager, "database.host"), "prod.db.server") // 生产环境覆盖
  assert_eq(ConfigManager::get_bool(config_manager, "debug.enabled"), false) // 生产环境覆盖
  
  // 测试环境配置继承
  ConfigManager::add_environment_config(config_manager, "staging", "service.port", "8080")
  ConfigManager::add_environment_config(config_manager, "staging", "database.host", "staging.db.server")
  
  // 设置继承链
  ConfigManager::set_environment_inheritance(config_manager, "staging", ["development"])
  
  ConfigManager::set_environment(config_manager, "staging")
  
  // 验证继承配置
  assert_eq(ConfigManager::get_int(config_manager, "service.port"), 8080) // staging特定
  assert_eq(ConfigManager::get_string(config_manager, "database.host"), "staging.db.server") // staging特定
  assert_eq(ConfigManager::get_bool(config_manager, "debug.enabled"), true) // 从development继承
  
  // 测试环境配置模板
  let template = {
    "service.name": "azimuth.telemetry",
    "service.port": 8080,
    "database.host": "localhost",
    "debug.enabled": false
  }
  
  ConfigManager::apply_environment_template(config_manager, "testing", template)
  ConfigManager::add_environment_config(config_manager, "testing", "database.host", "test.db.server")
  
  ConfigManager::set_environment(config_manager, "testing")
  
  // 验证模板应用
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth.telemetry")
  assert_eq(ConfigManager::get_int(config_manager, "service.port"), 8080)
  assert_eq(ConfigManager::get_string(config_manager, "database.host"), "test.db.server") // 覆盖模板值
  assert_eq(ConfigManager::get_bool(config_manager, "debug.enabled"), false) // 从模板
}

// 测试6: 配置热重载
test "配置热重载功能" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new("hotreload.config")
  
  // 添加配置项
  ConfigManager::add_string_config(config_manager, "log.level", "INFO")
  ConfigManager::add_int_config(config_manager, "metrics.interval", 60)
  ConfigManager::add_bool_config(config_manager, "tracing.enabled", true)
  
  // 启用文件监控
  let config_file = "/tmp/hotreload_config.json"
  ConfigManager::save_to_file(config_manager, config_file)
  
  let reload_events = []
  ConfigManager::enable_file_watching(config_manager, config_file, fn(changes) {
    reload_events.push(changes)
  })
  
  // 初始配置
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "INFO")
  assert_eq(ConfigManager::get_int(config_manager, "metrics.interval"), 60)
  assert_eq(ConfigManager::get_bool(config_manager, "tracing.enabled"), true)
  
  // 模拟外部配置文件修改
  let external_config = {
    "log.level": "DEBUG",
    "metrics.interval": 30,
    "tracing.enabled": false,
    "new.config": "new.value"
  }
  
  File::write_json(config_file, external_config)
  
  // 等待文件监控检测到变化
  Thread::sleep(1000)
  
  // 验证配置已热重载
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "DEBUG")
  assert_eq(ConfigManager::get_int(config_manager, "metrics.interval"), 30)
  assert_eq(ConfigManager::get_bool(config_manager, "tracing.enabled"), false)
  assert_eq(ConfigManager::get_string(config_manager, "new.config"), "new.value") // 新配置项
  
  // 验证重载事件
  assert_true(reload_events.length() > 0)
  
  // 测试重载失败处理
  let invalid_config = "{ invalid json }"
  File::write(config_file, invalid_config)
  
  // 等待文件监控检测到变化
  Thread::sleep(1000)
  
  // 验证配置未因无效JSON而改变
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "DEBUG")
  assert_eq(ConfigManager::get_int(config_manager, "metrics.interval"), 30)
  assert_eq(ConfigManager::get_bool(config_manager, "tracing.enabled"), false)
  
  // 测试配置重载钩子
  let pre_reload_called = false
  let post_reload_called = false
  
  ConfigManager::add_pre_reload_hook(config_manager, fn(old_config, new_config) {
    pre_reload_called = true
    assert_eq(old_config.get("log.level"), "DEBUG")
    assert_eq(new_config.get("log.level"), "INFO")
  })
  
  ConfigManager::add_post_reload_hook(config_manager, fn(config) {
    post_reload_called = true
    assert_eq(config.get("log.level"), "INFO")
  })
  
  // 恢复有效配置并触发重载
  let valid_config = {
    "log.level": "INFO",
    "metrics.interval": 60,
    "tracing.enabled": true
  }
  
  File::write_json(config_file, valid_config)
  
  // 等待文件监控检测到变化
  Thread::sleep(1000)
  
  // 验证钩子被调用
  assert_true(pre_reload_called)
  assert_true(post_reload_called)
  
  // 验证配置已更新
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "INFO")
  assert_eq(ConfigManager::get_int(config_manager, "metrics.interval"), 60)
  assert_eq(ConfigManager::get_bool(config_manager, "tracing.enabled"), true)
  
  // 停止文件监控
  ConfigManager::disable_file_watching(config_manager)
}