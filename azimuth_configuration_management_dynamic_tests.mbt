// 配置管理和动态更新测试
// 测试Azimuth遥测系统的配置管理和动态更新功能

test "配置加载和验证" {
  // 测试配置加载和验证功能
  let config_manager = ConfigurationManager::new()
  
  // 设置配置模式
  let config_schema = ConfigSchema::new()
  ConfigSchema::add_property(config_schema, "service.name", StringType, true)
  ConfigSchema::add_property(config_schema, "service.version", StringType, true)
  ConfigSchema::add_property(config_schema, "server.port", IntegerType, true)
  ConfigSchema::add_property(config_schema, "database.url", StringType, true)
  ConfigSchema::add_property(config_schema, "database.pool.size", IntegerType, false)
  ConfigSchema::add_property(config_schema, "logging.level", StringType, false)
  ConfigSchema::add_property(config_schema, "metrics.enabled", BooleanType, false)
  ConfigSchema::add_property(config_schema, "tracing.enabled", BooleanType, false)
  
  ConfigurationManager::set_schema(config_manager, config_schema)
  
  // 创建有效配置
  let valid_config = Config::new()
  Config::set_property(valid_config, "service.name", "azimuth-telemetry")
  Config::set_property(valid_config, "service.version", "1.0.0")
  Config::set_property(valid_config, "server.port", 8080)
  Config::set_property(valid_config, "database.url", "postgresql://localhost:5432/azimuth")
  Config::set_property(valid_config, "database.pool.size", 10)
  Config::set_property(valid_config, "logging.level", "INFO")
  Config::set_property(valid_config, "metrics.enabled", true)
  Config::set_property(valid_config, "tracing.enabled", true)
  
  // 验证有效配置
  let validation_result = ConfigurationManager::validate(config_manager, valid_config)
  assert_true(ValidationResult::is_valid(validation_result))
  
  // 创建无效配置（缺少必需属性）
  let invalid_config = Config::new()
  Config::set_property(invalid_config, "service.name", "azimuth-telemetry")
  Config::set_property(invalid_config, "server.port", 8080)
  // 缺少 service.version 和 database.url
  
  // 验证无效配置
  let invalid_validation_result = ConfigurationManager::validate(config_manager, invalid_config)
  assert_false(ValidationResult::is_valid(invalid_validation_result))
  
  let errors = ValidationResult::get_errors(invalid_validation_result)
  assert_true(Array::length(errors) >= 2)
  assert_true(Array::contains(errors, "Missing required property: service.version"))
  assert_true(Array::contains(errors, "Missing required property: database.url"))
  
  // 测试类型验证
  let type_invalid_config = Config::new()
  Config::set_property(type_invalid_config, "service.name", "azimuth-telemetry")
  Config::set_property(type_invalid_config, "service.version", "1.0.0")
  Config::set_property(type_invalid_config, "server.port", "8080") // 字符串而非整数
  Config::set_property(type_invalid_config, "database.url", "postgresql://localhost:5432/azimuth")
  
  let type_validation_result = ConfigurationManager::validate(config_manager, type_invalid_config)
  assert_false(ValidationResult::is_valid(type_validation_result))
  
  let type_errors = ValidationResult::get_errors(type_validation_result)
  assert_true(Array::contains(type_errors, "Invalid type for property server.port: expected Integer, got String"))
}

test "多环境配置管理" {
  // 测试多环境配置管理
  let environment_config_manager = EnvironmentConfigManager::new()
  
  // 创建基础配置
  let base_config = Config::new()
  Config::set_property(base_config, "service.name", "azimuth-telemetry")
  Config::set_property(base_config, "service.version", "1.0.0")
  Config::set_property(base_config, "server.port", 8080)
  Config::set_property(base_config, "database.pool.size", 10)
  Config::set_property(base_config, "logging.level", "INFO")
  Config::set_property(base_config, "metrics.enabled", true)
  
  // 创建开发环境配置
  let dev_config = Config::new()
  Config::set_property(dev_config, "database.url", "postgresql://localhost:5432/azimuth_dev")
  Config::set_property(dev_config, "logging.level", "DEBUG")
  Config::set_property(dev_config, "debug.enabled", true)
  
  // 创建测试环境配置
  let test_config = Config::new()
  Config::set_property(test_config, "database.url", "postgresql://test-db:5432/azimuth_test")
  Config::set_property(test_config, "logging.level", "INFO")
  Config::set_property(test_config, "mock.external.services", true)
  
  // 创建生产环境配置
  let prod_config = Config::new()
  Config::set_property(prod_config, "server.port", 443)
  Config::set_property(prod_config, "database.url", "postgresql://prod-db:5432/azimuth")
  Config::set_property(prod_config, "database.pool.size", 50)
  Config::set_property(prod_config, "logging.level", "WARN")
  Config::set_property(prod_config, "ssl.enabled", true)
  Config::set_property(prod_config, "monitoring.enabled", true)
  
  // 注册环境配置
  EnvironmentConfigManager::register_base_config(environment_config_manager, base_config)
  EnvironmentConfigManager::register_environment_config(environment_config_manager, "development", dev_config)
  EnvironmentConfigManager::register_environment_config(environment_config_manager, "test", test_config)
  EnvironmentConfigManager::register_environment_config(environment_config_manager, "production", prod_config)
  
  // 获取开发环境配置
  let dev_merged_config = EnvironmentConfigManager::get_config(environment_config_manager, "development")
  
  // 验证开发环境配置
  assert_eq(Config::get_property(dev_merged_config, "service.name"), "azimuth-telemetry")
  assert_eq(Config::get_property(dev_merged_config, "service.version"), "1.0.0")
  assert_eq(Config::get_property(dev_merged_config, "server.port"), 8080)
  assert_eq(Config::get_property(dev_merged_config, "database.url"), "postgresql://localhost:5432/azimuth_dev")
  assert_eq(Config::get_property(dev_merged_config, "database.pool.size"), 10)
  assert_eq(Config::get_property(dev_merged_config, "logging.level"), "DEBUG")
  assert_eq(Config::get_property(dev_merged_config, "debug.enabled"), true)
  
  // 获取生产环境配置
  let prod_merged_config = EnvironmentConfigManager::get_config(environment_config_manager, "production")
  
  // 验证生产环境配置
  assert_eq(Config::get_property(prod_merged_config, "service.name"), "azimuth-telemetry")
  assert_eq(Config::get_property(prod_merged_config, "service.version"), "1.0.0")
  assert_eq(Config::get_property(prod_merged_config, "server.port"), 443) // 覆盖基础配置
  assert_eq(Config::get_property(prod_merged_config, "database.url"), "postgresql://prod-db:5432/azimuth")
  assert_eq(Config::get_property(prod_merged_config, "database.pool.size"), 50) // 覆盖基础配置
  assert_eq(Config::get_property(prod_merged_config, "logging.level"), "WARN") // 覆盖基础配置
  assert_eq(Config::get_property(prod_merged_config, "ssl.enabled"), true)
  assert_eq(Config::get_property(prod_merged_config, "monitoring.enabled"), true)
}

test "动态配置更新" {
  // 测试动态配置更新功能
  let dynamic_config_manager = DynamicConfigManager::new()
  
  // 设置初始配置
  let initial_config = Config::new()
  Config::set_property(initial_config, "service.name", "azimuth-telemetry")
  Config::set_property(initial_config, "service.version", "1.0.0")
  Config::set_property(initial_config, "server.port", 8080)
  Config::set_property(initial_config, "database.pool.size", 10)
  Config::set_property(initial_config, "logging.level", "INFO")
  Config::set_property(initial_config, "metrics.enabled", true)
  Config::set_property(initial_config, "tracing.enabled", true)
  
  DynamicConfigManager::set_config(dynamic_config_manager, initial_config)
  
  // 注册配置变更监听器
  let listener = ConfigChangeListener::new()
  ConfigChangeListener::on_property_change(listener, "logging.level", (old_value, new_value) => {
    assert_eq(old_value, "INFO")
    assert_eq(new_value, "DEBUG")
  })
  
  ConfigChangeListener::on_property_change(listener, "database.pool.size", (old_value, new_value) => {
    assert_eq(old_value, 10)
    assert_eq(new_value, 20)
  })
  
  DynamicConfigManager::add_listener(dynamic_config_manager, listener)
  
  // 动态更新配置
  let config_update = ConfigUpdate::new()
  ConfigUpdate::set_property(config_update, "logging.level", "DEBUG")
  ConfigUpdate::set_property(config_update, "database.pool.size", 20)
  ConfigUpdate::set_property(config_update, "new.feature.enabled", true)
  
  let update_result = DynamicConfigManager::update_config(dynamic_config_manager, config_update)
  
  // 验证更新结果
  assert_true(UpdateResult::is_success(update_result))
  
  // 验证配置已更新
  let updated_config = DynamicConfigManager::get_config(dynamic_config_manager)
  assert_eq(Config::get_property(updated_config, "logging.level"), "DEBUG")
  assert_eq(Config::get_property(updated_config, "database.pool.size"), 20)
  assert_eq(Config::get_property(updated_config, "new.feature.enabled"), true)
  
  // 验证未变更的属性保持不变
  assert_eq(Config::get_property(updated_config, "service.name"), "azimuth-telemetry")
  assert_eq(Config::get_property(updated_config, "service.version"), "1.0.0")
  assert_eq(Config::get_property(updated_config, "server.port"), 8080)
  
  // 测试配置回滚
  let rollback_result = DynamicConfigManager::rollback_config(dynamic_config_manager)
  assert_true(UpdateResult::is_success(rollback_result))
  
  // 验证配置已回滚
  let rolled_back_config = DynamicConfigManager::get_config(dynamic_config_manager)
  assert_eq(Config::get_property(rolled_back_config, "logging.level"), "INFO")
  assert_eq(Config::get_property(rolled_back_config, "database.pool.size"), 10)
}

test "配置热重载和配置源管理" {
  // 测试配置热重载和配置源管理
  let config_source_manager = ConfigSourceManager::new()
  
  // 创建文件配置源
  let file_source = FileConfigSource::new("/etc/azimuth/config.yaml")
  ConfigSourceManager::add_source(config_source_manager, "file", file_source)
  
  // 创建环境变量配置源
  let env_source = EnvironmentVariableConfigSource::new("AZIMUTH_")
  ConfigSourceManager::add_source(config_source_manager, "env", env_source)
  
  // 创建远程配置源
  let remote_source = RemoteConfigSource::new("https://config-service.example.com/azimuth")
  ConfigSourceManager::add_source(config_source_manager, "remote", remote_source)
  
  // 设置配置源优先级
  ConfigSourceManager::set_priority(config_source_manager, "remote", 1) // 最高优先级
  ConfigSourceManager::set_priority(config_source_manager, "env", 2)
  ConfigSourceManager::set_priority(config_source_manager, "file", 3) // 最低优先级
  
  // 加载配置
  let hot_reload_config = ConfigHotReloader::new(config_source_manager)
  let loaded_config = ConfigHotReloader::load_config(hot_reload_config)
  
  // 验证配置加载
  assert_true(Config::has_property(loaded_config, "service.name"))
  
  // 启用热重载
  ConfigHotReloader::enable_hot_reload(hot_reload_config, 5000) // 每5秒检查一次
  
  // 模拟配置文件变更
  FileConfigSource::simulate_change(file_source)
  
  // 等待热重载
  ConfigHotReloader::wait_for_reload(hot_reload_config)
  
  // 验证配置已重新加载
  let reloaded_config = ConfigHotReloader::get_current_config(hot_reload_config)
  assert_true(Config::has_property(reloaded_config, "service.name"))
  
  // 测试配置源健康检查
  let health_check_result = ConfigSourceManager::health_check(config_source_manager)
  
  // 验证健康检查结果
  let source_statuses = HealthCheckResult::get_source_statuses(health_check_result)
  assert_true(Array::length(source_statuses) >= 3)
  
  for status in source_statuses {
    let source_name = SourceStatus::get_source_name(status)
    let is_healthy = SourceStatus::is_healthy(status)
    
    if source_name == "file" {
      assert_true(is_healthy)
    } else if source_name == "env" {
      assert_true(is_healthy)
    } else if source_name == "remote" {
      // 远程源可能不健康，取决于网络状态
    }
  }
}

test "配置版本管理和历史记录" {
  // 测试配置版本管理和历史记录
  let config_version_manager = ConfigVersionManager::new()
  
  // 创建初始配置
  let v1_config = Config::new()
  Config::set_property(v1_config, "service.name", "azimuth-telemetry")
  Config::set_property(v1_config, "service.version", "1.0.0")
  Config::set_property(v1_config, "server.port", 8080)
  Config::set_property(v1_config, "database.pool.size", 10)
  
  // 保存版本1
  let v1_result = ConfigVersionManager::save_version(config_version_manager, v1_config, "Initial configuration")
  assert_true(VersionResult::is_success(v1_result))
  let v1_id = VersionResult::get_version_id(v1_result)
  
  // 更新配置
  let v2_config = Config::clone(v1_config)
  Config::set_property(v2_config, "service.version", "1.1.0")
  Config::set_property(v2_config, "database.pool.size", 15)
  Config::set_property(v2_config, "new.feature.enabled", true)
  
  // 保存版本2
  let v2_result = ConfigVersionManager::save_version(config_version_manager, v2_config, "Add new feature and increase pool size")
  assert_true(VersionResult::is_success(v2_result))
  let v2_id = VersionResult::get_version_id(v2_result)
  
  // 再次更新配置
  let v3_config = Config::clone(v2_config)
  Config::set_property(v3_config, "service.version", "1.2.0")
  Config::set_property(v3_config, "server.port", 8443)
  Config::set_property(v3_config, "ssl.enabled", true)
  
  // 保存版本3
  let v3_result = ConfigVersionManager::save_version(config_version_manager, v3_config, "Enable SSL and change port")
  assert_true(VersionResult::is_success(v3_result))
  let v3_id = VersionResult::get_version_id(v3_result)
  
  // 获取版本历史
  let version_history = ConfigVersionManager::get_version_history(config_version_manager)
  assert_eq(Array::length(version_history), 3)
  
  // 验证版本历史
  let v1_info = Array::get(version_history, 0)
  let v2_info = Array::get(version_history, 1)
  let v3_info = Array::get(version_history, 2)
  
  assert_eq(VersionInfo::get_id(v1_info), v1_id)
  assert_eq(VersionInfo::get_id(v2_info), v2_id)
  assert_eq(VersionInfo::get_id(v3_info), v3_id)
  
  assert_eq(VersionInfo::get_description(v1_info), "Initial configuration")
  assert_eq(VersionInfo::get_description(v2_info), "Add new feature and increase pool size")
  assert_eq(VersionInfo::get_description(v3_info), "Enable SSL and change port")
  
  // 恢复到版本2
  let restore_result = ConfigVersionManager::restore_version(config_version_manager, v2_id)
  assert_true(RestoreResult::is_success(restore_result))
  
  let restored_config = RestoreResult::get_config(restore_result)
  assert_eq(Config::get_property(restored_config, "service.version"), "1.1.0")
  assert_eq(Config::get_property(restored_config, "database.pool.size"), 15)
  assert_eq(Config::get_property(restored_config, "new.feature.enabled"), true)
  assert_false(Config::has_property(restored_config, "ssl.enabled")) // 版本2中没有这个属性
  
  // 比较版本差异
  let diff_result = ConfigVersionManager::compare_versions(config_version_manager, v1_id, v3_id)
  let diff = DiffResult::get_diff(diff_result)
  
  // 验证差异
  assert_true(Diff::has_property_change(diff, "service.version"))
  assert_true(Diff::has_property_change(diff, "server.port"))
  assert_true(Diff::has_property_change(diff, "database.pool.size"))
  assert_true(Diff::has_added_property(diff, "new.feature.enabled"))
  assert_true(Diff::has_added_property(diff, "ssl.enabled"))
}

test "配置加密和安全" {
  // 测试配置加密和安全功能
  let config_security_manager = ConfigSecurityManager::new()
  
  // 生成加密密钥
  let encryption_key = EncryptionKey::generate()
  
  // 设置敏感属性
  let sensitive_properties = [
    "database.password",
    "api.secret.key",
    "jwt.secret",
    "encryption.key"
  ]
  
  for property in sensitive_properties {
    ConfigSecurityManager::mark_sensitive(config_security_manager, property)
  }
  
  // 创建包含敏感信息的配置
  let secure_config = Config::new()
  Config::set_property(secure_config, "service.name", "azimuth-telemetry")
  Config::set_property(secure_config, "database.url", "postgresql://localhost:5432/azimuth")
  Config::set_property(secure_config, "database.password", "super-secret-password")
  Config::set_property(secure_config, "api.secret.key", "api-key-12345")
  Config::set_property(secure_config, "jwt.secret", "jwt-secret-key")
  Config::set_property(secure_config, "encryption.key", "encryption-key-value")
  Config::set_property(secure_config, "logging.level", "INFO")
  
  // 加密敏感属性
  let encrypted_config = ConfigSecurityManager::encrypt_sensitive_properties(
    config_security_manager, 
    secure_config, 
    encryption_key
  )
  
  // 验证敏感属性已加密
  assert_eq(Config::get_property(encrypted_config, "service.name"), "azimuth-telemetry") // 非敏感属性未加密
  assert_eq(Config::get_property(encrypted_config, "logging.level"), "INFO") // 非敏感属性未加密
  
  let encrypted_password = Config::get_property(encrypted_config, "database.password")
  let encrypted_api_key = Config::get_property(encrypted_config, "api.secret.key")
  
  assert_ne(encrypted_password, "super-secret-password") // 已加密
  assert_ne(encrypted_api_key, "api-key-12345") // 已加密
  
  // 解密敏感属性
  let decrypted_config = ConfigSecurityManager::decrypt_sensitive_properties(
    config_security_manager, 
    encrypted_config, 
    encryption_key
  )
  
  // 验证敏感属性已解密
  assert_eq(Config::get_property(decrypted_config, "database.password"), "super-secret-password")
  assert_eq(Config::get_property(decrypted_config, "api.secret.key"), "api-key-12345")
  assert_eq(Config::get_property(decrypted_config, "jwt.secret"), "jwt-secret-key")
  assert_eq(Config::get_property(decrypted_config, "encryption.key"), "encryption-key-value")
  
  // 测试访问控制
  let access_controller = ConfigAccessController::new()
  
  // 设置访问权限
  AccessController::grant_permission(access_controller, "admin", ["read", "write", "decrypt"])
  AccessController::grant_permission(access_controller, "operator", ["read"])
  AccessController::grant_permission(access_controller, "service", ["read", "decrypt"])
  
  // 测试不同角色的访问权限
  let admin_access = AccessController::check_permission(access_controller, "admin", "read", "database.password")
  let operator_access = AccessController::check_permission(access_controller, "operator", "read", "database.password")
  let service_decrypt_access = AccessController::check_permission(access_controller, "service", "decrypt", "database.password")
  let operator_write_access = AccessController::check_permission(access_controller, "operator", "write", "database.password")
  
  assert_true(admin_access)
  assert_true(operator_access)
  assert_true(service_decrypt_access)
  assert_false(operator_write_access) // 操作员没有写权限
  
  // 测试审计日志
  let audit_logger = ConfigAuditLogger::new()
  
  // 记录配置访问
  AuditLogger::log_access(audit_logger, "operator", "read", "database.url", 1704067200000L)
  AuditLogger::log_access(audit_logger, "admin", "write", "database.pool.size", 1704067201000L)
  AuditLogger::log_access(audit_logger, "service", "decrypt", "database.password", 1704067202000L)
  AuditLogger::log_access_denied(audit_logger, "operator", "write", "database.password", 1704067203000L)
  
  // 查询审计日志
  let access_logs = AuditLogger::get_access_logs(audit_logger)
  assert_eq(Array::length(access_logs), 4)
  
  let denied_logs = AuditLogger::get_denied_access_logs(audit_logger)
  assert_eq(Array::length(denied_logs), 1)
  
  let sensitive_access_logs = AuditLogger::get_sensitive_property_access_logs(audit_logger)
  assert_eq(Array::length(sensitive_access_logs), 2)
}

test "配置模板和继承" {
  // 测试配置模板和继承功能
  let config_template_manager = ConfigTemplateManager::new()
  
  // 创建基础模板
  let base_template = ConfigTemplate::new("base")
  ConfigTemplate::set_property(base_template, "service.name", "azimuth-telemetry")
  ConfigTemplate::set_property(base_template, "service.version", "${VERSION:1.0.0}")
  ConfigTemplate::set_property(base_template, "server.port", "${SERVER_PORT:8080}")
  ConfigTemplate::set_property(base_template, "logging.level", "${LOG_LEVEL:INFO}")
  ConfigTemplate::set_property(base_template, "metrics.enabled", "${METRICS_ENABLED:true}")
  
  // 创建数据库模板
  let database_template = ConfigTemplate::new("database")
  ConfigTemplate::extend_from(database_template, "base") // 继承基础模板
  ConfigTemplate::set_property(database_template, "database.url", "${DATABASE_URL:postgresql://localhost:5432/azimuth}")
  ConfigTemplate::set_property(database_template, "database.pool.size", "${DB_POOL_SIZE:10}")
  ConfigTemplate::set_property(database_template, "database.password", "${DATABASE_PASSWORD}")
  
  // 创建微服务模板
  let microservice_template = ConfigTemplate::new("microservice")
  ConfigTemplate::extend_from(microservice_template, "base") // 继承基础模板
  ConfigTemplate::set_property(microservice_template, "discovery.enabled", "${DISCOVERY_ENABLED:true}")
  ConfigTemplate::set_property(microservice_template, "discovery.service.name", "${SERVICE_NAME}")
  ConfigTemplate::set_property(microservice_template, "tracing.enabled", "${TRACING_ENABLED:true}")
  ConfigTemplate::set_property(microservice_template, "tracing.sampling.rate", "${TRACING_SAMPLING_RATE:0.1}")
  
  // 注册模板
  ConfigTemplateManager::register_template(config_template_manager, base_template)
  ConfigTemplateManager::register_template(config_template_manager, database_template)
  ConfigTemplateManager::register_template(config_template_manager, microservice_template)
  
  // 设置变量值
  let variables = Variables::new()
  Variables::set(variables, "VERSION", "1.2.3")
  Variables::set(variables, "SERVER_PORT", "9090")
  Variables::set(variables, "DATABASE_URL", "postgresql://prod-db:5432/azimuth_prod")
  Variables::set(variables, "DATABASE_PASSWORD", "prod-secret-password")
  Variables::set(variables, "SERVICE_NAME", "user-service")
  
  // 从数据库模板渲染配置
  let database_config = ConfigTemplateManager::render_template(
    config_template_manager, 
    "database", 
    variables
  )
  
  // 验证渲染结果
  assert_eq(Config::get_property(database_config, "service.name"), "azimuth-telemetry")
  assert_eq(Config::get_property(database_config, "service.version"), "1.2.3")
  assert_eq(Config::get_property(database_config, "server.port"), 9090)
  assert_eq(Config::get_property(database_config, "logging.level"), "INFO") // 使用默认值
  assert_eq(Config::get_property(database_config, "metrics.enabled"), true) // 使用默认值
  assert_eq(Config::get_property(database_config, "database.url"), "postgresql://prod-db:5432/azimuth_prod")
  assert_eq(Config::get_property(database_config, "database.pool.size"), 10) // 使用默认值
  assert_eq(Config::get_property(database_config, "database.password"), "prod-secret-password")
  
  // 从微服务模板渲染配置
  let microservice_config = ConfigTemplateManager::render_template(
    config_template_manager, 
    "microservice", 
    variables
  )
  
  // 验证渲染结果
  assert_eq(Config::get_property(microservice_config, "service.name"), "azimuth-telemetry")
  assert_eq(Config::get_property(microservice_config, "service.version"), "1.2.3")
  assert_eq(Config::get_property(microservice_config, "server.port"), 9090)
  assert_eq(Config::get_property(microservice_config, "discovery.enabled"), true) // 使用默认值
  assert_eq(Config::get_property(microservice_config, "discovery.service.name"), "user-service")
  assert_eq(Config::get_property(microservice_config, "tracing.enabled"), true) // 使用默认值
  assert_eq(Config::get_property(microservice_config, "tracing.sampling.rate"), 0.1) // 使用默认值
  
  // 测试模板组合
  let combined_template = ConfigTemplate::new("combined")
  ConfigTemplate::extend_from(combined_template, "database")
  ConfigTemplate::extend_from(combined_template, "microservice")
  ConfigTemplate::set_property(combined_template, "custom.property", "custom.value")
  
  ConfigTemplateManager::register_template(config_template_manager, combined_template)
  
  let combined_config = ConfigTemplateManager::render_template(
    config_template_manager, 
    "combined", 
    variables
  )
  
  // 验证组合模板结果
  assert_eq(Config::get_property(combined_config, "service.name"), "azimuth-telemetry")
  assert_eq(Config::get_property(combined_config, "database.url"), "postgresql://prod-db:5432/azimuth_prod")
  assert_eq(Config::get_property(combined_config, "discovery.enabled"), true)
  assert_eq(Config::get_property(combined_config, "custom.property"), "custom.value")
}