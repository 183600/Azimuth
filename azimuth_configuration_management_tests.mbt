// 阿兹米克配置管理测试用例
// 专注于遥测系统的配置加载、动态更新和环境适配功能

// 测试1: 基本配置加载和解析
test "基本配置加载和解析测试" {
  let config_manager = ConfigurationManager::new()
  
  // 创建配置内容
  let config_content = "{
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service\",
        \"version\": \"1.0.0\",
        \"port\": 8080
      },
      \"metrics\": {
        \"enabled\": true,
        \"interval\": 5000,
        \"retention_days\": 30
      },
      \"logging\": {
        \"level\": \"info\",
        \"format\": \"json\",
        \"output\": \"stdout\"
      }
    }
  }"
  
  // 加载配置
  let config = ConfigurationManager::load_from_string(config_manager, config_content)
  
  // 验证配置加载
  assert_true(ConfigurationManager::is_valid(config))
  
  // 获取配置值
  let service_name = ConfigurationManager::get_string(config, "telemetry.service.name")
  match service_name {
    Some(name) => assert_eq(name, "azimuth-service")
    None => assert_true(false)
  }
  
  let service_version = ConfigurationManager::get_string(config, "telemetry.service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  
  let service_port = ConfigurationManager::get_int(config, "telemetry.service.port")
  match service_port {
    Some(port) => assert_eq(port, 8080)
    None => assert_true(false)
  }
  
  let metrics_enabled = ConfigurationManager::get_bool(config, "telemetry.metrics.enabled")
  match metrics_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let metrics_interval = ConfigurationManager::get_int(config, "telemetry.metrics.interval")
  match metrics_interval {
    Some(interval) => assert_eq(interval, 5000)
    None => assert_true(false)
  }
  
  // 测试不存在的配置键
  let non_existent = ConfigurationManager::get_string(config, "telemetry.nonexistent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// 测试2: 多环境配置管理
test "多环境配置管理测试" {
  let config_manager = ConfigurationManager::new()
  
  // 创建不同环境的配置
  let dev_config_content = "{
    \"environment\": \"development\",
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service-dev\",
        \"port\": 3000
      },
      \"logging\": {
        \"level\": \"debug\",
        \"output\": \"console\"
      },
      \"database\": {
        \"host\": \"localhost\",
        \"port\": 5432,
        \"name\": \"azimuth_dev\"
      }
    }
  }"
  
  let prod_config_content = "{
    \"environment\": \"production\",
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service-prod\",
        \"port\": 8080
      },
      \"logging\": {
        \"level\": \"info\",
        \"output\": \"file\"
      },
      \"database\": {
        \"host\": \"prod-db.example.com\",
        \"port\": 5432,
        \"name\": \"azimuth_prod\"
      }
    }
  }"
  
  // 加载不同环境配置
  let dev_config = ConfigurationManager::load_from_string(config_manager, dev_config_content)
  let prod_config = ConfigurationManager::load_from_string(config_manager, prod_config_content)
  
  // 注册环境配置
  ConfigurationManager::register_environment_config(config_manager, "development", dev_config)
  ConfigurationManager::register_environment_config(config_manager, "production", prod_config)
  
  // 设置当前环境
  ConfigurationManager::set_current_environment(config_manager, "development")
  
  // 获取当前环境配置
  let current_config = ConfigurationManager::get_current_config(config_manager)
  
  // 验证开发环境配置
  let env_name = ConfigurationManager::get_string(current_config, "environment")
  match env_name {
    Some(name) => assert_eq(name, "development")
    None => assert_true(false)
  }
  
  let service_name = ConfigurationManager::get_string(current_config, "telemetry.service.name")
  match service_name {
    Some(name) => assert_eq(name, "azimuth-service-dev")
    None => assert_true(false)
  }
  
  let log_level = ConfigurationManager::get_string(current_config, "telemetry.logging.level")
  match log_level {
    Some(level) => assert_eq(level, "debug")
    None => assert_true(false)
  }
  
  // 切换到生产环境
  ConfigurationManager::set_current_environment(config_manager, "production")
  let prod_current_config = ConfigurationManager::get_current_config(config_manager)
  
  // 验证生产环境配置
  let prod_service_name = ConfigurationManager::get_string(prod_current_config, "telemetry.service.name")
  match prod_service_name {
    Some(name) => assert_eq(name, "azimuth-service-prod")
    None => assert_true(false)
  }
  
  let prod_log_level = ConfigurationManager::get_string(prod_current_config, "telemetry.logging.level")
  match prod_log_level {
    Some(level) => assert_eq(level, "info")
    None => assert_true(false)
  }
  
  // 测试配置合并
  let base_config_content = "{
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service\"
      },
      \"metrics\": {
        \"enabled\": true
      }
    }
  }"
  
  let env_override_content = "{
    \"telemetry\": {
      \"service\": {
        \"port\": 8080
      },
      \"metrics\": {
        \"interval\": 5000
      }
    }
  }"
  
  let base_config = ConfigurationManager::load_from_string(config_manager, base_config_content)
  let env_override = ConfigurationManager::load_from_string(config_manager, env_override_content)
  
  let merged_config = ConfigurationManager::merge_configs(config_manager, base_config, env_override)
  
  // 验证合并结果
  let merged_name = ConfigurationManager::get_string(merged_config, "telemetry.service.name")
  match merged_name {
    Some(name) => assert_eq(name, "azimuth-service") // 来自基础配置
    None => assert_true(false)
  }
  
  let merged_port = ConfigurationManager::get_int(merged_config, "telemetry.service.port")
  match merged_port {
    Some(port) => assert_eq(port, 8080) // 来自覆盖配置
    None => assert_true(false)
  }
  
  let merged_enabled = ConfigurationManager::get_bool(merged_config, "telemetry.metrics.enabled")
  match merged_enabled {
    Some(enabled) => assert_true(enabled) // 来自基础配置
    None => assert_true(false)
  }
  
  let merged_interval = ConfigurationManager::get_int(merged_config, "telemetry.metrics.interval")
  match merged_interval {
    Some(interval) => assert_eq(interval, 5000) // 来自覆盖配置
    None => assert_true(false)
  }
}

// 测试3: 动态配置更新
test "动态配置更新测试" {
  let config_manager = ConfigurationManager::new()
  let dynamic_config = DynamicConfiguration::new()
  
  // 初始化配置
  let initial_config_content = "{
    \"telemetry\": {
      \"metrics\": {
        \"enabled\": true,
        \"interval\": 5000
      },
      \"logging\": {
        \"level\": \"info\"
      }
    }
  }"
  
  let initial_config = ConfigurationManager::load_from_string(config_manager, initial_config_content)
  DynamicConfiguration::set_config(dynamic_config, initial_config)
  
  // 注册配置变更监听器
  let metrics_listener = ConfigChangeListener::new("metrics.listener")
  ConfigChangeListener::on_change(metrics_listener, fn(key, old_value, new_value) {
    if key == "telemetry.metrics.interval" {
      // 处理指标间隔变更
      assert_true(new_value.to_int() > 0)
    }
  })
  
  let logging_listener = ConfigChangeListener::new("logging.listener")
  ConfigChangeListener::on_change(logging_listener, fn(key, old_value, new_value) {
    if key == "telemetry.logging.level" {
      // 处理日志级别变更
      assert_true(new_value.is_one_of(["debug", "info", "warn", "error"]))
    }
  })
  
  DynamicConfiguration::add_listener(dynamic_config, metrics_listener)
  DynamicConfiguration::add_listener(dynamic_config, logging_listener)
  
  // 验证初始配置
  let initial_interval = DynamicConfiguration::get_int(dynamic_config, "telemetry.metrics.interval")
  match initial_interval {
    Some(interval) => assert_eq(interval, 5000)
    None => assert_true(false)
  }
  
  // 动态更新配置
  let updated_config_content = "{
    \"telemetry\": {
      \"metrics\": {
        \"enabled\": true,
        \"interval\": 10000
      },
      \"logging\": {
        \"level\": \"debug\"
      }
    }
  }"
  
  let updated_config = ConfigurationManager::load_from_string(config_manager, updated_config_content)
  DynamicConfiguration::update_config(dynamic_config, updated_config)
  
  // 验证配置已更新
  let updated_interval = DynamicConfiguration::get_int(dynamic_config, "telemetry.metrics.interval")
  match updated_interval {
    Some(interval) => assert_eq(interval, 10000)
    None => assert_true(false)
  }
  
  let updated_log_level = DynamicConfiguration::get_string(dynamic_config, "telemetry.logging.level")
  match updated_log_level {
    Some(level) => assert_eq(level, "debug")
    None => assert_true(false)
  }
  
  // 测试部分配置更新
  DynamicConfiguration::update_value(dynamic_config, "telemetry.metrics.interval", 3000)
  
  let partially_updated_interval = DynamicConfiguration::get_int(dynamic_config, "telemetry.metrics.interval")
  match partially_updated_interval {
    Some(interval) => assert_eq(interval, 3000)
    None => assert_true(false)
  }
  
  // 验证其他配置未受影响
  let unchanged_log_level = DynamicConfiguration::get_string(dynamic_config, "telemetry.logging.level")
  match unchanged_log_level {
    Some(level) => assert_eq(level, "debug")
    None => assert_true(false)
  }
  
  // 测试配置回滚
  DynamicConfiguration::create_checkpoint(dynamic_config, "before.major.change")
  
  DynamicConfiguration::update_value(dynamic_config, "telemetry.metrics.interval", 15000)
  DynamicConfiguration::update_value(dynamic_config, "telemetry.logging.level", "error")
  
  // 验证变更
  let changed_interval = DynamicConfiguration::get_int(dynamic_config, "telemetry.metrics.interval")
  match changed_interval {
    Some(interval) => assert_eq(interval, 15000)
    None => assert_true(false)
  }
  
  // 回滚到检查点
  DynamicConfiguration::rollback_to_checkpoint(dynamic_config, "before.major.change")
  
  // 验证回滚结果
  let rolled_back_interval = DynamicConfiguration::get_int(dynamic_config, "telemetry.metrics.interval")
  match rolled_back_interval {
    Some(interval) => assert_eq(interval, 3000)
    None => assert_true(false)
  }
  
  let rolled_back_log_level = DynamicConfiguration::get_string(dynamic_config, "telemetry.logging.level")
  match rolled_back_log_level {
    Some(level) => assert_eq(level, "debug")
    None => assert_true(false)
  }
}

// 测试4: 配置验证和约束
test "配置验证和约束测试" {
  let config_validator = ConfigurationValidator::new()
  
  // 添加验证规则
  ConfigValidator::add_rule(config_validator, ValidationRule::required("telemetry.service.name"))
  ConfigValidator::add_rule(config_validator, ValidationRule::min_value("telemetry.service.port", 1))
  ConfigValidator::add_rule(config_validator, ValidationRule::max_value("telemetry.service.port", 65535))
  ConfigValidator::add_rule(config_validator, ValidationRule::one_of("telemetry.logging.level", ["debug", "info", "warn", "error"]))
  ConfigValidator::add_rule(config_validator, ValidationRule::min_value("telemetry.metrics.interval", 1000))
  ConfigValidator::add_rule(config_validator, ValidationRule::max_value("telemetry.metrics.retention_days", 365))
  
  // 测试有效配置
  let valid_config_content = "{
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service\",
        \"port\": 8080
      },
      \"logging\": {
        \"level\": \"info\"
      },
      \"metrics\": {
        \"interval\": 5000,
        \"retention_days\": 30
      }
    }
  }"
  
  let valid_config = ConfigurationManager::load_from_string(config_validator, valid_config_content)
  let validation_result = ConfigValidator::validate(config_validator, valid_config)
  
  assert_true(ValidationResult::is_valid(validation_result))
  assert_eq(ValidationResult::error_count(validation_result), 0)
  
  // 测试无效配置
  let invalid_config_content = "{
    \"telemetry\": {
      \"service\": {
        \"port\": 70000
      },
      \"logging\": {
        \"level\": \"invalid_level\"
      },
      \"metrics\": {
        \"interval\": 500,
        \"retention_days\": 400
      }
    }
  }"
  
  let invalid_config = ConfigurationManager::load_from_string(config_validator, invalid_config_content)
  let invalid_validation_result = ConfigValidator::validate(config_validator, invalid_config)
  
  assert_false(ValidationResult::is_valid(invalid_validation_result))
  assert_true(ValidationResult::error_count(invalid_validation_result) >= 4)
  
  // 验证具体错误
  let errors = ValidationResult::get_errors(invalid_validation_result)
  assert_true(errors.any(fn(error) {
    ValidationError::field(error) == "telemetry.service.name" && ValidationError::type(error) == "required"
  }))
  assert_true(errors.any(fn(error) {
    ValidationError::field(error) == "telemetry.service.port" && ValidationError::type(error) == "max_value"
  }))
  assert_true(errors.any(fn(error) {
    ValidationError::field(error) == "telemetry.logging.level" && ValidationError::type(error) == "one_of"
  }))
  assert_true(errors.any(fn(error) {
    ValidationError::field(error) == "telemetry.metrics.interval" && ValidationError::type(error) == "min_value"
  }))
  
  // 测试自定义验证规则
  let custom_rule = ValidationRule::custom("telemetry.service.name", fn(value) {
    match value {
      StringValue(name) => name.starts_with("azimuth-")
      _ => false
    }
  }, "Service name must start with 'azimuth-'")
  
  ConfigValidator::add_rule(config_validator, custom_rule)
  
  let invalid_name_config_content = "{
    \"telemetry\": {
      \"service\": {
        \"name\": \"invalid-service\",
        \"port\": 8080
      }
    }
  }"
  
  let invalid_name_config = ConfigurationManager::load_from_string(config_validator, invalid_name_config_content)
  let invalid_name_result = ConfigValidator::validate(config_validator, invalid_name_config)
  
  assert_false(ValidationResult::is_valid(invalid_name_result))
  
  let custom_errors = ValidationResult::get_errors(invalid_name_result)
  assert_true(custom_errors.any(fn(error) {
    ValidationError::field(error) == "telemetry.service.name" && ValidationError::message(error) == "Service name must start with 'azimuth-'"
  }))
}

// 测试5: 配置模板和继承
test "配置模板和继承测试" {
  let template_manager = ConfigurationTemplateManager::new()
  
  // 创建基础模板
  let base_template_content = "{
    \"telemetry\": {
      \"service\": {
        \"name\": \"{{service_name}}\",
        \"version\": \"{{service_version}}\",
        \"port\": {{service_port}}
      },
      \"logging\": {
        \"level\": \"{{log_level}}\",
        \"format\": \"json\"
      },
      \"metrics\": {
        \"enabled\": true,
        \"interval\": 5000
      }
    }
  }"
  
  // 注册模板
  TemplateManager::register_template(template_manager, "base", base_template_content)
  
  // 创建服务特定模板
  let web_service_template_content = "{
    \"extends\": \"base\",
    \"telemetry\": {
      \"service\": {
        \"name\": \"{{service_name}}-web\",
        \"port\": 3000
      },
      \"logging\": {
        \"level\": \"debug\"
      },
      \"web\": {
        \"static_files\": \"./public\",
        \"session_timeout\": 3600
      }
    }
  }"
  
  TemplateManager::register_template(template_manager, "web-service", web_service_template_content)
  
  // 创建API服务模板
  let api_service_template_content = "{
    \"extends\": \"base\",
    \"telemetry\": {
      \"service\": {
        \"name\": \"{{service_name}}-api\",
        \"port\": 8080
      },
      \"api\": {
        \"rate_limit\": 1000,
        \"timeout\": 30000
      }
    }
  }"
  
  TemplateManager::register_template(template_manager, "api-service", api_service_template_content)
  
  // 渲染Web服务配置
  let web_variables = [
    ("service_name", "azimuth"),
    ("service_version", "1.0.0"),
    ("service_port", 3000),
    ("log_level", "debug")
  ]
  
  let web_config = TemplateManager::render(template_manager, "web-service", web_variables)
  
  // 验证Web服务配置
  let web_service_name = ConfigurationManager::get_string(web_config, "telemetry.service.name")
  match web_service_name {
    Some(name) => assert_eq(name, "azimuth-web")
    None => assert_true(false)
  }
  
  let web_service_port = ConfigurationManager::get_int(web_config, "telemetry.service.port")
  match web_service_port {
    Some(port) => assert_eq(port, 3000)
    None => assert_true(false)
  }
  
  let web_log_level = ConfigurationManager::get_string(web_config, "telemetry.logging.level")
  match web_log_level {
    Some(level) => assert_eq(level, "debug")
    None => assert_true(false)
  }
  
  let web_static_files = ConfigurationManager::get_string(web_config, "telemetry.web.static_files")
  match web_static_files {
    Some(path) => assert_eq(path, "./public")
    None => assert_true(false)
  }
  
  // 渲染API服务配置
  let api_variables = [
    ("service_name", "azimuth"),
    ("service_version", "1.0.0"),
    ("service_port", 8080),
    ("log_level", "info")
  ]
  
  let api_config = TemplateManager::render(template_manager, "api-service", api_variables)
  
  // 验证API服务配置
  let api_service_name = ConfigurationManager::get_string(api_config, "telemetry.service.name")
  match api_service_name {
    Some(name) => assert_eq(name, "azimuth-api")
    None => assert_true(false)
  }
  
  let api_rate_limit = ConfigurationManager::get_int(api_config, "telemetry.api.rate_limit")
  match api_rate_limit {
    Some(limit) => assert_eq(limit, 1000)
    None => assert_true(false)
  }
  
  // 测试模板继承链
  let template_hierarchy = TemplateManager::get_inheritance_chain(template_manager, "web-service")
  assert_eq(template_hierarchy[0], "web-service")
  assert_eq(template_hierarchy[1], "base")
  
  // 测试模板变量验证
  let invalid_variables = [
    ("service_name", "azimuth"),
    ("service_version", "1.0.0")
    // 缺少service_port和log_level
  ]
  
  let invalid_config = TemplateManager::render(template_manager, "web-service", invalid_variables)
  
  // 验证缺少变量的处理
  let missing_port = ConfigurationManager::get_string(invalid_config, "telemetry.service.port")
  match missing_port {
    Some(port) => assert_eq(port, "{{service_port}}") // 未被替换
    None => assert_true(false)
  }
}

// 测试6: 配置安全性和加密
test "配置安全性和加密测试" {
  let secure_config_manager = SecureConfigurationManager::new()
  
  // 创建加密密钥
  let encryption_key = SecureConfigurationManager::generate_encryption_key(secure_config_manager)
  
  // 创建包含敏感信息的配置
  let sensitive_config_content = "{
    \"telemetry\": {
      \"database\": {
        \"host\": \"localhost\",
        \"port\": 5432,
        \"username\": \"admin\",
        \"password\": \"secret_password\",
        \"ssl_cert\": \"-----BEGIN CERTIFICATE-----\\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA...\\n-----END CERTIFICATE-----\"
      },
      \"api_keys\": {
        \"external_service\": \"sk-1234567890abcdef\",
        \"internal_service\": \"internal_key_123\"
      },
      \"public_info\": {
        \"service_name\": \"azimuth-service\",
        \"version\": \"1.0.0\"
      }
    }
  }"
  
  // 配置加密规则
  SecureConfigurationManager::add_encryption_rule(secure_config_manager, "telemetry.database.password")
  SecureConfigurationManager::add_encryption_rule(secure_config_manager, "telemetry.database.ssl_cert")
  SecureConfigurationManager::add_encryption_rule(secure_config_manager, "telemetry.api_keys.*")
  
  // 加载并加密配置
  let secure_config = SecureConfigurationManager::load_and_encrypt(secure_config_manager, sensitive_config_content, encryption_key)
  
  // 验证敏感字段已加密
  let encrypted_password = ConfigurationManager::get_string(secure_config, "telemetry.database.password")
  match encrypted_password {
    Some(password) => {
      assert_neq(password, "secret_password")
      assert_true(password.starts_with("encrypted:"))
    }
    None => assert_true(false)
  }
  
  let encrypted_cert = ConfigurationManager::get_string(secure_config, "telemetry.database.ssl_cert")
  match encrypted_cert {
    Some(cert) => {
      assert_neq(cert, "-----BEGIN CERTIFICATE-----")
      assert_true(cert.starts_with("encrypted:"))
    }
    None => assert_true(false)
  }
  
  let encrypted_api_key = ConfigurationManager::get_string(secure_config, "telemetry.api_keys.external_service")
  match encrypted_api_key {
    Some(key) => {
      assert_neq(key, "sk-1234567890abcdef")
      assert_true(key.starts_with("encrypted:"))
    }
    None => assert_true(false)
  }
  
  // 验证非敏感字段未加密
  let public_service_name = ConfigurationManager::get_string(secure_config, "telemetry.public_info.service_name")
  match public_service_name {
    Some(name) => assert_eq(name, "azimuth-service")
    None => assert_true(false)
  }
  
  // 解密配置
  let decrypted_config = SecureConfigurationManager::decrypt(secure_config_manager, secure_config, encryption_key)
  
  // 验证解密结果
  let decrypted_password = ConfigurationManager::get_string(decrypted_config, "telemetry.database.password")
  match decrypted_password {
    Some(password) => assert_eq(password, "secret_password")
    None => assert_true(false)
  }
  
  let decrypted_cert = ConfigurationManager::get_string(decrypted_config, "telemetry.database.ssl_cert")
  match decrypted_cert {
    Some(cert) => assert_eq(cert, "-----BEGIN CERTIFICATE-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA...\n-----END CERTIFICATE-----")
    None => assert_true(false)
  }
  
  // 测试配置访问控制
  let access_control = ConfigurationAccessControl::new()
  
  // 定义权限
  AccessControl::add_permission(access_control, "admin", ["telemetry.*"])
  AccessControl::add_permission(access_control, "developer", ["telemetry.public_info.*", "telemetry.database.host", "telemetry.database.port"])
  AccessControl::add_permission(access_control, "readonly", ["telemetry.public_info.*"])
  
  // 测试不同权限级别的访问
  let admin_access = AccessControl::can_access(access_control, "admin", "telemetry.database.password")
  assert_true(admin_access)
  
  let developer_access = AccessControl::can_access(access_control, "developer", "telemetry.database.password")
  assert_false(developer_access)
  
  let developer_public_access = AccessControl::can_access(access_control, "developer", "telemetry.public_info.service_name")
  assert_true(developer_public_access)
  
  let readonly_access = AccessControl::can_access(access_control, "readonly", "telemetry.database.host")
  assert_false(readonly_access)
  
  // 应用访问控制到配置
  let filtered_config_for_developer = AccessControl::filter_config(access_control, decrypted_config, "developer")
  
  // 验证过滤结果
  let filtered_password = ConfigurationManager::get_string(filtered_config_for_developer, "telemetry.database.password")
  match filtered_password {
    Some(_) => assert_true(false) // 开发者不应该能访问密码
    None => assert_true(true)
  }
  
  let filtered_host = ConfigurationManager::get_string(filtered_config_for_developer, "telemetry.database.host")
  match filtered_host {
    Some(host) => assert_eq(host, "localhost")
    None => assert_true(false)
  }
}

// 测试7: 配置版本控制和历史
test "配置版本控制和历史测试" {
  let version_control = ConfigurationVersionControl::new()
  
  // 创建初始配置
  let v1_config_content = "{
    \"version\": \"1.0.0\",
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service\",
        \"port\": 8080
      },
      \"metrics\": {
        \"enabled\": true,
        \"interval\": 5000
      }
    }
  }"
  
  // 提交初始版本
  let v1_config = ConfigurationManager::load_from_string(version_control, v1_config_content)
  let v1_commit = ConfigurationVersionControl::commit(version_control, v1_config, "Initial configuration")
  
  // 验证提交信息
  assert_eq(CommitInfo::version(v1_commit), "1.0.0")
  assert_eq(CommitInfo::message(v1_commit), "Initial configuration")
  assert_true(CommitInfo::timestamp(v1_commit) > 0)
  
  // 创建更新配置
  let v2_config_content = "{
    \"version\": \"1.1.0\",
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service\",
        \"port\": 8080,
        \"health_check\": \"/health\"
      },
      \"metrics\": {
        \"enabled\": true,
        \"interval\": 3000
      },
      \"logging\": {
        \"level\": \"debug\"
      }
    }
  }"
  
  // 提交更新版本
  let v2_config = ConfigurationManager::load_from_string(version_control, v2_config_content)
  let v2_commit = ConfigurationVersionControl::commit(version_control, v2_config, "Add health check and logging")
  
  // 验证版本历史
  let history = ConfigurationVersionControl::get_history(version_control)
  assert_eq(history.length(), 2)
  assert_eq(CommitInfo::version(history[0]), "1.1.0")
  assert_eq(CommitInfo::version(history[1]), "1.0.0")
  
  // 检出版本差异
  let diff = ConfigurationVersionControl::diff(version_control, "1.0.0", "1.1.0")
  
  // 验证差异
  assert_true(DiffInfo::has_changes(diff))
  assert_true(DiffInfo::has_additions(diff))
  assert_false(DiffInfo::has_deletions(diff)) // 没有删除字段
  
  let additions = DiffInfo::get_additions(diff)
  assert_true(additions.any(fn(change) {
    Change::path(change) == "telemetry.service.health_check"
  }))
  assert_true(additions.any(fn(change) {
    Change::path(change) == "telemetry.logging.level"
  }))
  
  let modifications = DiffInfo::get_modifications(diff)
  assert_true(modifications.any(fn(change) {
    Change::path(change) == "telemetry.metrics.interval" && 
    Change::old_value(change) == "5000" && 
    Change::new_value(change) == "3000"
  }))
  
  // 检出特定版本
  let v1_checkout = ConfigurationVersionControl::checkout(version_control, "1.0.0")
  let v1_interval = ConfigurationManager::get_int(v1_checkout, "telemetry.metrics.interval")
  match v1_interval {
    Some(interval) => assert_eq(interval, 5000)
    None => assert_true(false)
  }
  
  let v1_health_check = ConfigurationManager::get_string(v1_checkout, "telemetry.service.health_check")
  match v1_health_check {
    Some(_) => assert_true(false) // v1不应该有health_check
    None => assert_true(true)
  }
  
  // 创建分支
  ConfigurationVersionControl::create_branch(version_control, "feature/new-metrics")
  ConfigurationVersionControl::switch_branch(version_control, "feature/new-metrics")
  
  // 在分支上提交
  let branch_config_content = "{
    \"version\": \"1.1.0-feature\",
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service\",
        \"port\": 8080,
        \"health_check\": \"/health\"
      },
      \"metrics\": {
        \"enabled\": true,
        \"interval\": 3000,
        \"new_feature\": true
      },
      \"logging\": {
        \"level\": \"debug\"
      }
    }
  }"
  
  let branch_config = ConfigurationManager::load_from_string(version_control, branch_config_content)
  ConfigurationVersionControl::commit(version_control, branch_config, "Add new metrics feature")
  
  // 验证分支历史
  let branch_history = ConfigurationVersionControl::get_history(version_control)
  assert_eq(branch_history.length(), 1) // 分支上只有一次提交
  
  // 合并分支
  ConfigurationVersionControl::switch_branch(version_control, "main")
  let merge_result = ConfigurationVersionControl::merge(version_control, "feature/new-metrics")
  
  // 验证合并结果
  assert_true(MergeResult::is_successful(merge_result))
  
  let merged_config = ConfigurationVersionControl::get_current_config(version_control)
  let new_feature = ConfigurationManager::get_bool(merged_config, "telemetry.metrics.new_feature")
  match new_feature {
    Some(feature) => assert_true(feature)
    None => assert_true(false)
  }
  
  // 测试标签
  ConfigurationVersionControl::create_tag(version_control, "v1.1.0", "1.1.0")
  
  let tagged_config = ConfigurationVersionControl::checkout_tag(version_control, "v1.1.0")
  let tagged_version = ConfigurationManager::get_string(tagged_config, "version")
  match tagged_version {
    Some(version) => assert_eq(version, "1.1.0")
    None => assert_true(false)
  }
}

// 测试8: 配置热重载和监控
test "配置热重载和监控测试" {
  let hot_reload_manager = HotReloadManager::new()
  let config_monitor = ConfigurationMonitor::new()
  
  // 创建初始配置文件
  let config_file_path = "/tmp/azimuth_config.json"
  let initial_config_content = "{
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service\",
        \"port\": 8080
      },
      \"metrics\": {
        \"enabled\": true,
        \"interval\": 5000
      }
    }
  }"
  
  // 写入配置文件
  FileUtil::write_file(config_file_path, initial_config_content)
  
  // 加载初始配置
  let initial_config = ConfigurationManager::load_from_file(hot_reload_manager, config_file_path)
  HotReloadManager::set_config(hot_reload_manager, initial_config)
  
  // 配置文件监控
  ConfigMonitor::watch_file(config_monitor, config_file_path)
  
  // 注册重载监听器
  let reload_listener = ReloadListener::new()
  ReloadListener::on_reload(reload_listener, fn(old_config, new_config) {
    // 验证配置变更
    let old_port = ConfigurationManager::get_int(old_config, "telemetry.service.port")
    let new_port = ConfigurationManager::get_int(new_config, "telemetry.service.port")
    
    match (old_port, new_port) {
      (Some(old), Some(new)) => {
        if old != new {
          // 处理端口变更
          assert_true(new > 0 && new < 65536)
        }
      }
      _ => assert_true(false)
    }
  })
  
  HotReloadManager::add_reload_listener(hot_reload_manager, reload_listener)
  
  // 验证初始配置
  let initial_port = HotReloadManager::get_int(hot_reload_manager, "telemetry.service.port")
  match initial_port {
    Some(port) => assert_eq(port, 8080)
    None => assert_true(false)
  }
  
  // 更新配置文件
  let updated_config_content = "{
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service\",
        \"port\": 9090
      },
      \"metrics\": {
        \"enabled\": true,
        \"interval\": 3000
      },
      \"logging\": {
        \"level\": \"debug\"
      }
    }
  }"
  
  FileUtil::write_file(config_file_path, updated_config_content)
  
  // 触发热重载
  let reload_result = HotReloadManager::reload(hot_reload_manager)
  
  // 验证重载结果
  assert_true(ReloadResult::is_successful(reload_result))
  
  let updated_port = HotReloadManager::get_int(hot_reload_manager, "telemetry.service.port")
  match updated_port {
    Some(port) => assert_eq(port, 9090)
    None => assert_true(false)
  }
  
  let updated_interval = HotReloadManager::get_int(hot_reload_manager, "telemetry.metrics.interval")
  match updated_interval {
    Some(interval) => assert_eq(interval, 3000)
    None => assert_true(false)
  }
  
  let added_log_level = HotReloadManager::get_string(hot_reload_manager, "telemetry.logging.level")
  match added_log_level {
    Some(level) => assert_eq(level, "debug")
    None => assert_true(false)
  }
  
  // 测试配置验证重载
  ConfigMonitor::set_validation_enabled(config_monitor, true)
  ConfigMonitor::add_validation_rule(config_monitor, ValidationRule::min_value("telemetry.service.port", 1000))
  
  // 尝试加载无效配置
  let invalid_config_content = "{
    \"telemetry\": {
      \"service\": {
        \"name\": \"azimuth-service\",
        \"port\": 999
      }
    }
  }"
  
  FileUtil::write_file(config_file_path, invalid_config_content)
  
  let invalid_reload_result = HotReloadManager::reload(hot_reload_manager)
  
  // 验证无效配置被拒绝
  assert_false(ReloadResult::is_successful(invalid_reload_result))
  assert_true(ReloadResult::has_validation_errors(invalid_reload_result))
  
  // 验证配置未变更
  let unchanged_port = HotReloadManager::get_int(hot_reload_manager, "telemetry.service.port")
  match unchanged_port {
    Some(port) => assert_eq(port, 9090) // 应该保持之前的值
    None => assert_true(false)
  }
  
  // 测试配置变更通知
  let notification_service = ConfigurationNotificationService::new()
  
  NotificationService::subscribe(notification_service, "config.changed", fn(event) {
    // 处理配置变更通知
    assert_true(NotificationEvent::type(event) == "config.changed")
    assert_true(NotificationEvent::has_changes(event))
  })
  
  // 模拟配置变更事件
  let change_event = NotificationEvent::config_changed("telemetry.service.port", 9090, 8080)
  NotificationService::publish(notification_service, change_event)
  
  // 测试配置备份和恢复
  let backup_result = HotReloadManager::backup_config(hot_reload_manager, "/tmp/config_backup.json")
  assert_true(BackupResult::is_successful(backup_result))
  
  // 恢复配置
  let restore_result = HotReloadManager::restore_config(hot_reload_manager, "/tmp/config_backup.json")
  assert_true(RestoreResult::is_successful(restore_result))
  
  // 验证恢复后的配置
  let restored_port = HotReloadManager::get_int(hot_reload_manager, "telemetry.service.port")
  match restored_port {
    Some(port) => assert_eq(port, 9090)
    None => assert_true(false)
  }
  
  // 测试配置监控指标
  let monitoring_metrics = ConfigMonitor::get_metrics(config_monitor)
  assert_eq(MonitoringMetrics::reload_count(monitoring_metrics), 3)
  assert_eq(MonitoringMetrics::validation_failure_count(monitoring_metrics), 1)
  assert_true(MonitoringMetrics::last_reload_time(monitoring_metrics) > 0)
}