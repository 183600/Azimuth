// Azimuth 配置管理测试用例
// 专注于配置加载、验证、热更新和环境管理

// 测试1: 基础配置加载和解析
test "基础配置加载和解析" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 创建测试配置内容
  let test_config_content = "{
    \"service\": {
      \"name\": \"payment-service\",
      \"version\": \"1.2.3\",
      \"port\": 8080,
      \"host\": \"0.0.0.0\"
    },
    \"database\": {
      \"host\": \"localhost\",
      \"port\": 5432,
      \"name\": \"payments_db\",
      \"username\": \"admin\",
      \"password\": \"secret\",
      \"max_connections\": 10,
      \"timeout\": 30000
    },
    \"logging\": {
      \"level\": \"info\",
      \"format\": \"json\",
      \"outputs\": [\"console\", \"file\"],
      \"file\": {
        \"path\": \"/var/log/payment-service.log\",
        \"max_size\": \"100MB\",
        \"max_files\": 10
      }
    },
    \"features\": {
      \"enable_metrics\": true,
      \"enable_tracing\": true,
      \"enable_profiling\": false
    },
    \"limits\": {
      \"max_request_size\": \"10MB\",
      \"max_concurrent_requests\": 100,
      \"request_timeout\": 30000
    }
  }"
  
  // 加载配置
  let config_load_result = ConfigManager::load_from_string(config_manager, test_config_content)
  
  match config_load_result {
    Success(config) => {
      // 验证服务配置
      assert_eq(config.get("service.name"), "payment-service")
      assert_eq(config.get("service.version"), "1.2.3")
      assert_eq(config.get("service.port"), 8080)
      assert_eq(config.get("service.host"), "0.0.0.0")
      
      // 验证数据库配置
      assert_eq(config.get("database.host"), "localhost")
      assert_eq(config.get("database.port"), 5432)
      assert_eq(config.get("database.name"), "payments_db")
      assert_eq(config.get("database.max_connections"), 10)
      
      // 验证日志配置
      assert_eq(config.get("logging.level"), "info")
      assert_eq(config.get("logging.format"), "json")
      assert_eq(config.get("logging.file.path"), "/var/log/payment-service.log")
      
      // 验证功能开关
      assert_eq(config.get("features.enable_metrics"), true)
      assert_eq(config.get("features.enable_tracing"), true)
      assert_eq(config.get("features.enable_profiling"), false)
      
      // 验证限制配置
      assert_eq(config.get("limits.max_concurrent_requests"), 100)
      assert_eq(config.get("limits.request_timeout"), 30000)
    }
    Error(error) => assert_true(false)
  }
  
  // 测试配置文件加载
  let temp_config_file = "/tmp/test_config.json"
  File::write(temp_config_file, test_config_content)
  
  let file_load_result = ConfigManager::load_from_file(config_manager, temp_config_file)
  assert_true(file_load_result.is_success())
  
  // 清理临时文件
  File::delete(temp_config_file)
  
  // 测试配置路径访问
  let config = file_load_result.unwrap()
  let service_config = config.get_section("service")
  assert_eq(service_config.get("name"), "payment-service")
  assert_eq(service_config.get("port"), 8080)
  
  let logging_outputs = config.get_array("logging.outputs")
  assert_eq(logging_outputs, ["console", "file"])
  
  // 测试嵌套配置访问
  let max_files = config.get("logging.file.max_files")
  assert_eq(max_files, 10)
}

// 测试2: 配置验证和类型检查
test "配置验证和类型检查" {
  // 创建带验证的配置管理器
  let validating_config_manager = ValidatingConfigManager::new()
  
  // 定义配置模式
  let config_schema = {
    type: "object",
    required: ["service", "database"],
    properties: {
      service: {
        type: "object",
        required: ["name", "port"],
        properties: {
          name: { type: "string", pattern: "^[a-z][a-z0-9-]*$" },
          version: { type: "string", pattern: "^\\d+\\.\\d+\\.\\d+$" },
          port: { type: "integer", minimum: 1, maximum: 65535 },
          host: { type: "string", default: "0.0.0.0" }
        }
      },
      database: {
        type: "object",
        required: ["host", "port", "name"],
        properties: {
          host: { type: "string" },
          port: { type: "integer", minimum: 1, maximum: 65535 },
          name: { type: "string" },
          username: { type: "string" },
          password: { type: "string" },
          max_connections: { type: "integer", minimum: 1, maximum: 1000 },
          timeout: { type: "integer", minimum: 1000 }
        }
      },
      features: {
        type: "object",
        properties: {
          enable_metrics: { type: "boolean" },
          enable_tracing: { type: "boolean" },
          enable_profiling: { type: "boolean" }
        }
      }
    }
  }
  
  // 设置配置模式
  ValidatingConfigManager::set_schema(validating_config_manager, config_schema)
  
  // 测试有效配置
  let valid_config = "{
    \"service\": {
      \"name\": \"api-service\",
      \"version\": \"2.1.0\",
      \"port\": 9000
    },
    \"database\": {
      \"host\": \"db.example.com\",
      \"port\": 5432,
      \"name\": \"api_db\",
      \"max_connections\": 20
    },
    \"features\": {
      \"enable_metrics\": true,
      \"enable_tracing\": false
    }
  }"
  
  let valid_result = ValidatingConfigManager::load_and_validate(validating_config_manager, valid_config)
  assert_true(valid_result.is_success())
  
  // 测试无效配置
  let invalid_config = "{
    \"service\": {
      \"name\": \"Invalid Service\",  # 包含空格，违反模式
      \"version\": \"2.1\",          # 不是完整版本号
      \"port\": 70000               # 超出端口范围
    },
    \"database\": {
      \"host\": \"db.example.com\",
      \"port\": 5432
      # 缺少必需的name字段
    },
    \"features\": {
      \"enable_metrics\": \"yes\"     # 应该是布尔值
    }
  }"
  
  let invalid_result = ValidatingConfigManager::load_and_validate(validating_config_manager, invalid_config)
  assert_true(invalid_result.is_error())
  
  match invalid_result {
    Error(validation_errors) => {
      assert_true(validation_errors.length() >= 4)
      
      # 验证具体错误
      let service_name_error = validation_errors.find(fn(e) { 
        e.path == "service.name" and e.error.contains("pattern")
      })
      assert_true(service_name_error != None)
      
      let service_port_error = validation_errors.find(fn(e) { 
        e.path == "service.port" and e.error.contains("maximum")
      })
      assert_true(service_port_error != None)
      
      let database_name_error = validation_errors.find(fn(e) { 
        e.path == "database" and e.error.contains("required")
      })
      assert_true(database_name_error != None)
      
      let metrics_type_error = validation_errors.find(fn(e) { 
        e.path == "features.enable_metrics" and e.error.contains("boolean")
      })
      assert_true(metrics_type_error != None)
    }
    _ => assert_true(false)
  }
  
  // 测试自定义验证规则
  ValidatingConfigManager::add_custom_validator(validating_config_manager, "port_range", fn(value, path) {
    let port = value.to_int()
    if port >= 8080 and port <= 9090 {
      ValidationSuccess()
    } else {
      ValidationError("Port must be between 8080 and 9090")
    }
  })
  
  // 应用自定义验证规则
  ValidatingConfigManager::apply_custom_validation(validating_config_manager, "service.port", "port_range")
  
  // 测试自定义验证
  let custom_invalid_config = "{
    \"service\": {
      \"name\": \"test-service\",
      \"port\": 7000  # 不在自定义端口范围内
    },
    \"database\": {
      \"host\": \"localhost\",
      \"port\": 5432,
      \"name\": \"test_db\"
    }
  }"
  
  let custom_invalid_result = ValidatingConfigManager::load_and_validate(validating_config_manager, custom_invalid_config)
  assert_true(custom_invalid_result.is_error())
  
  match custom_invalid_result {
    Error(validation_errors) => {
      let custom_error = validation_errors.find(fn(e) { 
        e.path == "service.port" and e.error.contains("Port must be between 8080 and 9090")
      })
      assert_true(custom_error != None)
    }
    _ => assert_true(false)
  }
}

// 测试3: 配置环境覆盖和变量替换
test "配置环境覆盖和变量替换" {
  // 创建环境感知的配置管理器
  let env_config_manager = EnvironmentAwareConfigManager::new()
  
  // 设置环境变量
  Environment::set("SERVICE_PORT", "9000")
  Environment::set("DB_HOST", "prod-db.example.com")
  Environment::set("DB_PASSWORD", "prod_secret_password")
  Environment::set("LOG_LEVEL", "warn")
  Environment::set("ENABLE_METRICS", "true")
  
  // 创建带变量替换的配置模板
  let config_template = "{
    \"service\": {
      \"name\": \"payment-service\",
      \"port\": \"${SERVICE_PORT:8080}\",
      \"host\": \"0.0.0.0\"
    },
    \"database\": {
      \"host\": \"${DB_HOST:localhost}\",
      \"port\": 5432,
      \"name\": \"payments_db\",
      \"username\": \"admin\",
      \"password\": \"${DB_PASSWORD}\"
    },
    \"logging\": {
      \"level\": \"${LOG_LEVEL:info}\",
      \"format\": \"json\"
    },
    \"features\": {
      \"enable_metrics\": \"${ENABLE_METRICS:false}\",
      \"enable_tracing\": true
    }
  }"
  
  // 加载并解析配置
  let resolved_config = EnvironmentAwareConfigManager::load_with_substitution(env_config_manager, config_template)
  
  // 验证环境变量替换
  assert_eq(resolved_config.get("service.port"), "9000")  # 来自环境变量
  assert_eq(resolved_config.get("database.host"), "prod-db.example.com")  # 来自环境变量
  assert_eq(resolved_config.get("database.password"), "prod_secret_password")  # 来自环境变量
  assert_eq(resolved_config.get("logging.level"), "warn")  # 来自环境变量
  assert_eq(resolved_config.get("features.enable_metrics"), "true")  # 来自环境变量
  
  // 验证默认值（当环境变量不存在时）
  Environment::unset("NONEXISTENT_VAR")
  let config_with_default = "{
    \"setting\": \"${NONEXISTENT_VAR:default_value}\"
  }"
  
  let default_config = EnvironmentAwareConfigManager::load_with_substitution(env_config_manager, config_with_default)
  assert_eq(default_config.get("setting"), "default_value")
  
  // 测试环境特定配置
  let base_config = "{
    \"service\": {
      \"name\": \"payment-service\",
      \"port\": 8080
    },
    \"database\": {
      \"host\": \"localhost\",
      \"name\": \"payments_db\"
    },
    \"logging\": {
      \"level\": \"info\"
    }
  }"
  
  let prod_override = "{
    \"service\": {
      \"port\": 9000
    },
    \"database\": {
      \"host\": \"prod-db.example.com\",
      \"max_connections\": 50
    },
    \"logging\": {
      \"level\": \"warn\"
    }
  }"
  
  let dev_override = "{
    \"service\": {
      \"port\": 8081
    },
    \"database\": {
      \"host\": \"dev-db.example.com\"
    },
    \"logging\": {
      \"level\": \"debug\"
    }
  }"
  
  // 合并基础配置和生产环境覆盖
  let prod_config = EnvironmentAwareConfigManager::merge_with_override(env_config_manager, base_config, prod_override)
  assert_eq(prod_config.get("service.name"), "payment-service")  # 来自基础配置
  assert_eq(prod_config.get("service.port"), 9000)  # 来自生产覆盖
  assert_eq(prod_config.get("database.host"), "prod-db.example.com")  # 来自生产覆盖
  assert_eq(prod_config.get("database.name"), "payments_db")  # 来自基础配置
  assert_eq(prod_config.get("database.max_connections"), 50)  # 来自生产覆盖
  assert_eq(prod_config.get("logging.level"), "warn")  # 来自生产覆盖
  
  // 合并基础配置和开发环境覆盖
  let dev_config = EnvironmentAwareConfigManager::merge_with_override(env_config_manager, base_config, dev_override)
  assert_eq(dev_config.get("service.name"), "payment-service")  # 来自基础配置
  assert_eq(dev_config.get("service.port"), 8081)  # 来自开发覆盖
  assert_eq(dev_config.get("database.host"), "dev-db.example.com")  # 来自开发覆盖
  assert_eq(dev_config.get("database.name"), "payments_db")  # 来自基础配置
  assert_eq(dev_config.get("logging.level"), "debug")  # 来自开发覆盖
  
  // 测试多环境配置加载
  let environment_configs = {
    "base": base_config,
    "production": prod_override,
    "development": dev_override
  }
  
  let loaded_prod_config = EnvironmentAwareConfigManager::load_for_environment(env_config_manager, environment_configs, "production")
  assert_eq(loaded_prod_config.get("service.port"), 9000)
  assert_eq(loaded_prod_config.get("database.host"), "prod-db.example.com")
  
  let loaded_dev_config = EnvironmentAwareConfigManager::load_for_environment(env_config_manager, environment_configs, "development")
  assert_eq(loaded_dev_config.get("service.port"), 8081)
  assert_eq(loaded_dev_config.get("database.host"), "dev-db.example.com")
}

// 测试4: 配置热更新和监听
test "配置热更新和监听" {
  // 创建热更新配置管理器
  let hot_reload_config_manager = HotReloadConfigManager::new()
  
  // 初始配置
  let initial_config = "{
    \"service\": {
      \"name\": \"api-service\",
      \"port\": 8080,
      \"workers\": 4
    },
    \"database\": {
      \"host\": \"localhost\",
      \"pool_size\": 10
    },
    \"features\": {
      \"enable_cache\": true,
      \"cache_ttl\": 300
    }
  }"
  
  // 加载初始配置
  let config = HotReloadConfigManager::load(hot_reload_config_manager, initial_config)
  assert_eq(config.get("service.port"), 8080)
  assert_eq(config.get("service.workers"), 4)
  assert_eq(config.get("database.pool_size"), 10)
  assert_eq(config.get("features.enable_cache"), true)
  assert_eq(config.get("features.cache_ttl"), 300)
  
  // 创建配置变更监听器
  let change_log = []
  
  HotReloadConfigManager::add_change_listener(hot_reload_config_manager, fn(changes) {
    for change in changes {
      change_log = change_log.push({
        path: change.path,
        old_value: change.old_value,
        new_value: change.new_value,
        timestamp: change.timestamp
      })
    }
  })
  
  // 添加特定路径监听器
  let service_changes = []
  
  HotReloadConfigManager::add_path_listener(hot_reload_config_manager, "service", fn(change) {
    service_changes = service_changes.push({
      path: change.path,
      new_value: change.new_value,
      timestamp: change.timestamp
    })
  })
  
  // 更新配置
  let updated_config = "{
    \"service\": {
      \"name\": \"api-service\",
      \"port\": 9090,
      \"workers\": 8
    },
    \"database\": {
      \"host\": \"localhost\",
      \"pool_size\": 20
    },
    \"features\": {
      \"enable_cache\": true,
      \"cache_ttl\": 600
    }
  }"
  
  // 触发热更新
  let update_result = HotReloadConfigManager::update(hot_reload_config_manager, updated_config)
  assert_true(update_result.success)
  
  // 验证配置已更新
  let updated_config_obj = HotReloadConfigManager::get_current(hot_reload_config_manager)
  assert_eq(updated_config_obj.get("service.port"), 9090)
  assert_eq(updated_config_obj.get("service.workers"), 8)
  assert_eq(updated_config_obj.get("database.pool_size"), 20)
  assert_eq(updated_config_obj.get("features.cache_ttl"), 600)
  
  // 验证变更日志
  assert_eq(change_log.length(), 4)  # 4个配置项变更
  
  let port_change = change_log.find(fn(c) { c.path == "service.port" })
  assert_true(port_change != None)
  assert_eq(port_change.old_value, 8080)
  assert_eq(port_change.new_value, 9090)
  
  let workers_change = change_log.find(fn(c) { c.path == "service.workers" })
  assert_true(workers_change != None)
  assert_eq(workers_change.old_value, 4)
  assert_eq(workers_change.new_value, 8)
  
  let pool_size_change = change_log.find(fn(c) { c.path == "database.pool_size" })
  assert_true(pool_size_change != None)
  assert_eq(pool_size_change.old_value, 10)
  assert_eq(pool_size_change.new_value, 20)
  
  let cache_ttl_change = change_log.find(fn(c) { c.path == "features.cache_ttl" })
  assert_true(cache_ttl_change != None)
  assert_eq(cache_ttl_change.old_value, 300)
  assert_eq(cache_ttl_change.new_value, 600)
  
  // 验证特定路径监听器
  assert_eq(service_changes.length(), 2)  # 2个service配置项变更
  
  // 测试配置回滚
  let rollback_result = HotReloadConfigManager::rollback(hot_reload_config_manager, 1)  # 回滚到1个版本前
  assert_true(rollback_result.success)
  
  let rolled_back_config = HotReloadConfigManager::get_current(hot_reload_config_manager)
  assert_eq(rolled_back_config.get("service.port"), 8080)  # 回滚到原始值
  assert_eq(rolled_back_config.get("service.workers"), 4)   # 回滚到原始值
  
  // 测试配置历史
  let config_history = HotReloadConfigManager::get_history(hot_reload_config_manager)
  assert_eq(config_history.length(), 3)  # 初始 + 更新 + 回滚
  
  // 测试配置差异比较
  let diff = HotReloadConfigManager::diff(hot_reload_config_manager, 0, 1)  # 比较版本0和版本1
  assert_eq(diff.added.length(), 0)  # 没有新增字段
  assert_eq(diff.removed.length(), 0)  # 没有删除字段
  assert_eq(diff.modified.length(), 4)  # 4个修改字段
}

// 测试5: 配置加密和安全
test "配置加密和安全" {
  // 创建安全配置管理器
  let secure_config_manager = SecureConfigManager::new({
    encryption_key: "test-encryption-key-32-chars-long",
    algorithm: "AES-256-GCM"
  })
  
  // 创建包含敏感信息的配置
  let sensitive_config = "{
    \"service\": {
      \"name\": \"payment-service\",
      \"port\": 8080
    },
    \"database\": {
      \"host\": \"db.example.com\",
      \"username\": \"admin\",
      \"password\": \"super_secret_password\",
      \"api_key\": \"sk-1234567890abcdef\"
    },
    \"external_apis\": {
      \"stripe\": {
        \"secret_key\": \"sk_live_1234567890\",
        \"webhook_secret\": \"whsec_1234567890\"
      },
      \"twilio\": {
        \"account_sid\": \"AC1234567890\",
        \"auth_token\": \"auth_token_1234567890\"
      }
    }
  }"
  
  // 标记敏感字段
  SecureConfigManager::mark_sensitive(secure_config_manager, [
    "database.password",
    "database.api_key",
    "external_apis.stripe.secret_key",
    "external_apis.stripe.webhook_secret",
    "external_apis.twilio.auth_token"
  ])
  
  // 加载配置并加密敏感字段
  let encrypted_config_result = SecureConfigManager::load_and_encrypt(secure_config_manager, sensitive_config)
  
  match encrypted_config_result {
    Success(encrypted_config) => {
      // 验证非敏感字段保持原样
      assert_eq(encrypted_config.get("service.name"), "payment-service")
      assert_eq(encrypted_config.get("service.port"), 8080)
      assert_eq(encrypted_config.get("database.host"), "db.example.com")
      assert_eq(encrypted_config.get("database.username"), "admin")
      
      // 验证敏感字段已加密
      let encrypted_password = encrypted_config.get("database.password")
      assert_not_eq(encrypted_password, "super_secret_password")
      assert_true(encrypted_password.starts_with("enc:"))
      
      let encrypted_api_key = encrypted_config.get("database.api_key")
      assert_not_eq(encrypted_api_key, "sk-1234567890abcdef")
      assert_true(encrypted_api_key.starts_with("enc:"))
      
      let encrypted_stripe_key = encrypted_config.get("external_apis.stripe.secret_key")
      assert_not_eq(encrypted_stripe_key, "sk_live_1234567890")
      assert_true(encrypted_stripe_key.starts_with("enc:"))
    }
    Error(error) => assert_true(false)
  }
  
  // 解密敏感字段
  let decrypted_config = SecureConfigManager::decrypt_sensitive(secure_config_manager, encrypted_config_result.unwrap())
  
  // 验证解密后的值
  assert_eq(decrypted_config.get("database.password"), "super_secret_password")
  assert_eq(decrypted_config.get("database.api_key"), "sk-1234567890abcdef")
  assert_eq(decrypted_config.get("external_apis.stripe.secret_key"), "sk_live_1234567890")
  
  // 测试配置访问控制
  let access_control_config = SecureConfigManager::with_access_control(secure_config_manager, {
    roles: {
      "admin": {
        read: ["*"],
        write: ["*"]
      },
      "operator": {
        read: ["service.*", "database.host", "database.username"],
        write: ["service.port"]
      },
      "monitor": {
        read: ["service.name", "service.port"],
        write: []
      }
    }
  })
  
  // 测试不同角色的访问权限
  let admin_access = SecureConfigManager::check_access(access_control_config, "admin", "read", "database.password")
  assert_true(admin_access.allowed)
  
  let operator_read_access = SecureConfigManager::check_access(access_control_config, "operator", "read", "database.password")
  assert_false(operator_read_access.allowed)  # 操作员不能读取密码
  
  let operator_write_access = SecureConfigManager::check_access(access_control_config, "operator", "write", "service.port")
  assert_true(operator_write_access.allowed)  # 操作员可以修改端口
  
  let monitor_write_access = SecureConfigManager::check_access(access_control_config, "monitor", "write", "service.port")
  assert_false(monitor_write_access.allowed)  # 监控角色不能写入
  
  // 测试配置审计日志
  let audit_log = []
  
  SecureConfigManager::enable_audit_logging(secure_config_manager, fn(entry) {
    audit_log = audit_log.push({
      action: entry.action,
      field: entry.field,
      user: entry.user,
      timestamp: entry.timestamp,
      success: entry.success
    })
  })
  
  // 模拟配置访问
  SecureConfigManager::log_access(secure_config_manager, {
    action: "read",
    field: "database.password",
    user: "admin",
    success: true
  })
  
  SecureConfigManager::log_access(secure_config_manager, {
    action: "write",
    field: "service.port",
    user: "operator",
    success: true
  })
  
  SecureConfigManager::log_access(secure_config_manager, {
    action: "read",
    field: "database.password",
    user: "operator",
    success: false
  })
  
  // 验证审计日志
  assert_eq(audit_log.length(), 3)
  
  let admin_read = audit_log.find(fn(entry) { 
    entry.user == "admin" and entry.action == "read" and entry.field == "database.password"
  })
  assert_true(admin_read != None)
  assert_true(admin_read.success)
  
  let operator_failed_read = audit_log.find(fn(entry) { 
    entry.user == "operator" and entry.action == "read" and entry.field == "database.password"
  })
  assert_true(operator_failed_read != None)
  assert_false(operator_failed_read.success)
}

// 测试6: 配置模板和继承
test "配置模板和继承" {
  // 创建模板配置管理器
  let template_config_manager = TemplateConfigManager::new()
  
  // 定义基础模板
  let base_template = "{
    \"service\": {
      \"name\": \"{{service_name}}\",
      \"version\": \"{{version | default: '1.0.0'}}\",
      \"port\": {{port | default: 8080}},
      \"host\": \"{{host | default: '0.0.0.0'}}\"
    },
    \"logging\": {
      \"level\": \"{{log_level | default: 'info'}}\",
      \"format\": \"{{log_format | default: 'json'}}\",
      \"outputs\": [\"{{log_output | default: 'console'}}\"]
    },
    \"metrics\": {
      \"enabled\": {{metrics_enabled | default: true}},
      \"endpoint\": \"{{metrics_endpoint | default: '/metrics'}}\"
    }
  }"
  
  // 定义数据库模板
  let database_template = "{
    \"database\": {
      \"type\": \"{{db_type | default: 'postgresql'}}\",
      \"host\": \"{{db_host}}\",
      \"port\": {{db_port | default: 5432}},
      \"name\": \"{{db_name}}\",
      \"pool_size\": {{db_pool_size | default: 10}},
      \"timeout\": {{db_timeout | default: 30000}}
    }
  }"
  
  // 注册模板
  TemplateConfigManager::register_template(template_config_manager, "base", base_template)
  TemplateConfigManager::register_template(template_config_manager, "database", database_template)
  
  // 定义模板变量
  let template_vars = {
    service_name: "payment-service",
    version: "2.1.0",
    port: 9000,
    log_level: "warn",
    db_type: "postgresql",
    db_host: "db.example.com",
    db_name: "payments",
    db_pool_size: 20
  }
  
  // 渲染基础模板
  let rendered_base = TemplateConfigManager::render_template(template_config_manager, "base", template_vars)
  
  // 验证渲染结果
  assert_eq(rendered_base.get("service.name"), "payment-service")
  assert_eq(rendered_base.get("service.version"), "2.1.0")
  assert_eq(rendered_base.get("service.port"), 9000)
  assert_eq(rendered_base.get("service.host"), "0.0.0.0")  # 使用默认值
  assert_eq(rendered_base.get("logging.level"), "warn")
  assert_eq(rendered_base.get("logging.format"), "json")  # 使用默认值
  assert_eq(rendered_base.get("metrics.enabled"), true)   # 使用默认值
  
  // 渲染数据库模板
  let rendered_db = TemplateConfigManager::render_template(template_config_manager, "database", template_vars)
  
  // 验证数据库模板渲染
  assert_eq(rendered_db.get("database.type"), "postgresql")
  assert_eq(rendered_db.get("database.host"), "db.example.com")
  assert_eq(rendered_db.get("database.port"), 5432)  # 使用默认值
  assert_eq(rendered_db.get("database.name"), "payments")
  assert_eq(rendered_db.get("database.pool_size"), 20)
  assert_eq(rendered_db.get("database.timeout"), 30000)  # 使用默认值
  
  // 测试模板继承
  let service_template = "{
    \"extends\": \"base\",
    \"service\": {
      \"name\": \"{{service_name}}-service\",
      \"discovery\": {
        \"enabled\": {{discovery_enabled | default: true}},
        \"type\": \"{{discovery_type | default: 'consul'}}\"
      }
    },
    \"features\": {
      \"enable_tracing\": {{tracing_enabled | default: false}},
      \"enable_profiling\": {{profiling_enabled | default: false}}
    }
  }"
  
  TemplateConfigManager::register_template(template_config_manager, "service", service_template)
  
  // 渲染继承模板
  let service_vars = {
    service_name: "user",
    port: 8081,
    log_level: "debug",
    discovery_type: "eureka",
    tracing_enabled: true,
    profiling_enabled: true
  }
  
  let rendered_service = TemplateConfigManager::render_template(template_config_manager, "service", service_vars)
  
  // 验证继承结果
  assert_eq(rendered_service.get("service.name"), "user-service")  # 继承并修改
  assert_eq(rendered_service.get("service.version"), "1.0.0")     # 继承默认值
  assert_eq(rendered_service.get("service.port"), 8081)           # 覆盖默认值
  assert_eq(rendered_service.get("service.discovery.enabled"), true)
  assert_eq(rendered_service.get("service.discovery.type"), "eureka")
  assert_eq(rendered_service.get("logging.level"), "debug")       # 继承并覆盖
  assert_eq(rendered_service.get("features.enable_tracing"), true)
  assert_eq(rendered_service.get("features.enable_profiling"), true)
  
  // 测试多层继承
  let microservice_template = "{
    \"extends\": \"service\",
    \"deployment\": {
      \"replicas\": {{replicas | default: 3}},
      \"resources\": {
        \"cpu\": {{cpu_limit | default: 500}},
        \"memory\": \"{{memory_limit | default: '512Mi'}}\"
      }
    }
  }"
  
  TemplateConfigManager::register_template(template_config_manager, "microservice", microservice_template)
  
  let microservice_vars = {
    service_name: "order",
    port: 8082,
    replicas: 5,
    cpu_limit: 1000,
    memory_limit: "1Gi"
  }
  
  let rendered_microservice = TemplateConfigManager::render_template(template_config_manager, "microservice", microservice_vars)
  
  // 验证多层继承
  assert_eq(rendered_microservice.get("service.name"), "order-service")
  assert_eq(rendered_microservice.get("service.port"), 8082)
  assert_eq(rendered_microservice.get("service.discovery.type"), "consul")  # 继承自service模板的默认值
  assert_eq(rendered_microservice.get("deployment.replicas"), 5)
  assert_eq(rendered_microservice.get("deployment.resources.cpu"), 1000)
  assert_eq(rendered_microservice.get("deployment.resources.memory"), "1Gi")
  
  // 测试模板组合
  let combined_config = TemplateConfigManager::combine_templates(template_config_manager, ["base", "database"], template_vars)
  
  // 验证组合结果
  assert_eq(combined_config.get("service.name"), "payment-service")
  assert_eq(combined_config.get("database.host"), "db.example.com")
  assert_eq(combined_config.get("logging.level"), "warn")
}

// 测试7: 配置版本管理和迁移
test "配置版本管理和迁移" {
  // 创建版本化配置管理器
  let versioned_config_manager = VersionedConfigManager::new()
  
  // 定义配置版本模式
  let version_schemas = {
    "1.0.0": {
      type: "object",
      required: ["service", "database"],
      properties: {
        service: {
          type: "object",
          required: ["name", "port"],
          properties: {
            name: { type: "string" },
            port: { type: "integer" }
          }
        },
        database: {
          type: "object",
          required: ["host", "port"],
          properties: {
            host: { type: "string" },
            port: { type: "integer" },
            name: { type: "string" }
          }
        }
      }
    },
    "2.0.0": {
      type: "object",
      required: ["service", "database"],
      properties: {
        service: {
          type: "object",
          required: ["name", "port", "version"],
          properties: {
            name: { type: "string" },
            port: { type: "integer" },
            version: { type: "string" },
            host: { type: "string" }
          }
        },
        database: {
          type: "object",
          required: ["host", "port", "name"],
          properties: {
            host: { type: "string" },
            port: { type: "integer" },
            name: { type: "string" },
            pool_size: { type: "integer" }
          }
        },
        logging: {
          type: "object",
          properties: {
            level: { type: "string" },
            format: { type: "string" }
          }
        }
      }
    }
  }
  
  // 注册版本模式
  for (version, schema) in version_schemas {
    VersionedConfigManager::register_schema(versioned_config_manager, version, schema)
  }
  
  // 定义迁移规则
  VersionedConfigManager::add_migration(versioned_config_manager, "1.0.0", "2.0.0", fn(config) {
    // 添加service.version字段
    if not(config.contains("service.version")) {
      config.set("service.version", "1.0.0")
    }
    
    // 添加service.host字段
    if not(config.contains("service.host")) {
      config.set("service.host", "0.0.0.0")
    }
    
    // 添加database.pool_size字段
    if not(config.contains("database.pool_size")) {
      config.set("database.pool_size", 10)
    }
    
    // 添加logging部分
    if not(config.contains("logging")) {
      config.set("logging.level", "info")
      config.set("logging.format", "json")
    }
    
    config
  })
  
  // 创建1.0.0版本的配置
  let v1_config = "{
    \"version\": \"1.0.0\",
    \"service\": {
      \"name\": \"payment-service\",
      \"port\": 8080
    },
    \"database\": {
      \"host\": \"localhost\",
      \"port\": 5432,
      \"name\": \"payments\"
    }
  }"
  
  // 加载1.0.0配置
  let loaded_v1 = VersionedConfigManager::load(versioned_config_manager, v1_config)
  assert_eq(loaded_v1.get("version"), "1.0.0")
  assert_eq(loaded_v1.get("service.name"), "payment-service")
  assert_eq(loaded_v1.get("service.port"), 8080)
  
  // 迁移到2.0.0版本
  let migrated_config = VersionedConfigManager::migrate(versioned_config_manager, loaded_v1, "2.0.0")
  
  // 验证迁移结果
  assert_eq(migrated_config.get("version"), "2.0.0")
  assert_eq(migrated_config.get("service.name"), "payment-service")
  assert_eq(migrated_config.get("service.port"), 8080)
  assert_eq(migrated_config.get("service.version"), "1.0.0")  # 新增字段
  assert_eq(migrated_config.get("service.host"), "0.0.0.0")   # 新增字段
  assert_eq(migrated_config.get("database.pool_size"), 10)    # 新增字段
  assert_eq(migrated_config.get("logging.level"), "info")     # 新增部分
  assert_eq(migrated_config.get("logging.format"), "json")    # 新增部分
  
  // 验证迁移后的配置符合2.0.0模式
  let validation_result = VersionedConfigManager::validate_against_schema(versioned_config_manager, migrated_config, "2.0.0")
  assert_true(validation_result.is_valid)
  
  // 测试版本兼容性检查
  let compatibility = VersionedConfigManager::check_compatibility(versioned_config_manager, "1.0.0", "2.0.0")
  assert_true(compatibility.compatible)
  assert_true(compatibility.requires_migration)
  
  let backward_compatibility = VersionedConfigManager::check_compatibility(versioned_config_manager, "2.0.0", "1.0.0")
  assert_false(backward_compatibility.compatible)
  
  // 测试配置版本历史
  let version_history = VersionedConfigManager::get_version_history(versioned_config_manager)
  assert_eq(version_history.length(), 2)
  assert_eq(version_history[0].version, "1.0.0")
  assert_eq(version_history[1].version, "2.0.0")
  
  // 测试版本回滚
  let rolled_back_config = VersionedConfig_manager::rollback_to_version(versioned_config_manager, migrated_config, "1.0.0")
  
  // 验证回滚结果
  assert_eq(rolled_back_config.get("version"), "1.0.0")
  assert_eq(rolled_back_config.get("service.name"), "payment-service")
  assert_eq(rolled_back_config.get("service.port"), 8080)
  assert_false(rolled_back_config.contains("service.version"))  # 1.0.0版本中不存在此字段
  assert_false(rolled_back_config.contains("logging"))         # 1.0.0版本中不存在此部分
}

// 测试8: 配置性能和优化
test "配置性能和优化" {
  // 创建性能优化配置管理器
  let performance_config_manager = PerformanceConfigManager::new({
    cache_size: 100,
    cache_ttl: 300,  # 5分钟
    lazy_loading: true,
    compression: true
  })
  
  // 创建大型配置文件进行性能测试
  let large_config_content = generate_large_config(1000)  # 生成包含1000个配置项的大型配置
  
  // 测试配置加载性能
  let load_start_time = Time::now()
  let load_result = PerformanceConfigManager::load(performance_config_manager, large_config_content)
  let load_time = Time::now() - load_start_time
  
  assert_true(load_result.is_success())
  assert_true(load_time < 1000)  # 应该在1秒内完成
  
  // 测试配置访问性能
  let access_times = []
  
  for i in 0..=100 {
    let access_start = Time::now()
    let value = load_result.get("item_" + i.to_string())
    let access_time = Time::now() - access_start
    access_times = access_times.push(access_time)
  }
  
  // 计算平均访问时间
  let avg_access_time = access_times.reduce(fn(acc, time) { acc + time }, 0) / access_times.length()
  assert_true(avg_access_time < 10)  # 平均访问时间应该小于10ms
  
  // 测试缓存效果
  let cache_access_times = []
  
  # 第一次访问（填充缓存）
  let first_access_start = Time::now()
  load_result.get("item_500")
  let first_access_time = Time::now() - first_access_start
  
  # 第二次访问（从缓存读取）
  let second_access_start = Time::now()
  load_result.get("item_500")
  let second_access_time = Time::now() - second_access_start
  
  # 缓存访问应该更快
  assert_true(second_access_time < first_access_time)
  
  // 测试配置压缩
  let compressed_config = PerformanceConfigManager::compress_config(performance_config_manager, large_config_content)
  let compression_ratio = compressed_config.length().to_float() / large_config_content.length().to_float()
  assert_true(compression_ratio < 0.8)  # 压缩率应该至少20%
  
  // 测试配置解压缩
  let decompressed_config = PerformanceConfigManager::decompress_config(performance_config_manager, compressed_config)
  assert_eq(decompressed_config.length(), large_config_content.length())
  
  // 测试配置分块加载
  let chunk_size = 100
  let chunks = PerformanceConfigManager::split_config(performance_config_manager, large_config_content, chunk_size)
  assert_eq(chunks.length(), 10)  # 1000项分成10块，每块100项
  
  // 并行加载配置块
  let parallel_load_start = Time::now()
  let loaded_chunks = []
  
  for chunk in chunks {
    let chunk_result = PerformanceConfigManager::load_chunk(performance_config_manager, chunk)
    loaded_chunks = loaded_chunks.push(chunk_result)
  }
  
  let parallel_load_time = Time::now() - parallel_load_start
  
  # 合并加载的块
  let merged_config = PerformanceConfigManager::merge_chunks(performance_config_manager, loaded_chunks)
  assert_true(merged_config.is_success())
  
  // 测试配置索引优化
  let indexed_config = PerformanceConfigManager::create_index(performance_config_manager, load_result, ["name", "type", "category"])
  
  // 使用索引进行快速查找
  let index_search_start = Time::now()
  let indexed_results = PerformanceConfigManager::search_by_index(indexed_config, "name", "item_500")
  let index_search_time = Time::now() - index_search_start
  
  // 常规搜索
  let regular_search_start = Time::now()
  let regular_results = load_result.filter(fn(key, value) { key == "item_500" })
  let regular_search_time = Time::now() - regular_search_start
  
  // 索引搜索应该更快
  assert_true(index_search_time < regular_search_time)
  
  // 测试配置预加载
  let frequently_accessed_keys = ["item_1", "item_2", "item_3", "item_4", "item_5"]
  PerformanceConfigManager::preload_keys(performance_config_manager, load_result, frequently_accessed_keys)
  
  // 验证预加载效果
  let preload_access_start = Time::now()
  for key in frequently_accessed_keys {
    load_result.get(key)
  }
  let preload_access_time = Time::now() - preload_access_start
  
  // 预加载的访问应该很快
  assert_true(preload_access_time < 50)  # 5个键的访问应该在50ms内完成
  
  // 获取性能统计
  let performance_stats = PerformanceConfigManager::get_stats(performance_config_manager)
  
  assert_true(performance_stats.cache_hits > 0)
  assert_true(performance_stats.cache_misses > 0)
  assert_true(performance_stats.avg_load_time > 0)
  assert_true(performance_stats.avg_access_time > 0)
  assert_true(performance_stats.compression_ratio > 0.0 and performance_stats.compression_ratio < 1.0)
}

// 辅助函数：生成大型配置内容
fn generate_large_config(num_items) {
  let config_parts = ["{"]
  
  # 添加基础配置
  config_parts = config_parts.push("\"service\": {\"name\": \"test-service\", \"port\": 8080},")
  
  # 生成大量配置项
  for i in 0..=num_items {
    let item = "\"item_" + i.to_string() + "\": {\"name\": \"item_" + i.to_string() + "\", \"value\": " + i.to_string() + ", \"type\": \"test\"}"
    if i < num_items {
      item = item + ","
    }
    config_parts = config_parts.push(item)
  }
  
  config_parts = config_parts.push("}")
  
  config_parts.join("")
}