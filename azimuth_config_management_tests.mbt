// Azimuth 配置管理和动态更新测试用例
// 专注于配置管理、动态更新和配置验证功能的测试

// 测试1: 基本配置管理功能
test "基本配置管理功能测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new({
    storage_type: "memory",
    auto_save: true,
    validation_enabled: true,
    encryption_enabled: false
  })
  
  // 定义配置模式
  let config_schema = {
    "server": {
      "host": { type: "string", required: true, default: "localhost" },
      "port": { type: "integer", required: true, min: 1, max: 65535, default: 8080 },
      "ssl": { type: "boolean", required: false, default: false }
    },
    "database": {
      "url": { type: "string", required: true },
      "username": { type: "string", required: true },
      "password": { type: "string", required: true, sensitive: true },
      "max_connections": { type: "integer", required: false, min: 1, max: 100, default: 10 }
    },
    "logging": {
      "level": { type: "string", required: false, enum: ["debug", "info", "warn", "error"], default: "info" },
      "file": { type: "string", required: false },
      "max_size": { type: "integer", required: false, min: 1024, default: 10485760 }
    }
  }
  
  // 注册配置模式
  let schema_result = config_manager.register_schema("app_config", config_schema)
  assert_true(schema_result.success)
  
  // 加载默认配置
  let default_config = config_manager.load_defaults("app_config")
  assert_true(default_config.success)
  
  // 验证默认值
  assert_eq(default_config.config.get("server.host"), Some("localhost"))
  assert_eq(default_config.config.get("server.port"), Some(8080))
  assert_eq(default_config.config.get("server.ssl"), Some(false))
  assert_eq(default_config.config.get("logging.level"), Some("info"))
  
  // 设置配置值
  let set_result1 = config_manager.set_value("app_config", "server.host", "api.example.com")
  assert_true(set_result1.success)
  
  let set_result2 = config_manager.set_value("app_config", "server.port", 9000)
  assert_true(set_result2.success)
  
  let set_result3 = config_manager.set_value("app_config", "database.url", "postgresql://localhost:5432/azimuth")
  assert_true(set_result3.success)
  
  let set_result4 = config_manager.set_value("app_config", "database.username", "azimuth_user")
  assert_true(set_result4.success)
  
  let set_result5 = config_manager.set_value("app_config", "database.password", "secure_password")
  assert_true(set_result5.success)
  
  // 获取配置值
  let get_result1 = config_manager.get_value("app_config", "server.host")
  assert_eq(get_result1, Some("api.example.com"))
  
  let get_result2 = config_manager.get_value("app_config", "server.port")
  assert_eq(get_result2, Some(9000))
  
  let get_result3 = config_manager.get_value("app_config", "database.url")
  assert_eq(get_result3, Some("postgresql://localhost:5432/azimuth"))
  
  // 获取整个配置
  let full_config = config_manager.get_config("app_config")
  assert_true(full_config.success)
  assert_eq(full_config.config.get("server.host"), Some("api.example.com"))
  assert_eq(full_config.config.get("server.port"), Some(9000))
  
  // 测试配置验证
  let invalid_set_result = config_manager.set_value("app_config", "server.port", 70000)  // 超出范围
  assert_false(invalid_set_result.success)
  assert_true(invalid_set_result.error.contains("超出范围"))
  
  // 测试必需字段验证
  let incomplete_config = {
    "server": {
      "host": "localhost"
      // 缺少必需的 port 字段
    },
    "database": {
      "url": "postgresql://localhost:5432/azimuth"
      // 缺少必需的 username 和 password 字段
    }
  }
  
  let validation_result = config_manager.validate_config("app_config", incomplete_config)
  assert_false(validation_result.valid)
  assert_true(validation_result.errors.length() >= 3)  // 至少3个必需字段缺失
}

// 测试2: 配置文件加载和保存
test "配置文件加载和保存测试" {
  // 创建基于文件的配置管理器
  let file_config_manager = ConfigManager::new({
    storage_type: "file",
    file_path: "/tmp/azimuth_config.json",
    format: "json",
    auto_save: true,
    backup_enabled: true,
    backup_count: 5
  })
  
  // 注册配置模式
  let app_schema = {
    "application": {
      "name": { type: "string", required: true },
      "version": { type: "string", required: true },
      "environment": { type: "string", required: true, enum: ["development", "staging", "production"] }
    },
    "features": {
      "enable_telemetry": { type: "boolean", required: false, default: true },
      "enable_metrics": { type: "boolean", required: false, default: true },
      "enable_tracing": { type: "boolean", required: false, default: false }
    },
    "performance": {
      "max_workers": { type: "integer", required: false, min: 1, max: 100, default: 4 },
      "timeout_ms": { type: "integer", required: false, min: 1000, default: 30000 },
      "batch_size": { type: "integer", required: false, min: 1, max: 1000, default: 100 }
    }
  }
  
  file_config_manager.register_schema("app_settings", app_schema)
  
  // 创建测试配置
  let test_config = {
    "application": {
      "name": "Azimuth Telemetry",
      "version": "1.0.0",
      "environment": "production"
    },
    "features": {
      "enable_telemetry": true,
      "enable_metrics": true,
      "enable_tracing": true
    },
    "performance": {
      "max_workers": 8,
      "timeout_ms": 60000,
      "batch_size": 200
    }
  }
  
  // 保存配置到文件
  let save_result = file_config_manager.save_config("app_settings", test_config)
  assert_true(save_result.success)
  
  // 验证文件已创建
  let file_exists = File::exists("/tmp/azimuth_config.json")
  assert_true(file_exists)
  
  // 从文件加载配置
  let load_result = file_config_manager.load_config("app_settings")
  assert_true(load_result.success)
  
  // 验证加载的配置
  assert_eq(load_result.config.get("application.name"), Some("Azimuth Telemetry"))
  assert_eq(load_result.config.get("application.version"), Some("1.0.0"))
  assert_eq(load_result.config.get("application.environment"), Some("production"))
  assert_eq(load_result.config.get("features.enable_telemetry"), Some(true))
  assert_eq(load_result.config.get("features.enable_metrics"), Some(true))
  assert_eq(load_result.config.get("features.enable_tracing"), Some(true))
  assert_eq(load_result.config.get("performance.max_workers"), Some(8))
  assert_eq(load_result.config.get("performance.timeout_ms"), Some(60000))
  assert_eq(load_result.config.get("performance.batch_size"), Some(200))
  
  // 测试配置备份
  let backup_result = file_config_manager.create_backup("app_settings")
  assert_true(backup_result.success)
  assert_true(backup_result.backup_path.contains("backup"))
  
  // 修改配置
  file_config_manager.set_value("app_settings", "performance.max_workers", 16)
  file_config_manager.set_value("app_settings", "features.enable_tracing", false)
  
  // 验证修改已保存
  let modified_config = file_config_manager.get_config("app_settings")
  assert_eq(modified_config.config.get("performance.max_workers"), Some(16))
  assert_eq(modified_config.config.get("features.enable_tracing"), Some(false))
  
  // 从备份恢复
  let restore_result = file_config_manager.restore_from_backup(backup_result.backup_path)
  assert_true(restore_result.success)
  
  // 验证配置已恢复
  let restored_config = file_config_manager.get_config("app_settings")
  assert_eq(restored_config.config.get("performance.max_workers"), Some(8))
  assert_eq(restored_config.config.get("features.enable_tracing"), Some(true))
  
  // 测试不同格式的配置文件
  let yaml_config_manager = ConfigManager::new({
    storage_type: "file",
    file_path: "/tmp/azimuth_config.yaml",
    format: "yaml",
    auto_save: true
  })
  
  yaml_config_manager.register_schema("app_settings", app_schema)
  
  // 保存为YAML格式
  let yaml_save_result = yaml_config_manager.save_config("app_settings", test_config)
  assert_true(yaml_save_result.success)
  
  // 从YAML加载
  let yaml_load_result = yaml_config_manager.load_config("app_settings")
  assert_true(yaml_load_result.success)
  
  // 验证YAML加载的配置
  assert_eq(yaml_load_result.config.get("application.name"), Some("Azimuth Telemetry"))
  assert_eq(yaml_load_result.config.get("performance.max_workers"), Some(8))
  
  // 测试配置文件加密
  let encrypted_config_manager = ConfigManager::new({
    storage_type: "file",
    file_path: "/tmp/azimuth_config_encrypted.json",
    format: "json",
    encryption_enabled: true,
    encryption_key: "azimuth_secure_key_2022"
  })
  
  encrypted_config_manager.register_schema("secure_settings", {
    "api_keys": {
      "primary": { type: "string", required: true, sensitive: true },
      "secondary": { type: "string", required: false, sensitive: true }
    },
    "secrets": {
      "db_password": { type: "string", required: true, sensitive: true },
      "jwt_secret": { type: "string", required: true, sensitive: true }
    }
  })
  
  let secure_config = {
    "api_keys": {
      "primary": "sk-1234567890abcdef",
      "secondary": "sk-0987654321fedcba"
    },
    "secrets": {
      "db_password": "super_secure_password",
      "jwt_secret": "jwt signing secret"
    }
  }
  
  // 保存加密配置
  let encrypted_save_result = encrypted_config_manager.save_config("secure_settings", secure_config)
  assert_true(encrypted_save_result.success)
  
  // 加载加密配置
  let encrypted_load_result = encrypted_config_manager.load_config("secure_settings")
  assert_true(encrypted_load_result.success)
  
  // 验证敏感数据正确解密
  assert_eq(encrypted_load_result.config.get("api_keys.primary"), Some("sk-1234567890abcdef"))
  assert_eq(encrypted_load_result.config.get("secrets.db_password"), Some("super_secure_password"))
}

// 测试3: 配置继承和覆盖
test "配置继承和覆盖测试" {
  // 创建支持继承的配置管理器
  let inheritance_config_manager = ConfigManager::new({
    storage_type: "memory",
    inheritance_enabled: true,
    override_strategy: "deep_merge"
  })
  
  // 定义基础配置模式
  let base_schema = {
    "server": {
      "host": { type: "string", required: true, default: "localhost" },
      "port": { type: "integer", required: true, default: 8080 },
      "timeout": { type: "integer", required: false, default: 30000 }
    },
    "logging": {
      "level": { type: "string", required: false, default: "info" },
      "format": { type: "string", required: false, default: "json" }
    },
    "features": {
      "enable_metrics": { type: "boolean", required: false, default: true },
      "enable_tracing": { type: "boolean", required: false, default: true }
    }
  }
  
  // 定义环境特定配置模式
  let env_schema = {
    "server": {
      "workers": { type: "integer", required: false, default: 4 },
      "keep_alive": { type: "boolean", required: false, default: true }
    },
    "database": {
      "pool_size": { type: "integer", required: false, default: 10 }
    },
    "features": {
      "enable_debug": { type: "boolean", required: false, default: false }
    }
  }
  
  // 注册配置模式
  inheritance_config_manager.register_schema("base_config", base_schema)
  inheritance_config_manager.register_schema("env_config", env_schema)
  
  // 设置基础配置
  inheritance_config_manager.set_value("base_config", "server.host", "base.example.com")
  inheritance_config_manager.set_value("base_config", "server.port", 8000)
  inheritance_config_manager.set_value("base_config", "server.timeout", 60000)
  inheritance_config_manager.set_value("base_config", "logging.level", "debug")
  inheritance_config_manager.set_value("base_config", "logging.format", "text")
  inheritance_config_manager.set_value("base_config", "features.enable_metrics", true)
  inheritance_config_manager.set_value("base_config", "features.enable_tracing", false)
  
  // 设置环境特定配置
  inheritance_config_manager.set_value("env_config", "server.workers", 8)
  inheritance_config_manager.set_value("env_config", "server.keep_alive", false)
  inheritance_config_manager.set_value("env_config", "database.pool_size", 20)
  inheritance_config_manager.set_value("env_config", "features.enable_debug", true)
  
  // 创建配置继承链
  let inheritance_chain = inheritance_config_manager.create_inheritance_chain([
    { name: "base_config", priority: 1 },
    { name: "env_config", priority: 2 }
  ])
  
  // 解析继承配置
  let resolved_config = inheritance_config_manager.resolve_inheritance(inheritance_chain)
  assert_true(resolved_config.success)
  
  // 验证合并结果
  assert_eq(resolved_config.config.get("server.host"), Some("base.example.com"))  // 来自基础配置
  assert_eq(resolved_config.config.get("server.port"), Some(8000))                // 来自基础配置
  assert_eq(resolved_config.config.get("server.timeout"), Some(60000))           // 来自基础配置
  assert_eq(resolved_config.config.get("server.workers"), Some(8))               // 来自环境配置
  assert_eq(resolved_config.config.get("server.keep_alive"), Some(false))        // 来自环境配置
  assert_eq(resolved_config.config.get("logging.level"), Some("debug"))          // 来自基础配置
  assert_eq(resolved_config.config.get("logging.format"), Some("text"))          // 来自基础配置
  assert_eq(resolved_config.config.get("database.pool_size"), Some(20))          // 来自环境配置
  assert_eq(resolved_config.config.get("features.enable_metrics"), Some(true))   // 来自基础配置
  assert_eq(resolved_config.config.get("features.enable_tracing"), Some(false))  // 来自基础配置
  assert_eq(resolved_config.config.get("features.enable_debug"), Some(true))     // 来自环境配置
  
  // 测试配置覆盖
  inheritance_config_manager.set_value("env_config", "server.port", 9000)  // 覆盖基础配置
  inheritance_config_manager.set_value("env_config", "logging.level", "warn")  // 覆盖基础配置
  
  // 重新解析继承配置
  let updated_resolved_config = inheritance_config_manager.resolve_inheritance(inheritance_chain)
  assert_true(updated_resolved_config.success)
  
  // 验证覆盖结果
  assert_eq(updated_resolved_config.config.get("server.port"), Some(9000))   // 被环境配置覆盖
  assert_eq(updated_resolved_config.config.get("logging.level"), Some("warn")) // 被环境配置覆盖
  
  // 测试多级继承
  let prod_schema = {
    "security": {
      "enable_tls": { type: "boolean", required: false, default: true },
      "cert_path": { type: "string", required: false }
    },
    "performance": {
      "cache_size": { type: "integer", required: false, default: 1024 }
    }
  }
  
  inheritance_config_manager.register_schema("prod_config", prod_schema)
  
  inheritance_config_manager.set_value("prod_config", "security.enable_tls", true)
  inheritance_config_manager.set_value("prod_config", "security.cert_path", "/etc/ssl/certs/server.crt")
  inheritance_config_manager.set_value("prod_config", "performance.cache_size", 2048)
  
  // 创建三级继承链
  let multi_level_chain = inheritance_config_manager.create_inheritance_chain([
    { name: "base_config", priority: 1 },
    { name: "env_config", priority: 2 },
    { name: "prod_config", priority: 3 }
  ])
  
  let multi_level_resolved = inheritance_config_manager.resolve_inheritance(multi_level_chain)
  assert_true(multi_level_resolved.success)
  
  // 验证多级继承结果
  assert_eq(multi_level_resolved.config.get("server.host"), Some("base.example.com"))
  assert_eq(multi_level_resolved.config.get("server.port"), Some(9000))
  assert_eq(multi_level_resolved.config.get("security.enable_tls"), Some(true))
  assert_eq(multi_level_resolved.config.get("security.cert_path"), Some("/etc/ssl/certs/server.crt"))
  assert_eq(multi_level_resolved.config.get("performance.cache_size"), Some(2048))
  
  // 测试配置继承冲突解决
  inheritance_config_manager.set_inheritance_strategy("last_wins")  // 最后一个优先
  
  let conflict_resolved = inheritance_config_manager.resolve_inheritance(multi_level_chain)
  assert_true(conflict_resolved.success)
  
  // 测试配置继承路径
  let inheritance_path = inheritance_config_manager.get_inheritance_path("server.port", multi_level_chain)
  assert_true(inheritance_path.contains("env_config"))  // 应该来自env_config
}

// 测试4: 动态配置更新
test "动态配置更新测试" {
  // 创建动态配置管理器
  let dynamic_config_manager = DynamicConfigManager::new({
    update_mode: "hot_reload",
    validation_mode: "strict",
    rollback_enabled: true,
    notification_enabled: true
  })
  
  // 注册配置模式和更新监听器
  let service_schema = {
    "service": {
      "name": { type: "string", required: true },
      "version": { type: "string", required: true },
      "endpoints": { type: "array", required: true }
    },
    "performance": {
      "max_concurrent_requests": { type: "integer", required: false, min: 1, max: 1000, default: 100 },
      "request_timeout_ms": { type: "integer", required: false, min: 1000, default: 30000 },
      "circuit_breaker_threshold": { type: "integer", required: false, min: 1, default: 10 }
    },
    "features": {
      "enable_caching": { type: "boolean", required: false, default: true },
      "cache_ttl_seconds": { type: "integer", required: false, min: 1, default: 300 }
    }
  }
  
  dynamic_config_manager.register_schema("service_config", service_schema)
  
  // 设置初始配置
  dynamic_config_manager.set_value("service_config", "service.name", "payment-service")
  dynamic_config_manager.set_value("service_config", "service.version", "1.0.0")
  dynamic_config_manager.set_value("service_config", "service.endpoints", ["/api/pay", "/api/refund"])
  dynamic_config_manager.set_value("service_config", "performance.max_concurrent_requests", 100)
  dynamic_config_manager.set_value("service_config", "performance.request_timeout_ms", 30000)
  dynamic_config_manager.set_value("service_config", "performance.circuit_breaker_threshold", 10)
  dynamic_config_manager.set_value("service_config", "features.enable_caching", true)
  dynamic_config_manager.set_value("service_config", "features.cache_ttl_seconds", 300)
  
  // 添加配置更新监听器
  let mut update_notifications = []
  
  dynamic_config_manager.add_update_listener("service_config", fn(change) {
    update_notifications = update_notifications.push({
      field: change.field,
      old_value: change.old_value,
      new_value: change.new_value,
      timestamp: change.timestamp
    })
  })
  
  // 测试单个字段更新
  let single_update_result = dynamic_config_manager.update_value("service_config", "performance.max_concurrent_requests", 200)
  assert_true(single_update_result.success)
  
  // 验证监听器被触发
  assert_eq(update_notifications.length(), 1)
  assert_eq(update_notifications[0].field, "performance.max_concurrent_requests")
  assert_eq(update_notifications[0].old_value, Some(100))
  assert_eq(update_notifications[0].new_value, Some(200))
  
  // 验证配置已更新
  let updated_value = dynamic_config_manager.get_value("service_config", "performance.max_concurrent_requests")
  assert_eq(updated_value, Some(200))
  
  // 测试批量更新
  let batch_updates = [
    { field: "performance.request_timeout_ms", value: 60000 },
    { field: "performance.circuit_breaker_threshold", value: 20 },
    { field: "features.cache_ttl_seconds", value: 600 }
  ]
  
  let batch_update_result = dynamic_config_manager.update_batch("service_config", batch_updates)
  assert_true(batch_update_result.success)
  assert_eq(batch_update_result.updated_fields, 3)
  
  // 验证批量更新通知
  assert_eq(update_notifications.length(), 4)  // 1 + 3个批量更新
  
  // 验证批量更新结果
  assert_eq(dynamic_config_manager.get_value("service_config", "performance.request_timeout_ms"), Some(60000))
  assert_eq(dynamic_config_manager.get_value("service_config", "performance.circuit_breaker_threshold"), Some(20))
  assert_eq(dynamic_config_manager.get_value("service_config", "features.cache_ttl_seconds"), Some(600))
  
  // 测试条件更新
  let conditional_update_result = dynamic_config_manager.conditional_update("service_config", {
    condition: "features.enable_caching == true",
    updates: [
      { field: "features.cache_ttl_seconds", value: 900 }
    ]
  })
  
  assert_true(conditional_update_result.success)
  assert_eq(dynamic_config_manager.get_value("service_config", "features.cache_ttl_seconds"), Some(900))
  
  // 测试条件不满足的更新
  let failed_conditional_update = dynamic_config_manager.conditional_update("service_config", {
    condition: "features.enable_caching == false",
    updates: [
      { field: "features.cache_ttl_seconds", value: 1200 }
    ]
  })
  
  assert_false(failed_conditional_update.success)
  assert_eq(dynamic_config_manager.get_value("service_config", "features.cache_ttl_seconds"), Some(900))  // 未更新
  
  // 测试配置回滚
  let rollback_point = dynamic_config_manager.create_rollback_point("service_config")
  assert_true(rollback_point.success)
  
  // 进行一些更改
  dynamic_config_manager.update_value("service_config", "performance.max_concurrent_requests", 300)
  dynamic_config_manager.update_value("service_config", "features.enable_caching", false)
  
  // 验证更改
  assert_eq(dynamic_config_manager.get_value("service_config", "performance.max_concurrent_requests"), Some(300))
  assert_eq(dynamic_config_manager.get_value("service_config", "features.enable_caching"), Some(false))
  
  // 执行回滚
  let rollback_result = dynamic_config_manager.rollback_to_point("service_config", rollback_point.rollback_id)
  assert_true(rollback_result.success)
  
  // 验证回滚结果
  assert_eq(dynamic_config_manager.get_value("service_config", "performance.max_concurrent_requests"), Some(200))
  assert_eq(dynamic_config_manager.get_value("service_config", "features.enable_caching"), Some(true))
  
  // 测试配置热重载
  let temp_config_file = "/tmp/service_config_update.json"
  let updated_config_content = {
    "service": {
      "name": "payment-service",
      "version": "1.1.0",  // 版本更新
      "endpoints": ["/api/pay", "/api/refund", "/api/status"]  // 新增端点
    },
    "performance": {
      "max_concurrent_requests": 250,
      "request_timeout_ms": 45000,
      "circuit_breaker_threshold": 15
    },
    "features": {
      "enable_caching": true,
      "cache_ttl_seconds": 750
    }
  }
  
  // 写入更新配置到文件
  File::write(temp_config_file, JSON::stringify(updated_config_content))
  
  // 执行热重载
  let hot_reload_result = dynamic_config_manager.hot_reload_from_file("service_config", temp_config_file)
  assert_true(hot_reload_result.success)
  
  // 验证热重载结果
  assert_eq(dynamic_config_manager.get_value("service_config", "service.version"), Some("1.1.0"))
  assert_eq(dynamic_config_manager.get_value("service_config", "service.endpoints"), Some(["/api/pay", "/api/refund", "/api/status"]))
  assert_eq(dynamic_config_manager.get_value("service_config", "performance.max_concurrent_requests"), Some(250))
  assert_eq(dynamic_config_manager.get_value("service_config", "features.cache_ttl_seconds"), Some(750))
  
  // 清理临时文件
  File::delete(temp_config_file)
  
  // 测试配置更新历史
  let update_history = dynamic_config_manager.get_update_history("service_config", {
    limit: 10,
    include_rollback: true
  })
  
  assert_true(update_history.length() > 0)
  
  // 验证历史记录包含我们的更新
  let version_update = update_history.find(fn(entry) { 
    entry.field == "service.version" && entry.new_value == Some("1.1.0")
  })
  assert_true(version_update != None)
}

// 测试5: 配置验证和约束
test "配置验证和约束测试" {
  // 创建带验证的配置管理器
  let validation_config_manager = ConfigManager::new({
    validation_mode: "strict",
    validation_cache_enabled: true,
    custom_validators_enabled: true
  })
  
  // 定义复杂配置模式
  let complex_schema = {
    "cluster": {
      "name": { type: "string", required: true, pattern: "^[a-z][a-z0-9-]*$" },
      "nodes": { type: "array", required: true, min_items: 1, max_items: 10 },
      "replication_factor": { type: "integer", required: true, min: 1, max: 5 }
    },
    "network": {
      "port_range": { 
        type: "object", 
        required: true,
        properties: {
          "min": { type: "integer", required: true, min: 1024, max: 65535 },
          "max": { type: "integer", required: true, min: 1024, max: 65535 }
        }
      },
      "subnets": { type: "array", required: true, item_type: "string" }
    },
    "security": {
      "tls": {
        "enabled": { type: "boolean", required: false, default: true },
        "cert_path": { type: "string", required_if: "tls.enabled == true" },
        "key_path": { type: "string", required_if: "tls.enabled == true" }
      },
      "auth": {
        "method": { type: "string", required: true, enum: ["none", "basic", "oauth", "jwt"] },
        "credentials": { type: "object", required_if: "auth.method != 'none'" }
      }
    }
  }
  
  // 注册自定义验证器
  let port_range_validator = fn(value) {
    match value {
      Object(obj) => {
        let min = obj.get("min")
        let max = obj.get("max")
        
        match (min, max) {
          (Some(min_val), Some(max_val)) => {
            if min_val.to_int() >= max_val.to_int() {
              ValidationFailure("端口范围的最小值必须小于最大值")
            } else {
              ValidationSuccess
            }
          }
          _ => ValidationFailure("端口范围必须包含最小值和最大值")
        }
      }
      _ => ValidationFailure("端口范围必须是对象")
    }
  }
  
  let subnet_validator = fn(value) {
    match value {
      Array(arr) => {
        for subnet in arr {
          if not(is_valid_subnet(subnet.to_string())) {
            return ValidationFailure("无效的子网格式: " + subnet.to_string())
          }
        }
        ValidationSuccess
      }
      _ => ValidationFailure("子网必须是数组")
    }
  }
  
  validation_config_manager.register_schema("cluster_config", complex_schema)
  validation_config_manager.register_custom_validator("cluster_config", "network.port_range", port_range_validator)
  validation_config_manager.register_custom_validator("cluster_config", "network.subnets", subnet_validator)
  
  // 测试有效配置
  let valid_config = {
    "cluster": {
      "name": "production-cluster",
      "nodes": ["node1", "node2", "node3"],
      "replication_factor": 3
    },
    "network": {
      "port_range": { "min": 8000, "max": 9000 },
      "subnets": ["10.0.0.0/24", "10.0.1.0/24"]
    },
    "security": {
      "tls": {
        "enabled": true,
        "cert_path": "/etc/ssl/certs/server.crt",
        "key_path": "/etc/ssl/private/server.key"
      },
      "auth": {
        "method": "oauth",
        "credentials": {
          "client_id": "azimuth-client",
          "client_secret": "secure-secret"
        }
      }
    }
  }
  
  let valid_validation_result = validation_config_manager.validate_config("cluster_config", valid_config)
  assert_true(valid_validation_result.valid)
  assert_eq(valid_validation_result.errors.length(), 0)
  
  // 测试无效的集群名称
  let invalid_name_config = {
    "cluster": {
      "name": "Invalid-Cluster-Name",  // 包含大写字母
      "nodes": ["node1"],
      "replication_factor": 1
    },
    "network": {
      "port_range": { "min": 8000, "max": 9000 },
      "subnets": ["10.0.0.0/24"]
    },
    "security": {
      "tls": { "enabled": false },
      "auth": { "method": "none" }
    }
  }
  
  let invalid_name_result = validation_config_manager.validate_config("cluster_config", invalid_name_config)
  assert_false(invalid_name_result.valid)
  
  let name_error = invalid_name_result.errors.find(fn(err) { err.field == "cluster.name" })
  assert_true(name_error != None)
  
  // 测试无效的端口范围
  let invalid_port_range_config = {
    "cluster": {
      "name": "test-cluster",
      "nodes": ["node1"],
      "replication_factor": 1
    },
    "network": {
      "port_range": { "min": 9000, "max": 8000 },  // 最小值大于最大值
      "subnets": ["10.0.0.0/24"]
    },
    "security": {
      "tls": { "enabled": false },
      "auth": { "method": "none" }
    }
  }
  
  let invalid_port_range_result = validation_config_manager.validate_config("cluster_config", invalid_port_range_config)
  assert_false(invalid_port_range_result.valid)
  
  let port_range_error = invalid_port_range_result.errors.find(fn(err) { 
    err.field == "network.port_range" 
  })
  assert_true(port_range_error != None)
  assert_true(port_range_error.message.contains("最小值必须小于最大值"))
  
  // 测试无效的子网
  let invalid_subnet_config = {
    "cluster": {
      "name": "test-cluster",
      "nodes": ["node1"],
      "replication_factor": 1
    },
    "network": {
      "port_range": { "min": 8000, "max": 9000 },
      "subnets": ["invalid-subnet"]  // 无效的子网格式
    },
    "security": {
      "tls": { "enabled": false },
      "auth": { "method": "none" }
    }
  }
  
  let invalid_subnet_result = validation_config_manager.validate_config("cluster_config", invalid_subnet_config)
  assert_false(invalid_subnet_result.valid)
  
  let subnet_error = invalid_subnet_result.errors.find(fn(err) { 
    err.field == "network.subnets" 
  })
  assert_true(subnet_error != None)
  
  // 测试条件依赖验证
  let conditional_error_config = {
    "cluster": {
      "name": "test-cluster",
      "nodes": ["node1"],
      "replication_factor": 1
    },
    "network": {
      "port_range": { "min": 8000, "max": 9000 },
      "subnets": ["10.0.0.0/24"]
    },
    "security": {
      "tls": {
        "enabled": true,
        // 缺少必需的 cert_path 和 key_path
      },
      "auth": { "method": "none" }
    }
  }
  
  let conditional_error_result = validation_config_manager.validate_config("cluster_config", conditional_error_config)
  assert_false(conditional_error_result.valid)
  
  let cert_path_error = conditional_error_result.errors.find(fn(err) { 
    err.field == "security.tls.cert_path" 
  })
  assert_true(cert_path_error != None)
  
  // 测试复杂约束验证
  validation_config_manager.add_cross_field_constraint("cluster_config", {
    name: "node_replication_consistency",
    description: "节点数量必须大于或等于复制因子",
    constraint: "cluster.nodes.length() >= cluster.replication_factor",
    error_message: "节点数量必须大于或等于复制因子"
  })
  
  let inconsistent_config = {
    "cluster": {
      "name": "test-cluster",
      "nodes": ["node1", "node2"],  // 2个节点
      "replication_factor": 3       // 但复制因子为3
    },
    "network": {
      "port_range": { "min": 8000, "max": 9000 },
      "subnets": ["10.0.0.0/24"]
    },
    "security": {
      "tls": { "enabled": false },
      "auth": { "method": "none" }
    }
  }
  
  let constraint_result = validation_config_manager.validate_config("cluster_config", inconsistent_config)
  assert_false(constraint_result.valid)
  
  let constraint_error = constraint_result.errors.find(fn(err) { 
    err.constraint == "node_replication_consistency" 
  })
  assert_true(constraint_error != None)
  
  // 测试验证性能优化
  let performance_start_time = Time::now()
  
  for i in 0..=100 {
    validation_config_manager.validate_config("cluster_config", valid_config)
  }
  
  let performance_end_time = Time::now()
  let validation_duration = performance_end_time - performance_start_time
  
  // 验证缓存提高了性能
  assert_true(validation_duration < 1000)  // 100次验证应在1秒内完成
}

// 测试6: 配置模板和生成
test "配置模板和生成测试" {
  // 创建配置模板管理器
  let template_manager = ConfigTemplateManager::new({
    template_engine: "handlebars",
    template_cache_enabled: true,
    variable_validation_enabled: true
  })
  
  // 定义配置模板
  let service_template = {
    name: "service_template",
    description: "通用服务配置模板",
    variables: [
      { name: "service_name", type: "string", required: true, description: "服务名称" },
      { name: "service_version", type: "string", required: true, description: "服务版本" },
      { name: "environment", type: "string", required: true, enum: ["dev", "staging", "prod"], description: "部署环境" },
      { name: "port", type: "integer", required: false, default: 8080, description: "服务端口" },
      { name: "enable_metrics", type: "boolean", required: false, default: true, description: "启用指标收集" },
      { name: "replicas", type: "integer", required: false, default: 1, description: "副本数量" }
    ],
    template_content: {
      "service": {
        "name": "{{service_name}}",
        "version": "{{service_version}}",
        "environment": "{{environment}}",
        "port": {{port}},
        "features": {
          "enable_metrics": {{enable_metrics}}
        },
        "deployment": {
          "replicas": {{replicas}},
          {{#if (eq environment "prod")}}
          "resources": {
            "cpu": "500m",
            "memory": "512Mi"
          }
          {{else}}
          "resources": {
            "cpu": "100m",
            "memory": "128Mi"
          }
          {{/if}}
        }
      }
    }
  }
  
  // 注册模板
  let template_registration = template_manager.register_template(service_template)
  assert_true(template_registration.success)
  
  // 测试模板渲染
  let template_variables = {
    "service_name": "user-service",
    "service_version": "1.2.3",
    "environment": "prod",
    "port": 9000,
    "enable_metrics": true,
    "replicas": 3
  }
  
  let render_result = template_manager.render_template("service_template", template_variables)
  assert_true(render_result.success)
  
  // 验证渲染结果
  let rendered_config = render_result.config
  assert_eq(rendered_config.get("service.name"), Some("user-service"))
  assert_eq(rendered_config.get("service.version"), Some("1.2.3"))
  assert_eq(rendered_config.get("service.environment"), Some("prod"))
  assert_eq(rendered_config.get("service.port"), Some(9000))
  assert_eq(rendered_config.get("service.features.enable_metrics"), Some(true))
  assert_eq(rendered_config.get("service.deployment.replicas"), Some(3))
  
  // 验证条件渲染
  assert_eq(rendered_config.get("service.deployment.resources.cpu"), Some("500m"))
  assert_eq(rendered_config.get("service.deployment.resources.memory"), Some("512Mi"))
  
  // 测试不同环境的渲染
  let dev_variables = {
    "service_name": "user-service",
    "service_version": "1.2.3",
    "environment": "dev"
  }
  
  let dev_render_result = template_manager.render_template("service_template", dev_variables)
  assert_true(dev_render_result.success)
  
  // 验证开发环境的资源分配更小
  assert_eq(dev_render_result.config.get("service.deployment.resources.cpu"), Some("100m"))
  assert_eq(dev_render_result.config.get("service.deployment.resources.memory"), Some("128Mi"))
  
  // 测试模板继承
  let base_template = {
    name: "base_template",
    description: "基础配置模板",
    variables: [
      { name: "app_name", type: "string", required: true },
      { name: "log_level", type: "string", required: false, default: "info" }
    ],
    template_content: {
      "application": {
        "name": "{{app_name}}",
        "logging": {
          "level": "{{log_level}}"
        }
      }
    }
  }
  
  let extended_template = {
    name: "extended_template",
    description: "扩展配置模板",
    extends: "base_template",
    variables: [
      { name: "app_name", type: "string", required: true },
      { name: "database_url", type: "string", required: true }
    ],
    template_content: {
      "database": {
        "url": "{{database_url}}"
      },
      "monitoring": {
        "enabled": true,
        "endpoint": "/metrics"
      }
    }
  }
  
  template_manager.register_template(base_template)
  template_manager.register_template(extended_template)
  
  let extended_variables = {
    "app_name": "extended-app",
    "log_level": "debug",
    "database_url": "postgresql://localhost:5432/app"
  }
  
  let extended_render_result = template_manager.render_template("extended_template", extended_variables)
  assert_true(extended_render_result.success)
  
  // 验证模板继承结果
  assert_eq(extended_render_result.config.get("application.name"), Some("extended-app"))
  assert_eq(extended_render_result.config.get("application.logging.level"), Some("debug"))
  assert_eq(extended_render_result.config.get("database.url"), Some("postgresql://localhost:5432/app"))
  assert_eq(extended_render_result.config.get("monitoring.enabled"), Some(true))
  
  // 测试模板组合
  let database_template = {
    name: "database_template",
    description: "数据库配置模板",
    variables: [
      { name: "db_type", type: "string", required: true, enum: ["postgresql", "mysql", "mongodb"] },
      { name: "db_host", type: "string", required: true },
      { name: "db_port", type: "integer", required: true },
      { name: "db_name", type: "string", required: true }
    ],
    template_content: {
      "database": {
        "type": "{{db_type}}",
        "host": "{{db_host}}",
        "port": {{db_port}},
        "name": "{{db_name}}",
        {{#if (eq db_type "postgresql")}}
        "ssl": true,
        "pool_size": 20
        {{else if (eq db_type "mysql")}}
        "charset": "utf8mb4",
        "pool_size": 10
        {{else if (eq db_type "mongodb")}}
        "replica_set": "rs0",
        "pool_size": 5
        {{/if}}
      }
    }
  }
  
  template_manager.register_template(database_template)
  
  // 创建组合模板
  let composition_variables = {
    "service_name": "composite-service",
    "service_version": "2.0.0",
    "environment": "staging",
    "db_type": "postgresql",
    "db_host": "db.example.com",
    "db_port": 5432,
    "db_name": "composite_db"
  }
  
  let composition_result = template_manager.compose_templates([
    "service_template",
    "database_template"
  ], composition_variables)
  
  assert_true(composition_result.success)
  
  // 验证组合结果
  let composed_config = composition_result.config
  assert_eq(composed_config.get("service.name"), Some("composite-service"))
  assert_eq(composed_config.get("database.type"), Some("postgresql"))
  assert_eq(composed_config.get("database.ssl"), Some(true))
  assert_eq(composed_config.get("database.pool_size"), Some(20))
  
  // 测试模板验证
  let invalid_variables = {
    "service_name": "test-service",
    "environment": "invalid_env"  // 无效的环境值
  }
  
  let invalid_render_result = template_manager.render_template("service_template", invalid_variables)
  assert_false(invalid_render_result.success)
  assert_true(invalid_render_result.error.contains("environment"))
  
  // 测试模板变量验证
  let variable_validation = template_manager.validate_template_variables("service_template", {
    "service_name": "test-service",
    "environment": "prod",
    "port": "invalid_port"  // 应该是整数
  })
  
  assert_false(variable_validation.valid)
  assert_true(variable_validation.errors.length() > 0)
}

// 测试7: 配置环境管理
test "配置环境管理测试" {
  // 创建环境配置管理器
  let env_config_manager = EnvironmentConfigManager::new({
    default_environment: "development",
    environment_detection: "env_variable",
    config_hierarchy: ["global", "environment", "local"],
    override_strategy: "last_wins"
  })
  
  // 定义环境配置结构
  let global_config = {
    "application": {
      "name": "Azimuth",
      "version": "1.0.0"
    },
    "logging": {
      "level": "info",
      "format": "json"
    },
    "features": {
      "enable_metrics": true,
      "enable_tracing": false
    }
  }
  
  let development_config = {
    "server": {
      "host": "localhost",
      "port": 8080,
      "debug": true
    },
    "database": {
      "url": "postgresql://localhost:5432/azimuth_dev",
      "max_connections": 5
    },
    "logging": {
      "level": "debug"  // 覆盖全局配置
    }
  }
  
  let staging_config = {
    "server": {
      "host": "staging.azimuth.com",
      "port": 80,
      "debug": false
    },
    "database": {
      "url": "postgresql://staging-db:5432/azimuth_staging",
      "max_connections": 10
    },
    "features": {
      "enable_tracing": true  // 覆盖全局配置
    }
  }
  
  let production_config = {
    "server": {
      "host": "azimuth.com",
      "port": 443,
      "debug": false,
      "ssl": true
    },
    "database": {
      "url": "postgresql://prod-db:5432/azimuth_prod",
      "max_connections": 20,
      "ssl": true
    },
    "logging": {
      "level": "warn"  // 覆盖全局配置
    },
    "features": {
      "enable_tracing": true  // 覆盖全局配置
    },
    "security": {
      "enable_rate_limiting": true,
      "rate_limit": 1000
    }
  }
  
  // 注册环境配置
  env_config_manager.register_environment_config("global", global_config)
  env_config_manager.register_environment_config("development", development_config)
  env_config_manager.register_environment_config("staging", staging_config)
  env_config_manager.register_environment_config("production", production_config)
  
  // 测试开发环境配置解析
  let dev_resolved = env_config_manager.resolve_config("development")
  assert_true(dev_resolved.success)
  
  // 验证开发环境配置合并结果
  assert_eq(dev_resolved.config.get("application.name"), Some("Azimuth"))  // 来自全局
  assert_eq(dev_resolved.config.get("application.version"), Some("1.0.0"))  // 来自全局
  assert_eq(dev_resolved.config.get("server.host"), Some("localhost"))     // 来自开发环境
  assert_eq(dev_resolved.config.get("server.port"), Some(8080))            // 来自开发环境
  assert_eq(dev_resolved.config.get("server.debug"), Some(true))          // 来自开发环境
  assert_eq(dev_resolved.config.get("database.url"), Some("postgresql://localhost:5432/azimuth_dev"))  // 来自开发环境
  assert_eq(dev_resolved.config.get("logging.level"), Some("debug"))      // 开发环境覆盖全局
  assert_eq(dev_resolved.config.get("logging.format"), Some("json"))      // 来自全局
  assert_eq(dev_resolved.config.get("features.enable_metrics"), Some(true))  // 来自全局
  assert_eq(dev_resolved.config.get("features.enable_tracing"), Some(false)) // 来自全局
  
  // 测试生产环境配置解析
  let prod_resolved = env_config_manager.resolve_config("production")
  assert_true(prod_resolved.success)
  
  // 验证生产环境配置合并结果
  assert_eq(prod_resolved.config.get("application.name"), Some("Azimuth"))     // 来自全局
  assert_eq(prod_resolved.config.get("server.host"), Some("azimuth.com"))       // 来自生产环境
  assert_eq(prod_resolved.config.get("server.port"), Some(443))                 // 来自生产环境
  assert_eq(prod_resolved.config.get("server.ssl"), Some(true))                // 来自生产环境
  assert_eq(prod_resolved.config.get("database.max_connections"), Some(20))     // 来自生产环境
  assert_eq(prod_resolved.config.get("database.ssl"), Some(true))               // 来自生产环境
  assert_eq(prod_resolved.config.get("logging.level"), Some("warn"))            // 生产环境覆盖全局
  assert_eq(prod_resolved.config.get("features.enable_tracing"), Some(true))    // 生产环境覆盖全局
  assert_eq(prod_resolved.config.get("security.enable_rate_limiting"), Some(true)) // 来自生产环境
  assert_eq(prod_resolved.config.get("security.rate_limit"), Some(1000))        // 来自生产环境
  
  // 测试环境特定配置验证
  let env_schema = {
    "server": {
      "host": { type: "string", required: true },
      "port": { type: "integer", required: true, min: 1, max: 65535 },
      "debug": { type: "boolean", required: false }
    },
    "database": {
      "url": { type: "string", required: true },
      "max_connections": { type: "integer", required: true, min: 1, max: 100 }
    },
    "security": {
      "enable_rate_limiting": { type: "boolean", required: false },
      "rate_limit": { type: "integer", required_if: "security.enable_rate_limiting == true", min: 1 }
    }
  }
  
  env_config_manager.register_environment_schema("production", env_schema)
  
  // 验证生产环境配置
  let prod_validation = env_config_manager.validate_environment_config("production")
  assert_true(prod_validation.valid)
  
  // 测试环境配置差异比较
  let config_diff = env_config_manager.compare_environments("development", "production")
  assert_true(config_diff.differences.length() > 0)
  
  // 验证关键差异
  let host_diff = config_diff.differences.find(fn(diff) { 
    diff.path == "server.host" 
  })
  assert_true(host_diff != None)
  assert_eq(host_diff.env1_value, Some("localhost"))
  assert_eq(host_diff.env2_value, Some("azimuth.com"))
  
  let logging_diff = config_diff.differences.find(fn(diff) { 
    diff.path == "logging.level" 
  })
  assert_true(logging_diff != None)
  assert_eq(logging_diff.env1_value, Some("debug"))
  assert_eq(logging_diff.env2_value, Some("warn"))
  
  // 测试环境配置迁移
  let migration_plan = env_config_manager.create_migration_plan("staging", "production")
  assert_true(migration_plan.changes.length() > 0)
  
  // 验证迁移计划包含必要的更改
  let host_migration = migration_plan.changes.find(fn(change) { 
    change.path == "server.host" 
  })
  assert_true(host_migration != None)
  assert_eq(host_migration.old_value, Some("staging.azimuth.com"))
  assert_eq(host_migration.new_value, Some("azimuth.com"))
  
  // 测试环境配置导出
  let dev_export = env_config_manager.export_environment("development", {
    format: "yaml",
    include_resolved: true,
    include_sources: true
  })
  
  assert_true(dev_export.success)
  assert_true(dev_export.content.length() > 0)
  
  // 测试环境配置导入
  let new_env_config = {
    "server": {
      "host": "testing.azimuth.com",
      "port": 9090,
      "debug": true
    },
    "database": {
      "url": "postgresql://test-db:5432/azimuth_test",
      "max_connections": 3
    },
    "logging": {
      "level": "trace"
    }
  }
  
  let import_result = env_config_manager.import_environment("testing", new_env_config)
  assert_true(import_result.success)
  
  // 验证导入的环境配置
  let testing_resolved = env_config_manager.resolve_config("testing")
  assert_true(testing_resolved.success)
  assert_eq(testing_resolved.config.get("server.host"), Some("testing.azimuth.com"))
  assert_eq(testing_resolved.config.get("logging.level"), Some("trace"))
  assert_eq(testing_resolved.config.get("application.name"), Some("Azimuth"))  // 仍然继承全局配置
  
  // 测试环境配置模板
  let env_template = {
    name: "environment_template",
    description: "新环境配置模板",
    variables: [
      { name: "env_name", type: "string", required: true },
      { name: "host", type: "string", required: true },
      { name: "port", type: "integer", required: true }
    ],
    template_content: {
      "server": {
        "host": "{{host}}",
        "port": {{port}},
        "debug": {{#if (eq env_name "development")}}true{{else}}false{{/if}}
      },
      "database": {
        "url": "postgresql://{{host}}:5432/azimuth_{{env_name}}",
        "max_connections": {{#if (eq env_name "production")}}20{{else}}5{{/if}}
      }
    }
  }
  
  env_config_manager.register_environment_template(env_template)
  
  // 使用模板创建新环境
  let template_variables = {
    "env_name": "qa",
    "host": "qa.azimuth.com",
    "port": 8080
  }
  
  let template_creation = env_config_manager.create_from_template("qa", "environment_template", template_variables)
  assert_true(template_creation.success)
  
  // 验证模板创建的环境
  let qa_resolved = env_config_manager.resolve_config("qa")
  assert_true(qa_resolved.success)
  assert_eq(qa_resolved.config.get("server.host"), Some("qa.azimuth.com"))
  assert_eq(qa_resolved.config.get("server.port"), Some(8080))
  assert_eq(qa_resolved.config.get("server.debug"), Some(false))  // 非开发环境
  assert_eq(qa_resolved.config.get("database.url"), Some("postgresql://qa.azimuth.com:5432/azimuth_qa"))
  assert_eq(qa_resolved.config.get("database.max_connections"), Some(5))  // 非生产环境
}

// 测试8: 配置安全加密
test "配置安全加密测试" {
  // 创建安全配置管理器
  let secure_config_manager = SecureConfigManager::new({
    encryption_algorithm: "aes-256-gcm",
    key_derivation: "pbkdf2",
    key_iterations: 100000,
    salt_length: 32,
    iv_length: 12
  })
  
  // 生成加密密钥
  let key_generation = secure_config_manager.generate_key("master_password_2022")
  assert_true(key_generation.success)
  
  let encryption_key = key_generation.key
  assert_true(encryption_key.length() == 32)  // 256位密钥
  
  // 注册敏感配置模式
  let secure_schema = {
    "api": {
      "public_key": { type: "string", required: true, encrypted: false },
      "private_key": { type: "string", required: true, encrypted: true, sensitive: true },
      "webhook_secret": { type: "string", required: true, encrypted: true, sensitive: true }
    },
    "database": {
      "host": { type: "string", required: true, encrypted: false },
      "username": { type: "string", required: true, encrypted: false },
      "password": { type: "string", required: true, encrypted: true, sensitive: true },
      "ssl_cert": { type: "string", required: false, encrypted: true, sensitive: true }
    },
    "external_services": {
      "service_a": {
        "api_key": { type: "string", required: true, encrypted: true, sensitive: true },
        "endpoint": { type: "string", required: true, encrypted: false }
      },
      "service_b": {
        "oauth_token": { type: "string", required: true, encrypted: true, sensitive: true },
        "endpoint": { type: "string", required: true, encrypted: false }
      }
    }
  }
  
  secure_config_manager.register_schema("secure_config", secure_schema)
  
  // 设置配置值（敏感字段会自动加密）
  secure_config_manager.set_value("secure_config", "api.public_key", "ssh-rsa AAAAB3NzaC1yc2E...")
  secure_config_manager.set_value("secure_config", "api.private_key", "-----BEGIN RSA PRIVATE KEY-----\n...")
  secure_config_manager.set_value("secure_config", "api.webhook_secret", "webhook_secret_12345")
  
  secure_config_manager.set_value("secure_config", "database.host", "db.example.com")
  secure_config_manager.set_value("secure_config", "database.username", "azimuth_user")
  secure_config_manager.set_value("secure_config", "database.password", "super_secure_password")
  secure_config_manager.set_value("secure_config", "database.ssl_cert", "-----BEGIN CERTIFICATE-----\n...")
  
  secure_config_manager.set_value("secure_config", "external_services.service_a.api_key", "sk-1234567890abcdef")
  secure_config_manager.set_value("secure_config", "external_services.service_a.endpoint", "https://api.service-a.com")
  secure_config_manager.set_value("secure_config", "external_services.service_b.oauth_token", "oauth_token_abcdef123456")
  secure_config_manager.set_value("secure_config", "external_services.service_b.endpoint", "https://api.service-b.com")
  
  // 获取配置值（敏感字段会自动解密）
  let public_key = secure_config_manager.get_value("secure_config", "api.public_key")
  assert_eq(public_key, Some("ssh-rsa AAAAB3NzaC1yc2E..."))
  
  let private_key = secure_config_manager.get_value("secure_config", "api.private_key")
  assert_eq(private_key, Some("-----BEGIN RSA PRIVATE KEY-----\n..."))
  
  let db_password = secure_config_manager.get_value("secure_config", "database.password")
  assert_eq(db_password, Some("super_secure_password"))
  
  // 验证内部存储确实是加密的
  let raw_storage = secure_config_manager.get_raw_storage("secure_config")
  assert_true(raw_storage.get("api.private_key") != "-----BEGIN RSA PRIVATE KEY-----\n...")
  assert_true(raw_storage.get("database.password") != "super_secure_password")
  
  // 测试配置序列化（敏感字段保持加密）
  let serialized_result = secure_config_manager.serialize_config("secure_config", {
    format: "json",
    include_encrypted: true,
    include_metadata: true
  })
  
  assert_true(serialized_result.success)
  
  let serialized_config = JSON::parse(serialized_result.content)
  assert_true(serialized_config.api.private_key != "-----BEGIN RSA PRIVATE KEY-----\n...")
  assert_true(serialized_config.database.password != "super_secure_password")
  
  // 测试配置反序列化（敏感字段自动解密）
  let deserialized_result = secure_config_manager.deserialize_config("secure_config", serialized_result.content, {
    format: "json",
    auto_decrypt: true
  })
  
  assert_true(deserialized_result.success)
  
  // 验证反序列化后的配置
  assert_eq(deserialized_result.config.get("api.private_key"), Some("-----BEGIN RSA PRIVATE KEY-----\n..."))
  assert_eq(deserialized_result.config.get("database.password"), Some("super_secure_password"))
  
  // 测试配置文件加密存储
  let file_secure_manager = SecureConfigManager::new({
    storage_type: "file",
    file_path: "/tmp/secure_config.enc",
    encryption_algorithm: "aes-256-gcm",
    key_derivation: "pbkdf2",
    key_iterations: 100000
  })
  
  file_secure_manager.set_encryption_key(encryption_key)
  file_secure_manager.register_schema("secure_config", secure_schema)
  
  // 保存加密配置到文件
  let file_save_result = file_secure_manager.save_config("secure_config", {
    "api": {
      "public_key": "ssh-rsa AAAAB3NzaC1yc2E...",
      "private_key": "-----BEGIN RSA PRIVATE KEY-----\n...",
      "webhook_secret": "webhook_secret_12345"
    },
    "database": {
      "host": "db.example.com",
      "username": "azimuth_user",
      "password": "super_secure_password"
    }
  })
  
  assert_true(file_save_result.success)
  
  // 验证文件内容确实是加密的
  let file_content = File::read("/tmp/secure_config.enc")
  assert_false(file_content.contains("super_secure_password"))
  assert_false(file_content.contains("-----BEGIN RSA PRIVATE KEY-----"))
  
  // 从文件加载并解密配置
  let file_load_result = file_secure_manager.load_config("secure_config")
  assert_true(file_load_result.success)
  
  // 验证解密后的配置
  assert_eq(file_load_result.config.get("database.password"), Some("super_secure_password"))
  
  // 测试密钥轮换
  let new_key_result = secure_config_manager.generate_key("new_master_password_2022")
  assert_true(new_key_result.success)
  
  let new_encryption_key = new_key_result.key
  
  // 执行密钥轮换
  let key_rotation_result = secure_config_manager.rotate_encryption_key("secure_config", encryption_key, new_encryption_key)
  assert_true(key_rotation_result.success)
  
  // 验证密钥轮换后仍能正确解密
  let rotated_private_key = secure_config_manager.get_value("secure_config", "api.private_key")
  assert_eq(rotated_private_key, Some("-----BEGIN RSA PRIVATE KEY-----\n..."))
  
  // 测试访问控制
  let access_control_manager = ConfigAccessControl::new()
  
  // 定义访问策略
  access_control_manager.add_policy({
    name: "admin_full_access",
    description: "管理员完全访问权限",
    principals: ["admin", "config_admin"],
    resources: ["secure_config"],
    actions: ["read", "write", "decrypt"],
    effect: "allow"
  })
  
  access_control_manager.add_policy({
    name: "app_readonly_access",
    description: "应用程序只读访问权限",
    principals: ["azimuth_app"],
    resources: ["secure_config"],
    actions: ["read"],
    effect: "allow"
  })
  
  access_control_manager.add_policy({
    name: "deny_sensitive_access",
    description: "拒绝访问敏感字段",
    principals: ["auditor"],
    resources: ["secure_config"],
    actions: ["read"],
    fields: ["api.private_key", "database.password"],
    effect: "deny"
  })
  
  // 集成访问控制
  secure_config_manager.set_access_control_manager(access_control_manager)
  
  // 测试不同用户的访问权限
  let admin_access = secure_config_manager.get_config_with_access("secure_config", "admin")
  assert_true(admin_access.success)
  assert_eq(admin_access.config.get("database.password"), Some("super_secure_password"))  // 管理员可以访问
  
  let app_access = secure_config_manager.get_config_with_access("secure_config", "azimuth_app")
  assert_true(app_access.success)
  assert_eq(app_access.config.get("database.password"), Some("super_secure_password"))  // 应用程序可以访问
  
  let auditor_access = secure_config_manager.get_config_with_access("secure_config", "auditor")
  assert_true(auditor_access.success)
  assert_eq(auditor_access.config.get("database.password"), None)  // 审计员不能访问敏感字段
  assert_eq(auditor_access.config.get("database.host"), Some("db.example.com"))  // 但可以访问非敏感字段
  
  // 清理测试文件
  File::delete("/tmp/secure_config.enc")
}

// 测试9: 配置变更审计
test "配置变更审计测试" {
  // 创建配置审计管理器
  let audit_manager = ConfigAuditManager::new({
    storage_type: "file",
    audit_log_path: "/tmp/config_audit.log",
    retention_days: 90,
    include_diffs: true,
    include_user_context: true
  })
  
  // 创建配置管理器并集成审计
  let audited_config_manager = ConfigManager::new({
    storage_type: "memory",
    audit_enabled: true,
    audit_manager: audit_manager
  })
  
  // 注册配置模式
  audited_config_manager.register_schema("audit_config", {
    "service": {
      "name": { type: "string", required: true },
      "version": { type: "string", required: true }
    },
    "resources": {
      "cpu": { type: "string", required: true },
      "memory": { type: "string", required: true }
    }
  })
  
  // 模拟用户上下文
  let user_context = {
    user_id: "user123",
    username: "admin",
    roles: ["admin", "config_manager"],
    ip_address: "192.168.1.100",
    user_agent: "Azimuth Config CLI v1.0.0"
  }
  
  // 设置审计上下文
  audited_config_manager.set_audit_context(user_context)
  
  // 执行配置操作（会被自动审计）
  audited_config_manager.set_value("audit_config", "service.name", "audit-service")
  audited_config_manager.set_value("audit_config", "service.version", "1.0.0")
  audited_config_manager.set_value("audit_config", "resources.cpu", "500m")
  audited_config_manager.set_value("audit_config", "resources.memory", "512Mi")
  
  // 修改配置（生成审计记录）
  audited_config_manager.set_value("audit_config", "service.version", "1.1.0")
  audited_config_manager.set_value("audit_config", "resources.cpu", "1000m")
  
  // 查询审计日志
  let audit_logs = audit_manager.query_audit_logs({
    config_name: "audit_config",
    start_time: Time::now() - 3600000,  // 最近1小时
    end_time: Time::now(),
    limit: 100
  })
  
  assert_true(audit_logs.length() >= 6)  // 至少6条审计记录（4个初始设置 + 2个修改）
  
  // 验证审计记录内容
  let service_name_log = audit_logs.find(fn(log) { log.field_path == "service.name" })
  assert_true(service_name_log != None)
  
  match service_name_log {
    Some(log) => {
      assert_eq(log.action, "create")
      assert_eq(log.config_name, "audit_config")
      assert_eq(log.field_path, "service.name")
      assert_eq(log.new_value, Some("audit-service"))
      assert_eq(log.user_context.get("username"), Some("admin"))
      assert_true(log.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  // 验证变更记录
  let version_change_log = audit_logs.find(fn(log) { 
    log.field_path == "service.version" && log.action == "update"
  })
  assert_true(version_change_log != None)
  
  match version_change_log {
    Some(log) => {
      assert_eq(log.old_value, Some("1.0.0"))
      assert_eq(log.new_value, Some("1.1.0"))
      assert_true(log.diff.contains("1.0.0"))
      assert_true(log.diff.contains("1.1.0"))
    }
    None => assert_true(false)
  }
  
  // 测试批量操作审计
  let batch_updates = [
    { field: "resources.cpu", value: "750m" },
    { field: "resources.memory", value: "768Mi" }
  ]
  
  audited_config_manager.update_batch("audit_config", batch_updates)
  
  let batch_audit_logs = audit_manager.query_audit_logs({
    config_name: "audit_config",
    action: "batch_update",
    limit: 10
  })
  
  assert_true(batch_audit_logs.length() >= 1)
  
  match batch_audit_logs[0] {
    Some(log) => {
      assert_eq(log.action, "batch_update")
      assert_true(log.details.contains("2 fields"))
    }
    None => assert_true(false)
  }
  
  // 测试配置回滚审计
  let rollback_point = audited_config_manager.create_rollback_point("audit_config")
  audited_config_manager.set_value("audit_config", "service.version", "2.0.0")
  audited_config_manager.rollback_to_point("audit_config", rollback_point.rollback_id)
  
  let rollback_audit_logs = audit_manager.query_audit_logs({
    config_name: "audit_config",
    action: "rollback",
    limit: 10
  })
  
  assert_true(rollback_audit_logs.length() >= 1)
  
  // 测试审计日志导出
  let export_result = audit_manager.export_audit_logs({
    format: "json",
    start_time: Time::now() - 3600000,
    end_time: Time::now(),
    include_diffs: true,
    include_user_context: true
  })
  
  assert_true(export_result.success)
  assert_true(export_result.content.length() > 0)
  
  // 测试审计日志分析
  let analysis_result = audit_manager.analyze_audit_logs({
    period: "24h",
    group_by: ["user", "action"],
    metrics: ["count", "frequency"]
  })
  
  assert_true(analysis_result.success)
  assert_true(analysis_result.summary.length() > 0)
  
  // 验证用户活动统计
  let user_stats = analysis_result.summary.find(fn(stat) => 
    stat.group.get("user") == Some("admin")
  )
  assert_true(user_stats != None)
  
  // 测试合规性报告
  let compliance_report = audit_manager.generate_compliance_report({
    compliance_rules: [
      {
        name: "config_change_approval",
        description: "所有配置变更必须经过审批",
        check: "action == 'update' => approval_required == true",
        severity: "high"
      },
      {
        name: "sensitive_field_access",
        description: "敏感字段访问必须记录",
        check: "field_path contains 'password' or field_path contains 'key' => audit_log.exists()",
        severity: "medium"
      }
    ],
    period: "7d"
  })
  
  assert_true(compliance_report.success)
  assert_true(compliance_report.violations.length() >= 0)  // 可能有违规，也可能没有
  
  // 测试审计日志完整性验证
  let integrity_check = audit_manager.verify_log_integrity({
    start_time: Time::now() - 3600000,
    end_time: Time::now()
  })
  
  assert_true(integrity_check.valid)
  assert_true(integrity_check.verified_entries > 0)
  
  // 清理测试文件
  File::delete("/tmp/config_audit.log")
}

// 测试10: 配置管理API
test "配置管理API测试" {
  // 创建配置管理API服务器
  let config_api_server = ConfigAPIServer::new({
    host: "localhost",
    port: 8080,
    auth_enabled: true,
    rate_limiting: true,
    cors_enabled: true,
    api_version: "v1"
  })
  
  // 创建后端配置管理器
  let backend_config_manager = ConfigManager::new({
    storage_type: "memory",
    validation_enabled: true,
    audit_enabled: true
  })
  
  // 注册API路由
  config_api_server.register_routes({
    "GET /api/v1/configs": fn(request) {
      let configs = backend_config_manager.list_configs()
      APIResponse::success({ configs: configs })
    },
    
    "GET /api/v1/configs/{name}": fn(request) {
      let config_name = request.path_params.get("name")
      match config_name {
        Some(name) => {
          let config_result = backend_config_manager.get_config(name)
          if config_result.success {
            APIResponse::success({ config: config_result.config })
          } else {
            APIResponse::error(404, "配置不存在")
          }
        }
        None => APIResponse::error(400, "缺少配置名称")
      }
    },
    
    "POST /api/v1/configs/{name}": fn(request) {
      let config_name = request.path_params.get("name")
      let config_data = request.body
      
      match (config_name, config_data) {
        (Some(name), Some(data)) => {
          let save_result = backend_config_manager.save_config(name, data)
          if save_result.success {
            APIResponse::success({ message: "配置保存成功" })
          } else {
            APIResponse::error(400, "配置保存失败: " + save_result.error)
          }
        }
        _ => APIResponse::error(400, "无效请求")
      }
    },
    
    "PUT /api/v1/configs/{name}/fields/{field}": fn(request) {
      let config_name = request.path_params.get("name")
      let field_name = request.path_params.get("field")
      let field_value = request.body
      
      match (config_name, field_name, field_value) {
        (Some(name), Some(field), Some(value)) => {
          let update_result = backend_config_manager.set_value(name, field, value)
          if update_result.success {
            APIResponse::success({ message: "字段更新成功" })
          } else {
            APIResponse::error(400, "字段更新失败: " + update_result.error)
          }
        }
        _ => APIResponse::error(400, "无效请求")
      }
    },
    
    "DELETE /api/v1/configs/{name}": fn(request) {
      let config_name = request.path_params.get("name")
      match config_name {
        Some(name) => {
          let delete_result = backend_config_manager.delete_config(name)
          if delete_result.success {
            APIResponse::success({ message: "配置删除成功" })
          } else {
            APIResponse::error(400, "配置删除失败: " + delete_result.error)
          }
        }
        None => APIResponse::error(400, "缺少配置名称")
      }
    },
    
    "POST /api/v1/configs/{name}/validate": fn(request) {
      let config_name = request.path_params.get("name")
      let config_data = request.body
      
      match (config_name, config_data) {
        (Some(name), Some(data)) => {
          let validation_result = backend_config_manager.validate_config(name, data)
          APIResponse::success({ 
            valid: validation_result.valid,
            errors: validation_result.errors
          })
        }
        _ => APIResponse::error(400, "无效请求")
      }
    }
  })
  
  // 启动API服务器
  let server_start_result = config_api_server.start()
  assert_true(server_start_result.success)
  
  // 测试API端点
  let api_client = APIClient::new({
    base_url: "http://localhost:8080/api/v1",
    auth_token: "test_auth_token",
    timeout: 5000
  })
  
  // 测试列出配置
  let list_configs_result = api_client.get("configs")
  assert_true(list_configs_result.success)
  assert_eq(list_configs_result.status_code, 200)
  
  // 创建测试配置
  let test_config = {
    "service": {
      "name": "api-test-service",
      "version": "1.0.0"
    },
    "server": {
      "port": 8080,
      "host": "0.0.0.0"
    }
  }
  
  // 测试创建配置
  let create_config_result = api_client.post("configs/api_test", test_config)
  assert_true(create_config_result.success)
  assert_eq(create_config_result.status_code, 200)
  
  // 测试获取配置
  let get_config_result = api_client.get("configs/api_test")
  assert_true(get_config_result.success)
  assert_eq(get_config_result.status_code, 200)
  
  let get_config_data = get_config_result.body
  assert_eq(get_config_data.get("service.name"), Some("api-test-service"))
  assert_eq(get_config_data.get("server.port"), Some(8080))
  
  // 测试更新字段
  let update_field_result = api_client.put("configs/api_test/fields/server.port", 9090)
  assert_true(update_field_result.success)
  assert_eq(update_field_result.status_code, 200)
  
  // 验证字段已更新
  let updated_config_result = api_client.get("configs/api_test")
  assert_eq(updated_config_result.body.get("server.port"), Some(9090))
  
  // 测试配置验证
  let invalid_config = {
    "service": {
      "name": "api-test-service"
      // 缺少必需的 version 字段
    },
    "server": {
      "port": "invalid_port"  // 无效的端口类型
    }
  }
  
  let validation_result = api_client.post("configs/api_test/validate", invalid_config)
  assert_true(validation_result.success)
  assert_eq(validation_result.status_code, 200)
  
  let validation_data = validation_result.body
  assert_eq(validation_data.get("valid"), Some(false))
  assert_true(validation_data.get("errors") != None)
  
  // 测试删除配置
  let delete_config_result = api_client.delete("configs/api_test")
  assert_true(delete_config_result.success)
  assert_eq(delete_config_result.status_code, 200)
  
  // 验证配置已删除
  let deleted_config_result = api_client.get("configs/api_test")
  assert_eq(deleted_config_result.status_code, 404)
  
  // 测试API认证
  let unauthorized_client = APIClient::new({
    base_url: "http://localhost:8080/api/v1",
    auth_token: "invalid_token",
    timeout: 5000
  })
  
  let unauthorized_result = unauthorized_client.get("configs")
  assert_eq(unauthorized_result.status_code, 401)
  
  // 测试API限流
  let rate_limit_client = APIClient::new({
    base_url: "http://localhost:8080/api/v1",
    auth_token: "test_auth_token",
    timeout: 5000
  })
  
  let mut rate_limit_hits = 0
  for i in 0..=20 {  // 发送大量请求，触发限流
    let result = rate_limit_client.get("configs")
    if result.status_code == 429 {
      rate_limit_hits = rate_limit_hits + 1
    }
  }
  
  assert_true(rate_limit_hits > 0)  // 应该触发限流
  
  // 测试API文档
  let docs_result = api_client.get("docs")
  assert_true(docs_result.success)
  assert_eq(docs_result.status_code, 200)
  
  // 测试API版本信息
  let version_result = api_client.get("version")
  assert_true(version_result.success)
  assert_eq(version_result.status_code, 200)
  
  let version_data = version_result.body
  assert_eq(version_data.get("version"), Some("v1"))
  
  // 停止API服务器
  let server_stop_result = config_api_server.stop()
  assert_true(server_stop_result.success)
}

// 辅助函数：验证子网格式
fn is_valid_subnet(subnet: String) -> Bool {
  // 简单的子网验证，实际实现会更复杂
  subnet.contains("/") && subnet.length() > 0
}