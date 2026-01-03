// Azimuth 配置管理测试用例
// 专注于测试配置加载、验证、更新和热重载功能

// 测试1: 配置加载和解析
test "配置加载和解析测试" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new({
    config_dir: "/etc/azimuth",
    config_format: "yaml",
    env_prefix: "AZIMUTH_",
    enable_watch: true
  })
  
  // 模拟配置文件内容
  let mock_config_content = """
# Azimuth Telemetry Configuration
service:
  name: "azimuth-telemetry"
  version: "1.2.3"
  environment: "production"
  
telemetry:
  tracing:
    enabled: true
    sampling_rate: 0.1
    exporter: "jaeger"
    jaeger:
      endpoint: "http://jaeger:14268/api/traces"
      service_name: "azimuth-telemetry"
  
  metrics:
    enabled: true
    port: 9090
    path: "/metrics"
    exporter: "prometheus"
    
  logging:
    level: "info"
    format: "json"
    output: "stdout"
    
database:
  host: "localhost"
  port: 5432
  name: "azimuth_db"
  username: "azimuth_user"
  password: "secure_password"
  pool_size: 10
  timeout_ms: 5000
  
cache:
  type: "redis"
  host: "redis"
  port: 6379
  ttl_seconds: 3600
"""
  
  // 加载配置
  let load_result = config_manager.load_from_string(mock_config_content)
  
  match load_result {
    Ok(config) => {
      // 验证服务配置
      assert_eq(config.get_string("service.name"), Some("azimuth-telemetry"))
      assert_eq(config.get_string("service.version"), Some("1.2.3"))
      assert_eq(config.get_string("service.environment"), Some("production"))
      
      // 验证遥测配置
      assert_eq(config.get_bool("telemetry.tracing.enabled"), Some(true))
      assert_eq(config.get_float("telemetry.tracing.sampling_rate"), Some(0.1))
      assert_eq(config.get_string("telemetry.tracing.exporter"), Some("jaeger"))
      
      assert_eq(config.get_bool("telemetry.metrics.enabled"), Some(true))
      assert_eq(config.get_int("telemetry.metrics.port"), Some(9090))
      assert_eq(config.get_string("telemetry.metrics.path"), Some("/metrics"))
      
      // 验证数据库配置
      assert_eq(config.get_string("database.host"), Some("localhost"))
      assert_eq(config.get_int("database.port"), Some(5432))
      assert_eq(config.get_string("database.name"), Some("azimuth_db"))
      assert_eq(config.get_int("database.pool_size"), Some(10))
      
      // 验证缓存配置
      assert_eq(config.get_string("cache.type"), Some("redis"))
      assert_eq(config.get_string("cache.host"), Some("redis"))
      assert_eq(config.get_int("cache.ttl_seconds"), Some(3600))
    }
    Err(error) => {
      assert_true(false, "配置加载失败: " + error.to_string())
    }
  }
  
  // 测试环境变量覆盖
  // 设置环境变量
  set_env("AZIMUTH_TELEMETRY_TRACING_SAMPLING_RATE", "0.5")
  set_env("AZIMUTH_DATABASE_POOL_SIZE", "20")
  
  // 重新加载配置
  let reload_result = config_manager.reload_with_env_override()
  
  match reload_result {
    Ok(config) => {
      // 验证环境变量覆盖
      assert_eq(config.get_float("telemetry.tracing.sampling_rate"), Some(0.5))  // 覆盖了0.1
      assert_eq(config.get_int("database.pool_size"), Some(20))  // 覆盖了10
      
      // 验证未覆盖的配置保持不变
      assert_eq(config.get_string("service.name"), Some("azimuth-telemetry"))
      assert_eq(config.get_bool("telemetry.tracing.enabled"), Some(true))
    }
    Err(error) => {
      assert_true(false, "配置重载失败: " + error.to_string())
    }
  }
  
  // 清理环境变量
  unset_env("AZIMUTH_TELEMETRY_TRACING_SAMPLING_RATE")
  unset_env("AZIMUTH_DATABASE_POOL_SIZE")
}

// 测试2: 配置验证和类型检查
test "配置验证和类型检查测试" {
  // 创建配置验证器
  let validator = ConfigValidator::new()
  
  // 定义配置模式
  let config_schema = {
    "service": {
      "type": "object",
      "required": ["name", "version"],
      "properties": {
        "name": { "type": "string", "min_length": 1, "max_length": 100 },
        "version": { "type": "string", "pattern": "^\\d+\\.\\d+\\.\\d+$" },
        "environment": { "type": "string", "enum": ["development", "staging", "production"] }
      }
    },
    "telemetry": {
      "type": "object",
      "properties": {
        "tracing": {
          "type": "object",
          "properties": {
            "enabled": { "type": "boolean" },
            "sampling_rate": { "type": "number", "minimum": 0, "maximum": 1 }
          }
        },
        "metrics": {
          "type": "object",
          "properties": {
            "port": { "type": "integer", "minimum": 1024, "maximum": 65535 }
          }
        }
      }
    }
  }
  
  validator.set_schema(config_schema)
  
  // 测试有效配置
  let valid_config = {
    "service": {
      "name": "test-service",
      "version": "1.0.0",
      "environment": "production"
    },
    "telemetry": {
      "tracing": {
        "enabled": true,
        "sampling_rate": 0.1
      },
      "metrics": {
        "port": 9090
      }
    }
  }
  
  let valid_result = validator.validate(valid_config)
  match valid_result {
    Ok(_) => {
      assert_true(true, "有效配置验证通过")
    }
    Err(errors) => {
      assert_true(false, "有效配置验证失败: " + errors.join(", "))
    }
  }
  
  // 测试无效配置 - 缺少必需字段
  let missing_field_config = {
    "service": {
      "version": "1.0.0"  // 缺少name字段
    },
    "telemetry": {}
  }
  
  let missing_field_result = validator.validate(missing_field_config)
  match missing_field_result {
    Ok(_) => {
      assert_true(false, "应该检测到缺少必需字段")
    }
    Err(errors) => {
      assert_true(errors.contains("service.name"), "应该报告缺少name字段")
    }
  }
  
  // 测试无效配置 - 类型不匹配
  let type_mismatch_config = {
    "service": {
      "name": "test-service",
      "version": "1.0.0",
      "environment": "invalid_env"  // 不在枚举值中
    },
    "telemetry": {
      "tracing": {
        "enabled": "true",  // 应该是boolean，不是string
        "sampling_rate": 1.5  // 超出最大值1
      }
    }
  }
  
  let type_mismatch_result = validator.validate(type_mismatch_config)
  match type_mismatch_result {
    Ok(_) => {
      assert_true(false, "应该检测到类型不匹配")
    }
    Err(errors) => {
      assert_true(errors.contains("service.environment"), "应该报告environment枚举错误")
      assert_true(errors.contains("telemetry.tracing.enabled"), "应该报告enabled类型错误")
      assert_true(errors.contains("telemetry.tracing.sampling_rate"), "应该报告sampling_rate范围错误")
    }
  }
  
  // 测试自定义验证规则
  validator.add_custom_rule("port_availability", |path, value| {
    if path.ends_with(".port") {
      let port = value.to_int()
      // 模拟端口检查
      if port == 8080 || port == 9090 {
        Ok(())
      } else {
        Err("端口 " + port.to_string() + " 不可用")
      }
    } else {
      Ok(())
    }
  })
  
  let custom_rule_config = {
    "service": {
      "name": "test-service",
      "version": "1.0.0"
    },
    "telemetry": {
      "metrics": {
        "port": 8081  // 不可用端口
      }
    }
  }
  
  let custom_rule_result = validator.validate(custom_rule_config)
  match custom_rule_result {
    Ok(_) => {
      assert_true(false, "应该检测到端口不可用")
    }
    Err(errors) => {
      assert_true(errors.contains("端口 8081 不可用"), "应该报告端口不可用")
    }
  }
}

// 测试3: 配置热重载
test "配置热重载测试" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new({
    config_file: "/tmp/test_config.yaml",
    watch_interval_ms: 100,
    enable_watch: true
  })
  
  // 初始配置
  let initial_config = """
service:
  name: "initial-service"
  version: "1.0.0"
  
telemetry:
  tracing:
    enabled: true
    sampling_rate: 0.1
"""
  
  // 写入初始配置
  File::write("/tmp/test_config.yaml", initial_config).unwrap()
  
  // 加载初始配置
  let load_result = config_manager.load()
  match load_result {
    Ok(config) => {
      assert_eq(config.get_string("service.name"), Some("initial-service"))
      assert_eq(config.get_float("telemetry.tracing.sampling_rate"), Some(0.1))
    }
    Err(_) => assert_true(false, "初始配置加载失败")
  }
  
  // 设置配置变更监听器
  let mut change_detected = false
  let mut new_sampling_rate = None
  
  config_manager.add_change_listener(|old_config, new_config| {
    change_detected = true
    new_sampling_rate = new_config.get_float("telemetry.tracing.sampling_rate")
  })
  
  // 修改配置文件
  let updated_config = """
service:
  name: "updated-service"
  version: "1.1.0"
  
telemetry:
  tracing:
    enabled: true
    sampling_rate: 0.5
"""
  
  File::write("/tmp/test_config.yaml", updated_config).unwrap()
  
  // 等待文件监控检测变更
  Thread::sleep(200)
  
  // 验证变更检测
  assert_true(change_detected, "应该检测到配置变更")
  assert_eq(new_sampling_rate, Some(0.5), "应该获取新的采样率")
  
  // 验证配置已更新
  let current_config = config_manager.get_current_config()
  assert_eq(current_config.get_string("service.name"), Some("updated-service"))
  assert_eq(current_config.get_float("telemetry.tracing.sampling_rate"), Some(0.5))
  
  // 测试无效配置热重载（应该保持旧配置）
  let invalid_config = """
invalid: yaml: content:
  - missing
  - proper
  - structure
"""
  
  File::write("/tmp/test_config.yaml", invalid_config).unwrap()
  
  // 等待文件监控检测变更
  Thread::sleep(200)
  
  // 验证配置未变更（保持上一个有效配置）
  let unchanged_config = config_manager.get_current_config()
  assert_eq(unchanged_config.get_string("service.name"), Some("updated-service"))
  assert_eq(unchanged_config.get_float("telemetry.tracing.sampling_rate"), Some(0.5))
  
  // 清理
  File::remove("/tmp/test_config.yaml").unwrap()
}

// 测试4: 配置分层和继承
test "配置分层和继承测试" {
  // 创建分层配置管理器
  let layered_config = LayeredConfigManager::new()
  
  // 添加默认配置层（最低优先级）
  let default_config = {
    "service": {
      "name": "azimuth",
      "version": "1.0.0",
      "port": 8080,
      "timeout": 30000
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1,
      "export_interval": 60000
    },
    "logging": {
      "level": "info",
      "format": "text"
    }
  }
  
  layered_config.add_layer("default", default_config, 100)
  
  // 添加环境配置层（中等优先级）
  let environment_config = {
    "service": {
      "name": "azimuth-staging",
      "port": 8081
    },
    "telemetry": {
      "sampling_rate": 0.5
    },
    "logging": {
      "level": "debug"
    }
  }
  
  layered_config.add_layer("environment", environment_config, 200)
  
  // 添加特定实例配置层（最高优先级）
  let instance_config = {
    "service": {
      "timeout": 15000
    },
    "telemetry": {
      "export_interval": 30000
    }
  }
  
  layered_config.add_layer("instance", instance_config, 300)
  
  // 获取合并后的配置
  let merged_config = layered_config.get_merged_config()
  
  // 验证配置合并结果
  // service.name来自environment层（覆盖了default）
  assert_eq(merged_config.get_string("service.name"), Some("azimuth-staging"))
  
  // service.version来自default层（未被覆盖）
  assert_eq(merged_config.get_string("service.version"), Some("1.0.0"))
  
  // service.port来自environment层（覆盖了default）
  assert_eq(merged_config.get_int("service.port"), Some(8081))
  
  // service.timeout来自instance层（覆盖了default）
  assert_eq(merged_config.get_int("service.timeout"), Some(15000))
  
  // telemetry.enabled来自default层（未被覆盖）
  assert_eq(merged_config.get_bool("telemetry.enabled"), Some(true))
  
  // telemetry.sampling_rate来自environment层（覆盖了default）
  assert_eq(merged_config.get_float("telemetry.sampling_rate"), Some(0.5))
  
  // telemetry.export_interval来自instance层（覆盖了default）
  assert_eq(merged_config.get_int("telemetry.export_interval"), Some(30000))
  
  // logging.level来自environment层（覆盖了default）
  assert_eq(merged_config.get_string("logging.level"), Some("debug"))
  
  // logging.format来自default层（未被覆盖）
  assert_eq(merged_config.get_string("logging.format"), Some("text"))
  
  // 测试配置层动态更新
  let new_instance_config = {
    "service": {
      "name": "azimuth-special"  // 覆盖environment层
    },
    "telemetry": {
      "enabled": false  // 覆盖default层
    }
  }
  
  layered_config.update_layer("instance", new_instance_config)
  
  let updated_config = layered_config.get_merged_config()
  
  // 验证更新结果
  assert_eq(updated_config.get_string("service.name"), Some("azimuth-special"))
  assert_eq(updated_config.get_bool("telemetry.enabled"), Some(false))
  
  // 其他配置应该保持不变
  assert_eq(updated_config.get_string("service.version"), Some("1.0.0"))
  assert_eq(updated_config.get_int("service.port"), Some(8081))
  
  // 测试配置层移除
  layered_config.remove_layer("environment")
  
  let without_env_config = layered_config.get_merged_config()
  
  // 验证environment层的配置被移除，回退到default层
  assert_eq(without_env_config.get_string("service.name"), Some("azimuth-special"))  // instance层保留
  assert_eq(without_env_config.get_int("service.port"), Some(8080))  // 回退到default层
  assert_eq(without_env_config.get_float("telemetry.sampling_rate"), Some(0.1))  // 回退到default层
  assert_eq(without_env_config.get_string("logging.level"), Some("info"))  // 回退到default层
}

// 测试5: 配置安全和加密
test "配置安全和加密测试" {
  // 创建安全配置管理器
  let secure_config = SecureConfigManager::new({
    encryption_key: "test_encryption_key_12345",
    encrypted_fields: ["database.password", "api.secret", "auth.jwt_secret"],
    hash_fields: ["admin.password_hash"],
    access_control: true
  })
  
  // 创建包含敏感信息的配置
  let sensitive_config = {
    "service": {
      "name": "secure-service",
      "port": 8080
    },
    "database": {
      "host": "localhost",
      "username": "admin",
      "password": "plain_text_password"  // 需要加密
    },
    "api": {
      "key": "public_key",
      "secret": "secret_api_key"  // 需要加密
    },
    "auth": {
      "jwt_secret": "jwt_signing_secret",  // 需要加密
      "token_expiry": 3600
    },
    "admin": {
      "username": "admin",
      "password_hash": "hashed_password_value"  // 需要哈希
    }
  }
  
  // 保存安全配置
  let save_result = secure_config.save(sensitive_config, "/tmp/secure_config.yaml")
  
  match save_result {
    Ok(_) => {
      assert_true(true, "安全配置保存成功")
    }
    Err(error) => {
      assert_true(false, "安全配置保存失败: " + error.to_string())
    }
  }
  
  // 读取保存的配置文件内容
  let file_content = File::read_to_string("/tmp/secure_config.yaml").unwrap()
  
  // 验证敏感字段已加密
  assert_false(file_content.contains("plain_text_password"), "密码应该被加密")
  assert_false(file_content.contains("secret_api_key"), "API密钥应该被加密")
  assert_false(file_content.contains("jwt_signing_secret"), "JWT密钥应该被加密")
  
  // 验证加密字段存在
  assert_true(file_content.contains("encrypted:"), "应该包含加密标记")
  
  // 加载并解密配置
  let load_result = secure_config.load("/tmp/secure_config.yaml")
  
  match load_result {
    Ok(loaded_config) => {
      // 验证解密后的配置
      assert_eq(loaded_config.get_string("service.name"), Some("secure-service"))
      assert_eq(loaded_config.get_string("database.password"), Some("plain_text_password"))
      assert_eq(loaded_config.get_string("api.secret"), Some("secret_api_key"))
      assert_eq(loaded_config.get_string("auth.jwt_secret"), Some("jwt_signing_secret"))
      assert_eq(loaded_config.get_string("admin.password_hash"), Some("hashed_password_value"))
    }
    Err(error) => {
      assert_true(false, "安全配置加载失败: " + error.to_string())
    }
  }
  
  // 测试访问控制
  secure_config.set_access_policy("database.password", ["admin", "db_admin"])
  secure_config.set_access_policy("api.secret", ["admin", "api_user"])
  
  // 模拟不同角色的访问
  let admin_access = secure_config.get_with_role("database.password", "admin")
  match admin_access {
    Ok(password) => {
      assert_eq(password, "plain_text_password")
    }
    Err(_) => {
      assert_true(false, "管理员应该能访问数据库密码")
    }
  }
  
  let user_access = secure_config.get_with_role("database.password", "regular_user")
  match user_access {
    Ok(_) => {
      assert_true(false, "普通用户不应该能访问数据库密码")
    }
    Err(error) => {
      match error {
        ConfigError::AccessDenied => {
          assert_true(true, "正确拒绝访问")
        }
        _ => {
          assert_true(false, "错误类型不匹配")
        }
      }
    }
  }
  
  // 测试配置审计日志
  let audit_log = secure_config.get_audit_log()
  assert_true(audit_log.length() > 0, "应该有审计记录")
  
  // 验证敏感操作被记录
  let sensitive_operations = audit_log.filter(|entry| 
    entry.action == "access_sensitive_field" || 
    entry.action == "decrypt_field"
  )
  assert_true(sensitive_operations.length() > 0, "应该记录敏感操作")
  
  // 清理
  File::remove("/tmp/secure_config.yaml").unwrap()
}