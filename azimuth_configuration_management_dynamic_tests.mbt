// Azimuth 配置管理和动态更新测试用例
// 专注于验证遥测系统的配置管理和动态更新功能

// 测试1: 基本配置加载和验证
test "基本配置加载和验证" {
  // 创建默认配置
  let default_config = {
    "service": {
      "name": "azimuth-service",
      "version": "1.0.0",
      "environment": "development"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 1.0,
      "export_interval": 5000,
      "batch_size": 100
    },
    "tracing": {
      "enabled": true,
      "max_spans_per_second": 1000,
      "span_timeout": 30000
    },
    "metrics": {
      "enabled": true,
      "export_interval": 10000,
      "aggregation_temporality": "cumulative"
    },
    "logging": {
      "enabled": true,
      "level": "info",
      "format": "json"
    }
  }
  
  // 加载配置
  let config_manager = ConfigurationManager::new()
  ConfigurationManager::load_config(config_manager, default_config)
  
  // 验证配置加载
  assert_eq(ConfigurationManager::get(config_manager, "service.name"), Some("azimuth-service"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.enabled"), Some("true"))
  assert_eq(ConfigurationManager::get(config_manager, "tracing.max_spans_per_second"), Some("1000"))
  
  // 验证嵌套配置访问
  let telemetry_config = ConfigurationManager::get_section(config_manager, "telemetry")
  assert_eq(telemetry_config["enabled"], "true")
  assert_eq(telemetry_config["sampling_rate"], "1.0")
  assert_eq(telemetry_config["export_interval"], "5000")
}

// 测试2: 配置文件热重载
test "配置文件热重载" {
  // 创建初始配置文件
  let initial_config = {
    "service": {
      "name": "hot-reload-service",
      "version": "1.0.0"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.5
    }
  }
  
  let config_file = "/tmp/azimuth_config.json"
  File::write(config_file, Json::stringify(initial_config))
  
  // 创建配置管理器并加载配置
  let config_manager = ConfigurationManager::new()
  ConfigurationManager::load_from_file(config_manager, config_file)
  
  // 验证初始配置
  assert_eq(ConfigurationManager::get(config_manager, "service.name"), Some("hot-reload-service"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling_rate"), Some("0.5"))
  
  // 更新配置文件
  let updated_config = {
    "service": {
      "name": "hot-reload-service",
      "version": "2.0.0"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.8,
      "export_interval": 10000
    }
  }
  
  File::write(config_file, Json::stringify(updated_config))
  
  // 触发热重载
  ConfigurationManager::reload(config_manager)
  
  // 验证更新后的配置
  assert_eq(ConfigurationManager::get(config_manager, "service.version"), Some("2.0.0"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling_rate"), Some("0.8"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.export_interval"), Some("10000"))
  
  // 清理
  File::remove(config_file)
}

// 测试3: 动态配置更新和回调
test "动态配置更新和回调" {
  let config_manager = ConfigurationManager::new()
  
  // 设置初始配置
  ConfigurationManager::set(config_manager, "service.name", "dynamic-service")
  ConfigurationManager::set(config_manager, "telemetry.sampling_rate", "0.5")
  ConfigurationManager::set(config_manager, "tracing.enabled", "true")
  
  // 创建配置变更监听器
  let mut change_log = []
  let listener = fn(key, old_value, new_value) {
    change_log.push({
      "key": key,
      "old": old_value,
      "new": new_value,
      "timestamp": Time::now()
    })
  }
  
  // 注册监听器
  ConfigurationManager::add_listener(config_manager, "telemetry.sampling_rate", listener)
  ConfigurationManager::add_listener(config_manager, "tracing.enabled", listener)
  
  // 动态更新配置
  ConfigurationManager::update(config_manager, "telemetry.sampling_rate", "0.8")
  ConfigurationManager::update(config_manager, "tracing.enabled", "false")
  ConfigurationManager::update(config_manager, "service.version", "1.5.0")
  
  // 验证配置更新
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling_rate"), Some("0.8"))
  assert_eq(ConfigurationManager::get(config_manager, "tracing.enabled"), Some("false"))
  assert_eq(ConfigurationManager::get(config_manager, "service.version"), Some("1.5.0"))
  
  // 验证变更日志
  assert_eq(change_log.length(), 2) // 只有监听的配置项触发回调
  assert_eq(change_log[0]["key"], "telemetry.sampling_rate")
  assert_eq(change_log[0]["old"], "0.5")
  assert_eq(change_log[0]["new"], "0.8")
  assert_eq(change_log[1]["key"], "tracing.enabled")
  assert_eq(change_log[1]["old"], "true")
  assert_eq(change_log[1]["new"], "false")
}

// 测试4: 配置验证和约束检查
test "配置验证和约束检查" {
  let config_manager = ConfigurationManager::new()
  
  // 设置配置验证规则
  ConfigurationManager::add_validator(config_manager, "telemetry.sampling_rate", fn(value) {
    let rate = value.to_float()
    rate >= 0.0 && rate <= 1.0
  })
  
  ConfigurationManager::add_validator(config_manager, "tracing.max_spans_per_second", fn(value) {
    let max_spans = value.to_int()
    max_spans > 0 && max_spans <= 10000
  })
  
  ConfigurationManager::add_validator(config_manager, "service.name", fn(value) {
    value.length() > 0 && value.length() <= 100
  })
  
  // 测试有效配置
  assert_true(ConfigurationManager::validate_and_set(config_manager, "telemetry.sampling_rate", "0.7"))
  assert_true(ConfigurationManager::validate_and_set(config_manager, "tracing.max_spans_per_second", "500"))
  assert_true(ConfigurationManager::validate_and_set(config_manager, "service.name", "valid-service-name"))
  
  // 测试无效配置
  assert_false(ConfigurationManager::validate_and_set(config_manager, "telemetry.sampling_rate", "1.5")) // 超出范围
  assert_false(ConfigurationManager::validate_and_set(config_manager, "tracing.max_spans_per_second", "0")) // 太小
  assert_false(ConfigurationManager::validate_and_set(config_manager, "tracing.max_spans_per_second", "15000")) // 太大
  assert_false(ConfigurationManager::validate_and_set(config_manager, "service.name", "")) // 空字符串
  
  // 验证配置未被无效值更新
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling_rate"), Some("0.7"))
  assert_eq(ConfigurationManager::get(config_manager, "tracing.max_spans_per_second"), Some("500"))
  assert_eq(ConfigurationManager::get(config_manager, "service.name"), Some("valid-service-name"))
}

// 测试5: 环境变量配置覆盖
test "环境变量配置覆盖" {
  let config_manager = ConfigurationManager::new()
  
  // 设置基础配置
  ConfigurationManager::set(config_manager, "service.name", "base-service")
  ConfigurationManager::set(config_manager, "telemetry.enabled", "false")
  ConfigurationManager::set(config_manager, "logging.level", "debug")
  
  // 设置环境变量
  Environment::set("AZIMUTH_SERVICE_NAME", "env-service")
  Environment::set("AZIMUTH_TELEMETRY_ENABLED", "true")
  Environment::set("AZIMUTH_LOGGING_LEVEL", "info")
  
  // 启用环境变量覆盖
  ConfigurationManager::enable_env_override(config_manager, "AZIMUTH_")
  
  // 重新加载配置
  ConfigurationManager::reload_with_env(config_manager)
  
  // 验证环境变量覆盖
  assert_eq(ConfigurationManager::get(config_manager, "service.name"), Some("env-service"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.enabled"), Some("true"))
  assert_eq(ConfigurationManager::get(config_manager, "logging.level"), Some("info"))
  
  // 清理环境变量
  Environment::unset("AZIMUTH_SERVICE_NAME")
  Environment::unset("AZIMUTH_TELEMETRY_ENABLED")
  Environment::unset("AZIMUTH_LOGGING_LEVEL")
}

// 测试6: 配置版本控制和回滚
test "配置版本控制和回滚" {
  let config_manager = ConfigurationManager::new()
  
  // 设置初始配置
  ConfigurationManager::set(config_manager, "service.version", "1.0.0")
  ConfigurationManager::set(config_manager, "telemetry.sampling_rate", "0.5")
  ConfigurationManager::set(config_manager, "tracing.enabled", "true")
  
  // 创建配置快照
  let snapshot1 = ConfigurationManager::create_snapshot(config_manager)
  
  // 更新配置
  ConfigurationManager::set(config_manager, "service.version", "1.1.0")
  ConfigurationManager::set(config_manager, "telemetry.sampling_rate", "0.7")
  
  // 创建第二个快照
  let snapshot2 = ConfigurationManager::create_snapshot(config_manager)
  
  // 再次更新配置
  ConfigurationManager::set(config_manager, "service.version", "1.2.0")
  ConfigurationManager::set(config_manager, "telemetry.sampling_rate", "0.9")
  ConfigurationManager::set(config_manager, "tracing.enabled", "false")
  
  // 验证当前配置
  assert_eq(ConfigurationManager::get(config_manager, "service.version"), Some("1.2.0"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling_rate"), Some("0.9"))
  assert_eq(ConfigurationManager::get(config_manager, "tracing.enabled"), Some("false"))
  
  // 回滚到第二个快照
  ConfigurationManager::rollback_to_snapshot(config_manager, snapshot2)
  
  // 验证回滚结果
  assert_eq(ConfigurationManager::get(config_manager, "service.version"), Some("1.1.0"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling_rate"), Some("0.7"))
  assert_eq(ConfigurationManager::get(config_manager, "tracing.enabled"), Some("true")) // 未在snapshot2中更改，保持原值
  
  // 回滚到第一个快照
  ConfigurationManager::rollback_to_snapshot(config_manager, snapshot1)
  
  // 验证回滚结果
  assert_eq(ConfigurationManager::get(config_manager, "service.version"), Some("1.0.0"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling_rate"), Some("0.5"))
  assert_eq(ConfigurationManager::get(config_manager, "tracing.enabled"), Some("true"))
}

// 测试7: 配置模板和继承
test "配置模板和继承" {
  let config_manager = ConfigurationManager::new()
  
  // 定义配置模板
  let base_template = {
    "service": {
      "environment": "production",
      "region": "us-west-2"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1
    },
    "logging": {
      "level": "warn",
      "format": "json"
    }
  }
  
  // 注册模板
  ConfigurationManager::register_template(config_manager, "base", base_template)
  
  // 从模板创建配置
  ConfigurationManager::apply_template(config_manager, "base")
  
  // 验证模板应用
  assert_eq(ConfigurationManager::get(config_manager, "service.environment"), Some("production"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling_rate"), Some("0.1"))
  assert_eq(ConfigurationManager::get(config_manager, "logging.level"), Some("warn"))
  
  // 覆盖模板配置
  ConfigurationManager::set(config_manager, "service.environment", "development")
  ConfigurationManager::set(config_manager, "logging.level", "debug")
  
  // 验证覆盖
  assert_eq(ConfigurationManager::get(config_manager, "service.environment"), Some("development"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.sampling_rate"), Some("0.1")) // 保持模板值
  assert_eq(ConfigurationManager::get(config_manager, "logging.level"), Some("debug"))
}

// 测试8: 配置加密和安全存储
test "配置加密和安全存储" {
  let config_manager = ConfigurationManager::new()
  
  // 启用加密存储
  let encryption_key = "azimuth-encryption-key-12345"
  ConfigurationManager::enable_encryption(config_manager, encryption_key)
  
  // 存储敏感配置
  ConfigurationManager::set_secure(config_manager, "database.password", "secret-password-123")
  ConfigurationManager::set_secure(config_manager, "api.key", "api-key-abcdef-456789")
  ConfigurationManager::set_secure(config_manager, "jwt.secret", "jwt-secret-very-long-string")
  
  // 存储普通配置
  ConfigurationManager::set(config_manager, "service.name", "secure-service")
  ConfigurationManager::set(config_manager, "telemetry.enabled", "true")
  
  // 验证普通配置可正常访问
  assert_eq(ConfigurationManager::get(config_manager, "service.name"), Some("secure-service"))
  assert_eq(ConfigurationManager::get(config_manager, "telemetry.enabled"), Some("true"))
  
  // 验证敏感配置需要特殊访问
  assert_eq(ConfigurationManager::get_secure(config_manager, "database.password"), Some("secret-password-123"))
  assert_eq(ConfigurationManager::get_secure(config_manager, "api.key"), Some("api-key-abcdef-456789"))
  assert_eq(ConfigurationManager::get_secure(config_manager, "jwt.secret"), Some("jwt-secret-very-long-string"))
  
  // 验证敏感配置不会通过普通get方法泄露
  assert_eq(ConfigurationManager::get(config_manager, "database.password"), None)
  assert_eq(ConfigurationManager::get(config_manager, "api.key"), None)
  
  // 测试配置导出时敏感数据的处理
  let exported_config = ConfigurationManager::export_config(config_manager)
  assert_true(exported_config.contains("\"service.name\":\"secure-service\""))
  assert_false(exported_config.contains("secret-password-123"))
  assert_false(exported_config.contains("api-key-abcdef-456789"))
  assert_true(exported_config.contains("\"database.password\":\"[ENCRYPTED]\""))
  assert_true(exported_config.contains("\"api.key\":\"[ENCRYPTED]\""))
}