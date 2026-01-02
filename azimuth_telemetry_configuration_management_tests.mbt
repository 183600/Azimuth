// Azimuth Telemetry System Configuration Management Tests
// 遥测系统配置管理测试用例，专注于配置加载、验证、更新和版本控制

// Test 1: 配置文件加载和解析
test "configuration file loading and parsing" {
  // 创建配置管理器
  let config_manager = azimuth::config::Manager::new()
  
  // 创建测试配置文件内容
  let config_content = """
  {
    "telemetry": {
      "service": {
        "name": "azimuth-test-service",
        "version": "1.0.0",
        "instance_id": "instance-12345"
      },
      "tracing": {
        "enabled": true,
        "sampling_rate": 0.1,
        "exporter": {
          "type": "jaeger",
          "endpoint": "http://jaeger:14268/api/traces",
          "timeout": 5000
        }
      },
      "metrics": {
        "enabled": true,
        "exporter": {
          "type": "prometheus",
          "endpoint": "http://prometheus:9090/metrics",
          "port": 8080
        }
      },
      "logging": {
        "level": "info",
        "format": "json",
        "output": "stdout"
      }
    },
    "processing": {
      "batch_size": 100,
      "flush_interval": 5000,
      "max_queue_size": 1000
    }
  }
  """
  
  // 创建临时配置文件
  let config_file = azimuth::fs::create_temp_file("config.json", config_content)
  
  // 加载配置
  let load_result = azimuth::config::load_from_file(config_manager, config_file)
  
  // 验证加载结果
  assert_true(azimuth::config::is_successful(load_result))
  
  // 获取配置值
  let service_name = azimuth::config::get_string(config_manager, "telemetry.service.name")
  let service_version = azimuth::config::get_string(config_manager, "telemetry.service.version")
  let tracing_enabled = azimuth::config::get_bool(config_manager, "telemetry.tracing.enabled")
  let sampling_rate = azimuth::config::get_float(config_manager, "telemetry.tracing.sampling_rate")
  let batch_size = azimuth::config::get_int(config_manager, "processing.batch_size")
  
  // 验证配置值
  assert_eq(service_name, "azimuth-test-service")
  assert_eq(service_version, "1.0.0")
  assert_true(tracing_enabled)
  assert_eq(sampling_rate, 0.1)
  assert_eq(batch_size, 100)
  
  // 清理临时文件
  azimuth::fs::delete_file(config_file)
}

// Test 2: 配置验证和默认值
test "configuration validation and default values" {
  // 创建配置管理器
  let config_manager = azimuth::config::Manager::new()
  
  // 配置验证规则
  azimuth::config::add_validation_rule(config_manager, "telemetry.service.name", "required", true)
  azimuth::config::add_validation_rule(config_manager, "telemetry.service.version", "pattern", "^\\d+\\.\\d+\\.\\d+$")
  azimuth::config::add_validation_rule(config_manager, "telemetry.tracing.sampling_rate", "range", "0.0..1.0")
  azimuth::config::add_validation_rule(config_manager, "processing.batch_size", "min", 1)
  azimuth::config::add_validation_rule(config_manager, "processing.flush_interval", "min", 100)
  
  // 设置默认值
  azimuth::config::set_default(config_manager, "telemetry.service.name", "default-service")
  azimuth::config::set_default(config_manager, "telemetry.service.version", "1.0.0")
  azimuth::config::set_default(config_manager, "telemetry.tracing.enabled", true)
  azimuth::config::set_default(config_manager, "telemetry.tracing.sampling_rate", 0.1)
  azimuth::config::set_default(config_manager, "processing.batch_size", 100)
  azimuth::config::set_default(config_manager, "processing.flush_interval", 5000)
  
  // 创建部分配置（缺少一些字段）
  let partial_config = """
  {
    "telemetry": {
      "service": {
        "name": "partial-service"
      },
      "tracing": {
        "sampling_rate": 0.2
      }
    }
  }
  """
  
  // 加载部分配置
  let config_file = azimuth::fs::create_temp_file("partial_config.json", partial_config)
  let load_result = azimuth::config::load_from_file(config_manager, config_file)
  
  // 验证加载结果
  assert_true(azimuth::config::is_successful(load_result))
  
  // 验证显式设置的值
  let service_name = azimuth::config::get_string(config_manager, "telemetry.service.name")
  let sampling_rate = azimuth::config::get_float(config_manager, "telemetry.tracing.sampling_rate")
  
  assert_eq(service_name, "partial-service")
  assert_eq(sampling_rate, 0.2)
  
  // 验证默认值
  let service_version = azimuth::config::get_string(config_manager, "telemetry.service.version")
  let tracing_enabled = azimuth::config::get_bool(config_manager, "telemetry.tracing.enabled")
  let batch_size = azimuth::config::get_int(config_manager, "processing.batch_size")
  
  assert_eq(service_version, "1.0.0") // 应该使用默认值
  assert_true(tracing_enabled) // 应该使用默认值
  assert_eq(batch_size, 100) // 应该使用默认值
  
  // 清理临时文件
  azimuth::fs::delete_file(config_file)
}

// Test 3: 配置验证错误处理
test "configuration validation error handling" {
  // 创建配置管理器
  let config_manager = azimuth::config::Manager::new()
  
  // 配置验证规则
  azimuth::config::add_validation_rule(config_manager, "telemetry.service.name", "required", true)
  azimuth::config::add_validation_rule(config_manager, "telemetry.tracing.sampling_rate", "range", "0.0..1.0")
  
  // 创建无效配置
  let invalid_config = """
  {
    "telemetry": {
      "service": {
        // 缺少必需的name字段
        "version": "invalid-version" // 不符合版本模式
      },
      "tracing": {
        "sampling_rate": 1.5 // 超出范围
      }
    }
  }
  """
  
  // 加载无效配置
  let config_file = azimuth::fs::create_temp_file("invalid_config.json", invalid_config)
  let load_result = azimuth::config::load_from_file(config_manager, config_file)
  
  // 验证加载失败
  assert_false(azimuth::config::is_successful(load_result))
  
  // 获取验证错误
  let validation_errors = azimuth::config::get_validation_errors(load_result)
  
  // 验证错误内容
  assert_eq(validation_errors.length(), 2)
  assert_true(validation_errors.any(fn(e) { e.contains("telemetry.service.name") && e.contains("required") }))
  assert_true(validation_errors.any(fn(e) { e.contains("telemetry.tracing.sampling_rate") && e.contains("range") }))
  
  // 清理临时文件
  azimuth::fs::delete_file(config_file)
}

// Test 4: 动态配置更新
test "dynamic configuration updates" {
  // 创建配置管理器
  let config_manager = azimuth::config::Manager::new()
  
  // 启用动态配置
  azimuth::config::enable_dynamic_updates(config_manager, true)
  
  // 设置初始配置
  let initial_config = """
  {
    "telemetry": {
      "tracing": {
        "sampling_rate": 0.1
      },
      "metrics": {
        "enabled": true
      }
    }
  }
  """
  
  let config_file = azimuth::fs::create_temp_file("dynamic_config.json", initial_config)
  azimuth::config::load_from_file(config_manager, config_file)
  
  // 验证初始配置
  let initial_sampling_rate = azimuth::config::get_float(config_manager, "telemetry.tracing.sampling_rate")
  let initial_metrics_enabled = azimuth::config::get_bool(config_manager, "telemetry.metrics.enabled")
  
  assert_eq(initial_sampling_rate, 0.1)
  assert_true(initial_metrics_enabled)
  
  // 注册配置变更监听器
  let mut change_events = []
  azimuth::config::add_change_listener(config_manager, fn(path, old_value, new_value) {
    change_events = change_events.push((path, old_value, new_value))
  })
  
  // 更新配置文件
  let updated_config = """
  {
    "telemetry": {
      "tracing": {
        "sampling_rate": 0.2
      },
      "metrics": {
        "enabled": false
      }
    }
  }
  """
  
  azimuth::fs::write_file(config_file, updated_config)
  
  // 触发配置重新加载
  azimuth::config::reload(config_manager)
  
  // 验证配置已更新
  let updated_sampling_rate = azimuth::config::get_float(config_manager, "telemetry.tracing.sampling_rate")
  let updated_metrics_enabled = azimuth::config::get_bool(config_manager, "telemetry.metrics.enabled")
  
  assert_eq(updated_sampling_rate, 0.2)
  assert_false(updated_metrics_enabled)
  
  // 验证变更事件
  assert_eq(change_events.length(), 2)
  
  let sampling_rate_change = change_events.find(fn((p, _, _)) { p == "telemetry.tracing.sampling_rate" }).unwrap()
  let metrics_enabled_change = change_events.find(fn((p, _, _)) { p == "telemetry.metrics.enabled" }).unwrap()
  
  assert_eq(sampling_rate_change.0, "telemetry.tracing.sampling_rate")
  assert_eq(sampling_rate_change.1, "0.1")
  assert_eq(sampling_rate_change.2, "0.2")
  
  assert_eq(metrics_enabled_change.0, "telemetry.metrics.enabled")
  assert_eq(metrics_enabled_change.1, "true")
  assert_eq(metrics_enabled_change.2, "false")
  
  // 清理临时文件
  azimuth::fs::delete_file(config_file)
}

// Test 5: 配置环境变量覆盖
test "configuration environment variable overrides" {
  // 创建配置管理器
  let config_manager = azimuth::config::Manager::new()
  
  // 启用环境变量覆盖
  azimuth::config::enable_env_overrides(config_manager, true)
  azimuth::config::set_env_prefix(config_manager, "AZIMUTH_")
  
  // 设置环境变量
  azimuth::env::set("AZIMUTH_TELEMETRY_SERVICE_NAME", "env-override-service")
  azimuth::env::set("AZIMUTH_TELEMETRY_TRACING_SAMPLING_RATE", "0.3")
  azimuth::env::set("AZIMUTH_PROCESSING_BATCH_SIZE", "200")
  
  // 创建基础配置
  let base_config = """
  {
    "telemetry": {
      "service": {
        "name": "base-service",
        "version": "1.0.0"
      },
      "tracing": {
        "sampling_rate": 0.1
      }
    },
    "processing": {
      "batch_size": 100
    }
  }
  """
  
  // 加载配置
  let config_file = azimuth::fs::create_temp_file("base_config.json", base_config)
  azimuth::config::load_from_file(config_manager, config_file)
  
  // 验证环境变量覆盖
  let service_name = azimuth::config::get_string(config_manager, "telemetry.service.name")
  let service_version = azimuth::config::get_string(config_manager, "telemetry.service.version")
  let sampling_rate = azimuth::config::get_float(config_manager, "telemetry.tracing.sampling_rate")
  let batch_size = azimuth::config::get_int(config_manager, "processing.batch_size")
  
  // 验证覆盖结果
  assert_eq(service_name, "env-override-service") // 应该被环境变量覆盖
  assert_eq(service_version, "1.0.0") // 应该保持原值
  assert_eq(sampling_rate, 0.3) // 应该被环境变量覆盖
  assert_eq(batch_size, 200) // 应该被环境变量覆盖
  
  // 清理环境变量和临时文件
  azimuth::env::unset("AZIMUTH_TELEMETRY_SERVICE_NAME")
  azimuth::env::unset("AZIMUTH_TELEMETRY_TRACING_SAMPLING_RATE")
  azimuth::env::unset("AZIMUTH_PROCESSING_BATCH_SIZE")
  azimuth::fs::delete_file(config_file)
}

// Test 6: 配置版本控制
test "configuration version control" {
  // 创建配置管理器
  let config_manager = azimuth::config::Manager::new()
  
  // 启用配置版本控制
  azimuth::config::enable_versioning(config_manager, true)
  azimuth::config::set_max_versions(config_manager, 5) // 最多保留5个版本
  
  // 创建初始配置
  let v1_config = """
  {
    "telemetry": {
      "service": {
        "name": "versioned-service",
        "version": "1.0.0"
      },
      "tracing": {
        "sampling_rate": 0.1
      }
    }
  }
  """
  
  let config_file = azimuth::fs::create_temp_file("versioned_config.json", v1_config)
  azimuth::config::load_from_file(config_manager, config_file)
  
  // 获取初始版本
  let v1_id = azimuth::config::get_current_version_id(config_manager)
  let v1_timestamp = azimuth::config::get_version_timestamp(config_manager, v1_id)
  
  // 更新配置
  let v2_config = """
  {
    "telemetry": {
      "service": {
        "name": "versioned-service",
        "version": "1.1.0"
      },
      "tracing": {
        "sampling_rate": 0.2
      }
    }
  }
  """
  
  azimuth::fs::write_file(config_file, v2_config)
  azimuth::config::reload(config_manager)
  
  // 获取新版本
  let v2_id = azimuth::config::get_current_version_id(config_manager)
  let v2_timestamp = azimuth::config::get_version_timestamp(config_manager, v2_id)
  
  // 验证版本信息
  assert_not_eq(v1_id, v2_id)
  assert_true(v2_timestamp > v1_timestamp)
  
  // 验证当前配置
  let current_service_version = azimuth::config::get_string(config_manager, "telemetry.service.version")
  let current_sampling_rate = azimuth::config::get_float(config_manager, "telemetry.tracing.sampling_rate")
  
  assert_eq(current_service_version, "1.1.0")
  assert_eq(current_sampling_rate, 0.2)
  
  // 获取版本历史
  let version_history = azimuth::config::get_version_history(config_manager)
  assert_eq(version_history.length(), 2)
  
  // 回滚到之前的版本
  azimuth::config::rollback_to_version(config_manager, v1_id)
  
  // 验证回滚结果
  let rolled_back_service_version = azimuth::config::get_string(config_manager, "telemetry.service.version")
  let rolled_back_sampling_rate = azimuth::config::get_float(config_manager, "telemetry.tracing.sampling_rate")
  
  assert_eq(rolled_back_service_version, "1.0.0")
  assert_eq(rolled_back_sampling_rate, 0.1)
  
  // 清理临时文件
  azimuth::fs::delete_file(config_file)
}

// Test 7: 配置模板和继承
test "configuration templates and inheritance" {
  // 创建配置管理器
  let config_manager = azimuth::config::Manager::new()
  
  // 创建基础模板
  let base_template = """
  {
    "telemetry": {
      "service": {
        "name": "{{SERVICE_NAME}}",
        "version": "{{SERVICE_VERSION}}"
      },
      "tracing": {
        "enabled": true,
        "sampling_rate": 0.1
      },
      "metrics": {
        "enabled": true
      }
    },
    "processing": {
      "batch_size": 100,
      "flush_interval": 5000
    }
  }
  """
  
  // 创建环境特定模板
  let production_template = """
  {
    "extends": "base",
    "telemetry": {
      "tracing": {
        "sampling_rate": 0.01
      },
      "metrics": {
        "exporter": {
          "type": "prometheus",
          "endpoint": "http://prometheus-prod:9090/metrics"
        }
      }
    },
    "processing": {
      "batch_size": 500
    }
  }
  """
  
  let development_template = """
  {
    "extends": "base",
    "telemetry": {
      "tracing": {
        "sampling_rate": 1.0
      },
      "logging": {
        "level": "debug"
      }
    },
    "processing": {
      "batch_size": 10
    }
  }
  """
  
  // 注册模板
  azimuth::config::register_template(config_manager, "base", base_template)
  azimuth::config::register_template(config_manager, "production", production_template)
  azimuth::config::register_template(config_manager, "development", development_template)
  
  // 设置模板变量
  let template_vars = [
    ("SERVICE_NAME", "template-service"),
    ("SERVICE_VERSION", "2.0.0")
  ]
  
  // 渲染生产配置
  let prod_config = azimuth::config::render_template(config_manager, "production", template_vars)
  
  // 验证生产配置
  let prod_service_name = azimuth::config::get_string_from_config(prod_config, "telemetry.service.name")
  let prod_service_version = azimuth::config::get_string_from_config(prod_config, "telemetry.service.version")
  let prod_sampling_rate = azimuth::config::get_float_from_config(prod_config, "telemetry.tracing.sampling_rate")
  let prod_batch_size = azimuth::config::get_int_from_config(prod_config, "processing.batch_size")
  let prod_metrics_endpoint = azimuth::config::get_string_from_config(prod_config, "telemetry.metrics.exporter.endpoint")
  
  assert_eq(prod_service_name, "template-service")
  assert_eq(prod_service_version, "2.0.0")
  assert_eq(prod_sampling_rate, 0.01) // 生产环境采样率应该更低
  assert_eq(prod_batch_size, 500) // 生产环境批次大小应该更大
  assert_eq(prod_metrics_endpoint, "http://prometheus-prod:9090/metrics")
  
  // 渲染开发配置
  let dev_config = azimuth::config::render_template(config_manager, "development", template_vars)
  
  // 验证开发配置
  let dev_sampling_rate = azimuth::config::get_float_from_config(dev_config, "telemetry.tracing.sampling_rate")
  let dev_batch_size = azimuth::config::get_int_from_config(dev_config, "processing.batch_size")
  let dev_log_level = azimuth::config::get_string_from_config(dev_config, "telemetry.logging.level")
  
  assert_eq(dev_sampling_rate, 1.0) // 开发环境应该全采样
  assert_eq(dev_batch_size, 10) // 开发环境批次大小应该更小
  assert_eq(dev_log_level, "debug") // 开发环境日志级别应该是debug
}

// Test 8: 配置加密和安全
test "configuration encryption and security" {
  // 创建配置管理器
  let config_manager = azimuth::config::Manager::new()
  
  // 配置加密
  let encryption_key = azimuth::crypto::generate_key("AES-256")
  azimuth::config::set_encryption_key(config_manager, encryption_key)
  azimuth::config::enable_encryption(config_manager, true)
  
  // 标记敏感字段
  azimuth::config::mark_sensitive(config_manager, "telemetry.exporter.api_key")
  azimuth::config::mark_sensitive(config_manager, "database.password")
  azimuth::config::mark_sensitive(config_manager, "auth.secret")
  
  // 创建包含敏感信息的配置
  let config_with_secrets = """
  {
    "telemetry": {
      "exporter": {
        "api_key": "secret-api-key-12345"
      }
    },
    "database": {
      "host": "localhost",
      "password": "secret-database-password"
    },
    "auth": {
      "secret": "secret-auth-token"
    }
  }
  """
  
  // 加密配置
  let encrypted_config = azimuth::config::encrypt_config(config_manager, config_with_secrets)
  
  // 验证加密结果
  assert_not_eq(encrypted_config, config_with_secrets)
  assert_false(encrypted_config.contains("secret-api-key-12345"))
  assert_false(encrypted_config.contains("secret-database-password"))
  assert_false(encrypted_config.contains("secret-auth-token"))
  
  // 解密配置
  let decrypted_config = azimuth::config::decrypt_config(config_manager, encrypted_config)
  
  // 验证解密结果
  assert_eq(decrypted_config, config_with_secrets)
  
  // 加载加密配置
  let config_file = azimuth::fs::create_temp_file("encrypted_config.json", encrypted_config)
  let load_result = azimuth::config::load_from_file(config_manager, config_file)
  
  // 验证加载结果
  assert_true(azimuth::config::is_successful(load_result))
  
  // 验证敏感字段可以正确访问
  let api_key = azimuth::config::get_string(config_manager, "telemetry.exporter.api_key")
  let db_password = azimuth::config::get_string(config_manager, "database.password")
  let auth_secret = azimuth::config::get_string(config_manager, "auth.secret")
  
  assert_eq(api_key, "secret-api-key-12345")
  assert_eq(db_password, "secret-database-password")
  assert_eq(auth_secret, "secret-auth-token")
  
  // 验证敏感字段在日志中被遮蔽
  let config_dump = azimuth::config::dump_config(config_manager, true) // 安全模式
  assert_false(config_dump.contains("secret-api-key-12345"))
  assert_true(config_dump.contains("***REDACTED***"))
  
  // 清理临时文件
  azimuth::fs::delete_file(config_file)
}