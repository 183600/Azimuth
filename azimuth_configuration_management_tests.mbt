// Azimuth Configuration Management Tests
// 测试配置管理、动态更新和验证功能

test "配置加载和解析" {
  // 测试配置加载和解析功能
  let config_content = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "version": "2.1.0",
      "environment": "production"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1,
      "batch_size": 100,
      "export_interval": 5000
    },
    "endpoints": {
      "collector": "https://collector.example.com:4317",
      "api": "https://api.example.com/v1"
    },
    "features": {
      "metrics": true,
      "tracing": true,
      "logging": true,
      "profiling": false
    },
    "limits": {
      "max_spans_per_second": 1000,
      "max_attributes_per_span": 128,
      "max_events_per_span": 128,
      "max_links_per_span": 128
    }
  }
  """
  
  // 解析配置
  let config = parse_configuration(config_content)
  
  // 验证服务配置
  assert_eq(config.service.name, "azimuth-telemetry")
  assert_eq(config.service.version, "2.1.0")
  assert_eq(config.service.environment, "production")
  
  // 验证遥测配置
  assert_true(config.telemetry.enabled)
  assert_eq(config.telemetry.sampling_rate, 0.1)
  assert_eq(config.telemetry.batch_size, 100)
  assert_eq(config.telemetry.export_interval, 5000)
  
  // 验证端点配置
  assert_eq(config.endpoints.collector, "https://collector.example.com:4317")
  assert_eq(config.endpoints.api, "https://api.example.com/v1")
  
  // 验证功能开关
  assert_true(config.features.metrics)
  assert_true(config.features.tracing)
  assert_true(config.features.logging)
  assert_false(config.features.profiling)
  
  // 验证限制配置
  assert_eq(config.limits.max_spans_per_second, 1000)
  assert_eq(config.limits.max_attributes_per_span, 128)
  assert_eq(config.limits.max_events_per_span, 128)
  assert_eq(config.limits.max_links_per_span, 128)
}

test "配置验证和默认值" {
  // 测试配置验证和默认值处理
  let incomplete_config = """
  {
    "service": {
      "name": "test-service"
    },
    "telemetry": {
      "enabled": true
    }
  }
  """
  
  // 解析不完整配置
  let config = parse_configuration(incomplete_config)
  
  // 验证默认值应用
  assert_eq(config.service.name, "test-service")
  assert_eq(config.service.version, "1.0.0")  // 默认版本
  assert_eq(config.service.environment, "development")  // 默认环境
  
  assert_true(config.telemetry.enabled)
  assert_eq(config.telemetry.sampling_rate, 1.0)  // 默认采样率
  assert_eq(config.telemetry.batch_size, 50)     // 默认批量大小
  assert_eq(config.telemetry.export_interval, 10000)  // 默认导出间隔
  
  // 测试配置验证
  let validation_result = validate_configuration(config)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // 测试无效配置
  let invalid_config = """
  {
    "service": {
      "name": ""
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 1.5  // 无效：应小于等于1.0
    }
  }
  """
  
  let invalid_parsed = parse_configuration(invalid_config)
  let invalid_validation = validate_configuration(invalid_parsed)
  assert_false(invalid_validation.is_valid)
  assert_true(invalid_validation.errors.length() > 0)
  
  // 验证具体错误
  let has_service_name_error = invalid_validation.errors.any(fn(err) {
    err.contains("service.name") && err.contains("empty")
  })
  let has_sampling_rate_error = invalid_validation.errors.any(fn(err) {
    err.contains("sampling_rate") && err.contains("range")
  })
  
  assert_true(has_service_name_error)
  assert_true(has_sampling_rate_error)
}

test "动态配置更新" {
  // 测试动态配置更新功能
  let initial_config = """
  {
    "service": {
      "name": "dynamic-service",
      "version": "1.0.0"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.5,
      "batch_size": 50
    }
  }
  """
  
  let config_manager = ConfigurationManager::new(initial_config)
  
  // 获取初始配置
  let current_config = ConfigurationManager::get_config(config_manager)
  assert_eq(current_config.telemetry.sampling_rate, 0.5)
  assert_eq(current_config.telemetry.batch_size, 50)
  
  // 测试部分更新
  let update_patch = """
  {
    "telemetry": {
      "sampling_rate": 0.2,
      "batch_size": 100
    }
  }
  """
  
  let update_result = ConfigurationManager::update_config(config_manager, update_patch)
  assert_true(update_result.success)
  
  // 验证更新后的配置
  let updated_config = ConfigurationManager::get_config(config_manager)
  assert_eq(updated_config.service.name, "dynamic-service")  // 未改变
  assert_eq(updated_config.telemetry.sampling_rate, 0.2)     // 已更新
  assert_eq(updated_config.telemetry.batch_size, 100)        // 已更新
  assert_true(updated_config.telemetry.enabled)              // 未改变
  
  // 测试配置变更通知
  let notification_received = ref false
  let updated_sampling_rate = ref 0.0
  
  ConfigurationManager::subscribe(config_manager, fn(change) {
    if change.path == "telemetry.sampling_rate" {
      updated_sampling_rate := change.new_value
      notification_received := true
    }
  })
  
  // 再次更新配置
  let second_update = """
  {
    "telemetry": {
      "sampling_rate": 0.8
    }
  }
  """
  
  ConfigurationManager::update_config(config_manager, second_update)
  
  // 验证通知
  assert_true(notification_received)
  assert_eq(updated_sampling_rate, 0.8)
}

test "配置环境变量覆盖" {
  // 测试环境变量覆盖配置
  let base_config = """
  {
    "service": {
      "name": "env-service",
      "environment": "development"
    },
    "database": {
      "host": "localhost",
      "port": 5432,
      "username": "admin"
    }
  }
  """
  
  // 设置环境变量
  set_environment_variable("AZIMUTH_SERVICE_ENVIRONMENT", "production")
  set_environment_variable("AZIMUTH_DATABASE_HOST", "prod-db.example.com")
  set_environment_variable("AZIMUTH_DATABASE_PORT", "5433")
  
  // 应用环境变量覆盖
  let config_with_env = apply_environment_overrides(base_config)
  
  // 验证覆盖效果
  assert_eq(config_with_env.service.name, "env-service")        // 未覆盖
  assert_eq(config_with_env.service.environment, "production")  // 已覆盖
  assert_eq(config_with_env.database.host, "prod-db.example.com") // 已覆盖
  assert_eq(config_with_env.database.port, 5433)                 // 已覆盖
  assert_eq(config_with_env.database.username, "admin")          // 未覆盖
  
  // 测试环境变量优先级
  set_environment_variable("AZIMUTH_SERVICE_NAME", "override-name")
  let config_with_name_override = apply_environment_overrides(base_config)
  assert_eq(config_with_name_override.service.name, "override-name")
  
  // 清理环境变量
  clear_environment_variable("AZIMUTH_SERVICE_ENVIRONMENT")
  clear_environment_variable("AZIMUTH_DATABASE_HOST")
  clear_environment_variable("AZIMUTH_DATABASE_PORT")
  clear_environment_variable("AZIMUTH_SERVICE_NAME")
}

test "配置模板和继承" {
  // 测试配置模板和继承功能
  let base_template = """
  {
    "service": {
      "name": "${SERVICE_NAME}",
      "version": "${SERVICE_VERSION:1.0.0}",
      "environment": "${ENVIRONMENT:development}"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1,
      "batch_size": 50
    },
    "logging": {
      "level": "INFO",
      "format": "json"
    }
  }
  """
  
  // 设置模板变量
  let template_vars = {
    "SERVICE_NAME": "template-service",
    "SERVICE_VERSION": "2.0.0"
    // ENVIRONMENT 使用默认值
  }
  
  // 渲染模板
  let rendered_config = render_config_template(base_template, template_vars)
  let parsed_config = parse_configuration(rendered_config)
  
  // 验证模板渲染
  assert_eq(parsed_config.service.name, "template-service")
  assert_eq(parsed_config.service.version, "2.0.0")
  assert_eq(parsed_config.service.environment, "development")  // 默认值
  assert_eq(parsed_config.telemetry.sampling_rate, 0.1)
  assert_eq(parsed_config.logging.level, "INFO")
  
  // 测试配置继承
  let parent_config = """
  {
    "service": {
      "name": "parent-service",
      "version": "1.0.0",
      "description": "Parent service configuration"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1
    },
    "common": {
      "timeout": 30000,
      "retries": 3
    }
  }
  """
  
  let child_config = """
  {
    "extends": "parent_config",
    "service": {
      "name": "child-service"
    },
    "telemetry": {
      "sampling_rate": 0.2
    },
    "child_specific": {
      "feature": true
    }
  }
  """
  
  // 处理继承
  let merged_config = process_config_inheritance(child_config, parent_config)
  
  // 验证继承结果
  assert_eq(merged_config.service.name, "child-service")        // 子配置覆盖
  assert_eq(merged_config.service.version, "1.0.0")             // 继承自父配置
  assert_eq(merged_config.service.description, "Parent service configuration") // 继承自父配置
  assert_true(merged_config.telemetry.enabled)                  // 继承自父配置
  assert_eq(merged_config.telemetry.sampling_rate, 0.2)         // 子配置覆盖
  assert_eq(merged_config.common.timeout, 30000)                // 继承自父配置
  assert_eq(merged_config.common.retries, 3)                    // 继承自父配置
  assert_true(merged_config.child_specific.feature)             // 子配置特有
}

test "配置加密和敏感信息" {
  // 测试配置加密和敏感信息处理
  let sensitive_config = """
  {
    "service": {
      "name": "secure-service"
    },
    "database": {
      "host": "db.example.com",
      "username": "admin",
      "password": "secret-password",
      "api_key": "sk-1234567890abcdef"
    },
    "external_apis": {
      "payment_provider": {
        "secret_key": "pk_live_1234567890",
        "webhook_secret": "whsec_1234567890"
      }
    }
  }
  """
  
  // 识别敏感字段
  let sensitive_fields = identify_sensitive_fields(sensitive_config)
  assert_true(sensitive_fields.contains("database.password"))
  assert_true(sensitive_fields.contains("database.api_key"))
  assert_true(sensitive_fields.contains("external_apis.payment_provider.secret_key"))
  assert_true(sensitive_fields.contains("external_apis.payment_provider.webhook_secret"))
  assert_false(sensitive_fields.contains("database.host"))
  assert_false(sensitive_fields.contains("service.name"))
  
  // 加密敏感配置
  let encryption_key = generate_encryption_key()
  let encrypted_config = encrypt_sensitive_fields(sensitive_config, sensitive_fields, encryption_key)
  
  // 验证敏感字段已加密
  assert_false(encrypted_config.contains("secret-password"))
  assert_false(encrypted_config.contains("sk-1234567890abcdef"))
  assert_true(encrypted_config.contains("encrypted:"))
  
  // 解密配置
  let decrypted_config = decrypt_sensitive_fields(encrypted_config, encryption_key)
  assert_true(decrypted_config.contains("secret-password"))
  assert_true(decrypted_config.contains("sk-1234567890abcdef"))
  
  // 测试安全配置导出
  let safe_export = export_safe_config(decrypted_config)
  assert_false(safe_export.contains("secret-password"))
  assert_false(safe_export.contains("sk-1234567890abcdef"))
  assert_true(safe_export.contains("******"))
  assert_true(safe_export.contains("db.example.com"))
}