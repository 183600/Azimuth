// Azimuth 遥测系统配置管理测试
// 测试遥测系统的配置管理功能

// 测试1: 基本配置管理
test "基本配置管理测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 创建测试配置
  let test_config = {
    telemetry: {
      service_name: "test-service",
      service_version: "1.0.0",
      environment: "test",
      enabled: true,
      sampling: {
        probability: 0.1,
        rate_limiting: {
          max_spans_per_second: 1000
        }
      },
      exporter: {
        type: "otlp",
        endpoint: "http://localhost:4317",
        timeout_ms: 5000,
        retry_policy: {
          max_attempts: 3,
          backoff_ms: 100
        }
      },
      processor: {
        batch: {
          max_queue_size: 2048,
          scheduled_delay_ms: 5000,
          max_export_batch_size: 512
        }
      }
    },
    logging: {
      level: "info",
      format: "json",
      output: "stdout"
    },
    metrics: {
      enabled: true,
      prefix: "azimuth_",
      export_interval_ms: 10000
    }
  }
  
  // 设置配置
  let set_result = ConfigManager::set_config(config_manager, test_config)
  
  // 验证设置结果
  assert_true(set_result.success)
  assert_true(set_result.config_id.length() > 0)
  assert_true(set_result.version > 0)
  
  // 获取配置
  let get_result = ConfigManager::get_config(config_manager, set_result.config_id)
  
  // 验证获取结果
  assert_true(get_result.success)
  assert_eq(get_result.config.telemetry.service_name, "test-service")
  assert_eq(get_result.config.telemetry.service_version, "1.0.0")
  assert_eq(get_result.config.telemetry.environment, "test")
  assert_true(get_result.config.telemetry.enabled)
  assert_eq(get_result.config.telemetry.sampling.probability, 0.1)
  assert_eq(get_result.config.telemetry.exporter.type, "otlp")
  assert_eq(get_result.config.logging.level, "info")
  assert_true(get_result.config.metrics.enabled)
  
  // 获取默认配置
  let default_result = ConfigManager::get_default_config(config_manager)
  
  // 验证默认配置
  assert_true(default_result.success)
  assert_true(default_result.config.telemetry.service_name.length() > 0)
  assert_true(default_result.config.telemetry.enabled)
  
  // 更新配置
  let updated_config = { test_config | 
    telemetry: { test_config.telemetry | 
      service_version: "1.1.0",
      sampling: { test_config.telemetry.sampling | 
        probability: 0.2 
      } 
    } 
  }
  
  let update_result = ConfigManager::update_config(config_manager, set_result.config_id, updated_config)
  
  // 验证更新结果
  assert_true(update_result.success)
  assert_eq(update_result.config_id, set_result.config_id)
  assert_true(update_result.version > set_result.version)
  
  // 验证更新后的配置
  let updated_get_result = ConfigManager::get_config(config_manager, set_result.config_id)
  assert_eq(updated_get_result.config.telemetry.service_version, "1.1.0")
  assert_eq(updated_get_result.config.telemetry.sampling.probability, 0.2)
  
  // 删除配置
  let delete_result = ConfigManager::delete_config(config_manager, set_result.config_id)
  
  // 验证删除结果
  assert_true(delete_result.success)
  
  // 验证配置已删除
  let deleted_get_result = ConfigManager::get_config(config_manager, set_result.config_id)
  assert_false(deleted_get_result.success)
}

// 测试2: 配置验证和模式检查
test "配置验证和模式检查测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 定义配置模式
  let config_schema = {
    type: "object",
    required: ["telemetry"],
    properties: {
      telemetry: {
        type: "object",
        required: ["service_name", "enabled"],
        properties: {
          service_name: {
            type: "string",
            pattern: "^[a-z][a-z0-9-]*$",
            minLength: 1,
            maxLength: 50
          },
          service_version: {
            type: "string",
            pattern: "^\\d+\\.\\d+\\.\\d+$"
          },
          environment: {
            type: "string",
            enum: ["development", "test", "staging", "production"]
          },
          enabled: {
            type: "boolean"
          },
          sampling: {
            type: "object",
            properties: {
              probability: {
                type: "number",
                minimum: 0,
                maximum: 1
              },
              rate_limiting: {
                type: "object",
                properties: {
                  max_spans_per_second: {
                    type: "integer",
                    minimum: 1,
                    maximum: 100000
                  }
                }
              }
            }
          },
          exporter: {
            type: "object",
            required: ["type"],
            properties: {
              type: {
                type: "string",
                enum: ["otlp", "jaeger", "zipkin", "prometheus"]
              },
              endpoint: {
                type: "string",
                format: "uri"
              },
              timeout_ms: {
                type: "integer",
                minimum: 100,
                maximum: 60000
              }
            }
          }
        }
      },
      logging: {
        type: "object",
        properties: {
          level: {
            type: "string",
            enum: ["debug", "info", "warn", "error"]
          },
          format: {
            type: "string",
            enum: ["json", "text"]
          }
        }
      }
    }
  }
  
  // 注册配置模式
  let schema_result = ConfigManager::register_schema(config_manager, "telemetry_config", config_schema)
  
  // 验证模式注册结果
  assert_true(schema_result.success)
  
  // 测试有效配置
  let valid_config = {
    telemetry: {
      service_name: "valid-service",
      service_version: "1.0.0",
      environment: "test",
      enabled: true,
      sampling: {
        probability: 0.5,
        rate_limiting: {
          max_spans_per_second: 1000
        }
      },
      exporter: {
        type: "otlp",
        endpoint: "http://localhost:4317",
        timeout_ms: 5000
      }
    },
    logging: {
      level: "info",
      format: "json"
    }
  }
  
  let valid_validation_result = ConfigManager::validate_config(config_manager, valid_config, "telemetry_config")
  
  // 验证有效配置验证结果
  assert_true(valid_validation_result.valid)
  assert_eq(valid_validation_result.errors.length(), 0)
  
  // 测试无效配置（缺少必填字段）
  let invalid_config_missing = {
    telemetry: {
      service_version: "1.0.0",  // 缺少service_name
      enabled: true
    }
  }
  
  let invalid_validation_result_missing = ConfigManager::validate_config(config_manager, invalid_config_missing, "telemetry_config")
  
  // 验证无效配置验证结果
  assert_false(invalid_validation_result_missing.valid)
  assert_true(invalid_validation_result_missing.errors.length() > 0)
  
  let missing_field_error = invalid_validation_result_missing.errors.find(fn(e) { 
    e.field == "telemetry.service_name" and e.error.contains("required") 
  })
  assert_true(missing_field_error != None)
  
  // 测试无效配置（类型错误）
  let invalid_config_type = {
    telemetry: {
      service_name: 123,  // 应该是字符串
      enabled: true
    }
  }
  
  let invalid_validation_result_type = ConfigManager::validate_config(config_manager, invalid_config_type, "telemetry_config")
  
  // 验证类型错误验证结果
  assert_false(invalid_validation_result_type.valid)
  
  let type_error = invalid_validation_result_type.errors.find(fn(e) { 
    e.field == "telemetry.service_name" and e.error.contains("string") 
  })
  assert_true(type_error != None)
  
  // 测试无效配置（值超出范围）
  let invalid_config_range = {
    telemetry: {
      service_name: "range-test-service",
      enabled: true,
      sampling: {
        probability: 1.5  // 超出0-1范围
      }
    }
  }
  
  let invalid_validation_result_range = ConfigManager::validate_config(config_manager, invalid_config_range, "telemetry_config")
  
  // 验证范围错误验证结果
  assert_false(invalid_validation_result_range.valid)
  
  let range_error = invalid_validation_result_range.errors.find(fn(e) { 
    e.field == "telemetry.sampling.probability" and e.error.contains("maximum") 
  })
  assert_true(range_error != None)
  
  // 测试无效配置（模式不匹配）
  let invalid_config_pattern = {
    telemetry: {
      service_name: "Invalid-Service",  // 包含大写字母和连字符
      enabled: true
    }
  }
  
  let invalid_validation_result_pattern = ConfigManager::validate_config(config_manager, invalid_config_pattern, "telemetry_config")
  
  // 验证模式错误验证结果
  assert_false(invalid_validation_result_pattern.valid)
  
  let pattern_error = invalid_validation_result_pattern.errors.find(fn(e) { 
    e.field == "telemetry.service_name" and e.error.contains("pattern") 
  })
  assert_true(pattern_error != None)
  
  // 测试配置自动修复
  let auto_fix_config = {
    telemetry: {
      service_name: "  fix-service  ",  // 前后有空格
      service_version: "1.0",  // 缺少补丁版本
      enabled: "true",  // 字符串而非布尔值
      sampling: {
        probability: 1.2  // 超出范围
      }
    }
  }
  
  let auto_fix_result = ConfigManager::auto_fix_config(config_manager, auto_fix_config, "telemetry_config")
  
  // 验证自动修复结果
  assert_true(auto_fix_result.success)
  assert_true(auto_fix_result.fixed)
  
  let fixed_config = auto_fix_result.config
  assert_eq(fixed_config.telemetry.service_name, "fix-service")  // 空格被移除
  assert_eq(fixed_config.telemetry.service_version, "1.0.0")  // 补丁版本被添加
  assert_eq(fixed_config.telemetry.enabled, true)  // 字符串被转换为布尔值
  assert_eq(fixed_config.telemetry.sampling.probability, 1.0)  // 超出范围的值被限制为最大值
  
  // 验证修复后的配置有效
  let fixed_validation_result = ConfigManager::validate_config(config_manager, fixed_config, "telemetry_config")
  assert_true(fixed_validation_result.valid)
}

// 测试3: 配置环境变量和模板
test "配置环境变量和模板测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 设置环境变量
  Env::set("AZIMUTH_SERVICE_NAME", "env-service")
  Env::set("AZIMUTH_SERVICE_VERSION", "2.0.0")
  Env::set("AZIMUTH_ENVIRONMENT", "production")
  Env::set("AZIMUTH_ENABLED", "true")
  Env::set("AZIMUTH_SAMPLING_PROBABILITY", "0.3")
  Env::set("AZIMUTH_EXPORTER_ENDPOINT", "http://prod-collector.example.com:4317")
  Env::set("AZIMUTH_LOG_LEVEL", "warn")
  
  // 创建配置模板
  let config_template = {
    telemetry: {
      service_name: "${AZIMUTH_SERVICE_NAME}",
      service_version: "${AZIMUTH_SERVICE_VERSION}",
      environment: "${AZIMUTH_ENVIRONMENT}",
      enabled: "${AZIMUTH_ENABLED}",
      sampling: {
        probability: "${AZIMUTH_SAMPLING_PROBABILITY}",
        rate_limiting: {
          max_spans_per_second: 1000
        }
      },
      exporter: {
        type: "otlp",
        endpoint: "${AZIMUTH_EXPORTER_ENDPOINT}",
        timeout_ms: 5000
      }
    },
    logging: {
      level: "${AZIMUTH_LOG_LEVEL}",
      format: "json"
    }
  }
  
  // 解析配置模板
  let template_result = ConfigManager::parse_template(config_manager, config_template)
  
  // 验证模板解析结果
  assert_true(template_result.success)
  
  let parsed_config = template_result.config
  assert_eq(parsed_config.telemetry.service_name, "env-service")
  assert_eq(parsed_config.telemetry.service_version, "2.0.0")
  assert_eq(parsed_config.telemetry.environment, "production")
  assert_eq(parsed_config.telemetry.enabled, true)
  assert_eq(parsed_config.telemetry.sampling.probability, 0.3)
  assert_eq(parsed_config.telemetry.exporter.endpoint, "http://prod-collector.example.com:4317")
  assert_eq(parsed_config.logging.level, "warn")
  
  // 测试默认值
  Env::unset("AZIMUTH_NONEXISTENT_VAR")
  
  let config_with_defaults = {
    telemetry: {
      service_name: "${AZIMUTH_SERVICE_NAME}",
      service_version: "${AZIMUTH_SERVICE_VERSION}",
      environment: "${AZIMUTH_NONEXISTENT_VAR:development}",  // 默认值development
      enabled: "${AZIMUTH_ENABLED}"
    }
  }
  
  let defaults_result = ConfigManager::parse_template(config_manager, config_with_defaults)
  
  // 验证默认值结果
  assert_true(defaults_result.success)
  assert_eq(defaults_result.config.telemetry.environment, "development")
  
  // 测试条件配置
  Env::set("AZIMUTH_FEATURE_FLAG", "true")
  
  let conditional_config = {
    telemetry: {
      service_name: "${AZIMUTH_SERVICE_NAME}",
      enabled: "${AZIMUTH_ENABLED}",
      advanced_features: "${if AZIMUTH_FEATURE_FLAG:true:false}"
    }
  }
  
  let conditional_result = ConfigManager::parse_template(config_manager, conditional_config)
  
  // 验证条件配置结果
  assert_true(conditional_result.success)
  assert_eq(conditional_result.config.telemetry.advanced_features, true)
  
  // 测试环境变量类型转换
  Env::set("AZIMUTH_TIMEOUT_MS", "10000")
  Env::set("AZIMUTH_BATCH_SIZE", "256")
  Env::set("AZIMUTH_DEBUG_MODE", "false")
  
  let type_conversion_config = {
    telemetry: {
      service_name: "${AZIMUTH_SERVICE_NAME}",
      exporter: {
        timeout_ms: "${AZIMUTH_TIMEOUT_MS:integer}",
        batch_size: "${AZIMUTH_BATCH_SIZE:integer}"
      },
      debug: {
        enabled: "${AZIMUTH_DEBUG_MODE:boolean}"
      }
    }
  }
  
  let type_conversion_result = ConfigManager::parse_template(config_manager, type_conversion_config)
  
  // 验证类型转换结果
  assert_true(type_conversion_result.success)
  assert_eq(type_conversion_result.config.telemetry.exporter.timeout_ms, 10000)
  assert_eq(type_conversion_result.config.telemetry.exporter.batch_size, 256)
  assert_eq(type_conversion_result.config.telemetry.debug.enabled, false)
  
  // 测试配置文件模板
  let config_file_template = {
    telemetry: {
      service_name: "${service.name}",
      service_version: "${service.version}",
      environment: "${deployment.environment}",
      enabled: "${telemetry.enabled:true}",
      exporter: {
        endpoint: "${collector.endpoint:http://localhost:4317}",
        timeout_ms: "${collector.timeout:5000}"
      }
    }
  }
  
  let template_variables = {
    "service.name": "file-service",
    "service.version": "3.0.0",
    "deployment.environment": "staging",
    "telemetry.enabled": "true",
    "collector.endpoint": "http://staging-collector.example.com:4317"
  }
  
  let file_template_result = ConfigManager::parse_template_with_variables(config_manager, config_file_template, template_variables)
  
  // 验证文件模板结果
  assert_true(file_template_result.success)
  
  let file_parsed_config = file_template_result.config
  assert_eq(file_parsed_config.telemetry.service_name, "file-service")
  assert_eq(file_parsed_config.telemetry.service_version, "3.0.0")
  assert_eq(file_parsed_config.telemetry.environment, "staging")
  assert_eq(file_parsed_config.telemetry.enabled, true)
  assert_eq(file_parsed_config.telemetry.exporter.endpoint, "http://staging-collector.example.com:4317")
  assert_eq(file_parsed_config.telemetry.exporter.timeout_ms, 5000)  // 使用默认值
}

// 测试4: 配置热重载
test "配置热重载测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 启用热重载
  let hot_reload_config = {
    enabled: true,
    watch_interval_ms: 1000,
    file_patterns: ["*.yaml", "*.yml", "*.json"],
    exclude_patterns: ["*.tmp", "*.bak"],
    reload_on_change: true,
    validate_on_reload: true,
    backup_on_reload: true
  }
  
  let hot_reload_result = ConfigManager::enable_hot_reload(config_manager, hot_reload_config)
  
  // 验证热重载启用结果
  assert_true(hot_reload_result.success)
  assert_true(hot_reload_result.config_id.length() > 0)
  
  // 创建初始配置文件
  let initial_config_content = {
    telemetry: {
      service_name: "hot-reload-service",
      service_version: "1.0.0",
      environment: "test",
      enabled: true,
      sampling: {
        probability: 0.1
      },
      exporter: {
        type: "otlp",
        endpoint: "http://localhost:4317"
      }
    }
  }
  
  let config_file_path = "/tmp/azimuth_config.yaml"
  File::write(config_file_path, Yaml::serialize(initial_config_content))
  
  // 加载配置文件
  let load_result = ConfigManager::load_from_file(config_manager, config_file_path)
  
  // 验证加载结果
  assert_true(load_result.success)
  assert_eq(load_result.config.telemetry.service_name, "hot-reload-service")
  assert_eq(load_result.config.telemetry.service_version, "1.0.0")
  assert_eq(load_result.config.telemetry.sampling.probability, 0.1)
  
  // 监听配置变化
  let change_listener = ConfigManager::add_change_listener(config_manager, {
    config_id: load_result.config_id,
    callback: fn(old_config, new_config) {
      // 验证配置变化
      assert_eq(new_config.telemetry.service_version, "2.0.0")
      assert_eq(new_config.telemetry.sampling.probability, 0.2)
      return true  // 接受变化
    }
  })
  
  // 修改配置文件
  let updated_config_content = {
    telemetry: {
      service_name: "hot-reload-service",
      service_version: "2.0.0",  // 更新版本
      environment: "test",
      enabled: true,
      sampling: {
        probability: 0.2  // 更新采样率
      },
      exporter: {
        type: "otlp",
        endpoint: "http://localhost:4317"
      }
    }
  }
  
  File::write(config_file_path, Yaml::serialize(updated_config_content))
  
  // 等待热重载
  Time::sleep(2000)
  
  // 验证配置已更新
  let reloaded_result = ConfigManager::get_config(config_manager, load_result.config_id)
  
  assert_eq(reloaded_result.config.telemetry.service_version, "2.0.0")
  assert_eq(reloaded_result.config.telemetry.sampling.probability, 0.2)
  
  // 测试配置回滚
  let rollback_config_content = {
    telemetry: {
      service_name: "hot-reload-service",
      service_version: "1.5.0",  // 中间版本
      environment: "test",
      enabled: true,
      sampling: {
        probability: 0.15
      },
      exporter: {
        type: "otlp",
        endpoint: "http://localhost:4317"
      }
    }
  }
  
  File::write(config_file_path, Yaml::serialize(rollback_config_content))
  
  // 等待热重载
  Time::sleep(2000)
  
  // 验证配置已更新
  let rollback_result = ConfigManager::get_config(config_manager, load_result.config_id)
  
  assert_eq(rollback_result.config.telemetry.service_version, "1.5.0")
  assert_eq(rollback_result.config.telemetry.sampling.probability, 0.15)
  
  // 测试配置回滚到之前版本
  let rollback_history = ConfigManager::get_config_history(config_manager, load_result.config_id)
  
  assert_true(rollback_history.length() >= 2)
  
  // 回滚到第一个版本
  let rollback_to_first_result = ConfigManager::rollback_to_version(config_manager, load_result.config_id, rollback_history[0].version)
  
  // 验证回滚结果
  assert_true(rollback_to_first_result.success)
  
  let rolled_back_result = ConfigManager::get_config(config_manager, load_result.config_id)
  
  assert_eq(rolled_back_result.config.telemetry.service_version, "1.0.0")
  assert_eq(rolled_back_result.config.telemetry.sampling.probability, 0.1)
  
  // 测试配置验证失败时的处理
  let invalid_config_content = {
    telemetry: {
      service_name: "hot-reload-service",
      service_version: "invalid",  // 无效版本格式
      environment: "test",
      enabled: true,
      sampling: {
        probability: 1.5  // 超出范围
      },
      exporter: {
        type: "otlp",
        endpoint: "http://localhost:4317"
      }
    }
  }
  
  File::write(config_file_path, Yaml::serialize(invalid_config_content))
  
  // 等待热重载
  Time::sleep(2000)
  
  // 验证配置未更新（验证失败）
  let validation_failed_result = ConfigManager::get_config(config_manager, load_result.config_id)
  
  assert_eq(validation_failed_result.config.telemetry.service_version, "1.0.0")  // 保持旧版本
  assert_eq(validation_failed_result.config.telemetry.sampling.probability, 0.1)
  
  // 验证错误日志
  let error_logs = ConfigManager::get_reload_errors(config_manager, load_result.config_id)
  assert_true(error_logs.length() > 0)
  
  let validation_error = error_logs.find(fn(log) { 
    log.error.contains("validation") 
  })
  assert_true(validation_error != None)
  
  // 清理
  File::delete(config_file_path)
  ConfigManager::disable_hot_reload(config_manager, load_result.config_id)
}

// 测试5: 配置继承和合并
test "配置继承和合并测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 创建基础配置
  let base_config = {
    telemetry: {
      service_name: "base-service",
      service_version: "1.0.0",
      environment: "development",
      enabled: true,
      sampling: {
        probability: 0.1,
        rate_limiting: {
          max_spans_per_second: 1000
        }
      },
      exporter: {
        type: "otlp",
        endpoint: "http://localhost:4317",
        timeout_ms: 5000,
        retry_policy: {
          max_attempts: 3,
          backoff_ms: 100
        }
      },
      processor: {
        batch: {
          max_queue_size: 2048,
          scheduled_delay_ms: 5000,
          max_export_batch_size: 512
        }
      }
    },
    logging: {
      level: "info",
      format: "json",
      output: "stdout"
    },
    metrics: {
      enabled: true,
      prefix: "azimuth_",
      export_interval_ms: 10000
    }
  }
  
  // 创建环境特定配置
  let prod_config = {
    telemetry: {
      service_version: "1.0.0",
      environment: "production",
      sampling: {
        probability: 0.01,  // 生产环境更低的采样率
        rate_limiting: {
          max_spans_per_second: 5000  // 生产环境更高的限制
        }
      },
      exporter: {
        endpoint: "http://prod-collector.example.com:4317",
        timeout_ms: 10000  // 生产环境更长的超时
      },
      processor: {
        batch: {
          max_queue_size: 4096,  // 生产环境更大的队列
          scheduled_delay_ms: 3000  // 生产环境更短的延迟
        }
      }
    },
    logging: {
      level: "warn",  // 生产环境更高的日志级别
      output: "file"
    },
    metrics: {
      export_interval_ms: 30000  // 生产环境更长的导出间隔
    }
  }
  
  // 创建服务特定配置
  let service_config = {
    telemetry: {
      service_name: "payment-service",  // 覆盖服务名
      sampling: {
        probability: 0.05  // 服务特定的采样率
      },
      exporter: {
        headers: {
          "x-service-token": "payment-service-token"
        }
      },
      resource: {
        "service.instance.id": "payment-service-001",
        "service.namespace": "payment"
      }
    },
    logging: {
      level: "debug"  // 服务特定的日志级别
    }
  }
  
  // 合并配置（基础配置 + 生产环境配置）
  let prod_merge_result = ConfigManager::merge_configs(config_manager, [base_config, prod_config])
  
  // 验证生产环境合并结果
  assert_true(prod_merge_result.success)
  
  let prod_merged = prod_merge_result.config
  assert_eq(prod_merged.telemetry.service_name, "base-service")  // 来自基础配置
  assert_eq(prod_merged.telemetry.service_version, "1.0.0")     // 来自生产配置
  assert_eq(prod_merged.telemetry.environment, "production")    // 来自生产配置
  assert_eq(prod_merged.telemetry.sampling.probability, 0.01)   // 来自生产配置
  assert_eq(prod_merged.telemetry.sampling.rate_limiting.max_spans_per_second, 5000)  // 来自生产配置
  assert_eq(prod_merged.telemetry.exporter.type, "otlp")       // 来自基础配置
  assert_eq(prod_merged.telemetry.exporter.endpoint, "http://prod-collector.example.com:4317")  // 来自生产配置
  assert_eq(prod_merged.telemetry.exporter.timeout_ms, 10000)   // 来自生产配置
  assert_eq(prod_merged.telemetry.exporter.retry_policy.max_attempts, 3)  // 来自基础配置
  assert_eq(prod_merged.telemetry.processor.batch.max_queue_size, 4096)  // 来自生产配置
  assert_eq(prod_merged.telemetry.processor.batch.scheduled_delay_ms, 3000)  // 来自生产配置
  assert_eq(prod_merged.logging.level, "warn")                 // 来自生产配置
  assert_eq(prod_merged.logging.format, "json")                // 来自基础配置
  assert_eq(prod_merged.logging.output, "file")                // 来自生产配置
  assert_eq(prod_merged.metrics.enabled, true)                 // 来自基础配置
  assert_eq(prod_merged.metrics.prefix, "azimuth_")            // 来自基础配置
  assert_eq(prod_merged.metrics.export_interval_ms, 30000)     // 来自生产配置
  
  // 合并配置（基础配置 + 生产环境配置 + 服务特定配置）
  let service_merge_result = ConfigManager::merge_configs(config_manager, [base_config, prod_config, service_config])
  
  // 验证服务合并结果
  assert_true(service_merge_result.success)
  
  let service_merged = service_merge_result.config
  assert_eq(service_merged.telemetry.service_name, "payment-service")  // 来自服务配置
  assert_eq(service_merged.telemetry.service_version, "1.0.0")           // 来自生产配置
  assert_eq(service_merged.telemetry.environment, "production")          // 来自生产配置
  assert_eq(service_merged.telemetry.sampling.probability, 0.05)         // 来自服务配置
  assert_eq(service_merged.telemetry.sampling.rate_limiting.max_spans_per_second, 5000)  // 来自生产配置
  assert_eq(service_merged.telemetry.exporter.type, "otlp")              // 来自基础配置
  assert_eq(service_merged.telemetry.exporter.endpoint, "http://prod-collector.example.com:4317")  // 来自生产配置
  assert_eq(service_merged.telemetry.exporter.timeout_ms, 10000)         // 来自生产配置
  assert_true(service_merged.telemetry.exporter.headers.contains_key("x-service-token"))  // 来自服务配置
  assert_eq(service_merged.telemetry.resource.get("service.instance.id"), Some("payment-service-001"))  // 来自服务配置
  assert_eq(service_merged.telemetry.resource.get("service.namespace"), Some("payment"))  // 来自服务配置
  assert_eq(service_merged.logging.level, "debug")                     // 来自服务配置
  assert_eq(service_merged.logging.format, "json")                    // 来自基础配置
  assert_eq(service_merged.logging.output, "file")                     // 来自生产配置
  
  // 测试配置继承
  let inheritance_config = {
    extends: "base_config",
    telemetry: {
      service_name: "child-service",
      sampling: {
        probability: 0.2
      }
    }
  }
  
  // 注册基础配置
  let base_register_result = ConfigManager::register_config(config_manager, "base_config", base_config)
  assert_true(base_register_result.success)
  
  // 解析继承配置
  let inheritance_result = ConfigManager::resolve_inheritance(config_manager, inheritance_config)
  
  // 验证继承结果
  assert_true(inheritance_result.success)
  
  let inherited = inheritance_result.config
  assert_eq(inherited.telemetry.service_name, "child-service")  // 覆盖值
  assert_eq(inherited.telemetry.service_version, "1.0.0")      // 继承值
  assert_eq(inherited.telemetry.environment, "development")     // 继承值
  assert_eq(inherited.telemetry.sampling.probability, 0.2)      // 覆盖值
  assert_eq(inherited.telemetry.sampling.rate_limiting.max_spans_per_second, 1000)  // 继承值
  
  // 测试深度合并
  let deep_merge_config1 = {
    telemetry: {
      exporter: {
        type: "otlp",
        endpoint: "http://localhost:4317",
        headers: {
          "x-api-key": "key1"
        }
      }
    }
  }
  
  let deep_merge_config2 = {
    telemetry: {
      exporter: {
        timeout_ms: 5000,
        headers: {
          "x-service": "service1"
        }
      }
    }
  }
  
  let deep_merge_result = ConfigManager::deep_merge(config_manager, deep_merge_config1, deep_merge_config2)
  
  // 验证深度合并结果
  assert_true(deep_merge_result.success)
  
  let deep_merged = deep_merge_result.config
  assert_eq(deep_merged.telemetry.exporter.type, "otlp")      // 来自第一个配置
  assert_eq(deep_merged.telemetry.exporter.endpoint, "http://localhost:4317")  // 来自第一个配置
  assert_eq(deep_merged.telemetry.exporter.timeout_ms, 5000)  // 来自第二个配置
  assert_eq(deep_merged.telemetry.exporter.headers.get("x-api-key"), Some("key1"))  // 来自第一个配置
  assert_eq(deep_merged.telemetry.exporter.headers.get("x-service"), Some("service1"))  // 来自第二个配置
}

// 测试6: 配置加密和安全
test "配置加密和安全测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 生成加密密钥
  let encryption_key = ConfigManager::generate_encryption_key(config_manager, {
    algorithm: "AES-256",
    source: "random"
  })
  
  assert_true(encryption_key.length() == 32)  // 256位密钥
  
  // 创建包含敏感信息的配置
  let sensitive_config = {
    telemetry: {
      service_name: "secure-service",
      enabled: true,
      exporter: {
        type: "otlp",
        endpoint: "http://collector.example.com:4317",
        headers: {
          "authorization": "Bearer secret-token-12345",
          "x-api-key": "api-key-67890"
        }
      }
    },
    database: {
      host: "db.example.com",
      port: 5432,
      username: "telemetry_user",
      password: "super_secret_password",
      ssl: {
        cert_path: "/path/to/cert.pem",
        key_path: "/path/to/key.pem",
        passphrase: "cert_passphrase"
      }
    },
    secrets: {
      jwt_secret: "jwt-secret-key",
      encryption_key: "data-encryption-key"
    }
  }
  
  // 配置加密规则
  let encryption_rules = [
    {
      path: "telemetry.exporter.headers.authorization",
      type: "encrypt",
      algorithm: "AES-256-GCM"
    },
    {
      path: "telemetry.exporter.headers.x-api-key",
      type: "encrypt",
      algorithm: "AES-256-GCM"
    },
    {
      path: "database.password",
      type: "encrypt",
      algorithm: "AES-256-GCM"
    },
    {
      path: "database.ssl.passphrase",
      type: "encrypt",
      algorithm: "AES-256-GCM"
    },
    {
      path: "secrets.jwt_secret",
      type: "encrypt",
      algorithm: "AES-256-GCM"
    },
    {
      path: "secrets.encryption_key",
      type: "encrypt",
      algorithm: "AES-256-GCM"
    }
  ]
  
  // 加密配置
  let encrypt_result = ConfigManager::encrypt_config(config_manager, sensitive_config, encryption_rules, encryption_key)
  
  // 验证加密结果
  assert_true(encrypt_result.success)
  
  let encrypted_config = encrypt_result.config
  
  // 验证敏感字段已加密
  assert_true(encrypted_config.telemetry.exporter.headers.authorization.starts_with("enc:"))
  assert_true(encrypted_config.telemetry.exporter.headers.get("x-api-key").unwrap().starts_with("enc:"))
  assert_true(encrypted_config.database.password.starts_with("enc:"))
  assert_true(encrypted_config.database.ssl.passphrase.starts_with("enc:"))
  assert_true(encrypted_config.secrets.jwt_secret.starts_with("enc:"))
  assert_true(encrypted_config.secrets.encryption_key.starts_with("enc:"))
  
  // 验证非敏感字段未加密
  assert_eq(encrypted_config.telemetry.service_name, "secure-service")
  assert_eq(encrypted_config.telemetry.exporter.type, "otlp")
  assert_eq(encrypted_config.database.host, "db.example.com")
  assert_eq(encrypted_config.database.username, "telemetry_user")
  
  // 解密配置
  let decrypt_result = ConfigManager::decrypt_config(config_manager, encrypted_config, encryption_key)
  
  // 验证解密结果
  assert_true(decrypt_result.success)
  
  let decrypted_config = decrypt_result.config
  
  // 验证解密后的配置与原始配置一致
  assert_eq(decrypted_config.telemetry.service_name, sensitive_config.telemetry.service_name)
  assert_eq(decrypted_config.telemetry.exporter.headers.authorization, sensitive_config.telemetry.exporter.headers.authorization)
  assert_eq(decrypted_config.telemetry.exporter.headers.get("x-api-key"), sensitive_config.telemetry.exporter.headers.get("x-api-key"))
  assert_eq(decrypted_config.database.password, sensitive_config.database.password)
  assert_eq(decrypted_config.database.ssl.passphrase, sensitive_config.database.ssl.passphrase)
  assert_eq(decrypted_config.secrets.jwt_secret, sensitive_config.secrets.jwt_secret)
  assert_eq(decrypted_config.secrets.encryption_key, sensitive_config.secrets.encryption_key)
  
  // 测试错误的密钥无法解密
  let wrong_key = "wrong-key-with-32-bytes-length-!!"
  let wrong_decrypt_result = ConfigManager::decrypt_config(config_manager, encrypted_config, wrong_key)
  
  // 验证错误密钥解密失败
  assert_false(wrong_decrypt_result.success)
  
  // 测试配置访问控制
  let access_policies = [
    {
      name: "admin_access",
      roles: ["admin"],
      permissions: ["read", "write", "decrypt"],
      field_patterns: ["*"]  // 所有字段
    },
    {
      name: "operator_access",
      roles: ["operator"],
      permissions: ["read", "write"],
      field_patterns: [
        "telemetry.*",
        "database.host",
        "database.port",
        "database.username"
      ]  // 排除敏感字段
    },
    {
      name: "viewer_access",
      roles: ["viewer"],
      permissions: ["read"],
      field_patterns: [
        "telemetry.service_name",
        "telemetry.enabled",
        "telemetry.exporter.type",
        "telemetry.exporter.endpoint"
      ]  // 只能查看非敏感字段
    }
  ]
  
  // 注册访问策略
  for policy in access_policies {
    let policy_result = ConfigManager::register_access_policy(config_manager, policy)
    assert_true(policy_result.success)
  }
  
  // 测试管理员访问
  let admin_access_result = ConfigManager::check_access(config_manager, {
    config_id: encrypt_result.config_id,
    role: "admin",
    permission: "decrypt",
    field: "database.password"
  })
  
  // 验证管理员访问
  assert_true(admin_access_result.allowed)
  
  // 测试操作员访问
  let operator_access_result = ConfigManager::check_access(config_manager, {
    config_id: encrypt_result.config_id,
    role: "operator",
    permission: "read",
    field: "database.password"
  })
  
  // 验证操作员无法访问敏感字段
  assert_false(operator_access_result.allowed)
  
  let operator_safe_access_result = ConfigManager::check_access(config_manager, {
    config_id: encrypt_result.config_id,
    role: "operator",
    permission: "read",
    field: "database.host"
  })
  
  // 验证操作员可以访问非敏感字段
  assert_true(operator_safe_access_result.allowed)
  
  // 测试查看者访问
  let viewer_access_result = ConfigManager::check_access(config_manager, {
    config_id: encrypt_result.config_id,
    role: "viewer",
    permission: "read",
    field: "database.host"
  })
  
  // 验证查看者无法访问未授权字段
  assert_false(viewer_access_result.allowed)
  
  // 测试配置审计日志
  let audit_logs = ConfigManager::get_access_logs(config_manager, {
    config_id: encrypt_result.config_id,
    start_time: Time::now() - 3600000,  // 最近1小时
    end_time: Time::now()
  })
  
  // 验证审计日志
  assert_true(audit_logs.length() > 0)
  
  // 验证审计日志包含访问信息
  let admin_log = audit_logs.find(fn(log) { 
    log.role == "admin" and log.permission == "decrypt" 
  })
  assert_true(admin_log != None)
  
  let operator_log = audit_logs.find(fn(log) { 
    log.role == "operator" and log.field == "database.password" 
  })
  assert_true(operator_log != None)
  match operator_log {
    Some(log) => assert_eq(log.result, "denied"),
    None => assert_true(false)
  }
}