// 配置管理测试用例

test "basic_configuration_loading" {
  // 基本配置加载测试
  
  // 创建配置管理器
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 从字符串加载配置
  let config_json = """{
    "service": {
      "name": "test-service",
      "version": "1.0.0",
      "instance_id": "instance-001"
    },
    "tracing": {
      "enabled": true,
      "sampling": {
        "type": "trace_id_ratio",
        "rate": 0.1
      },
      "exporter": {
        "type": "console",
        "batch_size": 100,
        "timeout": 5000
      }
    },
    "metrics": {
      "enabled": true,
      "exporter": {
        "type": "otlp_http",
        "endpoint": "http://localhost:4318/v1/metrics",
        "headers": {
          "Authorization": "Bearer token123"
        }
      }
    },
    "logging": {
      "enabled": true,
      "level": "INFO",
      "exporter": {
        "type": "console"
      }
    }
  }"""
  
  let config = config_manager.load_from_string(config_json)
  
  // 验证配置加载
  assert_eq(config.service.name, "test-service")
  assert_eq(config.service.version, "1.0.0")
  assert_eq(config.service.instance_id, "instance-001")
  
  assert_eq(config.tracing.enabled, true)
  assert_eq(config.tracing.sampling.type, "trace_id_ratio")
  assert_eq(config.tracing.sampling.rate, 0.1)
  assert_eq(config.tracing.exporter.type, "console")
  assert_eq(config.tracing.exporter.batch_size, 100)
  assert_eq(config.tracing.exporter.timeout, 5000)
  
  assert_eq(config.metrics.enabled, true)
  assert_eq(config.metrics.exporter.type, "otlp_http")
  assert_eq(config.metrics.exporter.endpoint, "http://localhost:4318/v1/metrics")
  
  assert_eq(config.logging.enabled, true)
  assert_eq(config.logging.level, "INFO")
  assert_eq(config.logging.exporter.type, "console")
}

test "environment_variable_configuration" {
  // 环境变量配置测试
  
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 设置环境变量
  azimuth::telemetry::sdk::platform::env::set_var("OTEL_SERVICE_NAME", "env-service")
  azimuth::telemetry::sdk::platform::env::set_var("OTEL_SERVICE_VERSION", "2.1.0")
  azimuth::telemetry::sdk::platform::env::set_var("OTEL_TRACES_SAMPLER", "always_on")
  azimuth::telemetry::sdk::platform::env::set_var("OTEL_TRACES_SAMPLER_ARG", "")
  azimuth::telemetry::sdk::platform::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", "http://otel-collector:4317")
  azimuth::telemetry::sdk::platform::env::set_var("OTEL_LOG_LEVEL", "WARN")
  
  // 从环境变量加载配置
  let config = config_manager.load_from_env()
  
  // 验证环境变量配置
  assert_eq(config.service.name, "env-service")
  assert_eq(config.service.version, "2.1.0")
  assert_eq(config.tracing.sampling.type, "always_on")
  assert_eq(config.tracing.exporter.type, "otlp_http")
  assert_eq(config.tracing.exporter.endpoint, "http://otel-collector:4317")
  assert_eq(config.logging.level, "WARN")
  
  // 清理环境变量
  azimuth::telemetry::sdk::platform::env::remove_var("OTEL_SERVICE_NAME")
  azimuth::telemetry::sdk::platform::env::remove_var("OTEL_SERVICE_VERSION")
  azimuth::telemetry::sdk::platform::env::remove_var("OTEL_TRACES_SAMPLER")
  azimuth::telemetry::sdk::platform::env::remove_var("OTEL_TRACES_SAMPLER_ARG")
  azimuth::telemetry::sdk::platform::env::remove_var("OTEL_EXPORTER_OTLP_ENDPOINT")
  azimuth::telemetry::sdk::platform::env::remove_var("OTEL_LOG_LEVEL")
}

test "configuration_file_loading" {
  // 配置文件加载测试
  
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 创建临时配置文件
  let config_content = """{
    "service": {
      "name": "file-service",
      "version": "1.5.2",
      "namespace": "production"
    },
    "resource": {
      "attributes": {
        "host.name": "prod-server-01",
        "cloud.provider": "aws",
        "cloud.region": "us-west-2"
      }
    },
    "tracing": {
      "enabled": true,
      "propagators": ["tracecontext", "baggage"],
      "sampling": {
        "type": "parent_based",
        "root": {
          "type": "trace_id_ratio",
          "rate": 0.2
        }
      },
      "exporter": {
        "type": "otlp_http",
        "endpoint": "https://otel.example.com:4318",
        "headers": {
          "x-api-key": "api-key-123",
          "x-service-name": "file-service"
        },
        "compression": "gzip",
        "timeout": 10000
      }
    },
    "metrics": {
      "enabled": true,
      "readers": [
        {
          "type": "periodic",
          "interval": 30000,
          "exporter": {
            "type": "otlp_http",
            "endpoint": "https://otel.example.com:4318/v1/metrics"
          }
        }
      ]
    },
    "logging": {
      "enabled": true,
      "level": "DEBUG",
      "exporter": {
        "type": "otlp_http",
        "endpoint": "https://otel.example.com:4318/v1/logs"
      }
    }
  }"""
  
  let temp_file = "/tmp/telemetry_config.json"
  azimuth::telemetry::sdk::platform::fs::write_file(temp_file, config_content)
  
  // 从文件加载配置
  let config = config_manager.load_from_file(temp_file)
  
  // 验证文件配置
  assert_eq(config.service.name, "file-service")
  assert_eq(config.service.version, "1.5.2")
  assert_eq(config.service.namespace, "production")
  
  assert_eq(config.resource.attributes.get("host.name").unwrap().to_string(), "prod-server-01")
  assert_eq(config.resource.attributes.get("cloud.provider").unwrap().to_string(), "aws")
  assert_eq(config.resource.attributes.get("cloud.region").unwrap().to_string(), "us-west-2")
  
  assert_eq(config.tracing.propagators.length(), 2)
  assert_eq(config.tracing.propagators[0], "tracecontext")
  assert_eq(config.tracing.propagators[1], "baggage")
  
  assert_eq(config.tracing.sampling.type, "parent_based")
  assert_eq(config.tracing.sampling.root.type, "trace_id_ratio")
  assert_eq(config.tracing.sampling.root.rate, 0.2)
  
  assert_eq(config.tracing.exporter.compression, "gzip")
  assert_eq(config.tracing.exporter.timeout, 10000)
  
  assert_eq(config.metrics.readers.length(), 1)
  assert_eq(config.metrics.readers[0].type, "periodic")
  assert_eq(config.metrics.readers[0].interval, 30000)
  
  assert_eq(config.logging.level, "DEBUG")
  
  // 清理临时文件
  azimuth::telemetry::sdk::platform::fs::remove_file(temp_file)
}

test "configuration_validation" {
  // 配置验证测试
  
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 测试无效配置
  let invalid_config_json = """{
    "service": {
      "name": "",  // 空名称应该无效
      "version": "invalid.version"  // 无效版本格式
    },
    "tracing": {
      "sampling": {
        "type": "invalid_sampler",  // 无效采样器类型
        "rate": 1.5  // 无效采样率（大于1）
      },
      "exporter": {
        "type": "invalid_exporter",  // 无效导出器类型
        "timeout": -1000  // 无效超时值
      }
    },
    "metrics": {
      "readers": [
        {
          "type": "periodic",
          "interval": -5000  // 无效间隔
        }
      ]
    },
    "logging": {
      "level": "INVALID_LEVEL"  // 无效日志级别
    }
  }"""
  
  let config = config_manager.load_from_string(invalid_config_json)
  
  // 验证配置验证结果
  let validation_result = config_manager.validate(config)
  
  assert_eq(validation_result.is_valid, false)
  assert_eq(validation_result.errors.length() > 0, true)
  
  // 检查特定错误
  let has_service_name_error = validation_result.errors.any(|error| 
    error.field == "service.name" and error.message.contains("empty")
  )
  assert_eq(has_service_name_error, true)
  
  let has_sampling_rate_error = validation_result.errors.any(|error| 
    error.field == "tracing.sampling.rate" and error.message.contains("range")
  )
  assert_eq(has_sampling_rate_error, true)
  
  let has_timeout_error = validation_result.errors.any(|error| 
    error.field == "tracing.exporter.timeout" and error.message.contains("positive")
  )
  assert_eq(has_timeout_error, true)
}

test "configuration_merge_and_override" {
  // 配置合并和覆盖测试
  
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 基础配置
  let base_config_json = """{
    "service": {
      "name": "base-service",
      "version": "1.0.0"
    },
    "tracing": {
      "enabled": true,
      "sampling": {
        "type": "always_on"
      },
      "exporter": {
        "type": "console",
        "timeout": 5000
      }
    },
    "metrics": {
      "enabled": true
    },
    "logging": {
      "enabled": true,
      "level": "INFO"
    }
  }"""
  
  // 覆盖配置
  let override_config_json = """{
    "service": {
      "name": "override-service",
      "instance_id": "instance-002"
    },
    "tracing": {
      "sampling": {
        "type": "trace_id_ratio",
        "rate": 0.1
      },
      "exporter": {
        "timeout": 10000
      }
    },
    "logging": {
      "level": "DEBUG"
    }
  }"""
  
  let base_config = config_manager.load_from_string(base_config_json)
  let override_config = config_manager.load_from_string(override_config_json)
  
  // 合并配置
  let merged_config = config_manager.merge(base_config, override_config)
  
  // 验证合并结果
  assert_eq(merged_config.service.name, "override-service")  // 被覆盖
  assert_eq(merged_config.service.version, "1.0.0")  // 保留
  assert_eq(merged_config.service.instance_id, "instance-002")  // 新增
  
  assert_eq(merged_config.tracing.enabled, true)  // 保留
  assert_eq(merged_config.tracing.sampling.type, "trace_id_ratio")  // 被覆盖
  assert_eq(merged_config.tracing.sampling.rate, 0.1)  // 新增
  assert_eq(merged_config.tracing.exporter.type, "console")  // 保留
  assert_eq(merged_config.tracing.exporter.timeout, 10000)  // 被覆盖
  
  assert_eq(merged_config.metrics.enabled, true)  // 保留
  
  assert_eq(merged_config.logging.enabled, true)  // 保留
  assert_eq(merged_config.logging.level, "DEBUG")  // 被覆盖
}

test "configuration_hot_reload" {
  // 配置热重载测试
  
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 初始配置
  let initial_config_json = """{
    "service": {
      "name": "reload-service",
      "version": "1.0.0"
    },
    "tracing": {
      "sampling": {
        "rate": 0.1
      }
    },
    "logging": {
      "level": "INFO"
    }
  }"""
  
  let config_file = "/tmp/hot_reload_config.json"
  azimuth::telemetry::sdk::platform::fs::write_file(config_file, initial_config_json)
  
  // 启用热重载
  let config_watcher = config_manager.enable_hot_reload(config_file)
  
  // 初始加载
  let initial_config = config_manager.get_current_config()
  assert_eq(initial_config.service.version, "1.0.0")
  assert_eq(initial_config.tracing.sampling.rate, 0.1)
  assert_eq(initial_config.logging.level, "INFO")
  
  // 更新配置文件
  let updated_config_json = """{
    "service": {
      "name": "reload-service",
      "version": "2.0.0"
    },
    "tracing": {
      "sampling": {
        "rate": 0.5
      }
    },
    "logging": {
      "level": "DEBUG"
    }
  }"""
  
  azimuth::telemetry::sdk::platform::fs::write_file(config_file, updated_config_json)
  
  // 等待热重载检测
  azimuth::telemetry::sdk::platform::time::sleep(1000)  // 1秒
  
  // 验证配置已更新
  let updated_config = config_manager.get_current_config()
  assert_eq(updated_config.service.version, "2.0.0")
  assert_eq(updated_config.tracing.sampling.rate, 0.5)
  assert_eq(updated_config.logging.level, "DEBUG")
  
  // 停止热重载
  config_watcher.stop()
  
  // 清理文件
  azimuth::telemetry::sdk::platform::fs::remove_file(config_file)
}

test "configuration_defaults" {
  // 配置默认值测试
  
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 空配置
  let empty_config_json = """{}"""
  
  let config = config_manager.load_from_string(empty_config_json)
  
  // 应用默认值
  let config_with_defaults = config_manager.apply_defaults(config)
  
  // 验证默认值
  assert_eq(config_with_defaults.service.name, "unknown_service")
  assert_eq(config_with_defaults.service.version, "1.0.0")
  
  assert_eq(config_with_defaults.tracing.enabled, true)
  assert_eq(config_with_defaults.tracing.sampling.type, "parent_based")
  assert_eq(config_with_defaults.tracing.sampling.root.type, "always_on")
  assert_eq(config_with_defaults.tracing.exporter.type, "console")
  assert_eq(config_with_defaults.tracing.exporter.batch_size, 512)
  assert_eq(config_with_defaults.tracing.exporter.timeout, 30000)
  
  assert_eq(config_with_defaults.metrics.enabled, true)
  assert_eq(config_with_defaults.metrics.readers.length(), 1)
  assert_eq(config_with_defaults.metrics.readers[0].type, "periodic")
  assert_eq(config_with_defaults.metrics.readers[0].interval, 60000)
  
  assert_eq(config_with_defaults.logging.enabled, true)
  assert_eq(config_with_defaults.logging.level, "INFO")
  assert_eq(config_with_defaults.logging.exporter.type, "console")
}

test "configuration_export_and_import" {
  // 配置导出和导入测试
  
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 创建完整配置
  let config_json = """{
    "service": {
      "name": "export-service",
      "version": "3.2.1",
      "instance_id": "instance-export-001"
    },
    "resource": {
      "attributes": {
        "deployment.environment": "staging",
        "host.name": "staging-node-01"
      }
    },
    "tracing": {
      "enabled": true,
      "propagators": ["tracecontext", "baggage", "tracestate"],
      "sampling": {
        "type": "parent_based",
        "root": {
          "type": "trace_id_ratio",
          "rate": 0.15
        }
      },
      "exporter": {
        "type": "otlp_http",
        "endpoint": "https://otel.example.com:4318",
        "headers": {
          "Authorization": "Bearer export-token",
          "x-service": "export-service"
        },
        "compression": "gzip",
        "timeout": 8000,
        "batch_size": 256
      }
    },
    "metrics": {
      "enabled": true,
      "readers": [
        {
          "type": "periodic",
          "interval": 45000,
          "exporter": {
            "type": "otlp_http",
            "endpoint": "https://otel.example.com:4318/v1/metrics",
            "headers": {
              "Authorization": "Bearer export-token"
            }
          }
        }
      ]
    },
    "logging": {
      "enabled": true,
      "level": "WARN",
      "exporter": {
        "type": "otlp_http",
        "endpoint": "https://otel.example.com:4318/v1/logs",
        "headers": {
          "Authorization": "Bearer export-token"
        }
      }
    }
  }"""
  
  let original_config = config_manager.load_from_string(config_json)
  
  // 导出配置
  let exported_json = config_manager.export_config(original_config)
  
  // 验证导出的JSON包含所有配置
  assert_eq(exported_json.contains("export-service"), true)
  assert_eq(exported_json.contains("3.2.1"), true)
  assert_eq(exported_json.contains("trace_id_ratio"), true)
  assert_eq(exported_json.contains("0.15"), true)
  assert_eq(exported_json.contains("https://otel.example.com:4318"), true)
  
  // 导入配置
  let imported_config = config_manager.import_config(exported_json)
  
  // 验证导入的配置与原始配置相同
  assert_eq(imported_config.service.name, original_config.service.name)
  assert_eq(imported_config.service.version, original_config.service.version)
  assert_eq(imported_config.service.instance_id, original_config.service.instance_id)
  
  assert_eq(imported_config.tracing.sampling.type, original_config.tracing.sampling.type)
  assert_eq(imported_config.tracing.sampling.root.rate, original_config.tracing.sampling.root.rate)
  
  assert_eq(imported_config.tracing.exporter.endpoint, original_config.tracing.exporter.endpoint)
  assert_eq(imported_config.tracing.exporter.compression, original_config.tracing.exporter.compression)
  
  assert_eq(imported_config.logging.level, original_config.logging.level)
}

test "configuration_profiles" {
  // 配置文件测试
  
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 创建多环境配置
  let multi_env_config_json = """{
    "profiles": {
      "development": {
        "service": {
          "name": "dev-service",
          "version": "1.0.0-dev"
        },
        "tracing": {
          "sampling": {
            "type": "always_on"
          },
          "exporter": {
            "type": "console"
          }
        },
        "logging": {
          "level": "DEBUG"
        }
      },
      "testing": {
        "service": {
          "name": "test-service",
          "version": "1.0.0-test"
        },
        "tracing": {
          "sampling": {
            "type": "always_off"
          },
          "exporter": {
            "type": "in_memory"
          }
        },
        "logging": {
          "level": "WARN"
        }
      },
      "production": {
        "service": {
          "name": "prod-service",
          "version": "1.0.0"
        },
        "tracing": {
          "sampling": {
            "type": "trace_id_ratio",
            "rate": 0.01
          },
          "exporter": {
            "type": "otlp_http",
            "endpoint": "https://otel-collector.prod.example.com:4318"
          }
        },
        "logging": {
          "level": "ERROR"
        }
      }
    },
    "active_profile": "development"
  }"""
  
  let config = config_manager.load_from_string(multi_env_config_json)
  
  // 验证当前活动配置
  assert_eq(config.service.name, "dev-service")
  assert_eq(config.service.version, "1.0.0-dev")
  assert_eq(config.tracing.sampling.type, "always_on")
  assert_eq(config.tracing.exporter.type, "console")
  assert_eq(config.logging.level, "DEBUG")
  
  // 切换到测试环境
  let test_config = config_manager.switch_profile(config, "testing")
  assert_eq(test_config.service.name, "test-service")
  assert_eq(test_config.service.version, "1.0.0-test")
  assert_eq(test_config.tracing.sampling.type, "always_off")
  assert_eq(test_config.tracing.exporter.type, "in_memory")
  assert_eq(test_config.logging.level, "WARN")
  
  // 切换到生产环境
  let prod_config = config_manager.switch_profile(config, "production")
  assert_eq(prod_config.service.name, "prod-service")
  assert_eq(prod_config.service.version, "1.0.0")
  assert_eq(prod_config.tracing.sampling.type, "trace_id_ratio")
  assert_eq(prod_config.tracing.sampling.rate, 0.01)
  assert_eq(prod_config.tracing.exporter.type, "otlp_http")
  assert_eq(prod_config.logging.level, "ERROR")
}

test "configuration_security" {
  // 配置安全测试
  
  let config_manager = azimuth::telemetry::sdk::config::ConfigurationManager::new()
  
  // 包含敏感信息的配置
  let sensitive_config_json = """{
    "service": {
      "name": "secure-service"
    },
    "tracing": {
      "exporter": {
        "type": "otlp_http",
        "endpoint": "https://otel.example.com:4318",
        "headers": {
          "Authorization": "Bearer secret-token-123",
          "x-api-key": "api-key-secret-456"
        }
      }
    },
    "metrics": {
      "exporter": {
        "type": "otlp_http",
        "endpoint": "https://otel.example.com:4318/v1/metrics",
        "headers": {
          "Authorization": "Bearer metrics-secret-789"
        }
      }
    },
    "secrets": {
      "database_password": "db-password-secret",
      "encryption_key": "encryption-key-secret-123456"
    }
  }"""
  
  let config = config_manager.load_from_string(sensitive_config_json)
  
  // 验证敏感信息检测
  let sensitive_fields = config_manager.detect_sensitive_fields(config)
  
  assert_eq(sensitive_fields.contains("tracing.exporter.headers.Authorization"), true)
  assert_eq(sensitive_fields.contains("tracing.exporter.headers.x-api-key"), true)
  assert_eq(sensitive_fields.contains("metrics.exporter.headers.Authorization"), true)
  assert_eq(sensitive_fields.contains("secrets.database_password"), true)
  assert_eq(sensitive_fields.contains("secrets.encryption_key"), true)
  
  // 导出时屏蔽敏感信息
  let sanitized_json = config_manager.export_sanitized_config(config)
  
  // 验证敏感信息被屏蔽
  assert_eq(sanitized_json.contains("secret-token-123") == false, true)
  assert_eq(sanitized_json.contains("api-key-secret-456") == false, true)
  assert_eq(sanitized_json.contains("metrics-secret-789") == false, true)
  assert_eq(sanitized_json.contains("db-password-secret") == false, true)
  assert_eq(sanitized_json.contains("encryption-key-secret-123456") == false, true)
  
  // 验证屏蔽标记存在
  assert_eq(sanitized_json.contains("***REDACTED***"), true)
}