// Azimuth 配置管理综合测试用例
// 专注于遥测系统的配置管理功能，包括配置加载、验证、热更新、环境变量处理等

// 测试1: 配置文件加载和解析
test "配置文件加载和解析测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 测试JSON配置加载
  let json_config_test = fn() {
    let json_config = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 0.1,
        "export_interval": 5000,
        "max_batch_size": 100
      },
      "tracing": {
        "service_name": "test-service",
        "service_version": "1.0.0",
        "exporter": {
          "type": "jaeger",
          "endpoint": "http://localhost:14268/api/traces"
        }
      },
      "metrics": {
        "enabled": true,
        "port": 9090,
        "path": "/metrics"
      }
    }
    
    let config_result = config_manager.load_from_json(json_config.to_string())
    
    // 验证配置加载成功
    assert_true(config_result.is_ok())
    
    match config_result {
      Ok(config) => {
        // 验证遥测配置
        assert_true(config.telemetry.enabled)
        assert_eq(config.telemetry.sampling_rate, 0.1)
        assert_eq(config.telemetry.export_interval, 5000)
        assert_eq(config.telemetry.max_batch_size, 100)
        
        // 验证追踪配置
        assert_eq(config.tracing.service_name, "test-service")
        assert_eq(config.tracing.service_version, "1.0.0")
        assert_eq(config.tracing.exporter.type, "jaeger")
        assert_eq(config.tracing.exporter.endpoint, "http://localhost:14268/api/traces")
        
        // 验证指标配置
        assert_true(config.metrics.enabled)
        assert_eq(config.metrics.port, 9090)
        assert_eq(config.metrics.path, "/metrics")
      }
      Err(_) => assert_true(false)
    }
  }
  
  json_config_test()
  
  // 测试YAML配置加载
  let yaml_config_test = fn() {
    let yaml_config = "
telemetry:
  enabled: true
  sampling_rate: 0.2
  export_interval: 10000
  max_batch_size: 200
tracing:
  service_name: yaml-test-service
  service_version: 2.0.0
  exporter:
    type: zipkin
    endpoint: http://localhost:9411/api/v2/spans
metrics:
  enabled: false
  port: 8080
  path: /custom-metrics
"
    
    let config_result = config_manager.load_from_yaml(yaml_config)
    
    // 验证配置加载成功
    assert_true(config_result.is_ok())
    
    match config_result {
      Ok(config) => {
        // 验证YAML特定配置
        assert_eq(config.telemetry.sampling_rate, 0.2)
        assert_eq(config.telemetry.export_interval, 10000)
        assert_eq(config.telemetry.max_batch_size, 200)
        assert_eq(config.tracing.service_name, "yaml-test-service")
        assert_eq(config.tracing.service_version, "2.0.0")
        assert_eq(config.tracing.exporter.type, "zipkin")
        assert_eq(config.tracing.exporter.endpoint, "http://localhost:9411/api/v2/spans")
        assert_false(config.metrics.enabled)
        assert_eq(config.metrics.port, 8080)
        assert_eq(config.metrics.path, "/custom-metrics")
      }
      Err(_) => assert_true(false)
    }
  }
  
  yaml_config_test()
  
  // 测试TOML配置加载
  let toml_config_test = fn() {
    let toml_config = "
[telemetry]
enabled = true
sampling_rate = 0.15
export_interval = 7500
max_batch_size = 150

[tracing]
service_name = \"toml-test-service\"
service_version = \"1.5.0\"

[tracing.exporter]
type = \"otlp\"
endpoint = \"http://localhost:4317\"

[metrics]
enabled = true
port = 9091
path = \"/toml-metrics\"
"
    
    let config_result = config_manager.load_from_toml(toml_config)
    
    // 验证配置加载成功
    assert_true(config_result.is_ok())
    
    match config_result {
      Ok(config) => {
        // 验证TOML特定配置
        assert_eq(config.telemetry.sampling_rate, 0.15)
        assert_eq(config.telemetry.export_interval, 7500)
        assert_eq(config.telemetry.max_batch_size, 150)
        assert_eq(config.tracing.service_name, "toml-test-service")
        assert_eq(config.tracing.service_version, "1.5.0")
        assert_eq(config.tracing.exporter.type, "otlp")
        assert_eq(config.tracing.exporter.endpoint, "http://localhost:4317")
        assert_eq(config.metrics.port, 9091)
        assert_eq(config.metrics.path, "/toml-metrics")
      }
      Err(_) => assert_true(false)
    }
  }
  
  toml_config_test()
  
  // 测试配置文件不存在的情况
  let missing_file_test = fn() {
    let config_result = config_manager.load_from_file("/nonexistent/config.json")
    
    // 应该返回错误
    assert_true(config_result.is_err())
    
    match config_result {
      Err(error) => assert_eq(error.error_type, "file_not_found"),
      Ok(_) => assert_true(false)
    }
  }
  
  missing_file_test()
  
  // 测试无效配置格式
  let invalid_format_test = fn() {
    let invalid_json = "{ invalid json format }"
    let config_result = config_manager.load_from_json(invalid_json)
    
    // 应该返回解析错误
    assert_true(config_result.is_err())
    
    match config_result {
      Err(error) => assert_eq(error.error_type, "parse_error"),
      Ok(_) => assert_true(false)
    }
  }
  
  invalid_format_test()
}

// 测试2: 配置验证和默认值
test "配置验证和默认值测试" {
  // 创建配置验证器
  let config_validator = ConfigValidator::new()
  
  // 测试有效配置验证
  let valid_config_test = fn() {
    let valid_config = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 0.5,
        "export_interval": 5000,
        "max_batch_size": 100
      },
      "tracing": {
        "service_name": "valid-service",
        "service_version": "1.0.0"
      },
      "metrics": {
        "enabled": true,
        "port": 9090
      }
    }
    
    let validation_result = config_validator.validate(valid_config)
    
    // 验证应该成功
    assert_true(validation_result.is_valid)
    assert_eq(validation_result.errors.length(), 0)
  }
  
  valid_config_test()
  
  // 测试无效配置验证
  let invalid_config_test = fn() {
    let invalid_config = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 1.5,  // 超出范围 [0.0, 1.0]
        "export_interval": -1000,  // 负数
        "max_batch_size": 0  // 不能为0
      },
      "tracing": {
        "service_name": "",  // 空字符串
        "service_version": "1.0.0"
      },
      "metrics": {
        "enabled": true,
        "port": 99999  // 超出有效端口范围
      }
    }
    
    let validation_result = config_validator.validate(invalid_config)
    
    // 验证应该失败
    assert_false(validation_result.is_valid)
    assert_true(validation_result.errors.length() > 0)
    
    // 验证具体错误
    let error_messages = validation_result.errors.map(fn(e) { e.message })
    assert_true(error_messages.any(fn(m) { m.contains("sampling_rate") }))
    assert_true(error_messages.any(fn(m) { m.contains("export_interval") }))
    assert_true(error_messages.any(fn(m) { m.contains("max_batch_size") }))
    assert_true(error_messages.any(fn(m) { m.contains("service_name") }))
    assert_true(error_messages.any(fn(m) { m.contains("port") }))
  }
  
  invalid_config_test()
  
  // 测试默认值应用
  let defaults_test = fn() {
    let partial_config = {
      "telemetry": {
        "enabled": true
        // 缺少其他字段
      },
      "tracing": {
        "service_name": "partial-service"
        // 缺少service_version
      }
      // 缺少metrics配置
    }
    
    let config_with_defaults = config_validator.apply_defaults(partial_config)
    
    // 验证默认值被正确应用
    assert_eq(config_with_defaults.telemetry.enabled, true)
    assert_eq(config_with_defaults.telemetry.sampling_rate, 0.1)  // 默认值
    assert_eq(config_with_defaults.telemetry.export_interval, 60000)  // 默认值
    assert_eq(config_with_defaults.telemetry.max_batch_size, 512)  // 默认值
    
    assert_eq(config_with_defaults.tracing.service_name, "partial-service")
    assert_eq(config_with_defaults.tracing.service_version, "1.0.0")  // 默认值
    
    assert_true(config_with_defaults.metrics.enabled)  // 默认值
    assert_eq(config_with_defaults.metrics.port, 9090)  // 默认值
    assert_eq(config_with_defaults.metrics.path, "/metrics")  // 默认值
  }
  
  defaults_test()
  
  // 测试配置类型验证
  let type_validation_test = fn() {
    let wrong_type_config = {
      "telemetry": {
        "enabled": "true",  // 应该是布尔值
        "sampling_rate": "0.5",  // 应该是数字
        "export_interval": 5000,
        "max_batch_size": 100
      },
      "tracing": {
        "service_name": 123,  // 应该是字符串
        "service_version": "1.0.0"
      }
    }
    
    let validation_result = config_validator.validate(wrong_type_config)
    
    // 验证应该失败
    assert_false(validation_result.is_valid)
    
    // 验证类型错误
    let type_errors = validation_result.errors.filter(fn(e) { e.error_type == "type_mismatch" })
    assert_true(type_errors.length() >= 2)
  }
  
  type_validation_test()
}

// 测试3: 环境变量配置
test "环境变量配置测试" {
  // 创建环境配置管理器
  let env_config_manager = EnvConfigManager::new()
  
  // 设置测试环境变量
  env_config_manager.set_env_var("AZIMUTH_TELEMETRY_ENABLED", "true")
  env_config_manager.set_env_var("AZIMUTH_TELEMETRY_SAMPLING_RATE", "0.25")
  env_config_manager.set_env_var("AZIMUTH_TELEMETRY_EXPORT_INTERVAL", "7500")
  env_config_manager.set_env_var("AZIMUTH_TRACING_SERVICE_NAME", "env-test-service")
  env_config_manager.set_env_var("AZIMUTH_TRACING_SERVICE_VERSION", "2.1.0")
  env_config_manager.set_env_var("AZIMUTH_METRICS_ENABLED", "false")
  env_config_manager.set_env_var("AZIMUTH_METRICS_PORT", "9091")
  
  // 测试环境变量加载
  let env_loading_test = fn() {
    let config_result = env_config_manager.load_from_env()
    
    // 验证环境变量加载成功
    assert_true(config_result.is_ok())
    
    match config_result {
      Ok(config) => {
        // 验证环境变量配置
        assert_true(config.telemetry.enabled)
        assert_eq(config.telemetry.sampling_rate, 0.25)
        assert_eq(config.telemetry.export_interval, 7500)
        assert_eq(config.tracing.service_name, "env-test-service")
        assert_eq(config.tracing.service_version, "2.1.0")
        assert_false(config.metrics.enabled)
        assert_eq(config.metrics.port, 9091)
        
        // 验证未设置的环境变量使用默认值
        assert_eq(config.telemetry.max_batch_size, 512)  // 默认值
        assert_eq(config.metrics.path, "/metrics")  // 默认值
      }
      Err(_) => assert_true(false)
    }
  }
  
  env_loading_test()
  
  // 测试环境变量优先级
  let priority_test = fn() {
    // 创建基础配置
    let base_config = {
      "telemetry": {
        "enabled": false,
        "sampling_rate": 0.1,
        "export_interval": 5000,
        "max_batch_size": 100
      },
      "tracing": {
        "service_name": "base-service",
        "service_version": "1.0.0"
      },
      "metrics": {
        "enabled": true,
        "port": 9090,
        "path": "/metrics"
      }
    }
    
    // 设置覆盖环境变量
    env_config_manager.set_env_var("AZIMUTH_TELEMETRY_ENABLED", "true")
    env_config_manager.set_env_var("AZIMUTH_TELEMETRY_SAMPLING_RATE", "0.3")
    env_config_manager.set_env_var("AZIMUTH_TRACING_SERVICE_NAME", "env-override-service")
    env_config_manager.set_env_var("AZIMUTH_METRICS_PORT", "9095")
    
    let merged_config = env_config_manager.merge_with_env(base_config)
    
    // 验证环境变量覆盖了基础配置
    assert_true(merged_config.telemetry.enabled)  // 环境变量覆盖
    assert_eq(merged_config.telemetry.sampling_rate, 0.3)  // 环境变量覆盖
    assert_eq(merged_config.telemetry.export_interval, 5000)  // 基础配置保持
    assert_eq(merged_config.telemetry.max_batch_size, 100)  // 基础配置保持
    
    assert_eq(merged_config.tracing.service_name, "env-override-service")  // 环境变量覆盖
    assert_eq(merged_config.tracing.service_version, "1.0.0")  // 基础配置保持
    
    assert_true(merged_config.metrics.enabled)  // 基础配置保持
    assert_eq(merged_config.metrics.port, 9095)  // 环境变量覆盖
    assert_eq(merged_config.metrics.path, "/metrics")  // 基础配置保持
  }
  
  priority_test()
  
  // 测试环境变量类型转换
  let type_conversion_test = fn() {
    // 设置各种类型的环境变量
    env_config_manager.set_env_var("AZIMUTH_TEST_BOOL", "true")
    env_config_manager.set_env_var("AZIMUTH_TEST_INT", "12345")
    env_config_manager.set_env_var("AZIMUTH_TEST_FLOAT", "1.23")
    env_config_manager.set_env_var("AZIMUTH_TEST_STRING", "test-value")
    
    // 测试类型转换
    let bool_value = env_config_manager.get_env_as_bool("AZIMUTH_TEST_BOOL")
    let int_value = env_config_manager.get_env_as_int("AZIMUTH_TEST_INT")
    let float_value = env_config_manager.get_env_as_float("AZIMUTH_TEST_FLOAT")
    let string_value = env_config_manager.get_env_as_string("AZIMUTH_TEST_STRING")
    
    // 验证类型转换
    assert_eq(bool_value, Some(true))
    assert_eq(int_value, Some(12345))
    assert_eq(float_value, Some(1.23))
    assert_eq(string_value, Some("test-value"))
    
    // 测试无效类型转换
    let invalid_bool = env_config_manager.get_env_as_bool("AZIMUTH_TEST_INT")  // 数字转布尔
    let invalid_int = env_config_manager.get_env_as_int("AZIMUTH_TEST_STRING")  // 字符串转整数
    
    // 无效转换应该返回None或默认值
    assert_eq(invalid_bool, None)
    assert_eq(invalid_int, None)
  }
  
  type_conversion_test()
  
  // 测试环境变量前缀过滤
  let prefix_filter_test = fn() {
    // 设置不同前缀的环境变量
    env_config_manager.set_env_var("AZIMUTH_TELEMETRY_ENABLED", "true")
    env_config_manager.set_env_var("AZIMUTH_TRACING_SERVICE_NAME", "azimuth-service")
    env_config_manager.set_env_var("OTHER_CONFIG_VALUE", "other")
    env_config_manager.set_env_var("AZIMUTH_OTHER_VALUE", "azimuth-other")
    
    // 获取特定前缀的环境变量
    let azimuth_vars = env_config_manager.get_env_vars_with_prefix("AZIMUTH_")
    
    // 验证过滤结果
    assert_eq(azimuth_vars.length(), 3)
    assert_true(azimuth_vars.contains(("AZIMUTH_TELEMETRY_ENABLED", "true")))
    assert_true(azimuth_vars.contains(("AZIMUTH_TRACING_SERVICE_NAME", "azimuth-service")))
    assert_true(azimuth_vars.contains(("AZIMUTH_OTHER_VALUE", "azimuth-other")))
    assert_false(azimuth_vars.contains(("OTHER_CONFIG_VALUE", "other")))
  }
  
  prefix_filter_test()
}

// 测试4: 配置热更新
test "配置热更新测试" {
  // 创建热更新管理器
  let hot_update_manager = HotUpdateManager::new()
  
  // 初始配置
  let initial_config = {
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1,
      "export_interval": 5000,
      "max_batch_size": 100
    },
    "tracing": {
      "service_name": "initial-service",
      "service_version": "1.0.0"
    },
    "metrics": {
      "enabled": true,
      "port": 9090,
      "path": "/metrics"
    }
  }
  
  hot_update_manager.set_initial_config(initial_config)
  
  // 测试配置热更新
  let hot_update_test = fn() {
    // 配置更新监听器
    let update_events = []
    let listener = fn(event) {
      update_events = update_events.push(event)
    }
    
    hot_update_manager.add_update_listener(listener)
    
    // 更新配置
    let updated_config = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 0.2,  // 更新
        "export_interval": 10000,  // 更新
        "max_batch_size": 100  // 保持不变
      },
      "tracing": {
        "service_name": "updated-service",  // 更新
        "service_version": "1.0.0"  // 保持不变
      },
      "metrics": {
        "enabled": false,  // 更新
        "port": 9090,  // 保持不变
        "path": "/metrics"  // 保持不变
      }
    }
    
    let update_result = hot_update_manager.update_config(updated_config)
    
    // 验证更新成功
    assert_true(update_result.success)
    
    // 验证更新事件
    assert_eq(update_events.length(), 1)
    assert_eq(update_events[0].type, "config_updated")
    assert_true(update_events[0].changes.length() > 0)
    
    // 验证具体变更
    let changes = update_events[0].changes
    assert_true(changes.contains(("telemetry.sampling_rate", "0.1", "0.2")))
    assert_true(changes.contains(("telemetry.export_interval", "5000", "10000")))
    assert_true(changes.contains(("tracing.service_name", "initial-service", "updated-service")))
    assert_true(changes.contains(("metrics.enabled", "true", "false")))
    
    // 验证当前配置
    let current_config = hot_update_manager.get_current_config()
    assert_eq(current_config.telemetry.sampling_rate, 0.2)
    assert_eq(current_config.telemetry.export_interval, 10000)
    assert_eq(current_config.tracing.service_name, "updated-service")
    assert_false(current_config.metrics.enabled)
  }
  
  hot_update_test()
  
  // 测试配置回滚
  let rollback_test = fn() {
    // 获取当前配置作为备份
    let backup_config = hot_update_manager.get_current_config()
    
    // 应用无效配置
    let invalid_config = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 1.5,  // 无效值
        "export_interval": 5000,
        "max_batch_size": 100
      },
      "tracing": {
        "service_name": "invalid-service",
        "service_version": "1.0.0"
      },
      "metrics": {
        "enabled": true,
        "port": 9090,
        "path": "/metrics"
      }
    }
    
    let update_result = hot_update_manager.update_config(invalid_config)
    
    // 更新应该失败
    assert_false(update_result.success)
    assert_true(update_result.error_message.contains("validation"))
    
    // 验证配置未改变
    let current_config = hot_update_manager.get_current_config()
    assert_eq(current_config.telemetry.sampling_rate, 0.2)  // 应该保持之前的值
    assert_eq(current_config.tracing.service_name, "updated-service")  // 应该保持之前的值
    
    // 测试手动回滚
    let rollback_result = hot_update_manager.rollback_to_backup(backup_config)
    
    // 验证回滚成功
    assert_true(rollback_result.success)
    
    let rolled_back_config = hot_update_manager.get_current_config()
    assert_eq(rolled_back_config.telemetry.sampling_rate, 0.1)  // 回滚到备份值
    assert_eq(rolled_back_config.tracing.service_name, "initial-service")  // 回滚到备份值
  }
  
  rollback_test()
  
  // 测试配置文件监控
  let file_watching_test = fn() {
    // 创建临时配置文件
    let temp_config_file = "/tmp/azimuth_test_config.json"
    let initial_file_content = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 0.1,
        "export_interval": 5000,
        "max_batch_size": 100
      },
      "tracing": {
        "service_name": "file-service",
        "service_version": "1.0.0"
      }
    }
    
    write_file(temp_config_file, initial_file_content.to_string())
    
    // 启用文件监控
    let file_update_events = []
    let file_listener = fn(event) {
      file_update_events = file_update_events.push(event)
    }
    
    hot_update_manager.enable_file_watching(temp_config_file, file_listener)
    
    // 修改配置文件
    let updated_file_content = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 0.3,  // 更新
        "export_interval": 8000,  // 更新
        "max_batch_size": 100
      },
      "tracing": {
        "service_name": "file-updated-service",  // 更新
        "service_version": "1.0.0"
      }
    }
    
    write_file(temp_config_file, updated_file_content.to_string())
    
    // 等待文件监控检测到变更
    Time::sleep(1100)
    
    // 验证文件更新事件
    assert_true(file_update_events.length() > 0)
    
    let latest_event = file_update_events[file_update_events.length() - 1]
    assert_eq(latest_event.type, "file_updated")
    assert_eq(latest_event.file_path, temp_config_file)
    
    // 验证配置已更新
    let current_config = hot_update_manager.get_current_config()
    assert_eq(current_config.telemetry.sampling_rate, 0.3)
    assert_eq(current_config.telemetry.export_interval, 8000)
    assert_eq(current_config.tracing.service_name, "file-updated-service")
    
    // 清理临时文件
    delete_file(temp_config_file)
  }
  
  file_watching_test()
  
  // 测试部分配置更新
  let partial_update_test = fn() {
    // 只更新部分配置
    let partial_update = {
      "telemetry": {
        "sampling_rate": 0.4  // 只更新这一个字段
      }
    }
    
    let update_result = hot_update_manager.update_partial(partial_update)
    
    // 验证部分更新成功
    assert_true(update_result.success)
    
    // 验证只有指定字段被更新
    let current_config = hot_update_manager.get_current_config()
    assert_eq(current_config.telemetry.sampling_rate, 0.4)  // 已更新
    assert_eq(current_config.telemetry.export_interval, 8000)  // 保持不变
    assert_eq(current_config.telemetry.max_batch_size, 100)  // 保持不变
    assert_eq(current_config.tracing.service_name, "file-updated-service")  // 保持不变
  }
  
  partial_update_test()
}

// 测试5: 配置模板和继承
test "配置模板和继承测试" {
  // 创建配置模板管理器
  let template_manager = ConfigTemplateManager::new()
  
  // 定义基础模板
  let base_template = {
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1,
      "export_interval": 5000,
      "max_batch_size": 100
    },
    "tracing": {
      "service_version": "1.0.0",
      "exporter": {
        "type": "jaeger",
        "timeout": 30000
      }
    },
    "metrics": {
      "enabled": true,
      "port": 9090,
      "path": "/metrics"
    }
  }
  
  template_manager.add_template("base", base_template)
  
  // 定义开发环境模板
  let dev_template = {
    "extends": "base",
    "telemetry": {
      "sampling_rate": 0.5,  // 覆盖基础模板
      "export_interval": 10000  // 覆盖基础模板
    },
    "tracing": {
      "service_name": "dev-service",
      "exporter": {
        "endpoint": "http://dev-jaeger:14268/api/traces"  // 覆盖基础模板的exporter.endpoint
      }
    },
    "logging": {  // 新增配置
      "level": "debug",
      "console": true
    }
  }
  
  template_manager.add_template("development", dev_template)
  
  // 定义生产环境模板
  let prod_template = {
    "extends": "base",
    "telemetry": {
      "sampling_rate": 0.01,  // 覆盖基础模板
      "max_batch_size": 1000  // 覆盖基础模板
    },
    "tracing": {
      "service_name": "prod-service",
      "exporter": {
        "type": "otlp",  // 覆盖基础模板
        "endpoint": "http://prod-collector:4317"
      }
    },
    "security": {  // 新增配置
      "enabled": true,
      "encryption": true
    }
  }
  
  template_manager.add_template("production", prod_template)
  
  // 测试模板继承
  let template_inheritance_test = fn() {
    // 渲染开发环境配置
    let dev_config_result = template_manager.render_template("development")
    
    assert_true(dev_config_result.is_ok())
    
    match dev_config_result {
      Ok(dev_config) => {
        // 验证继承的基础配置
        assert_true(dev_config.telemetry.enabled)  // 从base继承
        assert_eq(dev_config.telemetry.max_batch_size, 100)  // 从base继承
        
        // 验证覆盖的配置
        assert_eq(dev_config.telemetry.sampling_rate, 0.5)  // development覆盖
        assert_eq(dev_config.telemetry.export_interval, 10000)  // development覆盖
        
        // 验证嵌套覆盖
        assert_eq(dev_config.tracing.service_version, "1.0.0")  // 从base继承
        assert_eq(dev_config.tracing.exporter.type, "jaeger")  // 从base继承
        assert_eq(dev_config.tracing.exporter.timeout, 30000)  // 从base继承
        assert_eq(dev_config.tracing.exporter.endpoint, "http://dev-jaeger:14268/api/traces")  // development覆盖
        
        // 验证新增配置
        assert_eq(dev_config.tracing.service_name, "dev-service")  // development新增
        assert_eq(dev_config.logging.level, "debug")  // development新增
        assert_true(dev_config.logging.console)  // development新增
        
        // 验证继承的metrics配置
        assert_true(dev_config.metrics.enabled)  // 从base继承
        assert_eq(dev_config.metrics.port, 9090)  // 从base继承
        assert_eq(dev_config.metrics.path, "/metrics")  // 从base继承
      }
      Err(_) => assert_true(false)
    }
    
    // 渲染生产环境配置
    let prod_config_result = template_manager.render_template("production")
    
    assert_true(prod_config_result.is_ok())
    
    match prod_config_result {
      Ok(prod_config) => {
        // 验证继承的基础配置
        assert_true(prod_config.telemetry.enabled)  // 从base继承
        assert_eq(prod_config.telemetry.export_interval, 5000)  // 从base继承
        
        // 验证覆盖的配置
        assert_eq(prod_config.telemetry.sampling_rate, 0.01)  // production覆盖
        assert_eq(prod_config.telemetry.max_batch_size, 1000)  // production覆盖
        
        // 验证嵌套覆盖
        assert_eq(prod_config.tracing.service_version, "1.0.0")  // 从base继承
        assert_eq(prod_config.tracing.exporter.timeout, 30000)  // 从base继承
        assert_eq(prod_config.tracing.exporter.type, "otlp")  // production覆盖
        assert_eq(prod_config.tracing.exporter.endpoint, "http://prod-collector:4317")  // production覆盖
        
        // 验证新增配置
        assert_eq(prod_config.tracing.service_name, "prod-service")  // production新增
        assert_true(prod_config.security.enabled)  // production新增
        assert_true(prod_config.security.encryption)  // production新增
      }
      Err(_) => assert_true(false)
    }
  }
  
  template_inheritance_test()
  
  // 测试多级继承
  let multi_level_inheritance_test = fn() {
    // 定义测试环境模板，继承自开发环境
    let test_template = {
      "extends": "development",
      "telemetry": {
        "sampling_rate": 0.8  // 覆盖development的配置
      },
      "tracing": {
        "service_name": "test-service"  // 覆盖development的配置
      },
      "features": {  // 新增配置
        "experimental": true,
        "debug_mode": true
      }
    }
    
    template_manager.add_template("testing", test_template)
    
    // 渲染测试环境配置
    let test_config_result = template_manager.render_template("testing")
    
    assert_true(test_config_result.is_ok())
    
    match test_config_result {
      Ok(test_config) => {
        // 验证多级继承链：base -> development -> testing
        
        // 从base继承
        assert_eq(test_config.telemetry.max_batch_size, 100)
        assert_eq(test_config.tracing.exporter.timeout, 30000)
        
        // 从development继承
        assert_eq(test_config.telemetry.export_interval, 10000)
        assert_eq(test_config.tracing.exporter.endpoint, "http://dev-jaeger:14268/api/traces")
        assert_eq(test_config.logging.level, "debug")
        
        // testing覆盖
        assert_eq(test_config.telemetry.sampling_rate, 0.8)
        assert_eq(test_config.tracing.service_name, "test-service")
        
        // testing新增
        assert_true(test_config.features.experimental)
        assert_true(test_config.features.debug_mode)
      }
      Err(_) => assert_true(false)
    }
  }
  
  multi_level_inheritance_test()
  
  // 测试模板变量替换
  let variable_substitution_test = fn() {
    // 定义带变量的模板
    let variable_template = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": "${SAMPLING_RATE}",
        "export_interval": "${EXPORT_INTERVAL}"
      },
      "tracing": {
        "service_name": "${SERVICE_NAME}",
        "service_version": "${SERVICE_VERSION}",
        "exporter": {
          "endpoint": "http://${COLLECTOR_HOST}:${COLLECTOR_PORT}/api/traces"
        }
      },
      "metrics": {
        "port": "${METRICS_PORT}"
      }
    }
    
    template_manager.add_template("variable", variable_template)
    
    // 定义变量
    let variables = @{
      "SAMPLING_RATE": "0.3",
      "EXPORT_INTERVAL": "7500",
      "SERVICE_NAME": "variable-service",
      "SERVICE_VERSION": "2.0.0",
      "COLLECTOR_HOST": "collector.example.com",
      "COLLECTOR_PORT": "14268",
      "METRICS_PORT": "9095"
    }
    
    // 渲染带变量的模板
    let rendered_result = template_manager.render_template_with_variables("variable", variables)
    
    assert_true(rendered_result.is_ok())
    
    match rendered_result {
      Ok(rendered_config) => {
        // 验证变量替换
        assert_eq(rendered_config.telemetry.sampling_rate, "0.3")
        assert_eq(rendered_config.telemetry.export_interval, "7500")
        assert_eq(rendered_config.tracing.service_name, "variable-service")
        assert_eq(rendered_config.tracing.service_version, "2.0.0")
        assert_eq(rendered_config.tracing.exporter.endpoint, "http://collector.example.com:14268/api/traces")
        assert_eq(rendered_config.metrics.port, "9095")
      }
      Err(_) => assert_true(false)
    }
  }
  
  variable_substitution_test()
  
  // 测试模板验证
  let template_validation_test = fn() {
    // 定义无效模板（循环继承）
    let invalid_template1 = {
      "extends": "invalid2"
    }
    
    let invalid_template2 = {
      "extends": "invalid1"
    }
    
    template_manager.add_template("invalid1", invalid_template1)
    template_manager.add_template("invalid2", invalid_template2)
    
    // 尝试渲染循环继承的模板
    let invalid_result = template_manager.render_template("invalid1")
    
    // 应该失败
    assert_true(invalid_result.is_err())
    
    match invalid_result {
      Err(error) => assert_true(error.message.contains("circular")),
      Ok(_) => assert_true(false)
    }
    
    // 定义继承不存在的模板
    let orphan_template = {
      "extends": "nonexistent_template"
    }
    
    template_manager.add_template("orphan", orphan_template)
    
    let orphan_result = template_manager.render_template("orphan")
    
    // 应该失败
    assert_true(orphan_result.is_err())
    
    match orphan_result {
      Err(error) => assert_true(error.message.contains("not found")),
      Ok(_) => assert_true(false)
    }
  }
  
  template_validation_test()
}

// 测试6: 配置加密和安全
test "配置加密和安全测试" {
  // 创建配置安全管理器
  let security_manager = ConfigSecurityManager::new()
  
  // 生成加密密钥
  let encryption_key = security_manager.generate_encryption_key()
  assert_true(encryption_key.length() > 0)
  
  // 测试配置值加密
  let encryption_test = fn() {
    let sensitive_values = [
      "database_password",
      "api_key_12345",
      "secret_token_abcde",
      "private_key_value"
    ]
    
    let encrypted_values = []
    
    // 加密敏感值
    for value in sensitive_values {
      let encrypted = security_manager.encrypt_value(value, encryption_key)
      assert_true(encrypted.is_ok())
      
      match encrypted {
        Ok(encrypted_value) => {
          assert_true(encrypted_value.length() > 0)
          assert_ne(encrypted_value, value)  // 加密后应该不同
          encrypted_values = encrypted_values.push((value, encrypted_value))
        }
        Err(_) => assert_true(false)
      }
    }
    
    // 解密并验证
    for (original, encrypted) in encrypted_values {
      let decrypted = security_manager.decrypt_value(encrypted, encryption_key)
      assert_true(decrypted.is_ok())
      
      match decrypted {
        Ok(decrypted_value) => assert_eq(decrypted_value, original),
        Err(_) => assert_true(false)
      }
    }
  }
  
  encryption_test()
  
  // 测试配置文件加密
  let file_encryption_test = fn() {
    let sensitive_config = {
      "database": {
        "host": "localhost",
        "port": 5432,
        "username": "admin",
        "password": "secret_password_123"
      },
      "api": {
        "key": "api_key_abcdef123456",
        "secret": "api_secret_xyz789"
      },
      "public_config": {
        "service_name": "test-service",
        "version": "1.0.0"
      }
    }
    
    // 指定需要加密的字段
    let encrypted_fields = [
      "database.password",
      "api.key",
      "api.secret"
    ]
    
    // 加密配置文件
    let encrypted_config_result = security_manager.encrypt_config_file(
      sensitive_config, 
      encrypted_fields, 
      encryption_key
    )
    
    assert_true(encrypted_config_result.is_ok())
    
    match encrypted_config_result {
      Ok(encrypted_config) => {
        // 验证敏感字段已加密
        assert_eq(encrypted_config.database.host, "localhost")  // 未加密
        assert_eq(encrypted_config.database.port, 5432)  // 未加密
        assert_eq(encrypted_config.database.username, "admin")  // 未加密
        assert_ne(encrypted_config.database.password, "secret_password_123")  // 已加密
        assert_ne(encrypted_config.api.key, "api_key_abcdef123456")  // 已加密
        assert_ne(encrypted_config.api.secret, "api_secret_xyz789")  // 已加密
        assert_eq(encrypted_config.public_config.service_name, "test-service")  // 未加密
        
        // 解密配置文件
        let decrypted_config_result = security_manager.decrypt_config_file(
          encrypted_config,
          encrypted_fields,
          encryption_key
        )
        
        assert_true(decrypted_config_result.is_ok())
        
        match decrypted_config_result {
          Ok(decrypted_config) => {
            // 验证解密后的配置与原始配置相同
            assert_eq(decrypted_config.database.password, "secret_password_123")
            assert_eq(decrypted_config.api.key, "api_key_abcdef123456")
            assert_eq(decrypted_config.api.secret, "api_secret_xyz789")
            assert_eq(decrypted_config.public_config.service_name, "test-service")
          }
          Err(_) => assert_true(false)
        }
      }
      Err(_) => assert_true(false)
    }
  }
  
  file_encryption_test()
  
  // 测试配置访问控制
  let access_control_test = fn() {
    let access_manager = ConfigAccessManager::new()
    
    // 定义角色和权限
    access_manager.add_role("admin", ["read", "write", "delete", "encrypt", "decrypt"])
    access_manager.add_role("operator", ["read", "write"])
    access_manager.add_role("viewer", ["read"])
    
    // 定义用户和角色
    access_manager.add_user("admin_user", "admin")
    access_manager.add_user("operator_user", "operator")
    access_manager.add_user("viewer_user", "viewer")
    
    // 测试管理员权限
    assert_true(access_manager.check_permission("admin_user", "read"))
    assert_true(access_manager.check_permission("admin_user", "write"))
    assert_true(access_manager.check_permission("admin_user", "delete"))
    assert_true(access_manager.check_permission("admin_user", "encrypt"))
    assert_true(access_manager.check_permission("admin_user", "decrypt"))
    
    // 测试操作员权限
    assert_true(access_manager.check_permission("operator_user", "read"))
    assert_true(access_manager.check_permission("operator_user", "write"))
    assert_false(access_manager.check_permission("operator_user", "delete"))
    assert_false(access_manager.check_permission("operator_user", "encrypt"))
    assert_false(access_manager.check_permission("operator_user", "decrypt"))
    
    // 测试查看者权限
    assert_true(access_manager.check_permission("viewer_user", "read"))
    assert_false(access_manager.check_permission("viewer_user", "write"))
    assert_false(access_manager.check_permission("viewer_user", "delete"))
    assert_false(access_manager.check_permission("viewer_user", "encrypt"))
    assert_false(access_manager.check_permission("viewer_user", "decrypt"))
    
    // 测试不存在的用户
    assert_false(access_manager.check_permission("nonexistent_user", "read"))
  }
  
  access_control_test()
  
  // 测试配置审计日志
  let audit_log_test = fn() {
    let audit_logger = ConfigAuditLogger::new()
    
    // 记录配置访问
    audit_logger.log_access("operator_user", "read", "telemetry.sampling_rate", "0.1")
    audit_logger.log_access("operator_user", "read", "tracing.service_name", "test-service")
    
    // 记录配置修改
    audit_logger.log_modification("operator_user", "write", "telemetry.sampling_rate", "0.1", "0.2")
    audit_logger.log_modification("admin_user", "write", "metrics.enabled", "true", "false")
    
    // 记录敏感操作
    audit_logger.log_sensitive_operation("admin_user", "encrypt", "database.password")
    audit_logger.log_sensitive_operation("admin_user", "decrypt", "api.key")
    
    // 获取审计日志
    let audit_logs = audit_logger.get_audit_logs()
    
    // 验证审计日志
    assert_eq(audit_logs.length(), 6)
    
    // 验证访问日志
    let access_logs = audit_logs.filter(fn(log) { log.operation_type == "access" })
    assert_eq(access_logs.length(), 2)
    assert_eq(access_logs[0].user, "operator_user")
    assert_eq(access_logs[0].action, "read")
    assert_eq(access_logs[0].config_key, "telemetry.sampling_rate")
    
    // 验证修改日志
    let modification_logs = audit_logs.filter(fn(log) { log.operation_type == "modification" })
    assert_eq(modification_logs.length(), 2)
    assert_eq(modification_logs[0].user, "operator_user")
    assert_eq(modification_logs[0].action, "write")
    assert_eq(modification_logs[0].old_value, "0.1")
    assert_eq(modification_logs[0].new_value, "0.2")
    
    // 验证敏感操作日志
    let sensitive_logs = audit_logs.filter(fn(log) { log.operation_type == "sensitive" })
    assert_eq(sensitive_logs.length(), 2)
    assert_eq(sensitive_logs[0].user, "admin_user")
    assert_eq(sensitive_logs[0].action, "encrypt")
    assert_eq(sensitive_logs[0].config_key, "database.password")
  }
  
  audit_log_test()
}

// 测试7: 配置版本管理
test "配置版本管理测试" {
  // 创建配置版本管理器
  let version_manager = ConfigVersionManager::new()
  
  // 初始配置版本
  let v1_config = {
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1,
      "export_interval": 5000
    },
    "tracing": {
      "service_name": "versioned-service",
      "service_version": "1.0.0"
    }
  }
  
  // 创建第一个版本
  let v1_result = version_manager.create_version("1.0.0", v1_config, "Initial configuration")
  
  assert_true(v1_result.is_ok())
  
  match v1_result {
    Ok(version_info) => {
      assert_eq(version_info.version, "1.0.0")
      assert_eq(version_info.description, "Initial configuration")
      assert_true(version_info.timestamp > 0)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试版本历史
  let version_history_test = fn() {
    // 创建更多版本
    let v2_config = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 0.2,  // 更新
        "export_interval": 5000
      },
      "tracing": {
        "service_name": "versioned-service",
        "service_version": "1.0.0"
      }
    }
    
    let v2_result = version_manager.create_version("1.1.0", v2_config, "Updated sampling rate")
    assert_true(v2_result.is_ok())
    
    let v3_config = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 0.2,
        "export_interval": 10000  // 更新
      },
      "tracing": {
        "service_name": "versioned-service-v2",  // 更新
        "service_version": "1.0.0"
      }
    }
    
    let v3_result = version_manager.create_version("1.2.0", v3_config, "Updated export interval and service name")
    assert_true(v3_result.is_ok())
    
    // 获取版本历史
    let history = version_manager.get_version_history()
    
    // 验证版本历史
    assert_eq(history.length(), 3)
    assert_eq(history[0].version, "1.0.0")
    assert_eq(history[1].version, "1.1.0")
    assert_eq(history[2].version, "1.2.0")
    
    // 验证时间戳递增
    assert_true(history[0].timestamp <= history[1].timestamp)
    assert_true(history[1].timestamp <= history[2].timestamp)
  }
  
  version_history_test()
  
  // 测试版本比较
  let version_comparison_test = fn() {
    // 测试版本比较
    assert_true(version_manager.compare_versions("1.2.0", "1.1.0") > 0)
    assert_true(version_manager.compare_versions("1.1.0", "1.2.0") < 0)
    assert_eq(version_manager.compare_versions("1.1.0", "1.1.0"), 0)
    
    // 测试复杂版本比较
    assert_true(version_manager.compare_versions("2.0.0", "1.9.9") > 0)
    assert_true(version_manager.compare_versions("1.0.1", "1.0.0") > 0)
    assert_true(version_manager.compare_versions("1.0.0", "1.0.1") < 0)
    
    // 测试预发布版本
    assert_true(version_manager.compare_versions("1.0.0", "1.0.0-alpha") > 0)
    assert_true(version_manager.compare_versions("1.0.0-alpha", "1.0.0-beta") < 0)
    assert_true(version_manager.compare_versions("1.0.0-beta", "1.0.0") < 0)
  }
  
  version_comparison_test()
  
  // 测试版本回滚
  let version_rollback_test = fn() {
    // 获取最新版本配置
    let latest_config = version_manager.get_latest_version()
    assert_eq(latest_config.version, "1.2.0")
    assert_eq(latest_config.config.tracing.service_name, "versioned-service-v2")
    
    // 回滚到特定版本
    let rollback_result = version_manager.rollback_to_version("1.1.0")
    
    assert_true(rollback_result.is_ok())
    
    // 验证回滚后的当前版本
    let current_version = version_manager.get_current_version()
    assert_eq(current_version.version, "1.1.0")
    assert_eq(current_version.config.telemetry.sampling_rate, 0.2)
    assert_eq(current_version.config.tracing.service_name, "versioned-service")
    
    // 创建新版本（基于回滚的版本）
    let v4_config = {
      "telemetry": {
        "enabled": true,
        "sampling_rate": 0.15,  // 新值
        "export_interval": 10000  // 保持回滚版本的值
      },
      "tracing": {
        "service_name": "versioned-service",
        "service_version": "1.0.0"
      }
    }
    
    let v4_result = version_manager.create_version("1.2.1", v4_config, "New version after rollback")
    assert_true(v4_result.is_ok())
    
    // 验证新版本成为当前版本
    let new_current = version_manager.get_current_version()
    assert_eq(new_current.version, "1.2.1")
    assert_eq(new_current.config.telemetry.sampling_rate, 0.15)
  }
  
  version_rollback_test()
  
  // 测试版本差异比较
  let version_diff_test = fn() {
    // 比较两个版本的差异
    let diff_result = version_manager.compare_versions("1.0.0", "1.2.0")
    
    assert_true(diff_result.is_ok())
    
    match diff_result {
      Ok(diff) => {
        // 验证差异报告
        assert_eq(diff.from_version, "1.0.0")
        assert_eq(diff.to_version, "1.2.0")
        
        // 验证变更列表
        let changes = diff.changes
        
        // 应该有telemetry.sampling_rate的变更
        let sampling_rate_change = changes.find(fn(c) { c.path == "telemetry.sampling_rate" })
        assert_true(sampling_rate_change.is_some())
        match sampling_rate_change {
          Some(change) => {
            assert_eq(change.old_value, "0.1")
            assert_eq(change.new_value, "0.2")
            assert_eq(change.change_type, "updated")
          }
          None => assert_true(false)
        }
        
        // 应该有telemetry.export_interval的变更
        let export_interval_change = changes.find(fn(c) { c.path == "telemetry.export_interval" })
        assert_true(export_interval_change.is_some())
        match export_interval_change {
          Some(change) => {
            assert_eq(change.old_value, "5000")
            assert_eq(change.new_value, "10000")
            assert_eq(change.change_type, "updated")
          }
          None => assert_true(false)
        }
        
        // 应该有tracing.service_name的变更
        let service_name_change = changes.find(fn(c) { c.path == "tracing.service_name" })
        assert_true(service_name_change.is_some())
        match service_name_change {
          Some(change) => {
            assert_eq(change.old_value, "versioned-service")
            assert_eq(change.new_value, "versioned-service-v2")
            assert_eq(change.change_type, "updated")
          }
          None => assert_true(false)
        }
      }
      Err(_) => assert_true(false)
    }
  }
  
  version_diff_test()
  
  // 测试版本标签
  let version_tagging_test = fn() {
    // 添加版本标签
    let tag_result1 = version_manager.add_tag("1.0.0", "stable")
    assert_true(tag_result1.is_ok())
    
    let tag_result2 = version_manager.add_tag("1.2.0", "production")
    assert_true(tag_result2.is_ok())
    
    let tag_result3 = version_manager.add_tag("1.2.1", "latest")
    assert_true(tag_result3.is_ok())
    
    // 通过标签查找版本
    let stable_version = version_manager.find_version_by_tag("stable")
    assert_true(stable_version.is_some())
    assert_eq(stable_version.unwrap().version, "1.0.0")
    
    let production_version = version_manager.find_version_by_tag("production")
    assert_true(production_version.is_some())
    assert_eq(production_version.unwrap().version, "1.2.0")
    
    let latest_version = version_manager.find_version_by_tag("latest")
    assert_true(latest_version.is_some())
    assert_eq(latest_version.unwrap().version, "1.2.1")
    
    // 获取所有标签
    let all_tags = version_manager.get_all_tags()
    assert_eq(all_tags.length(), 3)
    assert_true(all_tags.contains(("1.0.0", "stable")))
    assert_true(all_tags.contains(("1.2.0", "production")))
    assert_true(all_tags.contains(("1.2.1", "latest")))
    
    // 移除标签
    let remove_result = version_manager.remove_tag("1.2.0", "production")
    assert_true(remove_result.is_ok())
    
    // 验证标签已移除
    let removed_version = version_manager.find_version_by_tag("production")
    assert_true(removed_version.is_none())
    
    let updated_tags = version_manager.get_all_tags()
    assert_eq(updated_tags.length(), 2)
  }
  
  version_tagging_test()
}