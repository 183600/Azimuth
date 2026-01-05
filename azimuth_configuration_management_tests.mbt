// Azimuth遥测配置管理测试用例
// 专注于遥测系统的配置加载、验证和动态更新

// 测试1: 基础配置加载与验证
test "basic configuration loading and validation" {
  // 定义遥测配置结构
  type TelemetryConfig = {
    service_name: String,
    service_version: String,
    environment: String,
    sampling_ratio: Float,
    batch_size: Int,
    flush_interval_ms: Int,
    endpoint_url: String,
    headers: Array[(String, String)],
    enabled_features: Array[String]
  }
  
  // 定义配置验证规则
  type ValidationRule = {
    field: String,
    required: Bool,
    min_value: Option[Int],
    max_value: Option[Int],
    allowed_values: Option[Array[String]],
    pattern: Option[String]
  }
  
  // 创建配置验证规则
  let validation_rules = [
    { field: "service_name", required: true, min_value: None, max_value: None, allowed_values: None, pattern: Some("^[a-zA-Z][a-zA-Z0-9_-]*$") },
    { field: "service_version", required: true, min_value: None, max_value: None, allowed_values: None, pattern: Some("^\\d+\\.\\d+\\.\\d+$") },
    { field: "environment", required: true, min_value: None, max_value: None, allowed_values: Some(["development", "staging", "production"]), pattern: None },
    { field: "sampling_ratio", required: true, min_value: Some(0), max_value: Some(100), allowed_values: None, pattern: None },
    { field: "batch_size", required: true, min_value: Some(1), max_value: Some(10000), allowed_values: None, pattern: None },
    { field: "flush_interval_ms", required: true, min_value: Some(100), max_value: Some(60000), allowed_values: None, pattern: None },
    { field: "endpoint_url", required: true, min_value: None, max_value: None, allowed_values: None, pattern: Some("^https?://.+") },
    { field: "headers", required: false, min_value: None, max_value: None, allowed_values: None, pattern: None },
    { field: "enabled_features", required: false, min_value: None, max_value: None, allowed_values: Some(["tracing", "metrics", "logs"]), pattern: None }
  ]
  
  // 简单的模式匹配验证
  let validate_pattern = fn(value: String, pattern: String) {
    // 简化的模式匹配实现
    if pattern == "^[a-zA-Z][a-zA-Z0-9_-]*$" {
      // 检查以字母开头，只包含字母、数字、下划线和连字符
      if value.length() == 0 {
        return false
      }
      
      let first_char = value[0]
      if not((first_char >= 'a' and first_char <= 'z') or (first_char >= 'A' and first_char <= 'Z')) {
        return false
      }
      
      for i in 1..value.length() {
        let char = value[i]
        if not((char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or (char >= '0' and char <= '9') or char == '_' or char == '-') {
          return false
        }
      }
      
      true
    } else if pattern == "^\\d+\\.\\d+\\.\\d+$" {
      // 检查语义版本格式
      let parts = value.split(".")
      if parts.length() != 3 {
        return false
      }
      
      for part in parts {
        if part.length() == 0 {
          return false
        }
        
        for i in 0..part.length() {
          let char = part[i]
          if char < '0' or char > '9' {
            return false
          }
        }
      }
      
      true
    } else if pattern == "^https?://.+" {
      // 检查URL格式
      value.starts_with("http://") or value.starts_with("https://")
    } else {
      true  // 未实现的模式默认通过
    }
  }
  
  // 验证配置
  let validate_config = fn(config: TelemetryConfig, rules: Array[ValidationRule]) {
    let mut errors = []
    
    for rule in rules {
      // 获取字段值（简化实现）
      let field_value = match rule.field {
        "service_name" => Some(config.service_name)
        "service_version" => Some(config.service_version)
        "environment" => Some(config.environment)
        "sampling_ratio" => Some(config.sampling_ratio.to_string())
        "batch_size" => Some(config.batch_size.to_string())
        "flush_interval_ms" => Some(config.flush_interval_ms.to_string())
        "endpoint_url" => Some(config.endpoint_url)
        "headers" => Some(config.headers.length().to_string())
        "enabled_features" => Some(config.enabled_features.length().to_string())
        _ => None
      }
      
      match field_value {
        Some(value) => {
          // 检查必填字段
          if rule.required and value.length() == 0 {
            errors = errors.push("Field " + rule.field + " is required")
          }
          
          // 检查最小值
          match rule.min_value {
            Some(min_val) => {
              let int_value = value.to_int()
              if int_value < min_val {
                errors = errors.push("Field " + rule.field + " is below minimum value " + min_val.to_string())
              }
            }
            None => ()
          }
          
          // 检查最大值
          match rule.max_value {
            Some(max_val) => {
              let int_value = value.to_int()
              if int_value > max_val {
                errors = errors.push("Field " + rule.field + " exceeds maximum value " + max_val.to_string())
              }
            }
            None => ()
          }
          
          // 检查允许的值
          match rule.allowed_values {
            Some(allowed) => {
              if not(allowed.contains(value)) {
                errors = errors.push("Field " + rule.field + " has invalid value: " + value)
              }
            }
            None => ()
          }
          
          // 检查模式
          match rule.pattern {
            Some(pattern) => {
              if not(validate_pattern(value, pattern)) {
                errors = errors.push("Field " + rule.field + " does not match required pattern")
              }
            }
            None => ()
          }
        }
        None => {
          if rule.required {
            errors = errors.push("Required field " + rule.field + " is missing")
          }
        }
      }
    }
    
    errors
  }
  
  // 创建有效配置
  let valid_config = {
    service_name: "payment-service",
    service_version: "1.2.3",
    environment: "production",
    sampling_ratio: 10.0,
    batch_size: 100,
    flush_interval_ms: 5000,
    endpoint_url: "https://telemetry.example.com/api/v1/traces",
    headers: [("Authorization", "Bearer token123")],
    enabled_features: ["tracing", "metrics"]
  }
  
  // 验证有效配置
  let valid_errors = validate_config(valid_config, validation_rules)
  assert_eq(valid_errors.length(), 0)
  
  // 创建无效配置
  let invalid_config = {
    service_name: "123invalid",  // 不以字母开头
    service_version: "1.2",      // 缺少补丁版本
    environment: "test",         // 不在允许的环境中
    sampling_ratio: 150.0,       // 超过最大值
    batch_size: 0,               // 低于最小值
    flush_interval_ms: 50,       // 低于最小值
    endpoint_url: "ftp://invalid", // 无效URL协议
    headers: [],
    enabled_features: ["invalid_feature"]  // 不在允许的功能中
  }
  
  // 验证无效配置
  let invalid_errors = validate_config(invalid_config, validation_rules)
  assert_true(invalid_errors.length() > 0)
  
  // 检查特定错误
  let has_service_name_error = invalid_errors.some(fn(error) {
    error.contains("service_name") and error.contains("pattern")
  })
  assert_true(has_service_name_error)
  
  let has_version_error = invalid_errors.some(fn(error) {
    error.contains("service_version") and error.contains("pattern")
  })
  assert_true(has_version_error)
  
  let has_environment_error = invalid_errors.some(fn(error) {
    error.contains("environment") and error.contains("invalid value")
  })
  assert_true(has_environment_error)
}

// 测试2: 配置默认值与合并
test "configuration defaults and merging" {
  // 定义配置默认值
  let default_config = {
    service_name: "",
    service_version: "1.0.0",
    environment: "development",
    sampling_ratio: 10.0,
    batch_size: 100,
    flush_interval_ms: 5000,
    endpoint_url: "https://localhost:4317/api/v1/traces",
    headers: [],
    enabled_features: ["tracing"]
  }
  
  // 配置合并函数
  let merge_configs = fn(base: TelemetryConfig, override: TelemetryConfig) {
    {
      service_name: if override.service_name.length() > 0 { override.service_name } else { base.service_name },
      service_version: if override.service_version.length() > 0 { override.service_version } else { base.service_version },
      environment: if override.environment.length() > 0 { override.environment } else { base.environment },
      sampling_ratio: if override.sampling_ratio >= 0.0 { override.sampling_ratio } else { base.sampling_ratio },
      batch_size: if override.batch_size > 0 { override.batch_size } else { base.batch_size },
      flush_interval_ms: if override.flush_interval_ms > 0 { override.flush_interval_ms } else { base.flush_interval_ms },
      endpoint_url: if override.endpoint_url.length() > 0 { override.endpoint_url } else { base.endpoint_url },
      headers: if override.headers.length() > 0 { override.headers } else { base.headers },
      enabled_features: if override.enabled_features.length() > 0 { override.enabled_features } else { base.enabled_features }
    }
  }
  
  // 测试部分配置覆盖
  let partial_config = {
    service_name: "user-service",
    service_version: "",  // 使用默认值
    environment: "staging",
    sampling_ratio: 20.0,
    batch_size: 0,        // 使用默认值
    flush_interval_ms: 10000,
    endpoint_url: "",     // 使用默认值
    headers: [("API-Key", "secret123")],
    enabled_features: []  // 使用默认值
  }
  
  let merged_config = merge_configs(default_config, partial_config)
  
  assert_eq(merged_config.service_name, "user-service")      // 覆盖
  assert_eq(merged_config.service_version, "1.0.0")         // 默认
  assert_eq(merged_config.environment, "staging")            // 覆盖
  assert_eq(merged_config.sampling_ratio, 20.0)             // 覆盖
  assert_eq(merged_config.batch_size, 100)                  // 默认
  assert_eq(merged_config.flush_interval_ms, 10000)         // 覆盖
  assert_eq(merged_config.endpoint_url, "https://localhost:4317/api/v1/traces")  // 默认
  assert_eq(merged_config.headers.length(), 1)              // 覆盖
  assert_eq(merged_config.headers[0], ("API-Key", "secret123"))
  assert_eq(merged_config.enabled_features, ["tracing"])    // 默认
  
  // 测试空配置（全部使用默认值）
  let empty_config = {
    service_name: "",
    service_version: "",
    environment: "",
    sampling_ratio: -1.0,
    batch_size: 0,
    flush_interval_ms: 0,
    endpoint_url: "",
    headers: [],
    enabled_features: []
  }
  
  let empty_merged = merge_configs(default_config, empty_config)
  assert_eq(empty_merged.service_name, "")                  // 默认值为空
  assert_eq(empty_merged.service_version, "1.0.0")          // 默认
  assert_eq(empty_merged.environment, "development")        // 默认
  assert_eq(empty_merged.sampling_ratio, 10.0)              // 默认
  assert_eq(empty_merged.batch_size, 100)                   // 默认
  assert_eq(empty_merged.flush_interval_ms, 5000)           // 默认
  assert_eq(empty_merged.endpoint_url, "https://localhost:4317/api/v1/traces")  // 默认
  assert_eq(empty_merged.headers.length(), 0)               // 默认
  assert_eq(empty_merged.enabled_features, ["tracing"])     // 默认
  
  // 测试多层配置合并
  let base_env_config = {
    service_name: "",
    service_version: "1.0.0",
    environment: "production",
    sampling_ratio: 5.0,
    batch_size: 200,
    flush_interval_ms: 3000,
    endpoint_url: "https://telemetry.prod.example.com/api/v1/traces",
    headers: [("Environment", "production")],
    enabled_features: ["tracing", "metrics"]
  }
  
  let service_specific_config = {
    service_name: "order-service",
    service_version: "2.1.0",
    environment: "",  // 使用环境配置
    sampling_ratio: 15.0,
    batch_size: 0,    // 使用环境配置
    flush_interval_ms: 0,  // 使用环境配置
    endpoint_url: "",  // 使用环境配置
    headers: [],      // 使用环境配置
    enabled_features: ["logs"]  // 覆盖环境配置
  }
  
  let final_config = merge_configs(base_env_config, service_specific_config)
  
  assert_eq(final_config.service_name, "order-service")     // 服务特定
  assert_eq(final_config.service_version, "2.1.0")          // 服务特定
  assert_eq(final_config.environment, "production")         // 环境
  assert_eq(final_config.sampling_ratio, 15.0)              // 服务特定
  assert_eq(final_config.batch_size, 200)                   // 环境
  assert_eq(final_config.flush_interval_ms, 3000)           // 环境
  assert_eq(final_config.endpoint_url, "https://telemetry.prod.example.com/api/v1/traces")  // 环境
  assert_eq(final_config.headers.length(), 1)               // 环境
  assert_eq(final_config.headers[0], ("Environment", "production"))
  assert_eq(final_config.enabled_features, ["logs"])        // 服务特定
}

// 测试3: 动态配置更新
test "dynamic configuration updates" {
  // 定义配置更新事件
  type ConfigUpdateEvent = {
    field: String,
    old_value: String,
    new_value: String,
    timestamp: Int
  }
  
  // 定义配置管理器
  type ConfigManager = {
    current_config: TelemetryConfig,
    update_history: Array[ConfigUpdateEvent],
    listeners: Array[(String) -> ()]
  }
  
  // 创建配置管理器
  let create_config_manager = fn(initial_config: TelemetryConfig) {
    {
      current_config: initial_config,
      update_history: [],
      listeners: []
    }
  }
  
  // 更新配置字段
  let update_config_field = fn(manager: ConfigManager, field: String, value: String) {
    let old_value = match field {
      "service_name" => manager.current_config.service_name
      "service_version" => manager.current_config.service_version
      "environment" => manager.current_config.environment
      "sampling_ratio" => manager.current_config.sampling_ratio.to_string()
      "batch_size" => manager.current_config.batch_size.to_string()
      "flush_interval_ms" => manager.current_config.flush_interval_ms.to_string()
      "endpoint_url" => manager.current_config.endpoint_url
      _ => ""
    }
    
    // 创建更新事件
    let update_event = {
      field,
      old_value,
      new_value: value,
      timestamp: 1640995200
    }
    
    // 更新配置
    let updated_config = match field {
      "service_name" => { manager.current_config | service_name: value }
      "service_version" => { manager.current_config | service_version: value }
      "environment" => { manager.current_config | environment: value }
      "sampling_ratio" => { manager.current_config | sampling_ratio: value.to_float() }
      "batch_size" => { manager.current_config | batch_size: value.to_int() }
      "flush_interval_ms" => { manager.current_config | flush_interval_ms: value.to_int() }
      "endpoint_url" => { manager.current_config | endpoint_url: value }
      _ => manager.current_config
    }
    
    // 更新管理器
    let updated_manager = {
      current_config: updated_config,
      update_history: manager.update_history + [update_event],
      listeners: manager.listeners
    }
    
    // 通知监听器
    for listener in manager.listeners {
      listener(field)
    }
    
    updated_manager
  }
  
  // 创建初始配置和管理器
  let initial_config = {
    service_name: "inventory-service",
    service_version: "1.0.0",
    environment: "development",
    sampling_ratio: 10.0,
    batch_size: 100,
    flush_interval_ms: 5000,
    endpoint_url: "https://localhost:4317/api/v1/traces",
    headers: [],
    enabled_features: ["tracing"]
  }
  
  let config_manager = create_config_manager(initial_config)
  
  // 测试配置更新
  let updated_manager1 = update_config_field(config_manager, "sampling_ratio", "20.0")
  assert_eq(updated_manager1.current_config.sampling_ratio, 20.0)
  assert_eq(updated_manager1.update_history.length(), 1)
  assert_eq(updated_manager1.update_history[0].field, "sampling_ratio")
  assert_eq(updated_manager1.update_history[0].old_value, "10.0")
  assert_eq(updated_manager1.update_history[0].new_value, "20.0")
  
  // 测试多次更新
  let updated_manager2 = update_config_field(updated_manager1, "environment", "staging")
  assert_eq(updated_manager2.current_config.environment, "staging")
  assert_eq(updated_manager2.update_history.length(), 2)
  
  let updated_manager3 = update_config_field(updated_manager2, "batch_size", "200")
  assert_eq(updated_manager3.current_config.batch_size, 200)
  assert_eq(updated_manager3.update_history.length(), 3)
  
  // 测试配置回滚
  let rollback_config = fn(manager: ConfigManager, steps: Int) {
    if steps >= manager.update_history.length() {
      return manager
    }
    
    let mut rollback_config = manager.current_config
    let history_to_rollback = manager.update_history.slice(0, manager.update_history.length() - steps)
    
    for event in history_to_rollback {
      rollback_config = match event.field {
        "service_name" => { rollback_config | service_name: event.old_value }
        "service_version" => { rollback_config | service_version: event.old_value }
        "environment" => { rollback_config | environment: event.old_value }
        "sampling_ratio" => { rollback_config | sampling_ratio: event.old_value.to_float() }
        "batch_size" => { rollback_config | batch_size: event.old_value.to_int() }
        "flush_interval_ms" => { rollback_config | flush_interval_ms: event.old_value.to_int() }
        "endpoint_url" => { rollback_config | endpoint_url: event.old_value }
        _ => rollback_config
      }
    }
    
    {
      current_config: rollback_config,
      update_history: history_to_rollback,
      listeners: manager.listeners
    }
  }
  
  // 回滚一次
  let rolled_back_manager = rollback_config(updated_manager3, 1)
  assert_eq(rolled_back_manager.current_config.batch_size, 100)  // 回滚到原始值
  assert_eq(rolled_back_manager.current_config.environment, "staging")  // 保持更新
  assert_eq(rolled_back_manager.current_config.sampling_ratio, 20.0)   // 保持更新
  assert_eq(rolled_back_manager.update_history.length(), 2)
  
  // 回滚所有更改
  let fully_rolled_back = rollback_config(rolled_back_manager, 2)
  assert_eq(fully_rolled_back.current_config.sampling_ratio, 10.0)  // 回滚到原始值
  assert_eq(fully_rolled_back.current_config.environment, "development")  // 回滚到原始值
  assert_eq(fully_rolled_back.current_config.batch_size, 100)  // 保持回滚状态
  assert_eq(fully_rolled_back.update_history.length(), 0)
}

// 测试4: 环境特定配置
test "environment-specific configurations" {
  // 定义环境配置模板
  type EnvironmentConfigTemplate = {
    environments: Array[String],
    common_config: TelemetryConfig,
    environment_overrides: Array[(String, TelemetryConfig)]
  }
  
  // 创建环境配置模板
  let env_template = {
    environments: ["development", "staging", "production"],
    common_config: {
      service_name: "notification-service",
      service_version: "1.0.0",
      environment: "",
      sampling_ratio: 10.0,
      batch_size: 100,
      flush_interval_ms: 5000,
      endpoint_url: "",
      headers: [],
      enabled_features: ["tracing"]
    },
    environment_overrides: [
      ("development", {
        service_name: "",
        service_version: "",
        environment: "development",
        sampling_ratio: 100.0,  // 开发环境全采样
        batch_size: 10,         // 小批次快速刷新
        flush_interval_ms: 1000,
        endpoint_url: "http://localhost:4317/api/v1/traces",
        headers: [],
        enabled_features: ["tracing", "metrics", "logs"]  // 开发环境启用所有功能
      }),
      ("staging", {
        service_name: "",
        service_version: "",
        environment: "staging",
        sampling_ratio: 50.0,   // 测试环境中等采样
        batch_size: 50,
        flush_interval_ms: 3000,
        endpoint_url: "https://telemetry.staging.example.com/api/v1/traces",
        headers: [("Environment", "staging")],
        enabled_features: ["tracing", "metrics"]
      }),
      ("production", {
        service_name: "",
        service_version: "",
        environment: "production",
        sampling_ratio: 1.0,    // 生产环境低采样
        batch_size: 500,        // 大批次减少网络开销
        flush_interval_ms: 10000,
        endpoint_url: "https://telemetry.prod.example.com/api/v1/traces",
        headers: [("Environment", "production"), ("Authorization", "Bearer prod-token")],
        enabled_features: ["tracing"]  // 生产环境只启用核心功能
      })
    ]
  }
  
  // 获取环境特定配置
  let get_environment_config = fn(template: EnvironmentConfigTemplate, environment: String) {
    let mut env_override = None
    
    for (env, config) in template.environment_overrides {
      if env == environment {
        env_override = Some(config)
        break
      }
    }
    
    match env_override {
      Some(override_config) => merge_configs(template.common_config, override_config)
      None => template.common_config
    }
  }
  
  // 测试开发环境配置
  let dev_config = get_environment_config(env_template, "development")
  assert_eq(dev_config.service_name, "notification-service")  // 通用配置
  assert_eq(dev_config.service_version, "1.0.0")              // 通用配置
  assert_eq(dev_config.environment, "development")            // 环境特定
  assert_eq(dev_config.sampling_ratio, 100.0)                 // 环境特定
  assert_eq(dev_config.batch_size, 10)                        // 环境特定
  assert_eq(dev_config.flush_interval_ms, 1000)               // 环境特定
  assert_eq(dev_config.endpoint_url, "http://localhost:4317/api/v1/traces")  // 环境特定
  assert_eq(dev_config.headers.length(), 0)                   // 环境特定
  assert_eq(dev_config.enabled_features, ["tracing", "metrics", "logs"])  // 环境特定
  
  // 测试测试环境配置
  let staging_config = get_environment_config(env_template, "staging")
  assert_eq(staging_config.service_name, "notification-service")  // 通用配置
  assert_eq(staging_config.service_version, "1.0.0")              // 通用配置
  assert_eq(staging_config.environment, "staging")                // 环境特定
  assert_eq(staging_config.sampling_ratio, 50.0)                  // 环境特定
  assert_eq(staging_config.batch_size, 50)                        // 环境特定
  assert_eq(staging_config.flush_interval_ms, 3000)               // 环境特定
  assert_eq(staging_config.endpoint_url, "https://telemetry.staging.example.com/api/v1/traces")  // 环境特定
  assert_eq(staging_config.headers.length(), 1)                   // 环境特定
  assert_eq(staging_config.headers[0], ("Environment", "staging"))
  assert_eq(staging_config.enabled_features, ["tracing", "metrics"])  // 环境特定
  
  // 测试生产环境配置
  let prod_config = get_environment_config(env_template, "production")
  assert_eq(prod_config.service_name, "notification-service")  // 通用配置
  assert_eq(prod_config.service_version, "1.0.0")              // 通用配置
  assert_eq(prod_config.environment, "production")             // 环境特定
  assert_eq(prod_config.sampling_ratio, 1.0)                   // 环境特定
  assert_eq(prod_config.batch_size, 500)                       // 环境特定
  assert_eq(prod_config.flush_interval_ms, 10000)              // 环境特定
  assert_eq(prod_config.endpoint_url, "https://telemetry.prod.example.com/api/v1/traces")  // 环境特定
  assert_eq(prod_config.headers.length(), 2)                   // 环境特定
  assert_eq(prod_config.headers[0], ("Environment", "production"))
  assert_eq(prod_config.headers[1], ("Authorization", "Bearer prod-token"))
  assert_eq(prod_config.enabled_features, ["tracing"])         // 环境特定
  
  // 测试未知环境（回退到通用配置）
  let unknown_config = get_environment_config(env_template, "testing")
  assert_eq(unknown_config.service_name, "notification-service")  // 通用配置
  assert_eq(unknown_config.service_version, "1.0.0")              // 通用配置
  assert_eq(unknown_config.environment, "")                      // 通用配置（空）
  assert_eq(unknown_config.sampling_ratio, 10.0)                 // 通用配置
  assert_eq(unknown_config.batch_size, 100)                      // 通用配置
  assert_eq(unknown_config.flush_interval_ms, 5000)              // 通用配置
  assert_eq(unknown_config.endpoint_url, "")                     // 通用配置（空）
  assert_eq(unknown_config.headers.length(), 0)                  // 通用配置
  assert_eq(unknown_config.enabled_features, ["tracing"])        // 通用配置
  
  // 测试配置验证
  let validate_environment_config = fn(config: TelemetryConfig, environment: String) {
    let mut warnings = []
    
    // 环境特定验证
    match environment {
      "production" => {
        if config.sampling_ratio > 10.0 {
          warnings = warnings.push("Production sampling ratio should be low to reduce overhead")
        }
        
        if config.batch_size < 100 {
          warnings = warnings.push("Production batch size should be large for efficiency")
        }
        
        if not(config.endpoint_url.starts_with("https://")) {
          warnings = warnings.push("Production should use HTTPS endpoint")
        }
      }
      "development" => {
        if config.sampling_ratio < 50.0 {
          warnings = warnings.push("Development sampling ratio should be high for debugging")
        }
      }
      "staging" => {
        if config.enabled_features.length() < 2 {
          warnings = warnings.push("Staging should enable multiple features for testing")
        }
      }
      _ => ()
    }
    
    warnings
  }
  
  // 验证开发环境配置
  let dev_warnings = validate_environment_config(dev_config, "development")
  assert_eq(dev_warnings.length(), 0)  // 开发配置应该有效
  
  // 验证生产环境配置
  let prod_warnings = validate_environment_config(prod_config, "production")
  assert_eq(prod_warnings.length(), 0)  // 生产配置应该有效
  
  // 测试无效的生产配置
  let invalid_prod_config = { prod_config | 
    sampling_ratio: 50.0,  // 生产环境采样率过高
    batch_size: 10,        // 生产环境批次大小过小
    endpoint_url: "http://insecure.example.com"  // 生产环境使用HTTP
  }
  
  let invalid_prod_warnings = validate_environment_config(invalid_prod_config, "production")
  assert_eq(invalid_prod_warnings.length(), 3)  // 应该有3个警告
}