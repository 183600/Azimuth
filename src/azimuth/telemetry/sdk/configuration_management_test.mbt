// 配置管理系统测试
// 测试配置管理、环境变量支持、配置验证和动态配置更新等功能

test "configuration_hierarchy_and_precedence" {
  // 测试配置层次结构和优先级
  
  enum ConfigSource {
    Default
    Environment
    File
    Programmatic
    CommandLine
  }
  
  struct ConfigValue {
    value: String
    source: ConfigSource
    priority: Int
    overridden: Bool
  }
  
  struct ConfigurationManager {
    configurations: Map[String, ConfigValue]
    sources: Array[ConfigSource]
    validation_rules: Map[String, (String) -> Bool]
  }
  
  // 配置源优先级
  let get_source_priority = fn(source: ConfigSource) -> Int {
    match source {
      Default => 1,
      Environment => 2,
      File => 3,
      Programmatic => 4,
      CommandLine => 5
    }
  }
  
  // 创建配置管理器
  let create_config_manager = fn() -> ConfigurationManager {
    let default_configs = Map::from_array([
      ("service_name", ConfigValue{ value: "default-service", source: Default, priority: 1, overridden: false }),
      ("service_version", ConfigValue{ value: "1.0.0", source: Default, priority: 1, overridden: false }),
      ("endpoint", ConfigValue{ value: "http://localhost:4317", source: Default, priority: 1, overridden: false }),
      ("batch_size", ConfigValue{ value: "512", source: Default, priority: 1, overridden: false }),
      ("enable_compression", ConfigValue{ value: "true", source: Default, priority: 1, overridden: false })
    ])
    
    ConfigurationManager{
      configurations: default_configs,
      sources: [Default],
      validation_rules: Map::empty()
    }
  }
  
  // 添加配置
  let add_configuration = fn(
    manager: ConfigurationManager,
    key: String,
    value: String,
    source: ConfigSource
  ) -> ConfigurationManager {
    let priority = get_source_priority(source)
    let existing_config = manager.configurations.get(key)
    
    let (new_config, should_update) = match existing_config {
      Some(existing) => {
        if priority > existing.priority {
          // 高优先级覆盖低优先级
          let overridden_config = { existing | overridden: true }
          let new_config = ConfigValue{
            value: value,
            source: source,
            priority: priority,
            overridden: false
          }
          (new_config, true)
        } else {
          // 低优先级被高优先级覆盖
          let overridden_config = { existing | overridden: true }
          (overridden_config, false)
        }
      }
      None => {
        // 新配置
        let new_config = ConfigValue{
          value: value,
          source: source,
          priority: priority,
          overridden: false
        }
        (new_config, true)
      }
    }
    
    let mut new_configurations = manager.configurations
    if should_update {
      new_configurations.insert(key, new_config)
    }
    
    // 更新源列表
    let mut new_sources = manager.sources.to_array()
    if !new_sources.contains(source) {
      new_sources.push(source)
    }
    
    { manager |
      configurations: new_configurations,
      sources: new_sources
    }
  }
  
  // 获取最终配置值
  let get_configuration = fn(manager: ConfigurationManager, key: String) -> Option[String] {
    match manager.configurations.get(key) {
      Some(config) => {
        if !config.overridden {
          Some(config.value)
        } else {
          None  // 被覆盖的配置
        }
      }
      None => None
    }
  }
  
  // 获取配置来源
  let get_configuration_source = fn(manager: ConfigurationManager, key: String) -> Option[ConfigSource] {
    match manager.configurations.get(key) {
      Some(config) => {
        if !config.overridden {
          Some(config.source)
        } else {
          None
        }
      }
      None => None
    }
  }
  
  // 测试配置层次结构
  let manager = create_config_manager()
  
  // 验证默认配置
  assert_eq(get_configuration(manager, "service_name"), Some("default-service"))
  assert_eq(get_configuration_source(manager, "service_name"), Some(Default))
  
  // 添加环境变量配置（更高优先级）
  let manager_with_env = add_configuration(manager, "service_name", "env-service", Environment)
  
  assert_eq(get_configuration(manager_with_env, "service_name"), Some("env-service"))
  assert_eq(get_configuration_source(manager_with_env, "service_name"), Some(Environment))
  
  // 添加文件配置（更高优先级）
  let manager_with_file = add_configuration(manager_with_env, "service_name", "file-service", File)
  
  assert_eq(get_configuration(manager_with_file, "service_name"), Some("file-service"))
  assert_eq(get_configuration_source(manager_with_file, "service_name"), Some(File))
  
  // 添加程序化配置（更高优先级）
  let manager_with_prog = add_configuration(manager_with_file, "service_name", "prog-service", Programmatic)
  
  assert_eq(get_configuration(manager_with_prog, "service_name"), Some("prog-service"))
  assert_eq(get_configuration_source(manager_with_prog, "service_name"), Some(Programmatic))
  
  // 添加命令行配置（最高优先级）
  let manager_with_cli = add_configuration(manager_with_prog, "service_name", "cli-service", CommandLine)
  
  assert_eq(get_configuration(manager_with_cli, "service_name"), Some("cli-service"))
  assert_eq(get_configuration_source(manager_with_cli, "service_name"), Some(CommandLine))
  
  // 测试低优先级无法覆盖高优先级
  let manager_final = add_configuration(manager_with_cli, "service_name", "low-priority", Default)
  
  assert_eq(get_configuration(manager_final, "service_name"), Some("cli-service"))  // 仍然是命令行配置
  assert_eq(get_configuration_source(manager_final, "service_name"), Some(CommandLine))
  
  // 测试不同配置键的独立管理
  let manager_multi = add_configuration(
    add_configuration(manager_with_env, "batch_size", "1024", CommandLine),
    "endpoint",
    "http://prod:4317",
    File
  )
  
  assert_eq(get_configuration(manager_multi, "service_name"), Some("env-service"))
  assert_eq(get_configuration(manager_multi, "batch_size"), Some("1024"))
  assert_eq(get_configuration(manager_multi, "endpoint"), Some("http://prod:4317"))
  
  assert_eq(get_configuration_source(manager_multi, "service_name"), Some(Environment))
  assert_eq(get_configuration_source(manager_multi, "batch_size"), Some(CommandLine))
  assert_eq(get_configuration_source(manager_multi, "endpoint"), Some(File))
}

test "environment_variable_configuration" {
  // 测试环境变量配置
  
  struct EnvironmentConfig {
    variables: Map[String, String]
    prefix: String
    separator: String
  }
  
  struct EnvVarMapping {
    env_var_name: String
    config_key: String
    required: Bool
    default_value: Option[String]
    transform_fn: Option[(String) -> String]
  }
  
  // 创建环境配置
  let create_environment_config = fn(prefix: String) -> EnvironmentConfig {
    EnvironmentConfig{
      variables: Map::from_array([
        ("OTEL_SERVICE_NAME", "production-service"),
        ("OTEL_SERVICE_VERSION", "2.1.0"),
        ("OTEL_EXPORTER_OTLP_ENDPOINT", "https://otel-collector:4317"),
        ("OTEL_BATCH_SIZE", "2048"),
        ("OTEL_ENABLE_COMPRESSION", "false"),
        ("OTEL_RESOURCE_ATTRIBUTES", "service.name=prod,env=production"),
        ("CUSTOM_TELEMETRY_ENABLED", "true"),
        ("CUSTOM_DEBUG_MODE", "false")
      ]),
      prefix: prefix,
      separator: "_"
    }
  }
  
  // 创建环境变量映射
  let create_env_mappings = fn() -> Array[EnvVarMapping] {
    [
      EnvVarMapping{
        env_var_name: "OTEL_SERVICE_NAME",
        config_key: "service_name",
        required: true,
        default_value: None,
        transform_fn: None
      },
      EnvVarMapping{
        env_var_name: "OTEL_SERVICE_VERSION",
        config_key: "service_version",
        required: false,
        default_value: Some("1.0.0"),
        transform_fn: None
      },
      EnvVarMapping{
        env_var_name: "OTEL_EXPORTER_OTLP_ENDPOINT",
        config_key: "endpoint",
        required: true,
        default_value: Some("http://localhost:4317"),
        transform_fn: None
      },
      EnvVarMapping{
        env_var_name: "OTEL_BATCH_SIZE",
        config_key: "batch_size",
        required: false,
        default_value: Some("512"),
        transform_fn: Some(fn(value: String) -> String {
          // 确保是有效的整数
          match value.parse_int() {
            Some(int_val) => int_val.to_string(),
            None => "512"  // 默认值
          }
        })
      },
      EnvVarMapping{
        env_var_name: "OTEL_ENABLE_COMPRESSION",
        config_key: "enable_compression",
        required: false,
        default_value: Some("true"),
        transform_fn: Some(fn(value: String) -> String {
          // 标准化布尔值
          match value.to_lowercase() {
            "true" | "1" | "yes" | "on" => "true",
            "false" | "0" | "no" | "off" => "false",
            _ => "true"  // 默认值
          }
        })
      },
      EnvVarMapping{
        env_var_name: "OTEL_RESOURCE_ATTRIBUTES",
        config_key: "resource_attributes",
        required: false,
        default_value: None,
        transform_fn: Some(fn(value: String) -> String {
          // 解析键值对
          value.split(",").map(fn(pair) {
            let parts = pair.split("=")
            if parts.length() == 2 {
              parts[0].trim() + ":" + parts[1].trim()
            } else {
              pair
            }
          }).join(";")
        })
      }
    ]
  }
  
  // 从环境变量加载配置
  let load_from_environment = fn(
    env_config: EnvironmentConfig,
    mappings: Array[EnvVarMapping]
  ) -> Map[String, String] {
    let mut configurations = Map::empty<String, String>()
    
    let mut i = 0
    while i < mappings.length() {
      let mapping = mappings[i]
      
      match env_config.variables.get(mapping.env_var_name) {
        Some(env_value) => {
          // 应用转换函数
          let final_value = match mapping.transform_fn {
            Some(transform_fn) => transform_fn(env_value)
            None => env_value
          }
          
          configurations.insert(mapping.config_key, final_value)
        }
        None => {
          // 使用默认值
          match mapping.default_value {
            Some(default) => {
              configurations.insert(mapping.config_key, default)
            }
            None => {
              if mapping.required {
                // 必需的环境变量缺失
                configurations.insert(mapping.config_key + "_error", "missing_required_env")
              }
            }
          }
        }
      }
      
      i = i + 1
    }
    
    configurations
  }
  
  // 验证环境变量配置
  let validate_env_config = fn(configurations: Map[String, String]) -> (Bool, Array[String]) {
    let mut errors = Array::empty<String>()
    let mut is_valid = true
    
    // 验证必需配置
    match configurations.get("service_name") {
      Some(value) => {
        if value.length() == 0 || value == "missing_required_env" {
          errors.push("service_name is required but missing or invalid")
          is_valid = false
        }
      }
      None => {
        errors.push("service_name is required")
        is_valid = false
      }
    }
    
    match configurations.get("endpoint") {
      Some(value) => {
        if !value.starts_with("http://") && !value.starts_with("https://") {
          errors.push("endpoint must be a valid URL")
          is_valid = false
        }
      }
      None => {
        errors.push("endpoint is required")
        is_valid = false
      }
    }
    
    // 验证数值配置
    match configurations.get("batch_size") {
      Some(value) => {
        match value.parse_int() {
          Some(batch_size) => {
            if batch_size <= 0 || batch_size > 10000 {
              errors.push("batch_size must be between 1 and 10000")
              is_valid = false
            }
          }
          None => {
            errors.push("batch_size must be a valid integer")
            is_valid = false
          }
        }
      }
      None => {}
    }
    
    // 验证布尔配置
    match configurations.get("enable_compression") {
      Some(value) => {
        if value != "true" && value != "false" {
          errors.push("enable_compression must be 'true' or 'false'")
          is_valid = false
        }
      }
      None => {}
    }
    
    (is_valid, errors)
  }
  
  // 创建测试环境配置
  let env_config = create_environment_config("OTEL")
  let mappings = create_env_mappings()
  
  // 加载配置
  let configurations = load_from_environment(env_config, mappings)
  
  // 验证加载的配置
  assert_eq(configurations.get("service_name"), Some("production-service"))
  assert_eq(configurations.get("service_version"), Some("2.1.0"))
  assert_eq(configurations.get("endpoint"), Some("https://otel-collector:4317"))
  assert_eq(configurations.get("batch_size"), Some("2048"))
  assert_eq(configurations.get("enable_compression"), Some("false"))
  
  // 验证转换函数
  assert_eq(configurations.get("resource_attributes"), Some("service.name:prod;env:production"))
  
  // 验证配置有效性
  let (is_valid, errors) = validate_env_config(configurations)
  assert_eq(is_valid, true)
  assert_eq(errors.length(), 0)
  
  // 测试缺失必需环境变量的情况
  let incomplete_env = { env_config |
    variables: Map::from_array([
      ("OTEL_SERVICE_VERSION", "2.1.0")
      // 缺少 OTEL_SERVICE_NAME 和 OTEL_EXPORTER_OTLP_ENDPOINT
    ])
  }
  
  let incomplete_configs = load_from_environment(incomplete_env, mappings)
  let (incomplete_valid, incomplete_errors) = validate_env_config(incomplete_configs)
  
  assert_eq(incomplete_valid, false)
  assert_eq(incomplete_errors.length() >= 2, true)
  assert_eq(incomplete_errors.any(fn(err) { err.contains("service_name") }), true)
  assert_eq(incomplete_errors.any(fn(err) { err.contains("endpoint") }), true)
}

test "dynamic_configuration_updates" {
  // 测试动态配置更新
  
  enum ConfigurationChangeType {
    Add
    Update
    Remove
  }
  
  struct ConfigurationChange {
    change_type: ConfigurationChangeType
    key: String
    old_value: Option[String]
    new_value: Option[String]
    timestamp: Int64
    source: String
  }
  
  struct DynamicConfigurationManager {
    current_config: Map[String, String]
    change_history: Array[ConfigurationChange]
    subscribers: Array[(ConfigurationChange) -> Unit]
    max_history_size: Int
  }
  
  // 创建动态配置管理器
  let create_dynamic_config_manager = fn(max_history: Int) -> DynamicConfigurationManager {
    let initial_config = Map::from_array([
      ("service_name", "dynamic-service"),
      ("batch_size", "256"),
      ("enable_compression", "true"),
      ("log_level", "info")
    ])
    
    DynamicConfigurationManager{
      current_config: initial_config,
      change_history: Array::empty(),
      subscribers: Array::empty(),
      max_history_size: max_history
    }
  }
  
  // 更新配置
  let update_configuration = fn(
    manager: DynamicConfigurationManager,
    key: String,
    new_value: String,
    source: String
  ) -> DynamicConfigurationManager {
    let old_value = manager.current_config.get(key)
    let timestamp = 1640995200000000L  // 简化的时间戳
    
    let change_type = match old_value {
      Some(_) => Update,
      None => Add
    }
    
    let change = ConfigurationChange{
      change_type: change_type,
      key: key,
      old_value: old_value,
      new_value: Some(new_value),
      timestamp: timestamp,
      source: source
    }
    
    // 更新配置
    let mut new_config = manager.current_config
    new_config.insert(key, new_value)
    
    // 更新历史记录
    let mut new_history = manager.change_history.to_array()
    new_history.push(change)
    
    // 限制历史记录大小
    if new_history.length() > manager.max_history_size {
      new_history = new_history.slice(new_history.length() - manager.max_history_size, new_history.length())
    }
    
    let updated_manager = { manager |
      current_config: new_config,
      change_history: new_history
    }
    
    // 通知订阅者
    let mut i = 0
    while i < updated_manager.subscribers.length() {
      let subscriber = updated_manager.subscribers[i]
      subscriber(change)
      i = i + 1
    }
    
    updated_manager
  }
  
  // 移除配置
  let remove_configuration = fn(
    manager: DynamicConfigurationManager,
    key: String,
    source: String
  ) -> DynamicConfigurationManager {
    let old_value = manager.current_config.get(key)
    let timestamp = 1640995200000000L
    
    match old_value {
      Some(_) => {
        let change = ConfigurationChange{
          change_type: Remove,
          key: key,
          old_value: old_value,
          new_value: None,
          timestamp: timestamp,
          source: source
        }
        
        // 移除配置
        let mut new_config = manager.current_config
        new_config.remove(key)
        
        // 更新历史记录
        let mut new_history = manager.change_history.to_array()
        new_history.push(change)
        
        // 限制历史记录大小
        if new_history.length() > manager.max_history_size {
          new_history = new_history.slice(new_history.length() - manager.max_history_size, new_history.length())
        }
        
        let updated_manager = { manager |
          current_config: new_config,
          change_history: new_history
        }
        
        // 通知订阅者
        let mut i = 0
        while i < updated_manager.subscribers.length() {
          let subscriber = updated_manager.subscribers[i]
          subscriber(change)
          i = i + 1
        }
        
        updated_manager
      }
      None => {
        // 配置不存在，无需移除
        manager
      }
    }
  }
  
  // 添加订阅者
  let add_subscriber = fn(
    manager: DynamicConfigurationManager,
    subscriber: (ConfigurationChange) -> Unit
  ) -> DynamicConfigurationManager {
    let mut new_subscribers = manager.subscribers.to_array()
    new_subscribers.push(subscriber)
    
    { manager |
      subscribers: new_subscribers
    }
  }
  
  // 测试动态配置更新
  let manager = create_dynamic_config_manager(10)
  
  // 验证初始配置
  assert_eq(manager.current_config.get("service_name"), Some("dynamic-service"))
  assert_eq(manager.current_config.get("batch_size"), Some("256"))
  assert_eq(manager.change_history.length(), 0)
  
  // 添加订阅者
  let mut notified_changes = Array::empty<ConfigurationChange>()
  
  let test_subscriber = fn(change: ConfigurationChange) {
    notified_changes.push(change)
  }
  
  let manager_with_subscriber = add_subscriber(manager, test_subscriber)
  assert_eq(manager_with_subscriber.subscribers.length(), 1)
  
  // 更新配置
  let updated_manager = update_configuration(
    manager_with_subscriber,
    "batch_size",
    "512",
    "api_update"
  )
  
  // 验证配置更新
  assert_eq(updated_manager.current_config.get("batch_size"), Some("512"))
  assert_eq(updated_manager.change_history.length(), 1)
  assert_eq(notified_changes.length(), 1)
  
  let change = updated_manager.change_history[0]
  assert_eq(change.change_type, Update)
  assert_eq(change.key, "batch_size")
  assert_eq(change.old_value, Some("256"))
  assert_eq(change.new_value, Some("512"))
  assert_eq(change.source, "api_update")
  
  // 添加新配置
  let manager_with_new_config = update_configuration(
    updated_manager,
    "new_feature_enabled",
    "true",
    "feature_flag"
  )
  
  assert_eq(manager_with_new_config.current_config.get("new_feature_enabled"), Some("true"))
  assert_eq(manager_with_new_config.change_history.length(), 2)
  assert_eq(notified_changes.length(), 2)
  
  let new_change = manager_with_new_config.change_history[1]
  assert_eq(new_change.change_type, Add)
  assert_eq(new_change.key, "new_feature_enabled")
  assert_eq(new_change.old_value, None)
  assert_eq(new_change.new_value, Some("true"))
  
  // 移除配置
  let manager_with_removal = remove_configuration(
    manager_with_new_config,
    "log_level",
    "cleanup"
  )
  
  assert_eq(manager_with_removal.current_config.contains("log_level"), false)
  assert_eq(manager_with_removal.change_history.length(), 3)
  assert_eq(notified_changes.length(), 3)
  
  let removal_change = manager_with_removal.change_history[2]
  assert_eq(removal_change.change_type, Remove)
  assert_eq(removal_change.key, "log_level")
  assert_eq(removal_change.old_value, Some("info"))
  assert_eq(removal_change.new_value, None)
  
  // 测试历史记录限制
  let mut final_manager = manager_with_removal
  let mut i = 0
  while i < 15 {  // 添加超过限制的变更
    final_manager = update_configuration(
      final_manager,
      "temp_config_" + i.to_string(),
      "value_" + i.to_string(),
      "test"
    )
    i = i + 1
  }
  
  assert_eq(final_manager.change_history.length(), final_manager.max_history_size)
  
  // 验证历史记录包含最新的变更
  let latest_change = final_manager.change_history[final_manager.change_history.length() - 1]
  assert_eq(latest_change.key, "temp_config_14")
}

test "configuration_validation_and_constraints" {
  // 测试配置验证和约束
  
  enum ValidationType {
    Required
    TypeCheck
    RangeCheck
    PatternMatch
    CustomValidation
  }
  
  struct ValidationRule {
    validation_type: ValidationType
    constraint: String
    error_message: String
    validation_fn: Option[(String) -> Bool]
  }
  
  struct ValidationResult {
    is_valid: Bool
    errors: Array[String]
    warnings: Array[String]
    normalized_value: Option[String]
  }
  
  struct ConfigurationSchema {
    rules: Map[String, Array[ValidationRule]]
    dependencies: Map[String, Array[String]]  // 配置依赖关系
  }
  
  // 创建配置模式
  let create_configuration_schema = fn() -> ConfigurationSchema {
    let rules = Map::from_array([
      ("service_name", [
        ValidationRule{
          validation_type: Required,
          constraint: "non_empty",
          error_message: "service_name is required and cannot be empty",
          validation_fn: Some(fn(value: String) -> Bool { value.length() > 0 })
        },
        ValidationRule{
          validation_type: PatternMatch,
          constraint: "^[a-zA-Z][a-zA-Z0-9-_]*$",
          error_message: "service_name must start with letter and contain only alphanumeric, dash, underscore",
          validation_fn: Some(fn(value: String) -> Bool {
            value.length() > 0 && 
            value.char_code(0).is_alpha() &&
            value.to_array().all(fn(c) { c.is_alpha() || c.is_digit() || c == '-' || c == '_' })
          })
        }
      ]),
      ("service_version", [
        ValidationRule{
          validation_type: PatternMatch,
          constraint: "^\\d+\\.\\d+\\.\\d+(-.*)?$",
          error_message: "service_version must follow semantic versioning (x.y.z)",
          validation_fn: Some(fn(value: String) -> Bool {
            let parts = value.split(".")
            parts.length() >= 3 && 
            parts[0].parse_int().is_some() &&
            parts[1].parse_int().is_some() &&
            parts[2].split("-")[0].parse_int().is_some()
          })
        }
      ]),
      ("batch_size", [
        ValidationRule{
          validation_type: TypeCheck,
          constraint: "integer",
          error_message: "batch_size must be an integer",
          validation_fn: Some(fn(value: String) -> Bool { value.parse_int().is_some() })
        },
        ValidationRule{
          validation_type: RangeCheck,
          constraint: "1-10000",
          error_message: "batch_size must be between 1 and 10000",
          validation_fn: Some(fn(value: String) -> Bool {
            match value.parse_int() {
              Some(int_val) => int_val >= 1 && int_val <= 10000,
              None => false
            }
          })
        }
      ]),
      ("sampling_ratio", [
        ValidationRule{
          validation_type: TypeCheck,
          constraint: "float",
          error_message: "sampling_ratio must be a float",
          validation_fn: Some(fn(value: String) -> Bool { value.parse_float().is_some() })
        },
        ValidationRule{
          validation_type: RangeCheck,
          constraint: "0.0-1.0",
          error_message: "sampling_ratio must be between 0.0 and 1.0",
          validation_fn: Some(fn(value: String) -> Bool {
            match value.parse_float() {
              Some(float_val) => float_val >= 0.0 && float_val <= 1.0,
              None => false
            }
          })
        }
      ]),
      ("endpoint", [
        ValidationRule{
          validation_type: Required,
          constraint: "url",
          error_message: "endpoint is required and must be a valid URL",
          validation_fn: Some(fn(value: String) -> Bool {
            value.starts_with("http://") || value.starts_with("https://")
          })
        }
      ])
    ])
    
    let dependencies = Map::from_array([
      ("sampling_enabled", ["sampling_ratio"]),
      ("compression_enabled", ["compression_level"]),
      ("advanced_mode", ["debug_mode", "verbose_logging"])
    ])
    
    ConfigurationSchema{
      rules: rules,
      dependencies: dependencies
    }
  }
  
  // 验证配置值
  let validate_configuration = fn(
    schema: ConfigurationSchema,
    key: String,
    value: String
  ) -> ValidationResult {
    let mut errors = Array::empty<String>()
    let mut warnings = Array::empty<String>()
    let mut normalized_value = Some(value)
    
    match schema.rules.get(key) {
      Some(rules) => {
        let mut i = 0
        while i < rules.length() {
          let rule = rules[i]
          
          let is_valid = match rule.validation_fn {
            Some(validation_fn) => validation_fn(value),
            None => true  // 没有验证函数则认为有效
          }
          
          if !is_valid {
            errors.push(rule.error_message)
          }
          
          i = i + 1
        }
      }
      None => {
        warnings.push("Unknown configuration key: " + key)
      }
    }
    
    ValidationResult{
      is_valid: errors.length() == 0,
      errors: errors,
      warnings: warnings,
      normalized_value: normalized_value
    }
  }
  
  // 验证配置依赖
  let validate_dependencies = fn(
    schema: ConfigurationSchema,
    configurations: Map[String, String]
  ) -> Array[String] {
    let mut dependency_errors = Array::empty<String>()
    
    let config_keys = configurations.keys()
    let mut i = 0
    while i < config_keys.length() {
      let config_key = config_keys[i]
      
      match schema.dependencies.get(config_key) {
        Some(dependencies) => {
          let mut j = 0
          while j < dependencies.length() {
            let dependency = dependencies[j]
            
            if !configurations.contains(dependency) {
              dependency_errors.push(
                config_key + " requires " + dependency + " to be configured"
              )
            }
            
            j = j + 1
          }
        }
        None => {}
      }
      
      i = i + 1
    }
    
    dependency_errors
  }
  
  // 批量验证配置
  let validate_configurations = fn(
    schema: ConfigurationSchema,
    configurations: Map[String, String]
  ) -> (Bool, Array[String], Array[String]) {
    let mut all_errors = Array::empty<String>()
    let mut all_warnings = Array::empty<String>()
    
    // 验证每个配置
    let config_keys = configurations.keys()
    let mut i = 0
    while i < config_keys.length() {
      let key = config_keys[i]
      let value = configurations.get(key).unwrap_or("")
      
      let result = validate_configuration(schema, key, value)
      
      let mut j = 0
      while j < result.errors.length() {
        all_errors.push(result.errors[j])
        j = j + 1
      }
      
      let mut k = 0
      while k < result.warnings.length() {
        all_warnings.push(result.warnings[k])
        k = k + 1
      }
      
      i = i + 1
    }
    
    // 验证依赖关系
    let dependency_errors = validate_dependencies(schema, configurations)
    let mut l = 0
    while l < dependency_errors.length() {
      all_errors.push(dependency_errors[l])
      l = l + 1
    }
    
    (all_errors.length() == 0, all_errors, all_warnings)
  }
  
  // 创建配置模式
  let schema = create_configuration_schema()
  
  // 测试有效配置
  let valid_configs = Map::from_array([
    ("service_name", "my-service"),
    ("service_version", "1.2.3"),
    ("batch_size", "1024"),
    ("sampling_ratio", "0.5"),
    ("endpoint", "https://otel-collector:4317")
  ])
  
  let (valid_is_valid, valid_errors, valid_warnings) = validate_configurations(schema, valid_configs)
  
  assert_eq(valid_is_valid, true)
  assert_eq(valid_errors.length(), 0)
  
  // 测试无效配置
  let invalid_configs = Map::from_array([
    ("service_name", ""),  // 空字符串
    ("service_version", "not-a-version"),  // 无效版本格式
    ("batch_size", "20480"),  // 超出范围
    ("sampling_ratio", "1.5"),  // 超出范围
    ("endpoint", "not-a-url")  // 无效URL
  ])
  
  let (invalid_is_valid, invalid_errors, invalid_warnings) = validate_configurations(schema, invalid_configs)
  
  assert_eq(invalid_is_valid, false)
  assert_eq(invalid_errors.length(), 5)
  
  // 验证具体错误信息
  assert_eq(invalid_errors.any(fn(err) { err.contains("service_name") && err.contains("empty") }), true)
  assert_eq(invalid_errors.any(fn(err) { err.contains("service_version") && err.contains("semantic versioning") }), true)
  assert_eq(invalid_errors.any(fn(err) { err.contains("batch_size") && err.contains("between 1 and 10000") }), true)
  assert_eq(invalid_errors.any(fn(err) { err.contains("sampling_ratio") && err.contains("between 0.0 and 1.0") }), true)
  assert_eq(invalid_errors.any(fn(err) { err.contains("endpoint") && err.contains("valid URL") }), true)
  
  // 测试依赖关系验证
  let configs_with_dependencies = Map::from_array([
    ("service_name", "my-service"),
    ("sampling_enabled", "true")
    // 缺少 sampling_ratio 依赖
  ])
  
  let (dep_is_valid, dep_errors, dep_warnings) = validate_configurations(schema, configs_with_dependencies)
  
  assert_eq(dep_is_valid, false)
  assert_eq(dep_errors.any(fn(err) { err.contains("sampling_enabled") && err.contains("sampling_ratio") }), true)
  
  // 测试单个配置验证
  let single_result = validate_configuration(schema, "batch_size", "512")
  assert_eq(single_result.is_valid, true)
  assert_eq(single_result.errors.length(), 0)
  assert_eq(single_result.normalized_value, Some("512"))
  
  let invalid_single_result = validate_configuration(schema, "batch_size", "invalid")
  assert_eq(invalid_single_result.is_valid, false)
  assert_eq(invalid_single_result.errors.length(), 1)
  assert_eq(invalid_single_result.errors[0].contains("must be an integer"), true)
}