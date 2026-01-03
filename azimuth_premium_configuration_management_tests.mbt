// Azimuth Premium Configuration Management Tests
// 高级配置管理测试用例，专注于遥测系统的配置加载、验证和动态更新

// 测试 1: 配置加载和解析
test "configuration loading and parsing" {
  // 定义配置值类型
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String])
    MapValue(Map[String, String])
  }
  
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    required: Bool,
    validated: Bool
  }
  
  // 定义配置
  type Configuration = {
    items: Map[String, ConfigItem],
    sources: Array[String],
    last_updated: Int
  }
  
  // 创建配置管理器
  let create_config_manager = fn() {
    // 解析配置值
    let parse_config_value = fn(str_value: String, type_hint: String) {
      match type_hint {
        "string" => ConfigValue::StringValue(str_value)
        "int" => ConfigValue::IntValue(str_value.to_int())
        "float" => ConfigValue::FloatValue(str_value.to_float())
        "bool" => ConfigValue::BoolValue(str_value == "true" || str_value == "1")
        "array" => {
          let parts = str_value.split(",")
          ConfigValue::ArrayValue(parts)
        }
        "map" => {
          let pairs = str_value.split(";")
          let mut map = Map::empty()
          
          for pair in pairs {
            let key_value = pair.split(":")
            if key_value.length() == 2 {
              let _ = Map::insert(map, key_value[0], key_value[1])
            }
          }
          
          ConfigValue::MapValue(map)
        }
        _ => ConfigValue::StringValue(str_value)  // 默认为字符串
      }
    }
    
    // 从键值对加载配置
    let load_from_key_values = fn(key_values: Array[(String, String)], defaults: Map[String, (String, String, Bool)]) {
      let mut items = Map::empty()
      
      for (key, value) in key_values {
        match Map::get(defaults, key) {
          Some((type_hint, description, required)) => {
            let parsed_value = parse_config_value(value, type_hint)
            let default_value = parse_config_value("", type_hint)  // 空字符串作为默认值
            
            let config_item = {
              key,
              value: parsed_value,
              default_value,
              description,
              required,
              validated: true
            }
            
            let _ = Map::insert(items, key, config_item)
          }
          None => {
            // 没有默认定义，使用字符串类型
            let config_item = {
              key,
              value: ConfigValue::StringValue(value),
              default_value: ConfigValue::StringValue(""),
              description: "",
              required: false,
              validated: false
            }
            
            let _ = Map::insert(items, key, config_item)
          }
        }
      }
      
      // 添加默认值中未提供的配置项
      for (default_key, (type_hint, description, required)) in defaults {
        if not Map::contains_key(items, default_key) {
          let default_value = parse_config_value("", type_hint)
          
          let config_item = {
            key: default_key,
            value: default_value,
            default_value,
            description,
            required,
            validated: true
          }
          
          let _ = Map::insert(items, default_key, config_item)
        }
      }
      
      {
        items,
        sources: ["key_values"],
        last_updated: 1000
      }
    }
    
    // 从JSON字符串加载配置（简化版）
    let load_from_json = fn(json_str: String, defaults: Map[String, (String, String, Bool)]) {
      // 简化的JSON解析，实际项目中应使用专门的JSON库
      let mut key_values = []
      
      // 移除花括号
      let trimmed = json_str.substring(1, json_str.length() - 1)
      
      // 分割键值对
      let pairs = trimmed.split(",")
      
      for pair in pairs {
        let parts = pair.split(":")
        if parts.length() == 2 {
          let key = parts[0].trim().replace("\"", "")
          let value = parts[1].trim().replace("\"", "")
          key_values = key_values.push((key, value))
        }
      }
      
      let config = load_from_key_values(key_values, defaults)
      
      {
        items: config.items,
        sources: ["json"],
        last_updated: 1000
      }
    }
    
    {
      load_from_key_values,
      load_from_json,
      parse_config_value
    }
  }
  
  // 测试配置加载和解析
  let manager = create_config_manager()
  
  // 定义默认配置
  let defaults = Map::from_array([
    ("service.name", ("string", "服务名称", true)),
    ("service.version", ("string", "服务版本", true)),
    ("server.port", ("int", "服务器端口", true)),
    ("server.timeout", ("float", "服务器超时时间(秒)", false)),
    ("debug.enabled", ("bool", "是否启用调试模式", false)),
    ("log.levels", ("array", "日志级别列表", false)),
    ("feature.flags", ("map", "功能开关", false))
  ])
  
  // 测试从键值对加载
  let key_values = [
    ("service.name", "azimuth-telemetry"),
    ("service.version", "1.0.0"),
    ("server.port", "8080"),
    ("server.timeout", "30.5"),
    ("debug.enabled", "true"),
    ("log.levels", "debug,info,warn,error"),
    ("feature.flags", "feature1:true;feature2:false")
  ]
  
  let config1 = manager.load_from_key_values(key_values, defaults)
  
  // 验证配置项
  let service_name_item = Map::get(config1.items, "service.name")
  match service_name_item {
    Some(item) => {
      assert_eq(item.key, "service.name")
      match item.value {
        ConfigValue::StringValue(name) => assert_eq(name, "azimuth-telemetry")
        _ => assert_true(false)
      }
      assert_eq(item.description, "服务名称")
      assert_true(item.required)
      assert_true(item.validated)
    }
    None => assert_true(false)
  }
  
  let server_port_item = Map::get(config1.items, "server.port")
  match server_port_item {
    Some(item) => {
      match item.value {
        ConfigValue::IntValue(port) => assert_eq(port, 8080)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let debug_enabled_item = Map::get(config1.items, "debug.enabled")
  match debug_enabled_item {
    Some(item) => {
      match item.value {
        ConfigValue::BoolValue(enabled) => assert_true(enabled)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let log_levels_item = Map::get(config1.items, "log.levels")
  match log_levels_item {
    Some(item) => {
      match item.value {
        ConfigValue::ArrayValue(levels) => {
          assert_eq(levels.length(), 4)
          assert_eq(levels[0], "debug")
          assert_eq(levels[1], "info")
          assert_eq(levels[2], "warn")
          assert_eq(levels[3], "error")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试从JSON加载
  let json_str = "{\"service.name\":\"azimuth-telemetry\",\"service.version\":\"1.0.0\",\"server.port\":\"8080\"}"
  let config2 = manager.load_from_json(json_str, defaults)
  
  // 验证JSON加载
  let json_service_name_item = Map::get(config2.items, "service.name")
  match json_service_name_item {
    Some(item) => {
      match item.value {
        ConfigValue::StringValue(name) => assert_eq(name, "azimuth-telemetry")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证默认值填充
  let server_timeout_item = Map::get(config2.items, "server.timeout")
  match server_timeout_item {
    Some(item) => {
      match item.value {
        ConfigValue::FloatValue(timeout) => assert_eq(timeout, 0.0)  // 默认值
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试 2: 配置验证
test "configuration validation" {
  // 定义配置值类型
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
  }
  
  // 定义验证规则
  enum ValidationRule {
    Required
    Min(Int)
    Max(Int)
    Range(Int, Int)
    MinLength(Int)
    MaxLength(Int)
    Pattern(String)  // 正则表达式
    OneOf(Array[String])  // 枚举值
    Custom(String -> Bool)  // 自定义验证函数
  }
  
  // 定义验证结果
  type ValidationResult = {
    valid: Bool,
    errors: Array[String]
  }
  
  // 创建配置验证器
  let create_config_validator = fn() {
    // 验证字符串值
    let validate_string = fn(value: String, rules: Array[ValidationRule]) {
      let mut errors = []
      
      for rule in rules {
        match rule {
          ValidationRule::Required => {
            if value.length() == 0 {
              errors = errors.push("值不能为空")
            }
          }
          ValidationRule::MinLength(min_len) => {
            if value.length() < min_len {
              errors = errors.push("长度不能少于" + min_len.to_string() + "个字符")
            }
          }
          ValidationRule::MaxLength(max_len) => {
            if value.length() > max_len {
              errors = errors.push("长度不能超过" + max_len.to_string() + "个字符")
            }
          }
          ValidationRule::Pattern(pattern) => {
            // 简化的正则匹配，实际项目中应使用正则库
            if pattern == "^[a-z]+$" && not value.chars().all(fn(c) { c >= 'a' && c <= 'z' }) {
              errors = errors.push("值只能包含小写字母")
            }
          }
          ValidationRule::OneOf(options) => {
            if not options.contains(value) {
              errors = errors.push("值必须是以下选项之一: " + options.join(", "))
            }
          }
          ValidationRule::Custom(validator) => {
            if not validator(value) {
              errors = errors.push("值不符合自定义验证规则")
            }
          }
          _ => {
            // 不适用于字符串的规则
          }
        }
      }
      
      {
        valid: errors.length() == 0,
        errors
      }
    }
    
    // 验证整数值
    let validate_int = fn(value: Int, rules: Array[ValidationRule]) {
      let mut errors = []
      
      for rule in rules {
        match rule {
          ValidationRule::Required => {
            // 整数没有空值概念
          }
          ValidationRule::Min(min) => {
            if value < min {
              errors = errors.push("值不能小于" + min.to_string())
            }
          }
          ValidationRule::Max(max) => {
            if value > max {
              errors = errors.push("值不能大于" + max.to_string())
            }
          }
          ValidationRule::Range(min, max) => {
            if value < min || value > max {
              errors = errors.push("值必须在" + min.to_string() + "到" + max.to_string() + "之间")
            }
          }
          ValidationRule::OneOf(options) => {
            let str_value = value.to_string()
            if not options.contains(str_value) {
              errors = errors.push("值必须是以下选项之一: " + options.join(", "))
            }
          }
          ValidationRule::Custom(validator) => {
            if not validator(value.to_string()) {
              errors = errors.push("值不符合自定义验证规则")
            }
          }
          _ => {
            // 不适用于整数的规则
          }
        }
      }
      
      {
        valid: errors.length() == 0,
        errors
      }
    }
    
    // 验证浮点数值
    let validate_float = fn(value: Float, rules: Array[ValidationRule]) {
      let mut errors = []
      
      for rule in rules {
        match rule {
          ValidationRule::Min(min) => {
            if value < (min as Float) {
              errors = errors.push("值不能小于" + min.to_string())
            }
          }
          ValidationRule::Max(max) => {
            if value > (max as Float) {
              errors = errors.push("值不能大于" + max.to_string())
            }
          }
          ValidationRule::Range(min, max) => {
            if value < (min as Float) || value > (max as Float) {
              errors = errors.push("值必须在" + min.to_string() + "到" + max.to_string() + "之间")
            }
          }
          ValidationRule::Custom(validator) => {
            if not validator(value.to_string()) {
              errors = errors.push("值不符合自定义验证规则")
            }
          }
          _ => {
            // 不适用的规则
          }
        }
      }
      
      {
        valid: errors.length() == 0,
        errors
      }
    }
    
    // 验证布尔值
    let validate_bool = fn(value: Bool, rules: Array[ValidationRule]) {
      let mut errors = []
      
      for rule in rules {
        match rule {
          ValidationRule::Custom(validator) => {
            if not validator(value.to_string()) {
              errors = errors.push("值不符合自定义验证规则")
            }
          }
          _ => {
            // 不适用于布尔值的规则
          }
        }
      }
      
      {
        valid: errors.length() == 0,
        errors
      }
    }
    
    {
      validate_string,
      validate_int,
      validate_float,
      validate_bool
    }
  }
  
  // 测试配置验证
  let validator = create_config_validator()
  
  // 测试字符串验证
  let string_rules = [
    ValidationRule::Required,
    ValidationRule::MinLength(3),
    ValidationRule::MaxLength(10),
    ValidationRule::Pattern("^[a-z]+$")
  ]
  
  let valid_string_result = validator.validate_string("azimuth", string_rules)
  assert_true(valid_string_result.valid)
  assert_eq(valid_string_result.errors.length(), 0)
  
  let invalid_string_result1 = validator.validate_string("", string_rules)  // 空字符串
  assert_false(invalid_string_result1.valid)
  assert_true(invalid_string_result1.errors.length() > 0)
  
  let invalid_string_result2 = validator.validate_string("ab", string_rules)  // 太短
  assert_false(invalid_string_result2.valid)
  assert_true(invalid_string_result2.errors.length() > 0)
  
  let invalid_string_result3 = validator.validate_string("very_long_string", string_rules)  // 太长
  assert_false(invalid_string_result3.valid)
  assert_true(invalid_string_result3.errors.length() > 0)
  
  let invalid_string_result4 = validator.validate_string("Azimuth", string_rules)  // 包含大写字母
  assert_false(invalid_string_result4.valid)
  assert_true(invalid_string_result4.errors.length() > 0)
  
  // 测试整数验证
  let int_rules = [
    ValidationRule::Range(1000, 9999)
  ]
  
  let valid_int_result = validator.validate_int(8080, int_rules)
  assert_true(valid_int_result.valid)
  
  let invalid_int_result1 = validator.validate_int(999, int_rules)  // 太小
  assert_false(invalid_int_result1.valid)
  
  let invalid_int_result2 = validator.validate_int(10000, int_rules)  // 太大
  assert_false(invalid_int_result2.valid)
  
  // 测试枚举验证
  let enum_rules = [
    ValidationRule::OneOf(["debug", "info", "warn", "error"])
  ]
  
  let valid_enum_result = validator.validate_string("info", enum_rules)
  assert_true(valid_enum_result.valid)
  
  let invalid_enum_result = validator.validate_string("trace", enum_rules)
  assert_false(invalid_enum_result.valid)
  
  // 测试自定义验证
  let custom_rules = [
    ValidationRule::Custom(fn(value) { value.contains("azimuth") })
  ]
  
  let valid_custom_result = validator.validate_string("azimuth-telemetry", custom_rules)
  assert_true(valid_custom_result.valid)
  
  let invalid_custom_result = validator.validate_string("other-telemetry", custom_rules)
  assert_false(invalid_custom_result.valid)
}

// 测试 3: 配置热更新
test "configuration hot reload" {
  // 定义配置变更监听器
  type ConfigChangeListener = {
    key: String,
    callback: (String, String, String) -> Unit  // (key, old_value, new_value)
  }
  
  // 定义配置管理器
  type ConfigManager = {
    values: Map[String, String],
    listeners: Array[ConfigChangeListener],
    version: Int
  }
  
  // 创建配置管理器
  let create_config_manager = fn(initial_values: Map[String, String]) {
    {
      values: initial_values,
      listeners: [],
      version: 1
    }
  }
  
  // 添加变更监听器
  let add_listener = fn(manager: ConfigManager, key: String, callback: (String, String, String) -> Unit) {
    let listener = {
      key,
      callback
    }
    
    {
      values: manager.values,
      listeners: manager.listeners.push(listener),
      version: manager.version
    }
  }
  
  // 更新配置值
  let update_config = fn(manager: ConfigManager, key: String, new_value: String) {
    let old_value = match Map::get(manager.values, key) {
      Some(value) => value
      None => ""
    }
    
    // 更新值
    let updated_values = Map::insert(manager.values, key, new_value)
    
    // 通知监听器
    for listener in manager.listeners {
      if listener.key == key {
        listener.callback(key, old_value, new_value)
      }
    }
    
    {
      values: updated_values,
      listeners: manager.listeners,
      version: manager.version + 1
    }
  }
  
  // 批量更新配置
  let batch_update_config = fn(manager: ConfigManager, updates: Map[String, String]) {
    let mut current_manager = manager
    
    for (key, value) in updates {
      current_manager = update_config(current_manager, key, value)
    }
    
    current_manager
  }
  
  // 测试配置热更新
  let initial_values = Map::from_array([
    ("service.name", "azimuth"),
    ("service.version", "1.0.0"),
    ("server.port", "8080")
  ])
  
  let manager = create_config_manager(initial_values)
  
  // 添加监听器
  let mut notification_log = []
  
  let log_callback = fn(key: String, old_value: String, new_value: String) {
    notification_log = notification_log.push(key + ": " + old_value + " -> " + new_value)
  }
  
  let manager1 = add_listener(manager, "service.name", log_callback)
  let manager2 = add_listener(manager1, "server.port", log_callback)
  
  // 更新单个配置
  let manager3 = update_config(manager2, "service.name", "azimuth-telemetry")
  
  // 验证更新
  assert_eq(manager3.version, manager.version + 1)
  assert_eq(Map::get(manager3.values, "service.name"), Some("azimuth-telemetry"))
  
  // 验证监听器被调用
  assert_eq(notification_log.length(), 1)
  assert_eq(notification_log[0], "service.name: azimuth -> azimuth-telemetry")
  
  // 批量更新配置
  let updates = Map::from_array([
    ("service.version", "1.1.0"),
    ("server.port", "9090")
  ])
  
  let manager4 = batch_update_config(manager3, updates)
  
  // 验证批量更新
  assert_eq(manager4.version, manager3.version + 2)
  assert_eq(Map::get(manager4.values, "service.version"), Some("1.1.0"))
  assert_eq(Map::get(manager4.values, "server.port"), Some("9090"))
  
  // 验证监听器被调用
  assert_eq(notification_log.length(), 3)
  assert_eq(notification_log[1], "server.port: 8080 -> 9090")
  assert_eq(notification_log[2], "service.version: 1.0.0 -> 1.1.0")
}

// 测试 4: 配置环境变量覆盖
test "configuration environment variable override" {
  // 定义配置源优先级
  enum ConfigSource {
    Default
    File
    Environment
    Command
  }
  
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: String,
    source: ConfigSource
  }
  
  // 创建配置解析器
  let create_config_parser = fn() {
    // 合并配置，按优先级
    let merge_configs = fn(sources: Array[(ConfigSource, Map[String, String])]) {
      let mut merged = Map::empty()
      
      // 按优先级从低到高处理
      let sorted_sources = sources.sort(fn(a, b) {
        match (a.0, b.0) {
          (ConfigSource::Default, _) => -1
          (_, ConfigSource::Default) => 1
          (ConfigSource::File, _) => -1
          (_, ConfigSource::File) => 1
          (ConfigSource::Environment, _) => -1
          (_, ConfigSource::Environment) => 1
          (ConfigSource::Command, _) => -1
          (_, ConfigSource::Command) => 1
        }
      })
      
      for (source, config) in sorted_sources {
        for (key, value) in config {
          let _ = Map::insert(merged, key, (value, source))
        }
      }
      
      merged
    }
    
    // 转换环境变量名为配置键
    let env_to_config_key = fn(env_var: String) {
      env_var.to_lowercase().replace("_", ".")
    }
    
    // 从环境变量加载配置
    let load_from_env = fn(env_vars: Map[String, String]) {
      let mut config = Map::empty()
      
      for (env_key, env_value) in env_vars {
        let config_key = env_to_config_key(env_key)
        let _ = Map::insert(config, config_key, env_value)
      }
      
      config
    }
    
    {
      merge_configs,
      env_to_config_key,
      load_from_env
    }
  }
  
  // 测试配置环境变量覆盖
  let parser = create_config_parser()
  
  // 定义默认配置
  let default_config = Map::from_array([
    ("service.name", "azimuth"),
    ("service.version", "1.0.0"),
    ("server.port", "8080"),
    ("log.level", "info")
  ])
  
  // 定义文件配置
  let file_config = Map::from_array([
    ("service.name", "azimuth-file"),
    ("server.port", "9090")
  ])
  
  // 定义环境变量配置
  let env_vars = Map::from_array([
    ("SERVICE_NAME", "azimuth-env"),
    ("SERVICE_VERSION", "2.0.0"),
    ("LOG_LEVEL", "debug")
  ])
  
  let env_config = parser.load_from_env(env_vars)
  
  // 定义命令行配置
  let command_config = Map::from_array([
    ("server.port", "8081")
  ])
  
  // 合并配置
  let sources = [
    (ConfigSource::Default, default_config),
    (ConfigSource::File, file_config),
    (ConfigSource::Environment, env_config),
    (ConfigSource::Command, command_config)
  ]
  
  let merged_config = parser.merge_configs(sources)
  
  // 验证合并结果
  // service.name: 环境变量覆盖文件和默认
  assert_eq(Map::get(merged_config, "service.name"), Some(("azimuth-env", ConfigSource::Environment)))
  
  // service.version: 只有环境变量和默认
  assert_eq(Map::get(merged_config, "service.version"), Some(("2.0.0", ConfigSource::Environment)))
  
  // server.port: 命令行覆盖文件和默认
  assert_eq(Map::get(merged_config, "server.port"), Some(("8081", ConfigSource::Command)))
  
  // log.level: 环境变量覆盖默认
  assert_eq(Map::get(merged_config, "log.level"), Some(("debug", ConfigSource::Environment)))
  
  // 测试环境变量名转换
  assert_eq(parser.env_to_config_key("SERVICE_NAME"), "service.name")
  assert_eq(parser.env_to_config_key("SERVER_PORT"), "server.port")
  assert_eq(parser.env_to_config_key("LOG_LEVEL"), "log.level")
}

// 测试 5: 配置模板和继承
test "configuration templates and inheritance" {
  // 定义配置模板
  type ConfigTemplate = {
    name: String,
    parent: Option[String],
    values: Map[String, String]
  }
  
  // 定义配置解析器
  let create_config_parser = fn() {
    // 解析继承的配置
    let resolve_inheritance = fn(templates: Map[String, ConfigTemplate], template_name: String) {
      let mut resolved = Map::empty()
      let mut visited = []
      let mut stack = [template_name]
      
      while stack.length() > 0 {
        let current = stack[stack.length() - 1]
        stack = stack.slice(0, stack.length() - 1)
        
        // 检查循环继承
        if visited.contains(current) {
          continue
        }
        
        visited = visited.push(current)
        
        match Map::get(templates, current) {
          Some(template) => {
            // 先处理父模板
            match template.parent {
              Some(parent) => {
                stack = stack.push(parent)
              }
              None => {}
            }
            
            // 然后处理当前模板的值
            for (key, value) in template.values {
              let _ = Map::insert(resolved, key, value)
            }
          }
          None => {}
        }
      }
      
      resolved
    }
    
    // 应用变量替换
    let apply_variable_substitution = fn(config: Map[String, String], variables: Map[String, String]) {
      let mut result = Map::empty()
      
      for (key, value) in config {
        let mut substituted_value = value
        
        // 简单的变量替换 ${var_name}
        for (var_name, var_value) in variables {
          let placeholder = "${" + var_name + "}"
          substituted_value = substituted_value.replace(placeholder, var_value)
        }
        
        let _ = Map::insert(result, key, substituted_value)
      }
      
      result
    }
    
    {
      resolve_inheritance,
      apply_variable_substitution
    }
  }
  
  // 测试配置模板和继承
  let parser = create_config_parser()
  
  // 定义模板
  let templates = Map::from_array([
    ("base", {
      name: "base",
      parent: None,
      values: Map::from_array([
        ("service.name", "azimuth"),
        ("log.level", "info"),
        ("server.port", "8080")
      ])
    }),
    ("production", {
      name: "production",
      parent: Some("base"),
      values: Map::from_array([
        ("log.level", "warn"),
        ("server.port", "80"),
        ("metrics.enabled", "true")
      ])
    }),
    ("development", {
      name: "development",
      parent: Some("base"),
      values: Map::from_array([
        ("log.level", "debug"),
        ("debug.enabled", "true")
      ])
    }),
    ("staging", {
      name: "staging",
      parent: Some("production"),
      values: Map::from_array([
        ("server.port", "8080")
      ])
    })
  ])
  
  // 解析继承
  let base_config = parser.resolve_inheritance(templates, "base")
  let production_config = parser.resolve_inheritance(templates, "production")
  let development_config = parser.resolve_inheritance(templates, "development")
  let staging_config = parser.resolve_inheritance(templates, "staging")
  
  // 验证基础配置
  assert_eq(Map::get(base_config, "service.name"), Some("azimuth"))
  assert_eq(Map::get(base_config, "log.level"), Some("info"))
  assert_eq(Map::get(base_config, "server.port"), Some("8080"))
  
  // 验证生产配置（继承base并覆盖）
  assert_eq(Map::get(production_config, "service.name"), Some("azimuth"))
  assert_eq(Map::get(production_config, "log.level"), Some("warn"))  // 覆盖
  assert_eq(Map::get(production_config, "server.port"), Some("80"))  // 覆盖
  assert_eq(Map::get(production_config, "metrics.enabled"), Some("true"))  // 新增
  
  // 验证开发配置（继承base并覆盖）
  assert_eq(Map::get(development_config, "service.name"), Some("azimuth"))
  assert_eq(Map::get(development_config, "log.level"), Some("debug"))  // 覆盖
  assert_eq(Map::get(development_config, "server.port"), Some("8080"))  // 继承
  assert_eq(Map::get(development_config, "debug.enabled"), Some("true"))  // 新增
  
  // 验证预发布配置（继承production并覆盖）
  assert_eq(Map::get(staging_config, "service.name"), Some("azimuth"))
  assert_eq(Map::get(staging_config, "log.level"), Some("warn"))  // 从production继承
  assert_eq(Map::get(staging_config, "server.port"), Some("8080"))  // 覆盖production
  assert_eq(Map::get(staging_config, "metrics.enabled"), Some("true"))  // 从production继承
  
  // 测试变量替换
  let variables = Map::from_array([
    ("env", "production"),
    ("version", "1.0.0")
  ])
  
  let config_with_vars = Map::from_array([
    ("service.name", "azimuth-${env}"),
    ("service.version", "${version}"),
    ("log.file", "/var/log/azimuth-${env}.log")
  ])
  
  let substituted_config = parser.apply_variable_substitution(config_with_vars, variables)
  
  // 验证变量替换
  assert_eq(Map::get(substituted_config, "service.name"), Some("azimuth-production"))
  assert_eq(Map::get(substituted_config, "service.version"), Some("1.0.0"))
  assert_eq(Map::get(substituted_config, "log.file"), Some("/var/log/azimuth-production.log"))
}