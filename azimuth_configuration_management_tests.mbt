// Azimuth 配置管理和动态更新测试
// 专注于测试配置系统的加载、验证、热更新和版本控制

// 测试1: 配置加载和验证
test "配置加载和验证机制验证" {
  // 定义配置值类型
  enum ConfigValue {
    String(String)
    Int(Int)
    Float(Float)
    Bool(Bool)
    Array(Array[ConfigValue])
    Object(Map[String, ConfigValue])
  }
  
  // 定义配置验证规则
  type ValidationRule = {
    name: String,
    required: Bool,
    value_type: String,  // "string", "int", "float", "bool", "array", "object"
    min_value: Option[ConfigValue],
    max_value: Option[ConfigValue],
    allowed_values: Option[Array[ConfigValue]],
    pattern: Option[String],
    custom_validator: Option[ConfigValue -> Bool]
  }
  
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    source: String,  // 配置来源：file, env, remote, default
    last_modified: Int,
    version: Int
  }
  
  // 定义配置验证结果
  type ValidationResult = {
    is_valid: Bool,
    errors: Array[String],
    warnings: Array[String]
  }
  
  // 创建配置验证器
  let create_config_validator = fn(rules: Map[String, ValidationRule]) {
    let validate_config = fn(config: Map[String, ConfigItem]) -> ValidationResult {
      let errors = []
      let warnings = []
      
      // 检查必需配置项
      for (key, rule) in rules {
        if rule.required && not Map::contains_key(config, key) {
          errors = errors.push("缺少必需配置项: " + key)
        }
      }
      
      // 验证现有配置项
      for (key, item) in config {
        match Map::get(rules, key) {
          Some(rule) => {
            // 验证类型
            let type_error = match (rule.value_type, item.value) {
              ("string", ConfigValue::String(_)) => None
              ("int", ConfigValue::Int(_)) => None
              ("float", ConfigValue::Float(_)) => None
              ("bool", ConfigValue::Bool(_)) => None
              ("array", ConfigValue::Array(_)) => None
              ("object", ConfigValue::Object(_)) => None
              _ => Some("配置项" + key + "类型不匹配，期望: " + rule.value_type)
            }
            
            match type_error {
              Some(error) => errors = errors.push(error)
              None => {}
            }
            
            // 验证最小值
            match rule.min_value {
              Some(min_val) => {
                let min_error = match (min_val, item.value) {
                  (ConfigValue::Int(min), ConfigValue::Int(val)) => {
                    if val < min { Some("配置项" + key + "值小于最小值: " + min.to_string()) } else { None }
                  }
                  (ConfigValue::Float(min), ConfigValue::Float(val)) => {
                    if val < min { Some("配置项" + key + "值小于最小值: " + min.to_string()) } else { None }
                  }
                  (ConfigValue::String(min), ConfigValue::String(val)) => {
                    if val.length() < min.to_int() { 
                      Some("配置项" + key + "长度小于最小值: " + min.to_string()) 
                    } else { None }
                  }
                  _ => None
                }
                
                match min_error {
                  Some(error) => errors = errors.push(error)
                  None => {}
                }
              }
              None => {}
            }
            
            // 验证最大值
            match rule.max_value {
              Some(max_val) => {
                let max_error = match (max_val, item.value) {
                  (ConfigValue::Int(max), ConfigValue::Int(val)) => {
                    if val > max { Some("配置项" + key + "值大于最大值: " + max.to_string()) } else { None }
                  }
                  (ConfigValue::Float(max), ConfigValue::Float(val)) => {
                    if val > max { Some("配置项" + key + "值大于最大值: " + max.to_string()) } else { None }
                  }
                  (ConfigValue::String(max), ConfigValue::String(val)) => {
                    if val.length() > max.to_int() { 
                      Some("配置项" + key + "长度大于最大值: " + max.to_string()) 
                    } else { None }
                  }
                  _ => None
                }
                
                match max_error {
                  Some(error) => errors = errors.push(error)
                  None => {}
                }
              }
              None => {}
            }
            
            // 验证允许的值
            match rule.allowed_values {
              Some(allowed) => {
                if not allowed.contains(item.value) {
                  let allowed_str = allowed.map(fn(val) { 
                    match val {
                      ConfigValue::String(s) => "\"" + s + "\""
                      ConfigValue::Int(i) => i.to_string()
                      ConfigValue::Float(f) => f.to_string()
                      ConfigValue::Bool(b) => b.to_string()
                      ConfigValue::Array(_) => "[...]"
                      ConfigValue::Object(_) => "{...}"
                    }
                  }).join(", ")
                  
                  errors = errors.push("配置项" + key + "值不在允许范围内，允许的值: " + allowed_str)
                }
              }
              None => {}
            }
            
            // 验证模式
            match rule.pattern {
              Some(pattern) => {
                match item.value {
                  ConfigValue::String(value) => {
                    if not Regex::matches(pattern, value) {
                      errors = errors.push("配置项" + key + "值不匹配模式: " + pattern)
                    }
                  }
                  _ => {
                    warnings = warnings.push("配置项" + key + "不是字符串类型，跳过模式验证")
                  }
                }
              }
              None => {}
            }
            
            // 自定义验证器
            match rule.custom_validator {
              Some(validator) => {
                if not validator(item.value) {
                  errors = errors.push("配置项" + key + "未通过自定义验证")
                }
              }
              None => {}
            }
          }
          None => {
            warnings = warnings.push("配置项" + key + "没有验证规则")
          }
        }
      }
      
      {
        is_valid: errors.length() == 0,
        errors: errors,
        warnings: warnings
      }
    }
    
    { validate_config }
  }
  
  // 创建测试配置
  let create_test_config = fn() {
    let config = Map::empty()
    
    // 添加配置项
    let _ = Map::insert(config, "service.name", {
      key: "service.name",
      value: ConfigValue::String("azimuth-telemetry"),
      source: "file",
      last_modified: Time::now(),
      version: 1
    })
    
    let _ = Map::insert(config, "service.port", {
      key: "service.port",
      value: ConfigValue::Int(8080),
      source: "file",
      last_modified: Time::now(),
      version: 1
    })
    
    let _ = Map::insert(config, "log.level", {
      key: "log.level",
      value: ConfigValue::String("info"),
      source: "env",
      last_modified: Time::now(),
      version: 1
    })
    
    let _ = Map::insert(config, "metrics.enabled", {
      key: "metrics.enabled",
      value: ConfigValue::Bool(true),
      source: "file",
      last_modified: Time::now(),
      version: 1
    })
    
    let _ = Map::insert(config, "sampling.rate", {
      key: "sampling.rate",
      value: ConfigValue::Float(0.1),
      source: "remote",
      last_modified: Time::now(),
      version: 1
    })
    
    let _ = Map::insert(config, "endpoints", {
      key: "endpoints",
      value: ConfigValue::Array([
        ConfigValue::String("http://localhost:9090"),
        ConfigValue::String("http://localhost:9091")
      ]),
      source: "file",
      last_modified: Time::now(),
      version: 1
    })
    
    config
  }
  
  // 创建验证规则
  let create_validation_rules = fn() {
    let rules = Map::empty()
    
    // 服务名称规则
    let _ = Map::insert(rules, "service.name", {
      name: "service.name",
      required: true,
      value_type: "string",
      min_value: Some(ConfigValue::String("a")),
      max_value: Some(ConfigValue::String("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")),  // 32字符
      allowed_values: None,
      pattern: Some("^[a-z][a-z0-9-]*$"),
      custom_validator: None
    })
    
    // 服务端口规则
    let _ = Map::insert(rules, "service.port", {
      name: "service.port",
      required: true,
      value_type: "int",
      min_value: Some(ConfigValue::Int(1024)),
      max_value: Some(ConfigValue::Int(65535)),
      allowed_values: None,
      pattern: None,
      custom_validator: None
    })
    
    // 日志级别规则
    let _ = Map::insert(rules, "log.level", {
      name: "log.level",
      required: true,
      value_type: "string",
      min_value: None,
      max_value: None,
      allowed_values: Some([
        ConfigValue::String("debug"),
        ConfigValue::String("info"),
        ConfigValue::String("warn"),
        ConfigValue::String("error")
      ]),
      pattern: None,
      custom_validator: None
    })
    
    // 指标启用规则
    let _ = Map::insert(rules, "metrics.enabled", {
      name: "metrics.enabled",
      required: true,
      value_type: "bool",
      min_value: None,
      max_value: None,
      allowed_values: None,
      pattern: None,
      custom_validator: None
    })
    
    // 采样率规则
    let _ = Map::insert(rules, "sampling.rate", {
      name: "sampling.rate",
      required: true,
      value_type: "float",
      min_value: Some(ConfigValue::Float(0.0)),
      max_value: Some(ConfigValue::Float(1.0)),
      allowed_values: None,
      pattern: None,
      custom_validator: None
    })
    
    // 端点规则
    let _ = Map::insert(rules, "endpoints", {
      name: "endpoints",
      required: false,
      value_type: "array",
      min_value: None,
      max_value: None,
      allowed_values: None,
      pattern: None,
      custom_validator: Some(fn(value) {
        match value {
          ConfigValue::Array(arr) => arr.length() > 0
          _ => false
        }
      })
    })
    
    // 可选配置项
    let _ = Map::insert(rules, "optional.config", {
      name: "optional.config",
      required: false,
      value_type: "string",
      min_value: None,
      max_value: None,
      allowed_values: None,
      pattern: None,
      custom_validator: None
    })
    
    rules
  }
  
  // 测试配置验证
  let validator = create_config_validator(create_validation_rules())
  let valid_config = create_test_config()
  
  let validation_result = validator.validate_config(valid_config)
  
  // 验证有效配置
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // 测试无效配置
  let invalid_config = Map::empty()
  
  // 添加无效配置项
  let _ = Map::insert(invalid_config, "service.name", {
    key: "service.name",
    value: ConfigValue::String("Invalid Name"),  // 包含空格和大写字母
    source: "file",
    last_modified: Time::now(),
    version: 1
  })
  
  let _ = Map::insert(invalid_config, "service.port", {
    key: "service.port",
    value: ConfigValue::Int(100),  // 小于最小端口
    source: "file",
    last_modified: Time::now(),
    version: 1
  })
  
  let _ = Map::insert(invalid_config, "log.level", {
    key: "log.level",
    value: ConfigValue::String("verbose"),  // 不在允许值列表中
    source: "env",
    last_modified: Time::now(),
    version: 1
  })
  
  let _ = Map::insert(invalid_config, "sampling.rate", {
    key: "sampling.rate",
    value: ConfigValue::Float(1.5),  // 大于最大值
    source: "remote",
    last_modified: Time::now(),
    version: 1
  })
  
  let invalid_validation_result = validator.validate_config(invalid_config)
  
  // 验证无效配置
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // 检查特定错误
  assert_true(invalid_validation_result.errors.any(fn(error) { 
    error.contains("service.name") && error.contains("模式")
  }))
  
  assert_true(invalid_validation_result.errors.any(fn(error) { 
    error.contains("service.port") && error.contains("小于最小值")
  }))
  
  assert_true(invalid_validation_result.errors.any(fn(error) { 
    error.contains("log.level") && error.contains("不在允许范围内")
  }))
  
  assert_true(invalid_validation_result.errors.any(fn(error) { 
    error.contains("sampling.rate") && error.contains("大于最大值")
  }))
  
  // 测试缺少必需配置
  let incomplete_config = Map::empty()
  let incomplete_validation_result = validator.validate_config(incomplete_config)
  
  assert_false(incomplete_validation_result.is_valid)
  assert_true(incomplete_validation_result.errors.any(fn(error) { 
    error.contains("缺少必需配置项")
  }))
}

// 测试2: 配置热更新
test "配置热更新机制验证" {
  // 定义配置变更通知
  type ConfigChangeNotification = {
    key: String,
    old_value: Option[ConfigValue],
    new_value: ConfigValue,
    source: String,
    timestamp: Int,
    version: Int
  }
  
  // 定义配置监听器
  type ConfigListener = {
    id: String,
    filter: Option[String -> Bool],  // 可选的键过滤器
    callback: ConfigChangeNotification -> Unit
  }
  
  // 定义配置管理器
  type ConfigManager = {
    get_config: fn(String) -> Option[ConfigValue],
    set_config: fn(String, ConfigValue, String) -> Bool,
    add_listener: fn(ConfigListener) -> String,
    remove_listener: fn(String) -> Unit,
    get_config_history: fn(String, Int) -> Array[ConfigChangeNotification],
    reload_config: fn() -> Bool
  }
  
  // 创建配置管理器
  let create_config_manager = fn(initial_config: Map[String, ConfigItem]) {
    let mut config = initial_config
    let mut listeners = Map::empty()
    let mut history = Map::empty()  // key -> Array[ConfigChangeNotification]
    let mut version_counter = 1
    
    let get_config = fn(key: String) {
      match Map::get(config, key) {
        Some(item) => Some(item.value)
        None => None
      }
    }
    
    let set_config = fn(key: String, value: ConfigValue, source: String) {
      let current_time = Time::now()
      version_counter = version_counter + 1
      
      let old_value = match Map::get(config, key) {
        Some(item) => Some(item.value)
        None => None
      }
      
      // 更新配置
      let updated_item = {
        key: key,
        value: value,
        source: source,
        last_modified: current_time,
        version: version_counter
      }
      
      let _ = Map::insert(config, key, updated_item)
      
      // 创建变更通知
      let notification = {
        key: key,
        old_value: old_value,
        new_value: value,
        source: source,
        timestamp: current_time,
        version: version_counter
      }
      
      // 更新历史记录
      let key_history = match Map::get(history, key) {
        Some(h) => h.push(notification)
        None => [notification]
      }
      let _ = Map::insert(history, key, key_history)
      
      // 通知监听器
      for (listener_id, listener) in listeners {
        let should_notify = match listener.filter {
          Some(filter) => filter(key)
          None => true
        }
        
        if should_notify {
          listener.callback(notification)
        }
      }
      
      true
    }
    
    let add_listener = fn(listener: ConfigListener) {
      let listener_id = listener.id
      let _ = Map::insert(listeners, listener_id, listener)
      listener_id
    }
    
    let remove_listener = fn(listener_id: String) {
      let _ = Map::remove(listeners, listener_id)
    }
    
    let get_config_history = fn(key: String, limit: Int) {
      match Map::get(history, key) {
        Some(h) => {
          if h.length() <= limit {
            h
          } else {
            h.slice(h.length() - limit)
          }
        }
        None => []
      }
    }
    
    let reload_config = fn() {
      // 在实际实现中，这里会从配置源重新加载配置
      // 这里简化为返回true
      true
    }
    
    {
      get_config,
      set_config,
      add_listener,
      remove_listener,
      get_config_history,
      reload_config
    }
  }
  
  // 创建测试配置
  let initial_config = Map::empty()
  
  let _ = Map::insert(initial_config, "service.name", {
    key: "service.name",
    value: ConfigValue::String("azimuth-telemetry"),
    source: "default",
    last_modified: Time::now(),
    version: 1
  })
  
  let _ = Map::insert(initial_config, "service.port", {
    key: "service.port",
    value: ConfigValue::Int(8080),
    source: "default",
    last_modified: Time::now(),
    version: 1
  })
  
  let _ = Map::insert(initial_config, "log.level", {
    key: "log.level",
    value: ConfigValue::String("info"),
    source: "default",
    last_modified: Time::now(),
    version: 1
  })
  
  // 创建配置管理器
  let manager = create_config_manager(initial_config)
  
  // 测试基本配置获取
  match manager.get_config("service.name") {
    Some(ConfigValue::String(name)) => assert_eq(name, "azimuth-telemetry")
    _ => assert_true(false)
  }
  
  match manager.get_config("service.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  match manager.get_config("nonexistent.key") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试配置更新
  let update_result = manager.set_config("service.port", ConfigValue::Int(9090), "file")
  assert_true(update_result)
  
  match manager.get_config("service.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  
  // 测试监听器
  let mut notifications_received = []
  
  let listener = {
    id: "test-listener-1",
    filter: None,
    callback: fn(notification) {
      notifications_received = notifications_received.push(notification)
    }
  }
  
  let listener_id = manager.add_listener(listener)
  assert_eq(listener_id, "test-listener-1")
  
  // 触发配置变更
  manager.set_config("log.level", ConfigValue::String("debug"), "env")
  
  // 验证通知
  assert_eq(notifications_received.length(), 1)
  
  let notification = notifications_received[0]
  assert_eq(notification.key, "log.level")
  
  match (notification.old_value, notification.new_value) {
    (Some(ConfigValue::String(old_val)), ConfigValue::String(new_val)) => {
      assert_eq(old_val, "info")
      assert_eq(new_val, "debug")
    }
    _ => assert_true(false)
  }
  
  assert_eq(notification.source, "env")
  assert_true(notification.timestamp > 0)
  assert_true(notification.version > 0)
  
  // 测试过滤器监听器
  let mut filtered_notifications = []
  
  let filtered_listener = {
    id: "test-listener-2",
    filter: Some(fn(key) { key == "service.name" }),  // 只监听service.name
    callback: fn(notification) {
      filtered_notifications = filtered_notifications.push(notification)
    }
  }
  
  let filtered_listener_id = manager.add_listener(filtered_listener)
  
  // 触发多个配置变更
  manager.set_config("service.name", ConfigValue::String("azimuth-v2"), "remote")
  manager.set_config("log.level", ConfigValue::String("warn"), "env")
  manager.set_config("service.port", ConfigValue::Int(8081), "file")
  
  // 验证过滤器监听器只收到service.name的变更
  assert_eq(filtered_notifications.length(), 1)
  assert_eq(filtered_notifications[0].key, "service.name")
  
  // 验证普通监听器收到所有变更
  assert_eq(notifications_received.length(), 4)  // 之前1个 + 新增3个
  
  // 测试配置历史
  let log_history = manager.get_config_history("log.level", 10)
  assert_eq(log_history.length(), 3)  // 初始 + debug + warn
  
  // 验证历史记录顺序（最新的在前）
  assert_eq(log_history[0].new_value, ConfigValue::String("warn"))
  assert_eq(log_history[1].new_value, ConfigValue::String("debug"))
  assert_eq(log_history[2].new_value, ConfigValue::String("info"))
  
  // 测试移除监听器
  manager.remove_listener(listener_id)
  
  // 再次触发配置变更
  notifications_received = []  // 清空通知记录
  manager.set_config("service.name", ConfigValue::String("azimuth-v3"), "remote")
  
  // 移除的监听器不应该收到通知
  assert_eq(notifications_received.length(), 0)
  
  // 过滤器监听器应该仍然收到通知
  assert_eq(filtered_notifications.length(), 2)  // 之前1个 + 新增1个
}

// 测试3: 配置分层和覆盖
test "配置分层和覆盖机制验证" {
  // 定义配置源优先级
  enum ConfigSource {
    Default    // 最低优先级
    File       // 配置文件
    Env        // 环境变量
    Remote     // 远程配置中心
    CommandLine // 命令行参数，最高优先级
  }
  
  // 定义分层配置
  type LayeredConfig = {
    layers: Map[ConfigSource, Map[String, ConfigValue]],
    merged_cache: Option[Map[String, ConfigValue]],
    cache_dirty: Bool
  }
  
  // 定义配置合并策略
  enum MergeStrategy {
    Override     // 高优先级覆盖低优先级
    Merge        // 合并数组和对象
    Append       // 追加到数组
    Prepend      // 前置到数组
  }
  
  // 创建分层配置管理器
  let create_layered_config_manager = fn() {
    let config = {
      layers: Map::empty(),
      merged_cache: None,
      cache_dirty: true
    }
    
    let set_layer = fn(source: ConfigSource, values: Map[String, ConfigValue]) {
      let updated_layers = Map::insert(config.layers, source, values)
      config = { config with layers: updated_layers, cache_dirty: true }
    }
    
    let get_layer = fn(source: ConfigSource) {
      Map::get(config.layers, source)
    }
    
    let merge_config = fn() {
      if not config.cache_dirty && config.merged_cache.is_some() {
        return config.merged_cache.unwrap()
      }
      
      let sources = [
        ConfigSource::Default,
        ConfigSource::File,
        ConfigSource::Env,
        ConfigSource::Remote,
        ConfigSource::CommandLine
      ]
      
      let merged = Map::empty()
      
      // 按优先级顺序合并配置
      for source in sources {
        match Map::get(config.layers, source) {
          Some(layer) => {
            for (key, value) in layer {
              let _ = Map::insert(merged, key, value)
            }
          }
          None => {}
        }
      }
      
      config = { config with merged_cache: Some(merged), cache_dirty: false }
      merged
    }
    
    let get_config = fn(key: String) {
      let merged = merge_config()
      Map::get(merged, key)
    }
    
    let get_config_with_source = fn(key: String) {
      // 按优先级顺序查找配置
      let sources = [
        ConfigSource::CommandLine,
        ConfigSource::Remote,
        ConfigSource::Env,
        ConfigSource::File,
        ConfigSource::Default
      ]
      
      for source in sources {
        match Map::get(config.layers, source) {
          Some(layer) => {
            match Map::get(layer, key) {
              Some(value) => {
                return Some((value, source))
              }
              None => {}
            }
          }
          None => {}
        }
      }
      
      None
    }
    
    let clear_layer = fn(source: ConfigSource) {
      let updated_layers = Map::remove(config.layers, source)
      config = { config with layers: updated_layers, cache_dirty: true }
    }
    
    {
      set_layer,
      get_layer,
      get_config,
      get_config_with_source,
      clear_layer
    }
  }
  
  // 创建测试配置
  let manager = create_layered_config_manager()
  
  // 设置默认配置
  let default_config = Map::empty()
  let _ = Map::insert(default_config, "service.name", ConfigValue::String("azimuth"))
  let _ = Map::insert(default_config, "service.port", ConfigValue::Int(8080))
  let _ = Map::insert(default_config, "log.level", ConfigValue::String("info"))
  let _ = Map::insert(default_config, "endpoints", ConfigValue::Array([
    ConfigValue::String("http://localhost:9090")
  ]))
  
  manager.set_layer(ConfigSource::Default, default_config)
  
  // 设置文件配置
  let file_config = Map::empty()
  let _ = Map::insert(file_config, "service.port", ConfigValue::Int(9090))  // 覆盖默认值
  let _ = Map::insert(file_config, "log.level", ConfigValue::String("debug"))  // 覆盖默认值
  let _ = Map::insert(file_config, "database.url", ConfigValue::String("postgresql://localhost:5432/azimuth"))
  
  manager.set_layer(ConfigSource::File, file_config)
  
  // 设置环境变量配置
  let env_config = Map::empty()
  let _ = Map::insert(env_config, "log.level", ConfigValue::String("warn"))  // 覆盖文件配置
  let _ = Map::insert(env_config, "feature.flags", ConfigValue::Array([
    ConfigValue::String("new-ui"),
    ConfigValue::String("advanced-metrics")
  ]))
  
  manager.set_layer(ConfigSource::Env, env_config)
  
  // 设置远程配置
  let remote_config = Map::empty()
  let _ = Map::insert(remote_config, "feature.flags", ConfigValue::Array([
    ConfigValue::String("new-ui"),
    ConfigValue::String("advanced-metrics"),
    ConfigValue::String("beta-features")
  ]))  // 覆盖环境变量配置
  let _ = Map::insert(remote_config, "cache.ttl", ConfigValue::Int(300))
  
  manager.set_layer(ConfigSource::Remote, remote_config)
  
  // 设置命令行配置
  let cmdline_config = Map::empty()
  let _ = Map::insert(cmdline_config, "service.port", ConfigValue::Int(8081))  // 覆盖所有其他配置
  let _ = Map::insert(cmdline_config, "debug.enabled", ConfigValue::Bool(true))
  
  manager.set_layer(ConfigSource::CommandLine, cmdline_config)
  
  // 测试配置合并结果
  match manager.get_config("service.name") {
    Some(ConfigValue::String(name)) => assert_eq(name, "azimuth")  // 来自默认配置
    _ => assert_true(false)
  }
  
  match manager.get_config("service.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 8081)  // 来自命令行配置
    _ => assert_true(false)
  }
  
  match manager.get_config("log.level") {
    Some(ConfigValue::String(level)) => assert_eq(level, "warn")  // 来自环境变量配置
    _ => assert_true(false)
  }
  
  match manager.get_config("database.url") {
    Some(ConfigValue::String(url)) => assert_eq(url, "postgresql://localhost:5432/azimuth")  // 来自文件配置
    _ => assert_true(false)
  }
  
  match manager.get_config("feature.flags") {
    Some(ConfigValue::Array(flags)) => {
      assert_eq(flags.length(), 3)  // 来自远程配置
      assert_true(flags.contains(ConfigValue::String("beta-features")))
    }
    _ => assert_true(false)
  }
  
  match manager.get_config("cache.ttl") {
    Some(ConfigValue::Int(ttl)) => assert_eq(ttl, 300)  // 来自远程配置
    _ => assert_true(false)
  }
  
  match manager.get_config("debug.enabled") {
    Some(ConfigValue::Bool(enabled)) => assert_true(enabled)  // 来自命令行配置
    _ => assert_true(false)
  }
  
  match manager.get_config("endpoints") {
    Some(ConfigValue::Array(endpoints)) => {
      assert_eq(endpoints.length(), 1)  // 来自默认配置
    }
    _ => assert_true(false)
  }
  
  // 测试配置源追踪
  match manager.get_config_with_source("service.port") {
    Some((ConfigValue::Int(port), source)) => {
      assert_eq(port, 8081)
      assert_eq(source, ConfigSource::CommandLine)
    }
    _ => assert_true(false)
  }
  
  match manager.get_config_with_source("log.level") {
    Some((ConfigValue::String(level), source)) => {
      assert_eq(level, "warn")
      assert_eq(source, ConfigSource::Env)
    }
    _ => assert_true(false)
  }
  
  match manager.get_config_with_source("service.name") {
    Some((ConfigValue::String(name), source)) => {
      assert_eq(name, "azimuth")
      assert_eq(source, ConfigSource::Default)
    }
    _ => assert_true(false)
  }
  
  // 测试清除配置层
  manager.clear_layer(ConfigSource::Remote)
  
  match manager.get_config("feature.flags") {
    Some(ConfigValue::Array(flags)) => {
      assert_eq(flags.length(), 2)  // 回退到环境变量配置
      assert_false(flags.contains(ConfigValue::String("beta-features")))
    }
    _ => assert_true(false)
  }
  
  match manager.get_config("cache.ttl") {
    Some(_) => assert_true(false)  // 应该被清除
    None => assert_true(true)
  }
  
  // 测试不存在的配置
  match manager.get_config("nonexistent.key") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// 测试4: 配置版本控制和回滚
test "配置版本控制和回滚验证" {
  // 定义配置版本
  type ConfigVersion = {
    version: String,
    timestamp: Int,
    description: String,
    author: String,
    config: Map[String, ConfigValue],
    changes: Array[(String, Option[ConfigValue], ConfigValue)]  // (key, old_value, new_value)
  }
  
  // 定义版本控制管理器
  type VersionControlManager = {
    save_version: fn(String, String) -> String,  // (description, author) -> version_id
    get_version: fn(String) -> Option[ConfigVersion],
    list_versions: fn() -> Array[ConfigVersion],
    rollback_to_version: fn(String) -> Bool,
    diff_versions: fn(String, String) -> Array[(String, Option[ConfigValue], ConfigValue)]
  }
  
  // 创建版本控制管理器
  let create_version_control_manager = fn() {
    let mut versions = Map::empty()  // version_id -> ConfigVersion
    let mut current_config = Map::empty()
    let mut version_counter = 0
    
    let save_version = fn(description: String, author: String) {
      version_counter = version_counter + 1
      let version_id = "v" + version_counter.to_string()
      let current_time = Time::now()
      
      // 计算与上一个版本的差异
      let changes = if version_counter == 1 {
        current_config.map_fn(key, value) { (key, None, value) }.to_array()
      } else {
        let prev_version_id = "v" + (version_counter - 1).to_string()
        match Map::get(versions, prev_version_id) {
          Some(prev_version) => {
            let mut changes = []
            
            // 检查新增和修改的配置
            for (key, new_value) in current_config {
              match Map::get(prev_version.config, key) {
                Some(old_value) => {
                  if old_value != new_value {
                    changes = changes.push((key, Some(old_value), new_value))
                  }
                }
                None => {
                  changes = changes.push((key, None, new_value))
                }
              }
            }
            
            // 检查删除的配置
            for (key, old_value) in prev_version.config {
              if not Map::contains_key(current_config, key) {
                changes = changes.push((key, Some(old_value), ConfigValue::String("DELETED")))
              }
            }
            
            changes
          }
          None => current_config.map_fn(key, value) { (key, None, value) }.to_array()
        }
      }
      
      let version = {
        version: version_id,
        timestamp: current_time,
        description: description,
        author: author,
        config: Map::clone(current_config),
        changes: changes
      }
      
      let _ = Map::insert(versions, version_id, version)
      version_id
    }
    
    let get_version = fn(version_id: String) {
      Map::get(versions, version_id)
    }
    
    let list_versions = fn() {
      versions.values().sort(fn(a, b) {
        if a.timestamp > b.timestamp { -1 }
        else if a.timestamp < b.timestamp { 1 }
        else { 0 }
      })
    }
    
    let rollback_to_version = fn(version_id: String) {
      match Map::get(versions, version_id) {
        Some(version) => {
          current_config = Map::clone(version.config)
          true
        }
        None => false
      }
    }
    
    let diff_versions = fn(version_id1: String, version_id2: String) {
      match (Map::get(versions, version_id1), Map::get(versions, version_id2)) {
        (Some(version1), Some(version2)) => {
          let mut differences = []
          
          // 检查版本1中的配置项
          for (key, value1) in version1.config {
            match Map::get(version2.config, key) {
              Some(value2) => {
                if value1 != value2 {
                  differences = differences.push((key, Some(value1), value2))
                }
              }
              None => {
                differences = differences.push((key, Some(value1), ConfigValue::String("DELETED")))
              }
            }
          }
          
          // 检查版本2中新增的配置项
          for (key, value2) in version2.config {
            if not Map::contains_key(version1.config, key) {
              differences = differences.push((key, None, value2))
            }
          }
          
          differences
        }
        _ => []
      }
    }
    
    let update_config = fn(key: String, value: ConfigValue) {
      let _ = Map::insert(current_config, key, value)
    }
    
    let remove_config = fn(key: String) {
      let _ = Map::remove(current_config, key)
    }
    
    let get_current_config = fn() { current_config }
    
    {
      save_version,
      get_version,
      list_versions,
      rollback_to_version,
      diff_versions,
      update_config,
      remove_config,
      get_current_config
    }
  }
  
  // 创建版本控制管理器
  let vcm = create_version_control_manager()
  
  // 初始配置
  vcm.update_config("service.name", ConfigValue::String("azimuth"))
  vcm.update_config("service.port", ConfigValue::Int(8080))
  vcm.update_config("log.level", ConfigValue::String("info"))
  
  // 保存初始版本
  let v1_id = vcm.save_version("Initial configuration", "admin")
  assert_eq(v1_id, "v1")
  
  // 验证版本
  match vcm.get_version(v1_id) {
    Some(version) => {
      assert_eq(version.version, "v1")
      assert_eq(version.description, "Initial configuration")
      assert_eq(version.author, "admin")
      assert_eq(version.changes.length(), 3)  // 3个新增配置项
      
      match Map::get(version.config, "service.name") {
        Some(ConfigValue::String(name)) => assert_eq(name, "azimuth")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 更新配置
  vcm.update_config("service.port", ConfigValue::Int(9090))
  vcm.update_config("log.level", ConfigValue::String("debug"))
  vcm.update_config("database.url", ConfigValue::String("postgresql://localhost:5432/azimuth"))
  
  // 保存第二个版本
  let v2_id = vcm.save_version("Update service port and log level", "admin")
  assert_eq(v2_id, "v2")
  
  // 验证版本2
  match vcm.get_version(v2_id) {
    Some(version) => {
      assert_eq(version.version, "v2")
      assert_eq(version.description, "Update service port and log level")
      
      // 验证变更
      let port_change = version.changes.find_fn(change) {
        match change { (key, _, _) => key == "service.port" }
      }
      
      match port_change {
        Some((_, Some(ConfigValue::Int(old_val)), ConfigValue::Int(new_val))) => {
          assert_eq(old_val, 8080)
          assert_eq(new_val, 9090)
        }
        _ => assert_true(false)
      }
      
      let log_change = version.changes.find_fn(change) {
        match change { (key, _, _) => key == "log.level" }
      }
      
      match log_change {
        Some((_, Some(ConfigValue::String(old_val)), ConfigValue::String(new_val))) => {
          assert_eq(old_val, "info")
          assert_eq(new_val, "debug")
        }
        _ => assert_true(false)
      }
      
      let db_change = version.changes.find_fn(change) {
        match change { (key, _, _) => key == "database.url" }
      }
      
      match db_change {
        Some((_, None, ConfigValue::String(url))) => {
          assert_eq(url, "postgresql://localhost:5432/azimuth")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 再次更新配置
  vcm.update_config("service.port", ConfigValue::Int(8081))
  vcm.remove_config("database.url")
  vcm.update_config("cache.enabled", ConfigValue::Bool(true))
  
  // 保存第三个版本
  let v3_id = vcm.save_version("Change port and remove database config", "user1")
  assert_eq(v3_id, "v3")
  
  // 验证版本列表
  let versions = vcm.list_versions()
  assert_eq(versions.length(), 3)
  
  // 验证版本顺序（最新的在前）
  assert_eq(versions[0].version, "v3")
  assert_eq(versions[1].version, "v2")
  assert_eq(versions[2].version, "v1")
  
  // 测试版本差异
  let diff_v1_v3 = vcm.diff_versions("v1", "v3")
  assert_true(diff_v1_v3.length() > 0)
  
  // 验证特定差异
  let port_diff = diff_v1_v3.find_fn(diff) {
    match diff { (key, _, _) => key == "service.port" }
  }
  
  match port_diff {
    Some((_, Some(ConfigValue::Int(old_val)), ConfigValue::Int(new_val))) => {
      assert_eq(old_val, 8080)  // v1中的值
      assert_eq(new_val, 8081)  // v3中的值
    }
    _ => assert_true(false)
  }
  
  let db_diff = diff_v1_v3.find_fn(diff) {
    match diff { (key, _, _) => key == "database.url" }
  }
  
  match db_diff {
    Some((_, Some(ConfigValue::String(_)), ConfigValue::String("DELETED"))) => {
      assert_true(true)  // 数据库配置被删除
    }
    _ => assert_true(false)
  }
  
  // 测试回滚
  let rollback_success = vcm.rollback_to_version("v2")
  assert_true(rollback_success)
  
  // 验证回滚后的配置
  match vcm.get_current_config().get("service.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 9090)  // 回滚到v2的值
    _ => assert_true(false)
  }
  
  match vcm.get_current_config().get("database.url") {
    Some(ConfigValue::String(url)) => assert_eq(url, "postgresql://localhost:5432/azimuth")  // v2中的值
    _ => assert_true(false)
  }
  
  match vcm.get_current_config().get("cache.enabled") {
    Some(_) => assert_true(false)  // v2中没有这个配置
    None => assert_true(true)
  }
  
  // 测试回滚到不存在的版本
  let rollback_fail = vcm.rollback_to_version("v999")
  assert_false(rollback_fail)
  
  // 验证回滚失败后配置不变
  match vcm.get_current_config().get("service.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 9090)  // 仍然是v2的值
    _ => assert_true(false)
  }
}