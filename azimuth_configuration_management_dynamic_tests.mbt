// Azimuth 配置管理和动态更新测试
// 专注于测试系统的配置管理和动态更新能力

// 测试1: 基本配置管理测试
test "基本配置管理测试" {
  // 定义配置类型
  type Config {
    settings : Map[String, String>
    defaults : Map[String, String>
  }
  
  // 创建配置
  let create_config = fn(defaults : Map<String, String>) -> Config {
    { settings: defaults, defaults: defaults }
  }
  
  // 获取配置值
  let get_config = fn(config : Config, key : String) -> String {
    match config.settings.get(key) {
      Some(value) => value
      None => {
        match config.defaults.get(key) {
          Some(default_value) => default_value
          None => ""
        }
      }
    }
  }
  
  // 设置配置值
  let set_config = fn(config : Config, key : String, value : String) -> Config {
    { settings: config.settings.insert(key, value), defaults: config.defaults }
  }
  
  // 重置配置到默认值
  let reset_config = fn(config : Config, key : String) -> Config {
    match config.defaults.get(key) {
      Some(default_value) => { settings: config.settings.insert(key, default_value), defaults: config.defaults }
      None => { settings: config.settings.remove(key), defaults: config.defaults }
    }
  }
  
  // 测试配置管理
  let defaults = {
    "host": "localhost",
    "port": "8080",
    "debug": "false",
    "timeout": "30"
  }
  
  let config = create_config(defaults)
  
  // 测试获取默认值
  assert_eq(get_config(config, "host"), "localhost")
  assert_eq(get_config(config, "port"), "8080")
  assert_eq(get_config(config, "debug"), "false")
  assert_eq(get_config(config, "timeout"), "30")
  
  // 测试获取不存在的键
  assert_eq(get_config(config, "nonexistent"), "")
  
  // 测试设置配置值
  let config1 = set_config(config, "host", "example.com")
  assert_eq(get_config(config1, "host"), "example.com")
  assert_eq(get_config(config1, "port"), "8080")  // 其他值不变
  
  // 测试重置配置
  let config2 = reset_config(config1, "host")
  assert_eq(get_config(config2, "host"), "localhost")  // 回到默认值
}

// 测试2: 配置验证测试
test "配置验证测试" {
  // 定义验证规则
  type ValidationRule {
    required : Bool
    min_length : Option[Int>
    max_length : Option<Int>
    pattern : Option<String>
  }
  
  type ValidationResult {
    is_valid : Bool
    error_message : Option<String>
  }
  
  // 创建验证规则
  let create_rule = fn(required : Bool) -> ValidationRule {
    { required: required, min_length: None, max_length: None, pattern: None }
  }
  
  let with_min_length = fn(rule : ValidationRule, length : Int) -> ValidationRule {
    { required: rule.required, min_length: Some(length), max_length: rule.max_length, pattern: rule.pattern }
  }
  
  let with_max_length = fn(rule : ValidationRule, length : Int) -> ValidationRule {
    { required: rule.required, min_length: rule.min_length, max_length: Some(length), pattern: rule.pattern }
  }
  
  let with_pattern = fn(rule : ValidationRule, pattern : String) -> ValidationRule {
    { required: rule.required, min_length: rule.min_length, max_length: rule.max_length, pattern: Some(pattern) }
  }
  
  // 验证配置值
  let validate_config = fn(value : String, rule : ValidationRule) -> ValidationResult {
    // 检查必填项
    if rule.required && value.is_empty() {
      return { is_valid: false, error_message: Some("Value is required") }
    }
    
    // 检查最小长度
    match rule.min_length {
      Some(min_len) => {
        if value.length() < min_len {
          return { is_valid: false, error_message: Some("Value is too short") }
        }
      }
      None => {}
    }
    
    // 检查最大长度
    match rule.max_length {
      Some(max_len) => {
        if value.length() > max_len {
          return { is_valid: false, error_message: Some("Value is too long") }
        }
      }
      None => {}
    }
    
    // 检查模式匹配（简化版）
    match rule.pattern {
      Some(pattern) => {
        if pattern == "email" && !value.contains("@") {
          return { is_valid: false, error_message: Some("Invalid email format") }
        } else if pattern == "port" && value.parse_int().is_none() {
          return { is_valid: false, error_message: Some("Invalid port number") }
        }
      }
      None => {}
    }
    
    { is_valid: true, error_message: None }
  }
  
  // 测试验证规则
  let required_rule = create_rule(true)
  let optional_rule = create_rule(false)
  
  let email_rule = with_pattern(required_rule, "email")
  let port_rule = with_pattern(required_rule, "port")
  let length_rule = with_min_length(with_max_length(optional_rule, 10), 3)
  
  // 测试必填验证
  let result1 = validate_config("", required_rule)
  assert_false(result1.is_valid)
  match result1.error_message {
    Some(msg) => assert_eq(msg, "Value is required")
    None => assert_true(false)
  }
  
  let result2 = validate_config("value", required_rule)
  assert_true(result2.is_valid)
  assert_eq(result2.error_message, None)
  
  // 测试邮箱验证
  let result3 = validate_config("user@example.com", email_rule)
  assert_true(result3.is_valid)
  
  let result4 = validate_config("invalid-email", email_rule)
  assert_false(result4.is_valid)
  match result4.error_message {
    Some(msg) => assert_eq(msg, "Invalid email format")
    None => assert_true(false)
  }
  
  // 测试端口验证
  let result5 = validate_config("8080", port_rule)
  assert_true(result5.is_valid)
  
  let result6 = validate_config("invalid", port_rule)
  assert_false(result6.is_valid)
  match result6.error_message {
    Some(msg) => assert_eq(msg, "Invalid port number")
    None => assert_true(false)
  }
  
  // 测试长度验证
  let result7 = validate_config("ab", length_rule)
  assert_false(result7.is_valid)
  match result7.error_message {
    Some(msg) => assert_eq(msg, "Value is too short")
    None => assert_true(false)
  }
  
  let result8 = validate_config("abc", length_rule)
  assert_true(result8.is_valid)
  
  let result9 = validate_config("abcdefghijk", length_rule)
  assert_false(result9.is_valid)
  match result9.error_message {
    Some(msg) => assert_eq(msg, "Value is too long")
    None => assert_true(false)
  }
}

// 测试3: 动态配置更新测试
test "动态配置更新测试" {
  // 定义配置更新事件
  type ConfigEvent {
    KeyChanged(String, String, String)  // key, old_value, new_value
    KeyAdded(String, String)            // key, value
    KeyRemoved(String, String)          // key, old_value
  }
  
  // 定义动态配置管理器
  type DynamicConfig {
    settings : Map[String, String>
    listeners : Array<String>  // 简化的监听器列表
    event_log : Array<ConfigEvent>
  }
  
  // 创建动态配置
  let create_dynamic_config = fn(initial : Map<String, String>) -> DynamicConfig {
    { settings: initial, listeners: [], event_log: [] }
  }
  
  // 添加监听器
  let add_listener = fn(config : DynamicConfig, listener : String) -> DynamicConfig {
    { settings: config.settings, listeners: config.listeners.push(listener), event_log: config.event_log }
  }
  
  // 更新配置
  let update_config = fn(config : DynamicConfig, key : String, value : String) -> DynamicConfig {
    let event = match config.settings.get(key) {
      Some(old_value) => KeyChanged(key, old_value, value)
      None => KeyAdded(key, value)
    }
    
    { 
      settings: config.settings.insert(key, value), 
      listeners: config.listeners, 
      event_log: config.event_log.push(event) 
    }
  }
  
  // 删除配置
  let remove_config = fn(config : DynamicConfig, key : String) -> DynamicConfig {
    match config.settings.get(key) {
      Some(old_value) => {
        let event = KeyRemoved(key, old_value)
        { 
          settings: config.settings.remove(key), 
          listeners: config.listeners, 
          event_log: config.event_log.push(event) 
        }
      }
      None => config
    }
  }
  
  // 测试动态配置更新
  let initial_settings = { "mode": "production", "level": "info" }
  let config = create_dynamic_config(initial_settings)
  
  // 添加监听器
  let config1 = add_listener(config, "listener1")
  let config2 = add_listener(config1, "listener2")
  assert_eq(config2.listeners.length(), 2)
  
  // 更新现有配置
  let config3 = update_config(config2, "mode", "development")
  assert_eq(get_config({ settings: config3.settings, defaults: {} }, "mode"), "development")
  assert_eq(config3.event_log.length(), 1)
  
  match config3.event_log[0] {
    KeyChanged(key, old, new) => {
      assert_eq(key, "mode")
      assert_eq(old, "production")
      assert_eq(new, "development")
    }
    _ => assert_true(false)
  }
  
  // 添加新配置
  let config4 = update_config(config3, "feature", "enabled")
  assert_eq(get_config({ settings: config4.settings, defaults: {} }, "feature"), "enabled")
  assert_eq(config4.event_log.length(), 2)
  
  match config4.event_log[1] {
    KeyAdded(key, value) => {
      assert_eq(key, "feature")
      assert_eq(value, "enabled")
    }
    _ => assert_true(false)
  }
  
  // 删除配置
  let config5 = remove_config(config4, "level")
  assert_eq(get_config({ settings: config5.settings, defaults: {} }, "level"), "")
  assert_eq(config5.event_log.length(), 3)
  
  match config5.event_log[2] {
    KeyRemoved(key, old) => {
      assert_eq(key, "level")
      assert_eq(old, "info")
    }
    _ => assert_true(false)
  }
}

// 测试4: 配置环境变量覆盖测试
test "配置环境变量覆盖测试" {
  // 定义配置源
  type ConfigSource {
    File
    Environment
    CommandLine
    Runtime
  }
  
  // 定义带优先级的配置项
  type ConfigItem {
    key : String
    value : String
    source : ConfigSource
    priority : Int
  }
  
  // 定义分层配置
  type LayeredConfig {
    items : Array<ConfigItem>
  }
  
  // 创建分层配置
  let create_layered_config = fn() -> LayeredConfig {
    { items: [] }
  }
  
  // 添加配置项
  let add_config_item = fn(config : LayeredConfig, key : String, value : String, source : ConfigSource, priority : Int) -> LayeredConfig {
    { items: config.items.push({ key: key, value: value, source: source, priority: priority }) }
  }
  
  // 获取配置值（按优先级）
  let get_layered_config = fn(config : LayeredConfig, key : String) -> String {
    let rec find_highest_priority = fn(items : Array<ConfigItem>, index : Int, best_item : Option<ConfigItem>) -> Option<ConfigItem> {
      if index >= items.length() { best_item }
      else {
        let item = items[index]
        if item.key == key {
          match best_item {
            Some(best) => {
              if item.priority > best.priority {
                find_highest_priority(items, index + 1, Some(item))
              } else {
                find_highest_priority(items, index + 1, best_item)
              }
            }
            None => find_highest_priority(items, index + 1, Some(item))
          }
        } else {
          find_highest_priority(items, index + 1, best_item)
        }
      }
    }
    
    match find_highest_priority(config.items, 0, None) {
      Some(item) => item.value
      None => ""
    }
  }
  
  // 测试分层配置
  let config = create_layered_config()
  
  // 添加不同来源的配置（优先级：CommandLine > Environment > File > Runtime）
  let config1 = add_config_item(config, "host", "default.com", File, 1)
  let config2 = add_config_item(config1, "host", "env.com", Environment, 2)
  let config3 = add_config_item(config2, "host", "cli.com", CommandLine, 3)
  let config4 = add_config_item(config3, "port", "3000", File, 1)
  let config5 = add_config_item(config4, "port", "8080", Environment, 2)
  
  // 测试优先级
  assert_eq(get_layered_config(config5, "host"), "cli.com")  // 最高优先级
  assert_eq(get_layered_config(config5, "port"), "8080")    // Environment优先于File
  
  // 测试不存在的键
  assert_eq(get_layered_config(config5, "nonexistent"), "")
}

// 测试5: 配置模板和继承测试
test "配置模板和继承测试" {
  // 定义配置模板
  type ConfigTemplate {
    name : String
    settings : Map[String, String>
  }
  
  // 定义配置实例
  type ConfigInstance {
    template_name : String
    overrides : Map<String, String>
  }
  
  // 创建模板
  let create_template = fn(name : String, settings : Map<String, String>) -> ConfigTemplate {
    { name: name, settings: settings }
  }
  
  // 创建实例
  let create_instance = fn(template_name : String, overrides : Map<String, String>) -> ConfigInstance {
    { template_name: template_name, overrides: overrides }
  }
  
  // 解析实例配置
  let resolve_instance = fn(templates : Array<ConfigTemplate>, instance : ConfigInstance) -> Map<String, String> {
    let rec find_template = fn(tmpls : Array<ConfigTemplate>, index : Int, name : String) -> Option<ConfigTemplate> {
      if index >= tmpls.length() { None }
      else {
        let tmpl = tmpls[index]
        if tmpl.name == name { Some(tmpl) }
        else { find_template(tmpls, index + 1, name) }
      }
    }
    
    match find_template(templates, 0, instance.template_name) {
      Some(template) => {
        let rec apply_overrides = fn(base : Map[String, String>, overrides : Map<String, String>, keys : Array<String>) -> Map<String, String> {
          if keys.length() == 0 { base }
          else {
            let key = keys[0]
            let new_base = match overrides.get(key) {
              Some(value) => base.insert(key, value)
              None => base
            }
            apply_overrides(new_base, overrides, keys.slice(1, keys.length()))
          }
        }
        
        apply_overrides(template.settings, instance.overrides, instance.overrides.keys())
      }
      None => {}
    }
  }
  
  // 测试配置模板和继承
  let base_template = create_template("base", {
    "host": "localhost",
    "port": "8080",
    "debug": "false",
    "timeout": "30"
  })
  
  let development_template = create_template("development", {
    "host": "dev.local",
    "port": "3000",
    "debug": "true"
  })
  
  let production_template = create_template("production", {
    "host": "prod.example.com",
    "port": "80",
    "debug": "false",
    "timeout": "60"
  })
  
  let templates = [base_template, development_template, production_template]
  
  // 创建实例
  let dev_instance = create_instance("development", { "timeout": "15" })
  let prod_instance = create_instance("production", { "port": "443" })
  
  // 解析实例配置
  let dev_config = resolve_instance(templates, dev_instance)
  let prod_config = resolve_instance(templates, prod_instance)
  
  // 验证开发配置
  assert_eq(dev_config.get("host"), Some("dev.local"))
  assert_eq(dev_config.get("port"), Some("3000"))
  assert_eq(dev_config.get("debug"), Some("true"))
  assert_eq(dev_config.get("timeout"), Some("15"))  // 覆盖值
  
  // 验证生产配置
  assert_eq(prod_config.get("host"), Some("prod.example.com"))
  assert_eq(prod_config.get("port"), Some("443"))  // 覆盖值
  assert_eq(prod_config.get("debug"), Some("false"))
  assert_eq(prod_config.get("timeout"), Some("60"))
}