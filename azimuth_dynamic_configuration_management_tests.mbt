// Azimuth 动态配置更新测试用例
// 包含遥测系统运行时配置更新、热重载和配置验证

// 测试1: 基础配置管理
test "基础配置管理" {
  // 定义配置项类型
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String>)
  }
  
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    is_sensitive: Bool
  }
  
  // 定义配置管理器
  type ConfigManager = {
    items: Array[ConfigItem>,
    version: Int,
    last_updated: Int
  }
  
  // 创建配置项
  let create_config_item = fn(
    key: String, 
    value: ConfigValue, 
    description: String, 
    is_sensitive: Bool
  ) {
    {
      key,
      value,
      default_value: value,
      description,
      is_sensitive
    }
  }
  
  // 创建配置管理器
  let create_config_manager = fn(items: Array[ConfigItem>) {
    {
      items,
      version: 1,
      last_updated: Time::now()
    }
  }
  
  // 获取配置值
  let get_config_value = fn(manager: ConfigManager, key: String) {
    for item in manager.items {
      if item.key == key {
        return Some(item.value)
      }
    }
    None
  }
  
  // 更新配置值
  let update_config_value = fn(manager: ConfigManager, key: String, new_value: ConfigValue) {
    let mut updated_manager = manager
    let mut found = false
    
    for i in 0..manager.items.length() {
      if manager.items[i].key == key {
        updated_manager.items[i] = {
          key: manager.items[i].key,
          value: new_value,
          default_value: manager.items[i].default_value,
          description: manager.items[i].description,
          is_sensitive: manager.items[i].is_sensitive
        }
        found = true
        break
      }
    }
    
    if found {
      updated_manager.version = updated_manager.version + 1
      updated_manager.last_updated = Time::now()
    }
    
    updated_manager
  }
  
  // 重置配置到默认值
  let reset_config_to_default = fn(manager: ConfigManager, key: String) {
    let mut updated_manager = manager
    let mut found = false
    
    for i in 0..manager.items.length() {
      if manager.items[i].key == key {
        updated_manager.items[i] = {
          key: manager.items[i].key,
          value: manager.items[i].default_value,
          default_value: manager.items[i].default_value,
          description: manager.items[i].description,
          is_sensitive: manager.items[i].is_sensitive
        }
        found = true
        break
      }
    }
    
    if found {
      updated_manager.version = updated_manager.version + 1
      updated_manager.last_updated = Time::now()
    }
    
    updated_manager
  }
  
  // 创建初始配置
  let initial_items = [
    create_config_item("service.name", ConfigValue::StringValue("azimuth-service"), "Service name", false),
    create_config_item("service.version", ConfigValue::StringValue("1.0.0"), "Service version", false),
    create_config_item("telemetry.enabled", ConfigValue::BoolValue(true), "Enable telemetry", false),
    create_config_item("telemetry.sample_rate", ConfigValue::FloatValue(0.1), "Sampling rate", false),
    create_config_item("telemetry.batch_size", ConfigValue::IntValue(100), "Batch size", false),
    create_config_item("database.password", ConfigValue::StringValue("secret"), "Database password", true),
    create_config_item("exporters", ConfigValue::ArrayValue(["otlp", "prometheus"]), "Telemetry exporters", false)
  ]
  
  let config_manager = create_config_manager(initial_items)
  
  // 测试获取配置值
  let service_name = get_config_value(config_manager, "service.name")
  match service_name {
    Some(ConfigValue::StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  let telemetry_enabled = get_config_value(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(ConfigValue::BoolValue(enabled)) => assert_true(enabled)
    _ => assert_true(false)
  }
  
  let sample_rate = get_config_value(config_manager, "telemetry.sample_rate")
  match sample_rate {
    Some(ConfigValue::FloatValue(rate)) => assert_eq(rate, 0.1)
    _ => assert_true(false)
  }
  
  // 测试更新配置值
  let updated_manager = update_config_value(
    config_manager, 
    "telemetry.sample_rate", 
    ConfigValue::FloatValue(0.2)
  )
  
  assert_eq(updated_manager.version, 2)
  assert_true(updated_manager.last_updated > config_manager.last_updated)
  
  let updated_sample_rate = get_config_value(updated_manager, "telemetry.sample_rate")
  match updated_sample_rate {
    Some(ConfigValue::FloatValue(rate)) => assert_eq(rate, 0.2)
    _ => assert_true(false)
  }
  
  // 测试重置配置到默认值
  let reset_manager = reset_config_to_default(updated_manager, "telemetry.sample_rate")
  
  assert_eq(reset_manager.version, 3)
  
  let reset_sample_rate = get_config_value(reset_manager, "telemetry.sample_rate")
  match reset_sample_rate {
    Some(ConfigValue::FloatValue(rate)) => assert_eq(rate, 0.1)  // 回到默认值
    _ => assert_true(false)
  }
  
  // 测试不存在的配置项
  let non_existent = get_config_value(config_manager, "non.existent.key")
  assert_eq(non_existent, None)
  
  // 测试更新不存在的配置项
  let unchanged_manager = update_config_value(
    config_manager, 
    "non.existent.key", 
    ConfigValue::StringValue("value")
  )
  
  assert_eq(unchanged_manager.version, config_manager.version)  // 版本不变
}

// 测试2: 配置验证
test "配置验证" {
  // 定义验证规则
  enum ValidationRule {
    MinValue(Float)
    MaxValue(Float)
    MinLength(Int)
    MaxLength(Int)
    AllowedValues(Array[String])
    Pattern(String)  // 简化的正则表达式
    Required
  }
  
  // 定义验证结果
  type ValidationResult = {
    is_valid: Bool,
    error_message: Option<String>
  }
  
  // 定义带验证的配置项
  type ValidatedConfigItem = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    is_sensitive: Bool,
    validation_rules: Array[ValidationRule>
  }
  
  // 定义带验证的配置管理器
  type ValidatedConfigManager = {
    items: Array[ValidatedConfigItem>,
    version: Int,
    last_updated: Int
  }
  
  // 验证配置值
  let validate_config_value = fn(value: ConfigValue, rules: Array[ValidationRule>) {
    for rule in rules {
      match (rule, value) {
        (ValidationRule::MinValue(min), ConfigValue::IntValue(v)) => {
          if (v as Float) < min {
            return {
              is_valid: false,
              error_message: Some("Value is below minimum allowed")
            }
          }
        }
        (ValidationRule::MinValue(min), ConfigValue::FloatValue(v)) => {
          if v < min {
            return {
              is_valid: false,
              error_message: Some("Value is below minimum allowed")
            }
          }
        }
        (ValidationRule::MaxValue(max), ConfigValue::IntValue(v)) => {
          if (v as Float) > max {
            return {
              is_valid: false,
              error_message: Some("Value exceeds maximum allowed")
            }
          }
        }
        (ValidationRule::MaxValue(max), ConfigValue::FloatValue(v)) => {
          if v > max {
            return {
              is_valid: false,
              error_message: Some("Value exceeds maximum allowed")
            }
          }
        }
        (ValidationRule::MinLength(min), ConfigValue::StringValue(v)) => {
          if v.length() < min {
            return {
              is_valid: false,
              error_message: Some("String is too short")
            }
          }
        }
        (ValidationRule::MaxLength(max), ConfigValue::StringValue(v)) => {
          if v.length() > max {
            return {
              is_valid: false,
              error_message: Some("String is too long")
            }
          }
        }
        (ValidationRule::AllowedValues(allowed), ConfigValue::StringValue(v)) => {
          if not(allowed.contains(v)) {
            return {
              is_valid: false,
              error_message: Some("Value is not in allowed list")
            }
          }
        }
        (ValidationRule::Pattern(pattern), ConfigValue::StringValue(v)) => {
          // 简化的模式匹配
          if pattern == "email" && not(v.contains("@")) {
            return {
              is_valid: false,
              error_message: Some("Invalid email format")
            }
          }
        }
        (ValidationRule::Required, ConfigValue::StringValue(v)) => {
          if v.length() == 0 {
            return {
              is_valid: false,
              error_message: Some("Value is required")
            }
          }
        }
        _ => {}  // 其他组合或不匹配的规则
      }
    }
    
    {
      is_valid: true,
      error_message: None
    }
  }
  
  // 创建带验证的配置项
  let create_validated_config_item = fn(
    key: String, 
    value: ConfigValue, 
    description: String, 
    is_sensitive: Bool,
    validation_rules: Array[ValidationRule>
  ) {
    {
      key,
      value,
      default_value: value,
      description,
      is_sensitive,
      validation_rules
    }
  }
  
  // 创建带验证的配置管理器
  let create_validated_config_manager = fn(items: Array[ValidatedConfigItem>) {
    {
      items,
      version: 1,
      last_updated: Time::now()
    }
  }
  
  // 验证并更新配置值
  let validate_and_update = fn(manager: ValidatedConfigManager, key: String, new_value: ConfigValue) {
    for item in manager.items {
      if item.key == key {
        let validation_result = validate_config_value(new_value, item.validation_rules)
        
        if validation_result.is_valid {
          let mut updated_manager = manager
          
          for i in 0..manager.items.length() {
            if manager.items[i].key == key {
              updated_manager.items[i] = {
                key: manager.items[i].key,
                value: new_value,
                default_value: manager.items[i].default_value,
                description: manager.items[i].description,
                is_sensitive: manager.items[i].is_sensitive,
                validation_rules: manager.items[i].validation_rules
              }
              break
            }
          }
          
          updated_manager.version = updated_manager.version + 1
          updated_manager.last_updated = Time::now()
          
          return {
            success: true,
            manager: updated_manager,
            error_message: None
          }
        } else {
          return {
            success: false,
            manager,
            error_message: validation_result.error_message
          }
        }
      }
    }
    
    {
      success: false,
      manager,
      error_message: Some("Configuration key not found")
    }
  }
  
  // 创建带验证的初始配置
  let validated_items = [
    create_validated_config_item(
      "service.name", 
      ConfigValue::StringValue("azimuth-service"), 
      "Service name", 
      false,
      [ValidationRule::Required, ValidationRule::MinLength(1), ValidationRule::MaxLength(50)]
    ),
    create_validated_config_item(
      "telemetry.sample_rate", 
      ConfigValue::FloatValue(0.1), 
      "Sampling rate", 
      false,
      [ValidationRule::MinValue(0.0), ValidationRule::MaxValue(1.0)]
    ),
    create_validated_config_item(
      "telemetry.batch_size", 
      ConfigValue::IntValue(100), 
      "Batch size", 
      false,
      [ValidationRule::MinValue(1.0), ValidationRule::MaxValue(1000.0)]
    ),
    create_validated_config_item(
      "service.environment", 
      ConfigValue::StringValue("production"), 
      "Environment", 
      false,
      [ValidationRule::AllowedValues(["development", "staging", "production"])]
    ),
    create_validated_config_item(
      "admin.email", 
      ConfigValue::StringValue("admin@example.com"), 
      "Admin email", 
      false,
      [ValidationRule::Required, ValidationRule::Pattern("email")]
    )
  ]
  
  let validated_manager = create_validated_config_manager(validated_items)
  
  // 测试有效更新
  let valid_update_result = validate_and_update(
    validated_manager, 
    "telemetry.sample_rate", 
    ConfigValue::FloatValue(0.2)
  )
  
  assert_true(valid_update_result.success)
  assert_eq(valid_update_result.manager.version, 2)
  
  // 测试无效更新 - 超出最大值
  let invalid_max_result = validate_and_update(
    validated_manager, 
    "telemetry.sample_rate", 
    ConfigValue::FloatValue(1.5)
  )
  
  assert_false(invalid_max_result.success)
  assert_eq(invalid_max_result.manager.version, validated_manager.version)  // 版本不变
  assert_eq(invalid_max_result.error_message, Some("Value exceeds maximum allowed"))
  
  // 测试无效更新 - 不在允许值列表中
  let invalid_allowed_result = validate_and_update(
    validated_manager, 
    "service.environment", 
    ConfigValue::StringValue("testing")
  )
  
  assert_false(invalid_allowed_result.success)
  assert_eq(invalid_allowed_result.error_message, Some("Value is not in allowed list"))
  
  // 测试无效更新 - 邮箱格式
  let invalid_email_result = validate_and_update(
    validated_manager, 
    "admin.email", 
    ConfigValue::StringValue("not-an-email")
  )
  
  assert_false(invalid_email_result.success)
  assert_eq(invalid_email_result.error_message, Some("Invalid email format"))
  
  // 测试无效更新 - 空字符串
  let invalid_required_result = validate_and_update(
    validated_manager, 
    "service.name", 
    ConfigValue::StringValue("")
  )
  
  assert_false(invalid_required_result.success)
  assert_eq(invalid_required_result.error_message, Some("Value is required"))
  
  // 测试不存在的配置项
  let non_existent_result = validate_and_update(
    validated_manager, 
    "non.existent.key", 
    ConfigValue::StringValue("value")
  )
  
  assert_false(non_existent_result.success)
  assert_eq(non_existent_result.error_message, Some("Configuration key not found"))
}

// 测试3: 配置热重载
test "配置热重载" {
  // 定义配置变更事件
  type ConfigChangeEvent = {
    key: String,
    old_value: Option[ConfigValue>,
    new_value: ConfigValue,
    timestamp: Int
  }
  
  // 定义配置监听器
  type ConfigListener = {
    id: String,
    name: String,
    interested_keys: Array<String>,
    changes_received: Array<ConfigChangeEvent>
  }
  
  // 定义热重载管理器
  type HotReloadManager = {
    config_manager: ValidatedConfigManager,
    listeners: Array[ConfigListener>,
    change_history: Array<ConfigChangeEvent>
  }
  
  // 创建配置监听器
  let create_config_listener = fn(id: String, name: String, interested_keys: Array<String>) {
    {
      id,
      name,
      interested_keys,
      changes_received: []
    }
  }
  
  // 创建热重载管理器
  let create_hot_reload_manager = fn(config_manager: ValidatedConfigManager) {
    {
      config_manager,
      listeners: [],
      change_history: []
    }
  }
  
  // 添加监听器
  let add_listener = fn(manager: HotReloadManager, listener: ConfigListener) {
    let mut updated_manager = manager
    updated_manager.listeners = updated_manager.listeners.push(listener)
    updated_manager
  }
  
  // 通知监听器
  let notify_listeners = fn(manager: HotReloadManager, change: ConfigChangeEvent) {
    let mut updated_manager = manager
    
    for i in 0..manager.listeners.length() {
      let listener = manager.listeners[i]
      
      // 检查监听器是否对此变更感兴趣
      if listener.interested_keys.contains(change.key) || 
         listener.interested_keys.contains("*") {
        
        // 添加变更到监听器的变更历史
        let mut updated_listener = listener
        updated_listener.changes_received = updated_listener.changes_received.push(change)
        updated_manager.listeners[i] = updated_listener
      }
    }
    
    updated_manager
  }
  
  // 热重载配置
  let hot_reload_config = fn(manager: HotReloadManager, key: String, new_value: ConfigValue) {
    // 获取当前值
    let current_value = match manager.config_manager.items.find(fn(item) { item.key == key }) {
      Some(item) => Some(item.value)
      None => None
    }
    
    // 验证并更新配置
    let update_result = validate_and_update(manager.config_manager, key, new_value)
    
    if update_result.success {
      // 创建变更事件
      let change = {
        key,
        old_value: current_value,
        new_value,
        timestamp: Time::now()
      }
      
      // 添加到变更历史
      let mut updated_manager = manager
      updated_manager.config_manager = update_result.manager
      updated_manager.change_history = updated_manager.change_history.push(change)
      
      // 通知监听器
      updated_manager = notify_listeners(updated_manager, change)
      
      {
        success: true,
        manager: updated_manager,
        change
      }
    } else {
      {
        success: false,
        manager,
        change: {
          key,
          old_value: current_value,
          new_value,
          timestamp: Time::now()
        }
      }
    }
  }
  
  // 使用前一个测试的类型和函数
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String>)
  }
  
  enum ValidationRule {
    MinValue(Float)
    MaxValue(Float)
    MinLength(Int)
    MaxLength(Int)
    AllowedValues(Array[String>)
    Pattern(String)
    Required
  }
  
  type ValidatedConfigItem = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    is_sensitive: Bool,
    validation_rules: Array[ValidationRule>
  }
  
  type ValidatedConfigManager = {
    items: Array[ValidatedConfigItem>,
    version: Int,
    last_updated: Int
  }
  
  let create_validated_config_item = fn(
    key: String, 
    value: ConfigValue, 
    description: String, 
    is_sensitive: Bool,
    validation_rules: Array[ValidationRule>
  ) {
    {
      key,
      value,
      default_value: value,
      description,
      is_sensitive,
      validation_rules
    }
  }
  
  let create_validated_config_manager = fn(items: Array<ValidatedConfigItem>) {
    {
      items,
      version: 1,
      last_updated: Time::now()
    }
  }
  
  let validate_config_value = fn(value: ConfigValue, rules: Array[ValidationRule>) {
    for rule in rules {
      match (rule, value) {
        (ValidationRule::MinValue(min), ConfigValue::IntValue(v)) => {
          if (v as Float) < min {
            return {
              is_valid: false,
              error_message: Some("Value is below minimum allowed")
            }
          }
        }
        (ValidationRule::MaxValue(max), ConfigValue::IntValue(v)) => {
          if (v as Float) > max {
            return {
              is_valid: false,
              error_message: Some("Value exceeds maximum allowed")
            }
          }
        }
        (ValidationRule::AllowedValues(allowed), ConfigValue::StringValue(v)) => {
          if not(allowed.contains(v)) {
            return {
              is_valid: false,
              error_message: Some("Value is not in allowed list")
            }
          }
        }
        _ => {}
      }
    }
    
    {
      is_valid: true,
      error_message: None
    }
  }
  
  let validate_and_update = fn(manager: ValidatedConfigManager, key: String, new_value: ConfigValue) {
    for item in manager.items {
      if item.key == key {
        let validation_result = validate_config_value(new_value, item.validation_rules)
        
        if validation_result.is_valid {
          let mut updated_manager = manager
          
          for i in 0..manager.items.length() {
            if manager.items[i].key == key {
              updated_manager.items[i] = {
                key: manager.items[i].key,
                value: new_value,
                default_value: manager.items[i].default_value,
                description: manager.items[i].description,
                is_sensitive: manager.items[i].is_sensitive,
                validation_rules: manager.items[i].validation_rules
              }
              break
            }
          }
          
          updated_manager.version = updated_manager.version + 1
          updated_manager.last_updated = Time::now()
          
          return {
            success: true,
            manager: updated_manager,
            error_message: None
          }
        } else {
          return {
            success: false,
            manager,
            error_message: validation_result.error_message
          }
        }
      }
    }
    
    {
      success: false,
      manager,
      error_message: Some("Configuration key not found")
    }
  }
  
  // 创建测试配置
  let test_items = [
    create_validated_config_item(
      "telemetry.enabled", 
      ConfigValue::BoolValue(true), 
      "Enable telemetry", 
      false,
      []
    ),
    create_validated_config_item(
      "telemetry.sample_rate", 
      ConfigValue::FloatValue(0.1), 
      "Sampling rate", 
      false,
      [ValidationRule::MinValue(0.0), ValidationRule::MaxValue(1.0)]
    ),
    create_validated_config_item(
      "service.environment", 
      ConfigValue::StringValue("production"), 
      "Environment", 
      false,
      [ValidationRule::AllowedValues(["development", "staging", "production"])]
    )
  ]
  
  let test_config_manager = create_validated_config_manager(test_items)
  let hot_reload_manager = create_hot_reload_manager(test_config_manager)
  
  // 添加监听器
  let telemetry_listener = create_config_listener(
    "listener-1", 
    "TelemetryListener", 
    ["telemetry.enabled", "telemetry.sample_rate"]
  )
  
  let service_listener = create_config_listener(
    "listener-2", 
    "ServiceListener", 
    ["service.environment"]
  )
  
  let all_listener = create_config_listener(
    "listener-3", 
    "AllListener", 
    ["*"]
  )
  
  let manager_with_listeners = add_listener(hot_reload_manager, telemetry_listener)
  let manager_with_all_listeners = add_listener(manager_with_listeners, service_listener)
  let final_manager = add_listener(manager_with_all_listeners, all_listener)
  
  // 测试热重载 - 成功更新
  let reload_result1 = hot_reload_config(
    final_manager, 
    "telemetry.enabled", 
    ConfigValue::BoolValue(false)
  )
  
  assert_true(reload_result1.success)
  assert_eq(reload_result1.manager.config_manager.version, 2)
  assert_eq(reload_result1.manager.change_history.length(), 1)
  
  // 验证监听器收到通知
  let updated_telemetry_listener = reload_result1.manager.listeners.find(
    fn(l) { l.id == "listener-1" }
  ).unwrap()
  
  assert_eq(updated_telemetry_listener.changes_received.length(), 1)
  assert_eq(updated_telemetry_listener.changes_received[0].key, "telemetry.enabled")
  
  // 验证感兴趣的监听器收到通知
  let updated_all_listener = reload_result1.manager.listeners.find(
    fn(l) { l.id == "listener-3" }
  ).unwrap()
  
  assert_eq(updated_all_listener.changes_received.length(), 1)
  
  // 验证不感兴趣的监听器没有收到通知
  let updated_service_listener = reload_result1.manager.listeners.find(
    fn(l) { l.id == "listener-2" }
  ).unwrap()
  
  assert_eq(updated_service_listener.changes_received.length(), 0)
  
  // 测试热重载 - 失败更新
  let reload_result2 = hot_reload_config(
    reload_result1.manager, 
    "telemetry.sample_rate", 
    ConfigValue::FloatValue(1.5)  // 超出最大值
  )
  
  assert_false(reload_result2.success)
  assert_eq(reload_result2.manager.config_manager.version, 2)  // 版本不变
  
  // 测试多个热重载
  let reload_result3 = hot_reload_config(
    reload_result2.manager, 
    "service.environment", 
    ConfigValue::StringValue("staging")
  )
  
  assert_true(reload_result3.success)
  assert_eq(reload_result3.manager.config_manager.version, 3)
  assert_eq(reload_result3.manager.change_history.length(), 2)
  
  // 验证变更历史
  let change_history = reload_result3.manager.change_history
  assert_eq(change_history[0].key, "telemetry.enabled")
  assert_eq(change_history[1].key, "service.environment")
  
  // 验证监听器状态
  let final_telemetry_listener = reload_result3.manager.listeners.find(
    fn(l) { l.id == "listener-1" }
  ).unwrap()
  
  let final_service_listener = reload_result3.manager.listeners.find(
    fn(l) { l.id == "listener-2" }
  ).unwrap()
  
  let final_all_listener = reload_result3.manager.listeners.find(
    fn(l) { l.id == "listener-3" }
  ).unwrap()
  
  assert_eq(final_telemetry_listener.changes_received.length(), 1)
  assert_eq(final_service_listener.changes_received.length(), 1)
  assert_eq(final_all_listener.changes_received.length(), 2)
}

// 测试4: 配置持久化和恢复
test "配置持久化和恢复" {
  // 定义配置快照
  type ConfigSnapshot = {
    version: Int,
    timestamp: Int,
    items: Array[ConfigItem>
  }
  
  // 定义持久化管理器
  type PersistenceManager = {
    snapshots: Array<ConfigSnapshot>,
    max_snapshots: Int
  }
  
  // 创建配置快照
  let create_config_snapshot = fn(manager: ConfigManager) {
    let items = []
    
    for item in manager.items {
      items = items.push({
        key: item.key,
        value: item.value,
        default_value: item.default_value,
        description: item.description,
        is_sensitive: item.is_sensitive
      })
    }
    
    {
      version: manager.version,
      timestamp: Time::now(),
      items
    }
  }
  
  // 创建持久化管理器
  let create_persistence_manager = fn(max_snapshots: Int) {
    {
      snapshots: [],
      max_snapshots
    }
  }
  
  // 保存快照
  let save_snapshot = fn(persistence: PersistenceManager, snapshot: ConfigSnapshot) {
    let mut updated_persistence = persistence
    updated_persistence.snapshots = updated_persistence.snapshots.push(snapshot)
    
    // 限制快照数量
    if updated_persistence.snapshots.length() > updated_persistence.max_snapshots {
      updated_persistence.snapshots = updated_persistence.snapshots.slice(1)
    }
    
    updated_persistence
  }
  
  // 从快照恢复配置
  let restore_from_snapshot = fn(snapshot: ConfigSnapshot) {
    let items = []
    
    for item in snapshot.items {
      items = items.push({
        key: item.key,
        value: item.value,
        default_value: item.default_value,
        description: item.description,
        is_sensitive: item.is_sensitive
      })
    }
    
    {
      items,
      version: snapshot.version,
      last_updated: snapshot.timestamp
    }
  }
  
  // 获取最新快照
  let get_latest_snapshot = fn(persistence: PersistenceManager) {
    if persistence.snapshots.length() > 0 {
      Some(persistence.snapshots[persistence.snapshots.length() - 1])
    } else {
      None
    }
  }
  
  // 使用前一个测试的类型
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String])
  }
  
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    is_sensitive: Bool
  }
  
  type ConfigManager = {
    items: Array[ConfigItem>,
    version: Int,
    last_updated: Int
  }
  
  let create_config_item = fn(
    key: String, 
    value: ConfigValue, 
    description: String, 
    is_sensitive: Bool
  ) {
    {
      key,
      value,
      default_value: value,
      description,
      is_sensitive
    }
  }
  
  let create_config_manager = fn(items: Array[ConfigItem>) {
    {
      items,
      version: 1,
      last_updated: Time::now()
    }
  }
  
  let update_config_value = fn(manager: ConfigManager, key: String, new_value: ConfigValue) {
    let mut updated_manager = manager
    let mut found = false
    
    for i in 0..manager.items.length() {
      if manager.items[i].key == key {
        updated_manager.items[i] = {
          key: manager.items[i].key,
          value: new_value,
          default_value: manager.items[i].default_value,
          description: manager.items[i].description,
          is_sensitive: manager.items[i].is_sensitive
        }
        found = true
        break
      }
    }
    
    if found {
      updated_manager.version = updated_manager.version + 1
      updated_manager.last_updated = Time::now()
    }
    
    updated_manager
  }
  
  // 创建测试配置
  let test_items = [
    create_config_item("service.name", ConfigValue::StringValue("azimuth-service"), "Service name", false),
    create_config_item("service.version", ConfigValue::StringValue("1.0.0"), "Service version", false),
    create_config_item("telemetry.enabled", ConfigValue::BoolValue(true), "Enable telemetry", false),
    create_config_item("telemetry.sample_rate", ConfigValue::FloatValue(0.1), "Sampling rate", false)
  ]
  
  let config_manager = create_config_manager(test_items)
  let persistence_manager = create_persistence_manager(3)  // 最多保留3个快照
  
  // 创建初始快照
  let initial_snapshot = create_config_snapshot(config_manager)
  let persistence_with_snapshot = save_snapshot(persistence_manager, initial_snapshot)
  
  assert_eq(persistence_with_snapshot.snapshots.length(), 1)
  assert_eq(persistence_with_snapshot.snapshots[0].version, 1)
  
  // 更新配置并创建新快照
  let updated_manager = update_config_value(
    config_manager, 
    "telemetry.sample_rate", 
    ConfigValue::FloatValue(0.2)
  )
  
  let second_snapshot = create_config_snapshot(updated_manager)
  let persistence_with_two_snapshots = save_snapshot(persistence_with_snapshot, second_snapshot)
  
  assert_eq(persistence_with_two_snapshots.snapshots.length(), 2)
  assert_eq(persistence_with_two_snapshots.snapshots[1].version, 2)
  
  // 再次更新配置并创建快照
  let updated_manager2 = update_config_value(
    updated_manager, 
    "service.version", 
    ConfigValue::StringValue("1.1.0")
  )
  
  let third_snapshot = create_config_snapshot(updated_manager2)
  let persistence_with_three_snapshots = save_snapshot(persistence_with_two_snapshots, third_snapshot)
  
  assert_eq(persistence_with_three_snapshots.snapshots.length(), 3)
  assert_eq(persistence_with_three_snapshots.snapshots[2].version, 3)
  
  // 测试快照数量限制
  let updated_manager3 = update_config_value(
    updated_manager2, 
    "telemetry.enabled", 
    ConfigValue::BoolValue(false)
  )
  
  let fourth_snapshot = create_config_snapshot(updated_manager3)
  let persistence_with_four_snapshots = save_snapshot(persistence_with_three_snapshots, fourth_snapshot)
  
  // 应该仍然只有3个快照，最早的被移除
  assert_eq(persistence_with_four_snapshots.snapshots.length(), 3)
  assert_eq(persistence_with_four_snapshots.snapshots[0].version, 2)  // 版本1的快照被移除
  
  // 测试从快照恢复
  let latest_snapshot = get_latest_snapshot(persistence_with_four_snapshots)
  match latest_snapshot {
    Some(snapshot) => {
      assert_eq(snapshot.version, 4)
      
      let restored_manager = restore_from_snapshot(snapshot)
      assert_eq(restored_manager.version, 4)
      assert_eq(restored_manager.last_updated, snapshot.timestamp)
      
      // 验证恢复的配置值
      let restored_sample_rate = restored_manager.items.find(
        fn(item) { item.key == "telemetry.sample_rate" }
      )
      
      match restored_sample_rate {
        Some(item) => {
          match item.value {
            ConfigValue::FloatValue(rate) => assert_eq(rate, 0.2)
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
      
      let restored_enabled = restored_manager.items.find(
        fn(item) { item.key == "telemetry.enabled" }
      )
      
      match restored_enabled {
        Some(item) => {
          match item.value {
            ConfigValue::BoolValue(enabled) => assert_false(enabled)
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试从特定版本快照恢复
  let version_3_snapshot = persistence_with_four_snapshots.snapshots.find(
    fn(s) { s.version == 3 }
  )
  
  match version_3_snapshot {
    Some(snapshot) => {
      let restored_manager_v3 = restore_from_snapshot(snapshot)
      assert_eq(restored_manager_v3.version, 3)
      
      // 验证恢复的配置值
      let restored_version = restored_manager_v3.items.find(
        fn(item) { item.key == "service.version" }
      )
      
      match restored_version {
        Some(item) => {
          match item.value {
            ConfigValue::StringValue(version) => assert_eq(version, "1.1.0")
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试空持久化管理器
  let empty_persistence = create_persistence_manager(5)
  let empty_latest = get_latest_snapshot(empty_persistence)
  assert_eq(empty_latest, None)
}

// 测试5: 配置模板和环境变量替换
test "配置模板和环境变量替换" {
  // 定义配置模板
  type ConfigTemplate = {
    key: String,
    template: String,
    description: String,
    required_vars: Array<String>
  }
  
  // 定义环境变量提供者
  type EnvironmentProvider = {
    variables: Array<(String, String)>
  }
  
  // 创建环境变量提供者
  let create_environment_provider = fn(variables: Array<(String, String)>) {
    { variables }
  }
  
  // 获取环境变量值
  let get_env_var = fn(provider: EnvironmentProvider, key: String) {
    for (k, v) in provider.variables {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  // 替换模板中的环境变量
  let substitute_template_vars = fn(template: String, provider: EnvironmentProvider) {
    let mut result = template
    let mut substitution_count = 0
    
    // 简化的模板替换，查找 ${VAR_NAME} 格式
    let mut start_index = 0
    
    while start_index < result.length() {
      let var_start = result.find("${", start_index)
      
      match var_start {
        Some(index) => {
          let var_end = result.find("}", index)
          
          match var_end {
            Some(end_index) => {
              let var_name = result.substring(index + 2, end_index - index - 2)
              let var_value = get_env_var(provider, var_name)
              
              match var_value {
                Some(value) => {
                  // 替换变量
                  result = result.substring(0, index) + value + 
                         result.substring(end_index + 1, result.length() - end_index - 1)
                  substitution_count = substitution_count + 1
                  start_index = index + value.length()
                }
                None => {
                  // 变量未找到，保持原样
                  start_index = end_index + 1
                }
              }
            }
            None => {
              break  // 没有闭合的 }
            }
          }
        }
        None => {
          break  // 没有更多的变量
        }
      }
    }
    
    (result, substitution_count)
  }
  
  // 从模板创建配置项
  let create_config_from_template = fn(
    template: ConfigTemplate, 
    provider: EnvironmentProvider
  ) {
    let (resolved_value, substitution_count) = substitute_template_vars(template.template, provider)
    
    // 检查所有必需的变量是否都已替换
    let missing_vars = []
    for var in template.required_vars {
      match get_env_var(provider, var) {
        Some(_) => {}
        None => {
          missing_vars = missing_vars.push(var)
        }
      }
    }
    
    if missing_vars.length() > 0 {
      {
        success: false,
        error_message: Some("Missing required environment variables: " + missing_vars.join(", ")),
        config_item: None
      }
    } else {
      // 尝试解析值的类型
      let parsed_value = if resolved_value == "true" {
        ConfigValue::BoolValue(true)
      } else if resolved_value == "false" {
        ConfigValue::BoolValue(false)
      } else if resolved_value.contains(".") && 
                resolved_value.chars().all(fn(c) { c.is_digit() || c == '.' }) {
        ConfigValue::FloatValue(resolved_value.to_float())
      } else if resolved_value.chars().all(fn(c) { c.is_digit() }) {
        ConfigValue::IntValue(resolved_value.to_int())
      } else {
        ConfigValue::StringValue(resolved_value)
      }
      
      {
        success: true,
        error_message: None,
        config_item: Some({
          key: template.key,
          value: parsed_value,
          default_value: parsed_value,
          description: template.description,
          is_sensitive: false
        })
      }
    }
  }
  
  // 使用前一个测试的类型
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String>)
  }
  
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    is_sensitive: Bool
  }
  
  // 创建配置模板
  let config_templates = [
    {
      key: "service.name",
      template: "${SERVICE_NAME:-azimuth-service}",
      description: "Service name",
      required_vars: []
    },
    {
      key: "service.port",
      template: "${SERVICE_PORT}",
      description: "Service port",
      required_vars: ["SERVICE_PORT"]
    },
    {
      key: "telemetry.enabled",
      template: "${TELEMETRY_ENABLED:-true}",
      description: "Enable telemetry",
      required_vars: []
    },
    {
      key: "telemetry.sample_rate",
      template: "${TELEMETRY_SAMPLE_RATE}",
      description: "Sampling rate",
      required_vars: ["TELEMETRY_SAMPLE_RATE"]
    },
    {
      key: "database.url",
      template: "postgresql://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${DB_PORT}/${DB_NAME}",
      description: "Database URL",
      required_vars: ["DB_USER", "DB_PASSWORD", "DB_HOST", "DB_PORT", "DB_NAME"]
    }
  ]
  
  // 创建环境变量提供者
  let env_provider = create_environment_provider([
    ("SERVICE_NAME", "production-service"),
    ("SERVICE_PORT", "8080"),
    ("TELEMETRY_ENABLED", "false"),
    ("TELEMETRY_SAMPLE_RATE", "0.25"),
    ("DB_USER", "azimuth"),
    ("DB_PASSWORD", "secret123"),
    ("DB_HOST", "db.example.com"),
    ("DB_PORT", "5432"),
    ("DB_NAME", "azimuth_db")
  ])
  
  // 测试模板替换
  let template_result = substitute_template_vars("${SERVICE_NAME:-default}", env_provider)
  assert_eq(template_result, ("production-service", 1))
  
  let default_result = substitute_template_vars("${MISSING_VAR:-default}", env_provider)
  assert_eq(default_result, ("default", 0))
  
  let complex_result = substitute_template_vars(
    "postgresql://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${DB_PORT}/${DB_NAME}", 
    env_provider
  )
  assert_eq(complex_result, ("postgresql://azimuth:secret123@db.example.com:5432/azimuth_db", 5))
  
  // 测试从模板创建配置
  let service_name_result = create_config_from_template(config_templates[0], env_provider)
  assert_true(service_name_result.success)
  
  match service_name_result.config_item {
    Some(item) => {
      assert_eq(item.key, "service.name")
      match item.value {
        ConfigValue::StringValue(name) => assert_eq(name, "production-service")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let port_result = create_config_from_template(config_templates[1], env_provider)
  assert_true(port_result.success)
  
  match port_result.config_item {
    Some(item) => {
      match item.value {
        ConfigValue::IntValue(port) => assert_eq(port, 8080)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let telemetry_result = create_config_from_template(config_templates[2], env_provider)
  assert_true(telemetry_result.success)
  
  match telemetry_result.config_item {
    Some(item) => {
      match item.value {
        ConfigValue::BoolValue(enabled) => assert_false(enabled)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let sample_rate_result = create_config_from_template(config_templates[3], env_provider)
  assert_true(sample_rate_result.success)
  
  match sample_rate_result.config_item {
    Some(item) => {
      match item.value {
        ConfigValue::FloatValue(rate) => assert_eq(rate, 0.25)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let db_url_result = create_config_from_template(config_templates[4], env_provider)
  assert_true(db_url_result.success)
  
  match db_url_result.config_item {
    Some(item) => {
      match item.value {
        ConfigValue::StringValue(url) => {
          assert_eq(url, "postgresql://azimuth:secret123@db.example.com:5432/azimuth_db")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试缺少必需环境变量的情况
  let incomplete_env = create_environment_provider([
    ("SERVICE_NAME", "test-service")
    // 缺少 SERVICE_PORT
  ])
  
  let incomplete_result = create_config_from_template(config_templates[1], incomplete_env)
  assert_false(incomplete_result.success)
  assert_eq(incomplete_result.error_message, Some("Missing required environment variables: SERVICE_PORT"))
  
  // 测试默认值
  let default_env = create_environment_provider([])  // 空环境
  
  let default_service_result = create_config_from_template(config_templates[0], default_env)
  assert_true(default_service_result.success)
  
  match default_service_result.config_item {
    Some(item) => {
      match item.value {
        ConfigValue::StringValue(name) => assert_eq(name, "azimuth-service")  // 使用默认值
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}