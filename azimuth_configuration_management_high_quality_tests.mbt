// Azimuth 配置管理高质量测试用例
// 专注于配置加载、验证、热更新和环境管理

// 测试1: 分层配置系统
test "分层配置系统测试" {
  // 定义配置值类型
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String])
    MapValue(Map[String, String])
  }
  
  // 定义配置源
  enum ConfigSource {
    File(String)
    Environment
    CommandLine
    Remote(String)
    Default
  }
  
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    source: ConfigSource,
    priority: Int,
    required: Bool,
    validated: Bool
  }
  
  // 定义配置层
  type ConfigLayer = {
    name: String,
    source: ConfigSource,
    priority: Int,
    items: Map[String, ConfigItem]
  }
  
  // 定义配置管理器
  type ConfigManager = {
    layers: Array[ConfigLayer],
    merged_config: Map[String, ConfigItem],
    validators: Map[String, (ConfigValue -> Bool)],
    watchers: Map[String, (String -> ())],
    last_updated: Int
  }
  
  // 创建配置项
  let create_config_item = fn(key: String, value: ConfigValue, source: ConfigSource, priority: Int, required: Bool) {
    {
      key,
      value,
      source,
      priority,
      required,
      validated: false
    }
  }
  
  // 创建配置层
  let create_config_layer = fn(name: String, source: ConfigSource, priority: Int) {
    {
      name,
      source,
      priority,
      items: Map::new()
    }
  }
  
  // 创建配置管理器
  let create_config_manager = fn() {
    {
      layers: [],
      merged_config: Map::new(),
      validators: Map::new(),
      watchers: Map::new(),
      last_updated: 1640995200
    }
  }
  
  // 添加配置项到层
  let add_config_item = fn(layer: ConfigLayer, key: String, value: ConfigValue, required: Bool) {
    let item = create_config_item(key, value, layer.source, layer.priority, required)
    let updated_items = layer.items.set(key, item)
    { layer | items: updated_items }
  }
  
  // 添加配置层
  let add_config_layer = fn(manager: ConfigManager, layer: ConfigLayer) {
    // 按优先级排序
    let mut sorted_layers = manager.layers.push(layer)
    sorted_layers = sorted_layers.sort(fn(a, b) { a.priority > b.priority })
    
    // 重新合并配置
    let mut merged_config = Map::new()
    
    for current_layer in sorted_layers {
      for (key, item) in current_layer.items {
        merged_config = merged_config.set(key, item)
      }
    }
    
    {
      layers: sorted_layers,
      merged_config,
      validators: manager.validators,
      watchers: manager.watchers,
      last_updated: 1640995200
    }
  }
  
  // 获取配置值
  let get_config_value = fn(manager: ConfigManager, key: String) {
    let item = manager.merged_config.get(key)
    match item {
      Some(config_item) => Some(config_item.value)
      None => None
    }
  }
  
  // 获取字符串配置
  let get_string_config = fn(manager: ConfigManager, key: String, default_value: String) {
    let value = get_config_value(manager, key)
    match value {
      Some(ConfigValue::StringValue(s)) => s,
      _ => default_value
    }
  }
  
  // 获取整数配置
  let get_int_config = fn(manager: ConfigManager, key: String, default_value: Int) {
    let value = get_config_value(manager, key)
    match value {
      Some(ConfigValue::IntValue(i)) => i,
      _ => default_value
    }
  }
  
  // 获取布尔配置
  let get_bool_config = fn(manager: ConfigManager, key: String, default_value: Bool) {
    let value = get_config_value(manager, key)
    match value {
      Some(ConfigValue::BoolValue(b)) => b,
      _ => default_value
    }
  }
  
  // 添加验证器
  let add_validator = fn(manager: ConfigManager, key: String, validator: ConfigValue -> Bool) {
    let updated_validators = manager.validators.set(key, validator)
    { manager | validators: updated_validators }
  }
  
  // 验证配置
  let validate_config = fn(manager: ConfigManager) {
    let mut validated_config = manager.merged_config
    let mut all_valid = true
    
    for (key, item) in manager.merged_config {
      let validator = manager.validators.get(key)
      match validator {
        Some(v) => {
          let is_valid = v(item.value)
          let updated_item = { item | validated: is_valid }
          validated_config = validated_config.set(key, updated_item)
          
          if not(is_valid) {
            all_valid = false
          }
        }
        None => {
          // 没有验证器，默认为有效
          let updated_item = { item | validated: true }
          validated_config = validated_config.set(key, updated_item)
        }
      }
    }
    
    let updated_manager = { manager | merged_config: validated_config }
    (updated_manager, all_valid)
  }
  
  // 添加监听器
  let add_watcher = fn(manager: ConfigManager, key: String, watcher: String -> ()) {
    let updated_watchers = manager.watchers.set(key, watcher)
    { manager | watchers: updated_watchers }
  }
  
  // 创建配置管理器
  let mut manager = create_config_manager()
  
  // 创建默认配置层
  let default_layer = create_config_layer("default", ConfigSource::Default, 1)
  default_layer = add_config_item(default_layer, "server.port", ConfigValue::IntValue(8080), true)
  default_layer = add_config_item(default_layer, "server.host", ConfigValue::StringValue("localhost"), true)
  default_layer = add_config_item(default_layer, "debug", ConfigValue::BoolValue(false), false)
  default_layer = add_config_item(default_layer, "log.level", ConfigValue::StringValue("INFO"), false)
  
  // 创建文件配置层
  let file_layer = create_config_layer("file", ConfigSource::File("config.json"), 10)
  file_layer = add_config_item(file_layer, "server.port", ConfigValue::IntValue(9000), true)
  file_layer = add_config_item(file_layer, "database.url", ConfigValue::StringValue("postgres://localhost:5432/mydb"), true)
  file_layer = add_config_item(file_layer, "debug", ConfigValue::BoolValue(true), false)
  
  // 创建环境变量配置层
  let env_layer = create_config_layer("env", ConfigSource::Environment, 100)
  env_layer = add_config_item(env_layer, "server.port", ConfigValue::IntValue(8081), true)
  env_layer = add_config_item(env_layer, "database.password", ConfigValue::StringValue("secret"), true)
  
  // 添加配置层
  manager = add_config_layer(manager, default_layer)
  manager = add_config_layer(manager, file_layer)
  manager = add_config_layer(manager, env_layer)
  
  // 验证配置优先级（环境变量 > 文件 > 默认）
  assert_eq(get_int_config(manager, "server.port", 0), 8081)  # 环境变量优先
  assert_eq(get_string_config(manager, "server.host", ""), "localhost")  # 默认值
  assert_eq(get_string_config(manager, "database.url", ""), "postgres://localhost:5432/mydb")  # 文件配置
  assert_eq(get_string_config(manager, "database.password", ""), "secret")  # 环境变量
  assert_eq(get_bool_config(manager, "debug", false), true)  # 文件配置覆盖默认
  
  // 添加验证器
  manager = add_validator(manager, "server.port", fn(value) {
    match value {
      ConfigValue::IntValue(port) => port > 0 and port < 65536
      _ => false
    }
  })
  
  manager = add_validator(manager, "database.url", fn(value) {
    match value {
      ConfigValue::StringValue(url) => url.starts_with("postgres://")
      _ => false
    }
  })
  
  // 验证配置
  let (validated_manager, all_valid) = validate_config(manager)
  assert_true(all_valid)
  
  // 验证验证结果
  let port_item = validated_manager.merged_config.get("server.port")
  match port_item {
    Some(item) => assert_true(item.validated)
    None => assert_true(false)
  }
  
  let url_item = validated_manager.merged_config.get("database.url")
  match url_item {
    Some(item) => assert_true(item.validated)
    None => assert_true(false)
  }
  
  // 测试无效配置
  let invalid_port_layer = create_config_layer("invalid", ConfigSource::CommandLine, 1000)
  invalid_port_layer = add_config_item(invalid_port_layer, "server.port", ConfigValue::IntValue(70000), true)  # 无效端口
  
  let invalid_manager = add_config_layer(validated_manager, invalid_port_layer)
  let (_, invalid_all_valid) = validate_config(invalid_manager)
  assert_false(invalid_all_valid)
  
  // 测试监听器
  let mut notified = false
  let test_watcher = fn(key: String) {
    if key == "debug" {
      notified = true
    }
  }
  
  let watched_manager = add_watcher(validated_manager, "debug", test_watcher)
  assert_eq(watched_manager.watchers.size(), 1)
}

// 测试2: 配置热更新机制
test "配置热更新机制测试" {
  // 定义配置变更事件
  type ConfigChangeEvent = {
    key: String,
    old_value: Option[ConfigValue],
    new_value: ConfigValue,
    source: ConfigSource,
    timestamp: Int
  }
  
  // 定义配置变更监听器
  type ConfigChangeListener = {
    id: String,
    pattern: String,  # 支持通配符模式
    handler: ConfigChangeEvent -> ()
  }
  
  // 定义配置热更新管理器
  type ConfigHotReloadManager = {
    base_manager: ConfigManager,
    listeners: Array[ConfigChangeListener],
    change_history: Array[ConfigChangeEvent],
    auto_reload: Bool,
    reload_interval_ms: Int
  }
  
  // 创建配置热更新管理器
  let create_config_hot_reload_manager = fn(base_manager: ConfigManager, auto_reload: Bool, reload_interval_ms: Int) {
    {
      base_manager,
      listeners: [],
      change_history: [],
      auto_reload,
      reload_interval_ms
    }
  }
  
  // 添加变更监听器
  let add_change_listener = fn(manager: ConfigHotReloadManager, id: String, pattern: String, handler: ConfigChangeEvent -> ()) {
    let listener = {
      id,
      pattern,
      handler
    }
    
    { manager | listeners: manager.listeners.push(listener) }
  }
  
  // 匹配模式
  let match_pattern = fn(pattern: String, key: String) {
    if pattern == "*" {
      true
    } else if pattern.contains("*") {
      // 简化的通配符匹配
      let parts = pattern.split("*")
      if parts.length() == 2 {
        key.starts_with(parts[0]) and key.ends_with(parts[1])
      } else {
        false
      }
    } else {
      pattern == key
    }
  }
  
  // 更新配置值
  let update_config_value = fn(manager: ConfigHotReloadManager, key: String, new_value: ConfigValue, source: ConfigSource) {
    let old_item = manager.base_manager.merged_config.get(key)
    let old_value = match old_item {
      Some(item) => Some(item.value)
      None => None
    }
    
    // 创建变更事件
    let change_event = {
      key,
      old_value,
      new_value,
      source,
      timestamp: 1640995200
    }
    
    // 更新配置
    let updated_layer = create_config_layer("hot-reload", source, 1000)
    let layer_with_item = add_config_item(updated_layer, key, new_value, true)
    let updated_base_manager = add_config_layer(manager.base_manager, layer_with_item)
    
    // 触发监听器
    for listener in manager.listeners {
      if match_pattern(listener.pattern, key) {
        listener.handler(change_event)
      }
    }
    
    let updated_manager = {
      base_manager: updated_base_manager,
      listeners: manager.listeners,
      change_history: manager.change_history.push(change_event),
      auto_reload: manager.auto_reload,
      reload_interval_ms: manager.reload_interval_ms
    }
    
    updated_manager
  }
  
  // 批量更新配置
  let batch_update_config = fn(manager: ConfigHotReloadManager, updates: Array[(String, ConfigValue)], source: ConfigSource) {
    let mut updated_manager = manager
    let mut change_events = []
    
    for (key, value) in updates {
      let old_item = updated_manager.base_manager.merged_config.get(key)
      let old_value = match old_item {
        Some(item) => Some(item.value)
        None => None
      }
      
      let change_event = {
        key,
        old_value,
        new_value: value,
        source,
        timestamp: 1640995200
      }
      
      change_events = change_events.push(change_event)
      
      // 更新配置
      let updated_layer = create_config_layer("batch-reload", source, 1000)
      let layer_with_item = add_config_item(updated_layer, key, value, true)
      updated_manager.base_manager = add_config_layer(updated_manager.base_manager, layer_with_item)
    }
    
    // 批量触发监听器
    for change_event in change_events {
      for listener in updated_manager.listeners {
        if match_pattern(listener.pattern, change_event.key) {
          listener.handler(change_event)
        }
      }
    }
    
    // 更新变更历史
    updated_manager.change_history = updated_manager.change_history + change_events
    
    updated_manager
  }
  
  // 获取变更历史
  let get_change_history = fn(manager: ConfigHotReloadManager, key: Option[String]) {
    match key {
      Some(k) => {
        manager.change_history.filter_fn(event) { event.key == k }
      }
      None => {
        manager.change_history
      }
    }
  }
  
  // 创建基础配置管理器
  let base_manager = create_config_manager()
  let default_layer = create_config_layer("default", ConfigSource::Default, 1)
  let default_layer_with_items = add_config_item(add_config_item(default_layer, "app.name", ConfigValue::StringValue("myapp"), true), "app.version", ConfigValue::StringValue("1.0.0"), true)
  let initial_manager = add_config_layer(base_manager, default_layer_with_items)
  
  // 创建热更新管理器
  let mut hot_reload_manager = create_config_hot_reload_manager(initial_manager, true, 5000)
  
  // 添加监听器
  let mut notifications = []
  
  let app_config_listener = {
    id: "app-listener",
    pattern: "app.*",
    handler: fn(event: ConfigChangeEvent) {
      notifications = notifications.push("App config changed: " + event.key)
    }
  }
  
  let all_config_listener = {
    id: "all-listener",
    pattern: "*",
    handler: fn(event: ConfigChangeEvent) {
      notifications = notifications.push("Any config changed: " + event.key)
    }
  }
  
  hot_reload_manager = add_change_listener(hot_reload_manager, "app-listener", "app.*", app_config_listener.handler)
  hot_reload_manager = add_change_listener(hot_reload_manager, "all-listener", "*", all_config_listener.handler)
  
  // 测试单个配置更新
  hot_reload_manager = update_config_value(hot_reload_manager, "app.name", ConfigValue::StringValue("updated-app"), ConfigSource::Remote("config-server"))
  
  // 验证配置已更新
  assert_eq(get_string_config(hot_reload_manager.base_manager, "app.name", ""), "updated-app")
  
  // 验证监听器被触发
  assert_eq(notifications.length(), 2)  # app监听器和全局监听器
  assert_true(notifications.contains("App config changed: app.name"))
  assert_true(notifications.contains("Any config changed: app.name"))
  
  // 测试批量配置更新
  notifications = []
  let updates = [
    ("app.version", ConfigValue::StringValue("2.0.0")),
    ("app.author", ConfigValue::StringValue("developer")),
    ("server.host", ConfigValue::StringValue("example.com"))
  ]
  
  hot_reload_manager = batch_update_config(hot_reload_manager, updates, ConfigSource::File("updated-config.json"))
  
  // 验证配置已更新
  assert_eq(get_string_config(hot_reload_manager.base_manager, "app.version", ""), "2.0.0")
  assert_eq(get_string_config(hot_reload_manager.base_manager, "app.author", ""), "developer")
  assert_eq(get_string_config(hot_reload_manager.base_manager, "server.host", ""), "example.com")
  
  // 验证监听器被触发
  assert_eq(notifications.length(), 5)  # 3个app.*配置触发app监听器，3个配置触发全局监听器
  
  // 验证变更历史
  let app_history = get_change_history(hot_reload_manager, Some("app.name"))
  assert_eq(app_history.length(), 1)
  assert_eq(app_history[0].key, "app.name")
  
  let all_history = get_change_history(hot_reload_manager, None)
  assert_eq(all_history.length(), 4)  # 1个单个更新 + 3个批量更新
}

// 测试3: 环境特定配置
test "环境特定配置测试" {
  // 定义环境类型
  enum Environment {
    Development
    Testing
    Staging
    Production
  }
  
  // 定义环境配置
  type EnvironmentConfig = {
    environment: Environment,
    configs: Map[String, ConfigValue],
    overrides: Map[String, ConfigValue]  # 覆盖通用配置
  }
  
  // 定义环境配置管理器
  type EnvironmentConfigManager = {
    current_environment: Environment,
    base_config: Map[String, ConfigValue],
    environment_configs: Map[String, EnvironmentConfig],
    merged_config: Map[String, ConfigValue]
  }
  
  // 创建环境配置
  let create_environment_config = fn(environment: Environment) {
    {
      environment,
      configs: Map::new(),
      overrides: Map::new()
    }
  }
  
  // 创建环境配置管理器
  let create_environment_config_manager = fn(current_environment: Environment, base_config: Map[String, ConfigValue]) {
    {
      current_environment,
      base_config,
      environment_configs: Map::new(),
      merged_config: base_config
    }
  }
  
  // 添加环境配置
  let add_environment_config = fn(manager: EnvironmentConfigManager, env_config: EnvironmentConfig) {
    let env_name = match env_config.environment {
      Environment::Development => "development",
      Environment::Testing => "testing",
      Environment::Staging => "staging",
      Environment::Production => "production"
    }
    
    let updated_env_configs = manager.environment_configs.set(env_name, env_config)
    
    // 如果是当前环境，重新合并配置
    let updated_merged_config = if env_config.environment == manager.current_environment {
      let mut merged = manager.base_config
      
      // 应用环境特定配置
      for (key, value) in env_config.configs {
        merged = merged.set(key, value)
      }
      
      // 应用覆盖配置
      for (key, value) in env_config.overrides {
        merged = merged.set(key, value)
      }
      
      merged
    } else {
      manager.merged_config
    }
    
    {
      current_environment: manager.current_environment,
      base_config: manager.base_config,
      environment_configs: updated_env_configs,
      merged_config: updated_merged_config
    }
  }
  
  // 切换环境
  let switch_environment = fn(manager: EnvironmentConfigManager, new_environment: Environment) {
    let env_name = match new_environment {
      Environment::Development => "development",
      Environment::Testing => "testing",
      Environment::Staging => "staging",
      Environment::Production => "production"
    }
    
    let env_config = manager.environment_configs.get(env_name)
    
    // 重新合并配置
    let mut merged = manager.base_config
    
    match env_config {
      Some(config) => {
        // 应用环境特定配置
        for (key, value) in config.configs {
          merged = merged.set(key, value)
        }
        
        // 应用覆盖配置
        for (key, value) in config.overrides {
          merged = merged.set(key, value)
        }
      }
      None => {}
    }
    
    {
      current_environment: new_environment,
      base_config: manager.base_config,
      environment_configs: manager.environment_configs,
      merged_config: merged
    }
  }
  
  // 获取环境配置
  let get_env_config = fn(manager: EnvironmentConfigManager, key: String) {
    manager.merged_config.get(key)
  }
  
  // 创建基础配置
  let mut base_config = Map::new()
  base_config = base_config.set("app.name", ConfigValue::StringValue("myapp"))
  base_config = base_config.set("app.version", ConfigValue::StringValue("1.0.0"))
  base_config = base_config.set("server.port", ConfigValue::IntValue(8080))
  base_config = base_config.set("database.host", ConfigValue::StringValue("localhost"))
  base_config = base_config.set("debug", ConfigValue::BoolValue(true))
  
  // 创建环境配置管理器
  let mut manager = create_environment_config_manager(Environment::Development, base_config)
  
  // 创建开发环境配置
  let mut dev_config = create_environment_config(Environment::Development)
  dev_config.configs = dev_config.configs.set("log.level", ConfigValue::StringValue("DEBUG"))
  dev_config.configs = dev_config.configs.set("feature.flags", ConfigValue::ArrayValue(["new_ui", "experimental_api"]))
  dev_config.overrides = dev_config.overrides.set("server.port", ConfigValue::IntValue(3000))
  
  // 创建测试环境配置
  let mut test_config = create_environment_config(Environment::Testing)
  test_config.configs = test_config.configs.set("log.level", ConfigValue::StringValue("WARN"))
  test_config.configs = test_config.configs.set("database.host", ConfigValue::StringValue("test-db"))
  test_config.overrides = test_config.overrides.set("debug", ConfigValue::BoolValue(false))
  
  // 创建生产环境配置
  let mut prod_config = create_environment_config(Environment::Production)
  prod_config.configs = prod_config.configs.set("log.level", ConfigValue::StringValue("ERROR"))
  prod_config.configs = prod_config.configs.set("database.host", ConfigValue::StringValue("prod-db.example.com"))
  prod_config.configs = prod_config.configs.set("cache.enabled", ConfigValue::BoolValue(true))
  prod_config.overrides = prod_config.overrides.set("debug", ConfigValue::BoolValue(false))
  prod_config.overrides = prod_config.overrides.set("server.port", ConfigValue::IntValue(80))
  
  // 添加环境配置
  manager = add_environment_config(manager, dev_config)
  manager = add_environment_config(manager, test_config)
  manager = add_environment_config(manager, prod_config)
  
  // 验证开发环境配置
  assert_eq(manager.current_environment, Environment::Development)
  
  let app_name = get_env_config(manager, "app.name")
  match app_name {
    Some(ConfigValue::StringValue(name)) => assert_eq(name, "myapp")
    _ => assert_true(false)
  }
  
  let server_port = get_env_config(manager, "server.port")
  match server_port {
    Some(ConfigValue::IntValue(port)) => assert_eq(port, 3000)  # 开发环境覆盖
    _ => assert_true(false)
  }
  
  let log_level = get_env_config(manager, "log.level")
  match log_level {
    Some(ConfigValue::StringValue(level)) => assert_eq(level, "DEBUG")  # 开发环境特定
    _ => assert_true(false)
  }
  
  let debug = get_env_config(manager, "debug")
  match debug {
    Some(ConfigValue::BoolValue(d)) => assert_true(d)  # 基础配置，未被覆盖
    _ => assert_true(false)
  }
  
  // 切换到测试环境
  manager = switch_environment(manager, Environment::Testing)
  assert_eq(manager.current_environment, Environment::Testing)
  
  // 验证测试环境配置
  let test_server_port = get_env_config(manager, "server.port")
  match test_server_port {
    Some(ConfigValue::IntValue(port)) => assert_eq(port, 8080)  # 基础配置，未被覆盖
    _ => assert_true(false)
  }
  
  let test_log_level = get_env_config(manager, "log.level")
  match test_log_level {
    Some(ConfigValue::StringValue(level)) => assert_eq(level, "WARN")  # 测试环境特定
    _ => assert_true(false)
  }
  
  let test_db_host = get_env_config(manager, "database.host")
  match test_db_host {
    Some(ConfigValue::StringValue(host)) => assert_eq(host, "test-db")  # 测试环境覆盖
    _ => assert_true(false)
  }
  
  let test_debug = get_env_config(manager, "debug")
  match test_debug {
    Some(ConfigValue::BoolValue(d)) => assert_false(d)  # 测试环境覆盖
    _ => assert_true(false)
  }
  
  // 切换到生产环境
  manager = switch_environment(manager, Environment::Production)
  assert_eq(manager.current_environment, Environment::Production)
  
  // 验证生产环境配置
  let prod_server_port = get_env_config(manager, "server.port")
  match prod_server_port {
    Some(ConfigValue::IntValue(port)) => assert_eq(port, 80)  # 生产环境覆盖
    _ => assert_true(false)
  }
  
  let prod_log_level = get_env_config(manager, "log.level")
  match prod_log_level {
    Some(ConfigValue::StringValue(level)) => assert_eq(level, "ERROR")  # 生产环境特定
    _ => assert_true(false)
  }
  
  let prod_db_host = get_env_config(manager, "database.host")
  match prod_db_host {
    Some(ConfigValue::StringValue(host)) => assert_eq(host, "prod-db.example.com")  # 生产环境覆盖
    _ => assert_true(false)
  }
  
  let prod_debug = get_env_config(manager, "debug")
  match prod_debug {
    Some(ConfigValue::BoolValue(d)) => assert_false(d)  # 生产环境覆盖
    _ => assert_true(false)
  }
  
  let prod_cache_enabled = get_env_config(manager, "cache.enabled")
  match prod_cache_enabled {
    Some(ConfigValue::BoolValue(enabled)) => assert_true(enabled)  # 生产环境特定
    _ => assert_true(false)
  }
}

// 测试4: 配置加密和安全
test "配置加密和安全测试" {
  // 定义加密配置项
  type EncryptedConfigItem = {
    key: String,
    encrypted_value: String,
    encryption_algorithm: String,
    iv: String,
    metadata: Map[String, String]
  }
  
  // 定义配置加密器
  type ConfigEncryptor = {
    algorithm: String,
    key: String,
    encrypt: String -> String,
    decrypt: String -> String
  }
  
  // 定义安全配置管理器
  type SecureConfigManager = {
    base_manager: ConfigManager,
    encryptor: ConfigEncryptor,
    encrypted_items: Map[String, EncryptedConfigItem],
    sensitive_keys: Set[String],
    access_log: Array[String]
  }
  
  // 创建简单加密器（模拟）
  let create_simple_encryptor = fn(key: String) {
    {
      algorithm: "AES-256",
      key,
      encrypt: fn(plaintext: String) {
        // 简单的模拟加密（实际应用中应使用真正的加密算法）
        "encrypted:" + plaintext + ":with_key:" + key
      },
      decrypt: fn(ciphertext: String) {
        // 简单的模拟解密
        if ciphertext.starts_with("encrypted:") and ciphertext.contains(":with_key:") {
          let parts = ciphertext.split(":")
          if parts.length() >= 4 and parts[3] == key {
            parts[1]
          } else {
            "DECRYPTION_FAILED"
          }
        } else {
          "DECRYPTION_FAILED"
        }
      }
    }
  }
  
  // 创建安全配置管理器
  let create_secure_config_manager = fn(base_manager: ConfigManager, encryptor: ConfigEncryptor) {
    {
      base_manager,
      encryptor,
      encrypted_items: Map::new(),
      sensitive_keys: Set::new(),
      access_log: []
    }
  }
  
  // 标记敏感键
  let mark_sensitive = fn(manager: SecureConfigManager, key: String) {
    let updated_sensitive_keys = manager.sensitive_keys.add(key)
    { manager | sensitive_keys: updated_sensitive_keys }
  }
  
  // 加密并存储配置值
  let store_encrypted_config = fn(manager: SecureConfigManager, key: String, value: String) {
    let encrypted_value = manager.encryptor.encrypt(value)
    let iv = "iv_" + Random::string(16)
    
    let encrypted_item = {
      key,
      encrypted_value,
      encryption_algorithm: manager.encryptor.algorithm,
      iv,
      metadata: Map::new()
    }
    
    let updated_encrypted_items = manager.encrypted_items.set(key, encrypted_item)
    let updated_access_log = manager.access_log.push("Stored encrypted config: " + key)
    
    {
      base_manager: manager.base_manager,
      encryptor: manager.encryptor,
      encrypted_items: updated_encrypted_items,
      sensitive_keys: manager.sensitive_keys,
      access_log: updated_access_log
    }
  }
  
  // 获取并解密配置值
  let get_decrypted_config = fn(manager: SecureConfigManager, key: String) {
    let encrypted_item = manager.encrypted_items.get(key)
    
    match encrypted_item {
      Some(item) => {
        let decrypted_value = manager.encryptor.decrypt(item.encrypted_value)
        let updated_access_log = manager.access_log.push("Accessed encrypted config: " + key)
        
        let updated_manager = {
          base_manager: manager.base_manager,
          encryptor: manager.encryptor,
          encrypted_items: manager.encrypted_items,
          sensitive_keys: manager.sensitive_keys,
          access_log: updated_access_log
        }
        
        (updated_manager, Some(decrypted_value))
      }
      None => (manager, None)
    }
  }
  
  // 安全地设置配置值（敏感值自动加密）
  let set_config_secure = fn(manager: SecureConfigManager, key: String, value: String) {
    let updated_manager = if manager.sensitive_keys.contains(key) {
      store_encrypted_config(manager, key, value)
    } else {
      // 非敏感值直接存储
      let config_item = create_config_item(key, ConfigValue::StringValue(value), ConfigSource::Default, 1, true)
      let updated_layer = create_config_layer("secure", ConfigSource::Default, 1)
      let layer_with_item = add_config_item(updated_layer, key, ConfigValue::StringValue(value), true)
      let updated_base_manager = add_config_layer(manager.base_manager, layer_with_item)
      
      {
        base_manager: updated_base_manager,
        encryptor: manager.encryptor,
        encrypted_items: manager.encrypted_items,
        sensitive_keys: manager.sensitive_keys,
        access_log: manager.access_log.push("Stored plain config: " + key)
      }
    }
    
    updated_manager
  }
  
  // 安全地获取配置值
  let get_config_secure = fn(manager: SecureConfigManager, key: String) {
    if manager.sensitive_keys.contains(key) {
      // 从加密存储获取
      let (updated_manager, decrypted_value) = get_decrypted_config(manager, key)
      (updated_manager, decrypted_value)
    } else {
      // 从普通配置获取
      let value = get_config_value(manager.base_manager, key)
      let updated_manager = {
        base_manager: manager.base_manager,
        encryptor: manager.encryptor,
        encrypted_items: manager.encrypted_items,
        sensitive_keys: manager.sensitive_keys,
        access_log: manager.access_log.push("Accessed plain config: " + key)
      }
      
      match value {
        Some(ConfigValue::StringValue(s)) => (updated_manager, Some(s))
        _ => (updated_manager, None)
      }
    }
  }
  
  // 创建基础配置管理器
  let base_manager = create_config_manager()
  
  // 创建加密器
  let encryptor = create_simple_encryptor("my-secret-key-12345")
  
  // 创建安全配置管理器
  let mut secure_manager = create_secure_config_manager(base_manager, encryptor)
  
  // 标记敏感键
  secure_manager = mark_sensitive(secure_manager, "database.password")
  secure_manager = mark_sensitive(secure_manager, "api.secret")
  secure_manager = mark_sensitive(secure_manager, "jwt.private_key")
  
  // 存储配置值
  secure_manager = set_config_secure(secure_manager, "app.name", "myapp")
  secure_manager = set_config_secure(secure_manager, "database.password", "super-secret-password")
  secure_manager = set_config_secure(secure_manager, "api.secret", "api-key-12345")
  secure_manager = set_config_secure(secure_manager, "jwt.private_key", "private-key-content")
  secure_manager = set_config_secure(secure_manager, "server.port", "8080")  # 非敏感
  
  // 验证访问日志
  assert_eq(secure_manager.access_log.length(), 5)
  assert_true(secure_manager.access_log.contains("Stored plain config: app.name"))
  assert_true(secure_manager.access_log.contains("Stored encrypted config: database.password"))
  assert_true(secure_manager.access_log.contains("Stored encrypted config: api.secret"))
  assert_true(secure_manager.access_log.contains("Stored encrypted config: jwt.private_key"))
  assert_true(secure_manager.access_log.contains("Stored plain config: server.port"))
  
  // 获取非敏感配置
  let (manager1, app_name) = get_config_secure(secure_manager, "app.name")
  match app_name {
    Some(name) => assert_eq(name, "myapp")
    None => assert_true(false)
  }
  
  // 获取敏感配置
  let (manager2, db_password) = get_config_secure(manager1, "database.password")
  match db_password {
    Some(password) => assert_eq(password, "super-secret-password")
    None => assert_true(false)
  }
  
  // 验证加密存储
  let encrypted_item = manager2.encrypted_items.get("database.password")
  match encrypted_item {
    Some(item) => {
      assert_eq(item.key, "database.password")
      assert_eq(item.encryption_algorithm, "AES-256")
      assert_true(item.encrypted_value.starts_with("encrypted:"))
      assert_true(item.encrypted_value.contains("super-secret-password"))
    }
    None => assert_true(false)
  }
  
  // 验证访问日志更新
  assert_eq(manager2.access_log.length(), 7)
  assert_true(manager2.access_log.contains("Accessed plain config: app.name"))
  assert_true(manager2.access_log.contains("Accessed encrypted config: database.password"))
  
  // 测试解密失败
  let wrong_encryptor = create_simple_encryptor("wrong-key")
  let manager_with_wrong_encryptor = { manager2 | encryptor: wrong_encryptor }
  
  let (_, wrong_password) = get_config_secure(manager_with_wrong_encryptor, "api.secret")
  match wrong_password {
    Some(password) => assert_eq(password, "DECRYPTION_FAILED")
    None => assert_true(false)
  }
  
  // 测试批量敏感操作
  let sensitive_keys = ["database.password", "api.secret", "jwt.private_key"]
  
  for key in sensitive_keys {
    assert_true(manager2.sensitive_keys.contains(key))
    
    let (_, value) = get_config_secure(manager2, key)
    match value {
      Some(v) => assert_true(v != "DECRYPTION_FAILED")
      None => assert_true(false)
    }
  }
  
  // 验证非敏感键
  assert_false(manager2.sensitive_keys.contains("app.name"))
  assert_false(manager2.sensitive_keys.contains("server.port"))
}

// 测试5: 配置模板和继承
test "配置模板和继承测试" {
  // 定义配置模板
  type ConfigTemplate = {
    name: String,
    description: String,
    parameters: Map[String, ConfigValue],
    template: Map[String, ConfigValue]
  }
  
  // 定义配置实例
  type ConfigInstance = {
    template_name: String,
    parameter_values: Map[String, ConfigValue],
    resolved_config: Map[String, ConfigValue]
  }
  
  // 定义模板管理器
  type ConfigTemplateManager = {
    templates: Map[String, ConfigTemplate],
    instances: Map[String, ConfigInstance],
    variable_resolver: String -> Option[ConfigValue]
  }
  
  // 创建配置模板
  let create_config_template = fn(name: String, description: String, parameters: Map[String, ConfigValue], template: Map[String, ConfigValue]) {
    {
      name,
      description,
      parameters,
      template
    }
  }
  
  // 创建配置实例
  let create_config_instance = fn(template_name: String, parameter_values: Map[String, ConfigValue]) {
    {
      template_name,
      parameter_values,
      resolved_config: Map::new()
    }
  }
  
  // 创建模板管理器
  let create_config_template_manager = fn() {
    {
      templates: Map::new(),
      instances: Map::new(),
      variable_resolver: fn(var_name: String) {
        // 简单的变量解析器
        match var_name {
          "ENV" => Some(ConfigValue::StringValue("production")),
          "HOSTNAME" => Some(ConfigValue::StringValue("web-server-01")),
          _ => None
        }
      }
    }
  }
  
  // 解析模板变量
  let resolve_template_variables = fn(template: Map[String, ConfigValue], parameters: Map[String, ConfigValue], resolver: String -> Option[ConfigValue]) {
    let mut resolved = Map::new()
    
    for (key, value) in template {
      let resolved_value = match value {
        ConfigValue::StringValue(s) => {
          if s.starts_with("${") and s.ends_with("}") {
            // 提取变量名
            let var_name = s.substring(2, s.length() - 2)
            
            // 首先检查参数
            let param_value = parameters.get(var_name)
            match param_value {
              Some(pv) => pv,
              None => {
                // 然后检查解析器
                resolver(var_name)
              }
            }
          } else {
            value
          }
        }
        _ => value
      }
      
      resolved = resolved.set(key, resolved_value)
    }
    
    resolved
  }
  
  // 添加模板
  let add_template = fn(manager: ConfigTemplateManager, template: ConfigTemplate) {
    let updated_templates = manager.templates.set(template.name, template)
    { manager | templates: updated_templates }
  }
  
  // 实例化模板
  let instantiate_template = fn(manager: ConfigTemplateManager, instance_name: String, template_name: String, parameter_values: Map[String, ConfigValue]) {
    let template = manager.templates.get(template_name)
    match template {
      Some(t) => {
        // 验证参数
        let mut all_params_provided = true
        
        for (param_name, _) in t.parameters {
          if not(parameter_values.contains(param_name)) {
            all_params_provided = false
            break
          }
        }
        
        if all_params_provided {
          // 解析模板
          let resolved_config = resolve_template_variables(t.template, parameter_values, manager.variable_resolver)
          
          let instance = {
            template_name,
            parameter_values,
            resolved_config
          }
          
          let updated_instances = manager.instances.set(instance_name, instance)
          
          let updated_manager = {
            templates: manager.templates,
            instances: updated_instances,
            variable_resolver: manager.variable_resolver
          }
          
          (updated_manager, Some(instance))
        } else {
          (manager, None)
        }
      }
      None => (manager, None)
    }
  }
  
  // 获取实例配置
  let get_instance_config = fn(manager: ConfigTemplateManager, instance_name: String) {
    let instance = manager.instances.get(instance_name)
    match instance {
      Some(i) => Some(i.resolved_config)
      None => None
    }
  }
  
  // 创建模板管理器
  let mut manager = create_config_template_manager()
  
  // 创建Web服务模板
  let web_service_params = Map::new()
  let web_service_params_with_port = web_service_params.set("port", ConfigValue::IntValue(8080))
  let web_service_params_with_port_host = web_service_params_with_port.set("host", ConfigValue::StringValue("localhost"))
  
  let mut web_service_template = Map::new()
  web_service_template = web_service_template.set("server.port", ConfigValue::StringValue("${port}"))
  web_service_template = web_service_template.set("server.host", ConfigValue::StringValue("${host}"))
  web_service_template = web_service_template.set("server.url", ConfigValue::StringValue("http://${host}:${port}"))
  web_service_template = web_service_template.set("environment", ConfigValue::StringValue("${ENV}"))
  
  let web_service_config_template = create_config_template(
    "web-service",
    "Template for web service configuration",
    web_service_params_with_port_host,
    web_service_template
  )
  
  // 创建数据库模板
  let db_params = Map::new()
  let db_params_with_type = db_params.set("type", ConfigValue::StringValue("postgres"))
  let db_params_with_type_host = db_params_with_type.set("host", ConfigValue::StringValue("localhost"))
  
  let mut db_template = Map::new()
  db_template = db_template.set("database.type", ConfigValue::StringValue("${type}"))
  db_template = db_template.set("database.host", ConfigValue::StringValue("${host}"))
  db_template = db_template.set("database.url", ConfigValue::StringValue("${type}://${host}:5432/mydb"))
  db_template = db_template.set("database.pool_size", ConfigValue::IntValue(10))
  
  let db_config_template = create_config_template(
    "database",
    "Template for database configuration",
    db_params_with_type_host,
    db_template
  )
  
  // 添加模板
  manager = add_template(manager, web_service_config_template)
  manager = add_template(manager, db_config_template)
  
  // 实例化Web服务
  let web_service_params = Map::new()
  let web_service_params_with_port = web_service_params.set("port", ConfigValue::IntValue(9000))
  let web_service_params_with_port_host = web_service_params_with_port.set("host", ConfigValue::StringValue("api.example.com"))
  
  let (manager1, web_instance) = instantiate_template(manager, "web-api", "web-service", web_service_params_with_port_host)
  assert_true(web_instance != None)
  
  // 验证Web服务实例
  match web_instance {
    Some(instance) => {
      assert_eq(instance.template_name, "web-service")
      assert_eq(instance.parameter_values.size(), 2)
      
      let server_port = instance.resolved_config.get("server.port")
      match server_port {
        Some(ConfigValue::IntValue(port)) => assert_eq(port, 9000)
        _ => assert_true(false)
      }
      
      let server_host = instance.resolved_config.get("server.host")
      match server_host {
        Some(ConfigValue::StringValue(host)) => assert_eq(host, "api.example.com")
        _ => assert_true(false)
      }
      
      let server_url = instance.resolved_config.get("server.url")
      match server_url {
        Some(ConfigValue::StringValue(url)) => assert_eq(url, "http://api.example.com:9000")
        _ => assert_true(false)
      }
      
      let environment = instance.resolved_config.get("environment")
      match environment {
        Some(ConfigValue::StringValue(env)) => assert_eq(env, "production")  # 从解析器获取
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 实例化数据库
  let db_params = Map::new()
  let db_params_with_type = db_params.set("type", ConfigValue::StringValue("mysql"))
  let db_params_with_type_host = db_params_with_type.set("host", ConfigValue::StringValue("db.example.com"))
  
  let (manager2, db_instance) = instantiate_template(manager1, "main-db", "database", db_params_with_type_host)
  assert_true(db_instance != None)
  
  // 验证数据库实例
  match db_instance {
    Some(instance) => {
      assert_eq(instance.template_name, "database")
      
      let database_type = instance.resolved_config.get("database.type")
      match database_type {
        Some(ConfigValue::StringValue(type)) => assert_eq(type, "mysql")
        _ => assert_true(false)
      }
      
      let database_host = instance.resolved_config.get("database.host")
      match database_host {
        Some(ConfigValue::StringValue(host)) => assert_eq(host, "db.example.com")
        _ => assert_true(false)
      }
      
      let database_url = instance.resolved_config.get("database.url")
      match database_url {
        Some(ConfigValue::StringValue(url)) => assert_eq(url, "mysql://db.example.com:5432/mydb")
        _ => assert_true(false)
      }
      
      let pool_size = instance.resolved_config.get("database.pool_size")
      match pool_size {
        Some(ConfigValue::IntValue(size)) => assert_eq(size, 10)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试缺少参数的情况
  let incomplete_params = Map::new()
  let incomplete_params_with_port = incomplete_params.set("port", ConfigValue::IntValue(8000))
  // 缺少host参数
  
  let (manager3, incomplete_instance) = instantiate_template(manager2, "incomplete", "web-service", incomplete_params_with_port)
  assert_eq(incomplete_instance, None)
  
  // 获取实例配置
  let web_config = get_instance_config(manager3, "web-api")
  assert_true(web_config != None)
  
  match web_config {
    Some(config) => {
      assert_eq(config.size(), 4)  # server.port, server.host, server.url, environment
    }
    None => assert_true(false)
  }
  
  let db_config = get_instance_config(manager3, "main-db")
  assert_true(db_config != None)
  
  match db_config {
    Some(config) => {
      assert_eq(config.size(), 4)  # database.type, database.host, database.url, database.pool_size
    }
    None => assert_true(false)
  }
}