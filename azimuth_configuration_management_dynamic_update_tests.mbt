// Azimuth Configuration Management and Dynamic Update Test Suite
// 测试配置管理和动态更新功能，确保系统配置的灵活性和可靠性

test "基础配置管理测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 测试空配置
  assert_eq(ConfigManager::size(config_manager), 0)
  assert_true(ConfigManager::is_empty(config_manager))
  
  // 添加配置项
  ConfigManager::set(config_manager, "service.name", "payment-service")
  ConfigManager::set(config_manager, "service.version", "2.1.0")
  ConfigManager::set(config_manager, "service.port", 8080)
  ConfigManager::set(config_manager, "service.enabled", true)
  
  // 验证配置项数量
  assert_eq(ConfigManager::size(config_manager), 4)
  assert_false(ConfigManager::is_empty(config_manager))
  
  // 获取配置项
  let service_name = ConfigManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "payment-service")
    None => assert_true(false)
  }
  
  let service_version = ConfigManager::get_string(config_manager, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "2.1.0")
    None => assert_true(false)
  }
  
  let service_port = ConfigManager::get_int(config_manager, "service.port")
  match service_port {
    Some(port) => assert_eq(port, 8080)
    None => assert_true(false)
  }
  
  let service_enabled = ConfigManager::get_bool(config_manager, "service.enabled")
  match service_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  // 获取不存在的配置项
  let non_existent = ConfigManager::get_string(config_manager, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试配置项存在性检查
  assert_true(ConfigManager::contains(config_manager, "service.name"))
  assert_false(ConfigManager::contains(config_manager, "non.existent.key"))
  
  // 删除配置项
  ConfigManager::remove(config_manager, "service.enabled")
  assert_eq(ConfigManager::size(config_manager), 3)
  assert_false(ConfigManager::contains(config_manager, "service.enabled"))
}

test "配置层次结构测试" {
  // 创建分层的配置管理器
  let config_manager = HierarchicalConfigManager::new()
  
  // 添加不同层级的配置
  HierarchicalConfigManager::set(config_manager, "service.name", "payment-service", "default")
  HierarchicalConfigManager::set(config_manager, "service.port", 8080, "default")
  HierarchicalConfigManager::set(config_manager, "service.port", 9090, "production") // 覆盖默认值
  HierarchicalConfigManager::set(config_manager, "service.debug", true, "development")
  HierarchicalConfigManager::set(config_manager, "service.debug", false, "production")
  
  // 设置当前环境
  HierarchicalConfigManager::set_environment(config_manager, "production")
  
  // 获取配置项（应使用当前环境的值）
  let service_name = HierarchicalConfigManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "payment-service") // 来自default环境
    None => assert_true(false)
  }
  
  let service_port = HierarchicalConfigManager::get_int(config_manager, "service.port")
  match service_port {
    Some(port) => assert_eq(port, 9090) // 来自production环境
    None => assert_true(false)
  }
  
  let service_debug = HierarchicalConfigManager::get_bool(config_manager, "service.debug")
  match service_debug {
    Some(debug) => assert_false(debug) // 来自production环境
    None => assert_true(false)
  }
  
  // 切换环境
  HierarchicalConfigManager::set_environment(config_manager, "development")
  
  // 获取配置项（应使用新环境的值）
  let service_debug_dev = HierarchicalConfigManager::get_bool(config_manager, "service.debug")
  match service_debug_dev {
    Some(debug) => assert_true(debug) // 来自development环境
    None => assert_true(false)
  }
  
  let service_port_dev = HierarchicalConfigManager::get_int(config_manager, "service.port")
  match service_port_dev {
    Some(port) => assert_eq(port, 8080) // 来自default环境（development环境没有设置）
    None => assert_true(false)
  }
}

test "配置文件加载测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 创建测试配置文件内容
  let json_config = "{\n    \"service\": {\n      \"name\": \"payment-service\",\n      \"version\": \"2.1.0\",\n      \"port\": 8080,\n      \"enabled\": true,\n      \"features\": [\"api\", \"webhook\", \"reporting\"]\n    },\n    \"database\": {\n      \"host\": \"localhost\",\n      \"port\": 5432,\n      \"name\": \"payment_db\",\n      \"ssl\": false\n    },\n    \"logging\": {\n      \"level\": \"info\",\n      \"format\": \"json\"\n    }\n  }"
  
  // 从JSON加载配置
  let load_result = ConfigManager::load_from_json(config_manager, json_config)
  assert_true(load_result)
  
  // 验证配置项已加载
  let service_name = ConfigManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "payment-service")
    None => assert_true(false)
  }
  
  let service_port = ConfigManager::get_int(config_manager, "service.port")
  match service_port {
    Some(port) => assert_eq(port, 8080)
    None => assert_true(false)
  }
  
  let service_enabled = ConfigManager::get_bool(config_manager, "service.enabled")
  match service_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let database_host = ConfigManager::get_string(config_manager, "database.host")
  match database_host {
    Some(host) => assert_eq(host, "localhost")
    None => assert_true(false)
  }
  
  // 测试数组配置项
  let service_features = ConfigManager::get_string_array(config_manager, "service.features")
  match service_features {
    Some(features) => {
      assert_eq(features.length(), 3)
      assert_eq(features[0], "api")
      assert_eq(features[1], "webhook")
      assert_eq(features[2], "reporting")
    }
    None => assert_true(false)
  }
  
  // 测试无效JSON
  let invalid_json = "{ invalid json }"
  let invalid_load_result = ConfigManager::load_from_json(config_manager, invalid_json)
  assert_false(invalid_load_result)
}

test "配置验证测试" {
  // 创建带验证规则的配置管理器
  let config_manager = ValidatingConfigManager::new()
  
  // 添加验证规则
  ValidatingConfigManager::add_rule(config_manager, "service.port", 
    ValidationRule::range(1, 65535))
  ValidatingConfigManager::add_rule(config_manager, "service.name", 
    ValidationRule::non_empty())
  ValidatingConfigManager::add_rule(config_manager, "service.version", 
    ValidationRule::regex("^\\d+\\.\\d+\\.\\d+$")) // 语义版本号
  ValidatingConfigManager::add_rule(config_manager, "database.connection_pool_size", 
    ValidationRule::range(1, 100))
  
  // 测试有效配置
  let valid_result = ValidatingConfigManager::set(config_manager, "service.port", 8080)
  assert_true(valid_result)
  
  let valid_result = ValidatingConfigManager::set(config_manager, "service.name", "payment-service")
  assert_true(valid_result)
  
  let valid_result = ValidatingConfigManager::set(config_manager, "service.version", "2.1.0")
  assert_true(valid_result)
  
  let valid_result = ValidatingConfigManager::set(config_manager, "database.connection_pool_size", 10)
  assert_true(valid_result)
  
  // 测试无效配置
  let invalid_result = ValidatingConfigManager::set(config_manager, "service.port", 70000) // 超出范围
  assert_false(invalid_result)
  
  let invalid_result = ValidatingConfigManager::set(config_manager, "service.name", "") // 空字符串
  assert_false(invalid_result)
  
  let invalid_result = ValidatingConfigManager::set(config_manager, "service.version", "2.1") // 不匹配正则
  assert_false(invalid_result)
  
  let invalid_result = ValidatingConfigManager::set(config_manager, "database.connection_pool_size", 0) // 小于最小值
  assert_false(invalid_result)
  
  // 验证所有配置
  let validation_result = ValidatingConfigManager::validate_all(config_manager)
  assert_true(validation_result.is_valid)
  
  // 添加无效配置
  ValidatingConfigManager::set_without_validation(config_manager, "service.port", 70000)
  
  // 再次验证所有配置
  let validation_result = ValidatingConfigManager::validate_all(config_manager)
  assert_false(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 1)
  assert_true(validation_result.errors[0].contains("service.port"))
}

test "动态配置更新测试" {
  // 创建动态配置管理器
  let config_manager = DynamicConfigManager::new()
  
  // 添加配置项
  DynamicConfigManager::set(config_manager, "service.name", "payment-service")
  DynamicConfigManager::set(config_manager, "service.port", 8080)
  DynamicConfigManager::set(config_manager, "service.enabled", true)
  
  // 添加配置变更监听器
  let mut change_events = []
  DynamicConfigManager::add_listener(config_manager, |key, old_value, new_value| {
    change_events = change_events + [(key, old_value, new_value)]
  })
  
  // 更新配置项
  DynamicConfigManager::set(config_manager, "service.port", 9090)
  DynamicConfigManager::set(config_manager, "service.enabled", false)
  
  // 验证变更事件
  assert_eq(change_events.length(), 2)
  assert_eq(change_events[0].0, "service.port")
  assert_eq(change_events[0].1, "8080")
  assert_eq(change_events[0].2, "9090")
  
  assert_eq(change_events[1].0, "service.enabled")
  assert_eq(change_events[1].1, "true")
  assert_eq(change_events[1].2, "false")
  
  // 验证配置值已更新
  let service_port = DynamicConfigManager::get_int(config_manager, "service.port")
  match service_port {
    Some(port) => assert_eq(port, 9090)
    None => assert_true(false)
  }
  
  let service_enabled = DynamicConfigManager::get_bool(config_manager, "service.enabled")
  match service_enabled {
    Some(enabled) => assert_false(enabled)
    None => assert_true(false)
  }
  
  // 测试批量更新
  let updates = [
    ("service.name", "order-service"),
    ("service.version", "3.0.0"),
    ("service.debug", true)
  ]
  DynamicConfigManager::set_batch(config_manager, updates)
  
  // 验证批量更新触发了变更事件
  assert_eq(change_events.length(), 5)
  assert_eq(change_events[2].0, "service.name")
  assert_eq(change_events[2].1, "payment-service")
  assert_eq(change_events[2].2, "order-service")
  
  assert_eq(change_events[3].0, "service.version")
  assert_eq(change_events[3].1, "")
  assert_eq(change_events[3].2, "3.0.0")
  
  assert_eq(change_events[4].0, "service.debug")
  assert_eq(change_events[4].1, "")
  assert_eq(change_events[4].2, "true")
}

test "配置热重载测试" {
  // 创建配置文件路径
  let config_file = "/tmp/azimuth_test_config.json"
  
  // 创建初始配置文件
  let initial_config = "{\n    \"service\": {\n      \"name\": \"payment-service\",\n      \"port\": 8080,\n      \"enabled\": true\n    },\n    \"database\": {\n      \"host\": \"localhost\",\n      \"port\": 5432\n    }\n  }"
  
  File::write(config_file, initial_config)
  
  // 创建热重载配置管理器
  let config_manager = HotReloadConfigManager::new(config_file)
  
  // 加载初始配置
  HotReloadConfigManager::load(config_manager)
  
  // 验证初始配置
  let service_name = HotReloadConfigManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "payment-service")
    None => assert_true(false)
  }
  
  let service_port = HotReloadConfigManager::get_int(config_manager, "service.port")
  match service_port {
    Some(port) => assert_eq(port, 8080)
    None => assert_true(false)
  }
  
  // 添加重载监听器
  let mut reload_events = []
  HotReloadConfigManager::add_reload_listener(config_manager, || {
    reload_events = reload_events + [Time::now()]
  })
  
  // 更新配置文件
  let updated_config = "{\n    \"service\": {\n      \"name\": \"order-service\",\n      \"port\": 9090,\n      \"enabled\": true\n    },\n    \"database\": {\n      \"host\": \"db.example.com\",\n      \"port\": 5432\n    }\n  }"
  
  File::write(config_file, updated_config)
  
  // 触发热重载
  HotReloadConfigManager::reload(config_manager)
  
  // 验证重载事件
  assert_eq(reload_events.length(), 1)
  
  // 验证配置已更新
  let updated_service_name = HotReloadConfigManager::get_string(config_manager, "service.name")
  match updated_service_name {
    Some(name) => assert_eq(name, "order-service")
    None => assert_true(false)
  }
  
  let updated_service_port = HotReloadConfigManager::get_int(config_manager, "service.port")
  match updated_service_port {
    Some(port) => assert_eq(port, 9090)
    None => assert_true(false)
  }
  
  let updated_db_host = HotReloadConfigManager::get_string(config_manager, "database.host")
  match updated_db_host {
    Some(host) => assert_eq(host, "db.example.com")
    None => assert_true(false)
  }
  
  // 清理测试文件
  File::delete(config_file)
}

test "配置加密测试" {
  // 创建加密配置管理器
  let config_manager = EncryptedConfigManager::new("encryption_key_123456789")
  
  // 添加敏感配置项
  EncryptedConfigManager::set_sensitive(config_manager, "database.password", "super_secret_password")
  EncryptedConfigManager::set_sensitive(config_manager, "api.key", "sk-1234567890abcdef")
  EncryptedConfigManager::set(config_manager, "service.name", "payment-service") // 非敏感配置
  
  // 获取敏感配置项
  let db_password = EncryptedConfigManager::get_string(config_manager, "database.password")
  match db_password {
    Some(password) => assert_eq(password, "super_secret_password")
    None => assert_true(false)
  }
  
  let api_key = EncryptedConfigManager::get_string(config_manager, "api.key")
  match api_key {
    Some(key) => assert_eq(key, "sk-1234567890abcdef")
    None => assert_true(false)
  }
  
  // 获取非敏感配置项
  let service_name = EncryptedConfigManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "payment-service")
    None => assert_true(false)
  }
  
  // 导出配置（敏感项应被加密）
  let exported_config = EncryptedConfigManager::export(config_manager)
  
  // 验证敏感项已加密
  assert_false(exported_config.contains("super_secret_password"))
  assert_false(exported_config.contains("sk-1234567890abcdef"))
  assert_true(exported_config.contains("payment-service")) // 非敏感项应保持原样
  
  // 创建新的配置管理器并导入配置
  let new_config_manager = EncryptedConfigManager::new("encryption_key_123456789")
  let import_result = EncryptedConfigManager::import(new_config_manager, exported_config)
  assert_true(import_result)
  
  // 验证导入的配置
  let imported_db_password = EncryptedConfigManager::get_string(new_config_manager, "database.password")
  match imported_db_password {
    Some(password) => assert_eq(password, "super_secret_password")
    None => assert_true(false)
  }
  
  let imported_api_key = EncryptedConfigManager::get_string(new_config_manager, "api.key")
  match imported_api_key {
    Some(key) => assert_eq(key, "sk-1234567890abcdef")
    None => assert_true(false)
  }
}

test "配置模板测试" {
  // 创建配置模板管理器
  let template_manager = ConfigTemplateManager::new()
  
  // 定义配置模板
  let service_template = "{\n    \"name\": \"{{service_name}}\",\n    \"version\": \"{{service_version}}\",\n    \"port\": {{service_port}},\n    \"enabled\": {{service_enabled}},\n    \"features\": [{{#each features}}\"{{this}}\"{{#unless @last}},{{/unless}}{{/each}}]\n  }"
  
  // 注册模板
  ConfigTemplateManager::register_template(template_manager, "service", service_template)
  
  // 定义模板数据
  let template_data = {
    "service_name": "payment-service",
    "service_version": "2.1.0",
    "service_port": 8080,
    "service_enabled": true,
    "features": ["api", "webhook", "reporting"]
  }
  
  // 渲染模板
  let rendered_config = ConfigTemplateManager::render(template_manager, "service", template_data)
  
  // 验证渲染结果
  assert_true(rendered_config.contains("\"name\": \"payment-service\""))
  assert_true(rendered_config.contains("\"version\": \"2.1.0\""))
  assert_true(rendered_config.contains("\"port\": 8080"))
  assert_true(rendered_config.contains("\"enabled\": true"))
  assert_true(rendered_config.contains("\"features\": [\"api\",\"webhook\",\"reporting\"]"))
  
  // 使用渲染的配置创建配置管理器
  let config_manager = ConfigManager::new()
  let load_result = ConfigManager::load_from_json(config_manager, rendered_config)
  assert_true(load_result)
  
  // 验证配置项
  let service_name = ConfigManager::get_string(config_manager, "name")
  match service_name {
    Some(name) => assert_eq(name, "payment-service")
    None => assert_true(false)
  }
  
  let service_port = ConfigManager::get_int(config_manager, "port")
  match service_port {
    Some(port) => assert_eq(port, 8080)
    None => assert_true(false)
  }
  
  let service_features = ConfigManager::get_string_array(config_manager, "features")
  match service_features {
    Some(features) => {
      assert_eq(features.length(), 3)
      assert_eq(features[0], "api")
      assert_eq(features[1], "webhook")
      assert_eq(features[2], "reporting")
    }
    None => assert_true(false)
  }
}

test "配置合并测试" {
  // 创建配置管理器
  let base_config = ConfigManager::new()
  
  // 添加基础配置
  ConfigManager::set(base_config, "service.name", "payment-service")
  ConfigManager::set(base_config, "service.version", "2.1.0")
  ConfigManager::set(base_config, "service.port", 8080)
  ConfigManager::set(base_config, "service.enabled", true)
  ConfigManager::set(base_config, "database.host", "localhost")
  ConfigManager::set(base_config, "database.port", 5432)
  
  // 创建覆盖配置
  let override_config = ConfigManager::new()
  
  // 添加覆盖配置
  ConfigManager::set(override_config, "service.port", 9090) // 覆盖基础配置
  ConfigManager::set(override_config, "service.debug", true) // 新增配置
  ConfigManager::set(override_config, "database.host", "db.example.com") // 覆盖基础配置
  ConfigManager::set(override_config, "database.ssl", true) // 新增配置
  
  // 合并配置
  let merged_config = ConfigManager::merge(base_config, override_config)
  
  // 验证合并结果
  let service_name = ConfigManager::get_string(merged_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "payment-service") // 来自基础配置
    None => assert_true(false)
  }
  
  let service_version = ConfigManager::get_string(merged_config, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "2.1.0") // 来自基础配置
    None => assert_true(false)
  }
  
  let service_port = ConfigManager::get_int(merged_config, "service.port")
  match service_port {
    Some(port) => assert_eq(port, 9090) // 来自覆盖配置
    None => assert_true(false)
  }
  
  let service_enabled = ConfigManager::get_bool(merged_config, "service.enabled")
  match service_enabled {
    Some(enabled) => assert_true(enabled) // 来自基础配置
    None => assert_true(false)
  }
  
  let service_debug = ConfigManager::get_bool(merged_config, "service.debug")
  match service_debug {
    Some(debug) => assert_true(debug) // 来自覆盖配置
    None => assert_true(false)
  }
  
  let database_host = ConfigManager::get_string(merged_config, "database.host")
  match database_host {
    Some(host) => assert_eq(host, "db.example.com") // 来自覆盖配置
    None => assert_true(false)
  }
  
  let database_port = ConfigManager::get_int(merged_config, "database.port")
  match database_port {
    Some(port) => assert_eq(port, 5432) // 来自基础配置
    None => assert_true(false)
  }
  
  let database_ssl = ConfigManager::get_bool(merged_config, "database.ssl")
  match database_ssl {
    Some(ssl) => assert_true(ssl) // 来自覆盖配置
    None => assert_true(false)
  }
}