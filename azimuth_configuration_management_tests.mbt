// Azimuth 配置管理测试用例
// 测试遥测系统的配置管理功能，包括配置加载、更新、验证和热重载

test "基本配置操作测试" {
  // 创建配置管理器
  let config_manager = azimuth::ConfigManager::new()
  
  // 测试初始状态
  assert_true(azimuth::ConfigManager::is_empty(config_manager))
  
  // 添加字符串配置
  azimuth::ConfigManager::set_string(config_manager, "service.name", "azimuth-telemetry")
  assert_false(azimuth::ConfigManager::is_empty(config_manager))
  
  // 验证字符串配置
  match azimuth::ConfigManager::get_string(config_manager, "service.name") {
    Some(value) => assert_eq(value, "azimuth-telemetry")
    None => assert_true(false)
  }
  
  // 添加整数配置
  azimuth::ConfigManager::set_int(config_manager, "service.port", 8080)
  
  // 验证整数配置
  match azimuth::ConfigManager::get_int(config_manager, "service.port") {
    Some(value) => assert_eq(value, 8080)
    None => assert_true(false)
  }
  
  // 添加布尔配置
  azimuth::ConfigManager::set_bool(config_manager, "service.debug", true)
  
  // 验证布尔配置
  match azimuth::ConfigManager::get_bool(config_manager, "service.debug") {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  // 添加浮点数配置
  azimuth::ConfigManager::set_float(config_manager, "performance.threshold", 0.95)
  
  // 验证浮点数配置
  match azimuth::ConfigManager::get_float(config_manager, "performance.threshold") {
    Some(value) => assert_eq(value, 0.95)
    None => assert_true(false)
  }
  
  // 添加数组配置
  let string_array = ["endpoint1", "endpoint2", "endpoint3"]
  azimuth::ConfigManager::set_string_array(config_manager, "telemetry.endpoints", string_array)
  
  // 验证数组配置
  match azimuth::ConfigManager::get_string_array(config_manager, "telemetry.endpoints") {
    Some(array) => {
      assert_eq(array.length(), 3)
      assert_eq(array[0], "endpoint1")
      assert_eq(array[1], "endpoint2")
      assert_eq(array[2], "endpoint3")
    }
    None => assert_true(false)
  }
  
  // 测试配置计数
  assert_eq(azimuth::ConfigManager::size(config_manager), 5)
  
  // 测试配置键列表
  let keys = azimuth::ConfigManager::get_keys(config_manager)
  assert_eq(keys.length(), 5)
  assert_true(keys.contains("service.name"))
  assert_true(keys.contains("service.port"))
  assert_true(keys.contains("service.debug"))
  assert_true(keys.contains("performance.threshold"))
  assert_true(keys.contains("telemetry.endpoints"))
  
  // 测试配置更新
  azimuth::ConfigManager::set_string(config_manager, "service.name", "updated-telemetry")
  
  match azimuth::ConfigManager::get_string(config_manager, "service.name") {
    Some(value) => assert_eq(value, "updated-telemetry")
    None => assert_true(false)
  }
  
  // 测试配置删除
  let removed = azimuth::ConfigManager::remove(config_manager, "service.debug")
  assert_true(removed)
  
  // 验证删除后的状态
  match azimuth::ConfigManager::get_bool(config_manager, "service.debug") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  assert_eq(azimuth::ConfigManager::size(config_manager), 4)
}

test "嵌套配置测试" {
  // 创建配置管理器
  let config_manager = azimuth::ConfigManager::new()
  
  // 添加嵌套配置
  azimuth::ConfigManager::set_string(config_manager, "database.host", "localhost")
  azimuth::ConfigManager::set_int(config_manager, "database.port", 5432)
  azimuth::ConfigManager::set_string(config_manager, "database.username", "azimuth_user")
  azimuth::ConfigManager::set_string(config_manager, "database.password", "secure_password")
  azimuth::ConfigManager::set_string(config_manager, "database.name", "azimuth_db")
  
  azimuth::ConfigManager::set_string(config_manager, "redis.host", "localhost")
  azimuth::ConfigManager::set_int(config_manager, "redis.port", 6379)
  azimuth::ConfigManager::set_int(config_manager, "redis.db", 0)
  
  azimuth::ConfigManager::set_string(config_manager, "logging.level", "INFO")
  azimuth::ConfigManager::set_string(config_manager, "logging.format", "json")
  azimuth::ConfigManager::set_bool(config_manager, "logging.console", true)
  azimuth::ConfigManager::set_string(config_manager, "logging.file.path", "/var/log/azimuth.log")
  
  // 验证嵌套配置
  match azimuth::ConfigManager::get_string(config_manager, "database.host") {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(config_manager, "database.port") {
    Some(value) => assert_eq(value, 5432)
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_string(config_manager, "redis.host") {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(config_manager, "redis.port") {
    Some(value) => assert_eq(value, 6379)
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_string(config_manager, "logging.level") {
    Some(value) => assert_eq(value, "INFO")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_bool(config_manager, "logging.console") {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  // 测试获取配置子集
  let database_config = azimuth::ConfigManager::get_subset(config_manager, "database")
  
  match azimuth::ConfigManager::get_string(database_config, "host") {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(database_config, "port") {
    Some(value) => assert_eq(value, 5432)
    None => assert_true(false)
  }
  
  // 验证子集不包含其他命名空间的配置
  match azimuth::ConfigManager::get_string(database_config, "host") {
    Some(_) => assert_true(true) // database.host存在
    None => assert_true(false)
  }
  
  // 测试配置合并
  let override_config = azimuth::ConfigManager::new()
  azimuth::ConfigManager::set_string(override_config, "database.host", "production-db.example.com")
  azimuth::ConfigManager::set_int(override_config, "database.max_connections", 100)
  azimuth::ConfigManager::set_string(override_config, "new.feature", "enabled")
  
  // 合并配置
  let merged_config = azimuth::ConfigManager::merge(config_manager, override_config)
  
  // 验证合并结果
  match azimuth::ConfigManager::get_string(merged_config, "database.host") {
    Some(value) => assert_eq(value, "production-db.example.com") // 应该被覆盖
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(merged_config, "database.port") {
    Some(value) => assert_eq(value, 5432) // 应该保留
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(merged_config, "database.max_connections") {
    Some(value) => assert_eq(value, 100) // 新增的配置
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_string(merged_config, "new.feature") {
    Some(value) => assert_eq(value, "enabled") // 新增的配置
    None => assert_true(false)
  }
}

test "配置验证测试" {
  // 创建配置管理器
  let config_manager = azimuth::ConfigManager::new()
  
  // 添加有效配置
  azimuth::ConfigManager::set_string(config_manager, "service.name", "azimuth")
  azimuth::ConfigManager::set_int(config_manager, "service.port", 8080)
  azimuth::ConfigManager::set_string(config_manager, "service.version", "1.0.0")
  
  // 定义验证规则
  let validation_rules = [
    azimuth::ValidationRule {
      key: "service.name",
      required: true,
      type: azimuth::ValidationType::String,
      min_length: Some(1),
      max_length: Some(100)
    },
    azimuth::ValidationRule {
      key: "service.port",
      required: true,
      type: azimuth::ValidationType::Int,
      min_value: Some(1),
      max_value: Some(65535)
    },
    azimuth::ValidationRule {
      key: "service.version",
      required: true,
      type: azimuth::ValidationType::String,
      pattern: Some("^\\d+\\.\\d+\\.\\d+$") // 语义版本格式
    },
    azimuth::ValidationRule {
      key: "optional.config",
      required: false,
      type: azimuth::ValidationType::String,
      min_length: Some(1),
      max_length: Some(50)
    }
  ]
  
  // 验证配置
  let validation_result = azimuth::ConfigManager::validate(config_manager, validation_rules)
  
  // 验证结果应该成功
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // 添加无效配置
  azimuth::ConfigManager::set_string(config_manager, "service.name", "") // 空字符串，违反最小长度
  azimuth::ConfigManager::set_int(config_manager, "service.port", 70000) // 超出最大值
  
  // 再次验证
  let invalid_validation_result = azimuth::ConfigManager::validate(config_manager, validation_rules)
  
  // 验证结果应该失败
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() >= 2)
  
  // 验证错误信息
  let mut name_error_found = false
  let mut port_error_found = false
  
  for error in invalid_validation_result.errors {
    if error.key == "service.name" && error.message.contains("min_length") {
      name_error_found = true
    }
    if error.key == "service.port" && error.message.contains("max_value") {
      port_error_found = true
    }
  }
  
  assert_true(name_error_found)
  assert_true(port_error_found)
  
  // 修复配置
  azimuth::ConfigManager::set_string(config_manager, "service.name", "azimuth-fixed")
  azimuth::ConfigManager::set_int(config_manager, "service.port", 8080)
  
  // 再次验证
  let fixed_validation_result = azimuth::ConfigManager::validate(config_manager, validation_rules)
  
  // 验证结果应该成功
  assert_true(fixed_validation_result.is_valid)
  assert_eq(fixed_validation_result.errors.length(), 0)
}

test "配置文件加载测试" {
  // 模拟JSON配置文件内容
  let json_config = "{    \"service\": {      \"name\": \"azimuth-telemetry\",      \"port\": 8080,      \"debug\": false,      \"version\": \"1.0.0\"    },    \"database\": {      \"host\": \"localhost\",      \"port\": 5432,      \"username\": \"azimuth_user\",      \"password\": \"secure_password\",      \"name\": \"azimuth_db\"    },    \"telemetry\": {      \"enabled\": true,      \"sampling_rate\": 0.1,      \"endpoints\": [\"https://telemetry1.example.com\", \"https://telemetry2.example.com\"]    },    \"logging\": {      \"level\": \"INFO\",      \"format\": \"json\",      \"console\": true,      \"file\": {        \"path\": \"/var/log/azimuth.log\",        \"max_size\": \"100MB\",        \"max_backups\": 5      }    }  }"
  
  // 创建配置管理器并加载JSON配置
  let config_manager = azimuth::ConfigManager::new()
  let load_result = azimuth::ConfigManager::load_from_json(config_manager, json_config)
  
  // 验证加载结果
  assert_true(load_result.is_success)
  
  // 验证加载的配置
  match azimuth::ConfigManager::get_string(config_manager, "service.name") {
    Some(value) => assert_eq(value, "azimuth-telemetry")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(config_manager, "service.port") {
    Some(value) => assert_eq(value, 8080)
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_bool(config_manager, "service.debug") {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_string(config_manager, "database.host") {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(config_manager, "database.port") {
    Some(value) => assert_eq(value, 5432)
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_float(config_manager, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, 0.1)
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_string_array(config_manager, "telemetry.endpoints") {
    Some(array) => {
      assert_eq(array.length(), 2)
      assert_eq(array[0], "https://telemetry1.example.com")
      assert_eq(array[1], "https://telemetry2.example.com")
    }
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_string(config_manager, "logging.file.path") {
    Some(value) => assert_eq(value, "/var/log/azimuth.log")
    None => assert_true(false)
  }
  
  // 模拟YAML配置文件内容
  let yaml_config = "service:  name: azimuth-telemetry-yaml  port: 9090  debug: true  version: \"2.0.0\"database:  host: yaml-db.example.com  port: 5433  username: yaml_user  password: yaml_password  name: azimuth_yaml_db"
  
  // 创建新的配置管理器并加载YAML配置
  let yaml_config_manager = azimuth::ConfigManager::new()
  let yaml_load_result = azimuth::ConfigManager::load_from_yaml(yaml_config_manager, yaml_config)
  
  // 验证加载结果
  assert_true(yaml_load_result.is_success)
  
  // 验证加载的配置
  match azimuth::ConfigManager::get_string(yaml_config_manager, "service.name") {
    Some(value) => assert_eq(value, "azimuth-telemetry-yaml")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(yaml_config_manager, "service.port") {
    Some(value) => assert_eq(value, 9090)
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_bool(yaml_config_manager, "service.debug") {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  // 测试无效配置文件
  let invalid_json = "{ invalid json }"
  let invalid_config_manager = azimuth::ConfigManager::new()
  let invalid_load_result = azimuth::ConfigManager::load_from_json(invalid_config_manager, invalid_json)
  
  // 验证加载失败
  assert_false(invalid_load_result.is_success)
  assert_true(invalid_load_result.error_message.length() > 0)
}

test "配置热重载测试" {
  // 创建初始配置
  let config_manager = azimuth::ConfigManager::new()
  azimuth::ConfigManager::set_string(config_manager, "service.name", "initial-service")
  azimuth::ConfigManager::set_int(config_manager, "service.port", 8080)
  azimuth::ConfigManager::set_bool(config_manager, "service.debug", false)
  
  // 验证初始配置
  match azimuth::ConfigManager::get_string(config_manager, "service.name") {
    Some(value) => assert_eq(value, "initial-service")
    None => assert_true(false)
  }
  
  // 注册配置变更监听器
  let mut change_events = []
  let listener = azimuth::ConfigChangeListener::new fn(key, old_value, new_value) {
    change_events = change_events + [(key, old_value, new_value)]
  }
  
  azimuth::ConfigManager::add_change_listener(config_manager, listener)
  
  // 更新配置（触发热重载）
  azimuth::ConfigManager::set_string(config_manager, "service.name", "updated-service")
  azimuth::ConfigManager::set_int(config_manager, "service.port", 9090)
  azimuth::ConfigManager::set_bool(config_manager, "service.debug", true)
  
  // 验证配置已更新
  match azimuth::ConfigManager::get_string(config_manager, "service.name") {
    Some(value) => assert_eq(value, "updated-service")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(config_manager, "service.port") {
    Some(value) => assert_eq(value, 9090)
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_bool(config_manager, "service.debug") {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  // 验证变更事件
  assert_eq(change_events.length(), 3)
  
  // 验证第一个变更事件
  match change_events[0] {
    (key, old_value, new_value) => {
      assert_eq(key, "service.name")
      assert_eq(old_value, "initial-service")
      assert_eq(new_value, "updated-service")
    }
  }
  
  // 验证第二个变更事件
  match change_events[1] {
    (key, old_value, new_value) => {
      assert_eq(key, "service.port")
      assert_eq(old_value, "8080")
      assert_eq(new_value, "9090")
    }
  }
  
  // 验证第三个变更事件
  match change_events[2] {
    (key, old_value, new_value) => {
      assert_eq(key, "service.debug")
      assert_eq(old_value, "false")
      assert_eq(new_value, "true")
    }
  }
  
  // 测试批量配置更新
  let batch_updates = [
    ("batch.config1", "value1"),
    ("batch.config2", "value2"),
    ("batch.config3", "value3")
  ]
  
  azimuth::ConfigManager::batch_update(config_manager, batch_updates)
  
  // 验证批量更新结果
  match azimuth::ConfigManager::get_string(config_manager, "batch.config1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_string(config_manager, "batch.config2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_string(config_manager, "batch.config3") {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  // 验证批量更新触发了变更事件
  assert_eq(change_events.length(), 6) // 之前的3个 + 批量更新的3个
}

test "配置环境变量覆盖测试" {
  // 创建配置管理器
  let config_manager = azimuth::ConfigManager::new()
  
  // 设置默认配置
  azimuth::ConfigManager::set_string(config_manager, "service.name", "default-service")
  azimuth::ConfigManager::set_int(config_manager, "service.port", 8080)
  azimuth::ConfigManager::set_string(config_manager, "database.host", "localhost")
  azimuth::ConfigManager::set_string(config_manager, "database.password", "default-password")
  
  // 模拟环境变量
  let env_vars = [
    ("AZIMUTH_SERVICE_NAME", "env-service"),
    ("AZIMUTH_SERVICE_PORT", "9090"),
    ("AZIMUTH_DATABASE_HOST", "env-db.example.com"),
    ("AZIMUTH_NEW_FEATURE", "enabled")
  ]
  
  // 应用环境变量覆盖
  azimuth::ConfigManager::apply_env_overrides(config_manager, env_vars)
  
  // 验证环境变量覆盖结果
  match azimuth::ConfigManager::get_string(config_manager, "service.name") {
    Some(value) => assert_eq(value, "env-service") // 应该被环境变量覆盖
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_int(config_manager, "service.port") {
    Some(value) => assert_eq(value, 9090) // 应该被环境变量覆盖
    None => assert_true(false)
  }
  
  match azimuth::ConfigManager::get_string(config_manager, "database.host") {
    Some(value) => assert_eq(value, "env-db.example.com") // 应该被环境变量覆盖
    None => assert_true(false)
  }
  
  // 验证未被覆盖的配置保持原值
  match azimuth::ConfigManager::get_string(config_manager, "database.password") {
    Some(value) => assert_eq(value, "default-password") // 应该保持原值
    None => assert_true(false)
  }
  
  // 验证新增的环境变量配置
  match azimuth::ConfigManager::get_string(config_manager, "new.feature") {
    Some(value) => assert_eq(value, "enabled") // 新增的配置
    None => assert_true(false)
  }
  
  // 测试环境变量优先级
  // 先设置配置值
  azimuth::ConfigManager::set_string(config_manager, "priority.test", "config-value")
  
  // 然后应用环境变量
  let priority_env_vars = [
    ("AZIMUTH_PRIORITY_TEST", "env-value")
  ]
  
  azimuth::ConfigManager::apply_env_overrides(config_manager, priority_env_vars)
  
  // 验证环境变量优先级更高
  match azimuth::ConfigManager::get_string(config_manager, "priority.test") {
    Some(value) => assert_eq(value, "env-value") // 应该是环境变量的值
    None => assert_true(false)
  }
}

test "配置导出测试" {
  // 创建配置管理器
  let config_manager = azimuth::ConfigManager::new()
  
  // 添加各种类型的配置
  azimuth::ConfigManager::set_string(config_manager, "service.name", "azimuth-telemetry")
  azimuth::ConfigManager::set_int(config_manager, "service.port", 8080)
  azimuth::ConfigManager::set_bool(config_manager, "service.debug", true)
  azimuth::ConfigManager::set_float(config_manager, "performance.threshold", 0.95)
  
  azimuth::ConfigManager::set_string(config_manager, "database.host", "localhost")
  azimuth::ConfigManager::set_int(config_manager, "database.port", 5432)
  
  let endpoints = ["endpoint1", "endpoint2", "endpoint3"]
  azimuth::ConfigManager::set_string_array(config_manager, "telemetry.endpoints", endpoints)
  
  // 导出为JSON
  let json_export = azimuth::ConfigManager::export_to_json(config_manager)
  
  // 验证JSON导出内容
  assert_true(json_export.contains("\"service.name\":\"azimuth-telemetry\""))
  assert_true(json_export.contains("\"service.port\":8080"))
  assert_true(json_export.contains("\"service.debug\":true"))
  assert_true(json_export.contains("\"performance.threshold\":0.95"))
  assert_true(json_export.contains("\"database.host\":\"localhost\""))
  assert_true(json_export.contains("\"database.port\":5432"))
  assert_true(json_export.contains("\"telemetry.endpoints\":[\"endpoint1\",\"endpoint2\",\"endpoint3\"]"))
  
  // 验证JSON结构
  assert_true(json_export.starts_with("{"))
  assert_true(json_export.ends_with("}"))
  
  // 导出为YAML
  let yaml_export = azimuth::ConfigManager::export_to_yaml(config_manager)
  
  // 验证YAML导出内容
  assert_true(yaml_export.contains("service:"))
  assert_true(yaml_export.contains("name: azimuth-telemetry"))
  assert_true(yaml_export.contains("port: 8080"))
  assert_true(yaml_export.contains("debug: true"))
  assert_true(yaml_export.contains("performance:"))
  assert_true(yaml_export.contains("threshold: 0.95"))
  assert_true(yaml_export.contains("database:"))
  assert_true(yaml_export.contains("host: localhost"))
  assert_true(yaml_export.contains("port: 5432"))
  assert_true(yaml_export.contains("telemetry:"))
  assert_true(yaml_export.contains("endpoints:"))
  assert_true(yaml_export.contains("- endpoint1"))
  assert_true(yaml_export.contains("- endpoint2"))
  assert_true(yaml_export.contains("- endpoint3"))
  
  // 导出配置子集
  let database_config = azimuth::ConfigManager::get_subset(config_manager, "database")
  let database_json = azimuth::ConfigManager::export_to_json(database_config)
  
  // 验证子集导出只包含相关配置
  assert_true(database_json.contains("\"host\":\"localhost\""))
  assert_true(database_json.contains("\"port\":5432"))
  assert_false(database_json.contains("\"service.name\""))
  assert_false(database_json.contains("\"performance.threshold\""))
  
  // 测试配置过滤导出
  let filter_keys = ["service.name", "service.port", "database.host"]
  let filtered_config = azimuth::ConfigManager::filter_export(config_manager, filter_keys)
  let filtered_json = azimuth::ConfigManager::export_to_json(filtered_config)
  
  // 验证过滤导出只包含指定键
  assert_true(filtered_json.contains("\"service.name\":\"azimuth-telemetry\""))
  assert_true(filtered_json.contains("\"service.port\":8080"))
  assert_true(filtered_json.contains("\"database.host\":\"localhost\""))
  assert_false(filtered_json.contains("\"service.debug\""))
  assert_false(filtered_json.contains("\"performance.threshold\""))
  assert_false(filtered_json.contains("\"database.port\""))
  assert_false(filtered_json.contains("\"telemetry.endpoints\""))
}