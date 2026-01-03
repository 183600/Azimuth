// Azimuth Configuration Validation and Defaults Tests
// 配置验证和默认值测试用例 - 专注于配置验证、默认值管理和动态配置更新

// Test 1: 基础配置验证测试
test "basic configuration validation" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 定义配置模式
  let schema = ConfigurationSchema::new()
  ConfigurationSchema::add_field(schema, "telemetry.enabled", BoolType, Some(true))
  ConfigurationSchema::add_field(schema, "telemetry.sampling_rate", FloatType, Some(0.1))
  ConfigurationSchema::add_field(schema, "telemetry.batch_size", IntType, Some(100))
  ConfigurationSchema::add_field(schema, "telemetry.service_name", StringType, None)
  ConfigurationSchema::add_field(schema, "telemetry.endpoints", ArrayType, Some([]))
  
  ConfigurationManager::set_schema(config_manager, schema)
  
  // 测试有效配置
  let valid_config = [
    ("telemetry.enabled", BoolValue(true)),
    ("telemetry.sampling_rate", FloatValue(0.2)),
    ("telemetry.batch_size", IntValue(200)),
    ("telemetry.service_name", StringValue("test-service")),
    ("telemetry.endpoints", ArrayStringValue(["http://endpoint1", "http://endpoint2"]))
  ]
  
  let validation_result = ConfigurationManager::validate(config_manager, valid_config)
  match validation_result {
    Ok(_) => assert_true(true)
    Err(errors) => assert_true(false, "Valid config should pass validation: " + errors.join(", "))
  }
  
  // 测试无效配置
  let invalid_config = [
    ("telemetry.enabled", StringValue("true")), // 错误类型
    ("telemetry.sampling_rate", FloatValue(1.5)), // 超出范围
    ("telemetry.batch_size", IntValue(-10)), // 负值
    ("unknown.field", StringValue("value")) // 未知字段
  ]
  
  let invalid_result = ConfigurationManager::validate(config_manager, invalid_config)
  match invalid_result {
    Ok(_) => assert_true(false, "Invalid config should fail validation")
    Err(errors) => {
      assert_true(errors.length() >= 3) // 至少3个错误
      assert_true(errors[0].contains("telemetry.enabled"))
      assert_true(errors[1].contains("telemetry.sampling_rate"))
      assert_true(errors[2].contains("telemetry.batch_size"))
    }
  }
}

// Test 2: 默认值管理测试
test "default values management" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 定义带默认值的配置模式
  let schema = ConfigurationSchema::new()
  ConfigurationSchema::add_field(schema, "service.name", StringType, Some("default-service"))
  ConfigurationSchema::add_field(schema, "service.version", StringType, Some("1.0.0"))
  ConfigurationSchema::add_field(schema, "service.port", IntType, Some(8080))
  ConfigurationSchema::add_field(schema, "log.level", StringType, Some("INFO"))
  ConfigurationSchema::add_field(schema, "log.format", StringType, Some("json"))
  
  ConfigurationManager::set_schema(config_manager, schema)
  
  // 应用默认配置
  ConfigurationManager::apply_defaults(config_manager)
  
  // 验证默认值
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "default-service")
    None => assert_true(false)
  }
  
  let service_version = ConfigurationManager::get_string(config_manager, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  
  let service_port = ConfigurationManager::get_int(config_manager, "service.port")
  match service_port {
    Some(port) => assert_eq(port, 8080)
    None => assert_true(false)
  }
  
  // 测试部分覆盖
  let partial_config = [
    ("service.name", StringValue("custom-service")),
    ("log.level", StringValue("DEBUG"))
  ]
  
  ConfigurationManager::update(config_manager, partial_config)
  
  // 验证部分覆盖
  let updated_name = ConfigurationManager::get_string(config_manager, "service.name")
  match updated_name {
    Some(name) => assert_eq(name, "custom-service") // 已覆盖
    None => assert_true(false)
  }
  
  let unchanged_version = ConfigurationManager::get_string(config_manager, "service.version")
  match unchanged_version {
    Some(version) => assert_eq(version, "1.0.0") // 未覆盖，仍为默认值
    None => assert_true(false)
  }
  
  let updated_log_level = ConfigurationManager::get_string(config_manager, "log.level")
  match updated_log_level {
    Some(level) => assert_eq(level, "DEBUG") // 已覆盖
    None => assert_true(false)
  }
  
  let unchanged_log_format = ConfigurationManager::get_string(config_manager, "log.format")
  match unchanged_log_format {
    Some(format) => assert_eq(format, "json") // 未覆盖，仍为默认值
    None => assert_true(false)
  }
}

// Test 3: 动态配置更新测试
test "dynamic configuration updates" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 定义配置模式
  let schema = ConfigurationSchema::new()
  ConfigurationSchema::add_field(schema, "cache.max_size", IntType, Some(1000))
  ConfigurationSchema::add_field(schema, "cache.ttl_seconds", IntType, Some(300))
  ConfigurationSchema::add_field(schema, "thread.pool_size", IntType, Some(10))
  ConfigurationSchema::add_field(schema, "feature.flags", MapType, Some([]))
  
  ConfigurationManager::set_schema(config_manager, schema)
  
  // 应用初始配置
  ConfigurationManager::apply_defaults(config_manager)
  
  // 创建配置变更监听器
  let mut change_events = []
  
  let listener = ConfigurationListener::new(fn(key: String, old_value: AttributeValue, new_value: AttributeValue) {
    change_events.push((key, old_value, new_value))
  })
  
  ConfigurationManager::add_listener(config_manager, listener)
  
  // 测试单个配置更新
  ConfigurationManager::set(config_manager, "cache.max_size", IntValue(2000))
  
  let updated_max_size = ConfigurationManager::get_int(config_manager, "cache.max_size")
  match updated_max_size {
    Some(size) => assert_eq(size, 2000)
    None => assert_true(false)
  }
  
  // 验证变更事件
  assert_eq(change_events.length(), 1)
  assert_eq(change_events[0].0, "cache.max_size")
  match change_events[0].1 {
    IntValue(old_val) => assert_eq(old_val, 1000)
    _ => assert_true(false)
  }
  match change_events[0].2 {
    IntValue(new_val) => assert_eq(new_val, 2000)
    _ => assert_true(false)
  }
  
  // 测试批量配置更新
  let batch_updates = [
    ("cache.ttl_seconds", IntValue(600)),
    ("thread.pool_size", IntValue(20))
  ]
  
  ConfigurationManager::update(config_manager, batch_updates)
  
  // 验证批量更新
  let updated_ttl = ConfigurationManager::get_int(config_manager, "cache.ttl_seconds")
  match updated_ttl {
    Some(ttl) => assert_eq(ttl, 600)
    None => assert_true(false)
  }
  
  let updated_pool_size = ConfigurationManager::get_int(config_manager, "thread.pool_size")
  match updated_pool_size {
    Some(size) => assert_eq(size, 20)
    None => assert_true(false)
  }
  
  // 验证批量变更事件
  assert_eq(change_events.length(), 3) // 1个单更新 + 2个批量更新
  
  // 测试条件更新
  let condition_updated = ConfigurationManager::set_if(
    config_manager,
    "cache.max_size",
    IntValue(5000),
    fn(current_value: AttributeValue) -> Bool {
      match current_value {
        IntValue(val) => val < 3000 // 只有当前值小于3000时才更新
        _ => false
      }
    }
  )
  
  assert_true(condition_updated) // 应该更新成功
  
  let conditionally_updated = ConfigurationManager::get_int(config_manager, "cache.max_size")
  match conditionally_updated {
    Some(size) => assert_eq(size, 5000)
    None => assert_true(false)
  }
  
  // 再次尝试条件更新（应该失败，因为当前值已经不满足条件）
  let second_condition_updated = ConfigurationManager::set_if(
    config_manager,
    "cache.max_size",
    IntValue(8000),
    fn(current_value: AttributeValue) -> Bool {
      match current_value {
        IntValue(val) => val < 3000
        _ => false
      }
    }
  )
  
  assert_false(second_condition_updated) // 应该更新失败
  
  let unchanged_value = ConfigurationManager::get_int(config_manager, "cache.max_size")
  match unchanged_value {
    Some(size) => assert_eq(size, 5000) // 值应该保持不变
    None => assert_true(false)
  }
}

// Test 4: 配置文件加载和保存测试
test "configuration file loading and saving" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 定义配置模式
  let schema = ConfigurationSchema::new()
  ConfigurationSchema::add_field(schema, "app.name", StringType, None)
  ConfigurationSchema::add_field(schema, "app.version", StringType, None)
  ConfigurationSchema::add_field(schema, "app.debug", BoolType, Some(false))
  ConfigurationSchema::add_field(schema, "database.host", StringType, None)
  ConfigurationSchema::add_field(schema, "database.port", IntType, Some(5432))
  ConfigurationSchema::add_field(schema, "database.username", StringType, None)
  ConfigurationSchema::add_field(schema, "database.password", StringType, None)
  
  ConfigurationManager::set_schema(config_manager, schema)
  
  // 创建测试配置文件
  let config_file_path = "/tmp/test_config.json"
  let config_content = {
    "app": {
      "name": "test-application",
      "version": "2.1.0",
      "debug": true
    },
    "database": {
      "host": "localhost",
      "port": 3306,
      "username": "testuser",
      "password": "testpass"
    }
  }
  
  // 写入配置文件
  File::write(config_file_path, config_content.to_json())
  
  // 加载配置文件
  let load_result = ConfigurationManager::load_from_file(config_manager, config_file_path)
  match load_result {
    Ok(_) => assert_true(true)
    Err(error) => assert_true(false, "Failed to load config: " + error)
  }
  
  // 验证加载的配置
  let app_name = ConfigurationManager::get_string(config_manager, "app.name")
  match app_name {
    Some(name) => assert_eq(name, "test-application")
    None => assert_true(false)
  }
  
  let app_version = ConfigurationManager::get_string(config_manager, "app.version")
  match app_version {
    Some(version) => assert_eq(version, "2.1.0")
    None => assert_true(false)
  }
  
  let app_debug = ConfigurationManager::get_bool(config_manager, "app.debug")
  match app_debug {
    Some(debug) => assert_true(debug)
    None => assert_true(false)
  }
  
  let db_host = ConfigurationManager::get_string(config_manager, "database.host")
  match db_host {
    Some(host) => assert_eq(host, "localhost")
    None => assert_true(false)
  }
  
  let db_port = ConfigurationManager::get_int(config_manager, "database.port")
  match db_port {
    Some(port) => assert_eq(port, 3306)
    None => assert_true(false)
  }
  
  // 修改配置
  ConfigurationManager::set(config_manager, "app.version", StringValue("2.2.0"))
  ConfigurationManager::set(config_manager, "database.port", IntValue(5432))
  
  // 保存配置到新文件
  let save_file_path = "/tmp/saved_config.json"
  let save_result = ConfigurationManager::save_to_file(config_manager, save_file_path)
  match save_result {
    Ok(_) => assert_true(true)
    Err(error) => assert_true(false, "Failed to save config: " + error)
  }
  
  // 验证保存的文件
  let saved_content = File::read(save_file_path)
  let saved_config = Json::parse(saved_content)
  
  assert_eq(saved_config["app"]["version"], "2.2.0")
  assert_eq(saved_config["database"]["port"], 5432)
  
  // 测试环境变量覆盖
  Environment::set("APP_NAME", "env-override-app")
  Environment::set("DATABASE_PORT", "9999")
  
  // 加载配置并应用环境变量覆盖
  let env_config_manager = ConfigurationManager::new()
  ConfigurationManager::set_schema(env_config_manager, schema)
  
  let env_load_result = ConfigurationManager::load_from_file_with_env_override(
    env_config_manager,
    config_file_path,
    "APP_" // 前缀
  )
  
  match env_load_result {
    Ok(_) => assert_true(true)
    Err(error) => assert_true(false, "Failed to load config with env override: " + error)
  }
  
  // 验证环境变量覆盖
  let env_app_name = ConfigurationManager::get_string(env_config_manager, "app.name")
  match env_app_name {
    Some(name) => assert_eq(name, "env-override-app") // 被环境变量覆盖
    None => assert_true(false)
  }
  
  let env_db_port = ConfigurationManager::get_int(env_config_manager, "database.port")
  match env_db_port {
    Some(port) => assert_eq(port, 9999) // 被环境变量覆盖
    None => assert_true(false)
  }
}

// Test 5: 配置依赖和验证规则测试
test "configuration dependencies and validation rules" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 定义带依赖的配置模式
  let schema = ConfigurationSchema::new()
  ConfigurationSchema::add_field(schema, "auth.enabled", BoolType, Some(false))
  ConfigurationSchema::add_field(schema, "auth.provider", StringType, None)
  ConfigurationSchema::add_field(schema, "auth.token_expiry", IntType, Some(3600))
  ConfigurationSchema::add_field(schema, "database.ssl_enabled", BoolType, Some(false))
  ConfigurationSchema::add_field(schema, "database.ssl_cert_path", StringType, None)
  
  // 添加依赖规则
  ConfigurationSchema::add_dependency(schema, "auth.provider", ["auth.enabled"], fn(values: Map[String, AttributeValue]) -> Bool {
    match values.get("auth.enabled") {
      Some(BoolValue(enabled)) => enabled == false || values.contains("auth.provider")
      _ => false
    }
  }, "auth.provider is required when auth.enabled is true")
  
  // 添加验证规则
  ConfigurationSchema::add_validation_rule(schema, "auth.token_expiry", fn(value: AttributeValue) -> Bool {
    match value {
      IntValue(expiry) => expiry > 0 && expiry <= 86400 // 1秒到24小时
      _ => false
    }
  }, "auth.token_expiry must be between 1 and 86400 seconds")
  
  ConfigurationSchema::add_validation_rule(schema, "database.ssl_cert_path", fn(value: AttributeValue) -> Bool {
    match value {
      StringValue(path) => path.length() > 0
      _ => false
    }
  }, "database.ssl_cert_path cannot be empty")
  
  ConfigurationManager::set_schema(config_manager, schema)
  
  // 测试有效配置
  let valid_config = [
    ("auth.enabled", BoolValue(true)),
    ("auth.provider", StringValue("oauth")),
    ("auth.token_expiry", IntValue(7200)),
    ("database.ssl_enabled", BoolValue(false))
  ]
  
  let valid_result = ConfigurationManager::validate(config_manager, valid_config)
  match valid_result {
    Ok(_) => assert_true(true)
    Err(errors) => assert_true(false, "Valid config should pass: " + errors.join(", "))
  }
  
  // 测试违反依赖规则的配置
  let dependency_violation_config = [
    ("auth.enabled", BoolValue(true))
    // 缺少 auth.provider
  ]
  
  let dependency_result = ConfigurationManager::validate(config_manager, dependency_violation_config)
  match dependency_result {
    Ok(_) => assert_true(false, "Config with dependency violation should fail")
    Err(errors) => {
      assert_true(errors.length() >= 1)
      assert_true(errors[0].contains("auth.provider"))
      assert_true(errors[0].contains("required"))
    }
  }
  
  // 测试违反验证规则的配置
  let validation_violation_config = [
    ("auth.enabled", BoolValue(false)),
    ("auth.token_expiry", IntValue(100000)), // 超过24小时
    ("database.ssl_enabled", BoolValue(true)),
    ("database.ssl_cert_path", StringValue("")) // 空路径
  ]
  
  let validation_result = ConfigurationManager::validate(config_manager, validation_violation_config)
  match validation_result {
    Ok(_) => assert_true(false, "Config with validation violation should fail")
    Err(errors) => {
      assert_true(errors.length() >= 2)
      assert_true(errors[0].contains("auth.token_expiry") || errors[1].contains("auth.token_expiry"))
      assert_true(errors[0].contains("database.ssl_cert_path") || errors[1].contains("database.ssl_cert_path"))
    }
  }
}

// Test 6: 配置热重载测试
test "configuration hot reload" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 定义配置模式
  let schema = ConfigurationSchema::new()
  ConfigurationSchema::add_field(schema, "feature.toggle_a", BoolType, Some(false))
  ConfigurationSchema::add_field(schema, "feature.toggle_b", BoolType, Some(false))
  ConfigurationSchema::add_field(schema, "feature.toggle_c", BoolType, Some(false))
  ConfigurationSchema::add_field(schema, "threshold.value", IntType, Some(100))
  
  ConfigurationManager::set_schema(config_manager, schema)
  
  // 创建初始配置文件
  let config_file_path = "/tmp/hot_reload_config.json"
  let initial_config = {
    "feature": {
      "toggle_a": true,
      "toggle_b": false,
      "toggle_c": true
    },
    "threshold": {
      "value": 200
    }
  }
  
  File::write(config_file_path, initial_config.to_json())
  
  // 加载初始配置
  ConfigurationManager::load_from_file(config_manager, config_file_path)
  
  // 验证初始配置
  let initial_toggle_a = ConfigurationManager::get_bool(config_manager, "feature.toggle_a")
  match initial_toggle_a {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let initial_threshold = ConfigurationManager::get_int(config_manager, "threshold.value")
  match initial_threshold {
    Some(value) => assert_eq(value, 200)
    None => assert_true(false)
  }
  
  // 创建热重载监听器
  let mut reload_events = []
  
  let reload_listener = ConfigurationReloadListener::new(fn(changes: Array[String]) {
    reload_events = reload_events.concat(changes)
  })
  
  ConfigurationManager::add_reload_listener(config_manager, reload_listener)
  
  // 启用热重载
  ConfigurationManager::enable_hot_reload(config_manager, config_file_path)
  
  // 修改配置文件
  let updated_config = {
    "feature": {
      "toggle_a": false, // 改变
      "toggle_b": true,  // 改变
      "toggle_c": true   // 不变
    },
    "threshold": {
      "value": 300 // 改变
    }
  }
  
  File::write(config_file_path, updated_config.to_json())
  
  // 等待热重载触发
  Time::sleep(1000)
  
  // 验证配置已更新
  let updated_toggle_a = ConfigurationManager::get_bool(config_manager, "feature.toggle_a")
  match updated_toggle_a {
    Some(value) => assert_false(value) // 应该已更新
    None => assert_true(false)
  }
  
  let updated_toggle_b = ConfigurationManager::get_bool(config_manager, "feature.toggle_b")
  match updated_toggle_b {
    Some(value) => assert_true(value) // 应该已更新
    None => assert_true(false)
  }
  
  let updated_threshold = ConfigurationManager::get_int(config_manager, "threshold.value")
  match updated_threshold {
    Some(value) => assert_eq(value, 300) // 应该已更新
    None => assert_true(false)
  }
  
  // 验证重载事件
  assert_true(reload_events.length() >= 3)
  assert_true(reload_events.contains("feature.toggle_a"))
  assert_true(reload_events.contains("feature.toggle_b"))
  assert_true(reload_events.contains("threshold.value"))
  
  // 禁用热重载
  ConfigurationManager::disable_hot_reload(config_manager)
  
  // 再次修改配置文件
  let final_config = {
    "feature": {
      "toggle_a": true,
      "toggle_b": false,
      "toggle_c": false
    },
    "threshold": {
      "value": 400
    }
  }
  
  File::write(config_file_path, final_config.to_json())
  
  // 等待一段时间
  Time::sleep(1000)
  
  // 验证配置未更新（热重载已禁用）
  let final_toggle_a = ConfigurationManager::get_bool(config_manager, "feature.toggle_a")
  match final_toggle_a {
    Some(value) => assert_false(value) // 应该保持之前的状态
    None => assert_true(false)
  }
}

// Test 7: 配置加密和安全测试
test "configuration encryption and security" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 定义配置模式
  let schema = ConfigurationSchema::new()
  ConfigurationSchema::add_field(schema, "api.key", StringType, None)
  ConfigurationSchema::add_field(schema, "database.password", StringType, None)
  ConfigurationSchema::add_field(schema, "jwt.secret", StringType, None)
  ConfigurationSchema::add_field(schema, "public.setting", StringType, Some("default"))
  
  // 标记敏感字段
  ConfigurationSchema::mark_sensitive(schema, "api.key")
  ConfigurationSchema::mark_sensitive(schema, "database.password")
  ConfigurationSchema::mark_sensitive(schema, "jwt.secret")
  
  ConfigurationManager::set_schema(config_manager, schema)
  
  // 设置加密密钥
  let encryption_key = "test-encryption-key-32-bytes-long"
  ConfigurationManager::set_encryption_key(config_manager, encryption_key)
  
  // 设置敏感配置
  ConfigurationManager::set(config_manager, "api.key", StringValue("secret-api-key-123"))
  ConfigurationManager::set(config_manager, "database.password", StringValue("db-password-456"))
  ConfigurationManager::set(config_manager, "jwt.secret", StringValue("jwt-secret-789"))
  ConfigurationManager::set(config_manager, "public.setting", StringValue("public-value"))
  
  // 验证敏感值被加密存储
  let raw_api_key = ConfigurationManager::get_raw(config_manager, "api.key")
  assert_true(raw_api_key !== "secret-api-key-123") // 应该是加密值
  assert_true(raw_api_key.length() > 0)
  
  // 验证解密访问
  let decrypted_api_key = ConfigurationManager::get_string(config_manager, "api.key")
  match decrypted_api_key {
    Some(key) => assert_eq(key, "secret-api-key-123") // 应该正确解密
    None => assert_true(false)
  }
  
  // 验证公共值不被加密
  let raw_public_setting = ConfigurationManager::get_raw(config_manager, "public.setting")
  assert_eq(raw_public_setting, "public-value") // 应该是明文
  
  // 保存配置到文件
  let encrypted_config_path = "/tmp/encrypted_config.json"
  ConfigurationManager::save_to_file(config_manager, encrypted_config_path)
  
  // 验证文件中的敏感值被加密
  let file_content = File::read(encrypted_config_path)
  assert_false(file_content.contains("secret-api-key-123"))
  assert_false(file_content.contains("db-password-456"))
  assert_false(file_content.contains("jwt-secret-789"))
  assert_true(file_content.contains("public-value")) // 公共值应该是明文
  
  // 创建新的配置管理器并加载加密配置
  let new_config_manager = ConfigurationManager::new()
  ConfigurationManager::set_schema(new_config_manager, schema)
  ConfigurationManager::set_encryption_key(new_config_manager, encryption_key)
  
  ConfigurationManager::load_from_file(new_config_manager, encrypted_config_path)
  
  // 验证加密配置能正确加载和解密
  let loaded_api_key = ConfigurationManager::get_string(new_config_manager, "api.key")
  match loaded_api_key {
    Some(key) => assert_eq(key, "secret-api-key-123")
    None => assert_true(false)
  }
  
  // 测试错误密钥无法解密
  let wrong_key_manager = ConfigurationManager::new()
  ConfigurationManager::set_schema(wrong_key_manager, schema)
  ConfigurationManager::set_encryption_key(wrong_key_manager, "wrong-encryption-key-32-bytes-long")
  
  let wrong_key_result = ConfigurationManager::load_from_file(wrong_key_manager, encrypted_config_path)
  match wrong_key_result {
    Ok(_) => assert_true(false, "Should fail with wrong encryption key")
    Err(error) => assert_true(error.contains("decrypt") || error.contains("encryption"))
  }
  
  // 测试配置脱敏
  let sanitized_config = ConfigurationManager::get_sanitized_config(config_manager)
  
  match sanitized_config.get("api.key") {
    Some(StringValue(value)) => assert_eq(value, "***")
    _ => assert_true(false)
  }
  
  match sanitized_config.get("database.password") {
    Some(StringValue(value)) => assert_eq(value, "***")
    _ => assert_true(false)
  }
  
  match sanitized_config.get("public.setting") {
    Some(StringValue(value)) => assert_eq(value, "public-value") // 公共值不脱敏
    _ => assert_true(false)
  }
}

// Test 8: 配置性能和内存测试
test "configuration performance and memory" {
  // 创建大型配置模式
  let large_schema = ConfigurationSchema::new()
  
  // 添加大量字段
  for i in 0..<1000 {
    let field_name = "field_" + i.to_string()
    let default_value = "default_" + i.to_string()
    ConfigurationSchema::add_field(large_schema, field_name, StringType, Some(default_value))
  }
  
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  ConfigurationManager::set_schema(config_manager, large_schema)
  
  // 测试大规模配置加载性能
  let start_time = Time::now()
  
  ConfigurationManager::apply_defaults(config_manager)
  
  let load_time = Time::now() - start_time
  assert_true(load_time < 5000) // 应该在5秒内完成
  
  // 测试大规模配置更新性能
  let update_start = Time::now()
  
  let large_updates = []
  for i in 0..<500 {
    let field_name = "field_" + i.to_string()
    let value = "updated_" + i.to_string()
    large_updates = large_updates.push((field_name, StringValue(value)))
  }
  
  ConfigurationManager::update(config_manager, large_updates)
  
  let update_time = Time::now() - update_start
  assert_true(update_time < 3000) // 应该在3秒内完成
  
  // 测试大规模配置访问性能
  let access_start = Time::now()
  
  for i in 0..<1000 {
    let field_name = "field_" + i.to_string()
    ConfigurationManager::get_string(config_manager, field_name)
  }
  
  let access_time = Time::now() - access_start
  assert_true(access_time < 1000) // 应该在1秒内完成
  
  // 测试内存使用
  let initial_memory = MemoryMonitor::current_usage()
  
  // 创建多个配置管理器实例
  let config_managers = []
  for i in 0..<10 {
    let manager = ConfigurationManager::new()
    ConfigurationManager::set_schema(manager, large_schema)
    ConfigurationManager::apply_defaults(manager)
    config_managers = config_managers.push(manager)
  }
  
  let after_creation_memory = MemoryMonitor::current_usage()
  let memory_increase = after_creation_memory.heap_size - initial_memory.heap_size
  
  // 验证内存使用合理
  assert_true(memory_increase < 50 * 1024 * 1024) // 小于50MB
  
  // 清理配置管理器
  for manager in config_managers {
    ConfigurationManager::cleanup(manager)
  }
  
  // 强制垃圾回收
  MemoryMonitor::force_gc()
  
  let after_cleanup_memory = MemoryMonitor::current_usage()
  let memory_after_cleanup = after_cleanup_memory.heap_size - initial_memory.heap_size
  
  // 验证内存释放
  assert_true(memory_after_cleanup < memory_increase / 2) // 至少释放一半内存
  
  // 获取配置指标
  let metrics = ConfigurationManager::get_metrics(config_manager)
  
  // 验证指标
  assert_eq(metrics.total_fields, 1000)
  assert_eq(metrics.loaded_fields, 1000)
  assert_true(metrics.access_count > 0)
  assert_true(metrics.update_count > 0)
  assert_true(metrics.average_access_time > 0)
  assert_true(metrics.average_update_time > 0)
}