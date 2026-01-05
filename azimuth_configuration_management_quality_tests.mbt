// Azimuth Telemetry System - Configuration Management Quality Tests
// This file contains comprehensive test cases for configuration management functionality

// Test 1: Basic Configuration Operations
test "basic configuration operations" {
  let config_manager = ConfigurationManager::new()
  
  // Test configuration loading from file
  let load_result = ConfigurationManager::load_from_file(config_manager, "test_config.json")
  match load_result {
    Ok(_) => assert_true(true),
    Error(_) => {
      // Create a test config file if it doesn't exist
      let test_config = TestConfig::new()
      TestConfig::set_string(test_config, "service.name", "telemetry-service")
      TestConfig::set_int(test_config, "service.port", 8080)
      TestConfig::set_bool(test_config, "service.debug", true)
      TestConfig::set_float(test_config, "service.timeout", 30.5)
      
      let save_result = TestConfig::save_to_file(test_config, "test_config.json")
      assert_true(save_result)
      
      // Try loading again
      let retry_result = ConfigurationManager::load_from_file(config_manager, "test_config.json")
      match retry_result {
        Ok(_) => assert_true(true),
        Error(_) => assert_true(false) // Should load after creating file
      }
    }
  }
  
  // Test getting configuration values
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "telemetry-service"),
    None => assert_true(false) // Should have value
  }
  
  let service_port = ConfigurationManager::get_int(config_manager, "service.port")
  match service_port {
    Some(port) => assert_eq(port, 8080),
    None => assert_true(false) // Should have value
  }
  
  let service_debug = ConfigurationManager::get_bool(config_manager, "service.debug")
  match service_debug {
    Some(debug) => assert_true(debug),
    None => assert_true(false) // Should have value
  }
  
  let service_timeout = ConfigurationManager::get_float(config_manager, "service.timeout")
  match service_timeout {
    Some(timeout) => assert_eq(timeout, 30.5),
    None => assert_true(false) // Should have value
  }
  
  // Test setting configuration values
  ConfigurationManager::set_string(config_manager, "service.version", "1.0.0")
  ConfigurationManager::set_int(config_manager, "service.max_connections", 100)
  ConfigurationManager::set_bool(config_manager, "service.ssl_enabled", false)
  ConfigurationManager::set_float(config_manager, "service.keep_alive", 60.0)
  
  // Verify new values
  let service_version = ConfigurationManager::get_string(config_manager, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0"),
    None => assert_true(false) // Should have value
  }
  
  let max_connections = ConfigurationManager::get_int(config_manager, "service.max_connections")
  match max_connections {
    Some(connections) => assert_eq(connections, 100),
    None => assert_true(false) // Should have value
  }
  
  let ssl_enabled = ConfigurationManager::get_bool(config_manager, "service.ssl_enabled")
  match ssl_enabled {
    Some(ssl) => assert_false(ssl),
    None => assert_true(false) // Should have value
  }
  
  let keep_alive = ConfigurationManager::get_float(config_manager, "service.keep_alive")
  match keep_alive {
    Some(keepalive) => assert_eq(keepalive, 60.0),
    None => assert_true(false) // Should have value
  }
  
  // Test configuration saving
  let save_result = ConfigurationManager::save_to_file(config_manager, "updated_config.json")
  assert_true(save_result)
  
  // Test configuration removal
  ConfigurationManager::remove(config_manager, "service.debug")
  
  let removed_debug = ConfigurationManager::get_bool(config_manager, "service.debug")
  match removed_debug {
    Some(_) => assert_true(false), // Should not have value
    None => assert_true(true)
  }
  
  // Test configuration clearing
  ConfigurationManager::clear(config_manager)
  
  let cleared_name = ConfigurationManager::get_string(config_manager, "service.name")
  match cleared_name {
    Some(_) => assert_true(false), // Should not have value
    None => assert_true(true)
  }
}

// Test 2: Configuration Validation
test "configuration validation" {
  let config_manager = ConfigurationManager::new()
  
  // Create validation schema
  let schema = ConfigurationSchema::new()
  
  // Add field validations
  ConfigurationSchema::add_string_field(schema, "service.name", true, None, None) // Required
  ConfigurationSchema::add_int_field(schema, "service.port", true, Some(1), Some(65535)) // Required, range 1-65535
  ConfigurationSchema::add_bool_field(schema, "service.debug", false, None, None) // Optional
  ConfigurationSchema::add_float_field(schema, "service.timeout", true, Some(0.1), Some(300.0)) // Required, range 0.1-300.0
  
  // Set schema
  ConfigurationManager::set_validation_schema(config_manager, schema)
  
  // Test valid configuration
  ConfigurationManager::set_string(config_manager, "service.name", "telemetry-service")
  ConfigurationManager::set_int(config_manager, "service.port", 8080)
  ConfigurationManager::set_bool(config_manager, "service.debug", true)
  ConfigurationManager::set_float(config_manager, "service.timeout", 30.5)
  
  let valid_result = ConfigurationManager::validate(config_manager)
  match valid_result {
    Ok(_) => assert_true(true),
    Error(errors) => assert_true(false) // Should be valid
  }
  
  // Test missing required field
  ConfigurationManager::remove(config_manager, "service.name")
  
  let missing_result = ConfigurationManager::validate(config_manager)
  match missing_result {
    Ok(_) => assert_true(false), // Should not be valid
    Error(errors) => {
      assert_true(errors.length() > 0)
      assert_true(contains_validation_error(errors, "service.name", "Required field missing"))
    }
  }
  
  // Fix missing field
  ConfigurationManager::set_string(config_manager, "service.name", "telemetry-service")
  
  // Test out of range value
  ConfigurationManager::set_int(config_manager, "service.port", 70000) // Above max
  
  let out_of_range_result = ConfigurationManager::validate(config_manager)
  match out_of_range_result {
    Ok(_) => assert_true(false), // Should not be valid
    Error(errors) => {
      assert_true(errors.length() > 0)
      assert_true(contains_validation_error(errors, "service.port", "Value out of range"))
    }
  }
  
  // Fix out of range value
  ConfigurationManager::set_int(config_manager, "service.port", 8080)
  
  // Test custom validation
  let custom_validator = CustomValidator::new("service.url", func(value : String) -> ValidationResult {
    if String::starts_with(value, "http://") || String::starts_with(value, "https://") {
      Valid
    } else {
      Invalid("URL must start with http:// or https://")
    }
  })
  
  ConfigurationSchema::add_custom_validator(schema, custom_validator)
  
  // Test invalid URL
  ConfigurationManager::set_string(config_manager, "service.url", "invalid-url")
  
  let custom_invalid_result = ConfigurationManager::validate(config_manager)
  match custom_invalid_result {
    Ok(_) => assert_true(false), // Should not be valid
    Error(errors) => {
      assert_true(errors.length() > 0)
      assert_true(contains_validation_error(errors, "service.url", "URL must start with http:// or https://"))
    }
  }
  
  // Test valid URL
  ConfigurationManager::set_string(config_manager, "service.url", "https://telemetry.example.com")
  
  let custom_valid_result = ConfigurationManager::validate(config_manager)
  match custom_valid_result {
    Ok(_) => assert_true(true),
    Error(_) => assert_true(false) // Should be valid
  }
  
  // Test validation with default values
  let default_schema = ConfigurationSchema::new()
  ConfigurationSchema::add_string_field_with_default(default_schema, "service.environment", "production")
  ConfigurationSchema::add_int_field_with_default(default_schema, "service.retries", 3)
  
  ConfigurationManager::set_validation_schema(config_manager, default_schema)
  
  // Don't set values, should use defaults
  let defaults_result = ConfigurationManager::validate_with_defaults(config_manager)
  match defaults_result {
    Ok(_) => assert_true(true),
    Error(_) => assert_true(false) // Should be valid with defaults
  }
  
  // Verify defaults were applied
  let environment = ConfigurationManager::get_string(config_manager, "service.environment")
  match environment {
    Some(env) => assert_eq(env, "production"),
    None => assert_true(false) // Should have default value
  }
  
  let retries = ConfigurationManager::get_int(config_manager, "service.retries")
  match retries {
    Some(r) => assert_eq(r, 3),
    None => assert_true(false) // Should have default value
  }
}

// Test 3: Configuration Watchers and Listeners
test "configuration watchers and listeners" {
  let config_manager = ConfigurationManager::new()
  
  // Set initial values
  ConfigurationManager::set_string(config_manager, "watched.key", "initial_value")
  ConfigurationManager::set_int(config_manager, "watched.number", 42)
  
  // Create configuration watcher
  let watcher = ConfigurationWatcher::new()
  
  // Add watch for specific key
  let string_watcher_id = ConfigurationWatcher::watch_string(watcher, "watched.key", func(old_value, new_value) {
    // String change callback
    assert_eq(old_value, "initial_value")
    assert_eq(new_value, "updated_value")
  })
  
  // Add watch for specific key with type checking
  let int_watcher_id = ConfigurationWatcher::watch_int(watcher, "watched.number", func(old_value, new_value) {
    // Int change callback
    assert_eq(old_value, 42)
    assert_eq(new_value, 100)
  })
  
  // Register watcher with config manager
  ConfigurationManager::add_watcher(config_manager, watcher)
  
  // Test watcher notifications
  ConfigurationManager::set_string(config_manager, "watched.key", "updated_value")
  ConfigurationManager::set_int(config_manager, "watched.number", 100)
  
  // Allow some time for callbacks to execute
  Time::sleep(100)
  
  // Remove watchers
  ConfigurationWatcher::remove_watch(watcher, string_watcher_id)
  ConfigurationWatcher::remove_watch(watcher, int_watcher_id)
  
  // Test global configuration listener
  let listener = ConfigurationListener::new(func(key, old_value, new_value) {
    // Global change callback
    assert_eq(key, "global.key")
    assert_eq(old_value, "old_global_value")
    assert_eq(new_value, "new_global_value")
  })
  
  ConfigurationManager::add_listener(config_manager, listener)
  
  // Test global listener notifications
  ConfigurationManager::set_string(config_manager, "global.key", "old_global_value")
  ConfigurationManager::set_string(config_manager, "global.key", "new_global_value")
  
  // Allow some time for callbacks to execute
  Time::sleep(100)
  
  // Remove listener
  ConfigurationManager::remove_listener(config_manager, listener)
  
  // Test conditional watcher
  let conditional_watcher_id = ConfigurationWatcher::watch_conditional(watcher, "conditional.key", func(old_value, new_value) {
    // Conditional callback - only notify if value length > 5
    String::length(new_value) > 5
  }, func(old_value, new_value) {
    // Conditional change callback
    assert_eq(old_value, "short")
    assert_eq(new_value, "longer_value")
  })
  
  // Test conditional notification (should not notify)
  ConfigurationManager::set_string(config_manager, "conditional.key", "short")
  
  // Test conditional notification (should notify)
  ConfigurationManager::set_string(config_manager, "conditional.key", "longer_value")
  
  // Allow some time for callbacks to execute
  Time::sleep(100)
  
  // Test batch changes
  ConfigurationManager::begin_batch(config_manager)
  ConfigurationManager::set_string(config_manager, "batch.key1", "value1")
  ConfigurationManager::set_string(config_manager, "batch.key2", "value2")
  ConfigurationManager::set_string(config_manager, "batch.key3", "value3")
  ConfigurationManager::end_batch(config_manager)
  
  // Verify batch watcher was notified
  let batch_notifications = ConfigurationWatcher::get_batch_notifications(watcher)
  assert_eq(batch_notifications.length(), 3)
}

// Test 4: Environment-Specific Configuration
test "environment specific configuration" {
  let config_manager = ConfigurationManager::new()
  
  // Set up environment-specific configurations
  let dev_config = EnvironmentConfig::new("development")
  EnvironmentConfig::set_string(dev_config, "database.host", "localhost")
  EnvironmentConfig::set_int(dev_config, "database.port", 5432)
  EnvironmentConfig::set_string(dev_config, "logging.level", "debug")
  
  let prod_config = EnvironmentConfig::new("production")
  EnvironmentConfig::set_string(prod_config, "database.host", "prod-db.example.com")
  EnvironmentConfig::set_int(prod_config, "database.port", 5432)
  EnvironmentConfig::set_string(prod_config, "logging.level", "info")
  
  let test_config = EnvironmentConfig::new("test")
  EnvironmentConfig::set_string(test_config, "database.host", "test-db.example.com")
  EnvironmentConfig::set_int(test_config, "database.port", 5433)
  EnvironmentConfig::set_string(test_config, "logging.level", "debug")
  
  // Register environment configurations
  ConfigurationManager::add_environment_config(config_manager, dev_config)
  ConfigurationManager::add_environment_config(config_manager, prod_config)
  ConfigurationManager::add_environment_config(config_manager, test_config)
  
  // Test loading development configuration
  ConfigurationManager::set_environment(config_manager, "development")
  
  let dev_db_host = ConfigurationManager::get_string(config_manager, "database.host")
  match dev_db_host {
    Some(host) => assert_eq(host, "localhost"),
    None => assert_true(false) // Should have value
  }
  
  let dev_log_level = ConfigurationManager::get_string(config_manager, "logging.level")
  match dev_log_level {
    Some(level) => assert_eq(level, "debug"),
    None => assert_true(false) // Should have value
  }
  
  // Test loading production configuration
  ConfigurationManager::set_environment(config_manager, "production")
  
  let prod_db_host = ConfigurationManager::get_string(config_manager, "database.host")
  match prod_db_host {
    Some(host) => assert_eq(host, "prod-db.example.com"),
    None => assert_true(false) // Should have value
  }
  
  let prod_log_level = ConfigurationManager::get_string(config_manager, "logging.level")
  match prod_log_level {
    Some(level) => assert_eq(level, "info"),
    None => assert_true(false) // Should have value
  }
  
  // Test loading test configuration
  ConfigurationManager::set_environment(config_manager, "test")
  
  let test_db_host = ConfigurationManager::get_string(config_manager, "database.host")
  match test_db_host {
    Some(host) => assert_eq(host, "test-db.example.com"),
    None => assert_true(false) // Should have value
  }
  
  let test_db_port = ConfigurationManager::get_int(config_manager, "database.port")
  match test_db_port {
    Some(port) => assert_eq(port, 5433),
    None => assert_true(false) // Should have value
  }
  
  // Test configuration inheritance
  let base_config = EnvironmentConfig::new("base")
  EnvironmentConfig::set_string(base_config, "service.name", "telemetry-service")
  EnvironmentConfig::set_string(base_config, "service.version", "1.0.0")
  
  let staging_config = EnvironmentConfig::new("staging")
  EnvironmentConfig::set_string(staging_config, "database.host", "staging-db.example.com")
  EnvironmentConfig::set_parent(staging_config, base_config)
  
  ConfigurationManager::add_environment_config(config_manager, staging_config)
  ConfigurationManager::set_environment(config_manager, "staging")
  
  // Should inherit from parent
  let staging_service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match staging_service_name {
    Some(name) => assert_eq(name, "telemetry-service"),
    None => assert_true(false) // Should inherit from parent
  }
  
  // Should override parent
  let staging_db_host = ConfigurationManager::get_string(config_manager, "database.host")
  match staging_db_host {
    Some(host) => assert_eq(host, "staging-db.example.com"),
    None => assert_true(false) // Should have own value
  }
  
  // Test environment detection from system
  let detected_env = ConfigurationManager::detect_environment(config_manager)
  match detected_env {
    Some(env) => assert_true(env == "development" || env == "production" || env == "test"),
    None => assert_true(true) // Might not be able to detect
  }
  
  // Test environment variable override
  System::set_env("CONFIG_OVERRIDE", "test")
  ConfigurationManager::enable_env_override(config_manager, true)
  
  let env_override_result = ConfigurationManager::load_from_env(config_manager)
  match env_override_result {
    Ok(_) => assert_true(true),
    Error(_) => assert_true(false) // Should load from environment
  }
  
  // Verify environment was set from override
  let current_env = ConfigurationManager::get_environment(config_manager)
  match current_env {
    Some(env) => assert_eq(env, "test"),
    None => assert_true(false) // Should have environment
  }
}

// Test 5: Configuration Encryption and Security
test "configuration encryption and security" {
  let config_manager = ConfigurationManager::new()
  
  // Create encryption key
  let encryption_key = EncryptionKey::generate()
  
  // Enable encryption
  ConfigurationManager::enable_encryption(config_manager, encryption_key)
  
  // Set sensitive values
  ConfigurationManager::set_string(config_manager, "database.password", "secret_password")
  ConfigurationManager::set_string(config_manager, "api.key", "secret_api_key")
  ConfigurationManager::set_string(config_manager, "jwt.secret", "secret_jwt_secret")
  
  // Mark fields as sensitive
  ConfigurationManager::mark_sensitive(config_manager, "database.password")
  ConfigurationManager::mark_sensitive(config_manager, "api.key")
  ConfigurationManager::mark_sensitive(config_manager, "jwt.secret")
  
  // Test that sensitive values are encrypted in storage
  let save_result = ConfigurationManager::save_to_file(config_manager, "encrypted_config.json")
  assert_true(save_result)
  
  // Read the raw file to verify encryption
  let raw_content = File::read_to_string("encrypted_config.json")
  match raw_content {
    Some(content) => {
      // Sensitive values should not appear in plain text
      assert_false(String::contains(content, "secret_password"))
      assert_false(String::contains(content, "secret_api_key"))
      assert_false(String::contains(content, "secret_jwt_secret"))
      
      // Non-sensitive values should appear in plain text
      assert_true(String::contains(content, "database.password")) // Key should be visible
      assert_true(String::contains(content, "api.key")) // Key should be visible
    }
    None => assert_true(false) // Should read file
  }
  
  // Test that values are properly decrypted when retrieved
  let password = ConfigurationManager::get_string(config_manager, "database.password")
  match password {
    Some(pwd) => assert_eq(pwd, "secret_password"),
    None => assert_true(false) // Should decrypt properly
  }
  
  let api_key = ConfigurationManager::get_string(config_manager, "api.key")
  match api_key {
    Some(key) => assert_eq(key, "secret_api_key"),
    None => assert_true(false) // Should decrypt properly
  }
  
  // Test configuration access control
  let access_control = AccessControl::new()
  
  // Add roles and permissions
  AccessControl::add_role(access_control, "admin")
  AccessControl::add_role(access_control, "operator")
  AccessControl::add_role(access_control, "viewer")
  
  // Add permissions to roles
  AccessControl::add_permission(access_control, "admin", ["read", "write", "delete"])
  AccessControl::add_permission(access_control, "operator", ["read", "write"])
  AccessControl::add_permission(access_control, "viewer", ["read"])
  
  // Set access control
  ConfigurationManager::set_access_control(config_manager, access_control)
  
  // Test access with different roles
  let admin_context = SecurityContext::new("admin")
  let operator_context = SecurityContext::new("operator")
  let viewer_context = SecurityContext::new("viewer")
  
  // Admin should be able to read and write
  let admin_read = ConfigurationManager::get_string_with_context(config_manager, "database.password", admin_context)
  match admin_read {
    Some(_) => assert_true(true),
    None => assert_true(false) // Should be able to read
  }
  
  let admin_write = ConfigurationManager::set_string_with_context(config_manager, "new.key", "new_value", admin_context)
  assert_true(admin_write)
  
  // Operator should be able to read and write
  let operator_read = ConfigurationManager::get_string_with_context(config_manager, "database.password", operator_context)
  match operator_read {
    Some(_) => assert_true(true),
    None => assert_true(false) // Should be able to read
  }
  
  let operator_write = ConfigurationManager::set_string_with_context(config_manager, "operator.key", "operator_value", operator_context)
  assert_true(operator_write)
  
  // Viewer should only be able to read
  let viewer_read = ConfigurationManager::get_string_with_context(config_manager, "database.password", viewer_context)
  match viewer_read {
    Some(_) => assert_true(true),
    None => assert_true(false) // Should be able to read
  }
  
  let viewer_write = ConfigurationManager::set_string_with_context(config_manager, "viewer.key", "viewer_value", viewer_context)
  assert_false(viewer_write) // Should not be able to write
  
  // Test configuration audit log
  let audit_logger = AuditLogger::new()
  ConfigurationManager::enable_audit_logging(config_manager, audit_logger)
  
  // Perform some operations
  ConfigurationManager::set_string_with_context(config_manager, "audit.test", "test_value", admin_context)
  ConfigurationManager::get_string_with_context(config_manager, "audit.test", operator_context)
  ConfigurationManager::remove_with_context(config_manager, "audit.test", admin_context)
  
  // Check audit log
  let audit_entries = AuditLogger::get_entries(audit_logger)
  assert_true(audit_entries.length() >= 3)
  
  // Verify audit entries contain required information
  for entry in audit_entries {
    assert_true(entry.timestamp > 0)
    assert_true(entry.user.length() > 0)
    assert_true(entry.action.length() > 0)
    assert_true(entry.key.length() > 0)
  }
}

// Test 6: Configuration Templates and Inheritance
test "configuration templates and inheritance" {
  let config_manager = ConfigurationManager::new()
  
  // Create configuration template
  let template = ConfigurationTemplate::new("base_service")
  
  // Add template values
  ConfigurationTemplate::set_string(template, "service.name", "")
  ConfigurationTemplate::set_int(template, "service.port", 8080)
  ConfigurationTemplate::set_bool(template, "service.ssl_enabled", false)
  ConfigurationTemplate::set_string(template, "logging.level", "info")
  ConfigurationTemplate::set_string(template, "database.host", "localhost")
  ConfigurationTemplate::set_int(template, "database.port", 5432)
  
  // Register template
  ConfigurationManager::register_template(config_manager, template)
  
  // Create configuration from template
  let service_config = ConfigurationManager::create_from_template(config_manager, "base_service")
  
  // Verify template values were applied
  let service_port = ConfigurationManager::get_int(service_config, "service.port")
  match service_port {
    Some(port) => assert_eq(port, 8080),
    None => assert_true(false) // Should have template value
  }
  
  let ssl_enabled = ConfigurationManager::get_bool(service_config, "service.ssl_enabled")
  match ssl_enabled {
    Some(ssl) => assert_false(ssl),
    None => assert_true(false) // Should have template value
  }
  
  // Override template values
  ConfigurationManager::set_string(service_config, "service.name", "telemetry-service")
  ConfigurationManager::set_int(service_config, "service.port", 9090)
  
  // Verify overrides
  let service_name = ConfigurationManager::get_string(service_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "telemetry-service"),
    None => assert_true(false) // Should have override value
  }
  
  let overridden_port = ConfigurationManager::get_int(service_config, "service.port")
  match overridden_port {
    Some(port) => assert_eq(port, 9090),
    None => assert_true(false) // Should have override value
  }
  
  // Verify non-overridden values still from template
  let logging_level = ConfigurationManager::get_string(service_config, "logging.level")
  match logging_level {
    Some(level) => assert_eq(level, "info"),
    None => assert_true(false) // Should have template value
  }
  
  // Test template inheritance
  let web_template = ConfigurationTemplate::new("web_service")
  ConfigurationTemplate::set_parent(web_template, "base_service")
  
  // Add web-specific values
  ConfigurationTemplate::set_string(web_template, "web.static_path", "/static")
  ConfigurationTemplate::set_int(web_template, "web.session_timeout", 3600)
  
  ConfigurationManager::register_template(config_manager, web_template)
  
  // Create configuration from inherited template
  let web_config = ConfigurationManager::create_from_template(config_manager, "web_service")
  
  // Should have base template values
  let db_host = ConfigurationManager::get_string(web_config, "database.host")
  match db_host {
    Some(host) => assert_eq(host, "localhost"),
    None => assert_true(false) // Should have inherited value
  }
  
  // Should have web-specific values
  let static_path = ConfigurationManager::get_string(web_config, "web.static_path")
  match static_path {
    Some(path) => assert_eq(path, "/static"),
    None => assert_true(false) // Should have web-specific value
  }
  
  // Test template composition
  let db_template = ConfigurationTemplate::new("database")
  ConfigurationTemplate::set_string(db_template, "database.host", "localhost")
  ConfigurationTemplate::set_int(db_template, "database.port", 5432)
  ConfigurationTemplate::set_string(db_template, "database.name", "telemetry")
  
  let cache_template = ConfigurationTemplate::new("cache")
  ConfigurationTemplate::set_string(cache_template, "cache.type", "redis")
  ConfigurationTemplate::set_string(cache_template, "cache.host", "localhost")
  ConfigurationTemplate::set_int(cache_template, "cache.port", 6379)
  
  ConfigurationManager::register_template(config_manager, db_template)
  ConfigurationManager::register_template(config_manager, cache_template)
  
  // Compose templates
  let composed_config = ConfigurationManager::compose_templates(config_manager, ["database", "cache"])
  
  // Should have values from both templates
  let db_name = ConfigurationManager::get_string(composed_config, "database.name")
  match db_name {
    Some(name) => assert_eq(name, "telemetry"),
    None => assert_true(false) // Should have database value
  }
  
  let cache_type = ConfigurationManager::get_string(composed_config, "cache.type")
  match cache_type {
    Some(type) => assert_eq(type, "redis"),
    None => assert_true(false) // Should have cache value
  }
  
  // Test template parameterization
  let param_template = ConfigurationTemplate::new("parameterized")
  ConfigurationTemplate::set_string(param_template, "service.name", "{{SERVICE_NAME}}")
  ConfigurationTemplate::set_int(param_template, "service.port", "{{SERVICE_PORT}}")
  ConfigurationTemplate::set_string(param_template, "service.url", "http://{{SERVICE_HOST}}:{{SERVICE_PORT}}")
  
  ConfigurationManager::register_template(config_manager, param_template)
  
  // Create parameterized configuration
  let parameters = Map::new()
  Map::set(parameters, "SERVICE_NAME", "parameterized-service")
  Map::set(parameters, "SERVICE_PORT", "8080")
  Map::set(parameters, "SERVICE_HOST", "localhost")
  
  let param_config = ConfigurationManager::create_from_template_with_params(config_manager, "parameterized", parameters)
  
  // Verify parameter substitution
  let param_service_name = ConfigurationManager::get_string(param_config, "service.name")
  match param_service_name {
    Some(name) => assert_eq(name, "parameterized-service"),
    None => assert_true(false) // Should have substituted value
  }
  
  let param_service_url = ConfigurationManager::get_string(param_config, "service.url")
  match param_service_url {
    Some(url) => assert_eq(url, "http://localhost:8080"),
    None => assert_true(false) // Should have substituted value
  }
}

// Test 7: Dynamic Configuration Updates
test "dynamic configuration updates" {
  let config_manager = ConfigurationManager::new()
  
  // Enable dynamic updates
  ConfigurationManager::enable_dynamic_updates(config_manager, true)
  
  // Set initial values
  ConfigurationManager::set_string(config_manager, "dynamic.value", "initial")
  ConfigurationManager::set_int(config_manager, "dynamic.number", 100)
  
  // Create dynamic updater
  let updater = DynamicUpdater::new()
  
  // Add update strategies
  let restart_strategy = RestartStrategy::new(["service.name", "service.port"])
  let reload_strategy = ReloadStrategy::new(["logging.level", "feature.flags"])
  let immediate_strategy = ImmediateStrategy::new(["cache.ttl", "timeout.value"])
  
  DynamicUpdater::add_strategy(updater, restart_strategy)
  DynamicUpdater::add_strategy(updater, reload_strategy)
  DynamicUpdater::add_strategy(updater, immediate_strategy)
  
  // Register updater
  ConfigurationManager::set_dynamic_updater(config_manager, updater)
  
  // Test immediate strategy updates
  ConfigurationManager::set_string(config_manager, "cache.ttl", "300")
  
  let immediate_value = ConfigurationManager::get_string(config_manager, "cache.ttl")
  match immediate_value {
    Some(value) => assert_eq(value, "300"),
    None => assert_true(false) // Should be updated immediately
  }
  
  // Test reload strategy updates
  ConfigurationManager::set_string(config_manager, "logging.level", "debug")
  
  let reload_value = ConfigurationManager::get_string(config_manager, "logging.level")
  match reload_value {
    Some(value) => assert_eq(value, "debug"),
    None => assert_true(false) // Should be updated
  }
  
  // Verify reload was triggered
  let reload_events = DynamicUpdater::get_reload_events(updater)
  assert_true(reload_events.length() > 0)
  
  // Test restart strategy updates
  ConfigurationManager::set_string(config_manager, "service.name", "updated-service")
  
  let restart_value = ConfigurationManager::get_string(config_manager, "service.name")
  match restart_value {
    Some(value) => assert_eq(value, "updated-service"),
    None => assert_true(false) // Should be updated
  }
  
  // Verify restart was scheduled
  let restart_events = DynamicUpdater::get_restart_events(updater)
  assert_true(restart_events.length() > 0)
  
  // Test configuration file watching
  ConfigurationManager::enable_file_watching(config_manager, "dynamic_config.json")
  
  // Create initial config file
  let file_config = TestConfig::new()
  TestConfig::set_string(file_config, "file.watched", "initial_value")
  TestConfig::save_to_file(file_config, "dynamic_config.json")
  
  // Load from file
  ConfigurationManager::load_from_file(config_manager, "dynamic_config.json")
  
  let initial_file_value = ConfigurationManager::get_string(config_manager, "file.watched")
  match initial_file_value {
    Some(value) => assert_eq(value, "initial_value"),
    None => assert_true(false) // Should have value
  }
  
  // Update file
  TestConfig::set_string(file_config, "file.watched", "updated_value")
  TestConfig::save_to_file(file_config, "dynamic_config.json")
  
  // Wait for file watcher to detect changes
  Time::sleep(1100)
  
  // Verify value was updated
  let updated_file_value = ConfigurationManager::get_string(config_manager, "file.watched")
  match updated_file_value {
    Some(value) => assert_eq(value, "updated_value"),
    None => assert_true(false) // Should be updated
  }
  
  // Test remote configuration updates
  let remote_config = RemoteConfiguration::new("https://config.example.com/telemetry")
  RemoteConfiguration::set_auth_token(remote_config, "auth_token")
  RemoteConfiguration::set_poll_interval(remote_config, 5000) // 5 seconds
  
  ConfigurationManager::set_remote_config(config_manager, remote_config)
  
  // Enable remote updates
  ConfigurationManager::enable_remote_updates(config_manager, true)
  
  // Simulate remote update
  RemoteConfiguration::simulate_update(remote_config, [
    ("remote.setting1", "value1"),
    ("remote.setting2", "value2")
  ])
  
  // Wait for remote update to be applied
  Time::sleep(100)
  
  // Verify remote values were applied
  let remote_value1 = ConfigurationManager::get_string(config_manager, "remote.setting1")
  match remote_value1 {
    Some(value) => assert_eq(value, "value1"),
    None => assert_true(false) // Should have remote value
  }
  
  let remote_value2 = ConfigurationManager::get_string(config_manager, "remote.setting2")
  match remote_value2 {
    Some(value) => assert_eq(value, "value2"),
    None => assert_true(false) // Should have remote value
  }
  
  // Test configuration rollback
  ConfigurationManager::create_checkpoint(config_manager, "before_rollback")
  
  // Make some changes
  ConfigurationManager::set_string(config_manager, "rollback.test", "new_value")
  ConfigurationManager::set_int(config_manager, "rollback.number", 999)
  
  // Verify changes
  let test_value = ConfigurationManager::get_string(config_manager, "rollback.test")
  match test_value {
    Some(value) => assert_eq(value, "new_value"),
    None => assert_true(false) // Should have new value
  }
  
  // Rollback to checkpoint
  let rollback_result = ConfigurationManager::rollback_to_checkpoint(config_manager, "before_rollback")
  assert_true(rollback_result)
  
  // Verify rollback
  let rolled_back_value = ConfigurationManager::get_string(config_manager, "rollback.test")
  match rolled_back_value {
    Some(_) => assert_true(false), // Should not have value
    None => assert_true(true)
  }
}

// Test 8: Configuration Merging and Overrides
test "configuration merging and overrides" {
  let config_manager = ConfigurationManager::new()
  
  // Create base configuration
  let base_config = TestConfig::new()
  TestConfig::set_string(base_config, "service.name", "telemetry-service")
  TestConfig::set_int(base_config, "service.port", 8080)
  TestConfig::set_bool(base_config, "service.debug", false)
  TestConfig::set_string(base_config, "database.host", "localhost")
  TestConfig::set_int(base_config, "database.port", 5432)
  
  // Create override configuration
  let override_config = TestConfig::new()
  TestConfig::set_string(override_config, "service.debug", "true")
  TestConfig::set_string(override_config, "database.host", "prod-db.example.com")
  TestConfig::set_int(override_config, "database.pool_size", 10)
  TestConfig::set_string(override_config, "cache.enabled", "true")
  
  // Load base configuration
  ConfigurationManager::load_from_config(config_manager, base_config)
  
  // Verify base values
  let base_service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match base_service_name {
    Some(name) => assert_eq(name, "telemetry-service"),
    None => assert_true(false) // Should have base value
  }
  
  let base_debug = ConfigurationManager::get_bool(config_manager, "service.debug")
  match base_debug {
    Some(debug) => assert_false(debug),
    None => assert_true(false) // Should have base value
  }
  
  // Merge override configuration
  ConfigurationManager::merge_config(config_manager, override_config)
  
  // Verify merged values
  let merged_service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match merged_service_name {
    Some(name) => assert_eq(name, "telemetry-service"), // Should keep base value
    None => assert_true(false) // Should have value
  }
  
  let merged_debug = ConfigurationManager::get_bool(config_manager, "service.debug")
  match merged_debug {
    Some(debug) => assert_true(debug), // Should use override value
    None => assert_true(false) // Should have value
  }
  
  let merged_db_host = ConfigurationManager::get_string(config_manager, "database.host")
  match merged_db_host {
    Some(host) => assert_eq(host, "prod-db.example.com"), // Should use override value
    None => assert_true(false) // Should have value
  }
  
  let merged_pool_size = ConfigurationManager::get_int(config_manager, "database.pool_size")
  match merged_pool_size {
    Some(size) => assert_eq(size, 10), // Should use new value from override
    None => assert_true(false) // Should have value
  }
  
  // Test command line argument overrides
  let args = [
    "--service.port=9090",
    "--database.host=cmd-db.example.com",
    "--new.cmd_only=value"
  ]
  
  ConfigurationManager::apply_command_line_args(config_manager, args)
  
  // Verify command line overrides
  let cmd_port = ConfigurationManager::get_int(config_manager, "service.port")
  match cmd_port {
    Some(port) => assert_eq(port, 9090), // Should use command line value
    None => assert_true(false) // Should have value
  }
  
  let cmd_db_host = ConfigurationManager::get_string(config_manager, "database.host")
  match cmd_db_host {
    Some(host) => assert_eq(host, "cmd-db.example.com"), // Should use command line value
    None => assert_true(false) // Should have value
  }
  
  let cmd_only = ConfigurationManager::get_string(config_manager, "new.cmd_only")
  match cmd_only {
    Some(value) => assert_eq(value, "value"), // Should have new value
    None => assert_true(false) // Should have value
  }
  
  // Test environment variable overrides
  System::set_env("TELEMETRY_SERVICE_DEBUG", "true")
  System::set_env("TELEMETRY_DATABASE_HOST", "env-db.example.com")
  System::set_env("TELEMETRY_NEW_ENV_ONLY", "env_value")
  
  ConfigurationManager::apply_environment_overrides(config_manager, "TELEMETRY_")
  
  // Verify environment overrides
  let env_debug = ConfigurationManager::get_bool(config_manager, "service.debug")
  match env_debug {
    Some(debug) => assert_true(debug), // Should use environment value
    None => assert_true(false) // Should have value
  }
  
  let env_db_host = ConfigurationManager::get_string(config_manager, "database.host")
  match env_db_host {
    Some(host) => assert_eq(host, "env-db.example.com"), // Should use environment value
    None => assert_true(false) // Should have value
  }
  
  let env_only = ConfigurationManager::get_string(config_manager, "new.env_only")
  match env_only {
    Some(value) => assert_eq(value, "env_value"), // Should have new value
    None => assert_true(false) // Should have value
  }
  
  // Test configuration precedence
  // Environment variables should override command line arguments
  System::set_env("TELEMETRY_SERVICE_PORT", "7070")
  ConfigurationManager::apply_environment_overrides(config_manager, "TELEMETRY_")
  
  let precedence_port = ConfigurationManager::get_int(config_manager, "service.port")
  match precedence_port {
    Some(port) => assert_eq(port, 7070), // Should use environment value (highest precedence)
    None => assert_true(false) // Should have value
  }
  
  // Test deep merging
  let deep_base = TestConfig::new()
  TestConfig::set_string(deep_base, "nested.key1", "value1")
  TestConfig::set_string(deep_base, "nested.key2", "value2")
  TestConfig::set_string(deep_base, "nested.sub.key1", "sub_value1")
  
  let deep_override = TestConfig::new()
  TestConfig::set_string(deep_override, "nested.key2", "new_value2")
  TestConfig::set_string(deep_override, "nested.sub.key2", "sub_value2")
  
  ConfigurationManager::load_from_config(config_manager, deep_base)
  ConfigurationManager::merge_config(config_manager, deep_override)
  
  // Verify deep merge
  let deep_key1 = ConfigurationManager::get_string(config_manager, "nested.key1")
  match deep_key1 {
    Some(value) => assert_eq(value, "value1"), // Should keep base value
    None => assert_true(false) // Should have value
  }
  
  let deep_key2 = ConfigurationManager::get_string(config_manager, "nested.key2")
  match deep_key2 {
    Some(value) => assert_eq(value, "new_value2"), // Should use override value
    None => assert_true(false) // Should have value
  }
  
  let deep_sub_key1 = ConfigurationManager::get_string(config_manager, "nested.sub.key1")
  match deep_sub_key1 {
    Some(value) => assert_eq(value, "sub_value1"), // Should keep base value
    None => assert_true(false) // Should have value
  }
  
  let deep_sub_key2 = ConfigurationManager::get_string(config_manager, "nested.sub.key2")
  match deep_sub_key2 {
    Some(value) => assert_eq(value, "sub_value2"), // Should use override value
    None => assert_true(false) // Should have value
  }
}

// Test 9: Configuration Validation and Transformation
test "configuration validation and transformation" {
  let config_manager = ConfigurationManager::new()
  
  // Create transformation rules
  let transformer = ConfigurationTransformer::new()
  
  // Add string transformations
  ConfigurationTransformer::add_string_transformer(transformer, "service.name", func(value) {
    String::to_lower_case(value)
  })
  
  ConfigurationTransformer::add_string_transformer(transformer, "service.host", func(value) {
    String::trim(value)
  })
  
  // Add number transformations
  ConfigurationTransformer::add_int_transformer(transformer, "service.port", func(value) {
    if value < 1024 {
      1024 // Minimum port number
    } else if value > 65535 {
      65535 // Maximum port number
    } else {
      value
    }
  })
  
  ConfigurationTransformer::add_float_transformer(transformer, "service.timeout", func(value) {
    value * 1000.0 // Convert seconds to milliseconds
  })
  
  // Add boolean transformations
  ConfigurationTransformer::add_bool_transformer(transformer, "service.debug", func(value) {
    if value {
      "true"
    } else {
      "false"
    }
  })
  
  // Set transformer
  ConfigurationManager::set_transformer(config_manager, transformer)
  
  // Set values that will be transformed
  ConfigurationManager::set_string(config_manager, "service.name", "TELEMETRY-SERVICE")
  ConfigurationManager::set_string(config_manager, "service.host", "  localhost  ")
  ConfigurationManager::set_int(config_manager, "service.port", 80) // Below minimum
  ConfigurationManager::set_float(config_manager, "service.timeout", 30.5) // Will be converted to milliseconds
  ConfigurationManager::set_bool(config_manager, "service.debug", true)
  
  // Verify transformations
  let transformed_name = ConfigurationManager::get_string(config_manager, "service.name")
  match transformed_name {
    Some(name) => assert_eq(name, "telemetry-service"), // Should be lowercase
    None => assert_true(false) // Should have value
  }
  
  let transformed_host = ConfigurationManager::get_string(config_manager, "service.host")
  match transformed_host {
    Some(host) => assert_eq(host, "localhost"), // Should be trimmed
    None => assert_true(false) // Should have value
  }
  
  let transformed_port = ConfigurationManager::get_int(config_manager, "service.port")
  match transformed_port {
    Some(port) => assert_eq(port, 1024), // Should be minimum
    None => assert_true(false) // Should have value
  }
  
  let transformed_timeout = ConfigurationManager::get_float(config_manager, "service.timeout")
  match transformed_timeout {
    Some(timeout) => assert_eq(timeout, 30500.0), // Should be converted to milliseconds
    None => assert_true(false) // Should have value
  }
  
  let transformed_debug = ConfigurationManager::get_string(config_manager, "service.debug")
  match transformed_debug {
    Some(debug) => assert_eq(debug, "true"), // Should be string "true"
    None => assert_true(false) // Should have value
  }
  
  // Test conditional transformations
  let conditional_transformer = ConfigurationTransformer::new()
  
  ConfigurationTransformer::add_conditional_transformer(conditional_transformer, "cache.size", func(value) {
    // If value is "auto", calculate based on available memory
    if value == "auto" {
      let available_memory = MemoryStats::available_memory()
      (available_memory / 10).to_string() // Use 10% of available memory
    } else {
      value
    }
  })
  
  ConfigurationManager::set_transformer(config_manager, conditional_transformer)
  
  // Test conditional transformation
  ConfigurationManager::set_string(config_manager, "cache.size", "auto")
  
  let cache_size = ConfigurationManager::get_string(config_manager, "cache.size")
  match cache_size {
    Some(size) => assert_true(size != "auto"), // Should be transformed
    None => assert_true(false) // Should have value
  }
  
  // Test validation with transformations
  let validation_transformer = ConfigurationTransformer::new()
  
  ConfigurationTransformer::add_validation_transformer(validation_transformer, "database.url", func(value) {
    // Validate URL format and add default port if missing
    if String::starts_with(value, "postgresql://") && !String::contains(value, ":5432") {
      String::replace(value, "postgresql://", "postgresql://localhost:5432/")
    } else {
      value
    }
  })
  
  ConfigurationManager::set_transformer(config_manager, validation_transformer)
  
  // Test validation transformation
  ConfigurationManager::set_string(config_manager, "database.url", "postgresql://telemetry")
  
  let db_url = ConfigurationManager::get_string(config_manager, "database.url")
  match db_url {
    Some(url) => assert_eq(url, "postgresql://localhost:5432/telemetry"), // Should be transformed
    None => assert_true(false) // Should have value
  }
  
  // Test transformation chains
  let chain_transformer = ConfigurationTransformer::new()
  
  // Create transformation chain: trim -> lowercase -> validate
  ConfigurationTransformer::add_string_transformer(chain_transformer, "api.key", func(value) {
    String::trim(value)
  })
  
  ConfigurationTransformer::add_string_transformer(chain_transformer, "api.key", func(value) {
    String::to_lower_case(value)
  })
  
  ConfigurationTransformer::add_validation_transformer(chain_transformer, "api.key", func(value) {
    if String::length(value) < 10 {
      "default_api_key_" + value
    } else {
      value
    }
  })
  
  ConfigurationManager::set_transformer(config_manager, chain_transformer)
  
  // Test transformation chain
  ConfigurationManager::set_string(config_manager, "api.key", "  KEY  ")
  
  let api_key = ConfigurationManager::get_string(config_manager, "api.key")
  match api_key {
    Some(key) => {
      assert_true(String::starts_with(key, "default_api_key_")) // Should be transformed
      assert_true(String::contains(key, "key")) // Should contain original value
    }
    None => assert_true(false) // Should have value
  }
}

// Test 10: Configuration Export and Import
test "configuration export and import" {
  let config_manager = ConfigurationManager::new()
  
  // Set up configuration
  ConfigurationManager::set_string(config_manager, "service.name", "telemetry-service")
  ConfigurationManager::set_int(config_manager, "service.port", 8080)
  ConfigurationManager::set_bool(config_manager, "service.debug", true)
  ConfigurationManager::set_float(config_manager, "service.timeout", 30.5)
  ConfigurationManager::set_string(config_manager, "database.host", "localhost")
  ConfigurationManager::set_int(config_manager, "database.port", 5432)
  
  // Test export to JSON
  let json_export_result = ConfigurationManager::export_to_json(config_manager, "exported_config.json")
  assert_true(json_export_result)
  
  // Verify JSON export
  let json_content = File::read_to_string("exported_config.json")
  match json_content {
    Some(content) => {
      assert_true(String::contains(content, "service.name"))
      assert_true(String::contains(content, "telemetry-service"))
      assert_true(String::contains(content, "service.port"))
      assert_true(String::contains(content, "8080"))
    }
    None => assert_true(false) // Should read file
  }
  
  // Test export to YAML
  let yaml_export_result = ConfigurationManager::export_to_yaml(config_manager, "exported_config.yaml")
  assert_true(yaml_export_result)
  
  // Verify YAML export
  let yaml_content = File::read_to_string("exported_config.yaml")
  match yaml_content {
    Some(content) => {
      assert_true(String::contains(content, "service.name"))
      assert_true(String::contains(content, "telemetry-service"))
      assert_true(String::contains(content, "service.port"))
      assert_true(String::contains(content, "8080"))
    }
    None => assert_true(false) // Should read file
  }
  
  // Test export to properties
  let properties_export_result = ConfigurationManager::export_to_properties(config_manager, "exported_config.properties")
  assert_true(properties_export_result)
  
  // Verify properties export
  let properties_content = File::read_to_string("exported_config.properties")
  match properties_content {
    Some(content) => {
      assert_true(String::contains(content, "service.name=telemetry-service"))
      assert_true(String::contains(content, "service.port=8080"))
    }
    None => assert_true(false) // Should read file
  }
  
  // Clear configuration
  ConfigurationManager::clear(config_manager)
  
  // Test import from JSON
  let json_import_result = ConfigurationManager::import_from_json(config_manager, "exported_config.json")
  assert_true(json_import_result)
  
  // Verify import
  let imported_name = ConfigurationManager::get_string(config_manager, "service.name")
  match imported_name {
    Some(name) => assert_eq(name, "telemetry-service"),
    None => assert_true(false) // Should have imported value
  }
  
  let imported_port = ConfigurationManager::get_int(config_manager, "service.port")
  match imported_port {
    Some(port) => assert_eq(port, 8080),
    None => assert_true(false) // Should have imported value
  }
  
  // Clear configuration
  ConfigurationManager::clear(config_manager)
  
  // Test import from YAML
  let yaml_import_result = ConfigurationManager::import_from_yaml(config_manager, "exported_config.yaml")
  assert_true(yaml_import_result)
  
  // Verify import
  let yaml_imported_name = ConfigurationManager::get_string(config_manager, "service.name")
  match yaml_imported_name {
    Some(name) => assert_eq(name, "telemetry-service"),
    None => assert_true(false) // Should have imported value
  }
  
  // Clear configuration
  ConfigurationManager::clear(config_manager)
  
  // Test import from properties
  let properties_import_result = ConfigurationManager::import_from_properties(config_manager, "exported_config.properties")
  assert_true(properties_import_result)
  
  // Verify import
  let properties_imported_name = ConfigurationManager::get_string(config_manager, "service.name")
  match properties_imported_name {
    Some(name) => assert_eq(name, "telemetry-service"),
    None => assert_true(false) // Should have imported value
  }
  
  // Test selective export
  let export_filter = ConfigurationFilter::new()
  ConfigurationFilter::include_pattern(export_filter, "service.*")
  ConfigurationFilter::exclude_pattern(export_filter, "*.debug")
  
  let selective_export_result = ConfigurationManager::export_filtered(config_manager, "selective_config.json", export_filter)
  assert_true(selective_export_result)
  
  // Verify selective export
  let selective_content = File::read_to_string("selective_config.json")
  match selective_content {
    Some(content) => {
      assert_true(String::contains(content, "service.name"))
      assert_false(String::contains(content, "service.debug")) // Should be excluded
      assert_false(String::contains(content, "database")) // Should be excluded
    }
    None => assert_true(false) // Should read file
  }
  
  // Test export with encryption
  let encryption_key = EncryptionKey::generate()
  let encrypted_export_result = ConfigurationManager::export_encrypted(config_manager, "encrypted_config.json", encryption_key)
  assert_true(encrypted_export_result)
  
  // Verify encrypted export
  let encrypted_content = File::read_to_string("encrypted_config.json")
  match encrypted_content {
    Some(content) => {
      // Should not contain plain text values
      assert_false(String::contains(content, "telemetry-service"))
      assert_false(String::contains(content, "8080"))
    }
    None => assert_true(false) // Should read file
  }
  
  // Test import with decryption
  ConfigurationManager::clear(config_manager)
  
  let encrypted_import_result = ConfigurationManager::import_encrypted(config_manager, "encrypted_config.json", encryption_key)
  assert_true(encrypted_import_result)
  
  // Verify decrypted import
  let decrypted_name = ConfigurationManager::get_string(config_manager, "service.name")
  match decrypted_name {
    Some(name) => assert_eq(name, "telemetry-service"),
    None => assert_true(false) // Should have imported value
  }
  
  // Test configuration comparison
  let config_manager2 = ConfigurationManager::new()
  ConfigurationManager::import_from_json(config_manager2, "exported_config.json")
  
  let comparison = ConfigurationManager::compare(config_manager, config_manager2)
  assert_true(comparison.are_equal)
  
  // Modify one configuration
  ConfigurationManager::set_int(config_manager2, "service.port", 9090)
  
  let comparison_after_change = ConfigurationManager::compare(config_manager, config_manager2)
  assert_false(comparison_after_change.are_equal)
  assert_true(comparison_after_change.differences.length() > 0)
}

// Helper function to check if validation error contains specific message
func contains_validation_error(errors : Array[ValidationError], key : String, message : String) -> Bool {
  for error in errors {
    if error.key == key && String::contains(error.message, message) {
      return true
    }
  }
  return false
}