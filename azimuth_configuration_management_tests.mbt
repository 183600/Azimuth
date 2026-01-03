// Azimuth Configuration Management Dynamic Update Tests
// This file contains test cases for dynamic configuration management

// Test 1: Basic Configuration Operations
test "basic configuration operations" {
  let config_manager = ConfigurationManager::new()
  
  // Test setting and getting configuration values
  ConfigurationManager::set(config_manager, "service.name", StringValue("azimuth-service"))
  ConfigurationManager::set(config_manager, "service.version", StringValue("1.0.0"))
  ConfigurationManager::set(config_manager, "service.port", IntValue(8080))
  ConfigurationManager::set(config_manager, "service.debug", BoolValue(false))
  
  // Test retrieving configuration values
  let service_name = ConfigurationManager::get(config_manager, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  let service_port = ConfigurationManager::get(config_manager, "service.port")
  match service_port {
    Some(IntValue(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  let service_debug = ConfigurationManager::get(config_manager, "service.debug")
  match service_debug {
    Some(BoolValue(debug)) => assert_false(debug)
    _ => assert_true(false)
  }
  
  // Test getting non-existent configuration
  let non_existent = ConfigurationManager::get(config_manager, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Configuration Default Values
test "configuration default values" {
  let config_manager = ConfigurationManager::new()
  
  // Set up default values
  ConfigurationManager::set_default(config_manager, "timeout.ms", IntValue(5000))
  ConfigurationManager::set_default(config_manager, "retry.count", IntValue(3))
  ConfigurationManager::set_default(config_manager, "compression.enabled", BoolValue(true))
  
  // Test default values are returned when no explicit value is set
  let timeout = ConfigurationManager::get_with_default(config_manager, "timeout.ms")
  match timeout {
    Some(IntValue(t)) => assert_eq(t, 5000)
    _ => assert_true(false)
  }
  
  // Test explicit values override defaults
  ConfigurationManager::set(config_manager, "timeout.ms", IntValue(10000))
  let updated_timeout = ConfigurationManager::get_with_default(config_manager, "timeout.ms")
  match updated_timeout {
    Some(IntValue(t)) => assert_eq(t, 10000)
    _ => assert_true(false)
  }
  
  // Test default values are still accessible
  let retry_count = ConfigurationManager::get_with_default(config_manager, "retry.count")
  match retry_count {
    Some(IntValue(count)) => assert_eq(count, 3)
    _ => assert_true(false)
  }
}

// Test 3: Configuration Validation
test "configuration validation" {
  let config_manager = ConfigurationManager::new()
  
  // Set up validation rules
  ConfigurationManager::add_validator(config_manager, "service.port", IntRange(1, 65535))
  ConfigurationManager::add_validator(config_manager, "service.name", NonEmptyString)
  ConfigurationManager::add_validator(config_manager, "retry.count", IntRange(0, 10))
  
  // Test valid values
  let valid_port = ConfigurationManager::set_validated(config_manager, "service.port", IntValue(8080))
  match valid_port {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  let valid_name = ConfigurationManager::set_validated(config_manager, "service.name", StringValue("test-service"))
  match valid_name {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Test invalid values
  let invalid_port = ConfigurationManager::set_validated(config_manager, "service.port", IntValue(70000))
  match invalid_port {
    Success(_) => assert_true(false)
    Error(error) => assert_eq(error.code, ValidationError)
  }
  
  let invalid_name = ConfigurationManager::set_validated(config_manager, "service.name", StringValue(""))
  match invalid_name {
    Success(_) => assert_true(false)
    Error(error) => assert_eq(error.code, ValidationError)
  }
}

// Test 4: Configuration Watchers
test "configuration watchers" {
  let config_manager = ConfigurationManager::new()
  
  // Set initial configuration
  ConfigurationManager::set(config_manager, "service.port", IntValue(8080))
  
  // Create a watcher
  let change_count = AtomicInt::new(0)
  let last_value = AtomicRef::new(None)
  
  let watcher = ConfigurationWatcher::new("service.port", {
    (old_value, new_value) => {
      AtomicInt::increment(change_count)
      AtomicRef::set(last_value, new_value)
    }
  })
  
  // Register watcher
  ConfigurationManager::add_watcher(config_manager, watcher)
  
  // Update configuration
  ConfigurationManager::set(config_manager, "service.port", IntValue(9090))
  
  // Verify watcher was called
  assert_eq(AtomicInt::get(change_count), 1)
  match AtomicRef::get(last_value) {
    Some(IntValue(port)) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  
  // Update configuration again
  ConfigurationManager::set(config_manager, "service.port", IntValue(7070))
  
  // Verify watcher was called again
  assert_eq(AtomicInt::get(change_count), 2)
  match AtomicRef::get(last_value) {
    Some(IntValue(port)) => assert_eq(port, 7070)
    _ => assert_true(false)
  }
}

// Test 5: Configuration Groups
test "configuration groups" {
  let config_manager = ConfigurationManager::new()
  
  // Create configuration groups
  let database_group = ConfigurationManager::create_group(config_manager, "database")
  let cache_group = ConfigurationManager::create_group(config_manager, "cache")
  
  // Set group-specific configuration
  ConfigurationManager::set_in_group(database_group, "host", StringValue("localhost"))
  ConfigurationManager::set_in_group(database_group, "port", IntValue(5432))
  ConfigurationManager::set_in_group(database_group, "name", StringValue("azimuth_db"))
  
  ConfigurationManager::set_in_group(cache_group, "host", StringValue("redis.example.com"))
  ConfigurationManager::set_in_group(cache_group, "port", IntValue(6379))
  ConfigurationManager::set_in_group(cache_group, "ttl", IntValue(3600))
  
  // Test group-specific retrieval
  let db_host = ConfigurationManager::get_from_group(database_group, "host")
  match db_host {
    Some(StringValue(host)) => assert_eq(host, "localhost")
    _ => assert_true(false)
  }
  
  let cache_port = ConfigurationManager::get_from_group(cache_group, "port")
  match cache_port {
    Some(IntValue(port)) => assert_eq(port, 6379)
    _ => assert_true(false)
  }
  
  // Test full key retrieval
  let db_host_full = ConfigurationManager::get(config_manager, "database.host")
  match db_host_full {
    Some(StringValue(host)) => assert_eq(host, "localhost")
    _ => assert_true(false)
  }
  
  // Test group listing
  let db_keys = ConfigurationManager::list_group_keys(database_group)
  assert_eq(db_keys.length(), 3)
  assert_true(db_keys.contains("host"))
  assert_true(db_keys.contains("port"))
  assert_true(db_keys.contains("name"))
}

// Test 6: Configuration Persistence
test "configuration persistence" {
  let config_manager = ConfigurationManager::new()
  
  // Set configuration
  ConfigurationManager::set(config_manager, "service.name", StringValue("azimuth-service"))
  ConfigurationManager::set(config_manager, "service.port", IntValue(8080))
  ConfigurationManager::set(config_manager, "service.debug", BoolValue(true))
  
  // Save configuration to file
  let temp_file = "/tmp/azimuth_config_test.json"
  let save_result = ConfigurationManager::save_to_file(config_manager, temp_file)
  match save_result {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Create new configuration manager
  let new_config_manager = ConfigurationManager::new()
  
  // Load configuration from file
  let load_result = ConfigurationManager::load_from_file(new_config_manager, temp_file)
  match load_result {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Verify loaded configuration
  let service_name = ConfigurationManager::get(new_config_manager, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  let service_port = ConfigurationManager::get(new_config_manager, "service.port")
  match service_port {
    Some(IntValue(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  let service_debug = ConfigurationManager::get(new_config_manager, "service.debug")
  match service_debug {
    Some(BoolValue(debug)) => assert_true(debug)
    _ => assert_true(false)
  }
}

// Test 7: Dynamic Configuration Updates
test "dynamic configuration updates" {
  let config_manager = ConfigurationManager::new()
  
  // Set initial configuration
  ConfigurationManager::set(config_manager, "service.port", IntValue(8080))
  ConfigurationManager::set(config_manager, "service.timeout", IntValue(5000))
  ConfigurationManager::set(config_manager, "service.retry", IntValue(3))
  
  // Create a service that uses configuration
  let service = TestService::new(config_manager)
  
  // Test service uses initial configuration
  assert_eq(TestService::get_port(service), 8080)
  assert_eq(TestService::get_timeout(service), 5000)
  assert_eq(TestService::get_retry(service), 3)
  
  // Update configuration dynamically
  ConfigurationManager::set(config_manager, "service.port", IntValue(9090))
  ConfigurationManager::set(config_manager, "service.timeout", IntValue(10000))
  
  // Test service reflects updated configuration
  assert_eq(TestService::get_port(service), 9090)
  assert_eq(TestService::get_timeout(service), 10000)
  assert_eq(TestService::get_retry(service), 3) // Unchanged
}

// Test 8: Configuration Environment Overrides
test "configuration environment overrides" {
  let config_manager = ConfigurationManager::new()
  
  // Set base configuration
  ConfigurationManager::set(config_manager, "service.port", IntValue(8080))
  ConfigurationManager::set(config_manager, "service.debug", BoolValue(false))
  ConfigurationManager::set(config_manager, "service.log_level", StringValue("INFO"))
  
  // Set environment overrides
  let env_overrides = [
    ("AZIMUTH_SERVICE_PORT", "9090"),
    ("AZIMUTH_SERVICE_DEBUG", "true"),
    ("AZIMUTH_SERVICE_LOG_LEVEL", "DEBUG")
  ]
  
  // Apply environment overrides
  ConfigurationManager::apply_env_overrides(config_manager, env_overrides, "AZIMUTH_")
  
  // Test environment overrides are applied
  let service_port = ConfigurationManager::get(config_manager, "service.port")
  match service_port {
    Some(IntValue(port)) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  
  let service_debug = ConfigurationManager::get(config_manager, "service.debug")
  match service_debug {
    Some(BoolValue(debug)) => assert_true(debug)
    _ => assert_true(false)
  }
  
  let log_level = ConfigurationManager::get(config_manager, "service.log_level")
  match log_level {
    Some(StringValue(level)) => assert_eq(level, "DEBUG")
    _ => assert_true(false)
  }
}

// Test 9: Configuration Rollback
test "configuration rollback" {
  let config_manager = ConfigurationManager::new()
  
  // Set initial configuration
  ConfigurationManager::set(config_manager, "service.port", IntValue(8080))
  ConfigurationManager::set(config_manager, "service.timeout", IntValue(5000))
  ConfigurationManager::set(config_manager, "service.debug", BoolValue(false))
  
  // Create checkpoint
  let checkpoint = ConfigurationManager::create_checkpoint(config_manager)
  
  // Update configuration
  ConfigurationManager::set(config_manager, "service.port", IntValue(9090))
  ConfigurationManager::set(config_manager, "service.timeout", IntValue(10000))
  ConfigurationManager::set(config_manager, "service.debug", BoolValue(true))
  
  // Verify configuration was updated
  let service_port = ConfigurationManager::get(config_manager, "service.port")
  match service_port {
    Some(IntValue(port)) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  
  // Rollback to checkpoint
  ConfigurationManager::rollback_to_checkpoint(config_manager, checkpoint)
  
  // Verify configuration was restored
  let restored_port = ConfigurationManager::get(config_manager, "service.port")
  match restored_port {
    Some(IntValue(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  let restored_timeout = ConfigurationManager::get(config_manager, "service.timeout")
  match restored_timeout {
    Some(IntValue(timeout)) => assert_eq(timeout, 5000)
    _ => assert_true(false)
  }
  
  let restored_debug = ConfigurationManager::get(config_manager, "service.debug")
  match restored_debug {
    Some(BoolValue(debug)) => assert_false(debug)
    _ => assert_true(false)
  }
}

// Test 10: Configuration Templates
test "configuration templates" {
  let config_manager = ConfigurationManager::new()
  
  // Create configuration template
  let template = ConfigurationTemplate::new("web-service")
  ConfigurationTemplate::add_field(template, "service.name", StringValue("default-service"))
  ConfigurationTemplate::add_field(template, "service.port", IntValue(8080))
  ConfigurationTemplate::add_field(template, "service.debug", BoolValue(false))
  ConfigurationTemplate::add_field(template, "service.host", StringValue("0.0.0.0"))
  
  // Register template
  ConfigurationManager::register_template(config_manager, template)
  
  // Apply template with overrides
  let overrides = [
    ("service.name", StringValue("azimuth-web")),
    ("service.port", IntValue(9090))
  ]
  
  ConfigurationManager::apply_template(config_manager, "web-service", overrides)
  
  // Verify template was applied with overrides
  let service_name = ConfigurationManager::get(config_manager, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-web")
    _ => assert_true(false)
  }
  
  let service_port = ConfigurationManager::get(config_manager, "service.port")
  match service_port {
    Some(IntValue(port)) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  
  let service_debug = ConfigurationManager::get(config_manager, "service.debug")
  match service_debug {
    Some(BoolValue(debug)) => assert_false(debug)
    _ => assert_true(false)
  }
  
  let service_host = ConfigurationManager::get(config_manager, "service.host")
  match service_host {
    Some(StringValue(host)) => assert_eq(host, "0.0.0.0")
    _ => assert_true(false)
  }
}