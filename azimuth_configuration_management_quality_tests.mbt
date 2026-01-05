// Azimuth Telemetry System - Configuration Management Quality Tests
// This file contains comprehensive test cases for configuration management functionality

// Test 1: Basic Configuration Loading
test "basic configuration loading" {
  let config_manager = ConfigurationManager::new()
  
  // Test loading configuration from file
  let config_path = "test_config.json"
  let config_content = {
    "service": {
      "name": "test_service",
      "version": "1.0.0"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.5
    },
    "logging": {
      "level": "info",
      "format": "json"
    }
  }
  
  // Write test config file
  FileManager::write_file(config_path, JsonSerializer::serialize(config_content))
  
  // Load configuration
  let load_result = ConfigurationManager::load_from_file(config_manager, config_path)
  match load_result {
    Ok(_) => assert_true(true)  // Should succeed with valid config
    Err(error) => assert_true(false)
  }
  
  // Verify configuration values
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "test_service")
    None => assert_true(false)
  }
  
  let service_version = ConfigurationManager::get_string(config_manager, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  
  let telemetry_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.5)
    None => assert_true(false)
  }
  
  // Clean up test file
  FileManager::delete_file(config_path)
}

// Test 2: Configuration Validation
test "configuration validation" {
  let config_manager = ConfigurationManager::new()
  
  // Define validation rules
  ConfigurationManager::add_validation_rule(config_manager, "service.name", ValidationRule::Required)
  ConfigurationManager::add_validation_rule(config_manager, "service.version", ValidationRule::Required)
  ConfigurationManager::add_validation_rule(config_manager, "telemetry.sampling_rate", ValidationRule::Range(0.0, 1.0))
  ConfigurationManager::add_validation_rule(config_manager, "logging.level", ValidationRule::Enum(["debug", "info", "warn", "error"]))
  
  // Test valid configuration
  let valid_config = {
    "service": {
      "name": "valid_service",
      "version": "1.0.0"
    },
    "telemetry": {
      "sampling_rate": 0.5
    },
    "logging": {
      "level": "info"
    }
  }
  
  let validation_result = ConfigurationManager::validate(config_manager, valid_config)
  match validation_result {
    Ok(_) => assert_true(true)  // Should pass validation
    Err(_) => assert_true(false)
  }
  
  // Test invalid configuration (missing required field)
  let invalid_config1 = {
    "service": {
      "version": "1.0.0"  // Missing name
    },
    "telemetry": {
      "sampling_rate": 0.5
    }
  }
  
  let validation_result1 = ConfigurationManager::validate(config_manager, invalid_config1)
  match validation_result1 {
    Ok(_) => assert_true(false)  // Should fail validation
    Err(errors) => assert_true(errors.contains("service.name") && errors.contains("required"))
  }
  
  // Test invalid configuration (out of range)
  let invalid_config2 = {
    "service": {
      "name": "invalid_service",
      "version": "1.0.0"
    },
    "telemetry": {
      "sampling_rate": 1.5  // Out of range
    }
  }
  
  let validation_result2 = ConfigurationManager::validate(config_manager, invalid_config2)
  match validation_result2 {
    Ok(_) => assert_true(false)  // Should fail validation
    Err(errors) => assert_true(errors.contains("telemetry.sampling_rate") && errors.contains("range"))
  }
  
  // Test invalid configuration (invalid enum)
  let invalid_config3 = {
    "service": {
      "name": "invalid_service",
      "version": "1.0.0"
    },
    "logging": {
      "level": "trace"  // Not in enum
    }
  }
  
  let validation_result3 = ConfigurationManager::validate(config_manager, invalid_config3)
  match validation_result3 {
    Ok(_) => assert_true(false)  // Should fail validation
    Err(errors) => assert_true(errors.contains("logging.level") && errors.contains("enum"))
  }
}

// Test 3: Configuration Defaults
test "configuration defaults" {
  let config_manager = ConfigurationManager::new()
  
  // Set default values
  ConfigurationManager::set_default(config_manager, "service.name", "default_service")
  ConfigurationManager::set_default(config_manager, "service.version", "1.0.0")
  ConfigurationManager::set_default(config_manager, "telemetry.enabled", true)
  ConfigurationManager::set_default(config_manager, "telemetry.sampling_rate", 0.1)
  ConfigurationManager::set_default(config_manager, "logging.level", "info")
  
  // Test getting default values when no config is loaded
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "default_service")
    None => assert_true(false)
  }
  
  let telemetry_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.1)
    None => assert_true(false)
  }
  
  // Load partial configuration
  let partial_config = {
    "service": {
      "name": "partial_service"  // Override default
    },
    "telemetry": {
      "sampling_rate": 0.5  // Override default
    }
  }
  
  ConfigurationManager::load_from_object(config_manager, partial_config)
  
  // Verify overridden values and defaults
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "partial_service")  // Overridden
    None => assert_true(false)
  }
  
  let service_version = ConfigurationManager::get_string(config_manager, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0")  // Default
    None => assert_true(false)
  }
  
  let telemetry_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)  // Default
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.5)  // Overridden
    None => assert_true(false)
  }
}

// Test 4: Configuration Updates and Reloading
test "configuration updates and reloading" {
  let config_manager = ConfigurationManager::new()
  let config_path = "dynamic_config.json"
  
  // Create initial config
  let initial_config = {
    "service": {
      "name": "initial_service",
      "version": "1.0.0"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1
    }
  }
  
  FileManager::write_file(config_path, JsonSerializer::serialize(initial_config))
  ConfigurationManager::load_from_file(config_manager, config_path)
  
  // Verify initial values
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "initial_service")
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.1)
    None => assert_true(false)
  }
  
  // Update config file
  let updated_config = {
    "service": {
      "name": "updated_service",
      "version": "2.0.0"
    },
    "telemetry": {
      "enabled": false,
      "sampling_rate": 0.5
    }
  }
  
  FileManager::write_file(config_path, JsonSerializer::serialize(updated_config))
  
  // Reload configuration
  let reload_result = ConfigurationManager::reload(config_manager)
  match reload_result {
    Ok(_) => assert_true(true)  // Should succeed
    Err(_) => assert_true(false)
  }
  
  // Verify updated values
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "updated_service")
    None => assert_true(false)
  }
  
  let service_version = ConfigurationManager::get_string(config_manager, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "2.0.0")
    None => assert_true(false)
  }
  
  let telemetry_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_false(enabled)
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.5)
    None => assert_true(false)
  }
  
  // Clean up test file
  FileManager::delete_file(config_path)
}

// Test 5: Configuration Watchers
test "configuration watchers" {
  let config_manager = ConfigurationManager::new()
  let config_path = "watched_config.json"
  
  // Create initial config
  let initial_config = {
    "service": {
      "name": "watched_service"
    },
    "telemetry": {
      "sampling_rate": 0.1
    }
  }
  
  FileManager::write_file(config_path, JsonSerializer::serialize(initial_config))
  ConfigurationManager::load_from_file(config_manager, config_path)
  
  // Set up configuration watchers
  let mut service_name_changes = []
  let mut sampling_rate_changes = []
  
  ConfigurationManager::add_watcher(config_manager, "service.name", func(old_value, new_value) {
    service_name_changes.push((old_value, new_value))
  })
  
  ConfigurationManager::add_watcher(config_manager, "telemetry.sampling_rate", func(old_value, new_value) {
    sampling_rate_changes.push((old_value, new_value))
  })
  
  // Update configuration
  let updated_config = {
    "service": {
      "name": "updated_watched_service"
    },
    "telemetry": {
      "sampling_rate": 0.5
    }
  }
  
  FileManager::write_file(config_path, JsonSerializer::serialize(updated_config))
  ConfigurationManager::reload(config_manager)
  
  // Verify watchers were called
  assert_eq(service_name_changes.length(), 1)
  match service_name_changes[0] {
    (Some(old), Some(new)) => {
      assert_eq(old, "watched_service")
      assert_eq(new, "updated_watched_service")
    }
    _ => assert_true(false)
  }
  
  assert_eq(sampling_rate_changes.length(), 1)
  match sampling_rate_changes[0] {
    (Some(old), Some(new)) => {
      assert_eq(old, 0.1)
      assert_eq(new, 0.5)
    }
    _ => assert_true(false)
  }
  
  // Clean up test file
  FileManager::delete_file(config_path)
}

// Test 6: Configuration Profiles
test "configuration profiles" {
  let config_manager = ConfigurationManager::new()
  
  // Create base configuration
  let base_config = {
    "service": {
      "name": "base_service",
      "version": "1.0.0"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1
    },
    "logging": {
      "level": "info"
    }
  }
  
  // Create development profile
  let dev_profile = {
    "service": {
      "name": "dev_service"
    },
    "telemetry": {
      "sampling_rate": 1.0
    },
    "logging": {
      "level": "debug"
    }
  }
  
  // Create production profile
  let prod_profile = {
    "telemetry": {
      "sampling_rate": 0.01
    },
    "logging": {
      "level": "warn"
    }
  }
  
  // Load base configuration
  ConfigurationManager::load_from_object(config_manager, base_config)
  
  // Add profiles
  ConfigurationManager::add_profile(config_manager, "development", dev_profile)
  ConfigurationManager::add_profile(config_manager, "production", prod_profile)
  
  // Test development profile
  ConfigurationManager::activate_profile(config_manager, "development")
  
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "dev_service")  // Overridden by profile
    None => assert_true(false)
  }
  
  let service_version = ConfigurationManager::get_string(config_manager, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0")  // From base config
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 1.0)  // Overridden by profile
    None => assert_true(false)
  }
  
  let logging_level = ConfigurationManager::get_string(config_manager, "logging.level")
  match logging_level {
    Some(level) => assert_eq(level, "debug")  // Overridden by profile
    None => assert_true(false)
  }
  
  // Switch to production profile
  ConfigurationManager::activate_profile(config_manager, "production")
  
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "base_service")  // From base config
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.01)  // Overridden by profile
    None => assert_true(false)
  }
  
  let logging_level = ConfigurationManager::get_string(config_manager, "logging.level")
  match logging_level {
    Some(level) => assert_eq(level, "warn")  // Overridden by profile
    None => assert_true(false)
  }
}

// Test 7: Configuration Encryption
test "configuration encryption" {
  let config_manager = ConfigurationManager::new()
  let encryption_key = "test_encryption_key_12345"
  
  // Enable encryption
  ConfigurationManager::enable_encryption(config_manager, encryption_key)
  
  // Set sensitive configuration values
  ConfigurationManager::set_string(config_manager, "database.password", "secret_password", true)  // Encrypt this value
  ConfigurationManager::set_string(config_manager, "api.key", "secret_api_key", true)  // Encrypt this value
  ConfigurationManager::set_string(config_manager, "service.name", "public_service", false)  // Don't encrypt this value
  
  // Verify encrypted values
  let db_password = ConfigurationManager::get_string(config_manager, "database.password")
  match db_password {
    Some(password) => assert_eq(password, "secret_password")  // Should be decrypted automatically
    None => assert_true(false)
  }
  
  let api_key = ConfigurationManager::get_string(config_manager, "api.key")
  match api_key {
    Some(key) => assert_eq(key, "secret_api_key")  // Should be decrypted automatically
    None => assert_true(false)
  }
  
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "public_service")
    None => assert_true(false)
  }
  
  // Export configuration
  let exported_config = ConfigurationManager::export(config_manager)
  
  // Verify sensitive values are encrypted in export
  assert_true(exported_config.contains("public_service"))  // Public value should be visible
  assert_false(exported_config.contains("secret_password"))  // Encrypted value should not be visible
  assert_false(exported_config.contains("secret_api_key"))  // Encrypted value should not be visible
  
  // Import configuration with different key (should fail)
  let wrong_key = "wrong_encryption_key_67890"
  let import_result = ConfigurationManager::import_with_key(config_manager, exported_config, wrong_key)
  match import_result {
    Ok(_) => assert_true(false)  // Should fail with wrong key
    Err(error) => assert_true(error.contains("key") || error.contains("decrypt"))
  }
  
  // Import configuration with correct key (should succeed)
  let import_result = ConfigurationManager::import_with_key(config_manager, exported_config, encryption_key)
  match import_result {
    Ok(_) => assert_true(true)  // Should succeed with correct key
    Err(_) => assert_true(false)
  }
  
  // Verify values after import
  let db_password = ConfigurationManager::get_string(config_manager, "database.password")
  match db_password {
    Some(password) => assert_eq(password, "secret_password")
    None => assert_true(false)
  }
}

// Test 8: Configuration Environment Variables Override
test "configuration environment variables override" {
  let config_manager = ConfigurationManager::new()
  
  // Set up environment variable overrides
  Environment::set("AZIMUTH_SERVICE_NAME", "env_service")
  Environment::set("AZIMUTH_TELEMETRY_SAMPLING_RATE", "0.75")
  Environment::set("AZIMUTH_LOGGING_LEVEL", "error")
  
  // Configure environment variable mapping
  ConfigurationManager::add_env_override(config_manager, "service.name", "AZIMUTH_SERVICE_NAME")
  ConfigurationManager::add_env_override(config_manager, "telemetry.sampling_rate", "AZIMUTH_TELEMETRY_SAMPLING_RATE")
  ConfigurationManager::add_env_override(config_manager, "logging.level", "AZIMUTH_LOGGING_LEVEL")
  
  // Load configuration
  let config = {
    "service": {
      "name": "file_service",  // Will be overridden by env
      "version": "1.0.0"
    },
    "telemetry": {
      "sampling_rate": 0.1  // Will be overridden by env
    },
    "logging": {
      "level": "info"  // Will be overridden by env
    }
  }
  
  ConfigurationManager::load_from_object(config_manager, config)
  
  // Verify environment variables override file values
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "env_service")  // From environment
    None => assert_true(false)
  }
  
  let service_version = ConfigurationManager::get_string(config_manager, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0")  // From file
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.75)  // From environment
    None => assert_true(false)
  }
  
  let logging_level = ConfigurationManager::get_string(config_manager, "logging.level")
  match logging_level {
    Some(level) => assert_eq(level, "error")  // From environment
    None => assert_true(false)
  }
  
  // Clean up environment variables
  Environment::unset("AZIMUTH_SERVICE_NAME")
  Environment::unset("AZIMUTH_TELEMETRY_SAMPLING_RATE")
  Environment::unset("AZIMUTH_LOGGING_LEVEL")
}

// Test 9: Configuration Type Conversion
test "configuration type conversion" {
  let config_manager = ConfigurationManager::new()
  
  // Load configuration with different types
  let config = {
    "string_value": "test_string",
    "int_value": "42",
    "float_value": "3.14",
    "bool_value": "true",
    "array_value": "[1, 2, 3]",
    "object_value": "{\"key\": \"value\"}"
  }
  
  ConfigurationManager::load_from_object(config_manager, config)
  
  // Test string value
  let string_value = ConfigurationManager::get_string(config_manager, "string_value")
  match string_value {
    Some(value) => assert_eq(value, "test_string")
    None => assert_true(false)
  }
  
  // Test int conversion from string
  let int_value = ConfigurationManager::get_int(config_manager, "int_value")
  match int_value {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  // Test float conversion from string
  let float_value = ConfigurationManager::get_float(config_manager, "float_value")
  match float_value {
    Some(value) => assert_eq(value, 3.14)
    None => assert_true(false)
  }
  
  // Test bool conversion from string
  let bool_value = ConfigurationManager::get_bool(config_manager, "bool_value")
  match bool_value {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  // Test array conversion from string
  let array_value = ConfigurationManager::get_string_array(config_manager, "array_value")
  match array_value {
    Some(array) => {
      assert_eq(array.length(), 3)
      assert_eq(array[0], "1")
      assert_eq(array[1], "2")
      assert_eq(array[2], "3")
    }
    None => assert_true(false)
  }
  
  // Test object conversion from string
  let object_value = ConfigurationManager::get_string_object(config_manager, "object_value")
  match object_value {
    Some(object) => {
      match object.get("key") {
        Some(value) => assert_eq(value, "value")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test invalid conversion
  let invalid_int = ConfigurationManager::get_int(config_manager, "string_value")
  match invalid_int {
    Some(_) => assert_true(false)  // Should not succeed
    None => assert_true(true)  // Expected to fail
  }
}

// Test 10: Configuration Backup and Restore
test "configuration backup and restore" {
  let config_manager = ConfigurationManager::new()
  let backup_path = "config_backup.json"
  
  // Load initial configuration
  let initial_config = {
    "service": {
      "name": "backup_service",
      "version": "1.0.0"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.1
    }
  }
  
  ConfigurationManager::load_from_object(config_manager, initial_config)
  
  // Create backup
  let backup_result = ConfigurationManager::create_backup(config_manager, backup_path)
  match backup_result {
    Ok(_) => assert_true(true)  // Should succeed
    Err(_) => assert_true(false)
  }
  
  // Modify configuration
  ConfigurationManager::set_string(config_manager, "service.name", "modified_service")
  ConfigurationManager::set_float(config_manager, "telemetry.sampling_rate", 0.5)
  ConfigurationManager::set_bool(config_manager, "telemetry.enabled", false)
  
  // Verify modifications
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "modified_service")
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.5)
    None => assert_true(false)
  }
  
  let telemetry_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_false(enabled)
    None => assert_true(false)
  }
  
  // Restore from backup
  let restore_result = ConfigurationManager::restore_from_backup(config_manager, backup_path)
  match restore_result {
    Ok(_) => assert_true(true)  // Should succeed
    Err(_) => assert_true(false)
  }
  
  // Verify restoration
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "backup_service")
    None => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.1)
    None => assert_true(false)
  }
  
  let telemetry_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  // Clean up backup file
  FileManager::delete_file(backup_path)
}