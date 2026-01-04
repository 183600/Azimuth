// Azimuth Telemetry System - Configuration Management Dynamic Update Tests
// This file contains test cases for configuration management and dynamic update functionality

// Test 1: Basic Configuration Loading and Retrieval
test "basic configuration loading and retrieval" {
  // Create a configuration manager
  let config_manager = ConfigManager::new()
  
  // Load configuration from a map
  let config_data = [
    ("service.name", "azimuth-telemetry"),
    ("service.version", "1.0.0"),
    ("server.port", "8080"),
    ("server.host", "localhost"),
    ("database.url", "postgresql://localhost:5432/azimuth"),
    ("log.level", "info"),
    ("feature.flags.new_ui", "true"),
    ("feature.flags.advanced_analytics", "false")
  ]
  
  ConfigManager::load_from_map(config_manager, config_data)
  
  // Test retrieving configuration values
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ConfigManager::get_string(config_manager, "service.version"), "1.0.0")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 8080)
  assert_eq(ConfigManager::get_string(config_manager, "server.host"), "localhost")
  assert_eq(ConfigManager::get_string(config_manager, "database.url"), "postgresql://localhost:5432/azimuth")
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "info")
  assert_true(ConfigManager::get_bool(config_manager, "feature.flags.new_ui"))
  assert_false(ConfigManager::get_bool(config_manager, "feature.flags.advanced_analytics"))
  
  // Test default values
  assert_eq(ConfigManager::get_string_with_default(config_manager, "nonexistent.key", "default"), "default")
  assert_eq(ConfigManager::get_int_with_default(config_manager, "nonexistent.int", 42), 42)
  assert_true(ConfigManager::get_bool_with_default(config_manager, "nonexistent.bool", true))
  
  // Test checking if key exists
  assert_true(ConfigManager::has_key(config_manager, "service.name"))
  assert_true(ConfigManager::has_key(config_manager, "server.port"))
  assert_false(ConfigManager::has_key(config_manager, "nonexistent.key"))
}

// Test 2: Configuration File Loading
test "configuration file loading" {
  // Create a configuration manager
  let config_manager = ConfigManager::new()
  
  // Create a temporary configuration file
  let config_content = "{
    \"service\": {
      \"name\": \"azimuth-telemetry\",
      \"version\": \"1.0.0\",
      \"environment\": \"production\"
    },
    \"server\": {
      \"port\": 8080,
      \"host\": \"0.0.0.0\",
      \"ssl\": {
        \"enabled\": true,
        \"cert_path\": \"/etc/ssl/certs/azimuth.crt\",
        \"key_path\": \"/etc/ssl/private/azimuth.key\"
      }
    },
    \"database\": {
      \"url\": \"postgresql://db.example.com:5432/azimuth_prod\",
      \"pool_size\": 20,
      \"timeout_ms\": 5000
    },
    \"logging\": {
      \"level\": \"warn\",
      \"format\": \"json\",
      \"outputs\": [\"console\", \"file\"]
    },
    \"feature_flags\": {
      \"new_ui\": true,
      \"advanced_analytics\": false,
      \"beta_features\": true
    }
  }"
  
  let temp_file = TempFile::create_with_content(config_content)
  
  // Load configuration from file
  let load_result = ConfigManager::load_from_file(config_manager, TempFile::path(temp_file))
  assert_true(ConfigManager::is_successful(load_result))
  
  // Test nested configuration values
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ConfigManager::get_string(config_manager, "service.version"), "1.0.0")
  assert_eq(ConfigManager::get_string(config_manager, "service.environment"), "production")
  
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 8080)
  assert_eq(ConfigManager::get_string(config_manager, "server.host"), "0.0.0.0")
  assert_true(ConfigManager::get_bool(config_manager, "server.ssl.enabled"))
  assert_eq(ConfigManager::get_string(config_manager, "server.ssl.cert_path"), "/etc/ssl/certs/azimuth.crt")
  
  assert_eq(ConfigManager::get_string(config_manager, "database.url"), "postgresql://db.example.com:5432/azimuth_prod")
  assert_eq(ConfigManager::get_int(config_manager, "database.pool_size"), 20)
  assert_eq(ConfigManager::get_int(config_manager, "database.timeout_ms"), 5000)
  
  assert_eq(ConfigManager::get_string(config_manager, "logging.level"), "warn")
  assert_eq(ConfigManager::get_string(config_manager, "logging.format"), "json")
  
  // Test array values
  let log_outputs = ConfigManager::get_string_array(config_manager, "logging.outputs")
  assert_eq(log_outputs.length(), 2)
  assert_eq(log_outputs[0], "console")
  assert_eq(log_outputs[1], "file")
  
  // Clean up
  TempFile::delete(temp_file)
}

// Test 3: Dynamic Configuration Updates
test "dynamic configuration updates" {
  // Create a configuration manager
  let config_manager = ConfigManager::new()
  
  // Load initial configuration
  let initial_config = [
    ("service.name", "azimuth-telemetry"),
    ("server.port", "8080"),
    ("log.level", "info"),
    ("feature.flags.new_ui", "true")
  ]
  
  ConfigManager::load_from_map(config_manager, initial_config)
  
  // Verify initial values
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 8080)
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "info")
  assert_true(ConfigManager::get_bool(config_manager, "feature.flags.new_ui"))
  
  // Register configuration change listeners
  let mut change_events = []
  
  ConfigManager::add_change_listener(config_manager, "service.name", |old_value, new_value| {
    change_events.push(("service.name", old_value, new_value))
  })
  
  ConfigManager::add_change_listener(config_manager, "server.port", |old_value, new_value| {
    change_events.push(("server.port", old_value, new_value))
  })
  
  // Update configuration values
  ConfigManager::set_string(config_manager, "service.name", "azimuth-telemetry-v2")
  ConfigManager::set_int(config_manager, "server.port", 9090)
  ConfigManager::set_string(config_manager, "log.level", "debug")
  ConfigManager::set_bool(config_manager, "feature.flags.new_ui", false)
  
  // Verify updated values
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry-v2")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 9090)
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "debug")
  assert_false(ConfigManager::get_bool(config_manager, "feature.flags.new_ui"))
  
  // Verify change events were fired
  assert_eq(change_events.length(), 2)  // Only for registered listeners
  
  assert_eq(change_events[0].0, "service.name")
  assert_eq(change_events[0].1, "azimuth-telemetry")  // Old value
  assert_eq(change_events[0].2, "azimuth-telemetry-v2")  // New value
  
  assert_eq(change_events[1].0, "server.port")
  assert_eq(change_events[1].1, "8080")  // Old value
  assert_eq(change_events[1].2, "9090")  // New value
}

// Test 4: Configuration Validation
test "configuration validation" {
  // Create a configuration manager with validation rules
  let config_manager = ConfigManager::new()
  
  // Add validation rules
  ConfigManager::add_validation_rule(config_manager, "server.port", ValidationRule::IntRange(1, 65535))
  ConfigManager::add_validation_rule(config_manager, "log.level", ValidationRule::Enum(["debug", "info", "warn", "error"]))
  ConfigManager::add_validation_rule(config_manager, "service.name", ValidationRule::NonEmpty)
  ConfigManager::add_validation_rule(config_manager, "database.pool_size", ValidationRule::MinValue(1))
  
  // Load valid configuration
  let valid_config = [
    ("service.name", "azimuth-telemetry"),
    ("server.port", "8080"),
    ("log.level", "info"),
    ("database.pool_size", "10")
  ]
  
  let valid_result = ConfigManager::load_with_validation(config_manager, valid_config)
  assert_true(ConfigManager::is_valid(valid_result))
  
  // Try to load invalid configuration
  let invalid_config = [
    ("service.name", ""),  // Empty string
    ("server.port", "70000"),  // Out of range
    ("log.level", "invalid"),  // Not in enum
    ("database.pool_size", "0")  // Below minimum
  ]
  
  let invalid_result = ConfigManager::load_with_validation(config_manager, invalid_config)
  assert_false(ConfigManager::is_valid(invalid_result))
  
  // Check validation errors
  let validation_errors = ConfigManager::get_validation_errors(invalid_result)
  assert_eq(validation_errors.length(), 4)
  
  let service_name_error = ConfigManager::get_error_for_key(invalid_result, "service.name")
  match service_name_error {
    Some(error) => assert_eq(ValidationError::get_message(error), "Value cannot be empty")
    None => assert_true(false)
  }
  
  let server_port_error = ConfigManager::get_error_for_key(invalid_result, "server.port")
  match server_port_error {
    Some(error) => assert_eq(ValidationError::get_message(error), "Value must be between 1 and 65535")
    None => assert_true(false)
  }
  
  // Test validation on update
  ConfigManager::load_with_validation(config_manager, valid_config)  // Load valid config first
  
  // Try to update with invalid value
  let update_result = ConfigManager::set_with_validation(config_manager, "server.port", "99999")
  assert_false(ConfigManager::is_successful(update_result))
  
  // Verify value was not updated
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 8080)
  
  // Update with valid value
  let valid_update = ConfigManager::set_with_validation(config_manager, "server.port", "9090")
  assert_true(ConfigManager::is_successful(valid_update))
  
  // Verify value was updated
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 9090)
}

// Test 5: Configuration Environment Override
test "configuration environment override" {
  // Create a configuration manager with environment override
  let config_manager = ConfigManager::new()
  ConfigManager::enable_environment_override(config_manager, true)
  ConfigManager::set_env_prefix(config_manager, "AZIMUTH_")
  
  // Load base configuration
  let base_config = [
    ("service.name", "azimuth-telemetry"),
    ("server.port", "8080"),
    ("log.level", "info"),
    ("database.url", "postgresql://localhost:5432/azimuth")
  ]
  
  ConfigManager::load_from_map(config_manager, base_config)
  
  // Set environment variables
  Env::set("AZIMUTH_SERVICE_NAME", "azimuth-production")
  Env::set("AZIMUTH_SERVER_PORT", "9090")
  Env::set("AZIMUTH_LOG_LEVEL", "debug")
  Env::set("AZIMUTH_FEATURE_FLAGS_NEW_UI", "true")
  
  // Reload configuration with environment overrides
  ConfigManager::reload_with_env_overrides(config_manager)
  
  // Verify environment overrides were applied
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-production")  // Overridden
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 9090)  // Overridden
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "debug")  // Overridden
  assert_eq(ConfigManager::get_string(config_manager, "database.url"), "postgresql://localhost:5432/azimuth")  // Not overridden
  
  // Test new key from environment
  assert_true(ConfigManager::has_key(config_manager, "feature.flags.new_ui"))
  assert_true(ConfigManager::get_bool(config_manager, "feature.flags.new_ui"))
  
  // Test environment-specific configuration
  ConfigManager::set_environment(config_manager, "production")
  
  let prod_config = [
    ("service.name", "azimuth-telemetry"),
    ("server.port", "8080"),
    ("debug.enabled", "false")
  ]
  
  ConfigManager::load_from_map(config_manager, prod_config)
  
  // Load environment-specific overrides
  let prod_overrides = [
    ("server.port", "443"),
    ("debug.enabled", "false"),
    ("monitoring.enabled", "true")
  ]
  
  ConfigManager::load_environment_overrides(config_manager, "production", prod_overrides)
  
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 443)  // Overridden
  assert_false(ConfigManager::get_bool(config_manager, "debug.enabled"))  // Overridden
  assert_true(ConfigManager::get_bool(config_manager, "monitoring.enabled"))  // New key
  
  // Clean up environment variables
  Env::unset("AZIMUTH_SERVICE_NAME")
  Env::unset("AZIMUTH_SERVER_PORT")
  Env::unset("AZIMUTH_LOG_LEVEL")
  Env::unset("AZIMUTH_FEATURE_FLAGS_NEW_UI")
}

// Test 6: Configuration Hot Reload
test "configuration hot reload" {
  // Create a configuration manager with hot reload enabled
  let config_manager = ConfigManager::new()
  ConfigManager::enable_hot_reload(config_manager, true)
  
  // Create a temporary configuration file
  let initial_config = "{
    \"service\": {
      \"name\": \"azimuth-telemetry\",
      \"version\": \"1.0.0\"
    },
    \"server\": {
      \"port\": 8080,
      \"host\": \"localhost\"
    }
  }"
  
  let temp_file = TempFile::create_with_content(initial_config)
  let file_path = TempFile::path(temp_file)
  
  // Load initial configuration
  ConfigManager::load_from_file(config_manager, file_path)
  
  // Verify initial values
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ConfigManager::get_string(config_manager, "service.version"), "1.0.0")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 8080)
  assert_eq(ConfigManager::get_string(config_manager, "server.host"), "localhost")
  
  // Register hot reload listener
  let mut reload_events = []
  
  ConfigManager::add_reload_listener(config_manager, |changes| {
    reload_events.push(changes)
  })
  
  // Update configuration file
  let updated_config = "{
    \"service\": {
      \"name\": \"azimuth-telemetry-v2\",
      \"version\": \"1.1.0\"
    },
    \"server\": {
      \"port\": 9090,
      \"host\": \"0.0.0.0\"
    },
    \"feature_flags\": {
      \"new_ui\": true
    }
  }"
  
  TempFile::update_content(temp_file, updated_config)
  
  // Trigger hot reload
  let reload_result = ConfigManager::hot_reload(config_manager)
  assert_true(ConfigManager::is_successful(reload_result))
  
  // Verify values were updated
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry-v2")
  assert_eq(ConfigManager::get_string(config_manager, "service.version"), "1.1.0")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 9090)
  assert_eq(ConfigManager::get_string(config_manager, "server.host"), "0.0.0.0")
  assert_true(ConfigManager::has_key(config_manager, "feature_flags.new_ui"))
  assert_true(ConfigManager::get_bool(config_manager, "feature_flags.new_ui"))
  
  // Verify reload event was fired
  assert_eq(reload_events.length(), 1)
  
  let changes = reload_events[0]
  assert_true(changes.contains("service.name"))
  assert_true(changes.contains("service.version"))
  assert_true(changes.contains("server.port"))
  assert_true(changes.contains("server.host"))
  assert_true(changes.contains("feature_flags.new_ui"))
  
  // Test reload with invalid configuration
  let invalid_config = "{
    \"service\": {
      \"name\": \"azimuth-telemetry\"
    },
    \"server\": {
      \"port\": \"invalid_port\"
    }
  }"
  
  TempFile::update_content(temp_file, invalid_config)
  
  let invalid_reload_result = ConfigManager::hot_reload(config_manager)
  assert_false(ConfigManager::is_successful(invalid_reload_result))
  
  // Verify values were not changed
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry-v2")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 9090)
  
  // Clean up
  TempFile::delete(temp_file)
}

// Test 7: Configuration Versioning and Rollback
test "configuration versioning and rollback" {
  // Create a configuration manager with versioning enabled
  let config_manager = ConfigManager::new()
  ConfigManager::enable_versioning(config_manager, true)
  ConfigManager::set_max_versions(config_manager, 5)
  
  // Load initial configuration (version 1)
  let v1_config = [
    ("service.name", "azimuth-telemetry"),
    ("server.port", "8080"),
    ("log.level", "info")
  ]
  
  ConfigManager::load_from_map(config_manager, v1_config)
  
  // Verify version 1
  assert_eq(ConfigManager::get_current_version(config_manager), 1)
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 8080)
  
  // Update configuration (version 2)
  ConfigManager::set_string(config_manager, "service.name", "azimuth-telemetry-v2")
  ConfigManager::set_int(config_manager, "server.port", 9090)
  
  // Verify version 2
  assert_eq(ConfigManager::get_current_version(config_manager), 2)
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry-v2")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 9090)
  
  // Update configuration again (version 3)
  ConfigManager::set_string(config_manager, "service.name", "azimuth-telemetry-v3")
  ConfigManager::set_string(config_manager, "log.level", "debug")
  
  // Verify version 3
  assert_eq(ConfigManager::get_current_version(config_manager), 3)
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry-v3")
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "debug")
  
  // Test rollback to version 2
  let rollback_result = ConfigManager::rollback_to_version(config_manager, 2)
  assert_true(ConfigManager::is_successful(rollback_result))
  
  // Verify rollback to version 2
  assert_eq(ConfigManager::get_current_version(config_manager), 2)
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry-v2")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 9090)
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "info")  // From version 2
  
  // Create a new version after rollback (version 4)
  ConfigManager::set_string(config_manager, "service.name", "azimuth-telemetry-v4")
  
  // Verify new version
  assert_eq(ConfigManager::get_current_version(config_manager), 4)
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry-v4")
  
  // Test version history
  let version_history = ConfigManager::get_version_history(config_manager)
  assert_eq(version_history.length(), 4)  // Versions 1-4
  
  // Test version diff
  let diff = ConfigManager::diff_versions(config_manager, 3, 4)
  assert_true(diff.contains("service.name"))
  assert_true(diff.contains("log.level"))
  
  // Test exceeding max versions
  ConfigManager::set_string(config_manager, "service.name", "azimuth-telemetry-v5")  // Version 5
  ConfigManager::set_string(config_manager, "service.name", "azimuth-telemetry-v6")  // Version 6
  
  // Should still only keep 5 versions (2-6)
  let updated_history = ConfigManager::get_version_history(config_manager)
  assert_eq(updated_history.length(), 5)  // Versions 2-6
}

// Test 8: Configuration Encryption
test "configuration encryption" {
  // Create a configuration manager with encryption
  let config_manager = ConfigManager::new()
  ConfigManager::enable_encryption(config_manager, true)
  
  // Set encryption key
  let encryption_key = "my-secret-encryption-key-32-chars"
  ConfigManager::set_encryption_key(config_manager, encryption_key)
  
  // Mark sensitive keys
  ConfigManager::mark_sensitive(config_manager, "database.password")
  ConfigManager::mark_sensitive(config_manager, "api.secret")
  ConfigManager::mark_sensitive(config_manager, "auth.token")
  
  // Load configuration with sensitive values
  let config_with_secrets = [
    ("service.name", "azimuth-telemetry"),
    ("database.url", "postgresql://localhost:5432/azimuth"),
    ("database.password", "super-secret-password"),
    ("api.key", "public-api-key"),
    ("api.secret", "secret-api-key"),
    ("auth.token", "jwt-auth-token")
  ]
  
  ConfigManager::load_from_map(config_manager, config_with_secrets)
  
  // Verify sensitive values are encrypted in storage
  let encrypted_password = ConfigManager::get_raw_value(config_manager, "database.password")
  assert_not_eq(encrypted_password, "super-secret-password")  // Should be encrypted
  assert_true(encrypted_password.length() > "super-secret-password".length())  // Encrypted values are longer
  
  // Verify values are decrypted when retrieved
  assert_eq(ConfigManager::get_string(config_manager, "database.password"), "super-secret-password")
  assert_eq(ConfigManager::get_string(config_manager, "api.secret"), "secret-api-key")
  assert_eq(ConfigManager::get_string(config_manager, "auth.token"), "jwt-auth-token")
  
  // Verify non-sensitive values are not encrypted
  let non_encrypted_name = ConfigManager::get_raw_value(config_manager, "service.name")
  assert_eq(non_encrypted_name, "azimuth-telemetry")  // Should not be encrypted
  
  // Test configuration export with encrypted values
  let exported_config = ConfigManager::export_config(config_manager, true)  // Include encrypted
  assert_true(exported_config.contains("database.password"))  // Should contain encrypted field
  
  // Test configuration export without sensitive values
  let safe_export = ConfigManager::export_config_safe(config_manager)  // Exclude sensitive
  assert_false(safe_export.contains("database.password"))  // Should not contain sensitive field
  assert_false(safe_export.contains("api.secret"))
  assert_false(safe_export.contains("auth.token"))
  assert_true(safe_export.contains("service.name"))  // Should contain non-sensitive fields
  
  // Test changing encryption key
  let new_key = "my-new-secret-encryption-key-32-chars"
  let reencrypt_result = ConfigManager::change_encryption_key(config_manager, new_key)
  assert_true(ConfigManager::is_successful(reencrypt_result))
  
  // Verify values are still accessible with new key
  assert_eq(ConfigManager::get_string(config_manager, "database.password"), "super-secret-password")
  assert_eq(ConfigManager::get_string(config_manager, "api.secret"), "secret-api-key")
}

// Test 9: Configuration Templates and Inheritance
test "configuration templates and inheritance" {
  // Create a configuration manager
  let config_manager = ConfigManager::new()
  
  // Define base template
  let base_template = [
    ("service.name", "azimuth-telemetry"),
    ("service.version", "1.0.0"),
    ("server.port", "8080"),
    ("server.host", "localhost"),
    ("log.level", "info"),
    ("log.format", "json")
  ]
  
  ConfigManager::create_template(config_manager, "base", base_template)
  
  // Define production template that extends base
  let production_overrides = [
    ("server.port", "443"),
    ("server.host", "0.0.0.0"),
    ("log.level", "warn"),
    ("monitoring.enabled", "true"),
    ("ssl.enabled", "true")
  ]
  
  ConfigManager::create_template_with_inheritance(config_manager, "production", "base", production_overrides)
  
  // Define development template that extends base
  let development_overrides = [
    ("server.port", "3000"),
    ("log.level", "debug"),
    ("hot_reload.enabled", "true"),
    ("debug.enabled", "true")
  ]
  
  ConfigManager::create_template_with_inheritance(config_manager, "development", "base", development_overrides)
  
  // Test loading from base template
  ConfigManager::load_from_template(config_manager, "base")
  
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ConfigManager::get_string(config_manager, "service.version"), "1.0.0")
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 8080)
  assert_eq(ConfigManager::get_string(config_manager, "server.host"), "localhost")
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "info")
  assert_eq(ConfigManager::get_string(config_manager, "log.format"), "json")
  
  // Test loading from production template
  ConfigManager::load_from_template(config_manager, "production")
  
  // Should have base values + production overrides
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry")  // From base
  assert_eq(ConfigManager::get_string(config_manager, "service.version"), "1.0.0")        // From base
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 443)                  // Override
  assert_eq(ConfigManager::get_string(config_manager, "server.host"), "0.0.0.0")         // Override
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "warn")               // Override
  assert_eq(ConfigManager::get_string(config_manager, "log.format"), "json")             // From base
  assert_true(ConfigManager::get_bool(config_manager, "monitoring.enabled"))            // Production only
  assert_true(ConfigManager::get_bool(config_manager, "ssl.enabled"))                   // Production only
  
  // Test loading from development template
  ConfigManager::load_from_template(config_manager, "development")
  
  // Should have base values + development overrides
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry")  // From base
  assert_eq(ConfigManager::get_string(config_manager, "service.version"), "1.0.0")        // From base
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 3000)                  // Override
  assert_eq(ConfigManager::get_string(config_manager, "server.host"), "localhost")        // From base
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "debug")              // Override
  assert_eq(ConfigManager::get_string(config_manager, "log.format"), "json")              // From base
  assert_true(ConfigManager::get_bool(config_manager, "hot_reload.enabled"))              // Development only
  assert_true(ConfigManager::get_bool(config_manager, "debug.enabled"))                   // Development only
  
  // Test multi-level inheritance
  let staging_overrides = [
    ("server.host", "staging.example.com"),
    ("log.level", "info")
  ]
  
  ConfigManager::create_template_with_inheritance(config_manager, "staging", "production", staging_overrides)
  ConfigManager::load_from_template(config_manager, "staging")
  
  // Should have base + production + staging overrides
  assert_eq(ConfigManager::get_string(config_manager, "service.name"), "azimuth-telemetry")  // From base
  assert_eq(ConfigManager::get_int(config_manager, "server.port"), 443)                     // From production
  assert_eq(ConfigManager::get_string(config_manager, "server.host"), "staging.example.com") // Staging override
  assert_eq(ConfigManager::get_string(config_manager, "log.level"), "info")                 // Staging override
  assert_true(ConfigManager::get_bool(config_manager, "ssl.enabled"))                       // From production
}

// Test 10: Configuration Metrics and Monitoring
test "configuration metrics and monitoring" {
  // Create a configuration manager with metrics enabled
  let config_manager = ConfigManager::new()
  ConfigManager::enable_metrics(config_manager, true)
  
  // Load initial configuration
  let initial_config = [
    ("service.name", "azimuth-telemetry"),
    ("server.port", "8080"),
    ("log.level", "info")
  ]
  
  ConfigManager::load_from_map(config_manager, initial_config)
  
  // Perform various configuration operations
  for i in 0..10 {
    ConfigManager::set_string(config_manager, "temp.key" + i.to_string(), "value" + i.to_string())
  }
  
  // Update some values
  ConfigManager::set_int(config_manager, "server.port", 9090)
  ConfigManager::set_string(config_manager, "log.level", "debug")
  
  // Retrieve some values
  ConfigManager::get_string(config_manager, "service.name")
  ConfigManager::get_int(config_manager, "server.port")
  ConfigManager::get_string(config_manager, "log.level")
  
  // Test non-existent key
  ConfigManager::get_string_with_default(config_manager, "nonexistent.key", "default")
  
  // Get configuration metrics
  let metrics = ConfigManager::get_metrics(config_manager)
  
  // Verify metrics
  assert_eq(ConfigMetrics::get_total_keys(metrics), 13)  // 3 initial + 10 temp keys
  assert_eq(ConfigMetrics::get_set_operations(metrics), 12)  // 3 initial + 10 temp + 2 updates
  assert_eq(ConfigMetrics::get_get_operations(metrics), 4)   // 3 gets + 1 get with default
  
  // Test key-specific metrics
  let service_name_metrics = ConfigManager::get_key_metrics(config_manager, "service.name")
  assert_eq(KeyMetrics::get_get_count(service_name_metrics), 1)
  assert_eq(KeyMetrics::get_set_count(service_name_metrics), 1)
  
  let server_port_metrics = ConfigManager::get_key_metrics(config_manager, "server.port")
  assert_eq(KeyMetrics::get_get_count(server_port_metrics), 1)
  assert_eq(KeyMetrics::get_set_count(server_port_metrics), 2)  // Initial load + update
  
  // Test most accessed keys
  let most_accessed = ConfigManager::get_most_accessed_keys(config_manager, 5)
  assert_true(most_accessed.length() > 0)
  assert_eq(most_accessed[0].0, "service.name")  // Most accessed
  
  // Test configuration change history
  let change_history = ConfigManager::get_change_history(config_manager)
  assert_true(change_history.length() >= 2)  // At least port and log level changes
  
  // Verify change details
  let port_change = change_history.find(|change| change.key == "server.port")
  match port_change {
    Some(change) => {
      assert_eq(change.old_value, "8080")
      assert_eq(change.new_value, "9090")
      assert_true(change.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  // Test configuration performance metrics
  let performance_metrics = ConfigManager::get_performance_metrics(config_manager)
  assert_true(PerformanceMetrics::get_avg_get_time(performance_metrics) >= 0)
  assert_true(PerformanceMetrics::get_avg_set_time(performance_metrics) >= 0)
  
  // Test configuration health metrics
  let health_metrics = ConfigManager::get_health_metrics(config_manager)
  assert_true(HealthMetrics::is_healthy(health_metrics))
  assert_eq(HealthMetrics::get_last_load_time(health_metrics), ConfigManager::get_last_load_time(config_manager))
  assert_eq(HealthMetrics::get_last_reload_time(health_metrics), ConfigManager::get_last_reload_time(config_manager))
}