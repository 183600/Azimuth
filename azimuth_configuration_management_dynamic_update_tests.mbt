// Azimuth Telemetry System - Configuration Management Dynamic Update Tests
// This file contains comprehensive test cases for configuration management and dynamic updates

// Test 1: Basic Configuration Loading and Validation
test "basic configuration loading and validation" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Load configuration from different sources
  let file_config = ConfigSource::file("config/azimuth.json")
  let env_config = ConfigSource::environment()
  let command_line_config = ConfigSource::command_line()
  
  // Add configuration sources with priority
  ConfigurationManager::add_source(config_manager, file_config, 1) // Lowest priority
  ConfigurationManager::add_source(config_manager, env_config, 2) // Medium priority
  ConfigurationManager::add_source(config_manager, command_line_config, 3) // Highest priority
  
  // Load configuration
  let load_result = ConfigurationManager::load(config_manager)
  match load_result {
    Ok(_) => assert_true(true),
    Error(e) => {
      // In test environment, config file might not exist
      assert_true(Error::is_file_error(e))
    }
  }
  
  // Test configuration validation
  let validation_rules = [
    ConfigValidationRule::required("service.name"),
    ConfigValidationRule::type_check("service.port", "integer"),
    ConfigValidationRule::range_check("service.port", 1, 65535),
    ConfigValidationRule::required("telemetry.enabled"),
    ConfigValidationRule::type_check("telemetry.enabled", "boolean"),
    ConfigValidationRule::one_of("telemetry.level", ["debug", "info", "warn", "error"])
  ]
  
  let validation_result = ConfigurationManager::validate(config_manager, validation_rules)
  match validation_result {
    Ok(_) => assert_true(true),
    Error(errors) => {
      // Check if errors are expected
      for error in errors {
        assert_true(ConfigValidationError::is_validation_error(error))
      }
    }
  }
  
  // Test getting configuration values
  let service_name = ConfigurationManager::get_string(config_manager, "service.name", Some("default_service"))
  let service_port = ConfigurationManager::get_int(config_manager, "service.port", Some(8080))
  let telemetry_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled", Some(true))
  let telemetry_level = ConfigurationManager::get_string(config_manager, "telemetry.level", Some("info"))
  
  // Verify default values are used when config is missing
  assert_eq(service_name, "default_service")
  assert_eq(service_port, 8080)
  assert_eq(telemetry_enabled, true)
  assert_eq(telemetry_level, "info")
}

// Test 2: Dynamic Configuration Updates
test "dynamic configuration updates" {
  // Create configuration manager with hot reload enabled
  let config_manager = ConfigurationManager::new()
  ConfigurationManager::enable_hot_reload(config_manager, 1000) // Check every 1 second
  
  // Create initial configuration
  let initial_config = "{
    \"service\": {
      \"name\": \"test_service\",
      \"port\": 8080,
      \"workers\": 4
    },
    \"telemetry\": {
      \"enabled\": true,
      \"level\": \"info\",
      \"endpoint\": \"https://api.example.com/telemetry\"
    }
  }"
  
  // Write initial config to file
  let config_file = "test_config.json"
  File::write(config_file, initial_config)
  
  // Load configuration
  ConfigurationManager::add_file_source(config_manager, config_file, 1)
  ConfigurationManager::load(config_manager)
  
  // Verify initial values
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name", None), Some("test_service"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(8080))
  assert_eq(ConfigurationManager::get_bool(config_manager, "telemetry.enabled", None), Some(true))
  
  // Set up configuration change listener
  let mut changes_received = 0
  let listener = ConfigurationChangeListener::new(
    fn(path, old_value, new_value) {
      changes_received = changes_received + 1
      assert_true(path.length() > 0)
      assert_true(old_value != new_value)
    }
  )
  
  ConfigurationManager::add_listener(config_manager, listener)
  
  // Update configuration file
  let updated_config = "{
    \"service\": {
      \"name\": \"updated_service\",
      \"port\": 9090,
      \"workers\": 8
    },
    \"telemetry\": {
      \"enabled\": false,
      \"level\": \"debug\",
      \"endpoint\": \"https://api.example.com/telemetry\"
    }
  }"
  
  File::write(config_file, updated_config)
  
  // Wait for hot reload (simulate with manual trigger in test)
  ConfigurationManager::trigger_reload(config_manager)
  
  // Verify updated values
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name", None), Some("updated_service"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(9090))
  assert_eq(ConfigurationManager::get_bool(config_manager, "telemetry.enabled", None), Some(false))
  
  // Verify change listener was called
  assert_true(changes_received > 0)
  
  // Clean up
  File::remove(config_file)
}

// Test 3: Configuration Profiles and Environments
test "configuration profiles and environments" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Create configuration profiles
  let development_config = "{
    \"service\": {
      \"name\": \"azimuth_dev\",
      \"port\": 3000,
      \"debug\": true
    },
    \"database\": {
      \"host\": \"localhost\",
      \"port\": 5432,
      \"name\": \"azimuth_dev_db\"
    },
    \"telemetry\": {
      \"enabled\": true,
      \"level\": \"debug\",
      \"sampling_rate\": 1.0
    }
  }"
  
  let production_config = "{
    \"service\": {
      \"name\": \"azimuth_prod\",
      \"port\": 80,
      \"debug\": false
    },
    \"database\": {
      \"host\": \"prod-db.example.com\",
      \"port\": 5432,
      \"name\": \"azimuth_prod_db\"
    },
    \"telemetry\": {
      \"enabled\": true,
      \"level\": \"warn\",
      \"sampling_rate\": 0.1
    }
  }"
  
  let testing_config = "{
    \"service\": {
      \"name\": \"azimuth_test\",
      \"port\": 3001,
      \"debug\": true
    },
    \"database\": {
      \"host\": \"test-db.example.com\",
      \"port\": 5432,
      \"name\": \"azimuth_test_db\"
    },
    \"telemetry\": {
      \"enabled\": true,
      \"level\": \"info\",
      \"sampling_rate\": 1.0
    }
  }"
  
  // Write profile configs to files
  File::write("config/development.json", development_config)
  File::write("config/production.json", production_config)
  File::write("config/testing.json", testing_config)
  
  // Add profile configurations
  ConfigurationManager::add_profile(config_manager, "development", "config/development.json")
  ConfigurationManager::add_profile(config_manager, "production", "config/production.json")
  ConfigurationManager::add_profile(config_manager, "testing", "config/testing.json")
  
  // Test loading different profiles
  ConfigurationManager::set_active_profile(config_manager, "development")
  ConfigurationManager::load(config_manager)
  
  // Verify development profile values
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name", None), Some("azimuth_dev"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(3000))
  assert_eq(ConfigurationManager::get_bool(config_manager, "service.debug", None), Some(true))
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host", None), Some("localhost"))
  assert_eq(ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate", None), Some(1.0))
  
  // Switch to production profile
  ConfigurationManager::set_active_profile(config_manager, "production")
  ConfigurationManager::load(config_manager)
  
  // Verify production profile values
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name", None), Some("azimuth_prod"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(80))
  assert_eq(ConfigurationManager::get_bool(config_manager, "service.debug", None), Some(false))
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host", None), Some("prod-db.example.com"))
  assert_eq(ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate", None), Some(0.1))
  
  // Switch to testing profile
  ConfigurationManager::set_active_profile(config_manager, "testing")
  ConfigurationManager::load(config_manager)
  
  // Verify testing profile values
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name", None), Some("azimuth_test"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(3001))
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host", None), Some("test-db.example.com"))
  
  // Clean up
  File::remove("config/development.json")
  File::remove("config/production.json")
  File::remove("config/testing.json")
}

// Test 4: Configuration Inheritance and Overrides
test "configuration inheritance and overrides" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Create base configuration
  let base_config = "{
    \"service\": {
      \"name\": \"azimuth\",
      \"port\": 8080,
      \"timeout\": 30,
      \"retries\": 3
    },
    \"telemetry\": {
      \"enabled\": true,
      \"level\": \"info\",
      \"endpoint\": \"https://api.example.com/telemetry\"
    },
    \"features\": {
      \"feature_a\": true,
      \"feature_b\": false,
      \"feature_c\": true
    }
  }"
  
  // Create override configuration
  let override_config = "{
    \"service\": {
      \"port\": 9090,
      \"timeout\": 60
    },
    \"telemetry\": {
      \"level\": \"debug\"
    },
    \"features\": {
      \"feature_b\": true,
      \"feature_d\": true
    }
  }"
  
  // Write config files
  File::write("config/base.json", base_config)
  File::write("config/override.json", override_config)
  
  // Add configuration sources with inheritance
  ConfigurationManager::add_file_source(config_manager, "config/base.json", 1) // Base config
  ConfigurationManager::add_file_source(config_manager, "config/override.json", 2) // Override config
  
  // Load configuration
  ConfigurationManager::load(config_manager)
  
  // Verify inherited values (from base config)
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name", None), Some("azimuth"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.retries", None), Some(3))
  assert_eq(ConfigurationManager::get_bool(config_manager, "telemetry.enabled", None), Some(true))
  assert_eq(ConfigurationManager::get_string(config_manager, "telemetry.endpoint", None), Some("https://api.example.com/telemetry"))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_a", None), Some(true))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_c", None), Some(true))
  
  // Verify overridden values
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(9090))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.timeout", None), Some(60))
  assert_eq(ConfigurationManager::get_string(config_manager, "telemetry.level", None), Some("debug"))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_b", None), Some(true))
  
  // Verify new values from override
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_d", None), Some(true))
  
  // Test runtime overrides
  ConfigurationManager::set_override(config_manager, "service.port", 7070)
  ConfigurationManager::set_override(config_manager, "new.runtime.value", "test_value")
  
  // Verify runtime overrides
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(7070))
  assert_eq(ConfigurationManager::get_string(config_manager, "new.runtime.value", None), Some("test_value"))
  
  // Clear runtime overrides
  ConfigurationManager::clear_overrides(config_manager)
  
  // Verify overrides are cleared
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(9090))
  assert_eq(ConfigurationManager::get_string(config_manager, "new.runtime.value", None), None)
  
  // Clean up
  File::remove("config/base.json")
  File::remove("config/override.json")
}

// Test 5: Configuration Encryption and Security
test "configuration encryption and security" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Create encryption key
  let encryption_key = EncryptionKey::generate()
  
  // Enable configuration encryption
  ConfigurationManager::enable_encryption(config_manager, encryption_key)
  
  // Create sensitive configuration
  let sensitive_config = "{
    \"database\": {
      \"host\": \"db.example.com\",
      \"port\": 5432,
      \"username\": \"azimuth_user\",
      \"password\": \"secret_password\",
      \"ssl_cert\": \"-----BEGIN CERTIFICATE-----\\nMIIBIjANBgkqhki...\\n-----END CERTIFICATE-----\"
    },
    \"api_keys\": {
      \"external_service\": \"sk-1234567890abcdef\",
      \"internal_service\": \"internal_key_12345\"
    },
    \"secrets\": {
      \"jwt_secret\": \"my_jwt_secret_key\",
      \"encryption_key\": \"my_encryption_key\"
    }
  }"
  
  // Write config file
  File::write("config/sensitive.json", sensitive_config)
  
  // Mark sensitive fields
  ConfigurationManager::mark_sensitive(config_manager, "database.password")
  ConfigurationManager::mark_sensitive(config_manager, "database.ssl_cert")
  ConfigurationManager::mark_sensitive(config_manager, "api_keys.external_service")
  ConfigurationManager::mark_sensitive(config_manager, "api_keys.internal_service")
  ConfigurationManager::mark_sensitive(config_manager, "secrets.jwt_secret")
  ConfigurationManager::mark_sensitive(config_manager, "secrets.encryption_key")
  
  // Load configuration
  ConfigurationManager::add_file_source(config_manager, "config/sensitive.json", 1)
  ConfigurationManager::load(config_manager)
  
  // Verify non-sensitive values are accessible
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host", None), Some("db.example.com"))
  assert_eq(ConfigurationManager::get_int(config_manager, "database.port", None), Some(5432))
  assert_eq(ConfigurationManager::get_string(config_manager, "database.username", None), Some("azimuth_user"))
  
  // Verify sensitive values are encrypted
  let encrypted_password = ConfigurationManager::get_encrypted_string(config_manager, "database.password")
  match encrypted_password {
    Some(encrypted_value) => {
      assert_true(encrypted_value != "secret_password")
      assert_true(encrypted_value.length() > 0)
    }
    None => assert_true(false)
  }
  
  // Test decryption
  let decrypted_password = ConfigurationManager::decrypt_string(config_manager, "database.password")
  assert_eq(decrypted_password, Some("secret_password".to_string()))
  
  // Test configuration export with sensitive data masked
  let exported_config = ConfigurationManager::export_config(config_manager, true) // Mask sensitive data
  assert_false(exported_config.contains("secret_password"))
  assert_false(exported_config.contains("sk-1234567890abcdef"))
  assert_false(exported_config.contains("my_jwt_secret_key"))
  assert_true(exported_config.contains("*****")) // Masked values
  
  // Test configuration export with sensitive data included
  let exported_config_full = ConfigurationManager::export_config(config_manager, false) // Include sensitive data
  assert_true(exported_config_full.contains("secret_password"))
  assert_true(exported_config_full.contains("sk-1234567890abcdef"))
  assert_true(exported_config_full.contains("my_jwt_secret_key"))
  
  // Clean up
  File::remove("config/sensitive.json")
}

// Test 6: Configuration Versioning and Rollback
test "configuration versioning and rollback" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Enable configuration versioning
  ConfigurationManager::enable_versioning(config_manager, 10) // Keep last 10 versions
  
  // Create initial configuration
  let initial_config = "{
    \"service\": {
      \"name\": \"azimuth\",
      \"version\": \"1.0.0\",
      \"port\": 8080
    },
    \"features\": {
      \"feature_a\": true,
      \"feature_b\": false
    }
  }"
  
  // Write and load initial config
  File::write("config/versioned.json", initial_config)
  ConfigurationManager::add_file_source(config_manager, "config/versioned.json", 1)
  ConfigurationManager::load(config_manager)
  
  // Verify initial configuration
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version", None), Some("1.0.0"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(8080))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_a", None), Some(true))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_b", None), Some(false))
  
  // Update configuration (version 2)
  let updated_config_v2 = "{
    \"service\": {
      \"name\": \"azimuth\",
      \"version\": \"1.1.0\",
      \"port\": 8080
    },
    \"features\": {
      \"feature_a\": true,
      \"feature_b\": true
    }
  }"
  
  File::write("config/versioned.json", updated_config_v2)
  ConfigurationManager::reload(config_manager)
  
  // Verify updated configuration
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version", None), Some("1.1.0"))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_b", None), Some(true))
  
  // Update configuration again (version 3)
  let updated_config_v3 = "{
    \"service\": {
      \"name\": \"azimuth\",
      \"version\": \"1.2.0\",
      \"port\": 9090
    },
    \"features\": {
      \"feature_a\": false,
      \"feature_b\": true
    }
  }"
  
  File::write("config/versioned.json", updated_config_v3)
  ConfigurationManager::reload(config_manager)
  
  // Verify updated configuration
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version", None), Some("1.2.0"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(9090))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_a", None), Some(false))
  
  // Get configuration history
  let history = ConfigurationManager::get_history(config_manager)
  assert_eq(history.length(), 3) // Should have 3 versions
  
  // Rollback to version 2
  let rollback_result = ConfigurationManager::rollback_to_version(config_manager, 2)
  match rollback_result {
    Ok(_) => assert_true(true),
    Error(e) => assert_true(false)
  }
  
  // Verify rollback
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version", None), Some("1.1.0"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(8080))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_a", None), Some(true))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_b", None), Some(true))
  
  // Rollback to version 1
  let rollback_result_v1 = ConfigurationManager::rollback_to_version(config_manager, 1)
  match rollback_result_v1 {
    Ok(_) => assert_true(true),
    Error(e) => assert_true(false)
  }
  
  // Verify rollback to initial configuration
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version", None), Some("1.0.0"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(8080))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_a", None), Some(true))
  assert_eq(ConfigurationManager::get_bool(config_manager, "features.feature_b", None), Some(false))
  
  // Clean up
  File::remove("config/versioned.json")
}

// Test 7: Configuration Templates and Generation
test "configuration templates and generation" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Create configuration template
  let template = ConfigTemplate::new()
  
  // Add template variables
  ConfigTemplate::add_variable(template, "service_name", "azimuth")
  ConfigTemplate::add_variable(template, "service_port", 8080)
  ConfigTemplate::add_variable(template, "db_host", "localhost")
  ConfigTemplate::add_variable(template, "db_port", 5432)
  ConfigTemplate::add_variable(template, "environment", "development")
  ConfigTemplate::add_variable(template, "debug_mode", true)
  
  // Add template content
  let template_content = "{
    \"service\": {
      \"name\": \"{{service_name}}\",
      \"port\": {{service_port}},
      \"debug\": {{debug_mode}}
    },
    \"database\": {
      \"host\": \"{{db_host}}\",
      \"port\": {{db_port}},
      \"name\": \"{{service_name}}_{{environment}}_db\"
    },
    \"telemetry\": {
      \"enabled\": true,
      \"level\": \"{{#if debug_mode}}debug{{else}}info{{/if}}\"
    }
  }"
  
  ConfigTemplate::set_content(template, template_content)
  
  // Generate configuration from template
  let generated_config = ConfigTemplate::generate(template)
  
  // Verify generated configuration
  assert_true(generated_config.contains("\"name\": \"azimuth\""))
  assert_true(generated_config.contains("\"port\": 8080"))
  assert_true(generated_config.contains("\"host\": \"localhost\""))
  assert_true(generated_config.contains("\"port\": 5432"))
  assert_true(generated_config.contains("\"name\": \"azimuth_development_db\""))
  assert_true(generated_config.contains("\"level\": \"debug\""))
  
  // Load generated configuration
  File::write("config/generated.json", generated_config)
  ConfigurationManager::add_file_source(config_manager, "config/generated.json", 1)
  ConfigurationManager::load(config_manager)
  
  // Verify loaded configuration
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name", None), Some("azimuth"))
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port", None), Some(8080))
  assert_eq(ConfigurationManager::get_bool(config_manager, "service.debug", None), Some(true))
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host", None), Some("localhost"))
  assert_eq(ConfigurationManager::get_int(config_manager, "database.port", None), Some(5432))
  assert_eq(ConfigurationManager::get_string(config_manager, "database.name", None), Some("azimuth_development_db"))
  assert_eq(ConfigurationManager::get_string(config_manager, "telemetry.level", None), Some("debug"))
  
  // Test template with different variables
  ConfigTemplate::set_variable(template, "environment", "production")
  ConfigTemplate::set_variable(template, "debug_mode", false)
  ConfigTemplate::set_variable(template, "service_port", 80)
  
  let prod_config = ConfigTemplate::generate(template)
  
  // Verify production configuration
  assert_true(prod_config.contains("\"port\": 80"))
  assert_true(prod_config.contains("\"debug\": false"))
  assert_true(prod_config.contains("\"name\": \"azimuth_production_db\""))
  assert_true(prod_config.contains("\"level\": \"info\""))
  
  // Clean up
  File::remove("config/generated.json")
}