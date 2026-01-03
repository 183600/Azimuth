// Azimuth Telemetry System - Configuration Management Tests
// This file contains test cases for configuration management functionality

// Test 1: Basic Configuration Loading
test "basic configuration loading" {
  let config_manager = ConfigurationManager::new()
  
  // Create test configuration
  let test_config = {
    "service.name": "azimuth-telemetry",
    "service.version": "1.0.0",
    "telemetry.enabled": true,
    "telemetry.sampling.rate": 0.1,
    "telemetry.export.interval": 60000,
    "logging.level": "INFO",
    "metrics.enabled": true,
    "tracing.enabled": true
  }
  
  // Load configuration
  ConfigurationManager::load_config(config_manager, test_config)
  
  // Verify configuration values
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version"), "1.0.0")
  assert_eq(ConfigurationManager::get_bool(config_manager, "telemetry.enabled"), true)
  assert_eq(ConfigurationManager::get_float(config_manager, "telemetry.sampling.rate"), 0.1)
  assert_eq(ConfigurationManager::get_int(config_manager, "telemetry.export.interval"), 60000)
  assert_eq(ConfigurationManager::get_string(config_manager, "logging.level"), "INFO")
  assert_eq(ConfigurationManager::get_bool(config_manager, "metrics.enabled"), true)
  assert_eq(ConfigurationManager::get_bool(config_manager, "tracing.enabled"), true)
  
  // Test default values
  assert_eq(ConfigurationManager::get_string_with_default(config_manager, "nonexistent.key", "default"), "default")
  assert_eq(ConfigurationManager::get_bool_with_default(config_manager, "nonexistent.bool", false), false)
  assert_eq(ConfigurationManager::get_int_with_default(config_manager, "nonexistent.int", 42), 42)
  assert_eq(ConfigurationManager::get_float_with_default(config_manager, "nonexistent.float", 3.14), 3.14)
}

// Test 2: Configuration File Loading
test "configuration file loading" {
  let file_config_manager = ConfigurationManager::new()
  
  // Create temporary configuration file
  let config_content = "{\n    \"service\": {\n      \"name\": \"azimuth-telemetry\",\n      \"version\": \"1.0.0\",\n      \"port\": 8080\n    },\n    \"telemetry\": {\n      \"enabled\": true,\n      \"sampling\": {\n        \"rate\": 0.1\n      },\n      \"export\": {\n        \"interval\": 60000,\n        \"endpoint\": \"https://api.example.com/telemetry\"\n      }\n    },\n    \"logging\": {\n      \"level\": \"INFO\",\n      \"format\": \"json\"\n    }\n  }"
  
  let config_file = "/tmp/test_config.json"
  File::write(config_file, config_content)
  
  // Load configuration from file
  let load_result = ConfigurationManager::load_from_file(file_config_manager, config_file)
  match load_result {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Verify loaded configuration
  assert_eq(ConfigurationManager::get_string(file_config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ConfigurationManager::get_string(file_config_manager, "service.version"), "1.0.0")
  assert_eq(ConfigurationManager::get_int(file_config_manager, "service.port"), 8080)
  assert_eq(ConfigurationManager::get_bool(file_config_manager, "telemetry.enabled"), true)
  assert_eq(ConfigurationManager::get_float(file_config_manager, "telemetry.sampling.rate"), 0.1)
  assert_eq(ConfigurationManager::get_int(file_config_manager, "telemetry.export.interval"), 60000)
  assert_eq(ConfigurationManager::get_string(file_config_manager, "telemetry.export.endpoint"), "https://api.example.com/telemetry")
  assert_eq(ConfigurationManager::get_string(file_config_manager, "logging.level"), "INFO")
  assert_eq(ConfigurationManager::get_string(file_config_manager, "logging.format"), "json")
  
  // Clean up
  File::remove(config_file)
}

// Test 3: Configuration Validation
test "configuration validation" {
  let validation_config_manager = ConfigurationManager::new()
  
  // Define validation rules
  ConfigurationManager::add_validation_rule(validation_config_manager, "service.name", {
    "type": "string",
    "required": true,
    "min_length": 1,
    "max_length": 100
  })
  
  ConfigurationManager::add_validation_rule(validation_config_manager, "service.port", {
    "type": "integer",
    "required": true,
    "min_value": 1,
    "max_value": 65535
  })
  
  ConfigurationManager::add_validation_rule(validation_config_manager, "telemetry.sampling.rate", {
    "type": "float",
    "required": false,
    "min_value": 0.0,
    "max_value": 1.0
  })
  
  ConfigurationManager::add_validation_rule(validation_config_manager, "logging.level", {
    "type": "enum",
    "required": false,
    "values": ["DEBUG", "INFO", "WARN", "ERROR"]
  })
  
  // Test valid configuration
  let valid_config = {
    "service.name": "azimuth-telemetry",
    "service.port": 8080,
    "telemetry.sampling.rate": 0.5,
    "logging.level": "INFO"
  }
  
  let valid_result = ConfigurationManager::validate_and_load(validation_config_manager, valid_config)
  match valid_result {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Test invalid configuration
  let invalid_config = {
    "service.name": "", // Too short
    "service.port": 70000, // Out of range
    "telemetry.sampling.rate": 1.5, // Out of range
    "logging.level": "INVALID" // Not in enum
  }
  
  let invalid_result = ConfigurationManager::validate_and_load(validation_config_manager, invalid_config)
  match invalid_result {
    Success(_) => assert_true(false)
    Error(errors) => {
      // Should have 4 validation errors
      assert_eq(errors.length(), 4)
      
      // Check specific error messages
      assert_true(errors.any(|error| error.contains("service.name") && error.contains("min_length")))
      assert_true(errors.any(|error| error.contains("service.port") && error.contains("max_value")))
      assert_true(errors.any(|error| error.contains("telemetry.sampling.rate") && error.contains("max_value")))
      assert_true(errors.any(|error| error.contains("logging.level") && error.contains("enum")))
    }
  }
}

// Test 4: Dynamic Configuration Updates
test "dynamic configuration updates" {
  let dynamic_config_manager = DynamicConfigurationManager::new()
  
  // Load initial configuration
  let initial_config = {
    "service.port": 8080,
    "telemetry.sampling.rate": 0.1,
    "logging.level": "INFO",
    "feature.flags.new_feature": false
  }
  
  DynamicConfigurationManager::load_config(dynamic_config_manager, initial_config)
  
  // Register configuration change listeners
  let port_changed = AtomicBool::new(false)
  let sampling_changed = AtomicBool::new(false)
  let feature_changed = AtomicBool::new(false)
  
  DynamicConfigurationManager::register_listener(dynamic_config_manager, "service.port", || {
    port_changed.store(true)
  })
  
  DynamicConfigurationManager::register_listener(dynamic_config_manager, "telemetry.sampling.rate", || {
    sampling_changed.store(true)
  })
  
  DynamicConfigurationManager::register_listener(dynamic_config_manager, "feature.flags.new_feature", || {
    feature_changed.store(true)
  })
  
  // Update configuration values
  DynamicConfigurationManager::update_value(dynamic_config_manager, "service.port", 9090)
  DynamicConfigurationManager::update_value(dynamic_config_manager, "telemetry.sampling.rate", 0.2)
  DynamicConfigurationManager::update_value(dynamic_config_manager, "logging.level", "DEBUG")
  DynamicConfigurationManager::update_value(dynamic_config_manager, "feature.flags.new_feature", true)
  
  // Verify values are updated
  assert_eq(DynamicConfigurationManager::get_int(dynamic_config_manager, "service.port"), 9090)
  assert_eq(DynamicConfigurationManager::get_float(dynamic_config_manager, "telemetry.sampling.rate"), 0.2)
  assert_eq(DynamicConfigurationManager::get_string(dynamic_config_manager, "logging.level"), "DEBUG")
  assert_eq(DynamicConfigurationManager::get_bool(dynamic_config_manager, "feature.flags.new_feature"), true)
  
  // Verify listeners were called
  assert_true(port_changed.load())
  assert_true(sampling_changed.load())
  assert_true(feature_changed.load())
  
  // Test batch updates
  let batch_updates = {
    "service.port": 8080,
    "telemetry.sampling.rate": 0.3,
    "logging.level": "WARN"
  }
  
  DynamicConfigurationManager::batch_update(dynamic_config_manager, batch_updates)
  
  // Verify batch updates
  assert_eq(DynamicConfigurationManager::get_int(dynamic_config_manager, "service.port"), 8080)
  assert_eq(DynamicConfigurationManager::get_float(dynamic_config_manager, "telemetry.sampling.rate"), 0.3)
  assert_eq(DynamicConfigurationManager::get_string(dynamic_config_manager, "logging.level"), "WARN")
}

// Test 5: Environment Variable Configuration
test "environment variable configuration" {
  let env_config_manager = EnvironmentConfigurationManager::new()
  
  // Set environment variables
  Env::set("AZIMUTH_SERVICE_NAME", "azimuth-from-env")
  Env::set("AZIMUTH_SERVICE_PORT", "9090")
  Env::set("AZIMUTH_TELEMETRY_ENABLED", "true")
  Env::set("AZIMUTH_TELEMETRY_SAMPLING_RATE", "0.25")
  Env::set("AZIMUTH_LOGGING_LEVEL", "DEBUG")
  
  // Configure environment variable mappings
  EnvironmentConfigurationManager::set_prefix(env_config_manager, "AZIMUTH_")
  EnvironmentConfigurationManager::add_mapping(env_config_manager, "service.name", "SERVICE_NAME")
  EnvironmentConfigurationManager::add_mapping(env_config_manager, "service.port", "SERVICE_PORT")
  EnvironmentConfigurationManager::add_mapping(env_config_manager, "telemetry.enabled", "TELEMETRY_ENABLED")
  EnvironmentConfigurationManager::add_mapping(env_config_manager, "telemetry.sampling.rate", "TELEMETRY_SAMPLING_RATE")
  EnvironmentConfigurationManager::add_mapping(env_config_manager, "logging.level", "LOGGING_LEVEL")
  
  // Load configuration from environment
  EnvironmentConfigurationManager::load_from_env(env_config_manager)
  
  // Verify loaded configuration
  assert_eq(EnvironmentConfigurationManager::get_string(env_config_manager, "service.name"), "azimuth-from-env")
  assert_eq(EnvironmentConfigurationManager::get_int(env_config_manager, "service.port"), 9090)
  assert_eq(EnvironmentConfigurationManager::get_bool(env_config_manager, "telemetry.enabled"), true)
  assert_eq(EnvironmentConfigurationManager::get_float(env_config_manager, "telemetry.sampling.rate"), 0.25)
  assert_eq(EnvironmentConfigurationManager::get_string(env_config_manager, "logging.level"), "DEBUG")
  
  // Clean up environment variables
  Env::unset("AZIMUTH_SERVICE_NAME")
  Env::unset("AZIMUTH_SERVICE_PORT")
  Env::unset("AZIMUTH_TELEMETRY_ENABLED")
  Env::unset("AZIMUTH_TELEMETRY_SAMPLING_RATE")
  Env::unset("AZIMUTH_LOGGING_LEVEL")
}

// Test 6: Configuration Profiles
test "configuration profiles" {
  let profile_config_manager = ProfileConfigurationManager::new()
  
  // Define base configuration
  let base_config = {
    "service.name": "azimuth-telemetry",
    "service.version": "1.0.0",
    "telemetry.enabled": true,
    "logging.level": "INFO"
  }
  
  // Define development profile
  let dev_profile = {
    "service.port": 8080,
    "telemetry.sampling.rate": 1.0,
    "logging.level": "DEBUG",
    "debug.enabled": true
  }
  
  // Define production profile
  let prod_profile = {
    "service.port": 80,
    "telemetry.sampling.rate": 0.1,
    "logging.level": "WARN",
    "debug.enabled": false
  }
  
  // Load configurations
  ProfileConfigurationManager::load_base_config(profile_config_manager, base_config)
  ProfileConfigurationManager::add_profile(profile_config_manager, "development", dev_profile)
  ProfileConfigurationManager::add_profile(profile_config_manager, "production", prod_profile)
  
  // Test development profile
  ProfileConfigurationManager::activate_profile(profile_config_manager, "development")
  
  assert_eq(ProfileConfigurationManager::get_string(profile_config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ProfileConfigurationManager::get_string(profile_config_manager, "service.version"), "1.0.0")
  assert_eq(ProfileConfigurationManager::get_int(profile_config_manager, "service.port"), 8080)
  assert_eq(ProfileConfigurationManager::get_bool(profile_config_manager, "telemetry.enabled"), true)
  assert_eq(ProfileConfigurationManager::get_float(profile_config_manager, "telemetry.sampling.rate"), 1.0)
  assert_eq(ProfileConfigurationManager::get_string(profile_config_manager, "logging.level"), "DEBUG")
  assert_eq(ProfileConfigurationManager::get_bool(profile_config_manager, "debug.enabled"), true)
  
  // Test production profile
  ProfileConfigurationManager::activate_profile(profile_config_manager, "production")
  
  assert_eq(ProfileConfigurationManager::get_string(profile_config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ProfileConfigurationManager::get_string(profile_config_manager, "service.version"), "1.0.0")
  assert_eq(ProfileConfigurationManager::get_int(profile_config_manager, "service.port"), 80)
  assert_eq(ProfileConfigurationManager::get_bool(profile_config_manager, "telemetry.enabled"), true)
  assert_eq(ProfileConfigurationManager::get_float(profile_config_manager, "telemetry.sampling.rate"), 0.1)
  assert_eq(ProfileConfigurationManager::get_string(profile_config_manager, "logging.level"), "WARN")
  assert_eq(ProfileConfigurationManager::get_bool(profile_config_manager, "debug.enabled"), false)
  
  // Test profile inheritance
  let test_profile = {
    "service.port": 3000,
    "test.mode": true
  }
  
  ProfileConfigurationManager::add_profile(profile_config_manager, "test", test_profile)
  ProfileConfigurationManager::activate_profile(profile_config_manager, "test")
  
  // Should inherit from base config
  assert_eq(ProfileConfigurationManager::get_string(profile_config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(ProfileConfigurationManager::get_string(profile_config_manager, "service.version"), "1.0.0")
  
  // Should use profile-specific values
  assert_eq(ProfileConfigurationManager::get_int(profile_config_manager, "service.port"), 3000)
  assert_eq(ProfileConfigurationManager::get_bool(profile_config_manager, "test.mode"), true)
  
  // Should use default values from base config
  assert_eq(ProfileConfigurationManager::get_bool(profile_config_manager, "telemetry.enabled"), true)
  assert_eq(ProfileConfigurationManager::get_string(profile_config_manager, "logging.level"), "INFO")
}

// Test 7: Configuration Encryption
test "configuration encryption" {
  let encryption_config_manager = EncryptedConfigurationManager::new()
  
  // Set encryption key
  EncryptedConfigurationManager::set_encryption_key(encryption_config_manager, "encryption_key_12345")
  
  // Create sensitive configuration
  let sensitive_config = {
    "database.password": "secret_password",
    "api.key": "secret_api_key",
    "service.name": "public_name", // Not sensitive
    "service.port": 8080 // Not sensitive
  }
  
  // Mark sensitive keys
  EncryptedConfigurationManager::mark_sensitive(encryption_config_manager, "database.password")
  EncryptedConfigurationManager::mark_sensitive(encryption_config_manager, "api.key")
  
  // Load configuration
  EncryptedConfigurationManager::load_config(encryption_config_manager, sensitive_config)
  
  // Verify values can be retrieved
  assert_eq(EncryptedConfigurationManager::get_string(encryption_config_manager, "database.password"), "secret_password")
  assert_eq(EncryptedConfigurationManager::get_string(encryption_config_manager, "api.key"), "secret_api_key")
  assert_eq(EncryptedConfigurationManager::get_string(encryption_config_manager, "service.name"), "public_name")
  assert_eq(EncryptedConfigurationManager::get_int(encryption_config_manager, "service.port"), 8080)
  
  // Save configuration to file
  let encrypted_config_file = "/tmp/encrypted_config.json"
  EncryptedConfigurationManager::save_to_file(encryption_config_manager, encrypted_config_file)
  
  // Load configuration with new manager
  let new_encryption_config_manager = EncryptedConfigurationManager::new()
  EncryptedConfigurationManager::set_encryption_key(new_encryption_config_manager, "encryption_key_12345")
  EncryptedConfigurationManager::mark_sensitive(new_encryption_config_manager, "database.password")
  EncryptedConfigurationManager::mark_sensitive(new_encryption_config_manager, "api.key")
  
  let load_result = EncryptedConfigurationManager::load_from_file(new_encryption_config_manager, encrypted_config_file)
  match load_result {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Verify values can be retrieved
  assert_eq(EncryptedConfigurationManager::get_string(new_encryption_config_manager, "database.password"), "secret_password")
  assert_eq(EncryptedConfigurationManager::get_string(new_encryption_config_manager, "api.key"), "secret_api_key")
  assert_eq(EncryptedConfigurationManager::get_string(new_encryption_config_manager, "service.name"), "public_name")
  assert_eq(EncryptedConfigurationManager::get_int(new_encryption_config_manager, "service.port"), 8080)
  
  // Test with wrong encryption key
  let wrong_encryption_config_manager = EncryptedConfigurationManager::new()
  EncryptedConfigurationManager::set_encryption_key(wrong_encryption_config_manager, "wrong_key")
  EncryptedConfigurationManager::mark_sensitive(wrong_encryption_config_manager, "database.password")
  EncryptedConfigurationManager::mark_sensitive(wrong_encryption_config_manager, "api.key")
  
  let wrong_load_result = EncryptedConfigurationManager::load_from_file(wrong_encryption_config_manager, encrypted_config_file)
  match wrong_load_result {
    Success(_) => assert_true(false) // Should fail with wrong key
    Error(_) => assert_true(true)
  }
  
  // Clean up
  File::remove(encrypted_config_file)
}

// Test 8: Configuration Change History
test "configuration change history" {
  let history_config_manager = HistoryConfigurationManager::new()
  
  // Enable history tracking
  HistoryConfigurationManager::enable_history_tracking(history_config_manager)
  
  // Load initial configuration
  let initial_config = {
    "service.port": 8080,
    "telemetry.sampling.rate": 0.1,
    "logging.level": "INFO"
  }
  
  HistoryConfigurationManager::load_config(history_config_manager, initial_config)
  
  // Make configuration changes
  HistoryConfigurationManager::update_value(history_config_manager, "service.port", 9090)
  HistoryConfigurationManager::update_value(history_config_manager, "telemetry.sampling.rate", 0.2)
  HistoryConfigurationManager::update_value(history_config_manager, "logging.level", "DEBUG")
  HistoryConfigurationManager::update_value(history_config_manager, "service.port", 8080) // Revert
  
  // Get change history
  let history = HistoryConfigurationManager::get_change_history(history_config_manager)
  
  // Should have 4 changes
  assert_eq(history.length(), 4)
  
  // Check first change
  let first_change = history[0]
  assert_eq(first_change.key, "service.port")
  assert_eq(first_change.old_value, "8080")
  assert_eq(first_change.new_value, "9090")
  assert_true(first_change.timestamp > 0)
  
  // Check second change
  let second_change = history[1]
  assert_eq(second_change.key, "telemetry.sampling.rate")
  assert_eq(second_change.old_value, "0.1")
  assert_eq(second_change.new_value, "0.2")
  
  // Check third change
  let third_change = history[2]
  assert_eq(third_change.key, "logging.level")
  assert_eq(third_change.old_value, "INFO")
  assert_eq(third_change.new_value, "DEBUG")
  
  // Check fourth change (revert)
  let fourth_change = history[3]
  assert_eq(fourth_change.key, "service.port")
  assert_eq(fourth_change.old_value, "9090")
  assert_eq(fourth_change.new_value, "8080")
  
  // Get history for specific key
  let port_history = HistoryConfigurationManager::get_key_history(history_config_manager, "service.port")
  assert_eq(port_history.length(), 2) // Initial change and revert
  
  // Revert to specific point in history
  HistoryConfigurationManager::revert_to_timestamp(history_config_manager, second_change.timestamp)
  
  // Verify revert
  assert_eq(HistoryConfigurationManager::get_int(history_config_manager, "service.port"), 9090)
  assert_eq(HistoryConfigurationManager::get_float(history_config_manager, "telemetry.sampling.rate"), 0.1)
  assert_eq(HistoryConfigurationManager::get_string(history_config_manager, "logging.level"), "INFO")
}

// Test 9: Configuration Templates
test "configuration templates" {
  let template_config_manager = TemplateConfigurationManager::new()
  
  // Define configuration template
  let service_template = {
    "name": "{{service_name}}",
    "version": "{{service_version}}",
    "port": {{service_port}},
    "telemetry": {
      "enabled": {{telemetry_enabled}},
      "sampling": {
        "rate": {{sampling_rate}}
      }
    },
    "logging": {
      "level": "{{logging_level}}"
    }
  }
  
  TemplateConfigurationManager::add_template(template_config_manager, "service", service_template)
  
  // Define template variables
  let template_vars = {
    "service_name": "azimuth-telemetry",
    "service_version": "1.0.0",
    "service_port": 8080,
    "telemetry_enabled": true,
    "sampling_rate": 0.1,
    "logging_level": "INFO"
  }
  
  // Generate configuration from template
  let generated_config = TemplateConfigurationManager::generate_from_template(template_config_manager, "service", template_vars)
  
  // Verify generated configuration
  assert_eq(generated_config["name"], "azimuth-telemetry")
  assert_eq(generated_config["version"], "1.0.0")
  assert_eq(generated_config["port"], 8080)
  assert_eq(generated_config["telemetry"]["enabled"], true)
  assert_eq(generated_config["telemetry"]["sampling"]["rate"], 0.1)
  assert_eq(generated_config["logging"]["level"], "INFO")
  
  // Load generated configuration
  TemplateConfigurationManager::load_config(template_config_manager, generated_config)
  
  // Verify loaded configuration
  assert_eq(TemplateConfigurationManager::get_string(template_config_manager, "name"), "azimuth-telemetry")
  assert_eq(TemplateConfigurationManager::get_string(template_config_manager, "version"), "1.0.0")
  assert_eq(TemplateConfigurationManager::get_int(template_config_manager, "port"), 8080)
  assert_eq(TemplateConfigurationManager::get_bool(template_config_manager, "telemetry.enabled"), true)
  assert_eq(TemplateConfigurationManager::get_float(template_config_manager, "telemetry.sampling.rate"), 0.1)
  assert_eq(TemplateConfigurationManager::get_string(template_config_manager, "logging.level"), "INFO")
  
  // Test template inheritance
  let base_template = {
    "service": {
      "name": "{{service_name}}",
      "version": "{{service_version}}"
    },
    "telemetry": {
      "enabled": {{telemetry_enabled}}
    }
  }
  
  let extended_template = {
    "extends": "base",
    "service": {
      "port": {{service_port}}
    },
    "telemetry": {
      "sampling": {
        "rate": {{sampling_rate}}
      }
    }
  }
  
  TemplateConfigurationManager::add_template(template_config_manager, "base", base_template)
  TemplateConfigurationManager::add_template(template_config_manager, "extended", extended_template)
  
  let extended_config = TemplateConfigurationManager::generate_from_template(template_config_manager, "extended", template_vars)
  
  // Verify extended configuration
  assert_eq(extended_config["service"]["name"], "azimuth-telemetry")
  assert_eq(extended_config["service"]["version"], "1.0.0")
  assert_eq(extended_config["service"]["port"], 8080)
  assert_eq(extended_config["telemetry"]["enabled"], true)
  assert_eq(extended_config["telemetry"]["sampling"]["rate"], 0.1)
}

// Test 10: Configuration Remote Synchronization
test "configuration remote synchronization" {
  let remote_config_manager = RemoteConfigurationManager::new()
  
  // Configure remote source
  RemoteConfigurationManager::configure_remote_source(remote_config_manager, {
    "url": "https://config.example.com/api/config",
    "poll_interval": 5000, // 5 seconds
    "timeout": 10000, // 10 seconds
    "retry_attempts": 3,
    "auth_token": "config_auth_token"
  })
  
  // Load local configuration
  let local_config = {
    "service.name": "azimuth-telemetry",
    "service.version": "1.0.0",
    "telemetry.enabled": true,
    "telemetry.sampling.rate": 0.1
  }
  
  RemoteConfigurationManager::load_config(remote_config_manager, local_config)
  
  // Simulate remote configuration update
  let remote_config = {
    "service.name": "azimuth-telemetry",
    "service.version": "1.1.0", // Updated
    "telemetry.enabled": true,
    "telemetry.sampling.rate": 0.2, // Updated
    "feature.flags.new_feature": true // New
  }
  
  // Simulate receiving remote update
  let update_result = RemoteConfigurationManager::process_remote_update(remote_config_manager, remote_config)
  match update_result {
    Success(updates) => {
      // Should have 3 updates
      assert_eq(updates.length(), 3)
      
      // Check specific updates
      assert_true(updates.contains("service.version"))
      assert_true(updates.contains("telemetry.sampling.rate"))
      assert_true(updates.contains("feature.flags.new_feature"))
    }
    Error(_) => assert_true(false)
  }
  
  // Verify configuration was updated
  assert_eq(RemoteConfigurationManager::get_string(remote_config_manager, "service.name"), "azimuth-telemetry")
  assert_eq(RemoteConfigurationManager::get_string(remote_config_manager, "service.version"), "1.1.0")
  assert_eq(RemoteConfigurationManager::get_bool(remote_config_manager, "telemetry.enabled"), true)
  assert_eq(RemoteConfigurationManager::get_float(remote_config_manager, "telemetry.sampling.rate"), 0.2)
  assert_eq(RemoteConfigurationManager::get_bool(remote_config_manager, "feature.flags.new_feature"), true)
  
  // Test conflict resolution (local vs remote)
  RemoteConfigurationManager::set_conflict_resolution(remote_config_manager, "local_wins")
  
  // Update local configuration
  RemoteConfigurationManager::update_value(remote_config_manager, "telemetry.sampling.rate", 0.3)
  
  // Simulate conflicting remote update
  let conflicting_remote_config = {
    "service.name": "azimuth-telemetry",
    "service.version": "1.1.0",
    "telemetry.enabled": true,
    "telemetry.sampling.rate": 0.4, // Conflicts with local value
    "feature.flags.new_feature": true
  }
  
  // Process conflicting update
  let conflict_result = RemoteConfigurationManager::process_remote_update(remote_config_manager, conflicting_remote_config)
  match conflict_result {
    Success(updates) => {
      // Should not include telemetry.sampling.rate due to conflict resolution
      assert_false(updates.contains("telemetry.sampling.rate"))
    }
    Error(_) => assert_true(false)
  }
  
  // Verify local value was preserved
  assert_eq(RemoteConfigurationManager::get_float(remote_config_manager, "telemetry.sampling.rate"), 0.3)
  
  // Test synchronization status
  let sync_status = RemoteConfigurationManager::get_sync_status(remote_config_manager)
  assert_true(sync_status.last_sync_time > 0)
  assert_eq(sync_status.total_updates, 4)
  assert_eq(sync_status.conflicts, 1)
  
  // Test manual synchronization
  let manual_sync_result = RemoteConfigurationManager::sync_now(remote_config_manager)
  match manual_sync_result {
    Success(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
}