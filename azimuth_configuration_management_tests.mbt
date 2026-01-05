// Azimuth Telemetry System - Configuration Management Tests
// This file contains test cases for configuration management functionality

// Test 1: Basic Configuration Operations
test "basic configuration operations" {
  // Create a configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Test setting and getting configuration values
  ConfigurationManager::set(config_manager, "telemetry.enabled", true)
  ConfigurationManager::set(config_manager, "telemetry.sample_rate", 0.1)
  ConfigurationManager::set(config_manager, "telemetry.endpoint", "https://telemetry.example.com/api/v1")
  ConfigurationManager::set(config_manager, "telemetry.timeout", 5000)
  
  // Test getting configuration values
  let enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match enabled {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let sample_rate = ConfigurationManager::get_float(config_manager, "telemetry.sample_rate")
  match sample_rate {
    Some(value) => assert_eq(value, 0.1)
    None => assert_true(false)
  }
  
  let endpoint = ConfigurationManager::get_string(config_manager, "telemetry.endpoint")
  match endpoint {
    Some(value) => assert_eq(value, "https://telemetry.example.com/api/v1")
    None => assert_true(false)
  }
  
  let timeout = ConfigurationManager::get_int(config_manager, "telemetry.timeout")
  match timeout {
    Some(value) => assert_eq(value, 5000)
    None => assert_true(false)
  }
  
  // Test getting non-existent configuration values
  let non_existent = ConfigurationManager::get_string(config_manager, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test checking if configuration key exists
  assert_true(ConfigurationManager::has(config_manager, "telemetry.enabled"))
  assert_false(ConfigurationManager::has(config_manager, "non.existent.key"))
  
  // Test removing configuration values
  ConfigurationManager::remove(config_manager, "telemetry.timeout")
  assert_false(ConfigurationManager::has(config_manager, "telemetry.timeout"))
}

// Test 2: Configuration Hierarchies
test "configuration hierarchies" {
  // Create a configuration manager with hierarchical support
  let config_manager = ConfigurationManager::with_hierarchy()
  
  // Set configuration values at different levels
  ConfigurationManager::set(config_manager, "telemetry.enabled", true)
  ConfigurationManager::set(config_manager, "telemetry.sample_rate", 0.1)
  ConfigurationManager::set(config_manager, "telemetry.http.enabled", true)
  ConfigurationManager::set(config_manager, "telemetry.http.timeout", 3000)
  ConfigurationManager::set(config_manager, "telemetry.grpc.enabled", false)
  ConfigurationManager::set(config_manager, "telemetry.grpc.timeout", 5000)
  
  // Test getting values at different levels
  let telemetry_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let http_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.http.enabled")
  match http_enabled {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let grpc_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.grpc.enabled")
  match grpc_enabled {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
  
  // Test getting all keys under a namespace
  let telemetry_keys = ConfigurationManager::get_keys(config_manager, "telemetry")
  assert_true(telemetry_keys.includes("telemetry.enabled"))
  assert_true(telemetry_keys.includes("telemetry.sample_rate"))
  assert_true(telemetry_keys.includes("telemetry.http.enabled"))
  assert_true(telemetry_keys.includes("telemetry.http.timeout"))
  assert_true(telemetry_keys.includes("telemetry.grpc.enabled"))
  assert_true(telemetry_keys.includes("telemetry.grpc.timeout"))
  
  let http_keys = ConfigurationManager::get_keys(config_manager, "telemetry.http")
  assert_true(http_keys.includes("telemetry.http.enabled"))
  assert_true(http_keys.includes("telemetry.http.timeout"))
  assert_false(http_keys.includes("telemetry.grpc.enabled"))
  
  // Test getting configuration as a subtree
  let http_config = ConfigurationManager::get_subtree(config_manager, "telemetry.http")
  assert_true(ConfigurationManager::has(http_config, "enabled"))
  assert_true(ConfigurationManager::has(http_config, "timeout"))
  assert_false(ConfigurationManager::has(http_config, "grpc.enabled"))
  
  // Test merging configurations
  let override_config = ConfigurationManager::new()
  ConfigurationManager::set(override_config, "telemetry.sample_rate", 0.2)
  ConfigurationManager::set(override_config, "telemetry.http.timeout", 4000)
  ConfigurationManager::set(override_config, "telemetry.new_feature.enabled", true)
  
  ConfigurationManager::merge(config_manager, override_config)
  
  // Verify merged values
  let merged_sample_rate = ConfigurationManager::get_float(config_manager, "telemetry.sample_rate")
  match merged_sample_rate {
    Some(value) => assert_eq(value, 0.2) // Should be overridden
    None => assert_true(false)
  }
  
  let merged_timeout = ConfigurationManager::get_int(config_manager, "telemetry.http.timeout")
  match merged_timeout {
    Some(value) => assert_eq(value, 4000) // Should be overridden
    None => assert_true(false)
  }
  
  let new_feature = ConfigurationManager::get_bool(config_manager, "telemetry.new_feature.enabled")
  match new_feature {
    Some(value) => assert_true(value) // Should be added
    None => assert_true(false)
  }
  
  // Original values should still exist
  let original_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match original_enabled {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
}

// Test 3: Configuration Validation
test "configuration validation" {
  // Create a configuration manager with validation
  let config_manager = ConfigurationManager::with_validation()
  
  // Add validation rules
  ConfigurationManager::add_validation_rule(config_manager, "telemetry.sample_rate", {
    "type": "float",
    "min": 0.0,
    "max": 1.0
  })
  
  ConfigurationManager::add_validation_rule(config_manager, "telemetry.timeout", {
    "type": "int",
    "min": 100,
    "max": 60000
  })
  
  ConfigurationManager::add_validation_rule(config_manager, "telemetry.endpoint", {
    "type": "string",
    "pattern": "^https?://.*"
  })
  
  // Test valid values
  let valid_result1 = ConfigurationManager::set_with_validation(config_manager, "telemetry.sample_rate", 0.5)
  assert_true(valid_result1.is_ok())
  
  let valid_result2 = ConfigurationManager::set_with_validation(config_manager, "telemetry.timeout", 5000)
  assert_true(valid_result2.is_ok())
  
  let valid_result3 = ConfigurationManager::set_with_validation(config_manager, "telemetry.endpoint", "https://telemetry.example.com")
  assert_true(valid_result3.is_ok())
  
  // Test invalid values
  let invalid_result1 = ConfigurationManager::set_with_validation(config_manager, "telemetry.sample_rate", 1.5)
  assert_true(invalid_result1.is_err())
  
  let invalid_result2 = ConfigurationManager::set_with_validation(config_manager, "telemetry.timeout", 50)
  assert_true(invalid_result2.is_err())
  
  let invalid_result3 = ConfigurationManager::set_with_validation(config_manager, "telemetry.endpoint", "not-a-url")
  assert_true(invalid_result3.is_err())
  
  // Test validation of existing configuration
  ConfigurationManager::set(config_manager, "telemetry.sample_rate", 0.8) // Bypass validation
  
  let validation_result = ConfigurationManager::validate_all(config_manager)
  assert_true(validation_result.is_ok())
  
  // Add an invalid value and validate again
  ConfigurationManager::set(config_manager, "telemetry.sample_rate", 1.2) // Bypass validation
  
  let invalid_validation_result = ConfigurationManager::validate_all(config_manager)
  assert_true(invalid_validation_result.is_err())
  
  match invalid_validation_result {
    Err(errors) => assert_true(errors.length() > 0)
    Ok(_) => assert_true(false)
  }
  
  // Test custom validation function
  ConfigurationManager::add_custom_validation(config_manager, "telemetry.api_key", fn(key) {
    if key.length() < 16 {
      ValidationFailure::new("API key must be at least 16 characters")
    } else {
      ValidationSuccess::new()
    }
  })
  
  let short_key_result = ConfigurationManager::set_with_validation(config_manager, "telemetry.api_key", "short-key")
  assert_true(short_key_result.is_err())
  
  let long_key_result = ConfigurationManager::set_with_validation(config_manager, "telemetry.api_key", "this-is-a-valid-api-key")
  assert_true(long_key_result.is_ok())
}

// Test 4: Configuration Persistence
test "configuration persistence" {
  // Create a configuration manager with persistence
  let config_manager = ConfigurationManager::with_persistence("/tmp/test_config.json")
  
  // Set some configuration values
  ConfigurationManager::set(config_manager, "telemetry.enabled", true)
  ConfigurationManager::set(config_manager, "telemetry.sample_rate", 0.15)
  ConfigurationManager::set(config_manager, "telemetry.endpoint", "https://telemetry.example.com/api/v1")
  ConfigurationManager::set(config_manager, "telemetry.timeout", 4000)
  
  // Save the configuration
  let save_result = ConfigurationManager::save(config_manager)
  assert_true(save_result.is_ok())
  
  // Create a new configuration manager and load the saved configuration
  let loaded_config_manager = ConfigurationManager::with_persistence("/tmp/test_config.json")
  let load_result = ConfigurationManager::load(loaded_config_manager)
  assert_true(load_result.is_ok())
  
  // Verify the loaded configuration
  let loaded_enabled = ConfigurationManager::get_bool(loaded_config_manager, "telemetry.enabled")
  match loaded_enabled {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let loaded_sample_rate = ConfigurationManager::get_float(loaded_config_manager, "telemetry.sample_rate")
  match loaded_sample_rate {
    Some(value) => assert_eq(value, 0.15)
    None => assert_true(false)
  }
  
  let loaded_endpoint = ConfigurationManager::get_string(loaded_config_manager, "telemetry.endpoint")
  match loaded_endpoint {
    Some(value) => assert_eq(value, "https://telemetry.example.com/api/v1")
    None => assert_true(false)
  }
  
  let loaded_timeout = ConfigurationManager::get_int(loaded_config_manager, "telemetry.timeout")
  match loaded_timeout {
    Some(value) => assert_eq(value, 4000)
    None => assert_true(false)
  }
  
  // Test auto-save
  ConfigurationManager::enable_auto_save(loaded_config_manager, 60000) // Auto-save every minute
  
  // Modify the configuration
  ConfigurationManager::set(loaded_config_manager, "telemetry.sample_rate", 0.2)
  
  // Trigger auto-save (in a real implementation, this would happen automatically)
  let auto_save_result = ConfigurationManager::trigger_auto_save(loaded_config_manager)
  assert_true(auto_save_result.is_ok())
  
  // Create another configuration manager and verify the auto-saved data
  let auto_loaded_config_manager = ConfigurationManager::with_persistence("/tmp/test_config.json")
  let auto_load_result = ConfigurationManager::load(auto_loaded_config_manager)
  assert_true(auto_load_result.is_ok())
  
  let auto_loaded_sample_rate = ConfigurationManager::get_float(auto_loaded_config_manager, "telemetry.sample_rate")
  match auto_loaded_sample_rate {
    Some(value) => assert_eq(value, 0.2)
    None => assert_true(false)
  }
}

// Test 5: Environment Variable Integration
test "environment variable integration" {
  // Create a configuration manager with environment variable support
  let config_manager = ConfigurationManager::with_env_support()
  
  // Map environment variables to configuration keys
  ConfigurationManager::map_env_var(config_manager, "TELEMETRY_ENABLED", "telemetry.enabled")
  ConfigurationManager::map_env_var(config_manager, "TELEMETRY_SAMPLE_RATE", "telemetry.sample_rate")
  ConfigurationManager::map_env_var(config_manager, "TELEMETRY_ENDPOINT", "telemetry.endpoint")
  
  // Set some environment variables (in a real implementation, these would come from the system)
  let env_vars = [
    ("TELEMETRY_ENABLED", "true"),
    ("TELEMETRY_SAMPLE_RATE", "0.25"),
    ("TELEMETRY_ENDPOINT", "https://env-telemetry.example.com")
  ]
  
  // Load configuration from environment variables
  ConfigurationManager::load_from_env(config_manager, env_vars)
  
  // Verify the configuration values
  let env_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match env_enabled {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let env_sample_rate = ConfigurationManager::get_float(config_manager, "telemetry.sample_rate")
  match env_sample_rate {
    Some(value) => assert_eq(value, 0.25)
    None => assert_true(false)
  }
  
  let env_endpoint = ConfigurationManager::get_string(config_manager, "telemetry.endpoint")
  match env_endpoint {
    Some(value) => assert_eq(value, "https://env-telemetry.example.com")
    None => assert_true(false)
  }
  
  // Test environment variable precedence
  ConfigurationManager::set(config_manager, "telemetry.sample_rate", 0.1) // Set directly
  
  // Reload from environment (in a real implementation, this would check if environment variables take precedence)
  ConfigurationManager::reload_from_env(config_manager, env_vars)
  
  // Verify environment variable takes precedence
  let reloaded_sample_rate = ConfigurationManager::get_float(config_manager, "telemetry.sample_rate")
  match reloaded_sample_rate {
    Some(value) => assert_eq(value, 0.25) // Should be from environment
    None => assert_true(false)
  }
  
  // Test environment variable with default value
  ConfigurationManager::map_env_var_with_default(config_manager, "TELEMETRY_TIMEOUT", "telemetry.timeout", "5000")
  
  // Environment variable not set, should use default
  let timeout_with_default = ConfigurationManager::get_int(config_manager, "telemetry.timeout")
  match timeout_with_default {
    Some(value) => assert_eq(value, 5000)
    None => assert_true(false)
  }
}

// Test 6: Configuration Profiles
test "configuration profiles" {
  // Create a configuration manager with profile support
  let config_manager = ConfigurationManager::with_profiles()
  
  // Define profiles
  let development_profile = Profile::new("development")
  Profile::set(development_profile, "telemetry.enabled", false)
  Profile::set(development_profile, "telemetry.sample_rate", 1.0) // Sample everything in development
  Profile::set(development_profile, "telemetry.endpoint", "https://dev-telemetry.example.com")
  Profile::set(development_profile, "logging.level", "debug")
  
  let production_profile = Profile::new("production")
  Profile::set(production_profile, "telemetry.enabled", true)
  Profile::set(production_profile, "telemetry.sample_rate", 0.1) // Sample 10% in production
  Profile::set(production_profile, "telemetry.endpoint", "https://telemetry.example.com")
  Profile::set(production_profile, "logging.level", "info")
  
  let testing_profile = Profile::new("testing")
  Profile::set(testing_profile, "telemetry.enabled", true)
  Profile::set(testing_profile, "telemetry.sample_rate", 1.0) // Sample everything in testing
  Profile::set(testing_profile, "telemetry.endpoint", "https://test-telemetry.example.com")
  Profile::set(testing_profile, "logging.level", "warn")
  
  // Add profiles to the configuration manager
  ConfigurationManager::add_profile(config_manager, development_profile)
  ConfigurationManager::add_profile(config_manager, production_profile)
  ConfigurationManager::add_profile(config_manager, testing_profile)
  
  // Activate the development profile
  ConfigurationManager::activate_profile(config_manager, "development")
  
  // Verify development profile settings
  let dev_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match dev_enabled {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
  
  let dev_sample_rate = ConfigurationManager::get_float(config_manager, "telemetry.sample_rate")
  match dev_sample_rate {
    Some(value) => assert_eq(value, 1.0)
    None => assert_true(false)
  }
  
  let dev_endpoint = ConfigurationManager::get_string(config_manager, "telemetry.endpoint")
  match dev_endpoint {
    Some(value) => assert_eq(value, "https://dev-telemetry.example.com")
    None => assert_true(false)
  }
  
  // Switch to production profile
  ConfigurationManager::activate_profile(config_manager, "production")
  
  // Verify production profile settings
  let prod_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match prod_enabled {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let prod_sample_rate = ConfigurationManager::get_float(config_manager, "telemetry.sample_rate")
  match prod_sample_rate {
    Some(value) => assert_eq(value, 0.1)
    None => assert_true(false)
  }
  
  let prod_endpoint = ConfigurationManager::get_string(config_manager, "telemetry.endpoint")
  match prod_endpoint {
    Some(value) => assert_eq(value, "https://telemetry.example.com")
    None => assert_true(false)
  }
  
  // Test profile inheritance
  let base_profile = Profile::new("base")
  Profile::set(base_profile, "telemetry.enabled", true)
  Profile::set(base_profile, "telemetry.sample_rate", 0.5)
  
  let extended_profile = Profile::new_with_parent("extended", "base")
  Profile::set(extended_profile, "telemetry.sample_rate", 0.1) // Override
  Profile::set(extended_profile, "telemetry.endpoint", "https://extended-telemetry.example.com")
  
  ConfigurationManager::add_profile(config_manager, base_profile)
  ConfigurationManager::add_profile(config_manager, extended_profile)
  
  ConfigurationManager::activate_profile(config_manager, "extended")
  
  // Verify inherited and overridden values
  let ext_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  match ext_enabled {
    Some(value) => assert_true(value) // Inherited from base
    None => assert_true(false)
  }
  
  let ext_sample_rate = ConfigurationManager::get_float(config_manager, "telemetry.sample_rate")
  match ext_sample_rate {
    Some(value) => assert_eq(value, 0.1) // Overridden in extended
    None => assert_true(false)
  }
  
  let ext_endpoint = ConfigurationManager::get_string(config_manager, "telemetry.endpoint")
  match ext_endpoint {
    Some(value) => assert_eq(value, "https://extended-telemetry.example.com") // Only in extended
    None => assert_true(false)
  }
}

// Test 7: Configuration Watchers
test "configuration watchers" {
  // Create a configuration manager with watcher support
  let config_manager = ConfigurationManager::with_watchers()
  
  // Create a watcher
  let watcher = ConfigurationWatcher::new()
  let watched_keys = []
  let watch_count = 0
  
  ConfigurationWatcher::on_change(watcher, fn(key, old_value, new_value) {
    watched_keys.push(key)
    watch_count = watch_count + 1
  })
  
  // Register the watcher
  ConfigurationManager::add_watcher(config_manager, watcher)
  
  // Change configuration values
  ConfigurationManager::set(config_manager, "telemetry.enabled", true)
  ConfigurationManager::set(config_manager, "telemetry.sample_rate", 0.2)
  ConfigurationManager::set(config_manager, "telemetry.endpoint", "https://new-telemetry.example.com")
  
  // Verify the watcher was notified
  assert_eq(watch_count, 3)
  assert_true(watched_keys.includes("telemetry.enabled"))
  assert_true(watched_keys.includes("telemetry.sample_rate"))
  assert_true(watched_keys.includes("telemetry.endpoint"))
  
  // Test selective watching
  let selective_watcher = ConfigurationWatcher::new()
  let selective_watched_keys = []
  let selective_watch_count = 0
  
  ConfigurationWatcher::on_change(selective_watcher, fn(key, old_value, new_value) {
    selective_watched_keys.push(key)
    selective_watch_count = selective_watch_count + 1
  })
  
  // Only watch for telemetry.enabled changes
  ConfigurationManager::add_selective_watcher(config_manager, selective_watcher, ["telemetry.enabled"])
  
  // Change configuration values
  ConfigurationManager::set(config_manager, "telemetry.enabled", false)
  ConfigurationManager::set(config_manager, "telemetry.sample_rate", 0.3)
  
  // Verify the selective watcher was only notified for the watched key
  assert_eq(selective_watch_count, 1)
  assert_true(selective_watched_keys.includes("telemetry.enabled"))
  assert_false(selective_watched_keys.includes("telemetry.sample_rate"))
  
  // Test watcher removal
  ConfigurationManager::remove_watcher(config_manager, watcher)
  
  // Change a configuration value
  ConfigurationManager::set(config_manager, "telemetry.endpoint", "https://another-telemetry.example.com")
  
  // Verify the removed watcher was not notified
  assert_eq(watch_count, 3) // Should still be 3, not increased
}

// Test 8: Configuration Templates
test "configuration templates" {
  // Create a configuration manager with template support
  let config_manager = ConfigurationManager::with_templates()
  
  // Define a configuration template
  let telemetry_template = ConfigurationTemplate::new("telemetry")
  ConfigurationTemplate::add_field(telemetry_template, "enabled", BoolValue(true))
  ConfigurationTemplate::add_field(telemetry_template, "sample_rate", FloatValue(0.1))
  ConfigurationTemplate::add_field(telemetry_template, "endpoint", StringValue("https://telemetry.example.com"))
  ConfigurationTemplate::add_field(telemetry_template, "timeout", IntValue(5000))
  ConfigurationTemplate::add_field(telemetry_template, "retry_attempts", IntValue(3))
  
  // Add the template to the configuration manager
  ConfigurationManager::add_template(config_manager, telemetry_template)
  
  // Create a configuration from the template
  let telemetry_config = ConfigurationManager::create_from_template(config_manager, "telemetry")
  
  // Verify the configuration has all the template fields with default values
  let enabled = ConfigurationManager::get_bool(telemetry_config, "enabled")
  match enabled {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let sample_rate = ConfigurationManager::get_float(telemetry_config, "sample_rate")
  match sample_rate {
    Some(value) => assert_eq(value, 0.1)
    None => assert_true(false)
  }
  
  let endpoint = ConfigurationManager::get_string(telemetry_config, "endpoint")
  match endpoint {
    Some(value) => assert_eq(value, "https://telemetry.example.com")
    None => assert_true(false)
  }
  
  let timeout = ConfigurationManager::get_int(telemetry_config, "timeout")
  match timeout {
    Some(value) => assert_eq(value, 5000)
    None => assert_true(false)
  }
  
  let retry_attempts = ConfigurationManager::get_int(telemetry_config, "retry_attempts")
  match retry_attempts {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  // Create a configuration from the template with custom values
  let custom_values = [
    ("sample_rate", FloatValue(0.2)),
    ("endpoint", StringValue("https://custom-telemetry.example.com")),
    ("timeout", IntValue(10000))
  ]
  
  let custom_config = ConfigurationManager::create_from_template_with_values(
    config_manager,
    "telemetry",
    custom_values
  )
  
  // Verify the configuration has custom values for the specified fields
  let custom_sample_rate = ConfigurationManager::get_float(custom_config, "sample_rate")
  match custom_sample_rate {
    Some(value) => assert_eq(value, 0.2)
    None => assert_true(false)
  }
  
  let custom_endpoint = ConfigurationManager::get_string(custom_config, "endpoint")
  match custom_endpoint {
    Some(value) => assert_eq(value, "https://custom-telemetry.example.com")
    None => assert_true(false)
  }
  
  let custom_timeout = ConfigurationManager::get_int(custom_config, "timeout")
  match custom_timeout {
    Some(value) => assert_eq(value, 10000)
    None => assert_true(false)
  }
  
  // Verify unspecified fields still have default values
  let custom_enabled = ConfigurationManager::get_bool(custom_config, "enabled")
  match custom_enabled {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let custom_retry_attempts = ConfigurationManager::get_int(custom_config, "retry_attempts")
  match custom_retry_attempts {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  // Test template inheritance
  let base_template = ConfigurationTemplate::new("base")
  ConfigurationTemplate::add_field(base_template, "debug", BoolValue(false))
  ConfigurationTemplate::add_field(base_template, "log_level", StringValue("info"))
  
  let extended_template = ConfigurationTemplate::new_with_parent("extended_telemetry", "telemetry")
  ConfigurationTemplate::add_field(extended_template, "batch_size", IntValue(100))
  ConfigurationTemplate::add_field(extended_template, "buffer_size", IntValue(1000))
  
  ConfigurationManager::add_template(config_manager, base_template)
  ConfigurationManager::add_template(config_manager, extended_template)
  
  let extended_config = ConfigurationManager::create_from_template(config_manager, "extended_telemetry")
  
  // Verify the configuration has fields from both the parent and child templates
  assert_true(ConfigurationManager::has(extended_config, "enabled"))
  assert_true(ConfigurationManager::has(extended_config, "sample_rate"))
  assert_true(ConfigurationManager::has(extended_config, "endpoint"))
  assert_true(ConfigurationManager::has(extended_config, "timeout"))
  assert_true(ConfigurationManager::has(extended_config, "retry_attempts"))
  assert_true(ConfigurationManager::has(extended_config, "batch_size"))
  assert_true(ConfigurationManager::has(extended_config, "buffer_size"))
}