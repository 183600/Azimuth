// Azimuth Configuration Management Tests
// This file contains test cases for dynamic configuration management and updates

test "basic configuration loading and validation" {
  let config = Configuration::new()
  
  // Test setting and getting configuration values
  Configuration::set(config, "service.name", "test_service")
  Configuration::set(config, "service.version", "1.0.0")
  Configuration::set(config, "telemetry.enabled", true)
  Configuration::set(config, "telemetry.sampling_rate", 0.5)
  Configuration::set(config, "telemetry.max_spans", 1000)
  
  // Test getting configuration values
  let service_name = Configuration::get_string(config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "test_service")
    None => assert_true(false)
  }
  
  let service_version = Configuration::get_string(config, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  
  let telemetry_enabled = Configuration::get_bool(config, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.5)
    None => assert_true(false)
  }
  
  let max_spans = Configuration::get_int(config, "telemetry.max_spans")
  match max_spans {
    Some(spans) => assert_eq(spans, 1000)
    None => assert_true(false)
  }
  
  // Test getting non-existent configuration value
  let non_existent = Configuration::get_string(config, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "configuration file loading" {
  // Create configuration file content
  let config_content = """
  {
    "service": {
      "name": "file_test_service",
      "version": "2.0.0",
      "instance_id": "instance-12345"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.75,
      "max_spans": 2000,
      "export_interval": 5000
    },
    "logging": {
      "level": "INFO",
      "format": "json"
    }
  }
  """
  
  // Write configuration to file
  let config_file = "/tmp/test_config.json"
  File::write(config_file, config_content)
  
  // Load configuration from file
  let config = Configuration::load_from_file(config_file)
  
  // Verify configuration was loaded correctly
  let service_name = Configuration::get_string(config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "file_test_service")
    None => assert_true(false)
  }
  
  let service_version = Configuration::get_string(config, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "2.0.0")
    None => assert_true(false)
  }
  
  let instance_id = Configuration::get_string(config, "service.instance_id")
  match instance_id {
    Some(id) => assert_eq(id, "instance-12345")
    None => assert_true(false)
  }
  
  let telemetry_enabled = Configuration::get_bool(config, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.75)
    None => assert_true(false)
  }
  
  let max_spans = Configuration::get_int(config, "telemetry.max_spans")
  match max_spans {
    Some(spans) => assert_eq(spans, 2000)
    None => assert_true(false)
  }
  
  let export_interval = Configuration::get_int(config, "telemetry.export_interval")
  match export_interval {
    Some(interval) => assert_eq(interval, 5000)
    None => assert_true(false)
  }
  
  let log_level = Configuration::get_string(config, "logging.level")
  match log_level {
    Some(level) => assert_eq(level, "INFO")
    None => assert_true(false)
  }
  
  let log_format = Configuration::get_string(config, "logging.format")
  match log_format {
    Some(format) => assert_eq(format, "json")
    None => assert_true(false)
  }
  
  // Clean up
  File::delete(config_file)
}

test "dynamic configuration updates" {
  let config = Configuration::new()
  
  // Set initial configuration
  Configuration::set(config, "service.name", "dynamic_service")
  Configuration::set(config, "telemetry.sampling_rate", 0.1)
  Configuration::set(config, "telemetry.max_spans", 100)
  
  // Verify initial configuration
  let service_name = Configuration::get_string(config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "dynamic_service")
    None => assert_true(false)
  }
  
  let sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.1)
    None => assert_true(false)
  }
  
  let max_spans = Configuration::get_int(config, "telemetry.max_spans")
  match max_spans {
    Some(spans) => assert_eq(spans, 100)
    None => assert_true(false)
  }
  
  // Update configuration
  Configuration::set(config, "service.name", "updated_service")
  Configuration::set(config, "telemetry.sampling_rate", 0.9)
  Configuration::set(config, "telemetry.max_spans", 5000)
  
  // Verify updated configuration
  let updated_service_name = Configuration::get_string(config, "service.name")
  match updated_service_name {
    Some(name) => assert_eq(name, "updated_service")
    None => assert_true(false)
  }
  
  let updated_sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match updated_sampling_rate {
    Some(rate) => assert_eq(rate, 0.9)
    None => assert_true(false)
  }
  
  let updated_max_spans = Configuration::get_int(config, "telemetry.max_spans")
  match updated_max_spans {
    Some(spans) => assert_eq(spans, 5000)
    None => assert_true(false)
  }
}

test "configuration validation" {
  let config = Configuration::new()
  
  // Test valid configuration values
  let valid_result1 = Configuration::set_validated(config, "telemetry.sampling_rate", 0.5, fn(v) { v >= 0.0 && v <= 1.0 })
  assert_true(valid_result1)
  
  let valid_result2 = Configuration::set_validated(config, "telemetry.max_spans", 1000, fn(v) { v > 0 })
  assert_true(valid_result2)
  
  // Test invalid configuration values
  let invalid_result1 = Configuration::set_validated(config, "telemetry.sampling_rate", 1.5, fn(v) { v >= 0.0 && v <= 1.0 })
  assert_false(invalid_result1)
  
  let invalid_result2 = Configuration::set_validated(config, "telemetry.max_spans", -100, fn(v) { v > 0 })
  assert_false(invalid_result2)
  
  // Verify invalid values were not set
  let sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.5) // Should still be the valid value
    None => assert_true(false)
  }
  
  let max_spans = Configuration::get_int(config, "telemetry.max_spans")
  match max_spans {
    Some(spans) => assert_eq(spans, 1000) // Should still be the valid value
    None => assert_true(false)
  }
}

test "configuration watchers and callbacks" {
  let config = Configuration::new()
  
  // Set up configuration change watcher
  let mut watch_count = 0
  let mut last_key = ""
  let mut last_value = ""
  
  Configuration::watch(config, "watched.key", fn(key, value) {
    watch_count = watch_count + 1
    last_key = key
    last_value = value
  })
  
  // Change watched configuration
  Configuration::set(config, "watched.key", "initial_value")
  assert_eq(watch_count, 1)
  assert_eq(last_key, "watched.key")
  assert_eq(last_value, "initial_value")
  
  // Change watched configuration again
  Configuration::set(config, "watched.key", "updated_value")
  assert_eq(watch_count, 2)
  assert_eq(last_key, "watched.key")
  assert_eq(last_value, "updated_value")
  
  // Change non-watched configuration
  Configuration::set(config, "unwatched.key", "unwatched_value")
  assert_eq(watch_count, 2) // Should not have changed
  assert_eq(last_key, "watched.key") // Should still be the last watched key
  assert_eq(last_value, "updated_value") // Should still be the last watched value
  
  // Remove watcher
  Configuration::unwatch(config, "watched.key")
  
  // Change configuration after removing watcher
  Configuration::set(config, "watched.key", "final_value")
  assert_eq(watch_count, 2) // Should not have changed
}

test "configuration environment variable overrides" {
  // Set environment variables
  Environment::set("AZIMUTH_SERVICE_NAME", "env_service")
  Environment::set("AZIMUTH_TELEMETRY_ENABLED", "true")
  Environment::set("AZIMUTH_TELEMETRY_SAMPLING_RATE", "0.75")
  Environment::set("AZIMUTH_TELEMETRY_MAX_SPANS", "3000")
  
  // Create configuration with environment variable support
  let config = Configuration::new_with_env()
  
  // Set default configuration
  Configuration::set(config, "service.name", "default_service")
  Configuration::set(config, "telemetry.enabled", false)
  Configuration::set(config, "telemetry.sampling_rate", 0.1)
  Configuration::set(config, "telemetry.max_spans", 100)
  
  // Verify environment variables override defaults
  let service_name = Configuration::get_string(config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "env_service")
    None => assert_true(false)
  }
  
  let telemetry_enabled = Configuration::get_bool(config, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.75)
    None => assert_true(false)
  }
  
  let max_spans = Configuration::get_int(config, "telemetry.max_spans")
  match max_spans {
    Some(spans) => assert_eq(spans, 3000)
    None => assert_true(false)
  }
  
  // Clean up environment variables
  Environment::unset("AZIMUTH_SERVICE_NAME")
  Environment::unset("AZIMUTH_TELEMETRY_ENABLED")
  Environment::unset("AZIMUTH_TELEMETRY_SAMPLING_RATE")
  Environment::unset("AZIMUTH_TELEMETRY_MAX_SPANS")
}

test "configuration profiles" {
  // Create configuration with profiles
  let config = Configuration::new()
  
  // Set default configuration
  Configuration::set(config, "service.name", "default_service")
  Configuration::set(config, "telemetry.sampling_rate", 0.1)
  Configuration::set(config, "logging.level", "INFO")
  
  // Create development profile
  Configuration::create_profile(config, "development")
  Configuration::set_profile(config, "development", "service.name", "dev_service")
  Configuration::set_profile(config, "development", "telemetry.sampling_rate", 1.0)
  Configuration::set_profile(config, "development", "logging.level", "DEBUG")
  
  // Create production profile
  Configuration::create_profile(config, "production")
  Configuration::set_profile(config, "production", "service.name", "prod_service")
  Configuration::set_profile(config, "production", "telemetry.sampling_rate", 0.01)
  Configuration::set_profile(config, "production", "logging.level", "WARN")
  
  // Test default profile
  let default_service_name = Configuration::get_string(config, "service.name")
  match default_service_name {
    Some(name) => assert_eq(name, "default_service")
    None => assert_true(false)
  }
  
  let default_sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match default_sampling_rate {
    Some(rate) => assert_eq(rate, 0.1)
    None => assert_true(false)
  }
  
  let default_log_level = Configuration::get_string(config, "logging.level")
  match default_log_level {
    Some(level) => assert_eq(level, "INFO")
    None => assert_true(false)
  }
  
  // Switch to development profile
  Configuration::use_profile(config, "development")
  
  let dev_service_name = Configuration::get_string(config, "service.name")
  match dev_service_name {
    Some(name) => assert_eq(name, "dev_service")
    None => assert_true(false)
  }
  
  let dev_sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match dev_sampling_rate {
    Some(rate) => assert_eq(rate, 1.0)
    None => assert_true(false)
  }
  
  let dev_log_level = Configuration::get_string(config, "logging.level")
  match dev_log_level {
    Some(level) => assert_eq(level, "DEBUG")
    None => assert_true(false)
  }
  
  // Switch to production profile
  Configuration::use_profile(config, "production")
  
  let prod_service_name = Configuration::get_string(config, "service.name")
  match prod_service_name {
    Some(name) => assert_eq(name, "prod_service")
    None => assert_true(false)
  }
  
  let prod_sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match prod_sampling_rate {
    Some(rate) => assert_eq(rate, 0.01)
    None => assert_true(false)
  }
  
  let prod_log_level = Configuration::get_string(config, "logging.level")
  match prod_log_level {
    Some(level) => assert_eq(level, "WARN")
    None => assert_true(false)
  }
}

test "configuration templates and inheritance" {
  // Create base configuration template
  let base_config = Configuration::new()
  Configuration::set(base_config, "service.name", "template_service")
  Configuration::set(base_config, "service.version", "1.0.0")
  Configuration::set(base_config, "telemetry.enabled", true)
  Configuration::set(base_config, "telemetry.sampling_rate", 0.5)
  
  // Create derived configuration from template
  let derived_config = Configuration::from_template(base_config)
  
  // Override some values in derived configuration
  Configuration::set(derived_config, "service.name", "derived_service")
  Configuration::set(derived_config, "telemetry.sampling_rate", 0.75)
  
  // Add new values to derived configuration
  Configuration::set(derived_config, "service.instance_id", "instance-123")
  
  // Verify derived configuration has all values
  let service_name = Configuration::get_string(derived_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "derived_service") // Overridden value
    None => assert_true(false)
  }
  
  let service_version = Configuration::get_string(derived_config, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0") // Inherited value
    None => assert_true(false)
  }
  
  let instance_id = Configuration::get_string(derived_config, "service.instance_id")
  match instance_id {
    Some(id) => assert_eq(id, "instance-123") // New value
    None => assert_true(false)
  }
  
  let telemetry_enabled = Configuration::get_bool(derived_config, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled) // Inherited value
    None => assert_true(false)
  }
  
  let sampling_rate = Configuration::get_float(derived_config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.75) // Overridden value
    None => assert_true(false)
  }
  
  // Verify base configuration is unchanged
  let base_service_name = Configuration::get_string(base_config, "service.name")
  match base_service_name {
    Some(name) => assert_eq(name, "template_service") // Original value
    None => assert_true(false)
  }
  
  let base_sampling_rate = Configuration::get_float(base_config, "telemetry.sampling_rate")
  match base_sampling_rate {
    Some(rate) => assert_eq(rate, 0.5) // Original value
    None => assert_true(false)
  }
}

test "configuration hot reload" {
  // Create initial configuration file
  let config_file = "/tmp/hot_reload_config.json"
  let initial_content = """
  {
    "service": {
      "name": "initial_service",
      "version": "1.0.0"
    },
    "telemetry": {
      "enabled": true,
      "sampling_rate": 0.5
    }
  }
  """
  
  File::write(config_file, initial_content)
  
  // Load configuration
  let config = Configuration::load_from_file(config_file)
  Configuration::enable_hot_reload(config, config_file)
  
  // Verify initial configuration
  let service_name = Configuration::get_string(config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "initial_service")
    None => assert_true(false)
  }
  
  let sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.5)
    None => assert_true(false)
  }
  
  // Update configuration file
  let updated_content = """
  {
    "service": {
      "name": "updated_service",
      "version": "2.0.0"
    },
    "telemetry": {
      "enabled": false,
      "sampling_rate": 0.75
    }
  }
  """
  
  File::write(config_file, updated_content)
  
  // Trigger hot reload
  Configuration::trigger_hot_reload(config)
  
  // Wait for reload to complete
  Time::sleep(100)
  
  // Verify updated configuration
  let updated_service_name = Configuration::get_string(config, "service.name")
  match updated_service_name {
    Some(name) => assert_eq(name, "updated_service")
    None => assert_true(false)
  }
  
  let updated_service_version = Configuration::get_string(config, "service.version")
  match updated_service_version {
    Some(version) => assert_eq(version, "2.0.0")
    None => assert_true(false)
  }
  
  let telemetry_enabled = Configuration::get_bool(config, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_false(enabled)
    None => assert_true(false)
  }
  
  let updated_sampling_rate = Configuration::get_float(config, "telemetry.sampling_rate")
  match updated_sampling_rate {
    Some(rate) => assert_eq(rate, 0.75)
    None => assert_true(false)
  }
  
  // Clean up
  Configuration::disable_hot_reload(config)
  File::delete(config_file)
}

test "configuration export and import" {
  // Create source configuration
  let source_config = Configuration::new()
  Configuration::set(source_config, "service.name", "export_service")
  Configuration::set(source_config, "service.version", "1.0.0")
  Configuration::set(source_config, "telemetry.enabled", true)
  Configuration::set(source_config, "telemetry.sampling_rate", 0.5)
  Configuration::set(source_config, "telemetry.max_spans", 1000)
  
  // Export configuration to string
  let exported_config = Configuration::export(source_config)
  assert_true(exported_config.length() > 0)
  
  // Import configuration to new config
  let target_config = Configuration::import(exported_config)
  
  // Verify imported configuration
  let service_name = Configuration::get_string(target_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "export_service")
    None => assert_true(false)
  }
  
  let service_version = Configuration::get_string(target_config, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  
  let telemetry_enabled = Configuration::get_bool(target_config, "telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let sampling_rate = Configuration::get_float(target_config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.5)
    None => assert_true(false)
  }
  
  let max_spans = Configuration::get_int(target_config, "telemetry.max_spans")
  match max_spans {
    Some(spans) => assert_eq(spans, 1000)
    None => assert_true(false)
  }
}