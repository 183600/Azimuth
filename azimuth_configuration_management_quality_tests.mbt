// Azimuth Telemetry System - Configuration Management Quality Tests
// This file contains comprehensive test cases for configuration management

// Test 1: Basic Configuration Loading
test "basic configuration loading" {
  // Create configuration with default values
  let config = Configuration::new()
  
  // Verify default values
  assert_eq(Configuration::get_service_name(config), "azimuth-service")
  assert_eq(Configuration::get_service_version(config), "1.0.0")
  assert_eq(Configuration::get_environment(config), "development")
  assert_eq(Configuration::get_sampling_probability(config), 1.0)
  assert_true(Configuration::is_tracing_enabled(config))
  assert_true(Configuration::is_metrics_enabled(config))
  assert_true(Configuration::is_logging_enabled(config))
  
  // Load configuration from map
  let config_map = [
    ("service.name", "test-service"),
    ("service.version", "2.0.0"),
    ("environment", "production"),
    ("sampling.probability", "0.5"),
    ("tracing.enabled", "false"),
    ("metrics.enabled", "true"),
    ("logging.enabled", "false")
  ]
  
  let loaded_config = Configuration::from_map(config_map)
  
  // Verify loaded values
  assert_eq(Configuration::get_service_name(loaded_config), "test-service")
  assert_eq(Configuration::get_service_version(loaded_config), "2.0.0")
  assert_eq(Configuration::get_environment(loaded_config), "production")
  assert_eq(Configuration::get_sampling_probability(loaded_config), 0.5)
  assert_false(Configuration::is_tracing_enabled(loaded_config))
  assert_true(Configuration::is_metrics_enabled(loaded_config))
  assert_false(Configuration::is_logging_enabled(loaded_config))
}

// Test 2: Configuration File Loading
test "configuration file loading" {
  // Create a temporary configuration file
  let config_content = """
  {
    "service": {
      "name": "file-test-service",
      "version": "3.0.0",
      "instance_id": "instance-12345"
    },
    "environment": "staging",
    "tracing": {
      "enabled": true,
      "sampling_probability": 0.75,
      "exporter": "jaeger"
    },
    "metrics": {
      "enabled": true,
      "exporter": "prometheus",
      "port": 9090
    },
    "logging": {
      "enabled": true,
      "level": "info",
      "format": "json"
    }
  }
  """
  
  let temp_file = TempFile::create_with_content(config_content)
  
  // Load configuration from file
  let file_config = Configuration::from_file(temp_file.path)
  
  // Verify loaded values
  assert_eq(Configuration::get_service_name(file_config), "file-test-service")
  assert_eq(Configuration::get_service_version(file_config), "3.0.0")
  assert_eq(Configuration::get_instance_id(file_config), "instance-12345")
  assert_eq(Configuration::get_environment(file_config), "staging")
  assert_true(Configuration::is_tracing_enabled(file_config))
  assert_eq(Configuration::get_sampling_probability(file_config), 0.75)
  assert_eq(Configuration::get_tracing_exporter(file_config), "jaeger")
  assert_true(Configuration::is_metrics_enabled(file_config))
  assert_eq(Configuration::get_metrics_exporter(file_config), "prometheus")
  assert_eq(Configuration::get_metrics_port(file_config), 9090)
  assert_true(Configuration::is_logging_enabled(file_config))
  assert_eq(Configuration::get_logging_level(file_config), "info")
  assert_eq(Configuration::get_logging_format(file_config), "json")
  
  // Clean up
  TempFile::remove(temp_file)
}

// Test 3: Environment Variable Configuration
test "environment variable configuration" {
  // Set environment variables
  Environment::set("AZIMUTH_SERVICE_NAME", "env-test-service")
  Environment::set("AZIMUTH_SERVICE_VERSION", "4.0.0")
  Environment::set("AZIMUTH_ENVIRONMENT", "testing")
  Environment::set("AZIMUTH_SAMPLING_PROBABILITY", "0.25")
  Environment::set("AZIMUTH_TRACING_ENABLED", "true")
  Environment::set("AZIMUTH_METRICS_ENABLED", "false")
  Environment::set("AZIMUTH_LOGGING_ENABLED", "true")
  
  // Load configuration from environment variables
  let env_config = Configuration::from_environment()
  
  // Verify loaded values
  assert_eq(Configuration::get_service_name(env_config), "env-test-service")
  assert_eq(Configuration::get_service_version(env_config), "4.0.0")
  assert_eq(Configuration::get_environment(env_config), "testing")
  assert_eq(Configuration::get_sampling_probability(env_config), 0.25)
  assert_true(Configuration::is_tracing_enabled(env_config))
  assert_false(Configuration::is_metrics_enabled(env_config))
  assert_true(Configuration::is_logging_enabled(env_config))
  
  // Clean up environment variables
  Environment::unset("AZIMUTH_SERVICE_NAME")
  Environment::unset("AZIMUTH_SERVICE_VERSION")
  Environment::unset("AZIMUTH_ENVIRONMENT")
  Environment::unset("AZIMUTH_SAMPLING_PROBABILITY")
  Environment::unset("AZIMUTH_TRACING_ENABLED")
  Environment::unset("AZIMUTH_METRICS_ENABLED")
  Environment::unset("AZIMUTH_LOGGING_ENABLED")
}

// Test 4: Configuration Priority and Merging
test "configuration priority and merging" {
  // Create base configuration
  let base_config = Configuration::new()
  
  // Create override configuration
  let override_map = [
    ("service.name", "override-service"),
    ("environment", "production"),
    ("tracing.enabled", "false")
  ]
  
  let override_config = Configuration::from_map(override_map)
  
  // Merge configurations
  let merged_config = Configuration::merge(base_config, override_config)
  
  // Verify merged values
  assert_eq(Configuration::get_service_name(merged_config), "override-service")  // Overridden
  assert_eq(Configuration::get_service_version(merged_config), "1.0.0")  // From base
  assert_eq(Configuration::get_environment(merged_config), "production")  // Overridden
  assert_false(Configuration::is_tracing_enabled(merged_config))  // Overridden
  assert_true(Configuration::is_metrics_enabled(merged_config))  // From base
  assert_true(Configuration::is_logging_enabled(merged_config))  // From base
  
  // Test multiple level priority: defaults < file < env < explicit
  Environment::set("AZIMUTH_SERVICE_NAME", "env-priority-service")
  
  let file_config_map = [
    ("service.name", "file-priority-service"),
    ("service.version", "5.0.0")
  ]
  
  let file_config = Configuration::from_map(file_config_map)
  let env_config = Configuration::from_environment()
  let explicit_config = Configuration::from_map([("service.version", "6.0.0")])
  
  let priority_config = Configuration::merge_with_priority(
    base_config,    // Lowest priority
    file_config,    // Low priority
    env_config,     // High priority
    explicit_config // Highest priority
  )
  
  // Verify priority order
  assert_eq(Configuration::get_service_name(priority_config), "env-priority-service")  // From env
  assert_eq(Configuration::get_service_version(priority_config), "6.0.0")  // From explicit
  
  // Clean up
  Environment::unset("AZIMUTH_SERVICE_NAME")
}

// Test 5: Configuration Validation
test "configuration validation" {
  // Test valid configuration
  let valid_config_map = [
    ("service.name", "valid-service"),
    ("service.version", "1.0.0"),
    ("sampling.probability", "0.5"),
    ("tracing.enabled", "true"),
    ("metrics.enabled", "true"),
    ("logging.enabled", "true")
  ]
  
  let valid_config = Configuration::from_map(valid_config_map)
  let validation_result = Configuration::validate(valid_config)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test invalid configuration
  let invalid_config_map = [
    ("service.name", ""),  // Empty service name
    ("service.version", "invalid"),  // Invalid version format
    ("sampling.probability", "1.5"),  // Invalid sampling probability (> 1.0)
    ("metrics.port", "invalid_port")  // Invalid port
  ]
  
  let invalid_config = Configuration::from_map(invalid_config_map)
  let invalid_validation_result = Configuration::validate(invalid_config)
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // Verify specific errors
  let has_service_name_error = invalid_validation_result.errors.any(fn(error) {
    StringUtils::contains(error.message, "service.name")
  })
  assert_true(has_service_name_error)
  
  let has_sampling_error = invalid_validation_result.errors.any(fn(error) {
    StringUtils::contains(error.message, "sampling.probability")
  })
  assert_true(has_sampling_error)
}

// Test 6: Dynamic Configuration Updates
test "dynamic configuration updates" {
  // Create initial configuration
  let config = Configuration::new()
  
  // Create configuration manager
  let config_manager = ConfigurationManager::new(config)
  
  // Verify initial values
  assert_eq(ConfigurationManager::get_service_name(config_manager), "azimuth-service")
  assert_eq(ConfigurationManager::get_sampling_probability(config_manager), 1.0)
  
  // Update configuration dynamically
  let updates = [
    ("service.name", "dynamic-service"),
    ("sampling.probability", "0.75")
  ]
  
  ConfigurationManager::update(config_manager, updates)
  
  // Verify updated values
  assert_eq(ConfigurationManager::get_service_name(config_manager), "dynamic-service")
  assert_eq(ConfigurationManager::get_sampling_probability(config_manager), 0.75)
  
  // Test configuration change callbacks
  let callback_called = AtomicBool::new(false)
  
  ConfigurationManager::register_callback(config_manager, fn(key, old_value, new_value) {
    if key == "service.name" {
      AtomicBool::store(callback_called, true)
      assert_eq(old_value, "dynamic-service")
      assert_eq(new_value, "callback-service")
    }
  })
  
  // Update configuration to trigger callback
  ConfigurationManager::update(config_manager, [("service.name", "callback-service")])
  
  // Verify callback was called
  assert_true(AtomicBool::load(callback_called))
}

// Test 7: Configuration Watcher
test "configuration watcher" {
  // Create temporary configuration file
  let initial_config_content = """
  {
    "service": {
      "name": "watcher-test-service",
      "version": "1.0.0"
    },
    "environment": "development"
  }
  """
  
  let temp_file = TempFile::create_with_content(initial_config_content)
  
  // Create configuration watcher
  let config = Configuration::from_file(temp_file.path)
  let watcher = ConfigurationWatcher::new(config, temp_file.path)
  
  // Verify initial values
  assert_eq(Configuration::get_service_name(watcher.config), "watcher-test-service")
  assert_eq(Configuration::get_environment(watcher.config), "development")
  
  // Update configuration file
  let updated_config_content = """
  {
    "service": {
      "name": "updated-watcher-service",
      "version": "2.0.0"
    },
    "environment": "production"
  }
  """
  
  TempFile::update_content(temp_file, updated_config_content)
  
  // Trigger file watching
  ConfigurationWatcher::check_for_changes(watcher)
  
  // Verify updated values
  assert_eq(Configuration::get_service_name(watcher.config), "updated-watcher-service")
  assert_eq(Configuration::get_service_version(watcher.config), "2.0.0")
  assert_eq(Configuration::get_environment(watcher.config), "production")
  
  // Clean up
  ConfigurationWatcher::stop(watcher)
  TempFile::remove(temp_file)
}

// Test 8: Configuration Templates and Profiles
test "configuration templates and profiles" {
  // Create configuration template
  let template = ConfigurationTemplate::new("service-template")
  template.add_parameter("service_name", "default-service")
  template.add_parameter("environment", "development")
  template.add_parameter("log_level", "info")
  
  // Create development profile
  let dev_profile = ConfigurationProfile::new("development")
  dev_profile.set_value("service_name", "dev-service")
  dev_profile.set_value("environment", "development")
  dev_profile.set_value("log_level", "debug")
  dev_profile.set_value("sampling_probability", "1.0")
  
  // Create production profile
  let prod_profile = ConfigurationProfile::new("production")
  prod_profile.set_value("service_name", "prod-service")
  prod_profile.set_value("environment", "production")
  prod_profile.set_value("log_level", "warn")
  prod_profile.set_value("sampling_probability", "0.1")
  
  // Generate configuration from template and profile
  let dev_config = Configuration::from_template_and_profile(template, dev_profile)
  let prod_config = Configuration::from_template_and_profile(template, prod_profile)
  
  // Verify development configuration
  assert_eq(Configuration::get_service_name(dev_config), "dev-service")
  assert_eq(Configuration::get_environment(dev_config), "development")
  assert_eq(Configuration::get_logging_level(dev_config), "debug")
  assert_eq(Configuration::get_sampling_probability(dev_config), 1.0)
  
  // Verify production configuration
  assert_eq(Configuration::get_service_name(prod_config), "prod-service")
  assert_eq(Configuration::get_environment(prod_config), "production")
  assert_eq(Configuration::get_logging_level(prod_config), "warn")
  assert_eq(Configuration::get_sampling_probability(prod_config), 0.1)
}

// Test 9: Configuration Encryption and Security
test "configuration encryption and security" {
  // Create sensitive configuration values
  let sensitive_values = [
    ("database.password", "secret123"),
    ("api.key", "api-key-456"),
    ("auth.token", "auth-token-789")
  ]
  
  // Create encryption key
  let encryption_key = EncryptionKey::generate()
  
  // Encrypt sensitive values
  let encrypted_values = []
  for (key, value) in sensitive_values {
    let encrypted_value = Encryption::encrypt(value, encryption_key)
    encrypted_values.push((key, encrypted_value))
  }
  
  // Create configuration with encrypted values
  let config_map = [
    ("service.name", "secure-service"),
    ("database.host", "localhost")
  ]
  
  for (key, encrypted_value) in encrypted_values {
    config_map.push((key, encrypted_value))
  }
  
  let secure_config = Configuration::from_map(config_map)
  
  // Verify encrypted values are stored
  for (key, _) in sensitive_values {
    let stored_value = Configuration::get_raw(secure_config, key)
    match stored_value {
      Some(value) => assert_not_eq(value, "")  // Value is stored
      None => assert_true(false)  // Value should be stored
    }
  }
  
  // Decrypt sensitive values
  let decrypted_config = Configuration::decrypt_sensitive(secure_config, encryption_key)
  
  // Verify decrypted values
  assert_eq(Configuration::get(decrypted_config, "database.password"), "secret123")
  assert_eq(Configuration::get(decrypted_config, "api.key"), "api-key-456")
  assert_eq(Configuration::get(decrypted_config, "auth.token"), "auth-token-789")
  
  // Verify non-sensitive values are unchanged
  assert_eq(Configuration::get(decrypted_config, "service.name"), "secure-service")
  assert_eq(Configuration::get(decrypted_config, "database.host"), "localhost")
}

// Test 10: Configuration Caching and Performance
test "configuration caching and performance" {
  // Create configuration with many values
  let config_map = []
  for i in 0..1000 {
    config_map.push(("key_" + i.to_string(), "value_" + i.to_string()))
  }
  
  let config = Configuration::from_map(config_map)
  
  // Create configuration cache
  let cache = ConfigurationCache::new(config)
  
  // Measure uncached access time
  let uncached_start = Time::now()
  for i in 0..1000 {
    Configuration::get(config, "key_" + i.to_string())
  }
  let uncached_end = Time::now()
  let uncached_duration = uncached_end - uncached_start
  
  // Measure cached access time
  let cached_start = Time::now()
  for i in 0..1000 {
    ConfigurationCache::get(cache, "key_" + i.to_string())
  }
  let cached_end = Time::now()
  let cached_duration = cached_end - cached_start
  
  // Verify caching improves performance
  let performance_improvement = uncached_duration.to_float() / cached_duration.to_float()
  assert_true(performance_improvement > 2.0)  // At least 2x improvement
  
  // Test cache invalidation
  ConfigurationCache::invalidate(cache, "key_500")
  
  // Verify value is still accessible after invalidation
  let value = ConfigurationCache::get(cache, "key_500")
  match value {
    Some(v) => assert_eq(v, "value_500")
    None => assert_true(false)
  }
  
  // Test cache clearing
  ConfigurationCache::clear(cache)
  
  // Verify cache is empty
  assert_true(ConfigurationCache::is_empty(cache))
  
  // Verify values are still accessible through the underlying configuration
  for i in 0..1000 {
    let value = Configuration::get(config, "key_" + i.to_string())
    match value {
      Some(v) => assert_eq(v, "value_" + i.to_string())
      None => assert_true(false)
    }
  }
}