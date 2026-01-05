// Azimuth Telemetry System - Configuration Management Tests
// This file contains comprehensive test cases for configuration management

// Test 1: Basic Configuration Operations
test "basic configuration operations" {
  // Test configuration creation
  let config = Configuration::new()
  
  // Test setting and getting values
  Configuration::set(config, "app.name", "Azimuth Telemetry")
  Configuration::set(config, "app.version", "1.0.0")
  Configuration::set(config, "app.port", 8080)
  Configuration::set(config, "app.debug", true)
  
  // Test getting string values
  let name = Configuration::get_string(config, "app.name")
  assert_eq(name, "Azimuth Telemetry")
  
  let version = Configuration::get_string(config, "app.version")
  assert_eq(version, "1.0.0")
  
  // Test getting integer values
  let port = Configuration::get_int(config, "app.port")
  assert_eq(port, 8080)
  
  // Test getting boolean values
  let debug = Configuration::get_bool(config, "app.debug")
  assert_eq(debug, true)
  
  // Test getting values with defaults
  let non_existent = Configuration::get_string_with_default(config, "app.nonexistent", "default_value")
  assert_eq(non_existent, "default_value")
  
  let non_existent_int = Configuration::get_int_with_default(config, "app.nonexistent_int", 42)
  assert_eq(non_existent_int, 42)
  
  let non_existent_bool = Configuration::get_bool_with_default(config, "app.nonexistent_bool", false)
  assert_eq(non_existent_bool, false)
  
  // Test checking if key exists
  assert_true(Configuration::has_key(config, "app.name"))
  assert_false(Configuration::has_key(config, "app.nonexistent"))
  
  // Test removing keys
  Configuration::remove(config, "app.debug")
  assert_false(Configuration::has_key(config, "app.debug"))
  
  // Test getting all keys
  let keys = Configuration::get_keys(config)
  assert_eq(keys.length(), 3)  // app.name, app.version, app.port
  assert_true(keys.contains("app.name"))
  assert_true(keys.contains("app.version"))
  assert_true(keys.contains("app.port"))
  
  // Test clearing configuration
  Configuration::clear(config)
  assert_eq(Configuration::get_keys(config).length(), 0)
}

// Test 2: Configuration File Loading
test "configuration file loading" {
  // Test JSON configuration file
  let json_config = """
  {
    "app": {
      "name": "Azimuth Telemetry",
      "version": "1.0.0",
      "port": 8080,
      "debug": true
    },
    "database": {
      "host": "localhost",
      "port": 5432,
      "name": "azimuth_db",
      "ssl": false
    },
    "logging": {
      "level": "info",
      "file": "/var/log/azimuth.log"
    }
  }
  """
  
  let json_file = "/tmp/test_config.json"
  write_file(json_file, json_config)
  
  // Load configuration from JSON file
  let config = Configuration::load_from_file(json_file)
  
  // Verify loaded values
  assert_eq(Configuration::get_string(config, "app.name"), "Azimuth Telemetry")
  assert_eq(Configuration::get_string(config, "app.version"), "1.0.0")
  assert_eq(Configuration::get_int(config, "app.port"), 8080)
  assert_eq(Configuration::get_bool(config, "app.debug"), true)
  
  assert_eq(Configuration::get_string(config, "database.host"), "localhost")
  assert_eq(Configuration::get_int(config, "database.port"), 5432)
  assert_eq(Configuration::get_string(config, "database.name"), "azimuth_db")
  assert_eq(Configuration::get_bool(config, "database.ssl"), false)
  
  assert_eq(Configuration::get_string(config, "logging.level"), "info")
  assert_eq(Configuration::get_string(config, "logging.file"), "/var/log/azimuth.log")
  
  // Test YAML configuration file
  let yaml_config = """
  app:
    name: Azimuth Telemetry
    version: 1.0.0
    port: 8080
    debug: true
  database:
    host: localhost
    port: 5432
    name: azimuth_db
    ssl: false
  logging:
    level: info
    file: /var/log/azimuth.log
  """
  
  let yaml_file = "/tmp/test_config.yaml"
  write_file(yaml_file, yaml_config)
  
  // Load configuration from YAML file
  let yaml_config_loaded = Configuration::load_from_file(yaml_file)
  
  // Verify loaded values
  assert_eq(Configuration::get_string(yaml_config_loaded, "app.name"), "Azimuth Telemetry")
  assert_eq(Configuration::get_string(yaml_config_loaded, "app.version"), "1.0.0")
  assert_eq(Configuration::get_int(yaml_config_loaded, "app.port"), 8080)
  assert_eq(Configuration::get_bool(yaml_config_loaded, "app.debug"), true)
  
  // Test TOML configuration file
  let toml_config = """
  [app]
  name = "Azimuth Telemetry"
  version = "1.0.0"
  port = 8080
  debug = true
  
  [database]
  host = "localhost"
  port = 5432
  name = "azimuth_db"
  ssl = false
  
  [logging]
  level = "info"
  file = "/var/log/azimuth.log"
  """
  
  let toml_file = "/tmp/test_config.toml"
  write_file(toml_file, toml_config)
  
  // Load configuration from TOML file
  let toml_config_loaded = Configuration::load_from_file(toml_file)
  
  // Verify loaded values
  assert_eq(Configuration::get_string(toml_config_loaded, "app.name"), "Azimuth Telemetry")
  assert_eq(Configuration::get_string(toml_config_loaded, "app.version"), "1.0.0")
  assert_eq(Configuration::get_int(toml_config_loaded, "app.port"), 8080)
  assert_eq(Configuration::get_bool(toml_config_loaded, "app.debug"), true)
  
  // Clean up
  delete_file(json_file)
  delete_file(yaml_file)
  delete_file(toml_file)
}

// Test 3: Configuration Environment Variables
test "configuration environment variables" {
  // Set environment variables
  set_env("AZIMUTH_APP_NAME", "Azimuth from Env")
  set_env("AZIMUTH_APP_PORT", "9090")
  set_env("AZIMUTH_APP_DEBUG", "false")
  set_env("AZIMUTH_DB_HOST", "env-host")
  set_env("AZIMUTH_DB_PORT", "5433")
  
  // Load configuration from environment variables
  let config = Configuration::load_from_env("AZIMUTH")
  
  // Verify loaded values
  assert_eq(Configuration::get_string(config, "app.name"), "Azimuth from Env")
  assert_eq(Configuration::get_int(config, "app.port"), 9090)
  assert_eq(Configuration::get_bool(config, "app.debug"), false)
  
  assert_eq(Configuration::get_string(config, "db.host"), "env-host")
  assert_eq(Configuration::get_int(config, "db.port"), 5433)
  
  // Clean up environment variables
  unset_env("AZIMUTH_APP_NAME")
  unset_env("AZIMUTH_APP_PORT")
  unset_env("AZIMUTH_APP_DEBUG")
  unset_env("AZIMUTH_DB_HOST")
  unset_env("AZIMUTH_DB_PORT")
}

// Test 4: Configuration Hierarchy and Merging
test "configuration hierarchy and merging" {
  // Create base configuration
  let base_config = Configuration::new()
  Configuration::set(base_config, "app.name", "Azimuth Telemetry")
  Configuration::set(base_config, "app.version", "1.0.0")
  Configuration::set(base_config, "app.port", 8080)
  Configuration::set(base_config, "app.debug", true)
  Configuration::set(base_config, "database.host", "localhost")
  Configuration::set(base_config, "database.port", 5432)
  Configuration::set(base_config, "database.name", "azimuth_db")
  
  // Create override configuration
  let override_config = Configuration::new()
  Configuration::set(override_config, "app.port", 9090)
  Configuration::set(override_config, "app.debug", false)
  Configuration::set(override_config, "database.host", "prod-server")
  Configuration::set(override_config, "database.ssl", true)
  Configuration::set(override_config, "logging.level", "warn")
  
  // Merge configurations
  let merged_config = Configuration::merge(base_config, override_config)
  
  // Verify merged values (override should take precedence)
  assert_eq(Configuration::get_string(merged_config, "app.name"), "Azimuth Telemetry")  // From base
  assert_eq(Configuration::get_string(merged_config, "app.version"), "1.0.0")  // From base
  assert_eq(Configuration::get_int(merged_config, "app.port"), 9090)  // From override
  assert_eq(Configuration::get_bool(merged_config, "app.debug"), false)  // From override
  
  assert_eq(Configuration::get_string(merged_config, "database.host"), "prod-server")  // From override
  assert_eq(Configuration::get_int(merged_config, "database.port"), 5432)  // From base
  assert_eq(Configuration::get_string(merged_config, "database.name"), "azimuth_db")  // From base
  assert_eq(Configuration::get_bool(merged_config, "database.ssl"), true)  // From override
  
  assert_eq(Configuration::get_string(merged_config, "logging.level"), "warn")  // From override
  
  // Test multi-level merging
  let config1 = Configuration::new()
  Configuration::set(config1, "app.name", "App1")
  Configuration::set(config1, "app.port", 8080)
  Configuration::set(config1, "db.host", "host1")
  
  let config2 = Configuration::new()
  Configuration::set(config2, "app.port", 9090)
  Configuration::set(config2, "db.port", 5432)
  
  let config3 = Configuration::new()
  Configuration::set(config3, "db.host", "host3")
  Configuration::set(config3, "db.name", "testdb")
  
  let multi_merged = Configuration::merge_multiple([config1, config2, config3])
  
  // Verify multi-level merge
  assert_eq(Configuration::get_string(multi_merged, "app.name"), "App1")  // From config1
  assert_eq(Configuration::get_int(multi_merged, "app.port"), 9090)  // From config2
  assert_eq(Configuration::get_string(multi_merged, "db.host"), "host3")  // From config3
  assert_eq(Configuration::get_int(multi_merged, "db.port"), 5432)  // From config2
  assert_eq(Configuration::get_string(multi_merged, "db.name"), "testdb")  // From config3
}

// Test 5: Configuration Validation
test "configuration validation" {
  // Create configuration with validation rules
  let config = Configuration::new()
  Configuration::set(config, "app.name", "Azimuth Telemetry")
  Configuration::set(config, "app.version", "1.0.0")
  Configuration::set(config, "app.port", 8080)
  Configuration::set(config, "app.debug", true)
  Configuration::set(config, "database.host", "localhost")
  Configuration::set(config, "database.port", 5432)
  Configuration::set(config, "database.name", "azimuth_db")
  
  // Define validation rules
  let validator = ConfigurationValidator::new()
  
  // Required fields
  validator.add_required("app.name")
  validator.add_required("app.version")
  validator.add_required("database.host")
  validator.add_required("database.port")
  
  // Type validation
  validator.add_type_rule("app.port", "integer")
  validator.add_type_rule("app.debug", "boolean")
  validator.add_type_rule("database.port", "integer")
  
  // Range validation
  validator.add_range_rule("app.port", 1, 65535)
  validator.add_range_rule("database.port", 1, 65535)
  
  // Pattern validation
  validator.add_pattern_rule("app.version", "^\\d+\\.\\d+\\.\\d+$")  // Semantic version pattern
  validator.add_pattern_rule("database.name", "^[a-zA-Z_][a-zA-Z0-9_]*$")  // Database name pattern
  
  // Custom validation
  validator.add_custom_rule("app.name", |value| {
    match value {
      String(name) => name.length() > 0,
      _ => false
    }
  })
  
  // Validate configuration
  let validation_result = validator.validate(config)
  
  // Verify validation passed
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test validation with errors
  let invalid_config = Configuration::new()
  Configuration::set(invalid_config, "app.name", "")
  Configuration::set(invalid_config, "app.port", "invalid_port")  // String instead of integer
  Configuration::set(invalid_config, "app.debug", "maybe")  // Invalid boolean
  Configuration::set(invalid_config, "database.port", 70000)  // Out of range
  Configuration::set(invalid_config, "app.version", "1.0")  // Invalid pattern
  // Missing required field: database.host
  
  let invalid_result = validator.validate(invalid_config)
  
  // Verify validation failed
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() > 0)
  
  // Check specific errors
  assert_true(invalid_result.errors.any(|error| error.contains("app.name")))
  assert_true(invalid_result.errors.any(|error| error.contains("app.port")))
  assert_true(invalid_result.errors.any(|error| error.contains("app.debug")))
  assert_true(invalid_result.errors.any(|error| error.contains("database.port")))
  assert_true(invalid_result.errors.any(|error| error.contains("app.version")))
  assert_true(invalid_result.errors.any(|error| error.contains("database.host")))
}

// Test 6: Configuration Watchers and Reloading
test "configuration watchers and reloading" {
  // Create configuration file
  let config_content = """
  {
    "app": {
      "name": "Azimuth Telemetry",
      "port": 8080,
      "debug": true
    }
  }
  """
  
  let config_file = "/tmp/watch_config.json"
  write_file(config_file, config_content)
  
  // Load configuration
  let config = Configuration::load_from_file(config_file)
  
  // Verify initial values
  assert_eq(Configuration::get_string(config, "app.name"), "Azimuth Telemetry")
  assert_eq(Configuration::get_int(config, "app.port"), 8080)
  assert_eq(Configuration::get_bool(config, "app.debug"), true)
  
  // Create configuration watcher
  let watcher = ConfigurationWatcher::new(config)
  let mut change_detected = false
  let mut change_key = ""
  
  // Add change callback
  watcher.add_callback(|key, old_value, new_value| {
    change_detected = true
    change_key = key
  })
  
  // Start watching
  watcher.start()
  
  // Update configuration file
  let updated_content = """
  {
    "app": {
      "name": "Updated Azimuth",
      "port": 9090,
      "debug": false
    }
  }
  """
  
  write_file(config_file, updated_content)
  
  // Wait for file watcher to detect changes
  sleep(100)
  
  // Reload configuration
  Configuration::reload(config)
  
  // Verify updated values
  assert_eq(Configuration::get_string(config, "app.name"), "Updated Azimuth")
  assert_eq(Configuration::get_int(config, "app.port"), 9090)
  assert_eq(Configuration::get_bool(config, "app.debug"), false)
  
  // Stop watching
  watcher.stop()
  
  // Clean up
  delete_file(config_file)
}

// Test 7: Configuration Templates and Substitution
test "configuration templates and substitution" {
  // Create configuration with templates
  let config = Configuration::new()
  
  // Set template values
  Configuration::set(config, "app.name", "Azimuth")
  Configuration::set(config, "app.env", "production")
  Configuration::set(config, "app.version", "1.0.0")
  Configuration::set(config, "server.host", "localhost")
  Configuration::set(config, "server.port", 8080)
  
  // Create template configuration
  let template_config = Configuration::new()
  
  // Set values with templates
  Configuration::set(template_config, "instance.id", "${app.name}-${app.env}-${app.version}")
  Configuration::set(template_config, "server.url", "http://${server.host}:${server.port}")
  Configuration::set(template_config, "log.file", "/var/log/${app.name}-${app.env}.log")
  
  // Substitute templates
  let substituted_config = Configuration::substitute_templates(template_config, config)
  
  // Verify substituted values
  assert_eq(Configuration::get_string(substituted_config, "instance.id"), "Azimuth-production-1.0.0")
  assert_eq(Configuration::get_string(substituted_config, "server.url"), "http://localhost:8080")
  assert_eq(Configuration::get_string(substituted_config, "log.file"), "/var/log/Azimuth-production.log")
  
  // Test nested template substitution
  let nested_config = Configuration::new()
  Configuration::set(nested_config, "base.url", "http://${server.host}:${server.port}")
  Configuration::set(nested_config, "api.endpoint", "${base.url}/api/v1")
  
  let nested_substituted = Configuration::substitute_templates(nested_config, config)
  assert_eq(Configuration::get_string(nested_substituted, "api.endpoint"), "http://localhost:8080/api/v1")
  
  // Test template with missing variable
  let incomplete_config = Configuration::new()
  Configuration::set(incomplete_config, "test.value", "${missing.variable}")
  
  let incomplete_substituted = Configuration::substitute_templates(incomplete_config, config)
  assert_eq(Configuration::get_string(incomplete_substituted, "test.value"), "${missing.variable}")
}

// Test 8: Configuration Encryption and Security
test "configuration encryption and security" {
  // Create configuration with sensitive data
  let config = Configuration::new()
  Configuration::set(config, "app.name", "Azimuth Telemetry")
  Configuration::set(config, "database.password", "secret_password")
  Configuration::set(config, "api.key", "api_key_12345")
  Configuration::set(config, "jwt.secret", "jwt_secret_key")
  
  // Create encryption key
  let encryption_key = "test_encryption_key_12345"
  
  // Encrypt sensitive values
  let encrypted_config = Configuration::encrypt_sensitive(config, encryption_key, ["database.password", "api.key", "jwt.secret"])
  
  // Verify encrypted values
  let encrypted_password = Configuration::get_string(encrypted_config, "database.password")
  let encrypted_api_key = Configuration::get_string(encrypted_config, "api.key")
  let encrypted_jwt_secret = Configuration::get_string(encrypted_config, "jwt.secret")
  
  // Encrypted values should not match original
  assert_ne(encrypted_password, "secret_password")
  assert_ne(encrypted_api_key, "api_key_12345")
  assert_ne(encrypted_jwt_secret, "jwt_secret_key")
  
  // Non-sensitive values should remain unchanged
  assert_eq(Configuration::get_string(encrypted_config, "app.name"), "Azimuth Telemetry")
  
  // Decrypt sensitive values
  let decrypted_config = Configuration::decrypt_sensitive(encrypted_config, encryption_key, ["database.password", "api.key", "jwt.secret"])
  
  // Verify decrypted values match original
  assert_eq(Configuration::get_string(decrypted_config, "database.password"), "secret_password")
  assert_eq(Configuration::get_string(decrypted_config, "api.key"), "api_key_12345")
  assert_eq(Configuration::get_string(decrypted_config, "jwt.secret"), "jwt_secret_key")
  assert_eq(Configuration::get_string(decrypted_config, "app.name"), "Azimuth Telemetry")
  
  // Test decryption with wrong key
  let wrong_key = "wrong_encryption_key"
  let wrong_decrypted = Configuration::decrypt_sensitive(encrypted_config, wrong_key, ["database.password", "api.key", "jwt.secret"])
  
  // Values should remain encrypted
  assert_ne(Configuration::get_string(wrong_decrypted, "database.password"), "secret_password")
  assert_ne(Configuration::get_string(wrong_decrypted, "api.key"), "api_key_12345")
  assert_ne(Configuration::get_string(wrong_decrypted, "jwt.secret"), "jwt_secret_key")
}

// Test 9: Configuration Profiles and Environments
test "configuration profiles and environments" {
  // Create base configuration
  let base_config = Configuration::new()
  Configuration::set(base_config, "app.name", "Azimuth Telemetry")
  Configuration::set(base_config, "app.version", "1.0.0")
  Configuration::set(base_config, "database.host", "localhost")
  Configuration::set(base_config, "database.port", 5432)
  Configuration::set(base_config, "logging.level", "info")
  
  // Create development profile
  let dev_config = Configuration::new()
  Configuration::set(dev_config, "app.debug", true)
  Configuration::set(dev_config, "database.name", "azimuth_dev")
  Configuration::set(dev_config, "logging.level", "debug")
  
  // Create production profile
  let prod_config = Configuration::new()
  Configuration::set(prod_config, "app.debug", false)
  Configuration::set(prod_config, "database.host", "prod-server")
  Configuration::set(prod_config, "database.name", "azimuth_prod")
  Configuration::set(prod_config, "database.ssl", true)
  Configuration::set(prod_config, "logging.level", "warn")
  
  // Create testing profile
  let test_config = Configuration::new()
  Configuration::set(test_config, "app.debug", true)
  Configuration::set(test_config, "database.name", "azimuth_test")
  Configuration::set(test_config, "database.host", "test-server")
  Configuration::set(test_config, "logging.level", "debug")
  
  // Create profile manager
  let profile_manager = ConfigurationProfileManager::new(base_config)
  profile_manager.add_profile("development", dev_config)
  profile_manager.add_profile("production", prod_config)
  profile_manager.add_profile("testing", test_config)
  
  // Test development profile
  let dev_active = profile_manager.activate_profile("development")
  assert_eq(Configuration::get_string(dev_active, "app.name"), "Azimuth Telemetry")  // From base
  assert_eq(Configuration::get_string(dev_active, "app.version"), "1.0.0")  // From base
  assert_eq(Configuration::get_bool(dev_active, "app.debug"), true)  // From dev profile
  assert_eq(Configuration::get_string(dev_active, "database.host"), "localhost")  // From base
  assert_eq(Configuration::get_string(dev_active, "database.name"), "azimuth_dev")  // From dev profile
  assert_eq(Configuration::get_string(dev_active, "logging.level"), "debug")  // From dev profile
  
  // Test production profile
  let prod_active = profile_manager.activate_profile("production")
  assert_eq(Configuration::get_string(prod_active, "app.name"), "Azimuth Telemetry")  // From base
  assert_eq(Configuration::get_string(prod_active, "app.version"), "1.0.0")  // From base
  assert_eq(Configuration::get_bool(prod_active, "app.debug"), false)  // From prod profile
  assert_eq(Configuration::get_string(prod_active, "database.host"), "prod-server")  // From prod profile
  assert_eq(Configuration::get_string(prod_active, "database.name"), "azimuth_prod")  // From prod profile
  assert_eq(Configuration::get_bool(prod_active, "database.ssl"), true)  // From prod profile
  assert_eq(Configuration::get_string(prod_active, "logging.level"), "warn")  // From prod profile
  
  // Test testing profile
  let test_active = profile_manager.activate_profile("testing")
  assert_eq(Configuration::get_string(test_active, "app.name"), "Azimuth Telemetry")  // From base
  assert_eq(Configuration::get_string(test_active, "app.version"), "1.0.0")  // From base
  assert_eq(Configuration::get_bool(test_active, "app.debug"), true)  // From test profile
  assert_eq(Configuration::get_string(test_active, "database.host"), "test-server")  // From test profile
  assert_eq(Configuration::get_string(test_active, "database.name"), "azimuth_test")  // From test profile
  assert_eq(Configuration::get_string(test_active, "logging.level"), "debug")  // From test profile
  
  // Test profile inheritance
  let extended_config = Configuration::new()
  Configuration::set(extended_config, "app.feature_flags", ["feature1", "feature2"])
  
  let extended_prod = Configuration::merge(prod_config, extended_config)
  profile_manager.add_profile("production_extended", extended_prod)
  
  let extended_prod_active = profile_manager.activate_profile("production_extended")
  assert_eq(Configuration::get_string(extended_prod_active, "database.name"), "azimuth_prod")  // From prod
  assert_eq(Configuration::get_array(extended_prod_active, "app.feature_flags"), ["feature1", "feature2"])  // From extended
}

// Test 10: Comprehensive Configuration Management Scenario
test "comprehensive configuration management scenario" {
  // Create a comprehensive configuration management scenario
  let scenario = ConfigurationManagerScenario::new()
  
  // Step 1: Create base configuration
  let base_config_path = "/tmp/base_config.json"
  let base_config_content = """
  {
    "app": {
      "name": "Azimuth Telemetry",
      "version": "1.0.0",
      "port": 8080
    },
    "database": {
      "host": "localhost",
      "port": 5432
    },
    "logging": {
      "level": "info"
    }
  }
  """
  write_file(base_config_path, base_config_content)
  
  // Step 2: Create environment-specific configurations
  let dev_config_path = "/tmp/dev_config.json"
  let dev_config_content = """
  {
    "app": {
      "debug": true
    },
    "database": {
      "name": "azimuth_dev"
    },
    "logging": {
      "level": "debug"
    }
  }
  """
  write_file(dev_config_path, dev_config_content)
  
  let prod_config_path = "/tmp/prod_config.json"
  let prod_config_content = """
  {
    "app": {
      "debug": false
    },
    "database": {
      "host": "prod-server",
      "name": "azimuth_prod",
      "ssl": true
    },
    "logging": {
      "level": "warn"
    }
  }
  """
  write_file(prod_config_path, prod_config_content)
  
  // Step 3: Set environment variables
  set_env("AZIMUTH_APP_PORT", "9090")
  set_env("AZIMUTH_DB_PASSWORD", "encrypted_password")
  
  // Step 4: Create configuration with validation rules
  let validator = ConfigurationValidator::new()
  validator.add_required("app.name")
  validator.add_required("app.version")
  validator.add_required("database.host")
  validator.add_required("database.port")
  validator.add_type_rule("app.port", "integer")
  validator.add_type_rule("database.port", "integer")
  validator.add_range_rule("app.port", 1, 65535)
  validator.add_range_rule("database.port", 1, 65535)
  
  // Step 5: Execute configuration scenario
  let config = scenario.execute(|| {
    // Load base configuration
    let config = Configuration::load_from_file(base_config_path)
    
    // Load environment-specific configuration based on environment variable
    let env = get_env_with_default("AZIMUTH_ENV", "development")
    let env_config_path = if env == "production" { prod_config_path } else { dev_config_path }
    let env_config = Configuration::load_from_file(env_config_path)
    
    // Merge configurations
    let merged_config = Configuration::merge(config, env_config)
    
    // Override with environment variables
    let env_overrides = Configuration::load_from_env("AZIMUTH")
    let final_config = Configuration::merge(merged_config, env_overrides)
    
    // Validate configuration
    let validation_result = validator.validate(final_config)
    if !validation_result.is_valid {
      throw ConfigurationError("Invalid configuration: " + validation_result.errors.join(", "))
    }
    
    // Encrypt sensitive values
    let encryption_key = "scenario_encryption_key"
    let encrypted_config = Configuration::encrypt_sensitive(final_config, encryption_key, ["database.password"])
    
    return encrypted_config
  })
  
  // Step 6: Verify configuration
  assert_eq(Configuration::get_string(config, "app.name"), "Azimuth Telemetry")
  assert_eq(Configuration::get_string(config, "app.version"), "1.0.0")
  assert_eq(Configuration::get_int(config, "app.port"), 9090)  // From environment variable
  assert_eq(Configuration::get_bool(config, "app.debug"), true)  // From dev config
  
  assert_eq(Configuration::get_string(config, "database.host"), "localhost")  // From base config
  assert_eq(Configuration::get_int(config, "database.port"), 5432)  // From base config
  assert_eq(Configuration::get_string(config, "database.name"), "azimuth_dev")  // From dev config
  assert_eq(Configuration::get_string(config, "database.password"), "encrypted_password")  // From env
  
  assert_eq(Configuration::get_string(config, "logging.level"), "debug")  // From dev config
  
  // Step 7: Test configuration change
  set_env("AZIMUTH_ENV", "production")
  let prod_config = scenario.execute(|| {
    // Load base configuration
    let config = Configuration::load_from_file(base_config_path)
    
    // Load environment-specific configuration based on environment variable
    let env = get_env_with_default("AZIMUTH_ENV", "development")
    let env_config_path = if env == "production" { prod_config_path } else { dev_config_path }
    let env_config = Configuration::load_from_file(env_config_path)
    
    // Merge configurations
    let merged_config = Configuration::merge(config, env_config)
    
    // Override with environment variables
    let env_overrides = Configuration::load_from_env("AZIMUTH")
    let final_config = Configuration::merge(merged_config, env_overrides)
    
    // Validate configuration
    let validation_result = validator.validate(final_config)
    if !validation_result.is_valid {
      throw ConfigurationError("Invalid configuration: " + validation_result.errors.join(", "))
    }
    
    // Encrypt sensitive values
    let encryption_key = "scenario_encryption_key"
    let encrypted_config = Configuration::encrypt_sensitive(final_config, encryption_key, ["database.password"])
    
    return encrypted_config
  })
  
  // Verify production configuration
  assert_eq(Configuration::get_string(prod_config, "app.name"), "Azimuth Telemetry")
  assert_eq(Configuration::get_string(prod_config, "app.version"), "1.0.0")
  assert_eq(Configuration::get_int(prod_config, "app.port"), 9090)  // From environment variable
  assert_eq(Configuration::get_bool(prod_config, "app.debug"), false)  // From prod config
  
  assert_eq(Configuration::get_string(prod_config, "database.host"), "prod-server")  // From prod config
  assert_eq(Configuration::get_int(prod_config, "database.port"), 5432)  // From base config
  assert_eq(Configuration::get_string(prod_config, "database.name"), "azimuth_prod")  // From prod config
  assert_eq(Configuration::get_bool(prod_config, "database.ssl"), true)  // From prod config
  assert_eq(Configuration::get_string(prod_config, "database.password"), "encrypted_password")  // From env
  
  assert_eq(Configuration::get_string(prod_config, "logging.level"), "warn")  // From prod config
  
  // Clean up
  delete_file(base_config_path)
  delete_file(dev_config_path)
  delete_file(prod_config_path)
  unset_env("AZIMUTH_APP_PORT")
  unset_env("AZIMUTH_DB_PASSWORD")
  unset_env("AZIMUTH_ENV")
}