// Azimuth Telemetry System - Configuration Management Tests
// This file contains test cases for configuration management functionality

// Test 1: Configuration Loading and Parsing
test "configuration loading and parsing" {
  let config_manager = ConfigurationManager::new()
  
  // Test JSON configuration loading
  let json_config = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "version": "1.0.0",
      "port": 8080,
      "host": "localhost"
    },
    "database": {
      "connection_string": "postgresql://localhost:5432/azimuth",
      "max_connections": 10,
      "timeout": 30000
    },
    "logging": {
      "level": "info",
      "format": "json",
      "output": "console"
    }
  }
  """
  
  let config_result = config_manager.load_from_string(json_config, "json")
  assert_true(config_result.is_success)
  
  let config = config_result.config
  assert_eq(config.get("service.name"), Some("azimuth-telemetry"))
  assert_eq(config.get("service.version"), Some("1.0.0"))
  assert_eq(config.get("service.port"), Some("8080"))
  assert_eq(config.get("database.max_connections"), Some("10"))
  
  // Test YAML configuration loading
  let yaml_config = """
  service:
    name: azimuth-telemetry
    version: "1.0.0"
    port: 8080
    host: localhost
  telemetry:
    enabled: true
    sampling_rate: 0.1
    export_interval: 60000
  """
  
  let yaml_result = config_manager.load_from_string(yaml_config, "yaml")
  assert_true(yaml_result.is_success)
  
  let yaml_config_obj = yaml_result.config
  assert_eq(yaml_config_obj.get("service.name"), Some("azimuth-telemetry"))
  assert_eq(yaml_config_obj.get("telemetry.enabled"), Some("true"))
  assert_eq(yaml_config_obj.get("telemetry.sampling_rate"), Some("0.1"))
  
  // Test TOML configuration loading
  let toml_config = """
  [service]
  name = "azimuth-telemetry"
  version = "1.0.0"
  
  [database]
  connection_string = "postgresql://localhost:5432/azimuth"
  max_connections = 10
  
  [cache]
  enabled = true
  ttl = 3600
  """
  
  let toml_result = config_manager.load_from_string(toml_config, "toml")
  assert_true(toml_result.is_success)
  
  let toml_config_obj = toml_result.config
  assert_eq(toml_config_obj.get("service.name"), Some("azimuth-telemetry"))
  assert_eq(toml_config_obj.get("cache.enabled"), Some("true"))
  assert_eq(toml_config_obj.get("cache.ttl"), Some("3600"))
}

// Test 2: Configuration Validation
test "configuration validation" {
  let config_manager = ConfigurationManager::new()
  
  // Define validation schema
  let validation_schema = ValidationSchema::new()
    .add_required_field("service.name", StringType)
    .add_required_field("service.port", IntegerType)
    .add_optional_field("service.host", StringType, "localhost")
    .add_required_field("database.connection_string", StringType)
    .add_optional_field("database.max_connections", IntegerType, 5)
    .add_range_constraint("service.port", 1, 65535)
    .add_range_constraint("database.max_connections", 1, 100)
  
  config_manager.set_validation_schema(validation_schema)
  
  // Test valid configuration
  let valid_config = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "port": 8080,
      "host": "localhost"
    },
    "database": {
      "connection_string": "postgresql://localhost:5432/azimuth",
      "max_connections": 10
    }
  }
  """
  
  let valid_result = config_manager.validate_config(valid_config)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid configuration (missing required fields)
  let invalid_config1 = """
  {
    "service": {
      "port": 8080
    },
    "database": {
      "max_connections": 10
    }
  }
  """
  
  let invalid_result1 = config_manager.validate_config(invalid_config1)
  assert_false(invalid_result1.is_valid)
  assert_true(invalid_result1.errors.length() >= 2)
  assert_true(invalid_result1.errors.any(fn(e) { e.contains("service.name") }))
  assert_true(invalid_result1.errors.any(fn(e) { e.contains("database.connection_string") }))
  
  // Test invalid configuration (out of range values)
  let invalid_config2 = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "port": 70000,
      "host": "localhost"
    },
    "database": {
      "connection_string": "postgresql://localhost:5432/azimuth",
      "max_connections": 150
    }
  }
  """
  
  let invalid_result2 = config_manager.validate_config(invalid_config2)
  assert_false(invalid_result2.is_valid)
  assert_true(invalid_result2.errors.length() >= 2)
  assert_true(invalid_result2.errors.any(fn(e) { e.contains("service.port") && e.contains("range") }))
  assert_true(invalid_result2.errors.any(fn(e) { e.contains("database.max_connections") && e.contains("range") }))
  
  // Test custom validation rules
  let custom_validator = fn(key: String, value: String) -> Result[Bool, String] {
    match key {
      "service.name" => {
        if value.length() >= 3 && value.length() <= 50 {
          Ok(true)
        } else {
          Err("Service name must be between 3 and 50 characters")
        }
      }
      "database.connection_string" => {
        if value.contains("postgresql://") || value.contains("mysql://") {
          Ok(true)
        } else {
          Err("Database connection string must be a valid URL")
        }
      }
      _ => Ok(true)
    }
  }
  
  config_manager.add_custom_validator(custom_validator)
  
  let custom_invalid_config = """
  {
    "service": {
      "name": "ab",
      "port": 8080
    },
    "database": {
      "connection_string": "invalid_connection_string",
      "max_connections": 10
    }
  }
  """
  
  let custom_invalid_result = config_manager.validate_config(custom_invalid_config)
  assert_false(custom_invalid_result.is_valid)
  assert_true(custom_invalid_result.errors.any(fn(e) { e.contains("Service name must be between 3 and 50 characters") }))
  assert_true(custom_invalid_result.errors.any(fn(e) { e.contains("Database connection string must be a valid URL") }))
}

// Test 3: Configuration Merging and Inheritance
test "configuration merging and inheritance" {
  let config_manager = ConfigurationManager::new()
  
  // Define base configuration
  let base_config = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "version": "1.0.0",
      "port": 8080,
      "host": "localhost"
    },
    "database": {
      "connection_string": "postgresql://localhost:5432/azimuth",
      "max_connections": 10,
      "timeout": 30000
    },
    "logging": {
      "level": "info",
      "format": "json"
    }
  }
  """
  
  // Define environment-specific configuration
  let env_config = """
  {
    "service": {
      "port": 9090,
      "host": "0.0.0.0"
    },
    "database": {
      "connection_string": "postgresql://prod-db:5432/azimuth",
      "max_connections": 20
    },
    "logging": {
      "level": "debug"
    }
  }
  """
  
  // Load base configuration
  let base_result = config_manager.load_from_string(base_config, "json")
  assert_true(base_result.is_success)
  let base_config_obj = base_result.config
  
  // Load environment configuration
  let env_result = config_manager.load_from_string(env_config, "json")
  assert_true(env_result.is_success)
  let env_config_obj = env_result.config
  
  // Merge configurations
  let merged_config = config_manager.merge_configs(base_config_obj, env_config_obj)
  
  // Verify merged configuration
  assert_eq(merged_config.get("service.name"), Some("azimuth-telemetry")) // From base
  assert_eq(merged_config.get("service.version"), Some("1.0.0")) // From base
  assert_eq(merged_config.get("service.port"), Some("9090")) // From env (override)
  assert_eq(merged_config.get("service.host"), Some("0.0.0.0")) // From env (override)
  assert_eq(merged_config.get("database.connection_string"), Some("postgresql://prod-db:5432/azimuth")) // From env (override)
  assert_eq(merged_config.get("database.max_connections"), Some("20")) // From env (override)
  assert_eq(merged_config.get("database.timeout"), Some("30000")) // From base
  assert_eq(merged_config.get("logging.level"), Some("debug")) // From env (override)
  assert_eq(merged_config.get("logging.format"), Some("json")) // From base
  
  // Test configuration inheritance with multiple levels
  let production_config = """
  {
    "service": {
      "port": 80
    },
    "database": {
      "connection_string": "postgresql://prod-cluster:5432/azimuth"
    }
  }
  """
  
  let prod_result = config_manager.load_from_string(production_config, "json")
  assert_true(prod_result.is_success)
  let prod_config_obj = prod_result.config
  
  // Create inheritance chain: base -> env -> prod
  let inherited_config = config_manager.inherit_configs([base_config_obj, env_config_obj, prod_config_obj])
  
  // Verify inherited configuration
  assert_eq(inherited_config.get("service.name"), Some("azimuth-telemetry")) // From base
  assert_eq(inherited_config.get("service.version"), Some("1.0.0")) // From base
  assert_eq(inherited_config.get("service.port"), Some("80")) // From prod (final override)
  assert_eq(inherited_config.get("service.host"), Some("0.0.0.0")) // From env
  assert_eq(inherited_config.get("database.connection_string"), Some("postgresql://prod-cluster:5432/azimuth")) // From prod (final override)
  assert_eq(inherited_config.get("database.max_connections"), Some("20")) // From env
  assert_eq(inherited_config.get("database.timeout"), Some("30000")) // From base
}

// Test 4: Configuration Environment Variables and Substitution
test "configuration environment variables and substitution" {
  let config_manager = ConfigurationManager::new()
  
  // Set environment variables
  Environment::set_var("AZIMUTH_SERVICE_PORT", "9090")
  Environment::set_var("AZIMUTH_DB_HOST", "prod-db.example.com")
  Environment::set_var("AZIMUTH_DB_PASSWORD", "secret123")
  Environment::set_var("AZIMUTH_LOG_LEVEL", "debug")
  
  // Test configuration with environment variable substitution
  let config_with_env_vars = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "port": "${AZIMUTH_SERVICE_PORT}",
      "host": "0.0.0.0"
    },
    "database": {
      "connection_string": "postgresql://${AZIMUTH_DB_HOST}:5432/azimuth?password=${AZIMUTH_DB_PASSWORD}",
      "max_connections": 10
    },
    "logging": {
      "level": "${AZIMUTH_LOG_LEVEL}",
      "format": "json"
    }
  }
  """
  
  let result = config_manager.load_from_string(config_with_env_vars, "json")
  assert_true(result.is_success)
  
  let resolved_config = result.config
  assert_eq(resolved_config.get("service.port"), Some("9090"))
  assert_eq(resolved_config.get("database.connection_string"), Some("postgresql://prod-db.example.com:5432/azimuth?password=secret123"))
  assert_eq(resolved_config.get("logging.level"), Some("debug"))
  
  // Test configuration with default values for missing environment variables
  let config_with_defaults = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "port": "${AZIMUTH_SERVICE_PORT:8080}",
      "host": "${AZIMUTH_SERVICE_HOST:localhost}"
    },
    "feature_flags": {
      "enable_metrics": "${AZIMUTH_ENABLE_METRICS:false}",
      "enable_tracing": "${AZIMUTH_ENABLE_TRACING:true}"
    }
  }
  """
  
  // Unset some environment variables to test defaults
  Environment::unset_var("AZIMUTH_SERVICE_HOST")
  Environment::unset_var("AZIMUTH_ENABLE_METRICS")
  
  let default_result = config_manager.load_from_string(config_with_defaults, "json")
  assert_true(default_result.is_success)
  
  let default_config = default_result.config
  assert_eq(default_config.get("service.port"), Some("9090")) // From env var
  assert_eq(default_config.get("service.host"), Some("localhost")) // From default value
  assert_eq(default_config.get("feature_flags.enable_metrics"), Some("false")) // From default value
  assert_eq(default_config.get("feature_flags.enable_tracing"), Some("true")) // From env var
  
  // Test configuration with computed values
  let config_with_computed = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "base_url": "http://${AZIMUTH_SERVICE_HOST:localhost}:${AZIMUTH_SERVICE_PORT:8080}",
      "full_name": "${service.name}-${service.version:1.0.0}"
    }
  }
  """
  
  let computed_result = config_manager.load_from_string(config_with_computed, "json")
  assert_true(computed_result.is_success)
  
  let computed_config = computed_result.config
  assert_eq(computed_config.get("service.base_url"), Some("http://localhost:9090"))
  assert_eq(computed_config.get("service.full_name"), Some("azimuth-telemetry-1.0.0"))
}

// Test 5: Configuration Hot Reload and Watch
test "configuration hot reload and watch" {
  let config_manager = ConfigurationManager::new()
  
  // Create temporary configuration file
  let temp_config_file = "/tmp/azimuth_config.json"
  let initial_config = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "port": 8080
    },
    "feature_flags": {
      "enable_metrics": true,
      "enable_tracing": false
    }
  }
  """
  
  File::write_string(temp_config_file, initial_config)
  
  // Load initial configuration
  let initial_result = config_manager.load_from_file(temp_config_file)
  assert_true(initial_result.is_success)
  
  let initial_config_obj = initial_result.config
  assert_eq(initial_config_obj.get("service.port"), Some("8080"))
  assert_eq(initial_config_obj.get("feature_flags.enable_tracing"), Some("false"))
  
  // Set up configuration watcher
  let watcher = config_manager.create_watcher(temp_config_file)
  let mut reload_count = 0
  let mut last_config = initial_config_obj
  
  watcher.on_reload(fn(config: Configuration) {
    reload_count = reload_count + 1
    last_config = config
  })
  
  // Start watching
  watcher.start()
  
  // Modify configuration file
  let updated_config = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "port": 9090
    },
    "feature_flags": {
      "enable_metrics": true,
      "enable_tracing": true
    }
  }
  """
  
  File::write_string(temp_config_file, updated_config)
  
  // Wait for reload (simulate)
  Thread::sleep(100) // 100ms
  
  // Verify configuration was reloaded
  assert_eq(reload_count, 1)
  assert_eq(last_config.get("service.port"), Some("9090"))
  assert_eq(last_config.get("feature_flags.enable_tracing"), Some("true"))
  
  // Test configuration validation on reload
  let validation_schema = ValidationSchema::new()
    .add_required_field("service.name", StringType)
    .add_range_constraint("service.port", 1, 65535)
  
  config_manager.set_validation_schema(validation_schema)
  
  // Try to write invalid configuration
  let invalid_config = """
  {
    "service": {
      "port": 70000
    },
    "feature_flags": {
      "enable_metrics": true,
      "enable_tracing": true
    }
  }
  """
  
  File::write_string(temp_config_file, invalid_config)
  Thread::sleep(100) // 100ms
  
  // Configuration should not be reloaded due to validation error
  assert_eq(reload_count, 1) // Still 1, not incremented
  assert_eq(last_config.get("service.port"), Some("9090")) // Still the old valid config
  
  // Test reload error handling
  let reload_errors = watcher.get_reload_errors()
  assert_true(reload_errors.length() > 0)
  assert_true(reload_errors.any(fn(e) { e.contains("validation") || e.contains("service.name") }))
  
  // Stop watching
  watcher.stop()
  
  // Clean up
  File::delete(temp_config_file)
}

// Test 6: Configuration Encryption and Security
test "configuration encryption and security" {
  let config_manager = ConfigurationManager::new()
  
  // Test configuration encryption
  let sensitive_config = """
  {
    "database": {
      "connection_string": "postgresql://user:password123@localhost:5432/azimuth",
      "ssl_cert": "-----BEGIN CERTIFICATE-----\nMIIC...-----END CERTIFICATE-----"
    },
    "api_keys": {
      "payment_provider": "sk_live_1234567890abcdef",
      "analytics": "ga_secret_9876543210fedcba"
    }
  }
  """
  
  // Create encryption key
  let encryption_key = EncryptionKey::generate()
  
  // Encrypt sensitive configuration
  let encrypted_config = config_manager.encrypt_config(sensitive_config, encryption_key)
  assert_true(encrypted_config.encrypted)
  assert_true(encrypted_config.data != sensitive_config)
  assert_true(encrypted_config.iv.length() > 0)
  assert_true(encrypted_config.tag.length() > 0)
  
  // Decrypt configuration
  let decrypted_config = config_manager.decrypt_config(encrypted_config, encryption_key)
  assert_eq(decrypted_config, sensitive_config)
  
  // Test configuration with encrypted fields
  let config_with_encrypted_fields = """
  {
    "service": {
      "name": "azimuth-telemetry"
    },
    "database": {
      "connection_string": "encrypted:eyJlbmNyeXB0ZWQiOnsiYWxnb3JpdGhtIjoiQUVTLUdDTSIsIml2IjoiYWJjZGVmIiwiY2lwaGVydGV4dCI6Inh5eiJ9fQ=="
    },
    "api_keys": {
      "payment_provider": "encrypted:eyJlbmNyeXB0ZWQiOnsiYWxnb3JpdGhtIjoiQUVTLUdDTSIsIml2IjoiZmVkaWJhIiwiY2lwaGVydGV4dCI6InF3ZXIifX0="
    }
  }
  """
  
  let encrypted_fields_result = config_manager.load_from_string(config_with_encrypted_fields, "json")
  assert_true(encrypted_fields_result.is_success)
  
  // Decrypt fields on access
  let decrypted_connection_string = config_manager.get_decrypted(encrypted_fields_result.config, "database.connection_string", encryption_key)
  assert_eq(decrypted_connection_string, Some("postgresql://user:password123@localhost:5432/azimuth"))
  
  let decrypted_api_key = config_manager.get_decrypted(encrypted_fields_result.config, "api_keys.payment_provider", encryption_key)
  assert_eq(decrypted_api_key, Some("sk_live_1234567890abcdef"))
  
  // Test configuration access control
  let access_manager = config_manager.get_access_manager()
  
  // Define access policies
  access_manager.add_policy("database.connection_string", ["admin", "db_admin"])
  access_manager.add_policy("api_keys.payment_provider", ["admin", "payment_admin"])
  access_manager.add_policy("service.name", ["*"]) // Public access
  
  // Test access control
  let admin_access = access_manager.check_access("admin", "database.connection_string")
  assert_true(admin_access)
  
  let user_access = access_manager.check_access("user", "database.connection_string")
  assert_false(user_access)
  
  let public_access = access_manager.check_access("any_user", "service.name")
  assert_true(public_access)
  
  // Test configuration audit log
  let audit_logger = config_manager.get_audit_logger()
  
  // Log configuration access
  audit_logger.log_access("admin", "database.connection_string", "read", true)
  audit_logger.log_access("user", "database.connection_string", "read", false)
  audit_logger.log_access("admin", "api_keys.payment_provider", "write", true)
  
  // Get audit log
  let audit_log = audit_logger.get_audit_log()
  assert_true(audit_log.length() >= 3)
  
  let admin_read = audit_log.find(fn(e) { e.user == "admin" && e.resource == "database.connection_string" && e.action == "read" })
  match admin_read {
    Some(entry) => {
      assert_true(entry.success)
      assert_true(entry.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  let user_denied = audit_log.find(fn(e) { e.user == "user" && e.resource == "database.connection_string" && e.action == "read" })
  match user_denied {
    Some(entry) => {
      assert_false(entry.success)
      assert_true(entry.timestamp > 0)
    }
    None => assert_true(false)
  }
}

// Test 7: Configuration Versioning and Rollback
test "configuration versioning and rollback" {
  let config_manager = ConfigurationManager::new()
  
  // Enable configuration versioning
  let versioning_config = VersioningConfig::new()
    .with_max_versions(10)
    .with_auto_versioning(true)
    .with_version_metadata(true)
  
  config_manager.enable_versioning(versioning_config)
  
  // Create initial configuration
  let v1_config = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "version": "1.0.0",
      "port": 8080
    },
    "feature_flags": {
      "enable_metrics": true,
      "enable_tracing": false
    }
  }
  """
  
  let v1_result = config_manager.load_from_string(v1_config, "json")
  assert_true(v1_result.is_success)
  assert_eq(v1_result.version, 1)
  
  // Update configuration
  let v2_config = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "version": "1.1.0",
      "port": 9090
    },
    "feature_flags": {
      "enable_metrics": true,
      "enable_tracing": true
    }
  }
  """
  
  let v2_result = config_manager.load_from_string(v2_config, "json")
  assert_true(v2_result.is_success)
  assert_eq(v2_result.version, 2)
  
  // Update configuration again
  let v3_config = """
  {
    "service": {
      "name": "azimuth-telemetry",
      "version": "1.2.0",
      "port": 9090
    },
    "feature_flags": {
      "enable_metrics": false,
      "enable_tracing": true
    }
  }
  """
  
  let v3_result = config_manager.load_from_string(v3_config, "json")
  assert_true(v3_result.is_success)
  assert_eq(v3_result.version, 3)
  
  // Test version history
  let version_history = config_manager.get_version_history()
  assert_eq(version_history.length(), 3)
  
  let v1_entry = version_history.find(fn(v) { v.version == 1 })
  match v1_entry {
    Some(entry) => {
      assert_eq(entry.config.get("service.version"), Some("1.0.0"))
      assert_eq(entry.config.get("service.port"), Some("8080"))
      assert_true(entry.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  let v2_entry = version_history.find(fn(v) { v.version == 2 })
  match v2_entry {
    Some(entry) => {
      assert_eq(entry.config.get("service.version"), Some("1.1.0"))
      assert_eq(entry.config.get("service.port"), Some("9090"))
      assert_true(entry.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  // Test rollback to previous version
  let rollback_result = config_manager.rollback_to_version(1)
  assert_true(rollback_result.is_success)
  
  let current_config = config_manager.get_current_config()
  assert_eq(current_config.get("service.version"), Some("1.0.0"))
  assert_eq(current_config.get("service.port"), Some("8080"))
  assert_eq(current_config.get("feature_flags.enable_tracing"), Some("false"))
  
  // Test version comparison
  let diff = config_manager.compare_versions(1, 2)
  assert_true(diff.changes.length() > 0)
  
  let port_change = diff.changes.find(fn(c) { c.path == "service.port" })
  match port_change {
    Some(change) => {
      assert_eq(change.old_value, "8080")
      assert_eq(change.new_value, "9090")
      assert_eq(change.change_type, Modified)
    }
    None => assert_true(false)
  }
  
  let tracing_change = diff.changes.find(fn(c) { c.path == "feature_flags.enable_tracing" })
  match tracing_change {
    Some(change) => {
      assert_eq(change.old_value, "false")
      assert_eq(change.new_value, "true")
      assert_eq(change.change_type, Modified)
    }
    None => assert_true(false)
  }
  
  // Test version tagging
  let tag_result = config_manager.tag_version(2, "stable")
  assert_true(tag_result.is_success)
  
  let tagged_version = config_manager.get_tagged_version("stable")
  match tagged_version {
    Some(version) => assert_eq(version, 2)
    None => assert_true(false)
  }
  
  // Test version cleanup
  for i in 0..=8 {
    let temp_config = """
    {
      "service": {
        "name": "azimuth-telemetry",
        "version": "1.""" + i.to_string() + """.0"
      }
    }
    """
    config_manager.load_from_string(temp_config, "json")
  }
  
  let cleanup_result = config_manager.cleanup_old_versions()
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.deleted_versions > 0)
  
  let updated_history = config_manager.get_version_history()
  assert_true(updated_history.length() <= 10) // Should be at most max_versions
}

// Test 8: Configuration Templates and Generation
test "configuration templates and generation" {
  let config_manager = ConfigurationManager::new()
  
  // Define configuration template
  let template = """
  {
    "service": {
      "name": "{{service_name}}",
      "version": "{{service_version}}",
      "port": {{service_port}},
      "host": "{{service_host}}"
    },
    "database": {
      "connection_string": "postgresql://{{db_host}}:{{db_port}}/{{db_name}}",
      "max_connections": {{db_max_connections}},
      "timeout": {{db_timeout}}
    },
    "logging": {
      "level": "{{log_level}}",
      "format": "{{log_format}}",
      "output": "{{log_output}}"
    }
  }
  """
  
  // Define template variables
  let variables = TemplateVariables::new()
    .add("service_name", "azimuth-telemetry")
    .add("service_version", "1.0.0")
    .add("service_port", 8080)
    .add("service_host", "localhost")
    .add("db_host", "localhost")
    .add("db_port", 5432)
    .add("db_name", "azimuth")
    .add("db_max_connections", 10)
    .add("db_timeout", 30000)
    .add("log_level", "info")
    .add("log_format", "json")
    .add("log_output", "console")
  
  // Generate configuration from template
  let generated_config = config_manager.generate_from_template(template, variables)
  
  // Verify generated configuration
  assert_eq(generated_config.get("service.name"), Some("azimuth-telemetry"))
  assert_eq(generated_config.get("service.version"), Some("1.0.0"))
  assert_eq(generated_config.get("service.port"), Some("8080"))
  assert_eq(generated_config.get("database.connection_string"), Some("postgresql://localhost:5432/azimuth"))
  assert_eq(generated_config.get("database.max_connections"), Some("10"))
  assert_eq(generated_config.get("logging.level"), Some("info"))
  
  // Test template with conditional sections
  let conditional_template = """
  {
    "service": {
      "name": "{{service_name}}",
      "port": {{service_port}}
      {{#if enable_ssl}}
      ,"ssl": {
        "enabled": true,
        "cert_path": "{{ssl_cert_path}}",
        "key_path": "{{ssl_key_path}}"
      }
      {{/if}}
    },
    "features": {
      {{#each features}}
      "{{name}}": {{enabled}}
      {{#unless @last}},{{/unless}}
      {{/each}}
    }
  }
  """
  
  let conditional_variables = TemplateVariables::new()
    .add("service_name", "azimuth-telemetry")
    .add("service_port", 8443)
    .add("enable_ssl", true)
    .add("ssl_cert_path", "/etc/ssl/certs/server.crt")
    .add("ssl_key_path", "/etc/ssl/private/server.key")
    .add("features", [
      {"name": "metrics", "enabled": true},
      {"name": "tracing", "enabled": false},
      {"name": "logging", "enabled": true}
    ])
  
  let conditional_config = config_manager.generate_from_template(conditional_template, conditional_variables)
  
  // Verify conditional sections
  assert_eq(conditional_config.get("service.name"), Some("azimuth-telemetry"))
  assert_eq(conditional_config.get("service.port"), Some("8443"))
  assert_eq(conditional_config.get("service.ssl.enabled"), Some("true"))
  assert_eq(conditional_config.get("service.ssl.cert_path"), Some("/etc/ssl/certs/server.crt"))
  assert_eq(conditional_config.get("features.metrics"), Some("true"))
  assert_eq(conditional_config.get("features.tracing"), Some("false"))
  assert_eq(conditional_config.get("features.logging"), Some("true"))
  
  // Test template validation
  let template_validator = TemplateValidator::new()
    .add_required_variable("service_name", StringType)
    .add_required_variable("service_port", IntegerType)
    .add_optional_variable("service_host", StringType, "localhost")
    .add_range_constraint("service_port", 1, 65535)
  
  let incomplete_variables = TemplateVariables::new()
    .add("service_name", "azimuth-telemetry")
    // Missing service_port
  
  let validation_result = template_validator.validate(template, incomplete_variables)
  assert_false(validation_result.is_valid)
  assert_true(validation_result.errors.any(fn(e) { e.contains("service_port") && e.contains("required") }))
  
  // Test template inheritance
  let base_template = """
  {
    "service": {
      "name": "{{service_name}}",
      "version": "{{service_version}}"
    },
    "database": {
      "host": "{{db_host}}",
      "port": {{db_port}}
    }
  }
  """
  
  let child_template = """
  {{>base}}
  ,"logging": {
    "level": "{{log_level}}"
  }
  """
  
  let inheritance_variables = TemplateVariables::new()
    .add("service_name", "azimuth-telemetry")
    .add("service_version", "1.0.0")
    .add("db_host", "localhost")
    .add("db_port", 5432)
    .add("log_level", "info")
  
  let inherited_config = config_manager.generate_from_template_with_inheritance(child_template, inheritance_variables)
  
  // Verify inherited configuration
  assert_eq(inherited_config.get("service.name"), Some("azimuth-telemetry"))
  assert_eq(inherited_config.get("service.version"), Some("1.0.0"))
  assert_eq(inherited_config.get("database.host"), Some("localhost"))
  assert_eq(inherited_config.get("database.port"), Some("5432"))
  assert_eq(inherited_config.get("logging.level"), Some("info"))
}