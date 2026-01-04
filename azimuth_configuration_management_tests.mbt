// Azimuth High-Quality Configuration Management Tests
// This file contains comprehensive test cases for configuration management

// Test 1: Configuration Loading and Validation
test "configuration loading and validation" {
  let config_manager = ConfigurationManager::new()
  
  // Test configuration schema definition
  let schema = ConfigurationSchema::new("azimuth_config")
    .with_field("server", FieldType::Object, FieldConstraint::Required)
    .with_field("database", FieldType::Object, FieldConstraint::Required)
    .with_field("logging", FieldType::Object, FieldConstraint::Optional)
    .with_field("features", FieldType::Object, FieldConstraint::Optional)
  
  // Define nested schema for server configuration
  let server_schema = ConfigurationSchema::new("server")
    .with_field("host", FieldType::String, FieldConstraint::Required)
    .with_field("port", FieldType::Integer, FieldConstraint::Required)
    .with_field("ssl_enabled", FieldType::Boolean, FieldConstraint::Optional)
    .with_field("max_connections", FieldType::Integer, FieldConstraint::Optional)
  
  // Define nested schema for database configuration
  let database_schema = ConfigurationSchema::new("database")
    .with_field("host", FieldType::String, FieldConstraint::Required)
    .with_field("port", FieldType::Integer, FieldConstraint::Required)
    .with_field("name", FieldType::String, FieldConstraint::Required)
    .with_field("username", FieldType::String, FieldConstraint::Required)
    .with_field("password", FieldType::String, FieldConstraint::Required)
    .with_field("ssl_mode", FieldType::String, FieldConstraint::Optional)
    .with_field("pool_size", FieldType::Integer, FieldConstraint::Optional)
  
  schema.add_nested_schema("server", server_schema)
  schema.add_nested_schema("database", database_schema)
  
  config_manager.register_schema(schema)
  
  // Test valid configuration loading
  let valid_config_content = {
    "server": {
      "host": "localhost",
      "port": 8080,
      "ssl_enabled": true,
      "max_connections": 100
    },
    "database": {
      "host": "db.example.com",
      "port": 5432,
      "name": "azimuth",
      "username": "azimuth_user",
      "password": "secure_password",
      "ssl_mode": "require",
      "pool_size": 10
    },
    "logging": {
      "level": "info",
      "format": "json"
    },
    "features": {
      "telemetry": true,
      "analytics": false
    }
  }
  
  let valid_config_result = config_manager.load_from_string("azimuth_config", valid_config_content.to_json())
  assert_true(valid_config_result.is_success())
  
  let valid_config = valid_config_result.unwrap()
  assert_eq(valid_config.get_string("server.host"), "localhost")
  assert_eq(valid_config.get_int("server.port"), 8080)
  assert_true(valid_config.get_bool("server.ssl_enabled"))
  assert_eq(valid_config.get_int("server.max_connections"), 100)
  
  assert_eq(valid_config.get_string("database.host"), "db.example.com")
  assert_eq(valid_config.get_int("database.port"), 5432)
  assert_eq(valid_config.get_string("database.name"), "azimuth")
  assert_eq(valid_config.get_string("database.username"), "azimuth_user")
  assert_eq(valid_config.get_string("database.password"), "secure_password")
  assert_eq(valid_config.get_string("database.ssl_mode"), "require")
  assert_eq(valid_config.get_int("database.pool_size"), 10)
  
  assert_eq(valid_config.get_string("logging.level"), "info")
  assert_eq(valid_config.get_string("logging.format"), "json")
  
  assert_true(valid_config.get_bool("features.telemetry"))
  assert_false(valid_config.get_bool("features.analytics"))
  
  // Test invalid configuration loading
  let invalid_config_content = {
    "server": {
      "host": "localhost",
      // Missing required field: port
      "ssl_enabled": true,
      "max_connections": 100
    },
    "database": {
      "host": "db.example.com",
      "port": 5432,
      "name": "azimuth",
      "username": "azimuth_user",
      // Missing required field: password
      "ssl_mode": "require",
      "pool_size": 10
    }
    // Missing required top-level fields
  }
  
  let invalid_config_result = config_manager.load_from_string("azimuth_config", invalid_config_content.to_json())
  assert_true(invalid_config_result.is_error())
  
  let validation_errors = invalid_config_result.unwrap_error()
  assert_true(validation_errors.length() >= 3) // At least 3 missing required fields
  
  let missing_port = validation_errors.any(|error| error.contains("server.port"))
  let missing_password = validation_errors.any(|error| error.contains("database.password"))
  let missing_fields = validation_errors.any(|error| error.contains("required"))
  
  assert_true(missing_port)
  assert_true(missing_password)
  assert_true(missing_fields)
  
  // Test configuration with type mismatches
  let type_mismatch_config_content = {
    "server": {
      "host": "localhost",
      "port": "8080", // String instead of integer
      "ssl_enabled": "true", // String instead of boolean
      "max_connections": 100
    },
    "database": {
      "host": "db.example.com",
      "port": 5432,
      "name": "azimuth",
      "username": "azimuth_user",
      "password": "secure_password",
      "ssl_mode": "require",
      "pool_size": 10
    }
  }
  
  let type_mismatch_result = config_manager.load_from_string("azimuth_config", type_mismatch_config_content.to_json())
  assert_true(type_mismatch_result.is_error())
  
  let type_errors = type_mismatch_result.unwrap_error()
  assert_true(type_errors.length() >= 2) // At least 2 type mismatches
  
  let port_type_error = type_errors.any(|error| error.contains("server.port") && error.contains("integer"))
  let ssl_type_error = type_errors.any(|error| error.contains("server.ssl_enabled") && error.contains("boolean"))
  
  assert_true(port_type_error)
  assert_true(ssl_type_error)
}

// Test 2: Configuration Sources and Hierarchies
test "configuration sources and hierarchies" {
  let config_manager = ConfigurationManager::new()
  
  // Register configuration schema
  let schema = ConfigurationSchema::new("app_config")
    .with_field("app_name", FieldType::String, FieldConstraint::Required)
    .with_field("version", FieldType::String, FieldConstraint::Required)
    .with_field("debug", FieldType::Boolean, FieldConstraint::Optional)
    .with_field("timeout", FieldType::Integer, FieldConstraint::Optional)
  
  config_manager.register_schema(schema)
  
  // Test configuration from different sources
  let default_config = {
    "app_name": "azimuth",
    "version": "1.0.0",
    "debug": false,
    "timeout": 30
  }
  
  let file_config = {
    "version": "1.1.0",
    "debug": true
  }
  
  let env_config = {
    "timeout": 60
  }
  
  let command_line_config = {
    "debug": false
  }
  
  // Add configuration sources in order of precedence (lowest to highest)
  config_manager.add_source("default", MemoryConfigurationSource::new(default_config))
  config_manager.add_source("file", MemoryConfigurationSource::new(file_config))
  config_manager.add_source("environment", MemoryConfigurationSource::new(env_config))
  config_manager.add_source("command_line", MemoryConfigurationSource::new(command_line_config))
  
  // Load configuration with hierarchy
  let config_result = config_manager.load_with_hierarchy("app_config")
  assert_true(config_result.is_success())
  
  let config = config_result.unwrap()
  
  // Verify configuration values (higher precedence sources override lower ones)
  assert_eq(config.get_string("app_name"), "azimuth") // Only in default
  assert_eq(config.get_string("version"), "1.1.0") // From file (overrides default)
  assert_false(config.get_bool("debug")) // From command line (overrides file)
  assert_eq(config.get_int("timeout"), 60) // From environment (overrides default)
  
  // Test configuration source priorities
  let source_priorities = config_manager.get_source_priorities()
  assert_eq(source_priorities[0], "default")
  assert_eq(source_priorities[1], "file")
  assert_eq(source_priorities[2], "environment")
  assert_eq(source_priorities[3], "command_line")
  
  // Test configuration source metadata
  let source_metadata = config_manager.get_source_metadata()
  
  assert_true(source_metadata.contains_key("default"))
  assert_true(source_metadata.contains_key("file"))
  assert_true(source_metadata.contains_key("environment"))
  assert_true(source_metadata.contains_key("command_line"))
  
  for (source_name, metadata) in source_metadata {
    assert_eq(metadata.name, source_name)
    assert_true(metadata.loaded_at > 0)
    assert_true(metadata.entries_count > 0)
  }
  
  // Test configuration value source tracking
  let value_sources = config_manager.get_value_sources()
  
  assert_eq(value_sources.get("app_name"), "default")
  assert_eq(value_sources.get("version"), "file")
  assert_eq(value_sources.get("debug"), "command_line")
  assert_eq(value_sources.get("timeout"), "environment")
  
  // Test configuration source reloading
  let updated_file_config = {
    "version": "1.2.0",
    "debug": false
  }
  
  let reload_result = config_manager.reload_source("file", MemoryConfigurationSource::new(updated_file_config))
  assert_true(reload_result.is_success())
  
  let reloaded_config = config_manager.get_config("app_config")
  assert_eq(reloaded_config.get_string("version"), "1.2.0") // Updated from file
  assert_false(reloaded_config.get_bool("debug")) // Still from command line (higher precedence)
}

// Test 3: Dynamic Configuration Updates
test "dynamic configuration updates" {
  let config_manager = ConfigurationManager::new()
  
  // Register configuration schema
  let schema = ConfigurationSchema::new("dynamic_config")
    .with_field("feature_flags", FieldType::Object, FieldConstraint::Required)
    .with_field("rate_limits", FieldType::Object, FieldConstraint::Required)
    .with_field("endpoints", FieldType::Array, FieldConstraint::Required)
  
  config_manager.register_schema(schema)
  
  // Load initial configuration
  let initial_config = {
    "feature_flags": {
      "new_ui": false,
      "advanced_search": false,
      "beta_features": false
    },
    "rate_limits": {
      "api_calls": 100,
      "search_requests": 50,
      "uploads": 10
    },
    "endpoints": [
      {"name": "api", "url": "https://api.example.com"},
      {"name": "auth", "url": "https://auth.example.com"}
    ]
  }
  
  let config_result = config_manager.load_from_string("dynamic_config", initial_config.to_json())
  assert_true(config_result.is_success())
  
  let config = config_result.unwrap()
  
  // Test configuration change notifications
  let notification_count = AtomicInt::new(0)
  let last_changed_key = AtomicString::new("")
  
  config_manager.add_change_observer("dynamic_config", || key, old_value, new_value {
    notification_count.increment()
    last_changed_key.set(key)
  })
  
  // Test dynamic configuration updates
  let update_result = config_manager.update_value("dynamic_config", "feature_flags.new_ui", true)
  assert_true(update_result.is_success())
  
  // Verify notification was sent
  assert_eq(notification_count.get(), 1)
  assert_eq(last_changed_key.get(), "feature_flags.new_ui")
  
  // Verify configuration was updated
  assert_true(config.get_bool("feature_flags.new_ui"))
  
  // Test batch updates
  let batch_updates = [
    {"key": "feature_flags.advanced_search", "value": true},
    {"key": "feature_flags.beta_features", "value": true},
    {"key": "rate_limits.api_calls", "value": 200}
  ]
  
  let batch_update_result = config_manager.batch_update("dynamic_config", batch_updates)
  assert_true(batch_update_result.is_success())
  
  // Verify batch updates
  assert_true(config.get_bool("feature_flags.advanced_search"))
  assert_true(config.get_bool("feature_flags.beta_features"))
  assert_eq(config.get_int("rate_limits.api_calls"), 200)
  
  // Test configuration rollback
  let rollback_result = config_manager.rollback("dynamic_config")
  assert_true(rollback_result.is_success())
  
  // Verify rollback restored previous state
  assert_true(config.get_bool("feature_flags.new_ui"))
  assert_false(config.get_bool("feature_flags.advanced_search"))
  assert_false(config.get_bool("feature_flags.beta_features"))
  assert_eq(config.get_int("rate_limits.api_calls"), 200)
  
  // Test configuration snapshots
  let snapshot_result = config_manager.create_snapshot("dynamic_config", "before_major_changes")
  assert_true(snapshot_result.is_success())
  
  // Make more changes
  config_manager.update_value("dynamic_config", "rate_limits.search_requests", 100)
  config_manager.update_value("dynamic_config", "rate_limits.uploads", 20)
  
  // Verify changes
  assert_eq(config.get_int("rate_limits.search_requests"), 100)
  assert_eq(config.get_int("rate_limits.uploads"), 20)
  
  // Restore from snapshot
  let restore_result = config_manager.restore_snapshot("dynamic_config", "before_major_changes")
  assert_true(restore_result.is_success())
  
  // Verify restoration
  assert_eq(config.get_int("rate_limits.search_requests"), 50) // Restored to original value
  assert_eq(config.get_int("rate_limits.uploads"), 10) // Restored to original value
  
  // Test configuration history
  let history = config_manager.get_change_history("dynamic_config")
  assert_true(history.length() > 0)
  
  for change in history {
    assert_true(change.key.length() > 0)
    assert_true(change.timestamp > 0)
    assert_true(change.old_value != change.new_value)
  }
  
  // Test configuration validation on updates
  let invalid_update_result = config_manager.update_value("dynamic_config", "rate_limits.api_calls", -10)
  assert_true(invalid_update_result.is_error())
  
  let validation_error = invalid_update_result.unwrap_error()
  assert_true(validation_error.contains("validation") || validation_error.contains("invalid"))
}

// Test 4: Configuration Encryption and Security
test "configuration encryption and security" {
  let config_manager = ConfigurationManager::new()
  
  // Initialize encryption
  let encryption_key = generate_encryption_key()
  config_manager.initialize_encryption(encryption_key)
  
  // Register configuration schema with sensitive fields
  let schema = ConfigurationSchema::new("secure_config")
    .with_field("api_key", FieldType::String, FieldConstraint::Required, true) // Sensitive
    .with_field("database_password", FieldType::String, FieldConstraint::Required, true) // Sensitive
    .with_field("jwt_secret", FieldType::String, FieldConstraint::Required, true) // Sensitive
    .with_field("app_name", FieldType::String, FieldConstraint::Required, false) // Not sensitive
    .with_field("debug", FieldType::Boolean, FieldConstraint::Optional, false) // Not sensitive
  
  config_manager.register_schema(schema)
  
  // Load configuration with sensitive values
  let secure_config = {
    "api_key": "sk-1234567890abcdef",
    "database_password": "super_secure_password",
    "jwt_secret": "jwt_secret_key_12345",
    "app_name": "azimuth",
    "debug": false
  }
  
  let config_result = config_manager.load_from_string("secure_config", secure_config.to_json())
  assert_true(config_result.is_success())
  
  let config = config_result.unwrap()
  
  // Test sensitive value access
  let api_key = config.get_string("api_key")
  assert_eq(api_key, "sk-1234567890abcdef")
  
  let db_password = config.get_string("database_password")
  assert_eq(db_password, "super_secure_password")
  
  let jwt_secret = config.get_string("jwt_secret")
  assert_eq(jwt_secret, "jwt_secret_key_12345")
  
  // Verify sensitive values are encrypted in storage
  let storage_data = config_manager.get_raw_storage_data("secure_config")
  
  // Sensitive values should be encrypted (not plain text)
  assert_false(storage_data.contains("sk-1234567890abcdef"))
  assert_false(storage_data.contains("super_secure_password"))
  assert_false(storage_data.contains("jwt_secret_key_12345"))
  
  // Non-sensitive values should be plain text
  assert_true(storage_data.contains("azimuth"))
  assert_true(storage_data.contains("debug"))
  
  // Test configuration export with sensitive data handling
  let export_options = ConfigurationExportOptions::new()
    .with_include_sensitive(false)
    .with_format("json")
  
  let export_result = config_manager.export("secure_config", export_options)
  assert_true(export_result.is_success())
  
  let exported_config = export_result.unwrap()
  
  // Sensitive values should be masked in export
  let exported_data = exported_config.content
  assert_false(exported_data.contains("sk-1234567890abcdef"))
  assert_false(exported_data.contains("super_secure_password"))
  assert_false(exported_data.contains("jwt_secret_key_12345"))
  
  // Non-sensitive values should be present
  assert_true(exported_data.contains("azimuth"))
  assert_true(exported_data.contains("debug"))
  
  // Test configuration export with sensitive data included
  let export_with_sensitive_options = ConfigurationExportOptions::new()
    .with_include_sensitive(true)
    .with_format("json")
  
  let export_with_sensitive_result = config_manager.export("secure_config", export_with_sensitive_options)
  assert_true(export_with_sensitive_result.is_success())
  
  let exported_with_sensitive_config = export_with_sensitive_result.unwrap()
  
  // All values should be present when including sensitive data
  let exported_with_sensitive_data = exported_with_sensitive_config.content
  assert_true(exported_with_sensitive_data.contains("sk-1234567890abcdef"))
  assert_true(exported_with_sensitive_data.contains("super_secure_password"))
  assert_true(exported_with_sensitive_data.contains("jwt_secret_key_12345"))
  
  // Test encryption key rotation
  let new_encryption_key = generate_encryption_key()
  let rotation_result = config_manager.rotate_encryption_key(new_encryption_key)
  assert_true(rotation_result.is_success())
  
  // Verify values are still accessible after key rotation
  let rotated_api_key = config.get_string("api_key")
  assert_eq(rotated_api_key, "sk-1234567890abcdef")
  
  // Test configuration access control
  let access_controller = ConfigurationAccessController::new()
  
  // Define access policies
  access_controller.add_policy("admin", AccessPolicy::new().with_allow_all())
  access_controller.add_policy("developer", AccessPolicy::new()
    .with_allow_read(["app_name", "debug"])
    .with_deny_read(["api_key", "database_password", "jwt_secret"])
    .with_deny_write_all()
  )
  access_controller.add_policy("readonly", AccessPolicy::new()
    .with_allow_read(["app_name"])
    .with_deny_read(["api_key", "database_password", "jwt_secret", "debug"])
    .with_deny_write_all()
  )
  
  config_manager.set_access_controller(access_controller)
  
  // Test access control with different roles
  let admin_context = AccessContext::new("admin")
  let developer_context = AccessContext::new("developer")
  let readonly_context = AccessContext::new("readonly")
  
  // Admin can access all values
  assert_true(config_manager.can_read("secure_config", "api_key", admin_context))
  assert_true(config_manager.can_write("secure_config", "api_key", admin_context))
  
  // Developer can read non-sensitive values only
  assert_true(config_manager.can_read("secure_config", "app_name", developer_context))
  assert_false(config_manager.can_read("secure_config", "api_key", developer_context))
  assert_false(config_manager.can_write("secure_config", "app_name", developer_context))
  
  // Readonly can read only specific non-sensitive values
  assert_true(config_manager.can_read("secure_config", "app_name", readonly_context))
  assert_false(config_manager.can_read("secure_config", "debug", readonly_context))
  assert_false(config_manager.can_read("secure_config", "api_key", readonly_context))
  assert_false(config_manager.can_write("secure_config", "app_name", readonly_context))
}

// Test 5: Configuration Template and Inheritance
test "configuration template and inheritance" {
  let config_manager = ConfigurationManager::new()
  
  // Define configuration template
  let base_template = ConfigurationTemplate::new("base_service")
    .with_field("service_name", FieldType::String, FieldConstraint::Required)
    .with_field("version", FieldType::String, FieldConstraint::Required)
    .with_field("port", FieldType::Integer, FieldConstraint::Required)
    .with_field("host", FieldType::String, FieldConstraint::Optional, "0.0.0.0")
    .with_field("workers", FieldType::Integer, FieldConstraint::Optional, 4)
    .with_field("timeout", FieldType::Integer, FieldConstraint::Optional, 30)
    .with_field("debug", FieldType::Boolean, FieldConstraint::Optional, false)
  
  // Define specialized template that extends base template
  let web_service_template = ConfigurationTemplate::new("web_service")
    .with_extends("base_service")
    .with_field("static_files_path", FieldType::String, FieldConstraint::Required)
    .with_field("session_timeout", FieldType::Integer, FieldConstraint::Optional, 3600)
    .with_field("max_upload_size", FieldType::Integer, FieldConstraint::Optional, 10485760)
  
  // Define another specialized template
  let api_service_template = ConfigurationTemplate::new("api_service")
    .with_extends("base_service")
    .with_field("rate_limit", FieldType::Integer, FieldConstraint::Optional, 100)
    .with_field("auth_required", FieldType::Boolean, FieldConstraint::Optional, true)
    .with_field("cors_enabled", FieldType::Boolean, FieldConstraint::Optional, false)
  
  config_manager.register_template(base_template)
  config_manager.register_template(web_service_template)
  config_manager.register_template(api_service_template)
  
  // Test template instantiation
  let web_service_config = {
    "service_name": "web_frontend",
    "version": "1.0.0",
    "port": 8080,
    "static_files_path": "/var/www/static"
    // Other fields will use default values from template
  }
  
  let web_config_result = config_manager.instantiate_from_template("web_service", web_service_config)
  assert_true(web_config_result.is_success())
  
  let web_config = web_config_result.unwrap()
  
  // Verify all fields are present (including inherited ones and defaults)
  assert_eq(web_config.get_string("service_name"), "web_frontend")
  assert_eq(web_config.get_string("version"), "1.0.0")
  assert_eq(web_config.get_int("port"), 8080)
  assert_eq(web_config.get_string("host"), "0.0.0.0") // Default value
  assert_eq(web_config.get_int("workers"), 4) // Default value
  assert_eq(web_config.get_int("timeout"), 30) // Default value
  assert_false(web_config.get_bool("debug")) // Default value
  assert_eq(web_config.get_string("static_files_path"), "/var/www/static")
  assert_eq(web_config.get_int("session_timeout"), 3600) // Default value
  assert_eq(web_config.get_int("max_upload_size"), 10485760) // Default value
  
  // Test API service configuration
  let api_service_config = {
    "service_name": "api_backend",
    "version": "2.1.0",
    "port": 9090,
    "rate_limit": 200,
    "auth_required": true,
    "cors_enabled": true
  }
  
  let api_config_result = config_manager.instantiate_from_template("api_service", api_service_config)
  assert_true(api_config_result.is_success())
  
  let api_config = api_config_result.unwrap()
  
  // Verify all fields are present
  assert_eq(api_config.get_string("service_name"), "api_backend")
  assert_eq(api_config.get_string("version"), "2.1.0")
  assert_eq(api_config.get_int("port"), 9090)
  assert_eq(api_config.get_string("host"), "0.0.0.0") // Default value
  assert_eq(api_config.get_int("workers"), 4) // Default value
  assert_eq(api_config.get_int("timeout"), 30) // Default value
  assert_false(api_config.get_bool("debug")) // Default value
  assert_eq(api_config.get_int("rate_limit"), 200) // Provided value
  assert_true(api_config.get_bool("auth_required")) // Provided value
  assert_true(api_config.get_bool("cors_enabled")) // Provided value
  
  // Test template inheritance chain
  let inheritance_chain = config_manager.get_inheritance_chain("web_service")
  assert_eq(inheritance_chain, ["web_service", "base_service"])
  
  let api_inheritance_chain = config_manager.get_inheritance_chain("api_service")
  assert_eq(api_inheritance_chain, ["api_service", "base_service"])
  
  // Test template validation
  let invalid_web_config = {
    "service_name": "web_frontend",
    // Missing required field: version
    "port": 8080
    // Missing required field: static_files_path
  }
  
  let invalid_result = config_manager.instantiate_from_template("web_service", invalid_web_config)
  assert_true(invalid_result.is_error())
  
  let validation_errors = invalid_result.unwrap_error()
  assert_true(validation_errors.length() >= 2) // At least 2 missing required fields
  
  // Test template override
  let template_override = {
    "host": "127.0.0.1", // Override default
    "workers": 8, // Override default
    "debug": true // Override default
  }
  
  let override_result = config_manager.apply_template_override("web_service", web_config, template_override)
  assert_true(override_result.is_success())
  
  let overridden_config = override_result.unwrap()
  
  // Verify overrides were applied
  assert_eq(overridden_config.get_string("host"), "127.0.0.1")
  assert_eq(overridden_config.get_int("workers"), 8)
  assert_true(overridden_config.get_bool("debug"))
  
  // Test template composition
  let monitoring_template = ConfigurationTemplate::new("monitoring")
    .with_field("metrics_enabled", FieldType::Boolean, FieldConstraint::Optional, true)
    .with_field("metrics_port", FieldType::Integer, FieldConstraint::Optional, 9090)
    .with_field("health_check_enabled", FieldType::Boolean, FieldConstraint::Optional, true)
    .with_field("log_level", FieldType::String, FieldConstraint::Optional, "info")
  
  config_manager.register_template(monitoring_template)
  
  // Compose web service with monitoring
  let composed_templates = ["web_service", "monitoring"]
  let composed_config = {
    "service_name": "web_frontend_with_monitoring",
    "version": "1.0.0",
    "port": 8080,
    "static_files_path": "/var/www/static",
    "metrics_port": 9091 // Override monitoring default
  }
  
  let composition_result = config_manager.compose_templates(composed_templates, composed_config)
  assert_true(composition_result.is_success())
  
  let composed_final_config = composition_result.unwrap()
  
  // Verify composed configuration includes fields from all templates
  assert_eq(composed_final_config.get_string("service_name"), "web_frontend_with_monitoring")
  assert_eq(composed_final_config.get_int("port"), 8080)
  assert_eq(composed_final_config.get_string("static_files_path"), "/var/www/static")
  assert_true(composed_final_config.get_bool("metrics_enabled")) // From monitoring template
  assert_eq(composed_final_config.get_int("metrics_port"), 9091) // Override value
  assert_true(composed_final_config.get_bool("health_check_enabled")) // From monitoring template
  assert_eq(composed_final_config.get_string("log_level"), "info") // From monitoring template
}

// Test 6: Configuration Environment Specificity
test "configuration environment specificity" {
  let config_manager = ConfigurationManager::new()
  
  // Register configuration schema
  let schema = ConfigurationSchema::new("environment_config")
    .with_field("app_name", FieldType::String, FieldConstraint::Required)
    .with_field("environment", FieldType::String, FieldConstraint::Required)
    .with_field("database", FieldType::Object, FieldConstraint::Required)
    .with_field("logging", FieldType::Object, FieldConstraint::Required)
    .with_field("features", FieldType::Object, FieldConstraint::Required)
  
  config_manager.register_schema(schema)
  
  // Define base configuration
  let base_config = {
    "app_name": "azimuth",
    "database": {
      "host": "localhost",
      "port": 5432,
      "name": "azimuth"
    },
    "logging": {
      "level": "info",
      "format": "text"
    },
    "features": {
      "telemetry": true,
      "analytics": false
    }
  }
  
  // Define development environment overrides
  let dev_config = {
    "environment": "development",
    "database": {
      "host": "dev-db.example.com",
      "ssl_mode": "disable"
    },
    "logging": {
      "level": "debug",
      "format": "json"
    },
    "features": {
      "debug_mode": true,
      "hot_reload": true
    }
  }
  
  // Define production environment overrides
  let prod_config = {
    "environment": "production",
    "database": {
      "host": "prod-db.example.com",
      "ssl_mode": "require",
      "pool_size": 20
    },
    "logging": {
      "level": "warn",
      "format": "json"
    },
    "features": {
      "analytics": true,
      "debug_mode": false
    }
  }
  
  // Define testing environment overrides
  let test_config = {
    "environment": "testing",
    "database": {
      "host": "test-db.example.com",
      "name": "azimuth_test"
    },
    "logging": {
      "level": "error"
    },
    "features": {
      "mock_external_services": true
    }
  }
  
  // Add environment-specific configurations
  config_manager.add_environment_config("base", base_config)
  config_manager.add_environment_config("development", dev_config)
  config_manager.add_environment_config("production", prod_config)
  config_manager.add_environment_config("testing", test_config)
  
  // Test loading configuration for specific environment
  let dev_result = config_manager.load_for_environment("environment_config", "development")
  assert_true(dev_result.is_success())
  
  let dev_loaded_config = dev_result.unwrap()
  
  // Verify development configuration
  assert_eq(dev_loaded_config.get_string("app_name"), "azimuth") // From base
  assert_eq(dev_loaded_config.get_string("environment"), "development") // From dev
  assert_eq(dev_loaded_config.get_string("database.host"), "dev-db.example.com") // From dev
  assert_eq(dev_loaded_config.get_int("database.port"), 5432) // From base
  assert_eq(dev_loaded_config.get_string("database.name"), "azimuth") // From base
  assert_eq(dev_loaded_config.get_string("database.ssl_mode"), "disable") // From dev
  assert_eq(dev_loaded_config.get_string("logging.level"), "debug") // From dev
  assert_eq(dev_loaded_config.get_string("logging.format"), "json") // From dev
  assert_true(dev_loaded_config.get_bool("features.telemetry")) // From base
  assert_false(dev_loaded_config.get_bool("features.analytics")) // From base
  assert_true(dev_loaded_config.get_bool("features.debug_mode")) // From dev
  assert_true(dev_loaded_config.get_bool("features.hot_reload")) // From dev
  
  // Test production configuration
  let prod_result = config_manager.load_for_environment("environment_config", "production")
  assert_true(prod_result.is_success())
  
  let prod_loaded_config = prod_result.unwrap()
  
  // Verify production configuration
  assert_eq(prod_loaded_config.get_string("app_name"), "azimuth") // From base
  assert_eq(prod_loaded_config.get_string("environment"), "production") // From prod
  assert_eq(prod_loaded_config.get_string("database.host"), "prod-db.example.com") // From prod
  assert_eq(prod_loaded_config.get_int("database.port"), 5432) // From base
  assert_eq(prod_loaded_config.get_string("database.name"), "azimuth") // From base
  assert_eq(prod_loaded_config.get_string("database.ssl_mode"), "require") // From prod
  assert_eq(prod_loaded_config.get_int("database.pool_size"), 20) // From prod
  assert_eq(prod_loaded_config.get_string("logging.level"), "warn") // From prod
  assert_eq(prod_loaded_config.get_string("logging.format"), "json") // From prod
  assert_true(prod_loaded_config.get_bool("features.telemetry")) // From base
  assert_true(prod_loaded_config.get_bool("features.analytics")) // From prod
  assert_false(prod_loaded_config.get_bool("features.debug_mode")) // From prod
  
  // Test testing configuration
  let test_result = config_manager.load_for_environment("environment_config", "testing")
  assert_true(test_result.is_success())
  
  let test_loaded_config = test_result.unwrap()
  
  // Verify testing configuration
  assert_eq(test_loaded_config.get_string("app_name"), "azimuth") // From base
  assert_eq(test_loaded_config.get_string("environment"), "testing") // From test
  assert_eq(test_loaded_config.get_string("database.host"), "test-db.example.com") // From test
  assert_eq(test_loaded_config.get_int("database.port"), 5432) // From base
  assert_eq(test_loaded_config.get_string("database.name"), "azimuth_test") // From test
  assert_eq(test_loaded_config.get_string("logging.level"), "error") // From test
  assert_eq(test_loaded_config.get_string("logging.format"), "text") // From base
  assert_true(test_loaded_config.get_bool("features.telemetry")) // From base
  assert_false(test_loaded_config.get_bool("features.analytics")) // From base
  assert_true(test_loaded_config.get_bool("features.mock_external_services")) // From test
  
  // Test environment configuration validation
  let invalid_env_result = config_manager.load_for_environment("environment_config", "staging")
  assert_true(invalid_env_result.is_error())
  
  let env_error = invalid_env_result.unwrap_error()
  assert_true(env_error.contains("environment") || env_error.contains("not found"))
  
  // Test environment-specific configuration templates
  let env_template = ConfigurationTemplate::new("service_with_env")
    .with_field("service_name", FieldType::String, FieldConstraint::Required)
    .with_field("port", FieldType::Integer, FieldConstraint::Required)
    .with_field("environment", FieldType::String, FieldConstraint::Required)
    .with_environment_override("development", {
      "port": 3000,
      "debug": true
    })
    .with_environment_override("production", {
      "port": 80,
      "debug": false
    })
  
  config_manager.register_template(env_template)
  
  // Test environment-specific template instantiation
  let service_config = {
    "service_name": "user_service"
  }
  
  let dev_service_result = config_manager.instantiate_template_for_environment("service_with_env", service_config, "development")
  assert_true(dev_service_result.is_success())
  
  let dev_service_config = dev_service_result.unwrap()
  assert_eq(dev_service_config.get_string("service_name"), "user_service")
  assert_eq(dev_service_config.get_int("port"), 3000) // From development override
  assert_eq(dev_service_config.get_string("environment"), "development")
  assert_true(dev_service_config.get_bool("debug")) // From development override
  
  let prod_service_result = config_manager.instantiate_template_for_environment("service_with_env", service_config, "production")
  assert_true(prod_service_result.is_success())
  
  let prod_service_config = prod_service_result.unwrap()
  assert_eq(prod_service_config.get_string("service_name"), "user_service")
  assert_eq(prod_service_config.get_int("port"), 80) // From production override
  assert_eq(prod_service_config.get_string("environment"), "production")
  assert_false(prod_service_config.get_bool("debug")) // From production override
}

// Test 7: Configuration Validation and Constraints
test "configuration validation and constraints" {
  let config_manager = ConfigurationManager::new()
  
  // Register configuration schema with constraints
  let schema = ConfigurationSchema::new("constrained_config")
    .with_field("server_port", FieldType::Integer, FieldConstraint::Required, false)
    .with_constraint("server_port", RangeConstraint::new(1, 65535))
    .with_field("database_url", FieldType::String, FieldConstraint::Required, false)
    .with_constraint("database_url", RegexConstraint::new("^postgres://.+"))
    .with_field("api_rate_limit", FieldType::Integer, FieldConstraint::Optional, false)
    .with_constraint("api_rate_limit", RangeConstraint::new(1, 10000))
    .with_field("log_level", FieldType::String, FieldConstraint::Optional, false)
    .with_constraint("log_level", EnumConstraint::new(["debug", "info", "warn", "error"]))
    .with_field("cache_ttl", FieldType::Integer, FieldConstraint::Optional, false)
    .with_constraint("cache_ttl", RangeConstraint::new(60, 86400))
    .with_field("feature_flags", FieldType::Object, FieldConstraint::Optional, false)
    .with_constraint("feature_flags", CustomConstraint::new(|value| {
      // Ensure all feature flags are boolean
      if value.is_object() {
        for (key, val) in value.as_object() {
          if !val.is_boolean() {
            return ValidationError::new("Feature flag '" + key + "' must be boolean")
          }
        }
        return None
      } else {
        return ValidationError::new("feature_flags must be an object")
      }
    }))
    .with_field("endpoints", FieldType::Array, FieldConstraint::Optional, false)
    .with_constraint("endpoints", ArrayConstraint::new(
      ArrayLengthConstraint::new(1, 10),
      ItemSchemaConstraint::new(ConfigurationSchema::new("endpoint")
        .with_field("name", FieldType::String, FieldConstraint::Required)
        .with_field("url", FieldType::String, FieldConstraint::Required)
        .with_constraint("url", RegexConstraint::new("^https?://.+"))
      )
    ))
  
  config_manager.register_schema(schema)
  
  // Test valid configuration
  let valid_config = {
    "server_port": 8080,
    "database_url": "postgres://user:pass@localhost:5432/azimuth",
    "api_rate_limit": 1000,
    "log_level": "info",
    "cache_ttl": 3600,
    "feature_flags": {
      "new_ui": true,
      "beta_features": false
    },
    "endpoints": [
      {"name": "api", "url": "https://api.example.com"},
      {"name": "auth", "url": "https://auth.example.com"}
    ]
  }
  
  let valid_result = config_manager.load_from_string("constrained_config", valid_config.to_json())
  assert_true(valid_result.is_success())
  
  // Test invalid configuration with constraint violations
  let invalid_config = {
    "server_port": 70000, // Outside valid range
    "database_url": "mysql://user:pass@localhost:3306/azimuth", // Doesn't match regex
    "api_rate_limit": 0, // Below minimum
    "log_level": "trace", // Not in enum
    "cache_ttl": 30, // Below minimum
    "feature_flags": {
      "new_ui": "yes", // Not boolean
      "beta_features": false
    },
    "endpoints": [
      {"name": "api", "url": "ftp://api.example.com"}, // Invalid URL protocol
      {"name": "auth", "url": "not-a-url"} // Invalid URL format
    ]
  }
  
  let invalid_result = config_manager.load_from_string("constrained_config", invalid_config.to_json())
  assert_true(invalid_result.is_error())
  
  let validation_errors = invalid_result.unwrap_error()
  assert_true(validation_errors.length() >= 7) // At least 7 constraint violations
  
  // Verify specific constraint errors
  let port_error = validation_errors.any(|error| error.contains("server_port") && error.contains("range"))
  let db_url_error = validation_errors.any(|error| error.contains("database_url") && error.contains("regex"))
  let rate_limit_error = validation_errors.any(|error| error.contains("api_rate_limit") && error.contains("range"))
  let log_level_error = validation_errors.any(|error| error.contains("log_level") && error.contains("enum"))
  let cache_ttl_error = validation_errors.any(|error| error.contains("cache_ttl") && error.contains("range"))
  let feature_flag_error = validation_errors.any(|error| error.contains("feature_flags") && error.contains("boolean"))
  let endpoint_error = validation_errors.any(|error| error.contains("endpoints") && (error.contains("url") || error.contains("regex")))
  
  assert_true(port_error)
  assert_true(db_url_error)
  assert_true(rate_limit_error)
  assert_true(log_level_error)
  assert_true(cache_ttl_error)
  assert_true(feature_flag_error)
  assert_true(endpoint_error)
  
  // Test conditional constraints
  let conditional_schema = ConfigurationSchema::new("conditional_config")
    .with_field("auth_enabled", FieldType::Boolean, FieldConstraint::Required)
    .with_field("auth_type", FieldType::String, FieldConstraint::Optional)
    .with_constraint("auth_type", ConditionalConstraint::new(
      "auth_enabled", // Condition field
      true, // Condition value
      EnumConstraint::new(["basic", "oauth", "jwt"]) // Constraint to apply when condition is met
    ))
    .with_field("auth_secret", FieldType::String, FieldConstraint::Optional)
    .with_constraint("auth_secret", ConditionalConstraint::new(
      "auth_enabled",
      true,
      RequiredConstraint::new() // auth_secret is required when auth_enabled is true
    ))
  
  config_manager.register_schema(conditional_schema)
  
  // Test valid conditional configuration
  let valid_conditional_config = {
    "auth_enabled": true,
    "auth_type": "oauth",
    "auth_secret": "secret_value"
  }
  
  let valid_conditional_result = config_manager.load_from_string("conditional_config", valid_conditional_config.to_json())
  assert_true(valid_conditional_result.is_success())
  
  // Test invalid conditional configuration
  let invalid_conditional_config = {
    "auth_enabled": true,
    "auth_type": "invalid_type", // Not in enum
    // Missing auth_secret
  }
  
  let invalid_conditional_result = config_manager.load_from_string("conditional_config", invalid_conditional_config.to_json())
  assert_true(invalid_conditional_result.is_error())
  
  let conditional_errors = invalid_conditional_result.unwrap_error()
  assert_true(conditional_errors.length() >= 2) // At least 2 errors
  
  let auth_type_error = conditional_errors.any(|error| error.contains("auth_type") && error.contains("enum"))
  let auth_secret_error = conditional_errors.any(|error| error.contains("auth_secret") && error.contains("required"))
  
  assert_true(auth_type_error)
  assert_true(auth_secret_error)
  
  // Test cross-field constraints
  let cross_field_schema = ConfigurationSchema::new("cross_field_config")
    .with_field("min_connections", FieldType::Integer, FieldConstraint::Required)
    .with_field("max_connections", FieldType::Integer, FieldConstraint::Required)
    .with_constraint("max_connections", CrossFieldConstraint::new(
      ["min_connections"], // Dependent fields
      |values| {
        let min_val = values.get("min_connections").to_int()
        let max_val = values.get("max_connections").to_int()
        if max_val <= min_val {
          return ValidationError::new("max_connections must be greater than min_connections")
        }
        return None
      }
    ))
    .with_field("connection_timeout", FieldType::Integer, FieldConstraint::Required)
    .with_field("request_timeout", FieldType::Integer, FieldConstraint::Required)
    .with_constraint("request_timeout", CrossFieldConstraint::new(
      ["connection_timeout"],
      |values| {
        let connection_timeout = values.get("connection_timeout").to_int()
        let request_timeout = values.get("request_timeout").to_int()
        if request_timeout <= connection_timeout {
          return ValidationError::new("request_timeout must be greater than connection_timeout")
        }
        return None
      }
    ))
  
  config_manager.register_schema(cross_field_schema)
  
  // Test valid cross-field configuration
  let valid_cross_field_config = {
    "min_connections": 10,
    "max_connections": 100,
    "connection_timeout": 5,
    "request_timeout": 30
  }
  
  let valid_cross_field_result = config_manager.load_from_string("cross_field_config", valid_cross_field_config.to_json())
  assert_true(valid_cross_field_result.is_success())
  
  // Test invalid cross-field configuration
  let invalid_cross_field_config = {
    "min_connections": 100,
    "max_connections": 50, // Less than min_connections
    "connection_timeout": 30,
    "request_timeout": 10 // Less than connection_timeout
  }
  
  let invalid_cross_field_result = config_manager.load_from_string("cross_field_config", invalid_cross_field_config.to_json())
  assert_true(invalid_cross_field_result.is_error())
  
  let cross_field_errors = invalid_cross_field_result.unwrap_error()
  assert_true(cross_field_errors.length() >= 2) // At least 2 errors
  
  let connections_error = cross_field_errors.any(|error| error.contains("max_connections") && error.contains("min_connections"))
  let timeout_error = cross_field_errors.any(|error| error.contains("request_timeout") && error.contains("connection_timeout"))
  
  assert_true(connections_error)
  assert_true(timeout_error)
}

// Test 8: Configuration Backup and Restore
test "configuration backup and restore" {
  let config_manager = ConfigurationManager::new()
  
  // Register configuration schema
  let schema = ConfigurationSchema::new("backup_config")
    .with_field("app_name", FieldType::String, FieldConstraint::Required)
    .with_field("version", FieldType::String, FieldConstraint::Required)
    .with_field("database", FieldType::Object, FieldConstraint::Required)
    .with_field("features", FieldType::Object, FieldConstraint::Required)
  
  config_manager.register_schema(schema)
  
  // Load initial configuration
  let initial_config = {
    "app_name": "azimuth",
    "version": "1.0.0",
    "database": {
      "host": "localhost",
      "port": 5432,
      "name": "azimuth"
    },
    "features": {
      "telemetry": true,
      "analytics": false
    }
  }
  
  let config_result = config_manager.load_from_string("backup_config", initial_config.to_json())
  assert_true(config_result.is_success())
  
  // Test configuration backup
  let backup_result = config_manager.create_backup("backup_config", "initial_setup")
  assert_true(backup_result.is_success())
  
  let backup_info = backup_result.unwrap()
  assert_eq(backup_info.config_name, "backup_config")
  assert_eq(backup_info.backup_name, "initial_setup")
  assert_true(backup_info.timestamp > 0)
  assert_true(backup_info.size_bytes > 0)
  assert_true(backup_info.checksum.length() > 0)
  
  // Test listing backups
  let backups = config_manager.list_backups("backup_config")
  assert_eq(backups.length(), 1)
  assert_eq(backups[0].backup_name, "initial_setup")
  
  // Make configuration changes
  config_manager.update_value("backup_config", "version", "1.1.0")
  config_manager.update_value("backup_config", "database.port", 5433)
  config_manager.update_value("backup_config", "features.analytics", true)
  
  // Verify changes
  let config = config_manager.get_config("backup_config")
  assert_eq(config.get_string("version"), "1.1.0")
  assert_eq(config.get_int("database.port"), 5433)
  assert_true(config.get_bool("features.analytics"))
  
  // Create another backup
  let backup_result2 = config_manager.create_backup("backup_config", "after_updates")
  assert_true(backup_result2.is_success())
  
  // Make more changes
  config_manager.update_value("backup_config", "version", "2.0.0")
  config_manager.update_value("backup_config", "database.host", "new-db.example.com")
  
  // Test restore from backup
  let restore_result = config_manager.restore_from_backup("backup_config", "initial_setup")
  assert_true(restore_result.is_success())
  
  // Verify restoration
  let restored_config = config_manager.get_config("backup_config")
  assert_eq(restored_config.get_string("version"), "1.0.0") // Restored to initial
  assert_eq(restored_config.get_int("database.port"), 5432) // Restored to initial
  assert_eq(restored_config.get_string("database.host"), "localhost") // Restored to initial
  assert_false(restored_config.get_bool("features.analytics")) // Restored to initial
  
  // Test selective restore
  config_manager.update_value("backup_config", "version", "2.0.0")
  config_manager.update_value("backup_config", "database.host", "new-db.example.com")
  
  let selective_restore_result = config_manager.selective_restore(
    "backup_config",
    "after_updates",
    ["database.port", "features.analytics"] // Only restore these fields
  )
  assert_true(selective_restore_result.is_success())
  
  // Verify selective restoration
  let selective_restored_config = config_manager.get_config("backup_config")
  assert_eq(selective_restored_config.get_string("version"), "2.0.0") // Not restored
  assert_eq(selective_restored_config.get_string("database.host"), "new-db.example.com") // Not restored
  assert_eq(selective_restored_config.get_int("database.port"), 5433) // Restored
  assert_true(selective_restored_config.get_bool("features.analytics")) // Restored
  
  // Test backup comparison
  let comparison_result = config_manager.compare_backups("backup_config", "initial_setup", "after_updates")
  assert_true(comparison_result.is_success())
  
  let comparison = comparison_result.unwrap()
  assert_true(comparison.differences.length() > 0)
  
  // Verify specific differences
  let version_diff = comparison.differences.find(|d| d.key == "version")
  assert_true(version_diff.is_some())
  assert_eq(version_diff.unwrap().old_value, "1.0.0")
  assert_eq(version_diff.unwrap().new_value, "1.1.0")
  
  let analytics_diff = comparison.differences.find(|d| d.key == "features.analytics")
  assert_true(analytics_diff.is_some())
  assert_eq(analytics_diff.unwrap().old_value, "false")
  assert_eq(analytics_diff.unwrap().new_value, "true")
  
  // Test backup deletion
  let delete_result = config_manager.delete_backup("backup_config", "initial_setup")
  assert_true(delete_result.is_success())
  
  // Verify backup was deleted
  let remaining_backups = config_manager.list_backups("backup_config")
  assert_eq(remaining_backups.length(), 1)
  assert_eq(remaining_backups[0].backup_name, "after_updates")
  
  // Test backup export
  let export_result = config_manager.export_backup("backup_config", "after_updates")
  assert_true(export_result.is_success())
  
  let exported_backup = export_result.unwrap()
  assert_true(exported_backup.content.length() > 0)
  assert_eq(exported_backup.format, "json")
  
  // Test backup import
  let import_result = config_manager.import_backup("backup_config", "imported_backup", exported_backup.content)
  assert_true(import_result.is_success())
  
  // Verify imported backup
  let imported_backups = config_manager.list_backups("backup_config")
  assert_true(imported_backups.length() >= 2)
  
  let imported_backup_info = imported_backups.find(|b| b.backup_name == "imported_backup")
  assert_true(imported_backup_info.is_some())
  
  // Test backup integrity verification
  let integrity_result = config_manager.verify_backup_integrity("backup_config", "after_updates")
  assert_true(integrity_result.is_success())
  
  let integrity = integrity_result.unwrap()
  assert_true(integrity.is_valid)
  assert_true(integrity.verified_checksum.length() > 0)
  
  // Test automatic backup scheduling
  let backup_schedule = BackupSchedule::new()
    .add_backup("daily", Duration::from_days(1), "00:00", 7) // Keep 7 daily backups
    .add_backup("weekly", Duration::from_days(7), "02:00", 4) // Keep 4 weekly backups
    .add_backup("monthly", Duration::from_days(30), "04:00", 12) // Keep 12 monthly backups
  
  config_manager.set_backup_schedule("backup_config", backup_schedule)
  
  // Verify scheduled backups
  let scheduled_backups = config_manager.get_scheduled_backups("backup_config")
  assert_true(scheduled_backups.length() == 3)
  
  for backup in scheduled_backups {
    assert_true(backup.name.length() > 0)
    assert_true(backup.interval > Duration::from_zero())
    assert_true(backup.execution_time.length() > 0)
    assert_true(backup.max_retention > 0)
    assert_true(backup.next_run_time > 0)
  }
}

// Test 9: Configuration Remote Synchronization
test "configuration remote synchronization" {
  let config_manager = ConfigurationManager::new()
  
  // Register configuration schema
  let schema = ConfigurationSchema::new("sync_config")
    .with_field("app_name", FieldType::String, FieldConstraint::Required)
    .with_field("version", FieldType::String, FieldConstraint::Required)
    .with_field("features", FieldType::Object, FieldConstraint::Required)
  
  config_manager.register_schema(schema)
  
  // Initialize remote synchronization
  let remote_sync = RemoteConfigurationSync::new()
  
  // Configure remote storage
  let remote_storage = S3ConfigurationStorage::new(
    "s3://config-bucket/azimuth/",
    "us-west-2",
    "access-key",
    "secret-key"
  )
  
  remote_sync.set_storage(remote_storage)
  
  // Load initial configuration
  let initial_config = {
    "app_name": "azimuth",
    "version": "1.0.0",
    "features": {
      "telemetry": true,
      "analytics": false
    }
  }
  
  let config_result = config_manager.load_from_string("sync_config", initial_config.to_json())
  assert_true(config_result.is_success())
  
  // Test initial upload to remote storage
  let upload_result = remote_sync.upload_config("sync_config", config_manager.get_config("sync_config"))
  assert_true(upload_result.is_success())
  
  let upload_info = upload_result.unwrap()
  assert_eq(upload_info.config_name, "sync_config")
  assert_true(upload_info.remote_path.length() > 0)
  assert_true(upload_info.version > 0)
  assert_true(upload_info.timestamp > 0)
  assert_true(upload_info.etag.length() > 0)
  
  // Test download from remote storage
  let download_result = remote_sync.download_config("sync_config")
  assert_true(download_result.is_success())
  
  let downloaded_config = download_result.unwrap()
  assert_eq(downloaded_config.get_string("app_name"), "azimuth")
  assert_eq(downloaded_config.get_string("version"), "1.0.0")
  assert_true(downloaded_config.get_bool("features.telemetry"))
  assert_false(downloaded_config.get_bool("features.analytics"))
  
  // Make local changes
  config_manager.update_value("sync_config", "version", "1.1.0")
  config_manager.update_value("sync_config", "features.analytics", true)
  
  // Test sync with conflict resolution (no conflict)
  let sync_result = config_manager.sync_with_remote("sync_config", ConflictResolution::LocalWins)
  assert_true(sync_result.is_success())
  
  let sync_info = sync_result.unwrap()
  assert_eq(sync_info.config_name, "sync_config")
  assert_true(sync_info.conflicts_resolved == 0) // No conflicts
  assert_true(sync_info.changes_uploaded > 0)
  
  // Simulate remote changes (in real scenario, this would be done by another instance)
  let remote_changes = {
    "app_name": "azimuth",
    "version": "1.2.0",
    "features": {
      "telemetry": true,
      "analytics": false,
      "beta_features": true
    }
  }
  
  // Upload remote changes directly
  remote_sync.upload_config_direct("sync_config", remote_changes.to_json())
  
  // Make local changes to create conflict
  config_manager.update_value("sync_config", "version", "1.3.0")
  config_manager.update_value("sync_config", "features.analytics", true)
  
  // Test sync with conflict resolution (LocalWins)
  let conflict_sync_result = config_manager.sync_with_remote("sync_config", ConflictResolution::LocalWins)
  assert_true(conflict_sync_result.is_success())
  
  let conflict_sync_info = conflict_sync_result.unwrap()
  assert_true(conflict_sync_info.conflicts_resolved > 0)
  
  // Verify local wins resolution
  let local_wins_config = config_manager.get_config("sync_config")
  assert_eq(local_wins_config.get_string("version"), "1.3.0") // Local value kept
  assert_true(local_wins_config.get_bool("features.analytics")) // Local value kept
  assert_true(local_wins_config.get_bool("features.beta_features")) // Remote value merged
  
  // Test sync with conflict resolution (RemoteWins)
  config_manager.update_value("sync_config", "version", "1.4.0")
  
  let remote_wins_sync_result = config_manager.sync_with_remote("sync_config", ConflictResolution::RemoteWins)
  assert_true(remote_wins_sync_result.is_success())
  
  let remote_wins_sync_info = remote_wins_sync_result.unwrap()
  assert_true(remote_wins_sync_info.conflicts_resolved > 0)
  
  // Verify remote wins resolution
  let remote_wins_config = config_manager.get_config("sync_config")
  assert_eq(remote_wins_config.get_string("version"), "1.2.0") // Remote value kept
  assert_false(remote_wins_config.get_bool("features.analytics")) // Remote value kept
  assert_true(remote_wins_config.get_bool("features.beta_features")) // Remote value kept
  
  // Test sync with conflict resolution (Manual)
  config_manager.update_value("sync_config", "version", "1.5.0")
  
  let manual_sync_result = config_manager.sync_with_remote("sync_config", ConflictResolution::Manual)
  assert_true(manual_sync_result.is_success())
  
  let manual_sync_info = manual_sync_result.unwrap()
  assert_true(manual_sync_info.conflicts_detected > 0)
  assert_true(manual_sync_info.conflicts.length() > 0)
  
  // Verify conflict details
  let version_conflict = manual_sync_info.conflicts.find(|c| c.key == "version")
  assert_true(version_conflict.is_some())
  
  let conflict = version_conflict.unwrap()
  assert_eq(conflict.local_value, "1.5.0")
  assert_eq(conflict.remote_value, "1.2.0")
  
  // Test manual conflict resolution
  let resolved_conflicts = [
    ConflictResolution::new("version", "1.6.0"), // Custom resolution
    ConflictResolution::new("features.analytics", "true") // Keep local value
  ]
  
  let manual_resolve_result = config_manager.resolve_conflicts("sync_config", resolved_conflicts)
  assert_true(manual_resolve_result.is_success())
  
  // Verify manual resolution
  let resolved_config = config_manager.get_config("sync_config")
  assert_eq(resolved_config.get_string("version"), "1.6.0") // Custom resolution
  assert_true(resolved_config.get_bool("features.analytics")) // Local value kept
  
  // Test sync history
  let sync_history = config_manager.get_sync_history("sync_config")
  assert_true(sync_history.length() > 0)
  
  for sync_event in sync_history {
    assert_true(sync_event.timestamp > 0)
    assert_true(sync_event.direction == "upload" || sync_event.direction == "download")
    assert_true(sync_event.conflicts_resolved >= 0)
  }
  
  // Test automatic sync scheduling
  let sync_schedule = SyncSchedule::new()
    .add_sync("auto_sync", Duration::from_minutes(5), ConflictResolution::LocalWins)
  
  config_manager.set_sync_schedule("sync_config", sync_schedule)
  
  // Verify scheduled sync
  let scheduled_syncs = config_manager.get_scheduled_syncs("sync_config")
  assert_true(scheduled_syncs.length() == 1)
  
  let auto_sync = scheduled_syncs[0]
  assert_eq(auto_sync.name, "auto_sync")
  assert_eq(auto_sync.interval, Duration::from_minutes(5))
  assert_eq(auto_sync.conflict_resolution, ConflictResolution::LocalWins)
  assert_true(auto_sync.next_sync_time > 0)
  
  // Test sync metrics
  let sync_metrics = config_manager.get_sync_metrics("sync_config")
  
  assert_true(sync_metrics.total_syncs > 0)
  assert_true(sync_metrics.successful_syncs > 0)
  assert_true(sync_metrics.failed_syncs >= 0)
  assert_true(sync_metrics.conflicts_resolved > 0)
  assert_true(sync_metrics.average_sync_time_ms > 0)
  assert_true(sync_metrics.last_sync_time > 0)
}

// Test 10: Configuration Performance and Optimization
test "configuration performance and optimization" {
  let config_manager = ConfigurationManager::new()
  
  // Register configuration schema
  let schema = ConfigurationSchema::new("performance_config")
    .with_field("app_name", FieldType::String, FieldConstraint::Required)
    .with_field("settings", FieldType::Object, FieldConstraint::Required)
  
  config_manager.register_schema(schema)
  
  // Generate large configuration
  let large_config = generate_large_configuration(1000) // 1000 settings
  
  // Test configuration loading performance
  let load_start_time = get_current_timestamp()
  let load_result = config_manager.load_from_string("performance_config", large_config.to_json())
  let load_end_time = get_current_timestamp()
  let load_time = load_end_time - load_start_time
  
  assert_true(load_result.is_success())
  assert_true(load_time.to_millis() < 1000) // Should load in less than 1 second
  
  // Test configuration access performance
  let config = config_manager.get_config("performance_config")
  
  let access_start_time = get_current_timestamp()
  for i in 0..=1000 {
    let key = "settings.setting_" + i.to_string()
    let _value = config.get_string(key)
  }
  let access_end_time = get_current_timestamp()
  let access_time = access_end_time - access_start_time
  
  assert_true(access_time.to_millis() < 100) // Should access 1000 values in less than 100ms
  
  // Test configuration update performance
  let update_start_time = get_current_timestamp()
  for i in 0..=100 {
    let key = "settings.setting_" + i.to_string()
    let value = "updated_value_" + i.to_string()
    config_manager.update_value("performance_config", key, value)
  }
  let update_end_time = get_current_timestamp()
  let update_time = update_end_time - update_start_time
  
  assert_true(update_time.to_millis() < 500) // Should update 100 values in less than 500ms
  
  // Test configuration caching
  config_manager.enable_caching(true)
  
  // Test cached access performance
  let cached_access_start_time = get_current_timestamp()
  for i in 0..=1000 {
    let key = "settings.setting_" + i.to_string()
    let _value = config.get_string(key)
  }
  let cached_access_end_time = get_current_timestamp()
  let cached_access_time = cached_access_end_time - cached_access_start_time
  
  // Cached access should be faster
  assert_true(cached_access_time.to_millis() < access_time.to_millis())
  
  // Test cache hit rate
  let cache_metrics = config_manager.get_cache_metrics()
  assert_true(cache_metrics.hit_rate > 0.8) // Should have high hit rate
  assert_true(cache_metrics.total_requests > 0)
  assert_true(cache_metrics.cache_hits > 0)
  
  // Test configuration validation performance
  let validation_schema = ConfigurationSchema::new("validation_performance")
    .with_field("field_1", FieldType::String, FieldConstraint::Required)
    .with_constraint("field_1", RegexConstraint::new("^[a-zA-Z0-9]+$"))
    .with_field("field_2", FieldType::Integer, FieldConstraint::Required)
    .with_constraint("field_2", RangeConstraint::new(1, 100))
    // Add many more fields for performance testing
  for i in 0..=100 {
    validation_schema.with_field("field_" + (i + 3).to_string(), FieldType::String, FieldConstraint::Optional)
  }
  
  config_manager.register_schema(validation_schema)
  
  let validation_config = generate_validation_test_config(103)
  
  let validation_start_time = get_current_timestamp()
  let validation_result = config_manager.load_from_string("validation_performance", validation_config.to_json())
  let validation_end_time = get_current_timestamp()
  let validation_time = validation_end_time - validation_start_time
  
  assert_true(validation_result.is_success())
  assert_true(validation_time.to_millis() < 500) // Should validate in less than 500ms
  
  // Test configuration serialization performance
  let serialization_start_time = get_current_timestamp()
  let serialized_config = config_manager.serialize("performance_config")
  let serialization_end_time = get_current_timestamp()
  let serialization_time = serialization_end_time - serialization_start_time
  
  assert_true(serialized_config.length() > 0)
  assert_true(serialization_time.to_millis() < 1000) // Should serialize in less than 1 second
  
  // Test configuration deserialization performance
  let deserialization_start_time = get_current_timestamp()
  let deserialization_result = config_manager.deserialize("performance_config", serialized_config)
  let deserialization_end_time = get_current_timestamp()
  let deserialization_time = deserialization_end_time - deserialization_start_time
  
  assert_true(deserialization_result.is_success())
  assert_true(deserialization_time.to_millis() < 1000) // Should deserialize in less than 1 second
  
  // Test configuration memory usage
  let memory_before = get_memory_usage()
  
  // Load multiple configurations
  for i in 0..=100 {
    let config_name = "memory_test_" + i.to_string()
    let test_config = {
      "app_name": "test_app_" + i.to_string(),
      "settings": large_config.settings
    }
    
    config_manager.load_from_string(config_name, test_config.to_json())
  }
  
  let memory_after = get_memory_usage()
  let memory_used = memory_after - memory_before
  
  // Verify reasonable memory usage
  assert_true(memory_used < 100 * 1024 * 1024) // Should use less than 100MB for 100 configs
  
  // Test configuration cleanup
  let cleanup_start_time = get_current_timestamp()
  
  for i in 0..=100 {
    let config_name = "memory_test_" + i.to_string()
    config_manager.unload(config_name)
  }
  
  let cleanup_end_time = get_current_timestamp()
  let cleanup_time = cleanup_end_time - cleanup_start_time
  
  assert_true(cleanup_time.to_millis() < 1000) // Should cleanup in less than 1 second
  
  // Verify memory was released
  let memory_after_cleanup = get_memory_usage()
  let memory_released = memory_after - memory_after_cleanup
  
  assert_true(memory_released > memory_used * 0.8) // Should release at least 80% of used memory
  
  // Test configuration performance metrics
  let performance_metrics = config_manager.get_performance_metrics()
  
  assert_true(performance_metrics.total_loads > 0)
  assert_true(performance_metrics.total_accesses > 0)
  assert_true(performance_metrics.total_updates > 0)
  assert_true(performance_metrics.average_load_time_ms > 0)
  assert_true(performance_metrics.average_access_time_ms > 0)
  assert_true(performance_metrics.average_update_time_ms > 0)
  assert_true(performance_metrics.peak_memory_usage > 0)
  assert_true(performance_metrics.current_memory_usage > 0)
}