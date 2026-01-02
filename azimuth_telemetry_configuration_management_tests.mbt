// Azimuth Telemetry Configuration Management Tests
// This file contains comprehensive test cases for telemetry configuration management

// Test 1: Dynamic Configuration Updates
test "dynamic configuration updates without service restart" {
  // Create configuration manager
  let config_manager = azimuth::ConfigurationManager::new()
  
  // Initialize with default configuration
  let default_config = {
    "service.name": "telemetry-service",
    "service.version": "1.0.0",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.1",
    "telemetry.export_interval_ms": "5000",
    "telemetry.max_batch_size": "100",
    "telemetry.compression": "gzip",
    "logging.level": "INFO",
    "metrics.enabled": "true",
    "tracing.enabled": "true"
  }
  
  azimuth::ConfigurationManager::load_initial_config(config_manager, default_config)
  
  // Verify initial configuration
  let initial_config = azimuth::ConfigurationManager::get_all(config_manager)
  assert_eq(initial_config["service.name"], "telemetry-service")
  assert_eq(initial_config["telemetry.sample_rate"], "0.1")
  assert_eq(initial_config["telemetry.export_interval_ms"], "5000")
  
  // Create telemetry processor with configuration
  let processor = azimuth::TelemetryProcessor::new()
  azimuth::TelemetryProcessor::configure(processor, config_manager)
  
  // Verify processor uses initial configuration
  let processor_config = azimuth::TelemetryProcessor::get_config(processor)
  assert_eq(processor_config.sample_rate, 0.1)
  assert_eq(processor_config.export_interval_ms, 5000)
  assert_eq(processor_config.max_batch_size, 100)
  
  // Test dynamic configuration update
  let updated_config = {
    "telemetry.sample_rate": "0.5", // Changed from 0.1 to 0.5
    "telemetry.export_interval_ms": "10000", // Changed from 5000 to 10000
    "telemetry.max_batch_size": "200", // Changed from 100 to 200
    "logging.level": "DEBUG" // Changed from INFO to DEBUG
  }
  
  let update_result = azimuth::ConfigurationManager::update_config(config_manager, updated_config)
  assert_true(update_result.success)
  assert_true(update_result.applied_changes > 0)
  
  // Verify configuration was updated
  let new_config = azimuth::ConfigurationManager::get_all(config_manager)
  assert_eq(new_config["telemetry.sample_rate"], "0.5")
  assert_eq(new_config["telemetry.export_interval_ms"], "10000")
  assert_eq(new_config["telemetry.max_batch_size"], "200")
  assert_eq(new_config["logging.level"], "DEBUG")
  
  // Unchanged values should remain the same
  assert_eq(new_config["service.name"], "telemetry-service")
  assert_eq(new_config["service.version"], "1.0.0")
  
  // Verify processor picked up new configuration without restart
  let updated_processor_config = azimuth::TelemetryProcessor::get_config(processor)
  assert_eq(updated_processor_config.sample_rate, 0.5)
  assert_eq(updated_processor_config.export_interval_ms, 10000)
  assert_eq(updated_processor_config.max_batch_size, 200)
  
  // Test configuration validation
  let invalid_config = {
    "telemetry.sample_rate": "1.5", // Invalid: > 1.0
    "telemetry.export_interval_ms": "-1000", // Invalid: negative
    "telemetry.max_batch_size": "0" // Invalid: zero
  }
  
  let invalid_update = azimuth::ConfigurationManager::update_config(config_manager, invalid_config)
  assert_false(invalid_update.success)
  assert_true(invalid_update.validation_errors.length() > 0)
  
  // Verify invalid configuration was not applied
  let config_after_invalid = azimuth::ConfigurationManager::get_all(config_manager)
  assert_eq(config_after_invalid["telemetry.sample_rate"], "0.5") // Should remain unchanged
  assert_eq(config_after_invalid["telemetry.export_interval_ms"], "10000") // Should remain unchanged
  assert_eq(config_after_invalid["telemetry.max_batch_size"], "200") // Should remain unchanged
  
  // Test configuration rollback
  let rollback_result = azimuth::ConfigurationManager::rollback_to_previous(config_manager)
  assert_true(rollback_result.success)
  
  let rolled_back_config = azimuth::ConfigurationManager::get_all(config_manager)
  assert_eq(rolled_back_config["telemetry.sample_rate"], "0.1") // Should be back to original
  assert_eq(rolled_back_config["telemetry.export_interval_ms"], "5000") // Should be back to original
  assert_eq(rolled_back_config["telemetry.max_batch_size"], "100") // Should be back to original
}

// Test 2: Configuration Environment Profiles
test "configuration environment profiles and inheritance" {
  // Create configuration manager with environment support
  let config_manager = azimuth::ConfigurationManager::new()
  azimuth::ConfigurationManager::enable_environments(config_manager, true)
  
  // Define base configuration
  let base_config = {
    "service.name": "telemetry-service",
    "service.version": "1.0.0",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.1",
    "telemetry.export_interval_ms": "5000",
    "database.host": "localhost",
    "database.port": "5432",
    "logging.level": "INFO"
  }
  
  // Define development environment overrides
  let dev_config = {
    "telemetry.sample_rate": "1.0", // Sample all in development
    "telemetry.export_interval_ms": "1000", // More frequent exports
    "database.host": "dev-db.example.com",
    "logging.level": "DEBUG",
    "debug.enabled": "true"
  }
  
  // Define production environment overrides
  let prod_config = {
    "telemetry.sample_rate": "0.01", // Sample less in production
    "telemetry.export_interval_ms": "10000", // Less frequent exports
    "database.host": "prod-db.example.com",
    "logging.level": "WARN",
    "debug.enabled": "false"
  }
  
  // Load configurations
  azimuth::ConfigurationManager::load_base_config(config_manager, base_config)
  azimuth::ConfigurationManager::load_environment_config(config_manager, "development", dev_config)
  azimuth::ConfigurationManager::load_environment_config(config_manager, "production", prod_config)
  
  // Test development environment
  azimuth::ConfigurationManager::set_active_environment(config_manager, "development")
  
  let dev_active_config = azimuth::ConfigurationManager::get_active_config(config_manager)
  
  // Verify base values are inherited
  assert_eq(dev_active_config["service.name"], "telemetry-service")
  assert_eq(dev_active_config["service.version"], "1.0.0")
  assert_eq(dev_active_config["telemetry.enabled"], "true")
  assert_eq(dev_active_config["database.port"], "5432")
  
  // Verify development overrides are applied
  assert_eq(dev_active_config["telemetry.sample_rate"], "1.0")
  assert_eq(dev_active_config["telemetry.export_interval_ms"], "1000")
  assert_eq(dev_active_config["database.host"], "dev-db.example.com")
  assert_eq(dev_active_config["logging.level"], "DEBUG")
  assert_eq(dev_active_config["debug.enabled"], "true")
  
  // Test production environment
  azimuth::ConfigurationManager::set_active_environment(config_manager, "production")
  
  let prod_active_config = azimuth::ConfigurationManager::get_active_config(config_manager)
  
  // Verify base values are inherited
  assert_eq(prod_active_config["service.name"], "telemetry-service")
  assert_eq(prod_active_config["service.version"], "1.0.0")
  assert_eq(prod_active_config["telemetry.enabled"], "true")
  assert_eq(prod_active_config["database.port"], "5432")
  
  // Verify production overrides are applied
  assert_eq(prod_active_config["telemetry.sample_rate"], "0.01")
  assert_eq(prod_active_config["telemetry.export_interval_ms"], "10000")
  assert_eq(prod_active_config["database.host"], "prod-db.example.com")
  assert_eq(prod_active_config["logging.level"], "WARN")
  assert_eq(prod_active_config["debug.enabled"], "false")
  
  // Test environment-specific configuration updates
  let dev_update = {
    "telemetry.sample_rate": "0.8", // Update dev-specific value
    "new.dev.feature": "enabled" // Add dev-specific value
  }
  
  let dev_update_result = azimuth::ConfigurationManager::update_environment_config(
    config_manager, 
    "development", 
    dev_update
  )
  
  assert_true(dev_update_result.success)
  
  // Verify only development environment is affected
  let updated_dev_config = azimuth::ConfigurationManager::get_environment_config(config_manager, "development")
  assert_eq(updated_dev_config["telemetry.sample_rate"], "0.8")
  assert_eq(updated_dev_config["new.dev.feature"], "enabled")
  
  let unchanged_prod_config = azimuth::ConfigurationManager::get_environment_config(config_manager, "production")
  assert_eq(unchanged_prod_config["telemetry.sample_rate"], "0.01") // Should be unchanged
  assert_true(!unchanged_prod_config.contains("new.dev.feature")) // Should not exist
  
  // Test configuration inheritance chain
  let staging_config = {
    "telemetry.sample_rate": "0.05",
    "logging.level": "INFO"
  }
  
  // Add staging environment that inherits from production
  azimuth::ConfigurationManager::load_environment_config_with_inheritance(
    config_manager, 
    "staging", 
    staging_config, 
    "production"
  )
  
  azimuth::ConfigurationManager::set_active_environment(config_manager, "staging")
  
  let staging_active_config = azimuth::ConfigurationManager::get_active_config(config_manager)
  
  // Should inherit from production, then override with staging values
  assert_eq(staging_active_config["telemetry.sample_rate"], "0.05") // Staging override
  assert_eq(staging_active_config["logging.level"], "INFO") // Staging override
  assert_eq(staging_active_config["telemetry.export_interval_ms"], "10000") // Inherited from production
  assert_eq(staging_active_config["database.host"], "prod-db.example.com") // Inherited from production
  assert_eq(staging_active_config["debug.enabled"], "false") // Inherited from production
}

// Test 3: Configuration Security and Access Control
test "configuration security and access control" {
  // Create secure configuration manager
  let config_manager = azimuth::SecureConfigurationManager::new()
  
  // Configure security settings
  azimuth::SecureConfigurationManager::configure_security(config_manager, {
    "encryption_at_rest": true,
    "encryption_in_transit": true,
    "access_control": true,
    "audit_logging": true,
    "sensitive_key_patterns": ["password", "secret", "key", "token"]
  })
  
  // Define user roles and permissions
  let admin_role = {
    "name": "admin",
    "permissions": ["read", "write", "delete", "manage_users", "manage_roles"],
    "restricted_keys": []
  }
  
  let operator_role = {
    "name": "operator",
    "permissions": ["read", "write"],
    "restricted_keys": ["database.password", "api.secret", "encryption.key"]
  }
  
  let viewer_role = {
    "name": "viewer",
    "permissions": ["read"],
    "restricted_keys": ["database.password", "api.secret", "encryption.key", "service.private_key"]
  }
  
  azimuth::SecureConfigurationManager::create_role(config_manager, admin_role)
  azimuth::SecureConfigurationManager::create_role(config_manager, operator_role)
  azimuth::SecureConfigurationManager::create_role(config_manager, viewer_role)
  
  // Create users
  let admin_user = azimuth::User::new("admin", "admin123", "admin")
  let operator_user = azimuth::User::new("operator", "operator123", "operator")
  let viewer_user = azimuth::User::new("viewer", "viewer123", "viewer")
  
  azimuth::SecureConfigurationManager::create_user(config_manager, admin_user)
  azimuth::SecureConfigurationManager::create_user(config_manager, operator_user)
  azimuth::SecureConfigurationManager::create_user(config_manager, viewer_user)
  
  // Load configuration with sensitive values
  let secure_config = {
    "service.name": "secure-service",
    "database.host": "db.example.com",
    "database.port": "5432",
    "database.password": "super-secret-password",
    "api.secret": "api-secret-key-12345",
    "encryption.key": "encryption-key-67890",
    "service.public_key": "public-key-value",
    "service.private_key": "private-key-value",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.1"
  }
  
  azimuth::SecureConfigurationManager::load_config(config_manager, secure_config)
  
  // Test admin access (full access)
  let admin_session = azimuth::SecureConfigurationManager::authenticate(
    config_manager, 
    "admin", 
    "admin123"
  )
  
  assert_true(admin_session.success)
  
  let admin_read_result = azimuth::SecureConfigurationManager::get_all_secure(
    config_manager, 
    admin_session.session_id
  )
  
  assert_true(admin_read_result.success)
  assert_true(admin_read_result.data.contains("database.password"))
  assert_true(admin_read_result.data.contains("api.secret"))
  assert_true(admin_read_result.data.contains("encryption.key"))
  assert_true(admin_read_result.data.contains("service.private_key"))
  
  // Test operator access (restricted access)
  let operator_session = azimuth::SecureConfigurationManager::authenticate(
    config_manager, 
    "operator", 
    "operator123"
  )
  
  assert_true(operator_session.success)
  
  let operator_read_result = azimuth::SecureConfigurationManager::get_all_secure(
    config_manager, 
    operator_session.session_id
  )
  
  assert_true(operator_read_result.success)
  assert_true(operator_read_result.data.contains("service.name"))
  assert_true(operator_read_result.data.contains("database.host"))
  assert_false(operator_read_result.data.contains("database.password")) // Restricted
  assert_false(operator_read_result.data.contains("api.secret")) // Restricted
  assert_false(operator_read_result.data.contains("encryption.key")) // Restricted
  
  // Test viewer access (most restricted)
  let viewer_session = azimuth::SecureConfigurationManager::authenticate(
    config_manager, 
    "viewer", 
    "viewer123"
  )
  
  assert_true(viewer_session.success)
  
  let viewer_read_result = azimuth::SecureConfigurationManager::get_all_secure(
    config_manager, 
    viewer_session.session_id
  )
  
  assert_true(viewer_read_result.success)
  assert_true(viewer_read_result.data.contains("service.name"))
  assert_true(viewer_read_result.data.contains("telemetry.enabled"))
  assert_false(viewer_read_result.data.contains("database.password")) // Restricted
  assert_false(viewer_read_result.data.contains("api.secret")) // Restricted
  assert_false(viewer_read_result.data.contains("encryption.key")) // Restricted
  assert_false(viewer_read_result.data.contains("service.private_key")) // Restricted
  
  // Test write permissions
  let admin_write_result = azimuth::SecureConfigurationManager::update_config_secure(
    config_manager, 
    admin_session.session_id,
    {"telemetry.sample_rate": "0.2"}
  )
  
  assert_true(admin_write_result.success)
  
  let operator_write_result = azimuth::SecureConfigurationManager::update_config_secure(
    config_manager, 
    operator_session.session_id,
    {"telemetry.sample_rate": "0.3"}
  )
  
  assert_true(operator_write_result.success)
  
  let viewer_write_result = azimuth::SecureConfigurationManager::update_config_secure(
    config_manager, 
    viewer_session.session_id,
    {"telemetry.sample_rate": "0.4"}
  )
  
  assert_false(viewer_write_result.success) // Viewer should not have write permission
  assert_true(viewer_write_result.error.contains("permission_denied"))
  
  // Test sensitive key access restrictions
  let operator_sensitive_update = azimuth::SecureConfigurationManager::update_config_secure(
    config_manager, 
    operator_session.session_id,
    {"database.password": "new-password"}
  )
  
  assert_false(operator_sensitive_update.success) // Operator should not update sensitive keys
  assert_true(operator_sensitive_update.error.contains("access_denied"))
  
  // Test audit logging
  let audit_log = azimuth::SecureConfigurationManager::get_audit_log(config_manager)
  
  assert_true(audit_log.length() > 0)
  
  // Verify audit log contains authentication events
  let auth_events = audit_log.filter(|entry| entry.event_type == "authentication")
  assert_true(auth_events.length() >= 3) // At least 3 authentication events
  
  // Verify audit log contains read events
  let read_events = audit_log.filter(|entry| entry.event_type == "config_read")
  assert_true(read_events.length() >= 3) // At least 3 read events
  
  // Verify audit log contains write events
  let write_events = audit_log.filter(|entry| entry.event_type == "config_write")
  assert_true(write_events.length() >= 2) // At least 2 write events (admin and operator)
  
  // Verify audit log contains denied access events
  let denied_events = audit_log.filter(|entry| entry.event_type == "access_denied")
  assert_true(denied_events.length() >= 2) // At least 2 denied events (viewer write and operator sensitive)
  
  // Test configuration encryption
  let encryption_status = azimuth::SecureConfigurationManager::get_encryption_status(config_manager)
  assert_true(encryption_status.at_rest_enabled)
  assert_true(encryption_status.in_transit_enabled)
  assert_true(encryption_status.key_id.length() > 0)
  
  // Verify sensitive values are encrypted
  let raw_storage = azimuth::SecureConfigurationManager::get_raw_storage(config_manager)
  assert_true(raw_storage.contains("database.password"))
  assert_false(raw_storage.contains("super-secret-password")) // Should be encrypted
}

// Test 4: Configuration Versioning and Rollback
test "configuration versioning and rollback capabilities" {
  // Create versioned configuration manager
  let versioned_config = azimuth::VersionedConfigurationManager::new()
  
  // Configure versioning settings
  azimuth::VersionedConfigurationManager::configure_versioning(versioned_config, {
    "max_versions": 10,
    "auto_version": true,
    "version_comment": true,
    "version_author": true
  })
  
  // Load initial configuration (version 1)
  let v1_config = {
    "service.name": "versioned-service",
    "service.version": "1.0.0",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.1",
    "telemetry.export_interval_ms": "5000",
    "logging.level": "INFO"
  }
  
  let v1_result = azimuth::VersionedConfigurationManager::load_config_with_version(
    versioned_config, 
    v1_config, 
    "Initial configuration", 
    "admin"
  )
  
  assert_true(v1_result.success)
  assert_eq(v1_result.version, 1)
  
  // Update configuration (version 2)
  let v2_update = {
    "telemetry.sample_rate": "0.2",
    "telemetry.export_interval_ms": "10000",
    "logging.level": "DEBUG"
  }
  
  let v2_result = azimuth::VersionedConfigurationManager::update_config_with_version(
    versioned_config, 
    v2_update, 
    "Increase sample rate and export interval", 
    "operator"
  )
  
  assert_true(v2_result.success)
  assert_eq(v2_result.version, 2)
  
  // Update configuration again (version 3)
  let v3_update = {
    "telemetry.sample_rate": "0.15",
    "new.feature.enabled": "true"
  }
  
  let v3_result = azimuth::VersionedConfigurationManager::update_config_with_version(
    versioned_config, 
    v3_update, 
    "Add new feature and adjust sample rate", 
    "admin"
  )
  
  assert_true(v3_result.success)
  assert_eq(v3_result.version, 3)
  
  // Verify version history
  let version_history = azimuth::VersionedConfigurationManager::get_version_history(versioned_config)
  assert_eq(version_history.length(), 3)
  
  // Verify version details
  let v1_details = azimuth::VersionedConfigurationManager::get_version_details(versioned_config, 1)
  assert_eq(v1_details.version, 1)
  assert_eq(v1_details.comment, "Initial configuration")
  assert_eq(v1_details.author, "admin")
  assert_eq(v1_details.config["telemetry.sample_rate"], "0.1")
  
  let v2_details = azimuth::VersionedConfigurationManager::get_version_details(versioned_config, 2)
  assert_eq(v2_details.version, 2)
  assert_eq(v2_details.comment, "Increase sample rate and export interval")
  assert_eq(v2_details.author, "operator")
  assert_eq(v2_details.config["telemetry.sample_rate"], "0.2")
  
  let v3_details = azimuth::VersionedConfigurationManager::get_version_details(versioned_config, 3)
  assert_eq(v3_details.version, 3)
  assert_eq(v3_details.comment, "Add new feature and adjust sample rate")
  assert_eq(v3_details.author, "admin")
  assert_eq(v3_details.config["telemetry.sample_rate"], "0.15")
  
  // Test rollback to specific version
  let rollback_result = azimuth::VersionedConfigurationManager::rollback_to_version(
    versioned_config, 
    2, 
    "Rollback due to performance issues", 
    "admin"
  )
  
  assert_true(rollback_result.success)
  assert_eq(rollback_result.active_version, 2)
  
  // Verify current configuration matches version 2
  let current_config = azimuth::VersionedConfigurationManager::get_current_config(versioned_config)
  assert_eq(current_config["telemetry.sample_rate"], "0.2") // From v2
  assert_eq(current_config["telemetry.export_interval_ms"], "10000") // From v2
  assert_eq(current_config["logging.level"], "DEBUG") // From v2
  assert_false(current_config.contains("new.feature.enabled")) // Not in v2
  
  // Verify rollback created a new version (v4)
  let updated_history = azimuth::VersionedConfigurationManager::get_version_history(versioned_config)
  assert_eq(updated_history.length(), 4)
  
  let v4_details = azimuth::VersionedConfigurationManager::get_version_details(versioned_config, 4)
  assert_eq(v4_details.version, 4)
  assert_eq(v4_details.comment, "Rollback due to performance issues")
  assert_eq(v4_details.author, "admin")
  assert_eq(v4_details.rollback_from_version, 3)
  assert_eq(v4_details.rollback_to_version, 2)
  
  // Test version comparison
  let comparison_result = azimuth::VersionedConfigurationManager::compare_versions(
    versioned_config, 
    3, 
    4
  )
  
  assert_true(comparison_result.has_differences)
  assert_true(comparison_result.differences.contains("telemetry.sample_rate"))
  assert_true(comparison_result.differences.contains("new.feature.enabled"))
  
  // Test version diff
  let diff_result = azimuth::VersionedConfigurationManager::version_diff(
    versioned_config, 
    2, 
    3
  )
  
  assert_true(diff_result.added.contains("new.feature.enabled"))
  assert_true(diff_result.modified.contains("telemetry.sample_rate"))
  assert_true(diff_result.removed.contains("logging.level")) // Assuming this was removed in v3
  
  // Test version cleanup (keep only last 3 versions)
  let cleanup_result = azimuth::VersionedConfigurationManager::cleanup_old_versions(
    versioned_config, 
    3
  )
  
  assert_true(cleanup_result.success)
  assert_eq(cleanup_result.deleted_versions, 1) // Should delete version 1
  
  let final_history = azimuth::VersionedConfigurationManager::get_version_history(versioned_config)
  assert_eq(final_history.length(), 3) // Should have versions 2, 3, 4
  
  // Verify remaining versions
  let remaining_versions = final_history.map(|v| v.version)
  assert_true(remaining_versions.contains(2))
  assert_true(remaining_versions.contains(3))
  assert_true(remaining_versions.contains(4))
  assert_false(remaining_versions.contains(1)) // Should be deleted
}

// Test 5: Configuration Templates and Inheritance
test "configuration templates and inheritance" {
  // Create template-based configuration manager
  let template_config = azimuth::TemplateConfigurationManager::new()
  
  // Define base templates
  let service_template = {
    "name": "service-template",
    "description": "Base template for all services",
    "config": {
      "service.name": "{{service_name}}",
      "service.version": "{{service_version}}",
      "service.environment": "{{environment}}",
      "telemetry.enabled": "true",
      "telemetry.sample_rate": "{{telemetry_sample_rate}}",
      "logging.level": "{{logging_level}}",
      "health.check.enabled": "true",
      "health.check.interval_ms": "30000"
    },
    "variables": [
      "service_name",
      "service_version", 
      "environment",
      "telemetry_sample_rate",
      "logging_level"
    ]
  }
  
  let database_template = {
    "name": "database-template",
    "description": "Database configuration template",
    "config": {
      "database.host": "{{db_host}}",
      "database.port": "{{db_port}}",
      "database.name": "{{db_name}}",
      "database.pool.size": "{{db_pool_size}}",
      "database.timeout_ms": "5000",
      "database.ssl.enabled": "{{db_ssl_enabled}}"
    },
    "variables": [
      "db_host",
      "db_port",
      "db_name", 
      "db_pool_size",
      "db_ssl_enabled"
    ]
  }
  
  let telemetry_template = {
    "name": "telemetry-template", 
    "description": "Advanced telemetry configuration",
    "config": {
      "telemetry.exporter": "{{telemetry_exporter}}",
      "telemetry.endpoint": "{{telemetry_endpoint}}",
      "telemetry.batch.size": "{{telemetry_batch_size}}",
      "telemetry.batch.timeout_ms": "{{telemetry_batch_timeout}}",
      "telemetry.compression": "{{telemetry_compression}}",
      "telemetry.retry.max_attempts": "3",
      "telemetry.retry.backoff_ms": "1000"
    },
    "variables": [
      "telemetry_exporter",
      "telemetry_endpoint",
      "telemetry_batch_size",
      "telemetry_batch_timeout",
      "telemetry_compression"
    ]
  }
  
  // Register templates
  azimuth::TemplateConfigurationManager::register_template(template_config, service_template)
  azimuth::TemplateConfigurationManager::register_template(template_config, database_template)
  azimuth::TemplateConfigurationManager::register_template(template_config, telemetry_template)
  
  // Create service configuration using templates
  let service_variables = {
    "service_name": "user-service",
    "service_version": "2.1.0",
    "environment": "production",
    "telemetry_sample_rate": "0.05",
    "logging_level": "INFO"
  }
  
  let database_variables = {
    "db_host": "prod-db.example.com",
    "db_port": "5432",
    "db_name": "userdb",
    "db_pool_size": "20",
    "db_ssl_enabled": "true"
  }
  
  let telemetry_variables = {
    "telemetry_exporter": "otlp",
    "telemetry_endpoint": "https://otel.example.com:4317",
    "telemetry_batch_size": "500",
    "telemetry_batch_timeout": "5000",
    "telemetry_compression": "gzip"
  }
  
  // Generate configuration from templates
  let generated_config = azimuth::TemplateConfigurationManager::generate_from_templates(
    template_config,
    ["service-template", "database-template", "telemetry-template"],
    {
      "service_name": "user-service",
      "service_version": "2.1.0", 
      "environment": "production",
      "telemetry_sample_rate": "0.05",
      "logging_level": "INFO",
      "db_host": "prod-db.example.com",
      "db_port": "5432",
      "db_name": "userdb",
      "db_pool_size": "20",
      "db_ssl_enabled": "true",
      "telemetry_exporter": "otlp",
      "telemetry_endpoint": "https://otel.example.com:4317",
      "telemetry_batch_size": "500",
      "telemetry_batch_timeout": "5000",
      "telemetry_compression": "gzip"
    }
  )
  
  assert_true(generated_config.success)
  
  let final_config = generated_config.config
  
  // Verify service template values
  assert_eq(final_config["service.name"], "user-service")
  assert_eq(final_config["service.version"], "2.1.0")
  assert_eq(final_config["service.environment"], "production")
  assert_eq(final_config["telemetry.sample_rate"], "0.05")
  assert_eq(final_config["logging.level"], "INFO")
  assert_eq(final_config["health.check.enabled"], "true")
  assert_eq(final_config["health.check.interval_ms"], "30000")
  
  // Verify database template values
  assert_eq(final_config["database.host"], "prod-db.example.com")
  assert_eq(final_config["database.port"], "5432")
  assert_eq(final_config["database.name"], "userdb")
  assert_eq(final_config["database.pool.size"], "20")
  assert_eq(final_config["database.ssl.enabled"], "true")
  assert_eq(final_config["database.timeout_ms"], "5000")
  
  // Verify telemetry template values
  assert_eq(final_config["telemetry.exporter"], "otlp")
  assert_eq(final_config["telemetry.endpoint"], "https://otel.example.com:4317")
  assert_eq(final_config["telemetry.batch.size"], "500")
  assert_eq(final_config["telemetry.batch.timeout_ms"], "5000")
  assert_eq(final_config["telemetry.compression"], "gzip")
  assert_eq(final_config["telemetry.retry.max_attempts"], "3")
  assert_eq(final_config["telemetry.retry.backoff_ms"], "1000")
  
  // Test template inheritance
  let web_service_template = {
    "name": "web-service-template",
    "description": "Web service specific configuration",
    "parent_template": "service-template",
    "config": {
      "server.port": "{{server_port}}",
      "server.host": "{{server_host}}",
      "server.ssl.enabled": "{{server_ssl_enabled}}",
      "server.max_connections": "{{server_max_connections}}",
      "web.cors.enabled": "true",
      "web.request.timeout_ms": "30000"
    },
    "variables": [
      "server_port",
      "server_host",
      "server_ssl_enabled",
      "server_max_connections"
    ]
  }
  
  azimuth::TemplateConfigurationManager::register_template(template_config, web_service_template)
  
  // Generate config using inherited template
  let web_service_variables = {
    "service_name": "api-gateway",
    "service_version": "1.5.0",
    "environment": "production",
    "telemetry_sample_rate": "0.1",
    "logging_level": "WARN",
    "server_port": "8080",
    "server_host": "0.0.0.0",
    "server_ssl_enabled": "true",
    "server_max_connections": "1000"
  }
  
  let web_config_result = azimuth::TemplateConfigurationManager::generate_from_template(
    template_config,
    "web-service-template",
    web_service_variables
  )
  
  assert_true(web_config_result.success)
  
  let web_config = web_config_result.config
  
  // Verify inherited values from parent template
  assert_eq(web_config["service.name"], "api-gateway")
  assert_eq(web_config["service.version"], "1.5.0")
  assert_eq(web_config["telemetry.enabled"], "true")
  assert_eq(web_config["health.check.enabled"], "true")
  
  // Verify values from child template
  assert_eq(web_config["server.port"], "8080")
  assert_eq(web_config["server.host"], "0.0.0.0")
  assert_eq(web_config["server.ssl.enabled"], "true")
  assert_eq(web_config["server.max_connections"], "1000")
  assert_eq(web_config["web.cors.enabled"], "true")
  
  // Test template validation
  let invalid_variables = {
    "service_name": "invalid-service",
    // Missing required variables
  }
  
  let invalid_result = azimuth::TemplateConfigurationManager::generate_from_template(
    template_config,
    "service-template",
    invalid_variables
  )
  
  assert_false(invalid_result.success)
  assert_true(invalid_result.validation_errors.length() > 0)
  
  // Test template composition
  let composed_templates = ["service-template", "database-template"]
  let composition_variables = {
    "service_name": "composed-service",
    "service_version": "1.0.0",
    "environment": "staging",
    "telemetry_sample_rate": "0.5",
    "logging_level": "DEBUG",
    "db_host": "staging-db.example.com",
    "db_port": "5432",
    "db_name": "composedb",
    "db_pool_size": "10",
    "db_ssl_enabled": "false"
  }
  
  let composition_result = azimuth::TemplateConfigurationManager::compose_templates(
    template_config,
    composed_templates,
    composition_variables
  )
  
  assert_true(composition_result.success)
  
  let composed_config = composition_result.config
  
  // Verify values from both templates
  assert_eq(composed_config["service.name"], "composed-service")
  assert_eq(composed_config["database.host"], "staging-db.example.com")
  assert_eq(composed_config["telemetry.enabled"], "true")
  assert_eq(composed_config["database.timeout_ms"], "5000")
}

// Test 6: Configuration Validation and Schema Enforcement
test "configuration validation and schema enforcement" {
  // Create schema-based configuration manager
  let schema_config = azimuth::SchemaConfigurationManager::new()
  
  // Define configuration schema
  let service_schema = {
    "type": "object",
    "properties": {
      "service.name": {
        "type": "string",
        "minLength": 1,
        "maxLength": 100,
        "pattern": "^[a-z0-9-]+$"
      },
      "service.version": {
        "type": "string",
        "pattern": "^\\d+\\.\\d+\\.\\d+$"
      },
      "service.environment": {
        "type": "string",
        "enum": ["development", "staging", "production"]
      },
      "telemetry.enabled": {
        "type": "boolean"
      },
      "telemetry.sample_rate": {
        "type": "number",
        "minimum": 0.0,
        "maximum": 1.0
      },
      "telemetry.export_interval_ms": {
        "type": "integer",
        "minimum": 1000,
        "maximum": 300000
      },
      "telemetry.max_batch_size": {
        "type": "integer",
        "minimum": 1,
        "maximum": 10000
      },
      "logging.level": {
        "type": "string",
        "enum": ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
      }
    },
    "required": ["service.name", "service.version", "service.environment"],
    "additionalProperties": false
  }
  
  // Register schema
  azimuth::SchemaConfigurationManager::register_schema(schema_config, "service-schema", service_schema)
  
  // Test valid configuration
  let valid_config = {
    "service.name": "telemetry-service",
    "service.version": "1.2.3",
    "service.environment": "production",
    "telemetry.enabled": true,
    "telemetry.sample_rate": 0.1,
    "telemetry.export_interval_ms": 5000,
    "telemetry.max_batch_size": 100,
    "logging.level": "INFO"
  }
  
  let valid_result = azimuth::SchemaConfigurationManager::validate_and_load(
    schema_config, 
    "service-schema", 
    valid_config
  )
  
  assert_true(valid_result.success)
  assert_eq(valid_result.validation_errors.length(), 0)
  
  // Test invalid configurations
  let invalid_configs = [
    // Missing required fields
    {
      "service.version": "1.2.3",
      "service.environment": "production"
    },
    // Invalid service name format
    {
      "service.name": "Invalid Service Name",
      "service.version": "1.2.3",
      "service.environment": "production"
    },
    // Invalid version format
    {
      "service.name": "valid-service",
      "service.version": "1.2",
      "service.environment": "production"
    },
    // Invalid environment
    {
      "service.name": "valid-service",
      "service.version": "1.2.3",
      "service.environment": "testing"
    },
    // Sample rate out of range
    {
      "service.name": "valid-service",
      "service.version": "1.2.3",
      "service.environment": "production",
      "telemetry.sample_rate": 1.5
    },
    // Export interval out of range
    {
      "service.name": "valid-service",
      "service.version": "1.2.3",
      "service.environment": "production",
      "telemetry.export_interval_ms": 500
    },
    // Additional properties not allowed
    {
      "service.name": "valid-service",
      "service.version": "1.2.3",
      "service.environment": "production",
      "invalid.property": "should-not-exist"
    }
  ]
  
  for invalid_config in invalid_configs {
    let invalid_result = azimuth::SchemaConfigurationManager::validate_and_load(
      schema_config, 
      "service-schema", 
      invalid_config
    )
    
    assert_false(invalid_result.success)
    assert_true(invalid_result.validation_errors.length() > 0)
  }
  
  // Test partial validation (allow additional properties)
  let permissive_schema = {
    "type": "object",
    "properties": {
      "service.name": {
        "type": "string",
        "minLength": 1
      },
      "service.version": {
        "type": "string"
      }
    },
    "required": ["service.name", "service.version"],
    "additionalProperties": true
  }
  
  azimuth::SchemaConfigurationManager::register_schema(schema_config, "permissive-schema", permissive_schema)
  
  let permissive_config = {
    "service.name": "permissive-service",
    "service.version": "1.0.0",
    "extra.property": "allowed",
    "another.extra": "also-allowed"
  }
  
  let permissive_result = azimuth::SchemaConfigurationManager::validate_and_load(
    schema_config, 
    "permissive-schema", 
    permissive_config
  )
  
  assert_true(permissive_result.success)
  
  // Test conditional validation
  let conditional_schema = {
    "type": "object",
    "properties": {
      "database.type": {
        "type": "string",
        "enum": ["postgresql", "mysql", "mongodb"]
      },
      "database.host": {
        "type": "string",
        "minLength": 1
      },
      "database.port": {
        "type": "integer",
        "minimum": 1,
        "maximum": 65535
      },
      "database.ssl.enabled": {
        "type": "boolean"
      },
      "database.ssl.cert_path": {
        "type": "string",
        "minLength": 1
      }
    },
    "required": ["database.type", "database.host", "database.port"],
    "allOf": [
      {
        "if": {
          "properties": {
            "database.ssl.enabled": {
              "const": true
            }
          },
          "required": ["database.ssl.enabled"]
        },
        "then": {
          "required": ["database.ssl.cert_path"]
        }
      }
    ]
  }
  
  azimuth::SchemaConfigurationManager::register_schema(schema_config, "conditional-schema", conditional_schema)
  
  // Valid config without SSL
  let no_ssl_config = {
    "database.type": "postgresql",
    "database.host": "db.example.com",
    "database.port": 5432,
    "database.ssl.enabled": false
  }
  
  let no_ssl_result = azimuth::SchemaConfigurationManager::validate_and_load(
    schema_config, 
    "conditional-schema", 
    no_ssl_config
  )
  
  assert_true(no_ssl_result.success)
  
  // Valid config with SSL
  let ssl_config = {
    "database.type": "postgresql",
    "database.host": "db.example.com",
    "database.port": 5432,
    "database.ssl.enabled": true,
    "database.ssl.cert_path": "/path/to/cert.pem"
  }
  
  let ssl_result = azimuth::SchemaConfigurationManager::validate_and_load(
    schema_config, 
    "conditional-schema", 
    ssl_config
  )
  
  assert_true(ssl_result.success)
  
  // Invalid config with SSL but no cert path
  let invalid_ssl_config = {
    "database.type": "postgresql",
    "database.host": "db.example.com",
    "database.port": 5432,
    "database.ssl.enabled": true
    // Missing database.ssl.cert_path
  }
  
  let invalid_ssl_result = azimuth::SchemaConfigurationManager::validate_and_load(
    schema_config, 
    "conditional-schema", 
    invalid_ssl_config
  )
  
  assert_false(invalid_ssl_result.success)
  assert_true(invalid_ssl_result.validation_errors.any(|e| e.contains("required")))
  
  // Test schema evolution
  let v1_schema = {
    "type": "object",
    "properties": {
      "service.name": {"type": "string"},
      "service.version": {"type": "string"}
    },
    "required": ["service.name", "service.version"]
  }
  
  let v2_schema = {
    "type": "object",
    "properties": {
      "service.name": {"type": "string"},
      "service.version": {"type": "string"},
      "service.description": {"type": "string"} // New optional field
    },
    "required": ["service.name", "service.version"]
  }
  
  azimuth::SchemaConfigurationManager::register_schema(schema_config, "v1-schema", v1_schema)
  azimuth::SchemaConfigurationManager::register_schema(schema_config, "v2-schema", v2_schema)
  
  // Config valid for both v1 and v2
  let compatible_config = {
    "service.name": "compatible-service",
    "service.version": "1.0.0"
  }
  
  let v1_validation = azimuth::SchemaConfigurationManager::validate(schema_config, "v1-schema", compatible_config)
  let v2_validation = azimuth::SchemaConfigurationManager::validate(schema_config, "v2-schema", compatible_config)
  
  assert_true(v1_validation.success)
  assert_true(v2_validation.success)
  
  // Config valid only for v2
  let v2_only_config = {
    "service.name": "v2-service",
    "service.version": "2.0.0",
    "service.description": "A service using v2 schema features"
  }
  
  let v2_only_validation = azimuth::SchemaConfigurationManager::validate(schema_config, "v2-schema", v2_only_config)
  assert_true(v2_only_validation.success)
}

// Test 7: Configuration Hot Reload and Notification
test "configuration hot reload and notification system" {
  // Create hot-reload configuration manager
  let hot_reload_config = azimuth::HotReloadConfigurationManager::new()
  
  // Configure hot reload settings
  azimuth::HotReloadConfigurationManager::configure(hot_reload_config, {
    "file_watching": true,
    "reload_delay_ms": 1000,
    "validation_on_reload": true,
    "rollback_on_error": true,
    "notification_enabled": true
  })
  
  // Initialize configuration
  let initial_config = {
    "service.name": "hot-reload-service",
    "service.version": "1.0.0",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.1",
    "telemetry.export_interval_ms": "5000",
    "logging.level": "INFO",
    "feature.flags": {
      "new_feature": "false",
      "experimental_api": "false"
    }
  }
  
  azimuth::HotReloadConfigurationManager::load_config(hot_reload_config, initial_config)
  
  // Create telemetry processor that listens for configuration changes
  let processor = azimuth::TelemetryProcessor::new()
  azimuth::TelemetryProcessor::configure(processor, hot_reload_config)
  
  // Register configuration change listeners
  let notifications_received = []
  
  let telemetry_listener = azimuth::HotReloadConfigurationManager::add_listener(
    hot_reload_config,
    "telemetry",
    |changes| {
      for change in changes {
        if change.key.starts_with("telemetry.") {
          notifications_received.push({
            "type": "telemetry",
            "key": change.key,
            "old_value": change.old_value,
            "new_value": change.new_value
          })
        }
      }
    }
  )
  
  let feature_flag_listener = azimuth::HotReloadConfigurationManager::add_listener(
    hot_reload_config,
    "feature_flags",
    |changes| {
      for change in changes {
        if change.key.starts_with("feature.flags.") {
          notifications_received.push({
            "type": "feature_flag",
            "key": change.key,
            "old_value": change.old_value,
            "new_value": change.new_value
          })
        }
      }
    }
  )
  
  // Test hot reload with valid configuration
  let updated_config = {
    "telemetry.sample_rate": "0.2", // Changed
    "telemetry.export_interval_ms": "10000", // Changed
    "logging.level": "DEBUG", // Changed
    "feature.flags": {
      "new_feature": "true", // Changed
      "experimental_api": "false" // Unchanged
    }
  }
  
  let reload_result = azimuth::HotReloadConfigurationManager::reload_config(
    hot_reload_config, 
    updated_config
  )
  
  assert_true(reload_result.success)
  assert_true(reload_result.changes_applied > 0)
  
  // Verify notifications were sent
  assert_true(notifications_received.length() > 0)
  
  let telemetry_notifications = notifications_received.filter(|n| n["type"] == "telemetry")
  let feature_flag_notifications = notifications_received.filter(|n| n["type"] == "feature_flag")
  
  assert_true(telemetry_notifications.length() >= 3) // sample_rate, export_interval_ms, logging.level
  assert_true(feature_flag_notifications.length() >= 1) // new_feature
  
  // Verify specific notifications
  let sample_rate_notification = telemetry_notifications.find(|n| n["key"] == "telemetry.sample_rate")
  assert_true(sample_rate_notification.is_some)
  assert_eq(sample_rate_notification.unwrap["old_value"], "0.1")
  assert_eq(sample_rate_notification.unwrap["new_value"], "0.2")
  
  let new_feature_notification = feature_flag_notifications.find(|n| n["key"] == "feature.flags.new_feature")
  assert_true(new_feature_notification.is_some)
  assert_eq(new_feature_notification.unwrap["old_value"], "false")
  assert_eq(new_feature_notification.unwrap["new_value"], "true")
  
  // Verify processor was updated
  let updated_processor_config = azimuth::TelemetryProcessor::get_config(processor)
  assert_eq(updated_processor_config.sample_rate, 0.2)
  assert_eq(updated_processor_config.export_interval_ms, 10000)
  
  // Test hot reload with invalid configuration (should rollback)
  let invalid_config = {
    "telemetry.sample_rate": "1.5", // Invalid: > 1.0
    "telemetry.export_interval_ms": "-1000" // Invalid: negative
  }
  
  let invalid_reload_result = azimuth::HotReloadConfigurationManager::reload_config(
    hot_reload_config, 
    invalid_config
  )
  
  assert_false(invalid_reload_result.success)
  assert_true(invalid_reload_result.rollback_triggered)
  
  // Verify configuration was rolled back
  let rollback_config = azimuth::HotReloadConfigurationManager::get_current_config(hot_reload_config)
  assert_eq(rollback_config["telemetry.sample_rate"], "0.2") // Should be previous valid value
  assert_eq(rollback_config["telemetry.export_interval_ms"], "10000") // Should be previous valid value
  
  // Test file-based hot reload
  let config_file = "/tmp/telemetry-config.json"
  azimuth::HotReloadConfigurationManager::save_to_file(hot_reload_config, config_file)
  
  // Start file watching
  let watch_result = azimuth::HotReloadConfigurationManager::start_file_watching(hot_reload_config, config_file)
  assert_true(watch_result.success)
  
  // Simulate file change
  let file_updated_config = {
    "service.name": "hot-reload-service",
    "service.version": "1.0.0",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.3", // Changed
    "telemetry.export_interval_ms": "15000", // Changed
    "logging.level": "WARN", // Changed
    "feature.flags": {
      "new_feature": "true",
      "experimental_api": "true" // Changed
    }
  }
  
  azimuth::HotReloadConfigurationManager::save_to_file(hot_reload_config, config_file, file_updated_config)
  
  // Wait for file watcher to detect changes
  azimuth::Thread::sleep(2000) // 2 seconds
  
  // Verify configuration was updated
  let file_updated_current_config = azimuth::HotReloadConfigurationManager::get_current_config(hot_reload_config)
  assert_eq(file_updated_current_config["telemetry.sample_rate"], "0.3")
  assert_eq(file_updated_current_config["telemetry.export_interval_ms"], "15000")
  assert_eq(file_updated_current_config["logging.level"], "WARN")
  
  // Test configuration change history
  let change_history = azimuth::HotReloadConfigurationManager::get_change_history(hot_reload_config)
  assert_true(change_history.length() > 0)
  
  // Verify history contains recent changes
  let recent_changes = change_history.filter(|change| change.timestamp > azimuth::Time::now() - 10000) // Last 10 seconds
  assert_true(recent_changes.length() > 0)
  
  // Test selective listener removal
  azimuth::HotReloadConfigurationManager::remove_listener(hot_reload_config, telemetry_listener)
  
  // Clear notifications
  notifications_received.clear()
  
  // Make another configuration change
  let selective_change_config = {
    "telemetry.sample_rate": "0.4",
    "feature.flags": {
      "new_feature": "false",
      "experimental_api": "false"
    }
  }
  
  azimuth::HotReloadConfigurationManager::reload_config(hot_reload_config, selective_change_config)
  
  // Should only receive feature flag notifications (telemetry listener was removed)
  assert_true(notifications_received.length() > 0)
  
  let selective_notifications = notifications_received.filter(|n| n["type"] == "telemetry")
  assert_eq(selective_notifications.length(), 0) // No telemetry notifications
  
  let selective_feature_notifications = notifications_received.filter(|n| n["type"] == "feature_flag")
  assert_true(selective_feature_notifications.length() > 0) // Should have feature flag notifications
  
  // Stop file watching
  azimuth::HotReloadConfigurationManager::stop_file_watching(hot_reload_config)
}

// Test 8: Configuration Import/Export and Migration
test "configuration import/export and migration capabilities" {
  // Create migration-capable configuration manager
  let migration_config = azimuth::MigrationConfigurationManager::new()
  
  // Define configuration migration rules
  let migration_rules = [
    {
      "version": "1.0.0",
      "description": "Initial configuration format",
      "migrations": []
    },
    {
      "version": "1.1.0",
      "description": "Add telemetry batch configuration",
      "migrations": [
        {
          "type": "add_field",
          "path": "telemetry.batch.size",
          "default_value": "100"
        },
        {
          "type": "add_field", 
          "path": "telemetry.batch.timeout_ms",
          "default_value": "5000"
        }
      ]
    },
    {
      "version": "1.2.0",
      "description": "Restructure logging configuration",
      "migrations": [
        {
          "type": "rename_field",
          "old_path": "logging.level",
          "new_path": "logging.root_level"
        },
        {
          "type": "add_field",
          "path": "logging.appenders",
          "default_value": "console"
        }
      ]
    },
    {
      "version": "2.0.0",
      "description": "Major configuration restructure",
      "migrations": [
        {
          "type": "move_field",
          "old_path": "telemetry.enabled",
          "new_path": "features.telemetry"
        },
        {
          "type": "move_field",
          "old_path": "telemetry.sample_rate",
          "new_path": "telemetry.sampling.rate"
        },
        {
          "type": "remove_field",
          "path": "legacy.feature"
        }
      ]
    }
  ]
  
  // Register migration rules
  for rule in migration_rules {
    azimuth::MigrationConfigurationManager::register_migration_rule(migration_config, rule)
  }
  
  // Test configuration export
  let current_config = {
    "service.name": "migration-service",
    "service.version": "2.0.0",
    "features.telemetry": "true",
    "telemetry.sampling.rate": "0.1",
    "telemetry.batch.size": "200",
    "telemetry.batch.timeout_ms": "3000",
    "logging.root_level": "INFO",
    "logging.appenders": ["console", "file"],
    "database.host": "db.example.com",
    "database.port": "5432"
  }
  
  azimuth::MigrationConfigurationManager::load_config(migration_config, current_config)
  
  // Export to different formats
  let json_export = azimuth::MigrationConfigurationManager::export_config(
    migration_config, 
    "json"
  )
  
  assert_true(json_export.success)
  assert_true(json_export.data.length() > 0)
  
  let yaml_export = azimuth::MigrationConfigurationManager::export_config(
    migration_config, 
    "yaml"
  )
  
  assert_true(yaml_export.success)
  assert_true(yaml_export.data.length() > 0)
  
  let toml_export = azimuth::MigrationConfigurationManager::export_config(
    migration_config, 
    "toml"
  )
  
  assert_true(toml_export.success)
  assert_true(toml_export.data.length() > 0)
  
  // Test configuration import
  let json_import_result = azimuth::MigrationConfigurationManager::import_config(
    migration_config, 
    json_export.data,
    "json"
  )
  
  assert_true(json_import_result.success)
  assert_eq(json_import_result.imported_config["service.name"], "migration-service")
  assert_eq(json_import_result.imported_config["features.telemetry"], "true")
  
  // Test configuration migration from older version
  let v1_0_config = {
    "service.name": "legacy-service",
    "service.version": "1.0.0",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.1",
    "logging.level": "INFO",
    "legacy.feature": "should_be_removed"
  }
  
  let migration_result = azimuth::MigrationConfigurationManager::migrate_config(
    migration_config, 
    v1_0_config, 
    "1.0.0", 
    "2.0.0"
  )
  
  assert_true(migration_result.success)
  assert_eq(migration_result.migrated_version, "2.0.0")
  assert_true(migration_result.applied_migrations.length() > 0)
  
  let migrated_config = migration_result.migrated_config
  
  // Verify migrations were applied correctly
  assert_true(migrated_config.contains("features.telemetry")) // Moved from telemetry.enabled
  assert_true(migrated_config.contains("telemetry.sampling.rate")) // Moved from telemetry.sample_rate
  assert_true(migrated_config.contains("telemetry.batch.size")) // Added in 1.1.0
  assert_true(migrated_config.contains("telemetry.batch.timeout_ms")) // Added in 1.1.0
  assert_true(migrated_config.contains("logging.root_level")) // Renamed from logging.level
  assert_true(migrated_config.contains("logging.appenders")) // Added in 1.2.0
  assert_false(migrated_config.contains("legacy.feature")) // Removed in 2.0.0
  
  // Test incremental migration
  let v1_1_config = {
    "service.name": "incremental-service",
    "service.version": "1.1.0",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.2",
    "telemetry.batch.size": "150",
    "logging.level": "DEBUG"
  }
  
  let incremental_result = azimuth::MigrationConfigurationManager::migrate_to_latest(
    migration_config, 
    v1_1_config
  )
  
  assert_true(incremental_result.success)
  assert_eq(incremental_result.migrated_version, "2.0.0")
  
  let incremental_migrated = incremental_result.migrated_config
  
  // Verify only necessary migrations were applied
  assert_true(incremental_migrated.contains("features.telemetry"))
  assert_true(incremental_migrated.contains("telemetry.sampling.rate"))
  assert_true(incremental_migrated.contains("logging.root_level"))
  assert_true(incremental_migrated.contains("logging.appenders"))
  
  // Test configuration validation during migration
  let invalid_v1_config = {
    "service.name": "", // Invalid: empty
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.1",
    "logging.level": "INFO"
  }
  
  let invalid_migration = azimuth::MigrationConfigurationManager::migrate_config(
    migration_config, 
    invalid_v1_config, 
    "1.0.0", 
    "2.0.0"
  )
  
  assert_false(invalid_migration.success)
  assert_true(invalid_migration.validation_errors.length() > 0)
  
  // Test configuration backup and restore
  let backup_result = azimuth::MigrationConfigurationManager::create_backup(
    migration_config, 
    "pre-migration-backup"
  )
  
  assert_true(backup_result.success)
  assert_true(backup_result.backup_id.length() > 0)
  
  // Make some changes
  let changes = {"service.name": "changed-service"}
  azimuth::MigrationConfigurationManager::update_config(migration_config, changes)
  
  // Verify changes were applied
  let changed_config = azimuth::MigrationConfigurationManager::get_current_config(migration_config)
  assert_eq(changed_config["service.name"], "changed-service")
  
  // Restore from backup
  let restore_result = azimuth::MigrationConfigurationManager::restore_from_backup(
    migration_config, 
    backup_result.backup_id
  )
  
  assert_true(restore_result.success)
  
  // Verify backup was restored
  let restored_config = azimuth::MigrationConfigurationManager::get_current_config(migration_config)
  assert_eq(restored_config["service.name"], "migration-service") // Should be back to original
  
  // Test configuration comparison
  let config_a = {
    "service.name": "service-a",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.1"
  }
  
  let config_b = {
    "service.name": "service-b", // Different
    "telemetry.enabled": "true", // Same
    "telemetry.sample_rate": "0.2" // Different
  }
  
  let comparison = azimuth::MigrationConfigurationManager::compare_configs(config_a, config_b)
  
  assert_true(comparison.has_differences)
  assert_true(comparison.added.contains("service.name")) // Different values
  assert_true(comparison.modified.contains("telemetry.sample_rate")) // Different values
  assert_false(comparison.modified.contains("telemetry.enabled")) // Same values
}

// Test 9: Multi-Service Configuration Management
test "multi-service configuration management" {
  // Create multi-service configuration manager
  let multi_service_config = azimuth::MultiServiceConfigurationManager::new()
  
  // Configure multi-service settings
  azimuth::MultiServiceConfigurationManager::configure(multi_service_config, {
    "centralized_config": true,
    "service_discovery": true,
    "shared_config": true,
    "service_specific_overrides": true,
    "config_propagation": true
  })
  
  // Define shared configuration
  let shared_config = {
    "telemetry.enabled": "true",
    "telemetry.exporter": "otlp",
    "telemetry.endpoint": "https://otel.example.com:4317",
    "telemetry.compression": "gzip",
    "telemetry.retry.max_attempts": "3",
    "logging.level": "INFO",
    "logging.format": "json",
    "security.tls.enabled": "true",
    "security.tls.version": "1.3"
  }
  
  azimuth::MultiServiceConfigurationManager::set_shared_config(multi_service_config, shared_config)
  
  // Register services
  let services = ["user-service", "order-service", "payment-service", "notification-service"]
  
  for service in services {
    azimuth::MultiServiceConfigurationManager::register_service(multi_service_config, service)
  }
  
  // Define service-specific configurations
  let user_service_config = {
    "service.name": "user-service",
    "service.port": "8080",
    "telemetry.sample_rate": "0.1",
    "database.host": "user-db.example.com",
    "database.name": "userdb",
    "cache.enabled": "true",
    "cache.ttl_seconds": "300"
  }
  
  let order_service_config = {
    "service.name": "order-service",
    "service.port": "8081",
    "telemetry.sample_rate": "0.5", // Higher sampling for order service
    "database.host": "order-db.example.com",
    "database.name": "orderdb",
    "inventory.check.enabled": "true",
    "inventory.check.timeout_ms": "5000"
  }
  
  let payment_service_config = {
    "service.name": "payment-service",
    "service.port": "8082",
    "telemetry.sample_rate": "1.0", // Sample all payment transactions
    "database.host": "payment-db.example.com",
    "database.name": "paymentdb",
    "security.encryption.enabled": "true",
    "security.encryption.key_rotation_days": "30"
  }
  
  let notification_service_config = {
    "service.name": "notification-service",
    "service.port": "8083",
    "telemetry.sample_rate": "0.05", // Lower sampling for notifications
    "providers.email.enabled": "true",
    "providers.sms.enabled": "true",
    "providers.push.enabled": "false"
  }
  
  // Set service-specific configurations
  azimuth::MultiServiceConfigurationManager::set_service_config(
    multi_service_config, 
    "user-service", 
    user_service_config
  )
  
  azimuth::MultiServiceConfigurationManager::set_service_config(
    multi_service_config, 
    "order-service", 
    order_service_config
  )
  
  azimuth::MultiServiceConfigurationManager::set_service_config(
    multi_service_config, 
    "payment-service", 
    payment_service_config
  )
  
  azimuth::MultiServiceConfigurationManager::set_service_config(
    multi_service_config, 
    "notification-service", 
    notification_service_config
  )
  
  // Get effective configuration for each service
  for service in services {
    let effective_config = azimuth::MultiServiceConfigurationManager::get_effective_config(
      multi_service_config, 
      service
    )
    
    // Verify shared config is included
    assert_eq(effective_config["telemetry.enabled"], "true")
    assert_eq(effective_config["telemetry.exporter"], "otlp")
    assert_eq(effective_config["logging.level"], "INFO")
    assert_eq(effective_config["security.tls.enabled"], "true")
    
    // Verify service-specific overrides are applied
    assert_eq(effective_config["service.name"], service)
    
    match service {
      "user-service" => {
        assert_eq(effective_config["service.port"], "8080")
        assert_eq(effective_config["telemetry.sample_rate"], "0.1")
        assert_eq(effective_config["database.host"], "user-db.example.com")
        assert_eq(effective_config["cache.enabled"], "true")
      }
      "order-service" => {
        assert_eq(effective_config["service.port"], "8081")
        assert_eq(effective_config["telemetry.sample_rate"], "0.5")
        assert_eq(effective_config["database.host"], "order-db.example.com")
        assert_eq(effective_config["inventory.check.enabled"], "true")
      }
      "payment-service" => {
        assert_eq(effective_config["service.port"], "8082")
        assert_eq(effective_config["telemetry.sample_rate"], "1.0")
        assert_eq(effective_config["database.host"], "payment-db.example.com")
        assert_eq(effective_config["security.encryption.enabled"], "true")
      }
      "notification-service" => {
        assert_eq(effective_config["service.port"], "8083")
        assert_eq(effective_config["telemetry.sample_rate"], "0.05")
        assert_eq(effective_config["providers.email.enabled"], "true")
        assert_eq(effective_config["providers.sms.enabled"], "true")
      }
      _ => {}
    }
  }
  
  // Test shared configuration updates
  let shared_updates = {
    "telemetry.compression": "lz4", // Changed from gzip
    "logging.level": "WARN", // Changed from INFO
    "new.shared.setting": "shared-value"
  }
  
  let shared_update_result = azimuth::MultiServiceConfigurationManager::update_shared_config(
    multi_service_config, 
    shared_updates
  )
  
  assert_true(shared_update_result.success)
  assert_true(shared_update_result.affected_services.contains("user-service"))
  assert_true(shared_update_result.affected_services.contains("order-service"))
  assert_true(shared_update_result.affected_services.contains("payment-service"))
  assert_true(shared_update_result.affected_services.contains("notification-service"))
  
  // Verify updates were propagated to all services
  for service in services {
    let updated_config = azimuth::MultiServiceConfigurationManager::get_effective_config(
      multi_service_config, 
      service
    )
    
    assert_eq(updated_config["telemetry.compression"], "lz4")
    assert_eq(updated_config["logging.level"], "WARN")
    assert_eq(updated_config["new.shared.setting"], "shared-value")
  }
  
  // Test service-specific configuration updates
  let user_service_updates = {
    "service.port": "9080", // Changed from 8080
    "cache.ttl_seconds": "600" // Changed from 300
  }
  
  let service_update_result = azimuth::MultiServiceConfigurationManager::update_service_config(
    multi_service_config, 
    "user-service", 
    user_service_updates
  )
  
  assert_true(service_update_result.success)
  
  // Verify only user-service was affected
  let updated_user_config = azimuth::MultiServiceConfigurationManager::get_effective_config(
    multi_service_config, 
    "user-service"
  )
  
  let unchanged_order_config = azimuth::MultiServiceConfigurationManager::get_effective_config(
    multi_service_config, 
    "order-service"
  )
  
  assert_eq(updated_user_config["service.port"], "9080")
  assert_eq(updated_user_config["cache.ttl_seconds"], "600")
  
  assert_eq(unchanged_order_config["service.port"], "8081") // Should be unchanged
  assert_true(!unchanged_order_config.contains("cache.ttl_seconds")) // Should not exist
  
  // Test configuration dependencies between services
  azimuth::MultiServiceConfigurationManager::add_dependency(
    multi_service_config, 
    "order-service", 
    "user-service"
  )
  
  azimuth::MultiServiceConfigurationManager::add_dependency(
    multi_service_config, 
    "payment-service", 
    "order-service"
  )
  
  // Create configuration templates for service types
  let web_service_template = {
    "service.type": "web",
    "health.check.enabled": "true",
    "health.check.path": "/health",
    "metrics.enabled": "true",
    "metrics.path": "/metrics"
  }
  
  let database_service_template = {
    "service.type": "database",
    "backup.enabled": "true",
    "backup.schedule": "0 2 * * *",
    "monitoring.enabled": "true"
  }
  
  azimuth::MultiServiceConfigurationManager::register_service_template(
    multi_service_config, 
    "web-service", 
    web_service_template
  )
  
  azimuth::MultiServiceConfigurationManager::register_service_template(
    multi_service_config, 
    "database-service", 
    database_service_template
  )
  
  // Apply templates to services
  azimuth::MultiServiceConfigurationManager::apply_template(
    multi_service_config, 
    "user-service", 
    "web-service"
  )
  
  azimuth::MultiServiceConfigurationManager::apply_template(
    multi_service_config, 
    "order-service", 
    "web-service"
  )
  
  // Verify template values were applied
  let templated_user_config = azimuth::MultiServiceConfigurationManager::get_effective_config(
    multi_service_config, 
    "user-service"
  )
  
  assert_eq(templated_user_config["service.type"], "web")
  assert_eq(templated_user_config["health.check.enabled"], "true")
  assert_eq(templated_user_config["health.check.path"], "/health")
  assert_eq(templated_user_config["metrics.enabled"], "true")
  assert_eq(templated_user_config["metrics.path"], "/metrics")
  
  // Test configuration validation across services
  let cross_service_validation = azimuth::MultiServiceConfigurationManager::validate_cross_service_config(
    multi_service_config
  )
  
  assert_true(cross_service_validation.valid)
  
  // Test configuration export for all services
  let multi_service_export = azimuth::MultiServiceConfigurationManager::export_all_configs(
    multi_service_config, 
    "yaml"
  )
  
  assert_true(multi_service_export.success)
  assert_true(multi_service_export.data.length() > 0)
  
  // Verify export contains all services
  for service in services {
    assert_true(multi_service_export.data.contains(service))
  }
}

// Test 10: Configuration Performance and Scalability
test "configuration performance and scalability" {
  // Create performance-optimized configuration manager
  let perf_config = azimuth::PerformanceConfigurationManager::new()
  
  // Configure for performance
  azimuth::PerformanceConfigurationManager::configure_for_performance(perf_config, {
    "caching": true,
    "cache_size": 1000,
    "lazy_loading": true,
    "async_updates": true,
    "batch_operations": true,
    "compression": true
  })
  
  // Load large configuration
  let large_config = {}
  
  // Generate configuration with many keys
  for i = 0; i < 10000; i = i + 1 {
    let key = "config.key." + i.to_string()
    let value = "value-" + i.to_string()
    large_config[key] = value
  }
  
  // Add some nested configuration
  large_config["nested.config"] = {}
  for i = 0; i < 1000; i = i + 1 {
    let key = "subkey." + i.to_string()
    let value = "nested-value-" + i.to_string()
    large_config["nested.config"][key] = value
  }
  
  // Measure load performance
  let load_start = azimuth::Time::now()
  
  let load_result = azimuth::PerformanceConfigurationManager::load_config(perf_config, large_config)
  
  let load_end = azimuth::Time::now()
  let load_time_ms = load_end - load_start
  
  assert_true(load_result.success)
  assert_true(load_time_ms < 1000) // Should load in under 1 second
  
  // Measure read performance
  let read_start = azimuth::Time::now()
  
  for i = 0; i < 1000; i = i + 1 {
    let key = "config.key." + i.to_string()
    let value = azimuth::PerformanceConfigurationManager::get(perf_config, key)
    assert_true(value.is_some)
  }
  
  let read_end = azimuth::Time::now()
  let read_time_ms = read_end - read_start
  
  assert_true(read_time_ms < 100) // Should read 1000 values in under 100ms
  
  // Test bulk read performance
  let keys_to_read = []
  for i = 0; i < 500; i = i + 1 {
    keys_to_read.push("config.key." + i.to_string())
  }
  
  let bulk_read_start = azimuth::Time::now()
  
  let bulk_values = azimuth::PerformanceConfigurationManager::get_bulk(perf_config, keys_to_read)
  
  let bulk_read_end = azimuth::Time::now()
  let bulk_read_time_ms = bulk_read_end - bulk_read_start
  
  assert_eq(bulk_values.length(), 500)
  assert_true(bulk_read_time_ms < 50) // Should be faster than individual reads
  
  // Test write performance
  let write_updates = {}
  for i = 10000; i < 11000; i = i + 1 {
    let key = "config.key." + i.to_string()
    let value = "updated-value-" + i.to_string()
    write_updates[key] = value
  }
  
  let write_start = azimuth::Time::now()
  
  let write_result = azimuth::PerformanceConfigurationManager::update_config_bulk(perf_config, write_updates)
  
  let write_end = azimuth::Time::now()
  let write_time_ms = write_end - write_start
  
  assert_true(write_result.success)
  assert_true(write_time_ms < 500) // Should write 1000 values in under 500ms
  
  // Test cache performance
  let cache_stats_before = azimuth::PerformanceConfigurationManager::get_cache_stats(perf_config)
  
  // Read same keys multiple times to test cache
  for i = 0; i < 100; i = i + 1 {
    for j = 0; j < 10; j = j + 1 {
      let key = "config.key." + i.to_string()
      azimuth::PerformanceConfigurationManager::get(perf_config, key)
    }
  }
  
  let cache_stats_after = azimuth::PerformanceConfigurationManager::get_cache_stats(perf_config)
  
  assert_true(cache_stats_after.hits > cache_stats_before.hits)
  assert_true(cache_stats_after.hit_ratio > cache_stats_before.hit_ratio)
  
  // Test configuration watch performance
  let watch_results = []
  
  // Add many watchers
  for i = 0; i < 100; i = i + 1 {
    let watcher_id = azimuth::PerformanceConfigurationManager::add_watcher(
      perf_config, 
      "config.key." + i.to_string(),
      |key, old_value, new_value| {
        watch_results.push((key, old_value, new_value))
      }
    )
    assert_true(watcher_id.length() > 0)
  }
  
  // Trigger watched changes
  let trigger_updates = {}
  for i = 0; i < 100; i = i + 1 {
    let key = "config.key." + i.to_string()
    let value = "triggered-value-" + i.to_string()
    trigger_updates[key] = value
  }
  
  let trigger_start = azimuth::Time::now()
  
  azimuth::PerformanceConfigurationManager::update_config_bulk(perf_config, trigger_updates)
  
  // Wait for async notifications
  azimuth::Thread::sleep(100)
  
  let trigger_end = azimuth::Time::now()
  let trigger_time_ms = trigger_end - trigger_start
  
  assert_true(watch_results.length() == 100) // All watchers should be notified
  assert_true(trigger_time_ms < 200) // Should notify 100 watchers quickly
  
  // Test concurrent access performance
  let concurrent_threads = []
  let operations_per_thread = 100
  
  for i = 0; i < 10; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j in 0..operations_per_thread {
        let key = "config.key." + ((i * operations_per_thread + j) % 1000).to_string()
        let value = "concurrent-value-" + (i * operations_per_thread + j).to_string()
        
        azimuth::PerformanceConfigurationManager::set(perf_config, key, value)
        
        let retrieved = azimuth::PerformanceConfigurationManager::get(perf_config, key)
        assert_true(retrieved.is_some)
      }
    })
    concurrent_threads.push(thread)
  }
  
  let concurrent_start = azimuth::Time::now()
  
  for thread in concurrent_threads {
    azimuth::Thread::join(thread)
  }
  
  let concurrent_end = azimuth::Time::now()
  let concurrent_time_ms = concurrent_end - concurrent_start
  
  assert_true(concurrent_time_ms < 1000) // Should handle 1000 concurrent operations quickly
  
  // Test memory usage
  let memory_before = azimuth::PerformanceConfigurationManager::get_memory_usage(perf_config)
  
  // Add more configuration data
  let additional_config = {}
  for i in 0..5000 {
    let key = "additional.key." + i.to_string()
    let value = "x".repeat(100) // Larger values
    additional_config[key] = value
  }
  
  azimuth::PerformanceConfigurationManager::update_config_bulk(perf_config, additional_config)
  
  let memory_after = azimuth::PerformanceConfigurationManager::get_memory_usage(perf_config)
  
  let memory_increase = memory_after - memory_before
  let expected_increase = 5000 * 100 // 5000 keys * 100 bytes each
  
  // Memory usage should be reasonable (within 2x expected)
  assert_true(memory_increase <= expected_increase * 2)
  
  // Test configuration serialization performance
  let current_config = azimuth::PerformanceConfigurationManager::get_all(perf_config)
  
  let serialize_start = azimuth::Time::now()
  
  let json_serialized = azimuth::PerformanceConfigurationManager::serialize_config(
    perf_config, 
    "json"
  )
  
  let serialize_end = azimuth::Time::now()
  let serialize_time_ms = serialize_end - serialize_start
  
  assert_true(json_serialized.success)
  assert_true(serialize_time_ms < 500) // Should serialize quickly even with large config
  
  // Test configuration compression
  let compress_start = azimuth::Time::now()
  
  let compressed = azimuth::PerformanceConfigurationManager::compress_config(
    perf_config, 
    current_config
  )
  
  let compress_end = azimuth::Time::now()
  let compress_time_ms = compress_end - compress_start
  
  assert_true(compressed.compressed)
  assert_true(compressed.compression_ratio > 1.5) // Should achieve reasonable compression
  assert_true(compress_time_ms < 200) // Should compress quickly
  
  // Get final performance statistics
  let final_stats = azimuth::PerformanceConfigurationManager::get_performance_stats(perf_config)
  
  assert_true(final_stats.total_keys > 15000) // Should have many keys
  assert_true(final_stats.cache_hit_ratio > 0.8) // Cache should be effective
  assert_true(final_stats.avg_read_time_us < 100) // Average read should be fast
  assert_true(final_stats.avg_write_time_us < 500) // Average write should be reasonable
}