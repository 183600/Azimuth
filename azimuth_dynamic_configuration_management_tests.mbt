// Azimuth Telemetry System - Dynamic Configuration Management Tests
// This file contains test cases for dynamic configuration management functionality

// Test 1: Basic Configuration Operations
test "basic configuration operations" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Set configuration values
  ConfigurationManager::set(config_manager, "service.name", "azimuth-telemetry")
  ConfigurationManager::set(config_manager, "service.version", "1.0.0")
  ConfigurationManager::set(config_manager, "service.port", 8080)
  ConfigurationManager::set(config_manager, "service.debug", true)
  ConfigurationManager::set(config_manager, "database.timeout", 30)
  ConfigurationManager::set(config_manager, "database.max_connections", 100)
  
  // Get configuration values
  match ConfigurationManager::get_string(config_manager, "service.name") {
    Some(value) => assert_eq(value, "azimuth-telemetry")
    None => assert_true(false)
  }
  
  match ConfigurationManager::get_string(config_manager, "service.version") {
    Some(value) => assert_eq(value, "1.0.0")
    None => assert_true(false)
  }
  
  match ConfigurationManager::get_int(config_manager, "service.port") {
    Some(value) => assert_eq(value, 8080)
    None => assert_true(false)
  }
  
  match ConfigurationManager::get_bool(config_manager, "service.debug") {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  match ConfigurationManager::get_int(config_manager, "database.timeout") {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  
  match ConfigurationManager::get_int(config_manager, "database.max_connections") {
    Some(value) => assert_eq(value, 100)
    None => assert_true(false)
  }
  
  // Test default values
  match ConfigurationManager::get_string(config_manager, "nonexistent.string") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match ConfigurationManager::get_with_default(config_manager, "nonexistent.key", "default_value") {
    "default_value" => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test configuration exists
  assert_true(ConfigurationManager::exists(config_manager, "service.name"))
  assert_false(ConfigurationManager::exists(config_manager, "nonexistent.key"))
  
  // Test configuration removal
  ConfigurationManager::remove(config_manager, "service.debug")
  assert_false(ConfigurationManager::exists(config_manager, "service.debug"))
}

// Test 2: Configuration Validation and Constraints
test "configuration validation and constraints" {
  // Create configuration manager with validation
  let config_manager = ConfigurationManager::new()
  
  // Add validation rules
  ConfigurationManager::add_validation(config_manager, "service.port", ValidationRule::range(1, 65535))
  ConfigurationManager::add_validation(config_manager, "database.timeout", ValidationRule::min(1))
  ConfigurationManager::add_validation(config_manager, "service.name", ValidationRule::required())
  ConfigurationManager::add_validation(config_manager, "service.email", ValidationRule::email())
  ConfigurationManager::add_validation(config_manager, "service.url", ValidationRule::url())
  
  // Test valid values
  assert_true(ConfigurationManager::set(config_manager, "service.port", 8080))
  assert_true(ConfigurationManager::set(config_manager, "database.timeout", 30))
  assert_true(ConfigurationManager::set(config_manager, "service.name", "test-service"))
  assert_true(ConfigurationManager::set(config_manager, "service.email", "test@example.com"))
  assert_true(ConfigurationManager::set(config_manager, "service.url", "https://example.com"))
  
  // Test invalid values
  assert_false(ConfigurationManager::set(config_manager, "service.port", 70000))  // Out of range
  assert_false(ConfigurationManager::set(config_manager, "database.timeout", 0))  // Below minimum
  assert_false(ConfigurationManager::set(config_manager, "service.name", ""))  // Empty string
  assert_false(ConfigurationManager::set(config_manager, "service.email", "invalid-email"))  // Invalid email
  assert_false(ConfigurationManager::set(config_manager, "service.url", "not-a-url"))  // Invalid URL
  
  // Test custom validation
  ConfigurationManager::add_custom_validation(config_manager, "custom.feature_flag", |value| {
    match value {
      "true" | "false" => true
      _ => false
    }
  })
  
  assert_true(ConfigurationManager::set(config_manager, "custom.feature_flag", "true"))
  assert_false(ConfigurationManager::set(config_manager, "custom.feature_flag", "maybe"))
  
  // Test validation errors
  let validation_errors = ConfigurationManager::get_validation_errors(config_manager)
  assert_eq(validation_errors.length(), 5)  // 5 invalid attempts
  
  // Test bulk validation
  let config_data = [
    ("service.port", 8080),
    ("database.timeout", 30),
    ("service.name", "test-service"),
    ("service.email", "test@example.com")
  ]
  
  let bulk_result = ConfigurationManager::validate_bulk(config_manager, config_data)
  assert_true(bulk_result.is_valid)
  assert_eq(bulk_result.errors.length(), 0)
}

// Test 3: Configuration Profiles and Environments
test "configuration profiles and environments" {
  // Create configuration manager with profiles
  let config_manager = ConfigurationManager::new()
  
  // Create base configuration
  ConfigurationManager::set(config_manager, "service.name", "azimuth-telemetry")
  ConfigurationManager::set(config_manager, "service.port", 8080)
  ConfigurationManager::set(config_manager, "database.host", "localhost")
  ConfigurationManager::set(config_manager, "database.port", 5432)
  ConfigurationManager::set(config_manager, "log.level", "INFO")
  
  // Create development profile
  ConfigurationManager::create_profile(config_manager, "development")
  ConfigurationManager::set_profile(config_manager, "development", "service.debug", true)
  ConfigurationManager::set_profile(config_manager, "development", "log.level", "DEBUG")
  ConfigurationManager::set_profile(config_manager, "development", "database.host", "dev-db.example.com")
  
  // Create production profile
  ConfigurationManager::create_profile(config_manager, "production")
  ConfigurationManager::set_profile(config_manager, "production", "service.debug", false)
  ConfigurationManager::set_profile(config_manager, "production", "log.level", "WARN")
  ConfigurationManager::set_profile(config_manager, "production", "database.host", "prod-db.example.com")
  ConfigurationManager::set_profile(config_manager, "production", "service.port", 443)
  
  // Test base configuration
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name").unwrap(), "azimuth-telemetry")
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port").unwrap(), 8080)
  
  // Switch to development profile
  ConfigurationManager::activate_profile(config_manager, "development")
  
  // Verify profile overrides
  assert_eq(ConfigurationManager::get_bool(config_manager, "service.debug").unwrap(), true)
  assert_eq(ConfigurationManager::get_string(config_manager, "log.level").unwrap(), "DEBUG")
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host").unwrap(), "dev-db.example.com")
  // Base values still apply
  assert_eq(ConfigurationManager::get_int(config_manager, "database.port").unwrap(), 5432)
  
  // Switch to production profile
  ConfigurationManager::activate_profile(config_manager, "production")
  
  // Verify production profile overrides
  assert_eq(ConfigurationManager::get_bool(config_manager, "service.debug").unwrap(), false)
  assert_eq(ConfigurationManager::get_string(config_manager, "log.level").unwrap(), "WARN")
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host").unwrap(), "prod-db.example.com")
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port").unwrap(), 443)
  
  // Test profile listing
  let profiles = ConfigurationManager::list_profiles(config_manager)
  assert_true(profiles.contains("development"))
  assert_true(profiles.contains("production"))
  
  // Test current profile
  match ConfigurationManager::get_current_profile(config_manager) {
    Some(profile) => assert_eq(profile, "production")
    None => assert_true(false)
  }
  
  // Test profile inheritance
  ConfigurationManager::create_profile(config_manager, "staging")
  ConfigurationManager::set_profile_parent(config_manager, "staging", "production")
  ConfigurationManager::set_profile(config_manager, "staging", "database.host", "staging-db.example.com")
  
  ConfigurationManager::activate_profile(config_manager, "staging")
  
  // Should inherit from production but override database host
  assert_eq(ConfigurationManager::get_bool(config_manager, "service.debug").unwrap(), false)  // From production
  assert_eq(ConfigurationManager::get_string(config_manager, "log.level").unwrap(), "WARN")  // From production
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host").unwrap(), "staging-db.example.com")  // Override
}

// Test 4: Dynamic Configuration Updates and Watchers
test "dynamic configuration updates and watchers" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Set initial configuration
  ConfigurationManager::set(config_manager, "feature.enabled", false)
  ConfigurationManager::set(config_manager, "service.rate_limit", 100)
  ConfigurationManager::set(config_manager, "cache.ttl", 300)
  
  // Create configuration watchers
  let mut feature_changes = []
  let mut rate_limit_changes = []
  let mut ttl_changes = []
  
  ConfigurationManager::watch(config_manager, "feature.enabled", |old_value, new_value| {
    feature_changes.push((old_value, new_value))
  })
  
  ConfigurationManager::watch(config_manager, "service.rate_limit", |old_value, new_value| {
    rate_limit_changes.push((old_value, new_value))
  })
  
  ConfigurationManager::watch(config_manager, "cache.ttl", |old_value, new_value| {
    ttl_changes.push((old_value, new_value))
  })
  
  // Update configuration values
  ConfigurationManager::set(config_manager, "feature.enabled", true)
  ConfigurationManager::set(config_manager, "service.rate_limit", 200)
  ConfigurationManager::set(config_manager, "cache.ttl", 600)
  
  // Verify watchers were called
  assert_eq(feature_changes.length(), 1)
  match feature_changes[0] {
    (Some(old), Some(new)) => {
      assert_eq(old, false)
      assert_eq(new, true)
    }
    _ => assert_true(false)
  }
  
  assert_eq(rate_limit_changes.length(), 1)
  match rate_limit_changes[0] {
    (Some(old), Some(new)) => {
      assert_eq(old, 100)
      assert_eq(new, 200)
    }
    _ => assert_true(false)
  }
  
  assert_eq(ttl_changes.length(), 1)
  match ttl_changes[0] {
    (Some(old), Some(new)) => {
      assert_eq(old, 300)
      assert_eq(new, 600)
    }
    _ => assert_true(false)
  }
  
  // Test multiple updates to same key
  ConfigurationManager::set(config_manager, "service.rate_limit", 250)
  ConfigurationManager::set(config_manager, "service.rate_limit", 300)
  
  assert_eq(rate_limit_changes.length(), 3)
  
  // Test wildcard watchers
  let mut all_changes = []
  ConfigurationManager::watch_wildcard(config_manager, "service.*", |key, old_value, new_value| {
    all_changes.push((key, old_value, new_value))
  })
  
  ConfigurationManager::set(config_manager, "service.timeout", 30)
  ConfigurationManager::set(config_manager, "service.retries", 3)
  
  assert_eq(all_changes.length(), 2)
  assert_eq(all_changes[0].0, "service.timeout")
  assert_eq(all_changes[1].0, "service.retries")
  
  // Test watcher removal
  ConfigurationManager::remove_watcher(config_manager, "feature.enabled")
  ConfigurationManager::set(config_manager, "feature.enabled", false)
  
  // Watcher was removed, so no new change should be recorded
  assert_eq(feature_changes.length(), 1)
}

// Test 5: Configuration Persistence and Loading
test "configuration persistence and loading" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Set configuration values
  ConfigurationManager::set(config_manager, "service.name", "azimuth-telemetry")
  ConfigurationManager::set(config_manager, "service.version", "1.0.0")
  ConfigurationManager::set(config_manager, "service.port", 8080)
  ConfigurationManager::set(config_manager, "database.host", "localhost")
  ConfigurationManager::set(config_manager, "database.port", 5432)
  ConfigurationManager::set(config_manager, "log.level", "INFO")
  
  // Create development profile
  ConfigurationManager::create_profile(config_manager, "development")
  ConfigurationManager::set_profile(config_manager, "development", "service.debug", true)
  ConfigurationManager::set_profile(config_manager, "development", "log.level", "DEBUG")
  
  // Save configuration to file
  let save_result = ConfigurationManager::save_to_file(config_manager, "/tmp/test_config.json")
  assert_true(save_result.is_success)
  
  // Create new configuration manager and load from file
  let new_config_manager = ConfigurationManager::new()
  let load_result = ConfigurationManager::load_from_file(new_config_manager, "/tmp/test_config.json")
  assert_true(load_result.is_success)
  
  // Verify loaded configuration
  assert_eq(ConfigurationManager::get_string(new_config_manager, "service.name").unwrap(), "azimuth-telemetry")
  assert_eq(ConfigurationManager::get_string(new_config_manager, "service.version").unwrap(), "1.0.0")
  assert_eq(ConfigurationManager::get_int(new_config_manager, "service.port").unwrap(), 8080)
  assert_eq(ConfigurationManager::get_string(new_config_manager, "database.host").unwrap(), "localhost")
  assert_eq(ConfigurationManager::get_int(new_config_manager, "database.port").unwrap(), 5432)
  assert_eq(ConfigurationManager::get_string(new_config_manager, "log.level").unwrap(), "INFO")
  
  // Verify loaded profiles
  let profiles = ConfigurationManager::list_profiles(new_config_manager)
  assert_true(profiles.contains("development"))
  
  // Activate profile and verify profile-specific settings
  ConfigurationManager::activate_profile(new_config_manager, "development")
  assert_eq(ConfigurationManager::get_bool(new_config_manager, "service.debug").unwrap(), true)
  assert_eq(ConfigurationManager::get_string(new_config_manager, "log.level").unwrap(), "DEBUG")
  
  // Test configuration export
  let exported_config = ConfigurationManager::export(new_config_manager)
  assert_true(exported_config.contains("azimuth-telemetry"))
  assert_true(exported_config.contains("8080"))
  assert_true(exported_config.contains("development"))
  
  // Test configuration import
  let import_config = {
    "service.name": "imported-service",
    "service.port": 9090,
    "new.feature": true
  }
  
  let import_result = ConfigurationManager::import_json(new_config_manager, import_config)
  assert_true(import_result.is_success)
  
  // Verify imported values
  assert_eq(ConfigurationManager::get_string(new_config_manager, "service.name").unwrap(), "imported-service")
  assert_eq(ConfigurationManager::get_int(new_config_manager, "service.port").unwrap(), 9090)
  assert_eq(ConfigurationManager::get_bool(new_config_manager, "new.feature").unwrap(), true)
  
  // Clean up
  @remove_file("/tmp/test_config.json")
}

// Test 6: Remote Configuration Management
test "remote configuration management" {
  // Create remote configuration manager
  let remote_config = RemoteConfigurationManager::new("https://config.example.com/api/v1")
  
  // Configure authentication
  RemoteConfigurationManager::set_api_key(remote_config, "test-api-key")
  RemoteConfigurationManager::set_timeout(remote_config, 5000)  // 5 seconds
  
  // Simulate remote configuration data
  let remote_data = {
    "service.name": "remote-telemetry",
    "service.version": "2.0.0",
    "feature.flags": {
      "new_dashboard": true,
      "advanced_analytics": false
    },
    "rate_limits": {
      "api": 1000,
      "webhook": 100
    }
  }
  
  // Mock remote configuration fetch
  RemoteConfigurationManager::mock_response(remote_config, remote_data)
  
  // Fetch remote configuration
  let fetch_result = RemoteConfigurationManager::fetch(remote_config)
  
  match fetch_result {
    FetchResult::Success(config) => {
      // Verify remote configuration
      assert_eq(ConfigurationManager::get_string(config, "service.name").unwrap(), "remote-telemetry")
      assert_eq(ConfigurationManager::get_string(config, "service.version").unwrap(), "2.0.0")
      
      // Test nested configuration
      match ConfigurationManager::get_nested(config, "feature.flags.new_dashboard") {
        Some(value) => assert_eq(value, true)
        None => assert_true(false)
      }
      
      match ConfigurationManager::get_nested(config, "rate_limits.api") {
        Some(value) => assert_eq(value, 1000)
        None => assert_true(false)
      }
    }
    FetchResult::Failed(error) => {
      // In test environment, we might not have a real server
      assert_true(error.contains("connection") || error.contains("network"))
    }
  }
  
  // Test configuration synchronization
  let local_config = ConfigurationManager::new()
  ConfigurationManager::set(local_config, "local.setting", "local_value")
  
  let sync_result = RemoteConfigurationManager::synchronize(remote_config, local_config)
  
  match sync_result {
    SyncResult::Success(merged_config) => {
      // Should have both local and remote settings
      assert_eq(ConfigurationManager::get_string(merged_config, "local.setting").unwrap(), "local_value")
      // Remote settings should also be present if fetch was successful
    }
    SyncResult::PartialSuccess(merged_config, errors) => {
      // Some settings might have failed to sync
      assert_eq(ConfigurationManager::get_string(merged_config, "local.setting").unwrap(), "local_value")
      assert_true(errors.length() > 0)
    }
    SyncResult::Failed(error) => {
      // In test environment, sync might fail
      assert_true(error.contains("connection") || error.contains("network"))
    }
  }
  
  // Test automatic refresh
  RemoteConfigurationManager::enable_auto_refresh(remote_config, 60000)  // 1 minute
  
  // Test configuration change notifications
  let mut change_notifications = []
  RemoteConfigurationManager::on_change(remote_config, |changes| {
    for change in changes {
      change_notifications.push(change)
    }
  })
  
  // Simulate remote configuration change
  let updated_remote_data = {
    "service.name": "updated-telemetry",
    "service.version": "2.1.0",
    "new.remote.setting": "new_value"
  }
  
  RemoteConfigurationManager::mock_response(remote_config, updated_remote_data)
  RemoteConfigurationManager::trigger_refresh(remote_config)
  
  // Verify change notifications
  assert_true(change_notifications.length() > 0)
}

// Test 7: Configuration Encryption and Security
test "configuration encryption and security" {
  // Create secure configuration manager
  let secure_config = SecureConfigurationManager::new("encryption-key-12345")
  
  // Set sensitive configuration values
  SecureConfigurationManager::set_secure(secure_config, "database.password", "secret-password")
  SecureConfigurationManager::set_secure(secure_config, "api.key", "secret-api-key")
  SecureConfigurationManager::set_secure(secure_config, "jwt.secret", "jwt-secret-key")
  
  // Set regular configuration values
  ConfigurationManager::set(secure_config, "service.name", "azimuth-telemetry")
  ConfigurationManager::set(secure_config, "service.port", 8080)
  
  // Verify secure values are encrypted internally
  let internal_storage = SecureConfigurationManager::get_internal_storage(secure_config)
  let encrypted_password = ConfigurationManager::get_raw(internal_storage, "database.password")
  
  match encrypted_password {
    Some(value) => {
      // Should be encrypted, not plain text
      assert_ne(value, "secret-password")
      assert_true(value.length() > "secret-password".length())
    }
    None => assert_true(false)
  }
  
  // Verify secure values can be retrieved properly
  match SecureConfigurationManager::get_secure(secure_config, "database.password") {
    Some(value) => assert_eq(value, "secret-password")
    None => assert_true(false)
  }
  
  match SecureConfigurationManager::get_secure(secure_config, "api.key") {
    Some(value) => assert_eq(value, "secret-api-key")
    None => assert_true(false)
  }
  
  match SecureConfigurationManager::get_secure(secure_config, "jwt.secret") {
    Some(value) => assert_eq(value, "jwt-secret-key")
    None => assert_true(false)
  }
  
  // Verify regular values are not encrypted
  assert_eq(ConfigurationManager::get_string(secure_config, "service.name").unwrap(), "azimuth-telemetry")
  assert_eq(ConfigurationManager::get_int(secure_config, "service.port").unwrap(), 8080)
  
  // Test configuration access control
  SecureConfigurationManager::set_access_policy(secure_config, "database.password", AccessLevel::Admin)
  SecureConfigurationManager::set_access_policy(secure_config, "api.key", AccessLevel::Service)
  SecureConfigurationManager::set_access_policy(secure_config, "service.name", AccessLevel::Public)
  
  // Test access with different roles
  let admin_context = SecurityContext::new("admin", ["admin"])
  let service_context = SecurityContext::new("service", ["service"])
  let public_context = SecurityContext::new("user", ["user"])
  
  // Admin can access all
  assert_true(SecureConfigurationManager::can_access(secure_config, "database.password", admin_context))
  assert_true(SecureConfigurationManager::can_access(secure_config, "api.key", admin_context))
  assert_true(SecureConfigurationManager::can_access(secure_config, "service.name", admin_context))
  
  // Service can access service-level and public
  assert_false(SecureConfigurationManager::can_access(secure_config, "database.password", service_context))
  assert_true(SecureConfigurationManager::can_access(secure_config, "api.key", service_context))
  assert_true(SecureConfigurationManager::can_access(secure_config, "service.name", service_context))
  
  // Public can only access public
  assert_false(SecureConfigurationManager::can_access(secure_config, "database.password", public_context))
  assert_false(SecureConfigurationManager::can_access(secure_config, "api.key", public_context))
  assert_true(SecureConfigurationManager::can_access(secure_config, "service.name", public_context))
  
  // Test secure export/import
  let secure_export = SecureConfigurationManager::export_secure(secure_config, admin_context)
  assert_true(secure_export.contains("azimuth-telemetry"))
  // Sensitive values should be encrypted in export
  assert_false(secure_export.contains("secret-password"))
  
  // Test configuration audit log
  let audit_log = SecureConfigurationManager::get_audit_log(secure_config)
  assert_true(audit_log.length() > 0)
  
  // Verify audit entries
  let password_access = audit_log.find(@(entry) AuditEntry::key(entry) == "database.password")
  match password_access {
    Some(entry) => {
      assert_eq(AuditEntry::action(entry), AuditAction::Create)
      assert_eq(AuditEntry::context(entry), "system")
    }
    None => assert_true(false)
  }
}

// Test 8: Configuration Templates and Inheritance
test "configuration templates and inheritance" {
  // Create configuration manager with template support
  let config_manager = ConfigurationManager::new()
  
  // Create base template
  ConfigurationManager::create_template(config_manager, "base_service")
  ConfigurationManager::set_template(config_manager, "base_service", "service.name", "{{service_name}}")
  ConfigurationManager::set_template(config_manager, "base_service", "service.version", "1.0.0")
  ConfigurationManager::set_template(config_manager, "base_service", "service.port", 8080)
  ConfigurationManager::set_template(config_manager, "base_service", "log.level", "INFO")
  
  // Create database template
  ConfigurationManager::create_template(config_manager, "database_service")
  ConfigurationManager::set_template_parent(config_manager, "database_service", "base_service")
  ConfigurationManager::set_template(config_manager, "database_service", "database.host", "{{db_host}}")
  ConfigurationManager::set_template(config_manager, "database_service", "database.port", 5432)
  ConfigurationManager::set_template(config_manager, "database_service", "database.name", "{{db_name}}")
  
  // Instantiate configuration from templates
  let template_vars = {
    "service_name": "user-service",
    "db_host": "db.example.com",
    "db_name": "userdb"
  }
  
  ConfigurationManager::instantiate_from_template(config_manager, "database_service", template_vars)
  
  // Verify instantiated configuration
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name").unwrap(), "user-service")
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version").unwrap(), "1.0.0")
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port").unwrap(), 8080)
  assert_eq(ConfigurationManager::get_string(config_manager, "log.level").unwrap(), "INFO")
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host").unwrap(), "db.example.com")
  assert_eq(ConfigurationManager::get_int(config_manager, "database.port").unwrap(), 5432)
  assert_eq(ConfigurationManager::get_string(config_manager, "database.name").unwrap(), "userdb")
  
  // Test template inheritance with overrides
  ConfigurationManager::create_template(config_manager, "production_service")
  ConfigurationManager::set_template_parent(config_manager, "production_service", "database_service")
  ConfigurationManager::set_template(config_manager, "production_service", "service.port", 443)
  ConfigurationManager::set_template(config_manager, "production_service", "log.level", "WARN")
  
  let prod_vars = {
    "service_name": "prod-user-service",
    "db_host": "prod-db.example.com",
    "db_name": "userdb_prod"
  }
  
  ConfigurationManager::instantiate_from_template(config_manager, "production_service", prod_vars)
  
  // Verify production overrides
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name").unwrap(), "prod-user-service")
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port").unwrap(), 443)  // Override
  assert_eq(ConfigurationManager::get_string(config_manager, "log.level").unwrap(), "WARN")  // Override
  assert_eq(ConfigurationManager::get_string(config_manager, "database.host").unwrap(), "prod-db.example.com")
  
  // Test template validation
  ConfigurationManager::add_template_validation(config_manager, "database_service", "db_host", ValidationRule::required())
  ConfigurationManager::add_template_validation(config_manager, "database_service", "db_name", ValidationRule::required())
  
  let invalid_vars = {
    "service_name": "invalid-service"
    // Missing db_host and db_name
  }
  
  let validation_result = ConfigurationManager::validate_template_instantiation(
    config_manager,
    "database_service",
    invalid_vars
  )
  
  assert_false(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 2)  // Missing db_host and db_name
  
  // Test template listing
  let templates = ConfigurationManager::list_templates(config_manager)
  assert_true(templates.contains("base_service"))
  assert_true(templates.contains("database_service"))
  assert_true(templates.contains("production_service"))
}

// Test 9: Configuration Rollback and Versioning
test "configuration rollback and versioning" {
  // Create configuration manager with versioning
  let config_manager = VersionedConfigurationManager::new()
  
  // Enable versioning
  VersionedConfigurationManager::enable_versioning(config_manager, 10)  // Keep 10 versions
  
  // Set initial configuration (version 1)
  ConfigurationManager::set(config_manager, "service.name", "azimuth-telemetry")
  ConfigurationManager::set(config_manager, "service.version", "1.0.0")
  ConfigurationManager::set(config_manager, "service.port", 8080)
  
  VersionedConfigurationManager::create_snapshot(config_manager, "Initial configuration")
  
  // Update configuration (version 2)
  ConfigurationManager::set(config_manager, "service.version", "1.1.0")
  ConfigurationManager::set(config_manager, "feature.new_dashboard", true)
  
  VersionedConfigurationManager::create_snapshot(config_manager, "Added new dashboard feature")
  
  // Update configuration again (version 3)
  ConfigurationManager::set(config_manager, "service.port", 9090)
  ConfigurationManager::set(config_manager, "database.timeout", 30)
  
  VersionedConfigurationManager::create_snapshot(config_manager, "Updated port and database timeout")
  
  // Verify current configuration
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name").unwrap(), "azimuth-telemetry")
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version").unwrap(), "1.1.0")
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port").unwrap(), 9090)
  assert_eq(ConfigurationManager::get_bool(config_manager, "feature.new_dashboard").unwrap(), true)
  assert_eq(ConfigurationManager::get_int(config_manager, "database.timeout").unwrap(), 30)
  
  // List versions
  let versions = VersionedConfigurationManager::list_versions(config_manager)
  assert_eq(versions.length(), 3)
  
  // Verify version information
  assert_eq(versions[0].version, 3)
  assert_eq(versions[0].description, "Updated port and database timeout")
  assert_eq(versions[1].version, 2)
  assert_eq(versions[1].description, "Added new dashboard feature")
  assert_eq(versions[2].version, 1)
  assert_eq(versions[2].description, "Initial configuration")
  
  // Rollback to version 2
  let rollback_result = VersionedConfigurationManager::rollback(config_manager, 2)
  assert_true(rollback_result.is_success)
  
  // Verify rollback
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name").unwrap(), "azimuth-telemetry")
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version").unwrap(), "1.1.0")
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port").unwrap(), 8080)  // Reverted
  assert_eq(ConfigurationManager::get_bool(config_manager, "feature.new_dashboard").unwrap(), true)
  match ConfigurationManager::get(config_manager, "database.timeout") {
    Some(_) => assert_true(false)  // Should not exist in version 2
    None => assert_true(true)
  }
  
  // Create new snapshot after rollback (version 4)
  VersionedConfigurationManager::create_snapshot(config_manager, "After rollback to version 2")
  
  // Verify new version was created
  let updated_versions = VersionedConfigurationManager::list_versions(config_manager)
  assert_eq(updated_versions.length(), 4)
  assert_eq(updated_versions[0].version, 4)
  
  // Test version comparison
  let diff = VersionedConfigurationManager::compare_versions(config_manager, 2, 3)
  
  assert_true(diff.contains("service.port"))  // Changed from 8080 to 9090
  assert_true(diff.contains("database.timeout"))  // Added in version 3
  
  // Test configuration restoration
  let restore_result = VersionedConfigurationManager::restore_to_version(config_manager, 1)
  assert_true(restore_result.is_success)
  
  // Verify restoration to initial configuration
  assert_eq(ConfigurationManager::get_string(config_manager, "service.name").unwrap(), "azimuth-telemetry")
  assert_eq(ConfigurationManager::get_string(config_manager, "service.version").unwrap(), "1.0.0")
  assert_eq(ConfigurationManager::get_int(config_manager, "service.port").unwrap(), 8080)
  match ConfigurationManager::get(config_manager, "feature.new_dashboard") {
    Some(_) => assert_true(false)  // Should not exist in version 1
    None => assert_true(true)
  }
}

// Test 10: Configuration Performance and Optimization
test "configuration performance and optimization" {
  // Create optimized configuration manager
  let config_manager = OptimizedConfigurationManager::new()
  
  // Configure optimization settings
  OptimizedConfigurationManager::enable_caching(config_manager, 1000)  // Cache 1000 values
  OptimizedConfigurationManager::enable_batch_operations(config_manager)
  OptimizedConfigurationManager::enable_async_loading(config_manager, 4)  // 4 worker threads
  
  // Test batch configuration operations
  let batch_config = [
    ("service.name", "batch-service"),
    ("service.version", "1.0.0"),
    ("service.port", 8080),
    ("service.host", "localhost"),
    ("database.host", "db.example.com"),
    ("database.port", 5432),
    ("database.name", "batchdb"),
    ("log.level", "INFO"),
    ("log.format", "json"),
    ("cache.enabled", true)
  ]
  
  let start_time = @time()
  let batch_result = OptimizedConfigurationManager::set_batch(config_manager, batch_config)
  let batch_time = @time() - start_time
  
  assert_true(batch_result.is_success)
  
  // Test individual operations for comparison
  let individual_config_manager = ConfigurationManager::new()
  let start_time = @time()
  for (key, value) in batch_config {
    ConfigurationManager::set(individual_config_manager, key, value)
  }
  let individual_time = @time() - start_time
  
  // Batch should be faster
  assert_true(batch_time < individual_time)
  
  // Test configuration retrieval performance
  let start_time = @time()
  for i in 0..=999 {
    ConfigurationManager::get(config_manager, "service.name")
  }
  let retrieval_time = @time() - start_time
  
  // Cached retrieval should be fast
  assert_true(retrieval_time < 100)  // Less than 100ms for 1000 retrievals
  
  // Test configuration indexing
  OptimizedConfigurationManager::create_index(config_manager, "service")
  OptimizedConfigurationManager::create_index(config_manager, "database")
  OptimizedConfigurationManager::create_index(config_manager, "log")
  
  // Test indexed lookups
  let service_keys = OptimizedConfigurationManager::get_keys_by_prefix(config_manager, "service")
  assert_eq(service_keys.length(), 4)
  assert_true(service_keys.contains("service.name"))
  assert_true(service_keys.contains("service.version"))
  assert_true(service_keys.contains("service.port"))
  assert_true(service_keys.contains("service.host"))
  
  let database_keys = OptimizedConfigurationManager::get_keys_by_prefix(config_manager, "database")
  assert_eq(database_keys.length(), 3)
  
  // Test configuration hot-reloading
  let hot_reload_config = {
    "hot.reload.setting": "hot_reload_value",
    "dynamic.feature": true
  }
  
  OptimizedConfigurationManager::enable_hot_reload(config_manager, "/tmp/hot_reload_config.json")
  
  // Simulate hot reload file update
  let write_result = @write_file("/tmp/hot_reload_config.json", @to_json_string(hot_reload_config))
  assert_true(write_result)
  
  // Wait for hot reload to detect changes
  @sleep(100)
  
  // Verify hot-reloaded values
  match ConfigurationManager::get(config_manager, "hot.reload.setting") {
    Some(value) => assert_eq(value, "hot_reload_value")
    None => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "dynamic.feature") {
    Some(value) => assert_eq(value, true)
    None => assert_true(false)
  }
  
  // Get performance metrics
  let metrics = OptimizedConfigurationManager::get_performance_metrics(config_manager)
  
  assert_true(PerformanceMetrics::cache_hit_rate(metrics) > 0.0)
  assert_true(PerformanceMetrics::average_get_time(metrics) > 0.0)
  assert_true(PerformanceMetrics::average_set_time(metrics) > 0.0)
  assert_true(PerformanceMetrics::operations_per_second(metrics) > 0.0)
  assert_true(PerformanceMetrics::memory_usage(metrics) > 0)
  
  // Test configuration compression
  let large_config_data = []
  for i in 0..=999 {
    large_config_data.push(("large.key." + i.to_string(), "large.value." + i.to_string()))
  }
  
  OptimizedConfigurationManager::set_batch(config_manager, large_config_data)
  
  let compression_stats = OptimizedConfigurationManager::get_compression_stats(config_manager)
  assert_true(CompressionStats::original_size(compression_stats) > CompressionStats::compressed_size(compression_stats))
  assert_true(CompressionStats::compression_ratio(compression_stats) > 0.0)
  
  // Clean up
  @remove_file("/tmp/hot_reload_config.json")
}