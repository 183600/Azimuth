// Azimuth Configuration Management Dynamic Update Tests
// This file contains test cases for configuration management and dynamic update functionality

// Test 1: Basic Configuration Loading and Access
test "basic configuration loading and access" {
  let config_manager = ConfigurationManager::new()
  
  // Load configuration from different sources
  let file_config = FileConfigSource::new("config.json")
  let env_config = EnvironmentConfigSource::new()
  let cmd_config = CommandLineConfigSource::new()
  
  ConfigurationManager::add_source(config_manager, file_config)
  ConfigurationManager::add_source(config_manager, env_config)
  ConfigurationManager::add_source(config_manager, cmd_config)
  
  // Test configuration access
  match ConfigurationManager::get(config_manager, "server.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "server.host") {
    Some(ConfigValue::String(host)) => assert_eq(host, "localhost")
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "database.max_connections") {
    Some(ConfigValue::Int(max_conn)) => assert_eq(max_conn, 10)
    _ => assert_true(false)
  }
  
  // Test default values
  match ConfigurationManager::get_with_default(config_manager, "cache.ttl", ConfigValue::Int(300)) {
    ConfigValue::Int(ttl) => assert_eq(ttl, 300)  // Should use default
    _ => assert_true(false)
  }
  
  // Test configuration existence
  assert_eq(ConfigurationManager::contains(config_manager, "server.port"), true)
  assert_eq(ConfigurationManager::contains(config_manager, "nonexistent.key"), false)
  
  // Test configuration hierarchy
  let server_config = ConfigurationManager::get_section(config_manager, "server")
  match server_config {
    Some(section) => {
      assert_eq(ConfigurationManager::contains(section, "port"), true)
      assert_eq(ConfigurationManager::contains(section, "host"), true)
    }
    None => assert_true(false)
  }
}

// Test 2: Dynamic Configuration Updates
test "dynamic configuration updates" {
  let config_manager = ConfigurationManager::new()
  
  // Add a file source that supports watching for changes
  let watchable_config = WatchableFileConfigSource::new("dynamic_config.json")
  ConfigurationManager::add_source(config_manager, watchable_config)
  
  // Get initial value
  match ConfigurationManager::get(config_manager, "feature.enabled") {
    Some(ConfigValue::Bool(enabled)) => assert_eq(enabled, false)
    _ => assert_true(false)
  }
  
  // Register a change listener
  let mut change_detected = false
  let change_listener = fn(key, old_value, new_value) {
    change_detected = true
    assert_eq(key, "feature.enabled")
    match (old_value, new_value) {
      (ConfigValue::Bool(old), ConfigValue::Bool(new)) => {
        assert_eq(old, false)
        assert_eq(new, true)
      }
      _ => assert_true(false)
    }
  }
  
  ConfigurationManager::add_change_listener(config_manager, "feature.enabled", change_listener)
  
  // Simulate configuration file change
  WatchableFileConfigSource::simulate_change(watchable_config, "feature.enabled", ConfigValue::Bool(true))
  
  // Allow time for change detection
  Clock::sleep(100)
  
  // Verify change was detected and applied
  assert_eq(change_detected, true)
  
  match ConfigurationManager::get(config_manager, "feature.enabled") {
    Some(ConfigValue::Bool(enabled)) => assert_eq(enabled, true)
    _ => assert_true(false)
  }
  
  // Test multiple change listeners
  let mut listener1_called = false
  let mut listener2_called = false
  
  let listener1 = fn(key, old_value, new_value) {
    listener1_called = true
  }
  
  let listener2 = fn(key, old_value, new_value) {
    listener2_called = true
  }
  
  ConfigurationManager::add_change_listener(config_manager, "feature.enabled", listener1)
  ConfigurationManager::add_change_listener(config_manager, "feature.enabled", listener2)
  
  // Change value again
  WatchableFileConfigSource::simulate_change(watchable_config, "feature.enabled", ConfigValue::Bool(false))
  Clock::sleep(100)
  
  assert_eq(listener1_called, true)
  assert_eq(listener2_called, true)
  
  match ConfigurationManager::get(config_manager, "feature.enabled") {
    Some(ConfigValue::Bool(enabled)) => assert_eq(enabled, false)
    _ => assert_true(false)
  }
}

// Test 3: Configuration Validation
test "configuration validation" {
  let config_manager = ConfigurationManager::new()
  
  // Define validation rules
  let port_validator = ValidationRule::range(1, 65535)  // Valid port range
  let host_validator = ValidationRule::non_empty_string()
  let url_validator = ValidationRule::url()
  let email_validator = ValidationRule::email()
  
  ConfigurationManager::add_validator(config_manager, "server.port", port_validator)
  ConfigurationManager::add_validator(config_manager, "server.host", host_validator)
  ConfigurationManager::add_validator(config_manager, "api.endpoint", url_validator)
  ConfigurationManager::add_validator(config_manager, "admin.email", email_validator)
  
  // Test valid configuration
  let valid_config = [
    ("server.port", ConfigValue::Int(8080)),
    ("server.host", ConfigValue::String("localhost")),
    ("api.endpoint", ConfigValue::String("https://api.example.com")),
    ("admin.email", ConfigValue::String("admin@example.com"))
  ]
  
  let validation_result = ConfigurationManager::validate(config_manager, valid_config)
  assert_eq(validation_result.is_valid, true)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test invalid configuration
  let invalid_config = [
    ("server.port", ConfigValue::Int(70000)),  // Invalid port
    ("server.host", ConfigValue::String("")),   // Empty host
    ("api.endpoint", ConfigValue::String("not-a-url")),  // Invalid URL
    ("admin.email", ConfigValue::String("not-an-email"))  // Invalid email
  ]
  
  let invalid_result = ConfigurationManager::validate(config_manager, invalid_config)
  assert_eq(invalid_result.is_valid, false)
  assert_eq(invalid_result.errors.length(), 4)
  
  // Check specific errors
  let mut port_error_found = false
  let mut host_error_found = false
  let mut url_error_found = false
  let mut email_error_found = false
  
  for error in invalid_result.errors {
    match error.key {
      "server.port" => port_error_found = true
      "server.host" => host_error_found = true
      "api.endpoint" => url_error_found = true
      "admin.email" => email_error_found = true
      _ => assert_true(false)
    }
  }
  
  assert_eq(port_error_found, true)
  assert_eq(host_error_found, true)
  assert_eq(url_error_found, true)
  assert_eq(email_error_found, true)
  
  // Test conditional validation
  let ssl_validator = ValidationRule::conditional(
    fn(config) {
      match ConfigurationManager::get(config, "server.ssl.enabled") {
        Some(ConfigValue::Bool(true)) => true
        _ => false
      }
    },
    ValidationRule::non_empty_string()
  )
  
  ConfigurationManager::add_validator(config_manager, "server.ssl.certificate_path", ssl_validator)
  
  // Config with SSL disabled should pass without certificate path
  let ssl_disabled_config = [
    ("server.ssl.enabled", ConfigValue::Bool(false))
  ]
  
  let ssl_disabled_result = ConfigurationManager::validate(config_manager, ssl_disabled_config)
  assert_eq(ssl_disabled_result.is_valid, true)
  
  // Config with SSL enabled should require certificate path
  let ssl_enabled_no_cert = [
    ("server.ssl.enabled", ConfigValue::Bool(true))
  ]
  
  let ssl_enabled_result = ConfigurationManager::validate(config_manager, ssl_enabled_no_cert)
  assert_eq(ssl_enabled_result.is_valid, false)
}

// Test 4: Configuration Profiles and Environments
test "configuration profiles and environments" {
  let config_manager = ConfigurationManager::new()
  
  // Load base configuration
  let base_config = FileConfigSource::new("base_config.json")
  ConfigurationManager::add_source(config_manager, base_config)
  
  // Load environment-specific configurations
  let dev_config = FileConfigSource::new("dev_config.json")
  let prod_config = FileConfigSource::new("prod_config.json")
  
  ConfigurationManager::add_profile(config_manager, "development", dev_config)
  ConfigurationManager::add_profile(config_manager, "production", prod_config)
  
  // Test development profile
  ConfigurationManager::set_active_profile(config_manager, "development")
  
  match ConfigurationManager::get(config_manager, "database.host") {
    Some(ConfigValue::String(host)) => assert_eq(host, "localhost")  // Dev value
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "logging.level") {
    Some(ConfigValue::String(level)) => assert_eq(level, "DEBUG")  // Dev value
    _ => assert_true(false)
  }
  
  // Test production profile
  ConfigurationManager::set_active_profile(config_manager, "production")
  
  match ConfigurationManager::get(config_manager, "database.host") {
    Some(ConfigValue::String(host)) => assert_eq(host, "prod-db.example.com")  // Prod value
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "logging.level") {
    Some(ConfigValue::String(level)) => assert_eq(level, "INFO")  // Prod value
    _ => assert_true(false)
  }
  
  // Test profile switching
  let mut profile_switch_detected = false
  let profile_switch_listener = fn(old_profile, new_profile) {
    profile_switch_detected = true
    assert_eq(old_profile, "production")
    assert_eq(new_profile, "development")
  }
  
  ConfigurationManager::add_profile_switch_listener(config_manager, profile_switch_listener)
  
  ConfigurationManager::set_active_profile(config_manager, "development")
  assert_eq(profile_switch_detected, true)
  
  // Test profile inheritance
  ConfigurationManager::set_active_profile(config_manager, "development")
  
  // Base value should be available if not overridden
  match ConfigurationManager::get(config_manager, "server.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 8080)  // From base config
    _ => assert_true(false)
  }
  
  // Test profile composition
  let test_config = [
    ("feature.test_mode", ConfigValue::Bool(true)),
    ("feature.debug_mode", ConfigValue::Bool(true))
  ]
  
  ConfigurationManager::add_profile_config(config_manager, "testing", test_config)
  ConfigurationManager::set_active_profile(config_manager, "testing")
  
  match ConfigurationManager::get(config_manager, "feature.test_mode") {
    Some(ConfigValue::Bool(enabled)) => assert_eq(enabled, true)
    _ => assert_true(false)
  }
}

// Test 5: Configuration Encryption and Security
test "configuration encryption and security" {
  let config_manager = ConfigurationManager::new()
  
  // Create encrypted configuration source
  let encryption_key = EncryptionKey::generate()
  let encrypted_config = EncryptedConfigSource::new("encrypted_config.json", encryption_key)
  ConfigurationManager::add_source(config_manager, encrypted_config)
  
  // Test encrypted value access
  match ConfigurationManager::get(config_manager, "database.password") {
    Some(ConfigValue::String(password)) => {
      assert_eq(password, "secret123")  // Should be automatically decrypted
    }
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "api.secret_key") {
    Some(ConfigValue::String(secret_key)) => {
      assert_eq(secret_key, "api-secret-key")
    }
    _ => assert_true(false)
  }
  
  // Test secure value update
  ConfigurationManager::set_secure(config_manager, "new.secret", ConfigValue::String("new-secret-value"))
  
  match ConfigurationManager::get(config_manager, "new.secret") {
    Some(ConfigValue::String(value)) => assert_eq(value, "new-secret-value")
    _ => assert_true(false)
  }
  
  // Test access control
  let access_manager = ConfigAccessManager::new()
  
  // Define access policies
  access_manager.add_policy("database.*", [Role::ADMIN, Role::DBA])
  access_manager.add_policy("api.*", [Role::API_USER, Role::ADMIN])
  access_manager.add_policy("server.*", [Role::WEB_USER, Role::ADMIN])
  
  ConfigurationManager::set_access_manager(config_manager, access_manager)
  
  // Test access with different roles
  let admin_context = SecurityContext::with_roles([Role::ADMIN])
  let user_context = SecurityContext::with_roles([Role::WEB_USER])
  
  // Admin should access everything
  assert_eq(ConfigurationManager::can_access(config_manager, "database.password", admin_context), true)
  assert_eq(ConfigurationManager::can_access(config_manager, "api.secret_key", admin_context), true)
  assert_eq(ConfigurationManager::can_access(config_manager, "server.port", admin_context), true)
  
  // Web user should only access server config
  assert_eq(ConfigurationManager::can_access(config_manager, "database.password", user_context), false)
  assert_eq(ConfigurationManager::can_access(config_manager, "api.secret_key", user_context), false)
  assert_eq(ConfigurationManager::can_access(config_manager, "server.port", user_context), true)
  
  // Test masked values for unauthorized access
  match ConfigurationManager::get_masked(config_manager, "database.password", user_context) {
    Some(ConfigValue::String(masked)) => assert_eq(masked, "*****")
    _ => assert_true(false)
  }
}

// Test 6: Configuration Templates and Generation
test "configuration templates and generation" {
  let config_manager = ConfigurationManager::new()
  
  // Define configuration template
  let template = ConfigTemplate::new("service_template")
  template.add_parameter("service_name", "string", "my-service")
  template.add_parameter("port", "int", "8080")
  template.add_parameter("enable_ssl", "bool", "false")
  template.add_parameter("max_connections", "int", "10")
  
  ConfigurationManager::add_template(config_manager, template)
  
  // Generate configuration from template with default values
  let default_config = ConfigurationManager::generate_from_template(config_manager, "service_template", [])
  
  match ConfigurationManager::get_section(default_config, "service") {
    Some(section) => {
      match ConfigurationManager::get(section, "name") {
        Some(ConfigValue::String(name)) => assert_eq(name, "my-service")
        _ => assert_true(false)
      }
      
      match ConfigurationManager::get(section, "port") {
        Some(ConfigValue::Int(port)) => assert_eq(port, 8080)
        _ => assert_true(false)
      }
      
      match ConfigurationManager::get(section, "ssl.enabled") {
        Some(ConfigValue::Bool(ssl)) => assert_eq(ssl, false)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Generate configuration with custom parameters
  let custom_params = [
    ("service_name", "custom-service"),
    ("port", "9090"),
    ("enable_ssl", "true"),
    ("max_connections", "20")
  ]
  
  let custom_config = ConfigurationManager::generate_from_template(config_manager, "service_template", custom_params)
  
  match ConfigurationManager::get_section(custom_config, "service") {
    Some(section) => {
      match ConfigurationManager::get(section, "name") {
        Some(ConfigValue::String(name)) => assert_eq(name, "custom-service")
        _ => assert_true(false)
      }
      
      match ConfigurationManager::get(section, "port") {
        Some(ConfigValue::Int(port)) => assert_eq(port, 9090)
        _ => assert_true(false)
      }
      
      match ConfigurationManager::get(section, "ssl.enabled") {
        Some(ConfigValue::Bool(ssl)) => assert_eq(ssl, true)
        _ => assert_true(false)
      }
      
      match ConfigurationManager::get(section, "database.max_connections") {
        Some(ConfigValue::Int(max_conn)) => assert_eq(max_conn, 20)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test template inheritance
  let base_template = ConfigTemplate::new("base_template")
  base_template.add_parameter("log_level", "string", "INFO")
  base_template.add_parameter("timeout", "int", "30")
  
  let extended_template = ConfigTemplate::with_parent("extended_template", "base_template")
  extended_template.add_parameter("retry_count", "int", "3")
  extended_template.add_parameter("enable_metrics", "bool", "true")
  
  ConfigurationManager::add_template(config_manager, base_template)
  ConfigurationManager::add_template(config_manager, extended_template)
  
  let inherited_config = ConfigurationManager::generate_from_template(config_manager, "extended_template", [])
  
  // Should have parameters from both templates
  assert_eq(ConfigurationManager::contains(inherited_config, "logging.level"), true)
  assert_eq(ConfigurationManager::contains(inherited_config, "request.timeout"), true)
  assert_eq(ConfigurationManager::contains(inherited_config, "retry.count"), true)
  assert_eq(ConfigurationManager::contains(inherited_config, "metrics.enabled"), true)
}

// Test 7: Configuration Backup and Restore
test "configuration backup and restore" {
  let config_manager = ConfigurationManager::new()
  
  // Set initial configuration
  ConfigurationManager::set(config_manager, "server.port", ConfigValue::Int(8080))
  ConfigurationManager::set(config_manager, "server.host", ConfigValue::String("localhost"))
  ConfigurationManager::set(config_manager, "database.url", ConfigValue::String("postgresql://localhost/db"))
  ConfigurationManager::set(config_manager, "feature.enabled", ConfigValue::Bool(true))
  
  // Create backup
  let backup_id = ConfigurationManager::create_backup(config_manager, "initial_setup")
  assert_eq(backup_id != "", true)
  
  // List backups
  let backups = ConfigurationManager::list_backups(config_manager)
  assert_eq(backups.length(), 1)
  assert_eq(backups[0].id, backup_id)
  assert_eq(backups[0].description, "initial_setup")
  
  // Modify configuration
  ConfigurationManager::set(config_manager, "server.port", ConfigValue::Int(9090))
  ConfigurationManager::set(config_manager, "feature.enabled", ConfigValue::Bool(false))
  ConfigurationManager::remove(config_manager, "database.url")
  
  // Verify changes
  match ConfigurationManager::get(config_manager, "server.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "feature.enabled") {
    Some(ConfigValue::Bool(enabled)) => assert_eq(enabled, false)
    _ => assert_true(false)
  }
  
  assert_eq(ConfigurationManager::contains(config_manager, "database.url"), false)
  
  // Create another backup
  let backup_id2 = ConfigurationManager::create_backup(config_manager, "modified_setup")
  
  // Restore from first backup
  let restore_result = ConfigurationManager::restore_from_backup(config_manager, backup_id)
  assert_eq(restore_result, true)
  
  // Verify restoration
  match ConfigurationManager::get(config_manager, "server.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "feature.enabled") {
    Some(ConfigValue::Bool(enabled)) => assert_eq(enabled, true)
    _ => assert_true(false)
  }
  
  assert_eq(ConfigurationManager::contains(config_manager, "database.url"), true)
  
  // Test selective restore
  let keys_to_restore = ["server.port", "server.host"]
  let selective_restore = ConfigurationManager::restore_keys(config_manager, backup_id2, keys_to_restore)
  assert_eq(selective_restore, true)
  
  // Only specified keys should be restored
  match ConfigurationManager::get(config_manager, "server.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 9090)  // Restored
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "feature.enabled") {
    Some(ConfigValue::Bool(enabled)) => assert_eq(enabled, true)  // Not restored
    _ => assert_true(false)
  }
  
  // Test backup deletion
  let delete_result = ConfigurationManager::delete_backup(config_manager, backup_id)
  assert_eq(delete_result, true)
  
  let remaining_backups = ConfigurationManager::list_backups(config_manager)
  assert_eq(remaining_backups.length(), 1)
}

// Test 8: Configuration Synchronization
test "configuration synchronization" {
  let config_manager1 = ConfigurationManager::new()
  let config_manager2 = ConfigurationManager::new()
  
  // Set up synchronization
  let sync_manager = ConfigSyncManager::new()
  sync_manager.add_node(config_manager1, "node1")
  sync_manager.add_node(config_manager2, "node2")
  
  // Configure sync strategy
  let sync_strategy = SyncStrategy::new()
    .with_mode(SyncMode::PeerToPeer)
    .with_conflict_resolution(LastWriteWins)
    .with_sync_interval(100)  // 100ms
  
  sync_manager.set_strategy(sync_strategy)
  
  // Set initial configuration on node1
  ConfigurationManager::set(config_manager1, "app.name", ConfigValue::String("my-app"))
  ConfigurationManager::set(config_manager1, "app.version", ConfigValue::String("1.0.0"))
  
  // Allow time for synchronization
  Clock::sleep(150)
  
  // Verify configuration synced to node2
  match ConfigurationManager::get(config_manager2, "app.name") {
    Some(ConfigValue::String(name)) => assert_eq(name, "my-app")
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager2, "app.version") {
    Some(ConfigValue::String(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // Update configuration on node2
  ConfigurationManager::set(config_manager2, "app.version", ConfigValue::String("1.1.0"))
  ConfigurationManager::set(config_manager2, "app.feature", ConfigValue::String("new-feature"))
  
  // Allow time for synchronization
  Clock::sleep(150)
  
  // Verify changes synced back to node1
  match ConfigurationManager::get(config_manager1, "app.version") {
    Some(ConfigValue::String(version)) => assert_eq(version, "1.1.0")
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager1, "app.feature") {
    Some(ConfigValue::String(feature)) => assert_eq(feature, "new-feature")
    _ => assert_true(false)
  }
  
  // Test conflict resolution
  sync_manager.set_conflict_resolution(FirstWriteWins)
  
  // Simultaneous updates to same key
  ConfigurationManager::set(config_manager1, "conflict.test", ConfigValue::String("value-from-node1"))
  ConfigurationManager::set(config_manager2, "conflict.test", ConfigValue::String("value-from-node2"))
  
  Clock::sleep(150)
  
  // With FirstWriteWins, both should have the first value
  match ConfigurationManager::get(config_manager1, "conflict.test") {
    Some(ConfigValue::String(value)) => assert_eq(value, "value-from-node1")
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager2, "conflict.test") {
    Some(ConfigValue::String(value)) => assert_eq(value, "value-from-node1")
    _ => assert_true(false)
  }
  
  // Test sync statistics
  let sync_stats = sync_manager.get_statistics()
  assert_eq(sync_stats.total_syncs > 0, true)
  assert_eq(sync_stats.conflicts_resolved > 0, true)
  assert_eq(sync_stats.nodes_count, 2)
}

// Test 9: Configuration Hot Reload
test "configuration hot reload" {
  let config_manager = ConfigurationManager::new()
  
  // Add hot-reloadable sources
  let main_config = HotReloadableFileConfigSource::new("main_config.json")
  let env_config = HotReloadableEnvironmentConfigSource::new()
  
  ConfigurationManager::add_source(config_manager, main_config)
  ConfigurationManager::add_source(config_manager, env_config)
  
  // Get initial values
  match ConfigurationManager::get(config_manager, "cache.size") {
    Some(ConfigValue::Int(size)) => assert_eq(size, 100)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "log.level") {
    Some(ConfigValue::String(level)) => assert_eq(level, "INFO")
    _ => assert_true(false)
  }
  
  // Register hot reload listeners
  let mut cache_size_changed = false
  let mut log_level_changed = false
  
  let cache_listener = fn(key, old_value, new_value) {
    cache_size_changed = true
    assert_eq(key, "cache.size")
    match (old_value, new_value) {
      (ConfigValue::Int(old), ConfigValue::Int(new)) => {
        assert_eq(old, 100)
        assert_eq(new, 200)
      }
      _ => assert_true(false)
    }
  }
  
  let log_listener = fn(key, old_value, new_value) {
    log_level_changed = true
    assert_eq(key, "log.level")
    match (old_value, new_value) {
      (ConfigValue::String(old), ConfigValue::String(new)) => {
        assert_eq(old, "INFO")
        assert_eq(new, "DEBUG")
      }
      _ => assert_true(false)
    }
  }
  
  ConfigurationManager::add_hot_reload_listener(config_manager, "cache.size", cache_listener)
  ConfigurationManager::add_hot_reload_listener(config_manager, "log.level", log_listener)
  
  // Simulate configuration file changes
  HotReloadableFileConfigSource::simulate_file_change(main_config, "cache.size", ConfigValue::Int(200))
  HotReloadableFileConfigSource::simulate_file_change(main_config, "log.level", ConfigValue::String("DEBUG"))
  
  // Allow time for hot reload
  Clock::sleep(100)
  
  // Verify changes were applied and listeners were called
  assert_eq(cache_size_changed, true)
  assert_eq(log_level_changed, true)
  
  match ConfigurationManager::get(config_manager, "cache.size") {
    Some(ConfigValue::Int(size)) => assert_eq(size, 200)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "log.level") {
    Some(ConfigValue::String(level)) => assert_eq(level, "DEBUG")
    _ => assert_true(false)
  }
  
  // Test hot reload with validation
  let port_validator = ValidationRule::range(1, 65535)
  ConfigurationManager::add_validator(config_manager, "server.port", port_validator)
  
  // Try to set invalid port
  HotReloadableFileConfigSource::simulate_file_change(main_config, "server.port", ConfigValue::Int(70000))
  Clock::sleep(100)
  
  // Should reject invalid value and keep original
  match ConfigurationManager::get(config_manager, "server.port") {
    Some(ConfigValue::Int(port)) => assert_eq(port, 8080)  // Should remain unchanged
    _ => assert_true(false)
  }
  
  // Test hot reload error handling
  let mut error_handled = false
  let error_handler = fn(key, error) {
    error_handled = true
    assert_eq(key, "server.port")
  }
  
  ConfigurationManager::set_error_handler(config_manager, error_handler)
  assert_eq(error_handled, true)
}

// Test 10: Configuration Metrics and Monitoring
test "configuration metrics and monitoring" {
  let config_manager = ConfigurationManager::new()
  
  // Enable metrics collection
  ConfigurationManager::enable_metrics(config_manager)
  
  // Perform various configuration operations
  ConfigurationManager::set(config_manager, "test.key1", ConfigValue::String("value1"))
  ConfigurationManager::set(config_manager, "test.key2", ConfigValue::Int(42))
  ConfigurationManager::set(config_manager, "test.key3", ConfigValue::Bool(true))
  
  match ConfigurationManager::get(config_manager, "test.key1") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Update existing key
  ConfigurationManager::set(config_manager, "test.key1", ConfigValue::String("updated_value1"))
  
  // Remove key
  ConfigurationManager::remove(config_manager, "test.key3")
  
  // Get metrics
  let metrics = ConfigurationManager::get_metrics(config_manager)
  
  assert_eq(metrics.total_sets, 4)  // 3 initial sets + 1 update
  assert_eq(metrics.total_gets, 2)  // 1 successful + 1 failed
  assert_eq(metrics.successful_gets, 1)
  assert_eq(metrics.failed_gets, 1)
  assert_eq(metrics.total_removes, 1)
  assert_eq(metrics.unique_keys, 2)  // key1 and key2 (key3 was removed)
  
  // Test operation timing metrics
  assert_eq(metrics.average_set_time > 0, true)
  assert_eq(metrics.average_get_time > 0, true)
  
  // Test hot reload metrics
  let hot_reload_config = HotReloadableFileConfigSource::new("metrics_config.json")
  ConfigurationManager::add_source(config_manager, hot_reload_config)
  
  // Trigger some hot reloads
  HotReloadableFileConfigSource::simulate_file_change(hot_reload_config, "reload.test1", ConfigValue::String("value1"))
  HotReloadableFileConfigSource::simulate_file_change(hot_reload_config, "reload.test2", ConfigValue::String("value2"))
  Clock::sleep(100)
  
  let updated_metrics = ConfigurationManager::get_metrics(config_manager)
  assert_eq(updated_metrics.hot_reload_count, 2)
  assert_eq(updated_metrics.hot_reload_success_count, 2)
  
  // Test access pattern metrics
  ConfigurationManager::set(config_manager, "popular.key", ConfigValue::String("popular"))
  
  // Access popular key multiple times
  for i = 0; i < 10; i = i + 1 {
    match ConfigurationManager::get(config_manager, "popular.key") {
      Some(_) => assert_true(true)
      None => assert_true(false)
    }
  }
  
  // Access unpopular key
  ConfigurationManager::set(config_manager, "unpopular.key", ConfigValue::String("unpopular"))
  match ConfigurationManager::get(config_manager, "unpopular.key") {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  let access_patterns = ConfigurationManager::get_access_patterns(config_manager)
  assert_eq(access_patterns.length(), 4)  // key1, key2, popular.key, unpopular.key
  
  // Find most accessed key
  let most_accessed = access_patterns[0]  // Should be sorted by access count
  assert_eq(most_accessed.key, "popular.key")
  assert_eq(most_accessed.access_count, 10)
  
  // Test configuration size metrics
  let size_metrics = ConfigurationManager::get_size_metrics(config_manager)
  assert_eq(size_metrics.total_keys, 3)  // key1, key2, popular.key
  assert_eq(size_metrics.total_memory_usage > 0, true)
  assert_eq(size_metrics.average_value_size > 0, true)
}