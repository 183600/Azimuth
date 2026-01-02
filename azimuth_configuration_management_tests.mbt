// Azimuth Telemetry System - Configuration Management Tests
// This file contains test cases for configuration management, dynamic updates, and validation

// Test 1: Basic Configuration Operations
test "basic configuration operations" {
  // Test configuration creation
  let config = Configuration::new()
  
  // Test setting and getting string values
  Configuration::set_string(config, "string.key", "test_value")
  let string_value = Configuration::get_string(config, "string.key")
  match string_value {
    Some(value) => assert_eq(value, "test_value")
    None => assert_true(false)
  }
  
  // Test setting and getting int values
  Configuration::set_int(config, "int.key", 42)
  let int_value = Configuration::get_int(config, "int.key")
  match int_value {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  // Test setting and getting float values
  Configuration::set_float(config, "float.key", 3.14)
  let float_value = Configuration::get_float(config, "float.key")
  match float_value {
    Some(value) => assert_eq(value, 3.14)
    None => assert_true(false)
  }
  
  // Test setting and getting bool values
  Configuration::set_bool(config, "bool.key", true)
  let bool_value = Configuration::get_bool(config, "bool.key")
  match bool_value {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  // Test setting and getting array values
  Configuration::set_string_array(config, "array.key", ["value1", "value2", "value3"])
  let array_value = Configuration::get_string_array(config, "array.key")
  match array_value {
    Some(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "value1")
      assert_eq(values[1], "value2")
      assert_eq(values[2], "value3")
    }
    None => assert_true(false)
  }
  
  // Test getting non-existent keys
  let non_existent = Configuration::get_string(config, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Configuration Persistence and Loading
test "configuration persistence and loading" {
  // Create a configuration with various settings
  let config = Configuration::new()
  
  Configuration::set_string(config, "service.name", "telemetry_service")
  Configuration::set_string(config, "service.version", "1.0.0")
  Configuration::set_int(config, "service.port", 8080)
  Configuration::set_bool(config, "service.enabled", true)
  Configuration::set_float(config, "sampling.probability", 0.1)
  
  // Serialize configuration to JSON
  let json_config = Configuration::to_json(config)
  assert_true(json_config.contains("telemetry_service"))
  assert_true(json_config.contains("1.0.0"))
  assert_true(json_config.contains("8080"))
  
  // Deserialize configuration from JSON
  let loaded_config = Configuration::from_json(json_config)
  match loaded_config {
    Some(loaded) => {
      // Verify loaded values
      let service_name = Configuration::get_string(loaded, "service.name")
      match service_name {
        Some(name) => assert_eq(name, "telemetry_service")
        None => assert_true(false)
      }
      
      let service_version = Configuration::get_string(loaded, "service.version")
      match service_version {
        Some(version) => assert_eq(version, "1.0.0")
        None => assert_true(false)
      }
      
      let service_port = Configuration::get_int(loaded, "service.port")
      match service_port {
        Some(port) => assert_eq(port, 8080)
        None => assert_true(false)
      }
      
      let service_enabled = Configuration::get_bool(loaded, "service.enabled")
      match service_enabled {
        Some(enabled) => assert_true(enabled)
        None => assert_true(false)
      }
      
      let sampling_prob = Configuration::get_float(loaded, "sampling.probability")
      match sampling_prob {
        Some(prob) => assert_eq(prob, 0.1)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test configuration file persistence
  let file_path = "/tmp/test_config.json"
  let save_result = Configuration::save_to_file(config, file_path)
  assert_true(save_result)
  
  // Load configuration from file
  let file_loaded_config = Configuration::load_from_file(file_path)
  match file_loaded_config {
    Some(loaded) => {
      let service_name = Configuration::get_string(loaded, "service.name")
      match service_name {
        Some(name) => assert_eq(name, "telemetry_service")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: Configuration Validation
test "configuration validation" {
  // Create a configuration schema
  let schema = ConfigurationSchema::new()
  
  // Define validation rules
  ConfigurationSchema::add_string_rule(schema, "service.name", true, None, None) // Required
  ConfigurationSchema::add_string_rule(schema, "service.version", true, Some("^\\d+\\.\\d+\\.\\d+$"), None) // Required, pattern match
  ConfigurationSchema::add_int_rule(schema, "service.port", true, Some(1024), Some(65535)) // Required, range 1024-65535
  ConfigurationSchema::add_bool_rule(schema, "service.enabled", false, None) // Optional
  ConfigurationSchema::add_float_rule(schema, "sampling.probability", false, Some(0.0), Some(1.0)) // Optional, range 0.0-1.0
  
  // Test valid configuration
  let valid_config = Configuration::new()
  Configuration::set_string(valid_config, "service.name", "test_service")
  Configuration::set_string(valid_config, "service.version", "1.0.0")
  Configuration::set_int(valid_config, "service.port", 8080)
  Configuration::set_bool(valid_config, "service.enabled", true)
  Configuration::set_float(valid_config, "sampling.probability", 0.1)
  
  let validation_result = Configuration::validate(valid_config, schema)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test invalid configuration
  let invalid_config = Configuration::new()
  Configuration::set_string(invalid_config, "service.name", "") // Empty name
  Configuration::set_string(invalid_config, "service.version", "1.0") // Invalid version format
  Configuration::set_int(invalid_config, "service.port", 80) // Port out of range
  // Missing required field: service.name is empty, service.version format is wrong, service.port is out of range
  
  let invalid_validation_result = Configuration::validate(invalid_config, schema)
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // Test missing required fields
  let incomplete_config = Configuration::new()
  Configuration::set_string(incomplete_config, "service.name", "test_service")
  // Missing service.version and service.port
  
  let incomplete_validation_result = Configuration::validate(incomplete_config, schema)
  assert_false(incomplete_validation_result.is_valid)
  assert_true(incomplete_validation_result.errors.length() >= 2) // At least 2 missing required fields
}

// Test 4: Dynamic Configuration Updates
test "dynamic configuration updates" {
  // Create a configuration with change listeners
  let config = Configuration::new()
  
  // Set up initial values
  Configuration::set_string(config, "dynamic.key", "initial_value")
  Configuration::set_int(config, "dynamic.counter", 0)
  
  // Add change listener
  let mut change_count = 0
  let listener = fn(key, old_value, new_value) {
    change_count = change_count + 1
  }
  
  Configuration::add_change_listener(config, "dynamic.key", listener)
  
  // Update configuration and verify listener is called
  Configuration::set_string(config, "dynamic.key", "updated_value")
  assert_eq(change_count, 1)
  
  // Update with same value should not trigger listener
  Configuration::set_string(config, "dynamic.key", "updated_value")
  assert_eq(change_count, 1) // Should still be 1, not 2
  
  // Update with different value should trigger listener again
  Configuration::set_string(config, "dynamic.key", "another_value")
  assert_eq(change_count, 2)
  
  // Test batch updates
  let updates = [
    ("batch.key1", StringValue("value1")),
    ("batch.key2", IntValue(42)),
    ("batch.key3", BoolValue(true))
  ]
  
  let batch_change_count = 0
  let batch_listener = fn(key, old_value, new_value) {
    change_count = change_count + 1
  }
  
  Configuration::add_change_listener(config, "batch.key1", batch_listener)
  Configuration::add_change_listener(config, "batch.key2", batch_listener)
  Configuration::add_change_listener(config, "batch.key3", batch_listener)
  
  Configuration::update_batch(config, updates)
  assert_eq(change_count, 5) // 2 from previous tests + 3 from batch update
  
  // Test configuration reload
  let new_config = Configuration::new()
  Configuration::set_string(new_config, "dynamic.key", "reloaded_value")
  Configuration::set_int(new_config, "dynamic.counter", 10)
  
  let reload_result = Configuration::reload(config, new_config)
  assert_true(reload_result)
  
  let reloaded_value = Configuration::get_string(config, "dynamic.key")
  match reloaded_value {
    Some(value) => assert_eq(value, "reloaded_value")
    None => assert_true(false)
  }
  
  let reloaded_counter = Configuration::get_int(config, "dynamic.counter")
  match reloaded_counter {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
}

// Test 5: Configuration Hierarchies and Inheritance
test "configuration hierarchies and inheritance" {
  // Create parent configuration
  let parent_config = Configuration::new()
  Configuration::set_string(parent_config, "parent.key", "parent_value")
  Configuration::set_string(parent_config, "inherited.key", "inherited_value")
  Configuration::set_int(parent_config, "override.key", 100)
  
  // Create child configuration
  let child_config = Configuration::with_parent(parent_config)
  Configuration::set_string(child_config, "child.key", "child_value")
  Configuration::set_int(child_config, "override.key", 200) // Override parent value
  
  // Test child can access its own values
  let child_value = Configuration::get_string(child_config, "child.key")
  match child_value {
    Some(value) => assert_eq(value, "child_value")
    None => assert_true(false)
  }
  
  // Test child can inherit parent values
  let inherited_value = Configuration::get_string(child_config, "inherited.key")
  match inherited_value {
    Some(value) => assert_eq(value, "inherited_value")
    None => assert_true(false)
  }
  
  // Test child overrides parent values
  let override_value = Configuration::get_int(child_config, "override.key")
  match override_value {
    Some(value) => assert_eq(value, 200) // Child's value, not parent's
    None => assert_true(false)
  }
  
  // Test parent is unaffected by child changes
  let parent_override_value = Configuration::get_int(parent_config, "override.key")
  match parent_override_value {
    Some(value) => assert_eq(value, 100) // Parent's original value
    None => assert_true(false)
  }
  
  // Test configuration hierarchy with multiple levels
  let grandchild_config = Configuration::with_parent(child_config)
  Configuration::set_string(grandchild_config, "grandchild.key", "grandchild_value")
  
  let grandchild_inherited = Configuration::get_string(grandchild_config, "inherited.key")
  match grandchild_inherited {
    Some(value) => assert_eq(value, "inherited_value") // From parent
    None => assert_true(false)
  }
  
  let grandchild_child = Configuration::get_string(grandchild_config, "child.key")
  match grandchild_child {
    Some(value) => assert_eq(value, "child_value") // From parent
    None => assert_true(false)
  }
  
  let grandchild_parent = Configuration::get_string(grandchild_config, "parent.key")
  match grandchild_parent {
    Some(value) => assert_eq(value, "parent_value") // From grandparent
    None => assert_true(false)
  }
}

// Test 6: Environment Variable Integration
test "environment variable integration" {
  // Create configuration with environment variable support
  let config = Configuration::new()
  
  // Set up environment variable mappings
  Configuration::map_env_variable(config, "env.service.name", "SERVICE_NAME")
  Configuration::map_env_variable(config, "env.service.port", "SERVICE_PORT")
  Configuration::map_env_variable(config, "env.debug.enabled", "DEBUG_ENABLED")
  
  // Load configuration from environment variables
  let load_result = Configuration::load_from_environment(config)
  assert_true(load_result)
  
  // Test getting values (these would come from actual environment in a real scenario)
  // For this test, we'll simulate environment variable values
  
  // Simulate environment variable values
  Configuration::set_string(config, "env.service.name", "env_service")
  Configuration::set_int(config, "env.service.port", 9090)
  Configuration::set_bool(config, "env.debug.enabled", true)
  
  // Verify loaded values
  let service_name = Configuration::get_string(config, "env.service.name")
  match service_name {
    Some(name) => assert_eq(name, "env_service")
    None => assert_true(false)
  }
  
  let service_port = Configuration::get_int(config, "env.service.port")
  match service_port {
    Some(port) => assert_eq(port, 9090)
    None => assert_true(false)
  }
  
  let debug_enabled = Configuration::get_bool(config, "env.debug.enabled")
  match debug_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  // Test environment variable precedence
  // Set a value directly, then load from environment to test precedence
  Configuration::set_string(config, "precedence.test", "direct_value")
  
  // In a real scenario, if PRECEDENCE_TEST env var was set, it would override the direct value
  // For this test, we'll simulate by setting the env value directly
  Configuration::set_string(config, "precedence.test", "env_value") // Simulate env override
  
  let precedence_value = Configuration::get_string(config, "precedence.test")
  match precedence_value {
    Some(value) => assert_eq(value, "env_value") // Environment should take precedence
    None => assert_true(false)
  }
}

// Test 7: Configuration Profiles and Variants
test "configuration profiles and variants" {
  // Create base configuration
  let base_config = Configuration::new()
  Configuration::set_string(base_config, "service.name", "base_service")
  Configuration::set_int(base_config, "service.port", 8080)
  Configuration::set_bool(base_config, "debug.enabled", false)
  Configuration::set_string(base_config, "log.level", "INFO")
  
  // Create development profile
  let dev_profile = Configuration::with_parent(base_config)
  Configuration::set_string(dev_profile, "service.name", "dev_service")
  Configuration::set_int(dev_profile, "service.port", 3000)
  Configuration::set_bool(dev_profile, "debug.enabled", true)
  Configuration::set_string(dev_profile, "log.level", "DEBUG")
  
  // Create production profile
  let prod_profile = Configuration::with_parent(base_config)
  Configuration::set_string(prod_profile, "service.name", "prod_service")
  Configuration::set_int(prod_profile, "service.port", 80)
  Configuration::set_bool(prod_profile, "debug.enabled", false)
  Configuration::set_string(prod_profile, "log.level", "WARN")
  
  // Create profile manager
  let profile_manager = ProfileManager::new()
  ProfileManager::add_profile(profile_manager, "base", base_config)
  ProfileManager::add_profile(profile_manager, "development", dev_profile)
  ProfileManager::add_profile(profile_manager, "production", prod_profile)
  
  // Test profile switching
  let active_dev = ProfileManager::activate_profile(profile_manager, "development")
  match active_dev {
    Some(dev_config) => {
      let service_name = Configuration::get_string(dev_config, "service.name")
      match service_name {
        Some(name) => assert_eq(name, "dev_service")
        None => assert_true(false)
      }
      
      let debug_enabled = Configuration::get_bool(dev_config, "debug.enabled")
      match debug_enabled {
        Some(enabled) => assert_true(enabled)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let active_prod = ProfileManager::activate_profile(profile_manager, "production")
  match active_prod {
    Some(prod_config) => {
      let service_name = Configuration::get_string(prod_config, "service.name")
      match service_name {
        Some(name) => assert_eq(name, "prod_service")
        None => assert_true(false)
      }
      
      let debug_enabled = Configuration::get_bool(prod_config, "debug.enabled")
      match debug_enabled {
        Some(enabled) => assert_false(enabled)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test profile inheritance
  let active_base = ProfileManager::activate_profile(profile_manager, "base")
  match active_base {
    Some(base_active_config) => {
      let service_name = Configuration::get_string(base_active_config, "service.name")
      match service_name {
        Some(name) => assert_eq(name, "base_service")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 8: Configuration Security and Encryption
test "configuration security and encryption" {
  // Create secure configuration
  let secure_config = Configuration::new()
  
  // Set sensitive values
  Configuration::set_string(secure_config, "database.password", "secret_password")
  Configuration::set_string(secure_config, "api.key", "secret_api_key")
  Configuration::set_string(secure_config, "jwt.secret", "jwt_secret_key")
  
  // Mark fields as sensitive
  Configuration::mark_sensitive(secure_config, "database.password")
  Configuration::mark_sensitive(secure_config, "api.key")
  Configuration::mark_sensitive(secure_config, "jwt.secret")
  
  // Test that sensitive values are masked in serialization
  let json_config = Configuration::to_json(secure_config)
  assert_false(json_config.contains("secret_password"))
  assert_false(json_config.contains("secret_api_key"))
  assert_false(json_config.contains("jwt_secret_key"))
  assert_true(json_config.contains("database.password"))
  assert_true(json_config.contains("api.key"))
  assert_true(json_config.contains("jwt.secret"))
  
  // Test encryption of configuration
  let encryption_key = "test_encryption_key_123"
  let encrypted_config = Configuration::encrypt(secure_config, encryption_key)
  assert_true(encrypted_config.length() > 0)
  
  // Test decryption of configuration
  let decrypted_config = Configuration::decrypt(encrypted_config, encryption_key)
  match decrypted_config {
    Some(decrypted) => {
      // Verify that sensitive values are restored
      let password = Configuration::get_string(decrypted, "database.password")
      match password {
        Some(value) => assert_eq(value, "secret_password")
        None => assert_true(false)
      }
      
      let api_key = Configuration::get_string(decrypted, "api.key")
      match api_key {
        Some(value) => assert_eq(value, "secret_api_key")
        None => assert_true(false)
      }
      
      let jwt_secret = Configuration::get_string(decrypted, "jwt.secret")
      match jwt_secret {
        Some(value) => assert_eq(value, "jwt_secret_key")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test decryption with wrong key fails
  let wrong_key_decrypted = Configuration::decrypt(encrypted_config, "wrong_key")
  match wrong_key_decrypted {
    Some(_) => assert_true(false) // Should not succeed with wrong key
    None => assert_true(true)     // Expected to fail
  }
  
  // Test configuration access control
  let access_config = Configuration::new()
  Configuration::set_string(access_config, "public.key", "public_value")
  Configuration::set_string(access_config, "private.key", "private_value")
  
  // Set access permissions
  Configuration::set_permission(access_config, "public.key", "read")
  Configuration::set_permission(access_config, "private.key", "admin")
  
  // Test access control
  let public_access = Configuration::check_access(access_config, "public.key", "read")
  assert_true(public_access)
  
  let private_read_access = Configuration::check_access(access_config, "private.key", "read")
  assert_false(private_read_access) // Should not have read access
  
  let private_admin_access = Configuration::check_access(access_config, "private.key", "admin")
  assert_true(private_admin_access)
}