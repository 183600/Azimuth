// Azimuth Configuration Management Tests
// This file contains test cases for configuration management functionality

// Test 1: Basic Configuration Operations
test "basic configuration operations" {
  // Create configuration manager
  let config_manager = {
    configurations: {},
    defaults: {},
    validators: {}
  }
  
  // Define configuration functions
  let set_config = fn(manager: {configurations: {String: String}, defaults: {String: String}, validators: {String: (String) -> Bool}}, key: String, value: String) {
    let new_configs = manager.configurations
    new_configs[key] = value
    {
      configurations: new_configs,
      defaults: manager.defaults,
      validators: manager.validators
    }
  }
  
  let get_config = fn(manager: {configurations: {String: String}, defaults: {String: String}, validators: {String: (String) -> Bool}}, key: String) {
    match manager.configurations.get(key) {
      Some(value) => Some(value)
      None => {
        match manager.defaults.get(key) {
          Some(default_value) => Some(default_value)
          None => None
        }
      }
    }
  }
  
  let set_default = fn(manager: {configurations: {String: String}, defaults: {String: String}, validators: {String: (String) -> Bool}}, key: String, default_value: String) {
    let new_defaults = manager.defaults
    new_defaults[key] = default_value
    {
      configurations: manager.configurations,
      defaults: new_defaults,
      validators: manager.validators
    }
  }
  
  // Test setting and getting configuration
  let manager_with_config = set_config(config_manager, "service.name", "test_service")
  let service_name = get_config(manager_with_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "test_service")
    None => assert_true(false)
  }
  
  // Test getting non-existent configuration
  let non_existent = get_config(manager_with_config, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test setting and getting default
  let manager_with_default = set_default(manager_with_config, "service.version", "1.0.0")
  let default_version = get_config(manager_with_default, "service.version")
  match default_version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  
  // Test overriding default
  let manager_with_override = set_config(manager_with_default, "service.version", "2.0.0")
  let override_version = get_config(manager_with_override, "service.version")
  match override_version {
    Some(version) => assert_eq(version, "2.0.0")
    None => assert_true(false)
  }
}

// Test 2: Configuration Validation
test "configuration validation" {
  // Create configuration manager with validators
  let config_manager = {
    configurations: {},
    defaults: {},
    validators: {
      "service.port": fn(port: String) {
        match port.to_int() {
          Some(p) => p > 0 and p <= 65535
          None => false
        }
      },
      "service.timeout": fn(timeout: String) {
        match timeout.to_int() {
          Some(t) => t > 0
          None => false
        }
      },
      "log.level": fn(level: String) {
        level == "DEBUG" or level == "INFO" or level == "WARN" or level == "ERROR"
      }
    }
  }
  
  // Define validation functions
  let validate_config = fn(manager: {configurations: {String: String}, defaults: {String: String}, validators: {String: (String) -> Bool}}, key: String, value: String) {
    match manager.validators.get(key) {
      Some(validator) => validator(value)
      None => true  // No validator means always valid
    }
  }
  
  let set_config_with_validation = fn(manager: {configurations: {String: String}, defaults: {String: String}, validators: {String: (String) -> Bool}}, key: String, value: String) {
    if validate_config(manager, key, value) {
      let new_configs = manager.configurations
      new_configs[key] = value
      {
        configurations: new_configs,
        defaults: manager.defaults,
        validators: manager.validators
      }
    } else {
      manager  // Return unchanged manager if validation fails
    }
  }
  
  // Test valid configurations
  let valid_port = set_config_with_validation(config_manager, "service.port", "8080")
  assert_true(valid_port.configurations.contains("service.port"))
  assert_eq(valid_port.configurations["service.port"], "8080")
  
  let valid_timeout = set_config_with_validation(valid_port, "service.timeout", "5000")
  assert_true(valid_timeout.configurations.contains("service.timeout"))
  assert_eq(valid_timeout.configurations["service.timeout"], "5000")
  
  let valid_log_level = set_config_with_validation(valid_timeout, "log.level", "INFO")
  assert_true(valid_log_level.configurations.contains("log.level"))
  assert_eq(valid_log_level.configurations["log.level"], "INFO")
  
  // Test invalid configurations
  let invalid_port = set_config_with_validation(valid_log_level, "service.port", "70000")
  assert_false(invalid_port.configurations.contains("service.port"))  // Should not be set
  
  let invalid_timeout = set_config_with_validation(invalid_port, "service.timeout", "-100")
  assert_false(invalid_timeout.configurations.contains("service.timeout"))  // Should not be set
  
  let invalid_log_level = set_config_with_validation(invalid_timeout, "log.level", "INVALID")
  assert_false(invalid_log_level.configurations.contains("log.level"))  // Should not be set
  
  // Test configuration without validator
  let no_validator = set_config_with_validation(invalid_log_level, "new.config", "some_value")
  assert_true(no_validator.configurations.contains("new.config"))
  assert_eq(no_validator.configurations["new.config"], "some_value")
}

// Test 3: Configuration Hierarchy and Inheritance
test "configuration hierarchy and inheritance" {
  // Create hierarchical configuration
  let base_config = {
    "service.name": "base_service",
    "service.version": "1.0.0",
    "service.port": "8080",
    "log.level": "INFO",
    "tracing.enabled": "true"
  }
  
  let environment_config = {
    "service.port": "9090",
    "log.level": "DEBUG",
    "database.host": "db.example.com"
  }
  
  let instance_config = {
    "service.instance.id": "instance-123",
    "log.level": "WARN"
  }
  
  // Define configuration resolution function
  let resolve_config = fn(base: {String: String}, env: {String: String}, instance: {String: String}, key: String) {
    match instance.get(key) {
      Some(value) => Some(value)
      None => {
        match env.get(key) {
          Some(value) => Some(value)
          None => {
            match base.get(key) {
              Some(value) => Some(value)
              None => None
            }
          }
        }
      }
    }
  }
  
  // Test configuration resolution
  let service_name = resolve_config(base_config, environment_config, instance_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "base_service")  // From base
    None => assert_true(false)
  }
  
  let service_port = resolve_config(base_config, environment_config, instance_config, "service.port")
  match service_port {
    Some(port) => assert_eq(port, "9090")  // From environment (overrides base)
    None => assert_true(false)
  }
  
  let log_level = resolve_config(base_config, environment_config, instance_config, "log.level")
  match log_level {
    Some(level) => assert_eq(level, "WARN")  // From instance (overrides environment and base)
    None => assert_true(false)
  }
  
  let database_host = resolve_config(base_config, environment_config, instance_config, "database.host")
  match database_host {
    Some(host) => assert_eq(host, "db.example.com")  // From environment only
    None => assert_true(false)
  }
  
  let instance_id = resolve_config(base_config, environment_config, instance_config, "service.instance.id")
  match instance_id {
    Some(id) => assert_eq(id, "instance-123")  // From instance only
    None => assert_true(false)
  }
  
  let non_existent = resolve_config(base_config, environment_config, instance_config, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Dynamic Configuration Updates
test "dynamic configuration updates" {
  // Create configuration manager with update listeners
  let config_manager = {
    configurations: {},
    listeners: [],
    update_count: 0
  }
  
  // Define configuration update functions
  let add_listener = fn(manager: {configurations: {String: String}, listeners: Array[(String, String) -> ()], update_count: Int}, listener: (String, String) -> ()) {
    {
      configurations: manager.configurations,
      listeners: manager.listeners.push(listener),
      update_count: manager.update_count
    }
  }
  
  let update_config = fn(manager: {configurations: {String: String}, listeners: Array[(String, String) -> ()], update_count: Int}, key: String, value: String) {
    let new_configs = manager.configurations
    new_configs[key] = value
    
    // Notify listeners
    for listener in manager.listeners {
      listener(key, value)
    }
    
    {
      configurations: new_configs,
      listeners: manager.listeners,
      update_count: manager.update_count + 1
    }
  }
  
  // Create a listener that tracks updates
  let update_log = { mut entries: [] }
  let tracking_listener = fn(key: String, value: String) {
    update_log.entries = update_log.entries.push((key, value))
  }
  
  // Add listener
  let manager_with_listener = add_listener(config_manager, tracking_listener)
  
  // Update configurations
  let manager1 = update_config(manager_with_listener, "service.name", "dynamic_service")
  let manager2 = update_config(manager1, "service.version", "1.5.0")
  let manager3 = update_config(manager2, "service.port", "3000")
  
  // Verify updates were applied
  assert_eq(manager3.configurations["service.name"], "dynamic_service")
  assert_eq(manager3.configurations["service.version"], "1.5.0")
  assert_eq(manager3.configurations["service.port"], "3000")
  assert_eq(manager3.update_count, 3)
  
  // Verify listener was notified
  assert_eq(update_log.entries.length(), 3)
  assert_eq(update_log.entries[0], ("service.name", "dynamic_service"))
  assert_eq(update_log.entries[1], ("service.version", "1.5.0"))
  assert_eq(update_log.entries[2], ("service.port", "3000"))
  
  // Test updating existing configuration
  let manager4 = update_config(manager3, "service.name", "updated_service")
  assert_eq(manager4.configurations["service.name"], "updated_service")
  assert_eq(manager4.update_count, 4)
  assert_eq(update_log.entries.length(), 4)
  assert_eq(update_log.entries[3], ("service.name", "updated_service"))
}

// Test 5: Configuration Persistence and Loading
test "configuration persistence and loading" {
  // Create initial configuration
  let config = {
    "service.name": "persistent_service",
    "service.version": "2.0.0",
    "service.port": "8080",
    "database.host": "db.example.com",
    "database.port": "5432",
    "log.level": "INFO",
    "tracing.enabled": "true"
  }
  
  // Define serialization and deserialization functions
  let serialize_config = fn(config: {String: String}) {
    let mut serialized = ""
    let mut first = true
    
    for (key, value) in config {
      if not(first) {
        serialized = serialized + "\n"
      }
      first = false
      
      serialized = serialized + key + "=" + value
    }
    
    serialized
  }
  
  let deserialize_config = fn(serialized: String) {
    let lines = serialized.split("\n")
    let mut config = {}
    
    for line in lines {
      let parts = line.split("=")
      if parts.length() == 2 {
        config[parts[0]] = parts[1]
      }
    }
    
    config
  }
  
  // Serialize configuration
  let serialized_config = serialize_config(config)
  
  // Verify serialized configuration
  assert_true(serialized_config.contains("service.name=persistent_service"))
  assert_true(serialized_config.contains("service.version=2.0.0"))
  assert_true(serialized_config.contains("service.port=8080"))
  assert_true(serialized_config.contains("database.host=db.example.com"))
  assert_true(serialized_config.contains("database.port=5432"))
  assert_true(serialized_config.contains("log.level=INFO"))
  assert_true(serialized_config.contains("tracing.enabled=true"))
  
  // Deserialize configuration
  let deserialized_config = deserialize_config(serialized_config)
  
  // Verify deserialized configuration matches original
  assert_eq(deserialized_config["service.name"], "persistent_service")
  assert_eq(deserialized_config["service.version"], "2.0.0")
  assert_eq(deserialized_config["service.port"], "8080")
  assert_eq(deserialized_config["database.host"], "db.example.com")
  assert_eq(deserialized_config["database.port"], "5432")
  assert_eq(deserialized_config["log.level"], "INFO")
  assert_eq(deserialized_config["tracing.enabled"], "true")
  
  // Test with partial configuration
  let partial_serialized = "service.name=partial_service\nlog.level=DEBUG"
  let partial_config = deserialize_config(partial_serialized)
  
  assert_eq(partial_config["service.name"], "partial_service")
  assert_eq(partial_config["log.level"], "DEBUG")
  assert_false(partial_config.contains("service.version"))  // Not in partial config
}

// Test 6: Environment-Specific Configuration
test "environment-specific configuration" {
  // Create environment-specific configurations
  let base_config = {
    "service.name": "multi_env_service",
    "service.version": "1.0.0",
    "log.format": "json"
  }
  
  let development_config = {
    "log.level": "DEBUG",
    "tracing.enabled": "true",
    "database.host": "localhost",
    "database.port": "5432"
  }
  
  let staging_config = {
    "log.level": "INFO",
    "tracing.enabled": "true",
    "database.host": "staging.db.example.com",
    "database.port": "5432"
  }
  
  let production_config = {
    "log.level": "WARN",
    "tracing.enabled": "false",
    "database.host": "prod.db.example.com",
    "database.port": "5432"
  }
  
  // Define environment-specific configuration resolution
  let resolve_for_environment = fn(base: {String: String}, env: {String: String}, environment: String) {
    let mut result = {}
    
    // Add base configuration
    for (key, value) in base {
      result[key] = value
    }
    
    // Override with environment-specific configuration
    for (key, value) in env {
      result[key] = value
    }
    
    result
  }
  
  // Resolve for different environments
  let dev_config = resolve_for_environment(base_config, development_config, "development")
  let staging_config_resolved = resolve_for_environment(base_config, staging_config, "staging")
  let prod_config = resolve_for_environment(base_config, production_config, "production")
  
  // Verify development configuration
  assert_eq(dev_config["service.name"], "multi_env_service")  // From base
  assert_eq(dev_config["log.level"], "DEBUG")  // From development
  assert_eq(dev_config["tracing.enabled"], "true")  // From development
  assert_eq(dev_config["database.host"], "localhost")  // From development
  
  // Verify staging configuration
  assert_eq(staging_config_resolved["service.name"], "multi_env_service")  // From base
  assert_eq(staging_config_resolved["log.level"], "INFO")  // From staging
  assert_eq(staging_config_resolved["tracing.enabled"], "true")  // From staging
  assert_eq(staging_config_resolved["database.host"], "staging.db.example.com")  // From staging
  
  // Verify production configuration
  assert_eq(prod_config["service.name"], "multi_env_service")  // From base
  assert_eq(prod_config["log.level"], "WARN")  // From production
  assert_eq(prod_config["tracing.enabled"], "false")  // From production
  assert_eq(prod_config["database.host"], "prod.db.example.com")  // From production
  
  // Test environment detection
  let detect_environment = fn() {
    // In a real implementation, this would check environment variables
    // For this test, we'll simulate it
    "development"
  }
  
  let current_env = detect_environment()
  assert_eq(current_env, "development")
  
  // Resolve configuration for current environment
  let current_config = if current_env == "development" {
    dev_config
  } else if current_env == "staging" {
    staging_config_resolved
  } else if current_env == "production" {
    prod_config
  } else {
    base_config
  }
  
  assert_eq(current_config["log.level"], "DEBUG")
}

// Test 7: Configuration Templates and Substitution
test "configuration templates and substitution" {
  // Create configuration with templates
  let template_config = {
    "service.name": "template_service",
    "service.host": "${HOSTNAME}",
    "service.port": "${PORT:8080}",
    "service.url": "http://${service.host}:${service.port}/api",
    "database.url": "postgresql://${DB_USER:admin}:${DB_PASS:password}@${DB_HOST:localhost}:${DB_PORT:5432}/${DB_NAME:azimuth}",
    "log.file": "/var/log/${service.name}/${service.version}.log"
  }
  
  // Define substitution functions
  let substitute_variables = fn(config: {String: String}, variables: {String: String}) {
    let mut result = {}
    
    for (key, template) in config {
      let mut value = template
      
      // Find and replace variables
      let mut i = 0
      while i < value.length() {
        if value.substring(i, 2) == "${" {
          let end_pos = value.indexOf("}", i)
          if end_pos > 0 {
            let var_expr = value.substring(i + 2, end_pos - (i + 2))
            
            // Check for default value
            let var_parts = var_expr.split(":")
            let var_name = var_parts[0]
            let default_value = if var_parts.length() > 1 {
              var_parts[1]
            } else {
              ""
            }
            
            // Get variable value or default
            let var_value = match variables.get(var_name) {
              Some(v) => v
              None => default_value
            }
            
            // Replace variable expression with value
            value = value.substring(0, i) + var_value + value.substring(end_pos + 1, value.length() - (end_pos + 1))
          } else {
            i = i + 1
          }
        } else {
          i = i + 1
        }
      }
      
      result[key] = value
    }
    
    result
  }
  
  // Define variables for substitution
  let variables = {
    "HOSTNAME": "localhost",
    "PORT": "9090",
    "DB_USER": "azimuth_user",
    "DB_PASS": "secret_password",
    "DB_HOST": "db.example.com",
    "DB_PORT": "5432",
    "DB_NAME": "azimuth_db",
    "service.version": "2.1.0"
  }
  
  // Perform substitution
  let substituted_config = substitute_variables(template_config, variables)
  
  // Verify substitution results
  assert_eq(substituted_config["service.name"], "template_service")  // No substitution
  assert_eq(substituted_config["service.host"], "localhost")  // ${HOSTNAME} -> localhost
  assert_eq(substituted_config["service.port"], "9090")  // ${PORT:8080} -> 9090 (variable provided)
  assert_eq(substituted_config["service.url"], "http://localhost:9090/api")  // Complex substitution
  assert_eq(substituted_config["database.url"], "postgresql://azimuth_user:secret_password@db.example.com:5432/azimuth_db")  // Complex substitution with defaults
  assert_eq(substituted_config["log.file"], "/var/log/template_service/2.1.0.log")  // Cross-reference substitution
  
  // Test with missing variables (should use defaults)
  let minimal_variables = {
    "HOSTNAME": "localhost"
  }
  
  let minimal_config = substitute_variables(template_config, minimal_variables)
  assert_eq(minimal_config["service.host"], "localhost")  // Variable provided
  assert_eq(minimal_config["service.port"], "8080")  // Default value used
  assert_eq(minimal_config["service.url"], "http://localhost:8080/api")  // Uses substituted values
  assert_eq(minimal_config["database.url"], "postgresql://admin:password@localhost:5432/azimuth")  // All defaults used
}

// Test 8: Configuration Security and Encryption
test "configuration security and encryption" {
  // Create sensitive configuration
  let sensitive_config = {
    "database.password": "secret_password",
    "api.key": "api_key_12345",
    "jwt.secret": "jwt_secret_key",
    "encryption.key": "encryption_key_value",
    "service.name": "secure_service"  // Non-sensitive
  }
  
  // Define encryption/decryption functions (simplified)
  let encrypt_value = fn(value: String, key: String) {
    // In a real implementation, this would use actual encryption
    // For this test, we'll simulate it with base64 encoding
    let encrypted = "encrypted:" + value + ":" + key
    encrypted
  }
  
  let decrypt_value = fn(encrypted_value: String, key: String) {
    // In a real implementation, this would use actual decryption
    // For this test, we'll simulate it
    if encrypted_value.starts_with("encrypted:") {
      let parts = encrypted_value.split(":")
      if parts.length() == 3 and parts[2] == key {
        parts[1]
      } else {
        encrypted_value
      }
    } else {
      encrypted_value
    }
  }
  
  // Define sensitive keys
  let sensitive_keys = ["database.password", "api.key", "jwt.secret", "encryption.key"]
  let encryption_key = "encryption_key_12345"
  
  // Encrypt sensitive values
  let encrypt_config = fn(config: {String: String}, keys: Array[String], enc_key: String) {
    let mut result = {}
    
    for (key, value) in config {
      if keys.contains(key) {
        result[key] = encrypt_value(value, enc_key)
      } else {
        result[key] = value
      }
    }
    
    result
  }
  
  // Decrypt sensitive values
  let decrypt_config = fn(config: {String: String}, keys: Array[String], enc_key: String) {
    let mut result = {}
    
    for (key, value) in config {
      if keys.contains(key) {
        result[key] = decrypt_value(value, enc_key)
      } else {
        result[key] = value
      }
    }
    
    result
  }
  
  // Encrypt configuration
  let encrypted_config = encrypt_config(sensitive_config, sensitive_keys, encryption_key)
  
  // Verify encryption
  assert_true(encrypted_config["database.password"].starts_with("encrypted:"))
  assert_true(encrypted_config["api.key"].starts_with("encrypted:"))
  assert_true(encrypted_config["jwt.secret"].starts_with("encrypted:"))
  assert_true(encrypted_config["encryption.key"].starts_with("encrypted:"))
  assert_eq(encrypted_config["service.name"], "secure_service")  // Non-sensitive value unchanged
  
  // Decrypt configuration
  let decrypted_config = decrypt_config(encrypted_config, sensitive_keys, encryption_key)
  
  // Verify decryption
  assert_eq(decrypted_config["database.password"], "secret_password")
  assert_eq(decrypted_config["api.key"], "api_key_12345")
  assert_eq(decrypted_config["jwt.secret"], "jwt_secret_key")
  assert_eq(decrypted_config["encryption.key"], "encryption_key_value")
  assert_eq(decrypted_config["service.name"], "secure_service")
  
  // Test with wrong encryption key
  let wrong_key_config = decrypt_config(encrypted_config, sensitive_keys, "wrong_key")
  assert_neq(wrong_key_config["database.password"], "secret_password")  // Should not decrypt correctly
}

// Test 9: Configuration Validation with Complex Rules
test "configuration validation with complex rules" {
  // Create configuration manager with complex validation rules
  let config_manager = {
    configurations: {},
    validators: {
      "database.url": fn(url: String) {
        url.starts_with("postgresql://") or url.starts_with("mysql://") or url.starts_with("sqlite://")
      },
      "service.endpoints": fn(endpoints: String) {
        // Validate comma-separated list of endpoints
        let parts = endpoints.split(",")
        if parts.length() == 0 {
          false
        } else {
          let mut all_valid = true
          for part in parts {
            if not(part.contains(":")) {
              all_valid = false
            }
          }
          all_valid
        }
      },
      "tls.certificate.path": fn(path: String) {
        path.ends_with(".crt") or path.ends_with(".pem")
      },
      "tls.private.key.path": fn(path: String) {
        path.ends_with(".key") or path.ends_with(".pem")
      },
      "cors.allowed.origins": fn(origins: String) {
        // Validate comma-separated list of origins
        let parts = origins.split(",")
        if parts.length() == 0 {
          false
        } else {
          let mut all_valid = true
          for part in parts {
            let trimmed = part.trim()
            if not(trimmed.starts_with("http://") or trimmed.starts_with("https://") or trimmed == "*") {
              all_valid = false
            }
          }
          all_valid
        }
      }
    }
  }
  
  // Define validation function
  let validate_all = fn(manager: {configurations: {String: String}, validators: {String: (String) -> Bool}}) {
    let mut errors = []
    
    for (key, validator) in manager.validators {
      match manager.configurations.get(key) {
        Some(value) => {
          if not(validator(value)) {
            errors = errors.push("Invalid value for " + key + ": " + value)
          }
        }
        None => {}  // Skip validation for missing keys
      }
    }
    
    errors
  }
  
  // Set valid configurations
  let valid_configs = {
    "database.url": "postgresql://user:pass@localhost:5432/db",
    "service.endpoints": "localhost:8080,localhost:8081,localhost:8082",
    "tls.certificate.path": "/etc/ssl/certs/server.crt",
    "tls.private.key.path": "/etc/ssl/private/server.key",
    "cors.allowed.origins": "https://example.com,https://app.example.com"
  }
  
  let manager_with_valid = {
    configurations: valid_configs,
    validators: config_manager.validators
  }
  
  let valid_errors = validate_all(manager_with_valid)
  assert_eq(valid_errors.length(), 0)  // No validation errors
  
  // Set invalid configurations
  let invalid_configs = {
    "database.url": "ftp://invalid.protocol",
    "service.endpoints": "localhost8080,localhost:8081",  // Missing colon
    "tls.certificate.path": "/etc/ssl/certs/server.txt",  // Invalid extension
    "tls.private.key.path": "/etc/ssl/private/server.txt",  // Invalid extension
    "cors.allowed.origins": "ftp://example.com,invalid-origin"  // Invalid protocols
  }
  
  let manager_with_invalid = {
    configurations: invalid_configs,
    validators: config_manager.validators
  }
  
  let invalid_errors = validate_all(manager_with_invalid)
  assert_eq(invalid_errors.length(), 5)  // All validations should fail
  
  // Verify specific error messages
  assert_true(invalid_errors[0].contains("database.url"))
  assert_true(invalid_errors[1].contains("service.endpoints"))
  assert_true(invalid_errors[2].contains("tls.certificate.path"))
  assert_true(invalid_errors[3].contains("tls.private.key.path"))
  assert_true(invalid_errors[4].contains("cors.allowed.origins"))
}

// Test 10: Configuration Hot Reload
test "configuration hot reload" {
  // Create configuration manager with hot reload capability
  let config_manager = {
    configurations: {
      "service.name": "hot_reload_service",
      "service.version": "1.0.0",
      "log.level": "INFO"
    },
    file_path: "/etc/azimuth/config.yaml",
    last_modified: 1640995200,
    reload_listeners: []
  }
  
  // Define hot reload functions
  let add_reload_listener = fn(manager: {configurations: {String: String}, file_path: String, last_modified: Int, reload_listeners: Array[() -> ()]}, listener: () -> ()) {
    {
      configurations: manager.configurations,
      file_path: manager.file_path,
      last_modified: manager.last_modified,
      reload_listeners: manager.reload_listeners.push(listener)
    }
  }
  
  let check_for_reload = fn(manager: {configurations: {String: String}, file_path: String, last_modified: Int, reload_listeners: Array[() -> ()]}) {
    // In a real implementation, this would check the file modification time
    // For this test, we'll simulate it with a time check
    let current_time = 1640995300  // 100 seconds later
    
    if current_time > manager.last_modified {
      // Load new configuration
      let new_config = {
        "service.name": "hot_reload_service",
        "service.version": "2.0.0",  // Updated
        "log.level": "DEBUG",  // Updated
        "new.feature.enabled": "true"  // New config
      }
      
      // Notify listeners
      for listener in manager.reload_listeners {
        listener()
      }
      
      {
        configurations: new_config,
        file_path: manager.file_path,
        last_modified: current_time,
        reload_listeners: manager.reload_listeners
      }
    } else {
      manager  // No reload needed
    }
  }
  
  // Create a reload listener
  let reload_count = { mut count: 0 }
  let reload_listener = fn() {
    reload_count.count = reload_count.count + 1
  }
  
  // Add reload listener
  let manager_with_listener = add_reload_listener(config_manager, reload_listener)
  
  // Check for reload (should trigger reload)
  let reloaded_manager = check_for_reload(manager_with_listener)
  
  // Verify configuration was updated
  assert_eq(reloaded_manager.configurations["service.version"], "2.0.0")
  assert_eq(reloaded_manager.configurations["log.level"], "DEBUG")
  assert_eq(reloaded_manager.configurations["new.feature.enabled"], "true")
  assert_eq(reloaded_manager.last_modified, 1640995300)
  
  // Verify listener was notified
  assert_eq(reload_count.count, 1)
  
  // Check for reload again (should not trigger reload)
  let not_reloaded_manager = check_for_reload(reloaded_manager)
  
  // Verify configuration unchanged
  assert_eq(not_reloaded_manager.configurations["service.version"], "2.0.0")
  assert_eq(not_reloaded_manager.last_modified, 1640995300)
  
  // Verify listener was not notified again
  assert_eq(reload_count.count, 1)
}