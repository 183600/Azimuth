// Azimuth Configuration Management Tests
// This file contains test cases for configuration management

// Test 1: Basic Configuration Loading
test "basic configuration loading" {
  // Simulate configuration structure
  let default_config = {
    service_name: "azimuth-service",
    service_version: "1.0.0",
    environment: "development",
    log_level: "info",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "http://localhost:8080"
  }
  
  // Test configuration validation
  let validate_config = fn(config) {
    let errors = []
    
    if config.service_name.length() == 0 {
      errors = errors.push("Service name cannot be empty")
    }
    
    if config.service_version.length() == 0 {
      errors = errors.push("Service version cannot be empty")
    }
    
    let valid_environments = ["development", "staging", "production"]
    if !valid_environments.contains(config.environment) {
      errors = errors.push("Invalid environment: " + config.environment)
    }
    
    let valid_log_levels = ["debug", "info", "warn", "error"]
    if !valid_log_levels.contains(config.log_level) {
      errors = errors.push("Invalid log level: " + config.log_level)
    }
    
    if !config.endpoint.starts_with("http://") && !config.endpoint.starts_with("https://") {
      errors = errors.push("Endpoint must start with http:// or https://")
    }
    
    errors
  }
  
  // Test valid configuration
  let validation_errors = validate_config(default_config)
  assert_eq(validation_errors.length(), 0)
  
  // Test invalid configurations
  let invalid_config1 = { 
    service_name: "",  // Empty service name
    service_version: "1.0.0",
    environment: "development",
    log_level: "info",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "http://localhost:8080"
  }
  
  let errors1 = validate_config(invalid_config1)
  assert_eq(errors1.length(), 1)
  assert_true(errors1[0].contains("Service name cannot be empty"))
  
  let invalid_config2 = { 
    service_name: "test-service",
    service_version: "1.0.0",
    environment: "invalid",  // Invalid environment
    log_level: "info",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "http://localhost:8080"
  }
  
  let errors2 = validate_config(invalid_config2)
  assert_eq(errors2.length(), 1)
  assert_true(errors2[0].contains("Invalid environment"))
  
  let invalid_config3 = { 
    service_name: "test-service",
    service_version: "1.0.0",
    environment: "development",
    log_level: "invalid",  // Invalid log level
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "ftp://localhost:8080"  // Invalid endpoint protocol
  }
  
  let errors3 = validate_config(invalid_config3)
  assert_eq(errors3.length(), 2)
  assert_true(errors3[0].contains("Invalid log level"))
  assert_true(errors3[1].contains("Endpoint must start with http:// or https://"))
}

// Test 2: Configuration Merging and Override
test "configuration merging and override" {
  // Simulate configuration merging
  let merge_configs = fn(base_config, override_config) {
    {
      service_name: match override_config.service_name {
        None => base_config.service_name
        Some(name) => name
      },
      service_version: match override_config.service_version {
        None => base_config.service_version
        Some(version) => version
      },
      environment: match override_config.environment {
        None => base_config.environment
        Some(env) => env
      },
      log_level: match override_config.log_level {
        None => base_config.log_level
        Some(level) => level
      },
      metrics_enabled: match override_config.metrics_enabled {
        None => base_config.metrics_enabled
        Some(enabled) => enabled
      },
      tracing_enabled: match override_config.tracing_enabled {
        None => base_config.tracing_enabled
        Some(enabled) => enabled
      },
      endpoint: match override_config.endpoint {
        None => base_config.endpoint
        Some(url) => url
      }
    }
  }
  
  let base_config = {
    service_name: "azimuth-service",
    service_version: "1.0.0",
    environment: "development",
    log_level: "info",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "http://localhost:8080"
  }
  
  // Test partial override
  let override_config1 = {
    service_name: Some("production-service"),
    environment: Some("production"),
    log_level: None,
    metrics_enabled: Some(false),
    tracing_enabled: None,
    endpoint: None,
    service_version: None
  }
  
  let merged_config1 = merge_configs(base_config, override_config1)
  assert_eq(merged_config1.service_name, "production-service")
  assert_eq(merged_config1.service_version, "1.0.0")  // From base
  assert_eq(merged_config1.environment, "production")
  assert_eq(merged_config1.log_level, "info")  // From base
  assert_eq(merged_config1.metrics_enabled, false)
  assert_eq(merged_config1.tracing_enabled, true)  // From base
  assert_eq(merged_config1.endpoint, "http://localhost:8080")  // From base
  
  // Test full override
  let override_config2 = {
    service_name: Some("full-override-service"),
    service_version: Some("2.0.0"),
    environment: Some("staging"),
    log_level: Some("debug"),
    metrics_enabled: Some(false),
    tracing_enabled: Some(false),
    endpoint: Some("https://staging.example.com")
  }
  
  let merged_config2 = merge_configs(base_config, override_config2)
  assert_eq(merged_config2.service_name, "full-override-service")
  assert_eq(merged_config2.service_version, "2.0.0")
  assert_eq(merged_config2.environment, "staging")
  assert_eq(merged_config2.log_level, "debug")
  assert_eq(merged_config2.metrics_enabled, false)
  assert_eq(merged_config2.tracing_enabled, false)
  assert_eq(merged_config2.endpoint, "https://staging.example.com")
  
  // Test no override
  let override_config3 = {
    service_name: None,
    service_version: None,
    environment: None,
    log_level: None,
    metrics_enabled: None,
    tracing_enabled: None,
    endpoint: None
  }
  
  let merged_config3 = merge_configs(base_config, override_config3)
  assert_eq(merged_config3.service_name, base_config.service_name)
  assert_eq(merged_config3.service_version, base_config.service_version)
  assert_eq(merged_config3.environment, base_config.environment)
  assert_eq(merged_config3.log_level, base_config.log_level)
  assert_eq(merged_config3.metrics_enabled, base_config.metrics_enabled)
  assert_eq(merged_config3.tracing_enabled, base_config.tracing_enabled)
  assert_eq(merged_config3.endpoint, base_config.endpoint)
}

// Test 3: Environment-Specific Configuration
test "environment-specific configuration" {
  // Define environment-specific configurations
  let development_config = {
    service_name: "azimuth-dev",
    service_version: "1.0.0-dev",
    environment: "development",
    log_level: "debug",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "http://localhost:8080",
    debug_mode: true,
    hot_reload: true
  }
  
  let staging_config = {
    service_name: "azimuth-staging",
    service_version: "1.0.0-staging",
    environment: "staging",
    log_level: "info",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "https://staging.azimuth.com",
    debug_mode: false,
    hot_reload: false
  }
  
  let production_config = {
    service_name: "azimuth-prod",
    service_version: "1.0.0",
    environment: "production",
    log_level: "warn",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "https://azimuth.com",
    debug_mode: false,
    hot_reload: false,
    rate_limiting: true,
    circuit_breaker: true
  }
  
  // Simulate environment configuration loader
  let load_config_for_environment = fn(environment) {
    match environment {
      "development" => development_config
      "staging" => staging_config
      "production" => production_config
      _ => {
        // Default to development for unknown environments
        development_config
      }
    }
  }
  
  // Test loading configurations for different environments
  let dev_config = load_config_for_environment("development")
  assert_eq(dev_config.environment, "development")
  assert_eq(dev_config.log_level, "debug")
  assert_true(dev_config.debug_mode)
  assert_true(dev_config.hot_reload)
  
  let staging_config_loaded = load_config_for_environment("staging")
  assert_eq(staging_config_loaded.environment, "staging")
  assert_eq(staging_config_loaded.log_level, "info")
  assert_false(staging_config_loaded.debug_mode)
  assert_false(staging_config_loaded.hot_reload)
  
  let prod_config = load_config_for_environment("production")
  assert_eq(prod_config.environment, "production")
  assert_eq(prod_config.log_level, "warn")
  assert_false(prod_config.debug_mode)
  assert_false(prod_config.hot_reload)
  assert_true(prod_config.rate_limiting)
  assert_true(prod_config.circuit_breaker)
  
  // Test unknown environment
  let unknown_config = load_config_for_environment("unknown")
  assert_eq(unknown_config.environment, "development")  // Should default to development
  assert_eq(unknown_config.log_level, "debug")
  assert_true(unknown_config.debug_mode)
}

// Test 4: Configuration Schema Validation
test "configuration schema validation" {
  // Define configuration schema
  let config_schema = {
    required_fields: ["service_name", "service_version", "environment"],
    field_types = {
      service_name: "string",
      service_version: "string",
      environment: "string",
      log_level: "string",
      metrics_enabled: "boolean",
      tracing_enabled: "boolean",
      endpoint: "string"
    },
    field_constraints = {
      service_name: { min_length: 1, max_length: 100 },
      service_version: { min_length: 1, max_length: 20 },
      environment: { allowed_values: ["development", "staging", "production"] },
      log_level: { allowed_values: ["debug", "info", "warn", "error"] },
      endpoint: { pattern: "^https?://.+" }
    }
  }
  
  // Simulate schema validation
  let validate_against_schema = fn(config, schema) {
    let errors = []
    
    // Check required fields
    for field in schema.required_fields {
      if !config.contains(field) {
        errors = errors.push("Required field missing: " + field)
      }
    }
    
    // Check field types
    for (field, expected_type) in schema.field_types {
      if config.contains(field) {
        let actual_type = match expected_type {
          "string" => if config[field].length() > 0 { "string" } else { "unknown" }
          "boolean" => if config[field] == true || config[field] == false { "boolean" } else { "unknown" }
          _ => "unknown"
        }
        
        if actual_type != expected_type {
          errors = errors.push("Invalid type for field " + field + ": expected " + expected_type + ", got " + actual_type)
        }
      }
    }
    
    // Check field constraints
    for (field, constraints) in schema.field_constraints {
      if config.contains(field) {
        let value = config[field]
        
        // Check min/max length for strings
        if constraints.contains("min_length") && constraints.contains("max_length") {
          if value.length() < constraints["min_length"] || value.length() > constraints["max_length"] {
            errors = errors.push("Field " + field + " length must be between " + constraints["min_length"].to_string() + " and " + constraints["max_length"].to_string())
          }
        }
        
        // Check allowed values
        if constraints.contains("allowed_values") {
          let allowed_values = constraints["allowed_values"]
          if !allowed_values.contains(value) {
            errors = errors.push("Field " + field + " must be one of: " + allowed_values.join(", "))
          }
        }
        
        // Check pattern
        if constraints.contains("pattern") {
          let pattern = constraints["pattern"]
          // Simplified pattern matching - in real implementation would use regex
          if pattern == "^https?://.+" && !value.starts_with("http") {
            errors = errors.push("Field " + field + " must match pattern: " + pattern)
          }
        }
      }
    }
    
    errors
  }
  
  // Test valid configuration
  let valid_config = {
    service_name: "test-service",
    service_version: "1.0.0",
    environment: "development",
    log_level: "debug",
    metrics_enabled: true,
    tracing_enabled: false,
    endpoint: "https://example.com"
  }
  
  let valid_errors = validate_against_schema(valid_config, config_schema)
  assert_eq(valid_errors.length(), 0)
  
  // Test missing required field
  let missing_field_config = {
    service_version: "1.0.0",
    environment: "development",
    log_level: "debug",
    metrics_enabled: true,
    tracing_enabled: false,
    endpoint: "https://example.com"
  }
  
  let missing_field_errors = validate_against_schema(missing_field_config, config_schema)
  assert_eq(missing_field_errors.length(), 1)
  assert_true(missing_field_errors[0].contains("Required field missing: service_name"))
  
  // Test invalid field value
  let invalid_value_config = {
    service_name: "test-service",
    service_version: "1.0.0",
    environment: "invalid-env",  // Invalid environment
    log_level: "debug",
    metrics_enabled: true,
    tracing_enabled: false,
    endpoint: "https://example.com"
  }
  
  let invalid_value_errors = validate_against_schema(invalid_value_config, config_schema)
  assert_eq(invalid_value_errors.length(), 1)
  assert_true(invalid_value_errors[0].contains("must be one of"))
  
  // Test invalid endpoint
  let invalid_endpoint_config = {
    service_name: "test-service",
    service_version: "1.0.0",
    environment: "development",
    log_level: "debug",
    metrics_enabled: true,
    tracing_enabled: false,
    endpoint: "ftp://example.com"  // Invalid protocol
  }
  
  let invalid_endpoint_errors = validate_against_schema(invalid_endpoint_config, config_schema)
  assert_eq(invalid_endpoint_errors.length(), 1)
  assert_true(invalid_endpoint_errors[0].contains("must match pattern"))
}

// Test 5: Dynamic Configuration Updates
test "dynamic configuration updates" {
  // Simulate configuration manager with dynamic updates
  let create_config_manager = fn(initial_config) {
    { current_config = initial_config, update_count = 0, last_updated = "2023-01-01T00:00:00Z" }
  }
  
  let update_config = fn(manager, new_config) {
    { 
      current_config = new_config,
      update_count = manager.update_count + 1,
      last_updated = "2023-01-01T12:00:00Z"  // Simulate timestamp
    }
  }
  
  let get_config = fn(manager) {
    manager.current_config
  }
  
  let get_update_count = fn(manager) {
    manager.update_count
  }
  
  // Test configuration updates
  let initial_config = {
    service_name: "test-service",
    service_version: "1.0.0",
    environment: "development",
    log_level: "info",
    metrics_enabled: true,
    tracing_enabled: true
  }
  
  let manager = create_config_manager(initial_config)
  assert_eq(get_update_count(manager), 0)
  
  let updated_config1 = {
    service_name: "test-service",
    service_version: "1.0.0",
    environment: "development",
    log_level: "debug",  // Changed
    metrics_enabled: true,
    tracing_enabled: true
  }
  
  let manager1 = update_config(manager, updated_config1)
  assert_eq(get_update_count(manager1), 1)
  assert_eq(get_config(manager1).log_level, "debug")
  
  let updated_config2 = {
    service_name: "test-service",
    service_version: "1.0.1",  // Changed
    environment: "staging",    // Changed
    log_level: "debug",
    metrics_enabled: false,    // Changed
    tracing_enabled: true
  }
  
  let manager2 = update_config(manager1, updated_config2)
  assert_eq(get_update_count(manager2), 2)
  assert_eq(get_config(manager2).service_version, "1.0.1")
  assert_eq(get_config(manager2).environment, "staging")
  assert_eq(get_config(manager2).metrics_enabled, false)
  
  // Test configuration change detection
  let detect_changes = fn(old_config, new_config) {
    let changes = []
    
    if old_config.service_name != new_config.service_name {
      changes = changes.push("service_name")
    }
    
    if old_config.service_version != new_config.service_version {
      changes = changes.push("service_version")
    }
    
    if old_config.environment != new_config.environment {
      changes = changes.push("environment")
    }
    
    if old_config.log_level != new_config.log_level {
      changes = changes.push("log_level")
    }
    
    if old_config.metrics_enabled != new_config.metrics_enabled {
      changes = changes.push("metrics_enabled")
    }
    
    if old_config.tracing_enabled != new_config.tracing_enabled {
      changes = changes.push("tracing_enabled")
    }
    
    changes
  }
  
  let changes1 = detect_changes(initial_config, updated_config1)
  assert_eq(changes1.length(), 1)
  assert_true(changes1.contains("log_level"))
  
  let changes2 = detect_changes(updated_config1, updated_config2)
  assert_eq(changes2.length(), 3)
  assert_true(changes2.contains("service_version"))
  assert_true(changes2.contains("environment"))
  assert_true(changes2.contains("metrics_enabled"))
}

// Test 6: Configuration Hot Reload
test "configuration hot reload" {
  // Simulate configuration file watcher
  let create_file_watcher = fn(config_path) {
    { watched_file = config_path, last_modified = "2023-01-01T00:00:00Z", watching = true }
  }
  
  let check_file_changes = fn(watcher) {
    // Simulate file change detection
    let current_time = "2023-01-01T12:00:00Z"
    if watcher.last_modified != current_time {
      { 
        watcher = { 
          watched_file = watcher.watched_file, 
          last_modified = current_time, 
          watching = watcher.watching 
        }, 
        changed = true 
      }
    } else {
      { watcher = watcher, changed = false }
    }
  }
  
  let reload_config = fn(config_path) {
    // Simulate configuration file loading
    if config_path.contains("development") {
      {
        service_name: "dev-service-reloaded",
        service_version: "1.0.1",
        environment: "development",
        log_level: "debug",
        metrics_enabled: true,
        tracing_enabled: true
      }
    } else if config_path.contains("production") {
      {
        service_name: "prod-service-reloaded",
        service_version: "1.0.1",
        environment: "production",
        log_level: "warn",
        metrics_enabled: true,
        tracing_enabled: true
      }
    } else {
      {
        service_name: "default-service-reloaded",
        service_version: "1.0.1",
        environment: "development",
        log_level: "info",
        metrics_enabled: true,
        tracing_enabled: true
      }
    }
  }
  
  // Test hot reload process
  let dev_config_path = "/etc/azimuth/development.json"
  let watcher = create_file_watcher(dev_config_path)
  
  // Simulate file change
  let change_result = check_file_changes(watcher)
  assert_true(change_result.changed)
  
  // Reload configuration
  if change_result.changed {
    let new_config = reload_config(change_result.watcher.watched_file)
    assert_eq(new_config.service_name, "dev-service-reloaded")
    assert_eq(new_config.service_version, "1.0.1")
    assert_eq(new_config.log_level, "debug")
  }
  
  // Test production config reload
  let prod_config_path = "/etc/azimuth/production.json"
  let prod_watcher = create_file_watcher(prod_config_path)
  
  let prod_change_result = check_file_changes(prod_watcher)
  assert_true(prod_change_result.changed)
  
  if prod_change_result.changed {
    let prod_new_config = reload_config(prod_change_result.watcher.watched_file)
    assert_eq(prod_new_config.service_name, "prod-service-reloaded")
    assert_eq(prod_new_config.service_version, "1.0.1")
    assert_eq(prod_new_config.log_level, "warn")
  }
  
  // Test no change scenario
  let no_change_watcher = { 
    watched_file = dev_config_path, 
    last_modified = "2023-01-01T12:00:00Z",  // Same as current time
    watching = true 
  }
  
  let no_change_result = check_file_changes(no_change_watcher)
  assert_false(no_change_result.changed)
}

// Test 7: Configuration Security and Sensitive Data
test "configuration security and sensitive data" {
  // Define sensitive fields
  let sensitive_fields = ["api_key", "database_password", "secret_token", "private_key"]
  
  // Simulate secure configuration handling
  let mask_sensitive_data = fn(config, sensitive_fields) {
    let masked_config = {}
    
    for (key, value) in config {
      if sensitive_fields.contains(key) {
        masked_config = masked_config.with(key, "***MASKED***")
      } else {
        masked_config = masked_config.with(key, value)
      }
    }
    
    masked_config
  }
  
  let validate_sensitive_fields = fn(config, sensitive_fields) {
    let errors = []
    
    for field in sensitive_fields {
      if config.contains(field) {
        let value = config[field]
        if value.length() < 8 {
          errors = errors.push("Sensitive field " + field + " must be at least 8 characters")
        }
        if value == "password" || value == "secret" || value == "12345678" {
          errors = errors.push("Sensitive field " + field + " cannot use common/default values")
        }
      }
    }
    
    errors
  }
  
  // Test with sensitive data
  let config_with_secrets = {
    service_name: "secure-service",
    service_version: "1.0.0",
    environment: "production",
    api_key: "sk-1234567890abcdef",
    database_password: "super_secure_password_123",
    secret_token: "token_abcdef123456",
    private_key: "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC...",
    public_key: "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKBAQEA..."
  }
  
  // Test masking
  let masked_config = mask_sensitive_data(config_with_secrets, sensitive_fields)
  assert_eq(masked_config["service_name"], "secure-service")
  assert_eq(masked_config["api_key"], "***MASKED***")
  assert_eq(masked_config["database_password"], "***MASKED***")
  assert_eq(masked_config["secret_token"], "***MASKED***")
  assert_eq(masked_config["private_key"], "***MASKED***")
  assert_eq(masked_config["public_key"], "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKBAQEA...")  // Not masked
  
  // Test validation
  let validation_errors = validate_sensitive_fields(config_with_secrets, sensitive_fields)
  assert_eq(validation_errors.length(), 0)
  
  // Test with weak sensitive data
  let weak_config = {
    service_name: "weak-service",
    service_version: "1.0.0",
    environment: "production",
    api_key: "weak",  // Too short
    database_password: "password",  // Common value
    secret_token: "12345678",  // Common value
    private_key: "short"  // Too short
  }
  
  let weak_validation_errors = validate_sensitive_fields(weak_config, sensitive_fields)
  assert_eq(weak_validation_errors.length(), 4)
  assert_true(weak_validation_errors[0].contains("api_key must be at least 8 characters"))
  assert_true(weak_validation_errors[1].contains("database_password cannot use common/default values"))
  assert_true(weak_validation_errors[2].contains("secret_token cannot use common/default values"))
  assert_true(weak_validation_errors[3].contains("private_key must be at least 8 characters"))
}

// Test 8: Configuration Templates and Inheritance
test "configuration templates and inheritance" {
  // Define base configuration template
  let base_template = {
    service_name: "{{SERVICE_NAME}}",
    service_version: "{{SERVICE_VERSION}}",
    environment: "{{ENVIRONMENT}}",
    log_level: "{{LOG_LEVEL}}",
    metrics_enabled: "{{METRICS_ENABLED}}",
    tracing_enabled: "{{TRACING_ENABLED}}",
    endpoint: "{{ENDPOINT}}",
    retry_attempts: 3,
    timeout_ms: 5000
  }
  
  // Define environment-specific templates
  let development_template = {
    extends: "base",
    log_level: "debug",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "http://localhost:8080",
    debug_mode: true
  }
  
  let production_template = {
    extends: "base",
    log_level: "warn",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "https://api.azimuth.com",
    retry_attempts: 5,
    timeout_ms: 10000,
    circuit_breaker: true
  }
  
  // Simulate template resolution
  let resolve_template = fn(template_name, templates, variables) {
    let template = match template_name {
      "base" => templates["base"]
      "development" => templates["development"]
      "production" => templates["production"]
      _ => templates["base"]
    }
    
    let resolved = {}
    for (key, value) in template {
      if value.length() > 2 && value[0] == '{' && value[1] == '{' && value[value.length() - 1] == '}' && value[value.length() - 2] == '}' {
        let var_name = value.substring(2, value.length() - 2)
        resolved = resolved.with(key, variables[var_name])
      } else {
        resolved = resolved.with(key, value)
      }
    }
    
    resolved
  }
  
  // Simulate template inheritance
  let apply_inheritance = fn(template_name, templates) {
    let template = templates[template_name]
    if template.contains("extends") {
      let parent_name = template["extends"]
      let parent = templates[parent_name]
      
      let inherited = {}
      for (key, value) in parent {
        inherited = inherited.with(key, value)
      }
      
      for (key, value) in template {
        if key != "extends" {
          inherited = inherited.with(key, value)
        }
      }
      
      inherited
    } else {
      template
    }
  }
  
  let templates = {
    "base": base_template,
    "development": development_template,
    "production": production_template
  }
  
  // Test template inheritance
  let dev_inherited = apply_inheritance("development", templates)
  assert_eq(dev_inherited["log_level"], "debug")  // From development template
  assert_eq(dev_inherited["retry_attempts"], 3)  // From base template
  assert_eq(dev_inherited["debug_mode"], true)   // From development template
  
  let prod_inherited = apply_inheritance("production", templates)
  assert_eq(prod_inherited["log_level"], "warn")  // From production template
  assert_eq(prod_inherited["retry_attempts"], 5)  // From production template (overrides base)
  assert_eq(prod_inherited["circuit_breaker"], true)  // From production template
  
  // Test template variable substitution
  let variables = {
    "SERVICE_NAME": "azimuth-service",
    "SERVICE_VERSION": "1.0.0",
    "ENVIRONMENT": "development",
    "LOG_LEVEL": "info",
    "METRICS_ENABLED": true,
    "TRACING_ENABLED": true,
    "ENDPOINT": "https://api.example.com"
  }
  
  let resolved_config = resolve_template("base", templates, variables)
  assert_eq(resolved_config["service_name"], "azimuth-service")
  assert_eq(resolved_config["service_version"], "1.0.0")
  assert_eq(resolved_config["environment"], "development")
  assert_eq(resolved_config["log_level"], "info")
  assert_eq(resolved_config["metrics_enabled"], true)
  assert_eq(resolved_config["tracing_enabled"], true)
  assert_eq(resolved_config["endpoint"], "https://api.example.com")
  assert_eq(resolved_config["retry_attempts"], 3)
  assert_eq(resolved_config["timeout_ms"], 5000)
}

// Test 9: Configuration Versioning and Migration
test "configuration versioning and migration" {
  // Define configuration versions
  let v1_config = {
    version: "1.0",
    service_name: "test-service",
    service_port: 8080,
    debug: true,
    log_file: "/var/log/app.log"
  }
  
  let v2_config = {
    version: "2.0",
    service_name: "test-service",
    endpoints: {
      http: { port: 8080, host: "localhost" },
      grpc: { port: 9090, host: "localhost" }
    },
    logging: {
      level: "debug",
      file: "/var/log/app.log",
      format: "json"
    }
  }
  
  let v3_config = {
    version: "3.0",
    service: {
      name: "test-service",
      version: "1.0.0"
    },
    servers: [
      { name: "http", port: 8080, host: "localhost", protocol: "http" },
      { name: "grpc", port: 9090, host: "localhost", protocol: "grpc" }
    ],
    observability: {
      logging: {
        level: "debug",
        outputs: [
          { type: "file", path: "/var/log/app.log", format: "json" }
        ]
      },
      metrics: {
        enabled: true,
        endpoint: "http://localhost:9090/metrics"
      },
      tracing: {
        enabled: true,
        endpoint: "http://localhost:9411/api/v2/spans"
      }
    }
  }
  
  // Simulate configuration migration
  let migrate_config = fn(config, target_version) {
    let current_version = config["version"]
    
    if current_version == "1.0" && target_version == "2.0" {
      // Migrate from v1 to v2
      {
        version: "2.0",
        service_name: config["service_name"],
        endpoints: {
          http: { port: config["service_port"], host: "localhost" },
          grpc: { port: 9090, host: "localhost" }
        },
        logging: {
          level: if config["debug"] { "debug" } else { "info" },
          file: config["log_file"],
          format: "json"
        }
      }
    } else if current_version == "2.0" && target_version == "3.0" {
      // Migrate from v2 to v3
      {
        version: "3.0",
        service: {
          name: config["service_name"],
          version: "1.0.0"
        },
        servers: [
          { name: "http", port: config["endpoints"]["http"]["port"], host: config["endpoints"]["http"]["host"], protocol: "http" },
          { name: "grpc", port: config["endpoints"]["grpc"]["port"], host: config["endpoints"]["grpc"]["host"], protocol: "grpc" }
        ],
        observability: {
          logging: {
            level: config["logging"]["level"],
            outputs: [
              { type: "file", path: config["logging"]["file"], format: config["logging"]["format"] }
            ]
          },
          metrics: {
            enabled: true,
            endpoint: "http://localhost:9090/metrics"
          },
          tracing: {
            enabled: true,
            endpoint: "http://localhost:9411/api/v2/spans"
          }
        }
      }
    } else {
      // No migration needed or unsupported migration
      config
    }
  }
  
  // Test v1 to v2 migration
  let migrated_v1_to_v2 = migrate_config(v1_config, "2.0")
  assert_eq(migrated_v1_to_v2["version"], "2.0")
  assert_eq(migrated_v1_to_v2["service_name"], "test-service")
  assert_eq(migrated_v1_to_v2["endpoints"]["http"]["port"], 8080)
  assert_eq(migrated_v1_to_v2["endpoints"]["http"]["host"], "localhost")
  assert_eq(migrated_v1_to_v2["endpoints"]["grpc"]["port"], 9090)
  assert_eq(migrated_v1_to_v2["endpoints"]["grpc"]["host"], "localhost")
  assert_eq(migrated_v1_to_v2["logging"]["level"], "debug")
  assert_eq(migrated_v1_to_v2["logging"]["file"], "/var/log/app.log")
  assert_eq(migrated_v1_to_v2["logging"]["format"], "json")
  
  // Test v2 to v3 migration
  let migrated_v2_to_v3 = migrate_config(v2_config, "3.0")
  assert_eq(migrated_v2_to_v3["version"], "3.0")
  assert_eq(migrated_v2_to_v3["service"]["name"], "test-service")
  assert_eq(migrated_v2_to_v3["service"]["version"], "1.0.0")
  assert_eq(migrated_v2_to_v3["servers"].length(), 2)
  assert_eq(migrated_v2_to_v3["servers"][0]["name"], "http")
  assert_eq(migrated_v2_to_v3["servers"][0]["port"], 8080)
  assert_eq(migrated_v2_to_v3["servers"][1]["name"], "grpc")
  assert_eq(migrated_v2_to_v3["servers"][1]["port"], 9090)
  assert_eq(migrated_v2_to_v3["observability"]["logging"]["level"], "debug")
  assert_eq(migrated_v2_to_v3["observability"]["metrics"]["enabled"], true)
  assert_eq(migrated_v2_to_v3["observability"]["tracing"]["enabled"], true)
  
  // Test no migration needed
  let no_migration = migrate_config(v3_config, "3.0")
  assert_eq(no_migration["version"], "3.0")
}

// Test 10: Configuration Validation with Business Rules
test "configuration validation with business rules" {
  // Define business rules for configuration validation
  let validate_business_rules = fn(config) {
    let errors = []
    
    // Rule 1: Production environment must have certain security settings
    if config["environment"] == "production" {
      if !config["metrics_enabled"] {
        errors = errors.push("Production environment must have metrics enabled")
      }
      
      if !config["tracing_enabled"] {
        errors = errors.push("Production environment must have tracing enabled")
      }
      
      if !config["endpoint"].starts_with("https://") {
        errors = errors.push("Production environment must use HTTPS endpoints")
      }
      
      if config["log_level"] == "debug" {
        errors = errors.push("Production environment should not use debug log level")
      }
    }
    
    // Rule 2: Development environment should not use production endpoints
    if config["environment"] == "development" && config["endpoint"].contains("production") {
      errors = errors.push("Development environment should not use production endpoints")
    }
    
    // Rule 3: Service version must follow semantic versioning
    if !config["service_version"].contains(".") {
      errors = errors.push("Service version must follow semantic versioning (e.g., 1.0.0)")
    }
    
    // Rule 4: Retry attempts must be reasonable
    if config.contains("retry_attempts") {
      let retry_attempts = config["retry_attempts"]
      if retry_attempts < 0 || retry_attempts > 10 {
        errors = errors.push("Retry attempts must be between 0 and 10")
      }
    }
    
    // Rule 5: Timeout must be reasonable
    if config.contains("timeout_ms") {
      let timeout = config["timeout_ms"]
      if timeout < 100 || timeout > 60000 {
        errors = errors.push("Timeout must be between 100ms and 60000ms")
      }
    }
    
    errors
  }
  
  // Test valid production configuration
  let valid_prod_config = {
    service_name: "prod-service",
    service_version: "1.2.3",
    environment: "production",
    log_level: "warn",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "https://api.azimuth.com",
    retry_attempts: 3,
    timeout_ms: 5000
  }
  
  let prod_errors = validate_business_rules(valid_prod_config)
  assert_eq(prod_errors.length(), 0)
  
  // Test invalid production configuration
  let invalid_prod_config = {
    service_name: "prod-service",
    service_version: "1.2.3",
    environment: "production",
    log_level: "debug",  // Invalid for production
    metrics_enabled: false,  // Invalid for production
    tracing_enabled: false,  // Invalid for production
    endpoint: "http://api.azimuth.com",  // Should be HTTPS
    retry_attempts: 3,
    timeout_ms: 5000
  }
  
  let invalid_prod_errors = validate_business_rules(invalid_prod_config)
  assert_eq(invalid_prod_errors.length(), 4)
  assert_true(invalid_prod_errors[0].contains("Production environment must have metrics enabled"))
  assert_true(invalid_prod_errors[1].contains("Production environment must have tracing enabled"))
  assert_true(invalid_prod_errors[2].contains("Production environment must use HTTPS endpoints"))
  assert_true(invalid_prod_errors[3].contains("Production environment should not use debug log level"))
  
  // Test invalid development configuration
  let invalid_dev_config = {
    service_name: "dev-service",
    service_version: "1.2.3",
    environment: "development",
    log_level: "debug",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "https://api.production.azimuth.com",  // Production endpoint in dev
    retry_attempts: 15,  // Too many retries
    timeout_ms: 50  // Too short timeout
  }
  
  let invalid_dev_errors = validate_business_rules(invalid_dev_config)
  assert_eq(invalid_dev_errors.length(), 3)
  assert_true(invalid_dev_errors[0].contains("Development environment should not use production endpoints"))
  assert_true(invalid_dev_errors[1].contains("Retry attempts must be between 0 and 10"))
  assert_true(invalid_dev_errors[2].contains("Timeout must be between 100ms and 60000ms"))
  
  // Test invalid version format
  let invalid_version_config = {
    service_name: "test-service",
    service_version: "v1",  // Invalid semantic version
    environment: "development",
    log_level: "debug",
    metrics_enabled: true,
    tracing_enabled: true,
    endpoint: "http://localhost:8080"
  }
  
  let invalid_version_errors = validate_business_rules(invalid_version_config)
  assert_eq(invalid_version_errors.length(), 1)
  assert_true(invalid_version_errors[0].contains("Service version must follow semantic versioning"))
}