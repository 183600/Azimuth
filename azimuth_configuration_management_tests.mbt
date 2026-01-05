// Azimuth Configuration Management Tests
// This file contains test cases for configuration management functionality

// Test 1: Basic Configuration Loading and Validation
test "basic configuration loading and validation" {
  // Define configuration value types
  type ConfigValue = {
    key: String,
    value: String,
    value_type: String,  // "string", "int", "float", "bool", "array"
    required: Bool,
    default_value: Option[String],
    description: String
  }
  
  type ConfigSchema = {
    name: String,
    version: String,
    values: Array[ConfigValue]
  }
  
  type Configuration = {
    schema: ConfigSchema,
    loaded_values: Array[(String, String)]
  }
  
  // Create configuration schema
  let create_schema = fn(name: String, version: String) {
    {
      name: name,
      version: version,
      values: []
    }
  }
  
  // Add configuration value to schema
  let add_config_value = fn(schema: ConfigSchema, key: String, value_type: String, required: Bool, default_value: Option[String], description: String) {
    let config_value = {
      key: key,
      value: "",
      value_type: value_type,
      required: required,
      default_value: default_value,
      description: description
    }
    
    { schema | values: schema.values.push(config_value) }
  }
  
  // Validate configuration value
  let validate_config_value = fn(value: String, value_type: String) {
    match value_type {
      "string" => value.length() > 0
      "int" => {
        let mut i = 0
        let mut valid = true
        
        if value.length() == 0 {
          valid = false
        } else {
          if value[0] == '-' {
            if value.length() == 1 {
              valid = false
            } else {
              i = 1
            }
          }
          
          while i < value.length() and valid {
            let c = value[i]
            if c < '0' or c > '9' {
              valid = false
            }
            i = i + 1
          }
        }
        
        valid
      }
      "float" => {
        let mut i = 0
        let mut valid = true
        let mut dot_seen = false
        
        if value.length() == 0 {
          valid = false
        } else {
          if value[0] == '-' {
            if value.length() == 1 {
              valid = false
            } else {
              i = 1
            }
          }
          
          while i < value.length() and valid {
            let c = value[i]
            if c == '.' {
              if dot_seen {
                valid = false
              } else {
                dot_seen = true
              }
            } else if c < '0' or c > '9' {
              valid = false
            }
            i = i + 1
          }
        }
        
        valid
      }
      "bool" => value == "true" or value == "false"
      "array" => value.length() > 2 and value[0] == '[' and value[value.length() - 1] == ']'
      _ => false
    }
  }
  
  // Load configuration
  let load_configuration = fn(schema: ConfigSchema, values: Array[(String, String)]) {
    let mut loaded = []
    let mut errors = []
    
    for config_value in schema.values {
      let value_pair = values.find_fn(v) { v.0 == config_value.key }
      
      match value_pair {
        Some(pair) => {
          if validate_config_value(pair.1, config_value.value_type) {
            loaded = loaded.push(pair)
          } else {
            errors = errors.push("Invalid value for " + config_value.key + ": " + pair.1)
          }
        }
        None => {
          if config_value.required {
            match config_value.default_value {
              Some(default) => {
                if validate_config_value(default, config_value.value_type) {
                  loaded = loaded.push((config_value.key, default))
                } else {
                  errors = errors.push("Invalid default value for " + config_value.key)
                }
              }
              None => {
                errors = errors.push("Required value missing for " + config_value.key)
              }
            }
          } else {
            match config_value.default_value {
              Some(default) => {
                if validate_config_value(default, config_value.value_type) {
                  loaded = loaded.push((config_value.key, default))
                }
              }
              None => {}  // Optional value with no default, skip
            }
          }
        }
      }
    }
    
    {
      schema: schema,
      loaded_values: loaded,
      errors: errors
    }
  }
  
  // Create test schema
  let schema = create_schema("telemetry_config", "1.0.0")
  let schema_with_values = add_config_value(
    add_config_value(
      add_config_value(
        add_config_value(
          add_config_value(schema, "service_name", "string", true, None, "Name of the service"),
          "port", "int", true, Some("8080"), "Port number for the service"
        ),
        "enable_metrics", "bool", false, Some("true"), "Enable metrics collection"
      ),
      "sampling_rate", "float", false, Some("0.1"), "Sampling rate for metrics"
    ),
    "tags", "array", false, Some("[\"production\"]"), "Tags for the service"
  )
  
  // Test loading configuration with all required values
  let config_values = [
    ("service_name", "payment-service"),
    ("port", "9090"),
    ("enable_metrics", "false"),
    ("sampling_rate", "0.25"),
    ("tags", "[\"production\", \"payment\"]")
  ]
  
  let config = load_configuration(schema_with_values.values, config_values)
  assert_eq(config.errors.length(), 0)
  assert_eq(config.loaded_values.length(), 5)
  
  // Check loaded values
  let service_name = config.loaded_values.find_fn(v) { v.0 == "service_name" }
  match service_name {
    Some(pair) => assert_eq(pair.1, "payment-service")
    None => assert_true(false)
  }
  
  let port = config.loaded_values.find_fn(v) { v.0 == "port" }
  match port {
    Some(pair) => assert_eq(pair.1, "9090")
    None => assert_true(false)
  }
  
  // Test loading configuration with missing required value
  let incomplete_values = [
    ("port", "9090"),
    ("enable_metrics", "true")
  ]
  
  let incomplete_config = load_configuration(schema_with_values.values, incomplete_values)
  assert_true(incomplete_config.errors.length() > 0)
  assert_true(incomplete_config.errors.some_fn(e) { e.contains("service_name") })
  
  // Test loading configuration with invalid value
  let invalid_values = [
    ("service_name", "payment-service"),
    ("port", "invalid_port"),  // Invalid integer
    ("enable_metrics", "maybe")  // Invalid boolean
  ]
  
  let invalid_config = load_configuration(schema_with_values.values, invalid_values)
  assert_true(invalid_config.errors.length() > 0)
  assert_true(invalid_config.errors.some_fn(e) { e.contains("port") })
  assert_true(invalid_config.errors.some_fn(e) { e.contains("enable_metrics") })
  
  // Test loading configuration with defaults
  let default_values = [
    ("service_name", "auth-service")
  ]
  
  let default_config = load_configuration(schema_with_values.values, default_values)
  assert_eq(default_config.errors.length(), 0)
  
  // Check that defaults were used
  let default_port = default_config.loaded_values.find_fn(v) { v.0 == "port" }
  match default_port {
    Some(pair) => assert_eq(pair.1, "8080")  // Default value
    None => assert_true(false)
  }
  
  let default_enable_metrics = default_config.loaded_values.find_fn(v) { v.0 == "enable_metrics" }
  match default_enable_metrics {
    Some(pair) => assert_eq(pair.1, "true")  // Default value
    None => assert_true(false)
  }
}

// Test 2: Configuration Hierarchy and Inheritance
test "configuration hierarchy and inheritance" {
  type ConfigLayer = {
    name: String,
    priority: Int,
    values: Array[(String, String)]
  }
  
  type HierarchicalConfig = {
    layers: Array[ConfigLayer],
    merged_values: Array[(String, String)]
  }
  
  // Create config layer
  let create_layer = fn(name: String, priority: Int) {
    {
      name: name,
      priority: priority,
      values: []
    }
  }
  
  // Add value to layer
  let add_value = fn(layer: ConfigLayer, key: String, value: String) {
    { layer | values: layer.values.push((key, value)) }
  }
  
  // Merge layers by priority
  let merge_layers = fn(layers: Array[ConfigLayer]) {
    // Sort layers by priority (higher priority first)
    let sorted_layers = {
      let mut sorted = layers
      let mut i = 0
      while i < sorted.length() {
        let mut j = i + 1
        while j < sorted.length() {
          if sorted[j].priority > sorted[i].priority {
            let temp = sorted[i]
            sorted[i] = sorted[j]
            sorted[j] = temp
          }
          j = j + 1
        }
        i = i + 1
      }
      sorted
    }
    
    // Merge values, higher priority layers override lower ones
    let mut merged = []
    let mut processed_keys = []
    
    for layer in sorted_layers {
      for (key, value) in layer.values {
        if not(processed_keys.contains(key)) {
          processed_keys = processed_keys.push(key)
          merged = merged.push((key, value))
        }
      }
    }
    
    merged
  }
  
  // Create hierarchical config
  let create_hierarchical_config = fn(layers: Array[ConfigLayer]) {
    {
      layers: layers,
      merged_values: merge_layers(layers)
    }
  }
  
  // Get value from hierarchical config
  let get_value = fn(config: HierarchicalConfig, key: String) {
    config.merged_values.find_fn(v) { v.0 == key }
  }
  
  // Create layers with different priorities
  let base_layer = create_layer("base", 1)
  let base_with_values = add_value(
    add_value(
      add_value(base_layer, "service_name", "telemetry-service"),
      "port", "8080"
    ),
    "log_level", "info"
  )
  
  let env_layer = create_layer("environment", 2)
  let env_with_values = add_value(
    add_value(env_layer, "port", "9090"),  // Override base port
    "environment", "production"
  )
  
  let local_layer = create_layer("local", 3)
  let local_with_values = add_value(
    add_value(
      add_value(local_layer, "service_name", "local-telemetry"),  // Override base service name
      "debug_mode", "true"
    ),
    "log_level", "debug"  // Override base log level
  )
  
  // Create hierarchical config
  let layers = [base_with_values, env_with_values, local_with_values]
  let config = create_hierarchical_config(layers)
  
  // Test merged values
  assert_eq(config.merged_values.length(), 5)
  
  // Check overrides
  let service_name = get_value(config, "service_name")
  match service_name {
    Some(pair) => assert_eq(pair.1, "local-telemetry")  // From local layer (highest priority)
    None => assert_true(false)
  }
  
  let port = get_value(config, "port")
  match port {
    Some(pair) => assert_eq(pair.1, "9090")  // From env layer
    None => assert_true(false)
  }
  
  let log_level = get_value(config, "log_level")
  match log_level {
    Some(pair) => assert_eq(pair.1, "debug")  // From local layer
    None => assert_true(false)
  }
  
  let environment = get_value(config, "environment")
  match environment {
    Some(pair) => assert_eq(pair.1, "production")  // From env layer
    None => assert_true(false)
  }
  
  let debug_mode = get_value(config, "debug_mode")
  match debug_mode {
    Some(pair) => assert_eq(pair.1, "true")  // From local layer
    None => assert_true(false)
  }
  
  // Test layer priority changes
  let new_env_layer = create_layer("environment", 4)  // Higher priority than local
  let new_env_with_values = add_value(new_env_layer, "service_name", "prod-telemetry")
  
  let new_layers = [base_with_values, env_with_values, local_with_values, new_env_with_values]
  let new_config = create_hierarchical_config(new_layers)
  
  let new_service_name = get_value(new_config, "service_name")
  match new_service_name {
    Some(pair) => assert_eq(pair.1, "prod-telemetry")  // From new env layer (highest priority)
    None => assert_true(false)
  }
}

// Test 3: Dynamic Configuration Updates
test "dynamic configuration updates" {
  type ConfigChange = {
    key: String,
    old_value: Option[String],
    new_value: String,
    timestamp: Int
  }
  
  type DynamicConfig = {
    values: Array[(String, String)],
    change_history: Array[ConfigChange],
    subscribers: Array[(String, (String, String) -> Unit)]
  }
  
  // Create dynamic config
  let create_dynamic_config = fn() {
    {
      values: [],
      change_history: [],
      subscribers: []
    }
  }
  
  // Get value from dynamic config
  let get_dynamic_value = fn(config: DynamicConfig, key: String) {
    config.values.find_fn(v) { v.0 == key }
  }
  
  // Set value in dynamic config
  let set_dynamic_value = fn(config: DynamicConfig, key: String, value: String) {
    let existing = get_dynamic_value(config, key)
    let old_value = match existing {
      Some(pair) => Some(pair.1)
      None => None
    }
    
    // Update or add value
    let mut updated = []
    let mut found = false
    
    for (k, v) in config.values {
      if k == key {
        updated = updated.push((key, value))
        found = true
      } else {
        updated = updated.push((k, v))
      }
    }
    
    if not(found) {
      updated = updated.push((key, value))
    }
    
    // Record change
    let change = {
      key: key,
      old_value: old_value,
      new_value: value,
      timestamp: 1640995200  // Fixed timestamp for testing
    }
    
    let updated_config = {
      values: updated,
      change_history: config.change_history.push(change),
      subscribers: config.subscribers
    }
    
    // Notify subscribers
    for (subscriber_key, callback) in updated_config.subscribers {
      callback(key, value)
    }
    
    updated_config
  }
  
  // Subscribe to configuration changes
  let subscribe = fn(config: DynamicConfig, subscriber_key: String, callback: (String, String) -> Unit) {
    { config | subscribers: config.subscribers.push((subscriber_key, callback)) }
  }
  
  // Unsubscribe from configuration changes
  let unsubscribe = fn(config: DynamicConfig, subscriber_key: String) {
    let mut updated = []
    for (key, callback) in config.subscribers {
      if key != subscriber_key {
        updated = updated.push((key, callback))
      }
    }
    { config | subscribers: updated }
  }
  
  // Get change history for a key
  let get_change_history = fn(config: DynamicConfig, key: String) {
    config.change_history.filter_fn(c) { c.key == key }
  }
  
  // Test dynamic configuration
  let config = create_dynamic_config()
  
  // Initial state
  assert_eq(config.values.length(), 0)
  assert_eq(config.change_history.length(), 0)
  assert_eq(config.subscribers.length(), 0)
  
  // Set initial values
  let config1 = set_dynamic_value(config, "service_name", "telemetry-service")
  assert_eq(config1.values.length(), 1)
  assert_eq(config1.change_history.length(), 1)
  
  let service_name = get_dynamic_value(config1, "service_name")
  match service_name {
    Some(pair) => assert_eq(pair.1, "telemetry-service")
    None => assert_true(false)
  }
  
  let change = config1.change_history[0]
  assert_eq(change.key, "service_name")
  assert_eq(change.old_value, None)
  assert_eq(change.new_value, "telemetry-service")
  
  // Update existing value
  let config2 = set_dynamic_value(config1, "service_name", "updated-service")
  assert_eq(config2.values.length(), 1)
  assert_eq(config2.change_history.length(), 2)
  
  let updated_service_name = get_dynamic_value(config2, "service_name")
  match updated_service_name {
    Some(pair) => assert_eq(pair.1, "updated-service")
    None => assert_true(false)
  }
  
  let update_change = config2.change_history[1]
  assert_eq(update_change.key, "service_name")
  assert_eq(update_change.old_value, Some("telemetry-service"))
  assert_eq(update_change.new_value, "updated-service")
  
  // Add another value
  let config3 = set_dynamic_value(config2, "port", "8080")
  assert_eq(config3.values.length(), 2)
  assert_eq(config3.change_history.length(), 3)
  
  // Test subscription
  let mut notifications = []
  
  let callback = fn(key: String, value: String) {
    notifications = notifications.push((key, value))
  }
  
  let config4 = subscribe(config3, "test_subscriber", callback)
  assert_eq(config4.subscribers.length(), 1)
  
  // Trigger notification
  let config5 = set_dynamic_value(config4, "port", "9090")
  assert_eq(notifications.length(), 1)
  assert_eq(notifications[0], ("port", "9090"))
  
  // Test unsubscribe
  let config6 = unsubscribe(config5, "test_subscriber")
  assert_eq(config6.subscribers.length(), 0)
  
  // No more notifications
  let config7 = set_dynamic_value(config6, "port", "10000")
  assert_eq(notifications.length(), 1)  // Still 1, no new notification
  
  // Test change history
  let port_history = get_change_history(config7, "port")
  assert_eq(port_history.length(), 3)
  
  assert_eq(port_history[0].new_value, "8080")
  assert_eq(port_history[1].new_value, "9090")
  assert_eq(port_history[2].new_value, "10000")
}

// Test 4: Configuration Environment-Specific Loading
test "configuration environment-specific loading" {
  type EnvironmentConfig = {
    environment: String,
    values: Array[(String, String)]
  }
  
  type MultiEnvConfig = {
    default_values: Array[(String, String)],
    environment_configs: Array[EnvironmentConfig]
  }
  
  // Create multi-environment config
  let create_multi_env_config = fn(default_values: Array[(String, String)]) {
    {
      default_values: default_values,
      environment_configs: []
    }
  }
  
  // Add environment config
  let add_environment_config = fn(config: MultiEnvConfig, environment: String, values: Array[(String, String)]) {
    let env_config = {
      environment: environment,
      values: values
    }
    { config | environment_configs: config.environment_configs.push(env_config) }
  }
  
  // Load configuration for specific environment
  let load_for_environment = fn(config: MultiEnvConfig, environment: String) {
    let env_config = config.environment_configs.find_fn(c) { c.environment == environment }
    
    match env_config {
      Some(env) => {
        // Start with default values
        let mut merged = config.default_values
        let mut processed_keys = config.default_values.map_fn(v) { v.0 }
        
        // Override with environment-specific values
        for (key, value) in env.values {
          if not(processed_keys.contains(key)) {
            merged = merged.push((key, value))
            processed_keys = processed_keys.push(key)
          } else {
            // Replace existing value
            let mut updated = []
            for (k, v) in merged {
              if k == key {
                updated = updated.push((key, value))
              } else {
                updated = updated.push((k, v))
              }
            }
            merged = updated
          }
        }
        
        merged
      }
      None => config.default_values
    }
  }
  
  // Create default configuration
  let default_values = [
    ("service_name", "telemetry-service"),
    ("port", "8080"),
    ("log_level", "info"),
    ("enable_metrics", "true"),
    ("sampling_rate", "0.1")
  ]
  
  let config = create_multi_env_config(default_values)
  
  // Add environment-specific configurations
  let dev_config = [
    ("port", "3000"),
    ("log_level", "debug"),
    ("enable_metrics", "false")
  ]
  
  let staging_config = [
    ("port", "8080"),
    ("log_level", "info"),
    ("sampling_rate", "0.5")
  ]
  
  let prod_config = [
    ("port", "80"),
    ("log_level", "warn"),
    ("sampling_rate", "0.01"),
    ("enable_metrics", "true")
  ]
  
  let config_with_envs = add_environment_config(
    add_environment_config(
      add_environment_config(config, "development", dev_config),
      "staging", staging_config
    ),
    "production", prod_config
  )
  
  // Test loading for different environments
  let dev_loaded = load_for_environment(config_with_envs, "development")
  assert_eq(dev_loaded.length(), 5)
  
  // Check development overrides
  let dev_port = dev_loaded.find_fn(v) { v.0 == "port" }
  match dev_port {
    Some(pair) => assert_eq(pair.1, "3000")
    None => assert_true(false)
  }
  
  let dev_log_level = dev_loaded.find_fn(v) { v.0 == "log_level" }
  match dev_log_level {
    Some(pair) => assert_eq(pair.1, "debug")
    None => assert_true(false)
  }
  
  let dev_service_name = dev_loaded.find_fn(v) { v.0 == "service_name" }
  match dev_service_name {
    Some(pair) => assert_eq(pair.1, "telemetry-service")  // From default
    None => assert_true(false)
  }
  
  // Test staging environment
  let staging_loaded = load_for_environment(config_with_envs, "staging")
  
  let staging_port = staging_loaded.find_fn(v) { v.0 == "port" }
  match staging_port {
    Some(pair) => assert_eq(pair.1, "8080")
    None => assert_true(false)
  }
  
  let staging_sampling_rate = staging_loaded.find_fn(v) { v.0 == "sampling_rate" }
  match staging_sampling_rate {
    Some(pair) => assert_eq(pair.1, "0.5")
    None => assert_true(false)
  }
  
  let staging_enable_metrics = staging_loaded.find_fn(v) { v.0 == "enable_metrics" }
  match staging_enable_metrics {
    Some(pair) => assert_eq(pair.1, "true")  // From default
    None => assert_true(false)
  }
  
  // Test production environment
  let prod_loaded = load_for_environment(config_with_envs, "production")
  
  let prod_port = prod_loaded.find_fn(v) { v.0 == "port" }
  match prod_port {
    Some(pair) => assert_eq(pair.1, "80")
    None => assert_true(false)
  }
  
  let prod_log_level = prod_loaded.find_fn(v) { v.0 == "log_level" }
  match prod_log_level {
    Some(pair) => assert_eq(pair.1, "warn")
    None => assert_true(false)
  }
  
  // Test unknown environment (should use defaults)
  let unknown_loaded = load_for_environment(config_with_envs, "unknown")
  assert_eq(unknown_loaded.length(), 5)
  assert_eq(unknown_loaded, default_values)
}

// Test 5: Configuration Validation and Constraints
test "configuration validation and constraints" {
  type ValidationRule = {
    name: String,
    validate: String -> Bool,
    error_message: String
  }
  
  type ConfigConstraint = {
    key: String,
    rules: Array[ValidationRule]
  }
  
  type ValidatedConfig = {
    values: Array[(String, String)],
    constraints: Array[ConfigConstraint],
    validation_errors: Array[String]
  }
  
  // Create validation rule
  let create_rule = fn(name: String, validate_fn: String -> Bool, error_message: String) {
    {
      name: name,
      validate: validate_fn,
      error_message: error_message
    }
  }
  
  // Create constraint
  let create_constraint = fn(key: String, rules: Array[ValidationRule]) {
    {
      key: key,
      rules: rules
    }
  }
  
  // Create validated config
  let create_validated_config = fn(values: Array[(String, String)], constraints: Array[ConfigConstraint]) {
    let mut errors = []
    
    for (key, value) in values {
      let constraint = constraints.find_fn(c) { c.key == key }
      
      match constraint {
        Some(cons) => {
          for rule in cons.rules {
            if not(rule.validate(value)) {
              errors = errors.push(key + ": " + rule.error_message)
            }
          }
        }
        None => {}  // No constraints for this key
      }
    }
    
    {
      values: values,
      constraints: constraints,
      validation_errors: errors
    }
  }
  
  // Common validation functions
  let non_empty = fn(value: String) { value.length() > 0 }
  let port_range = fn(value: String) {
    if value.length() == 0 {
      false
    } else {
      let mut i = 0
      let mut valid = true
      
      while i < value.length() and valid {
        let c = value[i]
        if c < '0' or c > '9' {
          valid = false
        }
        i = i + 1
      }
      
      if valid {
        let port = value.to_int()
        port >= 1 and port <= 65535
      } else {
        false
      }
    }
  }
  
  let sampling_rate_range = fn(value: String) {
    if value.length() == 0 {
      false
    } else {
      let mut i = 0
      let mut valid = true
      let mut dot_seen = false
      
      while i < value.length() and valid {
        let c = value[i]
        if c == '.' {
          if dot_seen {
            valid = false
          } else {
            dot_seen = true
          }
        } else if c < '0' or c > '9' {
          valid = false
        }
        i = i + 1
      }
      
      if valid {
        let rate = value.to_float()
        rate >= 0.0 and rate <= 1.0
      } else {
        false
      }
    }
  }
  
  let url_format = fn(value: String) {
    value.starts_with("http://") or value.starts_with("https://")
  }
  
  // Create validation rules
  let non_empty_rule = create_rule("non_empty", non_empty, "Value cannot be empty")
  let port_rule = create_rule("port_range", port_range, "Port must be between 1 and 65535")
  let sampling_rate_rule = create_rule("sampling_rate_range", sampling_rate_range, "Sampling rate must be between 0.0 and 1.0")
  let url_rule = create_rule("url_format", url_format, "URL must start with http:// or https://")
  
  // Create constraints
  let service_name_constraint = create_constraint("service_name", [non_empty_rule])
  let port_constraint = create_constraint("port", [port_rule])
  let sampling_rate_constraint = create_constraint("sampling_rate", [sampling_rate_rule])
  let endpoint_constraint = create_constraint("endpoint", [non_empty_rule, url_rule])
  
  let constraints = [
    service_name_constraint,
    port_constraint,
    sampling_rate_constraint,
    endpoint_constraint
  ]
  
  // Test valid configuration
  let valid_values = [
    ("service_name", "telemetry-service"),
    ("port", "8080"),
    ("sampling_rate", "0.1"),
    ("endpoint", "https://api.example.com")
  ]
  
  let valid_config = create_validated_config(valid_values, constraints)
  assert_eq(valid_config.validation_errors.length(), 0)
  
  // Test invalid configuration
  let invalid_values = [
    ("service_name", ""),  // Empty
    ("port", "70000"),  // Out of range
    ("sampling_rate", "1.5"),  // Out of range
    ("endpoint", "invalid-url")  // Invalid format
  ]
  
  let invalid_config = create_validated_config(invalid_values, constraints)
  assert_eq(invalid_config.validation_errors.length(), 4)
  assert_true(invalid_config.validation_errors.some_fn(e) { e.contains("service_name") and e.contains("cannot be empty") })
  assert_true(invalid_config.validation_errors.some_fn(e) { e.contains("port") and e.contains("between 1 and 65535") })
  assert_true(invalid_config.validation_errors.some_fn(e) { e.contains("sampling_rate") and e.contains("between 0.0 and 1.0") })
  assert_true(invalid_config.validation_errors.some_fn(e) { e.contains("endpoint") and e.contains("http:// or https://") })
  
  // Test partial invalid configuration
  let partial_values = [
    ("service_name", "valid-service"),
    ("port", "invalid-port"),  // Invalid
    ("sampling_rate", "0.5")  // Valid
  ]
  
  let partial_config = create_validated_config(partial_values, constraints)
  assert_eq(partial_config.validation_errors.length(), 1)
  assert_true(partial_config.validation_errors.some_fn(e) { e.contains("port") })
  
  // Test configuration with unknown keys (no validation errors)
  let unknown_values = [
    ("unknown_key1", "value1"),
    ("unknown_key2", "value2")
  ]
  
  let unknown_config = create_validated_config(unknown_values, constraints)
  assert_eq(unknown_config.validation_errors.length(), 0)
}