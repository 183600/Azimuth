// Azimuth Configuration Management Test Suite
// This file contains comprehensive test cases for configuration management

// Test 1: Basic Configuration Loading
test "basic configuration loading" {
  // Define configuration data structure
  type ConfigValue {
    string_value : Option[String]
    int_value : Option[Int]
    float_value : Option[Float]
    bool_value : Option[Bool]
    array_value : Option[Array[String]]
  }
  
  type Configuration {
    values : Array[(String, ConfigValue)]
    sources : Array[String]
    last_modified : Int
  }
  
  let create_config = fn() {
    { values: [], sources: [], last_modified: 0 }
  }
  
  let set_string_value = fn(config, key, value) {
    let config_value = {
      string_value: Some(value),
      int_value: None,
      float_value: None,
      bool_value: None,
      array_value: None
    }
    
    let mut updated_values = []
    let mut found = false
    
    for (k, v) in config.values {
      if k == key {
        updated_values = updated_values @ [(k, config_value)]
        found = true
      } else {
        updated_values = updated_values @ [(k, v)]
      }
    }
    
    if not found {
      updated_values = updated_values @ [(key, config_value)]
    }
    
    {
      values: updated_values,
      sources: config.sources,
      last_modified: config.last_modified + 1
    }
  }
  
  let set_int_value = fn(config, key, value) {
    let config_value = {
      string_value: None,
      int_value: Some(value),
      float_value: None,
      bool_value: None,
      array_value: None
    }
    
    let mut updated_values = []
    let mut found = false
    
    for (k, v) in config.values {
      if k == key {
        updated_values = updated_values @ [(k, config_value)]
        found = true
      } else {
        updated_values = updated_values @ [(k, v)]
      }
    }
    
    if not found {
      updated_values = updated_values @ [(key, config_value)]
    }
    
    {
      values: updated_values,
      sources: config.sources,
      last_modified: config.last_modified + 1
    }
  }
  
  let get_string_value = fn(config, key, default_value) {
    for (k, v) in config.values {
      if k == key {
        match v.string_value {
          Some(value) => return value
          None => ()
        }
      }
    }
    default_value
  }
  
  let get_int_value = fn(config, key, default_value) {
    for (k, v) in config.values {
      if k == key {
        match v.int_value {
          Some(value) => return value
          None => ()
        }
      }
    }
    default_value
  }
  
  // Test configuration loading
  let config = create_config()
  
  // Set configuration values
  let config1 = set_string_value(config, "app.name", "Azimuth")
  let config2 = set_int_value(config1, "app.port", 8080)
  let config3 = set_string_value(config2, "app.version", "1.0.0")
  
  // Get configuration values
  let app_name = get_string_value(config3, "app.name", "Unknown")
  let app_port = get_int_value(config3, "app.port", 3000)
  let app_version = get_string_value(config3, "app.version", "0.0.0")
  let app_description = get_string_value(config3, "app.description", "No description")
  
  // Verify values
  assert_eq(app_name, "Azimuth")
  assert_eq(app_port, 8080)
  assert_eq(app_version, "1.0.0")
  assert_eq(app_description, "No description")  // Default value
  
  // Verify configuration structure
  assert_eq(config3.values.length(), 3)
  assert_eq(config3.last_modified, 3)
}

// Test 2: Configuration Hierarchy and Inheritance
test "configuration hierarchy and inheritance" {
  type ConfigSection {
    name : String
    values : Array[(String, String)]
    subsections : Array[ConfigSection]
  }
  
  let create_section = fn(name) {
    { name: name, values: [], subsections: [] }
  }
  
  let add_value = fn(section, key, value) {
    let new_values = section.values @ [(key, value)]
    { name: section.name, values: new_values, subsections: section.subsections }
  }
  
  let add_subsection = fn(parent, child) {
    let new_subsections = parent.subsections @ [child]
    { name: parent.name, values: parent.values, subsections: new_subsections }
  }
  
  let find_value = fn(section, key) {
    // Search in current section
    for (k, v) in section.values {
      if k == key {
        return Some(v)
      }
    }
    
    // Search in subsections
    for subsection in section.subsections {
      match find_value(subsection, key) {
        Some(value) => return Some(value)
        None => ()
      }
    }
    
    None
  }
  
  let find_value_in_path = fn(root, path) {
    let mut current_section = root
    let path_parts = path.split(".")
    
    for i in 0..path_parts.length() - 1 {
      let section_name = path_parts[i]
      let mut found = false
      
      for subsection in current_section.subsections {
        if subsection.name == section_name {
          current_section = subsection
          found = true
          break
        }
      }
      
      if not found {
        return None
      }
    }
    
    let key = path_parts[path_parts.length() - 1]
    find_value(current_section, key)
  }
  
  // Create configuration hierarchy
  let root = create_section("root")
  let database = create_section("database")
  let server = create_section("server")
  let cache = create_section("cache")
  
  let database_with_values = add_value(add_value(database, "host", "localhost"), "port", "5432")
  let server_with_values = add_value(add_value(server, "host", "0.0.0.0"), "port", "8080")
  let cache_with_values = add_value(add_value(cache, "type", "redis"), "ttl", "3600")
  
  let root_with_db = add_subsection(root, database_with_values)
  let root_with_db_server = add_subsection(root_with_db, server_with_values)
  let full_config = add_subsection(root_with_db_server, cache_with_values)
  
  // Test value lookup
  match find_value_in_path(full_config, "database.host") {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  
  match find_value_in_path(full_config, "database.port") {
    Some(value) => assert_eq(value, "5432")
    None => assert_true(false)
  }
  
  match find_value_in_path(full_config, "server.host") {
    Some(value) => assert_eq(value, "0.0.0.0")
    None => assert_true(false)
  }
  
  match find_value_in_path(full_config, "cache.ttl") {
    Some(value) => assert_eq(value, "3600")
    None => assert_true(false)
  }
  
  // Test non-existent path
  match find_value_in_path(full_config, "nonexistent.key") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Configuration Validation
test "configuration validation" {
  type ValidationRule {
    name : String
    validator : String -> Option[String]  // Returns error message if invalid
  }
  
  type ValidationResult {
    is_valid : Bool
    errors : Array[String]
  }
  
  let create_validation_rule = fn(name, validator_fn) {
    { name: name, validator: validator_fn }
  }
  
  let validate_required = fn(value) {
    if value.length() == 0 {
      Some("Value is required")
    } else {
      None
    }
  }
  
  let validate_port = fn(value) {
    let port = value.to_int()
    if port < 1 or port > 65535 {
      Some("Port must be between 1 and 65535")
    } else {
      None
    }
  }
  
  let validate_email = fn(value) {
    if not value.contains("@") {
      Some("Invalid email format")
    } else {
      None
    }
  }
  
  let validate_url = fn(value) {
    if not (value.starts_with("http://") or value.starts_with("https://")) {
      Some("URL must start with http:// or https://")
    } else {
      None
    }
  }
  
  let validate_configuration = fn(config, rules) {
    let mut errors = []
    
    for (key, value) in config {
      for rule in rules {
        match rule.validator(value) {
          Some(error) => errors = errors @ [key + ": " + error]
          None => ()
        }
      }
    }
    
    { is_valid: errors.length() == 0, errors: errors }
  }
  
  // Create validation rules
  let required_rule = create_validation_rule("required", validate_required)
  let port_rule = create_validation_rule("port", validate_port)
  let email_rule = create_validation_rule("email", validate_email)
  let url_rule = create_validation_rule("url", validate_url)
  
  // Test valid configuration
  let valid_config = [
    ("app.name", "Azimuth"),
    ("server.port", "8080"),
    ("admin.email", "admin@example.com"),
    ("api.url", "https://api.example.com")
  ]
  
  let valid_rules = [required_rule, port_rule, email_rule, url_rule]
  let valid_result = validate_configuration(valid_config, valid_rules)
  
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid configuration
  let invalid_config = [
    ("app.name", ""),  // Empty (required)
    ("server.port", "70000"),  // Invalid port
    ("admin.email", "adminexample.com"),  // Invalid email
    ("api.url", "api.example.com")  // Invalid URL
  ]
  
  let invalid_result = validate_configuration(invalid_config, valid_rules)
  
  assert_false(invalid_result.is_valid)
  assert_eq(invalid_result.errors.length(), 4)
  assert_true(invalid_result.errors[0].contains("app.name"))
  assert_true(invalid_result.errors[1].contains("server.port"))
  assert_true(invalid_result.errors[2].contains("admin.email"))
  assert_true(invalid_result.errors[3].contains("api.url"))
}

// Test 4: Configuration Environment Overrides
test "configuration environment overrides" {
  type EnvironmentConfig {
    base_config : Array[(String, String)]
    environment_overrides : Array[(String, Array[(String, String)])]  // (environment, overrides)
    current_environment : String
  }
  
  let create_environment_config = fn(base_config, current_environment) {
    { 
      base_config: base_config, 
      environment_overrides: [], 
      current_environment: current_environment 
    }
  }
  
  let add_environment_override = fn(env_config, environment, overrides) {
    let new_overrides = env_config.environment_overrides @ [(environment, overrides)]
    { 
      base_config: env_config.base_config, 
      environment_overrides: new_overrides, 
      current_environment: env_config.current_environment 
    }
  }
  
  let get_env_config_value = fn(env_config, key) {
    // First check environment-specific overrides
    for (env, overrides) in env_config.environment_overrides {
      if env == env_config.current_environment {
        for (k, v) in overrides {
          if k == key {
            return v
          }
        }
      }
    }
    
    // Fall back to base configuration
    for (k, v) in env_config.base_config {
      if k == key {
        return v
      }
    }
    
    "NOT_FOUND"
  }
  
  // Create base configuration
  let base_config = [
    ("database.host", "localhost"),
    ("database.port", "5432"),
    ("api.url", "http://localhost:8080"),
    ("debug.mode", "false")
  ]
  
  let env_config = create_environment_config(base_config, "development")
  
  // Add environment overrides
  let dev_overrides = [
    ("database.host", "dev-db.example.com"),
    ("debug.mode", "true")
  ]
  
  let prod_overrides = [
    ("database.host", "prod-db.example.com"),
    ("api.url", "https://api.example.com"),
    ("debug.mode", "false")
  ]
  
  let env_config_with_dev = add_environment_override(env_config, "development", dev_overrides)
  let full_env_config = add_environment_override(env_config_with_dev, "production", prod_overrides)
  
  // Test development environment
  let dev_config = { 
    base_config: full_env_config.base_config, 
    environment_overrides: full_env_config.environment_overrides, 
    current_environment: "development" 
  }
  
  assert_eq(get_env_config_value(dev_config, "database.host"), "dev-db.example.com")
  assert_eq(get_env_config_value(dev_config, "database.port"), "5432")  // From base
  assert_eq(get_env_config_value(dev_config, "api.url"), "http://localhost:8080")  // From base
  assert_eq(get_env_config_value(dev_config, "debug.mode"), "true")
  
  // Test production environment
  let prod_config = { 
    base_config: full_env_config.base_config, 
    environment_overrides: full_env_config.environment_overrides, 
    current_environment: "production" 
  }
  
  assert_eq(get_env_config_value(prod_config, "database.host"), "prod-db.example.com")
  assert_eq(get_env_config_value(prod_config, "database.port"), "5432")  // From base
  assert_eq(get_env_config_value(prod_config, "api.url"), "https://api.example.com")
  assert_eq(get_env_config_value(prod_config, "debug.mode"), "false")
  
  // Test unknown environment (should use base config)
  let unknown_config = { 
    base_config: full_env_config.base_config, 
    environment_overrides: full_env_config.environment_overrides, 
    current_environment: "staging" 
  }
  
  assert_eq(get_env_config_value(unknown_config, "database.host"), "localhost")
  assert_eq(get_env_config_value(unknown_config, "database.port"), "5432")
  assert_eq(get_env_config_value(unknown_config, "api.url"), "http://localhost:8080")
  assert_eq(get_env_config_value(unknown_config, "debug.mode"), "false")
}

// Test 5: Configuration Hot Reload
test "configuration hot reload" {
  type ConfigWatcher {
    config : Array[(String, String)]
    last_modified : Int
    change_listeners : Array[Int -> Array[(String, String)] -> ()]  // (timestamp, config) -> ()
  }
  
  let create_config_watcher = fn(initial_config) {
    { 
      config: initial_config, 
      last_modified: 0, 
      change_listeners: [] 
    }
  }
  
  let add_change_listener = fn(watcher, listener) {
    let new_listeners = watcher.change_listeners @ [listener]
    { 
      config: watcher.config, 
      last_modified: watcher.last_modified, 
      change_listeners: new_listeners 
    }
  }
  
  let update_config = fn(watcher, new_config) {
    let new_timestamp = watcher.last_modified + 1
    let updated_watcher = { 
      config: new_config, 
      last_modified: new_timestamp, 
      change_listeners: watcher.change_listeners 
    }
    
    // Notify all listeners
    for listener in updated_watcher.change_listeners {
      listener(new_timestamp, new_config)
    }
    
    updated_watcher
  }
  
  let get_config_value = fn(watcher, key, default_value) {
    for (k, v) in watcher.config {
      if k == key {
        return v
      }
    }
    default_value
  }
  
  // Test configuration hot reload
  let initial_config = [
    ("app.name", "Azimuth"),
    ("app.version", "1.0.0"),
    ("server.port", "8080")
  ]
  
  let watcher = create_config_watcher(initial_config)
  
  // Add change listener
  let mut notification_count = 0
  let mut last_notified_timestamp = 0
  let mut last_notified_config = []
  
  let change_listener = fn(timestamp, config) {
    notification_count = notification_count + 1
    last_notified_timestamp = timestamp
    last_notified_config = config
  }
  
  let watcher_with_listener = add_change_listener(watcher, change_listener)
  
  // Update configuration
  let updated_config = [
    ("app.name", "Azimuth"),
    ("app.version", "1.1.0"),  // Changed
    ("server.port", "8080"),
    ("debug.mode", "true")      // Added
  ]
  
  let updated_watcher = update_config(watcher_with_listener, updated_config)
  
  // Verify notifications
  assert_eq(notification_count, 1)
  assert_eq(last_notified_timestamp, 1)
  assert_eq(last_notified_config.length(), 4)
  
  // Verify updated configuration
  assert_eq(get_config_value(updated_watcher, "app.version", "0.0.0"), "1.1.0")
  assert_eq(get_config_value(updated_watcher, "debug.mode", "false"), "true")
  assert_eq(updated_watcher.last_modified, 1)
  
  // Update again
  let final_config = [
    ("app.name", "Azimuth"),
    ("app.version", "1.2.0"),  // Changed again
    ("server.port", "9090"),   // Changed
    ("debug.mode", "true")
  ]
  
  let final_watcher = update_config(updated_watcher, final_config)
  
  // Verify second notification
  assert_eq(notification_count, 2)
  assert_eq(last_notified_timestamp, 2)
  assert_eq(get_config_value(final_watcher, "app.version", "0.0.0"), "1.2.0")
  assert_eq(get_config_value(final_watcher, "server.port", "8080"), "9090")
  assert_eq(final_watcher.last_modified, 2)
}

// Test 6: Configuration Schema Validation
test "configuration schema validation" {
  type SchemaType {
    type_name : String
    required : Bool
    default_value : Option[String]
    allowed_values : Option[Array[String]]
    min_value : Option[Int]
    max_value : Option[Int]
  }
  
  type ConfigSchema {
    fields : Array[(String, SchemaType)]
  }
  
  let create_schema_type = fn(type_name, required) {
    { 
      type_name: type_name, 
      required: required, 
      default_value: None, 
      allowed_values: None, 
      min_value: None, 
      max_value: None 
    }
  }
  
  let with_default = fn(schema_type, default_value) {
    { 
      type_name: schema_type.type_name, 
      required: schema_type.required, 
      default_value: Some(default_value), 
      allowed_values: schema_type.allowed_values, 
      min_value: schema_type.min_value, 
      max_value: schema_type.max_value 
    }
  }
  
  let with_allowed_values = fn(schema_type, allowed_values) {
    { 
      type_name: schema_type.type_name, 
      required: schema_type.required, 
      default_value: schema_type.default_value, 
      allowed_values: Some(allowed_values), 
      min_value: schema_type.min_value, 
      max_value: schema_type.max_value 
    }
  }
  
  let with_range = fn(schema_type, min_value, max_value) {
    { 
      type_name: schema_type.type_name, 
      required: schema_type.required, 
      default_value: schema_type.default_value, 
      allowed_values: schema_type.allowed_values, 
      min_value: Some(min_value), 
      max_value: Some(max_value) 
    }
  }
  
  let validate_against_schema = fn(config, schema) {
    let mut errors = []
    let mut validated_config = []
    
    // Check all fields in schema
    for (field_name, field_schema) in schema.fields {
      let mut found_in_config = false
      let mut field_value = ""
      
      // Check if field exists in config
      for (config_key, config_value) in config {
        if config_key == field_name {
          found_in_config = true
          field_value = config_value
          break
        }
      }
      
      if not found_in_config {
        if field_schema.required {
          match field_schema.default_value {
            Some(default) => {
              field_value = default
              validated_config = validated_config @ [(field_name, default)]
            }
            None => errors = errors @ ["Required field '" + field_name + "' is missing"]
          }
        } else {
          match field_schema.default_value {
            Some(default) => validated_config = validated_config @ [(field_name, default)]
            None => ()  // Optional field with no default, skip
          }
        }
      } else {
        // Field exists, validate its value
        validated_config = validated_config @ [(field_name, field_value)]
        
        // Type-specific validation
        match field_schema.type_name {
          "string" => {
            if field_value.length() == 0 and field_schema.required {
              errors = errors @ ["String field '" + field_name + "' cannot be empty"]
            }
          }
          "int" => {
            let int_value = field_value.to_int()
            match field_schema.min_value {
              Some(min) => {
                if int_value < min {
                  errors = errors @ ["Integer field '" + field_name + "' is below minimum value " + min.to_string()]
                }
              }
              None => ()
            }
            match field_schema.max_value {
              Some(max) => {
                if int_value > max {
                  errors = errors @ ["Integer field '" + field_name + "' is above maximum value " + max.to_string()]
                }
              }
              None => ()
            }
          }
          "enum" => {
            match field_schema.allowed_values {
              Some(allowed) => {
                let mut found = false
                for allowed_value in allowed {
                  if allowed_value == field_value {
                    found = true
                    break
                  }
                }
                if not found {
                  errors = errors @ ["Enum field '" + field_name + "' has invalid value '" + field_value + "'"]
                }
              }
              None => ()
            }
          }
          _ => ()
        }
      }
    }
    
    // Check for unknown fields in config
    for (config_key, _) in config {
      let mut found_in_schema = false
      for (schema_key, _) in schema.fields {
        if schema_key == config_key {
          found_in_schema = true
          break
        }
      }
      
      if not found_in_schema {
        errors = errors @ ["Unknown field '" + config_key + "' in configuration"]
      }
    }
    
    (errors.length() == 0, errors, validated_config)
  }
  
  // Create schema
  let string_type = create_schema_type("string", true)
  let optional_string_type = create_schema_type("string", false)
  let int_type = create_schema_type("int", true)
  let enum_type = create_schema_type("enum", true)
  
  let port_schema = with_range(int_type, 1, 65535)
  let log_level_schema = with_allowed_values(enum_type, ["debug", "info", "warn", "error"])
  let host_schema = with_default(optional_string_type, "localhost")
  
  let app_schema = {
    fields: [
      ("app.name", string_type),
      ("server.port", port_schema),
      ("database.host", host_schema),
      ("log.level", log_level_schema)
    ]
  }
  
  // Test valid configuration
  let valid_config = [
    ("app.name", "Azimuth"),
    ("server.port", "8080"),
    ("database.host", "db.example.com"),
    ("log.level", "info")
  ]
  
  let (valid_is_valid, valid_errors, valid_validated_config) = validate_against_schema(valid_config, app_schema)
  
  assert_true(valid_is_valid)
  assert_eq(valid_errors.length(), 0)
  assert_eq(valid_validated_config.length(), 4)
  
  // Test invalid configuration
  let invalid_config = [
    ("app.name", "Azimuth"),
    ("server.port", "70000"),  // Out of range
    ("log.level", "trace"),    // Invalid enum value
    ("unknown.field", "value") // Unknown field
    // Missing database.host (should use default)
  ]
  
  let (invalid_is_valid, invalid_errors, invalid_validated_config) = validate_against_schema(invalid_config, app_schema)
  
  assert_false(invalid_is_valid)
  assert_eq(invalid_errors.length(), 3)
  assert_true(invalid_errors[0].contains("server.port"))
  assert_true(invalid_errors[1].contains("log.level"))
  assert_true(invalid_errors[2].contains("unknown.field"))
  
  // Verify default value was applied
  let mut found_default_host = false
  for (key, value) in invalid_validated_config {
    if key == "database.host" and value == "localhost" {
      found_default_host = true
      break
    }
  }
  assert_true(found_default_host)
}

// Test 7: Configuration Encryption
test "configuration encryption" {
  // Simple XOR cipher for demonstration (not secure for production)
  let xor_encrypt = fn(text, key) {
    let mut result = ""
    let key_length = key.length()
    
    for i in 0..text.length() {
      let text_char = text[i]
      let key_char = key[i % key_length]
      let encrypted_char = text_char.to_int() ^ key_char.to_int()
      result = result + encrypted_char.to_string() + ","
    }
    
    result
  }
  
  let xor_decrypt = fn(encrypted_text, key) {
    let parts = encrypted_text.split(",")
    let mut result = ""
    let key_length = key.length()
    
    for i in 0..parts.length() - 1 {  // Last part is empty due to trailing comma
      if parts[i].length() > 0 {
        let encrypted_char = parts[i].to_int()
        let key_char = key[i % key_length]
        let decrypted_char = encrypted_char ^ key_char.to_int()
        result = result + decrypted_char.to_char().to_string()
      }
    }
    
    result
  }
  
  let encrypt_config_value = fn(value, key) {
    xor_encrypt(value, key)
  }
  
  let decrypt_config_value = fn(encrypted_value, key) {
    xor_decrypt(encrypted_value, key)
  }
  
  let encrypt_config = fn(config, encryption_keys) {
    let mut encrypted_config = []
    
    for (key, value) in config {
      let mut should_encrypt = false
      let mut encryption_key = ""
      
      // Check if this key should be encrypted
      for (pattern, key) in encryption_keys {
        if key.contains(pattern) {
          should_encrypt = true
          encryption_key = key
          break
        }
      }
      
      if should_encrypt {
        let encrypted_value = encrypt_config_value(value, encryption_key)
        encrypted_config = encrypted_config @ [(key, encrypted_value)]
      } else {
        encrypted_config = encrypted_config @ [(key, value)]
      }
    }
    
    encrypted_config
  }
  
  let decrypt_config = fn(encrypted_config, encryption_keys) {
    let mut decrypted_config = []
    
    for (key, value) in encrypted_config {
      let mut should_decrypt = false
      let mut decryption_key = ""
      
      // Check if this key should be decrypted
      for (pattern, key) in encryption_keys {
        if key.contains(pattern) {
          should_decrypt = true
          decryption_key = key
          break
        }
      }
      
      if should_decrypt {
        let decrypted_value = decrypt_config_value(value, decryption_key)
        decrypted_config = decrypted_config @ [(key, decrypted_value)]
      } else {
        decrypted_config = decrypted_config @ [(key, value)]
      }
    }
    
    decrypted_config
  }
  
  // Test configuration encryption
  let config = [
    ("app.name", "Azimuth"),
    ("database.password", "secret123"),
    ("api.key", "abc123xyz"),
    ("server.port", "8080")
  ]
  
  let encryption_keys = [
    ("password", "encryption_key_1"),
    ("key", "encryption_key_2")
  ]
  
  // Encrypt configuration
  let encrypted_config = encrypt_config(config, encryption_keys)
  
  // Verify encryption
  assert_eq(encrypted_config[0], ("app.name", "Azimuth"))  // Not encrypted
  assert_eq(encrypted_config[3], ("server.port", "8080"))  // Not encrypted
  
  // Password and API key should be encrypted
  assert_true(encrypted_config[1].1.contains(","))  // Encrypted values contain commas
  assert_true(encrypted_config[2].1.contains(","))
  
  assert_not_eq(encrypted_config[1].1, "secret123")
  assert_not_eq(encrypted_config[2].1, "abc123xyz")
  
  // Decrypt configuration
  let decrypted_config = decrypt_config(encrypted_config, encryption_keys)
  
  // Verify decryption
  assert_eq(decrypted_config[0], ("app.name", "Azimuth"))
  assert_eq(decrypted_config[1], ("database.password", "secret123"))
  assert_eq(decrypted_config[2], ("api.key", "abc123xyz"))
  assert_eq(decrypted_config[3], ("server.port", "8080"))
}

// Test 8: Configuration Merging
test "configuration merging" {
  let merge_configs = fn(base_config, override_config) {
    let mut merged_config = base_config
    
    for (key, value) in override_config {
      let mut found = false
      
      // Update existing key or add new one
      let mut updated_config = []
      for (k, v) in merged_config {
        if k == key {
          updated_config = updated_config @ [(k, value)]
          found = true
        } else {
          updated_config = updated_config @ [(k, v)]
        }
      }
      
      if not found {
        updated_config = updated_config @ [(key, value)]
      }
      
      merged_config = updated_config
    }
    
    merged_config
  }
  
  let merge_configs_with_strategy = fn(base_config, override_config, merge_strategy) {
    let mut merged_config = base_config
    
    for (key, value) in override_config {
      let mut found = false
      
      // Update existing key or add new one based on strategy
      let mut updated_config = []
      for (k, v) in merged_config {
        if k == key {
          match merge_strategy {
            "override" => updated_config = updated_config @ [(k, value)]
            "append" => updated_config = updated_config @ [(k, v + "," + value)]
            "prepend" => updated_config = updated_config @ [(k, value + "," + v)]
            _ => updated_config = updated_config @ [(k, value)]
          }
          found = true
        } else {
          updated_config = updated_config @ [(k, v)]
        }
      }
      
      if not found {
        updated_config = updated_config @ [(key, value)]
      }
      
      merged_config = updated_config
    }
    
    merged_config
  }
  
  // Test basic merging
  let base_config = [
    ("app.name", "Azimuth"),
    ("app.version", "1.0.0"),
    ("server.port", "8080"),
    ("database.host", "localhost")
  ]
  
  let override_config = [
    ("app.version", "1.1.0"),  // Override
    ("server.port", "9090"),   // Override
    ("debug.mode", "true")     // Add new
  ]
  
  let merged_config = merge_configs(base_config, override_config)
  
  // Verify merged configuration
  assert_eq(merged_config.length(), 5)
  assert_eq(merged_config[0], ("app.name", "Azimuth"))  // From base
  assert_eq(merged_config[1], ("app.version", "1.1.0"))  // From override
  assert_eq(merged_config[2], ("server.port", "9090"))   // From override
  assert_eq(merged_config[3], ("database.host", "localhost"))  // From base
  assert_eq(merged_config[4], ("debug.mode", "true"))    // From override
  
  // Test merge strategies
  let list_base_config = [
    ("app.features", "feature1,feature2"),
    ("app.modules", "module1,module2")
  ]
  
  let list_override_config = [
    ("app.features", "feature3"),
    ("app.modules", "module3")
  ]
  
  // Override strategy
  let override_merged = merge_configs_with_strategy(list_base_config, list_override_config, "override")
  assert_eq(override_merged[0], ("app.features", "feature3"))
  assert_eq(override_merged[1], ("app.modules", "module3"))
  
  // Append strategy
  let append_merged = merge_configs_with_strategy(list_base_config, list_override_config, "append")
  assert_eq(append_merged[0], ("app.features", "feature1,feature2,feature3"))
  assert_eq(append_merged[1], ("app.modules", "module1,module2,module3"))
  
  // Prepend strategy
  let prepend_merged = merge_configs_with_strategy(list_base_config, list_override_config, "prepend")
  assert_eq(prepend_merged[0], ("app.features", "feature3,feature1,feature2"))
  assert_eq(prepend_merged[1], ("app.modules", "module3,module1,module2"))
}

// Test 9: Configuration Templates
test "configuration templates" {
  type ConfigTemplate {
    name : String
    template : Array[(String, String)]
    variables : Array[String]  // Variable names that need to be substituted
  }
  
  let create_template = fn(name, template) {
    let mut variables = []
    
    // Extract variables from template (variables are in {{variable}} format)
    for (_, value) in template {
      let mut i = 0
      while i < value.length() - 1 {
        if value[i] == '{' and value[i + 1] == '{' {
          let mut j = i + 2
          while j < value.length() - 1 and not (value[j] == '}' and value[j + 1] == '}') {
            j = j + 1
          }
          
          if j < value.length() - 1 {
            let variable_name = value.substring(i + 2, j)
            let mut already_found = false
            
            for var in variables {
              if var == variable_name {
                already_found = true
                break
              }
            }
            
            if not already_found {
              variables = variables @ [variable_name]
            }
            
            i = j + 1
          } else {
            i = i + 1
          }
        } else {
          i = i + 1
        }
      }
    }
    
    { name: name, template: template, variables: variables }
  }
  
  let render_template = fn(template, variable_values) {
    let mut rendered_config = []
    
    for (key, value) in template.template {
      let mut rendered_value = value
      let mut i = 0
      
      while i < rendered_value.length() - 1 {
        if rendered_value[i] == '{' and rendered_value[i + 1] == '{' {
          let mut j = i + 2
          while j < rendered_value.length() - 1 and not (rendered_value[j] == '}' and rendered_value[j + 1] == '}') {
            j = j + 1
          }
          
          if j < rendered_value.length() - 1 {
            let variable_name = rendered_value.substring(i + 2, j)
            let mut replacement = ""
            
            // Find variable value
            for (var_name, var_value) in variable_values {
              if var_name == variable_name {
                replacement = var_value
                break
              }
            }
            
            rendered_value = rendered_value.substring(0, i) + replacement + rendered_value.substring(j + 2, rendered_value.length())
            i = i + replacement.length()
          } else {
            i = i + 1
          }
        } else {
          i = i + 1
        }
      }
      
      rendered_config = rendered_config @ [(key, rendered_value)]
    }
    
    rendered_config
  }
  
  // Create configuration template
  let app_template = create_template("web-app", [
    ("app.name", "{{APP_NAME}}"),
    ("server.host", "{{SERVER_HOST}}"),
    ("server.port", "{{SERVER_PORT}}"),
    ("database.url", "postgresql://{{DB_USER}}:{{DB_PASS}}@{{DB_HOST}}:{{DB_PORT}}/{{DB_NAME}}"),
    ("log.level", "{{LOG_LEVEL}}")
  ])
  
  // Verify template variables
  assert_eq(app_template.variables.length(), 7)
  assert_true(app_template.variables.contains("APP_NAME"))
  assert_true(app_template.variables.contains("SERVER_HOST"))
  assert_true(app_template.variables.contains("DB_USER"))
  
  // Render template with variable values
  let variable_values = [
    ("APP_NAME", "Azimuth"),
    ("SERVER_HOST", "0.0.0.0"),
    ("SERVER_PORT", "8080"),
    ("DB_USER", "azimuth_user"),
    ("DB_PASS", "secret123"),
    ("DB_HOST", "localhost"),
    ("DB_PORT", "5432"),
    ("DB_NAME", "azimuth_db"),
    ("LOG_LEVEL", "info")
  ]
  
  let rendered_config = render_template(app_template, variable_values)
  
  // Verify rendered configuration
  assert_eq(rendered_config.length(), 5)
  assert_eq(rendered_config[0], ("app.name", "Azimuth"))
  assert_eq(rendered_config[1], ("server.host", "0.0.0.0"))
  assert_eq(rendered_config[2], ("server.port", "8080"))
  assert_eq(rendered_config[3], ("database.url", "postgresql://azimuth_user:secret123@localhost:5432/azimuth_db"))
  assert_eq(rendered_config[4], ("log.level", "info"))
  
  // Test with missing variable (should leave placeholder)
  let incomplete_values = [
    ("APP_NAME", "Azimuth"),
    ("SERVER_HOST", "0.0.0.0")
    // Missing other variables
  ]
  
  let incomplete_config = render_template(app_template, incomplete_values)
  assert_eq(incomplete_config[0], ("app.name", "Azimuth"))
  assert_eq(incomplete_config[1], ("server.host", "0.0.0.0"))
  assert_eq(incomplete_config[2], ("server.port", "{{SERVER_PORT}}"))  // Unsubstituted
}

// Test 10: Configuration Versioning
test "configuration versioning" {
  type ConfigVersion {
    version : String
    config : Array[(String, String)]
    timestamp : Int
    changes : Array[String]  // Description of changes
  }
  
  type VersionedConfig {
    current_version : String
    versions : Array[ConfigVersion]
  }
  
  let create_versioned_config = fn(initial_version, initial_config) {
    let initial_config_version = {
      version: initial_version,
      config: initial_config,
      timestamp: 1000,  // Simulated timestamp
      changes: ["Initial configuration"]
    }
    
    {
      current_version: initial_version,
      versions: [initial_config_version]
    }
  }
  
  let update_config = fn(versioned_config, new_version, new_config, changes) {
    let new_config_version = {
      version: new_version,
      config: new_config,
      timestamp: 1000 + versioned_config.versions.length(),  // Incrementing timestamp
      changes: changes
    }
    
    {
      current_version: new_version,
      versions: versioned_config.versions @ [new_config_version]
    }
  }
  
  let get_config_version = fn(versioned_config, version) {
    for config_version in versioned_config.versions {
      if config_version.version == version {
        return Some(config_version)
      }
    }
    None
  }
  
  let get_current_config = fn(versioned_config) {
    get_config_version(versioned_config, versioned_config.current_version)
  }
  
  let rollback_to_version = fn(versioned_config, target_version) {
    match get_config_version(versioned_config, target_version) {
      Some(config_version) => {
        {
          current_version: target_version,
          versions: versioned_config.versions
        }
      }
      None => versioned_config  // Version not found, return unchanged
    }
  }
  
  let get_version_history = fn(versioned_config) {
    let mut history = []
    
    for config_version in versioned_config.versions {
      history = history @ [(config_version.version, config_version.timestamp, config_version.changes)]
    }
    
    history
  }
  
  // Test configuration versioning
  let initial_config = [
    ("app.name", "Azimuth"),
    ("app.version", "1.0.0"),
    ("server.port", "8080")
  ]
  
  let versioned_config = create_versioned_config("1.0.0", initial_config)
  
  assert_eq(versioned_config.current_version, "1.0.0")
  assert_eq(versioned_config.versions.length(), 1)
  
  // Update configuration
  let updated_config = [
    ("app.name", "Azimuth"),
    ("app.version", "1.1.0"),
    ("server.port", "8080"),
    ("debug.mode", "true")
  ]
  
  let versioned_config2 = update_config(
    versioned_config, 
    "1.1.0", 
    updated_config, 
    ["Updated app version to 1.1.0", "Added debug mode"]
  )
  
  assert_eq(versioned_config2.current_version, "1.1.0")
  assert_eq(versioned_config2.versions.length(), 2)
  
  // Update again
  let latest_config = [
    ("app.name", "Azimuth"),
    ("app.version", "1.2.0"),
    ("server.port", "9090"),  // Changed
    ("debug.mode", "true")
  ]
  
  let versioned_config3 = update_config(
    versioned_config2, 
    "1.2.0", 
    latest_config, 
    ["Updated app version to 1.2.0", "Changed server port to 9090"]
  )
  
  assert_eq(versioned_config3.current_version, "1.2.0")
  assert_eq(versioned_config3.versions.length(), 3)
  
  // Get current configuration
  match get_current_config(versioned_config3) {
    Some(current) => {
      assert_eq(current.version, "1.2.0")
      assert_eq(current.config.length(), 4)
      assert_eq(current.config[2], ("server.port", "9090"))
    }
    None => assert_true(false)
  }
  
  // Get specific version
  match get_config_version(versioned_config3, "1.0.0") {
    Some(v1) => {
      assert_eq(v1.version, "1.0.0")
      assert_eq(v1.config.length(), 3)
      assert_eq(v1.config[2], ("server.port", "8080"))
    }
    None => assert_true(false)
  }
  
  // Rollback to previous version
  let rolled_back_config = rollback_to_version(versioned_config3, "1.1.0")
  assert_eq(rolled_back_config.current_version, "1.1.0")
  
  match get_current_config(rolled_back_config) {
    Some(current) => {
      assert_eq(current.version, "1.1.0")
      assert_eq(current.config.length(), 4)
      assert_eq(current.config[2], ("server.port", "8080"))  // Port is back to 8080
    }
    None => assert_true(false)
  }
  
  // Get version history
  let history = get_version_history(rolled_back_config)
  assert_eq(history.length(), 3)
  assert_eq(history[0], ("1.0.0", 1000, ["Initial configuration"]))
  assert_eq(history[1], ("1.1.0", 1001, ["Updated app version to 1.1.0", "Added debug mode"]))
  assert_eq(history[2], ("1.2.0", 1002, ["Updated app version to 1.2.0", "Changed server port to 9090"]))
}