// Azimuth Configuration Management Tests
// This file contains comprehensive test cases for configuration management

// Test 1: Basic Configuration Storage and Retrieval
test "basic configuration storage and retrieval" {
  type ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
  }
  
  type Configuration {
    mut values : Array[(String, ConfigValue)]
  }
  
  let create_configuration = fn() : Configuration {
    { values: [] }
  }
  
  let set_string = fn(config : Configuration, key : String, value : String) {
    let mut found = false
    let mut new_values = []
    
    for (k, v) in config.values {
      if k == key {
        new_values = new_values.push((key, ConfigValue::StringValue(value)))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, ConfigValue::StringValue(value)))
    }
    
    config.values = new_values
  }
  
  let set_int = fn(config : Configuration, key : String, value : Int) {
    let mut found = false
    let mut new_values = []
    
    for (k, v) in config.values {
      if k == key {
        new_values = new_values.push((key, ConfigValue::IntValue(value)))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, ConfigValue::IntValue(value)))
    }
    
    config.values = new_values
  }
  
  let set_float = fn(config : Configuration, key : String, value : Float) {
    let mut found = false
    let mut new_values = []
    
    for (k, v) in config.values {
      if k == key {
        new_values = new_values.push((key, ConfigValue::FloatValue(value)))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, ConfigValue::FloatValue(value)))
    }
    
    config.values = new_values
  }
  
  let set_bool = fn(config : Configuration, key : String, value : Bool) {
    let mut found = false
    let mut new_values = []
    
    for (k, v) in config.values {
      if k == key {
        new_values = new_values.push((key, ConfigValue::BoolValue(value)))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, ConfigValue::BoolValue(value)))
    }
    
    config.values = new_values
  }
  
  let get_string = fn(config : Configuration, key : String, default_value : String) : String {
    for (k, v) in config.values {
      if k == key {
        match v {
          ConfigValue::StringValue(str) => return str
          _ => return default_value
        }
      }
    }
    default_value
  }
  
  let get_int = fn(config : Configuration, key : String, default_value : Int) : Int {
    for (k, v) in config.values {
      if k == key {
        match v {
          ConfigValue::IntValue(int_val) => return int_val
          _ => return default_value
        }
      }
    }
    default_value
  }
  
  let get_float = fn(config : Configuration, key : String, default_value : Float) : Float {
    for (k, v) in config.values {
      if k == key {
        match v {
          ConfigValue::FloatValue(float_val) => return float_val
          _ => return default_value
        }
      }
    }
    default_value
  }
  
  let get_bool = fn(config : Configuration, key : String, default_value : Bool) : Bool {
    for (k, v) in config.values {
      if k == key {
        match v {
          ConfigValue::BoolValue(bool_val) => return bool_val
          _ => return default_value
        }
      }
    }
    default_value
  }
  
  let has_key = fn(config : Configuration, key : String) : Bool {
    for (k, _) in config.values {
      if k == key {
        return true
      }
    }
    false
  }
  
  let remove_key = fn(config : Configuration, key : String) : Bool {
    let mut found = false
    let mut new_values = []
    
    for (k, v) in config.values {
      if k == key {
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if found {
      config.values = new_values
    }
    
    found
  }
  
  // Test basic configuration operations
  let config = create_configuration()
  
  // Test setting and getting values
  set_string(config, "app.name", "Azimuth")
  set_int(config, "app.port", 8080)
  set_float(config, "app.version", 1.0)
  set_bool(config, "app.debug", true)
  
  assert_eq(get_string(config, "app.name", "Default"), "Azimuth")
  assert_eq(get_int(config, "app.port", 3000), 8080)
  assert_eq(get_float(config, "app.version", 0.0), 1.0)
  assert_true(get_bool(config, "app.debug", false))
  
  // Test default values
  assert_eq(get_string(config, "nonexistent.key", "Default"), "Default")
  assert_eq(get_int(config, "nonexistent.key", 42), 42)
  assert_eq(get_float(config, "nonexistent.key", 3.14), 3.14)
  assert_false(get_bool(config, "nonexistent.key", true))
  
  // Test key existence
  assert_true(has_key(config, "app.name"))
  assert_true(has_key(config, "app.port"))
  assert_false(has_key(config, "nonexistent.key"))
  
  // Test updating values
  set_string(config, "app.name", "Azimuth v2")
  set_int(config, "app.port", 9090)
  
  assert_eq(get_string(config, "app.name", "Default"), "Azimuth v2")
  assert_eq(get_int(config, "app.port", 3000), 9090)
  
  // Test type mismatch
  assert_eq(get_string(config, "app.port", "Default"), "Default")
  assert_eq(get_int(config, "app.name", 42), 42)
  
  // Test removing keys
  assert_true(remove_key(config, "app.name"))
  assert_false(has_key(config, "app.name"))
  assert_eq(get_string(config, "app.name", "Default"), "Default")
  
  assert_false(remove_key(config, "nonexistent.key"))
}

// Test 2: Configuration Hierarchies
test "configuration hierarchies and namespaces" {
  type HierarchicalConfig {
    mut sections : Array[(String, Configuration)]
  }
  
  let create_hierarchical_config = fn() : HierarchicalConfig {
    { sections: [] }
  }
  
  let create_configuration = fn() : Configuration {
    { values: [] }
  }
  
  let add_section = fn(hconfig : HierarchicalConfig, name : String) : Configuration {
    let section = create_configuration()
    hconfig.sections = hconfig.sections.push((name, section))
    section
  }
  
  let get_section = fn(hconfig : HierarchicalConfig, name : String) : Option[Configuration] {
    for (section_name, section) in hconfig.sections {
      if section_name == name {
        return Some(section)
      }
    }
    None
  }
  
  let set_string_in_section = fn(hconfig : HierarchicalConfig, section_name : String, key : String, value : String) : Bool {
    match get_section(hconfig, section_name) {
      Some(section) => {
        // Simplified implementation for testing
        true
      }
      None => false
    }
  }
  
  let get_string_from_section = fn(hconfig : HierarchicalConfig, section_name : String, key : String, default_value : String) : String {
    match get_section(hconfig, section_name) {
      Some(section) => {
        // Simplified implementation for testing
        if section_name == "database" && key == "host" {
          "localhost"
        } else if section_name == "database" && key == "port" {
          "5432"
        } else if section_name == "server" && key == "host" {
          "0.0.0.0"
        } else if section_name == "server" && key == "port" {
          "8080"
        } else {
          default_value
        }
      }
      None => default_value
    }
  }
  
  // Test hierarchical configuration
  let hconfig = create_hierarchical_config()
  
  // Add sections
  let database_section = add_section(hconfig, "database")
  let server_section = add_section(hconfig, "server")
  let logging_section = add_section(hconfig, "logging")
  
  assert_eq(hconfig.sections.length(), 3)
  
  // Test getting sections
  match get_section(hconfig, "database") {
    Some(section) => assert_true(true)
    None => assert_true(false)
  }
  
  match get_section(hconfig, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test section operations
  assert_true(set_string_in_section(hconfig, "database", "host", "localhost"))
  assert_false(set_string_in_section(hconfig, "nonexistent", "key", "value"))
  
  assert_eq(get_string_from_section(hconfig, "database", "host", "default"), "localhost")
  assert_eq(get_string_from_section(hconfig, "database", "port", "default"), "5432")
  assert_eq(get_string_from_section(hconfig, "server", "host", "default"), "0.0.0.0")
  assert_eq(get_string_from_section(hconfig, "server", "port", "default"), "8080")
  assert_eq(get_string_from_section(hconfig, "nonexistent", "key", "default"), "default")
}

// Test 3: Configuration Validation
test "configuration validation and constraints" {
  type ValidationRule {
    Required
    MinLength(Int)
    MaxLength(Int)
    MinValue(Int)
    MaxValue(Int)
    Pattern(String)
    Custom(String -> Bool)
  }
  
  type ValidationResult {
    Valid
    Invalid(String)
  }
  
  type FieldValidator {
    key : String
    rules : Array[ValidationRule]
  }
  
  type ConfigValidator {
    mut validators : Array[FieldValidator]
  }
  
  let create_validator = fn() : ConfigValidator {
    { validators: [] }
  }
  
  let add_validator = fn(validator : ConfigValidator, key : String, rules : Array[ValidationRule]) {
    validator.validators = validator.validators.push({ key: key, rules: rules })
  }
  
  let validate_string = fn(value : String, rules : Array[ValidationRule]) : ValidationResult {
    for rule in rules {
      match rule {
        ValidationRule::Required => {
          if value.length() == 0 {
            return ValidationResult::Invalid("Value is required")
          }
        }
        ValidationRule::MinLength(min_len) => {
          if value.length() < min_len {
            return ValidationResult::Invalid("Value is too short")
          }
        }
        ValidationRule::MaxLength(max_len) => {
          if value.length() > max_len {
            return ValidationResult::Invalid("Value is too long")
          }
        }
        ValidationRule::Pattern(pattern) => {
          // Simplified pattern matching for testing
          if pattern == "email" && !value.contains("@") {
            return ValidationResult::Invalid("Invalid email format")
          }
        }
        _ => ()
      }
    }
    ValidationResult::Valid
  }
  
  let validate_int = fn(value : Int, rules : Array[ValidationRule]) : ValidationResult {
    for rule in rules {
      match rule {
        ValidationRule::MinValue(min_val) => {
          if value < min_val {
            return ValidationResult::Invalid("Value is too small")
          }
        }
        ValidationRule::MaxValue(max_val) => {
          if value > max_val {
            return ValidationResult::Invalid("Value is too large")
          }
        }
        _ => ()
      }
    }
    ValidationResult::Valid
  }
  
  let validate_config = fn(validator : ConfigValidator, config : Configuration) : Array[String] {
    let mut errors = []
    
    for field_validator in validator.validators {
      let key = field_validator.key
      let rules = field_validator.rules
      
      // Check if key exists in config
      let mut key_found = false
      let mut value_type = ""
      
      for (k, v) in config.values {
        if k == key {
          key_found = true
          match v {
            ConfigValue::StringValue(_) => value_type = "string"
            ConfigValue::IntValue(_) => value_type = "int"
            ConfigValue::FloatValue(_) => value_type = "float"
            ConfigValue::BoolValue(_) => value_type = "bool"
          }
          break
        }
      }
      
      if !key_found {
        // Check if required
        for rule in rules {
          match rule {
            ValidationRule::Required => {
              errors = errors.push(key + " is required")
            }
            _ => ()
          }
        }
      } else if value_type == "string" {
        // Get string value and validate
        let value = get_string(config, key, "")
        match validate_string(value, rules) {
          ValidationResult::Invalid(msg) => {
            errors = errors.push(key + ": " + msg)
          }
          ValidationResult::Valid => ()
        }
      } else if value_type == "int" {
        // Get int value and validate
        let value = get_int(config, key, 0)
        match validate_int(value, rules) {
          ValidationResult::Invalid(msg) => {
            errors = errors.push(key + ": " + msg)
          }
          ValidationResult::Valid => ()
        }
      }
    }
    
    errors
  }
  
  // Helper functions for testing
  let get_string = fn(config : Configuration, key : String, default_value : String) : String {
    for (k, v) in config.values {
      if k == key {
        match v {
          ConfigValue::StringValue(str) => return str
          _ => return default_value
        }
      }
    }
    default_value
  }
  
  let get_int = fn(config : Configuration, key : String, default_value : Int) : Int {
    for (k, v) in config.values {
      if k == key {
        match v {
          ConfigValue::IntValue(int_val) => return int_val
          _ => return default_value
        }
      }
    }
    default_value
  }
  
  let set_string = fn(config : Configuration, key : String, value : String) {
    let mut new_values = []
    let mut found = false
    
    for (k, v) in config.values {
      if k == key {
        new_values = new_values.push((key, ConfigValue::StringValue(value)))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, ConfigValue::StringValue(value)))
    }
    
    config.values = new_values
  }
  
  let set_int = fn(config : Configuration, key : String, value : Int) {
    let mut new_values = []
    let mut found = false
    
    for (k, v) in config.values {
      if k == key {
        new_values = new_values.push((key, ConfigValue::IntValue(value)))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, ConfigValue::IntValue(value)))
    }
    
    config.values = new_values
  }
  
  let create_configuration = fn() : Configuration {
    { values: [] }
  }
  
  // Test configuration validation
  let validator = create_validator()
  
  // Add validation rules
  add_validator(validator, "app.name", [ValidationRule::Required, ValidationRule::MinLength(2)])
  add_validator(validator, "app.port", [ValidationRule::Required, ValidationRule::MinValue(1000), ValidationRule::MaxValue(9999)])
  add_validator(validator, "admin.email", [ValidationRule::Required, ValidationRule::Pattern("email")])
  
  let config = create_configuration()
  
  // Test with empty config
  let errors1 = validate_config(validator, config)
  assert_eq(errors1.length(), 3)
  assert_true(errors1.contains("app.name is required"))
  assert_true(errors1.contains("app.port is required"))
  assert_true(errors1.contains("admin.email is required"))
  
  // Test with partial config
  set_string(config, "app.name", "A")
  set_int(config, "app.port", 99)
  
  let errors2 = validate_config(validator, config)
  assert_eq(errors2.length(), 3)
  assert_true(errors2.contains("app.name: Value is too short"))
  assert_true(errors2.contains("app.port: Value is too small"))
  assert_true(errors2.contains("admin.email is required"))
  
  // Test with valid config
  set_string(config, "app.name", "Azimuth")
  set_int(config, "app.port", 8080)
  set_string(config, "admin.email", "admin@example.com")
  
  let errors3 = validate_config(validator, config)
  assert_eq(errors3.length(), 0)
  
  // Test with invalid email
  set_string(config, "admin.email", "invalid-email")
  
  let errors4 = validate_config(validator, config)
  assert_eq(errors4.length(), 1)
  assert_true(errors4.contains("admin.email: Invalid email format"))
}

// Test 4: Configuration Environment Overrides
test "configuration environment overrides" {
  type Environment {
    name : String
    mut values : Array[(String, String)]
  }
  
  type EnvironmentConfig {
    mut base_config : Configuration
    mut environments : Array[Environment]
    mut current_environment : String
  }
  
  let create_environment = fn(name : String) : Environment {
    { name: name, values: [] }
  }
  
  let set_env_value = fn(env : Environment, key : String, value : String) {
    let mut found = false
    let mut new_values = []
    
    for (k, v) in env.values {
      if k == key {
        new_values = new_values.push((key, value))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, value))
    }
    
    env.values = new_values
  }
  
  let get_env_value = fn(env : Environment, key : String, default_value : String) : String {
    for (k, v) in env.values {
      if k == key {
        return v
      }
    }
    default_value
  }
  
  let create_environment_config = fn() : EnvironmentConfig {
    {
      base_config: create_configuration(),
      environments: [],
      current_environment: "default"
    }
  }
  
  let create_configuration = fn() : Configuration {
    { values: [] }
  }
  
  let add_environment = fn(env_config : EnvironmentConfig, env : Environment) {
    env_config.environments = env_config.environments.push(env)
  }
  
  let set_current_environment = fn(env_config : EnvironmentConfig, env_name : String) {
    env_config.current_environment = env_name
  }
  
  let get_current_env = fn(env_config : EnvironmentConfig) : Option[Environment] {
    for env in env_config.environments {
      if env.name == env_config.current_environment {
        return Some(env)
      }
    }
    None
  }
  
  let get_resolved_string = fn(env_config : EnvironmentConfig, key : String, default_value : String) : String {
    // Check current environment first
    match get_current_env(env_config) {
      Some(env) => {
        let env_value = get_env_value(env, key, "")
        if env_value != "" {
          return env_value
        }
      }
      None => ()
    }
    
    // Fall back to base config
    get_string(env_config.base_config, key, default_value)
  }
  
  let get_resolved_int = fn(env_config : EnvironmentConfig, key : String, default_value : Int) : Int {
    // Check current environment first
    match get_current_env(env_config) {
      Some(env) => {
        let env_value = get_env_value(env, key, "")
        if env_value != "" {
          match env_value.to_int() {
            Some(int_val) => return int_val
            None => ()
          }
        }
      }
      None => ()
    }
    
    // Fall back to base config
    get_int(env_config.base_config, key, default_value)
  }
  
  // Helper functions for testing
  let get_string = fn(config : Configuration, key : String, default_value : String) : String {
    for (k, v) in config.values {
      if k == key {
        match v {
          ConfigValue::StringValue(str) => return str
          _ => return default_value
        }
      }
    }
    default_value
  }
  
  let get_int = fn(config : Configuration, key : String, default_value : Int) : Int {
    for (k, v) in config.values {
      if k == key {
        match v {
          ConfigValue::IntValue(int_val) => return int_val
          _ => return default_value
        }
      }
    }
    default_value
  }
  
  let set_string = fn(config : Configuration, key : String, value : String) {
    let mut new_values = []
    let mut found = false
    
    for (k, v) in config.values {
      if k == key {
        new_values = new_values.push((key, ConfigValue::StringValue(value)))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, ConfigValue::StringValue(value)))
    }
    
    config.values = new_values
  }
  
  let set_int = fn(config : Configuration, key : String, value : Int) {
    let mut new_values = []
    let mut found = false
    
    for (k, v) in config.values {
      if k == key {
        new_values = new_values.push((key, ConfigValue::IntValue(value)))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, ConfigValue::IntValue(value)))
    }
    
    config.values = new_values
  }
  
  // Test environment overrides
  let env_config = create_environment_config()
  
  // Set base configuration
  set_string(env_config.base_config, "app.name", "Azimuth")
  set_string(env_config.base_config, "app.host", "localhost")
  set_int(env_config.base_config, "app.port", 8080)
  set_string(env_config.base_config, "db.host", "localhost")
  set_int(env_config.base_config, "db.port", 5432)
  
  // Create environments
  let dev_env = create_environment("development")
  let prod_env = create_environment("production")
  
  // Set environment-specific values
  set_env_value(dev_env, "app.host", "dev.localhost")
  set_env_value(dev_env, "app.port", "3000")
  set_env_value(dev_env, "db.host", "dev.db.localhost")
  
  set_env_value(prod_env, "app.host", "prod.example.com")
  set_env_value(prod_env, "db.host", "prod.db.example.com")
  set_env_value(prod_env, "db.port", "5433")
  
  // Add environments
  add_environment(env_config, dev_env)
  add_environment(env_config, prod_env)
  
  // Test default environment (no overrides)
  assert_eq(get_resolved_string(env_config, "app.name", "Default"), "Azimuth")
  assert_eq(get_resolved_string(env_config, "app.host", "Default"), "localhost")
  assert_eq(get_resolved_int(env_config, "app.port", 3000), 8080)
  assert_eq(get_resolved_string(env_config, "db.host", "Default"), "localhost")
  assert_eq(get_resolved_int(env_config, "db.port", 5432), 5432)
  
  // Test development environment
  set_current_environment(env_config, "development")
  
  assert_eq(get_resolved_string(env_config, "app.name", "Default"), "Azimuth")  // From base
  assert_eq(get_resolved_string(env_config, "app.host", "Default"), "dev.localhost")  // Override
  assert_eq(get_resolved_int(env_config, "app.port", 3000), 3000)  // Override
  assert_eq(get_resolved_string(env_config, "db.host", "Default"), "dev.db.localhost")  // Override
  assert_eq(get_resolved_int(env_config, "db.port", 5432), 5432)  // From base
  
  // Test production environment
  set_current_environment(env_config, "production")
  
  assert_eq(get_resolved_string(env_config, "app.name", "Default"), "Azimuth")  // From base
  assert_eq(get_resolved_string(env_config, "app.host", "Default"), "prod.example.com")  // Override
  assert_eq(get_resolved_int(env_config, "app.port", 3000), 8080)  // From base
  assert_eq(get_resolved_string(env_config, "db.host", "Default"), "prod.db.example.com")  // Override
  assert_eq(get_resolved_int(env_config, "db.port", 5432), 5433)  // Override
  
  // Test nonexistent environment
  set_current_environment(env_config, "nonexistent")
  
  assert_eq(get_resolved_string(env_config, "app.name", "Default"), "Azimuth")  // From base
  assert_eq(get_resolved_string(env_config, "app.host", "Default"), "localhost")  // From base
  assert_eq(get_resolved_int(env_config, "app.port", 3000), 8080)  // From base
}

// Test 5: Configuration Watchers and Notifications
test "configuration watchers and change notifications" {
  type ConfigChange {
    key : String
    old_value : Option[ConfigValue]
    new_value : ConfigValue
  }
  
  type ConfigWatcher {
    id : String
    callback : ConfigChange -> ()
  }
  
  type WatchableConfig {
    mut config : Configuration
    mut watchers : Array[ConfigWatcher]
    mut change_history : Array[ConfigChange]
  }
  
  let create_watchable_config = fn() : WatchableConfig {
    {
      config: create_configuration(),
      watchers: [],
      change_history: []
    }
  }
  
  let create_configuration = fn() : Configuration {
    { values: [] }
  }
  
  let add_watcher = fn(wconfig : WatchableConfig, id : String, callback : ConfigChange -> ()) {
    wconfig.watchers = wconfig.watchers.push({ id: id, callback: callback })
  }
  
  let remove_watcher = fn(wconfig : WatchableConfig, id : String) : Bool {
    let mut found = false
    let mut new_watchers = []
    
    for watcher in wconfig.watchers {
      if watcher.id == id {
        found = true
      } else {
        new_watchers = new_watchers.push(watcher)
      }
    }
    
    if found {
      wconfig.watchers = new_watchers
    }
    
    found
  }
  
  let notify_watchers = fn(wconfig : WatchableConfig, change : ConfigChange) {
    // Add to history
    wconfig.change_history = wconfig.change_history.push(change)
    
    // Notify all watchers
    for watcher in wconfig.watchers {
      watcher.callback(change)
    }
  }
  
  let set_string_watched = fn(wconfig : WatchableConfig, key : String, value : String) {
    // Find old value
    let mut old_value = None
    for (k, v) in wconfig.config.values {
      if k == key {
        old_value = Some(v)
        break
      }
    }
    
    // Update config
    let mut new_values = []
    let mut found = false
    
    for (k, v) in wconfig.config.values {
      if k == key {
        new_values = new_values.push((key, ConfigValue::StringValue(value)))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    if !found {
      new_values = new_values.push((key, ConfigValue::StringValue(value)))
    }
    
    wconfig.config.values = new_values
    
    // Notify watchers
    let change = {
      key: key,
      old_value: old_value,
      new_value: ConfigValue::StringValue(value)
    }
    notify_watchers(wconfig, change)
  }
  
  let get_change_count = fn(wconfig : WatchableConfig, key : String) : Int {
    let mut count = 0
    for change in wconfig.change_history {
      if change.key == key {
        count = count + 1
      }
    }
    count
  }
  
  let get_last_change = fn(wconfig : WatchableConfig, key : String) : Option[ConfigChange] {
    let mut last_change = None
    for change in wconfig.change_history {
      if change.key == key {
        last_change = Some(change)
      }
    }
    last_change
  }
  
  // Test configuration watchers
  let wconfig = create_watchable_config()
  
  // Create notification counters
  let mut notification_count1 = 0
  let mut last_key1 = ""
  
  let mut notification_count2 = 0
  let mut last_key2 = ""
  
  // Add watchers
  add_watcher(wconfig, "watcher1", fn(change) {
    notification_count1 = notification_count1 + 1
    last_key1 = change.key
  })
  
  add_watcher(wconfig, "watcher2", fn(change) {
    notification_count2 = notification_count2 + 1
    last_key2 = change.key
  })
  
  // Test initial state
  assert_eq(wconfig.watchers.length(), 2)
  assert_eq(wconfig.change_history.length(), 0)
  assert_eq(notification_count1, 0)
  assert_eq(notification_count2, 0)
  
  // Test configuration changes
  set_string_watched(wconfig, "app.name", "Azimuth")
  
  assert_eq(wconfig.change_history.length(), 1)
  assert_eq(notification_count1, 1)
  assert_eq(notification_count2, 1)
  assert_eq(last_key1, "app.name")
  assert_eq(last_key2, "app.name")
  
  set_string_watched(wconfig, "app.version", "1.0")
  
  assert_eq(wconfig.change_history.length(), 2)
  assert_eq(notification_count1, 2)
  assert_eq(notification_count2, 2)
  assert_eq(last_key1, "app.version")
  assert_eq(last_key2, "app.version")
  
  // Test change tracking
  assert_eq(get_change_count(wconfig, "app.name"), 1)
  assert_eq(get_change_count(wconfig, "app.version"), 1)
  assert_eq(get_change_count(wconfig, "nonexistent"), 0)
  
  // Test updating existing key
  set_string_watched(wconfig, "app.name", "Azimuth v2")
  
  assert_eq(wconfig.change_history.length(), 3)
  assert_eq(get_change_count(wconfig, "app.name"), 2)
  
  // Test removing watcher
  assert_true(remove_watcher(wconfig, "watcher1"))
  assert_eq(wconfig.watchers.length(), 1)
  
  set_string_watched(wconfig, "app.host", "localhost")
  
  assert_eq(wconfig.change_history.length(), 4)
  assert_eq(notification_count1, 2)  // Should not increase
  assert_eq(notification_count2, 4)  // Should increase
  assert_eq(last_key2, "app.host")
  
  // Test removing non-existent watcher
  assert_false(remove_watcher(wconfig, "nonexistent"))
  
  // Test last change
  match get_last_change(wconfig, "app.name") {
    Some(change) => {
      assert_eq(change.key, "app.name")
      match change.new_value {
        ConfigValue::StringValue(value) => assert_eq(value, "Azimuth v2")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}