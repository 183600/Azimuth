// Azimuth Configuration Management Test Suite
// This file contains test cases for configuration management mechanisms

// Test 1: Basic Configuration Storage
test "basic configuration storage" {
  // Simulate a configuration store
  type ConfigValue = {
    string_value: Option<String>,
    int_value: Option<Int>,
    bool_value: Option<Bool>
  }
  
  type ConfigStore = Array<(String, ConfigValue)>
  
  let create_store = fn() -> ConfigStore {
    []
  }
  
  let store_set_string = fn(store: ConfigStore, key: String, value: String) -> ConfigStore {
    let mut result = store
    let mut found = false
    
    for i in 0..<store.length() {
      let (k, _) = store[i]
      if k == key {
        let config_value = { 
          string_value: Some(value), 
          int_value: None, 
          bool_value: None 
        }
        result = result.with(i, (k, config_value))
        found = true
        break
      }
    }
    
    if !found {
      let config_value = { 
        string_value: Some(value), 
        int_value: None, 
        bool_value: None 
      }
      result = result.push((key, config_value))
    }
    
    result
  }
  
  let store_set_int = fn(store: ConfigStore, key: String, value: Int) -> ConfigStore {
    let mut result = store
    let mut found = false
    
    for i in 0..<store.length() {
      let (k, _) = store[i]
      if k == key {
        let config_value = { 
          string_value: None, 
          int_value: Some(value), 
          bool_value: None 
        }
        result = result.with(i, (k, config_value))
        found = true
        break
      }
    }
    
    if !found {
      let config_value = { 
        string_value: None, 
        int_value: Some(value), 
        bool_value: None 
      }
      result = result.push((key, config_value))
    }
    
    result
  }
  
  let store_set_bool = fn(store: ConfigStore, key: String, value: Bool) -> ConfigStore {
    let mut result = store
    let mut found = false
    
    for i in 0..<store.length() {
      let (k, _) = store[i]
      if k == key {
        let config_value = { 
          string_value: None, 
          int_value: None, 
          bool_value: Some(value) 
        }
        result = result.with(i, (k, config_value))
        found = true
        break
      }
    }
    
    if !found {
      let config_value = { 
        string_value: None, 
        int_value: None, 
        bool_value: Some(value) 
      }
      result = result.push((key, config_value))
    }
    
    result
  }
  
  let store_get_string = fn(store: ConfigStore, key: String, default_value: String) -> String {
    for (k, v) in store {
      if k == key {
        match v.string_value {
          Some(value) => return value
          None => {}
        }
      }
    }
    default_value
  }
  
  let store_get_int = fn(store: ConfigStore, key: String, default_value: Int) -> Int {
    for (k, v) in store {
      if k == key {
        match v.int_value {
          Some(value) => return value
          None => {}
        }
      }
    }
    default_value
  }
  
  let store_get_bool = fn(store: ConfigStore, key: String, default_value: Bool) -> Bool {
    for (k, v) in store {
      if k == key {
        match v.bool_value {
          Some(value) => return value
          None => {}
        }
      }
    }
    default_value
  }
  
  // Test configuration store
  let store1 = create_store()
  
  // Test default values
  assert_eq(store_get_string(store1, "name", "default"), "default")
  assert_eq(store_get_int(store1, "count", 0), 0)
  assert_false(store_get_bool(store1, "enabled", false))
  
  // Set and get values
  let store2 = store_set_string(store1, "name", "test")
  assert_eq(store_get_string(store2, "name", "default"), "test")
  
  let store3 = store_set_int(store2, "count", 42)
  assert_eq(store_get_int(store3, "count", 0), 42)
  
  let store4 = store_set_bool(store3, "enabled", true)
  assert_true(store_get_bool(store4, "enabled", false))
  
  // Update existing values
  let store5 = store_set_string(store4, "name", "updated")
  assert_eq(store_get_string(store5, "name", "default"), "updated")
  
  let store6 = store_set_int(store5, "count", 100)
  assert_eq(store_get_int(store6, "count", 0), 100)
  
  let store7 = store_set_bool(store6, "enabled", false)
  assert_false(store_get_bool(store7, "enabled", false))
}

// Test 2: Configuration Hierarchy
test "configuration hierarchy" {
  // Simulate hierarchical configuration
  type ConfigNode = {
    key: String,
    value: Option<String>,
    children: Array<ConfigNode>
  }
  
  let create_node = fn(key: String, value: Option<String>) -> ConfigNode {
    { key: key, value: value, children: [] }
  }
  
  let add_child = fn(parent: ConfigNode, child: ConfigNode) -> ConfigNode {
    { 
      key: parent.key, 
      value: parent.value, 
      children: parent.children.push(child) 
    }
  }
  
  let find_child = fn(node: ConfigNode, key: String) -> Option<ConfigNode> {
    for child in node.children {
      if child.key == key {
        return Some(child)
      }
    }
    None
  }
  
  let find_value = fn(node: ConfigNode, path: Array<String>) -> Option<String> {
    if path.length() == 0 {
      return node.value
    }
    
    let head = path[0]
    let tail = path.slice(1, path.length())
    
    match find_child(node, head) {
      Some(child) => find_value(child, tail)
      None => None
    }
  }
  
  let set_value = fn(node: ConfigNode, path: Array<String>, value: String) -> ConfigNode {
    if path.length() == 0 {
      return { key: node.key, value: Some(value), children: node.children }
    }
    
    let head = path[0]
    let tail = path.slice(1, path.length())
    
    match find_child(node, head) {
      Some(child) => {
        let updated_child = set_value(child, tail, value)
        let mut new_children = []
        for c in node.children {
          if c.key == head {
            new_children = new_children.push(updated_child)
          } else {
            new_children = new_children.push(c)
          }
        }
        { key: node.key, value: node.value, children: new_children }
      }
      None => {
        if tail.length() == 0 {
          let new_child = create_node(head, Some(value))
          add_child(node, new_child)
        } else {
          let new_child = set_value(create_node(head, None), tail, value)
          add_child(node, new_child)
        }
      }
    }
  }
  
  // Test configuration hierarchy
  let root = create_node("root", None)
  
  // Add nested configuration
  let root1 = set_value(root, ["database", "host"], "localhost")
  let root2 = set_value(root1, ["database", "port"], "5432")
  let root3 = set_value(root2, ["server", "host"], "0.0.0.0")
  let root4 = set_value(root3, ["server", "port"], "8080")
  
  // Get values
  match find_value(root4, ["database", "host"]) {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  
  match find_value(root4, ["database", "port"]) {
    Some(value) => assert_eq(value, "5432")
    None => assert_true(false)
  }
  
  match find_value(root4, ["server", "host"]) {
    Some(value) => assert_eq(value, "0.0.0.0")
    None => assert_true(false)
  }
  
  match find_value(root4, ["server", "port"]) {
    Some(value) => assert_eq(value, "8080")
    None => assert_true(false)
  }
  
  // Test non-existent paths
  match find_value(root4, ["nonexistent"]) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match find_value(root4, ["database", "nonexistent"]) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Update existing values
  let root5 = set_value(root4, ["database", "host"], "remotehost")
  match find_value(root5, ["database", "host"]) {
    Some(value) => assert_eq(value, "remotehost")
    None => assert_true(false)
  }
}

// Test 3: Configuration Validation
test "configuration validation" {
  // Simulate configuration validation
  type ValidationRule = {
    key: String,
    validator: (String) -> Bool,
    error_message: String
  }
  
  type ValidationResult = {
    is_valid: Bool,
    errors: Array<String>
  }
  
  let validate_config = fn(config: Array<(String, String)>, rules: Array<ValidationRule>) -> ValidationResult {
    let mut errors = []
    
    for rule in rules {
      let mut found = false
      for (key, value) in config {
        if key == rule.key {
          found = true
          if !rule.validator(value) {
            errors = errors.push(rule.error_message)
          }
          break
        }
      }
      
      if !found {
        errors = errors.push("Missing required configuration: " + rule.key)
      }
    }
    
    { is_valid: errors.length() == 0, errors: errors }
  }
  
  // Define validation rules
  let rules = [
    { 
      key: "host", 
      validator: fn(s) { s.length() > 0 }, 
      error_message: "Host cannot be empty" 
    },
    { 
      key: "port", 
      validator: fn(s) { 
        match s {
          "80" => true
          "443" => true
          "8080" => true
          "3000" => true
          _ => false
        }
      }, 
      error_message: "Port must be a valid port number" 
    },
    { 
      key: "timeout", 
      validator: fn(s) { 
        match s {
          "30" => true
          "60" => true
          "90" => true
          "120" => true
          _ => false
        }
      }, 
      error_message: "Timeout must be 30, 60, 90, or 120 seconds" 
    }
  ]
  
  // Test valid configuration
  let valid_config = [
    ("host", "localhost"),
    ("port", "8080"),
    ("timeout", "60")
  ]
  
  let valid_result = validate_config(valid_config, rules)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid configuration
  let invalid_config = [
    ("host", ""),
    ("port", "9999"),
    ("timeout", "45")
  ]
  
  let invalid_result = validate_config(invalid_config, rules)
  assert_false(invalid_result.is_valid)
  assert_eq(invalid_result.errors.length(), 4)  // 3 validation errors + 1 missing config
  
  // Test missing configuration
  let incomplete_config = [
    ("host", "localhost")
    // Missing port and timeout
  ]
  
  let incomplete_result = validate_config(incomplete_config, rules)
  assert_false(incomplete_result.is_valid)
  assert_eq(incomplete_result.errors.length(), 2)  // 2 missing configs
}

// Test 4: Environment-Specific Configuration
test "environment-specific configuration" {
  // Simulate environment-specific configuration
  type EnvironmentConfig = {
    environment: String,
    configs: Array<(String, String)>
  }
  
  type ConfigManager = {
    default_config: Array<(String, String)>,
    environment_configs: Array<EnvironmentConfig>
  }
  
  let create_manager = fn(default_config: Array<(String, String)>) -> ConfigManager {
    { default_config: default_config, environment_configs: [] }
  }
  
  let add_environment_config = fn(manager: ConfigManager, environment: String, configs: Array<(String, String)>) -> ConfigManager {
    let env_config = { environment: environment, configs: configs }
    { 
      default_config: manager.default_config, 
      environment_configs: manager.environment_configs.push(env_config) 
    }
  }
  
  let get_config = fn(manager: ConfigManager, environment: String, key: String) -> Option<String> {
    // First try to find in environment-specific config
    for env_config in manager.environment_configs {
      if env_config.environment == environment {
        for (k, v) in env_config.configs {
          if k == key {
            return Some(v)
          }
        }
      }
    }
    
    // Fall back to default config
    for (k, v) in manager.default_config {
      if k == key {
        return Some(v)
      }
    }
    
    None
  }
  
  let get_all_config = fn(manager: ConfigManager, environment: String) -> Array<(String, String)> {
    let mut result = manager.default_config
    let mut keys = []
    
    // Collect all keys from default config
    for (k, _) in manager.default_config {
      keys = keys.push(k)
    }
    
    // Override with environment-specific config
    for env_config in manager.environment_configs {
      if env_config.environment == environment {
        for (k, v) in env_config.configs {
          let mut found = false
          for i in 0..<result.length() {
            if result[i].0 == k {
              result = result.with(i, (k, v))
              found = true
              break
            }
          }
          
          if !found {
            result = result.push((k, v))
          }
        }
      }
    }
    
    result
  }
  
  // Test environment-specific configuration
  let default_config = [
    ("host", "localhost"),
    ("port", "8080"),
    ("debug", "false")
  ]
  
  let manager1 = create_manager(default_config)
  
  let dev_config = [
    ("host", "dev-host"),
    ("debug", "true")
  ]
  
  let prod_config = [
    ("host", "prod-host"),
    ("port", "443")
  ]
  
  let manager2 = add_environment_config(manager1, "development", dev_config)
  let manager3 = add_environment_config(manager2, "production", prod_config)
  
  // Test getting config values
  match get_config(manager3, "development", "host") {
    Some(value) => assert_eq(value, "dev-host")
    None => assert_true(false)
  }
  
  match get_config(manager3, "development", "port") {
    Some(value) => assert_eq(value, "8080")  // From default
    None => assert_true(false)
  }
  
  match get_config(manager3, "development", "debug") {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  match get_config(manager3, "production", "host") {
    Some(value) => assert_eq(value, "prod-host")
    None => assert_true(false)
  }
  
  match get_config(manager3, "production", "port") {
    Some(value) => assert_eq(value, "443")
    None => assert_true(false)
  }
  
  match get_config(manager3, "production", "debug") {
    Some(value) => assert_eq(value, "false")  // From default
    None => assert_true(false)
  }
  
  // Test getting all config for an environment
  let dev_all_config = get_all_config(manager3, "development")
  assert_eq(dev_all_config.length(), 3)
  
  let prod_all_config = get_all_config(manager3, "production")
  assert_eq(prod_all_config.length(), 3)
}

// Test 5: Configuration Watcher
test "configuration watcher" {
  // Simulate a configuration watcher
  type ConfigChange = {
    key: String,
    old_value: Option<String>,
    new_value: Option<String>
  }
  
  type ConfigWatcher = {
    config: Array<(String, String)>,
    changes: Array<ConfigChange>
  }
  
  let create_watcher = fn(initial_config: Array<(String, String)>) -> ConfigWatcher {
    { config: initial_config, changes: [] }
  }
  
  let watcher_get = fn(watcher: ConfigWatcher, key: String) -> Option<String> {
    for (k, v) in watcher.config {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  let watcher_set = fn(watcher: ConfigWatcher, key: String, value: String) -> ConfigWatcher {
    let old_value = watcher_get(watcher, key)
    let mut found = false
    let mut new_config = watcher.config
    
    for i in 0..<watcher.config.length() {
      let (k, v) = watcher.config[i]
      if k == key {
        new_config = new_config.with(i, (k, value))
        found = true
        break
      }
    }
    
    if !found {
      new_config = new_config.push((key, value))
    }
    
    let change = { 
      key: key, 
      old_value: old_value, 
      new_value: Some(value) 
    }
    
    { 
      config: new_config, 
      changes: watcher.changes.push(change) 
    }
  }
  
  let watcher_remove = fn(watcher: ConfigWatcher, key: String) -> ConfigWatcher {
    let old_value = watcher_get(watcher, key)
    let mut new_config = []
    let mut found = false
    
    for (k, v) in watcher.config {
      if k == key {
        found = true
      } else {
        new_config = new_config.push((k, v))
      }
    }
    
    if found {
      let change = { 
        key: key, 
        old_value: old_value, 
        new_value: None 
      }
      
      { 
        config: new_config, 
        changes: watcher.changes.push(change) 
      }
    } else {
      watcher
    }
  }
  
  let watcher_get_changes = fn(watcher: ConfigWatcher) -> Array<ConfigChange> {
    watcher.changes
  }
  
  let watcher_clear_changes = fn(watcher: ConfigWatcher) -> ConfigWatcher {
    { config: watcher.config, changes: [] }
  }
  
  // Test configuration watcher
  let initial_config = [
    ("host", "localhost"),
    ("port", "8080")
  ]
  
  let watcher1 = create_watcher(initial_config)
  
  // Get initial values
  match watcher_get(watcher1, "host") {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  
  match watcher_get(watcher1, "port") {
    Some(value) => assert_eq(value, "8080")
    None => assert_true(false)
  }
  
  // Set new values
  let watcher2 = watcher_set(watcher1, "host", "remotehost")
  let watcher3 = watcher_set(watcher2, "port", "9090")
  let watcher4 = watcher_set(watcher3, "debug", "true")  // New key
  
  // Check values were updated
  match watcher_get(watcher4, "host") {
    Some(value) => assert_eq(value, "remotehost")
    None => assert_true(false)
  }
  
  match watcher_get(watcher4, "port") {
    Some(value) => assert_eq(value, "9090")
    None => assert_true(false)
  }
  
  match watcher_get(watcher4, "debug") {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  // Check changes
  let changes = watcher_get_changes(watcher4)
  assert_eq(changes.length(), 3)
  
  assert_eq(changes[0].key, "host")
  match changes[0].old_value {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  match changes[0].new_value {
    Some(value) => assert_eq(value, "remotehost")
    None => assert_true(false)
  }
  
  assert_eq(changes[1].key, "port")
  match changes[1].old_value {
    Some(value) => assert_eq(value, "8080")
    None => assert_true(false)
  }
  match changes[1].new_value {
    Some(value) => assert_eq(value, "9090")
    None => assert_true(false)
  }
  
  assert_eq(changes[2].key, "debug")
  match changes[2].old_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match changes[2].new_value {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  // Remove a key
  let watcher5 = watcher_remove(watcher4, "host")
  
  match watcher_get(watcher5, "host") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Check changes after removal
  let changes_after_removal = watcher_get_changes(watcher5)
  assert_eq(changes_after_removal.length(), 4)
  
  assert_eq(changes_after_removal[3].key, "host")
  match changes_after_removal[3].old_value {
    Some(value) => assert_eq(value, "remotehost")
    None => assert_true(false)
  }
  match changes_after_removal[3].new_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Clear changes
  let watcher6 = watcher_clear_changes(watcher5)
  let cleared_changes = watcher_get_changes(watcher6)
  assert_eq(cleared_changes.length(), 0)
}

// Test 6: Configuration Template
test "configuration template" {
  // Simulate configuration templates
  type ConfigTemplate = {
    name: String,
    variables: Array<String>,
    template: Array<(String, String)>
  }
  
  type TemplateInstance = Array<(String, String)>
  
  let create_template = fn(name: String, variables: Array<String>, template: Array<(String, String)>) -> ConfigTemplate {
    { name: name, variables: variables, template: template }
  }
  
  let instantiate_template = fn(template: ConfigTemplate, values: Array<(String, String)>) -> TemplateInstance {
    let mut result = []
    
    for (key, value) in template.template {
      let mut new_value = value
      
      for (var_name, var_value) in values {
        let placeholder = "${" + var_name + "}"
        let mut updated_value = ""
        let mut i = 0
        
        while i < new_value.length() {
          if i <= new_value.length() - placeholder.length() && 
             new_value.substring(i, placeholder.length()) == placeholder {
            updated_value = updated_value + var_value
            i = i + placeholder.length()
          } else {
            updated_value = updated_value + new_value.substring(i, 1)
            i = i + 1
          }
        }
        
        new_value = updated_value
      }
      
      result = result.push((key, new_value))
    }
    
    result
  }
  
  // Test configuration template
  let db_template = create_template(
    "database",
    ["host", "port", "name"],
    [
      ("db.host", "${host}"),
      ("db.port", "${port}"),
      ("db.url", "postgresql://${host}:${port}/${name}"),
      ("db.pool_size", "10")
    ]
  )
  
  // Instantiate template with values
  let values = [
    ("host", "localhost"),
    ("port", "5432"),
    ("name", "mydb")
  ]
  
  let instance = instantiate_template(db_template, values)
  
  assert_eq(instance.length(), 4)
  
  let find_value = fn(instance: TemplateInstance, key: String) -> Option<String> {
    for (k, v) in instance {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  match find_value(instance, "db.host") {
    Some(value) => assert_eq(value, "localhost")
    None => assert_true(false)
  }
  
  match find_value(instance, "db.port") {
    Some(value) => assert_eq(value, "5432")
    None => assert_true(false)
  }
  
  match find_value(instance, "db.url") {
    Some(value) => assert_eq(value, "postgresql://localhost:5432/mydb")
    None => assert_true(false)
  }
  
  match find_value(instance, "db.pool_size") {
    Some(value) => assert_eq(value, "10")
    None => assert_true(false)
  }
  
  // Test with different values
  let prod_values = [
    ("host", "prod-db.example.com"),
    ("port", "5432"),
    ("name", "production")
  ]
  
  let prod_instance = instantiate_template(db_template, prod_values)
  
  match find_value(prod_instance, "db.host") {
    Some(value) => assert_eq(value, "prod-db.example.com")
    None => assert_true(false)
  }
  
  match find_value(prod_instance, "db.url") {
    Some(value) => assert_eq(value, "postgresql://prod-db.example.com:5432/production")
    None => assert_true(false)
  }
}

// Test 7: Configuration Encryption
test "configuration encryption" {
  // Simulate simple configuration encryption
  let encrypt = fn(value: String) -> String {
    let mut result = ""
    for i in 0..<value.length() {
      let char = value.substring(i, 1)
      let char_code = match char {
        "a" => 0
        "b" => 1
        "c" => 2
        "d" => 3
        "e" => 4
        "f" => 5
        "g" => 6
        "h" => 7
        "i" => 8
        "j" => 9
        "k" => 10
        "l" => 11
        "m" => 12
        "n" => 13
        "o" => 14
        "p" => 15
        "q" => 16
        "r" => 17
        "s" => 18
        "t" => 19
        "u" => 20
        "v" => 21
        "w" => 22
        "x" => 23
        "y" => 24
        "z" => 25
        "0" => 26
        "1" => 27
        "2" => 28
        "3" => 29
        "4" => 30
        "5" => 31
        "6" => 32
        "7" => 33
        "8" => 34
        "9" => 35
        _ => 36
      }
      let encrypted_code = (char_code + 1) % 37
      let encrypted_char = match encrypted_code {
        0 => "a"
        1 => "b"
        2 => "c"
        3 => "d"
        4 => "e"
        5 => "f"
        6 => "g"
        7 => "h"
        8 => "i"
        9 => "j"
        10 => "k"
        11 => "l"
        12 => "m"
        13 => "n"
        14 => "o"
        15 => "p"
        16 => "q"
        17 => "r"
        18 => "s"
        19 => "t"
        20 => "u"
        21 => "v"
        22 => "w"
        23 => "x"
        24 => "y"
        25 => "z"
        26 => "0"
        27 => "1"
        28 => "2"
        29 => "3"
        30 => "4"
        31 => "5"
        32 => "6"
        33 => "7"
        34 => "8"
        35 => "9"
        _ => "?"
      }
      result = result + encrypted_char
    }
    "enc:" + result
  }
  
  let decrypt = fn(encrypted_value: String) -> String {
    if encrypted_value.length() > 4 && encrypted_value.substring(0, 4) == "enc:" {
      let encrypted = encrypted_value.substring(4, encrypted_value.length() - 4)
      let mut result = ""
      
      for i in 0..<encrypted.length() {
        let char = encrypted.substring(i, 1)
        let char_code = match char {
          "a" => 0
          "b" => 1
          "c" => 2
          "d" => 3
          "e" => 4
          "f" => 5
          "g" => 6
          "h" => 7
          "i" => 8
          "j" => 9
          "k" => 10
          "l" => 11
          "m" => 12
          "n" => 13
          "o" => 14
          "p" => 15
          "q" => 16
          "r" => 17
          "s" => 18
          "t" => 19
          "u" => 20
          "v" => 21
          "w" => 22
          "x" => 23
          "y" => 24
          "z" => 25
          "0" => 26
          "1" => 27
          "2" => 28
          "3" => 29
          "4" => 30
          "5" => 31
          "6" => 32
          "7" => 33
          "8" => 34
          "9" => 35
          _ => 36
        }
        let decrypted_code = (char_code - 1 + 37) % 37
        let decrypted_char = match decrypted_code {
          0 => "a"
          1 => "b"
          2 => "c"
          3 => "d"
          4 => "e"
          5 => "f"
          6 => "g"
          7 => "h"
          8 => "i"
          9 => "j"
          10 => "k"
          11 => "l"
          12 => "m"
          13 => "n"
          14 => "o"
          15 => "p"
          16 => "q"
          17 => "r"
          18 => "s"
          19 => "t"
          20 => "u"
          21 => "v"
          22 => "w"
          23 => "x"
          24 => "y"
          25 => "z"
          26 => "0"
          27 => "1"
          28 => "2"
          29 => "3"
          30 => "4"
          31 => "5"
          32 => "6"
          33 => "7"
          34 => "8"
          35 => "9"
          _ => "?"
        }
        result = result + decrypted_char
      }
      
      result
    } else {
      encrypted_value
    }
  }
  
  // Test encryption and decryption
  let original_value = "password123"
  let encrypted_value = encrypt(original_value)
  assert_eq(encrypted_value, "enc:qbttxpse234")
  
  let decrypted_value = decrypt(encrypted_value)
  assert_eq(decrypted_value, "password123")
  
  // Test with non-encrypted values
  let non_encrypted = "plaintext"
  let decrypted_non_encrypted = decrypt(non_encrypted)
  assert_eq(decrypted_non_encrypted, "plaintext")
}

// Test 8: Configuration Import/Export
test "configuration import/export" {
  // Simulate configuration import/export
  type ConfigFormat = {
    version: String,
    timestamp: String,
    config: Array<(String, String)>
  }
  
  let export_config = fn(config: Array<(String, String)>, version: String, timestamp: String) -> String {
    let mut result = "version:" + version + "\n"
    result = result + "timestamp:" + timestamp + "\n"
    result = result + "config:\n"
    
    for (key, value) in config {
      result = result + key + "=" + value + "\n"
    }
    
    result
  }
  
  let import_config = fn(exported: String) -> Option<ConfigFormat> {
    let lines = []
    let mut current = ""
    
    for i in 0..<exported.length() {
      let char = exported.substring(i, 1)
      if char == "\n" {
        lines = lines.push(current)
        current = ""
      } else {
        current = current + char
      }
    }
    
    if current.length() > 0 {
      lines = lines.push(current)
    }
    
    if lines.length() < 3 {
      return None
    }
    
    let version_line = lines[0]
    let timestamp_line = lines[1]
    
    let version = if version_line.length() > 8 && version_line.substring(0, 8) == "version:" {
      version_line.substring(8, version_line.length() - 8)
    } else {
      return None
    }
    
    let timestamp = if timestamp_line.length() > 10 && timestamp_line.substring(0, 10) == "timestamp:" {
      timestamp_line.substring(10, timestamp_line.length() - 10)
    } else {
      return None
    }
    
    if lines[2] != "config:" {
      return None
    }
    
    let mut config = []
    for i in 3..<lines.length() {
      let line = lines[i]
      let eq_pos = line.find("=")
      match eq_pos {
        Some(pos) => {
          let key = line.substring(0, pos)
          let value = line.substring(pos + 1, line.length() - pos - 1)
          config = config.push((key, value))
        }
        None => {}
      }
    }
    
    Some({ version: version, timestamp: timestamp, config: config })
  }
  
  // Test configuration export/import
  let original_config = [
    ("host", "localhost"),
    ("port", "8080"),
    ("debug", "true")
  ]
  
  let exported = export_config(original_config, "1.0", "2023-01-01T00:00:00Z")
  
  let imported = import_config(exported)
  match imported {
    Some(config_format) => {
      assert_eq(config_format.version, "1.0")
      assert_eq(config_format.timestamp, "2023-01-01T00:00:00Z")
      assert_eq(config_format.config.length(), 3)
      
      let find_value = fn(config: Array<(String, String)>, key: String) -> Option<String> {
        for (k, v) in config {
          if k == key {
            return Some(v)
          }
        }
        None
      }
      
      match find_value(config_format.config, "host") {
        Some(value) => assert_eq(value, "localhost")
        None => assert_true(false)
      }
      
      match find_value(config_format.config, "port") {
        Some(value) => assert_eq(value, "8080")
        None => assert_true(false)
      }
      
      match find_value(config_format.config, "debug") {
        Some(value) => assert_eq(value, "true")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test invalid import
  let invalid_exported = "invalid format"
  let invalid_imported = import_config(invalid_exported)
  match invalid_imported {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}