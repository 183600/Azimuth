// Azimuth Telemetry System - High Quality Configuration Management Tests
// This file contains comprehensive test cases for configuration management functionality

// Test 1: Basic Configuration Loading and Retrieval
test "basic configuration loading and retrieval" {
  // Test configuration creation
  let config = Configuration::new()
  
  // Set basic configuration values
  Configuration::set_string(config, "service.name", "azimuth-service")
  Configuration::set_string(config, "service.version", "1.0.0")
  Configuration::set_string(config, "service.namespace", "production")
  
  Configuration::set_int(config, "server.port", 8080)
  Configuration::set_int(config, "server.max_connections", 1000)
  
  Configuration::set_bool(config, "debug.enabled", false)
  Configuration::set_bool(config, "metrics.enabled", true)
  
  Configuration::set_float(config, "sampling.ratio", 0.5)
  Configuration::set_float(config, "timeout.seconds", 30.0)
  
  // Verify string values
  let service_name = Configuration::get_string(config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "azimuth-service"),
    None => assert_true(false) // Should have service.name
  }
  
  let service_version = Configuration::get_string(config, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "1.0.0"),
    None => assert_true(false) // Should have service.version
  }
  
  // Verify integer values
  let server_port = Configuration::get_int(config, "server.port")
  match server_port {
    Some(port) => assert_eq(port, 8080),
    None => assert_true(false) // Should have server.port
  }
  
  let max_connections = Configuration::get_int(config, "server.max_connections")
  match max_connections {
    Some(connections) => assert_eq(connections, 1000),
    None => assert_true(false) // Should have server.max_connections
  }
  
  // Verify boolean values
  let debug_enabled = Configuration::get_bool(config, "debug.enabled")
  match debug_enabled {
    Some(enabled) => assert_false(enabled),
    None => assert_true(false) // Should have debug.enabled
  }
  
  let metrics_enabled = Configuration::get_bool(config, "metrics.enabled")
  match metrics_enabled {
    Some(enabled) => assert_true(enabled),
    None => assert_true(false) // Should have metrics.enabled
  }
  
  // Verify float values
  let sampling_ratio = Configuration::get_float(config, "sampling.ratio")
  match sampling_ratio {
    Some(ratio) => assert_eq(ratio, 0.5),
    None => assert_true(false) // Should have sampling.ratio
  }
  
  let timeout_seconds = Configuration::get_float(config, "timeout.seconds")
  match timeout_seconds {
    Some(timeout) => assert_eq(timeout, 30.0),
    None => assert_true(false) // Should have timeout.seconds
  }
  
  // Test default values
  let non_existent_string = Configuration::get_string(config, "non.existent.key")
  assert_eq(non_existent_string, None)
  
  let default_string = Configuration::get_string_with_default(config, "non.existent.key", "default-value")
  assert_eq(default_string, "default-value")
  
  let non_existent_int = Configuration::get_int(config, "non.existent.int.key")
  assert_eq(non_existent_int, None)
  
  let default_int = Configuration::get_int_with_default(config, "non.existent.int.key", 42)
  assert_eq(default_int, 42)
  
  // Test configuration keys existence
  assert_true(Configuration::has_key(config, "service.name"))
  assert_true(Configuration::has_key(config, "server.port"))
  assert_true(Configuration::has_key(config, "debug.enabled"))
  assert_false(Configuration::has_key(config, "non.existent.key"))
  
  // Test all keys retrieval
  let all_keys = Configuration::keys(config)
  assert_true(all_keys.length() >= 7) // At least the keys we set
  
  assert_true(all_keys.contains("service.name"))
  assert_true(all_keys.contains("service.version"))
  assert_true(all_keys.contains("server.port"))
  assert_true(all_keys.contains("debug.enabled"))
  assert_true(all_keys.contains("metrics.enabled"))
  assert_true(all_keys.contains("sampling.ratio"))
  assert_true(all_keys.contains("timeout.seconds"))
}

// Test 2: Configuration File Loading and Parsing
test "configuration file loading and parsing" {
  // Test JSON configuration loading
  let json_config_content = "{
    \"service\": {
      \"name\": \"azimuth-service\",
      \"version\": \"1.0.0\",
      \"namespace\": \"production\"
    },
    \"server\": {
      \"port\": 8080,
      \"max_connections\": 1000,
      \"host\": \"0.0.0.0\"
    },
    \"debug\": {
      \"enabled\": false,
      \"level\": \"info\"
    },
    \"metrics\": {
      \"enabled\": true,
      \"interval\": 60
    },
    \"sampling\": {
      \"ratio\": 0.5,
      \"strategy\": \"probabilistic\"
    },
    \"timeout\": {
      \"seconds\": 30.0
    }
  }"
  
  // Create temporary JSON config file
  let json_config_file = TempFile::create_with_content(json_config_content)
  
  // Load configuration from JSON file
  let json_config = Configuration::load_from_json_file(json_config_file.path())
  
  // Verify loaded configuration
  let service_name = Configuration::get_string(json_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "azimuth-service"),
    None => assert_true(false) // Should have service.name
  }
  
  let server_port = Configuration::get_int(json_config, "server.port")
  match server_port {
    Some(port) => assert_eq(port, 8080),
    None => assert_true(false) // Should have server.port
  }
  
  let debug_enabled = Configuration::get_bool(json_config, "debug.enabled")
  match debug_enabled {
    Some(enabled) => assert_false(enabled),
    None => assert_true(false) // Should have debug.enabled
  }
  
  let sampling_ratio = Configuration::get_float(json_config, "sampling.ratio")
  match sampling_ratio {
    Some(ratio) => assert_eq(ratio, 0.5),
    None => assert_true(false) // Should have sampling.ratio
  }
  
  // Test YAML configuration loading
  let yaml_config_content = "service:
  name: azimuth-yaml-service
  version: 2.0.0
  namespace: staging
server:
  port: 9090
  max_connections: 2000
  host: 127.0.0.1
debug:
  enabled: true
  level: debug
metrics:
  enabled: true
  interval: 30
sampling:
  ratio: 0.8
  strategy: deterministic
timeout:
  seconds: 60.0"
  
  // Create temporary YAML config file
  let yaml_config_file = TempFile::create_with_content(yaml_config_content)
  
  // Load configuration from YAML file
  let yaml_config = Configuration::load_from_yaml_file(yaml_config_file.path())
  
  // Verify loaded configuration
  let yaml_service_name = Configuration::get_string(yaml_config, "service.name")
  match yaml_service_name {
    Some(name) => assert_eq(name, "azimuth-yaml-service"),
    None => assert_true(false) // Should have service.name
  }
  
  let yaml_server_port = Configuration::get_int(yaml_config, "server.port")
  match yaml_server_port {
    Some(port) => assert_eq(port, 9090),
    None => assert_true(false) // Should have server.port
  }
  
  let yaml_debug_enabled = Configuration::get_bool(yaml_config, "debug.enabled")
  match yaml_debug_enabled {
    Some(enabled) => assert_true(enabled),
    None => assert_true(false) // Should have debug.enabled
  }
  
  // Test TOML configuration loading
  let toml_config_content = "[service]
name = \"azimuth-toml-service\"
version = \"3.0.0\"
namespace = \"development\"

[server]
port = 7070
max_connections = 500
host = \"localhost\"

[debug]
enabled = true
level = \"trace\"

[metrics]
enabled = false
interval = 120

[sampling]
ratio = 0.2
strategy = \"constant\"

[timeout]
seconds = 15.0"
  
  // Create temporary TOML config file
  let toml_config_file = TempFile::create_with_content(toml_config_content)
  
  // Load configuration from TOML file
  let toml_config = Configuration::load_from_toml_file(toml_config_file.path())
  
  // Verify loaded configuration
  let toml_service_name = Configuration::get_string(toml_config, "service.name")
  match toml_service_name {
    Some(name) => assert_eq(name, "azimuth-toml-service"),
    None => assert_true(false) // Should have service.name
  }
  
  let toml_server_port = Configuration::get_int(toml_config, "server.port")
  match toml_server_port {
    Some(port) => assert_eq(port, 7070),
    None => assert_true(false) // Should have server.port
  }
  
  let toml_metrics_enabled = Configuration::get_bool(toml_config, "metrics.enabled")
  match toml_metrics_enabled {
    Some(enabled) => assert_false(enabled),
    None => assert_true(false) // Should have metrics.enabled
  }
  
  // Test configuration loading with errors
  let invalid_json_content = "{ invalid json }"
  let invalid_json_file = TempFile::create_with_content(invalid_json_content)
  
  let invalid_json_result = Configuration::load_from_json_file(invalid_json_file.path())
  match invalid_json_result {
    Ok(_) => assert_true(false), // Should fail
    Err(_) => assert_true(true)  // Expected to fail
  }
  
  let non_existent_file = "/path/to/non/existent/config.json"
  let non_existent_result = Configuration::load_from_json_file(non_existent_file)
  match non_existent_result {
    Ok(_) => assert_true(false), // Should fail
    Err(_) => assert_true(true)  // Expected to fail
  }
}

// Test 3: Configuration Hierarchy and Inheritance
test "configuration hierarchy and inheritance" {
  // Create base configuration
  let base_config = Configuration::new()
  
  Configuration::set_string(base_config, "service.name", "azimuth-service")
  Configuration::set_string(base_config, "service.version", "1.0.0")
  Configuration::set_string(base_config, "service.namespace", "production")
  
  Configuration::set_int(base_config, "server.port", 8080)
  Configuration::set_int(base_config, "server.max_connections", 1000)
  
  Configuration::set_bool(base_config, "debug.enabled", false)
  Configuration::set_bool(base_config, "metrics.enabled", true)
  
  Configuration::set_float(base_config, "sampling.ratio", 0.5)
  
  // Create environment-specific configuration
  let staging_config = Configuration::new()
  
  Configuration::set_string(staging_config, "service.namespace", "staging")
  Configuration::set_int(staging_config, "server.port", 8081)
  Configuration::set_bool(staging_config, "debug.enabled", true)
  Configuration::set_float(staging_config, "sampling.ratio", 0.8)
  
  // Merge configurations
  let merged_config = Configuration::merge(base_config, staging_config)
  
  // Verify merged values
  let service_name = Configuration::get_string(merged_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "azimuth-service"), // From base
    None => assert_true(false)
  }
  
  let service_namespace = Configuration::get_string(merged_config, "service.namespace")
  match service_namespace {
    Some(namespace) => assert_eq(namespace, "staging"), // From staging
    None => assert_true(false)
  }
  
  let server_port = Configuration::get_int(merged_config, "server.port")
  match server_port {
    Some(port) => assert_eq(port, 8081), // From staging
    None => assert_true(false)
  }
  
  let max_connections = Configuration::get_int(merged_config, "server.max_connections")
  match max_connections {
    Some(connections) => assert_eq(connections, 1000), // From base
    None => assert_true(false)
  }
  
  let debug_enabled = Configuration::get_bool(merged_config, "debug.enabled")
  match debug_enabled {
    Some(enabled) => assert_true(enabled), // From staging
    None => assert_true(false)
  }
  
  let metrics_enabled = Configuration::get_bool(merged_config, "metrics.enabled")
  match metrics_enabled {
    Some(enabled) => assert_true(enabled), // From base
    None => assert_true(false)
  }
  
  // Test multiple levels of inheritance
  let local_config = Configuration::new()
  Configuration::set_string(local_config, "service.version", "1.1.0-dev")
  Configuration::set_int(local_config, "server.max_connections", 500)
  
  let final_config = Configuration::merge(merged_config, local_config)
  
  let final_service_version = Configuration::get_string(final_config, "service.version")
  match final_service_version {
    Some(version) => assert_eq(version, "1.1.0-dev"), // From local
    None => assert_true(false)
  }
  
  let final_max_connections = Configuration::get_int(final_config, "server.max_connections")
  match final_max_connections {
    Some(connections) => assert_eq(connections, 500), // From local
    None => assert_true(false)
  }
  
  // Test configuration precedence with multiple sources
  let config_sources = [
    ("global", base_config),
    ("environment", staging_config),
    ("local", local_config)
  ]
  
  let hierarchical_config = Configuration::merge_multiple(config_sources)
  
  // Verify precedence: local > environment > global
  let hierarchical_service_name = Configuration::get_string(hierarchical_config, "service.name")
  match hierarchical_service_name {
    Some(name) => assert_eq(name, "azimuth-service"), // From global
    None => assert_true(false)
  }
  
  let hierarchical_service_namespace = Configuration::get_string(hierarchical_config, "service.namespace")
  match hierarchical_service_namespace {
    Some(namespace) => assert_eq(namespace, "staging"), // From environment
    None => assert_true(false)
  }
  
  let hierarchical_service_version = Configuration::get_string(hierarchical_config, "service.version")
  match hierarchical_service_version {
    Some(version) => assert_eq(version, "1.1.0-dev"), // From local
    None => assert_true(false)
  }
  
  // Test configuration with nested precedence
  let nested_base = Configuration::new()
  Configuration::set_string(nested_base, "database.host", "db.example.com")
  Configuration::set_string(nested_base, "database.name", "azimuth")
  Configuration::set_int(nested_base, "database.port", 5432)
  
  let nested_override = Configuration::new()
  Configuration::set_string(nested_override, "database.host", "localhost") // Override host
  // Don't override name or port
  
  let nested_merged = Configuration::merge(nested_base, nested_override)
  
  let nested_host = Configuration::get_string(nested_merged, "database.host")
  match nested_host {
    Some(host) => assert_eq(host, "localhost"), // Override
    None => assert_true(false)
  }
  
  let nested_name = Configuration::get_string(nested_merged, "database.name")
  match nested_name {
    Some(name) => assert_eq(name, "azimuth"), // From base
    None => assert_true(false)
  }
  
  let nested_port = Configuration::get_int(nested_merged, "database.port")
  match nested_port {
    Some(port) => assert_eq(port, 5432), // From base
    None => assert_true(false)
  }
}

// Test 4: Environment Variable Configuration
test "environment variable configuration" {
  // Set environment variables
  Environment::set_var("AZIMUTH_SERVICE_NAME", "env-service")
  Environment::set_var("AZIMUTH_SERVICE_VERSION", "2.0.0")
  Environment::set_var("AZIMUTH_SERVER_PORT", "9090")
  Environment::set_var("AZIMUTH_DEBUG_ENABLED", "true")
  Environment::set_var("AZIMUTH_SAMPLING_RATIO", "0.75")
  
  // Load configuration from environment variables
  let env_config = Configuration::load_from_environment("AZIMUTH")
  
  // Verify loaded configuration
  let service_name = Configuration::get_string(env_config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "env-service"),
    None => assert_true(false) // Should have service.name
  }
  
  let service_version = Configuration::get_string(env_config, "service.version")
  match service_version {
    Some(version) => assert_eq(version, "2.0.0"),
    None => assert_true(false) // Should have service.version
  }
  
  let server_port = Configuration::get_int(env_config, "server.port")
  match server_port {
    Some(port) => assert_eq(port, 9090),
    None => assert_true(false) // Should have server.port
  }
  
  let debug_enabled = Configuration::get_bool(env_config, "debug.enabled")
  match debug_enabled {
    Some(enabled) => assert_true(enabled),
    None => assert_true(false) // Should have debug.enabled
  }
  
  let sampling_ratio = Configuration::get_float(env_config, "sampling.ratio")
  match sampling_ratio {
    Some(ratio) => assert_eq(ratio, 0.75),
    None => assert_true(false) // Should have sampling.ratio
  }
  
  // Test configuration with mixed sources (file + environment)
  let base_config = Configuration::new()
  Configuration::set_string(base_config, "service.name", "file-service")
  Configuration::set_string(base_config, "service.version", "1.0.0")
  Configuration::set_int(base_config, "server.port", 8080)
  Configuration::set_bool(base_config, "debug.enabled", false)
  Configuration::set_float(base_config, "sampling.ratio", 0.5)
  
  // Environment should override file
  let mixed_config = Configuration::merge_with_environment(base_config, "AZIMUTH")
  
  let mixed_service_name = Configuration::get_string(mixed_config, "service.name")
  match mixed_service_name {
    Some(name) => assert_eq(name, "env-service"), // From environment
    None => assert_true(false)
  }
  
  let mixed_service_version = Configuration::get_string(mixed_config, "service.version")
  match mixed_service_version {
    Some(version) => assert_eq(version, "2.0.0"), // From environment
    None => assert_true(false)
  }
  
  let mixed_server_port = Configuration::get_int(mixed_config, "server.port")
  match mixed_server_port {
    Some(port) => assert_eq(port, 9090), // From environment
    None => assert_true(false)
  }
  
  let mixed_debug_enabled = Configuration::get_bool(mixed_config, "debug.enabled")
  match mixed_debug_enabled {
    Some(enabled) => assert_true(enabled), // From environment
    None => assert_true(false)
  }
  
  let mixed_sampling_ratio = Configuration::get_float(mixed_config, "sampling.ratio")
  match mixed_sampling_ratio {
    Some(ratio) => assert_eq(ratio, 0.75), // From environment
    None => assert_true(false)
  }
  
  // Clean up environment variables
  Environment::remove_var("AZIMUTH_SERVICE_NAME")
  Environment::remove_var("AZIMUTH_SERVICE_VERSION")
  Environment::remove_var("AZIMUTH_SERVER_PORT")
  Environment::remove_var("AZIMUTH_DEBUG_ENABLED")
  Environment::remove_var("AZIMUTH_SAMPLING_RATIO")
}

// Test 5: Configuration Validation and Constraints
test "configuration validation and constraints" {
  // Test configuration with validation rules
  let config_with_validation = Configuration::new_with_validation()
  
  // Add validation rules
  Configuration::add_validation_rule(config_with_validation, "server.port", ValidationRule::int_range(1, 65535))
  Configuration::add_validation_rule(config_with_validation, "server.max_connections", ValidationRule::int_min(1))
  Configuration::add_validation_rule(config_with_validation, "sampling.ratio", ValidationRule::float_range(0.0, 1.0))
  Configuration::add_validation_rule(config_with_validation, "service.name", ValidationRule::non_empty_string())
  Configuration::add_validation_rule(config_with_validation, "timeout.seconds", ValidationRule::float_min(0.1))
  
  // Set valid values
  Configuration::set_string(config_with_validation, "service.name", "valid-service")
  Configuration::set_int(config_with_validation, "server.port", 8080)
  Configuration::set_int(config_with_validation, "server.max_connections", 1000)
  Configuration::set_float(config_with_validation, "sampling.ratio", 0.5)
  Configuration::set_float(config_with_validation, "timeout.seconds", 30.0)
  
  // Validate configuration
  let validation_result = Configuration::validate(config_with_validation)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test invalid values
  let invalid_config = Configuration::new_with_validation()
  
  Configuration::add_validation_rule(invalid_config, "server.port", ValidationRule::int_range(1, 65535))
  Configuration::add_validation_rule(invalid_config, "server.max_connections", ValidationRule::int_min(1))
  Configuration::add_validation_rule(invalid_config, "sampling.ratio", ValidationRule::float_range(0.0, 1.0))
  Configuration::add_validation_rule(invalid_config, "service.name", ValidationRule::non_empty_string())
  
  // Set invalid values
  Configuration::set_string(invalid_config, "service.name", "") // Empty string
  Configuration::set_int(invalid_config, "server.port", 70000) // Out of range
  Configuration::set_int(invalid_config, "server.max_connections", 0) // Less than minimum
  Configuration::set_float(invalid_config, "sampling.ratio", 1.5) // Out of range
  
  // Validate invalid configuration
  let invalid_validation_result = Configuration::validate(invalid_config)
  assert_false(invalid_validation_result.is_valid)
  assert_eq(invalid_validation_result.errors.length(), 4)
  
  // Check error messages
  assert_true(invalid_validation_result.errors.any(|error| error.contains("service.name")))
  assert_true(invalid_validation_result.errors.any(|error| error.contains("server.port")))
  assert_true(invalid_validation_result.errors.any(|error| error.contains("server.max_connections")))
  assert_true(invalid_validation_result.errors.any(|error| error.contains("sampling.ratio")))
  
  // Test required fields
  let required_config = Configuration::new_with_validation()
  
  Configuration::add_required_field(required_config, "service.name")
  Configuration::add_required_field(required_config, "server.port")
  Configuration::add_required_field(required_config, "database.host")
  
  // Set only some required fields
  Configuration::set_string(required_config, "service.name", "required-service")
  Configuration::set_int(required_config, "server.port", 8080)
  // Don't set database.host
  
  // Validate required fields
  let required_validation_result = Configuration::validate(required_config)
  assert_false(required_validation_result.is_valid)
  assert_eq(required_validation_result.errors.length(), 1)
  assert_true(required_validation_result.errors.any(|error| error.contains("database.host")))
  
  // Test custom validation functions
  let custom_config = Configuration::new_with_validation()
  
  // Add custom validation for URLs
  Configuration::add_custom_validation(
    custom_config,
    "api.url",
    |value| {
      match value {
        String(url) => url.starts_with("http://") || url.starts_with("https://"),
        _ => false
      }
    },
    "URL must start with http:// or https://"
  )
  
  // Add custom validation for email addresses
  Configuration::add_custom_validation(
    custom_config,
    "admin.email",
    |value| {
      match value {
        String(email) => email.contains("@") && email.contains("."),
        _ => false
      }
    },
    "Email must contain @ and ."
  )
  
  // Set valid values
  Configuration::set_string(custom_config, "api.url", "https://api.example.com")
  Configuration::set_string(custom_config, "admin.email", "admin@example.com")
  
  // Validate custom configuration
  let custom_validation_result = Configuration::validate(custom_config)
  assert_true(custom_validation_result.is_valid)
  assert_eq(custom_validation_result.errors.length(), 0)
  
  // Set invalid values
  Configuration::set_string(custom_config, "api.url", "ftp://api.example.com") // Invalid protocol
  Configuration::set_string(custom_config, "admin.email", "invalid-email") // Invalid email
  
  // Validate invalid custom configuration
  let invalid_custom_validation_result = Configuration::validate(custom_config)
  assert_false(invalid_custom_validation_result.is_valid)
  assert_eq(invalid_custom_validation_result.errors.length(), 2)
  assert_true(invalid_custom_validation_result.errors.any(|error| error.contains("URL must start with")))
  assert_true(invalid_custom_validation_result.errors.any(|error| error.contains("Email must contain")))
}

// Test 6: Configuration Hot Reloading and Watching
test "configuration hot reloading and watching" {
  // Create initial configuration file
  let initial_config_content = "{
    \"service\": {
      \"name\": \"initial-service\",
      \"version\": \"1.0.0\"
    },
    \"server\": {
      \"port\": 8080,
      \"max_connections\": 1000
    },
    \"debug\": {
      \"enabled\": false
    },
    \"sampling\": {
      \"ratio\": 0.5
    }
  }"
  
  let config_file = TempFile::create_with_content(initial_config_content)
  
  // Load initial configuration
  let config = Configuration::load_from_json_file(config_file.path())
  
  let initial_service_name = Configuration::get_string(config, "service.name")
  match initial_service_name {
    Some(name) => assert_eq(name, "initial-service"),
    None => assert_true(false)
  }
  
  // Set up configuration watcher
  let watcher = ConfigurationWatcher::new(config)
  let mut reload_count = 0
  
  ConfigurationWatcher::on_change(watcher, || {
    reload_count = reload_count + 1
  })
  
  // Start watching
  ConfigurationWatcher::start(watcher, config_file.path())
  
  // Update configuration file
  let updated_config_content = "{
    \"service\": {
      \"name\": \"updated-service\",
      \"version\": \"1.1.0\"
    },
    \"server\": {
      \"port\": 9090,
      \"max_connections\": 2000
    },
    \"debug\": {
      \"enabled\": true
    },
    \"sampling\": {
      \"ratio\": 0.8
    }
  }"
  
  TempFile::update_content(config_file, updated_config_content)
  
  // Wait for file change to be detected
  Thread::sleep(100)
  
  // Verify configuration was reloaded
  let updated_service_name = Configuration::get_string(config, "service.name")
  match updated_service_name {
    Some(name) => assert_eq(name, "updated-service"),
    None => assert_true(false)
  }
  
  let updated_server_port = Configuration::get_int(config, "server.port")
  match updated_server_port {
    Some(port) => assert_eq(port, 9090),
    None => assert_true(false)
  }
  
  let updated_debug_enabled = Configuration::get_bool(config, "debug.enabled")
  match updated_debug_enabled {
    Some(enabled) => assert_true(enabled),
    None => assert_true(false)
  }
  
  // Verify change callback was called
  assert_true(reload_count > 0)
  
  // Stop watching
  ConfigurationWatcher::stop(watcher)
  
  // Test configuration with multiple files
  let base_file = TempFile::create_with_content("{
    \"service\": {
      \"name\": \"base-service\",
      \"version\": \"1.0.0\"
    },
    \"server\": {
      \"port\": 8080
    }
  }")
  
  let override_file = TempFile::create_with_content("{
    \"service\": {
      \"version\": \"1.1.0\"
    },
    \"debug\": {
      \"enabled\": true
    }
  }")
  
  // Load configuration from multiple files
  let multi_config = Configuration::load_from_multiple_json_files([
    base_file.path(),
    override_file.path()
  ])
  
  let multi_service_name = Configuration::get_string(multi_config, "service.name")
  match multi_service_name {
    Some(name) => assert_eq(name, "base-service"), // From base
    None => assert_true(false)
  }
  
  let multi_service_version = Configuration::get_string(multi_config, "service.version")
  match multi_service_version {
    Some(version) => assert_eq(version, "1.1.0"), // From override
    None => assert_true(false)
  }
  
  let multi_server_port = Configuration::get_int(multi_config, "server.port")
  match multi_server_port {
    Some(port) => assert_eq(port, 8080), // From base
    None => assert_true(false)
  }
  
  let multi_debug_enabled = Configuration::get_bool(multi_config, "debug.enabled")
  match multi_debug_enabled {
    Some(enabled) => assert_true(enabled), // From override
    None => assert_true(false)
  }
  
  // Test configuration with hot reloading and validation
  let validated_config_file = TempFile::create_with_content("{
    \"server\": {
      \"port\": 8080
    },
    \"sampling\": {
      \"ratio\": 0.5
    }
  }")
  
  let validated_config = Configuration::load_from_json_file_with_validation(
    validated_config_file.path(),
    [
      ("server.port", ValidationRule::int_range(1, 65535)),
      ("sampling.ratio", ValidationRule::float_range(0.0, 1.0))
    ]
  )
  
  // Update with valid configuration
  let valid_update_content = "{
    \"server\": {
      \"port\": 9090
    },
    \"sampling\": {
      \"ratio\": 0.8
    }
  }"
  
  TempFile::update_content(validated_config_file, valid_update_content)
  Thread::sleep(100)
  
  let valid_server_port = Configuration::get_int(validated_config, "server.port")
  match valid_server_port {
    Some(port) => assert_eq(port, 9090),
    None => assert_true(false)
  }
  
  // Try to update with invalid configuration
  let invalid_update_content = "{
    \"server\": {
      \"port\": 70000
    },
    \"sampling\": {
      \"ratio\": 1.5
    }
  }"
  
  TempFile::update_content(validated_config_file, invalid_update_content)
  Thread::sleep(100)
  
  // Configuration should not be updated due to validation failure
  let invalid_server_port = Configuration::get_int(validated_config, "server.port")
  match invalid_server_port {
    Some(port) => assert_eq(port, 9090), // Should still be 9090
    None => assert_true(false)
  }
}

// Test 7: Configuration Encryption and Security
test "configuration encryption and security" {
  // Create configuration with sensitive data
  let secure_config = Configuration::new()
  
  Configuration::set_string(secure_config, "database.password", "secret123")
  Configuration::set_string(secure_config, "api.key", "api-key-456")
  Configuration::set_string(secure_config, "jwt.secret", "jwt-secret-789")
  Configuration::set_string(secure_config, "service.name", "public-service")
  Configuration::set_int(secure_config, "server.port", 8080)
  
  // Mark sensitive fields
  Configuration::mark_sensitive(secure_config, "database.password")
  Configuration::mark_sensitive(secure_config, "api.key")
  Configuration::mark_sensitive(secure_config, "jwt.secret")
  
  // Test sensitive field masking
  let masked_password = Configuration::get_string_masked(secure_config, "database.password")
  match masked_password {
    Some(masked) => assert_eq(masked, "*****"),
    None => assert_true(false)
  }
  
  let masked_api_key = Configuration::get_string_masked(secure_config, "api.key")
  match masked_api_key {
    Some(masked) => assert_eq(masked, "*****"),
    None => assert_true(false)
  }
  
  let public_service_name = Configuration::get_string_masked(secure_config, "service.name")
  match public_service_name {
    Some(name) => assert_eq(name, "public-service"), // Not masked
    None => assert_true(false)
  }
  
  // Test configuration encryption
  let encryption_key = "encryption-key-12345"
  let encrypted_config = Configuration::encrypt(secure_config, encryption_key)
  
  // Verify encrypted data is not readable
  let encrypted_password = Configuration::get_string(encrypted_config, "database.password")
  match encrypted_password {
    Some(value) => assert_not_eq(value, "secret123"),
    None => assert_true(false)
  }
  
  // Test configuration decryption
  let decrypted_config = Configuration::decrypt(encrypted_config, encryption_key)
  
  let decrypted_password = Configuration::get_string(decrypted_config, "database.password")
  match decrypted_password {
    Some(password) => assert_eq(password, "secret123"),
    None => assert_true(false)
  }
  
  let decrypted_api_key = Configuration::get_string(decrypted_config, "api.key")
  match decrypted_api_key {
    Some(key) => assert_eq(key, "api-key-456"),
    None => assert_true(false)
  }
  
  // Test configuration with key rotation
  let old_key = "old-encryption-key"
  let new_key = "new-encryption-key"
  
  let encrypted_with_old_key = Configuration::encrypt(secure_config, old_key)
  
  // Decrypt with old key
  let decrypted_with_old_key = Configuration::decrypt(encrypted_with_old_key, old_key)
  let old_key_password = Configuration::get_string(decrypted_with_old_key, "database.password")
  match old_key_password {
    Some(password) => assert_eq(password, "secret123"),
    None => assert_true(false)
  }
  
  // Rotate key
  let reencrypted_config = Configuration::rotate_key(encrypted_with_old_key, old_key, new_key)
  
  // Decrypt with new key
  let decrypted_with_new_key = Configuration::decrypt(reencrypted_config, new_key)
  let new_key_password = Configuration::get_string(decrypted_with_new_key, "database.password")
  match new_key_password {
    Some(password) => assert_eq(password, "secret123"),
    None => assert_true(false)
  }
  
  // Test configuration access control
  let access_controlled_config = Configuration::new_with_access_control()
  
  Configuration::set_string(access_controlled_config, "admin.password", "admin-secret")
  Configuration::set_string(access_controlled_config, "user.password", "user-secret")
  Configuration::set_string(access_controlled_config, "service.name", "public-service")
  
  // Set access permissions
  Configuration::set_access_level(access_controlled_config, "admin.password", "admin")
  Configuration::set_access_level(access_controlled_config, "user.password", "user")
  Configuration::set_access_level(access_controlled_config, "service.name", "public")
  
  // Test access with admin role
  let admin_access = Configuration::with_role(access_controlled_config, "admin")
  
  let admin_password = Configuration::get_string(admin_access, "admin.password")
  match admin_password {
    Some(password) => assert_eq(password, "admin-secret"),
    None => assert_true(false)
  }
  
  let user_password = Configuration::get_string(admin_access, "user.password")
  match user_password {
    Some(password) => assert_eq(password, "user-secret"),
    None => assert_true(false)
  }
  
  let service_name = Configuration::get_string(admin_access, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "public-service"),
    None => assert_true(false)
  }
  
  // Test access with user role
  let user_access = Configuration::with_role(access_controlled_config, "user")
  
  let admin_password_for_user = Configuration::get_string(user_access, "admin.password")
  assert_eq(admin_password_for_user, None) // Should not have access
  
  let user_password_for_user = Configuration::get_string(user_access, "user.password")
  match user_password_for_user {
    Some(password) => assert_eq(password, "user-secret"),
    None => assert_true(false)
  }
  
  let service_name_for_user = Configuration::get_string(user_access, "service.name")
  match service_name_for_user {
    Some(name) => assert_eq(name, "public-service"),
    None => assert_true(false)
  }
  
  // Test access with public role
  let public_access = Configuration::with_role(access_controlled_config, "public")
  
  let admin_password_for_public = Configuration::get_string(public_access, "admin.password")
  assert_eq(admin_password_for_public, None) // Should not have access
  
  let user_password_for_public = Configuration::get_string(public_access, "user.password")
  assert_eq(user_password_for_public, None) // Should not have access
  
  let service_name_for_public = Configuration::get_string(public_access, "service.name")
  match service_name_for_public {
    Some(name) => assert_eq(name, "public-service"),
    None => assert_true(false)
  }
}

// Test 8: Configuration Templates and Profiles
test "configuration templates and profiles" {
  // Create configuration template
  let template = ConfigurationTemplate::new("service-template")
  
  ConfigurationTemplate::add_field(template, "service.name", "azimuth-service", StringType)
  ConfigurationTemplate::add_field(template, "service.version", "1.0.0", StringType)
  ConfigurationTemplate::add_field(template, "server.port", 8080, IntType)
  ConfigurationTemplate::add_field(template, "server.max_connections", 1000, IntType)
  ConfigurationTemplate::add_field(template, "debug.enabled", false, BoolType)
  ConfigurationTemplate::add_field(template, "sampling.ratio", 0.5, FloatType)
  
  // Create configuration from template
  let config_from_template = Configuration::from_template(template)
  
  // Verify default values
  let template_service_name = Configuration::get_string(config_from_template, "service.name")
  match template_service_name {
    Some(name) => assert_eq(name, "azimuth-service"),
    None => assert_true(false)
  }
  
  let template_server_port = Configuration::get_int(config_from_template, "server.port")
  match template_server_port {
    Some(port) => assert_eq(port, 8080),
    None => assert_true(false)
  }
  
  let template_debug_enabled = Configuration::get_bool(config_from_template, "debug.enabled")
  match template_debug_enabled {
    Some(enabled) => assert_false(enabled),
    None => assert_true(false)
  }
  
  // Override template values
  Configuration::set_string(config_from_template, "service.name", "custom-service")
  Configuration::set_int(config_from_template, "server.port", 9090)
  Configuration::set_bool(config_from_template, "debug.enabled", true)
  
  // Verify overridden values
  let custom_service_name = Configuration::get_string(config_from_template, "service.name")
  match custom_service_name {
    Some(name) => assert_eq(name, "custom-service"),
    None => assert_true(false)
  }
  
  let custom_server_port = Configuration::get_int(config_from_template, "server.port")
  match custom_server_port {
    Some(port) => assert_eq(port, 9090),
    None => assert_true(false)
  }
  
  let custom_debug_enabled = Configuration::get_bool(config_from_template, "debug.enabled")
  match custom_debug_enabled {
    Some(enabled) => assert_true(enabled),
    None => assert_true(false)
  }
  
  // Test configuration profiles
  let development_profile = ConfigurationProfile::new("development")
  ConfigurationProfile::set_value(development_profile, "debug.enabled", true)
  ConfigurationProfile::set_value(development_profile, "server.port", 3000)
  ConfigurationProfile::set_value(development_profile, "sampling.ratio", 1.0)
  
  let production_profile = ConfigurationProfile::new("production")
  ConfigurationProfile::set_value(production_profile, "debug.enabled", false)
  ConfigurationProfile::set_value(production_profile, "server.port", 8080)
  ConfigurationProfile::set_value(production_profile, "sampling.ratio", 0.1)
  
  let staging_profile = ConfigurationProfile::new("staging")
  ConfigurationProfile::set_value(staging_profile, "debug.enabled", true)
  ConfigurationProfile::set_value(staging_profile, "server.port", 8081)
  ConfigurationProfile::set_value(staging_profile, "sampling.ratio", 0.5)
  
  // Create base configuration
  let base_profile_config = Configuration::new()
  Configuration::set_string(base_profile_config, "service.name", "azimuth-service")
  Configuration::set_string(base_profile_config, "service.version", "1.0.0")
  Configuration::set_int(base_profile_config, "server.max_connections", 1000)
  
  // Apply development profile
  let dev_config = Configuration::apply_profile(base_profile_config, development_profile)
  
  let dev_debug_enabled = Configuration::get_bool(dev_config, "debug.enabled")
  match dev_debug_enabled {
    Some(enabled) => assert_true(enabled), // From profile
    None => assert_true(false)
  }
  
  let dev_server_port = Configuration::get_int(dev_config, "server.port")
  match dev_server_port {
    Some(port) => assert_eq(port, 3000), // From profile
    None => assert_true(false)
  }
  
  let dev_service_name = Configuration::get_string(dev_config, "service.name")
  match dev_service_name {
    Some(name) => assert_eq(name, "azimuth-service"), // From base
    None => assert_true(false)
  }
  
  // Apply production profile
  let prod_config = Configuration::apply_profile(base_profile_config, production_profile)
  
  let prod_debug_enabled = Configuration::get_bool(prod_config, "debug.enabled")
  match prod_debug_enabled {
    Some(enabled) => assert_false(enabled), // From profile
    None => assert_true(false)
  }
  
  let prod_server_port = Configuration::get_int(prod_config, "server.port")
  match prod_server_port {
    Some(port) => assert_eq(port, 8080), // From profile
    None => assert_true(false)
  }
  
  let prod_service_name = Configuration::get_string(prod_config, "service.name")
  match prod_service_name {
    Some(name) => assert_eq(name, "azimuth-service"), // From base
    None => assert_true(false)
  }
  
  // Test profile inheritance
  let base_profile = ConfigurationProfile::new("base")
  ConfigurationProfile::set_value(base_profile, "debug.enabled", false)
  ConfigurationProfile::set_value(base_profile, "server.port", 8080)
  
  let derived_profile = ConfigurationProfile::new("derived")
  ConfigurationProfile::inherit_from(derived_profile, base_profile)
  ConfigurationProfile::set_value(derived_profile, "debug.enabled", true) // Override
  
  let inherited_config = Configuration::apply_profile(base_profile_config, derived_profile)
  
  let inherited_debug_enabled = Configuration::get_bool(inherited_config, "debug.enabled")
  match inherited_debug_enabled {
    Some(enabled) => assert_true(enabled), // From derived
    None => assert_true(false)
  }
  
  let inherited_server_port = Configuration::get_int(inherited_config, "server.port")
  match inherited_server_port {
    Some(port) => assert_eq(port, 8080), // From base
    None => assert_true(false)
  }
  
  // Test profile with validation
  let validated_profile = ConfigurationProfile::new("validated")
  ConfigurationProfile::set_value(validated_profile, "server.port", 8080)
  ConfigurationProfile::add_validation_rule(validated_profile, "server.port", ValidationRule::int_range(1, 65535))
  
  let validated_config = Configuration::apply_profile(base_profile_config, validated_profile)
  
  let validated_server_port = Configuration::get_int(validated_config, "server.port")
  match validated_server_port {
    Some(port) => assert_eq(port, 8080), // From profile
    None => assert_true(false)
  }
  
  // Try to set invalid value in profile
  ConfigurationProfile::set_value(validated_profile, "server.port", 70000)
  
  let invalid_profile_result = Configuration::apply_profile_with_validation(base_profile_config, validated_profile)
  match invalid_profile_result {
    Ok(_) => assert_true(false), // Should fail
    Err(errors) => assert_true(errors.length() > 0) // Expected to fail
  }
}

// Test 9: Configuration Export and Import
test "configuration export and import" {
  // Create configuration with various data types
  let config = Configuration::new()
  
  Configuration::set_string(config, "service.name", "azimuth-service")
  Configuration::set_string(config, "service.version", "1.0.0")
  Configuration::set_string(config, "service.namespace", "production")
  
  Configuration::set_int(config, "server.port", 8080)
  Configuration::set_int(config, "server.max_connections", 1000)
  
  Configuration::set_bool(config, "debug.enabled", false)
  Configuration::set_bool(config, "metrics.enabled", true)
  
  Configuration::set_float(config, "sampling.ratio", 0.5)
  Configuration::set_float(config, "timeout.seconds", 30.0)
  
  // Export to JSON
  let json_export = Configuration::export_to_json(config)
  assert_true(json_export.contains("\"service.name\": \"azimuth-service\""))
  assert_true(json_export.contains("\"server.port\": 8080"))
  assert_true(json_export.contains("\"debug.enabled\": false"))
  assert_true(json_export.contains("\"sampling.ratio\": 0.5"))
  
  // Export to YAML
  let yaml_export = Configuration::export_to_yaml(config)
  assert_true(yaml_export.contains("service.name: azimuth-service"))
  assert_true(yaml_export.contains("server.port: 8080"))
  assert_true(yaml_export.contains("debug.enabled: false"))
  assert_true(yaml_export.contains("sampling.ratio: 0.5"))
  
  // Export to TOML
  let toml_export = Configuration::export_to_toml(config)
  assert_true(toml_export.contains("service.name = \"azimuth-service\""))
  assert_true(toml_export.contains("server.port = 8080"))
  assert_true(toml_export.contains("debug.enabled = false"))
  assert_true(toml_export.contains("sampling.ratio = 0.5"))
  
  // Import from JSON
  let json_imported_config = Configuration::import_from_json(json_export)
  
  let imported_service_name = Configuration::get_string(json_imported_config, "service.name")
  match imported_service_name {
    Some(name) => assert_eq(name, "azimuth-service"),
    None => assert_true(false)
  }
  
  let imported_server_port = Configuration::get_int(json_imported_config, "server.port")
  match imported_server_port {
    Some(port) => assert_eq(port, 8080),
    None => assert_true(false)
  }
  
  let imported_debug_enabled = Configuration::get_bool(json_imported_config, "debug.enabled")
  match imported_debug_enabled {
    Some(enabled) => assert_false(enabled),
    None => assert_true(false)
  }
  
  let imported_sampling_ratio = Configuration::get_float(json_imported_config, "sampling.ratio")
  match imported_sampling_ratio {
    Some(ratio) => assert_eq(ratio, 0.5),
    None => assert_true(false)
  }
  
  // Import from YAML
  let yaml_imported_config = Configuration::import_from_yaml(yaml_export)
  
  let yaml_imported_service_name = Configuration::get_string(yaml_imported_config, "service.name")
  match yaml_imported_service_name {
    Some(name) => assert_eq(name, "azimuth-service"),
    None => assert_true(false)
  }
  
  // Import from TOML
  let toml_imported_config = Configuration::import_from_toml(toml_export)
  
  let toml_imported_service_name = Configuration::get_string(toml_imported_config, "service.name")
  match toml_imported_service_name {
    Some(name) => assert_eq(name, "azimuth-service"),
    None => assert_true(false)
  }
  
  // Test selective export
  let selective_export = Configuration::export_selective_to_json(config, [
    "service.name",
    "server.port",
    "debug.enabled"
  ])
  
  assert_true(selective_export.contains("\"service.name\": \"azimuth-service\""))
  assert_true(selective_export.contains("\"server.port\": 8080"))
  assert_true(selective_export.contains("\"debug.enabled\": false"))
  assert_false(selective_export.contains("\"service.version\""))
  assert_false(selective_export.contains("\"sampling.ratio\""))
  
  // Test export with masking
  let secure_config = Configuration::new()
  Configuration::set_string(secure_config, "service.name", "public-service")
  Configuration::set_string(secure_config, "database.password", "secret123")
  Configuration::set_string(secure_config, "api.key", "api-key-456")
  
  Configuration::mark_sensitive(secure_config, "database.password")
  Configuration::mark_sensitive(secure_config, "api.key")
  
  let masked_export = Configuration::export_to_json_with_masking(secure_config)
  
  assert_true(masked_export.contains("\"service.name\": \"public-service\""))
  assert_true(masked_export.contains("\"database.password\": \"*****\""))
  assert_true(masked_export.contains("\"api.key\": \"*****\""))
  assert_false(masked_export.contains("secret123"))
  assert_false(masked_export.contains("api-key-456"))
  
  // Test configuration diff
  let config1 = Configuration::new()
  Configuration::set_string(config1, "service.name", "service-1")
  Configuration::set_int(config1, "server.port", 8080)
  Configuration::set_bool(config1, "debug.enabled", false)
  
  let config2 = Configuration::new()
  Configuration::set_string(config2, "service.name", "service-2")
  Configuration::set_int(config2, "server.port", 8080)
  Configuration::set_bool(config2, "debug.enabled", true)
  Configuration::set_string(config2, "new.field", "new-value")
  
  let diff = Configuration::diff(config1, config2)
  
  assert_eq(diff.changed.length(), 2) // service.name and debug.enabled
  assert_eq(diff.added.length(), 1)  // new.field
  assert_eq(diff.removed.length(), 0) // None
  
  assert_true(diff.changed.any(|change| change.key == "service.name" && change.old_value == "service-1" && change.new_value == "service-2"))
  assert_true(diff.changed.any(|change| change.key == "debug.enabled" && change.old_value == false && change.new_value == true))
  assert_true(diff.added.any(|addition| addition.key == "new.field" && addition.value == "new-value"))
  
  // Test configuration merge with diff
  let merged_with_diff = Configuration::merge_with_diff(config1, diff)
  
  let merged_service_name = Configuration::get_string(merged_with_diff, "service.name")
  match merged_service_name {
    Some(name) => assert_eq(name, "service-2"),
    None => assert_true(false)
  }
  
  let merged_debug_enabled = Configuration::get_bool(merged_with_diff, "debug.enabled")
  match merged_debug_enabled {
    Some(enabled) => assert_true(enabled),
    None => assert_true(false)
  }
  
  let merged_new_field = Configuration::get_string(merged_with_diff, "new.field")
  match merged_new_field {
    Some(value) => assert_eq(value, "new-value"),
    None => assert_true(false)
  }
}

// Test 10: Configuration Performance and Optimization
test "configuration performance and optimization" {
  // Test configuration creation performance
  let start_time = PerformanceCounter::now()
  
  for i = 0; i < 10000; i = i + 1 {
    let config = Configuration::new()
    Configuration::set_string(config, "key" + i.to_string(), "value" + i.to_string())
    Configuration::set_int(config, "int_key" + i.to_string(), i)
    Configuration::set_bool(config, "bool_key" + i.to_string(), i % 2 == 0)
  }
  
  let end_time = PerformanceCounter::now()
  let duration = PerformanceCounter::duration_ms(start_time, end_time)
  
  // Should complete within reasonable time
  assert_true(duration < 5000.0) // 5 seconds
  
  // Test configuration lookup performance
  let large_config = Configuration::new()
  
  for i = 0; i < 10000; i = i + 1 {
    Configuration::set_string(large_config, "key" + i.to_string(), "value" + i.to_string())
    Configuration::set_int(large_config, "int_key" + i.to_string(), i)
    Configuration::set_bool(large_config, "bool_key" + i.to_string(), i % 2 == 0)
  }
  
  let lookup_start = PerformanceCounter::now()
  
  for i = 0; i < 10000; i = i + 1 {
    let key = "key" + i.to_string()
    let _ = Configuration::get_string(large_config, key)
    
    let int_key = "int_key" + i.to_string()
    let _ = Configuration::get_int(large_config, int_key)
    
    let bool_key = "bool_key" + i.to_string()
    let _ = Configuration::get_bool(large_config, bool_key)
  }
  
  let lookup_end = PerformanceCounter::now()
  let lookup_duration = PerformanceCounter::duration_ms(lookup_start, lookup_end)
  
  // Should complete within reasonable time
  assert_true(lookup_duration < 3000.0) // 3 seconds
  
  // Test configuration merge performance
  let configs = []
  
  for i = 0; i < 100; i = i + 1 {
    let config = Configuration::new()
    Configuration::set_string(config, "key" + i.to_string(), "value" + i.to_string())
    Configuration::set_int(config, "int_key" + i.to_string(), i)
    configs.push(config)
  }
  
  let merge_start = PerformanceCounter::now()
  
  let merged_config = Configuration::merge_multiple(configs)
  
  let merge_end = PerformanceCounter::now()
  let merge_duration = PerformanceCounter::duration_ms(merge_start, merge_end)
  
  // Should complete within reasonable time
  assert_true(merge_duration < 1000.0) // 1 second
  
  // Test configuration serialization performance
  let serialization_config = Configuration::new()
  
  for i = 0; i < 1000; i = i + 1 {
    Configuration::set_string(serialization_config, "key" + i.to_string(), "value" + i.to_string())
    Configuration::set_int(serialization_config, "int_key" + i.to_string(), i)
    Configuration::set_bool(serialization_config, "bool_key" + i.to_string(), i % 2 == 0)
  }
  
  let serialization_start = PerformanceCounter::now()
  
  let json_export = Configuration::export_to_json(serialization_config)
  let yaml_export = Configuration::export_to_yaml(serialization_config)
  let toml_export = Configuration::export_to_toml(serialization_config)
  
  let serialization_end = PerformanceCounter::now()
  let serialization_duration = PerformanceCounter::duration_ms(serialization_start, serialization_end)
  
  // Should complete within reasonable time
  assert_true(serialization_duration < 2000.0) // 2 seconds
  
  // Verify exports are not empty
  assert_true(json_export.length() > 0)
  assert_true(yaml_export.length() > 0)
  assert_true(toml_export.length() > 0)
  
  // Test configuration deserialization performance
  let deserialization_start = PerformanceCounter::now()
  
  let json_imported = Configuration::import_from_json(json_export)
  let yaml_imported = Configuration::import_from_yaml(yaml_export)
  let toml_imported = Configuration::import_from_toml(toml_export)
  
  let deserialization_end = PerformanceCounter::now()
  let deserialization_duration = PerformanceCounter::duration_ms(deserialization_start, deserialization_end)
  
  // Should complete within reasonable time
  assert_true(deserialization_duration < 3000.0) // 3 seconds
  
  // Verify imported configurations have the right number of keys
  assert_eq(Configuration::keys(json_imported).length(), 3000) // 1000 each for string, int, bool
  assert_eq(Configuration::keys(yaml_imported).length(), 3000)
  assert_eq(Configuration::keys(toml_imported).length(), 3000)
  
  // Test memory usage
  let initial_memory = System::memory_usage()
  
  let memory_configs = []
  for i = 0; i < 1000; i = i + 1 {
    let config = Configuration::new()
    Configuration::set_string(config, "key", "x".repeat(1000)) // 1KB per config
    memory_configs.push(config)
  }
  
  let peak_memory = System::memory_usage()
  let memory_increase = peak_memory - initial_memory
  
  // Memory usage should be reasonable
  assert_true(memory_increase < 50000000) // Less than 50MB for 1000 configs with 1KB each
  
  // Test configuration caching
  let cache = ConfigurationCache::new(100) // Cache 100 configurations
  
  let cached_config = Configuration::new()
  Configuration::set_string(cached_config, "service.name", "cached-service")
  Configuration::set_int(cached_config, "server.port", 8080)
  
  // Store in cache
  ConfigurationCache::put(cache, "test-config", cached_config)
  
  // Retrieve from cache
  let cache_start = PerformanceCounter::now()
  
  for i = 0; i < 10000; i = i + 1 {
    let _ = ConfigurationCache::get(cache, "test-config")
  }
  
  let cache_end = PerformanceCounter::now()
  let cache_duration = PerformanceCounter::duration_ms(cache_start, cache_end)
  
  // Should be very fast
  assert_true(cache_duration < 1000.0) // 1 second
  
  // Verify cached value
  let cached_retrieved = ConfigurationCache::get(cache, "test-config")
  match cached_retrieved {
    Some(config) => {
      let service_name = Configuration::get_string(config, "service.name")
      match service_name {
        Some(name) => assert_eq(name, "cached-service"),
        None => assert_true(false)
      }
    }
    None => assert_true(false) // Should find cached config
  }
}

// Mock implementations for testing
type Configuration
type ConfigurationTemplate
type ConfigurationProfile
type ConfigurationWatcher
type ConfigurationCache
type ConfigurationDiff
type ValidationRule
type ValidationResult
type TempFile
type Environment
type Thread

// Configuration
func Configuration::new() -> Configuration { /* implementation */ }
func Configuration::new_with_validation() -> Configuration { /* implementation */ }
func Configuration::new_with_access_control() -> Configuration { /* implementation */ }
func Configuration::set_string(config : Configuration, key : String, value : String) -> Unit { /* implementation */ }
func Configuration::set_int(config : Configuration, key : String, value : Int) -> Unit { /* implementation */ }
func Configuration::set_bool(config : Configuration, key : String, value : Bool) -> Unit { /* implementation */ }
func Configuration::set_float(config : Configuration, key : String, value : Float) -> Unit { /* implementation */ }
func Configuration::get_string(config : Configuration, key : String) -> Option[String] { Some("") }
func Configuration::get_string_with_default(config : Configuration, key : String, default : String) -> String { default }
func Configuration::get_int(config : Configuration, key : String) -> Option[Int] { Some(0) }
func Configuration::get_int_with_default(config : Configuration, key : String, default : Int) -> Int { default }
func Configuration::get_bool(config : Configuration, key : String) -> Option[Bool] { Some(false) }
func Configuration::get_float(config : Configuration, key : String) -> Option[Float] { Some(0.0) }
func Configuration::get_string_masked(config : Configuration, key : String) -> Option[String] { Some("") }
func Configuration::has_key(config : Configuration, key : String) -> Bool { false }
func Configuration::keys(config : Configuration) -> Array[String] { [] }
func Configuration::load_from_json_file(path : String) -> Configuration { /* implementation */ }
func Configuration::load_from_yaml_file(path : String) -> Configuration { /* implementation */ }
func Configuration::load_from_toml_file(path : String) -> Configuration { /* implementation */ }
func Configuration::load_from_environment(prefix : String) -> Configuration { /* implementation */ }
func Configuration::merge(base : Configuration, override : Configuration) -> Configuration { /* implementation */ }
func Configuration::merge_multiple(configs : Array[(String, Configuration)]) -> Configuration { /* implementation */ }
func Configuration::merge_with_environment(config : Configuration, prefix : String) -> Configuration { /* implementation */ }
func Configuration::add_validation_rule(config : Configuration, key : String, rule : ValidationRule) -> Unit { /* implementation */ }
func Configuration::add_required_field(config : Configuration, key : String) -> Unit { /* implementation */ }
func Configuration::add_custom_validation(config : Configuration, key : String, validator_fn : Any -> Bool, message : String) -> Unit { /* implementation */ }
func Configuration::validate(config : Configuration) -> ValidationResult { /* implementation */ }
func Configuration::load_from_json_file_with_validation(path : String, rules : Array[(String, ValidationRule)]) -> Configuration { /* implementation */ }
func Configuration::mark_sensitive(config : Configuration, key : String) -> Unit { /* implementation */ }
func Configuration::encrypt(config : Configuration, key : String) -> Configuration { /* implementation */ }
func Configuration::decrypt(config : Configuration, key : String) -> Configuration { /* implementation */ }
func Configuration::rotate_key(config : Configuration, old_key : String, new_key : String) -> Configuration { /* implementation */ }
func Configuration::set_access_level(config : Configuration, key : String, level : String) -> Unit { /* implementation */ }
func Configuration::with_role(config : Configuration, role : String) -> Configuration { /* implementation */ }
func Configuration::from_template(template : ConfigurationTemplate) -> Configuration { /* implementation */ }
func Configuration::apply_profile(config : Configuration, profile : ConfigurationProfile) -> Configuration { /* implementation */ }
func Configuration::apply_profile_with_validation(config : Configuration, profile : ConfigurationProfile) -> Result[Configuration, Array[String]] { /* implementation */ }
func Configuration::export_to_json(config : Configuration) -> String { "" }
func Configuration::export_to_yaml(config : Configuration) -> String { "" }
func Configuration::export_to_toml(config : Configuration) -> String { "" }
func Configuration::export_selective_to_json(config : Configuration, keys : Array[String]) -> String { "" }
func Configuration::export_to_json_with_masking(config : Configuration) -> String { "" }
func Configuration::import_from_json(json : String) -> Configuration { /* implementation */ }
func Configuration::import_from_yaml(yaml : String) -> Configuration { /* implementation */ }
func Configuration::import_from_toml(toml : String) -> Configuration { /* implementation */ }
func Configuration::load_from_multiple_json_files(paths : Array[String]) -> Configuration { /* implementation */ }
func Configuration::diff(config1 : Configuration, config2 : Configuration) -> ConfigurationDiff { /* implementation */ }
func Configuration::merge_with_diff(config : Configuration, diff : ConfigurationDiff) -> Configuration { /* implementation */ }

// Configuration template
func ConfigurationTemplate::new(name : String) -> ConfigurationTemplate { /* implementation */ }
func ConfigurationTemplate::add_field(template : ConfigurationTemplate, key : String, default_value : Any, type : Any) -> Unit { /* implementation */ }

// Configuration profile
func ConfigurationProfile::new(name : String) -> ConfigurationProfile { /* implementation */ }
func ConfigurationProfile::set_value(profile : ConfigurationProfile, key : String, value : Any) -> Unit { /* implementation */ }
func ConfigurationProfile::inherit_from(profile : ConfigurationProfile, parent : ConfigurationProfile) -> Unit { /* implementation */ }
func ConfigurationProfile::add_validation_rule(profile : ConfigurationProfile, key : String, rule : ValidationRule) -> Unit { /* implementation */ }

// Configuration watcher
func ConfigurationWatcher::new(config : Configuration) -> ConfigurationWatcher { /* implementation */ }
func ConfigurationWatcher::on_change(watcher : ConfigurationWatcher, callback_fn : () -> Unit) -> Unit { /* implementation */ }
func ConfigurationWatcher::start(watcher : ConfigurationWatcher, file_path : String) -> Unit { /* implementation */ }
func ConfigurationWatcher::stop(watcher : ConfigurationWatcher) -> Unit { /* implementation */ }

// Configuration cache
func ConfigurationCache::new(size : Int) -> ConfigurationCache { /* implementation */ }
func ConfigurationCache::put(cache : ConfigurationCache, key : String, config : Configuration) -> Unit { /* implementation */ }
func ConfigurationCache::get(cache : ConfigurationCache, key : String) -> Option[Configuration] { Some(/* config */) }

// Validation rule
func ValidationRule::int_range(min : Int, max : Int) -> ValidationRule { /* implementation */ }
func ValidationRule::int_min(min : Int) -> ValidationRule { /* implementation */ }
func ValidationRule::float_range(min : Float, max : Float) -> ValidationRule { /* implementation */ }
func ValidationRule::float_min(min : Float) -> ValidationRule { /* implementation */ }
func ValidationRule::non_empty_string() -> ValidationRule { /* implementation */ }

// Validation result
type ValidationResult {
  is_valid : Bool,
  errors : Array[String]
}

// Temp file
func TempFile::create_with_content(content : String) -> TempFile { /* implementation */ }
func TempFile::path(file : TempFile) -> String { "" }
func TempFile::update_content(file : TempFile, content : String) -> Unit { /* implementation */ }

// Environment
func Environment::set_var(key : String, value : String) -> Unit { /* implementation */ }
func Environment::remove_var(key : String) -> Unit { /* implementation */ }

// Thread utilities
func Thread::sleep(ms : Int) -> Unit { /* implementation */ }

// Performance counter
func PerformanceCounter::now() -> PerformanceCounter { /* implementation */ }
func PerformanceCounter::duration_ms(start : PerformanceCounter, end : PerformanceCounter) -> Float { 0.0 }

// System utilities
func System::memory_usage() -> Int { 0 }

// Types
type ConfigurationDiff {
  changed : Array[ConfigurationChange],
  added : Array[ConfigurationAddition],
  removed : Array[ConfigurationRemoval]
}

type ConfigurationChange {
  key : String,
  old_value : Any,
  new_value : Any
}

type ConfigurationAddition {
  key : String,
  value : Any
}

type ConfigurationRemoval {
  key : String,
  value : Any
}

type ValidationRule
type StringType
type IntType
type BoolType
type FloatType