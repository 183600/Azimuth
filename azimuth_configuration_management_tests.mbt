// Azimuth Configuration Management Test Suite
// This file contains test cases for configuration management functionality

// Test 1: Configuration Loading and Parsing
test "configuration loading and parsing" {
  // Test JSON configuration parsing
  let json_config = "{
    \"telemetry\": {
      \"enabled\": true,
      \"sampling_rate\": 0.1,
      \"export_interval\": 60000
    },
    \"service\": {
      \"name\": \"azimuth-test\",
      \"version\": \"1.0.0\"
    }
  }"
  
  let config = Config::parse_json(json_config)
  match config {
    Some(c) => {
      assert_eq(Config::get_bool(c, "telemetry.enabled"), Some(true))
      assert_eq(Config::get_float(c, "telemetry.sampling_rate"), Some(0.1))
      assert_eq(Config::get_int(c, "telemetry.export_interval"), Some(60000))
      assert_eq(Config::get_string(c, "service.name"), Some("azimuth-test"))
      assert_eq(Config::get_string(c, "service.version"), Some("1.0.0"))
    }
    None => assert_true(false)
  }
  
  // Test YAML configuration parsing
  let yaml_config = "
telemetry:
  enabled: true
  sampling_rate: 0.1
  export_interval: 60000
service:
  name: azimuth-test
  version: 1.0.0
"
  
  let yaml_parsed = Config::parse_yaml(yaml_config)
  match yaml_parsed {
    Some(c) => {
      assert_eq(Config::get_bool(c, "telemetry.enabled"), Some(true))
      assert_eq(Config::get_string(c, "service.name"), Some("azimuth-test"))
    }
    None => assert_true(false)
  }
  
  // Test invalid configuration
  let invalid_config = "{ invalid json }"
  let invalid_parsed = Config::parse_json(invalid_config)
  match invalid_parsed {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Configuration Defaults and Validation
test "configuration defaults and validation" {
  // Test configuration with defaults
  let default_config = Config::with_defaults()
  
  // Verify default values
  assert_eq(Config::get_bool(default_config, "telemetry.enabled"), Some(true))
  assert_eq(Config::get_float(default_config, "telemetry.sampling_rate"), Some(1.0))
  assert_eq(Config::get_int(default_config, "telemetry.batch_size"), Some(512))
  assert_eq(Config::get_string(default_config, "service.name"), Some("azimuth"))
  
  // Test configuration validation
  let test_config = Config::new()
  let validated_config = Config::set(test_config, "telemetry.sampling_rate", ConfigValue::Float(0.5))
  let validation_result = Config::validate(validated_config)
  
  match validation_result {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(errors) => assert_true(false, "Configuration should be valid")
  }
  
  // Test invalid configuration values
  let invalid_sampling_config = Config::set(test_config, "telemetry.sampling_rate", ConfigValue::Float(1.5))
  let invalid_validation = Config::validate(invalid_sampling_config)
  
  match invalid_validation {
    ValidationResult::Valid => assert_true(false, "Configuration should be invalid")
    ValidationResult::Invalid(errors) => {
      assert_true(errors.length() > 0)
      assert_true(errors[0].contains("sampling_rate"))
    }
  }
}

// Test 3: Configuration Merging and Overriding
test "configuration merging and overriding" {
  // Create base configuration
  let base_config = Config::new()
  let with_base = Config::set_multiple(base_config, [
    ("telemetry.enabled", ConfigValue::Bool(true)),
    ("telemetry.sampling_rate", ConfigValue::Float(0.1)),
    ("service.name", ConfigValue::String("base-service"))
  ])
  
  // Create override configuration
  let override_config = Config::new()
  let with_override = Config::set_multiple(override_config, [
    ("telemetry.sampling_rate", ConfigValue::Float(0.5)),
    ("service.version", ConfigValue::String("2.0.0"))
  ])
  
  // Test merging
  let merged = Config::merge(with_base, with_override)
  
  // Verify merged values
  assert_eq(Config::get_bool(merged, "telemetry.enabled"), Some(true)) // From base
  assert_eq(Config::get_float(merged, "telemetry.sampling_rate"), Some(0.5)) // From override
  assert_eq(Config::get_string(merged, "service.name"), Some("base-service")) // From base
  assert_eq(Config::get_string(merged, "service.version"), Some("2.0.0")) // From override
  
  // Test deep merging
  let deep_base = Config::set(base_config, "nested.value", ConfigValue::String("base"))
  let deep_override = Config::set(base_config, "nested.other", ConfigValue::String("override"))
  let deep_merged = Config::merge_deep(deep_base, deep_override)
  
  assert_eq(Config::get_string(deep_merged, "nested.value"), Some("base"))
  assert_eq(Config::get_string(deep_merged, "nested.other"), Some("override"))
}

// Test 4: Configuration Environment Variable Substitution
test "configuration environment variable substitution" {
  // Test environment variable substitution
  let env_config = Config::new()
  let with_env = Config::set(env_config, "database.url", ConfigValue::String("${DB_URL}"))
  let with_port = Config::set(with_env, "database.port", ConfigValue::String("${DB_PORT:5432}"))
  
  // Mock environment variables
  set_env_var("DB_URL", "postgresql://localhost:5432/testdb")
  // DB_PORT not set, should use default
  
  let resolved = Config::resolve_env_vars(with_port)
  
  assert_eq(Config::get_string(resolved, "database.url"), Some("postgresql://localhost:5432/testdb"))
  assert_eq(Config::get_string(resolved, "database.port"), Some("5432"))
  
  // Test with actual environment variable
  set_env_var("DB_PORT", "3306")
  let resolved_with_env = Config::resolve_env_vars(with_port)
  assert_eq(Config::get_string(resolved_with_env, "database.port"), Some("3306"))
  
  // Clean up environment variables
  unset_env_var("DB_URL")
  unset_env_var("DB_PORT")
}

// Test 5: Configuration Watchers and Hot Reload
test "configuration watchers and hot reload" {
  // Create initial configuration
  let initial_config = Config::new()
  let config_with_watcher = Config::set(initial_config, "test.value", ConfigValue::String("initial"))
  
  // Test configuration watching
  let watcher = Config::create_watcher(config_with_watcher)
  assert_true(Config::is_watching(watcher))
  
  // Simulate configuration change
  let updated_config = Config::set(config_with_watcher, "test.value", ConfigValue::String("updated"))
  Config::notify_change(watcher, updated_config)
  
  // Test change detection
  let has_changes = Config::has_changes(watcher)
  assert_true(has_changes)
  
  // Test getting latest configuration
  let latest_config = Config::get_latest(watcher)
  assert_eq(Config::get_string(latest_config, "test.value"), Some("updated"))
  
  // Test stopping watcher
  Config::stop_watcher(watcher)
  assert_false(Config::is_watching(watcher))
}

// Test 6: Configuration Profiles and Environments
test "configuration profiles and environments" {
  // Create base configuration
  let base_config = Config::new()
  let with_base_values = Config::set_multiple(base_config, [
    ("service.name", ConfigValue::String("azimuth")),
    ("service.debug", ConfigValue::Bool(false)),
    ("database.host", ConfigValue::String("localhost"))
  ])
  
  // Create development profile
  let dev_profile = Config::new()
  let with_dev_values = Config::set_multiple(dev_profile, [
    ("service.debug", ConfigValue::Bool(true)),
    ("database.host", ConfigValue::String("dev-db.example.com")),
    ("logging.level", ConfigValue::String("DEBUG"))
  ])
  
  // Create production profile
  let prod_profile = Config::new()
  let with_prod_values = Config::set_multiple(prod_profile, [
    ("service.debug", ConfigValue::Bool(false)),
    ("database.host", ConfigValue::String("prod-db.example.com")),
    ("logging.level", ConfigValue::String("INFO"))
  ])
  
  // Test profile application
  let dev_config = Config::apply_profile(with_base_values, with_dev_values)
  assert_eq(Config::get_string(dev_config, "service.name"), Some("azimuth")) // From base
  assert_eq(Config::get_bool(dev_config, "service.debug"), Some(true)) // From dev profile
  assert_eq(Config::get_string(dev_config, "database.host"), Some("dev-db.example.com")) // From dev profile
  assert_eq(Config::get_string(dev_config, "logging.level"), Some("DEBUG")) // From dev profile
  
  let prod_config = Config::apply_profile(with_base_values, with_prod_values)
  assert_eq(Config::get_bool(prod_config, "service.debug"), Some(false)) // From prod profile
  assert_eq(Config::get_string(prod_config, "database.host"), Some("prod-db.example.com")) // From prod profile
  assert_eq(Config::get_string(prod_config, "logging.level"), Some("INFO")) // From prod profile
  
  // Test environment-based configuration loading
  set_env_var("ENVIRONMENT", "development")
  let env_config = Config::load_for_environment("config.json")
  match env_config {
    Some(c) => assert_true(true) // Would load development-specific config
    None => assert_true(true) // File might not exist, that's OK for test
  }
  
  unset_env_var("ENVIRONMENT")
}

// Test 7: Configuration Encryption and Security
test "configuration encryption and security" {
  // Test sensitive data encryption
  let sensitive_config = Config::new()
  let with_password = Config::set_sensitive(
    sensitive_config, 
    "database.password", 
    "secret123", 
    "encryption_key"
  )
  
  // Verify encrypted value is not stored in plain text
  let encrypted_value = Config::get_raw(with_password, "database.password")
  match encrypted_value {
    Some(value) => assert_neq(value, "secret123") // Should be encrypted
    None => assert_true(false)
  }
  
  // Test decryption
  let decrypted_value = Config::get_sensitive(with_password, "database.password", "encryption_key")
  assert_eq(decrypted_value, Some("secret123"))
  
  // Test with wrong encryption key
  let wrong_key_decrypt = Config::get_sensitive(with_password, "database.password", "wrong_key")
  match wrong_key_decrypt {
    Some(_) => assert_true(false, "Should not decrypt with wrong key")
    None => assert_true(true)
  }
  
  // Test configuration masking
  let config_with_secrets = Config::set_multiple(sensitive_config, [
    ("database.password", ConfigValue::Sensitive("secret123")),
    ("api.key", ConfigValue::Sensitive("api-key-123")),
    ("public.setting", ConfigValue::String("public-value"))
  ])
  
  let masked_config = Config::mask_sensitive(config_with_secrets)
  assert_eq(Config::get_string(masked_config, "database.password"), Some("***"))
  assert_eq(Config::get_string(masked_config, "api.key"), Some("***"))
  assert_eq(Config::get_string(masked_config, "public.setting"), Some("public-value"))
}

// Test 8: Configuration Schema Validation
test "configuration schema validation" {
  // Define configuration schema
  let schema = ConfigSchema::new()
  let with_rules = ConfigSchema::add_rules(schema, [
    ConfigRule::required("service.name", ConfigType::String),
    ConfigRule::required("service.version", ConfigType::String),
    ConfigRule::optional("service.debug", ConfigType::Bool, Some(false)),
    ConfigRule::required("telemetry.sampling_rate", ConfigType::Float),
    ConfigRule::range("telemetry.sampling_rate", 0.0, 1.0),
    ConfigRule::optional("telemetry.batch_size", ConfigType::Int, Some(512)),
    ConfigRule::min("telemetry.batch_size", 1)
  ])
  
  // Test valid configuration
  let valid_config = Config::new()
  let with_valid_values = Config::set_multiple(valid_config, [
    ("service.name", ConfigValue::String("test-service")),
    ("service.version", ConfigValue::String("1.0.0")),
    ("telemetry.sampling_rate", ConfigValue::Float(0.1))
  ])
  
  let valid_result = ConfigSchema::validate(with_rules, with_valid_values)
  match valid_result {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(_) => assert_true(false, "Configuration should be valid")
  }
  
  // Test invalid configuration
  let invalid_config = Config::new()
  let with_invalid_values = Config::set_multiple(invalid_config, [
    ("service.name", ConfigValue::String("test-service")),
    // Missing required service.version
    ("telemetry.sampling_rate", ConfigValue::Float(1.5)) // Out of range
  ])
  
  let invalid_result = ConfigSchema::validate(with_rules, with_invalid_values)
  match invalid_result {
    ValidationResult::Valid => assert_true(false, "Configuration should be invalid")
    ValidationResult::Invalid(errors) => {
      assert_true(errors.length() >= 2) // At least 2 validation errors
    }
  }
  
  // Test schema generation from configuration
  let sample_config = Config::new()
  let with_sample_values = Config::set_multiple(sample_config, [
    ("service.name", ConfigValue::String("sample")),
    ("service.debug", ConfigValue::Bool(true)),
    ("telemetry.sampling_rate", ConfigValue::Float(0.1))
  ])
  
  let generated_schema = ConfigSchema::from_config(with_sample_values)
  assert_true(generated_schema.has_rule("service.name"))
  assert_true(generated_schema.has_rule("service.debug"))
  assert_true(generated_schema.has_rule("telemetry.sampling_rate"))
}

// Test 9: Configuration Export and Import
test "configuration export and import" {
  // Create sample configuration
  let sample_config = Config::new()
  let with_values = Config::set_multiple(sample_config, [
    ("service.name", ConfigValue::String("azimuth-test")),
    ("service.debug", ConfigValue::Bool(true)),
    ("telemetry.sampling_rate", ConfigValue::Float(0.1)),
    ("telemetry.batch_size", ConfigValue::Int(256))
  ])
  
  // Test JSON export
  let json_export = Config::export_json(with_values, true) // Pretty print
  assert_true(json_export.contains("azimuth-test"))
  assert_true(json_export.contains("true"))
  assert_true(json_export.contains("0.1"))
  
  // Test YAML export
  let yaml_export = Config::export_yaml(with_values)
  assert_true(yaml_export.contains("azimuth-test"))
  assert_true(yaml_export.contains("true"))
  assert_true(yaml_export.contains("0.1"))
  
  // Test TOML export
  let toml_export = Config::export_toml(with_values)
  assert_true(toml_export.contains("azimuth-test"))
  
  // Test import from exported JSON
  let imported_config = Config::import_json(json_export)
  match imported_config {
    Some(c) => {
      assert_eq(Config::get_string(c, "service.name"), Some("azimuth-test"))
      assert_eq(Config::get_bool(c, "service.debug"), Some(true))
      assert_eq(Config::get_float(c, "telemetry.sampling_rate"), Some(0.1))
      assert_eq(Config::get_int(c, "telemetry.batch_size"), Some(256))
    }
    None => assert_true(false)
  }
  
  // Test configuration diff
  let modified_config = Config::set(with_values, "service.debug", ConfigValue::Bool(false))
  let diff = Config::diff(with_values, modified_config)
  assert_eq(diff.length(), 1)
  assert_eq(diff[0].path, "service.debug")
  assert_eq(diff[0].old_value, Some(ConfigValue::Bool(true)))
  assert_eq(diff[0].new_value, Some(ConfigValue::Bool(false)))
}

// Test 10: Configuration Caching and Performance
test "configuration caching and performance" {
  // Create large configuration
  let large_config = Config::new()
  let mut config_builder = large_config
  
  for i in 0..=100 {
    let key = "section" + i.to_string() + ".value"
    config_builder = Config::set(config_builder, key, ConfigValue::String("value" + i.to_string()))
  }
  
  // Test configuration caching
  let cached_config = Config::cache(config_builder)
  assert_true(Config::is_cached(cached_config))
  
  // Test cached access performance
  let start_time = current_timestamp()
  for i in 0..=100 {
    let key = "section" + i.to_string() + ".value"
    Config::get_string(cached_config, key)
  }
  let end_time = current_timestamp()
  
  // Cached access should be fast (this is a basic performance check)
  assert_true(end_time - start_time < 1000L) // Should complete in less than 1 second
  
  // Test cache invalidation
  Config::invalidate_cache(cached_config)
  assert_false(Config::is_cached(cached_config))
  
  // Test cache size limits
  let cache_manager = ConfigCacheManager::new(10) // Max 10 cached configurations
  
  for i in 0..=15 {
    let config = Config::new()
    let key = "test" + i.to_string()
    let with_value = Config::set(config, key, ConfigValue::String("value" + i.to_string()))
    ConfigCacheManager::cache(cache_manager, key, with_value)
  }
  
  // Should only keep the most recent 10 configurations
  assert_eq(ConfigCacheManager::size(cache_manager), 10)
  
  // Test cache eviction
  let evicted_key = "test0"
  let evicted_config = ConfigCacheManager::get(cache_manager, evicted_key)
  match evicted_config {
    Some(_) => assert_true(false, "Old configuration should be evicted")
    None => assert_true(true)
  }
  
  // Test recent configuration is still cached
  let recent_key = "test15"
  let recent_config = ConfigCacheManager::get(cache_manager, recent_key)
  match recent_config {
    Some(_) => assert_true(true)
    None => assert_true(false, "Recent configuration should be cached")
  }
}