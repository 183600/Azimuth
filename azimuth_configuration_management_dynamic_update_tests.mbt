// Azimuth Telemetry System - Configuration Management and Dynamic Update Tests
// This file contains comprehensive test cases for configuration management and dynamic updates

// Test 1: Basic Configuration Loading and Validation
test "basic configuration loading and validation" {
  let config_manager = ConfigurationManager::new()
  
  // Test loading configuration from file
  let config_file = "test_config.json"
  let config_content = create_test_config_content()
  
  let result = ConfigurationManager::load_from_file(config_manager, config_file, config_content)
  match result {
    Success(config) => {
      // Verify configuration values
      assert_eq(config.get_string("service.name"), "test-service")
      assert_eq(config.get_int("service.port"), 8080)
      assert_eq(config.get_bool("service.enabled"), true)
      assert_eq(config.get_float("service.threshold"), 0.95)
      
      // Verify nested configuration
      assert_eq(config.get_string("telemetry.exporter.type"), "jaeger")
      assert_eq(config.get_int("telemetry.exporter.port"), 14268)
      
      // Verify array configuration
      let endpoints = config.get_string_array("service.endpoints")
      assert_eq(endpoints.length(), 3)
      assert_eq(endpoints[0], "http://localhost:8080")
      assert_eq(endpoints[1], "http://localhost:8081")
      assert_eq(endpoints[2], "http://localhost:8082")
    }
    Error(_) => assert_true(false)
  }
  
  // Test configuration validation
  let validation_schema = create_validation_schema()
  let validation_result = ConfigurationManager::validate(config_manager, validation_schema)
  
  match validation_result {
    Success => assert_true(true)
    Error(errors) => {
      // Print validation errors for debugging
      for error in errors {
        println("Validation error: " + error.message)
      }
      assert_true(false) // Should pass validation
    }
  }
  
  // Test invalid configuration
  let invalid_config_content = create_invalid_config_content()
  let result = ConfigurationManager::load_from_file(config_manager, "invalid_config.json", invalid_config_content)
  match result {
    Success(_) => assert_true(false) // Should fail
    Error(err) => assert_eq(err.code, "INVALID_CONFIGURATION")
  }
}

// Test 2: Configuration Schema Validation
test "configuration schema validation" {
  let config_manager = ConfigurationManager::new()
  let schema_manager = SchemaManager::new()
  
  // Create configuration schema
  let schema = SchemaManager::create_schema(schema_manager, "telemetry_config")
  
  // Add required fields
  SchemaManager::add_required_field(schema, "service.name", "string")
  SchemaManager::add_required_field(schema, "service.port", "integer")
  SchemaManager::add_required_field(schema, "service.enabled", "boolean")
  
  // Add optional fields with defaults
  SchemaManager::add_optional_field(schema, "service.timeout", "integer", Some("30"))
  SchemaManager::add_optional_field(schema, "service.retries", "integer", Some("3"))
  SchemaManager::add_optional_field(schema, "service.debug", "boolean", Some("false"))
  
  // Add field constraints
  SchemaManager::add_constraint(schema, "service.port", "min", "1")
  SchemaManager::add_constraint(schema, "service.port", "max", "65535")
  SchemaManager::add_constraint(schema, "service.timeout", "min", "1")
  SchemaManager::add_constraint(schema, "service.timeout", "max", "300")
  
  // Test valid configuration
  let valid_config = create_valid_config_with_schema()
  let validation_result = SchemaManager::validate(schema_manager, schema, valid_config)
  match validation_result {
    Success => assert_true(true)
    Error(errors) => {
      for error in errors {
        println("Schema validation error: " + error.message)
      }
      assert_true(false)
    }
  }
  
  // Test invalid configuration - missing required field
  let invalid_config_missing = create_config_missing_required_field()
  let validation_result = SchemaManager::validate(schema_manager, schema, invalid_config_missing)
  match validation_result {
    Success => assert_true(false) // Should fail
    Error(errors) => {
      assert_true(errors.length() > 0)
      assert_true(errors[0].message.contains("Missing required field"))
    }
  }
  
  // Test invalid configuration - constraint violation
  let invalid_config_constraint = create_config_with_constraint_violation()
  let validation_result = SchemaManager::validate(schema_manager, schema, invalid_config_constraint)
  match validation_result {
    Success => assert_true(false) // Should fail
    Error(errors) => {
      assert_true(errors.length() > 0)
      assert_true(errors[0].message.contains("Constraint violation"))
    }
  }
}

// Test 3: Dynamic Configuration Updates
test "dynamic configuration updates" {
  let config_manager = ConfigurationManager::new()
  let update_manager = DynamicUpdateManager::new()
  
  // Load initial configuration
  let initial_config = create_initial_config()
  ConfigurationManager::load(config_manager, initial_config)
  
  // Register configuration change listeners
  let mut listener_calls = 0
  let listener = DynamicUpdateManager::register_listener(update_manager, "service.port", fn(old_value, new_value) {
    listener_calls = listener_calls + 1
    assert_eq(old_value, "8080")
    assert_eq(new_value, "9090")
  })
  
  // Test simple configuration update
  let update_result = DynamicUpdateManager::update_value(update_manager, "service.port", "9090")
  match update_result {
    Success => {
      assert_eq(listener_calls, 1) // Listener should be called
      assert_eq(config_manager.get_string("service.port"), "9090")
    }
    Error(_) => assert_true(false)
  }
  
  // Test nested configuration update
  let nested_update_result = DynamicUpdateManager::update_value(update_manager, "telemetry.exporter.port", "14269")
  match nested_update_result {
    Success => {
      assert_eq(config_manager.get_string("telemetry.exporter.port"), "14269")
    }
    Error(_) => assert_true(false)
  }
  
  // Test batch configuration update
  let batch_updates = [
    ("service.timeout", "60"),
    ("service.retries", "5"),
    ("telemetry.batch.size", "1000")
  ]
  
  let batch_result = DynamicUpdateManager::update_batch(update_manager, batch_updates)
  match batch_result {
    Success => {
      assert_eq(config_manager.get_string("service.timeout"), "60")
      assert_eq(config_manager.get_string("service.retries"), "5")
      assert_eq(config_manager.get_string("telemetry.batch.size"), "1000")
    }
    Error(_) => assert_true(false)
  }
  
  // Test configuration update with validation
  let validation_schema = create_validation_schema()
  DynamicUpdateManager::enable_validation(update_manager, validation_schema)
  
  // Valid update should succeed
  let valid_update = DynamicUpdateManager::update_value(update_manager, "service.port", "8081")
  match valid_update {
    Success => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Invalid update should fail
  let invalid_update = DynamicUpdateManager::update_value(update_manager, "service.port", "99999") // Out of range
  match invalid_update {
    Success => assert_true(false) // Should fail
    Error(err) => assert_eq(err.code, "VALIDATION_ERROR")
  }
}

// Test 4: Configuration Rollback and Recovery
test "configuration rollback and recovery" {
  let config_manager = ConfigurationManager::new()
  let rollback_manager = RollbackManager::new()
  
  // Load initial configuration
  let initial_config = create_initial_config()
  ConfigurationManager::load(config_manager, initial_config)
  
  // Create configuration snapshot
  let snapshot_id = RollbackManager::create_snapshot(rollback_manager, config_manager)
  assert_true(snapshot_id > 0)
  
  // Make configuration changes
  ConfigurationManager::set_value(config_manager, "service.port", "9090")
  ConfigurationManager::set_value(config_manager, "service.timeout", "60")
  ConfigurationManager::set_value(config_manager, "telemetry.exporter.port", "14269")
  
  // Verify changes
  assert_eq(config_manager.get_string("service.port"), "9090")
  assert_eq(config_manager.get_string("service.timeout"), "60")
  assert_eq(config_manager.get_string("telemetry.exporter.port"), "14269")
  
  // Rollback to snapshot
  let rollback_result = RollbackManager::rollback_to_snapshot(rollback_manager, snapshot_id)
  match rollback_result {
    Success => {
      // Verify rollback restored original values
      assert_eq(config_manager.get_string("service.port"), "8080")
      assert_eq(config_manager.get_string("service.timeout"), "30")
      assert_eq(config_manager.get_string("telemetry.exporter.port"), "14268")
    }
    Error(_) => assert_true(false)
  }
  
  // Test multiple snapshots
  let snapshot1 = RollbackManager::create_snapshot(rollback_manager, config_manager)
  ConfigurationManager::set_value(config_manager, "service.port", "9090")
  
  let snapshot2 = RollbackManager::create_snapshot(rollback_manager, config_manager)
  ConfigurationManager::set_value(config_manager, "service.port", "10101")
  
  // Rollback to second snapshot
  RollbackManager::rollback_to_snapshot(rollback_manager, snapshot2)
  assert_eq(config_manager.get_string("service.port"), "9090")
  
  // Rollback to first snapshot
  RollbackManager::rollback_to_snapshot(rollback_manager, snapshot1)
  assert_eq(config_manager.get_string("service.port"), "8080")
  
  // Test automatic rollback on validation failure
  let validation_schema = create_validation_schema()
  ConfigurationManager::enable_validation(config_manager, validation_schema)
  
  // Try to make invalid changes
  let invalid_update = ConfigurationManager::try_set_value(config_manager, "service.port", "99999")
  match invalid_update {
    Success => assert_true(false) // Should fail
    Error(_) => {
      // Configuration should remain unchanged
      assert_eq(config_manager.get_string("service.port"), "8080")
    }
  }
}

// Test 5: Configuration Environment Overrides
test "configuration environment overrides" {
  let config_manager = ConfigurationManager::new()
  let env_manager = EnvironmentOverrideManager::new()
  
  // Load base configuration
  let base_config = create_base_config()
  ConfigurationManager::load(config_manager, base_config)
  
  // Set environment variables
  EnvironmentOverrideManager::set_env_var(env_manager, "AZIMUTH_SERVICE_PORT", "9090")
  EnvironmentOverrideManager::set_env_var(env_manager, "AZIMUTH_SERVICE_ENABLED", "false")
  EnvironmentOverrideManager::set_env_var(env_manager, "AZIMUTH_TELEMETRY_EXPORTER_TYPE", "zipkin")
  
  // Apply environment overrides
  EnvironmentOverrideManager::apply_overrides(env_manager, config_manager)
  
  // Verify overrides were applied
  assert_eq(config_manager.get_string("service.port"), "9090") // Overridden
  assert_eq(config_manager.get_string("service.enabled"), "false") // Overridden
  assert_eq(config_manager.get_string("telemetry.exporter.type"), "zipkin") // Overridden
  
  // Verify non-overridden values remain
  assert_eq(config_manager.get_string("service.name"), "test-service") // Not overridden
  assert_eq(config_manager.get_string("telemetry.exporter.port"), "14268") // Not overridden
  
  // Test environment-specific configurations
  let dev_config = create_environment_specific_config("development")
  let prod_config = create_environment_specific_config("production")
  
  // Load development configuration
  ConfigurationManager::load_environment(config_manager, "development", dev_config)
  assert_eq(config_manager.get_string("service.debug"), "true")
  assert_eq(config_manager.get_string("telemetry.sampling.probability"), "1.0")
  
  // Load production configuration
  ConfigurationManager::load_environment(config_manager, "production", prod_config)
  assert_eq(config_manager.get_string("service.debug"), "false")
  assert_eq(config_manager.get_string("telemetry.sampling.probability"), "0.1")
}

// Test 6: Configuration Hot Reload
test "configuration hot reload" {
  let config_manager = ConfigurationManager::new()
  let hot_reload_manager = HotReloadManager::new()
  
  // Load initial configuration
  let initial_config = create_initial_config()
  ConfigurationManager::load(config_manager, initial_config)
  
  // Enable hot reload for configuration file
  let config_file = "hot_reload_config.json"
  HotReloadManager::watch_file(hot_reload_manager, config_file)
  
  // Register hot reload listener
  let mut reload_count = 0
  HotReloadManager::register_reload_listener(hot_reload_manager, fn(file_path) {
    reload_count = reload_count + 1
    assert_eq(file_path, config_file)
  })
  
  // Simulate file change
  let updated_config = create_updated_config()
  HotReloadManager::simulate_file_change(hot_reload_manager, config_file, updated_config)
  
  // Verify hot reload was triggered
  assert_eq(reload_count, 1)
  
  // Verify configuration was updated
  assert_eq(config_manager.get_string("service.port"), "9090")
  assert_eq(config_manager.get_string("service.timeout"), "60")
  
  // Test hot reload with validation
  let validation_schema = create_validation_schema()
  HotReloadManager::enable_validation(hot_reload_manager, validation_schema)
  
  // Valid configuration change should succeed
  let valid_config_change = create_valid_config_change()
  HotReloadManager::simulate_file_change(hot_reload_manager, config_file, valid_config_change)
  assert_eq(config_manager.get_string("service.port"), "8081")
  
  // Invalid configuration change should be rejected
  let invalid_config_change = create_invalid_config_change()
  HotReloadManager::simulate_file_change(hot_reload_manager, config_file, invalid_config_change)
  assert_eq(config_manager.get_string("service.port"), "8081") // Should remain unchanged
  
  // Test hot reload with automatic rollback
  HotReloadManager::enable_auto_rollback(hot_reload_manager)
  
  // Configuration change that causes system failure should trigger rollback
  let failing_config = create_failing_config()
  HotReloadManager::simulate_file_change(hot_reload_manager, config_file, failing_config)
  
  // Should rollback to last known good configuration
  assert_eq(config_manager.get_string("service.port"), "8081")
}

// Test 7: Configuration Encryption and Security
test "configuration encryption and security" {
  let config_manager = ConfigurationManager::new()
  let security_manager = ConfigurationSecurityManager::new()
  
  // Load configuration with sensitive data
  let config_with_secrets = create_config_with_secrets()
  ConfigurationManager::load(config_manager, config_with_secrets)
  
  // Identify sensitive fields
  let sensitive_fields = ["database.password", "api.key", "service.secret"]
  
  // Encrypt sensitive fields
  for field in sensitive_fields {
    let value = config_manager.get_string(field)
    let encrypted_value = ConfigurationSecurityManager::encrypt(security_manager, value)
    ConfigurationManager::set_encrypted_value(config_manager, field, encrypted_value)
  }
  
  // Verify encrypted values are not in plain text
  for field in sensitive_fields {
    let encrypted_value = ConfigurationManager::get_encrypted_value(config_manager, field)
    assert_ne(encrypted_value, config_manager.get_string(field))
  }
  
  // Test decryption
  for field in sensitive_fields {
    let encrypted_value = ConfigurationManager::get_encrypted_value(config_manager, field)
    let decrypted_value = ConfigurationSecurityManager::decrypt(security_manager, encrypted_value)
    assert_eq(decrypted_value, config_manager.get_string(field))
  }
  
  // Test configuration access control
  let access_manager = ConfigurationAccessManager::new()
  
  // Define access policies
  AccessManager::add_policy(access_manager, "admin", ["*"]) // Admin can access all
  AccessManager::add_policy(access_manager, "operator", ["service.*", "telemetry.*"]) // Operator can access service and telemetry
  AccessManager::add_policy(access_manager, "viewer", ["service.name", "service.port"]) // Viewer can only read service name and port
  
  // Test access control
  assert_true(AccessManager::can_access(access_manager, "admin", "database.password"))
  assert_true(AccessManager::can_access(access_manager, "operator", "service.port"))
  assert_false(AccessManager::can_access(access_manager, "viewer", "database.password"))
  assert_true(AccessManager::can_access(access_manager, "viewer", "service.name"))
  
  // Test configuration audit logging
  let audit_logger = ConfigurationAuditLogger::new()
  ConfigurationManager::enable_audit_logging(config_manager, audit_logger)
  
  // Make configuration changes
  ConfigurationManager::set_value(config_manager, "service.port", "9090")
  ConfigurationManager::set_value(config_manager, "database.password", "new_password")
  
  // Verify audit log entries
  let audit_log = ConfigurationManager::get_audit_log(config_manager)
  assert_true(audit_log.length() >= 2)
  
  // Verify sensitive changes are masked in audit log
  let password_change_entry = audit_log.find(fn(entry) { entry.key == "database.password" })
  match password_change_entry {
    Some(entry) => assert_eq(entry.new_value, "***MASKED***")
    None => assert_true(false)
  }
}

// Test 8: Configuration Templates and Inheritance
test "configuration templates and inheritance" {
  let config_manager = ConfigurationManager::new()
  let template_manager = ConfigurationTemplateManager::new()
  
  // Create base template
  let base_template = create_base_template()
  TemplateManager::register_template(template_manager, "base", base_template)
  
  // Create service-specific templates
  let web_service_template = create_web_service_template()
  TemplateManager::register_template(template_manager, "web-service", web_service_template)
  
  let database_service_template = create_database_service_template()
  TemplateManager::register_template(template_manager, "database-service", database_service_template)
  
  // Test template inheritance
  let web_service_config = TemplateManager::create_from_template(template_manager, "web-service")
  assert_eq(web_service_config.get_string("service.type"), "web")
  assert_eq(web_service_config.get_string("service.port"), "8080") // From base template
  assert_eq(web_service_config.get_string("web.server"), "nginx") // From web service template
  
  let database_service_config = TemplateManager::create_from_template(template_manager, "database-service")
  assert_eq(database_service_config.get_string("service.type"), "database")
  assert_eq(database_service_config.get_string("service.port"), "5432") // Overridden in database template
  assert_eq(database_service_config.get_string("database.engine"), "postgresql") // From database template
  
  // Test template composition
  let composite_template = create_composite_template()
  TemplateManager::register_template(template_manager, "composite", composite_template)
  
  let composite_config = TemplateManager::create_from_template(template_manager, "composite")
  assert_eq(composite_config.get_string("service.type"), "composite")
  assert_eq(composite_config.get_string("web.server"), "nginx") // From web service template
  assert_eq(composite_config.get_string("database.engine"), "postgresql") // From database service template
  
  // Test template parameterization
  let parameterized_template = create_parameterized_template()
  TemplateManager::register_template(template_manager, "parameterized", parameterized_template)
  
  let parameters = [
    ("service.name", "custom-service"),
    ("service.port", "9999"),
    ("environment", "production")
  ]
  
  let parameterized_config = TemplateManager::create_from_template_with_params(
    template_manager, "parameterized", parameters
  )
  
  assert_eq(parameterized_config.get_string("service.name"), "custom-service")
  assert_eq(parameterized_config.get_string("service.port"), "9999")
  assert_eq(parameterized_config.get_string("service.environment"), "production")
}

// Test 9: Configuration Performance and Scalability
test "configuration performance and scalability" {
  let config_manager = ConfigurationManager::new()
  
  // Create large configuration
  let large_config = create_large_configuration(1000) // 1000 configuration keys
  ConfigurationManager::load(config_manager, large_config)
  
  // Test configuration lookup performance
  let start_time = get_current_time_millis()
  
  for i in 0..10000 {
    let key = "config.key." + (i % 1000).to_string()
    config_manager.get_string(key)
  }
  
  let lookup_time = get_current_time_millis() - start_time
  
  // Should perform 10000 lookups within reasonable time
  assert_true(lookup_time < 1000) // Within 1 second
  
  // Test configuration update performance
  let start_time = get_current_time_millis()
  
  for i in 0..1000 {
    let key = "config.key." + i.to_string()
    let value = "updated.value." + i.to_string()
    ConfigurationManager::set_value(config_manager, key, value)
  }
  
  let update_time = get_current_time_millis() - start_time
  
  // Should perform 1000 updates within reasonable time
  assert_true(update_time < 500) // Within 0.5 seconds
  
  // Test configuration serialization performance
  let start_time = get_current_time_millis()
  
  for i in 0..100 {
    let serialized = ConfigurationManager::serialize(config_manager)
    ConfigurationManager::deserialize(config_manager, serialized)
  }
  
  let serialization_time = get_current_time_millis() - start_time
  
  // Should perform 100 serialization/deserialization cycles within reasonable time
  assert_true(serialization_time < 2000) // Within 2 seconds
  
  // Test memory usage
  let initial_memory = get_memory_usage()
  
  // Create multiple configuration instances
  let mut config_instances = []
  for i in 0..100 {
    let instance = ConfigurationManager::new()
    let config = create_medium_configuration(100)
    ConfigurationManager::load(instance, config)
    config_instances = config_instances + [instance]
  }
  
  let peak_memory = get_memory_usage()
  
  // Clean up
  for instance in config_instances {
    ConfigurationManager::destroy(instance)
  }
  
  let final_memory = get_memory_usage()
  
  // Verify memory is properly cleaned up
  let memory_leak = final_memory - initial_memory
  assert_true(memory_leak < 1024 * 1024) // Less than 1MB leak
}

// Test 10: Configuration Integration with Telemetry System
test "configuration integration with telemetry system" {
  let telemetry_system = TelemetrySystem::new()
  let config_manager = ConfigurationManager::new()
  
  // Load telemetry configuration
  let telemetry_config = create_telemetry_configuration()
  ConfigurationManager::load(config_manager, telemetry_config)
  
  // Configure telemetry system based on configuration
  TelemetrySystem::configure_from(telemetry_system, config_manager)
  
  // Verify telemetry system is configured correctly
  assert_eq(TelemetrySystem::get_service_name(telemetry_system), "test-service")
  assert_eq(TelemetrySystem::get_service_version(telemetry_system), "1.0.0")
  assert_true(TelemetrySystem::is_enabled(telemetry_system))
  
  // Test dynamic reconfiguration
  ConfigurationManager::set_value(config_manager, "telemetry.sampling.probability", "0.5")
  TelemetrySystem::apply_configuration_changes(telemetry_system, config_manager)
  
  assert_eq(TelemetrySystem::get_sampling_probability(telemetry_system), 0.5)
  
  // Test configuration-driven component behavior
  let tracer = TelemetrySystem::get_tracer(telemetry_system)
  let meter = TelemetrySystem::get_meter(telemetry_system)
  let logger = TelemetrySystem::get_logger(telemetry_system)
  
  // Verify components are configured according to configuration
  assert_eq(Tracer::get_exporter_type(tracer), "jaeger")
  assert_eq(Meter::get_exporter_type(meter), "prometheus")
  assert_eq(Logger::get_exporter_type(logger), "elasticsearch")
  
  // Test configuration validation integration
  let validation_schema = create_telemetry_validation_schema()
  ConfigurationManager::enable_validation(config_manager, validation_schema)
  
  // Valid configuration changes should be applied
  ConfigurationManager::set_value(config_manager, "telemetry.sampling.probability", "0.8")
  TelemetrySystem::apply_configuration_changes(telemetry_system, config_manager)
  assert_eq(TelemetrySystem::get_sampling_probability(telemetry_system), 0.8)
  
  // Invalid configuration changes should be rejected
  let invalid_change = ConfigurationManager::try_set_value(config_manager, "telemetry.sampling.probability", "1.5")
  match invalid_change {
    Success => assert_true(false) // Should fail
    Error(_) => {
      assert_eq(TelemetrySystem::get_sampling_probability(telemetry_system), 0.8) // Should remain unchanged
    }
  }
  
  // Test configuration change notifications
  let mut notification_count = 0
  TelemetrySystem::register_configuration_listener(telemetry_system, fn(key, old_value, new_value) {
    notification_count = notification_count + 1
  })
  
  ConfigurationManager::set_value(config_manager, "telemetry.batch.size", "1000")
  TelemetrySystem::apply_configuration_changes(telemetry_system, config_manager)
  
  assert_eq(notification_count, 1) // Should receive notification
  assert_eq(TelemetrySystem::get_batch_size(telemetry_system), 1000)
}

// Helper functions for creating test data
fn create_test_config_content() -> String {
  "{
    \"service\": {
      \"name\": \"test-service\",
      \"port\": 8080,
      \"enabled\": true,
      \"timeout\": 30,
      \"retries\": 3,
      \"debug\": false,
      \"endpoints\": [
        \"http://localhost:8080\",
        \"http://localhost:8081\",
        \"http://localhost:8082\"
      ],
      \"threshold\": 0.95
    },
    \"telemetry\": {
      \"exporter\": {
        \"type\": \"jaeger\",
        \"port\": 14268,
        \"endpoint\": \"http://localhost:14268/api/traces\"
      },
      \"batch\": {
        \"size\": 512,
        \"timeout\": 5000
      },
      \"sampling\": {
        \"probability\": 1.0
      }
    }
  }"
}

fn create_invalid_config_content() -> String {
  "{
    \"service\": {
      \"name\": \"test-service\",
      \"port\": \"invalid_port\",
      \"enabled\": \"not_a_boolean\"
    }
  }"
}

fn create_validation_schema() -> ValidationSchema {
  let schema = ValidationSchema::new()
  ValidationSchema::add_required_field(schema, "service.name", "string")
  ValidationSchema::add_required_field(schema, "service.port", "integer")
  ValidationSchema::add_required_field(schema, "service.enabled", "boolean")
  ValidationSchema::add_constraint(schema, "service.port", "min", "1")
  ValidationSchema::add_constraint(schema, "service.port", "max", "65535")
  schema
}

fn create_valid_config_with_schema() -> Configuration {
  let config = Configuration::new()
  Configuration::set_value(config, "service.name", "test-service")
  Configuration::set_value(config, "service.port", "8080")
  Configuration::set_value(config, "service.enabled", "true")
  Configuration::set_value(config, "service.timeout", "30")
  config
}

fn create_config_missing_required_field() -> Configuration {
  let config = Configuration::new()
  Configuration::set_value(config, "service.name", "test-service")
  // Missing service.port and service.enabled
  config
}

fn create_config_with_constraint_violation() -> Configuration {
  let config = Configuration::new()
  Configuration::set_value(config, "service.name", "test-service")
  Configuration::set_value(config, "service.port", "99999") // Out of range
  Configuration::set_value(config, "service.enabled", "true")
  config
}

fn create_initial_config() -> Configuration {
  let config = Configuration::new()
  Configuration::set_value(config, "service.name", "test-service")
  Configuration::set_value(config, "service.port", "8080")
  Configuration::set_value(config, "service.timeout", "30")
  Configuration::set_value(config, "telemetry.exporter.port", "14268")
  config
}

fn create_base_config() -> Configuration {
  let config = Configuration::new()
  Configuration::set_value(config, "service.name", "test-service")
  Configuration::set_value(config, "service.port", "8080")
  Configuration::set_value(config, "service.enabled", "true")
  Configuration::set_value(config, "telemetry.exporter.type", "jaeger")
  Configuration::set_value(config, "telemetry.exporter.port", "14268")
  config
}

fn create_environment_specific_config(env : String) -> Configuration {
  let config = Configuration::new()
  
  match env {
    "development" => {
      Configuration::set_value(config, "service.debug", "true")
      Configuration::set_value(config, "telemetry.sampling.probability", "1.0")
      Configuration::set_value(config, "logging.level", "debug")
    }
    "production" => {
      Configuration::set_value(config, "service.debug", "false")
      Configuration::set_value(config, "telemetry.sampling.probability", "0.1")
      Configuration::set_value(config, "logging.level", "info")
    }
    _ => {
      Configuration::set_value(config, "service.debug", "false")
      Configuration::set_value(config, "telemetry.sampling.probability", "0.5")
      Configuration::set_value(config, "logging.level", "warn")
    }
  }
  
  config
}

fn create_updated_config() -> String {
  "{
    \"service\": {
      \"name\": \"test-service\",
      \"port\": 9090,
      \"timeout\": 60
    },
    \"telemetry\": {
      \"exporter\": {
        \"port\": 14269
      }
    }
  }"
}

fn create_valid_config_change() -> String {
  "{
    \"service\": {
      \"name\": \"test-service\",
      \"port\": 8081
    }
  }"
}

fn create_invalid_config_change() -> String {
  "{
    \"service\": {
      \"name\": \"test-service\",
      \"port\": 99999
    }
  }"
}

fn create_failing_config() -> String {
  "{
    \"service\": {
      \"name\": \"test-service\",
      \"port\": \"invalid\"
    }
  }"
}

fn create_config_with_secrets() -> Configuration {
  let config = Configuration::new()
  Configuration::set_value(config, "database.password", "secret_password")
  Configuration::set_value(config, "api.key", "secret_api_key")
  Configuration::set_value(config, "service.secret", "secret_service_key")
  Configuration::set_value(config, "service.name", "test-service")
  config
}

fn create_base_template() -> ConfigurationTemplate {
  let template = ConfigurationTemplate::new()
  Template::set_value(template, "service.port", "8080")
  Template::set_value(template, "service.timeout", "30")
  Template::set_value(template, "telemetry.enabled", "true")
  template
}

fn create_web_service_template() -> ConfigurationTemplate {
  let template = ConfigurationTemplate::new()
  Template::set_parent(template, "base")
  Template::set_value(template, "service.type", "web")
  Template::set_value(template, "web.server", "nginx")
  Template::set_value(template, "web.ssl.enabled", "true")
  template
}

fn create_database_service_template() -> ConfigurationTemplate {
  let template = ConfigurationTemplate::new()
  Template::set_parent(template, "base")
  Template::set_value(template, "service.type", "database")
  Template::set_value(template, "service.port", "5432")
  Template::set_value(template, "database.engine", "postgresql")
  template
}

fn create_composite_template() -> ConfigurationTemplate {
  let template = ConfigurationTemplate::new()
  Template::set_parent(template, "web-service")
  Template::set_value(template, "service.type", "composite")
  Template::set_value(template, "database.engine", "postgresql")
  template
}

fn create_parameterized_template() -> ConfigurationTemplate {
  let template = ConfigurationTemplate::new()
  Template::set_value(template, "service.name", "${service.name}")
  Template::set_value(template, "service.port", "${service.port}")
  Template::set_value(template, "service.environment", "${environment}")
  Template::set_value(template, "service.url", "http://${service.name}:${service.port}/${environment}")
  template
}

fn create_large_configuration(key_count : Int) -> Configuration {
  let config = Configuration::new()
  
  for i in 0..key_count {
    let key = "config.key." + i.to_string()
    let value = "value." + i.to_string()
    Configuration::set_value(config, key, value)
  }
  
  config
}

fn create_medium_configuration(key_count : Int) -> Configuration {
  create_large_configuration(key_count)
}

fn create_telemetry_configuration() -> Configuration {
  let config = Configuration::new()
  Configuration::set_value(config, "service.name", "test-service")
  Configuration::set_value(config, "service.version", "1.0.0")
  Configuration::set_value(config, "telemetry.enabled", "true")
  Configuration::set_value(config, "telemetry.exporter.type", "jaeger")
  Configuration::set_value(config, "telemetry.exporter.endpoint", "http://localhost:14268/api/traces")
  Configuration::set_value(config, "telemetry.sampling.probability", "1.0")
  Configuration::set_value(config, "telemetry.batch.size", "512")
  Configuration::set_value(config, "telemetry.logging.enabled", "true")
  Configuration::set_value(config, "telemetry.logging.level", "info")
  config
}

fn create_telemetry_validation_schema() -> ValidationSchema {
  let schema = ValidationSchema::new()
  ValidationSchema::add_required_field(schema, "service.name", "string")
  ValidationSchema::add_required_field(schema, "service.version", "string")
  ValidationSchema::add_required_field(schema, "telemetry.enabled", "boolean")
  ValidationSchema::add_constraint(schema, "telemetry.sampling.probability", "min", "0.0")
  ValidationSchema::add_constraint(schema, "telemetry.sampling.probability", "max", "1.0")
  ValidationSchema::add_constraint(schema, "telemetry.batch.size", "min", "1")
  ValidationSchema::add_constraint(schema, "telemetry.batch.size", "max", "10000")
  schema
}

// Helper function to get current time in milliseconds
fn get_current_time_millis() -> Int {
  // Mock implementation - in real code would use system time
  1609459200000 + Random::int(0, 86400000) // Random time within a day
}

// Helper function to get memory usage
fn get_memory_usage() -> Int {
  // Mock implementation - in real code would use system memory API
  Random::int(1000000, 10000000) // Random memory usage between 1MB and 10MB
}