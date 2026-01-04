// Dynamic Configuration Update Tests for Azimuth Telemetry System
// This file contains tests for dynamic configuration management and updates

test "basic configuration loading and retrieval" {
  let config_manager = ConfigurationManager::new()
  
  // Load initial configuration
  let initial_config = Configuration::with([
    ("telemetry.enabled", BoolValue(true)),
    ("telemetry.sampling.rate", FloatValue(0.1)),
    ("telemetry.batch.size", IntValue(100)),
    ("telemetry.export.interval", IntValue(5000)),
    ("service.name", StringValue("test-service"))
  ])
  
  ConfigurationManager::load(config_manager, initial_config)
  
  // Verify configuration values
  let telemetry_enabled = ConfigurationManager::get(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get(config_manager, "telemetry.sampling.rate")
  match sampling_rate {
    Some(FloatValue(v)) => assert_eq(v, 0.1)
    _ => assert_true(false)
  }
  
  let batch_size = ConfigurationManager::get(config_manager, "telemetry.batch.size")
  match batch_size {
    Some(IntValue(v)) => assert_eq(v, 100)
    _ => assert_true(false)
  }
  
  let service_name = ConfigurationManager::get(config_manager, "service.name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "test-service")
    _ => assert_true(false)
  }
}

test "dynamic configuration update notifications" {
  let config_manager = ConfigurationManager::new()
  let notification_handler = MockNotificationHandler::new()
  
  // Register notification handler
  ConfigurationManager::register_handler(config_manager, "telemetry.sampling.rate", notification_handler)
  
  // Load initial configuration
  let initial_config = Configuration::with([
    ("telemetry.sampling.rate", FloatValue(0.1))
  ])
  
  ConfigurationManager::load(config_manager, initial_config)
  
  // Update configuration value
  ConfigurationManager::update(config_manager, "telemetry.sampling.rate", FloatValue(0.5))
  
  // Verify notification was sent
  assert_true(MockNotificationHandler::was_notified(notification_handler))
  assert_eq(MockNotificationHandler::get_old_value(notification_handler), Some(FloatValue(0.1)))
  assert_eq(MockNotificationHandler::get_new_value(notification_handler), Some(FloatValue(0.5)))
}

test "configuration validation on update" {
  let config_manager = ConfigurationManager::new()
  let validator = ConfigurationValidator::new()
  
  // Register validation rules
  ConfigurationValidator::add_rule(validator, "telemetry.sampling.rate", fn(value) {
    match value {
      FloatValue(v) => v >= 0.0 && v <= 1.0  // Sampling rate must be between 0 and 1
      _ => false
    }
  })
  
  ConfigurationValidator::add_rule(validator, "telemetry.batch.size", fn(value) {
    match value {
      IntValue(v) => v > 0 && v <= 10000  // Batch size must be positive and <= 10000
      _ => false
    }
  })
  
  ConfigurationManager::set_validator(config_manager, validator)
  
  // Load initial configuration
  let initial_config = Configuration::with([
    ("telemetry.sampling.rate", FloatValue(0.1)),
    ("telemetry.batch.size", IntValue(100))
  ])
  
  ConfigurationManager::load(config_manager, initial_config)
  
  // Test valid updates
  let valid_sampling_update = ConfigurationManager::update(config_manager, "telemetry.sampling.rate", FloatValue(0.5))
  assert_true(valid_sampling_update)
  
  let valid_batch_update = ConfigurationManager::update(config_manager, "telemetry.batch.size", IntValue(500))
  assert_true(valid_batch_update)
  
  // Test invalid updates
  let invalid_sampling_update = ConfigurationManager::update(config_manager, "telemetry.sampling.rate", FloatValue(1.5))
  assert_false(invalid_sampling_update)
  
  let invalid_batch_update = ConfigurationManager::update(config_manager, "telemetry.batch.size", IntValue(-10))
  assert_false(invalid_batch_update)
  
  // Verify configuration values remain unchanged after invalid updates
  let sampling_rate = ConfigurationManager::get(config_manager, "telemetry.sampling.rate")
  match sampling_rate {
    Some(FloatValue(v)) => assert_eq(v, 0.5)  // Should be last valid value
    _ => assert_true(false)
  }
}

test "configuration persistence and reload" {
  let config_manager = ConfigurationManager::new()
  let temp_config_file = "/tmp/azimuth-test-config.json"
  
  // Load initial configuration
  let initial_config = Configuration::with([
    ("telemetry.enabled", BoolValue(true)),
    ("telemetry.sampling.rate", FloatValue(0.1)),
    ("service.name", StringValue("test-service"))
  ])
  
  ConfigurationManager::load(config_manager, initial_config)
  
  // Persist configuration to file
  let persist_result = ConfigurationManager::persist_to_file(config_manager, temp_config_file)
  assert_true(persist_result)
  
  // Create new configuration manager and reload from file
  let new_config_manager = ConfigurationManager::new()
  let reload_result = ConfigurationManager::load_from_file(new_config_manager, temp_config_file)
  assert_true(reload_result)
  
  // Verify reloaded configuration values
  let telemetry_enabled = ConfigurationManager::get(new_config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get(new_config_manager, "telemetry.sampling.rate")
  match sampling_rate {
    Some(FloatValue(v)) => assert_eq(v, 0.1)
    _ => assert_true(false)
  }
  
  let service_name = ConfigurationManager::get(new_config_manager, "service.name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "test-service")
    _ => assert_true(false)
  }
}

test "configuration environment variable overrides" {
  let config_manager = ConfigurationManager::new()
  
  // Set environment variables
  Environment::set("AZIMUTH_TELEMETRY_ENABLED", "false")
  Environment::set("AZIMUTH_TELEMETRY_SAMPLING_RATE", "0.25")
  Environment::set("AZIMUTH_SERVICE_NAME", "env-override-service")
  
  // Load base configuration
  let base_config = Configuration::with([
    ("telemetry.enabled", BoolValue(true)),
    ("telemetry.sampling.rate", FloatValue(0.1)),
    ("service.name", StringValue("base-service"))
  ])
  
  ConfigurationManager::load(config_manager, base_config)
  
  // Apply environment variable overrides
  ConfigurationManager::apply_env_overrides(config_manager, "AZIMUTH_")
  
  // Verify environment variable overrides
  let telemetry_enabled = ConfigurationManager::get(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(BoolValue(v)) => assert_false(v)  // Should be overridden by env var
    _ => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get(config_manager, "telemetry.sampling.rate")
  match sampling_rate {
    Some(FloatValue(v)) => assert_eq(v, 0.25)  // Should be overridden by env var
    _ => assert_true(false)
  }
  
  let service_name = ConfigurationManager::get(config_manager, "service.name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "env-override-service")  // Should be overridden by env var
    _ => assert_true(false)
  }
}

test "configuration hot reload for telemetry components" {
  let config_manager = ConfigurationManager::new()
  let telemetry_provider = TelemetryProvider::new()
  
  // Connect telemetry provider to configuration manager
  TelemetryProvider::connect_to_config(telemetry_provider, config_manager)
  
  // Load initial configuration
  let initial_config = Configuration::with([
    ("telemetry.sampling.rate", FloatValue(0.1)),
    ("telemetry.batch.size", IntValue(100)),
    ("telemetry.export.interval", IntValue(5000))
  ])
  
  ConfigurationManager::load(config_manager, initial_config)
  
  // Verify initial telemetry configuration
  assert_eq(TelemetryProvider::get_sampling_rate(telemetry_provider), 0.1)
  assert_eq(TelemetryProvider::get_batch_size(telemetry_provider), 100)
  assert_eq(TelemetryProvider::get_export_interval(telemetry_provider), 5000)
  
  // Update configuration
  ConfigurationManager::update(config_manager, "telemetry.sampling.rate", FloatValue(0.5))
  ConfigurationManager::update(config_manager, "telemetry.batch.size", IntValue(500))
  ConfigurationManager::update(config_manager, "telemetry.export.interval", IntValue(10000))
  
  // Verify telemetry provider was updated
  assert_eq(TelemetryProvider::get_sampling_rate(telemetry_provider), 0.5)
  assert_eq(TelemetryProvider::get_batch_size(telemetry_provider), 500)
  assert_eq(TelemetryProvider::get_export_interval(telemetry_provider), 10000)
}

test "configuration change transaction and rollback" {
  let config_manager = ConfigurationManager::new()
  
  // Load initial configuration
  let initial_config = Configuration::with([
    ("telemetry.enabled", BoolValue(true)),
    ("telemetry.sampling.rate", FloatValue(0.1)),
    ("telemetry.batch.size", IntValue(100))
  ])
  
  ConfigurationManager::load(config_manager, initial_config)
  
  // Start configuration transaction
  let transaction = ConfigurationManager::begin_transaction(config_manager)
  
  // Make multiple changes within transaction
  ConfigurationManager::update_in_transaction(transaction, "telemetry.enabled", BoolValue(false))
  ConfigurationManager::update_in_transaction(transaction, "telemetry.sampling.rate", FloatValue(0.5))
  ConfigurationManager::update_in_transaction(transaction, "telemetry.batch.size", IntValue(500))
  
  // Verify changes are not yet applied
  let telemetry_enabled = ConfigurationManager::get(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(BoolValue(v)) => assert_true(v)  // Should still be original value
    _ => assert_true(false)
  }
  
  // Commit transaction
  let commit_result = ConfigurationManager::commit_transaction(transaction)
  assert_true(commit_result)
  
  // Verify changes are now applied
  let telemetry_enabled_after = ConfigurationManager::get(config_manager, "telemetry.enabled")
  match telemetry_enabled_after {
    Some(BoolValue(v)) => assert_false(v)  // Should be updated
    _ => assert_true(false)
  }
  
  // Test rollback
  let rollback_transaction = ConfigurationManager::begin_transaction(config_manager)
  ConfigurationManager::update_in_transaction(rollback_transaction, "telemetry.sampling.rate", FloatValue(0.9))
  ConfigurationManager::rollback_transaction(rollback_transaction)
  
  // Verify rollback worked
  let sampling_rate_after_rollback = ConfigurationManager::get(config_manager, "telemetry.sampling.rate")
  match sampling_rate_after_rollback {
    Some(FloatValue(v)) => assert_eq(v, 0.5)  // Should be value before rollback transaction
    _ => assert_true(false)
  }
}

test "configuration inheritance and overrides" {
  let config_manager = ConfigurationManager::new()
  
  // Load base configuration
  let base_config = Configuration::with([
    ("telemetry.enabled", BoolValue(true)),
    ("telemetry.sampling.rate", FloatValue(0.1)),
    ("telemetry.batch.size", IntValue(100)),
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  ConfigurationManager::load(config_manager, base_config)
  
  // Load environment-specific configuration
  let env_config = Configuration::with([
    ("telemetry.sampling.rate", FloatValue(0.5)),  // Override base
    ("telemetry.batch.size", IntValue(500)),       // Override base
    ("service.environment", StringValue("production"))  // New value
  ])
  
  ConfigurationManager::load_with_overrides(config_manager, env_config)
  
  // Verify configuration values
  let telemetry_enabled = ConfigurationManager::get(config_manager, "telemetry.enabled")
  match telemetry_enabled {
    Some(BoolValue(v)) => assert_true(v)  // Should be from base config
    _ => assert_true(false)
  }
  
  let sampling_rate = ConfigurationManager::get(config_manager, "telemetry.sampling.rate")
  match sampling_rate {
    Some(FloatValue(v)) => assert_eq(v, 0.5)  // Should be overridden by env config
    _ => assert_true(false)
  }
  
  let batch_size = ConfigurationManager::get(config_manager, "telemetry.batch.size")
  match batch_size {
    Some(IntValue(v)) => assert_eq(v, 500)  // Should be overridden by env config
    _ => assert_true(false)
  }
  
  let service_name = ConfigurationManager::get(config_manager, "service.name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "base-service")  // Should be from base config
    _ => assert_true(false)
  }
  
  let service_environment = ConfigurationManager::get(config_manager, "service.environment")
  match service_environment {
    Some(StringValue(v)) => assert_eq(v, "production")  // Should be from env config
    _ => assert_true(false)
  }
}

test "configuration schema validation" {
  let config_manager = ConfigurationManager::new()
  let schema = ConfigurationSchema::new()
  
  // Define configuration schema
  ConfigurationSchema::add_field(schema, "telemetry.enabled", BoolField, true, None)  // Required, no default
  ConfigurationSchema::add_field(schema, "telemetry.sampling.rate", FloatField, true, Some(FloatValue(0.1)))  // Required with default
  ConfigurationSchema::add_field(schema, "telemetry.batch.size", IntField, false, Some(IntValue(100)))  // Optional with default
  ConfigurationSchema::add_field(schema, "service.name", StringField, true, None)  // Required, no default
  
  ConfigurationManager::set_schema(config_manager, schema)
  
  // Test valid configuration
  let valid_config = Configuration::with([
    ("telemetry.enabled", BoolValue(true)),
    ("telemetry.sampling.rate", FloatValue(0.5)),
    ("service.name", StringValue("test-service"))
  ])
  
  let valid_result = ConfigurationManager::validate_and_load(config_manager, valid_config)
  assert_true(valid_result)
  
  // Test missing required field
  let missing_required_config = Configuration::with([
    ("telemetry.sampling.rate", FloatValue(0.5))
    // Missing telemetry.enabled and service.name
  ])
  
  let missing_required_result = ConfigurationManager::validate_and_load(config_manager, missing_required_config)
  assert_false(missing_required_result)
  
  // Test configuration with defaults applied
  let config_with_defaults = Configuration::with([
    ("telemetry.enabled", BoolValue(true)),
    ("service.name", StringValue("test-service"))
    // Missing telemetry.sampling.rate, should use default
  ])
  
  let defaults_result = ConfigurationManager::validate_and_load(config_manager, config_with_defaults)
  assert_true(defaults_result)
  
  // Verify default was applied
  let sampling_rate = ConfigurationManager::get(config_manager, "telemetry.sampling.rate")
  match sampling_rate {
    Some(FloatValue(v)) => assert_eq(v, 0.1)  // Should be default value
    _ => assert_true(false)
  }
}

test "configuration change history and audit" {
  let config_manager = ConfigurationManager::new()
  let audit_log = ConfigurationAuditLog::new()
  
  // Connect audit log to configuration manager
  ConfigurationManager::set_audit_log(config_manager, audit_log)
  
  // Load initial configuration
  let initial_config = Configuration::with([
    ("telemetry.enabled", BoolValue(true)),
    ("telemetry.sampling.rate", FloatValue(0.1))
  ])
  
  ConfigurationManager::load(config_manager, initial_config)
  
  // Make configuration changes
  ConfigurationManager::update(config_manager, "telemetry.sampling.rate", FloatValue(0.5))
  ConfigurationManager::update(config_manager, "telemetry.sampling.rate", FloatValue(0.25))
  ConfigurationManager::update(config_manager, "telemetry.enabled", BoolValue(false))
  
  // Verify audit log entries
  let audit_entries = ConfigurationAuditLog::get_entries(audit_log)
  assert_eq(audit_entries.length(), 5)  // 1 initial load + 3 updates + 1 final state
  
  // Verify specific audit entry
  let first_update = audit_entries[1]
  assert_eq(ConfigurationAuditEntry::get_key(first_update), "telemetry.sampling.rate")
  assert_eq(ConfigurationAuditEntry::get_old_value(first_update), Some(FloatValue(0.1)))
  assert_eq(ConfigurationAuditEntry::get_new_value(first_update), Some(FloatValue(0.5)))
  
  // Verify configuration history
  let history = ConfigurationManager::get_history(config_manager, "telemetry.sampling.rate")
  assert_eq(history.length(), 3)  // Initial + 2 updates
  
  assert_eq(history[0], Some(FloatValue(0.1)))  // Initial value
  assert_eq(history[1], Some(FloatValue(0.5)))  // First update
  assert_eq(history[2], Some(FloatValue(0.25))) // Second update
}