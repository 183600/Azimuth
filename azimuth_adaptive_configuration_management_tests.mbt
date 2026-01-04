// Adaptive Configuration Management Tests for Azimuth Telemetry System
// This file contains test cases for adaptive configuration management capabilities

// Test 1: Dynamic Configuration Updates
test "dynamic configuration updates" {
  let config_manager = AdaptiveConfigManager::new()
  
  // Initialize configuration manager
  config_manager.initialize(ConfigInitOptions::new()
    .with_config_file_path("/etc/azimuth/config.json")
    .with_auto_reload_enabled(true)
    .with_validation_enabled(true)
    .with_backup_enabled(true))
  
  // Start configuration manager
  config_manager.start()
  
  // Get initial configuration
  let initial_config = config_manager.get_configuration()
  assert_true(initial_config.contains_key("telemetry"))
  assert_true(initial_config.contains_key("sampling"))
  assert_true(initial_config.contains_key("storage"))
  assert_true(initial_config.contains_key("network"))
  
  // Update telemetry configuration
  let telemetry_updates = {
    "collection_interval": 500,
    "batch_size": 200,
    "compression_enabled": true,
    "retention_days": 30
  }
  
  let telemetry_update_result = config_manager.update_configuration("telemetry", telemetry_updates)
  assert_true(telemetry_update_result.success)
  assert_true(telemetry_update_result.updated_keys.length() == 4)
  assert_true(telemetry_update_result.backup_created)
  
  // Verify telemetry configuration update
  let updated_config = config_manager.get_configuration()
  assert_eq(updated_config.get("telemetry.collection_interval"), Some(500))
  assert_eq(updated_config.get("telemetry.batch_size"), Some(200))
  assert_eq(updated_config.get("telemetry.compression_enabled"), Some(true))
  assert_eq(updated_config.get("telemetry.retention_days"), Some(30))
  
  // Update sampling configuration
  let sampling_updates = {
    "strategy": "adaptive",
    "rate": 0.1,
    "max_samples_per_second": 1000,
    "error_sampling_rate": 1.0
  }
  
  let sampling_update_result = config_manager.update_configuration("sampling", sampling_updates)
  assert_true(sampling_update_result.success)
  assert_true(sampling_update_result.updated_keys.length() == 4)
  
  // Verify sampling configuration update
  let updated_sampling_config = config_manager.get_configuration()
  assert_eq(updated_sampling_config.get("sampling.strategy"), Some("adaptive"))
  assert_eq(updated_sampling_config.get("sampling.rate"), Some(0.1))
  assert_eq(updated_sampling_config.get("sampling.max_samples_per_second"), Some(1000))
  assert_eq(updated_sampling_config.get("sampling.error_sampling_rate"), Some(1.0))
  
  // Test configuration rollback
  let rollback_result = config_manager.rollback_configuration("telemetry")
  assert_true(rollback_result.success)
  assert_true(rollback_result.rollback_successful)
  assert_true(rollback_result.backup_used.length() > 0)
  
  // Verify telemetry configuration rollback
  let rolled_back_config = config_manager.get_configuration()
  assert_eq(rolled_back_config.get("telemetry.collection_interval"), initial_config.get("telemetry.collection_interval"))
  assert_eq(rolled_back_config.get("telemetry.batch_size"), initial_config.get("telemetry.batch_size"))
  
  // Test configuration validation
  let invalid_updates = {
    "collection_interval": -100,  // Invalid: negative value
    "batch_size": 0,              // Invalid: zero value
    "retention_days": -30         // Invalid: negative value
  }
  
  let invalid_update_result = config_manager.update_configuration("telemetry", invalid_updates)
  assert_false(invalid_update_result.success)
  assert_true(invalid_update_result.validation_errors.length() > 0)
  
  for error in invalid_update_result.validation_errors {
    assert_true(error.field.length() > 0)
    assert_true(error.message.length() > 0)
    assert_true(error.severity == "error" || error.severity == "warning")
  }
  
  // Stop configuration manager
  config_manager.stop()
}

// Test 2: Environment-based Configuration
test "environment-based configuration" {
  let config_manager = AdaptiveConfigManager::new()
  
  // Initialize with environment-specific configuration
  config_manager.initialize(ConfigInitOptions::new()
    .with_environment("production")
    .with_config_directory("/etc/azimuth/environments")
    .with_environment_overrides_enabled(true))
  
  // Start configuration manager
  config_manager.start()
  
  // Test environment detection
  let detected_environment = config_manager.detect_environment()
  assert_eq(detected_environment, "production")
  
  // Get environment-specific configuration
  let env_config = config_manager.get_environment_configuration()
  assert_true(env_config.contains_key("environment"))
  assert_eq(env_config.get("environment.name"), Some("production"))
  assert_true(env_config.contains_key("logging"))
  assert_eq(env_config.get("logging.level"), Some("warn"))
  assert_true(env_config.contains_key("performance"))
  assert_eq(env_config.get("performance.monitoring_enabled"), Some(true))
  
  // Test environment switching
  let environments = ["development", "staging", "production", "testing"]
  
  for env in environments {
    let switch_result = config_manager.switch_environment(env)
    assert_true(switch_result.success)
    
    let current_env = config_manager.get_current_environment()
    assert_eq(current_env, env)
    
    let env_specific_config = config_manager.get_environment_configuration()
    assert_eq(env_specific_config.get("environment.name"), Some(env))
    
    // Verify environment-specific settings
    match env {
      "development" => {
        assert_eq(env_specific_config.get("logging.level"), Some("debug"))
        assert_eq(env_specific_config.get("performance.monitoring_enabled"), Some(false))
      },
      "staging" => {
        assert_eq(env_specific_config.get("logging.level"), Some("info"))
        assert_eq(env_specific_config.get("performance.monitoring_enabled"), Some(true))
      },
      "production" => {
        assert_eq(env_specific_config.get("logging.level"), Some("warn"))
        assert_eq(env_specific_config.get("performance.monitoring_enabled"), Some(true))
      },
      "testing" => {
        assert_eq(env_specific_config.get("logging.level"), Some("error"))
        assert_eq(env_specific_config.get("performance.monitoring_enabled"), Some(false))
      },
      _ => assert_true(false)
    }
  }
  
  // Test environment variable overrides
  let env_overrides = {
    "AZIMUTH_TELEMETRY_COLLECTION_INTERVAL": "1000",
    "AZIMUTH_SAMPLING_RATE": "0.2",
    "AZIMUTH_LOGGING_LEVEL": "trace"
  }
  
  let override_result = config_manager.apply_environment_overrides(env_overrides)
  assert_true(override_result.success)
  assert_true(override_result.applied_overrides.length() == 3)
  
  // Verify environment variable overrides
  let overridden_config = config_manager.get_configuration()
  assert_eq(overridden_config.get("telemetry.collection_interval"), Some(1000))
  assert_eq(overridden_config.get("sampling.rate"), Some(0.2))
  assert_eq(overridden_config.get("logging.level"), Some("trace"))
  
  // Test configuration inheritance
  let base_config = config_manager.get_base_configuration()
  let current_config = config_manager.get_configuration()
  
  // Verify current config inherits from base config
  assert_true(base_config.contains_key("telemetry"))
  assert_true(current_config.contains_key("telemetry"))
  
  // Verify overrides take precedence
  assert_ne(current_config.get("telemetry.collection_interval"), base_config.get("telemetry.collection_interval"))
  
  // Stop configuration manager
  config_manager.stop()
}

// Test 3: Adaptive Configuration Based on System Metrics
test "adaptive configuration based on system metrics" {
  let config_manager = AdaptiveConfigManager::new()
  
  // Initialize adaptive configuration
  config_manager.initialize(ConfigInitOptions::new()
    .with_adaptation_enabled(true)
    .with_metrics_source("system_monitor")
    .with_adaptation_interval(10000) // 10 seconds
    .with_adaptation_thresholds(AdaptationThresholds::new()
      .with_cpu_threshold(80.0)
      .with_memory_threshold(85.0)
      .with_disk_threshold(90.0)
      .with_network_threshold(70.0)))
  
  // Start configuration manager
  config_manager.start()
  
  // Generate system metrics
  let system_metrics = generate_system_metrics(50)
  
  // Process system metrics and adapt configuration
  let adaptation_events = []
  for metrics in system_metrics {
    let adaptation_result = config_manager.adapt_configuration(metrics)
    if adaptation_result.adaptation_occurred {
      adaptation_events.push(adaptation_result)
    }
  }
  
  // Verify adaptation events
  assert_true(adaptation_events.length() > 0)
  
  for event in adaptation_events {
    assert_true(event.timestamp > 0)
    assert_true(event.triggering_metric.length() > 0)
    assert_true(event.metric_value > 0.0)
    assert_true(event.adapted_configuration_key.length() > 0)
    assert_true(event.old_value != event.new_value)
    assert_true(event.adaptation_reason.length() > 0)
  }
  
  // Test CPU-based adaptation
  let high_cpu_metrics = generate_high_cpu_metrics(20)
  let cpu_adaptations = []
  
  for metrics in high_cpu_metrics {
    let adaptation_result = config_manager.adapt_configuration(metrics)
    if adaptation_result.adaptation_occurred && adaptation_result.triggering_metric == "cpu" {
      cpu_adaptations.push(adaptation_result)
    }
  }
  
  // Verify CPU-based adaptations
  assert_true(cpu_adaptations.length() > 0)
  
  for adaptation in cpu_adaptations {
    // Should reduce sampling rate when CPU is high
    if adaptation.adapted_configuration_key == "sampling.rate" {
      assert_true(adaptation.new_value < adaptation.old_value)
    }
    
    // Should increase batch size when CPU is high
    if adaptation.adapted_configuration_key == "telemetry.batch_size" {
      assert_true(adaptation.new_value > adaptation.old_value)
    }
  }
  
  // Test memory-based adaptation
  let high_memory_metrics = generate_high_memory_metrics(20)
  let memory_adaptations = []
  
  for metrics in high_memory_metrics {
    let adaptation_result = config_manager.adapt_configuration(metrics)
    if adaptation_result.adaptation_occurred && adaptation_result.triggering_metric == "memory" {
      memory_adaptations.push(adaptation_result)
    }
  }
  
  // Verify memory-based adaptations
  assert_true(memory_adaptations.length() > 0)
  
  for adaptation in memory_adaptations {
    // Should reduce buffer size when memory is high
    if adaptation.adapted_configuration_key == "telemetry.buffer_size" {
      assert_true(adaptation.new_value < adaptation.old_value)
    }
    
    // Should increase compression when memory is high
    if adaptation.adapted_configuration_key == "telemetry.compression_level" {
      assert_true(adaptation.new_value > adaptation.old_value)
    }
  }
  
  // Test disk-based adaptation
  let high_disk_metrics = generate_high_disk_metrics(20)
  let disk_adaptations = []
  
  for metrics in high_disk_metrics {
    let adaptation_result = config_manager.adapt_configuration(metrics)
    if adaptation_result.adaptation_occurred && adaptation_result.triggering_metric == "disk" {
      disk_adaptations.push(adaptation_result)
    }
  }
  
  // Verify disk-based adaptations
  assert_true(disk_adaptations.length() > 0)
  
  for adaptation in disk_adaptations {
    // Should reduce retention days when disk is high
    if adaptation.adapted_configuration_key == "telemetry.retention_days" {
      assert_true(adaptation.new_value < adaptation.old_value)
    }
    
    // Should enable more aggressive cleanup when disk is high
    if adaptation.adapted_configuration_key == "storage.cleanup_enabled" {
      assert_eq(adaptation.new_value, true)
    }
  }
  
  // Test adaptation policies
  let adaptation_policies = [
    AdaptationPolicy::new("conservative", 0.8, 0.1, 300), // High threshold, small changes, slow frequency
    AdaptationPolicy::new("moderate", 0.7, 0.2, 180),    // Medium threshold, medium changes, medium frequency
    AdaptationPolicy::new("aggressive", 0.6, 0.3, 60)    // Low threshold, large changes, fast frequency
  ]
  
  for policy in adaptation_policies {
    config_manager.set_adaptation_policy(policy)
    
    let policy_metrics = generate_stress_test_metrics(30)
    let policy_adaptations = []
    
    for metrics in policy_metrics {
      let adaptation_result = config_manager.adapt_configuration(metrics)
      if adaptation_result.adaptation_occurred {
        policy_adaptations.push(adaptation_result)
      }
    }
    
    // Verify policy-specific behavior
    match policy.name {
      "conservative" => {
        // Should have fewer adaptations with smaller changes
        assert_true(policy_adaptations.length() < 10)
        for adaptation in policy_adaptations {
          let change_magnitude = abs(adaptation.new_value - adaptation.old_value) / adaptation.old_value
          assert_true(change_magnitude < 0.2)
        }
      },
      "moderate" => {
        // Should have moderate adaptations with medium changes
        assert_true(policy_adaptations.length() >= 10 && policy_adaptations.length() < 20)
        for adaptation in policy_adaptations {
          let change_magnitude = abs(adaptation.new_value - adaptation.old_value) / adaptation.old_value
          assert_true(change_magnitude < 0.4)
        }
      },
      "aggressive" => {
        // Should have more adaptations with larger changes
        assert_true(policy_adaptations.length() >= 20)
        for adaptation in policy_adaptations {
          let change_magnitude = abs(adaptation.new_value - adaptation.old_value) / adaptation.old_value
          assert_true(change_magnitude < 0.6)
        }
      },
      _ => assert_true(false)
    }
  }
  
  // Test adaptation history
  let adaptation_history = config_manager.get_adaptation_history()
  assert_true(adaptation_history.length() > 0)
  
  for event in adaptation_history {
    assert_true(event.event_id.length() > 0)
    assert_true(event.timestamp > 0)
    assert_true(event.policy.length() > 0)
    assert_true(event.triggering_metric.length() > 0)
    assert_true(event.metric_value > 0.0)
    assert_true(event.adapted_configuration_key.length() > 0)
    assert_true(event.old_value != event.new_value)
    assert_true(event.adaptation_reason.length() > 0)
  }
  
  // Stop configuration manager
  config_manager.stop()
}

// Test 4: Configuration Templates and Profiles
test "configuration templates and profiles" {
  let config_manager = AdaptiveConfigManager::new()
  
  // Initialize with template support
  config_manager.initialize(ConfigInitOptions::new()
    .with_template_directory("/etc/azimuth/templates")
    .with_profile_directory("/etc/azimuth/profiles")
    .with_template_inheritance_enabled(true))
  
  // Start configuration manager
  config_manager.start()
  
  // Test configuration templates
  let templates = config_manager.list_templates()
  assert_true(templates.length() > 0)
  
  for template in templates {
    assert_true(template.name.length() > 0)
    assert_true(template.description.length() > 0)
    assert_true(template.version.length() > 0)
    assert_true(template.created_at > 0)
    assert_true(template.parameters.length() > 0)
  }
  
  // Apply template with parameters
  let template_params = {
    "collection_interval": 1000,
    "batch_size": 500,
    "compression_enabled": true,
    "retention_days": 60
  }
  
  let template_result = config_manager.apply_template("high_performance", template_params)
  assert_true(template_result.success)
  assert_true(template_result.applied_settings.length() > 0)
  
  // Verify template application
  let template_config = config_manager.get_configuration()
  assert_eq(template_config.get("telemetry.collection_interval"), Some(1000))
  assert_eq(template_config.get("telemetry.batch_size"), Some(500))
  assert_eq(template_config.get("telemetry.compression_enabled"), Some(true))
  assert_eq(template_config.get("telemetry.retention_days"), Some(60))
  
  // Test configuration profiles
  let profiles = config_manager.list_profiles()
  assert_true(profiles.length() > 0)
  
  for profile in profiles {
    assert_true(profile.name.length() > 0)
    assert_true(profile.description.length() > 0)
    assert_true(profile.environment.length() > 0)
    assert_true(profile.created_at > 0)
    assert_true(profile.settings.length() > 0)
  }
  
  // Apply configuration profile
  let profile_result = config_manager.apply_profile("production_high_throughput")
  assert_true(profile_result.success)
  assert_true(profile_result.applied_settings.length() > 0)
  
  // Verify profile application
  let profile_config = config_manager.get_configuration()
  assert_eq(profile_config.get("profile.name"), Some("production_high_throughput"))
  assert_true(profile_config.contains_key("telemetry"))
  assert_true(profile_config.contains_key("sampling"))
  assert_true(profile_config.contains_key("storage"))
  assert_true(profile_config.contains_key("network"))
  
  // Test profile inheritance
  let base_profile_result = config_manager.apply_profile("base")
  assert_true(base_profile_result.success)
  
  let extended_profile_result = config_manager.apply_profile("extended")
  assert_true(extended_profile_result.success)
  
  // Verify profile inheritance
  let inherited_config = config_manager.get_configuration()
  assert_true(inherited_config.contains_key("base_settings"))
  assert_true(inherited_config.contains_key("extended_settings"))
  
  // Test template validation
  let invalid_template_params = {
    "collection_interval": -1000,  // Invalid: negative value
    "batch_size": 0,               // Invalid: zero value
    "retention_days": -60          // Invalid: negative value
  }
  
  let invalid_template_result = config_manager.apply_template("high_performance", invalid_template_params)
  assert_false(invalid_template_result.success)
  assert_true(invalid_template_result.validation_errors.length() > 0)
  
  // Test template composition
  let template_compositions = [
    ["base", "performance"],
    ["base", "security"],
    ["base", "performance", "security"],
    ["performance", "security", "monitoring"]
  ]
  
  for composition in template_compositions {
    let composition_result = config_manager.compose_templates(composition)
    assert_true(composition_result.success)
    assert_true(composition_result.composed_settings.length() > 0)
    
    // Verify template composition
    let composed_config = config_manager.get_configuration()
    assert_true(composed_config.contains_key("telemetry"))
    
    // Verify no conflicts in composition
    assert_true(composition_result.conflicts.length() == 0)
  }
  
  // Test profile versioning
  let profile_versions = config_manager.get_profile_versions("production_high_throughput")
  assert_true(profile_versions.length() > 0)
  
  for version in profile_versions {
    assert_true(version.version.length() > 0)
    assert_true(version.created_at > 0)
    assert_true(version.changelog.length() > 0)
    assert_true(version.active == false || version.active == true)
  }
  
  // Test profile rollback to previous version
  let previous_version = profile_versions[profile_versions.length() - 2]
  let rollback_result = config_manager.rollback_profile("production_high_throughput", previous_version.version)
  assert_true(rollback_result.success)
  assert_true(rollback_result.rollback_successful)
  
  // Stop configuration manager
  config_manager.stop()
}

// Test 5: Configuration Security and Access Control
test "configuration security and access control" {
  let config_manager = AdaptiveConfigManager::new()
  
  // Initialize with security features
  config_manager.initialize(ConfigInitOptions::new()
    .with_encryption_enabled(true)
    .with_access_control_enabled(true)
    .with_audit_logging_enabled(true)
    .with_role_based_access_enabled(true))
  
  // Start configuration manager
  config_manager.start()
  
  // Test user authentication
  let users = [
    User::new("admin", "admin_password", ["admin", "read", "write"]),
    User::new("operator", "operator_password", ["operator", "read", "write"]),
    User::new("viewer", "viewer_password", ["viewer", "read"]),
    User::new("unauthorized", "unauthorized_password", [])
  ]
  
  let auth_results = []
  for user in users {
    let result = config_manager.authenticate_user(user.username, user.password)
    auth_results.push((user.username, result))
  }
  
  // Verify authentication results
  assert_eq(auth_results.length(), 4)
  
  for (username, result) in auth_results {
    if username != "unauthorized" {
      assert_true(result.authenticated)
      assert_true(result.session_token.length() > 0)
      assert_true(result.expires_at > get_current_time_ms())
      assert_true(result.roles.length() > 0)
    } else {
      // Unauthorized user should still authenticate but have no roles
      assert_true(result.authenticated)
      assert_eq(result.roles.length(), 0)
    }
  }
  
  // Test role-based access control
  let admin_token = auth_results[0].1.session_token
  let operator_token = auth_results[1].1.session_token
  let viewer_token = auth_results[2].1.session_token
  let unauthorized_token = auth_results[3].1.session_token
  
  // Test read access
  let read_operations = [
    ("admin", admin_token, "read", "telemetry"),
    ("operator", operator_token, "read", "telemetry"),
    ("viewer", viewer_token, "read", "telemetry"),
    ("unauthorized", unauthorized_token, "read", "telemetry")
  ]
  
  for (user, token, operation, section) in read_operations {
    let access_result = config_manager.check_access(token, operation, section)
    
    match user {
      "admin" => assert_true(access_result.granted),
      "operator" => assert_true(access_result.granted),
      "viewer" => assert_true(access_result.granted),
      "unauthorized" => assert_false(access_result.granted),
      _ => assert_true(false)
    }
  }
  
  // Test write access
  let write_operations = [
    ("admin", admin_token, "write", "telemetry"),
    ("operator", operator_token, "write", "telemetry"),
    ("viewer", viewer_token, "write", "telemetry"),
    ("unauthorized", unauthorized_token, "write", "telemetry")
  ]
  
  for (user, token, operation, section) in write_operations {
    let access_result = config_manager.check_access(token, operation, section)
    
    match user {
      "admin" => assert_true(access_result.granted),
      "operator" => assert_true(access_result.granted),
      "viewer" => assert_false(access_result.granted),
      "unauthorized" => assert_false(access_result.granted),
      _ => assert_true(false)
    }
  }
  
  // Test admin operations
  let admin_operations = [
    ("admin", admin_token, "admin", "users"),
    ("operator", operator_token, "admin", "users"),
    ("viewer", viewer_token, "admin", "users"),
    ("unauthorized", unauthorized_token, "admin", "users")
  ]
  
  for (user, token, operation, section) in admin_operations {
    let access_result = config_manager.check_access(token, operation, section)
    
    match user {
      "admin" => assert_true(access_result.granted),
      "operator" => assert_false(access_result.granted),
      "viewer" => assert_false(access_result.granted),
      "unauthorized" => assert_false(access_result.granted),
      _ => assert_true(false)
    }
  }
  
  // Test configuration encryption
  let sensitive_config = {
    "database_password": "secret_password_123",
    "api_key": "secret_api_key_456",
    "encryption_key": "secret_encryption_key_789"
  }
  
  let encryption_result = config_manager.encrypt_sensitive_configuration(sensitive_config)
  assert_true(encryption_result.success)
  assert_true(encryption_result.encrypted_data.length() > 0)
  assert_true(encryption_result.encryption_key_id.length() > 0)
  
  // Test configuration decryption
  let decryption_result = config_manager.decrypt_sensitive_configuration(encryption_result.encrypted_data)
  assert_true(decryption_result.success)
  assert_eq(decryption_result.decrypted_data.get("database_password"), Some("secret_password_123"))
  assert_eq(decryption_result.decrypted_data.get("api_key"), Some("secret_api_key_456"))
  assert_eq(decryption_result.decrypted_data.get("encryption_key"), Some("secret_encryption_key_789"))
  
  // Test audit logging
  let audit_logs = config_manager.get_audit_logs()
  assert_true(audit_logs.length() > 0)
  
  for log in audit_logs {
    assert_true(log.log_id.length() > 0)
    assert_true(log.timestamp > 0)
    assert_true(log.user.length() > 0)
    assert_true(log.operation.length() > 0)
    assert_true(log.resource.length() > 0)
    assert_true(log.result == "success" || log.result == "failure")
    assert_true(log.details.length() > 0)
  }
  
  // Test configuration backup with encryption
  let backup_result = config_manager.create_encrypted_backup()
  assert_true(backup_result.success)
  assert_true(backup_result.backup_path.length() > 0)
  assert_true(backup_result.backup_size > 0)
  assert_true(backup_result.encryption_enabled)
  assert_true(backup_result.checksum.length() > 0)
  
  // Test encrypted backup restore
  let restore_result = config_manager.restore_encrypted_backup(backup_result.backup_path)
  assert_true(restore_result.success)
  assert_true(restore_result.restore_successful)
  assert_true(restore_result.backup_verified)
  
  // Stop configuration manager
  config_manager.stop()
}

// Test 6: Configuration Validation and Schema
test "configuration validation and schema" {
  let config_manager = AdaptiveConfigManager::new()
  
  // Initialize with validation features
  config_manager.initialize(ConfigInitOptions::new()
    .with_schema_validation_enabled(true)
    .with_schema_directory("/etc/azimuth/schemas")
    .with_strict_validation_enabled(false)
    .with_custom_validators_enabled(true))
  
  // Start configuration manager
  config_manager.start()
  
  // Test configuration schema loading
  let schemas = config_manager.list_schemas()
  assert_true(schemas.length() > 0)
  
  for schema in schemas {
    assert_true(schema.name.length() > 0)
    assert_true(schema.version.length() > 0)
    assert_true(schema.description.length() > 0)
    assert_true(schema.schema_content.length() > 0)
  }
  
  // Test valid configuration against schema
  let valid_config = {
    "telemetry": {
      "collection_interval": 1000,
      "batch_size": 100,
      "compression_enabled": true,
      "retention_days": 30
    },
    "sampling": {
      "strategy": "adaptive",
      "rate": 0.1,
      "max_samples_per_second": 1000
    },
    "storage": {
      "type": "file",
      "path": "/var/lib/azimuth/telemetry",
      "max_size_gb": 100
    }
  }
  
  let validation_result = config_manager.validate_configuration(valid_config)
  assert_true(validation_result.valid)
  assert_eq(validation_result.errors.length(), 0)
  assert_true(validation_result.warnings.length() >= 0)
  
  // Test invalid configuration against schema
  let invalid_config = {
    "telemetry": {
      "collection_interval": -1000,  // Invalid: negative value
      "batch_size": 0,               // Invalid: zero value
      "compression_enabled": "true", // Invalid: string instead of boolean
      "retention_days": -30          // Invalid: negative value
    },
    "sampling": {
      "strategy": "invalid_strategy", // Invalid: not in enum
      "rate": 1.5,                    // Invalid: value > 1.0
      "max_samples_per_second": -1000 // Invalid: negative value
    },
    "storage": {
      "type": 123,                    // Invalid: number instead of string
      "path": "",                     // Invalid: empty string
      "max_size_gb": -50              // Invalid: negative value
    }
  }
  
  let invalid_validation_result = config_manager.validate_configuration(invalid_config)
  assert_false(invalid_validation_result.valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  for error in invalid_validation_result.errors {
    assert_true(error.field.length() > 0)
    assert_true(error.message.length() > 0)
    assert_true(error.value.length() > 0)
    assert_true(error.constraint.length() > 0)
  }
  
  // Test custom validators
  let custom_validators = [
    CustomValidator::new("port_range", "network.port", |value| {
      match value {
        Int(port) => port >= 1 && port <= 65535,
        _ => false
      }
    }),
    CustomValidator::new("url_format", "api.endpoint", |value| {
      match value {
        String(url) => url.starts_with("http://") || url.starts_with("https://"),
        _ => false
      }
    }),
    CustomValidator::new("email_format", "notification.email", |value| {
      match value {
        String(email) => email.contains("@") && email.contains("."),
        _ => false
      }
    })
  ]
  
  for validator in custom_validators {
    let registration_result = config_manager.register_custom_validator(validator)
    assert_true(registration_result.success)
  }
  
  // Test configuration with custom validators
  let config_with_custom_fields = {
    "network": {
      "port": 8080,
      "host": "localhost"
    },
    "api": {
      "endpoint": "https://api.azimuth.com",
      "timeout": 30000
    },
    "notification": {
      "email": "admin@azimuth.com",
      "enabled": true
    }
  }
  
  let custom_validation_result = config_manager.validate_configuration(config_with_custom_fields)
  assert_true(custom_validation_result.valid)
  
  // Test configuration that fails custom validation
  let invalid_custom_config = {
    "network": {
      "port": 70000,  // Invalid: port > 65535
      "host": "localhost"
    },
    "api": {
      "endpoint": "invalid_url",  // Invalid: not a valid URL
      "timeout": 30000
    },
    "notification": {
      "email": "invalid_email",  // Invalid: not a valid email
      "enabled": true
    }
  }
  
  let invalid_custom_validation_result = config_manager.validate_configuration(invalid_custom_config)
  assert_false(invalid_custom_validation_result.valid)
  assert_true(invalid_custom_validation_result.errors.length() >= 3)
  
  // Test schema versioning
  let schema_versions = config_manager.get_schema_versions("telemetry")
  assert_true(schema_versions.length() > 0)
  
  for version in schema_versions {
    assert_true(version.version.length() > 0)
    assert_true(version.created_at > 0)
    assert_true(schema_versions.contains(version.version))
  }
  
  // Test schema migration
  let old_config = {
    "telemetry": {
      "interval": 5000,  // Old field name
      "batch": 50        // Old field name
    }
  }
  
  let migration_result = config_manager.migrate_configuration(old_config, "telemetry", "1.0.0", "2.0.0")
  assert_true(migration_result.success)
  assert_true(migration_result.migrated)
  assert_true(migration_result.migrated_fields.length() > 0)
  
  // Verify migration
  let migrated_config = migration_result.migrated_configuration
  assert_true(migrated_config.contains_key("telemetry.collection_interval"))
  assert_true(migrated_config.contains_key("telemetry.batch_size"))
  assert_false(migrated_config.contains_key("telemetry.interval"))
  assert_false(migrated_config.contains_key("telemetry.batch"))
  
  // Stop configuration manager
  config_manager.stop()
}

// Test 7: Configuration Monitoring and Alerting
test "configuration monitoring and alerting" {
  let config_manager = AdaptiveConfigManager::new()
  
  // Initialize with monitoring features
  config_manager.initialize(ConfigInitOptions::new()
    .with_monitoring_enabled(true)
    .with_alerting_enabled(true)
    .with_change_tracking_enabled(true)
    .with_monitoring_interval(5000))
  
  // Start configuration manager
  config_manager.start()
  
  // Test configuration monitoring
  let monitoring_data = generate_monitoring_data(100)
  
  let monitoring_events = []
  for data in monitoring_data {
    let event = config_manager.monitor_configuration(data)
    if event.event_occurred {
      monitoring_events.push(event)
    }
  }
  
  // Verify monitoring events
  assert_true(monitoring_events.length() > 0)
  
  for event in monitoring_events {
    assert_true(event.event_id.length() > 0)
    assert_true(event.timestamp > 0)
    assert_true(event.event_type.length() > 0)
    assert_true(event.configuration_key.length() > 0)
    assert_true(event.old_value != event.new_value)
    assert_true(event.description.length() > 0)
  }
  
  // Test configuration change tracking
  let initial_config = config_manager.get_configuration()
  
  // Make configuration changes
  let changes = [
    ("telemetry.collection_interval", 2000),
    ("sampling.rate", 0.2),
    ("storage.max_size_gb", 200)
  ]
  
  let change_results = []
  for (key, value) in changes {
    let result = config_manager.update_configuration_by_key(key, value)
    change_results.push(result)
  }
  
  // Verify change tracking
  let change_history = config_manager.get_change_history()
  assert_true(change_history.length() >= 3)
  
  for change in change_history {
    assert_true(change.change_id.length() > 0)
    assert_true(change.timestamp > 0)
    assert_true(change.configuration_key.length() > 0)
    assert_true(change.old_value != change.new_value)
    assert_true(change.changed_by.length() > 0)
    assert_true(change.reason.length() > 0)
  }
  
  // Test configuration drift detection
  let expected_config = config_manager.get_configuration()
  
  // Simulate configuration drift
  let drift_config = expected_config.clone()
  drift_config.set("telemetry.collection_interval", 5000)  // Drifted value
  drift_config.set("sampling.rate", 0.5)                   // Drifted value
  
  let drift_result = config_manager.detect_configuration_drift(drift_config)
  assert_true(drift_result.drift_detected)
  assert_true(drift_result.drifted_keys.length() == 2)
  assert_true(drift_result.drifted_keys.contains("telemetry.collection_interval"))
  assert_true(drift_result.drifted_keys.contains("sampling.rate"))
  
  // Test configuration alerting
  let alert_rules = [
    AlertRule::new("high_collection_interval", "telemetry.collection_interval", ">", 3000, "warning"),
    AlertRule::new("low_sampling_rate", "sampling.rate", "<", 0.05, "warning"),
    AlertRule::new("high_storage_usage", "storage.max_size_gb", ">", 500, "critical")
  ]
  
  for rule in alert_rules {
    let registration_result = config_manager.register_alert_rule(rule)
    assert_true(registration_result.success)
  }
  
  // Trigger alerts
  let alert_config = {
    "telemetry": {
      "collection_interval": 5000,  // Triggers high_collection_interval alert
      "batch_size": 100
    },
    "sampling": {
      "rate": 0.01,                 // Triggers low_sampling_rate alert
      "strategy": "adaptive"
    },
    "storage": {
      "max_size_gb": 1000           // Triggers high_storage_usage alert
    }
  }
  
  let alert_result = config_manager.check_configuration_alerts(alert_config)
  assert_true(alert_result.alerts_triggered)
  assert_true(alert_result.alerts.length() == 3)
  
  // Verify alerts
  for alert in alert_result.alerts {
    assert_true(alert.alert_id.length() > 0)
    assert_true(alert.rule_name.length() > 0)
    assert_true(alert.configuration_key.length() > 0)
    assert_true(alert.current_value != alert.threshold_value)
    assert_true(alert.severity == "warning" || alert.severity == "critical")
    assert_true(alert.message.length() > 0)
    assert_true(alert.timestamp > 0)
  }
  
  // Test configuration health checks
  let health_check_result = config_manager.run_configuration_health_check()
  assert_true(health_check_result.overall_health == "healthy" || health_check_result.overall_health == "warning" || health_check_result.overall_health == "critical")
  assert_true(health_check_result.checks.length() > 0)
  
  for check in health_check_result.checks {
    assert_true(check.check_name.length() > 0)
    assert_true(check.status == "pass" || check.status == "fail" || check.status == "warn")
    assert_true(check.check_time_ms >= 0)
    assert_true(check.details.length() > 0)
  }
  
  // Test configuration metrics
  let config_metrics = config_manager.get_configuration_metrics()
  assert_true(config_metrics.total_configuration_keys > 0)
  assert_true(config_metrics.configuration_changes_today >= 0)
  assert_true(config_metrics.configuration_validation_failures >= 0)
  assert_true(config_metrics.alerts_triggered_today >= 0)
  assert_true(config_metrics.last_configuration_change > 0)
  assert_true(config_metrics.configuration_size_bytes > 0)
  
  // Test configuration reporting
  let report_result = config_manager.generate_configuration_report()
  assert_true(report_result.success)
  assert_true(report_result.report_content.length() > 0)
  assert_true(report_result.report_format == "json" || report_result.report_format == "yaml" || report_result.report_format == "csv")
  assert_true(report_result.generated_at > 0)
  
  // Stop configuration manager
  config_manager.stop()
}

// Test 8: Configuration Backup and Recovery
test "configuration backup and recovery" {
  let config_manager = AdaptiveConfigManager::new()
  
  // Initialize with backup features
  config_manager.initialize(ConfigInitOptions::new()
    .with_backup_enabled(true)
    .with_backup_directory("/var/lib/azimuth/backups")
    .with_auto_backup_enabled(true)
    .with_backup_retention_days(30)
    .with_backup_compression_enabled(true))
  
  // Start configuration manager
  config_manager.start()
  
  // Test manual backup creation
  let backup_result = config_manager.create_backup("manual_backup_001")
  assert_true(backup_result.success)
  assert_true(backup_result.backup_id.length() > 0)
  assert_true(backup_result.backup_path.length() > 0)
  assert_true(backup_result.backup_size > 0)
  assert_true(backup_result.created_at > 0)
  assert_true(backup_result.compressed)
  
  // Test backup listing
  let backups = config_manager.list_backups()
  assert_true(backups.length() > 0)
  
  for backup in backups {
    assert_true(backup.backup_id.length() > 0)
    assert_true(backup.backup_path.length() > 0)
    assert_true(backup.backup_size > 0)
    assert_true(backup.created_at > 0)
    assert_true(backup.compressed == false || backup.compressed == true)
    assert_true(backup.description.length() > 0)
  }
  
  // Test backup verification
  let verification_result = config_manager.verify_backup(backup_result.backup_id)
  assert_true(verification_result.verified)
  assert_true(verification_result.checksum_valid)
  assert_true(verification_result.integrity_check_passed)
  assert_true(verification_result.verification_time_ms > 0)
  
  // Test configuration restoration
  let original_config = config_manager.get_configuration()
  
  // Make configuration changes
  let config_changes = {
    "telemetry.collection_interval": 3000,
    "sampling.rate": 0.3,
    "storage.max_size_gb": 300
  }
  
  for (key, value) in config_changes {
    config_manager.update_configuration_by_key(key, value)
  }
  
  let modified_config = config_manager.get_configuration()
  assert_ne(modified_config.get("telemetry.collection_interval"), original_config.get("telemetry.collection_interval"))
  
  // Restore from backup
  let restore_result = config_manager.restore_from_backup(backup_result.backup_id)
  assert_true(restore_result.success)
  assert_true(restore_result.restored)
  assert_true(restore_result.backup_id == backup_result.backup_id)
  assert_true(restore_result.restore_time_ms > 0)
  
  // Verify restoration
  let restored_config = config_manager.get_configuration()
  assert_eq(restored_config.get("telemetry.collection_interval"), original_config.get("telemetry.collection_interval"))
  assert_eq(restored_config.get("sampling.rate"), original_config.get("sampling.rate"))
  assert_eq(restored_config.get("storage.max_size_gb"), original_config.get("storage.max_size_gb"))
  
  // Test scheduled backups
  let schedule_result = config_manager.schedule_backup("daily", 24 * 60 * 60 * 1000) // 24 hours
  assert_true(schedule_result.scheduled)
  assert_true(schedule_result.schedule_id.length() > 0)
  assert_true(schedule_result.interval_ms == 24 * 60 * 60 * 1000)
  assert_true(schedule_result.next_backup_time > get_current_time_ms())
  
  // Verify scheduled backup
  let scheduled_backups = config_manager.get_scheduled_backups()
  assert_true(scheduled_backups.length() > 0)
  
  let scheduled_backup = scheduled_backups[0]
  assert_eq(scheduled_backup.schedule_id, schedule_result.schedule_id)
  assert_eq(scheduled_backup.interval_ms, 24 * 60 * 60 * 1000)
  assert_true(scheduled_backup.next_backup_time > get_current_time_ms())
  assert_false(scheduled_backup.completed)
  
  // Test backup retention
  let retention_result = config_manager.apply_backup_retention_policy()
  assert_true(retention_result.success)
  assert_true(retention_result.backups_deleted >= 0)
  assert_true(retention_result.space_freed_mb >= 0)
  
  // Test backup export
  let export_result = config_manager.export_backup(backup_result.backup_id, "/tmp/exported_backup.json")
  assert_true(export_result.success)
  assert_true(export_result.export_path.length() > 0)
  assert_true(export_result.export_size > 0)
  assert_true(export_result.exported_at > 0)
  
  // Test backup import
  let import_result = config_manager.import_backup("/tmp/exported_backup.json")
  assert_true(import_result.success)
  assert_true(import_result.imported_backup_id.length() > 0)
  assert_true(import_result.imported_at > 0)
  
  // Test incremental backups
  let incremental_backup_result = config_manager.create_incremental_backup(backup_result.backup_id)
  assert_true(incremental_backup_result.success)
  assert_true(incremental_backup_result.backup_id.length() > 0)
  assert_true(incremental_backup_result.incremental)
  assert_true(incremental_backup_result.base_backup_id == backup_result.backup_id)
  assert_true(incremental_backup_result.backup_size < backup_result.backup_size) // Should be smaller
  
  // Test backup encryption
  let encrypted_backup_result = config_manager.create_encrypted_backup("encrypted_backup_001")
  assert_true(encrypted_backup_result.success)
  assert_true(encrypted_backup_result.backup_id.length() > 0)
  assert_true(encrypted_backup_result.encrypted)
  assert_true(encrypted_backup_result.encryption_key_id.length() > 0)
  
  // Test encrypted backup restoration
  let encrypted_restore_result = config_manager.restore_from_encrypted_backup(encrypted_backup_result.backup_id)
  assert_true(encrypted_restore_result.success)
  assert_true(encrypted_restore_result.restored)
  assert_true(encrypted_restore_result.decryption_successful)
  
  // Stop configuration manager
  config_manager.stop()
}

// Test 9: Configuration API and Integration
test "configuration api and integration" {
  let config_manager = AdaptiveConfigManager::new()
  
  // Initialize with API features
  config_manager.initialize(ConfigInitOptions::new()
    .with_api_enabled(true)
    .with_api_port(8080)
    .with_api_authentication_enabled(true)
    .with_api_rate_limiting_enabled(true)
    .with_api_cors_enabled(true))
  
  // Start configuration manager
  config_manager.start()
  
  // Test API endpoints
  let api_endpoints = [
    ("GET", "/api/v1/configuration", "get_configuration"),
    ("PUT", "/api/v1/configuration", "update_configuration"),
    ("GET", "/api/v1/configuration/history", "get_configuration_history"),
    ("POST", "/api/v1/configuration/backup", "create_backup"),
    ("POST", "/api/v1/configuration/restore", "restore_configuration"),
    ("GET", "/api/v1/configuration/schema", "get_configuration_schema"),
    ("POST", "/api/v1/configuration/validate", "validate_configuration")
  ]
  
  for (method, path, operation) in api_endpoints {
    let endpoint_result = config_manager.register_api_endpoint(method, path, operation)
    assert_true(endpoint_result.success)
    assert_true(endpoint_result.endpoint_registered)
  }
  
  // Test API authentication
  let api_credentials = [
    ApiCredentials::new("api_user_001", "api_key_001", ["read", "write"]),
    ApiCredentials::new("api_user_002", "api_key_002", ["read"]),
    ApiCredentials::new("api_user_003", "api_key_003", [])
  ]
  
  for credentials in api_credentials {
    let auth_result = config_manager.authenticate_api_request(credentials.api_key)
    
    match credentials.username {
      "api_user_001" => {
        assert_true(auth_result.authenticated)
        assert_true(auth_result.permissions.contains("read"))
        assert_true(auth_result.permissions.contains("write"))
      },
      "api_user_002" => {
        assert_true(auth_result.authenticated)
        assert_true(auth_result.permissions.contains("read"))
        assert_false(auth_result.permissions.contains("write"))
      },
      "api_user_003" => {
        assert_true(auth_result.authenticated)
        assert_eq(auth_result.permissions.length(), 0)
      },
      _ => assert_true(false)
    }
  }
  
  // Test API rate limiting
  let rate_limit_result = config_manager.set_api_rate_limit(100, 60) // 100 requests per minute
  assert_true(rate_limit_result.success)
  assert_true(rate_limit_result.rate_limit_set)
  assert_eq(rate_limit_result.requests_per_minute, 100)
  
  // Test API requests
  let api_requests = [
    ApiRequest::new("GET", "/api/v1/configuration", "api_key_001"),
    ApiRequest::new("PUT", "/api/v1/configuration", "api_key_001"),
    ApiRequest::new("GET", "/api/v1/configuration", "api_key_002"),
    ApiRequest::new("PUT", "/api/v1/configuration", "api_key_002"), // Should fail: no write permission
    ApiRequest::new("GET", "/api/v1/configuration", "api_key_003"), // Should fail: no read permission
    ApiRequest::new("PUT", "/api/v1/configuration", "invalid_api_key") // Should fail: invalid API key
  ]
  
  let api_responses = []
  for request in api_requests {
    let response = config_manager.process_api_request(request)
    api_responses.push(response)
  }
  
  // Verify API responses
  assert_eq(api_responses.length(), 6)
  
  // Valid GET request with read permission
  assert_eq(api_responses[0].status_code, 200)
  assert_true(api_responses[0].success)
  
  // Valid PUT request with write permission
  assert_eq(api_responses[1].status_code, 200)
  assert_true(api_responses[1].success)
  
  // Valid GET request with read permission
  assert_eq(api_responses[2].status_code, 200)
  assert_true(api_responses[2].success)
  
  // Invalid PUT request without write permission
  assert_eq(api_responses[3].status_code, 403)
  assert_false(api_responses[3].success)
  assert_eq(api_responses[3].error_message, "Insufficient permissions")
  
  // Invalid GET request without read permission
  assert_eq(api_responses[4].status_code, 403)
  assert_false(api_responses[4].success)
  assert_eq(api_responses[4].error_message, "Insufficient permissions")
  
  // Invalid request with invalid API key
  assert_eq(api_responses[5].status_code, 401)
  assert_false(api_responses[5].success)
  assert_eq(api_responses[5].error_message, "Invalid API key")
  
  // Test API documentation
  let api_docs = config_manager.generate_api_documentation()
  assert_true(api_docs.length() > 0)
  assert_true(api_docs.contains("\"title\": \"Azimuth Configuration API\""))
  assert_true(api_docs.contains("\"version\": \"v1\""))
  assert_true(api_docs.contains("\"paths\""))
  assert_true(api_docs.contains("\"components\""))
  
  // Test API metrics
  let api_metrics = config_manager.get_api_metrics()
  assert_true(api_metrics.total_requests > 0)
  assert_true(api_metrics.successful_requests > 0)
  assert_true(api_metrics.failed_requests > 0)
  assert_true(api_metrics.requests_per_minute > 0)
  assert_true(api_metrics.average_response_time_ms > 0)
  assert_true(api_metrics.active_api_keys > 0)
  
  // Test configuration webhook integration
  let webhook_config = WebhookConfig::new()
    .with_url("https://webhook.azimuth.com/configuration")
    .with_secret("webhook_secret_123")
    .with_events(["configuration_updated", "backup_created", "restore_completed"])
    .with_retry_policy(RetryPolicy::exponential(3, 1000, 2.0))
  
  let webhook_result = config_manager.register_webhook(webhook_config)
  assert_true(webhook_result.success)
  assert_true(webhook_result.webhook_registered)
  assert_true(webhook_result.webhook_id.length() > 0)
  
  // Trigger webhook events
  let webhook_events = [
    WebhookEvent::new("configuration_updated", {"key": "telemetry.collection_interval", "value": 2000}),
    WebhookEvent::new("backup_created", {"backup_id": "backup_001", "size": 1024}),
    WebhookEvent::new("restore_completed", {"backup_id": "backup_001", "success": true})
  ]
  
  let webhook_results = []
  for event in webhook_events {
    let result = config_manager.trigger_webhook(event)
    webhook_results.push(result)
  }
  
  // Verify webhook results
  assert_eq(webhook_results.length(), 3)
  
  for result in webhook_results {
    assert_true(result.delivered || result.failed)
    assert_true(result.attempts > 0)
    assert_true(result.response_time_ms > 0)
    
    if result.delivered {
      assert_eq(result.status_code, 200)
    } else {
      assert_true(result.error_message.length() > 0)
    }
  }
  
  // Test external configuration source integration
  let external_source_config = ExternalSourceConfig::new()
    .with_type("consul")
    .with_endpoint("http://consul:8500")
    .with_path("azimuth/configuration")
    .with_polling_interval(30000) // 30 seconds
    .with_authentication_token("consul_token_123")
  
  let external_source_result = config_manager.register_external_source(external_source_config)
  assert_true(external_source_result.success)
  assert_true(external_source_result.source_registered)
  assert_true(external_source_result.source_id.length() > 0)
  
  // Test external configuration synchronization
  let sync_result = config_manager.synchronize_with_external_source(external_source_result.source_id)
  assert_true(sync_result.success)
  assert_true(sync_result.synchronized)
  assert_true(sync_result.keys_synchronized >= 0)
  assert_true(sync_result.sync_time_ms > 0)
  
  // Stop configuration manager
  config_manager.stop()
}