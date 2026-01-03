// Azimuth Telemetry System - Configuration Management Dynamic Update Tests
// This file contains test cases for configuration management and dynamic update functionality

// Test 1: Configuration Creation and Basic Operations
test "configuration creation and basic operations" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Create configuration with default values
  let default_config = [
    ("telemetry.enabled", "true"),
    ("telemetry.sampling_rate", "0.1"),
    ("telemetry.batch_size", "100"),
    ("telemetry.flush_interval_ms", "5000"),
    ("telemetry.max_retries", "3")
  ]
  
  // Set default configuration
  let manager_with_defaults = ConfigurationManager::set_defaults(config_manager, default_config)
  
  // Verify default values
  match ConfigurationManager::get(manager_with_defaults, "telemetry.enabled") {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  match ConfigurationManager::get(manager_with_defaults, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.1")
    None => assert_true(false)
  }
  
  // Override configuration values
  let overrides = [
    ("telemetry.sampling_rate", "0.5"),
    ("telemetry.batch_size", "200"),
    ("telemetry.custom_setting", "custom_value")
  ]
  
  let manager_with_overrides = ConfigurationManager::set_overrides(manager_with_defaults, overrides)
  
  // Verify overridden values
  match ConfigurationManager::get(manager_with_overrides, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.5") // Should be overridden
    None => assert_true(false)
  }
  
  match ConfigurationManager::get(manager_with_overrides, "telemetry.batch_size") {
    Some(value) => assert_eq(value, "200") // Should be overridden
    None => assert_true(false)
  }
  
  match ConfigurationManager::get(manager_with_overrides, "telemetry.enabled") {
    Some(value) => assert_eq(value, "true") // Should remain default
    None => assert_true(false)
  }
  
  match ConfigurationManager::get(manager_with_overrides, "telemetry.custom_setting") {
    Some(value) => assert_eq(value, "custom_value") // New value
    None => assert_true(false)
  }
  
  // Test configuration with different types
  let manager_with_types = ConfigurationManager::set_with_types(
    manager_with_overrides,
    [
      ("telemetry.timeout_ms", "10000", "integer"),
      ("telemetry.enable_compression", "true", "boolean"),
      ("telemetry.buffer_size", "1024", "integer"),
      ("telemetry.debug_mode", "false", "boolean")
    ]
  )
  
  // Verify typed values
  match ConfigurationManager::get_as_integer(manager_with_types, "telemetry.timeout_ms") {
    Some(value) => assert_eq(value, 10000)
    None => assert_true(false)
  }
  
  match ConfigurationManager::get_as_boolean(manager_with_types, "telemetry.enable_compression") {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  match ConfigurationManager::get_as_integer(manager_with_types, "telemetry.buffer_size") {
    Some(value) => assert_eq(value, 1024)
    None => assert_true(false)
  }
  
  match ConfigurationManager::get_as_boolean(manager_with_types, "telemetry.debug_mode") {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
}

// Test 2: Configuration File Loading and Saving
test "configuration file loading and saving" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Create sample configuration file content
  let config_file_content = "# Azimuth Telemetry Configuration\n"
    + "telemetry.enabled=true\n"
    + "telemetry.sampling_rate=0.1\n"
    + "telemetry.batch_size=100\n"
    + "telemetry.flush_interval_ms=5000\n"
    + "telemetry.max_retries=3\n"
    + "# Network configuration\n"
    + "network.timeout_ms=30000\n"
    + "network.max_connections=10\n"
    + "# Storage configuration\n"
    + "storage.retention_days=30\n"
    + "storage.compression=true\n"
  
  // Load configuration from string
  let manager_from_file = ConfigurationManager::load_from_string(config_manager, config_file_content)
  
  // Verify configuration was loaded
  match ConfigurationManager::get(manager_from_file, "telemetry.enabled") {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  match ConfigurationManager::get(manager_from_file, "network.timeout_ms") {
    Some(value) => assert_eq(value, "30000")
    None => assert_true(false)
  }
  
  match ConfigurationManager::get(manager_from_file, "storage.retention_days") {
    Some(value) => assert_eq(value, "30")
    None => assert_true(false)
  }
  
  // Save configuration to string
  let saved_config = ConfigurationManager::save_to_string(manager_from_file)
  
  // Verify saved configuration contains expected values
  assert_true(String::contains(saved_config, "telemetry.enabled=true"))
  assert_true(String::contains(saved_config, "network.timeout_ms=30000"))
  assert_true(String::contains(saved_config, "storage.retention_days=30"))
  
  // Test configuration with sections
  let sectioned_config = ConfigurationManager::load_with_sections(config_manager, config_file_content)
  
  // Verify sections were parsed
  let telemetry_section = ConfigurationManager::get_section(sectioned_config, "telemetry")
  assert_eq(telemetry_section.length(), 5)
  
  let network_section = ConfigurationManager::get_section(sectioned_config, "network")
  assert_eq(network_section.length(), 2)
  
  let storage_section = ConfigurationManager::get_section(sectioned_config, "storage")
  assert_eq(storage_section.length(), 2)
  
  // Test configuration validation
  let validation_rules = [
    ("telemetry.sampling_rate", \value => {
      match Float::parse(value) {
        Some(rate) => rate >= 0.0 && rate <= 1.0
        None => false
      }
    }),
    ("telemetry.batch_size", \value => {
      match Int::parse(value) {
        Some(size) => size > 0
        None => false
      }
    }),
    ("network.timeout_ms", \value => {
      match Int::parse(value) {
        Some(timeout) => timeout > 0
        None => false
      }
    })
  ]
  
  let validation_result = ConfigurationManager::validate(manager_from_file, validation_rules)
  
  // Verify validation passed
  assert_true(ValidationResult::is_valid(validation_result))
  assert_eq(ValidationResult::error_count(validation_result), 0)
  
  // Test invalid configuration
  let invalid_config_content = "telemetry.sampling_rate=1.5\n"
    + "telemetry.batch_size=0\n"
    + "network.timeout_ms=-100\n"
  
  let manager_with_invalid = ConfigurationManager::load_from_string(config_manager, invalid_config_content)
  let invalid_validation_result = ConfigurationManager::validate(manager_with_invalid, validation_rules)
  
  // Verify validation failed
  assert_false(ValidationResult::is_valid(invalid_validation_result))
  assert_eq(ValidationResult::error_count(invalid_validation_result), 3)
}

// Test 3: Dynamic Configuration Updates
test "dynamic configuration updates" {
  // Create configuration manager with dynamic update support
  let config_manager = DynamicConfigurationManager::new()
  
  // Set initial configuration
  let initial_config = [
    ("telemetry.enabled", "true"),
    ("telemetry.sampling_rate", "0.1"),
    ("telemetry.batch_size", "100")
  ]
  
  let manager_with_initial = DynamicConfigurationManager::set_configuration(config_manager, initial_config)
  
  // Create configuration change listeners
  let listener1 = ConfigurationChangeListener::new("listener1", \key, old_value, new_value => {
    // Simulate listener callback
    true // Return true to indicate success
  })
  
  let listener2 = ConfigurationChangeListener::new("listener2", \key, old_value, new_value => {
    // Simulate listener callback
    true
  })
  
  // Register listeners
  let manager_with_listeners = DynamicConfigurationManager::add_listener(
    DynamicConfigurationManager::add_listener(manager_with_initial, listener1),
    listener2)
  
  // Verify listeners were registered
  assert_eq(DynamicConfigurationManager::listener_count(manager_with_listeners), 2)
  
  // Update configuration dynamically
  let update_result = DynamicConfigurationManager::update_value(
    manager_with_listeners,
    "telemetry.sampling_rate",
    "0.5"
  )
  
  // Verify update was successful
  assert_true(ConfigurationUpdateResult::is_success(update_result))
  
  // Verify value was updated
  match DynamicConfigurationManager::get(manager_with_listeners, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.5")
    None => assert_true(false)
  }
  
  // Verify change notifications were sent
  let notifications = ConfigurationUpdateResult::get_notifications(update_result)
  assert_eq(notifications.length(), 2) // One for each listener
  
  // Test batch configuration updates
  let batch_updates = [
    ("telemetry.enabled", "false"),
    ("telemetry.batch_size", "200"),
    ("telemetry.new_setting", "new_value")
  ]
  
  let batch_update_result = DynamicConfigurationManager::update_batch(manager_with_listeners, batch_updates)
  
  // Verify batch update was successful
  assert_true(ConfigurationUpdateResult::is_success(batch_update_result))
  
  // Verify all values were updated
  match DynamicConfigurationManager::get(manager_with_listeners, "telemetry.enabled") {
    Some(value) => assert_eq(value, "false")
    None => assert_true(false)
  }
  
  match DynamicConfigurationManager::get(manager_with_listeners, "telemetry.batch_size") {
    Some(value) => assert_eq(value, "200")
    None => assert_true(false)
  }
  
  match DynamicConfigurationManager::get(manager_with_listeners, "telemetry.new_setting") {
    Some(value) => assert_eq(value, "new_value")
    None => assert_true(false)
  }
  
  // Test conditional updates
  let conditional_result = DynamicConfigurationManager::update_if(
    manager_with_listeners,
    "telemetry.sampling_rate",
    "0.8",
    \current_value => {
      match current_value {
        Some(value) => value == "0.5" // Only update if current value is 0.5
        None => false
      }
    }
  )
  
  // Verify conditional update was successful
  assert_true(ConfigurationUpdateResult::is_success(conditional_result))
  
  match DynamicConfigurationManager::get(manager_with_listeners, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.8")
    None => assert_true(false)
  }
  
  // Test failed conditional update
  let failed_conditional_result = DynamicConfigurationManager::update_if(
    manager_with_listeners,
    "telemetry.sampling_rate",
    "1.0",
    \current_value => {
      match current_value {
        Some(value) => value == "0.5" // Condition is now false
        None => false
      }
    }
  )
  
  // Verify conditional update failed
  assert_false(ConfigurationUpdateResult::is_success(failed_conditional_result))
  
  // Value should remain unchanged
  match DynamicConfigurationManager::get(manager_with_listeners, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.8")
    None => assert_true(false)
  }
}

// Test 4: Configuration Rollback and Versioning
test "configuration rollback and versioning" {
  // Create configuration manager with versioning support
  let config_manager = VersionedConfigurationManager::new()
  
  // Set initial configuration
  let initial_config = [
    ("telemetry.enabled", "true"),
    ("telemetry.sampling_rate", "0.1"),
    ("telemetry.batch_size", "100"),
    ("telemetry.flush_interval_ms", "5000")
  ]
  
  let manager_with_initial = VersionedConfigurationManager::set_configuration(
    config_manager,
    initial_config,
    "Initial configuration"
  )
  
  // Verify initial version
  assert_eq(VersionedConfigurationManager::current_version(manager_with_initial), 1)
  
  // Update configuration (creates version 2)
  let update1 = [
    ("telemetry.sampling_rate", "0.3"),
    ("telemetry.batch_size", "150")
  ]
  
  let manager_after_update1 = VersionedConfigurationManager::update_configuration(
    manager_with_initial,
    update1,
    "Increased sampling rate and batch size"
  )
  
  // Verify version 2
  assert_eq(VersionedConfigurationManager::current_version(manager_after_update1), 2)
  
  match VersionedConfigurationManager::get(manager_after_update1, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.3")
    None => assert_true(false)
  }
  
  // Update configuration again (creates version 3)
  let update2 = [
    ("telemetry.sampling_rate", "0.5"),
    ("telemetry.enabled", "false")
  ]
  
  let manager_after_update2 = VersionedConfigurationManager::update_configuration(
    manager_after_update1,
    update2,
    "Disabled telemetry and increased sampling rate"
  )
  
  // Verify version 3
  assert_eq(VersionedConfigurationManager::current_version(manager_after_update2), 3)
  
  match VersionedConfigurationManager::get(manager_after_update2, "telemetry.enabled") {
    Some(value) => assert_eq(value, "false")
    None => assert_true(false)
  }
  
  // Get version history
  let history = VersionedConfigurationManager::get_version_history(manager_after_update2)
  assert_eq(history.length(), 3)
  
  // Verify version details
  let version1 = history[0]
  assert_eq(VersionInfo::number(version1), 1)
  match VersionInfo::description(version1) {
    Some(desc) => assert_eq(desc, "Initial configuration")
    None => assert_true(false)
  }
  
  let version2 = history[1]
  assert_eq(VersionInfo::number(version2), 2)
  match VersionInfo::description(version2) {
    Some(desc) => assert_eq(desc, "Increased sampling rate and batch size")
    None => assert_true(false)
  }
  
  // Rollback to version 2
  let manager_after_rollback = VersionedConfigurationManager::rollback_to_version(
    manager_after_update2,
    2,
    "Rollback due to performance issues"
  )
  
  // Verify rollback
  assert_eq(VersionedConfigurationManager::current_version(manager_after_rollback), 4) // New version after rollback
  
  match VersionedConfigurationManager::get(manager_after_rollback, "telemetry.enabled") {
    Some(value) => assert_eq(value, "true") // Should be restored to version 2 value
    None => assert_true(false)
  }
  
  match VersionedConfigurationManager::get(manager_after_rollback, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.3") // Should be restored to version 2 value
    None => assert_true(false)
  }
  
  // Get configuration at specific version
  let version3_config = VersionedConfigurationManager::get_configuration_at_version(
    manager_after_rollback,
    3
  )
  
  match VersionedConfigurationManager::get(version3_config, "telemetry.enabled") {
    Some(value) => assert_eq(value, "false") // Version 3 had telemetry disabled
    None => assert_true(false)
  }
  
  // Compare versions
  let diff = VersionedConfigurationManager::compare_versions(
    manager_after_rollback,
    4, // Current version
    2  // Rollback target
  )
  
  // Verify differences
  assert_true(ConfigurationDiff::has_changes(diff))
  let changes = ConfigurationDiff::get_changes(diff)
  assert_eq(changes.length(), 1) // Only telemetry.sampling_rate should be different
}

// Test 5: Configuration Templates and Inheritance
test "configuration templates and inheritance" {
  // Create configuration manager with template support
  let config_manager = TemplateConfigurationManager::new()
  
  // Create base template
  let base_template = [
    ("telemetry.enabled", "true"),
    ("telemetry.sampling_rate", "0.1"),
    ("telemetry.batch_size", "100"),
    ("telemetry.flush_interval_ms", "5000"),
    ("network.timeout_ms", "30000"),
    ("network.max_connections", "10")
  ]
  
  let manager_with_base = TemplateConfigurationManager::create_template(
    config_manager,
    "base",
    base_template
  )
  
  // Create production template that inherits from base
  let production_overrides = [
    ("telemetry.sampling_rate", "0.05"),
    ("telemetry.batch_size", "500"),
    ("network.timeout_ms", "60000"),
    ("environment", "production")
  ]
  
  let manager_with_production = TemplateConfigurationManager::create_template(
    manager_with_base,
    "production",
    production_overrides
  )
  
  // Set production template to inherit from base
  let manager_with_inheritance = TemplateConfigurationManager::set_inheritance(
    manager_with_production,
    "production",
    "base"
  )
  
  // Create development template that inherits from base
  let development_overrides = [
    ("telemetry.sampling_rate", "1.0"),
    ("telemetry.debug_mode", "true"),
    ("environment", "development")
  ]
  
  let manager_with_development = TemplateConfigurationManager::create_template(
    manager_with_inheritance,
    "development",
    development_overrides
  )
  
  // Set development template to inherit from base
  let manager_with_dev_inheritance = TemplateConfigurationManager::set_inheritance(
    manager_with_development,
    "development",
    "base"
  )
  
  // Create configuration from production template
  let production_config = TemplateConfigurationManager::create_from_template(
    manager_with_dev_inheritance,
    "production"
  )
  
  // Verify production configuration has inherited values and overrides
  match TemplateConfigurationManager::get(production_config, "telemetry.enabled") {
    Some(value) => assert_eq(value, "true") // Inherited from base
    None => assert_true(false)
  }
  
  match TemplateConfigurationManager::get(production_config, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.05") // Overridden in production
    None => assert_true(false)
  }
  
  match TemplateConfigurationManager::get(production_config, "environment") {
    Some(value) => assert_eq(value, "production") // Production-specific
    None => assert_true(false)
  }
  
  // Create configuration from development template
  let development_config = TemplateConfigurationManager::create_from_template(
    manager_with_dev_inheritance,
    "development"
  )
  
  // Verify development configuration has inherited values and overrides
  match TemplateConfigurationManager::get(development_config, "telemetry.enabled") {
    Some(value) => assert_eq(value, "true") // Inherited from base
    None => assert_true(false)
  }
  
  match TemplateConfigurationManager::get(development_config, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "1.0") // Overridden in development
    None => assert_true(false)
  }
  
  match TemplateConfigurationManager::get(development_config, "telemetry.debug_mode") {
    Some(value) => assert_eq(value, "true") // Development-specific
    None => assert_true(false)
  }
  
  // Test template hierarchy
  let hierarchy = TemplateConfigurationManager::get_template_hierarchy(
    manager_with_dev_inheritance,
    "production"
  )
  
  assert_eq(hierarchy.length(), 2)
  assert_eq(hierarchy[0], "production")
  assert_eq(hierarchy[1], "base")
  
  // Test template validation
  let template_validation_rules = [
    ("telemetry.sampling_rate", \value => {
      match Float::parse(value) {
        Some(rate) => rate >= 0.0 && rate <= 1.0
        None => false
      }
    }),
    ("environment", \value => value == "production" || value == "development" || value == "staging")
  ]
  
  let validation_result = TemplateConfigurationManager::validate_template(
    manager_with_dev_inheritance,
    "production",
    template_validation_rules
  )
  
  // Verify validation passed
  assert_true(ValidationResult::is_valid(validation_result))
  
  // Test template composition
  let combined_overrides = [
    ("telemetry.batch_size", "1000"),
    ("logging.level", "info")
  ]
  
  let manager_with_composed = TemplateConfigurationManager::compose_templates(
    manager_with_dev_inheritance,
    "composed",
    ["production", "development"],
    combined_overrides
  )
  
  // Create configuration from composed template
  let composed_config = TemplateConfigurationManager::create_from_template(
    manager_with_composed,
    "composed"
  )
  
  // Verify composed configuration
  match TemplateConfigurationManager::get(composed_config, "environment") {
    Some(value) => assert_eq(value, "production") // From production template
    None => assert_true(false)
  }
  
  match TemplateConfigurationManager::get(composed_config, "telemetry.debug_mode") {
    Some(value) => assert_eq(value, "true") // From development template
    None => assert_true(false)
  }
  
  match TemplateConfigurationManager::get(composed_config, "logging.level") {
    Some(value) => assert_eq(value, "info") // From compose overrides
    None => assert_true(false)
  }
}

// Test 6: Environment-Specific Configuration
test "environment specific configuration" {
  // Create environment-aware configuration manager
  let config_manager = EnvironmentConfigurationManager::new()
  
  // Create base configuration
  let base_config = [
    ("telemetry.enabled", "true"),
    ("telemetry.sampling_rate", "0.1"),
    ("telemetry.batch_size", "100"),
    ("service.name", "azimuth-telemetry"),
    ("service.version", "1.0.0")
  ]
  
  let manager_with_base = EnvironmentConfigurationManager::set_base_config(
    config_manager,
    base_config
  )
  
  // Create development environment configuration
  let dev_config = [
    ("telemetry.sampling_rate", "1.0"),
    ("telemetry.debug_mode", "true"),
    ("logging.level", "debug"),
    ("network.timeout_ms", "5000")
  ]
  
  let manager_with_dev = EnvironmentConfigurationManager::set_environment_config(
    manager_with_base,
    "development",
    dev_config
  )
  
  // Create production environment configuration
  let prod_config = [
    ("telemetry.sampling_rate", "0.01"),
    ("telemetry.debug_mode", "false"),
    ("logging.level", "warn"),
    ("network.timeout_ms", "30000"),
    ("security.encryption", "true")
  ]
  
  let manager_with_prod = EnvironmentConfigurationManager::set_environment_config(
    manager_with_dev,
    "production",
    prod_config
  )
  
  // Create staging environment configuration
  let staging_config = [
    ("telemetry.sampling_rate", "0.1"),
    ("telemetry.debug_mode", "false"),
    ("logging.level", "info"),
    ("network.timeout_ms", "15000")
  ]
  
  let manager_with_staging = EnvironmentConfigurationManager::set_environment_config(
    manager_with_prod,
    "staging",
    staging_config
  )
  
  // Get configuration for different environments
  let dev_active_config = EnvironmentConfigurationManager::get_active_config(
    manager_with_staging,
    "development"
  )
  
  // Verify development configuration
  match EnvironmentConfigurationManager::get(dev_active_config, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "1.0") // Development override
    None => assert_true(false)
  }
  
  match EnvironmentConfigurationManager::get(dev_active_config, "telemetry.debug_mode") {
    Some(value) => assert_eq(value, "true") // Development specific
    None => assert_true(false)
  }
  
  match EnvironmentConfigurationManager::get(dev_active_config, "service.name") {
    Some(value) => assert_eq(value, "azimuth-telemetry") // From base
    None => assert_true(false)
  }
  
  let prod_active_config = EnvironmentConfigurationManager::get_active_config(
    manager_with_staging,
    "production"
  )
  
  // Verify production configuration
  match EnvironmentConfigurationManager::get(prod_active_config, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.01") // Production override
    None => assert_true(false)
  }
  
  match EnvironmentConfigurationManager::get(prod_active_config, "security.encryption") {
    Some(value) => assert_eq(value, "true") // Production specific
    None => assert_true(false)
  }
  
  match EnvironmentConfigurationManager::get(prod_active_config, "telemetry.debug_mode") {
    Some(value) => assert_eq(value, "false") // Production override
    None => assert_true(false)
  }
  
  // Test environment detection
  let detected_env = EnvironmentConfigurationManager::detect_environment()
  assert_true(detected_env == "development" || detected_env == "production" || detected_env == "staging")
  
  // Get configuration for detected environment
  let auto_config = EnvironmentConfigurationManager::get_active_config(
    manager_with_staging,
    detected_env
  )
  
  // Verify auto-detected configuration
  match EnvironmentConfigurationManager::get(auto_config, "service.name") {
    Some(value) => assert_eq(value, "azimuth-telemetry")
    None => assert_true(false)
  }
  
  // Test environment-specific validation
  let dev_validation_rules = [
    ("telemetry.sampling_rate", \value => {
      match Float::parse(value) {
        Some(rate) => rate >= 0.5 && rate <= 1.0 // Development allows higher sampling
        None => false
      }
    }),
    ("logging.level", \value => value == "debug" || value == "info")
  ]
  
  let prod_validation_rules = [
    ("telemetry.sampling_rate", \value => {
      match Float::parse(value) {
        Some(rate) => rate >= 0.0 && rate <= 0.1 // Production requires lower sampling
        None => false
      }
    }),
    ("logging.level", \value => value == "warn" || value == "error")
  ]
  
  // Set environment-specific validation rules
  let manager_with_validation = EnvironmentConfigurationManager::set_validation_rules(
    manager_with_staging,
    [
      ("development", dev_validation_rules),
      ("production", prod_validation_rules)
    ]
  )
  
  // Validate development configuration
  let dev_validation_result = EnvironmentConfigurationManager::validate_environment(
    manager_with_validation,
    "development"
  )
  
  assert_true(ValidationResult::is_valid(dev_validation_result))
  
  // Validate production configuration
  let prod_validation_result = EnvironmentConfigurationManager::validate_environment(
    manager_with_validation,
    "production"
  )
  
  assert_true(ValidationResult::is_valid(prod_validation_result))
  
  // Test environment configuration export
  let dev_export = EnvironmentConfigurationManager::export_environment(
    manager_with_validation,
    "development"
  )
  
  assert_true(String::contains(dev_export, "telemetry.sampling_rate=1.0"))
  assert_true(String::contains(dev_export, "telemetry.debug_mode=true"))
  
  // Test environment configuration import
  let import_config = "telemetry.sampling_rate=0.8\n"
    + "telemetry.batch_size=200\n"
    + "logging.level=trace\n"
  
  let manager_with_import = EnvironmentConfigurationManager::import_environment_config(
    manager_with_validation,
    "development",
    import_config
  )
  
  // Verify import
  let updated_dev_config = EnvironmentConfigurationManager::get_active_config(
    manager_with_import,
    "development"
  )
  
  match EnvironmentConfigurationManager::get(updated_dev_config, "telemetry.sampling_rate") {
    Some(value) => assert_eq(value, "0.8") // Imported value
    None => assert_true(false)
  }
  
  match EnvironmentConfigurationManager::get(updated_dev_config, "logging.level") {
    Some(value) => assert_eq(value, "trace") // Imported value
    None => assert_true(false)
  }
}