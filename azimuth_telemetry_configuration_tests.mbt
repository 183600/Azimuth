// Azimuth Telemetry Configuration Test Suite
// This file contains comprehensive test cases for telemetry system configuration management

// Test 1: Dynamic Configuration Updates
test "dynamic configuration updates for telemetry system" {
  // Define configuration structure
  type TelemetryConfig = {
    sampling_rate: Float,
    batch_size: Int,
    flush_interval: Int,
    export_timeout: Int,
    max_spans_per_trace: Int,
    tags: Array<(String, String)>
  }
  
  // Define configuration change
  type ConfigChange = {
    field: String,
    old_value: String,
    new_value: String,
    timestamp: Int,
    applied_by: String
  }
  
  // Define configuration manager
  type ConfigManager = {
    current_config: TelemetryConfig,
    default_config: TelemetryConfig,
    change_history: Array<ConfigChange>,
    validation_rules: Array<(String, (String) -> Bool)>
  }
  
  // Create default configuration
  let create_default_config = fn() {
    {
      sampling_rate: 0.1,
      batch_size: 100,
      flush_interval: 5000,
      export_timeout: 30000,
      max_spans_per_trace: 1000,
      tags: [("service", "telemetry"), ("version", "1.0.0")]
    }
  }
  
  // Create configuration manager
  let create_config_manager = fn() {
    let default_config = create_default_config()
    
    let validation_rules = [
      ("sampling_rate", fn(value: String) {
        let rate = value.to_float()
        rate >= 0.0 && rate <= 1.0
      }),
      ("batch_size", fn(value: String) {
        let size = value.to_int()
        size > 0 && size <= 10000
      }),
      ("flush_interval", fn(value: String) {
        let interval = value.to_int()
        interval > 0 && interval <= 300000  // Max 5 minutes
      }),
      ("export_timeout", fn(value: String) {
        let timeout = value.to_int()
        timeout > 0 && timeout <= 300000  // Max 5 minutes
      }),
      ("max_spans_per_trace", fn(value: String) {
        let max_spans = value.to_int()
        max_spans > 0 && max_spans <= 10000
      })
    ]
    
    {
      current_config: default_config,
      default_config,
      change_history: [],
      validation_rules
    }
  }
  
  // Validate configuration change
  let validate_config_change = fn(manager: ConfigManager, field: String, value: String) {
    match manager.validation_rules.find(fn(rule) { rule.0 == field }) {
      Some((_, validator)) => validator(value)
      None => true  // No validation rule means it's valid
    }
  }
  
  // Apply configuration change
  let apply_config_change = fn(manager: ConfigManager, field: String, value: String, applied_by: String, current_time: Int) {
    // Validate the change
    if not(validate_config_change(manager, field, value)) {
      return {
        current_config: manager.current_config,
        default_config: manager.default_config,
        change_history: manager.change_history.push({
          field,
          old_value: "INVALID",
          new_value: value,
          timestamp: current_time,
          applied_by
        }),
        validation_rules: manager.validation_rules
      }
    }
    
    // Get old value
    let old_value = match field {
      "sampling_rate" => manager.current_config.sampling_rate.to_string()
      "batch_size" => manager.current_config.batch_size.to_string()
      "flush_interval" => manager.current_config.flush_interval.to_string()
      "export_timeout" => manager.current_config.export_timeout.to_string()
      "max_spans_per_trace" => manager.current_config.max_spans_per_trace.to_string()
      _ => "unknown"
    }
    
    // Apply the change
    let updated_config = match field {
      "sampling_rate" => {
        { manager.current_config | sampling_rate: value.to_float() }
      }
      "batch_size" => {
        { manager.current_config | batch_size: value.to_int() }
      }
      "flush_interval" => {
        { manager.current_config | flush_interval: value.to_int() }
      }
      "export_timeout" => {
        { manager.current_config | export_timeout: value.to_int() }
      }
      "max_spans_per_trace" => {
        { manager.current_config | max_spans_per_trace: value.to_int() }
      }
      _ => {
        manager.current_config  // No change for unknown field
      }
    }
    
    // Add to change history
    let change = {
      field,
      old_value,
      new_value: value,
      timestamp: current_time,
      applied_by
    }
    
    {
      current_config: updated_config,
      default_config: manager.default_config,
      change_history: manager.change_history.push(change),
      validation_rules: manager.validation_rules
    }
  }
  
  // Test configuration manager
  let config_manager = create_config_manager()
  
  // Verify initial configuration
  assert_eq(config_manager.current_config.sampling_rate, 0.1)
  assert_eq(config_manager.current_config.batch_size, 100)
  assert_eq(config_manager.current_config.flush_interval, 5000)
  assert_eq(config_manager.change_history.length(), 0)
  
  // Apply valid configuration changes
  let manager1 = apply_config_change(config_manager, "sampling_rate", "0.2", "admin", 1640995200)
  assert_eq(manager1.current_config.sampling_rate, 0.2)
  assert_eq(manager1.change_history.length(), 1)
  assert_eq(manager1.change_history[0].field, "sampling_rate")
  assert_eq(manager1.change_history[0].old_value, "0.1")
  assert_eq(manager1.change_history[0].new_value, "0.2")
  
  let manager2 = apply_config_change(manager1, "batch_size", "200", "admin", 1640995210)
  assert_eq(manager2.current_config.batch_size, 200)
  assert_eq(manager2.change_history.length(), 2)
  
  // Apply invalid configuration change
  let manager3 = apply_config_change(manager2, "sampling_rate", "1.5", "admin", 1640995220)
  assert_eq(manager3.current_config.sampling_rate, 0.2)  // Should remain unchanged
  assert_eq(manager3.change_history.length(), 3)
  assert_eq(manager3.change_history[2].old_value, "INVALID")  // Invalid change marked
  
  // Test configuration rollback
  let rollback_config = fn(manager: ConfigManager, target_timestamp: Int) {
    // Find the state of config at the target timestamp
    let changes_before_target = manager.change_history.filter(fn(change) {
      change.timestamp <= target_timestamp
    })
    
    // Start with default config and apply changes up to target
    let mut rollback_config = manager.default_config
    
    for change in changes_before_target {
      if change.old_value != "INVALID" {  // Skip invalid changes
        rollback_config = match change.field {
          "sampling_rate" => {
            { rollback_config | sampling_rate: change.new_value.to_float() }
          }
          "batch_size" => {
            { rollback_config | batch_size: change.new_value.to_int() }
          }
          "flush_interval" => {
            { rollback_config | flush_interval: change.new_value.to_int() }
          }
          "export_timeout" => {
            { rollback_config | export_timeout: change.new_value.to_int() }
          }
          "max_spans_per_trace" => {
            { rollback_config | max_spans_per_trace: change.new_value.to_int() }
          }
          _ => rollback_config
        }
      }
    }
    
    rollback_config
  }
  
  // Test rollback to previous state
  let rolled_back_config = rollback_config(manager3, 1640995210)
  assert_eq(rolled_back_config.sampling_rate, 0.2)  // Should have the change at 1640995200
  assert_eq(rolled_back_config.batch_size, 200)    // Should have the change at 1640995210
  
  // Test configuration comparison
  let compare_configs = fn(config1: TelemetryConfig, config2: TelemetryConfig) {
    let differences = []
    
    if config1.sampling_rate != config2.sampling_rate {
      differences = differences.push(("sampling_rate", config1.sampling_rate.to_string(), config2.sampling_rate.to_string()))
    }
    
    if config1.batch_size != config2.batch_size {
      differences = differences.push(("batch_size", config1.batch_size.to_string(), config2.batch_size.to_string()))
    }
    
    if config1.flush_interval != config2.flush_interval {
      differences = differences.push(("flush_interval", config1.flush_interval.to_string(), config2.flush_interval.to_string()))
    }
    
    if config1.export_timeout != config2.export_timeout {
      differences = differences.push(("export_timeout", config1.export_timeout.to_string(), config2.export_timeout.to_string()))
    }
    
    if config1.max_spans_per_trace != config2.max_spans_per_trace {
      differences = differences.push(("max_spans_per_trace", config1.max_spans_per_trace.to_string(), config2.max_spans_per_trace.to_string()))
    }
    
    differences
  }
  
  let differences = compare_configs(manager3.current_config, manager3.default_config)
  assert_eq(differences.length(), 2)  // sampling_rate and batch_size changed
  assert_true(differences.any(fn(d) { d.0 == "sampling_rate" }))
  assert_true(differences.any(fn(d) { d.0 == "batch_size" }))
}

// Test 2: Environment-Specific Configuration
test "environment-specific configuration management" {
  // Define environment types
  enum Environment {
    Development
    Testing
    Staging
    Production
  }
  
  // Define environment-specific configuration
  type EnvironmentConfig = {
    environment: Environment,
    telemetry_config: TelemetryConfig,
    feature_flags: Array<(String, Bool)>
  }
  
  // Define TelemetryConfig for this test
  type TelemetryConfig = {
    sampling_rate: Float,
    batch_size: Int,
    flush_interval: Int,
    debug_enabled: Bool,
    export_enabled: Bool
  }
  
  // Create environment-specific configurations
  let create_environment_configs = fn() {
    [
      {
        environment: Environment::Development,
        telemetry_config: {
          sampling_rate: 1.0,    // Sample everything in development
          batch_size: 10,        // Small batches for immediate feedback
          flush_interval: 1000,  // Flush frequently
          debug_enabled: true,   // Enable debug
          export_enabled: false  // Don't export in development
        },
        feature_flags: [
          ("verbose_logging", true),
          ("local_export", true),
          ("real_time_metrics", true)
        ]
      },
      {
        environment: Environment::Testing,
        telemetry_config: {
          sampling_rate: 1.0,    // Sample everything in testing
          batch_size: 50,        // Medium batches
          flush_interval: 5000,  // Standard flush interval
          debug_enabled: true,   // Enable debug for testing
          export_enabled: false  // Don't export in testing
        },
        feature_flags: [
          ("verbose_logging", true),
          ("local_export", false),
          ("real_time_metrics", false)
        ]
      },
      {
        environment: Environment::Staging,
        telemetry_config: {
          sampling_rate: 0.5,    // Sample half in staging
          batch_size: 100,       // Standard batches
          flush_interval: 10000, // Longer flush interval
          debug_enabled: false,  // Disable debug in staging
          export_enabled: true   // Enable export in staging
        },
        feature_flags: [
          ("verbose_logging", false),
          ("local_export", false),
          ("real_time_metrics", true)
        ]
      },
      {
        environment: Environment::Production,
        telemetry_config: {
          sampling_rate: 0.1,    // Sample 10% in production
          batch_size: 500,       // Large batches for efficiency
          flush_interval: 30000, // Longest flush interval
          debug_enabled: false,  // Disable debug in production
          export_enabled: true   // Enable export in production
        },
        feature_flags: [
          ("verbose_logging", false),
          ("local_export", false),
          ("real_time_metrics", false)
        ]
      }
    ]
  }
  
  // Get configuration for environment
  let get_config_for_environment = fn(environment: Environment, configs: Array<EnvironmentConfig>) {
    match configs.find(fn(config) { config.environment == environment }) {
      Some(config) => config
      None => {
        // Return production config as fallback
        configs.find(fn(config) { config.environment == Environment::Production }).unwrap_or(configs[0])
      }
    }
  }
  
  // Test environment-specific configurations
  let env_configs = create_environment_configs()
  
  // Get development configuration
  let dev_config = get_config_for_environment(Environment::Development, env_configs)
  assert_eq(dev_config.telemetry_config.sampling_rate, 1.0)
  assert_eq(dev_config.telemetry_config.batch_size, 10)
  assert_eq(dev_config.telemetry_config.flush_interval, 1000)
  assert_true(dev_config.telemetry_config.debug_enabled)
  assert_false(dev_config.telemetry_config.export_enabled)
  assert_true(dev_config.feature_flags.any(fn(flag) { flag.0 == "verbose_logging" && flag.1 }))
  
  // Get production configuration
  let prod_config = get_config_for_environment(Environment::Production, env_configs)
  assert_eq(prod_config.telemetry_config.sampling_rate, 0.1)
  assert_eq(prod_config.telemetry_config.batch_size, 500)
  assert_eq(prod_config.telemetry_config.flush_interval, 30000)
  assert_false(prod_config.telemetry_config.debug_enabled)
  assert_true(prod_config.telemetry_config.export_enabled)
  assert_false(prod_config.feature_flags.any(fn(flag) { flag.0 == "verbose_logging" && flag.1 }))
  
  // Test configuration inheritance
  let inherit_config = fn(base_config: EnvironmentConfig, overrides: Array<(String, String)>) {
    let mut updated_config = base_config.telemetry_config
    
    for (field, value) in overrides {
      updated_config = match field {
        "sampling_rate" => { updated_config | sampling_rate: value.to_float() }
        "batch_size" => { updated_config | batch_size: value.to_int() }
        "flush_interval" => { updated_config | flush_interval: value.to_int() }
        "debug_enabled" => { updated_config | debug_enabled: value == "true" }
        "export_enabled" => { updated_config | export_enabled: value == "true" }
        _ => updated_config
      }
    }
    
    {
      environment: base_config.environment,
      telemetry_config: updated_config,
      feature_flags: base_config.feature_flags
    }
  }
  
  // Test inheritance with staging config and overrides
  let staging_config = get_config_for_environment(Environment::Staging, env_configs)
  let inherited_config = inherit_config(staging_config, [
    ("sampling_rate", "0.8"),  // Override sampling rate
    ("debug_enabled", "true")  // Enable debug for troubleshooting
  ])
  
  assert_eq(inherited_config.telemetry_config.sampling_rate, 0.8)  // Overridden
  assert_eq(inherited_config.telemetry_config.batch_size, 100)     // Inherited
  assert_true(inherited_config.telemetry_config.debug_enabled)     // Overridden
  assert_true(inherited_config.telemetry_config.export_enabled)   // Inherited
  
  // Test configuration validation for different environments
  let validate_config_for_environment = fn(config: EnvironmentConfig) {
    let mut errors = []
    
    match config.environment {
      Environment::Development => {
        if config.telemetry_config.sampling_rate < 0.5 {
          errors = errors.push("Development should have high sampling rate for debugging")
        }
        if not(config.telemetry_config.debug_enabled) {
          errors = errors.push("Debug should be enabled in development")
        }
      }
      Environment::Testing => {
        if config.telemetry_config.sampling_rate < 0.5 {
          errors = errors.push("Testing should have high sampling rate for comprehensive testing")
        }
      }
      Environment::Staging => {
        if config.telemetry_config.sampling_rate > 0.8 {
          errors = errors.push("Staging should not have too high sampling rate")
        }
        if not(config.telemetry_config.export_enabled) {
          errors = errors.push("Export should be enabled in staging")
        }
      }
      Environment::Production => {
        if config.telemetry_config.sampling_rate > 0.2 {
          errors = errors.push("Production should have low sampling rate for performance")
        }
        if config.telemetry_config.debug_enabled {
          errors = errors.push("Debug should be disabled in production")
        }
        if not(config.telemetry_config.export_enabled) {
          errors = errors.push("Export should be enabled in production")
        }
      }
    }
    
    errors
  }
  
  // Test validation
  let dev_validation = validate_config_for_environment(dev_config)
  assert_eq(dev_validation.length(), 0)  // Development config should be valid
  
  let prod_validation = validate_config_for_environment(prod_config)
  assert_eq(prod_validation.length(), 0)  // Production config should be valid
  
  // Test invalid config for production
  let invalid_prod_config = {
    environment: Environment::Production,
    telemetry_config: {
      sampling_rate: 0.5,    // Too high for production
      batch_size: 500,
      flush_interval: 30000,
      debug_enabled: true,   // Should be disabled in production
      export_enabled: true
    },
    feature_flags: []
  }
  
  let invalid_prod_validation = validate_config_for_environment(invalid_prod_config)
  assert_eq(invalid_prod_validation.length(), 2)  // Two validation errors
}

// Test 3: Configuration Schema Validation
test "configuration schema validation and type checking" {
  // Define configuration schema
  type SchemaField = {
    name: String,
    type: String,
    required: Bool,
    default_value: Option<String>,
    min_value: Option<Float>,
    max_value: Option<Float>,
    allowed_values: Option<Array<String>>
  }
  
  type ConfigSchema = {
    version: String,
    fields: Array<SchemaField>
  }
  
  // Define validation error
  type ValidationError = {
    field: String,
    message: String,
    value: String
  }
  
  // Create telemetry configuration schema
  let create_telemetry_schema = fn() {
    {
      version: "1.0",
      fields: [
        {
          name: "sampling_rate",
          type: "float",
          required: true,
          default_value: Some("0.1"),
          min_value: Some(0.0),
          max_value: Some(1.0),
          allowed_values: None
        },
        {
          name: "batch_size",
          type: "int",
          required: true,
          default_value: Some("100"),
          min_value: Some(1),
          max_value: Some(10000),
          allowed_values: None
        },
        {
          name: "flush_interval",
          type: "int",
          required: true,
          default_value: Some("5000"),
          min_value: Some(100),
          max_value: Some(300000),
          allowed_values: None
        },
        {
          name: "export_format",
          type: "string",
          required: false,
          default_value: Some("json"),
          min_value: None,
          max_value: None,
          allowed_values: Some(["json", "protobuf", "csv"])
        },
        {
          name: "debug_enabled",
          type: "boolean",
          required: false,
          default_value: Some("false"),
          min_value: None,
          max_value: None,
          allowed_values: None
        }
      ]
    }
  }
  
  // Validate configuration against schema
  let validate_config_against_schema = fn(config: Array<(String, String)>, schema: ConfigSchema) {
    let mut errors = []
    
    // Check required fields
    for field in schema.fields {
      if field.required {
        match config.find(fn(item) { item.0 == field.name }) {
          Some(_) => {}  // Field exists
          None => {
            errors = errors.push({
              field: field.name,
              message: "Required field is missing",
              value: ""
            })
          }
        }
      }
    }
    
    // Validate field values
    for (field_name, field_value) in config {
      match schema.fields.find(fn(f) { f.name == field_name }) {
        Some(field) => {
          // Type validation
          let type_valid = match field.type {
            "float" => {
              match field_value.to_float() {
                _ => true  // Successfully parsed as float
                _ => false  // Failed to parse as float
              }
            }
            "int" => {
              match field_value.to_int() {
                _ => true  // Successfully parsed as int
                _ => false  // Failed to parse as int
              }
            }
            "boolean" => {
              field_value == "true" || field_value == "false"
            }
            "string" => {
              field_value.length() > 0
            }
            _ => false  // Unknown type
          }
          
          if not(type_valid) {
            errors = errors.push({
              field: field_name,
              message: "Invalid type for field (expected: " + field.type + ")",
              value: field_value
            })
          } else {
            // Range validation
            match field.type {
              "float" => {
                let value = field_value.to_float()
                match field.min_value {
                  Some(min) => {
                    if value < min {
                      errors = errors.push({
                        field: field_name,
                        message: "Value below minimum (" + min.to_string() + ")",
                        value: field_value
                      })
                    }
                  }
                  None => {}
                }
                
                match field.max_value {
                  Some(max) => {
                    if value > max {
                      errors = errors.push({
                        field: field_name,
                        message: "Value above maximum (" + max.to_string() + ")",
                        value: field_value
                      })
                    }
                  }
                  None => {}
                }
              }
              "int" => {
                let value = field_value.to_int()
                match field.min_value {
                  Some(min) => {
                    if value < min as Int {
                      errors = errors.push({
                        field: field_name,
                        message: "Value below minimum (" + min.to_string() + ")",
                        value: field_value
                      })
                    }
                  }
                  None => {}
                }
                
                match field.max_value {
                  Some(max) => {
                    if value > max as Int {
                      errors = errors.push({
                        field: field_name,
                        message: "Value above maximum (" + max.to_string() + ")",
                        value: field_value
                      })
                    }
                  }
                  None => {}
                }
              }
              _ => {}
            }
            
            // Allowed values validation
            match field.allowed_values {
              Some(allowed) => {
                if not(allowed.contains(field_value)) {
                  errors = errors.push({
                    field: field_name,
                    message: "Value not in allowed list: " + allowed.join(", "),
                    value: field_value
                  })
                }
              }
              None => {}
            }
          }
        }
        None => {
          errors = errors.push({
            field: field_name,
            message: "Unknown field",
            value: field_value
          })
        }
      }
    }
    
    errors
  }
  
  // Apply default values for missing fields
  let apply_defaults = fn(config: Array<(String, String)>, schema: ConfigSchema) {
    let mut result = config
    
    for field in schema.fields {
      if not(field.required) && not(config.any(fn(item) { item.0 == field.name })) {
        match field.default_value {
          Some(default_value) => {
            result = result.push((field.name, default_value))
          }
          None => {}
        }
      }
    }
    
    result
  }
  
  // Test schema validation
  let schema = create_telemetry_schema()
  
  // Test valid configuration
  let valid_config = [
    ("sampling_rate", "0.2"),
    ("batch_size", "200"),
    ("flush_interval", "10000"),
    ("export_format", "json"),
    ("debug_enabled", "true")
  ]
  
  let valid_errors = validate_config_against_schema(valid_config, schema)
  assert_eq(valid_errors.length(), 0)  // Should be no errors
  
  // Test invalid configuration
  let invalid_config = [
    ("sampling_rate", "1.5"),      // Above max value
    ("batch_size", "0"),           // Below min value
    ("flush_interval", "50000"),   // Valid
    ("export_format", "xml"),      // Not in allowed values
    ("debug_enabled", "yes"),      // Invalid boolean value
    ("unknown_field", "value")     // Unknown field
  ]
  
  let invalid_errors = validate_config_against_schema(invalid_config, schema)
  assert_eq(invalid_errors.length(), 5)  // Should have 5 errors
  
  assert_true(invalid_errors.any(fn(e) { e.field == "sampling_rate" && e.message.contains("maximum") }))
  assert_true(invalid_errors.any(fn(e) { e.field == "batch_size" && e.message.contains("minimum") }))
  assert_true(invalid_errors.any(fn(e) { e.field == "export_format" && e.message.contains("allowed list") }))
  assert_true(invalid_errors.any(fn(e) { e.field == "debug_enabled" && e.message.contains("type") }))
  assert_true(invalid_errors.any(fn(e) { e.field == "unknown_field" && e.message.contains("Unknown") }))
  
  // Test missing required fields
  let missing_fields_config = [
    ("batch_size", "100"),
    ("flush_interval", "5000")
  ]
  
  let missing_fields_errors = validate_config_against_schema(missing_fields_config, schema)
  assert_eq(missing_fields_errors.length(), 1)  // sampling_rate is required
  assert_eq(missing_fields_errors[0].field, "sampling_rate")
  assert_eq(missing_fields_errors[0].message, "Required field is missing")
  
  // Test default value application
  let config_with_defaults = apply_defaults(missing_fields_config, schema)
  assert_eq(config_with_defaults.length(), 4)  // Original 2 + 2 defaults
  assert_true(config_with_defaults.any(fn(item) { item.0 == "export_format" && item.1 == "json" }))
  assert_true(config_with_defaults.any(fn(item) { item.0 == "debug_enabled" && item.1 == "false" }))
  
  // Test schema versioning
  let migrate_config = fn(config: Array<(String, String)>, from_version: String, to_version: String) {
    if from_version == "1.0" && to_version == "2.0" {
      // Example migration: rename field and change default
      let mut migrated = []
      
      for (field_name, field_value) in config {
        let new_field = match field_name {
          "batch_size" => ("buffer_size", field_value)  // Renamed field
          _ => (field_name, field_value)               // Unchanged
        }
        migrated = migrated.push(new_field)
      }
      
      // Add new field with default
      migrated = migrated.push(("compression", "gzip"))
      
      migrated
    } else {
      config  // No migration needed
    }
  }
  
  let old_config = [
    ("sampling_rate", "0.1"),
    ("batch_size", "100")
  ]
  
  let migrated_config = migrate_config(old_config, "1.0", "2.0")
  assert_eq(migrated_config.length(), 3)  // Original 2 + 1 new
  assert_true(migrated_config.any(fn(item) { item.0 == "buffer_size" && item.1 == "100" }))
  assert_true(migrated_config.any(fn(item) { item.0 == "compression" && item.1 == "gzip" }))
}

// Test 4: Configuration Templates and Profiles
test "configuration templates and profiles management" {
  // Define configuration profile
  type ConfigProfile = {
    name: String,
    description: String,
    config: Array<(String, String)>,
    tags: Array<String>
  }
  
  // Define configuration template
  type ConfigTemplate = {
    name: String,
    description: String,
    variables: Array<(String, String)>,  // (variable_name, default_value)
    config_template: Array<(String, String)>  // Config with variable placeholders
  }
  
  // Define rendered configuration
  type RenderedConfig = {
    profile_name: String,
    config: Array<(String, String)>,
    variables_used: Array<(String, String)>
  }
  
  // Create configuration profiles
  let create_config_profiles = fn() {
    [
      {
        name: "high-throughput",
        description: "Optimized for high throughput scenarios",
        config: [
          ("sampling_rate", "0.05"),
          ("batch_size", "1000"),
          ("flush_interval", "60000"),
          ("compression", "lz4"),
          ("async_export", "true")
        ],
        tags: ["performance", "production"]
      },
      {
        name: "low-latency",
        description: "Optimized for low latency scenarios",
        config: [
          ("sampling_rate", "0.8"),
          ("batch_size", "10"),
          ("flush_interval", "100"),
          ("compression", "none"),
          ("async_export", "false")
        ],
        tags: ["latency", "real-time"]
      },
      {
        name: "debugging",
        description: "Optimized for debugging and development",
        config: [
          ("sampling_rate", "1.0"),
          ("batch_size", "1"),
          ("flush_interval", "10"),
          ("debug_enabled", "true"),
          ("verbose_logging", "true")
        ],
        tags: ["development", "debugging"]
      },
      {
        name: "cost-optimized",
        description: "Optimized to minimize costs",
        config: [
          ("sampling_rate", "0.01"),
          ("batch_size", "500"),
          ("flush_interval", "300000"),
          ("compression", "gzip"),
          ("export_only_on_failure", "true")
        ],
        tags: ["cost", "efficiency"]
      }
    ]
  }
  
  // Create configuration templates
  let create_config_templates = fn() {
    [
      {
        name: "custom-service",
        description: "Template for custom service configuration",
        variables: [
          ("service_name", "unknown-service"),
          ("sampling_rate", "0.1"),
          ("batch_size", "100"),
          ("environment", "production")
        ],
        config_template: [
          ("service.name", "${service_name}"),
          ("sampling_rate", "${sampling_rate}"),
          ("batch_size", "${batch_size}"),
          ("environment", "${environment}"),
          ("export.enabled", "true"),
          ("debug.enabled", "false")
        ]
      },
      {
        name: "microservice",
        description: "Template for microservice configuration",
        variables: [
          ("service_name", "microservice"),
          ("instance_count", "1"),
          ("region", "us-west-2")
        ],
        config_template: [
          ("service.name", "${service_name}"),
          ("instance.count", "${instance_count}"),
          ("region", "${region}"),
          ("sampling_rate", "0.1"),
          ("batch_size", "50"),
          ("discovery.enabled", "true")
        ]
      }
    ]
  }
  
  // Get profile by name
  let get_profile_by_name = fn(profiles: Array<ConfigProfile>, name: String) {
    match profiles.find(fn(profile) { profile.name == name }) {
      Some(profile) => Some(profile)
      None => None
    }
  }
  
  // Render template with variables
  let render_template = fn(template: ConfigTemplate, variables: Array<(String, String)>) {
    let mut rendered_config = []
    let mut variables_used = []
    
    for (key, value_with_placeholder) in template.config_template {
      let mut rendered_value = value_with_placeholder
      
      // Replace placeholders with actual values
      for (var_name, var_value) in template.variables {
        let placeholder = "${" + var_name + "}"
        
        // Check if placeholder exists in value
        if rendered_value.contains(placeholder) {
          // Find the variable value (use provided value or default)
          let actual_value = match variables.find(fn(v) { v.0 == var_name }) {
            Some((_, value)) => value
            None => var_value
          }
          
          rendered_value = rendered_value.replace(placeholder, actual_value)
          
          // Track variable usage
          if not(variables_used.any(fn(v) { v.0 == var_name })) {
            variables_used = variables_used.push((var_name, actual_value))
          }
        }
      }
      
      rendered_config = rendered_config.push((key, rendered_value))
    }
    
    {
      profile_name: template.name,
      config: rendered_config,
      variables_used
    }
  }
  
  // Test configuration profiles
  let profiles = create_config_profiles()
  
  // Get high-throughput profile
  let high_throughput_profile = get_profile_by_name(profiles, "high-throughput")
  assert_true(high_throughput_profile.is_some())
  
  match high_throughput_profile {
    Some(profile) => {
      assert_eq(profile.name, "high-throughput")
      assert_eq(profile.config.length(), 5)
      assert_eq(profile.config[0], ("sampling_rate", "0.05"))
      assert_eq(profile.config[1], ("batch_size", "1000"))
      assert_true(profile.tags.contains("performance"))
    }
    None => assert_true(false)
  }
  
  // Get low-latency profile
  let low_latency_profile = get_profile_by_name(profiles, "low-latency")
  assert_true(low_latency_profile.is_some())
  
  match low_latency_profile {
    Some(profile) => {
      assert_eq(profile.name, "low-latency")
      assert_eq(profile.config[2], ("flush_interval", "100"))
      assert_eq(profile.config[4], ("async_export", "false"))
    }
    None => assert_true(false)
  }
  
  // Test configuration templates
  let templates = create_config_templates()
  
  // Render custom-service template with variables
  let custom_service_template = templates[0]
  let custom_variables = [
    ("service_name", "payment-service"),
    ("sampling_rate", "0.2"),
    ("environment", "staging")
  ]
  
  let rendered_config = render_template(custom_service_template, custom_variables)
  
  assert_eq(rendered_config.profile_name, "custom-service")
  assert_eq(rendered_config.config.length(), 6)
  assert_eq(rendered_config.config[0], ("service.name", "payment-service"))
  assert_eq(rendered_config.config[1], ("sampling_rate", "0.2"))
  assert_eq(rendered_config.config[2], ("batch_size", "100"))  // Used default value
  assert_eq(rendered_config.config[3], ("environment", "staging"))
  
  assert_eq(rendered_config.variables_used.length(), 3)
  assert_true(rendered_config.variables_used.any(fn(v) { v.0 == "service_name" && v.1 == "payment-service" }))
  
  // Render microservice template with all variables
  let microservice_template = templates[1]
  let microservice_variables = [
    ("service_name", "user-service"),
    ("instance_count", "5"),
    ("region", "eu-west-1")
  ]
  
  let microservice_rendered = render_template(microservice_template, microservice_variables)
  
  assert_eq(microservice_rendered.config[0], ("service.name", "user-service"))
  assert_eq(microservice_rendered.config[1], ("instance.count", "5"))
  assert_eq(microservice_rendered.config[2], ("region", "eu-west-1"))
  
  // Test profile merging
  let merge_profiles = fn(base_profile: ConfigProfile, override_profile: ConfigProfile) {
    let mut merged_config = base_profile.config
    
    // Apply overrides
    for (key, value) in override_profile.config {
      let existing_index = merged_config.index_of(fn(item) { item.0 == key })
      
      match existing_index {
        Some(index) => {
          // Override existing value
          merged_config = merged_config.update(index, (key, value))
        }
        None => {
          // Add new key-value pair
          merged_config = merged_config.push((key, value))
        }
      }
    }
    
    {
      name: base_profile.name + "+" + override_profile.name,
      description: "Merged profile: " + base_profile.description + " + " + override_profile.description,
      config: merged_config,
      tags: base_profile.tags + override_profile.tags
    }
  }
  
  // Merge high-throughput with debugging profile
  let high_throughput = profiles[0]
  let debugging = profiles[2]
  
  let merged_profile = merge_profiles(high_throughput, debugging)
  
  assert_eq(merged_profile.name, "high-throughput+debugging")
  assert_eq(merged_profile.config.length(), 7)  // 5 + 2 new from debugging
  
  // Check that debugging values override high-throughput values
  assert_true(merged_profile.config.any(fn(item) { item.0 == "sampling_rate" && item.1 == "1.0" }))
  assert_true(merged_profile.config.any(fn(item) { item.0 == "debug_enabled" && item.1 == "true" }))
  
  // Check that high-throughput values are preserved where not overridden
  assert_true(merged_profile.config.any(fn(item) { item.0 == "batch_size" && item.1 == "1000" }))
  assert_true(merged_profile.config.any(fn(item) { item.0 == "compression" && item.1 == "lz4" }))
  
  // Test profile comparison
  let compare_profiles = fn(profile1: ConfigProfile, profile2: ConfigProfile) {
    let common_keys = []
    let different_keys = []
    let unique_to_profile1 = []
    let unique_to_profile2 = []
    
    // Find keys in profile1
    for (key, value) in profile1.config {
      match profile2.config.find_fn(item) { item.0 == key }) {
        Some((_, value2)) => {
          if value == value2 {
            common_keys = common_keys.push(key)
          } else {
            different_keys = different_keys.push((key, value, value2))
          }
        }
        None => {
          unique_to_profile1 = unique_to_profile1.push((key, value))
        }
      }
    }
    
    // Find keys unique to profile2
    for (key, value) in profile2.config {
      if not(profile1.config.any_fn(item) { item.0 == key }) {
        unique_to_profile2 = unique_to_profile2.push((key, value))
      }
    }
    
    {
      common_keys,
      different_keys,
      unique_to_profile1,
      unique_to_profile2
    }
  }
  
  // Compare high-throughput and low-latency profiles
  let comparison = compare_profiles(high_throughput, low_latency.unwrap())
  
  assert_eq(comparison.common_keys.length(), 0)  // No common keys with same values
  assert_eq(comparison.different_keys.length(), 5)  // All keys differ
  assert_eq(comparison.unique_to_profile1.length(), 0)  // No unique keys
  assert_eq(comparison.unique_to_profile2.length(), 0)  // No unique keys
  
  // Verify some differences
  assert_true(comparison.different_keys.any_fn(d) { 
    d.0 == "sampling_rate" && d.1 == "0.05" && d.2 == "0.8" 
  }))
  assert_true(comparison.different_keys.any_fn(d) { 
    d.0 == "batch_size" && d.1 == "1000" && d.2 == "10" 
  }))
}

// Test 5: Configuration Backup and Restore
test "configuration backup and restore functionality" {
  // Define configuration backup
  type ConfigBackup = {
    id: String,
    timestamp: Int,
    config: Array<(String, String)>,
    description: String,
    created_by: String
  }
  
  // Define backup manager
  type BackupManager = {
    backups: Array<ConfigBackup>,
    max_backups: Int,
    backup_interval: Int,
    last_backup_time: Int
  }
  
  // Create backup manager
  let create_backup_manager = fn(max_backups: Int, backup_interval: Int) {
    {
      backups: [],
      max_backups,
      backup_interval,
      last_backup_time: 0
    }
  }
  
  // Create backup
  let create_backup = fn(config: Array<(String, String)>, description: String, created_by: String, current_time: Int) {
    {
      id: "backup-" + current_time.to_string(),
      timestamp: current_time,
      config,
      description,
      created_by
    }
  }
  
  // Add backup to manager
  let add_backup = fn(manager: BackupManager, backup: ConfigBackup) {
    let mut updated_backups = manager.backups.push(backup)
    
    // Sort backups by timestamp (newest first)
    updated_backups = updated_backups.sort_fn(a, b) {
      if a.timestamp > b.timestamp { -1 }
      else if a.timestamp < b.timestamp { 1 }
      else { 0 }
    }
    
    // Trim if exceeding max_backups
    if updated_backups.length() > manager.max_backups {
      updated_backups = updated_backups.slice(0, manager.max_backups)
    }
    
    {
      backups: updated_backups,
      max_backups: manager.max_backups,
      backup_interval: manager.backup_interval,
      last_backup_time: backup.timestamp
    }
  }
  
  // Get backup by ID
  let get_backup_by_id = fn(manager: BackupManager, backup_id: String) {
    match manager.backups.find_fn(backup) { backup.id == backup_id } {
      Some(backup) => Some(backup)
      None => None
    }
  }
  
  // Restore from backup
  let restore_from_backup = fn(manager: BackupManager, backup_id: String) {
    match get_backup_by_id(manager, backup_id) {
      Some(backup) => Some(backup.config)
      None => None
    }
  }
  
  // Test backup and restore functionality
  let backup_manager = create_backup_manager(5, 3600)  // Max 5 backups, 1 hour interval
  
  // Create test configurations
  let config1 = [
    ("sampling_rate", "0.1"),
    ("batch_size", "100"),
    ("flush_interval", "5000")
  ]
  
  let config2 = [
    ("sampling_rate", "0.2"),
    ("batch_size", "200"),
    ("flush_interval", "10000")
  ]
  
  let config3 = [
    ("sampling_rate", "0.05"),
    ("batch_size", "500"),
    ("flush_interval", "30000")
  ]
  
  // Create backups
  let backup1 = create_backup(config1, "Initial configuration", "admin", 1640995200)
  let backup2 = create_backup(config2, "Updated configuration", "admin", 1640995300)
  let backup3 = create_backup(config3, "Performance optimized", "admin", 1640995400)
  
  // Add backups to manager
  let manager1 = add_backup(backup_manager, backup1)
  assert_eq(manager1.backups.length(), 1)
  assert_eq(manager1.last_backup_time, 1640995200)
  
  let manager2 = add_backup(manager1, backup2)
  assert_eq(manager2.backups.length(), 2)
  assert_eq(manager2.last_backup_time, 1640995300)
  
  let manager3 = add_backup(manager2, backup3)
  assert_eq(manager3.backups.length(), 3)
  assert_eq(manager3.last_backup_time, 1640995400)
  
  // Verify backups are sorted by timestamp (newest first)
  assert_eq(manager3.backups[0].timestamp, 1640995400)  // backup3
  assert_eq(manager3.backups[1].timestamp, 1640995300)  // backup2
  assert_eq(manager3.backups[2].timestamp, 1640995200)  // backup1
  
  // Test backup retrieval
  let retrieved_backup = get_backup_by_id(manager3, backup2.id)
  assert_true(retrieved_backup.is_some())
  
  match retrieved_backup {
    Some(backup) => {
      assert_eq(backup.id, backup2.id)
      assert_eq(backup.description, "Updated configuration")
      assert_eq(backup.config[0], ("sampling_rate", "0.2"))
    }
    None => assert_true(false)
  }
  
  // Test restore from backup
  let restored_config = restore_from_backup(manager3, backup1.id)
  assert_true(restored_config.is_some())
  
  match restored_config {
    Some(config) => {
      assert_eq(config.length(), 3)
      assert_eq(config[0], ("sampling_rate", "0.1"))
      assert_eq(config[1], ("batch_size", "100"))
      assert_eq(config[2], ("flush_interval", "5000"))
    }
    None => assert_true(false)
  }
  
  // Test backup limit enforcement
  let config4 = [
    ("sampling_rate", "0.15"),
    ("batch_size", "150"),
    ("flush_interval", "7500")
  ]
  
  let config5 = [
    ("sampling_rate", "0.25"),
    ("batch_size", "250"),
    ("flush_interval", "15000")
  ]
  
  let config6 = [
    ("sampling_rate", "0.3"),
    ("batch_size", "300"),
    ("flush_interval", "20000")
  ]
  
  let backup4 = create_backup(config4, "Configuration 4", "admin", 1640995500)
  let backup5 = create_backup(config5, "Configuration 5", "admin", 1640995600)
  let backup6 = create_backup(config6, "Configuration 6", "admin", 1640995700)
  
  // Add backups to exceed limit
  let manager4 = add_backup(manager3, backup4)
  let manager5 = add_backup(manager4, backup5)
  let manager6 = add_backup(manager5, backup6)
  
  assert_eq(manager6.backups.length(), 5)  // Should be limited to max_backups
  
  // Oldest backup should be removed (backup1)
  assert_false(manager6.backups.any_fn(backup) { backup.id == backup1.id })
  assert_true(manager6.backups.any_fn(backup) { backup.id == backup6.id })  // Newest should be present
  
  // Test backup comparison
  let compare_backups = fn(backup1: ConfigBackup, backup2: ConfigBackup) {
    let differences = []
    
    // Compare configurations
    for (key, value) in backup1.config {
      match backup2.config.find_fn(item) { item.0 == key } {
        Some((_, value2)) => {
          if value != value2 {
            differences = differences.push((key, value, value2))
          }
        }
        None => {
          differences = differences.push((key, value, "MISSING"))
        }
      }
    }
    
    // Find keys unique to backup2
    for (key, value) in backup2.config {
      if not(backup1.config.any_fn(item) { item.0 == key }) {
        differences = differences.push((key, "MISSING", value))
      }
    }
    
    differences
  }
  
  // Compare backup1 and backup2
  let backup_differences = compare_backups(backup1, backup2)
  assert_eq(backup_differences.length(), 3)  // All three values differ
  
  assert_true(backup_differences.any_fn(diff) { 
    diff.0 == "sampling_rate" && diff.1 == "0.1" && diff.2 == "0.2" 
  }))
  
  // Test backup scheduling
  let should_create_backup = fn(manager: BackupManager, current_time: Int) {
    current_time - manager.last_backup_time >= manager.backup_interval
  }
  
  // Should not create backup if interval not passed
  assert_false(should_create_backup(manager6, 1640995700 + 1800))  // 30 minutes passed (less than 1 hour)
  
  // Should create backup if interval passed
  assert_true(should_create_backup(manager6, 1640995700 + 3600))  // 1 hour passed
  
  // Test backup validation
  let validate_backup = fn(backup: ConfigBackup) {
    let mut errors = []
    
    // Check required fields
    if backup.id.length() == 0 {
      errors = errors.push("Backup ID is required")
    }
    
    if backup.description.length() == 0 {
      errors = errors.push("Backup description is required")
    }
    
    if backup.created_by.length() == 0 {
      errors = errors.push("Backup creator is required")
    }
    
    if backup.config.length() == 0 {
      errors = errors.push("Backup configuration cannot be empty")
    }
    
    // Check timestamp
    if backup.timestamp <= 0 {
      errors = errors.push("Backup timestamp must be positive")
    }
    
    errors
  }
  
  let valid_backup_errors = validate_backup(backup1)
  assert_eq(valid_backup_errors.length(), 0)  // Should be valid
  
  let invalid_backup = {
    id: "",
    timestamp: -1,
    config: [],
    description: "",
    created_by: ""
  }
  
  let invalid_backup_errors = validate_backup(invalid_backup)
  assert_eq(invalid_backup_errors.length(), 5)  // All fields invalid
}