// Azimuth Telemetry Metadata Management Tests
// This file contains test cases for telemetry metadata management

// Test 1: Dynamic Metadata Schema Management
test "dynamic metadata schema management" {
  // Define metadata field types
  enum MetadataFieldType {
    String
    Integer
    Float
    Boolean
    Array(String)
    Map(String, String)
    Timestamp
    Enum(Array[String])
  }
  
  // Define metadata field definition
  type MetadataField = {
    name: String,
    field_type: MetadataFieldType,
    required: Bool,
    default_value: Option[String],
    validation_rules: Array[String],
    description: String
  }
  
  // Define metadata schema
  type MetadataSchema = {
    name: String,
    version: String,
    fields: Array[MetadataField],
    created_at: Int,
    updated_at: Int
  }
  
  // Define metadata validation result
  type MetadataValidationResult = {
    is_valid: Bool,
    missing_fields: Array[String],
    invalid_fields: Array[(String, String)],  // field_name, error_message
    applied_defaults: Array[(String, String)]
  }
  
  // Validate metadata against schema
  let validate_metadata = fn(metadata: Array[(String, String)], schema: MetadataSchema) {
    let mut missing_fields = []
    let mut invalid_fields = []
    let mut applied_defaults = []
    
    for field in schema.fields {
      match metadata.find_fn(item) { item.0 == field.name } {
        Some((key, value)) => {
          // Validate field type and rules
          let type_valid = match field.field_type {
            MetadataFieldType::String => value.length() > 0
            MetadataFieldType::Integer => {
              match value.parse_int() {
                Some(_) => true
                None => false
              }
            }
            MetadataFieldType::Float => {
              match value.parse_float() {
                Some(_) => true
                None => false
              }
            }
            MetadataFieldType::Boolean => {
              value == "true" || value == "false"
            }
            MetadataFieldType::Array(_) => {
              value.starts_with("[") && value.ends_with("]")
            }
            MetadataFieldType::Map(_, _) => {
              value.starts_with("{") && value.ends_with("}")
            }
            MetadataFieldType::Timestamp => {
              match value.parse_int() {
                Some(ts) => ts > 0
                None => false
              }
            }
            MetadataFieldType::Enum(options) => {
              options.some_fn(opt) { opt == value }
            }
          }
          
          if not(type_valid) {
            invalid_fields = invalid_fields.push((field.name, "invalid_type"))
          }
        }
        None => {
          if field.required {
            missing_fields = missing_fields.push(field.name)
          } else {
            // Apply default value if available
            match field.default_value {
              Some(default) => {
                applied_defaults = applied_defaults.push((field.name, default))
              }
              None => {}  // No default value
            }
          }
        }
      }
    }
    
    {
      is_valid: missing_fields.length() == 0 && invalid_fields.length() == 0,
      missing_fields: missing_fields,
      invalid_fields: invalid_fields,
      applied_defaults: applied_defaults
    }
  }
  
  // Test metadata schema validation
  let telemetry_schema = {
    name: "telemetry_metadata",
    version: "1.0.0",
    fields: [
      {
        name: "service_name",
        field_type: MetadataFieldType::String,
        required: true,
        default_value: None,
        validation_rules: ["min_length:3", "max_length:50"],
        description: "Name of the service generating telemetry"
      },
      {
        name: "trace_id",
        field_type: MetadataFieldType::String,
        required: true,
        default_value: None,
        validation_rules: ["pattern:^[a-f0-9]{32}$"],
        description: "Unique identifier for the trace"
      },
      {
        name: "duration_ms",
        field_type: MetadataFieldType::Integer,
        required: true,
        default_value: None,
        validation_rules: ["min:0"],
        description: "Duration of the operation in milliseconds"
      },
      {
        name: "success",
        field_type: MetadataFieldType::Boolean,
        required: false,
        default_value: Some("true"),
        validation_rules: [],
        description: "Whether the operation was successful"
      },
      {
        name: "status_code",
        field_type: MetadataFieldType::Enum(["200", "400", "404", "500"]),
        required: false,
        default_value: Some("200"),
        validation_rules: [],
        description: "HTTP status code"
      },
      {
        name: "tags",
        field_type: MetadataFieldType::Array(String),
        required: false,
        default_value: Some("[]"),
        validation_rules: [],
        description: "Arbitrary tags for the telemetry data"
      }
    ],
    created_at: 1640995200,
    updated_at: 1640995200
  }
  
  // Test valid metadata
  let valid_metadata = [
    ("service_name", "payment-service"),
    ("trace_id", "4bf92f3577b34da6a3ce929d0e0e4736"),
    ("duration_ms", "250"),
    ("success", "true"),
    ("status_code", "200"),
    ("tags", "[\"critical\", \"payment\"]")
  ]
  
  let valid_validation = validate_metadata(valid_metadata, telemetry_schema)
  assert_true(valid_validation.is_valid)
  assert_eq(valid_validation.missing_fields.length(), 0)
  assert_eq(valid_validation.invalid_fields.length(), 0)
  assert_eq(valid_validation.applied_defaults.length(), 0)
  
  // Test metadata with missing required fields
  let incomplete_metadata = [
    ("service_name", "user-service"),
    ("duration_ms", "150")
    // Missing trace_id (required)
  ]
  
  let incomplete_validation = validate_metadata(incomplete_metadata, telemetry_schema)
  assert_false(incomplete_validation.is_valid)
  assert_eq(incomplete_validation.missing_fields.length(), 1)
  assert_eq(incomplete_validation.missing_fields[0], "trace_id")
  
  // Test metadata with invalid field types
  let invalid_metadata = [
    ("service_name", "api-service"),
    ("trace_id", "invalid-trace-id"),
    ("duration_ms", "not-a-number"),
    ("success", "maybe"),
    ("status_code", "301")  // Not in enum
  ]
  
  let invalid_validation = validate_metadata(invalid_metadata, telemetry_schema)
  assert_false(invalid_validation.is_valid)
  assert_eq(incomplete_validation.invalid_fields.length(), 4)
  assert_true(invalid_validation.invalid_fields.some_fn(f) { f.0 == "trace_id" })
  assert_true(invalid_validation.invalid_fields.some_fn(f) { f.0 == "duration_ms" })
  assert_true(invalid_validation.invalid_fields.some_fn(f) { f.0 == "success" })
  assert_true(invalid_validation.invalid_fields.some_fn(f) { f.0 == "status_code" })
  
  // Test metadata with default values applied
  let minimal_metadata = [
    ("service_name", "cache-service"),
    ("trace_id", "abc123def4567890abc123def4567890"),
    ("duration_ms", "50")
  ]
  
  let minimal_validation = validate_metadata(minimal_metadata, telemetry_schema)
  assert_true(minimal_validation.is_valid)
  assert_eq(minimal_validation.applied_defaults.length(), 3)
  assert_true(minimal_validation.applied_defaults.some_fn(d) { d.0 == "success" && d.1 == "true" })
  assert_true(minimal_validation.applied_defaults.some_fn(d) { d.0 == "status_code" && d.1 == "200" })
  assert_true(minimal_validation.applied_defaults.some_fn(d) { d.0 == "tags" && d.1 == "[]" })
}

// Test 2: Metadata Versioning and Migration
test "metadata versioning and migration" {
  // Define migration operation
  enum MigrationOperation {
    AddField(MetadataField)
    RemoveField(String)
    RenameField(String, String)
    ChangeFieldType(String, MetadataFieldType)
    SetDefaultValue(String, Option[String])
    AddValidationRule(String, String)
    RemoveValidationRule(String, String)
  }
  
  // Define schema migration
  type SchemaMigration = {
    from_version: String,
    to_version: String,
    operations: Array[MigrationOperation>,
    migration_date: Int,
    description: String
  }
  
  // Define migration result
  type MigrationResult = {
    success: Bool,
    migrated_metadata: Array[(String, String)],
    errors: Array[String],
    warnings: Array[String]
  }
  
  // Apply migration to metadata
  let apply_migration = fn(metadata: Array[(String, String)], migration: SchemaMigration) {
    let mut migrated_metadata = metadata
    let mut errors = []
    let mut warnings = []
    
    for operation in migration.operations {
      match operation {
        MigrationOperation::AddField(field) => {
          if not(migrated_metadata.some_fn(item) { item.0 == field.name }) {
            match field.default_value {
              Some(default) => {
                migrated_metadata = migrated_metadata.push((field.name, default))
              }
              None => {
                if field.required {
                  errors = errors.push("required_field_without_default: " + field.name)
                }
              }
            }
          } else {
            warnings = warnings.push("field_already_exists: " + field.name)
          }
        }
        MigrationOperation::RemoveField(field_name) => {
          migrated_metadata = migrated_metadata.filter_fn(item) { item.0 != field_name }
        }
        MigrationOperation::RenameField(old_name, new_name) => {
          let mut found = false
          migrated_metadata = migrated_metadata.map_fn(item) {
            if item.0 == old_name {
              found = true
              (new_name, item.1)
            } else {
              item
            }
          }
          if not(found) {
            warnings = warnings.push("field_not_found_for_rename: " + old_name)
          }
        }
        MigrationOperation::ChangeFieldType(field_name, new_type) => {
          // In a real implementation, this would validate the field value
          // For this test, we just record that the operation was attempted
          match migrated_metadata.find_fn(item) { item.0 == field_name } {
            Some(_) => {}  // Field exists, type change noted
            None => {
              warnings = warnings.push("field_not_found_for_type_change: " + field_name)
            }
          }
        }
        MigrationOperation::SetDefaultValue(field_name, default_value) => {
          if not(migrated_metadata.some_fn(item) { item.0 == field_name }) {
            match default_value {
              Some(default) => {
                migrated_metadata = migrated_metadata.push((field_name, default))
              }
              None => {}  // Remove default value
            }
          }
        }
        MigrationOperation::AddValidationRule(field_name, rule) => {
          // In a real implementation, this would add validation rules
          match migrated_metadata.find_fn(item) { item.0 == field_name } {
            Some(_) => {}  // Field exists, rule added
            None => {
              warnings = warnings.push("field_not_found_for_rule_addition: " + field_name)
            }
          }
        }
        MigrationOperation::RemoveValidationRule(field_name, rule) => {
          // In a real implementation, this would remove validation rules
          match migrated_metadata.find_fn(item) { item.0 == field_name } {
            Some(_) => {}  // Field exists, rule removed
            None => {
              warnings = warnings.push("field_not_found_for_rule_removal: " + field_name)
            }
          }
        }
      }
    }
    
    {
      success: errors.length() == 0,
      migrated_metadata: migrated_metadata,
      errors: errors,
      warnings: warnings
    }
  }
  
  // Test metadata migration
  let v1_to_v2_migration = {
    from_version: "1.0.0",
    to_version: "2.0.0",
    operations: [
      MigrationOperation::AddField({
        name: "environment",
        field_type: MetadataFieldType::String,
        required: false,
        default_value: Some("production"),
        validation_rules: ["enum:development,production,staging"],
        description: "Environment where the telemetry was generated"
      }),
      MigrationOperation::RenameField("duration_ms", "duration_microseconds"),
      MigrationOperation::AddField({
        name: "component",
        field_type: MetadataFieldType::String,
        required: false,
        default_value: None,
        validation_rules: [],
        description: "Component within the service"
      }),
      MigrationOperation::RemoveField("legacy_field")
    ],
    migration_date: 1640995200,
    description: "Add environment field and rename duration field"
  }
  
  let v1_metadata = [
    ("service_name", "api-service"),
    ("trace_id", "abc123def4567890abc123def4567890"),
    ("duration_ms", "250"),
    ("success", "true"),
    ("legacy_field", "old_value")
  ]
  
  let migration_result = apply_migration(v1_metadata, v1_to_v2_migration)
  assert_true(migration_result.success)
  assert_eq(migration_result.errors.length(), 0)
  
  // Check that fields were added/renamed/removed correctly
  assert_true(migration_result.migrated_metadata.some_fn(item) { item.0 == "environment" && item.1 == "production" })
  assert_true(migration_result.migrated_metadata.some_fn(item) { item.0 == "duration_microseconds" && item.1 == "250" })
  assert_false(migration_result.migrated_metadata.some_fn(item) { item.0 == "duration_ms" })
  assert_false(migration_result.migrated_metadata.some_fn(item) { item.0 == "legacy_field" })
  
  // Test migration with required field without default
  let problematic_migration = {
    from_version: "1.0.0",
    to_version: "2.0.0",
    operations: [
      MigrationOperation::AddField({
        name: "required_field",
        field_type: MetadataFieldType::String,
        required: true,
        default_value: None,  // No default value for required field
        validation_rules: [],
        description: "Required field without default"
      })
    ],
    migration_date: 1640995200,
    description: "Add required field without default"
  }
  
  let problematic_result = apply_migration(v1_metadata, problematic_migration)
  assert_false(problematic_result.success)
  assert_eq(problematic_result.errors.length(), 1)
  assert_eq(problematic_result.errors[0], "required_field_without_default: required_field")
}

// Test 3: Metadata Template and Inheritance
test "metadata template and inheritance" {
  // Define metadata template
  type MetadataTemplate = {
    name: String,
    base_fields: Array[(String, String)],
    variable_fields: Array[String],  // Fields that can be overridden
    description: String
  }
  
  // Define template instance
  type TemplateInstance = {
    template_name: String,
    overrides: Array[(String, String)],
    computed_metadata: Array[(String, String)]
  }
  
  // Create metadata from template
  let create_from_template = fn(template: MetadataTemplate, overrides: Array[(String, String)]) {
    let mut metadata = template.base_fields
    
    // Apply overrides
    for (key, value) in overrides {
      if template.variable_fields.contains(key) {
        // Update existing field or add new one
        let mut found = false
        metadata = metadata.map_fn(item) {
          if item.0 == key {
            found = true
            (key, value)
          } else {
            item
          }
        }
        
        if not(found) {
          metadata = metadata.push((key, value))
        }
      }
    }
    
    {
      template_name: template.name,
      overrides: overrides,
      computed_metadata: metadata
    }
  }
  
  // Test template inheritance
  let service_template = {
    name: "service_metadata",
    base_fields: [
      ("service_name", ""),
      ("version", "1.0.0"),
      ("environment", "development"),
      ("region", "us-west-2"),
      ("datacenter", "dc1")
    ],
    variable_fields: ["service_name", "environment", "region", "datacenter"],
    description: "Base template for service metadata"
  }
  
  let api_overrides = [
    ("service_name", "api-service"),
    ("environment", "production"),
    ("region", "us-east-1")
  ]
  
  let api_instance = create_from_template(service_template, api_overrides)
  assert_eq(api_instance.template_name, "service_metadata")
  assert_eq(api_instance.overrides.length(), 3)
  assert_eq(api_instance.computed_metadata.length(), 5)
  
  // Check that overrides were applied
  assert_true(api_instance.computed_metadata.some_fn(item) { item.0 == "service_name" && item.1 == "api-service" })
  assert_true(api_instance.computed_metadata.some_fn(item) { item.0 == "environment" && item.1 == "production" })
  assert_true(api_instance.computed_metadata.some_fn(item) { item.0 == "region" && item.1 == "us-east-1" })
  
  // Check that base values were preserved for non-overridden fields
  assert_true(api_instance.computed_metadata.some_fn(item) { item.0 == "version" && item.1 == "1.0.0" })
  assert_true(api_instance.computed_metadata.some_fn(item) { item.0 == "datacenter" && item.1 == "dc1" })
  
  // Test template composition
  let compose_templates = fn(base_template: MetadataTemplate, extension_template: MetadataTemplate) {
    let combined_base_fields = base_template.base_fields.concat(extension_template.base_fields)
    let combined_variable_fields = base_template.variable_fields.concat(extension_template.variable_fields)
    
    {
      name: base_template.name + "_" + extension_template.name,
      base_fields: combined_base_fields,
      variable_fields: combined_variable_fields,
      description: "Composed template: " + base_template.description + " + " + extension_template.description
    }
  }
  
  let monitoring_template = {
    name: "monitoring_metadata",
    base_fields: [
      ("monitoring_enabled", "true"),
      ("metrics_port", "9090"),
      ("health_check_path", "/health")
    ],
    variable_fields: ["monitoring_enabled", "metrics_port"],
    description: "Template for monitoring metadata"
  }
  
  let composed_template = compose_templates(service_template, monitoring_template)
  assert_eq(composed_template.name, "service_metadata_monitoring_metadata")
  assert_eq(composed_template.base_fields.length(), 8)
  assert_eq(composed_template.variable_fields.length(), 6)
  
  // Test with composed template
  let composed_overrides = [
    ("service_name", "composed-service"),
    ("monitoring_enabled", "false"),
    ("metrics_port", "9091")
  ]
  
  let composed_instance = create_from_template(composed_template, composed_overrides)
  assert_eq(composed_instance.computed_metadata.length(), 8)
  assert_true(composed_instance.computed_metadata.some_fn(item) { item.0 == "service_name" && item.1 == "composed-service" })
  assert_true(composed_instance.computed_metadata.some_fn(item) { item.0 == "monitoring_enabled" && item.1 == "false" })
  assert_true(composed_instance.computed_metadata.some_fn(item) { item.0 == "metrics_port" && item.1 == "9091" })
}

// Test 4: Metadata Search and Indexing
test "metadata search and indexing" {
  // Define search query
  enum SearchOperator {
    Equals
    Contains
    StartsWith
    EndsWith
    GreaterThan
    LessThan
    In(Array[String])
  }
  
  // Define search condition
  type SearchCondition = {
    field: String,
    operator: SearchOperator,
    value: String
  }
  
  // Define search query
  type SearchQuery = {
    conditions: Array[SearchCondition>,
    operator: String  // "AND" or "OR"
  }
  
  // Define search result
  type SearchResult = {
    matches: Array[Int],  // Indices of matching metadata entries
    total_count: Int,
    query: SearchQuery
  }
  
  // Search metadata
  let search_metadata = fn(metadata_collection: Array<Array[(String, String)]>, query: SearchQuery) {
    let mut matches = []
    
    for (index, metadata) in metadata_collection.enumerate() {
      let mut condition_results = []
      
      for condition in query.conditions {
        let field_value = match metadata.find_fn(item) { item.0 == condition.field } {
          Some((_, value)) => value
          None => ""
        }
        
        let condition_result = match condition.operator {
          SearchOperator::Equals => field_value == condition.value
          SearchOperator::Contains => field_value.contains(condition.value)
          SearchOperator::StartsWith => field_value.starts_with(condition.value)
          SearchOperator::EndsWith => field_value.ends_with(condition.value)
          SearchOperator::GreaterThan => {
            match (field_value.parse_float(), condition.value.parse_float()) {
              (Some(field_num), Some(condition_num)) => field_num > condition_num
              _ => false
            }
          }
          SearchOperator::LessThan => {
            match (field_value.parse_float(), condition.value.parse_float()) {
              (Some(field_num), Some(condition_num)) => field_num < condition_num
              _ => false
            }
          }
          SearchOperator::In(options) => options.some_fn(opt) { opt == field_value }
        }
        
        condition_results = condition_results.push(condition_result)
      }
      
      // Evaluate conditions based on operator
      let query_match = match query.operator {
        "AND" => condition_results.all_fn(result) { result }
        "OR" => condition_results.some_fn(result) { result }
        _ => false
      }
      
      if query_match {
        matches = matches.push(index)
      }
    }
    
    {
      matches: matches,
      total_count: matches.length(),
      query: query
    }
  }
  
  // Test metadata search
  let metadata_collection = [
    [
      ("service_name", "api-service"),
      ("environment", "production"),
      ("version", "1.2.3"),
      ("region", "us-west-2"),
      ("success_rate", "0.95")
    ],
    [
      ("service_name", "payment-service"),
      ("environment", "production"),
      ("version", "1.1.0"),
      ("region", "us-east-1"),
      ("success_rate", "0.98")
    ],
    [
      ("service_name", "user-service"),
      ("environment", "staging"),
      ("version", "1.3.0"),
      ("region", "us-west-2"),
      ("success_rate", "0.92")
    ],
    [
      ("service_name", "notification-service"),
      ("environment", "production"),
      ("version", "1.0.5"),
      ("region", "eu-west-1"),
      ("success_rate", "0.97")
    ]
  ]
  
  // Test simple equality search
  let production_query = {
    conditions: [
      {
        field: "environment",
        operator: SearchOperator::Equals,
        value: "production"
      }
    ],
    operator: "AND"
  }
  
  let production_result = search_metadata(metadata_collection, production_query)
  assert_eq(production_result.total_count, 3)
  assert_eq(production_result.matches, [0, 1, 3])
  
  // Test multiple conditions with AND
  let production_api_query = {
    conditions: [
      {
        field: "environment",
        operator: SearchOperator::Equals,
        value: "production"
      },
      {
        field: "service_name",
        operator: SearchOperator::Contains,
        value: "service"
      }
    ],
    operator: "AND"
  }
  
  let production_api_result = search_metadata(metadata_collection, production_api_query)
  assert_eq(production_api_result.total_count, 3)
  assert_eq(production_api_result.matches, [0, 1, 3])
  
  // Test multiple conditions with OR
  let region_query = {
    conditions: [
      {
        field: "region",
        operator: SearchOperator::Equals,
        value: "us-west-2"
      },
      {
        field: "region",
        operator: SearchOperator::Equals,
        value: "eu-west-1"
      }
    ],
    operator: "OR"
  }
  
  let region_result = search_metadata(metadata_collection, region_query)
  assert_eq(region_result.total_count, 3)
  assert_eq(region_result.matches, [0, 2, 3])
  
  // Test numeric comparison
  let high_success_rate_query = {
    conditions: [
      {
        field: "success_rate",
        operator: SearchOperator::GreaterThan,
        value: "0.96"
      }
    ],
    operator: "AND"
  }
  
  let high_success_rate_result = search_metadata(metadata_collection, high_success_rate_query)
  assert_eq(high_success_rate_result.total_count, 2)
  assert_eq(high_success_rate_result.matches, [1, 3])
  
  // Test IN operator
  let version_query = {
    conditions: [
      {
        field: "version",
        operator: SearchOperator::In(["1.2.3", "1.3.0", "1.0.5"]),
        value: ""
      }
    ],
    operator: "AND"
  }
  
  let version_result = search_metadata(metadata_collection, version_query)
  assert_eq(version_result.total_count, 3)
  assert_eq(version_result.matches, [0, 2, 3])
  
  // Test metadata indexing
  let create_metadata_index = fn(metadata_collection: Array<Array[(String, String)]>, field_name: String) {
    let mut index = Map::empty()
    
    for (index, metadata) in metadata_collection.enumerate() {
      match metadata.find_fn(item) { item.0 == field_name } {
        Some((_, value)) => {
          let current_indices = match Map::get(index, value) {
            Some(indices) => indices
            None => []
          }
          let _ = Map::insert(index, value, current_indices.push(index))
        }
        None => {}  // Field not present in this metadata
      }
    }
    
    index
  }
  
  // Test index creation and lookup
  let service_index = create_metadata_index(metadata_collection, "service_name")
  assert_eq(Map::size(service_index), 4)  // 4 unique service names
  
  let api_service_indices = match Map::get(service_index, "api-service") {
    Some(indices) => indices
    None => []
  }
  assert_eq(api_service_indices, [0])
  
  let payment_service_indices = match Map::get(service_index, "payment-service") {
    Some(indices) => indices
    None => []
  }
  assert_eq(payment_service_indices, [1])
  
  // Test indexed search
  let search_with_index = fn(index: Map<String, Array[Int>>, field_value: String) {
    match Map::get(index, field_value) {
      Some(indices) => indices
      None => []
    }
  }
  
  let indexed_result = search_with_index(service_index, "user-service")
  assert_eq(indexed_result, [2])
  
  let missing_result = search_with_index(service_index, "non-existent-service")
  assert_eq(missing_result.length(), 0)
}