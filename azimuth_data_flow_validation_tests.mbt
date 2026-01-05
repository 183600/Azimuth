// Azimuth Telemetry System - Data Flow Validation Tests
// This file contains comprehensive test cases for data flow validation and integrity

// Test 1: Data Schema Validation
test "data schema validation" {
  let schema_registry = SchemaRegistry::new()
  
  // Test schema definition
  let user_schema = Schema::new("User", [
    ("id", SchemaType::Int, true),
    ("name", SchemaType::String, true),
    ("email", SchemaType::String, true),
    ("age", SchemaType::Int, false),
    ("active", SchemaType::Boolean, false)
  ])
  
  SchemaRegistry::register(schema_registry, user_schema)
  
  // Test valid data validation
  let valid_user_data = [
    ("id", IntValue(1)),
    ("name", StringValue("John Doe")),
    ("email", StringValue("john@example.com")),
    ("age", IntValue(30)),
    ("active", BoolValue(true))
  ]
  
  let valid_result = SchemaValidator::validate(schema_registry, "User", valid_user_data)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid data validation (missing required field)
  let invalid_user_data = [
    ("id", IntValue(1)),
    ("name", StringValue("John Doe"))
    // Missing required "email" field
  ]
  
  let invalid_result = SchemaValidator::validate(schema_registry, "User", invalid_user_data)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() > 0)
  
  // Test type mismatch validation
  let type_mismatch_data = [
    ("id", StringValue("not_an_int")), // Type mismatch
    ("name", StringValue("John Doe")),
    ("email", StringValue("john@example.com"))
  ]
  
  let type_mismatch_result = SchemaValidator::validate(schema_registry, "User", type_mismatch_data)
  assert_false(type_mismatch_result.is_valid)
  assert_true(type_mismatch_result.errors.length() > 0)
}

// Test 2: Data Transformation Validation
test "data transformation validation" {
  let transformer = DataTransformer::new()
  
  // Test field mapping transformation
  let mapping_rules = [
    ("user_id", "id"),
    ("user_name", "name"),
    ("user_email", "email")
  ]
  
  let source_data = [
    ("user_id", IntValue(1)),
    ("user_name", StringValue("Jane Smith")),
    ("user_email", StringValue("jane@example.com"))
  ]
  
  let transformed_data = DataTransformer::apply_mapping(transformer, source_data, mapping_rules)
  assert_true(DataUtils::has_field(transformed_data, "id"))
  assert_true(DataUtils::has_field(transformed_data, "name"))
  assert_true(DataUtils::has_field(transformed_data, "email"))
  
  // Test data type transformation
  let type_transformations = [
    ("age", StringToInt),
    ("score", StringToFloat),
    ("active", StringToBoolean)
  ]
  
  let string_data = [
    ("age", StringValue("25")),
    ("score", StringValue("95.5")),
    ("active", StringValue("true"))
  ]
  
  let typed_data = DataTransformer::apply_type_transformations(transformer, string_data, type_transformations)
  
  match DataUtils::get_field(typed_data, "age") {
    Some(IntValue(age)) => assert_eq(age, 25)
    _ => assert_true(false)
  }
  
  match DataUtils::get_field(typed_data, "score") {
    Some(FloatValue(score)) => assert_eq(score, 95.5)
    _ => assert_true(false)
  }
  
  match DataUtils::get_field(typed_data, "active") {
    Some(BoolValue(active)) => assert_true(active)
    _ => assert_true(false)
  }
}

// Test 3: Data Consistency Validation
test "data consistency validation" {
  let consistency_validator = ConsistencyValidator::new()
  
  // Test referential consistency
  let users = [
    (1, "Alice"),
    (2, "Bob"),
    (3, "Charlie")
  ]
  
  let orders = [
    (101, 1, "Product A"), // Valid - references user 1
    (102, 2, "Product B"), // Valid - references user 2
    (103, 4, "Product C")  // Invalid - references non-existent user 4
  ]
  
  let referential_result = ConsistencyValidator::validate_referential_integrity(
    consistency_validator,
    users,
    orders,
    0, // user_id index in users
    1  // user_id index in orders
  )
  
  assert_false(referential_result.is_consistent)
  assert_eq(referential_result.violations.length(), 1)
  
  // Test temporal consistency
  let events = [
    (1L, 1000L, "User created"),
    (2L, 2000L, "Order placed"),
    (3L, 1500L, "Profile updated") // Out of order timestamp
  ]
  
  let temporal_result = ConsistencyValidator::validate_temporal_consistency(
    consistency_validator,
    events,
    1, // timestamp index
    0  // event_id index
  )
  
  assert_false(temporal_result.is_consistent)
  
  // Test business rule consistency
  let business_rules = [
    BusinessRule::new("order_total_must_be_positive", "total > 0"),
    BusinessRule::new("discount_cannot_exceed_total", "discount <= total"),
    BusinessRule::new("shipping_date_must_be_after_order_date", "shipping_date >= order_date")
  ]
  
  let order_data = [
    ("total", FloatValue(100.0)),
    ("discount", FloatValue(20.0)),
    ("shipping_date", IntValue(20230115)),
    ("order_date", IntValue(20230110))
  ]
  
  let business_result = ConsistencyValidator::validate_business_rules(
    consistency_validator,
    order_data,
    business_rules
  )
  
  assert_true(business_result.is_consistent)
}

// Test 4: Data Pipeline Validation
test "data pipeline validation" {
  let pipeline_validator = PipelineValidator::new()
  
  // Test pipeline step validation
  let step1 = PipelineStep::new("extract", "Data extraction", [])
  let step2 = PipelineStep::new("transform", "Data transformation", [])
  let step3 = PipelineStep::new("load", "Data loading", [])
  
  let pipeline = Pipeline::new([step1, step2, step3])
  
  // Test pipeline structure validation
  let structure_result = PipelineValidator::validate_structure(pipeline_validator, pipeline)
  assert_true(structure_result.is_valid)
  
  // Test pipeline data flow validation
  let data_flow_rules = [
    DataFlowRule::new("extract_to_transform", "extract", "transform", [
      ("required_fields", ["user_id", "timestamp"])
    ]),
    DataFlowRule::new("transform_to_load", "transform", "load", [
      ("required_fields", ["processed_data", "validation_status"])
    ])
  ]
  
  let flow_result = PipelineValidator::validate_data_flow(
    pipeline_validator,
    pipeline,
    data_flow_rules
  )
  
  // Test pipeline execution validation
  let execution_context = ExecutionContext::new()
  ExecutionContext::set_data(execution_context, "extract_output", [
    ("user_id", IntValue(123)),
    ("timestamp", IntValue(1640995200L)),
    ("raw_data", StringValue("sample data"))
  ])
  
  let step_validation = PipelineValidator::validate_step_execution(
    pipeline_validator,
    step2,
    execution_context
  )
  
  assert_true(step_validation.has_required_inputs)
}

// Test 5: Data Quality Validation
test "data quality validation" {
  let quality_validator = DataQualityValidator::new()
  
  // Test completeness validation
  let complete_data = [
    ("field1", StringValue("value1")),
    ("field2", IntValue(42)),
    ("field3", BoolValue(true))
  ]
  
  let incomplete_data = [
    ("field1", StringValue("value1")),
    ("field2", IntValue(42))
    // Missing field3
  ]
  
  let completeness_config = CompletenessConfig::new(0.9) // 90% completeness threshold
  let complete_result = QualityValidator::validate_completeness(
    quality_validator,
    complete_data,
    completeness_config
  )
  
  assert_true(complete_result.meets_threshold)
  assert_eq(complete_result.completeness_score, 1.0)
  
  let incomplete_result = QualityValidator::validate_completeness(
    quality_validator,
    incomplete_data,
    completeness_config
  )
  
  assert_false(incomplete_result.meets_threshold)
  assert_eq(incomplete_result.completeness_score, 0.6666666666666666)
  
  // Test accuracy validation
  let accuracy_rules = [
    AccuracyRule::new("email_format", "email", Regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$")),
    AccuracyRule::new("phone_format", "phone", Regex("^\d{3}-\d{3}-\d{4}$")),
    AccuracyRule::new("age_range", "age", Range(0, 150))
  ]
  
  let accurate_data = [
    ("email", StringValue("user@example.com")),
    ("phone", StringValue("123-456-7890")),
    ("age", IntValue(25))
  ]
  
  let accurate_result = QualityValidator::validate_accuracy(
    quality_validator,
    accurate_data,
    accuracy_rules
  )
  
  assert_true(accurate_result.meets_threshold)
  assert_eq(accurate_result.accuracy_score, 1.0)
  
  // Test uniqueness validation
  let unique_fields = ["user_id", "email"]
  let dataset = [
    [("user_id", IntValue(1)), ("email", StringValue("user1@example.com"))],
    [("user_id", IntValue(2)), ("email", StringValue("user2@example.com"))],
    [("user_id", IntValue(3)), ("email", StringValue("user3@example.com"))]
  ]
  
  let uniqueness_result = QualityValidator::validate_uniqueness(
    quality_validator,
    dataset,
    unique_fields
  )
  
  assert_true(uniqueness_result.meets_threshold)
  assert_eq(uniqueness_result.uniqueness_score, 1.0)
}

// Test 6: Data Anomaly Detection
test "data anomaly detection" {
  let anomaly_detector = AnomalyDetector::new()
  
  // Test statistical anomaly detection
  let numeric_values = [10.0, 12.0, 11.5, 13.0, 9.5, 11.0, 50.0] // 50.0 is an outlier
  let statistical_anomalies = AnomalyDetector::detect_statistical_anomalies(
    anomaly_detector,
    numeric_values,
    2.0 // 2 standard deviations
  )
  
  assert_true(statistical_anomalies.length() > 0)
  assert_true(statistical_anomalies.contains(50.0))
  
  // Test pattern anomaly detection
  let time_series_data = [
    (1000L, 10.0),
    (2000L, 12.0),
    (3000L, 11.5),
    (4000L, 13.0),
    (5000L, 9.5),
    (6000L, 11.0),
    (7000L, 50.0) // Anomalous spike
  ]
  
  let pattern_anomalies = AnomalyDetector::detect_pattern_anomalies(
    anomaly_detector,
    time_series_data,
    PatternType::Seasonal
  )
  
  assert_true(pattern_anomalies.length() > 0)
  
  // Test rule-based anomaly detection
  let anomaly_rules = [
    AnomalyRule::new("negative_values", "value < 0"),
    AnomalyRule::new("extreme_values", "value > 1000"),
    AnomalyRule::new("invalid_dates", "timestamp < 0")
  ]
  
  let test_records = [
    [("value", IntValue(100)), ("timestamp", IntValue(1640995200L))],
    [("value", IntValue(-5)), ("timestamp", IntValue(1640995300L))], // Anomalous: negative value
    [("value", IntValue(1500)), ("timestamp", IntValue(1640995400L))] // Anomalous: extreme value
  ]
  
  let rule_anomalies = AnomalyDetector::detect_rule_anomalies(
    anomaly_detector,
    test_records,
    anomaly_rules
  )
  
  assert_eq(rule_anomalies.length(), 2)
}

// Test 7: Data Integrity Verification
test "data integrity verification" {
  let integrity_verifier = IntegrityVerifier::new()
  
  // Test checksum verification
  let data = "sample data for integrity testing"
  let checksum = IntegrityVerifier::calculate_checksum(integrity_verifier, data)
  
  let is_valid = IntegrityVerifier::verify_checksum(
    integrity_verifier,
    data,
    checksum
  )
  
  assert_true(is_valid)
  
  // Test with corrupted data
  let corrupted_data = "corrupted data for integrity testing"
  let is_corrupted = IntegrityVerifier::verify_checksum(
    integrity_verifier,
    corrupted_data,
    checksum
  )
  
  assert_false(is_corrupted)
  
  // Test hash chain verification
  let data_blocks = [
    "block1 data",
    "block2 data",
    "block3 data"
  ]
  
  let hash_chain = IntegrityVerifier::create_hash_chain(integrity_verifier, data_blocks)
  let chain_valid = IntegrityVerifier::verify_hash_chain(
    integrity_verifier,
    data_blocks,
    hash_chain
  )
  
  assert_true(chain_valid)
  
  // Test with modified block
  let modified_blocks = [
    "block1 data",
    "modified block2 data",
    "block3 data"
  ]
  
  let chain_invalid = IntegrityVerifier::verify_hash_chain(
    integrity_verifier,
    modified_blocks,
    hash_chain
  )
  
  assert_false(chain_invalid)
}

// Test 8: Data Validation Workflow
test "data validation workflow" {
  let workflow = ValidationWorkflow::new()
  
  // Define validation steps
  let schema_step = ValidationStep::new(
    "schema_validation",
    ValidationType::Schema,
    ["User", "Order", "Product"]
  )
  
  let quality_step = ValidationStep::new(
    "quality_validation",
    ValidationType::Quality,
    ["completeness", "accuracy", "uniqueness"]
  )
  
  let integrity_step = ValidationStep::new(
    "integrity_validation",
    ValidationType::Integrity,
    ["checksum", "hash_chain"]
  )
  
  ValidationWorkflow::add_step(workflow, schema_step)
  ValidationWorkflow::add_step(workflow, quality_step)
  ValidationWorkflow::add_step(workflow, integrity_step)
  
  // Test workflow execution
  let test_data = [
    [("id", IntValue(1)), ("name", StringValue("Test User")), ("email", StringValue("test@example.com"))],
    [("id", IntValue(2)), ("name", StringValue("Another User")), ("email", StringValue("another@example.com"))]
  ]
  
  let workflow_result = ValidationWorkflow::execute(workflow, test_data)
  
  assert_true(workflow_result.all_steps_completed)
  assert_eq(workflow_result.results.length(), 3) // Three validation steps
  
  // Test workflow with failure handling
  let workflow_with_failure_handling = ValidationWorkflow::with_failure_handling(
    workflow,
    FailureStrategy::Continue // Continue on validation failures
  )
  
  let invalid_data = [
    [("id", StringValue("not_an_int")), ("name", StringValue(""))] // Invalid data
  ]
  
  let failure_result = ValidationWorkflow::execute(workflow_with_failure_handling, invalid_data)
  
  assert_false(failure_result.all_successful)
  assert_true(failure_result.has_failures)
  assert_eq(failure_result.failure_count, 1)
}