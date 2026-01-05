// Azimuth Telemetry Data Integrity Tests
// This file contains test cases for telemetry data integrity verification

// Test 1: Telemetry Data Checksum Validation
test "telemetry data checksum validation" {
  // Define checksum algorithms
  enum ChecksumAlgorithm {
    CRC32
    MD5
    SHA256
    Adler32
    SimpleSum
  }
  
  // Define telemetry data with integrity metadata
  type IntegrityTelemetry = {
    id: String,
    payload: String,
    checksum: String,
    algorithm: ChecksumAlgorithm,
    timestamp: Int
  }
  
  // Simple checksum calculation (simulated)
  let calculate_checksum = fn(data: String, algorithm: ChecksumAlgorithm) {
    match algorithm {
      ChecksumAlgorithm::SimpleSum => {
        let sum = data.chars().fold(0, fn(acc, char) { 
          acc + char.to_int() 
        })
        sum.to_string()
      }
      ChecksumAlgorithm::CRC32 => {
        // Simulated CRC32 calculation
        let hash = data.length() * 31 + data.chars().fold(0, fn(acc, char) { 
          acc + char.to_int() * 17 
        })
        "crc32_" + (hash % 1000000000).to_string()
      }
      ChecksumAlgorithm::MD5 => {
        // Simulated MD5 calculation
        let hash = data.length() * 13 + data.chars().fold(0, fn(acc, char) { 
          acc + char.to_int() * 7 
        })
        "md5_" + (hash % 1000000000).to_string()
      }
      ChecksumAlgorithm::SHA256 => {
        // Simulated SHA256 calculation
        let hash = data.length() * 23 + data.chars().fold(0, fn(acc, char) { 
          acc + char.to_int() * 11 
        })
        "sha256_" + (hash % 1000000000).to_string()
      }
      ChecksumAlgorithm::Adler32 => {
        // Simulated Adler32 calculation
        let hash = data.length() * 29 + data.chars().fold(0, fn(acc, char) { 
          acc + char.to_int() * 19 
        })
        "adler32_" + (hash % 1000000000).to_string()
      }
    }
  }
  
  // Verify telemetry data integrity
  let verify_integrity = fn(telemetry: IntegrityTelemetry) {
    let calculated_checksum = calculate_checksum(telemetry.payload, telemetry.algorithm)
    {
      is_valid: calculated_checksum == telemetry.checksum,
      expected_checksum: calculated_checksum,
      actual_checksum: telemetry.checksum,
      algorithm: telemetry.algorithm
    }
  }
  
  // Create telemetry with integrity
  let create_integrity_telemetry = fn(id: String, payload: String, algorithm: ChecksumAlgorithm) {
    let checksum = calculate_checksum(payload, algorithm)
    {
      id: id,
      payload: payload,
      checksum: checksum,
      algorithm: algorithm,
      timestamp: 1640995200
    }
  }
  
  // Test checksum calculation and verification
  let payload1 = "trace_id=abc123,span_name=database_query,duration=250,status=success"
  let telemetry1 = create_integrity_telemetry("tele-1", payload1, ChecksumAlgorithm::SimpleSum)
  
  let verification1 = verify_integrity(telemetry1)
  assert_true(verification1.is_valid)
  assert_eq(verification1.expected_checksum, verification1.actual_checksum)
  
  // Test different algorithms
  let telemetry_crc32 = create_integrity_telemetry("tele-2", payload1, ChecksumAlgorithm::CRC32)
  let verification_crc32 = verify_integrity(telemetry_crc32)
  assert_true(verification_crc32.is_valid)
  assert_true(verification_crc32.expected_checksum.starts_with("crc32_"))
  
  let telemetry_md5 = create_integrity_telemetry("tele-3", payload1, ChecksumAlgorithm::MD5)
  let verification_md5 = verify_integrity(telemetry_md5)
  assert_true(verification_md5.is_valid)
  assert_true(verification_md5.expected_checksum.starts_with("md5_"))
  
  // Test integrity failure detection
  let corrupted_telemetry = {
    id: "tele-4",
    payload: payload1 + "corrupted",
    checksum: telemetry1.checksum,  // Original checksum
    algorithm: ChecksumAlgorithm::SimpleSum,
    timestamp: 1640995200
  }
  
  let corrupted_verification = verify_integrity(corrupted_telemetry)
  assert_false(corrupted_verification.is_valid)
  assert_not_eq(corrupted_verification.expected_checksum, corrupted_verification.actual_checksum)
  
  // Test batch integrity verification
  let verify_batch_integrity = fn(batch: Array[IntegrityTelemetry>) {
    let mut valid_count = 0
    let mut invalid_count = 0
    let mut failed_items = []
    
    for item in batch {
      let verification = verify_integrity(item)
      if verification.is_valid {
        valid_count = valid_count + 1
      } else {
        invalid_count = invalid_count + 1
        failed_items = failed_items.push(item.id)
      }
    }
    
    {
      total_items: batch.length(),
      valid_count: valid_count,
      invalid_count: invalid_count,
      success_rate: (valid_count as Float) / (batch.length() as Float),
      failed_items: failed_items
    }
  }
  
  let telemetry_batch = [
    telemetry1,
    telemetry_crc32,
    telemetry_md5,
    corrupted_telemetry
  ]
  
  let batch_verification = verify_batch_integrity(telemetry_batch)
  assert_eq(batch_verification.total_items, 4)
  assert_eq(batch_verification.valid_count, 3)
  assert_eq(batch_verification.invalid_count, 1)
  assert_eq(batch_verification.success_rate, 0.75)
  assert_eq(batch_verification.failed_items.length(), 1)
  assert_eq(batch_verification.failed_items[0], "tele-4")
}

// Test 2: Telemetry Data Sequence Validation
test "telemetry data sequence validation" {
  // Define sequence validation types
  enum SequenceType {
    MonotonicIncreasing
    MonotonicDecreasing
    Sequential
    Bounded(Int, Int)  // min, max
  }
  
  // Define sequence validator
  type SequenceValidator = {
    sequence_type: SequenceType,
    tolerance: Int,  // Allowed deviation
    window_size: Int  // Validation window size
  }
  
  // Define sequence validation result
  type ValidationResult = {
    is_valid: Bool,
    violations: Array[Int],  // Indices of violations
    violation_count: Int,
    total_items: Int
  }
  
  // Validate sequence integrity
  let validate_sequence = fn(values: Array[Int], validator: SequenceValidator) {
    let mut violations = []
    
    match validator.sequence_type {
      SequenceType::MonotonicIncreasing => {
        for i in 1..values.length() {
          if values[i] < values[i-1] - validator.tolerance {
            violations = violations.push(i)
          }
        }
      }
      SequenceType::MonotonicDecreasing => {
        for i in 1..values.length() {
          if values[i] > values[i-1] + validator.tolerance {
            violations = violations.push(i)
          }
        }
      }
      SequenceType::Sequential => {
        for i in 1..values.length() {
          if values[i] != values[i-1] + 1 {
            violations = violations.push(i)
          }
        }
      }
      SequenceType::Bounded(min, max) => {
        for i in 0..values.length() {
          if values[i] < min || values[i] > max {
            violations = violations.push(i)
          }
        }
      }
    }
    
    {
      is_valid: violations.length() == 0,
      violations: violations,
      violation_count: violations.length(),
      total_items: values.length()
    }
  }
  
  // Test monotonic increasing sequence
  let monotonic_validator = {
    sequence_type: SequenceType::MonotonicIncreasing,
    tolerance: 0,
    window_size: 10
  }
  
  let increasing_timestamps = [1000, 1010, 1020, 1030, 1040, 1050]
  let increasing_validation = validate_sequence(increasing_timestamps, monotonic_validator)
  assert_true(increasing_validation.is_valid)
  assert_eq(increasing_validation.violation_count, 0)
  
  let violated_timestamps = [1000, 1010, 1005, 1030, 1020, 1050]
  let violated_validation = validate_sequence(violated_timestamps, monotonic_validator)
  assert_false(violated_validation.is_valid)
  assert_eq(violated_validation.violation_count, 2)
  assert_eq(violated_validation.violations, [2, 4])
  
  // Test with tolerance
  let tolerant_validator = {
    sequence_type: SequenceType::MonotonicIncreasing,
    tolerance: 10,
    window_size: 10
  }
  
  let tolerant_validation = validate_sequence(violated_timestamps, tolerant_validator)
  assert_true(tolerant_validation.is_valid)  // Within tolerance
  
  // Test sequential validation
  let sequential_validator = {
    sequence_type: SequenceType::Sequential,
    tolerance: 0,
    window_size: 10
  }
  
  let sequence_numbers = [1, 2, 3, 4, 5, 6]
  let sequential_validation = validate_sequence(sequence_numbers, sequential_validator)
  assert_true(sequential_validation.is_valid)
  
  let broken_sequence = [1, 2, 4, 5, 7, 8]
  let broken_validation = validate_sequence(broken_sequence, sequential_validator)
  assert_false(broken_validation.is_valid)
  assert_eq(broken_validation.violation_count, 2)
  
  // Test bounded validation
  let bounded_validator = {
    sequence_type: SequenceType::Bounded(100, 200),
    tolerance: 0,
    window_size: 10
  }
  
  let bounded_values = [120, 150, 180, 190, 200]
  let bounded_validation = validate_sequence(bounded_values, bounded_validator)
  assert_true(bounded_validation.is_valid)
  
  let out_of_bounds = [120, 250, 180, 50, 200]
  let bounds_validation = validate_sequence(out_of_bounds, bounded_validator)
  assert_false(bounds_validation.is_valid)
  assert_eq(bounds_validation.violation_count, 2)
  
  // Test telemetry sequence validation with real-world data
  type TelemetryPoint = {
    timestamp: Int,
    sequence_number: Int,
    metric_value: Float
  }
  
  let validate_telemetry_sequence = fn(points: Array[TelemetryPoint]) {
    let timestamps = points.map_fn(p) { p.timestamp }
    let sequence_numbers = points.map_fn(p) { p.sequence_number }
    
    let timestamp_validator = {
      sequence_type: SequenceType::MonotonicIncreasing,
      tolerance: 0,
      window_size: 10
    }
    
    let sequence_validator = {
      sequence_type: SequenceType::Sequential,
      tolerance: 0,
      window_size: 10
    }
    
    let timestamp_validation = validate_sequence(timestamps, timestamp_validator)
    let sequence_validation = validate_sequence(sequence_numbers, sequence_validator)
    
    {
      timestamp_valid: timestamp_validation.is_valid,
      sequence_valid: sequence_validation.is_valid,
      overall_valid: timestamp_validation.is_valid && sequence_validation.is_valid,
      timestamp_violations: timestamp_validation.violations,
      sequence_violations: sequence_validation.violations
    }
  }
  
  let telemetry_points = [
    { timestamp: 1000, sequence_number: 1, metric_value: 95.5 },
    { timestamp: 1010, sequence_number: 2, metric_value: 97.2 },
    { timestamp: 1020, sequence_number: 3, metric_value: 94.8 },
    { timestamp: 1030, sequence_number: 4, metric_value: 96.1 },
    { timestamp: 1040, sequence_number: 5, metric_value: 95.9 }
  ]
  
  let telemetry_validation = validate_telemetry_sequence(telemetry_points)
  assert_true(telemetry_validation.timestamp_valid)
  assert_true(telemetry_validation.sequence_valid)
  assert_true(telemetry_validation.overall_valid)
  
  let corrupted_telemetry_points = [
    { timestamp: 1000, sequence_number: 1, metric_value: 95.5 },
    { timestamp: 1010, sequence_number: 2, metric_value: 97.2 },
    { timestamp: 1005, sequence_number: 4, metric_value: 94.8 },  // Timestamp violation
    { timestamp: 1030, sequence_number: 3, metric_value: 96.1 },  // Sequence violation
    { timestamp: 1040, sequence_number: 5, metric_value: 95.9 }
  ]
  
  let corrupted_telemetry_validation = validate_telemetry_sequence(corrupted_telemetry_points)
  assert_false(corrupted_telemetry_validation.timestamp_valid)
  assert_false(corrupted_telemetry_validation.sequence_valid)
  assert_false(corrupted_telemetry_validation.overall_valid)
  assert_eq(corrupted_telemetry_validation.timestamp_violations, [2])
  assert_eq(corrupted_telemetry_validation.sequence_violations, [3])
}

// Test 3: Telemetry Data Consistency Validation
test "telemetry data consistency validation" {
  // Define consistency rules
  enum ConsistencyRule {
    RequiredAttribute(String)           // Attribute name that must be present
    AttributeType(String, String)       // Attribute name and expected type
    AttributeRange(String, Float, Float) // Attribute name and min/max values
    ConditionalDependency(String, String, String) // If attribute1 has value, then attribute2 must exist
    CrossAttributeConsistency(String, String)     // Two attributes must be consistent
  }
  
  // Define consistency validator
  type ConsistencyValidator = {
    rules: Array[ConsistencyRule],
    strict_mode: Bool  // Fail fast or collect all violations
  }
  
  // Define consistency validation result
  type ConsistencyResult = {
    is_consistent: Bool,
    violations: Array[String],
    rule_results: Array[(String, Bool)]
  }
  
  // Validate telemetry data consistency
  let validate_consistency = fn(attributes: Array[(String, String)], validator: ConsistencyValidator) {
    let mut violations = []
    let mut rule_results = []
    
    for rule in validator.rules {
      let (rule_name, is_valid) = match rule {
        ConsistencyRule::RequiredAttribute(attr_name) => {
          let is_present = attributes.some_fn(a) { a.0 == attr_name }
          ("required_" + attr_name, is_present)
        }
        ConsistencyRule::AttributeType(attr_name, expected_type) => {
          match attributes.find_fn(a) { a.0 == attr_name } {
            Some((_, value)) => {
              let actual_type = match value.parse_float() {
                Some(_) => "number"
                None => {
                  if value == "true" || value == "false" { "boolean" }
                  else { "string" }
                }
              }
              ("type_" + attr_name, actual_type == expected_type)
            }
            None => ("type_" + attr_name, false)  // Attribute not found
          }
        }
        ConsistencyRule::AttributeRange(attr_name, min_val, max_val) => {
          match attributes.find_fn(a) { a.0 == attr_name } {
            Some((_, value)) => {
              match value.parse_float() {
                Some(num_val) => {
                  ("range_" + attr_name, num_val >= min_val && num_val <= max_val)
                }
                None => ("range_" + attr_name, false)  // Not a number
              }
            }
            None => ("range_" + attr_name, false)  // Attribute not found
          }
        }
        ConsistencyRule::ConditionalDependency(cond_attr, cond_value, req_attr) => {
          match attributes.find_fn(a) { a.0 == cond_attr } {
            Some((_, value)) => {
              if value == cond_value {
                let has_required = attributes.some_fn(a) { a.0 == req_attr }
                ("conditional_" + cond_attr + "_" + req_attr, has_required)
              } else {
                ("conditional_" + cond_attr + "_" + req_attr, true)  // Condition not met
              }
            }
            None => ("conditional_" + cond_attr + "_" + req_attr, true)  // Condition not met
          }
        }
        ConsistencyRule::CrossAttributeConsistency(attr1, attr2) => {
          match (attributes.find_fn(a) { a.0 == attr1 }, attributes.find_fn(a) { a.0 == attr2 }) {
            (Some((_, val1)), Some((_, val2))) => {
              // Simple consistency check: values should not be identical unless it makes sense
              let is_consistent = match (attr1, attr2) {
                ("start_time", "end_time") => {
                  match (val1.parse_int(), val2.parse_int()) {
                    (Some(start), Some(end)) => start < end
                    _ => false
                  }
                }
                ("status", "error") => {
                  val1 != "error" || val2.length() > 0
                }
                _ => {
                  val1 != val2  // Default: different values
                }
              }
              ("cross_" + attr1 + "_" + attr2, is_consistent)
            }
            _ => ("cross_" + attr1 + "_" + attr2, false)  // One or both attributes missing
          }
        }
      }
      
      rule_results = rule_results.push((rule_name, is_valid))
      
      if not(is_valid) {
        violations = violations.push(rule_name + "_violation")
        
        if validator.strict_mode {
          break  // Fail fast
        }
      }
    }
    
    {
      is_consistent: violations.length() == 0,
      violations: violations,
      rule_results: rule_results
    }
  }
  
  // Test consistency validation
  let validator = {
    rules: [
      ConsistencyRule::RequiredAttribute("trace_id"),
      ConsistencyRule::RequiredAttribute("span_name"),
      ConsistencyRule::AttributeType("duration_ms", "number"),
      ConsistencyRule::AttributeRange("duration_ms", 0.0, 3600000.0),  // 0 to 1 hour
      ConsistencyRule::ConditionalDependency("status", "error", "error_message"),
      ConsistencyRule::CrossAttributeConsistency("start_time", "end_time")
    ],
    strict_mode: false
  }
  
  let valid_attributes = [
    ("trace_id", "trace-12345"),
    ("span_name", "database_query"),
    ("duration_ms", "250"),
    ("status", "success"),
    ("start_time", "1640995200"),
    ("end_time", "1640995450")
  ]
  
  let valid_validation = validate_consistency(valid_attributes, validator)
  assert_true(valid_validation.is_consistent)
  assert_eq(valid_validation.violations.length(), 0)
  
  let invalid_attributes = [
    ("span_name", "database_query"),  // Missing trace_id
    ("duration_ms", "250"),
    ("status", "error"),  // Missing error_message
    ("start_time", "1640995450"),
    ("end_time", "1640995200")  // End time before start time
  ]
  
  let invalid_validation = validate_consistency(invalid_attributes, validator)
  assert_false(invalid_validation.is_consistent)
  assert_eq(invalid_validation.violations.length(), 3)
  assert_true(invalid_validation.violations.some_fn(v) { v.contains("required_trace_id") })
  assert_true(invalid_validation.violations.some_fn(v) { v.contains("conditional_status_error_message") })
  assert_true(invalid_validation.violations.some_fn(v) { v.contains("cross_start_time_end_time") })
  
  // Test edge case validation
  let edge_case_attributes = [
    ("trace_id", "trace-67890"),
    ("span_name", "api_call"),
    ("duration_ms", "-100"),  // Negative duration
    ("status", "error"),
    ("error_message", ""),    // Empty error message
    ("start_time", "1640995600"),
    ("end_time", "1640995600")  // Same time (zero duration)
  ]
  
  let edge_case_validation = validate_consistency(edge_case_attributes, validator)
  assert_false(edge_case_validation.is_consistent)
  assert_eq(edge_case_validation.violations.length(), 2)
  assert_true(edge_case_validation.violations.some_fn(v) { v.contains("range_duration_ms") })
  assert_true(edge_case_validation.violations.some_fn(v) { v.contains("cross_start_time_end_time") })
  
  // Test batch consistency validation
  let validate_batch_consistency = fn(batch: Array<Array[(String, String)]>, validator: ConsistencyValidator) {
    let mut consistent_count = 0
    let mut inconsistent_count = 0
    let mut batch_results = []
    
    for (index, attributes) in batch.enumerate() {
      let validation = validate_consistency(attributes, validator)
      batch_results = batch_results.push({
        batch_index: index,
        is_consistent: validation.is_consistent,
        violation_count: validation.violations.length(),
        violations: validation.violations
      })
      
      if validation.is_consistent {
        consistent_count = consistent_count + 1
      } else {
        inconsistent_count = inconsistent_count + 1
      }
    }
    
    {
      total_batches: batch.length(),
      consistent_count: consistent_count,
      inconsistent_count: inconsistent_count,
      consistency_rate: (consistent_count as Float) / (batch.length() as Float),
      batch_results: batch_results
    }
  }
  
  let attribute_batch = [
    valid_attributes,
    invalid_attributes,
    edge_case_attributes,
    [
      ("trace_id", "trace-11111"),
      ("span_name", "cache_operation"),
      ("duration_ms", "50"),
      ("status", "success")
    ]
  ]
  
  let batch_consistency = validate_batch_consistency(attribute_batch, validator)
  assert_eq(batch_consistency.total_batches, 4)
  assert_eq(batch_consistency.consistent_count, 2)
  assert_eq(batch_consistency.inconsistent_count, 2)
  assert_eq(batch_consistency.consistency_rate, 0.5)
}

// Test 4: Telemetry Data Completeness Validation
test "telemetry data completeness validation" {
  // Define completeness requirements
  type CompletenessRequirement = {
    attribute_name: String,
    required: Bool,
    default_value: Option[String>,
    validation_fn: Option<(String) -> Bool>
  }
  
  // Define completeness validator
  type CompletenessValidator = {
    requirements: Array[CompletenessRequirement],
    completeness_threshold: Float  // 0.0 to 1.0
  }
  
  // Define completeness result
  type CompletenessResult = {
    completeness_score: Float,
    is_complete: Bool,
    missing_attributes: Array[String],
    invalid_attributes: Array[String],
    filled_attributes: Array[String>
  }
  
  // Validate telemetry data completeness
  let validate_completeness = fn(attributes: Array[(String, String)], validator: CompletenessValidator) {
    let mut present_count = 0
    let mut missing_attributes = []
    let mut invalid_attributes = []
    let mut filled_attributes = []
    
    for requirement in validator.requirements {
      match attributes.find_fn(a) { a.0 == requirement.attribute_name } {
        Some((key, value)) => {
          // Check if value is valid
          let is_valid = match requirement.validation_fn {
            Some(validation_fn) => validation_fn(value)
            None => value.length() > 0  // Default validation: non-empty
          }
          
          if is_valid {
            present_count = present_count + 1
            filled_attributes = filled_attributes.push(key)
          } else {
            invalid_attributes = invalid_attributes.push(key)
          }
        }
        None => {
          if requirement.required {
            missing_attributes = missing_attributes.push(requirement.attribute_name)
          }
          
          // Check if default value can be used
          match requirement.default_value {
            Some(default) => {
              present_count = present_count + 1
              filled_attributes = filled_attributes.push(requirement.attribute_name + " (default)")
            }
            None => {}  // No default, leave as missing
          }
        }
      }
    }
    
    let completeness_score = (present_count as Float) / (validator.requirements.length() as Float)
    let is_complete = completeness_score >= validator.completeness_threshold
    
    {
      completeness_score: completeness_score,
      is_complete: is_complete,
      missing_attributes: missing_attributes,
      invalid_attributes: invalid_attributes,
      filled_attributes: filled_attributes
    }
  }
  
  // Test completeness validation
  let completeness_validator = {
    requirements: [
      {
        attribute_name: "trace_id",
        required: true,
        default_value: None,
        validation_fn: Some(fn(value) { value.length() >= 8 })
      },
      {
        attribute_name: "span_name",
        required: true,
        default_value: None,
        validation_fn: Some(fn(value) { value.length() > 0 })
      },
      {
        attribute_name: "duration_ms",
        required: true,
        default_value: Some("0"),
        validation_fn: Some(fn(value) { 
          match value.parse_int() {
            Some(num) => num >= 0
            None => false
          }
        })
      },
      {
        attribute_name: "status",
        required: false,
        default_value: Some("unknown"),
        validation_fn: Some(fn(value) { 
          ["success", "error", "timeout", "unknown"].some_fn(s) { s == value }
        })
      },
      {
        attribute_name: "service_name",
        required: true,
        default_value: None,
        validation_fn: Some(fn(value) { value.length() >= 3 })
      },
      {
        attribute_name: "environment",
        required: false,
        default_value: Some("development"),
        validation_fn: None
      }
    ],
    completeness_threshold: 0.8  // 80% completeness required
  }
  
  let complete_attributes = [
    ("trace_id", "trace-12345678"),
    ("span_name", "database_query"),
    ("duration_ms", "250"),
    ("status", "success"),
    ("service_name", "payment-service"),
    ("environment", "production")
  ]
  
  let complete_validation = validate_completeness(complete_attributes, completeness_validator)
  assert_eq(complete_validation.completeness_score, 1.0)
  assert_true(complete_validation.is_complete)
  assert_eq(complete_validation.missing_attributes.length(), 0)
  assert_eq(complete_validation.invalid_attributes.length(), 0)
  assert_eq(complete_validation.filled_attributes.length(), 6)
  
  let partial_attributes = [
    ("trace_id", "trace-12"),  // Too short
    ("span_name", "api_call"),
    ("duration_ms", "-100"),  // Invalid negative
    ("service_name", "api")   // Too short
  ]
  
  let partial_validation = validate_completeness(partial_attributes, completeness_validator)
  assert_true(partial_validation.completeness_score < 0.8)
  assert_false(partial_validation.is_complete)
  assert_eq(partial_validation.missing_attributes.length(), 0)  // All required present
  assert_eq(partial_validation.invalid_attributes.length(), 3)
  assert_true(partial_validation.invalid_attributes.contains("trace_id"))
  assert_true(partial_validation.invalid_attributes.contains("duration_ms"))
  assert_true(partial_validation.invalid_attributes.contains("service_name"))
  
  // Test with default values
  let minimal_attributes = [
    ("span_name", "cache_operation"),
    ("duration_ms", "50"),
    ("service_name", "auth-service")
  ]
  
  let minimal_validation = validate_completeness(minimal_attributes, completeness_validator)
  assert_eq(minimal_validation.completeness_score, 0.8333333333333334)  // 5/6 with defaults
  assert_true(minimal_validation.is_complete)
  assert_eq(minimal_validation.missing_attributes.length(), 1)  // trace_id
  assert_eq(minimal_validation.filled_attributes.length(), 5)
  assert_true(minimal_validation.filled_attributes.some_fn(a) { a.contains("default") })
  
  // Test batch completeness validation
  let validate_batch_completeness = fn(batch: Array<Array[(String, String)]>, validator: CompletenessValidator) {
    let mut batch_results = []
    let mut total_completeness = 0.0
    
    for (index, attributes) in batch.enumerate() {
      let validation = validate_completeness(attributes, validator)
      batch_results = batch_results.push({
        batch_index: index,
        completeness_score: validation.completeness_score,
        is_complete: validation.is_complete,
        missing_count: validation.missing_attributes.length(),
        invalid_count: validation.invalid_attributes.length()
      })
      total_completeness = total_completeness + validation.completeness_score
    }
    
    {
      total_batches: batch.length(),
      average_completeness: total_completeness / (batch.length() as Float),
      complete_batches: batch_results.filter_fn(r) { r.is_complete }.length(),
      incomplete_batches: batch_results.filter_fn(r) { not(r.is_complete) }.length(),
      batch_results: batch_results
    }
  }
  
  let completeness_batch = [
    complete_attributes,
    partial_attributes,
    minimal_attributes,
    []  // Empty
  ]
  
  let batch_completeness = validate_batch_completeness(completeness_batch, completeness_validator)
  assert_eq(batch_completeness.total_batches, 4)
  assert_eq(batch_completeness.complete_batches, 2)
  assert_eq(batch_completeness.incomplete_batches, 2)
  assert_true(batch_completeness.average_completeness > 0.5)
  assert_true(batch_completeness.average_completeness < 1.0)
}