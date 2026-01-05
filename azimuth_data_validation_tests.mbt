// Azimuth Data Validation Test Suite
// This file contains test cases for data validation and integrity checks

// Test 1: Schema Validation
test "schema validation for telemetry data" {
  // Define validation result
  type ValidationResult = {
    is_valid: Bool,
    errors: Array[String],
    warnings: Array[String]
  }
  
  // Define field validator
  type FieldValidator = {
    field_name: String,
    required: Bool,
    validator: (String) -> Bool,
    error_message: String
  }
  
  // Define schema
  type Schema = {
    name: String,
    validators: Array[FieldValidator>
  }
  
  // Create successful validation result
  let success = fn() {
    {
      is_valid: true,
      errors: [],
      warnings: []
    }
  }
  
  // Create validation result with errors
  let failure = fn(errors: Array[String>) {
    {
      is_valid: false,
      errors,
      warnings: []
    }
  }
  
  // Validate string is not empty
  let validate_non_empty = fn(value: String) {
    value.length() > 0
  }
  
  // Validate string matches pattern
  let validate_pattern = fn(pattern: String) {
    fn(value: String) {
      // Simplified pattern matching
      if pattern == "trace_id" {
        value.starts_with("trace-") && value.length() == 16
      } else if pattern == "span_id" {
        value.starts_with("span-") && value.length() == 15
      } else if pattern == "timestamp" {
        value.chars().all(fn(c) { c >= '0' && c <= '9' })
      } else {
        true
      }
    }
  }
  
  // Validate numeric range
  let validate_range = fn(min: Int, max: Int) {
    fn(value: String) {
      match value.parse_int() {
        Some(num) => num >= min && num <= max
        None => false
      }
    }
  }
  
  // Create telemetry span schema
  let span_schema = {
    name: "telemetry_span",
    validators: [
      {
        field_name: "trace_id",
        required: true,
        validator: validate_pattern("trace_id"),
        error_message: "Invalid trace ID format"
      },
      {
        field_name: "span_id",
        required: true,
        validator: validate_pattern("span_id"),
        error_message: "Invalid span ID format"
      },
      {
        field_name: "operation_name",
        required: true,
        validator: validate_non_empty,
        error_message: "Operation name cannot be empty"
      },
      {
        field_name: "start_time",
        required: true,
        validator: validate_pattern("timestamp"),
        error_message: "Invalid start time format"
      },
      {
        field_name: "duration_ms",
        required: false,
        validator: validate_range(0, 3600000),  // Max 1 hour
        error_message: "Duration must be between 0 and 3600000ms"
      }
    ]
  }
  
  // Validate data against schema
  let validate_schema = fn(data: Array[(String, String)>, schema: Schema) {
    let mut errors = []
    let mut warnings = []
    
    for validator in schema.validators {
      let field_value = data.find(fn(field) { field.0 == validator.field_name })
      
      match field_value {
        Some((_, value)) => {
          if not(validator.validator(value)) {
            errors = errors.push(validator.field_name + ": " + validator.error_message)
          }
        }
        None => {
          if validator.required {
            errors = errors.push("Missing required field: " + validator.field_name)
          }
        }
      }
    }
    
    {
      is_valid: errors.length() == 0,
      errors,
      warnings
    }
  }
  
  // Test valid data
  let valid_data = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("operation_name", "database_query"),
    ("start_time", "1640995200"),
    ("duration_ms", "250")
  ]
  
  let valid_result = validate_schema(valid_data, span_schema)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid data
  let invalid_data = [
    ("trace_id", "invalid-trace"),  // Invalid format
    ("span_id", "span-123"),        // Too short
    ("operation_name", ""),         // Empty
    ("start_time", "not-a-number")  // Invalid timestamp
  ]
  
  let invalid_result = validate_schema(invalid_data, span_schema)
  assert_false(invalid_result.is_valid)
  assert_eq(invalid_result.errors.length(), 4)
  assert_true(invalid_result.errors[0].contains("Invalid trace ID"))
  assert_true(invalid_result.errors[1].contains("Invalid span ID"))
  assert_true(invalid_result.errors[2].contains("Operation name cannot be empty"))
  assert_true(invalid_result.errors[3].contains("Invalid start time"))
  
  // Test missing required fields
  let incomplete_data = [
    ("trace_id", "trace-1234567890"),
    // Missing span_id and operation_name
    ("start_time", "1640995200")
  ]
  
  let incomplete_result = validate_schema(incomplete_data, span_schema)
  assert_false(incomplete_result.is_valid)
  assert_eq(incomplete_result.errors.length(), 2)
  assert_true(incomplete_result.errors[0].contains("Missing required field: span_id"))
  assert_true(incomplete_result.errors[1].contains("Missing required field: operation_name"))
  
  // Test optional field validation
  let data_with_optional = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("operation_name", "database_query"),
    ("start_time", "1640995200"),
    ("duration_ms", "5000")  // Valid optional field
  ]
  
  let optional_result = validate_schema(data_with_optional, span_schema)
  assert_true(optional_result.is_valid)
  
  // Test invalid optional field
  let data_with_invalid_optional = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("operation_name", "database_query"),
    ("start_time", "1640995200"),
    ("duration_ms", "5000000")  // Exceeds max
  ]
  
  let invalid_optional_result = validate_schema(data_with_invalid_optional, span_schema)
  assert_false(invalid_optional_result.is_valid)
  assert_eq(invalid_optional_result.errors.length(), 1)
  assert_true(invalid_optional_result.errors[0].contains("Duration must be between 0 and 3600000ms"))
}

// Test 2: Type Validation and Conversion
test "type validation and conversion operations" {
  // Define validation result with converted value
  type TypeValidationResult[T] = {
    is_valid: Bool,
    value: Option[T],
    error: Option[String]
  }
  
  // Validate and convert to integer
  let validate_int = fn(value: String, min: Option[Int>, max: Option<Int>) {
    match value.parse_int() {
      Some(int_value) => {
        let min_valid = match min {
          Some(m) => int_value >= m
          None => true
        }
        
        let max_valid = match max {
          Some(m) => int_value <= m
          None => true
        }
        
        if min_valid && max_valid {
          {
            is_valid: true,
            value: Some(int_value),
            error: None
          }
        } else {
          {
            is_valid: false,
            value: None,
            error: Some("Value out of range")
          }
        }
      }
      None => {
        {
          is_valid: false,
          value: None,
          error: Some("Invalid integer format")
        }
      }
    }
  }
  
  // Validate and convert to float
  let validate_float = fn(value: String, min: Option[Float>, max: Option[Float>) {
    match value.parse_float() {
      Some(float_value) => {
        let min_valid = match min {
          Some(m) => float_value >= m
          None => true
        }
        
        let max_valid = match max {
          Some(m) => float_value <= m
          None => true
        }
        
        if min_valid && max_valid {
          {
            is_valid: true,
            value: Some(float_value),
            error: None
          }
        } else {
          {
            is_valid: false,
            value: None,
            error: Some("Value out of range")
          }
        }
      }
      None => {
        {
          is_valid: false,
          value: None,
          error: Some("Invalid float format")
        }
      }
    }
  }
  
  // Validate boolean
  let validate_bool = fn(value: String) {
    let lower_value = value.to_lowercase()
    if lower_value == "true" || lower_value == "1" || lower_value == "yes" {
      {
        is_valid: true,
        value: Some(true),
        error: None
      }
    } else if lower_value == "false" || lower_value == "0" || lower_value == "no" {
      {
        is_valid: true,
        value: Some(false),
        error: None
      }
    } else {
      {
        is_valid: false,
        value: None,
        error: Some("Invalid boolean format")
      }
    }
  }
  
  // Validate timestamp
  let validate_timestamp = fn(value: String) {
    match value.parse_int() {
      Some(timestamp) => {
        // Check if timestamp is in reasonable range (2000 to 2030)
        if timestamp >= 946684800 && timestamp <= 1893456000 {
          {
            is_valid: true,
            value: Some(timestamp),
            error: None
          }
        } else {
          {
            is_valid: false,
            value: None,
            error: Some("Timestamp out of valid range")
          }
        }
      }
      None => {
        {
          is_valid: false,
          value: None,
          error: Some("Invalid timestamp format")
        }
      }
    }
  }
  
  // Test integer validation
  let valid_int = validate_int("42", Some(0), Some(100))
  assert_true(valid_int.is_valid)
  assert_eq(valid_int.value, Some(42))
  
  let invalid_int_range = validate_int("150", Some(0), Some(100))
  assert_false(invalid_int_range.is_valid)
  assert_eq(invalid_int_range.error, Some("Value out of range"))
  
  let invalid_int_format = validate_int("not-a-number", Some(0), Some(100))
  assert_false(invalid_int_format.is_valid)
  assert_eq(invalid_int_format.error, Some("Invalid integer format"))
  
  // Test float validation
  let valid_float = validate_float("3.14", Some(0.0), Some(10.0))
  assert_true(valid_float.is_valid)
  assert_eq(valid_float.value, Some(3.14))
  
  let invalid_float_range = validate_float("15.5", Some(0.0), Some(10.0))
  assert_false(invalid_float_range.is_valid)
  assert_eq(invalid_float_range.error, Some("Value out of range"))
  
  // Test boolean validation
  let true_values = ["true", "TRUE", "1", "yes", "YES"]
  for true_val in true_values {
    let result = validate_bool(true_val)
    assert_true(result.is_valid)
    assert_eq(result.value, Some(true))
  }
  
  let false_values = ["false", "FALSE", "0", "no", "NO"]
  for false_val in false_values {
    let result = validate_bool(false_val)
    assert_true(result.is_valid)
    assert_eq(result.value, Some(false))
  }
  
  let invalid_bool = validate_bool("maybe")
  assert_false(invalid_bool.is_valid)
  assert_eq(invalid_bool.error, Some("Invalid boolean format"))
  
  // Test timestamp validation
  let valid_timestamp = validate_timestamp("1640995200")  // 2022-01-01
  assert_true(valid_timestamp.is_valid)
  assert_eq(valid_timestamp.value, Some(1640995200))
  
  let old_timestamp = validate_timestamp("500000000")  // 1985-11-05
  assert_false(old_timestamp.is_valid)
  assert_eq(old_timestamp.error, Some("Timestamp out of valid range"))
  
  let future_timestamp = validate_timestamp("2000000000")  // 2033-05-18
  assert_false(future_timestamp.is_valid)
  assert_eq(future_timestamp.error, Some("Timestamp out of valid range"))
  
  // Test array validation
  let validate_array = fn(value: String, item_validator: (String) -> TypeValidationResult[String>) {
    if value.starts_with("[") && value.ends_with("]") {
      let content = value.substring(1, value.length() - 2)
      if content.length() == 0 {
        {
          is_valid: true,
          value: Some([]),
          error: None
        }
      } else {
        let items = content.split(",")
        let mut validated_items = []
        let mut has_errors = false
        let mut errors = []
        
        for item in items {
          let trimmed = item.trim()
          let item_result = item_validator(trimmed)
          
          if item_result.is_valid {
            match item_result.value {
              Some(v) => validated_items = validated_items.push(v)
              None => {}
            }
          } else {
            has_errors = true
            match item_result.error {
              Some(e) => errors = errors.push(e)
              None => {}
            }
          }
        }
        
        if has_errors {
          {
            is_valid: false,
            value: None,
            error: Some("Array contains invalid items: " + errors.join(", "))
          }
        } else {
          {
            is_valid: true,
            value: Some(validated_items),
            error: None
          }
        }
      }
    } else {
      {
        is_valid: false,
        value: None,
        error: Some("Invalid array format")
      }
    }
  }
  
  // Test array validation
  let string_validator = fn(s: String) {
    {
      is_valid: s.length() > 0,
      value: Some(s),
      error: if s.length() == 0 { Some("Empty string") } else { None }
    }
  }
  
  let valid_array = validate_array("[item1,item2,item3]", string_validator)
  assert_true(valid_array.is_valid)
  assert_eq(valid_array.value, Some(["item1", "item2", "item3"]))
  
  let invalid_array_format = validate_array("item1,item2,item3", string_validator)
  assert_false(invalid_array_format.is_valid)
  assert_eq(invalid_array_format.error, Some("Invalid array format"))
  
  let empty_array = validate_array("[]", string_validator)
  assert_true(empty_array.is_valid)
  assert_eq(empty_array.value, Some([]))
}

// Test 3: Business Rule Validation
test "business rule validation for telemetry constraints" {
  // Define business rule
  type BusinessRule = {
    rule_id: String,
    name: String,
    description: String,
    validator: (Array[(String, String)>) -> Bool,
    error_message: String
  }
  
  // Define rule engine
  type RuleEngine = {
    rules: Array[BusinessRule>
  }
  
  // Create rule engine
  let create_rule_engine = fn(rules: Array[BusinessRule>) {
    { rules }
  }
  
  // Validate data against business rules
  let validate_business_rules = fn(data: Array[(String, String)>, engine: RuleEngine) {
    let mut violations = []
    
    for rule in engine.rules {
      if not(rule.validator(data)) {
        violations = violations.push({
          rule_id: rule.rule_id,
          name: rule.name,
          error_message: rule.error_message
        })
      }
    }
    
    {
      is_valid: violations.length() == 0,
      violations
    }
  }
  
  // Get field value from data
  let get_field = fn(data: Array[(String, String)>, field_name: String) {
    data.find(fn(field) { field.0 == field_name })
  }
  
  // Define business rules for telemetry data
  let telemetry_rules = [
    {
      rule_id: "span_duration_check",
      name: "Span Duration Validation",
      description: "Span duration should be positive and reasonable",
      validator: fn(data: Array[(String, String)>) {
        match get_field(data, "start_time") {
          Some((_, start_str)) => {
            match get_field(data, "end_time") {
              Some((_, end_str)) => {
                match (start_str.parse_int(), end_str.parse_int()) {
                  (Some(start), Some(end)) => {
                    let duration = end - start
                    duration > 0 && duration <= 3600000  // Max 1 hour
                  }
                  _ => false
                }
              }
              None => true  // End time is optional
            }
          }
          None => false
        }
      },
      error_message: "Span duration must be positive and less than 1 hour"
    },
    {
      rule_id: "trace_hierarchy_check",
      name: "Trace Hierarchy Validation",
      description: "Child spans must have parent span in same trace",
      validator: fn(data: Array[(String, String)>) {
        match get_field(data, "trace_id") {
          Some((_, trace_id)) => {
            match get_field(data, "parent_span_id") {
              Some((_, parent_span_id)) => {
                // In a real implementation, this would check if parent span exists
                parent_span_id.starts_with("span-")
              }
              None => true  // Root span doesn't need parent
            }
          }
          None => false
        }
      },
      error_message: "Parent span ID must be valid when present"
    },
    {
      rule_id: "service_name_check",
      name: "Service Name Validation",
      description: "Service name should follow naming conventions",
      validator: fn(data: Array[(String, String)>) {
        match get_field(data, "service_name") {
          Some((_, service_name)) => {
            // Service names should be lowercase with hyphens
            service_name.length() > 0 && 
            service_name.chars().all(fn(c) { 
              (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'
            })
          }
          None => false
        }
      },
      error_message: "Service name must be lowercase with hyphens only"
    },
    {
      rule_id: "status_code_check",
      name: "Status Code Validation",
      description: "HTTP status codes should be in valid range",
      validator: fn(data: Array[(String, String)>) {
        match get_field(data, "http.status_code") {
          Some((_, status_str)) => {
            match status_str.parse_int() {
              Some(status) => status >= 100 && status < 600
              None => false
            }
          }
          None => true  // Status code is optional
        }
      },
      error_message: "HTTP status code must be between 100 and 599"
    }
  ]
  
  // Create rule engine
  let rule_engine = create_rule_engine(telemetry_rules)
  
  // Test valid telemetry data
  let valid_telemetry = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("parent_span_id", "span-123456789"),
    ("service_name", "user-service"),
    ("start_time", "1640995200"),
    ("end_time", "1640995250"),
    ("http.status_code", "200")
  ]
  
  let valid_result = validate_business_rules(valid_telemetry, rule_engine)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.violations.length(), 0)
  
  // Test invalid span duration
  let invalid_duration = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("service_name", "user-service"),
    ("start_time", "1640995250"),  // After end time
    ("end_time", "1640995200")
  ]
  
  let duration_result = validate_business_rules(invalid_duration, rule_engine)
  assert_false(duration_result.is_valid)
  assert_eq(duration_result.violations.length(), 1)
  assert_eq(duration_result.violations[0].rule_id, "span_duration_check")
  
  // Test invalid service name
  let invalid_service = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("service_name", "User-Service"),  // Contains uppercase
    ("start_time", "1640995200")
  ]
  
  let service_result = validate_business_rules(invalid_service, rule_engine)
  assert_false(service_result.is_valid)
  assert_eq(service_result.violations.length(), 1)
  assert_eq(service_result.violations[0].rule_id, "service_name_check")
  
  // Test invalid HTTP status code
  let invalid_status = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("service_name", "user-service"),
    ("start_time", "1640995200"),
    ("http.status_code", "999")  // Invalid status code
  ]
  
  let status_result = validate_business_rules(invalid_status, rule_engine)
  assert_false(status_result.is_valid)
  assert_eq(status_result.violations.length(), 1)
  assert_eq(status_result.violations[0].rule_id, "status_code_check")
  
  // Test multiple violations
  let multiple_violations = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("service_name", "User_Service"),  // Invalid: contains underscore
    ("start_time", "1640995250"),     // After end time
    ("end_time", "1640995200"),
    ("http.status_code", "999")        // Invalid status code
  ]
  
  let multiple_result = validate_business_rules(multiple_violations, rule_engine)
  assert_false(multiple_result.is_valid)
  assert_eq(multiple_result.violations.length(), 3)
  
  let violation_ids = multiple_result.violations.map(fn(v) { v.rule_id })
  assert_true(violation_ids.contains("span_duration_check"))
  assert_true(violation_ids.contains("service_name_check"))
  assert_true(violation_ids.contains("status_code_check"))
  
  // Test conditional validation
  let conditional_rules = [
    {
      rule_id: "error_span_check",
      name: "Error Span Validation",
      description: "Error spans should have error information",
      validator: fn(data: Array[(String, String)>) {
        match get_field(data, "status") {
          Some((_, status)) => {
            if status == "error" {
              // Error spans should have error.message or error.type
              let has_error_message = get_field(data, "error.message").is_some()
              let has_error_type = get_field(data, "error.type").is_some()
              has_error_message || has_error_type
            } else {
              true  // Non-error spans don't need error info
            }
          }
          None => true
        }
      },
      error_message: "Error spans must include error.message or error.type"
    }
  ]
  
  let conditional_engine = create_rule_engine(conditional_rules)
  
  // Test valid error span
  let valid_error_span = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("status", "error"),
    ("error.message", "Database connection failed")
  ]
  
  let valid_error_result = validate_business_rules(valid_error_span, conditional_engine)
  assert_true(valid_error_result.is_valid)
  
  // Test invalid error span (missing error info)
  let invalid_error_span = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("status", "error")
    // Missing error information
  ]
  
  let invalid_error_result = validate_business_rules(invalid_error_span, conditional_engine)
  assert_false(invalid_error_result.is_valid)
  assert_eq(invalid_error_result.violations.length(), 1)
  assert_eq(invalid_error_result.violations[0].rule_id, "error_span_check")
}

// Test 4: Data Consistency Validation
test "data consistency validation across related records" {
  // Define consistency check
  type ConsistencyCheck = {
    check_id: String,
    name: String,
    description: String,
    validator: (Array[Array[(String, String)>>) -> Bool,
    error_message: String
  }
  
  // Define consistency validator
  type ConsistencyValidator = {
    checks: Array[ConsistencyCheck>
  }
  
  // Create consistency validator
  let create_consistency_validator = fn(checks: Array[ConsistencyCheck>) {
    { checks }
  }
  
  // Validate consistency across multiple records
  let validate_consistency = fn(records: Array[Array[(String, String)>>, validator: ConsistencyValidator) {
    let mut violations = []
    
    for check in validator.checks {
      if not(check.validator(records)) {
        violations = violations.push({
          check_id: check.check_id,
          name: check.name,
          error_message: check.error_message
        })
      }
    }
    
    {
      is_consistent: violations.length() == 0,
      violations
    }
  }
  
  // Define consistency checks for trace data
  let consistency_checks = [
    {
      check_id: "trace_id_consistency",
      name: "Trace ID Consistency",
      description: "All spans in a trace should have the same trace ID",
      validator: fn(records: Array[Array[(String, String)>>) {
        if records.length() <= 1 {
          true
        } else {
          let first_trace_id = records[0].find(fn(f) { f.0 == "trace_id" })
          match first_trace_id {
            Some((_, trace_id)) => {
              records.all(fn(record) {
                match record.find(fn(f) { f.0 == "trace_id" }) {
                  Some((_, id)) => id == trace_id
                  None => false
                }
              })
            }
            None => false
          }
        }
      },
      error_message: "All spans must have the same trace ID"
    },
    {
      check_id: "parent_child_consistency",
      name: "Parent-Child Consistency",
      description: "Child spans should start after parent span",
      validator: fn(records: Array[Array[(String, String)>>) {
        // Create a map of span_id to span data
        let span_map = {}
        for record in records {
          match record.find(fn(f) { f.0 == "span_id" }) {
            Some((_, span_id)) => {
              span_map = span_map.set(span_id, record)
            }
            None => {}
          }
        }
        
        // Check each span's parent relationship
        records.all(fn(record) {
          match record.find(fn(f) { f.0 == "parent_span_id" }) {
            Some((_, parent_id)) => {
              match span_map.get(parent_id) {
                Some(parent_record) => {
                  match (record.find(fn(f) { f.0 == "start_time" }), parent_record.find(fn(f) { f.0 == "start_time" })) {
                    (Some((_, child_start_str)), Some((_, parent_start_str))) => {
                      match (child_start_str.parse_int(), parent_start_str.parse_int()) {
                        (Some(child_start), Some(parent_start)) => child_start >= parent_start
                        _ => false
                      }
                    }
                    _ => false
                  }
                }
                None => false  // Parent span not found
              }
            }
            None => true  // Root span has no parent
          }
        })
      },
      error_message: "Child spans must start after their parent span"
    },
    {
      check_id: "timeline_consistency",
      name: "Timeline Consistency",
      description: "Span timestamps should be within reasonable time bounds",
      validator: fn(records: Array[Array[(String, String)>>) {
        if records.length() == 0 {
          true
        } else {
          // Find earliest and latest timestamps
          let mut timestamps = []
          for record in records {
            match record.find(fn(f) { f.0 == "start_time" }) {
              Some((_, time_str)) => {
                match time_str.parse_int() {
                  Some(time) => timestamps = timestamps.push(time)
                  None => {}
                }
              }
              None => {}
            }
          }
          
          if timestamps.length() == 0 {
            false
          } else {
            let min_time = timestamps.reduce(fn(min, time) { if time < min { time } else { min } }, timestamps[0])
            let max_time = timestamps.reduce(fn(max, time) { if time > max { time } else { max } }, timestamps[0])
            
            // Check if all spans are within 24 hours
            (max_time - min_time) <= 86400
          }
        }
      },
      error_message: "All spans should be within 24 hours of each other"
    }
  ]
  
  // Create consistency validator
  let consistency_validator = create_consistency_validator(consistency_checks)
  
  // Test consistent trace data
  let consistent_trace = [
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-111111111"),
      ("parent_span_id", ""),
      ("service_name", "api-gateway"),
      ("start_time", "1640995200"),
      ("end_time", "1640995250")
    ],
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-222222222"),
      ("parent_span_id", "span-111111111"),
      ("service_name", "auth-service"),
      ("start_time", "1640995210"),
      ("end_time", "1640995230")
    ],
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-333333333"),
      ("parent_span_id", "span-222222222"),
      ("service_name", "user-service"),
      ("start_time", "1640995220"),
      ("end_time", "1640995240")
    ]
  ]
  
  let consistent_result = validate_consistency(consistent_trace, consistency_validator)
  assert_true(consistent_result.is_consistent)
  assert_eq(consistent_result.violations.length(), 0)
  
  // Test inconsistent trace IDs
  let inconsistent_trace_ids = [
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-111111111"),
      ("service_name", "api-gateway"),
      ("start_time", "1640995200")
    ],
    [
      ("trace_id", "trace-0987654321"),  // Different trace ID
      ("span_id", "span-222222222"),
      ("service_name", "auth-service"),
      ("start_time", "1640995210")
    ]
  ]
  
  let trace_id_result = validate_consistency(inconsistent_trace_ids, consistency_validator)
  assert_false(trace_id_result.is_consistent)
  assert_eq(trace_id_result.violations.length(), 1)
  assert_eq(trace_id_result.violations[0].check_id, "trace_id_consistency")
  
  // Test parent-child inconsistency
  let parent_child_inconsistent = [
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-111111111"),
      ("service_name", "api-gateway"),
      ("start_time", "1640995200")
    ],
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-222222222"),
      ("parent_span_id", "span-111111111"),
      ("service_name", "auth-service"),
      ("start_time", "1640995190")  // Before parent
    ]
  ]
  
  let parent_child_result = validate_consistency(parent_child_inconsistent, consistency_validator)
  assert_false(parent_child_result.is_consistent)
  assert_eq(parent_child_result.violations.length(), 1)
  assert_eq(parent_child_result.violations[0].check_id, "parent_child_consistency")
  
  // Test timeline inconsistency
  let timeline_inconsistent = [
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-111111111"),
      ("service_name", "api-gateway"),
      ("start_time", "1640995200")  // Day 1
    ],
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-222222222"),
      ("service_name", "auth-service"),
      ("start_time", "1641081600")  // Day 2 (24 hours later)
    ]
  ]
  
  let timeline_result = validate_consistency(timeline_inconsistent, consistency_validator)
  assert_false(timeline_result.is_consistent)
  assert_eq(timeline_result.violations.length(), 1)
  assert_eq(timeline_result.violations[0].check_id, "timeline_consistency")
  
  // Test multiple consistency violations
  let multiple_violations_trace = [
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-111111111"),
      ("service_name", "api-gateway"),
      ("start_time", "1640995200")
    ],
    [
      ("trace_id", "trace-0987654321"),  // Different trace ID
      ("span_id", "span-222222222"),
      ("parent_span_id", "span-111111111"),
      ("service_name", "auth-service"),
      ("start_time", "1640995190")  // Before parent
    ],
    [
      ("trace_id", "trace-1234567890"),
      ("span_id", "span-333333333"),
      ("service_name", "user-service"),
      ("start_time", "1641081600")  // 24 hours later
    ]
  ]
  
  let multiple_violations_result = validate_consistency(multiple_violations_trace, consistency_validator)
  assert_false(multiple_violations_result.is_consistent)
  assert_eq(multiple_violations_result.violations.length(), 3)
  
  let violation_ids = multiple_violations_result.violations.map(fn(v) { v.check_id })
  assert_true(violation_ids.contains("trace_id_consistency"))
  assert_true(violation_ids.contains("parent_child_consistency"))
  assert_true(violation_ids.contains("timeline_consistency"))
}

// Test 5: Data Quality Metrics
test "data quality metrics and scoring" {
  // Define data quality dimension
  enum QualityDimension {
    Completeness
    Accuracy
    Consistency
    Validity
    Timeliness
  }
  
  // Define quality metric
  type QualityMetric = {
    dimension: QualityDimension,
    score: Float,  // 0.0 to 1.0
    weight: Float, // Importance weight
    details: String
  }
  
  // Define quality assessment
  type QualityAssessment = {
    overall_score: Float,
    metrics: Array[QualityMetric>,
    recommendations: Array[String]
  }
  
  // Calculate completeness score
  let calculate_completeness = fn(data: Array[(String, String)>, required_fields: Array<String>) {
    let present_fields = data.map(fn(field) { field.0 })
    let missing_fields = required_fields.filter(fn(field) { 
      not(present_fields.contains(field)) 
    })
    
    let completeness_score = if required_fields.length() > 0 {
      (required_fields.length() - missing_fields.length()) as Float / required_fields.length() as Float
    } else {
      1.0
    }
    
    {
      dimension: QualityDimension::Completeness,
      score: completeness_score,
      weight: 0.3,
      details: "Missing fields: " + missing_fields.join(", ")
    }
  }
  
  // Calculate accuracy score
  let calculate_accuracy = fn(data: Array[(String, String)>) {
    let mut accuracy_issues = 0
    let mut total_fields = 0
    
    for field in data {
      total_fields = total_fields + 1
      
      // Check for common accuracy issues
      if field.0 == "email" && not(field.1.contains("@")) {
        accuracy_issues = accuracy_issues + 1
      } else if field.0 == "phone" && field.1.length() < 10 {
        accuracy_issues = accuracy_issues + 1
      } else if field.0 == "url" && not(field.1.starts_with("http")) {
        accuracy_issues = accuracy_issues + 1
      }
    }
    
    let accuracy_score = if total_fields > 0 {
      (total_fields - accuracy_issues) as Float / total_fields as Float
    } else {
      1.0
    }
    
    {
      dimension: QualityDimension::Accuracy,
      score: accuracy_score,
      weight: 0.25,
      details: "Accuracy issues found: " + accuracy_issues.to_string()
    }
  }
  
  // Calculate validity score
  let calculate_validity = fn(data: Array[(String, String)>, validators: Array<(String, (String) -> Bool)>) {
    let mut validity_issues = 0
    let mut total_validations = 0
    
    for (field_name, validator) in validators {
      match data.find(fn(f) { f.0 == field_name }) {
        Some((_, value)) => {
          total_validations = total_validations + 1
          if not(validator(value)) {
            validity_issues = validity_issues + 1
          }
        }
        None => {}  // Skip validation for missing fields
      }
    }
    
    let validity_score = if total_validations > 0 {
      (total_validations - validity_issues) as Float / total_validations as Float
    } else {
      1.0
    }
    
    {
      dimension: QualityDimension::Validity,
      score: validity_score,
      weight: 0.25,
      details: "Validity issues: " + validity_issues.to_string() + "/" + total_validations.to_string()
    }
  }
  
  // Calculate timeliness score
  let calculate_timeliness = fn(data: Array[(String, String)>, current_time: Int, max_age_seconds: Int) {
    match data.find(fn(f) { f.0 == "timestamp" }) {
      Some((_, timestamp_str)) => {
        match timestamp_str.parse_int() {
          Some(timestamp) => {
            let age = current_time - timestamp
            if age <= 0 {
              // Future timestamp
              {
                dimension: QualityDimension::Timeliness,
                score: 0.0,
                weight: 0.2,
                details: "Timestamp is in the future"
              }
            } else if age <= max_age_seconds {
              // Recent data
              let timeliness_score = 1.0 - (age as Float / max_age_seconds as Float) * 0.5
              {
                dimension: QualityDimension::Timeliness,
                score: timeliness_score,
                weight: 0.2,
                details: "Data age: " + age.to_string() + " seconds"
              }
            } else {
              // Old data
              {
                dimension: QualityDimension::Timeliness,
                score: 0.1,
                weight: 0.2,
                details: "Data is too old: " + age.to_string() + " seconds"
              }
            }
          }
          None => {
            {
              dimension: QualityDimension::Timeliness,
              score: 0.0,
              weight: 0.2,
              details: "Invalid timestamp format"
            }
          }
        }
      }
      None => {
        {
          dimension: QualityDimension::Timeliness,
          score: 0.5,
          weight: 0.2,
          details: "No timestamp provided"
        }
      }
    }
  }
  
  // Calculate overall quality score
  let calculate_overall_quality = fn(metrics: Array[QualityMetric>) {
    let weighted_sum = metrics.reduce(fn(sum, metric) {
      sum + (metric.score * metric.weight)
    }, 0.0)
    
    let total_weight = metrics.reduce(fn(sum, metric) { sum + metric.weight }, 0.0)
    
    if total_weight > 0.0 {
      weighted_sum / total_weight
    } else {
      0.0
    }
  }
  
  // Generate recommendations based on quality assessment
  let generate_recommendations = fn(metrics: Array[QualityMetric>) {
    let mut recommendations = []
    
    for metric in metrics {
      if metric.score < 0.7 {
        match metric.dimension {
          QualityDimension::Completeness => {
            recommendations = recommendations.push("Improve data completeness by ensuring all required fields are populated")
          }
          QualityDimension::Accuracy => {
            recommendations = recommendations.push("Enhance data accuracy through validation rules and data cleansing")
          }
          QualityDimension::Consistency => {
            recommendations = recommendations.push("Ensure data consistency across related records and systems")
          }
          QualityDimension::Validity => {
            recommendations = recommendations.push("Strengthen validation rules to ensure data format compliance")
          }
          QualityDimension::Timeliness => {
            recommendations = recommendations.push("Optimize data collection and processing to improve timeliness")
          }
        }
      }
    }
    
    recommendations
  }
  
  // Test quality assessment
  let test_data = [
    ("trace_id", "trace-1234567890"),
    ("span_id", "span-098765432"),
    ("service_name", "user-service"),
    ("email", "user@example.com"),
    ("timestamp", "1640995200")
  ]
  
  let required_fields = ["trace_id", "span_id", "service_name", "operation_name", "start_time"]
  
  let validators = [
    ("trace_id", fn(value: String) { value.starts_with("trace-") && value.length() == 16 }),
    ("span_id", fn(value: String) { value.starts_with("span-") && value.length() == 15 }),
    ("email", fn(value: String) { value.contains("@") })
  ]
  
  let current_time = 1640995300
  let max_age = 3600  // 1 hour
  
  // Calculate quality metrics
  let completeness = calculate_completeness(test_data, required_fields)
  let accuracy = calculate_accuracy(test_data)
  let validity = calculate_validity(test_data, validators)
  let timeliness = calculate_timeliness(test_data, current_time, max_age)
  
  let metrics = [completeness, accuracy, validity, timeliness]
  
  // Test individual metrics
  assert_eq(completeness.dimension, QualityDimension::Completeness)
  assert_eq(completeness.score, 0.6)  // 3 out of 5 required fields present
  assert_eq(completeness.weight, 0.3)
  
  assert_eq(accuracy.dimension, QualityDimension::Accuracy)
  assert_eq(accuracy.score, 1.0)  // No accuracy issues found
  assert_eq(accuracy.weight, 0.25)
  
  assert_eq(validity.dimension, QualityDimension::Validity)
  assert_eq(validity.score, 1.0)  // All validations pass
  assert_eq(validity.weight, 0.25)
  
  assert_eq(timeliness.dimension, QualityDimension::Timeliness)
  assert_eq(timeliness.score, 0.9972)  // Very recent data
  assert_eq(timeliness.weight, 0.2)
  
  // Calculate overall quality score
  let overall_score = calculate_overall_quality(metrics)
  assert_true(overall_score > 0.8)  // Should be high quality
  assert_true(overall_score < 1.0)  // But not perfect due to completeness
  
  // Generate recommendations
  let recommendations = generate_recommendations(metrics)
  assert_eq(recommendations.length(), 1)
  assert_true(recommendations[0].contains("completeness"))
  
  // Create quality assessment
  let assessment = {
    overall_score,
    metrics,
    recommendations
  }
  
  assert_eq(assessment.overall_score, overall_score)
  assert_eq(assessment.metrics.length(), 4)
  assert_eq(assessment.recommendations.length(), 1)
  
  // Test low quality data
  let low_quality_data = [
    ("trace_id", "invalid"),  // Invalid format
    ("email", "not-an-email"),  // Invalid email
    ("timestamp", "0")  // Very old timestamp
  ]
  
  let low_completeness = calculate_completeness(low_quality_data, required_fields)
  let low_accuracy = calculate_accuracy(low_quality_data)
  let low_validity = calculate_validity(low_quality_data, validators)
  let low_timeliness = calculate_timeliness(low_quality_data, current_time, max_age)
  
  let low_metrics = [low_completeness, low_accuracy, low_validity, low_timeliness]
  let low_overall = calculate_overall_quality(low_metrics)
  let low_recommendations = generate_recommendations(low_metrics)
  
  assert_true(low_overall < 0.5)  // Should be low quality
  assert_eq(low_recommendations.length(), 4)  // All dimensions need improvement
}