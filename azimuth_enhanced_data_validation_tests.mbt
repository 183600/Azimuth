// Enhanced Data Validation Tests for Azimuth Telemetry System
// This file contains comprehensive data validation and serialization tests

// Test 1: JSON-like Data Structure Validation
test "json-like data structure validation" {
  // Define a JSON-like structure
  type JsonValue {
    Null
    Bool(Bool)
    Int(Int)
    Float(Float)
    String(String)
    Array([JsonValue])
    Object({String: JsonValue})
  }
  
  // Create test data
  let json_obj = JsonValue::Object({
    "name": JsonValue::String("Azimuth"),
    "version": JsonValue::Float(1.0),
    "active": JsonValue::Bool(true),
    "metrics": JsonValue::Array([
      JsonValue::Int(100),
      JsonValue::Int(200),
      JsonValue::Int(300)
    ]),
    "config": JsonValue::Object({
      "debug": JsonValue::Bool(false),
      "timeout": JsonValue::Int(30)
    })
  })
  
  // Validation function
  let validate_json_object = fn(json : JsonValue, required_fields : [String]) {
    match json {
      JsonValue::Object(obj) => {
        for field in required_fields {
          if not obj.contains(field) {
            return false
          }
        }
        true
      }
      _ => false
    }
  }
  
  // Test validation
  let required_fields = ["name", "version", "active"]
  assert_true(validate_json_object(json_obj, required_fields))
  
  let missing_fields = ["name", "version", "missing"]
  assert_false(validate_json_object(json_obj, missing_fields))
  
  // Test nested validation
  let validate_nested = fn(json : JsonValue) {
    match json {
      JsonValue::Object(obj) => {
        match obj.get("metrics") {
          Some(JsonValue::Array(arr)) => arr.length() > 0,
          _ => false
        }
      }
      _ => false
    }
  }
  
  assert_true(validate_nested(json_obj))
}

// Test 2: Data Serialization and Deserialization
test "data serialization and deserialization" {
  // Simple serialization format
  let serialize_int = fn(value : Int) {
    "int:" + value.to_string()
  }
  
  let serialize_string = fn(value : String) {
    "str:" + value.length().to_string() + ":" + value
  }
  
  let serialize_bool = fn(value : Bool) {
    if value { "bool:true" } else { "bool:false" }
  }
  
  // Deserialization functions
  let deserialize_int = fn(data : String) {
    if data.starts_with("int:") {
      let num_str = data.substring(4, data.length())
      match num_str.to_int() {
        Some(value) => Some(value),
        None => None
      }
    } else {
      None
    }
  }
  
  let deserialize_string = fn(data : String) {
    if data.starts_with("str:") {
      let colon_pos = data.index_of(":", 4).or_else(0)
      let length_str = data.substring(4, colon_pos)
      match length_str.to_int() {
        Some(length) => {
          let start_pos = colon_pos + 1
          let end_pos = start_pos + length
          if end_pos <= data.length() {
            Some(data.substring(start_pos, end_pos))
          } else {
            None
          }
        }
        None => None
      }
    } else {
      None
    }
  }
  
  let deserialize_bool = fn(data : String) {
    if data == "bool:true" {
      Some(true)
    } else if data == "bool:false" {
      Some(false)
    } else {
      None
    }
  }
  
  // Test serialization
  let int_serialized = serialize_int(42)
  let string_serialized = serialize_string("hello")
  let bool_serialized = serialize_bool(true)
  
  assert_eq(int_serialized, "int:42")
  assert_eq(string_serialized, "str:5:hello")
  assert_eq(bool_serialized, "bool:true")
  
  // Test deserialization
  let int_deserialized = deserialize_int(int_serialized)
  let string_deserialized = deserialize_string(string_serialized)
  let bool_deserialized = deserialize_bool(bool_serialized)
  
  match int_deserialized {
    Some(value) => assert_eq(value, 42),
    None => assert_true(false)
  }
  
  match string_deserialized {
    Some(value) => assert_eq(value, "hello"),
    None => assert_true(false)
  }
  
  match bool_deserialized {
    Some(value) => assert_true(value),
    None => assert_true(false)
  }
}

// Test 3: Data Format Validation
test "data format validation" {
  // Email validation
  let validate_email = fn(email : String) {
    let has_at = email.contains("@")
    let has_dot = email.contains(".")
    let at_before_dot = email.index_of("@").or_else(0) < email.index_of(".").or_else(0)
    let valid_length = email.length() > 5 && email.length() < 100
    
    has_at && has_dot && at_before_dot && valid_length
  }
  
  // URL validation
  let validate_url = fn(url : String) {
    let has_protocol = url.starts_with("http://") || url.starts_with("https://")
    let has_domain = url.contains(".") && url.length() > 10
    
    has_protocol && has_domain
  }
  
  // Phone number validation (simple pattern)
  let validate_phone = fn(phone : String) {
    let digits_only = phone.filter(fn(c) { c >= '0' && c <= '9' })
    let valid_length = digits_only.length() >= 10 && digits_only.length() <= 15
    
    valid_length
  }
  
  // Test email validation
  assert_true(validate_email("user@example.com"))
  assert_true(validate_email("test.email@domain.co.uk"))
  assert_false(validate_email("invalid-email"))
  assert_false(validate_email("@example.com"))
  assert_false(validate_email("user@"))
  assert_false(validate_email("user.example.com"))
  
  // Test URL validation
  assert_true(validate_url("https://www.example.com"))
  assert_true(validate_url("http://api.service.org/v1"))
  assert_false(validate_url("www.example.com"))
  assert_false(validate_url("example.com"))
  assert_false(validate_url("ftp://example.com"))
  
  // Test phone validation
  assert_true(validate_phone("123-456-7890"))
  assert_true(validate_phone("+1 (123) 456-7890"))
  assert_true(validate_phone("1234567890"))
  assert_false(validate_phone("123"))
  assert_false(validate_phone("12345678901234567890"))
}

// Test 4: Range and Constraint Validation
test "range and constraint validation" {
  // Numeric range validation
  let validate_range = fn(value : Int, min : Int, max : Int) {
    value >= min && value <= max
  }
  
  // Float precision validation
  let validate_float_precision = fn(value : Float, decimals : Int) {
    let multiplier = 10.0.pow(decimals.to_float())
    let rounded = (value * multiplier).round() / multiplier
    (value - rounded).abs() < 0.0000001
  }
  
  // String length validation
  let validate_string_length = fn(value : String, min : Int, max : Int) {
    let length = value.length()
    length >= min && length <= max
  }
  
  // Array size validation
  let validate_array_size = fn[T](arr : [T], min : Int, max : Int) {
    let size = arr.length()
    size >= min && size <= max
  }
  
  // Test range validation
  assert_true(validate_range(5, 1, 10))
  assert_true(validate_range(1, 1, 10))
  assert_true(validate_range(10, 1, 10))
  assert_false(validate_range(0, 1, 10))
  assert_false(validate_range(11, 1, 10))
  
  // Test float precision validation
  assert_true(validate_float_precision(3.14159, 4))
  assert_true(validate_float_precision(1.2345, 3))
  assert_false(validate_float_precision(1.234567, 3))
  
  // Test string length validation
  assert_true(validate_string_length("hello", 3, 10))
  assert_true(validate_string_length("abc", 3, 10))
  assert_true(validate_string_length("abcdefghij", 3, 10))
  assert_false(validate_string_length("ab", 3, 10))
  assert_false(validate_string_length("abcdefghijk", 3, 10))
  
  // Test array size validation
  assert_true(validate_array_size([1, 2, 3], 1, 5))
  assert_true(validate_array_size([1], 1, 5))
  assert_true(validate_array_size([1, 2, 3, 4, 5], 1, 5))
  assert_false(validate_array_size([], 1, 5))
  assert_false(validate_array_size([1, 2, 3, 4, 5, 6], 1, 5))
}

// Test 5: Data Integrity Validation
test "data integrity validation" {
  // Checksum calculation (simple)
  let calculate_checksum = fn(data : String) {
    let mut sum = 0
    for c in data.to_chars() {
      sum = sum + c.to_int()
    }
    sum % 256
  }
  
  // Verify data integrity
  let verify_integrity = fn(data : String, expected_checksum : Int) {
    let actual_checksum = calculate_checksum(data)
    actual_checksum == expected_checksum
  }
  
  // Hash-like function (simple)
  let simple_hash = fn(data : String) {
    let mut hash = 5381
    for c in data.to_chars() {
      hash = ((hash << 5) + hash) + c.to_int()
    }
    hash.abs()
  }
  
  // Test checksum calculation
  let test_data = "Azimuth telemetry data"
  let checksum = calculate_checksum(test_data)
  assert_eq(checksum, 219)  // Pre-calculated value
  
  // Test integrity verification
  assert_true(verify_integrity(test_data, checksum))
  assert_false(verify_integrity(test_data, checksum + 1))
  
  // Test hash function consistency
  let hash1 = simple_hash(test_data)
  let hash2 = simple_hash(test_data)
  assert_eq(hash1, hash2)
  
  let different_data = "Different telemetry data"
  let hash3 = simple_hash(different_data)
  assert_not_eq(hash1, hash3)
}

// Test 6: Schema Validation
test "schema validation" {
  // Define schema types
  type FieldType {
    StringField
    IntField
    BoolField
    FloatField
    ArrayField(FieldType)
    ObjectField({String: FieldType})
  }
  
  type SchemaRule {
    Required(String, FieldType)
    Optional(String, FieldType)
  }
  
  // Create a schema
  let telemetry_schema = [
    SchemaRule::Required("id", FieldType::StringField),
    SchemaRule::Required("timestamp", FieldType::IntField),
    SchemaRule::Required("value", FieldType::FloatField),
    SchemaRule::Optional("tags", FieldType::ArrayField(FieldType::StringField)),
    SchemaRule::Optional("metadata", FieldType::ObjectField({
      "source": FieldType::StringField,
      "version": FieldType::StringField
    }))
  ]
  
  // Validation function
  let validate_schema = fn(data : {String: JsonValue}, schema : [SchemaRule]) {
    for rule in schema {
      match rule {
        SchemaRule::Required(field_name, field_type) => {
          if not data.contains(field_name) {
            return false
          }
          
          let value = data.get(field_name).or_else(JsonValue::Null)
          if not validate_field_type(value, field_type) {
            return false
          }
        }
        SchemaRule::Optional(field_name, field_type) => {
          if data.contains(field_name) {
            let value = data.get(field_name).or_else(JsonValue::Null)
            if not validate_field_type(value, field_type) {
              return false
            }
          }
        }
      }
    }
    true
  }
  
  // Field type validation
  let validate_field_type = fn(value : JsonValue, field_type : FieldType) {
    match (value, field_type) {
      (JsonValue::String(_), FieldType::StringField) => true,
      (JsonValue::Int(_), FieldType::IntField) => true,
      (JsonValue::Bool(_), FieldType::BoolField) => true,
      (JsonValue::Float(_), FieldType::FloatField) => true,
      (JsonValue::Array(arr), FieldType::ArrayField(inner_type)) => {
        for item in arr {
          if not validate_field_type(item, inner_type) {
            return false
          }
        }
        true
      }
      (JsonValue::Object(obj), FieldType::ObjectField(fields)) => {
        for (field_name, field_type) in fields.entries() {
          match obj.get(field_name) {
            Some(value) => {
              if not validate_field_type(value, field_type) {
                return false
              }
            }
            None => return false
          }
        }
        true
      }
      _ => false
    }
  }
  
  // Test valid data
  let valid_data = {
    "id": JsonValue::String("telemetry_001"),
    "timestamp": JsonValue::Int(1234567890),
    "value": JsonValue::Float(42.5),
    "tags": JsonValue::Array([
      JsonValue::String("production"),
      JsonValue::String("server")
    ]),
    "metadata": JsonValue::Object({
      "source": JsonValue::String("server-01"),
      "version": JsonValue::String("1.0.0")
    })
  }
  
  assert_true(validate_schema(valid_data, telemetry_schema))
  
  // Test missing required field
  let invalid_data_missing = {
    "timestamp": JsonValue::Int(1234567890),
    "value": JsonValue::Float(42.5)
  }
  
  assert_false(validate_schema(invalid_data_missing, telemetry_schema))
  
  // Test wrong field type
  let invalid_data_type = {
    "id": JsonValue::Int(123),  // Should be string
    "timestamp": JsonValue::Int(1234567890),
    "value": JsonValue::Float(42.5)
  }
  
  assert_false(validate_schema(invalid_data_type, telemetry_schema))
}

// Test 7: Data Transformation Validation
test "data transformation validation" {
  // Safe type conversion
  let safe_int_to_string = fn(value : Int) {
    Some(value.to_string())
  }
  
  let safe_string_to_int = fn(value : String) {
    match value.to_int() {
      Some(int_value) => Some(int_value),
      None => None
    }
  }
  
  let safe_string_to_float = fn(value : String) {
    match value.to_float() {
      Some(float_value) => Some(float_value),
      None => None
    }
  }
  
  // Data normalization
  let normalize_string = fn(value : String) {
    value.trim().to_lower()
  }
  
  let normalize_phone = fn(phone : String) {
    let digits = phone.filter(fn(c) { c >= '0' && c <= '9' })
    if digits.length() == 10 {
      Some("(" + digits.substring(0, 3) + ") " + digits.substring(3, 6) + "-" + digits.substring(6, 10))
    } else if digits.length() == 11 && digits[0] == '1' {
      Some("+1 (" + digits.substring(1, 4) + ") " + digits.substring(4, 7) + "-" + digits.substring(7, 11))
    } else {
      None
    }
  }
  
  // Test safe conversions
  let int_to_string_result = safe_int_to_string(42)
  match int_to_string_result {
    Some(value) => assert_eq(value, "42"),
    None => assert_true(false)
  }
  
  let string_to_int_valid = safe_string_to_int("123")
  match string_to_int_valid {
    Some(value) => assert_eq(value, 123),
    None => assert_true(false)
  }
  
  let string_to_int_invalid = safe_string_to_int("abc")
  match string_to_int_invalid {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  let string_to_float_valid = safe_string_to_float("3.14")
  match string_to_float_valid {
    Some(value) => assert_eq(value, 3.14),
    None => assert_true(false)
  }
  
  // Test normalization
  assert_eq(normalize_string("  HeLLo WoRLd  "), "hello world")
  assert_eq(normalize_string("\tTEST\n"), "test")
  
  let normalized_phone1 = normalize_phone("123-456-7890")
  match normalized_phone1 {
    Some(value) => assert_eq(value, "(123) 456-7890"),
    None => assert_true(false)
  }
  
  let normalized_phone2 = normalize_phone("+1 (123) 456-7890")
  match normalized_phone2 {
    Some(value) => assert_eq(value, "+1 (123) 456-7890"),
    None => assert_true(false)
  }
  
  let normalized_phone_invalid = normalize_phone("123")
  match normalized_phone_invalid {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
}

// Test 8: Batch Data Validation
test "batch data validation" {
  // Batch validation result
  type ValidationResult {
    Valid
    Invalid(String)
  }
  
  // Validate single item
  let validate_item = fn(item : String) {
    if item.length() > 0 && item.length() <= 100 {
      ValidationResult::Valid
    } else {
      ValidationResult::Invalid("Invalid length")
    }
  }
  
  // Batch validation
  let validate_batch = fn(items : [String]) {
    let results = []
    let mut valid_count = 0
    let mut invalid_count = 0
    
    for item in items {
      let result = validate_item(item)
      match result {
        ValidationResult::Valid => valid_count = valid_count + 1,
        ValidationResult::Invalid(_) => invalid_count = invalid_count + 1
      }
      results = results.push(result)
    }
    
    {
      "results": results,
      "valid_count": valid_count,
      "invalid_count": invalid_count,
      "total_count": items.length(),
      "is_all_valid": invalid_count == 0
    }
  }
  
  // Test batch validation
  let test_items = [
    "valid_item_1",
    "valid_item_2",
    "",
    "valid_item_3",
    "this_is_a_very_long_item_that_exceeds_the_maximum_allowed_length_and_should_be_invalid"
  ]
  
  let batch_result = validate_batch(test_items)
  
  assert_eq(batch_result["total_count"], 5)
  assert_eq(batch_result["valid_count"], 3)
  assert_eq(batch_result["invalid_count"], 2)
  assert_false(batch_result["is_all_valid"])
  
  // Test all valid batch
  let valid_items = [
    "item1",
    "item2",
    "item3"
  ]
  
  let valid_batch_result = validate_batch(valid_items)
  
  assert_eq(valid_batch_result["total_count"], 3)
  assert_eq(valid_batch_result["valid_count"], 3)
  assert_eq(valid_batch_result["invalid_count"], 0)
  assert_true(valid_batch_result["is_all_valid"])
}

// Test 9: Conditional Validation
test "conditional validation" {
  // Conditional validation rules
  let validate_conditional = fn(data : {String: JsonValue}) {
    let has_field_a = data.contains("field_a")
    let has_field_b = data.contains("field_b")
    
    // Rule: If field_a exists, field_b must also exist
    if has_field_a && not has_field_b {
      return false
    }
    
    // Rule: field_c must be greater than field_d if both exist
    match (data.get("field_c"), data.get("field_d")) {
      (Some(JsonValue::Int(c)), Some(JsonValue::Int(d))) => {
        if c <= d {
          return false
        }
      }
      _ => {}
    }
    
    // Rule: field_e must be "active" if status is "enabled"
    match (data.get("status"), data.get("field_e")) {
      (Some(JsonValue::String(status)), Some(JsonValue::String(field_e))) => {
        if status == "enabled" && field_e != "active" {
          return false
        }
      }
      (Some(JsonValue::String(status)), None) => {
        if status == "enabled" {
          return false
        }
      }
      _ => {}
    }
    
    true
  }
  
  // Test conditional validation
  let valid_data1 = {
    "field_a": JsonValue::String("value_a"),
    "field_b": JsonValue::String("value_b")
  }
  
  assert_true(validate_conditional(valid_data1))
  
  let invalid_data1 = {
    "field_a": JsonValue::String("value_a")
    // Missing field_b
  }
  
  assert_false(validate_conditional(invalid_data1))
  
  let valid_data2 = {
    "field_c": JsonValue::Int(10),
    "field_d": JsonValue::Int(5)
  }
  
  assert_true(validate_conditional(valid_data2))
  
  let invalid_data2 = {
    "field_c": JsonValue::Int(5),
    "field_d": JsonValue::Int(10)
  }
  
  assert_false(validate_conditional(invalid_data2))
  
  let valid_data3 = {
    "status": JsonValue::String("enabled"),
    "field_e": JsonValue::String("active")
  }
  
  assert_true(validate_conditional(valid_data3))
  
  let invalid_data3 = {
    "status": JsonValue::String("enabled"),
    "field_e": JsonValue::String("inactive")
  }
  
  assert_false(validate_conditional(invalid_data3))
}

// Test 10: Performance Validation
test "performance validation" {
  // Measure validation performance
  let measure_validation_time = fn(data : [String], validator : (String) -> Bool) {
    let start_time = get_current_time_millis()
    
    let mut valid_count = 0
    for item in data {
      if validator(item) {
        valid_count = valid_count + 1
      }
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "duration": duration,
      "valid_count": valid_count,
      "total_count": data.length(),
      "items_per_second": if duration > 0 { (data.length().to_float() / duration.to_float()) * 1000.0 } else { 0.0 }
    }
  }
  
  // Test validators
  let email_validator = fn(email : String) {
    let has_at = email.contains("@")
    let has_dot = email.contains(".")
    let at_before_dot = email.index_of("@").or_else(0) < email.index_of(".").or_else(0)
    has_at && has_dot && at_before_dot
  }
  
  let simple_validator = fn(value : String) {
    value.length() > 0
  }
  
  let complex_validator = fn(value : String) {
    let has_upper = value.to_lower() != value
    let has_lower = value.to_upper() != value
    let has_digit = value.filter(fn(c) { c >= '0' && c <= '9' }).length() > 0
    let has_special = value.filter(fn(c) { not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) }).length() > 0
    
    value.length() >= 8 && has_upper && has_lower && has_digit && has_special
  }
  
  // Generate test data
  let test_emails = [
    "user@example.com",
    "test.email@domain.co.uk",
    "invalid-email",
    "@example.com",
    "user@",
    "user.example.com",
    "another@test.org",
    "yet.another@domain.net"
  ]
  
  let test_passwords = [
    "simple",
    "complex123",
    "VeryComplex123!",
    "NoDigits!",
    "nouppercase123!",
    "NOLOWERCASE123!",
    "ValidPassword123!"
  ]
  
  // Test performance
  let email_performance = measure_validation_time(test_emails, email_validator)
  let simple_performance = measure_validation_time(test_passwords, simple_validator)
  let complex_performance = measure_validation_time(test_passwords, complex_validator)
  
  // Validate performance expectations
  assert_true(email_performance["duration"] < 100)  // Should complete in less than 100ms
  assert_true(simple_performance["duration"] < 50)  // Simple validation should be faster
  assert_true(complex_performance["duration"] < 200)  // Complex validation should still be reasonable
  
  // Check validation results
  assert_eq(email_performance["valid_count"], 5)  // 5 out of 8 emails are valid
  assert_eq(simple_performance["valid_count"], 7)  // 7 out of 7 passwords are non-empty
  assert_eq(complex_performance["valid_count"], 2)  // 2 out of 7 passwords meet complexity requirements
  
  // Check items per second
  assert_true(email_performance["items_per_second"] > 10.0)  // Should process at least 10 items/second
  assert_true(simple_performance["items_per_second"] > 50.0)  // Simple validation should be faster
}

// Helper function to get current time in milliseconds
fn get_current_time_millis() -> Int {
  // Implementation would depend on the available time functions in MoonBit
  // This is a placeholder for the actual implementation
  0
}