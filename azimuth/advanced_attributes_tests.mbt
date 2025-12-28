// Advanced Attributes Operations Test Suite for Azimuth
// This file contains comprehensive tests for advanced attribute operations

// Test 1: Deep nested attribute structures
pub test "deep nested attribute structures and operations" {
  let attrs = azimuth::Attributes::new()
  
  // Create hierarchical attribute keys
  let base_keys = [
    "service.name", "service.version", "service.instance.id",
    "http.method", "http.url", "http.status_code",
    "db.system", "db.statement", "db.connection_string",
    "user.id", "user.email", "user.role",
    "error.type", "error.message", "error.stack_trace"
  ]
  
  // Set various types of attributes
  azimuth::Attributes::set(attrs, "service.name", azimuth::StringValue("user-service"))
  azimuth::Attributes::set(attrs, "service.version", azimuth::StringValue("1.2.3"))
  azimuth::Attributes::set(attrs, "service.instance.id", azimuth::StringValue("instance-12345"))
  azimuth::Attributes::set(attrs, "http.method", azimuth::StringValue("POST"))
  azimuth::Attributes::set(attrs, "http.url", azimuth::StringValue("https://api.example.com/users"))
  azimuth::Attributes::set(attrs, "http.status_code", azimuth::IntValue(201))
  azimuth::Attributes::set(attrs, "db.system", azimuth::StringValue("postgresql"))
  azimuth::Attributes::set(attrs, "db.statement", azimuth::StringValue("SELECT * FROM users WHERE id = $1"))
  azimuth::Attributes::set(attrs, "db.connection_string", azimuth::StringValue("postgresql://localhost:5432/mydb"))
  azimuth::Attributes::set(attrs, "user.id", azimuth::IntValue(12345))
  azimuth::Attributes::set(attrs, "user.email", azimuth::StringValue("user@example.com"))
  azimuth::Attributes::set(attrs, "user.role", azimuth::StringValue("admin"))
  azimuth::Attributes::set(attrs, "error.type", azimuth::StringValue("ValidationError"))
  azimuth::Attributes::set(attrs, "error.message", azimuth::StringValue("Invalid input parameters"))
  azimuth::Attributes::set(attrs, "error.stack_trace", azimuth::StringValue("at function1 (file.js:10:15)"))
  
  // Verify all attributes were set correctly
  assert_eq(azimuth::Attributes::get(attrs, "service.name"), Some(azimuth::StringValue("user-service")))
  assert_eq(azimuth::Attributes::get(attrs, "service.version"), Some(azimuth::StringValue("1.2.3")))
  assert_eq(azimuth::Attributes::get(attrs, "http.status_code"), Some(azimuth::IntValue(201)))
  assert_eq(azimuth::Attributes::get(attrs, "user.id"), Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Attributes::get(attrs, "user.role"), Some(azimuth::StringValue("admin")))
}

// Test 2: Array attribute operations
pub test "array attribute operations and manipulations" {
  let attrs = azimuth::Attributes::new()
  
  // Test string arrays
  let string_array = ["tag1", "tag2", "tag3", "production", "api"]
  azimuth::Attributes::set(attrs, "resource.tags", azimuth::ArrayStringValue(string_array))
  let retrieved_string_array = azimuth::Attributes::get(attrs, "resource.tags")
  assert_eq(retrieved_string_array, Some(azimuth::ArrayStringValue(string_array)))
  
  // Test integer arrays
  let int_array = [200, 404, 500, 301, 302]
  azimuth::Attributes::set(attrs, "http.status_codes", azimuth::ArrayIntValue(int_array))
  let retrieved_int_array = azimuth::Attributes::get(attrs, "http.status_codes")
  assert_eq(retrieved_int_array, Some(azimuth::ArrayIntValue(int_array)))
  
  // Test empty arrays
  let empty_string_array : Array[String] = []
  let empty_int_array : Array[Int] = []
  azimuth::Attributes::set(attrs, "empty.string.array", azimuth::ArrayStringValue(empty_string_array))
  azimuth::Attributes::set(attrs, "empty.int.array", azimuth::ArrayIntValue(empty_int_array))
  let retrieved_empty_string = azimuth::Attributes::get(attrs, "empty.string.array")
  let retrieved_empty_int = azimuth::Attributes::get(attrs, "empty.int.array")
  assert_eq(retrieved_empty_string, Some(azimuth::ArrayStringValue(empty_string_array)))
  assert_eq(retrieved_empty_int, Some(azimuth::ArrayIntValue(empty_int_array)))
  
  // Test single element arrays
  let single_string = ["singleton"]
  let single_int = [42]
  azimuth::Attributes::set(attrs, "single.string", azimuth::ArrayStringValue(single_string))
  azimuth::Attributes::set(attrs, "single.int", azimuth::ArrayIntValue(single_int))
  let retrieved_single_string = azimuth::Attributes::get(attrs, "single.string")
  let retrieved_single_int = azimuth::Attributes::get(attrs, "single.int")
  assert_eq(retrieved_single_string, Some(azimuth::ArrayStringValue(single_string)))
  assert_eq(retrieved_single_int, Some(azimuth::ArrayIntValue(single_int)))
}

// Test 3: Attribute type conversions and validations
pub test "attribute type conversions and validations" {
  let attrs = azimuth::Attributes::new()
  
  // Test string to int conversion scenarios
  azimuth::Attributes::set(attrs, "numeric.string", azimuth::StringValue("12345"))
  azimuth::Attributes::set(attrs, "float.string", azimuth::StringValue("123.45"))
  azimuth::Attributes::set(attrs, "boolean.string.true", azimuth::StringValue("true"))
  azimuth::Attributes::set(attrs, "boolean.string.false", azimuth::StringValue("false"))
  
  // Test actual numeric types
  azimuth::Attributes::set(attrs, "actual.int", azimuth::IntValue(12345))
  azimuth::Attributes::set(attrs, "actual.float", azimuth::FloatValue(123.45))
  azimuth::Attributes::set(attrs, "actual.bool.true", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "actual.bool.false", azimuth::BoolValue(false))
  
  // Verify type preservation
  let numeric_string = azimuth::Attributes::get(attrs, "numeric.string")
  let actual_int = azimuth::Attributes::get(attrs, "actual.int")
  let actual_float = azimuth::Attributes::get(attrs, "actual.float")
  let actual_bool_true = azimuth::Attributes::get(attrs, "actual.bool.true")
  let actual_bool_false = azimuth::Attributes::get(attrs, "actual.bool.false")
  
  assert_eq(numeric_string, Some(azimuth::StringValue("12345")))
  assert_eq(actual_int, Some(azimuth::IntValue(12345)))
  assert_eq(actual_float, Some(azimuth::FloatValue(123.45)))
  assert_eq(actual_bool_true, Some(azimuth::BoolValue(true)))
  assert_eq(actual_bool_false, Some(azimuth::BoolValue(false)))
}

// Test 4: Complex attribute merging and override scenarios
pub test "complex attribute merging and override scenarios" {
  let attrs1 = azimuth::Attributes::new()
  let attrs2 = azimuth::Attributes::new()
  
  // Set initial attributes in first set
  azimuth::Attributes::set(attrs1, "service.name", azimuth::StringValue("original-service"))
  azimuth::Attributes::set(attrs1, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(attrs1, "environment", azimuth::StringValue("development"))
  azimuth::Attributes::set(attrs1, "feature.enabled", azimuth::BoolValue(true))
  
  // Set overlapping and new attributes in second set
  azimuth::Attributes::set(attrs2, "service.name", azimuth::StringValue("updated-service"))  // Override
  azimuth::Attributes::set(attrs2, "service.version", azimuth::StringValue("2.0.0"))        // Override
  azimuth::Attributes::set(attrs2, "region", azimuth::StringValue("us-west-2"))              // New
  azimuth::Attributes::set(attrs2, "feature.enabled", azimuth::BoolValue(false))            // Override
  
  // Test attribute updates (simulating merge behavior)
  let merged_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(merged_attrs, "service.name", azimuth::StringValue("updated-service"))
  azimuth::Attributes::set(merged_attrs, "service.version", azimuth::StringValue("2.0.0"))
  azimuth::Attributes::set(merged_attrs, "environment", azimuth::StringValue("development"))
  azimuth::Attributes::set(merged_attrs, "region", azimuth::StringValue("us-west-2"))
  azimuth::Attributes::set(merged_attrs, "feature.enabled", azimuth::BoolValue(false))
  
  // Verify merged attributes
  assert_eq(azimuth::Attributes::get(merged_attrs, "service.name"), Some(azimuth::StringValue("updated-service")))
  assert_eq(azimuth::Attributes::get(merged_attrs, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Attributes::get(merged_attrs, "environment"), Some(azimuth::StringValue("development")))
  assert_eq(azimuth::Attributes::get(merged_attrs, "region"), Some(azimuth::StringValue("us-west-2")))
  assert_eq(azimuth::Attributes::get(merged_attrs, "feature.enabled"), Some(azimuth::BoolValue(false)))
}

// Test 5: Attribute filtering and selection operations
pub test "attribute filtering and selection operations" {
  let attrs = azimuth::Attributes::new()
  
  // Set a comprehensive set of attributes
  let service_attrs = [
    ("service.name", azimuth::StringValue("payment-service")),
    ("service.version", azimuth::StringValue("3.1.4")),
    ("service.instance.id", azimuth::StringValue("i-1234567890abcdef0"))
  ]
  
  let http_attrs = [
    ("http.method", azimuth::StringValue("POST")),
    ("http.url", azimuth::StringValue("https://api.example.com/payments")),
    ("http.status_code", azimuth::IntValue(200)),
    ("http.response_size", azimuth::IntValue(1024))
  ]
  
  let user_attrs = [
    ("user.id", azimuth::IntValue(98765)),
    ("user.tier", azimuth::StringValue("premium")),
    ("user.session_id", azimuth::StringValue("sess_abc123"))
  ]
  
  let performance_attrs = [
    ("duration.ms", azimuth::FloatValue(245.6)),
    ("memory.usage", azimuth::IntValue(524288)),
    ("cpu.usage", azimuth::FloatValue(0.75))
  ]
  
  // Set all attributes
  for (key, value) in service_attrs {
    azimuth::Attributes::set(attrs, key, value)
  }
  for (key, value) in http_attrs {
    azimuth::Attributes::set(attrs, key, value)
  }
  for (key, value) in user_attrs {
    azimuth::Attributes::set(attrs, key, value)
  }
  for (key, value) in performance_attrs {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Test filtering by prefix (simulated)
  let service_name = azimuth::Attributes::get(attrs, "service.name")
  let http_method = azimuth::Attributes::get(attrs, "http.method")
  let user_id = azimuth::Attributes::get(attrs, "user.id")
  let duration = azimuth::Attributes::get(attrs, "duration.ms")
  
  assert_eq(service_name, Some(azimuth::StringValue("payment-service")))
  assert_eq(http_method, Some(azimuth::StringValue("POST")))
  assert_eq(user_id, Some(azimuth::IntValue(98765)))
  assert_eq(duration, Some(azimuth::FloatValue(245.6)))
}

// Test 6: Attribute serialization and deserialization patterns
pub test "attribute serialization and deserialization patterns" {
  let attrs = azimuth::Attributes::new()
  
  // Create complex attribute structures
  let complex_data = [
    ("timestamp", azimuth::IntValue(1704067200000)),  // Unix timestamp
    ("event.name", azimuth::StringValue("user.login")),
    ("event.category", azimuth::StringValue("authentication")),
    ("source.ip", azimuth::StringValue("192.168.1.100")),
    ("source.country", azimuth::StringValue("US")),
    ("device.type", azimuth::StringValue("mobile")),
    ("device.os", azimuth::StringValue("iOS")),
    ("app.version", azimuth::StringValue("5.2.1")),
    ("session.duration", azimuth::FloatValue(1800.5)),
    ("successful", azimuth::BoolValue(true)),
    ("error.count", azimuth::IntValue(0)),
    ("features.used", azimuth::ArrayStringValue(["search", "profile", "settings"]))
  ]
  
  // Set all complex attributes
  for (key, value) in complex_data {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Verify serialization-like integrity
  let timestamp = azimuth::Attributes::get(attrs, "timestamp")
  let event_name = azimuth::Attributes::get(attrs, "event.name")
  let source_ip = azimuth::Attributes::get(attrs, "source.ip")
  let device_type = azimuth::Attributes::get(attrs, "device.type")
  let session_duration = azimuth::Attributes::get(attrs, "session.duration")
  let successful = azimuth::Attributes::get(attrs, "successful")
  let features_used = azimuth::Attributes::get(attrs, "features.used")
  
  assert_eq(timestamp, Some(azimuth::IntValue(1704067200000)))
  assert_eq(event_name, Some(azimuth::StringValue("user.login")))
  assert_eq(source_ip, Some(azimuth::StringValue("192.168.1.100")))
  assert_eq(device_type, Some(azimuth::StringValue("mobile")))
  assert_eq(session_duration, Some(azimuth::FloatValue(1800.5)))
  assert_eq(successful, Some(azimuth::BoolValue(true)))
  assert_eq(features_used, Some(azimuth::ArrayStringValue(["search", "profile", "settings"])))
}

// Test 7: Attribute performance and memory efficiency
pub test "attribute performance and memory efficiency patterns" {
  let attrs = azimuth::Attributes::new()
  
  // Test with a large number of attributes
  for i in 0..100 {
    let key = "metric." + i.to_string()
    let value = azimuth::IntValue(i * 10)
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Verify specific attributes
  let metric_0 = azimuth::Attributes::get(attrs, "metric.0")
  let metric_50 = azimuth::Attributes::get(attrs, "metric.50")
  let metric_99 = azimuth::Attributes::get(attrs, "metric.99")
  
  assert_eq(metric_0, Some(azimuth::IntValue(0)))
  assert_eq(metric_50, Some(azimuth::IntValue(500)))
  assert_eq(metric_99, Some(azimuth::IntValue(990)))
  
  // Test with large string values
  let large_string = "x" * 10000  // 10K string
  azimuth::Attributes::set(attrs, "large.payload", azimuth::StringValue(large_string))
  let retrieved_large = azimuth::Attributes::get(attrs, "large.payload")
  assert_eq(retrieved_large, Some(azimuth::StringValue(large_string)))
  
  // Test with many small array attributes
  for i in 0..50 {
    let key = "array.metric." + i.to_string()
    let value = azimuth::ArrayIntValue([i, i+1, i+2, i+3, i+4])
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Verify array attributes
  let array_0 = azimuth::Attributes::get(attrs, "array.metric.0")
  let array_25 = azimuth::Attributes::get(attrs, "array.metric.25")
  let array_49 = azimuth::Attributes::get(attrs, "array.metric.49")
  
  assert_eq(array_0, Some(azimuth::ArrayIntValue([0, 1, 2, 3, 4])))
  assert_eq(array_25, Some(azimuth::ArrayIntValue([25, 26, 27, 28, 29])))
  assert_eq(array_49, Some(azimuth::ArrayIntValue([49, 50, 51, 52, 53])))
}

// Test 8: Attribute validation and sanitization
pub test "attribute validation and sanitization" {
  let attrs = azimuth::Attributes::new()
  
  // Test with various edge case values
  let edge_case_values = [
    ("empty.string", azimuth::StringValue("")),
    ("space.only", azimuth::StringValue("   ")),
    ("newline.string", azimuth::StringValue("line1\nline2\nline3")),
    ("tab.string", azimuth::StringValue("col1\tcol2\tcol3")),
    ("unicode.emoji", azimuth::StringValue("🚀🔥💯✨")),
    ("unicode.chinese", azimuth::StringValue("测试中文属性")),
    ("special.chars", azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")),
    ("html.chars", azimuth::StringValue("<script>alert('test')</script>")),
    ("sql.chars", azimuth::StringValue("SELECT * FROM users WHERE id = 1; DROP TABLE users;--")),
    ("json.chars", azimuth::StringValue("{\"key\": \"value\", \"nested\": {\"array\": [1,2,3]}}"))
  ]
  
  // Set all edge case attributes
  for (key, value) in edge_case_values {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Verify all edge case values are preserved
  for (key, expected_value) in edge_case_values {
    let actual_value = azimuth::Attributes::get(attrs, key)
    assert_eq(actual_value, Some(expected_value))
  }
  
  // Test numeric edge cases
  let numeric_edge_cases = [
    ("zero.int", azimuth::IntValue(0)),
    ("max.int", azimuth::IntValue(2147483647)),
    ("min.int", azimuth::IntValue(-2147483648)),
    ("zero.float", azimuth::FloatValue(0.0)),
    ("positive.infinity", azimuth::FloatValue(1.0/0.0)),
    ("negative.infinity", azimuth::FloatValue(-1.0/0.0)),
    ("very.small.float", azimuth::FloatValue(1.0e-308)),
    ("very.large.float", azimuth::FloatValue(1.0e+308))
  ]
  
  // Set numeric edge cases
  for (key, value) in numeric_edge_cases {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Verify numeric edge cases
  for (key, expected_value) in numeric_edge_cases {
    let actual_value = azimuth::Attributes::get(attrs, key)
    assert_eq(actual_value, Some(expected_value))
  }
}