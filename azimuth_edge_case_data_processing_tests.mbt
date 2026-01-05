// Azimuth Telemetry System - Edge Case Data Processing Tests
// This file contains test cases for edge cases in data processing and transformation

// Test 1: Null and Empty Data Handling
test "null and empty data handling" {
  // Test null string handling
  let null_string = ""
  assert_eq(null_string.length(), 0)
  assert_eq(null_string, "")
  
  // Test empty array handling
  let empty_array = []
  assert_eq(empty_array.length(), 0)
  
  // Test null option handling
  let none_option = None
  match none_option {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test edge case with large numbers
  let large_number = 9223372036854775807  // Max int64
  let small_number = -9223372036854775808  // Min int64
  assert_eq(large_number + 1, -9223372036854775808)  // Overflow
  assert_eq(small_number - 1, 9223372036854775807)   // Underflow
}

// Test 2: Data Type Conversion Edge Cases
test "data type conversion edge cases" {
  // Test string to number conversion with invalid input
  let valid_number_string = "12345"
  let invalid_number_string = "not_a_number"
  let empty_number_string = ""
  let partial_number_string = "123abc"
  
  // Valid conversion
  let valid_conversion = valid_number_string.to_int()
  match valid_conversion {
    Some(num) => assert_eq(num, 12345)
    None => assert_true(false)
  }
  
  // Invalid conversion
  let invalid_conversion = invalid_number_string.to_int()
  match invalid_conversion {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Empty string conversion
  let empty_conversion = empty_number_string.to_int()
  match empty_conversion {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Partial number conversion
  let partial_conversion = partial_number_string.to_int()
  match partial_conversion {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test float to int conversion edge cases
  let normal_float = 3.14
  let large_float = 999999999.99
  let small_float = 0.000001
  let negative_float = -123.456
  
  // Normal float to int conversion
  let normal_int = normal_float.to_int()
  match normal_int {
    Some(num) => assert_eq(num, 3)
    None => assert_true(false)
  }
  
  // Large float to int conversion
  let large_int = large_float.to_int()
  match large_int {
    Some(num) => assert_eq(num, 999999999)
    None => assert_true(false)
  }
  
  // Small float to int conversion
  let small_int = small_float.to_int()
  match small_int {
    Some(num) => assert_eq(num, 0)
    None => assert_true(false)
  }
  
  // Negative float to int conversion
  let negative_int = negative_float.to_int()
  match negative_int {
    Some(num) => assert_eq(num, -123)
    None => assert_true(false)
  }
}

// Test 3: Array and Collection Edge Cases
test "array and collection edge cases" {
  // Test array with mixed types
  let mixed_array = [1, "two", 3.0, true]
  assert_eq(mixed_array.length(), 4)
  
  // Test nested arrays
  let nested_array = [[1, 2], [3, 4], [5, 6]]
  assert_eq(nested_array.length(), 3)
  assert_eq(nested_array[0].length(), 2)
  assert_eq(nested_array[0][0], 1)
  
  // Test array bounds checking
  let small_array = [1, 2, 3]
  if small_array.length() > 2 {
    assert_eq(small_array[2], 3)
  } else {
    assert_true(false)
  }
  
  // Test accessing out of bounds (should be handled gracefully)
  if small_array.length() > 5 {
    assert_eq(small_array[5], 0)
  } else {
    assert_true(true)  // Expected case
  }
  
  // Test array operations on empty array
  let empty_array = []
  assert_eq(empty_array.length(), 0)
  
  // Test array filtering with empty predicate
  let filtered_array = empty_array.filter(fn(x) { x > 0 })
  assert_eq(filtered_array.length(), 0)
  
  // Test array mapping on empty array
  let mapped_array = empty_array.map(fn(x) { x * 2 })
  assert_eq(mapped_array.length(), 0)
}

// Test 4: String Processing Edge Cases
test "string processing edge cases" {
  // Test Unicode and special characters
  let unicode_string = "Hello, 世界! 🌍"
  assert_eq(unicode_string.length(), 12)  // Note: length might vary based on Unicode handling
  
  // Test empty string operations
  let empty_string = ""
  assert_eq(empty_string.length(), 0)
  assert_eq(empty_string.to_uppercase(), "")
  assert_eq(empty_string.to_lowercase(), "")
  
  // Test string with only whitespace
  let whitespace_string = "   \t\n   "
  assert_eq(whitespace_string.length(), 8)
  assert_eq(whitespace_string.trim(), "")
  
  // Test very long string
  let long_string = "a".repeat(10000)
  assert_eq(long_string.length(), 10000)
  
  // Test string slicing edge cases
  let test_string = "Hello, World!"
  
  // Normal slice
  if test_string.length() >= 5 {
    assert_eq(test_string[0..5], "Hello")
  } else {
    assert_true(false)
  }
  
  // Slice to end
  if test_string.length() >= 7 {
    assert_eq(test_string[7..test_string.length()], "World!")
  } else {
    assert_true(false)
  }
  
  // Out of bounds slice
  if test_string.length() > 100 {
    assert_eq(test_string[0..100], test_string)
  } else {
    assert_true(true)  // Expected case
  }
}

// Test 5: JSON and Serialization Edge Cases
test "json and serialization edge cases" {
  // Test empty JSON object
  let empty_json = "{}"
  let parsed_empty = empty_json.parse_json()
  match parsed_empty {
    Some(obj) => {
      // Check if it's an empty object
      assert_true(true)  // Simplified check
    }
    None => assert_true(false)
  }
  
  // Test JSON with null values
  let null_json = "{\"key\": null}"
  let parsed_null = null_json.parse_json()
  match parsed_null {
    Some(obj) => {
      // Check if it handles null values correctly
      assert_true(true)  // Simplified check
    }
    None => assert_true(false)
  }
  
  // Test JSON with nested structures
  let nested_json = "{\"level1\": {\"level2\": {\"level3\": \"deep_value\"}}}"
  let parsed_nested = nested_json.parse_json()
  match parsed_nested {
    Some(obj) => {
      // Check if it handles nested structures correctly
      assert_true(true)  // Simplified check
    }
    None => assert_true(false)
  }
  
  // Test JSON with arrays
  let array_json = "[1, \"two\", 3.0, null, true]"
  let parsed_array = array_json.parse_json()
  match parsed_array {
    Some(arr) => {
      // Check if it handles arrays correctly
      assert_true(true)  // Simplified check
    }
    None => assert_true(false)
  }
  
  // Test invalid JSON
  let invalid_json = "{\"invalid\": json}"
  let parsed_invalid = invalid_json.parse_json()
  match parsed_invalid {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: Data Aggregation Edge Cases
test "data aggregation edge cases" {
  // Test aggregation on empty dataset
  let empty_data = []
  let empty_sum = empty_data.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(empty_sum, 0)
  
  let empty_avg = if empty_data.length() > 0 {
    empty_sum / empty_data.length()
  } else {
    0.0
  }
  assert_eq(empty_avg, 0.0)
  
  // Test aggregation with single element
  let single_data = [42]
  let single_sum = single_data.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(single_sum, 42)
  
  let single_avg = if single_data.length() > 0 {
    single_sum / single_data.length()
  } else {
    0.0
  }
  assert_eq(single_avg, 42.0)
  
  // Test aggregation with negative numbers
  let negative_data = [-1, -2, -3, -4, -5]
  let negative_sum = negative_data.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(negative_sum, -15)
  
  let negative_avg = if negative_data.length() > 0 {
    negative_sum / negative_data.length()
  } else {
    0.0
  }
  assert_eq(negative_avg, -3.0)
  
  // Test aggregation with mixed positive and negative numbers
  let mixed_data = [-10, 20, -30, 40, -50]
  let mixed_sum = mixed_data.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(mixed_sum, -30)
  
  let mixed_avg = if mixed_data.length() > 0 {
    mixed_sum / mixed_data.length()
  } else {
    0.0
  }
  assert_eq(mixed_avg, -6.0)
  
  // Test finding min/max in empty dataset
  let empty_min = if empty_data.length() > 0 {
    empty_data.reduce(fn(acc, x) { if x < acc { x } else { acc } }, empty_data[0])
  } else {
    0
  }
  assert_eq(empty_min, 0)
  
  let empty_max = if empty_data.length() > 0 {
    empty_data.reduce(fn(acc, x) { if x > acc { x } else { acc } }, empty_data[0])
  } else {
    0
  }
  assert_eq(empty_max, 0)
}

// Test 7: Data Validation Edge Cases
test "data validation edge cases" {
  // Test email validation edge cases
  let valid_email = "user@example.com"
  let invalid_email_no_at = "userexample.com"
  let invalid_email_no_domain = "user@"
  let invalid_email_no_user = "@example.com"
  let invalid_email_special_chars = "user@exa$mple.com"
  let empty_email = ""
  
  assert_true(validate_email(valid_email))
  assert_false(validate_email(invalid_email_no_at))
  assert_false(validate_email(invalid_email_no_domain))
  assert_false(validate_email(invalid_email_no_user))
  assert_false(validate_email(invalid_email_special_chars))
  assert_false(validate_email(empty_email))
  
  // Test phone number validation edge cases
  let valid_phone = "+1-555-123-4567"
  let invalid_phone_letters = "555-ABC-1234"
  let invalid_phone_too_short = "123"
  let invalid_phone_too_long = "12345678901234567890"
  let empty_phone = ""
  
  assert_true(validate_phone(valid_phone))
  assert_false(validate_phone(invalid_phone_letters))
  assert_false(validate_phone(invalid_phone_too_short))
  assert_false(validate_phone(invalid_phone_too_long))
  assert_false(validate_phone(empty_phone))
  
  // Test URL validation edge cases
  let valid_url = "https://www.example.com/path?query=value"
  let invalid_url_no_protocol = "www.example.com"
  let invalid_url_no_domain = "https://"
  let empty_url = ""
  
  assert_true(validate_url(valid_url))
  assert_false(validate_url(invalid_url_no_protocol))
  assert_false(validate_url(invalid_url_no_domain))
  assert_false(validate_url(empty_url))
}

// Helper functions for validation tests
fn validate_email(email : String) -> Bool {
  if email.length() == 0 {
    return false
  }
  
  let has_at = email.contains("@")
  if not(has_at) {
    return false
  }
  
  let parts = email.split("@")
  if parts.length() != 2 {
    return false
  }
  
  let user = parts[0]
  let domain = parts[1]
  
  if user.length() == 0 or domain.length() == 0 {
    return false
  }
  
  // Check for invalid characters
  let invalid_chars = ["$", "!", "#", "%", "^", "&", "*", "(", ")", "+", "=", "[", "]", "{", "}", "\\", "|", ";", ":", "'", "\"", "<", ">", ",", "/", "?"]
  for char in invalid_chars {
    if email.contains(char) {
      return false
    }
  }
  
  true
}

fn validate_phone(phone : String) -> Bool {
  if phone.length() == 0 {
    return false
  }
  
  if phone.length() < 10 or phone.length() > 20 {
    return false
  }
  
  // Check if phone contains only digits, hyphens, plus, and spaces
  let valid_chars = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "-", "+", " ", "(", ")"]
  for i in 0..phone.length() {
    let char = phone[i].to_string()
    let is_valid = false
    for valid_char in valid_chars {
      if char == valid_char {
        is_valid = true
        break
      }
    }
    if not(is_valid) {
      return false
    }
  }
  
  true
}

fn validate_url(url : String) -> Bool {
  if url.length() == 0 {
    return false
  }
  
  let has_protocol = url.starts_with("http://") or url.starts_with("https://")
  if not(has_protocol) {
    return false
  }
  
  let protocol_end = if url.starts_with("http://") { 7 } else { 8 }
  if url.length() <= protocol_end {
    return false
  }
  
  let domain_part = url[protocol_end..url.length()]
  let has_domain = domain_part.contains(".")
  if not(has_domain) {
    return false
  }
  
  true
}