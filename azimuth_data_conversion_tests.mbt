// Azimuth Telemetry System - Data Type Conversion Tests
// This file contains test cases for data type conversion and validation

// Test 1: String to Numeric Conversion
test "string to numeric conversion" {
  // Test valid integer conversion
  let int_str = "42"
  let int_result = int_str.to_int()
  match int_result {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  // Test valid float conversion
  let float_str = "3.14"
  let float_result = float_str.to_float()
  match float_result {
    Some(value) => assert_eq(value, 3.14)
    None => assert_true(false)
  }
  
  // Test invalid integer conversion
  let invalid_int_str = "not_a_number"
  let invalid_int_result = invalid_int_str.to_int()
  match invalid_int_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test invalid float conversion
  let invalid_float_str = "not_a_float"
  let invalid_float_result = invalid_float_str.to_float()
  match invalid_float_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: AttributeValue Type Conversion
test "attribute value type conversion" {
  // Test string to int conversion
  let string_attr = StringValue("123")
  let converted_int = AttributeValue::to_int(string_attr)
  match converted_int {
    Some(IntValue(value)) => assert_eq(value, 123)
    _ => assert_true(false)
  }
  
  // Test string to float conversion
  let float_string_attr = StringValue("3.14159")
  let converted_float = AttributeValue::to_float(float_string_attr)
  match converted_float {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false)
  }
  
  // Test int to string conversion
  let int_attr = IntValue(456)
  let converted_string = AttributeValue::to_string(int_attr)
  match converted_string {
    Some(StringValue(value)) => assert_eq(value, "456")
    _ => assert_true(false)
  }
  
  // Test float to string conversion
  let float_attr = FloatValue(2.71828)
  let converted_float_string = AttributeValue::to_string(float_attr)
  match converted_float_string {
    Some(StringValue(value)) => assert_eq(value, "2.71828")
    _ => assert_true(false)
  }
  
  // Test bool to string conversion
  let bool_attr = BoolValue(true)
  let converted_bool_string = AttributeValue::to_string(bool_attr)
  match converted_bool_string {
    Some(StringValue(value)) => assert_eq(value, "true")
    _ => assert_true(false)
  }
}

// Test 3: Array Type Conversion
test "array type conversion" {
  // Test string array to int array conversion
  let string_array = ArrayStringValue(["1", "2", "3", "4", "5"])
  let int_array = AttributeValue::to_int_array(string_array)
  match int_array {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false)
  }
  
  // Test int array to string array conversion
  let int_array_attr = ArrayIntValue([10, 20, 30])
  let string_array_result = AttributeValue::to_string_array(int_array_attr)
  match string_array_result {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "10")
      assert_eq(values[2], "30")
    }
    _ => assert_true(false)
  }
  
  // Test mixed array conversion (should fail)
  let invalid_array = ArrayStringValue(["1", "invalid", "3"])
  let invalid_conversion = AttributeValue::to_int_array(invalid_array)
  match invalid_conversion {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Test attribute value JSON serialization
  let string_attr = StringValue("test_value")
  let json_string = AttributeValue::to_json(string_attr)
  assert_eq(json_string, "\"test_value\"")
  
  let int_attr = IntValue(42)
  let json_int = AttributeValue::to_json(int_attr)
  assert_eq(json_int, "42")
  
  let bool_attr = BoolValue(true)
  let json_bool = AttributeValue::to_json(bool_attr)
  assert_eq(json_bool, "true")
  
  let float_attr = FloatValue(3.14)
  let json_float = AttributeValue::to_json(float_attr)
  assert_eq(json_float, "3.14")
  
  // Test array JSON serialization
  let string_array = ArrayStringValue(["a", "b", "c"])
  let json_array = AttributeValue::to_json(string_array)
  assert_eq(json_array, "[\"a\", \"b\", \"c\"]")
  
  let int_array = ArrayIntValue([1, 2, 3])
  let json_int_array = AttributeValue::to_json(int_array)
  assert_eq(json_int_array, "[1, 2, 3]")
  
  // Test JSON deserialization
  let deserialized_string = AttributeValue::from_json("\"test_value\"")
  match deserialized_string {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false)
  }
  
  let deserialized_int = AttributeValue::from_json("42")
  match deserialized_int {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let deserialized_bool = AttributeValue::from_json("true")
  match deserialized_bool {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
}

// Test 5: Binary Data Conversion
test "binary data conversion" {
  // Test string to binary conversion
  let text = "Hello, World!"
  let binary_data = String::to_bytes(text)
  assert_eq(binary_data.length(), 13)
  
  // Test binary to string conversion
  let converted_text = Bytes::to_string(binary_data)
  assert_eq(converted_text, "Hello, World!")
  
  // Test binary to hex conversion
  let hex_string = Bytes::to_hex(binary_data)
  assert_eq(hex_string.length(), 26) // 13 bytes * 2 hex chars each
  
  // Test hex to binary conversion
  let converted_binary = Bytes::from_hex(hex_string)
  let back_to_text = Bytes::to_string(converted_binary)
  assert_eq(back_to_text, "Hello, World!")
  
  // Test binary to base64 conversion
  let base64_string = Bytes::to_base64(binary_data)
  assert_true(base64_string.length() > 0)
  
  // Test base64 to binary conversion
  let base64_binary = Bytes::from_base64(base64_string)
  let base64_to_text = Bytes::to_string(base64_binary)
  assert_eq(base64_to_text, "Hello, World!")
}

// Test 6: Timestamp Conversion
test "timestamp conversion" {
  // Test timestamp to string conversion
  let timestamp = 1672531200L // 2023-01-01 00:00:00 UTC
  let timestamp_str = Timestamp::to_string(timestamp)
  assert_eq(timestamp_str, "2023-01-01T00:00:00Z")
  
  // Test string to timestamp conversion
  let parsed_timestamp = Timestamp::from_string("2023-01-01T00:00:00Z")
  match parsed_timestamp {
    Some(value) => assert_eq(value, 1672531200L)
    None => assert_true(false)
  }
  
  // Test timestamp to date components
  let date_components = Timestamp::to_date_components(timestamp)
  assert_eq(date_components.year, 2023)
  assert_eq(date_components.month, 1)
  assert_eq(date_components.day, 1)
  assert_eq(date_components.hour, 0)
  assert_eq(date_components.minute, 0)
  assert_eq(date_components.second, 0)
  
  // Test date components to timestamp
  let reconstructed_timestamp = Timestamp::from_date_components(date_components)
  assert_eq(reconstructed_timestamp, 1672531200L)
  
  // Test timestamp arithmetic
  let later_timestamp = Timestamp::add_hours(timestamp, 24)
  let later_timestamp_str = Timestamp::to_string(later_timestamp)
  assert_eq(later_timestamp_str, "2023-01-02T00:00:00Z")
  
  let earlier_timestamp = Timestamp::subtract_days(timestamp, 1)
  let earlier_timestamp_str = Timestamp::to_string(earlier_timestamp)
  assert_eq(earlier_timestamp_str, "2022-12-31T00:00:00Z")
}

// Test 7: Unit Conversion
test "unit conversion" {
  // Test time unit conversion
  let seconds = 3600
  let hours = UnitConverter::seconds_to_hours(seconds)
  assert_eq(hours, 1.0)
  
  let minutes = UnitConverter::seconds_to_minutes(seconds)
  assert_eq(minutes, 60.0)
  
  let milliseconds = UnitConverter::seconds_to_milliseconds(1.0)
  assert_eq(milliseconds, 1000.0)
  
  // Test data size conversion
  let bytes = 1024
  let kilobytes = UnitConverter::bytes_to_kilobytes(bytes)
  assert_eq(kilobytes, 1.0)
  
  let megabytes = UnitConverter::bytes_to_megabytes(bytes)
  assert_eq(megabytes, 0.001)
  
  let gigabytes = UnitConverter::bytes_to_gigabytes(1073741824) // 1024 * 1024 * 1024
  assert_eq(gigabytes, 1.0)
  
  // Test temperature conversion
  let celsius = 0.0
  let fahrenheit = UnitConverter::celsius_to_fahrenheit(celsius)
  assert_eq(fahrenheit, 32.0)
  
  let kelvin = UnitConverter::celsius_to_kelvin(celsius)
  assert_eq(kelvin, 273.15)
  
  let boiling_celsius = 100.0
  let boiling_fahrenheit = UnitConverter::celsius_to_fahrenheit(boiling_celsius)
  assert_eq(boiling_fahrenheit, 212.0)
}

// Test 8: Encoding Conversion
test "encoding conversion" {
  // Test URL encoding
  let url_text = "Hello World! @#$%"
  let url_encoded = UrlEncoder::encode(url_text)
  assert_eq(url_encoded, "Hello%20World%21%20%40%23%24%25")
  
  let url_decoded = UrlEncoder::decode(url_encoded)
  assert_eq(url_decoded, "Hello World! @#$%")
  
  // Test HTML encoding
  let html_text = "<script>alert('xss')</script>"
  let html_encoded = HtmlEncoder::encode(html_text)
  assert_eq(html_encoded, "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;")
  
  let html_decoded = HtmlEncoder::decode(html_encoded)
  assert_eq(html_decoded, "<script>alert('xss')</script>")
  
  // Test Unicode normalization
  let unicode_text = "café"
  let normalized = UnicodeNormalizer::normalize(unicode_text)
  assert_eq(normalized.length(), 4)
  
  // Test case conversion
  let mixed_case = "Hello World"
  let upper_case = String::to_upper(mixed_case)
  assert_eq(upper_case, "HELLO WORLD")
  
  let lower_case = String::to_lower(mixed_case)
  assert_eq(lower_case, "hello world")
}