// Error Handling and Boundary Conditions Test Suite for Azimuth
// This file contains comprehensive tests for error handling and edge cases

// Test 1: Empty and null value handling
pub test "empty and null value handling in attributes" {
  let attrs = azimuth::Attributes::new()
  
  // Test empty string values
  azimuth::Attributes::set(attrs, "empty.string", azimuth::StringValue(""))
  let empty_string = azimuth::Attributes::get(attrs, "empty.string")
  assert_eq(empty_string, Some(azimuth::StringValue("")))
  
  // Test zero values
  azimuth::Attributes::set(attrs, "zero.int", azimuth::IntValue(0))
  azimuth::Attributes::set(attrs, "zero.float", azimuth::FloatValue(0.0))
  let zero_int = azimuth::Attributes::get(attrs, "zero.int")
  let zero_float = azimuth::Attributes::get(attrs, "zero.float")
  assert_eq(zero_int, Some(azimuth::IntValue(0)))
  assert_eq(zero_float, Some(azimuth::FloatValue(0.0)))
  
  // Test boolean false
  azimuth::Attributes::set(attrs, "false.bool", azimuth::BoolValue(false))
  let false_bool = azimuth::Attributes::get(attrs, "false.bool")
  assert_eq(false_bool, Some(azimuth::BoolValue(false)))
  
  // Test empty arrays
  azimuth::Attributes::set(attrs, "empty.string.array", azimuth::ArrayStringValue([]))
  azimuth::Attributes::set(attrs, "empty.int.array", azimuth::ArrayIntValue([]))
  let empty_string_array = azimuth::Attributes::get(attrs, "empty.string.array")
  let empty_int_array = azimuth::Attributes::get(attrs, "empty.int.array")
  assert_eq(empty_string_array, Some(azimuth::ArrayStringValue([])))
  assert_eq(empty_int_array, Some(azimuth::ArrayIntValue([])))
}

// Test 2: Maximum boundary values
pub test "maximum boundary values in telemetry data" {
  let attrs = azimuth::Attributes::new()
  
  // Test maximum integer values
  azimuth::Attributes::set(attrs, "max.int", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(attrs, "min.int", azimuth::IntValue(-2147483648))
  let max_int = azimuth::Attributes::get(attrs, "max.int")
  let min_int = azimuth::Attributes::get(attrs, "min.int")
  assert_eq(max_int, Some(azimuth::IntValue(2147483647)))
  assert_eq(min_int, Some(azimuth::IntValue(-2147483648)))
  
  // Test maximum float values
  azimuth::Attributes::set(attrs, "max.float", azimuth::FloatValue(1.7976931348623157e+308))
  azimuth::Attributes::set(attrs, "min.float", azimuth::FloatValue(-1.7976931348623157e+308))
  let max_float = azimuth::Attributes::get(attrs, "max.float")
  let min_float = azimuth::Attributes::get(attrs, "min.float")
  assert_eq(max_float, Some(azimuth::FloatValue(1.7976931348623157e+308)))
  assert_eq(min_float, Some(azimuth::FloatValue(-1.7976931348623157e+308)))
  
  // Test infinity and NaN handling
  azimuth::Attributes::set(attrs, "infinity", azimuth::FloatValue(1.0/0.0))
  azimuth::Attributes::set(attrs, "neg.infinity", azimuth::FloatValue(-1.0/0.0))
  let infinity = azimuth::Attributes::get(attrs, "infinity")
  let neg_infinity = azimuth::Attributes::get(attrs, "neg.infinity")
  assert_eq(infinity, Some(azimuth::FloatValue(1.0/0.0)))
  assert_eq(neg_infinity, Some(azimuth::FloatValue(-1.0/0.0)))
}

// Test 3: Invalid span context handling
pub test "invalid span context handling" {
  // Test empty trace ID and span ID
  let invalid_ctx1 = azimuth::SpanContext::new("", "", false, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_ctx1))
  assert_false(azimuth::SpanContext::is_sampled(invalid_ctx1))
  
  // Test valid trace ID but empty span ID
  let invalid_ctx2 = azimuth::SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", false, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_ctx2))
  
  // Test empty trace ID but valid span ID
  let invalid_ctx3 = azimuth::SpanContext::new("", "b7ad6b7169203331", false, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_ctx3))
  
  // Test valid context
  let valid_ctx = azimuth::SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_true(azimuth::SpanContext::is_valid(valid_ctx))
  assert_true(azimuth::SpanContext::is_sampled(valid_ctx))
}

// Test 4: Long string and special character handling
pub test "long string and special character handling" {
  let attrs = azimuth::Attributes::new()
  
  // Test very long string (1000 characters)
  let long_string = "a" * 1000
  azimuth::Attributes::set(attrs, "long.string", azimuth::StringValue(long_string))
  let retrieved_long = azimuth::Attributes::get(attrs, "long.string")
  assert_eq(retrieved_long, Some(azimuth::StringValue(long_string)))
  
  // Test special characters
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~\\"
  azimuth::Attributes::set(attrs, "special.chars", azimuth::StringValue(special_chars))
  let retrieved_special = azimuth::Attributes::get(attrs, "special.chars")
  assert_eq(retrieved_special, Some(azimuth::StringValue(special_chars)))
  
  // Test Unicode characters
  let unicode_chars = "测试中文字符 🚀 emoji ñáéíóú"
  azimuth::Attributes::set(attrs, "unicode.chars", azimuth::StringValue(unicode_chars))
  let retrieved_unicode = azimuth::Attributes::get(attrs, "unicode.chars")
  assert_eq(retrieved_unicode, Some(azimuth::StringValue(unicode_chars)))
  
  // Test newlines and tabs
  let whitespace_chars = "line1\nline2\tindented\r\nwindows"
  azimuth::Attributes::set(attrs, "whitespace.chars", azimuth::StringValue(whitespace_chars))
  let retrieved_whitespace = azimuth::Attributes::get(attrs, "whitespace.chars")
  assert_eq(retrieved_whitespace, Some(azimuth::StringValue(whitespace_chars)))
}

// Test 5: Context operations with edge cases
pub test "context operations with edge cases" {
  let root_ctx = azimuth::Context::root()
  
  // Test context with empty key
  let empty_key = azimuth::ContextKey::new("")
  let ctx_with_empty_key = azimuth::Context::with_value(root_ctx, empty_key, "value")
  let empty_key_value = azimuth::Context::get(ctx_with_empty_key, empty_key)
  assert_eq(empty_key_value, Some("value"))
  
  // Test context with empty value
  let normal_key = azimuth::ContextKey::new("normal.key")
  let ctx_with_empty_value = azimuth::Context::with_value(root_ctx, normal_key, "")
  let empty_value = azimuth::Context::get(ctx_with_empty_value, normal_key)
  assert_eq(empty_value, Some(""))
  
  // Test context with very long key and value
  let long_key = "very.long.key.name." * 50
  let long_key_obj = azimuth::ContextKey::new(long_key)
  let long_value = "very.long.value." * 50
  let ctx_with_long = azimuth::Context::with_value(root_ctx, long_key_obj, long_value)
  let retrieved_long_value = azimuth::Context::get(ctx_with_long, long_key_obj)
  assert_eq(retrieved_long_value, Some(long_value))
}

// Test 6: Baggage operations with boundary conditions
pub test "baggage operations with boundary conditions" {
  let baggage = azimuth::Baggage::new()
  
  // Test baggage with empty key
  let baggage_empty_key = azimuth::Baggage::set_entry(baggage, "", "value")
  let empty_key_value = azimuth::Baggage::get_entry(baggage_empty_key, "")
  assert_eq(empty_key_value, Some("value"))
  
  // Test baggage with empty value
  let baggage_empty_value = azimuth::Baggage::set_entry(baggage, "key", "")
  let empty_value_result = azimuth::Baggage::get_entry(baggage_empty_value, "key")
  assert_eq(empty_value_result, Some(""))
  
  // Test baggage with special characters in key and value
  let special_key = "special/key?name=value"
  let special_value = "special/value?data=test"
  let baggage_special = azimuth::Baggage::set_entry(baggage, special_key, special_value)
  let special_result = azimuth::Baggage::get_entry(baggage_special, special_key)
  assert_eq(special_result, Some(special_value))
  
  // Test baggage removal of non-existent entry
  let baggage_after_removal = azimuth::Baggage::remove_entry(baggage, "non.existent.key")
  let non_existent_value = azimuth::Baggage::get_entry(baggage_after_removal, "non.existent.key")
  assert_eq(non_existent_value, None)
}

// Test 7: HTTP operations with edge cases
pub test "http operations with edge cases" {
  // Test HTTP request with empty URL
  let empty_headers : Array[(String, String)] = []
  let request_empty_url = azimuth::HttpRequest::new("GET", "", empty_headers)
  assert_eq(azimuth::HttpRequest::url(request_empty_url), "")
  assert_eq(azimuth::HttpRequest::http_method(request_empty_url), "GET")
  
  // Test HTTP request with very long URL
  let long_url = "https://example.com/" + "path/" * 100
  let request_long_url = azimuth::HttpRequest::new("POST", long_url, empty_headers)
  assert_eq(azimuth::HttpRequest::url(request_long_url), long_url)
  assert_eq(azimuth::HttpRequest::http_method(request_long_url), "POST")
  
  // Test HTTP response with extreme status codes
  let response_min_status = azimuth::HttpResponse::new(0, empty_headers)
  let response_max_status = azimuth::HttpResponse::new(999, empty_headers)
  assert_eq(azimuth::HttpResponse::status_code(response_min_status), 0)
  assert_eq(azimuth::HttpResponse::status_code(response_max_status), 999)
  
  // Test HTTP with empty headers
  let request_empty_headers = azimuth::HttpRequest::new("PUT", "https://api.example.com", empty_headers)
  assert_eq(azimuth::HttpRequest::http_method(request_empty_headers), "PUT")
}

// Test 8: Metrics with boundary conditions
pub test "metrics with boundary conditions" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "boundary-test-meter")
  
  // Test counter with negative values
  let counter = azimuth::Meter::create_counter(meter, "negative.test.counter")
  azimuth::Counter::add(counter, -100.0)
  azimuth::Counter::add(counter, 0.0)
  azimuth::Counter::add(counter, 1.7976931348623157e+308)  // Max double
  
  // Test histogram with extreme values
  let histogram = azimuth::Meter::create_histogram(meter, "extreme.histogram")
  azimuth::Histogram::record(histogram, -1.7976931348623157e+308)  // Min double
  azimuth::Histogram::record(histogram, 0.0)
  azimuth::Histogram::record(histogram, 1.7976931348623157e+308)   // Max double
  azimuth::Histogram::record(histogram, 1.0/0.0)                  // Infinity
  
  // Test up-down counter with large values
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "large.updown")
  azimuth::UpDownCounter::add(updown_counter, 1.7976931348623157e+308)
  azimuth::UpDownCounter::add(updown_counter, -1.7976931348623157e+308)
  
  // Test gauge with empty description and unit
  let gauge = azimuth::Meter::create_gauge(meter, "empty.gauge", None, None)
  assert_eq(gauge.name, "empty.gauge")
  assert_eq(gauge.description, None)
  assert_eq(gauge.unit, None)
}