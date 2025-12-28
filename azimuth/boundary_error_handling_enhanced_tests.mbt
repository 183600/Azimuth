// Enhanced Boundary Conditions and Error Handling Tests
// Tests edge cases, invalid inputs, and error recovery scenarios

test "attribute_value_extreme_values" {
  // Test with very large numbers
  let large_int = IntValue(2147483647)  // Max 32-bit int
  let large_float = FloatValue(1.7976931348623157e+308)  // Max double
  
  match large_int {
    IntValue(i) => assert_eq(i, 2147483647)
    _ => @test.fail("Expected IntValue for large integer")
  }
  
  match large_float {
    FloatValue(f) => assert_eq(f, 1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue for large float")
  }
  
  // Test with negative values
  let negative_int = IntValue(-2147483648)  // Min 32-bit int
  let negative_float = FloatValue(-1.7976931348623157e+308)
  
  match negative_int {
    IntValue(i) => assert_eq(i, -2147483648)
    _ => @test.fail("Expected IntValue for negative integer")
  }
  
  match negative_float {
    FloatValue(f) => assert_eq(f, -1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue for negative float")
  }
}

test "span_context_invalid_formats" {
  // Test with malformed trace IDs
  let malformed_trace = SpanContext::new("invalid-trace", "span123", true, "")
  assert_eq(SpanContext::is_valid(malformed_trace), false)
  
  // Test with invalid span IDs  
  let malformed_span = SpanContext::new("trace123", "invalid-span", true, "")
  assert_eq(SpanContext::is_valid(malformed_span), false)
  
  // Test with empty trace state
  let empty_state = SpanContext::new("trace123", "span456", true, "")
  assert_eq(SpanContext::is_sampled(empty_state), true)
  assert_eq(SpanContext::is_valid(empty_state), true)
}

test "context_key_edge_cases" {
  let root_ctx = Context::root()
  
  // Test with empty key
  let empty_key = ContextKey::new("")
  let ctx_with_empty = Context::with_value(root_ctx, empty_key, "value")
  assert_eq(Context::get(ctx_with_empty, empty_key), Some("value"))
  
  // Test with very long key
  let long_key_str = "this_is_a_very_long_key_name_that_exceeds_typical_limits_and_should_be_handled_properly"
  let long_key = ContextKey::new(long_key_str)
  let ctx_with_long = Context::with_value(root_ctx, long_key, "long_value")
  assert_eq(Context::get(ctx_with_long, long_key), Some("long_value"))
  
  // Test with special characters in key
  let special_key = ContextKey::new("key.with-special_chars_and_numbers_123")
  let ctx_with_special = Context::with_value(root_ctx, special_key, "special_value")
  assert_eq(Context::get(ctx_with_special, special_key), Some("special_value"))
}

test "array_attribute_boundary_conditions" {
  // Test empty arrays
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty string array")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty int array")
  }
  
  // Test single element arrays
  let single_string = ArrayStringValue(["only"])
  let single_int = ArrayIntValue([42])
  
  match single_string {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "only")
    }
    _ => @test.fail("Expected single element string array")
  }
  
  match single_int {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42)
    }
    _ => @test.fail("Expected single element int array")
  }
}

test "log_record_severity_boundaries" {
  // Test all severity levels
  let trace_log = LogRecord::new(Trace, "Trace message")
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  let fatal_log = LogRecord::new(Fatal, "Fatal message")
  
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Test with empty body
  let empty_body_log = LogRecord::new(Error, "")
  assert_eq(LogRecord::body(empty_body_log), Some(""))
}

test "http_request_response_edge_cases" {
  // Test HTTP request with empty values
  let empty_request = HttpRequest::new("", "", [], None)
  assert_eq(HttpRequest::http_method(empty_request), "")
  assert_eq(HttpRequest::url(empty_request), "")
  assert_eq(HttpRequest::body(empty_request), None)
  
  // Test HTTP response with extreme status codes
  let min_status = HttpResponse::new(0, [], None)
  let max_status = HttpResponse::new(999, [], None)
  
  assert_eq(HttpResponse::status_code(min_status), 0)
  assert_eq(HttpResponse::status_code(max_status), 999)
  
  // Test with empty headers
  let no_headers_request = HttpRequest::new("GET", "https://example.com", [], None)
  assert_eq(HttpRequest::http_method(no_headers_request), "GET")
  assert_eq(HttpRequest::url(no_headers_request), "https://example.com")
}

test "resource_attribute_edge_cases" {
  let resource = Resource::new()
  
  // Test with empty attribute name
  let empty_name_attrs = [("", StringValue("value"))]
  let resource_with_empty_name = Resource::with_attributes(resource, empty_name_attrs)
  assert_eq(Resource::get_attribute(resource_with_empty_name, ""), Some(StringValue("value")))
  
  // Test with empty attribute value
  let empty_value_attrs = [("key", StringValue(""))]
  let resource_with_empty_value = Resource::with_attributes(resource, empty_value_attrs)
  assert_eq(Resource::get_attribute(resource_with_empty_value, "key"), Some(StringValue("")))
  
  // Test with duplicate attribute names (last one should win)
  let duplicate_attrs = [
    ("duplicate", StringValue("first")),
    ("duplicate", StringValue("second"))
  ]
  let resource_with_duplicates = Resource::with_attributes(resource, duplicate_attrs)
  // Note: This depends on implementation - checking if it handles duplicates
  let result = Resource::get_attribute(resource_with_duplicates, "duplicate")
  assert_eq(result.is_some(), true)
}

test "baggage_boundary_conditions" {
  let baggage = Baggage::new()
  
  // Test with empty key and value
  let baggage_empty_key = Baggage::set_entry(baggage, "", "value")
  assert_eq(Baggage::get_entry(baggage_empty_key, ""), Some("value"))
  
  let baggage_empty_value = Baggage::set_entry(baggage, "key", "")
  assert_eq(Baggage::get_entry(baggage_empty_value, "key"), Some(""))
  
  // Test with very long key and value
  let long_key = "this_is_a_very_long_key_name_that_should_be_handled_properly"
  let long_value = "this_is_a_very_long_value_that_should_also_be_handled_properly_with_extended_length"
  
  let baggage_long = Baggage::set_entry(baggage, long_key, long_value)
  assert_eq(Baggage::get_entry(baggage_long, long_key), Some(long_value))
  
  // Test removal of non-existent entry
  let baggage_after_removal = Baggage::remove_entry(baggage, "non_existent")
  assert_eq(Baggage::get_entry(baggage_after_removal, "non_existent"), None)
}