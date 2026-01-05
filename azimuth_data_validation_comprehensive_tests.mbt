// Azimuth Telemetry System - Data Validation Comprehensive Tests
// This file contains comprehensive test cases for data validation

// Test 1: Attribute Value Type Validation
test "attribute value type validation" {
  // Test string attribute validation
  let valid_string = StringValue("valid_string")
  assert_true(AttributeValidator::is_valid(valid_string))
  
  let empty_string = StringValue("")
  assert_true(AttributeValidator::is_valid(empty_string))  // Empty string is valid
  
  let long_string = StringValue("x".repeat(10000))
  assert_false(AttributeValidator::is_valid(long_string))  // Too long
  
  // Test int attribute validation
  let valid_int = IntValue(42)
  assert_true(AttributeValidator::is_valid(valid_int))
  
  let large_int = IntValue(2147483647)
  assert_true(AttributeValidator::is_valid(large_int))  // Max 32-bit int
  
  let too_large_int = IntValue(2147483648)
  assert_false(AttributeValidator::is_valid(too_large_int))  // Too large
  
  // Test float attribute validation
  let valid_float = FloatValue(3.14)
  assert_true(AttributeValidator::is_valid(valid_float))
  
  let inf_float = FloatValue(1.0 / 0.0)  // Infinity
  assert_false(AttributeValidator::is_valid(inf_float))
  
  let nan_float = FloatValue(0.0 / 0.0)  // NaN
  assert_false(AttributeValidator::is_valid(nan_float))
  
  // Test bool attribute validation
  let true_bool = BoolValue(true)
  assert_true(AttributeValidator::is_valid(true_bool))
  
  let false_bool = BoolValue(false)
  assert_true(AttributeValidator::is_valid(false_bool))
  
  // Test array attribute validation
  let valid_array = ArrayStringValue(["a", "b", "c"])
  assert_true(AttributeValidator::is_valid(valid_array))
  
  let empty_array = ArrayStringValue([])
  assert_true(AttributeValidator::is_valid(empty_array))
  
  let large_array = ArrayStringValue(["x"].repeat(1000))
  assert_false(AttributeValidator::is_valid(large_array))
}

// Test 2: Span Data Validation
test "span data validation" {
  // Test valid span
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_span_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "")
  let valid_span = Span::new("valid_operation", Internal, valid_span_ctx)
  assert_true(SpanValidator::is_valid(valid_span))
  
  // Test invalid trace ID
  let invalid_trace_id = "invalid"
  let invalid_trace_span_ctx = SpanContext::new(invalid_trace_id, valid_span_id, true, "")
  let invalid_trace_span = Span::new("invalid_trace_operation", Internal, invalid_trace_span_ctx)
  assert_false(SpanValidator::is_valid(invalid_trace_span))
  
  // Test invalid span ID
  let invalid_span_id = "invalid"
  let invalid_span_span_ctx = SpanContext::new(valid_trace_id, invalid_span_id, true, "")
  let invalid_span_span = Span::new("invalid_span_operation", Internal, invalid_span_span_ctx)
  assert_false(SpanValidator::is_valid(invalid_span_span))
  
  // Test span with invalid name
  let empty_name_span = Span::new("", Internal, valid_span_ctx)
  assert_false(SpanValidator::is_valid(empty_name_span))
  
  let long_name_span = Span::new("x".repeat(300), Internal, valid_span_ctx)
  assert_false(SpanValidator::is_valid(long_name_span))
  
  // Test span with invalid timestamps
  let future_time = Time::now() + 86400000000000L  // 1 day in future
  let invalid_time_span = Span::new_with_time("invalid_time_operation", Internal, valid_span_ctx, future_time)
  assert_false(SpanValidator::is_valid(invalid_time_span))
}

// Test 3: Log Record Validation
test "log record validation" {
  // Test valid log record
  let valid_attrs = Attributes::new()
  Attributes::set(valid_attrs, "key", StringValue("value"))
  
  let valid_log = LogRecord::new_with_context(
    Info,
    Some("Valid log message"),
    Some(valid_attrs),
    Some(Time::now()),
    Some(Time::now()),
    Some("trace_id"),
    Some("span_id"),
    Some(Context::root())
  )
  assert_true(LogRecordValidator::is_valid(valid_log))
  
  // Test invalid severity
  let invalid_severity_log = LogRecord::new_with_context(
    999,  // Invalid severity
    Some("Invalid severity log"),
    Some(valid_attrs),
    Some(Time::now()),
    Some(Time::now()),
    Some("trace_id"),
    Some("span_id"),
    Some(Context::root())
  )
  assert_false(LogRecordValidator::is_valid(invalid_severity_log))
  
  // Test log with invalid timestamp
  let negative_time_log = LogRecord::new_with_context(
    Info,
    Some("Negative time log"),
    Some(valid_attrs),
    Some(-1000L),  // Negative timestamp
    Some(Time::now()),
    Some("trace_id"),
    Some("span_id"),
    Some(Context::root())
  )
  assert_false(LogRecordValidator::is_valid(negative_time_log))
  
  // Test log with invalid trace ID
  let invalid_trace_log = LogRecord::new_with_context(
    Info,
    Some("Invalid trace log"),
    Some(valid_attrs),
    Some(Time::now()),
    Some(Time::now()),
    Some(""),  // Empty trace ID
    Some("span_id"),
    Some(Context::root())
  )
  assert_false(LogRecordValidator::is_valid(invalid_trace_log))
  
  // Test log with too long message
  let long_message = "x".repeat(100000)
  let long_message_log = LogRecord::new_with_context(
    Info,
    Some(long_message),
    Some(valid_attrs),
    Some(Time::now()),
    Some(Time::now()),
    Some("trace_id"),
    Some("span_id"),
    Some(Context::root())
  )
  assert_false(LogRecordValidator::is_valid(long_message_log))
}

// Test 4: Metric Data Validation
test "metric data validation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "validation_meter")
  
  // Test valid metric name
  let valid_counter = Meter::create_counter(meter, "valid_counter", Some("Valid counter"), Some("count"))
  assert_true(MetricValidator::is_valid(valid_counter))
  
  // Test invalid metric name
  let invalid_name_counter = Meter::create_counter(meter, "", Some("Invalid name counter"), Some("count"))
  assert_false(MetricValidator::is_valid(invalid_name_counter))
  
  let long_name_counter = Meter::create_counter(meter, "x".repeat(300), Some("Long name counter"), Some("count"))
  assert_false(MetricValidator::is_valid(long_name_counter))
  
  // Test valid metric values
  Counter::add(valid_counter, 1.0)
  Counter::add(valid_counter, 100.5)
  assert_true(MetricValidator::has_valid_values(valid_counter))
  
  // Test invalid metric values
  Counter::add(valid_counter, 1.0 / 0.0)  // Infinity
  assert_false(MetricValidator::has_valid_values(valid_counter))
  
  // Test valid histogram
  let valid_histogram = Meter::create_histogram(meter, "valid_histogram", Some("Valid histogram"), Some("ms"))
  Histogram::record(valid_histogram, 1.0)
  Histogram::record(valid_histogram, 100.5)
  assert_true(MetricValidator::is_valid(valid_histogram))
  
  // Test invalid histogram values
  Histogram::record(valid_histogram, -1.0)  // Negative value
  assert_false(MetricValidator::has_valid_values(valid_histogram))
}

// Test 5: Resource Validation
test "resource validation" {
  // Test valid resource
  let valid_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let valid_resource = Resource::with_attributes(Resource::new(), valid_attrs)
  assert_true(ResourceValidator::is_valid(valid_resource))
  
  // Test resource without required service name
  let no_service_name_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("1.0.0"))
  ])
  assert_false(ResourceValidator::is_valid(no_service_name_resource))
  
  // Test resource with invalid service name
  let invalid_service_name_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("")),  // Empty service name
    ("service.version", StringValue("1.0.0"))
  ])
  assert_false(ResourceValidator::is_valid(invalid_service_name_resource))
  
  // Test resource with invalid version format
  let invalid_version_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("invalid.version.format"))
  ])
  assert_false(ResourceValidator::is_valid(invalid_version_resource))
  
  // Test resource with too many attributes
  let many_attrs = []
  for i in 0..2000 {
    many_attrs.push(("attr_" + i.to_string(), StringValue("value_" + i.to_string())))
  }
  
  let too_many_attrs_resource = Resource::with_attributes(Resource::new(), many_attrs)
  assert_false(ResourceValidator::is_valid(too_many_attrs_resource))
}

// Test 6: Context Validation
test "context validation" {
  // Test valid context
  let key = ContextKey::new("test_key")
  let valid_ctx = Context::with_value(Context::root(), key, "test_value")
  assert_true(ContextValidator::is_valid(valid_ctx))
  
  // Test context with invalid key
  let empty_key_ctx = Context::with_value(Context::root(), ContextKey::new(""), "test_value")
  assert_false(ContextValidator::is_valid(empty_key_ctx))
  
  // Test context with too many values
  let ctx_with_many_values = Context::root()
  for i in 0..1000 {
    let key = ContextKey::new("key_" + i.to_string())
    ctx_with_many_values = Context::with_value(ctx_with_many_values, key, "value_" + i.to_string())
  }
  assert_false(ContextValidator::is_valid(ctx_with_many_values))
  
  // Test baggage validation
  let valid_baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(valid_baggage, "valid_key", "valid_value")
  let ctx_with_valid_baggage = Context::with_baggage(Context::root(), updated_baggage)
  assert_true(ContextValidator::is_valid(ctx_with_valid_baggage))
  
  // Test baggage with invalid key
  let invalid_baggage = Baggage::set_entry(valid_baggage, "", "value")
  let ctx_with_invalid_baggage = Context::with_baggage(Context::root(), invalid_baggage)
  assert_false(ContextValidator::is_valid(ctx_with_invalid_baggage))
}

// Test 7: Trace ID and Span ID Validation
test "trace id and span id validation" {
  // Test valid trace IDs
  let valid_trace_ids = [
    "0af7651916cd43dd8448eb211c80319c",
    "1234567890abcdef1234567890abcdef",
    "ffffffffffffffffffffffffffffffff"
  ]
  
  for trace_id in valid_trace_ids {
    assert_true(IdValidator::is_valid_trace_id(trace_id))
  }
  
  // Test invalid trace IDs
  let invalid_trace_ids = [
    "",  // Empty
    "123",  // Too short
    "1234567890abcdef1234567890abcde",  // Too short (31 chars)
    "1234567890abcdef1234567890abcdef1",  // Too long (33 chars)
    "gggggggggggggggggggggggggggggggg",  // Invalid hex characters
    "1234567890ABCDEFG1234567890ABCDEF"  // Uppercase hex
  ]
  
  for trace_id in invalid_trace_ids {
    assert_false(IdValidator::is_valid_trace_id(trace_id))
  }
  
  // Test valid span IDs
  let valid_span_ids = [
    "b7ad6b7169203331",
    "1234567890abcdef",
    "ffffffffffffffff"
  ]
  
  for span_id in valid_span_ids {
    assert_true(IdValidator::is_valid_span_id(span_id))
  }
  
  // Test invalid span IDs
  let invalid_span_ids = [
    "",  // Empty
    "123",  // Too short
    "1234567890abcde",  // Too short (15 chars)
    "1234567890abcdef1",  // Too long (17 chars)
    "gggggggggggggggg",  // Invalid hex characters
    "1234567890ABCDEF"  // Uppercase hex
  ]
  
  for span_id in invalid_span_ids {
    assert_false(IdValidator::is_valid_span_id(span_id))
  }
}

// Test 8: HTTP Request/Response Validation
test "http request response validation" {
  // Test valid HTTP request
  let valid_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123")
  ]
  
  let valid_request = HttpRequest::new("GET", "https://example.com/api", valid_headers, Some("request body"))
  assert_true(HttpValidator::is_valid_request(valid_request))
  
  // Test invalid HTTP method
  let invalid_method_request = HttpRequest::new("INVALID", "https://example.com/api", valid_headers, None)
  assert_false(HttpValidator::is_valid_request(invalid_method_request))
  
  // Test invalid URL
  let invalid_url_request = HttpRequest::new("GET", "not-a-url", valid_headers, None)
  assert_false(HttpValidator::is_valid_request(invalid_url_request))
  
  // Test request with too large body
  let large_body = "x".repeat(10000000)  // 10MB
  let large_body_request = HttpRequest::new("POST", "https://example.com/api", valid_headers, Some(large_body))
  assert_false(HttpValidator::is_valid_request(large_body_request))
  
  // Test valid HTTP response
  let valid_response_headers = [
    ("Content-Type", "application/json"),
    ("Content-Length", "1024")
  ]
  
  let valid_response = HttpResponse::new(200, valid_response_headers, Some("response body"))
  assert_true(HttpValidator::is_valid_response(valid_response))
  
  // Test invalid status code
  let invalid_status_response = HttpResponse::new(999, valid_response_headers, Some("response body"))
  assert_false(HttpValidator::is_valid_response(invalid_status_response))
  
  // Test response with too large body
  let large_response_body = "x".repeat(10000000)  // 10MB
  let large_body_response = HttpResponse::new(200, valid_response_headers, Some(large_response_body))
  assert_false(HttpValidator::is_valid_response(large_body_response))
}

// Test 9: Custom Validation Rules
test "custom validation rules" {
  // Create custom validator
  let custom_validator = CustomValidator::new()
  
  // Add custom rule for service name
  CustomValidator::add_rule(custom_validator, "service.name", fn(value) {
    match value {
      StringValue(v) => {
        v.length() >= 3 && v.length() <= 50 && v.chars().all(fn(c) { c.is_alphanumeric() || c == '-' || c == '_' })
      }
      _ => false
    }
  })
  
  // Add custom rule for version
  CustomValidator::add_rule(custom_validator, "service.version", fn(value) {
    match value {
      StringValue(v) => {
        let parts = v.split(".")
        parts.length() == 3 && parts.all(fn(part) { part.chars().all(fn(c) { c.is_numeric() }) })
      }
      _ => false
    }
  })
  
  // Test valid values
  let valid_service_name = StringValue("my-service")
  let valid_version = StringValue("1.0.0")
  
  assert_true(CustomValidator::validate(custom_validator, "service.name", valid_service_name))
  assert_true(CustomValidator::validate(custom_validator, "service.version", valid_version))
  
  // Test invalid service name
  let invalid_service_name1 = StringValue("my")  // Too short
  let invalid_service_name2 = StringValue("my service")  // Contains space
  let invalid_service_name3 = StringValue("x".repeat(60))  // Too long
  
  assert_false(CustomValidator::validate(custom_validator, "service.name", invalid_service_name1))
  assert_false(CustomValidator::validate(custom_validator, "service.name", invalid_service_name2))
  assert_false(CustomValidator::validate(custom_validator, "service.name", invalid_service_name3))
  
  // Test invalid version
  let invalid_version1 = StringValue("1.0")  // Missing patch version
  let invalid_version2 = StringValue("1.0.0.0")  // Too many parts
  let invalid_version3 = StringValue("1.a.0")  // Non-numeric part
  
  assert_false(CustomValidator::validate(custom_validator, "service.version", invalid_version1))
  assert_false(CustomValidator::validate(custom_validator, "service.version", invalid_version2))
  assert_false(CustomValidator::validate(custom_validator, "service.version", invalid_version3))
}

// Test 10: Validation Error Reporting
test "validation error reporting" {
  // Create validator with detailed error reporting
  let validator = DetailedValidator::new()
  
  // Test valid span
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_span_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "")
  let valid_span = Span::new("valid_operation", Internal, valid_span_ctx)
  
  let valid_result = DetailedValidator::validate_span(validator, valid_span)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid span with multiple errors
  let invalid_span = Span::new("", Internal, SpanContext::new("", "", true, ""))
  Span::add_event(invalid_span, "", Some([("", StringValue(""))]))  // Invalid event
  
  let invalid_result = DetailedValidator::validate_span(validator, invalid_span)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() > 0)
  
  // Verify specific errors
  let has_name_error = invalid_result.errors.any(fn(error) {
    error.field == "name" && error.code == "EMPTY_NAME"
  })
  assert_true(has_name_error)
  
  let has_trace_id_error = invalid_result.errors.any(fn(error) {
    error.field == "trace_id" && error.code == "INVALID_TRACE_ID"
  })
  assert_true(has_trace_id_error)
  
  let has_span_id_error = invalid_result.errors.any(fn(error) {
    error.field == "span_id" && error.code == "INVALID_SPAN_ID"
  })
  assert_true(has_span_id_error)
  
  // Test warning generation
  let warning_span = Span::new("warning_operation", Internal, valid_span_ctx)
  Span::add_event(warning_span, "test_event", Some([("large_attr", StringValue("x".repeat(9000)))]))
  
  let warning_result = DetailedValidator::validate_span(validator, warning_span)
  assert_true(warning_result.is_valid)  // Still valid, but with warnings
  assert_true(warning_result.warnings.length() > 0)
  
  let has_large_attr_warning = warning_result.warnings.any(fn(warning) {
    warning.field == "attributes" && warning.code == "LARGE_ATTRIBUTE_VALUE"
  })
  assert_true(has_large_attr_warning)
  
  // Test error message formatting
  let error_message = DetailedValidator::format_errors(invalid_result)
  assert_true(StringUtils::contains(error_message, "name"))
  assert_true(StringUtils::contains(error_message, "trace_id"))
  assert_true(StringUtils::contains(error_message, "span_id"))
  
  let warning_message = DetailedValidator::format_warnings(warning_result)
  assert_true(StringUtils::contains(warning_message, "large_attr"))
}