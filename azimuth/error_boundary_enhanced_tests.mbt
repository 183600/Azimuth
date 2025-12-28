// Error Boundary and Enhanced Condition Tests for Azimuth Telemetry System
// Focus on error handling, edge cases, and boundary conditions

test "null_empty_string_attribute_values" {
  let attrs = Attributes::new()
  
  // Test setting and getting null/empty values
  Attributes::set(attrs, "empty.string", StringValue(""))
  Attributes::set(attrs, "normal.string", StringValue("test"))
  
  // Test empty string retrieval
  @assertion.assert_eq(Attributes::get(attrs, "empty.string"), Some(StringValue("")))?
  @assertion.assert_eq(Attributes::get(attrs, "normal.string"), Some(StringValue("test"))?
  
  // Test non-existent key
  @assertion.assert_eq(Attributes::get(attrs, "non.existent.key"), None)?
}

test "extreme_numeric_values" {
  let attrs = Attributes::new()
  
  // Test extreme numeric values
  Attributes::set(attrs, "max.int", IntValue(2147483647))  // Max 32-bit int
  Attributes::set(attrs, "min.int", IntValue(-2147483648))  // Min 32-bit int
  Attributes::set(attrs, "zero.int", IntValue(0))
  Attributes::set(attrs, "max.float", FloatValue(1.7976931348623157e+308))  // Max double
  Attributes::set(attrs, "min.float", FloatValue(-1.7976931348623157e+308))  // Min double
  Attributes::set(attrs, "inf.float", FloatValue(1.0/0.0))  // Infinity
  Attributes::set(attrs, "neg.inf.float", FloatValue(-1.0/0.0))  // Negative infinity
  
  // Verify retrieval
  @assertion.assert_eq(Attributes::get(attrs, "max.int"), Some(IntValue(2147483647)))?
  @assertion.assert_eq(Attributes::get(attrs, "min.int"), Some(IntValue(-2147483648)))?
  @assertion.assert_eq(Attributes::get(attrs, "zero.int"), Some(IntValue(0)))?
}

test "span_context_edge_cases" {
  // Test with very long trace/span IDs
  let long_trace_id = "a".repeat(128)
  let long_span_id = "b".repeat(64)
  let long_ctx = SpanContext::new(long_trace_id, long_span_id, true, "state=very_long_value")
  
  @assertion.assert_eq(SpanContext::trace_id(long_ctx), long_trace_id)?
  @assertion.assert_eq(SpanContext::span_id(long_ctx), long_span_id)?
  @assertion.assert_eq(SpanContext::is_valid(long_ctx), true)?
  
  // Test with special characters in trace state
  let special_ctx = SpanContext::new("trace123", "span456", true, "key=value&other=test&special=!@#$%^&*()")
  @assertion.assert_eq(SpanContext::is_sampled(special_ctx), true)?
  
  // Test with unicode characters
  let unicode_ctx = SpanContext::new("trâce123", "spän456", true, "状态=测试")
  @assertion.assert_eq(SpanContext::is_valid(unicode_ctx), true)?
}

test "context_with_large_values" {
  let ctx = Context::root()
  let large_key = ContextKey::new("large_key")
  
  // Test with very large string value
  let large_value = "x".repeat(10000)
  let ctx_with_large = Context::with_value(ctx, large_key, large_value)
  
  @assertion.assert_eq(Context::get(ctx_with_large, large_key), Some(large_value))?
  
  // Test with unicode and special characters
  let unicode_key = ContextKey::new("unicode_key")
  let unicode_value = "测试中文字符 🚀 emoji & special chars !@#$%"
  let ctx_with_unicode = Context::with_value(ctx, unicode_key, unicode_value)
  
  @assertion.assert_eq(Context::get(ctx_with_unicode, unicode_key), Some(unicode_value))?
}

test "log_record_severity_boundaries" {
  // Test all severity levels
  let trace_log = LogRecord::new(Trace, "Trace message")
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  let fatal_log = LogRecord::new(Fatal, "Fatal message")
  
  @assertion.assert_eq(LogRecord::severity_number(trace_log), Trace)?
  @assertion.assert_eq(LogRecord::severity_number(debug_log), Debug)?
  @assertion.assert_eq(LogRecord::severity_number(info_log), Info)?
  @assertion.assert_eq(LogRecord::severity_number(warn_log), Warn)?
  @assertion.assert_eq(LogRecord::severity_number(error_log), Error)?
  @assertion.assert_eq(LogRecord::severity_number(fatal_log), Fatal)?
  
  // Test with empty body
  let empty_body_log = LogRecord::new(Info, "")
  @assertion.assert_eq(LogRecord::body(empty_body_log), Some(""))?
}

test "resource_attribute_edge_cases" {
  let resource = Resource::new()
  
  // Test with duplicate attribute keys
  let duplicate_attrs = [
    ("service.name", StringValue("service1")),
    ("service.name", StringValue("service2")),  // This should override the first
    ("service.version", StringValue("1.0.0"))
  ]
  
  let resource_with_duplicates = Resource::with_attributes(resource, duplicate_attrs)
  @assertion.assert_eq(
    Resource::get_attribute(resource_with_duplicates, "service.name"),
    Some(StringValue("service2"))  // Last value should win
  )?
  
  // Test with empty attribute values
  let empty_attrs = [
    ("empty.string", StringValue("")),
    ("empty.array", ArrayStringValue([])),
    ("empty.int", IntValue(0))
  ]
  
  let resource_with_empty = Resource::with_attributes(resource, empty_attrs)
  @assertion.assert_eq(
    Resource::get_attribute(resource_with_empty, "empty.string"),
    Some(StringValue(""))
  )?
}

test "baggage_with_special_characters" {
  let baggage = Baggage::new()
  
  // Test baggage with special characters and unicode
  let baggage_with_special = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_unicode = Baggage::set_entry(baggage_with_special, "用户名", "张三")
  let baggage_with_encoded = Baggage::set_entry(baggage_with_unicode, "encoded.value", "value%20with%20spaces")
  
  @assertion.assert_eq(Baggage::get_entry(baggage_with_encoded, "user.id"), Some("12345"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_with_encoded, "用户名"), Some("张三"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_with_encoded, "encoded.value"), Some("value%20with%20spaces"))?
}

test "http_request_response_boundaries" {
  // Test HTTP request with empty values
  let empty_request = HttpRequest::new("", "", [], None)
  @assertion.assert_eq(HttpRequest::http_method(empty_request), "")?
  @assertion.assert_eq(HttpRequest::url(empty_request), "")?
  @assertion.assert_eq(HttpRequest::body(empty_request), None)?
  
  // Test HTTP response with extreme status codes
  let min_status_response = HttpResponse::new(0, [], None)
  let max_status_response = HttpResponse::new(999, [], None)
  let standard_response = HttpResponse::new(200, [], Some("OK"))
  
  @assertion.assert_eq(HttpResponse::status_code(min_status_response), 0)?
  @assertion.assert_eq(HttpResponse::status_code(max_status_response), 999)?
  @assertion.assert_eq(HttpResponse::status_code(standard_response), 200)?
  @assertion.assert_eq(HttpResponse::body(standard_response), Some("OK"))?
}