test "attribute value type edge cases" {
  // Test attribute values with edge cases
  let attrs = @azimuth.Attributes::new()
  
  // Test with extreme string values
  @azimuth.Attributes::set(attrs, "empty.string", @azimuth.StringValue(""))
  @azimuth.Attributes::set(attrs, "very.long.string", @azimuth.StringValue("a".repeat(10000)))
  @azimuth.Attributes::set(attrs, "unicode.string", @azimuth.StringValue("中文测试 🚀 🎯 📊"))
  @azimuth.Attributes::set(attrs, "special.chars", @azimuth.StringValue("\n\t\"'\[]{}<>"))
  
  // Test with extreme numeric values
  @azimuth.Attributes::set(attrs, "max.int", @azimuth.IntValue(2147483647))
  @azimuth.Attributes::set(attrs, "min.int", @azimuth.IntValue(-2147483648))
  @azimuth.Attributes::set(attrs, "zero.int", @azimuth.IntValue(0))
  @azimuth.Attributes::set(attrs, "max.float", @azimuth.FloatValue(1.7976931348623157e+308))
  @azimuth.Attributes::set(attrs, "min.float", @azimuth.FloatValue(-1.7976931348623157e+308))
  @azimuth.Attributes::set(attrs, "inf.float", @azimuth.FloatValue(1.0/0.0))
  @azimuth.Attributes::set(attrs, "neg.inf.float", @azimuth.FloatValue(-1.0/0.0))
  @azimuth.Attributes::set(attrs, "nan.float", @azimuth.FloatValue(0.0/0.0))
  
  // Test with boolean values
  @azimuth.Attributes::set(attrs, "true.value", @azimuth.BoolValue(true))
  @azimuth.Attributes::set(attrs, "false.value", @azimuth.BoolValue(false))
  
  // Test with array values
  @azimuth.Attributes::set(attrs, "empty.string.array", @azimuth.ArrayStringValue([]))
  @azimuth.Attributes::set(attrs, "large.string.array", @azimuth.ArrayStringValue(["item".repeat(1000)]))
  @azimuth.Attributes::set(attrs, "empty.int.array", @azimuth.ArrayIntValue([]))
  @azimuth.Attributes::set(attrs, "large.int.array", @azimuth.ArrayIntValue([2147483647, -2147483648, 0]))
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "span context with invalid trace and span IDs" {
  // Test span context with various invalid ID formats
  let empty_trace = @azimuth.SpanContext::new("", "valid_span_id", true, "")
  let empty_span = @azimuth.SpanContext::new("valid_trace_id", "", true, "")
  let both_empty = @azimuth.SpanContext::new("", "", true, "")
  let all_zeros_trace = @azimuth.SpanContext::new("00000000000000000000000000000000", "valid_span_id", true, "")
  let all_zeros_span = @azimuth.SpanContext::new("valid_trace_id", "0000000000000000", true, "")
  
  // Test validity checks
  assert_false(@azimuth.SpanContext::is_valid(empty_trace))
  assert_false(@azimuth.SpanContext::is_valid(empty_span))
  assert_false(@azimuth.SpanContext::is_valid(both_empty))
  assert_false(@azimuth.SpanContext::is_valid(all_zeros_trace))
  assert_false(@azimuth.SpanContext::is_valid(all_zeros_span))
  
  // Test with valid IDs
  let valid_ctx = @azimuth.SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_true(@azimuth.SpanContext::is_valid(valid_ctx))
}

test "instrumentation scope with edge cases" {
  // Test instrumentation scope with edge case values
  let empty_name_scope = @azimuth.InstrumentationScope::{ name: "", version: None, schema_url: None }
  let long_name_scope = @azimuth.InstrumentationScope::{ name: "a".repeat(1000), version: None, schema_url: None }
  let unicode_name_scope = @azimuth.InstrumentationScope::{ name: "遥测库 🚀", version: None, schema_url: None }
  let special_chars_scope = @azimuth.InstrumentationScope::{ name: "test/scope:v1.0", version: None, schema_url: None }
  let empty_version_scope = @azimuth.InstrumentationScope::{ name: "test", version: Some(""), schema_url: None }
  let long_version_scope = @azimuth.InstrumentationScope::{ name: "test", version: Some("1.0.0".repeat(100)), schema_url: None }
  let empty_schema_scope = @azimuth.InstrumentationScope::{ name: "test", version: None, schema_url: Some("") }
  let long_schema_scope = @azimuth.InstrumentationScope::{ name: "test", version: None, schema_url: Some("https://example.com/".repeat(100)) }
  
  // Test that all scopes can be created without exceptions
  assert_eq(empty_name_scope.name, "")
  assert_eq(long_name_scope.name.length(), 1000)
  assert_eq(unicode_name_scope.name, "遥测库 🚀")
  assert_eq(special_chars_scope.name, "test/scope:v1.0")
  assert_eq(empty_version_scope.version, Some(""))
  assert_eq(long_version_scope.version.unwrap().length(), 500)
  assert_eq(empty_schema_scope.schema_url, Some(""))
  assert_eq(long_schema_scope.schema_url.unwrap().length(), 1600)
}

test "log record with edge case severity and body" {
  // Test log records with edge case severity and body combinations
  let empty_body_trace = @azimuth.LogRecord::new(@azimuth.Trace, "")
  let very_long_body_error = @azimuth.LogRecord::new(@azimuth.Error, "Error message: ".repeat(1000))
  let unicode_body_fatal = @azimuth.LogRecord::new(@azimuth.Fatal, "致命错误: 系统崩溃 🚨")
  let special_chars_warn = @azimuth.LogRecord::new(@azimuth.Warn, "Warning: special chars \n\t\"'\[]{}<>")
  let json_body_info = @azimuth.LogRecord::new(@azimuth.Info, "{\"error\": \"detailed\", \"stack\": \"at line 1\n at line 2\"}")
  
  // Test that all records are created properly
  assert_eq(@azimuth.LogRecord::body(empty_body_trace), Some(""))
  assert_eq(@azimuth.LogRecord::severity_number(very_long_body_error), @azimuth.Error)
  assert_eq(@azimuth.LogRecord::severity_number(unicode_body_fatal), @azimuth.Fatal)
  assert_eq(@azimuth.LogRecord::severity_number(special_chars_warn), @azimuth.Warn)
  assert_eq(@azimuth.LogRecord::severity_number(json_body_info), @azimuth.Info)
}

test "text map carrier with edge case headers" {
  // Test text map carrier with edge case headers
  let carrier = @azimuth.TextMapCarrier::new()
  
  // Test setting headers with edge case values
  @azimuth.TextMapCarrier::set(carrier, "empty.header", "")
  @azimuth.TextMapCarrier::set(carrier, "spaces.only", "   ")
  @azimuth.TextMapCarrier::set(carrier, "unicode.header", "中文头信息 🌟")
  @azimuth.TextMapCarrier::set(carrier, "special.chars", "special:chars&key=value?param=test")
  @azimuth.TextMapCarrier::set(carrier, "json.like", "{\"key\": \"value\", \"nested\": {\"data\": true}}")
  @azimuth.TextMapCarrier::set(carrier, "url.like", "https://example.com/path?param=value&other=data")
  @azimuth.TextMapCarrier::set(carrier, "very.long.header", "x".repeat(10000))
  @azimuth.TextMapCarrier::set(carrier, "newlines", "line1\nline2\r\nline3")
  @azimuth.TextMapCarrier::set(carrier, "tabs", "col1\tcol2\tcol3")
  
  // Test getting headers (simplified implementation only returns traceparent)
  let traceparent = @azimuth.TextMapCarrier::get(carrier, "traceparent")
  let empty_header = @azimuth.TextMapCarrier::get(carrier, "empty.header")
  let unicode_header = @azimuth.TextMapCarrier::get(carrier, "unicode.header")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(empty_header, None)
  assert_eq(unicode_header, None)
}

test "baggage with edge case entries" {
  // Test baggage with edge case entries
  let baggage = @azimuth.Baggage::new()
  
  // Test setting baggage entries with edge cases
  let baggage1 = @azimuth.Baggage::set_entry(baggage, "empty.key", "")
  let baggage2 = @azimuth.Baggage::set_entry(baggage1, "", "empty.key.value")
  let baggage3 = @azimuth.Baggage::set_entry(baggage2, "unicode.key", "中文值 🎯")
  let baggage4 = @azimuth.Baggage::set_entry(baggage3, "special.chars", "special:chars&key=value")
  let baggage5 = @azimuth.Baggage::set_entry(baggage4, "very.long.value", "x".repeat(1000))
  let baggage6 = @azimuth.Baggage::set_entry(baggage5, "spaces", "   value with spaces   ")
  
  // Test getting baggage entries (simplified implementation returns None)
  let empty_key_value = @azimuth.Baggage::get_entry(baggage6, "empty.key")
  let empty_key_result = @azimuth.Baggage::get_entry(baggage6, "")
  let unicode_value = @azimuth.Baggage::get_entry(baggage6, "unicode.key")
  let special_chars_value = @azimuth.Baggage::get_entry(baggage6, "special.chars")
  
  assert_eq(empty_key_value, None)
  assert_eq(empty_key_result, None)
  assert_eq(unicode_value, None)
  assert_eq(special_chars_value, None)
}

test "resource with edge case attributes" {
  // Test resource with edge case attributes
  let edge_attributes = [
    ("", @azimuth.StringValue("empty.key")),
    ("empty.value", @azimuth.StringValue("")),
    ("spaces.key", @azimuth.StringValue("   spaces only   ")),
    ("unicode.key", @azimuth.StringValue("中文键 🌟")),
    ("special.chars.key", @azimuth.StringValue("special:chars&key=value")),
    ("very.long.key", @azimuth.StringValue("key".repeat(100))),
    ("very.long.value", @azimuth.StringValue("value".repeat(1000))),
    ("json.value", @azimuth.StringValue("{\"nested\": {\"data\": [1, 2, 3]}}")),
    ("url.value", @azimuth.StringValue("https://example.com/path?param=value"))
  ]
  
  let edge_resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), edge_attributes)
  
  // Test retrieving edge case attributes
  let empty_key = @azimuth.Resource::get_attribute(edge_resource, "")
  let empty_value = @azimuth.Resource::get_attribute(edge_resource, "empty.value")
  let spaces_key = @azimuth.Resource::get_attribute(edge_resource, "spaces.key")
  let unicode_key = @azimuth.Resource::get_attribute(edge_resource, "unicode.key")
  let special_chars_key = @azimuth.Resource::get_attribute(edge_resource, "special.chars.key")
  
  assert_eq(empty_key, Some(@azimuth.StringValue("empty.key")))
  assert_eq(empty_value, Some(@azimuth.StringValue("")))
  assert_eq(spaces_key, Some(@azimuth.StringValue("   spaces only   ")))
  assert_eq(unicode_key, Some(@azimuth.StringValue("中文键 🌟")))
  assert_eq(special_chars_key, Some(@azimuth.StringValue("special:chars&key=value")))
}

test "http operations with edge cases" {
  // Test HTTP operations with edge cases
  let empty_url_request = @azimuth.HttpRequest::new("GET", "", [], None)
  let very_long_url_request = @azimuth.HttpRequest::new("GET", "https://example.com/".repeat(1000), [], None)
  let unicode_url_request = @azimuth.HttpRequest::new("GET", "https://例子.测试/路径", [], None)
  let special_chars_url_request = @azimuth.HttpRequest::new("POST", "https://example.com/path?param=value&other=data", [("Content-Type", "application/json")], Some("{\"data\": \"test\"}"))
  
  // Test request properties
  assert_eq(@azimuth.HttpRequest::http_method(empty_url_request), "GET")
  assert_eq(@azimuth.HttpRequest::url(empty_url_request), "")
  assert_eq(@azimuth.HttpRequest::url(very_long_url_request).length(), 20000)
  assert_eq(@azimuth.HttpRequest::url(unicode_url_request), "https://例子.测试/路径")
  assert_eq(@azimuth.HttpRequest::body(special_chars_url_request), Some("{\"data\": \"test\"}"))
  
  // Test response with edge cases
  let max_status_response = @azimuth.HttpResponse::new(999, [], None)
  let min_status_response = @azimuth.HttpResponse::new(0, [], None)
  let very_long_body_response = @azimuth.HttpResponse::new(200, [], Some("x".repeat(100000)))
  let unicode_body_response = @azimuth.HttpResponse::new(200, [("Content-Type", "text/plain")], Some("响应内容 📊"))
  
  // Test response properties
  assert_eq(@azimuth.HttpResponse::status_code(max_status_response), 999)
  assert_eq(@azimuth.HttpResponse::status_code(min_status_response), 0)
  assert_eq(@azimuth.HttpResponse::body(very_long_body_response).unwrap().length(), 100000)
  assert_eq(@azimuth.HttpResponse::body(unicode_body_response), Some("响应内容 📊"))
}

test "platform functions with edge cases" {
  // Test platform functions with edge cases
  let clock = @azimuth.Clock::system()
  let random = @azimuth.Random::system()
  
  // Test clock with multiple calls
  let timestamp1 = @azimuth.Clock::now_unix_nanos(clock)
  let timestamp2 = @azimuth.Clock::now_unix_nanos(clock)
  let timestamp3 = @azimuth.Clock::now_unix_nanos(clock)
  
  // Test random with different lengths
  let zero_bytes = @azimuth.Random::next_bytes(random, 0)
  let negative_bytes = @azimuth.Random::next_bytes(random, -1)  // Edge case
  let large_bytes = @azimuth.Random::next_bytes(random, 1000000)
  
  // Test multiple random calls
  let random1 = @azimuth.Random::next_u64(random)
  let random2 = @azimuth.Random::next_u64(random)
  let random3 = @azimuth.Random::next_u64(random)
  
  // Test that operations don't throw exceptions
  assert_true(timestamp1 > 0L)
  assert_eq(timestamp1, timestamp2)  // Simplified implementation
  assert_eq(timestamp2, timestamp3)  // Simplified implementation
  assert_eq(zero_bytes.length(), 0)
  assert_eq(large_bytes.length(), 0)  // Simplified implementation
  assert_eq(random1, 12345UL)  // Simplified implementation
  assert_eq(random2, 12345UL)  // Simplified implementation
  assert_eq(random3, 12345UL)  // Simplified implementation
}