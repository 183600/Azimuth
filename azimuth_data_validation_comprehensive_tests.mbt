// Azimuth Telemetry System - Data Validation Comprehensive Tests
// This file contains comprehensive data validation test cases

// Test 1: Trace ID Validation
test "trace ID validation" {
  // Valid trace IDs (32-character hexadecimal)
  let valid_trace_ids = [
    "0af7651916cd43dd8448eb211c80319c",
    "1234567890abcdef1234567890abcdef",
    "ffffffffffffffffffffffffffffffff",
    "00000000000000000000000000000000"
  ]
  
  for trace_id in valid_trace_ids {
    let span_ctx = SpanContext::new(trace_id, "1234567890abcdef", true, "test")
    assert_true(SpanContext::is_valid(span_ctx))
    assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  }
  
  // Invalid trace IDs
  let invalid_trace_ids = [
    "", // Empty
    "short", // Too short
    "g1234567890abcdef1234567890abcde", // Contains non-hex character
    "1234567890abcdef1234567890abcde", // 31 characters
    "1234567890abcdef1234567890abcdef1", // 33 characters
    "1234567890ABCDEFG1234567890ABCDEF", // Uppercase
    "123 4567890abcdef1234567890abcdef", // Contains space
    "123-4567890abcdef1234567890abcdef"  // Contains dash
  ]
  
  for trace_id in invalid_trace_ids {
    let span_ctx = SpanContext::new(trace_id, "1234567890abcdef", true, "test")
    assert_false(SpanContext::is_valid(span_ctx))
  }
}

// Test 2: Span ID Validation
test "span ID validation" {
  // Valid span IDs (16-character hexadecimal)
  let valid_span_ids = [
    "b7ad6b7169203331",
    "1234567890abcdef",
    "ffffffffffffffff",
    "0000000000000000"
  ]
  
  for span_id in valid_span_ids {
    let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", span_id, true, "test")
    assert_true(SpanContext::is_valid(span_ctx))
    assert_eq(SpanContext::span_id(span_ctx), span_id)
  }
  
  // Invalid span IDs
  let invalid_span_ids = [
    "", // Empty
    "short", // Too short
    "g1234567890abcde", // Contains non-hex character
    "1234567890abcde", // 15 characters
    "1234567890abcdef1", // 17 characters
    "1234567890ABCDEF", // Uppercase
    "123 4567890abcdef", // Contains space
    "123-4567890abcdef"  // Contains dash
  ]
  
  for span_id in invalid_span_ids {
    let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", span_id, true, "test")
    assert_false(SpanContext::is_valid(span_ctx))
  }
}

// Test 3: Attribute Key Validation
test "attribute key validation" {
  let attrs = Attributes::new()
  
  // Valid attribute keys
  let valid_keys = [
    "valid.key",
    "valid_key_with_underscores",
    "valid-key-with-dashes",
    "valid.key.with.multiple.parts",
    "key123withnumbers456",
    "a", // Single character
    "valid.key.with.trailing.",
    ".valid.key.with.leading"
  ]
  
  for key in valid_keys {
    Attributes::set(attrs, key, StringValue("test_value"))
    let retrieved = Attributes::get(attrs, key)
    match retrieved {
      Some(StringValue(v)) => assert_eq(v, "test_value")
      None => assert_true(false)
    }
  }
  
  // Test empty key (should handle gracefully)
  Attributes::set(attrs, "", StringValue("empty_key_value"))
  let empty_key_retrieved = Attributes::get(attrs, "")
  match empty_key_retrieved {
    Some(StringValue(v)) => assert_eq(v, "empty_key_value")
    None => assert_true(true) // Acceptable behavior
  }
}

// Test 4: Attribute Value Validation
test "attribute value validation" {
  let attrs = Attributes::new()
  
  // String value validation
  let valid_strings = [
    "normal_string",
    "", // Empty string
    "string with spaces",
    "string-with-dashes",
    "string_with_underscores",
    "string.with.dots",
    "string/with/slashes",
    "string\\with\\backslashes",
    "string@with@symbols",
    "string#with#hashes",
    "string$with$dollars",
    "string%with%percents",
    "string^with^carets",
    "string&with&ampersands",
    "string*with*asterisks",
    "string(with)parentheses",
    "string[with]brackets",
    "string{with}braces",
    "string|with|pipes",
    "string+with+plus",
    "string=with=equals",
    "string?with?questions",
    "string<with>angles",
    "string\"with\"quotes",
    "string'with'apostrophes",
    "string\nwith\nnewlines",
    "string\twith\ttabs",
    "string\rwith\rcarriage",
    "测试中文",
    "🚀emoji",
    "mix🚀of🌟different✨unicode"
  ]
  
  for str_val in valid_strings {
    Attributes::set(attrs, "string_test", StringValue(str_val))
    let retrieved = Attributes::get(attrs, "string_test")
    match retrieved {
      Some(StringValue(v)) => assert_eq(v, str_val)
      None => assert_true(false)
    }
  }
  
  // Integer value validation
  let valid_ints = [
    0, 1, -1, 42, -42,
    2147483647, // Max 32-bit int
    -2147483648, // Min 32-bit int
    9223372036854775807, // Max 64-bit int
    -9223372036854775808  // Min 64-bit int
  ]
  
  for int_val in valid_ints {
    Attributes::set(attrs, "int_test", IntValue(int_val))
    let retrieved = Attributes::get(attrs, "int_test")
    match retrieved {
      Some(IntValue(v)) => assert_eq(v, int_val)
      None => assert_true(false)
    }
  }
  
  // Float value validation
  let valid_floats = [
    0.0, 1.0, -1.0, 3.14159, -3.14159,
    1.7976931348623157e+308, // Max double
    -1.7976931348623157e+308, // Min double
    2.2250738585072014e-308, // Min positive double
    -2.2250738585072014e-308, // Max negative double
    0.0/0.0, // NaN (if supported)
    1.0/0.0, // Infinity (if supported)
    -1.0/0.0  // Negative infinity (if supported)
  ]
  
  for float_val in valid_floats {
    Attributes::set(attrs, "float_test", FloatValue(float_val))
    let retrieved = Attributes::get(attrs, "float_test")
    match retrieved {
      Some(FloatValue(v)) => {
        // Handle special cases
        if float_val.is_nan() && v.is_nan() {
          assert_true(true) // Both NaN
        } else if float_val.is_infinite() && v.is_infinite() {
          assert_true(true) // Both infinite
        } else {
          assert_true((v - float_val).abs() < 0.0001)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Boolean value validation
  let bool_values = [true, false]
  
  for bool_val in bool_values {
    Attributes::set(attrs, "bool_test", BoolValue(bool_val))
    let retrieved = Attributes::get(attrs, "bool_test")
    match retrieved {
      Some(BoolValue(v)) => assert_eq(v, bool_val)
      None => assert_true(false)
    }
  }
}

// Test 5: Array Attribute Validation
test "array attribute validation" {
  let attrs = Attributes::new()
  
  // String array validation
  let string_arrays = [
    [], // Empty array
    ["single"],
    ["first", "second"],
    ["item1", "item2", "item3", "item4", "item5"],
    ["", " ", "\t", "\n"], // Special strings
    ["测试", "🚀", "emoji"], // Unicode
    ["very long string that exceeds normal length limits and contains various characters and symbols"],
    ["duplicate", "duplicate", "unique"]
  ]
  
  for (i, arr) in string_arrays.enumerate() {
    Attributes::set(attrs, "string_array_" + i.to_string(), ArrayStringValue(arr))
    let retrieved = Attributes::get(attrs, "string_array_" + i.to_string())
    match retrieved {
      Some(ArrayStringValue(v)) => {
        assert_eq(v.length(), arr.length())
        for (j, item) in arr.enumerate() {
          assert_eq(v[j], item)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Int array validation
  let int_arrays = [
    [], // Empty array
    [42],
    [1, 2, 3, 4, 5],
    [0, -1, 2147483647, -2147483648],
    [9223372036854775807, -9223372036854775808]
  ]
  
  for (i, arr) in int_arrays.enumerate() {
    Attributes::set(attrs, "int_array_" + i.to_string(), ArrayIntValue(arr))
    let retrieved = Attributes::get(attrs, "int_array_" + i.to_string())
    match retrieved {
      Some(ArrayIntValue(v)) => {
        assert_eq(v.length(), arr.length())
        for (j, item) in arr.enumerate() {
          assert_eq(v[j], item)
        }
      }
      None => assert_true(false)
    }
  }
}

// Test 6: HTTP URL Validation
test "http URL validation" {
  let client = HttpClient::new()
  
  // Valid URLs
  let valid_urls = [
    "http://example.com",
    "https://example.com",
    "http://example.com/path",
    "https://example.com/path/to/resource",
    "http://example.com:8080",
    "https://example.com:443",
    "http://192.168.1.1",
    "https://[2001:db8::1]", // IPv6
    "http://user:pass@example.com",
    "http://example.com?query=value",
    "http://example.com#fragment",
    "http://example.com/path?query=value#fragment",
    "https://subdomain.example.com/path/to/resource?param1=value1&param2=value2#section"
  ]
  
  for url in valid_urls {
    let headers = []
    let request = HttpRequest::new("GET", url, headers, None)
    assert_eq(HttpRequest::url(request), url)
  }
  
  // Invalid URLs (should handle gracefully)
  let invalid_urls = [
    "", // Empty
    "not-a-url",
    "ftp://example.com", // Non-HTTP protocol
    "http://", // Incomplete
    "://example.com", // Missing protocol
    "http:///example.com", // Triple slash
    "http://example .com", // Space in domain
    "http://example.com:99999", // Invalid port
    "http://[invalid-ipv6]" // Invalid IPv6
  ]
  
  for url in invalid_urls {
    let headers = []
    let request = HttpRequest::new("GET", url, headers, None)
    // Should not crash, just handle gracefully
    assert_eq(HttpRequest::url(request), url)
  }
}

// Test 7: HTTP Header Validation
test "http header validation" {
  let client = HttpClient::new()
  
  // Valid headers
  let valid_headers = [
    [("Content-Type", "application/json")],
    [("Authorization", "Bearer token123")],
    [("User-Agent", "Mozilla/5.0")],
    [("Accept", "application/json, text/plain")],
    [("X-Custom-Header", "custom-value")],
    [("Content-Length", "1024")],
    [("Cache-Control", "no-cache")],
    [("Content-Type", "application/json"), ("Authorization", "Bearer token123")],
    [("X-Header1", "value1"), ("X-Header2", "value2"), ("X-Header3", "value3")],
    [("Header-With-Dashes", "value-with-dashes")],
    [("Header_With_Underscores", "value_with_underscores")],
    [("Header.With.Dots", "value.with.dots")]
  ]
  
  for headers in valid_headers {
    let request = HttpRequest::new("GET", "http://example.com", headers, None)
    assert_eq(HttpRequest::http_method(request), "GET")
  }
  
  // Headers with special values
  let special_value_headers = [
    [("Special-Header", "")], // Empty value
    [("Special-Header", " ")], // Space
    [("Special-Header", "value with spaces")],
    [("Special-Header", "value\nwith\nnewlines")],
    [("Special-Header", "value\twith\ttabs")],
    [("Special-Header", "value\"with\"quotes")],
    [("Special-Header", "value'with'apostrophes")],
    [("Special-Header", "value:with:colons")],
    [("Special-Header", "value;with;semicolons")],
    [("Special-Header", "测试中文")],
    [("Special-Header", "🚀emoji")]
  ]
  
  for headers in special_value_headers {
    let request = HttpRequest::new("GET", "http://example.com", headers, None)
    assert_eq(HttpRequest::http_method(request), "GET")
  }
}

// Test 8: Log Record Validation
test "log record validation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "validation_logger")
  
  // Valid severity levels
  let valid_severities = [Trace, Debug, Info, Warn, Error, Fatal]
  
  for severity in valid_severities {
    let log_record = LogRecord::new(severity, "Test message")
    assert_eq(LogRecord::severity_number(log_record), severity)
    Logger::emit(logger, log_record)
  }
  
  // Valid log messages
  let valid_messages = [
    "Normal message",
    "", // Empty message
    " ", // Space
    "Message with spaces",
    "Message\nwith\nnewlines",
    "Message\twith\ttabs",
    "Message\"with\"quotes",
    "Message'with'apostrophes",
    "Message:with:colons",
    "Message;with;semicolons",
    "Message@with@symbols",
    "Message#with#hashes",
    "Message$with$dollars",
    "Message%with%percents",
    "Message^with^carets",
    "Message&with&ampersands",
    "Message*with*asterisks",
    "Message(with)parentheses",
    "Message[with]brackets",
    "Message{with}braces",
    "Message|with|pipes",
    "Message+with+plus",
    "Message=with=equals",
    "Message?with?questions",
    "Message<with>angles",
    "Message/with/slashes",
    "Message\\with\\backslashes",
    "测试中文",
    "🚀emoji",
    "Very long message that exceeds normal length limits and contains various characters and symbols to test the system's ability to handle large log messages without issues or crashes"
  ]
  
  for message in valid_messages {
    let log_record = LogRecord::new(Info, message)
    match LogRecord::body(log_record) {
      Some(body) => assert_eq(body, message)
      None => assert_true(false)
    }
    Logger::emit(logger, log_record)
  }
  
  // Valid timestamps
  let valid_timestamps = [
    0L, // Unix epoch
    1234567890L, // Normal timestamp
    -1234567890L, // Negative timestamp
    9223372036854775807L, // Max 64-bit
    -9223372036854775808L  // Min 64-bit
  ]
  
  for timestamp in valid_timestamps {
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Timestamp test"),
      None,
      Some(timestamp),
      Some(timestamp + 1L),
      None,
      None,
      None
    )
    Logger::emit(logger, log_record)
  }
}

// Test 9: Resource Attribute Validation
test "resource attribute validation" {
  let resource = Resource::new()
  
  // Standard resource attributes
  let standard_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("localhost")),
    ("host.arch", StringValue("x86_64")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.4.0")),
    ("process.pid", IntValue(1234)),
    ("process.executable.name", StringValue("test-app")),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("0.1.0")),
    ("telemetry.sdk.language", StringValue("moonbit"))
  ]
  
  let resource_with_standard = Resource::with_attributes(resource, standard_attrs)
  
  for (key, expected_value) in standard_attrs {
    let retrieved_value = Resource::get_attribute(resource_with_standard, key)
    match retrieved_value {
      Some(value) => {
        match (value, expected_value) {
          (StringValue(v), StringValue(expected)) => assert_eq(v, expected)
          (IntValue(v), IntValue(expected)) => assert_eq(v, expected)
          (FloatValue(v), FloatValue(expected)) => assert_true((v - expected).abs() < 0.0001)
          (BoolValue(v), BoolValue(expected)) => assert_eq(v, expected)
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Custom resource attributes
  let custom_attrs = [
    ("custom.string", StringValue("custom-value")),
    ("custom.int", IntValue(42)),
    ("custom.float", FloatValue(3.14)),
    ("custom.bool", BoolValue(true)),
    ("custom.empty", StringValue("")),
    ("custom.zero", IntValue(0)),
    ("custom.false", BoolValue(false)),
    ("custom.unicode", StringValue("测试🚀"))
  ]
  
  let resource_with_custom = Resource::with_attributes(resource, custom_attrs)
  
  for (key, expected_value) in custom_attrs {
    let retrieved_value = Resource::get_attribute(resource_with_custom, key)
    match retrieved_value {
      Some(value) => {
        match (value, expected_value) {
          (StringValue(v), StringValue(expected)) => assert_eq(v, expected)
          (IntValue(v), IntValue(expected)) => assert_eq(v, expected)
          (FloatValue(v), FloatValue(expected)) => assert_true((v - expected).abs() < 0.0001)
          (BoolValue(v), BoolValue(expected)) => assert_eq(v, expected)
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
}

// Test 10: Metrics Validation
test "metrics validation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "validation_meter")
  
  // Valid instrument names
  let valid_names = [
    "counter",
    "test_counter",
    "test-counter",
    "test.counter",
    "test_counter_with_underscores",
    "test-counter-with-dashes",
    "test.counter.with.dots",
    "counter123",
    "123counter", // Though not recommended
    "a", // Single character
    "very_long_instrument_name_that_exceeds_normal_length_but_should_still_be_valid"
  ]
  
  for name in valid_names {
    let counter = Meter::create_counter(meter, name, None, None)
    let histogram = Meter::create_histogram(meter, name, None, None)
    let updown_counter = Meter::create_updown_counter(meter, name, None, None)
    let gauge = Meter::create_gauge(meter, name, None, None)
    
    // Test instrument operations
    Counter::add(counter, 1.0)
    Histogram::record(histogram, 100.0)
    UpDownCounter::add(updown_counter, 1.0)
    Gauge::record(gauge, 42.0)
  }
  
  // Valid instrument descriptions
  let valid_descriptions = [
    "", // Empty
    "Simple description",
    "Description with spaces",
    "Description-with-dashes",
    "Description_with_underscores",
    "Description.with.dots",
    "Description with various symbols: !@#$%^&*()",
    "Description\nwith\nnewlines",
    "Description\twith\ttabs",
    "Description\"with\"quotes",
    "Description'with'apostrophes",
    "测试中文描述",
    "🚀Description with emoji",
    "Very long description that exceeds normal length limits and contains various characters and symbols to test the system's ability to handle large descriptions without issues or crashes"
  ]
  
  for description in valid_descriptions {
    let counter = Meter::create_counter(meter, "desc_test", Some(description), None)
    Counter::add(counter, 1.0)
  }
  
  // Valid instrument units
  let valid_units = [
    "", // Empty
    "s", // seconds
    "ms", // milliseconds
    "us", // microseconds
    "ns", // nanoseconds
    "bytes",
    "By", // bytes (alternative)
    "count",
    "items",
    "%", // percentage
    "°C", // degrees Celsius
    "°F", // degrees Fahrenheit
    "m", // meters
    "km", // kilometers
    "g", // grams
    "kg", // kilograms
    "MB", // megabytes
    "GB", // gigabytes
    "Hz", // hertz
    "kHz", // kilohertz
    "MHz", // megahertz
    "custom-unit"
  ]
  
  for unit in valid_units {
    let counter = Meter::create_counter(meter, "unit_test", None, Some(unit))
    Counter::add(counter, 1.0)
  }
  
  // Valid metric values
  let valid_values = [
    0.0, 1.0, -1.0, 3.14159, -3.14159,
    1.7976931348623157e+308, // Max double
    -1.7976931348623157e+308, // Min double
    2.2250738585072014e-308, // Min positive double
    -2.2250738585072014e-308, // Max negative double
    0.0/0.0, // NaN (if supported)
    1.0/0.0, // Infinity (if supported)
    -1.0/0.0  // Negative infinity (if supported)
  ]
  
  let counter = Meter::create_counter(meter, "value_test", None, None)
  let histogram = Meter::create_histogram(meter, "value_test", None, None)
  let updown_counter = Meter::create_updown_counter(meter, "value_test", None, None)
  let gauge = Meter::create_gauge(meter, "value_test", None, None)
  
  for value in valid_values {
    Counter::add(counter, value)
    Histogram::record(histogram, value)
    UpDownCounter::add(updown_counter, value)
    Gauge::record(gauge, value)
  }
}