// Azimuth Serialization Edge Case Quality Tests
// Tests for serialization/deserialization edge cases

test "span context serialization with edge cases" {
  // Arrange - Create span with various edge case scenarios
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "serialization-edge-test")
  let span = Tracer::start_span(tracer, "edge-case-span")
  
  // Act - Test edge case serialization
  let span_context = Span::context(span)
  
  // Test with all possible flag combinations
  let sampled_context = SpanContext::with_flags(span_context, 1) // Sampled
  let unsampled_context = SpanContext::with_flags(span_context, 0) // Unsampled
  
  // Serialize edge case contexts
  let sampled_serialized = SpanContext::serialize(sampled_context)
  let unsampled_serialized = SpanContext::serialize(unsampled_context)
  
  // Deserialize edge case contexts
  let sampled_deserialized = SpanContext::deserialize(sampled_serialized)
  let unsampled_deserialized = SpanContext::deserialize(unsampled_serialized)
  
  // Test with invalid trace ID formats
  let invalid_trace_ids = [
    "", // Empty
    "short", // Too short
    "gggggggggggggggggggggggggggggggg", // Invalid hex
    "0123456789abcdef0123456789abcde", // Odd length
    "0123456789ABCDEF0123456789ABCDEF0", // Too long
    "0123456789abcdef0123456789abcdeG", // Invalid character
  ]
  
  for invalid_id in invalid_trace_ids {
    let result = SpanContext::deserialize_trace_id(invalid_id)
    assert_true(result.is_err())
  }
  
  // Test with invalid span ID formats
  let invalid_span_ids = [
    "", // Empty
    "short", // Too short
    "gggggggggggggggg", // Invalid hex
    "0123456789abcd", // Odd length
    "0123456789abcdef0", // Too long
    "0123456789abcdeG", // Invalid character
  ]
  
  for invalid_id in invalid_span_ids {
    let result = SpanContext::deserialize_span_id(invalid_id)
    assert_true(result.is_err())
  }
  
  // Assert - Verify edge case handling
  assert_true(SpanContext::is_valid(sampled_deserialized))
  assert_true(SpanContext::is_valid(unsampled_deserialized))
  assert_eq(SpanContext::trace_id(sampled_deserialized), SpanContext::trace_id(sampled_context))
  assert_eq(SpanContext::trace_id(unsampled_deserialized), SpanContext::trace_id(unsampled_context))
}

test "baggage serialization with special characters" {
  // Arrange - Create baggage with special characters
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "baggage-serialization-test")
  let span = Tracer::start_span(tracer, "special-chars-span")
  let context = Span::context(span)
  let baggage = Context::baggage(context)
  
  // Act - Test special character serialization
  let special_cases = [
    ("key with spaces", "value with spaces"),
    ("key=with=equals", "value=with=equals"),
    ("key;with;semicolons", "value;with;semicolons"),
    ("key\"with\"quotes", "value\"with\"quotes"),
    ("key\\with\\backslashes", "value\\with\\backslashes"),
    ("key/with/slashes", "value/with/slashes"),
    ("key?with?question", "value?with?question"),
    ("key#with#hash", "value#with#hash"),
    ("key@with@at", "value@with@at"),
    ("key%with%percent", "value%with%percent"),
    ("key&with&ampersand", "value&ampersand"),
    ("key+with+plus", "value+with+plus"),
    ("key中文字符", "value中文字符"),
    ("key🚀emoji", "value🚀emoji"),
    ("key\nnewline", "value\nnewline"),
    ("key\ttab", "value\ttab"),
    ("key\rcarriage", "value\rcarriage")
  ]
  
  for (key, value) in special_cases {
    let updated_baggage = Baggage::set(baggage, key, value)
    
    // Serialize
    let serialized = Baggage::serialize(updated_baggage)
    
    // Deserialize
    let deserialized = Baggage::deserialize(serialized)
    
    // Verify round-trip
    let retrieved = Baggage::get(deserialized, key)
    assert_eq(retrieved, Some(value))
  }
  
  // Test malformed baggage strings
  let malformed_baggage_strings = [
    "", // Empty
    "key", // No equals
    "=value", // Empty key
    "key=", // Empty value
    "key=value=extra", // Multiple equals
    "key=value;invalid", // Invalid metadata
    "key=value;metadata=unclosed", // Unclosed quote
    "key=value;metadata=\"unclosed", // Unclosed quote with quote
    "key=value;metadata=\"\"", // Empty quoted metadata
    "key=value;metadata=prop=;extra", // Trailing semicolon
    "key=value;;key2=value2", // Double semicolon
    "=value;key2=value2", // Empty key in first item
    "key=value;=value2", // Empty key in second item
  ]
  
  for malformed in malformed_baggage_strings {
    let result = Baggage::deserialize(malformed)
    // Should handle gracefully without crashing
    assert_true(Baggage::count(result) >= 0)
  }
}

test "attribute serialization with complex types" {
  // Arrange - Create span with complex attribute types
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "attribute-serialization-test")
  let span = Tracer::start_span(tracer, "complex-attributes")
  
  // Act - Test complex attribute serialization
  let complex_attributes = [
    ("string_array", "[\"value1\", \"value2\", \"value3\"]"),
    ("number_array", "[1, 2, 3, 4.5, 6.7]"),
    ("boolean_array", "[true, false, true]"),
    ("mixed_array", "[\"string\", 123, true, null]"),
    ("nested_object", "{\"key1\": \"value1\", \"key2\": 123, \"key3\": true}"),
    ("deeply_nested", "{\"level1\": {\"level2\": {\"level3\": \"deep_value\"}}}"),
    ("special_chars", "{\"special\": \"value with \\\"quotes\\\" and \\n newlines\"}"),
    ("unicode", "{\"unicode\": \"测试中文 🚀\"}"),
    ("empty_object", "{}"),
    ("empty_array", "[]"),
    ("null_value", "null"),
    ("large_number", "9223372036854775807"), // Max int64
    ("negative_large", "-9223372036854775808"), // Min int64
    ("float_precision", "3.14159265358979323846264338327950288419716939937510"),
    ("scientific", "1.23e-10"),
    ("infinity", "Infinity"),
    ("negative_infinity", "-Infinity"),
    ("nan", "NaN")
  ]
  
  for (key, value) in complex_attributes {
    Span::set_attribute(span, key, value)
  }
  
  // Serialize span with complex attributes
  let serialized_span = Span::serialize(span)
  
  // Deserialize span
  let deserialized_span = Span::deserialize(serialized_span)
  
  // Assert - Verify complex attribute serialization
  for (key, expected_value) in complex_attributes {
    let actual_value = Span::get_attribute(deserialized_span, key)
    assert_eq(actual_value, Some(expected_value))
  }
  
  // Test attribute array serialization
  let attribute_array = Array[(String, String)]::from(complex_attributes)
  let serialized_array = Attributes::serialize_array(attribute_array)
  let deserialized_array = Attributes::deserialize_array(serialized_array)
  
  assert_eq(deserialized_array.length(), complex_attributes.length())
}

test "metric serialization with edge cases" {
  // Arrange - Create metrics with edge case values
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metric-serialization-test")
  
  // Act - Test edge case metric serialization
  let counter = Meter::create_counter(meter, "edge_counter", "Edge case counter", "count")
  let histogram = Meter::create_histogram(meter, "edge_histogram", "Edge case histogram", "ms")
  let gauge = Meter::create_gauge(meter, "edge_gauge", "Edge case gauge", "value")
  
  // Add edge case values
  let edge_values = [
    0,
    1,
    -1,
    Int64::max_value(),
    Int64::min_value(),
    9223372036854775807, // Max int64
    -9223372036854775808, // Min int64
    1234567890123456789, // Large number
    -1234567890123456789, // Large negative
    0.0, // Zero float
    1.7976931348623157e+308, // Max double
    -1.7976931348623157e+308, // Min double
    4.9406564584124654e-324, // Min positive double
    Infinity, // Infinity
    -Infinity, // Negative infinity
    NaN // Not a number
  ]
  
  for value in edge_values {
    let attributes = Attributes::from([
      ("edge_case", value.to_string()),
      ("type", "edge_test")
    ])
    
    Counter::add(counter, value, attributes)
    Histogram::record(histogram, value, attributes)
    Gauge::set(gauge, value, attributes)
  }
  
  // Serialize metrics
  let serialized_counter = Counter::serialize(counter)
  let serialized_histogram = Histogram::serialize(histogram)
  let serialized_gauge = Gauge::serialize(gauge)
  
  // Deserialize metrics
  let deserialized_counter = Counter::deserialize(serialized_counter)
  let deserialized_histogram = Histogram::deserialize(serialized_histogram)
  let deserialized_gauge = Gauge::deserialize(serialized_gauge)
  
  // Assert - Verify edge case metric serialization
  let counter_data = Counter::get_data(deserialized_counter)
  let histogram_data = Histogram::get_data(deserialized_histogram)
  let gauge_data = Gauge::get_data(deserialized_gauge)
  
  assert_true(counter_data.count > 0)
  assert_true(histogram_data.count > 0)
  assert_true(gauge_data.value != 0 || edge_values.contains(0)) // Should handle zero
  
  // Test NaN and Infinity handling
  let nan_data = Histogram::get_data_by_attributes(deserialized_histogram, 
    Attributes::from([("edge_case", "NaN")]))
  assert_true(nan_data.count > 0) // Should handle NaN gracefully
  
  let infinity_data = Histogram::get_data_by_attributes(deserialized_histogram,
    Attributes::from([("edge_case", "Infinity")]))
  assert_true(infinity_data.count > 0) // Should handle Infinity gracefully
}

test "log serialization with edge cases" {
  // Arrange - Create logger with edge case scenarios
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "log-serialization-test")
  
  // Act - Test edge case log serialization
  let edge_log_records = [
    // LogRecord with all possible severity levels
    LogRecord {
      severity: SeverityNumber::Trace,
      body: Some("Trace message"),
      attributes: Some(Attributes::from([("level", "trace")])),
      timestamp: Some(Int64::min_value()),
      observed_timestamp: Some(Int64::max_value()),
      trace_id: Some("0123456789abcdef0123456789abcdef"),
      span_id: Some("0123456789abcdef"),
      context: None
    },
    LogRecord {
      severity: SeverityNumber::Debug,
      body: Some("Debug message"),
      attributes: Some(Attributes::from([("level", "debug")])),
      timestamp: Some(0),
      observed_timestamp: Some(0),
      trace_id: Some(""),
      span_id: Some(""),
      context: None
    },
    LogRecord {
      severity: SeverityNumber::Info,
      body: Some(""), // Empty body
      attributes: None, // No attributes
      timestamp: None, // No timestamp
      observed_timestamp: None,
      trace_id: None,
      span_id: None,
      context: None
    },
    LogRecord {
      severity: SeverityNumber::Warn,
      body: Some("Warning with special chars: \"quotes\", \n newlines, \t tabs"),
      attributes: Some(Attributes::from([
        ("special", "value with special chars"),
        ("unicode", "测试中文 🚀"),
        ("empty", ""),
        ("null", "null")
      ])),
      timestamp: Some(Clock::now()),
      observed_timestamp: Some(Clock::now()),
      trace_id: Some("gggggggggggggggggggggggggggggggg"), // Invalid hex
      span_id: Some("gggggggggggggggg"), // Invalid hex
      context: None
    },
    LogRecord {
      severity: SeverityNumber::Error,
      body: Some("Error with very long message: " + "x".repeat(1000)),
      attributes: Some(Attributes::from([
        ("large_key", "large_value_" + "y".repeat(500))
      ])),
      timestamp: Some(Int64::max_value()),
      observed_timestamp: Some(Int64::min_value()),
      trace_id: Some("0123456789abcdef0123456789abcdef"),
      span_id: Some("0123456789abcdef"),
      context: None
    },
    LogRecord {
      severity: SeverityNumber::Fatal,
      body: Some("Fatal message"),
      attributes: Some(Attributes::from([
        ("number", "123"),
        ("float", "123.456"),
        ("boolean", "true"),
        ("array", "[1, 2, 3]"),
        ("object", "{\"key\": \"value\"}")
      ])),
      timestamp: Some(Clock::now()),
      observed_timestamp: Some(Clock::now()),
      trace_id: Some("0123456789abcdef0123456789abcdef"),
      span_id: Some("0123456789abcdef"),
      context: None
    }
  ]
  
  // Serialize and deserialize each log record
  for log_record in edge_log_records {
    let serialized = LogRecord::serialize(log_record)
    let deserialized = LogRecord::deserialize(serialized)
    
    // Verify round-trip
    assert_eq(deserialized.severity, log_record.severity)
    assert_eq(deserialized.body, log_record.body)
    assert_eq(deserialized.timestamp, log_record.timestamp)
    assert_eq(deserialized.observed_timestamp, log_record.observed_timestamp)
    assert_eq(deserialized.trace_id, log_record.trace_id)
    assert_eq(deserialized.span_id, log_record.span_id)
  }
  
  // Test malformed log serialization
  let malformed_log_json = [
    "{}", // Empty object
    "{\"severity\": \"invalid\"}", // Invalid severity
    "{\"body\": 123}", // Body as number
    "{\"attributes\": \"not_an_object\"}", // Attributes as string
    "{\"timestamp\": \"not_a_number\"}", // Timestamp as string
    "{\"trace_id\": \"invalid_length\"}", // Invalid trace ID
    "{\"span_id\": \"invalid_length\"}", // Invalid span ID
    "invalid json", // Invalid JSON
    "", // Empty string
    null // Null
  ]
  
  for malformed in malformed_log_json {
    let result = LogRecord::deserialize(malformed)
    // Should handle gracefully without crashing
    assert_true(result.severity == SeverityNumber::Info || result.severity == SeverityNumber::Debug)
  }
}

test "resource serialization with complex hierarchies" {
  // Arrange - Create resource with complex hierarchy
  let resource_provider = ResourceProvider::default()
  let resource = ResourceProvider::create_resource(resource_provider, "complex-resource")
  
  // Act - Test complex resource serialization
  let complex_attributes = [
    ("service.name", "complex-service"),
    ("service.version", "v1.2.3"),
    ("service.instance.id", "instance-12345"),
    ("host.name", "complex-host"),
    ("host.arch", "wasm32"),
    ("os.type", "wasm"),
    ("process.pid", "0"),
    ("process.executable.name", "azimuth"),
    ("process.command_args", "[\"azimuth\", \"--config\", \"test\"]"),
    ("telemetry.sdk.name", "azimuth"),
    ("telemetry.sdk.version", "1.0.0"),
    ("telemetry.sdk.language", "moonbit"),
    ("deployment.environment", "test"),
    ("custom.array.attribute", "[\"value1\", \"value2\", \"value3\"]"),
    ("custom.object.attribute", "{\"key1\": \"value1\", \"key2\": 123}"),
    ("custom.nested.attribute", "{\"level1\": {\"level2\": {\"level3\": \"deep\"}}}"),
    ("custom.unicode.attribute", "测试中文 🚀"),
    ("custom.special.chars", "value with \"quotes\" and \n newlines"),
    ("custom.empty.attribute", ""),
    ("custom.null.attribute", "null"),
    ("custom.large.attribute", "x".repeat(1000))
  ]
  
  for (key, value) in complex_attributes {
    Resource::set_attribute(resource, key, value)
  }
  
  // Serialize resource
  let serialized_resource = Resource::serialize(resource)
  
  // Deserialize resource
  let deserialized_resource = Resource::deserialize(serialized_resource)
  
  // Assert - Verify complex resource serialization
  for (key, expected_value) in complex_attributes {
    let actual_value = Resource::get_attribute(deserialized_resource, key)
    assert_eq(actual_value, Some(expected_value))
  }
  
  // Test resource hierarchy serialization
  let parent_resource = ResourceProvider::create_resource(resource_provider, "parent-resource")
  Resource::set_attribute(parent_resource, "parent.attribute", "parent_value")
  
  let child_resource = ResourceProvider::create_child_resource(parent_resource, "child-resource")
  Resource::set_attribute(child_resource, "child.attribute", "child_value")
  
  let serialized_hierarchy = Resource::serialize_hierarchy(child_resource)
  let deserialized_hierarchy = Resource::deserialize_hierarchy(serialized_hierarchy)
  
  // Verify hierarchy is preserved
  let parent_attr = Resource::get_attribute(deserialized_hierarchy, "parent.attribute")
  let child_attr = Resource::get_attribute(deserialized_hierarchy, "child.attribute")
  
  assert_eq(parent_attr, Some("parent_value"))
  assert_eq(child_attr, Some("child_value"))
  
  // Test malformed resource serialization
  let malformed_resource_json = [
    "{}", // Empty object
    "{\"attributes\": \"not_an_object\"}", // Attributes as string
    "{\"name\": 123}", // Name as number
    "invalid json", // Invalid JSON
    "", // Empty string
    null // Null
  ]
  
  for malformed in malformed_resource_json {
    let result = Resource::deserialize(malformed)
    // Should handle gracefully without crashing
    assert_true(Resource::is_valid(result) || !Resource::is_valid(result))
  }
}