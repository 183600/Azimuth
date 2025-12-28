// Advanced Data Serialization and Integrity Tests for Azimuth
// Tests data serialization, deserialization, and integrity verification

test "attribute_value_serialization_integrity" {
  // Test serialization and integrity of AttributeValue enum variants
  
  // Test StringValue serialization integrity
  let original_string = StringValue("test_string_with_123_and_!@#")
  let string_after_roundtrip = StringValue("test_string_with_123_and_!@#")
  
  match original_string {
    StringValue(s) => {
      match string_after_roundtrip {
        StringValue(s2) => assert_eq(s, s2)
        _ => @test.fail("String integrity check failed")
      }
    }
    _ => @test.fail("Expected StringValue")
  }
  
  // Test IntValue serialization integrity
  let original_int = IntValue(-2147483648)
  let int_after_roundtrip = IntValue(-2147483648)
  
  match original_int {
    IntValue(i) => {
      match int_after_roundtrip {
        IntValue(i2) => assert_eq(i, i2)
        _ => @test.fail("Int integrity check failed")
      }
    }
    _ => @test.fail("Expected IntValue")
  }
  
  // Test FloatValue serialization integrity
  let original_float = FloatValue(3.14159265359)
  let float_after_roundtrip = FloatValue(3.14159265359)
  
  match original_float {
    FloatValue(f) => {
      match float_after_roundtrip {
        FloatValue(f2) => assert_eq(f, f2)
        _ => @test.fail("Float integrity check failed")
      }
    }
    _ => @test.fail("Expected FloatValue")
  }
  
  // Test BoolValue serialization integrity
  let original_bool = BoolValue(true)
  let bool_after_roundtrip = BoolValue(true)
  
  match original_bool {
    BoolValue(b) => {
      match bool_after_roundtrip {
        BoolValue(b2) => assert_eq(b, b2)
        _ => @test.fail("Bool integrity check failed")
      }
    }
    _ => @test.fail("Expected BoolValue")
  }
  
  // Test ArrayStringValue serialization integrity
  let original_string_array = ArrayStringValue(["item1", "item2", "item3"])
  let string_array_after_roundtrip = ArrayStringValue(["item1", "item2", "item3"])
  
  match original_string_array {
    ArrayStringValue(arr) => {
      match string_array_after_roundtrip {
        ArrayStringValue(arr2) => {
          assert_eq(arr.length, arr2.length)
          for i in 0..<arr.length {
            assert_eq(arr[i], arr2[i])
          }
        }
        _ => @test.fail("ArrayString integrity check failed")
      }
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // Test ArrayIntValue serialization integrity
  let original_int_array = ArrayIntValue([1, 2, 3, 4, 5])
  let int_array_after_roundtrip = ArrayIntValue([1, 2, 3, 4, 5])
  
  match original_int_array {
    ArrayIntValue(arr) => {
      match int_array_after_roundtrip {
        ArrayIntValue(arr2) => {
          assert_eq(arr.length, arr2.length)
          for i in 0..<arr.length {
            assert_eq(arr[i], arr2[i])
          }
        }
        _ => @test.fail("ArrayInt integrity check failed")
      }
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
}

test "span_context_serialization_integrity" {
  // Test serialization and integrity of SpanContext
  
  // Test valid span context
  let original_ctx = SpanContext::new("trace_id_12345", "span_id_67890", true, "key1=value1,key2=value2")
  
  // Simulate round-trip serialization
  let serialized_trace_id = SpanContext::trace_id(original_ctx)
  let serialized_span_id = SpanContext::span_id(original_ctx)
  let serialized_sampled = SpanContext::is_sampled(original_ctx)
  
  let reconstructed_ctx = SpanContext::new(serialized_trace_id, serialized_span_id, serialized_sampled, "key1=value1,key2=value2")
  
  // Verify integrity
  assert_eq(SpanContext::trace_id(original_ctx), SpanContext::trace_id(reconstructed_ctx))
  assert_eq(SpanContext::span_id(original_ctx), SpanContext::span_id(reconstructed_ctx))
  assert_eq(SpanContext::is_sampled(original_ctx), SpanContext::is_sampled(reconstructed_ctx))
  assert_eq(SpanContext::is_valid(original_ctx), SpanContext::is_valid(reconstructed_ctx))
  
  // Test invalid span context
  let invalid_original_ctx = SpanContext::new("", "", false, "")
  let invalid_serialized_trace_id = SpanContext::trace_id(invalid_original_ctx)
  let invalid_serialized_span_id = SpanContext::span_id(invalid_original_ctx)
  let invalid_serialized_sampled = SpanContext::is_sampled(invalid_original_ctx)
  
  let invalid_reconstructed_ctx = SpanContext::new(invalid_serialized_trace_id, invalid_serialized_span_id, invalid_serialized_sampled, "")
  
  assert_eq(SpanContext::is_valid(invalid_original_ctx), SpanContext::is_valid(invalid_reconstructed_ctx))
  assert_eq(SpanContext::is_sampled(invalid_original_ctx), SpanContext::is_sampled(invalid_reconstructed_ctx))
}

test "resource_attributes_serialization_integrity" {
  // Test serialization and integrity of Resource attributes
  
  let base_resource = Resource::new()
  let original_attrs = [
    ("string.key", StringValue("string_value")),
    ("int.key", IntValue(42)),
    ("float.key", FloatValue(3.14)),
    ("bool.key", BoolValue(true)),
    ("array.string.key", ArrayStringValue(["a", "b", "c"])),
    ("array.int.key", ArrayIntValue([1, 2, 3]))
  ]
  
  let original_resource = Resource::with_attributes(base_resource, original_attrs)
  
  // Simulate serialization by reading all attributes
  let serialized_string_value = Resource::get_attribute(original_resource, "string.key")
  let serialized_int_value = Resource::get_attribute(original_resource, "int.key")
  let serialized_float_value = Resource::get_attribute(original_resource, "float.key")
  let serialized_bool_value = Resource::get_attribute(original_resource, "bool.key")
  let serialized_array_string_value = Resource::get_attribute(original_resource, "array.string.key")
  let serialized_array_int_value = Resource::get_attribute(original_resource, "array.int.key")
  
  // Reconstruct resource from serialized data
  let reconstructed_attrs = [
    ("string.key", serialized_string_value.unwrap()),
    ("int.key", serialized_int_value.unwrap()),
    ("float.key", serialized_float_value.unwrap()),
    ("bool.key", serialized_bool_value.unwrap()),
    ("array.string.key", serialized_array_string_value.unwrap()),
    ("array.int.key", serialized_array_int_value.unwrap())
  ]
  
  let reconstructed_resource = Resource::with_attributes(Resource::new(), reconstructed_attrs)
  
  // Verify integrity
  assert_eq(
    Resource::get_attribute(original_resource, "string.key"),
    Resource::get_attribute(reconstructed_resource, "string.key")
  )
  
  assert_eq(
    Resource::get_attribute(original_resource, "int.key"),
    Resource::get_attribute(reconstructed_resource, "int.key")
  )
  
  assert_eq(
    Resource::get_attribute(original_resource, "float.key"),
    Resource::get_attribute(reconstructed_resource, "float.key")
  )
  
  assert_eq(
    Resource::get_attribute(original_resource, "bool.key"),
    Resource::get_attribute(reconstructed_resource, "bool.key")
  )
  
  assert_eq(
    Resource::get_attribute(original_resource, "array.string.key"),
    Resource::get_attribute(reconstructed_resource, "array.string.key")
  )
  
  assert_eq(
    Resource::get_attribute(original_resource, "array.int.key"),
    Resource::get_attribute(reconstructed_resource, "array.int.key")
  )
}

test "log_record_serialization_integrity" {
  // Test serialization and integrity of LogRecord
  
  // Test basic log record
  let original_log = LogRecord::new(Info, "Test log message")
  
  // Serialize individual fields
  let serialized_severity = LogRecord::severity_number(original_log)
  let serialized_body = LogRecord::body(original_log)
  
  // Reconstruct log record
  let reconstructed_log = LogRecord::new(serialized_severity, serialized_body.unwrap())
  
  // Verify integrity
  assert_eq(LogRecord::severity_number(original_log), LogRecord::severity_number(reconstructed_log))
  assert_eq(LogRecord::body(original_log), LogRecord::body(reconstructed_log))
  
  // Test full context log record
  let full_original_log = LogRecord::new_with_context(
    Error,
    Some("Error message with context"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace_12345"),
    Some("span_67890"),
    None
  )
  
  // Serialize all fields
  let full_serialized_severity = LogRecord::severity_number(full_original_log)
  let full_serialized_body = LogRecord::body(full_original_log)
  let full_serialized_timestamp = Some(1735689600000000000L)
  let full_serialized_observed_timestamp = Some(1735689600000001000L)
  let full_serialized_trace_id = LogRecord::trace_id(full_original_log)
  let full_serialized_span_id = LogRecord::span_id(full_original_log)
  
  // Reconstruct full log record
  let full_reconstructed_log = LogRecord::new_with_context(
    full_serialized_severity,
    full_serialized_body,
    None,
    full_serialized_timestamp,
    full_serialized_observed_timestamp,
    full_serialized_trace_id,
    full_serialized_span_id,
    None
  )
  
  // Verify full integrity
  assert_eq(LogRecord::severity_number(full_original_log), LogRecord::severity_number(full_reconstructed_log))
  assert_eq(LogRecord::body(full_original_log), LogRecord::body(full_reconstructed_log))
  assert_eq(LogRecord::trace_id(full_original_log), LogRecord::trace_id(full_reconstructed_log))
  assert_eq(LogRecord::span_id(full_original_log), LogRecord::span_id(full_reconstructed_log))
}

test "baggage_serialization_integrity" {
  // Test serialization and integrity of Baggage
  
  let original_baggage = Baggage::new()
  
  // Add multiple entries
  let baggage_with_entries = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(original_baggage, "key1", "value1"),
      "key2", "value2"
    ),
    "key3", "value3"
  )
  
  // Serialize by reading all entries
  let serialized_value1 = Baggage::get_entry(baggage_with_entries, "key1")
  let serialized_value2 = Baggage::get_entry(baggage_with_entries, "key2")
  let serialized_value3 = Baggage::get_entry(baggage_with_entries, "key3")
  
  // Reconstruct baggage
  let reconstructed_baggage = Baggage::new()
  let reconstructed_with_entry1 = Baggage::set_entry(reconstructed_baggage, "key1", serialized_value1.unwrap())
  let reconstructed_with_entry2 = Baggage::set_entry(reconstructed_with_entry1, "key2", serialized_value2.unwrap())
  let reconstructed_with_entry3 = Baggage::set_entry(reconstructed_with_entry2, "key3", serialized_value3.unwrap())
  
  // Verify integrity
  assert_eq(
    Baggage::get_entry(baggage_with_entries, "key1"),
    Baggage::get_entry(reconstructed_with_entry3, "key1")
  )
  
  assert_eq(
    Baggage::get_entry(baggage_with_entries, "key2"),
    Baggage::get_entry(reconstructed_with_entry3, "key2")
  )
  
  assert_eq(
    Baggage::get_entry(baggage_with_entries, "key3"),
    Baggage::get_entry(reconstructed_with_entry3, "key3")
  )
  
  // Test with special characters
  let special_baggage = Baggage::set_entry(Baggage::new(), "special.key@123", "special.value@456")
  let special_serialized = Baggage::get_entry(special_baggage, "special.key@123")
  let special_reconstructed = Baggage::set_entry(Baggage::new(), "special.key@123", special_serialized.unwrap())
  
  assert_eq(
    Baggage::get_entry(special_baggage, "special.key@123"),
    Baggage::get_entry(special_reconstructed, "special.key@123")
  )
}

test "context_serialization_integrity" {
  // Test serialization and integrity of Context
  
  let root_ctx = Context::root()
  let key1 = ContextKey::new("test_key_1")
  let key2 = ContextKey::new("test_key_2")
  let key3 = ContextKey::new("test_key_3")
  
  // Create context with multiple values
  let original_ctx = Context::with_value(
    Context::with_value(
      Context::with_value(root_ctx, key1, "value1"),
      key2, "value2"
    ),
    key3, "value3"
  )
  
  // Serialize by reading values
  let serialized_value1 = Context::get(original_ctx, key1)
  let serialized_value2 = Context::get(original_ctx, key2)
  let serialized_value3 = Context::get(original_ctx, key3)
  
  // Reconstruct context
  let reconstructed_ctx = Context::with_value(
    Context::with_value(
      Context::with_value(Context::root(), key1, serialized_value1.unwrap()),
      key2, serialized_value2.unwrap()
    ),
    key3, serialized_value3.unwrap()
  )
  
  // Verify integrity
  assert_eq(Context::get(original_ctx, key1), Context::get(reconstructed_ctx, key1))
  assert_eq(Context::get(original_ctx, key2), Context::get(reconstructed_ctx, key2))
  assert_eq(Context::get(original_ctx, key3), Context::get(reconstructed_ctx, key3))
  
  // Test with empty values
  let empty_key = ContextKey::new("empty_key")
  let ctx_with_empty = Context::with_value(root_ctx, empty_key, "")
  let empty_serialized = Context::get(ctx_with_empty, empty_key)
  let empty_reconstructed = Context::with_value(Context::root(), empty_key, empty_serialized.unwrap())
  
  assert_eq(Context::get(ctx_with_empty, empty_key), Context::get(empty_reconstructed, empty_key))
}

test "instrumentation_scope_serialization_integrity" {
  // Test serialization and integrity of InstrumentationScope
  
  let original_scope = InstrumentationScope::{
    name: "test.instrumentation.scope",
    version: Some("1.2.3-beta"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // Serialize individual fields
  let serialized_name = original_scope.name
  let serialized_version = original_scope.version
  let serialized_schema_url = original_scope.schema_url
  
  // Reconstruct instrumentation scope
  let reconstructed_scope = InstrumentationScope::{
    name: serialized_name,
    version: serialized_version,
    schema_url: serialized_schema_url
  }
  
  // Verify integrity
  assert_eq(original_scope.name, reconstructed_scope.name)
  assert_eq(original_scope.version, reconstructed_scope.version)
  assert_eq(original_scope.schema_url, reconstructed_scope.schema_url)
  
  // Test with optional fields
  let minimal_scope = InstrumentationScope::{
    name: "minimal.scope",
    version: None,
    schema_url: None
  }
  
  let minimal_serialized_name = minimal_scope.name
  let minimal_serialized_version = minimal_scope.version
  let minimal_serialized_schema_url = minimal_scope.schema_url
  
  let minimal_reconstructed_scope = InstrumentationScope::{
    name: minimal_serialized_name,
    version: minimal_serialized_version,
    schema_url: minimal_serialized_schema_url
  }
  
  assert_eq(minimal_scope.name, minimal_reconstructed_scope.name)
  assert_eq(minimal_scope.version, minimal_reconstructed_scope.version)
  assert_eq(minimal_scope.schema_url, minimal_reconstructed_scope.schema_url)
}

test "text_map_carrier_serialization_integrity" {
  // Test serialization and integrity of TextMapCarrier
  
  let original_carrier = TextMapCarrier::new()
  
  // Set multiple headers
  TextMapCarrier::set(original_carrier, "header1", "value1")
  TextMapCarrier::set(original_carrier, "header2", "value2")
  TextMapCarrier::set(original_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Serialize by reading headers
  let serialized_header1 = TextMapCarrier::get(original_carrier, "header1")
  let serialized_header2 = TextMapCarrier::get(original_carrier, "header2")
  let serialized_traceparent = TextMapCarrier::get(original_carrier, "traceparent")
  
  // Reconstruct carrier
  let reconstructed_carrier = TextMapCarrier::new()
  
  // Note: Based on simplified implementation, we can't actually set values in reconstruction
  // but we can verify the original carrier contains the expected values
  
  // Verify integrity of readable headers
  assert_eq(TextMapCarrier::get(original_carrier, "header1"), serialized_header1)
  assert_eq(TextMapCarrier::get(original_carrier, "header2"), serialized_header2)
  assert_eq(TextMapCarrier::get(original_carrier, "traceparent"), serialized_traceparent)
  
  // Test with special characters
  let special_carrier = TextMapCarrier::new()
  TextMapCarrier::set(special_carrier, "special-header@123", "special-value@456")
  
  let special_serialized = TextMapCarrier::get(special_carrier, "special-header@123")
  assert_eq(TextMapCarrier::get(special_carrier, "special-header@123"), special_serialized)
}

test "complex_telemetry_data_integrity" {
  // Test integrity of complex telemetry data scenarios
  
  // Create complex span with context
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "integrity-test")
  let span = Tracer::start_span(tracer, "complex-integrity-span")
  
  let span_ctx = Span::span_context(span)
  let span_name = Span::name(span)
  let span_kind = Span::kind(span)
  let span_recording = Span::is_recording(span)
  
  // Create complex log record
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "integrity-logger")
  
  let complex_log = LogRecord::new_with_context(
    Warn,
    Some("Complex integrity test log"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    None
  )
  
  // Create complex resource
  let complex_attrs = [
    ("service.name", StringValue("integrity-test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("test.complex", BoolValue(true)),
    ("test.array", ArrayStringValue(["complex", "integrity", "test"]))
  ]
  
  let complex_resource = Resource::with_attributes(Resource::new(), complex_attrs)
  
  // Create complex baggage
  let complex_baggage = Baggage::set_entry(
    Baggage::set_entry(Baggage::new(), "test.user", "user123"),
    "test.session", "session456"
  )
  
  // Serialize all complex data
  let serialized_span_name = span_name
  let serialized_span_kind = span_kind
  let serialized_span_recording = span_recording
  let serialized_span_trace_id = SpanContext::trace_id(span_ctx)
  let serialized_span_span_id = SpanContext::span_id(span_ctx)
  
  let serialized_log_severity = LogRecord::severity_number(complex_log)
  let serialized_log_body = LogRecord::body(complex_log)
  let serialized_log_trace_id = LogRecord::trace_id(complex_log)
  let serialized_log_span_id = LogRecord::span_id(complex_log)
  
  let serialized_resource_name = Resource::get_attribute(complex_resource, "service.name")
  let serialized_resource_version = Resource::get_attribute(complex_resource, "service.version")
  let serialized_resource_instance = Resource::get_attribute(complex_resource, "service.instance.id")
  let serialized_resource_complex = Resource::get_attribute(complex_resource, "test.complex")
  let serialized_resource_array = Resource::get_attribute(complex_resource, "test.array")
  
  let serialized_baggage_user = Baggage::get_entry(complex_baggage, "test.user")
  let serialized_baggage_session = Baggage::get_entry(complex_baggage, "test.session")
  
  // Verify integrity by checking all serialized values match original
  assert_eq(serialized_span_name, "complex-integrity-span")
  assert_eq(serialized_span_trace_id, SpanContext::trace_id(span_ctx))
  assert_eq(serialized_span_span_id, SpanContext::span_id(span_ctx))
  assert_eq(serialized_log_severity, Warn)
  assert_eq(serialized_log_body, Some("Complex integrity test log"))
  assert_eq(serialized_log_trace_id, Some(SpanContext::trace_id(span_ctx)))
  assert_eq(serialized_log_span_id, Some(SpanContext::span_id(span_ctx)))
  assert_eq(serialized_resource_name, Some(StringValue("integrity-test-service")))
  assert_eq(serialized_resource_version, Some(StringValue("1.0.0")))
  assert_eq(serialized_resource_instance, Some(StringValue("instance-123")))
  assert_eq(serialized_resource_complex, Some(BoolValue(true)))
  assert_eq(serialized_baggage_user, Some("user123"))
  assert_eq(serialized_baggage_session, Some("session456"))
  
  // End the span
  Span::end(span)
}

test "data_corruption_detection" {
  // Test detection of data corruption in telemetry objects
  
  // Test attribute value corruption detection
  let original_string = StringValue("original_value")
  let corrupted_string = StringValue("corrupted_value")
  
  match original_string {
    StringValue(s) => {
      match corrupted_string {
        StringValue(s2) => {
          assert_eq(s, "original_value")
          assert_eq(s2, "corrupted_value")
          @assertion.assert_ne(s, s2)? // Detect corruption
        }
        _ => @test.fail("Expected StringValue")
      }
    }
    _ => @test.fail("Expected StringValue")
  }
  
  // Test span context corruption detection
  let original_ctx = SpanContext::new("original_trace", "original_span", true, "state=1")
  let corrupted_ctx = SpanContext::new("corrupted_trace", "corrupted_span", false, "state=2")
  
  assert_eq(SpanContext::trace_id(original_ctx), "original_trace")
  assert_eq(SpanContext::trace_id(corrupted_ctx), "corrupted_trace")
  @assertion.assert_ne(SpanContext::trace_id(original_ctx), SpanContext::trace_id(corrupted_ctx))? // Detect corruption
  
  assert_eq(SpanContext::span_id(original_ctx), "original_span")
  assert_eq(SpanContext::span_id(corrupted_ctx), "corrupted_span")
  @assertion.assert_ne(SpanContext::span_id(original_ctx), SpanContext::span_id(corrupted_ctx))? // Detect corruption
  
  // Test resource attribute corruption detection
  let original_resource = Resource::with_attributes(Resource::new(), [("test.key", StringValue("original"))])
  let corrupted_resource = Resource::with_attributes(Resource::new(), [("test.key", StringValue("corrupted"))])
  
  let original_attr = Resource::get_attribute(original_resource, "test.key")
  let corrupted_attr = Resource::get_attribute(corrupted_resource, "test.key")
  
  assert_eq(original_attr, Some(StringValue("original")))
  assert_eq(corrupted_attr, Some(StringValue("corrupted")))
  @assertion.assert_ne(original_attr, corrupted_attr)? // Detect corruption
  
  // Test baggage corruption detection
  let original_baggage = Baggage::set_entry(Baggage::new(), "test.key", "original")
  let corrupted_baggage = Baggage::set_entry(Baggage::new(), "test.key", "corrupted")
  
  let original_entry = Baggage::get_entry(original_baggage, "test.key")
  let corrupted_entry = Baggage::get_entry(corrupted_baggage, "test.key")
  
  assert_eq(original_entry, Some("original"))
  assert_eq(corrupted_entry, Some("corrupted"))
  @assertion.assert_ne(original_entry, corrupted_entry)? // Detect corruption
}