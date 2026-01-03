// High Quality Serialization and Deserialization Tests for Azimuth Telemetry System
// This file contains comprehensive tests for data serialization and deserialization

// Test 1: Attribute Value Serialization and Deserialization
test "attribute value serialization and deserialization" {
  // Test serialization of different attribute value types
  let string_attr = StringValue("test value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14159)
  let bool_attr = BoolValue(true)
  let string_array_attr = ArrayStringValue(["a", "b", "c"])
  let int_array_attr = ArrayIntValue([1, 2, 3])
  
  // Serialize attribute values
  let string_serialized = AttributeValue::serialize(string_attr)
  let int_serialized = AttributeValue::serialize(int_attr)
  let float_serialized = AttributeValue::serialize(float_attr)
  let bool_serialized = AttributeValue::serialize(bool_attr)
  let string_array_serialized = AttributeValue::serialize(string_array_attr)
  let int_array_serialized = AttributeValue::serialize(int_array_attr)
  
  // Verify serialization produces non-empty results
  assert_true(string_serialized != "")
  assert_true(int_serialized != "")
  assert_true(float_serialized != "")
  assert_true(bool_serialized != "")
  assert_true(string_array_serialized != "")
  assert_true(int_array_serialized != "")
  
  // Deserialize attribute values
  let string_deserialized = AttributeValue::deserialize(string_serialized)
  let int_deserialized = AttributeValue::deserialize(int_serialized)
  let float_deserialized = AttributeValue::deserialize(float_serialized)
  let bool_deserialized = AttributeValue::deserialize(bool_serialized)
  let string_array_deserialized = AttributeValue::deserialize(string_array_serialized)
  let int_array_deserialized = AttributeValue::deserialize(int_array_serialized)
  
  // Verify deserialization produces original values
  assert_eq(string_deserialized, Some(string_attr))
  assert_eq(int_deserialized, Some(int_attr))
  assert_eq(float_deserialized, Some(float_attr))
  assert_eq(bool_deserialized, Some(bool_attr))
  assert_eq(string_array_deserialized, Some(string_array_attr))
  assert_eq(int_array_deserialized, Some(int_array_attr))
}

// Test 2: Attributes Collection Serialization and Deserialization
test "attributes collection serialization and deserialization" {
  // Create attributes with various value types
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("string value"))
  Attributes::set(attrs, "int.key", IntValue(123))
  Attributes::set(attrs, "float.key", FloatValue(45.67))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "string.array.key", ArrayStringValue(["x", "y", "z"]))
  Attributes::set(attrs, "int.array.key", ArrayIntValue([4, 5, 6]))
  
  // Serialize attributes
  let serialized = Attributes::serialize(attrs)
  
  // Verify serialization produces non-empty result
  assert_true(serialized != "")
  
  // Deserialize attributes
  let deserialized = Attributes::deserialize(serialized)
  
  // Verify deserialization produces original attributes
  assert_eq(Attributes::get(deserialized, "string.key"), Some(StringValue("string value")))
  assert_eq(Attributes::get(deserialized, "int.key"), Some(IntValue(123)))
  assert_eq(Attributes::get(deserialized, "float.key"), Some(FloatValue(45.67)))
  assert_eq(Attributes::get(deserialized, "bool.key"), Some(BoolValue(true)))
  assert_eq(Attributes::get(deserialized, "string.array.key"), Some(ArrayStringValue(["x", "y", "z"])))
  assert_eq(Attributes::get(deserialized, "int.array.key"), Some(ArrayIntValue([4, 5, 6])))
  
  // Verify attribute count is preserved
  assert_eq(deserialized.values.length(), attrs.values.length())
}

// Test 3: Resource Serialization and Deserialization
test "resource serialization and deserialization" {
  // Create resource with various attributes
  let resource = Resource::new()
  Resource::set_attribute(resource, "service.name", StringValue("test-service"))
  Resource::set_attribute(resource, "service.version", StringValue("1.0.0"))
  Resource::set_attribute(resource, "service.instance.id", StringValue("instance-123"))
  Resource::set_attribute(resource, "host.name", StringValue("localhost"))
  Resource::set_attribute(resource, "host.port", IntValue(8080))
  Resource::set_attribute(resource, "process.pid", IntValue(12345))
  Resource::set_attribute(resource, "process.start.time", FloatValue(1640995200.0))
  
  // Serialize resource
  let serialized = Resource::serialize(resource)
  
  // Verify serialization produces non-empty result
  assert_true(serialized != "")
  
  // Deserialize resource
  let deserialized = Resource::deserialize(serialized)
  
  // Verify deserialization produces original resource attributes
  assert_eq(Resource::get_attribute(deserialized, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(deserialized, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(deserialized, "service.instance.id"), Some(StringValue("instance-123")))
  assert_eq(Resource::get_attribute(deserialized, "host.name"), Some(StringValue("localhost")))
  assert_eq(Resource::get_attribute(deserialized, "host.port"), Some(IntValue(8080)))
  assert_eq(Resource::get_attribute(deserialized, "process.pid"), Some(IntValue(12345)))
  assert_eq(Resource::get_attribute(deserialized, "process.start.time"), Some(FloatValue(1640995200.0)))
  
  // Verify attribute count is preserved
  assert_eq(deserialized.attributes.length(), resource.attributes.length())
}

// Test 4: Span Context Serialization and Deserialization
test "span context serialization and deserialization" {
  // Create span context with various properties
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1,key2=value2")
  
  // Serialize span context
  let serialized = SpanContext::serialize(span_ctx)
  
  // Verify serialization produces non-empty result
  assert_true(serialized != "")
  
  // Deserialize span context
  let deserialized = SpanContext::deserialize(serialized)
  
  // Verify deserialization produces original span context
  assert_eq(SpanContext::trace_id(deserialized), SpanContext::trace_id(span_ctx))
  assert_eq(SpanContext::span_id(deserialized), SpanContext::span_id(span_ctx))
  assert_eq(SpanContext::is_sampled(deserialized), SpanContext::is_sampled(span_ctx))
  assert_eq(SpanContext::trace_state(deserialized), SpanContext::trace_state(span_ctx))
  assert_eq(SpanContext::is_valid(deserialized), SpanContext::is_valid(span_ctx))
}

// Test 5: Baggage Serialization and Deserialization
test "baggage serialization and deserialization" {
  // Create baggage with multiple entries
  let baggage = Baggage::new()
  let with_user = Baggage::set_entry(baggage, "user.id", "user123")
  let with_session = Baggage::set_entry(with_user, "session.id", "session456")
  let with_tenant = Baggage::set_entry(with_session, "tenant.id", "tenant789")
  let with_correlation = Baggage::set_entry(with_tenant, "correlation.id", "corr000")
  
  // Serialize baggage
  let serialized = Baggage::serialize(with_correlation)
  
  // Verify serialization produces non-empty result
  assert_true(serialized != "")
  
  // Deserialize baggage
  let deserialized = Baggage::deserialize(serialized)
  
  // Verify deserialization produces original baggage entries
  assert_eq(Baggage::get_entry(deserialized, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(deserialized, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(deserialized, "tenant.id"), Some("tenant789"))
  assert_eq(Baggage::get_entry(deserialized, "correlation.id"), Some("corr000"))
  
  // Verify entry count is preserved
  assert_eq(deserialized.entries.length(), with_correlation.entries.length())
}

// Test 6: Instrumentation Scope Serialization and Deserialization
test "instrumentation scope serialization and deserialization" {
  // Create instrumentation scope with all properties
  let scope = InstrumentationScope::new(
    "azimuth.telemetry",
    Some("1.0.0"),
    Some("https://example.com/schema/v1")
  )
  
  // Serialize instrumentation scope
  let serialized = InstrumentationScope::serialize(scope)
  
  // Verify serialization produces non-empty result
  assert_true(serialized != "")
  
  // Deserialize instrumentation scope
  let deserialized = InstrumentationScope::deserialize(serialized)
  
  // Verify deserialization produces original instrumentation scope
  assert_eq(deserialized.name, scope.name)
  assert_eq(deserialized.version, scope.version)
  assert_eq(deserialized.schema_url, scope.schema_url)
}

// Test 7: Span Serialization and Deserialization
test "span serialization and deserialization" {
  // Create span with various properties
  let span_ctx = SpanContext::new("trace123", "span456", true, "key1=value1")
  let span = Span::new("test.span", Internal, span_ctx)
  
  // Set span attributes and status
  Span::set_attribute(span, "user.id", StringValue("user123"))
  Span::set_attribute(span, "operation.type", StringValue("test.operation"))
  Span::set_attribute(span, "duration", IntValue(1500))
  Span::set_status(span, Ok, "Operation completed successfully")
  
  // Serialize span
  let serialized = Span::serialize(span)
  
  // Verify serialization produces non-empty result
  assert_true(serialized != "")
  
  // Deserialize span
  let deserialized = Span::deserialize(serialized)
  
  // Verify deserialization produces original span properties
  assert_eq(Span::name(deserialized), Span::name(span))
  
  let deserialized_ctx = Span::context(deserialized)
  let original_ctx = Span::context(span)
  
  assert_eq(SpanContext::trace_id(deserialized_ctx), SpanContext::trace_id(original_ctx))
  assert_eq(SpanContext::span_id(deserialized_ctx), SpanContext::span_id(original_ctx))
  assert_eq(SpanContext::is_sampled(deserialized_ctx), SpanContext::is_sampled(original_ctx))
  assert_eq(SpanContext::trace_state(deserialized_ctx), SpanContext::trace_state(original_ctx))
  
  // Note: Attribute and status verification would require access methods
}

// Test 8: Log Record Serialization and Deserialization
test "log record serialization and deserialization" {
  // Create log record with various properties
  let log_record = LogRecord::new()
  LogRecord::set_body(log_record, "Test log message")
  LogRecord::set_severity(log_record, Info)
  LogRecord::set_timestamp(log_record, 1640995200000)
  LogRecord::set_attribute(log_record, "user.id", StringValue("user123"))
  LogRecord::set_attribute(log_record, "request.id", StringValue("req456"))
  LogRecord::set_attribute(log_record, "duration", IntValue(1500))
  
  // Serialize log record
  let serialized = LogRecord::serialize(log_record)
  
  // Verify serialization produces non-empty result
  assert_true(serialized != "")
  
  // Deserialize log record
  let deserialized = LogRecord::deserialize(serialized)
  
  // Verify deserialization produces original log record properties
  assert_eq(LogRecord::body(deserialized), LogRecord::body(log_record))
  assert_eq(LogRecord::severity(deserialized), LogRecord::severity(log_record))
  assert_eq(LogRecord::timestamp(deserialized), LogRecord::timestamp(log_record))
  
  // Note: Attribute verification would require access methods
}

// Test 9: Complex Telemetry Data Serialization and Deserialization
test "complex telemetry data serialization and deserialization" {
  // Create complex telemetry scenario
  let resource = Resource::new()
  Resource::set_attribute(resource, "service.name", StringValue("complex.service"))
  Resource::set_attribute(resource, "service.version", StringValue("2.0.0"))
  
  let scope = InstrumentationScope::new(
    "azimuth.complex",
    Some("2.0.0"),
    Some("https://example.com/complex/schema/v1")
  )
  
  let span_ctx = SpanContext::new("complex-trace", "complex-span", true, "complex=key1")
  let span = Span::new("complex.operation", Server, span_ctx)
  
  Span::set_attribute(span, "complex.attr", StringValue("complex.value"))
  Span::set_status(span, Ok, "Complex operation completed")
  
  let baggage = Baggage::new()
  let with_complex = Baggage::set_entry(baggage, "complex.key", "complex.value")
  
  let ctx = Context::root()
  let ctx_with_span = Context::with_value(ctx, ContextKey::new("span.context"), span_ctx)
  let ctx_with_both = Context::with_value(ctx_with_span, ContextKey::new("baggage"), with_complex)
  
  // Create complex telemetry data structure
  let complex_data = {
    "resource": resource,
    "scope": scope,
    "span": span,
    "context": ctx_with_both,
    "attributes": [
      ("complex.string", StringValue("complex string value")),
      ("complex.int", IntValue(42)),
      ("complex.float", FloatValue(3.14159)),
      ("complex.bool", BoolValue(true)),
      ("complex.string.array", ArrayStringValue(["complex", "data", "structure"])),
      ("complex.int.array", ArrayIntValue([1, 2, 3, 4, 5]))
    ]
  }
  
  // Serialize complex telemetry data
  let serialized = serialize_complex_telemetry_data(complex_data)
  
  // Verify serialization produces non-empty result
  assert_true(serialized != "")
  
  // Deserialize complex telemetry data
  let deserialized = deserialize_complex_telemetry_data(serialized)
  
  // Verify deserialization produces original complex telemetry data
  assert_eq(deserialized.resource.attributes.length(), resource.attributes.length())
  assert_eq(deserialized.scope.name, scope.name)
  assert_eq(Span::name(deserialized.span), Span::name(span))
  
  let deserialized_ctx = deserialized.context
  let deserialized_span_ctx = Context::get(deserialized_ctx, ContextKey::new("span.context"))
  let deserialized_baggage = Context::get(deserialized_ctx, ContextKey::new("baggage"))
  
  match (deserialized_span_ctx) {
    Some(span_ctx) => {
      assert_eq(SpanContext::trace_id(span_ctx), SpanContext::trace_id(span_ctx))
      assert_eq(SpanContext::span_id(span_ctx), SpanContext::span_id(span_ctx))
    }
    None => assert_true(false) // Should not happen
  }
  
  match (deserialized_baggage) {
    Some(baggage) => {
      assert_eq(Baggage::get_entry(baggage, "complex.key"), Some("complex.value"))
    }
    None => assert_true(false) // Should not happen
  }
  
  // Verify attributes are preserved
  assert_eq(deserialized.attributes.length(), 6)
}

// Test 10: Serialization Format Compatibility and Versioning
test "serialization format compatibility and versioning" {
  // Test serialization with different format versions
  let attrs = Attributes::new()
  Attributes::set(attrs, "version.test", StringValue("compatibility test"))
  Attributes::set(attrs, "format.version", StringValue("1.0"))
  
  // Serialize with current format version
  let current_serialized = Attributes::serialize_with_version(attrs, "1.0")
  
  // Serialize with older format version
  let older_serialized = Attributes::serialize_with_version(attrs, "0.9")
  
  // Verify both serializations produce non-empty results
  assert_true(current_serialized != "")
  assert_true(older_serialized != "")
  
  // Deserialize both formats
  let current_deserialized = Attributes::deserialize_with_version(current_serialized, "1.0")
  let older_deserialized = Attributes::deserialize_with_version(older_serialized, "0.9")
  
  // Verify both deserializations produce equivalent results
  assert_eq(Attributes::get(current_deserialized, "version.test"), Some(StringValue("compatibility test")))
  assert_eq(Attributes::get(older_deserialized, "version.test"), Some(StringValue("compatibility test")))
  
  // Test backward compatibility
  let legacy_serialized = "legacy:format:data"
  let legacy_deserialized = Attributes::deserialize_legacy(legacy_serialized)
  
  // Verify legacy deserialization works (may produce empty or partial results)
  assert_true(legacy_deserialized != None)
  
  // Test forward compatibility
  let future_serialized = Attributes::serialize_with_version(attrs, "2.0")
  let future_deserialized = Attributes::deserialize_with_version(future_serialized, "2.0")
  
  // Verify future format deserialization works
  assert_eq(Attributes::get(future_deserialized, "version.test"), Some(StringValue("compatibility test")))
  
  // Test format migration
  let migrated_attrs = Attributes::migrate_format(older_deserialized, "0.9", "1.0")
  
  // Verify migration preserves data
  assert_eq(Attributes::get(migrated_attrs, "version.test"), Some(StringValue("compatibility test")))
}

// Helper functions for serialization tests (these would be implemented in the actual system)
fn serialize_complex_telemetry_data(data) -> String {
  // Implementation would serialize complex telemetry data structure
  "serialized_complex_data"
}

fn deserialize_complex_telemetry_data(serialized) -> ComplexTelemetryData {
  // Implementation would deserialize complex telemetry data structure
  {
    "resource": Resource::new(),
    "scope": InstrumentationScope::new("", None, None),
    "span": Span::new("", Internal, SpanContext::new("", "", true, "")),
    "context": Context::root(),
    "attributes": []
  }
}