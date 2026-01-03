// High Quality Fundamental Tests for Azimuth Telemetry System
// This file contains comprehensive tests for fundamental data types and operations

// Test 1: Advanced Attribute Value Type Operations and Conversions
test "attribute value type comprehensive operations" {
  // Test string attribute values
  let string_attr = StringValue("test value")
  assert_eq(string_attr, StringValue("test value"))
  
  // Test numeric attribute values with edge cases
  let int_attr = IntValue(42)
  let negative_int_attr = IntValue(-42)
  let zero_int_attr = IntValue(0)
  let max_int_attr = IntValue(2147483647)
  let min_int_attr = IntValue(-2147483648)
  
  assert_eq(int_attr, IntValue(42))
  assert_eq(negative_int_attr, IntValue(-42))
  assert_eq(zero_int_attr, IntValue(0))
  assert_eq(max_int_attr, IntValue(2147483647))
  assert_eq(min_int_attr, IntValue(-2147483648))
  
  // Test floating point attribute values with precision
  let float_attr = FloatValue(3.14159)
  let negative_float_attr = FloatValue(-2.71828)
  let zero_float_attr = FloatValue(0.0)
  let scientific_float_attr = FloatValue(1.23e-4)
  
  assert_eq(float_attr, FloatValue(3.14159))
  assert_eq(negative_float_attr, FloatValue(-2.71828))
  assert_eq(zero_float_attr, FloatValue(0.0))
  assert_eq(scientific_float_attr, FloatValue(1.23e-4))
  
  // Test boolean attribute values
  let true_attr = BoolValue(true)
  let false_attr = BoolValue(false)
  
  assert_eq(true_attr, BoolValue(true))
  assert_eq(false_attr, BoolValue(false))
  assert_true(true_attr != false_attr)
}

// Test 2: Array Attribute Value Operations
test "array attribute value operations" {
  // Test string array attributes
  let empty_string_array = ArrayStringValue([])
  let single_string_array = ArrayStringValue(["single"])
  let multi_string_array = ArrayStringValue(["first", "second", "third"])
  
  assert_eq(empty_string_array, ArrayStringValue([]))
  assert_eq(single_string_array, ArrayStringValue(["single"]))
  assert_eq(multi_string_array, ArrayStringValue(["first", "second", "third"]))
  
  // Test integer array attributes
  let empty_int_array = ArrayIntValue([])
  let single_int_array = ArrayIntValue([42])
  let multi_int_array = ArrayIntValue([1, 2, 3, 4, 5])
  let negative_int_array = ArrayIntValue([-1, -2, -3])
  
  assert_eq(empty_int_array, ArrayIntValue([]))
  assert_eq(single_int_array, ArrayIntValue([42]))
  assert_eq(multi_int_array, ArrayIntValue([1, 2, 3, 4, 5]))
  assert_eq(negative_int_array, ArrayIntValue([-1, -2, -3]))
}

// Test 3: Attributes Collection Operations
test "attributes collection comprehensive operations" {
  // Test empty attributes
  let empty_attrs = Attributes::new()
  assert_eq(empty_attrs.values.length(), 0)
  
  // Test single attribute operations
  let single_attrs = Attributes::new()
  Attributes::set(single_attrs, "key1", StringValue("value1"))
  assert_eq(single_attrs.values.length(), 1)
  assert_eq(Attributes::get(single_attrs, "key1"), Some(StringValue("value1")))
  assert_eq(Attributes::get(single_attrs, "nonexistent"), None)
  
  // Test multiple attributes with different types
  let multi_attrs = Attributes::new()
  Attributes::set(multi_attrs, "string.key", StringValue("string value"))
  Attributes::set(multi_attrs, "int.key", IntValue(123))
  Attributes::set(multi_attrs, "float.key", FloatValue(45.67))
  Attributes::set(multi_attrs, "bool.key", BoolValue(true))
  Attributes::set(multi_attrs, "string.array.key", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(multi_attrs, "int.array.key", ArrayIntValue([1, 2, 3]))
  
  assert_eq(multi_attrs.values.length(), 6)
  assert_eq(Attributes::get(multi_attrs, "string.key"), Some(StringValue("string value")))
  assert_eq(Attributes::get(multi_attrs, "int.key"), Some(IntValue(123)))
  assert_eq(Attributes::get(multi_attrs, "float.key"), Some(FloatValue(45.67)))
  assert_eq(Attributes::get(multi_attrs, "bool.key"), Some(BoolValue(true)))
  assert_eq(Attributes::get(multi_attrs, "string.array.key"), Some(ArrayStringValue(["a", "b", "c"])))
  assert_eq(Attributes::get(multi_attrs, "int.array.key"), Some(ArrayIntValue([1, 2, 3])))
}

// Test 4: Resource Operations with Complex Attributes
test "resource operations with complex attributes" {
  // Test empty resource
  let empty_resource = Resource::new()
  assert_eq(empty_resource.attributes.length(), 0)
  
  // Test resource with multiple attributes
  let resource = Resource::new()
  Resource::set_attribute(resource, "service.name", StringValue("azimuth-service"))
  Resource::set_attribute(resource, "service.version", StringValue("1.0.0"))
  Resource::set_attribute(resource, "service.instance.id", StringValue("instance-123"))
  Resource::set_attribute(resource, "host.name", StringValue("localhost"))
  Resource::set_attribute(resource, "host.port", IntValue(8080))
  Resource::set_attribute(resource, "process.pid", IntValue(12345))
  Resource::set_attribute(resource, "process.start.time", FloatValue(1640995200.0))
  
  assert_eq(resource.attributes.length(), 7)
  assert_eq(Resource::get_attribute(resource, "service.name"), Some(StringValue("azimuth-service")))
  assert_eq(Resource::get_attribute(resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(resource, "host.port"), Some(IntValue(8080)))
  assert_eq(Resource::get_attribute(resource, "process.start.time"), Some(FloatValue(1640995200.0)))
}

// Test 5: Instrumentation Scope Operations
test "instrumentation scope comprehensive operations" {
  // Test minimal scope
  let minimal_scope = InstrumentationScope::new("test-scope", None, None)
  assert_eq(minimal_scope.name, "test-scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
  
  // Test complete scope
  let complete_scope = InstrumentationScope::new(
    "azimuth-telemetry",
    Some("1.0.0"),
    Some("https://example.com/schema/v1")
  )
  assert_eq(complete_scope.name, "azimuth-telemetry")
  assert_eq(complete_scope.version, Some("1.0.0"))
  assert_eq(complete_scope.schema_url, Some("https://example.com/schema/v1"))
}

// Test 6: Context Operations with Complex Data
test "context operations with complex data" {
  // Test root context
  let root_ctx = Context::root()
  assert_eq(Context::data(root_ctx), None)
  
  // Test context with simple values
  let simple_key = ContextKey::new("simple.key")
  let ctx_with_simple = Context::with_value(root_ctx, simple_key, "simple value")
  assert_eq(Context::get(ctx_with_simple, simple_key), Some("simple value"))
  
  // Test context with multiple values
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  let key3 = ContextKey::new("key3")
  
  let ctx_with_key1 = Context::with_value(root_ctx, key1, "value1")
  let ctx_with_key2 = Context::with_value(ctx_with_key1, key2, "value2")
  let ctx_with_key3 = Context::with_value(ctx_with_key2, key3, "value3")
  
  assert_eq(Context::get(ctx_with_key3, key1), Some("value1"))
  assert_eq(Context::get(ctx_with_key3, key2), Some("value2"))
  assert_eq(Context::get(ctx_with_key3, key3), Some("value3"))
}

// Test 7: Baggage Operations with Complex Entries
test "baggage operations with complex entries" {
  // Test empty baggage
  let empty_baggage = Baggage::new()
  assert_eq(empty_baggage.entries.length(), 0)
  
  // Test baggage with single entry
  let single_baggage = Baggage::new()
  let with_entry = Baggage::set_entry(single_baggage, "user.id", "user123")
  assert_eq(with_entry.entries.length(), 1)
  assert_eq(Baggage::get_entry(with_entry, "user.id"), Some("user123"))
  
  // Test baggage with multiple entries
  let multi_baggage = Baggage::new()
  let with_user = Baggage::set_entry(multi_baggage, "user.id", "user123")
  let with_session = Baggage::set_entry(with_user, "session.id", "session456")
  let with_tenant = Baggage::set_entry(with_session, "tenant.id", "tenant789")
  let with_correlation = Baggage::set_entry(with_tenant, "correlation.id", "corr000")
  
  assert_eq(with_correlation.entries.length(), 4)
  assert_eq(Baggage::get_entry(with_correlation, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(with_correlation, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(with_correlation, "tenant.id"), Some("tenant789"))
  assert_eq(Baggage::get_entry(with_correlation, "correlation.id"), Some("corr000"))
  
  // Test baggage entry removal
  let without_user = Baggage::remove_entry(with_correlation, "user.id")
  assert_eq(without_user.entries.length(), 3)
  assert_eq(Baggage::get_entry(without_user, "user.id"), None)
  assert_eq(Baggage::get_entry(without_user, "session.id"), Some("session456"))
}

// Test 8: Span Context Operations with Validation
test "span context operations with validation" {
  // Test valid span context
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1,key2=value2")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_eq(SpanContext::trace_id(valid_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(valid_ctx), "b7ad6b7169203331")
  assert_true(SpanContext::is_sampled(valid_ctx))
  assert_eq(SpanContext::trace_state(valid_ctx), "key1=value1,key2=value2")
  
  // Test span context with different sampling states
  let sampled_ctx = SpanContext::new("trace123", "span456", true, "")
  let not_sampled_ctx = SpanContext::new("trace123", "span456", false, "")
  
  assert_true(SpanContext::is_sampled(sampled_ctx))
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
  
  // Test span context with empty trace state
  let empty_trace_state_ctx = SpanContext::new("trace123", "span456", true, "")
  assert_eq(SpanContext::trace_state(empty_trace_state_ctx), "")
}

// Test 9: String and Unicode Operations
test "string and unicode operations" {
  // Test basic string operations
  let basic_string = "hello world"
  assert_eq(basic_string.length(), 11)
  assert_eq(basic_string + "!", "hello world!")
  
  // Test unicode strings
  let unicode_string = "测试中文🚀"
  assert_eq(unicode_string.length(), 5) // Unicode characters count
  assert_eq(unicode_string + " telemetry", "测试中文🚀 telemetry")
  
  // Test emoji strings
  let emoji_string = "😀🎉🔥"
  assert_eq(emoji_string.length(), 3)
  assert_eq(emoji_string + " test", "😀🎉🔥 test")
  
  // Test special characters
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq(special_chars.length(), 26)
  assert_eq(special_chars + "end", "!@#$%^&*()_+-=[]{}|;':\",./<>?end")
  
  // Test empty string
  let empty_string = ""
  assert_eq(empty_string.length(), 0)
  assert_eq(empty_string + "not empty", "not empty")
}

// Test 10: Numeric Operations and Boundary Conditions
test "numeric operations and boundary conditions" {
  // Test integer arithmetic operations
  assert_eq(1 + 1, 2)
  assert_eq(10 - 3, 7)
  assert_eq(5 * 4, 20)
  assert_eq(20 / 4, 5)
  assert_eq(20 % 3, 2)
  
  // Test negative number operations
  assert_eq(-5 + 10, 5)
  assert_eq(-10 - 5, -15)
  assert_eq(-5 * -5, 25)
  assert_eq(-20 / 5, -4)
  
  // Test zero operations
  assert_eq(0 + 5, 5)
  assert_eq(5 + 0, 5)
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
  assert_eq(0 / 5, 0)
  
  // Test large number operations
  let large_num = 1000000
  assert_eq(large_num + large_num, 2000000)
  assert_eq(large_num * 2, 2000000)
  
  // Test float operations
  assert_eq(1.5 + 2.5, 4.0)
  assert_eq(5.0 - 2.0, 3.0)
  assert_eq(2.5 * 4.0, 10.0)
  assert_eq(10.0 / 2.0, 5.0)
  
  // Test float precision
  let pi = 3.14159
  let e = 2.71828
  assert_eq((pi + e).to_int(), 5) // 3.14159 + 2.71828 = 5.85987 -> 5 when converted to int
}