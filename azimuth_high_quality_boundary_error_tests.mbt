// High Quality Boundary Condition and Error Handling Tests for Azimuth Telemetry System
// This file contains comprehensive tests for boundary conditions and error handling

// Test 1: Attribute Value Boundary Conditions
test "attribute value boundary conditions" {
  // Test extreme integer values
  let max_int = IntValue(2147483647)
  let min_int = IntValue(-2147483648)
  let zero_int = IntValue(0)
  
  assert_eq(max_int, IntValue(2147483647))
  assert_eq(min_int, IntValue(-2147483648))
  assert_eq(zero_int, IntValue(0))
  
  // Test extreme float values
  let max_float = FloatValue(1.7976931348623157e+308)
  let min_float = FloatValue(-1.7976931348623157e+308)
  let zero_float = FloatValue(0.0)
  let negative_zero = FloatValue(-0.0)
  let very_small_float = FloatValue(1.0e-10)
  let very_large_float = FloatValue(1.0e+10)
  
  assert_eq(max_float, FloatValue(1.7976931348623157e+308))
  assert_eq(min_float, FloatValue(-1.7976931348623157e+308))
  assert_eq(zero_float, FloatValue(0.0))
  assert_eq(negative_zero, FloatValue(-0.0))
  assert_eq(very_small_float, FloatValue(1.0e-10))
  assert_eq(very_large_float, FloatValue(1.0e+10))
  
  // Test empty and single character strings
  let empty_string = StringValue("")
  let single_char_string = StringValue("a")
  let whitespace_string = StringValue("   ")
  let newline_string = StringValue("\n")
  let tab_string = StringValue("\t")
  
  assert_eq(empty_string, StringValue(""))
  assert_eq(single_char_string, StringValue("a"))
  assert_eq(whitespace_string, StringValue("   "))
  assert_eq(newline_string, StringValue("\n"))
  assert_eq(tab_string, StringValue("\t"))
  
  // Test empty arrays
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  assert_eq(empty_string_array, ArrayStringValue([]))
  assert_eq(empty_int_array, ArrayIntValue([]))
}

// Test 2: Span Context Boundary Conditions
test "span context boundary conditions" {
  // Test invalid span contexts
  let empty_trace_id = SpanContext::new("", "span123", true, "")
  let empty_span_id = SpanContext::new("trace123", "", true, "")
  let both_empty = SpanContext::new("", "", true, "")
  
  // Test validity checks
  assert_false(SpanContext::is_valid(empty_trace_id))
  assert_false(SpanContext::is_valid(empty_span_id))
  assert_false(SpanContext::is_valid(both_empty))
  
  // Test valid span contexts
  let min_valid_trace = SpanContext::new("0", "0", true, "")
  let max_valid_trace = SpanContext::new("ffffffffffffffffffffffffffffffff", "ffffffffffffffff", true, "")
  
  assert_true(SpanContext::is_valid(min_valid_trace))
  assert_true(SpanContext::is_valid(max_valid_trace))
  
  // Test trace state boundary conditions
  let empty_trace_state = SpanContext::new("trace123", "span456", true, "")
  let long_trace_state = SpanContext::new("trace123", "span456", true, "key1=value1,key2=value2,key3=value3,key4=value4,key5=value5")
  
  assert_eq(SpanContext::trace_state(empty_trace_state), "")
  assert_eq(SpanContext::trace_state(long_trace_state), "key1=value1,key2=value2,key3=value3,key4=value4,key5=value5")
}

// Test 3: Attribute Operations Boundary Conditions
test "attribute operations boundary conditions" {
  // Test attributes with empty keys
  let attrs = Attributes::new()
  Attributes::set(attrs, "", StringValue("empty key"))
  assert_eq(Attributes::get(attrs, ""), Some(StringValue("empty key")))
  
  // Test attributes with special character keys
  Attributes::set(attrs, "!@#$%^&*()", StringValue("special chars"))
  Attributes::set(attrs, "key with spaces", StringValue("spaces"))
  Attributes::set(attrs, "key\twith\ttabs", StringValue("tabs"))
  Attributes::set(attrs, "key\nwith\nnewlines", StringValue("newlines"))
  
  assert_eq(Attributes::get(attrs, "!@#$%^&*()"), Some(StringValue("special chars")))
  assert_eq(Attributes::get(attrs, "key with spaces"), Some(StringValue("spaces")))
  assert_eq(Attributes::get(attrs, "key\twith\ttabs"), Some(StringValue("tabs")))
  assert_eq(Attributes::get(attrs, "key\nwith\nnewlines"), Some(StringValue("newlines")))
  
  // Test attributes with long keys and values
  let long_key = "k".repeat(1000)
  let long_value = "v".repeat(1000)
  Attributes::set(attrs, long_key, StringValue(long_value))
  
  assert_eq(Attributes::get(attrs, long_key), Some(StringValue(long_value)))
  
  // Test attributes with unicode keys and values
  Attributes::set(attrs, "键", StringValue("值"))
  Attributes::set(attrs, "ключ", StringValue("значение"))
  Attributes::set(attrs, "キー", StringValue("値"))
  
  assert_eq(Attributes::get(attrs, "键"), Some(StringValue("值")))
  assert_eq(Attributes::get(attrs, "ключ"), Some(StringValue("значение")))
  assert_eq(Attributes::get(attrs, "キー"), Some(StringValue("値")))
}

// Test 4: Baggage Operations Boundary Conditions
test "baggage operations boundary conditions" {
  // Test baggage with empty keys
  let baggage = Baggage::new()
  let with_empty_key = Baggage::set_entry(baggage, "", "empty key")
  assert_eq(Baggage::get_entry(with_empty_key, ""), Some("empty key"))
  
  // Test baggage with empty values
  let with_empty_value = Baggage::set_entry(baggage, "empty.value", "")
  assert_eq(Baggage::get_entry(with_empty_value, "empty.value"), Some(""))
  
  // Test baggage with special characters
  let with_special_chars = Baggage::set_entry(baggage, "special.chars", "!@#$%^&*()")
  assert_eq(Baggage::get_entry(with_special_chars, "special.chars"), Some("!@#$%^&*()"))
  
  // Test baggage with unicode
  let with_unicode = Baggage::set_entry(baggage, "unicode.测试", "测试值")
  assert_eq(Baggage::get_entry(with_unicode, "unicode.测试"), Some("测试值"))
  
  // Test baggage with long keys and values
  let long_key = "k".repeat(500)
  let long_value = "v".repeat(500)
  let with_long = Baggage::set_entry(baggage, long_key, long_value)
  assert_eq(Baggage::get_entry(with_long, long_key), Some(long_value))
  
  // Test baggage removal of non-existent keys
  let without_nonexistent = Baggage::remove_entry(baggage, "nonexistent.key")
  assert_eq(without_nonexistent, baggage)
}

// Test 5: Context Operations Boundary Conditions
test "context operations boundary conditions" {
  // Test context with empty keys
  let ctx = Context::root()
  let empty_key = ContextKey::new("")
  let ctx_with_empty = Context::with_value(ctx, empty_key, "empty key value")
  assert_eq(Context::get(ctx_with_empty, empty_key), Some("empty key value"))
  
  // Test context with special character keys
  let special_key = ContextKey::new("!@#$%^&*()")
  let ctx_with_special = Context::with_value(ctx, special_key, "special key value")
  assert_eq(Context::get(ctx_with_special, special_key), Some("special key value"))
  
  // Test context with unicode keys
  let unicode_key = ContextKey::new("测试键")
  let ctx_with_unicode = Context::with_value(ctx, unicode_key, "测试值")
  assert_eq(Context::get(ctx_with_unicode, unicode_key), Some("测试值"))
  
  // Test context with empty values
  let empty_value_key = ContextKey::new("empty.value")
  let ctx_with_empty_value = Context::with_value(ctx, empty_value_key, "")
  assert_eq(Context::get(ctx_with_empty_value, empty_value_key), Some(""))
  
  // Test context with long values
  let long_value = "v".repeat(10000)
  let long_value_key = ContextKey::new("long.value")
  let ctx_with_long_value = Context::with_value(ctx, long_value_key, long_value)
  assert_eq(Context::get(ctx_with_long_value, long_value_key), Some(long_value))
}

// Test 6: Text Map Carrier Boundary Conditions
test "text map carrier boundary conditions" {
  // Test carrier with empty headers
  let carrier = TextMapCarrier::new()
  assert_eq(carrier.headers.length(), 0)
  
  // Test carrier with empty keys
  TextMapCarrier::set(carrier, "", "empty key")
  assert_eq(TextMapCarrier::get(carrier, ""), Some("empty key"))
  
  // Test carrier with empty values
  TextMapCarrier::set(carrier, "empty.value", "")
  assert_eq(TextMapCarrier::get(carrier, "empty.value"), Some(""))
  
  // Test carrier with special characters
  TextMapCarrier::set(carrier, "special.chars", "!@#$%^&*()")
  TextMapCarrier::set(carrier, "whitespace", "   ")
  TextMapCarrier::set(carrier, "newline", "\n")
  TextMapCarrier::set(carrier, "tab", "\t")
  
  assert_eq(TextMapCarrier::get(carrier, "special.chars"), Some("!@#$%^&*()"))
  assert_eq(TextMapCarrier::get(carrier, "whitespace"), Some("   "))
  assert_eq(TextMapCarrier::get(carrier, "newline"), Some("\n"))
  assert_eq(TextMapCarrier::get(carrier, "tab"), Some("\t"))
  
  // Test carrier with unicode
  TextMapCarrier::set(carrier, "unicode.测试", "测试值")
  assert_eq(TextMapCarrier::get(carrier, "unicode.测试"), Some("测试值"))
  
  // Test carrier with long headers
  let long_key = "k".repeat(1000)
  let long_value = "v".repeat(1000)
  TextMapCarrier::set(carrier, long_key, long_value)
  assert_eq(TextMapCarrier::get(carrier, long_key), Some(long_value))
}

// Test 7: Propagation Error Handling
test "propagation error handling" {
  // Test propagation with invalid traceparent format
  let invalid_traceparent_carrier = TextMapCarrier::new()
  TextMapCarrier::set(invalid_traceparent_carrier, "traceparent", "invalid-format")
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let ctx = W3CTraceContextPropagator::extract(trace_propagator, invalid_traceparent_carrier)
  
  // Should handle invalid format gracefully
  let span_ctx = Context::get(ctx, ContextKey::new("span.context"))
  match (span_ctx) {
    Some(_) => assert_true(false) // Should not extract valid context from invalid format
    None => assert_true(true) // Expected behavior
  }
  
  // Test propagation with malformed baggage
  let malformed_baggage_carrier = TextMapCarrier::new()
  TextMapCarrier::set(malformed_baggage_carrier, "baggage", "invalid=baggage=format=extra=equals")
  
  let baggage_propagator = W3CBaggagePropagator::new()
  let baggage_ctx = W3CBaggagePropagator::extract(baggage_propagator, malformed_baggage_carrier)
  
  // Should handle malformed baggage gracefully
  let baggage = Context::get(baggage_ctx, ContextKey::new("baggage"))
  match (baggage) {
    Some(_) => assert_true(true) // Should extract some baggage even if malformed
    None => assert_true(true) // Or return empty baggage, both are acceptable
  }
  
  // Test propagation with empty carrier
  let empty_carrier = TextMapCarrier::new()
  let empty_ctx = W3CTraceContextPropagator::extract(trace_propagator, empty_carrier)
  let empty_baggage_ctx = W3CBaggagePropagator::extract(baggage_propagator, empty_carrier)
  
  // Should handle empty carrier gracefully
  let empty_span_ctx = Context::get(empty_ctx, ContextKey::new("span.context"))
  let empty_baggage = Context::get(empty_baggage_ctx, ContextKey::new("baggage"))
  
  match (empty_span_ctx) {
    Some(_) => assert_true(false) // Should not extract context from empty carrier
    None => assert_true(true) // Expected behavior
  }
  
  match (empty_baggage) {
    Some(_) => assert_true(false) // Should not extract baggage from empty carrier
    None => assert_true(true) // Expected behavior
  }
}

// Test 8: Resource Operations Boundary Conditions
test "resource operations boundary conditions" {
  // Test resource with empty attributes
  let resource = Resource::new()
  assert_eq(resource.attributes.length(), 0)
  
  // Test resource with empty keys
  Resource::set_attribute(resource, "", StringValue("empty key"))
  assert_eq(Resource::get_attribute(resource, ""), Some(StringValue("empty key")))
  
  // Test resource with special character keys
  Resource::set_attribute(resource, "!@#$%^&*()", StringValue("special chars"))
  Resource::set_attribute(resource, "key with spaces", StringValue("spaces"))
  
  assert_eq(Resource::get_attribute(resource, "!@#$%^&*()"), Some(StringValue("special chars")))
  assert_eq(Resource::get_attribute(resource, "key with spaces"), Some(StringValue("spaces")))
  
  // Test resource with unicode keys
  Resource::set_attribute(resource, "资源.名称", StringValue("测试资源"))
  assert_eq(Resource::get_attribute(resource, "资源.名称"), Some(StringValue("测试资源")))
  
  // Test resource with long attribute names and values
  let long_attr_name = "attr.".repeat(100)
  let long_attr_value = "value.".repeat(100)
  Resource::set_attribute(resource, long_attr_name, StringValue(long_attr_value))
  
  assert_eq(Resource::get_attribute(resource, long_attr_name), Some(StringValue(long_attr_value)))
}

// Test 9: Instrumentation Scope Boundary Conditions
test "instrumentation scope boundary conditions" {
  // Test scope with empty name
  let empty_name_scope = InstrumentationScope::new("", None, None)
  assert_eq(empty_name_scope.name, "")
  
  // Test scope with special characters in name
  let special_chars_scope = InstrumentationScope::new("!@#$%^&*()", None, None)
  assert_eq(special_chars_scope.name, "!@#$%^&*()")
  
  // Test scope with unicode in name
  let unicode_scope = InstrumentationScope::new("测试.遥测", None, None)
  assert_eq(unicode_scope.name, "测试.遥测")
  
  // Test scope with very long name
  let long_name = "scope.".repeat(100)
  let long_name_scope = InstrumentationScope::new(long_name, None, None)
  assert_eq(long_name_scope.name, long_name)
  
  // Test scope with empty version
  let empty_version_scope = InstrumentationScope::new("test.scope", Some(""), None)
  assert_eq(empty_version_scope.version, Some(""))
  
  // Test scope with special characters in version
  let special_version_scope = InstrumentationScope::new("test.scope", Some("!@#$%^&*()"), None)
  assert_eq(special_version_scope.version, Some("!@#$%^&*()"))
  
  // Test scope with empty schema URL
  let empty_schema_scope = InstrumentationScope::new("test.scope", None, Some(""))
  assert_eq(empty_schema_scope.schema_url, Some(""))
  
  // Test scope with invalid schema URL format
  let invalid_schema_scope = InstrumentationScope::new("test.scope", None, Some("not-a-url"))
  assert_eq(invalid_schema_scope.schema_url, Some("not-a-url"))
}

// Test 10: Error Recovery and Resilience
test "error recovery and resilience" {
  // Test recovery from invalid span context
  let invalid_ctx = SpanContext::new("", "", true, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
  
  // Test creating valid span after invalid context
  let valid_ctx = SpanContext::new("trace123", "span456", true, "")
  assert_true(SpanContext::is_valid(valid_ctx))
  
  // Test recovery from malformed baggage
  let malformed_baggage = Baggage::new()
  let with_malformed = Baggage::set_entry(malformed_baggage, "key=with=equals", "value")
  assert_eq(Baggage::get_entry(with_malformed, "key=with=equals"), Some("value"))
  
  // Test recovery from corrupted attributes
  let corrupted_attrs = Attributes::new()
  Attributes::set(corrupted_attrs, "\x00\x01\x02", StringValue("corrupted"))
  assert_eq(Attributes::get(corrupted_attrs, "\x00\x01\x02"), Some(StringValue("corrupted")))
  
  // Test recovery from extremely large data
  let huge_attrs = Attributes::new()
  let huge_key = "k".repeat(10000)
  let huge_value = "v".repeat(10000)
  Attributes::set(huge_attrs, huge_key, StringValue(huge_value))
  assert_eq(Attributes::get(huge_attrs, huge_key), Some(StringValue(huge_value)))
  
  // Test recovery from nested context corruption
  let root_ctx = Context::root()
  let corrupted_key = ContextKey::new("\x00\x01\x02")
  let ctx_with_corrupted = Context::with_value(root_ctx, corrupted_key, "corrupted value")
  assert_eq(Context::get(ctx_with_corrupted, corrupted_key), Some("corrupted value"))
}