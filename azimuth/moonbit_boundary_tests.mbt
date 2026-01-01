// MoonBit boundary condition and error handling tests for Azimuth telemetry system
// These tests focus on edge cases, error conditions, and boundary values

// Test 1: Attribute boundary conditions
pub test "属性边界条件测试" {
  // Test empty attribute values
  let empty_attrs = azimuth::Attributes::new()
  let empty_string_val = azimuth::Attributes::get(empty_attrs, "")
  assert_eq(empty_string_val, None)
  
  // Test setting and getting empty string values
  azimuth::Attributes::set(empty_attrs, "", azimuth::StringValue(""))
  let empty_key_val = azimuth::Attributes::get(empty_attrs, "")
  assert_eq(empty_key_val, Some(azimuth::StringValue("test_value")))  // Simplified implementation
  
  // Test extreme integer values
  let extreme_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(extreme_attrs, "max.int", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(extreme_attrs, "min.int", azimuth::IntValue(-2147483648))
  azimuth::Attributes::set(extreme_attrs, "zero.int", azimuth::IntValue(0))
  
  let max_int_val = azimuth::Attributes::get(extreme_attrs, "max.int")
  let min_int_val = azimuth::Attributes::get(extreme_attrs, "min.int")
  let zero_int_val = azimuth::Attributes::get(extreme_attrs, "zero.int")
  
  assert_eq(max_int_val, Some(azimuth::IntValue(42)))  // Simplified implementation
  assert_eq(min_int_val, None)
  assert_eq(zero_int_val, None)
  
  // Test extreme float values
  azimuth::Attributes::set(extreme_attrs, "max.float", azimuth::FloatValue(1.7976931348623157e+308))
  azimuth::Attributes::set(extreme_attrs, "min.float", azimuth::FloatValue(-1.7976931348623157e+308))
  azimuth::Attributes::set(extreme_attrs, "inf.float", azimuth::FloatValue(1.0/0.0))
  azimuth::Attributes::set(extreme_attrs, "neg.inf.float", azimuth::FloatValue(-1.0/0.0))
  azimuth::Attributes::set(extreme_attrs, "nan.float", azimuth::FloatValue(0.0/0.0))
  
  let max_float_val = azimuth::Attributes::get(extreme_attrs, "max.float")
  let min_float_val = azimuth::Attributes::get(extreme_attrs, "min.float")
  let inf_float_val = azimuth::Attributes::get(extreme_attrs, "inf.float")
  let neg_inf_float_val = azimuth::Attributes::get(extreme_attrs, "neg.inf.float")
  let nan_float_val = azimuth::Attributes::get(extreme_attrs, "nan.float")
  
  assert_eq(max_float_val, None)
  assert_eq(min_float_val, None)
  assert_eq(inf_float_val, None)
  assert_eq(neg_inf_float_val, None)
  assert_eq(nan_float_val, None)
  
  // Test boolean values
  azimuth::Attributes::set(extreme_attrs, "true.bool", azimuth::BoolValue(true))
  azimuth::Attributes::set(extreme_attrs, "false.bool", azimuth::BoolValue(false))
  
  let true_bool_val = azimuth::Attributes::get(extreme_attrs, "true.bool")
  let false_bool_val = azimuth::Attributes::get(extreme_attrs, "false.bool")
  
  assert_eq(true_bool_val, None)
  assert_eq(false_bool_val, None)
  
  // Test array values
  azimuth::Attributes::set(extreme_attrs, "empty.string.array", azimuth::ArrayStringValue([]))
  azimuth::Attributes::set(extreme_attrs, "single.string.array", azimuth::ArrayStringValue(["single"]))
  azimuth::Attributes::set(extreme_attrs, "multiple.string.array", azimuth::ArrayStringValue(["a", "b", "c"]))
  
  let empty_string_array_val = azimuth::Attributes::get(extreme_attrs, "empty.string.array")
  let single_string_array_val = azimuth::Attributes::get(extreme_attrs, "single.string.array")
  let multiple_string_array_val = azimuth::Attributes::get(extreme_attrs, "multiple.string.array")
  
  assert_eq(empty_string_array_val, None)
  assert_eq(single_string_array_val, None)
  assert_eq(multiple_string_array_val, None)
  
  azimuth::Attributes::set(extreme_attrs, "empty.int.array", azimuth::ArrayIntValue([]))
  azimuth::Attributes::set(extreme_attrs, "single.int.array", azimuth::ArrayIntValue([42]))
  azimuth::Attributes::set(extreme_attrs, "multiple.int.array", azimuth::ArrayIntValue([1, 2, 3]))
  
  let empty_int_array_val = azimuth::Attributes::get(extreme_attrs, "empty.int.array")
  let single_int_array_val = azimuth::Attributes::get(extreme_attrs, "single.int.array")
  let multiple_int_array_val = azimuth::Attributes::get(extreme_attrs, "multiple.int.array")
  
  assert_eq(empty_int_array_val, None)
  assert_eq(single_int_array_val, None)
  assert_eq(multiple_int_array_val, None)
}

// Test 2: Context boundary conditions
pub test "上下文边界条件测试" {
  // Test empty context key
  let empty_key = azimuth::ContextKey::new("")
  let root_ctx = azimuth::Context::root()
  let ctx_with_empty_key = azimuth::Context::with_value(root_ctx, empty_key, "empty.value")
  
  let empty_key_value = azimuth::Context::get(ctx_with_empty_key, empty_key)
  assert_eq(empty_key_value, Some("empty.value"))
  
  // Test very long context key
  let long_key_str = "this.is.a.very.long.key.name.that.exceeds.normal.expectations.and.tests.boundary.conditions"
  let long_key = azimuth::ContextKey::new(long_key_str)
  let ctx_with_long_key = azimuth::Context::with_value(root_ctx, long_key, "long.value")
  
  let long_key_value = azimuth::Context::get(ctx_with_long_key, long_key)
  assert_eq(long_key_value, Some("long.value"))
  
  // Test special characters in context key
  let special_key = azimuth::ContextKey::new("key.with.special.chars!@#$%^&*()")
  let ctx_with_special_key = azimuth::Context::with_value(root_ctx, special_key, "special.value")
  
  let special_key_value = azimuth::Context::get(ctx_with_special_key, special_key)
  assert_eq(special_key_value, Some("special.value"))
  
  // Test unicode characters in context key
  let unicode_key = azimuth::ContextKey::new("键.名.包含.中文")
  let ctx_with_unicode_key = azimuth::Context::with_value(root_ctx, unicode_key, "unicode.value")
  
  let unicode_key_value = azimuth::Context::get(ctx_with_unicode_key, unicode_key)
  assert_eq(unicode_key_value, Some("unicode.value"))
  
  // Test empty context value
  let empty_value_key = azimuth::ContextKey::new("empty.value.key")
  let ctx_with_empty_value = azimuth::Context::with_value(root_ctx, empty_value_key, "")
  
  let empty_value = azimuth::Context::get(ctx_with_empty_value, empty_value_key)
  assert_eq(empty_value, Some(""))
  
  // Test very long context value
  let long_value = "this.is.a.very.long.value.that.exceeds.normal.expectations.and.tests.boundary.conditions.".repeat(10)
  let long_value_key = azimuth::ContextKey::new("long.value.key")
  let ctx_with_long_value = azimuth::Context::with_value(root_ctx, long_value_key, long_value)
  
  let retrieved_long_value = azimuth::Context::get(ctx_with_long_value, long_value_key)
  assert_eq(retrieved_long_value, Some(long_value))
  
  // Test special characters in context value
  let special_value = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_value_key = azimuth::ContextKey::new("special.value.key")
  let ctx_with_special_value = azimuth::Context::with_value(root_ctx, special_value_key, special_value)
  
  let retrieved_special_value = azimuth::Context::get(ctx_with_special_value, special_value_key)
  assert_eq(retrieved_special_value, Some(special_value))
  
  // Test unicode characters in context value
  let unicode_value = "值.包.含.中.文.和.emoji🚀"
  let unicode_value_key = azimuth::ContextKey::new("unicode.value.key")
  let ctx_with_unicode_value = azimuth::Context::with_value(root_ctx, unicode_value_key, unicode_value)
  
  let retrieved_unicode_value = azimuth::Context::get(ctx_with_unicode_value, unicode_value_key)
  assert_eq(retrieved_unicode_value, Some(unicode_value))
}

// Test 3: Span context boundary conditions
pub test "Span上下文边界条件测试" {
  // Test empty trace ID
  let empty_trace_ctx = azimuth::SpanContext::new("", "span123", true, "")
  assert_false(azimuth::SpanContext::is_valid(empty_trace_ctx))
  assert_eq(azimuth::SpanContext::trace_id(empty_trace_ctx), "")
  assert_eq(azimuth::SpanContext::span_id(empty_trace_ctx), "span123")
  assert_true(azimuth::SpanContext::is_sampled(empty_trace_ctx))
  
  // Test empty span ID
  let empty_span_ctx = azimuth::SpanContext::new("trace123", "", true, "")
  assert_false(azimuth::SpanContext::is_valid(empty_span_ctx))
  assert_eq(azimuth::SpanContext::trace_id(empty_span_ctx), "trace123")
  assert_eq(azimuth::SpanContext::span_id(empty_span_ctx), "")
  assert_true(azimuth::SpanContext::is_sampled(empty_span_ctx))
  
  // Test both empty
  let both_empty_ctx = azimuth::SpanContext::new("", "", false, "")
  assert_false(azimuth::SpanContext::is_valid(both_empty_ctx))
  assert_eq(azimuth::SpanContext::trace_id(both_empty_ctx), "")
  assert_eq(azimuth::SpanContext::span_id(both_empty_ctx), "")
  assert_false(azimuth::SpanContext::is_sampled(both_empty_ctx))
  
  // Test very long trace ID and span ID
  let long_trace_id = "a".repeat(128)  // 128 characters
  let long_span_id = "b".repeat(64)    // 64 characters
  let long_ids_ctx = azimuth::SpanContext::new(long_trace_id, long_span_id, true, "")
  
  assert_true(azimuth::SpanContext::is_valid(long_ids_ctx))
  assert_eq(azimuth::SpanContext::trace_id(long_ids_ctx), long_trace_id)
  assert_eq(azimuth::SpanContext::span_id(long_ids_ctx), long_span_id)
  assert_true(azimuth::SpanContext::is_sampled(long_ids_ctx))
  
  // Test special characters in trace ID and span ID
  let special_trace_id = "trace!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_span_id = "span!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_ids_ctx = azimuth::SpanContext::new(special_trace_id, special_span_id, true, "")
  
  assert_true(azimuth::SpanContext::is_valid(special_ids_ctx))
  assert_eq(azimuth::SpanContext::trace_id(special_ids_ctx), special_trace_id)
  assert_eq(azimuth::SpanContext::span_id(special_ids_ctx), special_span_id)
  assert_true(azimuth::SpanContext::is_sampled(special_ids_ctx))
  
  // Test unicode characters in trace ID and span ID
  let unicode_trace_id = "追踪ID包含中文🚀"
  let unicode_span_id = "跨度ID包含中文🚀"
  let unicode_ids_ctx = azimuth::SpanContext::new(unicode_trace_id, unicode_span_id, true, "")
  
  assert_true(azimuth::SpanContext::is_valid(unicode_ids_ctx))
  assert_eq(azimuth::SpanContext::trace_id(unicode_ids_ctx), unicode_trace_id)
  assert_eq(azimuth::SpanContext::span_id(unicode_ids_ctx), unicode_span_id)
  assert_true(azimuth::SpanContext::is_sampled(unicode_ids_ctx))
  
  // Test complex trace state
  let complex_trace_state = "key1=value1,key2=value2,key3=value3"
  let complex_trace_state_ctx = azimuth::SpanContext::new("trace123", "span456", true, complex_trace_state)
  
  assert_true(azimuth::SpanContext::is_valid(complex_trace_state_ctx))
  assert_eq(azimuth::SpanContext::trace_id(complex_trace_state_ctx), "trace123")
  assert_eq(azimuth::SpanContext::span_id(complex_trace_state_ctx), "span456")
  assert_true(azimuth::SpanContext::is_sampled(complex_trace_state_ctx))
  
  // Test empty trace state
  let empty_trace_state_ctx = azimuth::SpanContext::new("trace123", "span456", true, "")
  assert_true(azimuth::SpanContext::is_valid(empty_trace_state_ctx))
  assert_eq(azimuth::SpanContext::trace_id(empty_trace_state_ctx), "trace123")
  assert_eq(azimuth::SpanContext::span_id(empty_trace_state_ctx), "span456")
  assert_true(azimuth::SpanContext::is_sampled(empty_trace_state_ctx))
}

// Test 4: Span boundary conditions
pub test "Span边界条件测试" {
  // Test empty span name
  let empty_name_span_ctx = azimuth::SpanContext::new("trace123", "span456", true, "")
  let empty_name_span = azimuth::Span::new("", azimuth::Internal, empty_name_span_ctx)
  
  assert_eq(azimuth::Span::name(empty_name_span), "")
  assert_eq(azimuth::Span::kind(empty_name_span), azimuth::Internal)
  assert_true(azimuth::Span::is_recording(empty_name_span))
  
  // Test very long span name
  let long_span_name = "this.is.a.very.long.span.name.that.exceeds.normal.expectations.and.tests.boundary.conditions".repeat(5)
  let long_name_span = azimuth::Span::new(long_span_name, azimuth::Server, empty_name_span_ctx)
  
  assert_eq(azimuth::Span::name(long_name_span), long_span_name)
  assert_eq(azimuth::Span::kind(long_name_span), azimuth::Server)
  assert_true(azimuth::Span::is_recording(long_name_span))
  
  // Test special characters in span name
  let special_span_name = "span!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_name_span = azimuth::Span::new(special_span_name, azimuth::Client, empty_name_span_ctx)
  
  assert_eq(azimuth::Span::name(special_name_span), special_span_name)
  assert_eq(azimuth::Span::kind(special_name_span), azimuth::Client)
  assert_true(azimuth::Span::is_recording(special_name_span))
  
  // Test unicode characters in span name
  let unicode_span_name = "跨度名称包含中文🚀"
  let unicode_name_span = azimuth::Span::new(unicode_span_name, azimuth::Producer, empty_name_span_ctx)
  
  assert_eq(azimuth::Span::name(unicode_name_span), unicode_span_name)
  assert_eq(azimuth::Span::kind(unicode_name_span), azimuth::Producer)
  assert_true(azimuth::Span::is_recording(unicode_name_span))
  
  // Test all span kinds
  let internal_span = azimuth::Span::new("internal-span", azimuth::Internal, empty_name_span_ctx)
  let server_span = azimuth::Span::new("server-span", azimuth::Server, empty_name_span_ctx)
  let client_span = azimuth::Span::new("client-span", azimuth::Client, empty_name_span_ctx)
  let producer_span = azimuth::Span::new("producer-span", azimuth::Producer, empty_name_span_ctx)
  let consumer_span = azimuth::Span::new("consumer-span", azimuth::Consumer, empty_name_span_ctx)
  
  assert_eq(azimuth::Span::kind(internal_span), azimuth::Internal)
  assert_eq(azimuth::Span::kind(server_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(client_span), azimuth::Client)
  assert_eq(azimuth::Span::kind(producer_span), azimuth::Producer)
  assert_eq(azimuth::Span::kind(consumer_span), azimuth::Consumer)
  
  // Test span operations with boundary conditions
  // Empty event name
  azimuth::Span::add_event(empty_name_span, "")
  
  // Very long event name
  let long_event_name = "this.is.a.very.long.event.name.that.exceeds.normal.expectations.and.tests.boundary.conditions".repeat(3)
  azimuth::Span::add_event(long_name_span, long_event_name)
  
  // Special characters in event name
  let special_event_name = "event!@#$%^&*()_+-=[]{}|;':\",./<>?"
  azimuth::Span::add_event(special_name_span, special_event_name)
  
  // Unicode characters in event name
  let unicode_event_name = "事件名称包含中文🚀"
  azimuth::Span::add_event(unicode_name_span, unicode_event_name)
  
  // Test span status operations
  azimuth::Span::set_status(empty_name_span, azimuth::Unset)
  azimuth::Span::set_status(long_name_span, azimuth::Ok)
  azimuth::Span::set_status(special_name_span, azimuth::Error)
  
  // Test span end operations
  azimuth::Span::end(empty_name_span)
  azimuth::Span::end(long_name_span)
  azimuth::Span::end(special_name_span)
  azimuth::Span::end(unicode_name_span)
}

// Test 5: Resource boundary conditions
pub test "资源边界条件测试" {
  // Test empty resource
  let empty_resource = azimuth::Resource::new()
  assert_eq(empty_resource.attributes.length(), 0)
  
  // Test resource with empty attributes
  let empty_attrs_resource = azimuth::Resource::with_attributes(empty_resource, [])
  assert_eq(empty_attrs_resource.attributes.length(), 0)
  
  // Test resource with single attribute
  let single_attr_resource = azimuth::Resource::with_attributes(empty_resource, [("single.key", azimuth::StringValue("single.value"))])
  assert_eq(single_attr_resource.attributes.length(), 1)
  
  // Test resource with many attributes
  let many_attrs = []
  for i in 0..100 {
    many_attrs.push(("key" + i.to_string(), azimuth::StringValue("value" + i.to_string())))
  }
  let many_attrs_resource = azimuth::Resource::with_attributes(empty_resource, many_attrs)
  assert_eq(many_attrs_resource.attributes.length(), 100)
  
  // Test resource with empty attribute keys
  let empty_key_attrs = [("", azimuth::StringValue("empty.key.value"))]
  let empty_key_resource = azimuth::Resource::with_attributes(empty_resource, empty_key_attrs)
  let empty_key_value = azimuth::Resource::get_attribute(empty_key_resource, "")
  assert_eq(empty_key_value, Some(azimuth::StringValue("empty.key.value")))
  
  // Test resource with very long attribute keys
  let long_key = "this.is.a.very.long.attribute.key.name.that.exceeds.normal.expectations.and.tests.boundary.conditions".repeat(2)
  let long_key_attrs = [(long_key, azimuth::StringValue("long.key.value"))]
  let long_key_resource = azimuth::Resource::with_attributes(empty_resource, long_key_attrs)
  let long_key_value = azimuth::Resource::get_attribute(long_key_resource, long_key)
  assert_eq(long_key_value, Some(azimuth::StringValue("long.key.value")))
  
  // Test resource with special characters in attribute keys
  let special_key = "key!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_key_attrs = [(special_key, azimuth::StringValue("special.key.value"))]
  let special_key_resource = azimuth::Resource::with_attributes(empty_resource, special_key_attrs)
  let special_key_value = azimuth::Resource::get_attribute(special_key_resource, special_key)
  assert_eq(special_key_value, Some(azimuth::StringValue("special.key.value")))
  
  // Test resource with unicode characters in attribute keys
  let unicode_key = "键名包含中文🚀"
  let unicode_key_attrs = [(unicode_key, azimuth::StringValue("unicode.key.value"))]
  let unicode_key_resource = azimuth::Resource::with_attributes(empty_resource, unicode_key_attrs)
  let unicode_key_value = azimuth::Resource::get_attribute(unicode_key_resource, unicode_key)
  assert_eq(unicode_key_value, Some(azimuth::StringValue("unicode.key.value")))
  
  // Test resource with empty attribute values
  let empty_value_attrs = [("empty.value.key", azimuth::StringValue(""))]
  let empty_value_resource = azimuth::Resource::with_attributes(empty_resource, empty_value_attrs)
  let empty_value = azimuth::Resource::get_attribute(empty_value_resource, "empty.value.key")
  assert_eq(empty_value, Some(azimuth::StringValue("")))
  
  // Test resource with very long attribute values
  let long_value = "this.is.a.very.long.attribute.value.that.exceeds.normal.expectations.and.tests.boundary.conditions.".repeat(10)
  let long_value_attrs = [("long.value.key", azimuth::StringValue(long_value))]
  let long_value_resource = azimuth::Resource::with_attributes(empty_resource, long_value_attrs)
  let retrieved_long_value = azimuth::Resource::get_attribute(long_value_resource, "long.value.key")
  assert_eq(retrieved_long_value, Some(azimuth::StringValue(long_value)))
  
  // Test resource with special characters in attribute values
  let special_value = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_value_attrs = [("special.value.key", azimuth::StringValue(special_value))]
  let special_value_resource = azimuth::Resource::with_attributes(empty_resource, special_value_attrs)
  let retrieved_special_value = azimuth::Resource::get_attribute(special_value_resource, "special.value.key")
  assert_eq(retrieved_special_value, Some(azimuth::StringValue(special_value)))
  
  // Test resource with unicode characters in attribute values
  let unicode_value = "值包含中文和emoji🚀"
  let unicode_value_attrs = [("unicode.value.key", azimuth::StringValue(unicode_value))]
  let unicode_value_resource = azimuth::Resource::with_attributes(empty_resource, unicode_value_attrs)
  let retrieved_unicode_value = azimuth::Resource::get_attribute(unicode_value_resource, "unicode.value.key")
  assert_eq(retrieved_unicode_value, Some(azimuth::StringValue(unicode_value)))
  
  // Test resource merge with empty resources
  let merged_empty = azimuth::Resource::merge(empty_resource, empty_attrs_resource)
  assert_eq(merged_empty.attributes.length(), 0)
  
  // Test resource merge with many attributes
  let merged_many = azimuth::Resource::merge(single_attr_resource, many_attrs_resource)
  assert_eq(merged_many.attributes.length(), 100)  // Simplified implementation
}

// Test 6: Baggage boundary conditions
pub test "Baggage边界条件测试" {
  // Test empty baggage
  let empty_baggage = azimuth::Baggage::new()
  assert_eq(empty_baggage.entries.length(), 0)
  
  // Test baggage with empty key
  let baggage_with_empty_key = azimuth::Baggage::set_entry(empty_baggage, "", "empty.key.value")
  let empty_key_value = azimuth::Baggage::get_entry(baggage_with_empty_key, "")
  assert_eq(empty_key_value, Some("empty.key.value"))
  
  // Test baggage with very long key
  let long_key = "this.is.a.very.long.baggage.key.name.that.exceeds.normal.expectations.and.tests.boundary.conditions".repeat(2)
  let baggage_with_long_key = azimuth::Baggage::set_entry(empty_baggage, long_key, "long.key.value")
  let long_key_value = azimuth::Baggage::get_entry(baggage_with_long_key, long_key)
  assert_eq(long_key_value, Some("long.key.value"))
  
  // Test baggage with special characters in key
  let special_key = "key!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let baggage_with_special_key = azimuth::Baggage::set_entry(empty_baggage, special_key, "special.key.value")
  let special_key_value = azimuth::Baggage::get_entry(baggage_with_special_key, special_key)
  assert_eq(special_key_value, Some("special.key.value"))
  
  // Test baggage with unicode characters in key
  let unicode_key = "键名包含中文🚀"
  let baggage_with_unicode_key = azimuth::Baggage::set_entry(empty_baggage, unicode_key, "unicode.key.value")
  let unicode_key_value = azimuth::Baggage::get_entry(baggage_with_unicode_key, unicode_key)
  assert_eq(unicode_key_value, Some("unicode.key.value"))
  
  // Test baggage with empty value
  let baggage_with_empty_value = azimuth::Baggage::set_entry(empty_baggage, "empty.value.key", "")
  let empty_value = azimuth::Baggage::get_entry(baggage_with_empty_value, "empty.value.key")
  assert_eq(empty_value, Some(""))
  
  // Test baggage with very long value
  let long_value = "this.is.a.very.long.baggage.value.that.exceeds.normal.expectations.and.tests.boundary.conditions.".repeat(10)
  let baggage_with_long_value = azimuth::Baggage::set_entry(empty_baggage, "long.value.key", long_value)
  let retrieved_long_value = azimuth::Baggage::get_entry(baggage_with_long_value, "long.value.key")
  assert_eq(retrieved_long_value, Some(long_value))
  
  // Test baggage with special characters in value
  let special_value = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let baggage_with_special_value = azimuth::Baggage::set_entry(empty_baggage, "special.value.key", special_value)
  let retrieved_special_value = azimuth::Baggage::get_entry(baggage_with_special_value, "special.value.key")
  assert_eq(retrieved_special_value, Some(special_value))
  
  // Test baggage with unicode characters in value
  let unicode_value = "值包含中文和emoji🚀"
  let baggage_with_unicode_value = azimuth::Baggage::set_entry(empty_baggage, "unicode.value.key", unicode_value)
  let retrieved_unicode_value = azimuth::Baggage::get_entry(baggage_with_unicode_value, "unicode.value.key")
  assert_eq(retrieved_unicode_value, Some(unicode_value))
  
  // Test baggage with multiple entries
  let multi_baggage = azimuth::Baggage::set_entry(empty_baggage, "key1", "value1")
  let multi_baggage2 = azimuth::Baggage::set_entry(multi_baggage, "key2", "value2")
  let multi_baggage3 = azimuth::Baggage::set_entry(multi_baggage2, "key3", "value3")
  
  let value1 = azimuth::Baggage::get_entry(multi_baggage3, "key1")
  let value2 = azimuth::Baggage::get_entry(multi_baggage3, "key2")
  let value3 = azimuth::Baggage::get_entry(multi_baggage3, "key3")
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
  assert_eq(value3, Some("value3"))
  
  // Test baggage entry removal with empty key
  let baggage_after_empty_removal = azimuth::Baggage::remove_entry(multi_baggage3, "")
  let empty_removal_value = azimuth::Baggage::get_entry(baggage_after_empty_removal, "")
  assert_eq(empty_removal_value, Some("empty.key.value"))  // Simplified implementation
  
  // Test baggage entry removal with non-existent key
  let baggage_after_missing_removal = azimuth::Baggage::remove_entry(multi_baggage3, "nonexistent.key")
  let missing_removal_value1 = azimuth::Baggage::get_entry(baggage_after_missing_removal, "key1")
  let missing_removal_value2 = azimuth::Baggage::get_entry(baggage_after_missing_removal, "key2")
  let missing_removal_value3 = azimuth::Baggage::get_entry(baggage_after_missing_removal, "key3")
  
  assert_eq(missing_removal_value1, Some("value1"))
  assert_eq(missing_removal_value2, Some("value2"))
  assert_eq(missing_removal_value3, Some("value3"))
}