// Context Chain Propagation Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for context chain propagation scenarios

test "basic context chain propagation" {
  // Create root context
  let root_ctx = Context::root()
  
  // Create first level context
  let key1 = ContextKey::new("chain.level1")
  let ctx1 = Context::with_value(root_ctx, key1, "level1_value")
  
  // Create second level context
  let key2 = ContextKey::new("chain.level2")
  let ctx2 = Context::with_value(ctx1, key2, "level2_value")
  
  // Create third level context
  let key3 = ContextKey::new("chain.level3")
  let ctx3 = Context::with_value(ctx2, key3, "level3_value")
  
  // Test value retrieval at different levels
  let level1_from_ctx1 = Context::get(ctx1, key1)
  let level2_from_ctx2 = Context::get(ctx2, key2)
  let level3_from_ctx3 = Context::get(ctx3, key3)
  
  assert_eq(level1_from_ctx1, Some("level1_value"))
  assert_eq(level2_from_ctx2, Some("level2_value"))
  assert_eq(level3_from_ctx3, Some("level3_value"))
  
  // Test inheritance in chain
  let level1_from_ctx2 = Context::get(ctx2, key1)
  let level2_from_ctx3 = Context::get(ctx3, key2)
  let level1_from_ctx3 = Context::get(ctx3, key1)
  
  assert_eq(level1_from_ctx2, Some("level1_value"))
  assert_eq(level2_from_ctx3, Some("level2_value"))
  assert_eq(level1_from_ctx3, Some("level1_value"))
}

test "context chain with key overriding" {
  // Create base context
  let base_ctx = Context::root()
  let shared_key = ContextKey::new("shared.key")
  let base_ctx_with_shared = Context::with_value(base_ctx, shared_key, "base_value")
  
  // Create first override
  let ctx1 = Context::with_value(base_ctx_with_shared, shared_key, "override1_value")
  let unique_key1 = ContextKey::new("unique.key1")
  let ctx1_with_unique = Context::with_value(ctx1, unique_key1, "unique1_value")
  
  // Create second override
  let ctx2 = Context::with_value(ctx1_with_unique, shared_key, "override2_value")
  let unique_key2 = ContextKey::new("unique.key2")
  let ctx2_with_unique = Context::with_value(ctx2, unique_key2, "unique2_value")
  
  // Create third override
  let ctx3 = Context::with_value(ctx2_with_unique, shared_key, "override3_value")
  let unique_key3 = ContextKey::new("unique.key3")
  let ctx3_with_unique = Context::with_value(ctx3, unique_key3, "unique3_value")
  
  // Test key overriding behavior
  let shared_from_base = Context::get(base_ctx_with_shared, shared_key)
  let shared_from_ctx1 = Context::get(ctx1_with_unique, shared_key)
  let shared_from_ctx2 = Context::get(ctx2_with_unique, shared_key)
  let shared_from_ctx3 = Context::get(ctx3_with_unique, shared_key)
  
  assert_eq(shared_from_base, Some("base_value"))
  assert_eq(shared_from_ctx1, Some("override1_value"))
  assert_eq(shared_from_ctx2, Some("override2_value"))
  assert_eq(shared_from_ctx3, Some("override3_value"))
  
  // Test unique keys are preserved
  let unique1_from_ctx1 = Context::get(ctx1_with_unique, unique_key1)
  let unique1_from_ctx2 = Context::get(ctx2_with_unique, unique_key1)
  let unique1_from_ctx3 = Context::get(ctx3_with_unique, unique_key1)
  
  let unique2_from_ctx2 = Context::get(ctx2_with_unique, unique_key2)
  let unique2_from_ctx3 = Context::get(ctx3_with_unique, unique_key2)
  
  let unique3_from_ctx3 = Context::get(ctx3_with_unique, unique_key3)
  
  assert_eq(unique1_from_ctx1, Some("unique1_value"))
  assert_eq(unique1_from_ctx2, Some("unique1_value"))
  assert_eq(unique1_from_ctx3, Some("unique1_value"))
  
  assert_eq(unique2_from_ctx2, Some("unique2_value"))
  assert_eq(unique2_from_ctx3, Some("unique2_value"))
  
  assert_eq(unique3_from_ctx3, Some("unique3_value"))
}

test "context chain with baggage propagation" {
  // Create root context with baggage
  let root_ctx = Context::root()
  let baggage_key = ContextKey::new("baggage")
  let root_with_baggage = Context::with_value(root_ctx, baggage_key, "user.id=123,session.id=456")
  
  // Create service context with additional baggage
  let service_key = ContextKey::new("service.name")
  let service_ctx = Context::with_value(root_with_baggage, service_key, "auth-service")
  let service_with_baggage = Context::with_value(service_ctx, baggage_key, "user.id=123,session.id=456,operation=authenticate")
  
  // Create operation context
  let operation_key = ContextKey::new("operation.name")
  let operation_ctx = Context::with_value(service_with_baggage, operation_key, "validate-token")
  let operation_with_baggage = Context::with_value(operation_ctx, baggage_key, "user.id=123,session.id=456,operation=authenticate,step=validation")
  
  // Test baggage propagation through chain
  let baggage_from_root = Context::get(root_with_baggage, baggage_key)
  let baggage_from_service = Context::get(service_with_baggage, baggage_key)
  let baggage_from_operation = Context::get(operation_with_baggage, baggage_key)
  
  assert_eq(baggage_from_root, Some("user.id=123,session.id=456"))
  assert_eq(baggage_from_service, Some("user.id=123,session.id=456,operation=authenticate"))
  assert_eq(baggage_from_operation, Some("user.id=123,session.id=456,operation=authenticate,step=validation"))
  
  // Test context inheritance
  let service_from_operation = Context::get(operation_with_baggage, service_key)
  let operation_from_operation = Context::get(operation_with_baggage, operation_key)
  
  assert_eq(service_from_operation, Some("auth-service"))
  assert_eq(operation_from_operation, Some("validate-token"))
}

test "context chain with trace propagation" {
  // Create trace context
  let trace_id = "chain_trace_1234567890abcdef"
  let span_id = "chain_span_1111222233334444"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "env=production")
  
  // Create root context with trace
  let root_ctx = Context::root()
  let trace_key = ContextKey::new("trace.context")
  let root_with_trace = Context::with_value(root_ctx, trace_key, trace_id + ":" + span_id)
  
  // Create service context
  let service_key = ContextKey::new("service.context")
  let service_ctx = Context::with_value(root_with_trace, service_key, "api-gateway")
  let service_with_trace = Context::with_value(service_ctx, trace_key, trace_id + ":" + span_id + ":service=api-gateway")
  
  // Create downstream context
  let downstream_key = ContextKey::new("downstream.context")
  let downstream_ctx = Context::with_value(service_with_trace, downstream_key, "user-service")
  let downstream_with_trace = Context::with_value(downstream_ctx, trace_key, trace_id + ":" + span_id + ":service=api-gateway:downstream=user-service")
  
  // Test trace propagation
  let trace_from_root = Context::get(root_with_trace, trace_key)
  let trace_from_service = Context::get(service_with_trace, trace_key)
  let trace_from_downstream = Context::get(downstream_with_trace, trace_key)
  
  assert_eq(trace_from_root, Some("chain_trace_1234567890abcdef:chain_span_1111222233334444"))
  assert_eq(trace_from_service, Some("chain_trace_1234567890abcdef:chain_span_1111222233334444:service=api-gateway"))
  assert_eq(trace_from_downstream, Some("chain_trace_1234567890abcdef:chain_span_1111222233334444:service=api-gateway:downstream=user-service"))
  
  // Test context chain integrity
  let service_from_downstream = Context::get(downstream_with_trace, service_key)
  let downstream_from_downstream = Context::get(downstream_with_trace, downstream_key)
  
  assert_eq(service_from_downstream, Some("api-gateway"))
  assert_eq(downstream_from_downstream, Some("user-service"))
}

test "context chain with complex data types" {
  // Create context with complex data
  let root_ctx = Context::root()
  
  // Add JSON-like data
  let json_key = ContextKey::new("json.data")
  let json_value = "{\"user\":{\"id\":123,\"name\":\"test\"},\"settings\":{\"theme\":\"dark\",\"lang\":\"en\"}}"
  let ctx_with_json = Context::with_value(root_ctx, json_key, json_value)
  
  // Add array-like data
  let array_key = ContextKey::new("array.data")
  let array_value = "[\"item1\",\"item2\",\"item3\"]"
  let ctx_with_array = Context::with_value(ctx_with_json, array_key, array_value)
  
  // Add numeric data
  let numeric_key = ContextKey::new("numeric.data")
  let numeric_value = "42.5"
  let ctx_with_numeric = Context::with_value(ctx_with_array, numeric_key, numeric_value)
  
  // Add boolean data
  let bool_key = ContextKey::new("bool.data")
  let bool_value = "true"
  let ctx_with_bool = Context::with_value(ctx_with_numeric, bool_key, bool_value)
  
  // Test complex data retrieval
  let json_result = Context::get(ctx_with_bool, json_key)
  let array_result = Context::get(ctx_with_bool, array_key)
  let numeric_result = Context::get(ctx_with_bool, numeric_key)
  let bool_result = Context::get(ctx_with_bool, bool_key)
  
  assert_eq(json_result, Some("{\"user\":{\"id\":123,\"name\":\"test\"},\"settings\":{\"theme\":\"dark\",\"lang\":\"en\"}}"))
  assert_eq(array_result, Some("[\"item1\",\"item2\",\"item3\"]"))
  assert_eq(numeric_result, Some("42.5"))
  assert_eq(bool_result, Some("true"))
  
  // Test partial context access
  let json_from_array = Context::get(ctx_with_array, json_key)
  let array_from_numeric = Context::get(ctx_with_numeric, array_key)
  let numeric_from_bool = Context::get(ctx_with_bool, numeric_key)
  
  assert_eq(json_from_array, Some("{\"user\":{\"id\":123,\"name\":\"test\"},\"settings\":{\"theme\":\"dark\",\"lang\":\"en\"}}"))
  assert_eq(array_from_numeric, Some("[\"item1\",\"item2\",\"item3\"]"))
  assert_eq(numeric_from_bool, Some("42.5"))
}

test "context chain with error handling" {
  // Create context chain with error conditions
  let root_ctx = Context::root()
  
  // Test with empty key
  let empty_key = ContextKey::new("")
  let ctx_with_empty = Context::with_value(root_ctx, empty_key, "empty_key_value")
  let empty_result = Context::get(ctx_with_empty, empty_key)
  assert_eq(empty_result, Some("empty_key_value"))
  
  // Test with empty value
  let normal_key = ContextKey::new("normal.key")
  let ctx_with_empty_value = Context::with_value(root_ctx, normal_key, "")
  let empty_value_result = Context::get(ctx_with_empty_value, normal_key)
  assert_eq(empty_value_result, Some(""))
  
  // Test with special characters
  let special_key = ContextKey::new("special.key!@#$%^&*()")
  let ctx_with_special = Context::with_value(root_ctx, special_key, "special_value")
  let special_result = Context::get(ctx_with_special, special_key)
  assert_eq(special_result, Some("special_value"))
  
  // Test with very long key and value
  let long_key = ContextKey::new("very.long.key." + "x".repeat(100))
  let long_value = "very.long.value." + "y".repeat(100)
  let ctx_with_long = Context::with_value(root_ctx, long_key, long_value)
  let long_result = Context::get(ctx_with_long, long_key)
  assert_eq(long_result, Some("very.long.value." + "y".repeat(100)))
  
  // Test with unicode characters
  let unicode_key = ContextKey::new("unicode.key.测试")
  let unicode_value = "unicode_value_测试"
  let ctx_with_unicode = Context::with_value(root_ctx, unicode_key, unicode_value)
  let unicode_result = Context::get(ctx_with_unicode, unicode_key)
  assert_eq(unicode_result, Some("unicode_value_测试"))
  
  // Test chain with error conditions
  let error_chain_ctx = Context::with_value(ctx_with_empty, normal_key, "chain_value")
  let chain_empty_result = Context::get(error_chain_ctx, empty_key)
  let chain_normal_result = Context::get(error_chain_ctx, normal_key)
  
  assert_eq(chain_empty_result, Some("empty_key_value"))
  assert_eq(chain_normal_result, Some("chain_value"))
}

test "context chain with propagation carriers" {
  // Create context chain with carrier-based propagation
  let root_ctx = Context::root()
  
  // Create carrier for propagation
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // Add context values
  let ctx1 = Context::with_value(root_ctx, ContextKey::new("user.id"), "user123")
  let ctx2 = Context::with_value(ctx1, ContextKey::new("session.id"), "session456")
  let ctx3 = Context::with_value(ctx2, ContextKey::new("request.id"), "req789")
  
  // Inject context into carrier
  CompositePropagator::inject(propagator, ctx3, carrier)
  
  // Extract context in new context chain
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // Build new context chain from extracted context
  let new_ctx1 = Context::with_value(extracted_ctx, ContextKey::new("service.name"), "new-service")
  let new_ctx2 = Context::with_value(new_ctx1, ContextKey::new("operation.name"), "new-operation")
  let new_ctx3 = Context::with_value(new_ctx2, ContextKey::new("correlation.id"), "corr123")
  
  // Test new context chain
  let new_service = Context::get(new_ctx3, ContextKey::new("service.name"))
  let new_operation = Context::get(new_ctx3, ContextKey::new("operation.name"))
  let new_correlation = Context::get(new_ctx3, ContextKey::new("correlation.id"))
  let new_extracted = Context::get(new_ctx3, ContextKey::new("extracted"))
  
  assert_eq(new_service, Some("new-service"))
  assert_eq(new_operation, Some("new-operation"))
  assert_eq(new_correlation, Some("corr123"))
  assert_eq(new_extracted, Some("true"))
  
  // Test round-trip propagation
  let carrier2 = TextMapCarrier::new()
  CompositePropagator::inject(propagator, new_ctx3, carrier2)
  let extracted_ctx2 = CompositePropagator::extract(propagator, carrier2)
  let extracted_value2 = Context::get(extracted_ctx2, ContextKey::new("extracted"))
  assert_eq(extracted_value2, Some("true"))
}

test "context chain performance with deep nesting" {
  // Create deeply nested context chain
  let ctx = Context::root()
  
  // Create 100-level deep context chain
  for i = 0; i < 100; i = i + 1 {
    let key = ContextKey::new("level." + i.to_string())
    ctx = Context::with_value(ctx, key, "value_at_level_" + i.to_string())
  }
  
  // Test retrieval from different levels
  let level_0 = Context::get(ctx, ContextKey::new("level.0"))
  let level_25 = Context::get(ctx, ContextKey::new("level.25"))
  let level_50 = Context::get(ctx, ContextKey::new("level.50"))
  let level_75 = Context::get(ctx, ContextKey::new("level.75"))
  let level_99 = Context::get(ctx, ContextKey::new("level.99"))
  
  assert_eq(level_0, Some("value_at_level_0"))
  assert_eq(level_25, Some("value_at_level_25"))
  assert_eq(level_50, Some("value_at_level_50"))
  assert_eq(level_75, Some("value_at_level_75"))
  assert_eq(level_99, Some("value_at_level_99"))
  
  // Test missing key from deep chain
  let missing_level = Context::get(ctx, ContextKey::new("level.100"))
  assert_eq(missing_level, None)
  
  // Test context modification in deep chain
  let modification_key = ContextKey::new("deep.modification")
  let modified_ctx = Context::with_value(ctx, modification_key, "modified_at_deep_level")
  let modification_result = Context::get(modified_ctx, modification_key)
  assert_eq(modification_result, Some("modified_at_deep_level"))
  
  // Test that original values are still accessible
  let level_after_modification = Context::get(modified_ctx, ContextKey::new("level.50"))
  assert_eq(level_after_modification, Some("value_at_level_50"))
}