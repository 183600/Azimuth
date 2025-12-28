// Context Chain Propagation Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for context chain propagation scenarios

test "context chain basic propagation" {
  // Test basic context chain propagation
  let root_ctx = Context::root()
  
  // Level 1: Add correlation ID
  let level1_key = ContextKey::new("correlation.id")
  let level1_ctx = Context::with_value(root_ctx, level1_key, "corr-12345")
  
  // Level 2: Add user context
  let level2_key = ContextKey::new("user.id")
  let level2_ctx = Context::with_value(level1_ctx, level2_key, "user-67890")
  
  // Level 3: Add session context
  let level3_key = ContextKey::new("session.id")
  let level3_ctx = Context::with_value(level2_ctx, level3_key, "sess-abcdef")
  
  // Verify propagation through chain
  let correlation_id = Context::get(level3_ctx, level1_key)
  let user_id = Context::get(level3_ctx, level2_key)
  let session_id = Context::get(level3_ctx, level3_key)
  
  assert_eq(correlation_id, Some("corr-12345"))
  assert_eq(user_id, Some("user-67890"))
  assert_eq(session_id, Some("sess-abcdef"))
  
  // Verify root context doesn't have the values
  let root_correlation = Context::get(root_ctx, level1_key)
  let root_user = Context::get(root_ctx, level2_key)
  let root_session = Context::get(root_ctx, level3_key)
  
  assert_eq(root_correlation, None)
  assert_eq(root_user, None)
  assert_eq(root_session, None)
}

test "context chain with trace context" {
  // Test context chain propagation with trace context
  let root_ctx = Context::root()
  
  // Create span context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // Add trace context to chain
  let trace_key = ContextKey::new("trace.context")
  let trace_ctx = Context::with_value(
    root_ctx, 
    trace_key, 
    "trace-id=" + trace_id + ";span-id=" + span_id
  )
  
  // Add business context to chain
  let business_key = ContextKey::new("business.operation")
  let business_ctx = Context::with_value(trace_ctx, business_key, "process.payment")
  
  // Add user context to chain
  let user_key = ContextKey::new("user.context")
  let user_ctx = Context::with_value(business_ctx, user_key, "user-123;role=admin")
  
  // Verify all context is preserved
  let trace_context = Context::get(user_ctx, trace_key)
  let business_operation = Context::get(user_ctx, business_key)
  let user_context = Context::get(user_ctx, user_key)
  
  assert_eq(trace_context, Some("trace-id=0af7651916cd43dd8448eb211c80319c;span-id=b7ad6b7169203331"))
  assert_eq(business_operation, Some("process.payment"))
  assert_eq(user_context, Some("user-123;role=admin"))
}

test "context chain with baggage propagation" {
  // Test context chain propagation with baggage
  let root_ctx = Context::root()
  
  // Level 1: Add initial baggage
  let baggage1_key = ContextKey::new("baggage")
  let baggage1_ctx = Context::with_value(
    root_ctx, 
    baggage1_key, 
    "service.name=api-gateway;service.version=1.0.0"
  )
  
  // Level 2: Add more baggage items
  let baggage2_key = ContextKey::new("additional.baggage")
  let baggage2_ctx = Context::with_value(
    baggage1_ctx, 
    baggage2_key, 
    "request.id=req-12345;client.ip=192.168.1.100"
  )
  
  // Level 3: Add user-specific baggage
  let baggage3_key = ContextKey::new("user.baggage")
  let baggage3_ctx = Context::with_value(
    baggage2_ctx, 
    baggage3_key, 
    "user.id=user-67890;user.tier=premium"
  )
  
  // Verify baggage propagation
  let initial_baggage = Context::get(baggage3_ctx, baggage1_key)
  let additional_baggage = Context::get(baggage3_ctx, baggage2_key)
  let user_baggage = Context::get(baggage3_ctx, baggage3_key)
  
  assert_eq(initial_baggage, Some("service.name=api-gateway;service.version=1.0.0"))
  assert_eq(additional_baggage, Some("request.id=req-12345;client.ip=192.168.1.100"))
  assert_eq(user_baggage, Some("user.id=user-67890;user.tier=premium"))
}

test "context chain branching scenarios" {
  // Test context chain branching scenarios
  let root_ctx = Context::root()
  
  // Common base context
  let common_key = ContextKey::new("common.context")
  let base_ctx = Context::with_value(root_ctx, common_key, "base.value")
  
  // Branch 1: API service
  let api_key = ContextKey::new("api.context")
  let api_ctx = Context::with_value(base_ctx, api_key, "api.service")
  
  // Branch 2: Database service
  let db_key = ContextKey::new("db.context")
  let db_ctx = Context::with_value(base_ctx, db_key, "database.service")
  
  // Branch 3: Cache service
  let cache_key = ContextKey::new("cache.context")
  let cache_ctx = Context::with_value(base_ctx, cache_key, "cache.service")
  
  // Extend branch 1
  let api_operation_key = ContextKey::new("api.operation")
  let api_extended_ctx = Context::with_value(api_ctx, api_operation_key, "get.user.profile")
  
  // Extend branch 2
  let db_operation_key = ContextKey::new("db.operation")
  let db_extended_ctx = Context::with_value(db_ctx, db_operation_key, "select.user.data")
  
  // Extend branch 3
  let cache_operation_key = ContextKey::new("cache.operation")
  let cache_extended_ctx = Context::with_value(cache_ctx, cache_operation_key, "get.user.cache")
  
  // Verify branch 1
  let api_common = Context::get(api_extended_ctx, common_key)
  let api_context = Context::get(api_extended_ctx, api_key)
  let api_operation = Context::get(api_extended_ctx, api_operation_key)
  
  assert_eq(api_common, Some("base.value"))
  assert_eq(api_context, Some("api.service"))
  assert_eq(api_operation, Some("get.user.profile"))
  
  // Verify branch 2
  let db_common = Context::get(db_extended_ctx, common_key)
  let db_context = Context::get(db_extended_ctx, db_key)
  let db_operation = Context::get(db_extended_ctx, db_operation_key)
  
  assert_eq(db_common, Some("base.value"))
  assert_eq(db_context, Some("database.service"))
  assert_eq(db_operation, Some("select.user.data"))
  
  // Verify branch 3
  let cache_common = Context::get(cache_extended_ctx, common_key)
  let cache_context = Context::get(cache_extended_ctx, cache_key)
  let cache_operation = Context::get(cache_extended_ctx, cache_operation_key)
  
  assert_eq(cache_common, Some("base.value"))
  assert_eq(cache_context, Some("cache.service"))
  assert_eq(cache_operation, Some("get.user.cache"))
}

test "context chain with special characters and encoding" {
  // Test context chain propagation with special characters and encoding
  let root_ctx = Context::root()
  
  // Add context with special characters
  let special_key = ContextKey::new("special.context")
  let special_ctx = Context::with_value(
    root_ctx, 
    special_key, 
    "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"
  )
  
  // Add Unicode context
  let unicode_key = ContextKey::new("unicode.context")
  let unicode_ctx = Context::with_value(special_ctx, unicode_key, "Unicode: 测试数据 αβγδεζ")
  
  // Add emoji context
  let emoji_key = ContextKey::new("emoji.context")
  let emoji_ctx = Context::with_value(unicode_ctx, emoji_key, "Emoji: 🚀📊🔍💡🎯")
  
  // Add JSON context
  let json_key = ContextKey::new("json.context")
  let json_ctx = Context::with_value(
    emoji_ctx, 
    json_key, 
    "{\"user\":\"test\",\"id\":123,\"active\":true}"
  )
  
  // Add URL context
  let url_key = ContextKey::new("url.context")
  let url_ctx = Context::with_value(
    json_ctx, 
    url_key, 
    "https://example.com/path?param=value&other=test#section"
  )
  
  // Verify all special context is preserved
  let special_context = Context::get(url_ctx, special_key)
  let unicode_context = Context::get(url_ctx, unicode_key)
  let emoji_context = Context::get(url_ctx, emoji_key)
  let json_context = Context::get(url_ctx, json_key)
  let url_context = Context::get(url_ctx, url_key)
  
  assert_eq(special_context, Some("Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"))
  assert_eq(unicode_context, Some("Unicode: 测试数据 αβγδεζ"))
  assert_eq(emoji_context, Some("Emoji: 🚀📊🔍💡🎯"))
  assert_eq(json_context, Some("{\"user\":\"test\",\"id\":123,\"active\":true}"))
  assert_eq(url_context, Some("https://example.com/path?param=value&other=test#section"))
}

test "context chain deep nesting scenarios" {
  // Test context chain with deep nesting scenarios
  let root_ctx = Context::root()
  
  // Create deeply nested context chain (10 levels)
  let level1 = Context::with_value(root_ctx, ContextKey::new("level1"), "value1")
  let level2 = Context::with_value(level1, ContextKey::new("level2"), "value2")
  let level3 = Context::with_value(level2, ContextKey::new("level3"), "value3")
  let level4 = Context::with_value(level3, ContextKey::new("level4"), "value4")
  let level5 = Context::with_value(level4, ContextKey::new("level5"), "value5")
  let level6 = Context::with_value(level5, ContextKey::new("level6"), "value6")
  let level7 = Context::with_value(level6, ContextKey::new("level7"), "value7")
  let level8 = Context::with_value(level7, ContextKey::new("level8"), "value8")
  let level9 = Context::with_value(level8, ContextKey::new("level9"), "value9")
  let level10 = Context::with_value(level9, ContextKey::new("level10"), "value10")
  
  // Verify all levels are accessible from the deepest level
  let value1 = Context::get(level10, ContextKey::new("level1"))
  let value2 = Context::get(level10, ContextKey::new("level2"))
  let value3 = Context::get(level10, ContextKey::new("level3"))
  let value4 = Context::get(level10, ContextKey::new("level4"))
  let value5 = Context::get(level10, ContextKey::new("level5"))
  let value6 = Context::get(level10, ContextKey::new("level6"))
  let value7 = Context::get(level10, ContextKey::new("level7"))
  let value8 = Context::get(level10, ContextKey::new("level8"))
  let value9 = Context::get(level10, ContextKey::new("level9"))
  let value10 = Context::get(level10, ContextKey::new("level10"))
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
  assert_eq(value3, Some("value3"))
  assert_eq(value4, Some("value4"))
  assert_eq(value5, Some("value5"))
  assert_eq(value6, Some("value6"))
  assert_eq(value7, Some("value7"))
  assert_eq(value8, Some("value8"))
  assert_eq(value9, Some("value9"))
  assert_eq(value10, Some("value10"))
  
  // Verify reverse access doesn't work
  let reverse_value = Context::get(level1, ContextKey::new("level10"))
  assert_eq(reverse_value, None)
}

test "context chain with same key overrides" {
  // Test context chain behavior when same key is used multiple times
  let root_ctx = Context::root()
  
  // Use same key at different levels
  let shared_key = ContextKey::new("shared.key")
  
  let level1 = Context::with_value(root_ctx, shared_key, "level1.value")
  let level2 = Context::with_value(level1, shared_key, "level2.value")
  let level3 = Context::with_value(level2, shared_key, "level3.value")
  
  // Verify the most recent value is returned
  let value_from_level3 = Context::get(level3, shared_key)
  assert_eq(value_from_level3, Some("level3.value"))
  
  // Verify intermediate contexts have their own values
  let value_from_level2 = Context::get(level2, shared_key)
  assert_eq(value_from_level2, Some("level2.value"))
  
  let value_from_level1 = Context::get(level1, shared_key)
  assert_eq(value_from_level1, Some("level1.value"))
  
  // Add different keys to test mixed scenarios
  let different_key = ContextKey::new("different.key")
  let level4 = Context::with_value(level3, different_key, "different.value")
  
  // Verify both keys are accessible
  let shared_value = Context::get(level4, shared_key)
  let different_value = Context::get(level4, different_key)
  
  assert_eq(shared_value, Some("level3.value"))
  assert_eq(different_value, Some("different.value"))
}

test "context chain with empty and null values" {
  // Test context chain with empty and null values
  let root_ctx = Context::root()
  
  // Add empty string value
  let empty_key = ContextKey::new("empty.value")
  let empty_ctx = Context::with_value(root_ctx, empty_key, "")
  
  // Add whitespace value
  let whitespace_key = ContextKey::new("whitespace.value")
  let whitespace_ctx = Context::with_value(empty_ctx, whitespace_key, "   ")
  
  // Add normal value
  let normal_key = ContextKey::new("normal.value")
  let normal_ctx = Context::with_value(whitespace_ctx, normal_key, "normal.value")
  
  // Verify all values are preserved
  let empty_value = Context::get(normal_ctx, empty_key)
  let whitespace_value = Context::get(normal_ctx, whitespace_key)
  let normal_value = Context::get(normal_ctx, normal_key)
  
  assert_eq(empty_value, Some(""))
  assert_eq(whitespace_value, Some("   "))
  assert_eq(normal_value, Some("normal.value"))
  
  // Test with keys that look similar but are different
  let similar_key1 = ContextKey::new("similar.key")
  let similar_key2 = ContextKey::new("similar.key ")
  let similar_key3 = ContextKey::new(" similar.key")
  
  let similar_ctx1 = Context::with_value(normal_ctx, similar_key1, "value1")
  let similar_ctx2 = Context::with_value(similar_ctx1, similar_key2, "value2")
  let similar_ctx3 = Context::with_value(similar_ctx2, similar_key3, "value3")
  
  // Verify similar but different keys work independently
  let similar_value1 = Context::get(similar_ctx3, similar_key1)
  let similar_value2 = Context::get(similar_ctx3, similar_key2)
  let similar_value3 = Context::get(similar_ctx3, similar_key3)
  
  assert_eq(similar_value1, Some("value1"))
  assert_eq(similar_value2, Some("value2"))
  assert_eq(similar_value3, Some("value3"))
}