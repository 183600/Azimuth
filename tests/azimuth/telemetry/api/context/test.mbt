test "context creation and value storage" {
  // Test basic context creation and value storage/retrieval
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let key = @azimuth.telemetry.api.context.ContextKey[String]("test.key")
  
  // Store and retrieve a value
  let ctx_with_value = @azimuth.telemetry.api.context.Context.with_value(ctx, key, "test_value")
  let retrieved_value = @azimuth.telemetry.api.context.Context.get(ctx_with_value, key)
  
  assert_eq(retrieved_value, Some("test_value"))
  
  // Original context should not have the value
  let original_value = @azimuth.telemetry.api.context.Context.get(ctx, key)
  assert_eq(original_value, None)
}

test "context chaining and immutability" {
  // Test that context is immutable and values can be chained
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let key1 = @azimuth.telemetry.api.context.ContextKey[String]("key1")
  let key2 = @azimuth.telemetry.api.context.ContextKey[Int]("key2")
  
  let ctx1 = @azimuth.telemetry.api.context.Context.with_value(ctx, key1, "value1")
  let ctx2 = @azimuth.telemetry.api.context.Context.with_value(ctx1, key2, 42)
  
  // Both values should be accessible in ctx2
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx2, key1), Some("value1"))
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx2, key2), Some(42))
  
  // ctx1 should only have key1
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx1, key1), Some("value1"))
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx1, key2), None)
}

test "baggage operations" {
  // Test baggage creation and manipulation
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  
  // Add entries to baggage
  let baggage1 = @azimuth.telemetry.api.context.Baggage.set_value(baggage, "user.id", "12345")
  let baggage2 = @azimuth.telemetry.api.context.Baggage.set_value(baggage1, "request.id", "req-67890")
  
  // Store baggage in context
  let ctx_with_baggage = @azimuth.telemetry.api.context.Context.with_baggage(ctx, baggage2)
  let retrieved_baggage = @azimuth.telemetry.api.context.Context.get_baggage(ctx_with_baggage)
  
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(retrieved_baggage, "user.id"), Some("12345"))
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(retrieved_baggage, "request.id"), Some("req-67890"))
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(retrieved_baggage, "nonexistent"), None)
}

test "baggage metadata and properties" {
  // Test baggage with metadata
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  let baggage_with_metadata = @azimuth.telemetry.api.context.Baggage.set_value_with_metadata(
    baggage, 
    "service.version", 
    "1.0.0", 
    Some("production")
  )
  
  let value = @azimuth.telemetry.api.context.Baggage.get_value(baggage_with_metadata, "service.version")
  let metadata = @azimuth.telemetry.api.context.Baggage.get_metadata(baggage_with_metadata, "service.version")
  
  assert_eq(value, Some("1.0.0"))
  assert_eq(metadata, Some("production"))
}

test "context with span context" {
  // Test storing and retrieving span context from context
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,  // sampled
    "congo=t61rcWkgMzE"
  )
  
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  let retrieved_span_ctx = @azimuth.telemetry.api.context.Context.get_span_context(ctx_with_span)
  
  assert_eq(retrieved_span_ctx.trace_id(), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(retrieved_span_ctx.span_id(), "b7ad6b7169203331")
  assert_true(retrieved_span_ctx.is_sampled())
  assert_eq(retrieved_span_ctx.trace_state(), "congo=t61rcWkgMzE")
}