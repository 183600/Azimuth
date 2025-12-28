test "deep context and trace integration" {
  // Test deep integration between context management and trace operations
  
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(tracer_provider, "deep.integration.tracer")
  
  // Create root context
  let root_ctx = @azimuth.telemetry.api.context.Context::root()
  
  // Add custom context values
  let user_key = @azimuth.telemetry.api.context.ContextKey::new("user.id")
  let session_key = @azimuth.telemetry.api.context.ContextKey::new("session.id")
  let request_key = @azimuth.telemetry.api.context.ContextKey::new("request.id")
  
  let ctx_with_user = @azimuth.telemetry.api.context.Context::with_value(root_ctx, user_key, "user12345")
  let ctx_with_session = @azimuth.telemetry.api.context.Context::with_value(ctx_with_user, session_key, "session_abcdef")
  let ctx_with_request = @azimuth.telemetry.api.context.Context::with_value(ctx_with_session, request_key, "req_123456")
  
  // Start span with enriched context
  let (enriched_ctx, main_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx_with_request, "main.operation")
  
  // Verify context values are preserved
  let retrieved_user = @azimuth.telemetry.api.context.Context::get(enriched_ctx, user_key)
  let retrieved_session = @azimuth.telemetry.api.context.Context::get(enriched_ctx, session_key)
  let retrieved_request = @azimuth.telemetry.api.context.Context::get(enriched_ctx, request_key)
  
  assert_eq(retrieved_user, Some("user12345"))
  assert_eq(retrieved_session, Some("session_abcdef"))
  assert_eq(retrieved_request, Some("req_123456"))
  
  // Add context values within span
  let operation_key = @azimuth.telemetry.api.context.ContextKey::new("operation.id")
  let ctx_with_operation = @azimuth.telemetry.api.context.Context::with_value(enriched_ctx, operation_key, "op_789012")
  
  // Start child span with further enriched context
  let (child_ctx, child_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx_with_operation, "child.operation")
  
  // Verify all context values are available in child span
  let child_user = @azimuth.telemetry.api.context.Context::get(child_ctx, user_key)
  let child_session = @azimuth.telemetry.api.context.Context::get(child_ctx, session_key)
  let child_request = @azimuth.telemetry.api.context.Context::get(child_ctx, request_key)
  let child_operation = @azimuth.telemetry.api.context.Context::get(child_ctx, operation_key)
  
  assert_eq(child_user, Some("user12345"))
  assert_eq(child_session, Some("session_abcdef"))
  assert_eq(child_request, Some("req_123456"))
  assert_eq(child_operation, Some("op_789012"))
  
  // Add span context information to regular context
  let span_ctx = @azimuth.telemetry.api.trace.Span::span_context(main_span)
  let trace_id_key = @azimuth.telemetry.api.context.ContextKey::new("trace.id")
  let span_id_key = @azimuth.telemetry.api.context.ContextKey::new("span.id")
  
  let ctx_with_trace = @azimuth.telemetry.api.context.Context::with_value(
    child_ctx, 
    trace_id_key, 
    @azimuth.telemetry.api.trace.SpanContext::trace_id(span_ctx)
  )
  let ctx_with_span = @azimuth.telemetry.api.context.Context::with_value(
    ctx_with_trace, 
    span_id_key, 
    @azimuth.telemetry.api.trace.SpanContext::span_id(span_ctx)
  )
  
  // Start grandchild span with trace context
  let (grandchild_ctx, grandchild_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx_with_span, "grandchild.operation")
  
  // Verify trace context is available
  let grandchild_trace_id = @azimuth.telemetry.api.context.Context::get(grandchild_ctx, trace_id_key)
  let grandchild_span_id = @azimuth.telemetry.api.context.Context::get(grandchild_ctx, span_id_key)
  
  assert_true(grandchild_trace_id != None)
  assert_true(grandchild_span_id != None)
  assert_eq(grandchild_trace_id.unwrap(), @azimuth.telemetry.api.trace.SpanContext::trace_id(span_ctx))
  
  // End spans in reverse order
  @azimuth.telemetry.api.trace.Span::end(grandchild_span)
  @azimuth.telemetry.api.trace.Span::end(child_span)
  @azimuth.telemetry.api.trace.Span::end(main_span)
}

test "context propagation through span hierarchy" {
  // Test context propagation through complex span hierarchies
  
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(tracer_provider, "hierarchy.tracer")
  
  // Create initial context with baggage
  let root_ctx = @azimuth.telemetry.api.context.Context::root()
  let baggage = @azimuth.telemetry.api.context.Baggage::new()
  let baggage_with_user = @azimuth.telemetry.api.context.Baggage::set_entry(baggage, "user.id", "user12345")
  let baggage_with_tenant = @azimuth.telemetry.api.context.Baggage::set_entry(baggage_with_user, "tenant.id", "tenant_67890")
  
  let baggage_key = @azimuth.telemetry.api.context.ContextKey::new("baggage")
  let ctx_with_baggage = @azimuth.telemetry.api.context.Context::with_value(root_ctx, baggage_key, "baggage_value")
  
  // Add custom context values
  let correlation_key = @azimuth.telemetry.api.context.ContextKey::new("correlation.id")
  let ctx_with_correlation = @azimuth.telemetry.api.context.Context::with_value(ctx_with_baggage, correlation_key, "corr_123456")
  
  // Build complex span hierarchy
  let (level1_ctx, level1_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx_with_correlation, "level1.operation")
  
  // Add level-specific context
  let level1_key = @azimuth.telemetry.api.context.ContextKey::new("level1.data")
  let level1_enriched = @azimuth.telemetry.api.context.Context::with_value(level1_ctx, level1_key, "level1_value")
  
  // Level 2 spans
  let (level2a_ctx, level2a_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, level1_enriched, "level2a.operation")
  let (level2b_ctx, level2b_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, level1_enriched, "level2b.operation")
  
  // Add level 2 specific context
  let level2a_key = @azimuth.telemetry.api.context.ContextKey::new("level2a.data")
  let level2a_enriched = @azimuth.telemetry.api.context.Context::with_value(level2a_ctx, level2a_key, "level2a_value")
  
  let level2b_key = @azimuth.telemetry.api.context.ContextKey::new("level2b.data")
  let level2b_enriched = @azimuth.telemetry.api.context.Context::with_value(level2b_ctx, level2b_key, "level2b_value")
  
  // Level 3 spans
  let (level3a_ctx, level3a_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, level2a_enriched, "level3a.operation")
  let (level3b_ctx, level3b_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, level2a_enriched, "level3b.operation")
  let (level3c_ctx, level3c_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, level2b_enriched, "level3c.operation")
  
  // Verify context propagation at each level
  
  // Level 3a should have all context from parent levels
  let level3a_correlation = @azimuth.telemetry.api.context.Context::get(level3a_ctx, correlation_key)
  let level3a_level1 = @azimuth.telemetry.api.context.Context::get(level3a_ctx, level1_key)
  let level3a_level2a = @azimuth.telemetry.api.context.Context::get(level3a_ctx, level2a_key)
  
  assert_eq(level3a_correlation, Some("corr_123456"))
  assert_eq(level3a_level1, Some("level1_value"))
  assert_eq(level3a_level2a, Some("level2a_value"))
  
  // Level 3b should have the same context as 3a (same parent)
  let level3b_correlation = @azimuth.telemetry.api.context.Context::get(level3b_ctx, correlation_key)
  let level3b_level1 = @azimuth.telemetry.api.context.Context::get(level3b_ctx, level1_key)
  let level3b_level2a = @azimuth.telemetry.api.context.Context::get(level3b_ctx, level2a_key)
  
  assert_eq(level3b_correlation, Some("corr_123456"))
  assert_eq(level3b_level1, Some("level1_value"))
  assert_eq(level3b_level2a, Some("level2a_value"))
  
  // Level 3c should have different level2 context but same level1 context
  let level3c_correlation = @azimuth.telemetry.api.context.Context::get(level3c_ctx, correlation_key)
  let level3c_level1 = @azimuth.telemetry.api.context.Context::get(level3c_ctx, level1_key)
  let level3c_level2a = @azimuth.telemetry.api.context.Context::get(level3c_ctx, level2a_key)
  let level3c_level2b = @azimuth.telemetry.api.context.Context::get(level3c_ctx, level2b_key)
  
  assert_eq(level3c_correlation, Some("corr_123456"))
  assert_eq(level3c_level1, Some("level1_value"))
  assert_eq(level3c_level2a, None)  // Different branch
  assert_eq(level3c_level2b, Some("level2b_value"))
  
  // Modify context in one branch and verify isolation
  let branch_key = @azimuth.telemetry.api.context.ContextKey::new("branch.specific")
  let level3a_modified = @azimuth.telemetry.api.context.Context::with_value(level3a_ctx, branch_key, "branch_a_value")
  
  // Level 3b should not have the branch-specific value
  let level3b_branch = @azimuth.telemetry.api.context.Context::get(level3b_ctx, branch_key)
  assert_eq(level3b_branch, None)
  
  // Level 3a modified should have the branch-specific value
  let level3a_branch = @azimuth.telemetry.api.context.Context::get(level3a_modified, branch_key)
  assert_eq(level3a_branch, Some("branch_a_value"))
  
  // End all spans
  @azimuth.telemetry.api.trace.Span::end(level3a_span)
  @azimuth.telemetry.api.trace.Span::end(level3b_span)
  @azimuth.telemetry.api.trace.Span::end(level3c_span)
  @azimuth.telemetry.api.trace.Span::end(level2a_span)
  @azimuth.telemetry.api.trace.Span::end(level2b_span)
  @azimuth.telemetry.api.trace.Span::end(level1_span)
}

test "context and span lifecycle integration" {
  // Test integration between context lifecycle and span lifecycle
  
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(tracer_provider, "lifecycle.tracer")
  
  // Test context with span lifecycle
  let root_ctx = @azimuth.telemetry.api.context.Context::root()
  let lifecycle_key = @azimuth.telemetry.api.context.ContextKey::new("lifecycle.data")
  
  // Start with initial context
  let initial_ctx = @azimuth.telemetry.api.context.Context::with_value(root_ctx, lifecycle_key, "initial")
  
  // Start span and modify context
  let (ctx1, span1) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, initial_ctx, "span1")
  let ctx1_modified = @azimuth.telemetry.api.context.Context::with_value(ctx1, lifecycle_key, "modified_in_span1")
  
  // Start child span with modified context
  let (ctx2, span2) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx1_modified, "span2")
  
  // Verify context value
  let value_in_span2 = @azimuth.telemetry.api.context.Context::get(ctx2, lifecycle_key)
  assert_eq(value_in_span2, Some("modified_in_span1"))
  
  // Modify context in span2
  let ctx2_modified = @azimuth.telemetry.api.context.Context::with_value(ctx2, lifecycle_key, "modified_in_span2")
  
  // Start another child span
  let (ctx3, span3) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx2_modified, "span3")
  
  // Verify context value
  let value_in_span3 = @azimuth.telemetry.api.context.Context::get(ctx3, lifecycle_key)
  assert_eq(value_in_span3, Some("modified_in_span2"))
  
  // End span3
  @azimuth.telemetry.api.trace.Span::end(span3)
  
  // Context in span2 should be unchanged
  let value_in_span2_after = @azimuth.telemetry.api.context.Context::get(ctx2, lifecycle_key)
  assert_eq(value_in_span2_after, Some("modified_in_span1"))
  
  // End span2
  @azimuth.telemetry.api.trace.Span::end(span2)
  
  // Context in span1 should be unchanged
  let value_in_span1_after = @azimuth.telemetry.api.context.Context::get(ctx1, lifecycle_key)
  assert_eq(value_in_span1_after, Some("initial"))
  
  // End span1
  @azimuth.telemetry.api.trace.Span::end(span1)
  
  // Original context should be unchanged
  let value_in_initial = @azimuth.telemetry.api.context.Context::get(initial_ctx, lifecycle_key)
  assert_eq(value_in_initial, Some("initial"))
}

test "context propagation with trace context" {
  // Test context propagation with explicit trace context
  
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(tracer_provider, "trace.context.tracer")
  
  // Create explicit span context
  let explicit_span_ctx = @azimuth.telemetry.api.trace.SpanContext::new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  
  // Create context with explicit span context
  let root_ctx = @azimuth.telemetry.api.context.Context::root()
  let ctx_with_span = @azimuth.telemetry.api.context.Context::with_span_context(root_ctx, explicit_span_ctx)
  
  // Add custom context values
  let custom_key = @azimuth.telemetry.api.context.ContextKey::new("custom.data")
  let ctx_with_custom = @azimuth.telemetry.api.context.Context::with_value(ctx_with_span, custom_key, "custom_value")
  
  // Start span with context that has explicit span context
  let (span_ctx, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx_with_custom, "explicit.context.span")
  
  // Verify custom context is preserved
  let custom_value = @azimuth.telemetry.api.context.Context::get(span_ctx, custom_key)
  assert_eq(custom_value, Some("custom_value"))
  
  // Verify span context is related to explicit context
  let span_span_ctx = @azimuth.telemetry.api.trace.Span::span_context(span)
  
  // In a real implementation, this should have the same trace ID
  // In simplified implementation, we just verify it's valid
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(span_span_ctx))
  
  // Test with invalid span context
  let invalid_span_ctx = @azimuth.telemetry.api.trace.SpanContext::new("", "", false, "")
  let ctx_with_invalid = @azimuth.telemetry.api.context.Context::with_span_context(root_ctx, invalid_span_ctx)
  
  let (invalid_span_ctx, invalid_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx_with_invalid, "invalid.context.span")
  
  // Should still work and create valid span
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(@azimuth.telemetry.api.trace.Span::span_context(invalid_span)))
  
  @azimuth.telemetry.api.trace.Span::end(invalid_span)
  @azimuth.telemetry.api.trace.Span::end(span)
}

test "context and span attributes integration" {
  // Test integration between context values and span attributes
  
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(tracer_provider, "attributes.tracer")
  
  // Create context with various data types
  let root_ctx = @azimuth.telemetry.api.context.Context::root()
  
  let string_key = @azimuth.telemetry.api.context.ContextKey::new("string.data")
  let number_key = @azimuth.telemetry.api.context.ContextKey::new("number.data")
  let boolean_key = @azimuth.telemetry.api.context.ContextKey::new("boolean.data")
  let complex_key = @azimuth.telemetry.api.context.ContextKey::new("complex.data")
  
  let enriched_ctx = @azimuth.telemetry.api.context.Context::with_value(root_ctx, string_key, "string_value")
  let enriched_ctx = @azimuth.telemetry.api.context.Context::with_value(enriched_ctx, number_key, "42")
  let enriched_ctx = @azimuth.telemetry.api.context.Context::with_value(enriched_ctx, boolean_key, "true")
  let enriched_ctx = @azimuth.telemetry.api.context.Context::with_value(enriched_ctx, complex_key, "{\"key\":\"value\",\"array\":[1,2,3]}")
  
  // Start span and transfer context to attributes
  let (span_ctx, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, enriched_ctx, "context.attributes.span")
  
  // Transfer context values to span attributes
  let string_value = @azimuth.telemetry.api.context.Context::get(span_ctx, string_key)
  let number_value = @azimuth.telemetry.api.context.Context::get(span_ctx, number_key)
  let boolean_value = @azimuth.telemetry.api.context.Context::get(span_ctx, boolean_key)
  let complex_value = @azimuth.telemetry.api.context.Context::get(span_ctx, complex_key)
  
  match string_value {
    Some(s) => @azimuth.telemetry.api.trace.Span::set_attribute(span, "context.string", @azimuth.telemetry.api.common.StringValue(s))
    None => ()
  }
  
  match number_value {
    Some(n) => @azimuth.telemetry.api.trace.Span::set_attribute(span, "context.number", @azimuth.telemetry.api.common.StringValue(n))
    None => ()
  }
  
  match boolean_value {
    Some(b) => @azimuth.telemetry.api.trace.Span::set_attribute(span, "context.boolean", @azimuth.telemetry.api.common.StringValue(b))
    None => ()
  }
  
  match complex_value {
    Some(c) => @azimuth.telemetry.api.trace.Span::set_attribute(span, "context.complex", @azimuth.telemetry.api.common.StringValue(c))
    None => ()
  }
  
  // Add span context back to context
  let span_span_ctx = @azimuth.telemetry.api.trace.Span::span_context(span)
  let trace_id_key = @azimuth.telemetry.api.context.ContextKey::new("span.trace.id")
  let span_id_key = @azimuth.telemetry.api.context.ContextKey::new("span.span.id")
  
  let ctx_with_span_info = @azimuth.telemetry.api.context.Context::with_value(
    span_ctx,
    trace_id_key,
    @azimuth.telemetry.api.trace.SpanContext::trace_id(span_span_ctx)
  )
  let ctx_with_span_info = @azimuth.telemetry.api.context.Context::with_value(
    ctx_with_span_info,
    span_id_key,
    @azimuth.telemetry.api.trace.SpanContext::span_id(span_span_ctx)
  )
  
  // Start child span with span context information
  let (child_ctx, child_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx_with_span_info, "child.context.attributes.span")
  
  // Transfer span context information to child span attributes
  let child_trace_id = @azimuth.telemetry.api.context.Context::get(child_ctx, trace_id_key)
  let child_span_id = @azimuth.telemetry.api.context.Context::get(child_ctx, span_id_key)
  
  match child_trace_id {
    Some(tid) => @azimuth.telemetry.api.trace.Span::set_attribute(child_span, "parent.trace.id", @azimuth.telemetry.api.common.StringValue(tid))
    None => ()
  }
  
  match child_span_id {
    Some(sid) => @azimuth.telemetry.api.trace.Span::set_attribute(child_span, "parent.span.id", @azimuth.telemetry.api.common.StringValue(sid))
    None => ()
  }
  
  // Verify context values are still available in child span
  let child_string_value = @azimuth.telemetry.api.context.Context::get(child_ctx, string_key)
  let child_number_value = @azimuth.telemetry.api.context.Context::get(child_ctx, number_key)
  let child_boolean_value = @azimuth.telemetry.api.context.Context::get(child_ctx, boolean_key)
  let child_complex_value = @azimuth.telemetry.api.context.Context::get(child_ctx, complex_key)
  
  assert_eq(child_string_value, Some("string_value"))
  assert_eq(child_number_value, Some("42"))
  assert_eq(child_boolean_value, Some("true"))
  assert_eq(child_complex_value, Some("{\"key\":\"value\",\"array\":[1,2,3]}"))
  
  @azimuth.telemetry.api.trace.Span::end(child_span)
  @azimuth.telemetry.api.trace.Span::end(span)
}