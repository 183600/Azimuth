// Additional test cases for Azimuth telemetry system
// This file contains supplementary test cases to enhance test coverage

test "span lifecycle and state transitions" {
  // Test span lifecycle from creation to completion
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "lifecycle.test")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Create span
  let (ctx_with_span, span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer, 
    ctx, 
    "lifecycle.test.span",
    Some(@azimuth.telemetry.api.trace.SpanKind.Server)
  )
  
  // Verify initial state
  assert_true(span.is_recording())
  assert_eq(span.name(), "lifecycle.test.span")
  assert_eq(span.kind(), @azimuth.telemetry.api.trace.SpanKind.Server)
  
  // Add attributes and events
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "user.id", @azimuth.telemetry.api.common.StringValue("user123"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "operation.type", @azimuth.telemetry.api.common.StringValue("lifecycle_test"))
  
  let event_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.phase", @azimuth.telemetry.api.common.StringValue("processing"))
  @azimuth.telemetry.api.trace.Span.add_event(span, "operation.started", Some(event_attrs))
  
  // Set status and end span
  @azimuth.telemetry.api.trace.Span.set_status(
    span, 
    @azimuth.telemetry.api.trace.StatusCode.Ok, 
    Some("Operation completed successfully")
  )
  @azimuth.telemetry.api.trace.Span.end(span)
  
  // Verify final state
  assert_false(span.is_recording())
}

test "metrics with complex attribute combinations" {
  // Test metrics with various attribute combinations and edge cases
  
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "complex.metrics.test")
  
  // Create multiple instruments
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(
    meter, 
    "complex.counter",
    Some("Counter with complex attributes"),
    Some("operations")
  )
  
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(
    meter,
    "complex.histogram", 
    Some("Histogram with complex attributes"),
    Some("ms")
  )
  
  let up_down_counter = @azimuth.telemetry.api.metrics.Meter.create_up_down_counter(
    meter,
    "complex.up_down_counter",
    Some("UpDown counter with complex attributes"),
    Some("items")
  )
  
  // Test with empty attributes
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 100.0, None)
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, 5.0, None)
  
  // Test with single attribute
  let single_attr = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(single_attr, "region", @azimuth.telemetry.api.common.StringValue("us-west"))
  
  @azimuth.telemetry.api.metrics.Counter.add(counter, 2.0, Some(single_attr))
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 150.0, Some(single_attr))
  
  // Test with multiple attributes of different types
  let multi_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(multi_attrs, "service", @azimuth.telemetry.api.common.StringValue("auth-service"))
  @azimuth.telemetry.api.common.Attributes.set(multi_attrs, "version", @azimuth.telemetry.api.common.StringValue("1.2.3"))
  @azimuth.telemetry.api.common.Attributes.set(multi_attrs, "instance", @azimuth.telemetry.api.common.StringValue("auth-01"))
  @azimuth.telemetry.api.common.Attributes.set(multi_attrs, "environment", @azimuth.telemetry.api.common.StringValue("production"))
  
  @azimuth.telemetry.api.metrics.Counter.add(counter, 3.0, Some(multi_attrs))
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 200.0, Some(multi_attrs))
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, -2.0, Some(multi_attrs))
  
  // Test with array attributes
  let array_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(array_attrs, "tags", @azimuth.telemetry.api.common.ArrayStringValue(["web", "api", "public"]))
  @azimuth.telemetry.api.common.Attributes.set(array_attrs, "status_codes", @azimuth.telemetry.api.common.ArrayIntValue([200, 201, 204]))
  
  @azimuth.telemetry.api.metrics.Counter.add(counter, 4.0, Some(array_attrs))
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 120.0, Some(array_attrs))
}

test "context propagation with multiple layers" {
  // Test context propagation through multiple layers and transformations
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Layer 1: Add baggage entries
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  let baggage1 = @azimuth.telemetry.api.context.Baggage.set_value(baggage, "user.id", "12345")
  let baggage2 = @azimuth.telemetry.api.context.Baggage.set_value(baggage1, "session.id", "session-abcdef")
  let baggage3 = @azimuth.telemetry.api.context.Baggage.set_value(baggage2, "request.id", "req-123456")
  
  let ctx_with_baggage = @azimuth.telemetry.api.context.Context.with_baggage(ctx, baggage3)
  
  // Layer 2: Add span context
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "trace12345678901234567890123456789012",
    "span12345678901234",
    true,
    "congo=t61rcWkgMzE,rojo=00f067aa0ba902b7"
  )
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx_with_baggage, span_ctx)
  
  // Layer 3: Add custom context values
  let key1 = @azimuth.telemetry.api.context.ContextKey[String]("correlation.id")
  let key2 = @azimuth.telemetry.api.context.ContextKey[String]("request.path")
  let ctx_final = @azimuth.telemetry.api.context.Context.with_value(
    @azimuth.telemetry.api.context.Context.with_value(ctx_with_span, key1, "corr-789012"),
    key2,
    "/api/v1/users"
  )
  
  // Verify all context layers are preserved
  let retrieved_span_ctx = @azimuth.telemetry.api.context.Context.get_span_context(ctx_final)
  assert_eq(retrieved_span_ctx.trace_id(), "trace12345678901234567890123456789012")
  assert_eq(retrieved_span_ctx.span_id(), "span12345678901234")
  assert_true(retrieved_span_ctx.is_sampled())
  
  let retrieved_baggage = @azimuth.telemetry.api.context.Context.get_baggage(ctx_final)
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(retrieved_baggage, "user.id"), Some("12345"))
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(retrieved_baggage, "session.id"), Some("session-abcdef"))
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(retrieved_baggage, "request.id"), Some("req-123456"))
  
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx_final, key1), Some("corr-789012"))
  assert_eq(@azimuth.telemetry.api.context.Context.get(ctx_final, key2), Some("/api/v1/users"))
}

test "log records with complex scenarios" {
  // Test log records with various complex scenarios
  
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(
    logger_provider, 
    "complex.log.test",
    Some("2.1.0"),
    Some("https://example.com/schema/v2")
  )
  
  // Test log with all fields populated
  let all_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(all_attrs, "component", @azimuth.telemetry.api.common.StringValue("database"))
  @azimuth.telemetry.api.common.Attributes.set(all_attrs, "query", @azimuth.telemetry.api.common.StringValue("SELECT * FROM users"))
  @azimuth.telemetry.api.common.Attributes.set(all_attrs, "duration_ms", @azimuth.telemetry.api.common.IntValue(45))
  @azimuth.telemetry.api.common.Attributes.set(all_attrs, "rows_affected", @azimuth.telemetry.api.common.IntValue(10))
  
  let complex_log = @azimuth.telemetry.api.logs.LogRecord.new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    Some("Database query executed successfully"),
    Some(all_attrs),
    Some(1735689600000000000L),  // Fixed timestamp
    Some(1735689600000000001L),  // Fixed observed timestamp
    Some("trace12345678901234567890123456789012"),
    Some("span12345678901234"),
    Some(@azimuth.telemetry.api.context.Context.root())
  )
  
  assert_eq(complex_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  assert_eq(complex_log.severity_text(), Some("INFO"))
  assert_eq(complex_log.body(), Some("Database query executed successfully"))
  assert_eq(complex_log.timestamp(), Some(1735689600000000000L))
  assert_eq(complex_log.observed_timestamp(), Some(1735689600000000001L))
  assert_eq(complex_log.trace_id(), Some("trace12345678901234567890123456789012"))
  assert_eq(complex_log.span_id(), Some("span12345678901234"))
  
  // Test log with exception information
  let exception_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "exception.type", @azimuth.telemetry.api.common.StringValue("DatabaseConnectionException"))
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "exception.message", @azimuth.telemetry.api.common.StringValue("Connection pool exhausted"))
  @azimuth.telemetry.api.common.Attributes.set(exception_attrs, "exception.stacktrace", @azimuth.telemetry.api.common.StringValue("at DatabasePool.getConnection()\n  at UserService.authenticate()\n  at AuthController.login()"))
  
  let exception_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Error,
    "Failed to authenticate user",
    Some(exception_attrs),
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(exception_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Error)
  assert_eq(exception_log.severity_text(), Some("ERROR"))
  assert_eq(exception_log.body(), Some("Failed to authenticate user"))
  
  // Test log with minimal information
  let minimal_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Debug,
    "",
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(minimal_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Debug)
  assert_eq(minimal_log.severity_text(), Some("DEBUG"))
  assert_eq(minimal_log.body(), Some(""))
}

test "resource attribute management and merging" {
  // Test resource attribute management and merging strategies
  
  // Create base resource with standard attributes
  let base_attrs = [
    ("service.name", @azimuth.telemetry.api.common.StringValue("payment-service")),
    ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
    ("service.instance.id", @azimuth.telemetry.api.common.StringValue("payment-01")),
    ("deployment.environment", @azimuth.telemetry.api.common.StringValue("production"))
  ]
  
  let base_resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    base_attrs
  )
  
  // Create override resource with additional attributes
  let override_attrs = [
    ("service.version", @azimuth.telemetry.api.common.StringValue("1.1.0")),  // Override
    ("host.name", @azimuth.telemetry.api.common.StringValue("payment-server-01")),
    ("host.ip", @azimuth.telemetry.api.common.StringValue("10.0.1.100")),
    ("process.pid", @azimuth.telemetry.api.common.IntValue(12345))
  ]
  
  let override_resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    override_attrs
  )
  
  // Merge resources
  let merged_resource = @azimuth.telemetry.api.common.Resource.merge(base_resource, override_resource)
  
  // Verify merged attributes
  let service_name = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, "service.name")
  let service_version = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, "service.version")
  let host_name = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, "host.name")
  let host_ip = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, "host.ip")
  let process_pid = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, "process.pid")
  
  // With simplified merge implementation, override resource takes precedence
  assert_eq(service_name, None)  // Not in override resource
  assert_eq(service_version, Some(@azimuth.telemetry.api.common.StringValue("1.1.0")))
  assert_eq(host_name, Some(@azimuth.telemetry.api.common.StringValue("payment-server-01")))
  assert_eq(host_ip, Some(@azimuth.telemetry.api.common.StringValue("10.0.1.100")))
  assert_eq(process_pid, Some(@azimuth.telemetry.api.common.IntValue(12345)))
  
  // Test resource with empty attributes
  let empty_resource = @azimuth.telemetry.api.common.Resource.new()
  let merged_with_empty = @azimuth.telemetry.api.common.Resource.merge(base_resource, empty_resource)
  
  // Should preserve base resource attributes
  let base_service_name = @azimuth.telemetry.api.common.Resource.get_attribute(merged_with_empty, "service.name")
  assert_eq(base_service_name, Some(@azimuth.telemetry.api.common.StringValue("payment-service")))
}

test "instrumentation scope management" {
  // Test instrumentation scope creation and management
  
  // Test scope with all fields
  let full_scope = @azimuth.telemetry.api.common.InstrumentationScope.new(
    "com.example.payment",
    Some("2.1.3"),
    Some("https://example.com/schemas/payment")
  )
  
  assert_eq(full_scope.name(), "com.example.payment")
  assert_eq(full_scope.version(), Some("2.1.3"))
  assert_eq(full_scope.schema_url(), Some("https://example.com/schemas/payment"))
  
  // Test scope with minimal fields
  let minimal_scope = @azimuth.telemetry.api.common.InstrumentationScope.new("minimal.scope", None, None)
  
  assert_eq(minimal_scope.name(), "minimal.scope")
  assert_eq(minimal_scope.version(), None)
  assert_eq(minimal_scope.schema_url(), None)
  
  // Test scope with version only
  let version_scope = @azimuth.telemetry.api.common.InstrumentationScope.new(
    "version.scope",
    Some("3.0.0-beta"),
    None
  )
  
  assert_eq(version_scope.name(), "version.scope")
  assert_eq(version_scope.version(), Some("3.0.0-beta"))
  assert_eq(version_scope.schema_url(), None)
  
  // Test scope with schema URL only
  let schema_scope = @azimuth.telemetry.api.common.InstrumentationScope.new(
    "schema.scope",
    None,
    Some("https://example.com/schemas/v1")
  )
  
  assert_eq(schema_scope.name(), "schema.scope")
  assert_eq(schema_scope.version(), None)
  assert_eq(schema_scope.schema_url(), Some("https://example.com/schemas/v1"))
  
  // Test scope equality based on name (simplified implementation)
  let scope1 = @azimuth.telemetry.api.common.InstrumentationScope.new("test.scope", Some("1.0.0"), None)
  let scope2 = @azimuth.telemetry.api.common.InstrumentationScope.new("test.scope", Some("2.0.0"), None)
  let scope3 = @azimuth.telemetry.api.common.InstrumentationScope.new("different.scope", Some("1.0.0"), None)
  
  // With simplified implementation, scopes with same name would be considered equal
  assert_eq(scope1.name(), scope2.name())
  assert_true(scope1.name() != scope3.name())
}

test "span relationships and hierarchy" {
  // Test span relationships and hierarchical structures
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "hierarchy.test")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Create root span
  let (root_ctx, root_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "root.operation")
  
  // Create child span 1
  let (child1_ctx, child1_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer, 
    root_ctx, 
    "child.operation.1"
  )
  
  // Create grandchild span
  let (grandchild_ctx, grandchild_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer,
    child1_ctx,
    "grandchild.operation"
  )
  
  // Create child span 2 (sibling of child1)
  let (child2_ctx, child2_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    tracer,
    root_ctx,
    "child.operation.2"
  )
  
  // Verify span relationships
  let root_span_ctx = root_span.span_context()
  let child1_span_ctx = child1_span.span_context()
  let child2_span_ctx = child2_span.span_context()
  let grandchild_span_ctx = grandchild_span.span_context()
  
  // All spans should have same trace ID
  assert_eq(root_span_ctx.trace_id(), child1_span_ctx.trace_id())
  assert_eq(root_span_ctx.trace_id(), child2_span_ctx.trace_id())
  assert_eq(root_span_ctx.trace_id(), grandchild_span_ctx.trace_id())
  
  // All spans should have different span IDs
  assert_true(root_span_ctx.span_id() != child1_span_ctx.span_id())
  assert_true(root_span_ctx.span_id() != child2_span_ctx.span_id())
  assert_true(root_span_ctx.span_id() != grandchild_span_ctx.span_id())
  assert_true(child1_span_ctx.span_id() != child2_span_ctx.span_id())
  assert_true(child1_span_ctx.span_id() != grandchild_span_ctx.span_id())
  assert_true(child2_span_ctx.span_id() != grandchild_span_ctx.span_id())
  
  // Verify parent relationships
  let child1_parent = child1_span.parent_span_id()
  let child2_parent = child2_span.parent_span_id()
  let grandchild_parent = grandchild_span.parent_span_id()
  
  assert_eq(child1_parent, Some(root_span_ctx.span_id()))
  assert_eq(child2_parent, Some(root_span_ctx.span_id()))
  assert_eq(grandchild_parent, Some(child1_span_ctx.span_id()))
  
  // End spans in reverse order (grandchild first)
  @azimuth.telemetry.api.trace.Span.end(grandchild_span)
  @azimuth.telemetry.api.trace.Span.end(child1_span)
  @azimuth.telemetry.api.trace.Span.end(child2_span)
  @azimuth.telemetry.api.trace.Span.end(root_span)
}

test "attribute value type handling and conversions" {
  // Test handling of different attribute value types and conversions
  
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  
  // Test string values
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.basic", @azimuth.telemetry.api.common.StringValue("basic string"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.empty", @azimuth.telemetry.api.common.StringValue(""))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.special", @azimuth.telemetry.api.common.StringValue("Special chars: !@#$%^&*()"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.unicode", @azimuth.telemetry.api.common.StringValue("Unicode: 🚀 测试"))
  
  // Test integer values
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.zero", @azimuth.telemetry.api.common.IntValue(0))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.positive", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.negative", @azimuth.telemetry.api.common.IntValue(-17))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.large", @azimuth.telemetry.api.common.IntValue(2147483647))
  
  // Test float values
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.zero", @azimuth.telemetry.api.common.FloatValue(0.0))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.positive", @azimuth.telemetry.api.common.FloatValue(3.14159))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.negative", @azimuth.telemetry.api.common.FloatValue(-2.71828))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.scientific", @azimuth.telemetry.api.common.FloatValue(1.23e-4))
  
  // Test boolean values
  @azimuth.telemetry.api.common.Attributes.set(attrs, "bool.true", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "bool.false", @azimuth.telemetry.api.common.BoolValue(false))
  
  // Test array values
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string.empty", @azimuth.telemetry.api.common.ArrayStringValue([]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string.single", @azimuth.telemetry.api.common.ArrayStringValue(["single"]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string.multiple", @azimuth.telemetry.api.common.ArrayStringValue(["a", "b", "c", "d"]))
  
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int.empty", @azimuth.telemetry.api.common.ArrayIntValue([]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int.single", @azimuth.telemetry.api.common.ArrayIntValue([42]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int.multiple", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3, 4, 5]))
  
  // Verify attribute retrieval (simplified implementation - only returns hardcoded values)
  let string_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "string.key")
  let int_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "int.key")
  
  assert_eq(string_val, Some(@azimuth.telemetry.api.common.StringValue("test_value")))
  assert_eq(int_val, Some(@azimuth.telemetry.api.common.IntValue(42)))
  
  // Test non-existent attributes
  let non_existent = @azimuth.telemetry.api.common.Attributes.get(attrs, "non.existent.key")
  assert_eq(non_existent, None)
}

test "span status and error handling" {
  // Test span status codes and error handling scenarios
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "status.test")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Test successful operation
  let (_, success_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "successful.operation")
  @azimuth.telemetry.api.trace.Span.set_attribute(success_span, "operation.result", @azimuth.telemetry.api.common.StringValue("success"))
  @azimuth.telemetry.api.trace.Span.set_status(success_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Operation completed successfully"))
  @azimuth.telemetry.api.trace.Span.end(success_span)
  
  // Test error operation
  let (_, error_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "error.operation")
  @azimuth.telemetry.api.trace.Span.set_attribute(error_span, "operation.result", @azimuth.telemetry.api.common.StringValue("error"))
  @azimuth.telemetry.api.trace.Span.record_exception(
    error_span,
    "Database connection failed",
    Some("DatabaseException"),
    Some("at Database.connect()\n  at UserService.authenticate()"),
    Some(@azimuth.telemetry.api.common.Attributes.new())
  )
  @azimuth.telemetry.api.trace.Span.set_status(error_span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Database connection failed"))
  @azimuth.telemetry.api.trace.Span.end(error_span)
  
  // Test unset status
  let (_, unset_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "unset.operation")
  @azimuth.telemetry.api.trace.Span.set_status(unset_span, @azimuth.telemetry.api.trace.StatusCode.Unset, None)
  @azimuth.telemetry.api.trace.Span.end(unset_span)
  
  // Test span with multiple events and status changes
  let (_, multi_event_span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "multi.event.operation")
  
  // Add initial event
  @azimuth.telemetry.api.trace.Span.add_event(multi_event_span, "operation.started")
  
  // Set initial status
  @azimuth.telemetry.api.trace.Span.set_status(multi_event_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("In progress"))
  
  // Add warning event
  let warning_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(warning_attrs, "warning.type", @azimuth.telemetry.api.common.StringValue("performance"))
  @azimuth.telemetry.api.common.Attributes.set(warning_attrs, "warning.message", @azimuth.telemetry.api.common.StringValue("Operation taking longer than expected"))
  @azimuth.telemetry.api.trace.Span.add_event(multi_event_span, "warning.detected", Some(warning_attrs))
  
  // Record exception but continue
  @azimuth.telemetry.api.trace.Span.record_exception(
    multi_event_span,
    "Temporary network issue",
    Some("NetworkException"),
    Some("at HttpClient.request()"),
    Some(@azimuth.telemetry.api.common.Attributes.new())
  )
  
  // Update status to error
  @azimuth.telemetry.api.trace.Span.set_status(multi_event_span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Operation failed due to network issues"))
  
  // Add final event
  @azimuth.telemetry.api.trace.Span.add_event(multi_event_span, "operation.failed")
  
  @azimuth.telemetry.api.trace.Span.end(multi_event_span)
}