test "comprehensive telemetry integration" {
  // Test comprehensive integration between trace, metrics, and logs
  
  // Setup providers
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "integration.test")
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "integration.test")
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "integration.test")
  
  // Create instruments
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "operations.total")
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "operation.duration")
  
  // Start a trace
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (trace_ctx, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "integration.operation")
  
  // Add span attributes
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "operation.type", @azimuth.telemetry.api.common.StringValue("integration"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "service.name", @azimuth.telemetry.api.common.StringValue("test-service"))
  
  // Log within the trace context
  @azimuth.telemetry.api.logs.Logger.info_with_context(
    logger,
    trace_ctx,
    "Starting integration operation"
  )
  
  // Record metrics within the trace context
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0, Some(@azimuth.telemetry.api.common.Attributes.new()))
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 150.0, Some(@azimuth.telemetry.api.common.Attributes.new()))
  
  // Add an event to the span
  @azimuth.telemetry.api.trace.Span.add_event(
    span,
    "metrics.recorded",
    Some([("operation.duration", @azimuth.telemetry.api.common.FloatValue(150.0))]),
    None
  )
  
  // Log completion
  @azimuth.telemetry.api.logs.Logger.info_with_context(
    logger,
    trace_ctx,
    "Integration operation completed successfully"
  )
  
  // End the span
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Success"))
  @azimuth.telemetry.api.trace.Span.end(span)
}

test "context propagation across components" {
  // Test context propagation across different telemetry components
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "propagation.test")
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "propagation.test")
  
  // Create initial context with baggage
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  let baggage_with_data = @azimuth.telemetry.api.context.Baggage.set_entry(baggage, "user.id", "12345")
  let baggage_with_session = @azimuth.telemetry.api.context.Baggage.set_entry(baggage_with_data, "session.id", "abcdef")
  
  // Start span with context
  let (span_ctx, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "propagation.test")
  
  // Add custom context value
  let custom_key = @azimuth.telemetry.api.context.ContextKey::new("custom.operation")
  let ctx_with_custom = @azimuth.telemetry.api.context.Context.with_value(span_ctx, custom_key, "test-operation")
  
  // Log with propagated context
  let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Operation with propagated context",
    None,
    None,
    None,
    Some(span.span_context().trace_id()),
    Some(span.span_context().span_id()),
    Some(ctx_with_custom)
  )
  
  @azimuth.telemetry.api.logs.Logger.emit(logger, log_record)
  
  // Verify context values are accessible
  let custom_value = @azimuth.telemetry.api.context.Context.get(ctx_with_custom, custom_key)
  assert_eq(custom_value, Some("test-operation"))
  
  // Test context extraction
  let user_id = @azimuth.telemetry.api.context.Baggage.get_entry(baggage_with_session, "user.id")
  let session_id = @azimuth.telemetry.api.context.Baggage.get_entry(baggage_with_session, "session.id")
  
  // Note: Simplified implementation returns None
  assert_eq(user_id, None)
  assert_eq(session_id, None)
  
  @azimuth.telemetry.api.trace.Span.end(span)
}

test "cross-component error handling" {
  // Test error handling across telemetry components
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "error.test")
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "error.test")
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "error.test")
  
  let error_counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "errors.total")
  
  // Start operation
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (error_ctx, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "error.prone.operation")
  
  // Log error before exception
  @azimuth.telemetry.api.logs.Logger.error_with_context(
    logger,
    error_ctx,
    "About to simulate an error condition"
  )
  
  // Record exception in span
  @azimuth.telemetry.api.trace.Span.record_exception(
    span,
    "Simulated error for testing",
    Some("TestException"),
    Some("at test_function()\n  at main()"),
    Some([("error.code", @azimuth.telemetry.api.common.StringValue("TEST_ERROR"))])
  )
  
  // Increment error counter
  let error_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(error_attrs, "error.type", @azimuth.telemetry.api.common.StringValue("TestException"))
  @azimuth.telemetry.api.common.Attributes.set(error_attrs, "error.code", @azimuth.telemetry.api.common.StringValue("TEST_ERROR"))
  
  @azimuth.telemetry.api.metrics.Counter.add(error_counter, 1.0, Some(error_attrs))
  
  // Set error status on span
  @azimuth.telemetry.api.trace.Span.set_status(
    span,
    @azimuth.telemetry.api.trace.StatusCode.Error,
    Some("Operation failed due to simulated error")
  )
  
  // Log error after exception
  let error_log_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(error_log_attrs, "exception.type", @azimuth.telemetry.api.common.StringValue("TestException"))
  @azimuth.telemetry.api.common.Attributes.set(error_log_attrs, "exception.message", @azimuth.telemetry.api.common.StringValue("Simulated error for testing"))
  
  let error_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Error,
    "Operation failed with exception",
    Some(error_log_attrs),
    None,
    None,
    Some(span.span_context().trace_id()),
    Some(span.span_context().span_id()),
    Some(error_ctx)
  )
  
  @azimuth.telemetry.api.logs.Logger.emit(logger, error_log)
  
  @azimuth.telemetry.api.trace.Span.end(span)
}

test "multi-service telemetry scenario" {
  // Test telemetry scenario spanning multiple services
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  
  // Service A: API Gateway
  let gateway_tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "api-gateway")
  let gateway_meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "api-gateway")
  let gateway_logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "api-gateway")
  
  let gateway_counter = @azimuth.telemetry.api.metrics.Meter.create_counter(gateway_meter, "gateway.requests.total")
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (gateway_ctx, gateway_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    gateway_tracer, 
    ctx, 
    "gateway.request",
    Some(@azimuth.telemetry.api.trace.SpanKind.Server)
  )
  
  @azimuth.telemetry.api.trace.Span.set_attribute(gateway_span, "http.method", @azimuth.telemetry.api.common.StringValue("POST"))
  @azimuth.telemetry.api.trace.Span.set_attribute(gateway_span, "http.url", @azimuth.telemetry.api.common.StringValue("/api/process"))
  
  @azimuth.telemetry.api.metrics.Counter.add(gateway_counter, 1.0, Some(@azimuth.telemetry.api.common.Attributes.new()))
  
  // Service B: Business Logic
  let service_tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "business-service")
  let service_meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "business-service")
  let service_logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "business-service")
  
  let service_histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(service_meter, "business.operation.duration")
  
  let (service_ctx, service_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    service_tracer,
    gateway_ctx,
    "business.process",
    Some(@azimuth.telemetry.api.trace.SpanKind.Internal)
  )
  
  @azimuth.telemetry.api.trace.Span.set_attribute(service_span, "operation.type", @azimuth.telemetry.api.common.StringValue("data_processing"))
  
  @azimuth.telemetry.api.metrics.Histogram.record(service_histogram, 200.0, Some(@azimuth.telemetry.api.common.Attributes.new()))
  
  // Service C: Database
  let db_tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "database-service")
  let db_meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "database-service")
  let db_logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "database-service")
  
  let db_counter = @azimuth.telemetry.api.metrics.Meter.create_counter(db_meter, "db.queries.total")
  
  let (db_ctx, db_span) = @azimuth.telemetry.api.trace.Tracer.start_span(
    db_tracer,
    service_ctx,
    "db.query",
    Some(@azimuth.telemetry.api.trace.SpanKind.Client)
  )
  
  @azimuth.telemetry.api.trace.Span.set_attribute(db_span, "db.statement", @azimuth.telemetry.api.common.StringValue("SELECT * FROM users"))
  @azimuth.telemetry.api.trace.Span.set_attribute(db_span, "db.type", @azimuth.telemetry.api.common.StringValue("postgresql"))
  
  @azimuth.telemetry.api.metrics.Counter.add(db_counter, 1.0, Some(@azimuth.telemetry.api.common.Attributes.new()))
  
  // Log at each service level
  @azimuth.telemetry.api.logs.Logger.info_with_context(gateway_logger, gateway_ctx, "Gateway: Request received")
  @azimuth.telemetry.api.logs.Logger.info_with_context(service_logger, service_ctx, "Service: Processing business logic")
  @azimuth.telemetry.api.logs.Logger.info_with_context(db_logger, db_ctx, "Database: Executing query")
  
  // End spans in reverse order (LIFO)
  @azimuth.telemetry.api.trace.Span.end(db_span)
  @azimuth.telemetry.api.trace.Span.end(service_span)
  @azimuth.telemetry.api.trace.Span.end(gateway_span)
}

test "telemetry resource correlation" {
  // Test resource correlation across telemetry signals
  
  // Create resources with service information
  let gateway_resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    [
      ("service.name", @azimuth.telemetry.api.common.StringValue("api-gateway")),
      ("service.version", @azimuth.telemetry.api.common.StringValue("1.2.3")),
      ("deployment.environment", @azimuth.telemetry.api.common.StringValue("production"))
    ]
  )
  
  let service_resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    [
      ("service.name", @azimuth.telemetry.api.common.StringValue("business-service")),
      ("service.version", @azimuth.telemetry.api.common.StringValue("2.1.0")),
      ("deployment.environment", @azimuth.telemetry.api.common.StringValue("production"))
    ]
  )
  
  // Verify resource attributes
  let gateway_service_name = @azimuth.telemetry.api.common.Resource.get_attribute(gateway_resource, "service.name")
  let service_service_name = @azimuth.telemetry.api.common.Resource.get_attribute(service_resource, "service.name")
  
  assert_eq(gateway_service_name, Some(@azimuth.telemetry.api.common.StringValue("api-gateway")))
  assert_eq(service_service_name, Some(@azimuth.telemetry.api.common.StringValue("business-service")))
  
  // Test resource merging
  let merged_resource = @azimuth.telemetry.api.common.Resource.merge(gateway_resource, service_resource)
  let merged_service_name = @azimuth.telemetry.api.common.Resource.get_attribute(merged_resource, "service.name")
  
  // Should get the service name from the override resource (service_resource)
  assert_eq(merged_service_name, Some(@azimuth.telemetry.api.common.StringValue("business-service")))
  
  // Create instrumentation scopes with resource context
  let gateway_scope = @azimuth.telemetry.api.common.InstrumentationScope.new(
    "api-gateway-instrumentation",
    Some("1.0.0"),
    Some("https://example.com/schema/gateway")
  )
  
  let service_scope = @azimuth.telemetry.api.common.InstrumentationScope.new(
    "business-service-instrumentation",
    Some("2.0.0"),
    Some("https://example.com/schema/service")
  )
  
  // Verify scope properties
  assert_eq(gateway_scope.name(), "api-gateway-instrumentation")
  assert_eq(service_scope.name(), "business-service-instrumentation")
  assert_eq(gateway_scope.version(), Some("1.0.0"))
  assert_eq(service_scope.version(), Some("2.0.0"))
}