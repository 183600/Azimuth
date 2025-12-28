test "comprehensive telemetry workflow" {
  // Test comprehensive telemetry workflow combining all components
  
  // Initialize all providers with custom configuration
  let resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    [
      ("service.name", @azimuth.telemetry.api.common.StringValue("comprehensive-test-service")),
      ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
      ("test.suite", @azimuth.telemetry.api.common.StringValue("comprehensive"))
    ]
  )
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.with_resource(resource)
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.with_resource(resource)
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.with_resource(resource)
  
  // Create instruments with full configuration
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(
    trace_provider, 
    "comprehensive.tracer", 
    Some("1.0.0"), 
    Some("https://example.com/schema/v1")
  )
  
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(
    metrics_provider, 
    "comprehensive.meter", 
    Some("1.0.0"), 
    Some("https://example.com/schema/v1")
  )
  
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(
    logs_provider, 
    "comprehensive.logger", 
    Some("1.0.0"), 
    Some("https://example.com/schema/v1")
  )
  
  // Create various metric instruments
  let request_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(
    meter, 
    "http.requests.total", 
    Some("Total HTTP requests"), 
    Some("requests")
  )
  
  let duration_histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(
    meter, 
    "http.request.duration", 
    Some("HTTP request duration"), 
    Some("ms")
  )
  
  let active_connections = @azimuth.telemetry.sdk.metrics.Meter.create_up_down_counter(
    meter, 
    "http.active_connections", 
    Some("Active HTTP connections"), 
    Some("connections")
  )
  
  let memory_usage = @azimuth.telemetry.sdk.metrics.Meter.create_gauge(
    meter, 
    "process.memory.usage", 
    Some("Process memory usage"), 
    Some("percent")
  )
  
  // Start main operation trace
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (main_ctx, main_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "main.operation")
  
  // Log start of operation
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, main_ctx, "Starting comprehensive telemetry test")
  
  // Update metrics
  @azimuth.telemetry.sdk.metrics.Counter.add(request_counter, 1.0)
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(active_connections, 1.0)
  @azimuth.telemetry.sdk.metrics.Gauge.record(memory_usage, 45.5)
  
  // Simulate sub-operation with child span
  let (sub_ctx, sub_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, main_ctx, "sub.operation.database")
  
  let sub_start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  // Add attributes to sub-span
  @azimuth.telemetry.api.trace.Span.set_attribute(sub_span, "db.operation", @azimuth.telemetry.api.common.StringValue("SELECT"))
  @azimuth.telemetry.api.trace.Span.set_attribute(sub_span, "db.table", @azimuth.telemetry.api.common.StringValue("users"))
  @azimuth.telemetry.api.trace.Span.set_attribute(sub_span, "db.rows", @azimuth.telemetry.api.common.IntValue(42))
  
  // Log sub-operation
  @azimuth.telemetry.sdk.logs.Logger.debug_with_context(logger, sub_ctx, "Executing database query")
  
  // Simulate another level of nesting
  let (query_ctx, query_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, sub_ctx, "query.execution")
  
  @azimuth.telemetry.api.trace.Span.set_attribute(query_span, "query.type", @azimuth.telemetry.api.common.StringValue("prepared_statement"))
  @azimuth.telemetry.api.trace.Span.add_event(query_span, "query.start", None, None)
  
  // Record query duration
  let query_end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let query_duration = (query_end_time - sub_start_time).to_double() / 1000000.0
  
  @azimuth.telemetry.sdk.metrics.Histogram.record(duration_histogram, query_duration)
  
  @azimuth.telemetry.api.trace.Span.add_event(query_span, "query.complete", None, None)
  @azimuth.telemetry.api.trace.Span.set_status(query_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Query completed successfully"))
  
  @azimuth.telemetry.sdk.logs.Logger.debug_with_context(logger, query_ctx, "Query execution completed")
  
  @azimuth.telemetry.api.trace.Span.end(query_span)
  
  // End sub-operation
  @azimuth.telemetry.api.trace.Span.set_status(sub_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Database operation completed"))
  @azimuth.telemetry.api.trace.Span.end(sub_span)
  
  // Simulate API call
  let (api_ctx, api_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, main_ctx, "api.call.external")
  
  @azimuth.telemetry.api.trace.Span.set_attribute(api_span, "http.method", @azimuth.telemetry.api.common.StringValue("POST"))
  @azimuth.telemetry.api.trace.Span.set_attribute(api_span, "http.url", @azimuth.telemetry.api.common.StringValue("https://api.example.com/webhook"))
  @azimuth.telemetry.api.trace.Span.set_attribute(api_span, "http.status_code", @azimuth.telemetry.api.common.IntValue(200))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, api_ctx, "Making external API call")
  
  // Record API call metrics
  let api_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(api_attrs, "endpoint", @azimuth.telemetry.api.common.StringValue("/webhook"))
  @azimuth.telemetry.api.common.Attributes.set(api_attrs, "status", @azimuth.telemetry.api.common.StringValue("200"))
  
  @azimuth.telemetry.sdk.metrics.Counter.add(request_counter, 1.0, Some(api_attrs))
  @azimuth.telemetry.sdk.metrics.Histogram.record(duration_histogram, 150.5, Some(api_attrs))
  
  @azimuth.telemetry.api.trace.Span.set_status(api_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("API call successful"))
  @azimuth.telemetry.api.trace.Span.end(api_span)
  
  // Simulate error scenario
  let (error_ctx, error_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, main_ctx, "error.prone.operation")
  
  @azimuth.telemetry.api.trace.Span.set_attribute(error_span, "operation.type", @azimuth.telemetry.api.common.StringValue("validation"))
  
  @azimuth.telemetry.sdk.logs.Logger.warn_with_context(logger, error_ctx, "Attempting error-prone operation")
  
  // Record exception
  @azimuth.telemetry.api.trace.Span.record_exception(
    error_span,
    "Validation failed for user input",
    Some("ValidationError"),
    Some("at validation.validate_input()\n  at process_request()"),
    Some([
      ("input.field", @azimuth.telemetry.api.common.StringValue("email")),
      ("error.code", @azimuth.telemetry.api.common.StringValue("INVALID_FORMAT"))
    ])
  )
  
  @azimuth.telemetry.api.trace.Span.set_status(error_span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Validation failed"))
  
  // Log error with structured data
  let error_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(error_attrs, "error.type", @azimuth.telemetry.api.common.StringValue("ValidationError"))
  @azimuth.telemetry.api.common.Attributes.set(error_attrs, "error.message", @azimuth.telemetry.api.common.StringValue("Invalid email format"))
  @azimuth.telemetry.api.common.Attributes.set(error_attrs, "user.id", @azimuth.telemetry.api.common.StringValue("12345"))
  
  @azimuth.telemetry.sdk.logs.Logger.error_with_attributes(logger, "Validation error occurred", error_attrs)
  
  // Record error metrics
  let error_metric_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(error_metric_attrs, "error.type", @azimuth.telemetry.api.common.StringValue("validation"))
  @azimuth.telemetry.api.common.Attributes.set(error_metric_attrs, "operation", @azimuth.telemetry.api.common.StringValue("input_validation"))
  
  @azimuth.telemetry.sdk.metrics.Counter.add(request_counter, 1.0, Some(error_metric_attrs))
  
  @azimuth.telemetry.api.trace.Span.end(error_span)
  
  // Update final metrics
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(active_connections, -1.0)
  @azimuth.telemetry.sdk.metrics.Gauge.record(memory_usage, 52.3)
  
  // Log completion
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(logger, main_ctx, "Comprehensive telemetry test completed")
  
  // End main span
  @azimuth.telemetry.api.trace.Span.set_status(main_span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("All operations completed"))
  @azimuth.telemetry.api.trace.Span.end(main_span)
}

test "telemetry context propagation across multiple operations" {
  // Test context propagation across multiple operations and services
  
  // Initialize service A
  let service_a_tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(
    @azimuth.telemetry.sdk.trace.TracerProvider.default(), 
    "service-a"
  )
  let service_a_meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(
    @azimuth.telemetry.sdk.metrics.MeterProvider.default(), 
    "service-a"
  )
  let service_a_logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(
    @azimuth.telemetry.sdk.logs.LoggerProvider.default(), 
    "service-a"
  )
  
  let service_a_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(service_a_meter, "service.a.requests")
  
  // Service A: Start operation
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (service_a_ctx, service_a_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(
    service_a_tracer, 
    ctx, 
    "service-a.process-request"
  )
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(service_a_logger, service_a_ctx, "Service A: Processing request")
  @azimuth.telemetry.sdk.metrics.Counter.add(service_a_counter, 1.0)
  
  // Add baggage
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  let baggage_with_data = @azimuth.telemetry.api.context.Baggage.set_entry(baggage, "user.id", "12345")
  let baggage_with_session = @azimuth.telemetry.api.context.Baggage.set_entry(baggage_with_data, "session.id", "abcdef12345")
  
  // Inject context for propagation
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  let propagator = @azimuth.telemetry.api.propagation.CompositePropagator.new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new(),
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator.new()
  ])
  
  @azimuth.telemetry.api.propagation.CompositePropagator.inject(propagator, service_a_ctx, carrier)
  
  // Service B: Extract and continue
  let service_b_tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(
    @azimuth.telemetry.sdk.trace.TracerProvider.default(), 
    "service-b"
  )
  let service_b_meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(
    @azimuth.telemetry.sdk.metrics.MeterProvider.default(), 
    "service-b"
  )
  let service_b_logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(
    @azimuth.telemetry.sdk.logs.LoggerProvider.default(), 
    "service-b"
  )
  
  let service_b_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(service_b_meter, "service.b.requests")
  
  let service_b_ctx = @azimuth.telemetry.api.propagation.CompositePropagator.extract(propagator, carrier)
  let (service_b_ctx_with_span, service_b_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(
    service_b_tracer, 
    service_b_ctx, 
    "service-b.process-data"
  )
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(service_b_logger, service_b_ctx_with_span, "Service B: Processing data")
  @azimuth.telemetry.sdk.metrics.Counter.add(service_b_counter, 1.0)
  
  // Service B calls Service C
  let carrier_b_to_c = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  @azimuth.telemetry.api.propagation.CompositePropagator.inject(propagator, service_b_ctx_with_span, carrier_b_to_c)
  
  // Service C: Extract and continue
  let service_c_tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(
    @azimuth.telemetry.sdk.trace.TracerProvider.default(), 
    "service-c"
  )
  let service_c_logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(
    @azimuth.telemetry.sdk.logs.LoggerProvider.default(), 
    "service-c"
  )
  
  let service_c_ctx = @azimuth.telemetry.api.propagation.CompositePropagator.extract(propagator, carrier_b_to_c)
  let (service_c_ctx_with_span, service_c_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(
    service_c_tracer, 
    service_c_ctx, 
    "service-c.finalize"
  )
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_context(service_c_logger, service_c_ctx_with_span, "Service C: Finalizing operation")
  
  // End spans in reverse order
  @azimuth.telemetry.api.trace.Span.end(service_c_span)
  @azimuth.telemetry.api.trace.Span.end(service_b_span)
  @azimuth.telemetry.api.trace.Span.end(service_a_span)
}

test "telemetry with complex attribute scenarios" {
  // Test telemetry with complex attribute scenarios and data types
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "complex.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "complex.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "complex.test.logger")
  
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "complex.test.counter")
  
  // Create complex attributes
  let complex_attrs = @azimuth.telemetry.api.common.Attributes.new()
  
  // String attributes with various content
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "empty.string", @azimuth.telemetry.api.common.StringValue(""))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "normal.string", @azimuth.telemetry.api.common.StringValue("normal_value"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "unicode.string", @azimuth.telemetry.api.common.StringValue("Unicode: 🚀 测试 🌟"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "special.chars", @azimuth.telemetry.api.common.StringValue("Special: !@#$%^&*()_+-=[]{}|;':\",./<>?"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "json.string", @azimuth.telemetry.api.common.StringValue("{\"key\": \"value\", \"number\": 42}"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "multiline.string", @azimuth.telemetry.api.common.StringValue("Line 1\nLine 2\nLine 3"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "very.long.string", @azimuth.telemetry.api.common.StringValue("x".repeat(1000)))
  
  // Numeric attributes
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "zero.int", @azimuth.telemetry.api.common.IntValue(0))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "positive.int", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "negative.int", @azimuth.telemetry.api.common.IntValue(-999))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "max.int", @azimuth.telemetry.api.common.IntValue(2147483647))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "min.int", @azimuth.telemetry.api.common.IntValue(-2147483648))
  
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "zero.float", @azimuth.telemetry.api.common.FloatValue(0.0))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "positive.float", @azimuth.telemetry.api.common.FloatValue(3.14159))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "negative.float", @azimuth.telemetry.api.common.FloatValue(-2.71828))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "very.small.float", @azimuth.telemetry.api.common.FloatValue(0.000001))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "very.large.float", @azimuth.telemetry.api.common.FloatValue(999999.999))
  
  // Boolean attributes
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "true.value", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "false.value", @azimuth.telemetry.api.common.BoolValue(false))
  
  // Array attributes
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "empty.string.array", @azimuth.telemetry.api.common.ArrayStringValue([]))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "single.string.array", @azimuth.telemetry.api.common.ArrayStringValue(["single"]))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "multiple.string.array", @azimuth.telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "unicode.string.array", @azimuth.telemetry.api.common.ArrayStringValue(["测试1", "🚀", "test3"]))
  
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "empty.int.array", @azimuth.telemetry.api.common.ArrayIntValue([]))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "single.int.array", @azimuth.telemetry.api.common.ArrayIntValue([42]))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "multiple.int.array", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3, 4, 5]))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "mixed.int.array", @azimuth.telemetry.api.common.ArrayIntValue([0, -1, 999, -999]))
  
  // Create span with complex attributes
  let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "complex.attributes.span")
  
  // Set all complex attributes on span
  for i in 0..20 {
    // This would normally set all attributes, but simplified implementation may not support it
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "attr." + i.to_string(), @azimuth.telemetry.api.common.StringValue("value." + i.to_string()))
  }
  
  // Add events with complex attributes
  let event_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.type", @azimuth.telemetry.api.common.StringValue("complex_event"))
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.data", @azimuth.telemetry.api.common.StringValue("{\"complex\": \"data\", \"nested\": {\"key\": \"value\"}}"))
  
  @azimuth.telemetry.api.trace.Span.add_event(span, "complex.event", Some([("event.type", @azimuth.telemetry.api.common.StringValue("complex_event"))]), None)
  
  // Record metric with complex attributes
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0, Some(complex_attrs))
  
  // Create log record with complex attributes
  let log_record = @azimuth.telemetry.sdk.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Log with complex attributes",
    Some(complex_attrs),
    Some(@azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())),
    None,
    Some("complex_trace_123"),
    Some("complex_span_456"),
    None
  )
  
  @azimuth.telemetry.sdk.logs.Logger.emit(logger, log_record)
  
  @azimuth.telemetry.api.trace.Span.end(span)
}

test "telemetry system resilience and recovery" {
  // Test telemetry system resilience and recovery scenarios
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "resilience.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "resilience.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "resilience.test.logger")
  
  let error_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "resilience.errors")
  let recovery_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "resilience.recoveries")
  
  // Test resilience with rapid span creation and termination
  for i in 0..100 {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "resilience.span." + i.to_string())
    
    if i % 10 == 0 {
      // Simulate error condition
      @azimuth.telemetry.api.trace.Span.record_exception(
        span,
        "Simulated error " + i.to_string(),
        Some("ResilienceTestError"),
        Some("at resilience.test()\n  at main()"),
        None
      )
      
      @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Simulated error"))
      @azimuth.telemetry.sdk.metrics.Counter.add(error_counter, 1.0)
      
      @azimuth.telemetry.sdk.logs.Logger.error(logger, "Simulated error " + i.to_string())
    } else {
      @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Operation successful"))
      @azimuth.telemetry.sdk.metrics.Counter.add(recovery_counter, 1.0)
      
      @azimuth.telemetry.sdk.logs.Logger.info(logger, "Successful operation " + i.to_string())
    }
    
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  // Test resilience with invalid data
  let (_, invalid_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "")
  
  @azimuth.telemetry.api.trace.Span.set_attribute(invalid_span, "", @azimuth.telemetry.api.common.StringValue(""))
  @azimuth.telemetry.api.trace.Span.add_event(invalid_span, "", None, None)
  @azimuth.telemetry.api.trace.Span.record_exception(invalid_span, "", None, None, None)
  
  @azimuth.telemetry.api.trace.Span.end(invalid_span)
  
  // Test resilience with large datasets
  let large_attrs = @azimuth.telemetry.api.common.Attributes.new()
  for i in 0..1000 {
    let key = "large.attr." + i.to_string()
    let value = @azimuth.telemetry.api.common.StringValue("large.value." + i.to_string() + " with additional content")
    @azimuth.telemetry.api.common.Attributes.set(large_attrs, key, value)
  }
  
  let (_, large_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "large.data.span")
  
  for i in 0..100 {
    @azimuth.telemetry.api.trace.Span.add_event(large_span, "large.event." + i.to_string(), Some([("event.data", @azimuth.telemetry.api.common.StringValue("x".repeat(1000)))]), None)
  }
  
  @azimuth.telemetry.sdk.metrics.Counter.add(error_counter, 999.0, Some(large_attrs))
  
  let large_log_record = @azimuth.telemetry.sdk.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "x".repeat(10000),
    Some(large_attrs),
    Some(@azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())),
    None,
    Some("large_trace_123"),
    Some("large_span_456"),
    None
  )
  
  @azimuth.telemetry.sdk.logs.Logger.emit(logger, large_log_record)
  
  @azimuth.telemetry.api.trace.Span.end(large_span)
  
  // Log resilience test completion
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "Resilience test completed successfully")
}