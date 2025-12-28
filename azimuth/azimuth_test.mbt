test "azimuth telemetry basic setup" {
  // Basic test to ensure Azimuth telemetry project setup is working
  assert_eq(1 + 1, 2)
  assert_true(true)
  assert_false(false)
}

test "azimuth common attributes" {
  // Test basic attribute creation and manipulation
  let attrs = Attributes.new()
  Attributes.set(attrs, "string.key", StringValue("test_value"))
  Attributes.set(attrs, "int.key", IntValue(42))
  
  let string_val = Attributes.get(attrs, "string.key")
  let int_val = Attributes.get(attrs, "int.key")
  
  assert_eq(string_val, Some(StringValue("test_value")))
  assert_eq(int_val, Some(IntValue(42)))
}

test "azimuth context operations" {
  // Test basic context operations
  let ctx = Context.root()
  let key = ContextKey[String]("test.key")
  
  let ctx_with_value = Context.with_value(ctx, key, "test_value")
  let retrieved_value = Context.get(ctx_with_value, key)
  
  assert_eq(retrieved_value, Some("test_value"))
}

test "azimuth span context" {
  // Test span context creation
  let span_ctx = SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,  // sampled
    "congo=t61rcWkgMzE"
  )
  
  assert_eq(span_ctx.trace_id(), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(span_ctx.span_id(), "b7ad6b7169203331")
  assert_true(span_ctx.is_valid())
  assert_true(span_ctx.is_sampled())
}

test "azimuth metrics counter" {
  // Test counter instrument
  let meter_provider = MeterProvider.noop()
  let meter = MeterProvider.get_meter(meter_provider, "test.meter")
  let counter = Meter.create_counter(meter, "test.counter")
  
  Counter.add(counter, 1.0)
  Counter.add(counter, 2.5)
  
  assert_eq(Instrument.name(counter), "test.counter")
}

test "azimuth logging operations" {
  // Test basic logging operations
  let logger_provider = LoggerProvider.noop()
  let logger = LoggerProvider.get_logger(logger_provider, "test.logger")
  
  let log_record = LogRecord.new(
    SeverityNumber.Info,
    "Test log message",
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(log_record.severity_number(), SeverityNumber.Info)
  assert_eq(log_record.body(), Some("Test log message"))
}

test "azimuth platform time" {
  // Test platform time abstraction
  let clock = Clock.system()
  let time = Clock.now_unix_nanos(clock)
  
  assert_true(time > 0)
  assert_true(time > 1700000000000000000)  // After 2023
}

test "azimuth platform random" {
  // Test platform random abstraction
  let random = Random.system()
  let bytes = Random.next_bytes(random, 16)
  
  assert_eq(bytes.length(), 16)
  
  let value = Random.next_u64(random)
  assert_true(value >= 0)
}

test "azimuth propagation" {
  // Test basic propagation operations
  let carrier = TextMapCarrier.new()
  TextMapCarrier.set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let traceparent = TextMapCarrier.get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
}

test "azimuth span operations" {
  // Test span creation and operations
  let span_ctx = SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  
  let span = Span.new(
    "test.span",
    SpanKind.Server,
    span_ctx
  )
  
  assert_eq(span.name(), "test.span")
  assert_eq(span.kind(), SpanKind.Server)
  assert_true(span.is_recording())
  assert_eq(span.span_context(), span_ctx)
  
  // Test span status
  span.set_status(StatusCode.Ok, "Operation completed")
  assert_eq(span.status(), StatusCode.Ok)
  
  // Test span events
  span.add_event("test.event", [
    ("event.key", StringValue("event.value"))
  ])
  
  span.end()
  assert_false(span.is_recording())
}

test "azimuth tracer functionality" {
  // Test tracer creation and span generation
  let tracer_provider = TracerProvider.default()
  let tracer = TracerProvider.get_tracer(tracer_provider, "test.tracer", Some("1.0.0"))
  
  assert_eq(tracer.instrumentation_scope().name(), "test.tracer")
  assert_eq(tracer.instrumentation_scope().version(), Some("1.0.0"))
  
  // Test span creation with tracer
  let span = Tracer.start_span(tracer, "operation.name", [
    ("operation.type", StringValue("http"))
  ])
  
  assert_eq(span.name(), "operation.name")
  assert_true(span.is_recording())
}

test "azimuth histogram metrics" {
  // Test histogram instrument
  let meter_provider = MeterProvider.default()
  let meter = MeterProvider.get_meter(meter_provider, "test.histogram.meter")
  let histogram = Meter.create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  Histogram.record(histogram, 100.0)
  Histogram.record(histogram, 200.0)
  Histogram.record(histogram, 150.0)
  
  assert_eq(Instrument.name(Histogram.as_instrument(histogram)), "test.histogram")
  assert_eq(Instrument.description(Histogram.as_instrument(histogram)), Some("Test histogram"))
  assert_eq(Instrument.unit(Histogram.as_instrument(histogram)), Some("ms"))
}

test "azimuth baggage operations" {
  // Test baggage creation and manipulation
  let baggage = Baggage.new()
  
  // Add entries
  let baggage_with_entries = Baggage.set_entry(baggage, "user.id", "12345")
  let updated_baggage = Baggage.set_entry(baggage_with_entries, "request.id", "req-67890")
  
  // Get entries
  let user_id = Baggage.get_entry(updated_baggage, "user.id")
  let request_id = Baggage.get_entry(updated_baggage, "request.id")
  let missing = Baggage.get_entry(updated_baggage, "nonexistent")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(missing, None)
  
  // Remove entry
  let final_baggage = Baggage.remove_entry(updated_baggage, "user.id")
  assert_eq(Baggage.get_entry(final_baggage, "user.id"), None)
  assert_eq(Baggage.get_entry(final_baggage, "request.id"), Some("req-67890"))
}

test "azimuth composite propagator" {
  // Test composite propagator with multiple propagators
  let trace_propagator = W3CTraceContextPropagator.new()
  let baggage_propagator = W3CBaggagePropagator.new()
  
  let composite = CompositePropagator.new([
    trace_propagator,
    baggage_propagator
  ])
  
  let carrier = TextMapCarrier.new()
  
  // Inject context
  let ctx = Context.root()
  CompositePropagator.inject(composite, ctx, carrier)
  
  // Extract context
  let extracted_ctx = CompositePropagator.extract(composite, carrier)
  
  assert_true(extracted_ctx != ctx)  // Should have additional context
}

test "azimuth http client operations" {
  // Test HTTP client functionality
  let client = HttpClient.new()
  
  let request = HttpRequest.new(
    "GET",
    "https://api.example.com/data",
    [
      ("Content-Type", "application/json"),
      ("Authorization", "Bearer token123")
    ],
    Some("{\"key\":\"value\"}")
  )
  
  assert_eq(request.http_method(), "GET")
  assert_eq(request.url(), "https://api.example.com/data")
  assert_eq(request.body(), Some("{\"key\":\"value\"}"))
  
  // Mock response
  let response = HttpResponse.new(
    200,
    [
      ("Content-Type", "application/json"),
      ("X-Request-ID", "req-12345")
    ],
    Some("{\"result\":\"success\"}")
  )
  
  assert_eq(response.status_code(), 200)
  assert_eq(response.body(), Some("{\"result\":\"success\"}"))
}

test "azimuth log record with full context" {
  // Test comprehensive log record creation
  let logger_provider = LoggerProvider.default()
  let logger = LoggerProvider.get_logger(logger_provider, "comprehensive.logger")
  
  let span_ctx = SpanContext.new(
    "trace123456789",
    "span987654321",
    true,
    "key1=value1"
  )
  
  let attributes = Attributes.new()
  Attributes.set(attributes, "log.level", StringValue("info"))
  Attributes.set(attributes, "component", StringValue("auth"))
  
  let log_record = LogRecord.new_with_context(
    SeverityNumber.Warn,
    Some("Authentication failed for user"),
    Some(attributes),
    Some(1735689600000000000L),  // timestamp
    Some(1735689600000000001L),  // observed timestamp
    Some(span_ctx.trace_id()),
    Some(span_ctx.span_id()),
    Some(Context.root())
  )
  
  assert_eq(log_record.severity_number(), SeverityNumber.Warn)
  assert_eq(log_record.body(), Some("Authentication failed for user"))
  assert_eq(log_record.trace_id(), Some("trace123456789"))
  assert_eq(log_record.span_id(), Some("span987654321"))
  
  Logger.emit(logger, log_record)
}

test "azimuth resource merging" {
  // Test resource attribute merging
  let base_resource = Resource.new()
  let base_with_attrs = Resource.with_attributes(base_resource, [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("production"))
  ])
  
  let override_resource = Resource.new()
  let override_with_attrs = Resource.with_attributes(override_resource, [
    ("service.name", StringValue("override-service")),  // This should override
    ("deployment.region", StringValue("us-west-2"))
  ])
  
  let merged = Resource.merge(base_with_attrs, override_with_attrs)
  
  // Check that service.name was overridden
  assert_eq(Resource.get_attribute(merged, "service.name"), Some(StringValue("override-service")))
  
  // Check that original attributes are preserved
  assert_eq(Resource.get_attribute(merged, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource.get_attribute(merged, "environment"), Some(StringValue("production")))
  
  // Check that new attributes were added
  assert_eq(Resource.get_attribute(merged, "deployment.region"), Some(StringValue("us-west-2")))
}
