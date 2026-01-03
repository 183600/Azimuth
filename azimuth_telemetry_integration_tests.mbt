// Telemetry Integration Tests for Azimuth Telemetry System
// This file contains tests for end-to-end telemetry integration scenarios

test "trace and metrics integration" {
  // Create providers
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  
  // Create tracer and meter
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration-test")
  let meter = MeterProvider::get_meter(meter_provider, "integration-test")
  
  // Create instruments
  let counter = Meter::create_counter(meter, "http.requests.total")
  let histogram = Meter::create_histogram(meter, "http.request.duration")
  
  // Start a span
  let span = Tracer::start_span(tracer, "http.request")
  
  // Add span attributes
  Span::add_event(span, "request.started", Some([("http.method", StringValue("GET")), ("http.url", StringValue("https://api.example.com/data"))]))
  
  // Update metrics
  Counter::add(counter, 1.0, Some(Attributes::new()))
  
  // Simulate processing
  Span::add_event(span, "processing.started")
  
  // Record processing time
  Histogram::record(histogram, 150.0, Some(Attributes::new()))
  
  // Complete request
  Span::add_event(span, "request.completed")
  Span::set_status(span, Ok)
  Span::end(span)
  
  // Verify integration
  assert_eq(span.name, "http.request")
  assert_eq(counter.name, "http.requests.total")
  assert_eq(histogram.name, "http.request.duration")
}

test "trace and logging integration" {
  // Create providers
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // Create tracer and logger
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration-test")
  let logger = LoggerProvider::get_logger(logger_provider, "integration-test")
  
  // Start a span
  let span = Tracer::start_span(tracer, "business.process")
  let span_ctx = Span::span_context(span)
  
  // Create log record with trace context
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Business process started"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    Some(Context::root())
  )
  
  // Emit log
  Logger::emit(logger, log_record)
  
  // Add span event
  Span::add_event(span, "process.step.completed", Some([("step", StringValue("validation"))]))
  
  // Create another log record
  let log_record2 = LogRecord::new_with_context(
    Debug,
    Some("Processing step completed"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record2)
  
  // Complete span
  Span::set_status(span, Ok)
  Span::end(span)
  
  // Verify integration
  assert_eq(span.name, "business.process")
  assert_eq(LogRecord::body(log_record), Some("Business process started"))
  assert_eq(LogRecord::trace_id(log_record), Some(SpanContext::trace_id(span_ctx)))
  assert_eq(LogRecord::span_id(log_record), Some(SpanContext::span_id(span_ctx)))
}

test "metrics and logging integration" {
  // Create providers
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // Create meter and logger
  let meter = MeterProvider::get_meter(meter_provider, "integration-test")
  let logger = LoggerProvider::get_logger(logger_provider, "integration-test")
  
  // Create instruments
  let counter = Meter::create_counter(meter, "operations.completed")
  let gauge = Meter::create_gauge(meter, "system.memory.usage")
  
  // Perform operation
  Counter::add(counter, 1.0, Some(Attributes::new()))
  
  // Log operation completion
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Operation completed successfully"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record)
  
  // Update gauge
  // Note: Gauge doesn't have a set method in simplified implementation
  // In real implementation, would set gauge value
  
  // Log gauge update
  let log_record2 = LogRecord::new_with_context(
    Debug,
    Some("Memory usage gauge updated"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record2)
  
  // Verify integration
  assert_eq(counter.name, "operations.completed")
  assert_eq(gauge.name, "system.memory.usage")
  assert_eq(LogRecord::body(log_record), Some("Operation completed successfully"))
}

test "context propagation integration" {
  // Create providers
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // Create tracer, meter, and logger
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration-test")
  let meter = MeterProvider::get_meter(meter_provider, "integration-test")
  let logger = LoggerProvider::get_logger(logger_provider, "integration-test")
  
  // Create propagator
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // Start parent span
  let parent_span = Tracer::start_span(tracer, "parent.operation")
  let parent_ctx = Context::root()
  let key = ContextKey::new("operation.context")
  let ctx_with_value = Context::with_value(parent_ctx, key, "parent.context.value")
  
  // Inject context
  CompositePropagator::inject(propagator, ctx_with_value, carrier)
  
  // Extract context in child operation
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // Start child span
  let child_span = Tracer::start_span(tracer, "child.operation")
  let child_span_ctx = Span::span_context(child_span)
  
  // Create metrics with context
  let counter = Meter::create_counter(meter, "child.operations")
  Counter::add(counter, 1.0, Some(Attributes::new()))
  
  // Create log with trace context
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Child operation started"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(child_span_ctx)),
    Some(SpanContext::span_id(child_span_ctx)),
    Some(extracted_ctx)
  )
  
  Logger::emit(logger, log_record)
  
  // Complete child operation
  Span::set_status(child_span, Ok)
  Span::end(child_span)
  
  // Complete parent operation
  Span::set_status(parent_span, Ok)
  Span::end(parent_span)
  
  // Verify integration
  assert_eq(parent_span.name, "parent.operation")
  assert_eq(child_span.name, "child.operation")
  assert_eq(counter.name, "child.operations")
  assert_eq(LogRecord::body(log_record), Some("Child operation started"))
}

test "resource integration" {
  // Create resource
  let resource = Resource::new()
  let resource_attrs = [
    ("service.name", StringValue("integration-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
  
  // Create providers with resource context
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // Create tracer, meter, and logger
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration-test")
  let meter = MeterProvider::get_meter(meter_provider, "integration-test")
  let logger = LoggerProvider::get_logger(logger_provider, "integration-test")
  
  // Create span with resource context
  let span = Tracer::start_span(tracer, "resource.aware.operation")
  
  // Add resource context to span
  Span::add_event(span, "operation.started", Some([
    ("service.name", StringValue("integration-service")),
    ("service.version", StringValue("1.0.0"))
  ]))
  
  // Create metrics with resource context
  let counter = Meter::create_counter(meter, "resource.aware.operations")
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("integration-service"))
  Counter::add(counter, 1.0, Some(attrs))
  
  // Create log with resource context
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Resource-aware operation completed"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record)
  
  // Complete span
  Span::set_status(span, Ok)
  Span::end(span)
  
  // Verify integration
  assert_eq(span.name, "resource.aware.operation")
  assert_eq(counter.name, "resource.aware.operations")
  assert_eq(LogRecord::body(log_record), Some("Resource-aware operation completed"))
  
  // Verify resource attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(s)) => assert_eq(s, "integration-service")
    _ => assert_true(false)
  }
}

test "end-to-end request flow" {
  // Simulate complete HTTP request flow
  
  // Initialize telemetry
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "http-server")
  let meter = MeterProvider::get_meter(meter_provider, "http-server")
  let logger = LoggerProvider::get_logger(logger_provider, "http-server")
  
  // Create metrics
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_counter = Meter::create_counter(meter, "http.responses.total")
  let duration_histogram = Meter::create_histogram(meter, "http.request.duration")
  
  // Start request span
  let request_span = Tracer::start_span(tracer, "http.request")
  
  // Log request received
  let request_log = LogRecord::new_with_context(
    Info,
    Some("HTTP request received"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(request_span))),
    Some(SpanContext::span_id(Span::span_context(request_span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, request_log)
  
  // Increment request counter
  Counter::add(request_counter, 1.0, Some(Attributes::new()))
  
  // Add request event
  Span::add_event(request_span, "request.received", Some([
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("/api/users")),
    ("http.user_agent", StringValue("test-client/1.0"))
  ]))
  
  // Simulate processing
  Span::add_event(request_span, "processing.started")
  
  // Log processing start
  let processing_log = LogRecord::new_with_context(
    Debug,
    Some("Request processing started"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(request_span))),
    Some(SpanContext::span_id(Span::span_context(request_span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, processing_log)
  
  // Simulate database operation
  let db_span = Tracer::start_span(tracer, "database.query")
  Span::add_event(db_span, "query.executed", Some([
    ("db.statement", StringValue("SELECT * FROM users")),
    ("db.type", StringValue("postgresql"))
  ]))
  Span::set_status(db_span, Ok)
  Span::end(db_span)
  
  // Log database operation
  let db_log = LogRecord::new_with_context(
    Debug,
    Some("Database query executed"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(db_span))),
    Some(SpanContext::span_id(Span::span_context(db_span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, db_log)
  
  // Complete processing
  Span::add_event(request_span, "processing.completed")
  
  // Record duration
  Histogram::record(duration_histogram, 250.0, Some(Attributes::new()))
  
  // Increment response counter
  Counter::add(response_counter, 1.0, Some(Attributes::new()))
  
  // Log response sent
  let response_log = LogRecord::new_with_context(
    Info,
    Some("HTTP response sent"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(request_span))),
    Some(SpanContext::span_id(Span::span_context(request_span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, response_log)
  
  // Add response event
  Span::add_event(request_span, "response.sent", Some([
    ("http.status_code", IntValue(200)),
    ("http.response_size", IntValue(1024))
  ]))
  
  // Complete request
  Span::set_status(request_span, Ok)
  Span::end(request_span)
  
  // Verify end-to-end flow
  assert_eq(request_span.name, "http.request")
  assert_eq(db_span.name, "database.query")
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_counter.name, "http.responses.total")
  assert_eq(duration_histogram.name, "http.request.duration")
}

test "cross-service telemetry integration" {
  // Simulate cross-service communication
  
  // Service A telemetry
  let tracer_provider_a = TracerProvider::default()
  let meter_provider_a = MeterProvider::default()
  let logger_provider_a = LoggerProvider::default()
  
  let tracer_a = TracerProvider::get_tracer(tracer_provider_a, "service-a")
  let meter_a = MeterProvider::get_meter(meter_provider_a, "service-a")
  let logger_a = LoggerProvider::get_logger(logger_provider_a, "service-a")
  
  // Service B telemetry
  let tracer_provider_b = TracerProvider::default()
  let meter_provider_b = MeterProvider::default()
  let logger_provider_b = LoggerProvider::default()
  
  let tracer_b = TracerProvider::get_tracer(tracer_provider_b, "service-b")
  let meter_b = MeterProvider::get_meter(meter_provider_b, "service-b")
  let logger_b = LoggerProvider::get_logger(logger_provider_b, "service-b")
  
  // Service A: Start operation
  let span_a = Tracer::start_span(tracer_a, "service.a.operation")
  let span_a_ctx = Span::span_context(span_a)
  
  // Service A: Log operation start
  let log_a = LogRecord::new_with_context(
    Info,
    Some("Service A operation started"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_a_ctx)),
    Some(SpanContext::span_id(span_a_ctx)),
    Some(Context::root())
  )
  
  Logger::emit(logger_a, log_a)
  
  // Service A: Create metrics
  let counter_a = Meter::create_counter(meter_a, "service.a.operations")
  Counter::add(counter_a, 1.0, Some(Attributes::new()))
  
  // Service A: Call Service B
  Span::add_event(span_a, "calling.service.b")
  
  // Propagate context to Service B
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx_a = Context::root()
  let ctx_a_with_trace = Context::with_value(ctx_a, ContextKey::new("trace.id"), SpanContext::trace_id(span_a_ctx))
  
  CompositePropagator::inject(propagator, ctx_a_with_trace, carrier)
  
  // Service B: Extract context
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // Service B: Start operation
  let span_b = Tracer::start_span(tracer_b, "service.b.operation")
  let span_b_ctx = Span::span_context(span_b)
  
  // Service B: Log operation start
  let log_b = LogRecord::new_with_context(
    Info,
    Some("Service B operation started"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_b_ctx)),
    Some(SpanContext::span_id(span_b_ctx)),
    Some(extracted_ctx)
  )
  
  Logger::emit(logger_b, log_b)
  
  // Service B: Create metrics
  let counter_b = Meter::create_counter(meter_b, "service.b.operations")
  Counter::add(counter_b, 1.0, Some(Attributes::new()))
  
  // Service B: Complete operation
  Span::set_status(span_b, Ok)
  Span::end(span_b)
  
  // Service B: Log operation completion
  let log_b_complete = LogRecord::new_with_context(
    Info,
    Some("Service B operation completed"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_b_ctx)),
    Some(SpanContext::span_id(span_b_ctx)),
    Some(extracted_ctx)
  )
  
  Logger::emit(logger_b, log_b_complete)
  
  // Service A: Receive response from Service B
  Span::add_event(span_a, "received.from.service.b")
  
  // Service A: Complete operation
  Span::set_status(span_a, Ok)
  Span::end(span_a)
  
  // Service A: Log operation completion
  let log_a_complete = LogRecord::new_with_context(
    Info,
    Some("Service A operation completed"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_a_ctx)),
    Some(SpanContext::span_id(span_a_ctx)),
    Some(Context::root())
  )
  
  Logger::emit(logger_a, log_a_complete)
  
  // Verify cross-service integration
  assert_eq(span_a.name, "service.a.operation")
  assert_eq(span_b.name, "service.b.operation")
  assert_eq(counter_a.name, "service.a.operations")
  assert_eq(counter_b.name, "service.b.operations")
  assert_eq(LogRecord::body(log_a), Some("Service A operation started"))
  assert_eq(LogRecord::body(log_b), Some("Service B operation started"))
}

test "baggage integration" {
  // Create providers
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "baggage-test")
  let logger = LoggerProvider::get_logger(logger_provider, "baggage-test")
  
  // Create initial baggage
  let baggage = Baggage::new()
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "request.id", "req456")
  let baggage3 = Baggage::set_entry(baggage2, "tenant.id", "tenant789")
  
  // Start span with baggage context
  let span = Tracer::start_span(tracer, "baggage.aware.operation")
  
  // Add baggage context to span
  Span::add_event(span, "operation.started", Some([
    ("user.id", StringValue("user123")),
    ("request.id", StringValue("req456")),
    ("tenant.id", StringValue("tenant789"))
  ]))
  
  // Create log with baggage context
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Operation with baggage context"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record)
  
  // Propagate baggage to next operation
  let next_baggage = Baggage::set_entry(baggage3, "operation.id", "op101")
  
  // Start next operation
  let next_span = Tracer::start_span(tracer, "next.operation")
  
  // Add propagated baggage to next operation
  Span::add_event(next_span, "next.operation.started", Some([
    ("user.id", StringValue("user123")),
    ("request.id", StringValue("req456")),
    ("tenant.id", StringValue("tenant789")),
    ("operation.id", StringValue("op101"))
  ]))
  
  // Complete operations
  Span::set_status(next_span, Ok)
  Span::end(next_span)
  Span::set_status(span, Ok)
  Span::end(span)
  
  // Verify baggage integration
  assert_eq(span.name, "baggage.aware.operation")
  assert_eq(next_span.name, "next.operation")
  assert_eq(LogRecord::body(log_record), Some("Operation with baggage context"))
  
  // Verify baggage entries
  let user_id = Baggage::get_entry(baggage3, "user.id")
  let request_id = Baggage::get_entry(baggage3, "request.id")
  let tenant_id = Baggage::get_entry(baggage3, "tenant.id")
  
  assert_eq(user_id, Some("user123"))
  assert_eq(request_id, Some("req456"))
  assert_eq(tenant_id, Some("tenant789"))
}

test "instrumentation scope integration" {
  // Create instrumentation scopes
  let scope1 = InstrumentationScope::{ name: "scope1", version: Some("1.0.0"), schema_url: Some("https://example.com/schema1") }
  let scope2 = InstrumentationScope::{ name: "scope2", version: Some("2.0.0"), schema_url: Some("https://example.com/schema2") }
  
  // Create providers with scopes
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // Create tracers with different scopes
  let tracer1 = Tracer::{ scope: scope1 }
  let tracer2 = Tracer::{ scope: scope2 }
  
  // Create meters with different scopes
  let meter1 = Meter::{ scope: scope1 }
  let meter2 = Meter::{ scope: scope2 }
  
  // Create loggers with different scopes
  let logger1 = Logger::{ scope: scope1 }
  let logger2 = Logger::{ scope: scope2 }
  
  // Create spans with different scopes
  let span1 = Tracer::start_span(tracer1, "scope1.operation")
  let span2 = Tracer::start_span(tracer2, "scope2.operation")
  
  // Create metrics with different scopes
  let counter1 = Meter::create_counter(meter1, "scope1.counter")
  let counter2 = Meter::create_counter(meter2, "scope2.counter")
  
  // Create logs with different scopes
  let log1 = LogRecord::new(Info, "Scope 1 log message")
  let log2 = LogRecord::new(Info, "Scope 2 log message")
  
  // Emit logs
  Logger::emit(logger1, log1)
  Logger::emit(logger2, log2)
  
  // Update metrics
  Counter::add(counter1, 1.0, Some(Attributes::new()))
  Counter::add(counter2, 1.0, Some(Attributes::new()))
  
  // Complete spans
  Span::end(span1)
  Span::end(span2)
  
  // Verify scope integration
  assert_eq(span1.name, "scope1.operation")
  assert_eq(span2.name, "scope2.operation")
  assert_eq(counter1.name, "scope1.counter")
  assert_eq(counter2.name, "scope2.counter")
  assert_eq(LogRecord::body(log1), Some("Scope 1 log message"))
  assert_eq(LogRecord::body(log2), Some("Scope 2 log message"))
  
  // Verify scope properties
  assert_eq(tracer1.scope.name, "scope1")
  assert_eq(tracer2.scope.name, "scope2")
  assert_eq(meter1.scope.name, "scope1")
  assert_eq(meter2.scope.name, "scope2")
  assert_eq(logger1.scope.name, "scope1")
  assert_eq(logger2.scope.name, "scope2")
}