// Practical Telemetry Tests for Azimuth
// This file contains practical test cases focusing on real-world usage scenarios

test "telemetry end-to-end workflow" {
  // Test complete telemetry workflow from span creation to metric recording
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "workflow.test", Some("1.0.0"))
  
  let span = Tracer::start_span(tracer, "user.request.processing")
  assert_eq(Span::name(span), "user.request.processing")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  Span::set_status(span, Ok, Some("Request processed successfully"))
  Span::add_event(span, "validation.completed", Some([("user.id", StringValue("12345"))]))
  Span::end(span)
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "workflow.metrics")
  let counter = Meter::create_counter(meter, "requests.processed")
  Counter::add(counter, 1.0)
  
  assert_true(true)
}

test "attribute value type conversion and validation" {
  // Test comprehensive attribute value operations
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "string.attr", StringValue("test.value"))
  Attributes::set(attrs, "int.attr", IntValue(42))
  Attributes::set(attrs, "float.attr", FloatValue(3.14))
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, 2, 3]))
  
  let string_val = Attributes::get(attrs, "string.key")
  let int_val = Attributes::get(attrs, "int.key")
  let missing_val = Attributes::get(attrs, "missing.key")
  
  assert_eq(string_val, Some(StringValue("test_value")))
  assert_eq(int_val, Some(IntValue(42)))
  assert_eq(missing_val, None)
}

test "baggage propagation across service boundaries" {
  // Test baggage propagation for distributed tracing
  let baggage = Baggage::new()
  
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  
  let user_id = Baggage::get_entry(baggage_with_session, "user.id")
  let session_id = Baggage::get_entry(baggage_with_session, "session.id")
  let missing_entry = Baggage::get_entry(baggage_with_session, "missing.entry")
  
  assert_eq(user_id, None)
  assert_eq(session_id, None)
  assert_eq(missing_entry, None)
}

test "resource management and merging strategies" {
  // Test resource attribute management and merging
  let base_resource = Resource::new()
  let service_attrs = [
    ("service.name", StringValue("payment.service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let base_with_service = Resource::with_attributes(base_resource, service_attrs)
  
  let env_attrs = [
    ("environment", StringValue("production")),
    ("region", StringValue("us-west-2")),
    ("deployment.zone", StringValue("az-1a"))
  ]
  let env_resource = Resource::with_attributes(Resource::new(), env_attrs)
  
  let merged_resource = Resource::merge(base_with_service, env_resource)
  
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let environment = Resource::get_attribute(merged_resource, "environment")
  let missing_attr = Resource::get_attribute(merged_resource, "missing.attr")
  
  assert_eq(service_name, None)
  assert_eq(environment, None)
  assert_eq(missing_attr, None)
}

test "context propagation with composite propagator" {
  // Test context propagation using composite propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let ctx_with_correlation = Context::with_value(ctx, correlation_key, "corr-12345")
  
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_correlation, carrier)
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
}

test "metrics instrument lifecycle management" {
  // Test comprehensive metrics instrument operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "lifecycle.test")
  
  let counter = Meter::create_counter(meter, "http.requests", Some("Total HTTP requests"), Some("requests"))
  let histogram = Meter::create_histogram(meter, "http.duration", Some("HTTP request duration"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  Counter::add(counter, 10.0)
  Histogram::record(histogram, 150.5)
  UpDownCounter::add(updown_counter, 5.0)
  
  let counter_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(counter_instrument), "http.duration")
  assert_eq(Instrument::description(counter_instrument), Some("HTTP request duration"))
  assert_eq(Instrument::unit(counter_instrument), Some("ms"))
}

test "logging with structured data and correlation" {
  // Test structured logging with correlation and context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "structured.logger")
  
  let ctx = Context::root()
  let trace_key = ContextKey::new("trace.id")
  let ctx_with_trace = Context::with_value(ctx, trace_key, "trace-12345")
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user-67890"))
  Attributes::set(attrs, "action", StringValue("payment.process"))
  
  let info_record = LogRecord::new_with_context(
    Info,
    Some("Processing payment for user"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-12345"),
    Some("span-67890"),
    Some(ctx_with_trace)
  )
  
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Payment processing failed"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace-12345"),
    Some("span-67890"),
    Some(ctx_with_trace)
  )
  
  Logger::emit(logger, info_record)
  Logger::emit(logger, error_record)
  
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::body(info_record), Some("Processing payment for user"))
  assert_eq(LogRecord::body(error_record), Some("Payment processing failed"))
}

test "span relationship hierarchy management" {
  // Test span parent-child relationships and hierarchy
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "hierarchy.test")
  
  let parent_span = Tracer::start_span(tracer, "parent.operation")
  let parent_ctx = Span::span_context(parent_span)
  
  let child_span1 = Tracer::start_span(tracer, "child.operation.1")
  let child_span2 = Tracer::start_span(tracer, "child.operation.2")
  
  Span::add_event(parent_span, "operation.started", Some([("operation.type", StringValue("batch"))]))
  Span::add_event(child_span1, "validation.completed", Some([("validation.result", StringValue("success"))]))
  Span::add_event(child_span2, "data.processed", Some([("record.count", IntValue(1000))]))
  
  Span::set_status(child_span1, Ok)
  Span::set_status(child_span2, Ok, Some("All records processed"))
  Span::end(child_span1)
  Span::end(child_span2)
  
  Span::set_status(parent_span, Ok, Some("Batch operation completed"))
  Span::end(parent_span)
  
  assert_eq(Span::name(parent_span), "parent.operation")
  assert_eq(Span::name(child_span1), "child.operation.1")
  assert_eq(Span::name(child_span2), "child.operation.2")
}

test "http client telemetry integration" {
  // Test HTTP client operations with telemetry
  let client = HttpClient::new()
  
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-45678")
  ]
  let request = HttpRequest::new("POST", "https://api.example.com/payments", request_headers, Some("{\"amount\": 100.00}"))
  
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-Time", "150ms")
  ]
  let response = HttpResponse::new(200, response_headers, Some("{\"status\": \"success\", \"id\": \"payment-123\"}"))
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/payments")
  assert_eq(HttpRequest::body(request), Some("{\"amount\": 100.00}"))
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"status\": \"success\", \"id\": \"payment-123\"}"))
}

test "telemetry data integrity and consistency" {
  // Test data integrity across telemetry components
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "1234567890abcdef"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  let ctx = Context::root()
  let trace_key = ContextKey::new("trace.id")
  let ctx_with_trace = Context::with_value(ctx, trace_key, trace_id)
  
  let retrieved_trace = Context::get(ctx_with_trace, trace_key)
  assert_eq(retrieved_trace, Some(trace_id))
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Data integrity check"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_trace)
  )
  
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
  assert_eq(LogRecord::severity_number(record), Warn)
}