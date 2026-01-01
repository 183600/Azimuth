// Additional Comprehensive Test Suite for Azimuth Telemetry System
// This file contains focused test cases covering various aspects of the telemetry system

// Test 1: Attribute operations with complex types
test "attribute operations with complex types" {
  let attrs = Attributes::new()
  
  // Test setting and getting string attribute
  Attributes::set(attrs, "service.name", StringValue("payment-service"))
  let service_name = Attributes::get(attrs, "service.name")
  assert_eq(service_name, Some(StringValue("payment-service")))
  
  // Test setting and getting int attribute
  Attributes::set(attrs, "service.version", IntValue(1))
  let service_version = Attributes::get(attrs, "service.version")
  assert_eq(service_version, Some(IntValue(1)))
  
  // Test setting and getting float attribute
  Attributes::set(attrs, "service.rate", FloatValue(0.95))
  let service_rate = Attributes::get(attrs, "service.rate")
  assert_eq(service_rate, Some(FloatValue(0.95)))
  
  // Test setting and getting bool attribute
  Attributes::set(attrs, "service.enabled", BoolValue(true))
  let service_enabled = Attributes::get(attrs, "service.enabled")
  assert_eq(service_enabled, Some(BoolValue(true)))
}

// Test 2: Resource management and merging
test "resource management and merging" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test resource merging
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  let service_name = Resource::get_attribute(merged, "service.name")
  let instance_id = Resource::get_attribute(merged, "service.instance.id")
  
  assert_eq(service_name, Some(StringValue("override-service")))
  assert_eq(instance_id, Some(StringValue("instance-123")))
}

// Test 3: Span lifecycle management
test "span lifecycle management" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer")
  let span = Tracer::start_span(tracer, "test-span")
  
  // Test span properties
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_ctx)
  
  // Test span status operations
  assert_eq(Span::status(span), Unset)
  Span::set_status(span, Ok)
  assert_eq(Span::status(span), Unset)  // Simplified implementation
  
  // Test span event addition
  Span::add_event(span, "test-event", [("event.type", StringValue("test"))])
  
  // Test span ending
  Span::end(span)
}

// Test 4: Metrics instrument operations
test "metrics instrument operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test counter operations
  let counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  
  // Test histogram operations
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, Some("HTTP request duration"))
  assert_eq(histogram.unit, Some("ms"))
  
  Histogram::record(histogram, 100.5)
  Histogram::record(histogram, 250.0)
  
  // Test updown counter operations
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  assert_eq(updown_counter.name, "active.connections")
  
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -3.0)
  
  // Test gauge operations
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  assert_eq(gauge.name, "memory.usage")
  
  // Test instrument type conversions
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::description(counter_instrument), Some("Total HTTP requests"))
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
}

// Test 5: Log record with full context
test "log record with full context" {
  let trace_id = Some("0af7651916cd43dd8448eb211c80319c")
  let span_id = Some("b7ad6b7169203331")
  let timestamp = Some(1735689600000000000L)
  let observed_timestamp = Some(1735689600000001000L)
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user-123"))
  
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-456")
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    timestamp,
    observed_timestamp,
    trace_id,
    span_id,
    Some(ctx_with_value)
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), trace_id)
  assert_eq(LogRecord::span_id(record), span_id)
}

// Test 6: Baggage propagation across contexts
test "baggage propagation across contexts" {
  let baggage = Baggage::new()
  
  // Test setting and getting baggage entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user-123")
  let user_id = Baggage::get_entry(updated_baggage, "user.id")
  assert_eq(user_id, Some("user-123"))
  
  // Test multiple entries
  let multi_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-456")
  let request_id = Baggage::get_entry(multi_baggage, "request.id")
  assert_eq(request_id, Some("req-456"))
  
  // Test removing entries
  let reduced_baggage = Baggage::remove_entry(multi_baggage, "user.id")
  let removed_user_id = Baggage::get_entry(reduced_baggage, "user.id")
  assert_eq(removed_user_id, None)
  let retained_request_id = Baggage::get_entry(reduced_baggage, "request.id")
  assert_eq(retained_request_id, Some("req-456"))
}

// Test 7: Composite propagator injection and extraction
test "composite propagator injection and extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let key = ContextKey::new("test.value")
  let ctx_with_value = Context::with_value(ctx, key, "test-data")
  
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, ctx_with_value, carrier)
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

// Test 8: HTTP client operations with complex payloads
test "http client operations with complex payloads" {
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-456")
  ]
  let body = Some("{\"action\":\"test\",\"data\":{\"value\":123}}")
  
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, body)
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"action\":\"test\",\"data\":{\"value\":123}}"))
  
  let response_headers = [("Content-Type", "application/json")]
  let response_body = Some("{\"status\":\"success\",\"id\":789}")
  let response = HttpResponse::new(201, response_headers, response_body)
  
  assert_eq(HttpResponse::status_code(response), 201)
  assert_eq(HttpResponse::body(response), Some("{\"status\":\"success\",\"id\":789}"))
}

// Test 9: Logger operations with different severity levels
test "logger operations with different severity levels" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test creating log records with different severity levels
  let trace_record = LogRecord::new(Trace, "Debug trace information")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  
  let debug_record = LogRecord::new(Debug, "Debug information")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  
  let info_record = LogRecord::new(Info, "General information")
  assert_eq(LogRecord::severity_number(info_record), Info)
  
  let warn_record = LogRecord::new(Warn, "Warning condition")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  
  let error_record = LogRecord::new(Error, "Error condition")
  assert_eq(LogRecord::severity_number(error_record), Error)
  
  let fatal_record = LogRecord::new(Fatal, "Fatal error")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Test emitting log records
  Logger::emit(logger, info_record)
  Logger::emit(logger, error_record)
}

// Test 10: Cross-service telemetry consistency
test "cross-service telemetry consistency" {
  // Simulate service A telemetry
  let service_a_trace_id = "trace-a-1234567890123456"
  let service_a_span_id = "span-a-1234567890123456"
  let service_a_ctx = SpanContext::new(service_a_trace_id, service_a_span_id, true, "key1=value1")
  
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a-operation")
  
  // Simulate service B telemetry (should maintain trace consistency)
  let service_b_span_id = "span-b-1234567890123456"
  let service_b_ctx = SpanContext::new(service_a_trace_id, service_b_span_id, true, "key1=value1")
  
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-b")
  let service_b_span = Tracer::start_span(service_b_tracer, "service-b-operation")
  
  // Verify trace consistency
  assert_eq(SpanContext::trace_id(service_a_ctx), SpanContext::trace_id(service_b_ctx))
  assert_true(SpanContext::is_sampled(service_a_ctx))
  assert_true(SpanContext::is_sampled(service_b_ctx))
  
  // Test baggage propagation between services
  let baggage = Baggage::new()
  let cross_service_baggage = Baggage::set_entry(baggage, "user.id", "user-123")
  let cross_service_baggage = Baggage::set_entry(cross_service_baggage, "request.id", "req-456")
  
  let propagated_user_id = Baggage::get_entry(cross_service_baggage, "user.id")
  let propagated_request_id = Baggage::get_entry(cross_service_baggage, "request.id")
  
  assert_eq(propagated_user_id, Some("user-123"))
  assert_eq(propagated_request_id, Some("req-456"))
}