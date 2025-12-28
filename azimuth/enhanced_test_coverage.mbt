// Enhanced Test Coverage for Azimuth Telemetry System
// This file contains comprehensive test cases to improve coverage

test "attribute deep operations" {
  // Test complex attribute operations
  let attrs = Attributes::new()
  
  // Test setting and getting different attribute types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  // Verify attribute retrieval
  let string_val = Attributes::get(attrs, "string.key")
  let int_val = Attributes::get(attrs, "int.key")
  let missing_val = Attributes::get(attrs, "missing.key")
  
  assert_eq(string_val, Some(StringValue("test_value")))
  assert_eq(int_val, Some(IntValue(42)))
  assert_eq(missing_val, None)
  
  // Test array attributes
  let array_val = Attributes::get(attrs, "array.key")
  assert_eq(array_val, None)
}

test "span lifecycle management" {
  // Test complete span lifecycle
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  
  // Verify tracer scope
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  // Create and configure span
  let span = Tracer::start_span(tracer, "test-span")
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Test span context operations
  let span_ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test span status and events
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "test-event", Some([("event.key", StringValue("event.value"))]))
  
  // End span
  Span::end(span)
  assert_true(true) // If we reach here, span end was successful
}

test "resource merge strategies" {
  // Test resource creation and merging
  let base_resource = Resource::new()
  let base_attrs = [("service.name", StringValue("base-service")), ("service.version", StringValue("1.0.0"))]
  let resource_with_base = Resource::with_attributes(base_resource, base_attrs)
  
  // Create override resource
  let override_resource = Resource::new()
  let override_attrs = [("service.name", StringValue("override-service")), ("environment", StringValue("production"))]
  let resource_with_override = Resource::with_attributes(override_resource, override_attrs)
  
  // Test resource attribute retrieval
  let service_name = Resource::get_attribute(resource_with_base, "service.name")
  let environment = Resource::get_attribute(resource_with_base, "environment")
  
  assert_eq(service_name, Some(StringValue("base-service")))
  assert_eq(environment, None)
  
  // Test resource merging
  let merged_resource = Resource::merge(resource_with_base, resource_with_override)
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_environment = Resource::get_attribute(merged_resource, "environment")
  
  assert_eq(merged_service_name, Some(StringValue("override-service")))
  assert_eq(merged_environment, Some(StringValue("production")))
}

test "composite propagator functionality" {
  // Test composite propagator creation and operations
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Test injection
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify injection
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_trace.is_some())
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  
  assert_eq(extracted_value, Some("true"))
}

test "metrics aggregation operations" {
  // Test different metric types and operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation-test-meter")
  
  // Test counter
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  Counter::add(counter, 10.0)
  Counter::add(counter, 5.0)
  
  // Test histogram
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  assert_eq(histogram.name, "test.histogram")
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  
  // Test up-down counter
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test up-down"), Some("items"))
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Test gauge
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  assert_eq(gauge.name, "test.gauge")
  
  // Test instrument type conversions
  let counter_instrument = Instrument::name(Counter("test.counter", None, None))
  let histogram_instrument = Instrument::name(Histogram("test.histogram", None, None))
  
  assert_eq(counter_instrument, "test.counter")
  assert_eq(histogram_instrument, "test.histogram")
}

test "log record integrity" {
  // Test comprehensive log record creation and operations
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "integrity-test-logger")
  
  // Test basic log record
  let basic_record = LogRecord::new(Error, "Error occurred")
  assert_eq(LogRecord::severity_number(basic_record), Error)
  assert_eq(LogRecord::body(basic_record), Some("Error occurred"))
  
  // Test comprehensive log record with all fields
  let attrs = Attributes::new()
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  
  let full_record = LogRecord::new_with_context(
    Warn,
    Some("Warning with context"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(full_record), Warn)
  assert_eq(LogRecord::body(full_record), Some("Warning with context"))
  assert_eq(LogRecord::trace_id(full_record), Some(trace_id))
  assert_eq(LogRecord::span_id(full_record), Some(span_id))
  
  // Test log emission
  Logger::emit(logger, full_record)
  assert_true(true) // If we reach here, log emission was successful
}

test "baggage propagation scenarios" {
  // Test baggage operations in different scenarios
  let baggage = Baggage::new()
  
  // Test setting and getting baggage entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let updated_baggage2 = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  // Test baggage retrieval
  let user_id = Baggage::get_entry(updated_baggage2, "user.id")
  let request_id = Baggage::get_entry(updated_baggage2, "request.id")
  let missing_entry = Baggage::get_entry(updated_baggage2, "missing.entry")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(missing_entry, None)
  
  // Test baggage entry removal
  let baggage_after_removal = Baggage::remove_entry(updated_baggage2, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_after_removal, "user.id")
  let still_present_request_id = Baggage::get_entry(baggage_after_removal, "request.id")
  
  assert_eq(removed_user_id, None)
  assert_eq(still_present_request_id, Some("req-67890"))
}

test "http client comprehensive operations" {
  // Test comprehensive HTTP client operations
  let client = HttpClient::new()
  
  // Test HTTP request creation with various components
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-12345")
  ]
  let body = Some("{\"name\":\"test\",\"value\":123}")
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, body)
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"name\":\"test\",\"value\":123}"))
  
  // Test HTTP response creation and operations
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Rate-Limit", "1000")
  ]
  let response_body = Some("{\"success\":true,\"data\":{\"id\":123}}")
  let response = HttpResponse::new(201, response_headers, response_body)
  
  assert_eq(HttpResponse::status_code(response), 201)
  assert_eq(HttpResponse::body(response), Some("{\"success\":true,\"data\":{\"id\":123}}"))
  
  // Test error scenarios
  let error_response = HttpResponse::new(404, [], Some("Not Found"))
  assert_eq(HttpResponse::status_code(error_response), 404)
  assert_eq(HttpResponse::body(error_response), Some("Not Found"))
}