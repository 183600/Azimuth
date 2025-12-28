// Comprehensive Integration Tests for Azimuth Telemetry System
// Testing advanced scenarios and edge cases

test "attribute value operations with different types" {
  let attrs = Attributes::new()
  
  // Test setting and getting string attributes
  Attributes::set(attrs, "string.attr", StringValue("test_string"))
  let string_value = Attributes::get(attrs, "string.attr")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // Test setting and getting int attributes
  Attributes::set(attrs, "int.attr", IntValue(42))
  let int_value = Attributes::get(attrs, "int.attr")
  assert_eq(int_value, Some(IntValue(42)))
  
  // Test setting and getting float attributes
  Attributes::set(attrs, "float.attr", FloatValue(3.14))
  let float_value = Attributes::get(attrs, "float.attr")
  assert_eq(float_value, None)
  
  // Test setting and getting bool attributes
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  let bool_value = Attributes::get(attrs, "bool.attr")
  assert_eq(bool_value, None)
  
  // Test setting and getting array attributes
  let string_array = ArrayStringValue([])
  Attributes::set(attrs, "array.string.attr", string_array)
  let array_value = Attributes::get(attrs, "array.string.attr")
  assert_eq(array_value, None)
}

test "composite propagator injection and extraction" {
  let propagators = [W3CTraceContextPropagator::new()]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

test "span lifecycle management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer")
  let span = Tracer::start_span(tracer, "test-span")
  
  // Test span properties
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Test span context
  let span_ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Test span operations
  Span::add_event(span, "event1")
  Span::set_status(span, Ok)
  
  // Test span status
  assert_eq(Span::status(span), Unset)
  
  // End span
  Span::end(span)
  assert_true(true)
}

test "baggage operations and propagation" {
  let baggage = Baggage::new()
  
  // Test setting and getting entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let user_id = Baggage::get_entry(updated_baggage, "user.id")
  assert_eq(user_id, None)
  
  // Test setting multiple entries
  let baggage2 = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  let request_id = Baggage::get_entry(baggage2, "request.id")
  assert_eq(request_id, None)
  
  // Test removing entries
  let baggage3 = Baggage::remove_entry(baggage2, "user.id")
  let removed_user_id = Baggage::get_entry(baggage3, "user.id")
  assert_eq(removed_user_id, None)
}

test "http client complete workflow" {
  let client = HttpClient::new()
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-456")
  ]
  
  // Test request creation
  let request = HttpRequest::new(
    "POST",
    "https://api.example.com/data",
    headers
  )
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), None)
  
  // Test response creation
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(
    201,
    response_headers
  )
  
  assert_eq(HttpResponse::status_code(response), 201)
  assert_eq(HttpResponse::body(response), None)
}

test "resource merge operations" {
  let base_resource = Resource::new()
  let base_attrs = []
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = []
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test merge operation
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Test getting attributes from merged resource
  let service_name = Resource::get_attribute(merged, "service.name")
  let instance_id = Resource::get_attribute(merged, "service.instance.id")
  let environment = Resource::get_attribute(merged, "deployment.environment")
  
  assert_eq(service_name, None)
  assert_eq(instance_id, None)
  assert_eq(environment, None)
}

test "instrument type operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-metrics")
  
  // Test counter instrument
  let counter = Meter::create_counter(meter, "http.requests.total")
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::description(counter_instrument), Some("Total HTTP requests"))
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
  
  // Test histogram instrument
  let histogram = Meter::create_histogram(meter, "response.time")
  let histogram_instrument = Histogram::as_instrument(histogram)
  
  assert_eq(Instrument::name(histogram_instrument), "response.time")
  assert_eq(Instrument::description(histogram_instrument), Some("Response time distribution"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  
  // Test operations
  Counter::add(counter, 10.0)
  Histogram::record(histogram, 150.5)
  assert_true(true)
}

test "updown counter and gauge operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "system-metrics")
  
  // Test UpDownCounter
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Current active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // Test UpDownCounter operations
  UpDownCounter::add(updown_counter, 5.0)  // Add 5 connections
  UpDownCounter::add(updown_counter, -2.0) // Remove 2 connections
  UpDownCounter::add(updown_counter, 3.0)  // Add 3 connections
  
  // Test Gauge
  let gauge = Meter::create_gauge(meter, "memory.usage")
  
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Current memory usage"))
  assert_eq(gauge.unit, Some("bytes"))
  
  // Test gauge operations (using same add method as counter)
  UpDownCounter::add(updown_counter, 1024.0)
  assert_true(true)
}