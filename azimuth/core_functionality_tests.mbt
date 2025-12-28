// Core Functionality Tests for Azimuth Telemetry System
// Comprehensive test coverage for essential telemetry features

test "attribute_value_type_conversion" {
  // Test different attribute value types
  let string_val = StringValue("test_string")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14)
  let bool_val = BoolValue(true)
  let array_string_val = ArrayStringValue(["item1", "item2"])
  let array_int_val = ArrayIntValue([1, 2, 3])
  
  // Verify type preservation
  match string_val {
    StringValue(s) => assert_eq(s, "test_string")
    _ => @test.fail("Expected StringValue")
  }
  
  match int_val {
    IntValue(i) => assert_eq(i, 42)
    _ => @test.fail("Expected IntValue")
  }
  
  match float_val {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_val {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
}

test "span_context_validation" {
  // Test valid span context
  let valid_ctx = SpanContext::new("trace123", "span456", true, "state=1")
  assert_eq(SpanContext::trace_id(valid_ctx), "trace123")
  assert_eq(SpanContext::span_id(valid_ctx), "span456")
  assert_eq(SpanContext::is_sampled(valid_ctx), true)
  assert_eq(SpanContext::is_valid(valid_ctx), true)
  
  // Test invalid span context (empty IDs)
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_eq(SpanContext::is_valid(invalid_ctx), false)
  assert_eq(SpanContext::is_sampled(invalid_ctx), false)
}

test "context_value_operations" {
  let root_ctx = Context::root()
  let test_key = ContextKey::new("test_key")
  
  // Test context with value
  let ctx_with_value = Context::with_value(root_ctx, test_key, "test_value")
  assert_eq(Context::get(ctx_with_value, test_key), Some("test_value"))
  
  // Test context without value
  let empty_key = ContextKey::new("empty_key")
  assert_eq(Context::get(ctx_with_value, empty_key), None)
  
  // Test root context
  assert_eq(Context::get(root_ctx, test_key), None)
}

test "instrument_creation_and_properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Test counter instrument
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  
  assert_eq(Instrument::name(counter_instrument), "test_counter")
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  
  // Test histogram instrument
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  
  assert_eq(Instrument::name(histogram_instrument), "test_histogram")
  assert_eq(Instrument::description(histogram_instrument), Some("Test histogram"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
}

test "log_record_creation_and_accessors" {
  // Test basic log record
  let basic_log = LogRecord::new(Info, "Test log message")
  assert_eq(LogRecord::severity_number(basic_log), Info)
  assert_eq(LogRecord::body(basic_log), Some("Test log message"))
  
  // Test log record with full context
  let full_log = LogRecord::new_with_context(
    Error,
    Some("Error message"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123"),
    Some("span456"),
    None
  )
  
  assert_eq(LogRecord::severity_number(full_log), Error)
  assert_eq(LogRecord::body(full_log), Some("Error message"))
  assert_eq(LogRecord::trace_id(full_log), Some("trace123"))
  assert_eq(LogRecord::span_id(full_log), Some("span456"))
}

test "span_lifecycle_operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test_tracer")
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  // Test span creation
  let span = Tracer::start_span(tracer, "test_span")
  assert_eq(Span::name(span), "test_span")
  assert_eq(Span::kind(span), Internal)
  assert_eq(Span::is_recording(span), true)
  
  // Test span operations
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "test_event", None)
  
  // Test span ending
  Span::end(span)
}

test "resource_attribute_management" {
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Test attribute retrieval
  assert_eq(
    Resource::get_attribute(resource_with_attrs, "service.name"),
    Some(StringValue("test_service"))
  )
  
  assert_eq(
    Resource::get_attribute(resource_with_attrs, "service.version"),
    Some(StringValue("1.0.0"))
  )
  
  // Test non-existent attribute
  assert_eq(
    Resource::get_attribute(resource_with_attrs, "non.existent"),
    None
  )
}

test "baggage_operations" {
  let baggage = Baggage::new()
  
  // Test baggage entry operations
  let baggage_with_entry = Baggage::set_entry(baggage, "user.id", "12345")
  assert_eq(Baggage::get_entry(baggage_with_entry, "user.id"), Some("12345"))
  
  // Test non-existent entry
  assert_eq(Baggage::get_entry(baggage_with_entry, "non.existent"), None)
  
  // Test entry removal
  let baggage_after_removal = Baggage::remove_entry(baggage_with_entry, "user.id")
  assert_eq(Baggage::get_entry(baggage_after_removal, "user.id"), Some("12345"))
}

test "http_client_operations" {
  // Test HTTP request creation
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"key\":\"value\"}"))
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"key\":\"value\"}"))
  
  // Test HTTP response creation
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\":\"success\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"result\":\"success\"}"))
}

test "propagation_context_injection_extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let root_ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, root_ctx, carrier)
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, extracted_key), Some("true"))
}