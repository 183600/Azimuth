// Azimuth Focused Test Suite - Core functionality tests

test "attributes_basic_operations" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  
  let string_val = Attributes::get(attrs, "string.key")
  let int_val = Attributes::get(attrs, "int.key")
  let missing_val = Attributes::get(attrs, "missing.key")
  
  match string_val {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => @expect(false)
  }
  
  match int_val {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => @expect(false)
  }
  
  match missing_val {
    None => @expect(true)
    _ => @expect(false)
  }
}

test "context_value_storage_and_retrieval" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test_key")
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  let missing_value = Context::get(root_ctx, key)
  
  match retrieved_value {
    Some(v) => assert_eq(v, "test_value")
    _ => @expect(false)
  }
  
  match missing_value {
    None => @expect(true)
    _ => @expect(false)
  }
}

test "span_context_creation_and_validation" {
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "state=test")
  
  assert_eq(SpanContext::trace_id(span_ctx), "trace_123")
  assert_eq(SpanContext::span_id(span_ctx), "span_456")
  assert_eq(SpanContext::is_sampled(span_ctx), true)
  assert_eq(SpanContext::is_valid(span_ctx), true)
  
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_eq(SpanContext::is_valid(invalid_ctx), false)
}

test "counter_metric_basic_functionality" {
  let provider = MeterProvider::noop()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let counter = Meter::create_counter(meter, "test_counter")
  
  Counter::add(counter, 1.0)
  
  let instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(instrument), "test_counter")
}

test "log_record_creation_and_properties" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  match LogRecord::body(record) {
    Some(body) => assert_eq(body, "Test log message")
    _ => @expect(false)
  }
  
  let record_with_context = LogRecord::new_with_context(
    Error,
    Some("Error message"),
    None,
    Some(1234567890L),
    Some(1234567891L),
    Some("trace_123"),
    Some("span_456"),
    None
  )
  
  assert_eq(LogRecord::severity_number(record_with_context), Error)
  match LogRecord::trace_id(record_with_context) {
    Some(trace_id) => assert_eq(trace_id, "trace_123")
    _ => @expect(false)
  }
}

test "resource_attribute_management" {
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance_123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => @expect(false)
  }
  
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attr")
  match missing_attr {
    None => @expect(true)
    _ => @expect(false)
  }
}

test "baggage_entry_operations" {
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user_123")
  
  let user_id = Baggage::get_entry(updated_baggage, "user.id")
  match user_id {
    Some(id) => assert_eq(id, "user_123")
    _ => @expect(false)
  }
  
  let missing_entry = Baggage::get_entry(updated_baggage, "missing.key")
  match missing_entry {
    None => @expect(true)
    _ => @expect(false)
  }
}

test "text_map_carrier_header_operations" {
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "custom.header", "custom_value")
  
  let trace_parent = TextMapCarrier::get(carrier, "traceparent")
  match trace_parent {
    Some(trace) => @expect(true)  // Should have the predefined traceparent
    _ => @expect(false)
  }
  
  let missing_header = TextMapCarrier::get(carrier, "missing.header")
  match missing_header {
    None => @expect(true)
    _ => @expect(false)
  }
}

test "span_lifecycle_management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test_tracer")
  let span = Tracer::start_span(tracer, "test_span")
  
  assert_eq(Span::name(span), "test_span")
  assert_eq(Span::kind(span), Internal)
  assert_eq(Span::is_recording(span), true)
  
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "test_event", Some([("event.type", StringValue("test"))]))
  
  let span_ctx = Span::span_context(span)
  assert_eq(SpanContext::is_valid(span_ctx), true)
  
  Span::end(span)
}

test "http_request_response_creation" {
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"test\": \"data\"}"))
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"test\": \"data\"}")
    _ => @expect(false)
  }
  
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\": \"success\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "{\"result\": \"success\"}")
    _ => @expect(false)
  }
}