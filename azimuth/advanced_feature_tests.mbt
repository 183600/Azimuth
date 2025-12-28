// Simple Feature Tests for Azimuth Telemetry System
// Focus on testing core functionality without complex type comparisons

test "basic attribute value creation" {
  // Test creating different attribute value types
  let string_attr = StringValue("test_string")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  let bool_attr = BoolValue(true)
  
  // Test attribute value matching
  match string_attr {
    StringValue(s) => assert_eq(s, "test_string")
    _ => assert_true(false)
  }
  
  match int_attr {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  match float_attr {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_true(false)
  }
  
  match bool_attr {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
}

test "context operations" {
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  // Test context value retrieval
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
  
  // Test missing key
  let missing_value = Context::get(ctx, ContextKey::new("missing.key"))
  assert_eq(missing_value, None)
}

test "span context operations" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // Test basic properties
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
}

test "span creation and basic operations" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Test initial span state
  assert_eq(Span::name(span), "test-span")
  assert_true(Span::is_recording(span))
  
  // Test span operations
  Span::add_event(span, "event1")
  Span::set_status(span, Ok)
  Span::end(span)
  
  assert_true(true) // Test passes if no errors occurred
}

test "text map carrier operations" {
  let carrier = TextMapCarrier::new()
  
  // Test setting headers
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Test getting headers
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let missing = TextMapCarrier::get(carrier, "missing-header")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(missing, None)
}

test "propagator operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_trace, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Test extraction
  let carrier_with_trace = TextMapCarrier::new()
  TextMapCarrier::set(carrier_with_trace, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  let extracted_ctx = CompositePropagator::extract(composite, carrier_with_trace)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "log record creation" {
  let record = LogRecord::new(Info, "Test log message")
  
  // Test log record properties
  let severity = LogRecord::severity_number(record)
  match severity {
    Info => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(LogRecord::body(record), Some("Test log message"))
}

test "metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests")
  
  // Test counter properties
  assert_eq(counter.name, "http.requests")
  
  // Test counter operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  
  assert_true(true) // Test passes if no errors occurred
}

test "resource operations" {
  let resource = Resource::new()
  
  // Test empty resource
  assert_eq(resource.attributes.length(), 0)
  
  // Test resource with attributes
  let attrs = [("service.name", StringValue("api-service")), ("service.version", StringValue("1.0.0"))]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  assert_eq(resource_with_attrs.attributes.length(), 2)
}

test "tracer operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer")
  
  // Test tracer properties
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  
  // Test span creation
  let span = Tracer::start_span(tracer, "operation")
  assert_eq(Span::name(span), "operation")
  assert_true(Span::is_recording(span))
}