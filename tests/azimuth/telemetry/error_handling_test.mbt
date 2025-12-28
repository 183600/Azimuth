test "span context with invalid trace id" {
  // Test span context creation with invalid trace ID
  let invalid_trace_ctx = SpanContext::new("", "span123", true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  let invalid_span_ctx = SpanContext::new("trace123", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  let both_invalid_ctx = SpanContext::new("", "", true, "")
  assert_false(SpanContext::is_valid(both_invalid_ctx))
}

test "context operations with None values" {
  // Test context operations with None values
  let root_ctx = Context::root()
  let key = ContextKey::new("test.key")
  
  // Getting non-existent key should return None
  let none_value = Context::get(root_ctx, key)
  assert_eq(none_value, None)
}

test "attributes with non-existent keys" {
  // Test getting attributes with non-existent keys
  let attrs = Attributes::new()
  
  let non_existent = Attributes::get(attrs, "non.existent.key")
  assert_eq(non_existent, None)
}

test "baggage operations with empty baggage" {
  // Test baggage operations with empty baggage
  let empty_baggage = Baggage::new()
  
  // Getting entries from empty baggage should return None
  let entry = Baggage::get_entry(empty_baggage, "any.key")
  assert_eq(entry, None)
  
  // Removing from empty baggage should not fail
  let after_removal = Baggage::remove_entry(empty_baggage, "any.key")
  assert_true(true)  // Test passes if no exception
}

test "text map carrier with empty headers" {
  // Test text map carrier operations with empty headers
  let empty_carrier = TextMapCarrier::new()
  
  // Getting from empty carrier should return None
  let header = TextMapCarrier::get(empty_carrier, "any.header")
  assert_eq(header, None)
}

test "propagator with empty carrier" {
  // Test propagator operations with empty carrier
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let empty_carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Operations should not fail with empty carrier
  CompositePropagator::inject(composite, ctx, empty_carrier)
  let extracted_ctx = CompositePropagator::extract(composite, empty_carrier)
  
  assert_true(true)  // Test passes if no exceptions
}

test "meter operations with noop provider" {
  // Test meter operations with noop provider
  let noop_provider = MeterProvider::noop()
  let meter = MeterProvider::get_meter(noop_provider, "noop.test")
  
  let counter = Meter::create_counter(meter, "noop.counter")
  let histogram = Meter::create_histogram(meter, "noop.histogram")
  
  // Operations should not fail with noop provider
  Counter::add(counter, 42.0)
  Histogram::record(histogram, 123.45)
  
  assert_true(true)  // Test passes if no exceptions
}

test "logger operations with noop provider" {
  // Test logger operations with noop provider
  let noop_provider = LoggerProvider::noop()
  let logger = LoggerProvider::get_logger(noop_provider, "noop.logger")
  
  let record = LogRecord::new(Info, "Noop test message")
  
  // Emit should not fail with noop provider
  Logger::emit(logger, record)
  
  assert_true(true)  // Test passes if no exceptions
}

test "span operations with ended span" {
  // Test span operations after span is ended
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test.span", Internal, span_ctx)
  
  // End the span
  Span::end(span)
  
  // Operations after ending should not fail
  Span::add_event(span, "event after end")
  Span::set_status(span, Error, Some("Error after end"))
  
  assert_true(true)  // Test passes if no exceptions
}

test "resource with invalid attribute operations" {
  // Test resource operations with invalid scenarios
  let resource = Resource::new()
  
  // Get non-existent attribute
  let non_existent = Resource::get_attribute(resource, "non.existent")
  assert_eq(non_existent, None)
  
  // Merge with empty resource
  let empty_resource = Resource::new()
  let merged = Resource::merge(resource, empty_resource)
  
  assert_true(true)  // Test passes if no exceptions
}