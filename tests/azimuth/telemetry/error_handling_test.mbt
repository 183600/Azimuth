test "span context with invalid trace id" {
  // Test span context creation with invalid trace ID
  let invalid_trace_ctx = @azimuth.SpanContext::new("", "span123", true, "")
  assert_false(@azimuth.SpanContext::is_valid(invalid_trace_ctx))
  
  let invalid_span_ctx = @azimuth.SpanContext::new("trace123", "", true, "")
  assert_false(@azimuth.SpanContext::is_valid(invalid_span_ctx))
  
  let both_invalid_ctx = @azimuth.SpanContext::new("", "", true, "")
  assert_false(@azimuth.SpanContext::is_valid(both_invalid_ctx))
}

test "context operations with None values" {
  // Test context operations with None values
  let root_ctx = @azimuth.Context::root()
  let key = @azimuth.ContextKey::new("test.key")
  
  // Getting non-existent key should return None
  let none_value = @azimuth.Context::get(root_ctx, key)
  assert_eq(none_value, None)
}

test "attributes with non-existent keys" {
  // Test getting attributes with non-existent keys
  let attrs = @azimuth.Attributes::new()
  
  let non_existent = @azimuth.Attributes::get(attrs, "non.existent.key")
  assert_eq(non_existent, None)
}

test "baggage operations with empty baggage" {
  // Test baggage operations with empty baggage
  let empty_baggage = @azimuth.Baggage::new()
  
  // Getting entries from empty baggage should return None
  let entry = @azimuth.Baggage::get_entry(empty_baggage, "any.key")
  assert_eq(entry, None)
  
  // Removing from empty baggage should not fail
  let after_removal = @azimuth.Baggage::remove_entry(empty_baggage, "any.key")
  assert_true(true)  // Test passes if no exception
}

test "text map carrier with empty headers" {
  // Test text map carrier operations with empty headers
  let empty_carrier = @azimuth.TextMapCarrier::new()
  
  // Getting from empty carrier should return None
  let header = @azimuth.TextMapCarrier::get(empty_carrier, "any.header")
  assert_eq(header, None)
}

test "propagator with empty carrier" {
  // Test propagator operations with empty carrier
  let trace_propagator = @azimuth.W3CTraceContextPropagator::new()
  let composite = @azimuth.CompositePropagator::new([trace_propagator])
  
  let empty_carrier = @azimuth.TextMapCarrier::new()
  let ctx = @azimuth.Context::root()
  
  // Operations should not fail with empty carrier
  @azimuth.CompositePropagator::inject(composite, ctx, empty_carrier)
  let extracted_ctx = @azimuth.CompositePropagator::extract(composite, empty_carrier)
  
  assert_true(true)  // Test passes if no exceptions
}

test "meter operations with noop provider" {
  // Test meter operations with noop provider
  let noop_provider = @azimuth.MeterProvider::noop()
  let meter = @azimuth.MeterProvider::get_meter(noop_provider, "noop.test")
  
  let counter = @azimuth.Meter::create_counter(meter, "noop.counter")
  let histogram = @azimuth.Meter::create_histogram(meter, "noop.histogram")
  
  // Operations should not fail with noop provider
  @azimuth.Counter::add(counter, 42.0)
  @azimuth.Histogram::record(histogram, 123.45)
  
  assert_true(true)  // Test passes if no exceptions
}

test "logger operations with noop provider" {
  // Test logger operations with noop provider
  let noop_provider = @azimuth.LoggerProvider::noop()
  let logger = @azimuth.LoggerProvider::get_logger(noop_provider, "noop.logger")
  
  let record = @azimuth.LogRecord::new(@azimuth.Info, "Noop test message")
  
  // Emit should not fail with noop provider
  @azimuth.Logger::emit(logger, record)
  
  assert_true(true)  // Test passes if no exceptions
}

test "span operations with ended span" {
  // Test span operations after span is ended
  let span_ctx = @azimuth.SpanContext::new("trace123", "span456", true, "")
  let span = @azimuth.Span::new("test.span", @azimuth.Internal, span_ctx)
  
  // End the span
  @azimuth.Span::end(span)
  
  // Operations after ending should not fail
  @azimuth.Span::add_event(span, "event after end")
  @azimuth.Span::set_status(span, @azimuth.Error, Some("Error after end"))
  
  assert_true(true)  // Test passes if no exceptions
}

test "resource with invalid attribute operations" {
  // Test resource operations with invalid scenarios
  let resource = @azimuth.Resource::new()
  
  // Get non-existent attribute
  let non_existent = @azimuth.Resource::get_attribute(resource, "non.existent")
  assert_eq(non_existent, None)
  
  // Merge with empty resource
  let empty_resource = @azimuth.Resource::new()
  let merged = @azimuth.Resource::merge(resource, empty_resource)
  
  assert_true(true)  // Test passes if no exceptions
}