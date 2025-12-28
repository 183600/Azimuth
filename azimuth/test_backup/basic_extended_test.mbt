// Basic extended tests for telemetry components

test "span context basic validation" {
  // Test valid span context
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_eq(SpanContext::is_valid(valid_ctx), true)
  
  // Test invalid span context with empty trace_id
  let invalid_trace_ctx = SpanContext::new("", "b7ad6b7169203331", true, "")
  assert_eq(SpanContext::is_valid(invalid_trace_ctx), false)
  
  // Test invalid span context with empty span_id
  let invalid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  assert_eq(SpanContext::is_valid(invalid_span_ctx), false)
}

test "context creation and basic operations" {
  let root_ctx = Context::root()
  assert_eq(root_ctx.data, None)
  
  let key = ContextKey::new("test.key")
  assert_eq(key.key, "test.key")
  
  let ctx_with_value = Context::with_value(root_ctx, key, "test.value")
  assert_eq(ctx_with_value.data, Some(("test.key", "test.value")))
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test.value"))
}

test "context chaining operations" {
  let ctx = Context::root()
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  
  let ctx1 = Context::with_value(ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  
  // Test retrieval with different keys
  assert_eq(Context::get(ctx2, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key2), Some("value2"))
  
  // Test non-existent key
  let non_existent_key = ContextKey::new("nonexistent")
  assert_eq(Context::get(ctx2, non_existent_key), None)
}

test "resource creation and basic operations" {
  let resource = Resource::new()
  
  // Test getting attribute from empty resource
  let non_existent = Resource::get_attribute(resource, "non.existent")
  assert_true(non_existent == None)
}

test "text map carrier operations" {
  let carrier = TextMapCarrier::new()
  
  // Test setting values
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "key1=value1,key2=value2")
  TextMapCarrier::set(carrier, "custom-header", "custom-value")
  
  // Test getting values
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Test non-existent key
  let non_existent = TextMapCarrier::get(carrier, "non-existent")
  assert_eq(non_existent, None)
}

test "baggage operations" {
  let baggage = Baggage::new()
  
  // Test setting entries
  let baggage1 = Baggage::set_entry(baggage, "key1", "value1")
  let baggage2 = Baggage::set_entry(baggage1, "key2", "value2")
  
  // Test non-existent entry
  let non_existent = Baggage::get_entry(baggage2, "nonexistent")
  assert_eq(non_existent, None)
  
  // Test removing entries
  let baggage3 = Baggage::remove_entry(baggage2, "key1")
  let removed_value = Baggage::get_entry(baggage3, "key1")
  assert_eq(removed_value, None)
}

test "counter instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let counter = Meter::create_counter(meter, "test.counter")
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, None)
  assert_eq(counter.unit, None)
  
  // Test counter add operation
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5)
  Counter::add(counter, -3.0)  // Should handle negative values
}

test "histogram instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let histogram = Meter::create_histogram(meter, "test.histogram")
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, None)
  assert_eq(histogram.unit, None)
  
  // Test histogram record operation
  Histogram::record(histogram, 1.0)
  Histogram::record(histogram, 2.5)
  Histogram::record(histogram, 10.0)
  Histogram::record(histogram, 0.1)
}

test "instrument type properties" {
  // Test instrument creation through Meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  
  // Test instrument names
  assert_eq(Instrument::name(counter), "test.counter")
  assert_eq(Instrument::name(histogram), "test.histogram")
  
  // Test instrument descriptions (should be None in simplified implementation)
  assert_eq(Instrument::description(counter), None)
  assert_eq(Instrument::description(histogram), None)
  
  // Test instrument units (should be None in simplified implementation)
  assert_eq(Instrument::unit(counter), None)
  assert_eq(Instrument::unit(histogram), None)
}

test "clock operations" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // Test that timestamp is reasonable (2025 timestamp)
  assert_eq(timestamp, 1735689600000000000L)
  
  // Test multiple calls return same timestamp (simplified implementation)
  let timestamp2 = Clock::now_unix_nanos(clock)
  assert_eq(timestamp, timestamp2)
}

test "random operations" {
  let random = Random::system()
  
  // Test next_bytes with different lengths
  let empty_bytes = Random::next_bytes(random, 0)
  
  let small_bytes = Random::next_bytes(random, 5)
  
  let large_bytes = Random::next_bytes(random, 100)
  
  // Test next_u64
  let random_value = Random::next_u64(random)
  assert_eq(random_value, 12345UL)  // Simplified implementation
  
  // Test multiple calls
  let random_value2 = Random::next_u64(random)
  assert_eq(random_value2, 12345UL)  // Simplified implementation
}

test "tracer operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test.tracer")
  assert_eq(scope.version, None)
  assert_eq(scope.schema_url, None)
  
  // Test starting span
  let span = Tracer::start_span(tracer, "test.span")
  assert_eq(Span::name(span), "test.span")
}

test "logger provider and logger creation" {
  let provider = LoggerProvider::noop()
  let logger1 = LoggerProvider::get_logger(provider, "test.logger")
  
  assert_eq(logger1.scope.name, "test.logger")
  assert_eq(logger1.scope.version, None)
  assert_eq(logger1.scope.schema_url, None)
}

test "composite propagator creation" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CTraceContextPropagator::new()  // Using same type for simplicity
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Test that composite was created by testing inject/extract operations
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test inject operation
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify that traceparent was set
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent != None)
}

test "composite propagator inject and extract" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test inject operation
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify that traceparent was set (simplified implementation)
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // Test extract operation
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify that context was modified (simplified implementation)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

test "span creation and basic properties" {
  let span_name = "test.span"
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  let span = Span::new(span_name, Internal, span_ctx)
  
  assert_eq(Span::name(span), span_name)
  assert_eq(Span::is_recording(span), true)
  assert_true(Span::span_context(span) != None)
}

test "span lifecycle" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("lifecycle.span", Internal, span_ctx)
  
  // Test initial state
  assert_eq(Span::is_recording(span), true)
  
  // Test ending span
  Span::end(span)
  
  // Test operations after ending
  Span::add_event(span, "after.end")
  Span::set_status(span, Unset)
}

test "span event operations" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test.span", Internal, span_ctx)
  
  // Test adding event without attributes
  Span::add_event(span, "event1")
  
  // Test adding event with attributes
  let attributes = [("key1", "value1"), ("key2", "value2")]
  Span::add_event(span, "event2", Some(attributes))
  
  // Test that span is still recording after events
  assert_eq(Span::is_recording(span), true)
  
  // End span
  Span::end(span)
}

test "meter provider operations" {
  let provider = MeterProvider::noop()
  let meter1 = MeterProvider::get_meter(provider, "meter1")
  let meter2 = MeterProvider::get_meter(provider, "meter2")
  
  assert_eq(meter1.scope.name, "meter1")
  assert_eq(meter1.scope.version, None)
  assert_eq(meter1.scope.schema_url, None)
  
  assert_eq(meter2.scope.name, "meter2")
  assert_eq(meter2.scope.version, None)
  assert_eq(meter2.scope.schema_url, None)
}

test "logger emit operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "emit.logger")
  
  // Test simple log record emission
  let simple_record = LogRecord::new(Info, "Simple log message")
  Logger::emit(logger, simple_record)
  
  // Test complex log record emission
  let complex_record = LogRecord::new_with_context(
    Error,
    Some("Complex log message"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  Logger::emit(logger, complex_record)
}

test "http request creation and properties" {
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers)
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
}

test "http response creation and properties" {
  let headers = [("Content-Type", "application/json"), ("Content-Length", "123")]
  let response = HttpResponse::new(200, headers)
  
  assert_eq(HttpResponse::status_code(response), 200)
}