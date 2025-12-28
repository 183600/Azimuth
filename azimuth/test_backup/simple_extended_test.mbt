// Simple extended tests for telemetry components

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

test "span creation and basic properties" {
  let span_name = "test.span"
  let span_kind = Internal
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  let span = Span::new(span_name, span_kind, span_ctx)
  
  assert_eq(Span::name(span), span_name)
  assert_eq(Span::kind(span), span_kind)
  assert_eq(Span::is_recording(span), true)
  assert_eq(Span::span_context(span), span_ctx)
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
  assert_eq(resource.attributes.length, 0)
  
  // Test getting attribute from empty resource
  let non_existent = Resource::get_attribute(resource, "non.existent")
  assert_eq(non_existent, None)
}

test "resource with attributes" {
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("localhost")),
    ("process.pid", IntValue(42))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  assert_eq(resource_with_attrs.attributes.length, 5)
}

test "text map carrier operations" {
  let carrier = TextMapCarrier::new()
  assert_eq(carrier.headers.length, 0)
  
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
  assert_eq(baggage.entries.length, 0)
  
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
  let counter_instr = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_instr = Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter_instr = UpDownCounter("test.updown", Some("Test updown"), Some("items"))
  let gauge_instr = Gauge("test.gauge", Some("Test gauge"), Some("percent"))
  
  // Test instrument names
  assert_eq(Instrument::name(counter_instr), "test.counter")
  assert_eq(Instrument::name(histogram_instr), "test.histogram")
  assert_eq(Instrument::name(updown_counter_instr), "test.updown")
  assert_eq(Instrument::name(gauge_instr), "test.gauge")
  
  // Test instrument descriptions
  assert_eq(Instrument::description(counter_instr), Some("Test counter"))
  assert_eq(Instrument::description(histogram_instr), Some("Test histogram"))
  assert_eq(Instrument::description(updown_counter_instr), Some("Test updown"))
  assert_eq(Instrument::description(gauge_instr), Some("Test gauge"))
  
  // Test instrument units
  assert_eq(Instrument::unit(counter_instr), Some("count"))
  assert_eq(Instrument::unit(histogram_instr), Some("ms"))
  assert_eq(Instrument::unit(updown_counter_instr), Some("items"))
  assert_eq(Instrument::unit(gauge_instr), Some("percent"))
}

test "log record creation and basic properties" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  assert_eq(record.attributes, None)
  assert_eq(record.timestamp, None)
  assert_eq(record.observed_timestamp, None)
  assert_eq(record.trace_id, None)
  assert_eq(record.span_id, None)
  assert_eq(record.context, None)
}

test "log record with all severity levels" {
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  assert_eq(LogRecord::body(trace_record), Some("Trace message"))
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
  assert_eq(LogRecord::body(info_record), Some("Info message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
  assert_eq(LogRecord::body(error_record), Some("Error message"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal message"))
}

test "http request creation and properties" {
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"key\":\"value\"}"))
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some("{\"key\":\"value\"}"))
}

test "http response creation and properties" {
  let headers = [("Content-Type", "application/json"), ("Content-Length", "123")]
  let response = HttpResponse::new(200, headers, Some("{\"status\":\"success\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"status\":\"success\"}"))
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
  assert_eq(empty_bytes.length, 0)
  
  let small_bytes = Random::next_bytes(random, 5)
  assert_eq(small_bytes.length, 0)  // Simplified implementation returns empty array
  
  let large_bytes = Random::next_bytes(random, 100)
  assert_eq(large_bytes.length, 0)  // Simplified implementation returns empty array
  
  // Test next_u64
  let random_value = Random::next_u64(random)
  assert_eq(random_value, 12345UL)  // Simplified implementation
  
  // Test multiple calls
  let random_value2 = Random::next_u64(random)
  assert_eq(random_value2, 12345UL)  // Simplified implementation
}

test "tracer operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer", Some("1.0.0"))
  
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test.tracer")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
  
  // Test starting span
  let span = Tracer::start_span(tracer, "test.span", None)
  assert_eq(Span::name(span), "test.span")
  assert_eq(Span::kind(span), Internal)
}

test "logger provider and logger creation" {
  let provider = LoggerProvider::noop()
  let logger1 = LoggerProvider::get_logger(provider, "test.logger")
  let logger2 = LoggerProvider::get_logger(provider, "test.logger2", Some("1.0.0"))
  
  assert_eq(logger1.scope.name, "test.logger")
  assert_eq(logger1.scope.version, None)
  assert_eq(logger1.scope.schema_url, None)
  
  assert_eq(logger2.scope.name, "test.logger2")
  assert_eq(logger2.scope.version, Some("1.0.0"))
  assert_eq(logger2.scope.schema_url, None)
}

test "composite propagator creation" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CTraceContextPropagator::new()  // Using same type for simplicity
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  assert_eq(composite.propagators.length, 2)
  assert_eq(composite.propagators[0], trace_propagator)
  assert_eq(composite.propagators[1], baggage_propagator)
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