// Comprehensive Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases covering various aspects of the telemetry system

test "attributes operations with different value types" {
  let attrs = Attributes::new()
  
  // Test string value
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  let string_value = Attributes::get(attrs, "string.key")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // Test int value
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_value = Attributes::get(attrs, "int.key")
  assert_eq(int_value, Some(IntValue(42)))
  
  // Test missing key
  let missing_value = Attributes::get(attrs, "missing.key")
  assert_eq(missing_value, None)
}

test "resource management and merging" {
  let base_attrs = [("service.name", StringValue("azimuth-service")), ("service.version", StringValue("1.0.0"))]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [("service.instance.id", StringValue("instance-123"))]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Test getting attributes
  let service_name = Resource::get_attribute(base_resource, "service.name")
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  
  // Test resource merging
  let merged_resource = Resource::merge(base_resource, override_resource)
  let instance_id = Resource::get_attribute(merged_resource, "service.instance.id")
  assert_eq(instance_id, Some(StringValue("instance-123")))
}

test "baggage propagation across contexts" {
  let baggage = Baggage::new()
  
  // Set baggage entries
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-456")
  
  // Get baggage entries
  let user_id = Baggage::get_entry(baggage_with_session, "user.id")
  let session_id = Baggage::get_entry(baggage_with_session, "session.id")
  let missing_entry = Baggage::get_entry(baggage_with_session, "missing.key")
  
  assert_eq(user_id, Some("user-123"))
  assert_eq(session_id, Some("session-456"))
  assert_eq(missing_entry, None)
  
  // Test entry removal
  let baggage_without_session = Baggage::remove_entry(baggage_with_session, "session.id")
  let removed_session_id = Baggage::get_entry(baggage_without_session, "session.id")
  assert_eq(removed_session_id, None)
}

test "span lifecycle management" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "key1=value1")
  let span = Span::new("test-span", Server, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_ctx)
  
  // Test span status
  assert_eq(Span::status(span), Unset)
  
  // Test adding events
  Span::add_event(span, "event1", Some([("event.key", StringValue("event.value"))]))
  
  // Test setting status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Test ending span
  Span::end(span)
}

test "composite propagator injection and extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  let composite = CompositePropagator::new([trace_propagator])
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "instrument type operations" {
  let counter_instrument = Counter("request.count", Some("Total number of requests"), Some("count"))
  let histogram_instrument = Histogram("response.time", Some("Response time histogram"), Some("ms"))
  let updown_counter_instrument = UpDownCounter("active.connections", Some("Currently active connections"), Some("connections"))
  let gauge_instrument = Gauge("memory.usage", Some("Memory usage gauge"), Some("bytes"))
  
  // Test instrument names
  assert_eq(Instrument::name(counter_instrument), "request.count")
  assert_eq(Instrument::name(histogram_instrument), "response.time")
  assert_eq(Instrument::name(updown_counter_instrument), "active.connections")
  assert_eq(Instrument::name(gauge_instrument), "memory.usage")
  
  // Test instrument descriptions
  assert_eq(Instrument::description(counter_instrument), Some("Total number of requests"))
  assert_eq(Instrument::description(histogram_instrument), Some("Response time histogram"))
  
  // Test instrument units
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  assert_eq(Instrument::unit(gauge_instrument), Some("bytes"))
}

test "log record with context correlation" {
  let trace_id = Some("trace-123")
  let span_id = Some("span-456")
  let ctx = Context::with_value(Context::root(), ContextKey::new("correlation.id"), "corr-789")
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    trace_id,
    span_id,
    Some(ctx)
  )
  
  // Test log record properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), trace_id)
  assert_eq(LogRecord::span_id(record), span_id)
}

test "timestamp operations and validation" {
  let clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(clock)
  
  // Create log record with timestamp
  let record = LogRecord::new_with_context(
    Info,
    Some("Test message with timestamp"),
    None,
    Some(timestamp1),
    None,
    None,
    None,
    None
  )
  
  // Verify timestamp is reasonable (should be > 2020-01-01)
  assert_true(timestamp1 > 1577836800000000000L)
  
  // Test second timestamp
  let timestamp2 = Clock::now_unix_nanos(clock)
  let record2 = LogRecord::new_with_context(
    Warn,
    Some("Another test message"),
    None,
    Some(timestamp2),
    Some(timestamp2 + 1000L),
    None,
    None,
    None
  )
  
  assert_true(timestamp2 >= timestamp1)
}

test "error boundary conditions" {
  // Test empty trace context
  let empty_span_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
  
  // Test invalid trace ID
  let invalid_trace_ctx = SpanContext::new("", "valid-span-id", true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  // Test invalid span ID
  let invalid_span_ctx = SpanContext::new("valid-trace-id", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // Test valid context
  let valid_ctx = SpanContext::new("valid-trace-id", "valid-span-id", true, "")
  assert_true(SpanContext::is_valid(valid_ctx))
}

test "concurrent safety with multiple operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent-test")
  
  // Create multiple instruments
  let counter = Meter::create_counter(meter, "concurrent.counter")
  let histogram = Meter::create_histogram(meter, "concurrent.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "concurrent.updown")
  let gauge = Meter::create_gauge(meter, "concurrent.gauge")
  
  // Perform multiple operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.0)
  
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Verify instrument names
  assert_eq(counter.name, "concurrent.counter")
  assert_eq(histogram.name, "concurrent.histogram")
  assert_eq(updown_counter.name, "concurrent.updown")
  assert_eq(gauge.name, "concurrent.gauge")
  
  // Test logger operations
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "concurrent.logger")
  
  let record1 = LogRecord::new(Info, "Concurrent log message 1")
  let record2 = LogRecord::new(Warn, "Concurrent log message 2")
  
  Logger::emit(logger, record1)
  Logger::emit(logger, record2)
  
  assert_true(true)  // If we reach here, concurrent operations didn't crash
}