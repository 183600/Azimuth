// Azimuth Telemetry System - New Fundamental Tests
// This file contains new test cases for fundamental telemetry functionality

// Test 1: Trace Context Propagation
test "trace context propagation across spans" {
  // Create root span with trace context
  let trace_id = "abc123def45678901234567890123456"
  let root_span_id = "1234567890abcdef"
  let root_ctx = SpanContext::new(trace_id, root_span_id, true, "root_state")
  let root_span = Span::new("root_operation", Server, root_ctx)
  
  // Create child span with parent context
  let child_span_id = "fedcba0987654321"
  let child_ctx = SpanContext::new(trace_id, child_span_id, true, "child_state")
  let child_span = Span::new("child_operation", Client, child_ctx)
  
  // Verify trace ID propagation
  assert_eq(SpanContext::trace_id(Span::span_context(root_span)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(child_span)), trace_id)
  
  // Verify different span IDs
  assert_eq(SpanContext::span_id(Span::span_context(root_span)), root_span_id)
  assert_eq(SpanContext::span_id(Span::span_context(child_span)), child_span_id)
  
  // Test span relationships
  assert_true(SpanContext::is_valid(Span::span_context(root_span)))
  assert_true(SpanContext::is_valid(Span::span_context(child_span)))
  
  // End spans
  Span::end(root_span)
  Span::end(child_span)
}

// Test 2: Metric Instrument Type Conversion
test "metric instrument type conversion" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "conversion_test_meter")
  
  // Create different types of instruments
  let counter = Meter::create_counter(meter, "conversion_counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "conversion_histogram", Some("Test histogram"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "conversion_gauge", Some("Test gauge"), Some("value"))
  
  // Record some values
  Counter::add(counter, 10.0)
  Histogram::record(histogram, 50.0)
  Gauge::set(gauge, 25.5)
  
  // Convert to generic instrument and verify properties
  let counter_instrument = Counter::as_instrument(counter)
  let histogram_instrument = Histogram::as_instrument(histogram)
  let gauge_instrument = Gauge::as_instrument(gauge)
  
  // Verify instrument names
  assert_eq(Instrument::name(counter_instrument), "conversion_counter")
  assert_eq(Instrument::name(histogram_instrument), "conversion_histogram")
  assert_eq(Instrument::name(gauge_instrument), "conversion_gauge")
  
  // Verify instrument descriptions
  match Instrument::description(counter_instrument) {
    Some(desc) => assert_eq(desc, "Test counter")
    None => assert_true(false)
  }
  
  match Instrument::description(histogram_instrument) {
    Some(desc) => assert_eq(desc, "Test histogram")
    None => assert_true(false)
  }
  
  match Instrument::description(gauge_instrument) {
    Some(desc) => assert_eq(desc, "Test gauge")
    None => assert_true(false)
  }
  
  // Verify instrument units
  match Instrument::unit(counter_instrument) {
    Some(unit) => assert_eq(unit, "count")
    None => assert_true(false)
  }
  
  match Instrument::unit(histogram_instrument) {
    Some(unit) => assert_eq(unit, "ms")
    None => assert_true(false)
  }
  
  match Instrument::unit(gauge_instrument) {
    Some(unit) => assert_eq(unit, "value")
    None => assert_true(false)
  }
}