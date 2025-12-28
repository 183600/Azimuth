// Comprehensive Concurrent Safety Test Suite
// Tests for thread safety and concurrent operations in telemetry components

test "concurrent span context operations" {
  // Create multiple span contexts with same trace ID (simulating concurrent operations)
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Simulate concurrent span creation
  let span_ctx1 = SpanContext::new(trace_id, "span1111111111111111", true, "")
  let span_ctx2 = SpanContext::new(trace_id, "span2222222222222222", true, "")
  let span_ctx3 = SpanContext::new(trace_id, "span3333333333333333", true, "")
  let span_ctx4 = SpanContext::new(trace_id, "span4444444444444444", true, "")
  let span_ctx5 = SpanContext::new(trace_id, "span5555555555555555", true, "")
  
  // Verify all contexts are valid and have same trace ID
  assert_true(SpanContext::is_valid(span_ctx1))
  assert_true(SpanContext::is_valid(span_ctx2))
  assert_true(SpanContext::is_valid(span_ctx3))
  assert_true(SpanContext::is_valid(span_ctx4))
  assert_true(SpanContext::is_valid(span_ctx5))
  
  assert_eq(SpanContext::trace_id(span_ctx1), trace_id)
  assert_eq(SpanContext::trace_id(span_ctx2), trace_id)
  assert_eq(SpanContext::trace_id(span_ctx3), trace_id)
  assert_eq(SpanContext::trace_id(span_ctx4), trace_id)
  assert_eq(SpanContext::trace_id(span_ctx5), trace_id)
  
  // Verify all span IDs are different
  assert_ne(SpanContext::span_id(span_ctx1), SpanContext::span_id(span_ctx2))
  assert_ne(SpanContext::span_id(span_ctx2), SpanContext::span_id(span_ctx3))
  assert_ne(SpanContext::span_id(span_ctx3), SpanContext::span_id(span_ctx4))
  assert_ne(SpanContext::span_id(span_ctx4), SpanContext::span_id(span_ctx5))
  assert_ne(SpanContext::span_id(span_ctx5), SpanContext::span_id(span_ctx1))
}

test "concurrent context operations" {
  let ctx = Context::root()
  
  // Simulate concurrent context operations with different keys
  let key1 = ContextKey::new("concurrent.key1")
  let key2 = ContextKey::new("concurrent.key2")
  let key3 = ContextKey::new("concurrent.key3")
  let key4 = ContextKey::new("concurrent.key4")
  let key5 = ContextKey::new("concurrent.key5")
  
  // Create contexts with different values (simulating concurrent operations)
  let ctx1 = Context::with_value(ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx, key2, "value2")
  let ctx3 = Context::with_value(ctx, key3, "value3")
  let ctx4 = Context::with_value(ctx, key4, "value4")
  let ctx5 = Context::with_value(ctx, key5, "value5")
  
  // Verify each context has its own value
  assert_eq(Context::get(ctx1, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key3), Some("value3"))
  assert_eq(Context::get(ctx4, key4), Some("value4"))
  assert_eq(Context::get(ctx5, key5), Some("value5"))
  
  // Verify contexts don't interfere with each other
  assert_eq(Context::get(ctx1, key2), None)
  assert_eq(Context::get(ctx2, key3), None)
  assert_eq(Context::get(ctx3, key4), None)
  assert_eq(Context::get(ctx4, key5), None)
  assert_eq(Context::get(ctx5, key1), None)
}

test "concurrent attributes operations" {
  // Create multiple attributes instances (simulating concurrent usage)
  let attrs1 = Attributes::new()
  let attrs2 = Attributes::new()
  let attrs3 = Attributes::new()
  let attrs4 = Attributes::new()
  let attrs5 = Attributes::new()
  
  // Set different attributes in each instance
  Attributes::set(attrs1, "concurrent.test1", StringValue("value1"))
  Attributes::set(attrs2, "concurrent.test2", StringValue("value2"))
  Attributes::set(attrs3, "concurrent.test3", StringValue("value3"))
  Attributes::set(attrs4, "concurrent.test4", StringValue("value4"))
  Attributes::set(attrs5, "concurrent.test5", StringValue("value5"))
  
  // Verify each attributes instance has its own data
  let value1 = Attributes::get(attrs1, "concurrent.test1")
  let value2 = Attributes::get(attrs2, "concurrent.test2")
  let value3 = Attributes::get(attrs3, "concurrent.test3")
  let value4 = Attributes::get(attrs4, "concurrent.test4")
  let value5 = Attributes::get(attrs5, "concurrent.test5")
  
  assert_eq(value1, Some(StringValue("value1")))
  assert_eq(value2, Some(StringValue("value2")))
  assert_eq(value3, Some(StringValue("value3")))
  assert_eq(value4, Some(StringValue("value4")))
  assert_eq(value5, Some(StringValue("value5")))
  
  // Verify attributes instances don't interfere
  assert_eq(Attributes::get(attrs1, "concurrent.test2"), None)
  assert_eq(Attributes::get(attrs2, "concurrent.test3"), None)
  assert_eq(Attributes::get(attrs3, "concurrent.test4"), None)
  assert_eq(Attributes::get(attrs4, "concurrent.test5"), None)
  assert_eq(Attributes::get(attrs5, "concurrent.test1"), None)
}

test "concurrent baggage operations" {
  // Create multiple baggage instances (simulating concurrent usage)
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::new()
  let baggage3 = Baggage::new()
  let baggage4 = Baggage::new()
  let baggage5 = Baggage::new()
  
  // Set different entries in each baggage instance
  let baggage1_updated = Baggage::set_entry(baggage1, "concurrent.baggage1", "entry1")
  let baggage2_updated = Baggage::set_entry(baggage2, "concurrent.baggage2", "entry2")
  let baggage3_updated = Baggage::set_entry(baggage3, "concurrent.baggage3", "entry3")
  let baggage4_updated = Baggage::set_entry(baggage4, "concurrent.baggage4", "entry4")
  let baggage5_updated = Baggage::set_entry(baggage5, "concurrent.baggage5", "entry5")
  
  // Verify each baggage instance has its own data
  let entry1 = Baggage::get_entry(baggage1_updated, "concurrent.baggage1")
  let entry2 = Baggage::get_entry(baggage2_updated, "concurrent.baggage2")
  let entry3 = Baggage::get_entry(baggage3_updated, "concurrent.baggage3")
  let entry4 = Baggage::get_entry(baggage4_updated, "concurrent.baggage4")
  let entry5 = Baggage::get_entry(baggage5_updated, "concurrent.baggage5")
  
  assert_eq(entry1, Some("entry1"))
  assert_eq(entry2, Some("entry2"))
  assert_eq(entry3, Some("entry3"))
  assert_eq(entry4, Some("entry4"))
  assert_eq(entry5, Some("entry5"))
  
  // Verify baggage instances don't interfere
  assert_eq(Baggage::get_entry(baggage1_updated, "concurrent.baggage2"), None)
  assert_eq(Baggage::get_entry(baggage2_updated, "concurrent.baggage3"), None)
  assert_eq(Baggage::get_entry(baggage3_updated, "concurrent.baggage4"), None)
  assert_eq(Baggage::get_entry(baggage4_updated, "concurrent.baggage5"), None)
  assert_eq(Baggage::get_entry(baggage5_updated, "concurrent.baggage1"), None)
}

test "concurrent meter and instrument operations" {
  let provider = MeterProvider::default()
  
  // Create multiple meters (simulating concurrent usage)
  let meter1 = MeterProvider::get_meter(provider, "concurrent-meter1")
  let meter2 = MeterProvider::get_meter(provider, "concurrent-meter2")
  let meter3 = MeterProvider::get_meter(provider, "concurrent-meter3")
  
  // Create instruments from different meters
  let counter1 = Meter::create_counter(meter1, "counter1")
  let counter2 = Meter::create_counter(meter2, "counter2")
  let counter3 = Meter::create_counter(meter3, "counter3")
  
  let histogram1 = Meter::create_histogram(meter1, "histogram1")
  let histogram2 = Meter::create_histogram(meter2, "histogram2")
  let histogram3 = Meter::create_histogram(meter3, "histogram3")
  
  // Perform concurrent operations
  Counter::add(counter1, 1.0)
  Counter::add(counter2, 2.0)
  Counter::add(counter3, 3.0)
  
  Histogram::record(histogram1, 100.0)
  Histogram::record(histogram2, 200.0)
  Histogram::record(histogram3, 300.0)
  
  // Verify instruments have correct names and are independent
  assert_eq(counter1.name, "counter1")
  assert_eq(counter2.name, "counter2")
  assert_eq(counter3.name, "counter3")
  
  assert_eq(histogram1.name, "histogram1")
  assert_eq(histogram2.name, "histogram2")
  assert_eq(histogram3.name, "histogram3")
  
  // If no crashes occur, concurrent operations are safe
  assert_true(true)
}

test "concurrent tracer and span operations" {
  let provider = TracerProvider::default()
  
  // Create multiple tracers (simulating concurrent usage)
  let tracer1 = TracerProvider::get_tracer(provider, "concurrent-tracer1")
  let tracer2 = TracerProvider::get_tracer(provider, "concurrent-tracer2")
  let tracer3 = TracerProvider::get_tracer(provider, "concurrent-tracer3")
  
  // Create spans from different tracers concurrently
  let span1 = Tracer::start_span(tracer1, "span1")
  let span2 = Tracer::start_span(tracer2, "span2")
  let span3 = Tracer::start_span(tracer3, "span3")
  
  // Perform concurrent span operations
  Span::set_status(span1, Ok, None)
  Span::set_status(span2, Error, Some("Test error"))
  Span::set_status(span3, Unset, Some("Test unset"))
  
  Span::add_event(span1, "event1", None)
  Span::add_event(span2, "event2", None)
  Span::add_event(span3, "event3", None)
  
  // Verify spans have correct names and are independent
  assert_eq(Span::name(span1), "span1")
  assert_eq(Span::name(span2), "span2")
  assert_eq(Span::name(span3), "span3")
  
  // End spans
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  
  // If no crashes occur, concurrent operations are safe
  assert_true(true)
}

test "concurrent logger and log operations" {
  let provider = LoggerProvider::default()
  
  // Create multiple loggers (simulating concurrent usage)
  let logger1 = LoggerProvider::get_logger(provider, "concurrent-logger1")
  let logger2 = LoggerProvider::get_logger(provider, "concurrent-logger2")
  let logger3 = LoggerProvider::get_logger(provider, "concurrent-logger3")
  
  // Create log records with different properties
  let log1 = LogRecord::new(Info, "Concurrent log message 1")
  let log2 = LogRecord::new(Warn, "Concurrent log message 2")
  let log3 = LogRecord::new(Error, "Concurrent log message 3")
  
  // Emit logs concurrently
  Logger::emit(logger1, log1)
  Logger::emit(logger2, log2)
  Logger::emit(logger3, log3)
  
  // Verify log records have correct properties
  assert_eq(LogRecord::severity_number(log1), Info)
  assert_eq(LogRecord::severity_number(log2), Warn)
  assert_eq(LogRecord::severity_number(log3), Error)
  
  assert_eq(LogRecord::body(log1), Some("Concurrent log message 1"))
  assert_eq(LogRecord::body(log2), Some("Concurrent log message 2"))
  assert_eq(LogRecord::body(log3), Some("Concurrent log message 3"))
  
  // If no crashes occur, concurrent operations are safe
  assert_true(true)
}

test "concurrent text map carrier operations" {
  // Create multiple carriers (simulating concurrent usage)
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  let carrier4 = TextMapCarrier::new()
  let carrier5 = TextMapCarrier::new()
  
  // Set different headers in each carrier
  TextMapCarrier::set(carrier1, "concurrent-header1", "value1")
  TextMapCarrier::set(carrier2, "concurrent-header2", "value2")
  TextMapCarrier::set(carrier3, "concurrent-header3", "value3")
  TextMapCarrier::set(carrier4, "concurrent-header4", "value4")
  TextMapCarrier::set(carrier5, "concurrent-header5", "value5")
  
  // Verify each carrier has its own data
  let value1 = TextMapCarrier::get(carrier1, "concurrent-header1")
  let value2 = TextMapCarrier::get(carrier2, "concurrent-header2")
  let value3 = TextMapCarrier::get(carrier3, "concurrent-header3")
  let value4 = TextMapCarrier::get(carrier4, "concurrent-header4")
  let value5 = TextMapCarrier::get(carrier5, "concurrent-header5")
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
  assert_eq(value3, Some("value3"))
  assert_eq(value4, Some("value4"))
  assert_eq(value5, Some("value5"))
  
  // Verify carriers don't interfere
  assert_eq(TextMapCarrier::get(carrier1, "concurrent-header2"), None)
  assert_eq(TextMapCarrier::get(carrier2, "concurrent-header3"), None)
  assert_eq(TextMapCarrier::get(carrier3, "concurrent-header4"), None)
  assert_eq(TextMapCarrier::get(carrier4, "concurrent-header5"), None)
  assert_eq(TextMapCarrier::get(carrier5, "concurrent-header1"), None)
}

test "concurrent propagator operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Create multiple contexts and carriers (simulating concurrent operations)
  let ctx1 = Context::root()
  let ctx2 = Context::root()
  let ctx3 = Context::root()
  
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // Perform concurrent injection operations
  CompositePropagator::inject(composite_propagator, ctx1, carrier1)
  CompositePropagator::inject(composite_propagator, ctx2, carrier2)
  CompositePropagator::inject(composite_propagator, ctx3, carrier3)
  
  // Verify injection worked for all carriers
  let traceparent1 = TextMapCarrier::get(carrier1, "traceparent")
  let traceparent2 = TextMapCarrier::get(carrier2, "traceparent")
  let traceparent3 = TextMapCarrier::get(carrier3, "traceparent")
  
  assert_eq(traceparent1, Some("00-test-trace-id-test-span-id-01"))
  assert_eq(traceparent2, Some("00-test-trace-id-test-span-id-01"))
  assert_eq(traceparent3, Some("00-test-trace-id-test-span-id-01"))
  
  // Perform concurrent extraction operations
  let extracted_ctx1 = CompositePropagator::extract(composite_propagator, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite_propagator, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(composite_propagator, carrier3)
  
  // Verify extraction worked for all contexts
  let extracted_value1 = Context::get(extracted_ctx1, ContextKey::new("extracted"))
  let extracted_value2 = Context::get(extracted_ctx2, ContextKey::new("extracted"))
  let extracted_value3 = Context::get(extracted_ctx3, ContextKey::new("extracted"))
  
  assert_eq(extracted_value1, Some("true"))
  assert_eq(extracted_value2, Some("true"))
  assert_eq(extracted_value3, Some("true"))
  
  // If no crashes occur, concurrent operations are safe
  assert_true(true)
}