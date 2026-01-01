// Concurrent Safety Tests for Azimuth Telemetry System
// This file contains test cases for concurrent safety operations

test "concurrent span operations" {
  let trace_id = "concurrent-trace-12345"
  let span_ctx = SpanContext::new(trace_id, "base-span", true, "")
  
  // Simulate concurrent span creation
  let span1 = Span::new("concurrent-span-1", Internal, span_ctx)
  let span2 = Span::new("concurrent-span-2", Server, span_ctx)
  let span3 = Span::new("concurrent-span-3", Client, span_ctx)
  
  // Verify all spans have correct context
  assert_eq(SpanContext::trace_id(Span::span_context(span1)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(span2)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(span3)), trace_id)
  
  // Simulate concurrent status updates
  Span::set_status(span1, Ok)
  Span::set_status(span2, Error, Some("Simulated error"))
  Span::set_status(span3, Ok, Some("Operation completed"))
  
  // Verify status updates
  assert_eq(Span::status(span1), Ok)
  assert_eq(Span::status(span2), Error)
  assert_eq(Span::status(span3), Ok)
}

test "concurrent metric operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent-meter")
  
  // Create multiple metrics
  let counter1 = Meter::create_counter(meter, "counter.1")
  let counter2 = Meter::create_counter(meter, "counter.2")
  let histogram1 = Meter::create_histogram(meter, "histogram.1")
  let updown_counter1 = Meter::create_updown_counter(meter, "updown.1")
  
  // Simulate concurrent metric updates
  Counter::add(counter1, 10.0)
  Counter::add(counter2, 20.0)
  Histogram::record(histogram1, 100.0)
  UpDownCounter::add(updown_counter1, 5.0)
  
  // Additional concurrent updates
  Counter::add(counter1, 15.0)
  Counter::add(counter2, 25.0)
  Histogram::record(histogram1, 150.0)
  UpDownCounter::add(updown_counter1, -2.0)
  
  // Verify operations completed without error
  assert_eq(counter1.name, "counter.1")
  assert_eq(counter2.name, "counter.2")
  assert_eq(histogram1.name, "histogram.1")
  assert_eq(updown_counter1.name, "updown.1")
}

test "concurrent context operations" {
  let root_ctx = Context::root()
  
  // Simulate concurrent context creation
  let ctx1 = Context::with_value(root_ctx, ContextKey::new("key1"), "value1")
  let ctx2 = Context::with_value(root_ctx, ContextKey::new("key2"), "value2")
  let ctx3 = Context::with_value(root_ctx, ContextKey::new("key3"), "value3")
  
  // Verify contexts are independent
  let value1 = Context::get(ctx1, ContextKey::new("key1"))
  let value2 = Context::get(ctx2, ContextKey::new("key2"))
  let value3 = Context::get(ctx3, ContextKey::new("key3"))
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
  assert_eq(value3, Some("value3"))
  
  // Verify cross-context access returns None
  let cross_access1 = Context::get(ctx1, ContextKey::new("key2"))
  let cross_access2 = Context::get(ctx2, ContextKey::new("key3"))
  let cross_access3 = Context::get(ctx3, ContextKey::new("key1"))
  
  assert_eq(cross_access1, None)
  assert_eq(cross_access2, None)
  assert_eq(cross_access3, None)
}

test "concurrent baggage operations" {
  let base_baggage = Baggage::new()
  
  // Simulate concurrent baggage modifications
  let baggage1 = Baggage::set_entry(base_baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(base_baggage, "session.id", "session-67890")
  let baggage3 = Baggage::set_entry(base_baggage, "request.id", "req-abcdef")
  
  // Verify baggage instances are independent
  let user_id1 = Baggage::get_entry(baggage1, "user.id")
  let session_id2 = Baggage::get_entry(baggage2, "session.id")
  let request_id3 = Baggage::get_entry(baggage3, "request.id")
  
  assert_eq(user_id1, Some("12345"))
  assert_eq(session_id2, Some("session-67890"))
  assert_eq(request_id3, Some("req-abcdef"))
  
  // Verify cross-baggage access returns None
  let cross_access1 = Baggage::get_entry(baggage1, "session.id")
  let cross_access2 = Baggage::get_entry(baggage2, "request.id")
  let cross_access3 = Baggage::get_entry(baggage3, "user.id")
  
  assert_eq(cross_access1, None)
  assert_eq(cross_access2, None)
  assert_eq(cross_access3, None)
}

test "concurrent logger operations" {
  let provider = LoggerProvider::default()
  
  // Create multiple loggers
  let logger1 = LoggerProvider::get_logger(provider, "concurrent-logger-1")
  let logger2 = LoggerProvider::get_logger(provider, "concurrent-logger-2")
  let logger3 = LoggerProvider::get_logger(provider, "concurrent-logger-3")
  
  // Create log records concurrently
  let record1 = LogRecord::new(Info, "Concurrent log message 1")
  let record2 = LogRecord::new(Warn, "Concurrent log message 2")
  let record3 = LogRecord::new(Error, "Concurrent log message 3")
  
  // Emit log records concurrently
  Logger::emit(logger1, record1)
  Logger::emit(logger2, record2)
  Logger::emit(logger3, record3)
  
  // Verify logger names are correct
  assert_eq(logger1.scope.name, "concurrent-logger-1")
  assert_eq(logger2.scope.name, "concurrent-logger-2")
  assert_eq(logger3.scope.name, "concurrent-logger-3")
}

test "concurrent resource operations" {
  // Create multiple resources with different attributes
  let resource1 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("concurrent-service-1")),
    ("service.instance.id", StringValue("instance-1"))
  ])
  
  let resource2 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("concurrent-service-2")),
    ("service.instance.id", StringValue("instance-2"))
  ])
  
  let resource3 = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("concurrent-service-3")),
    ("service.instance.id", StringValue("instance-3"))
  ])
  
  // Verify resource independence
  let service_name1 = Resource::get_attribute(resource1, "service.name")
  let service_name2 = Resource::get_attribute(resource2, "service.name")
  let service_name3 = Resource::get_attribute(resource3, "service.name")
  
  assert_eq(service_name1, Some(StringValue("concurrent-service-1")))
  assert_eq(service_name2, Some(StringValue("concurrent-service-2")))
  assert_eq(service_name3, Some(StringValue("concurrent-service-3")))
  
  // Merge resources concurrently
  let merged1_2 = Resource::merge(resource1, resource2)
  let merged1_3 = Resource::merge(resource1, resource3)
  let merged2_3 = Resource::merge(resource2, resource3)
  
  // Verify merge results
  let merged1_2_name = Resource::get_attribute(merged1_2, "service.name")
  let merged1_3_name = Resource::get_attribute(merged1_3, "service.name")
  let merged2_3_name = Resource::get_attribute(merged2_3, "service.name")
  
  assert_eq(merged1_2_name, Some(StringValue("concurrent-service-2")))
  assert_eq(merged1_3_name, Some(StringValue("concurrent-service-3")))
  assert_eq(merged2_3_name, Some(StringValue("concurrent-service-3")))
}

test "concurrent propagator operations" {
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let baggage_propagator1 = W3CBaggagePropagator::new()
  let baggage_propagator2 = W3CBaggagePropagator::new()
  
  // Create composite propagators
  let composite1 = CompositePropagator::new([trace_propagator1, baggage_propagator1])
  let composite2 = CompositePropagator::new([trace_propagator2, baggage_propagator2])
  
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  
  let ctx1 = Context::root()
  let ctx2 = Context::root()
  
  // Concurrent injection operations
  CompositePropagator::inject(composite1, ctx1, carrier1)
  CompositePropagator::inject(composite2, ctx2, carrier2)
  
  // Concurrent extraction operations
  let extracted1 = CompositePropagator::extract(composite1, carrier1)
  let extracted2 = CompositePropagator::extract(composite2, carrier2)
  
  // Verify extractions completed
  let extracted_value1 = Context::get(extracted1, ContextKey::new("extracted"))
  let extracted_value2 = Context::get(extracted2, ContextKey::new("extracted"))
  
  assert_eq(extracted_value1, Some("true"))
  assert_eq(extracted_value2, Some("true"))
}

test "concurrent attribute operations" {
  // Create multiple attribute sets
  let attrs1 = Attributes::new()
  let attrs2 = Attributes::new()
  let attrs3 = Attributes::new()
  
  // Set attributes concurrently
  Attributes::set(attrs1, "concurrent.key1", StringValue("value1"))
  Attributes::set(attrs2, "concurrent.key2", IntValue(42))
  Attributes::set(attrs3, "concurrent.key3", BoolValue(true))
  
  // Verify attribute independence
  let value1 = Attributes::get(attrs1, "concurrent.key1")
  let value2 = Attributes::get(attrs2, "concurrent.key2")
  let value3 = Attributes::get(attrs3, "concurrent.key3")
  
  assert_eq(value1, Some(StringValue("value1")))
  assert_eq(value2, Some(IntValue(42)))
  assert_eq(value3, Some(BoolValue(true)))
  
  // Verify cross-attribute access returns None
  let cross_access1 = Attributes::get(attrs1, "concurrent.key2")
  let cross_access2 = Attributes::get(attrs2, "concurrent.key3")
  let cross_access3 = Attributes::get(attrs3, "concurrent.key1")
  
  assert_eq(cross_access1, None)
  assert_eq(cross_access2, None)
  assert_eq(cross_access3, None)
}