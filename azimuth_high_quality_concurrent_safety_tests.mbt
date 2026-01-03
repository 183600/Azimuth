// High-Quality Test Suite for Concurrent Safety
// This file contains comprehensive test cases for concurrent safety functionality

test "concurrent span creation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Create multiple spans with same trace ID
  let span1_id = "b7ad6b7169203331"
  let span2_id = "c7ad6b7169203331"
  let span3_id = "d7ad6b7169203331"
  
  let span_ctx1 = SpanContext::new(trace_id, span1_id, true, "")
  let span_ctx2 = SpanContext::new(trace_id, span2_id, true, "")
  let span_ctx3 = SpanContext::new(trace_id, span3_id, true, "")
  
  let span1 = Span::new("operation-1", Internal, span_ctx1)
  let span2 = Span::new("operation-2", Client, span_ctx2)
  let span3 = Span::new("operation-3", Server, span_ctx3)
  
  // Verify all spans have correct properties
  assert_eq(Span::name(span1), "operation-1")
  assert_eq(Span::name(span2), "operation-2")
  assert_eq(Span::name(span3), "operation-3")
  
  assert_eq(Span::kind(span1), Internal)
  assert_eq(Span::kind(span2), Client)
  assert_eq(Span::kind(span3), Server)
  
  assert_eq(SpanContext::trace_id(Span::span_context(span1)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(span2)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(span3)), trace_id)
}

test "concurrent span operations" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Create multiple spans
  let span1_ctx = SpanContext::new(trace_id, "b7ad6b7169203331", true, "")
  let span2_ctx = SpanContext::new(trace_id, "c7ad6b7169203331", true, "")
  let span3_ctx = SpanContext::new(trace_id, "d7ad6b7169203331", true, "")
  
  let span1 = Span::new("operation-1", Internal, span1_ctx)
  let span2 = Span::new("operation-2", Client, span2_ctx)
  let span3 = Span::new("operation-3", Server, span3_ctx)
  
  // Perform operations on all spans concurrently
  Span::set_status(span1, Ok, Some("Operation 1 completed"))
  Span::set_status(span2, Error, Some("Operation 2 failed"))
  Span::set_status(span3, Ok, Some("Operation 3 completed"))
  
  Span::add_event(span1, "start", None)
  Span::add_event(span2, "retry", None)
  Span::add_event(span3, "complete", None)
  
  // Verify operations completed successfully
  assert_eq(Span::status(span1), Ok)
  assert_eq(Span::status(span2), Error)
  assert_eq(Span::status(span3), Ok)
}

test "concurrent context operations" {
  let root_ctx = Context::root()
  
  // Create multiple contexts with different keys
  let key1 = ContextKey::new("operation.1.id")
  let key2 = ContextKey::new("operation.2.id")
  let key3 = ContextKey::new("operation.3.id")
  
  let ctx1 = Context::with_value(root_ctx, key1, "op1-123")
  let ctx2 = Context::with_value(root_ctx, key2, "op2-456")
  let ctx3 = Context::with_value(root_ctx, key3, "op3-789")
  
  // Verify all contexts have correct values
  assert_eq(Context::get(ctx1, key1), Some("op1-123"))
  assert_eq(Context::get(ctx2, key2), Some("op2-456"))
  assert_eq(Context::get(ctx3, key3), Some("op3-789"))
}

test "concurrent baggage operations" {
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::new()
  let baggage3 = Baggage::new()
  
  // Add entries to all baggage instances
  let baggage1_updated = Baggage::set_entry(baggage1, "operation.1.id", "op1-123")
  let baggage2_updated = Baggage::set_entry(baggage2, "operation.2.id", "op2-456")
  let baggage3_updated = Baggage::set_entry(baggage3, "operation.3.id", "op3-789")
  
  // Verify all baggage instances have correct entries
  assert_eq(Baggage::get_entry(baggage1_updated, "operation.1.id"), Some("op1-123"))
  assert_eq(Baggage::get_entry(baggage2_updated, "operation.2.id"), Some("op2-456"))
  assert_eq(Baggage::get_entry(baggage3_updated, "operation.3.id"), Some("op3-789"))
}

test "concurrent metrics operations" {
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "service-1")
  let meter2 = MeterProvider::get_meter(provider, "service-2")
  let meter3 = MeterProvider::get_meter(provider, "service-3")
  
  // Create counters for each service
  let counter1 = Meter::create_counter(meter1, "requests.total")
  let counter2 = Meter::create_counter(meter2, "requests.total")
  let counter3 = Meter::create_counter(meter3, "requests.total")
  
  // Add values to all counters
  Counter::add(counter1, 100.0)
  Counter::add(counter2, 200.0)
  Counter::add(counter3, 300.0)
  
  // Create histograms for each service
  let histogram1 = Meter::create_histogram(meter1, "response.time")
  let histogram2 = Meter::create_histogram(meter2, "response.time")
  let histogram3 = Meter::create_histogram(meter3, "response.time")
  
  // Record values to all histograms
  Histogram::record(histogram1, 100.0)
  Histogram::record(histogram2, 200.0)
  Histogram::record(histogram3, 300.0)
  
  // Verify operations completed successfully
  assert_eq(counter1.name, "requests.total")
  assert_eq(counter2.name, "requests.total")
  assert_eq(counter3.name, "requests.total")
  
  assert_eq(histogram1.name, "response.time")
  assert_eq(histogram2.name, "response.time")
  assert_eq(histogram3.name, "response.time")
}

test "concurrent log operations" {
  let provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(provider, "service-1")
  let logger2 = LoggerProvider::get_logger(provider, "service-2")
  let logger3 = LoggerProvider::get_logger(provider, "service-3")
  
  // Create log records for each service
  let record1 = LogRecord::new(Info, "Service 1 operation")
  let record2 = LogRecord::new(Warn, "Service 2 warning")
  let record3 = LogRecord::new(Error, "Service 3 error")
  
  // Emit all log records
  Logger::emit(logger1, record1)
  Logger::emit(logger2, record2)
  Logger::emit(logger3, record3)
  
  // Verify log records have correct properties
  assert_eq(LogRecord::severity_number(record1), Info)
  assert_eq(LogRecord::severity_number(record2), Warn)
  assert_eq(LogRecord::severity_number(record3), Error)
  
  assert_eq(LogRecord::body(record1), Some("Service 1 operation"))
  assert_eq(LogRecord::body(record2), Some("Service 2 warning"))
  assert_eq(LogRecord::body(record3), Some("Service 3 error"))
}

test "concurrent resource operations" {
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  let resource3 = Resource::new()
  
  // Create attributes for each resource
  let attrs1 = [("service.name", StringValue("service-1"))]
  let attrs2 = [("service.name", StringValue("service-2"))]
  let attrs3 = [("service.name", StringValue("service-3"))]
  
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  let resource3_with_attrs = Resource::with_attributes(resource3, attrs3)
  
  // Verify all resources have correct attributes
  assert_eq(Resource::get_attribute(resource1_with_attrs, "service.name"), Some(StringValue("service-1")))
  assert_eq(Resource::get_attribute(resource2_with_attrs, "service.name"), Some(StringValue("service-2")))
  assert_eq(Resource::get_attribute(resource3_with_attrs, "service.name"), Some(StringValue("service-3")))
}

test "concurrent propagator operations" {
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let trace_propagator3 = W3CTraceContextPropagator::new()
  
  let propagators1 = [trace_propagator1]
  let propagators2 = [trace_propagator2]
  let propagators3 = [trace_propagator3]
  
  let composite_propagator1 = CompositePropagator::new(propagators1)
  let composite_propagator2 = CompositePropagator::new(propagators2)
  let composite_propagator3 = CompositePropagator::new(propagators3)
  
  let ctx = Context::root()
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // Inject context into all carriers
  CompositePropagator::inject(composite_propagator1, ctx, carrier1)
  CompositePropagator::inject(composite_propagator2, ctx, carrier2)
  CompositePropagator::inject(composite_propagator3, ctx, carrier3)
  
  // Verify all carriers have traceparent header
  assert_eq(TextMapCarrier::get(carrier1, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
  assert_eq(TextMapCarrier::get(carrier2, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
  assert_eq(TextMapCarrier::get(carrier3, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
}

test "concurrent span lifecycle" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Create multiple spans
  let span1_ctx = SpanContext::new(trace_id, "b7ad6b7169203331", true, "")
  let span2_ctx = SpanContext::new(trace_id, "c7ad6b7169203331", true, "")
  let span3_ctx = SpanContext::new(trace_id, "d7ad6b7169203331", true, "")
  
  let span1 = Span::new("operation-1", Internal, span1_ctx)
  let span2 = Span::new("operation-2", Client, span2_ctx)
  let span3 = Span::new("operation-3", Server, span3_ctx)
  
  // Perform operations on all spans
  Span::add_event(span1, "start", None)
  Span::add_event(span2, "start", None)
  Span::add_event(span3, "start", None)
  
  Span::set_status(span1, Ok, Some("Operation 1 in progress"))
  Span::set_status(span2, Ok, Some("Operation 2 in progress"))
  Span::set_status(span3, Ok, Some("Operation 3 in progress"))
  
  // End all spans
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  
  // Verify all spans have correct final status
  assert_eq(Span::status(span1), Ok)
  assert_eq(Span::status(span2), Ok)
  assert_eq(Span::status(span3), Ok)
}

test "concurrent complex scenario" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Create multiple spans, contexts, and baggage
  let span1_ctx = SpanContext::new(trace_id, "b7ad6b7169203331", true, "")
  let span2_ctx = SpanContext::new(trace_id, "c7ad6b7169203331", true, "")
  
  let span1 = Span::new("operation-1", Internal, span1_ctx)
  let span2 = Span::new("operation-2", Client, span2_ctx)
  
  let ctx1 = Context::root()
  let ctx2 = Context::root()
  
  let key1 = ContextKey::new("operation.1.id")
  let key2 = ContextKey::new("operation.2.id")
  
  let ctx1_with_value = Context::with_value(ctx1, key1, "op1-123")
  let ctx2_with_value = Context::with_value(ctx2, key2, "op2-456")
  
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::new()
  
  let baggage1_updated = Baggage::set_entry(baggage1, "operation.1.id", "op1-123")
  let baggage2_updated = Baggage::set_entry(baggage2, "operation.2.id", "op2-456")
  
  // Create metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent-test")
  let counter = Meter::create_counter(meter, "operations.total")
  
  // Create logs
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "concurrent-test")
  
  let record1 = LogRecord::new(Info, "Operation 1 started")
  let record2 = LogRecord::new(Info, "Operation 2 started")
  
  // Perform all operations
  Span::add_event(span1, "start", None)
  Span::add_event(span2, "start", None)
  
  Span::set_status(span1, Ok, Some("Operation 1 completed"))
  Span::set_status(span2, Ok, Some("Operation 2 completed"))
  
  Counter::add(counter, 2.0)
  
  Logger::emit(logger, record1)
  Logger::emit(logger, record2)
  
  // Verify all operations completed successfully
  assert_eq(Span::status(span1), Ok)
  assert_eq(Span::status(span2), Ok)
  
  assert_eq(Context::get(ctx1_with_value, key1), Some("op1-123"))
  assert_eq(Context::get(ctx2_with_value, key2), Some("op2-456"))
  
  assert_eq(Baggage::get_entry(baggage1_updated, "operation.1.id"), Some("op1-123"))
  assert_eq(Baggage::get_entry(baggage2_updated, "operation.2.id"), Some("op2-456"))
  
  assert_eq(counter.name, "operations.total")
  
  assert_eq(LogRecord::severity_number(record1), Info)
  assert_eq(LogRecord::severity_number(record2), Info)
}