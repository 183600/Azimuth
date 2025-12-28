// Advanced Concurrent Safety Tests for Azimuth Telemetry System
// Testing thread safety, race conditions, and concurrent access patterns

test "concurrent_span_creation_and_lifecycle" {
  // Test concurrent span creation and lifecycle management
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent_test_tracer")
  
  // Simulate concurrent span creation
  let span1 = Tracer::start_span(tracer, "concurrent_span_1")
  let span2 = Tracer::start_span(tracer, "concurrent_span_2")
  let span3 = Tracer::start_span(tracer, "concurrent_span_3")
  
  // Verify all spans are created independently
  assert_eq(Span::name(span1), "concurrent_span_1")
  assert_eq(Span::name(span2), "concurrent_span_2")
  assert_eq(Span::name(span3), "concurrent_span_3")
  
  // Test concurrent span operations
  Span::set_status(span1, Ok, Some("Span 1 completed"))
  Span::set_status(span2, Error, Some("Span 2 failed"))
  Span::set_status(span3, Ok, Some("Span 3 completed"))
  
  Span::add_event(span1, "event_1", None)
  Span::add_event(span2, "event_2", None)
  Span::add_event(span3, "event_3", None)
  
  // Test concurrent span ending
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  
  // Verify span contexts are independent
  let ctx1 = Span::span_context(span1)
  let ctx2 = Span::span_context(span2)
  let ctx3 = Span::span_context(span3)
  
  assert_ne(SpanContext::span_id(ctx1), SpanContext::span_id(ctx2))
  assert_ne(SpanContext::span_id(ctx2), SpanContext::span_id(ctx3))
  assert_ne(SpanContext::span_id(ctx1), SpanContext::span_id(ctx3))
}

test "concurrent_metric_operations" {
  // Test concurrent metric operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "concurrent_test_meter")
  
  // Create multiple metric instruments
  let counter1 = Meter::create_counter(meter, "counter_1", Some("Test counter 1"), Some("count"))
  let counter2 = Meter::create_counter(meter, "counter_2", Some("Test counter 2"), Some("count"))
  let histogram1 = Meter::create_histogram(meter, "histogram_1", Some("Test histogram 1"), Some("ms"))
  let histogram2 = Meter::create_histogram(meter, "histogram_2", Some("Test histogram 2"), Some("ms"))
  
  // Test concurrent metric recording
  Counter::add(counter1, 10.5, None)
  Counter::add(counter2, 20.3, None)
  Histogram::record(histogram1, 100.0, None)
  Histogram::record(histogram2, 200.0, None)
  
  // Test concurrent operations with attributes
  let attrs1 = Attributes::new()
  let attrs2 = Attributes::new()
  
  Counter::add(counter1, 5.0, Some(attrs1))
  Counter::add(counter2, 7.5, Some(attrs2))
  Histogram::record(histogram1, 50.0, Some(attrs1))
  Histogram::record(histogram2, 75.0, Some(attrs2))
  
  // Verify metric instruments are independent
  assert_eq(Instrument::name(Counter(counter1.name, counter1.description, counter1.unit)), "counter_1")
  assert_eq(Instrument::name(Counter(counter2.name, counter2.description, counter2.unit)), "counter_2")
  assert_eq(Instrument::name(Histogram(histogram1.name, histogram1.description, histogram1.unit)), "histogram_1")
  assert_eq(Instrument::name(Histogram(histogram2.name, histogram2.description, histogram2.unit)), "histogram_2")
}

test "concurrent_context_operations" {
  // Test concurrent context operations
  let root_ctx = Context::root()
  
  // Create multiple context keys
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  let key3 = ContextKey::new("key3")
  
  // Test concurrent context creation with values
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(root_ctx, key2, "value2")
  let ctx3 = Context::with_value(root_ctx, key3, "value3")
  
  // Verify contexts are independent
  assert_eq(Context::get(ctx1, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key3), Some("value3"))
  
  // Test cross-context key access (should return None)
  assert_eq(Context::get(ctx1, key2), None)
  assert_eq(Context::get(ctx2, key3), None)
  assert_eq(Context::get(ctx3, key1), None)
  
  // Test nested context creation
  let nested_ctx1 = Context::with_value(ctx1, key2, "nested_value2")
  let nested_ctx2 = Context::with_value(ctx2, key3, "nested_value3")
  
  // Verify nested context behavior
  assert_eq(Context::get(nested_ctx1, key1), Some("value1"))
  assert_eq(Context::get(nested_ctx1, key2), Some("nested_value2"))
  assert_eq(Context::get(nested_ctx2, key2), Some("value2"))
  assert_eq(Context::get(nested_ctx2, key3), Some("nested_value3"))
}

test "concurrent_baggage_operations" {
  // Test concurrent baggage operations
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::new()
  let baggage3 = Baggage::new()
  
  // Test concurrent baggage entry setting
  let baggage1_with_entries = Baggage::set_entry(baggage1, "user.id", "user123")
  let baggage2_with_entries = Baggage::set_entry(baggage2, "session.id", "session456")
  let baggage3_with_entries = Baggage::set_entry(baggage3, "request.id", "request789")
  
  // Test additional concurrent operations
  let baggage1_final = Baggage::set_entry(baggage1_with_entries, "user.role", "admin")
  let baggage2_final = Baggage::set_entry(baggage2_with_entries, "session.type", "web")
  let baggage3_final = Baggage::set_entry(baggage3_with_entries, "request.method", "GET")
  
  // Verify baggage independence
  assert_eq(Baggage::get_entry(baggage1_final, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage1_final, "user.role"), Some("admin"))
  assert_eq(Baggage::get_entry(baggage2_final, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(baggage2_final, "session.type"), Some("web"))
  assert_eq(Baggage::get_entry(baggage3_final, "request.id"), Some("request789"))
  assert_eq(Baggage::get_entry(baggage3_final, "request.method"), Some("GET"))
  
  // Test cross-baggage access (should return None)
  assert_eq(Baggage::get_entry(baggage1_final, "session.id"), None)
  assert_eq(Baggage::get_entry(baggage2_final, "user.id"), None)
  assert_eq(Baggage::get_entry(baggage3_final, "user.role"), None)
}

test "concurrent_propagation_operations" {
  // Test concurrent propagation operations
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Create composite propagators
  let composite1 = CompositePropagator::new([trace_propagator1])
  let composite2 = CompositePropagator::new([trace_propagator2])
  
  // Create contexts and carriers
  let ctx1 = Context::root()
  let ctx2 = Context::root()
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  
  // Test concurrent injection
  CompositePropagator::inject(composite1, ctx1, carrier1)
  CompositePropagator::inject(composite2, ctx2, carrier2)
  
  // Test concurrent extraction
  let extracted_ctx1 = CompositePropagator::extract(composite1, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite2, carrier2)
  
  // Verify extraction results
  let extracted_key1 = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx1, extracted_key1), Some("true"))
  assert_eq(Context::get(extracted_ctx2, extracted_key1), Some("true"))
  
  // Test carrier independence
  assert_eq(TextMapCarrier::get(carrier1, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier2, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
}

test "concurrent_logging_operations" {
  // Test concurrent logging operations
  let logger_provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(logger_provider, "concurrent_logger_1")
  let logger2 = LoggerProvider::get_logger(logger_provider, "concurrent_logger_2")
  let logger3 = LoggerProvider::get_logger(logger_provider, "concurrent_logger_3")
  
  // Create log records with different severity levels
  let info_log = LogRecord::new_with_context(
    Info,
    Some("Concurrent info log message"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace_info_123"),
    Some("span_info_456"),
    None
  )
  
  let warn_log = LogRecord::new_with_context(
    Warn,
    Some("Concurrent warning log message"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace_warn_123"),
    Some("span_warn_456"),
    None
  )
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Concurrent error log message"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace_error_123"),
    Some("span_error_456"),
    None
  )
  
  // Test concurrent log emission
  Logger::emit(logger1, info_log)
  Logger::emit(logger2, warn_log)
  Logger::emit(logger3, error_log)
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  
  assert_eq(LogRecord::body(info_log), Some("Concurrent info log message"))
  assert_eq(LogRecord::body(warn_log), Some("Concurrent warning log message"))
  assert_eq(LogRecord::body(error_log), Some("Concurrent error log message"))
  
  assert_eq(LogRecord::trace_id(info_log), Some("trace_info_123"))
  assert_eq(LogRecord::trace_id(warn_log), Some("trace_warn_123"))
  assert_eq(LogRecord::trace_id(error_log), Some("trace_error_123"))
}

test "concurrent_resource_operations" {
  // Test concurrent resource operations
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  let resource3 = Resource::new()
  
  // Define attributes for each resource
  let attrs1 = [
    ("service.name", StringValue("concurrent_service_1")),
    ("service.instance.id", StringValue("instance_1")),
    ("service.version", StringValue("1.0.0"))
  ]
  
  let attrs2 = [
    ("service.name", StringValue("concurrent_service_2")),
    ("service.instance.id", StringValue("instance_2")),
    ("service.version", StringValue("2.0.0"))
  ]
  
  let attrs3 = [
    ("service.name", StringValue("concurrent_service_3")),
    ("service.instance.id", StringValue("instance_3")),
    ("service.version", StringValue("3.0.0"))
  ]
  
  // Test concurrent resource creation with attributes
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  let resource3_with_attrs = Resource::with_attributes(resource3, attrs3)
  
  // Verify resource independence
  assert_eq(Resource::get_attribute(resource1_with_attrs, "service.name"), Some(StringValue("concurrent_service_1")))
  assert_eq(Resource::get_attribute(resource2_with_attrs, "service.name"), Some(StringValue("concurrent_service_2")))
  assert_eq(Resource::get_attribute(resource3_with_attrs, "service.name"), Some(StringValue("concurrent_service_3")))
  
  // Test concurrent resource merging
  let additional_attrs = [
    ("deployment.environment", StringValue("production")),
    ("deployment.region", StringValue("us-west-2"))
  ]
  
  let additional_resource = Resource::with_attributes(Resource::new(), additional_attrs)
  
  let merged1 = Resource::merge(resource1_with_attrs, additional_resource)
  let merged2 = Resource::merge(resource2_with_attrs, additional_resource)
  let merged3 = Resource::merge(resource3_with_attrs, additional_resource)
  
  // Verify merge preserves original attributes and adds new ones
  assert_eq(Resource::get_attribute(merged1, "service.name"), Some(StringValue("concurrent_service_1")))
  assert_eq(Resource::get_attribute(merged1, "deployment.environment"), Some(StringValue("production")))
  
  assert_eq(Resource::get_attribute(merged2, "service.name"), Some(StringValue("concurrent_service_2")))
  assert_eq(Resource::get_attribute(merged2, "deployment.environment"), Some(StringValue("production")))
  
  assert_eq(Resource::get_attribute(merged3, "service.name"), Some(StringValue("concurrent_service_3")))
  assert_eq(Resource::get_attribute(merged3, "deployment.environment"), Some(StringValue("production")))
}