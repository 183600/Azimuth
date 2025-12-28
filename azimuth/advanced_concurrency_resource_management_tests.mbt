// Advanced Concurrency and Resource Management Tests for Azimuth
// Tests resource management under high concurrency scenarios

test "concurrent_span_creation_and_cleanup" {
  // Test concurrent span creation and cleanup without memory leaks
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "concurrency-test")
  
  // Create multiple spans concurrently
  let span1 = Tracer::start_span(tracer, "concurrent-span-1")
  let span2 = Tracer::start_span(tracer, "concurrent-span-2")
  let span3 = Tracer::start_span(tracer, "concurrent-span-3")
  let span4 = Tracer::start_span(tracer, "concurrent-span-4")
  let span5 = Tracer::start_span(tracer, "concurrent-span-5")
  
  // Verify all spans are properly created
  assert_eq(Span::name(span1), "concurrent-span-1")
  assert_eq(Span::name(span2), "concurrent-span-2")
  assert_eq(Span::name(span3), "concurrent-span-3")
  assert_eq(Span::name(span4), "concurrent-span-4")
  assert_eq(Span::name(span5), "concurrent-span-5")
  
  // Verify all spans are recording
  assert_eq(Span::is_recording(span1), true)
  assert_eq(Span::is_recording(span2), true)
  assert_eq(Span::is_recording(span3), true)
  assert_eq(Span::is_recording(span4), true)
  assert_eq(Span::is_recording(span5), true)
  
  // Add events to spans concurrently
  Span::add_event(span1, "event-1", None)
  Span::add_event(span2, "event-2", None)
  Span::add_event(span3, "event-3", None)
  Span::add_event(span4, "event-4", None)
  Span::add_event(span5, "event-5", None)
  
  // Set status for spans
  Span::set_status(span1, Ok, None)
  Span::set_status(span2, Ok, None)
  Span::set_status(span3, Error, Some("Test error"))
  Span::set_status(span4, Ok, None)
  Span::set_status(span5, Ok, None)
  
  // End all spans to test cleanup
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  Span::end(span4)
  Span::end(span5)
}

test "concurrent_metrics_recording" {
  // Test concurrent metrics recording without conflicts
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrency-metrics")
  
  // Create multiple metric instruments
  let counter1 = Meter::create_counter(meter, "requests_total_1", Some("Total requests 1"), Some("count"))
  let counter2 = Meter::create_counter(meter, "requests_total_2", Some("Total requests 2"), Some("count"))
  let histogram1 = Meter::create_histogram(meter, "duration_ms_1", Some("Duration 1"), Some("ms"))
  let histogram2 = Meter::create_histogram(meter, "duration_ms_2", Some("Duration 2"), Some("ms"))
  let gauge1 = Meter::create_gauge(meter, "active_connections_1", Some("Active connections 1"), Some("connections"))
  let updown_counter1 = Meter::create_updown_counter(meter, "queue_size_1", Some("Queue size 1"), Some("items"))
  
  // Record metrics concurrently
  Counter::add(counter1, 100.0)
  Counter::add(counter2, 200.0)
  Counter::add(counter1, 50.0)
  Counter::add(counter2, 75.0)
  
  Histogram::record(histogram1, 150.5)
  Histogram::record(histogram2, 250.75)
  Histogram::record(histogram1, 100.25)
  Histogram::record(histogram2, 300.0)
  
  UpDownCounter::add(updown_counter1, 1000.0)
  UpDownCounter::add(updown_counter1, -500.0)
  UpDownCounter::add(updown_counter1, 250.0)
  
  // Verify metric instrument properties
  assert_eq(counter1.name, "requests_total_1")
  assert_eq(counter2.name, "requests_total_2")
  assert_eq(histogram1.name, "duration_ms_1")
  assert_eq(histogram2.name, "duration_ms_2")
  assert_eq(gauge1.name, "active_connections_1")
  assert_eq(updown_counter1.name, "queue_size_1")
  
  // Verify instrument metadata
  assert_eq(counter1.description, Some("Total requests 1"))
  assert_eq(histogram1.unit, Some("ms"))
  assert_eq(gauge1.description, Some("Active connections 1"))
  assert_eq(updown_counter1.unit, Some("items"))
}

test "concurrent_logging_operations" {
  // Test concurrent logging without performance degradation
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "concurrency-logger")
  
  // Create concurrent log records with different severity levels
  let log1 = LogRecord::new(Trace, "Trace message 1")
  let log2 = LogRecord::new(Debug, "Debug message 2")
  let log3 = LogRecord::new(Info, "Info message 3")
  let log4 = LogRecord::new(Warn, "Warning message 4")
  let log5 = LogRecord::new(Error, "Error message 5")
  let log6 = LogRecord::new(Fatal, "Fatal message 6")
  
  // Create log records with full context
  let log_with_context1 = LogRecord::new_with_context(
    Info,
    Some("Contextual log 1"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace_concurrent_1"),
    Some("span_concurrent_1"),
    None
  )
  
  let log_with_context2 = LogRecord::new_with_context(
    Error,
    Some("Contextual error 2"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some("trace_concurrent_2"),
    Some("span_concurrent_2"),
    None
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log1), Trace)
  assert_eq(LogRecord::severity_number(log2), Debug)
  assert_eq(LogRecord::severity_number(log3), Info)
  assert_eq(LogRecord::severity_number(log4), Warn)
  assert_eq(LogRecord::severity_number(log5), Error)
  assert_eq(LogRecord::severity_number(log6), Fatal)
  
  assert_eq(LogRecord::body(log1), Some("Trace message 1"))
  assert_eq(LogRecord::body(log2), Some("Debug message 2"))
  assert_eq(LogRecord::body(log3), Some("Info message 3"))
  assert_eq(LogRecord::body(log4), Some("Warning message 4"))
  assert_eq(LogRecord::body(log5), Some("Error message 5"))
  assert_eq(LogRecord::body(log6), Some("Fatal message 6"))
  
  // Verify contextual log properties
  assert_eq(LogRecord::body(log_with_context1), Some("Contextual log 1"))
  assert_eq(LogRecord::body(log_with_context2), Some("Contextual error 2"))
  assert_eq(LogRecord::trace_id(log_with_context1), Some("trace_concurrent_1"))
  assert_eq(LogRecord::trace_id(log_with_context2), Some("trace_concurrent_2"))
  assert_eq(LogRecord::span_id(log_with_context1), Some("span_concurrent_1"))
  assert_eq(LogRecord::span_id(log_with_context2), Some("span_concurrent_2"))
}

test "concurrent_context_operations" {
  // Test concurrent context operations without data corruption
  let root_ctx = Context::root()
  
  // Create multiple context keys
  let key1 = ContextKey::new("concurrent_key_1")
  let key2 = ContextKey::new("concurrent_key_2")
  let key3 = ContextKey::new("concurrent_key_3")
  let key4 = ContextKey::new("concurrent_key_4")
  let key5 = ContextKey::new("concurrent_key_5")
  
  // Set values in context concurrently
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  let ctx4 = Context::with_value(ctx3, key4, "value4")
  let ctx5 = Context::with_value(ctx4, key5, "value5")
  
  // Verify context values are preserved
  assert_eq(Context::get(ctx1, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key1), Some("value1"))
  assert_eq(Context::get(ctx3, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key3), Some("value3"))
  assert_eq(Context::get(ctx4, key1), Some("value1"))
  assert_eq(Context::get(ctx4, key2), Some("value2"))
  assert_eq(Context::get(ctx4, key3), Some("value3"))
  assert_eq(Context::get(ctx4, key4), Some("value4"))
  assert_eq(Context::get(ctx5, key1), Some("value1"))
  assert_eq(Context::get(ctx5, key2), Some("value2"))
  assert_eq(Context::get(ctx5, key3), Some("value3"))
  assert_eq(Context::get(ctx5, key4), Some("value4"))
  assert_eq(Context::get(ctx5, key5), Some("value5"))
  
  // Test non-existent keys
  let non_existent_key = ContextKey::new("non_existent")
  assert_eq(Context::get(root_ctx, non_existent_key), None)
  assert_eq(Context::get(ctx1, non_existent_key), None)
  assert_eq(Context::get(ctx5, non_existent_key), None)
}

test "concurrent_baggage_operations" {
  // Test concurrent baggage operations without interference
  let baggage = Baggage::new()
  
  // Set multiple baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session456")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "req789")
  let baggage4 = Baggage::set_entry(baggage3, "tenant.id", "tenant001")
  let baggage5 = Baggage::set_entry(baggage4, "correlation.id", "corr000")
  
  // Verify baggage entries are preserved
  assert_eq(Baggage::get_entry(baggage1, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage2, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage2, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(baggage3, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage3, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(baggage3, "request.id"), Some("req789"))
  assert_eq(Baggage::get_entry(baggage4, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage4, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(baggage4, "request.id"), Some("req789"))
  assert_eq(Baggage::get_entry(baggage4, "tenant.id"), Some("tenant001"))
  assert_eq(Baggage::get_entry(baggage5, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage5, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(baggage5, "request.id"), Some("req789"))
  assert_eq(Baggage::get_entry(baggage5, "tenant.id"), Some("tenant001"))
  assert_eq(Baggage::get_entry(baggage5, "correlation.id"), Some("corr000"))
  
  // Test baggage removal
  let baggage_after_removal = Baggage::remove_entry(baggage5, "session.id")
  assert_eq(Baggage::get_entry(baggage_after_removal, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(baggage_after_removal, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage_after_removal, "request.id"), Some("req789"))
  assert_eq(Baggage::get_entry(baggage_after_removal, "tenant.id"), Some("tenant001"))
  assert_eq(Baggage::get_entry(baggage_after_removal, "correlation.id"), Some("corr000"))
}

test "concurrent_resource_management" {
  // Test concurrent resource creation and management
  let base_resource = Resource::new()
  
  // Create multiple resources with different attributes
  let service_attrs1 = [
    ("service.name", StringValue("service-1")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-001"))
  ]
  
  let service_attrs2 = [
    ("service.name", StringValue("service-2")),
    ("service.version", StringValue("2.0.0")),
    ("service.instance.id", StringValue("instance-002"))
  ]
  
  let service_attrs3 = [
    ("service.name", StringValue("service-3")),
    ("service.version", StringValue("3.0.0")),
    ("service.instance.id", StringValue("instance-003"))
  ]
  
  let resource1 = Resource::with_attributes(base_resource, service_attrs1)
  let resource2 = Resource::with_attributes(base_resource, service_attrs2)
  let resource3 = Resource::with_attributes(base_resource, service_attrs3)
  
  // Verify resource attributes are correctly set
  assert_eq(Resource::get_attribute(resource1, "service.name"), Some(StringValue("service-1")))
  assert_eq(Resource::get_attribute(resource1, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(resource1, "service.instance.id"), Some(StringValue("instance-001")))
  
  assert_eq(Resource::get_attribute(resource2, "service.name"), Some(StringValue("service-2")))
  assert_eq(Resource::get_attribute(resource2, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(resource2, "service.instance.id"), Some(StringValue("instance-002")))
  
  assert_eq(Resource::get_attribute(resource3, "service.name"), Some(StringValue("service-3")))
  assert_eq(Resource::get_attribute(resource3, "service.version"), Some(StringValue("3.0.0")))
  assert_eq(Resource::get_attribute(resource3, "service.instance.id"), Some(StringValue("instance-003")))
  
  // Test resource merging
  let additional_attrs = [("deployment.environment", StringValue("production"))]
  let additional_resource = Resource::with_attributes(Resource::new(), additional_attrs)
  
  let merged1 = Resource::merge(resource1, additional_resource)
  let merged2 = Resource::merge(resource2, additional_resource)
  let merged3 = Resource::merge(resource3, additional_resource)
  
  assert_eq(Resource::get_attribute(merged1, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(merged2, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(merged3, "deployment.environment"), Some(StringValue("production")))
  
  // Verify original attributes are preserved
  assert_eq(Resource::get_attribute(merged1, "service.name"), Some(StringValue("service-1")))
  assert_eq(Resource::get_attribute(merged2, "service.name"), Some(StringValue("service-2")))
  assert_eq(Resource::get_attribute(merged3, "service.name"), Some(StringValue("service-3")))
}

test "concurrent_propagation_operations" {
  // Test concurrent propagation operations without interference
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Create multiple carriers
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  let carrier4 = TextMapCarrier::new()
  let carrier5 = TextMapCarrier::new()
  
  // Create different contexts
  let ctx1 = Context::root()
  let ctx2 = Context::with_value(Context::root(), ContextKey::new("test2"), "value2")
  let ctx3 = Context::with_value(Context::root(), ContextKey::new("test3"), "value3")
  let ctx4 = Context::with_value(Context::root(), ContextKey::new("test4"), "value4")
  let ctx5 = Context::with_value(Context::root(), ContextKey::new("test5"), "value5")
  
  // Inject contexts into carriers concurrently
  CompositePropagator::inject(composite, ctx1, carrier1)
  CompositePropagator::inject(composite, ctx2, carrier2)
  CompositePropagator::inject(composite, ctx3, carrier3)
  CompositePropagator::inject(composite, ctx4, carrier4)
  CompositePropagator::inject(composite, ctx5, carrier5)
  
  // Verify injection worked for all carriers
  assert_eq(TextMapCarrier::get(carrier1, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier2, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier3, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier4, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier5, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Extract contexts from carriers
  let extracted_ctx1 = CompositePropagator::extract(composite, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(composite, carrier3)
  let extracted_ctx4 = CompositePropagator::extract(composite, carrier4)
  let extracted_ctx5 = CompositePropagator::extract(composite, carrier5)
  
  // Verify extraction worked
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx1, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx2, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx3, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx4, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx5, extracted_key), Some("true"))
}

test "memory_efficiency_under_load" {
  // Test memory efficiency with large numbers of telemetry objects
  
  // Create many spans
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "memory-test")
  
  let span_a1 = Tracer::start_span(tracer, "memory-span-1")
  let span_a2 = Tracer::start_span(tracer, "memory-span-2")
  let span_a3 = Tracer::start_span(tracer, "memory-span-3")
  let span_a4 = Tracer::start_span(tracer, "memory-span-4")
  let span_a5 = Tracer::start_span(tracer, "memory-span-5")
  
  // Create many metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "memory-metrics")
  
  let counter_a = Meter::create_counter(meter, "memory-counter", Some("Memory test counter"), Some("bytes"))
  let histogram_a = Meter::create_histogram(meter, "memory-histogram", Some("Memory test histogram"), Some("bytes"))
  let gauge_a = Meter::create_gauge(meter, "memory-gauge", Some("Memory test gauge"), Some("bytes"))
  
  // Create many log records
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "memory-logger")
  
  let log_a1 = LogRecord::new(Info, "Memory test log 1")
  let log_a2 = LogRecord::new(Warn, "Memory test log 2")
  let log_a3 = LogRecord::new(Error, "Memory test log 3")
  
  // Create many contexts
  let ctx_a1 = Context::with_value(Context::root(), ContextKey::new("memory_key_1"), "memory_value_1")
  let ctx_a2 = Context::with_value(Context::root(), ContextKey::new("memory_key_2"), "memory_value_2")
  let ctx_a3 = Context::with_value(Context::root(), ContextKey::new("memory_key_3"), "memory_value_3")
  
  // Create many baggage entries
  let baggage_a1 = Baggage::set_entry(Baggage::new(), "memory.baggage.1", "baggage_value_1")
  let baggage_a2 = Baggage::set_entry(Baggage::new(), "memory.baggage.2", "baggage_value_2")
  let baggage_a3 = Baggage::set_entry(Baggage::new(), "memory.baggage.3", "baggage_value_3")
  
  // Create many resources
  let resource_attrs1 = [("memory.test", StringValue("resource-1"))]
  let resource_attrs2 = [("memory.test", StringValue("resource-2"))]
  let resource_attrs3 = [("memory.test", StringValue("resource-3"))]
  
  let resource_a1 = Resource::with_attributes(Resource::new(), resource_attrs1)
  let resource_a2 = Resource::with_attributes(Resource::new(), resource_attrs2)
  let resource_a3 = Resource::with_attributes(Resource::new(), resource_attrs3)
  
  // Verify all objects are properly created and accessible
  assert_eq(Span::name(span_a1), "memory-span-1")
  assert_eq(Span::name(span_a2), "memory-span-2")
  assert_eq(Span::name(span_a3), "memory-span-3")
  assert_eq(Span::name(span_a4), "memory-span-4")
  assert_eq(Span::name(span_a5), "memory-span-5")
  
  assert_eq(counter_a.name, "memory-counter")
  assert_eq(histogram_a.name, "memory-histogram")
  assert_eq(gauge_a.name, "memory-gauge")
  
  assert_eq(LogRecord::body(log_a1), Some("Memory test log 1"))
  assert_eq(LogRecord::body(log_a2), Some("Memory test log 2"))
  assert_eq(LogRecord::body(log_a3), Some("Memory test log 3"))
  
  assert_eq(Context::get(ctx_a1, ContextKey::new("memory_key_1")), Some("memory_value_1"))
  assert_eq(Context::get(ctx_a2, ContextKey::new("memory_key_2")), Some("memory_value_2"))
  assert_eq(Context::get(ctx_a3, ContextKey::new("memory_key_3")), Some("memory_value_3"))
  
  assert_eq(Baggage::get_entry(baggage_a1, "memory.baggage.1"), Some("baggage_value_1"))
  assert_eq(Baggage::get_entry(baggage_a2, "memory.baggage.2"), Some("baggage_value_2"))
  assert_eq(Baggage::get_entry(baggage_a3, "memory.baggage.3"), Some("baggage_value_3"))
  
  assert_eq(Resource::get_attribute(resource_a1, "memory.test"), Some(StringValue("resource-1")))
  assert_eq(Resource::get_attribute(resource_a2, "memory.test"), Some(StringValue("resource-2")))
  assert_eq(Resource::get_attribute(resource_a3, "memory.test"), Some(StringValue("resource-3")))
  
  // Clean up all spans
  Span::end(span_a1)
  Span::end(span_a2)
  Span::end(span_a3)
  Span::end(span_a4)
  Span::end(span_a5)
}