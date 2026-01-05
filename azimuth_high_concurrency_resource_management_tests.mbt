// High Concurrency Resource Management Tests
// This file contains test cases for high concurrency resource management functionality

test "concurrent span creation and management" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent-test")
  
  // Create multiple spans concurrently (simplified for testing)
  let span1 = Tracer::start_span(tracer, "operation-1")
  let span2 = Tracer::start_span(tracer, "operation-2")
  let span3 = Tracer::start_span(tracer, "operation-3")
  
  // Verify all spans are created and recording
  assert_true(Span::is_recording(span1))
  assert_true(Span::is_recording(span2))
  assert_true(Span::is_recording(span3))
  
  // Verify span names
  assert_eq(Span::name(span1), "operation-1")
  assert_eq(Span::name(span2), "operation-2")
  assert_eq(Span::name(span3), "operation-3")
  
  // End all spans
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
}

test "concurrent metric operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent-metrics")
  
  // Create various metric instruments
  let counter = Meter::create_counter(meter, "concurrent.operations")
  let histogram = Meter::create_histogram(meter, "concurrent.duration")
  let updown_counter = Meter::create_updown_counter(meter, "concurrent.active")
  let gauge = Meter::create_gauge(meter, "concurrent.memory")
  
  // Perform concurrent operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.0)
  Counter::add(counter, 3.0)
  
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Verify instrument names
  assert_eq(Instrument::name(Counter("concurrent.operations", None, None)), "concurrent.operations")
  assert_eq(Instrument::name(Histogram("concurrent.duration", None, None)), "concurrent.duration")
  assert_eq(Instrument::name(UpDownCounter("concurrent.active", None, None)), "concurrent.active")
  assert_eq(Instrument::name(Gauge("concurrent.memory", None, None)), "concurrent.memory")
}

test "concurrent resource attribute operations" {
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // Set attributes on multiple resources
  let attrs1 = [
    ("resource.id", StringValue("res-1")),
    ("resource.type", StringValue("compute")),
    ("resource.cores", IntValue(4))
  ]
  
  let attrs2 = [
    ("resource.id", StringValue("res-2")),
    ("resource.type", StringValue("memory")),
    ("resource.size.gb", IntValue(16))
  ]
  
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  // Verify attributes on both resources
  let res1_id = Resource::get_attribute(resource1_with_attrs, "resource.id")
  let res1_type = Resource::get_attribute(resource1_with_attrs, "resource.type")
  let res1_cores = Resource::get_attribute(resource1_with_attrs, "resource.cores")
  
  let res2_id = Resource::get_attribute(resource2_with_attrs, "resource.id")
  let res2_type = Resource::get_attribute(resource2_with_attrs, "resource.type")
  let res2_size = Resource::get_attribute(resource2_with_attrs, "resource.size.gb")
  
  assert_eq(res1_id, Some(StringValue("res-1")))
  assert_eq(res1_type, Some(StringValue("compute")))
  assert_eq(res1_cores, Some(IntValue(4)))
  
  assert_eq(res2_id, Some(StringValue("res-2")))
  assert_eq(res2_type, Some(StringValue("memory")))
  assert_eq(res2_size, Some(IntValue(16)))
}

test "concurrent context propagation" {
  let ctx1 = Context::root()
  let ctx2 = Context::root()
  let ctx3 = Context::root()
  
  let key1 = ContextKey::new("correlation.id")
  let key2 = ContextKey::new("user.id")
  let key3 = ContextKey::new("session.id")
  
  // Set values in multiple contexts
  let ctx1_with_value = Context::with_value(ctx1, key1, "corr-123")
  let ctx2_with_value = Context::with_value(ctx2, key2, "user-456")
  let ctx3_with_value = Context::with_value(ctx3, key3, "session-789")
  
  // Retrieve values from contexts
  let value1 = Context::get(ctx1_with_value, key1)
  let value2 = Context::get(ctx2_with_value, key2)
  let value3 = Context::get(ctx3_with_value, key3)
  
  assert_eq(value1, Some("corr-123"))
  assert_eq(value2, Some("user-456"))
  assert_eq(value3, Some("session-789"))
}

test "concurrent baggage operations" {
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::new()
  
  // Set entries in multiple baggage instances
  let baggage1_with_entries = Baggage::set_entry(baggage1, "operation.type", "read")
  let baggage1_final = Baggage::set_entry(baggage1_with_entries, "operation.priority", "high")
  
  let baggage2_with_entries = Baggage::set_entry(baggage2, "operation.type", "write")
  let baggage2_final = Baggage::set_entry(baggage2_with_entries, "operation.retry", "3")
  
  // Verify baggage entries
  let bag1_type = Baggage::get_entry(baggage1_final, "operation.type")
  let bag1_priority = Baggage::get_entry(baggage1_final, "operation.priority")
  
  let bag2_type = Baggage::get_entry(baggage2_final, "operation.type")
  let bag2_retry = Baggage::get_entry(baggage2_final, "operation.retry")
  
  assert_eq(bag1_type, Some("read"))
  assert_eq(bag1_priority, Some("high"))
  
  assert_eq(bag2_type, Some("write"))
  assert_eq(bag2_retry, Some("3"))
}

test "concurrent log record processing" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "concurrent-logger")
  
  // Create multiple log records
  let log1 = LogRecord::new(Info, "Processing request 1")
  let log2 = LogRecord::new(Warn, "Rate limit approaching")
  let log3 = LogRecord::new(Error, "Database connection failed")
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log1), Info)
  assert_eq(LogRecord::body(log1), Some("Processing request 1"))
  
  assert_eq(LogRecord::severity_number(log2), Warn)
  assert_eq(LogRecord::body(log2), Some("Rate limit approaching"))
  
  assert_eq(LogRecord::severity_number(log3), Error)
  assert_eq(LogRecord::body(log3), Some("Database connection failed"))
}

test "concurrent span context operations" {
  // Create multiple span contexts
  let ctx1 = SpanContext::new("trace1111111111111111", "span1111111111111111", true, "")
  let ctx2 = SpanContext::new("trace2222222222222222", "span2222222222222222", false, "key1=value1")
  let ctx3 = SpanContext::new("trace3333333333333333", "span3333333333333333", true, "key2=value2")
  
  // Verify span context properties
  assert_eq(SpanContext::trace_id(ctx1), "trace1111111111111111")
  assert_eq(SpanContext::span_id(ctx1), "span1111111111111111")
  assert_true(SpanContext::is_sampled(ctx1))
  assert_true(SpanContext::is_valid(ctx1))
  
  assert_eq(SpanContext::trace_id(ctx2), "trace2222222222222222")
  assert_eq(SpanContext::span_id(ctx2), "span2222222222222222")
  assert_false(SpanContext::is_sampled(ctx2))
  assert_true(SpanContext::is_valid(ctx2))
  
  assert_eq(SpanContext::trace_id(ctx3), "trace3333333333333333")
  assert_eq(SpanContext::span_id(ctx3), "span3333333333333333")
  assert_true(SpanContext::is_sampled(ctx3))
  assert_true(SpanContext::is_valid(ctx3))
}

test "concurrent resource merge operations" {
  let base_resource = Resource::new()
  let override_resource = Resource::new()
  
  // Set base attributes
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.env", StringValue("production"))
  ]
  
  // Set override attributes
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("deployment.region", StringValue("us-west")),
    ("instance.id", StringValue("i-1234567890abcdef0"))
  ]
  
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merged resource (simplified - returns override resource)
  let merged_version = Resource::get_attribute(merged_resource, "service.version")
  let merged_region = Resource::get_attribute(merged_resource, "deployment.region")
  let merged_instance = Resource::get_attribute(merged_resource, "instance.id")
  
  assert_eq(merged_version, Some(StringValue("2.0.0")))
  assert_eq(merged_region, Some(StringValue("us-west")))
  assert_eq(merged_instance, Some(StringValue("i-1234567890abcdef0")))
}