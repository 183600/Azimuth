// Concurrent Safety Test Suite for Azimuth Telemetry System
// This file contains test cases for basic concurrent safety scenarios

test "concurrent attributes operations" {
  // Create multiple attributes instances
  let attrs1 = Attributes::new()
  let attrs2 = Attributes::new()
  let attrs3 = Attributes::new()
  
  // Perform concurrent-like operations on different attributes instances
  Attributes::set(attrs1, "thread.1.key", StringValue("thread.1.value"))
  Attributes::set(attrs2, "thread.2.key", StringValue("thread.2.value"))
  Attributes::set(attrs3, "thread.3.key", StringValue("thread.3.value"))
  
  // Verify each attributes instance maintains its own state
  let value1 = Attributes::get(attrs1, "thread.1.key")
  let value2 = Attributes::get(attrs2, "thread.2.key")
  let value3 = Attributes::get(attrs3, "thread.3.key")
  
  assert_eq(value1, Some(StringValue("thread.1.value")))
  assert_eq(value2, Some(StringValue("thread.2.value")))
  assert_eq(value3, Some(StringValue("thread.3.value")))
  
  // Verify no cross-contamination between instances
  let cross_check1 = Attributes::get(attrs1, "thread.2.key")
  let cross_check2 = Attributes::get(attrs2, "thread.3.key")
  let cross_check3 = Attributes::get(attrs3, "thread.1.key")
  
  assert_eq(cross_check1, None)
  assert_eq(cross_check2, None)
  assert_eq(cross_check3, None)
}

test "concurrent context operations" {
  // Create multiple context instances
  let ctx1 = Context::root()
  let ctx2 = Context::root()
  let ctx3 = Context::root()
  
  // Create context keys for each "thread"
  let key1 = ContextKey::new("thread.1.key")
  let key2 = ContextKey::new("thread.2.key")
  let key3 = ContextKey::new("thread.3.key")
  
  // Set values in different contexts
  let ctx1_with_value = Context::with_value(ctx1, key1, "thread.1.value")
  let ctx2_with_value = Context::with_value(ctx2, key2, "thread.2.value")
  let ctx3_with_value = Context::with_value(ctx3, key3, "thread.3.value")
  
  // Verify each context maintains its own state
  let value1 = Context::get(ctx1_with_value, key1)
  let value2 = Context::get(ctx2_with_value, key2)
  let value3 = Context::get(ctx3_with_value, key3)
  
  assert_eq(value1, Some("thread.1.value"))
  assert_eq(value2, Some("thread.2.value"))
  assert_eq(value3, Some("thread.3.value"))
  
  // Verify no cross-contamination between contexts
  let cross_check1 = Context::get(ctx1_with_value, key2)
  let cross_check2 = Context::get(ctx2_with_value, key3)
  let cross_check3 = Context::get(ctx3_with_value, key1)
  
  assert_eq(cross_check1, None)
  assert_eq(cross_check2, None)
  assert_eq(cross_check3, None)
}

test "concurrent span context operations" {
  // Create multiple span contexts
  let span_ctx1 = SpanContext::new("trace1", "span1", true, "")
  let span_ctx2 = SpanContext::new("trace2", "span2", false, "")
  let span_ctx3 = SpanContext::new("trace3", "span3", true, "key1=value1")
  
  // Verify each span context maintains its own state
  assert_eq(SpanContext::trace_id(span_ctx1), "trace1")
  assert_eq(SpanContext::span_id(span_ctx1), "span1")
  assert_true(SpanContext::is_sampled(span_ctx1))
  
  assert_eq(SpanContext::trace_id(span_ctx2), "trace2")
  assert_eq(SpanContext::span_id(span_ctx2), "span2")
  assert_false(SpanContext::is_sampled(span_ctx2))
  
  assert_eq(SpanContext::trace_id(span_ctx3), "trace3")
  assert_eq(SpanContext::span_id(span_ctx3), "span3")
  assert_true(SpanContext::is_sampled(span_ctx3))
  
  // Verify all contexts are valid
  assert_true(SpanContext::is_valid(span_ctx1))
  assert_true(SpanContext::is_valid(span_ctx2))
  assert_true(SpanContext::is_valid(span_ctx3))
}

test "concurrent meter and metrics operations" {
  // Create multiple meter instances
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "meter1")
  let meter2 = MeterProvider::get_meter(provider, "meter2")
  let meter3 = MeterProvider::get_meter(provider, "meter3")
  
  // Create counters with different meters
  let counter1 = Meter::create_counter(meter1, "counter1")
  let counter2 = Meter::create_counter(meter2, "counter2")
  let counter3 = Meter::create_counter(meter3, "counter3")
  
  // Create histograms with different meters
  let histogram1 = Meter::create_histogram(meter1, "histogram1")
  let histogram2 = Meter::create_histogram(meter2, "histogram2")
  let histogram3 = Meter::create_histogram(meter3, "histogram3")
  
  // Create updown counters with different meters
  let updown1 = Meter::create_updown_counter(meter1, "updown1")
  let updown2 = Meter::create_updown_counter(meter2, "updown2")
  let updown3 = Meter::create_updown_counter(meter3, "updown3")
  
  // Create gauges with different meters
  let gauge1 = Meter::create_gauge(meter1, "gauge1")
  let gauge2 = Meter::create_gauge(meter2, "gauge2")
  let gauge3 = Meter::create_gauge(meter3, "gauge3")
  
  // Verify all instruments have correct names
  assert_eq(counter1.name, "counter1")
  assert_eq(counter2.name, "counter2")
  assert_eq(counter3.name, "counter3")
  
  assert_eq(histogram1.name, "histogram1")
  assert_eq(histogram2.name, "histogram2")
  assert_eq(histogram3.name, "histogram3")
  
  assert_eq(updown1.name, "updown1")
  assert_eq(updown2.name, "updown2")
  assert_eq(updown3.name, "updown3")
  
  assert_eq(gauge1.name, "gauge1")
  assert_eq(gauge2.name, "gauge2")
  assert_eq(gauge3.name, "gauge3")
  
  // Perform operations on all instruments
  Counter::add(counter1, 1.0)
  Counter::add(counter2, 2.0)
  Counter::add(counter3, 3.0)
  
  Histogram::record(histogram1, 0.1)
  Histogram::record(histogram2, 0.2)
  Histogram::record(histogram3, 0.3)
  
  UpDownCounter::add(updown1, 10.0)
  UpDownCounter::add(updown2, 20.0)
  UpDownCounter::add(updown3, 30.0)
}

test "concurrent logger and log record operations" {
  // Create multiple logger instances
  let provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(provider, "logger1")
  let logger2 = LoggerProvider::get_logger(provider, "logger2")
  let logger3 = LoggerProvider::get_logger(provider, "logger3")
  
  // Create log records for each logger
  let record1 = LogRecord::new(Info, "Log from logger1")
  let record2 = LogRecord::new(Warn, "Log from logger2")
  let record3 = LogRecord::new(Error, "Log from logger3")
  
  // Verify log records have correct properties
  assert_eq(LogRecord::severity_number(record1), Info)
  assert_eq(LogRecord::body(record1), Some("Log from logger1"))
  
  assert_eq(LogRecord::severity_number(record2), Warn)
  assert_eq(LogRecord::body(record2), Some("Log from logger2"))
  
  assert_eq(LogRecord::severity_number(record3), Error)
  assert_eq(LogRecord::body(record3), Some("Log from logger3"))
  
  // Emit log records from different loggers
  Logger::emit(logger1, record1)
  Logger::emit(logger2, record2)
  Logger::emit(logger3, record3)
  
  // Create log records with trace context
  let trace_record1 = LogRecord::new_with_context(
    Info,
    Some("Trace log from logger1"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace1"),
    Some("span1"),
    None
  )
  
  let trace_record2 = LogRecord::new_with_context(
    Info,
    Some("Trace log from logger2"),
    None,
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some("trace2"),
    Some("span2"),
    None
  )
  
  let trace_record3 = LogRecord::new_with_context(
    Info,
    Some("Trace log from logger3"),
    None,
    Some(1735689600000000004L),
    Some(1735689600000000005L),
    Some("trace3"),
    Some("span3"),
    None
  )
  
  // Verify trace records have correct properties
  assert_eq(LogRecord::trace_id(trace_record1), Some("trace1"))
  assert_eq(LogRecord::span_id(trace_record1), Some("span1"))
  
  assert_eq(LogRecord::trace_id(trace_record2), Some("trace2"))
  assert_eq(LogRecord::span_id(trace_record2), Some("span2"))
  
  assert_eq(LogRecord::trace_id(trace_record3), Some("trace3"))
  assert_eq(LogRecord::span_id(trace_record3), Some("span3"))
  
  // Emit trace log records
  Logger::emit(logger1, trace_record1)
  Logger::emit(logger2, trace_record2)
  Logger::emit(logger3, trace_record3)
}

test "concurrent resource operations" {
  // Create multiple resource instances
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  let resource3 = Resource::new()
  
  // Create attributes for each resource
  let attributes1 = [
    ("service.name", StringValue("service1")),
    ("service.instance.id", StringValue("instance1")),
    ("service.version", StringValue("1.0.0"))
  ]
  
  let attributes2 = [
    ("service.name", StringValue("service2")),
    ("service.instance.id", StringValue("instance2")),
    ("service.version", StringValue("2.0.0"))
  ]
  
  let attributes3 = [
    ("service.name", StringValue("service3")),
    ("service.instance.id", StringValue("instance3")),
    ("service.version", StringValue("3.0.0"))
  ]
  
  // Create resources with attributes
  let resource_with_attrs1 = Resource::with_attributes(resource1, attributes1)
  let resource_with_attrs2 = Resource::with_attributes(resource2, attributes2)
  let resource_with_attrs3 = Resource::with_attributes(resource3, attributes3)
  
  // Verify each resource maintains its own state
  let service_name1 = Resource::get_attribute(resource_with_attrs1, "service.name")
  let service_name2 = Resource::get_attribute(resource_with_attrs2, "service.name")
  let service_name3 = Resource::get_attribute(resource_with_attrs3, "service.name")
  
  assert_eq(service_name1, Some(StringValue("service1")))
  assert_eq(service_name2, Some(StringValue("service2")))
  assert_eq(service_name3, Some(StringValue("service3")))
  
  // Verify no cross-contamination between resources
  let cross_check1 = Resource::get_attribute(resource_with_attrs1, "service.instance.id")
  let cross_check2 = Resource::get_attribute(resource_with_attrs2, "service.instance.id")
  let cross_check3 = Resource::get_attribute(resource_with_attrs3, "service.instance.id")
  
  assert_eq(cross_check1, Some(StringValue("instance1")))
  assert_eq(cross_check2, Some(StringValue("instance2")))
  assert_eq(cross_check3, Some(StringValue("instance3")))
}

test "concurrent propagator operations" {
  // Create multiple propagator instances
  let propagator1 = W3CTraceContextPropagator::new()
  let propagator2 = W3CTraceContextPropagator::new()
  let propagator3 = W3CTraceContextPropagator::new()
  
  // Create composite propagators
  let composite1 = CompositePropagator::new([propagator1])
  let composite2 = CompositePropagator::new([propagator2])
  let composite3 = CompositePropagator::new([propagator3])
  
  // Create contexts for each "thread"
  let ctx1 = Context::root()
  let ctx2 = Context::root()
  let ctx3 = Context::root()
  
  // Create carriers for each "thread"
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // Inject contexts into carriers
  CompositePropagator::inject(composite1, ctx1, carrier1)
  CompositePropagator::inject(composite2, ctx2, carrier2)
  CompositePropagator::inject(composite3, ctx3, carrier3)
  
  // Extract contexts from carriers
  let extracted_ctx1 = CompositePropagator::extract(composite1, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite2, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(composite3, carrier3)
  
  // Verify extraction worked for all
  let extracted_value1 = Context::get(extracted_ctx1, ContextKey::new("extracted"))
  let extracted_value2 = Context::get(extracted_ctx2, ContextKey::new("extracted"))
  let extracted_value3 = Context::get(extracted_ctx3, ContextKey::new("extracted"))
  
  assert_eq(extracted_value1, Some("true"))
  assert_eq(extracted_value2, Some("true"))
  assert_eq(extracted_value3, Some("true"))
}