// Focused Test Suite for Azimuth Telemetry System
// This file contains targeted test cases covering key functionality

test "attributes operations" {
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  let string_val = Attributes::get(attrs, "string.key")
  let int_val = Attributes::get(attrs, "int.key")
  let missing_val = Attributes::get(attrs, "missing.key")
  
  assert_eq(string_val, Some(StringValue("test_value")))
  assert_eq(int_val, Some(IntValue(42)))
  assert_eq(missing_val, None)
}

test "resource management" {
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attr")
  
  assert_eq(service_name, Some(StringValue("test-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(missing_attr, None)
}

test "baggage operations" {
  let baggage = Baggage::new()
  
  let updated_baggage = Baggage::set_entry(baggage, "key1", "value1")
  let final_baggage = Baggage::set_entry(updated_baggage, "key2", "value2")
  
  let value1 = Baggage::get_entry(final_baggage, "key1")
  let value2 = Baggage::get_entry(final_baggage, "key2")
  let missing_value = Baggage::get_entry(final_baggage, "missing.key")
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
  assert_eq(missing_value, None)
}

test "span lifecycle" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("test-span", Server, span_ctx)
  
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_ctx)
  
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "event1", [("event.data", StringValue("test"))])
  Span::end(span)
}

test "tracer operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  let span = Tracer::start_span(tracer, "operation")
  assert_eq(Span::name(span), "operation")
  assert_eq(Span::kind(span), Internal)
}

test "histogram metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-meter")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time in ms"), Some("ms"))
  
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time in ms"))
  assert_eq(histogram.unit, Some("ms"))
  
  Histogram::record(histogram, 100.5)
  Histogram::record(histogram, 200.3)
  Histogram::record(histogram, 150.7)
  
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "response.time")
  assert_eq(Instrument::description(instrument), Some("Response time in ms"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
}

test "composite propagator" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  let composite = CompositePropagator::new([trace_propagator])
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, key)
  
  assert_eq(extracted_value, Some("true"))
}

test "enhanced log records" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("INFO"))
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Enhanced log message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Enhanced log message"))
  assert_eq(LogRecord::trace_id(record), Some("trace-123"))
  assert_eq(LogRecord::span_id(record), Some("span-456"))
}

test "updown counter metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "counter-meter")
  let counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  
  assert_eq(counter.name, "active.connections")
  assert_eq(counter.description, Some("Active connections"))
  assert_eq(counter.unit, Some("connections"))
  
  UpDownCounter::add(counter, 5.0)
  UpDownCounter::add(counter, -2.0)
  UpDownCounter::add(counter, 3.0)
}

test "gauge metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge-meter")
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage percentage"), Some("%"))
  
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Memory usage percentage"))
  assert_eq(gauge.unit, Some("%"))
}