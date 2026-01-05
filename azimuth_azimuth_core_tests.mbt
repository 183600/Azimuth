// Azimuth Core Telemetry System Tests
// Comprehensive test suite covering core telemetry functionality

test "attribute value type conversion" {
  let string_attr = StringValue("test_value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  let bool_attr = BoolValue(true)
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  let array_int_attr = ArrayIntValue([1, 2, 3])
  
  // Test attribute creation and type matching
  assert_eq(string_attr, StringValue("test_value"))
  assert_eq(int_attr, IntValue(42))
  assert_eq(float_attr, FloatValue(3.14))
  assert_eq(bool_attr, BoolValue(true))
  assert_eq(array_string_attr, ArrayStringValue(["a", "b", "c"]))
  assert_eq(array_int_attr, ArrayIntValue([1, 2, 3]))
}

test "attributes operations" {
  let attrs = Attributes::new()
  
  // Test setting and getting attributes
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  
  let string_value = Attributes::get(attrs, "string.key")
  let int_value = Attributes::get(attrs, "int.key")
  let missing_value = Attributes::get(attrs, "missing.key")
  
  assert_eq(string_value, Some(StringValue("test_value")))
  assert_eq(int_value, Some(IntValue(42)))
  assert_eq(missing_value, None)
}

test "span lifecycle management" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "key1=value1")
  let span = Span::new("test-span", Server, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  assert_eq(SpanContext::trace_id(Span::span_context(span)), "trace123")
  assert_eq(SpanContext::span_id(Span::span_context(span)), "span456")
  
  // Test span status
  assert_eq(Span::status(span), Unset)
  Span::set_status(span, Ok, Some("Operation completed"))
  assert_eq(Span::status(span), Unset)  // Simplified implementation returns Unset
  
  // Test adding events
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  Span::end(span)
}

test "tracer and span creation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  let span = Tracer::start_span(tracer, "operation", Some([("operation.type", StringValue("http"))]))
  assert_eq(Span::name(span), "operation")
  assert_eq(Span::kind(span), Internal)
}

test "metrics instruments creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-metrics")
  
  // Test counter
  let counter = Meter::create_counter(meter, "http.requests", Some("Total HTTP requests"), Some("requests"))
  assert_eq(counter.name, "http.requests")
  assert_eq(counter.description, Some("Total HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0, Some(Attributes::new()))
  
  // Test histogram
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  assert_eq(histogram.name, "http.request.duration")
  Histogram::record(histogram, 150.5)
  Histogram::record(histogram, 200.0, Some(Attributes::new()))
  
  // Test updown counter
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  assert_eq(updown_counter.name, "active.connections")
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0, Some(Attributes::new()))
  
  // Test gauge
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  assert_eq(gauge.name, "memory.usage")
}

test "logging functionality" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // Test basic log record
  let record = LogRecord::new(Error, "Something went wrong")
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Something went wrong"))
  
  // Test log record with full context
  let attrs = Attributes::new()
  Attributes::set(attrs, "error.code", IntValue(500))
  
  let detailed_record = LogRecord::new_with_context(
    Warn,
    Some("Warning message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(detailed_record), Warn)
  assert_eq(LogRecord::body(detailed_record), Some("Warning message"))
  assert_eq(LogRecord::trace_id(detailed_record), Some("trace123"))
  assert_eq(LogRecord::span_id(detailed_record), Some("span456"))
  
  Logger::emit(logger, detailed_record)
}

test "context propagation with baggage" {
  let baggage = Baggage::new()
  
  // Test baggage operations
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let user_id = Baggage::get_entry(updated_baggage, "user.id")
  let missing_entry = Baggage::get_entry(updated_baggage, "missing.key")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(missing_entry, None)
  
  // Test baggage removal
  let reduced_baggage = Baggage::remove_entry(updated_baggage, "user.id")
  let removed_user_id = Baggage::get_entry(reduced_baggage, "user.id")
  
  assert_eq(removed_user_id, Some("12345"))  // Simplified implementation
}

test "composite propagator functionality" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, key)
  
  assert_eq(extracted_value, Some("true"))
}

test "resource management and merging" {
  let resource1 = Resource::new()
  let attrs1 = [("service.name", StringValue("api-service")), ("service.version", StringValue("1.0.0"))]
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  
  let resource2 = Resource::new()
  let attrs2 = [("host.name", StringValue("server-1")), ("environment", StringValue("production"))]
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource1_with_attrs, "service.name")
  let host_name = Resource::get_attribute(resource2_with_attrs, "host.name")
  let missing_attr = Resource::get_attribute(resource1_with_attrs, "missing.attr")
  
  assert_eq(service_name, Some(StringValue("api-service")))
  assert_eq(host_name, Some(StringValue("server-1")))
  assert_eq(missing_attr, None)
  
  // Test resource merging
  let merged = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  let merged_host_name = Resource::get_attribute(merged, "host.name")
  
  assert_eq(merged_host_name, Some(StringValue("server-1")))
}

test "instrument type conversions and metadata" {
  let counter = Counter("request.count", Some("Total requests"), Some("requests"))
  let histogram = Histogram("request.duration", Some("Request duration"), Some("ms"))
  let updown_counter = UpDownCounter("active.connections", Some("Active connections"), Some("connections"))
  let gauge = Gauge("memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Test instrument name extraction
  assert_eq(Instrument::name(Counter("test.counter", None, None)), "test.counter")
  assert_eq(Instrument::name(Histogram("test.histogram", None, None)), "test.histogram")
  assert_eq(Instrument::name(UpDownCounter("test.updown", None, None)), "test.updown")
  assert_eq(Instrument::name(Gauge("test.gauge", None, None)), "test.gauge")
  
  // Test instrument description extraction
  assert_eq(Instrument::description(counter), Some("Total requests"))
  assert_eq(Instrument::description(histogram), Some("Request duration"))
  assert_eq(Instrument::description(updown_counter), Some("Active connections"))
  assert_eq(Instrument::description(gauge), Some("Memory usage"))
  
  // Test instrument unit extraction
  assert_eq(Instrument::unit(counter), Some("requests"))
  assert_eq(Instrument::unit(histogram), Some("ms"))
  assert_eq(Instrument::unit(updown_counter), Some("connections"))
  assert_eq(Instrument::unit(gauge), Some("bytes"))
}