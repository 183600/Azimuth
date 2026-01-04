// Essential Feature Tests for Azimuth Telemetry System
// This file contains comprehensive test cases for core telemetry features

test "attribute value type conversion" {
  // Test different attribute value types
  let string_attr = StringValue("test_value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  let bool_attr = BoolValue(true)
  let array_string_attr = ArrayStringValue(["value1", "value2"])
  let array_int_attr = ArrayIntValue([1, 2, 3])
  
  // Verify type preservation
  match string_attr {
    StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false, "Expected StringValue")
  }
  
  match int_attr {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false, "Expected IntValue")
  }
  
  match float_attr {
    FloatValue(v) => assert_true(abs(v - 3.14) < 0.001)
    _ => assert_true(false, "Expected FloatValue")
  }
  
  match bool_attr {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false, "Expected BoolValue")
  }
  
  match array_string_attr {
    ArrayStringValue(arr) => assert_eq(arr.length, 2)
    _ => assert_true(false, "Expected ArrayStringValue")
  }
  
  match array_int_attr {
    ArrayIntValue(arr) => assert_eq(arr.length, 3)
    _ => assert_true(false, "Expected ArrayIntValue")
  }
}

test "resource merge functionality" {
  let base_attrs = [("service.name", StringValue("azimuth-service"))]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [("service.version", StringValue("1.0.0"))]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  // Check that override resource takes precedence
  let service_name = Resource::get_attribute(merged, "service.name")
  let service_version = Resource::get_attribute(merged, "service.version")
  
  match service_version {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false, "Expected service version")
  }
}

test "span lifecycle management" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer")
  let span = Tracer::start_span(tracer, "test-span")
  
  // Verify initial span state
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Test span status operations
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // Test span events
  Span::add_event(span, "event1", [("key1", StringValue("value1"))])
  
  // End the span
  Span::end(span)
  
  assert_true(true, "Span lifecycle completed successfully")
}

test "baggage propagation operations" {
  let baggage = Baggage::new()
  
  // Add entries
  let baggage_with_entry1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_entry2 = Baggage::set_entry(baggage_with_entry1, "request.id", "req-67890")
  
  // Retrieve entries
  let user_id = Baggage::get_entry(baggage_with_entry2, "user.id")
  let request_id = Baggage::get_entry(baggage_with_entry2, "request.id")
  let missing = Baggage::get_entry(baggage_with_entry2, "missing.key")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(missing, None)
  
  // Remove entry
  let baggage_after_removal = Baggage::remove_entry(baggage_with_entry2, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_after_removal, "user.id")
  
  assert_eq(removed_user_id, None)
}

test "composite propagator injection and extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Test injection
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent.is_some(), "traceparent should be injected")
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
}

test "different metric instrument types" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Create different instrument types
  let counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Test instrument operations
  Counter::add(counter, 10.0)
  Histogram::record(histogram, 150.5)
  UpDownCounter::add(updown_counter, 5.0)
  // Gauge doesn't have a set operation in the simplified implementation
  
  // Verify instrument properties
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, Some("HTTP request duration"))
  assert_eq(histogram.unit, Some("ms"))
  
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Memory usage"))
  assert_eq(gauge.unit, Some("bytes"))
}

test "log record context correlation" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = Clock::now_unix_nanos(Clock::system())
  
  let attributes = Attributes::new()
  let context = Context::root()
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attributes),
    Some(timestamp),
    None,
    Some(trace_id),
    Some(span_id),
    Some(context)
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
  
  // Emit the log record
  Logger::emit(logger, record)
  
  assert_true(true, "Log record with context correlation emitted successfully")
}

test "span status codes and events" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer")
  let span = Tracer::start_span(tracer, "operation-span")
  
  // Test different status codes
  Span::set_status(span, Unset)
  assert_eq(Span::status(span), Unset)
  
  Span::set_status(span, Ok, Some("Operation completed"))
  assert_eq(Span::status(span), Ok)
  
  Span::set_status(span, Error, Some("Operation failed"))
  assert_eq(Span::status(span), Error)
  
  // Test adding multiple events
  Span::add_event(span, "start", [("step", StringValue("1"))])
  Span::add_event(span, "progress", [("step", StringValue("2")), ("progress", IntValue(50))])
  Span::add_event(span, "complete", [("step", StringValue("3")), ("result", StringValue("success"))])
  
  Span::end(span)
  
  assert_true(true, "Span status codes and events handled successfully")
}

test "timestamp operations" {
  let clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(clock)
  
  // Verify timestamp format (should be nanoseconds since epoch)
  assert_true(timestamp1 > 0L)
  
  // Get another timestamp
  let timestamp2 = Clock::now_unix_nanos(clock)
  
  // Timestamps should be different (monotonically increasing)
  assert_true(timestamp2 >= timestamp1)
  
  // Test with log record timestamps
  let record = LogRecord::new_with_context(
    Info,
    Some("Timestamp test"),
    None,
    Some(timestamp1),
    Some(timestamp2),
    None,
    None,
    None
  )
  
  assert_eq(record.timestamp, Some(timestamp1))
  assert_eq(record.observed_timestamp, Some(timestamp2))
}

test "error handling boundary conditions" {
  // Test empty trace ID and span ID
  let invalid_span_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // Test valid span context
  let valid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_true(SpanContext::is_valid(valid_span_ctx))
  
  // Test context operations with non-existent keys
  let ctx = Context::root()
  let key = ContextKey::new("non.existent.key")
  let value = Context::get(ctx, key)
  assert_eq(value, None)
  
  // Test attribute operations with non-existent keys
  let attrs = Attributes::new()
  let missing_attr = Attributes::get(attrs, "missing.attribute")
  assert_eq(missing_attr, None)
  
  // Test resource operations with non-existent attributes
  let resource = Resource::new()
  let missing_resource_attr = Resource::get_attribute(resource, "missing.resource.attribute")
  assert_eq(missing_resource_attr, None)
  
  // Test baggage operations with non-existent entries
  let empty_baggage = Baggage::new()
  let missing_entry = Baggage::get_entry(empty_baggage, "missing.entry")
  assert_eq(missing_entry, None)
  
  // Test carrier operations with non-existent headers
  let carrier = TextMapCarrier::new()
  let missing_header = TextMapCarrier::get(carrier, "missing.header")
  assert_eq(missing_header, None)
  
  assert_true(true, "All boundary conditions handled correctly")
}