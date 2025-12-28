// Additional MoonBit Test Cases for Azimuth Telemetry System
// This file contains focused test cases for specific telemetry functionality

test "attributes operations with different value types" {
  let attrs = Attributes::new()
  
  // Test setting different attribute value types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  // Test getting string attribute
  let string_value = Attributes::get(attrs, "string.key")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // Test getting int attribute
  let int_value = Attributes::get(attrs, "int.key")
  assert_eq(int_value, Some(IntValue(42)))
  
  // Test getting non-existent attribute
  let missing_value = Attributes::get(attrs, "missing.key")
  assert_eq(missing_value, None)
}

test "baggage operations with multiple entries" {
  let baggage = Baggage::new()
  
  // Test setting baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "request.id", "req-67890")
  let baggage3 = Baggage::set_entry(baggage2, "session.id", "sess-abcde")
  
  // Test getting baggage entries
  let user_id = Baggage::get_entry(baggage3, "user.id")
  let request_id = Baggage::get_entry(baggage3, "request.id")
  let session_id = Baggage::get_entry(baggage3, "session.id")
  let missing = Baggage::get_entry(baggage3, "missing.key")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(session_id, Some("sess-abcde"))
  assert_eq(missing, None)
  
  // Test removing baggage entries
  let baggage4 = Baggage::remove_entry(baggage3, "user.id")
  let removed_user_id = Baggage::get_entry(baggage4, "user.id")
  assert_eq(removed_user_id, None)
}

test "span operations with status and events" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-span", Server, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  assert_eq(SpanContext::trace_id(Span::span_context(span)), "trace123")
  assert_eq(SpanContext::span_id(Span::span_context(span)), "span456")
  
  // Test span status operations
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // Test adding events
  Span::add_event(span, "database.query", Some([("db.statement", StringValue("SELECT * FROM users"))]))
  Span::add_event(span, "cache.hit", Some([("cache.key", StringValue("user:12345"))]))
  
  // Test ending span
  Span::end(span)
  assert_true(true)
}

test "tracer operations with different span kinds" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  
  // Test tracer scope
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  // Test creating spans with different kinds
  let internal_span = Tracer::start_span(tracer, "internal-operation", Some([("operation.type", StringValue("validation"))]))
  assert_eq(Span::kind(internal_span), Internal)
  
  let client_span = Tracer::start_span(tracer, "client-request", Some([("http.method", StringValue("GET"))]))
  assert_eq(Span::name(client_span), "client-request")
  
  let server_span = Tracer::start_span(tracer, "server-request", Some([("http.method", StringValue("POST"))]))
  assert_eq(Span::name(server_span), "server-request")
  
  assert_true(true)
}

test "histogram and updown counter metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-test")
  
  // Test histogram creation and operations
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("Request duration"), Some("ms"))
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, Some("Request duration"))
  assert_eq(histogram.unit, Some("ms"))
  
  Histogram::record(histogram, 150.5)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 75.25)
  
  // Test updown counter creation and operations
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("count"))
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Active connections"))
  assert_eq(updown_counter.unit, Some("count"))
  
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -3.0)
  UpDownCounter::add(updown_counter, 5.0)
  
  assert_true(true)
}

test "composite propagator injection and extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Create composite propagator (simplified - only using trace propagator)
  let composite = CompositePropagator::new([trace_propagator])
  
  // Test injection
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify injection worked
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent != None)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "logger operations with different severity levels" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test.logger")
  
  // Test logger scope
  let scope = logger.scope
  assert_eq(scope.name, "test.logger")
  
  // Test creating log records with different severity levels
  let trace_record = LogRecord::new(Trace, "Debug trace information")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Information message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal error message")
  
  // Test severity levels
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Test emission
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  
  assert_true(true)
}

test "resource merge operations" {
  let base_resource = Resource::new()
  let base_with_attrs = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  let override_resource = Resource::new()
  let override_with_attrs = Resource::with_attributes(override_resource, [
    ("service.name", StringValue("override-service")),
    ("environment", StringValue("production"))
  ])
  
  // Test resource merging
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Test getting attributes from merged resource
  let service_name = Resource::get_attribute(merged, "service.name")
  let service_version = Resource::get_attribute(merged, "service.version")
  let environment = Resource::get_attribute(merged, "environment")
  
  assert_eq(service_name, Some(StringValue("override-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(environment, Some(StringValue("production")))
}

test "attribute value type conversions" {
  let string_attr = StringValue("test")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14159)
  let bool_attr = BoolValue(true)
  
  // Test creating array attributes
  let string_array_attr = ArrayStringValue(["a", "b", "c"])
  let int_array_attr = ArrayIntValue([1, 2, 3])
  
  // Test attribute value patterns
  match string_attr {
    StringValue(s) => assert_eq(s, "test")
    _ => assert_true(false)
  }
  
  match int_attr {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  match float_attr {
    FloatValue(f) => assert_true(f > 3.0 && f < 4.0)
    _ => assert_true(false)
  }
  
  match bool_attr {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  match string_array_attr {
    ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => assert_true(false)
  }
  
  match int_array_attr {
    ArrayIntValue(arr) => assert_eq(arr.length(), 3)
    _ => assert_true(false)
  }
}

test "context chain operations" {
  // Test building a context chain with multiple values
  let ctx1 = Context::root()
  let key1 = ContextKey::new("trace.id")
  let ctx2 = Context::with_value(ctx1, key1, "trace-12345")
  
  let key2 = ContextKey::new("user.id")
  let ctx3 = Context::with_value(ctx2, key2, "user-67890")
  
  let key3 = ContextKey::new("request.id")
  let ctx4 = Context::with_value(ctx3, key3, "req-abcde")
  
  // Test that all values are accessible
  let trace_id = Context::get(ctx4, key1)
  let user_id = Context::get(ctx4, key2)
  let request_id = Context::get(ctx4, key3)
  
  assert_eq(trace_id, Some("trace-12345"))
  assert_eq(user_id, Some("user-67890"))
  assert_eq(request_id, Some("req-abcde"))
  
  // Test that missing keys return None
  let missing = Context::get(ctx4, ContextKey::new("missing.key"))
  assert_eq(missing, None)
  
  // Test that original context is unchanged
  let original_value = Context::get(ctx1, key1)
  assert_eq(original_value, None)
}