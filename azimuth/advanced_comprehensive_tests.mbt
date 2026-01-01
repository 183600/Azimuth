// Advanced Comprehensive Test Suite for Azimuth Telemetry System
// This file contains advanced test cases covering complex scenarios and edge cases

test "complex attribute operations with type conversion" {
  let attrs = Attributes::new()
  
  // Test setting and getting different attribute types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["value1", "value2"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([1, 2, 3]))
  
  // Verify retrieval of different types
  let string_val = Attributes::get(attrs, "string.key")
  let int_val = Attributes::get(attrs, "int.key")
  let float_val = Attributes::get(attrs, "float.key")
  let bool_val = Attributes::get(attrs, "bool.key")
  let array_string_val = Attributes::get(attrs, "array.string.key")
  let array_int_val = Attributes::get(attrs, "array.int.key")
  
  assert_eq(string_val, Some(StringValue("test_value")))
  assert_eq(int_val, Some(IntValue(42)))
  assert_eq(float_val, Some(FloatValue(3.14)))
  assert_eq(bool_val, Some(BoolValue(true)))
  assert_eq(array_string_val, Some(ArrayStringValue(["value1", "value2"])))
  assert_eq(array_int_val, Some(ArrayIntValue([1, 2, 3])))
  
  // Test missing attribute
  let missing_val = Attributes::get(attrs, "missing.key")
  assert_eq(missing_val, None)
}

test "resource management and merge strategies" {
  let base_attrs = [("service.name", StringValue("azimuth-service"))]
  let override_attrs = [("service.version", StringValue("1.0.0")), ("service.name", StringValue("updated-service"))]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Test resource attribute retrieval
  let service_name = Resource::get_attribute(base_resource, "service.name")
  let service_version = Resource::get_attribute(override_resource, "service.version")
  
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  
  // Test resource merge
  let merged_resource = Resource::merge(base_resource, override_resource)
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  let merged_service_version = Resource::get_attribute(merged_resource, "service.version")
  
  assert_eq(merged_service_name, Some(StringValue("updated-service")))
  assert_eq(merged_service_version, Some(StringValue("1.0.0")))
}

test "composite propagator injection and extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Verify injection by checking carrier
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_traceparent.is_some())
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  
  assert_eq(extracted_value, Some("true"))
}

test "comprehensive log record with full context" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("info"))
  Attributes::set(attrs, "log.module", StringValue("azimuth.core"))
  
  let span_ctx = SpanContext::new("trace123", "span456", true, "key1=value1")
  let log_ctx = Context::with_value(Context::root(), ContextKey::new("user.id"), "user123")
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Warning: Resource limit approaching"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123"),
    Some("span456"),
    Some(log_ctx)
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(record), Warn)
  assert_eq(LogRecord::body(record), Some("Warning: Resource limit approaching"))
  assert_eq(LogRecord::trace_id(record), Some("trace123"))
  assert_eq(LogRecord::span_id(record), Some("span456"))
  
  // Test logger emission
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "azimuth.logger")
  Logger::emit(logger, record)
  
  assert_true(true) // If we reach here, emission was successful
}

test "span lifecycle management with events and status" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "azimuth.tracer", Some("1.0.0"))
  
  let span_attrs = [("operation.type", StringValue("database"))]
  let span = Tracer::start_span(tracer, "database.query", Some(span_attrs))
  
  // Verify span properties
  assert_eq(Span::name(span), "database.query")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Add events to span
  Span::add_event(span, "query.start", Some([("query.sql", StringValue("SELECT * FROM users"))]))
  Span::add_event(span, "query.complete", Some([("rows.affected", IntValue(42))]))
  
  // Set span status
  Span::set_status(span, Ok, Some("Query completed successfully"))
  
  // End span
  Span::end(span)
  
  // Verify tracer instrumentation scope
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "azimuth.tracer")
  assert_eq(scope.version, Some("1.0.0"))
}

test "comprehensive metrics operations with all instruments" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "azimuth.metrics")
  
  // Create all instrument types
  let counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Test instrument properties
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  // Test instrument as Instrument enum
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::description(counter_instrument), Some("Total HTTP requests"))
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
  
  // Test recording metrics
  let attrs = Attributes::new()
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  
  Counter::add(counter, 1.0, Some(attrs))
  Histogram::record(histogram, 150.5, Some(attrs))
  UpDownCounter::add(updown_counter, 1.0, Some(attrs))
  Gauge::create_gauge(meter, "cpu.usage", Some("CPU usage"), Some("percent"))
  
  assert_true(true) // If we reach here, all operations were successful
}

test "baggage operations with cross-context propagation" {
  let initial_baggage = Baggage::new()
  
  // Set baggage entries
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user.id", "user123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session456")
  let baggage_with_request = Baggage::set_entry(baggage_with_session, "request.id", "request789")
  
  // Get baggage entries
  let user_id = Baggage::get_entry(baggage_with_request, "user.id")
  let session_id = Baggage::get_entry(baggage_with_request, "session.id")
  let request_id = Baggage::get_entry(baggage_with_request, "request.id")
  
  assert_eq(user_id, Some("user123"))
  assert_eq(session_id, Some("session456"))
  assert_eq(request_id, Some("request789"))
  
  // Test missing entry
  let missing_entry = Baggage::get_entry(baggage_with_request, "missing.key")
  assert_eq(missing_entry, None)
  
  // Test entry removal
  let baggage_without_user = Baggage::remove_entry(baggage_with_request, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_without_user, "user.id")
  
  assert_eq(removed_user_id, None)
  
  // Verify other entries still exist
  let remaining_session_id = Baggage::get_entry(baggage_without_user, "session.id")
  let remaining_request_id = Baggage::get_entry(baggage_without_user, "request.id")
  
  assert_eq(remaining_session_id, Some("session456"))
  assert_eq(remaining_request_id, Some("request789"))
}

test "error handling and boundary conditions" {
  // Test empty span context
  let empty_span_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
  
  // Test valid span context
  let valid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1")
  assert_true(SpanContext::is_valid(valid_span_ctx))
  assert_true(SpanContext::is_sampled(valid_span_ctx))
  
  // Test context with no data
  let empty_ctx = Context::root()
  let empty_key = ContextKey::new("nonexistent.key")
  let empty_value = Context::get(empty_ctx, empty_key)
  assert_eq(empty_value, None)
  
  // Test HTTP operations with edge cases
  let empty_headers = [] : Array[(String, String)]
  let empty_request = HttpRequest::new("", "", empty_headers, Some(""))
  assert_eq(HttpRequest::http_method(empty_request), "")
  assert_eq(HttpRequest::url(empty_request), "")
  assert_eq(HttpRequest::body(empty_request), Some(""))
  
  // Test response with edge case status codes
  let error_response = HttpResponse::new(500, empty_headers, Some("Internal Server Error"))
  assert_eq(HttpResponse::status_code(error_response), 500)
  assert_eq(HttpResponse::body(error_response), Some("Internal Server Error"))
  
  // Test clock operations
  let clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(clock)
  let timestamp2 = Clock::now_unix_nanos(clock)
  
  assert_true(timestamp1 > 0L)
  assert_true(timestamp2 >= timestamp1) // Monotonic or nearly monotonic
  
  // Test random operations
  let random = Random::system()
  let random_bytes = Random::next_bytes(random, 0)
  assert_eq(random_bytes.length(), 0)
  
  let random_u64 = Random::next_u64(random)
  assert_true(random_u64 >= 0UL)
}