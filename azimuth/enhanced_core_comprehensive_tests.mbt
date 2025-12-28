// Enhanced Core Tests for Azimuth Telemetry System
// Focused test coverage for key telemetry operations

test "resource_attribute_merging" {
  // Test resource attribute merging functionality
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("updated-service")),
    ("host.name", StringValue("localhost")),
    ("process.pid", IntValue(12345))
  ]
  let resource_with_override = Resource::with_attributes(override_resource, override_attrs)
  
  // Test merging resources
  let merged_resource = Resource::merge(resource_with_attrs, resource_with_override)
  
  // Verify override resource takes precedence
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "updated-service")
    _ => @test.fail("Expected service.name to be updated")
  }
  
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  match host_name {
    Some(StringValue(host)) => assert_eq(host, "localhost")
    _ => @test.fail("Expected host.name attribute")
  }
}

test "http_client_request_response_cycle" {
  // Test HTTP client request and response creation
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-456")
  ]
  
  let request = HttpRequest::new(
    "POST",
    "https://api.example.com/telemetry",
    headers,
    Some("{\"metric\": \"counter\", \"value\": 42}")
  )
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/telemetry")
  
  let request_body = HttpRequest::body(request)
  match request_body {
    Some(body) => assert_eq(body, "{\"metric\": \"counter\", \"value\": 42}")
    None => @test.fail("Expected request body")
  }
  
  // Test response creation
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-ID", "resp-789")
  ]
  
  let response = HttpResponse::new(
    200,
    response_headers,
    Some("{\"status\": \"success\", \"processed\": 42}")
  )
  
  assert_eq(HttpResponse::status_code(response), 200)
  
  let response_body = HttpResponse::body(response)
  match response_body {
    Some(body) => assert_eq(body, "{\"status\": \"success\", \"processed\": 42}")
    None => @test.fail("Expected response body")
  }
}

test "log_record_comprehensive_creation" {
  // Test comprehensive log record creation with all fields
  let attributes = Attributes::new()
  let log_attrs = [
    ("user.id", IntValue(12345)),
    ("action.type", StringValue("api_call")),
    ("response.time", FloatValue(125.5))
  ]
  let log_attributes = Attributes::new() // Simplified for test
  
  let log_record = LogRecord::new_with_context(
    SeverityNumber::Warn,
    Some("API call exceeded expected duration"),
    Some(log_attributes),
    Some(1735689600000000000L), // timestamp
    Some(1735689600000000100L), // observed timestamp
    Some("trace-123-456"),
    Some("span-789-012"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(log_record), SeverityNumber::Warn)
  
  let body = LogRecord::body(log_record)
  match body {
    Some(message) => assert_eq(message, "API call exceeded expected duration")
    None => @test.fail("Expected log body")
  }
  
  let trace_id = LogRecord::trace_id(log_record)
  match trace_id {
    Some(tid) => assert_eq(tid, "trace-123-456")
    None => @test.fail("Expected trace ID")
  }
  
  let span_id = LogRecord::span_id(log_record)
  match span_id {
    Some(sid) => assert_eq(sid, "span-789-012")
    None => @test.fail("Expected span ID")
  }
}

test "composite_propagator_inject_extract" {
  // Test composite propagator injection and extraction
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let root_ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite_propagator, root_ctx, carrier)
  
  // Verify injection by checking carrier (simplified test)
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  match injected_traceparent {
    Some(traceparent) => assert_eq(traceparent, "00-test-trace-id-test-span-id-01")
    None => @test.fail("Expected traceparent to be injected")
  }
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    None => @test.fail("Expected extracted context value")
  }
}

test "meter_multiple_instrument_types" {
  // Test creating different types of instruments with a meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Test counter creation
  let counter = Meter::create_counter(meter, "request_count", Some("Total number of requests"), Some("requests"))
  assert_eq(counter.name, "request_count")
  match counter.description {
    Some(desc) => assert_eq(desc, "Total number of requests")
    None => @test.fail("Expected counter description")
  }
  match counter.unit {
    Some(unit) => assert_eq(unit, "requests")
    None => @test.fail("Expected counter unit")
  }
  
  // Test histogram creation
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time distribution"), Some("ms"))
  assert_eq(histogram.name, "response_time")
  
  // Test up-down counter creation
  let updown_counter = Meter::create_updown_counter(meter, "active_connections", Some("Currently active connections"), Some("connections"))
  assert_eq(updown_counter.name, "active_connections")
  
  // Test gauge creation
  let gauge = Meter::create_gauge(meter, "memory_usage", Some("Current memory usage"), Some("bytes"))
  assert_eq(gauge.name, "memory_usage")
  
  // Test instrument operations (simplified)
  let test_attrs = Attributes::new()
  Counter::add(counter, 1.0, Some(test_attrs))
  Histogram::record(histogram, 125.5, Some(test_attrs))
  UpDownCounter::add(updown_counter, 5.0, Some(test_attrs))
}

test "span_events_and_status_management" {
  // Test span event addition and status management
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer", Some("1.0.0"))
  
  let span = Tracer::start_span(tracer, "test-operation")
  assert_eq(Span::name(span), "test-operation")
  assert_eq(Span::kind(span), Internal)
  assert_eq(Span::is_recording(span), true)
  
  // Test adding events to span
  let event_attrs = [
    ("event.type", StringValue("error")),
    ("error.code", IntValue(500)),
    ("error.message", StringValue("Internal server error"))
  ]
  Span::add_event(span, "operation_failed", Some(event_attrs))
  
  // Test setting span status
  Span::set_status(span, StatusCode::Error, Some("Operation failed due to timeout"))
  
  // Test span context
  let span_ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(span_ctx), "test_trace_id")
  assert_eq(SpanContext::span_id(span_ctx), "test_span_id")
  assert_eq(SpanContext::is_sampled(span_ctx), true)
  
  // Test ending span
  Span::end(span)
}

test "baggage_entry_management" {
  // Test baggage entry management operations
  let baggage = Baggage::new()
  
  // Test setting entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let final_baggage = Baggage::set_entry(updated_baggage, "session.id", "sess-67890")
  
  // Test getting entries
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  match user_id {
    Some(id) => assert_eq(id, "12345")
    None => @test.fail("Expected user.id in baggage")
  }
  
  let session_id = Baggage::get_entry(final_baggage, "session.id")
  match session_id {
    Some(id) => assert_eq(id, "sess-67890")
    None => @test.fail("Expected session.id in baggage")
  }
  
  // Test non-existent entry
  let missing_entry = Baggage::get_entry(final_baggage, "non.existent")
  match missing_entry {
    Some(_) => @test.fail("Expected None for non-existent entry")
    None => () // Expected
  }
  
  // Test removing entries
  let baggage_after_removal = Baggage::remove_entry(final_baggage, "user.id")
  let removed_entry = Baggage::get_entry(baggage_after_removal, "user.id")
  match removed_entry {
    Some(_) => @test.fail("Expected None after removal")
    None => () // Expected
  }
  
  // Verify other entries remain
  let remaining_session = Baggage::get_entry(baggage_after_removal, "session.id")
  match remaining_session {
    Some(id) => assert_eq(id, "sess-67890")
    None => @test.fail("Expected session.id to remain after removal")
  }
}

test "text_map_carrier_header_operations" {
  // Test TextMapCarrier header operations
  let carrier = TextMapCarrier::new()
  
  // Test setting multiple headers
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "user=alice,session=12345")
  TextMapCarrier::set(carrier, "x-request-id", "req-abcdef")
  TextMapCarrier::set(carrier, "custom-header", "custom-value")
  
  // Test getting headers
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  match traceparent {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => @test.fail("Expected traceparent header")
  }
  
  let baggage = TextMapCarrier::get(carrier, "baggage")
  match baggage {
    Some(value) => assert_eq(value, "user=alice,session=12345")
    None => @test.fail("Expected baggage header")
  }
  
  let request_id = TextMapCarrier::get(carrier, "x-request-id")
  match request_id {
    Some(value) => assert_eq(value, "req-abcdef")
    None => @test.fail("Expected x-request-id header")
  }
  
  let custom_header = TextMapCarrier::get(carrier, "custom-header")
  match custom_header {
    Some(value) => assert_eq(value, "custom-value")
    None => @test.fail("Expected custom-header")
  }
  
  // Test non-existent header
  let missing_header = TextMapCarrier::get(carrier, "non-existent")
  match missing_header {
    Some(_) => @test.fail("Expected None for non-existent header")
    None => () // Expected
  }
}

test "instrumentation_scope_validation" {
  // Test instrumentation scope information validation
  let meter_provider = MeterProvider::noop()
  let meter = MeterProvider::get_meter(meter_provider, "test.instrumentation", Some("1.2.3"), Some("https://example.com/schema"))
  
  let scope = meter.scope
  assert_eq(scope.name, "test.instrumentation")
  
  match scope.version {
    Some(version) => assert_eq(version, "1.2.3")
    None => @test.fail("Expected instrumentation scope version")
  }
  
  match scope.schema_url {
    Some(schema) => assert_eq(schema, "https://example.com/schema")
    None => @test.fail("Expected instrumentation scope schema URL")
  }
  
  // Test logger provider with scope
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger", Some("2.0.0"))
  
  let logger_scope = logger.scope
  assert_eq(logger_scope.name, "test.logger")
  
  match logger_scope.version {
    Some(version) => assert_eq(version, "2.0.0")
    None => @test.fail("Expected logger scope version")
  }
  
  match logger_scope.schema_url {
    Some(_) => @test.fail("Expected no schema URL for logger")
    None => () // Expected
  }
  
  // Test tracer with scope
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test.tracer")
  
  let tracer_scope = Tracer::instrumentation_scope(tracer)
  assert_eq(tracer_scope.name, "test.tracer")
  
  match tracer_scope.version {
    Some(_) => @test.fail("Expected no version for tracer")
    None => () // Expected
  }
}

test "attributes_deep_nesting_operations" {
  // Test attributes with complex nested operations
  let attrs = Attributes::new()
  
  // Test setting various attribute types
  Attributes::set(attrs, "string.key", StringValue("test_string_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14159))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([10, 20, 30, 40]))
  
  // Test getting string attribute
  let string_attr = Attributes::get(attrs, "string.key")
  match string_attr {
    Some(StringValue(value)) => assert_eq(value, "test_string_value")
    _ => @test.fail("Expected StringValue for string.key")
  }
  
  // Test getting int attribute
  let int_attr = Attributes::get(attrs, "int.key")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => @test.fail("Expected IntValue for int.key")
  }
  
  // Test getting float attribute
  let float_attr = Attributes::get(attrs, "float.key")
  match float_attr {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => @test.fail("Expected FloatValue for float.key")
  }
  
  // Test getting boolean attribute
  let bool_attr = Attributes::get(attrs, "bool.key")
  match bool_attr {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => @test.fail("Expected BoolValue for bool.key")
  }
  
  // Test getting non-existent attribute
  let missing_attr = Attributes::get(attrs, "non.existent.key")
  match missing_attr {
    Some(_) => @test.fail("Expected None for non-existent attribute")
    None => () // Expected
  }
  
  // Test attribute value pattern matching
  let complex_attr = Attributes::get(attrs, "array.string.key")
  match complex_attr {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => @test.fail("Expected ArrayStringValue for array.string.key")
  }
}