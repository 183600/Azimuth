// Advanced Telemetry Test Suite for Azimuth Telemetry System
// This file contains advanced test cases focusing on complex scenarios and edge cases

// Import the azimuth module
use "azimuth"

// Test 1: Complex Distributed Tracing Scenarios
test "complex distributed tracing scenarios" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "azimuth.advanced.tracer")
  
  // Create a parent span
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  let parent_context = Span::span_context(parent_span)
  
  // Create child spans with different operations
  let child_span1 = Tracer::start_span(tracer, "child-operation-1")
  let child_span2 = Tracer::start_span(tracer, "child-operation-2")
  
  // Add events to parent span
  Span::add_event(parent_span, "operation.started", Some([
    ("operation.type", StringValue("complex_workflow")),
    ("timestamp", IntValue(1634567890)),
    ("user.id", StringValue("user123"))
  ]))
  
  // Add events to child spans
  Span::add_event(child_span1, "database.query", Some([
    ("query.type", StringValue("SELECT")),
    ("table.name", StringValue("users")),
    ("execution.time", IntValue(150))
  ]))
  
  Span::add_event(child_span2, "api.call", Some([
    ("endpoint", StringValue("/api/v1/process")),
    ("method", StringValue("POST")),
    ("response.code", IntValue(200))
  ]))
  
  // Set status on spans
  Span::set_status(child_span1, Ok, Some("Query completed successfully"))
  Span::set_status(child_span2, Error, Some("API call failed with timeout"))
  
  // End spans in reverse order (child first, then parent)
  Span::end(child_span1)
  Span::end(child_span2)
  Span::end(parent_span)
  
  // Verify span contexts
  assert_true(SpanContext::is_valid(parent_context))
  assert_true(SpanContext::is_sampled(parent_context))
  assert_eq(SpanContext::trace_id(parent_context), "test_trace_id")
}

// Test 2: Advanced Metrics Aggregation
test "advanced metrics aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "azimuth.advanced.metrics")
  
  // Create multiple instruments with different attributes
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_histogram = Meter::create_histogram(meter, "http.response.duration")
  let error_counter = Meter::create_counter(meter, "http.errors.total")
  let active_connections = Meter::create_updown_counter(meter, "http.active.connections")
  
  // Create attributes for different scenarios
  let success_attrs = Attributes::new()
  Attributes::set(success_attrs, "status.code", IntValue(200))
  Attributes::set(success_attrs, "endpoint", StringValue("/api/users"))
  
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "status.code", IntValue(500))
  Attributes::set(error_attrs, "endpoint", StringValue("/api/orders"))
  
  let timeout_attrs = Attributes::new()
  Attributes::set(timeout_attrs, "status.code", IntValue(504))
  Attributes::set(timeout_attrs, "endpoint", StringValue("/api/process"))
  
  // Simulate metric recordings
  Counter::add(request_counter, 1000.0, Some(success_attrs))
  Counter::add(request_counter, 50.0, Some(error_attrs))
  Counter::add(request_counter, 25.0, Some(timeout_attrs))
  
  Histogram::record(response_histogram, 100.5, Some(success_attrs))
  Histogram::record(response_histogram, 150.7, Some(success_attrs))
  Histogram::record(response_histogram, 200.3, Some(error_attrs))
  Histogram::record(response_histogram, 5000.0, Some(timeout_attrs))
  
  Counter::add(error_counter, 50.0, Some(error_attrs))
  Counter::add(error_counter, 25.0, Some(timeout_attrs))
  
  UpDownCounter::add(active_connections, 100.0)
  UpDownCounter::add(active_connections, -10.0)
  UpDownCounter::add(active_connections, 5.0)
  
  // Verify instrument properties
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(error_counter.name, "http.errors.total")
  assert_eq(active_connections.name, "http.active.connections")
}

// Test 3: Complex Logging with Correlation
test "complex logging with correlation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "azimuth.advanced.logger")
  
  // Create a trace context for correlation
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  // Create correlated log records
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Request processing started"),
    Some(Attributes::new()),
    Some(1634567890000000000L),
    Some(1634567890000001000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  let process_log = LogRecord::new_with_context(
    Debug,
    Some("Processing business logic"),
    Some(Attributes::new()),
    Some(1634567890000002000L),
    Some(1634567890000003000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(1634567890000004000L),
    Some(1634567890000005000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  let end_log = LogRecord::new_with_context(
    Info,
    Some("Request processing completed"),
    Some(Attributes::new()),
    Some(1634567890000006000L),
    Some(1634567890000007000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Verify log correlation
  assert_eq(LogRecord::trace_id(start_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(process_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(error_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(end_log), Some(trace_id))
  
  assert_eq(LogRecord::span_id(start_log), Some(span_id))
  assert_eq(LogRecord::span_id(process_log), Some(span_id))
  assert_eq(LogRecord::span_id(error_log), Some(span_id))
  assert_eq(LogRecord::span_id(end_log), Some(span_id))
  
  // Emit all log records
  Logger::emit(logger, start_log)
  Logger::emit(logger, process_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, end_log)
}

// Test 4: Advanced Context Propagation
test "advanced context propagation" {
  // Create a complex context chain
  let root_ctx = Context::root()
  
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let request_key = ContextKey::new("request.id")
  let trace_key = ContextKey::new("trace.id")
  
  // Build context chain
  let ctx1 = Context::with_value(root_ctx, user_key, "user123")
  let ctx2 = Context::with_value(ctx1, session_key, "session456")
  let ctx3 = Context::with_value(ctx2, request_key, "request789")
  let ctx4 = Context::with_value(ctx3, trace_key, "trace012")
  
  // Verify all context values are accessible
  assert_eq(Context::get(ctx4, user_key), Some("user123"))
  assert_eq(Context::get(ctx4, session_key), Some("session456"))
  assert_eq(Context::get(ctx4, request_key), Some("request789"))
  assert_eq(Context::get(ctx4, trace_key), Some("trace012"))
  
  // Test context propagation through carriers
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  
  // Inject context
  CompositePropagator::inject(propagator, ctx4, carrier)
  
  // Extract context in a different context
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // Verify original context is still intact
  assert_eq(Context::get(ctx4, user_key), Some("user123"))
  assert_eq(Context::get(ctx4, session_key), Some("session456"))
  
  // Verify extracted context has extraction marker
  let extracted_marker = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_marker, Some("true"))
}

// Test 5: Resource Attribute Management
test "resource attribute management" {
  // Create base resource with service information
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.namespace", StringValue("production"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create override resource with additional attributes
  let override_attrs = [
    ("service.name", StringValue("azimuth-service-v2")),  // This should override
    ("deployment.environment", StringValue("staging")),
    ("host.name", StringValue("host-123")),
    ("os.type", StringValue("linux"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify merged attributes
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  let service_namespace = Resource::get_attribute(merged_resource, "service.namespace")
  let deployment_env = Resource::get_attribute(merged_resource, "deployment.environment")
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  let os_type = Resource::get_attribute(merged_resource, "os.type")
  
  // Verify override behavior
  assert_eq(service_name, Some(StringValue("azimuth-service-v2")))
  assert_eq(service_version, Some(StringValue("1.2.3")))
  assert_eq(service_namespace, Some(StringValue("production")))
  assert_eq(deployment_env, Some(StringValue("staging")))
  assert_eq(host_name, Some(StringValue("host-123")))
  assert_eq(os_type, Some(StringValue("linux")))
  
  // Test attribute types
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service-v2")
    _ => assert_true(false)
  }
  
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.2.3")
    _ => assert_true(false)
  }
}

// Test 6: HTTP Client Error Handling
test "http client error handling" {
  // Test various HTTP scenarios
  let success_headers = [("Content-Type", "application/json")]
  let success_response = HttpResponse::new(200, success_headers, Some("{\"status\":\"success\"}"))
  
  let error_headers = [("Content-Type", "text/plain")]
  let error_response = HttpResponse::new(404, error_headers, Some("Not Found"))
  
  let server_error_response = HttpResponse::new(500, [], Some("Internal Server Error"))
  let timeout_response = HttpResponse::new(504, [], Some("Gateway Timeout"))
  
  // Verify success response
  assert_eq(HttpResponse::status_code(success_response), 200)
  assert_eq(HttpResponse::body(success_response), Some("{\"status\":\"success\"}"))
  
  // Verify error responses
  assert_eq(HttpResponse::status_code(error_response), 404)
  assert_eq(HttpResponse::body(error_response), Some("Not Found"))
  
  assert_eq(HttpResponse::status_code(server_error_response), 500)
  assert_eq(HttpResponse::body(server_error_response), Some("Internal Server Error"))
  
  assert_eq(HttpResponse::status_code(timeout_response), 504)
  assert_eq(HttpResponse::body(timeout_response), Some("Gateway Timeout"))
  
  // Test request creation with different methods
  let get_request = HttpRequest::new("GET", "https://api.example.com/data", [])
  let post_request = HttpRequest::new("POST", "https://api.example.com/data", [], Some("{\"data\":\"value\"}"))
  let put_request = HttpRequest::new("PUT", "https://api.example.com/data/123", [], Some("{\"data\":\"updated\"}"))
  let delete_request = HttpRequest::new("DELETE", "https://api.example.com/data/123", [])
  
  assert_eq(HttpRequest::http_method(get_request), "GET")
  assert_eq(HttpRequest::http_method(post_request), "POST")
  assert_eq(HttpRequest::http_method(put_request), "PUT")
  assert_eq(HttpRequest::http_method(delete_request), "DELETE")
  
  assert_eq(HttpRequest::body(get_request), None)
  assert_eq(HttpRequest::body(post_request), Some("{\"data\":\"value\"}"))
  assert_eq(HttpRequest::body(put_request), Some("{\"data\":\"updated\"}"))
  assert_eq(HttpRequest::body(delete_request), None)
}

// Test 7: Baggage Operations with Complex Values
test "baggage operations with complex values" {
  // Test baggage with multiple entries
  let baggage = Baggage::new()
  
  // Add multiple baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session456")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "request789")
  let baggage4 = Baggage::set_entry(baggage3, "trace.id", "trace012")
  
  // Verify all entries exist
  assert_eq(Baggage::get_entry(baggage4, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage4, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(baggage4, "request.id"), Some("request789"))
  assert_eq(Baggage::get_entry(baggage4, "trace.id"), Some("trace012"))
  
  // Test baggage entry removal
  let baggage_reduced = Baggage::remove_entry(baggage4, "session.id")
  assert_eq(Baggage::get_entry(baggage_reduced, "session.id"), None)
  
  // Verify other entries still exist
  assert_eq(Baggage::get_entry(baggage_reduced, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage_reduced, "request.id"), Some("request789"))
  assert_eq(Baggage::get_entry(baggage_reduced, "trace.id"), Some("trace012"))
  
  // Test multiple removals
  let baggage_minimal = Baggage::remove_entry(
    Baggage::remove_entry(
      Baggage::remove_entry(baggage_reduced, "user.id"),
      "request.id"
    ),
    "trace.id"
  )
  
  assert_eq(Baggage::get_entry(baggage_minimal, "user.id"), None)
  assert_eq(Baggage::get_entry(baggage_minimal, "session.id"), None)
  assert_eq(Baggage::get_entry(baggage_minimal, "request.id"), None)
  assert_eq(Baggage::get_entry(baggage_minimal, "trace.id"), None)
}

// Test 8: Advanced Instrument Type Handling
test "advanced instrument type handling" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "azimuth.advanced.instruments")
  
  // Create all instrument types
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test updown"), Some("items"))
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  
  // Convert to instrument type and verify properties
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  let updown_instrument = UpDownCounter(updown_counter.name, updown_counter.description, updown_counter.unit)
  let gauge_instrument = Gauge(gauge.name, gauge.description, gauge.unit)
  
  // Test instrument name extraction
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  assert_eq(Instrument::name(updown_instrument), "test.updown")
  assert_eq(Instrument::name(gauge_instrument), "test.gauge")
  
  // Test instrument description extraction
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::description(histogram_instrument), Some("Test histogram"))
  assert_eq(Instrument::description(updown_instrument), Some("Test updown"))
  assert_eq(Instrument::description(gauge_instrument), Some("Test gauge"))
  
  // Test instrument unit extraction
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  assert_eq(Instrument::unit(updown_instrument), Some("items"))
  assert_eq(Instrument::unit(gauge_instrument), Some("percent"))
  
  // Test instrument operations
  Counter::add(counter, 100.0)
  Histogram::record(histogram, 150.5)
  UpDownCounter::add(updown_counter, 50.0)
  // Note: Gauge doesn't have a record operation in the current implementation
}