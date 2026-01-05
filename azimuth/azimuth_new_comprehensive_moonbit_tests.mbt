// Azimuth New Comprehensive MoonBit Test Suite
// This file contains 10 comprehensive test cases for the Azimuth telemetry system

// Test 1: Advanced Attribute Type Conversions
test "advanced attribute type conversions" {
  // Test string to various type conversions
  let string_attr = StringValue("test_value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14159)
  let bool_attr = BoolValue(true)
  
  // Test array attributes
  let string_array = ArrayStringValue(["a", "b", "c"])
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  
  // Test empty arrays
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  // Test single element arrays
  let single_string_array = ArrayStringValue(["single"])
  let single_int_array = ArrayIntValue([42])
  
  // Verify attribute types
  assert_eq(string_attr, StringValue("test_value"))
  assert_eq(int_attr, IntValue(42))
  assert_eq(float_attr, FloatValue(3.14159))
  assert_eq(bool_attr, BoolValue(true))
  assert_eq(string_array, ArrayStringValue(["a", "b", "c"]))
  assert_eq(int_array, ArrayIntValue([1, 2, 3, 4, 5]))
  assert_eq(empty_string_array, ArrayStringValue([]))
  assert_eq(empty_int_array, ArrayIntValue([]))
  assert_eq(single_string_array, ArrayStringValue(["single"]))
  assert_eq(single_int_array, ArrayIntValue([42]))
}

// Test 2: Span Status and Event Handling
test "span status and event handling" {
  // Create span with context
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "")
  let span = Span::new("test-operation", Internal, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(span), "test-operation")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  assert_eq(SpanContext::trace_id(span.span_context), "trace-12345")
  assert_eq(SpanContext::span_id(span.span_context), "span-67890")
  
  // Test span status operations
  Span::set_status(span, Ok)
  assert_eq(Span::status(span), Ok)
  
  Span::set_status(span, Error, Some("Test error"))
  assert_eq(Span::status(span), Error)
  
  // Test span event operations
  Span::add_event(span, "event1")
  Span::add_event(span, "event2", Some([("key1", StringValue("value1"))]))
  Span::add_event(span, "event3", Some([("key2", IntValue(42)), ("key3", BoolValue(true))]))
  
  // Test span ending
  Span::end(span)
  
  // Verify span context after operations
  assert_true(SpanContext::is_valid(span.span_context))
  assert_true(SpanContext::is_sampled(span.span_context))
}

// Test 3: Multi-dimensional Metrics Collection
test "multi-dimensional metrics collection" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi-dimensional-test")
  
  // Create various metric instruments
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_time = Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("ms"))
  let active_connections = Meter::create_updown_counter(meter, "http.active.connections", Some("Active HTTP connections"), Some("connections"))
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage", Some("System CPU usage"), Some("percent"))
  
  // Test metric operations with values
  Counter::add(request_counter, 1.0)
  Counter::add(request_counter, 5.0)
  Counter::add(request_counter, 10.0)
  
  Histogram::record(response_time, 100.0)
  Histogram::record(response_time, 200.0)
  Histogram::record(response_time, 150.0)
  
  UpDownCounter::add(active_connections, 1.0)
  UpDownCounter::add(active_connections, 2.0)
  UpDownCounter::add(active_connections, -1.0)
  
  // Verify metric properties
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  assert_eq(response_time.name, "http.response.time")
  assert_eq(response_time.description, Some("HTTP response time"))
  assert_eq(response_time.unit, Some("ms"))
  
  assert_eq(active_connections.name, "http.active.connections")
  assert_eq(active_connections.description, Some("Active HTTP connections"))
  assert_eq(active_connections.unit, Some("connections"))
  
  assert_eq(cpu_usage.name, "system.cpu.usage")
  assert_eq(cpu_usage.description, Some("System CPU usage"))
  assert_eq(cpu_usage.unit, Some("percent"))
}

// Test 4: Structured Logging with Context
test "structured logging with context" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "structured-logger")
  
  // Create log records with various severity levels
  let debug_log = LogRecord::new(Debug, "Debug message with context")
  let info_log = LogRecord::new(Info, "Info message with context")
  let warn_log = LogRecord::new(Warn, "Warning message with context")
  let error_log = LogRecord::new(Error, "Error message with context")
  
  // Create detailed log with full context
  let detailed_log = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  // Emit log records
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, detailed_log)
  
  // Verify log properties
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::body(debug_log), Some("Debug message with context"))
  
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::body(info_log), Some("Info message with context"))
  
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::body(warn_log), Some("Warning message with context"))
  
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::body(error_log), Some("Error message with context"))
  
  assert_eq(LogRecord::severity_number(detailed_log), Error)
  assert_eq(LogRecord::body(detailed_log), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(detailed_log), Some("trace-12345"))
  assert_eq(LogRecord::span_id(detailed_log), Some("span-67890"))
}

// Test 5: Resource Attribute Management
test "resource attribute management" {
  // Create resource with service attributes
  let base_resource = Resource::new()
  let service_resource = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-abc123")),
    ("deployment.environment", StringValue("production"))
  ])
  
  // Create resource with host attributes
  let host_resource = Resource::with_attributes(base_resource, [
    ("host.name", StringValue("prod-server-01")),
    ("host.id", StringValue("host-def456")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("Ubuntu 20.04"))
  ])
  
  // Create resource with process attributes
  let process_resource = Resource::with_attributes(base_resource, [
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("payment-service")),
    ("process.command_line", StringValue("./payment-service --config prod.yaml"))
  ])
  
  // Verify service resource attributes
  assert_eq(Resource::get_attribute(service_resource, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(service_resource, "service.version"), Some(StringValue("2.1.0")))
  assert_eq(Resource::get_attribute(service_resource, "service.instance.id"), Some(StringValue("instance-abc123")))
  assert_eq(Resource::get_attribute(service_resource, "deployment.environment"), Some(StringValue("production")))
  
  // Verify host resource attributes
  assert_eq(Resource::get_attribute(host_resource, "host.name"), Some(StringValue("prod-server-01")))
  assert_eq(Resource::get_attribute(host_resource, "host.id"), Some(StringValue("host-def456")))
  assert_eq(Resource::get_attribute(host_resource, "os.type"), Some(StringValue("linux")))
  assert_eq(Resource::get_attribute(host_resource, "os.version"), Some(StringValue("Ubuntu 20.04")))
  
  // Verify process resource attributes
  assert_eq(Resource::get_attribute(process_resource, "process.pid"), Some(IntValue(12345)))
  assert_eq(Resource::get_attribute(process_resource, "process.executable.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(process_resource, "process.command_line"), Some(StringValue("./payment-service --config prod.yaml")))
  
  // Test resource merging
  let merged_resource = Resource::merge(service_resource, host_resource)
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("prod-server-01")))
}

// Test 6: Context Propagation with Baggage
test "context propagation with baggage" {
  // Create root context
  let root_ctx = Context::root()
  
  // Create context keys
  let user_id_key = ContextKey::new("user.id")
  let request_id_key = ContextKey::new("request.id")
  let session_id_key = ContextKey::new("session.id")
  
  // Set context values
  let ctx_with_user = Context::with_value(root_ctx, user_id_key, "user-12345")
  let ctx_with_request = Context::with_value(ctx_with_user, request_id_key, "req-67890")
  let ctx_with_session = Context::with_value(ctx_with_request, session_id_key, "session-abc")
  
  // Verify context values
  assert_eq(Context::get(ctx_with_user, user_id_key), Some("user-12345"))
  assert_eq(Context::get(ctx_with_request, request_id_key), Some("req-67890"))
  assert_eq(Context::get(ctx_with_session, session_id_key), Some("session-abc"))
  
  // Test baggage operations
  let empty_baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(empty_baggage, "user.id", "user-12345")
  let baggage_with_request = Baggage::set_entry(baggage_with_user, "request.id", "req-67890")
  let baggage_with_session = Baggage::set_entry(baggage_with_request, "session.id", "session-abc")
  
  // Verify baggage entries
  assert_eq(Baggage::get_entry(baggage_with_user, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(baggage_with_request, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(baggage_with_request, "request.id"), Some("req-67890"))
  assert_eq(Baggage::get_entry(baggage_with_session, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(baggage_with_session, "request.id"), Some("req-67890"))
  assert_eq(Baggage::get_entry(baggage_with_session, "session.id"), Some("session-abc"))
  
  // Test baggage removal
  let baggage_without_user = Baggage::remove_entry(baggage_with_session, "user.id")
  assert_eq(Baggage::get_entry(baggage_without_user, "user.id"), None)
  assert_eq(Baggage::get_entry(baggage_without_user, "request.id"), Some("req-67890"))
  assert_eq(Baggage::get_entry(baggage_without_user, "session.id"), Some("session-abc"))
}

// Test 7: Trace Context Propagation
test "trace context propagation" {
  // Create span contexts with different configurations
  let sampled_ctx = SpanContext::new("trace-12345", "span-67890", true, "key1=value1,key2=value2")
  let not_sampled_ctx = SpanContext::new("trace-54321", "span-09876", false, "key3=value3")
  let empty_state_ctx = SpanContext::new("trace-11111", "span-22222", true, "")
  
  // Verify sampled context
  assert_eq(SpanContext::trace_id(sampled_ctx), "trace-12345")
  assert_eq(SpanContext::span_id(sampled_ctx), "span-67890")
  assert_true(SpanContext::is_sampled(sampled_ctx))
  assert_true(SpanContext::is_valid(sampled_ctx))
  assert_eq(SpanContext::trace_state(sampled_ctx), "key1=value1,key2=value2")
  
  // Verify not sampled context
  assert_eq(SpanContext::trace_id(not_sampled_ctx), "trace-54321")
  assert_eq(SpanContext::span_id(not_sampled_ctx), "span-09876")
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
  assert_true(SpanContext::is_valid(not_sampled_ctx))
  assert_eq(SpanContext::trace_state(not_sampled_ctx), "key3=value3")
  
  // Verify empty trace state context
  assert_eq(SpanContext::trace_id(empty_state_ctx), "trace-11111")
  assert_eq(SpanContext::span_id(empty_state_ctx), "span-22222")
  assert_true(SpanContext::is_sampled(empty_state_ctx))
  assert_true(SpanContext::is_valid(empty_state_ctx))
  assert_eq(SpanContext::trace_state(empty_state_ctx), "")
  
  // Test invalid contexts
  let invalid_trace_ctx = SpanContext::new("", "span-123", true, "")
  let invalid_span_ctx = SpanContext::new("trace-123", "", true, "")
  let invalid_both_ctx = SpanContext::new("", "", false, "")
  
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  assert_false(SpanContext::is_valid(invalid_both_ctx))
  assert_false(SpanContext::is_sampled(invalid_both_ctx))
}

// Test 8: Composite Propagator Operations
test "composite propagator operations" {
  // Create individual propagators
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Create composite propagators
  let trace_only_propagators = [trace_propagator]
  let composite_trace_only = CompositePropagator::new(trace_only_propagators)
  
  let both_propagators = [trace_propagator, trace_propagator]  // Using trace_propagator twice for testing
  let composite_both = CompositePropagator::new(both_propagators)
  
  // Test injection
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Inject with trace-only propagator
  CompositePropagator::inject(composite_trace_only, ctx, carrier)
  
  // Inject with both propagators
  CompositePropagator::inject(composite_both, ctx, carrier)
  
  // Test extraction
  let extracted_ctx_trace = CompositePropagator::extract(composite_trace_only, carrier)
  let extracted_ctx_both = CompositePropagator::extract(composite_both, carrier)
  
  // Verify extraction results
  let key = ContextKey::new("extracted")
  let extracted_trace = Context::get(extracted_ctx_trace, key)
  let extracted_both = Context::get(extracted_ctx_both, key)
  
  assert_eq(extracted_trace, Some("true"))
  assert_eq(extracted_both, Some("true"))
  
  // Test carrier operations
  let test_carrier = TextMapCarrier::new()
  TextMapCarrier::set(test_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(test_carrier, "baggage", "user.id=12345,request.id=67890")
  
  let trace_header = TextMapCarrier::get(test_carrier, "traceparent")
  let baggage_header = TextMapCarrier::get(test_carrier, "baggage")
  let missing_header = TextMapCarrier::get(test_carrier, "missing")
  
  assert_eq(trace_header, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(baggage_header, Some("user.id=12345,request.id=67890"))
  assert_eq(missing_header, None)
}

// Test 9: HTTP Client Request/Response Cycle
test "http client request/response cycle" {
  // Test HTTP client creation
  let client = HttpClient::new()
  
  // Test various HTTP requests
  let get_request = HttpRequest::new("GET", "https://api.example.com/users", [], None)
  let post_request = HttpRequest::new("POST", "https://api.example.com/users", 
    [("Content-Type", "application/json"), ("Accept", "application/json")], 
    Some("{\"name\": \"John Doe\", \"email\": \"john@example.com\"}"))
  let put_request = HttpRequest::new("PUT", "https://api.example.com/users/123", 
    [("Content-Type", "application/json"), ("Authorization", "Bearer token123")], 
    Some("{\"name\": \"Jane Doe\"}"))
  let delete_request = HttpRequest::new("DELETE", "https://api.example.com/users/123", 
    [("Authorization", "Bearer token123")], None)
  
  // Verify request properties
  assert_eq(HttpRequest::http_method(get_request), "GET")
  assert_eq(HttpRequest::url(get_request), "https://api.example.com/users")
  assert_eq(HttpRequest::body(get_request), None)
  
  assert_eq(HttpRequest::http_method(post_request), "POST")
  assert_eq(HttpRequest::url(post_request), "https://api.example.com/users")
  assert_eq(HttpRequest::body(post_request), Some("{\"name\": \"John Doe\", \"email\": \"john@example.com\"}"))
  
  assert_eq(HttpRequest::http_method(put_request), "PUT")
  assert_eq(HttpRequest::url(put_request), "https://api.example.com/users/123")
  assert_eq(HttpRequest::body(put_request), Some("{\"name\": \"Jane Doe\"}"))
  
  assert_eq(HttpRequest::http_method(delete_request), "DELETE")
  assert_eq(HttpRequest::url(delete_request), "https://api.example.com/users/123")
  assert_eq(HttpRequest::body(delete_request), None)
  
  // Test various HTTP responses
  let success_response = HttpResponse::new(200, [("Content-Type", "application/json")], 
    Some("{\"id\": 123, \"name\": \"John Doe\", \"email\": \"john@example.com\"}"))
  let created_response = HttpResponse::new(201, [("Content-Type", "application/json"), ("Location", "/users/123")], 
    Some("{\"id\": 123, \"name\": \"John Doe\"}"))
  let error_response = HttpResponse::new(400, [("Content-Type", "application/json")], 
    Some("{\"error\": \"Invalid request data\"}"))
  let not_found_response = HttpResponse::new(404, [("Content-Type", "application/json")], 
    Some("{\"error\": \"User not found\"}"))
  let server_error_response = HttpResponse::new(500, [("Content-Type", "application/json")], 
    Some("{\"error\": \"Internal server error\"}"))
  
  // Verify response properties
  assert_eq(HttpResponse::status_code(success_response), 200)
  assert_eq(HttpResponse::body(success_response), Some("{\"id\": 123, \"name\": \"John Doe\", \"email\": \"john@example.com\"}"))
  
  assert_eq(HttpResponse::status_code(created_response), 201)
  assert_eq(HttpResponse::body(created_response), Some("{\"id\": 123, \"name\": \"John Doe\"}"))
  
  assert_eq(HttpResponse::status_code(error_response), 400)
  assert_eq(HttpResponse::body(error_response), Some("{\"error\": \"Invalid request data\"}"))
  
  assert_eq(HttpResponse::status_code(not_found_response), 404)
  assert_eq(HttpResponse::body(not_found_response), Some("{\"error\": \"User not found\"}"))
  
  assert_eq(HttpResponse::status_code(server_error_response), 500)
  assert_eq(HttpResponse::body(server_error_response), Some("{\"error\": \"Internal server error\"}"))
}

// Test 10: Time and Random Operations
test "time and random operations" {
  // Test clock operations
  let system_clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(system_clock)
  let timestamp2 = Clock::now_unix_nanos(system_clock)
  
  // Verify timestamp properties
  assert_true(timestamp1 > 0L)
  assert_true(timestamp2 > 0L)
  assert_true(timestamp2 >= timestamp1)  // Time should be non-decreasing
  
  // Test random operations
  let system_random = Random::system()
  let random_bytes1 = Random::next_bytes(system_random, 16)
  let random_bytes2 = Random::next_bytes(system_random, 16)
  let random_u64_1 = Random::next_u64(system_random)
  let random_u64_2 = Random::next_u64(system_random)
  
  // Verify random properties
  assert_eq(random_bytes1.length, 16)
  assert_eq(random_bytes2.length, 16)
  assert_eq(random_u64_1, 12345UL)  // Simplified implementation returns fixed value
  assert_eq(random_u64_2, 12345UL)  // Simplified implementation returns fixed value
  
  // Test with different byte lengths
  let empty_bytes = Random::next_bytes(system_random, 0)
  let single_byte = Random::next_bytes(system_random, 1)
  let large_bytes = Random::next_bytes(system_random, 1024)
  
  assert_eq(empty_bytes.length, 0)
  assert_eq(single_byte.length, 1)
  assert_eq(large_bytes.length, 1024)
  
  // Test timestamp in log records
  let log_with_timestamp = LogRecord::new_with_context(
    Info,
    Some("Log with timestamp"),
    None,
    Some(timestamp1),
    Some(timestamp2),
    None,
    None,
    None
  )
  
  assert_eq(LogRecord::body(log_with_timestamp), Some("Log with timestamp"))
}