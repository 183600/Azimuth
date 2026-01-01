// Azimuth Comprehensive Test Suite
// This file contains comprehensive test cases for the Azimuth telemetry system

// Test 1: Advanced attributes operations with edge cases
pub test "comprehensive attributes edge cases" {
  // Test empty and null attribute handling
  let empty_attrs = azimuth::Attributes::new()
  assert_eq(azimuth::Attributes::get(empty_attrs, "nonexistent.key"), None)
  
  // Test attributes with special characters and Unicode
  let unicode_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(unicode_attrs, "unicode.test", azimuth::StringValue("测试中文🚀"))
  azimuth::Attributes::set(unicode_attrs, "emoji.test", azimuth::StringValue("😀🎉🔥"))
  azimuth::Attributes::set(unicode_attrs, "special.chars", azimuth::StringValue("!@#$%^&*()"))
  
  // Test extreme numeric values
  let extreme_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(extreme_attrs, "max.int", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(extreme_attrs, "min.int", azimuth::IntValue(-2147483648))
  azimuth::Attributes::set(extreme_attrs, "max.float", azimuth::FloatValue(1.7976931348623157e+308))
  azimuth::Attributes::set(extreme_attrs, "min.float", azimuth::FloatValue(-1.7976931348623157e+308))
  azimuth::Attributes::set(extreme_attrs, "zero.float", azimuth::FloatValue(0.0))
  azimuth::Attributes::set(extreme_attrs, "negative.zero", azimuth::FloatValue(-0.0))
  
  // Test boolean edge cases
  let bool_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(bool_attrs, "true.value", azimuth::BoolValue(true))
  azimuth::Attributes::set(bool_attrs, "false.value", azimuth::BoolValue(false))
  
  // Test array attributes with various data
  let array_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(array_attrs, "empty.string.array", azimuth::ArrayStringValue([]))
  azimuth::Attributes::set(array_attrs, "single.string.array", azimuth::ArrayStringValue(["single"]))
  azimuth::Attributes::set(array_attrs, "multi.string.array", azimuth::ArrayStringValue(["a", "b", "c", "d", "e"]))
  azimuth::Attributes::set(array_attrs, "empty.int.array", azimuth::ArrayIntValue([]))
  azimuth::Attributes::set(array_attrs, "single.int.array", azimuth::ArrayIntValue([42]))
  azimuth::Attributes::set(array_attrs, "multi.int.array", azimuth::ArrayIntValue([1, 2, 3, 4, 5]))
}

// Test 2: Comprehensive span lifecycle management
pub test "comprehensive span lifecycle" {
  // Test span creation with various configurations
  let span_ctx = azimuth::SpanContext::new("trace-12345", "span-67890", true, "key1=value1,key2=value2")
  let internal_span = azimuth::Span::new("internal-operation", azimuth::Internal, span_ctx)
  let server_span = azimuth::Span::new("server-operation", azimuth::Server, span_ctx)
  let client_span = azimuth::Span::new("client-operation", azimuth::Client, span_ctx)
  let producer_span = azimuth::Span::new("producer-operation", azimuth::Producer, span_ctx)
  let consumer_span = azimuth::Span::new("consumer-operation", azimuth::Consumer, span_ctx)
  
  // Verify span properties
  assert_eq(azimuth::Span::name(internal_span), "internal-operation")
  assert_eq(azimuth::Span::kind(internal_span), azimuth::Internal)
  assert_eq(azimuth::Span::name(server_span), "server-operation")
  assert_eq(azimuth::Span::kind(server_span), azimuth::Server)
  assert_eq(azimuth::Span::name(client_span), "client-operation")
  assert_eq(azimuth::Span::kind(client_span), azimuth::Client)
  assert_eq(azimuth::Span::name(producer_span), "producer-operation")
  assert_eq(azimuth::Span::kind(producer_span), azimuth::Producer)
  assert_eq(azimuth::Span::name(consumer_span), "consumer-operation")
  assert_eq(azimuth::Span::kind(consumer_span), azimuth::Consumer)
  
  // Test span context operations
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), "trace-12345")
  assert_eq(azimuth::SpanContext::span_id(span_ctx), "span-67890")
  assert_true(azimuth::SpanContext::is_valid(span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  assert_eq(azimuth::SpanContext::trace_state(span_ctx), "key1=value1,key2=value2")
  
  // Test invalid span contexts
  let invalid_trace_ctx = azimuth::SpanContext::new("", "span-123", true, "")
  let invalid_span_ctx = azimuth::SpanContext::new("trace-123", "", true, "")
  let invalid_both_ctx = azimuth::SpanContext::new("", "", false, "")
  
  assert_false(azimuth::SpanContext::is_valid(invalid_trace_ctx))
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx))
  assert_false(azimuth::SpanContext::is_valid(invalid_both_ctx))
  assert_false(azimuth::SpanContext::is_sampled(invalid_both_ctx))
}

// Test 3: Comprehensive metrics operations
pub test "comprehensive metrics operations" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "comprehensive-test-meter")
  
  // Test counter operations
  let counter = azimuth::Meter::create_counter(meter, "test.counter", Some("Test counter description"), Some("count"))
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, Some("Test counter description"))
  assert_eq(counter.unit, Some("count"))
  
  // Test histogram operations
  let histogram = azimuth::Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test up-down counter operations
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "test.updown", Some("Test up-down counter"), Some("items"))
  assert_eq(updown_counter.name, "test.updown")
  assert_eq(updown_counter.description, Some("Test up-down counter"))
  assert_eq(updown_counter.unit, Some("items"))
  
  // Test gauge operations
  let gauge = azimuth::Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  assert_eq(gauge.name, "test.gauge")
  assert_eq(gauge.description, Some("Test gauge"))
  assert_eq(gauge.unit, Some("percent"))
  
  // Test instrument conversions
  let counter_instrument = azimuth::Histogram::as_instrument(histogram)  // Using histogram as proxy
  assert_eq(azimuth::Instrument::name(counter_instrument), "test.histogram")
  assert_eq(azimuth::Instrument::description(counter_instrument), Some("Test histogram"))
  assert_eq(azimuth::Instrument::unit(counter_instrument), Some("ms"))
  
  // Test metrics with edge case names
  let empty_name_metric = azimuth::Meter::create_counter(meter, "", Some("Empty name"), None)
  let long_name_metric = azimuth::Meter::create_counter(meter, "this.is.a.very.long.metric.name.that.tests.boundary.conditions", Some("Long name"), None)
  let special_chars_metric = azimuth::Meter::create_counter(meter, "metric.with-special_chars_123", Some("Special chars"), None)
  
  assert_eq(empty_name_metric.name, "")
  assert_eq(long_name_metric.name, "this.is.a.very.long.metric.name.that.tests.boundary.conditions")
  assert_eq(special_chars_metric.name, "metric.with-special_chars_123")
}

// Test 4: Comprehensive logging operations
pub test "comprehensive logging operations" {
  // Test log records with different severity levels
  let trace_log = azimuth::LogRecord::new(azimuth::Trace, "Trace level message")
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug level message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info level message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning level message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error level message")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal level message")
  
  // Verify severity levels
  assert_eq(azimuth::LogRecord::severity_number(trace_log), azimuth::Trace)
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // Verify log bodies
  assert_eq(azimuth::LogRecord::body(trace_log), Some("Trace level message"))
  assert_eq(azimuth::LogRecord::body(debug_log), Some("Debug level message"))
  assert_eq(azimuth::LogRecord::body(info_log), Some("Info level message"))
  assert_eq(azimuth::LogRecord::body(warn_log), Some("Warning level message"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("Error level message"))
  assert_eq(azimuth::LogRecord::body(fatal_log), Some("Fatal level message"))
  
  // Test detailed log records with full context
  let detailed_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Detailed error message"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(azimuth::Context::root())
  )
  
  assert_eq(azimuth::LogRecord::severity_number(detailed_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(detailed_log), Some("Detailed error message"))
  assert_eq(azimuth::LogRecord::trace_id(detailed_log), Some("trace-12345"))
  assert_eq(azimuth::LogRecord::span_id(detailed_log), Some("span-67890"))
  
  // Test logger operations
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "comprehensive-test-logger")
  
  assert_eq(logger.scope.name, "comprehensive-test-logger")
  
  // Emit various log records
  azimuth::Logger::emit(logger, trace_log)
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, detailed_log)
}

// Test 5: Comprehensive resource management
pub test "comprehensive resource management" {
  // Test resource creation and operations
  let empty_resource = azimuth::Resource::new()
  let resource_with_attrs = azimuth::Resource::with_attributes(empty_resource, [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("deployment.environment", azimuth::StringValue("test")),
    ("host.name", azimuth::StringValue("test-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("process.id", azimuth::IntValue(12345)),
    ("process.name", azimuth::StringValue("test-process")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ])
  
  // Verify resource attributes
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "service.name"), Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "deployment.environment"), Some(azimuth::StringValue("test")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "host.name"), Some(azimuth::StringValue("test-host")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "os.version"), Some(azimuth::StringValue("5.15.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "process.id"), Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "process.name"), Some(azimuth::StringValue("test-process")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
  
  // Test resource merging
  let override_resource = azimuth::Resource::with_attributes(empty_resource, [
    ("service.version", azimuth::StringValue("2.0.0")),  // Override existing
    ("deployment.environment", azimuth::StringValue("production")),  // Override existing
    ("new.attribute", azimuth::StringValue("new-value"))  // New attribute
  ])
  
  let merged_resource = azimuth::Resource::merge(resource_with_attrs, override_resource)
  
  // Verify merged resource (override should win)
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("test-service")))  // Unchanged
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("2.0.0")))  // Overridden
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.environment"), Some(azimuth::StringValue("production")))  // Overridden
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "new.attribute"), Some(azimuth::StringValue("new-value")))  // New
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "host.name"), Some(azimuth::StringValue("test-host")))  // Unchanged
}

// Test 6: Comprehensive context and baggage operations
pub test "comprehensive context and baggage operations" {
  // Test context operations
  let root_ctx = azimuth::Context::root()
  
  // Test context with various key types
  let string_key = azimuth::ContextKey::new("string.key")
  let int_key = azimuth::ContextKey::new("int.key")
  let bool_key = azimuth::ContextKey::new("bool.key")
  let empty_key = azimuth::ContextKey::new("")
  let long_key = azimuth::ContextKey::new("this.is.a.very.long.key.name.that.tests.boundary.conditions")
  let special_key = azimuth::ContextKey::new("key.with-special_chars_123")
  
  // Set context values
  let ctx_with_string = azimuth::Context::with_value(root_ctx, string_key, "string.value")
  let ctx_with_int = azimuth::Context::with_value(ctx_with_string, int_key, "42")
  let ctx_with_bool = azimuth::Context::with_value(ctx_with_int, bool_key, "true")
  let ctx_with_empty = azimuth::Context::with_value(ctx_with_bool, empty_key, "empty.value")
  let ctx_with_long = azimuth::Context::with_value(ctx_with_empty, long_key, "long.value")
  let ctx_with_special = azimuth::Context::with_value(ctx_with_long, special_key, "special.value")
  
  // Verify context values
  assert_eq(azimuth::Context::get(ctx_with_string, string_key), Some("string.value"))
  assert_eq(azimuth::Context::get(ctx_with_int, int_key), Some("42"))
  assert_eq(azimuth::Context::get(ctx_with_bool, bool_key), Some("true"))
  assert_eq(azimuth::Context::get(ctx_with_empty, empty_key), Some("empty.value"))
  assert_eq(azimuth::Context::get(ctx_with_long, long_key), Some("long.value"))
  assert_eq(azimuth::Context::get(ctx_with_special, special_key), Some("special.value"))
  
  // Test missing keys
  let missing_key = azimuth::ContextKey::new("missing.key")
  assert_eq(azimuth::Context::get(root_ctx, missing_key), None)
  
  // Test baggage operations
  let empty_baggage = azimuth::Baggage::new()
  let baggage_with_entry = azimuth::Baggage::set_entry(empty_baggage, "user.id", "12345")
  let baggage_with_multiple = azimuth::Baggage::set_entry(baggage_with_entry, "request.id", "req-67890")
  let baggage_with_special = azimuth::Baggage::set_entry(baggage_with_multiple, "special.key", "special.value")
  
  // Verify baggage entries
  assert_eq(azimuth::Baggage::get_entry(empty_baggage, "user.id"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_with_entry, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_multiple, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_multiple, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_special, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_special, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_special, "special.key"), Some("special.value"))
}

// Test 7: Comprehensive propagator operations
pub test "comprehensive propagator operations" {
  // Test individual propagators
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // Test composite propagator with single propagator
  let single_propagator = [trace_propagator]
  let composite_single = azimuth::CompositePropagator::new(single_propagator)
  
  // Test composite propagator with multiple propagators
  let multiple_propagators = [trace_propagator, trace_propagator]  // Using trace_propagator twice for testing
  let composite_multiple = azimuth::CompositePropagator::new(multiple_propagators)
  
  // Test carrier operations
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // Test inject operations
  azimuth::CompositePropagator::inject(composite_single, ctx, carrier)
  azimuth::CompositePropagator::inject(composite_multiple, ctx, carrier)
  
  // Test extract operations
  let extracted_ctx_single = azimuth::CompositePropagator::extract(composite_single, carrier)
  let extracted_ctx_multiple = azimuth::CompositePropagator::extract(composite_multiple, carrier)
  
  // Verify extraction results
  let key = azimuth::ContextKey::new("extracted")
  let extracted_single = azimuth::Context::get(extracted_ctx_single, key)
  let extracted_multiple = azimuth::Context::get(extracted_ctx_multiple, key)
  
  assert_eq(extracted_single, Some("true"))
  assert_eq(extracted_multiple, Some("true"))
  
  // Test carrier header operations
  let populated_carrier = azimuth::TextMapCarrier::new()
  // Simulate populated carrier headers
  let trace_header = azimuth::TextMapCarrier::get(populated_carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(populated_carrier, "baggage")
  
  // In a real implementation, these would be populated during inject
  // For testing purposes, we verify the carrier operations work
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
}

// Test 8: Comprehensive HTTP client operations
pub test "comprehensive http client operations" {
  // Test HTTP client creation
  let client = azimuth::HttpClient::new()
  
  // Test HTTP requests with various methods
  let get_request = azimuth::HttpRequest::new("GET", "https://api.example.com/data", [], None)
  let post_request = azimuth::HttpRequest::new("POST", "https://api.example.com/create", [("Content-Type", "application/json")], Some("{\"name\": \"test\"}"))
  let put_request = azimuth::HttpRequest::new("PUT", "https://api.example.com/update", [("Content-Type", "application/json"), ("Authorization", "Bearer token")], Some("{\"id\": 123}"))
  let delete_request = azimuth::HttpRequest::new("DELETE", "https://api.example.com/delete/123", [("Authorization", "Bearer token")], None)
  
  // Verify request properties
  assert_eq(azimuth::HttpRequest::http_method(get_request), "GET")
  assert_eq(azimuth::HttpRequest::url(get_request), "https://api.example.com/data")
  assert_eq(azimuth::HttpRequest::body(get_request), None)
  
  assert_eq(azimuth::HttpRequest::http_method(post_request), "POST")
  assert_eq(azimuth::HttpRequest::url(post_request), "https://api.example.com/create")
  assert_eq(azimuth::HttpRequest::body(post_request), Some("{\"name\": \"test\"}"))
  
  assert_eq(azimuth::HttpRequest::http_method(put_request), "PUT")
  assert_eq(azimuth::HttpRequest::url(put_request), "https://api.example.com/update")
  assert_eq(azimuth::HttpRequest::body(put_request), Some("{\"id\": 123}"))
  
  assert_eq(azimuth::HttpRequest::http_method(delete_request), "DELETE")
  assert_eq(azimuth::HttpRequest::url(delete_request), "https://api.example.com/delete/123")
  assert_eq(azimuth::HttpRequest::body(delete_request), None)
  
  // Test HTTP responses with various status codes
  let ok_response = azimuth::HttpResponse::new(200, [("Content-Type", "application/json")], Some("{\"status\": \"success\"}"))
  let created_response = azimuth::HttpResponse::new(201, [("Content-Type", "application/json")], Some("{\"id\": 123}"))
  let bad_request_response = azimuth::HttpResponse::new(400, [("Content-Type", "application/json")], Some("{\"error\": \"Bad request\"}"))
  let unauthorized_response = azimuth::HttpResponse::new(401, [("Content-Type", "application/json")], Some("{\"error\": \"Unauthorized\"}"))
  let not_found_response = azimuth::HttpResponse::new(404, [("Content-Type", "application/json")], Some("{\"error\": \"Not found\"}"))
  let server_error_response = azimuth::HttpResponse::new(500, [("Content-Type", "application/json")], Some("{\"error\": \"Internal server error\"}"))
  
  // Verify response properties
  assert_eq(azimuth::HttpResponse::status_code(ok_response), 200)
  assert_eq(azimuth::HttpResponse::body(ok_response), Some("{\"status\": \"success\"}"))
  
  assert_eq(azimuth::HttpResponse::status_code(created_response), 201)
  assert_eq(azimuth::HttpResponse::body(created_response), Some("{\"id\": 123}"))
  
  assert_eq(azimuth::HttpResponse::status_code(bad_request_response), 400)
  assert_eq(azimuth::HttpResponse::body(bad_request_response), Some("{\"error\": \"Bad request\"}"))
  
  assert_eq(azimuth::HttpResponse::status_code(unauthorized_response), 401)
  assert_eq(azimuth::HttpResponse::body(unauthorized_response), Some("{\"error\": \"Unauthorized\"}"))
  
  assert_eq(azimuth::HttpResponse::status_code(not_found_response), 404)
  assert_eq(azimuth::HttpResponse::body(not_found_response), Some("{\"error\": \"Not found\"}"))
  
  assert_eq(azimuth::HttpResponse::status_code(server_error_response), 500)
  assert_eq(azimuth::HttpResponse::body(server_error_response), Some("{\"error\": \"Internal server error\"}"))
}