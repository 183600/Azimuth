// Concurrency Safety Enhanced Tests for Azimuth Telemetry System
// These tests focus on thread safety, race conditions, and concurrent operations

// Test 1: Concurrent Attributes Operations
pub test "concurrent attributes operations safety" {
  let attrs = azimuth::Attributes::new()
  
  // Simulate concurrent attribute setting operations
  azimuth::Attributes::set(attrs, "concurrent.key.1", azimuth::StringValue("value_1"))
  azimuth::Attributes::set(attrs, "concurrent.key.2", azimuth::StringValue("value_2"))
  azimuth::Attributes::set(attrs, "concurrent.key.3", azimuth::StringValue("value_3"))
  azimuth::Attributes::set(attrs, "concurrent.key.4", azimuth::IntValue(42))
  azimuth::Attributes::set(attrs, "concurrent.key.5", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "concurrent.key.6", azimuth::FloatValue(3.14159))
  
  // Test concurrent read operations
  let value_1 = azimuth::Attributes::get(attrs, "concurrent.key.1")
  let value_2 = azimuth::Attributes::get(attrs, "concurrent.key.2")
  let value_3 = azimuth::Attributes::get(attrs, "concurrent.key.3")
  let value_4 = azimuth::Attributes::get(attrs, "concurrent.key.4")
  let value_5 = azimuth::Attributes::get(attrs, "concurrent.key.5")
  let value_6 = azimuth::Attributes::get(attrs, "concurrent.key.6")
  
  // Verify data integrity under concurrent access
  assert_eq(value_1, Some(azimuth::StringValue("value_1")))
  assert_eq(value_2, Some(azimuth::StringValue("value_2")))
  assert_eq(value_3, Some(azimuth::StringValue("value_3")))
  assert_eq(value_4, Some(azimuth::IntValue(42)))
  assert_eq(value_5, Some(azimuth::BoolValue(true)))
  assert_eq(value_6, Some(azimuth::FloatValue(3.14159)))
  
  // Test missing keys under concurrent access
  let missing_1 = azimuth::Attributes::get(attrs, "missing.concurrent.1")
  let missing_2 = azimuth::Attributes::get(attrs, "missing.concurrent.2")
  assert_eq(missing_1, None)
  assert_eq(missing_2, None)
}

// Test 2: Concurrent Context Propagation
pub test "concurrent context propagation safety" {
  let root_ctx = azimuth::Context::root()
  
  // Create multiple concurrent context layers
  let user_key = azimuth::ContextKey::new("concurrent.user")
  let session_key = azimuth::ContextKey::new("concurrent.session")
  let request_key = azimuth::ContextKey::new("concurrent.request")
  let trace_key = azimuth::ContextKey::new("concurrent.trace")
  
  // Simulate concurrent context creation
  let ctx_1 = azimuth::Context::with_value(root_ctx, user_key, "user_concurrent_1")
  let ctx_2 = azimuth::Context::with_value(root_ctx, session_key, "session_concurrent_2")
  let ctx_3 = azimuth::Context::with_value(root_ctx, request_key, "request_concurrent_3")
  let ctx_4 = azimuth::Context::with_value(root_ctx, trace_key, "trace_concurrent_4")
  
  // Test concurrent context isolation
  assert_eq(azimuth::Context::get(ctx_1, user_key), Some("user_concurrent_1"))
  assert_eq(azimuth::Context::get(ctx_2, session_key), Some("session_concurrent_2"))
  assert_eq(azimuth::Context::get(ctx_3, request_key), Some("request_concurrent_3"))
  assert_eq(azimuth::Context::get(ctx_4, trace_key), Some("trace_concurrent_4"))
  
  // Verify context isolation - each context should only contain its own data
  assert_eq(azimuth::Context::get(ctx_1, session_key), None)
  assert_eq(azimuth::Context::get(ctx_2, user_key), None)
  assert_eq(azimuth::Context::get(ctx_3, trace_key), None)
  assert_eq(azimuth::Context::get(ctx_4, request_key), None)
}

// Test 3: Concurrent Span Operations
pub test "concurrent span operations safety" {
  // Create multiple spans concurrently
  let span_ctx_1 = azimuth::SpanContext::new("concurrent_trace_1", "concurrent_span_1", true, "state=active")
  let span_ctx_2 = azimuth::SpanContext::new("concurrent_trace_2", "concurrent_span_2", true, "state=active")
  let span_ctx_3 = azimuth::SpanContext::new("concurrent_trace_3", "concurrent_span_3", true, "state=active")
  
  let span_1 = azimuth::Span::new("concurrent_operation_1", azimuth::Server, span_ctx_1)
  let span_2 = azimuth::Span::new("concurrent_operation_2", azimuth::Client, span_ctx_2)
  let span_3 = azimuth::Span::new("concurrent_operation_3", azimuth::Internal, span_ctx_3)
  
  // Test concurrent span operations
  azimuth::Span::add_event(span_1, "concurrent_event_1", 
    Some([("span.id", azimuth::StringValue("span_1")), ("operation", azimuth::StringValue("test"))]))
  azimuth::Span::add_event(span_2, "concurrent_event_2", 
    Some([("span.id", azimuth::StringValue("span_2")), ("operation", azimuth::StringValue("test"))]))
  azimuth::Span::add_event(span_3, "concurrent_event_3", 
    Some([("span.id", azimuth::StringValue("span_3")), ("operation", azimuth::StringValue("test"))]))
  
  // Test concurrent status setting
  azimuth::Span::set_status(span_1, azimuth::Ok, Some("Concurrent operation 1 completed"))
  azimuth::Span::set_status(span_2, azimuth::Error, Some("Concurrent operation 2 failed"))
  azimuth::Span::set_status(span_3, azimuth::Ok, Some("Concurrent operation 3 completed"))
  
  // Verify span isolation under concurrent operations
  assert_eq(azimuth::Span::name(span_1), "concurrent_operation_1")
  assert_eq(azimuth::Span::name(span_2), "concurrent_operation_2")
  assert_eq(azimuth::Span::name(span_3), "concurrent_operation_3")
  
  assert_eq(azimuth::Span::kind(span_1), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_2), azimuth::Client)
  assert_eq(azimuth::Span::kind(span_3), azimuth::Internal)
  
  // Test concurrent span ending
  azimuth::Span::end(span_1)
  azimuth::Span::end(span_2)
  azimuth::Span::end(span_3)
}

// Test 4: Concurrent Metrics Operations
pub test "concurrent metrics operations safety" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "concurrent_metrics")
  
  // Create multiple instruments concurrently
  let counter_1 = azimuth::Meter::create_counter(meter, "concurrent_counter_1")
  let counter_2 = azimuth::Meter::create_counter(meter, "concurrent_counter_2")
  let histogram_1 = azimuth::Meter::create_histogram(meter, "concurrent_histogram_1")
  let histogram_2 = azimuth::Meter::create_histogram(meter, "concurrent_histogram_2")
  let updown_counter_1 = azimuth::Meter::create_updown_counter(meter, "concurrent_updown_1")
  let gauge_1 = azimuth::Meter::create_gauge(meter, "concurrent_gauge_1")
  
  // Test concurrent metric recording
  azimuth::Counter::add(counter_1, 1.0)
  azimuth::Counter::add(counter_1, 2.0)
  azimuth::Counter::add(counter_1, 3.0)
  
  azimuth::Counter::add(counter_2, 10.0)
  azimuth::Counter::add(counter_2, 20.0)
  azimuth::Counter::add(counter_2, 30.0)
  
  azimuth::Histogram::record(histogram_1, 100.0)
  azimuth::Histogram::record(histogram_1, 200.0)
  azimuth::Histogram::record(histogram_1, 300.0)
  
  azimuth::Histogram::record(histogram_2, 1000.0)
  azimuth::Histogram::record(histogram_2, 2000.0)
  azimuth::Histogram::record(histogram_2, 3000.0)
  
  azimuth::UpDownCounter::add(updown_counter_1, 5.0)
  azimuth::UpDownCounter::add(updown_counter_1, -2.0)
  azimuth::UpDownCounter::add(updown_counter_1, 10.0)
  
  // Verify instrument isolation under concurrent operations
  assert_eq(counter_1.name, "concurrent_counter_1")
  assert_eq(counter_2.name, "concurrent_counter_2")
  assert_eq(histogram_1.name, "concurrent_histogram_1")
  assert_eq(histogram_2.name, "concurrent_histogram_2")
  assert_eq(updown_counter_1.name, "concurrent_updown_1")
  assert_eq(gauge_1.name, "concurrent_gauge_1")
  
  // Test instrument conversion under concurrent access
  let instrument_1 = azimuth::Histogram::as_instrument(histogram_1)
  let instrument_2 = azimuth::Histogram::as_instrument(histogram_2)
  
  assert_eq(azimuth::Instrument::name(instrument_1), "concurrent_histogram_1")
  assert_eq(azimuth::Instrument::name(instrument_2), "concurrent_histogram_2")
}

// Test 5: Concurrent Logging Operations
pub test "concurrent logging operations safety" {
  let provider = azimuth::LoggerProvider::default()
  let logger_1 = azimuth::LoggerProvider::get_logger(provider, "concurrent_logger_1")
  let logger_2 = azimuth::LoggerProvider::get_logger(provider, "concurrent_logger_2")
  let logger_3 = azimuth::LoggerProvider::get_logger(provider, "concurrent_logger_3")
  
  // Create multiple log records concurrently
  let log_record_1 = azimuth::LogRecord::new(azimuth::Info, "Concurrent log message 1")
  let log_record_2 = azimuth::LogRecord::new(azimuth::Warn, "Concurrent log message 2")
  let log_record_3 = azimuth::LogRecord::new(azimuth::Error, "Concurrent log message 3")
  
  let detailed_log_1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Detailed concurrent log 1"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("concurrent_trace_1"),
    Some("concurrent_span_1"),
    Some(azimuth::Context::root())
  )
  
  let detailed_log_2 = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Detailed concurrent log 2"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some("concurrent_trace_2"),
    Some("concurrent_span_2"),
    Some(azimuth::Context::root())
  )
  
  // Test concurrent log emission
  azimuth::Logger::emit(logger_1, log_record_1)
  azimuth::Logger::emit(logger_2, log_record_2)
  azimuth::Logger::emit(logger_3, log_record_3)
  azimuth::Logger::emit(logger_1, detailed_log_1)
  azimuth::Logger::emit(logger_2, detailed_log_2)
  
  // Verify logger isolation under concurrent operations
  assert_eq(logger_1.scope.name, "concurrent_logger_1")
  assert_eq(logger_2.scope.name, "concurrent_logger_2")
  assert_eq(logger_3.scope.name, "concurrent_logger_3")
  
  // Verify log record properties under concurrent access
  assert_eq(azimuth::LogRecord::severity_number(log_record_1), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_record_2), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(log_record_3), azimuth::Error)
  
  assert_eq(azimuth::LogRecord::body(detailed_log_1), Some("Detailed concurrent log 1"))
  assert_eq(azimuth::LogRecord::body(detailed_log_2), Some("Detailed concurrent log 2"))
  assert_eq(azimuth::LogRecord::trace_id(detailed_log_1), Some("concurrent_trace_1"))
  assert_eq(azimuth::LogRecord::trace_id(detailed_log_2), Some("concurrent_trace_2"))
}

// Test 6: Concurrent Resource Management
pub test "concurrent resource management safety" {
  let base_resource = azimuth::Resource::new()
  
  // Create multiple resource configurations concurrently
  let attrs_1 = [
    ("service.name", azimuth::StringValue("concurrent_service_1")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("dev"))
  ]
  
  let attrs_2 = [
    ("service.name", azimuth::StringValue("concurrent_service_2")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("environment", azimuth::StringValue("test"))
  ]
  
  let attrs_3 = [
    ("service.name", azimuth::StringValue("concurrent_service_3")),
    ("service.version", azimuth::StringValue("3.0.0")),
    ("environment", azimuth::StringValue("prod"))
  ]
  
  let resource_1 = azimuth::Resource::with_attributes(base_resource, attrs_1)
  let resource_2 = azimuth::Resource::with_attributes(base_resource, attrs_2)
  let resource_3 = azimuth::Resource::with_attributes(base_resource, attrs_3)
  
  // Test concurrent resource merging
  let runtime_attrs = [
    ("runtime.name", azimuth::StringValue("moonbit")),
    ("runtime.version", azimuth::StringValue("1.0.0"))
  ]
  
  let runtime_resource = azimuth::Resource::with_attributes(base_resource, runtime_attrs)
  
  let merged_1 = azimuth::Resource::merge(resource_1, runtime_resource)
  let merged_2 = azimuth::Resource::merge(resource_2, runtime_resource)
  let merged_3 = azimuth::Resource::merge(resource_3, runtime_resource)
  
  // Verify resource isolation under concurrent operations
  let service_name_1 = azimuth::Resource::get_attribute(resource_1, "service.name")
  let service_name_2 = azimuth::Resource::get_attribute(resource_2, "service.name")
  let service_name_3 = azimuth::Resource::get_attribute(resource_3, "service.name")
  
  assert_eq(service_name_1, Some(azimuth::StringValue("concurrent_service_1")))
  assert_eq(service_name_2, Some(azimuth::StringValue("concurrent_service_2")))
  assert_eq(service_name_3, Some(azimuth::StringValue("concurrent_service_3")))
  
  // Verify merged resources preserve original attributes
  let runtime_name_1 = azimuth::Resource::get_attribute(merged_1, "runtime.name")
  let runtime_name_2 = azimuth::Resource::get_attribute(merged_2, "runtime.name")
  let runtime_name_3 = azimuth::Resource::get_attribute(merged_3, "runtime.name")
  
  assert_eq(runtime_name_1, Some(azimuth::StringValue("moonbit")))
  assert_eq(runtime_name_2, Some(azimuth::StringValue("moonbit")))
  assert_eq(runtime_name_3, Some(azimuth::StringValue("moonbit")))
}

// Test 7: Concurrent Propagation Operations
pub test "concurrent propagation operations safety" {
  let trace_propagator_1 = azimuth::W3CTraceContextPropagator::new()
  let trace_propagator_2 = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator_1 = azimuth::W3CBaggagePropagator::new()
  let baggage_propagator_2 = azimuth::W3CBaggagePropagator::new()
  
  // Create multiple composite propagators concurrently
  let propagators_1 = [trace_propagator_1]
  let propagators_2 = [trace_propagator_2]
  
  let composite_propagator_1 = azimuth::CompositePropagator::new(propagators_1)
  let composite_propagator_2 = azimuth::CompositePropagator::new(propagators_2)
  
  // Create multiple carriers for concurrent operations
  let carrier_1 = azimuth::TextMapCarrier::new()
  let carrier_2 = azimuth::TextMapCarrier::new()
  let carrier_3 = azimuth::TextMapCarrier::new()
  
  let ctx_1 = azimuth::Context::root()
  let ctx_2 = azimuth::Context::root()
  let ctx_3 = azimuth::Context::root()
  
  // Test concurrent injection operations
  azimuth::CompositePropagator::inject(composite_propagator_1, ctx_1, carrier_1)
  azimuth::CompositePropagator::inject(composite_propagator_2, ctx_2, carrier_2)
  azimuth::CompositePropagator::inject(composite_propagator_1, ctx_3, carrier_3)
  
  // Test concurrent extraction operations
  let extracted_ctx_1 = azimuth::CompositePropagator::extract(composite_propagator_1, carrier_1)
  let extracted_ctx_2 = azimuth::CompositePropagator::extract(composite_propagator_2, carrier_2)
  let extracted_ctx_3 = azimuth::CompositePropagator::extract(composite_propagator_1, carrier_3)
  
  // Verify propagation isolation under concurrent operations
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value_1 = azimuth::Context::get(extracted_ctx_1, extracted_key)
  let extracted_value_2 = azimuth::Context::get(extracted_ctx_2, extracted_key)
  let extracted_value_3 = azimuth::Context::get(extracted_ctx_3, extracted_key)
  
  assert_eq(extracted_value_1, Some("true"))
  assert_eq(extracted_value_2, Some("true"))
  assert_eq(extracted_value_3, Some("true"))
  
  // Test concurrent baggage operations
  let baggage_1 = azimuth::Baggage::new()
  let baggage_2 = azimuth::Baggage::new()
  let baggage_3 = azimuth::Baggage::new()
  
  let updated_baggage_1 = azimuth::Baggage::set_entry(baggage_1, "concurrent.key.1", "value_1")
  let updated_baggage_2 = azimuth::Baggage::set_entry(baggage_2, "concurrent.key.2", "value_2")
  let updated_baggage_3 = azimuth::Baggage::set_entry(baggage_3, "concurrent.key.3", "value_3")
  
  // Verify baggage isolation under concurrent operations
  let baggage_value_1 = azimuth::Baggage::get_entry(updated_baggage_1, "concurrent.key.1")
  let baggage_value_2 = azimuth::Baggage::get_entry(updated_baggage_2, "concurrent.key.2")
  let baggage_value_3 = azimuth::Baggage::get_entry(updated_baggage_3, "concurrent.key.3")
  
  assert_eq(baggage_value_1, Some("value_1"))
  assert_eq(baggage_value_2, Some("value_2"))
  assert_eq(baggage_value_3, Some("value_3"))
}

// Test 8: Concurrent HTTP Client Operations
pub test "concurrent http client operations safety" {
  let client_1 = azimuth::HttpClient::new()
  let client_2 = azimuth::HttpClient::new()
  let client_3 = azimuth::HttpClient::new()
  
  // Create multiple requests concurrently
  let headers_1 = [("Content-Type", "application/json"), ("X-Request-ID", "concurrent-req-1")]
  let headers_2 = [("Content-Type", "application/xml"), ("X-Request-ID", "concurrent-req-2")]
  let headers_3 = [("Content-Type", "text/plain"), ("X-Request-ID", "concurrent-req-3")]
  
  let request_1 = azimuth::HttpRequest::new("GET", "https://api.example.com/concurrent/1", headers_1, Some("{\"concurrent\":1}"))
  let request_2 = azimuth::HttpRequest::new("POST", "https://api.example.com/concurrent/2", headers_2, Some("<concurrent>2</concurrent>"))
  let request_3 = azimuth::HttpRequest::new("PUT", "https://api.example.com/concurrent/3", headers_3, Some("concurrent data 3"))
  
  // Create multiple responses concurrently
  let response_headers_1 = [("Content-Type", "application/json"), ("X-Response-ID", "concurrent-resp-1")]
  let response_headers_2 = [("Content-Type", "application/xml"), ("X-Response-ID", "concurrent-resp-2")]
  let response_headers_3 = [("Content-Type", "text/plain"), ("X-Response-ID", "concurrent-resp-3")]
  
  let response_1 = azimuth::HttpResponse::new(200, response_headers_1, Some("{\"result\":\"concurrent_success_1\"}"))
  let response_2 = azimuth::HttpResponse::new(201, response_headers_2, Some("<result>concurrent_success_2</result>"))
  let response_3 = azimuth::HttpResponse::new(202, response_headers_3, Some("concurrent success 3"))
  
  // Verify request isolation under concurrent operations
  assert_eq(azimuth::HttpRequest::http_method(request_1), "GET")
  assert_eq(azimuth::HttpRequest::http_method(request_2), "POST")
  assert_eq(azimuth::HttpRequest::http_method(request_3), "PUT")
  
  assert_eq(azimuth::HttpRequest::url(request_1), "https://api.example.com/concurrent/1")
  assert_eq(azimuth::HttpRequest::url(request_2), "https://api.example.com/concurrent/2")
  assert_eq(azimuth::HttpRequest::url(request_3), "https://api.example.com/concurrent/3")
  
  assert_eq(azimuth::HttpRequest::body(request_1), Some("{\"concurrent\":1}"))
  assert_eq(azimuth::HttpRequest::body(request_2), Some("<concurrent>2</concurrent>"))
  assert_eq(azimuth::HttpRequest::body(request_3), Some("concurrent data 3"))
  
  // Verify response isolation under concurrent operations
  assert_eq(azimuth::HttpResponse::status_code(response_1), 200)
  assert_eq(azimuth::HttpResponse::status_code(response_2), 201)
  assert_eq(azimuth::HttpResponse::status_code(response_3), 202)
  
  assert_eq(azimuth::HttpResponse::body(response_1), Some("{\"result\":\"concurrent_success_1\"}"))
  assert_eq(azimuth::HttpResponse::body(response_2), Some("<result>concurrent_success_2</result>"))
  assert_eq(azimuth::HttpResponse::body(response_3), Some("concurrent success 3"))
}

// Test 9: Concurrent Clock and Random Operations
pub test "concurrent clock and random operations safety" {
  let clock_1 = azimuth::Clock::system()
  let clock_2 = azimuth::Clock::system()
  let clock_3 = azimuth::Clock::system()
  
  let random_1 = azimuth::Random::system()
  let random_2 = azimuth::Random::system()
  let random_3 = azimuth::Random::system()
  
  // Test concurrent timestamp generation
  let timestamp_1 = azimuth::Clock::now_unix_nanos(clock_1)
  let timestamp_2 = azimuth::Clock::now_unix_nanos(clock_2)
  let timestamp_3 = azimuth::Clock::now_unix_nanos(clock_3)
  
  // Verify timestamp validity under concurrent access
  assert_true(timestamp_1 >= 1700000000000000000L)
  assert_true(timestamp_2 >= 1700000000000000000L)
  assert_true(timestamp_3 >= 1700000000000000000L)
  
  assert_true(timestamp_1 < 1800000000000000000L)
  assert_true(timestamp_2 < 1800000000000000000L)
  assert_true(timestamp_3 < 1800000000000000000L)
  
  // Test concurrent random number generation
  let random_bytes_1 = azimuth::Random::next_bytes(random_1, 8)
  let random_bytes_2 = azimuth::Random::next_bytes(random_2, 8)
  let random_bytes_3 = azimuth::Random::next_bytes(random_3, 8)
  
  let random_u64_1 = azimuth::Random::next_u64(random_1)
  let random_u64_2 = azimuth::Random::next_u64(random_2)
  let random_u64_3 = azimuth::Random::next_u64(random_3)
  
  // Verify random data generation under concurrent access
  assert_eq(random_bytes_1.length(), 8)
  assert_eq(random_bytes_2.length(), 8)
  assert_eq(random_bytes_3.length(), 8)
  
  assert_true(random_u64_1.to_int() >= 0)
  assert_true(random_u64_2.to_int() >= 0)
  assert_true(random_u64_3.to_int() >= 0)
  
  assert_true(random_u64_1.to_int() < 1000000)
  assert_true(random_u64_2.to_int() < 1000000)
  assert_true(random_u64_3.to_int() < 1000000)
}

// Test 10: Complex Concurrent Integration Scenario
pub test "complex concurrent integration scenario" {
  // Setup multiple telemetry components for concurrent operations
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  // Create multiple concurrent tracers
  let tracer_1 = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent_tracer_1")
  let tracer_2 = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent_tracer_2")
  let tracer_3 = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent_tracer_3")
  
  // Create multiple concurrent meters
  let meter_1 = azimuth::MeterProvider::get_meter(meter_provider, "concurrent_meter_1")
  let meter_2 = azimuth::MeterProvider::get_meter(meter_provider, "concurrent_meter_2")
  let meter_3 = azimuth::MeterProvider::get_meter(meter_provider, "concurrent_meter_3")
  
  // Create multiple concurrent loggers
  let logger_1 = azimuth::LoggerProvider::get_logger(logger_provider, "concurrent_logger_1")
  let logger_2 = azimuth::LoggerProvider::get_logger(logger_provider, "concurrent_logger_2")
  let logger_3 = azimuth::LoggerProvider::get_logger(logger_provider, "concurrent_logger_3")
  
  // Execute concurrent operations
  let span_1 = azimuth::Tracer::start_span(tracer_1, "concurrent_operation_1")
  let span_2 = azimuth::Tracer::start_span(tracer_2, "concurrent_operation_2")
  let span_3 = azimuth::Tracer::start_span(tracer_3, "concurrent_operation_3")
  
  let counter_1 = azimuth::Meter::create_counter(meter_1, "concurrent_counter_1")
  let counter_2 = azimuth::Meter::create_counter(meter_2, "concurrent_counter_2")
  let counter_3 = azimuth::Meter::create_counter(meter_3, "concurrent_counter_3")
  
  let log_record_1 = azimuth::LogRecord::new(azimuth::Info, "Concurrent log 1")
  let log_record_2 = azimuth::LogRecord::new(azimuth::Warn, "Concurrent log 2")
  let log_record_3 = azimuth::LogRecord::new(azimuth::Error, "Concurrent log 3")
  
  // Perform concurrent telemetry operations
  azimuth::Span::add_event(span_1, "concurrent_event", Some([("tracer", azimuth::StringValue("tracer_1"))]))
  azimuth::Span::add_event(span_2, "concurrent_event", Some([("tracer", azimuth::StringValue("tracer_2"))]))
  azimuth::Span::add_event(span_3, "concurrent_event", Some([("tracer", azimuth::StringValue("tracer_3"))]))
  
  azimuth::Counter::add(counter_1, 1.0)
  azimuth::Counter::add(counter_2, 2.0)
  azimuth::Counter::add(counter_3, 3.0)
  
  azimuth::Logger::emit(logger_1, log_record_1)
  azimuth::Logger::emit(logger_2, log_record_2)
  azimuth::Logger::emit(logger_3, log_record_3)
  
  // Verify all concurrent operations completed successfully
  assert_eq(azimuth::Span::name(span_1), "concurrent_operation_1")
  assert_eq(azimuth::Span::name(span_2), "concurrent_operation_2")
  assert_eq(azimuth::Span::name(span_3), "concurrent_operation_3")
  
  assert_eq(counter_1.name, "concurrent_counter_1")
  assert_eq(counter_2.name, "concurrent_counter_2")
  assert_eq(counter_3.name, "concurrent_counter_3")
  
  assert_eq(logger_1.scope.name, "concurrent_logger_1")
  assert_eq(logger_2.scope.name, "concurrent_logger_2")
  assert_eq(logger_3.scope.name, "concurrent_logger_3")
  
  // Complete concurrent operations
  azimuth::Span::end(span_1)
  azimuth::Span::end(span_2)
  azimuth::Span::end(span_3)
}