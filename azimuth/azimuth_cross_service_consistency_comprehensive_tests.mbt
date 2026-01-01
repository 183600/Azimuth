// Azimuth Cross-Service Consistency Comprehensive Test Suite
// This file contains comprehensive test cases for cross-service consistency in telemetry data

// Test 1: Cross-service trace consistency
pub test "cross-service trace consistency" {
  // Simulate a distributed trace across multiple services
  let root_trace_id = "cross-service-trace-12345"
  
  // Service A: Entry point
  let service_a_span_ctx = azimuth::SpanContext::new(root_trace_id, "service-a-span-67890", true, "service=entry-point,version=1.0")
  let service_a_span = azimuth::Span::new("service-a-operation", azimuth::Server, service_a_span_ctx)
  
  // Service B: Middle tier
  let service_b_span_ctx = azimuth::SpanContext::new(root_trace_id, "service-b-span-11111", true, "service=middle-tier,version=2.0")
  let service_b_span = azimuth::Span::new("service-b-operation", azimuth::Client, service_b_span_ctx)
  
  // Service C: Backend service
  let service_c_span_ctx = azimuth::SpanContext::new(root_trace_id, "service-c-span-22222", true, "service=backend,version=1.5")
  let service_c_span = azimuth::Span::new("service-c-operation", azimuth::Server, service_c_span_ctx)
  
  // Service D: Database service
  let service_d_span_ctx = azimuth::SpanContext::new(root_trace_id, "service-d-span-33333", true, "service=database,version=3.0")
  let service_d_span = azimuth::Span::new("service-d-operation", azimuth::Client, service_d_span_ctx)
  
  // Verify trace ID consistency across all services
  assert_eq(azimuth::SpanContext::trace_id(service_a_span_ctx), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_b_span_ctx), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_c_span_ctx), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_d_span_ctx), root_trace_id)
  
  // Verify span ID uniqueness across all services
  assert_neq(azimuth::SpanContext::span_id(service_a_span_ctx), azimuth::SpanContext::span_id(service_b_span_ctx))
  assert_neq(azimuth::SpanContext::span_id(service_b_span_ctx), azimuth::SpanContext::span_id(service_c_span_ctx))
  assert_neq(azimuth::SpanContext::span_id(service_c_span_ctx), azimuth::SpanContext::span_id(service_d_span_ctx))
  assert_neq(azimuth::SpanContext::span_id(service_d_span_ctx), azimuth::SpanContext::span_id(service_a_span_ctx))
  
  // Verify sampling consistency across all services
  assert_true(azimuth::SpanContext::is_sampled(service_a_span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(service_b_span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(service_c_span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(service_d_span_ctx))
  
  // Verify span context validity across all services
  assert_true(azimuth::SpanContext::is_valid(service_a_span_ctx))
  assert_true(azimuth::SpanContext::is_valid(service_b_span_ctx))
  assert_true(azimuth::SpanContext::is_valid(service_c_span_ctx))
  assert_true(azimuth::SpanContext::is_valid(service_d_span_ctx))
  
  // Test span kind consistency with service roles
  assert_eq(azimuth::Span::kind(service_a_span), azimuth::Server)  // Entry point service
  assert_eq(azimuth::Span::kind(service_b_span), azimuth::Client)  // Middle tier calling backend
  assert_eq(azimuth::Span::kind(service_c_span), azimuth::Server)  // Backend service
  assert_eq(azimuth::Span::kind(service_d_span), azimuth::Client)  // Database client
}

// Test 2: Cross-service baggage propagation consistency
pub test "cross-service baggage propagation consistency" {
  // Initialize baggage at service entry point
  let initial_baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(initial_baggage, "user.id", "user-12345")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "session-67890")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_session, "request.id", "request-11111")
  let baggage_with_tenant = azimuth::Baggage::set_entry(baggage_with_request, "tenant.id", "tenant-22222")
  
  // Service A: Entry point - sets initial baggage
  let service_a_baggage = baggage_with_tenant
  
  // Service B: Middle tier - receives and adds to baggage
  let service_b_baggage = azimuth::Baggage::set_entry(service_a_baggage, "service.b.entry", "2023-01-01T00:00:00Z")
  let service_b_baggage_final = azimuth::Baggage::set_entry(service_b_baggage, "service.b.operation", "process-data")
  
  // Service C: Backend service - receives and adds to baggage
  let service_c_baggage = azimuth::Baggage::set_entry(service_b_baggage_final, "service.c.entry", "2023-01-01T00:00:01Z")
  let service_c_baggage_final = azimuth::Baggage::set_entry(service_c_baggage, "service.c.operation", "validate-data")
  
  // Service D: Database service - receives and adds to baggage
  let service_d_baggage = azimuth::Baggage::set_entry(service_c_baggage_final, "service.d.entry", "2023-01-01T00:00:02Z")
  let service_d_baggage_final = azimuth::Baggage::set_entry(service_d_baggage, "service.d.operation", "query-database")
  
  // Verify baggage consistency across all services
  // Original baggage entries should be preserved
  assert_eq(azimuth::Baggage::get_entry(service_a_baggage, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage_final, "user.id"), Some("user-12345"))
  
  assert_eq(azimuth::Baggage::get_entry(service_a_baggage, "session.id"), Some("session-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "session.id"), Some("session-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "session.id"), Some("session-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage_final, "session.id"), Some("session-67890"))
  
  assert_eq(azimuth::Baggage::get_entry(service_a_baggage, "request.id"), Some("request-11111"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "request.id"), Some("request-11111"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "request.id"), Some("request-11111"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage_final, "request.id"), Some("request-11111"))
  
  assert_eq(azimuth::Baggage::get_entry(service_a_baggage, "tenant.id"), Some("tenant-22222"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "tenant.id"), Some("tenant-22222"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "tenant.id"), Some("tenant-22222"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage_final, "tenant.id"), Some("tenant-22222"))
  
  // Service-specific baggage entries should be present in downstream services
  assert_eq(azimuth::Baggage::get_entry(service_a_baggage, "service.b.entry"), None)  // Not set yet
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "service.b.entry"), Some("2023-01-01T00:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "service.b.entry"), Some("2023-01-01T00:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage_final, "service.b.entry"), Some("2023-01-01T00:00:00Z"))
  
  assert_eq(azimuth::Baggage::get_entry(service_a_baggage, "service.c.entry"), None)  // Not set yet
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "service.c.entry"), None)  // Not set yet
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "service.c.entry"), Some("2023-01-01T00:00:01Z"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage_final, "service.c.entry"), Some("2023-01-01T00:00:01Z"))
  
  assert_eq(azimuth::Baggage::get_entry(service_a_baggage, "service.d.entry"), None)  // Not set yet
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "service.d.entry"), None)  // Not set yet
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "service.d.entry"), None)  // Not set yet
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage_final, "service.d.entry"), Some("2023-01-01T00:00:02Z"))
}

// Test 3: Cross-service context propagation consistency
pub test "cross-service context propagation consistency" {
  // Initialize context at service entry point
  let root_ctx = azimuth::Context::root()
  
  // Service A: Entry point - sets initial context
  let user_key = azimuth::ContextKey::new("user.id")
  let session_key = azimuth::ContextKey::new("session.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let tenant_key = azimuth::ContextKey::new("tenant.id")
  
  let service_a_ctx = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  let service_a_ctx_with_session = azimuth::Context::with_value(service_a_ctx, session_key, "session-67890")
  let service_a_ctx_with_request = azimuth::Context::with_value(service_a_ctx_with_session, request_key, "request-11111")
  let service_a_ctx_final = azimuth::Context::with_value(service_a_ctx_with_request, tenant_key, "tenant-22222")
  
  // Service B: Middle tier - receives and adds to context
  let service_b_key = azimuth::ContextKey::new("service.b.operation")
  let service_b_timestamp_key = azimuth::ContextKey::new("service.b.timestamp")
  
  let service_b_ctx = azimuth::Context::with_value(service_a_ctx_final, service_b_key, "process-data")
  let service_b_ctx_final = azimuth::Context::with_value(service_b_ctx, service_b_timestamp_key, "2023-01-01T00:00:00Z")
  
  // Service C: Backend service - receives and adds to context
  let service_c_key = azimuth::ContextKey::new("service.c.operation")
  let service_c_timestamp_key = azimuth::ContextKey::new("service.c.timestamp")
  
  let service_c_ctx = azimuth::Context::with_value(service_b_ctx_final, service_c_key, "validate-data")
  let service_c_ctx_final = azimuth::Context::with_value(service_c_ctx, service_c_timestamp_key, "2023-01-01T00:00:01Z")
  
  // Service D: Database service - receives and adds to context
  let service_d_key = azimuth::ContextKey::new("service.d.operation")
  let service_d_timestamp_key = azimuth::ContextKey::new("service.d.timestamp")
  
  let service_d_ctx = azimuth::Context::with_value(service_c_ctx_final, service_d_key, "query-database")
  let service_d_ctx_final = azimuth::Context::with_value(service_d_ctx, service_d_timestamp_key, "2023-01-01T00:00:02Z")
  
  // Verify context consistency across all services
  // Original context values should be preserved
  assert_eq(azimuth::Context::get(service_a_ctx_final, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(service_b_ctx_final, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, user_key), Some("user-12345"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, session_key), Some("session-67890"))
  assert_eq(azimuth::Context::get(service_b_ctx_final, session_key), Some("session-67890"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, session_key), Some("session-67890"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, session_key), Some("session-67890"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, request_key), Some("request-11111"))
  assert_eq(azimuth::Context::get(service_b_ctx_final, request_key), Some("request-11111"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, request_key), Some("request-11111"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, request_key), Some("request-11111"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, tenant_key), Some("tenant-22222"))
  assert_eq(azimuth::Context::get(service_b_ctx_final, tenant_key), Some("tenant-22222"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, tenant_key), Some("tenant-22222"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, tenant_key), Some("tenant-22222"))
  
  // Service-specific context values should be present in downstream services
  assert_eq(azimuth::Context::get(service_a_ctx_final, service_b_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_b_ctx_final, service_b_key), Some("process-data"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, service_b_key), Some("process-data"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, service_b_key), Some("process-data"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, service_c_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_b_ctx_final, service_c_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_c_ctx_final, service_c_key), Some("validate-data"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, service_c_key), Some("validate-data"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, service_d_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_b_ctx_final, service_d_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_c_ctx_final, service_d_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_d_ctx_final, service_d_key), Some("query-database"))
}

// Test 4: Cross-service metrics consistency
pub test "cross-service metrics consistency" {
  // Create meter providers for each service
  let service_a_provider = azimuth::MeterProvider::default()
  let service_b_provider = azimuth::MeterProvider::default()
  let service_c_provider = azimuth::MeterProvider::default()
  let service_d_provider = azimuth::MeterProvider::default()
  
  // Create meters for each service
  let service_a_meter = azimuth::MeterProvider::get_meter(service_a_provider, "service-a-meter")
  let service_b_meter = azimuth::MeterProvider::get_meter(service_b_provider, "service-b-meter")
  let service_c_meter = azimuth::MeterProvider::get_meter(service_c_provider, "service-c-meter")
  let service_d_meter = azimuth::MeterProvider::get_meter(service_d_provider, "service-d-meter")
  
  // Create consistent metric names across services
  let request_counter_name = "http.requests.total"
  let response_time_histogram_name = "http.response.time"
  let error_counter_name = "http.errors.total"
  let active_connections_gauge_name = "http.connections.active"
  
  // Service A: Entry point metrics
  let service_a_request_counter = azimuth::Meter::create_counter(service_a_meter, request_counter_name, Some("Total HTTP requests"), Some("count"))
  let service_a_response_time = azimuth::Meter::create_histogram(service_a_meter, response_time_histogram_name, Some("HTTP response time"), Some("ms"))
  let service_a_error_counter = azimuth::Meter::create_counter(service_a_meter, error_counter_name, Some("Total HTTP errors"), Some("count"))
  let service_a_active_connections = azimuth::Meter::create_gauge(service_a_meter, active_connections_gauge_name, Some("Active HTTP connections"), Some("connections"))
  
  // Service B: Middle tier metrics
  let service_b_request_counter = azimuth::Meter::create_counter(service_b_meter, request_counter_name, Some("Total HTTP requests"), Some("count"))
  let service_b_response_time = azimuth::Meter::create_histogram(service_b_meter, response_time_histogram_name, Some("HTTP response time"), Some("ms"))
  let service_b_error_counter = azimuth::Meter::create_counter(service_b_meter, error_counter_name, Some("Total HTTP errors"), Some("count"))
  let service_b_active_connections = azimuth::Meter::create_gauge(service_b_meter, active_connections_gauge_name, Some("Active HTTP connections"), Some("connections"))
  
  // Service C: Backend service metrics
  let service_c_request_counter = azimuth::Meter::create_counter(service_c_meter, request_counter_name, Some("Total HTTP requests"), Some("count"))
  let service_c_response_time = azimuth::Meter::create_histogram(service_c_meter, response_time_histogram_name, Some("HTTP response time"), Some("ms"))
  let service_c_error_counter = azimuth::Meter::create_counter(service_c_meter, error_counter_name, Some("Total HTTP errors"), Some("count"))
  let service_c_active_connections = azimuth::Meter::create_gauge(service_c_meter, active_connections_gauge_name, Some("Active HTTP connections"), Some("connections"))
  
  // Service D: Database service metrics
  let service_d_request_counter = azimuth::Meter::create_counter(service_d_meter, request_counter_name, Some("Total HTTP requests"), Some("count"))
  let service_d_response_time = azimuth::Meter::create_histogram(service_d_meter, response_time_histogram_name, Some("HTTP response time"), Some("ms"))
  let service_d_error_counter = azimuth::Meter::create_counter(service_d_meter, error_counter_name, Some("Total HTTP errors"), Some("count"))
  let service_d_active_connections = azimuth::Meter::create_gauge(service_d_meter, active_connections_gauge_name, Some("Active HTTP connections"), Some("connections"))
  
  // Verify metric name consistency across all services
  assert_eq(service_a_request_counter.name, request_counter_name)
  assert_eq(service_b_request_counter.name, request_counter_name)
  assert_eq(service_c_request_counter.name, request_counter_name)
  assert_eq(service_d_request_counter.name, request_counter_name)
  
  assert_eq(service_a_response_time.name, response_time_histogram_name)
  assert_eq(service_b_response_time.name, response_time_histogram_name)
  assert_eq(service_c_response_time.name, response_time_histogram_name)
  assert_eq(service_d_response_time.name, response_time_histogram_name)
  
  assert_eq(service_a_error_counter.name, error_counter_name)
  assert_eq(service_b_error_counter.name, error_counter_name)
  assert_eq(service_c_error_counter.name, error_counter_name)
  assert_eq(service_d_error_counter.name, error_counter_name)
  
  assert_eq(service_a_active_connections.name, active_connections_gauge_name)
  assert_eq(service_b_active_connections.name, active_connections_gauge_name)
  assert_eq(service_c_active_connections.name, active_connections_gauge_name)
  assert_eq(service_d_active_connections.name, active_connections_gauge_name)
  
  // Verify metric description consistency across all services
  assert_eq(service_a_request_counter.description, Some("Total HTTP requests"))
  assert_eq(service_b_request_counter.description, Some("Total HTTP requests"))
  assert_eq(service_c_request_counter.description, Some("Total HTTP requests"))
  assert_eq(service_d_request_counter.description, Some("Total HTTP requests"))
  
  assert_eq(service_a_response_time.description, Some("HTTP response time"))
  assert_eq(service_b_response_time.description, Some("HTTP response time"))
  assert_eq(service_c_response_time.description, Some("HTTP response time"))
  assert_eq(service_d_response_time.description, Some("HTTP response time"))
  
  assert_eq(service_a_error_counter.description, Some("Total HTTP errors"))
  assert_eq(service_b_error_counter.description, Some("Total HTTP errors"))
  assert_eq(service_c_error_counter.description, Some("Total HTTP errors"))
  assert_eq(service_d_error_counter.description, Some("Total HTTP errors"))
  
  assert_eq(service_a_active_connections.description, Some("Active HTTP connections"))
  assert_eq(service_b_active_connections.description, Some("Active HTTP connections"))
  assert_eq(service_c_active_connections.description, Some("Active HTTP connections"))
  assert_eq(service_d_active_connections.description, Some("Active HTTP connections"))
  
  // Verify metric unit consistency across all services
  assert_eq(service_a_request_counter.unit, Some("count"))
  assert_eq(service_b_request_counter.unit, Some("count"))
  assert_eq(service_c_request_counter.unit, Some("count"))
  assert_eq(service_d_request_counter.unit, Some("count"))
  
  assert_eq(service_a_response_time.unit, Some("ms"))
  assert_eq(service_b_response_time.unit, Some("ms"))
  assert_eq(service_c_response_time.unit, Some("ms"))
  assert_eq(service_d_response_time.unit, Some("ms"))
  
  assert_eq(service_a_error_counter.unit, Some("count"))
  assert_eq(service_b_error_counter.unit, Some("count"))
  assert_eq(service_c_error_counter.unit, Some("count"))
  assert_eq(service_d_error_counter.unit, Some("count"))
  
  assert_eq(service_a_active_connections.unit, Some("connections"))
  assert_eq(service_b_active_connections.unit, Some("connections"))
  assert_eq(service_c_active_connections.unit, Some("connections"))
  assert_eq(service_d_active_connections.unit, Some("connections"))
  
  // Test metric recording with consistent values
  azimuth::Counter::add(service_a_request_counter, 100.0)
  azimuth::Counter::add(service_b_request_counter, 95.0)
  azimuth::Counter::add(service_c_request_counter, 90.0)
  azimuth::Counter::add(service_d_request_counter, 85.0)
  
  azimuth::Histogram::record(service_a_response_time, 50.0)
  azimuth::Histogram::record(service_b_response_time, 75.0)
  azimuth::Histogram::record(service_c_response_time, 100.0)
  azimuth::Histogram::record(service_d_response_time, 125.0)
  
  azimuth::Counter::add(service_a_error_counter, 5.0)
  azimuth::Counter::add(service_b_error_counter, 3.0)
  azimuth::Counter::add(service_c_error_counter, 2.0)
  azimuth::Counter::add(service_d_error_counter, 1.0)
  
  azimuth::UpDownCounter::add(service_a_active_connections, 10.0)
  azimuth::UpDownCounter::add(service_b_active_connections, 8.0)
  azimuth::UpDownCounter::add(service_c_active_connections, 6.0)
  azimuth::UpDownCounter::add(service_d_active_connections, 4.0)
}

// Test 5: Cross-service logging consistency
pub test "cross-service logging consistency" {
  // Create logger providers for each service
  let service_a_provider = azimuth::LoggerProvider::default()
  let service_b_provider = azimuth::LoggerProvider::default()
  let service_c_provider = azimuth::LoggerProvider::default()
  let service_d_provider = azimuth::LoggerProvider::default()
  
  // Create loggers for each service
  let service_a_logger = azimuth::LoggerProvider::get_logger(service_a_provider, "service-a-logger")
  let service_b_logger = azimuth::LoggerProvider::get_logger(service_b_provider, "service-b-logger")
  let service_c_logger = azimuth::LoggerProvider::get_logger(service_c_provider, "service-c-logger")
  let service_d_logger = azimuth::LoggerProvider::get_logger(service_d_provider, "service-d-logger")
  
  // Create consistent log messages across services
  let request_received_msg = "Request received"
  let processing_msg = "Processing request"
  let request_completed_msg = "Request completed"
  let error_msg = "Error occurred"
  
  // Create log records with consistent structure
  let service_a_request_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some(request_received_msg),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("cross-service-trace-12345"),
    Some("service-a-span-67890"),
    Some(azimuth::Context::root())
  )
  
  let service_b_processing_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some(processing_msg),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some("cross-service-trace-12345"),
    Some("service-b-span-11111"),
    Some(azimuth::Context::root())
  )
  
  let service_c_processing_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some(processing_msg),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000004L),
    Some(1735689600000000005L),
    Some("cross-service-trace-12345"),
    Some("service-c-span-22222"),
    Some(azimuth::Context::root())
  )
  
  let service_d_processing_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some(processing_msg),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000006L),
    Some(1735689600000000007L),
    Some("cross-service-trace-12345"),
    Some("service-d-span-33333"),
    Some(azimuth::Context::root())
  )
  
  let service_c_completed_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some(request_completed_msg),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000008L),
    Some(1735689600000000009L),
    Some("cross-service-trace-12345"),
    Some("service-c-span-22222"),
    Some(azimuth::Context::root())
  )
  
  let service_b_completed_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some(request_completed_msg),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000010L),
    Some(1735689600000000011L),
    Some("cross-service-trace-12345"),
    Some("service-b-span-11111"),
    Some(azimuth::Context::root())
  )
  
  let service_a_completed_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some(request_completed_msg),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000012L),
    Some(1735689600000000013L),
    Some("cross-service-trace-12345"),
    Some("service-a-span-67890"),
    Some(azimuth::Context::root())
  )
  
  // Create error log records with consistent structure
  let service_d_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some(error_msg),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000014L),
    Some(1735689600000000015L),
    Some("cross-service-trace-12345"),
    Some("service-d-span-33333"),
    Some(azimuth::Context::root())
  )
  
  // Verify trace ID consistency across all log records
  assert_eq(azimuth::LogRecord::trace_id(service_a_request_log), Some("cross-service-trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(service_b_processing_log), Some("cross-service-trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(service_c_processing_log), Some("cross-service-trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(service_d_processing_log), Some("cross-service-trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(service_c_completed_log), Some("cross-service-trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(service_b_completed_log), Some("cross-service-trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(service_a_completed_log), Some("cross-service-trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(service_d_error_log), Some("cross-service-trace-12345"))
  
  // Verify span ID consistency within each service
  assert_eq(azimuth::LogRecord::span_id(service_a_request_log), Some("service-a-span-67890"))
  assert_eq(azimuth::LogRecord::span_id(service_a_completed_log), Some("service-a-span-67890"))
  
  assert_eq(azimuth::LogRecord::span_id(service_b_processing_log), Some("service-b-span-11111"))
  assert_eq(azimuth::LogRecord::span_id(service_b_completed_log), Some("service-b-span-11111"))
  
  assert_eq(azimuth::LogRecord::span_id(service_c_processing_log), Some("service-c-span-22222"))
  assert_eq(azimuth::LogRecord::span_id(service_c_completed_log), Some("service-c-span-22222"))
  
  assert_eq(azimuth::LogRecord::span_id(service_d_processing_log), Some("service-d-span-33333"))
  assert_eq(azimuth::LogRecord::span_id(service_d_error_log), Some("service-d-span-33333"))
  
  // Verify severity level consistency
  assert_eq(azimuth::LogRecord::severity_number(service_a_request_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(service_b_processing_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(service_c_processing_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(service_d_processing_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(service_c_completed_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(service_b_completed_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(service_a_completed_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(service_d_error_log), azimuth::Error)
  
  // Verify log message consistency
  assert_eq(azimuth::LogRecord::body(service_a_request_log), Some(request_received_msg))
  assert_eq(azimuth::LogRecord::body(service_b_processing_log), Some(processing_msg))
  assert_eq(azimuth::LogRecord::body(service_c_processing_log), Some(processing_msg))
  assert_eq(azimuth::LogRecord::body(service_d_processing_log), Some(processing_msg))
  assert_eq(azimuth::LogRecord::body(service_c_completed_log), Some(request_completed_msg))
  assert_eq(azimuth::LogRecord::body(service_b_completed_log), Some(request_completed_msg))
  assert_eq(azimuth::LogRecord::body(service_a_completed_log), Some(request_completed_msg))
  assert_eq(azimuth::LogRecord::body(service_d_error_log), Some(error_msg))
  
  // Emit log records from each service
  azimuth::Logger::emit(service_a_logger, service_a_request_log)
  azimuth::Logger::emit(service_b_logger, service_b_processing_log)
  azimuth::Logger::emit(service_c_logger, service_c_processing_log)
  azimuth::Logger::emit(service_d_logger, service_d_processing_log)
  azimuth::Logger::emit(service_c_logger, service_c_completed_log)
  azimuth::Logger::emit(service_b_logger, service_b_completed_log)
  azimuth::Logger::emit(service_a_logger, service_a_completed_log)
  azimuth::Logger::emit(service_d_logger, service_d_error_log)
}

// Test 6: Cross-service resource consistency
pub test "cross-service resource consistency" {
  // Create resources for each service
  let empty_resource = azimuth::Resource::new()
  
  // Service A: Entry point resource
  let service_a_resource = azimuth::Resource::with_attributes(empty_resource, [
    ("service.name", azimuth::StringValue("service-a")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("service-a-instance-12345")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("service-a-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("process.id", azimuth::IntValue(1001)),
    ("process.name", azimuth::StringValue("service-a-process")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ])
  
  // Service B: Middle tier resource
  let service_b_resource = azimuth::Resource::with_attributes(empty_resource, [
    ("service.name", azimuth::StringValue("service-b")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("service.instance.id", azimuth::StringValue("service-b-instance-23456")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("service-b-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("process.id", azimuth::IntValue(1002)),
    ("process.name", azimuth::StringValue("service-b-process")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ])
  
  // Service C: Backend service resource
  let service_c_resource = azimuth::Resource::with_attributes(empty_resource, [
    ("service.name", azimuth::StringValue("service-c")),
    ("service.version", azimuth::StringValue("1.5.0")),
    ("service.instance.id", azimuth::StringValue("service-c-instance-34567")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("service-c-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("process.id", azimuth::IntValue(1003)),
    ("process.name", azimuth::StringValue("service-c-process")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ])
  
  // Service D: Database service resource
  let service_d_resource = azimuth::Resource::with_attributes(empty_resource, [
    ("service.name", azimuth::StringValue("service-d")),
    ("service.version", azimuth::StringValue("3.0.0")),
    ("service.instance.id", azimuth::StringValue("service-d-instance-45678")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("service-d-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("process.id", azimuth::IntValue(1004)),
    ("process.name", azimuth::StringValue("service-d-process")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ])
  
  // Verify consistent attributes across all services
  // Deployment environment should be consistent
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  
  // OS type should be consistent
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "os.type"), Some(azimuth::StringValue("linux")))
  
  // OS version should be consistent
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "os.version"), Some(azimuth::StringValue("5.15.0")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "os.version"), Some(azimuth::StringValue("5.15.0")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "os.version"), Some(azimuth::StringValue("5.15.0")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "os.version"), Some(azimuth::StringValue("5.15.0")))
  
  // Telemetry SDK name should be consistent
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  
  // Telemetry SDK version should be consistent
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
  
  // Verify service-specific attributes are unique per service
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "service.name"), Some(azimuth::StringValue("service-a")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "service.name"), Some(azimuth::StringValue("service-b")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "service.name"), Some(azimuth::StringValue("service-c")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "service.name"), Some(azimuth::StringValue("service-d")))
  
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "service.version"), Some(azimuth::StringValue("1.5.0")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "service.version"), Some(azimuth::StringValue("3.0.0")))
  
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "service.instance.id"), Some(azimuth::StringValue("service-a-instance-12345")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "service.instance.id"), Some(azimuth::StringValue("service-b-instance-23456")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "service.instance.id"), Some(azimuth::StringValue("service-c-instance-34567")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "service.instance.id"), Some(azimuth::StringValue("service-d-instance-45678")))
  
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "host.name"), Some(azimuth::StringValue("service-a-host")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "host.name"), Some(azimuth::StringValue("service-b-host")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "host.name"), Some(azimuth::StringValue("service-c-host")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "host.name"), Some(azimuth::StringValue("service-d-host")))
  
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "process.id"), Some(azimuth::IntValue(1001)))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "process.id"), Some(azimuth::IntValue(1002)))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "process.id"), Some(azimuth::IntValue(1003)))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "process.id"), Some(azimuth::IntValue(1004)))
  
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "process.name"), Some(azimuth::StringValue("service-a-process")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "process.name"), Some(azimuth::StringValue("service-b-process")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "process.name"), Some(azimuth::StringValue("service-c-process")))
  assert_eq(azimuth::Resource::get_attribute(service_d_resource, "process.name"), Some(azimuth::StringValue("service-d-process")))
}

// Test 7: Cross-service propagator consistency
pub test "cross-service propagator consistency" {
  // Create propagators for each service
  let service_a_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let service_a_baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let service_a_composite_propagator = azimuth::CompositePropagator::new([service_a_trace_propagator, service_a_baggage_propagator])
  
  let service_b_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let service_b_baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let service_b_composite_propagator = azimuth::CompositePropagator::new([service_b_trace_propagator, service_b_baggage_propagator])
  
  let service_c_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let service_c_baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let service_c_composite_propagator = azimuth::CompositePropagator::new([service_c_trace_propagator, service_c_baggage_propagator])
  
  let service_d_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let service_d_baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let service_d_composite_propagator = azimuth::CompositePropagator::new([service_d_trace_propagator, service_d_baggage_propagator])
  
  // Create context with consistent data
  let root_ctx = azimuth::Context::root()
  let user_key = azimuth::ContextKey::new("user.id")
  let session_key = azimuth::ContextKey::new("session.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let tenant_key = azimuth::ContextKey::new("tenant.id")
  
  let service_a_ctx = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  let service_a_ctx_with_session = azimuth::Context::with_value(service_a_ctx, session_key, "session-67890")
  let service_a_ctx_with_request = azimuth::Context::with_value(service_a_ctx_with_session, request_key, "request-11111")
  let service_a_ctx_final = azimuth::Context::with_value(service_a_ctx_with_request, tenant_key, "tenant-22222")
  
  // Service A: Inject context into carrier
  let service_a_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(service_a_composite_propagator, service_a_ctx_final, service_a_carrier)
  
  // Service B: Extract context from carrier
  let service_b_ctx = azimuth::CompositePropagator::extract(service_b_composite_propagator, service_a_carrier)
  
  // Add service B specific context
  let service_b_key = azimuth::ContextKey::new("service.b.operation")
  let service_b_ctx_final = azimuth::Context::with_value(service_b_ctx, service_b_key, "process-data")
  
  // Service B: Inject context into carrier
  let service_b_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(service_b_composite_propagator, service_b_ctx_final, service_b_carrier)
  
  // Service C: Extract context from carrier
  let service_c_ctx = azimuth::CompositePropagator::extract(service_c_composite_propagator, service_b_carrier)
  
  // Add service C specific context
  let service_c_key = azimuth::ContextKey::new("service.c.operation")
  let service_c_ctx_final = azimuth::Context::with_value(service_c_ctx, service_c_key, "validate-data")
  
  // Service C: Inject context into carrier
  let service_c_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(service_c_composite_propagator, service_c_ctx_final, service_c_carrier)
  
  // Service D: Extract context from carrier
  let service_d_ctx = azimuth::CompositePropagator::extract(service_d_composite_propagator, service_c_carrier)
  
  // Add service D specific context
  let service_d_key = azimuth::ContextKey::new("service.d.operation")
  let service_d_ctx_final = azimuth::Context::with_value(service_d_ctx, service_d_key, "query-database")
  
  // Verify context consistency across all services
  // Original context values should be preserved
  assert_eq(azimuth::Context::get(service_a_ctx_final, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(service_b_ctx_final, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, user_key), Some("user-12345"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, session_key), Some("session-67890"))
  assert_eq(azimuth::Context::get(service_b_ctx_final, session_key), Some("session-67890"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, session_key), Some("session-67890"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, session_key), Some("session-67890"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, request_key), Some("request-11111"))
  assert_eq(azimuth::Context::get(service_b_ctx_final, request_key), Some("request-11111"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, request_key), Some("request-11111"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, request_key), Some("request-11111"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, tenant_key), Some("tenant-22222"))
  assert_eq(azimuth::Context::get(service_b_ctx_final, tenant_key), Some("tenant-22222"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, tenant_key), Some("tenant-22222"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, tenant_key), Some("tenant-22222"))
  
  // Service-specific context values should be present in downstream services
  assert_eq(azimuth::Context::get(service_a_ctx_final, service_b_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_b_ctx_final, service_b_key), Some("process-data"))
  assert_eq(azimuth::Context::get(service_c_ctx_final, service_b_key), Some("process-data"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, service_b_key), Some("process-data"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, service_c_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_b_ctx_final, service_c_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_c_ctx_final, service_c_key), Some("validate-data"))
  assert_eq(azimuth::Context::get(service_d_ctx_final, service_c_key), Some("validate-data"))
  
  assert_eq(azimuth::Context::get(service_a_ctx_final, service_d_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_b_ctx_final, service_d_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_c_ctx_final, service_d_key), None)  // Not set yet
  assert_eq(azimuth::Context::get(service_d_ctx_final, service_d_key), Some("query-database"))
}

// Test 8: Cross-service HTTP consistency
pub test "cross-service http consistency" {
  // Create HTTP clients for each service
  let service_a_client = azimuth::HttpClient::new()
  let service_b_client = azimuth::HttpClient::new()
  let service_c_client = azimuth::HttpClient::new()
  let service_d_client = azimuth::HttpClient::new()
  
  // Service A: Create request to Service B
  let service_a_request = azimuth::HttpRequest::new(
    "POST",
    "https://service-b.example.com/api/process",
    [
      ("Content-Type", "application/json"),
      ("X-Trace-ID", "cross-service-trace-12345"),
      ("X-Request-ID", "request-11111"),
      ("X-User-ID", "user-12345"),
      ("X-Session-ID", "session-67890"),
      ("X-Tenant-ID", "tenant-22222")
    ],
    Some("{\"data\": \"request-data\"}")
  )
  
  // Service B: Create request to Service C
  let service_b_request = azimuth::HttpRequest::new(
    "POST",
    "https://service-c.example.com/api/validate",
    [
      ("Content-Type", "application/json"),
      ("X-Trace-ID", "cross-service-trace-12345"),
      ("X-Request-ID", "request-11111"),
      ("X-User-ID", "user-12345"),
      ("X-Session-ID", "session-67890"),
      ("X-Tenant-ID", "tenant-22222"),
      ("X-Service-B-Operation", "process-data")
    ],
    Some("{\"data\": \"processed-data\"}")
  )
  
  // Service C: Create request to Service D
  let service_c_request = azimuth::HttpRequest::new(
    "POST",
    "https://service-d.example.com/api/query",
    [
      ("Content-Type", "application/json"),
      ("X-Trace-ID", "cross-service-trace-12345"),
      ("X-Request-ID", "request-11111"),
      ("X-User-ID", "user-12345"),
      ("X-Session-ID", "session-67890"),
      ("X-Tenant-ID", "tenant-22222"),
      ("X-Service-B-Operation", "process-data"),
      ("X-Service-C-Operation", "validate-data")
    ],
    Some("{\"data\": \"validated-data\"}")
  )
  
  // Create responses for each service
  let service_a_response = azimuth::HttpResponse::new(
    200,
    [
      ("Content-Type", "application/json"),
      ("X-Trace-ID", "cross-service-trace-12345"),
      ("X-Request-ID", "request-11111"),
      ("X-Service-A-Operation", "handle-request")
    ],
    Some("{\"status\": \"success\", \"data\": \"response-data\"}")
  )
  
  let service_b_response = azimuth::HttpResponse::new(
    200,
    [
      ("Content-Type", "application/json"),
      ("X-Trace-ID", "cross-service-trace-12345"),
      ("X-Request-ID", "request-11111"),
      ("X-Service-B-Operation", "process-data")
    ],
    Some("{\"status\": \"success\", \"data\": \"processed-response-data\"}")
  )
  
  let service_c_response = azimuth::HttpResponse::new(
    200,
    [
      ("Content-Type", "application/json"),
      ("X-Trace-ID", "cross-service-trace-12345"),
      ("X-Request-ID", "request-11111"),
      ("X-Service-C-Operation", "validate-data")
    ],
    Some("{\"status\": \"success\", \"data\": \"validated-response-data\"}")
  )
  
  let service_d_response = azimuth::HttpResponse::new(
    200,
    [
      ("Content-Type", "application/json"),
      ("X-Trace-ID", "cross-service-trace-12345"),
      ("X-Request-ID", "request-11111"),
      ("X-Service-D-Operation", "query-database")
    ],
    Some("{\"status\": \"success\", \"data\": \"query-response-data\"}")
  )
  
  // Verify HTTP method consistency
  assert_eq(azimuth::HttpRequest::http_method(service_a_request), "POST")
  assert_eq(azimuth::HttpRequest::http_method(service_b_request), "POST")
  assert_eq(azimuth::HttpRequest::http_method(service_c_request), "POST")
  
  // Verify content type consistency
  assert_eq(azimuth::HttpRequest::url(service_a_request), "https://service-b.example.com/api/process")
  assert_eq(azimuth::HttpRequest::url(service_b_request), "https://service-c.example.com/api/validate")
  assert_eq(azimuth::HttpRequest::url(service_c_request), "https://service-d.example.com/api/query")
  
  // Verify response status consistency
  assert_eq(azimuth::HttpResponse::status_code(service_a_response), 200)
  assert_eq(azimuth::HttpResponse::status_code(service_b_response), 200)
  assert_eq(azimuth::HttpResponse::status_code(service_c_response), 200)
  assert_eq(azimuth::HttpResponse::status_code(service_d_response), 200)
  
  // Verify response content type consistency
  // Note: In a real implementation, we would need to access response headers to verify content type
  // This is a simplified test to demonstrate the concept
  
  // Verify trace ID consistency across all requests and responses
  // Note: In a real implementation, we would need to access request/response headers to verify trace ID
  // This is a simplified test to demonstrate the concept
  
  // Verify request ID consistency across all requests and responses
  // Note: In a real implementation, we would need to access request/response headers to verify request ID
  // This is a simplified test to demonstrate the concept
  
  // Verify user ID consistency across all requests and responses
  // Note: In a real implementation, we would need to access request/response headers to verify user ID
  // This is a simplified test to demonstrate the concept
  
  // Verify session ID consistency across all requests and responses
  // Note: In a real implementation, we would need to access request/response headers to verify session ID
  // This is a simplified test to demonstrate the concept
  
  // Verify tenant ID consistency across all requests and responses
  // Note: In a real implementation, we would need to access request/response headers to verify tenant ID
  // This is a simplified test to demonstrate the concept
}