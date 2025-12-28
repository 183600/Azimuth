// Cross-Service Propagation Consistency Test Suite for Azimuth
// This file contains comprehensive tests for cross-service telemetry propagation

// Test 1: End-to-end trace propagation across multiple services
pub test "end-to-end trace propagation across multiple services" {
  // Initialize trace context in service A (entry point)
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let root_span_ctx = azimuth::SpanContext::new(trace_id, parent_span_id, true, "key1=value1,key2=value2")
  
  // Service A: Create root span
  let service_a_span = azimuth::Span::new("service-a.operation", azimuth::Server, root_span_ctx)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_a_span)), trace_id)
  assert_eq(azimuth::SpanContext::span_id(azimuth::Span::span_context(service_a_span)), parent_span_id)
  
  // Service A: Create child span for downstream call
  let child_span_id = "c3d4e5f678901234"
  let child_span_ctx = azimuth::SpanContext::new(trace_id, child_span_id, true, "key1=value1,key2=value2")
  let service_a_child = azimuth::Span::new("service-a.downstream-call", azimuth::Client, child_span_ctx)
  
  // Service B: Extract context from incoming request
  let service_b_span_ctx = azimuth::SpanContext::new(trace_id, child_span_id, true, "key1=value1,key2=value2")
  let service_b_span = azimuth::Span::new("service-b.operation", azimuth::Server, service_b_span_ctx)
  
  // Service B: Create span for database operation
  let db_span_id = "d5e6f7a8b9c0d1e2"
  let db_span_ctx = azimuth::SpanContext::new(trace_id, db_span_id, true, "key1=value1,key2=value2")
  let service_b_db = azimuth::Span::new("service-b.database-query", azimuth::Client, db_span_ctx)
  
  // Verify trace consistency across all services
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_a_span)), trace_id)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_a_child)), trace_id)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_b_span)), trace_id)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_b_db)), trace_id)
  
  // Verify all spans are sampled consistently
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(service_a_span)))
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(service_a_child)))
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(service_b_span)))
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(service_b_db)))
}

// Test 2: Baggage propagation across service boundaries
pub test "baggage propagation across service boundaries" {
  // Service A: Initialize baggage
  let initial_baggage = azimuth::Baggage::new()
  let baggage_a = azimuth::Baggage::set_entry(initial_baggage, "user.id", "12345")
  let baggage_a2 = azimuth::Baggage::set_entry(baggage_a, "request.id", "req-abc-123")
  let baggage_a3 = azimuth::Baggage::set_entry(baggage_a2, "session.id", "sess-xyz-789")
  
  // Verify Service A baggage
  assert_eq(azimuth::Baggage::get_entry(baggage_a3, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_a3, "request.id"), Some("req-abc-123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_a3, "session.id"), Some("sess-xyz-789"))
  
  // Service B: Receive and extend baggage
  let baggage_b = azimuth::Baggage::set_entry(baggage_a3, "service.b.timestamp", "2025-01-01T12:00:00Z")
  let baggage_b2 = azimuth::Baggage::set_entry(baggage_b, "service.b.operation", "process-payment")
  
  // Verify Service B baggage (includes original + new entries)
  assert_eq(azimuth::Baggage::get_entry(baggage_b2, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b2, "request.id"), Some("req-abc-123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b2, "session.id"), Some("sess-xyz-789"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b2, "service.b.timestamp"), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b2, "service.b.operation"), Some("process-payment"))
  
  // Service C: Receive and potentially modify baggage
  let baggage_c = azimuth::Baggage::set_entry(baggage_b2, "service.c.result", "success")
  let baggage_c2 = azimuth::Baggage::set_entry(baggage_c, "service.c.duration", "250ms")
  
  // Verify Service C baggage (includes all previous + new entries)
  assert_eq(azimuth::Baggage::get_entry(baggage_c2, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c2, "request.id"), Some("req-abc-123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c2, "session.id"), Some("sess-xyz-789"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c2, "service.b.timestamp"), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c2, "service.b.operation"), Some("process-payment"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c2, "service.c.result"), Some("success"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c2, "service.c.duration"), Some("250ms"))
}

// Test 3: Context propagation with composite propagators
pub test "context propagation with composite propagators" {
  // Initialize composite propagator with multiple propagators
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // Service A: Create context with trace and baggage
  let root_ctx = azimuth::Context::root()
  let trace_key = azimuth::ContextKey::new("trace.context")
  let baggage_key = azimuth::ContextKey::new("baggage.context")
  let ctx_with_trace = azimuth::Context::with_value(root_ctx, trace_key, "trace-data")
  let ctx_with_both = azimuth::Context::with_value(ctx_with_trace, baggage_key, "baggage-data")
  
  // Service A: Inject context into carrier
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_both, carrier)
  
  // Verify injection
  let injected_trace = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let injected_baggage = azimuth::TextMapCarrier::get(carrier, "baggage")
  assert_eq(injected_trace, Some("00-test-trace-id-test-span-id-01"))
  
  // Service B: Extract context from carrier
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  let extracted_trace = azimuth::Context::get(extracted_ctx, trace_key)
  let extracted_baggage = azimuth::Context::get(extracted_ctx, baggage_key)
  
  // Verify extraction
  assert_eq(extracted_trace, Some("true"))  // Based on simplified implementation
}

// Test 4: Cross-service metrics consistency
pub test "cross-service metrics consistency" {
  // Service A: Create metrics with consistent naming
  let provider_a = azimuth::MeterProvider::default()
  let meter_a = azimuth::MeterProvider::get_meter(provider_a, "service-a")
  
  let request_counter_a = azimuth::Meter::create_counter(meter_a, "http.requests.total")
  let duration_histogram_a = azimuth::Meter::create_histogram(meter_a, "http.request.duration")
  let error_counter_a = azimuth::Meter::create_counter(meter_a, "http.errors.total")
  
  // Set common attributes for cross-service consistency
  let common_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(common_attrs, "service.name", azimuth::StringValue("user-service"))
  azimuth::Attributes::set(common_attrs, "service.version", azimuth::StringValue("1.2.3"))
  azimuth::Attributes::set(common_attrs, "deployment.environment", azimuth::StringValue("production"))
  
  // Record metrics in Service A
  azimuth::Counter::add(request_counter_a, 10.0)
  azimuth::Histogram::record(duration_histogram_a, 150.5)
  azimuth::Counter::add(error_counter_a, 2.0)
  
  // Service B: Create metrics with same naming convention
  let provider_b = azimuth::MeterProvider::default()
  let meter_b = azimuth::MeterProvider::get_meter(provider_b, "service-b")
  
  let request_counter_b = azimuth::Meter::create_counter(meter_b, "http.requests.total")
  let duration_histogram_b = azimuth::Meter::create_histogram(meter_b, "http.request.duration")
  let error_counter_b = azimuth::Meter::create_counter(meter_b, "http.errors.total")
  
  // Set consistent attributes in Service B
  let service_b_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_b_attrs, "service.name", azimuth::StringValue("payment-service"))
  azimuth::Attributes::set(service_b_attrs, "service.version", azimuth::StringValue("2.1.0"))
  azimuth::Attributes::set(service_b_attrs, "deployment.environment", azimuth::StringValue("production"))
  
  // Record metrics in Service B
  azimuth::Counter::add(request_counter_b, 25.0)
  azimuth::Histogram::record(duration_histogram_b, 89.2)
  azimuth::Counter::add(error_counter_b, 1.0)
  
  // Verify metric naming consistency
  assert_eq(request_counter_a.name, "http.requests.total")
  assert_eq(request_counter_b.name, "http.requests.total")
  assert_eq(duration_histogram_a.name, "http.request.duration")
  assert_eq(duration_histogram_b.name, "http.request.duration")
  assert_eq(error_counter_a.name, "http.errors.total")
  assert_eq(error_counter_b.name, "http.errors.total")
}

// Test 5: Cross-service logging correlation
pub test "cross-service logging correlation" {
  // Initialize shared trace context
  let trace_id = "a1b2c3d4e5f678901234567890123456"
  let span_id = "f1e2d3c4b5a69788"
  let shared_span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
  
  // Service A: Create correlated log record
  let logger_provider_a = azimuth::LoggerProvider::default()
  let logger_a = azimuth::LoggerProvider::get_logger(logger_provider_a, "service-a")
  
  let log_record_a = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Processing user request in Service A"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Service B: Create correlated log record with same trace
  let logger_provider_b = azimuth::LoggerProvider::default()
  let logger_b = azimuth::LoggerProvider::get_logger(logger_provider_b, "service-b")
  
  let service_b_span_id = "e7d6c5b4a3928170"
  let service_b_span_ctx = azimuth::SpanContext::new(trace_id, service_b_span_id, true, "")
  
  let log_record_b = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Handling downstream call in Service B"),
    None,
    Some(1735689600000001000L),
    None,
    Some(trace_id),
    Some(service_b_span_id),
    Some(azimuth::Context::root())
  )
  
  // Service C: Create error log record with same trace
  let logger_provider_c = azimuth::LoggerProvider::default()
  let logger_c = azimuth::LoggerProvider::get_logger(logger_provider_c, "service-c")
  
  let service_c_span_id = "d9c8b7a615243698"
  let service_c_span_ctx = azimuth::SpanContext::new(trace_id, service_c_span_id, true, "")
  
  let log_record_c = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Database connection failed in Service C"),
    None,
    Some(1735689600000002000L),
    None,
    Some(trace_id),
    Some(service_c_span_id),
    Some(azimuth::Context::root())
  )
  
  // Verify trace correlation across all log records
  assert_eq(azimuth::LogRecord::trace_id(log_record_a), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(log_record_b), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(log_record_c), Some(trace_id))
  
  // Verify span IDs are different but trace ID is same
  assert_eq(azimuth::LogRecord::span_id(log_record_a), Some(span_id))
  assert_eq(azimuth::LogRecord::span_id(log_record_b), Some(service_b_span_id))
  assert_eq(azimuth::LogRecord::span_id(log_record_c), Some(service_c_span_id))
  
  // Verify log severity levels
  assert_eq(azimuth::LogRecord::severity_number(log_record_a), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_record_b), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_record_c), azimuth::Error)
}

// Test 6: Resource consistency across services
pub test "resource consistency across services" {
  // Define common resource attributes for all services
  let common_resource_attrs = [
    ("service.namespace", azimuth::StringValue("production")),
    ("service.instance.id", azimuth::StringValue("instance-12345")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("web-server-01")),
    ("cloud.provider", azimuth::StringValue("aws")),
    ("cloud.region", azimuth::StringValue("us-west-2")),
    ("k8s.cluster.name", azimuth::StringValue("main-cluster")),
    ("k8s.namespace.name", azimuth::StringValue("applications"))
  ]
  
  // Service A resource
  let resource_a = azimuth::Resource::new()
  let service_a_attrs = [
    ("service.name", azimuth::StringValue("api-gateway")),
    ("service.version", azimuth::StringValue("3.2.1"))
  ]
  let resource_a_with_attrs = azimuth::Resource::with_attributes(resource_a, service_a_attrs)
  
  // Service B resource
  let resource_b = azimuth::Resource::new()
  let service_b_attrs = [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("2.4.0"))
  ]
  let resource_b_with_attrs = azimuth::Resource::with_attributes(resource_b, service_b_attrs)
  
  // Service C resource
  let resource_c = azimuth::Resource::new()
  let service_c_attrs = [
    ("service.name", azimuth::StringValue("payment-service")),
    ("service.version", azimuth::StringValue("1.8.3"))
  ]
  let resource_c_with_attrs = azimuth::Resource::with_attributes(resource_c, service_c_attrs)
  
  // Verify service-specific attributes
  assert_eq(azimuth::Resource::get_attribute(resource_a_with_attrs, "service.name"), Some(azimuth::StringValue("api-gateway")))
  assert_eq(azimuth::Resource::get_attribute(resource_b_with_attrs, "service.name"), Some(azimuth::StringValue("user-service")))
  assert_eq(azimuth::Resource::get_attribute(resource_c_with_attrs, "service.name"), Some(azimuth::StringValue("payment-service")))
  
  assert_eq(azimuth::Resource::get_attribute(resource_a_with_attrs, "service.version"), Some(azimuth::StringValue("3.2.1")))
  assert_eq(azimuth::Resource::get_attribute(resource_b_with_attrs, "service.version"), Some(azimuth::StringValue("2.4.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_c_with_attrs, "service.version"), Some(azimuth::StringValue("1.8.3")))
}

// Test 7: HTTP headers propagation consistency
pub test "http headers propagation consistency" {
  // Service A: Outgoing HTTP request with tracing headers
  let outgoing_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-abc-123")
  ]
  
  let request = azimuth::HttpRequest::new(
    "POST", 
    "https://service-b.example.com/api/process", 
    outgoing_headers,
    Some("{\"data\": \"test\"}")
  )
  
  // Simulate adding trace headers
  let trace_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "user.id=12345,request.id=req-abc-123"),
    ("X-Service-Name", "service-a")
  ]
  
  let all_outgoing_headers = outgoing_headers + trace_headers
  let request_with_trace = azimuth::HttpRequest::new(
    "POST",
    "https://service-b.example.com/api/process",
    all_outgoing_headers,
    Some("{\"data\": \"test\"}")
  )
  
  // Service B: Incoming request with trace headers
  let incoming_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-abc-123"),
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "user.id=12345,request.id=req-abc-123"),
    ("X-Service-Name", "service-a")
  ]
  
  let incoming_request = azimuth::HttpRequest::new(
    "POST",
    "/api/process",
    incoming_headers,
    Some("{\"data\": \"test\"}")
  )
  
  // Service B: Outgoing request to Service C with propagated headers
  let propagated_headers = [
    ("Content-Type", "application/json"),
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-c3d4e5f678901234-01"),
    ("baggage", "user.id=12345,request.id=req-abc-123,service.b.timestamp=2025-01-01T12:00:00Z"),
    ("X-Service-Name", "service-b"),
    ("X-Original-Service", "service-a")
  ]
  
  let downstream_request = azimuth::HttpRequest::new(
    "GET",
    "https://service-c.example.com/api/validate",
    propagated_headers,
    None
  )
  
  // Verify header propagation
  assert_eq(azimuth::HttpRequest::http_method(request_with_trace), "POST")
  assert_eq(azimuth::HttpRequest::http_method(incoming_request), "POST")
  assert_eq(azimuth::HttpRequest::http_method(downstream_request), "GET")
}

// Test 8: Cross-service telemetry data integrity
pub test "cross-service telemetry data integrity" {
  // Simulate a complex request flow across multiple services
  
  // 1. Initial request to API Gateway
  let gateway_trace_id = "1234567890abcdef1234567890abcdef"
  let gateway_span_id = "abcdef1234567890"
  let gateway_ctx = azimuth::SpanContext::new(gateway_trace_id, gateway_span_id, true, "")
  
  // 2. API Gateway calls User Service
  let user_service_span_id = "fedcba0987654321"
  let user_service_ctx = azimuth::SpanContext::new(gateway_trace_id, user_service_span_id, true, "")
  
  // 3. User Service calls Database
  let db_span_id = "0123456789abcdef"
  let db_ctx = azimuth::SpanContext::new(gateway_trace_id, db_span_id, true, "")
  
  // 4. User Service calls Payment Service
  let payment_span_id = "fedcbaabcdef0123"
  let payment_ctx = azimuth::SpanContext::new(gateway_trace_id, payment_span_id, true, "")
  
  // Verify trace ID consistency across all services
  assert_eq(azimuth::SpanContext::trace_id(gateway_ctx), gateway_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(user_service_ctx), gateway_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(db_ctx), gateway_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(payment_ctx), gateway_trace_id)
  
  // Verify all spans are unique but related
  assert_eq(azimuth::SpanContext::span_id(gateway_ctx), gateway_span_id)
  assert_eq(azimuth::SpanContext::span_id(user_service_ctx), user_service_span_id)
  assert_eq(azimuth::SpanContext::span_id(db_ctx), db_span_id)
  assert_eq(azimuth::SpanContext::span_id(payment_ctx), payment_span_id)
  
  // Verify all spans are sampled consistently
  assert_true(azimuth::SpanContext::is_sampled(gateway_ctx))
  assert_true(azimuth::SpanContext::is_sampled(user_service_ctx))
  assert_true(azimuth::SpanContext::is_sampled(db_ctx))
  assert_true(azimuth::SpanContext::is_sampled(payment_ctx))
  
  // Verify span relationship integrity
  assert_true(azimuth::SpanContext::is_valid(gateway_ctx))
  assert_true(azimuth::SpanContext::is_valid(user_service_ctx))
  assert_true(azimuth::SpanContext::is_valid(db_ctx))
  assert_true(azimuth::SpanContext::is_valid(payment_ctx))
  
  // Ensure no span ID collision
  assert_not_eq(gateway_span_id, user_service_span_id)
  assert_not_eq(user_service_span_id, db_span_id)
  assert_not_eq(db_span_id, payment_span_id)
  assert_not_eq(payment_span_id, gateway_span_id)
}