// Distributed Tracing Scenario Tests
// This file contains test cases that simulate real-world distributed tracing scenarios

// Test 1: End-to-end request tracing across multiple services
pub test "end-to-end request tracing" {
  // Simulate a web request flowing through multiple microservices
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "web-service")
  
  // Create root span for incoming request
  let root_span = azimuth::Tracer::start_span(tracer, "HTTP GET /api/users")
  azimuth::Span::set_attribute(root_span, "http.method", azimuth::StringValue("GET"))
  azimuth::Span::set_attribute(root_span, "http.url", azimuth::StringValue("https://api.example.com/users"))
  azimuth::Span::set_attribute(root_span, "http.user_agent", azimuth::StringValue("Mozilla/5.0"))
  azimuth::Span::set_attribute(root_span, "net.host.name", azimuth::StringValue("api.example.com"))
  azimuth::Span::set_attribute(root_span, "net.host.port", azimuth::IntValue(443))
  
  // Service A calls Service B
  let service_b_span = azimuth::Tracer::start_span_with_parent(tracer, "Service B: user authentication", root_span)
  azimuth::Span::set_attribute(service_b_span, "service.name", azimuth::StringValue("auth-service"))
  azimuth::Span::set_attribute(service_b_span, "db.operation", azimuth::StringValue("SELECT"))
  azimuth::Span::set_attribute(service_b_span, "db.statement", azimuth::StringValue("SELECT * FROM users WHERE id = ?"))
  
  // Service B calls database
  let db_span = azimuth::Tracer::start_span_with_parent(tracer, "Database Query", service_b_span)
  azimuth::Span::set_attribute(db_span, "db.system", azimuth::StringValue("postgresql"))
  azimuth::Span::set_attribute(db_span, "db.name", azimuth::StringValue("auth_db"))
  azimuth::Span::set_attribute(db_span, "db.user", azimuth::StringValue("auth_user"))
  azimuth::Span::set_attribute(db_span, "db.connection_string", azimuth::StringValue("postgresql://localhost:5432/auth_db"))
  azimuth::Span::set_status_code(db_span, azimuth::Ok)
  azimuth::Span::end(db_span)
  
  // Service B responds to Service A
  azimuth::Span::set_status_code(service_b_span, azimuth::Ok)
  azimuth::Span::end(service_b_span)
  
  // Service A calls Service C
  let service_c_span = azimuth::Tracer::start_span_with_parent(tracer, "Service C: user profile", root_span)
  azimuth::Span::set_attribute(service_c_span, "service.name", azimuth::StringValue("profile-service"))
  azimuth::Span::set_attribute(service_c_span, "cache.hit", azimuth::BoolValue(false))
  
  // Service C calls cache
  let cache_span = azimuth::Tracer::start_span_with_parent(tracer, "Cache Operation", service_c_span)
  azimuth::Span::set_attribute(cache_span, "cache.system", azimuth::StringValue("redis"))
  azimuth::Span::set_attribute(cache_span, "cache.operation", azimuth::StringValue("GET"))
  azimuth::Span::set_attribute(cache_span, "cache.key", azimuth::StringValue("profile:user:123"))
  azimuth::Span::set_status_code(cache_span, azimuth::Ok)
  azimuth::Span::end(cache_span)
  
  // Service C responds to Service A
  azimuth::Span::set_status_code(service_c_span, azimuth::Ok)
  azimuth::Span::end(service_c_span)
  
  // Root span completes
  azimuth::Span::set_status_code(root_span, azimuth::Ok)
  azimuth::Span::set_attribute(root_span, "http.status_code", azimuth::IntValue(200))
  azimuth::Span::end(root_span)
  
  // Verify span relationships
  assert_true(azimuth::Span::context(root_span) != azimuth::Span::context(service_b_span))
  assert_true(azimuth::Span::context(root_span) != azimuth::Span::context(service_c_span))
  assert_true(azimuth::Span::context(service_b_span) != azimuth::Span::context(service_c_span))
}

// Test 2: Asynchronous operation tracing
pub test "asynchronous operation tracing" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "async-service")
  
  // Main operation span
  let main_span = azimuth::Tracer::start_span(tracer, "Async Data Processing")
  azimuth::Span::set_attribute(main_span, "operation.type", azimuth::StringValue("async_batch"))
  azimuth::Span::set_attribute(main_span, "batch.size", azimuth::IntValue(1000))
  
  // Simulate multiple parallel async operations
  let async_span1 = azimuth::Tracer::start_span_with_parent(tracer, "Async Operation 1", main_span)
  azimuth::Span::set_attribute(async_span1, "operation.id", azimuth::StringValue("op-001"))
  azimuth::Span::set_attribute(async_span1, "data.source", azimuth::StringValue("database"))
  
  let async_span2 = azimuth::Tracer::start_span_with_parent(tracer, "Async Operation 2", main_span)
  azimuth::Span::set_attribute(async_span2, "operation.id", azimuth::StringValue("op-002"))
  azimuth::Span::set_attribute(async_span2, "data.source", azimuth::StringValue("api"))
  
  let async_span3 = azimuth::Tracer::start_span_with_parent(tracer, "Async Operation 3", main_span)
  azimuth::Span::set_attribute(async_span3, "operation.id", azimuth::StringValue("op-003"))
  azimuth::Span::set_attribute(async_span3, "data.source", azimuth::StringValue("file_system"))
  
  // Simulate async operations completing in different order
  azimuth::Span::set_status_code(async_span2, azimuth::Ok)
  azimuth::Span::set_attribute(async_span2, "processing.time", azimuth::IntValue(150))
  azimuth::Span::end(async_span2)
  
  azimuth::Span::set_status_code(async_span3, azimuth::Error)
  azimuth::Span::set_attribute(async_span3, "processing.time", azimuth::IntValue(300))
  azimuth::Span::set_attribute(async_span3, "error.message", azimuth::StringValue("File not found"))
  azimuth::Span::end(async_span3)
  
  azimuth::Span::set_status_code(async_span1, azimuth::Ok)
  azimuth::Span::set_attribute(async_span1, "processing.time", azimuth::IntValue(200))
  azimuth::Span::end(async_span1)
  
  // Main operation completes with partial success
  azimuth::Span::set_status_code(main_span, azimuth::Error)
  azimuth::Span::set_attribute(main_span, "successful.operations", azimuth::IntValue(2))
  azimuth::Span::set_attribute(main_span, "failed.operations", azimuth::IntValue(1))
  azimuth::Span::end(main_span)
  
  // Verify span contexts are different
  assert_true(azimuth::Span::context(main_span) != azimuth::Span::context(async_span1))
  assert_true(azimuth::Span::context(async_span1) != azimuth::Span::context(async_span2))
  assert_true(azimuth::Span::context(async_span2) != azimuth::Span::context(async_span3))
}

// Test 3: Error propagation and context preservation
pub test "error propagation with context preservation" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-prone-service")
  
  // Service entry point
  let entry_span = azimuth::Tracer::start_span(tracer, "Service Entry")
  azimuth::Span::set_attribute(entry_span, "service.name", azimuth::StringValue("main-service"))
  azimuth::Span::set_attribute(entry_span, "request.id", azimuth::StringValue("req-12345"))
  
  // First operation that fails
  let failing_span = azimuth::Tracer::start_span_with_parent(tracer, "Failing Operation", entry_span)
  azimuth::Span::set_attribute(failing_span, "operation.type", azimuth::StringValue("validation"))
  azimuth::Span::set_attribute(failing_span, "input.data", azimuth::StringValue("invalid_input"))
  
  // Simulate error occurrence
  azimuth::Span::set_status_code(failing_span, azimuth::Error)
  azimuth::Span::set_attribute(failing_span, "error.type", azimuth::StringValue("ValidationError"))
  azimuth::Span::set_attribute(failing_span, "error.message", azimuth::StringValue("Invalid input format"))
  azimuth::Span::set_attribute(failing_span, "error.stack", azimuth::StringValue("at ValidationService.validate (line 42)"))
  azimuth::Span::end(failing_span)
  
  // Error handling operation
  let error_handling_span = azimuth::Tracer::start_span_with_parent(tracer, "Error Handling", entry_span)
  azimuth::Span::set_attribute(error_handling_span, "error.recovered", azimuth::BoolValue(false))
  azimuth::Span::set_attribute(error_handling_span, "fallback.used", azimuth::BoolValue(true))
  
  // Fallback operation
  let fallback_span = azimuth::Tracer::start_span_with_parent(tracer, "Fallback Operation", error_handling_span)
  azimuth::Span::set_attribute(fallback_span, "fallback.type", azimuth::StringValue("default_response"))
  azimuth::Span::set_status_code(fallback_span, azimuth::Ok)
  azimuth::Span::end(fallback_span)
  
  azimuth::Span::set_status_code(error_handling_span, azimuth::Ok)
  azimuth::Span::end(error_handling_span)
  
  // Entry span completes with error recovery
  azimuth::Span::set_status_code(entry_span, azimuth::Error)
  azimuth::Span::set_attribute(entry_span, "error.recovered", azimuth::BoolValue(true))
  azimuth::Span::end(entry_span)
  
  // Verify error context is preserved
  let failing_ctx = azimuth::Span::context(failing_span)
  let entry_ctx = azimuth::Span::context(entry_span)
  assert_true(azimuth::SpanContext::trace_id(failing_ctx) == azimuth::SpanContext::trace_id(entry_ctx))
}

// Test 4: Baggage propagation across service boundaries
pub test "cross-service baggage propagation" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "baggage-test-service")
  
  // Initial context with baggage
  let root_ctx = azimuth::Context::root()
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "sess-67890")
  let baggage_with_tenant = azimuth::Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-001")
  let ctx_with_baggage = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("baggage"), baggage_with_tenant)
  
  // Service A span
  let service_a_span = azimuth::Tracer::start_span_with_context(tracer, "Service A Operation", ctx_with_baggage)
  azimuth::Span::set_attribute(service_a_span, "service.name", azimuth::StringValue("service-a"))
  
  // Extract baggage from context and add service-specific entry
  let current_baggage = azimuth::Context::get(azimuth::Span::context(service_a_span), azimuth::ContextKey::new("baggage"))
  let updated_baggage = azimuth::Baggage::set_entry(current_baggage, "service.a.timestamp", "1640995200")
  let service_a_ctx = azimuth::Context::with_value(azimuth::Span::context(service_a_span), azimuth::ContextKey::new("baggage"), updated_baggage)
  
  // Service B span (called by Service A)
  let service_b_span = azimuth::Tracer::start_span_with_context(tracer, "Service B Operation", service_a_ctx)
  azimuth::Span::set_attribute(service_b_span, "service.name", azimuth::StringValue("service-b"))
  
  // Verify baggage is propagated and contains all entries
  let propagated_baggage = azimuth::Context::get(azimuth::Span::context(service_b_span), azimuth::ContextKey::new("baggage"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "session.id"), Some("sess-67890"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "tenant.id"), Some("tenant-001"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "service.a.timestamp"), Some("1640995200"))
  
  // Add service B specific baggage
  let final_baggage = azimuth::Baggage::set_entry(propagated_baggage, "service.b.result", "success")
  let service_b_ctx = azimuth::Context::with_value(azimuth::Span::context(service_b_span), azimuth::ContextKey::new("baggage"), final_baggage)
  
  // Service C span (called by Service B)
  let service_c_span = azimuth::Tracer::start_span_with_context(tracer, "Service C Operation", service_b_ctx)
  azimuth::Span::set_attribute(service_c_span, "service.name", azimuth::StringValue("service-c"))
  
  // Verify all baggage entries are preserved through the chain
  let final_propagated_baggage = azimuth::Context::get(azimuth::Span::context(service_c_span), azimuth::ContextKey::new("baggage"))
  assert_eq(azimuth::Baggage::get_entry(final_propagated_baggage, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(final_propagated_baggage, "session.id"), Some("sess-67890"))
  assert_eq(azimuth::Baggage::get_entry(final_propagated_baggage, "tenant.id"), Some("tenant-001"))
  assert_eq(azimuth::Baggage::get_entry(final_propagated_baggage, "service.a.timestamp"), Some("1640995200"))
  assert_eq(azimuth::Baggage::get_entry(final_propagated_baggage, "service.b.result"), Some("success"))
  
  // End all spans
  azimuth::Span::end(service_c_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
}