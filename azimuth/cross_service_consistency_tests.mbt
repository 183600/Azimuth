// Cross Service Consistency Tests
// This file contains test cases for cross-service consistency and data integrity

// Test 1: Trace context propagation across services
pub test "trace_context_propagation_across_services" {
  // Service A tracer
  let service_a_provider = azimuth::TracerProvider::default()
  let service_a_tracer = azimuth::TracerProvider::get_tracer(service_a_provider, "service-a")
  
  // Create root trace in Service A
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "Service A Operation")
  azimuth::Span::set_attribute(service_a_span, "service.name", azimuth::StringValue("service-a"))
  azimuth::Span::set_attribute(service_a_span, "operation.type", azimuth::StringValue("user_request"))
  azimuth::Span::set_attribute(service_a_span, "user.id", azimuth::StringValue("user-123"))
  
  // Extract context for propagation
  let service_a_context = azimuth::Span::context(service_a_span)
  
  // Simulate context propagation to Service B
  let service_b_provider = azimuth::TracerProvider::default()
  let service_b_tracer = azimuth::TracerProvider::get_tracer(service_b_provider, "service-b")
  
  let service_b_span = azimuth::Tracer::start_span_with_context(service_b_tracer, "Service B Operation", service_a_context)
  azimuth::Span::set_attribute(service_b_span, "service.name", azimuth::StringValue("service-b"))
  azimuth::Span::set_attribute(service_b_span, "operation.type", azimuth::StringValue("data_processing"))
  azimuth::Span::set_attribute(service_b_span, "parent.service", azimuth::StringValue("service-a"))
  
  // Verify trace context is preserved
  let service_b_context = azimuth::Span::context(service_b_span)
  assert_eq(azimuth::SpanContext::trace_id(service_a_context), azimuth::SpanContext::trace_id(service_b_context))
  assert_true(azimuth::SpanContext::is_sampled(service_a_context) == azimuth::SpanContext::is_sampled(service_b_context))
  
  // Simulate context propagation to Service C
  let service_c_provider = azimuth::TracerProvider::default()
  let service_c_tracer = azimuth::TracerProvider::get_tracer(service_c_provider, "service-c")
  
  let service_c_span = azimuth::Tracer::start_span_with_context(service_c_tracer, "Service C Operation", service_b_context)
  azimuth::Span::set_attribute(service_c_span, "service.name", azimuth::StringValue("service-c"))
  azimuth::Span::set_attribute(service_c_span, "operation.type", azimuth::StringValue("data_storage"))
  azimuth::Span::set_attribute(service_c_span, "parent.service", azimuth::StringValue("service-b"))
  
  // Verify trace context is preserved through the chain
  let service_c_context = azimuth::Span::context(service_c_span)
  assert_eq(azimuth::SpanContext::trace_id(service_a_context), azimuth::SpanContext::trace_id(service_c_context))
  assert_eq(azimuth::SpanContext::trace_id(service_b_context), azimuth::SpanContext::trace_id(service_c_context))
  
  // End all spans
  azimuth::Span::set_status_code(service_c_span, azimuth::Ok)
  azimuth::Span::set_status_code(service_b_span, azimuth::Ok)
  azimuth::Span::set_status_code(service_a_span, azimuth::Ok)
  azimuth::Span::end(service_c_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
}

// Test 2: Baggage consistency across service boundaries
pub test "baggage_consistency_across_services" {
  // Service A: Create initial baggage
  let service_a_provider = azimuth::TracerProvider::default()
  let service_a_tracer = azimuth::TracerProvider::get_tracer(service_a_provider, "service-a")
  
  let root_ctx = azimuth::Context::root()
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "session-456")
  let baggage_with_tenant = azimuth::Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-789")
  
  let ctx_with_baggage = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("baggage"), baggage_with_tenant)
  
  let service_a_span = azimuth::Tracer::start_span_with_context(service_a_tracer, "Service A Operation", ctx_with_baggage)
  azimuth::Span::set_attribute(service_a_span, "service.name", azimuth::StringValue("service-a"))
  
  // Service A adds service-specific baggage
  let current_baggage = azimuth::Context::get(azimuth::Span::context(service_a_span), azimuth::ContextKey::new("baggage"))
  let service_a_baggage = azimuth::Baggage::set_entry(current_baggage, "service.a.timestamp", "1640995200")
  let service_a_ctx = azimuth::Context::with_value(azimuth::Span::context(service_a_span), azimuth::ContextKey::new("baggage"), service_a_baggage)
  
  // Propagate to Service B
  let service_b_provider = azimuth::TracerProvider::default()
  let service_b_tracer = azimuth::TracerProvider::get_tracer(service_b_provider, "service-b")
  
  let service_b_span = azimuth::Tracer::start_span_with_context(service_b_tracer, "Service B Operation", service_a_ctx)
  azimuth::Span::set_attribute(service_b_span, "service.name", azimuth::StringValue("service-b"))
  
  // Verify baggage consistency
  let propagated_baggage = azimuth::Context::get(azimuth::Span::context(service_b_span), azimuth::ContextKey::new("baggage"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "session.id"), Some("session-456"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "tenant.id"), Some("tenant-789"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "service.a.timestamp"), Some("1640995200"))
  
  // Service B adds its own baggage
  let service_b_baggage = azimuth::Baggage::set_entry(propagated_baggage, "service.b.result", "success")
  let service_b_ctx = azimuth::Context::with_value(azimuth::Span::context(service_b_span), azimuth::ContextKey::new("baggage"), service_b_baggage)
  
  // Propagate to Service C
  let service_c_provider = azimuth::TracerProvider::default()
  let service_c_tracer = azimuth::TracerProvider::get_tracer(service_c_provider, "service-c")
  
  let service_c_span = azimuth::Tracer::start_span_with_context(service_c_tracer, "Service C Operation", service_b_ctx)
  azimuth::Span::set_attribute(service_c_span, "service.name", azimuth::StringValue("service-c"))
  
  // Verify final baggage consistency
  let final_baggage = azimuth::Context::get(azimuth::Span::context(service_c_span), azimuth::ContextKey::new("baggage"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "session.id"), Some("session-456"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "tenant.id"), Some("tenant-789"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "service.a.timestamp"), Some("1640995200"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "service.b.result"), Some("success"))
  
  // End all spans
  azimuth::Span::set_status_code(service_c_span, azimuth::Ok)
  azimuth::Span::set_status_code(service_b_span, azimuth::Ok)
  azimuth::Span::set_status_code(service_a_span, azimuth::Ok)
  azimuth::Span::end(service_c_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
}

// Test 3: Metrics consistency across services
pub test "metrics_consistency_across_services" {
  // Create metrics providers for each service
  let service_a_provider = azimuth::MeterProvider::default()
  let service_a_meter = azimuth::MeterProvider::get_meter(service_a_provider, "service-a")
  
  let service_b_provider = azimuth::MeterProvider::default()
  let service_b_meter = azimuth::MeterProvider::get_meter(service_b_provider, "service-b")
  
  let service_c_provider = azimuth::MeterProvider::default()
  let service_c_meter = azimuth::MeterProvider::get_meter(service_c_provider, "service-c")
  
  // Create consistent metric instruments across services
  let service_a_request_counter = azimuth::Meter::create_counter(service_a_meter, "requests.total", Some("Total requests"), Some("count"))
  let service_b_request_counter = azimuth::Meter::create_counter(service_b_meter, "requests.total", Some("Total requests"), Some("count"))
  let service_c_request_counter = azimuth::Meter::create_counter(service_c_meter, "requests.total", Some("Total requests"), Some("count"))
  
  let service_a_latency_histogram = azimuth::Meter::create_histogram(service_a_meter, "request.duration", Some("Request duration"), Some("ms"))
  let service_b_latency_histogram = azimuth::Meter::create_histogram(service_b_meter, "request.duration", Some("Request duration"), Some("ms"))
  let service_c_latency_histogram = azimuth::Meter::create_histogram(service_c_meter, "request.duration", Some("Request duration"), Some("ms"))
  
  // Record metrics with consistent attributes
  let common_attributes = [("user.id", "user-123"), ("request.id", "req-456"), ("tenant.id", "tenant-789")]
  
  // Service A records metrics
  azimuth::Counter::add(service_a_request_counter, 1, common_attributes + [("service", "service-a")])
  azimuth::Histogram::record(service_a_latency_histogram, 100.0, common_attributes + [("service", "service-a")])
  
  // Service B records metrics
  azimuth::Counter::add(service_b_request_counter, 1, common_attributes + [("service", "service-b")])
  azimuth::Histogram::record(service_b_latency_histogram, 150.0, common_attributes + [("service", "service-b")])
  
  // Service C records metrics
  azimuth::Counter::add(service_c_request_counter, 1, common_attributes + [("service", "service-c")])
  azimuth::Histogram::record(service_c_latency_histogram, 200.0, common_attributes + [("service", "service-c")])
  
  // Verify metric consistency
  assert_eq(service_a_request_counter.name, "requests.total")
  assert_eq(service_b_request_counter.name, "requests.total")
  assert_eq(service_c_request_counter.name, "requests.total")
  
  assert_eq(service_a_latency_histogram.name, "request.duration")
  assert_eq(service_b_latency_histogram.name, "request.duration")
  assert_eq(service_c_latency_histogram.name, "request.duration")
  
  assert_eq(service_a_request_counter.description, Some("Total requests"))
  assert_eq(service_b_request_counter.description, Some("Total requests"))
  assert_eq(service_c_request_counter.description, Some("Total requests"))
  
  assert_eq(service_a_latency_histogram.unit, Some("ms"))
  assert_eq(service_b_latency_histogram.unit, Some("ms"))
  assert_eq(service_c_latency_histogram.unit, Some("ms"))
}

// Test 4: Resource consistency across services
pub test "resource_consistency_across_services" {
  // Create consistent base resource
  let base_resource = azimuth::Resource::new()
  let base_resource_attrs = azimuth::Resource::with_attributes(base_resource, [
    ("service.namespace", azimuth::StringValue("production")),
    ("deployment.environment", azimuth::StringValue("prod")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ])
  
  // Service A resource
  let service_a_resource = azimuth::Resource::with_attributes(base_resource_attrs, [
    ("service.name", azimuth::StringValue("service-a")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("service-a-instance-1"))
  ])
  
  // Service B resource
  let service_b_resource = azimuth::Resource::with_attributes(base_resource_attrs, [
    ("service.name", azimuth::StringValue("service-b")),
    ("service.version", azimuth::StringValue("1.2.0")),
    ("service.instance.id", azimuth::StringValue("service-b-instance-1"))
  ])
  
  // Service C resource
  let service_c_resource = azimuth::Resource::with_attributes(base_resource_attrs, [
    ("service.name", azimuth::StringValue("service-c")),
    ("service.version", azimuth::StringValue("1.1.0")),
    ("service.instance.id", azimuth::StringValue("service-c-instance-1"))
  ])
  
  // Verify base resource consistency
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "service.namespace"), Some(azimuth::StringValue("production")))
  
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "deployment.environment"), Some(azimuth::StringValue("prod")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "deployment.environment"), Some(azimuth::StringValue("prod")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "deployment.environment"), Some(azimuth::StringValue("prod")))
  
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  
  // Verify service-specific attributes
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "service.name"), Some(azimuth::StringValue("service-a")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "service.name"), Some(azimuth::StringValue("service-b")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "service.name"), Some(azimuth::StringValue("service-c")))
}

// Test 5: Logging consistency across services
pub test "logging_consistency_across_services" {
  // Create loggers for each service
  let service_a_logger_provider = azimuth::LoggerProvider::default()
  let service_a_logger = azimuth::LoggerProvider::get_logger(service_a_logger_provider, "service-a")
  
  let service_b_logger_provider = azimuth::LoggerProvider::default()
  let service_b_logger = azimuth::LoggerProvider::get_logger(service_b_logger_provider, "service-b")
  
  let service_c_logger_provider = azimuth::LoggerProvider::default()
  let service_c_logger = azimuth::LoggerProvider::get_logger(service_c_logger_provider, "service-c")
  
  // Create consistent log attributes
  let common_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(common_log_attrs, "request.id", azimuth::StringValue("req-456"))
  azimuth::Attributes::set(common_log_attrs, "user.id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(common_log_attrs, "trace.id", azimuth::StringValue("trace-789"))
  
  // Service A logs
  let service_a_log = azimuth::LogRecord::new(azimuth::Info, "Service A processing request")
  let service_a_enriched = azimuth::LogRecord::with_attributes(service_a_log, common_log_attrs)
  let service_a_final_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_a_final_attrs, "service.name", azimuth::StringValue("service-a"))
  azimuth::Attributes::set(service_a_final_attrs, "operation", azimuth::StringValue("data_validation"))
  let service_a_final = azimuth::LogRecord::with_attributes(service_a_enriched, service_a_final_attrs)
  azimuth::Logger::emit(service_a_logger, service_a_final)
  
  // Service B logs
  let service_b_log = azimuth::LogRecord::new(azimuth::Info, "Service B processing request")
  let service_b_enriched = azimuth::LogRecord::with_attributes(service_b_log, common_log_attrs)
  let service_b_final_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_b_final_attrs, "service.name", azimuth::StringValue("service-b"))
  azimuth::Attributes::set(service_b_final_attrs, "operation", azimuth::StringValue("data_transformation"))
  let service_b_final = azimuth::LogRecord::with_attributes(service_b_enriched, service_b_final_attrs)
  azimuth::Logger::emit(service_b_logger, service_b_final)
  
  // Service C logs
  let service_c_log = azimuth::LogRecord::new(azimuth::Info, "Service C processing request")
  let service_c_enriched = azimuth::LogRecord::with_attributes(service_c_log, common_log_attrs)
  let service_c_final_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_c_final_attrs, "service.name", azimuth::StringValue("service-c"))
  azimuth::Attributes::set(service_c_final_attrs, "operation", azimuth::StringValue("data_storage"))
  let service_c_final = azimuth::LogRecord::with_attributes(service_c_enriched, service_c_final_attrs)
  azimuth::Logger::emit(service_c_logger, service_c_final)
  
  // Verify logger consistency
  assert_eq(service_a_logger.scope.name, "service-a")
  assert_eq(service_b_logger.scope.name, "service-b")
  assert_eq(service_c_logger.scope.name, "service-c")
  
  // Verify log severity consistency
  assert_eq(azimuth::LogRecord::severity_number(service_a_final), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(service_b_final), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(service_c_final), azimuth::Info)
}

// Test 6: End-to-end transaction consistency
pub test "end_to_end_transaction_consistency" {
  // Create a complete transaction flow across services
  let transaction_id = "txn-123456"
  let user_id = "user-789"
  let session_id = "session-456"
  
  // Service A: Entry point
  let service_a_provider = azimuth::TracerProvider::default()
  let service_a_tracer = azimuth::TracerProvider::get_tracer(service_a_provider, "service-a")
  
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "Process User Request")
  azimuth::Span::set_attribute(service_a_span, "service.name", azimuth::StringValue("service-a"))
  azimuth::Span::set_attribute(service_a_span, "transaction.id", azimuth::StringValue(transaction_id))
  azimuth::Span::set_attribute(service_a_span, "user.id", azimuth::StringValue(user_id))
  azimuth::Span::set_attribute(service_a_span, "session.id", azimuth::StringValue(session_id))
  azimuth::Span::set_attribute(service_a_span, "operation.type", azimuth::StringValue("request_validation"))
  
  // Create consistent baggage for the transaction
  let baggage = azimuth::Baggage::new()
  let baggage_with_txn = azimuth::Baggage::set_entry(baggage, "transaction.id", transaction_id)
  let baggage_with_user = azimuth::Baggage::set_entry(baggage_with_txn, "user.id", user_id)
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", session_id)
  
  let ctx_with_baggage = azimuth::Context::with_value(azimuth::Span::context(service_a_span), azimuth::ContextKey::new("baggage"), baggage_with_session)
  
  // Service B: Processing
  let service_b_provider = azimuth::TracerProvider::default()
  let service_b_tracer = azimuth::TracerProvider::get_tracer(service_b_provider, "service-b")
  
  let service_b_span = azimuth::Tracer::start_span_with_context(service_b_tracer, "Process Business Logic", ctx_with_baggage)
  azimuth::Span::set_attribute(service_b_span, "service.name", azimuth::StringValue("service-b"))
  azimuth::Span::set_attribute(service_b_span, "transaction.id", azimuth::StringValue(transaction_id))
  azimuth::Span::set_attribute(service_b_span, "user.id", azimuth::StringValue(user_id))
  azimuth::Span::set_attribute(service_b_span, "session.id", azimuth::StringValue(session_id))
  azimuth::Span::set_attribute(service_b_span, "operation.type", azimuth::StringValue("business_logic"))
  
  // Service C: Storage
  let service_c_provider = azimuth::TracerProvider::default()
  let service_c_tracer = azimuth::TracerProvider::get_tracer(service_c_provider, "service-c")
  
  let service_c_span = azimuth::Tracer::start_span_with_context(service_c_tracer, "Store Results", azimuth::Span::context(service_b_span))
  azimuth::Span::set_attribute(service_c_span, "service.name", azimuth::StringValue("service-c"))
  azimuth::Span::set_attribute(service_c_span, "transaction.id", azimuth::StringValue(transaction_id))
  azimuth::Span::set_attribute(service_c_span, "user.id", azimuth::StringValue(user_id))
  azimuth::Span::set_attribute(service_c_span, "session.id", azimuth::StringValue(session_id))
  azimuth::Span::set_attribute(service_c_span, "operation.type", azimuth::StringValue("data_storage"))
  
  // Verify transaction consistency across all spans
  assert_eq(azimuth::Span::get_attribute(service_a_span, "transaction.id"), Some(azimuth::StringValue(transaction_id)))
  assert_eq(azimuth::Span::get_attribute(service_b_span, "transaction.id"), Some(azimuth::StringValue(transaction_id)))
  assert_eq(azimuth::Span::get_attribute(service_c_span, "transaction.id"), Some(azimuth::StringValue(transaction_id)))
  
  assert_eq(azimuth::Span::get_attribute(service_a_span, "user.id"), Some(azimuth::StringValue(user_id)))
  assert_eq(azimuth::Span::get_attribute(service_b_span, "user.id"), Some(azimuth::StringValue(user_id)))
  assert_eq(azimuth::Span::get_attribute(service_c_span, "user.id"), Some(azimuth::StringValue(user_id)))
  
  assert_eq(azimuth::Span::get_attribute(service_a_span, "session.id"), Some(azimuth::StringValue(session_id)))
  assert_eq(azimuth::Span::get_attribute(service_b_span, "session.id"), Some(azimuth::StringValue(session_id)))
  assert_eq(azimuth::Span::get_attribute(service_c_span, "session.id"), Some(azimuth::StringValue(session_id)))
  
  // Verify trace context consistency
  let service_a_context = azimuth::Span::context(service_a_span)
  let service_b_context = azimuth::Span::context(service_b_span)
  let service_c_context = azimuth::Span::context(service_c_span)
  
  assert_eq(azimuth::SpanContext::trace_id(service_a_context), azimuth::SpanContext::trace_id(service_b_context))
  assert_eq(azimuth::SpanContext::trace_id(service_b_context), azimuth::SpanContext::trace_id(service_c_context))
  
  // End spans in reverse order (proper span lifecycle)
  azimuth::Span::set_status_code(service_c_span, azimuth::Ok)
  azimuth::Span::end(service_c_span)
  
  azimuth::Span::set_status_code(service_b_span, azimuth::Ok)
  azimuth::Span::end(service_b_span)
  
  azimuth::Span::set_status_code(service_a_span, azimuth::Ok)
  azimuth::Span::end(service_a_span)
}