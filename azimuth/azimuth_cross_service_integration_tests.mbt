// Azimuth Cross-Service Integration Test Suite
// This file contains cross-service integration test cases for the Azimuth telemetry system

// Test 1: End-to-end trace propagation across services
pub test "cross service trace propagation" {
  // Simulate a trace flowing through multiple services
  
  // Service A: Entry point (client-facing)
  let service_a_trace_id = "trace-12345"
  let service_a_span_id = "span-a-67890"
  let service_a_ctx = azimuth::SpanContext::new(service_a_trace_id, service_a_span_id, true, "key1=value1")
  let service_a_span = azimuth::Span::new("service-a-operation", azimuth::Server, service_a_ctx)
  
  // Service B: Middleware service
  let service_b_span_id = "span-b-11111"
  let service_b_ctx = azimuth::SpanContext::new(service_a_trace_id, service_b_span_id, true, "key1=value1,key2=value2")
  let service_b_span = azimuth::Span::new("service-b-operation", azimuth::Internal, service_b_ctx)
  
  // Service C: Backend service
  let service_c_span_id = "span-c-22222"
  let service_c_ctx = azimuth::SpanContext::new(service_a_trace_id, service_c_span_id, true, "key1=value1,key2=value2,key3=value3")
  let service_c_span = azimuth::Span::new("service-c-operation", azimuth::Server, service_c_ctx)
  
  // Service D: Database service
  let service_d_span_id = "span-d-33333"
  let service_d_ctx = azimuth::SpanContext::new(service_a_trace_id, service_d_span_id, true, "key1=value1,key2=value2,key3=value3,key4=value4")
  let service_d_span = azimuth::Span::new("service-d-operation", azimuth::Client, service_d_ctx)
  
  // Verify trace consistency across services
  assert_eq(azimuth::SpanContext::trace_id(service_a_ctx), service_a_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_b_ctx), service_a_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_c_ctx), service_a_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_d_ctx), service_a_trace_id)
  
  // Verify span IDs are unique
  assert_not_eq(service_a_span_id, service_b_span_id)
  assert_not_eq(service_b_span_id, service_c_span_id)
  assert_not_eq(service_c_span_id, service_d_span_id)
  
  // Verify all contexts are valid and sampled
  assert_true(azimuth::SpanContext::is_valid(service_a_ctx))
  assert_true(azimuth::SpanContext::is_valid(service_b_ctx))
  assert_true(azimuth::SpanContext::is_valid(service_c_ctx))
  assert_true(azimuth::SpanContext::is_valid(service_d_ctx))
  
  assert_true(azimuth::SpanContext::is_sampled(service_a_ctx))
  assert_true(azimuth::SpanContext::is_sampled(service_b_ctx))
  assert_true(azimuth::SpanContext::is_sampled(service_c_ctx))
  assert_true(azimuth::SpanContext::is_sampled(service_d_ctx))
}

// Test 2: Cross-service baggage propagation
pub test "cross service baggage propagation" {
  // Service A: Initial baggage
  let service_a_baggage = azimuth::Baggage::new()
  let service_a_baggage_with_entries = azimuth::Baggage::set_entry(service_a_baggage, "user.id", "12345")
  let service_a_final_baggage = azimuth::Baggage::set_entry(service_a_baggage_with_entries, "request.id", "req-67890")
  
  // Service B: Adds additional baggage
  let service_b_baggage = azimuth::Baggage::set_entry(service_a_final_baggage, "service.b.timestamp", "2025-01-01T12:00:00Z")
  let service_b_final_baggage = azimuth::Baggage::set_entry(service_b_baggage, "service.b.session", "session-abc")
  
  // Service C: Adds more baggage
  let service_c_baggage = azimuth::Baggage::set_entry(service_b_final_baggage, "service.c.correlation", "corr-xyz")
  let service_c_final_baggage = azimuth::Baggage::set_entry(service_c_baggage, "service.c.priority", "high")
  
  // Service D: Final service with all baggage
  let service_d_baggage = azimuth::Baggage::set_entry(service_c_final_baggage, "service.d.db.query", "select-users")
  
  // Verify baggage propagation across services
  assert_eq(azimuth::Baggage::get_entry(service_a_final_baggage, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(service_a_final_baggage, "request.id"), Some("req-67890"))
  
  assert_eq(azimuth::Baggage::get_entry(service_b_final_baggage, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(service_b_final_baggage, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_b_final_baggage, "service.b.timestamp"), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(service_b_final_baggage, "service.b.session"), Some("session-abc"))
  
  assert_eq(azimuth::Baggage::get_entry(service_c_final_baggage, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(service_c_final_baggage, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_c_final_baggage, "service.b.timestamp"), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(service_c_final_baggage, "service.b.session"), Some("session-abc"))
  assert_eq(azimuth::Baggage::get_entry(service_c_final_baggage, "service.c.correlation"), Some("corr-xyz"))
  assert_eq(azimuth::Baggage::get_entry(service_c_final_baggage, "service.c.priority"), Some("high"))
  
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage, "service.b.timestamp"), Some("2025-01-01T12:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage, "service.b.session"), Some("session-abc"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage, "service.c.correlation"), Some("corr-xyz"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage, "service.c.priority"), Some("high"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage, "service.d.db.query"), Some("select-users"))
}

// Test 3: Cross-service metrics aggregation
pub test "cross service metrics aggregation" {
  // Service A: Creates metrics
  let service_a_provider = azimuth::MeterProvider::default()
  let service_a_meter = azimuth::MeterProvider::get_meter(service_a_provider, "service-a")
  let service_a_counter = azimuth::Meter::create_counter(service_a_meter, "http.requests.total")
  let service_a_histogram = azimuth::Meter::create_histogram(service_a_meter, "http.request.duration")
  
  // Service B: Creates metrics
  let service_b_provider = azimuth::MeterProvider::default()
  let service_b_meter = azimuth::MeterProvider::get_meter(service_b_provider, "service-b")
  let service_b_counter = azimuth::Meter::create_counter(service_b_meter, "http.requests.total")
  let service_b_histogram = azimuth::Meter::create_histogram(service_b_meter, "http.request.duration")
  
  // Service C: Creates metrics
  let service_c_provider = azimuth::MeterProvider::default()
  let service_c_meter = azimuth::MeterProvider::get_meter(service_c_provider, "service-c")
  let service_c_counter = azimuth::Meter::create_counter(service_c_meter, "http.requests.total")
  let service_c_histogram = azimuth::Meter::create_histogram(service_c_meter, "http.request.duration")
  
  // Simulate requests across services
  for i = 0; i < 10; i = i + 1 {
    // Service A processes request
    azimuth::Counter::add(service_a_counter, 1.0)
    azimuth::Histogram::record(service_a_histogram, 50.0 + i.to_double())
    
    // Service B processes request
    azimuth::Counter::add(service_b_counter, 1.0)
    azimuth::Histogram::record(service_b_histogram, 100.0 + i.to_double())
    
    // Service C processes request
    azimuth::Counter::add(service_c_counter, 1.0)
    azimuth::Histogram::record(service_c_histogram, 25.0 + i.to_double())
  }
  
  // Verify metrics instrumentation
  assert_eq(service_a_counter.name, "http.requests.total")
  assert_eq(service_a_histogram.name, "http.request.duration")
  
  assert_eq(service_b_counter.name, "http.requests.total")
  assert_eq(service_b_histogram.name, "http.request.duration")
  
  assert_eq(service_c_counter.name, "http.requests.total")
  assert_eq(service_c_histogram.name, "http.request.duration")
}

// Test 4: Cross-service log correlation
pub test "cross service log correlation" {
  // Service A: Logger
  let service_a_logger_provider = azimuth::LoggerProvider::default()
  let service_a_logger = azimuth::LoggerProvider::get_logger(service_a_logger_provider, "service-a")
  
  // Service B: Logger
  let service_b_logger_provider = azimuth::LoggerProvider::default()
  let service_b_logger = azimuth::LoggerProvider::get_logger(service_b_logger_provider, "service-b")
  
  // Service C: Logger
  let service_c_logger_provider = azimuth::LoggerProvider::default()
  let service_c_logger = azimuth::LoggerProvider::get_logger(service_c_logger_provider, "service-c")
  
  // Common trace ID for correlation
  let trace_id = "correlation-trace-12345"
  
  // Service A logs
  let service_a_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service A: Processing request"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some(trace_id),
    Some("span-a-67890"),
    None
  )
  
  // Service B logs
  let service_b_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service B: Handling middleware logic"),
    None,
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some(trace_id),
    Some("span-b-11111"),
    None
  )
  
  // Service C logs
  let service_c_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service C: Accessing database"),
    None,
    Some(1735689600000000004L),
    Some(1735689600000000005L),
    Some(trace_id),
    Some("span-c-22222"),
    None
  )
  
  // Emit logs from all services
  azimuth::Logger::emit(service_a_logger, service_a_log)
  azimuth::Logger::emit(service_b_logger, service_b_log)
  azimuth::Logger::emit(service_c_logger, service_c_log)
  
  // Verify log correlation
  assert_eq(azimuth::LogRecord::trace_id(service_a_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_b_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_c_log), Some(trace_id))
  
  assert_eq(azimuth::LogRecord::span_id(service_a_log), Some("span-a-67890"))
  assert_eq(azimuth::LogRecord::span_id(service_b_log), Some("span-b-11111"))
  assert_eq(azimuth::LogRecord::span_id(service_c_log), Some("span-c-22222"))
  
  assert_eq(azimuth::LogRecord::body(service_a_log), Some("Service A: Processing request"))
  assert_eq(azimuth::LogRecord::body(service_b_log), Some("Service B: Handling middleware logic"))
  assert_eq(azimuth::LogRecord::body(service_c_log), Some("Service C: Accessing database"))
}

// Test 5: Cross-service resource consistency
pub test "cross service resource consistency" {
  // Common resource attributes across services
  let common_attributes = [
    ("service.namespace", azimuth::StringValue("production")),
    ("service.version", azimuth::StringValue("1.2.3")),
    ("deployment.environment", azimuth::StringValue("production"))
  ]
  
  // Service A: Resource with service-specific attributes
  let service_a_base_resource = azimuth::Resource::new()
  let service_a_specific_attributes = [
    ("service.name", azimuth::StringValue("service-a")),
    ("service.instance.id", azimuth::StringValue("instance-a-123")),
    ("host.name", azimuth::StringValue("host-a.example.com"))
  ]
  let service_a_all_attributes = common_attributes.concat(service_a_specific_attributes)
  let service_a_resource = azimuth::Resource::with_attributes(service_a_base_resource, service_a_all_attributes)
  
  // Service B: Resource with service-specific attributes
  let service_b_base_resource = azimuth::Resource::new()
  let service_b_specific_attributes = [
    ("service.name", azimuth::StringValue("service-b")),
    ("service.instance.id", azimuth::StringValue("instance-b-456")),
    ("host.name", azimuth::StringValue("host-b.example.com"))
  ]
  let service_b_all_attributes = common_attributes.concat(service_b_specific_attributes)
  let service_b_resource = azimuth::Resource::with_attributes(service_b_base_resource, service_b_all_attributes)
  
  // Service C: Resource with service-specific attributes
  let service_c_base_resource = azimuth::Resource::new()
  let service_c_specific_attributes = [
    ("service.name", azimuth::StringValue("service-c")),
    ("service.instance.id", azimuth::StringValue("instance-c-789")),
    ("host.name", azimuth::StringValue("host-c.example.com"))
  ]
  let service_c_all_attributes = common_attributes.concat(service_c_specific_attributes)
  let service_c_resource = azimuth::Resource::with_attributes(service_c_base_resource, service_c_all_attributes)
  
  // Verify common resource attributes across services
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "service.namespace"), Some(azimuth::StringValue("production")))
  
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "service.version"), Some(azimuth::StringValue("1.2.3")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "service.version"), Some(azimuth::StringValue("1.2.3")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "service.version"), Some(azimuth::StringValue("1.2.3")))
  
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  
  // Verify service-specific attributes
  assert_eq(azimuth::Resource::get_attribute(service_a_resource, "service.name"), Some(azimuth::StringValue("service-a")))
  assert_eq(azimuth::Resource::get_attribute(service_b_resource, "service.name"), Some(azimuth::StringValue("service-b")))
  assert_eq(azimuth::Resource::get_attribute(service_c_resource, "service.name"), Some(azimuth::StringValue("service-c")))
}

// Test 6: Cross-service context propagation with HTTP headers
pub test "cross service context propagation http" {
  // Service A: Creates context and injects into HTTP headers
  let service_a_ctx = azimuth::Context::root()
  let service_a_key = azimuth::ContextKey::new("user.id")
  let service_a_ctx_with_value = azimuth::Context::with_value(service_a_ctx, service_a_key, "12345")
  
  let service_a_baggage = azimuth::Baggage::new()
  let service_a_baggage_with_entry = azimuth::Baggage::set_entry(service_a_baggage, "request.id", "req-67890")
  
  let service_a_span_ctx = azimuth::SpanContext::new("trace-12345", "span-a-67890", true, "key1=value1")
  
  // Create HTTP carrier for propagation
  let service_a_carrier = azimuth::TextMapCarrier::new()
  
  // Inject context into carrier (simulating HTTP headers)
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CTraceContextPropagator::new()  // Using trace propagator for simplicity
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  azimuth::CompositePropagator::inject(composite_propagator, service_a_ctx_with_value, service_a_carrier)
  
  // Service B: Extracts context from HTTP headers
  let service_b_carrier = service_a_carrier  // In a real scenario, this would be received via HTTP
  let service_b_extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, service_b_carrier)
  
  // Service B adds its own context
  let service_b_key = azimuth::ContextKey::new("service.b.timestamp")
  let service_b_ctx_with_value = azimuth::Context::with_value(service_b_extracted_ctx, service_b_key, "2025-01-01T12:00:00Z")
  
  // Service B creates new carrier for Service C
  let service_b_carrier_for_c = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, service_b_ctx_with_value, service_b_carrier_for_c)
  
  // Service C: Extracts context from HTTP headers
  let service_c_carrier = service_b_carrier_for_c  // In a real scenario, this would be received via HTTP
  let service_c_extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, service_c_carrier)
  
  // Verify context propagation across services
  let service_b_extracted_key = azimuth::ContextKey::new("extracted")
  let service_b_extracted_value = azimuth::Context::get(service_b_extracted_ctx, service_b_extracted_key)
  assert_eq(service_b_extracted_value, Some("true"))
  
  let service_c_extracted_key = azimuth::ContextKey::new("extracted")
  let service_c_extracted_value = azimuth::Context::get(service_c_extracted_ctx, service_c_extracted_key)
  assert_eq(service_c_extracted_value, Some("true"))
}

// Test 7: Cross-service error propagation
pub test "cross service error propagation" {
  // Service A: Initiates operation
  let service_a_trace_id = "error-trace-12345"
  let service_a_span_id = "span-a-67890"
  let service_a_ctx = azimuth::SpanContext::new(service_a_trace_id, service_a_span_id, true, "")
  let service_a_span = azimuth::Span::new("service-a-operation", azimuth::Server, service_a_ctx)
  
  // Service A sets error status
  azimuth::Span::set_status(service_a_span, azimuth::Error, Some("Service A: Invalid input"))
  
  // Service B: Receives error and propagates
  let service_b_span_id = "span-b-11111"
  let service_b_ctx = azimuth::SpanContext::new(service_a_trace_id, service_b_span_id, true, "")
  let service_b_span = azimuth::Span::new("service-b-operation", azimuth::Internal, service_b_ctx)
  
  // Service B adds its own error information
  azimuth::Span::set_status(service_b_span, azimuth::Error, Some("Service B: Validation failed"))
  
  // Service C: Final error handler
  let service_c_span_id = "span-c-22222"
  let service_c_ctx = azimuth::SpanContext::new(service_a_trace_id, service_c_span_id, true, "")
  let service_c_span = azimuth::Span::new("service-c-operation", azimuth::Server, service_c_ctx)
  
  // Service C handles the error
  azimuth::Span::set_status(service_c_span, azimuth::Error, Some("Service C: Database connection failed"))
  
  // Verify error propagation
  assert_eq(azimuth::Span::status(service_a_span), azimuth::Error)
  assert_eq(azimuth::Span::status(service_b_span), azimuth::Error)
  assert_eq(azimuth::Span::status(service_c_span), azimuth::Error)
  
  // Verify trace consistency
  assert_eq(azimuth::SpanContext::trace_id(service_a_ctx), service_a_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_b_ctx), service_a_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_c_ctx), service_a_trace_id)
  
  // Create error logs for correlation
  let service_a_logger_provider = azimuth::LoggerProvider::default()
  let service_a_logger = azimuth::LoggerProvider::get_logger(service_a_logger_provider, "service-a")
  
  let service_b_logger_provider = azimuth::LoggerProvider::default()
  let service_b_logger = azimuth::LoggerProvider::get_logger(service_b_logger_provider, "service-b")
  
  let service_c_logger_provider = azimuth::LoggerProvider::default()
  let service_c_logger = azimuth::LoggerProvider::get_logger(service_c_logger_provider, "service-c")
  
  let service_a_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Service A: Invalid input parameter"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some(service_a_trace_id),
    Some(service_a_span_id),
    None
  )
  
  let service_b_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Service B: Input validation failed"),
    None,
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some(service_a_trace_id),
    Some(service_b_span_id),
    None
  )
  
  let service_c_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Service C: Unable to connect to database"),
    None,
    Some(1735689600000000004L),
    Some(1735689600000000005L),
    Some(service_a_trace_id),
    Some(service_c_span_id),
    None
  )
  
  // Emit error logs
  azimuth::Logger::emit(service_a_logger, service_a_error_log)
  azimuth::Logger::emit(service_b_logger, service_b_error_log)
  azimuth::Logger::emit(service_c_logger, service_c_error_log)
  
  // Verify error log correlation
  assert_eq(azimuth::LogRecord::trace_id(service_a_error_log), Some(service_a_trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_b_error_log), Some(service_a_trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_c_error_log), Some(service_a_trace_id))
  
  assert_eq(azimuth::LogRecord::severity_number(service_a_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(service_b_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(service_c_error_log), azimuth::Error)
}

// Test 8: Cross-service performance monitoring
pub test "cross service performance monitoring" {
  // Service A: Entry point
  let service_a_provider = azimuth::MeterProvider::default()
  let service_a_meter = azimuth::MeterProvider::get_meter(service_a_provider, "service-a")
  let service_a_request_counter = azimuth::Meter::create_counter(service_a_meter, "http.requests.total")
  let service_a_duration_histogram = azimuth::Meter::create_histogram(service_a_meter, "http.request.duration")
  let service_a_active_gauge = azimuth::Meter::create_gauge(service_a_meter, "http.requests.active")
  
  // Service B: Middleware
  let service_b_provider = azimuth::MeterProvider::default()
  let service_b_meter = azimuth::MeterProvider::get_meter(service_b_provider, "service-b")
  let service_b_request_counter = azimuth::Meter::create_counter(service_b_meter, "http.requests.total")
  let service_b_duration_histogram = azimuth::Meter::create_histogram(service_b_meter, "http.request.duration")
  let service_b_active_gauge = azimuth::Meter::create_gauge(service_b_meter, "http.requests.active")
  
  // Service C: Backend
  let service_c_provider = azimuth::MeterProvider::default()
  let service_c_meter = azimuth::MeterProvider::get_meter(service_c_provider, "service-c")
  let service_c_request_counter = azimuth::Meter::create_counter(service_c_meter, "http.requests.total")
  let service_c_duration_histogram = azimuth::Meter::create_histogram(service_c_meter, "http.request.duration")
  let service_c_active_gauge = azimuth::Meter::create_gauge(service_c_meter, "http.requests.active")
  
  // Simulate request processing across services
  for i = 0; i < 50; i = i + 1 {
    // Service A processes request
    azimuth::Counter::add(service_a_request_counter, 1.0)
    azimuth::Histogram::record(service_a_duration_histogram, 50.0 + (i % 10).to_double())
    
    // Service B processes request
    azimuth::Counter::add(service_b_request_counter, 1.0)
    azimuth::Histogram::record(service_b_duration_histogram, 100.0 + (i % 15).to_double())
    
    // Service C processes request
    azimuth::Counter::add(service_c_request_counter, 1.0)
    azimuth::Histogram::record(service_c_duration_histogram, 25.0 + (i % 5).to_double())
  }
  
  // Create performance logs
  let service_a_logger_provider = azimuth::LoggerProvider::default()
  let service_a_logger = azimuth::LoggerProvider::get_logger(service_a_logger_provider, "service-a")
  
  let service_b_logger_provider = azimuth::LoggerProvider::default()
  let service_b_logger = azimuth::LoggerProvider::get_logger(service_b_logger_provider, "service-b")
  
  let service_c_logger_provider = azimuth::LoggerProvider::default()
  let service_c_logger = azimuth::LoggerProvider::get_logger(service_c_logger_provider, "service-c")
  
  let performance_trace_id = "performance-trace-12345"
  
  let service_a_perf_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service A: Request processed in 55ms"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some(performance_trace_id),
    Some("span-a-perf"),
    None
  )
  
  let service_b_perf_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service B: Request processed in 107ms"),
    None,
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some(performance_trace_id),
    Some("span-b-perf"),
    None
  )
  
  let service_c_perf_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service C: Request processed in 27ms"),
    None,
    Some(1735689600000000004L),
    Some(1735689600000000005L),
    Some(performance_trace_id),
    Some("span-c-perf"),
    None
  )
  
  // Emit performance logs
  azimuth::Logger::emit(service_a_logger, service_a_perf_log)
  azimuth::Logger::emit(service_b_logger, service_b_perf_log)
  azimuth::Logger::emit(service_c_logger, service_c_perf_log)
  
  // Verify performance metrics
  assert_eq(service_a_request_counter.name, "http.requests.total")
  assert_eq(service_a_duration_histogram.name, "http.request.duration")
  assert_eq(service_a_active_gauge.name, "http.requests.active")
  
  assert_eq(service_b_request_counter.name, "http.requests.total")
  assert_eq(service_b_duration_histogram.name, "http.request.duration")
  assert_eq(service_b_active_gauge.name, "http.requests.active")
  
  assert_eq(service_c_request_counter.name, "http.requests.total")
  assert_eq(service_c_duration_histogram.name, "http.request.duration")
  assert_eq(service_c_active_gauge.name, "http.requests.active")
  
  // Verify performance log correlation
  assert_eq(azimuth::LogRecord::trace_id(service_a_perf_log), Some(performance_trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_b_perf_log), Some(performance_trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_c_perf_log), Some(performance_trace_id))
}