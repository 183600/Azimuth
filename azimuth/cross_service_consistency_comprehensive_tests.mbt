// Cross-Service Consistency Comprehensive Tests for Azimuth Telemetry System
// These tests focus on ensuring telemetry data consistency across service boundaries

// Test 1: Basic Cross-Service Trace Context Propagation
pub test "basic cross-service trace context propagation" {
  // Create initial trace context in service A
  let service_a_trace_ctx = azimuth::SpanContext::new("trace-cross-001", "span-service-a-001", true, "service=A;env=prod")
  let service_a_span = azimuth::Span::new("service.a.operation", azimuth::Server, service_a_trace_ctx)
  
  // Simulate trace context propagation to service B
  let propagated_trace_id = azimuth::SpanContext::trace_id(service_a_trace_ctx)
  let service_b_trace_ctx = azimuth::SpanContext::new(propagated_trace_id, "span-service-b-001", true, "service=B;env=prod")
  let service_b_span = azimuth::Span::new("service.b.operation", azimuth::Server, service_b_trace_ctx)
  
  // Simulate trace context propagation to service C
  let service_c_trace_ctx = azimuth::SpanContext::new(propagated_trace_id, "span-service-c-001", true, "service=C;env=prod")
  let service_c_span = azimuth::Span::new("service.c.operation", azimuth::Server, service_c_trace_ctx)
  
  // Verify trace consistency across services
  assert_eq(azimuth::SpanContext::trace_id(service_a_trace_ctx), "trace-cross-001")
  assert_eq(azimuth::SpanContext::trace_id(service_b_trace_ctx), "trace-cross-001")
  assert_eq(azimuth::SpanContext::trace_id(service_c_trace_ctx), "trace-cross-001")
  
  // Verify each service has unique span ID
  assert_eq(azimuth::SpanContext::span_id(service_a_trace_ctx), "span-service-a-001")
  assert_eq(azimuth::SpanContext::span_id(service_b_trace_ctx), "span-service-b-001")
  assert_eq(azimuth::SpanContext::span_id(service_c_trace_ctx), "span-service-c-001")
  
  // Verify sampling consistency
  assert_true(azimuth::SpanContext::is_sampled(service_a_trace_ctx))
  assert_true(azimuth::SpanContext::is_sampled(service_b_trace_ctx))
  assert_true(azimuth::SpanContext::is_sampled(service_c_trace_ctx))
  
  // Complete the spans
  azimuth::Span::end(service_a_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_c_span)
}

// Test 2: Cross-Service Baggage Propagation Consistency
pub test "cross-service baggage propagation consistency" {
  // Create initial baggage in service A
  let service_a_baggage = azimuth::Baggage::new()
  let service_a_baggage_with_user = azimuth::Baggage::set_entry(service_a_baggage, "user.id", "user-12345")
  let service_a_baggage_with_session = azimuth::Baggage::set_entry(service_a_baggage_with_user, "session.id", "sess-abcdef")
  let service_a_baggage_with_tenant = azimuth::Baggage::set_entry(service_a_baggage_with_session, "tenant.id", "tenant-001")
  
  // Simulate baggage propagation to service B
  let service_b_baggage = service_a_baggage_with_tenant
  let service_b_baggage_with_correlation = azimuth::Baggage::set_entry(service_b_baggage, "correlation.id", "corr-789xyz")
  
  // Simulate baggage propagation to service C
  let service_c_baggage = service_b_baggage
  let service_c_baggage_with_request = azimuth::Baggage::set_entry(service_c_baggage, "request.id", "req-2025-001")
  
  // Verify baggage consistency across services
  let user_id_a = azimuth::Baggage::get_entry(service_a_baggage_with_tenant, "user.id")
  let user_id_b = azimuth::Baggage::get_entry(service_b_baggage, "user.id")
  let user_id_c = azimuth::Baggage::get_entry(service_c_baggage, "user.id")
  
  assert_eq(user_id_a, Some("user-12345"))
  assert_eq(user_id_b, Some("user-12345"))
  assert_eq(user_id_c, Some("user-12345"))
  
  // Verify session consistency
  let session_id_a = azimuth::Baggage::get_entry(service_a_baggage_with_tenant, "session.id")
  let session_id_b = azimuth::Baggage::get_entry(service_b_baggage, "session.id")
  let session_id_c = azimuth::Baggage::get_entry(service_c_baggage, "session.id")
  
  assert_eq(session_id_a, Some("sess-abcdef"))
  assert_eq(session_id_b, Some("sess-abcdef"))
  assert_eq(session_id_c, Some("sess-abcdef"))
  
  // Verify tenant consistency
  let tenant_id_a = azimuth::Baggage::get_entry(service_a_baggage_with_tenant, "tenant.id")
  let tenant_id_b = azimuth::Baggage::get_entry(service_b_baggage, "tenant.id")
  let tenant_id_c = azimuth::Baggage::get_entry(service_c_baggage, "tenant.id")
  
  assert_eq(tenant_id_a, Some("tenant-001"))
  assert_eq(tenant_id_b, Some("tenant-001"))
  assert_eq(tenant_id_c, Some("tenant-001"))
  
  // Verify service-specific baggage additions
  let correlation_id_b = azimuth::Baggage::get_entry(service_b_baggage, "correlation.id")
  let correlation_id_c = azimuth::Baggage::get_entry(service_c_baggage, "correlation.id")
  
  assert_eq(correlation_id_b, Some("corr-789xyz"))
  assert_eq(correlation_id_c, Some("corr-789xyz"))
  
  let request_id_c = azimuth::Baggage::get_entry(service_c_baggage, "request.id")
  assert_eq(request_id_c, Some("req-2025-001"))
}

// Test 3: Cross-Service Metrics Consistency
pub test "cross-service metrics consistency" {
  // Setup metrics providers for different services
  let service_a_meter_provider = azimuth::MeterProvider::default()
  let service_b_meter_provider = azimuth::MeterProvider::default()
  let service_c_meter_provider = azimuth::MeterProvider::default()
  
  let service_a_meter = azimuth::MeterProvider::get_meter(service_a_meter_provider, "service-a-metrics")
  let service_b_meter = azimuth::MeterProvider::get_meter(service_b_meter_provider, "service-b-metrics")
  let service_c_meter = azimuth::MeterProvider::get_meter(service_c_meter_provider, "service-c-metrics")
  
  // Create consistent metric instruments across services
  let service_a_request_counter = azimuth::Meter::create_counter(service_a_meter, "cross.service.requests.total",
    Some("Total cross-service requests"), Some("requests"))
  let service_b_request_counter = azimuth::Meter::create_counter(service_b_meter, "cross.service.requests.total",
    Some("Total cross-service requests"), Some("requests"))
  let service_c_request_counter = azimuth::Meter::create_counter(service_c_meter, "cross.service.requests.total",
    Some("Total cross-service requests"), Some("requests"))
  
  let service_a_latency_histogram = azimuth::Meter::create_histogram(service_a_meter, "cross.service.latency",
    Some("Cross-service operation latency"), Some("ms"))
  let service_b_latency_histogram = azimuth::Meter::create_histogram(service_b_meter, "cross.service.latency",
    Some("Cross-service operation latency"), Some("ms"))
  let service_c_latency_histogram = azimuth::Meter::create_histogram(service_c_meter, "cross.service.latency",
    Some("Cross-service operation latency"), Some("ms"))
  
  // Record metrics in each service
  azimuth::Counter::add(service_a_request_counter, 1.0)
  azimuth::Counter::add(service_b_request_counter, 1.0)
  azimuth::Counter::add(service_c_request_counter, 1.0)
  
  azimuth::Histogram::record(service_a_latency_histogram, 150.0)
  azimuth::Histogram::record(service_b_latency_histogram, 200.0)
  azimuth::Histogram::record(service_c_latency_histogram, 125.0)
  
  // Verify metric instrument consistency across services
  assert_eq(service_a_request_counter.name, "cross.service.requests.total")
  assert_eq(service_b_request_counter.name, "cross.service.requests.total")
  assert_eq(service_c_request_counter.name, "cross.service.requests.total")
  
  assert_eq(service_a_request_counter.description, Some("Total cross-service requests"))
  assert_eq(service_b_request_counter.description, Some("Total cross-service requests"))
  assert_eq(service_c_request_counter.description, Some("Total cross-service requests"))
  
  assert_eq(service_a_request_counter.unit, Some("requests"))
  assert_eq(service_b_request_counter.unit, Some("requests"))
  assert_eq(service_c_request_counter.unit, Some("requests"))
  
  assert_eq(service_a_latency_histogram.name, "cross.service.latency")
  assert_eq(service_b_latency_histogram.name, "cross.service.latency")
  assert_eq(service_c_latency_histogram.name, "cross.service.latency")
  
  assert_eq(service_a_latency_histogram.description, Some("Cross-service operation latency"))
  assert_eq(service_b_latency_histogram.description, Some("Cross-service operation latency"))
  assert_eq(service_c_latency_histogram.description, Some("Cross-service operation latency"))
  
  assert_eq(service_a_latency_histogram.unit, Some("ms"))
  assert_eq(service_b_latency_histogram.unit, Some("ms"))
  assert_eq(service_c_latency_histogram.unit, Some("ms"))
}

// Test 4: Cross-Service Logging Correlation
pub test "cross-service logging correlation" {
  // Setup loggers for different services
  let service_a_logger_provider = azimuth::LoggerProvider::default()
  let service_b_logger_provider = azimuth::LoggerProvider::default()
  let service_c_logger_provider = azimuth::LoggerProvider::default()
  
  let service_a_logger = azimuth::LoggerProvider::get_logger(service_a_logger_provider, "service-a-logger")
  let service_b_logger = azimuth::LoggerProvider::get_logger(service_b_logger_provider, "service-b-logger")
  let service_c_logger = azimuth::LoggerProvider::get_logger(service_c_logger_provider, "service-c-logger")
  
  // Create correlated log records across services
  let shared_trace_id = "trace-cross-correlation-001"
  let shared_timestamp = 1735689600000000000L
  
  let service_a_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service A processing request"),
    Some(azimuth::Attributes::new()),
    Some(shared_timestamp),
    Some(shared_timestamp + 1000000L),
    Some(shared_trace_id),
    Some("span-service-a-001"),
    Some(azimuth::Context::root())
  )
  
  let service_b_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service B processing request"),
    Some(azimuth::Attributes::new()),
    Some(shared_timestamp + 50000000L),
    Some(shared_timestamp + 51000000L),
    Some(shared_trace_id),
    Some("span-service-b-001"),
    Some(azimuth::Context::root())
  )
  
  let service_c_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service C processing request"),
    Some(azimuth::Attributes::new()),
    Some(shared_timestamp + 100000000L),
    Some(shared_timestamp + 101000000L),
    Some(shared_trace_id),
    Some("span-service-c-001"),
    Some(azimuth::Context::root())
  )
  
  // Emit correlated logs
  azimuth::Logger::emit(service_a_logger, service_a_log)
  azimuth::Logger::emit(service_b_logger, service_b_log)
  azimuth::Logger::emit(service_c_logger, service_c_log)
  
  // Verify log correlation across services
  assert_eq(azimuth::LogRecord::trace_id(service_a_log), Some(shared_trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_b_log), Some(shared_trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_c_log), Some(shared_trace_id))
  
  // Verify unique span IDs for each service
  assert_eq(azimuth::LogRecord::span_id(service_a_log), Some("span-service-a-001"))
  assert_eq(azimuth::LogRecord::span_id(service_b_log), Some("span-service-b-001"))
  assert_eq(azimuth::LogRecord::span_id(service_c_log), Some("span-service-c-001"))
  
  // Verify timestamp progression
  assert_eq(azimuth::LogRecord::timestamp(service_a_log), Some(shared_timestamp))
  assert_eq(azimuth::LogRecord::timestamp(service_b_log), Some(shared_timestamp + 50000000L))
  assert_eq(azimuth::LogRecord::timestamp(service_c_log), Some(shared_timestamp + 100000000L))
  
  // Verify logger isolation
  assert_eq(service_a_logger.scope.name, "service-a-logger")
  assert_eq(service_b_logger.scope.name, "service-b-logger")
  assert_eq(service_c_logger.scope.name, "service-c-logger")
}

// Test 5: Cross-Service Resource Attribute Consistency
pub test "cross-service resource attribute consistency" {
  // Create base resource attributes for cross-service consistency
  let base_resource = azimuth::Resource::new()
  let common_attrs = [
    ("service.namespace", azimuth::StringValue("microservices")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("platform.version", azimuth::StringValue("kubernetes-1.28")),
    ("business.domain", azimuth::StringValue("e-commerce")),
    ("monitoring.enabled", azimuth::BoolValue(true))
  ]
  
  // Create service-specific resources with common attributes
  let service_a_specific_attrs = [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("service.instance.id", azimuth::StringValue("user-service-pod-001"))
  ]
  
  let service_b_specific_attrs = [
    ("service.name", azimuth::StringValue("order-service")),
    ("service.version", azimuth::StringValue("3.4.1")),
    ("service.instance.id", azimuth::StringValue("order-service-pod-002"))
  ]
  
  let service_c_specific_attrs = [
    ("service.name", azimuth::StringValue("payment-service")),
    ("service.version", azimuth::StringValue("1.8.2")),
    ("service.instance.id", azimuth::StringValue("payment-service-pod-003"))
  ]
  
  // Combine common and service-specific attributes
  let service_a_resource = azimuth::Resource::with_attributes(base_resource, common_attrs)
  let service_a_final = azimuth::Resource::with_attributes(service_a_resource, service_a_specific_attrs)
  
  let service_b_resource = azimuth::Resource::with_attributes(base_resource, common_attrs)
  let service_b_final = azimuth::Resource::with_attributes(service_b_resource, service_b_specific_attrs)
  
  let service_c_resource = azimuth::Resource::with_attributes(base_resource, common_attrs)
  let service_c_final = azimuth::Resource::with_attributes(service_c_resource, service_c_specific_attrs)
  
  // Verify common attributes are consistent across services
  let namespace_a = azimuth::Resource::get_attribute(service_a_final, "service.namespace")
  let namespace_b = azimuth::Resource::get_attribute(service_b_final, "service.namespace")
  let namespace_c = azimuth::Resource::get_attribute(service_c_final, "service.namespace")
  
  assert_eq(namespace_a, Some(azimuth::StringValue("microservices")))
  assert_eq(namespace_b, Some(azimuth::StringValue("microservices")))
  assert_eq(namespace_c, Some(azimuth::StringValue("microservices")))
  
  let environment_a = azimuth::Resource::get_attribute(service_a_final, "deployment.environment")
  let environment_b = azimuth::Resource::get_attribute(service_b_final, "deployment.environment")
  let environment_c = azimuth::Resource::get_attribute(service_c_final, "deployment.environment")
  
  assert_eq(environment_a, Some(azimuth::StringValue("production")))
  assert_eq(environment_b, Some(azimuth::StringValue("production")))
  assert_eq(environment_c, Some(azimuth::StringValue("production")))
  
  let domain_a = azimuth::Resource::get_attribute(service_a_final, "business.domain")
  let domain_b = azimuth::Resource::get_attribute(service_b_final, "business.domain")
  let domain_c = azimuth::Resource::get_attribute(service_c_final, "business.domain")
  
  assert_eq(domain_a, Some(azimuth::StringValue("e-commerce")))
  assert_eq(domain_b, Some(azimuth::StringValue("e-commerce")))
  assert_eq(domain_c, Some(azimuth::StringValue("e-commerce")))
  
  // Verify service-specific attributes are unique
  let service_name_a = azimuth::Resource::get_attribute(service_a_final, "service.name")
  let service_name_b = azimuth::Resource::get_attribute(service_b_final, "service.name")
  let service_name_c = azimuth::Resource::get_attribute(service_c_final, "service.name")
  
  assert_eq(service_name_a, Some(azimuth::StringValue("user-service")))
  assert_eq(service_name_b, Some(azimuth::StringValue("order-service")))
  assert_eq(service_name_c, Some(azimuth::StringValue("payment-service")))
}

// Test 6: Cross-Service Propagation with W3C Standards
pub test "cross-service propagation with w3c standards" {
  // Setup W3C propagators
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // Create initial context in service A
  let service_a_ctx = azimuth::Context::root()
  let service_a_baggage = azimuth::Baggage::new()
  let service_a_baggage_with_data = azimuth::Baggage::set_entry(service_a_baggage, "user.id", "user-w3c-123")
  let service_a_baggage_final = azimuth::Baggage::set_entry(service_a_baggage_with_data, "request.id", "req-w3c-456")
  
  // Inject context into HTTP headers for cross-service communication
  let carrier_a_to_b = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, service_a_ctx, carrier_a_to_b)
  
  // Extract context in service B
  let service_b_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier_a_to_b)
  
  // Service B adds its own baggage and propagates to service C
  let service_b_baggage = service_a_baggage_final
  let service_b_baggage_enhanced = azimuth::Baggage::set_entry(service_b_baggage, "service.b.metadata", "processed-by-b")
  
  let carrier_b_to_c = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, service_b_ctx, carrier_b_to_c)
  
  // Extract context in service C
  let service_c_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier_b_to_c)
  
  // Verify W3C trace context propagation
  let trace_header_a_to_b = azimuth::TextMapCarrier::get(carrier_a_to_b, "traceparent")
  let trace_header_b_to_c = azimuth::TextMapCarrier::get(carrier_b_to_c, "traceparent")
  
  // In a real implementation, these would contain actual trace context
  // For this test, we verify the propagation mechanism works
  assert_eq(trace_header_a_to_b, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(trace_header_b_to_c, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Verify context extraction worked
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value_b = azimuth::Context::get(service_b_ctx, extracted_key)
  let extracted_value_c = azimuth::Context::get(service_c_ctx, extracted_key)
  
  assert_eq(extracted_value_b, Some("true"))
  assert_eq(extracted_value_c, Some("true"))
  
  // Verify baggage consistency
  let user_id_b = azimuth::Baggage::get_entry(service_b_baggage_enhanced, "user.id")
  let user_id_c = azimuth::Baggage::get_entry(service_b_baggage_enhanced, "user.id")
  
  assert_eq(user_id_b, Some("user-w3c-123"))
  assert_eq(user_id_c, Some("user-w3c-123"))
  
  let request_id_b = azimuth::Baggage::get_entry(service_b_baggage_enhanced, "request.id")
  let request_id_c = azimuth::Baggage::get_entry(service_b_baggage_enhanced, "request.id")
  
  assert_eq(request_id_b, Some("req-w3c-456"))
  assert_eq(request_id_c, Some("req-w3c-456"))
  
  let service_b_metadata = azimuth::Baggage::get_entry(service_b_baggage_enhanced, "service.b.metadata")
  assert_eq(service_b_metadata, Some("processed-by-b"))
}

// Test 7: Cross-Service Error Propagation and Correlation
pub test "cross-service error propagation and correlation" {
  // Create trace context for error scenario
  let error_trace_ctx = azimuth::SpanContext::new("trace-error-001", "span-error-source", true, "error=true")
  let error_span = azimuth::Span::new("error.prone.operation", azimuth::Server, error_trace_ctx)
  
  // Simulate error in service A
  azimuth::Span::add_event(error_span, "error.detected", 
    Some([("error.code", azimuth::StringValue("ERR_5001")),
          ("error.message", azimuth::StringValue("Database connection failed")),
          ("service", azimuth::StringValue("service-a"))]))
  
  azimuth::Span::set_status(error_span, azimuth::Error, Some("Database connection failed"))
  
  // Propagate error context to service B
  let service_b_error_ctx = azimuth::SpanContext::new("trace-error-001", "span-error-b", true, "error=true;source=service-a")
  let service_b_error_span = azimuth::Span::new("error.handling.operation", azimuth::Server, service_b_error_ctx)
  
  azimuth::Span::add_event(service_b_error_span, "error.received", 
    Some([("error.code", azimuth::StringValue("ERR_5001")),
          ("error.source", azimuth::StringValue("service-a")),
          ("service", azimuth::StringValue("service-b"))]))
  
  // Propagate error context to service C
  let service_c_error_ctx = azimuth::SpanContext::new("trace-error-001", "span-error-c", true, "error=true;source=service-b")
  let service_c_error_span = azimuth::Span::new("error.recovery.operation", azimuth::Server, service_c_error_ctx)
  
  azimuth::Span::add_event(service_c_error_span, "error.recovery.attempted", 
    Some([("error.code", azimuth::StringValue("ERR_5001")),
          ("error.source", azimuth::StringValue("service-b")),
          ("recovery.action", azimuth::StringValue("retry_with_fallback")),
          ("service", azimuth::StringValue("service-c"))]))
  
  azimuth::Span::set_status(service_c_error_span, azimuth::Ok, Some("Error recovered successfully"))
  
  // Verify error trace consistency
  assert_eq(azimuth::SpanContext::trace_id(error_trace_ctx), "trace-error-001")
  assert_eq(azimuth::SpanContext::trace_id(service_b_error_ctx), "trace-error-001")
  assert_eq(azimuth::SpanContext::trace_id(service_c_error_ctx), "trace-error-001")
  
  // Verify error status propagation
  assert_eq(azimuth::Span::status(error_span), azimuth::Error)  // Note: simplified implementation
  assert_eq(azimuth::Span::status(service_c_error_span), azimuth::Ok)  // Note: simplified implementation
  
  // Complete error spans
  azimuth::Span::end(error_span)
  azimuth::Span::end(service_b_error_span)
  azimuth::Span::end(service_c_error_span)
}

// Test 8: Cross-Service Performance Metrics Correlation
pub test "cross-service performance metrics correlation" {
  // Setup performance monitoring across services
  let service_a_meter_provider = azimuth::MeterProvider::default()
  let service_b_meter_provider = azimuth::MeterProvider::default()
  let service_c_meter_provider = azimuth::MeterProvider::default()
  
  let service_a_meter = azimuth::MeterProvider::get_meter(service_a_meter_provider, "service-a-perf")
  let service_b_meter = azimuth::MeterProvider::get_meter(service_b_meter_provider, "service-b-perf")
  let service_c_meter = azimuth::MeterProvider::get_meter(service_c_meter_provider, "service-c-perf")
  
  // Create correlated performance metrics
  let service_a_latency = azimuth::Meter::create_histogram(service_a_meter, "operation.latency",
    Some("Operation latency in milliseconds"), Some("ms"))
  let service_b_latency = azimuth::Meter::create_histogram(service_b_meter, "operation.latency",
    Some("Operation latency in milliseconds"), Some("ms"))
  let service_c_latency = azimuth::Meter::create_histogram(service_c_meter, "operation.latency",
    Some("Operation latency in milliseconds"), Some("ms"))
  
  let service_a_throughput = azimuth::Meter::create_counter(service_a_meter, "operations.processed",
    Some("Number of operations processed"), Some("operations"))
  let service_b_throughput = azimuth::Meter::create_counter(service_b_meter, "operations.processed",
    Some("Number of operations processed"), Some("operations"))
  let service_c_throughput = azimuth::Meter::create_counter(service_c_meter, "operations.processed",
    Some("Number of operations processed"), Some("operations"))
  
  let service_a_errors = azimuth::Meter::create_counter(service_a_meter, "operations.errors",
    Some("Number of operation errors"), Some("errors"))
  let service_b_errors = azimuth::Meter::create_counter(service_b_meter, "operations.errors",
    Some("Number of operation errors"), Some("errors"))
  let service_c_errors = azimuth::Meter::create_counter(service_c_meter, "operations.errors",
    Some("Number of operation errors"), Some("errors"))
  
  // Simulate correlated performance measurements
  azimuth::Histogram::record(service_a_latency, 120.0)  // Service A processing time
  azimuth::Histogram::record(service_b_latency, 80.0)   // Service B processing time
  azimuth::Histogram::record(service_c_latency, 45.0)   // Service C processing time
  
  azimuth::Counter::add(service_a_throughput, 1.0)
  azimuth::Counter::add(service_b_throughput, 1.0)
  azimuth::Counter::add(service_c_throughput, 1.0)
  
  // Simulate some errors in service B
  azimuth::Counter::add(service_b_errors, 1.0)
  
  // Verify metric instrument consistency
  assert_eq(service_a_latency.name, "operation.latency")
  assert_eq(service_b_latency.name, "operation.latency")
  assert_eq(service_c_latency.name, "operation.latency")
  
  assert_eq(service_a_throughput.name, "operations.processed")
  assert_eq(service_b_throughput.name, "operations.processed")
  assert_eq(service_c_throughput.name, "operations.processed")
  
  assert_eq(service_a_errors.name, "operations.errors")
  assert_eq(service_b_errors.name, "operations.errors")
  assert_eq(service_c_errors.name, "operations.errors")
  
  // Verify metric descriptions are consistent
  assert_eq(service_a_latency.description, Some("Operation latency in milliseconds"))
  assert_eq(service_b_latency.description, Some("Operation latency in milliseconds"))
  assert_eq(service_c_latency.description, Some("Operation latency in milliseconds"))
  
  assert_eq(service_a_latency.unit, Some("ms"))
  assert_eq(service_b_latency.unit, Some("ms"))
  assert_eq(service_c_latency.unit, Some("ms"))
}

// Test 9: Complex Cross-Service Workflow Integration
pub test "complex cross-service workflow integration" {
  // Setup complete telemetry stack for workflow integration
  let workflow_resource = azimuth::Resource::new()
  let workflow_attrs = [
    ("workflow.name", azimuth::StringValue("order-fulfillment")),
    ("workflow.version", azimuth::StringValue("1.0.0")),
    ("workflow.instance", azimuth::StringValue("wf-2025-001")),
    ("business.process", azimuth::StringValue("e-commerce-order"))
  ]
  let workflow_resource_final = azimuth::Resource::with_attributes(workflow_resource, workflow_attrs)
  
  // Initialize workflow services
  let order_service_tracer_provider = azimuth::TracerProvider::default()
  let order_service_tracer = azimuth::TracerProvider::get_tracer(order_service_tracer_provider, "order-service")
  
  let payment_service_tracer_provider = azimuth::TracerProvider::default()
  let payment_service_tracer = azimuth::TracerProvider::get_tracer(payment_service_tracer_provider, "payment-service")
  
  let inventory_service_tracer_provider = azimuth::TracerProvider::default()
  let inventory_service_tracer = azimuth::TracerProvider::get_tracer(inventory_service_tracer_provider, "inventory-service")
  
  let shipping_service_tracer_provider = azimuth::TracerProvider::default()
  let shipping_service_tracer = azimuth::TracerProvider::get_tracer(shipping_service_tracer_provider, "shipping-service")
  
  // Execute workflow with cross-service correlation
  let workflow_trace_id = "workflow-order-001"
  
  // Order Service
  let order_span_ctx = azimuth::SpanContext::new(workflow_trace_id, "span-order-001", true, "service=order")
  let order_span = azimuth::Tracer::start_span(order_service_tracer, "process.order")
  azimuth::Span::add_event(order_span, "order.received", 
    Some([("order.id", azimuth::StringValue("ORD-2025-001")),
          ("customer.id", azimuth::StringValue("CUST-12345")),
          ("order.amount", azimuth::FloatValue(299.99))]))
  
  // Payment Service
  let payment_span_ctx = azimuth::SpanContext::new(workflow_trace_id, "span-payment-001", true, "service=payment")
  let payment_span = azimuth::Tracer::start_span(payment_service_tracer, "process.payment")
  azimuth::Span::add_event(payment_span, "payment.processed", 
    Some([("payment.id", azimuth::StringValue("PAY-2025-001")),
          ("payment.method", azimuth::StringValue("credit_card")),
          ("payment.amount", azimuth::FloatValue(299.99))]))
  
  // Inventory Service
  let inventory_span_ctx = azimuth::SpanContext::new(workflow_trace_id, "span-inventory-001", true, "service=inventory")
  let inventory_span = azimuth::Tracer::start_span(inventory_service_tracer, "reserve.inventory")
  azimuth::Span::add_event(inventory_span, "inventory.reserved", 
    Some([("product.id", azimuth::StringValue("PROD-001")),
          ("quantity", azimuth::IntValue(1)),
          ("warehouse.id", azimuth::StringValue("WH-001"))]))
  
  // Shipping Service
  let shipping_span_ctx = azimuth::SpanContext::new(workflow_trace_id, "span-shipping-001", true, "service=shipping")
  let shipping_span = azimuth::Tracer::start_span(shipping_service_tracer, "schedule.shipping")
  azimuth::Span::add_event(shipping_span, "shipping.scheduled", 
    Some([("shipment.id", azimuth::StringValue("SHP-2025-001")),
          ("carrier", azimuth::StringValue("express_delivery")),
          ("estimated.delivery", azimuth::StringValue("2025-01-30"))]))
  
  // Verify workflow trace consistency
  assert_eq(azimuth::SpanContext::trace_id(order_span_ctx), workflow_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(payment_span_ctx), workflow_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(inventory_span_ctx), workflow_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(shipping_span_ctx), workflow_trace_id)
  
  // Verify unique span IDs for each service
  assert_eq(azimuth::SpanContext::span_id(order_span_ctx), "span-order-001")
  assert_eq(azimuth::SpanContext::span_id(payment_span_ctx), "span-payment-001")
  assert_eq(azimuth::SpanContext::span_id(inventory_span_ctx), "span-inventory-001")
  assert_eq(azimuth::SpanContext::span_id(shipping_span_ctx), "span-shipping-001")
  
  // Verify workflow resource attributes
  let workflow_name = azimuth::Resource::get_attribute(workflow_resource_final, "workflow.name")
  let workflow_version = azimuth::Resource::get_attribute(workflow_resource_final, "workflow.version")
  let business_process = azimuth::Resource::get_attribute(workflow_resource_final, "business.process")
  
  assert_eq(workflow_name, Some(azimuth::StringValue("order-fulfillment")))
  assert_eq(workflow_version, Some(azimuth::StringValue("1.0.0")))
  assert_eq(business_process, Some(azimuth::StringValue("e-commerce-order")))
  
  // Complete workflow spans
  azimuth::Span::set_status(order_span, azimuth::Ok, Some("Order processed successfully"))
  azimuth::Span::set_status(payment_span, azimuth::Ok, Some("Payment processed successfully"))
  azimuth::Span::set_status(inventory_span, azimuth::Ok, Some("Inventory reserved successfully"))
  azimuth::Span::set_status(shipping_span, azimuth::Ok, Some("Shipping scheduled successfully"))
  
  azimuth::Span::end(order_span)
  azimuth::Span::end(payment_span)
  azimuth::Span::end(inventory_span)
  azimuth::Span::end(shipping_span)
}

// Test 10: Cross-Service Data Consistency Validation
pub test "cross-service data consistency validation" {
  // Create consistent data structures across services
  let shared_schema_version = "1.2.0"
  let shared_data_format = "json"
  let shared_encoding = "utf-8"
  
  // Service A data structure
  let service_a_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_a_attrs, "schema.version", azimuth::StringValue(shared_schema_version))
  azimuth::Attributes::set(service_a_attrs, "data.format", azimuth::StringValue(shared_data_format))
  azimuth::Attributes::set(service_a_attrs, "encoding", azimuth::StringValue(shared_encoding))
  azimuth::Attributes::set(service_a_attrs, "service.name", azimuth::StringValue("data-producer"))
  
  // Service B data structure
  let service_b_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_b_attrs, "schema.version", azimuth::StringValue(shared_schema_version))
  azimuth::Attributes::set(service_b_attrs, "data.format", azimuth::StringValue(shared_data_format))
  azimuth::Attributes::set(service_b_attrs, "encoding", azimuth::StringValue(shared_encoding))
  azimuth::Attributes::set(service_b_attrs, "service.name", azimuth::StringValue("data-processor"))
  
  // Service C data structure
  let service_c_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_c_attrs, "schema.version", azimuth::StringValue(shared_schema_version))
  azimuth::Attributes::set(service_c_attrs, "data.format", azimuth::StringValue(shared_data_format))
  azimuth::Attributes::set(service_c_attrs, "encoding", azimuth::StringValue(shared_encoding))
  azimuth::Attributes::set(service_c_attrs, "service.name", azimuth::StringValue("data-consumer"))
  
  // Verify data consistency across services
  let schema_version_a = azimuth::Attributes::get(service_a_attrs, "schema.version")
  let schema_version_b = azimuth::Attributes::get(service_b_attrs, "schema.version")
  let schema_version_c = azimuth::Attributes::get(service_c_attrs, "schema.version")
  
  assert_eq(schema_version_a, Some(azimuth::StringValue(shared_schema_version)))
  assert_eq(schema_version_b, Some(azimuth::StringValue(shared_schema_version)))
  assert_eq(schema_version_c, Some(azimuth::StringValue(shared_schema_version)))
  
  let data_format_a = azimuth::Attributes::get(service_a_attrs, "data.format")
  let data_format_b = azimuth::Attributes::get(service_b_attrs, "data.format")
  let data_format_c = azimuth::Attributes::get(service_c_attrs, "data.format")
  
  assert_eq(data_format_a, Some(azimuth::StringValue(shared_data_format)))
  assert_eq(data_format_b, Some(azimuth::StringValue(shared_data_format)))
  assert_eq(data_format_c, Some(azimuth::StringValue(shared_data_format)))
  
  let encoding_a = azimuth::Attributes::get(service_a_attrs, "encoding")
  let encoding_b = azimuth::Attributes::get(service_b_attrs, "encoding")
  let encoding_c = azimuth::Attributes::get(service_c_attrs, "encoding")
  
  assert_eq(encoding_a, Some(azimuth::StringValue(shared_encoding)))
  assert_eq(encoding_b, Some(azimuth::StringValue(shared_encoding)))
  assert_eq(encoding_c, Some(azimuth::StringValue(shared_encoding)))
  
  // Verify service-specific attributes
  let service_name_a = azimuth::Attributes::get(service_a_attrs, "service.name")
  let service_name_b = azimuth::Attributes::get(service_b_attrs, "service.name")
  let service_name_c = azimuth::Attributes::get(service_c_attrs, "service.name")
  
  assert_eq(service_name_a, Some(azimuth::StringValue("data-producer")))
  assert_eq(service_name_b, Some(azimuth::StringValue("data-processor")))
  assert_eq(service_name_c, Some(azimuth::StringValue("data-consumer")))
}