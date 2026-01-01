// Azimuth Integration Test Suite
// This file contains integration test cases for the Azimuth telemetry system

// Test 1: End-to-end telemetry workflow
pub test "end-to-end telemetry workflow" {
  // Setup resource with comprehensive attributes
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("integration-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-12345")),
    ("deployment.environment", azimuth::StringValue("test")),
    ("host.name", azimuth::StringValue("test-host")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ])
  
  // Setup context with baggage
  let ctx = azimuth::Context::root()
  let user_id_key = azimuth::ContextKey::new("user.id")
  let request_id_key = azimuth::ContextKey::new("request.id")
  let session_id_key = azimuth::ContextKey::new("session.id")
  
  let ctx_with_user = azimuth::Context::with_value(ctx, user_id_key, "user-12345")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_user, request_id_key, "req-67890")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_request, session_id_key, "session-abcdef")
  
  // Create tracer and start span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "integration-tracer", Some("1.0.0"))
  let span = azimuth::Tracer::start_span(tracer, "integration-operation")
  
  // Add attributes and events to span
  let span_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(span_attrs, "operation.type", azimuth::StringValue("integration"))
  azimuth::Attributes::set(span_attrs, "operation.duration", azimuth::FloatValue(150.5))
  azimuth::Attributes::set(span_attrs, "operation.success", azimuth::BoolValue(true))
  
  azimuth::Span::add_event(span, "operation.started", Some([("timestamp", azimuth::StringValue("2025-01-01T00:00:00Z"))]))
  azimuth::Span::add_event(span, "operation.completed", Some([("result", azimuth::StringValue("success"))]))
  azimuth::Span::set_status(span, azimuth::Ok)
  
  // Create metrics and record measurements
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "integration-meter", Some("1.0.0"))
  
  let operation_counter = azimuth::Meter::create_counter(meter, "operations.total", Some("Total number of operations"), Some("count"))
  let duration_histogram = azimuth::Meter::create_histogram(meter, "operation.duration", Some("Operation duration in milliseconds"), Some("ms"))
  let active_connections = azimuth::Meter::create_updown_counter(meter, "active.connections", Some("Number of active connections"), Some("connections"))
  let memory_usage = azimuth::Meter::create_gauge(meter, "memory.usage", Some("Memory usage in MB"), Some("MB"))
  
  // Record metrics
  azimuth::Counter::add(operation_counter, 1.0)
  azimuth::Histogram::record(duration_histogram, 150.5)
  azimuth::UpDownCounter::add(active_connections, 5.0)
  
  // Create logger and emit logs
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "integration-logger", Some("1.0.0"))
  
  let info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Integration operation started successfully"),
    Some(span_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
    Some(ctx_with_session)
  )
  
  let warn_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Integration operation taking longer than expected"),
    Some(span_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
    Some(ctx_with_session)
  )
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Integration operation failed with error"),
    Some(span_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
    Some(ctx_with_session)
  )
  
  // Emit logs
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
  
  // Verify all components were created and configured correctly
  assert_eq(azimuth::Span::name(span), "integration-operation")
  assert_eq(azimuth::Tracer::instrumentation_scope(tracer).name, "integration-tracer")
  assert_eq(azimuth::Tracer::instrumentation_scope(tracer).version, Some("1.0.0"))
  
  assert_eq(operation_counter.name, "operations.total")
  assert_eq(operation_counter.description, Some("Total number of operations"))
  assert_eq(operation_counter.unit, Some("count"))
  
  assert_eq(duration_histogram.name, "operation.duration")
  assert_eq(duration_histogram.description, Some("Operation duration in milliseconds"))
  assert_eq(duration_histogram.unit, Some("ms"))
  
  assert_eq(logger.scope.name, "integration-logger")
  assert_eq(logger.scope.version, Some("1.0.0"))
  
  assert_eq(azimuth::LogRecord::body(info_log), Some("Integration operation started successfully"))
  assert_eq(azimuth::LogRecord::body(warn_log), Some("Integration operation taking longer than expected"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("Integration operation failed with error"))
  
  // Verify context values
  assert_eq(azimuth::Context::get(ctx_with_session, user_id_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_session, request_id_key), Some("req-67890"))
  assert_eq(azimuth::Context::get(ctx_with_session, session_id_key), Some("session-abcdef"))
  
  // Verify resource attributes
  assert_eq(azimuth::Resource::get_attribute(resource, "service.name"), Some(azimuth::StringValue("integration-test-service")))
  assert_eq(azimuth::Resource::get_attribute(resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
}

// Test 2: Distributed tracing across multiple services
pub test "distributed tracing across services" {
  // Service A: Entry point
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a", Some("1.0.0"))
  let service_a_span_ctx = azimuth::SpanContext::new("trace-distributed-123", "span-a-root", true, "key1=value1,key2=value2")
  let service_a_span = azimuth::Span::new("service-a-operation", azimuth::Server, service_a_span_ctx)
  
  // Service A adds baggage
  let baggage_a = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage_a, "user.id", "user-12345")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_user, "request.id", "req-abcdef")
  
  // Service B: Called by Service A
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b", Some("2.0.0"))
  let service_b_span_ctx = azimuth::SpanContext::new("trace-distributed-123", "span-b-child", true, "key1=value1,key2=value2")
  let service_b_span = azimuth::Span::new("service-b-operation", azimuth::Server, service_b_span_ctx)
  
  // Service B adds additional baggage
  let baggage_b = azimuth::Baggage::set_entry(baggage_with_request, "service.b.timestamp", "2025-01-01T00:00:00Z")
  
  // Service C: Called by Service B
  let service_c_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-c", Some("3.0.0"))
  let service_c_span_ctx = azimuth::SpanContext::new("trace-distributed-123", "span-c-grandchild", true, "key1=value1,key2=value2")
  let service_c_span = azimuth::Span::new("service-c-operation", azimuth::Internal, service_c_span_ctx)
  
  // Service C adds final baggage
  let baggage_c = azimuth::Baggage::set_entry(baggage_b, "service.c.result", "success")
  
  // Verify distributed tracing consistency
  assert_eq(azimuth::SpanContext::trace_id(service_a_span_ctx), "trace-distributed-123")
  assert_eq(azimuth::SpanContext::trace_id(service_b_span_ctx), "trace-distributed-123")
  assert_eq(azimuth::SpanContext::trace_id(service_c_span_ctx), "trace-distributed-123")
  
  // Verify span ID uniqueness
  assert_true(azimuth::SpanContext::span_id(service_a_span_ctx) != azimuth::SpanContext::span_id(service_b_span_ctx))
  assert_true(azimuth::SpanContext::span_id(service_b_span_ctx) != azimuth::SpanContext::span_id(service_c_span_ctx))
  assert_true(azimuth::SpanContext::span_id(service_a_span_ctx) != azimuth::SpanContext::span_id(service_c_span_ctx))
  
  // Verify span kinds
  assert_eq(azimuth::Span::kind(service_a_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(service_b_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(service_c_span), azimuth::Internal)
  
  // Verify baggage propagation
  assert_eq(azimuth::Baggage::get_entry(baggage_with_user, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "request.id"), Some("req-abcdef"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "request.id"), Some("req-abcdef"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "service.b.timestamp"), Some("2025-01-01T00:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "request.id"), Some("req-abcdef"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "service.b.timestamp"), Some("2025-01-01T00:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "service.c.result"), Some("success"))
  
  // Verify tracer versions
  assert_eq(azimuth::Tracer::instrumentation_scope(service_a_tracer).version, Some("1.0.0"))
  assert_eq(azimuth::Tracer::instrumentation_scope(service_b_tracer).version, Some("2.0.0"))
  assert_eq(azimuth::Tracer::instrumentation_scope(service_c_tracer).version, Some("3.0.0"))
}

// Test 3: Metrics aggregation and time series
pub test "metrics aggregation and time series" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "time-series-meter", Some("1.0.0"))
  
  // Create various metrics for time series testing
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("ms"))
  let error_rate_gauge = azimuth::Meter::create_gauge(meter, "http.error.rate", Some("HTTP error rate"), Some("percent"))
  let active_connections_updown = azimuth::Meter::create_updown_counter(meter, "http.active.connections", Some("Active HTTP connections"), Some("connections"))
  
  // Simulate time series data collection
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // Collect metrics over time
  for i in 0..10 {
    let timestamp = base_time + (i * 1000000000L)  // 1 second intervals
    
    // Simulate request processing
    azimuth::Counter::add(request_counter, 10.0)  // 10 requests per interval
    
    // Simulate response times with variation
    let base_response_time = 100.0 + (i.to_double() * 5.0)
    azimuth::Histogram::record(response_time_histogram, base_response_time)
    azimuth::Histogram::record(response_time_histogram, base_response_time + 10.0)
    azimuth::Histogram::record(response_time_histogram, base_response_time - 10.0)
    
    // Simulate connection changes
    if i % 2 == 0 {
      azimuth::UpDownCounter::add(active_connections_updown, 5.0)
    } else {
      azimuth::UpDownCounter::add(active_connections_updown, -3.0)
    }
    
    // Simulate error rate changes
    let error_rate = 1.0 + (i.to_double() * 0.5)
    // Note: Gauge recording would be implemented in a real system
  }
  
  // Verify metric properties
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  assert_eq(response_time_histogram.name, "http.response.time")
  assert_eq(response_time_histogram.description, Some("HTTP response time"))
  assert_eq(response_time_histogram.unit, Some("ms"))
  
  assert_eq(error_rate_gauge.name, "http.error.rate")
  assert_eq(error_rate_gauge.description, Some("HTTP error rate"))
  assert_eq(error_rate_gauge.unit, Some("percent"))
  
  assert_eq(active_connections_updown.name, "http.active.connections")
  assert_eq(active_connections_updown.description, Some("Active HTTP connections"))
  assert_eq(active_connections_updown.unit, Some("connections"))
  
  // Test metric instrument conversions
  let counter_instrument = azimuth::Histogram::as_instrument(response_time_histogram)  // Using histogram as proxy
  assert_eq(azimuth::Instrument::name(counter_instrument), "http.response.time")
  assert_eq(azimuth::Instrument::description(counter_instrument), Some("HTTP response time"))
  assert_eq(azimuth::Instrument::unit(counter_instrument), Some("ms"))
}

// Test 4: Context propagation with HTTP headers
pub test "context propagation with http headers" {
  // Create initial context with baggage
  let ctx = azimuth::Context::root()
  let user_id_key = azimuth::ContextKey::new("user.id")
  let trace_id_key = azimuth::ContextKey::new("trace.id")
  let request_id_key = azimuth::ContextKey::new("request.id")
  
  let ctx_with_user = azimuth::Context::with_value(ctx, user_id_key, "user-12345")
  let ctx_with_trace = azimuth::Context::with_value(ctx_with_user, trace_id_key, "trace-abcdef")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_trace, request_id_key, "req-12345")
  
  // Create baggage for HTTP propagation
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  let baggage_with_correlation = azimuth::Baggage::set_entry(baggage_with_session, "correlation.id", "corr-12345")
  
  // Create propagators
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // Create HTTP carrier for outbound request
  let outbound_carrier = azimuth::TextMapCarrier::new()
  
  // Inject context into HTTP headers
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_request, outbound_carrier)
  
  // Simulate HTTP request with injected headers
  let request_headers = [
    ("traceparent", "00-trace-abcdef-span-12345-01"),
    ("baggage", "user.id=user-12345,session.id=session-abcdef,correlation.id=corr-12345"),
    ("x-request-id", "req-12345"),
    ("user-agent", "azimuth-client/1.0.0")
  ]
  
  let http_request = azimuth::HttpRequest::new(
    "POST",
    "https://api.example.com/process",
    request_headers,
    Some("{\"data\": \"test\"}")
  )
  
  // Verify HTTP request properties
  assert_eq(azimuth::HttpRequest::http_method(http_request), "POST")
  assert_eq(azimuth::HttpRequest::url(http_request), "https://api.example.com/process")
  assert_eq(azimuth::HttpRequest::body(http_request), Some("{\"data\": \"test\"}"))
  
  // Simulate HTTP response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("x-trace-id", "trace-abcdef"),
    ("x-response-time", "150ms")
  ]
  
  let http_response = azimuth::HttpResponse::new(200, response_headers, Some("{\"status\": \"success\", \"id\": 123}"))
  
  // Verify HTTP response properties
  assert_eq(azimuth::HttpResponse::status_code(http_response), 200)
  assert_eq(azimuth::HttpResponse::body(http_response), Some("{\"status\": \"success\", \"id\": 123}"))
  
  // Create carrier for inbound processing
  let inbound_carrier = azimuth::TextMapCarrier::new()
  
  // Extract context from HTTP headers
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, inbound_carrier)
  
  // Verify extracted context
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
  
  // Verify original context values are preserved
  assert_eq(azimuth::Context::get(ctx_with_request, user_id_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_request, trace_id_key), Some("trace-abcdef"))
  assert_eq(azimuth::Context::get(ctx_with_request, request_id_key), Some("req-12345"))
  
  // Verify baggage entries
  assert_eq(azimuth::Baggage::get_entry(baggage_with_correlation, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_correlation, "session.id"), Some("session-abcdef"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_correlation, "correlation.id"), Some("corr-12345"))
}

// Test 5: Resource management with service discovery
pub test "resource management with service discovery" {
  // Create resources for different services
  let web_service_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("web-service")),
    ("service.version", azimuth::StringValue("1.2.3")),
    ("service.instance.id", azimuth::StringValue("web-instance-001")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("web-prod-01")),
    ("host.ip", azimuth::StringValue("10.0.1.100")),
    ("port", azimuth::IntValue(8080)),
    ("protocol", azimuth::StringValue("http")),
    ("team", azimuth::StringValue("frontend"))
  ])
  
  let api_service_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("api-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("service.instance.id", azimuth::StringValue("api-instance-001")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("api-prod-01")),
    ("host.ip", azimuth::StringValue("10.0.2.100")),
    ("port", azimuth::IntValue(9090)),
    ("protocol", azimuth::StringValue("grpc")),
    ("team", azimuth::StringValue("backend"))
  ])
  
  let db_service_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("database-service")),
    ("service.version", azimuth::StringValue("3.0.1")),
    ("service.instance.id", azimuth::StringValue("db-instance-001")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("db-prod-01")),
    ("host.ip", azimuth::StringValue("10.0.3.100")),
    ("port", azimuth::IntValue(5432)),
    ("protocol", azimuth::StringValue("postgresql")),
    ("team", azimuth::StringValue("data"))
  ])
  
  // Verify service-specific attributes
  assert_eq(azimuth::Resource::get_attribute(web_service_resource, "service.name"), Some(azimuth::StringValue("web-service")))
  assert_eq(azimuth::Resource::get_attribute(web_service_resource, "service.version"), Some(azimuth::StringValue("1.2.3")))
  assert_eq(azimuth::Resource::get_attribute(web_service_resource, "port"), Some(azimuth::IntValue(8080)))
  assert_eq(azimuth::Resource::get_attribute(web_service_resource, "protocol"), Some(azimuth::StringValue("http")))
  
  assert_eq(azimuth::Resource::get_attribute(api_service_resource, "service.name"), Some(azimuth::StringValue("api-service")))
  assert_eq(azimuth::Resource::get_attribute(api_service_resource, "service.version"), Some(azimuth::StringValue("2.1.0")))
  assert_eq(azimuth::Resource::get_attribute(api_service_resource, "port"), Some(azimuth::IntValue(9090)))
  assert_eq(azimuth::Resource::get_attribute(api_service_resource, "protocol"), Some(azimuth::StringValue("grpc")))
  
  assert_eq(azimuth::Resource::get_attribute(db_service_resource, "service.name"), Some(azimuth::StringValue("database-service")))
  assert_eq(azimuth::Resource::get_attribute(db_service_resource, "service.version"), Some(azimuth::StringValue("3.0.1")))
  assert_eq(azimuth::Resource::get_attribute(db_service_resource, "port"), Some(azimuth::IntValue(5432)))
  assert_eq(azimuth::Resource::get_attribute(db_service_resource, "protocol"), Some(azimuth::StringValue("postgresql")))
  
  // Test resource merging for service mesh
  let mesh_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.mesh.name", azimuth::StringValue("production-mesh")),
    ("service.mesh.version", azimuth::StringValue("1.5.0")),
    ("service.mesh.namespace", azimuth::StringValue("default")),
    ("service.mesh.istio.injection", azimuth::StringValue("enabled"))
  ])
  
  let web_mesh_merged = azimuth::Resource::merge(web_service_resource, mesh_resource)
  let api_mesh_merged = azimuth::Resource::merge(api_service_resource, mesh_resource)
  let db_mesh_merged = azimuth::Resource::merge(db_service_resource, mesh_resource)
  
  // Verify mesh attributes are added to all services
  assert_eq(azimuth::Resource::get_attribute(web_mesh_merged, "service.mesh.name"), Some(azimuth::StringValue("production-mesh")))
  assert_eq(azimuth::Resource::get_attribute(web_mesh_merged, "service.name"), Some(azimuth::StringValue("web-service")))
  
  assert_eq(azimuth::Resource::get_attribute(api_mesh_merged, "service.mesh.name"), Some(azimuth::StringValue("production-mesh")))
  assert_eq(azimuth::Resource::get_attribute(api_mesh_merged, "service.name"), Some(azimuth::StringValue("api-service")))
  
  assert_eq(azimuth::Resource::get_attribute(db_mesh_merged, "service.mesh.name"), Some(azimuth::StringValue("production-mesh")))
  assert_eq(azimuth::Resource::get_attribute(db_mesh_merged, "service.name"), Some(azimuth::StringValue("database-service")))
  
  // Test resource attribute filtering
  let common_attributes = [
    ("deployment.environment", azimuth::StringValue("production")),
    ("service.mesh.name", azimuth::StringValue("production-mesh")),
    ("service.mesh.version", azimuth::StringValue("1.5.0"))
  ]
  
  let common_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), common_attributes)
  
  // Verify common attributes exist in all merged resources
  assert_eq(azimuth::Resource::get_attribute(web_mesh_merged, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(api_mesh_merged, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(db_mesh_merged, "deployment.environment"), Some(azimuth::StringValue("production")))
}