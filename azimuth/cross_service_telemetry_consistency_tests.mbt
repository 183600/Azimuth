// Cross-Service Telemetry Consistency Tests
// Testing telemetry data consistency across multiple services

test "trace context consistency across services" {
  // Test trace context propagation and consistency across services
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  // Service A: Create root span
  let service_a_span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  let service_a_span = Span::new("service-a-operation", Internal, service_a_span_ctx)
  
  // Verify Service A span context
  assert_eq(SpanContext::trace_id(service_a_span_ctx), trace_id)
  assert_eq(SpanContext::span_id(service_a_span_ctx), span_id)
  assert_true(SpanContext::is_sampled(service_a_span_ctx))
  
  // Service B: Extract and create child span
  let service_b_span_ctx = SpanContext::new(trace_id, "child-span-456", true, "key1=value1")
  let service_b_span = Span::new("service-b-operation", Server, service_b_span_ctx)
  
  // Verify Service B maintains same trace ID
  assert_eq(SpanContext::trace_id(service_b_span_ctx), trace_id)
  assert_eq(SpanContext::span_id(service_b_span_ctx), "child-span-456")
  assert_true(SpanContext::is_sampled(service_b_span_ctx))
  
  // Verify trace ID consistency across services
  assert_eq(SpanContext::trace_id(service_a_span_ctx), SpanContext::trace_id(service_b_span_ctx))
}

test "baggage consistency across service boundaries" {
  // Test baggage propagation consistency across services
  let baggage = Baggage::new()
  
  // Service A: Set baggage entries
  let service_a_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "request.id", "req-abc123")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "session.id", "sess-def456")
  
  // Service B: Extract baggage
  let service_b_user_id = Baggage::get_entry(service_a_baggage, "user.id")
  let service_b_request_id = Baggage::get_entry(service_a_baggage, "request.id")
  let service_b_session_id = Baggage::get_entry(service_a_baggage, "session.id")
  
  // Verify baggage consistency
  assert_eq(service_b_user_id, Some("12345"))
  assert_eq(service_b_request_id, Some("req-abc123"))
  assert_eq(service_b_session_id, Some("sess-def456"))
  
  // Service C: Add additional baggage
  let service_c_baggage = Baggage::set_entry(service_a_baggage, "service.c.timestamp", "2025-01-01T00:00:00Z")
  let service_c_user_id = Baggage::get_entry(service_c_baggage, "user.id")
  let service_c_timestamp = Baggage::get_entry(service_c_baggage, "service.c.timestamp")
  
  // Verify original baggage is preserved and new baggage is added
  assert_eq(service_c_user_id, Some("12345"))
  assert_eq(service_c_timestamp, Some("2025-01-01T00:00:00Z"))
}

test "metrics consistency across multiple services" {
  // Test metrics naming and consistency across services
  let service_a_provider = MeterProvider::default()
  let service_a_meter = MeterProvider::get_meter(service_a_provider, "service-a")
  
  let service_b_provider = MeterProvider::default()
  let service_b_meter = MeterProvider::get_meter(service_b_provider, "service-b")
  
  let service_c_provider = MeterProvider::default()
  let service_c_meter = MeterProvider::get_meter(service_c_provider, "service-c")
  
  // Create consistent metrics across services
  let service_a_counter = Meter::create_counter(service_a_meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let service_b_counter = Meter::create_counter(service_b_meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let service_c_counter = Meter::create_counter(service_c_meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  
  // Create consistent histograms across services
  let service_a_histogram = Meter::create_histogram(service_a_meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  let service_b_histogram = Meter::create_histogram(service_b_meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  let service_c_histogram = Meter::create_histogram(service_c_meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  
  // Verify metric name consistency
  assert_eq(service_a_counter.name, service_b_counter.name)
  assert_eq(service_b_counter.name, service_c_counter.name)
  assert_eq(service_a_histogram.name, service_b_histogram.name)
  assert_eq(service_b_histogram.name, service_c_histogram.name)
  
  // Verify metric description consistency
  assert_eq(service_a_counter.description, service_b_counter.description)
  assert_eq(service_b_counter.description, service_c_counter.description)
  assert_eq(service_a_histogram.description, service_b_histogram.description)
  assert_eq(service_b_histogram.description, service_c_histogram.description)
  
  // Verify metric unit consistency
  assert_eq(service_a_counter.unit, service_b_counter.unit)
  assert_eq(service_b_counter.unit, service_c_counter.unit)
  assert_eq(service_a_histogram.unit, service_b_histogram.unit)
  assert_eq(service_b_histogram.unit, service_c_histogram.unit)
}

test "log consistency across service chain" {
  // Test log consistency across service chain
  let service_a_provider = LoggerProvider::default()
  let service_a_logger = LoggerProvider::get_logger(service_a_provider, "service-a")
  
  let service_b_provider = LoggerProvider::default()
  let service_b_logger = LoggerProvider::get_logger(service_b_provider, "service-b")
  
  let trace_id = "trace-12345"
  let span_id_a = "span-a-67890"
  let span_id_b = "span-b-11111"
  
  // Service A: Create log record with trace context
  let service_a_record = LogRecord::new_with_context(
    Info,
    Some("Service A processing request"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(trace_id),
    Some(span_id_a),
    None
  )
  
  // Service B: Create log record with same trace ID
  let service_b_record = LogRecord::new_with_context(
    Info,
    Some("Service B processing request"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some(trace_id),
    Some(span_id_b),
    None
  )
  
  // Verify trace ID consistency across services
  assert_eq(LogRecord::trace_id(service_a_record), LogRecord::trace_id(service_b_record))
  assert_eq(LogRecord::trace_id(service_a_record), Some(trace_id))
  
  // Verify different span IDs for different services
  assert_eq(LogRecord::span_id(service_a_record), Some(span_id_a))
  assert_eq(LogRecord::span_id(service_b_record), Some(span_id_b))
  assert_true(LogRecord::span_id(service_a_record) != LogRecord::span_id(service_b_record))
}

test "resource consistency across services" {
  // Test resource attribute consistency across services
  let service_a_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-a")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("service.namespace", StringValue("backend"))
  ])
  
  let service_b_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-b")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("service.namespace", StringValue("backend"))
  ])
  
  let service_c_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-c")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("service.namespace", StringValue("backend"))
  ])
  
  // Verify consistent attributes across services
  let service_a_version = Resource::get_attribute(service_a_resource, "service.version")
  let service_b_version = Resource::get_attribute(service_b_resource, "service.version")
  let service_c_version = Resource::get_attribute(service_c_resource, "service.version")
  
  let service_a_env = Resource::get_attribute(service_a_resource, "deployment.environment")
  let service_b_env = Resource::get_attribute(service_b_resource, "deployment.environment")
  let service_c_env = Resource::get_attribute(service_c_resource, "deployment.environment")
  
  let service_a_namespace = Resource::get_attribute(service_a_resource, "service.namespace")
  let service_b_namespace = Resource::get_attribute(service_b_resource, "service.namespace")
  let service_c_namespace = Resource::get_attribute(service_c_resource, "service.namespace")
  
  // Verify consistency
  assert_eq(service_a_version, service_b_version)
  assert_eq(service_b_version, service_c_version)
  assert_eq(service_a_env, service_b_env)
  assert_eq(service_b_env, service_c_env)
  assert_eq(service_a_namespace, service_b_namespace)
  assert_eq(service_b_namespace, service_c_namespace)
  
  // Verify service-specific differences
  let service_a_name = Resource::get_attribute(service_a_resource, "service.name")
  let service_b_name = Resource::get_attribute(service_b_resource, "service.name")
  let service_c_name = Resource::get_attribute(service_c_resource, "service.name")
  
  assert_eq(service_a_name, Some(StringValue("service-a")))
  assert_eq(service_b_name, Some(StringValue("service-b")))
  assert_eq(service_c_name, Some(StringValue("service-c")))
}

test "cross-service error propagation consistency" {
  // Test error status propagation across services
  let trace_id = "error-trace-123"
  
  // Service A: Create span with error status
  let service_a_span_ctx = SpanContext::new(trace_id, "span-a-error", true, "")
  let service_a_span = Span::new("service-a-operation", Internal, service_a_span_ctx)
  Span::set_status(service_a_span, Error, Some("Database connection failed"))
  
  // Service B: Propagate error status
  let service_b_span_ctx = SpanContext::new(trace_id, "span-b-error", true, "")
  let service_b_span = Span::new("service-b-operation", Server, service_b_span_ctx)
  Span::set_status(service_b_span, Error, Some("Upstream service error"))
  
  // Service C: Handle and propagate error
  let service_c_span_ctx = SpanContext::new(trace_id, "span-c-error", true, "")
  let service_c_span = Span::new("service-c-operation", Client, service_c_span_ctx)
  Span::set_status(service_c_span, Error, Some("Service unavailable"))
  
  // Verify error status consistency
  let service_a_status = Span::status(service_a_span)
  let service_b_status = Span::status(service_b_span)
  let service_c_status = Span::status(service_c_span)
  
  match service_a_status {
    Error => assert_true(true)
    _ => assert_false(true)
  }
  
  match service_b_status {
    Error => assert_true(true)
    _ => assert_false(true)
  }
  
  match service_c_status {
    Error => assert_true(true)
    _ => assert_false(true)
  }
  
  // Verify trace ID consistency in error scenario
  assert_eq(SpanContext::trace_id(service_a_span_ctx), SpanContext::trace_id(service_b_span_ctx))
  assert_eq(SpanContext::trace_id(service_b_span_ctx), SpanContext::trace_id(service_c_span_ctx))
}

test "cross-service sampling consistency" {
  // Test sampling decision consistency across services
  let trace_id_sampled = "sampled-trace-123"
  let trace_id_not_sampled = "not-sampled-trace-456"
  
  // Sampled trace across services
  let service_a_sampled_ctx = SpanContext::new(trace_id_sampled, "span-a-sampled", true, "")
  let service_b_sampled_ctx = SpanContext::new(trace_id_sampled, "span-b-sampled", true, "")
  let service_c_sampled_ctx = SpanContext::new(trace_id_sampled, "span-c-sampled", true, "")
  
  // Not sampled trace across services
  let service_a_not_sampled_ctx = SpanContext::new(trace_id_not_sampled, "span-a-not-sampled", false, "")
  let service_b_not_sampled_ctx = SpanContext::new(trace_id_not_sampled, "span-b-not-sampled", false, "")
  let service_c_not_sampled_ctx = SpanContext::new(trace_id_not_sampled, "span-c-not-sampled", false, "")
  
  // Verify sampling consistency for sampled trace
  assert_true(SpanContext::is_sampled(service_a_sampled_ctx))
  assert_true(SpanContext::is_sampled(service_b_sampled_ctx))
  assert_true(SpanContext::is_sampled(service_c_sampled_ctx))
  
  // Verify sampling consistency for not sampled trace
  assert_false(SpanContext::is_sampled(service_a_not_sampled_ctx))
  assert_false(SpanContext::is_sampled(service_b_not_sampled_ctx))
  assert_false(SpanContext::is_sampled(service_c_not_sampled_ctx))
  
  // Verify trace ID consistency
  assert_eq(SpanContext::trace_id(service_a_sampled_ctx), SpanContext::trace_id(service_b_sampled_ctx))
  assert_eq(SpanContext::trace_id(service_b_sampled_ctx), SpanContext::trace_id(service_c_sampled_ctx))
  
  assert_eq(SpanContext::trace_id(service_a_not_sampled_ctx), SpanContext::trace_id(service_b_not_sampled_ctx))
  assert_eq(SpanContext::trace_id(service_b_not_sampled_ctx), SpanContext::trace_id(service_c_not_sampled_ctx))
}

test "cross-service instrumentation scope consistency" {
  // Test instrumentation scope consistency across services
  let service_a_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service-a-tracer", Some("1.0.0"))
  let service_a_scope = Tracer::instrumentation_scope(service_a_tracer)
  
  let service_b_provider = TracerProvider::default()
  let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service-b-tracer", Some("1.0.0"))
  let service_b_scope = Tracer::instrumentation_scope(service_b_tracer)
  
  let service_c_provider = TracerProvider::default()
  let service_c_tracer = TracerProvider::get_tracer(service_c_provider, "service-c-tracer", Some("1.0.0"))
  let service_c_scope = Tracer::instrumentation_scope(service_c_tracer)
  
  // Verify version consistency across services
  assert_eq(service_a_scope.version, service_b_scope.version)
  assert_eq(service_b_scope.version, service_c_scope.version)
  assert_eq(service_a_scope.version, Some("1.0.0"))
  
  // Verify service-specific differences
  assert_eq(service_a_scope.name, "service-a-tracer")
  assert_eq(service_b_scope.name, "service-b-tracer")
  assert_eq(service_c_scope.name, "service-c-tracer")
  
  // Verify schema URL consistency (all None in this case)
  assert_eq(service_a_scope.schema_url, service_b_scope.schema_url)
  assert_eq(service_b_scope.schema_url, service_c_scope.schema_url)
  assert_eq(service_a_scope.schema_url, None)
}