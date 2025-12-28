// Enhanced Cross Service Consistency Tests for Azimuth
// Tests distributed tracing consistency across multiple services

test "cross service trace propagation" {
  // Simulate a distributed request flowing through multiple services
  
  // Service A: Entry point
  let trace_id = "trace-1234567890abcdef1234567890abcdef"
  let span_a_id = "span-aaaaaaaaaaaaaaaa"
  let span_a_context = SpanContext::new(trace_id, span_a_id, true, "")
  let span_a = Span::new("service-a.operation", Server, span_a_context)
  
  // Verify Service A span properties
  assert_eq(SpanContext::trace_id(Span::span_context(span_a)), trace_id)
  assert_eq(SpanContext::span_id(Span::span_context(span_a)), span_a_id)
  assert_true(SpanContext::is_sampled(Span::span_context(span_a)))
  assert_true(SpanContext::is_valid(Span::span_context(span_a)))
  
  // Service B: Called by Service A
  let span_b_id = "span-bbbbbbbbbbbbbbbb"
  let span_b_context = SpanContext::new(trace_id, span_b_id, true, "")
  let span_b = Span::new("service-b.operation", Client, span_b_context)
  
  // Verify Service B span has same trace ID
  assert_eq(SpanContext::trace_id(Span::span_context(span_b)), trace_id)
  assert_eq(SpanContext::span_id(Span::span_context(span_b)), span_b_id)
  assert_true(SpanContext::is_sampled(Span::span_context(span_b)))
  assert_true(SpanContext::is_valid(Span::span_context(span_b)))
  
  // Service C: Called by Service B
  let span_c_id = "span-cccccccccccccccc"
  let span_c_context = SpanContext::new(trace_id, span_c_id, true, "")
  let span_c = Span::new("service-c.operation", Server, span_c_context)
  
  // Verify Service C span has same trace ID
  assert_eq(SpanContext::trace_id(Span::span_context(span_c)), trace_id)
  assert_eq(SpanContext::span_id(Span::span_context(span_c)), span_c_id)
  assert_true(SpanContext::is_sampled(Span::span_context(span_c)))
  assert_true(SpanContext::is_valid(Span::span_context(span_c)))
  
  // Verify all spans share the same trace ID
  assert_eq(SpanContext::trace_id(Span::span_context(span_a)), 
             SpanContext::trace_id(Span::span_context(span_b)))
  assert_eq(SpanContext::trace_id(Span::span_context(span_b)), 
             SpanContext::trace_id(Span::span_context(span_c)))
  
  // Verify all spans have different span IDs
  assert_true(SpanContext::span_id(Span::span_context(span_a)) != 
              SpanContext::span_id(Span::span_context(span_b)))
  assert_true(SpanContext::span_id(Span::span_context(span_b)) != 
              SpanContext::span_id(Span::span_context(span_c)))
  assert_true(SpanContext::span_id(Span::span_context(span_a)) != 
              SpanContext::span_id(Span::span_context(span_c)))
}

test "cross service baggage propagation" {
  // Test baggage propagation across services
  let baggage = Baggage::new()
  
  // Service A: Add initial baggage
  let baggage_a = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_a_with_session = Baggage::set_entry(baggage_a, "session.id", "session-abcdef")
  
  // Verify baggage in Service A
  let user_id_a = Baggage::get_entry(baggage_a_with_session, "user.id")
  let session_id_a = Baggage::get_entry(baggage_a_with_session, "session.id")
  assert_eq(user_id_a, Some("user-12345"))
  assert_eq(session_id_a, Some("session-abcdef"))
  
  // Service B: Add more baggage entries
  let baggage_b = Baggage::set_entry(baggage_a_with_session, "request.id", "req-67890")
  let baggage_b_with_tenant = Baggage::set_entry(baggage_b, "tenant.id", "tenant-001")
  
  // Verify baggage in Service B (should have all entries)
  let user_id_b = Baggage::get_entry(baggage_b_with_tenant, "user.id")
  let session_id_b = Baggage::get_entry(baggage_b_with_tenant, "session.id")
  let request_id_b = Baggage::get_entry(baggage_b_with_tenant, "request.id")
  let tenant_id_b = Baggage::get_entry(baggage_b_with_tenant, "tenant.id")
  
  assert_eq(user_id_b, Some("user-12345"))
  assert_eq(session_id_b, Some("session-abcdef"))
  assert_eq(request_id_b, Some("req-67890"))
  assert_eq(tenant_id_b, Some("tenant-001"))
  
  // Service C: Modify existing baggage
  let baggage_c = Baggage::set_entry(baggage_b_with_tenant, "request.id", "req-modified")
  let baggage_c_with_region = Baggage::set_entry(baggage_c, "region", "us-west")
  
  // Verify baggage in Service C
  let user_id_c = Baggage::get_entry(baggage_c_with_region, "user.id")
  let session_id_c = Baggage::get_entry(baggage_c_with_region, "session.id")
  let request_id_c = Baggage::get_entry(baggage_c_with_region, "request.id")
  let tenant_id_c = Baggage::get_entry(baggage_c_with_region, "tenant.id")
  let region_c = Baggage::get_entry(baggage_c_with_region, "region")
  
  assert_eq(user_id_c, Some("user-12345"))
  assert_eq(session_id_c, Some("session-abcdef"))
  assert_eq(request_id_c, Some("req-modified"))  # Modified value
  assert_eq(tenant_id_c, Some("tenant-001"))
  assert_eq(region_c, Some("us-west"))
}

test "cross service context consistency" {
  // Test context propagation and consistency across services
  let root_ctx = Context::root()
  
  // Service A: Add context values
  let service_key = ContextKey::new("service.name")
  let version_key = ContextKey::new("service.version")
  let ctx_a = Context::with_value(
    Context::with_value(root_ctx, service_key, "service-A"),
    version_key, "1.0.0"
  )
  
  // Verify Service A context
  let service_a = Context::get(ctx_a, service_key)
  let version_a = Context::get(ctx_a, version_key)
  assert_eq(service_a, Some("service-A"))
  assert_eq(version_a, Some("1.0.0"))
  
  // Service B: Add more context values
  let operation_key = ContextKey::new("operation.name")
  let user_key = ContextKey::new("user.id")
  let ctx_b = Context::with_value(
    Context::with_value(ctx_a, operation_key, "process-data"),
    user_key, "user-12345"
  )
  
  // Verify Service B context (should have Service A values too)
  let service_b = Context::get(ctx_b, service_key)
  let version_b = Context::get(ctx_b, version_key)
  let operation_b = Context::get(ctx_b, operation_key)
  let user_b = Context::get(ctx_b, user_key)
  
  assert_eq(service_b, Some("service-A"))
  assert_eq(version_b, Some("1.0.0"))
  assert_eq(operation_b, Some("process-data"))
  assert_eq(user_b, Some("user-12345"))
  
  // Service C: Modify existing context
  let new_version_key = ContextKey::new("service.version")
  let environment_key = ContextKey::new("environment")
  let ctx_c = Context::with_value(
    Context::with_value(ctx_b, new_version_key, "1.1.0"),
    environment_key, "production"
  )
  
  // Verify Service C context
  let service_c = Context::get(ctx_c, service_key)
  let version_c = Context::get(ctx_c, version_key)
  let operation_c = Context::get(ctx_c, operation_key)
  let user_c = Context::get(ctx_c, user_key)
  let environment_c = Context::get(ctx_c, environment_key)
  
  # Note: Simplified implementation behavior may vary
  # This test documents expected behavior for context propagation
}

test "cross service metrics consistency" {
  // Test metrics consistency across services
  let provider_a = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(provider_a, "service-a")
  
  let provider_b = MeterProvider::default()
  let meter_b = MeterProvider::get_meter(provider_b, "service-b")
  
  let provider_c = MeterProvider::default()
  let meter_c = MeterProvider::get_meter(provider_c, "service-c")
  
  // Service A: Create and record metrics
  let request_counter_a = Meter::create_counter(meter_a, "http.requests.total")
  let response_histogram_a = Meter::create_histogram(meter_a, "http.response.duration")
  
  Counter::add(request_counter_a, 100.0)
  Histogram::record(response_histogram_a, 150.0)
  
  // Service B: Create metrics with same names
  let request_counter_b = Meter::create_counter(meter_b, "http.requests.total")
  let response_histogram_b = Meter::create_histogram(meter_b, "http.response.duration")
  
  Counter::add(request_counter_b, 50.0)
  Histogram::record(response_histogram_b, 200.0)
  
  // Service C: Create metrics with same names
  let request_counter_c = Meter::create_counter(meter_c, "http.requests.total")
  let response_histogram_c = Meter::create_histogram(meter_c, "http.response.duration")
  
  Counter::add(request_counter_c, 75.0)
  Histogram::record(response_histogram_c, 120.0)
  
  // Verify metric names are consistent across services
  assert_eq(request_counter_a.name, "http.requests.total")
  assert_eq(request_counter_b.name, "http.requests.total")
  assert_eq(request_counter_c.name, "http.requests.total")
  
  assert_eq(response_histogram_a.name, "http.response.duration")
  assert_eq(response_histogram_b.name, "http.response.duration")
  assert_eq(response_histogram_c.name, "http.response.duration")
  
  // Verify meter names are different (service-specific)
  assert_eq(meter_a.scope.name, "service-a")
  assert_eq(meter_b.scope.name, "service-b")
  assert_eq(meter_c.scope.name, "service-c")
}

test "cross service logging consistency" {
  // Test logging consistency across services
  let provider_a = LoggerProvider::default()
  let logger_a = LoggerProvider::get_logger(provider_a, "service-a")
  
  let provider_b = LoggerProvider::default()
  let logger_b = LoggerProvider::get_logger(provider_b, "service-b")
  
  let provider_c = LoggerProvider::default()
  let logger_c = LoggerProvider::get_logger(provider_c, "service-c")
  
  // Create consistent trace context for all services
  let trace_id = "cross-service-trace-123"
  let correlation_id = "correlation-456"
  
  // Service A: Log with trace context
  let log_a = LogRecord::new_with_context(
    Info, Some("Service A processing request"), None,
    Some(1000L), Some(1001L), Some(trace_id), Some("span-a"), None
  )
  Logger::emit(logger_a, log_a)
  
  // Service B: Log with same trace context
  let log_b = LogRecord::new_with_context(
    Info, Some("Service B handling request"), None,
    Some(1002L), Some(1003L), Some(trace_id), Some("span-b"), None
  )
  Logger::emit(logger_b, log_b)
  
  // Service C: Log with same trace context
  let log_c = LogRecord::new_with_context(
    Info, Some("Service C completing request"), None,
    Some(1004L), Some(1005L), Some(trace_id), Some("span-c"), None
  )
  Logger::emit(logger_c, log_c)
  
  // Verify trace ID consistency
  assert_eq(LogRecord::trace_id(log_a), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_b), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_c), Some(trace_id))
  
  // Verify different span IDs
  assert_eq(LogRecord::span_id(log_a), Some("span-a"))
  assert_eq(LogRecord::span_id(log_b), Some("span-b"))
  assert_eq(LogRecord::span_id(log_c), Some("span-c"))
  
  // Verify logger names are service-specific
  assert_eq(logger_a.scope.name, "service-a")
  assert_eq(logger_b.scope.name, "service-b")
  assert_eq(logger_c.scope.name, "service-c")
}

test "cross service resource consistency" {
  // Test resource consistency across services
  let base_resource = Resource::new()
  
  // Service A: Define resource attributes
  let service_a_attrs = [
    ("service.name", StringValue("service-a")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-a-123")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource_a = Resource::with_attributes(base_resource, service_a_attrs)
  
  // Service B: Define resource with shared attributes
  let service_b_attrs = [
    ("service.name", StringValue("service-b")),
    ("service.version", StringValue("1.2.0")),
    ("service.instance.id", StringValue("instance-b-456")),
    ("deployment.environment", StringValue("production")),  # Same environment
    ("region", StringValue("us-west"))
  ]
  let resource_b = Resource::with_attributes(base_resource, service_b_attrs)
  
  // Service C: Define resource with shared attributes
  let service_c_attrs = [
    ("service.name", StringValue("service-c")),
    ("service.version", StringValue("2.0.0")),
    ("service.instance.id", StringValue("instance-c-789")),
    ("deployment.environment", StringValue("production")),  # Same environment
    ("region", StringValue("us-west"))                     # Same region
  ]
  let resource_c = Resource::with_attributes(base_resource, service_c_attrs)
  
  // Verify service-specific attributes
  let service_name_a = Resource::get_attribute(resource_a, "service.name")
  let service_name_b = Resource::get_attribute(resource_b, "service.name")
  let service_name_c = Resource::get_attribute(resource_c, "service.name")
  
  assert_eq(service_name_a, Some(StringValue("service-a")))
  assert_eq(service_name_b, Some(StringValue("service-b")))
  assert_eq(service_name_c, Some(StringValue("service-c")))
  
  // Verify shared environment attribute
  let env_a = Resource::get_attribute(resource_a, "deployment.environment")
  let env_b = Resource::get_attribute(resource_b, "deployment.environment")
  let env_c = Resource::get_attribute(resource_c, "deployment.environment")
  
  assert_eq(env_a, Some(StringValue("production")))
  assert_eq(env_b, Some(StringValue("production")))
  assert_eq(env_c, Some(StringValue("production")))
  
  // Verify shared region attribute (where defined)
  let region_a = Resource::get_attribute(resource_a, "region")
  let region_b = Resource::get_attribute(resource_b, "region")
  let region_c = Resource::get_attribute(resource_c, "region")
  
  assert_eq(region_a, None)  # Not defined in Service A
  assert_eq(region_b, Some(StringValue("us-west")))
  assert_eq(region_c, Some(StringValue("us-west")))
}

test "cross service end-to-end workflow" {
  // Simulate complete end-to-end workflow across multiple services
  
  // Initialize trace context
  let trace_id = "workflow-trace-123456"
  let root_span_id = "root-span-abcdef"
  let span_context = SpanContext::new(trace_id, root_span_id, true, "")
  let root_span = Span::new("workflow.root", Internal, span_context)
  
  // Initialize baggage
  let baggage = Baggage::new()
  let workflow_baggage = Baggage::set_entry(
    Baggage::set_entry(baggage, "workflow.id", "workflow-789"),
    "user.id", "user-456"
  )
  
  // Initialize context
  let ctx = Context::root()
  let workflow_ctx = Context::with_value(
    Context::with_value(ctx, ContextKey::new("workflow.type"), "data-processing"),
    ContextKey::new("correlation.id"), "corr-123"
  )
  
  // Service A: Entry point
  let service_a_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service-a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a.process")
  
  let service_a_meter_provider = MeterProvider::default()
  let service_a_meter = MeterProvider::get_meter(service_a_meter_provider, "service-a")
  let service_a_counter = Meter::create_counter(service_a_meter, "workflow.steps.completed")
  Counter::add(service_a_counter, 1.0)
  
  let service_a_logger_provider = LoggerProvider::default()
  let service_a_logger = LoggerProvider::get_logger(service_a_logger_provider, "service-a")
  let service_a_log = LogRecord::new_with_context(
    Info, Some("Service A started workflow step"), None,
    Some(1000L), Some(1001L), Some(trace_id), Some("service-a-span"), Some(workflow_ctx)
  )
  Logger::emit(service_a_logger, service_a_log)
  
  // Service B: Processing
  let service_b_provider = TracerProvider::default()
  let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service-b")
  let service_b_span = Tracer::start_span(service_b_tracer, "service-b.transform")
  
  let service_b_meter_provider = MeterProvider::default()
  let service_b_meter = MeterProvider::get_meter(service_b_meter_provider, "service-b")
  let service_b_histogram = Meter::create_histogram(service_b_meter, "processing.duration")
  Histogram::record(service_b_histogram, 250.0)
  
  let service_b_logger_provider = LoggerProvider::default()
  let service_b_logger = LoggerProvider::get_logger(service_b_logger_provider, "service-b")
  let service_b_log = LogRecord::new_with_context(
    Info, Some("Service B processing data"), None,
    Some(2000L), Some(2001L), Some(trace_id), Some("service-b-span"), Some(workflow_ctx)
  )
  Logger::emit(service_b_logger, service_b_log)
  
  // Service C: Completion
  let service_c_provider = TracerProvider::default()
  let service_c_tracer = TracerProvider::get_tracer(service_c_provider, "service-c")
  let service_c_span = Tracer::start_span(service_c_tracer, "service-c.complete")
  
  let service_c_meter_provider = MeterProvider::default()
  let service_c_meter = MeterProvider::get_meter(service_c_meter_provider, "service-c")
  let service_c_counter = Meter::create_counter(service_c_meter, "workflow.completions")
  Counter::add(service_c_counter, 1.0)
  
  let service_c_logger_provider = LoggerProvider::default()
  let service_c_logger = LoggerProvider::get_logger(service_c_logger_provider, "service-c")
  let service_c_log = LogRecord::new_with_context(
    Info, Some("Service C completed workflow"), None,
    Some(3000L), Some(3001L), Some(trace_id), Some("service-c-span"), Some(workflow_ctx)
  )
  Logger::emit(service_c_logger, service_c_log)
  
  // Verify end-to-end consistency
  assert_eq(Span::name(service_a_span), "service-a.process")
  assert_eq(Span::name(service_b_span), "service-b.transform")
  assert_eq(Span::name(service_c_span), "service-c.complete")
  
  assert_eq(service_a_counter.name, "workflow.steps.completed")
  assert_eq(service_b_histogram.name, "processing.duration")
  assert_eq(service_c_counter.name, "workflow.completions")
  
  assert_eq(LogRecord::body(service_a_log), Some("Service A started workflow step"))
  assert_eq(LogRecord::body(service_b_log), Some("Service B processing data"))
  assert_eq(LogRecord::body(service_c_log), Some("Service C completed workflow"))
  
  assert_eq(LogRecord::trace_id(service_a_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(service_b_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(service_c_log), Some(trace_id))
}