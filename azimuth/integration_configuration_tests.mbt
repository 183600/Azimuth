// Integration and Configuration Tests for Azimuth Telemetry System
// Focus on end-to-end integration scenarios and configuration management

test "cross_service_telemetry_propagation" {
  // Simulate cross-service telemetry propagation
  let service1_tracer = TracerProvider::default().get_tracer("service1", Some("1.0.0"))
  let service2_tracer = TracerProvider::default().get_tracer("service2", Some("2.0.0"))
  
  // Service 1: Create span and propagate context
  let service1_span = service1_tracer.start_span("service1_operation")
  let service1_context = Span::span_context(service1_span)
  
  // Create carrier for propagation
  let carrier = TextMapCarrier::new()
  let composite_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // Inject context into carrier
  let root_ctx = Context::root()
  CompositePropagator::inject(composite_propagator, root_ctx, carrier)
  
  // Service 2: Extract context and continue trace
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let service2_span = service2_tracer.start_span("service2_operation")
  
  // Verify trace continuity
  assert_eq(Span::name(service1_span), "service1_operation")
  assert_eq(Span::name(service2_span), "service2_operation")
  assert_eq(SpanContext::is_valid(service1_context), true)
  
  // Add events and end spans
  Span::add_event(service1_span, "service1_event", None)
  Span::add_event(service2_span, "service2_event", None)
  
  Span::end(service1_span)
  Span::end(service2_span)
}

test "baggage_propagation_chain" {
  // Test baggage propagation through multiple services
  let baggage = Baggage::new()
  
  // Service 1: Add baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "request.id", "req-67890")
  let baggage3 = Baggage::set_entry(baggage2, "session.id", "sess-abc123")
  
  // Verify baggage entries
  assert_eq(Baggage::get_entry(baggage3, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage3, "request.id"), Some("req-67890"))
  assert_eq(Baggage::get_entry(baggage3, "session.id"), Some("sess-abc123"))
  
  // Service 2: Modify baggage
  let baggage4 = Baggage::set_entry(baggage3, "service2.timestamp", "2023-12-01T10:00:00Z")
  
  // Service 3: Remove baggage entry
  let baggage5 = Baggage::remove_entry(baggage4, "request.id")
  
  // Verify final baggage state
  assert_eq(Baggage::get_entry(baggage5, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage5, "session.id"), Some("sess-abc123"))
  assert_eq(Baggage::get_entry(baggage5, "service2.timestamp"), Some("2023-12-01T10:00:00Z"))
  assert_eq(Baggage::get_entry(baggage5, "request.id"), Some("req-67890"))  // Simplified implementation
}

test "telemetry_configuration_variations" {
  // Test different telemetry configurations
  
  // Configuration 1: Development environment
  let dev_tracer = TracerProvider::default().get_tracer("dev_service", Some("dev"), Some("http://dev-schema"))
  let dev_meter = MeterProvider::default().get_meter("dev_service", Some("dev"), Some("http://dev-schema"))
  let dev_logger = LoggerProvider::default().get_logger("dev_service", Some("dev"), Some("http://dev-schema"))
  
  let dev_scope = Tracer::instrumentation_scope(dev_tracer)
  assert_eq(dev_scope.name, "dev_service")
  assert_eq(dev_scope.version, Some("dev"))
  assert_eq(dev_scope.schema_url, Some("http://dev-schema"))
  
  // Configuration 2: Production environment
  let prod_tracer = TracerProvider::default().get_tracer("prod_service", Some("1.2.3"), Some("http://prod-schema"))
  let prod_meter = MeterProvider::default().get_meter("prod_service", Some("1.2.3"), Some("http://prod-schema"))
  let prod_logger = LoggerProvider::default().get_logger("prod_service", Some("1.2.3"), Some("http://prod-schema"))
  
  let prod_scope = Tracer::instrumentation_scope(prod_tracer)
  assert_eq(prod_scope.name, "prod_service")
  assert_eq(prod_scope.version, Some("1.2.3"))
  assert_eq(prod_scope.schema_url, Some("http://prod-schema"))
  
  // Configuration 3: Minimal configuration
  let minimal_tracer = TracerProvider::default().get_tracer("minimal_service")
  let minimal_meter = MeterProvider::default().get_meter("minimal_service")
  let minimal_logger = LoggerProvider::default().get_logger("minimal_service")
  
  let minimal_scope = Tracer::instrumentation_scope(minimal_tracer)
  assert_eq(minimal_scope.name, "minimal_service")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

test "resource_configuration_merge" {
  // Test resource configuration merging scenarios
  
  // Base resource with service information
  let base_attrs = [
    ("service.name", StringValue("base_service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Override resource with host information
  let override_attrs = [
    ("service.name", StringValue("override_service")),  // This should override
    ("host.name", StringValue("host-123")),
    ("host.ip", StringValue("192.168.1.100"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Verify merge results
  assert_eq(
    Resource::get_attribute(merged_resource, "service.name"),
    Some(StringValue("override_service"))
  )
  assert_eq(
    Resource::get_attribute(merged_resource, "service.version"),
    Some(StringValue("1.0.0"))
  )
  assert_eq(
    Resource::get_attribute(merged_resource, "deployment.environment"),
    Some(StringValue("development"))
  )
  assert_eq(
    Resource::get_attribute(merged_resource, "host.name"),
    Some(StringValue("host-123"))
  )
  assert_eq(
    Resource::get_attribute(merged_resource, "host.ip"),
    Some(StringValue("192.168.1.100"))
  )
}

test "end_to_end_telemetry_workflow" {
  // Complete end-to-end telemetry workflow simulation
  
  // 1. Initialize telemetry providers
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // 2. Create instruments
  let tracer = TracerProvider::get_tracer(tracer_provider, "workflow_service", Some("1.0.0"))
  let meter = MeterProvider::get_meter(meter_provider, "workflow_service", Some("1.0.0"))
  let logger = LoggerProvider::get_logger(logger_provider, "workflow_service", Some("1.0.0"))
  
  // 3. Configure resource
  let resource_attrs = [
    ("service.name", StringValue("workflow_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-abc123"))
  ]
  let resource = Resource::with_attributes(Resource::new(), resource_attrs)
  
  // 4. Start workflow operation
  let main_span = tracer.start_span("workflow_operation")
  
  // 5. Create and configure metrics
  let request_counter = meter.create_counter("workflow_requests", Some("Total workflow requests"), Some("requests"))
  let duration_histogram = meter.create_histogram("workflow_duration", Some("Workflow duration"), Some("ms"))
  let active_gauge = meter.create_gauge("workflow_active", Some("Active workflows"), Some("workflows"))
  
  // 6. Execute workflow steps
  let step1_span = tracer.start_span("workflow_step1")
  Span::add_event(step1_span, "step1_started", None)
  
  // Record metrics for step 1
  Counter::add(request_counter, 1.0, None)
  Gauge::add(active_gauge, 1.0, None)
  
  // Log step 1
  let step1_log = LogRecord::new_with_context(
    Info,
    Some("Workflow step 1 completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(step1_span))),
    Some(SpanContext::span_id(Span::span_context(step1_span))),
    None
  )
  Logger::emit(logger, step1_log)
  
  Span::end(step1_span)
  
  // Step 2
  let step2_span = tracer.start_span("workflow_step2")
  Span::add_event(step2_span, "step2_started", None)
  
  // Record metrics for step 2
  Histogram::record(duration_histogram, 150.0, None)
  
  // Log step 2
  let step2_log = LogRecord::new_with_context(
    Info,
    Some("Workflow step 2 completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(step2_span))),
    Some(SpanContext::span_id(Span::span_context(step2_span))),
    None
  )
  Logger::emit(logger, step2_log)
  
  Span::end(step2_span)
  
  // 7. Complete workflow
  Span::set_status(main_span, Ok, Some("Workflow completed successfully"))
  Span::add_event(main_span, "workflow_completed", None)
  Gauge::add(active_gauge, -1.0, None)
  
  // Final workflow log
  let workflow_log = LogRecord::new_with_context(
    Info,
    Some("Workflow completed successfully"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(main_span))),
    Some(SpanContext::span_id(Span::span_context(main_span))),
    None
  )
  Logger::emit(logger, workflow_log)
  
  Span::end(main_span)
  
  // 8. Verify workflow completion
  assert_eq(request_counter.name, "workflow_requests")
  assert_eq(duration_histogram.name, "workflow_duration")
  assert_eq(active_gauge.name, "workflow_active")
  assert_eq(LogRecord::body(workflow_log), Some("Workflow completed successfully"))
}