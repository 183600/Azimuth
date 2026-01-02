test "azimuth_integrated_telemetry_workflow_tests" {
  // Test 1: End-to-end trace with metrics and logs correlation
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "integrated_test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "integrated_test")
  let counter = Meter::create_counter(meter, "operation_counter")
  let histogram = Meter::create_histogram(meter, "operation_duration")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "integrated_test")
  
  // Start a root span
  let root_span = Tracer::start_span(tracer, "root_operation")
  let root_span_ctx = Span::span_context(root_span)
  let trace_id = SpanContext::trace_id(root_span_ctx)
  let span_id = SpanContext::span_id(root_span_ctx)
  
  // Record metrics for the operation
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 100.0)
  
  // Create correlated log record
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Starting root operation"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  Logger::emit(logger, log_record)
  
  // Create child span
  let child_span = Tracer::start_span(tracer, "child_operation")
  let child_span_ctx = Span::span_context(child_span)
  let child_span_id = SpanContext::span_id(child_span_ctx)
  
  // Record metrics for child operation
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 50.0)
  
  // Create correlated log record for child
  let child_log = LogRecord::new_with_context(
    Info,
    Some("Executing child operation"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some(child_span_id),
    None
  )
  Logger::emit(logger, child_log)
  
  // End spans
  Span::end(child_span)
  Span::end(root_span)
  
  // Verify trace and span IDs are consistent
  @assert(trace_id == SpanContext::trace_id(root_span_ctx))
  @assert(trace_id == SpanContext::trace_id(child_span_ctx))
  
  // Test 2: Context propagation with baggage across service boundaries
  let root_ctx = Context::root()
  let baggage = Baggage::new()
  let populated_baggage = Baggage::set_entry(baggage, "user_id", "12345")
  let _ = Baggage::set_entry(populated_baggage, "request_id", "req-67890")
  
  // Create context with baggage
  let ctx_key = ContextKey::new("operation")
  let ctx_with_baggage = Context::with_value(root_ctx, ctx_key, "process_data")
  
  // Create propagator for cross-service propagation
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Create carrier for HTTP headers
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, ctx_with_baggage, carrier)
  
  // Extract context from carrier (simulating service boundary)
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify context propagation
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  @assert(extracted_value.is_some())
  
  // Test 3: Resource attributes inheritance in telemetry pipeline
  let service_resource = Resource::new()
  let service_attrs = [
    ("service.name", StringValue("azimuth_test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let populated_service_resource = Resource::with_attributes(service_resource, service_attrs)
  
  let host_resource = Resource::new()
  let host_attrs = [
    ("host.name", StringValue("test-host")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux"))
  ]
  let populated_host_resource = Resource::with_attributes(host_resource, host_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(populated_service_resource, populated_host_resource)
  
  // Verify merged resource contains all attributes
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  
  @assert(service_name.is_some())
  @assert(host_name.is_some())
  
  // Test 4: Multi-instrument metric aggregation scenario
  let request_counter = Meter::create_counter(meter, "http_requests_total")
  let response_counter = Meter::create_counter(meter, "http_responses_total")
  let error_counter = Meter::create_counter(meter, "http_errors_total")
  let duration_histogram = Meter::create_histogram(meter, "http_request_duration")
  let active_requests_gauge = Meter::create_gauge(meter, "http_active_requests")
  
  // Simulate request processing
  let request_attrs = Attributes::new()
  Attributes::set(request_attrs, "method", StringValue("GET"))
  Attributes::set(request_attrs, "endpoint", StringValue("/api/data"))
  
  Counter::add(request_counter, 1.0, Some(request_attrs))
  // Simulate active requests
  // Note: Gauge doesn't have a set method in the current implementation
  
  // Simulate processing time
  Histogram::record(duration_histogram, 150.0, Some(request_attrs))
  
  // Simulate response
  Counter::add(response_counter, 1.0, Some(request_attrs))
  
  // Simulate error scenario (10% of requests)
  let random = Random::system()
  let random_value = Random::next_u64(random)
  if random_value % 10 == 0 {
    Counter::add(error_counter, 1.0, Some(request_attrs))
  }
  
  // Test 5: Comprehensive logging with trace and span correlation
  let operation_span = Tracer::start_span(tracer, "database_operation")
  let operation_ctx = Span::span_context(operation_span)
  
  // Create log with full correlation
  let db_log = LogRecord::new_with_context(
    Debug,
    Some("Executing database query"),
    Some(request_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(operation_ctx)),
    Some(SpanContext::span_id(operation_ctx)),
    Some(ctx_with_baggage)
  )
  Logger::emit(logger, db_log)
  
  // Create error log with correlation
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Database query failed"),
    Some(request_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(operation_ctx)),
    Some(SpanContext::span_id(operation_ctx)),
    Some(ctx_with_baggage)
  )
  Logger::emit(logger, error_log)
  
  Span::end(operation_span)
  
  // Test 6: Complex span hierarchy with events and status
  let parent_span = Tracer::start_span(tracer, "complex_operation")
  Span::set_status(parent_span, Ok, Some("Operation started"))
  
  // Add events to parent span
  Span::add_event(parent_span, "Initialization complete", Some([
    ("component", StringValue("initializer")),
    ("duration_ms", IntValue(50))
  ]))
  
  // Create nested child spans
  let child1_span = Tracer::start_span(tracer, "sub_operation_1")
  Span::add_event(child1_span, "Processing step 1", Some([
    ("step", IntValue(1)),
    ("status", StringValue("in_progress"))
  ]))
  Span::end(child1_span)
  
  let child2_span = Tracer::start_span(tracer, "sub_operation_2")
  Span::add_event(child2_span, "Processing step 2", Some([
    ("step", IntValue(2)),
    ("status", StringValue("completed"))
  ]))
  Span::end(child2_span)
  
  // Complete parent span
  Span::set_status(parent_span, Ok, Some("All operations completed successfully"))
  Span::end(parent_span)
  
  // Test 7: Instrumentation scope consistency across telemetry types
  let scope_name = "azimuth.integrated.test"
  let scope_version = "1.0.0"
  let scope_schema = "https://example.com/schema/v1"
  
  let scoped_tracer = TracerProvider::get_tracer(tracer_provider, scope_name, Some(scope_version))
  let scoped_meter = MeterProvider::get_meter(meter_provider, scope_name)
  let scoped_logger = LoggerProvider::get_logger(logger_provider, scope_name)
  
  // Verify instrumentation scope consistency
  let tracer_scope = Tracer::instrumentation_scope(scoped_tracer)
  let meter_scope = Meter::scope(scoped_meter)
  let logger_scope = Logger::scope(scoped_logger)
  
  @assert(tracer_scope.name == scope_name)
  @assert(meter_scope.name == scope_name)
  @assert(logger_scope.name == scope_name)
}