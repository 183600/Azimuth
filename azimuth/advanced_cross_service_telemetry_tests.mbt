// Advanced Cross-Service Telemetry Tests for Azimuth
// Tests complex scenarios involving distributed tracing and telemetry consistency

test "distributed_trace_context_integrity" {
  // Test trace context integrity across multiple service hops
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Service Gateway: Entry point
  let gateway_ctx = Context::root()
  let gateway_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, gateway_ctx, gateway_carrier)
  
  // Service A: Processes request
  let service_a_ctx = CompositePropagator::extract(composite, gateway_carrier)
  let service_a_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, service_a_ctx, service_a_carrier)
  
  // Service B: Processes request from Service A
  let service_b_ctx = CompositePropagator::extract(composite, service_a_carrier)
  let service_b_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, service_b_ctx, service_b_carrier)
  
  // Service C: Final service in chain
  let service_c_ctx = CompositePropagator::extract(composite, service_b_carrier)
  
  // Verify trace context propagation integrity
  let gateway_trace = TextMapCarrier::get(gateway_carrier, "traceparent")
  let service_a_trace = TextMapCarrier::get(service_a_carrier, "traceparent")
  let service_b_trace = TextMapCarrier::get(service_b_carrier, "traceparent")
  
  assert_eq(gateway_trace, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(service_a_trace, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(service_b_trace, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Verify context extraction works consistently
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(service_a_ctx, extracted_key), Some("true"))
  assert_eq(Context::get(service_b_ctx, extracted_key), Some("true"))
  assert_eq(Context::get(service_c_ctx, extracted_key), Some("true"))
}

test "baggage_propagation_across_service_boundaries" {
  // Test baggage propagation across multiple services with different baggage entries
  let baggage_propagator = W3CBaggagePropagator::new()
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Service 1: Sets initial baggage
  let baggage1 = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage1, "user.id", "user123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session456")
  
  // Service 2: Adds more baggage entries
  let baggage2 = Baggage::set_entry(baggage_with_session, "request.id", "req789")
  let baggage_with_tenant = Baggage::set_entry(baggage2, "tenant.id", "tenant001")
  
  // Service 3: Reads and modifies baggage
  let user_id = Baggage::get_entry(baggage_with_tenant, "user.id")
  let session_id = Baggage::get_entry(baggage_with_tenant, "session.id")
  let request_id = Baggage::get_entry(baggage_with_tenant, "request.id")
  let tenant_id = Baggage::get_entry(baggage_with_tenant, "tenant.id")
  
  assert_eq(user_id, Some("user123"))
  assert_eq(session_id, Some("session456"))
  assert_eq(request_id, Some("req789"))
  assert_eq(tenant_id, Some("tenant001"))
  
  // Test baggage removal
  let baggage_after_removal = Baggage::remove_entry(baggage_with_tenant, "session.id")
  assert_eq(Baggage::get_entry(baggage_after_removal, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(baggage_after_removal, "user.id"), Some("user123"))
}

test "cross_service_metrics_correlation" {
  // Test metrics correlation across different services
  let provider = MeterProvider::default()
  let meter_service_a = MeterProvider::get_meter(provider, "service-a")
  let meter_service_b = MeterProvider::get_meter(provider, "service-b")
  let meter_service_c = MeterProvider::get_meter(provider, "service-c")
  
  // Service A: Creates metrics
  let counter_a = Meter::create_counter(meter_service_a, "requests_processed", Some("Total requests processed"), Some("count"))
  let histogram_a = Meter::create_histogram(meter_service_a, "request_duration", Some("Request duration"), Some("ms"))
  
  // Service B: Creates metrics
  let counter_b = Meter::create_counter(meter_service_b, "requests_processed", Some("Total requests processed"), Some("count"))
  let gauge_b = Meter::create_gauge(meter_service_b, "active_connections", Some("Active connections"), Some("connections"))
  
  // Service C: Creates metrics
  let updown_counter_c = Meter::create_updown_counter(meter_service_c, "queue_size", Some("Queue size"), Some("items"))
  
  // Verify metric creation and properties
  assert_eq(counter_a.name, "requests_processed")
  assert_eq(counter_a.description, Some("Total requests processed"))
  assert_eq(counter_a.unit, Some("count"))
  
  assert_eq(histogram_a.name, "request_duration")
  assert_eq(gauge_b.name, "active_connections")
  assert_eq(updown_counter_c.name, "queue_size")
  
  // Test metric operations
  Counter::add(counter_a, 10.0)
  Counter::add(counter_b, 5.0)
  UpDownCounter::add(updown_counter_c, 100.0)
  Histogram::record(histogram_a, 250.5)
  
  // Verify instrument type conversions
  let counter_instrument = Counter(counter_a.name, counter_a.description, counter_a.unit)
  let histogram_instrument = Histogram(histogram_a.name, histogram_a.description, histogram_a.unit)
  
  assert_eq(Instrument::name(counter_instrument), "requests_processed")
  assert_eq(Instrument::description(histogram_instrument), Some("Request duration"))
}

test "distributed_logging_correlation" {
  // Test log correlation across distributed services
  let logger_provider = LoggerProvider::default()
  let logger_a = LoggerProvider::get_logger(logger_provider, "service-a")
  let logger_b = LoggerProvider::get_logger(logger_provider, "service-b")
  let logger_c = LoggerProvider::get_logger(logger_provider, "service-c")
  
  // Create correlated log records with trace context
  let trace_id = "trace_123456"
  let span_id_a = "span_a_789"
  let span_id_b = "span_b_012"
  let span_id_c = "span_c_345"
  
  // Service A: Creates log entry
  let log_a = LogRecord::new_with_context(
    Info,
    Some("Processing request in Service A"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(trace_id),
    Some(span_id_a),
    None
  )
  
  // Service B: Creates log entry
  let log_b = LogRecord::new_with_context(
    Info,
    Some("Processing request in Service B"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some(trace_id),
    Some(span_id_b),
    None
  )
  
  // Service C: Creates log entry
  let log_c = LogRecord::new_with_context(
    Warn,
    Some("Warning in Service C"),
    None,
    Some(1735689600000004000L),
    Some(1735689600000005000L),
    Some(trace_id),
    Some(span_id_c),
    None
  )
  
  // Verify log correlation through trace ID
  assert_eq(LogRecord::trace_id(log_a), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_b), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_c), Some(trace_id))
  
  // Verify individual span IDs
  assert_eq(LogRecord::span_id(log_a), Some(span_id_a))
  assert_eq(LogRecord::span_id(log_b), Some(span_id_b))
  assert_eq(LogRecord::span_id(log_c), Some(span_id_c))
  
  // Verify log severity and body
  assert_eq(LogRecord::severity_number(log_a), Info)
  assert_eq(LogRecord::severity_number(log_b), Info)
  assert_eq(LogRecord::severity_number(log_c), Warn)
  
  assert_eq(LogRecord::body(log_a), Some("Processing request in Service A"))
  assert_eq(LogRecord::body(log_b), Some("Processing request in Service B"))
  assert_eq(LogRecord::body(log_c), Some("Warning in Service C"))
}

test "resource_attributes_consistency" {
  // Test resource attributes consistency across services
  let base_resource = Resource::new()
  let service_attrs = [
    ("service.name", StringValue("unified-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-abc123")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("web-server-01"))
  ]
  
  let unified_resource = Resource::with_attributes(base_resource, service_attrs)
  
  // Service A: Uses unified resource
  let service_a_resource = unified_resource
  let service_a_name = Resource::get_attribute(service_a_resource, "service.name")
  let service_a_version = Resource::get_attribute(service_a_resource, "service.version")
  
  // Service B: Uses unified resource
  let service_b_resource = unified_resource
  let service_b_instance = Resource::get_attribute(service_b_resource, "service.instance.id")
  let service_b_env = Resource::get_attribute(service_b_resource, "deployment.environment")
  
  // Service C: Uses unified resource
  let service_c_resource = unified_resource
  let service_c_host = Resource::get_attribute(service_c_resource, "host.name")
  
  // Verify resource attribute consistency
  assert_eq(service_a_name, Some(StringValue("unified-service")))
  assert_eq(service_a_version, Some(StringValue("2.1.0")))
  assert_eq(service_b_instance, Some(StringValue("instance-abc123")))
  assert_eq(service_b_env, Some(StringValue("production")))
  assert_eq(service_c_host, Some(StringValue("web-server-01")))
  
  // Test resource merging
  let additional_attrs = [("custom.attribute", StringValue("custom-value"))]
  let additional_resource = Resource::with_attributes(Resource::new(), additional_attrs)
  let merged_resource = Resource::merge(unified_resource, additional_resource)
  
  assert_eq(
    Resource::get_attribute(merged_resource, "custom.attribute"),
    Some(StringValue("custom-value"))
  )
}

test "span_hierarchy_across_services" {
  // Test span hierarchy and parent-child relationships across services
  let provider = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider, "service-a")
  let tracer_b = TracerProvider::get_tracer(provider, "service-b")
  let tracer_c = TracerProvider::get_tracer(provider, "service-c")
  
  // Service A: Creates root span
  let root_span_ctx = SpanContext::new("root_trace_789", "root_span_123", true, "key1=value1")
  let root_span = Span::new("root-operation", Server, root_span_ctx)
  
  assert_eq(Span::name(root_span), "root-operation")
  assert_eq(Span::kind(root_span), Server)
  assert_eq(Span::is_recording(root_span), true)
  
  // Service B: Creates child span
  let child_span_ctx = SpanContext::new("root_trace_789", "child_span_456", true, "key1=value1,key2=value2")
  let child_span = Span::new("child-operation", Client, child_span_ctx)
  
  assert_eq(Span::name(child_span), "child-operation")
  assert_eq(Span::kind(child_span), Client)
  assert_eq(SpanContext::trace_id(Span::span_context(child_span)), "root_trace_789")
  
  // Service C: Creates grandchild span
  let grandchild_span_ctx = SpanContext::new("root_trace_789", "grandchild_span_789", true, "key1=value1,key2=value2,key3=value3")
  let grandchild_span = Span::new("grandchild-operation", Internal, grandchild_span_ctx)
  
  assert_eq(Span::name(grandchild_span), "grandchild-operation")
  assert_eq(Span::kind(grandchild_span), Internal)
  assert_eq(SpanContext::trace_id(Span::span_context(grandchild_span)), "root_trace_789")
  
  // Verify span context relationships
  assert_eq(SpanContext::trace_id(root_span_ctx), "root_trace_789")
  assert_eq(SpanContext::trace_id(child_span_ctx), "root_trace_789")
  assert_eq(SpanContext::trace_id(grandchild_span_ctx), "root_trace_789")
  
  assert_eq(SpanContext::span_id(root_span_ctx), "root_span_123")
  assert_eq(SpanContext::span_id(child_span_ctx), "child_span_456")
  assert_eq(SpanContext::span_id(grandchild_span_ctx), "grandchild_span_789")
  
  // Test span operations
  Span::set_status(root_span, Ok, Some("Root operation completed"))
  Span::add_event(child_span, "Processing child operation", None)
  Span::end(grandchild_span)
  Span::end(child_span)
  Span::end(root_span)
}

test "error_propagation_across_services" {
  // Test error propagation and handling across distributed services
  let logger_provider = LoggerProvider::default()
  let logger_a = LoggerProvider::get_logger(logger_provider, "service-a")
  let logger_b = LoggerProvider::get_logger(logger_provider, "service-b")
  let logger_c = LoggerProvider::get_logger(logger_provider, "service-c")
  
  let provider = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider, "service-a")
  let tracer_b = TracerProvider::get_tracer(provider, "service-b")
  let tracer_c = TracerProvider::get_tracer(provider, "service-c")
  
  // Service A: Encounteres error and logs it
  let error_span_a = Tracer::start_span(tracer_a, "error-operation-a")
  let error_log_a = LogRecord::new_with_context(
    Error,
    Some("Database connection failed in Service A"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("error_trace_001"),
    Some("error_span_a"),
    None
  )
  
  Span::set_status(error_span_a, Error, Some("Database connection timeout"))
  Span::end(error_span_a)
  
  // Service B: Receives error and propagates
  let error_span_b = Tracer::start_span(tracer_b, "error-operation-b")
  let error_log_b = LogRecord::new_with_context(
    Error,
    Some("Upstream service error in Service B"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some("error_trace_001"),
    Some("error_span_b"),
    None
  )
  
  Span::set_status(error_span_b, Error, Some("Upstream service unavailable"))
  Span::end(error_span_b)
  
  // Service C: Handles error gracefully
  let error_span_c = Tracer::start_span(tracer_c, "error-operation-c")
  let error_log_c = LogRecord::new_with_context(
    Warn,
    Some("Graceful degradation in Service C"),
    None,
    Some(1735689600000004000L),
    Some(1735689600000005000L),
    Some("error_trace_001"),
    Some("error_span_c"),
    None
  )
  
  Span::set_status(error_span_c, Ok, Some("Fallback mechanism activated"))
  Span::end(error_span_c)
  
  // Verify error correlation and trace consistency
  assert_eq(LogRecord::trace_id(error_log_a), Some("error_trace_001"))
  assert_eq(LogRecord::trace_id(error_log_b), Some("error_trace_001"))
  assert_eq(LogRecord::trace_id(error_log_c), Some("error_trace_001"))
  
  assert_eq(LogRecord::severity_number(error_log_a), Error)
  assert_eq(LogRecord::severity_number(error_log_b), Error)
  assert_eq(LogRecord::severity_number(error_log_c), Warn)
  
  assert_eq(Span::status(error_span_a), Error)
  assert_eq(Span::status(error_span_b), Error)
  assert_eq(Span::status(error_span_c), Ok)
}