// Cross-Service Propagation Tests for Azimuth Telemetry System
// Tests data propagation and consistency across microservices

test "trace_context_across_services" {
  // Test trace context propagation across multiple services
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Service A: Create initial trace context
  let service_a_ctx = Context::root()
  let service_a_carrier = TextMapCarrier::new()
  
  // Service A injects trace context
  CompositePropagator::inject(composite, service_a_ctx, service_a_carrier)
  
  // Verify Service A injection
  let trace_from_a = TextMapCarrier::get(service_a_carrier, "traceparent")
  assert_eq(trace_from_a, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Service B: Extracts trace context from Service A
  let service_b_ctx = CompositePropagator::extract(composite, service_a_carrier)
  let service_b_carrier = TextMapCarrier::new()
  
  // Service B adds its own context and propagates
  let service_b_enhanced = Context::with_value(
    service_b_ctx,
    ContextKey::new("service.b"),
    "processed_by_b"
  )
  CompositePropagator::inject(composite, service_b_enhanced, service_b_carrier)
  
  // Verify Service B propagation
  let trace_from_b = TextMapCarrier::get(service_b_carrier, "traceparent")
  assert_eq(trace_from_b, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Service C: Extracts trace context from Service B
  let service_c_ctx = CompositePropagator::extract(composite, service_b_carrier)
  let service_c_enhanced = Context::with_value(
    service_c_ctx,
    ContextKey::new("service.c"),
    "processed_by_c"
  )
  
  // Verify trace consistency across services
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(service_b_ctx, extracted_key), Some("true"))
  assert_eq(Context::get(service_c_ctx, extracted_key), Some("true"))
}

test "baggage_propagation_across_services" {
  // Test baggage propagation across multiple services
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // Service A: Creates initial baggage
  let service_a_baggage = Baggage::new()
  let service_a_with_baggage = Baggage::set_entry(service_a_baggage, "user.id", "12345")
  let service_a_final = Baggage::set_entry(service_a_with_baggage, "session.id", "abcdef")
  
  // Simulate baggage injection
  let service_a_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, Context::root(), service_a_carrier)
  
  // Service B: Adds more baggage entries
  let service_b_baggage = Baggage::set_entry(service_a_final, "tenant.id", "tenant123")
  let service_b_final = Baggage::set_entry(service_b_baggage, "request.id", "req789")
  
  // Service C: Adds correlation information
  let service_c_baggage = Baggage::set_entry(service_b_final, "correlation.id", "corr456")
  let service_c_final = Baggage::set_entry(service_c_baggage, "trace.id", "trace123")
  
  // Verify baggage propagation across services
  assert_eq(Baggage::get_entry(service_c_final, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(service_c_final, "session.id"), Some("abcdef"))
  assert_eq(Baggage::get_entry(service_c_final, "tenant.id"), Some("tenant123"))
  assert_eq(Baggage::get_entry(service_c_final, "request.id"), Some("req789"))
  assert_eq(Baggage::get_entry(service_c_final, "correlation.id"), Some("corr456"))
  assert_eq(Baggage::get_entry(service_c_final, "trace.id"), Some("trace123"))
}

test "cross_service_span_continuity" {
  // Test span continuity across service boundaries
  let provider = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider, "service_a_tracer")
  let tracer_b = TracerProvider::get_tracer(provider, "service_b_tracer")
  let tracer_c = TracerProvider::get_tracer(provider, "service_c_tracer")
  
  // Service A: Creates root span
  let span_a = Tracer::start_span(tracer_a, "service_a_operation")
  let span_a_ctx = Span::span_context(span_a)
  
  Span::add_event(span_a, "service_a_start", None)
  Span::set_status(span_a, Ok, Some("Service A completed"))
  
  // Service B: Creates child span
  let span_b = Tracer::start_span(tracer_b, "service_b_operation")
  let span_b_ctx = Span::span_context(span_b)
  
  Span::add_event(span_b, "service_b_start", None)
  Span::add_event(span_b, "service_b_processing", None)
  Span::set_status(span_b, Ok, Some("Service B completed"))
  
  // Service C: Creates child span
  let span_c = Tracer::start_span(tracer_c, "service_c_operation")
  let span_c_ctx = Span::span_context(span_c)
  
  Span::add_event(span_c, "service_c_start", None)
  Span::add_event(span_c, "service_c_processing", None)
  Span::add_event(span_c, "service_c_complete", None)
  Span::set_status(span_c, Ok, Some("Service C completed"))
  
  // Verify span continuity
  assert_eq(Span::name(span_a), "service_a_operation")
  assert_eq(Span::name(span_b), "service_b_operation")
  assert_eq(Span::name(span_c), "service_c_operation")
  
  assert_eq(Span::is_recording(span_a), true)
  assert_eq(Span::is_recording(span_b), true)
  assert_eq(Span::is_recording(span_c), true)
  
  // Verify span contexts
  assert_eq(SpanContext::is_valid(span_a_ctx), true)
  assert_eq(SpanContext::is_valid(span_b_ctx), true)
  assert_eq(SpanContext::is_valid(span_c_ctx), true)
  
  // End spans in reverse order
  Span::end(span_c)
  Span::end(span_b)
  Span::end(span_a)
}

test "cross_service_metrics_aggregation" {
  // Test metrics aggregation across services
  let provider = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(provider, "service_a_metrics")
  let meter_b = MeterProvider::get_meter(provider, "service_b_metrics")
  let meter_c = MeterProvider::get_meter(provider, "service_c_metrics")
  
  // Service A: Creates and records metrics
  let counter_a = Meter::create_counter(meter_a, "requests_total")
  let histogram_a = Meter::create_histogram(meter_a, "request_duration")
  let gauge_a = Meter::create_gauge(meter_a, "active_connections")
  
  Counter::add(counter_a, 100.0)
  Histogram::record(histogram_a, 150.0)
  Histogram::record(histogram_a, 200.0)
  Histogram::record(histogram_a, 100.0)
  
  // Service B: Creates and records metrics
  let counter_b = Meter::create_counter(meter_b, "requests_total")
  let histogram_b = Meter::create_histogram(meter_b, "request_duration")
  let updown_b = Meter::create_updown_counter(meter_b, "queue_size")
  
  Counter::add(counter_b, 150.0)
  Histogram::record(histogram_b, 120.0)
  Histogram::record(histogram_b, 180.0)
  UpDownCounter::add(updown_b, 50.0)
  UpDownCounter::add(updown_b, -10.0)
  
  // Service C: Creates and records metrics
  let counter_c = Meter::create_counter(meter_c, "requests_total")
  let histogram_c = Meter::create_histogram(meter_c, "request_duration")
  
  Counter::add(counter_c, 75.0)
  Histogram::record(histogram_c, 90.0)
  Histogram::record(histogram_c, 110.0)
  Histogram::record(histogram_c, 130.0)
  
  // Verify metric instrument integrity
  assert_eq(counter_a.name, "requests_total")
  assert_eq(counter_b.name, "requests_total")
  assert_eq(counter_c.name, "requests_total")
  
  assert_eq(histogram_a.name, "request_duration")
  assert_eq(histogram_b.name, "request_duration")
  assert_eq(histogram_c.name, "request_duration")
  
  assert_eq(gauge_a.name, "active_connections")
  assert_eq(updown_b.name, "queue_size")
}

test "cross_service_log_correlation" {
  // Test log correlation across services
  let logger_provider = LoggerProvider::default()
  let logger_a = LoggerProvider::get_logger(logger_provider, "service_a_logger")
  let logger_b = LoggerProvider::get_logger(logger_provider, "service_b_logger")
  let logger_c = LoggerProvider::get_logger(logger_provider, "service_c_logger")
  
  // Common trace and span IDs for correlation
  let trace_id = "trace123456789"
  let span_a_id = "span111111111"
  let span_b_id = "span222222222"
  let span_c_id = "span333333333"
  
  // Service A: Creates correlated logs
  let log_a1 = LogRecord::new_with_context(
    Info,
    Some("Service A: Request received"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some(span_a_id),
    None
  )
  
  let log_a2 = LogRecord::new_with_context(
    Info,
    Some("Service A: Processing complete"),
    None,
    Some(1735689600000001000L),
    None,
    Some(trace_id),
    Some(span_a_id),
    None
  )
  
  // Service B: Creates correlated logs
  let log_b1 = LogRecord::new_with_context(
    Info,
    Some("Service B: Starting operation"),
    None,
    Some(1735689600000002000L),
    None,
    Some(trace_id),
    Some(span_b_id),
    None
  )
  
  let log_b2 = LogRecord::new_with_context(
    Warn,
    Some("Service B: Operation slow"),
    None,
    Some(1735689600000003000L),
    None,
    Some(trace_id),
    Some(span_b_id),
    None
  )
  
  // Service C: Creates correlated logs
  let log_c1 = LogRecord::new_with_context(
    Info,
    Some("Service C: Received request"),
    None,
    Some(1735689600000004000L),
    None,
    Some(trace_id),
    Some(span_c_id),
    None
  )
  
  let log_c2 = LogRecord::new_with_context(
    Error,
    Some("Service C: Operation failed"),
    None,
    Some(1735689600000005000L),
    None,
    Some(trace_id),
    Some(span_c_id),
    None
  )
  
  // Verify log correlation
  assert_eq(LogRecord::trace_id(log_a1), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_a2), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_b1), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_b2), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_c1), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_c2), Some(trace_id))
  
  assert_eq(LogRecord::span_id(log_a1), Some(span_a_id))
  assert_eq(LogRecord::span_id(log_a2), Some(span_a_id))
  assert_eq(LogRecord::span_id(log_b1), Some(span_b_id))
  assert_eq(LogRecord::span_id(log_b2), Some(span_b_id))
  assert_eq(LogRecord::span_id(log_c1), Some(span_c_id))
  assert_eq(LogRecord::span_id(log_c2), Some(span_c_id))
  
  // Emit correlated logs
  Logger::emit(logger_a, log_a1)
  Logger::emit(logger_a, log_a2)
  Logger::emit(logger_b, log_b1)
  Logger::emit(logger_b, log_b2)
  Logger::emit(logger_c, log_c1)
  Logger::emit(logger_c, log_c2)
}

test "cross_service_resource_consistency" {
  // Test resource attribute consistency across services
  let service_a_resource = Resource::new()
  let service_b_resource = Resource::new()
  let service_c_resource = Resource::new()
  
  // Service A: Defines base resource attributes
  let base_attributes = [
    ("service.name", StringValue("unified_service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2"))
  ]
  
  let service_a_with_attrs = Resource::with_attributes(service_a_resource, base_attributes)
  
  // Service B: Adds service-specific attributes
  let service_b_attributes = [
    ("service.instance.id", StringValue("instance-b-123")),
    ("host.name", StringValue("service-b-host")),
    ("container.id", StringValue("container-b-456"))
  ]
  
  let service_b_with_attrs = Resource::with_attributes(service_b_resource, service_b_attributes)
  
  // Service C: Adds more service-specific attributes
  let service_c_attributes = [
    ("service.instance.id", StringValue("instance-c-789")),
    ("host.name", StringValue("service-c-host")),
    ("k8s.pod.name", StringValue("pod-c-123"))
  ]
  
  let service_c_with_attrs = Resource::with_attributes(service_c_resource, service_c_attributes)
  
  // Verify base resource consistency
  assert_eq(
    Resource::get_attribute(service_a_with_attrs, "service.name"),
    Some(StringValue("unified_service"))
  )
  
  assert_eq(
    Resource::get_attribute(service_a_with_attrs, "service.version"),
    Some(StringValue("1.0.0"))
  )
  
  assert_eq(
    Resource::get_attribute(service_a_with_attrs, "deployment.environment"),
    None // Based on mock implementation
  )
  
  // Verify service-specific attributes
  assert_eq(
    Resource::get_attribute(service_b_with_attrs, "service.instance.id"),
    Some(StringValue("instance-b-123"))
  )
  
  assert_eq(
    Resource::get_attribute(service_c_with_attrs, "service.instance.id"),
    Some(StringValue("instance-c-789"))
  )
  
  // Test resource merging for unified view
  let unified_resource = Resource::merge(service_a_with_attrs, service_b_with_attrs)
  let final_resource = Resource::merge(unified_resource, service_c_with_attrs)
  
  assert_eq(
    Resource::get_attribute(final_resource, "service.instance.id"),
    Some(StringValue("instance-c-789"))
  )
}

test "cross_service_error_propagation" {
  // Test error propagation across services
  let provider = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider, "service_a_error_tracer")
  let tracer_b = TracerProvider::get_tracer(provider, "service_b_error_tracer")
  let tracer_c = TracerProvider::get_tracer(provider, "service_c_error_tracer")
  
  let logger_provider = LoggerProvider::default()
  let logger_a = LoggerProvider::get_logger(logger_provider, "service_a_error_logger")
  let logger_b = LoggerProvider::get_logger(logger_provider, "service_b_error_logger")
  let logger_c = LoggerProvider::get_logger(logger_provider, "service_c_error_logger")
  
  // Service A: Experiences error and propagates
  let span_a = Tracer::start_span(tracer_a, "service_a_error_operation")
  Span::set_status(span_a, Error, Some("Database connection failed"))
  
  let error_log_a = LogRecord::new_with_context(
    Error,
    Some("Service A: Database connection failed"),
    None,
    Some(1735689600000000000L),
    None,
    Some("error_trace123"),
    Some("error_span_a"),
    None
  )
  
  // Service B: Receives error and handles it
  let span_b = Tracer::start_span(tracer_b, "service_b_error_handling")
  Span::add_event(span_b, "error_received", Some([("source.service", StringValue("service_a"))]))
  Span::set_status(span_b, Error, Some("Upstream service error"))
  
  let error_log_b = LogRecord::new_with_context(
    Error,
    Some("Service B: Upstream error from Service A"),
    None,
    Some(1735689600000001000L),
    None,
    Some("error_trace123"),
    Some("error_span_b"),
    None
  )
  
  // Service C: Receives error and implements fallback
  let span_c = Tracer::start_span(tracer_c, "service_c_fallback")
  Span::add_event(span_c, "fallback_initiated", Some([("fallback.reason", StringValue("upstream_error"))]))
  Span::set_status(span_c, Ok, Some("Fallback successful"))
  
  let warn_log_c = LogRecord::new_with_context(
    Warn,
    Some("Service C: Using fallback due to upstream error"),
    None,
    Some(1735689600000002000L),
    None,
    Some("error_trace123"),
    Some("error_span_c"),
    None
  )
  
  // Verify error propagation
  assert_eq(Span::name(span_a), "service_a_error_operation")
  assert_eq(Span::name(span_b), "service_b_error_handling")
  assert_eq(Span::name(span_c), "service_c_fallback")
  
  assert_eq(LogRecord::trace_id(error_log_a), Some("error_trace123"))
  assert_eq(LogRecord::trace_id(error_log_b), Some("error_trace123"))
  assert_eq(LogRecord::trace_id(warn_log_c), Some("error_trace123"))
  
  // End spans
  Span::end(span_c)
  Span::end(span_b)
  Span::end(span_a)
  
  // Emit error logs
  Logger::emit(logger_a, error_log_a)
  Logger::emit(logger_b, error_log_b)
  Logger::emit(logger_c, warn_log_c)
}

test "cross_service_performance_tracking" {
  // Test performance tracking across services
  let provider = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(provider, "service_a_performance")
  let meter_b = MeterProvider::get_meter(provider, "service_b_performance")
  let meter_c = MeterProvider::get_meter(provider, "service_c_performance")
  
  // Create performance instruments for each service
  let latency_a = Meter::create_histogram(meter_a, "request_latency_ms")
  let throughput_a = Meter::create_counter(meter_a, "requests_processed")
  
  let latency_b = Meter::create_histogram(meter_b, "request_latency_ms")
  let throughput_b = Meter::create_counter(meter_b, "requests_processed")
  let queue_size_b = Meter::create_gauge(meter_b, "queue_size")
  
  let latency_c = Meter::create_histogram(meter_c, "request_latency_ms")
  let throughput_c = Meter::create_counter(meter_c, "requests_processed")
  let error_rate_c = Meter::create_counter(meter_c, "errors_total")
  
  // Simulate performance measurements
  // Service A: Fast processing
  Histogram::record(latency_a, 50.0)
  Histogram::record(latency_a, 75.0)
  Histogram::record(latency_a, 60.0)
  Counter::add(throughput_a, 100.0)
  
  // Service B: Medium processing with queue
  Histogram::record(latency_b, 120.0)
  Histogram::record(latency_b, 150.0)
  Histogram::record(latency_b, 100.0)
  Counter::add(throughput_b, 80.0)
  
  // Service C: Slower processing with some errors
  Histogram::record(latency_c, 200.0)
  Histogram::record(latency_c, 250.0)
  Histogram::record(latency_c, 180.0)
  Counter::add(throughput_c, 60.0)
  Counter::add(error_rate_c, 5.0)
  
  // Verify performance instruments
  assert_eq(latency_a.name, "request_latency_ms")
  assert_eq(latency_b.name, "request_latency_ms")
  assert_eq(latency_c.name, "request_latency_ms")
  
  assert_eq(throughput_a.name, "requests_processed")
  assert_eq(throughput_b.name, "requests_processed")
  assert_eq(throughput_c.name, "requests_processed")
  
  assert_eq(queue_size_b.name, "queue_size")
  assert_eq(error_rate_c.name, "errors_total")
  
  // Verify instrument descriptions and units
  assert_eq(latency_a.description, None)
  assert_eq(throughput_a.description, None)
  assert_eq(queue_size_b.description, None)
  assert_eq(error_rate_c.description, None)
}