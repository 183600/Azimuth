// Cross-Service Consistency Comprehensive Tests
// This file contains test cases for cross-service consistency functionality

test "cross-service trace context propagation" {
  // Create initial trace context in service A
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id_a = "aaaaaaaaaaaaaaaa"
  let span_ctx_a = SpanContext::new(trace_id, span_id_a, true, "service=A")
  
  // Propagate to service B
  let span_id_b = "bbbbbbbbbbbbbbbb"
  let span_ctx_b = SpanContext::new(trace_id, span_id_b, true, "service=B")
  
  // Propagate to service C
  let span_id_c = "cccccccccccccccc"
  let span_ctx_c = SpanContext::new(trace_id, span_id_c, true, "service=C")
  
  // Verify trace ID consistency across services
  assert_eq(SpanContext::trace_id(span_ctx_a), trace_id)
  assert_eq(SpanContext::trace_id(span_ctx_b), trace_id)
  assert_eq(SpanContext::trace_id(span_ctx_c), trace_id)
  
  // Verify each service has unique span ID
  assert_eq(SpanContext::span_id(span_ctx_a), span_id_a)
  assert_eq(SpanContext::span_id(span_ctx_b), span_id_b)
  assert_eq(SpanContext::span_id(span_ctx_c), span_id_c)
  
  // Verify all spans are sampled
  assert_true(SpanContext::is_sampled(span_ctx_a))
  assert_true(SpanContext::is_sampled(span_ctx_b))
  assert_true(SpanContext::is_sampled(span_ctx_c))
}

test "cross-service baggage consistency" {
  // Initialize baggage in service A
  let baggage_a = Baggage::new()
  let baggage_a_with_user = Baggage::set_entry(baggage_a, "user.id", "user123")
  let baggage_a_final = Baggage::set_entry(baggage_a_with_user, "request.id", "req456")
  
  // Propagate to service B and add service-specific entry
  let baggage_b = Baggage::set_entry(baggage_a_final, "service.b.operation", "process")
  
  // Propagate to service C and add service-specific entry
  let baggage_c = Baggage::set_entry(baggage_b, "service.c.cache", "hit")
  
  // Verify all original entries are preserved
  let user_id = Baggage::get_entry(baggage_c, "user.id")
  let request_id = Baggage::get_entry(baggage_c, "request.id")
  let service_b_op = Baggage::get_entry(baggage_c, "service.b.operation")
  let service_c_cache = Baggage::get_entry(baggage_c, "service.c.cache")
  
  assert_eq(user_id, Some("user123"))
  assert_eq(request_id, Some("req456"))
  assert_eq(service_b_op, Some("process"))
  assert_eq(service_c_cache, Some("hit"))
}

test "cross-service resource attribute consistency" {
  // Create service A resource
  let resource_a = Resource::new()
  let service_a_attrs = [
    ("service.name", StringValue("service-a")),
    ("service.version", StringValue("1.2.3")),
    ("deployment.env", StringValue("production"))
  ]
  let resource_a_final = Resource::with_attributes(resource_a, service_a_attrs)
  
  // Create service B resource with common attributes
  let resource_b = Resource::new()
  let service_b_attrs = [
    ("service.name", StringValue("service-b")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.env", StringValue("production")),
    ("service.b.feature", StringValue("enabled"))
  ]
  let resource_b_final = Resource::with_attributes(resource_b, service_b_attrs)
  
  // Create service C resource with common attributes
  let resource_c = Resource::new()
  let service_c_attrs = [
    ("service.name", StringValue("service-c")),
    ("service.version", StringValue("3.0.1")),
    ("deployment.env", StringValue("production")),
    ("service.c.mode", StringValue("advanced"))
  ]
  let resource_c_final = Resource::with_attributes(resource_c, service_c_attrs)
  
  // Verify common deployment environment
  let a_env = Resource::get_attribute(resource_a_final, "deployment.env")
  let b_env = Resource::get_attribute(resource_b_final, "deployment.env")
  let c_env = Resource::get_attribute(resource_c_final, "deployment.env")
  
  assert_eq(a_env, Some(StringValue("production")))
  assert_eq(b_env, Some(StringValue("production")))
  assert_eq(c_env, Some(StringValue("production")))
  
  // Verify service-specific attributes
  let b_feature = Resource::get_attribute(resource_b_final, "service.b.feature")
  let c_mode = Resource::get_attribute(resource_c_final, "service.c.mode")
  
  assert_eq(b_feature, Some(StringValue("enabled")))
  assert_eq(c_mode, Some(StringValue("advanced")))
}

test "cross-service metric consistency" {
  // Create metrics for service A
  let provider_a = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(provider_a, "service-a")
  let counter_a = Meter::create_counter(meter_a, "requests.total")
  
  // Create metrics for service B
  let provider_b = MeterProvider::default()
  let meter_b = MeterProvider::get_meter(provider_b, "service-b")
  let counter_b = Meter::create_counter(meter_b, "requests.total")
  
  // Create metrics for service C
  let provider_c = MeterProvider::default()
  let meter_c = MeterProvider::get_meter(provider_c, "service-c")
  let counter_c = Meter::create_counter(meter_c, "requests.total")
  
  // Record metrics in all services
  Counter::add(counter_a, 10.0)
  Counter::add(counter_b, 5.0)
  Counter::add(counter_c, 7.0)
  
  // Verify metric names are consistent
  assert_eq(counter_a.name, "requests.total")
  assert_eq(counter_b.name, "requests.total")
  assert_eq(counter_c.name, "requests.total")
}

test "cross-service log correlation" {
  let logger_provider = LoggerProvider::default()
  
  // Create loggers for each service
  let logger_a = LoggerProvider::get_logger(logger_provider, "service-a")
  let logger_b = LoggerProvider::get_logger(logger_provider, "service-b")
  let logger_c = LoggerProvider::get_logger(logger_provider, "service-c")
  
  // Create correlated log records with same trace context
  let trace_id = "fedcba0987654321fedcba0987654321"
  let log_a = LogRecord::new_with_context(
    Info,
    Some("Service A processing request"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some("spanaaaaaaaaaaaaaaaa"),
    None
  )
  
  let log_b = LogRecord::new_with_context(
    Info,
    Some("Service B processing request"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some("spanbbbbbbbbbbbbbbbb"),
    None
  )
  
  let log_c = LogRecord::new_with_context(
    Info,
    Some("Service C processing request"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some("spancccccccccccccccc"),
    None
  )
  
  // Verify trace ID correlation across all logs
  assert_eq(LogRecord::trace_id(log_a), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_b), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_c), Some(trace_id))
  
  // Verify unique span IDs
  assert_eq(LogRecord::span_id(log_a), Some("spanaaaaaaaaaaaaaaaa"))
  assert_eq(LogRecord::span_id(log_b), Some("spanbbbbbbbbbbbbbbbb"))
  assert_eq(LogRecord::span_id(log_c), Some("spancccccccccccccccc"))
}

test "cross-service context propagation with composite propagator" {
  // Create composite propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Service A creates context
  let ctx_a = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let ctx_a_with_corr = Context::with_value(ctx_a, correlation_key, "corr-12345")
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_a_with_corr, carrier)
  
  // Service B extracts context
  let ctx_b = CompositePropagator::extract(composite, carrier)
  
  // Service C extracts context
  let ctx_c = CompositePropagator::extract(composite, carrier)
  
  // Verify extracted context in all services
  let extracted_b = Context::get(ctx_b, ContextKey::new("extracted"))
  let extracted_c = Context::get(ctx_c, ContextKey::new("extracted"))
  
  assert_eq(extracted_b, Some("true"))
  assert_eq(extracted_c, Some("true"))
}

test "cross-service span hierarchy consistency" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross-service-test")
  
  // Service A creates root span
  let root_span = Tracer::start_span(tracer, "service-a.operation")
  let root_ctx = Span::span_context(root_span)
  
  // Service B creates child span
  let child_span_b = Tracer::start_span(tracer, "service-b.operation")
  let child_ctx_b = Span::span_context(child_span_b)
  
  // Service C creates child span
  let child_span_c = Tracer::start_span(tracer, "service-c.operation")
  let child_ctx_c = Span::span_context(child_span_c)
  
  // Verify span hierarchy properties
  assert_eq(Span::name(root_span), "service-a.operation")
  assert_eq(Span::kind(root_span), Internal)
  assert_true(Span::is_recording(root_span))
  
  assert_eq(Span::name(child_span_b), "service-b.operation")
  assert_eq(Span::kind(child_span_b), Internal)
  assert_true(Span::is_recording(child_span_b))
  
  assert_eq(Span::name(child_span_c), "service-c.operation")
  assert_eq(Span::kind(child_span_c), Internal)
  assert_true(Span::is_recording(child_span_c))
  
  // End all spans
  Span::end(root_span)
  Span::end(child_span_b)
  Span::end(child_span_c)
}

test "cross-service error propagation consistency" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error-propagation-test")
  
  // Service A creates error log
  let error_log_a = LogRecord::new_with_context(
    Error,
    Some("Service A: Database connection failed"),
    None,
    Some(1735689600000000000L),
    None,
    Some("error-trace-12345678"),
    Some("error-span-aaaaaaaa"),
    None
  )
  
  // Service B creates error log with same trace
  let error_log_b = LogRecord::new_with_context(
    Error,
    Some("Service B: Processing failed due to upstream error"),
    None,
    Some(1735689600000000000L),
    None,
    Some("error-trace-12345678"),
    Some("error-span-bbbbbbbb"),
    None
  )
  
  // Service C creates error log with same trace
  let error_log_c = LogRecord::new_with_context(
    Error,
    Some("Service C: Request failed after retries"),
    None,
    Some(1735689600000000000L),
    None,
    Some("error-trace-12345678"),
    Some("error-span-cccccccc"),
    None
  )
  
  // Verify error trace correlation
  assert_eq(LogRecord::trace_id(error_log_a), Some("error-trace-12345678"))
  assert_eq(LogRecord::trace_id(error_log_b), Some("error-trace-12345678"))
  assert_eq(LogRecord::trace_id(error_log_c), Some("error-trace-12345678"))
  
  // Verify all are error severity
  assert_eq(LogRecord::severity_number(error_log_a), Error)
  assert_eq(LogRecord::severity_number(error_log_b), Error)
  assert_eq(LogRecord::severity_number(error_log_c), Error)
}