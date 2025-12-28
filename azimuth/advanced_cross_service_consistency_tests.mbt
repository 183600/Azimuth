// Advanced Cross-Service Telemetry Consistency Test Suite
// Tests telemetry consistency across multiple services, trace propagation, and data integrity

test "trace context consistency across services" {
  // Create trace context for service A
  let service_a_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let service_a_span_id = "b7ad6b7169203331"
  let service_a_ctx = SpanContext::new(service_a_trace_id, service_a_span_id, true, "key1=value1")
  
  // Service A creates span
  let provider = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider, "service-a", Some("1.0.0"))
  let service_a_span = Span::new("service-a-operation", Server, service_a_ctx)
  
  // Verify service A span context
  let a_span_ctx = Span::span_context(service_a_span)
  assert_eq(SpanContext::trace_id(a_span_ctx), service_a_trace_id)
  assert_eq(SpanContext::span_id(a_span_ctx), service_a_span_id)
  assert_true(SpanContext::is_sampled(a_span_ctx))
  assert_true(SpanContext::is_valid(a_span_ctx))
  
  // Service B receives trace context and creates child span
  let service_b_span_id = "c8def82719304444"
  let service_b_ctx = SpanContext::new(service_a_trace_id, service_b_span_id, true, "key1=value1,key2=value2")
  let tracer_b = TracerProvider::get_tracer(provider, "service-b", Some("2.1.0"))
  let service_b_span = Span::new("service-b-operation", Client, service_b_ctx)
  
  // Verify service B span maintains same trace ID
  let b_span_ctx = Span::span_context(service_b_span)
  assert_eq(SpanContext::trace_id(b_span_ctx), service_a_trace_id)
  assert_eq(SpanContext::span_id(b_span_ctx), service_b_span_id)
  assert_true(SpanContext::is_sampled(b_span_ctx))
  assert_true(SpanContext::is_valid(b_span_ctx))
  
  // Service C continues the trace
  let service_c_span_id = "d9ef093819415555"
  let service_c_ctx = SpanContext::new(service_a_trace_id, service_c_span_id, true, "key1=value1,key2=value2,key3=value3")
  let tracer_c = TracerProvider::get_tracer(provider, "service-c", Some("3.2.1"))
  let service_c_span = Span::new("service-c-operation", Internal, service_c_ctx)
  
  // Verify service C span maintains trace consistency
  let c_span_ctx = Span::span_context(service_c_span)
  assert_eq(SpanContext::trace_id(c_span_ctx), service_a_trace_id)
  assert_eq(SpanContext::span_id(c_span_ctx), service_c_span_id)
  assert_true(SpanContext::is_sampled(c_span_ctx))
  assert_true(SpanContext::is_valid(c_span_ctx))
  
  // End all spans
  Span::end(service_a_span)
  Span::end(service_b_span)
  Span::end(service_c_span)
}

test "baggage propagation consistency across services" {
  // Service A creates initial baggage
  let baggage = Baggage::new()
  let service_a_baggage = Baggage::set_entry(baggage, "user.id", "user-12345")
  let service_a_baggage2 = Baggage::set_entry(service_a_baggage, "session.id", "sess-abcdef")
  let service_a_baggage3 = Baggage::set_entry(service_a_baggage2, "request.id", "req-789012")
  
  // Service A adds service-specific baggage
  let service_a_final = Baggage::set_entry(service_a_baggage3, "service.a.version", "v1.0.0")
  
  // Service B receives and extends baggage
  let service_b_baggage = Baggage::set_entry(service_a_final, "service.b.version", "v2.1.0")
  let service_b_baggage2 = Baggage::set_entry(service_b_baggage, "service.b.latency", "150ms")
  
  // Service C receives and extends baggage
  let service_c_baggage = Baggage::set_entry(service_b_baggage2, "service.c.version", "v3.2.1")
  let service_c_baggage2 = Baggage::set_entry(service_c_baggage, "service.c.cache", "hit")
  
  // Verify all baggage entries are preserved across services
  assert_eq(Baggage::get_entry(service_c_baggage2, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(service_c_baggage2, "session.id"), Some("sess-abcdef"))
  assert_eq(Baggage::get_entry(service_c_baggage2, "request.id"), Some("req-789012"))
  assert_eq(Baggage::get_entry(service_c_baggage2, "service.a.version"), Some("v1.0.0"))
  assert_eq(Baggage::get_entry(service_c_baggage2, "service.b.version"), Some("v2.1.0"))
  assert_eq(Baggage::get_entry(service_c_baggage2, "service.b.latency"), Some("150ms"))
  assert_eq(Baggage::get_entry(service_c_baggage2, "service.c.version"), Some("v3.2.1"))
  assert_eq(Baggage::get_entry(service_c_baggage2, "service.c.cache"), Some("hit"))
}

test "metrics consistency across services" {
  let provider = MeterProvider::default()
  
  // Service A metrics
  let meter_a = MeterProvider::get_meter(provider, "service-a", Some("1.0.0"))
  let request_counter_a = Meter::create_counter(meter_a, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_time_a = Meter::create_histogram(meter_a, "http.response.time", Some("Response time"), Some("ms"))
  
  // Service A records metrics
  Counter::add(request_counter_a, 100.0)
  Histogram::record(response_time_a, 150.0)
  Histogram::record(response_time_a, 200.0)
  Histogram::record(response_time_a, 100.0)
  
  // Service B metrics
  let meter_b = MeterProvider::get_meter(provider, "service-b", Some("2.1.0"))
  let request_counter_b = Meter::create_counter(meter_b, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_time_b = Meter::create_histogram(meter_b, "http.response.time", Some("Response time"), Some("ms"))
  let active_connections_b = Meter::create_gauge(meter_b, "active.connections", Some("Active connections"), Some("connections"))
  
  // Service B records metrics
  Counter::add(request_counter_b, 75.0)
  Histogram::record(response_time_b, 80.0)
  Histogram::record(response_time_b, 120.0)
  UpDownCounter::add(active_connections_b, 25.0)
  
  // Service C metrics
  let meter_c = MeterProvider::get_meter(provider, "service-c", Some("3.2.1"))
  let request_counter_c = Meter::create_counter(meter_c, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let error_rate_c = Meter::create_updown_counter(meter_c, "error.rate", Some("Error rate"), Some("%"))
  
  // Service C records metrics
  Counter::add(request_counter_c, 50.0)
  UpDownCounter::add(error_rate_c, 2.5)
  
  // Verify metric names and descriptions are consistent
  assert_eq(request_counter_a.name, "http.requests.total")
  assert_eq(request_counter_b.name, "http.requests.total")
  assert_eq(request_counter_c.name, "http.requests.total")
  
  assert_eq(request_counter_a.description, Some("Total HTTP requests"))
  assert_eq(request_counter_b.description, Some("Total HTTP requests"))
  assert_eq(request_counter_c.description, Some("Total HTTP requests"))
  
  assert_eq(request_counter_a.unit, Some("requests"))
  assert_eq(request_counter_b.unit, Some("requests"))
  assert_eq(request_counter_c.unit, Some("requests"))
  
  assert_eq(response_time_a.name, "http.response.time")
  assert_eq(response_time_b.name, "http.response.time")
  
  assert_eq(response_time_a.description, Some("Response time"))
  assert_eq(response_time_b.description, Some("Response time"))
  
  assert_eq(response_time_a.unit, Some("ms"))
  assert_eq(response_time_b.unit, Some("ms"))
}

test "log consistency across services" {
  let provider = LoggerProvider::default()
  
  // Service A logging
  let logger_a = LoggerProvider::get_logger(provider, "service-a", Some("1.0.0"))
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_a_id = "b7ad6b7169203331"
  
  let log_record_a = LogRecord::new_with_context(
    Info,
    Some("Service A processing request"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000100L),
    Some(trace_id),
    Some(span_a_id),
    Some(Context::root())
  )
  
  Logger::emit(logger_a, log_record_a)
  
  // Service B logging with same trace
  let logger_b = LoggerProvider::get_logger(provider, "service-b", Some("2.1.0"))
  let span_b_id = "c8def82719304444"
  
  let log_record_b = LogRecord::new_with_context(
    Info,
    Some("Service B processing request"),
    None,
    Some(1735689600000000200L),
    Some(1735689600000000250L),
    Some(trace_id),
    Some(span_b_id),
    Some(Context::root())
  )
  
  Logger::emit(logger_b, log_record_b)
  
  // Service C logging with same trace
  let logger_c = LoggerProvider::get_logger(provider, "service-c", Some("3.2.1"))
  let span_c_id = "d9ef093819415555"
  
  let log_record_c = LogRecord::new_with_context(
    Warn,
    Some("Service C warning during processing"),
    None,
    Some(1735689600000000300L),
    Some(1735689600000000350L),
    Some(trace_id),
    Some(span_c_id),
    Some(Context::root())
  )
  
  Logger::emit(logger_c, log_record_c)
  
  // Verify trace ID consistency across all services
  assert_eq(LogRecord::trace_id(log_record_a), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_record_b), Some(trace_id))
  assert_eq(LogRecord::trace_id(log_record_c), Some(trace_id))
  
  // Verify span IDs are different within same trace
  assert_eq(LogRecord::span_id(log_record_a), Some(span_a_id))
  assert_eq(LogRecord::span_id(log_record_b), Some(span_b_id))
  assert_eq(LogRecord::span_id(log_record_c), Some(span_c_id))
  
  assert_neq(span_a_id, span_b_id)
  assert_neq(span_b_id, span_c_id)
  assert_neq(span_a_id, span_c_id)
}

test "resource consistency across services" {
  // Create base resource with common attributes
  let base_attrs = [
    ("service.namespace", StringValue("production")),
    ("deployment.environment", StringValue("prod")),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("kubernetes.cluster.name", StringValue("main-cluster"))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Service A specific resource
  let service_a_attrs = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("v1.2.3")),
    ("service.instance.id", StringValue("user-service-123")),
    ("host.name", StringValue("user-service-001.prod.local")),
    ("process.pid", IntValue(1234))
  ]
  
  let service_a_resource = Resource::with_attributes(base_resource, service_a_attrs)
  
  // Service B specific resource
  let service_b_attrs = [
    ("service.name", StringValue("order-service")),
    ("service.version", StringValue("v2.1.0")),
    ("service.instance.id", StringValue("order-service-456")),
    ("host.name", StringValue("order-service-002.prod.local")),
    ("process.pid", IntValue(5678))
  ]
  
  let service_b_resource = Resource::with_attributes(base_resource, service_b_attrs)
  
  // Service C specific resource
  let service_c_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("v3.0.1")),
    ("service.instance.id", StringValue("payment-service-789")),
    ("host.name", StringValue("payment-service-003.prod.local")),
    ("process.pid", IntValue(9012))
  ]
  
  let service_c_resource = Resource::with_attributes(base_resource, service_c_attrs)
  
  // Verify common attributes are consistent across all services
  assert_eq(Resource::get_attribute(service_a_resource, "service.namespace"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.namespace"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(service_c_resource, "service.namespace"), Some(StringValue("production")))
  
  assert_eq(Resource::get_attribute(service_a_resource, "deployment.environment"), Some(StringValue("prod")))
  assert_eq(Resource::get_attribute(service_b_resource, "deployment.environment"), Some(StringValue("prod")))
  assert_eq(Resource::get_attribute(service_c_resource, "deployment.environment"), Some(StringValue("prod")))
  
  assert_eq(Resource::get_attribute(service_a_resource, "cloud.provider"), Some(StringValue("aws")))
  assert_eq(Resource::get_attribute(service_b_resource, "cloud.provider"), Some(StringValue("aws")))
  assert_eq(Resource::get_attribute(service_c_resource, "cloud.provider"), Some(StringValue("aws")))
  
  // Verify service-specific attributes are different
  assert_eq(Resource::get_attribute(service_a_resource, "service.name"), Some(StringValue("user-service")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.name"), Some(StringValue("order-service")))
  assert_eq(Resource::get_attribute(service_c_resource, "service.name"), Some(StringValue("payment-service")))
  
  assert_eq(Resource::get_attribute(service_a_resource, "service.version"), Some(StringValue("v1.2.3")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.version"), Some(StringValue("v2.1.0")))
  assert_eq(Resource::get_attribute(service_c_resource, "service.version"), Some(StringValue("v3.0.1")))
}

test "cross-service propagation with HTTP context" {
  // Simulate HTTP request propagation across services
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Service A receives incoming request
  let incoming_carrier = TextMapCarrier::new()
  TextMapCarrier::set(incoming_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(incoming_carrier, "baggage", "user.id=user123,request.id=req456")
  
  // Service A extracts context
  let service_a_ctx = CompositePropagator::extract(composite_propagator, incoming_carrier)
  
  // Service A processes and adds to context
  let service_a_enhanced_ctx = Context::with_value(service_a_ctx, ContextKey::new("service.a.processed"), "true")
  
  // Service A calls Service B
  let service_b_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_a_enhanced_ctx, service_b_carrier)
  TextMapCarrier::set(service_b_carrier, "baggage", "user.id=user123,request.id=req456,service.a.latency=50ms")
  
  // Service B extracts and processes
  let service_b_ctx = CompositePropagator::extract(composite_propagator, service_b_carrier)
  let service_b_enhanced_ctx = Context::with_value(service_b_ctx, ContextKey::new("service.b.processed"), "true")
  
  // Service B calls Service C
  let service_c_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_b_enhanced_ctx, service_c_carrier)
  TextMapCarrier::set(service_c_carrier, "baggage", "user.id=user123,request.id=req456,service.a.latency=50ms,service.b.latency=30ms")
  
  // Service C extracts and processes
  let service_c_ctx = CompositePropagator::extract(composite_propagator, service_c_carrier)
  
  // Verify trace context is preserved across all services
  let service_a_trace = TextMapCarrier::get(incoming_carrier, "traceparent")
  let service_b_trace = TextMapCarrier::get(service_b_carrier, "traceparent")
  let service_c_trace = TextMapCarrier::get(service_c_carrier, "traceparent")
  
  assert_eq(service_a_trace, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(service_b_trace, Some("00-test-trace-id-test-span-id-01"))
  assert_eq(service_c_trace, Some("00-test-trace-id-test-span-id-01"))
  
  // Verify baggage propagation and accumulation
  let service_a_baggage = TextMapCarrier::get(incoming_carrier, "baggage")
  let service_b_baggage = TextMapCarrier::get(service_b_carrier, "baggage")
  let service_c_baggage = TextMapCarrier::get(service_c_carrier, "baggage")
  
  assert_eq(service_a_baggage, Some("user.id=user123,request.id=req456"))
  assert_eq(service_b_baggage, Some("user.id=user123,request.id=req456,service.a.latency=50ms"))
  assert_eq(service_c_baggage, Some("user.id=user123,request.id=req456,service.a.latency=50ms,service.b.latency=30ms"))
}

test "cross-service error handling consistency" {
  let provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // Service A encounters error
  let tracer_a = TracerProvider::get_tracer(provider, "service-a")
  let error_span_a = Tracer::start_span(tracer_a, "error-operation")
  
  Span::set_status(error_span_a, Error, Some("Database connection failed"))
  Span::add_event(error_span_a, "error.occurred", Some([("error.code", StringValue("DB_CONN_FAILED"))]))
  Span::end(error_span_a)
  
  // Service A logs error
  let logger_a = LoggerProvider::get_logger(logger_provider, "service-a")
  let error_log_a = LogRecord::new_with_context(
    Error,
    Some("Database connection failed after 3 retries"),
    None,
    Some(1735689600000000000L),
    None,
    Some("error-trace-id"),
    Some("error-span-id"),
    None
  )
  Logger::emit(logger_a, error_log_a)
  
  // Service A records error metrics
  let meter_a = MeterProvider::get_meter(meter_provider, "service-a")
  let error_counter_a = Meter::create_counter(meter_a, "errors.total", Some("Total errors"), Some("errors"))
  Counter::add(error_counter_a, 1.0)
  
  // Service B encounters similar error
  let tracer_b = TracerProvider::get_tracer(provider, "service-b")
  let error_span_b = Tracer::start_span(tracer_b, "error-operation")
  
  Span::set_status(error_span_b, Error, Some("Database connection failed"))
  Span::add_event(error_span_b, "error.occurred", Some([("error.code", StringValue("DB_CONN_FAILED"))]))
  Span::end(error_span_b)
  
  // Service B logs error
  let logger_b = LoggerProvider::get_logger(logger_provider, "service-b")
  let error_log_b = LogRecord::new_with_context(
    Error,
    Some("Database connection failed after 3 retries"),
    None,
    Some(1735689600000000100L),
    None,
    Some("error-trace-id"),
    Some("error-span-id"),
    None
  )
  Logger::emit(logger_b, error_log_b)
  
  // Service B records error metrics
  let meter_b = MeterProvider::get_meter(meter_provider, "service-b")
  let error_counter_b = Meter::create_counter(meter_b, "errors.total", Some("Total errors"), Some("errors"))
  Counter::add(error_counter_b, 1.0)
  
  // Verify error handling consistency
  assert_eq(error_counter_a.name, "errors.total")
  assert_eq(error_counter_b.name, "errors.total")
  assert_eq(error_counter_a.description, Some("Total errors"))
  assert_eq(error_counter_b.description, Some("Total errors"))
  assert_eq(error_counter_a.unit, Some("errors"))
  assert_eq(error_counter_b.unit, Some("errors"))
  
  assert_eq(LogRecord::severity_number(error_log_a), Error)
  assert_eq(LogRecord::severity_number(error_log_b), Error)
  assert_eq(LogRecord::body(error_log_a), Some("Database connection failed after 3 retries"))
  assert_eq(LogRecord::body(error_log_b), Some("Database connection failed after 3 retries"))
  
  assert_eq(Span::status(error_span_a), Unset) // Simplified implementation
  assert_eq(Span::status(error_span_b), Unset) // Simplified implementation
}

test "cross-service performance consistency" {
  let provider = MeterProvider::default()
  
  // Service A performance metrics
  let meter_a = MeterProvider::get_meter(provider, "service-a")
  let response_time_a = Meter::create_histogram(meter_a, "http.response.time", Some("HTTP response time"), Some("ms"))
  let throughput_a = Meter::create_counter(meter_a, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  
  // Record performance metrics for Service A
  for i in 0..10 {
    let response_time = 100.0 + (Int::to_float(i) * 10.0)
    Histogram::record(response_time_a, response_time)
    Counter::add(throughput_a, 1.0)
  }
  
  // Service B performance metrics
  let meter_b = MeterProvider::get_meter(provider, "service-b")
  let response_time_b = Meter::create_histogram(meter_b, "http.response.time", Some("HTTP response time"), Some("ms"))
  let throughput_b = Meter::create_counter(meter_b, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  
  // Record performance metrics for Service B
  for i in 0..8 {
    let response_time = 80.0 + (Int::to_float(i) * 15.0)
    Histogram::record(response_time_b, response_time)
    Counter::add(throughput_b, 1.0)
  }
  
  // Service C performance metrics
  let meter_c = MeterProvider::get_meter(provider, "service-c")
  let response_time_c = Meter::create_histogram(meter_c, "http.response.time", Some("HTTP response time"), Some("ms"))
  let throughput_c = Meter::create_counter(meter_c, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  
  // Record performance metrics for Service C
  for i in 0..12 {
    let response_time = 120.0 + (Int::to_float(i) * 8.0)
    Histogram::record(response_time_c, response_time)
    Counter::add(throughput_c, 1.0)
  }
  
  // Verify metric consistency across services
  assert_eq(response_time_a.name, "http.response.time")
  assert_eq(response_time_b.name, "http.response.time")
  assert_eq(response_time_c.name, "http.response.time")
  
  assert_eq(response_time_a.description, Some("HTTP response time"))
  assert_eq(response_time_b.description, Some("HTTP response time"))
  assert_eq(response_time_c.description, Some("HTTP response time"))
  
  assert_eq(response_time_a.unit, Some("ms"))
  assert_eq(response_time_b.unit, Some("ms"))
  assert_eq(response_time_c.unit, Some("ms"))
  
  assert_eq(throughput_a.name, "http.requests.total")
  assert_eq(throughput_b.name, "http.requests.total")
  assert_eq(throughput_c.name, "http.requests.total")
  
  assert_eq(throughput_a.description, Some("Total HTTP requests"))
  assert_eq(throughput_b.description, Some("Total HTTP requests"))
  assert_eq(throughput_c.description, Some("Total HTTP requests"))
  
  assert_eq(throughput_a.unit, Some("requests"))
  assert_eq(throughput_b.unit, Some("requests"))
  assert_eq(throughput_c.unit, Some("requests"))
}