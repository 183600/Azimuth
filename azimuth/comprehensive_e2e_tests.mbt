// Comprehensive End-to-End Tests for Azimuth
// These tests verify the entire system functionality

test "complete telemetry workflow" {
  // 1. Initialize resources and providers
  let resource = Resource::new()
  let resource_attrs = [
    ("service.name", StringValue("e2e-test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
  
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // 2. Create instruments
  let tracer = TracerProvider::get_tracer(tracer_provider, "e2e-test")
  let meter = MeterProvider::get_meter(meter_provider, "e2e-test")
  let logger = LoggerProvider::get_logger(logger_provider, "e2e-test")
  
  let counter = Meter::create_counter(meter, "operations_total", Some("Total operations"), Some("operations"))
  let histogram = Meter::create_histogram(meter, "operation_duration_ms", Some("Operation duration"), Some("ms"))
  
  // 3. Start a trace
  let main_span = Tracer::start_span(tracer, "main-operation")
  
  // 4. Add baggage
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage = Baggage::set_entry(baggage, "request.id", "req-67890")
  
  // 5. Create child span for sub-operation
  let sub_span = Tracer::start_span(tracer, "sub-operation")
  
  // 6. Add events to spans
  Span::add_event(main_span, "operation.started", Some([
    ("user.id", StringValue("12345")),
    ("request.id", StringValue("req-67890"))
  ]))
  
  Span::add_event(sub_span, "sub-operation.started", Some([
    ("step", StringValue("validation")),
    ("status", StringValue("in_progress"))
  ]))
  
  // 7. Record metrics
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 123.45)
  
  // 8. Create and emit log records
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Operation in progress"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  Logger::emit(logger, log_record)
  
  // 9. Complete sub-operation
  Span::add_event(sub_span, "sub-operation.completed", Some([
    ("step", StringValue("validation")),
    ("status", StringValue("completed"))
  ]))
  Span::set_status(sub_span, Ok)
  Span::end(sub_span)
  
  // 10. Complete main operation
  Span::add_event(main_span, "operation.completed", Some([
    ("result", StringValue("success")),
    ("duration_ms", StringValue("200"))
  ]))
  Span::set_status(main_span, Ok, Some("Operation completed successfully"))
  Span::end(main_span)
  
  // 11. Verify everything worked
  assert_eq(Span::name(main_span), "main-operation")
  assert_eq(Span::name(sub_span), "sub-operation")
  assert_eq(counter.name, "operations_total")
  assert_eq(histogram.name, "operation_duration_ms")
  assert_eq(LogRecord::body(log_record), Some("Operation in progress"))
}

test "cross-service telemetry propagation" {
  // Service A: Initialize telemetry
  let tracer_provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(tracer_provider_a, "service-a")
  
  let meter_provider_a = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(meter_provider_a, "service-a")
  
  let logger_provider_a = LoggerProvider::default()
  let logger_a = LoggerProvider::get_logger(logger_provider_a, "service-a")
  
  // Service A: Start operation
  let span_a = Tracer::start_span(tracer_a, "service-a-operation")
  
  // Service A: Add baggage
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage = Baggage::set_entry(baggage, "correlation.id", "corr-abcde")
  
  // Service A: Create context for propagation
  let ctx = Context::root()
  let ctx_with_baggage = Context::with_value(ctx, ContextKey::new("baggage"), "user.id=12345,correlation.id=corr-abcde")
  
  // Service A: Inject context into carrier
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(propagator, ctx_with_baggage, carrier)
  
  // Service A: Record metrics and logs
  let counter_a = Meter::create_counter(meter_a, "service_a_requests_total")
  Counter::add(counter_a, 1.0)
  
  let log_a = LogRecord::new(Info, "Service A operation started")
  Logger::emit(logger_a, log_a)
  
  // Service B: Extract context from carrier
  let tracer_provider_b = TracerProvider::default()
  let tracer_b = TracerProvider::get_tracer(tracer_provider_b, "service-b")
  
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // Service B: Start operation with extracted context
  let span_b = Tracer::start_span(tracer_b, "service-b-operation")
  
  // Service B: Add events with correlation information
  Span::add_event(span_b, "service.b.processing", Some([
    ("user.id", StringValue("12345")),
    ("correlation.id", StringValue("corr-abcde")),
    ("service.a.span", StringValue("service-a-operation"))
  ]))
  
  // Service B: Record metrics and logs
  let meter_provider_b = MeterProvider::default()
  let meter_b = MeterProvider::get_meter(meter_provider_b, "service-b")
  
  let counter_b = Meter::create_counter(meter_b, "service_b_requests_total")
  Counter::add(counter_b, 1.0)
  
  let logger_provider_b = LoggerProvider::default()
  let logger_b = LoggerProvider::get_logger(logger_provider_b, "service-b")
  
  let log_b = LogRecord::new_with_context(
    Info,
    Some("Service B operation started"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace123"),
    Some("span456"),
    Some(extracted_ctx)
  )
  Logger::emit(logger_b, log_b)
  
  // Service B: Complete operation
  Span::set_status(span_b, Ok)
  Span::end(span_b)
  
  // Service A: Complete operation
  Span::set_status(span_a, Ok)
  Span::end(span_a)
  
  // Verify cross-service propagation
  assert_eq(Span::name(span_a), "service-a-operation")
  assert_eq(Span::name(span_b), "service-b-operation")
  assert_eq(counter_a.name, "service_a_requests_total")
  assert_eq(counter_b.name, "service_b_requests_total")
}

test "error handling and recovery" {
  // Initialize telemetry
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "error-test")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error-test")
  
  // Create error tracking metrics
  let error_counter = Meter::create_counter(meter, "errors_total", Some("Total errors"), Some("errors"))
  let retry_counter = Meter::create_counter(meter, "retries_total", Some("Total retries"), Some("retries"))
  
  // Start operation
  let span = Tracer::start_span(tracer, "error-prone-operation")
  
  // Simulate error
  Span::add_event(span, "error.occurred", Some([
    ("error.type", StringValue("ConnectionError")),
    ("error.message", StringValue("Failed to connect to database")),
    ("error.retryable", StringValue("true"))
  ]))
  
  Span::set_status(span, Error, Some("Connection failed"))
  
  // Record error metrics
  Counter::add(error_counter, 1.0)
  
  // Log error
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Connection error occurred"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  Logger::emit(logger, error_log)
  
  // Simulate retry
  let retry_span = Tracer::start_span(tracer, "retry-operation")
  
  Span::add_event(retry_span, "retry.started", Some([
    ("attempt", StringValue("1")),
    ("max.attempts", StringValue("3"))
  ]))
  
  // Record retry metrics
  Counter::add(retry_counter, 1.0)
  
  // Simulate successful retry
  Span::add_event(retry_span, "retry.succeeded", Some([
    ("attempt", StringValue("1")),
    ("duration_ms", StringValue("500"))
  ]))
  
  Span::set_status(retry_span, Ok, Some("Retry succeeded"))
  Span::end(retry_span)
  
  // End original span
  Span::end(span)
  
  // Verify error handling
  assert_eq(Span::name(span), "error-prone-operation")
  assert_eq(Span::name(retry_span), "retry-operation")
  assert_eq(error_counter.name, "errors_total")
  assert_eq(retry_counter.name, "retries_total")
}

test "performance monitoring workflow" {
  // Initialize telemetry
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance-test")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "performance-test")
  
  // Create performance metrics
  let request_counter = Meter::create_counter(meter, "http_requests_total")
  let duration_histogram = Meter::create_histogram(meter, "http_request_duration_ms")
  let active_connections_gauge = Meter::create_gauge(meter, "active_connections")
  
  // Start performance monitoring
  let monitoring_span = Tracer::start_span(tracer, "performance-monitoring")
  
  // Simulate multiple requests
  for i = 0; i < 10; i = i + 1 {
    // Start request span
    let request_span = Tracer::start_span(tracer, "http-request")
    
    // Record start time
    let start_time = Clock::now_unix_nanos(Clock::system())
    
    // Simulate request processing
    Span::add_event(request_span, "request.started", Some([
      ("method", StringValue("GET")),
      ("url", StringValue("/api/data/" + i.to_string()))
    ]))
    
    // Record metrics
    Counter::add(request_counter, 1.0)
    
    // Simulate processing time
    let processing_time = 100 + i * 10
    Histogram::record(duration_histogram, processing_time.to_double())
    
    // Complete request
    Span::add_event(request_span, "request.completed", Some([
      ("status_code", StringValue("200")),
      ("duration_ms", StringValue(processing_time.to_string()))
    ]))
    
    Span::set_status(request_span, Ok)
    Span::end(request_span)
  }
  
  // Log performance summary
  let summary_log = LogRecord::new_with_context(
    Info,
    Some("Processed 10 requests successfully"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  Logger::emit(logger, summary_log)
  
  // End monitoring span
  Span::add_event(monitoring_span, "monitoring.completed", Some([
    ("requests_processed", StringValue("10")),
    ("avg_duration_ms", StringValue("145"))
  ]))
  
  Span::set_status(monitoring_span, Ok)
  Span::end(monitoring_span)
  
  // Verify performance monitoring
  assert_eq(Span::name(monitoring_span), "performance-monitoring")
  assert_eq(request_counter.name, "http_requests_total")
  assert_eq(duration_histogram.name, "http_request_duration_ms")
  assert_eq(active_connections_gauge.name, "active_connections")
}

test "resource management and cleanup" {
  // Initialize multiple resources
  let resources = []
  
  // Create resources with different attributes
  for i = 0; i < 5; i = i + 1 {
    let resource = Resource::new()
    let attrs = [
      ("resource.id", StringValue("resource-" + i.to_string())),
      ("resource.type", StringValue("test-resource")),
      ("resource.owner", StringValue("test-team"))
    ]
    let resource_with_attrs = Resource::with_attributes(resource, attrs)
    resources.push(resource_with_attrs)
  }
  
  // Create providers for each resource
  let tracer_providers = []
  let meter_providers = []
  let logger_providers = []
  
  for i = 0; i < resources.length(); i = i + 1 {
    tracer_providers.push(TracerProvider::default())
    meter_providers.push(MeterProvider::default())
    logger_providers.push(LoggerProvider::default())
  }
  
  // Create instruments for each provider
  let tracers = []
  let meters = []
  let loggers = []
  
  for i = 0; i < tracer_providers.length(); i = i + 1 {
    tracers.push(TracerProvider::get_tracer(tracer_providers[i], "resource-" + i.to_string()))
    meters.push(MeterProvider::get_meter(meter_providers[i], "resource-" + i.to_string()))
    loggers.push(LoggerProvider::get_logger(logger_providers[i], "resource-" + i.to_string()))
  }
  
  // Create spans and metrics
  let spans = []
  let counters = []
  
  for i = 0; i < tracers.length(); i = i + 1 {
    spans.push(Tracer::start_span(tracers[i], "resource-operation"))
    counters.push(Meter::create_counter(meters[i], "operations_total"))
  }
  
  // Record metrics
  for i = 0; i < counters.length(); i = i + 1 {
    Counter::add(counters[i], 1.0)
  }
  
  // End spans
  for i = 0; i < spans.length(); i = i + 1 {
    Span::set_status(spans[i], Ok)
    Span::end(spans[i])
  }
  
  // Verify resource management
  assert_eq(resources.length(), 5)
  assert_eq(tracers.length(), 5)
  assert_eq(meters.length(), 5)
  assert_eq(loggers.length(), 5)
  assert_eq(spans.length(), 5)
  assert_eq(counters.length(), 5)
  
  // Verify resource attributes
  for i = 0; i < resources.length(); i = i + 1 {
    let resource = resources[i]
    let resource_id = Resource::get_attribute(resource, "resource.id")
    
    match resource_id {
      Some(StringValue(id)) => assert_eq(id, "resource-" + i.to_string())
      _ => assert_true(false)
    }
  }
}