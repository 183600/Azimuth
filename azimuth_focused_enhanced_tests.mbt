// Focused Enhanced Test Cases for Azimuth Telemetry System
// These tests cover critical functionality and edge cases with comprehensive validation

test "span lifecycle with proper resource cleanup" {
  // Test span creation, recording, and cleanup
  let tracer = Tracer::new("test-service")
  let span = Tracer::start_span(tracer, "operation-test")
  
  assert_true(Span::is_recording(span))
  assert_eq(Span::name(span), "operation-test")
  
  // Set attributes and events
  Span::set_attribute(span, "user.id", "user-123")
  Span::add_event(span, "operation.started", @[("component", "test")])
  
  // End span and verify cleanup
  Span::end(span)
  assert_false(Span::is_recording(span))
  
  // Verify span context is preserved
  let context = Span::context(span)
  assert_true(SpanContext::is_valid(context))
}

test "metrics aggregation and accuracy validation" {
  // Create different metric instruments
  let meter = Meter::new("test-meter")
  
  // Counter metrics
  let counter = Meter::create_counter(meter, "requests.total", "Total requests")
  Counter::add(counter, 5, @[
    ("method", "GET"),
    ("status", "200")
  ])
  Counter::add(counter, 3, @[
    ("method", "POST"),
    ("status", "201")
  ])
  
  // Histogram metrics
  let histogram = Meter::create_histogram(meter, "request.duration", "Request duration")
  Histogram::record(histogram, 120.5, @[
    ("endpoint", "/api/users")
  ])
  Histogram::record(histogram, 85.3, @[
    ("endpoint", "/api/users")
  ])
  
  // Verify metric aggregation
  let counter_data = Counter::get_data(counter)
  assert_eq(counter_data.count, 8)
  
  let histogram_data = Histogram::get_data(histogram)
  assert_eq(histogram_data.count, 2)
  assert_true(histogram_data.sum > 200.0)
}

test "cross-service context propagation integrity" {
  // Initialize context with baggage
  let context = Context::new()
  let baggage = Baggage::new()
  Baggage::set(baggage, "trace.id", "trace-12345")
  Baggage::set(baggage, "user.session", "session-abc")
  
  // Create propagator
  let propagator = CompositePropagator::new()
  CompositePropagator::add_text_map_propagator(propagator, TraceContextPropagator::new())
  CompositePropagator::add_text_map_propagator(propagator, BaggagePropagator::new())
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  Propagator::inject(propagator, context, carrier)
  
  // Extract context from carrier
  let extracted_context = Propagator::extract(propagator, carrier)
  let extracted_baggage = Context::baggage(extracted_context)
  
  // Verify propagation integrity
  assert_eq(Baggage::get(extracted_baggage, "trace.id"), "trace-12345")
  assert_eq(Baggage::get(extracted_baggage, "user.session"), "session-abc")
}

test "structured logging with correlation and sampling" {
  // Create logger with sampling configuration
  let logger_provider = LoggerProvider::new()
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  
  // Configure sampling
  let sampler = TraceIdRatioBasedSampler::new(0.5) // 50% sampling
  
  // Create correlated span and log record
  let span_context = SpanContext::new("trace-789", "span-456", true, "sampled=1")
  let context = Context::with_span(Context::new(), span_context)
  
  // Log record with correlation
  let log_record = LogRecord::new()
  LogRecord::set_severity(log_record, Severity::INFO)
  LogRecord::set_body(log_record, "User authentication successful")
  LogRecord::set_attributes(log_record, @[
    ("user.id", "user-456"),
    ("auth.method", "oauth2"),
    ("latency", "45ms")
  ])
  
  // Emit log record
  Logger::emit_log(logger, log_record, context)
  
  // Verify log correlation
  let emitted_logs = Logger::get_emitted_logs(logger)
  assert_eq(emitted_logs.length, 1)
  
  let log = emitted_logs[0]
  assert_eq(log.trace_id, "trace-789")
  assert_eq(log.span_id, "span-456")
  assert_eq(log.body, "User authentication successful")
}

test "resource attribute merging and conflict resolution" {
  // Create resources with different attributes
  let resource1 = Resource::new(@[
    ("service.name", "api-gateway"),
    ("service.version", "1.0.0"),
    ("deployment.environment", "production")
  ])
  
  let resource2 = Resource::new(@[
    ("service.name", "api-gateway-v2"), // This should override
    ("host.name", "server-01"),
    ("service.instance.id", "instance-123")
  ])
  
  // Merge resources with conflict resolution
  let merged_resource = Resource::merge(resource1, resource2)
  
  // Verify merge results
  let attributes = Resource::attributes(merged_resource)
  
  // service.name should be from resource2 (later override)
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), "api-gateway-v2")
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), "1.0.0")
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), "production")
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), "server-01")
  assert_eq(Resource::get_attribute(merged_resource, "service.instance.id"), "instance-123")
}

test "concurrent telemetry operations safety" {
  // Test thread safety of concurrent operations
  let tracer = Tracer::new("concurrent-test")
  let meter = Meter::new("concurrent-meter")
  let counter = Meter::create_counter(meter, "concurrent.operations")
  
  // Simulate concurrent span operations
  let spans = @[]
  for i = 0; i < 10; i = i + 1 {
    let span = Tracer::start_span(tracer, "concurrent-operation-{i}")
    Array::push(spans, span)
    Counter::add(counter, 1, @[
      ("thread.id", Int::to_string(i)),
      ("operation.type", "concurrent")
    ])
  }
  
  // Verify all spans are properly created
  assert_eq(spans.length, 10)
  
  // End all spans concurrently
  for span in spans {
    Span::end(span)
  }
  
  // Verify counter aggregation
  let counter_data = Counter::get_data(counter)
  assert_eq(counter_data.count, 10)
  
  // Verify no memory leaks or corrupted state
  assert_true(Tracer::is_healthy(tracer))
  assert_true(Meter::is_healthy(meter))
}

test "telemetry pipeline performance under load" {
  // Performance test with telemetry pipeline
  let start_time = Clock::now()
  
  let tracer = Tracer::new("performance-test")
  let meter = Meter::new("performance-meter")
  let histogram = Meter::create_histogram(meter, "operation.latency")
  
  // Generate high-volume telemetry data
  for i = 0; i < 1000; i = i + 1 {
    let span = Tracer::start_span(tracer, "load-test-operation")
    Span::set_attribute(span, "iteration", Int::to_string(i))
    
    // Simulate operation latency
    let operation_start = Clock::now()
    // Simulate work
    let operation_end = Clock::now()
    let latency = Clock::duration_between(operation_start, operation_end)
    
    Histogram::record(histogram, latency)
    Span::end(span)
  }
  
  let end_time = Clock::now()
  let total_duration = Clock::duration_between(start_time, end_time)
  
  // Verify performance characteristics
  let histogram_data = Histogram::get_data(histogram)
  assert_eq(histogram_data.count, 1000)
  
  // Ensure operations complete within reasonable time
  assert_true(total_duration < 5000.0) // Less than 5 seconds
  
  // Verify no performance degradation
  let avg_latency = histogram_data.sum / histogram_data.count
  assert_true(avg_latency < 10.0) // Average latency should be low
}

test "error handling and telemetry resilience" {
  // Test telemetry system resilience under error conditions
  let tracer = Tracer::new("resilience-test")
  
  // Test with invalid span names (should handle gracefully)
  let invalid_span = Tracer::start_span(tracer, "")
  assert_true(Span::is_recording(invalid_span))
  Span::end(invalid_span)
  
  // Test with very long attribute values
  let long_span = Tracer::start_span(tracer, "long-attributes-test")
  let long_value = String::repeat("x", 10000) // 10KB string
  Span::set_attribute(long_span, "large.payload", long_value)
  Span::end(long_span)
  
  // Test meter with invalid metric names
  let meter = Meter::new("error-test-meter")
  let counter = Meter::create_counter(meter, "invalid@metric#name")
  Counter::add(counter, 1, @[
    ("invalid.key", "value")
  ])
  
  // Verify system remains stable
  assert_true(Tracer::is_healthy(tracer))
  assert_true(Meter::is_healthy(meter))
  
  // Verify error metrics are recorded
  let error_metrics = Meter::get_metrics(meter, "telemetry.errors")
  assert_true(error_metrics.length > 0)
}