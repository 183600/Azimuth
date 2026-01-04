// Azimuth Telemetry System - Integrated System Tests
// This file contains comprehensive test cases that integrate multiple components of the telemetry system

// Test 1: End-to-End Telemetry Pipeline
test "end_to_end_telemetry_pipeline" {
  // Initialize telemetry system
  let telemetry_config = TelemetryConfig::new()
    .with_service_name("test_service")
    .with_service_version("1.0.0")
    .with_sampling_rate(1.0) // 100% sampling for testing
    .with_exporter_endpoint("http://localhost:4317")
  
  let telemetry_provider = TelemetryProvider::initialize(telemetry_config)
  assert_true(TelemetryProvider::is_initialized(telemetry_provider))
  
  // Create a tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "test_tracer")
  
  // Create a root span
  let root_span = Tracer::start_span(tracer, "root_operation")
  Span::set_attribute(root_span, "operation.type", "root")
  Span::set_attribute(root_span, "user.id", "12345")
  
  // Create child spans
  let child_span1 = Tracer::start_span_with_parent(tracer, "database_query", root_span)
  Span::set_attribute(child_span1, "db.type", "postgresql")
  Span::set_attribute(child_span1, "db.statement", "SELECT * FROM users")
  Span::add_event(child_span1, "query.start", Some([("timestamp", StringValue("2023-01-01T00:00:00Z"))]))
  Span::end(child_span1)
  
  let child_span2 = Tracer::start_span_with_parent(tracer, "api_call", root_span)
  Span::set_attribute(child_span2, "http.method", "GET")
  Span::set_attribute(child_span2, "http.url", "https://api.example.com/data")
  Span::set_status(child_span2, Ok, None)
  Span::end(child_span2)
  
  // End root span
  Span::end(root_span)
  
  // Verify spans are recorded
  let recorded_spans = TelemetryProvider::get_exported_spans(telemetry_provider)
  assert_eq(recorded_spans.length(), 3)
  
  // Verify span relationships
  let root_span_data = recorded_spans[0]
  assert_eq(SpanData::name(root_span_data), "root_operation")
  assert_eq(SpanData::parent_span_id(root_span_data), None)
  
  let child_span1_data = recorded_spans[1]
  assert_eq(SpanData::name(child_span1_data), "database_query")
  assert_eq(SpanData::parent_span_id(child_span1_data), Some(SpanData::span_id(root_span_data)))
  
  let child_span2_data = recorded_spans[2]
  assert_eq(SpanData::name(child_span2_data), "api_call")
  assert_eq(SpanData::parent_span_id(child_span2_data), Some(SpanData::span_id(root_span_data)))
  
  // Verify attributes
  assert_eq(SpanData::get_attribute(root_span_data, "operation.type"), Some(StringValue("root")))
  assert_eq(SpanData::get_attribute(child_span1_data, "db.type"), Some(StringValue("postgresql")))
  assert_eq(SpanData::get_attribute(child_span2_data, "http.method"), Some(StringValue("GET")))
  
  // Shutdown telemetry provider
  TelemetryProvider::shutdown(telemetry_provider)
}

// Test 2: Metrics Collection and Aggregation
test "metrics_collection_and_aggregation" {
  // Initialize metrics system
  let metrics_config = MetricsConfig::new()
    .with_export_interval(1000) // 1 second
    .with_exporter_endpoint("http://localhost:9090")
  
  let metrics_provider = MetricsProvider::initialize(metrics_config)
  let meter = MetricsProvider::get_meter(metrics_provider, "test_meter")
  
  // Create instruments
  let counter = Meter::create_counter(meter, "http_requests_total", 
    Some("Total number of HTTP requests"), Some("requests"))
  
  let histogram = Meter::create_histogram(meter, "http_request_duration_seconds",
    Some("HTTP request duration in seconds"), Some("seconds"))
  
  let gauge = Meter::create_gauge(meter, "active_connections",
    Some("Number of active connections"), Some("connections"))
  
  // Record metrics
  Counter::add(counter, 1.0, Some(Attributes::from([("method", StringValue("GET")), ("status", StringValue("200"))])))
  Counter::add(counter, 1.0, Some(Attributes::from([("method", StringValue("POST")), ("status", StringValue("201"))])))
  Counter::add(counter, 1.0, Some(Attributes::from([("method", StringValue("GET")), ("status", StringValue("404"))])))
  
  Histogram::record(histogram, 0.1, Some(Attributes::from([("method", StringValue("GET"))])))
  Histogram::record(histogram, 0.2, Some(Attributes::from([("method", StringValue("GET"))])))
  Histogram::record(histogram, 0.5, Some(Attributes::from([("method", StringValue("POST"))])))
  
  Gauge::record(gauge, 10.0)
  Gauge::record(gauge, 15.0) // Should overwrite previous value
  
  // Wait for metrics to be collected
  MetricsProvider::force_flush(metrics_provider)
  
  // Verify metrics are collected
  let collected_metrics = MetricsProvider::get_collected_metrics(metrics_provider)
  assert_true(collected_metrics.length() >= 3) // At least one for each instrument
  
  // Verify counter metrics
  let counter_metrics = MetricsProvider::filter_metrics_by_name(collected_metrics, "http_requests_total")
  assert_true(counter_metrics.length() > 0)
  
  // Verify histogram metrics
  let histogram_metrics = MetricsProvider::filter_metrics_by_name(collected_metrics, "http_request_duration_seconds")
  assert_true(histogram_metrics.length() > 0)
  
  // Verify gauge metrics
  let gauge_metrics = MetricsProvider::filter_metrics_by_name(collected_metrics, "active_connections")
  assert_true(gauge_metrics.length() > 0)
  
  // Verify metric attributes
  let get_counter = MetricsProvider::filter_metrics_by_attributes(counter_metrics, 
    Attributes::from([("method", StringValue("GET")), ("status", StringValue("200"))]))
  assert_true(get_counter.length() > 0)
  
  // Shutdown metrics provider
  MetricsProvider::shutdown(metrics_provider)
}

// Test 3: Logging and Correlation
test "logging_and_correlation" {
  // Initialize logging system
  let logging_config = LoggingConfig::new()
    .with_level(LogLevel::Info)
    .with_format(LogFormat::Json)
    .with_include_trace_context(true)
  
  let logger_provider = LoggingProvider::initialize(logging_config)
  let logger = LoggingProvider::get_logger(logger_provider, "test_logger")
  
  // Initialize telemetry for context propagation
  let telemetry_config = TelemetryConfig::new()
    .with_service_name("test_service")
  let telemetry_provider = TelemetryProvider::initialize(telemetry_config)
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "test_tracer")
  
  // Create a span for context
  let span = Tracer::start_span(tracer, "operation_with_logging")
  Span::set_attribute(span, "operation.id", "12345")
  
  // Get the current context with the span
  let context = Context::with_span(Context::current(), span)
  
  // Log messages with context
  Logger::emit_with_context(logger, LogLevel::Info, "Operation started", Some(context))
  Logger::emit_with_context(logger, LogLevel::Debug, "Processing data", Some(context))
  Logger::emit_with_context(logger, LogLevel::Warn, "Slow operation detected", Some(context))
  Logger::emit_with_context(logger, LogLevel::Info, "Operation completed", Some(context))
  
  // End the span
  Span::end(span)
  
  // Wait for logs to be processed
  LoggingProvider::force_flush(logger_provider)
  
  // Verify logs are recorded
  let recorded_logs = LoggingProvider::get_recorded_logs(logger_provider)
  assert_eq(recorded_logs.length(), 4)
  
  // Verify log correlation with trace context
  for log in recorded_logs {
    assert_true(LogRecord::contains_trace_context(log))
    assert_eq(LogRecord::trace_id(log), Some(Span::trace_id(span)))
    assert_eq(LogRecord::span_id(log), Some(Span::span_id(span)))
  }
  
  // Verify log levels
  assert_eq(LogRecord::level(recorded_logs[0]), LogLevel::Info)
  assert_eq(LogRecord::level(recorded_logs[1]), LogLevel::Debug)
  assert_eq(LogRecord::level(recorded_logs[2]), LogLevel::Warn)
  assert_eq(LogRecord::level(recorded_logs[3]), LogLevel::Info)
  
  // Verify log messages
  assert_eq(LogRecord::message(recorded_logs[0]), "Operation started")
  assert_eq(LogRecord::message(recorded_logs[1]), "Processing data")
  assert_eq(LogRecord::message(recorded_logs[2]), "Slow operation detected")
  assert_eq(LogRecord::message(recorded_logs[3]), "Operation completed")
  
  // Shutdown providers
  LoggingProvider::shutdown(logger_provider)
  TelemetryProvider::shutdown(telemetry_provider)
}

// Test 4: Context Propagation Across Services
test "context_propagation_across_services" {
  // Initialize service A
  let service_a_config = TelemetryConfig::new()
    .with_service_name("service_a")
    .with_propagators([TraceContextPropagator::new(), BaggagePropagator::new()])
  
  let service_a_provider = TelemetryProvider::initialize(service_a_config)
  let service_a_tracer = TelemetryProvider::get_tracer(service_a_provider, "service_a_tracer")
  
  // Create a span in service A
  let service_a_span = Tracer::start_span(service_a_tracer, "service_a_operation")
  Span::set_attribute(service_a_span, "service", "A")
  
  // Add baggage to context
  let context = Context::with_span(Context::current(), service_a_span)
  let context_with_baggage = Baggage::set_value(context, "user.id", "12345")
  let context_with_baggage = Baggage::set_value(context_with_baggage, "request.id", "req-67890")
  
  // Inject context into headers for service B
  let headers = []
  let propagator = CompositePropagator::new([TraceContextPropagator::new(), BaggagePropagator::new()])
  let injected_headers = Propagator::inject(propagator, context_with_baggage, headers)
  
  // Initialize service B
  let service_b_config = TelemetryConfig::new()
    .with_service_name("service_b")
    .with_propagators([TraceContextPropagator::new(), BaggagePropagator::new()])
  
  let service_b_provider = TelemetryProvider::initialize(service_b_config)
  let service_b_tracer = TelemetryProvider::get_tracer(service_b_provider, "service_b_tracer")
  
  // Extract context in service B
  let service_b_context = Propagator::extract(propagator, injected_headers)
  
  // Create a span in service B with extracted context
  let service_b_span = Tracer::start_span_with_context(service_b_tracer, "service_b_operation", service_b_context)
  Span::set_attribute(service_b_span, "service", "B")
  
  // Verify trace context propagation
  assert_eq(Span::trace_id(service_a_span), Span::trace_id(service_b_span))
  assert_ne(Span::span_id(service_a_span), Span::span_id(service_b_span))
  assert_eq(Span::parent_span_id(service_b_span), Some(Span::span_id(service_a_span)))
  
  // Verify baggage propagation
  let user_id = Baggage::get_value(service_b_context, "user.id")
  let request_id = Baggage::get_value(service_b_context, "request.id")
  
  match user_id {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  match request_id {
    Some(value) => assert_eq(value, "req-67890")
    None => assert_true(false)
  }
  
  // End spans
  Span::end(service_a_span)
  Span::end(service_b_span)
  
  // Verify spans are recorded in both services
  let service_a_spans = TelemetryProvider::get_exported_spans(service_a_provider)
  let service_b_spans = TelemetryProvider::get_exported_spans(service_b_provider)
  
  assert_eq(service_a_spans.length(), 1)
  assert_eq(service_b_spans.length(), 1)
  
  // Verify span relationships
  let service_a_span_data = service_a_spans[0]
  let service_b_span_data = service_b_spans[0]
  
  assert_eq(SpanData::name(service_a_span_data), "service_a_operation")
  assert_eq(SpanData::name(service_b_span_data), "service_b_operation")
  assert_eq(SpanData::trace_id(service_a_span_data), SpanData::trace_id(service_b_span_data))
  assert_eq(SpanData::span_id(service_a_span_data), SpanData::parent_span_id(service_b_span_data))
  
  // Shutdown providers
  TelemetryProvider::shutdown(service_a_provider)
  TelemetryProvider::shutdown(service_b_provider)
}

// Test 5: Resource Management and Cleanup
test "resource_management_and_cleanup" {
  // Initialize telemetry with resource configuration
  let resource = Resource::new()
    .with_attribute("service.name", StringValue("test_service"))
    .with_attribute("service.version", StringValue("1.0.0"))
    .with_attribute("deployment.environment", StringValue("test"))
    .with_attribute("host.name", StringValue("test-host"))
  
  let telemetry_config = TelemetryConfig::new()
    .with_resource(resource)
  
  let telemetry_provider = TelemetryProvider::initialize(telemetry_config)
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "test_tracer")
  
  // Verify resource is attached to telemetry provider
  let provider_resource = TelemetryProvider::get_resource(telemetry_provider)
  assert_eq(Resource::get_attribute(provider_resource, "service.name"), Some(StringValue("test_service")))
  assert_eq(Resource::get_attribute(provider_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(provider_resource, "deployment.environment"), Some(StringValue("test")))
  assert_eq(Resource::get_attribute(provider_resource, "host.name"), Some(StringValue("test-host")))
  
  // Create spans and verify resource is attached
  let span = Tracer::start_span(tracer, "resource_test")
  Span::end(span)
  
  let recorded_spans = TelemetryProvider::get_exported_spans(telemetry_provider)
  assert_eq(recorded_spans.length(), 1)
  
  let span_resource = SpanData::resource(recorded_spans[0])
  assert_eq(Resource::get_attribute(span_resource, "service.name"), Some(StringValue("test_service")))
  
  // Test resource cleanup
  TelemetryProvider::cleanup_resources(telemetry_provider)
  
  // Verify cleanup doesn't affect existing spans
  let span2 = Tracer::start_span(tracer, "cleanup_test")
  Span::end(span2)
  
  let recorded_spans_after_cleanup = TelemetryProvider::get_exported_spans(telemetry_provider)
  assert_eq(recorded_spans_after_cleanup.length(), 2)
  
  // Test memory cleanup
  let initial_memory = MemoryUtil::get_used_memory()
  
  // Create many spans to test memory usage
  for i in 0..=1000 {
    let temp_span = Tracer::start_span(tracer, "temp_span_" + i.to_string())
    Span::end(temp_span)
  }
  
  // Force garbage collection
  MemoryUtil::force_gc()
  
  let final_memory = MemoryUtil::get_used_memory()
  let memory_increase = final_memory - initial_memory
  
  // Memory increase should be reasonable (less than 10MB)
  assert_true(memory_increase < 10 * 1024 * 1024)
  
  // Shutdown telemetry provider
  TelemetryProvider::shutdown(telemetry_provider)
  
  // Verify all resources are cleaned up
  assert_false(TelemetryProvider::is_initialized(telemetry_provider))
}

// Test 6: Error Handling and Recovery
test "error_handling_and_recovery" {
  // Initialize telemetry with error handling configuration
  let error_config = TelemetryConfig::new()
    .with_error_handling_enabled(true)
    .with_retry_policy(RetryPolicy::exponential_backoff(3, 100)) // 3 retries, 100ms base delay
    .with_circuit_breaker(CircuitBreakerConfig::new(5, 10000)) // 5 failures, 10s timeout
  
  let telemetry_provider = TelemetryProvider::initialize(error_config)
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "error_test_tracer")
  
  // Test error handling in span operations
  let span = Tracer::start_span(tracer, "error_test_operation")
  
  // Simulate an error
  Span::record_exception(span, "Test exception", Some([("error.code", StringValue("500"))]))
  Span::set_status(span, Error, Some("Something went wrong"))
  Span::end(span)
  
  // Verify error is recorded
  let recorded_spans = TelemetryProvider::get_exported_spans(telemetry_provider)
  assert_eq(recorded_spans.length(), 1)
  
  let span_data = recorded_spans[0]
  assert_eq(SpanData::status(span_data), Error)
  assert_eq(SpanData::status_message(span_data), Some("Something went wrong"))
  
  let events = SpanData::events(span_data)
  assert_true(events.length() > 0)
  
  let exception_event = events[0]
  assert_eq(Event::name(exception_event), "exception")
  assert_eq(Event::get_attribute(exception_event, "exception.message"), Some(StringValue("Test exception")))
  assert_eq(Event::get_attribute(exception_event, "error.code"), Some(StringValue("500")))
  
  // Test retry mechanism with simulated exporter failures
  let failing_exporter = FailingExporter::new(2) // Fail 2 times, then succeed
  TelemetryProvider::set_exporter(telemetry_provider, failing_exporter)
  
  let retry_span = Tracer::start_span(tracer, "retry_test_operation")
  Span::end(retry_span)
  
  // Verify retry count
  let retry_attempts = FailingExporter::get_retry_attempts(failing_exporter)
  assert_eq(retry_attempts, 2)
  
  // Test circuit breaker
  let circuit_breaker_exporter = CircuitBreakerExporter::new(3) // Trip after 3 failures
  TelemetryProvider::set_exporter(telemetry_provider, circuit_breaker_exporter)
  
  // Trigger circuit breaker
  for i in 0..=5 {
    let cb_span = Tracer::start_span(tracer, "circuit_breaker_test_" + i.to_string())
    Span::end(cb_span)
  }
  
  // Verify circuit breaker is tripped
  assert_true(CircuitBreakerExporter::is_tripped(circuit_breaker_exporter))
  
  // Test recovery after timeout
  CircuitBreakerExporter::advance_time(circuit_breaker_exporter, 11000) // Advance past timeout
  
  let recovery_span = Tracer::start_span(tracer, "recovery_test_operation")
  Span::end(recovery_span)
  
  // Verify circuit breaker is reset
  assert_false(CircuitBreakerExporter::is_tripped(circuit_breaker_exporter))
  
  // Shutdown telemetry provider
  TelemetryProvider::shutdown(telemetry_provider)
}

// Test 7: Performance and Scalability
test "performance_and_scalability" {
  // Initialize telemetry with performance configuration
  let perf_config = TelemetryConfig::new()
    .with_batch_size(512)
    .with_max_export_batch_size(512)
    .with_max_queue_size(2048)
    .with_export_timeout_millis(30000)
  
  let telemetry_provider = TelemetryProvider::initialize(perf_config)
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "perf_test_tracer")
  
  // Measure performance with many concurrent spans
  let start_time = TimeUtil::current_time()
  
  // Create 1000 spans concurrently
  let mut spans = []
  for i in 0..=999 {
    let span = Tracer::start_span(tracer, "perf_test_span_" + i.to_string())
    spans.push(span)
  }
  
  // End all spans
  for span in spans {
    Span::end(span)
  }
  
  // Force flush to ensure all spans are exported
  TelemetryProvider::force_flush(telemetry_provider)
  
  let end_time = TimeUtil::current_time()
  let duration = end_time - start_time
  
  // Performance should be reasonable (less than 5 seconds for 1000 spans)
  assert_true(duration < 5000)
  
  // Verify all spans are recorded
  let recorded_spans = TelemetryProvider::get_exported_spans(telemetry_provider)
  assert_eq(recorded_spans.length(), 1000)
  
  // Test memory usage with many spans
  let initial_memory = MemoryUtil::get_used_memory()
  
  // Create many spans without ending them
  let mut active_spans = []
  for i in 0..=5000 {
    let span = Tracer::start_span(tracer, "active_span_" + i.to_string())
    active_spans.push(span)
  }
  
  let peak_memory = MemoryUtil::get_used_memory()
  let memory_increase = peak_memory - initial_memory
  
  // End all active spans
  for span in active_spans {
    Span::end(span)
  }
  
  // Force garbage collection
  MemoryUtil::force_gc()
  
  let final_memory = MemoryUtil::get_used_memory()
  let memory_after_cleanup = final_memory - initial_memory
  
  // Memory usage should be reasonable and should be cleaned up properly
  assert_true(memory_increase < 50 * 1024 * 1024) // Less than 50MB increase
  assert_true(memory_after_cleanup < 5 * 1024 * 1024) // Less than 5MB after cleanup
  
  // Test throughput with high-frequency operations
  let throughput_start = TimeUtil::current_time()
  let span_count = 10000
  
  for i in 0..=span_count - 1 {
    let span = Tracer::start_span(tracer, "throughput_span_" + i.to_string())
    Span::set_attribute(span, "index", IntValue(i))
    Span::end(span)
  }
  
  TelemetryProvider::force_flush(telemetry_provider)
  
  let throughput_end = TimeUtil::current_time()
  let throughput_duration = throughput_end - throughput_start
  let throughput_spans_per_second = (span_count * 1000) / throughput_duration
  
  // Should handle at least 1000 spans per second
  assert_true(throughput_spans_per_second >= 1000)
  
  // Shutdown telemetry provider
  TelemetryProvider::shutdown(telemetry_provider)
}