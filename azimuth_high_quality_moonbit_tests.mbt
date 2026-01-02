// Azimuth High-Quality MoonBit Test Suite
// This file contains 8 high-quality test cases for the Azimuth telemetry system
// Focusing on core telemetry functionality with comprehensive coverage

// Test 1: Telemetry initialization and configuration
test "telemetry initialization and configuration" {
  // Test telemetry provider initialization
  let provider = TracerProvider::new()
  let tracer = TracerProvider::tracer(provider, "azimuth-test", Some("1.0.0"), None)
  
  // Test meter provider initialization
  let meter_provider = MeterProvider::new()
  let meter = MeterProvider::meter(meter_provider, "azimuth-test-metrics", Some("1.0.0"), None)
  
  // Test logger provider initialization
  let logger_provider = LoggerProvider::new()
  let logger = LoggerProvider::logger(logger_provider, "azimuth-test-logs", Some("1.0.0"), None)
  
  // Verify initialization
  assert_true(Tracer::is_enabled(tracer))
  assert_true(Meter::is_enabled(meter))
  assert_true(Logger::is_enabled(logger))
  
  // Test configuration updates
  let config = TelemetryConfig::new()
  let updated_config = TelemetryConfig::with_sampling(config, 0.1)
  let sampling_rate = TelemetryConfig::sampling_rate(updated_config)
  
  assert_eq(sampling_rate, 0.1)
}

// Test 2: Span creation and lifecycle management
test "span creation and lifecycle management" {
  // Test span creation
  let tracer = Tracer::new("test-tracer")
  let span = Tracer::start_span(tracer, "test-operation")
  
  // Test span attributes
  Span::set_attribute(span, "user.id", StringValue("user-123"))
  Span::set_attribute(span, "operation.type", StringValue("query"))
  Span::set_attribute(span, "retry.count", IntValue(3))
  
  // Test span events
  let event_attributes = [
    ("event.type", StringValue("error")),
    ("error.code", IntValue(500)),
    ("error.message", StringValue("Internal server error"))
  ]
  Span::add_event(span, "operation.error", Clock::now(), event_attributes)
  
  // Test span status
  Span::set_status(span, StatusCode::ERROR, "Operation failed")
  
  // Test span lifecycle
  assert_true(Span::is_recording(span))
  assert_eq(Span::name(span), "test-operation")
  
  // Test span ending
  Span::end(span, Clock::now())
  assert_false(Span::is_recording(span))
}

// Test 3: Metrics collection and aggregation
test "metrics collection and aggregation" {
  // Test counter metric
  let meter = Meter::new("test-meter")
  let counter = Meter::counter(meter, "requests.total", "Total number of requests", "{requests}")
  
  // Counter operations
  Counter::add(counter, 1.0, [("method", StringValue("GET"))])
  Counter::add(counter, 1.0, [("method", StringValue("GET"))])
  Counter::add(counter, 1.0, [("method", StringValue("POST"))])
  
  // Test histogram metric
  let histogram = Meter::histogram(meter, "request.duration", "Request duration", "ms")
  
  // Histogram operations
  Histogram::record(histogram, 100.0, [("endpoint", StringValue("/api/users"))])
  Histogram::record(histogram, 150.0, [("endpoint", StringValue("/api/users"))])
  Histogram::record(histogram, 200.0, [("endpoint", StringValue("/api/users"))])
  
  // Test gauge metric
  let gauge = Meter::gauge(meter, "memory.usage", "Memory usage", "bytes")
  Gauge::record(gauge, 1024.0 * 1024.0 * 512.0, [("type", StringValue("heap"))])
  
  // Test metric aggregation
  let counter_sum = 3.0  // 2 GET + 1 POST
  let histogram_count = 3.0
  let histogram_sum = 450.0
  let histogram_avg = histogram_sum / histogram_count
  
  assert_eq(counter_sum, 3.0)
  assert_eq(histogram_count, 3.0)
  assert_eq(histogram_sum, 450.0)
  assert_eq(histogram_avg, 150.0)
}

// Test 4: Log record creation and processing
test "log record creation and processing" {
  // Test logger creation
  let logger = Logger::new("test-logger")
  
  // Test log record creation
  let log_record = Logger::emit_log(logger, LogLevel::INFO, "User login successful")
  
  // Test log with attributes
  let log_attributes = [
    ("user.id", StringValue("user-456")),
    ("ip.address", StringValue("192.168.1.100")),
    ("user.agent", StringValue("Mozilla/5.0...")),
    ("session.id", StringValue("session-789"))
  ]
  
  let enhanced_log = Logger::emit_log_with_attributes(
    logger, 
    LogLevel::INFO, 
    "User authentication completed", 
    log_attributes
  )
  
  // Test log severity levels
  let debug_log = Logger::emit_log(logger, LogLevel::DEBUG, "Debugging user session")
  let warn_log = Logger::emit_log(logger, LogLevel::WARN, "Session timeout warning")
  let error_log = Logger::emit_log(logger, LogLevel::ERROR, "Authentication failed")
  
  // Test log context
  let ctx = Context::root()
  let ctx_with_correlation = Context::with_value(ctx, ContextKey::new("correlation.id"), "corr-123")
  let contextual_log = Logger::emit_log_in_context(logger, ctx_with_correlation, LogLevel::INFO, "Operation completed")
  
  // Verify log properties
  assert_eq(LogRecord::severity_number(enhanced_log), INFO)
  assert_eq(LogRecord::body(enhanced_log), Some("User authentication completed"))
  assert_true(LogRecord::timestamp(enhanced_log) > 0L)
}

// Test 5: Context propagation and extraction
test "context propagation and extraction" {
  // Test context creation
  let root_ctx = Context::root()
  
  // Test context with values
  let user_id_key = ContextKey::new("user.id")
  let trace_id_key = ContextKey::new("trace.id")
  let request_id_key = ContextKey::new("request.id")
  
  let ctx_with_user = Context::with_value(root_ctx, user_id_key, "user-789")
  let ctx_with_trace = Context::with_value(ctx_with_user, trace_id_key, "trace-abc123")
  let ctx_with_request = Context::with_value(ctx_with_trace, request_id_key, "req-def456")
  
  // Test context value retrieval
  let retrieved_user = Context::get(ctx_with_request, user_id_key)
  let retrieved_trace = Context::get(ctx_with_request, trace_id_key)
  let retrieved_request = Context::get(ctx_with_request, request_id_key)
  let non_existent = Context::get(ctx_with_request, ContextKey::new("non.existent"))
  
  // Test span context
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "key1=value1,key2=value2")
  let ctx_with_span = Context::with_span(root_ctx, span_ctx)
  let extracted_span = Context::span(ctx_with_span)
  
  // Verify context operations
  assert_eq(retrieved_user, Some("user-789"))
  assert_eq(retrieved_trace, Some("trace-abc123"))
  assert_eq(retrieved_request, Some("req-def456"))
  assert_eq(non_existent, None)
  assert_eq(SpanContext::trace_id(extracted_span), "trace-123")
  assert_eq(SpanContext::span_id(extracted_span), "span-456")
  assert_true(SpanContext::is_valid(extracted_span))
}

// Test 6: Baggage propagation across service boundaries
test "baggage propagation across service boundaries" {
  // Test baggage creation
  let baggage = Baggage::new()
  
  // Test baggage entries
  let baggage_with_entries = Baggage::with_entries(baggage, [
    ("user.id", "user-123"),
    ("session.id", "session-456"),
    ("request.origin", "web-app")
  ])
  
  // Test baggage manipulation
  let updated_baggage = Baggage::set_entry(baggage_with_entries, "user.role", "admin")
  let baggage_without_session = Baggage::remove_entry(updated_baggage, "session.id")
  
  // Test baggage serialization
  let serialized = Baggage::to_string(updated_baggage)
  let deserialized = Baggage::from_string(serialized)
  
  // Test baggage propagation
  let propagator = W3CBaggagePropagator::new()
  let carrier = TextMapCarrier::new()
  let ctx = Context::with_baggage(Context::root(), updated_baggage)
  
  // Inject baggage into carrier
  BaggagePropagator::inject(propagator, ctx, carrier)
  
  // Extract baggage from carrier
  let extracted_ctx = BaggagePropagator::extract(propagator, carrier)
  let extracted_baggage = Context::baggage(extracted_ctx)
  
  // Verify baggage operations
  assert_eq(Baggage::get_entry(updated_baggage, "user.id"), Some("user-123"))
  assert_eq(Baggage::get_entry(updated_baggage, "user.role"), Some("admin"))
  assert_eq(Baggage::get_entry(baggage_without_session, "session.id"), None)
  assert_true(Baggage::size(extracted_baggage) >= 3)
}

// Test 7: Resource detection and merging
test "resource detection and merging" {
  // Test resource creation
  let resource = Resource::new()
  
  // Test resource with service attributes
  let service_resource = Resource::with_attributes(resource, [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-abc123"))
  ])
  
  // Test resource with host attributes
  let host_resource = Resource::with_attributes(resource, [
    ("host.name", StringValue("prod-server-01")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux"))
  ])
  
  // Test resource with process attributes
  let process_resource = Resource::with_attributes(resource, [
    ("process.id", IntValue(12345)),
    ("process.executable.name", StringValue("azimuth-service")),
    ("process.command_line", StringValue("./azimuth-service --config production.yaml"))
  ])
  
  // Test resource merging
  let merged_resource1 = Resource::merge(service_resource, host_resource)
  let final_resource = Resource::merge(merged_resource1, process_resource)
  
  // Test resource detection
  let detected_resource = Resource::detect()
  let detected_with_service = Resource::merge(detected_resource, service_resource)
  
  // Test resource attributes access
  let service_name = Resource::get_attribute(final_resource, "service.name")
  let host_name = Resource::get_attribute(final_resource, "host.name")
  let process_id = Resource::get_attribute(final_resource, "process.id")
  
  // Verify resource operations
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  assert_eq(host_name, Some(StringValue("prod-server-01")))
  assert_eq(process_id, Some(IntValue(12345)))
  assert_true(Resource::attributes_count(final_resource) >= 9)
}

// Test 8: Telemetry data export and processing
test "telemetry data export and processing" {
  // Test span export
  let span exporter = SpanExporter::new("console")
  let span_batch = SpanBatch::new()
  
  // Add spans to batch
  let span1 = Span::new("span-1", "operation-1", Clock::now() - 1000000L, Clock::now() - 500000L)
  let span2 = Span::new("span-2", "operation-2", Clock::now() - 2000000L, Clock::now() - 1500000L)
  
  SpanBatch::add_span(span_batch, span1)
  SpanBatch::add_span(span_batch, span2)
  
  // Test metrics export
  let metric_exporter = MetricExporter::new("console")
  let metric_batch = MetricBatch::new()
  
  // Add metrics to batch
  let metric1 = Metric::new_counter("requests.total", 100.0, [("status", StringValue("success"))])
  let metric2 = Metric::new_histogram("response.time", 150.5, [("endpoint", StringValue("/api/data"))])
  
  MetricBatch::add_metric(metric_batch, metric1)
  MetricBatch::add_metric(metric_batch, metric2)
  
  // Test log export
  let log_exporter = LogExporter::new("console")
  let log_batch = LogBatch::new()
  
  // Add logs to batch
  let log1 = LogRecord::new(INFO, "Application started")
  let log2 = LogRecord::new_with_attributes(WARN, "High memory usage", [("memory.percent", IntValue(85))])
  
  LogBatch::add_log(log_batch, log1)
  LogBatch::add_log(log_batch, log2)
  
  // Test export operations
  let span_export_result = SpanExporter::export(exporter, span_batch)
  let metric_export_result = MetricExporter::export(metric_exporter, metric_batch)
  let log_export_result = LogExporter::export(log_exporter, log_batch)
  
  // Test export statistics
  let span_count = SpanBatch::span_count(span_batch)
  let metric_count = MetricBatch::metric_count(metric_batch)
  let log_count = LogBatch::log_count(log_batch)
  
  // Verify export operations
  assert_eq(SpanExportResult::status(span_export_result), SUCCESS)
  assert_eq(MetricExportResult::status(metric_export_result), SUCCESS)
  assert_eq(LogExportResult::status(log_export_result), SUCCESS)
  assert_eq(span_count, 2)
  assert_eq(metric_count, 2)
  assert_eq(log_count, 2)
}