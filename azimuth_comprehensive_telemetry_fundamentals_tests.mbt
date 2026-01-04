// Azimuth Telemetry System - Comprehensive Fundamentals Tests
// This file contains comprehensive test cases for fundamental telemetry functionality

// Test 1: Telemetry Provider Initialization
test "telemetry provider initialization" {
  let provider = TelemetryProvider::new()
  assert_true(TelemetryProvider::is_initialized(provider))
  
  // Test provider configuration
  let config = TelemetryConfig::new()
  TelemetryConfig::set_service_name(config, "test_service")
  TelemetryConfig::set_service_version(config, "1.0.0")
  TelemetryConfig::set_sampling_rate(config, 1.0)
  
  let configured_provider = TelemetryProvider::with_config(config)
  assert_true(TelemetryProvider::is_initialized(configured_provider))
}

// Test 2: Trace Operations
test "trace operations and propagation" {
  let tracer = TracerProvider::get_tracer("test_tracer")
  
  // Test span creation with attributes
  let span = Tracer::start_span(tracer, "test_operation", [
    ("operation.type", StringValue("test")),
    ("operation.priority", IntValue(1))
  ])
  
  assert_eq(Span::name(span), "test_operation")
  assert_true(Span::is_recording(span))
  
  // Test adding events to span
  Span::add_event(span, "operation_started", Some([
    ("timestamp", IntValue(1234567890)),
    ("status", StringValue("initialized"))
  ]))
  
  Span::add_event(span, "operation_progress", Some([
    ("progress", IntValue(50)),
    ("current_step", StringValue("processing"))
  ]))
  
  // Test setting span status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Test ending span
  Span::end(span)
  assert_false(Span::is_recording(span))
}

// Test 3: Metrics Collection and Aggregation
test "metrics collection and aggregation" {
  let meter = MeterProvider::get_meter("test_meter")
  
  // Test counter metric
  let request_counter = Meter::create_counter(meter, "http_requests_total", 
    Some("Total HTTP requests"), Some("requests"))
  
  Counter::add(request_counter, 1.0, Some([
    ("method", StringValue("GET")),
    ("status", StringValue("200"))
  ]))
  
  Counter::add(request_counter, 1.0, Some([
    ("method", StringValue("POST")),
    ("status", StringValue("201"))
  ]))
  
  // Test histogram metric
  let response_time = Meter::create_histogram(meter, "http_response_time_seconds",
    Some("HTTP response time in seconds"), Some("seconds"))
  
  Histogram::record(response_time, 0.1, Some([
    ("endpoint", StringValue("/api/users"))
  ]))
  
  Histogram::record(response_time, 0.25, Some([
    ("endpoint", StringValue("/api/orders"))
  ]))
  
  // Test gauge metric
  let active_connections = Meter::create_gauge(meter, "active_connections",
    Some("Number of active connections"), Some("connections"))
  
  Gauge::record(active_connections, 42.0, Some([
    ("connection_type", StringValue("websocket"))
  ]))
}

// Test 4: Logging and Correlation
test "logging and trace correlation" {
  let logger = LoggerProvider::get_logger("test_logger")
  
  // Test log record with severity levels
  let info_log = LogRecord::new(Info, "Application started successfully")
  Logger::emit(logger, info_log)
  
  let warn_log = LogRecord::new(Warn, "Deprecated API endpoint used")
  Logger::emit(logger, warn_log)
  
  let error_log = LogRecord::new(Error, "Database connection failed")
  Logger::emit(logger, error_log)
  
  // Test log with structured attributes
  let structured_log = LogRecord::new_with_attributes(
    Info,
    "User authentication attempt",
    [
      ("user.id", StringValue("user123")),
      ("user.ip", StringValue("192.168.1.1")),
      ("auth.method", StringValue("oauth2")),
      ("timestamp", IntValue(1234567890))
    ]
  )
  Logger::emit(logger, structured_log)
  
  // Test log with trace correlation
  let trace_context = SpanContext::new("trace123", "span456", true, "active")
  let correlated_log = LogRecord::new_with_trace_context(
    Error,
    "Payment processing failed",
    trace_context,
    [
      ("payment.id", StringValue("pay789")),
      ("error.code", StringValue("CARD_DECLINED")),
      ("amount", FloatValue(99.99))
    ]
  )
  Logger::emit(logger, correlated_log)
}

// Test 5: Context Propagation
test "context propagation across operations" {
  // Create initial context with baggage
  let root_ctx = Context::root()
  let baggage_key = ContextKey::new("correlation_id")
  let ctx_with_baggage = Context::with_value(root_ctx, baggage_key, "corr-12345")
  
  // Test context propagation through span
  let tracer = TracerProvider::get_tracer("propagation_test")
  let span = Tracer::start_span_with_context(tracer, "operation1", ctx_with_baggage)
  
  // Extract context from span
  let span_ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(span_ctx), "trace123")
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Test context retrieval
  let retrieved_value = Context::get(ctx_with_baggage, baggage_key)
  match retrieved_value {
    Some(value) => assert_eq(value, "corr-12345")
    None => assert_true(false)
  }
  
  // Test nested context operations
  let nested_key = ContextKey::new("operation_id")
  let nested_ctx = Context::with_value(ctx_with_baggage, nested_key, "op-67890")
  
  let nested_value = Context::get(nested_ctx, nested_key)
  match nested_value {
    Some(value) => assert_eq(value, "op-67890")
    None => assert_true(false)
  }
  
  // Original context should still be accessible
  let original_value = Context::get(nested_ctx, baggage_key)
  match original_value {
    Some(value) => assert_eq(value, "corr-12345")
    None => assert_true(false)
  }
  
  Span::end(span)
}

// Test 6: Resource Detection and Configuration
test "resource detection and configuration" {
  // Test automatic resource detection
  let auto_resource = Resource::detect()
  let service_name = Resource::get_attribute(auto_resource, "service.name")
  
  // Test manual resource configuration
  let manual_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("azimuth_telemetry")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-abc123")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("web-server-01")),
    ("process.pid", IntValue(12345))
  ])
  
  // Test resource merging
  let env_resource = Resource::with_attributes(Resource::new(), [
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("k8s.namespace", StringValue("production")),
    ("k8s.pod.name", StringValue("azimuth-pod-xyz"))
  ])
  
  let merged_resource = Resource::merge(manual_resource, env_resource)
  
  // Verify merged attributes
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  match merged_service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth_telemetry")
    _ => assert_true(false)
  }
  
  let cloud_provider = Resource::get_attribute(merged_resource, "cloud.provider")
  match cloud_provider {
    Some(StringValue(provider)) => assert_eq(provider, "aws")
    _ => assert_true(false)
  }
}

// Test 7: Sampling Strategies
test "sampling strategies and configuration" {
  // Test always-on sampling
  let always_on_sampler = Sampler::always_on()
  let sampling_decision1 = Sampler::should_sample(always_on_sampler, 
    "trace123", "span456", "operation_name", [])
  assert_eq(sampling_decision1.decision, RecordAndSample)
  
  // Test always-off sampling
  let always_off_sampler = Sampler::always_off()
  let sampling_decision2 = Sampler::should_sample(always_off_sampler,
    "trace456", "span789", "operation_name", [])
  assert_eq(sampling_decision2.decision, Drop)
  
  // Test trace ID ratio sampler
  let ratio_sampler = Sampler::trace_id_ratio(0.5) // 50% sampling
  let sampling_decision3 = Sampler::should_sample(ratio_sampler,
    "trace789", "span012", "operation_name", [])
  // Decision depends on trace ID, should be deterministic
  
  // Test parent-based sampler
  let parent_based_sampler = Sampler::parent_based(always_on_sampler)
  let parent_context = SpanContext::new("parent_trace", "parent_span", true, "active")
  let sampling_decision4 = Sampler::should_sample_with_parent(parent_based_sampler,
    "child_trace", "child_span", "operation_name", [], Some(parent_context))
  assert_eq(sampling_decision4.decision, RecordAndSample)
}

// Test 8: Instrumentation Library Configuration
test "instrumentation library configuration" {
  // Test instrumentation library creation
  let instrumentation = InstrumentationLibrary::new("azimuth.telemetry", "1.0.0")
  
  assert_eq(InstrumentationLibrary::name(instrumentation), "azimuth.telemetry")
  assert_eq(InstrumentationLibrary::version(instrumentation), Some("1.0.0"))
  
  // Test instrumentation scope
  let scope = InstrumentationScope::new("azimuth.metrics", "2.1.0", 
    Some("Azimuth metrics collection"), Some("azimuth"))
  
  assert_eq(InstrumentationScope::name(scope), "azimuth.metrics")
  assert_eq(InstrumentationScope::version(scope), Some("2.1.0"))
  match InstrumentationScope::schema_url(scope) {
    Some(url) => assert_eq(url, "azimuth")
    None => assert_true(false)
  }
}

// Test 9: Batch Processing and Exporting
test "batch processing and exporting" {
  // Test batch span processor
  let span_exporter = SpanExporter::new("console")
  let batch_processor = BatchSpanProcessor::new(span_exporter, 
    BatchSpanProcessorConfig::new()
      .with_max_queue_size(2048)
      .with_max_export_batch_size(512)
      .with_export_timeout_millis(30000))
  
  // Test adding spans to batch
  let tracer = TracerProvider::get_tracer("batch_test")
  for i in 0..=10 {
    let span = Tracer::start_span(tracer, "batch_operation_" + i.to_string())
    Span::add_event(span, "processing_item", Some([
      ("item.id", IntValue(i)),
      ("batch.size", IntValue(10))
    ]))
    Span::end(span)
  }
  
  // Test force flush
  BatchSpanProcessor::force_flush(batch_processor)
  
  // Test shutdown
  BatchSpanProcessor::shutdown(batch_processor)
}

// Test 10: Telemetry Signal Integration
test "telemetry signal integration" {
  // Test integrated telemetry signals
  let telemetry = TelemetryBuilder::new()
    .with_service_name("integrated_service")
    .with_service_version("1.0.0")
    .with_traces_enabled(true)
    .with_metrics_enabled(true)
    .with_logs_enabled(true)
    .build()
  
  let tracer = Telemetry::get_tracer(telemetry, "integrated_tracer")
  let meter = Telemetry::get_meter(telemetry, "integrated_meter")
  let logger = Telemetry::get_logger(telemetry, "integrated_logger")
  
  // Create integrated operation
  let span = Tracer::start_span(tracer, "integrated_operation")
  
  // Record metric during operation
  let operation_counter = Meter::create_counter(meter, "integrated_operations_total")
  Counter::add(operation_counter, 1.0, Some([
    ("operation.type", StringValue("integrated"))
  ]))
  
  // Log during operation
  let operation_log = LogRecord::new_with_trace_context(
    Info,
    "Integrated operation in progress",
    Span::span_context(span),
    [
      ("operation.phase", StringValue("processing"))
    ]
  )
  Logger::emit(logger, operation_log)
  
  // Complete operation
  Span::set_status(span, Ok, Some("Integrated operation completed"))
  Span::end(span)
}