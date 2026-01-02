// Focused Telemetry Tests for Azimuth System
// This file contains focused test cases covering core telemetry functionality

// Test 1: Basic telemetry data collection and validation
test "basic telemetry data collection" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  
  // Create a span with basic attributes
  let span = Tracer::start_span(tracer, "test.operation")
  Span::set_attribute(span, "user.id", StringValue("user-123"))
  Span::set_attribute(span, "operation.type", StringValue("query"))
  Span::set_attribute(span, "duration", IntValue(1000))
  
  // Verify span attributes
  let user_id = Span::get_attribute(span, "user.id")
  let op_type = Span::get_attribute(span, "operation.type")
  let duration = Span::get_attribute(span, "duration")
  
  match user_id {
    Some(StringValue("user-123")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match op_type {
    Some(StringValue("query")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match duration {
    Some(IntValue(1000)) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 2: Cross-service context propagation
test "cross-service context propagation" {
  let propagator = TraceContextPropagator::new()
  let context = Context::current()
  
  // Inject context into carrier
  let carrier = TextMapPropagator::new()
  Propagator::inject(propagator, context, carrier)
  
  // Extract context from carrier
  let extracted_context = Propagator::extract(propagator, carrier)
  
  // Verify trace context is preserved
  let trace_id = Context::get_trace_id(extracted_context)
  let span_id = Context::get_span_id(extracted_context)
  
  assert_true(trace_id.length() > 0)
  assert_true(span_id.length() > 0)
}

// Test 3: Metrics aggregation and calculation
test "metrics aggregation and calculation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  // Create counter instrument
  let counter = Meter::create_counter(meter, "test.requests", Some("Test requests"), Some("count"))
  
  // Record measurements
  Counter::add(counter, 10.0)
  Counter::add(counter, 20.0)
  Counter::add(counter, 30.0)
  
  // Create histogram instrument
  let histogram = Meter::create_histogram(meter, "test.latency", Some("Test latency"), Some("ms"))
  
  // Record latency measurements
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 300.0)
  
  // Verify metric instruments are created
  assert_eq(counter.name, "test.requests")
  assert_eq(histogram.name, "test.latency")
}

// Test 4: Structured logging with correlation
test "structured logging with correlation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test.logger")
  
  // Create log record with structured data
  let record = LogRecord::new()
  LogRecord::set_severity(record, Severity::INFO)
  LogRecord::set_body(record, StringValue("User login successful"))
  LogRecord::set_attribute(record, "user.id", StringValue("user-456"))
  LogRecord::set_attribute(record, "ip.address", StringValue("192.168.1.1"))
  LogRecord::set_attribute(record, "timestamp", IntValue(1640995200))
  
  // Emit log record
  Logger::emit(logger, record)
  
  // Verify log record attributes
  let severity = LogRecord::get_severity(record)
  let body = LogRecord::get_body(record)
  let user_id = LogRecord::get_attribute(record, "user.id")
  
  assert_eq(severity, Severity::INFO)
  
  match body {
    Some(StringValue("User login successful")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match user_id {
    Some(StringValue("user-456")) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 5: Resource management with environment detection
test "resource management with environment detection" {
  let resource = Resource::new()
  
  // Set environment-specific attributes
  Resource::set_attribute(resource, "service.name", StringValue("azimuth.service"))
  Resource::set_attribute(resource, "service.version", StringValue("1.0.0"))
  Resource::set_attribute(resource, "deployment.environment", StringValue("production"))
  Resource::set_attribute(resource, "host.name", StringValue("prod-server-01"))
  
  // Create resource with automatic detection
  let auto_resource = Resource::detect_environment()
  
  // Merge resources
  let merged_resource = Resource::merge(resource, auto_resource)
  
  // Verify resource attributes
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let environment = Resource::get_attribute(merged_resource, "deployment.environment")
  
  match service_name {
    Some(StringValue("azimuth.service")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match environment {
    Some(StringValue("production")) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 6: Error handling and recovery
test "error handling and recovery" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "error.test")
  
  // Create span for error scenario
  let span = Tracer::start_span(tracer, "error.prone.operation")
  
  // Simulate error scenario
  let error_occurred = true
  
  if error_occurred {
    Span::set_status(span, StatusCode::ERROR)
    Span::set_attribute(span, "error.type", StringValue("timeout"))
    Span::set_attribute(span, "error.message", StringValue("Operation timed out after 30s"))
    Span::add_event(span, "error.occurred", [
      ("error.code", StringValue("TIMEOUT")),
      ("retry.count", IntValue(3))
    ])
  }
  
  // Verify error handling
  let status = Span::get_status(span)
  let error_type = Span::get_attribute(span, "error.type")
  
  assert_eq(status, StatusCode::ERROR)
  
  match error_type {
    Some(StringValue("timeout")) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 7: Performance optimization with batch operations
test "performance optimization with batch operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance.test")
  
  // Create multiple metric instruments
  let counters = [
    Meter::create_counter(meter, "batch.counter.1", Some("Batch counter 1"), Some("count")),
    Meter::create_counter(meter, "batch.counter.2", Some("Batch counter 2"), Some("count")),
    Meter::create_counter(meter, "batch.counter.3", Some("Batch counter 3"), Some("count"))
  ]
  
  // Perform batch operations
  for i = 0; i < counters.length(); i = i + 1 {
    for j = 0; j < 100; j = j + 1 {
      Counter::add(counters[i], 1.0)
    }
  }
  
  // Verify batch operation results
  assert_eq(counters.length(), 3)
  
  for i = 0; i < counters.length(); i = i + 1 {
    match counters[i].name {
      "batch.counter.1" => assert_true(true)
      "batch.counter.2" => assert_true(true)
      "batch.counter.3" => assert_true(true)
      _ => assert_true(false)
    }
  }
}

// Test 8: Configuration management with validation
test "configuration management with validation" {
  let config = TelemetryConfig::new()
  
  // Set configuration values
  TelemetryConfig::set_sampling_rate(config, 0.1)
  TelemetryConfig::set_max_batch_size(config, 512)
  TelemetryConfig::set_export_timeout(config, 30000)
  TelemetryConfig::set_attribute_limit(config, 1000)
  
  // Validate configuration
  let is_valid = TelemetryConfig::validate(config)
  
  // Get configuration values
  let sampling_rate = TelemetryConfig::get_sampling_rate(config)
  let max_batch_size = TelemetryConfig::get_max_batch_size(config)
  let export_timeout = TelemetryConfig::get_export_timeout(config)
  let attribute_limit = TelemetryConfig::get_attribute_limit(config)
  
  assert_true(is_valid)
  assert_eq(sampling_rate, 0.1)
  assert_eq(max_batch_size, 512)
  assert_eq(export_timeout, 30000)
  assert_eq(attribute_limit, 1000)
}

// Test 9: Data serialization and deserialization
test "data serialization and deserialization" {
  let span_data = SpanData::new()
  
  // Set span data
  SpanData::set_trace_id(span_data, "trace-123456789")
  SpanData::set_span_id(span_data, "span-987654321")
  SpanData::set_parent_span_id(span_data, "parent-555555555")
  SpanData::set_operation_name(span_data, "test.operation")
  SpanData::set_start_time(span_data, 1640995200000)
  SpanData::set_end_time(span_data, 1640995201000)
  SpanData::set_status(span_data, StatusCode::OK)
  
  // Add attributes
  SpanData::add_attribute(span_data, "service.name", StringValue("test.service"))
  SpanData::add_attribute(span_data, "operation.type", StringValue("read"))
  
  // Serialize span data
  let serialized = SpanData::serialize(span_data)
  
  // Deserialize span data
  let deserialized = SpanData::deserialize(serialized)
  
  // Verify serialization/deserialization
  let original_trace_id = SpanData::get_trace_id(span_data)
  let restored_trace_id = SpanData::get_trace_id(deserialized)
  
  assert_eq(original_trace_id, restored_trace_id)
}

// Test 10: Lifecycle management with cleanup
test "lifecycle management with cleanup" {
  let telemetry = TelemetrySystem::new()
  
  // Initialize telemetry system
  TelemetrySystem::initialize(telemetry)
  
  // Create providers
  let tracer_provider = TelemetrySystem::get_tracer_provider(telemetry)
  let meter_provider = TelemetrySystem::get_meter_provider(telemetry)
  let logger_provider = TelemetrySystem::get_logger_provider(telemetry)
  
  // Create instruments
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test")
  let meter = MeterProvider::get_meter(meter_provider, "lifecycle.test")
  let logger = LoggerProvider::get_logger(logger_provider, "lifecycle.test")
  
  // Verify providers are initialized
  assert_true(tracer_provider != None)
  assert_true(meter_provider != None)
  assert_true(logger_provider != None)
  
  // Verify instruments are created
  assert_true(tracer != None)
  assert_true(meter != None)
  assert_true(logger != None)
  
  // Shutdown telemetry system
  let shutdown_result = TelemetrySystem::shutdown(telemetry)
  assert_true(shutdown_result)
}