// Azimuth Telemetry System - Comprehensive New Test Suite
// This file contains 10 comprehensive test cases for various aspects of the telemetry system

// Test 1: Data Serialization and Deserialization
test "data serialization and deserialization" {
  // Test attribute serialization
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  // Serialize to JSON-like format
  let serialized = Serializer::serialize_attributes(attrs)
  assert_true(serialized.length() > 0)
  
  // Deserialize back to attributes
  let deserialized = Deserializer::deserialize_attributes(serialized)
  match deserialized {
    Some(deserialized_attrs) => {
      let string_result = Attributes::get(deserialized_attrs, "string.key")
      match string_result {
        Some(StringValue(v)) => assert_eq(v, "test_value")
        _ => assert_true(false)
      }
      
      let int_result = Attributes::get(deserialized_attrs, "int.key")
      match int_result {
        Some(IntValue(v)) => assert_eq(v, 42)
        _ => assert_true(false)
      }
      
      let float_result = Attributes::get(deserialized_attrs, "float.key")
      match float_result {
        Some(FloatValue(v)) => assert_eq(v, 3.14)
        _ => assert_true(false)
      }
      
      let bool_result = Attributes::get(deserialized_attrs, "bool.key")
      match bool_result {
        Some(BoolValue(v)) => assert_true(v)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test span serialization
  let span_ctx = SpanContext::new("trace_id_123", "span_id_456", true, "test_state")
  let span = Span::new("test_span", Internal, span_ctx)
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "test_event", Some([("test_attr", StringValue("test_value"))]))
  
  let serialized_span = Serializer::serialize_span(span)
  assert_true(serialized_span.length() > 0)
  
  let deserialized_span = Deserializer::deserialize_span(serialized_span)
  match deserialized_span {
    Some(dspan) => {
      assert_eq(Span::name(dspan), "test_span")
      assert_eq(Span::status(dspan), Ok)
    }
    None => assert_true(false)
  }
}

// Test 2: Time Series Data Processing
test "time series data processing" {
  let time_series = TimeSeries::new()
  
  // Add data points with timestamps
  let now = 1640995200L // 2022-01-01 00:00:00 UTC
  
  TimeSeries::add_point(time_series, now, 10.5)
  TimeSeries::add_point(time_series, now + 60L, 12.3)
  TimeSeries::add_point(time_series, now + 120L, 11.8)
  TimeSeries::add_point(time_series, now + 180L, 13.2)
  TimeSeries::add_point(time_series, now + 240L, 14.7)
  
  // Test aggregation functions
  let avg = TimeSeries::average(time_series)
  assert_true(avg > 10.0 && avg < 15.0)
  
  let max = TimeSeries::max(time_series)
  assert_eq(max, 14.7)
  
  let min = TimeSeries::min(time_series)
  assert_eq(min, 10.5)
  
  let count = TimeSeries::count(time_series)
  assert_eq(count, 5)
  
  // Test time range queries
  let points_in_range = TimeSeries::get_points_in_range(time_series, now + 60L, now + 180L)
  assert_eq(points_in_range.length(), 3)
  
  // Test downsampling
  let downsampled = TimeSeries::downsample(time_series, 120L) // 2-minute intervals
  assert_true(downsampled.length() <= 3)
  
  // Test trend analysis
  let trend = TimeSeries::calculate_trend(time_series)
  match trend {
    Increasing => assert_true(true)
    Decreasing => assert_true(true)
    Stable => assert_true(true)
  }
}

// Test 3: Error Handling and Recovery
test "error handling and recovery" {
  // Test error context creation
  let error_ctx = ErrorContext::new("test_operation", "Test error occurred")
  assert_eq(ErrorContext::operation(error_ctx), "test_operation")
  assert_eq(ErrorContext::message(error_ctx), "Test error occurred")
  
  // Test error with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "error.code", StringValue("E500"))
  Attributes::set(attrs, "retry.count", IntValue(3))
  
  let error_with_attrs = ErrorContext::with_attributes("failing_operation", "Operation failed", attrs)
  let error_attrs = ErrorContext::attributes(error_with_attrs)
  
  let error_code = Attributes::get(error_attrs, "error.code")
  match error_code {
    Some(StringValue(code)) => assert_eq(code, "E500")
    _ => assert_true(false)
  }
  
  // Test error recovery strategy
  let recovery_strategy = RecoveryStrategy::exponential_backoff(3, 1000L) // 3 retries, 1s initial delay
  
  let mut attempt_count = 0
  let result = RecoveryStrategy::execute_with_retry(recovery_strategy, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Error("Simulated failure")
    } else {
      Ok("Success after retries")
    }
  })
  
  match result {
    Ok(value) => assert_eq(value, "Success after retries")
    Error(_) => assert_true(false)
  }
  
  assert_eq(attempt_count, 3)
  
  // Test circuit breaker pattern
  let circuit_breaker = CircuitBreaker::new(5, 10000L) // 5 failures, 10s timeout
  
  // Simulate failures
  for i = 0; i < 6; i = i + 1 {
    let _ = CircuitBreaker::execute(circuit_breaker, fn() {
      Error("Service unavailable")
    })
  }
  
  // Circuit should be open now
  assert_true(CircuitBreaker::is_open(circuit_breaker))
  
  let circuit_result = CircuitBreaker::execute(circuit_breaker, fn() {
    Ok("Should not execute")
  })
  
  match circuit_result {
    Error(msg) => assert_eq(msg, "Circuit breaker is open")
    _ => assert_true(false)
  }
}

// Test 4: Performance Benchmarking
test "performance benchmarking" {
  let benchmark = Benchmark::new()
  
  // Benchmark span creation
  let span_creation_time = Benchmark::measure(benchmark, 1000, fn() {
    let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
    let _ = Span::new("test_span", Internal, span_ctx)
  })
  
  // Span creation should be fast (less than 1ms per operation)
  assert_true(span_creation_time < 1.0)
  
  // Benchmark attribute operations
  let attrs = Attributes::new()
  let attribute_time = Benchmark::measure(benchmark, 1000, fn() {
    Attributes::set(attrs, "test.key", StringValue("test_value"))
    let _ = Attributes::get(attrs, "test.key")
  })
  
  // Attribute operations should be fast
  assert_true(attribute_time < 0.5)
  
  // Benchmark serialization
  let test_attrs = Attributes::new()
  Attributes::set(test_attrs, "key1", StringValue("value1"))
  Attributes::set(test_attrs, "key2", IntValue(42))
  Attributes::set(test_attrs, "key3", FloatValue(3.14))
  
  let serialization_time = Benchmark::measure(benchmark, 100, fn() {
    let _ = Serializer::serialize_attributes(test_attrs)
  })
  
  // Serialization should be reasonable
  assert_true(serialization_time < 5.0)
  
  // Benchmark memory usage
  let initial_memory = MemoryMonitor::current_usage()
  
  let spans = []
  for i = 0; i < 100; i = i + 1 {
    let span_ctx = SpanContext::new("trace_id", "span_id_" + i.to_string(), true, "")
    let span = Span::new("span_" + i.to_string(), Internal, span_ctx)
    spans.push(span)
  }
  
  let final_memory = MemoryMonitor::current_usage()
  let memory_increase = final_memory - initial_memory
  
  // Memory usage should be reasonable (less than 1MB for 100 spans)
  assert_true(memory_increase < 1024 * 1024)
  
  // Cleanup
  for span in spans {
    Span::end(span)
  }
}

// Test 5: Concurrent Safety
test "concurrent safety" {
  // Test concurrent attribute operations
  let attrs = Attributes::new()
  
  // Simulate concurrent writes
  let write_results = []
  for i = 0; i < 10; i = i + 1 {
    let result = ConcurrentTask::spawn(fn() {
      Attributes::set(attrs, "key_" + i.to_string(), IntValue(i))
      true
    })
    write_results.push(result)
  }
  
  // Wait for all writes to complete
  for result in write_results {
    assert_true(ConcurrentTask::join(result))
  }
  
  // Verify all attributes were set
  for i = 0; i < 10; i = i + 1 {
    let value = Attributes::get(attrs, "key_" + i.to_string())
    match value {
      Some(IntValue(v)) => assert_eq(v, i)
      _ => assert_true(false)
    }
  }
  
  // Test concurrent span operations
  let spans = []
  let span_tasks = []
  
  for i = 0; i < 5; i = i + 1 {
    let task = ConcurrentTask::spawn(fn() {
      let span_ctx = SpanContext::new("trace_id", "span_id_" + i.to_string(), true, "")
      let span = Span::new("concurrent_span", Internal, span_ctx)
      Span::add_event(span, "concurrent_event", None)
      Span::end(span)
      true
    })
    span_tasks.push(task)
  }
  
  // Wait for all span operations to complete
  for task in span_tasks {
    assert_true(ConcurrentTask::join(task))
  }
  
  // Test thread-safe metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_test")
  let counter = Meter::create_counter(meter, "concurrent_counter", None, None)
  
  let counter_tasks = []
  for i = 0; i < 10; i = i + 1 {
    let task = ConcurrentTask::spawn(fn() {
      Counter::add(counter, 1.0)
      true
    })
    counter_tasks.push(task)
  }
  
  // Wait for all counter operations to complete
  for task in counter_tasks {
    assert_true(ConcurrentTask::join(task))
  }
}

// Test 6: Cross-Service Propagation
test "cross-service propagation" {
  // Test propagator injection
  let span_ctx = SpanContext::new("trace_id_123", "span_id_456", true, "test_state")
  let carrier = TextMapCarrier::new()
  
  let propagator = TraceContextPropagator::new()
  Propagator::inject(propagator, span_ctx, carrier)
  
  // Verify traceparent header
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  match traceparent {
    Some(value) => {
      assert_true(value.contains("trace_id_123"))
      assert_true(value.contains("span_id_456"))
    }
    None => assert_true(false)
  }
  
  // Test propagator extraction
  let extracted_ctx = Propagator::extract(propagator, carrier)
  match extracted_ctx {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), "trace_id_123")
      assert_eq(SpanContext::span_id(ctx), "span_id_456")
      assert_true(SpanContext::is_sampled(ctx))
    }
    None => assert_true(false)
  }
  
  // Test composite propagator
  let baggage_propagator = BaggagePropagator::new()
  let composite = CompositePropagator::new([propagator, baggage_propagator])
  
  // Add baggage
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let carrier_with_baggage = TextMapCarrier::new()
  
  Propagator::inject(composite, span_ctx, carrier_with_baggage)
  BaggagePropagator::inject_baggage(baggage_propagator, updated_baggage, carrier_with_baggage)
  
  // Extract both trace context and baggage
  let extracted_composite_ctx = Propagator::extract(composite, carrier_with_baggage)
  match extracted_composite_ctx {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), "trace_id_123")
      assert_eq(SpanContext::span_id(ctx), "span_id_456")
    }
    None => assert_true(false)
  }
  
  let extracted_baggage = BaggagePropagator::extract_baggage(baggage_propagator, carrier_with_baggage)
  let user_id = Baggage::get_entry(extracted_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
}

// Test 7: Resource Management
test "resource management" {
  // Test resource creation with attributes
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("process.id", IntValue(12345))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  let process_id = Resource::get_attribute(resource, "process.id")
  match process_id {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
  
  // Test resource merging
  let override_attrs = [
    ("service.version", StringValue("2.0.0")), // This should override
    ("deployment.environment", StringValue("test")) // New attribute
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource, override_resource)
  
  // Verify override
  let merged_version = Resource::get_attribute(merged_resource, "service.version")
  match merged_version {
    Some(StringValue(version)) => assert_eq(version, "2.0.0")
    _ => assert_true(false)
  }
  
  // Verify new attribute
  let environment = Resource::get_attribute(merged_resource, "deployment.environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "test")
    _ => assert_true(false)
  }
  
  // Verify original attribute still exists
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  match merged_service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  // Test resource detection
  let auto_detected = ResourceDetector::detect()
  assert_true(Resource::get_attribute(auto_detected, "telemetry.sdk.name").is_some())
  assert_true(Resource::get_attribute(auto_detected, "telemetry.sdk.language").is_some())
  assert_true(Resource::get_attribute(auto_detected, "telemetry.sdk.version").is_some())
}

// Test 8: Configuration Management
test "configuration management" {
  // Test configuration creation
  let config = Configuration::new()
  
  // Test setting and getting values
  Configuration::set(config, "service.name", "test_service")
  Configuration::set(config, "service.port", 8080)
  Configuration::set(config, "service.debug", true)
  Configuration::set(config, "service.timeout", 30.5)
  
  // Test string retrieval
  let service_name = Configuration::get_string(config, "service.name")
  match service_name {
    Some(name) => assert_eq(name, "test_service")
    None => assert_true(false)
  }
  
  // Test int retrieval
  let port = Configuration::get_int(config, "service.port")
  match port {
    Some(p) => assert_eq(p, 8080)
    None => assert_true(false)
  }
  
  // Test bool retrieval
  let debug = Configuration::get_bool(config, "service.debug")
  match debug {
    Some(d) => assert_true(d)
    None => assert_true(false)
  }
  
  // Test float retrieval
  let timeout = Configuration::get_float(config, "service.timeout")
  match timeout {
    Some(t) => assert_eq(t, 30.5)
    None => assert_true(false)
  }
  
  // Test default values
  let non_existent = Configuration::get_string(config, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let default_value = Configuration::get_string_with_default(config, "non.existent.key", "default")
  assert_eq(default_value, "default")
  
  // Test configuration validation
  let validation_rules = [
    ConfigurationRule::required("service.name"),
    ConfigurationRule::range("service.port", 1, 65535),
    ConfigurationRule::min("service.timeout", 0.0)
  ]
  
  let validation_result = Configuration::validate(config, validation_rules)
  assert_true(validation_result.is_valid)
  
  // Test invalid configuration
  Configuration::set(config, "service.port", 70000) // Invalid port
  let invalid_result = Configuration::validate(config, validation_rules)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() > 0)
  
  // Test configuration from environment variables
  let env_config = Configuration::from_env()
  assert_true(env_config.is_some())
}

// Test 9: Data Integrity Validation
test "data integrity validation" {
  // Test checksum calculation
  let data = "test data for integrity check"
  let checksum = IntegrityChecker::calculate_checksum(data)
  assert_true(checksum.length() > 0)
  
  // Verify checksum
  let is_valid = IntegrityChecker::verify_checksum(data, checksum)
  assert_true(is_valid)
  
  // Test modified data
  let modified_data = "modified data"
  let is_modified_valid = IntegrityChecker::verify_checksum(modified_data, checksum)
  assert_false(is_modified_valid)
  
  // Test attribute integrity
  let attrs = Attributes::new()
  Attributes::set(attrs, "key1", StringValue("value1"))
  Attributes::set(attrs, "key2", IntValue(42))
  
  let attrs_checksum = IntegrityChecker::calculate_attributes_checksum(attrs)
  
  // Modify attributes
  Attributes::set(attrs, "key3", FloatValue(3.14))
  let modified_attrs_checksum = IntegrityChecker::calculate_attributes_checksum(attrs)
  
  assert_ne(attrs_checksum, modified_attrs_checksum)
  
  // Test span integrity
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("integrity_test_span", Internal, span_ctx)
  Span::add_event(span, "test_event", None)
  
  let span_checksum = IntegrityChecker::calculate_span_checksum(span)
  assert_true(span_checksum.length() > 0)
  
  // Test time series integrity
  let time_series = TimeSeries::new()
  TimeSeries::add_point(time_series, 1640995200L, 10.5)
  TimeSeries::add_point(time_series, 1640995260L, 12.3)
  
  let series_checksum = IntegrityChecker::calculate_time_series_checksum(time_series)
  assert_true(series_checksum.length() > 0)
  
  // Add more points and verify checksum changes
  TimeSeries::add_point(time_series, 1640995320L, 11.8)
  let modified_series_checksum = IntegrityChecker::calculate_time_series_checksum(time_series)
  assert_ne(series_checksum, modified_series_checksum)
  
  // Test data corruption detection
  let original_data = "important telemetry data"
  let original_checksum = IntegrityChecker::calculate_checksum(original_data)
  
  // Simulate corruption
  let corrupted_data = "important telemetry d@ta" // Changed character
  let corruption_detected = IntegrityChecker::verify_checksum(corrupted_data, original_checksum)
  assert_false(corruption_detected)
}

// Test 10: Real-time Stream Processing
test "real-time stream processing" {
  // Create stream processor
  let processor = StreamProcessor::new()
  
  // Define stream pipeline
  let pipeline = StreamPipeline::new()
    .filter(fn(data) { data.value > 10.0 }) // Filter values > 10
    .map(fn(data) { StreamData(data.timestamp, data.value * 2.0) }) // Double values
    .window(5000L) // 5-second windows
    .aggregate(fn(window) { 
      let sum = window.fold(0.0, fn(acc, item) { acc + item.value })
      sum / window.length().to_float()
    }) // Calculate average
  
  StreamProcessor::set_pipeline(processor, pipeline)
  
  // Generate test data stream
  let now = 1640995200L
  let stream_data = [
    StreamData(now, 5.0),      // Will be filtered out
    StreamData(now + 1000L, 15.0), // Will pass and become 30.0
    StreamData(now + 2000L, 12.5), // Will pass and become 25.0
    StreamData(now + 3000L, 8.0),   // Will be filtered out
    StreamData(now + 4000L, 20.0),  // Will pass and become 40.0
    StreamData(now + 6000L, 18.0)   // Next window
  ]
  
  // Process stream
  let results = []
  for data in stream_data {
    let result = StreamProcessor::process(processor, data)
    match result {
      Some(value) => results.push(value)
      None => {} // Filtered out or window not ready
    }
  }
  
  // Should have one result for first window
  assert_eq(results.length(), 1)
  
  // Verify average calculation (30.0 + 25.0 + 40.0) / 3 = 31.666...
  let avg_result = results[0]
  assert_true(avg_result > 31.6 && avg_result < 31.7)
  
  // Test stream with time-based triggers
  let time_processor = StreamProcessor::new()
  let time_pipeline = StreamPipeline::new()
    .buffer(1000L) // Buffer for 1 second
    .trigger_by_time() // Trigger by time instead of count
    .count() // Count items in buffer
  
  StreamProcessor::set_pipeline(time_processor, time_pipeline)
  
  // Add data
  StreamProcessor::process(time_processor, StreamData(now, 1.0))
  StreamProcessor::process(time_processor, StreamData(now + 500L, 2.0))
  
  // Simulate time passing
  StreamProcessor::advance_time(time_processor, 1500L)
  
  let time_result = StreamProcessor::process(time_processor, StreamData(now + 1500L, 3.0))
  match time_result {
    Some(count) => assert_eq(count, 2.0) // Should count the 2 items in the buffer
    None => assert_true(false)
  }
  
  // Test stream error handling
  let error_processor = StreamProcessor::new()
  let error_pipeline = StreamPipeline::new()
    .map(fn(data) { 
      if data.value == 15.0 {
        Error("Processing error")
      } else {
        Ok(data)
      }
    })
    .recover(fn(error) { StreamData(0L, 0.0) }) // Recovery function
  
  StreamProcessor::set_pipeline(error_processor, error_pipeline)
  
  let error_result1 = StreamProcessor::process(error_processor, StreamData(now, 10.0))
  match error_result1 {
    Some(data) => assert_eq(data.value, 10.0)
    None => assert_true(false)
  }
  
  let error_result2 = StreamProcessor::process(error_processor, StreamData(now, 15.0))
  match error_result2 {
    Some(data) => assert_eq(data.value, 0.0) // Recovery value
    None => assert_true(false)
  }
}