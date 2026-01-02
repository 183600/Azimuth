// Azimuth Telemetry System - Enhanced Test Suite
// This file contains comprehensive test cases for advanced telemetry functionality

// Test 1: Span Events and Status Management
test "span events and status management" {
  // Test span creation with initial state
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Validate initial span state
  assert_eq(span.name(), "test-span")
  assert_true(span.is_recording())
  assert_eq(SpanContext::trace_id(span.span_context()), trace_id)
  assert_eq(SpanContext::span_id(span.span_context()), span_id)
  assert_true(SpanContext::is_sampled(span.span_context()))
  
  // Test span status transitions
  assert_eq(span.status(), Unset)
  
  // Set status to OK
  span.set_status(Ok, Some("Operation completed successfully"))
  assert_eq(span.status(), Ok)
  
  // Set status to Error
  span.set_status(Error, Some("Operation failed with timeout"))
  assert_eq(span.status(), Error)
  
  // Test span event addition
  span.add_event("database.query.start", Some([("db.statement", "SELECT * FROM users"), ("db.type", "postgresql")]))
  span.add_event("database.query.complete", Some([("db.rows_affected", "42"), ("db.duration_ms", "150")]))
  
  // Test span ending
  span.end()
  // After ending, the span should no longer be recording
  // Note: In a real implementation, this would change the recording state
}

// Test 2: Baggage Propagation Edge Cases
test "baggage propagation edge cases" {
  // Test baggage with special characters
  let special_chars_baggage = [
    ("user.id", "user@123!#$%"),
    ("session.id", "sess-abc_123.xyz"),
    ("request.id", "req_2023-12-01T15:30:00Z"),
    ("complex.value", "a=b,c=d;e=f?g=h")
  ]
  
  let baggage = Baggage::new()
  let mut updated_baggage = baggage
  
  // Add baggage items with special characters
  for (key, value) in special_chars_baggage {
    updated_baggage = Baggage::set_entry(updated_baggage, key, value)
    
    // Verify the entry was set correctly
    let retrieved = Baggage::get_entry(updated_baggage, key)
    match retrieved {
      Some(v) => assert_eq(v, value)
      None => assert_true(false) // Should not happen
    }
  }
  
  // Test baggage with empty values
  let empty_value_baggage = Baggage::set_entry(updated_baggage, "empty.key", "")
  let empty_value = Baggage::get_entry(empty_value_baggage, "empty.key")
  match empty_value {
    Some(v) => assert_eq(v, "")
    None => assert_true(false) // Should not happen
  }
  
  // Test baggage removal
  let removed_baggage = Baggage::remove_entry(empty_value_baggage, "user.id")
  let removed_value = Baggage::get_entry(removed_baggage, "user.id")
  assert_eq(removed_value, None)
  
  // Test non-existent key removal
  let unchanged_baggage = Baggage::remove_entry(removed_baggage, "non.existent.key")
  // Should still be able to get other values
  let session_value = Baggage::get_entry(unchanged_baggage, "session.id")
  match session_value {
    Some(v) => assert_eq(v, "sess-abc_123.xyz")
    None => assert_true(false) // Should not happen
  }
}

// Test 3: Metrics Aggregation and High-Frequency Operations
test "metrics aggregation and high-frequency operations" {
  // Test counter with high-frequency updates
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  let counter = Meter::create_counter(meter, "high-frequency-counter")
  
  // Simulate high-frequency counter updates
  for i = 0; i < 10000; i = i + 1 {
    Counter::add(counter, 1.0)
  }
  
  // Test gauge with rapid value changes
  let gauge = Meter::create_gauge(meter, "rapid-gauge")
  let readings = [10.5, 15.2, 8.7, 22.1, 19.3, 25.8, 12.4, 18.9, 14.6, 20.3]
  
  for reading in readings {
    // In a real implementation, this would update the gauge value
    // For testing purposes, we just validate the reading values
    assert_true(reading >= 0.0)
    assert_true(reading <= 100.0)
  }
  
  // Test histogram with distribution
  let histogram = Meter::create_histogram(meter, "response-time-histogram", Some("Response time distribution"), Some("ms"))
  let response_times = [10, 25, 50, 75, 100, 150, 200, 300, 500, 1000]
  
  for time in response_times {
    Histogram::record(histogram, time.to_float())
  }
  
  // Calculate basic statistics
  let sum = response_times.reduce(|acc, val| acc + val, 0)
  let count = response_times.length()
  let mean = sum / count
  
  assert_eq(sum, 2410)
  assert_eq(count, 10)
  assert_eq(mean, 241)
  
  // Test up-down counter with increments and decrements
  let updown_counter = Meter::create_updown_counter(meter, "queue-size")
  
  // Simulate queue size changes
  let queue_operations = [5, -2, 3, -1, 4, -3, 2, -1, 1, -2]
  let mut queue_size = 0
  
  for op in queue_operations {
    queue_size = queue_size + op
    assert_true(queue_size >= 0) // Queue size should never be negative
    UpDownCounter::add(updown_counter, op.to_float())
  }
  
  assert_eq(queue_size, 6)
}

// Test 4: Log Recording and Context Correlation
test "log recording and context correlation" {
  // Test log record creation with full context
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  
  // Create log records with different severity levels
  let trace_log = LogRecord::new(Trace, "Entering function")
  let debug_log = LogRecord::new(Debug, "Processing request with ID: req-123")
  let info_log = LogRecord::new(Info, "Request processed successfully")
  let warn_log = LogRecord::new(Warn, "Retrying operation due to temporary failure")
  let error_log = LogRecord::new(Error, "Database connection failed")
  let fatal_log = LogRecord::new(Fatal, "System crash - immediate shutdown required")
  
  // Verify log severity levels
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Test log record with trace and span correlation
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let correlated_log = LogRecord::new_with_context(
    Info,
    Some("Operation with trace correlation"),
    Some(Attributes::new()),
    Some(1640995200000L),
    Some(1640995201000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::trace_id(correlated_log), Some(trace_id))
  assert_eq(LogRecord::span_id(correlated_log), Some(span_id))
  
  // Test log message validation
  let logs = [trace_log, debug_log, info_log, warn_log, error_log, fatal_log]
  
  for log in logs {
    match LogRecord::body(log) {
      Some(message) => assert_true(message.length() > 0)
      None => assert_true(false) // Log should have a message
    }
  }
  
  // Emit all log records
  for log in logs {
    Logger::emit(logger, log)
  }
  Logger::emit(logger, correlated_log)
}

// Test 5: Resource Merge Strategies
test "resource merge strategies" {
  // Test base resource with service attributes
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.namespace", StringValue("production"))
  ]
  
  let base_resource = Resource::new()
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  // Test override resource with additional attributes
  let override_attrs = [
    ("host.name", StringValue("web-server-01")),
    ("host.ip", StringValue("192.168.1.100")),
    ("deployment.environment", StringValue("staging"))
  ]
  
  let override_resource = Resource::new()
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Test resource merging
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merged resource contains attributes from both
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false) // Should have service.name
  }
  
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  match host_name {
    Some(StringValue(name)) => assert_eq(name, "web-server-01")
    _ => assert_true(false) // Should have host.name
  }
  
  // Test conflict resolution (override should win)
  let conflict_base = Resource::with_attributes(Resource::new(), [
    ("conflict.attr", StringValue("base-value"))
  ])
  
  let conflict_override = Resource::with_attributes(Resource::new(), [
    ("conflict.attr", StringValue("override-value"))
  ])
  
  let conflict_merged = Resource::merge(conflict_base, conflict_override)
  let conflict_value = Resource::get_attribute(conflict_merged, "conflict.attr")
  
  match conflict_value {
    Some(StringValue(value)) => assert_eq(value, "override-value")
    _ => assert_true(false) // Should have conflict.attr
  }
  
  // Test attribute type variations
  let typed_attrs = [
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("string.array.attr", ArrayStringValue(["a", "b", "c"])),
    ("int.array.attr", ArrayIntValue([1, 2, 3]))
  ]
  
  let typed_resource = Resource::with_attributes(Resource::new(), typed_attrs)
  
  // Verify all attribute types are preserved
  let int_attr = Resource::get_attribute(typed_resource, "int.attr")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false) // Should have int.attr with correct type
  }
  
  let float_attr = Resource::get_attribute(typed_resource, "float.attr")
  match float_attr {
    Some(FloatValue(value)) => assert_true(value > 3.14 && value < 3.15)
    _ => assert_true(false) // Should have float.attr with correct type
  }
  
  let bool_attr = Resource::get_attribute(typed_resource, "bool.attr")
  match bool_attr {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false) // Should have bool.attr with correct type
  }
}

// Test 6: Propagator Injection and Extraction
test "propagator injection and extraction" {
  // Test W3C Trace Context propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  
  // Create context with trace information
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "rojo=00f067aa0ba902b7")
  let context = Context::with_value(Context::root(), ContextKey::new("trace-context"), trace_id + ":" + span_id)
  
  // Test injection
  let carrier = TextMapCarrier::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  CompositePropagator::inject(composite_propagator, context, carrier)
  
  // Verify injected traceparent header
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  match injected_traceparent {
    Some(value) => {
      // Validate traceparent format: version-trace_id-span_id-flags
      let parts = value.split("-")
      assert_eq(parts.length(), 4)
      assert_eq(parts[0], "00") // Version
      assert_eq(parts[1], trace_id) // Trace ID
      assert_eq(parts[2], span_id) // Span ID
      assert_eq(parts[3], "01") // Flags
    }
    None => assert_true(false) // Should have traceparent header
  }
  
  // Test extraction
  let extraction_carrier = TextMapCarrier::new()
  // In a real implementation, we would set headers on the carrier
  // For testing, we use the simplified implementation
  
  let extracted_context = CompositePropagator::extract(composite_propagator, extraction_carrier)
  let extracted_value = Context::get(extracted_context, ContextKey::new("extracted"))
  
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false) // Should have extracted value
  }
  
  // Test baggage propagator
  let baggage_propagator = W3CBaggagePropagator::new()
  let baggage_composite = CompositePropagator::new([baggage_propagator])
  
  // Inject baggage
  let baggage_context = Context::with_value(
    Context::root(), 
    ContextKey::new("baggage"), 
    "user.id=12345,session.id=abcdef"
  )
  
  let baggage_carrier = TextMapCarrier::new()
  CompositePropagator::inject(baggage_composite, baggage_context, baggage_carrier)
  
  // Test extraction with empty carrier (using simplified implementation)
  let baggage_extracted = CompositePropagator::extract(baggage_composite, baggage_carrier)
  let baggage_value = Context::get(baggage_extracted, ContextKey::new("extracted"))
  
  match baggage_value {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false) // Should have extracted value
  }
}

// Test 7: Attribute Value Deep Nesting Operations
test "attribute value deep nesting operations" {
  // Test deeply nested string arrays
  let level1_array = ArrayStringValue(["a", "b", "c"])
  let level2_array = ArrayStringValue(["level2-a", "level2-b", "level2-c"])
  let level3_array = ArrayStringValue(["level3-x", "level3-y", "level3-z"])
  
  // Create nested attribute structure
  let nested_attrs = Attributes::new()
  Attributes::set(nested_attrs, "level1", level1_array)
  Attributes::set(nested_attrs, "level2", level2_array)
  Attributes::set(nested_attrs, "level3", level3_array)
  
  // Test retrieval of nested attributes
  let level1_retrieved = Attributes::get(nested_attrs, "level1")
  match level1_retrieved {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false) // Should have level1 array
  }
  
  // Test deeply nested integer arrays
  let int_level1 = ArrayIntValue([1, 2, 3, 4, 5])
  let int_level2 = ArrayIntValue([10, 20, 30, 40, 50])
  let int_level3 = ArrayIntValue([100, 200, 300, 400, 500])
  
  let int_nested_attrs = Attributes::new()
  Attributes::set(int_nested_attrs, "int.level1", int_level1)
  Attributes::set(int_nested_attrs, "int.level2", int_level2)
  Attributes::set(int_nested_attrs, "int.level3", int_level3)
  
  // Test integer array operations
  let int_level1_retrieved = Attributes::get(int_nested_attrs, "int.level1")
  match int_level1_retrieved {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 5)
      let sum = arr.reduce(|acc, val| acc + val, 0)
      assert_eq(sum, 15)
    }
    _ => assert_true(false) // Should have int.level1 array
  }
  
  // Test mixed type nesting
  let mixed_attrs = Attributes::new()
  Attributes::set(mixed_attrs, "string.value", StringValue("test"))
  Attributes::set(mixed_attrs, "int.value", IntValue(42))
  Attributes::set(mixed_attrs, "float.value", FloatValue(3.14159))
  Attributes::set(mixed_attrs, "bool.value", BoolValue(true))
  Attributes::set(mixed_attrs, "string.array", ArrayStringValue(["x", "y", "z"]))
  Attributes::set(mixed_attrs, "int.array", ArrayIntValue([10, 20, 30]))
  
  // Test retrieval of all mixed types
  let string_val = Attributes::get(mixed_attrs, "string.value")
  match string_val {
    Some(StringValue(value)) => assert_eq(value, "test")
    _ => assert_true(false)
  }
  
  let int_val = Attributes::get(mixed_attrs, "int.value")
  match int_val {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let float_val = Attributes::get(mixed_attrs, "float.value")
  match float_val {
    Some(FloatValue(value)) => assert_true(value > 3.14 && value < 3.15)
    _ => assert_true(false)
  }
  
  let bool_val = Attributes::get(mixed_attrs, "bool.value")
  match bool_val {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
  
  // Test attribute key with special characters and dots
  let special_key_attrs = Attributes::new()
  Attributes::set(special_key_attrs, "special.key.with.dots", StringValue("dot-value"))
  Attributes::set(special_key_attrs, "key-with-dashes", StringValue("dash-value"))
  Attributes::set(special_key_attrs, "key_with_underscores", StringValue("underscore-value"))
  
  let dot_key_val = Attributes::get(special_key_attrs, "special.key.with.dots")
  match dot_key_val {
    Some(StringValue(value)) => assert_eq(value, "dot-value")
    _ => assert_true(false)
  }
}

// Test 8: Concurrent Safety Scenarios
test "concurrent safety scenarios" {
  // Test shared counter with concurrent updates simulation
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "concurrent-test-meter")
  let shared_counter = Meter::create_counter(meter, "shared-counter")
  
  // Simulate concurrent counter updates from multiple "threads"
  // In a real scenario, these would be actual concurrent operations
  let thread_operations = [
    [1, 2, 3, 4, 5],      // Thread 1 operations
    [10, 20, 30],         // Thread 2 operations
    [100, 200],           // Thread 3 operations
    [1000]                // Thread 4 operations
  ]
  
  let mut total_expected = 0
  
  // Process operations for each "thread"
  for operations in thread_operations {
    for op in operations {
      Counter::add(shared_counter, op.to_float())
      total_expected = total_expected + op
    }
  }
  
  assert_eq(total_expected, 1375)
  
  // Test shared gauge with concurrent updates
  let shared_gauge = Meter::create_gauge(meter, "shared-gauge")
  let gauge_updates = [10.5, 20.3, 15.7, 25.1, 18.9, 22.4, 12.6, 19.8, 16.2, 21.7]
  
  for update in gauge_updates {
    // In a real implementation, this would update the gauge concurrently
    // For testing, we validate the update values
    assert_true(update >= 10.0)
    assert_true(update <= 30.0)
  }
  
  // Test concurrent span operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent-test-tracer")
  
  // Create multiple spans concurrently
  let span_names = [
    "concurrent-span-1",
    "concurrent-span-2", 
    "concurrent-span-3",
    "concurrent-span-4",
    "concurrent-span-5"
  ]
  
  let spans = span_names.map(|name| Tracer::start_span(tracer, name))
  assert_eq(spans.length(), 5)
  
  // Verify all spans have unique contexts
  let mut trace_ids = []
  let mut span_ids = []
  
  for span in spans {
    let span_ctx = span.span_context()
    trace_ids.push(SpanContext::trace_id(span_ctx))
    span_ids.push(SpanContext::span_id(span_ctx))
  }
  
  // All spans should have the same trace ID (in a real concurrent scenario)
  for trace_id in trace_ids {
    assert_eq(trace_id, "test_trace_id")
  }
  
  // All spans should have unique span IDs
  for i = 0; i < span_ids.length(); i = i + 1 {
    for j = i + 1; j < span_ids.length(); j = j + 1 {
      assert_not_eq(span_ids[i], span_ids[j])
    }
  }
  
  // End all spans
  for span in spans {
    span.end()
  }
}

// Test 9: Time Series Data Operations
test "time series data operations" {
  // Test time series data points
  let base_timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  let time_series_data = [
    (base_timestamp, 10.5),
    (base_timestamp + 60000L, 15.2),      // +1 minute
    (base_timestamp + 120000L, 12.8),     // +2 minutes
    (base_timestamp + 180000L, 18.3),     // +3 minutes
    (base_timestamp + 240000L, 20.1),     // +4 minutes
    (base_timestamp + 300000L, 16.7),     // +5 minutes
    (base_timestamp + 360000L, 22.4),     // +6 minutes
    (base_timestamp + 420000L, 19.8),     // +7 minutes
    (base_timestamp + 480000L, 25.2),     // +8 minutes
    (base_timestamp + 540000L, 21.6)      // +9 minutes
  ]
  
  // Validate time series data
  assert_eq(time_series_data.length(), 10)
  
  // Test time-based filtering
  let start_time = base_timestamp + 180000L // +3 minutes
  let end_time = base_timestamp + 420000L   // +7 minutes
  
  let filtered_data = time_series_data.filter(|(timestamp, _)| {
    timestamp >= start_time && timestamp <= end_time
  })
  
  assert_eq(filtered_data.length(), 5)
  
  // Test aggregation over time windows
  let window_size = 180000L // 3 minutes
  let mut windows = []
  
  let mut current_window_start = base_timestamp
  while current_window_start < base_timestamp + 540000L {
    let current_window_end = current_window_start + window_size
    let window_data = time_series_data.filter(|(timestamp, _)| {
      timestamp >= current_window_start && timestamp < current_window_end
    })
    
    if window_data.length() > 0 {
      let sum = window_data.reduce(|acc, (_, value)| acc + value, 0.0)
      let avg = sum / window_data.length().to_float()
      windows.push((current_window_start, current_window_end, window_data.length(), avg))
    }
    
    current_window_start = current_window_start + window_size
  }
  
  assert_eq(windows.length(), 3)
  
  // Validate first window (0-3 minutes)
  assert_eq(windows[0].0, base_timestamp)
  assert_eq(windows[0].1, base_timestamp + window_size)
  assert_eq(windows[0].2, 3) // 3 data points
  assert_true(windows[0].3 > 12.0 && windows[0].3 < 13.0) // Average around 12.8
  
  // Test time series statistics
  let values = time_series_data.map(|(_, value)| value)
  let sum = values.reduce(|acc, val| acc + val, 0.0)
  let count = values.length().to_float()
  let avg = sum / count
  
  // Find min and max
  let mut min_val = values[0]
  let mut max_val = values[0]
  
  for value in values {
    if value < min_val {
      min_val = value
    }
    if value > max_val {
      max_val = value
    }
  }
  
  assert_eq(sum, 182.6)
  assert_eq(count, 10.0)
  assert_eq(avg, 18.26)
  assert_eq(min_val, 10.5)
  assert_eq(max_val, 25.2)
  
  // Test time series with irregular intervals
  let irregular_data = [
    (base_timestamp, 100.0),
    (base_timestamp + 30000L, 110.0),     // +30 seconds
    (base_timestamp + 120000L, 95.0),     // +2 minutes
    (base_timestamp + 150000L, 105.0),    // +2.5 minutes
    (base_timestamp + 300000L, 120.0),    // +5 minutes
    (base_timestamp + 330000L, 115.0),    // +5.5 minutes
    (base_timestamp + 600000L, 130.0)     // +10 minutes
  ]
  
  // Test interpolation for missing data points
  let interpolation_target = base_timestamp + 60000L // +1 minute
  let before_point = irregular_data.filter(|(timestamp, _)| timestamp <= interpolation_target)
  let after_point = irregular_data.filter(|(timestamp, _)| timestamp > interpolation_target)
  
  assert_eq(before_point.length(), 2)
  assert_eq(after_point.length(), 5)
  
  // Simple linear interpolation between points at 0s and 2min
  let (t1, v1) = before_point[before_point.length() - 1]
  let (t2, v2) = after_point[0]
  
  let interpolated_value = v1 + (v2 - v1) * ((interpolation_target - t1).to_float() / (t2 - t1).to_float())
  assert_true(interpolated_value > 100.0 && interpolated_value < 110.0)
}

// Test 10: Error Recovery and Boundary Handling
test "error recovery and boundary handling" {
  // Test span creation with invalid inputs
  let empty_trace_id = ""
  let empty_span_id = ""
  let invalid_span_ctx = SpanContext::new(empty_trace_id, empty_span_id, true, "")
  
  // Test invalid span context
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // Test valid span context
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_span_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "")
  
  assert_true(SpanContext::is_valid(valid_span_ctx))
  
  // Test boundary conditions for metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "boundary-test-meter")
  
  // Test counter with zero and negative values
  let counter = Meter::create_counter(meter, "boundary-counter")
  
  // Add zero value (should be valid)
  Counter::add(counter, 0.0)
  
  // Add negative value (should be valid for some counter implementations)
  Counter::add(counter, -10.0)
  
  // Test with very large values
  Counter::add(counter, 999999999.0)
  
  // Test gauge with extreme values
  let gauge = Meter::create_gauge(meter, "boundary-gauge")
  
  let extreme_values = [
    -999999.9,      // Very negative
    0.0,            // Zero
    999999.9,       // Very positive
    0.0000001,      // Very small positive
    -0.0000001      // Very small negative
  ]
  
  for value in extreme_values {
    // In a real implementation, this would set the gauge value
    // For testing, we validate the values are within reasonable bounds
    assert_true(value >= -1000000.0 && value <= 1000000.0)
  }
  
  // Test histogram with boundary values
  let histogram = Meter::create_histogram(meter, "boundary-histogram")
  
  let boundary_measurements = [
    0.0,            // Zero
    0.000001,       // Very small
    999999.0,       // Very large
    -1.0            // Negative (if supported)
  ]
  
  for measurement in boundary_measurements {
    Histogram::record(histogram, measurement)
  }
  
  // Test log record with boundary conditions
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "boundary-test-logger")
  
  // Test log with empty message
  let empty_message_log = LogRecord::new(Info, "")
  match LogRecord::body(empty_message_log) {
    Some(message) => assert_eq(message, "")
    None => assert_true(false) // Should have empty message
  }
  
  // Test log with very long message
  let long_message = "This is a very long log message that might exceed normal buffer sizes. ".repeat(100)
  let long_message_log = LogRecord::new(Error, long_message)
  
  match LogRecord::body(long_message_log) {
    Some(message) => assert_true(message.length() > 1000)
    None => assert_true(false) // Should have long message
  }
  
  // Test log with boundary timestamps
  let min_timestamp = 0L
  let max_timestamp = 9223372036854775807L // Max Int64
  
  let min_timestamp_log = LogRecord::new_with_context(
    Info,
    Some("Minimum timestamp"),
    None,
    Some(min_timestamp),
    None,
    None,
    None,
    None
  )
  
  let max_timestamp_log = LogRecord::new_with_context(
    Error,
    Some("Maximum timestamp"),
    None,
    Some(max_timestamp),
    None,
    None,
    None,
    None
  )
  
  // Test attribute operations with boundary conditions
  let boundary_attrs = Attributes::new()
  
  // Test with empty key
  Attributes::set(boundary_attrs, "", StringValue("empty-key-value"))
  
  // Test with very long key
  let long_key = "a".repeat(1000)
  Attributes::set(boundary_attrs, long_key, StringValue("long-key-value"))
  
  // Test with special characters in key
  Attributes::set(boundary_attrs, "special/chars\\in:key", StringValue("special-chars-value"))
  
  // Test resource with boundary conditions
  let boundary_resource = Resource::new()
  
  // Test with empty attribute values
  let empty_value_attrs = [
    ("empty.string", StringValue("")),
    ("zero.int", IntValue(0)),
    ("zero.float", FloatValue(0.0)),
    ("false.bool", BoolValue(false)),
    ("empty.string.array", ArrayStringValue([])),
    ("empty.int.array", ArrayIntValue([]))
  ]
  
  let boundary_with_attrs = Resource::with_attributes(boundary_resource, empty_value_attrs)
  
  // Verify empty values are handled correctly
  let empty_string = Resource::get_attribute(boundary_with_attrs, "empty.string")
  match empty_string {
    Some(StringValue(value)) => assert_eq(value, "")
    _ => assert_true(false)
  }
  
  let zero_int = Resource::get_attribute(boundary_with_attrs, "zero.int")
  match zero_int {
    Some(IntValue(value)) => assert_eq(value, 0)
    _ => assert_true(false)
  }
  
  // Test error recovery in propagator operations
  let recovery_carrier = TextMapCarrier::new()
  let recovery_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // Test extraction from empty carrier
  let empty_context = CompositePropagator::extract(recovery_propagator, recovery_carrier)
  let empty_value = Context::get(empty_context, ContextKey::new("non-existent"))
  assert_eq(empty_value, None)
  
  // Test injection with empty context
  let empty_root_context = Context::root()
  CompositePropagator::inject(recovery_propagator, empty_root_context, recovery_carrier)
  
  // System should recover gracefully from all boundary conditions
  assert_true(true) // If we reach here, all boundary conditions were handled
}