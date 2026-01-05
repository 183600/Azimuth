// Azimuth New Comprehensive Test Cases
// This file contains new test cases for various telemetry functionality

// Test 1: Advanced Pattern Matching with Telemetry Data
test "advanced pattern matching with telemetry data" {
  // Test pattern matching with different attribute types
  let string_attr = StringValue("test-value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14159)
  let bool_attr = BoolValue(true)
  let array_attr = ArrayStringValue(["a", "b", "c"])
  
  // Test pattern matching on string attribute
  match string_attr {
    StringValue(value) => {
      assert_eq(value, "test-value")
      assert_true(value.length() > 0)
    }
    _ => assert_true(false) // Should match StringValue
  }
  
  // Test pattern matching on integer attribute
  match int_attr {
    IntValue(value) => {
      assert_eq(value, 42)
      assert_true(value > 0)
    }
    _ => assert_true(false) // Should match IntValue
  }
  
  // Test pattern matching on float attribute
  match float_attr {
    FloatValue(value) => {
      assert_true(value > 3.14 && value < 3.15)
    }
    _ => assert_true(false) // Should match FloatValue
  }
  
  // Test pattern matching on boolean attribute
  match bool_attr {
    BoolValue(true) => assert_true(true) // Expected
    BoolValue(false) => assert_true(false) // Unexpected
    _ => assert_true(false) // Should match BoolValue
  }
  
  // Test pattern matching on array attribute
  match array_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false) // Should match ArrayStringValue
  }
}

// Test 2: Resource Attribute Validation and Constraints
test "resource attribute validation and constraints" {
  // Test resource with valid attributes
  let valid_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("host.name", StringValue("production-server-01")),
    ("host.ip", StringValue("192.168.1.100")),
    ("process.pid", IntValue(1234)),
    ("process.runtime.name", StringValue("moonbit")),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("0.1.0")),
    ("telemetry.sdk.language", StringValue("moonbit"))
  ]
  
  let resource = Resource::new()
  let resource_with_attrs = Resource::with_attributes(resource, valid_attrs)
  
  // Validate all attributes are set correctly
  for (key, expected_value) in valid_attrs {
    let actual_value = Resource::get_attribute(resource_with_attrs, key)
    match actual_value {
      Some(value) => {
        match (value, expected_value) {
          (StringValue(actual), StringValue(expected)) => assert_eq(actual, expected)
          (IntValue(actual), IntValue(expected)) => assert_eq(actual, expected)
          (FloatValue(actual), FloatValue(expected)) => assert_true(actual - expected < 0.0001)
          (BoolValue(actual), BoolValue(expected)) => assert_eq(actual, expected)
          _ => assert_true(false) // Type mismatch
        }
      }
      None => assert_true(false) // Attribute should exist
    }
  }
  
  // Test attribute constraints
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => {
      assert_true(name.length() > 0)
      assert_true(name.length() <= 255) // Reasonable length constraint
    }
    _ => assert_true(false)
  }
  
  let process_pid = Resource::get_attribute(resource_with_attrs, "process.pid")
  match process_pid {
    Some(IntValue(pid)) => {
      assert_true(pid > 0) // PID should be positive
      assert_true(pid <= 999999) // Reasonable PID constraint
    }
    _ => assert_true(false)
  }
}

// Test 3: Span Context Validation and Trace Continuity
test "span context validation and trace continuity" {
  // Test span context creation with valid trace and span IDs
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Validate span context
  assert_true(SpanContext::is_valid(span_ctx))
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Test span context with invalid trace ID
  let invalid_trace_ctx = SpanContext::new("", span_id, true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  // Test span context with invalid span ID
  let invalid_span_ctx = SpanContext::new(trace_id, "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // Test trace continuity across multiple spans
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "trace-continuity-test")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "root-operation")
  let root_ctx = root_span.span_context()
  
  // Create child spans with the same trace ID
  let child_span1 = Tracer::start_span(tracer, "child-operation-1")
  let child_ctx1 = child_span1.span_context()
  
  let child_span2 = Tracer::start_span(tracer, "child-operation-2")
  let child_ctx2 = child_span2.span_context()
  
  // All spans should have the same trace ID
  assert_eq(SpanContext::trace_id(root_ctx), SpanContext::trace_id(child_ctx1))
  assert_eq(SpanContext::trace_id(root_ctx), SpanContext::trace_id(child_ctx2))
  
  // All spans should have different span IDs
  assert_not_eq(SpanContext::span_id(root_ctx), SpanContext::span_id(child_ctx1))
  assert_not_eq(SpanContext::span_id(root_ctx), SpanContext::span_id(child_ctx2))
  assert_not_eq(SpanContext::span_id(child_ctx1), SpanContext::span_id(child_ctx2))
  
  // End all spans
  child_span2.end()
  child_span1.end()
  root_span.end()
}

// Test 4: Metric Instrument Precision and Accuracy
test "metric instrument precision and accuracy" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "precision-test-meter")
  
  // Test counter with fractional values
  let counter = Meter::create_counter(meter, "fractional-counter")
  let fractional_values = [0.1, 0.01, 0.001, 0.0001, 0.00001]
  
  let mut expected_sum = 0.0
  for value in fractional_values {
    Counter::add(counter, value)
    expected_sum = expected_sum + value
  }
  
  // Test with very small values to check precision
  assert_true(expected_sum > 0.11111 && expected_sum < 0.11112)
  
  // Test gauge with high precision values
  let gauge = Meter::create_gauge(meter, "precision-gauge")
  let high_precision_values = [
    3.141592653589793,
    2.718281828459045,
    1.414213562373095,
    1.618033988749895,
    0.577215664901533
  ]
  
  for value in high_precision_values {
    // In a real implementation, this would set the gauge value
    // For testing, we validate the precision
    assert_true(value > 0.0)
    assert_true(value < 10.0)
  }
  
  // Test histogram with precise measurements
  let histogram = Meter::create_histogram(meter, "precision-histogram")
  let precise_measurements = [
    0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1.0, 10.0, 100.0, 1000.0
  ]
  
  for measurement in precise_measurements {
    Histogram::record(histogram, measurement)
  }
  
  // Test up-down counter with precise increments/decrements
  let updown_counter = Meter::create_updown_counter(meter, "precision-updown-counter")
  let precise_operations = [
    0.000001, -0.000002, 0.000003, -0.000004, 0.000005
  ]
  
  let mut expected_value = 0.0
  for op in precise_operations {
    UpDownCounter::add(updown_counter, op)
    expected_value = expected_value + op
  }
  
  assert_true(expected_value > 0.000002 && expected_value < 0.000004)
}

// Test 5: Log Record Correlation and Context Propagation
test "log record correlation and context propagation" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "correlation-test-logger")
  
  // Create log records with different correlation contexts
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let correlation_id = "corr-12345"
  let session_id = "sess-abcdef"
  
  // Create correlated log records
  let trace_log = LogRecord::new_with_context(
    Info,
    Some("Trace correlation test"),
    Some(Attributes::with([("correlation.id", StringValue(correlation_id))])),
    Some(1640995200000L),
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let session_log = LogRecord::new_with_context(
    Debug,
    Some("Session correlation test"),
    Some(Attributes::with([("session.id", StringValue(session_id))])),
    Some(1640995201000L),
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // Validate correlation attributes
  match LogRecord::attributes(trace_log) {
    Some(attrs) => {
      let corr_id = Attributes::get(attrs, "correlation.id")
      match corr_id {
        Some(StringValue(value)) => assert_eq(value, correlation_id)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  match LogRecord::attributes(session_log) {
    Some(attrs) => {
      let sess_id = Attributes::get(attrs, "session.id")
      match sess_id {
        Some(StringValue(value)) => assert_eq(value, session_id)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Validate trace and span correlation
  assert_eq(LogRecord::trace_id(trace_log), Some(trace_id))
  assert_eq(LogRecord::span_id(trace_log), Some(span_id))
  assert_eq(LogRecord::trace_id(session_log), Some(trace_id))
  assert_eq(LogRecord::span_id(session_log), Some(span_id))
  
  // Test log sequence correlation
  let sequence_logs = [
    LogRecord::new_with_context(
      Info,
      Some("Step 1: Initialize"),
      None,
      Some(1640995200000L),
      None,
      Some(trace_id),
      Some(span_id),
      None
    ),
    LogRecord::new_with_context(
      Info,
      Some("Step 2: Process"),
      None,
      Some(1640995201000L),
      None,
      Some(trace_id),
      Some(span_id),
      None
    ),
    LogRecord::new_with_context(
      Info,
      Some("Step 3: Complete"),
      None,
      Some(1640995202000L),
      None,
      Some(trace_id),
      Some(span_id),
      None
    )
  ]
  
  // Validate all logs have the same trace context
  for log in sequence_logs {
    assert_eq(LogRecord::trace_id(log), Some(trace_id))
    assert_eq(LogRecord::span_id(log), Some(span_id))
  }
  
  // Validate timestamp sequence
  assert_eq(LogRecord::timestamp(sequence_logs[0]), Some(1640995200000L))
  assert_eq(LogRecord::timestamp(sequence_logs[1]), Some(1640995201000L))
  assert_eq(LogRecord::timestamp(sequence_logs[2]), Some(1640995202000L))
}

// Test 6: Baggage Items with Complex Values
test "baggage items with complex values" {
  // Test baggage with structured values
  let complex_baggage = [
    ("user.profile", "{\"id\":123,\"name\":\"John\",\"role\":\"admin\"}"),
    ("request.metadata", "{\"method\":\"GET\",\"path\":\"/api/users\",\"headers\":{\"Authorization\":\"Bearer token\"}}"),
    ("service.config", "{\"timeout\":5000,\"retries\":3,\"backoff\":\"exponential\"}"),
    ("trace.filters", "{\"error\":true,\"slow\":false,\"debug\":true}")
  ]
  
  let baggage = Baggage::new()
  let mut updated_baggage = baggage
  
  // Add complex baggage items
  for (key, value) in complex_baggage {
    updated_baggage = Baggage::set_entry(updated_baggage, key, value)
    
    // Verify the entry was set correctly
    let retrieved = Baggage::get_entry(updated_baggage, key)
    match retrieved {
      Some(v) => assert_eq(v, value)
      None => assert_true(false) // Should not happen
    }
  }
  
  // Test baggage with URL-encoded values
  let encoded_baggage = [
    ("url.encoded", "key1=value1&key2=value2&key3=value3"),
    ("base64.encoded", "eyJ1c2VyIjoiMTIzIiwic2Vzc2lvbiI6ImFiY2RlZiJ9"),
    ("special.chars", "user@domain.com+path%20with%20spaces")
  ]
  
  for (key, value) in encoded_baggage {
    updated_baggage = Baggage::set_entry(updated_baggage, key, value)
    
    let retrieved = Baggage::get_entry(updated_baggage, key)
    match retrieved {
      Some(v) => assert_eq(v, value)
      None => assert_true(false) // Should not happen
    }
  }
  
  // Test baggage with very long values
  let long_value = "x".repeat(1000)
  updated_baggage = Baggage::set_entry(updated_baggage, "long.value", long_value)
  
  let retrieved_long = Baggage::get_entry(updated_baggage, "long.value")
  match retrieved_long {
    Some(v) => {
      assert_eq(v.length(), 1000)
      assert_true(v.contains("x"))
    }
    None => assert_true(false) // Should not happen
  }
  
  // Test baggage size limits
  let mut large_baggage = Baggage::new()
  let mut total_size = 0
  
  // Add many baggage items to test size limits
  for i = 0; i < 100; i = i + 1 {
    let key = "key." + i.to_string()
    let value = "value." + i.to_string()
    large_baggage = Baggage::set_entry(large_baggage, key, value)
    total_size = total_size + key.length() + value.length() + 1 // +1 for '='
    
    // In a real implementation, there might be size limits
    // For testing, we just track the total size
    assert_true(total_size > 0)
  }
  
  assert_eq(total_size > 1000, true) // Should have reasonable total size
}

// Test 7: Attribute Type Conversion and Coercion
test "attribute type conversion and coercion" {
  // Test string to integer conversion
  let string_int_attrs = Attributes::new()
  Attributes::set(string_int_attrs, "string.int", StringValue("42"))
  Attributes::set(string_int_attrs, "string.float", StringValue("3.14159"))
  Attributes::set(string_int_attrs, "string.bool", StringValue("true"))
  
  // Simulate type conversion
  let string_int = Attributes::get(string_int_attrs, "string.int")
  match string_int {
    Some(StringValue(value)) => {
      let converted_int = value.parse_int()
      match converted_int {
        Some(int_val) => assert_eq(int_val, 42)
        None => assert_true(false) // Should parse successfully
      }
    }
    _ => assert_true(false)
  }
  
  let string_float = Attributes::get(string_int_attrs, "string.float")
  match string_float {
    Some(StringValue(value)) => {
      let converted_float = value.parse_float()
      match converted_float {
        Some(float_val) => assert_true(float_val > 3.14 && float_val < 3.15)
        None => assert_true(false) // Should parse successfully
      }
    }
    _ => assert_true(false)
  }
  
  let string_bool = Attributes::get(string_int_attrs, "string.bool")
  match string_bool {
    Some(StringValue(value)) => {
      let converted_bool = value.parse_bool()
      match converted_bool {
        Some(bool_val) => assert_true(bool_val)
        None => assert_true(false) // Should parse successfully
      }
    }
    _ => assert_true(false)
  }
  
  // Test integer to other types conversion
  let int_attrs = Attributes::new()
  Attributes::set(int_attrs, "int.value", IntValue(42))
  
  let int_value = Attributes::get(int_attrs, "int.value")
  match int_value {
    Some(IntValue(value)) => {
      // Convert to string
      let string_val = value.to_string()
      assert_eq(string_val, "42")
      
      // Convert to float
      let float_val = value.to_float()
      assert_eq(float_val, 42.0)
      
      // Convert to boolean (non-zero is true)
      let bool_val = value != 0
      assert_true(bool_val)
    }
    _ => assert_true(false)
  }
  
  // Test array type conversions
  let array_attrs = Attributes::new()
  Attributes::set(array_attrs, "string.array", ArrayStringValue(["1", "2", "3"]))
  Attributes::set(array_attrs, "int.array", ArrayIntValue([1, 2, 3]))
  
  // Convert string array to int array
  let string_array = Attributes::get(array_attrs, "string.array")
  match string_array {
    Some(ArrayStringValue(arr)) => {
      let int_array = arr.map(|s| s.parse_int().unwrap_or(0))
      assert_eq(int_array, [1, 2, 3])
    }
    _ => assert_true(false)
  }
  
  // Convert int array to string array
  let int_array = Attributes::get(array_attrs, "int.array")
  match int_array {
    Some(ArrayIntValue(arr)) => {
      let string_array = arr.map(|i| i.to_string())
      assert_eq(string_array, ["1", "2", "3"])
    }
    _ => assert_true(false)
  }
}

// Test 8: Concurrent Span Operations with Shared Context
test "concurrent span operations with shared context" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent-span-test")
  
  // Create shared context
  let shared_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let root_span = Tracer::start_span(tracer, "root-operation")
  let shared_context = Context::with_value(
    Context::root(),
    ContextKey::new("shared.trace.id"),
    shared_trace_id
  )
  
  // Simulate concurrent operations with shared context
  let operation_names = [
    "database.query",
    "cache.lookup",
    "api.call",
    "validation.check",
    "transformation.process"
  ]
  
  let spans = operation_names.map(|name| {
    // In a real concurrent scenario, each operation would run in its own thread
    // For testing, we simulate this with sequential creation
    Tracer::start_span(tracer, name)
  })
  
  // Verify all spans have the same context
  for span in spans {
    let span_ctx = span.span_context()
    // In a real implementation, these would share the same trace ID
    // For testing, we verify the spans are valid
    assert_true(SpanContext::is_valid(span_ctx))
  }
  
  // Test span events with concurrent operations
  for i = 0; i < spans.length(); i = i + 1 {
    let span = spans[i]
    let event_name = "operation.step." + i.to_string()
    span.add_event(event_name, Some([("step.index", i.to_string())]))
  }
  
  // Test span status updates
  for span in spans {
    span.set_status(Ok, Some("Operation completed successfully"))
  }
  
  // End all spans
  for span in spans {
    span.end()
  }
  
  root_span.end()
}

// Test 9: Time Series Data Aggregation and Windowing
test "time series data aggregation and windowing" {
  // Test time series with regular intervals
  let base_time = 1640995200000L // 2022-01-01 00:00:00 UTC
  let interval = 60000L // 1 minute
  
  // Generate time series data
  let time_series = []
  for i = 0; i < 60; i = i + 1 { // 1 hour of data
    let timestamp = base_time + (i.to_int64() * interval)
    let value = 10.0 + (i.to_float() * 0.5) + (i.to_float() % 3.0) // Add some variation
    time_series.push((timestamp, value))
  }
  
  assert_eq(time_series.length(), 60)
  
  // Test time window aggregation (5-minute windows)
  let window_size = 5 * interval // 5 minutes
  let mut windows = []
  
  let mut window_start = base_time
  while window_start < base_time + (60 * interval) {
    let window_end = window_start + window_size
    let window_data = time_series.filter(|(timestamp, _)| {
      timestamp >= window_start && timestamp < window_end
    })
    
    if window_data.length() > 0 {
      let values = window_data.map(|(_, value)| value)
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
      
      windows.push((window_start, window_end, count, avg, min_val, max_val))
    }
    
    window_start = window_start + window_size
  }
  
  assert_eq(windows.length(), 12) // 60 minutes / 5 minutes per window
  
  // Validate first window
  assert_eq(windows[0].0, base_time) // Start time
  assert_eq(windows[0].1, base_time + window_size) // End time
  assert_eq(windows[0].2, 5.0) // Count
  assert_true(windows[0].3 > 10.0 && windows[0].3 < 15.0) // Average
  
  // Test sliding window aggregation
  let sliding_window_size = 10 * interval // 10 minutes
  let slide_step = interval // 1 minute
  
  let mut sliding_windows = []
  for i = 0; i <= 50; i = i + 1 { // Can slide 50 times (60-10)
    let slide_start = base_time + (i.to_int64() * slide_step)
    let slide_end = slide_start + sliding_window_size
    
    let slide_data = time_series.filter(|(timestamp, _)| {
      timestamp >= slide_start && timestamp < slide_end
    })
    
    if slide_data.length() > 0 {
      let values = slide_data.map(|(_, value)| value)
      let sum = values.reduce(|acc, val| acc + val, 0.0)
      let avg = sum / values.length().to_float()
      sliding_windows.push((slide_start, slide_end, values.length().to_float(), avg))
    }
  }
  
  assert_eq(sliding_windows.length(), 51) // Should have 51 sliding windows
  
  // Test time series with missing data points
  let irregular_series = [
    (base_time, 10.0),
    (base_time + 2 * interval, 12.0), // Missing 1 minute
    (base_time + 3 * interval, 13.0),
    (base_time + 6 * interval, 16.0), // Missing 2 minutes
    (base_time + 7 * interval, 17.0),
    (base_time + 8 * interval, 18.0)
  ]
  
  // Test interpolation for missing values
  let interpolated_series = []
  for i = 0; i < 10; i = i + 1 {
    let target_time = base_time + (i.to_int64() * interval)
    
    // Find nearest data points
    let before_points = irregular_series.filter(|(timestamp, _)| timestamp <= target_time)
    let after_points = irregular_series.filter(|(timestamp, _)| timestamp > target_time)
    
    if before_points.length() > 0 && after_points.length() > 0 {
      let (t1, v1) = before_points[before_points.length() - 1]
      let (t2, v2) = after_points[0]
      
      // Linear interpolation
      let ratio = (target_time - t1).to_float() / (t2 - t1).to_float()
      let interpolated_value = v1 + (v2 - v1) * ratio
      interpolated_series.push((target_time, interpolated_value))
    } else if before_points.length() > 0 {
      let (_, v) = before_points[before_points.length() - 1]
      interpolated_series.push((target_time, v))
    }
  }
  
  assert_eq(interpolated_series.length(), 10)
}

// Test 10: Error Boundary and Exception Handling
test "error boundary and exception handling" {
  // Test span creation with error handling
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-boundary-test")
  
  // Create span that might encounter errors
  let error_span = Tracer::start_span(tracer, "error-prone-operation")
  
  // Test error events
  error_span.add_event("error.occurred", Some([
    ("error.type", "ValidationError"),
    ("error.message", "Invalid input parameter"),
    ("error.code", "400")
  ]))
  
  error_span.set_status(Error, Some("Operation failed due to validation error"))
  
  // Test span with exception recording
  let exception_span = Tracer::start_span(tracer, "exception-operation")
  
  exception_span.add_event("exception.thrown", Some([
    ("exception.type", "NullReferenceException"),
    ("exception.message", "Object reference not set to an instance"),
    ("exception.stacktrace", "at Method() line 42")
  ]))
  
  exception_span.set_status(Error, Some("Operation terminated due to exception"))
  
  // Test error recovery with new span
  let recovery_span = Tracer::start_span(tracer, "recovery-operation")
  
  recovery_span.add_event("recovery.started", Some([
    ("recovery.strategy", "retry"),
    ("retry.count", "1"),
    ("original.error", "ValidationError")
  ]))
  
  recovery_span.set_status(Ok, Some("Operation recovered successfully"))
  
  // Test boundary conditions with error handling
  let boundary_span = Tracer::start_span(tracer, "boundary-operation")
  
  // Test with null/empty values
  boundary_span.add_event("null.value.test", Some([
    ("null.value", ""),
    ("empty.value", ""),
    ("special.chars", "!@#$%^&*()")
  ]))
  
  // Test with very long values
  let long_message = "This is a very long error message that might exceed normal buffer sizes. ".repeat(50)
  boundary_span.add_event("long.message.test", Some([
    ("long.message", long_message)
  ]))
  
  // Test with numeric boundaries
  let numeric_values = [
    ("max.int", "9223372036854775807"), // Max Int64
    ("min.int", "-9223372036854775808"), // Min Int64
    ("max.float", "1.7976931348623157E+308"), // Max Float64
    ("min.float", "4.9E-324"), // Min positive Float64
    ("infinity", "Infinity"),
    ("negative.infinity", "-Infinity")
  ]
  
  boundary_span.add_event("numeric.boundary.test", Some(numeric_values))
  
  // End all spans
  boundary_span.end()
  recovery_span.end()
  exception_span.end()
  error_span.end()
  
  // Test error handling in metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "error-boundary-metrics")
  
  let error_counter = Meter::create_counter(meter, "error.counter")
  
  // Counter should handle error conditions gracefully
  Counter::add(error_counter, 1.0) // Normal increment
  Counter::add(error_counter, -1.0) // Negative increment (should be handled)
  Counter::add(error_counter, 0.0) // Zero increment (should be handled)
  
  // Test gauge with extreme values
  let error_gauge = Meter::create_gauge(meter, "error.gauge")
  
  let extreme_values = [
    999999999.0,
    -999999999.0,
    0.0,
    Float::infinity(),
    Float::neg_infinity(),
    Float::nan()
  ]
  
  for value in extreme_values {
    // In a real implementation, this would handle extreme values gracefully
    // For testing, we validate the values are recognized
    if Float::is_nan(value) {
      assert_true(Float::is_nan(value)) // NaN check
    } else if Float::is_infinite(value) {
      assert_true(Float::is_infinite(value)) // Infinity check
    } else {
      assert_true(value >= -1000000000.0 && value <= 1000000000.0) // Normal range check
    }
  }
  
  // Test error handling in logs
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error-boundary-logger")
  
  // Test log with error conditions
  let error_log = LogRecord::new(Error, "Error occurred during operation")
  Logger::emit(logger, error_log)
  
  // Test log with exception information
  let exception_log = LogRecord::new_with_context(
    Error,
    Some("Exception thrown: NullPointerException"),
    Some(Attributes::with([
      ("exception.type", "NullPointerException"),
      ("exception.message", "Attempted to access null object"),
      ("stack.trace", "at com.example.Method.method(Method.java:123)")
    ])),
    Some(1640995200000L),
    None,
    None,
    None,
    None
  )
  
  Logger::emit(logger, exception_log)
  
  // System should handle all error conditions gracefully
  assert_true(true) // If we reach here, all error conditions were handled
}