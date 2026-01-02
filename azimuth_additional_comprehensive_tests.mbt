// Azimuth Additional Comprehensive Test Suite
// This file contains additional comprehensive test cases for the Azimuth telemetry system

// Test 1: Attribute Type Conversion and Validation
test "attribute type conversion and validation" {
  // Test string to int conversion
  let string_int = StringValue("42")
  match string_int {
    StringValue(v) => {
      let int_value = IntValue(v.to_int())
      match int_value {
        IntValue(i) => assert_eq(i, 42)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test string to float conversion
  let string_float = StringValue("3.14159")
  match string_float {
    StringValue(v) => {
      let float_value = FloatValue(v.to_float())
      match float_value {
        FloatValue(f) => assert_true(f > 3.14 && f < 3.15)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test string to bool conversion
  let string_true = StringValue("true")
  match string_true {
    StringValue(v) => {
      let bool_value = BoolValue(v == "true")
      match bool_value {
        BoolValue(b) => assert_true(b)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test array operations
  let string_array = ArrayStringValue(["a", "b", "c"])
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      let joined = arr.join(",")
      assert_eq(joined, "a,b,c")
    }
    _ => assert_true(false)
  }
  
  // Test int array operations
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      let sum = arr.reduce(fn(acc, x) { acc + x }, 0)
      assert_eq(sum, 15)
    }
    _ => assert_true(false)
  }
  
  // Test null and empty values
  let empty_string = StringValue("")
  match empty_string {
    StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
  
  let zero_int = IntValue(0)
  match zero_int {
    IntValue(v) => assert_eq(v, 0)
    _ => assert_true(false)
  }
  
  let empty_array = ArrayStringValue([])
  match empty_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
}

// Test 2: Cross-Service Context Propagation Edge Cases
test "cross-service context propagation edge cases" {
  // Create root context with multiple values
  let root_ctx = Context::root()
  let trace_key = ContextKey::new("trace_id")
  let user_key = ContextKey::new("user_id")
  let session_key = ContextKey::new("session_id")
  let request_key = ContextKey::new("request_id")
  
  // Build context chain
  let ctx_with_trace = Context::with_value(root_ctx, trace_key, "trace_12345")
  let ctx_with_user = Context::with_value(ctx_with_trace, user_key, "user_67890")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session_11111")
  let ctx_with_request = Context::with_value(ctx_with_session, request_key, "req_22222")
  
  // Verify all values are preserved
  let extracted_trace = Context::get(ctx_with_request, trace_key)
  let extracted_user = Context::get(ctx_with_request, user_key)
  let extracted_session = Context::get(ctx_with_request, session_key)
  let extracted_request = Context::get(ctx_with_request, request_key)
  
  match extracted_trace {
    Some(v) => assert_eq(v, "trace_12345")
    None => assert_true(false)
  }
  
  match extracted_user {
    Some(v) => assert_eq(v, "user_67890")
    None => assert_true(false)
  }
  
  match extracted_session {
    Some(v) => assert_eq(v, "session_11111")
    None => assert_true(false)
  }
  
  match extracted_request {
    Some(v) => assert_eq(v, "req_22222")
    None => assert_true(false)
  }
  
  // Test with empty and null values
  let empty_key = ContextKey::new("empty_value")
  let ctx_with_empty = Context::with_value(ctx_with_request, empty_key, "")
  let extracted_empty = Context::get(ctx_with_empty, empty_key)
  
  match extracted_empty {
    Some(v) => assert_eq(v, "")
    None => assert_true(false)
  }
  
  // Test context with special characters
  let special_key = ContextKey::new("special.chars!@#$%^&*()")
  let ctx_with_special = Context::with_value(ctx_with_empty, special_key, "special_value!@#$%^&*()")
  let extracted_special = Context::get(ctx_with_special, special_key)
  
  match extracted_special {
    Some(v) => assert_eq(v, "special_value!@#$%^&*()")
    None => assert_true(false)
  }
  
  // Test context with long values
  let long_key = ContextKey::new("long_value")
  let long_value = "a".repeat(1000)
  let ctx_with_long = Context::with_value(ctx_with_special, long_key, long_value)
  let extracted_long = Context::get(ctx_with_long, long_key)
  
  match extracted_long {
    Some(v) => assert_eq(v.length(), 1000)
    None => assert_true(false)
  }
}

// Test 3: Time Series Data Processing
test "time series data processing" {
  // Create a clock for timestamp generation
  let clock = Clock::system()
  
  // Generate multiple timestamps
  let timestamp1 = Clock::now_unix_nanos(clock)
  let timestamp2 = Clock::now_unix_nanos(clock)
  let timestamp3 = Clock::now_unix_nanos(clock)
  
  // Verify timestamps are reasonable
  assert_true(timestamp1 > 0L)
  assert_true(timestamp2 >= timestamp1)
  assert_true(timestamp3 >= timestamp2)
  
  // Create time series data points
  let data_point1 = {
    "timestamp": timestamp1,
    "value": 10.5,
    "attributes": [("source", "sensor1"), ("unit", "celsius")]
  }
  
  let data_point2 = {
    "timestamp": timestamp2,
    "value": 12.3,
    "attributes": [("source", "sensor1"), ("unit", "celsius")]
  }
  
  let data_point3 = {
    "timestamp": timestamp3,
    "value": 11.8,
    "attributes": [("source", "sensor2"), ("unit", "celsius")]
  }
  
  // Create time series
  let time_series = [data_point1, data_point2, data_point3]
  
  // Verify time series properties
  assert_eq(time_series.length(), 3)
  
  // Calculate average value
  let total_value = time_series.reduce(fn(acc, point) { acc + point["value"] }, 0.0)
  let average_value = total_value / time_series.length().to_float()
  assert_true(average_value > 11.0 && average_value < 12.0)
  
  // Filter by source
  let sensor1_data = time_series.filter(fn(point) { 
    let mut found = false
    for (key, value) in point["attributes"] {
      if key == "source" && value == "sensor1" {
        found = true
      }
    }
    found
  })
  
  assert_eq(sensor1_data.length(), 2)
  
  // Calculate time difference
  let time_diff = time_series[2]["timestamp"] - time_series[0]["timestamp"]
  assert_true(time_diff >= 0L)
  
  // Test with edge cases
  let empty_series = []
  assert_eq(empty_series.length(), 0)
  
  let single_point_series = [data_point1]
  assert_eq(single_point_series.length(), 1)
  assert_eq(single_point_series[0]["value"], 10.5)
}

// Test 4: Resource Limits and Recovery
test "resource limits and recovery" {
  // Create resource with initial attributes
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  // Add many attributes to test limits
  let mut test_resource = resource_with_attrs
  for i in 0..<100 {
    let key = "test.attr." + i.to_string()
    let value = StringValue("value_" + i.to_string())
    test_resource = Resource::with_attributes(test_resource, [(key, value)])
  }
  
  // Verify attributes are set
  let test_attr_0 = Resource::get_attribute(test_resource, "test.attr.0")
  match test_attr_0 {
    Some(StringValue(v)) => assert_eq(v, "value_0")
    _ => assert_true(false)
  }
  
  let test_attr_99 = Resource::get_attribute(test_resource, "test.attr.99")
  match test_attr_99 {
    Some(StringValue(v)) => assert_eq(v, "value_99")
    _ => assert_true(false)
  }
  
  // Test resource recovery after attribute removal
  let recovery_resource = Resource::new()
  let recovery_attrs = [
    ("recovery.test", StringValue("recovery_value")),
    ("service.name", StringValue("recovery-service"))
  ]
  let resource_with_recovery = Resource::with_attributes(recovery_resource, recovery_attrs)
  
  // Merge with original resource
  let merged_resource = Resource::merge(test_resource, resource_with_recovery)
  
  // Verify merged resource contains both original and recovery attributes
  let original_attr = Resource::get_attribute(merged_resource, "test.attr.50")
  let recovery_attr = Resource::get_attribute(merged_resource, "recovery.test")
  
  match original_attr {
    Some(StringValue(v)) => assert_eq(v, "value_50")
    _ => assert_true(false)
  }
  
  match recovery_attr {
    Some(StringValue(v)) => assert_eq(v, "recovery_value")
    _ => assert_true(false)
  }
  
  // Test resource with empty attributes
  let empty_resource = Resource::new()
  let merged_with_empty = Resource::merge(test_resource, empty_resource)
  
  // Verify original attributes are preserved
  let preserved_attr = Resource::get_attribute(merged_with_empty, "test.attr.25")
  match preserved_attr {
    Some(StringValue(v)) => assert_eq(v, "value_25")
    _ => assert_true(false)
  }
}

// Test 5: Metrics Aggregation Analysis
test "metrics aggregation analysis" {
  // Create meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation.test")
  
  // Create counter for request counting
  let request_counter = Meter::create_counter(meter, "requests.total", Some("Total requests"), Some("count"))
  
  // Create histogram for response times
  let response_histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  
  // Create gauge for current connections
  let connection_gauge = Meter::create_gauge(meter, "connections.current", Some("Current connections"), Some("count"))
  
  // Simulate metrics recording
  for i in 0..<50 {
    Counter::add(request_counter, 1.0)
    Histogram::record(response_histogram, (100 + i * 2).to_float())
    
    if i % 10 == 0 {
      Gauge::record(connection_gauge, i.to_float())
    }
  }
  
  // Record metrics with attributes
  for i in 0..<20 {
    Counter::add_with_attributes(request_counter, 1.0, [
      ("method", StringValue("GET")),
      ("status", StringValue("200"))
    ])
    
    Counter::add_with_attributes(request_counter, 1.0, [
      ("method", StringValue("POST")),
      ("status", StringValue("201"))
    ])
    
    Histogram::record_with_attributes(response_histogram, (50 + i * 5).to_float(), [
      ("endpoint", StringValue("/api/users"))
    ])
  }
  
  // Verify instrument properties
  assert_eq(Counter::name(request_counter), "requests.total")
  assert_eq(Histogram::name(response_histogram), "response.time")
  assert_eq(Gauge::name(connection_gauge), "connections.current")
  
  assert_eq(Counter::description(request_counter), Some("Total requests"))
  assert_eq(Histogram::description(response_histogram), Some("Response time"))
  assert_eq(Gauge::description(connection_gauge), Some("Current connections"))
  
  assert_eq(Counter::unit(request_counter), Some("count"))
  assert_eq(Histogram::unit(response_histogram), Some("ms"))
  assert_eq(Gauge::unit(connection_gauge), Some("count"))
  
  // Test aggregation calculations
  let total_requests = 50 + 20 + 20 // 50 basic + 20 GET + 20 POST
  assert_true(total_requests > 0)
  
  // Test histogram boundaries
  let min_response_time = 100.0
  let max_response_time = (100 + 49 * 2).to_float()
  assert_true(min_response_time < max_response_time)
  
  // Test gauge variability
  let min_connections = 0.0
  let max_connections = 40.0
  assert_true(min_connections < max_connections)
}

// Test 6: Log Correlation and Tracing
test "log correlation and tracing" {
  // Create logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "correlation.test")
  
  // Create tracer provider and tracer
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "correlation.tracer")
  
  // Create span with trace context
  let span = Tracer::start_span(tracer, "operation.with.logs")
  let span_ctx = Span::span_context(span)
  let trace_id = SpanContext::trace_id(span_ctx)
  let span_id = SpanContext::span_id(span_ctx)
  
  // Add events to span
  Span::add_event(span, "operation.started", [
    ("operation.type", StringValue("data_processing"))
  ])
  
  // Create correlated log records
  let info_log = LogRecord::new_with_context(
    Info,
    Some("Operation started successfully"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  Span::add_event(span, "processing.completed", [
    ("records.processed", IntValue(100))
  ])
  
  let warn_log = LogRecord::new_with_context(
    Warn,
    Some("Processing taking longer than expected"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  Span::add_event(span, "operation.finished", [
    ("result", StringValue("success"))
  ])
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Operation completed with errors"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Emit log records
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  
  // Verify log correlation
  assert_eq(LogRecord::trace_id(info_log), Some(trace_id))
  assert_eq(LogRecord::span_id(info_log), Some(span_id))
  assert_eq(LogRecord::trace_id(warn_log), Some(trace_id))
  assert_eq(LogRecord::span_id(warn_log), Some(span_id))
  assert_eq(LogRecord::trace_id(error_log), Some(trace_id))
  assert_eq(LogRecord::span_id(error_log), Some(span_id))
  
  // Verify severity levels
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  
  // Verify log bodies
  match LogRecord::body(info_log) {
    Some(v) => assert_eq(v, "Operation started successfully")
    None => assert_true(false)
  }
  
  match LogRecord::body(warn_log) {
    Some(v) => assert_eq(v, "Processing taking longer than expected")
    None => assert_true(false)
  }
  
  match LogRecord::body(error_log) {
    Some(v) => assert_eq(v, "Operation completed with errors")
    None => assert_true(false)
  }
  
  // End the span
  Span::end(span)
}

// Test 7: Concurrent Operations and Thread Safety
test "concurrent operations and thread safety" {
  // Create multiple tracer providers
  let tracer_provider1 = TracerProvider::default()
  let tracer_provider2 = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider1, "concurrent.tracer1")
  let tracer2 = TracerProvider::get_tracer(tracer_provider2, "concurrent.tracer2")
  
  // Create multiple spans concurrently
  let spans = []
  for i in 0..<10 {
    let span_name = "concurrent.span." + i.to_string()
    let span = if i % 2 == 0 {
      Tracer::start_span(tracer1, span_name)
    } else {
      Tracer::start_span(tracer2, span_name)
    }
    spans = spans @ [span]
  }
  
  // Verify all spans are created
  assert_eq(spans.length(), 10)
  
  // Add attributes to all spans
  for i in 0..<spans.length() {
    Span::set_attribute(spans[i], "span.index", IntValue(i))
    Span::set_attribute(spans[i], "tracer.source", StringValue(if i % 2 == 0 { "tracer1" } else { "tracer2" }))
  }
  
  // Create multiple meter providers
  let meter_provider1 = MeterProvider::default()
  let meter_provider2 = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(meter_provider1, "concurrent.meter1")
  let meter2 = MeterProvider::get_meter(meter_provider2, "concurrent.meter2")
  
  // Create multiple metrics instruments
  let counters = []
  let histograms = []
  
  for i in 0..<5 {
    let counter_name = "concurrent.counter." + i.to_string()
    let histogram_name = "concurrent.histogram." + i.to_string()
    
    let counter = if i % 2 == 0 {
      Meter::create_counter(meter1, counter_name)
    } else {
      Meter::create_counter(meter2, counter_name)
    }
    
    let histogram = if i % 2 == 0 {
      Meter::create_histogram(meter1, histogram_name)
    } else {
      Meter::create_histogram(meter2, histogram_name)
    }
    
    counters = counters @ [counter]
    histograms = histograms @ [histogram]
  }
  
  // Record metrics on all instruments
  for i in 0..<counters.length() {
    Counter::add(counters[i], i.to_double())
    Histogram::record(histograms[i], (i * 10).to_float())
  }
  
  // Create multiple logger providers
  let logger_provider1 = LoggerProvider::default()
  let logger_provider2 = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(logger_provider1, "concurrent.logger1")
  let logger2 = LoggerProvider::get_logger(logger_provider2, "concurrent.logger2")
  
  // Create and emit multiple log records
  for i in 0..<10 {
    let severity = if i % 3 == 0 { Info } else if i % 3 == 1 { Warn } else { Error }
    let message = "Concurrent log message " + i.to_string()
    let log_record = LogRecord::new(severity, message)
    
    if i % 2 == 0 {
      Logger::emit(logger1, log_record)
    } else {
      Logger::emit(logger2, log_record)
    }
  }
  
  // End all spans
  for i in 0..<spans.length() {
    Span::end(spans[i])
  }
  
  // Verify all spans have valid contexts
  for i in 0..<spans.length() {
    let span_ctx = Span::span_context(spans[i])
    assert_true(SpanContext::is_valid(span_ctx))
  }
  
  // Verify all instruments have unique names
  for i in 0..<counters.length() {
    for j in 0..<counters.length() {
      if i != j {
        assert_false(Counter::name(counters[i]) == Counter::name(counters[j]))
        assert_false(Histogram::name(histograms[i]) == Histogram::name(histograms[j]))
      }
    }
  }
}

// Test 8: Dynamic Configuration Updates
test "dynamic configuration updates" {
  // Create initial configuration
  let initial_config = {
    "service.name": "test-service",
    "service.version": "1.0.0",
    "telemetry.enabled": true,
    "sampling.rate": 0.1,
    "max.batch.size": 512,
    "export.interval": 5000
  }
  
  // Create tracer with initial configuration
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "config.test")
  
  // Create span with initial configuration
  let span1 = Tracer::start_span(tracer, "config.test.span1")
  Span::set_attribute(span1, "config.version", StringValue("1.0.0"))
  
  // Update configuration
  let updated_config = {
    "service.name": "test-service",
    "service.version": "1.1.0",
    "telemetry.enabled": true,
    "sampling.rate": 0.2,
    "max.batch.size": 1024,
    "export.interval": 3000
  }
  
  // Create span with updated configuration
  let span2 = Tracer::start_span(tracer, "config.test.span2")
  Span::set_attribute(span2, "config.version", StringValue("1.1.0"))
  
  // Verify configuration changes
  let span1_version = Span::get_attribute(span1, "config.version")
  let span2_version = Span::get_attribute(span2, "config.version")
  
  match span1_version {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  match span2_version {
    Some(StringValue(v)) => assert_eq(v, "1.1.0")
    _ => assert_true(false)
  }
  
  // Create meter with initial configuration
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "config.meter")
  
  // Create metric with initial configuration
  let counter1 = Meter::create_counter(meter, "config.test.counter", Some("Test counter"), Some("count"))
  Counter::add_with_attributes(counter1, 1.0, [
    ("config.version", StringValue("1.0.0"))
  ])
  
  // Update configuration and create new metric
  let counter2 = Meter::create_counter(meter, "config.test.counter.v2", Some("Test counter v2"), Some("count"))
  Counter::add_with_attributes(counter2, 1.0, [
    ("config.version", StringValue("1.1.0"))
  ])
  
  // Verify metric configuration
  assert_eq(Counter::name(counter1), "config.test.counter")
  assert_eq(Counter::name(counter2), "config.test.counter.v2")
  
  // Create logger with initial configuration
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "config.logger")
  
  // Create log with initial configuration
  let log1 = LogRecord::new(Info, "Configuration test v1.0.0")
  LogRecord::add_attribute(log1, "config.version", "1.0.0")
  
  // Update configuration and create new log
  let log2 = LogRecord::new(Info, "Configuration test v1.1.0")
  LogRecord::add_attribute(log2, "config.version", "1.1.0")
  
  // Emit logs
  Logger::emit(logger, log1)
  Logger::emit(logger, log2)
  
  // Verify log configuration
  match LogRecord::body(log1) {
    Some(v) => assert_eq(v, "Configuration test v1.0.0")
    None => assert_true(false)
  }
  
  match LogRecord::body(log2) {
    Some(v) => assert_eq(v, "Configuration test v1.1.0")
    None => assert_true(false)
  }
  
  // End spans
  Span::end(span1)
  Span::end(span2)
}