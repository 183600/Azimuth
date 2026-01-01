// Telemetry Data Serialization and Integrity Tests
// This file contains test cases for telemetry data serialization and integrity

test "telemetry data serialization integrity" {
  // Test span serialization and deserialization
  let span_ctx = azimuth::SpanContext::new("trace-12345", "span-67890", true, "key1=value1,key2=value2")
  let span = azimuth::Span::new("serialization-test", azimuth::Internal, span_ctx)
  
  // Add attributes to the span
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "user.id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(attrs, "request.size", azimuth::IntValue(1024))
  azimuth::Attributes::set(attrs, "processing.time", azimuth::FloatValue(1.5))
  azimuth::Attributes::set(attrs, "cache.hit", azimuth::BoolValue(true))
  
  // Serialize the span with attributes
  let serialized = azimuth::Span::serialize(span, attrs)
  assert_true(serialized.length > 0)
  
  // Deserialize and verify integrity
  let (deserialized_span, deserialized_attrs) = azimuth::Span::deserialize(serialized)
  assert_eq(azimuth::Span::name(deserialized_span), "serialization-test")
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::context(deserialized_span)), "trace-12345")
  assert_eq(azimuth::SpanContext::span_id(azimuth::Span::context(deserialized_span)), "span-67890")
  
  // Verify attributes integrity
  let user_id = azimuth::Attributes::get(deserialized_attrs, "user.id")
  let request_size = azimuth::Attributes::get(deserialized_attrs, "request.size")
  let processing_time = azimuth::Attributes::get(deserialized_attrs, "processing.time")
  let cache_hit = azimuth::Attributes::get(deserialized_attrs, "cache.hit")
  
  assert_eq(user_id, Some(azimuth::StringValue("user-123")))
  assert_eq(request_size, Some(azimuth::IntValue(1024)))
  assert_eq(processing_time, Some(azimuth::FloatValue(1.5)))
  assert_eq(cache_hit, Some(azimuth::BoolValue(true)))
}

test "metric serialization with complex values" {
  // Test metric serialization with complex values
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "serialization-test")
  
  // Create various metric types
  let counter = azimuth::Meter::create_counter(meter, "request.count", "Total requests", "1")
  let histogram = azimuth::Meter::create_histogram(meter, "request.duration", "Request duration", "s")
  let gauge = azimuth::Meter::create_gauge(meter, "system.memory", "System memory usage", "bytes")
  
  // Record metric values
  azimuth::Counter::add(counter, 10, [("method", azimuth::StringValue("GET"))])
  azimuth::Counter::add(counter, 5, [("method", azimuth::StringValue("POST"))])
  
  azimuth::Histogram::record(histogram, 0.1, [("endpoint", azimuth::StringValue("/api/users"))])
  azimuth::Histogram::record(histogram, 0.5, [("endpoint", azimuth::StringValue("/api/orders"))])
  azimuth::Histogram::record(histogram, 1.2, [("endpoint", azimuth::StringValue("/api/products"))])
  
  azimuth::Gauge::set(gauge, 1024 * 1024 * 512, [("type", azimuth::StringValue("physical"))])
  azimuth::Gauge::set(gauge, 1024 * 1024 * 256, [("type", azimuth::StringValue("virtual"))])
  
  // Serialize metrics
  let serialized_metrics = azimuth::Meter::serialize_metrics(meter)
  assert_true(serialized_metrics.length > 0)
  
  // Deserialize and verify
  let deserialized_metrics = azimuth::Meter::deserialize_metrics(serialized_metrics)
  assert_true(deserialized_metrics.length >= 3)  // At least 3 metrics
  
  // Verify specific metric values
  let counter_data = azimuth::MetricData::get_by_name(deserialized_metrics, "request.count")
  match counter_data {
    Some(data) => {
      assert_eq(azimuth::MetricData::type(data), azimuth::Counter)
      assert_eq(azimuth::MetricData::total_value(data), 15)
    }
    None => assert_true(false, "Counter metric not found")
  }
}

test "log record serialization and correlation" {
  // Test log record serialization with span correlation
  let span_ctx = azimuth::SpanContext::new("trace-98765", "span-43210", true, "")
  let span = azimuth::Span::new("logging-operation", azimuth::Server, span_ctx)
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "test-logger")
  
  // Create log records with different severity levels
  let info_log = azimuth::Logger::create_log_record(
    logger, 
    azimuth::Info, 
    "User login successful", 
    [("user.id", azimuth::StringValue("user-456")), ("ip.address", azimuth::StringValue("192.168.1.1"))]
  )
  
  let error_log = azimuth::Logger::create_log_record(
    logger, 
    azimuth::Error, 
    "Database connection failed", 
    [("error.code", azimuth::IntValue(500)), ("retry.count", azimuth::IntValue(3))]
  )
  
  // Correlate logs with span
  let correlated_info_log = azimuth::LogRecord::with_span_context(info_log, span_ctx)
  let correlated_error_log = azimuth::LogRecord::with_span_context(error_log, span_ctx)
  
  // Serialize log records
  let serialized_logs = azimuth::Logger::serialize_log_records(logger, [correlated_info_log, correlated_error_log])
  assert_true(serialized_logs.length > 0)
  
  // Deserialize and verify
  let deserialized_logs = azimuth::Logger::deserialize_log_records(serialized_logs)
  assert_eq(deserialized_logs.length, 2)
  
  // Verify log correlation
  let first_log = deserialized_logs[0]
  let second_log = deserialized_logs[1]
  
  assert_eq(azimuth::LogRecord::trace_id(first_log), "trace-98765")
  assert_eq(azimuth::LogRecord::span_id(first_log), "span-43210")
  assert_eq(azimuth::LogRecord::trace_id(second_log), "trace-98765")
  assert_eq(azimuth::LogRecord::span_id(second_log), "span-43210")
  
  // Verify log content
  assert_eq(azimuth::LogRecord::severity(first_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity(second_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(first_log), "User login successful")
  assert_eq(azimuth::LogRecord::body(second_log), "Database connection failed")
}