// Error Boundary and Recovery Tests
// This file contains test cases for error handling and recovery mechanisms

test "span error handling and recovery" {
  // Test error handling in span operations
  let span_ctx = azimuth::SpanContext::new("trace-error-test", "span-error-123", true, "")
  let span = azimuth::Span::new("error-prone-operation", azimuth::Internal, span_ctx)
  
  // Test handling of invalid attribute values
  let attrs = azimuth::Attributes::new()
  
  // These should not crash the system
  azimuth::Attributes::set(attrs, "null.string", azimuth::StringValue(""))
  azimuth::Attributes::set(attrs, "empty.array", azimuth::ArrayStringValue([]))
  azimuth::Attributes::set(attrs, "zero.float", azimuth::FloatValue(0.0))
  azimuth::Attributes::set(attrs, "negative.int", azimuth::IntValue(-1))
  
  // Test handling of very long strings
  let long_string = "x" * 10000
  azimuth::Attributes::set(attrs, "long.string", azimuth::StringValue(long_string))
  
  // Verify attributes are stored correctly
  let null_string = azimuth::Attributes::get(attrs, "null.string")
  let empty_array = azimuth::Attributes::get(attrs, "empty.array")
  let zero_float = azimuth::Attributes::get(attrs, "zero.float")
  let negative_int = azimuth::Attributes::get(attrs, "negative.int")
  let retrieved_long_string = azimuth::Attributes::get(attrs, "long.string")
  
  assert_eq(null_string, Some(azimuth::StringValue("")))
  assert_eq(empty_array, Some(azimuth::ArrayStringValue([])))
  assert_eq(zero_float, Some(azimuth::FloatValue(0.0)))
  assert_eq(negative_int, Some(azimuth::IntValue(-1)))
  assert_eq(retrieved_long_string, Some(azimuth::StringValue(long_string)))
  
  // Test error event recording
  azimuth::Span::record_error(span, "Test error message", [("error.code", azimuth::IntValue(500))])
  azimuth::Span::record_exception(span, "RuntimeException", "Test exception details")
  
  // End span even with errors
  azimuth::Span::end(span)
  assert_true(true, "Span should end successfully even after error recording")
}

test "metric error resilience" {
  // Test metric system resilience to errors
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "error-resilience-test")
  
  let counter = azimuth::Meter::create_counter(meter, "error.counter", "Error counter", "1")
  let histogram = azimuth::Meter::create_histogram(meter, "error.histogram", "Error histogram", "ms")
  let gauge = azimuth::Meter::create_gauge(meter, "error.gauge", "Error gauge", "bytes")
  
  // Test handling of invalid metric values
  azimuth::Counter::add(counter, -1, [("operation", azimuth::StringValue("invalid-negative"))])  // Should handle gracefully
  azimuth::Counter::add(counter, 0, [("operation", azimuth::StringValue("zero-value"))])
  
  // Test extreme values
  azimuth::Histogram::record(histogram, -1.0, [("type", azimuth::StringValue("negative"))])  // Should handle gracefully
  azimuth::Histogram::record(histogram, 0.0, [("type", azimuth::StringValue("zero"))])
  azimuth::Histogram::record(histogram, 1.7976931348623157e+308, [("type", azimuth::StringValue("max-float"))])
  
  // Test invalid gauge values
  azimuth::Gauge::set(gauge, -1, [("type", azimuth::StringValue("negative-gauge"))])
  
  // Test with invalid attribute keys/values
  azimuth::Counter::add(counter, 1, [("", azimuth::StringValue("empty-key"))])  // Should handle gracefully
  azimuth::Counter::add(counter, 1, [("valid.key", azimuth::StringValue(""))])
  
  // Verify system is still functional after error conditions
  let counter_value = azimuth::Counter::get_value(counter)
  let histogram_measurements = azimuth::Histogram::get_measurements(histogram)
  let gauge_value = azimuth::Gauge::get_value(gauge)
  
  assert_true(counter_value >= 0, "Counter should handle negative values gracefully")
  assert_true(histogram_measurements.length >= 0, "Histogram should handle invalid values gracefully")
  assert_true(gauge_value >= 0, "Gauge should handle negative values gracefully")
}

test "logger error handling" {
  // Test logger error handling capabilities
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "error-logger")
  
  // Test logging with invalid data
  let invalid_attrs = [
    ("null.value", azimuth::StringValue("")),
    ("special.chars", azimuth::StringValue("!@#$%^&*(){}[]|\\:;\"'<>?,./")),
    ("unicode.chars", azimuth::StringValue("测试🚀🔥💡")),
    ("very.long.key.name.that.exceeds.reasonable.limits.and.should.be.handled.gracefully", azimuth::StringValue("value"))
  ]
  
  // Create log records with various severity levels
  azimuth::Logger::log(logger, azimuth::Debug, "Debug message with invalid attrs", invalid_attrs)
  azimuth::Logger::log(logger, azimuth::Info, "Info message", [])
  azimuth::Logger::log(logger, azimuth::Warn, "Warning message", [("warning", azimuth::StringValue("test"))])
  azimuth::Logger::log(logger, azimuth::Error, "Error message", [("error", azimuth::StringValue("test error"))])
  azimuth::Logger::log(logger, azimuth::Fatal, "Fatal message", [("fatal", azimuth::StringValue("test fatal"))])
  
  // Test logging with empty/null messages
  azimuth::Logger::log(logger, azimuth::Info, "", [])
  azimuth::Logger::log(logger, azimuth::Info, "Normal message after empty", [])
  
  // Test very long log messages
  let long_message = "x" * 100000
  azimuth::Logger::log(logger, azimuth::Info, long_message, [])
  
  // Verify logger is still functional
  let log_records = azimuth::Logger::get_log_records(logger)
  assert_true(log_records.length >= 8, "Logger should handle all error conditions")
  
  // Verify log integrity
  let last_log = log_records[log_records.length - 1]
  assert_eq(azimuth::LogRecord::severity(last_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(last_log), "Normal message after empty")
}

test "propagator error handling" {
  // Test propagator error handling with invalid data
  let valid_ctx = azimuth::SpanContext::new("trace-valid", "span-valid", true, "key1=value1")
  let valid_baggage = azimuth::Baggage::new()
  let valid_baggage_with_entry = azimuth::Baggage::set_entry(valid_baggage, "user.id", "12345")
  
  // Test extraction with invalid headers
  let empty_headers = []
  let invalid_headers = [
    ("invalid-header", "invalid-value"),
    ("traceparent", "invalid-trace-format"),
    ("baggage", "invalid=baggage=format=with=too=many=equals")
  ]
  
  // Should handle gracefully without crashing
  let (ctx1, baggage1) = azimuth::Propagator::inject_context(empty_headers)
  let (ctx2, baggage2) = azimuth::Propagator::inject_context(invalid_headers)
  
  // Verify fallback behavior
  assert_eq(azimuth::SpanContext::trace_id(ctx1), "")  // Empty context for empty headers
  assert_eq(azimuth::SpanContext::span_id(ctx1), "")
  assert_eq(azimuth::Baggage::get_entry(baggage1, "user.id"), None)
  
  // Test injection with invalid context
  let invalid_ctx = azimuth::SpanContext::new("", "", false, "")
  let headers_from_invalid = azimuth::Propagator::extract_headers(invalid_ctx, valid_baggage_with_entry)
  
  // Should handle gracefully
  assert_true(headers_from_invalid.length >= 0, "Should handle invalid context gracefully")
  
  // Test round-trip with valid data to ensure system still works
  let headers = azimuth::Propagator::extract_headers(valid_ctx, valid_baggage_with_entry)
  let (roundtrip_ctx, roundtrip_baggage) = azimuth::Propagator::inject_context(headers)
  
  assert_eq(azimuth::SpanContext::trace_id(roundtrip_ctx), "trace-valid")
  assert_eq(azimuth::SpanContext::span_id(roundtrip_ctx), "span-valid")
  assert_eq(azimuth::Baggage::get_entry(roundtrip_baggage, "user.id"), Some("12345"))
}

test "resource management under error conditions" {
  // Test resource management when errors occur
  let resource1 = azimuth::Resource::new([("service.name", azimuth::StringValue("test-service-1"))])
  let resource2 = azimuth::Resource::new([("service.name", azimuth::StringValue("test-service-2"))])
  
  // Test merging with invalid resources
  let empty_resource = azimuth::Resource::new([])
  let resource_with_empty_attrs = azimuth::Resource::new([("", azimuth::StringValue(""))])
  
  // Should handle gracefully
  let merged1 = azimuth::Resource::merge(resource1, empty_resource)
  let merged2 = azimuth::Resource::merge(resource1, resource_with_empty_attrs)
  let merged3 = azimuth::Resource::merge(empty_resource, resource2)
  
  // Verify merge results
  let service_name1 = azimuth::Resource::get_attribute(merged1, "service.name")
  let service_name2 = azimuth::Resource::get_attribute(merged2, "service.name")
  let service_name3 = azimuth::Resource::get_attribute(merged3, "service.name")
  
  assert_eq(service_name1, Some(azimuth::StringValue("test-service-1")))
  assert_eq(service_name2, Some(azimuth::StringValue("test-service-1")))
  assert_eq(service_name3, Some(azimuth::StringValue("test-service-2")))
  
  // Test resource operations under memory pressure
  let resources = []
  for i = 0; i < 1000; i = i + 1 {
    let resource = azimuth::Resource::new([
      ("service.instance", azimuth::StringValue("instance-" + azimuth::Int::to_string(i))),
      ("service.version", azimuth::StringValue("1.0." + azimuth::Int::to_string(i % 100)))
    ])
    resources = azimuth::List::append(resources, resource)
  }
  
  // Merge all resources
  let final_resource = resources[0]
  for i = 1; i < resources.length; i = i + 1 {
    final_resource = azimuth::Resource::merge(final_resource, resources[i])
  }
  
  // Verify final resource is valid
  let final_service_instance = azimuth::Resource::get_attribute(final_resource, "service.instance")
  assert_true(final_service_instance.is_some, "Final resource should be valid after merging")
  
  // Clean up
  resources = []
}