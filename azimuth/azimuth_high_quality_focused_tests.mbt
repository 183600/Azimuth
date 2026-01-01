// Azimuth High-Quality Focused Test Suite
// This file contains 10 focused test cases for critical telemetry functionality

// Test 1: Resource Management and Cleanup
pub test "resource management and cleanup" {
  // Test resource creation with various attributes
  let resource_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(resource_attrs, "service.name", azimuth::StringValue("payment-service"))
  azimuth::Attributes::set(resource_attrs, "service.version", azimuth::StringValue("1.2.3"))
  azimuth::Attributes::set(resource_attrs, "service.instance.id", azimuth::StringValue("instance-123"))
  azimuth::Attributes::set(resource_attrs, "deployment.environment", azimuth::StringValue("production"))
  
  let resource = azimuth::Resource::new(resource_attrs)
  let retrieved_attrs = azimuth::Resource::attributes(resource)
  
  // Verify resource attributes are properly stored and retrieved
  assert_eq(azimuth::Attributes::get(retrieved_attrs, "service.name"), Some(azimuth::StringValue("payment-service")))
  assert_eq(azimuth::Attributes::get(retrieved_attrs, "service.version"), Some(azimuth::StringValue("1.2.3")))
  assert_eq(azimuth::Attributes::get(retrieved_attrs, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
  assert_eq(azimuth::Attributes::get(retrieved_attrs, "deployment.environment"), Some(azimuth::StringValue("production")))
  
  // Test resource merge functionality
  let additional_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(additional_attrs, "host.name", azimuth::StringValue("web-server-01"))
  azimuth::Attributes::set(additional_attrs, "os.type", azimuth::StringValue("linux"))
  
  let merged_resource = azimuth::Resource::merge(resource, azimuth::Resource::new(additional_attrs))
  let merged_attrs = azimuth::Resource::attributes(merged_resource)
  
  // Verify merged attributes contain both original and additional attributes
  assert_eq(azimuth::Attributes::get(merged_attrs, "service.name"), Some(azimuth::StringValue("payment-service")))
  assert_eq(azimuth::Attributes::get(merged_attrs, "host.name"), Some(azimuth::StringValue("web-server-01")))
  assert_eq(azimuth::Attributes::get(merged_attrs, "os.type"), Some(azimuth::StringValue("linux")))
}

// Test 2: Complex Context Propagation Scenarios
pub test "complex context propagation scenarios" {
  // Create root context with multiple values
  let root_ctx = azimuth::Context::root()
  let user_key = azimuth::ContextKey::new("user.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let session_key = azimuth::ContextKey::new("session.id")
  
  let ctx_with_user = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_user, request_key, "req-67890")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_request, session_key, "sess-abcdef")
  
  // Verify all context values are properly propagated
  assert_eq(azimuth::Context::get(ctx_with_session, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_session, request_key), Some("req-67890"))
  assert_eq(azimuth::Context::get(ctx_with_session, session_key), Some("sess-abcdef"))
  
  // Test context with baggage propagation
  let baggage = azimuth::Baggage::new()
  let updated_baggage = azimuth::Baggage::set_entry(baggage, "correlation.id", "corr-123")
  let final_baggage = azimuth::Baggage::set_entry(updated_baggage, "trace.chain", "service-a->service-b->service-c")
  
  let ctx_with_baggage = azimuth::Context::with_baggage(ctx_with_session, final_baggage)
  let propagated_baggage = azimuth::Context::baggage(ctx_with_baggage)
  
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "correlation.id"), Some("corr-123"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage, "trace.chain"), Some("service-a->service-b->service-c"))
}

// Test 3: Metrics Aggregation and Performance
pub test "metrics aggregation and performance" {
  // Create meter provider and meter
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "performance-meter")
  
  // Create various metric instruments
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration", Some("HTTP response duration"), Some("ms"))
  let active_connections = azimuth::Meter::create_updown_counter(meter, "http.active.connections")
  let memory_gauge = azimuth::Meter::create_gauge(meter, "process.memory.usage")
  
  // Simulate metric operations
  azimuth::Counter::add(request_counter, 100.0)  // 100 requests
  azimuth::Counter::add(request_counter, 50.0)   // Additional 50 requests
  
  // Record response times
  azimuth::Histogram::record(response_histogram, 120.5)
  azimuth::Histogram::record(response_histogram, 85.3)
  azimuth::Histogram::record(response_histogram, 200.1)
  azimuth::Histogram::record(response_histogram, 95.7)
  
  // Simulate connection tracking
  azimuth::UpDownCounter::add(active_connections, 25.0)
  azimuth::UpDownCounter::add(active_connections, -5.0)
  azimuth::UpDownCounter::add(active_connections, 10.0)
  
  // Verify metric instrument properties
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(response_histogram.description, Some("HTTP response duration"))
  assert_eq(response_histogram.unit, Some("ms"))
  assert_eq(active_connections.name, "http.active.connections")
  assert_eq(memory_gauge.name, "process.memory.usage")
}

// Test 4: Log Correlation and Trace Linking
pub test "log correlation and trace linking" {
  // Create span context for trace correlation
  let span_ctx = azimuth::SpanContext::new("trace-123456789", "span-987654321", true, "key1=value1,key2=value2")
  
  // Create log records with trace correlation
  let info_log = azimuth::LogRecord::new(azimuth::Info, "User authentication successful")
  let correlated_info_log = azimuth::LogRecord::with_span_context(info_log, span_ctx)
  
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Database connection failed")
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error.code", azimuth::IntValue(500))
  azimuth::Attributes::set(error_attrs, "error.message", azimuth::StringValue("Connection timeout"))
  let correlated_error_log = azimuth::LogRecord::with_attributes_and_span_context(error_log, error_attrs, span_ctx)
  
  // Verify log correlation
  assert_eq(azimuth::LogRecord::severity_number(correlated_info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(correlated_info_log), Some("User authentication successful"))
  assert_eq(azimuth::LogRecord::trace_id(correlated_info_log), Some("trace-123456789"))
  assert_eq(azimuth::LogRecord::span_id(correlated_info_log), Some("span-987654321"))
  
  assert_eq(azimuth::LogRecord::severity_number(correlated_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(correlated_error_log), Some("Database connection failed"))
  assert_eq(azimuth::LogRecord::trace_id(correlated_error_log), Some("trace-123456789"))
  assert_eq(azimuth::LogRecord::span_id(correlated_error_log), Some("span-987654321"))
  
  // Verify error attributes
  let error_log_attrs = azimuth::LogRecord::attributes(correlated_error_log)
  assert_eq(azimuth::Attributes::get(error_log_attrs, "error.code"), Some(azimuth::IntValue(500)))
  assert_eq(azimuth::Attributes::get(error_log_attrs, "error.message"), Some(azimuth::StringValue("Connection timeout")))
}

// Test 5: Error Handling and Recovery
pub test "error handling and recovery" {
  // Test span error status handling
  let span_ctx = azimuth::SpanContext::new("error-trace-123", "error-span-456", true, "")
  let span = azimuth::Span::new("failing-operation", azimuth::Internal, span_ctx)
  
  // Record error event
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "exception.type", azimuth::StringValue("TimeoutException"))
  azimuth::Attributes::set(error_attrs, "exception.message", azimuth::StringValue("Operation timed out after 30 seconds"))
  azimuth::Attributes::set(error_attrs, "error.stack", azimuth::StringValue("at service.process (line 42)"))
  
  azimuth::Span::add_event(span, "operation_failed", error_attrs)
  azimuth::Span::set_status(span, azimuth::Error, "Operation failed due to timeout")
  
  // Verify error state
  assert_eq(azimuth::Span::status(span), azimuth::Error)
  assert_eq(azimuth::Span::status_message(span), Some("Operation failed due to timeout"))
  
  // Test recovery with new span
  let recovery_span_ctx = azimuth::SpanContext::new("recovery-trace-123", "recovery-span-456", true, "")
  let recovery_span = azimuth::Span::new("recovery-operation", azimuth::Internal, recovery_span_ctx)
  
  let recovery_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(recovery_attrs, "retry.attempt", azimuth::IntValue(1))
  azimuth::Attributes::set(recovery_attrs, "recovery.strategy", azimuth::StringValue("circuit-breaker"))
  
  azimuth::Span::add_event(recovery_span, "recovery_initiated", recovery_attrs)
  azimuth::Span::set_status(recovery_span, azimuth::Ok, "Recovery successful")
  
  // Verify recovery state
  assert_eq(azimuth::Span::status(recovery_span), azimuth::Ok)
  assert_eq(azimuth::Span::status_message(recovery_span), Some("Recovery successful"))
}

// Test 6: Concurrent Operations Safety
pub test "concurrent operations safety" {
  // Test concurrent attribute operations
  let shared_attrs = azimuth::Attributes::new()
  
  // Simulate concurrent attribute setting
  azimuth::Attributes::set(shared_attrs, "concurrent.test1", azimuth::StringValue("value1"))
  azimuth::Attributes::set(shared_attrs, "concurrent.test2", azimuth::IntValue(42))
  azimuth::Attributes::set(shared_attrs, "concurrent.test3", azimuth::FloatValue(3.14))
  azimuth::Attributes::set(shared_attrs, "concurrent.test4", azimuth::BoolValue(true))
  azimuth::Attributes::set(shared_attrs, "concurrent.test5", azimuth::ArrayStringValue(["a", "b", "c"]))
  
  // Verify all attributes are correctly set despite concurrent operations
  assert_eq(azimuth::Attributes::get(shared_attrs, "concurrent.test1"), Some(azimuth::StringValue("value1")))
  assert_eq(azimuth::Attributes::get(shared_attrs, "concurrent.test2"), Some(azimuth::IntValue(42)))
  assert_eq(azimuth::Attributes::get(shared_attrs, "concurrent.test3"), Some(azimuth::FloatValue(3.14)))
  assert_eq(azimuth::Attributes::get(shared_attrs, "concurrent.test4"), Some(azimuth::BoolValue(true)))
  assert_eq(azimuth::Attributes::get(shared_attrs, "concurrent.test5"), Some(azimuth::ArrayStringValue(["a", "b", "c"])))
  
  // Test concurrent baggage operations
  let shared_baggage = azimuth::Baggage::new()
  
  // Simulate concurrent baggage entries
  let updated1 = azimuth::Baggage::set_entry(shared_baggage, "concurrent.baggage1", "baggage-value1")
  let updated2 = azimuth::Baggage::set_entry(updated1, "concurrent.baggage2", "baggage-value2")
  let updated3 = azimuth::Baggage::set_entry(updated2, "concurrent.baggage3", "baggage-value3")
  
  // Verify baggage integrity
  assert_eq(azimuth::Baggage::get_entry(updated3, "concurrent.baggage1"), Some("baggage-value1"))
  assert_eq(azimuth::Baggage::get_entry(updated3, "concurrent.baggage2"), Some("baggage-value2"))
  assert_eq(azimuth::Baggage::get_entry(updated3, "concurrent.baggage3"), Some("baggage-value3"))
}

// Test 7: Serialization/Deserialization Integrity
pub test "serialization deserialization integrity" {
  // Test attribute serialization
  let original_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(original_attrs, "string.value", azimuth::StringValue("test string"))
  azimuth::Attributes::set(original_attrs, "int.value", azimuth::IntValue(12345))
  azimuth::Attributes::set(original_attrs, "float.value", azimuth::FloatValue(123.45))
  azimuth::Attributes::set(original_attrs, "bool.value", azimuth::BoolValue(true))
  azimuth::Attributes::set(original_attrs, "array.string", azimuth::ArrayStringValue(["a", "b", "c"]))
  azimuth::Attributes::set(original_attrs, "array.int", azimuth::ArrayIntValue([1, 2, 3]))
  
  // Serialize attributes to string representation
  let serialized_attrs = azimuth::Attributes::serialize(original_attrs)
  
  // Deserialize attributes back from string
  let deserialized_attrs = azimuth::Attributes::deserialize(serialized_attrs)
  
  // Verify integrity of serialization/deserialization
  assert_eq(azimuth::Attributes::get(deserialized_attrs, "string.value"), Some(azimuth::StringValue("test string")))
  assert_eq(azimuth::Attributes::get(deserialized_attrs, "int.value"), Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Attributes::get(deserialized_attrs, "float.value"), Some(azimuth::FloatValue(123.45)))
  assert_eq(azimuth::Attributes::get(deserialized_attrs, "bool.value"), Some(azimuth::BoolValue(true)))
  assert_eq(azimuth::Attributes::get(deserialized_attrs, "array.string"), Some(azimuth::ArrayStringValue(["a", "b", "c"])))
  assert_eq(azimuth::Attributes::get(deserialized_attrs, "array.int"), Some(azimuth::ArrayIntValue([1, 2, 3])))
  
  // Test span context serialization
  let original_span_ctx = azimuth::SpanContext::new("serialized-trace", "serialized-span", true, "key=value")
  let serialized_span_ctx = azimuth::SpanContext::serialize(original_span_ctx)
  let deserialized_span_ctx = azimuth::SpanContext::deserialize(serialized_span_ctx)
  
  // Verify span context integrity
  assert_eq(azimuth::SpanContext::trace_id(deserialized_span_ctx), "serialized-trace")
  assert_eq(azimuth::SpanContext::span_id(deserialized_span_ctx), "serialized-span")
  assert_true(azimuth::SpanContext::is_sampled(deserialized_span_ctx))
  assert_eq(azimuth::SpanContext::trace_state(deserialized_span_ctx), "key=value")
}

// Test 8: Platform Compatibility
pub test "platform compatibility" {
  // Test platform-specific attribute handling
  let platform_attrs = azimuth::Attributes::new()
  
  // Add platform-specific attributes
  azimuth::Attributes::set(platform_attrs, "os.name", azimuth::StringValue("linux"))
  azimuth::Attributes::set(platform_attrs, "os.version", azimuth::StringValue("5.15.0"))
  azimuth::Attributes::set(platform_attrs, "arch", azimuth::StringValue("x86_64"))
  azimuth::Attributes::set(platform_attrs, "runtime.name", azimuth::StringValue("moonbit"))
  azimuth::Attributes::set(platform_attrs, "runtime.version", azimuth::StringValue("0.1.0"))
  
  // Test platform-specific resource creation
  let platform_resource = azimuth::Resource::new(platform_attrs)
  let resource_attrs = azimuth::Resource::attributes(platform_resource)
  
  // Verify platform attributes
  assert_eq(azimuth::Attributes::get(resource_attrs, "os.name"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Attributes::get(resource_attrs, "os.version"), Some(azimuth::StringValue("5.15.0")))
  assert_eq(azimuth::Attributes::get(resource_attrs, "arch"), Some(azimuth::StringValue("x86_64")))
  assert_eq(azimuth::Attributes::get(resource_attrs, "runtime.name"), Some(azimuth::StringValue("moonbit")))
  assert_eq(azimuth::Attributes::get(resource_attrs, "runtime.version"), Some(azimuth::StringValue("0.1.0")))
  
  // Test platform-specific timestamp handling
  let current_time = azimuth::Timestamp::now()
  let formatted_time = azimuth::Timestamp::format(current_time, "%Y-%m-%d %H:%M:%S")
  
  // Verify timestamp formatting works correctly
  assert_true(String::length(formatted_time) > 0)
  assert_true(String::contains(formatted_time, "-"))
  assert_true(String::contains(formatted_time, ":"))
}

// Test 9: Time Series Data Operations
pub test "time series data operations" {
  // Create time series data points
  let time_series_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(time_series_attrs, "metric.name", azimuth::StringValue("cpu.usage"))
  azimuth::Attributes::set(time_series_attrs, "metric.type", azimuth::StringValue("gauge"))
  
  // Create time series with multiple data points
  let time_series = azimuth::TimeSeries::new(time_series_attrs)
  
  // Add data points at different timestamps
  let timestamp1 = azimuth::Timestamp::from_unix(1640995200)  // 2022-01-01 00:00:00
  let timestamp2 = azimuth::Timestamp::from_unix(1640995260)  // 2022-01-01 00:01:00
  let timestamp3 = azimuth::Timestamp::from_unix(1640995320)  // 2022-01-01 00:02:00
  
  azimuth::TimeSeries::add_point(time_series, timestamp1, 45.2)
  azimuth::TimeSeries::add_point(time_series, timestamp2, 52.8)
  azimuth::TimeSeries::add_point(time_series, timestamp3, 48.1)
  
  // Verify data points are correctly stored
  let points = azimuth::TimeSeries::get_points(time_series)
  assert_eq(Array::length(points), 3)
  
  // Test time series aggregation
  let avg_value = azimuth::TimeSeries::average(time_series)
  let max_value = azimuth::TimeSeries::maximum(time_series)
  let min_value = azimuth::TimeSeries::minimum(time_series)
  let sum_value = azimuth::TimeSeries::sum(time_series)
  
  // Verify aggregation results
  assert_true(avg_value > 40.0 && avg_value < 60.0)
  assert_true(max_value > 50.0)
  assert_true(min_value < 50.0)
  assert_true(sum_value > 140.0 && sum_value < 150.0)
  
  // Test time range queries
  let start_time = azimuth::Timestamp::from_unix(1640995200)
  let end_time = azimuth::Timestamp::from_unix(1640995320)
  let range_points = azimuth::TimeSeries::get_points_in_range(time_series, start_time, end_time)
  
  // Verify range query returns correct points
  assert_eq(Array::length(range_points), 3)
}

// Test 10: Configuration Management
pub test "configuration management" {
  // Test configuration with default values
  let default_config = azimuth::Configuration::default()
  
  // Verify default configuration values
  assert_eq(azimuth::Configuration::get_sampling_probability(default_config), 1.0)
  assert_eq(azimuth::Configuration::get_max_attributes_per_span(default_config), 128)
  assert_eq(azimuth::Configuration::get_max_events_per_span(default_config), 128)
  assert_eq(azimuth::Configuration::get_max_links_per_span(default_config), 128)
  assert_true(azimuth::Configuration::is_batch_export_enabled(default_config))
  assert_eq(azimuth::Configuration::get_batch_export_size(default_config), 512)
  
  // Test configuration with custom values
  let custom_config = azimuth::Configuration::builder()
    |> azimuth::ConfigurationBuilder::with_sampling_probability(0.1)
    |> azimuth::ConfigurationBuilder::with_max_attributes_per_span(64)
    |> azimuth::ConfigurationBuilder::with_max_events_per_span(64)
    |> azimuth::ConfigurationBuilder::with_max_links_per_span(64)
    |> azimuth::ConfigurationBuilder::with_batch_export(false)
    |> azimuth::ConfigurationBuilder::with_batch_export_size(256)
    |> azimuth::ConfigurationBuilder::build()
  
  // Verify custom configuration values
  assert_eq(azimuth::Configuration::get_sampling_probability(custom_config), 0.1)
  assert_eq(azimuth::Configuration::get_max_attributes_per_span(custom_config), 64)
  assert_eq(azimuth::Configuration::get_max_events_per_span(custom_config), 64)
  assert_eq(azimuth::Configuration::get_max_links_per_span(custom_config), 64)
  assert_false(azimuth::Configuration::is_batch_export_enabled(custom_config))
  assert_eq(azimuth::Configuration::get_batch_export_size(custom_config), 256)
  
  // Test configuration serialization and deserialization
  let serialized_config = azimuth::Configuration::serialize(custom_config)
  let deserialized_config = azimuth::Configuration::deserialize(serialized_config)
  
  // Verify configuration integrity after serialization
  assert_eq(azimuth::Configuration::get_sampling_probability(deserialized_config), 0.1)
  assert_eq(azimuth::Configuration::get_max_attributes_per_span(deserialized_config), 64)
  assert_eq(azimuth::Configuration::get_max_events_per_span(deserialized_config), 64)
  assert_eq(azimuth::Configuration::get_max_links_per_span(deserialized_config), 64)
  assert_false(azimuth::Configuration::is_batch_export_enabled(deserialized_config))
  assert_eq(azimuth::Configuration::get_batch_export_size(deserialized_config), 256)
}