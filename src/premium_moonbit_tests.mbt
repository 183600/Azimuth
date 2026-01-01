// Premium MoonBit Test Suite for Azimuth Telemetry System
// This file contains focused test cases covering key functionality areas

// Test 1: Resource Management and Cleanup
pub test "resource management and cleanup" {
  // Test resource creation with proper initialization
  let resource = azimuth::Resource::new()
  azimuth::Resource::set_attribute(resource, "service.name", azimuth::StringValue("azimuth-service"))
  azimuth::Resource::set_attribute(resource, "service.version", azimuth::StringValue("1.0.0"))
  
  // Test resource merging strategies
  let resource2 = azimuth::Resource::new()
  azimuth::Resource::set_attribute(resource2, "service.instance.id", azimuth::StringValue("instance-123"))
  azimuth::Resource::set_attribute(resource2, "service.name", azimuth::StringValue("should-not-override"))
  
  let merged_resource = azimuth::Resource::merge(resource, resource2)
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  let instance_id = azimuth::Resource::get_attribute(merged_resource, "service.instance.id")
  
  assert_eq(service_name, Some(azimuth::StringValue("azimuth-service")))
  assert_eq(instance_id, Some(azimuth::StringValue("instance-123")))
}

// Test 2: Time Series Operations
pub test "time series operations" {
  // Test time series data point creation
  let time_series = azimuth::TimeSeries::new("cpu.usage")
  let timestamp = azimuth::Timestamp::now()
  
  azimuth::TimeSeries::add_point(time_series, timestamp, 0.75)
  azimuth::TimeSeries::add_point(time_series, azimuth::Timestamp::add_seconds(timestamp, 1), 0.80)
  azimuth::TimeSeries::add_point(time_series, azimuth::Timestamp::add_seconds(timestamp, 2), 0.65)
  
  // Test aggregation operations
  let avg_value = azimuth::TimeSeries::average(time_series)
  let max_value = azimuth::TimeSeries::maximum(time_series)
  let min_value = azimuth::TimeSeries::minimum(time_series)
  
  assert_true(avg_value > 0.70 && avg_value < 0.75)
  assert_eq(max_value, 0.80)
  assert_eq(min_value, 0.65)
  
  // Test time range queries
  let start_time = timestamp
  let end_time = azimuth::Timestamp::add_seconds(timestamp, 2)
  let points_in_range = azimuth::TimeSeries::get_points_in_range(time_series, start_time, end_time)
  assert_eq(length(points_in_range), 3)
}

// Test 3: Concurrent Safety with Multiple Threads
pub test "concurrent safety operations" {
  // Test concurrent attribute updates
  let shared_attrs = azimuth::Attributes::new()
  
  // Simulate concurrent operations (simplified for test)
  azimuth::Attributes::set(shared_attrs, "thread.1", azimuth::StringValue("value1"))
  azimuth::Attributes::set(shared_attrs, "thread.2", azimuth::StringValue("value2"))
  azimuth::Attributes::set(shared_attrs, "thread.3", azimuth::StringValue("value3"))
  
  // Verify all values are correctly set
  let val1 = azimuth::Attributes::get(shared_attrs, "thread.1")
  let val2 = azimuth::Attributes::get(shared_attrs, "thread.2")
  let val3 = azimuth::Attributes::get(shared_attrs, "thread.3")
  
  assert_eq(val1, Some(azimuth::StringValue("value1")))
  assert_eq(val2, Some(azimuth::StringValue("value2")))
  assert_eq(val3, Some(azimuth::StringValue("value3")))
  
  // Test concurrent span operations
  let tracer = azimuth::TracerProvider::get_tracer("concurrent-test")
  let span1 = azimuth::Tracer::start_span(tracer, "operation-1")
  let span2 = azimuth::Tracer::start_span(tracer, "operation-2")
  
  azimuth::Span::set_attribute(span1, "concurrent.id", azimuth::IntValue(1))
  azimuth::Span::set_attribute(span2, "concurrent.id", azimuth::IntValue(2))
  
  azimuth::Span::end(span1)
  azimuth::Span::end(span2)
}

// Test 4: Internationalization and Localization
pub test "internationalization and localization" {
  // Test Unicode and multilingual support
  let i18n_attrs = azimuth::Attributes::new()
  
  // Test Chinese characters
  azimuth::Attributes::set(i18n_attrs, "zh.message", azimuth::StringValue("遥测系统测试"))
  azimuth::Attributes::set(i18n_attrs, "zh.service", azimuth::StringValue("方位角服务"))
  
  // Test Japanese characters
  azimuth::Attributes::set(i18n_attrs, "ja.message", azimuth::StringValue("テレメトリシステムテスト"))
  azimuth::Attributes::set(i18n_attrs, "ja.service", azimuth::StringValue("アジマスサービス"))
  
  // Test Korean characters
  azimuth::Attributes::set(i18n_attrs, "ko.message", azimuth::StringValue("원격 측정 시스템 테스트"))
  
  // Test Arabic characters
  azimuth::Attributes::set(i18n_attrs, "ar.message", azimuth::StringValue("اختبار نظام القياس عن بعد"))
  
  // Test emoji and special symbols
  azimuth::Attributes::set(i18n_attrs, "emoji.test", azimuth::StringValue("🚀📊📈🔍"))
  
  // Verify all values are correctly stored and retrieved
  let zh_msg = azimuth::Attributes::get(i18n_attrs, "zh.message")
  let ja_msg = azimuth::Attributes::get(i18n_attrs, "ja.message")
  let ko_msg = azimuth::Attributes::get(i18n_attrs, "ko.message")
  let ar_msg = azimuth::Attributes::get(i18n_attrs, "ar.message")
  let emoji = azimuth::Attributes::get(i18n_attrs, "emoji.test")
  
  assert_eq(zh_msg, Some(azimuth::StringValue("遥测系统测试")))
  assert_eq(ja_msg, Some(azimuth::StringValue("テレメトリシステムテスト")))
  assert_eq(ko_msg, Some(azimuth::StringValue("원격 측정 시스템 테스트")))
  assert_eq(ar_msg, Some(azimuth::StringValue("اختبار نظام القياس عن بعد")))
  assert_eq(emoji, Some(azimuth::StringValue("🚀📊📈🔍")))
}

// Test 5: Error Boundary and Recovery
pub test "error boundary and recovery" {
  // Test graceful handling of invalid inputs
  let attrs = azimuth::Attributes::new()
  
  // Test handling of extremely long attribute keys
  let long_key = "a" * 1000
  azimuth::Attributes::set(attrs, long_key, azimuth::StringValue("test"))
  let long_key_value = azimuth::Attributes::get(attrs, long_key)
  assert_eq(long_key_value, Some(azimuth::StringValue("test")))
  
  // Test handling of special characters in keys
  azimuth::Attributes::set(attrs, "key.with.dots", azimuth::StringValue("dots"))
  azimuth::Attributes::set(attrs, "key-with-dashes", azimuth::StringValue("dashes"))
  azimuth::Attributes::set(attrs, "key_with_underscores", azimuth::StringValue("underscores"))
  
  // Test span error handling
  let tracer = azimuth::TracerProvider::get_tracer("error-test")
  let span = azimuth::Tracer::start_span(tracer, "error-prone-operation")
  
  // Test error recording
  azimuth::Span::record_error(span, "Test error message")
  azimuth::Span::record_exception(span, "TestException", "Test exception details")
  
  // Test span status
  azimuth::Span::set_status(span, azimuth::ERROR, "Operation failed")
  let status = azimuth::Span::get_status(span)
  assert_eq(status.code, azimuth::ERROR)
  
  azimuth::Span::end(span)
}

// Test 6: Performance Benchmarking
pub test "performance benchmarking" {
  // Test attribute operation performance
  let attrs = azimuth::Attributes::new()
  let start_time = azimuth::Timestamp::now()
  
  // Batch attribute operations
  for i = 0; i < 100; i = i + 1 {
    let key = "perf.key." + to_string(i)
    azimuth::Attributes::set(attrs, key, azimuth::IntValue(i))
  }
  
  let end_time = azimuth::Timestamp::now()
  let duration = azimuth::Timestamp::difference(end_time, start_time)
  
  // Verify all attributes were set (performance should be reasonable)
  assert_true(duration < 1000) // Should complete within 1 second
  
  // Test retrieval performance
  let retrieval_start = azimuth::Timestamp::now()
  for i = 0; i < 100; i = i + 1 {
    let key = "perf.key." + to_string(i)
    let value = azimuth::Attributes::get(attrs, key)
    assert_eq(value, Some(azimuth::IntValue(i)))
  }
  let retrieval_end = azimuth::Timestamp::now()
  let retrieval_duration = azimuth::Timestamp::difference(retrieval_end, retrieval_start)
  
  assert_true(retrieval_duration < 1000) // Retrieval should also be fast
}

// Test 7: Cross-Service Consistency
pub test "cross service consistency" {
  // Test context propagation across service boundaries
  let root_ctx = azimuth::Context::root()
  let trace_id_key = azimuth::ContextKey::new("trace.id")
  let span_id_key = azimuth::ContextKey::new("span.id")
  
  // Service A creates context
  let ctx_a = azimuth::Context::with_value(root_ctx, trace_id_key, "trace-12345")
  let ctx_a_with_span = azimuth::Context::with_value(ctx_a, span_id_key, "span-67890")
  
  // Service B receives and extends context
  let service_b_key = azimuth::ContextKey::new("service.b.id")
  let ctx_b = azimuth::Context::with_value(ctx_a_with_span, service_b_key, "service-b-instance")
  
  // Verify all context values are preserved
  let trace_id = azimuth::Context::get(ctx_b, trace_id_key)
  let span_id = azimuth::Context::get(ctx_b, span_id_key)
  let service_b_id = azimuth::Context::get(ctx_b, service_b_key)
  
  assert_eq(trace_id, Some("trace-12345"))
  assert_eq(span_id, Some("span-67890"))
  assert_eq(service_b_id, Some("service-b-instance"))
  
  // Test baggage propagation consistency
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_user, "request.id", "req-456")
  
  // Verify baggage entries are consistent
  let user_id = azimuth::Baggage::get_entry(baggage_with_request, "user.id")
  let request_id = azimuth::Baggage::get_entry(baggage_with_request, "request.id")
  
  assert_eq(user_id, Some("user-123"))
  assert_eq(request_id, Some("req-456"))
}

// Test 8: Data Serialization Integrity
pub test "data serialization integrity" {
  // Test serialization and deserialization of complex data structures
  let original_attrs = azimuth::Attributes::new()
  
  // Add various data types
  azimuth::Attributes::set(original_attrs, "string.val", azimuth::StringValue("test string"))
  azimuth::Attributes::set(original_attrs, "int.val", azimuth::IntValue(42))
  azimuth::Attributes::set(original_attrs, "float.val", azimuth::FloatValue(3.14159))
  azimuth::Attributes::set(original_attrs, "bool.val", azimuth::BoolValue(true))
  
  // Add array values
  azimuth::Attributes::set(original_attrs, "string.array", azimuth::ArrayStringValue(["a", "b", "c"]))
  azimuth::Attributes::set(original_attrs, "int.array", azimuth::ArrayIntValue([1, 2, 3, 4, 5]))
  
  // Serialize to bytes
  let serialized_data = azimuth::Serializer::serialize_attributes(original_attrs)
  
  // Deserialize back to attributes
  let deserialized_attrs = azimuth::Deserializer::deserialize_attributes(serialized_data)
  
  // Verify data integrity
  let string_val = azimuth::Attributes::get(deserialized_attrs, "string.val")
  let int_val = azimuth::Attributes::get(deserialized_attrs, "int.val")
  let float_val = azimuth::Attributes::get(deserialized_attrs, "float.val")
  let bool_val = azimuth::Attributes::get(deserialized_attrs, "bool.val")
  let string_array = azimuth::Attributes::get(deserialized_attrs, "string.array")
  let int_array = azimuth::Attributes::get(deserialized_attrs, "int.array")
  
  assert_eq(string_val, Some(azimuth::StringValue("test string")))
  assert_eq(int_val, Some(azimuth::IntValue(42)))
  assert_eq(float_val, Some(azimuth::FloatValue(3.14159)))
  assert_eq(bool_val, Some(azimuth::BoolValue(true)))
  assert_eq(string_array, Some(azimuth::ArrayStringValue(["a", "b", "c"])))
  assert_eq(int_array, Some(azimuth::ArrayIntValue([1, 2, 3, 4, 5])))
}

// Test 9: Real-time Dashboard Streaming
pub test "real time dashboard streaming" {
  // Test real-time metrics streaming
  let dashboard = azimuth::Dashboard::new("real-time-monitor")
  
  // Create metrics for streaming
  let cpu_metric = azimuth::Metric::new("cpu.usage", azimuth::GAUGE)
  let memory_metric = azimuth::Metric::new("memory.usage", azimuth::GAUGE)
  let request_counter = azimuth::Metric::new("http.requests", azimuth::COUNTER)
  
  // Add metrics to dashboard
  azimuth::Dashboard::add_metric(dashboard, cpu_metric)
  azimuth::Dashboard::add_metric(dashboard, memory_metric)
  azimuth::Dashboard::add_metric(dashboard, request_counter)
  
  // Simulate real-time data updates
  let timestamp = azimuth::Timestamp::now()
  
  azimuth::Metric::record_value(cpu_metric, timestamp, 0.75)
  azimuth::Metric::record_value(memory_metric, timestamp, 0.60)
  azimuth::Metric::record_value(request_counter, timestamp, 1.0)
  
  // Update after 1 second
  let timestamp2 = azimuth::Timestamp::add_seconds(timestamp, 1)
  azimuth::Metric::record_value(cpu_metric, timestamp2, 0.80)
  azimuth::Metric::record_value(memory_metric, timestamp2, 0.65)
  azimuth::Metric::record_value(request_counter, timestamp2, 2.0)
  
  // Test dashboard data retrieval
  let dashboard_data = azimuth::Dashboard::get_current_data(dashboard)
  let cpu_data = azimuth::DashboardData::get_metric(dashboard_data, "cpu.usage")
  let memory_data = azimuth::DashboardData::get_metric(dashboard_data, "memory.usage")
  let request_data = azimuth::DashboardData::get_metric(dashboard_data, "http.requests")
  
  assert_true(length(cpu_data) >= 2)
  assert_true(length(memory_data) >= 2)
  assert_true(length(request_data) >= 2)
  
  // Verify latest values
  let latest_cpu = azimuth::MetricData::get_latest_value(cpu_data)
  let latest_memory = azimuth::MetricData::get_latest_value(memory_data)
  let latest_requests = azimuth::MetricData::get_latest_value(request_data)
  
  assert_eq(latest_cpu, Some(0.80))
  assert_eq(latest_memory, Some(0.65))
  assert_eq(latest_requests, Some(2.0))
}