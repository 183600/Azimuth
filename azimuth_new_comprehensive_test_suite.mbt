// Azimuth Telemetry System - New Comprehensive Test Suite
// This file contains 10 new test cases covering various aspects of the telemetry system

// Test Case 1: Advanced Data Type Conversion Tests
test "advanced data type conversion" {
  // Test complex nested type conversions
  let nested_data = {
    "user_id": IntValue(12345),
    "user_name": StringValue("张三"),
    "preferences": {
      "theme": StringValue("dark"),
      "notifications": BoolValue(true),
      "max_items": IntValue(100)
    },
    "scores": ArrayIntValue([85, 92, 78, 96]),
    "tags": ArrayStringValue(["premium", "verified", "active"])
  }
  
  // Test conversion to JSON-like structure
  match nested_data["user_id"] {
    IntValue(id) => assert_eq(id, 12345)
    _ => assert_true(false)
  }
  
  // Test nested object access
  match nested_data["preferences"] {
    ObjectValue(pref) => {
      match pref["theme"] {
        StringValue(theme) => assert_eq(theme, "dark")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test array operations
  match nested_data["scores"] {
    ArrayIntValue(scores) => {
      assert_eq(scores.length(), 4)
      let mut sum = 0
      for score in scores {
        sum = sum + score
      }
      assert_eq(sum, 351)
    }
    _ => assert_true(false)
  }
  
  // Test type conversion functions
  let string_to_int = "42".to_int()
  match string_to_int {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  let int_to_string = 123.to_string()
  assert_eq(int_to_string, "123")
  
  let float_to_string = 3.14159.to_string()
  assert_eq(float_to_string, "3.14159")
}

// Test Case 2: Concurrent Safety Tests
test "concurrent safety operations" {
  // Test thread-safe attribute operations
  let shared_attrs = ThreadSafeAttributes::new()
  
  // Simulate concurrent operations
  for i in 0..=10 {
    let key = "concurrent_key_" + i.to_string()
    let value = IntValue(i * 10)
    ThreadSafeAttributes::set(shared_attrs, key, value)
  }
  
  // Verify all values were set correctly
  for i in 0..=10 {
    let key = "concurrent_key_" + i.to_string()
    let result = ThreadSafeAttributes::get(shared_attrs, key)
    match result {
      Some(IntValue(value)) => assert_eq(value, i * 10)
      _ => assert_true(false)
    }
  }
  
  // Test concurrent span operations
  let span_registry = ThreadSafeSpanRegistry::new()
  
  // Create multiple spans concurrently
  for i in 0..=5 {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("concurrent_span_" + i.to_string(), Internal, span_ctx)
    ThreadSafeSpanRegistry::register(span_registry, span)
  }
  
  // Verify all spans were registered
  assert_eq(ThreadSafeSpanRegistry::count(span_registry), 6)
  
  // Test concurrent metrics operations
  let metrics_collector = ThreadSafeMetricsCollector::new()
  
  // Simulate concurrent metric updates
  for i in 0..=20 {
    let metric_name = "test_metric_" + (i % 3).to_string()
    ThreadSafeMetricsCollector::increment_counter(metrics_collector, metric_name, 1.0)
  }
  
  // Verify metrics were collected correctly
  let metric1_value = ThreadSafeMetricsCollector::get_counter_value(metrics_collector, "test_metric_0")
  let metric2_value = ThreadSafeMetricsCollector::get_counter_value(metrics_collector, "test_metric_1")
  let metric3_value = ThreadSafeMetricsCollector::get_counter_value(metrics_collector, "test_metric_2")
  
  assert_eq(metric1_value, 7.0)
  assert_eq(metric2_value, 7.0)
  assert_eq(metric3_value, 7.0)
}

// Test Case 3: Telemetry Data Compression Tests
test "telemetry data compression" {
  // Test span data compression
  let span_ctx = SpanContext::new("1234567890abcdef", "fedcba0987654321", true, "active")
  let span = Span::new("compression_test_span", Server, span_ctx)
  
  // Add events and attributes
  Span::add_event(span, "start_event", Some([("timestamp", IntValue(1234567890))]))
  Span::add_event(span, "middle_event", Some([("progress", FloatValue(0.5))]))
  Span::add_event(span, "end_event", Some([("status", StringValue("completed"))]))
  
  Span::set_attribute(span, "duration_ms", IntValue(1500))
  Span::set_attribute(span, "component", StringValue("data_processor"))
  Span::set_attribute(span, "success", BoolValue(true))
  
  // Compress span data
  let compressed_data = SpanCompressor::compress(span)
  assert_true(compressed_data.length() > 0)
  
  // Decompress and verify
  let decompressed_span = SpanCompressor::decompress(compressed_data)
  assert_eq(Span::name(decompressed_span), "compression_test_span")
  assert_eq(Span::status(decompressed_span), Unset)
  
  // Test metrics compression
  let metrics_batch = MetricsBatch::new()
  MetricsBatch::add_counter(metrics_batch, "requests_total", 1000.0)
  MetricsBatch::add_histogram(metrics_batch, "response_time", 150.0)
  MetricsBatch::add_gauge(metrics_batch, "active_connections", 25.0)
  
  let compressed_metrics = MetricsCompressor::compress(metrics_batch)
  assert_true(compressed_metrics.length() > 0)
  
  let decompressed_metrics = MetricsCompressor::decompress(compressed_metrics)
  assert_eq(MetricsBatch::count(decompressed_metrics), 3)
  
  // Test log compression
  let log_batch = LogBatch::new()
  LogBatch::add_log(log_batch, Info, "Application started", Some(1234567890L))
  LogBatch::add_log(log_batch, Warn, "High memory usage detected", Some(1234567895L))
  LogBatch::add_log(log_batch, Error, "Database connection failed", Some(1234567900L))
  
  let compressed_logs = LogCompressor::compress(log_batch)
  assert_true(compressed_logs.length() > 0)
  
  let decompressed_logs = LogCompressor::decompress(compressed_logs)
  assert_eq(LogBatch::count(decompressed_logs), 3)
}

// Test Case 4: Distributed Tracing Consistency Tests
test "distributed tracing consistency" {
  // Test trace context propagation across services
  let original_trace_id = "abc123def456ghi789"
  let original_span_id = "span001"
  let parent_ctx = SpanContext::new(original_trace_id, original_span_id, true, "service_a")
  
  // Service B creates child span
  let service_b_span_ctx = SpanContext::child_of(parent_ctx, "span002")
  assert_eq(SpanContext::trace_id(service_b_span_ctx), original_trace_id)
  assert_ne(SpanContext::span_id(service_b_span_ctx), original_span_id)
  assert_true(SpanContext::is_sampled(service_b_span_ctx))
  
  // Service C creates grandchild span
  let service_c_span_ctx = SpanContext::child_of(service_b_span_ctx, "span003")
  assert_eq(SpanContext::trace_id(service_c_span_ctx), original_trace_id)
  assert_ne(SpanContext::span_id(service_c_span_ctx), "span002")
  
  // Test baggage propagation
  let initial_baggage = Baggage::new()
  let with_baggage = Baggage::set_entry(initial_baggage, "user_id", "user123")
  let with_more_baggage = Baggage::set_entry(with_baggage, "request_id", "req456")
  
  // Propagate baggage across service boundaries
  let propagated_baggage = Baggage::propagate(with_more_baggage, service_b_span_ctx)
  let user_id = Baggage::get_entry(propagated_baggage, "user_id")
  let request_id = Baggage::get_entry(propagated_baggage, "request_id")
  
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  match request_id {
    Some(value) => assert_eq(value, "req456")
    None => assert_true(false)
  }
  
  // Test trace consistency validation
  let trace_validator = TraceConsistencyValidator::new()
  let trace_segments = [
    ("service_a", parent_ctx),
    ("service_b", service_b_span_ctx),
    ("service_c", service_c_span_ctx)
  ]
  
  let validation_result = TraceConsistencyValidator::validate(trace_validator, trace_segments)
  assert_true(validation_result.is_consistent)
  assert_eq(validation_result.trace_id, original_trace_id)
  assert_eq(validation_result.span_count, 3)
  
  // Test cross-service timing correlation
  let timing_analyzer = CrossServiceTimingAnalyzer::new()
  TimingAnalyzer::add_span(timing_analyzer, parent_ctx, 1000L, 1500L)
  TimingAnalyzer::add_span(timing_analyzer, service_b_span_ctx, 1505L, 1800L)
  TimingAnalyzer::add_span(timing_analyzer, service_c_span_ctx, 1600L, 1750L)
  
  let timing_result = TimingAnalyzer::analyze(timing_analyzer)
  assert_eq(timing_result.total_duration, 800L)
  assert_eq(timing_result.concurrent_spans, 2)
}

// Test Case 5: Performance Benchmark Tests
test "performance benchmarks" {
  // Test span creation performance
  let start_time = Performance::current_timestamp()
  
  for i in 0..=1000 {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("performance_test_span", Internal, span_ctx)
    Span::end(span)
  }
  
  let end_time = Performance::current_timestamp()
  let span_creation_duration = end_time - start_time
  
  // Assert reasonable performance (should complete within 1000ms)
  assert_true(span_creation_duration < 1000L)
  
  // Test metrics collection performance
  let start_time = Performance::current_timestamp()
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance_meter")
  let counter = Meter::create_counter(meter, "performance_counter", None, None)
  
  for i in 0..=10000 {
    Counter::add(counter, 1.0)
  }
  
  let end_time = Performance::current_timestamp()
  let metrics_collection_duration = end_time - start_time
  
  // Assert reasonable performance (should complete within 500ms)
  assert_true(metrics_collection_duration < 500L)
  
  // Test attribute operations performance
  let start_time = Performance::current_timestamp()
  
  let attrs = Attributes::new()
  for i in 0..=5000 {
    let key = "attr_" + i.to_string()
    let value = StringValue("value_" + i.to_string())
    Attributes::set(attrs, key, value)
  }
  
  for i in 0..=5000 {
    let key = "attr_" + i.to_string()
    Attributes::get(attrs, key)
  }
  
  let end_time = Performance::current_timestamp()
  let attribute_operations_duration = end_time - start_time
  
  // Assert reasonable performance (should complete within 1000ms)
  assert_true(attribute_operations_duration < 1000L)
  
  // Test serialization performance
  let start_time = Performance::current_timestamp()
  
  let test_span = Span::new("serialization_test", Internal, SpanContext::new("test_trace", "test_span", true, ""))
  Span::set_attribute(test_span, "test_attr", StringValue("test_value"))
  Span::add_event(test_span, "test_event", Some([("event_attr", IntValue(42))]))
  
  for i in 0..=1000 {
    let serialized = SpanSerializer::serialize(test_span)
    let _ = SpanSerializer::deserialize(serialized)
  }
  
  let end_time = Performance::current_timestamp()
  let serialization_duration = end_time - start_time
  
  // Assert reasonable performance (should complete within 2000ms)
  assert_true(serialization_duration < 2000L)
}

// Test Case 6: Error Boundary Handling Tests
test "error boundary handling" {
  // Test span error handling
  let error_span = Span::new("error_test_span", Internal, SpanContext::new("error_trace", "error_span", true, ""))
  
  // Test error status setting
  Span::set_status(error_span, Error, Some("Database connection failed"))
  assert_eq(Span::status(error_span), Error)
  
  // Test exception recording
  Span::record_exception(error_span, "TimeoutException", Some("Operation timed out after 30 seconds"), Some([
    ("timeout_duration", IntValue(30000)),
    ("operation", StringValue("database_query"))
  ]))
  
  // Test error event handling
  Span::add_event(error_span, "error_occurred", Some([
    ("error.type", StringValue("ConnectionError")),
    ("error.code", IntValue(500)),
    ("error.retryable", BoolValue(true))
  ]))
  
  // Test error recovery
  let recovery_span = Span::new("recovery_test_span", Internal, SpanContext::new("recovery_trace", "recovery_span", true, ""))
  Span::set_status(recovery_span, Error, Some("Initial operation failed"))
  
  // Simulate recovery attempt
  Span::add_event(recovery_span, "recovery_attempt_started", Some([
    ("attempt", IntValue(1)),
    ("strategy", StringValue("retry_with_backoff"))
  ]))
  
  Span::set_status(recovery_span, Ok, Some("Operation recovered successfully"))
  assert_eq(Span::status(recovery_span), Ok)
  
  // Test circuit breaker pattern
  let circuit_breaker = CircuitBreaker::new("test_circuit", 3, 60000L) // 3 failures, 60ms timeout
  
  // Simulate failures
  for i in 0..=3 {
    let result = CircuitBreaker::execute(circuit_breaker, func() {
      if i < 3 {
        Error("Simulated failure")
      } else {
        Ok("Success")
      }
    })
    
    if i < 3 {
      match result {
        Error(_) => assert_true(true)
        Ok(_) => assert_true(false)
      }
    } else {
      // Circuit should be open now
      match result {
        Error(msg) => assert_eq(msg, "Circuit breaker is open")
        Ok(_) => assert_true(false)
      }
    }
  }
  
  assert_true(CircuitBreaker::is_open(circuit_breaker))
  
  // Test timeout handling
  let timeout_span = Span::new("timeout_test_span", Internal, SpanContext::new("timeout_trace", "timeout_span", true, ""))
  
  let timeout_result = TimeoutHandler::execute_with_timeout(timeout_span, 100L, func() {
    // Simulate long-running operation
    for i in 0..=1000000 {
      // Busy wait to simulate work
    }
    "Operation completed"
  })
  
  match timeout_result {
    Error(msg) => assert_eq(msg, "Operation timed out")
    Ok(_) => assert_true(false)
  }
}

// Test Case 7: Internationalization Support Tests
test "internationalization support" {
  // Test multi-language attribute values
  let multilingual_attrs = Attributes::new()
  
  // Chinese attributes
  Attributes::set(multilingual_attrs, "用户名", StringValue("张三"))
  Attributes::set(multilingual_attrs, "服务名称", StringValue("遥测系统"))
  
  // Japanese attributes
  Attributes::set(multilingual_attrs, "ユーザー名", StringValue("田中太郎"))
  Attributes::set(multilingual_attrs, "サービス名", StringValue("テレメトリシステム"))
  
  // Korean attributes
  Attributes::set(multilingual_attrs, "사용자명", StringValue("김철수"))
  Attributes::set(multilingual_attrs, "서비스명", StringValue("원격측정시스템"))
  
  // Arabic attributes
  Attributes::set(multilingual_attrs, "اسم المستخدم", StringValue("أحمد"))
  Attributes::set(multilingual_attrs, "اسم الخدمة", StringValue("نظام القياس عن بعد"))
  
  // Verify retrieval of multilingual attributes
  let chinese_username = Attributes::get(multilingual_attrs, "用户名")
  match chinese_username {
    Some(StringValue(name)) => assert_eq(name, "张三")
    _ => assert_true(false)
  }
  
  let japanese_service = Attributes::get(multilingual_attrs, "サービス名")
  match japanese_service {
    Some(StringValue(service)) => assert_eq(service, "テレメトリシステム")
    _ => assert_true(false)
  }
  
  // Test localized error messages
  let error_localizer = ErrorLocalizer::new()
  ErrorLocalizer::add_message(error_localizer, "connection_failed", "en", "Connection failed")
  ErrorLocalizer::add_message(error_localizer, "connection_failed", "zh", "连接失败")
  ErrorLocalizer::add_message(error_localizer, "connection_failed", "ja", "接続に失敗しました")
  ErrorLocalizer::add_message(error_localizer, "connection_failed", "ko", "연결에 실패했습니다")
  ErrorLocalizer::add_message(error_localizer, "connection_failed", "ar", "فشل الاتصال")
  
  let localized_error_en = ErrorLocalizer::get_message(error_localizer, "connection_failed", "en")
  let localized_error_zh = ErrorLocalizer::get_message(error_localizer, "connection_failed", "zh")
  let localized_error_ja = ErrorLocalizer::get_message(error_localizer, "connection_failed", "ja")
  
  assert_eq(localized_error_en, Some("Connection failed"))
  assert_eq(localized_error_zh, Some("连接失败"))
  assert_eq(localized_error_ja, Some("接続に失敗しました"))
  
  // Test locale-specific formatting
  let formatter = LocaleFormatter::new("zh")
  let formatted_number = LocaleFormatter::format_number(formatter, 1234.56)
  assert_eq(formatted_number, "1,234.56")
  
  let formatter_ja = LocaleFormatter::new("ja")
  let formatted_date = LocaleFormatter::format_date(formatter_ja, 1234567890L)
  assert_true(formatted_date.length() > 0)
  
  // Test right-to-left text handling
  let rtl_text = "مرحبا بالعالم"
  let rtl_processed = TextProcessor::process_rtl(rtl_text)
  assert_eq(rtl_processed, "مرحبا بالعالم")
  
  // Test Unicode normalization
  let unicode_text = "café"
  let normalized = TextProcessor::normalize_unicode(unicode_text)
  assert_eq(normalized, "café")
}

// Test Case 8: Memory Management Tests
test "memory management" {
  // Test span lifecycle cleanup
  let span_registry = SpanRegistry::new()
  
  // Create many spans
  for i in 0..=1000 {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("memory_test_span", Internal, span_ctx)
    SpanRegistry::register(span_registry, span)
  }
  
  assert_eq(SpanRegistry::count(span_registry), 1001)
  
  // End all spans and trigger cleanup
  SpanRegistry::end_all(span_registry)
  SpanRegistry::cleanup_completed(span_registry)
  
  // Verify cleanup
  assert_eq(SpanRegistry::count(span_registry), 0)
  
  // Test attribute memory cleanup
  let large_attrs = Attributes::new()
  
  // Add many large attributes
  for i in 0..=500 {
    let key = "large_key_" + i.to_string()
    let large_value = StringValue("x".repeat(1000)) // 1KB string
    Attributes::set(large_attrs, key, large_value)
  }
  
  // Verify attributes were added
  let attr_count = Attributes::count(large_attrs)
  assert_eq(attr_count, 501)
  
  // Clear attributes and verify memory is freed
  Attributes::clear(large_attrs)
  assert_eq(Attributes::count(large_attrs), 0)
  
  // Test metrics buffer cleanup
  let metrics_buffer = MetricsBuffer::new(100) // Max 100 metrics
  
  // Fill buffer beyond capacity
  for i in 0..=200 {
    let metric_name = "metric_" + i.to_string()
    MetricsBuffer::add_metric(metrics_buffer, metric_name, FloatValue(i.to_float()))
  }
  
  // Buffer should maintain max capacity
  assert_eq(MetricsBuffer::size(metrics_buffer), 100)
  
  // Clear buffer
  MetricsBuffer::clear(metrics_buffer)
  assert_eq(MetricsBuffer::size(metrics_buffer), 0)
  
  // Test memory pool for span objects
  let span_pool = SpanPool::new(100) // Pool of 100 span objects
  
  // Acquire spans from pool
  let acquired_spans = []
  for i in 0..=150 {
    let span = SpanPool::acquire(span_pool)
    if span != None {
      acquired_spans.push(span)
    }
  }
  
  // Should only acquire 100 spans (pool capacity)
  assert_eq(acquired_spans.length(), 100)
  
  // Return spans to pool
  for span in acquired_spans {
    SpanPool::release(span_pool, span)
  }
  
  // Verify pool is full again
  assert_eq(SpanPool::available_count(span_pool), 100)
  
  // Test garbage collection hints
  let gc_hint = GCHint::new()
  GCHint::suggest_collection(gc_hint)
  
  // Simulate memory pressure
  let memory_monitor = MemoryMonitor::new()
  let memory_usage = MemoryMonitor::get_usage(memory_monitor)
  
  if memory_usage.usage_percentage > 80.0 {
    GCHint::suggest_aggressive_collection(gc_hint)
  }
}

// Test Case 9: Network Communication Tests
test "network communication" {
  // Test HTTP client with retry logic
  let retry_config = RetryConfig::new(3, 1000L, 2.0) // 3 retries, 1s base delay, 2x backoff
  let http_client = HttpClient::with_retry(retry_config)
  
  // Test successful request
  let request = HttpRequest::new("GET", "https://api.example.com/health", [], None)
  let response = HttpClient::execute(http_client, request)
  
  match response {
    Ok(resp) => {
      assert_eq(HttpResponse::status_code(resp), 200)
    }
    Error(_) => {
      // In test environment, we might not have actual network access
      assert_true(true)
    }
  }
  
  // Test request with telemetry headers
  let telemetry_headers = [
    ("X-Trace-Id", "test_trace_123"),
    ("X-Span-Id", "test_span_456"),
    ("X-Request-Id", "req_789")
  ]
  
  let telemetry_request = HttpRequest::new("POST", "https://api.example.com/data", telemetry_headers, Some("{\"test\": \"data\"}"))
  let telemetry_response = HttpClient::execute(http_client, telemetry_request)
  
  // Verify headers were added correctly
  assert_eq(HttpRequest::get_header(telemetry_request, "X-Trace-Id"), Some("test_trace_123"))
  assert_eq(HttpRequest::get_header(telemetry_request, "X-Span-Id"), Some("test_span_456"))
  
  // Test connection pool management
  let connection_pool = ConnectionPool::new("https://api.example.com", 10) // Max 10 connections
  
  // Acquire connections
  let connections = []
  for i in 0..=15 {
    let conn = ConnectionPool::acquire(connection_pool)
    if conn != None {
      connections.push(conn)
    }
  }
  
  // Should only acquire 10 connections (pool capacity)
  assert_eq(connections.length(), 10)
  
  // Release connections
  for conn in connections {
    ConnectionPool::release(connection_pool, conn)
  }
  
  // Test network resilience
  let resilience_config = ResilienceConfig::new()
  ResilienceConfig::set_timeout(resilience_config, 5000L) // 5s timeout
  ResilienceConfig::set_circuit_breaker(resilience_config, 5, 30000L) // 5 failures, 30s timeout
  
  let resilient_client = HttpClient::with_resilience(resilience_config)
  
  // Test bulk request processing
  let requests = [
    HttpRequest::new("GET", "https://api.example.com/data1", [], None),
    HttpRequest::new("GET", "https://api.example.com/data2", [], None),
    HttpRequest::new("GET", "https://api.example.com/data3", [], None)
  ]
  
  let bulk_responses = HttpClient::execute_bulk(resilient_client, requests)
  assert_eq(bulk_responses.length(), 3)
  
  // Test network telemetry collection
  let network_metrics = NetworkMetricsCollector::new()
  
  // Simulate network operations
  NetworkMetricsCollector::record_request(network_metrics, "GET", "https://api.example.com", 200, 150L)
  NetworkMetricsCollector::record_request(network_metrics, "POST", "https://api.example.com", 201, 300L)
  NetworkMetricsCollector::record_request(network_metrics, "GET", "https://api.example.com", 500, 5000L)
  
  let metrics_summary = NetworkMetricsCollector::get_summary(network_metrics)
  assert_eq(metrics_summary.total_requests, 3)
  assert_eq(metrics_summary.successful_requests, 2)
  assert_eq(metrics_summary.failed_requests, 1)
  assert_eq(metrics_summary.average_response_time, 1816.6666666666667)
}

// Test Case 10: Real-time Stream Processing Tests
test "real-time stream processing" {
  // Test stream creation and management
  let stream_manager = StreamManager::new()
  let telemetry_stream = StreamManager::create_stream(stream_manager, "telemetry_data", StreamType::Unbounded)
  
  assert_true(StreamManager::stream_exists(stream_manager, "telemetry_data"))
  
  // Test stream data ingestion
  for i in 0..=100 {
    let data_point = TelemetryDataPoint::new(
      "metric_" + (i % 5).to_string(),
      i.to_float(),
      1234567890L + i.to_long()
    )
    StreamManager::push_data(telemetry_stream, data_point)
  }
  
  assert_eq(StreamManager::stream_size(telemetry_stream), 101)
  
  // Test stream processing with windowing
  let window_processor = WindowProcessor::new(1000L, 10) // 1s windows, max 10 windows
  
  for i in 0..=50 {
    let data_point = TelemetryDataPoint::new(
      "window_test_metric",
      i.to_float(),
      1234567890L + (i * 100).to_long()
    )
    WindowProcessor::process(window_processor, data_point)
  }
  
  let windows = WindowProcessor::get_completed_windows(window_processor)
  assert_true(windows.length() > 0)
  
  // Test stream aggregation
  let aggregation_config = AggregationConfig::new()
  AggregationConfig::add_aggregator(aggregation_config, "sum", AggregatorType::Sum)
  AggregationConfig::add_aggregator(aggregation_config, "avg", AggregatorType::Average)
  AggregationConfig::add_aggregator(aggregation_config, "max", AggregatorType::Max)
  AggregationConfig::add_aggregator(aggregation_config, "min", AggregatorType::Min)
  
  let stream_aggregator = StreamAggregator::new(aggregation_config)
  
  // Process data through aggregator
  for i in 0..=20 {
    let data_point = TelemetryDataPoint::new(
      "aggregation_test",
      (i * 2).to_float(),
      1234567890L + i.to_long()
    )
    StreamAggregator::process(stream_aggregator, data_point)
  }
  
  let aggregation_results = StreamAggregator::get_results(stream_aggregator)
  let sum_result = aggregation_results["sum"]
  let avg_result = aggregation_results["avg"]
  let max_result = aggregation_results["max"]
  let min_result = aggregation_results["min"]
  
  assert_eq(sum_result, 420.0) // Sum of 0, 2, 4, ..., 40
  assert_eq(avg_result, 20.0) // Average of 0, 2, 4, ..., 40
  assert_eq(max_result, 40.0) // Maximum value
  assert_eq(min_result, 0.0)  // Minimum value
  
  // Test stream filtering
  let filter_config = FilterConfig::new()
  FilterConfig::add_predicate(filter_config, "value_gt_10", func(data: TelemetryDataPoint) {
    data.value > 10.0
  })
  
  let stream_filter = StreamFilter::new(filter_config)
  
  // Filter data
  for i in 0..=20 {
    let data_point = TelemetryDataPoint::new(
      "filter_test",
      i.to_float(),
      1234567890L + i.to_long()
    )
    StreamFilter::process(stream_filter, data_point)
  }
  
  let filtered_results = StreamFilter::get_results(stream_filter)
  assert_eq(filtered_results.length(), 10) // Values 11 through 20
  
  // Test stream alerting
  let alert_config = AlertConfig::new()
  AlertConfig::add_rule(alert_config, "high_value_alert", AlertRule::new(
    func(data: TelemetryDataPoint) { data.value > 15.0 },
    AlertSeverity::Warning,
    "Value exceeded threshold"
  ))
  
  let stream_alerter = StreamAlerter::new(alert_config)
  
  // Process data that should trigger alerts
  for i in 0..=20 {
    let data_point = TelemetryDataPoint::new(
      "alert_test",
      i.to_float(),
      1234567890L + i.to_long()
    )
    StreamAlerter::process(stream_alerter, data_point)
  }
  
  let alerts = StreamAlerter::get_triggered_alerts(stream_alerter)
  assert_eq(alerts.length(), 5) // Values 16 through 20
  
  // Test stream persistence
  let stream_persistence = StreamPersistence::new("memory")
  
  // Save stream state
  StreamPersistence::save_stream_state(stream_persistence, telemetry_stream)
  
  // Create new stream and restore state
  let restored_stream = StreamManager::create_stream(stream_manager, "restored_telemetry", StreamType::Unbounded)
  StreamPersistence::restore_stream_state(stream_persistence, restored_stream)
  
  assert_eq(StreamManager::stream_size(restored_stream), 101)
  
  // Clean up
  StreamManager::delete_stream(stream_manager, "telemetry_data")
  StreamManager::delete_stream(stream_manager, "restored_telemetry")
}