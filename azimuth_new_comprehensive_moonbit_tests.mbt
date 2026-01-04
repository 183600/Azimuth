// Azimuth New Comprehensive MoonBit Tests
// This file contains new comprehensive test cases for the Azimuth telemetry system

// Test 1: Advanced Data Structure Operations
test "advanced data structure operations" {
  // Test queue operations
  let mut queue = Queue::new()
  Queue::enqueue(queue, 1)
  Queue::enqueue(queue, 2)
  Queue::enqueue(queue, 3)
  
  match Queue::dequeue(queue) {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  assert_eq(Queue::size(queue), 2)
  
  // Test stack operations
  let mut stack = Stack::new()
  Stack::push(stack, "a")
  Stack::push(stack, "b")
  Stack::push(stack, "c")
  
  match Stack::pop(stack) {
    Some(value) => assert_eq(value, "c")
    None => assert_true(false)
  }
  
  assert_eq(Stack::size(stack), 2)
  
  // Test hash map operations
  let mut map = HashMap::new()
  HashMap::insert(map, "key1", "value1")
  HashMap::insert(map, "key2", "value2")
  
  match HashMap::get(map, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  assert_eq(HashMap::size(map), 2)
}

// Test 2: Telemetry Data Aggregation
test "telemetry data aggregation" {
  let aggregator = TelemetryAggregator::new()
  
  // Add some telemetry data points
  TelemetryAggregator::add_metric(aggregator, "cpu_usage", 75.5)
  TelemetryAggregator::add_metric(aggregator, "cpu_usage", 80.2)
  TelemetryAggregator::add_metric(aggregator, "cpu_usage", 65.8)
  
  TelemetryAggregator::add_metric(aggregator, "memory_usage", 1024.0)
  TelemetryAggregator::add_metric(aggregator, "memory_usage", 1536.0)
  
  // Test aggregation results
  let cpu_stats = TelemetryAggregator::get_stats(aggregator, "cpu_usage")
  match cpu_stats {
    Some(stats) => {
      assert_eq(Stats::count(stats), 3)
      assert_eq(Stats::min(stats), 65.8)
      assert_eq(Stats::max(stats), 80.2)
      assert_eq(Stats::average(stats), 73.83333333333333)
    }
    None => assert_true(false)
  }
  
  let memory_stats = TelemetryAggregator::get_stats(aggregator, "memory_usage")
  match memory_stats {
    Some(stats) => {
      assert_eq(Stats::count(stats), 2)
      assert_eq(Stats::min(stats), 1024.0)
      assert_eq(Stats::max(stats), 1536.0)
      assert_eq(Stats::average(stats), 1280.0)
    }
    None => assert_true(false)
  }
}

// Test 3: Trace Context Propagation
test "trace context propagation" {
  // Create a parent span context
  let parent_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let parent_ctx = SpanContext::new(parent_trace_id, parent_span_id, true, "parent_state")
  
  // Create a child span with parent context
  let child_span_id = "b7ad6b7169203332"
  let child_ctx = SpanContext::with_parent(parent_ctx, child_span_id, "child_state")
  
  // Verify trace ID propagation
  assert_eq(SpanContext::trace_id(child_ctx), parent_trace_id)
  assert_eq(SpanContext::span_id(child_ctx), child_span_id)
  assert_eq(SpanContext::parent_span_id(child_ctx), Some(parent_span_id))
  
  // Create a grandchild span
  let grandchild_span_id = "b7ad6b7169203333"
  let grandchild_ctx = SpanContext::with_parent(child_ctx, grandchild_span_id, "grandchild_state")
  
  // Verify multi-level propagation
  assert_eq(SpanContext::trace_id(grandchild_ctx), parent_trace_id)
  assert_eq(SpanContext::span_id(grandchild_ctx), grandchild_span_id)
  assert_eq(SpanContext::parent_span_id(grandchild_ctx), Some(child_span_id))
}

// Test 4: Telemetry Data Serialization
test "telemetry data serialization" {
  // Create a span with attributes
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Add attributes and events
  Span::set_attribute(span, "user.id", StringValue("user123"))
  Span::set_attribute(span, "request.size", IntValue(1024))
  Span::add_event(span, "request_start", None)
  Span::add_event(span, "request_end", Some([("duration", IntValue(250)]))
  
  // Serialize the span
  let serialized = SpanSerializer::serialize(span)
  
  // Deserialize the span
  match SpanDeserializer::deserialize(serialized) {
    Some(deserialized_span) => {
      assert_eq(Span::name(deserialized_span), "test_span")
      assert_eq(Span::span_context(deserialized_span), span_ctx)
      
      // Verify attributes
      let user_id = Span::get_attribute(deserialized_span, "user.id")
      match user_id {
        Some(StringValue(id)) => assert_eq(id, "user123")
        _ => assert_true(false)
      }
      
      let request_size = Span::get_attribute(deserialized_span, "request.size")
      match request_size {
        Some(IntValue(size)) => assert_eq(size, 1024)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 5: Performance Metrics Collection
test "performance metrics collection" {
  let metrics_collector = PerformanceMetricsCollector::new()
  
  // Start performance measurement
  let measurement_id = PerformanceMetricsCollector::start_measurement(metrics_collector, "operation_1")
  
  // Simulate some work
  let mut sum = 0
  for i in 0..=1000 {
    sum = sum + i
  }
  
  // End performance measurement
  let metrics = PerformanceMetricsCollector::end_measurement(metrics_collector, measurement_id)
  
  // Verify metrics
  match metrics {
    Some(m) => {
      assert_eq(Metrics::operation_name(m), "operation_1")
      assert_true(Metrics::duration_ms(m) > 0)
      assert_eq(Metrics::result(m), Some(500500)) // Sum of 0..1000
    }
    None => assert_true(false)
  }
  
  // Test multiple measurements
  let measurement_id_2 = PerformanceMetricsCollector::start_measurement(metrics_collector, "operation_2")
  let measurement_id_3 = PerformanceMetricsCollector::start_measurement(metrics_collector, "operation_3")
  
  PerformanceMetricsCollector::end_measurement(metrics_collector, measurement_id_2)
  PerformanceMetricsCollector::end_measurement(metrics_collector, measurement_id_3)
  
  // Get aggregated metrics
  let all_metrics = PerformanceMetricsCollector::get_all_metrics(metrics_collector)
  assert_eq(all_metrics.length(), 3)
}

// Test 6: Error Handling and Recovery
test "error handling and recovery" {
  let error_handler = ErrorHandler::new()
  
  // Test error reporting
  let error = Error::new("test_error", "This is a test error", ErrorSeverity::Warning)
  ErrorHandler::report(error_handler, error)
  
  // Test error recovery strategy
  let recovery_strategy = RetryStrategy::new(3, 100) // 3 retries, 100ms delay
  
  let mut attempt_count = 0
  let result = ErrorHandler::execute_with_recovery(error_handler, recovery_strategy, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Error::new("retry_error", "Operation failed, will retry", ErrorSeverity::Error)
    } else {
      "success"
    }
  })
  
  match result {
    Ok(value) => {
      assert_eq(value, "success")
      assert_eq(attempt_count, 3)
    }
    Err(_) => assert_true(false)
  }
  
  // Test error statistics
  let stats = ErrorHandler::get_error_stats(error_handler)
  assert_eq(ErrorStats::total_errors(stats), 1)
  assert_eq(ErrorStats::errors_by_type(stats, "test_error"), 1)
}

// Test 7: Time Series Data Processing
test "time series data processing" {
  let time_series_processor = TimeSeriesProcessor::new()
  
  // Add time series data points
  let timestamp1 = 1609459200L // 2021-01-01 00:00:00 UTC
  let timestamp2 = 1609459260L // 2021-01-01 00:01:00 UTC
  let timestamp3 = 1609459320L // 2021-01-01 00:02:00 UTC
  
  TimeSeriesProcessor::add_data_point(time_series_processor, "cpu_usage", timestamp1, 50.0)
  TimeSeriesProcessor::add_data_point(time_series_processor, "cpu_usage", timestamp2, 75.0)
  TimeSeriesProcessor::add_data_point(time_series_processor, "cpu_usage", timestamp3, 60.0)
  
  // Test time range query
  let start_time = 1609459200L
  let end_time = 1609459320L
  let data_points = TimeSeriesProcessor::get_data_points(time_series_processor, "cpu_usage", start_time, end_time)
  
  assert_eq(data_points.length(), 3)
  assert_eq(data_points[0].timestamp, timestamp1)
  assert_eq(data_points[0].value, 50.0)
  assert_eq(data_points[1].timestamp, timestamp2)
  assert_eq(data_points[1].value, 75.0)
  assert_eq(data_points[2].timestamp, timestamp3)
  assert_eq(data_points[2].value, 60.0)
  
  // Test aggregation
  let avg_value = TimeSeriesProcessor::aggregate(time_series_processor, "cpu_usage", start_time, end_time, AggregationType::Average)
  match avg_value {
    Some(value) => assert_eq(value, 61.666666666666664)
    None => assert_true(false)
  }
}

// Test 8: Configuration Management
test "configuration management" {
  let config_manager = ConfigurationManager::new()
  
  // Set configuration values
  ConfigurationManager::set(config_manager, "telemetry.enabled", BoolValue(true))
  ConfigurationManager::set(config_manager, "telemetry.sampling_rate", FloatValue(0.1))
  ConfigurationManager::set(config_manager, "telemetry.max_spans", IntValue(1000))
  ConfigurationManager::set(config_manager, "telemetry.exporter", StringValue("jaeger"))
  
  // Get configuration values
  match ConfigurationManager::get(config_manager, "telemetry.enabled") {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "telemetry.sampling_rate") {
    Some(FloatValue(value)) => assert_eq(value, 0.1)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "telemetry.max_spans") {
    Some(IntValue(value)) => assert_eq(value, 1000)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "telemetry.exporter") {
    Some(StringValue(value)) => assert_eq(value, "jaeger")
    _ => assert_true(false)
  }
  
  // Test default values
  match ConfigurationManager::get(config_manager, "non.existent.key") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test configuration with defaults
  let default_value = ConfigurationManager::get_with_default(config_manager, "non.existent.key", StringValue("default"))
  match default_value {
    StringValue(value) => assert_eq(value, "default")
    _ => assert_true(false)
  }
}

// Test 9: Concurrent Telemetry Processing
test "concurrent telemetry processing" {
  let processor = ConcurrentTelemetryProcessor::new(4) // 4 worker threads
  
  // Create telemetry data
  let telemetry_data = [
    ("trace1", "span1", "operation1", 100),
    ("trace2", "span2", "operation2", 200),
    ("trace3", "span3", "operation3", 300),
    ("trace4", "span4", "operation4", 400),
    ("trace5", "span5", "operation5", 500)
  ]
  
  // Process telemetry data concurrently
  let mut results = []
  for data in telemetry_data {
    let result = ConcurrentTelemetryProcessor::process_async(processor, data.0, data.1, data.2, data.3)
    results.push(result)
  }
  
  // Wait for all processing to complete
  let processed_results = ConcurrentTelemetryProcessor::wait_for_all(processor, results)
  
  // Verify results
  assert_eq(processed_results.length(), 5)
  
  for result in processed_results {
    match result {
      ProcessedResult(data) => {
        assert_true(data.trace_id != "")
        assert_true(data.span_id != "")
        assert_true(data.operation_name != "")
        assert_true(data.duration_ms > 0)
      }
      Error(_) => assert_true(false)
    }
  }
  
  // Test processor statistics
  let stats = ConcurrentTelemetryProcessor::get_stats(processor)
  assert_eq(ProcessorStats::processed_count(stats), 5)
  assert_eq(ProcessorStats::error_count(stats), 0)
}

// Test 10: Adaptive Sampling Strategy
test "adaptive sampling strategy" {
  let adaptive_sampler = AdaptiveSampler::new()
  
  // Configure initial sampling rate
  AdaptiveSampler::set_base_sampling_rate(adaptive_sampler, 0.1) // 10%
  
  // Test sampling decisions
  let mut sampled_count = 0
  let total_requests = 1000
  
  for i in 0..<total_requests {
    let trace_id = "trace_" + i.to_string()
    let decision = AdaptiveSampler::should_sample(adaptive_sampler, trace_id)
    if decision {
      sampled_count = sampled_count + 1
    }
  }
  
  // Verify sampling rate is approximately 10%
  let actual_sampling_rate = sampled_count.to_float() / total_requests.to_float()
  assert_true(actual_sampling_rate > 0.05 && actual_sampling_rate < 0.15)
  
  // Test adaptive adjustment based on throughput
  AdaptiveSampler::adjust_sampling_rate(adaptive_sampler, 1000) // High throughput
  let adjusted_rate = AdaptiveSampler::get_current_sampling_rate(adaptive_sampler)
  assert_true(adjusted_rate < 0.1) // Should decrease sampling rate under high throughput
  
  // Test adaptive adjustment based on error rate
  AdaptiveSampler::adjust_sampling_rate(adaptive_sampler, 100) // Low throughput
  AdaptiveSampler::report_error_rate(adaptive_sampler, 0.05) // 5% error rate
  let adjusted_rate_after_errors = AdaptiveSampler::get_current_sampling_rate(adaptive_sampler)
  assert_true(adjusted_rate_after_errors > adjusted_rate) // Should increase sampling rate with errors
}