// Azimuth Telemetry System - Data Aggregation and Batch Processing Tests
// This file contains comprehensive test cases for telemetry data aggregation and batch processing

// Test 1: Telemetry Data Aggregation with Multiple Metrics
test "telemetry data aggregation with multiple metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_test_meter")
  
  // Create multiple counters for different metrics
  let request_counter = Meter::create_counter(meter, "http_requests_total", Some("Total HTTP requests"), Some("count"))
  let error_counter = Meter::create_counter(meter, "http_errors_total", Some("Total HTTP errors"), Some("count"))
  let response_time_histogram = Meter::create_histogram(meter, "http_response_time_ms", Some("HTTP response time"), Some("ms"))
  
  // Simulate metric collection over time
  for i in 0..=10 {
    Counter::add(request_counter, 1.0)
    
    if i % 3 == 0 {
      Counter::add(error_counter, 1.0)
    }
    
    Histogram::record(response_time_histogram, (i * 10.0) + 50.0)
  }
  
  // Test aggregation functionality
  let aggregator = MetricAggregator::new()
  MetricAggregator::add_instrument(aggregator, Counter::as_instrument(request_counter))
  MetricAggregator::add_instrument(aggregator, Counter::as_instrument(error_counter))
  MetricAggregator::add_instrument(aggregator, Histogram::as_instrument(response_time_histogram))
  
  // Retrieve aggregated data
  let aggregated_metrics = MetricAggregator::get_aggregated_data(aggregator)
  assert_eq(aggregated_metrics.length(), 3)
  
  // Verify request counter aggregation
  let request_data = MetricAggregator::get_metric_data(aggregator, "http_requests_total")
  match request_data {
    Some(data) => {
      match data {
        CounterData(total) => assert_eq(total, 11.0)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Verify error counter aggregation
  let error_data = MetricAggregator::get_metric_data(aggregator, "http_errors_total")
  match error_data {
    Some(data) => {
      match data {
        CounterData(total) => assert_eq(total, 4.0)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 2: Batch Processing with Time Windows
test "batch processing with time windows" {
  let batch_processor = BatchProcessor::new(100, 5000) // batch_size=100, timeout_ms=5000
  
  // Create test telemetry data
  let telemetry_data = [
    TelemetryData::new("trace1", "span1", "metric1", 100.0, 1000),
    TelemetryData::new("trace1", "span2", "metric2", 200.0, 1100),
    TelemetryData::new("trace2", "span1", "metric1", 150.0, 1200),
    TelemetryData::new("trace2", "span2", "metric2", 250.0, 1300),
    TelemetryData::new("trace3", "span1", "metric1", 120.0, 1400)
  ]
  
  // Add data to batch processor
  for data in telemetry_data {
    BatchProcessor::add_data(batch_processor, data)
  }
  
  // Test batch formation
  let batches = BatchProcessor::get_ready_batches(batch_processor)
  assert_eq(batches.length(), 1)
  
  let batch = batches[0]
  assert_eq(batch.data.length(), 5)
  assert_eq(batch.trace_count, 3)
  assert_eq(batch.span_count, 5)
  
  // Test time window grouping
  let time_windowed_batches = BatchProcessor::get_time_windowed_batches(batch_processor, 200)
  assert_eq(time_windowed_batches.length(), 3) // Each span in different window
}

// Test 3: Aggregation with Attribute Filtering
test "aggregation with attribute filtering" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "filtering_test_meter")
  
  let counter = Meter::create_counter(meter, "filtered_requests", Some("Filtered requests"), Some("count"))
  
  // Create attributes for filtering
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "service", StringValue("auth"))
  Attributes::set(attrs1, "method", StringValue("POST"))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "service", StringValue("user"))
  Attributes::set(attrs2, "method", StringValue("GET"))
  
  let attrs3 = Attributes::new()
  Attributes::set(attrs3, "service", StringValue("auth"))
  Attributes::set(attrs3, "method", StringValue("GET"))
  
  // Add metrics with different attributes
  Counter::add(counter, 5.0, Some(attrs1))
  Counter::add(counter, 10.0, Some(attrs2))
  Counter::add(counter, 3.0, Some(attrs3))
  
  // Test aggregation with filtering
  let aggregator = MetricAggregator::new()
  MetricAggregator::add_instrument(aggregator, Counter::as_instrument(counter))
  
  // Filter by service=auth
  let auth_filter = AttributeFilter::new()
  AttributeFilter::add_condition(auth_filter, "service", StringValue("auth"))
  
  let auth_metrics = MetricAggregator::get_filtered_data(aggregator, auth_filter)
  assert_eq(auth_metrics.length(), 1)
  
  match auth_metrics[0] {
    FilteredMetricData(name, value, attrs) => {
      assert_eq(name, "filtered_requests")
      match value {
        CounterData(total) => assert_eq(total, 8.0) // 5 + 3
        _ => assert_true(false)
      }
    }
  }
  
  // Filter by method=GET
  let get_filter = AttributeFilter::new()
  AttributeFilter::add_condition(get_filter, "method", StringValue("GET"))
  
  let get_metrics = MetricAggregator::get_filtered_data(aggregator, get_filter)
  assert_eq(get_metrics.length(), 1)
  
  match get_metrics[0] {
    FilteredMetricData(name, value, attrs) => {
      assert_eq(name, "filtered_requests")
      match value {
        CounterData(total) => assert_eq(total, 13.0) // 10 + 3
        _ => assert_true(false)
      }
    }
  }
}

// Test 4: Percentile and Statistical Aggregation
test "percentile and statistical aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "stats_test_meter")
  
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time"), Some("ms"))
  
  // Record response times with known distribution
  let response_times = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  
  for time in response_times {
    Histogram::record(histogram, time)
  }
  
  // Test statistical aggregation
  let stats_aggregator = StatisticsAggregator::new()
  StatisticsAggregator::add_histogram_data(stats_aggregator, histogram)
  
  let stats = StatisticsAggregator::calculate_statistics(stats_aggregator)
  
  // Verify basic statistics
  assert_eq(stats.count, 10)
  assert_eq(stats.sum, 550.0)
  assert_eq(stats.min, 10.0)
  assert_eq(stats.max, 100.0)
  assert_eq(stats.mean, 55.0)
  
  // Verify percentiles
  let p50 = StatisticsAggregator::percentile(stats_aggregator, 50.0)
  assert_eq(p50, 55.0) // Median
  
  let p90 = StatisticsAggregator::percentile(stats_aggregator, 90.0)
  assert_eq(p90, 90.0)
  
  let p95 = StatisticsAggregator::percentile(stats_aggregator, 95.0)
  assert_eq(p95, 95.0)
  
  let p99 = StatisticsAggregator::percentile(stats_aggregator, 99.0)
  assert_eq(p99, 99.0)
}

// Test 5: Multi-Trace Correlation Aggregation
test "multi-trace correlation aggregation" {
  let correlation_analyzer = TraceCorrelationAnalyzer::new()
  
  // Create correlated trace data
  let trace1_data = [
    SpanData::new("trace1", "span1", "serviceA", 1000, 1100),
    SpanData::new("trace1", "span2", "serviceB", 1100, 1200),
    SpanData::new("trace1", "span3", "serviceC", 1200, 1300)
  ]
  
  let trace2_data = [
    SpanData::new("trace2", "span1", "serviceA", 2000, 2100),
    SpanData::new("trace2", "span2", "serviceB", 2100, 2200)
  ]
  
  let trace3_data = [
    SpanData::new("trace3", "span1", "serviceA", 3000, 3100),
    SpanData::new("trace3", "span2", "serviceD", 3100, 3200),
    SpanData::new("trace3", "span3", "serviceE", 3200, 3300),
    SpanData::new("trace3", "span4", "serviceF", 3300, 3400)
  ]
  
  // Add trace data to analyzer
  for span in trace1_data {
    TraceCorrelationAnalyzer::add_span(correlation_analyzer, span)
  }
  
  for span in trace2_data {
    TraceCorrelationAnalyzer::add_span(correlation_analyzer, span)
  }
  
  for span in trace3_data {
    TraceCorrelationAnalyzer::add_span(correlation_analyzer, span)
  }
  
  // Test service correlation analysis
  let service_correlations = TraceCorrelationAnalyzer::get_service_correlations(correlation_analyzer)
  
  // serviceA appears in all traces
  let service_a_corr = TraceCorrelationAnalyzer::get_service_correlation(correlation_analyzer, "serviceA")
  assert_eq(service_a_corr.trace_count, 3)
  assert_eq(service_a_corr.correlated_services.length(), 3) // serviceB, serviceD, serviceD
  
  // serviceB appears in trace1 and trace2
  let service_b_corr = TraceCorrelationAnalyzer::get_service_correlation(correlation_analyzer, "serviceB")
  assert_eq(service_b_corr.trace_count, 2)
  assert_eq(service_b_corr.correlated_services.length(), 2) // serviceA, serviceC
  
  // Test critical path analysis
  let critical_paths = TraceCorrelationAnalyzer::get_critical_paths(correlation_analyzer)
  assert_eq(critical_paths.length(), 3)
  
  // trace3 has the longest duration
  match critical_paths[0] {
    CriticalPath(trace_id, duration, path) => {
      assert_eq(trace_id, "trace3")
      assert_eq(duration, 400) // 3400 - 3000
      assert_eq(path.length(), 4)
    }
  }
}

// Test 6: Batch Compression and Optimization
test "batch compression and optimization" {
  let batch_optimizer = BatchOptimizer::new()
  
  // Create test telemetry data with redundant information
  let base_trace_id = "base_trace_12345"
  let base_service = "payment_service"
  
  let telemetry_data = []
  for i in 0..=50 {
    let span_data = SpanData::new(
      base_trace_id,
      "span_" + i.to_string(),
      base_service,
      1000 + (i * 100),
      1100 + (i * 100)
    )
    telemetry_data.push(span_data)
  }
  
  // Create batch from telemetry data
  let batch = Batch::from_span_data(telemetry_data)
  
  // Test batch compression
  let compressed_batch = BatchOptimizer::compress(batch_optimizer, batch)
  
  // Verify compression results
  assert_true(compressed_batch.size < batch.size)
  assert_eq(compressed_batch.span_count, batch.span_count)
  assert_eq(compressed_batch.trace_count, batch.trace_count)
  
  // Test batch decompression
  let decompressed_batch = BatchOptimizer::decompress(batch_optimizer, compressed_batch)
  
  // Verify decompression restores original data
  assert_eq(decompressed_batch.span_count, batch.span_count)
  assert_eq(decompressed_batch.trace_count, batch.trace_count)
  assert_eq(decompressed_batch.size, batch.size)
  
  // Test optimization for network transmission
  let network_optimized = BatchOptimizer::optimize_for_network(batch_optimizer, batch)
  assert_true(network_optimized.transmission_size < batch.size)
}

// Test 7: Real-time Aggregation with Sliding Windows
test "real-time aggregation with sliding windows" {
  let realtime_aggregator = RealtimeAggregator::new(10000) // 10 second sliding window
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "realtime_meter")
  
  let counter = Meter::create_counter(meter, "realtime_requests", Some("Realtime requests"), Some("count"))
  
  // Simulate real-time data ingestion
  let current_time = 1609459200000 // 2021-01-01 00:00:00 UTC in milliseconds
  
  for i in 0..=20 {
    let timestamp = current_time + (i * 500) // 500ms intervals
    let attrs = Attributes::new()
    Attributes::set(attrs, "endpoint", StringValue("/api/v" + (i % 4).to_string()))
    
    Counter::add(counter, 1.0, Some(attrs))
    RealtimeAggregator::add_metric_at_time(realtime_aggregator, Counter::as_instrument(counter), timestamp)
  }
  
  // Test sliding window aggregation
  let window_start = current_time + 5000 // 5 seconds in
  let window_end = current_time + 10000 // 10 seconds in
  
  let window_metrics = RealtimeAggregator::get_window_metrics(realtime_aggregator, window_start, window_end)
  assert_eq(window_metrics.length(), 1)
  
  match window_metrics[0] {
    WindowMetric(name, value, time_range) => {
      assert_eq(name, "realtime_requests")
      match value {
        CounterData(total) => assert_eq(total, 10.0) // 10 metrics in the 5-second window
        _ => assert_true(false)
      }
      assert_eq(time_range.start, window_start)
      assert_eq(time_range.end, window_end)
    }
  }
  
  // Test endpoint-specific aggregation in window
  let endpoint_metrics = RealtimeAggregator::get_window_metrics_by_attribute(
    realtime_aggregator, 
    window_start, 
    window_end,
    "endpoint",
    StringValue("/api/v1")
  )
  
  assert_eq(endpoint_metrics.length(), 1)
  match endpoint_metrics[0] {
    WindowMetric(name, value, time_range) => {
      match value {
        CounterData(total) => assert_eq(total, 3.0) // 3 requests to /api/v1 in window
        _ => assert_true(false)
      }
    }
  }
}