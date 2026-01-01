// Azimuth Aggregated Metrics Operations Tests
// This file contains tests for aggregated metrics operations in the Azimuth telemetry system

// Test 1: Counter aggregation operations
pub test "aggregated counter operations" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "aggregated-test-meter")
  
  // Create multiple counters for aggregation
  let counter1 = azimuth::Meter::create_counter(meter, "request.count", Some("Total requests"), Some("count"))
  let counter2 = azimuth::Meter::create_counter(meter, "error.count", Some("Total errors"), Some("count"))
  let counter3 = azimuth::Meter::create_counter(meter, "success.count", Some("Total successes"), Some("count"))
  
  // Simulate metric recordings
  for i = 0; i < 100; i = i + 1 {
    azimuth::Counter::add(counter1, 1, [("endpoint", "api/v1/users"), ("method", "GET")])
    if i % 10 == 0 {
      azimuth::Counter::add(counter2, 1, [("endpoint", "api/v1/users"), ("method", "GET"), ("error.type", "timeout")])
    } else {
      azimuth::Counter::add(counter3, 1, [("endpoint", "api/v1/users"), ("method", "GET")])
    }
  }
  
  // Test aggregation by time window
  let time_window = 60000000000L  // 60 seconds in nanoseconds
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let end_time = start_time + time_window
  
  // Aggregate counters within time window
  let aggregated_counters = azimuth::Aggregator::aggregate_counters_by_time([counter1, counter2, counter3], start_time, end_time)
  
  // Verify aggregation results
  assert_true(aggregated_counters.length() >= 3)
  
  // Test aggregation by attributes
  let attribute_filter = [("endpoint", "api/v1/users")]
  let filtered_counters = azimuth::Aggregator::filter_by_attributes([counter1, counter2, counter3], attribute_filter)
  
  assert_true(filtered_counters.length() >= 3)
  
  // Test rate calculation
  let rate1 = azimuth::Aggregator::calculate_rate(counter1, start_time, end_time)
  let rate2 = azimuth::Aggregator::calculate_rate(counter2, start_time, end_time)
  let rate3 = azimuth::Aggregator::calculate_rate(counter3, start_time, end_time)
  
  assert_true(rate1 > 0.0)
  assert_true(rate2 >= 0.0)
  assert_true(rate3 > 0.0)
  
  // Test percentage calculation
  let error_rate = azimuth::Aggregator::calculate_percentage(counter2, counter1)
  let success_rate = azimuth::Aggregator::calculate_percentage(counter3, counter1)
  
  assert_true(error_rate >= 0.0 && error_rate <= 100.0)
  assert_true(success_rate >= 0.0 && success_rate <= 100.0)
  assert_eq(error_rate + success_rate, 100.0)
}

// Test 2: Histogram aggregation operations
pub test "aggregated histogram operations" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "histogram-test-meter")
  
  // Create histograms for response times
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time distribution"), Some("ms"))
  let cpu_usage_histogram = azimuth::Meter::create_histogram(meter, "cpu.usage", Some("CPU usage distribution"), Some("percent"))
  
  // Simulate response time recordings
  let response_times = [10, 25, 50, 75, 100, 150, 200, 300, 500, 1000]
  for i = 0; i < 1000; i = i + 1 {
    let response_time = response_times[i % response_times.length()]
    azimuth::Histogram::record(response_time_histogram, response_time.to_double(), [("endpoint", "api/v1/data")])
  }
  
  // Simulate CPU usage recordings
  for i = 0; i < 500; i = i + 1 {
    let cpu_usage = (i % 100).to_double()
    azimuth::Histogram::record(cpu_usage_histogram, cpu_usage, [("instance", "server-1")])
  }
  
  // Test percentile calculations
  let p50 = azimuth::Aggregator::calculate_percentile(response_time_histogram, 50.0)
  let p95 = azimuth::Aggregator::calculate_percentile(response_time_histogram, 95.0)
  let p99 = azimuth::Aggregator::calculate_percentile(response_time_histogram, 99.0)
  
  assert_true(p50 > 0.0)
  assert_true(p95 >= p50)
  assert_true(p99 >= p95)
  
  // Test histogram statistics
  let stats = azimuth::Aggregator::calculate_histogram_stats(response_time_histogram)
  assert_true(stats.count > 0)
  assert_true(stats.sum > 0.0)
  assert_true(stats.min >= 0.0)
  assert_true(stats.max >= stats.min)
  assert_true(stats.mean >= stats.min && stats.mean <= stats.max)
  
  // Test histogram bucket aggregation
  let buckets = azimuth::Aggregator::get_histogram_buckets(response_time_histogram)
  assert_true(buckets.length() > 0)
  
  // Verify bucket boundaries and counts
  for bucket in buckets {
    assert_true(bucket.upper_bound >= 0.0)
    assert_true(bucket.count >= 0)
  }
  
  // Test multi-histogram aggregation
  let aggregated_histograms = azimuth::Aggregator::aggregate_histograms([response_time_histogram, cpu_usage_histogram])
  assert_true(aggregated_histograms.length() >= 2)
}

// Test 3: Gauge aggregation operations
pub test "aggregated gauge operations" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "gauge-test-meter")
  
  // Create gauges for system metrics
  let memory_gauge = azimuth::Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  let disk_gauge = azimuth::Meter::create_gauge(meter, "disk.usage", Some("Disk usage"), Some("bytes"))
  let network_gauge = azimuth::Meter::create_gauge(meter, "network.throughput", Some("Network throughput"), Some("bps"))
  
  // Simulate gauge recordings over time
  let time_series = []
  for i = 0; i < 60; i = i + 1 {  // 60 data points
    let timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // Simulate memory usage (gradual increase with occasional drops)
    let memory_usage = (1024 * 1024 * 1024).to_long() + (i * 10 * 1024 * 1024).to_long()  // Base 1GB + incremental
    if i % 10 == 0 { memory_usage = memory_usage - (100 * 1024 * 1024).to_long() }  // Occasional drop
    
    // Simulate disk usage (steady increase)
    let disk_usage = (500 * 1024 * 1024 * 1024).to_long() + (i * 1024 * 1024).to_long()  // Base 500GB + incremental
    
    // Simulate network throughput (variable)
    let network_throughput = (1000000 + (i % 100) * 10000).to_long()  // Variable between 1Mbps and 2Mbps
    
    azimuth::Gauge::set(memory_gauge, memory_usage, [("instance", "server-1")])
    azimuth::Gauge::set(disk_gauge, disk_usage, [("instance", "server-1")])
    azimuth::Gauge::set(network_gauge, network_throughput, [("instance", "server-1")])
    
    time_series.push((timestamp, memory_usage, disk_usage, network_throughput))
  }
  
  // Test time series aggregation
  let memory_time_series = azimuth::Aggregator::extract_time_series(memory_gauge, "instance", "server-1")
  let disk_time_series = azimuth::Aggregator::extract_time_series(disk_gauge, "instance", "server-1")
  let network_time_series = azimuth::Aggregator::extract_time_series(network_gauge, "instance", "server-1")
  
  assert_true(memory_time_series.length() >= 60)
  assert_true(disk_time_series.length() >= 60)
  assert_true(network_time_series.length() >= 60)
  
  // Test trend analysis
  let memory_trend = azimuth::Aggregator::calculate_trend(memory_time_series)
  let disk_trend = azimuth::Aggregator::calculate_trend(disk_time_series)
  let network_trend = azimuth::Aggregator::calculate_trend(network_time_series)
  
  // Memory should generally increase (positive trend)
  assert_true(memory_trend >= 0.0)
  // Disk should steadily increase (positive trend)
  assert_true(disk_trend > 0.0)
  // Network throughput should be variable (could be positive or negative)
  assert_true(network_trend >= -100.0 && network_trend <= 100.0)
  
  // Test anomaly detection
  let memory_anomalies = azimuth::Aggregator::detect_anomalies(memory_time_series, 2.0)  // 2 standard deviations
  let disk_anomalies = azimuth::Aggregator::detect_anomalies(disk_time_series, 2.0)
  let network_anomalies = azimuth::Aggregator::detect_anomalies(network_time_series, 2.0)
  
  // Memory should have some anomalies due to occasional drops
  assert_true(memory_anomalies.length() >= 5)
  // Disk should have fewer anomalies (steady increase)
  assert_true(disk_anomalies.length() <= 2)
  // Network should have some anomalies due to variability
  assert_true(network_anomalies.length() >= 3)
}

// Test 4: Cross-metric aggregation and correlation
pub test "cross-metric aggregation and correlation" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "correlation-test-meter")
  
  // Create related metrics
  let request_counter = azimuth::Meter::create_counter(meter, "request.count", Some("Request count"), Some("count"))
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  let error_counter = azimuth::Meter::create_counter(meter, "error.count", Some("Error count"), Some("count"))
  let cpu_gauge = azimuth::Meter::create_gauge(meter, "cpu.usage", Some("CPU usage"), Some("percent"))
  
  // Simulate correlated metrics
  for i = 0; i < 100; i = i + 1 {
    let endpoint = if i % 3 == 0 { "api/v1/heavy" } else { "api/v1/light" }
    let cpu_usage = if i % 3 == 0 { 80.0 + (i % 20).to_double() } else { 20.0 + (i % 10).to_double() }
    let response_time = if i % 3 == 0 { 500.0 + (i % 200).to_double() } else { 50.0 + (i % 50).to_double() }
    
    // Record metrics
    azimuth::Counter::add(request_counter, 1, [("endpoint", endpoint)])
    azimuth::Histogram::record(response_time_histogram, response_time, [("endpoint", endpoint)])
    azimuth::Gauge::set(cpu_gauge, cpu_usage.to_long(), [("endpoint", endpoint)])
    
    // Simulate errors under high load
    if i % 3 == 0 && i % 10 == 0 {
      azimuth::Counter::add(error_counter, 1, [("endpoint", endpoint), ("error.type", "timeout")])
    }
  }
  
  // Test metric correlation
  let correlations = azimuth::Aggregator::calculate_correlations([
    ("request.count", request_counter),
    ("response.time", response_time_histogram),
    ("error.count", error_counter),
    ("cpu.usage", cpu_gauge)
  ])
  
  assert_true(correlations.length() >= 6)  // At least 6 correlation pairs
  
  // Find and verify specific correlations
  let request_response_corr = azimuth::Aggregator::find_correlation(correlations, "request.count", "response.time")
  let cpu_error_corr = azimuth::Aggregator::find_correlation(correlations, "cpu.usage", "error.count")
  
  assert_true(request_response_corr is Some)
  assert_true(cpu_error_corr is Some)
  
  // High response times should correlate with high CPU usage
  match request_response_corr {
    Some(corr) => assert_true(corr.coefficient >= 0.0)  // Positive correlation expected
    None => assert_true(false)
  }
  
  // High CPU usage should correlate with more errors
  match cpu_error_corr {
    Some(corr) => assert_true(corr.coefficient >= 0.0)  // Positive correlation expected
    None => assert_true(false)
  }
  
  // Test metric grouping and aggregation
  let grouped_metrics = azimuth::Aggregator::group_by_attributes([
    ("request.count", request_counter),
    ("response.time", response_time_histogram),
    ("error.count", error_counter),
    ("cpu.usage", cpu_gauge)
  ], ["endpoint"])
  
  assert_true(grouped_metrics.length() >= 2)  // At least 2 endpoints
  
  // Verify group statistics
  for group in grouped_metrics {
    let group_key = group.attributes["endpoint"]
    assert_true(group_key == "api/v1/heavy" || group_key == "api/v1/light")
    
    // Heavy endpoint should have higher metrics
    if group_key == "api/v1/heavy" {
      assert_true(group.metrics["response.time"].mean > 500.0)
      assert_true(group.metrics["cpu.usage"].mean > 80.0)
    } else {
      assert_true(group.metrics["response.time"].mean < 100.0)
      assert_true(group.metrics["cpu.usage"].mean < 30.0)
    }
  }
}