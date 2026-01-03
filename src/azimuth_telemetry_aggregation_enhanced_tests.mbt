// Azimuth Telemetry System - Enhanced Data Aggregation Tests
// This file contains comprehensive test cases for telemetry data aggregation functionality

// Test 1: Metrics Aggregation with Different Time Windows
test "metrics aggregation with time windows" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_test_meter")
  
  // Create counter for time-based aggregation
  let counter = Meter::create_counter(meter, "request_count", Some("Total requests"), Some("count"))
  
  // Simulate metrics over time windows
  for i in 0..=10 {
    Counter::add(counter, 1.0)
  }
  
  // Create histogram for latency metrics
  let latency_histogram = Meter::create_histogram(meter, "request_latency", Some("Request latency"), Some("ms"))
  
  // Record latency values
  let latency_values = [10.0, 25.0, 50.0, 75.0, 100.0, 150.0, 200.0, 300.0]
  for latency in latency_values {
    Histogram::record(latency_histogram, latency)
  }
  
  // Test aggregation results
  let aggregation = MetricsAggregator::aggregate_metrics([counter, latency_histogram])
  
  // Verify counter aggregation
  match MetricsAggregator::get_counter_sum(aggregation, "request_count") {
    Some(sum) => assert_eq(sum, 11.0)
    None => assert_true(false)
  }
  
  // Verify histogram statistics
  match MetricsAggregator::get_histogram_stats(aggregation, "request_latency") {
    Some(stats) => {
      assert_true(stats.min >= 10.0)
      assert_true(stats.max <= 300.0)
      assert_true(stats.mean > 0.0)
      assert_true(stats.count == 8)
    }
    None => assert_true(false)
  }
}

// Test 2: Multi-dimensional Attribute Aggregation
test "multi-dimensional attribute aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi_dim_test_meter")
  
  // Create counter with attributes
  let counter = Meter::create_counter(meter, "api_requests", Some("API requests"), Some("count"))
  
  // Define attribute sets for different dimensions
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "endpoint", StringValue("/api/users"))
  Attributes::set(attrs1, "method", StringValue("GET"))
  Attributes::set(attrs1, "status", StringValue("200"))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "endpoint", StringValue("/api/users"))
  Attributes::set(attrs2, "method", StringValue("POST"))
  Attributes::set(attrs2, "status", StringValue("201"))
  
  let attrs3 = Attributes::new()
  Attributes::set(attrs3, "endpoint", StringValue("/api/orders"))
  Attributes::set(attrs3, "method", StringValue("GET"))
  Attributes::set(attrs3, "status", StringValue("200"))
  
  // Record metrics with different attribute combinations
  for i in 0..=5 {
    Counter::add(counter, 1.0, Some(attrs1))
  }
  
  for i in 0..=3 {
    Counter::add(counter, 1.0, Some(attrs2))
  }
  
  for i in 0..=7 {
    Counter::add(counter, 1.0, Some(attrs3))
  }
  
  // Test multi-dimensional aggregation
  let aggregation = MetricsAggregator::aggregate_by_dimensions(counter, ["endpoint", "method"])
  
  // Verify aggregation results
  let endpoint_counts = MetricsAggregator::get_dimension_counts(aggregation, "endpoint")
  match endpoint_counts {
    Some(counts) => {
      assert_eq(counts.get("/api/users"), 9) // 5 + 3 + 1
      assert_eq(counts.get("/api/orders"), 8) // 7 + 1
    }
    None => assert_true(false)
  }
  
  let method_counts = MetricsAggregator::get_dimension_counts(aggregation, "method")
  match method_counts {
    Some(counts) => {
      assert_eq(counts.get("GET"), 13) // 5 + 7 + 1
      assert_eq(counts.get("POST"), 4) // 3 + 1
    }
    None => assert_true(false)
  }
}

// Test 3: Percentile and Quantile Calculations
test "percentile and quantile calculations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "percentile_test_meter")
  
  // Create histogram for percentile calculations
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time"), Some("ms"))
  
  // Generate response time data with known distribution
  let response_times = [
    10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0,
    60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0, 100.0, 150.0,
    200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0
  ]
  
  for time in response_times {
    Histogram::record(histogram, time)
  }
  
  // Calculate percentiles
  let p50 = MetricsAggregator::calculate_percentile(histogram, 0.5)
  let p90 = MetricsAggregator::calculate_percentile(histogram, 0.9)
  let p95 = MetricsAggregator::calculate_percentile(histogram, 0.95)
  let p99 = MetricsAggregator::calculate_percentile(histogram, 0.99)
  
  // Verify percentile calculations
  match p50 {
    Some(value) => assert_true(value >= 50.0 && value <= 60.0)
    None => assert_true(false)
  }
  
  match p90 {
    Some(value) => assert_true(value >= 400.0 && value <= 450.0)
    None => assert_true(false)
  }
  
  match p95 {
    Some(value) => assert_true(value >= 450.0 && value <= 500.0)
    None => assert_true(false)
  }
  
  match p99 {
    Some(value) => assert_true(value >= 490.0 && value <= 500.0)
    None => assert_true(false)
  }
}

// Test 4: Time Series Data Aggregation
test "time series data aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries_test_meter")
  
  // Create gauge for time series data
  let gauge = Meter::create_gauge(meter, "cpu_usage", Some("CPU usage"), Some("percent"))
  
  // Simulate time series data points
  let time_points = [
    (1000L, 25.5),   // timestamp, value
    (2000L, 30.2),
    (3000L, 28.7),
    (4000L, 35.1),
    (5000L, 40.3),
    (6000L, 38.9),
    (7000L, 42.1),
    (8000L, 45.6),
    (9000L, 43.2),
    (10000L, 48.7)
  ]
  
  // Record time series data
  for (timestamp, value) in time_points {
    Gauge::set(gauge, value, Some(timestamp))
  }
  
  // Test time series aggregation
  let timeseries = TimeSeriesAggregator::create_from_gauge(gauge)
  
  // Test average calculation over time window
  let avg_5_to_8_seconds = TimeSeriesAggregator::average_in_range(timeseries, 5000L, 8000L)
  match avg_5_to_8_seconds {
    Some(avg) => assert_true(avg >= 40.0 && avg <= 45.0)
    None => assert_true(false)
  }
  
  // Test trend calculation
  let trend = TimeSeriesAggregator::calculate_trend(timeseries, 0L, 10000L)
  match trend {
    Some(trend_value) => assert_true(trend_value > 0.0) // Should be increasing
    None => assert_true(false)
  }
  
  // Test peak detection
  let peaks = TimeSeriesAggregator::find_peaks(timeseries, 2) // minimum peak height
  assert_true(peaks.length() >= 2)
}

// Test 5: Cross-Metric Correlation Analysis
test "cross-metric correlation analysis" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "correlation_test_meter")
  
  // Create related metrics
  let request_counter = Meter::create_counter(meter, "requests", Some("Request count"), Some("count"))
  let error_counter = Meter::create_counter(meter, "errors", Some("Error count"), Some("count"))
  let latency_histogram = Meter::create_histogram(meter, "latency", Some("Request latency"), Some("ms"))
  
  // Simulate correlated metrics
  let scenarios = [
    (100, 5, 50.0),   // requests, errors, avg latency
    (150, 8, 75.0),
    (200, 12, 100.0),
    (180, 15, 120.0),
    (120, 7, 65.0),
    (90, 3, 45.0)
  ]
  
  for (requests, errors, latency) in scenarios {
    // Record requests
    for i in 0..=requests {
      Counter::add(request_counter, 1.0)
    }
    
    // Record errors
    for i in 0..=errors {
      Counter::add(error_counter, 1.0)
    }
    
    // Record latency values around the average
    for i in 0..=10 {
      let latency_variation = latency + (Int::to_float(i - 5) * 2.0)
      Histogram::record(latency_histogram, latency_variation)
    }
  }
  
  // Test correlation analysis
  let correlation_analyzer = CorrelationAnalyzer::new()
  
  // Add metrics to analyzer
  CorrelationAnalyzer::add_metric(correlation_analyzer, request_counter)
  CorrelationAnalyzer::add_metric(correlation_analyzer, error_counter)
  CorrelationAnalyzer::add_metric(correlation_analyzer, latency_histogram)
  
  // Calculate correlations
  let request_error_corr = CorrelationAnalyzer::calculate_correlation(
    correlation_analyzer, 
    "requests", 
    "errors"
  )
  
  let request_latency_corr = CorrelationAnalyzer::calculate_correlation(
    correlation_analyzer, 
    "requests", 
    "latency"
  )
  
  // Verify positive correlations (as requests increase, errors and latency tend to increase)
  match request_error_corr {
    Some(corr) => assert_true(corr > 0.5)
    None => assert_true(false)
  }
  
  match request_latency_corr {
    Some(corr) => assert_true(corr > 0.3)
    None => assert_true(false)
  }
}