// Azimuth Telemetry System - Advanced Data Aggregation Tests
// This file contains comprehensive test cases for telemetry data aggregation functionality

// Test 1: Basic Metrics Aggregation
test "basic metrics aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_test")
  
  // Create counter for aggregation
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  // Add values to counter
  Counter::add(counter, 10.0)
  Counter::add(counter, 20.0)
  Counter::add(counter, 30.0)
  
  // Test aggregation functions
  let aggregator = MetricAggregator::new()
  let aggregated = MetricAggregator::aggregate_counter(aggregator, counter)
  
  match aggregated {
    CounterAggregation(sum, count) => {
      assert_eq(sum, 60.0)
      assert_eq(count, 3)
    }
    _ => assert_true(false)
  }
  
  // Create histogram for aggregation
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  // Record values to histogram
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  Histogram::record(histogram, 300.0)
  
  let hist_aggregated = MetricAggregator::aggregate_histogram(aggregator, histogram)
  
  match hist_aggregated {
    HistogramAggregation(count, sum, min, max, buckets) => {
      assert_eq(count, 4)
      assert_eq(sum, 750.0)
      assert_eq(min, 100.0)
      assert_eq(max, 300.0)
      assert_true(buckets.length() > 0)
    }
    _ => assert_true(false)
  }
}

// Test 2: Time Series Data Aggregation
test "time series data aggregation" {
  let time_series = TimeSeries::new("test_metric")
  
  // Add data points with timestamps
  let timestamp1 = 1609459200L // 2021-01-01 00:00:00
  let timestamp2 = 1609459260L // 2021-01-01 00:01:00
  let timestamp3 = 1609459320L // 2021-01-01 00:02:00
  
  TimeSeries::add_point(time_series, DataPoint::new(timestamp1, 10.0))
  TimeSeries::add_point(time_series, DataPoint::new(timestamp2, 20.0))
  TimeSeries::add_point(time_series, DataPoint::new(timestamp3, 15.0))
  
  // Test time window aggregation
  let window_start = 1609459200L
  let window_end = 1609459380L // 3 minutes window
  
  let aggregated = TimeSeries::aggregate_window(time_series, window_start, window_end, Average)
  
  match aggregated {
    WindowedAggregation(start, end, value, count) => {
      assert_eq(start, window_start)
      assert_eq(end, window_end)
      assert_eq(value, 15.0) // (10 + 20 + 15) / 3
      assert_eq(count, 3)
    }
    _ => assert_true(false)
  }
  
  // Test different aggregation types
  let sum_aggregated = TimeSeries::aggregate_window(time_series, window_start, window_end, Sum)
  match sum_aggregated {
    WindowedAggregation(_, _, value, _) => assert_eq(value, 45.0)
    _ => assert_true(false)
  }
  
  let max_aggregated = TimeSeries::aggregate_window(time_series, window_start, window_end, Max)
  match max_aggregated {
    WindowedAggregation(_, _, value, _) => assert_eq(value, 20.0)
    _ => assert_true(false)
  }
  
  let min_aggregated = TimeSeries::aggregate_window(time_series, window_start, window_end, Min)
  match min_aggregated {
    WindowedAggregation(_, _, value, _) => assert_eq(value, 10.0)
    _ => assert_true(false)
  }
}

// Test 3: Multi-dimensional Data Aggregation
test "multi-dimensional data aggregation" {
  let aggregator = MultiDimensionalAggregator::new()
  
  // Create data points with multiple dimensions
  let data_point1 = MultiDimensionalDataPoint::new(100.0, [
    ("region", StringValue("us-east")),
    ("service", StringValue("auth")),
    ("environment", StringValue("production"))
  ])
  
  let data_point2 = MultiDimensionalDataPoint::new(150.0, [
    ("region", StringValue("us-east")),
    ("service", StringValue("auth")),
    ("environment", StringValue("production"))
  ])
  
  let data_point3 = MultiDimensionalDataPoint::new(200.0, [
    ("region", StringValue("us-west")),
    ("service", StringValue("auth")),
    ("environment", StringValue("production"))
  ])
  
  let data_point4 = MultiDimensionalDataPoint::new(120.0, [
    ("region", StringValue("us-east")),
    ("service", StringValue("payment")),
    ("environment", StringValue("production"))
  ])
  
  // Add data points to aggregator
  MultiDimensionalAggregator::add_point(aggregator, data_point1)
  MultiDimensionalAggregator::add_point(aggregator, data_point2)
  MultiDimensionalAggregator::add_point(aggregator, data_point3)
  MultiDimensionalAggregator::add_point(aggregator, data_point4)
  
  // Test aggregation by region
  let region_filter = AttributeFilter::new([("environment", StringValue("production"))])
  let region_groups = MultiDimensionalAggregator::group_by(aggregator, "region", region_filter)
  
  assert_eq(region_groups.length(), 2)
  
  // Check us-east group
  let us_east_group = region_groups.find(|g| g.dimension_value == "us-east")
  match us_east_group {
    Some(group) => {
      assert_eq(group.count, 3)
      assert_eq(group.sum, 370.0) // 100 + 150 + 120
      assert_eq(group.average, 123.33) // 370 / 3
    }
    None => assert_true(false)
  }
  
  // Check us-west group
  let us_west_group = region_groups.find(|g| g.dimension_value == "us-west")
  match us_west_group {
    Some(group) => {
      assert_eq(group.count, 1)
      assert_eq(group.sum, 200.0)
      assert_eq(group.average, 200.0)
    }
    None => assert_true(false)
  }
}

// Test 4: Real-time Stream Aggregation
test "real-time stream aggregation" {
  let stream_aggregator = RealTimeStreamAggregator::new(1000) // 1 second windows
  
  // Simulate real-time data stream
  let stream_data = [
    StreamDataPoint::new(1609459200L, 10.0),
    StreamDataPoint::new(1609459200L + 100L, 15.0),
    StreamDataPoint::new(1609459200L + 200L, 20.0),
    StreamDataPoint::new(1609459200L + 1100L, 25.0), // Next window
    StreamDataPoint::new(1609459200L + 1200L, 30.0),
    StreamDataPoint::new(1609459200L + 1300L, 35.0)
  ]
  
  // Process stream data
  for data_point in stream_data {
    RealTimeStreamAggregator::process_point(stream_aggregator, data_point)
  }
  
  // Get aggregated windows
  let windows = RealTimeStreamAggregator::get_completed_windows(stream_aggregator)
  
  assert_eq(windows.length(), 2)
  
  // Check first window
  let first_window = windows[0]
  assert_eq(first_window.start_time, 1609459200L)
  assert_eq(first_window.end_time, 1609459200L + 1000L)
  assert_eq(first_window.count, 3)
  assert_eq(first_window.sum, 45.0)
  assert_eq(first_window.average, 15.0)
  
  // Check second window
  let second_window = windows[1]
  assert_eq(second_window.start_time, 1609459200L + 1000L)
  assert_eq(second_window.end_time, 1609459200L + 2000L)
  assert_eq(second_window.count, 3)
  assert_eq(second_window.sum, 90.0)
  assert_eq(second_window.average, 30.0)
}

// Test 5: Statistical Aggregation Functions
test "statistical aggregation functions" {
  let data_points = [10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0]
  
  // Test mean calculation
  let mean = StatisticalFunctions::mean(data_points)
  assert_eq(mean, 25.0) // (10 + 15 + 20 + 25 + 30 + 35 + 40) / 7
  
  // Test median calculation
  let median = StatisticalFunctions::median(data_points)
  assert_eq(median, 25.0) // Middle value in sorted array
  
  // Test standard deviation
  let std_dev = StatisticalFunctions::standard_deviation(data_points)
  assert_true(std_dev > 10.0 && std_dev < 11.0) // Approximate value
  
  // Test percentiles
  let p50 = StatisticalFunctions::percentile(data_points, 50.0)
  assert_eq(p50, 25.0) // 50th percentile is median
  
  let p90 = StatisticalFunctions::percentile(data_points, 90.0)
  assert_eq(p90, 38.0) // 90th percentile
  
  // Test variance
  let variance = StatisticalFunctions::variance(data_points)
  assert_true(variance > 100.0 && variance < 125.0) // Approximate value
}

// Test 6: Histogram-based Aggregation
test "histogram-based aggregation" {
  let histogram_aggregator = HistogramAggregator::new([0.0, 10.0, 20.0, 30.0, 40.0, 50.0])
  
  // Add values to histogram
  let values = [5.0, 15.0, 25.0, 35.0, 45.0, 8.0, 18.0, 28.0, 38.0, 48.0]
  
  for value in values {
    HistogramAggregator::add_value(histogram_aggregator, value)
  }
  
  // Get histogram buckets
  let buckets = HistogramAggregator::get_buckets(histogram_aggregator)
  
  assert_eq(buckets.length(), 6) // 5 boundaries + 1 overflow bucket
  
  // Check bucket counts
  assert_eq(buckets[0].count, 2) // 0-10: 5.0, 8.0
  assert_eq(buckets[1].count, 2) // 10-20: 15.0, 18.0
  assert_eq(buckets[2].count, 2) // 20-30: 25.0, 28.0
  assert_eq(buckets[3].count, 2) // 30-40: 35.0, 38.0
  assert_eq(buckets[4].count, 2) // 40-50: 45.0, 48.0
  assert_eq(buckets[5].count, 0) // >50: none
  
  // Test percentile approximation from histogram
  let p50_approx = HistogramAggregator::percentile_approximate(histogram_aggregator, 50.0)
  assert_true(p50_approx >= 20.0 && p50_approx <= 30.0)
  
  let p95_approx = HistogramAggregator::percentile_approximate(histogram_aggregator, 95.0)
  assert_true(p95_approx >= 40.0 && p95_approx <= 50.0)
}

// Test 7: Aggregation with Sampling
test "aggregation with sampling" {
  let sampling_aggregator = SamplingAggregator::new(0.5) // 50% sampling rate
  
  // Add data points with timestamps
  let base_timestamp = 1609459200L
  for i in 0..=100 {
    let timestamp = base_timestamp + (i * 10L)
    let value = (i * 2) as Float
    
    let data_point = DataPoint::new(timestamp, value)
    SamplingAggregator::add_point(sampling_aggregator, data_point)
  }
  
  // Get aggregated results
  let aggregated = SamplingAggregator::get_aggregated(sampling_aggregator)
  
  match aggregated {
    SampledAggregation(original_count, sampled_count, sum, average) => {
      assert_eq(original_count, 101)
      assert_true(sampled_count > 40 && sampled_count < 60) // Approximately 50% of original
      assert_true(sum > 0.0)
      assert_true(average > 0.0)
    }
    _ => assert_true(false)
  }
  
  // Test different sampling rates
  let high_rate_aggregator = SamplingAggregator::new(0.9) // 90% sampling rate
  for i in 0..=100 {
    let timestamp = base_timestamp + (i * 10L)
    let value = (i * 2) as Float
    
    let data_point = DataPoint::new(timestamp, value)
    SamplingAggregator::add_point(high_rate_aggregator, data_point)
  }
  
  let high_rate_aggregated = SamplingAggregator::get_aggregated(high_rate_aggregator)
  match high_rate_aggregated {
    SampledAggregation(original_count, sampled_count, _, _) => {
      assert_eq(original_count, 101)
      assert_true(sampled_count > 80) // More than 80% should be sampled
    }
    _ => assert_true(false)
  }
}

// Test 8: Distributed Aggregation
test "distributed aggregation" {
  let distributed_aggregator = DistributedAggregator::new()
  
  // Simulate data from multiple nodes
  let node1_data = [
    NodeDataPoint::new("node1", 1609459200L, 10.0),
    NodeDataPoint::new("node1", 1609459210L, 15.0),
    NodeDataPoint::new("node1", 1609459220L, 20.0)
  ]
  
  let node2_data = [
    NodeDataPoint::new("node2", 1609459200L, 12.0),
    NodeDataPoint::new("node2", 1609459210L, 18.0),
    NodeDataPoint::new("node2", 1609459220L, 22.0)
  ]
  
  let node3_data = [
    NodeDataPoint::new("node3", 1609459200L, 8.0),
    NodeDataPoint::new("node3", 1609459210L, 13.0),
    NodeDataPoint::new("node3", 1609459220L, 17.0)
  ]
  
  // Add data from each node
  for data_point in node1_data {
    DistributedAggregator::add_point(distributed_aggregator, data_point)
  }
  
  for data_point in node2_data {
    DistributedAggregator::add_point(distributed_aggregator, data_point)
  }
  
  for data_point in node3_data {
    DistributedAggregator::add_point(distributed_aggregator, data_point)
  }
  
  // Get distributed aggregation results
  let results = DistributedAggregator::get_aggregated(distributed_aggregator)
  
  // Check per-node results
  assert_eq(results.length(), 3)
  
  let node1_result = results.find(|r| r.node_id == "node1")
  match node1_result {
    Some(result) => {
      assert_eq(result.count, 3)
      assert_eq(result.sum, 45.0)
      assert_eq(result.average, 15.0)
    }
    None => assert_true(false)
  }
  
  let node2_result = results.find(|r| r.node_id == "node2")
  match node2_result {
    Some(result) => {
      assert_eq(result.count, 3)
      assert_eq(result.sum, 52.0)
      assert_eq(result.average, 17.33)
    }
    None => assert_true(false)
  }
  
  let node3_result = results.find(|r| r.node_id == "node3")
  match node3_result {
    Some(result) => {
      assert_eq(result.count, 3)
      assert_eq(result.sum, 38.0)
      assert_eq(result.average, 12.67)
    }
    None => assert_true(false)
  }
  
  // Check global aggregation
  let global_result = DistributedAggregator::get_global_aggregated(distributed_aggregator)
  match global_result {
    GlobalAggregation(total_count, total_sum, global_average, node_count) => {
      assert_eq(total_count, 9)
      assert_eq(total_sum, 135.0)
      assert_eq(global_average, 15.0)
      assert_eq(node_count, 3)
    }
    _ => assert_true(false)
  }
}

// Test 9: Adaptive Aggregation
test "adaptive aggregation" {
  let adaptive_aggregator = AdaptiveAggregator::new()
  
  // Simulate varying data rates
  let base_timestamp = 1609459200L
  
  // Low rate period (1 point per second)
  for i in 0..=10 {
    let timestamp = base_timestamp + (i * 1000L)
    let value = (i * 2) as Float
    let data_point = DataPoint::new(timestamp, value)
    AdaptiveAggregator::add_point(adaptive_aggregator, data_point)
  }
  
  // Check aggregation strategy for low rate
  let low_rate_strategy = AdaptiveAggregator::get_current_strategy(adaptive_aggregator)
  match low_rate_strategy {
    LowRateStrategy(window_size) => assert_eq(window_size, 60000L) // 1 minute
    _ => assert_true(false)
  }
  
  // High rate period (10 points per second)
  for i in 0..=100 {
    let timestamp = base_timestamp + 11000L + (i * 100L)
    let value = (i * 2) as Float
    let data_point = DataPoint::new(timestamp, value)
    AdaptiveAggregator::add_point(adaptive_aggregator, data_point)
  }
  
  // Check aggregation strategy for high rate
  let high_rate_strategy = AdaptiveAggregator::get_current_strategy(adaptive_aggregator)
  match high_rate_strategy {
    HighRateStrategy(window_size, sampling_rate) => {
      assert_eq(window_size, 10000L) // 10 seconds
      assert_eq(sampling_rate, 0.1) // 10% sampling
    }
    _ => assert_true(false)
  }
  
  // Get adaptive aggregation results
  let results = AdaptiveAggregator::get_aggregated(adaptive_aggregator)
  assert_true(results.length() > 0)
}

// Test 10: Aggregation Performance Optimization
test "aggregation performance optimization" {
  let performance_aggregator = PerformanceOptimizedAggregator::new()
  
  // Add large number of data points
  let base_timestamp = 1609459200L
  let start_time = Time::now()
  
  for i in 0..=10000 {
    let timestamp = base_timestamp + (i as Long)
    let value = (i % 100) as Float
    let data_point = DataPoint::new(timestamp, value)
    PerformanceOptimizedAggregator::add_point(performance_aggregator, data_point)
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Performance should be reasonable for 10,000 points
  assert_true(duration < 5000L) // Less than 5 seconds
  
  // Get optimized aggregation results
  let results = PerformanceOptimizedAggregator::get_aggregated(performance_aggregator)
  
  match results {
    OptimizedAggregation(count, sum, average, min, max, buckets) => {
      assert_eq(count, 10001)
      assert_eq(sum, 495000.0) // Sum of 0-99 repeated 101 times
      assert_eq(average, 49.5)
      assert_eq(min, 0.0)
      assert_eq(max, 99.0)
      assert_true(buckets.length() > 0)
    }
    _ => assert_true(false)
  }
  
  // Test memory usage optimization
  let memory_usage = PerformanceOptimizedAggregator::get_memory_usage(performance_aggregator)
  assert_true(memory_usage < 1000000) // Less than 1MB
  
  // Test batch processing optimization
  let batch_aggregator = PerformanceOptimizedAggregator::new()
  let batch_size = 1000
  let batch_data = []
  
  for i in 0..=10000 {
    let timestamp = base_timestamp + (i as Long)
    let value = (i % 100) as Float
    let data_point = DataPoint::new(timestamp, value)
    batch_data.push(data_point)
    
    if batch_data.length() >= batch_size {
      PerformanceOptimizedAggregator::add_batch(batch_aggregator, batch_data)
      batch_data = []
    }
  }
  
  // Process remaining data
  if batch_data.length() > 0 {
    PerformanceOptimizedAggregator::add_batch(batch_aggregator, batch_data)
  }
  
  let batch_results = PerformanceOptimizedAggregator::get_aggregated(batch_aggregator)
  match batch_results {
    OptimizedAggregation(count, _, _, _, _, _) => assert_eq(count, 10001)
    _ => assert_true(false)
  }
}