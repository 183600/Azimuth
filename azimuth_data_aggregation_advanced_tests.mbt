// Advanced Data Aggregation Tests for Azimuth Telemetry System
// This file contains advanced test cases for data aggregation functionality

// Test 1: Time Window Aggregation
test "time window aggregation functionality" {
  // Define telemetry data points
  type DataPoint = {
    timestamp: Int,
    value: Float,
    metric_name: String
  }
  
  // Create sample data points
  let data_points = [
    { timestamp: 1640995200, value: 10.5, metric_name: "cpu_usage" },
    { timestamp: 1640995260, value: 15.2, metric_name: "cpu_usage" },
    { timestamp: 1640995320, value: 12.8, metric_name: "cpu_usage" },
    { timestamp: 1640995380, value: 18.3, metric_name: "cpu_usage" },
    { timestamp: 1640995440, value: 14.7, metric_name: "cpu_usage" },
    { timestamp: 1640995500, value: 16.9, metric_name: "cpu_usage" }
  ]
  
  // Time window aggregation function
  let aggregate_by_time_window = fn(data: Array[DataPoint], window_size: Int) {
    let mut windows = []
    let mut current_window = []
    let mut window_start = data[0].timestamp
    
    for point in data {
      if point.timestamp < window_start + window_size {
        current_window = current_window.push(point)
      } else {
        windows = windows.push(current_window)
        current_window = [point]
        window_start = point.timestamp
      }
    }
    
    if current_window.length() > 0 {
      windows = windows.push(current_window)
    }
    
    windows
  }
  
  // Aggregate by 5-minute windows (300 seconds)
  let windows = aggregate_by_time_window(data_points, 300)
  assert_eq(windows.length(), 2)
  assert_eq(windows[0].length(), 5)
  assert_eq(windows[1].length(), 1)
  
  // Calculate average for each window
  let calculate_average = fn(window: Array[DataPoint]) {
    let mut sum = 0.0
    for point in window {
      sum = sum + point.value
    }
    sum / window.length().to_float()
  }
  
  let window1_avg = calculate_average(windows[0])
  let window2_avg = calculate_average(windows[1])
  
  assert_true(window1_avg > 10.0 and window1_avg < 20.0)
  assert_eq(window2_avg, 16.9)
}

// Test 2: Metric Aggregation by Dimensions
test "metric aggregation by multiple dimensions" {
  // Define metric data with dimensions
  type MetricData = {
    name: String,
    value: Float,
    dimensions: Array[(String, String)]
  }
  
  // Create sample metrics with different dimensions
  let metrics = [
    { 
      name: "response_time", 
      value: 120.5, 
      dimensions: [("service", "api"), ("region", "us-west"), ("version", "v1.0")]
    },
    { 
      name: "response_time", 
      value: 85.3, 
      dimensions: [("service", "web"), ("region", "us-west"), ("version", "v1.0")]
    },
    { 
      name: "response_time", 
      value: 95.7, 
      dimensions: [("service", "api"), ("region", "us-east"), ("version", "v1.0")]
    },
    { 
      name: "response_time", 
      value: 110.2, 
      dimensions: [("service", "api"), ("region", "us-west"), ("version", "v1.1")]
    },
    { 
      name: "response_time", 
      value: 78.9, 
      dimensions: [("service", "web"), ("region", "us-east"), ("version", "v1.0")]
    }
  ]
  
  // Group metrics by dimensions
  let group_by_dimensions = fn(metrics: Array[MetricData], dimension_keys: Array[String]) {
    let mut groups = []
    
    for metric in metrics {
      // Create dimension key
      let mut dim_values = []
      for key in dimension_keys {
        let mut found = false
        for (k, v) in metric.dimensions {
          if k == key {
            dim_values = dim_values.push(v)
            found = true
          }
        }
        if not(found) {
          dim_values = dim_values.push("")
        }
      }
      
      let group_key = dim_values.join("|")
      
      // Find or create group
      let mut existing_group = None
      let mut group_index = 0
      
      for i in 0..groups.length() {
        if groups[i].key == group_key {
          existing_group = Some(groups[i])
          group_index = i
        }
      }
      
      match existing_group {
        Some(group) => {
          // Update existing group
          let updated_metrics = group.metrics.push(metric)
          let updated_sum = group.sum + metric.value
          let updated_group = { group | metrics: updated_metrics, sum: updated_sum }
          groups[group_index] = updated_group
        }
        None => {
          // Create new group
          groups = groups.push({
            key: group_key,
            metrics: [metric],
            sum: metric.value
          })
        }
      }
    }
    
    groups
  }
  
  // Group by service and region
  let groups = group_by_dimensions(metrics, ["service", "region"])
  assert_eq(groups.length(), 4)  // api+us-west, web+us-west, api+us-east, web+us-east
  
  // Calculate averages for each group
  let calculate_group_averages = fn(groups: Array[{key: String, metrics: Array[MetricData], sum: Float}]) {
    let mut averages = []
    for group in groups {
      let avg = group.sum / group.metrics.length().to_float()
      averages = averages.push((group.key, avg))
    }
    averages
  }
  
  let averages = calculate_group_averages(groups)
  
  // Verify api+us-west average
  let api_us_west_avg = match averages.find(fn(pair) { pair.0 == "api|us-west" }) {
    Some(pair) => pair.1
    None => 0.0
  }
  assert_true(api_us_west_avg > 115.0 and api_us_west_avg < 116.0)  // (120.5 + 110.2) / 2
}

// Test 3: Percentile Calculations
test "percentile calculations for metrics" {
  // Calculate percentiles from a set of values
  let values = [10.2, 15.5, 12.8, 20.1, 18.7, 14.3, 16.9, 11.4, 19.6, 13.2]
  
  // Sort values
  let sorted_values = values.sort(fn(a, b) { a <= b })
  
  // Calculate percentile function
  let calculate_percentile = fn(sorted_vals: Array[Float], percentile: Float) {
    let index = (percentile / 100.0) * (sorted_vals.length() - 1).to_float()
    let lower_index = index.to_int()
    let upper_index = lower_index + 1
    
    if upper_index >= sorted_vals.length() {
      sorted_vals[sorted_vals.length() - 1]
    } else if lower_index == upper_index {
      sorted_vals[lower_index]
    } else {
      let weight = index - lower_index.to_float()
      sorted_vals[lower_index] * (1.0 - weight) + sorted_vals[upper_index] * weight
    }
  }
  
  // Calculate various percentiles
  let p50 = calculate_percentile(sorted_values, 50.0)
  let p90 = calculate_percentile(sorted_values, 90.0)
  let p95 = calculate_percentile(sorted_values, 95.0)
  let p99 = calculate_percentile(sorted_values, 99.0)
  
  // Verify percentile calculations
  assert_true(p50 >= 14.0 and p50 <= 15.0)  // Median
  assert_true(p90 >= 19.0 and p90 <= 20.0)
  assert_true(p95 >= 19.0 and p95 <= 20.0)
  assert_true(p99 >= 19.0 and p99 <= 20.0)
  
  // Test edge cases
  let p0 = calculate_percentile(sorted_values, 0.0)
  let p100 = calculate_percentile(sorted_values, 100.0)
  
  assert_eq(p0, sorted_values[0])
  assert_eq(p100, sorted_values[sorted_values.length() - 1])
}

// Test 4: Histogram Aggregation
test "histogram aggregation and bucketing" {
  // Define histogram bucket
  type HistogramBucket = {
    upper_bound: Float,
    count: Int
  }
  
  // Define histogram
  type Histogram = {
    buckets: Array[HistogramBucket],
    count: Int,
    sum: Float
  }
  
  // Create histogram with predefined buckets
  let create_histogram = fn(upper_bounds: Array[Float]) {
    let buckets = upper_bounds.map(fn(bound) { { upper_bound: bound, count: 0 } })
    { buckets, count: 0, sum: 0.0 }
  }
  
  // Observe a value in the histogram
  let observe = fn(hist: Histogram, value: Float) {
    let mut updated_buckets = hist.buckets
    let mut bucket_index = -1
    
    // Find appropriate bucket
    for i in 0..updated_buckets.length() {
      if value <= updated_buckets[i].upper_bound {
        bucket_index = i
      }
    }
    
    // Update bucket count
    if bucket_index >= 0 {
      let bucket = updated_buckets[bucket_index]
      updated_buckets[bucket_index] = { bucket | count: bucket.count + 1 }
    }
    
    {
      buckets: updated_buckets,
      count: hist.count + 1,
      sum: hist.sum + value
    }
  }
  
  // Create histogram with buckets [5.0, 10.0, 25.0, 50.0, 100.0, +Inf]
  let bounds = [5.0, 10.0, 25.0, 50.0, 100.0, 1000.0]  // Using 1000.0 to represent +Inf
  let mut histogram = create_histogram(bounds)
  
  // Observe values
  let values = [3.2, 7.8, 15.5, 42.1, 87.3, 120.5, 4.5, 22.8, 65.2, 95.7]
  for value in values {
    histogram = observe(histogram, value)
  }
  
  // Verify bucket counts
  assert_eq(histogram.buckets[0].count, 2)  // <= 5.0: 3.2, 4.5
  assert_eq(histogram.buckets[1].count, 2)  // <= 10.0: 7.8, (previous 2)
  assert_eq(histogram.buckets[2].count, 3)  // <= 25.0: 15.5, 22.8, (previous 4)
  assert_eq(histogram.buckets[3].count, 5)  // <= 50.0: 42.1, (previous 5)
  assert_eq(histogram.buckets[4].count, 8)  // <= 100.0: 87.3, 65.2, 95.7, (previous 7)
  assert_eq(histogram.buckets[5].count, 10) // <= 1000.0: 120.5, (previous 9)
  
  // Verify total count and sum
  assert_eq(histogram.count, 10)
  let expected_sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
  assert_eq(histogram.sum, expected_sum)
  
  // Calculate approximate average
  let avg = histogram.sum / histogram.count.to_float()
  assert_true(avg > 40.0 and avg < 60.0)
}

// Test 5: Rate Aggregation
test "rate aggregation and time series calculations" {
  // Define time series point
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float
  }
  
  // Define time series
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint]
  }
  
  // Create sample time series
  let time_series = {
    name: "request_count",
    points: [
      { timestamp: 1640995200, value: 100.0 },
      { timestamp: 1640995260, value: 105.0 },
      { timestamp: 1640995320, value: 110.0 },
      { timestamp: 1640995380, value: 115.0 },
      { timestamp: 1640995440, value: 120.0 },
      { timestamp: 1640995500, value: 125.0 }
    ]
  }
  
  // Calculate rate between consecutive points
  let calculate_rates = fn(ts: TimeSeries) {
    let mut rates = []
    
    for i in 1..ts.points.length() {
      let current = ts.points[i]
      let previous = ts.points[i - 1]
      
      let time_diff = current.timestamp - previous.timestamp
      let value_diff = current.value - previous.value
      
      if time_diff > 0 {
        let rate = value_diff / time_diff.to_float()
        rates = rates.push({
          timestamp: current.timestamp,
          rate: rate
        })
      }
    }
    
    rates
  }
  
  let rates = calculate_rates(time_series)
  assert_eq(rates.length(), 5)
  
  // Verify rate calculations (each should be 5.0 / 60 = 0.08333...)
  let expected_rate = 5.0 / 60.0
  for rate_point in rates {
    assert_true(rate_point.rate > expected_rate - 0.001 and rate_point.rate < expected_rate + 0.001)
  }
  
  // Calculate average rate
  let avg_rate = rates.reduce(fn(acc, rp) { acc + rp.rate }, 0.0) / rates.length().to_float()
  assert_true(avg_rate > expected_rate - 0.001 and avg_rate < expected_rate + 0.001)
  
  // Calculate rate over a window
  let calculate_window_rate = fn(ts: TimeSeries, window_start: Int, window_end: Int) {
    let start_point = ts.points.find(fn(p) { p.timestamp >= window_start })
    let end_point = ts.points.find(fn(p) { p.timestamp >= window_end })
    
    match (start_point, end_point) {
      (Some(sp), Some(ep)) => {
        let time_diff = ep.timestamp - sp.timestamp
        let value_diff = ep.value - sp.value
        
        if time_diff > 0 {
          Some(value_diff / time_diff.to_float())
        } else {
          None
        }
      }
      _ => None
    }
  }
  
  let window_rate = calculate_window_rate(time_series, 1640995200, 1640995500)
  match window_rate {
    Some(rate) => {
      // Over the entire period: (125 - 100) / 300 = 25 / 300 = 0.08333...
      assert_true(rate > 0.083 and rate < 0.084)
    }
    None => assert_true(false)
  }
}

// Test 6: Cardinality Estimation
test "cardinality estimation for high-cardinality data" {
  // HyperLogLog-inspired cardinality estimation (simplified)
  type CardinalityEstimator = {
    registers: Array[Int],
    precision: Int
  }
  
  // Create estimator
  let create_estimator = fn(precision: Int) {
    let register_count = 1 << precision  // 2^precision
    let registers = [0; register_count]
    { registers, precision }
  }
  
  // Hash function (simplified)
  let simple_hash = fn(value: String) {
    let mut hash = 0
    let chars = value.to_char_array()
    
    for i in 0..chars.length() {
      hash = hash + chars[i].to_int() * (i + 1)
    }
    
    hash
  }
  
  // Count leading zeros in binary representation
  let count_leading_zeros = fn(value: Int) {
    let mut count = 0
    let mut mask = 1 << 30  // Start with high bit
    
    while mask > 0 and (value & mask) == 0 {
      count = count + 1
      mask = mask >> 1
    }
    
    count
  }
  
  // Add value to estimator
  let add_value = fn(estimator: CardinalityEstimator, value: String) {
    let hash = simple_hash(value)
    let register_index = hash & ((1 << estimator.precision) - 1)
    let remaining_bits = hash >> estimator.precision
    let leading_zeros = count_leading_zeros(remaining_bits)
    
    let updated_registers = estimator.registers
    let current_max = updated_registers[register_index]
    
    if leading_zeros > current_max {
      updated_registers[register_index] = leading_zeros
    }
    
    { estimator | registers: updated_registers }
  }
  
  // Estimate cardinality
  let estimate_cardinality = fn(estimator: CardinalityEstimator) {
    let register_sum = estimator.registers.reduce(fn(acc, val) { acc + val.to_float() }, 0.0)
    let register_count = estimator.registers.length().to_float()
    let avg = register_sum / register_count
    
    // Simplified cardinality estimation
    let alpha = 0.7213 / (1.0 + 1.079 / register_count)
    (alpha * register_count * (2.0 ^ avg)).to_int()
  }
  
  // Test with sample data
  let mut estimator = create_estimator(4)  // 16 registers
  
  // Add some values
  let values = [
    "user-1", "user-2", "user-3", "user-4", "user-5",
    "user-6", "user-7", "user-8", "user-9", "user-10",
    "user-1", "user-2", "user-3",  // Some duplicates
    "session-1", "session-2", "session-3"
  ]
  
  for value in values {
    estimator = add_value(estimator, value)
  }
  
  // Estimate cardinality
  let estimated = estimate_cardinality(estimator)
  
  // Actual unique values: user-1 through user-10 (10) + session-1 through session-3 (3) = 13
  let actual_unique = 13
  
  // For this simplified estimator, we'll just check that it's in a reasonable range
  assert_true(estimated > 5 and estimated < 25)
  
  // Test with more values
  let more_values = [
    "request-1", "request-2", "request-3", "request-4", "request-5",
    "request-6", "request-7", "request-8", "request-9", "request-10",
    "request-11", "request-12", "request-13", "request-14", "request-15",
    "request-16", "request-17", "request-18", "request-19", "request-20"
  ]
  
  for value in more_values {
    estimator = add_value(estimator, value)
  }
  
  let new_estimated = estimate_cardinality(estimator)
  
  // Should be larger than before
  assert_true(new_estimated > estimated)
  
  // Actual unique values: 13 (previous) + 20 (new) = 33
  // Again, just check that it's in a reasonable range
  assert_true(new_estimated > 15 and new_estimated < 50)
}

// Test 7: Data Downsampling
test "data downsampling for long-term storage" {
  // Define downsampled data point
  type DownsampledPoint = {
    timestamp: Int,
    min: Float,
    max: Float,
    sum: Float,
    count: Int
  }
  
  // Downsample function
  let downsample = fn(data: Array[{timestamp: Int, value: Float}], interval: Int) {
    if data.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut current_interval_start = data[0].timestamp
    let mut current_min = data[0].value
    let mut current_max = data[0].value
    let mut current_sum = data[0].value
    let mut current_count = 1
    
    for i in 1..data.length() {
      let point = data[i]
      
      if point.timestamp < current_interval_start + interval {
        // Same interval
        if point.value < current_min {
          current_min = point.value
        }
        if point.value > current_max {
          current_max = point.value
        }
        current_sum = current_sum + point.value
        current_count = current_count + 1
      } else {
        // New interval, save current and reset
        result = result.push({
          timestamp: current_interval_start,
          min: current_min,
          max: current_max,
          sum: current_sum,
          count: current_count
        })
        
        // Start new interval
        current_interval_start = point.timestamp
        current_min = point.value
        current_max = point.value
        current_sum = point.value
        current_count = 1
      }
    }
    
    // Add last interval
    result = result.push({
      timestamp: current_interval_start,
      min: current_min,
      max: current_max,
      sum: current_sum,
      count: current_count
    })
    
    result
  }
  
  // Create sample data with 1-minute intervals
  let sample_data = [
    { timestamp: 1640995200, value: 10.5 },
    { timestamp: 1640995260, value: 12.3 },
    { timestamp: 1640995320, value: 11.8 },
    { timestamp: 1640995380, value: 14.2 },
    { timestamp: 1640995440, value: 13.7 },
    { timestamp: 1640995500, value: 15.1 },
    { timestamp: 1640995560, value: 14.9 },
    { timestamp: 1640995620, value: 16.3 },
    { timestamp: 1640995680, value: 15.8 },
    { timestamp: 1640995740, value: 17.2 }
  ]
  
  // Downsample to 5-minute intervals (300 seconds)
  let downsampled = downsample(sample_data, 300)
  
  // Should have 2 intervals: 1640995200-1640995499 and 1640995500-1640995799
  assert_eq(downsampled.length(), 2)
  
  // Verify first interval (1640995200)
  assert_eq(downsampled[0].timestamp, 1640995200)
  assert_eq(downsampled[0].min, 10.5)  // Minimum of first 5 points
  assert_eq(downsampled[0].max, 15.1)  // Maximum of first 5 points
  assert_eq(downsampled[0].count, 5)
  
  let first_sum = downsampled[0].sum
  let expected_first_sum = 10.5 + 12.3 + 11.8 + 14.2 + 13.7
  assert_eq(first_sum, expected_first_sum)
  
  // Verify second interval (1640995500)
  assert_eq(downsampled[1].timestamp, 1640995500)
  assert_eq(downsampled[1].min, 14.9)  // Minimum of last 5 points
  assert_eq(downsampled[1].max, 17.2)  // Maximum of last 5 points
  assert_eq(downsampled[1].count, 5)
  
  let second_sum = downsampled[1].sum
  let expected_second_sum = 15.1 + 14.9 + 16.3 + 15.8 + 17.2
  assert_eq(second_sum, expected_second_sum)
  
  // Calculate averages from downsampled data
  let avg1 = downsampled[0].sum / downsampled[0].count.to_float()
  let avg2 = downsampled[1].sum / downsampled[1].count.to_float()
  
  assert_true(avg1 > 12.0 and avg1 < 13.0)
  assert_true(avg2 > 15.0 and avg2 < 17.0)
}

// Test 8: Anomaly Detection in Aggregated Data
test "anomaly detection in aggregated data" {
  // Define aggregated metric
  type AggregatedMetric = {
    name: String,
    timestamp: Int,
    value: Float,
    baseline: Float,
    deviation_threshold: Float
  }
  
  // Detect anomalies
  let detect_anomalies = fn(metrics: Array[AggregatedMetric]) {
    let mut anomalies = []
    
    for metric in metrics {
      let deviation = (metric.value - metric.baseline).abs() / metric.baseline
      
      if deviation > metric.deviation_threshold {
        anomalies = anomalies.push({
          metric: metric,
          deviation: deviation,
          severity: if deviation > metric.deviation_threshold * 2.0 {
            "high"
          } else {
            "medium"
          }
        })
      }
    }
    
    anomalies
  }
  
  // Create sample metrics with baselines
  let metrics = [
    { 
      name: "cpu_usage", 
      timestamp: 1640995200, 
      value: 15.5, 
      baseline: 20.0, 
      deviation_threshold: 0.25 
    },
    { 
      name: "memory_usage", 
      timestamp: 1640995200, 
      value: 75.2, 
      baseline: 70.0, 
      deviation_threshold: 0.15 
    },
    { 
      name: "response_time", 
      timestamp: 1640995200, 
      value: 120.5, 
      baseline: 100.0, 
      deviation_threshold: 0.30 
    },
    { 
      name: "error_rate", 
      timestamp: 1640995200, 
      value: 8.5, 
      baseline: 2.0, 
      deviation_threshold: 0.50 
    },
    { 
      name: "throughput", 
      timestamp: 1640995200, 
      value: 850.0, 
      baseline: 1000.0, 
      deviation_threshold: 0.20 
    }
  ]
  
  // Detect anomalies
  let anomalies = detect_anomalies(metrics)
  
  // Should detect anomalies in metrics that exceed their thresholds
  assert_true(anomalies.length() >= 2)
  
  // Check for specific anomalies
  let cpu_anomaly = anomalies.find(fn(a) { a.metric.name == "cpu_usage" })
  let memory_anomaly = anomalies.find(fn(a) { a.metric.name == "memory_usage" })
  let error_anomaly = anomalies.find(fn(a) { a.metric.name == "error_rate" })
  let throughput_anomaly = anomalies.find(fn(a) { a.metric.name == "throughput" })
  
  // CPU usage: (15.5 - 20.0) / 20.0 = 22.5% < 25% threshold, no anomaly
  assert_eq(cpu_anomaly, None)
  
  // Memory usage: (75.2 - 70.0) / 70.0 = 7.4% < 15% threshold, no anomaly
  assert_eq(memory_anomaly, None)
  
  // Error rate: (8.5 - 2.0) / 2.0 = 325% > 50% threshold, anomaly
  assert_true(error_anomaly.is_some())
  match error_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.metric.name, "error_rate")
      assert_true(anomaly.deviation > 3.0)
      assert_eq(anomaly.severity, "high")  # > 100% deviation (2x threshold)
    }
    None => assert_true(false)
  }
  
  // Throughput: (850.0 - 1000.0) / 1000.0 = 15% < 20% threshold, no anomaly
  assert_eq(throughput_anomaly, None)
  
  // Test anomaly severity classification
  let high_severity_anomalies = anomalies.filter(fn(a) { a.severity == "high" })
  let medium_severity_anomalies = anomalies.filter(fn(a) { a.severity == "medium" })
  
  // Error rate should be high severity
  assert_true(high_severity_anomalies.length() >= 1)
  assert_eq(high_severity_anomalies[0].metric.name, "error_rate")
}