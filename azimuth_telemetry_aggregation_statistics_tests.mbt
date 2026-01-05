// Azimuth Telemetry Aggregation and Statistics Test Suite
// This file contains tests for telemetry data aggregation and statistical analysis

// Test 1: Basic Aggregation Functions
test "basic telemetry aggregation functions" {
  // Metric data structure
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Aggregation functions
  let sum = fn(metrics: Array[Metric]) {
    let mut total = 0.0
    for metric in metrics {
      total = total + metric.value
    }
    total
  }
  
  let average = fn(metrics: Array[Metric]) {
    if metrics.length() == 0 {
      0.0
    } else {
      sum(metrics) / metrics.length().to_float()
    }
  }
  
  let min = fn(metrics: Array[Metric]) {
    if metrics.length() == 0 {
      Float::infinity()
    } else {
      let mut minimum = metrics[0].value
      for metric in metrics {
        if metric.value < minimum {
          minimum = metric.value
        }
      }
      minimum
    }
  }
  
  let max = fn(metrics: Array[Metric]) {
    if metrics.length() == 0 {
      0.0
    } else {
      let mut maximum = metrics[0].value
      for metric in metrics {
        if metric.value > maximum {
          maximum = metric.value
        }
      }
      maximum
    }
  }
  
  let count = fn(metrics: Array[Metric]) {
    metrics.length()
  }
  
  // Create test metrics
  let metrics = [
    { name: "response_time", value: 120.5, timestamp: 1640995200, tags: [("service", "api")] },
    { name: "response_time", value: 85.3, timestamp: 1640995260, tags: [("service", "api")] },
    { name: "response_time", value: 200.1, timestamp: 1640995320, tags: [("service", "api")] },
    { name: "response_time", value: 95.7, timestamp: 1640995380, tags: [("service", "api")] },
    { name: "response_time", value: 150.2, timestamp: 1640995440, tags: [("service", "api")] }
  ]
  
  // Test aggregation functions
  assert_eq(sum(metrics), 651.8)
  assert_eq(average(metrics), 130.36)
  assert_eq(min(metrics), 85.3)
  assert_eq(max(metrics), 200.1)
  assert_eq(count(metrics), 5)
  
  // Test with empty array
  let empty_metrics = []
  assert_eq(sum(empty_metrics), 0.0)
  assert_eq(average(empty_metrics), 0.0)
  assert_eq(min(empty_metrics), Float::infinity())
  assert_eq(max(empty_metrics), 0.0)
  assert_eq(count(empty_metrics), 0)
  
  // Test with single metric
  let single_metric = [{ name: "cpu_usage", value: 75.5, timestamp: 1640995500, tags: [] }]
  assert_eq(sum(single_metric), 75.5)
  assert_eq(average(single_metric), 75.5)
  assert_eq(min(single_metric), 75.5)
  assert_eq(max(single_metric), 75.5)
  assert_eq(count(single_metric), 1)
}

// Test 2: Time-based Aggregation
test "time-based telemetry aggregation" {
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Time window aggregation
  let aggregate_by_time_window = fn(metrics: Array[Metric], window_size_seconds: Int) {
    if metrics.length() == 0 or window_size_seconds <= 0 {
      return []
    }
    
    // Sort metrics by timestamp
    let sorted_metrics = metrics.sort(fn(a, b) { a.timestamp - b.timestamp })
    
    let mut windows = []
    let mut current_window = []
    let mut window_start = sorted_metrics[0].timestamp
    let mut window_end = window_start + window_size_seconds
    
    for metric in sorted_metrics {
      if metric.timestamp >= window_start and metric.timestamp < window_end {
        current_window = current_window.push(metric)
      } else {
        if current_window.length() > 0 {
          windows = windows.push(current_window)
        }
        
        // Start new window
        window_start = metric.timestamp
        window_end = window_start + window_size_seconds
        current_window = [metric]
      }
    }
    
    // Add the last window
    if current_window.length() > 0 {
      windows = windows.push(current_window)
    }
    
    windows
  }
  
  let calculate_window_stats = fn(windows: Array[Array[Metric]]) {
    let mut stats = []
    
    for window in windows {
      let sum = window.reduce(fn(acc, m) { acc + m.value }, 0.0)
      let count = window.length().to_float()
      let avg = if count > 0.0 { sum / count } else { 0.0 }
      let min_val = window.reduce(fn(acc, m) { if m.value < acc { m.value } else { acc }, Float::infinity())
      let max_val = window.reduce(fn(acc, m) { if m.value > acc { m.value } else { acc }, 0.0)
      
      let window_start = if window.length() > 0 { window[0].timestamp } else { 0 }
      let window_end = if window.length() > 0 { window[window.length() - 1].timestamp } else { 0 }
      
      stats = stats.push({
        window_start,
        window_end,
        count,
        sum,
        avg,
        min_val,
        max_val
      })
    }
    
    stats
  }
  
  // Create test metrics with timestamps
  let base_timestamp = 1640995200  // Base timestamp
  let metrics = [
    { name: "response_time", value: 100.0, timestamp: base_timestamp, tags: [] },
    { name: "response_time", value: 120.0, timestamp: base_timestamp + 30, tags: [] },
    { name: "response_time", value: 80.0, timestamp: base_timestamp + 60, tags: [] },
    { name: "response_time", value: 150.0, timestamp: base_timestamp + 300, tags: [] },  // New window
    { name: "response_time", value: 90.0, timestamp: base_timestamp + 330, tags: [] },
    { name: "response_time", value: 110.0, timestamp: base_timestamp + 360, tags: [] },
    { name: "response_time", value: 130.0, timestamp: base_timestamp + 700, tags: [] },  // Another new window
    { name: "response_time", value: 70.0, timestamp: base_timestamp + 730, tags: [] }
  ]
  
  // Test time window aggregation (2-minute windows)
  let windows = aggregate_by_time_window(metrics, 120)
  assert_eq(windows.length(), 4)
  
  // First window: 3 metrics (0s, 30s, 60s)
  assert_eq(windows[0].length(), 3)
  
  // Second window: 3 metrics (300s, 330s, 360s)
  assert_eq(windows[1].length(), 3)
  
  // Third window: 2 metrics (700s, 730s)
  assert_eq(windows[2].length(), 2)
  
  // Calculate stats for each window
  let stats = calculate_window_stats(windows)
  assert_eq(stats.length(), 4)
  
  // First window stats
  assert_eq(stats[0].window_start, base_timestamp)
  assert_eq(stats[0].count, 3.0)
  assert_eq(stats[0].sum, 300.0)
  assert_eq(stats[0].avg, 100.0)
  assert_eq(stats[0].min_val, 80.0)
  assert_eq(stats[0].max_val, 120.0)
  
  // Second window stats
  assert_eq(stats[1].window_start, base_timestamp + 300)
  assert_eq(stats[1].count, 3.0)
  assert_eq(stats[1].sum, 350.0)
  assert_eq(stats[1].avg, 350.0 / 3.0)
  assert_eq(stats[1].min_val, 90.0)
  assert_eq(stats[1].max_val, 150.0)
  
  // Test with empty metrics
  let empty_windows = aggregate_by_time_window([], 60)
  assert_eq(empty_windows.length(), 0)
  
  let empty_stats = calculate_window_stats(empty_windows)
  assert_eq(empty_stats.length(), 0)
}

// Test 3: Percentile and Distribution Analysis
test "percentile and distribution analysis" {
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Percentile calculation
  let calculate_percentile = fn(sorted_values: Array[Float], percentile: Float) {
    if sorted_values.length() == 0 {
      return 0.0
    }
    
    if percentile <= 0.0 {
      return sorted_values[0]
    }
    
    if percentile >= 100.0 {
      return sorted_values[sorted_values.length() - 1]
    }
    
    let index = (percentile / 100.0 * (sorted_values.length() - 1).to_float()).to_int()
    sorted_values[index]
  }
  
  // Distribution analysis
  let analyze_distribution = fn(metrics: Array[Metric]) {
    if metrics.length() == 0 {
      return {
        count: 0,
        mean: 0.0,
        median: 0.0,
        p50: 0.0,
        p90: 0.0,
        p95: 0.0,
        p99: 0.0,
        min: 0.0,
        max: 0.0,
        std_dev: 0.0
      }
    }
    
    // Extract values and sort them
    let values = metrics.map(fn(m) { m.value }).sort(fn(a, b) { a - b })
    
    // Calculate mean
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let mean = sum / values.length().to_float()
    
    // Calculate median
    let median = if values.length() % 2 == 0 {
      let mid = values.length() / 2
      (values[mid - 1] + values[mid]) / 2.0
    } else {
      values[values.length() / 2]
    }
    
    // Calculate percentiles
    let p50 = calculate_percentile(values, 50.0)
    let p90 = calculate_percentile(values, 90.0)
    let p95 = calculate_percentile(values, 95.0)
    let p99 = calculate_percentile(values, 99.0)
    
    // Calculate standard deviation
    let variance = values.reduce(fn(acc, v) { acc + (v - mean) * (v - mean) }, 0.0) / values.length().to_float()
    let std_dev = if variance >= 0.0 { variance.sqrt() } else { 0.0 }
    
    {
      count: values.length(),
      mean,
      median,
      p50,
      p90,
      p95,
      p99,
      min: values[0],
      max: values[values.length() - 1],
      std_dev
    }
  }
  
  // Create test metrics with varied values
  let metrics = [
    { name: "response_time", value: 10.0, timestamp: 1, tags: [] },
    { name: "response_time", value: 20.0, timestamp: 2, tags: [] },
    { name: "response_time", value: 30.0, timestamp: 3, tags: [] },
    { name: "response_time", value: 40.0, timestamp: 4, tags: [] },
    { name: "response_time", value: 50.0, timestamp: 5, tags: [] },
    { name: "response_time", value: 60.0, timestamp: 6, tags: [] },
    { name: "response_time", value: 70.0, timestamp: 7, tags: [] },
    { name: "response_time", value: 80.0, timestamp: 8, tags: [] },
    { name: "response_time", value: 90.0, timestamp: 9, tags: [] },
    { name: "response_time", value: 100.0, timestamp: 10, tags: [] }
  ]
  
  // Test distribution analysis
  let distribution = analyze_distribution(metrics)
  
  assert_eq(distribution.count, 10)
  assert_eq(distribution.mean, 55.0)
  assert_eq(distribution.median, 55.0)
  assert_eq(distribution.p50, 50.0)
  assert_eq(distribution.p90, 90.0)
  assert_eq(distribution.p95, 95.0)
  assert_eq(distribution.p99, 100.0)
  assert_eq(distribution.min, 10.0)
  assert_eq(distribution.max, 100.0)
  
  // Test with odd number of values
  let odd_metrics = [
    { name: "response_time", value: 10.0, timestamp: 1, tags: [] },
    { name: "response_time", value: 20.0, timestamp: 2, tags: [] },
    { name: "response_time", value: 30.0, timestamp: 3, tags: [] },
    { name: "response_time", value: 40.0, timestamp: 4, tags: [] },
    { name: "response_time", value: 50.0, timestamp: 5, tags: [] }
  ]
  
  let odd_distribution = analyze_distribution(odd_metrics)
  assert_eq(odd_distribution.count, 5)
  assert_eq(odd_distribution.mean, 30.0)
  assert_eq(odd_distribution.median, 30.0)
  assert_eq(odd_distribution.p50, 30.0)
  assert_eq(odd_distribution.p90, 50.0)
  assert_eq(odd_distribution.p95, 50.0)
  assert_eq(odd_distribution.p99, 50.0)
  assert_eq(odd_distribution.min, 10.0)
  assert_eq(odd_distribution.max, 50.0)
  
  // Test with empty metrics
  let empty_distribution = analyze_distribution([])
  assert_eq(empty_distribution.count, 0)
  assert_eq(empty_distribution.mean, 0.0)
  assert_eq(empty_distribution.median, 0.0)
}

// Test 4: Tag-based Aggregation
test "tag-based telemetry aggregation" {
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Group metrics by tag
  let group_by_tag = fn(metrics: Array[Metric], tag_key: String) {
    let mut groups = {}
    
    for metric in metrics {
      // Find the value for the specified tag key
      let mut tag_value = "unknown"
      let mut tag_found = false
      
      for (key, value) in metric.tags {
        if key == tag_key {
          tag_value = value
          tag_found = true
          break
        }
      }
      
      // Add metric to the appropriate group
      let group = match groups.get(tag_value) {
        Some(existing_group) => existing_group.push(metric),
        None => [metric]
      }
      
      groups = groups.insert(tag_value, group)
    }
    
    groups
  }
  
  // Calculate stats for each group
  let calculate_group_stats = fn(groups) {
    let mut stats = []
    
    for (tag_value, group_metrics) in groups {
      let sum = group_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
      let count = group_metrics.length().to_float()
      let avg = if count > 0.0 { sum / count } else { 0.0 }
      let min_val = group_metrics.reduce(fn(acc, m) { if m.value < acc { m.value } else { acc }, Float::infinity())
      let max_val = group_metrics.reduce(fn(acc, m) { if m.value > acc { m.value } else { acc }, 0.0)
      
      stats = stats.push({
        tag_value,
        count,
        sum,
        avg,
        min_val,
        max_val
      })
    }
    
    stats
  }
  
  // Create test metrics with different tags
  let metrics = [
    { name: "response_time", value: 100.0, timestamp: 1, tags: [("service", "api"), ("env", "prod")] },
    { name: "response_time", value: 120.0, timestamp: 2, tags: [("service", "api"), ("env", "prod")] },
    { name: "response_time", value: 80.0, timestamp: 3, tags: [("service", "web"), ("env", "prod")] },
    { name: "response_time", value: 90.0, timestamp: 4, tags: [("service", "web"), ("env", "staging")] },
    { name: "response_time", value: 110.0, timestamp: 5, tags: [("service", "api"), ("env", "staging")] },
    { name: "response_time", value: 70.0, timestamp: 6, tags: [("service", "db"), ("env", "prod")] },
    { name: "response_time", value: 85.0, timestamp: 7, tags: [("service", "db"), ("env", "staging")] },
    { name: "response_time", value: 95.0, timestamp: 8, tags: [("service", "web"), ("env", "prod")] }
  ]
  
  // Test grouping by "service" tag
  let service_groups = group_by_tag(metrics, "service")
  let service_stats = calculate_group_stats(service_groups)
  
  // Should have 3 groups: api, web, db
  assert_eq(service_stats.length(), 3)
  
  // Find stats for each service
  let find_stat = fn(stats, tag_value) {
    let mut result = None
    for stat in stats {
      if stat.tag_value == tag_value {
        result = Some(stat)
        break
      }
    }
    result
  }
  
  let api_stat = find_stat(service_stats, "api")
  assert_true(api_stat.is_some())
  assert_eq(api_stat.unwrap().count, 3.0)
  assert_eq(api_stat.unwrap().sum, 330.0)
  assert_eq(api_stat.unwrap().avg, 110.0)
  assert_eq(api_stat.unwrap().min_val, 100.0)
  assert_eq(api_stat.unwrap().max_val, 120.0)
  
  let web_stat = find_stat(service_stats, "web")
  assert_true(web_stat.is_some())
  assert_eq(web_stat.unwrap().count, 3.0)
  assert_eq(web_stat.unwrap().sum, 265.0)
  assert_eq(web_stat.unwrap().avg, 265.0 / 3.0)
  assert_eq(web_stat.unwrap().min_val, 80.0)
  assert_eq(web_stat.unwrap().max_val, 95.0)
  
  let db_stat = find_stat(service_stats, "db")
  assert_true(db_stat.is_some())
  assert_eq(db_stat.unwrap().count, 2.0)
  assert_eq(db_stat.unwrap().sum, 155.0)
  assert_eq(db_stat.unwrap().avg, 77.5)
  assert_eq(db_stat.unwrap().min_val, 70.0)
  assert_eq(db_stat.unwrap().max_val, 85.0)
  
  // Test grouping by "env" tag
  let env_groups = group_by_tag(metrics, "env")
  let env_stats = calculate_group_stats(env_groups)
  
  // Should have 2 groups: prod, staging
  assert_eq(env_stats.length(), 2)
  
  let prod_stat = find_stat(env_stats, "prod")
  assert_true(prod_stat.is_some())
  assert_eq(prod_stat.unwrap().count, 5.0)
  assert_eq(prod_stat.unwrap().sum, 445.0)
  assert_eq(prod_stat.unwrap().avg, 89.0)
  
  let staging_stat = find_stat(env_stats, "staging")
  assert_true(staging_stat.is_some())
  assert_eq(staging_stat.unwrap().count, 3.0)
  assert_eq(staging_stat.unwrap().sum, 285.0)
  assert_eq(staging_stat.unwrap().avg, 95.0)
  
  // Test with non-existent tag key
  let unknown_groups = group_by_tag(metrics, "unknown")
  let unknown_stats = calculate_group_stats(unknown_groups)
  
  // Should have 1 group with tag_value "unknown"
  assert_eq(unknown_stats.length(), 1)
  assert_eq(unknown_stats[0].tag_value, "unknown")
  assert_eq(unknown_stats[0].count, 8.0)
}

// Test 5: Rate and Trend Analysis
test "rate and trend analysis" {
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Calculate rate of change
  let calculate_rate = fn(metrics: Array[Metric], time_window: Int) {
    if metrics.length() < 2 or time_window <= 0 {
      return []
    }
    
    // Sort metrics by timestamp
    let sorted_metrics = metrics.sort(fn(a, b) { a.timestamp - b.timestamp })
    
    let mut rates = []
    
    for i in 1..sorted_metrics.length() {
      let current = sorted_metrics[i]
      let previous = sorted_metrics[i - 1]
      
      let time_diff = (current.timestamp - previous.timestamp).to_float()
      
      if time_diff > 0.0 {
        let value_diff = current.value - previous.value
        let rate = value_diff / time_diff * time_window.to_float()
        
        rates = rates.push({
          timestamp: current.timestamp,
          rate
        })
      }
    }
    
    rates
  }
  
  // Calculate moving average
  let moving_average = fn(values: Array[Float], window_size: Int) {
    if values.length() == 0 or window_size <= 0 {
      return []
    }
    
    let mut averages = []
    
    for i in 0..values.length() {
      let start = if i - window_size + 1 >= 0 { i - window_size + 1 } else { 0 }
      let end = i
      
      let mut sum = 0.0
      let mut count = 0
      
      for j in start..=end {
        sum = sum + values[j]
        count = count + 1
      }
      
      averages = averages.push(sum / count.to_float())
    }
    
    averages
  }
  
  // Detect trends
  let detect_trend = fn(values: Array[Float]) {
    if values.length() < 3 {
      return "insufficient_data"
    }
    
    let mut increasing_count = 0
    let mut decreasing_count = 0
    
    for i in 1..values.length() {
      if values[i] > values[i - 1] {
        increasing_count = increasing_count + 1
      } else if values[i] < values[i - 1] {
        decreasing_count = decreasing_count + 1
      }
    }
    
    let total_changes = increasing_count + decreasing_count
    
    if total_changes == 0 {
      "stable"
    } else if increasing_count > decreasing_count * 2 {
      "strongly_increasing"
    } else if increasing_count > decreasing_count {
      "increasing"
    } else if decreasing_count > increasing_count * 2 {
      "strongly_decreasing"
    } else if decreasing_count > increasing_count {
      "decreasing"
    } else {
      "oscillating"
    }
  }
  
  // Create test metrics with time series data
  let base_timestamp = 1640995200
  let metrics = [
    { name: "cpu_usage", value: 50.0, timestamp: base_timestamp, tags: [] },
    { name: "cpu_usage", value: 55.0, timestamp: base_timestamp + 60, tags: [] },
    { name: "cpu_usage", value: 60.0, timestamp: base_timestamp + 120, tags: [] },
    { name: "cpu_usage", value: 58.0, timestamp: base_timestamp + 180, tags: [] },
    { name: "cpu_usage", value: 65.0, timestamp: base_timestamp + 240, tags: [] },
    { name: "cpu_usage", value: 70.0, timestamp: base_timestamp + 300, tags: [] },
    { name: "cpu_usage", value: 68.0, timestamp: base_timestamp + 360, tags: [] },
    { name: "cpu_usage", value: 75.0, timestamp: base_timestamp + 420, tags: [] }
  ]
  
  // Test rate calculation
  let rates = calculate_rate(metrics, 60)  // Rate per minute
  assert_eq(rates.length(), 7)
  
  // First rate: (55 - 50) / 60 * 60 = 5
  assert_eq(rates[0].rate, 5.0)
  
  // Second rate: (60 - 55) / 60 * 60 = 5
  assert_eq(rates[1].rate, 5.0)
  
  // Third rate: (58 - 60) / 60 * 60 = -2
  assert_eq(rates[2].rate, -2.0)
  
  // Test moving average
  let values = metrics.map(fn(m) { m.value })
  let ma3 = moving_average(values, 3)
  let ma5 = moving_average(values, 5)
  
  assert_eq(ma3.length(), 8)
  assert_eq(ma5.length(), 8)
  
  // First 3-period moving average: (50 + 55 + 60) / 3 = 55
  assert_eq(ma3[2], 55.0)
  
  // First 5-period moving average: (50 + 55 + 60 + 58 + 65) / 5 = 57.6
  assert_eq(ma5[4], 57.6)
  
  // Test trend detection
  let increasing_values = [10.0, 20.0, 30.0, 40.0, 50.0]
  assert_eq(detect_trend(increasing_values), "strongly_increasing")
  
  let decreasing_values = [50.0, 40.0, 30.0, 20.0, 10.0]
  assert_eq(detect_trend(decreasing_values), "strongly_decreasing")
  
  let stable_values = [30.0, 30.0, 30.0, 30.0, 30.0]
  assert_eq(detect_trend(stable_values), "stable")
  
  let oscillating_values = [10.0, 50.0, 20.0, 40.0, 30.0]
  assert_eq(detect_trend(oscillating_values), "oscillating")
  
  let cpu_trend = detect_trend(values)
  assert_eq(cpu_trend, "increasing")
  
  // Test with empty metrics
  let empty_rates = calculate_rate([], 60)
  assert_eq(empty_rates.length(), 0)
  
  let empty_ma = moving_average([], 3)
  assert_eq(empty_ma.length(), 0)
  
  let empty_trend = detect_trend([])
  assert_eq(empty_trend, "insufficient_data")
}