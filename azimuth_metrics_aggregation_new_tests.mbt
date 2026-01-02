// Azimuth Metrics Aggregation Tests
// This file contains test cases for metrics aggregation and calculation

// Test 1: Counter Metric Aggregation
test "counter metric aggregation operations" {
  let counter_samples = [
    ("requests_total", 100),
    ("requests_total", 150),
    ("requests_total", 200),
    ("requests_total", 250),
    ("errors_total", 5),
    ("errors_total", 8),
    ("errors_total", 12)
  ]
  
  // Aggregate by metric name
  let mut request_counts = []
  let mut error_counts = []
  
  for sample in counter_samples {
    match sample.0 {
      "requests_total" => request_counts = request_counts.push(sample.1)
      "errors_total" => error_counts = error_counts.push(sample.1)
      _ => ()
    }
  }
  
  // Calculate counter increments
  let request_total = if request_counts.length() > 0 {
    request_counts[request_counts.length() - 1] - request_counts[0]
  } else {
    0
  }
  
  let error_total = if error_counts.length() > 0 {
    error_counts[error_counts.length() - 1] - error_counts[0]
  } else {
    0
  }
  
  assert_eq(request_total, 150) // 250 - 100
  assert_eq(error_total, 7) // 12 - 5
  
  // Calculate error rate
  let error_rate = if request_total > 0 {
    Int::to_float(error_total) / Int::to_float(request_total) * 100.0
  } else {
    0.0
  }
  
  assert_eq(error_rate, 4.666666666666667)
}

// Test 2: Gauge Metric Aggregation
test "gauge metric aggregation operations" {
  let gauge_samples = [
    ("cpu_usage", 25.5),
    ("cpu_usage", 30.2),
    ("cpu_usage", 45.8),
    ("cpu_usage", 35.1),
    ("memory_usage", 60.5),
    ("memory_usage", 65.3),
    ("memory_usage", 70.1)
  ]
  
  // Separate metrics by name
  let cpu_values = gauge_samples.filter(fn(sample) { sample.0 == "cpu_usage" }).map(fn(sample) { sample.1 })
  let memory_values = gauge_samples.filter(fn(sample) { sample.0 == "memory_usage" }).map(fn(sample) { sample.1 })
  
  // Calculate statistics for CPU usage
  let cpu_avg = cpu_values.reduce(fn(acc, val) { acc + val }, 0.0) / Int::to_float(cpu_values.length())
  let cpu_max = cpu_values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, cpu_values[0])
  let cpu_min = cpu_values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, cpu_values[0])
  
  // Calculate statistics for memory usage
  let memory_avg = memory_values.reduce(fn(acc, val) { acc + val }, 0.0) / Int::to_float(memory_values.length())
  let memory_max = memory_values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, memory_values[0])
  let memory_min = memory_values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, memory_values[0])
  
  assert_eq(cpu_avg, 34.15)
  assert_eq(cpu_max, 45.8)
  assert_eq(cpu_min, 25.5)
  
  assert_eq(memory_avg, 65.3)
  assert_eq(memory_max, 70.1)
  assert_eq(memory_min, 60.5)
}

// Test 3: Histogram Metric Aggregation
test "histogram metric aggregation operations" {
  let histogram_buckets = [
    (0.0, 10.0, 5),    // 0-10ms: 5 requests
    (10.0, 50.0, 15),  // 10-50ms: 15 requests
    (50.0, 100.0, 25), // 50-100ms: 25 requests
    (100.0, 500.0, 10), // 100-500ms: 10 requests
    (500.0, 1000.0, 3)  // 500-1000ms: 3 requests
  ]
  
  // Calculate total count
  let total_count = histogram_buckets.reduce(fn(acc, bucket) { acc + bucket.2 }, 0)
  assert_eq(total_count, 58)
  
  // Calculate percentiles (approximation)
  let mut cumulative_count = 0
  let p50_threshold = Int::to_float(total_count) * 0.5
  let p95_threshold = Int::to_float(total_count) * 0.95
  let mut p50_value = 0.0
  let mut p95_value = 0.0
  
  for bucket in histogram_buckets {
    cumulative_count = cumulative_count + bucket.2
    
    if p50_value == 0.0 && Int::to_float(cumulative_count) >= p50_threshold {
      p50_value = bucket.1 // Upper bound of bucket containing 50th percentile
    }
    
    if p95_value == 0.0 && Int::to_float(cumulative_count) >= p95_threshold {
      p95_value = bucket.1 // Upper bound of bucket containing 95th percentile
    }
  }
  
  assert_eq(p50_value, 100.0)
  assert_eq(p95_value, 500.0)
}

// Test 4: Summary Metric Aggregation
test "summary metric aggregation operations" {
  let summary_samples = [
    (100.0, 10), // 100ms, 10 observations
    (150.0, 8),  // 150ms, 8 observations
    (200.0, 5),  // 200ms, 5 observations
    (50.0, 12),  // 50ms, 12 observations
    (75.0, 15)   // 75ms, 15 observations
  ]
  
  // Calculate weighted average
  let total_observations = summary_samples.reduce(fn(acc, sample) { acc + sample.1 }, 0)
  let weighted_sum = summary_samples.reduce(fn(acc, sample) { 
    acc + (sample.0 * Int::to_float(sample.1)) 
  }, 0.0)
  let weighted_avg = weighted_sum / Int::to_float(total_observations)
  
  assert_eq(total_observations, 50)
  assert_eq(weighted_avg, 98.0)
  
  // Find min and max values
  let min_value = summary_samples.reduce(fn(acc, sample) { 
    if sample.0 < acc.0 { sample } else { acc } 
  }, summary_samples[0]).0
  
  let max_value = summary_samples.reduce(fn(acc, sample) { 
    if sample.0 > acc.0 { sample } else { acc } 
  }, summary_samples[0]).0
  
  assert_eq(min_value, 50.0)
  assert_eq(max_value, 200.0)
}

// Test 5: Multi-Dimensional Metric Aggregation
test "multi-dimensional metric aggregation" {
  let labeled_metrics = [
    ("cpu_usage", "server-01", "prod", 25.5),
    ("cpu_usage", "server-02", "prod", 30.2),
    ("cpu_usage", "server-03", "dev", 15.8),
    ("cpu_usage", "server-04", "dev", 20.1),
    ("memory_usage", "server-01", "prod", 60.5),
    ("memory_usage", "server-02", "prod", 65.3),
    ("memory_usage", "server-03", "dev", 40.2),
    ("memory_usage", "server-04", "dev", 45.8)
  ]
  
  // Aggregate by environment
  let prod_metrics = labeled_metrics.filter(fn(m) { m.2 == "prod" })
  let dev_metrics = labeled_metrics.filter(fn(m) { m.2 == "dev" })
  
  // Calculate averages by environment
  let prod_cpu_avg = prod_metrics
    .filter(fn(m) { m.0 == "cpu_usage" })
    .map(fn(m) { m.3 })
    .reduce(fn(acc, val) { acc + val }, 0.0) / 2.0
  
  let dev_cpu_avg = dev_metrics
    .filter(fn(m) { m.0 == "cpu_usage" })
    .map(fn(m) { m.3 })
    .reduce(fn(acc, val) { acc + val }, 0.0) / 2.0
  
  assert_eq(prod_cpu_avg, 27.85) // (25.5 + 30.2) / 2
  assert_eq(dev_cpu_avg, 17.95) // (15.8 + 20.1) / 2
  
  // Aggregate by metric type
  let cpu_metrics = labeled_metrics.filter(fn(m) { m.0 == "cpu_usage" })
  let memory_metrics = labeled_metrics.filter(fn(m) { m.0 == "memory_usage" })
  
  let overall_cpu_avg = cpu_metrics
    .map(fn(m) { m.3 })
    .reduce(fn(acc, val) { acc + val }, 0.0) / Int::to_float(cpu_metrics.length())
  
  let overall_memory_avg = memory_metrics
    .map(fn(m) { m.3 })
    .reduce(fn(acc, val) { acc + val }, 0.0) / Int::to_float(memory_metrics.length())
  
  assert_eq(overall_cpu_avg, 22.9) // (25.5 + 30.2 + 15.8 + 20.1) / 4
  assert_eq(overall_memory_avg, 52.95) // (60.5 + 65.3 + 40.2 + 45.8) / 4
}

// Test 6: Time-Based Metric Aggregation
test "time-based metric aggregation" {
  let time_series_metrics = [
    (1640995200, 10.5), // 2022-01-01 00:00:00
    (1640995260, 12.3), // 2022-01-01 00:01:00
    (1640995320, 11.8), // 2022-01-01 00:02:00
    (1640995380, 13.2), // 2022-01-01 00:03:00
    (1640995440, 14.1), // 2022-01-01 00:04:00
    (1640995500, 12.9), // 2022-01-01 00:05:00
    (1640995560, 15.3), // 2022-01-01 00:06:00
    (1640995620, 14.8)  // 2022-01-01 00:07:00
  ]
  
  // Aggregate by minute (already in minute intervals)
  let minute_averages = time_series_metrics.map(fn(point) { point.1 })
  let overall_avg = minute_averages.reduce(fn(acc, val) { acc + val }, 0.0) / Int::to_float(minute_averages.length())
  
  assert_eq(overall_avg, 13.1125)
  
  // Aggregate into 5-minute buckets
  let five_min_buckets = []
  for i in 0..<time_series_metrics.length() {
    if i % 5 == 0 {
      let bucket_start = i
      let bucket_end = if i + 5 > time_series_metrics.length() {
        time_series_metrics.length()
      } else {
        i + 5
      }
      
      let mut bucket_sum = 0.0
      let mut bucket_count = 0
      
      for j in bucket_start..<bucket_end {
        bucket_sum = bucket_sum + time_series_metrics[j].1
        bucket_count = bucket_count + 1
      }
      
      five_min_buckets = five_min_buckets.push(bucket_sum / Int::to_float(bucket_count))
    }
  }
  
  assert_eq(five_min_buckets.length(), 2)
  assert_eq(five_min_buckets[0], 12.38) // First 5 minutes average
  assert_eq(five_min_buckets[1], 14.33) // Last 3 minutes average
}

// Test 7: Rate Calculation Aggregation
test "rate calculation aggregation" {
  let counter_time_series = [
    (1640995200, 100), // 2022-01-01 00:00:00
    (1640995260, 120), // 2022-01-01 00:01:00
    (1640995320, 140), // 2022-01-01 00:02:00
    (1640995380, 165), // 2022-01-01 00:03:00
    (1640995440, 185), // 2022-01-01 00:04:00
    (1640995500, 210)  // 2022-01-01 00:05:00
  ]
  
  // Calculate rates per second
  let rates = []
  for i in 1..<counter_time_series.length() {
    let current = counter_time_series[i]
    let previous = counter_time_series[i - 1]
    let time_diff = current.0 - previous.0
    let value_diff = current.1 - previous.1
    let rate = Int::to_float(value_diff) / Int::to_float(time_diff)
    rates = rates.push(rate)
  }
  
  assert_eq(rates.length(), 5)
  
  // Verify rate calculations
  assert_eq(rates[0], 20.0 / 60.0)   // (120-100)/60
  assert_eq(rates[1], 20.0 / 60.0)   // (140-120)/60
  assert_eq(rates[2], 25.0 / 60.0)   // (165-140)/60
  assert_eq(rates[3], 20.0 / 60.0)   // (185-165)/60
  assert_eq(rates[4], 25.0 / 60.0)   // (210-185)/60
  
  // Calculate average rate
  let avg_rate = rates.reduce(fn(acc, rate) { acc + rate }, 0.0) / Int::to_float(rates.length())
  assert_eq(avg_rate, 22.0 / 60.0)
}

// Test 8: Metric Health Score Calculation
test "metric health score aggregation" {
  let system_metrics = [
    ("cpu_usage", 25.5, 80.0),    // value, weight
    ("memory_usage", 65.3, 70.0),
    ("disk_usage", 45.2, 60.0),
    ("network_latency", 120.5, 90.0),
    ("error_rate", 2.1, 100.0)
  ]
  
  // Define thresholds for each metric
  let thresholds = [
    ("cpu_usage", 50.0),
    ("memory_usage", 80.0),
    ("disk_usage", 70.0),
    ("network_latency", 200.0),
    ("error_rate", 5.0)
  ]
  
  // Calculate health scores (0-100, higher is better)
  let health_scores = []
  for metric in system_metrics {
    let threshold = thresholds.filter(fn(t) { t.0 == metric.0 })[0].1
    
    let score = if metric.1 <= threshold {
      100.0 - ((metric.1 / threshold) * 50.0) // Penalty for usage
    } else {
      50.0 - ((metric.1 - threshold) / threshold) * 50.0 // Higher penalty for exceeding threshold
    }
    
    health_scores = health_scores.push((metric.0, score, metric.2))
  }
  
  // Calculate weighted health score
  let weighted_sum = health_scores.reduce(fn(acc, score) { 
    acc + (score.1 * score.2) 
  }, 0.0)
  
  let total_weight = health_scores.reduce(fn(acc, score) { 
    acc + score.2 
  }, 0.0)
  
  let overall_health_score = weighted_sum / total_weight
  
  assert_eq(overall_health_score > 70.0, true) // System should be reasonably healthy
  assert_eq(overall_health_score < 100.0, true) // But not perfect
  
  // Check individual scores
  let cpu_score = health_scores.filter(fn(s) { s.0 == "cpu_usage" })[0].1
  let memory_score = health_scores.filter(fn(s) { s.0 == "memory_usage" })[0].1
  
  assert_eq(cpu_score > memory_score, true) // CPU usage should have better score than memory
}