// Azimuth Metrics Collection Test Suite
// This file contains comprehensive test cases for metrics collection and aggregation

// Test 1: Counter Metrics Collection
test "counter metrics collection and aggregation" {
  // Define counter metric structure
  type CounterMetric = {
    name: String,
    value: Int,
    labels: Array<(String, String)>,
    timestamp: Int
  }
  
  // Create counter metrics
  let counter_metrics = [
    { name: "http_requests_total", value: 1, labels: [("method", "GET"), ("status", "200")], timestamp: 1640995200 },
    { name: "http_requests_total", value: 1, labels: [("method", "GET"), ("status", "200")], timestamp: 1640995210 },
    { name: "http_requests_total", value: 1, labels: [("method", "POST"), ("status", "201")], timestamp: 1640995220 },
    { name: "http_requests_total", value: 1, labels: [("method", "GET"), ("status", "404")], timestamp: 1640995230 },
    { name: "http_requests_total", value: 1, labels: [("method", "POST"), ("status", "400")], timestamp: 1640995240 }
  ]
  
  // Aggregate counters by labels
  let aggregate_by_labels = fn(metrics: Array[CounterMetric]) {
    let groups = Map::empty()
    
    for metric in metrics {
      let label_key = metric.labels.map(fn(l) { l.0 + "=" + l.1 }).join(",")
      
      let existing = match Map::get(groups, label_key) {
        Some(value) => value
        None => 0
      }
      
      let _ = Map::insert(groups, label_key, existing + metric.value)
    }
    
    groups
  }
  
  let aggregated_counters = aggregate_by_labels(counter_metrics)
  
  // Verify aggregation results
  assert_eq(match Map::get(aggregated_counters, "method=GET,status=200") { Some(v) => v | None => 0 }, 2)
  assert_eq(match Map::get(aggregated_counters, "method=POST,status=201") { Some(v) => v | None => 0 }, 1)
  assert_eq(match Map::get(aggregated_counters, "method=GET,status=404") { Some(v) => v | None => 0 }, 1)
  assert_eq(match Map::get(aggregated_counters, "method=POST,status=400") { Some(v) => v | None => 0 }, 1)
  
  // Calculate total requests
  let total_requests = counter_metrics.reduce(0, fn(acc, m) { acc + m.value })
  assert_eq(total_requests, 5)
  
  // Calculate requests by method
  let get_requests = counter_metrics.filter(fn(m) { 
    m.labels.any(fn(l) { l.0 == "method" && l.1 == "GET" })
  }).reduce(0, fn(acc, m) { acc + m.value })
  assert_eq(get_requests, 3)
  
  let post_requests = counter_metrics.filter(fn(m) { 
    m.labels.any(fn(l) { l.0 == "method" && l.1 == "POST" })
  }).reduce(0, fn(acc, m) { acc + m.value })
  assert_eq(post_requests, 2)
}

// Test 2: Gauge Metrics Collection
test "gauge metrics collection and temporal analysis" {
  // Define gauge metric structure
  type GaugeMetric = {
    name: String,
    value: Float,
    labels: Array<(String, String)>,
    timestamp: Int
  }
  
  // Create gauge metrics representing memory usage over time
  let memory_gauges = [
    { name: "memory_usage_bytes", value: 1024.0 * 1024.0 * 100.0, labels: [("instance", "server-1")], timestamp: 1640995200 },
    { name: "memory_usage_bytes", value: 1024.0 * 1024.0 * 120.0, labels: [("instance", "server-1")], timestamp: 1640995210 },
    { name: "memory_usage_bytes", value: 1024.0 * 1024.0 * 95.0, labels: [("instance", "server-1")], timestamp: 1640995220 },
    { name: "memory_usage_bytes", value: 1024.0 * 1024.0 * 110.0, labels: [("instance", "server-1")], timestamp: 1640995230 },
    { name: "memory_usage_bytes", value: 1024.0 * 1024.0 * 105.0, labels: [("instance", "server-1")], timestamp: 1640995240 }
  ]
  
  // Extract values for analysis
  let memory_values = memory_gauges.map(fn(g) { g.value })
  assert_eq(memory_values.length(), 5)
  
  // Calculate statistics
  let min_memory = memory_values.reduce(memory_values[0], fn(acc, v) { if v < acc { v } else { acc } })
  let max_memory = memory_values.reduce(memory_values[0], fn(acc, v) { if v > acc { v } else { acc } })
  let avg_memory = memory_values.reduce(0.0, fn(acc, v) { acc + v }) / (memory_values.length() as Float)
  
  assert_eq(min_memory, 1024.0 * 1024.0 * 95.0)
  assert_eq(max_memory, 1024.0 * 1024.0 * 120.0)
  assert_eq(avg_memory, 1024.0 * 1024.0 * 106.0)
  
  // Calculate rate of change
  let calculate_rate_of_change = fn(values: Array[Float], timestamps: Array[Int]) {
    if values.length() < 2 || timestamps.length() < 2 {
      return []
    }
    
    let mut rates = []
    for i in 1..values.length() {
      let value_change = values[i] - values[i-1]
      let time_change = (timestamps[i] - timestamps[i-1]) as Float
      let rate = value_change / time_change
      rates = rates.push(rate)
    }
    rates
  }
  
  let timestamps = memory_gauges.map(fn(g) { g.timestamp })
  let memory_rates = calculate_rate_of_change(memory_values, timestamps)
  
  assert_eq(memory_rates.length(), 4)
  
  // Test threshold detection
  let memory_threshold = 1024.0 * 1024.0 * 115.0  // 115MB
  let threshold_violations = memory_gauges.filter(fn(g) { g.value > memory_threshold })
  assert_eq(threshold_violations.length(), 1)
  assert_eq(threshold_violations[0].value, 1024.0 * 1024.0 * 120.0)
}

// Test 3: Histogram Metrics Collection
test "histogram metrics collection and percentile calculation" {
  // Define histogram metric structure
  type HistogramMetric = {
    name: String,
    buckets: Array[(Float, Int)>,  // (upper_bound, count)
    count: Int,
    sum: Float,
    labels: Array<(String, String)>,
    timestamp: Int
  }
  
  // Create histogram for request duration
  let request_duration_histogram = {
    name: "http_request_duration_seconds",
    buckets: [
      (0.1, 10),   // 10 requests <= 0.1s
      (0.5, 25),   // 25 requests <= 0.5s
      (1.0, 40),   // 40 requests <= 1.0s
      (2.5, 48),   // 48 requests <= 2.5s
      (5.0, 50),   // 50 requests <= 5.0s
      (10.0, 50)   // 50 requests <= 10.0s
    ],
    count: 50,
    sum: 37.5,
    labels: [("endpoint", "/api/orders")],
    timestamp: 1640995200
  }
  
  // Calculate bucket counts
  let calculate_bucket_counts = fn(histogram: HistogramMetric) {
    let mut previous_count = 0
    let bucket_counts = histogram.buckets.map(fn(bucket) {
      let (upper_bound, cumulative_count) = bucket
      let bucket_count = cumulative_count - previous_count
      previous_count = cumulative_count
      (upper_bound, bucket_count)
    })
    bucket_counts
  }
  
  let bucket_counts = calculate_bucket_counts(request_duration_histogram)
  assert_eq(bucket_counts, [(0.1, 10), (0.5, 15), (1.0, 15), (2.5, 8), (5.0, 2), (10.0, 0)])
  
  // Calculate approximate percentiles
  let calculate_percentile = fn(histogram: HistogramMetric, percentile: Float) {
    let target_count = ((histogram.count as Float) * percentile / 100.0) as Int
    
    for bucket in histogram.buckets {
      let (_, cumulative_count) = bucket
      if cumulative_count >= target_count {
        return bucket.0
      }
    }
    
    histogram.buckets[histogram.buckets.length() - 1].0
  }
  
  let p50 = calculate_percentile(request_duration_histogram, 50.0)
  let p90 = calculate_percentile(request_duration_histogram, 90.0)
  let p95 = calculate_percentile(request_duration_histogram, 95.0)
  let p99 = calculate_percentile(request_duration_histogram, 99.0)
  
  assert_eq(p50, 0.5)   // 50th percentile falls in 0.5s bucket
  assert_eq(p90, 2.5)   // 90th percentile falls in 2.5s bucket
  assert_eq(p95, 5.0)   // 95th percentile falls in 5.0s bucket
  assert_eq(p99, 5.0)   // 99th percentile falls in 5.0s bucket
  
  // Calculate average request duration
  let avg_duration = request_duration_histogram.sum / (request_duration.count as Float)
  assert_eq(avg_duration, 0.75)  // 37.5 / 50
}

// Test 4: Summary Metrics Collection
test "summary metrics collection with quantiles" {
  // Define summary metric structure
  type SummaryMetric = {
    name: String,
    quantiles: Array[(Float, Float)>,  // (quantile, value)
    count: Int,
    sum: Float,
    labels: Array<(String, String)>,
    timestamp: Int
  }
  
  // Create summary for response sizes
  let response_size_summary = {
    name: "http_response_size_bytes",
    quantiles: [
      (0.5, 1024.0),    // 50th percentile: 1KB
      (0.9, 5120.0),    // 90th percentile: 5KB
      (0.95, 10240.0),  // 95th percentile: 10KB
      (0.99, 51200.0)   // 99th percentile: 50KB
    ],
    count: 1000,
    sum: 2048000.0,  // Total 2MB
    labels: [("endpoint", "/api/data")],
    timestamp: 1640995200
  }
  
  // Test quantile access
  let get_quantile_value = fn(summary: SummaryMetric, quantile: Float) {
    match summary.quantiles.find(fn(q) { q.0 == quantile }) {
      Some((_, value)) => Some(value)
      None => None
    }
  }
  
  assert_eq(get_quantile_value(response_size_summary, 0.5), Some(1024.0))
  assert_eq(get_quantile_value(response_size_summary, 0.9), Some(5120.0))
  assert_eq(get_quantile_value(response_size_summary, 0.95), Some(10240.0))
  assert_eq(get_quantile_value(response_size_summary, 0.99), Some(51200.0))
  assert_eq(get_quantile_value(response_size_summary, 0.75), None)  // Not defined
  
  // Calculate average response size
  let avg_response_size = response_size_summary.sum / (response_size_summary.count as Float)
  assert_eq(avg_response_size, 2048.0)  // 2MB / 1000 = 2KB
  
  // Test quantile interpolation
  let interpolate_quantile = fn(summary: SummaryMetric, target_quantile: Float) {
    if summary.quantiles.length() < 2 {
      return None
    }
    
    let sorted_quantiles = summary.quantiles.sort(fn(a, b) {
      if a.0 < b.0 { -1 } else if a.0 > b.0 { 1 } else { 0 }
    })
    
    for i in 0..(sorted_quantiles.length() - 1) {
      let (q1, v1) = sorted_quantiles[i]
      let (q2, v2) = sorted_quantiles[i + 1]
      
      if target_quantile >= q1 && target_quantile <= q2 {
        let ratio = (target_quantile - q1) / (q2 - q1)
        let interpolated_value = v1 + ratio * (v2 - v1)
        return Some(interpolated_value)
      }
    }
    
    None
  }
  
  let interpolated_75th = interpolate_quantile(response_size_summary, 0.75)
  assert_eq(interpolated_75th, Some(3072.0))  // Interpolated between 1024.0 and 5120.0
}

// Test 5: Multi-dimensional Metrics Aggregation
test "multi-dimensional metrics aggregation and rollups" {
  // Define multi-dimensional metric
  type MultiDimMetric = {
    name: String,
    value: Float,
    dimensions: Array<(String, String)>,
    timestamp: Int
  }
  
  // Create metrics with multiple dimensions
  let multi_dim_metrics = [
    { name: "cpu_usage_percent", value: 45.2, dimensions: [("host", "server-1"), ("region", "us-west"), ("env", "prod")], timestamp: 1640995200 },
    { name: "cpu_usage_percent", value: 38.7, dimensions: [("host", "server-2"), ("region", "us-west"), ("env", "prod")], timestamp: 1640995200 },
    { name: "cpu_usage_percent", value: 72.1, dimensions: [("host", "server-3"), ("region", "us-east"), ("env", "prod")], timestamp: 1640995200 },
    { name: "cpu_usage_percent", value: 25.4, dimensions: [("host", "server-1"), ("region", "us-west"), ("env", "dev")], timestamp: 1640995200 },
    { name: "cpu_usage_percent", value: 31.9, dimensions: [("host", "server-2"), ("region", "us-west"), ("env", "dev")], timestamp: 1640995200 },
    { name: "cpu_usage_percent", value: 68.3, dimensions: [("host", "server-4"), ("region", "eu-west"), ("env", "prod")], timestamp: 1640995200 }
  ]
  
  // Aggregate by specific dimensions
  let aggregate_by_dimensions = fn(metrics: Array[MultiDimMetric], group_by: Array<String>) {
    let groups = Map::empty()
    
    for metric in metrics {
      let dimension_values = group_by.map(fn(dim) {
        match metric.dimensions.find(fn(d) { d.0 == dim }) {
          Some((_, value)) => value
          None => "unknown"
        }
      })
      let group_key = dimension_values.join("|")
      
      let existing = match Map::get(groups, group_key) {
        Some(values) => values
        None => []
      }
      
      let _ = Map::insert(groups, group_key, existing.push(metric.value))
    }
    
    groups
  }
  
  // Aggregate by region
  let region_groups = aggregate_by_dimensions(multi_dim_metrics, ["region"])
  
  // Calculate averages for each region
  let calculate_average = fn(values: Array[Float>) {
    if values.length() == 0 {
      0.0
    } else {
      values.reduce(0.0, fn(acc, v) { acc + v }) / (values.length() as Float)
    }
  }
  
  let us_west_values = match Map::get(region_groups, "us-west") { Some(v) => v | None => [] }
  let us_east_values = match Map::get(region_groups, "us-east") { Some(v) => v | None => [] }
  let eu_west_values = match Map::get(region_groups, "eu-west") { Some(v) => v | None => [] }
  
  assert_eq(us_west_values.length(), 4)
  assert_eq(us_east_values.length(), 1)
  assert_eq(eu_west_values.length(), 1)
  
  assert_eq(calculate_average(us_west_values).round(), 35.3)  // (45.2 + 38.7 + 25.4 + 31.9) / 4
  assert_eq(calculate_average(us_east_values), 72.1)
  assert_eq(calculate_average(eu_west_values), 68.3)
  
  // Aggregate by region and environment
  let region_env_groups = aggregate_by_dimensions(multi_dim_metrics, ["region", "env"])
  
  let prod_us_west = match Map::get(region_env_groups, "us-west|prod") { Some(v) => v | None => [] }
  let dev_us_west = match Map::get(region_env_groups, "us-west|dev") { Some(v) => v | None => [] }
  let prod_us_east = match Map::get(region_env_groups, "us-east|prod") { Some(v) => v | None => [] }
  let prod_eu_west = match Map::get(region_env_groups, "eu-west|prod") { Some(v) => v | None => [] }
  
  assert_eq(prod_us_west.length(), 2)  // server-1, server-2
  assert_eq(dev_us_west.length(), 2)   // server-1, server-2
  assert_eq(prod_us_east.length(), 1)  // server-3
  assert_eq(prod_eu_west.length(), 1)  // server-4
  
  assert_eq(calculate_average(prod_us_west).round(), 42.0)  // (45.2 + 38.7) / 2
  assert_eq(calculate_average(dev_us_west).round(), 29.0)   // (25.4 + 31.9) / 2
  assert_eq(calculate_average(prod_us_east), 72.1)
  assert_eq(calculate_average(prod_eu_west), 68.3)
}

// Test 6: Time Series Metrics Analysis
test "time series metrics analysis and trend detection" {
  // Define time series metric
  type TimeSeriesMetric = {
    name: String,
    values: Array[(Int, Float)>,  // (timestamp, value)
    labels: Array<(String, String)>
  }
  
  // Create time series for error rate over time
  let error_rate_time_series = {
    name: "error_rate_percent",
    values: [
      (1640995200, 1.2),  // 00:00:00
      (1640995260, 1.5),  // 00:01:00
      (1640995320, 1.8),  // 00:02:00
      (1640995380, 2.1),  // 00:03:00
      (1640995440, 1.9),  // 00:04:00
      (1640995500, 1.6),  // 00:05:00
      (1640995560, 1.4),  // 00:06:00
      (1640995620, 1.3),  // 00:07:00
      (1640995680, 1.1),  // 00:08:00
      (1640995740, 1.0)   // 00:09:00
    ],
    labels: [("service", "api-gateway")]
  }
  
  // Extract timestamps and values
  let timestamps = error_rate_time_series.values.map(fn(p) { p.0 })
  let error_rates = error_rate_time_series.values.map(fn(p) { p.1 })
  
  assert_eq(timestamps.length(), 10)
  assert_eq(error_rates.length(), 10)
  
  // Calculate moving average
  let calculate_moving_average = fn(values: Array[Float>, window_size: Int) {
    if window_size <= 0 || values.length() < window_size {
      return []
    }
    
    let mut moving_averages = []
    for i in (window_size - 1)..values.length() {
      let mut sum = 0.0
      for j in (i - window_size + 1)..=i {
        sum = sum + values[j]
      }
      let avg = sum / (window_size as Float)
      moving_averages = moving_averages.push(avg)
    }
    moving_averages
  }
  
  let moving_averages_3 = calculate_moving_average(error_rates, 3)
  assert_eq(moving_averages_3.length(), 8)
  assert_eq(moving_averages_3[0].round(), 1.5)  // (1.2 + 1.5 + 1.8) / 3
  assert_eq(moving_averages_3[7].round(), 1.1)  // (1.1 + 1.0) / 3
  
  // Detect trend
  let detect_trend = fn(values: Array[Float>) {
    if values.length() < 2 {
      return "insufficient_data"
    }
    
    let first_half = values.slice(0, values.length() / 2)
    let second_half = values.slice(values.length() / 2, values.length())
    
    let first_avg = first_half.reduce(0.0, fn(acc, v) { acc + v }) / (first_half.length() as Float)
    let second_avg = second_half.reduce(0.0, fn(acc, v) { acc + v }) / (second_half.length() as Float)
    
    let difference = second_avg - first_avg
    let threshold = 0.1  // 10% change threshold
    
    if difference > threshold {
      "increasing"
    } else if difference < -threshold {
      "decreasing"
    } else {
      "stable"
    }
  }
  
  let overall_trend = detect_trend(error_rates)
  assert_eq(overall_trend, "stable")  // Error rate is relatively stable
  
  // Detect anomalies
  let detect_anomalies = fn(values: Array[Float], threshold: Float) {
    let mean = values.reduce(0.0, fn(acc, v) { acc + v }) / (values.length() as Float)
    let variance = values.reduce(0.0, fn(acc, v) { acc + (v - mean) * (v - mean) }) / (values.length() as Float)
    let std_dev = variance.sqrt()
    
    values.filter(fn(v) { (v - mean).abs() > threshold * std_dev })
  }
  
  let anomalies = detect_anomalies(error_rates, 2.0)  // 2 standard deviations
  assert_eq(anomalies.length(), 2)  // 2.1 and possibly 1.0 depending on calculation
}

// Test 7: Metrics Sampling and Rate Limiting
test "metrics sampling and rate limiting strategies" {
  // Define sampling strategy
  enum SamplingStrategy {
    FixedRate(Int)  // Sample every Nth metric
    TimeWindow(Int)  // Sample at most N metrics per time window
    Random(Float)   // Random sampling with probability
    Adaptive(Float)  // Adaptive sampling based on metric volume
  }
  
  // Create test metrics
  let test_metrics = [
    { name: "metric_1", value: 1.0, timestamp: 1640995200 },
    { name: "metric_2", value: 2.0, timestamp: 1640995210 },
    { name: "metric_3", value: 3.0, timestamp: 1640995220 },
    { name: "metric_4", value: 4.0, timestamp: 1640995230 },
    { name: "metric_5", value: 5.0, timestamp: 1640995240 },
    { name: "metric_6", value: 6.0, timestamp: 1640995250 },
    { name: "metric_7", value: 7.0, timestamp: 1640995260 },
    { name: "metric_8", value: 8.0, timestamp: 1640995270 },
    { name: "metric_9", value: 9.0, timestamp: 1640995280 },
    { name: "metric_10", value: 10.0, timestamp: 1640995290 }
  ]
  
  // Apply fixed rate sampling (every 3rd metric)
  let apply_fixed_rate_sampling = fn(metrics: Array<{name: String, value: Float, timestamp: Int}>, rate: Int) {
    let mut sampled = []
    for i in 0..metrics.length() {
      if i % rate == 0 {
        sampled = sampled.push(metrics[i])
      }
    }
    sampled
  }
  
  let fixed_rate_sampled = apply_fixed_rate_sampling(test_metrics, 3)
  assert_eq(fixed_rate_sampled.length(), 4)  // metrics 1, 4, 7, 10
  assert_eq(fixed_rate_sampled[0].name, "metric_1")
  assert_eq(fixed_rate_sampled[1].name, "metric_4")
  assert_eq(fixed_rate_sampled[2].name, "metric_7")
  assert_eq(fixed_rate_sampled[3].name, "metric_10")
  
  // Apply time window sampling (max 3 per 60-second window)
  let apply_time_window_sampling = fn(metrics: Array<{name: String, value: Float, timestamp: Int}>, max_per_window: Int, window_size: Int) {
    let mut sampled = []
    let mut window_count = 0
    let mut window_start = if metrics.length() > 0 { metrics[0].timestamp } else { 0 }
    
    for metric in metrics {
      if metric.timestamp - window_start >= window_size {
        window_count = 0
        window_start = metric.timestamp
      }
      
      if window_count < max_per_window {
        sampled = sampled.push(metric)
        window_count = window_count + 1
      }
    }
    
    sampled
  }
  
  let time_window_sampled = apply_time_window_sampling(test_metrics, 3, 60)  // 60-second window
  assert_eq(time_window_sampled.length(), 10)  // All metrics fit within the rate limit
  
  // Apply random sampling
  let apply_random_sampling = fn(metrics: Array<{name: String, value: Float, timestamp: Int}>, probability: Float) {
    let mut sampled = []
    for metric in metrics {
      // Simple pseudo-random based on metric name length
      let pseudo_random = (metric.name.length() as Float) / 10.0
      if pseudo_random <= probability {
        sampled = sampled.push(metric)
      }
    }
    sampled
  }
  
  let random_sampled = apply_random_sampling(test_metrics, 0.5)  // 50% probability
  assert_eq(random_sampled.length(), 5)  // metrics with name length <= 5
  
  // Apply adaptive sampling based on metric value
  let apply_adaptive_sampling = fn(metrics: Array<{name: String, value: Float, timestamp: Int}>, threshold: Float) {
    let mut sampled = []
    for metric in metrics {
      // Sample metrics with values above threshold more frequently
      let sampling_rate = if metric.value > threshold { 0.8 } else { 0.3 }
      let pseudo_random = (metric.name.length() as Float) / 10.0
      
      if pseudo_random <= sampling_rate {
        sampled = sampled.push(metric)
      }
    }
    sampled
  }
  
  let adaptive_sampled = apply_adaptive_sampling(test_metrics, 5.0)
  assert_true(adaptive_sampled.length() > 0)
  
  // Verify high-value metrics are sampled more frequently
  let high_value_metrics = adaptive_sampled.filter(fn(m) { m.value > 5.0 })
  let low_value_metrics = adaptive_sampled.filter(fn(m) { m.value <= 5.0 })
  
  assert_true(high_value_metrics.length() >= low_value_metrics.length())
}

// Test 8: Metrics Downsampling and Retention
test "metrics downsampling and retention policies" {
  // Define retention policy
  type RetentionPolicy = {
    raw_resolution: Int,    // Keep raw data for this many seconds
    downsample_1m: Int,     // Keep 1-minute downsampled data for this many seconds
    downsample_5m: Int,     // Keep 5-minute downsampled data for this many seconds
    downsample_1h: Int,     // Keep 1-hour downsampled data for this many seconds
  }
  
  // Create retention policy
  let policy = {
    raw_resolution: 3600,   // 1 hour
    downsample_1m: 86400,   // 1 day
    downsample_5m: 604800,  // 1 week
    downsample_1h: 2592000  // 1 month
  }
  
  // Create high-resolution metrics
  let high_res_metrics = []
  for i in 0..60 {
    high_res_metrics = high_res_metrics.push({
      name: "cpu_usage",
      value: 50.0 + (i as Float) * 0.5,
      timestamp: 1640995200 + i * 10  // Every 10 seconds
    })
  }
  
  assert_eq(high_res_metrics.length(), 60)
  
  // Downsample to 1-minute resolution
  let downsample_to_1m = fn(metrics: Array<{name: String, value: Float, timestamp: Int}>) {
    let mut downsampled = []
    let mut window_start = if metrics.length() > 0 { metrics[0].timestamp } else { 0 }
    let mut window_values = []
    
    for metric in metrics {
      if metric.timestamp - window_start >= 60 {
        // Process current window
        if window_values.length() > 0 {
          let avg = window_values.reduce(0.0, fn(acc, v) { acc + v }) / (window_values.length() as Float)
          downsampled = downsampled.push({
            name: metrics[0].name,
            value: avg,
            timestamp: window_start
          })
        }
        
        // Start new window
        window_start = metric.timestamp
        window_values = [metric.value]
      } else {
        window_values = window_values.push(metric.value)
      }
    }
    
    // Process last window
    if window_values.length() > 0 {
      let avg = window_values.reduce(0.0, fn(acc, v) { acc + v }) / (window_values.length() as Float)
      downsampled = downsampled.push({
        name: metrics[0].name,
        value: avg,
        timestamp: window_start
      })
    }
    
    downsampled
  }
  
  let downsampled_1m = downsample_to_1m(high_res_metrics)
  assert_eq(downsampled_1m.length(), 10)  // 60 points / 6 points per minute
  
  // Downsample to 5-minute resolution
  let downsample_to_5m = fn(metrics: Array<{name: String, value: Float, timestamp: Int}>) {
    let mut downsampled = []
    let mut window_start = if metrics.length() > 0 { metrics[0].timestamp } else { 0 }
    let mut window_values = []
    
    for metric in metrics {
      if metric.timestamp - window_start >= 300 {
        // Process current window
        if window_values.length() > 0 {
          let avg = window_values.reduce(0.0, fn(acc, v) { acc + v }) / (window_values.length() as Float)
          downsampled = downsampled.push({
            name: metrics[0].name,
            value: avg,
            timestamp: window_start
          })
        }
        
        // Start new window
        window_start = metric.timestamp
        window_values = [metric.value]
      } else {
        window_values = window_values.push(metric.value)
      }
    }
    
    // Process last window
    if window_values.length() > 0 {
      let avg = window_values.reduce(0.0, fn(acc, v) { acc + v }) / (window_values.length() as Float)
      downsampled = downsampled.push({
        name: metrics[0].name,
        value: avg,
        timestamp: window_start
      })
    }
    
    downsampled
  }
  
  let downsampled_5m = downsample_to_5m(high_res_metrics)
  assert_eq(downsampled_5m.length(), 2)  // 60 points / 30 points per 5 minutes
  
  // Test retention policy application
  let apply_retention_policy = fn(current_time: Int, metrics: Array<{name: String, value: Float, timestamp: Int}>, policy: RetentionPolicy) {
    let raw_metrics = metrics.filter(fn(m) { current_time - m.timestamp <= policy.raw_resolution })
    let _1m_metrics = downsample_to_1m(metrics).filter(fn(m) { current_time - m.timestamp <= policy.downsample_1m })
    let _5m_metrics = downsample_to_5m(metrics).filter(fn(m) { current_time - m.timestamp <= policy.downsample_5m })
    
    { raw_metrics, downsampled_1m: _1m_metrics, downsampled_5m: _5m_metrics }
  }
  
  let current_time = 1640995200 + 60 * 60  // 1 hour after first metric
  let retained_metrics = apply_retention_policy(current_time, high_res_metrics, policy)
  
  assert_eq(retained_metrics.raw_metrics.length(), 0)  // All raw metrics expired
  assert_eq(retained_metrics.downsampled_1m.length(), 10)  // All 1-minute metrics retained
  assert_eq(retained_metrics.downsampled_5m.length(), 2)   // All 5-minute metrics retained
  
  // Verify downsampling accuracy
  assert_eq(downsampled_1m[0].value.round(), 50.0 + 0.5 * 2.5)  // Average of first 6 points
  assert_eq(downsampled_1m[9].value.round(), 50.0 + 0.5 * 57.5) // Average of last 6 points
  
  assert_eq(downsampled_5m[0].value.round(), 50.0 + 0.5 * 14.5)  // Average of first 30 points
  assert_eq(downsampled_5m[1].value.round(), 50.0 + 0.5 * 44.5)  // Average of last 30 points
}