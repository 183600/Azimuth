// Azimuth Telemetry System - New Metrics Collection Tests
// This file contains test cases for metrics collection and analysis functionality

// Test 1: Counter Metrics Collection
test "counter metrics collection operations" {
  // Create counter metric
  let counter = {
    name: "http_requests_total",
    description: "Total number of HTTP requests",
    unit: "count",
    value: 0,
    labels: [("method", "GET"), ("status", "200")]
  }
  
  // Increment counter function
  let increment_counter = fn(counter: Dynamic, amount: Int) {
    { counter | value: counter.value + amount }
  }
  
  // Add label function
  let add_label = fn(counter: Dynamic, key: String, value: String) {
    let existing_labels = counter.labels
    let updated_labels = existing_labels.push((key, value))
    { counter | labels: updated_labels }
  }
  
  // Test counter increment
  let incremented = increment_counter(counter, 1)
  assert_eq(incremented.value, 1)
  
  let incremented_more = increment_counter(incremented, 5)
  assert_eq(incremented_more.value, 6)
  
  // Test adding labels
  let with_labels = add_label(counter, "service", "api-gateway")
  assert_eq(with_labels.labels.length(), 3)
  assert_true(with_labels.labels.contains(("service", "api-gateway")))
  
  // Test counter with different label combinations
  let counter1 = increment_counter(add_label(counter, "endpoint", "/api/users"), 10)
  let counter2 = increment_counter(add_label(counter, "endpoint", "/api/orders"), 5)
  
  assert_eq(counter1.value, 10)
  assert_eq(counter2.value, 5)
  assert_eq(counter1.labels.find_fn(l) { l.0 == "endpoint" }.1, "/api/users")
  assert_eq(counter2.labels.find_fn(l) { l.0 == "endpoint" }.1, "/api/orders")
}

// Test 2: Gauge Metrics Collection
test "gauge metrics collection operations" {
  // Create gauge metric
  let gauge = {
    name: "memory_usage_bytes",
    description: "Current memory usage in bytes",
    unit: "bytes",
    value: 1024000,
    labels: [("service", "payment-service")]
  }
  
  // Set gauge value function
  let set_gauge = fn(gauge: Dynamic, value: Int) {
    { gauge | value: value }
  }
  
  // Increment gauge function
  let increment_gauge = fn(gauge: Dynamic, amount: Int) {
    { gauge | value: gauge.value + amount }
  }
  
  // Decrement gauge function
  let decrement_gauge = fn(gauge: Dynamic, amount: Int) {
    { gauge | value: gauge.value - amount }
  }
  
  // Test setting gauge value
  let set_value = set_gauge(gauge, 2048000)
  assert_eq(set_value.value, 2048000)
  
  // Test incrementing gauge
  let incremented = increment_gauge(gauge, 512000)
  assert_eq(incremented.value, 1536000) // 1024000 + 512000
  
  // Test decrementing gauge
  let decremented = decrement_gauge(incremented, 256000)
  assert_eq(decremented.value, 1280000) // 1536000 - 256000
  
  // Test gauge with multiple operations
  let mut current_gauge = gauge
  current_gauge = increment_gauge(current_gauge, 1000000)
  current_gauge = set_gauge(current_gauge, 500000)
  current_gauge = increment_gauge(current_gauge, 200000)
  current_gauge = decrement_gauge(current_gauge, 100000)
  
  assert_eq(current_gauge.value, 600000) // 500000 + 200000 - 100000
}

// Test 3: Histogram Metrics Collection
test "histogram metrics collection operations" {
  // Create histogram metric
  let histogram = {
    name: "request_duration_seconds",
    description: "Request duration in seconds",
    unit: "seconds",
    buckets: [0.1, 0.5, 1.0, 2.5, 5.0, 10.0],
    bucket_counts: [0, 0, 0, 0, 0, 0, 0], // One extra for > max bucket
    sum: 0.0,
    count: 0,
    labels: [("endpoint", "/api/process")]
  }
  
  // Observe value function
  let observe = fn(histogram: Dynamic, value: Float) {
    let mut bucket_counts = histogram.bucket_counts
    let mut i = 0
    
    while i < histogram.buckets.length() {
      if value <= histogram.buckets[i] {
        bucket_counts[i] = bucket_counts[i] + 1
      }
      i = i + 1
    }
    
    // Always increment the last bucket (for values > max bucket)
    bucket_counts[histogram.buckets.length()] = bucket_counts[histogram.buckets.length()] + 1
    
    {
      name: histogram.name,
      description: histogram.description,
      unit: histogram.unit,
      buckets: histogram.buckets,
      bucket_counts: bucket_counts,
      sum: histogram.sum + value,
      count: histogram.count + 1,
      labels: histogram.labels
    }
  }
  
  // Calculate percentile function
  let calculate_percentile = fn(histogram: Dynamic, percentile: Float) {
    if histogram.count == 0 {
      0.0
    } else {
      let target_count = (percentile * histogram.count.to_float()) / 100.0
      let mut cumulative_count = 0
      let mut i = 0
      
      while i < histogram.buckets.length() {
        cumulative_count = cumulative_count + histogram.bucket_counts[i]
        if cumulative_count >= target_count {
          return histogram.buckets[i]
        }
        i = i + 1
      }
      
      // If we haven't reached the target, return the max bucket value
      histogram.buckets[histogram.buckets.length() - 1]
    }
  }
  
  // Test observing values
  let observed1 = observe(histogram, 0.3)
  assert_eq(observed1.count, 1)
  assert_eq(observed1.sum, 0.3)
  assert_eq(observed1.bucket_counts, [1, 1, 1, 1, 1, 1, 1]) // 0.3 <= all buckets
  
  let observed2 = observe(observed1, 0.8)
  assert_eq(observed2.count, 2)
  assert_eq(observed2.sum, 1.1) // 0.3 + 0.8
  assert_eq(observed2.bucket_counts, [2, 2, 2, 2, 2, 2, 2]) // 0.8 <= all buckets
  
  let observed3 = observe(observed2, 3.0)
  assert_eq(observed3.count, 3)
  assert_eq(observed3.sum, 4.1) // 1.1 + 3.0
  assert_eq(observed3.bucket_counts, [3, 3, 3, 3, 3, 3, 3]) // 3.0 <= all buckets
  
  let observed4 = observe(observed3, 15.0)
  assert_eq(observed4.count, 4)
  assert_eq(observed4.sum, 19.1) // 4.1 + 15.0
  assert_eq(observed4.bucket_counts, [4, 4, 4, 4, 4, 4, 4]) // 15.0 > all buckets, but counts increment for all
  
  // Test percentile calculation
  let p50 = calculate_percentile(observed4, 50.0)
  let p95 = calculate_percentile(observed4, 95.0)
  let p99 = calculate_percentile(observed4, 99.0)
  
  assert_eq(p50, 0.1) // 50th percentile falls in first bucket
  assert_eq(p95, 10.0) // 95th percentile falls in last bucket
  assert_eq(p99, 10.0) // 99th percentile falls in last bucket
}

// Test 4: Metrics Aggregation
test "metrics aggregation operations" {
  // Create multiple metrics with same name but different labels
  let metrics = [
    {
      name: "http_requests_total",
      value: 100,
      labels: [("method", "GET"), ("status", "200")]
    },
    {
      name: "http_requests_total",
      value: 25,
      labels: [("method", "GET"), ("status", "404")]
    },
    {
      name: "http_requests_total",
      value: 50,
      labels: [("method", "POST"), ("status", "200")]
    },
    {
      name: "http_requests_total",
      value: 10,
      labels: [("method", "POST"), ("status", "500")]
    },
    {
      name: "cpu_usage_percent",
      value: 75.5,
      labels: [("service", "api-gateway")]
    },
    {
      name: "cpu_usage_percent",
      value: 45.2,
      labels: [("service", "payment-service")]
    }
  ]
  
  // Aggregate metrics by name
  let aggregate_by_name = fn(metrics: Array[Dynamic]) {
    let mut result = []
    let mut processed_names = []
    
    for metric in metrics {
      let metric_name = metric.name
      
      if not(processed_names.contains(metric_name)) {
        let same_name_metrics = metrics.filter_fn(m) { m.name == metric_name }
        let total_value = same_name_metrics.reduce(fn(sum, m) { sum + m.value }, 0)
        
        result = result.push({
          name: metric_name,
          total_value: total_value,
          count: same_name_metrics.length(),
          metrics: same_name_metrics
        })
        
        processed_names = processed_names.push(metric_name)
      }
    }
    
    result
  }
  
  // Aggregate metrics by label
  let aggregate_by_label = fn(metrics: Array[Dynamic], label_key: String) {
    let mut result = []
    let mut processed_values = []
    
    for metric in metrics {
      let label_value = metric.labels.find_fn(l) { l.0 == label_key }.1
      
      if not(processed_values.contains(label_value)) {
        let same_label_metrics = metrics.filter_fn(m) { 
          m.labels.find_fn(l) { l.0 == label_key }.1 == label_value 
        }
        let total_value = same_label_metrics.reduce(fn(sum, m) { sum + m.value }, 0)
        
        result = result.push({
          label_key: label_key,
          label_value: label_value,
          total_value: total_value,
          count: same_label_metrics.length(),
          metrics: same_label_metrics
        })
        
        processed_values = processed_values.push(label_value)
      }
    }
    
    result
  }
  
  // Test aggregation by name
  let by_name = aggregate_by_name(metrics)
  assert_eq(by_name.length(), 2) // http_requests_total, cpu_usage_percent
  
  let http_metrics = by_name.find_fn(a) { a.name == "http_requests_total" }
  assert_eq(http_metrics.total_value, 185) // 100 + 25 + 50 + 10
  assert_eq(http_metrics.count, 4)
  
  let cpu_metrics = by_name.find_fn(a) { a.name == "cpu_usage_percent" }
  assert_eq(cpu_metrics.total_value, 120.7) // 75.5 + 45.2
  assert_eq(cpu_metrics.count, 2)
  
  // Test aggregation by method label
  let by_method = aggregate_by_label(metrics.filter_fn(m) { m.name == "http_requests_total" }, "method")
  assert_eq(by_method.length(), 2) // GET, POST
  
  let get_metrics = by_method.find_fn(a) { a.label_value == "GET" }
  assert_eq(get_metrics.total_value, 125) // 100 + 25
  assert_eq(get_metrics.count, 2)
  
  let post_metrics = by_method.find_fn(a) { a.label_value == "POST" }
  assert_eq(post_metrics.total_value, 60) // 50 + 10
  assert_eq(post_metrics.count, 2)
}

// Test 5: Metrics Rate Calculation
test "metrics rate calculation operations" {
  // Create time-series metrics data
  let time_series_metrics = [
    {
      timestamp: 1640995200,
      metrics: [
        { name: "requests_total", value: 100 },
        { name: "errors_total", value: 5 }
      ]
    },
    {
      timestamp: 1640995260, // 1 minute later
      metrics: [
        { name: "requests_total", value: 160 },
        { name: "errors_total", value: 8 }
      ]
    },
    {
      timestamp: 1640995320, // 2 minutes later
      metrics: [
        { name: "requests_total", value: 220 },
        { name: "errors_total", value: 12 }
      ]
    },
    {
      timestamp: 1640995380, // 3 minutes later
      metrics: [
        { name: "requests_total", value: 280 },
        { name: "errors_total", value: 15 }
      ]
    }
  ]
  
  // Calculate rate function
  let calculate_rate = fn(data: Array[Dynamic], metric_name: String, window_minutes: Int) {
    if data.length() < 2 {
      0.0
    } else {
      let latest = data[data.length() - 1]
      let window_start_timestamp = latest.timestamp - (window_minutes * 60)
      
      // Find the data point at the start of the window
      let window_start_data = data.find_fn(d) { d.timestamp >= window_start_timestamp }
      
      match window_start_data {
        Some(start_data) => {
          let latest_value = latest.metrics.find_fn(m) { m.name == metric_name }.value
          let start_value = start_data.metrics.find_fn(m) { m.name == metric_name }.value
          let time_diff = (latest.timestamp - start_data.timestamp) / 60 // in minutes
          
          if time_diff > 0 {
            (latest_value - start_value).to_float() / time_diff.to_float()
          } else {
            0.0
          }
        }
        None => {
          0.0
        }
      }
    }
  }
  
  // Calculate error rate function
  let calculate_error_rate = fn(data: Array[Dynamic]) {
    if data.length() < 2 {
      0.0
    } else {
      let latest = data[data.length() - 1]
      let window_start_timestamp = latest.timestamp - (3 * 60) // 3 minute window
      
      // Find the data point at the start of the window
      let window_start_data = data.find_fn(d) { d.timestamp >= window_start_timestamp }
      
      match window_start_data {
        Some(start_data) => {
          let latest_requests = latest.metrics.find_fn(m) { m.name == "requests_total" }.value
          let latest_errors = latest.metrics.find_fn(m) { m.name == "errors_total" }.value
          let start_requests = start_data.metrics.find_fn(m) { m.name == "requests_total" }.value
          let start_errors = start_data.metrics.find_fn(m) { m.name == "errors_total" }.value
          
          let request_diff = latest_requests - start_requests
          let error_diff = latest_errors - start_errors
          
          if request_diff > 0 {
            (error_diff.to_float() / request_diff.to_float()) * 100.0
          } else {
            0.0
          }
        }
        None => {
          0.0
        }
      }
    }
  }
  
  // Test rate calculation
  let request_rate = calculate_rate(time_series_metrics, "requests_total", 3)
  assert_eq(request_rate, 60.0) // (280 - 100) / 3 minutes
  
  let error_rate = calculate_rate(time_series_metrics, "errors_total", 3)
  assert_eq(error_rate, 3.33) // (15 - 5) / 3 minutes ≈ 3.33
  
  // Test error rate percentage
  let error_percentage = calculate_error_rate(time_series_metrics)
  assert_eq(error_percentage, 5.55) // (15 - 5) / (280 - 100) * 100 ≈ 5.55%
  
  // Test with insufficient data
  let insufficient_data = [time_series_metrics[0]]
  let insufficient_rate = calculate_rate(insufficient_data, "requests_total", 1)
  assert_eq(insufficient_rate, 0.0)
}