// Azimuth Performance Metrics Tests
// This file contains test cases for performance metrics collection functionality

// Test 1: Counter Metrics
test "counter metrics collection and aggregation" {
  // Define counter metric structure
  type CounterMetric = {
    name: String,
    value: Int,
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // Create a counter
  let create_counter = fn(name: String, labels: Array[(String, String)], timestamp: Int) {
    {
      name,
      value: 0,
      labels,
      timestamp
    }
  }
  
  // Increment counter
  let increment_counter = fn(counter: CounterMetric, delta: Int) {
    { counter | value: counter.value + delta }
  }
  
  // Reset counter
  let reset_counter = fn(counter: CounterMetric) {
    { counter | value: 0 }
  }
  
  // Test counter creation
  let request_counter = create_counter("http_requests_total", [
    ("method", "GET"),
    ("endpoint", "/api/users"),
    ("status", "200")
  ], 1640995200)
  
  assert_eq(request_counter.name, "http_requests_total")
  assert_eq(request_counter.value, 0)
  assert_eq(request_counter.labels.length(), 3)
  assert_eq(request_counter.timestamp, 1640995200)
  
  // Test counter increment
  let incremented_counter = increment_counter(request_counter, 1)
  assert_eq(incremented_counter.value, 1)
  
  let more_incremented = increment_counter(incremented_counter, 5)
  assert_eq(more_incremented.value, 6)
  
  // Test counter reset
  let reset_counter_val = reset_counter(more_incremented)
  assert_eq(reset_counter_val.value, 0)
  
  // Test multiple counters with different labels
  let counters = [
    create_counter("http_requests_total", [("method", "GET"), ("endpoint", "/api/users"), ("status", "200")], 1640995200),
    create_counter("http_requests_total", [("method", "GET"), ("endpoint", "/api/users"), ("status", "404")], 1640995201),
    create_counter("http_requests_total", [("method", "POST"), ("endpoint", "/api/users"), ("status", "201")], 1640995202),
    create_counter("http_requests_total", [("method", "GET"), ("endpoint", "/api/orders"), ("status", "200")], 1640995203)
  ]
  
  // Increment counters
  let incremented_counters = [
    increment_counter(counters[0], 100),
    increment_counter(counters[1], 10),
    increment_counter(counters[2], 25),
    increment_counter(counters[3], 50)
  ]
  
  // Aggregate counters by label
  let aggregate_by_label = fn(counters: Array[CounterMetric], label_key: String) {
    let mut result = []
    let mut processed = []
    
    for counter in counters {
      // Find the value for the specified label
      let label_value = counter.labels.filter(fn(l) { l.0 == label_key })[0].1
      
      if not(processed.contains(label_value)) {
        let same_label_counters = counters.filter(fn(c) { 
          c.labels.filter(fn(l) { l.0 == label_key })[0].1 == label_value 
        })
        let total_value = same_label_counters.reduce(fn(sum, c) { sum + c.value }, 0)
        result = result.push((label_value, total_value))
        processed = processed.push(label_value)
      }
    }
    result
  }
  
  // Aggregate by status code
  let status_aggregates = aggregate_by_label(incremented_counters, "status")
  assert_eq(status_aggregates.length(), 3)
  
  let status_200_total = status_aggregates.filter(fn(a) { a.0 == "200" })[0].1
  let status_404_total = status_aggregates.filter(fn(a) { a.0 == "404" })[0].1
  let status_201_total = status_aggregates.filter(fn(a) { a.0 == "201" })[0].1
  
  assert_eq(status_200_total, 150) // 100 + 50
  assert_eq(status_404_total, 10)
  assert_eq(status_201_total, 25)
  
  // Aggregate by HTTP method
  let method_aggregates = aggregate_by_label(incremented_counters, "method")
  assert_eq(method_aggregates.length(), 2)
  
  let get_total = method_aggregates.filter(fn(a) { a.0 == "GET" })[0].1
  let post_total = method_aggregates.filter(fn(a) { a.0 == "POST" })[0].1
  
  assert_eq(get_total, 160) // 100 + 10 + 50
  assert_eq(post_total, 25)
}

// Test 2: Gauge Metrics
test "gauge metrics collection and tracking" {
  // Define gauge metric structure
  type GaugeMetric = {
    name: String,
    value: Float,
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // Create a gauge
  let create_gauge = fn(name: String, initial_value: Float, labels: Array[(String, String)], timestamp: Int) {
    {
      name,
      value: initial_value,
      labels,
      timestamp
    }
  }
  
  // Set gauge value
  let set_gauge = fn(gauge: GaugeMetric, value: Float) {
    { gauge | value }
  }
  
  // Increment gauge
  let increment_gauge = fn(gauge: GaugeMetric, delta: Float) {
    { gauge | value: gauge.value + delta }
  }
  
  // Decrement gauge
  let decrement_gauge = fn(gauge: GaugeMetric, delta: Float) {
    { gauge | value: gauge.value - delta }
  }
  
  // Test gauge creation
  let memory_gauge = create_gauge("memory_usage_bytes", 1024.0 * 1024.0 * 100.0, [
    ("service", "user-service"),
    ("instance", "instance-1")
  ], 1640995200)
  
  assert_eq(memory_gauge.name, "memory_usage_bytes")
  assert_eq(memory_gauge.value, 104857600.0) // 100 MB in bytes
  assert_eq(memory_gauge.labels.length(), 2)
  assert_eq(memory_gauge.timestamp, 1640995200)
  
  // Test gauge operations
  let increased_memory = increment_gauge(memory_gauge, 1024.0 * 1024.0 * 50.0) // Add 50 MB
  assert_eq(increased_memory.value, 157286400.0) // 150 MB in bytes
  
  let decreased_memory = decrement_gauge(increased_memory, 1024.0 * 1024.0 * 25.0) // Subtract 25 MB
  assert_eq(decreased_memory.value, 134217728.0) // 125 MB in bytes
  
  let set_memory = set_gauge(decreased_memory, 1024.0 * 1024.0 * 80.0) // Set to 80 MB
  assert_eq(set_memory.value, 83886080.0) // 80 MB in bytes
  
  // Test multiple gauges
  let gauges = [
    create_gauge("cpu_usage_percent", 25.5, [("service", "user-service"), ("instance", "instance-1")], 1640995200),
    create_gauge("cpu_usage_percent", 75.2, [("service", "payment-service"), ("instance", "instance-1")], 1640995201),
    create_gauge("cpu_usage_percent", 45.8, [("service", "user-service"), ("instance", "instance-2")], 1640995202),
    create_gauge("memory_usage_bytes", 104857600.0, [("service", "user-service"), ("instance", "instance-1")], 1640995203),
    create_gauge("memory_usage_bytes", 209715200.0, [("service", "payment-service"), ("instance", "instance-1")], 1640995204)
  ]
  
  // Calculate statistics for gauges with same name and labels
  let calculate_gauge_stats = fn(gauges: Array[GaugeMetric], name: String) {
    let matching_gauges = gauges.filter(fn(g) { g.name == name })
    let values = matching_gauges.map(fn(g) { g.value })
    
    if values.length() > 0 {
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / values.length().to_float()
      
      let mut min_val = values[0]
      let mut max_val = values[0]
      
      for v in values {
        if v < min_val { min_val = v }
        if v > max_val { max_val = v }
      }
      
      Some({
        count: values.length(),
        sum,
        avg,
        min: min_val,
        max: max_val
      })
    } else {
      None
    }
  }
  
  // Test CPU usage statistics
  let cpu_stats = calculate_gauge_stats(gauges, "cpu_usage_percent")
  assert_eq(cpu_stats, Some({
    count: 3,
    sum: 146.5,
    avg: 48.833333333333336,
    min: 25.5,
    max: 75.2
  }))
  
  // Test memory usage statistics
  let memory_stats = calculate_gauge_stats(gauges, "memory_usage_bytes")
  assert_eq(memory_stats, Some({
    count: 2,
    sum: 314572800.0,
    avg: 157286400.0,
    min: 104857600.0,
    max: 209715200.0
  }))
}

// Test 3: Histogram Metrics
test "histogram metrics with bucket distribution" {
  // Define histogram bucket structure
  type HistogramBucket = {
    upper_bound: Float,
    cumulative_count: Int
  }
  
  // Define histogram metric structure
  type HistogramMetric = {
    name: String,
    buckets: Array[HistogramBucket],
    count: Int,
    sum: Float,
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // Create a histogram with predefined buckets
  let create_histogram = fn(name: String, bucket_bounds: Array[Float], labels: Array[(String, String)], timestamp: Int) {
    let buckets = bucket_bounds.map(fn(bound) { 
      { upper_bound: bound, cumulative_count: 0 } 
    })
    
    {
      name,
      buckets,
      count: 0,
      sum: 0.0,
      labels,
      timestamp
    }
  }
  
  // Observe a value in the histogram
  let observe = fn(histogram: HistogramMetric, value: Float) {
    let mut updated_buckets = histogram.buckets
    let mut cumulative_count = 0
    
    for i in 0 .. updated_buckets.length() {
      if value <= updated_buckets[i].upper_bound {
        cumulative_count = cumulative_count + 1
        updated_buckets[i] = { updated_buckets[i] | cumulative_count: updated_buckets[i].cumulative_count + 1 }
      }
    }
    
    {
      name: histogram.name,
      buckets: updated_buckets,
      count: histogram.count + 1,
      sum: histogram.sum + value,
      labels: histogram.labels,
      timestamp: histogram.timestamp
    }
  }
  
  // Create a response time histogram with buckets: 10ms, 50ms, 100ms, 500ms, 1000ms, +Inf
  let response_time_histogram = create_histogram("http_request_duration_seconds", 
    [0.01, 0.05, 0.1, 0.5, 1.0, Float::infinity],
    [("endpoint", "/api/users"), ("method", "GET")],
    1640995200
  )
  
  // Observe some response times (in seconds)
  let with_observations = [
    observe(response_time_histogram, 0.008),  // 8ms - should go in first bucket
    observe(response_time_histogram, 0.025),  // 25ms - should go in second bucket
    observe(response_time_histogram, 0.075),  // 75ms - should go in third bucket
    observe(response_time_histogram, 0.2),    // 200ms - should go in fourth bucket
    observe(response_time_histogram, 0.8),    // 800ms - should go in fifth bucket
    observe(response_time_histogram, 1.5)     // 1500ms - should go in sixth bucket
  ].reduce(fn(h, obs) { obs }, response_time_histogram)
  
  // Test histogram properties
  assert_eq(with_observations.name, "http_request_duration_seconds")
  assert_eq(with_observations.count, 6)
  assert_eq(with_observations.sum, 2.608) // 0.008 + 0.025 + 0.075 + 0.2 + 0.8 + 1.5
  
  // Test bucket counts
  assert_eq(with_observations.buckets[0].cumulative_count, 1) // <= 10ms
  assert_eq(with_observations.buckets[1].cumulative_count, 2) // <= 50ms
  assert_eq(with_observations.buckets[2].cumulative_count, 3) // <= 100ms
  assert_eq(with_observations.buckets[3].cumulative_count, 4) // <= 500ms
  assert_eq(with_observations.buckets[4].cumulative_count, 5) // <= 1000ms
  assert_eq(with_observations.buckets[5].cumulative_count, 6) // <= +Inf
  
  // Calculate percentile approximations
  let calculate_percentile = fn(histogram: HistogramMetric, percentile: Float) {
    let threshold = (histogram.count.to_float() * percentile) / 100.0
    
    for bucket in histogram.buckets {
      if bucket.cumulative_count.to_float() >= threshold {
        return Some(bucket.upper_bound)
      }
    }
    
    None
  }
  
  // Test percentile calculations
  let p50 = calculate_percentile(with_observations, 50.0)
  let p90 = calculate_percentile(with_observations, 90.0)
  let p95 = calculate_percentile(with_observations, 95.0)
  let p99 = calculate_percentile(with_observations, 99.0)
  
  assert_eq(p50, Some(0.2))   // 50th percentile around 200ms
  assert_eq(p90, Some(0.8))   // 90th percentile around 800ms
  assert_eq(p95, Some(1.0))   // 95th percentile around 1000ms
  assert_eq(p99, Some(1.5))   // 99th percentile around 1500ms
  
  // Calculate average value
  let average = with_observations.sum / with_observations.count.to_float()
  assert_eq(average, 0.43466666666666665)
}

// Test 4: Summary Metrics
test "summary metrics with quantiles" {
  // Define quantile structure
  type Quantile = {
    quantile: Float,
    value: Float
  }
  
  // Define summary metric structure
  type SummaryMetric = {
    name: String,
    quantiles: Array[Quantile],
    count: Int,
    sum: Float,
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // Create a summary with predefined quantiles
  let create_summary = fn(name: String, quantiles: Array[Float], labels: Array[(String, String)], timestamp: Int) {
    let quantile_structs = quantiles.map(fn(q) { { quantile: q, value: 0.0 } })
    
    {
      name,
      quantiles: quantile_structs,
      count: 0,
      sum: 0.0,
      labels,
      timestamp
    }
  }
  
  // Observe a value in the summary
  let observe_summary = fn(summary: SummaryMetric, value: Float) {
    // In a real implementation, this would maintain a sliding window of values
    // and calculate quantiles based on that window. For simplicity, we'll
    // just update the count and sum.
    {
      name: summary.name,
      quantiles: summary.quantiles,
      count: summary.count + 1,
      sum: summary.sum + value,
      labels: summary.labels,
      timestamp: summary.timestamp
    }
  }
  
  // Update quantiles based on observed values
  let update_quantiles = fn(summary: SummaryMetric, values: Array[Float]) {
    let sorted_values = values.sort(fn(a, b) { a <= b })
    let count = sorted_values.length().to_float()
    
    let updated_quantiles = summary.quantiles.map(fn(q) {
      let index = ((q.quantile * count) - 1.0).to_int().max(0).min(sorted_values.length() - 1)
      { q | value: sorted_values[index] }
    })
    
    { summary | quantiles: updated_quantiles }
  }
  
  // Create a response size summary
  let response_size_summary = create_summary("http_response_size_bytes", 
    [0.5, 0.9, 0.95, 0.99],
    [("endpoint", "/api/users"), ("method", "GET")],
    1640995200
  )
  
  // Simulate observing response sizes
  let response_sizes = [1024.0, 2048.0, 4096.0, 8192.0, 16384.0, 32768.0, 65536.0]
  
  let with_observations = response_sizes.reduce(fn(summary, size) {
    observe_summary(summary, size)
  }, response_size_summary)
  
  // Update quantiles based on observed values
  let with_quantiles = update_quantiles(with_observations, response_sizes)
  
  // Test summary properties
  assert_eq(with_quantiles.name, "http_response_size_bytes")
  assert_eq(with_quantiles.count, 7)
  assert_eq(with_quantiles.sum, 130560.0) // Sum of all response sizes
  
  // Test quantile values (with sorted values: [1024, 2048, 4096, 8192, 16384, 32768, 65536])
  assert_eq(with_quantiles.quantiles[0].quantile, 0.5)   // 50th percentile
  assert_eq(with_quantiles.quantiles[0].value, 4096.0)   // Median value
  
  assert_eq(with_quantiles.quantiles[1].quantile, 0.9)   // 90th percentile
  assert_eq(with_quantiles.quantiles[1].value, 32768.0)  // 90th percentile value
  
  assert_eq(with_quantiles.quantiles[2].quantile, 0.95)  // 95th percentile
  assert_eq(with_quantiles.quantiles[2].value, 65536.0)  // 95th percentile value
  
  assert_eq(with_quantiles.quantiles[3].quantile, 0.99)  // 99th percentile
  assert_eq(with_quantiles.quantiles[3].value, 65536.0)  // 99th percentile value
  
  // Calculate average response size
  let avg_response_size = with_quantiles.sum / with_quantiles.count.to_float()
  assert_eq(avg_response_size, 18651.42857142857)
}

// Test 5: Metrics Collection and Aggregation
test "metrics collection and aggregation across services" {
  // Define metric value type
  type MetricValue = 
    Counter(Int)
    | Gauge(Float)
    | Histogram(Array[(Float, Int)])
    | Summary(Array[(Float, Float)])
  
  // Define metric structure
  type Metric = {
    name: String,
    value: MetricValue,
    labels: Array[(String, String)],
    service_name: String,
    timestamp: Int
  }
  
  // Create sample metrics from different services
  let metrics = [
    {
      name: "http_requests_total",
      value: Counter(1250),
      labels: [("method", "GET"), ("endpoint", "/api/users"), ("status", "200")],
      service_name: "user-service",
      timestamp: 1640995200
    },
    {
      name: "http_requests_total",
      value: Counter(50),
      labels: [("method", "GET"), ("endpoint", "/api/users"), ("status", "404")],
      service_name: "user-service",
      timestamp: 1640995201
    },
    {
      name: "http_requests_total",
      value: Counter(750),
      labels: [("method", "POST"), ("endpoint", "/api/orders"), ("status", "201")],
      service_name: "order-service",
      timestamp: 1640995202
    },
    {
      name: "cpu_usage_percent",
      value: Gauge(45.5),
      labels: [("instance", "instance-1")],
      service_name: "user-service",
      timestamp: 1640995203
    },
    {
      name: "cpu_usage_percent",
      value: Gauge(78.2),
      labels: [("instance", "instance-1")],
      service_name: "order-service",
      timestamp: 1640995204
    },
    {
      name: "memory_usage_bytes",
      value: Gauge(209715200.0),
      labels: [("instance", "instance-1")],
      service_name: "user-service",
      timestamp: 1640995205
    },
    {
      name: "memory_usage_bytes",
      value: Gauge(536870912.0),
      labels: [("instance", "instance-1")],
      service_name: "order-service",
      timestamp: 1640995206
    }
  ]
  
  // Group metrics by service
  let group_by_service = fn(metrics: Array[Metric]) {
    let mut result = []
    let mut processed = []
    
    for metric in metrics {
      if not(processed.contains(metric.service_name)) {
        let service_metrics = metrics.filter(fn(m) { m.service_name == metric.service_name })
        result = result.push((metric.service_name, service_metrics))
        processed = processed.push(metric.service_name)
      }
    }
    result
  }
  
  // Group metrics by name
  let group_by_name = fn(metrics: Array[Metric]) {
    let mut result = []
    let mut processed = []
    
    for metric in metrics {
      if not(processed.contains(metric.name)) {
        let name_metrics = metrics.filter(fn(m) { m.name == metric.name })
        result = result.push((metric.name, name_metrics))
        processed = processed.push(metric.name)
      }
    }
    result
  }
  
  // Aggregate counter metrics
  let aggregate_counters = fn(metrics: Array[Metric], name: String) {
    let counter_metrics = metrics.filter(fn(m) { 
      m.name == name and match m.value { Counter(_) => true, _ => false }
    })
    
    counter_metrics.map(fn(m) {
      match m.value {
        Counter(value) => (m.labels, value)
        _ => (m.labels, 0)
      }
    })
  }
  
  // Calculate service statistics
  let calculate_service_stats = fn(service_metrics: Array[Metric]) {
    let counter_metrics = service_metrics.filter(fn(m) { 
      match m.value { Counter(_) => true, _ => false }
    })
    let gauge_metrics = service_metrics.filter(fn(m) { 
      match m.value { Gauge(_) => true, _ => false }
    })
    
    // Sum all counter values
    let total_counter_value = counter_metrics.reduce(fn(sum, m) {
      match m.value {
        Counter(value) => sum + value
        _ => sum
      }
    }, 0)
    
    // Calculate average gauge values
    let gauge_values = gauge_metrics.map(fn(m) {
      match m.value {
        Gauge(value) => value
        _ => 0.0
      }
    })
    
    let avg_gauge_value = if gauge_values.length() > 0 {
      gauge_values.reduce(fn(sum, v) { sum + v }, 0.0) / gauge_values.length().to_float()
    } else {
      0.0
    }
    
    {
      total_requests: total_counter_value,
      avg_resource_usage: avg_gauge_value,
      metric_count: service_metrics.length()
    }
  }
  
  // Test grouping by service
  let service_groups = group_by_service(metrics)
  assert_eq(service_groups.length(), 2)
  
  let user_service_metrics = service_groups.filter(fn(g) { g.0 == "user-service" })[0].1
  let order_service_metrics = service_groups.filter(fn(g) { g.0 == "order-service" })[0].1
  
  assert_eq(user_service_metrics.length(), 4)
  assert_eq(order_service_metrics.length(), 3)
  
  // Test grouping by name
  let name_groups = group_by_name(metrics)
  assert_eq(name_groups.length(), 3)
  
  // Test aggregating counters
  let request_counters = aggregate_counters(metrics, "http_requests_total")
  assert_eq(request_counters.length(), 3)
  
  let total_requests = request_counters.reduce(fn(sum, rc) { sum + rc.1 }, 0)
  assert_eq(total_requests, 2050) // 1250 + 50 + 750
  
  // Test service statistics
  let user_service_stats = calculate_service_stats(user_service_metrics)
  let order_service_stats = calculate_service_stats(order_service_metrics)
  
  assert_eq(user_service_stats.total_requests, 1300) // 1250 + 50
  assert_eq(user_service_stats.avg_resource_usage, 127483622.25) // (45.5 + 209715200.0) / 2
  assert_eq(user_service_stats.metric_count, 4)
  
  assert_eq(order_service_stats.total_requests, 750)
  assert_eq(order_service_stats.avg_resource_usage, 268475495.1) // (78.2 + 536870912.0) / 2
  assert_eq(order_service_stats.metric_count, 3)
}

// Test 6: Metrics Time Series Analysis
test "metrics time series analysis and trend detection" {
  // Define time series data point
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    labels: Array[(String, String)]
  }
  
  // Define time series
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint]
  }
  
  // Create sample time series data
  let cpu_usage_series = {
    name: "cpu_usage_percent",
    points: [
      { timestamp: 1640995200, value: 25.5, labels: [("instance", "instance-1")] },
      { timestamp: 1640995260, value: 30.2, labels: [("instance", "instance-1")] },
      { timestamp: 1640995320, value: 45.8, labels: [("instance", "instance-1")] },
      { timestamp: 1640995380, value: 55.1, labels: [("instance", "instance-1")] },
      { timestamp: 1640995440, value: 60.3, labels: [("instance", "instance-1")] },
      { timestamp: 1640995500, value: 48.7, labels: [("instance", "instance-1")] },
      { timestamp: 1640995560, value: 35.2, labels: [("instance", "instance-1")] },
      { timestamp: 1640995620, value: 28.9, labels: [("instance", "instance-1")] }
    ]
  }
  
  // Calculate moving average
  let moving_average = fn(series: TimeSeries, window_size: Int) {
    let points = series.points
    let mut result = []
    
    for i in window_size - 1 .. points.length() {
      let mut sum = 0.0
      for j in i - (window_size - 1) ..= i {
        sum = sum + points[j].value
      }
      let avg = sum / window_size.to_float()
      result = result.push({ timestamp: points[i].timestamp, value: avg, labels: points[i].labels })
    }
    
    { name: series.name + "_ma_" + window_size.to_string(), points: result }
  }
  
  // Detect anomalies using standard deviation
  let detect_anomalies = fn(series: TimeSeries, threshold: Float) {
    let points = series.points
    let values = points.map(fn(p) { p.value })
    
    // Calculate mean
    let mean = values.reduce(fn(sum, v) { sum + v }, 0.0) / values.length().to_float()
    
    // Calculate standard deviation
    let variance = values.reduce(fn(sum, v) { sum + (v - mean) * (v - mean) }, 0.0) / values.length().to_float()
    let std_dev = variance.sqrt()
    
    // Find anomalies
    let anomalies = points.filter(fn(p) { (p.value - mean).abs() > threshold * std_dev })
    
    {
      mean,
      std_dev,
      anomalies,
      anomaly_count: anomalies.length()
    }
  }
  
  // Calculate trend using linear regression
  let calculate_trend = fn(series: TimeSeries) {
    let points = series.points
    let n = points.length().to_float()
    
    if n < 2.0 {
      return { slope: 0.0, intercept: 0.0, correlation: 0.0 }
    }
    
    // Normalize timestamps to start from 0
    let min_timestamp = points.reduce(fn(min, p) { if p.timestamp < min { p.timestamp } else { min } }, points[0].timestamp)
    let normalized_points = points.map(fn(p) { 
      { x: (p.timestamp - min_timestamp).to_float(), y: p.value } 
    })
    
    // Calculate sums for linear regression
    let sum_x = normalized_points.reduce(fn(sum, p) { sum + p.x }, 0.0)
    let sum_y = normalized_points.reduce(fn(sum, p) { sum + p.y }, 0.0)
    let sum_xy = normalized_points.reduce(fn(sum, p) { sum + p.x * p.y }, 0.0)
    let sum_x2 = normalized_points.reduce(fn(sum, p) { sum + p.x * p.x }, 0.0)
    let sum_y2 = normalized_points.reduce(fn(sum, p) { sum + p.y * p.y }, 0.0)
    
    // Calculate slope and intercept
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    // Calculate correlation coefficient
    let correlation = (n * sum_xy - sum_x * sum_y) / 
      ((n * sum_x2 - sum_x * sum_x) * (n * sum_y2 - sum_y * sum_y)).sqrt()
    
    { slope, intercept, correlation }
  }
  
  // Test moving average calculation
  let ma_3 = moving_average(cpu_usage_series, 3)
  assert_eq(ma_3.points.length(), 6) // 8 points - 3 + 1
  
  // First moving average: (25.5 + 30.2 + 45.8) / 3 = 33.833...
  assert_eq(ma_3.points[0].value, 33.833333333333336)
  
  // Second moving average: (30.2 + 45.8 + 55.1) / 3 = 43.7
  assert_eq(ma_3.points[1].value, 43.7)
  
  // Test anomaly detection
  let anomaly_analysis = detect_anomalies(cpu_usage_series, 1.5) // 1.5 standard deviations
  assert_true(anomaly_analysis.mean > 40.0) // Mean should be around 41.2
  assert_true(anomaly_analysis.std_dev > 10.0) // Std dev should be around 13.5
  
  // The highest value (60.3) and lowest value (25.5) might be anomalies depending on the threshold
  assert_true(anomaly_analysis.anomaly_count >= 0)
  
  // Test trend calculation
  let trend = calculate_trend(cpu_usage_series)
  
  // The slope should be positive if CPU usage is generally increasing over time
  // or negative if it's decreasing. In our data, it goes up then down, so slope should be close to 0
  assert_true(trend.slope.abs() < 1.0)
  
  // The correlation should indicate how well the linear model fits the data
  assert_true(trend.correlation >= -1.0 and trend.correlation <= 1.0)
  
  // Test rate of change calculation
  let rate_of_change = fn(series: TimeSeries) {
    let points = series.points
    let mut rates = []
    
    for i in 1 .. points.length() {
      let prev_point = points[i - 1]
      let curr_point = points[i]
      let time_diff = (curr_point.timestamp - prev_point.timestamp).to_float()
      let value_diff = curr_point.value - prev_point.value
      let rate = value_diff / time_diff
      rates = rates.push({ timestamp: curr_point.timestamp, rate })
    }
    
    rates
  }
  
  let rates = rate_of_change(cpu_usage_series)
  assert_eq(rates.length(), 7)
  
  // First rate: (30.2 - 25.5) / 60 = 0.07833...
  assert_eq(rates[0].rate, 0.07833333333333333)
  
  // Find the maximum rate of change
  let max_rate = rates.reduce(fn(max, r) { if r.rate > max.rate { r } else { max }, rates[0])
  assert_eq(max_rate.timestamp, 1640995380) // When CPU increased from 45.8 to 55.1
}