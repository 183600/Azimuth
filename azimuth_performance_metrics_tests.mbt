// Azimuth Performance Metrics Collection Test Suite
// This file contains test cases for performance metrics collection functionality

// Test 1: Counter Metrics
test "counter metrics operations" {
  type Counter = {
    name: String,
    value: Int,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  let create_counter = fn(name: String) {
    {
      name,
      value: 0,
      timestamp: 1640995200,
      tags: []
    }
  }
  
  let increment = fn(counter: Counter) {
    { counter | value: counter.value + 1, timestamp: 1640995200 }
  }
  
  let increment_by = fn(counter: Counter, amount: Int) {
    { counter | value: counter.value + amount, timestamp: 1640995200 }
  }
  
  let add_tag = fn(counter: Counter, key: String, value: String) {
    { counter | tags: counter.tags + [(key, value)] }
  }
  
  let request_counter = create_counter("http_requests_total")
  assert_eq(request_counter.name, "http_requests_total")
  assert_eq(request_counter.value, 0)
  assert_eq(request_counter.tags.length(), 0)
  
  let incremented_counter = increment(request_counter)
  assert_eq(incremented_counter.value, 1)
  
  let incremented_by_counter = increment_by(incremented_counter, 5)
  assert_eq(incremented_by_counter.value, 6)
  
  let tagged_counter = add_tag(incremented_by_counter, "method", "GET")
  assert_eq(tagged_counter.tags.length(), 1)
  assert_true(tagged_counter.tags.contains(("method", "GET")))
  
  let multi_tagged_counter = add_tag(tagged_counter, "status", "200")
  assert_eq(multi_tagged_counter.tags.length(), 2)
  assert_true(multi_tagged_counter.tags.contains(("method", "GET")))
  assert_true(multi_tagged_counter.tags.contains(("status", "200")))
}

// Test 2: Gauge Metrics
test "gauge metrics operations" {
  type Gauge = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  let create_gauge = fn(name: String, initial_value: Float) {
    {
      name,
      value: initial_value,
      timestamp: 1640995200,
      tags: []
    }
  }
  
  let set_value = fn(gauge: Gauge, value: Float) {
    { gauge | value: value, timestamp: 1640995200 }
  }
  
  let adjust_value = fn(gauge: Gauge, delta: Float) {
    { gauge | value: gauge.value + delta, timestamp: 1640995200 }
  }
  
  let memory_gauge = create_gauge("memory_usage_bytes", 1024.0)
  assert_eq(memory_gauge.name, "memory_usage_bytes")
  assert_eq(memory_gauge.value, 1024.0)
  
  let updated_gauge = set_value(memory_gauge, 2048.0)
  assert_eq(updated_gauge.value, 2048.0)
  
  let adjusted_gauge = adjust_value(updated_gauge, -512.0)
  assert_eq(adjusted_gauge.value, 1536.0)
  
  let cpu_gauge = create_gauge("cpu_usage_percent", 75.5)
  assert_eq(cpu_gauge.value, 75.5)
  
  let increased_cpu = adjust_value(cpu_gauge, 10.0)
  assert_eq(increased_cpu.value, 85.5)
  
  let decreased_cpu = adjust_value(increased_cpu, -20.0)
  assert_eq(decreased_cpu.value, 65.5)
}

// Test 3: Histogram Metrics
test "histogram metrics operations" {
  type HistogramBucket = {
    upper_bound: Float,
    count: Int
  }
  
  type Histogram = {
    name: String,
    buckets: Array[HistogramBucket],
    count: Int,
    sum: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  let create_histogram = fn(name: String, bucket_bounds: Array[Float]) {
    let buckets = bucket_bounds.map(fn(bound) { 
      { upper_bound: bound, count: 0 } 
    })
    {
      name,
      buckets,
      count: 0,
      sum: 0.0,
      timestamp: 1640995200,
      tags: []
    }
  }
  
  let observe = fn(histogram: Histogram, value: Float) {
    let mut updated_buckets = []
    for bucket in histogram.buckets {
      if value <= bucket.upper_bound {
        updated_buckets = updated_buckets.push({ bucket | count: bucket.count + 1 })
      } else {
        updated_buckets = updated_buckets.push(bucket)
      }
    }
    
    {
      name: histogram.name,
      buckets: updated_buckets,
      count: histogram.count + 1,
      sum: histogram.sum + value,
      timestamp: histogram.timestamp,
      tags: histogram.tags
    }
  }
  
  let get_bucket_count = fn(histogram: Histogram, upper_bound: Float) {
    let mut found = 0
    for bucket in histogram.buckets {
      if bucket.upper_bound == upper_bound {
        found = bucket.count
      }
    }
    found
  }
  
  let response_time_histogram = create_histogram("http_request_duration_seconds", [0.1, 0.5, 1.0, 5.0, 10.0])
  assert_eq(response_time_histogram.buckets.length(), 5)
  assert_eq(response_time_histogram.count, 0)
  assert_eq(response_time_histogram.sum, 0.0)
  
  let histogram1 = observe(response_time_histogram, 0.05)
  assert_eq(histogram1.count, 1)
  assert_eq(histogram1.sum, 0.05)
  assert_eq(get_bucket_count(histogram1, 0.1), 1)
  assert_eq(get_bucket_count(histogram1, 0.5), 1)
  assert_eq(get_bucket_count(histogram1, 1.0), 1)
  assert_eq(get_bucket_count(histogram1, 5.0), 1)
  assert_eq(get_bucket_count(histogram1, 10.0), 1)
  
  let histogram2 = observe(histogram1, 0.3)
  assert_eq(histogram2.count, 2)
  assert_eq(histogram2.sum, 0.35)
  assert_eq(get_bucket_count(histogram2, 0.1), 1)
  assert_eq(get_bucket_count(histogram2, 0.5), 2)
  assert_eq(get_bucket_count(histogram2, 1.0), 2)
  assert_eq(get_bucket_count(histogram2, 5.0), 2)
  assert_eq(get_bucket_count(histogram2, 10.0), 2)
  
  let histogram3 = observe(histogram2, 2.5)
  assert_eq(histogram3.count, 3)
  assert_eq(histogram3.sum, 2.85)
  assert_eq(get_bucket_count(histogram3, 0.1), 1)
  assert_eq(get_bucket_count(histogram3, 0.5), 2)
  assert_eq(get_bucket_count(histogram3, 1.0), 2)
  assert_eq(get_bucket_count(histogram3, 5.0), 3)
  assert_eq(get_bucket_count(histogram3, 10.0), 3)
}

// Test 4: Summary Metrics
test "summary metrics operations" {
  type Quantile = {
    quantile: Float,
    value: Float
  }
  
  type Summary = {
    name: String,
    quantiles: Array[Quantile],
    count: Int,
    sum: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  let create_summary = fn(name: String, quantiles: Array[Float]) {
    {
      name,
      quantiles: quantiles.map(fn(q) { { quantile: q, value: 0.0 } }),
      count: 0,
      sum: 0.0,
      timestamp: 1640995200,
      tags: []
    }
  }
  
  let observe = fn(summary: Summary, value: Float) {
    // Simplified quantile calculation (in real implementation, would use proper algorithm)
    let update_quantiles = fn(values: Array[Float], quantiles: Array[Quantile]) {
      let sorted_values = values.sort(fn(a, b) { a <= b })
      quantiles.map(fn(q) {
        let index = ((q.quantile * (sorted_values.length() - 1).to_float()).to_int()).max(0).min(sorted_values.length() - 1)
        { q | value: sorted_values[index] }
      })
    }
    
    // This is a simplified implementation - in reality would store all values
    let all_values = [value]  // Simplified
    
    {
      name: summary.name,
      quantiles: update_quantiles(all_values, summary.quantiles),
      count: summary.count + 1,
      sum: summary.sum + value,
      timestamp: summary.timestamp,
      tags: summary.tags
    }
  }
  
  let get_quantile_value = fn(summary: Summary, quantile: Float) {
    let mut found = 0.0
    for q in summary.quantiles {
      if q.quantile == quantile {
        found = q.value
      }
    }
    found
  }
  
  let latency_summary = create_summary("request_latency_seconds", [0.5, 0.9, 0.95, 0.99])
  assert_eq(latency_summary.quantiles.length(), 4)
  assert_eq(latency_summary.count, 0)
  assert_eq(latency_summary.sum, 0.0)
  
  let summary1 = observe(latency_summary, 0.1)
  assert_eq(summary1.count, 1)
  assert_eq(summary1.sum, 0.1)
  
  let summary2 = observe(summary1, 0.2)
  assert_eq(summary2.count, 2)
  assert_eq(summary2.sum, 0.3)
  
  let summary3 = observe(summary2, 0.5)
  assert_eq(summary3.count, 3)
  assert_eq(summary3.sum, 0.8)
  
  // In this simplified implementation, quantiles would be calculated properly
  // in a real implementation with proper algorithm
}

// Test 5: Metric Aggregation
test "metric aggregation operations" {
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  let create_metric = fn(name: String, value: Float, tags: Array[(String, String)]) {
    {
      name,
      value,
      timestamp: 1640995200,
      tags
    }
  }
  
  let sum_by_tags = fn(metrics: Array[Metric], tag_keys: Array[String]) {
    let groups = {}
    
    for metric in metrics {
      let mut key = metric.name
      for tag_key in tag_keys {
        let mut found = ""
        for (k, v) in metric.tags {
          if k == tag_key {
            found = v
          }
        }
        key = key + ":" + found
      }
      
      match groups[key] {
        Some(sum) => groups[key] = Some(sum + metric.value)
        None => groups[key] = Some(metric.value)
      }
    }
    
    groups
  }
  
  let avg_by_tags = fn(metrics: Array[Metric], tag_keys: Array[String]) {
    let sums = sum_by_tags(metrics, tag_keys)
    let counts = {}
    
    for metric in metrics {
      let mut key = metric.name
      for tag_key in tag_keys {
        let mut found = ""
        for (k, v) in metric.tags {
          if k == tag_key {
            found = v
          }
        }
        key = key + ":" + found
      }
      
      match counts[key] {
        Some(count) => counts[key] = Some(count + 1)
        None => counts[key] = Some(1)
      }
    }
    
    let averages = {}
    for key in sums.keys() {
      match (sums[key], counts[key]) {
        (Some(sum), Some(count)) => averages[key] = Some(sum / count.to_float())
        _ => {}
      }
    }
    
    averages
  }
  
  let metrics = [
    create_metric("response_time", 100.0, [("service", "web"), ("method", "GET")]),
    create_metric("response_time", 150.0, [("service", "web"), ("method", "POST")]),
    create_metric("response_time", 200.0, [("service", "api"), ("method", "GET")]),
    create_metric("response_time", 120.0, [("service", "web"), ("method", "GET")]),
    create_metric("response_time", 180.0, [("service", "api"), ("method", "POST")])
  ]
  
  let sums = sum_by_tags(metrics, ["service"])
  assert_eq(sums["response_time:web"], Some(370.0))  // 100 + 150 + 120
  assert_eq(sums["response_time:api"], Some(380.0))  // 200 + 180
  
  let method_sums = sum_by_tags(metrics, ["method"])
  assert_eq(method_sums["response_time:GET"], Some(420.0))  // 100 + 200 + 120
  assert_eq(method_sums["response_time:POST"], Some(330.0))  // 150 + 180
  
  let averages = avg_by_tags(metrics, ["service"])
  assert_eq(averages["response_time:web"], Some(123.33333333333333))  // 370 / 3
  assert_eq(averages["response_time:api"], Some(190.0))  // 380 / 2
  
  let method_averages = avg_by_tags(metrics, ["method"])
  assert_eq(method_averages["response_time:GET"], Some(140.0))  // 420 / 3
  assert_eq(method_averages["response_time:POST"], Some(165.0))  // 330 / 2
}

// Test 6: Metric Registry
test "metric registry operations" {
  type MetricRegistry = {
    counters: Array[String],
    gauges: Array[String],
    histograms: Array[String],
    summaries: Array[String]
  }
  
  let create_registry = fn() {
    {
      counters: [],
      gauges: [],
      histograms: [],
      summaries: []
    }
  }
  
  let register_counter = fn(registry: MetricRegistry, name: String) {
    if not(registry.counters.contains(name)) {
      { registry | counters: registry.counters + [name] }
    } else {
      registry
    }
  }
  
  let register_gauge = fn(registry: MetricRegistry, name: String) {
    if not(registry.gauges.contains(name)) {
      { registry | gauges: registry.gauges + [name] }
    } else {
      registry
    }
  }
  
  let register_histogram = fn(registry: MetricRegistry, name: String) {
    if not(registry.histograms.contains(name)) {
      { registry | histograms: registry.histograms + [name] }
    } else {
      registry
    }
  }
  
  let is_counter_registered = fn(registry: MetricRegistry, name: String) {
    registry.counters.contains(name)
  }
  
  let is_gauge_registered = fn(registry: MetricRegistry, name: String) {
    registry.gauges.contains(name)
  }
  
  let is_histogram_registered = fn(registry: MetricRegistry, name: String) {
    registry.histograms.contains(name)
  }
  
  let registry = create_registry()
  assert_eq(registry.counters.length(), 0)
  assert_eq(registry.gauges.length(), 0)
  assert_eq(registry.histograms.length(), 0)
  
  let registry1 = register_counter(registry, "http_requests_total")
  assert_true(is_counter_registered(registry1, "http_requests_total"))
  assert_eq(registry1.counters.length(), 1)
  
  let registry2 = register_counter(registry1, "http_requests_total")
  assert_eq(registry2.counters.length(), 1)  // No duplicate
  
  let registry3 = register_counter(registry2, "error_count")
  assert_eq(registry3.counters.length(), 2)
  assert_true(is_counter_registered(registry3, "http_requests_total"))
  assert_true(is_counter_registered(registry3, "error_count"))
  
  let registry4 = register_gauge(registry3, "memory_usage")
  assert_true(is_gauge_registered(registry4, "memory_usage"))
  assert_eq(registry4.gauges.length(), 1)
  
  let registry5 = register_histogram(registry4, "request_duration")
  assert_true(is_histogram_registered(registry5, "request_duration"))
  assert_eq(registry5.histograms.length(), 1)
}

// Test 7: Metric Collection and Export
test "metric collection and export" {
  type MetricSnapshot = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)],
    metric_type: String
  }
  
  type MetricCollector = {
    snapshots: Array[MetricSnapshot]
  }
  
  let create_collector = fn() {
    { snapshots: [] }
  }
  
  let collect_counter = fn(collector: MetricCollector, name: String, value: Int, tags: Array[(String, String)]) {
    let snapshot = {
      name,
      value: value.to_float(),
      timestamp: 1640995200,
      tags,
      metric_type: "counter"
    }
    { collector | snapshots: collector.snapshots + [snapshot] }
  }
  
  let collect_gauge = fn(collector: MetricCollector, name: String, value: Float, tags: Array[(String, String)]) {
    let snapshot = {
      name,
      value,
      timestamp: 1640995200,
      tags,
      metric_type: "gauge"
    }
    { collector | snapshots: collector.snapshots + [snapshot] }
  }
  
  let export_to_prometheus_format = fn(snapshots: Array[MetricSnapshot]) {
    let mut lines = []
    
    for snapshot in snapshots {
      let mut tags_str = ""
      if snapshot.tags.length() > 0 {
        tags_str = "{" + snapshot.tags.map(fn(t) { t.0 + "=\"" + t.1 + "\"" }).join(",") + "}"
      }
      
      lines = lines +(snapshot.name + tags_str + " " + snapshot.value.to_string())
    }
    
    lines.join("\n")
  }
  
  let collector = create_collector()
  
  let collector1 = collect_counter(collector, "http_requests_total", 1000, [("method", "GET"), ("status", "200")])
  let collector2 = collect_gauge(collector1, "memory_usage_bytes", 1048576.0, [("instance", "server-1")])
  let collector3 = collect_counter(collector2, "http_requests_total", 500, [("method", "POST"), ("status", "200")])
  
  assert_eq(collector3.snapshots.length(), 3)
  
  let prometheus_export = export_to_prometheus_format(collector3.snapshots)
  assert_true(prometheus_export.contains("http_requests_total{method=\"GET\",status=\"200\"} 1000"))
  assert_true(prometheus_export.contains("memory_usage_bytes{instance=\"server-1\"} 1048576"))
  assert_true(prometheus_export.contains("http_requests_total{method=\"POST\",status=\"200\"} 500"))
}

// Test 8: Metric Rate Calculation
test "metric rate calculation" {
  type MetricPoint = {
    value: Float,
    timestamp: Int
  }
  
  type MetricRate = {
    rate: Float,
    time_window: Int
  }
  
  let calculate_rate = fn(points: Array[MetricPoint], time_window: Int) {
    if points.length() < 2 {
      { rate: 0.0, time_window }
    } else {
      let sorted_points = points.sort(fn(a, b) { a.timestamp <= b.timestamp })
      let oldest = sorted_points[0]
      let newest = sorted_points[sorted_points.length() - 1]
      
      let time_diff = newest.timestamp - oldest.timestamp
      if time_diff <= 0 {
        { rate: 0.0, time_window }
      } else {
        let value_diff = newest.value - oldest.value
        { rate: value_diff / time_diff.to_float(), time_window }
      }
    }
  }
  
  let calculate_rate_in_window = fn(points: Array[MetricPoint], window_start: Int, window_end: Int) {
    let window_points = points.filter(fn(p) { p.timestamp >= window_start and p.timestamp <= window_end })
    calculate_rate(window_points, window_end - window_start)
  }
  
  let metric_points = [
    { value: 100.0, timestamp: 1000 },
    { value: 150.0, timestamp: 1010 },
    { value: 200.0, timestamp: 1020 },
    { value: 300.0, timestamp: 1030 },
    { value: 350.0, timestamp: 1040 }
  ]
  
  let overall_rate = calculate_rate(metric_points, 40)
  assert_eq(overall_rate.rate, 6.25)  // (350 - 100) / 40
  assert_eq(overall_rate.time_window, 40)
  
  let window_rate = calculate_rate_in_window(metric_points, 1010, 1030)
  assert_eq(window_rate.rate, 7.5)  // (300 - 150) / 20
  assert_eq(window_rate.time_window, 20)
  
  let single_point = [{ value: 100.0, timestamp: 1000 }]
  let single_rate = calculate_rate(single_point, 10)
  assert_eq(single_rate.rate, 0.0)
  assert_eq(single_rate.time_window, 10)
  
  let empty_points: Array[MetricPoint] = []
  let empty_rate = calculate_rate(empty_points, 10)
  assert_eq(empty_rate.rate, 0.0)
  assert_eq(empty_rate.time_window, 10)
}