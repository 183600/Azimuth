// Azimuth Metrics Collection Test Suite
// This file contains test cases for metrics collection and aggregation

// Test 1: Counter Metrics
test "counter metrics increment and aggregation" {
  // Define counter structure
  type Counter = {
    name: String,
    value: Int,
    labels: Array[(String, String)]
  }
  
  // Create counter
  let request_counter = {
    name: "http_requests_total",
    value: 0,
    labels: [("method", "GET"), ("status", "200")]
  }
  
  // Increment counter function
  let increment_counter = fn(counter: Counter, amount: Int) {
    { counter | value: counter.value + amount }
  }
  
  // Test counter increment
  let counter1 = increment_counter(request_counter, 1)
  assert_eq(counter1.value, 1)
  
  let counter2 = increment_counter(counter1, 5)
  assert_eq(counter2.value, 6)
  
  // Test counter with different labels
  let error_counter = {
    name: "http_requests_total",
    value: 0,
    labels: [("method", "POST"), ("status", "500")]
  }
  
  let error_counter1 = increment_counter(error_counter, 1)
  assert_eq(error_counter1.value, 1)
  assert_eq(error_counter1.labels[0], ("method", "POST"))
  assert_eq(error_counter1.labels[1], ("status", "500"))
  
  // Test counter aggregation
  let aggregate_counters = fn(counters: Array[Counter]) {
    let mut result = []
    for counter in counters {
      let existing = result.find_index(fn(c) { 
        c.name == counter.name && c.labels == counter.labels 
      })
      
      match existing {
        Some(index) => {
          result[index] = { result[index] | value: result[index].value + counter.value }
        }
        None => {
          result = result.push(counter)
        }
      }
    }
    result
  }
  
  let counters = [counter1, counter2, error_counter1]
  let aggregated = aggregate_counters(counters)
  assert_eq(aggregated.length(), 2)
  
  let success_counter = aggregated.find(fn(c) { c.labels.contains(("status", "200")) })
  match success_counter {
    Some(counter) => assert_eq(counter.value, 7)  // 1 + 6
    None => assert_true(false)
  }
  
  let error_counter_result = aggregated.find(fn(c) { c.labels.contains(("status", "500")) })
  match error_counter_result {
    Some(counter) => assert_eq(counter.value, 1)
    None => assert_true(false)
  }
}

// Test 2: Gauge Metrics
test "gauge metrics with set and increment operations" {
  // Define gauge structure
  type Gauge = {
    name: String,
    value: Float,
    labels: Array[(String, String)]
  }
  
  // Create gauge
  let memory_gauge = {
    name: "memory_usage_bytes",
    value: 0.0,
    labels: [("instance", "server-1"), ("region", "us-west")]
  }
  
  // Set gauge value
  let set_gauge = fn(gauge: Gauge, new_value: Float) {
    { gauge | value: new_value }
  }
  
  // Increment gauge value
  let increment_gauge = fn(gauge: Gauge, amount: Float) {
    { gauge | value: gauge.value + amount }
  }
  
  // Decrement gauge value
  let decrement_gauge = fn(gauge: Gauge, amount: Float) {
    { gauge | value: gauge.value - amount }
  }
  
  // Test gauge operations
  let gauge1 = set_gauge(memory_gauge, 1024.0)
  assert_eq(gauge1.value, 1024.0)
  
  let gauge2 = increment_gauge(gauge1, 256.0)
  assert_eq(gauge2.value, 1280.0)
  
  let gauge3 = decrement_gauge(gauge2, 128.0)
  assert_eq(gauge3.value, 1152.0)
  
  // Test multiple gauges
  let cpu_gauge = {
    name: "cpu_usage_percent",
    value: 0.0,
    labels: [("instance", "server-1"), ("region", "us-west")]
  }
  
  let cpu_gauge1 = set_gauge(cpu_gauge, 75.5)
  assert_eq(cpu_gauge1.value, 75.5)
  
  // Test gauge aggregation (average)
  let average_gauges = fn(gauges: Array[Gauge]) {
    if gauges.length() == 0 {
      0.0
    } else {
      let sum = gauges.reduce(fn(acc, gauge) { acc + gauge.value }, 0.0)
      sum / (gauges.length() as Float)
    }
  }
  
  let gauges = [gauge3, cpu_gauge1]
  let average = average_gauges(gauges)
  assert_eq(average.round(), 613.8)  // (1152.0 + 75.5) / 2
  
  // Test gauge rate calculation
  let calculate_gauge_rate = fn(current: Gauge, previous: Gauge, time_interval: Float) {
    if time_interval > 0.0 {
      (current.value - previous.value) / time_interval
    } else {
      0.0
    }
  }
  
  let previous_memory = { memory_gauge | value: 800.0 }
  let memory_rate = calculate_gauge_rate(gauge3, previous_memory, 60.0)
  assert_eq(memory_rate.round(), 5.87)  // (1152.0 - 800.0) / 60
}

// Test 3: Histogram Metrics
test "histogram metrics with buckets and statistics" {
  // Define histogram bucket
  type HistogramBucket = {
    upper_bound: Float,
    cumulative_count: Int
  }
  
  // Define histogram
  type Histogram = {
    name: String,
    buckets: Array[HistogramBucket],
    count: Int,
    sum: Float,
    labels: Array[(String, String)]
  }
  
  // Create histogram with default buckets
  let create_histogram = fn(name: String, buckets: Array[Float], labels: Array[(String, String)]) {
    let histogram_buckets = buckets.map(fn(upper_bound) {
      { upper_bound, cumulative_count: 0 }
    })
    
    {
      name,
      buckets: histogram_buckets,
      count: 0,
      sum: 0.0,
      labels
    }
  }
  
  // Observe value in histogram
  let observe_histogram = fn(histogram: Histogram, value: Float) {
    let updated_buckets = histogram.buckets.map(fn(bucket) {
      if value <= bucket.upper_bound {
        { bucket | cumulative_count: bucket.cumulative_count + 1 }
      } else {
        bucket
      }
    })
    
    {
      name: histogram.name,
      buckets: updated_buckets,
      count: histogram.count + 1,
      sum: histogram.sum + value,
      labels: histogram.labels
    }
  }
  
  // Create request duration histogram
  let default_buckets = [0.1, 0.5, 1.0, 2.5, 5.0, 10.0]
  let request_histogram = create_histogram(
    "http_request_duration_seconds",
    default_buckets,
    [("method", "GET"), ("/api/users", "path")]
  )
  
  // Test histogram observation
  let histogram1 = observe_histogram(request_histogram, 0.05)  // <= 0.1
  let histogram2 = observe_histogram(histogram1, 0.3)         // <= 0.5
  let histogram3 = observe_histogram(histogram2, 1.2)        // <= 2.5
  let histogram4 = observe_histogram(histogram3, 0.8)        // <= 1.0
  let histogram5 = observe_histogram(histogram4, 3.5)        // <= 5.0
  
  assert_eq(histogram5.count, 5)
  assert_eq(histogram5.sum.round(), 5.85)  // 0.05 + 0.3 + 1.2 + 0.8 + 3.5
  
  // Test bucket counts
  let bucket_01 = histogram5.buckets.find(fn(b) { b.upper_bound == 0.1 })
  match bucket_01 {
    Some(bucket) => assert_eq(bucket.cumulative_count, 1)  // Only 0.05
    None => assert_true(false)
  }
  
  let bucket_05 = histogram5.buckets.find(fn(b) { b.upper_bound == 0.5 })
  match bucket_05 {
    Some(bucket) => assert_eq(bucket.cumulative_count, 2)  // 0.05, 0.3
    None => assert_true(false)
  }
  
  let bucket_10 = histogram5.buckets.find(fn(b) { b.upper_bound == 1.0 })
  match bucket_10 {
    Some(bucket) => assert_eq(bucket.cumulative_count, 4)  // 0.05, 0.3, 0.8, 1.2
    None => assert_true(false)
  }
  
  let bucket_50 = histogram5.buckets.find(fn(b) { b.upper_bound == 5.0 })
  match bucket_50 {
    Some(bucket) => assert_eq(bucket.cumulative_count, 5)  // All values
    None => assert_true(false)
  }
  
  // Calculate histogram statistics
  let calculate_histogram_stats = fn(histogram: Histogram) {
    if histogram.count == 0 {
      { mean: 0.0, rate: 0.0 }
    } else {
      let mean = histogram.sum / (histogram.count as Float)
      let rate = histogram.count as Float / 60.0  // Assuming 1 minute window
      { mean, rate }
    }
  }
  
  let stats = calculate_histogram_stats(histogram5)
  assert_eq(stats.mean.round(), 1.17)  // 5.85 / 5
  assert_eq(stats.rate.round(), 0.08)  // 5 / 60
  
  // Calculate percentiles from histogram
  let calculate_percentile = fn(histogram: Histogram, percentile: Float) {
    if histogram.count == 0 {
      0.0
    } else {
      let target_count = ((histogram.count as Float) * percentile / 100.0) as Int
      
      let find_bucket = fn(buckets: Array[HistogramBucket]) {
        let mut result = 0.0
        for bucket in buckets {
          if bucket.cumulative_count >= target_count {
            result = bucket.upper_bound
            break
          }
        }
        result
      }
      
      find_bucket(histogram.buckets)
    }
  }
  
  let p50 = calculate_percentile(histogram5, 50.0)
  let p95 = calculate_percentile(histogram5, 95.0)
  
  assert_eq(p50, 1.0)  // 50th percentile falls in 1.0 bucket
  assert_eq(p95, 5.0)  // 95th percentile falls in 5.0 bucket
}

// Test 4: Summary Metrics
test "summary metrics with quantiles and sliding windows" {
  // Define quantile
  type Quantile = {
    quantile: Float,
    value: Float
  }
  
  // Define summary
  type Summary = {
    name: String,
    quantiles: Array[Quantile],
    count: Int,
    sum: Float,
    labels: Array[(String, String)]
  }
  
  // Create summary
  let create_summary = fn(name: String, quantiles: Array[Float], labels: Array[(String, String)]) {
    let summary_quantiles = quantiles.map(fn(q) {
      { quantile: q, value: 0.0 }
    })
    
    {
      name,
      quantiles: summary_quantiles,
      count: 0,
      sum: 0.0,
      labels
    }
  }
  
  // Observe value in summary
  let observe_summary = fn(summary: Summary, value: Float) {
    // In a real implementation, this would use a sliding window and reservoir sampling
    // For simplicity, we'll just update count and sum
    {
      name: summary.name,
      quantiles: summary.quantiles,
      count: summary.count + 1,
      sum: summary.sum + value,
      labels: summary.labels
    }
  }
  
  // Update quantiles (simplified implementation)
  let update_quantiles = fn(summary: Summary, values: Array[Float]) {
    if values.length() == 0 {
      summary
    } else {
      let sorted_values = values.sort(fn(a, b) {
        if a < b { -1 } else if a > b { 1 } else { 0 }
      })
      
      let updated_quantiles = summary.quantiles.map(fn(quantile) {
        let index = ((sorted_values.length() as Float) * quantile.quantile) as Int
        let clamped_index = if index >= sorted_values.length() {
          sorted_values.length() - 1
        } else {
          index
        }
        
        { quantile | value: sorted_values[clamped_index] }
      })
      
      { summary | quantiles: updated_quantiles }
    }
  }
  
  // Create response size summary
  let quantiles = [0.5, 0.9, 0.95, 0.99]
  let response_summary = create_summary(
    "http_response_size_bytes",
    quantiles,
    [("method", "GET"), ("/api/data", "path")]
  )
  
  // Simulate observing values
  let observed_values = [1024.0, 2048.0, 512.0, 4096.0, 1536.0, 3072.0, 2560.0]
  let final_summary = observed_values.reduce(fn(acc, value) {
    observe_summary(acc, value)
  }, response_summary)
  
  assert_eq(final_summary.count, 7)
  assert_eq(final_summary.sum, 14802.0)
  
  // Update quantiles based on observed values
  let updated_summary = update_quantiles(final_summary, observed_values)
  
  // Test quantile values
  let p50 = updated_summary.quantiles.find(fn(q) { q.quantile == 0.5 })
  match p50 {
    Some(quantile) => assert_eq(quantile.value, 2048.0)  // Median of sorted values
    None => assert_true(false)
  }
  
  let p90 = updated_summary.quantiles.find(fn(q) { q.quantile == 0.9 })
  match p90 {
    Some(quantile) => assert_eq(quantile.value, 4096.0)  // 90th percentile
    None => assert_true(false)
  }
  
  let p95 = updated_summary.quantiles.find(fn(q) { q.quantile == 0.95 })
  match p95 {
    Some(quantile) => assert_eq(quantile.value, 4096.0)  // 95th percentile
    None => assert_true(false)
  }
  
  let p99 = updated_summary.quantiles.find(fn(q) { q.quantile == 0.99 })
  match p99 {
    Some(quantile) => assert_eq(quantile.value, 4096.0)  // 99th percentile
    None => assert_true(false)
  }
  
  // Test summary statistics
  let calculate_summary_stats = fn(summary: Summary) {
    if summary.count == 0 {
      { mean: 0.0, rate: 0.0 }
    } else {
      let mean = summary.sum / (summary.count as Float)
      let rate = summary.count as Float / 300.0  // Assuming 5 minute window
      { mean, rate }
    }
  }
  
  let stats = calculate_summary_stats(updated_summary)
  assert_eq(stats.mean.round(), 2114.57)  // 14802.0 / 7
  assert_eq(stats.rate.round(), 0.02)     // 7 / 300
}

// Test 5: Metric Registry and Collection
test "metric registry and collection operations" {
  // Define metric types
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // Define metric entry
  type MetricEntry = {
    name: String,
    metric_type: MetricType,
    labels: Array[(String, String)],
    data: String  // Serialized metric data
  }
  
  // Define metric registry
  type MetricRegistry = {
    metrics: Array[MetricEntry]
  }
  
  // Create registry
  let create_registry = fn() {
    { metrics: [] }
  }
  
  // Register metric
  let register_metric = fn(registry: MetricRegistry, entry: MetricEntry) {
    let existing = registry.metrics.find_index(fn(m) { 
      m.name == entry.name && m.labels == entry.labels 
    })
    
    match existing {
      Some(index) => {
        let updated_metrics = registry.metrics.slice(0, index) + 
                           [entry] + 
                           registry.metrics.slice(index + 1, registry.metrics.length())
        { registry | metrics: updated_metrics }
      }
      None => {
        { registry | metrics: registry.metrics.push(entry) }
      }
    }
  }
  
  // Get metric by name and labels
  let get_metric = fn(registry: MetricRegistry, name: String, labels: Array[(String, String)]) {
    registry.metrics.find(fn(m) { m.name == name && m.labels == labels })
  }
  
  // Get all metrics by name
  let get_metrics_by_name = fn(registry: MetricRegistry, name: String) {
    registry.metrics.filter(fn(m) { m.name == name })
  }
  
  // Create registry and register metrics
  let registry = create_registry()
  
  let counter_entry = {
    name: "requests_total",
    metric_type: MetricType::Counter,
    labels: [("method", "GET")],
    data: "42"
  }
  
  let gauge_entry = {
    name: "memory_usage",
    metric_type: MetricType::Gauge,
    labels: [("instance", "server-1")],
    data: "1024.5"
  }
  
  let registry1 = register_metric(registry, counter_entry)
  let registry2 = register_metric(registry1, gauge_entry)
  
  assert_eq(registry2.metrics.length(), 2)
  
  // Test metric retrieval
  let retrieved_counter = get_metric(registry2, "requests_total", [("method", "GET")])
  match retrieved_counter {
    Some(metric) => {
      assert_eq(metric.name, "requests_total")
      assert_eq(metric.data, "42")
    }
    None => assert_true(false)
  }
  
  let non_existent = get_metric(registry2, "non_existent", [])
  assert_eq(non_existent, None)
  
  // Test filtering by name
  let request_metrics = get_metrics_by_name(registry2, "requests_total")
  assert_eq(request_metrics.length(), 1)
  
  // Register another metric with same name but different labels
  let another_counter = {
    name: "requests_total",
    metric_type: MetricType::Counter,
    labels: [("method", "POST")],
    data: "15"
  }
  
  let registry3 = register_metric(registry2, another_counter)
  
  let all_request_metrics = get_metrics_by_name(registry3, "requests_total")
  assert_eq(all_request_metrics.length(), 2)
  
  // Test metric collection and serialization
  let collect_metrics = fn(registry: MetricRegistry, metric_type: MetricType) {
    registry.metrics.filter(fn(m) { m.metric_type == metric_type })
  }
  
  let counters = collect_metrics(registry3, MetricType::Counter)
  assert_eq(counters.length(), 2)
  
  let gauges = collect_metrics(registry3, MetricType::Gauge)
  assert_eq(gauges.length(), 1)
  
  let histograms = collect_metrics(registry3, MetricType::Histogram)
  assert_eq(histograms.length(), 0)
}

// Test 6: Metric Labels and Dimensionality
test "metric labels and dimensionality management" {
  // Define label constraint
  type LabelConstraint = {
    name: String,
    allowed_values: Array[String],
    required: Bool
  }
  
  // Define metric specification
  type MetricSpec = {
    name: String,
    metric_type: String,
    label_constraints: Array[LabelConstraint]
  }
  
  // Validate labels against constraints
  let validate_labels = fn(labels: Array[(String, String)], constraints: Array[LabelConstraint]) {
    let mut errors = []
    
    // Check required labels
    for constraint in constraints {
      if constraint.required {
        let has_label = labels.any(fn(label) { label.0 == constraint.name })
        if not(has_label) {
          errors = errors.push("Required label missing: " + constraint.name)
        }
      }
    }
    
    // Check allowed values
    for label in labels {
      let constraint = constraints.find(fn(c) { c.name == label.0 })
      match constraint {
        Some(c) => {
          if c.allowed_values.length() > 0 && not(c.allowed_values.contains(label.1)) {
            errors = errors.push("Invalid value for label " + c.name + ": " + label.1)
          }
        }
        None => {}
      }
    }
    
    errors
  }
  
  // Create metric specification
  let request_metric_spec = {
    name: "http_requests_total",
    metric_type: "counter",
    label_constraints: [
      { name: "method", allowed_values: ["GET", "POST", "PUT", "DELETE"], required: true },
      { name: "status", allowed_values: ["200", "400", "404", "500"], required: true },
      { name: "service", allowed_values: [], required: false }  // Empty allowed_values means any value
    ]
  }
  
  // Test valid labels
  let valid_labels = [
    ("method", "GET"),
    ("status", "200"),
    ("service", "api-gateway")
  ]
  
  let valid_errors = validate_labels(valid_labels, request_metric_spec.label_constraints)
  assert_eq(valid_errors.length(), 0)
  
  // Test missing required label
  let missing_required = [
    ("method", "GET"),
    ("service", "api-gateway")
  ]
  
  let missing_errors = validate_labels(missing_required, request_metric_spec.label_constraints)
  assert_eq(missing_errors.length(), 1)
  assert_true(missing_errors[0].contains("Required label missing: status"))
  
  // Test invalid value
  let invalid_value = [
    ("method", "PATCH"),  // Not in allowed values
    ("status", "200"),
    ("service", "api-gateway")
  ]
  
  let value_errors = validate_labels(invalid_value, request_metric_spec.label_constraints)
  assert_eq(value_errors.length(), 1)
  assert_true(value_errors[0].contains("Invalid value for label method"))
  
  // Test label cardinality calculation
  let calculate_cardinality = fn(spec: MetricSpec, existing_label_sets: Array<Array[(String, String)]>) {
    // Calculate theoretical maximum cardinality
    let max_cardinality = spec.label_constraints.reduce(fn(acc, constraint) {
      if constraint.allowed_values.length() > 0 {
        acc * constraint.allowed_values.length()
      } else {
        acc * 10  // Assume up to 10 different values for unconstrained labels
      }
    }, 1)
    
    // Calculate current cardinality
    let current_cardinality = existing_label_sets.length()
    
    {
      max_cardinality,
      current_cardinality,
      utilization_percent: if max_cardinality > 0 {
        (current_cardinality as Float / max_cardinality as Float) * 100.0
      } else {
        0.0
      }
    }
  }
  
  let existing_labels = [
    [("method", "GET"), ("status", "200"), ("service", "api")],
    [("method", "GET"), ("status", "400"), ("service", "api")],
    [("method", "POST"), ("status", "200"), ("service", "web")]
  ]
  
  let cardinality = calculate_cardinality(request_metric_spec, existing_labels)
  assert_eq(cardinality.max_cardinality, 16)  // 4 methods * 4 statuses * 1 (service is optional)
  assert_eq(cardinality.current_cardinality, 3)
  assert_eq(cardinality.utilization_percent.round(), 18.75)  // 3 / 16 * 100
  
  // Test label normalization
  let normalize_labels = fn(labels: Array[(String, String)]) {
    // Convert label names to lowercase and remove spaces
    labels.map(fn(label) {
      let normalized_name = label.0.to_lowercase().replace(" ", "_")
      let normalized_value = label.1.to_lowercase().replace(" ", "_")
      (normalized_name, normalized_value)
    })
  }
  
  let unnormalized_labels = [
    ("HTTP Method", "GET"),
    ("Status Code", "200"),
    ("Service Name", "API Gateway")
  ]
  
  let normalized = normalize_labels(unnormalized_labels)
  assert_eq(normalized[0], ("http_method", "get"))
  assert_eq(normalized[1], ("status_code", "200"))
  assert_eq(normalized[2], ("service_name", "api_gateway"))
}

// Test 7: Metric Export and Formatting
test "metric export and formatting operations" {
  // Define metric export format
  enum ExportFormat {
    Prometheus
    JSON
    InfluxDB
    OpenTelemetry
  }
  
  // Export metric to Prometheus format
  let export_prometheus = fn(name: String, value: String, labels: Array[(String, String)], metric_type: String) {
    let label_str = if labels.length() > 0 {
      "{" + labels.map(fn(l) { l.0 + "=\"" + l.1 + "\"" }).join(",") + "}"
    } else {
      ""
    }
    
    name + label_str + " " + value + "\n" +
    "# TYPE " + name + " " + metric_type + "\n"
  }
  
  // Export metric to JSON format
  let export_json = fn(name: String, value: String, labels: Array[(String, String)], metric_type: String) {
    let labels_obj = labels.map(fn(l) { "\"" + l.0 + "\": \"" + l.1 + "\"" }).join(",")
    
    "{\n" +
    "  \"name\": \"" + name + "\",\n" +
    "  \"type\": \"" + metric_type + "\",\n" +
    "  \"value\": " + value + ",\n" +
    "  \"labels\": {" + labels_obj + "}\n" +
    "}\n"
  }
  
  // Export metric to InfluxDB format
  let export_influxdb = fn(name: String, value: String, labels: Array[(String, String)], timestamp: Int) {
    let tags = labels.map(fn(l) { l.0 + "=" + l.1 }).join(",")
    let measurement = if tags.length() > 0 {
      name + "," + tags
    } else {
      name
    }
    
    measurement + " value=" + value + " " + timestamp.to_string()
  }
  
  // Create test metric
  let metric_name = "http_request_duration_seconds"
  let metric_value = "0.245"
  let metric_labels = [
    ("method", "GET"),
    ("status", "200"),
    ("service", "api-gateway")
  ]
  let metric_type = "histogram"
  let timestamp = 1640995200
  
  // Test Prometheus export
  let prometheus_export = export_prometheus(metric_name, metric_value, metric_labels, metric_type)
  assert_true(prometheus_export.contains(metric_name))
  assert_true(prometheus_export.contains(metric_value))
  assert_true(prometheus_export.contains("method=\"GET\""))
  assert_true(prometheus_export.contains("# TYPE"))
  
  // Test JSON export
  let json_export = export_json(metric_name, metric_value, metric_labels, metric_type)
  assert_true(json_export.contains("\"name\": \"" + metric_name + "\""))
  assert_true(json_export.contains("\"type\": \"" + metric_type + "\""))
  assert_true(json_export.contains("\"value\": " + metric_value))
  assert_true(json_export.contains("\"method\": \"GET\""))
  
  // Test InfluxDB export
  let influxdb_export = export_influxdb(metric_name, metric_value, metric_labels, timestamp)
  assert_true(influxdb_export.contains(metric_name))
  assert_true(influxdb_export.contains("method=GET"))
  assert_true(influxdb_export.contains("value=" + metric_value))
  assert_true(influxdb_export.contains(timestamp.to_string()))
  
  // Test batch export
  let batch_export = fn(metrics: Array[(String, String, Array[(String, String)], String)], format: ExportFormat) {
    match format {
      ExportFormat::Prometheus => {
        metrics.map(fn(m) {
          export_prometheus(m.0, m.1, m.2, m.3)
        }).join("")
      }
      ExportFormat::JSON => {
        "[" + metrics.map(fn(m) {
          export_json(m.0, m.1, m.2, m.3)
        }).join(",") + "]"
      }
      ExportFormat::InfluxDB => {
        metrics.map(fn(m) {
          export_influxdb(m.0, m.1, m.2, timestamp)
        }).join("\n")
      }
      ExportFormat::OpenTelemetry => {
        // Simplified OTLP export
        metrics.map(fn(m) {
          "OTLP: " + m.0 + "=" + m.1
        }).join("\n")
      }
    }
  }
  
  let metrics_batch = [
    (metric_name, metric_value, metric_labels, metric_type),
    ("http_requests_total", "1024", [("method", "GET")], "counter"),
    ("memory_usage_bytes", "1073741824", [("instance", "server-1")], "gauge")
  ]
  
  let prometheus_batch = batch_export(metrics_batch, ExportFormat::Prometheus)
  assert_true(prometheus_batch.contains(metric_name))
  assert_true(prometheus_batch.contains("http_requests_total"))
  assert_true(prometheus_batch.contains("memory_usage_bytes"))
  
  let json_batch = batch_export(metrics_batch, ExportFormat::JSON)
  assert_true(json_batch.startsWith("["))
  assert_true(json_batch.endsWith("]"))
  
  let influxdb_batch = batch_export(metrics_batch, ExportFormat::InfluxDB)
  assert_true(influxdb_batch.contains("value=0.245"))
  assert_true(influxdb_batch.contains("value=1024"))
  assert_true(influxdb_batch.contains("value=1073741824"))
}