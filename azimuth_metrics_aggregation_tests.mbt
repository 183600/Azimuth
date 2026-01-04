// Azimuth Telemetry System - Metrics Aggregation Tests
// This file contains test cases for metrics aggregation and calculation functionality

// Test 1: Counter Metric Creation and Aggregation
test "counter metric creation and aggregation" {
  // Create a counter metric
  let counter_metric = CounterMetric::new("http_requests_total", [
    ("method", "GET"),
    ("status", "200"),
    ("endpoint", "/api/users")
  ])
  
  // Verify initial counter value
  assert_eq(CounterMetric::value(counter_metric), 0)
  assert_eq(CounterMetric::name(counter_metric), "http_requests_total")
  assert_eq(CounterMetric::labels(counter_metric).length(), 3)
  
  // Increment counter
  CounterMetric::inc(counter_metric)
  assert_eq(CounterMetric::value(counter_metric), 1)
  
  // Increment counter by specific amount
  CounterMetric::add(counter_metric, 5)
  assert_eq(CounterMetric::value(counter_metric), 6)
  
  // Create multiple counters with different label combinations
  let counters = [
    CounterMetric::new("http_requests_total", [("method", "GET"), ("status", "200")]),
    CounterMetric::new("http_requests_total", [("method", "GET"), ("status", "404")]),
    CounterMetric::new("http_requests_total", [("method", "POST"), ("status", "200")]),
    CounterMetric::new("http_requests_total", [("method", "POST"), ("status", "500")])
  ]
  
  // Increment counters with different values
  CounterMetric::add(counters[0], 100) // GET 200: 100
  CounterMetric::add(counters[1], 10)  // GET 404: 10
  CounterMetric::add(counters[2], 50)  // POST 200: 50
  CounterMetric::add(counters[3], 5)   // POST 500: 5
  
  // Test counter aggregation by method
  let get_method_aggregated = CounterMetric::aggregate_by_label(counters, "method", "GET")
  assert_eq(get_method_aggregated.value, 110) // 100 + 10
  
  let post_method_aggregated = CounterMetric::aggregate_by_label(counters, "method", "POST")
  assert_eq(post_method_aggregated.value, 55) // 50 + 5
  
  // Test counter aggregation by status
  let success_status_aggregated = CounterMetric::aggregate_by_label(counters, "status", "200")
  assert_eq(success_status_aggregated.value, 150) // 100 + 50
  
  let error_status_aggregated = CounterMetric::aggregate_by_label(counters, "status", "404")
  assert_eq(error_status_aggregated.value, 10)
  
  // Test counter rate calculation
  let time_window = 60000 // 1 minute in milliseconds
  let timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  let counter_with_timestamps = [
    CounterMetricWithTimestamp::new("http_requests_total", [("method", "GET")], 100, timestamp),
    CounterMetricWithTimestamp::new("http_requests_total", [("method", "GET")], 150, timestamp + 30000), // 30s later
    CounterMetricWithTimestamp::new("http_requests_total", [("method", "GET")], 200, timestamp + 60000)  // 60s later
  ]
  
  let rate = CounterMetric::calculate_rate(counter_with_timestamps, time_window)
  assert_eq(rate, 100.0) // (200 - 100) requests per minute
  
  // Test counter percentage calculation
  let total_requests = CounterMetric::aggregate_all(counters)
  let get_requests = CounterMetric::aggregate_by_label(counters, "method", "GET")
  let get_percentage = CounterMetric::calculate_percentage(get_requests, total_requests)
  
  assert_eq(get_percentage, 110.0 / 165.0 * 100.0) // 110 / 165 * 100%
  
  // Test counter growth rate
  let growth_rate = CounterMetric::calculate_growth_rate(counter_with_timestamps)
  assert_eq(growth_rate, 1.0) // (200 - 100) / 100 = 100% growth
}

// Test 2: Gauge Metric Creation and Aggregation
test "gauge metric creation and aggregation" {
  // Create a gauge metric
  let gauge_metric = GaugeMetric::new("memory_usage_bytes", [
    ("host", "server-01"),
    ("region", "us-west-2")
  ])
  
  // Verify initial gauge value
  assert_eq(GaugeMetric::value(gauge_metric), 0)
  assert_eq(GaugeMetric::name(gauge_metric), "memory_usage_bytes")
  assert_eq(GaugeMetric::labels(gauge_metric).length(), 2)
  
  // Set gauge value
  GaugeMetric::set(gauge_metric, 1024 * 1024 * 512) // 512MB
  assert_eq(GaugeMetric::value(gauge_metric), 1024 * 1024 * 512)
  
  // Increment gauge
  GaugeMetric::inc(gauge_metric)
  assert_eq(GaugeMetric::value(gauge_metric), 1024 * 1024 * 512 + 1)
  
  // Decrement gauge
  GaugeMetric::dec(gauge_metric)
  assert_eq(GaugeMetric::value(gauge_metric), 1024 * 1024 * 512)
  
  // Add to gauge
  GaugeMetric::add(gauge_metric, 1024 * 1024) // Add 1MB
  assert_eq(GaugeMetric::value(gauge_metric), 1024 * 1024 * 513)
  
  // Subtract from gauge
  GaugeMetric::sub(gauge_metric, 1024 * 1024) // Subtract 1MB
  assert_eq(GaugeMetric::value(gauge_metric), 1024 * 1024 * 512)
  
  // Create multiple gauges for different hosts
  let gauges = [
    GaugeMetric::new("memory_usage_bytes", [("host", "server-01")]),
    GaugeMetric::new("memory_usage_bytes", [("host", "server-02")]),
    GaugeMetric::new("memory_usage_bytes", [("host", "server-03")]),
    GaugeMetric::new("memory_usage_bytes", [("host", "server-04")])
  ]
  
  // Set different values for each host
  GaugeMetric::set(gauges[0], 1024 * 1024 * 512)  // 512MB
  GaugeMetric::set(gauges[1], 1024 * 1024 * 256)  // 256MB
  GaugeMetric::set(gauges[2], 1024 * 1024 * 1024) // 1GB
  GaugeMetric::set(gauges[3], 1024 * 1024 * 768)  // 768MB
  
  // Test gauge aggregation functions
  let avg_memory = GaugeMetric::aggregate_avg(gauges)
  assert_eq(avg_memory, (512 + 256 + 1024 + 768) * 1024 * 1024 / 4)
  
  let max_memory = GaugeMetric::aggregate_max(gauges)
  assert_eq(max_memory, 1024 * 1024 * 1024) // 1GB
  
  let min_memory = GaugeMetric::aggregate_min(gauges)
  assert_eq(min_memory, 1024 * 1024 * 256) // 256MB
  
  let sum_memory = GaugeMetric::aggregate_sum(gauges)
  assert_eq(sum_memory, (512 + 256 + 1024 + 768) * 1024 * 1024)
  
  // Test gauge percentile calculation
  let percentiles = GaugeMetric::calculate_percentiles(gauges, [50.0, 90.0, 95.0, 99.0])
  
  assert_eq(percentiles.get(50.0), Some(1024 * 1024 * 640)) // Median
  assert_eq(percentiles.get(90.0), Some(1024 * 1024 * 1024)) // 90th percentile
  assert_eq(percentiles.get(95.0), Some(1024 * 1024 * 1024)) // 95th percentile
  assert_eq(percentiles.get(99.0), Some(1024 * 1024 * 1024)) // 99th percentile
  
  // Test gauge rate of change
  let timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  let gauge_series = [
    GaugeMetricWithTimestamp::new("memory_usage_bytes", [("host", "server-01")], 1024 * 1024 * 512, timestamp),
    GaugeMetricWithTimestamp::new("memory_usage_bytes", [("host", "server-01")], 1024 * 1024 * 640, timestamp + 30000), // 30s later
    GaugeMetricWithTimestamp::new("memory_usage_bytes", [("host", "server-01")], 1024 * 1024 * 768, timestamp + 60000)  // 60s later
  ]
  
  let rate_of_change = GaugeMetric::calculate_rate_of_change(gauge_series)
  assert_eq(rate_of_change, (768 - 512) * 1024 * 1024 / 60.0) // 256MB per minute
  
  // Test gauge prediction
  let prediction = GaugeMetric::predict_next_value(gauge_series, 30000) // Predict 30s into future
  assert_eq(prediction, 1024 * 1024 * 896) // Linear extrapolation
}

// Test 3: Histogram Metric Creation and Aggregation
test "histogram metric creation and aggregation" {
  // Create a histogram metric with custom buckets
  let buckets = [10.0, 50.0, 100.0, 500.0, 1000.0]
  let histogram_metric = HistogramMetric::new("http_request_duration_seconds", buckets, [
    ("method", "GET"),
    ("endpoint", "/api/users")
  ])
  
  // Verify initial histogram state
  assert_eq(HistogramMetric::name(histogram_metric), "http_request_duration_seconds")
  assert_eq(HistogramMetric::buckets(histogram_metric), buckets)
  assert_eq(HistogramMetric::count(histogram_metric), 0)
  assert_eq(HistogramMetric::sum(histogram_metric), 0.0)
  
  // Observe values
  HistogramMetric::observe(histogram_metric, 5.0)   // Falls in bucket < 10
  HistogramMetric::observe(histogram_metric, 25.0)  // Falls in bucket < 50
  HistogramMetric::observe(histogram_metric, 75.0)  // Falls in bucket < 100
  HistogramMetric::observe(histogram_metric, 250.0) // Falls in bucket < 500
  HistogramMetric::observe(histogram_metric, 750.0) // Falls in bucket < 1000
  HistogramMetric::observe(histogram_metric, 1500.0) // Falls in bucket > 1000
  
  // Verify histogram state after observations
  assert_eq(HistogramMetric::count(histogram_metric), 6)
  assert_eq(HistogramMetric::sum(histogram_metric), 5.0 + 25.0 + 75.0 + 250.0 + 750.0 + 1500.0)
  
  // Verify bucket counts
  let bucket_counts = HistogramMetric::bucket_counts(histogram_metric)
  assert_eq(bucket_counts.get("< 10.0"), Some(1))
  assert_eq(bucket_counts.get("< 50.0"), Some(2))
  assert_eq(bucket_counts.get("< 100.0"), Some(3))
  assert_eq(bucket_counts.get("< 500.0"), Some(4))
  assert_eq(bucket_counts.get("< 1000.0"), Some(5))
  assert_eq(bucket_counts.get("+Inf"), Some(6))
  
  // Test histogram statistics
  let stats = HistogramMetric::calculate_statistics(histogram_metric)
  
  assert_eq(stats.count, 6)
  assert_eq(stats.sum, 5.0 + 25.0 + 75.0 + 250.0 + 750.0 + 1500.0)
  assert_eq(stats.min, 5.0)
  assert_eq(stats.max, 1500.0)
  assert_eq(stats.mean, stats.sum / stats.count.to_float())
  
  // Test histogram percentile estimation
  let p50 = HistogramMetric::estimate_percentile(histogram_metric, 50.0)
  let p95 = HistogramMetric::estimate_percentile(histogram_metric, 95.0)
  let p99 = HistogramMetric::estimate_percentile(histogram_metric, 99.0)
  
  assert_true(p50 >= 50.0 && p50 <= 100.0) // Should be between 50-100ms
  assert_true(p95 >= 500.0 && p95 <= 1000.0) // Should be between 500-1000ms
  assert_true(p99 >= 1000.0) // Should be > 1000ms
  
  // Create multiple histograms for aggregation
  let histograms = [
    HistogramMetric::new("http_request_duration_seconds", buckets, [("method", "GET")]),
    HistogramMetric::new("http_request_duration_seconds", buckets, [("method", "POST")]),
    HistogramMetric::new("http_request_duration_seconds", buckets, [("method", "PUT")])
  ]
  
  // Add observations to each histogram
  let get_observations = [10.0, 20.0, 30.0, 40.0, 50.0]
  let post_observations = [100.0, 200.0, 300.0, 400.0, 500.0]
  let put_observations = [50.0, 150.0, 250.0, 350.0, 450.0]
  
  for obs in get_observations {
    HistogramMetric::observe(histograms[0], obs)
  }
  
  for obs in post_observations {
    HistogramMetric::observe(histograms[1], obs)
  }
  
  for obs in put_observations {
    HistogramMetric::observe(histograms[2], obs)
  }
  
  // Test histogram aggregation
  let aggregated_histogram = HistogramMetric::aggregate(histograms)
  
  assert_eq(HistogramMetric::count(aggregated_histogram), 15) // 5 + 5 + 5
  assert_eq(HistogramMetric::sum(aggregated_histogram), 
    get_observations.reduce(fn(acc, x) { acc + x }, 0.0) +
    post_observations.reduce(fn(acc, x) { acc + x }, 0.0) +
    put_observations.reduce(fn(acc, x) { acc + x }, 0.0)
  )
  
  // Test histogram comparison
  let get_stats = HistogramMetric::calculate_statistics(histograms[0])
  let post_stats = HistogramMetric::calculate_statistics(histograms[1])
  
  let comparison = HistogramMetric::compare(histograms[0], histograms[1])
  
  assert_true(comparison.mean_difference < 0) // GET mean should be less than POST mean
  assert_true(comparison.p50_difference < 0) // GET p50 should be less than POST p50
  assert_true(comparison.p95_difference < 0) // GET p95 should be less than POST p95
  
  // Test histogram with exponential buckets
  let exponential_histogram = HistogramMetric::with_exponential_buckets(
    "response_size_bytes",
    1.0,    // Start at 1 byte
    2.0,    // Factor of 2
    10,     // 10 buckets
    [("endpoint", "/api/data")]
  )
  
  // Add observations across exponential range
  for i in 0..=9 {
    HistogramMetric::observe(exponential_histogram, 2.0.pow(i.to_float()))
  }
  
  assert_eq(HistogramMetric::count(exponential_histogram), 10)
  
  // Verify exponential bucket distribution
  let exp_bucket_counts = HistogramMetric::bucket_counts(exponential_histogram)
  for i in 0..=9 {
    let bucket_label = "< " + 2.0.pow((i + 1).to_float()).to_string()
    assert_eq(exp_bucket_counts.get(bucket_label), Some(i + 1))
  }
}

// Test 4: Summary Metric Creation and Aggregation
test "summary metric creation and aggregation" {
  // Create a summary metric with custom quantiles
  let quantiles = [0.5, 0.9, 0.95, 0.99]
  let summary_metric = SummaryMetric::new("rpc_duration_seconds", quantiles, [
    ("service", "user-service"),
    ("method", "GetUser")
  ])
  
  // Verify initial summary state
  assert_eq(SummaryMetric::name(summary_metric), "rpc_duration_seconds")
  assert_eq(SummaryMetric::quantiles(summary_metric), quantiles)
  assert_eq(SummaryMetric::count(summary_metric), 0)
  assert_eq(SummaryMetric::sum(summary_metric), 0.0)
  
  // Observe values
  let observations = [0.1, 0.2, 0.3, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0]
  
  for obs in observations {
    SummaryMetric::observe(summary_metric, obs)
  }
  
  // Verify summary state after observations
  assert_eq(SummaryMetric::count(summary_metric), 10)
  assert_eq(SummaryMetric::sum(summary_metric), observations.reduce(fn(acc, x) { acc + x }, 0.0))
  
  // Verify quantile values
  let quantile_values = SummaryMetric::quantile_values(summary_metric)
  assert_eq(quantile_values.get(0.5), Some(0.5))   // Median
  assert_eq(quantile_values.get(0.9), Some(10.0))  // 90th percentile
  assert_eq(quantile_values.get(0.95), Some(20.0)) // 95th percentile
  assert_eq(quantile_values.get(0.99), Some(50.0)) // 99th percentile
  
  // Test summary with age-based quantiles
  let age_summary = SummaryMetric::with_age_buckets(
    "database_query_duration_seconds",
    quantiles,
    5,      // 5 age buckets
    60000,  // 1 minute per bucket
    [("table", "users")]
  )
  
  // Add observations with different timestamps
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  for i in 0..=9 {
    let timestamp = base_timestamp + i * 10000 // 10 seconds apart
    SummaryMetric::observe_with_timestamp(age_summary, observations[i], timestamp)
  }
  
  // Test time-decayed summary
  let current_time = base_timestamp + 120000 // 2 minutes later
  let decayed_summary = SummaryMetric::get_time_decayed(age_summary, current_time)
  
  // Should have fewer observations due to age decay
  assert_true(SummaryMetric::count(decayed_summary) <= SummaryMetric::count(age_summary))
  
  // Create multiple summaries for aggregation
  let summaries = [
    SummaryMetric::new("rpc_duration_seconds", quantiles, [("service", "user-service")]),
    SummaryMetric::new("rpc_duration_seconds", quantiles, [("service", "order-service")]),
    SummaryMetric::new("rpc_duration_seconds", quantiles, [("service", "payment-service")])
  ]
  
  // Add observations to each summary
  let user_observations = [0.1, 0.2, 0.3, 0.5, 1.0]
  let order_observations = [0.5, 1.0, 2.0, 5.0, 10.0]
  let payment_observations = [1.0, 2.0, 5.0, 10.0, 20.0]
  
  for obs in user_observations {
    SummaryMetric::observe(summaries[0], obs)
  }
  
  for obs in order_observations {
    SummaryMetric::observe(summaries[1], obs)
  }
  
  for obs in payment_observations {
    SummaryMetric::observe(summaries[2], obs)
  }
  
  // Test summary aggregation
  let aggregated_summary = SummaryMetric::aggregate(summaries)
  
  assert_eq(SummaryMetric::count(aggregated_summary), 15) // 5 + 5 + 5
  assert_eq(SummaryMetric::sum(aggregated_summary), 
    user_observations.reduce(fn(acc, x) { acc + x }, 0.0) +
    order_observations.reduce(fn(acc, x) { acc + x }, 0.0) +
    payment_observations.reduce(fn(acc, x) { acc + x }, 0.0)
  )
  
  // Test summary statistics
  let user_stats = SummaryMetric::calculate_statistics(summaries[0])
  let order_stats = SummaryMetric::calculate_statistics(summaries[1])
  let payment_stats = SummaryMetric::calculate_statistics(summaries[2])
  
  assert_true(user_stats.mean < order_stats.mean)
  assert_true(order_stats.mean < payment_stats.mean)
  
  // Test summary comparison
  let comparison = SummaryMetric::compare(summaries[0], summaries[2])
  
  assert_true(comparison.mean_difference < 0) // User service mean should be less than payment service
  assert_true(comparison.p95_difference < 0) // User service p95 should be less than payment service
  assert_true(comparison.count_difference == 0) // Same number of observations
  
  // Test summary with sliding window
  let sliding_summary = SummaryMetric::with_sliding_window(
    "api_response_time_seconds",
    quantiles,
    10,     // Max 10 observations
    [("endpoint", "/api/users")]
  )
  
  // Add more observations than window size
  for i in 0..=15 {
    SummaryMetric::observe(sliding_summary, i.to_float())
  }
  
  // Should only keep the last 10 observations
  assert_eq(SummaryMetric::count(sliding_summary), 10)
  assert_eq(SummaryMetric::sum(sliding_summary), 6.0 + 7.0 + 8.0 + 9.0 + 10.0 + 11.0 + 12.0 + 13.0 + 14.0 + 15.0)
}

// Test 5: Multi-Dimensional Metrics Aggregation
test "multi-dimensional metrics aggregation" {
  // Create metrics with multiple dimensions
  let metrics = []
  
  // HTTP request metrics with method, status, and endpoint dimensions
  let endpoints = ["/api/users", "/api/orders", "/api/products"]
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let statuses = ["200", "400", "404", "500"]
  
  for endpoint in endpoints {
    for method in methods {
      for status in statuses {
        let counter = CounterMetric::new("http_requests_total", [
          ("endpoint", endpoint),
          ("method", method),
          ("status", status)
        ])
        
        // Set random values for testing
        let value = Random::int(0, 100)
        CounterMetric::add(counter, value)
        
        metrics = metrics.push(counter)
      }
    }
  }
  
  // Test aggregation by single dimension
  let get_requests = CounterMetric::aggregate_by_label(metrics, "method", "GET")
  let post_requests = CounterMetric::aggregate_by_label(metrics, "method", "POST")
  let put_requests = CounterMetric::aggregate_by_label(metrics, "method", "PUT")
  let delete_requests = CounterMetric::aggregate_by_label(metrics, "method", "DELETE")
  
  assert_eq(get_requests.labels, [("method", "GET")])
  assert_eq(post_requests.labels, [("method", "POST")])
  assert_eq(put_requests.labels, [("method", "PUT")])
  assert_eq(delete_requests.labels, [("method", "DELETE")])
  
  // Test aggregation by multiple dimensions
  let get_success = CounterMetric::aggregate_by_labels(metrics, [
    ("method", "GET"),
    ("status", "200")
  ])
  
  let post_errors = CounterMetric::aggregate_by_labels(metrics, [
    ("method", "POST"),
    ("status", "400")
  ])
  
  assert_eq(get_success.labels.length(), 2)
  assert_eq(post_errors.labels.length(), 2)
  
  // Test group by dimension
  let grouped_by_method = CounterMetric::group_by(metrics, "method")
  
  assert_eq(grouped_by_method.length(), 4) // GET, POST, PUT, DELETE
  assert_true(grouped_by_method.contains_key("GET"))
  assert_true(grouped_by_method.contains_key("POST"))
  assert_true(grouped_by_method.contains_key("PUT"))
  assert_true(grouped_by_method.contains_key("DELETE"))
  
  // Test group by multiple dimensions
  let grouped_by_method_and_status = CounterMetric::group_by_multiple(metrics, ["method", "status"])
  
  assert_eq(grouped_by_method_and_status.length(), 16) // 4 methods * 4 statuses
  
  // Test rollup aggregation
  let endpoint_rollup = CounterMetric::rollup(metrics, ["endpoint"])
  
  assert_eq(endpoint_rollup.length(), 3) // 3 endpoints
  
  for endpoint_metric in endpoint_rollup {
    let endpoint_label = endpoint_metric.labels.find(fn(l) { l.0 == "endpoint" })
    match endpoint_label {
      Some((_, value)) => {
        assert_true(endpoints.contains(value))
      }
      None => assert_true(false)
    }
  }
  
  // Test drill-down aggregation
  let method_drilldown = CounterMetric::drilldown(metrics, "method", "GET")
  
  for drilldown_metric in method_drilldown {
    let method_label = drilldown_metric.labels.find(fn(l) { l.0 == "method" })
    match method_label {
      Some((_, value)) => assert_eq(value, "GET")
      None => assert_true(false)
    }
  }
  
  // Test pivot table aggregation
  let pivot_table = CounterMetric::pivot(metrics, "method", "status")
  
  assert_eq(pivot_table.rows.length(), 4) // 4 methods
  assert_eq(pivot_table.columns.length(), 4) // 4 statuses
  
  // Test cross-tabulation
  let crosstab = CounterMetric::crosstab(metrics, "endpoint", "method")
  
  assert_eq(crosstab.rows.length(), 3) // 3 endpoints
  assert_eq(crosstab.columns.length(), 4) // 4 methods
  
  // Test multi-dimensional histogram
  let histogram_metrics = []
  
  for endpoint in endpoints {
    for method in methods {
      let histogram = HistogramMetric::new("request_duration_seconds", [10.0, 50.0, 100.0, 500.0], [
        ("endpoint", endpoint),
        ("method", method)
      ])
      
      // Add random observations
      for i in 0..=9 {
        let value = Random::float() * 1000.0 // 0-1000ms
        HistogramMetric::observe(histogram, value)
      }
      
      histogram_metrics = histogram_metrics.push(histogram)
    }
  }
  
  // Test histogram aggregation across dimensions
  let aggregated_histograms = HistogramMetric::aggregate_by_dimension(histogram_metrics, "endpoint")
  
  assert_eq(aggregated_histograms.length(), 3) // 3 endpoints
  
  for agg_hist in aggregated_histograms {
    let endpoint_label = agg_hist.labels.find(fn(l) { l.0 == "endpoint" })
    match endpoint_label {
      Some((_, value)) => {
        assert_true(endpoints.contains(value))
        assert_eq(HistogramMetric::count(agg_hist), 40) // 4 methods * 10 observations each
      }
      None => assert_true(false)
    }
  }
}

// Test 6: Real-Time Metrics Aggregation
test "real-time metrics aggregation" {
  // Create a real-time metrics aggregator
  let aggregator = RealTimeMetricsAggregator::new()
  
  // Define aggregation rules
  let counter_rule = AggregationRule::counter("http_requests_total", [
    "method", "status", "endpoint"
  ], [
    Aggregation::Sum,
    Aggregation::Rate(60000), // Per minute
    Aggregation::Percentage
  ])
  
  let gauge_rule = AggregationRule::gauge("memory_usage_bytes", [
    "host", "region"
  ], [
    Aggregation::Avg,
    Aggregation::Max,
    Aggregation::Min,
    Aggregation::Percentile([50.0, 95.0, 99.0])
  ])
  
  let histogram_rule = AggregationRule::histogram("request_duration_seconds", [
    "service", "operation"
  ], [
    Aggregation::Percentile([50.0, 90.0, 95.0, 99.0]),
    Aggregation::Rate(60000) // Per minute
  ])
  
  // Add rules to aggregator
  aggregator.add_rule(counter_rule)
  aggregator.add_rule(gauge_rule)
  aggregator.add_rule(histogram_rule)
  
  // Simulate real-time metric updates
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  for i in 0..=59 { // 1 minute of data
    let timestamp = base_timestamp + i * 1000 // 1-second intervals
    
    // Update counter metrics
    let counter = CounterMetric::new("http_requests_total", [
      ("method", if i % 2 == 0 { "GET" } else { "POST" }),
      ("status", if i % 10 == 0 { "500" } else { "200" }),
      ("endpoint", "/api/users")
    ])
    CounterMetric::add(counter, 1)
    aggregator.update(counter, timestamp)
    
    // Update gauge metrics
    let gauge = GaugeMetric::new("memory_usage_bytes", [
      ("host", "server-" + ((i % 3) + 1).to_string()),
      ("region", "us-west-2")
    ])
    GaugeMetric::set(gauge, 1024 * 1024 * (512 + (i % 256))) // 512-767MB
    aggregator.update(gauge, timestamp)
    
    // Update histogram metrics
    let histogram = HistogramMetric::new("request_duration_seconds", [10.0, 50.0, 100.0, 500.0], [
      ("service", "user-service"),
      ("operation", "GetUser")
    ])
    HistogramMetric::observe(histogram, Random::float() * 1000.0) // 0-1000ms
    aggregator.update(histogram, timestamp)
  }
  
  // Test real-time aggregation results
  let counter_results = aggregator.get_aggregated("http_requests_total", base_timestamp + 60000)
  
  assert_true(counter_results.contains_key("sum"))
  assert_true(counter_results.contains_key("rate"))
  assert_true(counter_results.contains_key("percentage"))
  
  let gauge_results = aggregator.get_aggregated("memory_usage_bytes", base_timestamp + 60000)
  
  assert_true(gauge_results.contains_key("avg"))
  assert_true(gauge_results.contains_key("max"))
  assert_true(gauge_results.contains_key("min"))
  assert_true(gauge_results.contains_key("p50"))
  assert_true(gauge_results.contains_key("p95"))
  assert_true(gauge_results.contains_key("p99"))
  
  let histogram_results = aggregator.get_aggregated("request_duration_seconds", base_timestamp + 60000)
  
  assert_true(histogram_results.contains_key("p50"))
  assert_true(histogram_results.contains_key("p90"))
  assert_true(histogram_results.contains_key("p95"))
  assert_true(histogram_results.contains_key("p99"))
  assert_true(histogram_results.contains_key("rate"))
  
  // Test sliding window aggregation
  let window_start = base_timestamp + 30000 // 30 seconds in
  let window_end = base_timestamp + 60000   // 60 seconds in
  
  let window_results = aggregator.get_aggregated_in_window(
    "http_requests_total",
    window_start,
    window_end
  )
  
  assert_true(window_results.contains_key("sum"))
  assert_true(window_results.contains_key("rate"))
  
  // Test time series aggregation
  let time_series = aggregator.get_time_series(
    "http_requests_total",
    "sum",
    base_timestamp,
    base_timestamp + 60000,
    10000 // 10-second intervals
  )
  
  assert_eq(time_series.length(), 6) // 60 seconds / 10 seconds = 6 intervals
  
  for point in time_series {
    assert_true(point.timestamp >= base_timestamp)
    assert_true(point.timestamp <= base_timestamp + 60000)
    assert_true(point.value >= 0)
  }
  
  // Test real-time alerting based on aggregated metrics
  let alert_rules = [
    AlertRule::new("high_error_rate", "http_requests_total.rate", "status=500", GreaterThan, 0.1),
    AlertRule::new("high_memory_usage", "memory_usage_bytes.avg", "*", GreaterThan, 1024 * 1024 * 700),
    AlertRule::new("slow_requests", "request_duration_seconds.p95", "*", GreaterThan, 500.0)
  ]
  
  for rule in alert_rules {
    aggregator.add_alert_rule(rule)
  }
  
  let alerts = aggregator.check_alerts(base_timestamp + 60000)
  
  // Should have alerts based on our test data
  assert_true(alerts.length() > 0)
  
  // Test metric retention and cleanup
  let old_timestamp = base_timestamp - 3600000 // 1 hour ago
  let old_counter = CounterMetric::new("http_requests_total", [
    ("method", "GET"),
    ("status", "200"),
    ("endpoint", "/api/old")
  ])
  CounterMetric::add(old_counter, 100)
  aggregator.update(old_counter, old_timestamp)
  
  // Cleanup metrics older than 30 minutes
  aggregator.cleanup(base_timestamp + 60000, 1800000) // 30 minutes
  
  // Old metrics should be cleaned up
  let old_metrics = aggregator.get_metrics_before(base_timestamp + 60000 - 1800000)
  assert_eq(old_metrics.length(), 0)
}

// Test 7: Distributed Metrics Aggregation
test "distributed metrics aggregation" {
  // Create distributed metrics from multiple nodes
  let nodes = ["node-01", "node-02", "node-03", "node-04"]
  let node_metrics = []
  
  for node in nodes {
    let metrics_for_node = []
    
    // Counter metrics
    let counter = CounterMetric::new("http_requests_total", [
      ("node", node),
      ("method", "GET"),
      ("status", "200")
    ])
    CounterMetric::add(counter, Random::int(100, 1000))
    metrics_for_node = metrics_for_node.push(counter)
    
    // Gauge metrics
    let gauge = GaugeMetric::new("memory_usage_bytes", [
      ("node", node),
      ("region", "us-west-2")
    ])
    GaugeMetric::set(gauge, Random::int(1024 * 1024 * 512, 1024 * 1024 * 1024))
    metrics_for_node = metrics_for_node.push(gauge)
    
    // Histogram metrics
    let histogram = HistogramMetric::new("request_duration_seconds", [10.0, 50.0, 100.0, 500.0], [
      ("node", node),
      ("service", "api")
    ])
    
    for i in 0..=99 {
      HistogramMetric::observe(histogram, Random::float() * 1000.0)
    }
    
    metrics_for_node = metrics_for_node.push(histogram)
    
    node_metrics = node_metrics.push((node, metrics_for_node))
  }
  
  // Create distributed aggregator
  let distributed_aggregator = DistributedMetricsAggregator::new()
  
  // Add node metrics to aggregator
  for (node, metrics) in node_metrics {
    distributed_aggregator.add_node_metrics(node, metrics)
  }
  
  // Test global aggregation
  let global_counters = distributed_aggregator.aggregate_globally("http_requests_total")
  
  assert_eq(global_counters.length(), 1)
  assert_eq(global_counters[0].labels.length(), 0) // No node label after global aggregation
  
  let expected_total = node_metrics.reduce(fn(acc, (_, metrics)) {
    acc + CounterMetric::value(metrics[0])
  }, 0)
  
  assert_eq(CounterMetric::value(global_counters[0]), expected_total)
  
  // Test per-node aggregation
  let per_node_memory = distributed_aggregator.aggregate_by_node("memory_usage_bytes")
  
  assert_eq(per_node_memory.length(), 4) // 4 nodes
  
  for (node, gauge) in per_node_memory {
    assert_true(nodes.contains(node))
    assert_eq(GaugeMetric::labels(gauge), [("node", node), ("region", "us-west-2")])
  }
  
  // Test hierarchical aggregation
  let hierarchy = distributed_aggregator.build_hierarchy([
    ("region", ["node-01", "node-02"]),
    ("region", ["node-03", "node-04"])
  ])
  
  let regional_metrics = distributed_aggregator.aggregate_by_hierarchy(hierarchy, "memory_usage_bytes")
  
  assert_eq(regional_metrics.length(), 2) // 2 regions
  
  // Test federated aggregation
  let federated_metrics = distributed_aggregator.federate_aggregation([
    ("node-01", ["http_requests_total", "memory_usage_bytes"]),
    ("node-02", ["http_requests_total", "memory_usage_bytes"]),
    ("node-03", ["http_requests_total"]),
    ("node-04", ["memory_usage_bytes"])
  ])
  
  assert_true(federated_metrics.contains_key("http_requests_total"))
  assert_true(federated_metrics.contains_key("memory_usage_bytes"))
  
  // Test distributed histogram aggregation
  let distributed_histograms = distributed_aggregator.aggregate_histograms("request_duration_seconds")
  
  assert_eq(distributed_histograms.length(), 1)
  
  let aggregated_histogram = distributed_histograms[0]
  assert_eq(HistogramMetric::labels(aggregated_histogram).length(), 1) // Only service label
  assert_eq(HistogramMetric::count(aggregated_histogram), 400) // 4 nodes * 100 observations each
  
  // Test percentile calculation across distributed data
  let percentiles = distributed_aggregator.calculate_percentiles("request_duration_seconds", [50.0, 95.0, 99.0])
  
  assert_true(percentiles.contains_key(50.0))
  assert_true(percentiles.contains_key(95.0))
  assert_true(percentiles.contains_key(99.0))
  
  // Test distributed anomaly detection
  let anomalies = distributed_aggregator.detect_anomalies("memory_usage_bytes", 2.0) // 2.0 standard deviations
  
  assert_true(anomalies.length() >= 0) // May or may not have anomalies
  
  // Test distributed metrics comparison
  let comparison = distributed_aggregator.compare_nodes("node-01", "node-02", [
    "http_requests_total",
    "memory_usage_bytes"
  ])
  
  assert_true(comparison.contains_key("http_requests_total"))
  assert_true(comparison.contains_key("memory_usage_bytes"))
  
  let http_comparison = comparison.get("http_requests_total")
  match http_comparison {
    Some(comp) => {
      assert_true(comp.node1_value >= 0)
      assert_true(comp.node2_value >= 0)
      assert_true(comp.ratio > 0)
    }
    None => assert_true(false)
  }
  
  // Test distributed metrics export
  let export_format = "prometheus"
  let exported_metrics = distributed_aggregator.export(export_format)
  
  assert_true(exported_metrics.length() > 0)
  assert_true(exported_metrics.contains("# TYPE http_requests_total counter"))
  assert_true(exported_metrics.contains("# TYPE memory_usage_bytes gauge"))
  assert_true(exported_metrics.contains("# TYPE request_duration_seconds histogram"))
}