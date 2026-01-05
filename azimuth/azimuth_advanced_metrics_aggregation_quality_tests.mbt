// Azimuth Advanced Metrics Aggregation Quality Tests
// Tests for complex metrics aggregation scenarios

test "multi-dimensional metrics aggregation" {
  // Arrange - Create meter provider
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "multi-dimensional-test")
  
  // Act - Create multi-dimensional metrics
  let counter = Meter::create_counter(meter, "multi_dim_counter", "Multi-dimensional counter", "operations")
  let histogram = Meter::create_histogram(meter, "multi_dim_histogram", "Multi-dimensional histogram", "ms")
  
  // Generate multi-dimensional data
  let services = ["auth", "payment", "inventory", "notification", "analytics"]
  let regions = ["us-east", "us-west", "eu-west", "ap-southeast"]
  let versions = ["v1.0", "v1.1", "v2.0"]
  let statuses = ["success", "error", "timeout"]
  
  // Create multi-dimensional counter data
  for service in services {
    for region in regions {
      for version in versions {
        for status in statuses {
          let attributes = Attributes::from([
            ("service", service),
            ("region", region),
            ("version", version),
            ("status", status)
          ])
          
          // Add random number of operations
          let operations = Random::next_int() % 1000 + 100
          Counter::add(counter, operations, attributes)
          
          // Add corresponding histogram data
          for i = 0; i < operations; i = i + 1 {
            let latency = Random::next_int() % 500 + 10 // 10-510ms
            Histogram::record(histogram, latency, attributes)
          }
        }
      }
    }
  }
  
  // Assert - Verify multi-dimensional aggregation
  let counter_data = Counter::get_data(counter)
  let histogram_data = Histogram::get_data(histogram)
  
  // Should have data for all dimension combinations
  let expected_combinations = services.length() * regions.length() * versions.length() * statuses.length()
  assert_true(counter_data.dimension_count >= expected_combinations)
  assert_true(histogram_data.dimension_count >= expected_combinations)
  
  // Verify aggregation by specific dimensions
  let auth_metrics = Counter::get_data_by_attributes(counter, Attributes::from([("service", "auth")]))
  assert_true(auth_metrics.count > 0)
  
  let us_east_metrics = Counter::get_data_by_attributes(counter, Attributes::from([("region", "us-east")]))
  assert_true(us_east_metrics.count > 0)
  
  let success_metrics = Histogram::get_data_by_attributes(histogram, Attributes::from([("status", "success")]))
  assert_true(success_metrics.count > 0)
}

test "time-series metrics aggregation" {
  // Arrange - Create meter for time-series testing
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "time-series-test")
  
  // Act - Create time-series metrics
  let gauge = Meter::create_gauge(meter, "cpu_usage", "CPU usage over time", "percent")
  let counter = Meter::create_counter(meter, "request_rate", "Request rate over time", "requests/sec")
  
  // Simulate time-series data collection
  let time_points = 100
  let start_time = Clock::now()
  
  for i = 0; i < time_points; i = i + 1 {
    let timestamp = start_time + (i * 1000) // 1 second intervals
    
    // Simulate CPU usage pattern
    let cpu_usage = 50 + (i % 20) + Random::next_int() % 10
    let time_attributes = Attributes::from([
      ("timestamp", timestamp.to_string()),
      ("metric_type", "gauge"),
      ("instance", "server-" + (i % 5).to_string())
    ])
    
    Gauge::set(gauge, cpu_usage, time_attributes)
    
    // Simulate request rate pattern
    let request_rate = 100 + (i * 2) + Random::next_int() % 50
    let counter_attributes = Attributes::from([
      ("timestamp", timestamp.to_string()),
      ("metric_type", "counter"),
      ("instance", "server-" + (i % 5).to_string())
    ])
    
    Counter::add(counter, request_rate, counter_attributes)
  }
  
  // Assert - Verify time-series aggregation
  let gauge_data = Gauge::get_data(gauge)
  let counter_data = Counter::get_data(counter)
  
  assert_eq(gauge_data.time_points, time_points)
  assert_eq(counter_data.time_points, time_points)
  
  // Test time-series aggregation functions
  let avg_cpu = Gauge::time_series_average(gauge, Attributes::from([("instance", "server-0")]))
  let max_cpu = Gauge::time_series_max(gauge, Attributes::from([("instance", "server-0")]))
  let min_cpu = Gauge::time_series_min(gauge, Attributes::from([("instance", "server-0")]))
  
  assert_true(avg_cpu > 0)
  assert_true(max_cpu >= avg_cpu)
  assert_true(min_cpu <= avg_cpu)
  
  // Test time-series rate calculation
  let request_rate_avg = Counter::time_series_rate(counter, Attributes::from([("instance", "server-0")]))
  assert_true(request_rate_avg > 0)
}

test "percentile and distribution aggregation" {
  // Arrange - Create meter for distribution testing
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "distribution-test")
  
  // Act - Create distribution metrics
  let response_time_histogram = Meter::create_histogram(meter, "response_time", "Response time distribution", "ms")
  let size_histogram = Meter::create_histogram(meter, "payload_size", "Payload size distribution", "bytes")
  
  // Generate data with different distributions
  let sample_count = 10000
  
  // Normal distribution for response times
  for i = 0; i < sample_count; i = i + 1 {
    let response_time = 100 + (Random::next_int() % 200) + (Random::next_int() % 100)
    let attributes = Attributes::from([
      ("endpoint", "/api/data"),
      ("method", "GET"),
      ("distribution", "normal")
    ])
    
    Histogram::record(response_time_histogram, response_time, attributes)
  }
  
  // Exponential distribution for payload sizes
  for i = 0; i < sample_count; i = i + 1 {
    let payload_size = 1024 * (1 + (Random::next_int() % 10))
    let attributes = Attributes::from([
      ("endpoint", "/api/upload"),
      ("method", "POST"),
      ("distribution", "exponential")
    ])
    
    Histogram::record(size_histogram, payload_size, attributes)
  }
  
  // Assert - Verify distribution aggregation
  let response_time_data = Histogram::get_data(response_time_histogram)
  let size_data = Histogram::get_data(size_histogram)
  
  // Test percentile calculations
  let p50_response = Histogram::percentile(response_time_histogram, 50.0)
  let p95_response = Histogram::percentile(response_time_histogram, 95.0)
  let p99_response = Histogram::percentile(response_time_histogram, 99.0)
  
  assert_true(p50_response > 0)
  assert_true(p95_response >= p50_response)
  assert_true(p99_response >= p95_response)
  
  let p50_size = Histogram::percentile(size_histogram, 50.0)
  let p95_size = Histogram::percentile(size_histogram, 95.0)
  let p99_size = Histogram::percentile(size_histogram, 99.0)
  
  assert_true(p50_size > 0)
  assert_true(p95_size >= p50_size)
  assert_true(p99_size >= p95_size)
  
  // Test distribution statistics
  let response_stats = Histogram::distribution_statistics(response_time_histogram)
  let size_stats = Histogram::distribution_statistics(size_histogram)
  
  assert_true(response_stats.mean > 0)
  assert_true(response_stats.variance >= 0)
  assert_true(response_stats.stddev >= 0)
  
  assert_true(size_stats.mean > 0)
  assert_true(size_stats.variance >= 0)
  assert_true(size_stats.stddev >= 0)
}

test "aggregation with dynamic attributes" {
  // Arrange - Create meter for dynamic attribute testing
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "dynamic-attributes-test")
  
  // Act - Create metrics with dynamic attributes
  let counter = Meter::create_counter(meter, "dynamic_counter", "Counter with dynamic attributes", "count")
  
  // Generate data with dynamically changing attributes
  let base_attributes = Attributes::from([
    ("service", "api-gateway"),
    ("environment", "production")
  ])
  
  for i = 0; i < 1000; i = i + 1 {
    // Dynamic attributes that change over time
    let dynamic_attributes = Attributes::from([
      ("instance", "instance-" + (i % 10).to_string()),
      ("version", "v" + ((i % 5) + 1).to_string()),
      ("build", "build-" + ((i % 20) + 1).to_string()),
      ("shard", "shard-" + (i % 3).to_string()),
      ("region", "region-" + (i % 4).to_string())
    ])
    
    // Merge base and dynamic attributes
    let merged_attributes = Attributes::merge(base_attributes, dynamic_attributes)
    
    Counter::add(counter, 1, merged_attributes)
  }
  
  // Assert - Verify dynamic attribute aggregation
  let counter_data = Counter::get_data(counter)
  
  // Should have many dimension combinations due to dynamic attributes
  assert_true(counter_data.dimension_count > 100)
  
  // Test aggregation by dynamic attributes
  let instance_metrics = Counter::aggregate_by_attribute(counter, "instance")
  assert_eq(instance_metrics.length(), 10) // 10 different instances
  
  let version_metrics = Counter::aggregate_by_attribute(counter, "version")
  assert_eq(version_metrics.length(), 5) // 5 different versions
  
  let region_metrics = Counter::aggregate_by_attribute(counter, "region")
  assert_eq(region_metrics.length(), 4) // 4 different regions
  
  // Test filtering by dynamic attributes
  let filtered_metrics = Counter::filter_by_attributes(counter, Attributes::from([
    ("version", "v3"),
    ("region", "region-2")
  ]))
  
  assert_true(filtered_metrics.count > 0)
}

test "cross-metrics correlation aggregation" {
  // Arrange - Create multiple related metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "correlation-test")
  
  // Act - Create correlated metrics
  let request_counter = Meter::create_counter(meter, "requests", "Total requests", "requests")
  let error_counter = Meter::create_counter(meter, "errors", "Total errors", "errors")
  let latency_histogram = Meter::create_histogram(meter, "latency", "Request latency", "ms")
  let active_connections_gauge = Meter::create_gauge(meter, "active_connections", "Active connections", "connections")
  
  // Simulate correlated metrics
  for i = 0; i < 500; i = i + 1 {
    let attributes = Attributes::from([
      ("endpoint", "/api/endpoint-" + (i % 5).to_string()),
      ("method", i % 2 == 0 ? "GET" : "POST"),
      ("status", i % 10 == 0 ? "error" : "success")
    ])
    
    // Record request
    Counter::add(request_counter, 1, attributes)
    
    // Record error if status is error
    if i % 10 == 0 {
      Counter::add(error_counter, 1, attributes)
    }
    
    // Record latency
    let latency = i % 10 == 0 ? 1000 + Random::next_int() % 500 : 50 + Random::next_int() % 100
    Histogram::record(latency_histogram, latency, attributes)
    
    // Update active connections
    let connections = 100 + (i % 50)
    Gauge::set(active_connections_gauge, connections, attributes)
  }
  
  // Assert - Verify correlated aggregation
  let request_data = Counter::get_data(request_counter)
  let error_data = Counter::get_data(error_counter)
  let latency_data = Histogram::get_data(latency_histogram)
  let connection_data = Gauge::get_data(active_connections_gauge)
  
  // Test error rate calculation
  let error_rate = MetricsCorrelation::calculate_error_rate(request_data, error_data)
  assert_true(error_rate > 0.0 && error_rate < 1.0)
  
  // Test latency percentiles by status
  let success_latency = Histogram::get_data_by_attributes(latency_histogram, Attributes::from([("status", "success")]))
  let error_latency = Histogram::get_data_by_attributes(latency_histogram, Attributes::from([("status", "error")]))
  
  assert_true(success_latency.mean < error_latency.mean) // Errors should have higher latency
  
  // Test correlation between metrics
  let correlation_data = MetricsCorrelation::correlate_metrics([
    ("requests", request_data),
    ("errors", error_data),
    ("latency", latency_data),
    ("active_connections", connection_data)
  ])
  
  assert_true(correlation_data.length() > 0)
  
  // Test aggregated dashboard metrics
  let dashboard_metrics = MetricsAggregation::create_dashboard_metrics([
    request_counter, error_counter, latency_histogram, active_connections_gauge
  ])
  
  assert_true(dashboard_metrics.contains("total_requests"))
  assert_true(dashboard_metrics.contains("error_rate"))
  assert_true(dashboard_metrics.contains("avg_latency"))
  assert_true(dashboard_metrics.contains("active_connections"))
}

test "aggregation with custom aggregation strategies" {
  // Arrange - Create meter for custom aggregation testing
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "custom-aggregation-test")
  
  // Act - Create metrics with custom aggregation
  let counter = Meter::create_counter_with_aggregation(meter, "custom_counter", 
    "Counter with custom aggregation", "count", 
    AggregationStrategy::SumWithReset)
  
  let histogram = Meter::create_histogram_with_aggregation(meter, "custom_histogram",
    "Histogram with custom aggregation", "ms",
    AggregationStrategy::ExponentialBuckets)
  
  let gauge = Meter::create_gauge_with_aggregation(meter, "custom_gauge",
    "Gauge with custom aggregation", "value",
    AggregationStrategy::LatestWithTimestamp)
  
  // Generate data for custom aggregation
  for i = 0; i < 1000; i = i + 1 {
    let attributes = Attributes::from([
      ("custom_tag", "value-" + (i % 10).to_string()),
      ("aggregation_type", "custom")
    ])
    
    Counter::add(counter, i % 100, attributes)
    Histogram::record(histogram, i % 1000, attributes)
    Gauge::set(gauge, i % 500, attributes)
  }
  
  // Assert - Verify custom aggregation behavior
  let counter_data = Counter::get_data_with_aggregation(counter, AggregationStrategy::SumWithReset)
  let histogram_data = Histogram::get_data_with_aggregation(histogram, AggregationStrategy::ExponentialBuckets)
  let gauge_data = Gauge::get_data_with_aggregation(gauge, AggregationStrategy::LatestWithTimestamp)
  
  // Test custom aggregation results
  assert_true(counter_data.aggregated_sum > 0)
  assert_true(counter_data.reset_count > 0)
  
  assert_true(histogram_data.bucket_counts.length() > 0)
  assert_true(histogram_data.exponential_scale > 0)
  
  assert_true(gauge_data.latest_value >= 0)
  assert_true(gauge_data.latest_timestamp > 0)
  
  // Test custom aggregation functions
  let custom_sum = CustomAggregation::weighted_average(counter_data, "custom_tag")
  let custom_percentile = CustomAggregation::adaptive_percentile(histogram_data, 95.0)
  let custom_trend = CustomAggregation::trend_analysis(gauge_data)
  
  assert_true(custom_sum >= 0)
  assert_true(custom_percentile >= 0)
  assert_true(custom_trend.direction != "") // Should have trend direction
}