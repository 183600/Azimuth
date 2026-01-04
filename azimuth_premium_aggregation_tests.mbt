// Azimuth Telemetry System - Premium Aggregation Tests
// This file contains advanced test cases for telemetry data aggregation functionality

// Test 1: Multi-dimensional Metric Aggregation
test "multi-dimensional metric aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_meter")
  
  // Create histogram for response time monitoring
  let response_histogram = Meter::create_histogram(
    meter, 
    "http.server.response_time", 
    Some("HTTP server response time"), 
    Some("ms")
  )
  
  // Record measurements with different dimensions
  let web_attrs = Attributes::new()
  Attributes::set(web_attrs, "service", StringValue("web"))
  Attributes::set(web_attrs, "endpoint", StringValue("/api/users"))
  Attributes::set(web_attrs, "method", StringValue("GET"))
  
  let api_attrs = Attributes::new()
  Attributes::set(api_attrs, "service", StringValue("api"))
  Attributes::set(api_attrs, "endpoint", StringValue("/api/data"))
  Attributes::set(api_attrs, "method", StringValue("POST"))
  
  // Record multiple measurements
  Histogram::record(response_histogram, 120.5, Some(web_attrs))
  Histogram::record(response_histogram, 85.3, Some(web_attrs))
  Histogram::record(response_histogram, 200.7, Some(web_attrs))
  Histogram::record(response_histogram, 95.2, Some(api_attrs))
  Histogram::record(response_histogram, 150.8, Some(api_attrs))
  
  // Test aggregation by service dimension
  let service_aggregation = Histogram::aggregate_by(response_histogram, ["service"])
  
  // Verify aggregated results
  match service_aggregation.get("web") {
    Some(web_stats) => {
      assert_eq(web_stats.count, 3)
      assert_eq(web_stats.sum, 120.5 + 85.3 + 200.7)
      assert_eq(web_stats.min, 85.3)
      assert_eq(web_stats.max, 200.7)
    }
    None => assert_true(false)
  }
  
  match service_aggregation.get("api") {
    Some(api_stats) => {
      assert_eq(api_stats.count, 2)
      assert_eq(api_stats.sum, 95.2 + 150.8)
      assert_eq(api_stats.min, 95.2)
      assert_eq(api_stats.max, 150.8)
    }
    None => assert_true(false)
  }
}

// Test 2: Time-series Data Aggregation
test "time-series data aggregation" {
  let time_series_provider = TimeSeriesProvider::new()
  let time_series = TimeSeriesProvider::create_series(
    time_series_provider,
    "cpu.utilization",
    TimeSeriesType::Gauge,
    Some("CPU utilization percentage")
  )
  
  // Record time-series data points
  let timestamp1 = 1609459200L // 2021-01-01 00:00:00
  let timestamp2 = 1609459260L // 2021-01-01 00:01:00
  let timestamp3 = 1609459320L // 2021-01-01 00:02:00
  let timestamp4 = 1609459380L // 2021-01-01 00:03:00
  
  TimeSeries::record_point(time_series, timestamp1, 45.2)
  TimeSeries::record_point(time_series, timestamp2, 52.8)
  TimeSeries::record_point(time_series, timestamp3, 48.1)
  TimeSeries::record_point(time_series, timestamp4, 61.3)
  
  // Test aggregation by time windows (1-minute windows)
  let windowed_aggregation = TimeSeries::aggregate_by_window(
    time_series,
    TimeWindow::Minutes(1),
    AggregationFunction::Average
  )
  
  // Verify windowed results
  assert_eq(windowed_aggregation.length(), 4)
  
  // Test downsampling to 5-minute windows
  let downsampled = TimeSeries::downsample(
    time_series,
    TimeWindow::Minutes(5),
    AggregationFunction::Average
  )
  
  // Verify downsampled results
  assert_eq(downsampled.length(), 1)
  match downsampled.get(0) {
    Some(point) => {
      assert_eq(point.value, (45.2 + 52.8 + 48.1 + 61.3) / 4.0)
    }
    None => assert_true(false)
  }
}

// Test 3: Distributed Aggregation with Multiple Nodes
test "distributed aggregation with multiple nodes" {
  let cluster = TelemetryCluster::new()
  
  // Simulate multiple nodes in the cluster
  let node1 = cluster.add_node("node-1")
  let node2 = cluster.add_node("node-2")
  let node3 = cluster.add_node("node-3")
  
  // Create counters on each node
  let counter1 = Node::create_counter(node1, "request.count")
  let counter2 = Node::create_counter(node2, "request.count")
  let counter3 = Node::create_counter(node3, "request.count")
  
  // Record measurements on each node
  Counter::add(counter1, 100.0)
  Counter::add(counter1, 50.0)
  Counter::add(counter2, 75.0)
  Counter::add(counter2, 25.0)
  Counter::add(counter3, 120.0)
  
  // Perform distributed aggregation
  let global_aggregation = cluster.aggregate_metric("request.count", AggregationFunction::Sum)
  
  // Verify global aggregation result
  assert_eq(global_aggregation.value, 100.0 + 50.0 + 75.0 + 25.0 + 120.0)
  assert_eq(global_aggregation.node_count, 3)
  
  // Test per-node breakdown
  let node_breakdown = cluster.get_node_breakdown("request.count")
  match node_breakdown.get("node-1") {
    Some(node1_value) => assert_eq(node1_value, 150.0)
    None => assert_true(false)
  }
  match node_breakdown.get("node-2") {
    Some(node2_value) => assert_eq(node2_value, 100.0)
    None => assert_true(false)
  }
  match node_breakdown.get("node-3") {
    Some(node3_value) => assert_eq(node3_value, 120.0)
    None => assert_true(false)
  }
}

// Test 4: Percentile-based Aggregation
test "percentile-based aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "percentile_meter")
  
  // Create histogram for latency measurements
  let latency_histogram = Meter::create_histogram(
    meter,
    "request.latency",
    Some("Request latency measurements"),
    Some("ms")
  )
  
  // Record latency measurements
  let latencies = [10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 100.0, 150.0, 200.0]
  for latency in latencies {
    Histogram::record(latency_histogram, latency)
  }
  
  // Calculate percentiles
  let p50 = Histogram::percentile(latency_histogram, 50.0)
  let p90 = Histogram::percentile(latency_histogram, 90.0)
  let p95 = Histogram::percentile(latency_histogram, 95.0)
  let p99 = Histogram::percentile(latency_histogram, 99.0)
  
  // Verify percentile calculations
  assert_eq(p50, 27.5) // Median of the dataset
  assert_eq(p90, 95.0)  // 90th percentile
  assert_eq(p95, 147.5) // 95th percentile
  assert_eq(p99, 198.0) // 99th percentile
  
  // Test percentile-based alerting
  let alert_threshold = 100.0
  let p99_above_threshold = p99 > alert_threshold
  assert_true(p99_above_threshold)
}

// Test 5: Aggregation with Attribute Filtering
test "aggregation with attribute filtering" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "filtering_meter")
  
  // Create counter for error tracking
  let error_counter = Meter::create_counter(
    meter,
    "error.count",
    Some("Error count by type and severity"),
    Some("count")
  )
  
  // Record errors with different attributes
  let critical_db_attrs = Attributes::new()
  Attributes::set(critical_db_attrs, "type", StringValue("database"))
  Attributes::set(critical_db_attrs, "severity", StringValue("critical"))
  
  let warning_network_attrs = Attributes::new()
  Attributes::set(warning_network_attrs, "type", StringValue("network"))
  Attributes::set(warning_network_attrs, "severity", StringValue("warning"))
  
  let info_auth_attrs = Attributes::new()
  Attributes::set(info_auth_attrs, "type", StringValue("authentication"))
  Attributes::set(info_auth_attrs, "severity", StringValue("info"))
  
  let another_critical_db_attrs = Attributes::new()
  Attributes::set(another_critical_db_attrs, "type", StringValue("database"))
  Attributes::set(another_critical_db_attrs, "severity", StringValue("critical"))
  
  // Record error counts
  Counter::add(error_counter, 5.0, Some(critical_db_attrs))
  Counter::add(error_counter, 3.0, Some(warning_network_attrs))
  Counter::add(error_counter, 10.0, Some(info_auth_attrs))
  Counter::add(error_counter, 2.0, Some(another_critical_db_attrs))
  
  // Filter by severity = critical
  let critical_filter = AttributeFilter::equals("severity", "critical")
  let critical_aggregation = Counter::aggregate_with_filter(error_counter, critical_filter)
  
  // Verify filtered aggregation
  assert_eq(critical_aggregation.value, 7.0) // 5 + 2 critical errors
  
  // Filter by type = database
  let db_filter = AttributeFilter::equals("type", "database")
  let db_aggregation = Counter::aggregate_with_filter(error_counter, db_filter)
  
  // Verify filtered aggregation
  assert_eq(db_aggregation.value, 7.0) // 5 + 2 database errors
  
  // Filter with multiple conditions (type = database AND severity = critical)
  let multi_filter = AttributeFilter::and([
    AttributeFilter::equals("type", "database"),
    AttributeFilter::equals("severity", "critical")
  ])
  let multi_aggregation = Counter::aggregate_with_filter(error_counter, multi_filter)
  
  // Verify multi-condition filtered aggregation
  assert_eq(multi_aggregation.value, 7.0) // Same as both conditions match the same errors
}

// Test 6: Aggregation with Temporal Patterns
test "aggregation with temporal patterns" {
  let time_series_provider = TimeSeriesProvider::new()
  let request_time_series = TimeSeriesProvider::create_series(
    time_series_provider,
    "request.rate",
    TimeSeriesType::Counter,
    Some("Request rate over time")
  )
  
  // Simulate request rate throughout a day (24 hours)
  let base_timestamp = 1609459200L // 2021-01-01 00:00:00
  let hourly_rate = [50.0, 45.0, 40.0, 35.0, 30.0, 40.0, 80.0, 120.0, 150.0, 180.0, 
                    200.0, 220.0, 210.0, 190.0, 180.0, 200.0, 250.0, 300.0, 280.0, 
                    200.0, 150.0, 100.0, 80.0, 60.0]
  
  // Record hourly request rates
  for i in 0..23 {
    let timestamp = base_timestamp + (i * 3600L) // Add i hours
    TimeSeries::record_point(request_time_series, timestamp, hourly_rate[i])
  }
  
  // Detect peak hours (top 3 hours with highest request rate)
  let peak_hours = TimeSeries::detect_peaks(request_time_series, 3)
  
  // Verify peak hours detection
  assert_eq(peak_hours.length(), 3)
  
  // The highest rate should be at hour 17 (5 PM) with 300.0 requests
  match peak_hours.get(0) {
    Some(peak) => {
      assert_eq(peak.timestamp, base_timestamp + (17 * 3600L))
      assert_eq(peak.value, 300.0)
    }
    None => assert_true(false)
  }
  
  // Test pattern detection - identify business hours (8 AM - 6 PM)
  let business_hours_pattern = TimeSeries::detect_pattern(
    request_time_series,
    PatternType::Recurring,
    TimeWindow::Hours(1),
    8, // Start hour
    18 // End hour
  )
  
  // Verify business hours pattern detection
  assert_true(business_hours_pattern.detected)
  assert_eq(business_hours_pattern.confidence, 0.8) // High confidence for business hours
  
  // Test anomaly detection
  let anomalies = TimeSeries::detect_anomalies(request_time_series, AnomalyMethod::ZScore, 2.0)
  
  // Verify anomaly detection (early morning hours with very low rates)
  assert_true(anomalies.length() > 0)
}