// Azimuth Telemetry Data Processing Comprehensive Tests
// This file contains comprehensive test cases for telemetry data processing functionality

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_meter")
  
  // Create counters for aggregation testing
  let request_counter = Meter::create_counter(meter, "http_requests", Some("HTTP requests"), Some("count"))
  let error_counter = Meter::create_counter(meter, "http_errors", Some("HTTP errors"), Some("count"))
  
  // Simulate data points
  for i in 0..=10 {
    Counter::add(request_counter, 1.0, Some(Attributes::with_data([
      ("method", StringValue("GET")),
      ("status", StringValue("200")),
      ("endpoint", StringValue("/api/data"))
    ])))
  }
  
  // Simulate error data points
  for i in 0..=3 {
    Counter::add(error_counter, 1.0, Some(Attributes::with_data([
      ("method", StringValue("POST")),
      ("status", StringValue("500")),
      ("endpoint", StringValue("/api/create"))
    ])))
  }
  
  // Test histogram for response time aggregation
  let response_histogram = Meter::create_histogram(meter, "response_time", Some("Response time"), Some("ms"))
  let response_times = [100.0, 150.0, 200.0, 250.0, 300.0, 120.0, 180.0, 220.0]
  
  for time in response_times {
    Histogram::record(response_histogram, time, Some(Attributes::with_data([
      ("endpoint", StringValue("/api/data"))
    ])))
  }
  
  // Verify aggregation data structure
  let aggregation_data = TelemetryData::aggregate([
    ("http_requests", MetricType::Counter),
    ("http_errors", MetricType::Counter),
    ("response_time", MetricType::Histogram)
  ])
  
  assert_true(TelemetryData::contains_metric(aggregation_data, "http_requests"))
  assert_true(TelemetryData::contains_metric(aggregation_data, "http_errors"))
  assert_true(TelemetryData::contains_metric(aggregation_data, "response_time"))
}

// Test 2: Multi-dimensional Attribute Querying
test "multi-dimensional attribute querying" {
  let telemetry_data = TelemetryData::new()
  
  // Add complex multi-dimensional data
  TelemetryData::add_span(telemetry_data, Span::with_attributes("service-a", [
    ("service.name", StringValue("service-a")),
    ("service.version", StringValue("1.2.3")),
    ("deployment.region", StringValue("us-west-1")),
    ("deployment.zone", StringValue("us-west-1a")),
    ("instance.type", StringValue("large"))
  ]))
  
  TelemetryData::add_span(telemetry_data, Span::with_attributes("service-b", [
    ("service.name", StringValue("service-b")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.region", StringValue("us-east-1")),
    ("deployment.zone", StringValue("us-east-1b")),
    ("instance.type", StringValue("medium"))
  ]))
  
  TelemetryData::add_span(telemetry_data, Span::with_attributes("service-c", [
    ("service.name", StringValue("service-c")),
    ("service.version", StringValue("1.2.3")),
    ("deployment.region", StringValue("us-west-1")),
    ("deployment.zone", StringValue("us-west-1b")),
    ("instance.type", StringValue("large"))
  ]))
  
  // Test multi-dimensional queries
  let query1 = QueryBuilder::new()
    .with_attribute("service.version", "1.2.3")
    .with_attribute("deployment.region", "us-west-1")
    .build()
  
  let results1 = TelemetryData::query(telemetry_data, query1)
  assert_eq(results1.length(), 2) // service-a and service-c
  
  let query2 = QueryBuilder::new()
    .with_attribute("instance.type", "large")
    .build()
  
  let results2 = TelemetryData::query(telemetry_data, query2)
  assert_eq(results2.length(), 2) // service-a and service-c
  
  let query3 = QueryBuilder::new()
    .with_attribute("deployment.region", "us-east-1")
    .with_attribute("instance.type", "medium")
    .build()
  
  let results3 = TelemetryData::query(telemetry_data, query3)
  assert_eq(results3.length(), 1) // service-b only
}

// Test 3: Time Series Data Processing
test "time series data processing" {
  let time_series = TimeSeries::new("cpu_utilization", "percentage")
  
  // Add time series data points
  let base_time = 1640995200L // 2022-01-01 00:00:00 UTC
  let data_points = [
    (base_time, 45.2),
    (base_time + 60L, 48.7),
    (base_time + 120L, 52.1),
    (base_time + 180L, 49.8),
    (base_time + 240L, 55.3),
    (base_time + 300L, 58.9),
    (base_time + 360L, 54.6),
    (base_time + 420L, 51.2)
  ]
  
  for (timestamp, value) in data_points {
    TimeSeries::add_point(time_series, timestamp, value, [
      ("host", StringValue("server-01")),
      ("region", StringValue("us-west-1"))
    ])
  }
  
  // Test time range queries
  let start_time = base_time + 120L
  let end_time = base_time + 360L
  let range_data = TimeSeries::query_range(time_series, start_time, end_time)
  
  assert_eq(range_data.length(), 5) // Points at 120, 180, 240, 300, 360 seconds
  
  // Test aggregation functions
  let avg_value = TimeSeries::aggregate(time_series, AggregationType::Average, start_time, end_time)
  assert_true(avg_value > 50.0 && avg_value < 55.0)
  
  let max_value = TimeSeries::aggregate(time_series, AggregationType::Maximum, start_time, end_time)
  assert_eq(max_value, 58.9)
  
  let min_value = TimeSeries::aggregate(time_series, AggregationType::Minimum, start_time, end_time)
  assert_eq(min_value, 49.8)
  
  // Test downsampling
  let downsampled = TimeSeries::downsample(time_series, 300L, AggregationType::Average)
  assert_eq(downsampled.length(), 3) // 0-300, 300-600, 600-900 second windows
}

// Test 4: Real-time Stream Processing
test "real-time stream processing" {
  let stream_processor = StreamProcessor::new()
  
  // Set up stream processing pipeline
  StreamProcessor::add_filter(stream_processor, Filter::by_attribute("service.name", "api-gateway"))
  StreamProcessor::add_transformer(stream_processor, Transformer::normalize_timestamps())
  StreamProcessor::add_aggregator(stream_processor, Aggregator::time_window(60L)) // 1-minute windows
  StreamProcessor::add_output(stream_processor, Output::to_memory())
  
  // Simulate real-time data stream
  let stream_data = [
    SpanData::new("request-1", "api-gateway", 1640995200L, [("endpoint", StringValue("/users"))]),
    SpanData::new("request-2", "api-gateway", 1640995205L, [("endpoint", StringValue("/orders"))]),
    SpanData::new("request-3", "web-server", 1640995210L, [("endpoint", StringValue("/home"))]), // Should be filtered
    SpanData::new("request-4", "api-gateway", 1640995215L, [("endpoint", StringValue("/users"))]),
    SpanData::new("request-5", "api-gateway", 1640995220L, [("endpoint", StringValue("/products"))])
  ]
  
  for data in stream_data {
    StreamProcessor::process(stream_processor, data)
  }
  
  // Verify processing results
  let processed_data = StreamProcessor::get_output(stream_processor)
  assert_eq(processed_data.length(), 4) // 3 items filtered by service.name
  
  // Verify aggregation
  let aggregated = StreamProcessor::get_aggregated(stream_processor)
  assert_true(aggregated.contains_key("api-gateway"))
  assert_eq(aggregated.get("api-gateway"), 4)
}

// Test 5: Data Quality Validation
test "data quality validation" {
  let validator = DataQualityValidator::new()
  
  // Add validation rules
  DataQualityValidator::add_rule(validator, ValidationRule::required_attribute("trace_id"))
  DataQualityValidator::add_rule(validator, ValidationRule::required_attribute("span_id"))
  DataQualityValidator::add_rule(validator, ValidationRule::timestamp_range(1640995200L, 1640995280L))
  DataQualityValidator::add_rule(validator, ValidationRule::attribute_length("service.name", 1, 50))
  DataQualityValidator::add_rule(validator, ValidationRule::numeric_range("duration_ms", 0, 300000))
  
  // Test valid data
  let valid_span = SpanData::with_attributes("valid-span", [
    ("trace_id", StringValue("0af7651916cd43dd8448eb211c80319c")),
    ("span_id", StringValue("b7ad6b7169203331")),
    ("service.name", StringValue("payment-service")),
    ("duration_ms", IntValue(250))
  ])
  
  let valid_result = DataQualityValidator::validate(validator, valid_span)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid data
  let invalid_span = SpanData::with_attributes("invalid-span", [
    ("trace_id", StringValue("")), // Empty trace_id
    ("service.name", StringValue("")), // Empty service name
    ("duration_ms", IntValue(500000)) // Duration too large
  ])
  
  let invalid_result = DataQualityValidator::validate(validator, invalid_span)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() >= 2)
  
  // Test data with out-of-range timestamp
  let old_span = SpanData::with_timestamp("old-span", 1640995000L, [
    ("trace_id", StringValue("0af7651916cd43dd8448eb211c80319c")),
    ("span_id", StringValue("b7ad6b7169203331"))
  ])
  
  let old_result = DataQualityValidator::validate(validator, old_span)
  assert_false(old_result.is_valid)
  assert_true(old_result.errors.contains("timestamp_out_of_range"))
}

// Test 6: Correlation Analysis
test "telemetry correlation analysis" {
  let correlation_analyzer = CorrelationAnalyzer::new()
  
  // Add related telemetry data
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  let spans = [
    SpanData::with_trace("frontend-request", trace_id, "frontend", 1000L, 1050L),
    SpanData::with_trace("api-call", trace_id, "api-gateway", 1050L, 1100L),
    SpanData::with_trace("auth-check", trace_id, "auth-service", 1100L, 1120L),
    SpanData::with_trace("data-query", trace_id, "database", 1120L, 1180L),
    SpanData::with_trace("response-build", trace_id, "api-gateway", 1180L, 1200L),
    SpanData::with_trace("frontend-response", trace_id, "frontend", 1200L, 1220L)
  ]
  
  for span in spans {
    CorrelationAnalyzer::add_span(correlation_analyzer, span)
  }
  
  // Analyze correlations
  let correlations = CorrelationAnalyzer::analyze(correlation_analyzer)
  
  // Verify parent-child relationships
  assert_true(correlations.has_parent_child("frontend-request", "api-call"))
  assert_true(correlations.has_parent_child("api-call", "auth-check"))
  assert_true(correlations.has_parent_child("auth-check", "data-query"))
  assert_true(correlations.has_parent_child("data-query", "response-build"))
  assert_true(correlations.has_parent_child("response-build", "frontend-response"))
  
  // Verify critical path analysis
  let critical_path = CorrelationAnalyzer::critical_path(correlation_analyzer)
  assert_eq(critical_path.length(), 6)
  assert_eq(critical_path[0], "frontend-request")
  assert_eq(critical_path[5], "frontend-response")
  
  // Verify service dependencies
  let dependencies = CorrelationAnalyzer::service_dependencies(correlation_analyzer)
  assert_true(dependencies.has_dependency("frontend", "api-gateway"))
  assert_true(dependencies.has_dependency("api-gateway", "auth-service"))
  assert_true(dependencies.has_dependency("api-gateway", "database"))
}

// Test 7: Anomaly Detection
test "telemetry anomaly detection" {
  let anomaly_detector = AnomalyDetector::new()
  
  // Configure anomaly detection rules
  AnomalyDetector::add_rule(anomaly_detector, AnomalyRule::response_time_spike(1000.0)) // Alert if > 1s
  AnomalyDetector::add_rule(anomaly_detector, AnomalyRule::error_rate_increase(0.1)) // Alert if > 10% errors
  AnomalyDetector::add_rule(anomaly_detector, AnomalyRule::traffic_drop(0.5)) // Alert if > 50% traffic drop
  
  // Add baseline data
  let baseline_data = [
    SpanData::with_duration("baseline-1", 100L),
    SpanData::with_duration("baseline-2", 150L),
    SpanData::with_duration("baseline-3", 120L),
    SpanData::with_duration("baseline-4", 200L),
    SpanData::with_duration("baseline-5", 180L)
  ]
  
  for data in baseline_data {
    AnomalyDetector::add_baseline(anomaly_detector, data)
  }
  
  // Add normal data
  let normal_data = [
    SpanData::with_duration("normal-1", 130L),
    SpanData::with_duration("normal-2", 160L),
    SpanData::with_duration("normal-3", 140L)
  ]
  
  for data in normal_data {
    let result = AnomalyDetector::analyze(anomaly_detector, data)
    assert_false(result.is_anomaly)
  }
  
  // Add anomalous data
  let anomalous_data = [
    SpanData::with_duration("slow-1", 1500L), // Response time spike
    SpanData::with_error("error-1", true), // Error
    SpanData::with_error("error-2", true), // Error
    SpanData::with_error("error-3", true) // Error
  ]
  
  for data in anomalous_data {
    let result = AnomalyDetector::analyze(anomaly_detector, data)
    if SpanData::duration(data) > 1000L {
      assert_true(result.is_anomaly)
      assert_true(result.anomaly_types.contains("response_time_spike"))
    }
  }
  
  // Test error rate anomaly
  let error_rate_result = AnomalyDetector::check_error_rate(anomaly_detector, 0.15) // 15% error rate
  assert_true(error_rate_result.is_anomaly)
  assert_true(error_rate_result.anomaly_types.contains("high_error_rate"))
}

// Test 8: Data Retention and Cleanup
test "data retention and cleanup" {
  let retention_manager = RetentionManager::new()
  
  // Configure retention policies
  RetentionManager::add_policy(retention_manager, RetentionPolicy::by_age(86400L)) // 24 hours
  RetentionManager::add_policy(retention_manager, RetentionPolicy::by_count(1000)) // Max 1000 items
  RetentionManager::add_policy(retention_manager, RetentionPolicy::by_attribute("importance", "low", 3600L)) // Low importance: 1 hour
  
  // Add test data with different timestamps and importance
  let current_time = 1640995200L
  let old_time = current_time - 90000L // More than 24 hours ago
  
  let data_items = [
    SpanData::with_timestamp_and_importance("recent-high", current_time - 1000L, "high"),
    SpanData::with_timestamp_and_importance("recent-medium", current_time - 2000L, "medium"),
    SpanData::with_timestamp_and_importance("recent-low", current_time - 3000L, "low"),
    SpanData::with_timestamp_and_importance("old-high", old_time, "high"),
    SpanData::with_timestamp_and_importance("old-medium", old_time - 1000L, "medium"),
    SpanData::with_timestamp_and_importance("old-low", old_time - 2000L, "low")
  ]
  
  for data in data_items {
    RetentionManager::add_data(retention_manager, data)
  }
  
  // Run cleanup
  let cleanup_result = RetentionManager::cleanup(retention_manager)
  
  // Verify cleanup results
  assert_true(cleanup_result.deleted_items > 0)
  assert_true(cleanup_result.remaining_items < 6)
  
  // Verify specific items are retained/deleted
  assert_true(RetentionManager::contains(retention_manager, "recent-high"))
  assert_true(RetentionManager::contains(retention_manager, "recent-medium"))
  assert_true(RetentionManager::contains(retention_manager, "recent-low"))
  assert_false(RetentionManager::contains(retention_manager, "old-high"))
  assert_false(RetentionManager::contains(retention_manager, "old-medium"))
  assert_false(RetentionManager::contains(retention_manager, "old-low"))
}