// Azimuth Telemetry System - Data Processing Tests
// This file contains test cases for telemetry data processing functionality

// Test 1: Telemetry Data Collection
test "telemetry data collection" {
  // Test collecting metrics data
  let metrics_collector = MetricsCollector::new()
  MetricsCollector::record_counter(metrics_collector, "request_count", 1.0)
  MetricsCollector::record_histogram(metrics_collector, "response_time", 150.5)
  MetricsCollector::record_gauge(metrics_collector, "active_connections", 42)
  
  // Verify collected data
  let metrics_data = MetricsCollector::get_data(metrics_collector)
  assert_true(metrics_data.length() > 0)
  
  // Test collecting trace data
  let trace_collector = TraceCollector::new()
  let span = TraceCollector::create_span(trace_collector, "operation", "parent_span_id")
  TraceCollector::add_event(span, "event1")
  TraceCollector::add_event(span, "event2")
  TraceCollector::finish_span(span)
  
  // Verify trace data
  let trace_data = TraceCollector::get_data(trace_collector)
  assert_true(trace_data.length() > 0)
}

// Test 2: Telemetry Data Aggregation
test "telemetry data aggregation" {
  let aggregator = DataAggregator::new()
  
  // Test counter aggregation
  for i in 0..=10 {
    DataAggregator::add_counter(aggregator, "requests", 1.0)
  }
  let counter_result = DataAggregator::get_counter(aggregator, "requests")
  match counter_result {
    Some(value) => assert_eq(value, 11.0)
    None => assert_true(false)
  }
  
  // Test histogram aggregation
  for value in [100.0, 150.0, 200.0, 120.0, 180.0] {
    DataAggregator::add_histogram_value(aggregator, "response_times", value)
  }
  let histogram_stats = DataAggregator::get_histogram_stats(aggregator, "response_times")
  match histogram_stats {
    Some(stats) => {
      assert_true(stats.min >= 100.0)
      assert_true(stats.max <= 200.0)
      assert_true(stats.count == 5)
    }
    None => assert_true(false)
  }
  
  // Test gauge aggregation (latest value)
  DataAggregator::set_gauge(aggregator, "memory_usage", 1024.0)
  DataAggregator::set_gauge(aggregator, "memory_usage", 2048.0)
  let gauge_result = DataAggregator::get_gauge(aggregator, "memory_usage")
  match gauge_result {
    Some(value) => assert_eq(value, 2048.0)
    None => assert_true(false)
  }
}

// Test 3: Telemetry Data Filtering
test "telemetry data filtering" {
  let processor = DataProcessor::new()
  
  // Create test data with various attributes
  let data_points = [
    DataPoint::new("metric1", 100.0, [("env", "production"), ("service", "api")]),
    DataPoint::new("metric2", 200.0, [("env", "staging"), ("service", "worker")]),
    DataPoint::new("metric3", 300.0, [("env", "production"), ("service", "database")]),
    DataPoint::new("metric4", 400.0, [("env", "development"), ("service", "api")])
  ]
  
  // Add data points to processor
  for point in data_points {
    DataProcessor::add_data_point(processor, point)
  }
  
  // Test filtering by environment
  let production_data = DataProcessor::filter_by_attribute(processor, "env", "production")
  assert_eq(production_data.length(), 2)
  
  // Test filtering by service
  let api_data = DataProcessor::filter_by_attribute(processor, "service", "api")
  assert_eq(api_data.length(), 2)
  
  // Test filtering by multiple attributes
  let prod_api_data = DataProcessor::filter_by_multiple_attributes(
    processor, 
    [("env", "production"), ("service", "api")]
  )
  assert_eq(prod_api_data.length(), 1)
}

// Test 4: Telemetry Data Transformation
test "telemetry data transformation" {
  let transformer = DataTransformer::new()
  
  // Test unit conversion
  let bytes_data = DataPoint::new("memory_bytes", 1048576.0, [])
  let mb_data = DataTransformer::convert_unit(transformer, bytes_data, "bytes", "megabytes")
  assert_eq(mb_data.value, 1.0)
  
  // Test rate calculation
  let time_series = [
    TimeSeriesPoint::new(1000L, 100.0),
    TimeSeriesPoint::new(2000L, 200.0),
    TimeSeriesPoint::new(3000L, 300.0)
  ]
  let rate_data = DataTransformer::calculate_rate(transformer, time_series)
  assert_true(rate_data.length() >= 1)
  
  // Test percentile calculation
  let values = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  let p50 = DataTransformer::calculate_percentile(transformer, values, 50.0)
  let p95 = DataTransformer::calculate_percentile(transformer, values, 95.0)
  assert_true(p50 >= 40.0 && p50 <= 60.0)
  assert_true(p95 >= 90.0 && p95 <= 100.0)
}

// Test 5: Telemetry Data Validation
test "telemetry data validation" {
  let validator = DataValidator::new()
  
  // Test valid data point
  let valid_point = DataPoint::new("valid_metric", 100.0, [("env", "production")])
  assert_true(DataValidator::is_valid(validator, valid_point))
  
  // Test invalid metric name
  let invalid_name_point = DataPoint::new("", 100.0, [])
  assert_false(DataValidator::is_valid(validator, invalid_name_point))
  
  // Test invalid value (NaN or infinity)
  let invalid_value_point = DataPoint::new("metric", 0.0/0.0, []) // NaN
  assert_false(DataValidator::is_valid(validator, invalid_value_point))
  
  // Test validation rules
  DataValidator::add_rule(validator, "response_time", ValidationRule::min_value(0.0))
  DataValidator::add_rule(validator, "response_time", ValidationRule::max_value(10000.0))
  
  let valid_response_time = DataPoint::new("response_time", 500.0, [])
  assert_true(DataValidator::validate_with_rules(validator, valid_response_time))
  
  let invalid_response_time = DataPoint::new("response_time", -100.0, [])
  assert_false(DataValidator::validate_with_rules(validator, invalid_response_time))
}

// Test 6: Telemetry Data Export
test "telemetry data export" {
  let exporter = DataExporter::new()
  
  // Prepare test data
  let data_batch = DataBatch::new()
  DataBatch::add_metric(data_batch, Metric::counter("requests", 100.0))
  DataBatch::add_metric(data_batch, Metric::histogram("response_time", [100.0, 200.0, 300.0]))
  DataBatch::add_trace(data_batch, Trace::new("trace_id", [Span::new("operation1"), Span::new("operation2")]))
  
  // Test JSON export
  let json_export = DataExporter::to_json(exporter, data_batch)
  assert_true(json_export.length() > 0)
  assert_true(json_export.contains("\"requests\""))
  
  // Test CSV export
  let csv_export = DataExporter::to_csv(exporter, data_batch)
  assert_true(csv_export.length() > 0)
  assert_true(csv_export.contains("metric,value"))
  
  // Test protobuf export (if available)
  let protobuf_export = DataExporter::to_protobuf(exporter, data_batch)
  assert_true(protobuf_export.length() > 0)
}

// Test 7: Telemetry Data Retention
test "telemetry data retention" {
  let retention_manager = RetentionManager::new()
  
  // Add data with different timestamps
  let now = Time::now()
  let old_timestamp = now - (7 * 24 * 60 * 60 * 1000) // 7 days ago
  let very_old_timestamp = now - (30 * 24 * 60 * 60 * 1000) // 30 days ago
  
  RetentionManager::add_data(retention_manager, DataPoint::with_timestamp("recent", 100.0, [], now))
  RetentionManager::add_data(retention_manager, DataPoint::with_timestamp("old", 200.0, [], old_timestamp))
  RetentionManager::add_data(retention_manager, DataPoint::with_timestamp("very_old", 300.0, [], very_old_timestamp))
  
  // Test retention policy (keep data for 14 days)
  RetentionManager::set_retention_policy(retention_manager, RetentionPolicy::max_age(14 * 24 * 60 * 60 * 1000))
  
  let retained_data = RetentionManager::get_retained_data(retention_manager)
  assert_eq(retained_data.length(), 2) // Should only keep recent and old data
  
  // Test retention by count
  RetentionManager::set_retention_policy(retention_manager, RetentionPolicy::max_count(1))
  let count_retained_data = RetentionManager::get_retained_data(retention_manager)
  assert_eq(count_retained_data.length(), 1) // Should only keep 1 most recent data point
}

// Test 8: Telemetry Data Sampling
test "telemetry data sampling" {
  let sampler = DataSampler::new()
  
  // Test deterministic sampling
  DataSampler::set_strategy(sampler, SamplingStrategy::deterministic(0.1)) // 10% sampling
  
  let sampled_count = 0
  for i in 0..=100 {
    let data_point = DataPoint::new("metric", i.to_float(), [])
    if DataSampler::should_sample(sampler, data_point) {
      sampled_count = sampled_count + 1
    }
  }
  
  // Should be approximately 10% of 100 samples (allowing for some variance)
  assert_true(sampled_count >= 5 && sampled_count <= 15)
  
  // Test priority sampling
  DataSampler::set_strategy(sampler, SamplingStrategy::priority())
  
  let high_priority_data = DataPoint::with_priority("critical_metric", 100.0, [], 10)
  let low_priority_data = DataPoint::with_priority("debug_metric", 100.0, [], 1)
  
  assert_true(DataSampler::should_sample(sampler, high_priority_data))
  assert_false(DataSampler::should_sample(sampler, low_priority_data))
}

// Test 9: Telemetry Data Enrichment
test "telemetry data enrichment" {
  let enricher = DataEnricher::new()
  
  // Add enrichment rules
  DataEnricher::add_static_enrichment(enricher, "env", "production")
  DataEnricher::add_dynamic_enrichment(enricher, "hostname", @() System::hostname())
  DataEnricher::add_conditional_enrichment(
    enricher, 
    "service_type",
    @() "web",
    (data) => data.name.contains("http")
  )
  
  // Test static enrichment
  let base_data = DataPoint::new("metric", 100.0, [])
  let enriched_data = DataEnricher::enrich(enricher, base_data)
  
  let env_value = DataPoint::get_attribute(enriched_data, "env")
  match env_value {
    Some(StringValue(value)) => assert_eq(value, "production")
    _ => assert_true(false)
  }
  
  // Test conditional enrichment
  let http_data = DataPoint::new("http_requests", 100.0, [])
  let enriched_http_data = DataEnricher::enrich(enricher, http_data)
  
  let service_type_value = DataPoint::get_attribute(enriched_http_data, "service_type")
  match service_type_value {
    Some(StringValue(value)) => assert_eq(value, "web")
    _ => assert_true(false)
  }
  
  // Test non-matching condition
  let db_data = DataPoint::new("db_queries", 100.0, [])
  let enriched_db_data = DataEnricher::enrich(enricher, db_data)
  
  let db_service_type_value = DataPoint::get_attribute(enriched_db_data, "service_type")
  match db_service_type_value {
    Some(_) => assert_true(false) // Should not have service_type attribute
    None => assert_true(true)
  }
}

// Test 10: Telemetry Data Analysis
test "telemetry data analysis" {
  let analyzer = DataAnalyzer::new()
  
  // Prepare time series data
  let time_series_data = [
    TimeSeriesPoint::new(1000L, 100.0),
    TimeSeriesPoint::new(2000L, 110.0),
    TimeSeriesPoint::new(3000L, 105.0),
    TimeSeriesPoint::new(4000L, 120.0),
    TimeSeriesPoint::new(5000L, 115.0),
    TimeSeriesPoint::new(6000L, 130.0),
    TimeSeriesPoint::new(7000L, 125.0),
    TimeSeriesPoint::new(8000L, 140.0),
    TimeSeriesPoint::new(9000L, 135.0),
    TimeSeriesPoint::new(10000L, 150.0)
  ]
  
  // Test trend analysis
  let trend = DataAnalyzer::calculate_trend(analyzer, time_series_data)
  match trend {
    TrendDirection::Increasing => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test anomaly detection
  let anomaly_data = [
    TimeSeriesPoint::new(1000L, 100.0),
    TimeSeriesPoint::new(2000L, 105.0),
    TimeSeriesPoint::new(3000L, 102.0),
    TimeSeriesPoint::new(4000L, 500.0), // Anomaly
    TimeSeriesPoint::new(5000L, 108.0),
    TimeSeriesPoint::new(6000L, 103.0)
  ]
  
  let anomalies = DataAnalyzer::detect_anomalies(analyzer, anomaly_data)
  assert_true(anomalies.length() >= 1)
  
  // Test forecasting
  let forecast = DataAnalyzer::forecast(analyzer, time_series_data, 3)
  assert_eq(forecast.length(), 3)
  
  // Test correlation analysis
  let series1 = [1.0, 2.0, 3.0, 4.0, 5.0]
  let series2 = [2.0, 4.0, 6.0, 8.0, 10.0]
  let correlation = DataAnalyzer::calculate_correlation(analyzer, series1, series2)
  assert_true(correlation > 0.9) // Should be highly correlated
}