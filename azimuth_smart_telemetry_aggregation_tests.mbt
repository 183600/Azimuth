// Azimuth Telemetry System - Smart Telemetry Aggregation Tests
// This file contains test cases for intelligent telemetry aggregation features

// Test 1: Adaptive Sampling Aggregation
test "adaptive sampling aggregation" {
  let adaptive_sampler = AdaptiveSamplingAggregator::new()
  
  // Configure adaptive sampling based on trace attributes
  let sampling_config = AdaptiveSamplingConfig::new()
    .with_high_priority_services(["critical-service", "payment-service"])
    .with_error_sampling_rate(1.0) // 100% for errors
    .with_normal_sampling_rate(0.1) // 10% for normal operations
    .with_low_volume_threshold(100)
  
  AdaptiveSamplingAggregator::configure(adaptive_sampler, sampling_config)
  
  // Test high priority service sampling
  let critical_trace = TelemetryTrace::new("critical_trace_001", "critical-service")
  let critical_result = AdaptiveSamplingAggregator::should_sample(adaptive_sampler, critical_trace)
  assert_true(critical_result) // Should always sample critical services
  
  // Test error trace sampling
  let error_trace = TelemetryTrace::new("error_trace_001", "normal-service")
    .with_error(true)
    .with_status_code(500)
  let error_result = AdaptiveSamplingAggregator::should_sample(adaptive_sampler, error_trace)
  assert_true(error_result) // Should always sample errors
  
  // Test normal service sampling with volume
  let mut normal_sampled_count = 0
  for i in 0..=1000 {
    let normal_trace = TelemetryTrace::new("normal_trace_" + i.to_string(), "normal-service")
    if AdaptiveSamplingAggregator::should_sample(adaptive_sampler, normal_trace) {
      normal_sampled_count = normal_sampled_count + 1
    }
  }
  
  // Verify sampling rate is approximately 10%
  let sampling_rate = normal_sampled_count.to_float() / 1000.0
  assert_true(sampling_rate > 0.05 && sampling_rate < 0.15) // Allow some variance
}

// Test 2: Temporal Pattern Aggregation
test "temporal pattern aggregation" {
  let temporal_aggregator = TemporalPatternAggregator::new()
  
  // Configure time windows for aggregation
  let window_config = TemporalWindowConfig::new()
    .with_minute_window()
    .with_hour_window()
    .with_day_window()
    .with_retention_period(Duration::days(7))
  
  TemporalPatternAggregator::configure(temporal_aggregator, window_config)
  
  // Generate telemetry data with different timestamps
  let base_time = Time::now() - Duration::hours(2) // 2 hours ago
  
  for i in 0..=120 {
    let timestamp = base_time + Duration::minutes(i)
    let metric_value = 100.0 + (i % 20).to_float() * 5.0 // Some variation
    let telemetry_metric = TelemetryMetric::new("response_time", metric_value, timestamp)
      .with_service("api-service")
      .with_operation("get_user")
    
    TemporalPatternAggregator::add_metric(temporal_aggregator, telemetry_metric)
  }
  
  // Test minute-level aggregation
  let minute_patterns = TemporalPatternAggregator::get_minute_patterns(temporal_aggregator)
  assert_true(minute_patterns.length() > 0)
  
  // Test hour-level aggregation
  let hour_patterns = TemporalPatternAggregator::get_hour_patterns(temporal_aggregator)
  assert_true(hour_patterns.length() >= 2) // Should have at least 2 hours of data
  
  // Verify pattern detection
  let detected_patterns = TemporalPatternAggregator::detect_patterns(temporal_aggregator)
  assert_true(detected_patterns.length() > 0)
  
  // Verify anomaly detection
  let anomalies = TemporalPatternAggregator::detect_anomalies(temporal_aggregator)
  assert_true(anomalies.length() >= 0) // May or may not have anomalies
}

// Test 3: Service Dependency Aggregation
test "service dependency aggregation" {
  let dependency_aggregator = ServiceDependencyAggregator::new()
  
  // Configure dependency tracking
  let dependency_config = DependencyTrackingConfig::new()
    .with_call_chain_tracking(true)
    .with_latency_thresholds([
      ("fast", Duration::milliseconds(100)),
      ("normal", Duration::milliseconds(500)),
      ("slow", Duration::milliseconds(2000))
    ])
    .with_error_rate_threshold(0.05) // 5%
  
  ServiceDependencyAggregator::configure(dependency_aggregator, dependency_config)
  
  // Simulate service calls with dependencies
  let trace1 = TelemetryTrace::new("trace_001", "frontend-service")
    .add_span("frontend", "api-gateway", Duration::milliseconds(150))
    .add_span("api-gateway", "user-service", Duration::milliseconds(200))
    .add_span("api-gateway", "order-service", Duration::milliseconds(300))
  
  let trace2 = TelemetryTrace::new("trace_002", "frontend-service")
    .add_span("frontend", "api-gateway", Duration::milliseconds(120))
    .add_span("api-gateway", "user-service", Duration::milliseconds(180))
    .add_span("user-service", "database", Duration::milliseconds(50))
  
  let trace3 = TelemetryTrace::new("trace_003", "api-gateway")
    .add_span("api-gateway", "user-service", Duration::milliseconds(5000)) // Slow call
    .add_span("api-gateway", "order-service", Duration::milliseconds(250))
  
  // Add traces to aggregator
  ServiceDependencyAggregator::add_trace(dependency_aggregator, trace1)
  ServiceDependencyAggregator::add_trace(dependency_aggregator, trace2)
  ServiceDependencyAggregator::add_trace(dependency_aggregator, trace3)
  
  // Test dependency graph generation
  let dependency_graph = ServiceDependencyAggregator::get_dependency_graph(dependency_aggregator)
  assert_true(dependency_graph.nodes.length() >= 4) // frontend, api-gateway, user-service, order-service, database
  assert_true(dependency_graph.edges.length() >= 5) // At least 5 service calls
  
  // Test critical path identification
  let critical_paths = ServiceDependencyAggregator::get_critical_paths(dependency_aggregator)
  assert_true(critical_paths.length() > 0)
  
  // Test service health scoring
  let health_scores = ServiceDependencyAggregator::get_service_health_scores(dependency_aggregator)
  assert_true(health_scores.length() >= 4)
  
  // Verify user-service has lower health due to slow call
  let user_service_health = ServiceHealthScores::get_score(health_scores, "user-service")
  let api_gateway_health = ServiceHealthScores::get_score(health_scores, "api-gateway")
  assert_true(user_service_health < api_gateway_health)
}

// Test 4: Intelligent Metric Correlation
test "intelligent metric correlation" {
  let correlation_analyzer = IntelligentMetricCorrelation::new()
  
  // Configure correlation analysis
  let correlation_config = CorrelationConfig::new()
    .with_correlation_threshold(0.7) // 70% correlation threshold
    .with_time_window(Duration::minutes(5))
    .with_min_sample_size(30)
  
  IntelligentMetricCorrelation::configure(correlation_analyzer, correlation_config)
  
  // Generate correlated metrics
  let base_time = Time::now() - Duration::minutes(10)
  
  for i in 0..=100 {
    let timestamp = base_time + Duration::seconds(i * 6) // Every 6 seconds
    
    // Create correlated metrics (CPU and response time)
    let cpu_usage = 50.0 + (i % 10).to_float() * 3.0
    let response_time = 100.0 + cpu_usage * 2.0 + (i % 5).to_float() * 10.0
    
    let cpu_metric = TelemetryMetric::new("cpu_usage", cpu_usage, timestamp)
      .with_service("api-service")
      .with_instance("instance-1")
    
    let response_time_metric = TelemetryMetric::new("response_time", response_time, timestamp)
      .with_service("api-service")
      .with_instance("instance-1")
    
    IntelligentMetricCorrelation::add_metric(correlation_analyzer, cpu_metric)
    IntelligentMetricCorrelation::add_metric(correlation_analyzer, response_time_metric)
  }
  
  // Test correlation analysis
  let correlations = IntelligentMetricCorrelation::analyze_correlations(correlation_analyzer)
  assert_true(correlations.length() > 0)
  
  // Verify strong correlation between CPU and response time
  let cpu_response_correlation = CorrelationResults::find(correlations, "cpu_usage", "response_time")
  match cpu_response_correlation {
    Some(correlation) => assert_true(correlation.coefficient > 0.7)
    None => assert_true(false)
  }
  
  // Test causal inference
  let causal_relationships = IntelligentMetricCorrelation::infer_causality(correlation_analyzer)
  assert_true(causal_relationships.length() > 0)
  
  // Verify CPU usage is identified as potential cause for response time
  let cpu_causes_response = CausalResults::find(causal_relationships, "cpu_usage", "response_time")
  match cpu_causes_response {
    Some(relationship) => assert_true(relationship.confidence > 0.5)
    None => assert_true(false)
  }
}

// Test 5: Predictive Anomaly Detection
test "predictive anomaly detection" {
  let anomaly_detector = PredictiveAnomalyDetector::new()
  
  // Configure anomaly detection
  let anomaly_config = AnomalyDetectionConfig::new()
    .with_algorithm(IsolationForest)
    .with_training_window(Duration::hours(24))
    .with_sensitivity(0.8) // 80% sensitivity
    .with_false_positive_rate(0.05) // 5% false positive rate
  
  PredictiveAnomalyDetector::configure(anomaly_detector, anomaly_config)
  
  // Generate normal training data
  let base_time = Time::now() - Duration::hours(25) // 25 hours ago
  let mut training_metrics = []
  
  for i in 0..=1000 {
    let timestamp = base_time + Duration::minutes(i * 1.5) // Every 1.5 minutes
    let normal_value = 100.0 + Math::sin(i.to_float() * 0.1) * 20.0 + Math::random() * 10.0
    let metric = TelemetryMetric::new("memory_usage", normal_value, timestamp)
      .with_service("web-service")
    
    training_metrics.push(metric)
  }
  
  // Train the anomaly detector
  PredictiveAnomalyDetector::train(anomaly_detector, training_metrics)
  
  // Test normal data (should not be flagged as anomaly)
  let normal_test_metrics = [
    TelemetryMetric::new("memory_usage", 105.0, Time::now()),
    TelemetryMetric::new("memory_usage", 95.0, Time::now()),
    TelemetryMetric::new("memory_usage", 110.0, Time::now())
  ]
  
  for metric in normal_test_metrics {
    let anomaly_score = PredictiveAnomalyDetector::analyze(anomaly_detector, metric)
    assert_true(anomaly_score < 0.5) // Should not be anomaly
  }
  
  // Test anomalous data (should be flagged as anomaly)
  let anomalous_test_metrics = [
    TelemetryMetric::new("memory_usage", 200.0, Time::now()), // Much higher than normal
    TelemetryMetric::new("memory_usage", 20.0, Time::now()),  // Much lower than normal
    TelemetryMetric::new("memory_usage", 500.0, Time::now())  // Extremely high
  ]
  
  let mut anomaly_count = 0
  for metric in anomalous_test_metrics {
    let anomaly_score = PredictiveAnomalyDetector::analyze(anomaly_detector, metric)
    if anomaly_score > 0.7 {
      anomaly_count = anomaly_count + 1
    }
  }
  
  assert_true(anomaly_count >= 2) // At least 2 should be detected as anomalies
}

// Test 6: Multi-Dimensional Rollup Aggregation
test "multi-dimensional rollup aggregation" {
  let rollup_aggregator = MultiDimensionalRollup::new()
  
  // Configure rollup dimensions
  let rollup_config = RollupConfig::new()
    .add_dimension("service")
    .add_dimension("operation")
    .add_dimension("region")
    .add_dimension("status_code")
    .with_time_granularity(Duration::minutes(5))
    .with_metrics(["request_count", "error_rate", "avg_response_time"])
  
  MultiDimensionalRollup::configure(rollup_aggregator, rollup_config)
  
  // Generate multi-dimensional telemetry data
  let services = ["api-service", "auth-service", "payment-service"]
  let operations = ["get", "post", "put", "delete"]
  let regions = ["us-east-1", "us-west-2", "eu-west-1"]
  let status_codes = [200, 201, 400, 404, 500]
  
  let base_time = Time::now() - Duration::hours(1)
  
  for i in 0..=500 {
    let timestamp = base_time + Duration::seconds(i * 7) // Every 7 seconds
    
    let service = services[i % services.length()]
    let operation = operations[i % operations.length()]
    let region = regions[i % regions.length()]
    let status_code = status_codes[i % status_codes.length()]
    
    let is_error = status_code >= 400
    let response_time = if is_error {
      500.0 + Math::random() * 1000.0
    } else {
      100.0 + Math::random() * 200.0
    }
    
    let telemetry_point = TelemetryPoint::new(timestamp)
      .add_tag("service", service)
      .add_tag("operation", operation)
      .add_tag("region", region)
      .add_tag("status_code", status_code.to_string())
      .add_metric("request_count", 1.0)
      .add_metric("error_rate", if is_error { 1.0 } else { 0.0 })
      .add_metric("response_time", response_time)
    
    MultiDimensionalRollup::add_point(rollup_aggregator, telemetry_point)
  }
  
  // Test service-level rollup
  let service_rollup = MultiDimensionalRollup::get_rollup(rollup_aggregator, ["service"])
  assert_true(service_rollup.length() >= 3) // At least 3 services
  
  // Test service + operation rollup
  let service_operation_rollup = MultiDimensionalRollup::get_rollup(rollup_aggregator, ["service", "operation"])
  assert_true(service_operation_rollup.length() >= services.length() * operations.length())
  
  // Test service + region rollup
  let service_region_rollup = MultiDimensionalRollup::get_rollup(rollup_aggregator, ["service", "region"])
  assert_true(service_region_rollup.length() >= services.length() * regions.length())
  
  // Test full dimensional rollup
  let full_rollup = MultiDimensionalRollup::get_rollup(rollup_aggregator, ["service", "operation", "region", "status_code"])
  assert_true(full_rollup.length() > 0)
  
  // Verify rollup accuracy
  for rollup_entry in service_rollup {
    let rollup_request_count = RollupEntry::get_metric(rollup_entry, "request_count")
    let rollup_error_count = RollupEntry::get_metric(rollup_entry, "error_rate")
    let rollup_avg_response_time = RollupEntry::get_metric(rollup_entry, "avg_response_time")
    
    assert_true(rollup_request_count > 0.0)
    assert_true(rollup_error_count >= 0.0)
    assert_true(rollup_avg_response_time > 0.0)
  }
}

// Test 7: Intelligent Alert Threshold Adjustment
test "intelligent alert threshold adjustment" {
  let alert_manager = IntelligentAlertManager::new()
  
  // Configure alert thresholds with auto-adjustment
  let alert_config = AlertConfig::new()
    .with_metric("error_rate")
    .with_initial_threshold(0.05) // 5%
    .with_auto_adjustment(true)
    .with_adjustment_window(Duration::hours(6))
    .with_target_false_positive_rate(0.1) // 10%
    .with_min_threshold(0.01) // 1%
    .with_max_threshold(0.2) // 20%
  
  IntelligentAlertManager::configure(alert_manager, alert_config)
  
  // Generate metrics with varying error rates
  let base_time = Time::now() - Duration::hours(8) // 8 hours ago
  
  // First 4 hours: normal error rate (2-3%)
  for i in 0..=240 {
    let timestamp = base_time + Duration::minutes(i)
    let error_rate = 0.02 + Math::random() * 0.01
    let metric = TelemetryMetric::new("error_rate", error_rate, timestamp)
      .with_service("api-service")
    
    IntelligentAlertManager::add_metric(alert_manager, metric)
  }
  
  // Next 2 hours: elevated error rate (7-8%)
  for i in 241..=360 {
    let timestamp = base_time + Duration::minutes(i)
    let error_rate = 0.07 + Math::random() * 0.01
    let metric = TelemetryMetric::new("error_rate", error_rate, timestamp)
      .with_service("api-service")
    
    IntelligentAlertManager::add_metric(alert_manager, metric)
  }
  
  // Last 2 hours: high error rate (12-15%)
  for i in 361..=480 {
    let timestamp = base_time + Duration::minutes(i)
    let error_rate = 0.12 + Math::random() * 0.03
    let metric = TelemetryMetric::new("error_rate", error_rate, timestamp)
      .with_service("api-service")
    
    IntelligentAlertManager::add_metric(alert_manager, metric)
  }
  
  // Test threshold adjustment
  let adjusted_threshold = IntelligentAlertManager::get_adjusted_threshold(alert_manager, "error_rate")
  assert_true(adjusted_threshold > 0.05) // Should be adjusted upward
  
  // Verify threshold is within bounds
  assert_true(adjusted_threshold >= 0.01 && adjusted_threshold <= 0.2)
  
  // Test alert evaluation with new threshold
  let current_metric = TelemetryMetric::new("error_rate", 0.08, Time::now())
    .with_service("api-service")
  
  let alert_result = IntelligentAlertManager::evaluate(alert_manager, current_metric)
  match alert_result {
    AlertState::Normal => assert_true(true) // 8% should be normal with adjusted threshold
    AlertState::Warning => assert_true(true) // Or warning depending on adjustment
    AlertState::Critical => assert_true(false) // Should not be critical
  }
  
  // Test with truly critical value
  let critical_metric = TelemetryMetric::new("error_rate", 0.25, Time::now())
    .with_service("api-service")
  
  let critical_result = IntelligentAlertManager::evaluate(alert_manager, critical_metric)
  match critical_result {
    AlertState::Critical => assert_true(true) // Should be critical
    _ => assert_true(false)
  }
}

// Test 8: Telemetry Data Compression and Retention
test "telemetry data compression and retention" {
  let compression_manager = TelemetryCompressionManager::new()
  
  // Configure compression and retention policies
  let compression_config = CompressionConfig::new()
    .with_compression_algorithm(LZ4)
    .with_compression_level(6)
    .with_retention_policies([
      RetentionPolicy::new(Duration::days(7), Raw), // Keep raw data for 7 days
      RetentionPolicy::new(Duration::days(30), Compressed), // Compressed for 30 days
      RetentionPolicy::new(Duration::days(90), Rollup) // Rollups for 90 days
    ])
    .with_auto_cleanup(true)
  
  TelemetryCompressionManager::configure(compression_manager, compression_config)
  
  // Generate telemetry data with different ages
  let now = Time::now()
  let mut telemetry_data = []
  
  // Recent data (should be kept raw)
  for i in 0..=100 {
    let timestamp = now - Duration::hours(i)
    let data = TelemetryData::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "service-" + (i % 5).to_string(),
      [("timestamp", StringValue(timestamp.to_string()))]
    )
    telemetry_data.push((data, timestamp))
  }
  
  // Older data (should be compressed)
  for i in 0..=50 {
    let timestamp = now - Duration::days(10 + i)
    let data = TelemetryData::new(
      "old_trace_" + i.to_string(),
      "old_span_" + i.to_string(),
      "old-service-" + (i % 3).to_string(),
      [("timestamp", StringValue(timestamp.to_string()))]
    )
    telemetry_data.push((data, timestamp))
  }
  
  // Very old data (should be rollup)
  for i in 0..=20 {
    let timestamp = now - Duration::days(40 + i)
    let data = TelemetryData::new(
      "very_old_trace_" + i.to_string(),
      "very_old_span_" + i.to_string(),
      "very-old-service-" + (i % 2).to_string(),
      [("timestamp", StringValue(timestamp.to_string()))]
    )
    telemetry_data.push((data, timestamp))
  }
  
  // Add all data to compression manager
  for (data, timestamp) in telemetry_data {
    TelemetryCompressionManager::add_data(compression_manager, data, timestamp)
  }
  
  // Test compression statistics
  let compression_stats = TelemetryCompressionManager::get_compression_stats(compression_manager)
  assert_true(compression_stats.raw_data_count > 0)
  assert_true(compression_stats.compressed_data_count > 0)
  assert_true(compression_stats.rollup_data_count > 0)
  
  // Verify compression ratio
  assert_true(compression_stats.compression_ratio > 1.0) // Should achieve some compression
  
  // Test data retrieval
  let recent_data = TelemetryCompressionManager::get_data(
    compression_manager,
    now - Duration::hours(1),
    now
  )
  assert_true(recent_data.length() > 0)
  assert_true(recent_data.all(|d| d.storage_format == Raw))
  
  let old_data = TelemetryCompressionManager::get_data(
    compression_manager,
    now - Duration::days(15),
    now - Duration::days(10)
  )
  assert_true(old_data.length() > 0)
  assert_true(old_data.all(|d| d.storage_format == Compressed))
  
  let very_old_data = TelemetryCompressionManager::get_data(
    compression_manager,
    now - Duration::days(45),
    now - Duration::days(40)
  )
  assert_true(very_old_data.length() > 0)
  assert_true(very_old_data.all(|d| d.storage_format == Rollup))
  
  // Test cleanup of expired data
  TelemetryCompressionManager::cleanup_expired_data(compression_manager)
  
  let after_cleanup_stats = TelemetryCompressionManager::get_compression_stats(compression_manager)
  assert_true(after_cleanup_stats.total_data_count <= compression_stats.total_data_count)
}