// Azimuth Telemetry System - New Feature Tests
// This file contains 10 new test cases focusing on advanced telemetry features

// Test 1: Real-time Telemetry Data Streaming
test "real-time telemetry data streaming" {
  // Test stream initialization
  let telemetry_stream = TelemetryStream::new("real-time-metrics", 1000)
  assert_true(TelemetryStream::is_active(telemetry_stream))
  assert_eq(TelemetryStream::buffer_size(telemetry_stream), 1000)
  
  // Test data ingestion
  let metric1 = TelemetryMetric::new("cpu_usage", 75.5, "percentage")
  let metric2 = TelemetryMetric::new("memory_usage", 1024.0, "megabytes")
  let metric3 = TelemetryMetric::new("disk_io", 150.2, "megabytes_per_second")
  
  TelemetryStream::ingest(telemetry_stream, metric1)
  TelemetryStream::ingest(telemetry_stream, metric2)
  TelemetryStream::ingest(telemetry_stream, metric3)
  
  assert_eq(TelemetryStream::count(telemetry_stream), 3)
  
  // Test stream processing
  let processor = StreamProcessor::new()
    .with_filter(fn(metric) { metric.value > 100.0 })
    .with_aggregation("avg")
  
  let processed_data = TelemetryStream::process(telemetry_stream, processor)
  assert_eq(processed_data.length(), 2) // memory_usage and disk_io
  
  // Test stream subscription
  let subscriber = StreamSubscriber::new("dashboard-client")
  TelemetryStream::subscribe(telemetry_stream, subscriber)
  
  assert_true(TelemetryStream::has_subscriber(telemetry_stream, "dashboard-client"))
  assert_eq(TelemetryStream::subscriber_count(telemetry_stream), 1)
  
  // Test stream shutdown
  TelemetryStream::shutdown(telemetry_stream)
  assert_false(TelemetryStream::is_active(telemetry_stream))
}

// Test 2: Distributed Tracing Across Services
test "distributed tracing across services" {
  // Test trace context creation
  let trace_context = TraceContext::new("trace-12345", "span-67890", "parent-abcde")
  assert_eq(TraceContext::trace_id(trace_context), "trace-12345")
  assert_eq(TraceContext::span_id(trace_context), "span-67890")
  assert_eq(TraceContext::parent_span_id(trace_context), "parent-abcde")
  
  // Test span creation with context
  let api_gateway_span = TraceSpan::new("api-gateway-request", trace_context)
    .with_attribute("service.name", "api-gateway")
    .with_attribute("http.method", "GET")
    .with_attribute("http.url", "/api/users")
  
  TraceSpan::start(api_gateway_span)
  assert_true(TraceSpan::is_recording(api_gateway_span))
  
  // Test child span creation
  let user_service_context = TraceContext::create_child(trace_context, "span-defgh")
  let user_service_span = TraceSpan::new("user-service-query", user_service_context)
    .with_attribute("service.name", "user-service")
    .with_attribute("db.query", "SELECT * FROM users WHERE id = ?")
  
  TraceSpan::start(user_service_span)
  
  // Test span events
  TraceSpan::add_event(api_gateway_span, "request_received", Some([
    ("timestamp", "1640995200000"),
    ("client.ip", "192.168.1.100")
  ]))
  
  TraceSpan::add_event(user_service_span, "db_query_started", Some([
    ("timestamp", "1640995200100"),
    ("db.connection", "conn-12345")
  ]))
  
  // Test span completion
  TraceSpan::end(user_service_span)
  assert_false(TraceSpan::is_recording(user_service_span))
  
  TraceSpan::end(api_gateway_span)
  assert_false(TraceSpan::is_recording(api_gateway_span))
  
  // Test trace collection
  let trace_collector = TraceCollector::new()
  TraceCollector::collect_span(trace_collector, api_gateway_span)
  TraceCollector::collect_span(trace_collector, user_service_span)
  
  let trace = TraceCollector::get_trace(trace_collector, "trace-12345")
  assert_eq(Trace::span_count(trace), 2)
  assert_true(Trace::has_span_with_name(trace, "api-gateway-request"))
  assert_true(Trace::has_span_with_name(trace, "user-service-query"))
}

// Test 3: Adaptive Sampling Strategies
test "adaptive sampling strategies" {
  // Test fixed rate sampling
  let fixed_sampler = FixedRateSampler::new(0.1) // 10% sampling rate
  assert_eq(FixedRateSampler::get_rate(fixed_sampler), 0.1)
  
  let sampled_count = { mut count: 0 }
  let total_count = 1000
  
  for i in 0..total_count {
    let trace_id = "trace-" + i.to_string()
    if FixedRateSampler::should_sample(fixed_sampler, trace_id) {
      sampled_count.count = sampled_count.count + 1
    }
  }
  
  // Should be approximately 10% of total
  let sampled_percentage = sampled_count.count * 100 / total_count
  assert_true(sampled_percentage >= 8 && sampled_percentage <= 12)
  
  // Test adaptive sampling based on error rates
  let adaptive_sampler = AdaptiveSampler::new()
    .with_base_rate(0.05)
    .with_max_rate(0.5)
    .with_error_threshold(0.1)
  
  // Simulate normal operation (low error rate)
  AdaptiveSampler::update_error_rate(adaptive_sampler, 0.02)
  assert_eq(AdaptiveSampler::get_current_rate(adaptive_sampler), 0.05)
  
  // Simulate high error rate
  AdaptiveSampler::update_error_rate(adaptive_sampler, 0.15)
  assert_true(AdaptiveSampler::get_current_rate(adaptive_sampler) > 0.05)
  
  // Test priority-based sampling
  let priority_sampler = PrioritySampler::new()
    .with_priority_rule("critical", 1.0) // Always sample critical traces
    .with_priority_rule("high", 0.5)     // Sample 50% of high priority traces
    .with_priority_rule("normal", 0.1)   // Sample 10% of normal priority traces
  
  assert_true(PrioritySampler::should_sample(priority_sampler, "trace-critical", "critical"))
  assert_false(PrioritySampler::should_sample(priority_sampler, "trace-normal", "normal"))
  
  // Test sampling decision recording
  let sampling_recorder = SamplingRecorder::new()
  SamplingRecorder::record_decision(sampling_recorder, "trace-1", true, "fixed-rate")
  SamplingRecorder::record_decision(sampling_recorder, "trace-2", false, "fixed-rate")
  SamplingRecorder::record_decision(sampling_recorder, "trace-3", true, "adaptive")
  
  assert_eq(SamplingRecorder::get_sampled_count(sampling_recorder), 2)
  assert_eq(SamplingRecorder::get_discarded_count(sampling_recorder), 1)
  
  let decisions = SamplingRecorder::get_decisions_by_strategy(sampling_recorder, "fixed-rate")
  assert_eq(decisions.length(), 2)
}

// Test 4: Metrics Aggregation and Downsampling
test "metrics aggregation and downsampling" {
  // Test time-series metrics creation
  let time_series = TimeSeries::new("response_time", "milliseconds")
  
  // Add data points over time
  TimeSeries::add_point(time_series, 1640995200000, 120.5)
  TimeSeries::add_point(time_series, 1640995260000, 135.2)
  TimeSeries::add_point(time_series, 1640995320000, 98.7)
  TimeSeries::add_point(time_series, 1640995380000, 145.3)
  TimeSeries::add_point(time_series, 1640995440000, 110.8)
  
  assert_eq(TimeSeries::point_count(time_series), 5)
  
  // Test aggregation functions
  let aggregator = TimeSeriesAggregator::new()
  
  let avg_response_time = TimeSeriesAggregator::aggregate(aggregator, time_series, "avg")
  assert_true(avg_response_time > 120.0 && avg_response_time < 125.0)
  
  let max_response_time = TimeSeriesAggregator::aggregate(aggregator, time_series, "max")
  assert_eq(max_response_time, 145.3)
  
  let min_response_time = TimeSeriesAggregator::aggregate(aggregator, time_series, "min")
  assert_eq(min_response_time, 98.7)
  
  // Test time window aggregation
  let window_aggregator = WindowAggregator::new(300000) // 5-minute windows
  let aggregated_series = WindowAggregator::aggregate_by_window(window_aggregator, time_series, "avg")
  
  assert_eq(aggregated_series.length(), 1) // All points fall within one 5-minute window
  
  // Test downsampling
  let downsampler = Downsampler::new()
    .with_target_interval(600000) // 10-minute intervals
    .with_aggregation_strategy("avg")
  
  let downsampled_series = Downsampler::downsample(downsampler, time_series)
  assert_eq(downsampled_series.length(), 1) // Downsampled to one 10-minute point
  
  // Test percentile calculations
  let percentile_calculator = PercentileCalculator::new()
  
  let p50 = PercentileCalculator::calculate(percentile_calculator, time_series, 50.0)
  let p95 = PercentileCalculator::calculate(percentile_calculator, time_series, 95.0)
  let p99 = PercentileCalculator::calculate(percentile_calculator, time_series, 99.0)
  
  assert_true(p50 > 110.0 && p50 < 130.0)
  assert_true(p95 > 140.0)
  assert_eq(p99, 145.3) // Maximum value for 99th percentile with 5 data points
}

// Test 5: Anomaly Detection in Telemetry Data
test "anomaly detection in telemetry data" {
  // Test statistical anomaly detection
  let statistical_detector = StatisticalAnomalyDetector::new()
    .with_threshold(2.0) // 2 standard deviations
    .with_window_size(100)
  
  // Generate normal data
  let normal_data = [100.0, 102.0, 98.0, 101.0, 99.0, 103.0, 97.0, 100.0, 102.0, 98.0]
  
  for value in normal_data {
    StatisticalAnomalyDetector::add_point(statistical_detector, value)
  }
  
  // Test normal point
  let normal_result = StatisticalAnomalyDetector::is_anomaly(statistical_detector, 101.5)
  assert_false(normal_result)
  
  // Test anomalous point
  let anomaly_result = StatisticalAnomalyDetector::is_anomaly(statistical_detector, 150.0)
  assert_true(anomaly_result)
  
  // Test machine learning-based anomaly detection
  let ml_detector = MLAnomalyDetector::new()
    .with_algorithm("isolation_forest")
    .with_training_window(1000)
  
  // Train the detector with normal data
  for i in 0..500 {
    let value = 100.0 + (i % 20) - 10.0 // Values between 90 and 110
    MLAnomalyDetector::train(ml_detector, value)
  }
  
  MLAnomalyDetector::build_model(ml_detector)
  
  // Test with normal data
  let normal_ml_result = MLAnomalyDetector::is_anomaly(ml_detector, 105.0)
  assert_false(normal_ml_result)
  
  // Test with anomalous data
  let anomaly_ml_result = MLAnomalyDetector::is_anomaly(ml_detector, 200.0)
  assert_true(anomaly_ml_result)
  
  // Test seasonal anomaly detection
  let seasonal_detector = SeasonalAnomalyDetector::new()
    .with_seasonality_period(86400000) // Daily seasonality
    .with_threshold(1.5)
  
  // Add data points with daily pattern
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  for day in 0..7 {
    for hour in 0..24 {
      let timestamp = base_timestamp + (day * 86400000) + (hour * 3600000)
      let value = 50.0 + 30.0 * ((hour as Float) / 24.0).sin() // Daily pattern
      SeasonalAnomalyDetector::add_point(seasonal_detector, timestamp, value)
    }
  }
  
  // Test normal seasonal point
  let normal_seasonal_result = SeasonalAnomalyDetector::is_anomaly(
    seasonal_detector, 
    base_timestamp + 43200000, // Noon
    65.0
  )
  assert_false(normal_seasonal_result)
  
  // Test anomalous seasonal point
  let anomaly_seasonal_result = SeasonalAnomalyDetector::is_anomaly(
    seasonal_detector, 
    base_timestamp + 43200000, // Noon
    120.0
  )
  assert_true(anomaly_seasonal_result)
  
  // Test anomaly alerting
  let alert_manager = AnomalyAlertManager::new()
    .with_alert_threshold(3) // Alert after 3 anomalies
    .with_reset_interval(3600000) // Reset after 1 hour
  
  AnomalyAlertManager::record_anomaly(alert_manager, "metric1", 1640995200000)
  AnomalyAlertManager::record_anomaly(alert_manager, "metric1", 1640995300000)
  AnomalyAlertManager::record_anomaly(alert_manager, "metric1", 1640995400000)
  
  assert_true(AnomalyAlertManager::should_alert(alert_manager, "metric1"))
  
  let alert = AnomalyAlertManager::create_alert(alert_manager, "metric1")
  assert_eq(alert.metric_name, "metric1")
  assert_eq(alert.anomaly_count, 3)
}

// Test 6: Telemetry Data Compression and Transmission
test "telemetry data compression and transmission" {
  // Test telemetry data serialization
  let telemetry_batch = TelemetryBatch::new("batch-12345")
  
  let span1 = TelemetrySpan::new("operation1", 1640995200000, 1640995250000)
    .with_attribute("service", "api")
    .with_attribute("user.id", "12345")
  
  let span2 = TelemetrySpan::new("operation2", 1640995300000, 1640995350000)
    .with_attribute("service", "database")
    .with_attribute("query.type", "select")
  
  let metric1 = TelemetryMetric::new("cpu_usage", 75.5, "percentage")
    .with_timestamp(1640995200000)
    .with_tags([("host", "server1"), ("region", "us-west")])
  
  let metric2 = TelemetryMetric::new("memory_usage", 4096.0, "megabytes")
    .with_timestamp(1640995250000)
    .with_tags([("host", "server1"), ("region", "us-west")])
  
  TelemetryBatch::add_span(telemetry_batch, span1)
  TelemetryBatch::add_span(telemetry_batch, span2)
  TelemetryBatch::add_metric(telemetry_batch, metric1)
  TelemetryBatch::add_metric(telemetry_batch, metric2)
  
  assert_eq(TelemetryBatch::span_count(telemetry_batch), 2)
  assert_eq(TelemetryBatch::metric_count(telemetry_batch), 2)
  
  // Test JSON serialization
  let json_serializer = JsonTelemetrySerializer::new()
  let json_data = JsonTelemetrySerializer::serialize(json_serializer, telemetry_batch)
  assert_true(json_data.length() > 0)
  
  // Test protobuf serialization
  let protobuf_serializer = ProtobufTelemetrySerializer::new()
  let protobuf_data = ProtobufTelemetrySerializer::serialize(protobuf_serializer, telemetry_batch)
  assert_true(protobuf_data.length() > 0)
  
  // Test compression
  let gzip_compressor = GzipCompressor::new()
  let compressed_json = GzipCompressor::compress(gzip_compressor, json_data)
  
  assert_true(compressed_json.length() < json_data.length())
  
  // Test decompression
  let decompressed_json = GzipCompressor::decompress(gzip_compressor, compressed_json)
  assert_eq(decompressed_json, json_data)
  
  // Test transmission queue
  let transmission_queue = TransmissionQueue::new(1000, 100) // Max 1000 items, batch size 100
  
  TransmissionQueue::add_batch(transmission_queue, telemetry_batch)
  assert_eq(TransmissionQueue::size(transmission_queue), 1)
  
  // Test batch transmission
  let transmitter = TelemetryTransmitter::new("https://telemetry.example.com/api/v1/traces")
    .with_retry_policy(ExponentialBackoffRetryPolicy::new(3, 1000))
    .with_compression(true)
    .with_timeout(5000)
  
  let transmission_result = TelemetryTransmitter::transmit(transmitter, telemetry_batch)
  assert_true(transmission_result.success)
  
  // Test async transmission
  let async_transmitter = AsyncTelemetryTransmitter::new("https://telemetry.example.com/api/v1/metrics")
    .with_concurrency(4)
    .with_queue_size(10000)
  
  let async_result = AsyncTelemetryTransmitter::transmit_async(async_transmitter, telemetry_batch)
  assert_true(async_result.queued)
  
  // Test transmission failure handling
  let failing_transmitter = FailingTelemetryTransmitter::new("https://invalid.example.com")
    .with_failure_rate(1.0) // Always fail
  
  let failure_result = TelemetryTransmitter::transmit(failing_transmitter, telemetry_batch)
  assert_false(failure_result.success)
  assert_true(failure_result.error_message.length() > 0)
}

// Test 7: Telemetry Data Retention and Archival
test "telemetry data retention and archival" {
  // Test retention policy creation
  let retention_policy = RetentionPolicy::new()
    .with_span_retention_days(7)
    .with_metric_retention_days(30)
    .with_log_retention_days(14)
    .with_archive_after_days(3)
  
  assert_eq(RetentionPolicy::get_span_retention_days(retention_policy), 7)
  assert_eq(RetentionPolicy::get_metric_retention_days(retention_policy), 30)
  assert_eq(RetentionPolicy::get_archive_after_days(retention_policy), 3)
  
  // Test data lifecycle management
  let lifecycle_manager = DataLifecycleManager::new(retention_policy)
  
  // Create test data with different ages
  let current_time = 1640995200000 // 2022-01-01 00:00:00 UTC
  let old_span_time = current_time - (8 * 86400000) // 8 days ago (should be expired)
  let recent_span_time = current_time - (3 * 86400000) // 3 days ago (should be archived)
  let very_recent_span_time = current_time - (1 * 86400000) // 1 day ago (should be active)
  
  let old_span = TelemetrySpan::new("old_operation", old_span_time, old_span_time + 1000)
  let recent_span = TelemetrySpan::new("recent_operation", recent_span_time, recent_span_time + 1000)
  let very_recent_span = TelemetrySpan::new("very_recent_operation", very_recent_span_time, very_recent_span_time + 1000)
  
  DataLifecycleManager::add_span(lifecycle_manager, old_span)
  DataLifecycleManager::add_span(lifecycle_manager, recent_span)
  DataLifecycleManager::add_span(lifecycle_manager, very_recent_span)
  
  // Test lifecycle processing
  let lifecycle_result = DataLifecycleManager::process(lifecycle_manager, current_time)
  
  assert_eq(lifecycle_result.expired_count, 1) // old_span should be expired
  assert_eq(lifecycle_result.archived_count, 1) // recent_span should be archived
  assert_eq(lifecycle_result.active_count, 1) // very_recent_span should remain active
  
  // Test archival storage
  let archival_storage = ArchivalStorage::new("s3://telemetry-archive/")
    .with_compression("gzip")
    .with_encryption(true)
  
  let archival_id = ArchivalStorage::archive(archival_storage, recent_span)
  assert_true(archival_id.length() > 0)
  
  // Test archival retrieval
  let retrieved_span = ArchivalStorage::retrieve(archival_storage, archival_id)
  assert_eq(retrieved_span.name, recent_span.name)
  assert_eq(retrieved_span.start_time, recent_span.start_time)
  
  // Test retention enforcement
  let retention_enforcer = RetentionEnforcer::new(retention_policy)
  
  // Add more test data
  for i in 0..100 {
    let timestamp = current_time - (i * 86400000)
    let span = TelemetrySpan::new("operation_" + i.to_string(), timestamp, timestamp + 1000)
    RetentionEnforcer::add_data(retention_enforcer, span)
  }
  
  let enforcement_result = RetentionEnforcer::enforce(retention_enforcer, current_time)
  assert_true(enforcement_result.deleted_count > 0)
  assert_true(enforcement_result.archived_count > 0)
  
  // Test data purging
  let purge_manager = DataPurgeManager::new()
    .with_purge_policy("strict") // Delete expired data immediately
    .with_batch_size(1000)
  
  let purge_result = DataPurgeManager::purge_expired_data(purge_manager, current_time)
  assert_true(purge_result.purged_count > 0)
  assert_eq(purge_result.error_count, 0)
}

// Test 8: Telemetry Configuration Management
test "telemetry configuration management" {
  // Test configuration source management
  let config_manager = TelemetryConfigManager::new()
  
  // Add file configuration source
  let file_config_source = FileConfigSource::new("/etc/telemetry/config.json")
  TelemetryConfigManager::add_source(config_manager, file_config_source)
  
  // Add environment configuration source
  let env_config_source = EnvironmentConfigSource::new("TELEMETRY_")
  TelemetryConfigManager::add_source(config_manager, env_config_source)
  
  // Add remote configuration source
  let remote_config_source = RemoteConfigSource::new("https://config.example.com/telemetry")
    .with_refresh_interval(300000) // 5 minutes
    .with_timeout(5000)
  
  TelemetryConfigManager::add_source(config_manager, remote_config_source)
  
  // Test configuration loading
  let config = TelemetryConfigManager::load_config(config_manager)
  assert_true(TelemetryConfig::is_valid(config))
  
  // Test configuration values
  match TelemetryConfig::get_string(config, "service.name") {
    Some(name) => assert_eq(name, "azimuth-telemetry")
    None => assert_true(false)
  }
  
  match TelemetryConfig::get_int(config, "service.port") {
    Some(port) => assert_eq(port, 8080)
    None => assert_true(false)
  }
  
  match TelemetryConfig::get_bool(config, "tracing.enabled") {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  // Test configuration validation
  let config_validator = TelemetryConfigValidator::new()
  
  ConfigValidator::add_rule(config_validator, "service.port", ConfigRule::int_range(1, 65535))
  ConfigValidator::add_rule(config_validator, "sampling.rate", ConfigRule::float_range(0.0, 1.0))
  ConfigValidator::add_rule(config_validator, "collector.endpoint", ConfigRule::url_format())
  
  let validation_result = ConfigValidator::validate(config_validator, config)
  assert_true(validation_result.is_valid)
  
  // Test configuration hot reload
  let hot_reload_manager = HotReloadConfigManager::new(config_manager)
    .with_watch_files(true)
    .with_poll_interval(10000) // 10 seconds
  
  HotReloadConfigManager::start_watching(hot_reload_manager)
  assert_true(HotReloadConfigManager::is_watching(hot_reload_manager))
  
  // Simulate configuration change
  let change_listener = TestConfigChangeListener::new()
  HotReloadConfigManager::add_listener(hot_reload_manager, change_listener)
  
  // Simulate file change
  HotReloadConfigManager::simulate_file_change(hot_reload_manager, "/etc/telemetry/config.json")
  
  // Verify change was detected
  assert_true(TestConfigChangeListener::was_notified(change_listener))
  
  // Test configuration profiles
  let profile_manager = ConfigProfileManager::new()
  
  ProfileManager::add_profile(profile_manager, "development", [
    ("log.level", "debug"),
    ("sampling.rate", "1.0"),
    ("metrics.export.interval", "10000")
  ])
  
  ProfileManager::add_profile(profile_manager, "production", [
    ("log.level", "info"),
    ("sampling.rate", "0.1"),
    ("metrics.export.interval", "60000")
  ])
  
  ProfileManager::set_active_profile(profile_manager, "development")
  assert_eq(ProfileManager::get_active_profile(profile_manager), "development")
  
  match ProfileManager::get_setting(profile_manager, "log.level") {
    Some(level) => assert_eq(level, "debug")
    None => assert_true(false)
  }
  
  // Test configuration encryption
  let encrypted_config_manager = EncryptedConfigManager::new("encryption-key-12345")
  
  EncryptedConfigManager::set_string(encrypted_config_manager, "api.key", "secret-api-key")
  EncryptedConfigManager::set_string(encrypted_config_manager, "database.password", "secret-password")
  
  match EncryptedConfigManager::get_string(encrypted_config_manager, "api.key") {
    Some(key) => assert_eq(key, "secret-api-key")
    None => assert_true(false)
  }
  
  // Test configuration export/import
  let config_exporter = ConfigExporter::new()
  let exported_config = ConfigExporter::export_to_json(config_exporter, config)
  assert_true(exported_config.length() > 0)
  
  let config_importer = ConfigImporter::new()
  let imported_config = ConfigImporter::import_from_json(config_importer, exported_config)
  assert_true(TelemetryConfig::is_valid(imported_config))
}

// Test 9: Telemetry Dashboard Integration
test "telemetry dashboard integration" {
  // Test dashboard data provider
  let dashboard_provider = DashboardDataProvider::new()
    .with_cache_duration(60000) // 1 minute cache
    .with_max_series(1000)
  
  // Add time series data
  let cpu_series = TimeSeries::new("cpu_usage", "percentage")
  let memory_series = TimeSeries::new("memory_usage", "percentage")
  let disk_series = TimeSeries::new("disk_io", "megabytes_per_second")
  
  // Add data points for the last hour
  let base_time = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  for i in 0..60 {
    let timestamp = base_time + (i * 60000) // 1-minute intervals
    
    TimeSeries::add_point(cpu_series, timestamp, 50.0 + 20.0 * ((i as Float) / 60.0))
    TimeSeries::add_point(memory_series, timestamp, 60.0 + 15.0 * ((i as Float) / 60.0))
    TimeSeries::add_point(disk_series, timestamp, 100.0 + 50.0 * ((i as Float) / 60.0))
  }
  
  DashboardDataProvider::add_series(dashboard_provider, cpu_series)
  DashboardDataProvider::add_series(dashboard_provider, memory_series)
  DashboardDataProvider::add_series(dashboard_provider, disk_series)
  
  // Test dashboard query API
  let query = DashboardQuery::new()
    .with_time_range(base_time, base_time + 3600000) // 1 hour
    .with_series_filter("cpu_usage")
    .with_aggregation("avg")
    .with_interval(300000) // 5-minute intervals
  
  let query_result = DashboardDataProvider::query(dashboard_provider, query)
  assert_eq(query_result.series.length(), 1)
  assert_eq(query_result.series[0].name, "cpu_usage")
  assert_eq(query_result.series[0].points.length(), 12) // 60 minutes / 5-minute intervals
  
  // Test dashboard alert configuration
  let alert_config = DashboardAlertConfig::new()
    .with_threshold("cpu_usage", 80.0, "greater_than")
    .with_threshold("memory_usage", 90.0, "greater_than")
    .with_threshold("disk_io", 200.0, "greater_than")
    .with_notification_channels(["email", "slack"])
  
  DashboardAlertConfig::add_recipient(alert_config, "email", "admin@example.com")
  DashboardAlertConfig::add_recipient(alert_config, "slack", "#telemetry-alerts")
  
  // Test alert evaluation
  let alert_evaluator = DashboardAlertEvaluator::new(alert_config)
  
  // Add data points that would trigger alerts
  TimeSeries::add_point(cpu_series, base_time + 3600000, 85.0) // Above threshold
  TimeSeries::add_point(memory_series, base_time + 3600000, 95.0) // Above threshold
  
  let alert_results = DashboardAlertEvaluator::evaluate(alert_evaluator, dashboard_provider)
  assert_eq(alert_results.length(), 2) // cpu_usage and memory_usage alerts
  
  // Test dashboard widget configuration
  let widget_config = DashboardWidgetConfig::new()
  
  let cpu_chart_widget = WidgetConfig::chart("CPU Usage")
    .with_series("cpu_usage")
    .with_chart_type("line")
    .with_time_range("1h")
    .with_refresh_interval(30000) // 30 seconds
  
  let memory_gauge_widget = WidgetConfig::gauge("Memory Usage")
    .with_series("memory_usage")
    .with_min_value(0.0)
    .with_max_value(100.0)
    .with_thresholds([70.0, 85.0, 95.0])
  
  let top_spans_widget = WidgetConfig::table("Top Spans by Duration")
    .with_query("SELECT span_name, avg(duration) FROM spans GROUP BY span_name ORDER BY avg(duration) DESC LIMIT 10")
    .with_columns(["span_name", "avg_duration"])
  
  DashboardWidgetConfig::add_widget(widget_config, cpu_chart_widget)
  DashboardWidgetConfig::add_widget(widget_config, memory_gauge_widget)
  DashboardWidgetConfig::add_widget(widget_config, top_spans_widget)
  
  assert_eq(DashboardWidgetConfig::widget_count(widget_config), 3)
  
  // Test dashboard rendering
  let dashboard_renderer = DashboardRenderer::new()
    .with_theme("dark")
    .with_width(1200)
    .with_height(800)
  
  let rendered_dashboard = DashboardRenderer::render(dashboard_renderer, widget_config, dashboard_provider)
  assert_true(rendered_dashboard.length() > 0)
  
  // Test dashboard export
  let dashboard_exporter = DashboardExporter::new()
  
  let pdf_export = DashboardExporter::export_to_pdf(dashboard_exporter, rendered_dashboard)
  assert_true(pdf_export.length() > 0)
  
  let png_export = DashboardExporter::export_to_png(dashboard_exporter, rendered_dashboard)
  assert_true(png_export.length() > 0)
}

// Test 10: Telemetry Security and Privacy
test "telemetry security and privacy" {
  // Test sensitive data detection
  let sensitive_data_detector = SensitiveDataDetector::new()
    .with_credit_card_detection(true)
    .with_email_detection(true)
    .with_phone_detection(true)
    .with_ssn_detection(true)
  
  let telemetry_data = TelemetryData::new()
    .with_attribute("user.email", "user@example.com")
    .with_attribute("user.phone", "+1234567890")
    .with_attribute("payment.card", "4111-1111-1111-1111")
    .with_attribute("user.id", "12345")
    .with_attribute("request.path", "/api/users")
  
  let detection_result = SensitiveDataDetector::detect(sensitive_data_detector, telemetry_data)
  assert_eq(detection_result.sensitive_fields.length(), 3) // email, phone, credit card
  assert_true(detection_result.sensitive_fields.contains("user.email"))
  assert_true(detection_result.sensitive_fields.contains("user.phone"))
  assert_true(detection_result.sensitive_fields.contains("payment.card"))
  
  // Test data redaction
  let data_redactor = DataRedactor::new()
    .with_redaction_strategy("partial") // Show first and last characters
    .with_redaction_char("*")
  
  let redacted_data = DataRedactor::redact(data_redactor, telemetry_data, detection_result.sensitive_fields)
  
  match TelemetryData::get_attribute(redacted_data, "user.email") {
    Some(email) => assert_eq(email, "u***l@example.com")
    None => assert_true(false)
  }
  
  match TelemetryData::get_attribute(redacted_data, "user.phone") {
    Some(phone) => assert_eq(phone, "+1******90")
    None => assert_true(false)
  }
  
  match TelemetryData::get_attribute(redacted_data, "payment.card") {
    Some(card) => assert_eq(card, "4***1")
    None => assert_true(false)
  }
  
  // Non-sensitive fields should remain unchanged
  match TelemetryData::get_attribute(redacted_data, "user.id") {
    Some(id) => assert_eq(id, "12345")
    None => assert_true(false)
  }
  
  // Test data encryption
  let encryption_manager = EncryptionManager::new("encryption-key-12345")
    .with_algorithm("aes-256-gcm")
  
  let sensitive_fields = ["user.email", "user.phone", "payment.card"]
  let encrypted_data = EncryptionManager::encrypt_fields(encryption_manager, telemetry_data, sensitive_fields)
  
  // Encrypted fields should be unreadable
  match TelemetryData::get_attribute(encrypted_data, "user.email") {
    Some(encrypted_email) => assert_true(encrypted_email.length() > 0)
    None => assert_true(false)
  }
  
  // Test data decryption
  let decrypted_data = EncryptionManager::decrypt_fields(encryption_manager, encrypted_data, sensitive_fields)
  
  match TelemetryData::get_attribute(decrypted_data, "user.email") {
    Some(email) => assert_eq(email, "user@example.com")
    None => assert_true(false)
  }
  
  // Test data access control
  let access_controller = DataAccessController::new()
  
  // Define access policies
  AccessController::add_policy(access_controller, "admin", ["*"]) // Full access
  AccessController::add_policy(access_controller, "analyst", ["user.id", "request.path"]) // Limited access
  AccessController::add_policy(access_controller, "viewer", ["request.path"]) // Minimal access
  
  // Test access control for different roles
  let admin_access = AccessController::check_access(access_controller, "admin", telemetry_data)
  assert_true(admin_access.can_access_all)
  
  let analyst_access = AccessController::check_access(access_controller, "analyst", telemetry_data)
  assert_false(analyst_access.can_access_all)
  assert_true(analyst_access.allowed_fields.contains("user.id"))
  assert_true(analyst_access.allowed_fields.contains("request.path"))
  assert_false(analyst_access.allowed_fields.contains("user.email"))
  
  let viewer_access = AccessController::check_access(access_controller, "viewer", telemetry_data)
  assert_false(viewer_access.can_access_all)
  assert_true(viewer_access.allowed_fields.contains("request.path"))
  assert_false(viewer_access.allowed_fields.contains("user.id"))
  
  // Test data anonymization
  let anonymizer = DataAnonymizer::new()
    .with_hash_salt("anonymization-salt")
    .with_pseudonymization(true)
  
  let anonymized_data = DataAnonymizer::anonymize(anonymizer, telemetry_data)
  
  // Pseudonymized fields should be consistent but not reveal original values
  match TelemetryData::get_attribute(anonymized_data, "user.id") {
    Some(anonymized_id) => {
      assert_true(anonymized_id.length() > 0)
      assert_false(anonymized_id == "12345")
    }
    None => assert_true(false)
  }
  
  // Test data retention based on privacy regulations
  let privacy_retention_manager = PrivacyRetentionManager::new()
    .with_gdpr_retention_days(365) // GDPR: 1 year
    .with_ccpa_retention_days(365) // CCPA: 1 year
    .with_pii_auto_delete(true)
  
  let pii_data = PIIContainer::new()
    .with_field("user.email", "user@example.com")
    .with_field("user.phone", "+1234567890")
    .with_timestamp(1640995200000) // 2022-01-01
  
  // Test if data should be retained
  let current_time = 1640995200000 + (400 * 86400000) // 400 days later
  let should_retain = PrivacyRetentionManager::should_retain(privacy_retention_manager, pii_data, current_time)
  assert_false(should_retain) // Should be deleted after 365 days
  
  // Test consent management
  let consent_manager = ConsentManager::new()
  
  ConsentManager::record_consent(consent_manager, "user-123", "analytics", true)
  ConsentManager::record_consent(consent_manager, "user-123", "marketing", false)
  ConsentManager::record_consent(consent_manager, "user-123", "personalization", true)
  
  assert_true(ConsentManager::has_consent(consent_manager, "user-123", "analytics"))
  assert_false(ConsentManager::has_consent(consent_manager, "user-123", "marketing"))
  assert_true(ConsentManager::has_consent(consent_manager, "user-123", "personalization"))
  
  // Test consent withdrawal
  ConsentManager::withdraw_consent(consent_manager, "user-123", "analytics")
  assert_false(ConsentManager::has_consent(consent_manager, "user-123", "analytics"))
  
  // Test data deletion based on consent withdrawal
  let consent_deletion_manager = ConsentDeletionManager::new()
  let deletion_result = ConsentDeletionManager::delete_user_data(consent_deletion_manager, "user-123")
  assert_true(deletion_result.success)
  assert_true(deletion_result.deleted_records > 0)
}