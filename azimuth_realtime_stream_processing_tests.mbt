// Real-time Stream Processing Tests for Azimuth Telemetry System
// This file contains test cases for real-time stream processing capabilities

// Test 1: Real-time Data Ingestion
test "real-time data ingestion" {
  let stream_processor = StreamProcessor::new()
  
  // Configure stream processor
  stream_processor.configure(StreamConfig::new()
    .with_buffer_size(1000)
    .with_batch_size(100)
    .with_processing_interval(50))
  
  // Start stream processor
  stream_processor.start()
  
  // Generate real-time telemetry data
  let telemetry_stream = generate_realtime_telemetry_stream(500)
  
  // Ingest data into stream processor
  let ingestion_results = []
  for data_point in telemetry_stream {
    let result = stream_processor.ingest(data_point)
    ingestion_results.push(result)
  }
  
  // Verify ingestion results
  assert_eq(ingestion_results.length(), 500)
  
  let successful_ingestions = ingestion_results.filter(|r| r.success).length()
  assert_true(successful_ingestions > 480) // At least 96% success rate
  
  // Verify processing metrics
  let metrics = stream_processor.get_metrics()
  assert_true(metrics.total_ingested > 480)
  assert_true(metrics.processing_latency_ms < 100)
  assert_true(metrics.buffer_utilization < 0.9)
  
  // Stop stream processor
  stream_processor.stop()
}

// Test 2: Stream Window Operations
test "stream window operations" {
  let stream_processor = StreamProcessor::new()
  
  // Configure window operations
  let tumbling_window = WindowConfig::tumbling(1000) // 1 second windows
  let sliding_window = WindowConfig::sliding(500, 100) // 500ms window, 100ms slide
  let session_window = WindowConfig::session(2000) // 2 second session timeout
  
  stream_processor.add_window_operation("tumbling_avg", WindowOperation::average(tumbling_window))
  stream_processor.add_window_operation("sliding_max", WindowOperation::maximum(sliding_window))
  stream_processor.add_window_operation("session_count", WindowOperation::count(session_window))
  
  // Start stream processor
  stream_processor.start()
  
  // Generate time-series telemetry data
  let time_series_data = generate_time_series_stream(2000, 10) // 2000ms duration, 10ms intervals
  
  // Process data through windows
  let window_results = []
  for data_point in time_series_data {
    let results = stream_processor.process_windows(data_point)
    window_results.push(results)
  }
  
  // Verify window operations
  assert_true(window_results.length() > 0)
  
  // Check tumbling window averages
  let tumbling_results = window_results.flat_map(|r| r).filter(|r| r.operation == "tumbling_avg")
  assert_true(tumbling_results.length() > 0)
  
  for result in tumbling_results {
    assert_true(result.value >= 0.0)
    assert_true(result.window_start < result.window_end)
    assert_true(result.window_end - result.window_start == 1000) // 1 second window
  }
  
  // Check sliding window maximums
  let sliding_results = window_results.flat_map(|r| r).filter(|r| r.operation == "sliding_max")
  assert_true(sliding_results.length() > 0)
  
  for result in sliding_results {
    assert_true(result.value >= 0.0)
    assert_true(result.window_end - result.window_start == 500) // 500ms window
  }
  
  // Check session window counts
  let session_results = window_results.flat_map(|r| r).filter(|r| r.operation == "session_count")
  assert_true(session_results.length() > 0)
  
  for result in session_results {
    assert_true(result.value >= 1.0) // At least 1 item per session
    assert_true(result.window_start < result.window_end)
  }
  
  // Stop stream processor
  stream_processor.stop()
}

// Test 3: Stream Aggregation and Analytics
test "stream aggregation and analytics" {
  let stream_processor = StreamProcessor::new()
  
  // Configure aggregations
  stream_processor.add_aggregation("avg_response_time", Aggregation::average("response_time"))
  stream_processor.add_aggregation("error_rate", Aggregation::rate("error", "total_requests"))
  stream_processor.add_aggregation("throughput", Aggregation::sum("request_count"))
  stream_processor.add_aggregation("p95_latency", Aggregation::percentile("latency", 95))
  
  // Start stream processor
  stream_processor.start()
  
  // Generate streaming telemetry with various metrics
  let stream_data = generate_multi_metric_stream(1000)
  
  // Process stream data
  let aggregation_results = []
  for data_point in stream_data {
    let results = stream_processor.aggregate(data_point)
    aggregation_results.push(results)
  }
  
  // Verify aggregation results
  assert_true(aggregation_results.length() > 0)
  
  // Check average response time
  let avg_results = aggregation_results.flat_map(|r| r).filter(|r| r.metric == "avg_response_time")
  assert_true(avg_results.length() > 0)
  
  for result in avg_results {
    assert_true(result.value >= 0.0)
    assert_true(result.timestamp > 0)
  }
  
  // Check error rate
  let error_rate_results = aggregation_results.flat_map(|r| r).filter(|r| r.metric == "error_rate")
  assert_true(error_rate_results.length() > 0)
  
  for result in error_rate_results {
    assert_true(result.value >= 0.0 && result.value <= 1.0) // Rate should be between 0 and 1
  }
  
  // Check throughput
  let throughput_results = aggregation_results.flat_map(|r| r).filter(|r| r.metric == "throughput")
  assert_true(throughput_results.length() > 0)
  
  for result in throughput_results {
    assert_true(result.value >= 0.0)
  }
  
  // Check 95th percentile latency
  let p95_results = aggregation_results.flat_map(|r| r).filter(|r| r.metric == "p95_latency")
  assert_true(p95_results.length() > 0)
  
  for result in p95_results {
    assert_true(result.value >= 0.0)
  }
  
  // Stop stream processor
  stream_processor.stop()
}

// Test 4: Stream Filtering and Transformation
test "stream filtering and transformation" {
  let stream_processor = StreamProcessor::new()
  
  // Configure filters
  stream_processor.add_filter("high_latency", Filter::greater_than("latency", 1000))
  stream_processor.add_filter("error_only", Filter::equals("status", "error"))
  stream_processor.add_filter("critical_services", Filter::in_list("service", ["auth", "payment", "database"]))
  
  // Configure transformations
  stream_processor.add_transformation("normalize_latency", Transform::normalize("latency", 0.0, 5000.0))
  stream_processor.add_transformation("extract_timestamp", Transform::extract_field("timestamp", "epoch"))
  stream_processor.add_transformation("calculate_duration", Transform::calculate("end_time", "start_time", "subtract"))
  
  // Start stream processor
  stream_processor.start()
  
  // Generate diverse stream data
  let diverse_data = generate_diverse_stream_data(800)
  
  // Process data through filters and transformations
  let processed_results = []
  for data_point in diverse_data {
    let filtered_data = stream_processor.apply_filters(data_point)
    if filtered_data.is_some() {
      let transformed_data = stream_processor.apply_transformations(filtered_data.unwrap())
      processed_results.push(transformed_data)
    }
  }
  
  // Verify filtering results
  assert_true(processed_results.length() > 0)
  assert_true(processed_results.length() < diverse_data.length()) // Some data should be filtered out
  
  // Verify high latency filter
  let high_latency_count = diverse_data.filter(|d| d.get("latency") > Some(1000)).length()
  let processed_high_latency = processed_results.filter(|d| d.get("latency") > Some(1000)).length()
  assert_eq(high_latency_count, processed_high_latency)
  
  // Verify error-only filter
  let error_data = processed_results.filter(|d| d.get("status") == Some("error"))
  for error in error_data {
    assert_eq(error.get("status"), Some("error"))
  }
  
  // Verify critical services filter
  let critical_services = ["auth", "payment", "database"]
  let service_data = processed_results.filter(|d| {
    match d.get("service") {
      Some(service) => critical_services.contains(service),
      None => false
    }
  })
  
  for service in service_data {
    match service.get("service") {
      Some(service_name) => assert_true(critical_services.contains(service_name)),
      None => assert_true(false)
    }
  }
  
  // Verify transformations
  for transformed in processed_results {
    // Check normalized latency
    match transformed.get("latency") {
      Some(normalized_latency) => {
        assert_true(normalized_latency >= 0.0 && normalized_latency <= 1.0)
      },
      None => {}
    }
    
    // Check extracted timestamp
    assert_true(transformed.contains_key("epoch"))
    
    // Check calculated duration
    if transformed.contains_key("end_time") && transformed.contains_key("start_time") {
      assert_true(transformed.contains_key("duration"))
    }
  }
  
  // Stop stream processor
  stream_processor.stop()
}

// Test 5: Stream Anomaly Detection
test "stream anomaly detection" {
  let stream_processor = StreamProcessor::new()
  
  // Configure anomaly detection
  let stat_detector = AnomalyDetector::statistical("z_score", 2.5)
  let ml_detector = AnomalyDetector::machine_learning("isolation_forest", 0.1)
  let pattern_detector = AnomalyDetector::pattern_based("sequential", 0.05)
  
  stream_processor.add_anomaly_detector("statistical", stat_detector)
  stream_processor.add_anomaly_detector("ml_based", ml_detector)
  stream_processor.add_anomaly_detector("pattern_based", pattern_detector)
  
  // Start stream processor
  stream_processor.start()
  
  // Generate normal stream data
  let normal_data = generate_normal_stream_data(500)
  
  // Train anomaly detectors
  for data_point in normal_data {
    stream_processor.train_anomaly_detectors(data_point)
  }
  
  // Generate mixed stream data (normal + anomalies)
  let mixed_data = generate_mixed_stream_data(500, 0.1) // 10% anomalies
  
  // Detect anomalies
  let anomaly_results = []
  for data_point in mixed_data {
    let anomalies = stream_processor.detect_anomalies(data_point)
    anomaly_results.push(anomalies)
  }
  
  // Verify anomaly detection results
  assert_eq(anomaly_results.length(), 500)
  
  let total_anomalies = anomaly_results.flat_map(|a| a).length()
  assert_true(total_anomalies > 20) // Should detect some anomalies
  assert_true(total_anomalies < 150) // But not too many false positives
  
  // Verify anomaly properties
  for anomalies in anomaly_results {
    for anomaly in anomalies {
      assert_true(anomaly.detector_name.length() > 0)
      assert_true(anomaly.anomaly_score > 0.5)
      assert_true(anomaly.confidence >= 0.0 && anomaly.confidence <= 1.0)
      assert_true(anomaly.timestamp > 0)
    }
  }
  
  // Check ensemble anomaly detection
  let ensemble_results = stream_detector.detect_ensemble_anomalies(mixed_data)
  assert_eq(ensemble_results.length(), 500)
  
  let ensemble_anomalies = ensemble_results.filter(|r| r.is_anomaly).length()
  assert_true(ensemble_anomalies > 20 && ensemble_anomalies < 150)
  
  // Verify ensemble provides better confidence
  for result in ensemble_results {
    if result.is_anomaly {
      assert_true(result.ensemble_score > 0.6)
      assert_true(result.confidence > 0.7)
      assert_true(result.detector_agreement > 0.5)
    }
  }
  
  // Stop stream processor
  stream_processor.stop()
}

// Test 6: Stream Backpressure and Flow Control
test "stream backpressure and flow control" {
  let stream_processor = StreamProcessor::new()
  
  // Configure flow control
  stream_processor.configure_flow_control(FlowControlConfig::new()
    .with_max_buffer_size(500)
    .with_backpressure_threshold(0.8)
    .with_slow_consumer_threshold(100))
  
  // Start stream processor
  stream_processor.start()
  
  // Simulate slow consumer
  let slow_consumer = SlowConsumer::new(50) // 50ms processing delay
  
  // Generate high-volume stream data
  let high_volume_data = generate_high_volume_stream_data(2000)
  
  // Process data with backpressure
  let processing_results = []
  let backpressure_events = []
  
  for data_point in high_volume_data {
    // Check for backpressure
    if stream_processor.is_backpressure_active() {
      backpressure_events.push(stream_processor.get_backpressure_info())
    }
    
    // Process data point
    let result = stream_processor.process_with_flow_control(data_point, slow_consumer)
    processing_results.push(result)
  }
  
  // Verify backpressure handling
  assert_true(backpressure_events.length() > 0) // Should have triggered backpressure
  
  for event in backpressure_events {
    assert_true(event.buffer_utilization > 0.8)
    assert_true(event.is_active)
    assert_true(event.strategy.length() > 0)
  }
  
  // Verify processing results
  assert_eq(processing_results.length(), 2000)
  
  let successful_processes = processing_results.filter(|r| r.success).length()
  assert_true(successful_processes > 1800) // At least 90% success rate
  
  // Verify no buffer overflow
  let metrics = stream_processor.get_metrics()
  assert_true(metrics.buffer_overflow_count == 0)
  assert_true(metrics.max_buffer_utilization <= 1.0)
  
  // Test different backpressure strategies
  let strategies = ["drop_oldest", "drop_newest", "batch_process", "throttle"]
  
  for strategy in strategies {
    stream_processor.set_backpressure_strategy(strategy)
    
    let strategy_data = generate_high_volume_stream_data(500)
    let strategy_results = []
    
    for data_point in strategy_data {
      let result = stream_processor.process_with_flow_control(data_point, slow_consumer)
      strategy_results.push(result)
    }
    
    // Verify strategy-specific behavior
    match strategy {
      "drop_oldest" => {
        // Should drop oldest data when buffer is full
        assert_true(strategy_results.filter(|r| r.dropped).length() > 0)
      },
      "drop_newest" => {
        // Should drop newest data when buffer is full
        assert_true(strategy_results.filter(|r| r.dropped).length() > 0)
      },
      "batch_process" => {
        // Should process data in batches
        let batch_count = strategy_results.filter(|r| r.batched).length()
        assert_true(batch_count > 0)
      },
      "throttle" => {
        // Should throttle processing rate
        let throttled_count = strategy_results.filter(|r| r.throttled).length()
        assert_true(throttled_count > 0)
      },
      _ => assert_true(false)
    }
  }
  
  // Stop stream processor
  stream_processor.stop()
}

// Test 7: Stream State Management and Recovery
test "stream state management and recovery" {
  let stream_processor = StreamProcessor::new()
  
  // Configure state management
  stream_processor.configure_state_management(StateConfig::new()
    .with_checkpoint_interval(1000)
    .with_state_backend("memory")
    .with_recovery_strategy("exactly_once"))
  
  // Start stream processor
  stream_processor.start()
  
  // Generate stream data with state
  let stateful_data = generate_stateful_stream_data(1500)
  
  // Process data and update state
  let state_updates = []
  for (i, data_point) in stateful_data.enumerate() {
    let result = stream_processor.process_with_state(data_point)
    state_updates.push(result)
    
    // Create checkpoints periodically
    if i % 100 == 0 {
      stream_processor.create_checkpoint()
    }
  }
  
  // Verify state management
  assert_eq(state_updates.length(), 1500)
  
  let successful_updates = state_updates.filter(|u| u.success).length()
  assert_true(successful_updates > 1400) // At least 93% success rate
  
  // Verify checkpoint creation
  let checkpoints = stream_processor.get_checkpoints()
  assert_true(checkpoints.length() > 10)
  
  for checkpoint in checkpoints {
    assert_true(checkpoint.checkpoint_id.length() > 0)
    assert_true(checkpoint.timestamp > 0)
    assert_true(checkpoint.state_size > 0)
  }
  
  // Simulate failure and recovery
  let failure_point = 750
  let pre_failure_state = stream_processor.get_current_state()
  
  // Process some data after failure point
  let post_failure_data = stateful_data.slice(failure_point, stateful_data.length())
  let post_failure_results = []
  
  for data_point in post_failure_data {
    let result = stream_processor.process_with_state(data_point)
    post_failure_results.push(result)
  }
  
  // Trigger recovery
  let recovery_checkpoint = checkpoints[checkpoints.length() / 2] // Use middle checkpoint
  let recovery_result = stream_processor.recover_from_checkpoint(recovery_checkpoint.checkpoint_id)
  
  assert_true(recovery_result.success)
  
  // Verify recovery state
  let recovered_state = stream_processor.get_current_state()
  assert_eq(recovered_state.checkpoint_id, recovery_checkpoint.checkpoint_id)
  
  // Re-process data from recovery point
  let recovery_data = stateful_data.slice(failure_point, stateful_data.length())
  let recovery_results = []
  
  for data_point in recovery_data {
    let result = stream_processor.process_with_state(data_point)
    recovery_results.push(result)
  }
  
  // Verify recovery results match original results
  assert_eq(recovery_results.length(), post_failure_results.length())
  
  for (i, recovery_result) in recovery_results.enumerate() {
    let original_result = post_failure_results[i]
    assert_eq(recovery_result.success, original_result.success)
    
    if recovery_result.success && original_result.success {
      assert_eq(recovery_result.output, original_result.output)
    }
  }
  
  // Test different recovery strategies
  let strategies = ["at_least_once", "exactly_once", "at_most_once"]
  
  for strategy in strategies {
    stream_processor.set_recovery_strategy(strategy)
    
    let strategy_data = generate_stateful_stream_data(200)
    let strategy_results = []
    
    // Create checkpoint
    stream_processor.create_checkpoint()
    
    // Process data
    for data_point in strategy_data {
      let result = stream_processor.process_with_state(data_point)
      strategy_results.push(result)
    }
    
    // Simulate failure and recovery
    let strategy_checkpoint = stream_processor.get_latest_checkpoint()
    let strategy_recovery = stream_processor.recover_from_checkpoint(strategy_checkpoint.checkpoint_id)
    
    assert_true(strategy_recovery.success)
    
    // Verify strategy-specific behavior
    match strategy {
      "at_least_once" => {
        // May process some data multiple times
        assert_true(strategy_results.filter(|r| r.duplicate).length() >= 0)
      },
      "exactly_once" => {
        // Should process each data point exactly once
        assert_eq(strategy_results.filter(|r| r.duplicate).length(), 0)
        assert_eq(strategy_results.filter(|r| r.missed).length(), 0)
      },
      "at_most_once" => {
        // May miss some data but never process duplicates
        assert_eq(strategy_results.filter(|r| r.duplicate).length(), 0)
      },
      _ => assert_true(false)
    }
  }
  
  // Stop stream processor
  stream_processor.stop()
}

// Test 8: Stream Scaling and Load Distribution
test "stream scaling and load distribution" {
  let stream_processor = StreamProcessor::new()
  
  // Configure scaling
  stream_processor.configure_scaling(ScalingConfig::new()
    .with_min_partitions(2)
    .with_max_partitions(8)
    .with_scale_up_threshold(0.8)
    .with_scale_down_threshold(0.3)
    .with_auto_scale_enabled(true))
  
  // Start stream processor
  stream_processor.start()
  
  // Generate variable load stream data
  let low_load_data = generate_stream_data_with_load(200, "low")
  let medium_load_data = generate_stream_data_with_load(400, "medium")
  let high_load_data = generate_stream_data_with_load(600, "high")
  
  // Process data with varying load
  let scaling_events = []
  
  // Process low load data
  for data_point in low_load_data {
    let result = stream_processor.process_with_scaling(data_point)
    
    if stream_processor.has_scaling_event_occurred() {
      scaling_events.push(stream_processor.get_latest_scaling_event())
    }
  }
  
  // Process medium load data
  for data_point in medium_load_data {
    let result = stream_processor.process_with_scaling(data_point)
    
    if stream_processor.has_scaling_event_occurred() {
      scaling_events.push(stream_processor.get_latest_scaling_event())
    }
  }
  
  // Process high load data
  for data_point in high_load_data {
    let result = stream_processor.process_with_scaling(data_point)
    
    if stream_processor.has_scaling_event_occurred() {
      scaling_events.push(stream_processor.get_latest_scaling_event())
    }
  }
  
  // Verify scaling events
  assert_true(scaling_events.length() > 0)
  
  for event in scaling_events {
    assert_true(event.event_type == "scale_up" || event.event_type == "scale_down")
    assert_true(event.old_partition_count > 0)
    assert_true(event.new_partition_count > 0)
    assert_true(event.timestamp > 0)
    assert_true(event.reason.length() > 0)
  }
  
  // Verify partition management
  let partitions = stream_processor.get_partitions()
  assert_true(partitions.length() >= 2) // At least min partitions
  assert_true(partitions.length() <= 8) // At most max partitions
  
  for partition in partitions {
    assert_true(partition.partition_id >= 0)
    assert_true(partition.current_load >= 0.0)
    assert_true(partition.max_capacity > 0.0)
    assert_true(partition.is_active)
  }
  
  // Test load distribution strategies
  let strategies = ["round_robin", "hash_based", "load_balanced"]
  
  for strategy in strategies {
    stream_processor.set_load_distribution_strategy(strategy)
    
    let strategy_data = generate_stream_data_with_load(300, "medium")
    let strategy_results = []
    
    for data_point in strategy_data {
      let result = stream_processor.process_with_scaling(data_point)
      strategy_results.push(result)
    }
    
    // Verify strategy-specific behavior
    let partition_usage = stream_processor.get_partition_usage()
    
    match strategy {
      "round_robin" => {
        // Should distribute evenly across partitions
        let max_usage = partition_usage.reduce(0.0, |max, usage| if usage > max { usage } else { max })
        let min_usage = partition_usage.reduce(max_usage, |min, usage| if usage < min { usage } else { min })
        assert_true(max_usage - min_usage < 0.2) // Fairly even distribution
      },
      "hash_based" => {
        // Should distribute based on data key
        assert_true(partition_usage.length() > 0)
      },
      "load_balanced" => {
        // Should distribute based on current load
        let max_usage = partition_usage.reduce(0.0, |max, usage| if usage > max { usage } else { max })
        let min_usage = partition_usage.reduce(max_usage, |min, usage| if usage < min { usage } else { min })
        assert_true(max_usage - min_usage < 0.3) // Reasonably balanced
      },
      _ => assert_true(false)
    }
  }
  
  // Stop stream processor
  stream_processor.stop()
}

// Test 9: Stream Monitoring and Metrics
test "stream monitoring and metrics" {
  let stream_processor = StreamProcessor::new()
  
  // Configure monitoring
  stream_processor.configure_monitoring(MonitoringConfig::new()
    .with_metrics_collection_interval(100)
    .with_performance_tracking_enabled(true)
    .with_health_check_enabled(true)
    .with_alert_thresholds(AlertThresholds::new()
      .with_latency_threshold(500)
      .with_error_rate_threshold(0.05)
      .with_throughput_threshold(1000)))
  
  // Start stream processor
  stream_processor.start()
  
  // Generate stream data for monitoring
  let monitoring_data = generate_monitoring_stream_data(1000)
  
  // Process data and collect metrics
  let processing_results = []
  let metrics_snapshots = []
  
  for (i, data_point) in monitoring_data.enumerate() {
    let result = stream_processor.process_with_monitoring(data_point)
    processing_results.push(result)
    
    // Collect metrics periodically
    if i % 100 == 0 {
      let metrics = stream_processor.get_metrics_snapshot()
      metrics_snapshots.push(metrics)
    }
  }
  
  // Verify processing results
  assert_eq(processing_results.length(), 1000)
  
  let successful_processes = processing_results.filter(|r| r.success).length()
  assert_true(successful_processes > 950) // At least 95% success rate
  
  // Verify metrics collection
  assert_true(metrics_snapshots.length() > 5)
  
  for metrics in metrics_snapshots {
    assert_true(metrics.timestamp > 0)
    assert_true(metrics.total_processed >= 0)
    assert_true(metrics.successful_processed >= 0)
    assert_true(metrics.failed_processed >= 0)
    assert_true(metrics.avg_latency_ms >= 0.0)
    assert_true(metrics.max_latency_ms >= metrics.avg_latency_ms)
    assert_true(metrics.throughput_per_second >= 0.0)
    assert_true(metrics.buffer_utilization >= 0.0 && metrics.buffer_utilization <= 1.0)
    assert_true(metrics.error_rate >= 0.0 && metrics.error_rate <= 1.0)
  }
  
  // Verify health checks
  let health_status = stream_processor.get_health_status()
  assert_true(health_status.is_healthy)
  assert_true(health_status.checks.length() > 0)
  
  for check in health_status.checks {
    assert_true(check.name.length() > 0)
    assert_true(check.status == "healthy" || check.status == "unhealthy" || check.status == "degraded")
    assert_true(check.last_check_time > 0)
    assert_true(check.response_time_ms >= 0)
  }
  
  // Verify alert generation
  let alerts = stream_processor.get_alerts()
  assert_true(alerts.length() >= 0)
  
  for alert in alerts {
    assert_true(alert.alert_id.length() > 0)
    assert_true(alert.alert_type.length() > 0)
    assert_true(alert.severity == "low" || alert.severity == "medium" || alert.severity == "high" || alert.severity == "critical")
    assert_true(alert.message.length() > 0)
    assert_true(alert.timestamp > 0)
    assert_true(alert.acknowledged == false || alert.acknowledged == true)
  }
  
  // Test performance benchmarks
  let benchmark_data = generate_benchmark_stream_data(2000)
  let benchmark_start_time = get_current_time_ms()
  
  for data_point in benchmark_data {
    stream_processor.process_with_monitoring(data_point)
  }
  
  let benchmark_end_time = get_current_time_ms()
  let benchmark_duration = benchmark_end_time - benchmark_start_time
  
  // Verify performance benchmarks
  assert_true(benchmark_duration < 10000) // Should complete within 10 seconds
  
  let benchmark_metrics = stream_processor.get_performance_benchmark()
  assert_true(benchmark_metrics.total_processed == 2000)
  assert_true(benchmark_metrics.avg_latency_ms < 100)
  assert_true(benchmark_metrics.max_latency_ms < 1000)
  assert_true(benchmark_metrics.throughput_per_second > 100)
  assert_true(benchmark_metrics.error_rate < 0.01)
  
  // Test metrics export
  let exported_metrics = stream_processor.export_metrics("json")
  assert_true(exported_metrics.length() > 0)
  
  // Verify metrics format
  assert_true(exported_metrics.contains("\"total_processed\""))
  assert_true(exported_metrics.contains("\"avg_latency_ms\""))
  assert_true(exported_metrics.contains("\"throughput_per_second\""))
  
  // Stop stream processor
  stream_processor.stop()
}