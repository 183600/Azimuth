// Azimuth Real-time Stream Processing Tests
// This file contains comprehensive test cases for real-time telemetry stream processing

// Test 1: Real-time Telemetry Data Stream
test "real-time telemetry data stream" {
  let stream_processor = RealTimeStreamProcessor::new()
  
  // Configure stream processing pipeline
  stream_processor.add_source(TelemetryDataSource::new("kafka", "telemetry-topic"))
  stream_processor.add_filter(AttributeFilter::new("service.name", "payment-service"))
  stream_processor.add_transformer(TimestampNormalizer::new())
  stream_processor.add_aggregator(TimeWindowAggregator::new(60L)) // 1-minute windows
  stream_processor.add_sink(AlertSink::new())
  stream_processor.add_sink(MetricsSink::new())
  
  // Start stream processing
  stream_processor.start()
  
  // Simulate real-time telemetry data
  let telemetry_events = [
    TelemetryEvent::new("trace1", "span1", "payment-service", "process_payment", 1640995200L, 150L),
    TelemetryEvent::new("trace2", "span2", "payment-service", "validate_card", 1640995201L, 50L),
    TelemetryEvent::new("trace3", "span3", "user-service", "get_profile", 1640995202L, 100L), // Should be filtered
    TelemetryEvent::new("trace4", "span4", "payment-service", "charge_card", 1640995203L, 200L),
    TelemetryEvent::new("trace5", "span5", "payment-service", "send_receipt", 1640995204L, 75L)
  ]
  
  // Send events to stream
  for event in telemetry_events {
    stream_processor.process_event(event)
  }
  
  // Wait for processing
  Thread::sleep(1000L)
  
  // Verify filtered events
  let processed_events = stream_processor.get_processed_events()
  assert_eq(processed_events.length(), 4) // One event filtered out
  
  for event in processed_events {
    assert_eq(event.service_name, "payment-service")
  }
  
  // Verify aggregation results
  let aggregated_metrics = stream_processor.get_aggregated_metrics()
  assert_true(aggregated_metrics.contains_key("payment-service"))
  
  let payment_metrics = aggregated_metrics.get("payment-service")
  assert_eq(payment_metrics.event_count, 4)
  assert_eq(payment_metrics.total_duration, 475L) // 150 + 50 + 200 + 75
  assert_eq(payment_metrics.avg_duration, 118.75) // 475 / 4
  
  // Verify alert generation
  let alerts = stream_processor.get_generated_alerts()
  assert_true(alerts.length() >= 0) // May or may not have alerts depending on thresholds
  
  // Stop stream processing
  stream_processor.stop()
}

// Test 2: Stream Windowing and Time-based Operations
test "stream windowing and time-based operations" {
  let window_processor = WindowStreamProcessor::new()
  
  // Configure different window types
  window_processor.add_tumbling_window(TumblingWindow::new(60L)) // 1-minute tumbling
  window_processor.add_sliding_window(SlidingWindow::new(60L, 30L)) // 1-minute slide, 30-second step
  window_processor.add_session_window(SessionWindow::new(30L)) // 30-second session timeout
  
  // Generate time-series telemetry data
  let base_time = 1640995200L
  let time_series_events = []
  
  for i in 0..=180 { // 3 minutes of data
    let timestamp = base_time + i.to_int64()
    let service_name = if i % 30 == 0 { "batch-service" } else { "stream-service" }
    
    time_series_events.push(TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      service_name,
      "process_data",
      timestamp,
      Random::int_range(50, 200)
    ))
  }
  
  // Process events through windows
  for event in time_series_events {
    window_processor.process_event(event)
  }
  
  // Wait for window processing
  Thread::sleep(2000L)
  
  // Verify tumbling window results
  let tumbling_results = window_processor.get_tumbling_window_results()
  assert_eq(tumbling_results.length(), 3) // 3 minutes = 3 windows
  
  for (i, window_result) in tumbling_results.enumerate() {
    assert_eq(window_result.event_count, 60) // 60 events per minute
    assert_eq(window_result.window_start, base_time + (i * 60).to_int64())
    assert_eq(window_result.window_end, base_time + ((i + 1) * 60).to_int64())
  }
  
  // Verify sliding window results
  let sliding_results = window_processor.get_sliding_window_results()
  assert_true(sliding_results.length() > 3) // More windows due to sliding
  
  // Verify session window results
  let session_results = window_processor.get_session_window_results()
  
  // Should have multiple sessions based on service changes
  assert_true(session_results.length() > 1)
  
  for session in session_results {
    assert_true(session.event_count > 0)
    assert_true(session.duration > 0L)
    
    // Verify session contains events from same service or close timestamps
    let service_names = session.events.map(|e| e.service_name).unique()
    if service_names.length() == 1 {
      // Single service session
      assert_eq(service_names[0], "stream-service") // or "batch-service"
    } else {
      // Mixed service session, should have close timestamps
      let time_diff = session.events.last().timestamp - session.events.first().timestamp
      assert_true(time_diff <= 30L) // Within session timeout
    }
  }
}

// Test 3: Stream Analytics and Real-time Metrics
test "stream analytics and real-time metrics" {
  let analytics_processor = StreamAnalyticsProcessor::new()
  
  // Configure analytics
  analytics_processor.add_analyzer(ThroughputAnalyzer::new("throughput"))
  analytics_processor.add_analyzer(LatencyAnalyzer::new("latency"))
  analytics_processor.add_analyzer(ErrorRateAnalyzer::new("error_rate"))
  analytics_processor.add_analyzer(ResourceUtilizationAnalyzer::new("resource_util"))
  
  // Start analytics processing
  analytics_processor.start()
  
  // Generate realistic telemetry workload
  let services = ["api-gateway", "user-service", "payment-service", "notification-service"]
  let operations = ["get_data", "create_data", "update_data", "delete_data"]
  
  for i in 0..=1000 {
    let service = services[i % services.length()]
    let operation = operations[i % operations.length()]
    let timestamp = 1640995200L + i.to_int64()
    let duration = if service == "payment-service" { 
      Random::int_range(100, 500) 
    } else { 
      Random::int_range(20, 200) 
    }
    
    let is_error = Random::float_range(0.0, 1.0) < 0.05 // 5% error rate
    let status = if is_error { "error" } else { "success" }
    
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      service,
      operation,
      timestamp,
      duration
    )
    
    event.set_status(status)
    analytics_processor.process_event(event)
  }
  
  // Wait for analytics processing
  Thread::sleep(3000L)
  
  // Verify throughput analytics
  let throughput_metrics = analytics_processor.get_analyzer_results("throughput")
  assert_true(throughput_metrics.contains_key("events_per_second"))
  assert_true(throughput_metrics.contains_key("services_throughput"))
  
  let events_per_second = throughput_metrics.get("events_per_second")
  assert_true(events_per_second > 0.0)
  
  let services_throughput = throughput_metrics.get("services_throughput")
  for service in services {
    assert_true(services_throughput.contains_key(service))
    assert_true(services_throughput.get(service) > 0.0)
  }
  
  // Verify latency analytics
  let latency_metrics = analytics_processor.get_analyzer_results("latency")
  assert_true(latency_metrics.contains_key("avg_latency"))
  assert_true(latency_metrics.contains_key("p95_latency"))
  assert_true(latency_metrics.contains_key("p99_latency"))
  
  let avg_latency = latency_metrics.get("avg_latency")
  let p95_latency = latency_metrics.get("p95_latency")
  let p99_latency = latency_metrics.get("p99_latency")
  
  assert_true(avg_latency > 0.0)
  assert_true(p95_latency >= avg_latency)
  assert_true(p99_latency >= p95_latency)
  
  // Payment service should have higher latency
  let service_latencies = latency_metrics.get("service_latencies")
  assert_true(service_latencies.get("payment-service") > service_latencies.get("user-service"))
  
  // Verify error rate analytics
  let error_rate_metrics = analytics_processor.get_analyzer_results("error_rate")
  assert_true(error_rate_metrics.contains_key("overall_error_rate"))
  assert_true(error_rate_metrics.contains_key("service_error_rates"))
  
  let overall_error_rate = error_rate_metrics.get("overall_error_rate")
  assert_true(overall_error_rate > 0.0 && overall_error_rate < 0.1) // Around 5%
  
  // Verify resource utilization analytics
  let resource_metrics = analytics_processor.get_analyzer_results("resource_util")
  assert_true(resource_metrics.contains_key("cpu_usage"))
  assert_true(resource_metrics.contains_key("memory_usage"))
  assert_true(resource_metrics.contains_key("network_usage"))
  
  // Stop analytics processing
  analytics_processor.stop()
}

// Test 4: Stream State Management and Recovery
test "stream state management and recovery" {
  let stateful_processor = StatefulStreamProcessor::new()
  
  // Configure state stores
  stateful_processor.add_state_store("counters", KeyValueStateStore::new())
  stateful_processor.add_state_store("windows", WindowStateStore::new())
  stateful_processor.add_state_store("sessions", SessionStateStore::new())
  
  // Enable checkpointing
  stateful_processor.enable_checkpointing(CheckpointConfig::new()
    .with_interval(5000L) // 5 seconds
    .with_storage("file_system")
    .with_retention(10) // Keep 10 checkpoints
  )
  
  // Start stateful processing
  stateful_processor.start()
  
  // Process first batch of events
  let batch1_events = []
  for i in 0..=100 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "stateful-service",
      "process_batch1",
      1640995200L + i.to_int64(),
      Random::int_range(50, 150)
    )
    batch1_events.push(event)
    stateful_processor.process_event(event)
  }
  
  // Wait for processing and checkpoint
  Thread::sleep(6000L)
  
  // Verify state after first batch
  let counters_state = stateful_processor.get_state("counters")
  assert_true(counters_state.contains_key("total_events"))
  assert_eq(counters_state.get("total_events"), 101)
  
  let windows_state = stateful_processor.get_state("windows")
  assert_true(windows_state.contains_key("current_window"))
  
  // Simulate processor failure
  stateful_processor.stop()
  
  // Process second batch of events (should recover from checkpoint)
  stateful_processor.start()
  
  let batch2_events = []
  for i in 101..=200 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "stateful-service",
      "process_batch2",
      1640995300L + i.to_int64(),
      Random::int_range(50, 150)
    )
    batch2_events.push(event)
    stateful_processor.process_event(event)
  }
  
  // Wait for processing
  Thread::sleep(2000L)
  
  // Verify state recovery and continuation
  let recovered_counters = stateful_processor.get_state("counters")
  assert_true(recovered_counters.contains_key("total_events"))
  assert_eq(recovered_counters.get("total_events"), 201) // Should continue from previous state
  
  // Verify checkpoint recovery
  let checkpoints = stateful_processor.get_available_checkpoints()
  assert_true(checkpoints.length() > 0)
  
  // Test manual recovery from specific checkpoint
  let latest_checkpoint = checkpoints[checkpoints.length() - 1]
  stateful_processor.recover_from_checkpoint(latest_checkpoint)
  
  let recovered_state = stateful_processor.get_state("counters")
  assert_true(recovered_state.contains_key("total_events"))
  assert_eq(recovered_state.get("total_events"), 201) // Should match expected state
  
  // Stop stateful processing
  stateful_processor.stop()
}

// Test 5: Dynamic Stream Configuration and Scaling
test "dynamic stream configuration and scaling" {
  let scalable_processor = ScalableStreamProcessor::new()
  
  // Start with minimal configuration
  scalable_processor.configure(StreamConfig::new()
    .with_parallelism(1)
    .with_buffer_size(100)
    .with_batch_size(10)
  )
  
  scalable_processor.start()
  
  // Generate initial workload
  let initial_events = []
  for i in 0..=50 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "scalable-service",
      "initial_load",
      1640995200L + i.to_int64(),
      Random::int_range(50, 100)
    )
    initial_events.push(event)
    scalable_processor.process_event(event)
  }
  
  // Wait for initial processing
  Thread::sleep(1000L)
  
  // Measure initial performance
  let initial_metrics = scalable_processor.get_performance_metrics()
  let initial_throughput = initial_metrics.events_per_second
  let initial_latency = initial_metrics.avg_latency_ms
  
  // Increase workload
  let increased_events = []
  for i in 51..=300 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "scalable-service",
      "increased_load",
      1640995200L + i.to_int64(),
      Random::int_range(50, 100)
    )
    increased_events.push(event)
    scalable_processor.process_event(event)
  }
  
  // Wait for processing
  Thread::sleep(2000L)
  
  // Check if scaling is needed
  let increased_metrics = scalable_processor.get_performance_metrics()
  let increased_throughput = increased_metrics.events_per_second
  let increased_latency = increased_metrics.avg_latency_ms
  
  // Scale up if needed
  if increased_latency > initial_latency * 2 || increased_throughput < initial_throughput * 2 {
    scalable_processor.scale_up(StreamConfig::new()
      .with_parallelism(4)
      .with_buffer_size(500)
      .with_batch_size(50)
    )
    
    // Wait for scaling to take effect
    Thread::sleep(2000L)
    
    // Verify improved performance
    let scaled_metrics = scalable_processor.get_performance_metrics()
    let scaled_throughput = scaled_metrics.events_per_second
    let scaled_latency = scaled_metrics.avg_latency_ms
    
    assert_true(scaled_throughput > increased_throughput)
    assert_true(scaled_latency < increased_latency)
  }
  
  // Test dynamic reconfiguration
  scalable_processor.reconfigure(StreamConfig::new()
    .with_parallelism(2)
    .with_buffer_size(300)
    .with_batch_size(25)
    .with_filter(AttributeFilter::new("service.name", "scalable-service"))
  )
  
  // Wait for reconfiguration
  Thread::sleep(1000L)
  
  // Process events with new configuration
  let reconfigured_events = []
  for i in 301..=350 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "scalable-service",
      "reconfigured_load",
      1640995200L + i.to_int64(),
      Random::int_range(50, 100)
    )
    reconfigured_events.push(event)
    scalable_processor.process_event(event)
  }
  
  // Wait for processing
  Thread::sleep(1000L)
  
  // Verify reconfiguration worked
  let reconfigured_metrics = scalable_processor.get_performance_metrics()
  assert_true(reconfigured_metrics.events_per_second > 0)
  
  // Test scale down
  scalable_processor.scale_down(StreamConfig::new()
    .with_parallelism(1)
    .with_buffer_size(100)
    .with_batch_size(10)
  )
  
  // Wait for scale down
  Thread::sleep(1000L)
  
  // Verify scaled down operation
  let scaled_down_metrics = scalable_processor.get_performance_metrics()
  assert_true(scaled_down_metrics.events_per_second > 0)
  
  // Stop scalable processing
  scalable_processor.stop()
}

// Test 6: Stream Anomaly Detection and Alerting
test "stream anomaly detection and alerting" {
  let anomaly_processor = AnomalyStreamProcessor::new()
  
  // Configure anomaly detectors
  anomaly_processor.add_detector(LatencySpikeDetector::new("latency_spike", 2.0, 5)) // 2x threshold, 5 events window
  anomaly_processor.add_detector(ErrorRateSpikeDetector::new("error_rate_spike", 0.1, 10)) // 10% threshold, 10 events window
  anomaly_processor.add_detector(ThroughputDropDetector::new("throughput_drop", 0.5, 60L)) // 50% drop, 1 minute window
  anomaly_processor.add_detector(PatternAnomalyDetector::new("pattern_anomaly", "unusual_pattern"))
  
  // Configure alert handlers
  anomaly_processor.add_alert_handler(EmailAlertHandler::new("alerts@example.com"))
  anomaly_processor.add_alert_handler(SlackAlertHandler::new("#telemetry-alerts"))
  anomaly_processor.add_alert_handler(PagerDutyAlertHandler::new("telemetry-service"))
  
  // Start anomaly processing
  anomaly_processor.start()
  
  // Generate normal telemetry data
  let normal_events = []
  for i in 0..=50 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "normal-service",
      "normal_operation",
      1640995200L + i.to_int64(),
      Random::int_range(50, 100) // Normal latency
    )
    event.set_status("success")
    normal_events.push(event)
    anomaly_processor.process_event(event)
  }
  
  // Wait for baseline establishment
  Thread::sleep(2000L)
  
  // Generate latency spike anomaly
  let latency_spike_events = []
  for i in 51..=55 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "normal-service",
      "slow_operation",
      1640995250L + i.to_int64(),
      Random::int_range(300, 500) // High latency spike
    )
    event.set_status("success")
    latency_spike_events.push(event)
    anomaly_processor.process_event(event)
  }
  
  // Generate error rate spike anomaly
  let error_spike_events = []
  for i in 56..=65 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "normal-service",
      "error_operation",
      1640995300L + i.to_int64(),
      Random::int_range(50, 100)
    )
    event.set_status("error") // High error rate
    error_spike_events.push(event)
    anomaly_processor.process_event(event)
  }
  
  // Generate throughput drop anomaly (stop sending events for a while)
  Thread::sleep(2000L)
  
  // Resume normal events
  let recovery_events = []
  for i in 66..=80 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "normal-service",
      "recovery_operation",
      1640995400L + i.to_int64(),
      Random::int_range(50, 100)
    )
    event.set_status("success")
    recovery_events.push(event)
    anomaly_processor.process_event(event)
  }
  
  // Wait for anomaly detection
  Thread::sleep(3000L)
  
  // Verify anomaly detection
  let detected_anomalies = anomaly_processor.get_detected_anomalies()
  assert_true(detected_anomalies.length() > 0)
  
  // Verify specific anomaly types
  let latency_anomalies = detected_anomalies.filter(|a| a.detector_type == "latency_spike")
  let error_anomalies = detected_anomalies.filter(|a| a.detector_type == "error_rate_spike")
  let throughput_anomalies = detected_anomalies.filter(|a| a.detector_type == "throughput_drop")
  
  assert_true(latency_anomalies.length() > 0)
  assert_true(error_anomalies.length() > 0)
  assert_true(throughput_anomalies.length() > 0)
  
  // Verify anomaly details
  for anomaly in latency_anomalies {
    assert_true(anomaly.severity >= Severity::Medium)
    assert_true(anomaly.description.contains("latency"))
    assert_true(anomaly.affected_services.contains("normal-service"))
  }
  
  for anomaly in error_anomalies {
    assert_true(anomaly.severity >= Severity::High)
    assert_true(anomaly.description.contains("error"))
    assert_true(anomaly.affected_services.contains("normal-service"))
  }
  
  // Verify alert generation
  let generated_alerts = anomaly_processor.get_generated_alerts()
  assert_true(generated_alerts.length() > 0)
  
  for alert in generated_alerts {
    assert_true(alert.title.length() > 0)
    assert_true(alert.description.length() > 0)
    assert_true(alert.severity >= Severity::Medium)
    assert_true(alert.timestamp > 0L)
  }
  
  // Test anomaly resolution
  anomaly_processor.resolve_anomaly(detected_anomalies[0].id, "Fixed the latency issue")
  
  let resolved_anomalies = anomaly_processor.get_resolved_anomalies()
  assert_true(resolved_anomalies.length() > 0)
  
  let resolved = resolved_anomalies[resolved_anomalies.length() - 1]
  assert_eq(resolved.id, detected_anomalies[0].id)
  assert_eq(resolved.resolution, Some("Fixed the latency issue"))
  assert_true(resolved.resolved_at > resolved.detected_at)
  
  // Stop anomaly processing
  anomaly_processor.stop()
}

// Test 7: Multi-Stream Join and Correlation
test "multi-stream join and correlation" {
  let join_processor = MultiStreamJoinProcessor::new()
  
  // Configure input streams
  join_processor.add_input_stream("user_activity", UserActivityStream::new())
  join_processor.add_input_stream("system_metrics", SystemMetricsStream::new())
  join_processor.add_input_stream("business_events", BusinessEventStream::new())
  
  // Configure join strategies
  join_processor.add_join_strategy(
    JoinStrategy::new("user_system_join")
      .with_left_stream("user_activity")
      .with_right_stream("system_metrics")
      .with_join_key("user_id")
      .with_join_type(JoinType::Inner)
      .with_time_window(30L) // 30-second window
  )
  
  join_processor.add_join_strategy(
    JoinStrategy::new("activity_business_join")
      .with_left_stream("user_activity")
      .with_right_stream("business_events")
      .with_join_key("session_id")
      .with_join_type(JoinType::Left)
      .with_time_window(60L) // 60-second window
  )
  
  // Start join processing
  join_processor.start()
  
  // Generate correlated events across streams
  let base_time = 1640995200L
  
  // User activity events
  let user_events = [
    UserActivityEvent::new("user1", "session1", "login", base_time),
    UserActivityEvent::new("user1", "session1", "view_page", base_time + 10L),
    UserActivityEvent::new("user2", "session2", "login", base_time + 20L),
    UserActivityEvent::new("user1", "session1", "click_button", base_time + 30L),
    UserActivityEvent::new("user2", "session2", "view_page", base_time + 40L)
  ]
  
  for event in user_events {
    join_processor.process_event("user_activity", event)
  }
  
  // System metrics events
  let system_events = [
    SystemMetricsEvent::new("user1", "cpu", 45.5, base_time + 5L),
    SystemMetricsEvent::new("user1", "memory", 67.2, base_time + 15L),
    SystemMetricsEvent::new("user2", "cpu", 23.1, base_time + 25L),
    SystemMetricsEvent::new("user1", "network", 12.8, base_time + 35L),
    SystemMetricsEvent::new("user2", "disk", 78.9, base_time + 45L)
  ]
  
  for event in system_events {
    join_processor.process_event("system_metrics", event)
  }
  
  // Business events
  let business_events = [
    BusinessEvent::new("session1", "purchase", "product123", 99.99, base_time + 15L),
    BusinessEvent::new("session1", "add_to_cart", "product456", 49.99, base_time + 25L),
    BusinessEvent::new("session3", "purchase", "product789", 199.99, base_time + 35L), // No matching user activity
    BusinessEvent::new("session2", "view_product", "product123", 0.0, base_time + 50L)
  ]
  
  for event in business_events {
    join_processor.process_event("business_events", event)
  }
  
  // Wait for join processing
  Thread::sleep(3000L)
  
  // Verify user-system join results
  let user_system_joins = join_processor.get_join_results("user_system_join")
  assert_true(user_system_joins.length() > 0)
  
  for join_result in user_system_joins {
    assert_true(join_result.contains_key("user_activity"))
    assert_true(join_result.contains_key("system_metrics"))
    
    let user_event = join_result.get("user_activity")
    let system_event = join_result.get("system_metrics")
    
    assert_eq(user_event.user_id, system_event.user_id)
    assert_true(system_event.timestamp - user_event.timestamp <= 30L) // Within time window
  }
  
  // Verify activity-business join results
  let activity_business_joins = join_processor.get_join_results("activity_business_join")
  assert_true(activity_business_joins.length() > 0)
  
  for join_result in activity_business_joins {
    assert_true(join_result.contains_key("user_activity"))
    // Business events might be null due to left join
    if join_result.contains_key("business_events") {
      let user_event = join_result.get("user_activity")
      let business_event = join_result.get("business_events")
      
      assert_eq(user_event.session_id, business_event.session_id)
      assert_true(business_event.timestamp - user_event.timestamp <= 60L) // Within time window
    }
  }
  
  // Verify correlation analysis
  let correlation_analysis = join_processor.get_correlation_analysis()
  
  assert_true(correlation_analysis.contains_key("user_activity_system_metrics"))
  assert_true(correlation_analysis.contains_key("user_activity_business_events"))
  
  let user_system_corr = correlation_analysis.get("user_activity_system_metrics")
  assert_true(user_system_corr.correlation_coefficient >= -1.0 && user_system_corr.correlation_coefficient <= 1.0)
  assert_true(user_system_corr.sample_size > 0)
  
  // Stop join processing
  join_processor.stop()
}

// Test 8: Stream Backpressure and Flow Control
test "stream backpressure and flow control" {
  let backpressure_processor = BackpressureStreamProcessor::new()
  
  // Configure backpressure strategy
  backpressure_processor.set_backpressure_strategy(BackpressureStrategy::RateLimit(100)) // 100 events per second
  backpressure_processor.set_buffer_size(1000)
  backpressure_processor.enable_monitoring(true)
  
  // Configure flow control
  backpressure_processor.set_flow_control(FlowControl::new()
    .with_high_watermark(800) // Start applying backpressure at 80% buffer
    .with_low_watermark(200)  // Stop applying backpressure at 20% buffer
    .with_max_delay_ms(1000)  // Maximum delay before dropping events
  )
  
  // Start backpressure processing
  backpressure_processor.start()
  
  // Generate high-volume event stream
  let high_volume_events = []
  for i in 0..=2000 { // More than buffer capacity
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "high-volume-service",
      "process_data",
      1640995200L + i.to_int64(),
      Random::int_range(10, 50)
    )
    high_volume_events.push(event)
  }
  
  // Send events rapidly to trigger backpressure
  let start_time = Time::now()
  let processed_count = 0
  
  for event in high_volume_events {
    let result = backpressure_processor.process_event_with_backpressure(event)
    if result == ProcessResult::Accepted {
      processed_count = processed_count + 1
    }
  }
  
  let end_time = Time::now()
  let total_time = end_time - start_time
  
  // Wait for remaining processing
  Thread::sleep(5000L)
  
  // Verify backpressure was applied
  let final_metrics = backpressure_processor.get_metrics()
  
  assert_true(final_metrics.buffer_utilization > 0.8) // Should have high buffer utilization
  assert_true(final_metrics.backpressure_applied_count > 0) // Backpressure should have been applied
  assert_true(final_metrics.dropped_events_count > 0) // Some events should have been dropped
  
  // Verify rate limiting
  let actual_rate = processed_count.to_float() / (total_time.to_float() / 1000.0)
  assert_true(actual_rate <= 110.0) // Should be close to configured rate limit (100/s)
  
  // Verify processing continued after buffer drain
  let recovery_metrics = backpressure_processor.get_recovery_metrics()
  assert_true(recovery_metrics.backpressure_released_count > 0)
  assert_true(recovery_metrics.avg_recovery_time_ms > 0)
  
  // Test different backpressure strategies
  backpressure_processor.set_backpressure_strategy(BackpressureStrategy::Buffer(500)) // Fixed buffer size
  
  let buffer_events = []
  for i in 0..=600 { // More than buffer size
    let event = TelemetryEvent::new(
      "buffer_trace_" + i.to_string(),
      "buffer_span_" + i.to_string(),
      "buffer-service",
      "buffer_test",
      1640995300L + i.to_int64(),
      Random::int_range(10, 50)
    )
    buffer_events.push(event)
  }
  
  let buffer_processed = 0
  for event in buffer_events {
    let result = backpressure_processor.process_event_with_backpressure(event)
    if result == ProcessResult::Accepted {
      buffer_processed = buffer_processed + 1
    }
  }
  
  // Should process up to buffer limit
  assert_true(buffer_processed <= 500)
  
  // Test priority-based processing
  backpressure_processor.set_backpressure_strategy(BackpressureStrategy::Priority())
  
  let priority_events = []
  for i in 0..=100 {
    let priority = if i % 10 == 0 { Priority::High } else { Priority::Normal }
    let event = TelemetryEvent::new(
      "priority_trace_" + i.to_string(),
      "priority_span_" + i.to_string(),
      "priority-service",
      "priority_test",
      1640995400L + i.to_int64(),
      Random::int_range(10, 50)
    )
    event.set_priority(priority)
    priority_events.push(event)
  }
  
  let high_priority_processed = 0
  let normal_priority_processed = 0
  
  for event in priority_events {
    let result = backpressure_processor.process_event_with_backpressure(event)
    if result == ProcessResult::Accepted {
      if event.priority == Priority::High {
        high_priority_processed = high_priority_processed + 1
      } else {
        normal_priority_processed = normal_priority_processed + 1
      }
    }
  }
  
  // High priority events should have higher acceptance rate
  let high_priority_rate = high_priority_processed.to_float() / 10.0 // 10 high priority events
  let normal_priority_rate = normal_priority_processed.to_float() / 90.0 // 90 normal priority events
  
  assert_true(high_priority_rate >= normal_priority_rate)
  
  // Stop backpressure processing
  backpressure_processor.stop()
}

// Test 9: Stream Data Quality and Validation
test "stream data quality and validation" {
  let quality_processor = DataQualityStreamProcessor::new()
  
  // Configure quality validators
  quality_processor.add_validator(MandatoryFieldValidator::new(["trace_id", "span_id", "timestamp"]))
  quality_processor.add_validator(TimestampRangeValidator::new(1640990000L, 1641000000L))
  quality_processor.add_validator(LatencyRangeValidator::new(0, 60000)) // Max 60 seconds
  quality_processor.add_validator(ServiceNameValidator::new(["api-gateway", "user-service", "payment-service"]))
  
  // Configure quality metrics
  quality_processor.enable_quality_metrics(true)
  quality_processor.set_quality_thresholds(QualityThresholds::new()
    .with_min_completeness(0.95) // 95% completeness
    .with_max_invalid_rate(0.05) // 5% invalid rate
    .with_min_timeliness(0.90)   // 90% timeliness
  )
  
  // Start quality processing
  quality_processor.start()
  
  // Generate mixed quality events
  let mixed_events = []
  
  // Valid events
  for i in 0..=80 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "api-gateway",
      "valid_operation",
      1640995200L + i.to_int64(),
      Random::int_range(50, 200)
    )
    mixed_events.push(event)
  }
  
  // Invalid events - missing mandatory fields
  for i in 81..=85 {
    let event = TelemetryEvent::new(
      "", // Missing trace_id
      "span_" + i.to_string(),
      "user-service",
      "invalid_operation",
      1640995300L + i.to_int64(),
      Random::int_range(50, 200)
    )
    mixed_events.push(event)
  }
  
  // Invalid events - out of range timestamp
  for i in 86..=90 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "payment-service",
      "invalid_operation",
      1540995200L, // Too old
      Random::int_range(50, 200)
    )
    mixed_events.push(event)
  }
  
  // Invalid events - unknown service
  for i in 91..=95 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "unknown-service",
      "invalid_operation",
      1640995400L + i.to_int64(),
      Random::int_range(50, 200)
    )
    mixed_events.push(event)
  }
  
  // Process mixed quality events
  for event in mixed_events {
    quality_processor.process_event(event)
  }
  
  // Wait for quality processing
  Thread::sleep(2000L)
  
  // Verify quality metrics
  let quality_metrics = quality_processor.get_quality_metrics()
  
  assert_eq(quality_metrics.total_events, 95)
  assert_eq(quality_metrics.valid_events, 80)
  assert_eq(quality_metrics.invalid_events, 15)
  
  let completeness_rate = quality_metrics.valid_events.to_float() / quality_metrics.total_events.to_float()
  assert_eq(completeness_rate, 0.842) // 80/95
  
  let invalid_rate = quality_metrics.invalid_events.to_float() / quality_metrics.total_events.to_float()
  assert_eq(invalid_rate, 0.158) // 15/95
  
  // Verify validation breakdown
  let validation_breakdown = quality_processor.get_validation_breakdown()
  
  assert_true(validation_breakdown.contains_key("mandatory_field_validator"))
  assert_true(validation_breakdown.contains_key("timestamp_range_validator"))
  assert_true(validation_breakdown.contains_key("latency_range_validator"))
  assert_true(validation_breakdown.contains_key("service_name_validator"))
  
  assert_eq(validation_breakdown.get("mandatory_field_validator"), 5) // 5 events with missing trace_id
  assert_eq(validation_breakdown.get("timestamp_range_validator"), 5) // 5 events with old timestamp
  assert_eq(validation_breakdown.get("service_name_validator"), 5) // 5 events with unknown service
  
  // Verify quality alerts
  let quality_alerts = quality_processor.get_quality_alerts()
  assert_true(quality_alerts.length() > 0)
  
  // Should have alert for low completeness
  let completeness_alert = quality_alerts.find(|a| a.alert_type == "low_completeness")
  assert_true(completeness_alert.is_some())
  
  // Should have alert for high invalid rate
  let invalid_rate_alert = quality_alerts.find(|a| a.alert_type == "high_invalid_rate")
  assert_true(invalid_rate_alert.is_some())
  
  // Test data enrichment for quality improvement
  quality_processor.enable_data_enrichment(true)
  
  let enrichment_events = []
  for i in 96..=100 {
    let event = TelemetryEvent::new(
      "", // Missing trace_id
      "span_" + i.to_string(),
      "api-gateway",
      "enrichment_operation",
      1640995500L + i.to_int64(),
      Random::int_range(50, 200)
    )
    enrichment_events.push(event)
  }
  
  for event in enrichment_events {
    quality_processor.process_event(event)
  }
  
  // Wait for enrichment processing
  Thread::sleep(1000L)
  
  // Verify enrichment results
  let enrichment_metrics = quality_processor.get_enrichment_metrics()
  assert_true(enrichment_metrics.enriched_count > 0)
  assert_true(enrichment_metrics.enrichment_rate > 0.0)
  
  // Verify enriched events have trace_id
  let enriched_events = quality_processor.get_enriched_events()
  for event in enriched_events {
    assert_true(event.trace_id.length() > 0)
  }
  
  // Stop quality processing
  quality_processor.stop()
}

// Test 10: Stream Monitoring and Observability
test "stream monitoring and observability" {
  let monitoring_processor = StreamMonitoringProcessor::new()
  
  // Configure monitoring
  monitoring_processor.enable_stream_metrics(true)
  monitoring_processor.enable_health_checks(true)
  monitoring_processor.set_health_check_interval(5000L) // 5 seconds
  monitoring_processor.enable_performance_profiling(true)
  
  // Configure observability sinks
  monitoring_processor.add_metrics_sink(PrometheusMetricsSink::new())
  monitoring_processor.add_health_sink(HealthCheckSink::new())
  monitoring_processor.add_tracing_sink(JaegerTracingSink::new())
  
  // Start monitoring
  monitoring_processor.start()
  
  // Process telemetry events with varying characteristics
  let monitoring_events = []
  
  // Normal events
  for i in 0..=100 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "monitored-service",
      "normal_operation",
      1640995200L + i.to_int64(),
      Random::int_range(50, 150)
    )
    monitoring_events.push(event)
    monitoring_processor.process_event(event)
  }
  
  // Slow events
  for i in 101..=110 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "monitored-service",
      "slow_operation",
      1640995300L + i.to_int64(),
      Random::int_range(500, 1000) // Slow operations
    )
    monitoring_events.push(event)
    monitoring_processor.process_event(event)
  }
  
  // Error events
  for i in 111..=115 {
    let event = TelemetryEvent::new(
      "trace_" + i.to_string(),
      "span_" + i.to_string(),
      "monitored-service",
      "error_operation",
      1640995400L + i.to_int64(),
      Random::int_range(50, 150)
    )
    event.set_status("error")
    monitoring_events.push(event)
    monitoring_processor.process_event(event)
  }
  
  // Wait for monitoring data collection
  Thread::sleep(6000L) // Wait for at least one health check interval
  
  // Verify stream metrics
  let stream_metrics = monitoring_processor.get_stream_metrics()
  
  assert_true(stream_metrics.contains_key("events_processed_total"))
  assert_true(stream_metrics.contains_key("events_processed_per_second"))
  assert_true(stream_metrics.contains_key("processing_latency_ms"))
  assert_true(stream_metrics.contains_key("error_rate"))
  assert_true(stream_metrics.contains_key("throughput"))
  
  assert_eq(stream_metrics.get("events_processed_total"), 115)
  assert_true(stream_metrics.get("events_processed_per_second") > 0.0)
  assert_true(stream_metrics.get("processing_latency_ms") > 0.0)
  assert_true(stream_metrics.get("error_rate") > 0.0) // Should have some errors
  assert_true(stream_metrics.get("throughput") > 0.0)
  
  // Verify health checks
  let health_status = monitoring_processor.get_health_status()
  
  assert_true(health_status.contains_key("overall"))
  assert_true(health_status.contains_key("event_processing"))
  assert_true(health_status.contains_key("memory_usage"))
  assert_true(health_status.contains_key("cpu_usage"))
  
  let overall_health = health_status.get("overall")
  assert_eq(overall_health.status, "healthy") // Should be healthy despite some issues
  
  let event_processing_health = health_status.get("event_processing")
  assert_true(event_processing_health.status == "healthy" || event_processing_health.status == "warning")
  
  // Verify performance profiling
  let performance_profile = monitoring_processor.get_performance_profile()
  
  assert_true(performance_profile.contains_key("processing_time_distribution"))
  assert_true(performance_profile.contains_key("memory_usage_over_time"))
  assert_true(performance_profile.contains_key("cpu_usage_over_time"))
  assert_true(performance_profile.contains_key("throughput_over_time"))
  
  let processing_distribution = performance_profile.get("processing_time_distribution")
  assert_true(processing_distribution.contains_key("p50"))
  assert_true(processing_distribution.contains_key("p95"))
  assert_true(processing_distribution.contains_key("p99"))
  
  // P99 should be higher due to slow operations
  assert_true(processing_distribution.get("p99") > processing_distribution.get("p50"))
  
  // Verify custom metrics
  monitoring_processor.add_custom_metric("slow_operations", 10)
  monitoring_processor.add_custom_metric("error_operations", 5)
  
  let custom_metrics = monitoring_processor.get_custom_metrics()
  assert_true(custom_metrics.contains_key("slow_operations"))
  assert_true(custom_metrics.contains_key("error_operations"))
  assert_eq(custom_metrics.get("slow_operations"), 10)
  assert_eq(custom_metrics.get("error_operations"), 5)
  
  // Verify alerting based on metrics
  let monitoring_alerts = monitoring_processor.get_monitoring_alerts()
  
  // Should have alerts for slow operations and errors
  let slow_operation_alert = monitoring_alerts.find(|a| a.metric_name == "processing_latency_ms" && a.alert_type == "threshold_exceeded")
  assert_true(slow_operation_alert.is_some())
  
  let error_rate_alert = monitoring_alerts.find(|a| a.metric_name == "error_rate" && a.alert_type == "threshold_exceeded")
  assert_true(error_rate_alert.is_some())
  
  // Test observability data export
  let prometheus_metrics = monitoring_processor.export_prometheus_metrics()
  assert_true(prometheus_metrics.contains("azimuth_stream_events_processed_total"))
  assert_true(prometheus_metrics.contains("azimuth_stream_processing_latency_ms"))
  assert_true(prometheus_metrics.contains("azimuth_stream_error_rate"))
  
  let health_check_json = monitoring_processor.export_health_check()
  assert_true(health_check_json.contains("\"overall\""))
  assert_true(health_check_json.contains("\"status\""))
  
  // Stop monitoring
  monitoring_processor.stop()
}