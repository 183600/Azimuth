// Premium Real-time Stream Processing Tests for Azimuth
// This file contains comprehensive test cases for real-time stream processing of telemetry data

// Test 1: Real-time Telemetry Data Stream Processing
test "real-time telemetry data stream processing" {
  let stream_processor = RealTimeStreamProcessor::new()
  let telemetry_sink = TelemetrySink::new()
  
  // Configure stream processor
  StreamProcessor::configure(stream_processor, [
    ("buffer_size", 1000),
    ("batch_size", 100),
    ("flush_interval_ms", 100)
  ])
  
  // Connect telemetry sink
  StreamProcessor::connect_sink(stream_processor, telemetry_sink)
  
  // Start processing
  StreamProcessor::start(stream_processor)
  
  // Generate telemetry data stream
  let base_timestamp = Time::now()
  for i in 0..=500 {
    let telemetry_data = TelemetryData::new(
      base_timestamp + i * 10, // 10ms intervals
      "service_" + (i % 5).to_string(),
      "operation_" + (i % 10).to_string(),
      (100 + i % 50).to_float(), // Response time between 100-150ms
      i % 2 == 0, // Success/failure
      Attributes::new()
    )
    
    // Add attributes
    let attrs = TelemetryData::attributes(telemetry_data)
    Attributes::set(attrs, "request_id", StringValue("req_" + i.to_string()))
    Attributes::set(attrs, "user_id", StringValue("user_" + (i % 20).to_string()))
    
    // Process telemetry data
    StreamProcessor::process(stream_processor, telemetry_data)
  }
  
  // Wait for processing to complete
  Time::sleep(200)
  
  // Stop processing
  StreamProcessor::stop(stream_processor)
  
  // Verify processing results
  let processed_count = TelemetrySink::get_processed_count(telemetry_sink)
  assert_eq(processed_count, 501)
  
  // Verify aggregated metrics
  let metrics = TelemetrySink::get_aggregated_metrics(telemetry_sink)
  assert_true(metrics.contains("avg_response_time"))
  assert_true(metrics.contains("success_rate"))
  assert_true(metrics.contains("request_count"))
  
  // Verify real-time processing latency
  let processing_stats = StreamProcessor::get_statistics(stream_processor)
  assert_true(processing_stats.avg_processing_time_ms < 50.0) // Should process within 50ms
  assert_true(processing_stats.max_processing_time_ms < 100.0) // Should never exceed 100ms
}

// Test 2: Stream Windowing and Time-based Aggregation
test "stream windowing and time-based aggregation" {
  let window_processor = WindowedStreamProcessor::new()
  
  // Configure windowing
  WindowedProcessor::configure_tumbling_window(window_processor, 1000) // 1 second windows
  WindowedProcessor::configure_sliding_window(window_processor, 2000, 500) // 2s window, 500s slide
  
  // Start processing
  WindowedProcessor::start(window_processor)
  
  // Generate data spanning multiple windows
  let base_timestamp = Time::now()
  for i in 0..=100 {
    let timestamp = base_timestamp + i * 50 // 50ms intervals
    let value = (i % 20).to_float()
    
    let data_point = StreamDataPoint::new(timestamp, value, Attributes::new())
    WindowedProcessor::process(window_processor, data_point)
  }
  
  // Wait for windows to complete
  Time::sleep(1500)
  
  // Get tumbling window results
  let tumbling_results = WindowedProcessor::get_tumbling_results(window_processor)
  assert_true(tumbling_results.length() > 0)
  
  for window in tumbling_results {
    let window_stats = WindowedProcessor::get_window_stats(window)
    assert_true(window_stats.count > 0)
    assert_true(window_stats.sum >= 0.0)
    assert_true(window_stats.average >= 0.0)
    assert_true(window_stats.min >= 0.0)
    assert_true(window_stats.max >= 0.0)
  }
  
  // Get sliding window results
  let sliding_results = WindowedProcessor::get_sliding_results(window_processor)
  assert_true(sliding_results.length() > 0)
  
  // Verify sliding windows overlap correctly
  if sliding_results.length() >= 2 {
    let window1 = sliding_results[0]
    let window2 = sliding_results[1]
    
    let window1_end = WindowedProcessor::get_end_time(window1)
    let window2_start = WindowedProcessor::get_start_time(window2)
    
    // Sliding windows should overlap
    assert_true(window1_end > window2_start)
  }
  
  WindowedProcessor::stop(window_processor)
}

// Test 3: Real-time Anomaly Detection in Streams
test "real-time anomaly detection in streams" {
  let anomaly_detector = StreamAnomalyDetector::new()
  let alert_sink = AlertSink::new()
  
  // Configure anomaly detection
  AnomalyDetector::configure_threshold(anomaly_detector, 2.0) // 2 standard deviations
  AnomalyDetector::configure_window_size(anomaly_detector, 50) // 50 data points
  AnomalyDetector::connect_alert_sink(anomaly_detector, alert_sink)
  
  // Start detection
  AnomalyDetector::start(anomaly_detector)
  
  // Generate normal data
  let base_timestamp = Time::now()
  for i in 0..=100 {
    let value = 50.0 + (Math::sin(i.to_float() * 0.1) * 10.0) // Normal pattern
    let data_point = StreamDataPoint::new(base_timestamp + i * 100, value, Attributes::new())
    AnomalyDetector::process(anomaly_detector, data_point)
  }
  
  // Generate anomalous data
  for i in 101..=110 {
    let value = 150.0 + i.to_float() // Anomalous high values
    let data_point = StreamDataPoint::new(base_timestamp + i * 100, value, Attributes::new())
    AnomalyDetector::process(anomaly_detector, data_point)
  }
  
  // Wait for processing
  Time::sleep(200)
  
  // Stop detection
  AnomalyDetector::stop(anomaly_detector)
  
  // Verify anomaly detection
  let alerts = AlertSink::get_alerts(alert_sink)
  assert_true(alerts.length() > 0)
  
  // Verify anomalies were detected in the expected range
  for alert in alerts {
    let alert_timestamp = Alert::get_timestamp(alert)
    assert_true(alert_timestamp >= base_timestamp + 10100) // Should be in anomalous range
    assert_true(Alert::get_severity(alert) >= AlertSeverity::Medium)
  }
  
  // Verify detection statistics
  let detection_stats = AnomalyDetector::get_statistics(anomaly_detector)
  assert_true(detection_stats.total_processed > 110)
  assert_true(detection_stats.anomalies_detected > 0)
  assert_true(detection_stats.false_positive_rate < 0.1) // Less than 10% false positives
}

// Test 4: Stream Filtering and Transformation
test "stream filtering and transformation" {
  let filter_processor = StreamFilterProcessor::new()
  let transform_processor = StreamTransformProcessor::new()
  let output_sink = OutputSink::new()
  
  // Configure filters
  FilterProcessor::add_filter(filter_processor, fn(data_point) {
    let value = StreamDataPoint::get_value(data_point)
    value > 50.0 // Only keep values > 50
  })
  
  FilterProcessor::add_attribute_filter(filter_processor, "region", ["us-east", "us-west"])
  
  // Configure transformations
  TransformProcessor::add_transform(transform_processor, fn(data_point) {
    let value = StreamDataPoint::get_value(data_point)
    let transformed_value = value * 1.5 // Multiply by 1.5
    StreamDataPoint::set_value(data_point, transformed_value)
    data_point
  })
  
  TransformProcessor::add_attribute_transform(transform_processor, "normalized_value", fn(data_point) {
    let value = StreamDataPoint::get_value(data_point)
    (value / 100.0).to_string()
  })
  
  // Connect processors in pipeline
  FilterProcessor::connect_to(filter_processor, transform_processor)
  TransformProcessor::connect_to(transform_processor, output_sink)
  
  // Start processing
  FilterProcessor::start(filter_processor)
  TransformProcessor::start(transform_processor)
  
  // Generate test data
  let base_timestamp = Time::now()
  let regions = ["us-east", "us-west", "eu-west", "ap-southeast"]
  
  for i in 0..=100 {
    let value = (i % 100).to_float()
    let region = regions[i % regions.length()]
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "region", StringValue(region))
    
    let data_point = StreamDataPoint::new(base_timestamp + i * 50, value, attrs)
    FilterProcessor::process(filter_processor, data_point)
  }
  
  // Wait for processing
  Time::sleep(200)
  
  // Stop processing
  FilterProcessor::stop(filter_processor)
  TransformProcessor::stop(transform_processor)
  
  // Verify filtering and transformation
  let output_data = OutputSink::get_data(output_sink)
  assert_true(output_data.length() > 0)
  
  // All output values should be > 50 (after filtering)
  for data_point in output_data {
    let value = StreamDataPoint::get_value(data_point)
    assert_true(value > 50.0)
    
    // All output values should be transformed (multiplied by 1.5)
    let original_value = value / 1.5
    assert_true(original_value > 50.0)
    
    // Check for transformed attribute
    let attrs = StreamDataPoint::get_attributes(data_point)
    let normalized_value = Attributes::get(attrs, "normalized_value")
    match normalized_value {
      Some(StringValue(norm_str)) => {
        let norm_value = norm_str.to_float()
        assert_true(norm_value > 0.0)
      }
      _ => assert_true(false)
    }
  }
}

// Test 5: Stream Joins and Correlations
test "stream joins and correlations" {
  let join_processor = StreamJoinProcessor::new()
  let correlation_processor = StreamCorrelationProcessor::new()
  let output_sink = OutputSink::new()
  
  // Configure stream join
  JoinProcessor::configure_join(join_processor, "user_id", 5000) // Join on user_id within 5s window
  
  // Configure correlation
  CorrelationProcessor::configure_correlation(correlation_processor, "session_id", 10000) // Correlate by session within 10s
  
  // Connect processors
  JoinProcessor::connect_to(join_processor, correlation_processor)
  CorrelationProcessor::connect_to(correlation_processor, output_sink)
  
  // Start processing
  JoinProcessor::start(join_processor)
  CorrelationProcessor::start(correlation_processor)
  
  // Generate correlated data streams
  let base_timestamp = Time::now()
  
  // Stream 1: User activity events
  for i in 0..=50 {
    let user_id = "user_" + (i % 10).to_string()
    let session_id = "session_" + (i % 5).to_string()
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "user_id", StringValue(user_id))
    Attributes::set(attrs, "session_id", StringValue(session_id))
    Attributes::set(attrs, "event_type", StringValue("activity"))
    
    let data_point = StreamDataPoint::new(base_timestamp + i * 200, i.to_float(), attrs)
    JoinProcessor::process_stream1(join_processor, data_point)
  }
  
  // Stream 2: User performance events (delayed)
  for i in 0..=50 {
    let user_id = "user_" + (i % 10).to_string()
    let session_id = "session_" + (i % 5).to_string()
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "user_id", StringValue(user_id))
    Attributes::set(attrs, "session_id", StringValue(session_id))
    Attributes::set(attrs, "event_type", StringValue("performance"))
    
    // Add delay to simulate real-world scenario
    let delayed_timestamp = base_timestamp + i * 200 + 1000
    let data_point = StreamDataPoint::new(delayed_timestamp, (i * 2).to_float(), attrs)
    JoinProcessor::process_stream2(join_processor, data_point)
  }
  
  // Wait for processing
  Time::sleep(500)
  
  // Stop processing
  JoinProcessor::stop(join_processor)
  CorrelationProcessor::stop(correlation_processor)
  
  // Verify joins and correlations
  let output_data = OutputSink::get_data(output_sink)
  assert_true(output_data.length() > 0)
  
  // Verify joined data contains attributes from both streams
  for data_point in output_data {
    let attrs = StreamDataPoint::get_attributes(data_point)
    
    let event_type = Attributes::get(attrs, "event_type")
    match event_type {
      Some(StringValue(event)) => {
        assert_true(event == "activity" || event == "performance")
      }
      _ => assert_true(false)
    }
    
    // Check for joined attributes
    let user_id = Attributes::get(attrs, "user_id")
    let session_id = Attributes::get(attrs, "session_id")
    
    match user_id {
      Some(StringValue(_)) => assert_true(true)
      _ => assert_true(false)
    }
    
    match session_id {
      Some(StringValue(_)) => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // Verify join statistics
  let join_stats = JoinProcessor::get_statistics(join_processor)
  assert_true(join_stats.total_joins > 0)
  assert_true(join_stats.join_success_rate > 0.5) // At least 50% success rate
}

// Test 6: Real-time Stream Analytics
test "real-time stream analytics" {
  let analytics_processor = StreamAnalyticsProcessor::new()
  let dashboard_sink = DashboardSink::new()
  
  // Configure analytics
  AnalyticsProcessor::enable_real_time_aggregation(analytics_processor, 1000) // 1 second aggregation
  AnalyticsProcessor::enable_trend_analysis(analytics_processor, 5000) // 5 second trend window
  AnalyticsProcessor::enable_percentile_calculation(analytics_processor, [50.0, 90.0, 95.0, 99.0])
  
  // Connect to dashboard
  AnalyticsProcessor::connect_to_dashboard(analytics_processor, dashboard_sink)
  
  // Start processing
  AnalyticsProcessor::start(analytics_processor)
  
  // Generate data with varying patterns
  let base_timestamp = Time::now()
  
  // Phase 1: Normal load
  for i in 0..=100 {
    let value = 50.0 + (Math::sin(i.to_float() * 0.1) * 10.0)
    let data_point = StreamDataPoint::new(base_timestamp + i * 50, value, Attributes::new())
    AnalyticsProcessor::process(analytics_processor, data_point)
  }
  
  // Phase 2: Increasing load (ramp-up)
  for i in 101..=200 {
    let value = 50.0 + (i - 100).to_float() * 0.5
    let data_point = StreamDataPoint::new(base_timestamp + i * 50, value, Attributes::new())
    AnalyticsProcessor::process(analytics_processor, data_point)
  }
  
  // Phase 3: High load with spikes
  for i in 201..=300 {
    let base_value = 100.0
    let spike = if i % 10 == 0 { 50.0 } else { 0.0 }
    let value = base_value + spike
    
    let data_point = StreamDataPoint::new(base_timestamp + i * 50, value, Attributes::new())
    AnalyticsProcessor::process(analytics_processor, data_point)
  }
  
  // Wait for analytics processing
  Time::sleep(1000)
  
  // Stop processing
  AnalyticsProcessor::stop(analytics_processor)
  
  // Verify analytics results
  let dashboard_data = DashboardSink::get_data(dashboard_sink)
  assert_true(dashboard_data.length() > 0)
  
  // Verify real-time aggregations
  let aggregations = DashboardSink::get_aggregations(dashboard_data)
  assert_true(aggregations.contains("current_avg"))
  assert_true(aggregations.contains("current_min"))
  assert_true(aggregations.contains("current_max"))
  assert_true(aggregations.contains("current_count"))
  
  // Verify trend analysis
  let trends = DashboardSink::get_trends(dashboard_data)
  assert_true(trends.contains("trend_direction"))
  assert_true(trends.contains("trend_strength"))
  
  // Verify percentile calculations
  let percentiles = DashboardSink::get_percentiles(dashboard_data)
  assert_true(percentiles.contains("p50"))
  assert_true(percentiles.contains("p90"))
  assert_true(percentiles.contains("p95"))
  assert_true(percentiles.contains("p99"))
  
  // Verify analytics statistics
  let analytics_stats = AnalyticsProcessor::get_statistics(analytics_processor)
  assert_true(analytics_stats.total_processed > 300)
  assert_true(analytics_stats.avg_processing_time_ms < 20.0)
}

// Test 7: Stream Backpressure and Flow Control
test "stream backpressure and flow control" {
  let flow_controlled_processor = FlowControlledProcessor::new()
  let slow_sink = SlowSink::new(10) // 10ms processing time per item
  
  // Configure flow control
  FlowControlledProcessor::configure_buffer(flow_controlled_processor, 100)
  FlowControlledProcessor::configure_backpressure_threshold(flow_controlled_processor, 80) // 80% buffer capacity
  FlowControlledProcessor::connect_sink(flow_controlled_processor, slow_sink)
  
  // Start processing
  FlowControlledProcessor::start(flow_controlled_processor)
  
  // Generate high-volume data to trigger backpressure
  let base_timestamp = Time::now()
  let start_time = Time::now()
  
  for i in 0..=500 {
    let data_point = StreamDataPoint::new(base_timestamp + i * 10, i.to_float(), Attributes::new())
    FlowControlledProcessor::process(flow_controlled_processor, data_point)
  }
  
  let generation_time = Time::now() - start_time
  
  // Wait for processing to complete
  while FlowControlledProcessor::get_buffer_size(flow_controlled_processor) > 0 {
    Time::sleep(10)
  }
  
  // Stop processing
  FlowControlledProcessor::stop(flow_controlled_processor)
  
  // Verify backpressure was applied
  let flow_stats = FlowControlledProcessor::get_statistics(flow_controlled_processor)
  assert_true(flow_stats.backpressure_events > 0)
  assert_true(flow_stats.max_buffer_size >= 80) // Should have reached backpressure threshold
  
  // Verify processing took longer due to backpressure
  let processing_time = SlowSink::get_total_processing_time(slow_sink)
  assert_true(processing_time > generation_time) // Processing should be slower than generation
  
  // Verify all data was eventually processed
  let processed_count = SlowSink::get_processed_count(slow_sink)
  assert_eq(processed_count, 501)
}

// Test 8: Stream State Management and Recovery
test "stream state management and recovery" {
  let stateful_processor = StatefulStreamProcessor::new()
  let state_store = MemoryStateStore::new()
  
  // Configure state management
  StatefulProcessor::configure_state_store(stateful_processor, state_store)
  StatefulProcessor::enable_checkpointing(stateful_processor, 2000) // Checkpoint every 2 seconds
  StatefulProcessor::enable_state_recovery(stateful_processor)
  
  // Start processing
  StatefulProcessor::start(stateful_processor)
  
  // Phase 1: Process data and create state
  let base_timestamp = Time::now()
  for i in 0..=100 {
    let key = "key_" + (i % 10).to_string()
    let value = i.to_float()
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "key", StringValue(key))
    
    let data_point = StreamDataPoint::new(base_timestamp + i * 50, value, attrs)
    StatefulProcessor::process(stateful_processor, data_point)
  }
  
  // Wait for checkpointing
  Time::sleep(2500)
  
  // Simulate processor failure and recovery
  StatefulProcessor::simulate_failure(stateful_processor)
  StatefulProcessor::recover(stateful_processor)
  
  // Phase 2: Continue processing after recovery
  for i in 101..=200 {
    let key = "key_" + (i % 10).to_string()
    let value = i.to_float()
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "key", StringValue(key))
    
    let data_point = StreamDataPoint::new(base_timestamp + i * 50, value, attrs)
    StatefulProcessor::process(stateful_processor, data_point)
  }
  
  // Wait for processing
  Time::sleep(500)
  
  // Stop processing
  StatefulProcessor::stop(stateful_processor)
  
  // Verify state recovery
  let final_state = StatefulProcessor::get_state(stateful_processor)
  assert_true(final_state.size() > 0)
  
  // Verify state continuity across recovery
  for i in 0..=9 {
    let key = "key_" + i.to_string()
    let state_value = StatefulProcessor::get_state_value(stateful_processor, key)
    
    match state_value {
      Some(value) => {
        // Should contain values from both before and after recovery
        assert_true(value > 0.0)
      }
      None => assert_true(false)
    }
  }
  
  // Verify recovery statistics
  let recovery_stats = StatefulProcessor::get_recovery_statistics(stateful_processor)
  assert_true(recovery_stats.checkpoints_created > 0)
  assert_true(recovery_stats.recovery_events > 0)
  assert_true(recovery_stats.state_restored)
}

// Test 9: Multi-stream Processing and Merging
test "multi-stream processing and merging" {
  let multi_stream_processor = MultiStreamProcessor::new()
  let merge_processor = StreamMergeProcessor::new()
  let output_sink = OutputSink::new()
  
  // Configure multi-stream processing
  MultiStreamProcessor::add_stream(multi_stream_processor, "metrics")
  MultiStreamProcessor::add_stream(multi_stream_processor, "traces")
  MultiStreamProcessor::add_stream(multi_stream_processor, "logs")
  
  // Configure merging strategy
  MergeProcessor::configure_merge_strategy(merge_processor, MergeStrategy::Timestamp)
  MergeProcessor::configure_time_window(merge_processor, 100) // 100ms time window for merging
  
  // Connect processors
  MultiStreamProcessor::connect_to(multi_stream_processor, merge_processor)
  MergeProcessor::connect_to(merge_processor, output_sink)
  
  // Start processing
  MultiStreamProcessor::start(multi_stream_processor)
  MergeProcessor::start(merge_processor)
  
  // Generate multi-stream data
  let base_timestamp = Time::now()
  
  // Metrics stream
  for i in 0..=50 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric_name", StringValue("response_time"))
    Attributes::set(attrs, "value", FloatValue((50 + i).to_float()))
    
    let data_point = StreamDataPoint::new(base_timestamp + i * 100, i.to_float(), attrs)
    MultiStreamProcessor::process_to_stream(multi_stream_processor, "metrics", data_point)
  }
  
  // Traces stream
  for i in 0..=30 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "trace_id", StringValue("trace_" + i.to_string()))
    Attributes::set(attrs, "span_name", StringValue("operation_" + (i % 5).to_string()))
    
    let data_point = StreamDataPoint::new(base_timestamp + i * 150 + 50, i.to_float(), attrs)
    MultiStreamProcessor::process_to_stream(multi_stream_processor, "traces", data_point)
  }
  
  // Logs stream
  for i in 0..=40 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "log_level", StringValue(["INFO", "WARN", "ERROR"][i % 3]))
    Attributes::set(attrs, "message", StringValue("Log message " + i.to_string()))
    
    let data_point = StreamDataPoint::new(base_timestamp + i * 120 + 25, i.to_float(), attrs)
    MultiStreamProcessor::process_to_stream(multi_stream_processor, "logs", data_point)
  }
  
  // Wait for processing
  Time::sleep(1000)
  
  // Stop processing
  MultiStreamProcessor::stop(multi_stream_processor)
  MergeProcessor::stop(merge_processor)
  
  // Verify multi-stream processing
  let output_data = OutputSink::get_data(output_sink)
  assert_true(output_data.length() > 0)
  
  // Verify merged data contains attributes from different streams
  let mut metrics_count = 0
  let mut traces_count = 0
  let mut logs_count = 0
  
  for data_point in output_data {
    let attrs = StreamDataPoint::get_attributes(data_point)
    
    let stream_type = Attributes::get(attrs, "stream_type")
    match stream_type {
      Some(StringValue(stream)) => {
        match stream {
          "metrics" => metrics_count = metrics_count + 1
          "traces" => traces_count = traces_count + 1
          "logs" => logs_count = logs_count + 1
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  assert_true(metrics_count > 0)
  assert_true(traces_count > 0)
  assert_true(logs_count > 0)
  
  // Verify merge statistics
  let merge_stats = MergeProcessor::get_statistics(merge_processor)
  assert_true(merge_stats.total_merged > 0)
  assert_true(merge_stats.streams_processed >= 3)
}

// Test 10: Real-time Stream Visualization
test "real-time stream visualization" {
  let visualization_processor = StreamVisualizationProcessor::new()
  let chart_sink = ChartSink::new()
  
  // Configure visualization
  VisualizationProcessor::enable_time_series_chart(visualization_processor, "response_time")
  VisualizationProcessor::enable_histogram_chart(visualization_processor, "response_time_distribution")
  VisualizationProcessor::enable_heatmap(visualization_processor, "service_activity")
  
  // Configure chart parameters
  VisualizationProcessor::configure_chart_update_interval(visualization_processor, 500) // Update every 500ms
  VisualizationProcessor::configure_data_retention(visualization_processor, 10000) // Retain 10 seconds of data
  
  // Connect to chart sink
  VisualizationProcessor::connect_to_charts(visualization_processor, chart_sink)
  
  // Start processing
  VisualizationProcessor::start(visualization_processor)
  
  // Generate data for visualization
  let base_timestamp = Time::now()
  let services = ["auth", "payment", "catalog", "order", "notification"]
  
  for i in 0..=200 {
    let service = services[i % services.length()]
    let response_time = 50.0 + (Math::sin(i.to_float() * 0.1) * 30.0) + (Math::random() * 20.0)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "service", StringValue(service))
    Attributes::set(attrs, "response_time", FloatValue(response_time))
    Attributes::set(attrs, "status_code", IntValue(if response_time < 80.0 { 200 } else { 500 }))
    
    let data_point = StreamDataPoint::new(base_timestamp + i * 50, response_time, attrs)
    VisualizationProcessor::process(visualization_processor, data_point)
  }
  
  // Wait for visualization processing
  Time::sleep(1000)
  
  // Stop processing
  VisualizationProcessor::stop(visualization_processor)
  
  // Verify visualization data
  let chart_data = ChartSink::get_chart_data(chart_sink)
  assert_true(chart_data.length() > 0)
  
  // Verify time series chart data
  let time_series_data = ChartSink::get_time_series_data(chart_data)
  assert_true(time_series_data.length() > 0)
  
  for series in time_series_data {
    let data_points = ChartSink::get_series_points(series)
    assert_true(data_points.length() > 0)
    
    // Verify time series has timestamps and values
    for point in data_points {
      let timestamp = ChartSink::get_point_timestamp(point)
      let value = ChartSink::get_point_value(point)
      
      assert_true(timestamp >= base_timestamp)
      assert_true(value >= 0.0)
    }
  }
  
  // Verify histogram chart data
  let histogram_data = ChartSink::get_histogram_data(chart_data)
  assert_true(histogram_data.length() > 0)
  
  for histogram in histogram_data {
    let buckets = ChartSink::get_histogram_buckets(histogram)
    assert_true(buckets.length() > 0)
    
    // Verify histogram has buckets and counts
    for bucket in buckets {
      let bucket_min = ChartSink::get_bucket_min(bucket)
      let bucket_max = ChartSink::get_bucket_max(bucket)
      let bucket_count = ChartSink::get_bucket_count(bucket)
      
      assert_true(bucket_max > bucket_min)
      assert_true(bucket_count >= 0)
    }
  }
  
  // Verify heatmap data
  let heatmap_data = ChartSink::get_heatmap_data(chart_data)
  assert_true(heatmap_data.length() > 0)
  
  for heatmap in heatmap_data {
    let cells = ChartSink::get_heatmap_cells(heatmap)
    assert_true(cells.length() > 0)
    
    // Verify heatmap has cells with coordinates and values
    for cell in cells {
      let x = ChartSink::get_cell_x(cell)
      let y = ChartSink::get_cell_y(cell)
      let value = ChartSink::get_cell_value(cell)
      
      assert_true(x >= 0)
      assert_true(y >= 0)
      assert_true(value >= 0.0)
    }
  }
  
  // Verify visualization statistics
  let viz_stats = VisualizationProcessor::get_statistics(visualization_processor)
  assert_true(viz_stats.total_processed > 200)
  assert_true(viz_stats.chart_updates > 0)
  assert_true(viz_stats.avg_render_time_ms < 50.0) // Should render quickly
}