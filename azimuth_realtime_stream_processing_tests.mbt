// Azimuth Real-time Stream Processing Tests
// This file contains test cases for real-time telemetry data stream processing

test "basic stream processing" {
  // Create stream processor
  let processor = StreamProcessor::new()
  
  // Create test data stream
  let stream = Stream::new()
  
  // Add telemetry data to stream
  for i in 0..10 {
    let span_ctx = SpanContext::new("stream_trace_" + i.to_string(), "stream_span_" + i.to_string(), true, "")
    let span = Span::new("stream_span_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "stream_id", IntValue(i))
    Attributes::set(attrs, "stream_value", StringValue("value_" + i.to_string()))
    
    Span::add_event(span, "stream_event", Some(attrs))
    
    // Add to stream
    Stream::add_data(stream, span)
  }
  
  // Process stream
  let processed_data = StreamProcessor::process(processor, stream)
  
  // Verify all data was processed
  assert_eq(processed_data.length(), 10)
  
  // Verify processed data is valid
  for data in processed_data {
    assert_true(Span::is_valid(data))
    
    let name = Span::name(data)
    assert_true(name.contains("stream_span_"))
  }
}

test "real-time metrics aggregation" {
  // Create metrics aggregator
  let aggregator = MetricsAggregator::new()
  
  // Create metrics stream
  let metrics_stream = Stream::new()
  
  // Add metrics to stream
  for i in 0..100 {
    let metric = Metric::new(
      "test_metric_" + (i % 5).to_string(), // 5 different metric names
      i.to_float(),
      Attributes::new()
    )
    
    Stream::add_data(metrics_stream, metric)
  }
  
  // Aggregate metrics in real-time
  let aggregated_metrics = MetricsAggregator::aggregate(aggregator, metrics_stream)
  
  // Verify aggregation results
  assert_eq(aggregated_metrics.length(), 5) // Should have 5 different metrics
  
  for metric in aggregated_metrics {
    let name = Metric::name(metric)
    assert_true(name.contains("test_metric_"))
    
    // Verify each metric was aggregated from 20 values (100 / 5)
    let value_count = Metric::value_count(metric)
    assert_eq(value_count, 20)
    
    // Verify aggregation statistics
    let avg = Metric::average(metric)
    let min = Metric::minimum(metric)
    let max = Metric::maximum(metric)
    
    assert_true(avg >= min && avg <= max)
  }
}

test "stream filtering and transformation" {
  // Create stream processor with filters and transformers
  let processor = StreamProcessor::new()
  
  // Add filter for specific trace IDs
  StreamProcessor::add_filter(processor, fn(data) {
    let ctx = Span::span_context(data)
    let trace_id = SpanContext::trace_id(ctx)
    trace_id.contains("filter")
  })
  
  // Add transformer to normalize attribute values
  StreamProcessor::add_transformer(processor, fn(data) {
    let attrs = Span::attributes(data)
    let normalized_attrs = Attributes::new()
    
    // Copy all attributes with normalized keys
    for (key, value) in Attributes::to_list(attrs) {
      let normalized_key = String::lowercase(key)
      Attributes::set(normalized_attrs, normalized_key, value)
    }
    
    // Replace attributes with normalized ones
    Span::set_attributes(data, normalized_attrs)
    
    data
  })
  
  // Create mixed data stream
  let stream = Stream::new()
  
  // Add data that should be filtered
  for i in 0..5 {
    let span_ctx = SpanContext::new("filter_trace_" + i.to_string(), "filter_span_" + i.to_string(), true, "")
    let span = Span::new("filter_span_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "MixedCase_Key", StringValue("MixedCase_Value"))
    
    Span::add_event(span, "filter_event", Some(attrs))
    
    Stream::add_data(stream, span)
  }
  
  // Add data that should be filtered out
  for i in 0..5 {
    let span_ctx = SpanContext::new("other_trace_" + i.to_string(), "other_span_" + i.to_string(), true, "")
    let span = Span::new("other_span_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "MixedCase_Key", StringValue("MixedCase_Value"))
    
    Span::add_event(span, "other_event", Some(attrs))
    
    Stream::add_data(stream, span)
  }
  
  // Process stream
  let processed_data = StreamProcessor::process(processor, stream)
  
  // Verify only filtered data was processed
  assert_eq(processed_data.length(), 5)
  
  // Verify transformation was applied
  for data in processed_data {
    let attrs = Span::attributes(data)
    
    // Check for normalized key
    let normalized_attr = Attributes::get(attrs, "mixedcase_key")
    match normalized_attr {
      StringValue(v) => assert_eq(v, "MixedCase_Value")
      _ => assert_true(false)
    }
    
    // Check original key is gone
    let original_attr = Attributes::get(attrs, "MixedCase_Key")
    match original_attr {
      Some(_) => assert_true(false) // Should be normalized
      None => assert_true(true)
    }
  }
}

test "windowed stream processing" {
  // Create windowed stream processor
  let processor = WindowedStreamProcessor::new()
  
  // Set up time windows (5-second windows)
  WindowedStreamProcessor::set_window_size(processor, 5000)
  
  // Create time-based data stream
  let stream = Stream::new()
  let base_time = Time::now()
  
  // Add data across different time windows
  for i in 0..20 {
    let span_ctx = SpanContext::new("window_trace_" + i.to_string(), "window_span_" + i.to_string(), true, "")
    let span = Span::new("window_span_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "window_index", IntValue(i / 5)) // 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, ...
    Attributes::set(attrs, "timestamp", IntValue(base_time + i * 1000)) // 1 second apart
    
    Span::add_event(span, "window_event", Some(attrs))
    
    Stream::add_data(stream, span)
  }
  
  // Process with windows
  let windowed_results = WindowedStreamProcessor::process(processor, stream)
  
  // Verify we have 4 windows (20 items / 5 per window)
  assert_eq(windowed_results.length(), 4)
  
  // Verify each window has correct data
  for i in 0..4 {
    let window = windowed_results[i]
    assert_eq(Window::size(window), 5)
    
    // Verify all items in window have correct window_index
    for data in Window::data(window) {
      let attrs = Span::attributes(data)
      let window_index = Attributes::get(attrs, "window_index")
      match window_index {
        IntValue(index) => assert_eq(index, i)
        _ => assert_true(false)
      }
    }
  }
}

test "stream anomaly detection" {
  // Create anomaly detector
  let detector = AnomalyDetector::new()
  
  // Configure anomaly detection
  AnomalyDetector::set_threshold(detector, 2.0) // 2 standard deviations
  AnomalyDetector::set_window_size(detector, 10) // Use last 10 values
  
  // Create metrics stream with anomalies
  let metrics_stream = Stream::new()
  
  // Add normal values (around 100)
  for i in 0..15 {
    let value = 100.0 + (Random::normal() * 10.0) // Normal distribution around 100
    let metric = Metric::new("anomaly_test", value, Attributes::new())
    Stream::add_data(metrics_stream, metric)
  }
  
  // Add anomalous values
  let anomalous_values = [200.0, 250.0, 50.0, 0.0, 300.0]
  for value in anomalous_values {
    let metric = Metric::new("anomaly_test", value, Attributes::new())
    Stream::add_data(metrics_stream, metric)
  }
  
  // Detect anomalies
  let anomalies = AnomalyDetector::detect(detector, metrics_stream)
  
  // Verify anomalies were detected
  assert_true(anomalies.length() > 0)
  
  // Verify detected anomalies are actually anomalous
  for anomaly in anomalies {
    let value = Anomaly::value(anomaly)
    assert_true(value > 150.0 || value < 50.0) // Should be outside normal range
    
    // Verify anomaly metadata
    let metric_name = Anomaly::metric_name(anomaly)
    assert_eq(metric_name, "anomaly_test")
    
    let score = Anomaly::score(anomaly)
    assert_true(score > 2.0) // Should exceed threshold
  }
}

test "real-time alerting" {
  // Create alert manager
  let alert_manager = AlertManager::new()
  
  // Configure alert conditions
  AlertManager::add_condition(alert_manager, "high_error_rate", fn(data) {
    let attrs = Span::attributes(data)
    let status = Attributes::get(attrs, "status")
    
    match status {
      StringValue(s) => s == "error"
      _ => false
    }
  }, 5, 10000) // 5 errors in 10 seconds
  
  AlertManager::add_condition(alert_manager, "high_latency", fn(data) {
    let attrs = Span::attributes(data)
    let latency = Attributes::get(attrs, "latency")
    
    match latency {
      IntValue(l) => l > 1000 // > 1000ms
      _ => false
    }
  }, 3, 5000) // 3 high latency events in 5 seconds
  
  // Create event stream
  let event_stream = Stream::new()
  
  // Add normal events
  for i in 0..10 {
    let span_ctx = SpanContext::new("alert_trace_" + i.to_string(), "alert_span_" + i.to_string(), true, "")
    let span = Span::new("alert_span_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "status", StringValue("success"))
    Attributes::set(attrs, "latency", IntValue(100 + i * 10)) // 100-190ms
    
    Span::add_event(span, "normal_event", Some(attrs))
    
    Stream::add_data(event_stream, span)
  }
  
  // Add error events (should trigger alert)
  for i in 0..5 {
    let span_ctx = SpanContext::new("alert_trace_error_" + i.to_string(), "alert_span_error_" + i.to_string(), true, "")
    let span = Span::new("alert_span_error_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "status", StringValue("error"))
    Attributes::set(attrs, "latency", IntValue(50))
    
    Span::add_event(span, "error_event", Some(attrs))
    
    Stream::add_data(event_stream, span)
  }
  
  // Add high latency events (should trigger alert)
  for i in 0..3 {
    let span_ctx = SpanContext::new("alert_trace_latency_" + i.to_string(), "alert_span_latency_" + i.to_string(), true, "")
    let span = Span::new("alert_span_latency_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "status", StringValue("success"))
    Attributes::set(attrs, "latency", IntValue(1500 + i * 100)) // 1500-1700ms
    
    Span::add_event(span, "latency_event", Some(attrs))
    
    Stream::add_data(event_stream, span)
  }
  
  // Process events and check for alerts
  let alerts = AlertManager::process_events(alert_manager, event_stream)
  
  // Verify alerts were generated
  assert_true(alerts.length() >= 2) // Should have at least 2 alerts (error rate and high latency)
  
  // Verify alert content
  let error_alerts = []
  let latency_alerts = []
  
  for alert in alerts {
    let alert_type = Alert::type(alert)
    
    if alert_type == "high_error_rate" {
      error_alerts.push(alert)
    } else if alert_type == "high_latency" {
      latency_alerts.push(alert)
    }
  }
  
  assert_true(error_alerts.length() > 0)
  assert_true(latency_alerts.length() > 0)
  
  // Verify error alert details
  for alert in error_alerts {
    let count = Alert::count(alert)
    assert_true(count >= 5)
    
    let message = Alert::message(alert)
    assert_true(message.contains("high_error_rate"))
  }
  
  // Verify latency alert details
  for alert in latency_alerts {
    let count = Alert::count(alert)
    assert_true(count >= 3)
    
    let message = Alert::message(alert)
    assert_true(message.contains("high_latency"))
  }
}

test "stream backpressure handling" {
  // Create stream processor with backpressure handling
  let processor = StreamProcessor::with_backpressure_handling()
  
  // Set backpressure threshold
  StreamProcessor::set_backpressure_threshold(processor, 100) // Max 100 items in buffer
  
  // Create high-volume data stream
  let stream = Stream::new()
  
  // Add large amount of data (more than threshold)
  for i in 0..500 {
    let span_ctx = SpanContext::new("backpressure_trace_" + i.to_string(), "backpressure_span_" + i.to_string(), true, "")
    let span = Span::new("backpressure_span_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "backpressure_id", IntValue(i))
    
    Span::add_event(span, "backpressure_event", Some(attrs))
    
    Stream::add_data(stream, span)
  }
  
  // Process stream with backpressure
  let processed_data = StreamProcessor::process_with_backpressure(processor, stream)
  
  // Verify backpressure was applied
  assert_true(StreamProcessor::backpressure_was_applied(processor))
  
  // Verify data was not lost
  assert_true(processed_data.length() > 0)
  assert_true(processed_data.length() <= 500)
  
  // Verify processed data is valid
  for data in processed_data {
    assert_true(Span::is_valid(data))
    
    let attrs = Span::attributes(data)
    let backpressure_id = Attributes::get(attrs, "backpressure_id")
    
    match backpressure_id {
      IntValue(id) => assert_true(id >= 0 && id < 500)
      _ => assert_true(false)
    }
  }
}

test "stream persistence and recovery" {
  // Create persistent stream processor
  let processor = PersistentStreamProcessor::new("/tmp/azimuth_stream_test")
  
  // Create data stream
  let stream = Stream::new()
  
  // Add data to stream
  for i in 0..20 {
    let span_ctx = SpanContext::new("persistence_trace_" + i.to_string(), "persistence_span_" + i.to_string(), true, "")
    let span = Span::new("persistence_span_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "persistence_id", IntValue(i))
    Attributes::set(attrs, "persistence_value", StringValue("value_" + i.to_string()))
    
    Span::add_event(span, "persistence_event", Some(attrs))
    
    Stream::add_data(stream, span)
  }
  
  // Process stream with persistence
  let checkpoint_id = PersistentStreamProcessor::process_with_checkpoint(processor, stream)
  assert_true(checkpoint_id.length() > 0)
  
  // Verify checkpoint was created
  assert_true(PersistentStreamProcessor::checkpoint_exists(processor, checkpoint_id))
  
  // Simulate failure and recovery
  let recovered_processor = PersistentStreamProcessor::new("/tmp/azimuth_stream_test")
  
  // Recover from checkpoint
  let recovery_result = PersistentStreamProcessor::recover_from_checkpoint(recovered_processor, checkpoint_id)
  assert_true(recovery_result)
  
  // Verify recovered state
  let recovered_data = PersistentStreamProcessor::get_processed_data(recovered_processor)
  assert_eq(recovered_data.length(), 20)
  
  // Verify recovered data is valid
  for data in recovered_data {
    assert_true(Span::is_valid(data))
    
    let attrs = Span::attributes(data)
    let persistence_id = Attributes::get(attrs, "persistence_id")
    
    match persistence_id {
      IntValue(id) => assert_true(id >= 0 && id < 20)
      _ => assert_true(false)
    }
  }
  
  // Clean up
  PersistentStreamProcessor::cleanup(recovered_processor)
}

test "multi-stream correlation" {
  // Create multi-stream processor
  let processor = MultiStreamProcessor::new()
  
  // Create multiple streams
  let span_stream = Stream::new()
  let metric_stream = Stream::new()
  let log_stream = Stream::new()
  
  // Add correlated data to streams
  for i in 0..10 {
    let correlation_id = "correlation_" + i.to_string()
    
    // Add span
    let span_ctx = SpanContext::new("span_trace_" + i.to_string(), "span_span_" + i.to_string(), true, "")
    let span = Span::new("span_" + i.to_string(), Internal, span_ctx)
    
    let span_attrs = Attributes::new()
    Attributes::set(span_attrs, "correlation_id", StringValue(correlation_id))
    Attributes::set(span_attrs, "span_data", StringValue("span_value_" + i.to_string()))
    
    Span::add_event(span, "span_event", Some(span_attrs))
    Stream::add_data(span_stream, span)
    
    // Add metric
    let metric = Metric::new(
      "metric_" + i.to_string(),
      i.to_float() * 10.0,
      Attributes::new()
    )
    
    let metric_attrs = Metric::attributes(metric)
    Attributes::set(metric_attrs, "correlation_id", StringValue(correlation_id))
    Attributes::set(metric_attrs, "metric_data", StringValue("metric_value_" + i.to_string()))
    
    Stream::add_data(metric_stream, metric)
    
    // Add log
    let log_record = LogRecord::new(
      Info,
      Some("Log message " + i.to_string()),
      Some(Attributes::new()),
      Some(1234567890L + i),
      Some(1234567891L + i),
      Some("log_trace_" + i.to_string()),
      Some("log_span_" + i.to_string())
    )
    
    let log_attrs = LogRecord::attributes(log_record)
    Attributes::set(log_attrs, "correlation_id", StringValue(correlation_id))
    Attributes::set(log_attrs, "log_data", StringValue("log_value_" + i.to_string()))
    
    Stream::add_data(log_stream, log_record)
  }
  
  // Process streams with correlation
  let correlated_results = MultiStreamProcessor::process_with_correlation(
    processor,
    [span_stream, metric_stream, log_stream]
  )
  
  // Verify correlation results
  assert_eq(correlated_results.length(), 10)
  
  // Verify each correlation group has correct data
  for correlation_group in correlated_results {
    let correlation_id = CorrelationGroup::id(correlation_group)
    assert_true(correlation_id.contains("correlation_"))
    
    // Verify each group has span, metric, and log
    assert_true(CorrelationGroup::has_span(correlation_group))
    assert_true(CorrelationGroup::has_metric(correlation_group))
    assert_true(CorrelationGroup::has_log(correlation_group))
    
    // Verify correlation IDs match
    let span = CorrelationGroup::span(correlation_group)
    let metric = CorrelationGroup::metric(correlation_group)
    let log = CorrelationGroup::log(correlation_group)
    
    let span_attrs = Span::attributes(span)
    let metric_attrs = Metric::attributes(metric)
    let log_attrs = LogRecord::attributes(log)
    
    let span_correlation_id = Attributes::get(span_attrs, "correlation_id")
    let metric_correlation_id = Attributes::get(metric_attrs, "correlation_id")
    let log_correlation_id = Attributes::get(log_attrs, "correlation_id")
    
    match (span_correlation_id, metric_correlation_id, log_correlation_id) {
      (StringValue(span_id), StringValue(metric_id), StringValue(log_id)) => {
        assert_eq(span_id, metric_id)
        assert_eq(metric_id, log_id)
      }
      _ => assert_true(false)
    }
  }
}