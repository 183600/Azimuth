// Azimuth Real-time Telemetry Data Processing Tests
// This file contains comprehensive test cases for real-time telemetry data processing

// Test 1: Real-time Stream Processing Pipeline
test "real-time stream processing pipeline" {
  // Create a real-time data processor
  let processor = azimuth::RealTimeProcessor::new(
    buffer_size = 1000,
    batch_size = 100,
    flush_interval_ms = 1000
  )
  
  // Configure processing pipeline
  azimuth::RealTimeProcessor::add_filter(processor, azimuth::Filter::by_attribute("service.name", "web-api"))
  azimuth::RealTimeProcessor::add_transformer(processor, azimuth::Transformer::normalize_timestamps())
  azimuth::RealTimeProcessor::add_aggregator(processor, azimuth::Aggregator::time_window(60000)) // 1-minute windows
  
  // Simulate real-time data stream
  let base_timestamp = 1640995200000
  let processed_data = []
  
  for i = 0; i < 500; i = i + 1 {
    let timestamp = base_timestamp + i * 1000 // 1-second intervals
    let data_point = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = "web-api",
      metric_name = "request.duration",
      metric_value = (100.0 + (i % 50).to_double() * 2.0),
      attributes = [
        ("http.method", "GET"),
        ("http.status", "200"),
        ("user.id", "user-" + (i % 10).to_string())
      ]
    )
    
    let result = azimuth::RealTimeProcessor::process(processor, data_point)
    if result.is_some {
      processed_data.push(result.unwrap())
    }
  }
  
  // Verify processing pipeline
  assert_true(processed_data.length() > 0)
  
  // Verify all processed data has normalized timestamps
  for data in processed_data {
    assert_eq(data.timestamp % 1000, 0) // Should be normalized to second boundaries
    assert_eq(data.service_name, "web-api") // Filter should only allow this service
  }
  
  // Verify aggregation results
  let aggregated_results = azimuth::RealTimeProcessor::get_aggregated_data(processor)
  assert_true(aggregated_results.length() > 0)
  
  // Each aggregated result should represent a time window
  for agg in aggregated_results {
    assert_true(agg.window_size_ms > 0)
    assert_true(agg.data_points.length() > 0)
    assert_true(agg.start_timestamp < agg.end_timestamp)
  }
}

// Test 2: Real-time Anomaly Detection
test "real-time anomaly detection in streaming data" {
  // Create anomaly detector for real-time streams
  let detector = azimuth::RealTimeAnomalyDetector::new(
    window_size = 100,
    threshold = 2.5, // 2.5 sigma
    min_points = 30
  )
  
  // Simulate normal data stream with occasional anomalies
  let base_timestamp = 1640995200000
  let detected_anomalies = []
  
  for i = 0; i < 200; i = i + 1 {
    let timestamp = base_timestamp + i * 5000 // 5-second intervals
    let mut value = 100.0 + 10.0 * ((i % 20).to_double() / 20.0)
    
    // Inject anomalies at specific points
    if i == 50 || i == 100 || i == 150 {
      value = value + 100.0 // Major spike
    }
    
    let data_point = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = "payment-service",
      metric_name = "transaction.amount",
      metric_value = value,
      attributes = [
        ("transaction.type", "payment"),
        ("currency", "USD")
      ]
    )
    
    let anomaly_result = azimuth::RealTimeAnomalyDetector::process(detector, data_point)
    if anomaly_result.is_anomaly {
      detected_anomalies.push(anomaly_result)
    }
  }
  
  // Verify anomaly detection
  assert_eq(detected_anomalies.length(), 3)
  
  // Verify anomaly timestamps
  assert_eq(detected_anomalies[0].timestamp, base_timestamp + 50 * 5000)
  assert_eq(detected_anomalies[1].timestamp, base_timestamp + 100 * 5000)
  assert_eq(detected_anomalies[2].timestamp, base_timestamp + 150 * 5000)
  
  // Verify anomaly scores
  for anomaly in detected_anomalies {
    assert_true(anomaly.score > 2.5) // Above threshold
    assert_true(anomaly.expected_value > 90.0) // Based on normal range
    assert_true(anomaly.actual_value > 190.0) // Anomaly values
  }
  
  // Test adaptive threshold adjustment
  azimuth::RealTimeAnomalyDetector::adjust_threshold(detector, 3.0) // Increase threshold
  
  // Process the same data again with higher threshold
  let adaptive_anomalies = []
  for i = 0; i < 200; i = i + 1 {
    let timestamp = base_timestamp + i * 5000
    let mut value = 100.0 + 10.0 * ((i % 20).to_double() / 20.0)
    
    if i == 50 || i == 100 || i == 150 {
      value = value + 100.0
    }
    
    let data_point = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = "payment-service",
      metric_name = "transaction.amount",
      metric_value = value,
      attributes = []
    )
    
    let anomaly_result = azimuth::RealTimeAnomalyDetector::process(detector, data_point)
    if anomaly_result.is_anomaly {
      adaptive_anomalies.push(anomaly_result)
    }
  }
  
  // With higher threshold, fewer anomalies should be detected
  assert_true(adaptive_anomalies.length() <= detected_anomalies.length())
}

// Test 3: Real-time Alerting and Notifications
test "real-time alerting and notification system" {
  // Create alert manager
  let alert_manager = azimuth::AlertManager::new()
  
  // Configure alert rules
  let high_latency_rule = azimuth::AlertRule::new(
    name = "high_request_latency",
    condition = azimuth::Condition::greater_than("request.duration", 1000.0),
    severity = azimuth::Severity::Warning,
    cooldown_ms = 30000 // 30 seconds cooldown
  )
  
  let error_rate_rule = azimuth::AlertRule::new(
    name = "high_error_rate",
    condition = azimuth::Condition::percentage_above("http.status", "4xx", 10.0),
    severity = azimuth::Severity::Critical,
    cooldown_ms = 60000 // 1 minute cooldown
  )
  
  azimuth::AlertManager::add_rule(alert_manager, high_latency_rule)
  azimuth::AlertManager::add_rule(alert_manager, error_rate_rule)
  
  // Configure notification channels
  let email_channel = azimuth::NotificationChannel::email(
    recipients = ["ops-team@example.com"],
    template = "alert-email"
  )
  
  let slack_channel = azimuth::NotificationChannel::slack(
    webhook_url = "https://hooks.slack.com/test",
    channel = "#alerts"
  )
  
  azimuth::AlertManager::add_channel(alert_manager, email_channel)
  azimuth::AlertManager::add_channel(alert_manager, slack_channel)
  
  // Simulate telemetry data that triggers alerts
  let base_timestamp = 1640995200000
  let triggered_alerts = []
  
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_timestamp + i * 10000 // 10-second intervals
    
    // Normal requests with occasional high latency
    let latency = if i >= 20 && i < 30 { 1500.0 } else { 200.0 }
    
    // Mix of success and error status codes
    let status_code = if i >= 40 && i < 50 { "404" } else { "200" }
    
    let data_point = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = "api-gateway",
      metric_name = "http.request",
      metric_value = 1.0,
      attributes = [
        ("request.duration", latency.to_string()),
        ("http.status", status_code),
        ("endpoint", "/api/users")
      ]
    )
    
    let alerts = azimuth::AlertManager::evaluate(alert_manager, data_point)
    for alert in alerts {
      triggered_alerts.push(alert)
    }
  }
  
  // Verify alert triggering
  assert_true(triggered_alerts.length() > 0)
  
  // Should have high latency alerts for indices 20-29
  let latency_alerts = triggered_alerts.filter(|a| a.rule_name == "high_request_latency")
  assert_true(latency_alerts.length() > 0)
  
  // Should have error rate alerts for indices 40-49
  let error_alerts = triggered_alerts.filter(|a| a.rule_name == "high_error_rate")
  assert_true(error_alerts.length() > 0)
  
  // Verify alert properties
  for alert in latency_alerts {
    assert_eq(alert.severity, azimuth::Severity::Warning)
    assert_true(alert.triggered_at >= base_timestamp + 20 * 10000)
    assert_true(alert.triggered_at <= base_timestamp + 30 * 10000)
  }
  
  for alert in error_alerts {
    assert_eq(alert.severity, azimuth::Severity::Critical)
    assert_true(alert.triggered_at >= base_timestamp + 40 * 10000)
    assert_true(alert.triggered_at <= base_timestamp + 50 * 10000)
  }
  
  // Test alert cooldown period
  let cooldown_test_data = azimuth::TelemetryData::new(
    timestamp = base_timestamp + 25 * 10000, // Within latency alert window
    service_name = "api-gateway",
    metric_name = "http.request",
    metric_value = 1.0,
    attributes = [
      ("request.duration", "1500.0"),
      ("http.status", "200"),
      ("endpoint", "/api/users")
    ]
  )
  
  let cooldown_alerts = azimuth::AlertManager::evaluate(alert_manager, cooldown_test_data)
  // Should not trigger new alert due to cooldown
  assert_true(cooldown_alerts.length() == 0)
}

// Test 4: Real-time Metrics Aggregation
test "real-time metrics aggregation with multiple dimensions" {
  // Create real-time metrics aggregator
  let aggregator = azimuth::RealTimeMetricsAggregator::new(
    window_size_ms = 60000, // 1-minute windows
    max_dimensions = 10
  )
  
  // Configure aggregations
  azimuth::RealTimeMetricsAggregator::add_counter(aggregator, "http.requests", ["http.method", "http.status"])
  azimuth::RealTimeMetricsAggregator::add_histogram(aggregator, "request.duration", ["service.name", "endpoint"])
  azimuth::RealTimeMetricsAggregator::add_gauge(aggregator, "active.connections", ["service.name"])
  
  // Simulate real-time metrics data
  let base_timestamp = 1640995200000
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let statuses = ["200", "201", "400", "404", "500"]
  let services = ["user-service", "order-service", "payment-service"]
  let endpoints = ["/api/users", "/api/orders", "/api/payments"]
  
  for i = 0; i < 1000; i = i + 1 {
    let timestamp = base_timestamp + i * 1000 // 1-second intervals
    
    // Counter metric: HTTP requests
    let method = methods[i % methods.length()]
    let status = statuses[i % statuses.length()]
    
    let counter_data = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = "api-gateway",
      metric_name = "http.requests",
      metric_value = 1.0,
      attributes = [
        ("http.method", method),
        ("http.status", status)
      ]
    )
    
    // Histogram metric: Request duration
    let service = services[i % services.length()]
    let endpoint = endpoints[i % endpoints.length()]
    let duration = 50.0 + (i % 200).to_double()
    
    let histogram_data = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = service,
      metric_name = "request.duration",
      metric_value = duration,
      attributes = [
        ("endpoint", endpoint)
      ]
    )
    
    // Gauge metric: Active connections
    let gauge_service = services[i % services.length()]
    let connections = 10 + (i % 50)
    
    let gauge_data = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = gauge_service,
      metric_name = "active.connections",
      metric_value = connections.to_double(),
      attributes = []
    )
    
    azimuth::RealTimeMetricsAggregator::process(aggregator, counter_data)
    azimuth::RealTimeMetricsAggregator::process(aggregator, histogram_data)
    azimuth::RealTimeMetricsAggregator::process(aggregator, gauge_data)
  }
  
  // Get aggregated results
  let counter_results = azimuth::RealTimeMetricsAggregator::get_counter_results(aggregator, "http.requests")
  let histogram_results = azimuth::RealTimeMetricsAggregator::get_histogram_results(aggregator, "request.duration")
  let gauge_results = azimuth::RealTimeMetricsAggregator::get_gauge_results(aggregator, "active.connections")
  
  // Verify counter aggregation
  assert_true(counter_results.length() > 0)
  
  // Should have results for each method/status combination
  for result in counter_results {
    assert_true(result.dimensions.contains("http.method"))
    assert_true(result.dimensions.contains("http.status"))
    assert_true(result.count > 0)
    assert_eq(result.metric_name, "http.requests")
  }
  
  // Verify histogram aggregation
  assert_true(histogram_results.length() > 0)
  
  for result in histogram_results {
    assert_true(result.dimensions.contains("service.name"))
    assert_true(result.dimensions.contains("endpoint"))
    assert_true(result.count > 0)
    assert_true(result.sum > 0.0)
    assert_true(result.min >= 0.0)
    assert_true(result.max >= result.min)
    assert_true(result.avg >= result.min && result.avg <= result.max)
  }
  
  // Verify gauge aggregation
  assert_true(gauge_results.length() > 0)
  
  for result in gauge_results {
    assert_true(result.dimensions.contains("service.name"))
    assert_true(result.last_value >= 0.0)
    assert_true(result.min >= 0.0)
    assert_true(result.max >= result.min)
  }
  
  // Test percentile calculations
  let duration_percentiles = azimuth::RealTimeMetricsAggregator::get_percentiles(
    aggregator, 
    "request.duration", 
    [50.0, 90.0, 95.0, 99.0]
  )
  
  assert_eq(duration_percentiles.length(), 4)
  
  // Verify percentile ordering
  assert_true(duration_percentiles[0].value <= duration_percentiles[1].value) // 50th <= 90th
  assert_true(duration_percentiles[1].value <= duration_percentiles[2].value) // 90th <= 95th
  assert_true(duration_percentiles[2].value <= duration_percentiles[3].value) // 95th <= 99th
}

// Test 5: Real-time Data Buffering and Batching
test "real-time data buffering and batching strategies" {
  // Test size-based buffering
  let size_buffer = azimuth::RealTimeBuffer::new(
    buffer_type = azimuth::BufferType::Size,
    max_size = 50,
    flush_timeout_ms = 10000
  )
  
  let base_timestamp = 1640995200000
  let size_batches = []
  
  // Add data points and collect batches
  for i = 0; i < 200; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = base_timestamp + i * 1000,
      service_name = "test-service",
      metric_name = "test.metric",
      metric_value = i.to_double(),
      attributes = [("index", i.to_string())]
    )
    
    let batch = azimuth::RealTimeBuffer::add(size_buffer, data_point)
    if batch.is_some {
      size_batches.push(batch.unwrap())
    }
  }
  
  // Should have 4 batches of 50 items each
  assert_eq(size_batches.length(), 4)
  for batch in size_batches {
    assert_eq(batch.length(), 50)
  }
  
  // Test time-based buffering
  let time_buffer = azimuth::RealTimeBuffer::new(
    buffer_type = azimuth::BufferType::Time,
    max_size = 1000, // Large size to force time-based flushing
    flush_timeout_ms = 5000 // 5 seconds
  )
  
  let time_batches = []
  
  // Add data points with delays to trigger time-based flushing
  for i = 0; i < 20; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = base_timestamp + i * 1000,
      service_name = "test-service",
      metric_name = "test.metric",
      metric_value = i.to_double(),
      attributes = []
    )
    
    let batch = azimuth::RealTimeBuffer::add(time_buffer, data_point)
    if batch.is_some {
      time_batches.push(batch.unwrap())
    }
    
    // Simulate time passing (in real implementation, this would be actual time)
    if i % 5 == 4 {
      // Trigger flush by simulating timeout
      let remaining = azimuth::RealTimeBuffer::flush(time_buffer)
      if remaining.length() > 0 {
        time_batches.push(remaining)
      }
    }
  }
  
  // Verify time-based batching
  assert_true(time_batches.length() > 0)
  for batch in time_batches {
    assert_true(batch.length() > 0)
  }
  
  // Test hybrid buffering (size or time, whichever comes first)
  let hybrid_buffer = azimuth::RealTimeBuffer::new(
    buffer_type = azimuth::BufferType::Hybrid,
    max_size = 30,
    flush_timeout_ms = 8000
  )
  
  let hybrid_batches = []
  
  for i = 0; i < 100; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = base_timestamp + i * 1000,
      service_name = "test-service",
      metric_name = "test.metric",
      metric_value = i.to_double(),
      attributes = []
    )
    
    let batch = azimuth::RealTimeBuffer::add(hybrid_buffer, data_point)
    if batch.is_some {
      hybrid_batches.push(batch.unwrap())
    }
    
    // Simulate occasional time-based flushes
    if i % 25 == 24 {
      let remaining = azimuth::RealTimeBuffer::flush(hybrid_buffer)
      if remaining.length() > 0 {
        hybrid_batches.push(remaining)
      }
    }
  }
  
  // Verify hybrid batching
  assert_true(hybrid_batches.length() > 0)
  
  // Batches should be either size-based (30 items) or time-based (variable items)
  for batch in hybrid_batches {
    assert_true(batch.length() > 0)
    assert_true(batch.length() <= 30) // Should not exceed max size
  }
}

// Test 6: Real-time Data Compression
test "real-time data compression for high-volume streams" {
  // Create real-time compressor
  let compressor = azimuth::RealTimeCompressor::new(
    compression_algorithm = azimuth::CompressionAlgorithm::Delta,
    compression_threshold = 100 // Compress after 100 points
  )
  
  // Generate high-frequency time series data
  let base_timestamp = 1640995200000
  let original_data = []
  
  for i = 0; i < 500; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = base_timestamp + i * 1000,
      service_name = "high-frequency-service",
      metric_name = "cpu.usage",
      metric_value = 50.0 + 20.0 * ((i % 60).to_double() / 60.0),
      attributes = [("host", "server-" + (i % 5).to_string())]
    )
    
    original_data.push(data_point)
    
    let compressed = azimuth::RealTimeCompressor::process(compressor, data_point)
    
    // Compression should happen every 100 points
    if (i + 1) % 100 == 0 {
      assert_true(compressed.is_compressed)
      assert_true(compressed.compression_ratio < 1.0)
    }
  }
  
  // Test decompression accuracy
  let compression_results = azimuth::RealTimeCompressor::get_compressed_data(compressor)
  assert_eq(compression_results.length(), 5) // 5 compressed batches
  
  for compressed_batch in compression_results {
    let decompressed = azimuth::RealTimeCompressor::decompress(compressor, compressed_batch)
    assert_eq(decompressed.length(), compressed_batch.original_count)
    
    // Verify decompression accuracy
    for i = 0; i < decompressed.length(); i = i + 1 {
      let original = compressed_batch.original_data[i]
      let restored = decompressed[i]
      
      assert_eq(restored.timestamp, original.timestamp)
      assert_eq(restored.service_name, original.service_name)
      assert_eq(restored.metric_name, original.metric_name)
      assert_true((restored.metric_value - original.metric_value).abs() < 0.001)
    }
  }
  
  // Test adaptive compression (chooses best algorithm based on data characteristics)
  let adaptive_compressor = azimuth::RealTimeCompressor::new(
    compression_algorithm = azimuth::CompressionAlgorithm::Adaptive,
    compression_threshold = 50
  )
  
  // Add mixed data patterns
  for i = 0; i < 300; i = i + 1 {
    let mut value = 100.0
    
    // Different patterns for different ranges
    if i < 100 {
      // Smooth data - good for delta compression
      value = value + 0.1 * i.to_double()
    } else if i < 200 {
      // Repetitive data - good for dictionary compression
      value = 100.0 + (i % 10).to_double() * 5.0
    } else {
      // Random data - may need different approach
      value = 100.0 + 50.0 * ((i * 7) % 13).to_double() / 13.0
    }
    
    let data_point = azimuth::TelemetryData::new(
      timestamp = base_timestamp + i * 1000,
      service_name = "adaptive-test",
      metric_name = "test.metric",
      metric_value = value,
      attributes = []
    )
    
    azimuth::RealTimeCompressor::process(adaptive_compressor, data_point)
  }
  
  let adaptive_results = azimuth::RealTimeCompressor::get_compressed_data(adaptive_compressor)
  assert_eq(adaptive_results.length(), 6) // 6 compressed batches
  
  // Verify adaptive algorithm selection
  for i = 0; i < adaptive_results.length(); i = i + 1 {
    let batch = adaptive_results[i]
    
    // First batch (smooth data) should use delta compression
    if i == 0 {
      assert_eq(batch.algorithm_used, azimuth::CompressionAlgorithm::Delta)
    }
    // Second batch (repetitive data) should use dictionary compression
    else if i == 2 {
      assert_eq(batch.algorithm_used, azimuth::CompressionAlgorithm::Dictionary)
    }
  }
}

// Test 7: Real-time Data Filtering and Routing
test "real-time data filtering and routing to multiple destinations" {
  // Create data router with multiple routes
  let router = azimuth::RealTimeRouter::new()
  
  // Configure routes based on data characteristics
  let high_priority_route = azimuth::Route::new(
    name = "high-priority",
    condition = azimuth::Condition::equals("priority", "high"),
    destinations = ["high-priority-storage", "real-time-dashboard"],
    buffer_size = 100
  )
  
  let error_route = azimuth::Route::new(
    name = "errors",
    condition = azimuth::Condition::regex_match("log.level", "ERROR|FATAL"),
    destinations = ["error-storage", "alert-system"],
    buffer_size = 50
  )
  
  let metrics_route = azimuth::Route::new(
    name = "metrics",
    condition = azimuth::Condition::starts_with("metric.type", "counter|histogram"),
    destinations = ["metrics-storage", "aggregation-service"],
    buffer_size = 200
  )
  
  let default_route = azimuth::Route::new(
    name = "default",
    condition = azimuth::Condition::always_true(),
    destinations = ["general-storage"],
    buffer_size = 500
  )
  
  azimuth::RealTimeRouter::add_route(router, high_priority_route)
  azimuth::RealTimeRouter::add_route(router, error_route)
  azimuth::RealTimeRouter::add_route(router, metrics_route)
  azimuth::RealTimeRouter::add_route(router, default_route)
  
  // Simulate mixed telemetry data
  let base_timestamp = 1640995200000
  let routed_data = {
    "high-priority-storage": [],
    "real-time-dashboard": [],
    "error-storage": [],
    "alert-system": [],
    "metrics-storage": [],
    "aggregation-service": [],
    "general-storage": []
  }
  
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_timestamp + i * 1000
    let mut attributes = []
    let mut metric_name = "test.metric"
    let mut service_name = "test-service"
    
    // Create different types of data
    if i % 20 == 0 {
      // High priority data
      attributes.push(("priority", "high"))
      service_name = "critical-service"
    } else if i % 15 == 0 {
      // Error logs
      metric_name = "error.log"
      attributes.push(("log.level", "ERROR"))
    } else if i % 10 == 0 {
      // Metrics data
      metric_name = "counter.requests"
      attributes.push(("metric.type", "counter"))
    }
    
    let data_point = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = service_name,
      metric_name = metric_name,
      metric_value = i.to_double(),
      attributes = attributes
    )
    
    let routes = azimuth::RealTimeRouter::route(router, data_point)
    for route_name in routes {
      if routed_data.contains(route_name) {
        routed_data[route_name].push(data_point)
      }
    }
  }
  
  // Verify routing results
  // High priority data should go to high-priority destinations
  assert_true(routed_data["high-priority-storage"].length() > 0)
  assert_true(routed_data["real-time-dashboard"].length() > 0)
  
  // Error data should go to error destinations
  assert_true(routed_data["error-storage"].length() > 0)
  assert_true(routed_data["alert-system"].length() > 0)
  
  // Metrics data should go to metrics destinations
  assert_true(routed_data["metrics-storage"].length() > 0)
  assert_true(routed_data["aggregation-service"].length() > 0)
  
  // Default route should catch remaining data
  assert_true(routed_data["general-storage"].length() > 0)
  
  // Verify data integrity across routes
  let total_routed = 0
  for (_, data) in routed_data {
    total_routed = total_routed + data.length()
  }
  
  // Each data point should be routed to at least one destination
  assert_true(total_routed >= 100)
  
  // Test route priority and exclusivity
  let exclusive_router = azimuth::RealTimeRouter::new()
  azimuth::RealTimeRouter::set_exclusive_mode(exclusive_router, true) // Stop at first match
  
  let route1 = azimuth::Route::new(
    name = "route1",
    condition = azimuth::Condition::always_true(),
    destinations = ["dest1"],
    buffer_size = 100
  )
  
  let route2 = azimuth::Route::new(
    name = "route2",
    condition = azimuth::Condition::always_true(),
    destinations = ["dest2"],
    buffer_size = 100
  )
  
  azimuth::RealTimeRouter::add_route(exclusive_router, route1)
  azimuth::RealTimeRouter::add_route(exclusive_router, route2)
  
  let test_data = azimuth::TelemetryData::new(
    timestamp = base_timestamp,
    service_name = "test",
    metric_name = "test",
    metric_value = 1.0,
    attributes = []
  )
  
  let exclusive_routes = azimuth::RealTimeRouter::route(exclusive_router, test_data)
  assert_eq(exclusive_routes.length(), 1) // Should only match first route
  assert_eq(exclusive_routes[0], "dest1")
}

// Test 8: Real-time Data Validation and Quality Checks
test "real-time data validation and quality assurance" {
  // Create data validator
  let validator = azimuth::RealTimeValidator::new()
  
  // Configure validation rules
  let timestamp_rule = azimuth::ValidationRule::timestamp_range(
    min_timestamp = 1640995200000, // 2022-01-01
    max_timestamp = 1704067199000  // 2023-12-31
  )
  
  let metric_value_rule = azimuth::ValidationRule::numeric_range(
    field = "metric_value",
    min_value = 0.0,
    max_value = 10000.0
  )
  
  let required_attributes_rule = azimuth::ValidationRule::required_attributes([
    "service.name",
    "metric.name"
  ])
  
  let attribute_format_rule = azimuth::ValidationRule::attribute_format(
    attribute = "service.name",
    pattern = "^[a-z0-9-]+$" // Only lowercase letters, numbers, and hyphens
  )
  
  azimuth::RealTimeValidator::add_rule(validator, timestamp_rule)
  azimuth::RealTimeValidator::add_rule(validator, metric_value_rule)
  azimuth::RealTimeValidator::add_rule(validator, required_attributes_rule)
  azimuth::RealTimeValidator::add_rule(validator, attribute_format_rule)
  
  // Configure data quality metrics
  azimuth::RealTimeValidator::enable_quality_metrics(validator, [
    "completeness_score",
    "accuracy_score",
    "timeliness_score",
    "consistency_score"
  ])
  
  // Test with valid and invalid data
  let base_timestamp = 1640995200000
  let validation_results = []
  let quality_metrics = azimuth::RealTimeValidator::get_quality_metrics(validator)
  
  for i = 0; i < 50; i = i + 1 {
    let timestamp = base_timestamp + i * 60000
    let mut service_name = "valid-service"
    let mut metric_name = "valid.metric"
    let mut metric_value = 100.0
    let mut attributes = []
    
    // Inject various data quality issues
    match i % 10 {
      0 => {
        // Invalid timestamp (too old)
        timestamp = 1000000000000
      }
      1 => {
        // Invalid metric value (negative)
        metric_value = -10.0
      }
      2 => {
        // Missing required attribute
        metric_name = ""
      }
      3 => {
        // Invalid service name format
        service_name = "Invalid_Service_Name"
      }
      _ => {
        // Valid data
        attributes = [("environment", "production")]
      }
    }
    
    let data_point = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = service_name,
      metric_name = metric_name,
      metric_value = metric_value,
      attributes = attributes
    )
    
    let result = azimuth::RealTimeValidator::validate(validator, data_point)
    validation_results.push(result)
  }
  
  // Verify validation results
  assert_eq(validation_results.length(), 50)
  
  let valid_count = validation_results.filter(|r| r.is_valid).length()
  let invalid_count = validation_results.filter(|r| !r.is_valid).length()
  
  // Should have 60% valid (40% with injected issues)
  assert_eq(valid_count, 30)
  assert_eq(invalid_count, 20)
  
  // Verify specific validation failures
  for i = 0; i < validation_results.length(); i = i + 1 {
    let result = validation_results[i]
    
    match i % 10 {
      0 => {
        assert_false(result.is_valid)
        assert_true(result.errors.contains("timestamp_out_of_range"))
      }
      1 => {
        assert_false(result.is_valid)
        assert_true(result.errors.contains("metric_value_out_of_range"))
      }
      2 => {
        assert_false(result.is_valid)
        assert_true(result.errors.contains("missing_required_attribute"))
      }
      3 => {
        assert_false(result.is_valid)
        assert_true(result.errors.contains("invalid_attribute_format"))
      }
      _ => {
        assert_true(result.is_valid)
        assert_eq(result.errors.length(), 0)
      }
    }
  }
  
  // Check quality metrics
  let updated_quality_metrics = azimuth::RealTimeValidator::get_quality_metrics(validator)
  
  assert_true(updated_quality_metrics.contains("completeness_score"))
  assert_true(updated_quality_metrics.contains("accuracy_score"))
  assert_true(updated_quality_metrics.contains("timeliness_score"))
  assert_true(updated_quality_metrics.contains("consistency_score"))
  
  // Completeness should be 60% (30/40 valid required fields)
  assert_eq(updated_quality_metrics["completeness_score"], 0.6)
  
  // Accuracy should be 60% (30/50 valid data points)
  assert_eq(updated_quality_metrics["accuracy_score"], 0.6)
  
  // Test data correction and enrichment
  let correction_rules = [
    azimuth::CorrectionRule::default_value("environment", "unknown"),
    azimuth::CorrectionRule::timestamp_normalization(60000), // Round to minute
    azimuth::CorrectionRule::metric_value_clamping(0.0, 1000.0)
  ]
  
  azimuth::RealTimeValidator::add_correction_rules(validator, correction_rules)
  
  let test_data = azimuth::TelemetryData::new(
    timestamp = base_timestamp + 12345, // Not rounded to minute
    service_name = "test-service",
    metric_name = "test.metric",
    metric_value = 1500.0, // Above max
    attributes = [] // Missing environment
  )
  
  let corrected_data = azimuth::RealTimeValidator::correct_and_enrich(validator, test_data)
  
  // Verify corrections
  assert_eq(corrected_data.timestamp, base_timestamp + 12000) // Rounded to minute
  assert_eq(corrected_data.metric_value, 1000.0) // Clamped to max
  assert_true(corrected_data.attributes.contains(("environment", "unknown")))
}

// Test 9: Real-time Data Backpressure and Flow Control
test "real-time data backpressure and flow control mechanisms" {
  // Create flow controller with backpressure support
  let flow_controller = azimuth::FlowController::new(
    max_queue_size = 1000,
    backpressure_threshold = 800, // Start backpressure at 80% capacity
    recovery_threshold = 400      // Resume normal processing at 40% capacity
  )
  
  // Configure backpressure strategies
  let sampling_strategy = azimuth::BackpressureStrategy::sampling(0.5) // Sample 50% of data
  let priority_strategy = azimuth::BackpressureStrategy::priority_based([
    ("priority", "high"),   // High priority always accepted
    ("priority", "medium"), // Medium priority sampled
    ("priority", "low")     // Low priority dropped first
  ])
  let load_shedding_strategy = azimuth::BackpressureStrategy::load_shedding(0.2) // Drop 20%
  
  azimuth::FlowController::add_strategy(flow_controller, sampling_strategy)
  azimuth::FlowController::add_strategy(flow_controller, priority_strategy)
  azimuth::FlowController::add_strategy(flow_controller, load_shedding_strategy)
  
  // Simulate high-load scenario
  let base_timestamp = 1640995200000
  let processed_data = []
  let dropped_data = []
  let flow_control_events = []
  
  // Simulate incoming data burst
  for i = 0; i < 1200; i = i + 1 { // More than max queue size
    let timestamp = base_timestamp + i * 10 // High frequency
    
    let mut priority = "low"
    if i % 10 == 0 {
      priority = "high"
    } else if i % 5 == 0 {
      priority = "medium"
    }
    
    let data_point = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = "burst-service",
      metric_name = "burst.metric",
      metric_value = i.to_double(),
      attributes = [("priority", priority)]
    )
    
    let result = azimuth::FlowController::process(flow_controller, data_point)
    
    match result.action {
      azimuth::FlowAction::Process => {
        processed_data.push(data_point)
      }
      azimuth::FlowAction::Drop => {
        dropped_data.push(data_point)
      }
      azimuth::FlowAction::Sample => {
        // Sampled data (accepted but marked as sampled)
        let sampled_data = azimuth::TelemetryData::with_attributes(
          data_point,
          data_point.attributes + [("sampled", "true")]
        )
        processed_data.push(sampled_data)
      }
    }
    
    // Track flow control events
    if result.triggered_backpressure {
      flow_control_events.push((timestamp, "backpressure_on"))
    } else if result.backpressure_recovered {
      flow_control_events.push((timestamp, "backpressure_off"))
    }
  }
  
  // Verify flow control behavior
  assert_true(processed_data.length() > 0)
  assert_true(dropped_data.length() > 0)
  
  // Should have processed some data but not all (due to backpressure)
  assert_true(processed_data.length() < 1200)
  assert_true(dropped_data.length() > 0)
  
  // High priority data should have higher acceptance rate
  let high_priority_processed = processed_data.filter(|d| 
    d.attributes.contains(("priority", "high"))
  ).length()
  let low_priority_processed = processed_data.filter(|d| 
    d.attributes.contains(("priority", "low"))
  ).length()
  
  let high_priority_total = 1200 / 10 // 120 high priority items
  let low_priority_total = 1200 - (1200 / 10) - (1200 / 5) // 720 low priority items
  
  let high_priority_rate = high_priority_processed.to_double() / high_priority_total.to_double()
  let low_priority_rate = low_priority_processed.to_double() / low_priority_total.to_double()
  
  assert_true(high_priority_rate > low_priority_rate)
  
  // Verify backpressure events
  assert_true(flow_control_events.length() > 0)
  
  // Should have backpressure triggered when queue exceeded threshold
  let backpressure_on_events = flow_control_events.filter(|(_, event)| event == "backpressure_on")
  assert_true(backpressure_on_events.length() > 0)
  
  // Test adaptive flow control
  let adaptive_controller = azimuth::FlowController::new(
    max_queue_size = 500,
    backpressure_threshold = 400,
    recovery_threshold = 200
  )
  
  azimuth::FlowController::enable_adaptive_control(adaptive_controller, true)
  
  // Simulate varying load patterns
  let adaptive_processed = []
  let adaptive_dropped = []
  
  // Low load period
  for i = 0; i < 100; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = base_timestamp + i * 1000,
      service_name = "adaptive-test",
      metric_name = "test.metric",
      metric_value = i.to_double(),
      attributes = [("priority", "normal")]
    )
    
    let result = azimuth::FlowController::process(adaptive_controller, data_point)
    if result.action == azimuth::FlowAction::Process {
      adaptive_processed.push(data_point)
    } else {
      adaptive_dropped.push(data_point)
    }
  }
  
  // High load period
  for i = 100; i < 600; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = base_timestamp + i * 100, // Higher frequency
      service_name = "adaptive-test",
      metric_name = "test.metric",
      metric_value = i.to_double(),
      attributes = [("priority", "normal")]
    )
    
    let result = azimuth::FlowController::process(adaptive_controller, data_point)
    if result.action == azimuth::FlowAction::Process {
      adaptive_processed.push(data_point)
    } else {
      adaptive_dropped.push(data_point)
    }
  }
  
  // Recovery period
  for i = 600; i < 650; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = base_timestamp + i * 2000, // Lower frequency
      service_name = "adaptive-test",
      metric_name = "test.metric",
      metric_value = i.to_double(),
      attributes = [("priority", "normal")]
    )
    
    let result = azimuth::FlowController::process(adaptive_controller, data_point)
    if result.action == azimuth::FlowAction::Process {
      adaptive_processed.push(data_point)
    } else {
      adaptive_dropped.push(data_point)
    }
  }
  
  // Verify adaptive behavior
  // During low load, most data should be processed
  // During high load, some data should be dropped
  // During recovery, processing should improve
  assert_true(adaptive_processed.length() > 0)
  assert_true(adaptive_dropped.length() > 0)
  
  // Get adaptive metrics
  let adaptive_metrics = azimuth::FlowController::get_metrics(adaptive_controller)
  assert_true(adaptive_metrics.contains("peak_queue_size"))
  assert_true(adaptive_metrics.contains("average_processing_rate"))
  assert_true(adaptive_metrics.contains("backpressure_duration_ms"))
}

// Test 10: Real-time Multi-tenant Data Isolation
test "real-time multi-tenant data isolation and resource management" {
  // Create multi-tenant processor
  let tenant_processor = azimuth::MultiTenantProcessor::new()
  
  // Configure tenants with different resource limits
  let tenant_a_config = azimuth::TenantConfig::new(
    tenant_id = "tenant-a",
    max_data_rate_per_second = 100,
    max_storage_mb = 100,
    max_concurrent_queries = 10,
    priority = 1
  )
  
  let tenant_b_config = azimuth::TenantConfig::new(
    tenant_id = "tenant-b",
    max_data_rate_per_second = 200,
    max_storage_mb = 200,
    max_concurrent_queries = 20,
    priority = 2
  )
  
  let tenant_c_config = azimuth::TenantConfig::new(
    tenant_id = "tenant-c",
    max_data_rate_per_second = 50,
    max_storage_mb = 50,
    max_concurrent_queries = 5,
    priority = 0 // Lowest priority
  )
  
  azimuth::MultiTenantProcessor::add_tenant(tenant_processor, tenant_a_config)
  azimuth::MultiTenantProcessor::add_tenant(tenant_processor, tenant_b_config)
  azimuth::MultiTenantProcessor::add_tenant(tenant_processor, tenant_c_config)
  
  // Simulate multi-tenant data streams
  let base_timestamp = 1640995200000
  let tenant_results = {
    "tenant-a": [],
    "tenant-b": [],
    "tenant-c": []
  }
  
  let rate_limit_violations = []
  
  // Generate data for all tenants
  for i = 0; i < 500; i = i + 1 {
    let timestamp = base_timestamp + i * 100 // 100ms intervals
    
    // Distribute data across tenants
    let tenants = ["tenant-a", "tenant-b", "tenant-c"]
    let tenant_id = tenants[i % tenants.length()]
    
    let data_point = azimuth::TelemetryData::new(
      timestamp = timestamp,
      service_name = tenant_id + "-service",
      metric_name = "tenant.metric",
      metric_value = i.to_double(),
      attributes = [
        ("tenant.id", tenant_id),
        ("data.source", "multi-tenant-test")
      ]
    )
    
    let result = azimuth::MultiTenantProcessor::process(tenant_processor, data_point)
    
    if result.processed {
      tenant_results[tenant_id].push(data_point)
    }
    
    if result.rate_limited {
      rate_limit_violations.push((tenant_id, timestamp))
    }
  }
  
  // Verify tenant isolation
  // Each tenant should only see their own data
  for (tenant_id, data) in tenant_results {
    for data_point in data {
      assert_eq(data_point.attributes.find(|(k, _)| k == "tenant.id").unwrap().1, tenant_id)
    }
  }
  
  // Verify rate limiting
  // Tenant C has lowest rate limit (50/sec), should have more violations
  let tenant_a_violations = rate_limit_violations.filter(|(t, _)| t == "tenant-a").length()
  let tenant_b_violations = rate_limit_violations.filter(|(t, _)| t == "tenant-b").length()
  let tenant_c_violations = rate_limit_violations.filter(|(t, _)| t == "tenant-c").length()
  
  assert_true(tenant_c_violations >= tenant_a_violations)
  assert_true(tenant_c_violations >= tenant_b_violations)
  
  // Test tenant resource quotas
  let resource_usage = azimuth::MultiTenantProcessor::get_resource_usage(tenant_processor)
  
  assert_true(resource_usage.contains("tenant-a"))
  assert_true(resource_usage.contains("tenant-b"))
  assert_true(resource_usage.contains("tenant-c"))
  
  for (tenant_id, usage) in resource_usage {
    let tenant_config = match tenant_id {
      "tenant-a" => tenant_a_config,
      "tenant-b" => tenant_b_config,
      "tenant-c" => tenant_c_config,
      _ => { continue }
    }
    
    // Storage usage should not exceed limits
    assert_true(usage.storage_mb <= tenant_config.max_storage_mb)
    
    // Data rate should be close to but not exceed limits
    assert_true(usage.data_rate_per_second <= tenant_config.max_data_rate_per_second * 1.1) // 10% tolerance
  }
  
  // Test tenant priority handling during resource contention
  azimuth::MultiTenantProcessor::simulate_resource_pressure(tenant_processor, 0.8) // 80% resource usage
  
  let priority_results = []
  
  // Generate high-load data during resource pressure
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_timestamp + i * 10 // Very high frequency
    
    for tenant_id in ["tenant-a", "tenant-b", "tenant-c"] {
      let data_point = azimuth::TelemetryData::new(
        timestamp = timestamp,
        service_name = tenant_id + "-service",
        metric_name = "priority.test",
        metric_value = i.to_double(),
        attributes = [("tenant.id", tenant_id)]
      )
      
      let result = azimuth::MultiTenantProcessor::process(tenant_processor, data_point)
      priority_results.push((tenant_id, result.processed))
    }
  }
  
  // Calculate processing rates by tenant during pressure
  let tenant_a_processed = priority_results.filter(|(t, p)| t == "tenant-a" && *p).length()
  let tenant_b_processed = priority_results.filter(|(t, p)| t == "tenant-b" && *p).length()
  let tenant_c_processed = priority_results.filter(|(t, p)| t == "tenant-c" && *p).length()
  
  // Higher priority tenants should have better processing rates
  let tenant_a_rate = tenant_a_processed.to_double() / 100.0
  let tenant_b_rate = tenant_b_processed.to_double() / 100.0
  let tenant_c_rate = tenant_c_processed.to_double() / 100.0
  
  assert_true(tenant_b_rate >= tenant_a_rate) // Priority 2 >= Priority 1
  assert_true(tenant_a_rate >= tenant_c_rate) // Priority 1 >= Priority 0
  
  // Test tenant data isolation at query time
  let query_service = azimuth::MultiTenantQueryService::new(tenant_processor)
  
  // Tenant A queries should only return Tenant A data
  let tenant_a_query = azimuth::Query::new()
    .with_tenant_id("tenant-a")
    .with_time_range(base_timestamp, base_timestamp + 50000)
    .with_metric_filter("tenant.metric")
  
  let tenant_a_results = azimuth::MultiTenantQueryService::execute(query_service, tenant_a_query)
  
  for result in tenant_a_results {
    assert_eq(result.attributes.find(|(k, _)| k == "tenant.id").unwrap().1, "tenant-a")
  }
  
  // Cross-tenant queries should be rejected
  let cross_tenant_query = azimuth::Query::new()
    .with_time_range(base_timestamp, base_timestamp + 50000)
    .with_metric_filter("tenant.metric")
    // No tenant_id specified - should be rejected
  
  let cross_tenant_results = azimuth::MultiTenantQueryService::execute(query_service, cross_tenant_query)
  assert_eq(cross_tenant_results.length(), 0)
  
  let query_error = azimuth::MultiTenantQueryService::get_last_error(query_service)
  assert_true(query_error.contains("tenant_id_required"))
}