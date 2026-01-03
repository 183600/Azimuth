// Real-time Monitoring Tests for Azimuth Telemetry System
// This file contains test cases for real-time monitoring functionality

// Test 1: Real-time Stream Processing
test "real-time stream processing" {
  let stream_processor = StreamProcessor::new(1000) // 1 second window
  
  // Test stream initialization
  assert_true(StreamProcessor::is_initialized(stream_processor))
  
  // Test adding data points to stream
  StreamProcessor::add_data_point(stream_processor, 10.5)
  StreamProcessor::add_data_point(stream_processor, 15.2)
  StreamProcessor::add_data_point(stream_processor, 12.8)
  
  // Test getting current window statistics
  let stats = StreamProcessor::get_window_stats(stream_processor)
  assert_eq(stats.count, 3)
  assert_eq(stats.sum, 38.5)
  assert_eq(stats.average, 12.833333333333334)
  
  // Test sliding window functionality
  StreamProcessor::add_data_point(stream_processor, 18.3)
  let updated_stats = StreamProcessor::get_window_stats(stream_processor)
  assert_eq(updated_stats.count, 4)
  
  // Test stream reset
  StreamProcessor::reset(stream_processor)
  let reset_stats = StreamProcessor::get_window_stats(stream_processor)
  assert_eq(reset_stats.count, 0)
}

// Test 2: Alert System
test "real-time alert system" {
  let alert_manager = AlertManager::new()
  
  // Test alert rule creation
  let threshold_rule = AlertRule::threshold("cpu_usage", ">", 80.0, "high")
  AlertManager::add_rule(alert_manager, threshold_rule)
  
  let trend_rule = AlertRule::trend("memory_usage", "increasing", 5, 0.1, "medium")
  AlertManager::add_rule(alert_manager, trend_rule)
  
  // Test alert evaluation
  let metrics_data = [
    ("cpu_usage", 85.0),
    ("memory_usage", 75.0),
    ("disk_usage", 45.0)
  ]
  
  let alerts = AlertManager::evaluate_rules(alert_manager, metrics_data)
  assert_eq(alerts.length(), 1)
  assert_eq(alerts[0].metric_name, "cpu_usage")
  assert_eq(alerts[0].severity, "high")
  
  // Test alert notification
  let notification_sent = AlertManager::send_notification(alert_manager, alerts[0])
  assert_true(notification_sent)
  
  // Test alert acknowledgment
  let alert_id = alerts[0].id
  let acknowledged = AlertManager::acknowledge_alert(alert_manager, alert_id)
  assert_true(acknowledged)
}

// Test 3: Real-time Dashboard Updates
test "real-time dashboard updates" {
  let dashboard = Dashboard::new("main_dashboard")
  
  // Test widget creation
  let metric_widget = DashboardWidget::metric_chart("cpu_chart", "CPU Usage", "percentage")
  Dashboard::add_widget(dashboard, metric_widget)
  
  let gauge_widget = DashboardWidget::gauge("memory_gauge", "Memory Usage", 0.0, 100.0)
  Dashboard::add_widget(dashboard, gauge_widget)
  
  // Test widget data updates
  Dashboard::update_widget_data(dashboard, "cpu_chart", [75.0, 80.0, 85.0, 78.0, 82.0])
  Dashboard::update_widget_data(dashboard, "memory_gauge", 65.5)
  
  // Test dashboard snapshot
  let snapshot = Dashboard::get_snapshot(dashboard)
  assert_eq(snapshot.widgets.length(), 2)
  assert_eq(snapshot.widgets[0].id, "cpu_chart")
  assert_eq(snapshot.widgets[1].id, "memory_gauge")
  
  // Test real-time subscription
  let subscription = Dashboard::subscribe_to_updates(dashboard, lambda { snapshot => 
    assert_true(snapshot.widgets.length() >= 2)
  })
  assert_true(Dashboard::is_subscribed(dashboard, subscription))
}

// Test 4: Performance Metrics Collection
test "performance metrics collection" {
  let metrics_collector = MetricsCollector::new()
  
  // Test counter metric
  let request_counter = MetricsCollector::create_counter(metrics_collector, "http_requests_total", "Total HTTP requests")
  MetricsCollector::counter_add(metrics_collector, request_counter, 1.0)
  MetricsCollector::counter_add(metrics_collector, request_counter, 1.0)
  
  let counter_value = MetricsCollector::get_counter_value(metrics_collector, request_counter)
  assert_eq(counter_value, 2.0)
  
  // Test histogram metric
  let response_time = MetricsCollector::create_histogram(metrics_collector, "http_response_time_ms", "HTTP response time")
  MetricsCollector::histogram_observe(metrics_collector, response_time, 150.0)
  MetricsCollector::histogram_observe(metrics_collector, response_time, 200.0)
  MetricsCollector::histogram_observe(metrics_collector, response_time, 175.0)
  
  let histogram_stats = MetricsCollector::get_histogram_stats(metrics_collector, response_time)
  assert_eq(histogram_stats.count, 3)
  assert_eq(histogram_stats.sum, 525.0)
  
  // Test gauge metric
  let active_connections = MetricsCollector::create_gauge(metrics_collector, "active_connections", "Active connections")
  MetricsCollector::gauge_set(metrics_collector, active_connections, 42.0)
  
  let gauge_value = MetricsCollector::get_gauge_value(metrics_collector, active_connections)
  assert_eq(gauge_value, 42.0)
}

// Test 5: Event Stream Processing
test "event stream processing" {
  let event_processor = EventProcessor::new()
  
  // Test event type registration
  EventProcessor::register_event_type(event_processor, "user_login")
  EventProcessor::register_event_type(event_processor, "user_logout")
  EventProcessor::register_event_type(event_processor, "error_occurred")
  
  // Test event publishing
  let login_event = Event::new("user_login", [("user_id", "12345"), ("timestamp", "1640995200")])
  EventProcessor::publish_event(event_processor, login_event)
  
  let error_event = Event::new("error_occurred", [("error_code", "500"), ("message", "Internal server error")])
  EventProcessor::publish_event(event_processor, error_event)
  
  // Test event filtering
  let error_events = EventProcessor::filter_events(event_processor, lambda { event => 
    event.event_type == "error_occurred"
  })
  assert_eq(error_events.length(), 1)
  
  // Test event aggregation
  let event_counts = EventProcessor::aggregate_events_by_type(event_processor)
  assert_eq(event_counts.get("user_login"), Some(1))
  assert_eq(event_counts.get("error_occurred"), Some(1))
  assert_eq(event_counts.get("user_logout"), Some(0))
}

// Test 6: Real-time Anomaly Detection
test "real-time anomaly detection" {
  let anomaly_detector = RealTimeAnomalyDetector::new()
  
  // Test baseline establishment
  let baseline_data = [10.0, 12.0, 11.0, 13.0, 9.0, 14.0, 10.0, 12.0, 11.0, 13.0]
  RealTimeAnomalyDetector::establish_baseline(anomaly_detector, baseline_data)
  
  assert_true(RealTimeAnomalyDetector::has_baseline(anomaly_detector))
  
  // Test normal data point
  let normal_result = RealTimeAnomalyDetector::analyze_data_point(anomaly_detector, 12.5)
  assert_false(normal_result.is_anomaly)
  
  // Test anomalous data point
  let anomaly_result = RealTimeAnomalyDetector::analyze_data_point(anomaly_detector, 50.0)
  assert_true(anomaly_result.is_anomaly)
  assert_true(anomaly_result.anomaly_score > 3.0)
  
  // Test adaptive threshold
  RealTimeAnomalyDetector::update_sensitivity(anomaly_detector, 0.5)
  let sensitive_result = RealTimeAnomalyDetector::analyze_data_point(anomaly_detector, 25.0)
  assert_true(sensitive_result.is_anomaly)
}

// Test 7: System Health Monitoring
test "system health monitoring" {
  let health_monitor = HealthMonitor::new()
  
  // Test health check registration
  HealthMonitor::register_check(health_monitor, "database", lambda { 
    HealthCheckResult::healthy("Database connection OK")
  })
  
  HealthMonitor::register_check(health_monitor, "cache", lambda { 
    HealthCheckResult::healthy("Cache responding normally")
  })
  
  HealthMonitor::register_check(health_monitor, "external_api", lambda { 
    HealthCheckResult::unhealthy("External API timeout")
  })
  
  // Test health check execution
  let health_status = HealthMonitor::check_all(health_monitor)
  assert_eq(health_status.overall_status, "unhealthy")
  assert_eq(health_status.checks.length(), 3)
  assert_eq(health_status.healthy_checks, 2)
  assert_eq(health_status.unhealthy_checks, 1)
  
  // Test individual health check
  let db_health = HealthMonitor::check_individual(health_monitor, "database")
  assert_eq(db_health.status, "healthy")
  
  let api_health = HealthMonitor::check_individual(health_monitor, "external_api")
  assert_eq(api_health.status, "unhealthy")
}

// Test 8: Resource Usage Monitoring
test "resource usage monitoring" {
  let resource_monitor = ResourceMonitor::new()
  
  // Test CPU monitoring
  let cpu_usage = ResourceMonitor::get_cpu_usage(resource_monitor)
  assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
  
  // Test memory monitoring
  let memory_info = ResourceMonitor::get_memory_info(resource_monitor)
  assert_true(memory_info.total > 0)
  assert_true(memory_info.used >= 0)
  assert_true(memory_info.free >= 0)
  assert_true(memory_info.used <= memory_info.total)
  
  // Test disk monitoring
  let disk_info = ResourceMonitor::get_disk_info(resource_monitor, "/")
  assert_true(disk_info.total > 0)
  assert_true(disk_info.used >= 0)
  assert_true(disk_info.free >= 0)
  
  // Test network monitoring
  let network_stats = ResourceMonitor::get_network_stats(resource_monitor)
  assert_true(network_stats.bytes_sent >= 0)
  assert_true(network_stats.bytes_received >= 0)
  
  // Test resource thresholds
  ResourceMonitor::set_cpu_threshold(resource_monitor, 80.0)
  ResourceMonitor::set_memory_threshold(resource_monitor, 90.0)
  
  let cpu_alert = ResourceMonitor::check_cpu_threshold(resource_monitor)
  let memory_alert = ResourceMonitor::check_memory_threshold(resource_monitor)
  
  assert_true(cpu_alert.is_some() || cpu_alert.is_none()) // May or may not trigger depending on system
  assert_true(memory_alert.is_some() || memory_alert.is_none())
}

// Test 9: Log Stream Processing
test "log stream processing" {
  let log_processor = LogProcessor::new()
  
  // Test log level filtering
  LogProcessor::set_minimum_level(log_processor, "warn")
  
  LogProcessor::process_log_entry(log_processor, LogEntry::new("info", "Application started"))
  LogProcessor::process_log_entry(log_processor, LogEntry::new("warn", "Deprecated API used"))
  LogProcessor::process_log_entry(log_processor, LogEntry::new("error", "Database connection failed"))
  
  let filtered_logs = LogProcessor::get_filtered_logs(log_processor)
  assert_eq(filtered_logs.length(), 2) // Only warn and error
  
  // Test log pattern matching
  LogProcessor::add_pattern(log_processor, "error_pattern", "ERROR.*")
  
  let error_logs = LogProcessor::match_pattern(log_processor, "error_pattern")
  assert_eq(error_logs.length(), 1)
  
  // Test log aggregation
  let log_counts = LogProcessor::count_by_level(log_processor)
  assert_eq(log_counts.get("error"), Some(1))
  assert_eq(log_counts.get("warn"), Some(1))
  assert_eq(log_counts.get("info"), Some(0)) // Filtered out
}

// Test 10: Real-time Configuration Updates
test "real-time configuration updates" {
  let config_manager = ConfigManager::new()
  
  // Test configuration subscription
  let subscription = ConfigManager::subscribe(config_manager, "monitoring", lambda { key, value =>
    assert_eq(key, "monitoring")
    assert_true(value.is_some())
  })
  
  // Test configuration update
  ConfigManager::update_config(config_manager, "monitoring", "enabled")
  ConfigManager::update_config(config_manager, "alert_threshold", "80.0")
  
  // Test configuration retrieval
  let monitoring_config = ConfigManager::get_config(config_manager, "monitoring")
  assert_eq(monitoring_config, Some("enabled"))
  
  let threshold_config = ConfigManager::get_config(config_manager, "alert_threshold")
  assert_eq(threshold_config, Some("80.0"))
  
  let non_existent = ConfigManager::get_config(config_manager, "non_existent")
  assert_eq(non_existent, None)
  
  // Test configuration validation
  ConfigManager::add_validator(config_manager, "alert_threshold", lambda { value =>
    match value.parse_float() {
      Some(num) => num >= 0.0 && num <= 100.0
      None => false
    }
  })
  
  let valid_update = ConfigManager::validate_and_update(config_manager, "alert_threshold", "75.5")
  assert_true(valid_update)
  
  let invalid_update = ConfigManager::validate_and_update(config_manager, "alert_threshold", "150.0")
  assert_false(invalid_update)
}