// Azimuth Telemetry System - Anomaly Detection and Alerting Tests
// This file contains test cases for anomaly detection and alerting functionality

// Test 1: Statistical Anomaly Detection
test "statistical anomaly detection" {
  // Create time series with normal data and some anomalies
  let series = TimeSeries::new("cpu.usage", "percentage", "CPU usage percentage")
  
  // Add normal data points (values around 50 with small variations)
  let mut series_with_data = series
  for i in 0..100 {
    let timestamp = 1640995200000L + (i * 60000L) // 1 minute intervals
    let value = 50.0 + (Math::random() * 10.0 - 5.0) // Values between 45 and 55
    let point = TimeSeriesPoint::new(timestamp, value, None)
    series_with_data = TimeSeries::add_point(series_with_data, point)
  }
  
  // Add some anomalies
  let anomaly_points = [
    TimeSeriesPoint::new(1640995200000L + (25 * 60000L), 85.0, None), // High anomaly
    TimeSeriesPoint::new(1640995200000L + (50 * 60000L), 15.0, None), // Low anomaly
    TimeSeriesPoint::new(1640995200000L + (75 * 60000L), 95.0, None)  // Extreme high anomaly
  ]
  
  for anomaly in anomaly_points {
    series_with_data = TimeSeries::add_point(series_with_data, anomaly)
  }
  
  // Create statistical anomaly detector
  let detector = StatisticalAnomalyDetector::new(2.0) // 2 standard deviations threshold
  
  // Detect anomalies
  let anomalies = AnomalyDetector::detect(detector, series_with_data)
  
  // Should detect at least the 3 anomalies we added
  assert_true(anomalies.length() >= 3)
  
  // Verify anomaly details
  for anomaly in anomalies {
    let point = Anomaly::point(anomaly)
    let score = Anomaly::score(anomaly)
    let threshold = Anomaly::threshold(anomaly)
    
    // Verify that anomaly score exceeds threshold
    assert_true(score > threshold)
    
    // Verify that the point is indeed one of our anomalies or a statistical one
    let value = TimeSeriesPoint::value(point)
    assert_true(value > 70.0 || value < 30.0) // Outside normal range
  }
}

// Test 2: Threshold-based Anomaly Detection
test "threshold-based anomaly detection" {
  // Create time series with varying values
  let series = TimeSeries::new("memory.usage", "percentage", "Memory usage percentage")
  
  let mut series_with_data = series
  for i in 0..50 {
    let timestamp = 1640995200000L + (i * 60000L)
    let value = 40.0 + (i % 20).to_float() * 2.0 // Values between 40 and 78
    let point = TimeSeriesPoint::new(timestamp, value, None)
    series_with_data = TimeSeries::add_point(series_with_data, point)
  }
  
  // Add values outside normal thresholds
  let threshold_violations = [
    TimeSeriesPoint::new(1640995200000L + (10 * 60000L), 95.0, None), // Above upper threshold
    TimeSeriesPoint::new(1640995200000L + (30 * 60000L), 15.0, None)  // Below lower threshold
  ]
  
  for violation in threshold_violations {
    series_with_data = TimeSeries::add_point(series_with_data, violation)
  }
  
  // Create threshold-based anomaly detector
  let detector = ThresholdAnomalyDetector::new(80.0, 25.0) // Upper: 80%, Lower: 25%
  
  // Detect anomalies
  let anomalies = AnomalyDetector::detect(detector, series_with_data)
  
  // Should detect exactly 2 threshold violations
  assert_eq(anomalies.length(), 2)
  
  // Verify anomaly details
  for anomaly in anomalies {
    let point = Anomaly::point(anomaly)
    let value = TimeSeriesPoint::value(point)
    
    // Verify that the value is indeed outside thresholds
    assert_true(value > 80.0 || value < 25.0)
  }
}

// Test 3: Pattern-based Anomaly Detection
test "pattern-based anomaly detection" {
  // Create time series with daily pattern
  let series = TimeSeries::new("web.traffic", "requests", "Web traffic requests")
  
  let mut series_with_data = series
  // Add 7 days of data with daily pattern (higher during day, lower at night)
  for day in 0..7 {
    for hour in 0..24 {
      let timestamp = 1640995200000L + (day * 24 * 3600000L) + (hour * 3600000L)
      
      // Normal pattern: high traffic during business hours (9-17), low at night
      let base_value = if hour >= 9 && hour <= 17 {
        1000.0 + (Math::random() * 200.0 - 100.0) // 900-1100 during business hours
      } else {
        200.0 + (Math::random() * 100.0 - 50.0)   // 150-250 during non-business hours
      }
      
      let point = TimeSeriesPoint::new(timestamp, base_value, None)
      series_with_data = TimeSeries::add_point(series_with_data, point)
    }
  }
  
  // Add pattern anomalies (unusual traffic at 3 AM)
  let pattern_anomalies = [
    TimeSeriesPoint::new(1640995200000L + (2 * 24 * 3600000L) + (3 * 3600000L), 1200.0, None), // Day 2, 3 AM
    TimeSeriesPoint::new(1640995200000L + (5 * 24 * 3600000L) + (2 * 3600000L), 1300.0, None)  // Day 5, 2 AM
  ]
  
  for anomaly in pattern_anomalies {
    series_with_data = TimeSeries::add_point(series_with_data, anomaly)
  }
  
  // Create pattern-based anomaly detector
  let detector = PatternAnomalyDetector::new(24 * 3600000L) // 24-hour pattern
  let anomalies = AnomalyDetector::detect(detector, series_with_data)
  
  // Should detect at least the 2 pattern anomalies
  assert_true(anomalies.length() >= 2)
  
  // Verify anomaly details
  for anomaly in anomalies {
    let point = Anomaly::point(anomaly)
    let timestamp = TimeSeriesPoint::timestamp(point)
    let hour_of_day = (timestamp / 3600000L) % 24
    
    // Pattern anomalies should occur during unusual hours
    let value = TimeSeriesPoint::value(point)
    if hour_of_day < 6 || hour_of_day > 20 {
      assert_true(value > 800.0) // High traffic during unusual hours
    }
  }
}

// Test 4: Alert Rule Creation and Evaluation
test "alert rule creation and evaluation" {
  // Create alert rule
  let conditions = [
    AlertCondition::new("cpu.usage", GreaterThan, 80.0, "CPU usage is too high"),
    AlertCondition::new("memory.usage", GreaterThan, 90.0, "Memory usage is critical")
  ]
  
  let rule = AlertRule::new("system.resource.alert", "System resource alert", Warning, conditions)
  
  assert_eq(AlertRule::name(rule), "system.resource.alert")
  match AlertRule::description(rule) {
    Some(desc) => assert_eq(desc, "System resource alert")
    None => assert_true(false)
  }
  assert_eq(AlertRule::severity(rule), Warning)
  assert_eq(AlertRule::condition_count(rule), 2)
  
  // Create evaluation context with metrics
  let metrics = [
    ("cpu.usage", 85.0),
    ("memory.usage", 75.0),
    ("disk.usage", 60.0)
  ]
  
  // Evaluate alert rule
  let evaluation = AlertRule::evaluate(rule, metrics)
  
  // Should trigger alert because CPU usage > 80%
  assert_true(AlertEvaluation::is_triggered(evaluation))
  
  // Check triggered conditions
  let triggered_conditions = AlertEvaluation::triggered_conditions(evaluation)
  assert_eq(triggered_conditions.length(), 1)
  
  let triggered_condition = triggered_conditions[0]
  assert_eq(AlertCondition::metric_name(triggered_condition), "cpu.usage")
  assert_eq(AlertCondition::threshold(triggered_condition), 80.0)
  assert_eq(AlertCondition::actual_value(triggered_condition), 85.0)
  
  // Test with all conditions triggered
  let critical_metrics = [
    ("cpu.usage", 85.0),
    ("memory.usage", 95.0),
    ("disk.usage", 60.0)
  ]
  
  let critical_evaluation = AlertRule::evaluate(rule, critical_metrics)
  assert_true(AlertEvaluation::is_triggered(critical_evaluation))
  
  let critical_triggered = AlertEvaluation::triggered_conditions(critical_evaluation)
  assert_eq(critical_triggered.length(), 2)
}

// Test 5: Alert Notification System
test "alert notification system" {
  // Create notification channels
  let email_channel = EmailNotificationChannel::new("admin@example.com")
  let webhook_channel = WebhookNotificationChannel::new("https://example.com/webhook")
  let slack_channel = SlackNotificationChannel::new("#alerts")
  
  // Create notification manager
  let manager = NotificationManager::new()
  let manager_with_channels = NotificationManager::add_channel(
    NotificationManager::add_channel(
      NotificationManager::add_channel(manager, email_channel),
      webhook_channel),
    slack_channel)
  
  // Create alert
  let alert = Alert::new(
    "system.cpu.high",
    "CPU usage is above threshold",
    Critical,
    1640995200000L,
    Some([("cpu.usage", "85.5"), ("threshold", "80.0")])
  )
  
  assert_eq(Alert::id(alert), "system.cpu.high")
  match Alert::message(alert) {
    Some(msg) => assert_eq(msg, "CPU usage is above threshold")
    None => assert_true(false)
  }
  assert_eq(Alert::severity(alert), Critical)
  assert_eq(Alert::timestamp(alert), 1640995200000L)
  
  // Send notifications
  let notification_results = NotificationManager::send_alert(manager_with_channels, alert)
  
  // Verify all channels were notified
  assert_eq(notification_results.length(), 3)
  
  // Verify notification results
  for result in notification_results {
    match result {
      NotificationSuccess(channel_id) => assert_true(true)
      NotificationFailure(channel_id, error) => assert_true(false)
    }
  }
  
  // Test notification filtering by severity
  let filtered_manager = NotificationManager::set_severity_filter(manager_with_channels, Warning)
  let info_alert = Alert::new("system.info", "Informational message", Info, 1640995200000L, None)
  let info_results = NotificationManager::send_alert(filtered_manager, info_alert)
  
  // Info alert should not be sent due to severity filter
  assert_eq(info_results.length(), 0)
}

// Test 6: Alert Escalation Policy
test "alert escalation policy" {
  // Create escalation policy
  let level1 = EscalationLevel::new(1, 300000L, [EmailNotificationChannel::new("team@example.com")]) // 5 minutes
  let level2 = EscalationLevel::new(2, 600000L, [EmailNotificationChannel::new("manager@example.com")]) // 10 minutes
  let level3 = EscalationLevel::new(3, 900000L, [SmsNotificationChannel::new("+1234567890")]) // 15 minutes
  
  let policy = EscalationPolicy::new("critical.system.escalation", [level1, level2, level3])
  
  // Create alert manager with escalation policy
  let manager = AlertManager::new()
  let manager_with_policy = AlertManager::add_escalation_policy(manager, policy)
  
  // Create critical alert
  let alert = Alert::new(
    "system.critical.failure",
    "Critical system failure detected",
    Critical,
    1640995200000L,
    Some([("component", "database", "error", "connection_timeout")])
  )
  
  // Process alert (should trigger level 1)
  let initial_result = AlertManager::process_alert(manager_with_policy, alert)
  assert_true(AlertProcessingResult::is_success(initial_result))
  
  // Simulate time passing (6 minutes) and check escalation
  let escalated_result = AlertManager::check_escalations(manager_with_policy, 1640995500000L) // 5 minutes later
  assert_true(AlertProcessingResult::is_success(escalated_result))
  
  // Simulate more time passing (11 minutes total) and check further escalation
  let further_escalated_result = AlertManager::check_escalations(manager_with_policy, 1640995800000L) // 10 minutes later
  assert_true(AlertProcessingResult::is_success(further_escalated_result))
  
  // Simulate final escalation (16 minutes total)
  let final_escalated_result = AlertManager::check_escalations(manager_with_policy, 1640996100000L) // 15 minutes later
  assert_true(AlertProcessingResult::is_success(final_escalated_result))
}

// Test 7: Alert Suppression and Deduplication
test "alert suppression and deduplication" {
  // Create alert manager
  let manager = AlertManager::new()
  
  // Create suppression rule
  let suppression_rule = SuppressionRule::new(
    "maintenance.window",
    ["system.maintenance"],
    3600000L // 1 hour
  )
  
  let manager_with_suppression = AlertManager::add_suppression_rule(manager, suppression_rule)
  
  // Create maintenance alert (should trigger suppression)
  let maintenance_alert = Alert::new(
    "system.maintenance",
    "System maintenance in progress",
    Info,
    1640995200000L,
    None
  )
  
  // Process maintenance alert
  let maintenance_result = AlertManager::process_alert(manager_with_suppression, maintenance_alert)
  assert_true(AlertProcessingResult::is_suppressed(maintenance_result))
  
  // Create duplicate alerts
  let duplicate_alert1 = Alert::new(
    "high.cpu.usage",
    "CPU usage is high",
    Warning,
    1640995200000L,
    Some([("cpu.usage", "85.0")])
  )
  
  let duplicate_alert2 = Alert::new(
    "high.cpu.usage",
    "CPU usage is high",
    Warning,
    1640995250000L, // 5 minutes later
    Some([("cpu.usage", "87.0")])
  )
  
  // Process first duplicate alert
  let first_result = AlertManager::process_alert(manager_with_suppression, duplicate_alert1)
  assert_true(AlertProcessingResult::is_success(first_result))
  
  // Process second duplicate alert (should be deduplicated)
  let second_result = AlertManager::process_alert(manager_with_suppression, duplicate_alert2)
  assert_true(AlertProcessingResult::is_deduplicated(second_result))
  
  // Check alert deduplication window
  let dedup_window = AlertManager::get_deduplication_window(manager_with_suppression)
  assert_eq(dedup_window, 1800000L) // Default 30 minutes
}

// Test 8: Alert Aggregation and Grouping
test "alert aggregation and grouping" {
  // Create alert aggregation rule
  let aggregation_rule = AlertAggregationRule::new(
    "host.grouping",
    ["host.name"], // Group by host name
    300000L,      // 5 minute window
    5             // Maximum 5 alerts per group
  )
  
  // Create alert manager with aggregation
  let manager = AlertManager::new()
  let manager_with_aggregation = AlertManager::add_aggregation_rule(manager, aggregation_rule)
  
  // Create multiple alerts for the same host
  let host_alerts = [
    Alert::new("high.cpu", "High CPU usage", Warning, 1640995200000L, Some([("host.name", "server-01")])),
    Alert::new("high.memory", "High memory usage", Warning, 1640995230000L, Some([("host.name", "server-01")])),
    Alert::new("disk.space.low", "Low disk space", Critical, 1640995260000L, Some([("host.name", "server-01")])),
    Alert::new("network.error", "Network error", Error, 1640995290000L, Some([("host.name", "server-01")])),
    Alert::new("service.down", "Service down", Critical, 1640995320000L, Some([("host.name", "server-01")]))
  ]
  
  // Process alerts
  let mut results = []
  for alert in host_alerts {
    let result = AlertManager::process_alert(manager_with_aggregation, alert)
    results = Array::push(results, result)
  }
  
  // Check that alerts were processed
  assert_eq(results.length(), 5)
  
  // Check aggregated alerts
  let aggregated_alerts = AlertManager::get_aggregated_alerts(manager_with_aggregation)
  assert_eq(aggregated_alerts.length(), 1) // All alerts should be in one group
  
  // Verify aggregated alert content
  let aggregated_alert = aggregated_alerts[0]
  assert_eq(Alert::id(aggregated_alert), "aggregated.host.grouping.server-01")
  match Alert::message(aggregated_alert) {
    Some(msg) => assert_true(String::contains(msg, "5 alerts"))
    None => assert_true(false)
  }
  
  // Verify that the highest severity is used (Critical)
  assert_eq(Alert::severity(aggregated_alert), Critical)
  
  // Test with alerts from different hosts
  let different_host_alert = Alert::new(
    "high.cpu",
    "High CPU usage",
    Warning,
    1640995200000L,
    Some([("host.name", "server-02")])
  )
  
  let different_host_result = AlertManager::process_alert(manager_with_aggregation, different_host_alert)
  assert_true(AlertProcessingResult::is_success(different_host_result))
  
  // Should now have 2 aggregated groups
  let updated_aggregated_alerts = AlertManager::get_aggregated_alerts(manager_with_aggregation)
  assert_eq(updated_aggregated_alerts.length(), 2)
}