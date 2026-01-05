// Azimuth Real-time Monitoring Test Suite
// This file contains test cases for real-time monitoring and alerting

// Test 1: Real-time Metrics Collection and Processing
test "real-time metrics collection and processing pipeline" {
  // Define metric type
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // Define time series point
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    labels: Array<(String, String)>
  }
  
  // Define time series
  type TimeSeries = {
    name: String,
    metric_type: MetricType,
    points: Array[TimeSeriesPoint>
  }
  
  // Define real-time processor
  type RealTimeProcessor = {
    window_size_ms: Int,
    slide_interval_ms: Int,
    aggregation_functions: Array<(String) -> Float>,
    alert_thresholds: Array<(String, Float)>
  }
  
  // Define processing window
  type ProcessingWindow = {
    start_time: Int,
    end_time: Int,
    time_series: Array<TimeSeries>
  }
  
  // Create time series
  let create_time_series = fn(name: String, metric_type: MetricType) {
    {
      name,
      metric_type,
      points: []
    }
  }
  
  // Add point to time series
  let add_point = fn(series: TimeSeries, timestamp: Int, value: Float, 
                     labels: Array<(String, String)>) {
    { series | points: series.points.push({
      timestamp,
      value,
      labels
    })}
  }
  
  // Create real-time processor
  let create_processor = fn(window_size_ms: Int, slide_interval_ms: Int) {
    {
      window_size_ms,
      slide_interval_ms,
      aggregation_functions: [
        fn(values: Array<Float>) { values.reduce(fn(sum, v) { sum + v }, 0.0) },  // sum
        fn(values: Array<Float>) { 
          if values.length() > 0 { values.reduce(fn(sum, v) { sum + v }, 0.0) / (values.length() as Float) } else { 0.0 }
        },  // avg
        fn(values: Array<Float>) { 
          if values.length() > 0 { 
            values.reduce(fn(max, v) { if v > max { v } else { max }, values[0]) 
          } else { 0.0 }
        }  // max
      ],
      alert_thresholds: [
        ("cpu_usage", 80.0),
        ("memory_usage", 85.0),
        ("error_rate", 5.0)
      ]
    }
  }
  
  // Create processing windows
  let create_windows = fn(series: Array<TimeSeries>, processor: RealTimeProcessor, 
                         current_time: Int) {
    if series.length() == 0 {
      []
    } else {
      let mut windows = []
      
      // Get earliest timestamp
      let earliest_time = series.reduce(fn(min, s) {
        if s.points.length() > 0 && s.points[0].timestamp < min {
          s.points[0].timestamp
        } else {
          min
        }
      }, 2147483647)
      
      let mut window_start = earliest_time
      while window_start <= current_time {
        let window_end = window_start + processor.window_size_ms
        
        // Find points within window
        let window_series = series.map(fn(s) {
          let window_points = s.points.filter(fn(p) {
            p.timestamp >= window_start && p.timestamp < window_end
          })
          
          { s | points: window_points }
        })
        
        windows = windows.push({
          start_time: window_start,
          end_time: window_end,
          time_series: window_series
        })
        
        window_start = window_start + processor.slide_interval_ms
      }
      
      windows
    }
  }
  
  // Process window
  let process_window = fn(window: ProcessingWindow, processor: RealTimeProcessor) {
    let mut results = []
    
    for series in window.time_series {
      if series.points.length() > 0 {
        let values = series.points.map(fn(p) { p.value })
        
        let mut aggregations = []
        for i in 0..processor.aggregation_functions.length() {
          let agg_value = processor.aggregation_functions[i](values)
          let agg_name = match i {
            0 => "sum"
            1 => "avg"
            2 => "max"
            _ => "unknown"
          }
          
          aggregations = aggregations.push({
            metric_name: series.name + "_" + agg_name,
            value: agg_value,
            timestamp: window.end_time,
            labels: series.points[0].labels  // Use labels from first point
          })
        }
        
        results = results + aggregations
      }
    }
    
    results
  }
  
  // Check for alerts
  let check_alerts = fn(results: Array<{metric_name: String, value: Float, timestamp: Int, labels: Array<(String, String)}>, 
                        processor: RealTimeProcessor) {
    let mut alerts = []
    
    for result in results {
      for (metric_name, threshold) in processor.alert_thresholds {
        if result.metric_name.contains(metric_name) && result.value > threshold {
          alerts = alerts.push({
            metric_name: result.metric_name,
            current_value: result.value,
            threshold,
            timestamp: result.timestamp,
            labels: result.labels,
            severity: if result.value > threshold * 1.5 { "CRITICAL" } else { "WARNING" }
          })
        }
      }
    }
    
    alerts
  }
  
  // Test time series creation
  let cpu_series = create_time_series("cpu_usage", MetricType::Gauge)
  let memory_series = create_time_series("memory_usage", MetricType::Gauge)
  let error_series = create_time_series("error_rate", MetricType::Gauge)
  
  // Add sample data points
  let base_time = 1640995200000  // Base timestamp in milliseconds
  
  let cpu_series1 = add_point(cpu_series, base_time, 45.2, [("instance", "server-1")])
  let cpu_series2 = add_point(cpu_series1, base_time + 5000, 52.8, [("instance", "server-1")])
  let cpu_series3 = add_point(cpu_series2, base_time + 10000, 78.5, [("instance", "server-1")])
  let cpu_series4 = add_point(cpu_series3, base_time + 15000, 85.2, [("instance", "server-1")])
  let cpu_series5 = add_point(cpu_series4, base_time + 20000, 91.7, [("instance", "server-1")])
  
  let memory_series1 = add_point(memory_series, base_time, 60.1, [("instance", "server-1")])
  let memory_series2 = add_point(memory_series1, base_time + 5000, 65.3, [("instance", "server-1")])
  let memory_series3 = add_point(memory_series2, base_time + 10000, 72.8, [("instance", "server-1")])
  let memory_series4 = add_point(memory_series3, base_time + 15000, 88.9, [("instance", "server-1")])
  let memory_series5 = add_point(memory_series4, base_time + 20000, 92.4, [("instance", "server-1")])
  
  let error_series1 = add_point(error_series, base_time, 0.5, [("instance", "server-1")])
  let error_series2 = add_point(error_series1, base_time + 5000, 1.2, [("instance", "server-1")])
  let error_series3 = add_point(error_series2, base_time + 10000, 3.8, [("instance", "server-1")])
  let error_series4 = add_point(error_series3, base_time + 15000, 6.5, [("instance", "server-1")])
  let error_series5 = add_point(error_series4, base_time + 20000, 8.9, [("instance", "server-1")])
  
  // Test processor creation
  let processor = create_processor(10000, 5000)  // 10s window, 5s slide
  assert_eq(processor.window_size_ms, 10000)
  assert_eq(processor.slide_interval_ms, 5000)
  
  // Test window creation
  let all_series = [cpu_series5, memory_series5, error_series5]
  let windows = create_windows(all_series, processor, base_time + 20000)
  assert_eq(windows.length(), 5)  // Should have 5 windows
  
  // Test first window (0-10s)
  assert_eq(windows[0].start_time, base_time)
  assert_eq(windows[0].end_time, base_time + 10000)
  assert_eq(windows[0].time_series[0].points.length(), 3)  // 3 CPU points
  
  // Test window processing
  let processed_results = process_window(windows[3], processor)  // Process 15-25s window
  assert_eq(processed_results.length(), 9)  // 3 series * 3 aggregations
  
  // Find CPU avg result
  let cpu_avg = processed_results.find(fn(r) { r.metric_name == "cpu_usage_avg" })
  match cpu_avg {
    Some(result) => {
      assert_eq(result.value.round(), 81.85)  // (78.5 + 85.2) / 2
      assert_eq(result.timestamp, base_time + 25000)
    }
    None => assert_true(false)
  }
  
  // Test alert checking
  let alerts = check_alerts(processed_results, processor)
  assert_eq(alerts.length(), 3)  // CPU, memory, and error rate alerts
  
  let cpu_alert = alerts.find(fn(a) { a.metric_name.contains("cpu_usage") })
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.current_value.round(), 91.7)
      assert_eq(alert.threshold, 80.0)
      assert_eq(alert.severity, "CRITICAL")
    }
    None => assert_true(false)
  }
  
  let memory_alert = alerts.find(fn(a) { a.metric_name.contains("memory_usage") })
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.current_value.round(), 90.65)  // (88.9 + 92.4) / 2
      assert_eq(alert.threshold, 85.0)
      assert_eq(alert.severity, "CRITICAL")
    }
    None => assert_true(false)
  }
  
  let error_alert = alerts.find(fn(a) { a.metric_name.contains("error_rate") })
  match error_alert {
    Some(alert) => {
      assert_eq(alert.current_value.round(), 7.7)  // (6.5 + 8.9) / 2
      assert_eq(alert.threshold, 5.0)
      assert_eq(alert.severity, "WARNING")
    }
    None => assert_true(false)
  }
}

// Test 2: Anomaly Detection in Real-time
test "real-time anomaly detection algorithms" {
  // Define anomaly type
  enum AnomalyType {
    Spike
    Drop
    Trend
    Outlier
    Pattern
  }
  
  // Define anomaly
  type Anomaly = {
    anomaly_type: AnomalyType,
    metric_name: String,
    timestamp: Int,
    actual_value: Float,
    expected_value: Float,
    confidence: Float,
    severity: String,
    description: String
  }
  
  // Define anomaly detector
  type AnomalyDetector = {
    algorithm: String,
    sensitivity: Float,
    window_size: Int,
    min_samples: Int
  }
  
  // Statistical anomaly detection
  let statistical_anomaly_detection = fn(values: Array<Float>, sensitivity: Float) {
    if values.length() < 3 {
      []
    } else {
      let mut anomalies = []
      
      // Calculate mean and standard deviation
      let mean = values.reduce(fn(sum, v) { sum + v }, 0.0) / (values.length() as Float)
      let variance = values.reduce(fn(sum, v) { 
        sum + (v - mean) * (v - mean) 
      }, 0.0) / (values.length() as Float)
      let std_dev = variance.sqrt()
      
      // Check for outliers (values beyond mean ± sensitivity * std_dev)
      for i in 0..values.length() {
        let value = values[i]
        let z_score = (value - mean) / std_dev
        
        if z_score.abs() > sensitivity {
          let anomaly_type = if value > mean { AnomalyType::Spike } else { AnomalyType::Drop }
          let severity = if z_score.abs() > 3.0 { "CRITICAL" } else { "WARNING" }
          
          anomalies = anomalies.push({
            anomaly_type,
            metric_name: "unknown",
            timestamp: 0,  // Would be actual timestamp
            actual_value: value,
            expected_value: mean,
            confidence: (z_score.abs() / 4.0).min(1.0),  // Normalize to 0-1
            severity,
            description: "Statistical outlier detected"
          })
        }
      }
      
      anomalies
    }
  }
  
  // Trend change detection
  let trend_change_detection = fn(values: Array<Float>, sensitivity: Float) {
    if values.length() < 5 {
      []
    } else {
      let mut anomalies = []
      
      // Calculate trend in first half vs second half
      let mid_point = values.length() / 2
      let first_half = values.slice(0, mid_point)
      let second_half = values.slice(mid_point, values.length())
      
      let first_slope = if first_half.length() > 1 {
        (first_half[first_half.length() - 1] - first_half[0]) / (first_half.length() as Float)
      } else {
        0.0
      }
      
      let second_slope = if second_half.length() > 1 {
        (second_half[second_half.length() - 1] - second_half[0]) / (second_half.length() as Float)
      } else {
        0.0
      }
      
      let slope_change = (second_slope - first_slope).abs()
      
      if slope_change > sensitivity {
        let anomaly_type = if second_slope > first_slope { 
          AnomalyType::Trend 
        } else { 
          AnomalyType::Trend 
        }
        
        anomalies = anomalies.push({
          anomaly_type,
          metric_name: "unknown",
          timestamp: 0,
          actual_value: second_slope,
          expected_value: first_slope,
          confidence: (slope_change / (sensitivity * 2.0)).min(1.0),
          severity: "WARNING",
          description: "Trend change detected"
        })
      }
      
      anomalies
    }
  }
  
  // Pattern anomaly detection
  let pattern_anomaly_detection = fn(values: Array<Float>, expected_pattern: Array<Float>, 
                                    sensitivity: Float) {
    if values.length() != expected_pattern.length() {
      []
    } else {
      let mut anomalies = []
      
      // Calculate pattern similarity (simplified correlation)
      let correlation = if values.length() > 0 {
        let mean_values = values.reduce(fn(sum, v) { sum + v }, 0.0) / (values.length() as Float)
        let mean_pattern = expected_pattern.reduce(fn(sum, v) { sum + v }, 0.0) / (expected_pattern.length() as Float)
        
        let numerator = values.reduce_with_index(fn(sum, v, i) { 
          sum + (v - mean_values) * (expected_pattern[i] - mean_pattern) 
        }, 0.0)
        
        let var_values = values.reduce(fn(sum, v) { sum + (v - mean_values) * (v - mean_values) }, 0.0)
        let var_pattern = expected_pattern.reduce(fn(sum, v) { sum + (v - mean_pattern) * (v - mean_pattern) }, 0.0)
        
        let denominator = (var_values * var_pattern).sqrt()
        
        if denominator > 0.0 {
          numerator / denominator
        } else {
          0.0
        }
      } else {
        0.0
      }
      
      if correlation < (1.0 - sensitivity) {
        anomalies = anomalies.push({
          anomaly_type: AnomalyType::Pattern,
          metric_name: "unknown",
          timestamp: 0,
          actual_value: correlation,
          expected_value: 1.0,
          confidence: 1.0 - correlation,
          severity: "WARNING",
          description: "Pattern deviation detected"
        })
      }
      
      anomalies
    }
  }
  
  // Create anomaly detector
  let create_detector = fn(algorithm: String, sensitivity: Float, window_size: Int) {
    {
      algorithm,
      sensitivity,
      window_size,
      min_samples: 3
    }
  }
  
  // Detect anomalies
  let detect_anomalies = fn(detector: AnomalyDetector, values: Array<Float>, 
                           metric_name: String, timestamp: Int, 
                           expected_pattern: Option<Array<Float>>) {
    let mut anomalies = []
    
    if values.length() >= detector.min_samples {
      let detection_results = match detector.algorithm {
        "statistical" => statistical_anomaly_detection(values, detector.sensitivity)
        "trend" => trend_change_detection(values, detector.sensitivity)
        "pattern" => {
          match expected_pattern {
            Some(pattern) => pattern_anomaly_detection(values, pattern, detector.sensitivity)
            None => []
          }
        }
        _ => []
      }
      
      // Update anomaly details
      anomalies = detection_results.map(fn(anomaly) {
        {
          anomaly_type: anomaly.anomaly_type,
          metric_name,
          timestamp,
          actual_value: anomaly.actual_value,
          expected_value: anomaly.expected_value,
          confidence: anomaly.confidence,
          severity: anomaly.severity,
          description: anomaly.description
        }
      })
    }
    
    anomalies
  }
  
  // Test detector creation
  let statistical_detector = create_detector("statistical", 2.0, 10)
  let trend_detector = create_detector("trend", 0.5, 10)
  let pattern_detector = create_detector("pattern", 0.3, 10)
  
  assert_eq(statistical_detector.algorithm, "statistical")
  assert_eq(statistical_detector.sensitivity, 2.0)
  
  // Test statistical anomaly detection
  let normal_values = [10.0, 12.0, 11.5, 10.8, 11.2, 10.9, 11.1, 10.7, 11.3, 10.6]
  let anomaly_values = [10.0, 12.0, 11.5, 10.8, 25.0, 10.9, 11.1, 10.7, 11.3, 10.6]  // Spike at index 4
  
  let normal_anomalies = detect_anomalies(statistical_detector, normal_values, 
                                         "cpu_usage", 1640995200, None)
  assert_eq(normal_anomalies.length(), 0)  // No anomalies in normal data
  
  let spike_anomalies = detect_anomalies(statistical_detector, anomaly_values, 
                                         "cpu_usage", 1640995200, None)
  assert_eq(spike_anomalies.length(), 1)  // One spike anomaly
  
  let spike_anomaly = spike_anomalies[0]
  assert_eq(spike_anomaly.anomaly_type, AnomalyType::Spike)
  assert_eq(spike_anomaly.actual_value, 25.0)
  assert_eq(spike_anomaly.severity, "CRITICAL")
  
  // Test trend change detection
  let increasing_values = [10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 22.0, 24.0, 26.0, 28.0]
  let trend_change_values = [10.0, 12.0, 14.0, 16.0, 18.0, 5.0, 2.0, 1.0, 0.5, 0.1]  // Trend change
  
  let trend_anomalies = detect_anomalies(trend_detector, trend_change_values, 
                                       "memory_usage", 1640995200, None)
  assert_eq(trend_anomalies.length(), 1)  // One trend anomaly
  
  let trend_anomaly = trend_anomalies[0]
  assert_eq(trend_anomaly.anomaly_type, AnomalyType::Trend)
  assert_eq(trend_anomaly.severity, "WARNING")
  
  // Test pattern anomaly detection
  let daily_pattern = [10.0, 15.0, 25.0, 40.0, 60.0, 80.0, 70.0, 50.0, 30.0, 20.0]
  let matching_values = [10.5, 14.8, 25.2, 39.5, 60.5, 79.8, 70.2, 50.5, 29.8, 20.2]  // Similar pattern
  let deviating_values = [10.0, 15.0, 25.0, 40.0, 60.0, 80.0, 70.0, 50.0, 30.0, 80.0]  // Last value deviates
  
  let matching_anomalies = detect_anomalies(pattern_detector, matching_values, 
                                           "response_time", 1640995200, Some(daily_pattern))
  assert_eq(matching_anomalies.length(), 0)  // No anomalies in matching pattern
  
  let deviating_anomalies = detect_anomalies(pattern_detector, deviating_values, 
                                           "response_time", 1640995200, Some(daily_pattern))
  assert_eq(deviating_anomalies.length(), 1)  // One pattern anomaly
  
  let pattern_anomaly = deviating_anomalies[0]
  assert_eq(pattern_anomaly.anomaly_type, AnomalyType::Pattern)
  assert_eq(pattern_anomaly.description, "Pattern deviation detected")
  
  // Test anomaly aggregation
  type AnomalySummary = {
    total_anomalies: Int,
    anomalies_by_type: Array[(AnomalyType, Int)>,
    anomalies_by_severity: Array[(String, Int)>,
    highest_confidence: Float,
    time_window: (Int, Int)
  }
  
  let aggregate_anomalies = fn(anomalies: Array<Anomaly>) {
    if anomalies.length() == 0 {
      {
        total_anomalies: 0,
        anomalies_by_type: [],
        anomalies_by_severity: [],
        highest_confidence: 0.0,
        time_window: (0, 0)
      }
    } else {
      // Group by type
      let mut type_counts = []
      let mut processed_types = []
      
      for anomaly in anomalies {
        if not(processed_types.contains(anomaly.anomaly_type)) {
          processed_types = processed_types.push(anomaly.anomaly_type)
          let count = anomalies.count(fn(a) { a.anomaly_type == anomaly.anomaly_type })
          type_counts = type_counts.push((anomaly.anomaly_type, count))
        }
      }
      
      // Group by severity
      let mut severity_counts = []
      let mut processed_severities = []
      
      for anomaly in anomalies {
        if not(processed_severities.contains(anomaly.severity)) {
          processed_severities = processed_severities.push(anomaly.severity)
          let count = anomalies.count(fn(a) { a.severity == anomaly.severity })
          severity_counts = severity_counts.push((anomaly.severity, count))
        }
      }
      
      // Find highest confidence
      let highest_confidence = anomalies.reduce(fn(max, anomaly) {
        if anomaly.confidence > max { anomaly.confidence } else { max }
      }, 0.0)
      
      // Find time window
      let timestamps = anomalies.map(fn(a) { a.timestamp })
      let min_time = timestamps.reduce(fn(min, ts) { if ts < min { ts } else { min } }, timestamps[0])
      let max_time = timestamps.reduce(fn(max, ts) { if ts > max { ts } else { max } }, timestamps[0])
      
      {
        total_anomalies: anomalies.length(),
        anomalies_by_type: type_counts,
        anomalies_by_severity: severity_counts,
        highest_confidence,
        time_window: (min_time, max_time)
      }
    }
  }
  
  // Test anomaly aggregation
  let all_anomalies = spike_anomalies + trend_anomalies + deviating_anomalies
  let summary = aggregate_anomalies(all_anomalies)
  
  assert_eq(summary.total_anomalies, 3)
  assert_eq(summary.anomalies_by_type.length(), 3)
  assert_eq(summary.anomalies_by_severity.length(), 2)
  assert_eq(summary.highest_confidence, 1.0)
}

// Test 3: Real-time Alerting and Notification
test "real-time alerting and notification system" {
  // Define alert severity
  enum AlertSeverity {
    Info
    Warning
    Error
    Critical
  }
  
  // Define alert status
  enum AlertStatus {
    Firing
    Resolved
    Suppressed
  }
  
  // Define alert
  type Alert = {
    id: String,
    name: String,
    severity: AlertSeverity,
    status: AlertStatus,
    message: String,
    labels: Array<(String, String)>,
    annotations: Array<(String, String)>,
    starts_at: Int,
    ends_at: Option<Int>,
    fingerprint: String
  }
  
  // Define notification channel
  type NotificationChannel = {
    id: String,
    name: String,
    type: String,  // "email", "slack", "pagerduty", "webhook"
    config: Array<(String, String)>,
    enabled: Bool
  }
  
  // Define alert rule
  type AlertRule = {
    id: String,
    name: String,
    condition: String,
    severity: AlertSeverity,
    for_duration: Int,  // seconds
    labels: Array<(String, String)>,
    annotations: Array<(String, String)>,
    notification_channels: Array<String>
  }
  
  // Define alert manager
  type AlertManager = {
    rules: Array<AlertRule>,
    active_alerts: Array<Alert>,
    notification_channels: Array<NotificationChannel>,
    alert_history: Array<Alert>
  }
  
  // Create alert manager
  let create_alert_manager = fn() {
    {
      rules: [],
      active_alerts: [],
      notification_channels: [],
      alert_history: []
    }
  }
  
  // Add alert rule
  let add_alert_rule = fn(manager: AlertManager, rule: AlertRule) {
    { manager | rules: manager.rules.push(rule) }
  }
  
  // Add notification channel
  let add_notification_channel = fn(manager: AlertManager, channel: NotificationChannel) {
    { manager | notification_channels: manager.notification_channels.push(channel) }
  }
  
  // Evaluate alert rule
  let evaluate_rule = fn(rule: AlertRule, metrics: Array<(String, Float)>, current_time: Int) {
    // Simplified rule evaluation - just check if any metric matches condition
    let condition_met = metrics.any(fn(metric) {
      // Very simplified condition parsing
      if rule.condition.contains("cpu_usage > 80") {
        metric.0 == "cpu_usage" && metric.1 > 80.0
      } else if rule.condition.contains("memory_usage > 85") {
        metric.0 == "memory_usage" && metric.1 > 85.0
      } else if rule.condition.contains("error_rate > 5") {
        metric.0 == "error_rate" && metric.1 > 5.0
      } else {
        false
      }
    })
    
    if condition_met {
      let fingerprint = rule.name + ":" + metrics.map(fn(m) { m.0 + "=" + m.1.to_string() }).join(",")
      
      Some({
        id: "alert-" + current_time.to_string(),
        name: rule.name,
        severity: rule.severity,
        status: AlertStatus::Firing,
        message: rule.name + " is firing",
        labels: rule.labels,
        annotations: rule.annotations,
        starts_at: current_time,
        ends_at: None,
        fingerprint
      })
    } else {
      None
    }
  }
  
  // Check for alert resolution
  let check_alert_resolution = fn(alert: Alert, metrics: Array<(String, Float)>, current_time: Int) {
    // Find the rule for this alert
    let rule_name = alert.name
    
    // Simplified - check if condition is no longer met
    let condition_met = metrics.any(fn(metric) {
      if rule_name.contains("High CPU") {
        metric.0 == "cpu_usage" && metric.1 > 80.0
      } else if rule_name.contains("High Memory") {
        metric.0 == "memory_usage" && metric.1 > 85.0
      } else if rule_name.contains("High Error Rate") {
        metric.0 == "error_rate" && metric.1 > 5.0
      } else {
        false
      }
    })
    
    if not(condition_met) {
      { alert | 
        status: AlertStatus::Resolved,
        ends_at: Some(current_time)
      }
    } else {
      alert
    }
  }
  
  // Send notification
  let send_notification = fn(alert: Alert, channel: NotificationChannel) {
    // Simulate sending notification
    {
      channel_id: channel.id,
      channel_type: channel.type,
      alert_id: alert.id,
      alert_name: alert.name,
      severity: match alert.severity {
        AlertSeverity::Info => "INFO"
        AlertSeverity::Warning => "WARNING"
        AlertSeverity::Error => "ERROR"
        AlertSeverity::Critical => "CRITICAL"
      },
      message: alert.message,
      timestamp: alert.starts_at,
      sent: true
    }
  }
  
  // Process alerts
  let process_alerts = fn(manager: AlertManager, metrics: Array<(String, Float)>, current_time: Int) {
    let mut new_alerts = []
    let mut updated_alerts = []
    
    // Evaluate all rules
    for rule in manager.rules {
      match evaluate_rule(rule, metrics, current_time) {
        Some(alert) => {
          // Check if this alert already exists
          let existing = manager.active_alerts.find(fn(a) { a.fingerprint == alert.fingerprint })
          
          match existing {
            Some(_) => {
              // Alert already exists, no action needed
            }
            None => {
              // New alert
              new_alerts = new_alerts.push(alert)
            }
          }
        }
        None => {}
      }
    }
    
    // Check for resolution of existing alerts
    for alert in manager.active_alerts {
      let updated = check_alert_resolution(alert, metrics, current_time)
      
      if updated.status == AlertStatus::Resolved {
        updated_alerts = updated_alerts.push(updated)
      }
    }
    
    // Send notifications for new alerts
    let mut notifications = []
    for alert in new_alerts {
      for channel_id in alert.labels
        .filter(fn(l) { l.0 == "notification_channels" })
        .flat_map(fn(l) { l.1.split(",") }) {
        let channel = manager.notification_channels.find(fn(c) { c.id == channel_id.trim() })
        
        match channel {
          Some(ch) => {
            if ch.enabled {
              notifications = notifications.push(send_notification(alert, ch))
            }
          }
          None => {}
        }
      }
    }
    
    {
      new_alerts,
      resolved_alerts: updated_alerts,
      notifications
    }
  }
  
  // Test alert manager creation
  let manager = create_alert_manager()
  assert_eq(manager.rules.length(), 0)
  assert_eq(manager.active_alerts.length(), 0)
  assert_eq(manager.notification_channels.length(), 0)
  
  // Create alert rules
  let cpu_rule = {
    id: "cpu-rule-1",
    name: "High CPU Usage",
    condition: "cpu_usage > 80",
    severity: AlertSeverity::Warning,
    for_duration: 300,
    labels: [
      ("team", "platform"),
      ("notification_channels", "email,slack")
    ],
    annotations: [
      ("description", "CPU usage is above 80%"),
      ("runbook_url", "https://runbooks.example.com/cpu")
    ],
    notification_channels: ["email-1", "slack-1"]
  }
  
  let memory_rule = {
    id: "memory-rule-1",
    name: "High Memory Usage",
    condition: "memory_usage > 85",
    severity: AlertSeverity::Critical,
    for_duration: 180,
    labels: [
      ("team", "platform"),
      ("notification_channels", "email,pagerduty")
    ],
    annotations: [
      ("description", "Memory usage is above 85%"),
      ("runbook_url", "https://runbooks.example.com/memory")
    ],
    notification_channels: ["email-1", "pagerduty-1"]
  }
  
  // Add rules
  let manager1 = add_alert_rule(manager, cpu_rule)
  let manager2 = add_alert_rule(manager1, memory_rule)
  assert_eq(manager2.rules.length(), 2)
  
  // Create notification channels
  let email_channel = {
    id: "email-1",
    name: "Email Notifications",
    type: "email",
    config: [
      ("smtp_server", "smtp.example.com"),
      ("from", "alerts@example.com"),
      ("to", "team@example.com")
    ],
    enabled: true
  }
  
  let slack_channel = {
    id: "slack-1",
    name: "Slack Notifications",
    type: "slack",
    config: [
      ("webhook_url", "https://hooks.slack.com/..."),
      ("channel", "#alerts")
    ],
    enabled: true
  }
  
  let pagerduty_channel = {
    id: "pagerduty-1",
    name: "PagerDuty Notifications",
    type: "pagerduty",
    config: [
      ("service_key", "abc123"),
      ("severity", "high")
    ],
    enabled: true
  }
  
  // Add channels
  let manager3 = add_notification_channel(manager2, email_channel)
  let manager4 = add_notification_channel(manager3, slack_channel)
  let manager5 = add_notification_channel(manager4, pagerduty_channel)
  assert_eq(manager5.notification_channels.length(), 3)
  
  // Test alert evaluation
  let normal_metrics = [
    ("cpu_usage", 65.0),
    ("memory_usage", 70.0),
    ("error_rate", 0.5)
  ]
  
  let normal_result = process_alerts(manager5, normal_metrics, 1640995200)
  assert_eq(normal_result.new_alerts.length(), 0)  // No alerts should fire
  assert_eq(normal_result.resolved_alerts.length(), 0)
  assert_eq(normal_result.notifications.length(), 0)
  
  // Test alert firing
  let high_cpu_metrics = [
    ("cpu_usage", 85.0),
    ("memory_usage", 70.0),
    ("error_rate", 0.5)
  ]
  
  let cpu_result = process_alerts(manager5, high_cpu_metrics, 1640995200)
  assert_eq(cpu_result.new_alerts.length(), 1)  // CPU alert should fire
  assert_eq(cpu_result.resolved_alerts.length(), 0)
  assert_eq(cpu_result.notifications.length(), 2)  // Email and Slack
  
  let cpu_alert = cpu_result.new_alerts[0]
  assert_eq(cpu_alert.name, "High CPU Usage")
  assert_eq(cpu_alert.severity, AlertSeverity::Warning)
  assert_eq(cpu_alert.status, AlertStatus::Firing)
  
  // Test critical alert
  let critical_metrics = [
    ("cpu_usage", 65.0),
    ("memory_usage", 90.0),
    ("error_rate", 0.5)
  ]
  
  let critical_result = process_alerts(manager5, critical_metrics, 1640995300)
  assert_eq(critical_result.new_alerts.length(), 1)  // Memory alert should fire
  assert_eq(critical_result.resolved_alerts.length(), 0)
  assert_eq(critical_result.notifications.length(), 2)  // Email and PagerDuty
  
  let memory_alert = critical_result.new_alerts[0]
  assert_eq(memory_alert.name, "High Memory Usage")
  assert_eq(memory_alert.severity, AlertSeverity::Critical)
  assert_eq(memory_alert.status, AlertStatus::Firing)
  
  // Test alert resolution
  let resolved_metrics = [
    ("cpu_usage", 60.0),
    ("memory_usage", 70.0),
    ("error_rate", 0.5)
  ]
  
  // First, simulate manager with active alerts
  let manager_with_alerts = { manager5 | 
    active_alerts: [cpu_alert, memory_alert] 
  }
  
  let resolved_result = process_alerts(manager_with_alerts, resolved_metrics, 1640995400)
  assert_eq(resolved_result.new_alerts.length(), 0)
  assert_eq(resolved_result.resolved_alerts.length(), 2)  // Both alerts should resolve
  assert_eq(resolved_result.notifications.length(), 0)  // No notifications for resolution
  
  let resolved_cpu = resolved_result.resolved_alerts.find(fn(a) { a.name == "High CPU Usage" })
  match resolved_cpu {
    Some(alert) => {
      assert_eq(alert.status, AlertStatus::Resolved)
      assert_eq(alert.ends_at, Some(1640995400))
    }
    None => assert_true(false)
  }
  
  // Test notification details
  let email_notification = cpu_result.notifications.find(fn(n) { n.channel_type == "email" })
  match email_notification {
    Some(notification) => {
      assert_eq(notification.alert_name, "High CPU Usage")
      assert_eq(notification.severity, "WARNING")
      assert_eq(notification.sent, true)
    }
    None => assert_true(false)
  }
}

// Test 4: Real-time Dashboard Updates
test "real-time dashboard updates and data streaming" {
  // Define dashboard widget
  type DashboardWidget = {
    id: String,
    type: String,  // "metric", "chart", "table", "text"
    title: String,
    query: String,
    refresh_interval: Int,  // seconds
    data: String  // JSON string
    last_updated: Int
  }
  
  // Define dashboard
  type Dashboard = {
    id: String,
    name: String,
    widgets: Array<DashboardWidget>,
    refresh_interval: Int,
    last_refresh: Int
  }
  
  // Define dashboard update
  type DashboardUpdate = {
    dashboard_id: String,
    widget_id: String,
    data: String,
    timestamp: Int
  }
  
  // Define dashboard manager
  type DashboardManager = {
    dashboards: Array<Dashboard>,
    updates: Array<DashboardUpdate>,
    subscribers: Array<String>  // WebSocket connection IDs
  }
  
  // Create dashboard
  let create_dashboard = fn(id: String, name: String, refresh_interval: Int) {
    {
      id,
      name,
      widgets: [],
      refresh_interval,
      last_refresh: 0
    }
  }
  
  // Add widget to dashboard
  let add_widget = fn(dashboard: Dashboard, widget: DashboardWidget) {
    { dashboard | widgets: dashboard.widgets.push(widget) }
  }
  
  // Create dashboard manager
  let create_dashboard_manager = fn() {
    {
      dashboards: [],
      updates: [],
      subscribers: []
    }
  }
  
  // Add dashboard to manager
  let add_dashboard = fn(manager: DashboardManager, dashboard: Dashboard) {
    { manager | dashboards: manager.dashboards.push(dashboard) }
  }
  
  // Add subscriber
  let add_subscriber = fn(manager: DashboardManager, subscriber_id: String) {
    { manager | subscribers: manager.subscribers.push(subscriber_id) }
  }
  
  // Simulate data query
  let query_data = fn(query: String, current_time: Int) {
    // Simplified query simulation
    if query.contains("cpu_usage") {
      "{\"value\": " + (65.0 + (current_time % 20) as Float).to_string() + ", \"unit\": \"percent\"}"
    } else if query.contains("memory_usage") {
      "{\"value\": " + (70.0 + (current_time % 25) as Float).to_string() + ", \"unit\": \"percent\"}"
    } else if query.contains("request_rate") {
      "{\"value\": " + (100.0 + (current_time % 50) as Float).to_string() + ", \"unit\": \"req/s\"}"
    } else if query.contains("error_rate") {
      "{\"value\": " + ((current_time % 10) as Float / 2.0).to_string() + ", \"unit\": \"percent\"}"
    } else {
      "{\"error\": \"Unknown query\"}"
    }
  }
  
  // Update dashboard widgets
  let update_dashboard = fn(manager: DashboardManager, dashboard_id: String, current_time: Int) {
    let dashboard = manager.dashboards.find(fn(d) { d.id == dashboard_id })
    
    match dashboard {
      Some(dash) => {
        let updated_widgets = dash.widgets.map(fn(widget) {
          if current_time - widget.last_updated >= widget.refresh_interval {
            let new_data = query_data(widget.query, current_time)
            
            {
              id: widget.id,
              type: widget.type,
              title: widget.title,
              query: widget.query,
              refresh_interval: widget.refresh_interval,
              data: new_data,
              last_updated: current_time
            }
          } else {
            widget
          }
        })
        
        let updated_dashboard = { dash | widgets: updated_widgets, last_refresh: current_time }
        
        // Create update records
        let updates = updated_widgets.filter(fn(w) { w.last_updated == current_time })
          .map(fn(widget) {
            {
              dashboard_id,
              widget_id: widget.id,
              data: widget.data,
              timestamp: current_time
            }
          })
        
        // Update dashboard in manager
        let updated_dashboards = manager.dashboards.map(fn(d) {
          if d.id == dashboard_id {
            updated_dashboard
          } else {
            d
          }
        })
        
        {
          dashboards: updated_dashboards,
          updates: manager.updates + updates,
          subscribers: manager.subscribers
        }
      }
      None => manager
    }
  }
  
  // Get dashboard for subscriber
  let get_dashboard_for_subscriber = fn(manager: DashboardManager, dashboard_id: String) {
    let dashboard = manager.dashboards.find(fn(d) { d.id == dashboard_id })
    
    match dashboard {
      Some(dash) => {
        Some({
          dashboard: dash,
          last_updates: manager.updates.filter(fn(u) { u.dashboard_id == dashboard_id })
        })
      }
      None => None
    }
  }
  
  // Test dashboard creation
  let dashboard = create_dashboard("main-dashboard", "System Overview", 5)
  assert_eq(dashboard.id, "main-dashboard")
  assert_eq(dashboard.name, "System Overview")
  assert_eq(dashboard.refresh_interval, 5)
  assert_eq(dashboard.widgets.length(), 0)
  
  // Create widgets
  let cpu_widget = {
    id: "cpu-metric",
    type: "metric",
    title: "CPU Usage",
    query: "cpu_usage",
    refresh_interval: 5,
    data: "{}",
    last_updated: 0
  }
  
  let memory_widget = {
    id: "memory-metric",
    type: "metric",
    title: "Memory Usage",
    query: "memory_usage",
    refresh_interval: 10,
    data: "{}",
    last_updated: 0
  }
  
  let request_chart = {
    id: "request-chart",
    type: "chart",
    title: "Request Rate",
    query: "request_rate",
    refresh_interval: 3,
    data: "{}",
    last_updated: 0
  }
  
  // Add widgets to dashboard
  let dashboard1 = add_widget(dashboard, cpu_widget)
  let dashboard2 = add_widget(dashboard1, memory_widget)
  let dashboard3 = add_widget(dashboard2, request_chart)
  
  assert_eq(dashboard3.widgets.length(), 3)
  
  // Test dashboard manager
  let manager = create_dashboard_manager()
  let manager1 = add_dashboard(manager, dashboard3)
  let manager2 = add_subscriber(manager1, "websocket-123")
  let manager3 = add_subscriber(manager2, "websocket-456")
  
  assert_eq(manager3.dashboards.length(), 1)
  assert_eq(manager3.subscribers.length(), 2)
  
  // Test dashboard updates
  let base_time = 1640995200
  let manager4 = update_dashboard(manager3, "main-dashboard", base_time)
  
  // All widgets should be updated
  let updated_dashboard = manager4.dashboards.find(fn(d) { d.id == "main-dashboard" })
  match updated_dashboard {
    Some(dash) => {
      assert_eq(dash.widgets[0].last_updated, base_time)
      assert_eq(dash.widgets[1].last_updated, base_time)
      assert_eq(dash.widgets[2].last_updated, base_time)
      
      // Check data updates
      assert_true(dash.widgets[0].data.contains("cpu_usage"))
      assert_true(dash.widgets[1].data.contains("memory_usage"))
      assert_true(dash.widgets[2].data.contains("request_rate"))
    }
    None => assert_true(false)
  }
  
  assert_eq(manager4.updates.length(), 3)
  
  // Test selective updates based on refresh interval
  let manager5 = update_dashboard(manager4, "main-dashboard", base_time + 3)
  
  let selective_dashboard = manager5.dashboards.find(fn(d) { d.id == "main-dashboard" })
  match selective_dashboard {
    Some(dash) => {
      // Only request chart (3s interval) should be updated
      assert_eq(dash.widgets[0].last_updated, base_time)  // CPU widget (5s interval) not updated
      assert_eq(dash.widgets[1].last_updated, base_time)  // Memory widget (10s interval) not updated
      assert_eq(dash.widgets[2].last_updated, base_time + 3)  // Request chart (3s interval) updated
    }
    None => assert_true(false)
  }
  
  assert_eq(manager5.updates.length(), 4)  // 3 initial + 1 new
  
  // Test dashboard retrieval for subscriber
  let dashboard_data = get_dashboard_for_subscriber(manager5, "main-dashboard")
  match dashboard_data {
    Some(data) => {
      assert_eq(data.dashboard.id, "main-dashboard")
      assert_eq(data.last_updates.length(), 4)
      
      // Find latest update for request chart
      let latest_request_update = data.last_updates
        .filter(fn(u) { u.widget_id == "request-chart" })
        .reduce(fn(latest, update) {
          if update.timestamp > latest.timestamp { update } else { latest }
        }, data.last_updates[0])
      
      assert_eq(latest_request_update.timestamp, base_time + 3)
    }
    None => assert_true(false)
  }
  
  // Test real-time data streaming
  type DataStream = {
    stream_id: String,
    data_points: Array<(Int, String)>,  // (timestamp, data)
    subscribers: Array<String>
  }
  
  let create_stream = fn(stream_id: String) {
    {
      stream_id,
      data_points: [],
      subscribers: []
    }
  }
  
  let subscribe_to_stream = fn(stream: DataStream, subscriber_id: String) {
    { stream | subscribers: stream.subscribers.push(subscriber_id) }
  }
  
  let add_data_point = fn(stream: DataStream, timestamp: Int, data: String) {
    { stream | data_points: stream.data_points.push((timestamp, data)) }
  }
  
  // Create stream for real-time updates
  let cpu_stream = create_stream("cpu-stream")
  let cpu_stream1 = subscribe_to_stream(cpu_stream, "websocket-123")
  let cpu_stream2 = add_data_point(cpu_stream1, base_time, "{\"value\": 75.5}")
  let cpu_stream3 = add_data_point(cpu_stream2, base_time + 1, "{\"value\": 78.2}")
  
  assert_eq(cpu_stream3.subscribers.length(), 1)
  assert_eq(cpu_stream3.data_points.length(), 2)
  
  // Test data aggregation for charts
  let aggregate_time_series = fn(data_points: Array<(Int, String)>, interval: Int) {
    let mut aggregated = []
    let mut processed_intervals = []
    
    for (timestamp, data) in data_points {
      let interval_start = (timestamp / interval) * interval
      
      if not(processed_intervals.contains(interval_start)) {
        processed_intervals = processed_intervals.push(interval_start)
        
        let interval_points = data_points.filter(fn((ts, _) {
          (ts / interval) * interval == interval_start
        })
        
        // Simple aggregation - just take the latest value
        let latest_point = interval_points.reduce(fn(latest, point) {
          if point.0 > latest.0 { point } else { latest }
        }, interval_points[0])
        
        aggregated = aggregated.push(latest_point)
      }
    }
    
    aggregated
  }
  
  // Test time series aggregation
  let time_series_data = [
    (base_time, "{\"value\": 10.0}"),
    (base_time + 1, "{\"value\": 12.0}"),
    (base_time + 2, "{\"value\": 11.0}"),
    (base_time + 3, "{\"value\": 15.0}"),
    (base_time + 4, "{\"value\": 14.0}"),
    (base_time + 5, "{\"value\": 18.0}"),
    (base_time + 6, "{\"value\": 16.0}"),
    (base_time + 7, "{\"value\": 20.0}")
  ]
  
  let aggregated = aggregate_time_series(time_series_data, 3)  // 3-second intervals
  assert_eq(aggregated.length(), 3)
  
  assert_eq(aggregated[0].0, base_time + 2)  // Latest in first interval
  assert_eq(aggregated[0].1, "{\"value\": 11.0}")
  
  assert_eq(aggregated[1].0, base_time + 5)  // Latest in second interval
  assert_eq(aggregated[1].1, "{\"value\": 18.0}")
  
  assert_eq(aggregated[2].0, base_time + 7)  // Latest in third interval
  assert_eq(aggregated[2].1, "{\"value\": 20.0}")
}

// Test 5: Performance Metrics for Real-time Systems
test "performance metrics and optimization for real-time monitoring" {
  // Define performance metric
  type PerformanceMetric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int
  }
  
  // Define performance profile
  type PerformanceProfile = {
    processing_latency_ms: Float,
    throughput_ops_per_sec: Float,
    memory_usage_mb: Float,
    cpu_usage_percent: Float,
    error_rate_percent: Float,
    timestamp: Int
  }
  
  // Define performance threshold
  type PerformanceThreshold = {
    metric_name: String,
    max_value: Float,
    min_value: Option<Float>,
    unit: String
  }
  
  // Calculate processing latency
  let measure_latency = fn(start_time: Int, end_time: Int) {
    (end_time - start_time) as Float
  }
  
  // Calculate throughput
  let calculate_throughput = fn(operation_count: Int, duration_ms: Int) {
    if duration_ms > 0 {
      (operation_count as Float) / ((duration_ms as Float) / 1000.0)
    } else {
      0.0
    }
  }
  
  // Create performance profile
  let create_performance_profile = fn(processing_latency_ms: Float, throughput_ops_per_sec: Float,
                                     memory_usage_mb: Float, cpu_usage_percent: Float,
                                     error_rate_percent: Float, timestamp: Int) {
    {
      processing_latency_ms,
      throughput_ops_per_sec,
      memory_usage_mb,
      cpu_usage_percent,
      error_rate_percent,
      timestamp
    }
  }
  
  // Check performance against thresholds
  let check_performance = fn(profile: PerformanceProfile, thresholds: Array<PerformanceThreshold>) {
    let mut violations = []
    
    for threshold in thresholds {
      let value = match threshold.metric_name {
        "processing_latency" => profile.processing_latency_ms
        "throughput" => profile.throughput_ops_per_sec
        "memory_usage" => profile.memory_usage_mb
        "cpu_usage" => profile.cpu_usage_percent
        "error_rate" => profile.error_rate_percent
        _ => 0.0
      }
      
      let max_violation = value > threshold.max_value
      let min_violation = match threshold.min_value {
        Some(min) => value < min
        None => false
      }
      
      if max_violation || min_violation {
        violations = violations.push({
          metric_name: threshold.metric_name,
          current_value: value,
          threshold_max: threshold.max_value,
          threshold_min: threshold.min_value,
          unit: threshold.unit,
          violated_max: max_violation,
          violated_min: min_violation
        })
      }
    }
    
    violations
  }
  
  // Test latency measurement
  let start_time = 1640995200000  // milliseconds
  let end_time = 1640995200150    // 150ms later
  let latency = measure_latency(start_time, end_time)
  assert_eq(latency, 150.0)
  
  // Test throughput calculation
  let throughput = calculate_throughput(1000, 5000)  // 1000 operations in 5 seconds
  assert_eq(throughput, 200.0)  // 200 ops/sec
  
  let zero_duration_throughput = calculate_throughput(100, 0)
  assert_eq(zero_duration_throughput, 0.0)
  
  // Test performance profile creation
  let profile = create_performance_profile(
    25.5,    // processing_latency_ms
    1500.0,  // throughput_ops_per_sec
    512.0,   // memory_usage_mb
    65.0,    // cpu_usage_percent
    0.1,     // error_rate_percent
    1640995200
  )
  
  assert_eq(profile.processing_latency_ms, 25.5)
  assert_eq(profile.throughput_ops_per_sec, 1500.0)
  assert_eq(profile.memory_usage_mb, 512.0)
  assert_eq(profile.cpu_usage_percent, 65.0)
  assert_eq(profile.error_rate_percent, 0.1)
  
  // Test performance thresholds
  let thresholds = [
    {
      metric_name: "processing_latency",
      max_value: 100.0,
      min_value: None,
      unit: "ms"
    },
    {
      metric_name: "throughput",
      max_value: 10000.0,
      min_value: Some(100.0),
      unit: "ops/sec"
    },
    {
      metric_name: "memory_usage",
      max_value: 1024.0,
      min_value: None,
      unit: "MB"
    },
    {
      metric_name: "cpu_usage",
      max_value: 80.0,
      min_value: None,
      unit: "percent"
    },
    {
      metric_name: "error_rate",
      max_value: 1.0,
      min_value: None,
      unit: "percent"
    }
  ]
  
  let violations = check_performance(profile, thresholds)
  assert_eq(violations.length(), 0)  // No violations with good profile
  
  // Test with violations
  let poor_profile = create_performance_profile(
    150.0,   // processing_latency_ms - exceeds threshold
    50.0,    // throughput_ops_per_sec - below minimum
    2048.0,  // memory_usage_mb - exceeds threshold
    85.0,    // cpu_usage_percent - exceeds threshold
    2.5,     // error_rate_percent - exceeds threshold
    1640995200
  )
  
  let poor_violations = check_performance(poor_profile, thresholds)
  assert_eq(poor_violations.length(), 5)  // All metrics violate thresholds
  
  let latency_violation = poor_violations.find(fn(v) { v.metric_name == "processing_latency" })
  match latency_violation {
    Some(violation) => {
      assert_eq(violation.current_value, 150.0)
      assert_eq(violation.threshold_max, 100.0)
      assert_eq(violation.violated_max, true)
    }
    None => assert_true(false)
  }
  
  let throughput_violation = poor_violations.find(fn(v) { v.metric_name == "throughput" })
  match throughput_violation {
    Some(violation) => {
      assert_eq(violation.current_value, 50.0)
      assert_eq(violation.threshold_min, Some(100.0))
      assert_eq(violation.violated_min, true)
    }
    None => assert_true(false)
  }
  
  // Test performance aggregation
  let aggregate_profiles = fn(profiles: Array<PerformanceProfile>) {
    if profiles.length() == 0 {
      {
        avg_processing_latency_ms: 0.0,
        avg_throughput_ops_per_sec: 0.0,
        avg_memory_usage_mb: 0.0,
        avg_cpu_usage_percent: 0.0,
        avg_error_rate_percent: 0.0,
        sample_count: 0
      }
    } else {
      let count = profiles.length() as Float
      
      {
        avg_processing_latency_ms: profiles.reduce(fn(sum, p) { sum + p.processing_latency_ms }, 0.0) / count,
        avg_throughput_ops_per_sec: profiles.reduce(fn(sum, p) { sum + p.throughput_ops_per_sec }, 0.0) / count,
        avg_memory_usage_mb: profiles.reduce(fn(sum, p) { sum + p.memory_usage_mb }, 0.0) / count,
        avg_cpu_usage_percent: profiles.reduce(fn(sum, p) { sum + p.cpu_usage_percent }, 0.0) / count,
        avg_error_rate_percent: profiles.reduce(fn(sum, p) { sum + p.error_rate_percent }, 0.0) / count,
        sample_count: profiles.length()
      }
    }
  }
  
  // Test performance aggregation
  let profiles = [
    create_performance_profile(20.0, 1000.0, 256.0, 50.0, 0.1, 1640995200),
    create_performance_profile(30.0, 1500.0, 512.0, 60.0, 0.2, 1640995210),
    create_performance_profile(25.0, 1200.0, 384.0, 55.0, 0.15, 1640995220),
    create_performance_profile(35.0, 800.0, 640.0, 70.0, 0.25, 1640995230),
    create_performance_profile(40.0, 2000.0, 768.0, 75.0, 0.05, 1640995240)
  ]
  
  let aggregated = aggregate_profiles(profiles)
  assert_eq(aggregated.avg_processing_latency_ms, 30.0)  // (20+30+25+35+40)/5
  assert_eq(aggregated.avg_throughput_ops_per_sec, 1300.0)  // (1000+1500+1200+800+2000)/5
  assert_eq(aggregated.avg_memory_usage_mb, 512.0)  // (256+512+384+640+768)/5
  assert_eq(aggregated.avg_cpu_usage_percent, 62.0)  // (50+60+55+70+75)/5
  assert_eq(aggregated.avg_error_rate_percent, 0.15)  // (0.1+0.2+0.15+0.25+0.05)/5
  assert_eq(aggregated.sample_count, 5)
  
  // Test performance trend analysis
  let analyze_performance_trend = fn(profiles: Array<PerformanceProfile>, metric_name: String) {
    if profiles.length() < 2 {
      {
        trend: "INSUFFICIENT_DATA",
        slope: 0.0,
        correlation: 0.0
      }
    } else {
      let values = profiles.map(fn(p) {
        match metric_name {
          "processing_latency" => p.processing_latency_ms
          "throughput" => p.throughput_ops_per_sec
          "memory_usage" => p.memory_usage_mb
          "cpu_usage" => p.cpu_usage_percent
          "error_rate" => p.error_rate_percent
          _ => 0.0
        }
      })
      
      let timestamps = profiles.map(fn(p) { p.timestamp })
      
      // Simple linear regression
      let n = values.length()
      let sum_x = timestamps.reduce(fn(sum, ts) { sum + ts }, 0)
      let sum_y = values.reduce(fn(sum, v) { sum + v }, 0)
      let sum_xy = timestamps.reduce_with_index(fn(sum, ts, i) { sum + (ts * values[i]) }, 0)
      let sum_x2 = timestamps.reduce(fn(sum, ts) { sum + (ts * ts) }, 0)
      
      let slope = ((n as Float * sum_xy as Float) - (sum_x as Float * sum_y as Float)) /
                  ((n as Float * sum_x2 as Float) - (sum_x as Float * sum_x as Float))
      
      // Determine trend
      let trend = if slope.abs() < 0.001 {
        "STABLE"
      } else if slope > 0 {
        "INCREASING"
      } else {
        "DECREASING"
      }
      
      // Simplified correlation calculation
      let correlation = if slope.abs() > 0.0 {
        (slope / (slope.abs())).min(1.0)
      } else {
        0.0
      }
      
      {
        trend,
        slope,
        correlation
      }
    }
  }
  
  // Test trend analysis
  let latency_trend = analyze_performance_trend(profiles, "processing_latency")
  assert_eq(latency_trend.trend, "INCREASING")  // Latency is increasing over time
  assert_true(latency_trend.slope > 0.0)
  
  let throughput_trend = analyze_performance_trend(profiles, "throughput")
  assert_eq(throughput_trend.trend, "STABLE")  // Throughput fluctuates but overall stable
  
  // Test performance optimization recommendations
  let generate_recommendations = fn(violations: Array<{
    metric_name: String, current_value: Float, threshold_max: Float, unit: String
  }>) {
    violations.map(fn(violation) {
      match violation.metric_name {
        "processing_latency" => {
          "Consider optimizing algorithms, adding caching, or scaling resources to reduce processing latency"
        }
        "throughput" => {
          "Consider horizontal scaling, optimizing queries, or reducing processing overhead to increase throughput"
        }
        "memory_usage" => {
          "Review memory usage patterns, implement memory pooling, or consider increasing available memory"
        }
        "cpu_usage" => {
          "Optimize CPU-intensive operations, consider scaling out, or profile for performance bottlenecks"
        }
        "error_rate" => {
          "Investigate and fix root causes of errors, improve error handling, and implement circuit breakers"
        }
        _ => {
          "Review system performance and identify optimization opportunities"
        }
      }
    })
  }
  
  // Test recommendations
  let recommendations = generate_recommendations(poor_violations)
  assert_eq(recommendations.length(), 5)
  
  let latency_recommendation = recommendations.find(fn(r) { r.contains("latency") })
  match latency_recommendation {
    Some(rec) => assert_true(rec.contains("optimizing algorithms"))
    None => assert_true(false)
  }
  
  let memory_recommendation = recommendations.find(fn(r) { r.contains("memory") })
  match memory_recommendation {
    Some(rec) => assert_true(rec.contains("memory usage patterns"))
    None => assert_true(false)
  }
}