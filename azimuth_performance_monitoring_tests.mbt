// Azimuth Performance Monitoring Tests
// This file contains test cases for performance monitoring functionality

// Test 1: Performance Metrics Collection
test "performance metrics collection" {
  // Define performance metric types
  type PerformanceMetric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[String]
  }
  
  // Create sample performance metrics
  let cpu_metrics = [
    { name: "cpu_usage", value: 45.2, unit: "percent", timestamp: 1640995200, tags: ["host:server1", "region:us-west"] },
    { name: "cpu_usage", value: 52.7, unit: "percent", timestamp: 1640995260, tags: ["host:server1", "region:us-west"] },
    { name: "cpu_usage", value: 48.3, unit: "percent", timestamp: 1640995320, tags: ["host:server1", "region:us-west"] }
  ]
  
  let memory_metrics = [
    { name: "memory_usage", value: 67.8, unit: "percent", timestamp: 1640995200, tags: ["host:server1", "region:us-west"] },
    { name: "memory_usage", value: 69.4, unit: "percent", timestamp: 1640995260, tags: ["host:server1", "region:us-west"] },
    { name: "memory_usage", value: 71.2, unit: "percent", timestamp: 1640995320, tags: ["host:server1", "region:us-west"] }
  ]
  
  let disk_metrics = [
    { name: "disk_usage", value: 34.5, unit: "percent", timestamp: 1640995200, tags: ["host:server1", "region:us-west"] },
    { name: "disk_usage", value: 34.6, unit: "percent", timestamp: 1640995260, tags: ["host:server1", "region:us-west"] },
    { name: "disk_usage", value: 34.7, unit: "percent", timestamp: 1640995320, tags: ["host:server1", "region:us-west"] }
  ]
  
  // Collect all metrics
  let all_metrics = cpu_metrics + memory_metrics + disk_metrics
  assert_eq(all_metrics.length(), 9)
  
  // Filter metrics by name
  let filter_by_name = fn(metrics: Array[PerformanceMetric], name: String) {
    metrics.filter(fn(m) { m.name == name })
  }
  
  let cpu_only = filter_by_name(all_metrics, "cpu_usage")
  assert_eq(cpu_only.length(), 3)
  
  // Filter metrics by time range
  let filter_by_time_range = fn(metrics: Array[PerformanceMetric], start: Int, end: Int) {
    metrics.filter(fn(m) { m.timestamp >= start and m.timestamp <= end })
  }
  
  let time_filtered = filter_by_time_range(all_metrics, 1640995200, 1640995260)
  assert_eq(time_filtered.length(), 6)
  
  // Filter metrics by tags
  let filter_by_tags = fn(metrics: Array[PerformanceMetric], tags: Array[String]) {
    metrics.filter(fn(m) { 
      tags.all_fn(tag) { m.tags.contains(tag) }
    })
  }
  
  let region_filtered = filter_by_tags(all_metrics, ["region:us-west"])
  assert_eq(region_filtered.length(), 9)
  
  // Calculate statistics for a metric
  let calculate_stats = fn(metrics: Array[PerformanceMetric]) {
    if metrics.length() == 0 {
      { min: 0.0, max: 0.0, avg: 0.0, count: 0 }
    } else {
      let values = metrics.map(fn(m) { m.value })
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
      let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
      
      {
        min: min,
        max: max,
        avg: sum / values.length().to_float(),
        count: values.length()
      }
    }
  }
  
  let cpu_stats = calculate_stats(cpu_only)
  assert_eq(cpu_stats.count, 3)
  assert_eq(cpu_stats.min, 45.2)
  assert_eq(cpu_stats.max, 52.7)
  assert_eq(cpu_stats.avg, (45.2 + 52.7 + 48.3) / 3.0)
}

// Test 2: Performance Threshold Monitoring
test "performance threshold monitoring" {
  type Threshold = {
    metric_name: String,
    warning_threshold: Float,
    critical_threshold: Float,
    operator: String  // "gt", "lt", "eq"
  }
  
  type Alert = {
    metric_name: String,
    current_value: Float,
    threshold_value: Float,
    severity: String,  // "warning", "critical"
    timestamp: Int
  }
  
  // Define monitoring thresholds
  let thresholds = [
    { metric_name: "cpu_usage", warning_threshold: 70.0, critical_threshold: 90.0, operator: "gt" },
    { metric_name: "memory_usage", warning_threshold: 80.0, critical_threshold: 95.0, operator: "gt" },
    { metric_name: "disk_usage", warning_threshold: 80.0, critical_threshold: 90.0, operator: "gt" },
    { metric_name: "response_time", warning_threshold: 1000.0, critical_threshold: 5000.0, operator: "gt" },
    { metric_name: "availability", warning_threshold: 99.0, critical_threshold: 95.0, operator: "lt" }
  ]
  
  // Create sample metrics
  let current_metrics = [
    { name: "cpu_usage", value: 75.5, unit: "percent", timestamp: 1640995200, tags: [] },
    { name: "memory_usage", value: 85.2, unit: "percent", timestamp: 1640995200, tags: [] },
    { name: "disk_usage", value: 45.3, unit: "percent", timestamp: 1640995200, tags: [] },
    { name: "response_time", value: 2500.0, unit: "ms", timestamp: 1640995200, tags: [] },
    { name: "availability", value: 97.5, unit: "percent", timestamp: 1640995200, tags: [] }
  ]
  
  // Check thresholds and generate alerts
  let check_thresholds = fn(metrics: Array[PerformanceMetric], thresholds: Array[Threshold]) {
    let mut alerts = []
    
    for metric in metrics {
      let threshold = thresholds.find(fn(t) { t.metric_name == metric.name })
      
      match threshold {
        Some(th) => {
          let warning_violated = match th.operator {
            "gt" => metric.value > th.warning_threshold
            "lt" => metric.value < th.warning_threshold
            "eq" => metric.value == th.warning_threshold
            _ => false
          }
          
          let critical_violated = match th.operator {
            "gt" => metric.value > th.critical_threshold
            "lt" => metric.value < th.critical_threshold
            "eq" => metric.value == th.critical_threshold
            _ => false
          }
          
          if critical_violated {
            alerts = alerts.push({
              metric_name: metric.name,
              current_value: metric.value,
              threshold_value: th.critical_threshold,
              severity: "critical",
              timestamp: metric.timestamp
            })
          } else if warning_violated {
            alerts = alerts.push({
              metric_name: metric.name,
              current_value: metric.value,
              threshold_value: th.warning_threshold,
              severity: "warning",
              timestamp: metric.timestamp
            })
          }
        }
        None => {} // No threshold defined for this metric
      }
    }
    
    alerts
  }
  
  let alerts = check_thresholds(current_metrics, thresholds)
  assert_eq(alerts.length(), 4)
  
  // Verify CPU usage alert (warning)
  let cpu_alert = alerts.find(fn(a) { a.metric_name == "cpu_usage" })
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.severity, "warning")
      assert_eq(alert.current_value, 75.5)
      assert_eq(alert.threshold_value, 70.0)
    }
    None => assert_true(false)
  }
  
  // Verify memory usage alert (warning)
  let memory_alert = alerts.find(fn(a) { a.metric_name == "memory_usage" })
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.severity, "warning")
      assert_eq(alert.current_value, 85.2)
      assert_eq(alert.threshold_value, 80.0)
    }
    None => assert_true(false)
  }
  
  // Verify response time alert (warning)
  let response_time_alert = alerts.find(fn(a) { a.metric_name == "response_time" })
  match response_time_alert {
    Some(alert) => {
      assert_eq(alert.severity, "warning")
      assert_eq(alert.current_value, 2500.0)
      assert_eq(alert.threshold_value, 1000.0)
    }
    None => assert_true(false)
  }
  
  // Verify availability alert (critical)
  let availability_alert = alerts.find(fn(a) { a.metric_name == "availability" })
  match availability_alert {
    Some(alert) => {
      assert_eq(alert.severity, "critical")
      assert_eq(alert.current_value, 97.5)
      assert_eq(alert.threshold_value, 95.0)
    }
    None => assert_true(false)
  }
  
  // No disk usage alert (below threshold)
  let disk_alert = alerts.find(fn(a) { a.metric_name == "disk_usage" })
  assert_eq(disk_alert, None)
}

// Test 3: Performance Trend Analysis
test "performance trend analysis" {
  type DataPoint = {
    timestamp: Int,
    value: Float
  }
  
  type TrendAnalysis = {
    direction: String,  // "increasing", "decreasing", "stable"
    slope: Float,
    confidence: Float,
    prediction: Float
  }
  
  // Create sample time series data
  let cpu_data = [
    { timestamp: 1640995200, value: 45.2 },
    { timestamp: 1640995260, value: 47.8 },
    { timestamp: 1640995320, value: 50.3 },
    { timestamp: 1640995380, value: 52.1 },
    { timestamp: 1640995440, value: 54.7 },
    { timestamp: 1640995500, value: 57.2 },
    { timestamp: 1640995560, value: 59.8 },
    { timestamp: 1640995620, value: 62.4 }
  ]
  
  // Simple linear regression for trend analysis
  let calculate_trend = fn(data: Array[DataPoint]) {
    if data.length() < 2 {
      { direction: "stable", slope: 0.0, confidence: 0.0, prediction: 0.0 }
    } else {
      // Calculate slope using simple linear regression
      let n = data.length().to_float()
      let sum_x = data.reduce(fn(acc, dp) { acc + dp.timestamp.to_float() }, 0.0)
      let sum_y = data.reduce(fn(acc, dp) { acc + dp.value }, 0.0)
      let sum_xy = data.reduce(fn(acc, dp) { acc + dp.timestamp.to_float() * dp.value }, 0.0)
      let sum_x2 = data.reduce(fn(acc, dp) { acc + dp.timestamp.to_float() * dp.timestamp.to_float() }, 0.0)
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      // Calculate correlation coefficient for confidence
      let mean_x = sum_x / n
      let mean_y = sum_y / n
      let numerator = data.reduce(fn(acc, dp) { 
        acc + (dp.timestamp.to_float() - mean_x) * (dp.value - mean_y) 
      }, 0.0)
      
      let sum_xx = data.reduce(fn(acc, dp) { 
        acc + (dp.timestamp.to_float() - mean_x) * (dp.timestamp.to_float() - mean_x) 
      }, 0.0)
      
      let sum_yy = data.reduce(fn(acc, dp) { 
        acc + (dp.value - mean_y) * (dp.value - mean_y) 
      }, 0.0)
      
      let correlation = if sum_xx == 0.0 or sum_yy == 0.0 {
        0.0
      } else {
        numerator / (sum_xx * sum_yy).sqrt()
      }
      
      let confidence = correlation.abs()
      
      // Determine direction
      let direction = if slope.abs() < 0.001 {
        "stable"
      } else if slope > 0 {
        "increasing"
      } else {
        "decreasing"
      }
      
      // Predict next value
      let last_timestamp = data[data.length() - 1].timestamp
      let next_timestamp = last_timestamp + 60 // Next time point
      let prediction = slope * next_timestamp.to_float() + intercept
      
      {
        direction: direction,
        slope: slope,
        confidence: confidence,
        prediction: prediction
      }
    }
  }
  
  let cpu_trend = calculate_trend(cpu_data)
  assert_eq(cpu_trend.direction, "increasing")
  assert_true(cpu_trend.slope > 0)
  assert_true(cpu_trend.confidence > 0.8) // High confidence due to clear trend
  assert_true(cpu_trend.prediction > cpu_data[cpu_data.length() - 1].value) // Prediction higher than last value
  
  // Test with decreasing data
  let memory_data = [
    { timestamp: 1640995200, value: 85.2 },
    { timestamp: 1640995260, value: 83.7 },
    { timestamp: 1640995320, value: 81.4 },
    { timestamp: 1640995380, value: 79.8 },
    { timestamp: 1640995440, value: 78.1 },
    { timestamp: 1640995500, value: 76.5 },
    { timestamp: 1640995560, value: 74.9 },
    { timestamp: 1640995620, value: 73.2 }
  ]
  
  let memory_trend = calculate_trend(memory_data)
  assert_eq(memory_trend.direction, "decreasing")
  assert_true(memory_trend.slope < 0)
  assert_true(memory_trend.confidence > 0.8)
  assert_true(memory_trend.prediction < memory_data[memory_data.length() - 1].value)
  
  // Test with stable data
  let stable_data = [
    { timestamp: 1640995200, value: 50.1 },
    { timestamp: 1640995260, value: 50.3 },
    { timestamp: 1640995320, value: 49.9 },
    { timestamp: 1640995380, value: 50.2 },
    { timestamp: 1640995440, value: 50.0 },
    { timestamp: 1640995500, value: 49.8 },
    { timestamp: 1640995560, value: 50.1 },
    { timestamp: 1640995620, value: 49.9 }
  ]
  
  let stable_trend = calculate_trend(stable_data)
  assert_eq(stable_trend.direction, "stable")
  assert_true(stable_trend.slope.abs() < 0.1)
}

// Test 4: Performance Baseline Comparison
test "performance baseline comparison" {
  type BaselineMetric = {
    name: String,
    baseline_value: Float,
    tolerance_percent: Float,
    unit: String
  }
  
  type ComparisonResult = {
    name: String,
    current_value: Float,
    baseline_value: Float,
    deviation_percent: Float,
    status: String  // "within_baseline", "warning", "critical"
  }
  
  // Define performance baselines
  let baselines = [
    { name: "cpu_usage", baseline_value: 50.0, tolerance_percent: 20.0, unit: "percent" },
    { name: "memory_usage", baseline_value: 70.0, tolerance_percent: 15.0, unit: "percent" },
    { name: "response_time", baseline_value: 500.0, tolerance_percent: 50.0, unit: "ms" },
    { name: "throughput", baseline_value: 1000.0, tolerance_percent: 25.0, unit: "req/s" }
  ]
  
  // Current performance metrics
  let current_metrics = [
    { name: "cpu_usage", value: 55.0, unit: "percent", timestamp: 1640995200, tags: [] },
    { name: "memory_usage", value: 85.0, unit: "percent", timestamp: 1640995200, tags: [] },
    { name: "response_time", value: 800.0, unit: "ms", timestamp: 1640995200, tags: [] },
    { name: "throughput", value: 900.0, unit: "req/s", timestamp: 1640995200, tags: [] }
  ]
  
  // Compare current metrics with baselines
  let compare_with_baseline = fn(current: Array[PerformanceMetric], baselines: Array[BaselineMetric]) {
    let mut results = []
    
    for metric in current {
      let baseline = baselines.find(fn(b) { b.name == metric.name })
      
      match baseline {
        Some(bl) => {
          let deviation = if bl.baseline_value == 0.0 {
            0.0
          } else {
            ((metric.value - bl.baseline_value) / bl.baseline_value) * 100.0
          }
          
          let abs_deviation = deviation.abs()
          let status = if abs_deviation <= bl.tolerance_percent {
            "within_baseline"
          } else if abs_deviation <= bl.tolerance_percent * 2.0 {
            "warning"
          } else {
            "critical"
          }
          
          results = results.push({
            name: metric.name,
            current_value: metric.value,
            baseline_value: bl.baseline_value,
            deviation_percent: deviation,
            status: status
          })
        }
        None => {} // No baseline for this metric
      }
    }
    
    results
  }
  
  let comparisons = compare_with_baseline(current_metrics, baselines)
  assert_eq(comparisons.length(), 4)
  
  // Check CPU usage comparison (within tolerance)
  let cpu_comparison = comparisons.find(fn(c) { c.name == "cpu_usage" })
  match cpu_comparison {
    Some(comp) => {
      assert_eq(comp.current_value, 55.0)
      assert_eq(comp.baseline_value, 50.0)
      assert_eq(comp.deviation_percent, 10.0)
      assert_eq(comp.status, "within_baseline")
    }
    None => assert_true(false)
  }
  
  // Check memory usage comparison (warning)
  let memory_comparison = comparisons.find(fn(c) { c.name == "memory_usage" })
  match memory_comparison {
    Some(comp) => {
      assert_eq(comp.current_value, 85.0)
      assert_eq(comp.baseline_value, 70.0)
      assert_eq(comp.deviation_percent, 21.43) // 15/70 * 100 ≈ 21.43%
      assert_eq(comp.status, "warning")
    }
    None => assert_true(false)
  }
  
  // Check response time comparison (warning)
  let response_time_comparison = comparisons.find(fn(c) { c.name == "response_time" })
  match response_time_comparison {
    Some(comp) => {
      assert_eq(comp.current_value, 800.0)
      assert_eq(comp.baseline_value, 500.0)
      assert_eq(comp.deviation_percent, 60.0)
      assert_eq(comp.status, "warning")
    }
    None => assert_true(false)
  }
  
  // Check throughput comparison (within tolerance)
  let throughput_comparison = comparisons.find(fn(c) { c.name == "throughput" })
  match throughput_comparison {
    Some(comp) => {
      assert_eq(comp.current_value, 900.0)
      assert_eq(comp.baseline_value, 1000.0)
      assert_eq(comp.deviation_percent, -10.0)
      assert_eq(comp.status, "within_baseline")
    }
    None => assert_true(false)
  }
}

// Test 5: Performance Anomaly Detection
test "performance anomaly detection" {
  type AnomalyDetector = {
    threshold_multiplier: Float,
    window_size: Int
  }
  
  type AnomalyResult = {
    timestamp: Int,
    value: Float,
    expected_range: (Float, Float),
    is_anomaly: Bool,
    anomaly_score: Float
  }
  
  // Create sample metrics with some anomalies
  let metrics_data = [
    { timestamp: 1640995200, value: 45.2 },
    { timestamp: 1640995260, value: 47.8 },
    { timestamp: 1640995320, value: 46.5 },
    { timestamp: 1640995380, value: 48.1 },
    { timestamp: 1640995440, value: 47.3 },
    { timestamp: 1640995500, value: 78.9 }, // Anomaly
    { timestamp: 1640995560, value: 49.2 },
    { timestamp: 1640995620, value: 46.8 },
    { timestamp: 1640995680, value: 47.5 },
    { timestamp: 1640995740, value: 48.3 },
    { timestamp: 1640995800, value: 12.1 }, // Another anomaly
    { timestamp: 1640995860, value: 47.9 }
  ]
  
  // Statistical anomaly detection using moving window
  let detect_anomalies = fn(data: Array[DataPoint], detector: AnomalyDetector) {
    let mut results = []
    
    for i in 0..data.length() {
      let point = data[i]
      
      // Calculate statistics from window (excluding current point if possible)
      let window_start = if i >= detector.window_size { i - detector.window_size } else { 0 }
      let window_end = if i > 0 { i - 1 } else { 0 }
      
      let window_data = if window_start <= window_end {
        let mut window = []
        for j in window_start..=window_end {
          window = window.push(data[j])
        }
        window
      } else {
        [] // No window data available
      }
      
      if window_data.length() > 0 {
        let values = window_data.map(fn(dp) { dp.value })
        let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
        
        let variance = values.reduce(fn(acc, v) { 
          acc + (v - mean) * (v - mean) 
        }, 0.0) / values.length().to_float()
        
        let std_dev = variance.sqrt()
        let threshold = detector.threshold_multiplier * std_dev
        
        let expected_min = mean - threshold
        let expected_max = mean + threshold
        
        let is_anomaly = point.value < expected_min or point.value > expected_max
        
        // Calculate anomaly score (how many standard deviations from mean)
        let anomaly_score = if std_dev == 0.0 {
          0.0
        } else {
          (point.value - mean).abs() / std_dev
        }
        
        results = results.push({
          timestamp: point.timestamp,
          value: point.value,
          expected_range: (expected_min, expected_max),
          is_anomaly: is_anomaly,
          anomaly_score: anomaly_score
        })
      } else {
        // Not enough data for detection
        results = results.push({
          timestamp: point.timestamp,
          value: point.value,
          expected_range: (0.0, 0.0),
          is_anomaly: false,
          anomaly_score: 0.0
        })
      }
    }
    
    results
  }
  
  let detector = { threshold_multiplier: 2.0, window_size: 5 }
  let anomaly_results = detect_anomalies(metrics_data, detector)
  
  // Find anomalies
  let anomalies = anomaly_results.filter(fn(r) { r.is_anomaly })
  assert_true(anomalies.length() >= 2) // Should detect at least the two obvious anomalies
  
  // Check first anomaly (value 78.9)
  let first_anomaly = anomalies.find(fn(a) { a.value == 78.9 })
  match first_anomaly {
    Some(anomaly) => {
      assert_true(anomaly.is_anomaly)
      assert_true(anomaly.anomaly_score > 2.0)
      assert_true(anomaly.value > anomaly.expected_range.1)
    }
    None => assert_true(false)
  }
  
  // Check second anomaly (value 12.1)
  let second_anomaly = anomalies.find(fn(a) { a.value == 12.1 })
  match second_anomaly {
    Some(anomaly) => {
      assert_true(anomaly.is_anomaly)
      assert_true(anomaly.anomaly_score > 2.0)
      assert_true(anomaly.value < anomaly.expected_range.0)
    }
    None => assert_true(false)
  }
  
  // Verify normal values are not flagged as anomalies
  let normal_values = anomaly_results.filter(fn(r) { not(r.is_anomaly) })
  assert_true(normal_values.length() > 0)
  
  // Check a normal value
  let normal_value = normal_values.find(fn(r) { r.value == 47.8 })
  match normal_value {
    Some(normal) => {
      assert_false(normal.is_anomaly)
      assert_true(normal.anomaly_score < 2.0)
      assert_true(normal.value >= normal.expected_range.0)
      assert_true(normal.value <= normal.expected_range.1)
    }
    None => assert_true(false)
  }
}