// Intelligent Alerting Thresholds Test Suite
// Test cases for adaptive and intelligent alerting mechanisms

test "dynamic threshold adjustment based on historical patterns" {
  // Test dynamic threshold adjustment using historical patterns
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dynamic-threshold-test")
  
  let error_rate_histogram = Meter::create_histogram(meter, "error.rate", Some("Error rate"), Some("percent"))
  let alert_counter = Meter::create_counter(meter, "alerts.triggered", Some("Alerts triggered"), Some("alerts"))
  
  // Simulate historical error rate data with patterns
  let historical_error_rates = [
    // Week 1: Normal error rates
    (1, 1.5), (2, 1.2), (3, 1.8), (4, 1.3), (5, 1.6), (6, 2.1), (7, 1.4),
    // Week 2: Normal error rates
    (8, 1.7), (9, 1.3), (10, 1.9), (11, 1.5), (12, 1.8), (13, 2.3), (14, 1.6),
    // Week 3: Slightly elevated error rates (deployment)
    (15, 3.2), (16, 3.8), (17, 4.1), (18, 3.5), (19, 2.9), (20, 2.4), (21, 2.1),
    // Week 4: Back to normal
    (22, 1.8), (23, 1.4), (24, 1.9), (25, 1.6), (26, 1.7), (27, 2.2), (28, 1.5)
  ]
  
  // Calculate baseline statistics from historical data
  let mut sum = 0.0
  let mut sum_squares = 0.0
  for (day, error_rate) in historical_error_rates {
    sum = sum + error_rate
    sum_squares = sum_squares + error_rate * error_rate
    Histogram::record(error_rate_histogram, error_rate)
  }
  
  let mean = sum / Double::from_int(historical_error_rates.length())
  let variance = sum_squares / Double::from_int(historical_error_rates.length()) - mean * mean
  let std_dev = Double::sqrt(variance)
  
  // Dynamic threshold: mean + 2 * std_dev
  let dynamic_threshold = mean + 2.0 * std_dev
  
  // Simulate current error rates with potential alert conditions
  let current_error_rates = [
    (29, 1.9),   // Normal
    (30, 2.1),   // Normal
    (31, 2.4),   // Normal
    (32, 3.1),   // Elevated but normal
    (33, 4.2),   // Alert threshold
    (34, 5.8),   // Alert threshold
    (35, 4.9),   // Alert threshold
    (36, 2.8)    // Recovering
  ]
  
  let mut alerts_triggered = 0.0
  
  for (day, error_rate) in current_error_rates {
    Histogram::record(error_rate_histogram, error_rate)
    
    // Check if error rate exceeds dynamic threshold
    if error_rate > dynamic_threshold {
      Counter::add(alert_counter, 1.0)
      alerts_triggered = alerts_triggered + 1.0
    }
  }
  
  // Verify histogram and counter properties
  assert_eq(error_rate_histogram.name, "error.rate")
  assert_eq(alert_counter.name, "alerts.triggered")
  
  // Verify dynamic threshold calculation
  assert_true(mean > 1.0 && mean < 3.0)
  assert_true(std_dev > 0.5 && std_dev < 1.5)
  assert_true(dynamic_threshold > mean)
  
  // Verify alert triggering
  assert_true(alerts_triggered == 3.0)  // Should trigger for days 33, 34, 35
}

test "multi-metric correlation alerting" {
  // Test alerting based on correlation between multiple metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "correlation-alert-test")
  
  let cpu_histogram = Meter::create_histogram(meter, "cpu.usage", Some("CPU usage"), Some("percent"))
  let memory_histogram = Meter::create_histogram(meter, "memory.usage", Some("Memory usage"), Some("percent"))
  let latency_histogram = Meter::create_histogram(meter, "response.latency", Some("Response latency"), Some("ms"))
  let correlation_alert_counter = Meter::create_counter(meter, "correlation.alerts", Some("Correlation alerts"), Some("alerts"))
  
  // Simulate correlated metrics with normal and anomalous patterns
  let correlated_metrics = [
    // Normal patterns: CPU and Memory increase together, latency remains stable
    (30.0, 40.0, 50.0),   // Normal
    (35.0, 45.0, 52.0),   // Normal
    (40.0, 50.0, 51.0),   // Normal
    (45.0, 55.0, 53.0),   // Normal
    (50.0, 60.0, 54.0),   // Normal
    // Anomalous pattern 1: High CPU, normal memory, high latency
    (85.0, 55.0, 120.0),  // Alert: CPU high + latency high
    (90.0, 58.0, 150.0),  // Alert: CPU high + latency high
    // Normal pattern resumes
    (45.0, 52.0, 55.0),   // Normal
    (40.0, 48.0, 52.0),   // Normal
    // Anomalous pattern 2: Normal CPU, high memory, high latency
    (42.0, 85.0, 110.0),  // Alert: Memory high + latency high
    (45.0, 90.0, 140.0),  // Alert: Memory high + latency high
    // Anomalous pattern 3: High CPU, high memory, normal latency
    (80.0, 82.0, 60.0),   // Warning: CPU high + Memory high
    (85.0, 88.0, 65.0),   // Warning: CPU high + Memory high
    // Anomalous pattern 4: All metrics high
    (92.0, 94.0, 180.0),  // Critical: All metrics high
    (95.0, 96.0, 220.0)   // Critical: All metrics high
  ]
  
  let mut correlation_alerts = 0.0
  
  for (cpu, memory, latency) in correlated_metrics {
    Histogram::record(cpu_histogram, cpu)
    Histogram::record(memory_histogram, memory)
    Histogram::record(latency_histogram, latency)
    
    // Multi-metric correlation alerting rules
    let cpu_high = cpu > 80.0
    let memory_high = memory > 80.0
    let latency_high = latency > 100.0
    
    // Alert if multiple metrics are high simultaneously
    if (cpu_high && latency_high) || (memory_high && latency_high) || (cpu_high && memory_high && latency_high) {
      Counter::add(correlation_alert_counter, 1.0)
      correlation_alerts = correlation_alerts + 1.0
    }
  }
  
  // Verify histogram and counter properties
  assert_eq(cpu_histogram.name, "cpu.usage")
  assert_eq(memory_histogram.name, "memory.usage")
  assert_eq(latency_histogram.name, "response.latency")
  assert_eq(correlation_alert_counter.name, "correlation.alerts")
  
  // Verify correlation alerting
  assert_true(correlation_alerts == 7.0)  // Should trigger for 7 anomalous patterns
}

test "context-aware alerting with business hours" {
  // Test context-aware alerting that considers business hours
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "context-aware-test")
  
  let response_time_histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  let business_hours_alert_counter = Meter::create_counter(meter, "business.hours.alerts", Some("Business hours alerts"), Some("alerts"))
  let after_hours_alert_counter = Meter::create_counter(meter, "after.hours.alerts", Some("After hours alerts"), Some("alerts"))
  
  // Simulate response times throughout the day with different thresholds
  let hourly_response_times = [
    // Night hours (0-6): Higher tolerance
    (0, 200.0), (1, 210.0), (2, 220.0), (3, 230.0), (4, 215.0), (5, 205.0),
    // Early morning (6-9): Medium tolerance
    (6, 150.0), (7, 160.0), (8, 170.0),
    // Business hours (9-17): Lower tolerance
    (9, 120.0), (10, 125.0), (11, 130.0), (12, 135.0), (13, 140.0), (14, 145.0),
    (15, 150.0), (16, 155.0), (17, 160.0),
    // Evening (17-22): Medium tolerance
    (18, 180.0), (19, 185.0), (20, 190.0), (21, 195.0),
    // Late night (22-24): Higher tolerance
    (22, 200.0), (23, 210.0)
  ]
  
  // Define thresholds based on time context
  let get_threshold = fn(hour) {
    if hour >= 0 && hour < 6 {
      250.0  // High tolerance during night
    } else if hour >= 6 && hour < 9 {
      180.0  // Medium tolerance in early morning
    } else if hour >= 9 && hour <= 17 {
      140.0  // Low tolerance during business hours
    } else if hour > 17 && hour <= 22 {
      200.0  // Medium tolerance in evening
    } else {
      250.0  // High tolerance during late night
    }
  }
  
  let mut business_hours_alerts = 0.0
  let mut after_hours_alerts = 0.0
  
  for (hour, response_time) in hourly_response_times {
    Histogram::record(response_time_histogram, response_time)
    
    let threshold = get_threshold(hour)
    let is_business_hours = hour >= 9 && hour <= 17
    
    // Check if response time exceeds threshold
    if response_time > threshold {
      if is_business_hours {
        Counter::add(business_hours_alert_counter, 1.0)
        business_hours_alerts = business_hours_alerts + 1.0
      } else {
        Counter::add(after_hours_alert_counter, 1.0)
        after_hours_alerts = after_hours_alerts + 1.0
      }
    }
  }
  
  // Verify histogram and counter properties
  assert_eq(response_time_histogram.name, "response.time")
  assert_eq(business_hours_alert_counter.name, "business.hours.alerts")
  assert_eq(after_hours_alert_counter.name, "after.hours.alerts")
  
  // Verify context-aware alerting
  assert_true(business_hours_alerts == 4.0)  // Hours 14, 15, 16, 17 exceed business threshold
  assert_true(after_hours_alerts == 0.0)     // No after-hours alerts
}

test "predictive alerting with trend analysis" {
  // Test predictive alerting based on trend analysis
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "predictive-alert-test")
  
  let disk_usage_histogram = Meter::create_histogram(meter, "disk.usage", Some("Disk usage"), Some("percent"))
  let predictive_alert_counter = Meter::create_counter(meter, "predictive.alerts", Some("Predictive alerts"), Some("alerts"))
  
  // Simulate disk usage over time with increasing trend
  let disk_usage_over_time = [
    (1, 45.0),   // Day 1
    (2, 46.2),   // Day 2
    (3, 47.5),   // Day 3
    (4, 48.8),   // Day 4
    (5, 50.1),   // Day 5
    (6, 51.4),   // Day 6
    (7, 52.7),   // Day 7
    (8, 54.0),   // Day 8
    (9, 55.3),   // Day 9
    (10, 56.6),  // Day 10
    (11, 57.9),  // Day 11
    (12, 59.2),  // Day 12
    (13, 60.5),  // Day 13
    (14, 61.8),  // Day 14
    (15, 63.1),  // Day 15
    (16, 64.4),  // Day 16
    (17, 65.7),  // Day 17
    (18, 67.0),  // Day 18
    (19, 68.3),  // Day 19
    (20, 69.6),  // Day 20
    (21, 70.9),  // Day 21
    (22, 72.2),  // Day 22
    (23, 73.5),  // Day 23
    (24, 74.8),  // Day 24
    (25, 76.1),  // Day 25
    (26, 77.4),  // Day 26
    (27, 78.7),  // Day 27
    (28, 80.0),  // Day 28
    (29, 81.3),  // Day 29
    (30, 82.6)   // Day 30
  ]
  
  let mut predictive_alerts = 0.0
  
  // Analyze trends and predict future values
  for i in 5..<disk_usage_over_time.length() {
    let (current_day, current_usage) = disk_usage_over_time[i]
    
    Histogram::record(disk_usage_histogram, current_usage)
    
    // Calculate trend based on last 5 days
    let mut trend_sum = 0.0
    for j in 0..<5 {
      let (day1, usage1) = disk_usage_over_time[i - j]
      let (day2, usage2) = disk_usage_over_time[i - j - 1]
      trend_sum = trend_sum + (usage1 - usage2)
    }
    let avg_daily_increase = trend_sum / 5.0
    
    // Predict disk usage 7 days in the future
    let predicted_usage_7_days = current_usage + (avg_daily_increase * 7.0)
    
    // Alert if predicted usage exceeds threshold
    if predicted_usage_7_days > 85.0 {
      Counter::add(predictive_alert_counter, 1.0)
      predictive_alerts = predictive_alerts + 1.0
    }
  }
  
  // Verify histogram and counter properties
  assert_eq(disk_usage_histogram.name, "disk.usage")
  assert_eq(predictive_alert_counter.name, "predictive.alerts")
  
  // Verify predictive alerting
  assert_true(predictive_alerts > 0.0)  // Should trigger alerts as disk usage trends upward
}

test "alert fatigue prevention with intelligent grouping" {
  // Test alert fatigue prevention through intelligent alert grouping
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "alert-fatigue-test")
  
  let error_counter = Meter::create_counter(meter, "errors", Some("Error count"), Some("errors"))
  let grouped_alert_counter = Meter::create_counter(meter, "grouped.alerts", Some("Grouped alerts"), Some("alerts"))
  let suppressed_alert_counter = Meter::create_counter(meter, "suppressed.alerts", Some("Suppressed alerts"), Some("alerts"))
  
  // Simulate error events that could cause alert fatigue
  let error_events = [
    // Service A errors (similar type, should be grouped)
    ("service-a", "timeout", "db-connection-failed", 1),
    ("service-a", "timeout", "db-connection-failed", 2),
    ("service-a", "timeout", "db-connection-failed", 3),
    ("service-a", "timeout", "db-connection-failed", 4),
    ("service-a", "timeout", "db-connection-failed", 5),
    ("service-a", "timeout", "db-connection-failed", 6),
    ("service-a", "timeout", "db-connection-failed", 7),
    ("service-a", "timeout", "db-connection-failed", 8),
    ("service-a", "timeout", "db-connection-failed", 9),
    ("service-a", "timeout", "db-connection-failed", 10),
    // Service B errors (different type, separate alert)
    ("service-b", "connection_error", "cache-unreachable", 11),
    ("service-b", "connection_error", "cache-unreachable", 12),
    ("service-b", "connection_error", "cache-unreachable", 13),
    // Service A different error (new group)
    ("service-a", "validation_error", "invalid-input", 14),
    ("service-a", "validation_error", "invalid-input", 15),
    ("service-a", "validation_error", "invalid-input", 16),
    // Service C errors (new group)
    ("service-c", "auth_error", "token-expired", 17),
    ("service-c", "auth_error", "token-expired", 18),
    // Service A original error continues (should be suppressed)
    ("service-a", "timeout", "db-connection-failed", 19),
    ("service-a", "timeout", "db-connection-failed", 20)
  ]
  
  // Alert grouping logic
  let mut alert_groups = []
  let mut grouped_alerts = 0.0
  let mut suppressed_alerts = 0.0
  
  for (service, error_type, error_detail, timestamp) in error_events {
    Counter::add(error_counter, 1.0)
    
    // Create alert signature for grouping
    let alert_signature = service + ":" + error_type + ":" + error_detail
    
    // Check if this alert group exists and is recent
    let mut existing_group = false
    let mut recent_group = false
    
    for (signature, last_timestamp, count) in alert_groups {
      if signature == alert_signature {
        existing_group = true
        // If last alert was less than 5 time units ago, group it
        if timestamp - last_timestamp < 5 {
          recent_group = true
        }
      }
    }
    
    if existing_group && recent_group {
      // Suppress this alert as it's part of a recent group
      Counter::add(suppressed_alert_counter, 1.0)
      suppressed_alerts = suppressed_alerts + 1.0
      
      // Update the group timestamp
      alert_groups = alert_groups.map(fn((signature, last_timestamp, count)) {
        if signature == alert_signature {
          (signature, timestamp, count + 1)
        } else {
          (signature, last_timestamp, count)
        }
      })
    } else {
      // Create new alert or new group
      Counter::add(grouped_alert_counter, 1.0)
      grouped_alerts = grouped_alerts + 1.0
      
      if existing_group {
        // Update existing group
        alert_groups = alert_groups.map(fn((signature, last_timestamp, count)) {
          if signature == alert_signature {
            (signature, timestamp, count + 1)
          } else {
            (signature, last_timestamp, count)
          }
        })
      } else {
        // Create new group
        alert_groups = alert_groups.push((alert_signature, timestamp, 1))
      }
    }
  }
  
  // Verify counter properties
  assert_eq(error_counter.name, "errors")
  assert_eq(grouped_alert_counter.name, "grouped.alerts")
  assert_eq(suppressed_alert_counter.name, "suppressed.alerts")
  
  // Verify alert grouping
  assert_true(grouped_alerts == 5.0)   // 5 unique alert groups
  assert_true(suppressed_alerts == 15.0)  // 15 suppressed alerts
  assert_true(grouped_alerts + suppressed_alerts == 20.0)  // Total events
}