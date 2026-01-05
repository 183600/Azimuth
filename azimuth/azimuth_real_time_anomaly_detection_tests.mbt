// Real-time Anomaly Detection Test Suite
// Test cases for anomaly detection algorithms and pattern recognition

test "statistical anomaly detection with standard deviation" {
  // Test anomaly detection using statistical methods (standard deviation)
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "anomaly-detection-test")
  
  let response_time_histogram = Meter::create_histogram(meter, "response.time", Some("Response time distribution"), Some("ms"))
  let anomaly_counter = Meter::create_counter(meter, "anomalies.detected", Some("Anomalies detected"), Some("anomalies"))
  
  // Simulate normal response times
  let normal_response_times = [
    100.0, 105.0, 110.0, 95.0, 120.0, 108.0, 115.0, 98.0, 102.0, 112.0,
    106.0, 99.0, 118.0, 103.0, 109.0, 97.0, 111.0, 104.0, 107.0, 96.0,
    113.0, 101.0, 108.0, 94.0, 116.0, 100.0, 105.0, 110.0, 95.0, 120.0
  ]
  
  // Calculate statistics for normal data
  let mut sum = 0.0
  for time in normal_response_times {
    sum = sum + time
    Histogram::record(response_time_histogram, time)
  }
  let mean = sum / Double::from_int(normal_response_times.length())
  
  // Calculate standard deviation
  let mut variance_sum = 0.0
  for time in normal_response_times {
    let diff = time - mean
    variance_sum = variance_sum + diff * diff
  }
  let variance = variance_sum / Double::from_int(normal_response_times.length())
  let std_dev = Double::sqrt(variance)
  
  // Simulate anomalous response times (beyond 3 standard deviations)
  let anomalous_response_times = [
    500.0,  600.0,  800.0,  1000.0,  1500.0
  ]
  
  let mut anomalies_detected = 0.0
  
  for time in anomalous_response_times {
    Histogram::record(response_time_histogram, time)
    
    // Check if value is beyond 3 standard deviations
    let z_score = (time - mean) / std_dev
    if Double::abs(z_score) > 3.0 {
      Counter::add(anomaly_counter, 1.0)
      anomalies_detected = anomalies_detected + 1.0
    }
  }
  
  // Verify histogram and counter properties
  assert_eq(response_time_histogram.name, "response.time")
  assert_eq(anomaly_counter.name, "anomalies.detected")
  
  // Verify anomaly detection
  assert_true(mean > 90.0 && mean < 120.0)
  assert_true(std_dev > 5.0 && std_dev < 15.0)
  assert_true(anomalies_detected == 5.0)
  assert_true(anomalies_detected == Double::from_int(anomalous_response_times.length()))
}

test "machine learning based anomaly detection" {
  // Test ML-based anomaly detection with pattern recognition
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "ml-anomaly-test")
  
  let pattern_counter = Meter::create_counter(meter, "pattern.matches", Some("Pattern matches"), Some("matches"))
  let ml_anomaly_counter = Meter::create_counter(meter, "ml.anomalies", Some("ML detected anomalies"), Some("anomalies"))
  
  // Simulate time series data with patterns
  let time_series_patterns = [
    // Normal pattern: regular heartbeat every 60 seconds
    (1, "heartbeat", 60),
    (2, "heartbeat", 60),
    (3, "heartbeat", 60),
    (4, "heartbeat", 60),
    (5, "heartbeat", 60),
    // Anomalous pattern: irregular heartbeat
    (6, "heartbeat", 120),  // Delayed heartbeat
    (7, "heartbeat", 30),   // Early heartbeat
    (8, "heartbeat", 180),  // Very delayed heartbeat
    // Normal pattern resumes
    (9, "heartbeat", 60),
    (10, "heartbeat", 60),
    // Anomalous pattern: missing heartbeat
    (11, "missing", 0),
    (12, "heartbeat", 60),
    (13, "heartbeat", 60)
  ]
  
  let mut pattern_matches = 0.0
  let mut ml_anomalies = 0.0
  
  for (timestamp, pattern_type, interval) in time_series_patterns {
    // In a real ML system, we would analyze the pattern
    // For this test, we simulate the detection logic
    
    if pattern_type == "heartbeat" && interval == 60 {
      Counter::add(pattern_counter, 1.0)
      pattern_matches = pattern_matches + 1.0
    } else if pattern_type == "missing" || interval > 90 || interval < 45 {
      Counter::add(ml_anomaly_counter, 1.0)
      ml_anomalies = ml_anomalies + 1.0
    }
  }
  
  // Verify counter properties
  assert_eq(pattern_counter.name, "pattern.matches")
  assert_eq(ml_anomaly_counter.name, "ml.anomalies")
  
  // Verify ML anomaly detection
  assert_true(pattern_matches == 8.0)
  assert_true(ml_anomalies == 5.0)
  assert_true(pattern_matches + ml_anomalies == 13.0)
}

test "seasonal anomaly detection with time series analysis" {
  // Test seasonal anomaly detection considering time-based patterns
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "seasonal-anomaly-test")
  
  let traffic_histogram = Meter::create_histogram(meter, "hourly.traffic", Some("Hourly traffic patterns"), Some("requests"))
  let seasonal_anomaly_counter = Meter::create_counter(meter, "seasonal.anomalies", Some("Seasonal anomalies"), Some("anomalies"))
  
  // Simulate hourly traffic patterns over a week
  // Normal pattern: higher traffic during business hours, lower at night
  let hourly_traffic_patterns = [
    // Weekday patterns (Monday-Friday)
    (100, 50),   // 1 AM - low traffic
    (200, 75),   // 2 AM - low traffic
    (300, 100),  // 3 AM - low traffic
    (400, 150),  // 4 AM - low traffic
    (500, 200),  // 5 AM - moderate traffic
    (600, 400),  // 6 AM - moderate traffic
    (700, 800),  // 7 AM - high traffic (morning rush)
    (800, 1200), // 8 AM - high traffic (morning rush)
    (900, 1000), // 9 AM - high traffic
    (1000, 900), // 10 AM - moderate-high traffic
    (1100, 850), // 11 AM - moderate-high traffic
    (1200, 1100), // 12 PM - high traffic (lunch)
    (1300, 950),  // 1 PM - moderate-high traffic
    (1400, 900),  // 2 PM - moderate-high traffic
    (1500, 950),  // 3 PM - moderate-high traffic
    (1600, 1000), // 4 PM - high traffic (evening rush)
    (1700, 1200), // 5 PM - high traffic (evening rush)
    (1800, 800),  // 6 PM - moderate traffic
    (1900, 600),  // 7 PM - moderate traffic
    (2000, 400),  // 8 PM - low-moderate traffic
    (2100, 300),  // 9 PM - low traffic
    (2200, 200),  // 10 PM - low traffic
    (2300, 100),  // 11 PM - low traffic
    (0, 50),      // 12 AM - low traffic
    // Weekend patterns (Saturday-Sunday)
    (100, 200),   // 1 AM Saturday - slightly higher than weekday
    (200, 300),   // 2 AM Saturday
    (300, 400),   // 3 AM Saturday
    (400, 500),   // 4 AM Saturday
    (500, 600),   // 5 AM Saturday
    (600, 700),   // 6 AM Saturday
    (700, 900),   // 7 AM Saturday
    (800, 1100),  // 8 AM Saturday
    (900, 1300),  // 9 AM Saturday - weekend peak
    (1000, 1400), // 10 AM Saturday - weekend peak
    (1100, 1500), // 11 AM Saturday - weekend peak
    (1200, 1400), // 12 PM Saturday
    (1300, 1300), // 1 PM Saturday
    (1400, 1200), // 2 PM Saturday
    (1500, 1100), // 3 PM Saturday
    (1600, 1000), // 4 PM Saturday
    (1700, 900),  // 5 PM Saturday
    (1800, 800),  // 6 PM Saturday
    (1900, 700),  // 7 PM Saturday
    (2000, 600),  // 8 PM Saturday
    (2100, 500),  // 9 PM Saturday
    (2200, 400),  // 10 PM Saturday
    (2300, 300),  // 11 PM Saturday
    (0, 200),     // 12 AM Saturday
    // Anomalous patterns
    (200, 2000),  // 2 AM with unusually high traffic
    (1400, 50),   // 2 PM with unusually low traffic
    (800, 3000),  // 8 AM with extreme traffic
    (1800, 100)   // 6 PM with very low traffic
  ]
  
  let mut seasonal_anomalies = 0.0
  
  for (hour, traffic) in hourly_traffic_patterns {
    Histogram::record(traffic_histogram, Double::from_int(traffic))
    
    // Detect anomalies based on expected patterns
    let is_weekend = hour >= 100  // Simplified weekend detection
    let is_business_hours = hour >= 700 && hour <= 1700 && hour < 100
    let is_night = hour < 600 || hour >= 2200
    
    let expected_min = if is_weekend && is_business_hours {
      900  // Higher baseline for weekend business hours
    } else if is_business_hours {
      800  // Weekday business hours
    } else if is_night {
      50   // Night time
    } else {
      200  // Other times
    }
    
    let expected_max = if is_weekend && is_business_hours {
      1500  // Higher ceiling for weekend business hours
    } else if is_business_hours {
      1200  // Weekday business hours
    } else if is_night {
      200   // Night time
    } else {
      600   // Other times
    }
    
    // Detect anomaly if traffic is outside expected range
    if traffic < expected_min || traffic > expected_max {
      Counter::add(seasonal_anomaly_counter, 1.0)
      seasonal_anomalies = seasonal_anomalies + 1.0
    }
  }
  
  // Verify histogram and counter properties
  assert_eq(traffic_histogram.name, "hourly.traffic")
  assert_eq(seasonal_anomaly_counter.name, "seasonal.anomalies")
  
  // Verify seasonal anomaly detection
  assert_true(seasonal_anomalies == 4.0)  // Should detect the 4 anomalous patterns
}

test "multivariate anomaly detection with correlation analysis" {
  // Test multivariate anomaly detection using correlation analysis
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multivariate-anomaly-test")
  
  let cpu_histogram = Meter::create_histogram(meter, "cpu.usage", Some("CPU usage"), Some("percent"))
  let memory_histogram = Meter::create_histogram(meter, "memory.usage", Some("Memory usage"), Some("percent"))
  let multivariate_anomaly_counter = Meter::create_counter(meter, "multivariate.anomalies", Some("Multivariate anomalies"), Some("anomalies"))
  
  // Simulate correlated metrics (CPU and Memory usage)
  let correlated_metrics = [
    // Normal correlation: CPU and Memory increase together
    (20.0, 30.0),   // Low CPU, Low Memory
    (25.0, 35.0),   // Low CPU, Low Memory
    (30.0, 40.0),   // Low-Medium CPU, Low-Medium Memory
    (35.0, 45.0),   // Low-Medium CPU, Low-Medium Memory
    (40.0, 50.0),   // Medium CPU, Medium Memory
    (45.0, 55.0),   // Medium CPU, Medium Memory
    (50.0, 60.0),   // Medium CPU, Medium Memory
    (55.0, 65.0),   // Medium-High CPU, Medium-High Memory
    (60.0, 70.0),   // Medium-High CPU, Medium-High Memory
    (65.0, 75.0),   // Medium-High CPU, Medium-High Memory
    (70.0, 80.0),   // High CPU, High Memory
    (75.0, 85.0),   // High CPU, High Memory
    // Anomalous patterns: unexpected correlation
    (90.0, 20.0),   // High CPU, Low Memory - anomaly
    (15.0, 95.0),   // Low CPU, High Memory - anomaly
    (95.0, 25.0),   // Very High CPU, Low Memory - anomaly
    (25.0, 90.0),   // Low CPU, Very High Memory - anomaly
    // Normal correlation resumes
    (50.0, 60.0),   // Medium CPU, Medium Memory
    (55.0, 65.0),   // Medium-High CPU, Medium-High Memory
  ]
  
  let mut multivariate_anomalies = 0.0
  
  for (cpu_usage, memory_usage) in correlated_metrics {
    Histogram::record(cpu_histogram, cpu_usage)
    Histogram::record(memory_histogram, memory_usage)
    
    // Detect anomalies based on correlation patterns
    // Normal pattern: memory_usage ≈ cpu_usage + 10
    let expected_memory = cpu_usage + 10.0
    let memory_deviation = Double::abs(memory_usage - expected_memory)
    
    // Anomaly if deviation is too large
    if memory_deviation > 30.0 {
      Counter::add(multivariate_anomaly_counter, 1.0)
      multivariate_anomalies = multivariate_anomalies + 1.0
    }
  }
  
  // Verify histogram and counter properties
  assert_eq(cpu_histogram.name, "cpu.usage")
  assert_eq(memory_histogram.name, "memory.usage")
  assert_eq(multivariate_anomaly_counter.name, "multivariate.anomalies")
  
  // Verify multivariate anomaly detection
  assert_true(multivariate_anomalies == 4.0)  // Should detect the 4 anomalous correlations
}

test "real-time streaming anomaly detection with sliding window" {
  // Test real-time anomaly detection with sliding window algorithm
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "streaming-anomaly-test")
  
  let request_rate_histogram = Meter::create_histogram(meter, "request.rate", Some("Request rate"), Some("req/s"))
  let streaming_anomaly_counter = Meter::create_counter(meter, "streaming.anomalies", Some("Streaming anomalies"), Some("anomalies"))
  
  // Simulate streaming request rate data
  let request_rate_stream = [
    // Normal stream: steady request rate
    100.0, 105.0, 98.0, 102.0, 110.0, 95.0, 108.0, 103.0, 97.0, 106.0,
    // Gradual increase (normal)
    110.0, 115.0, 120.0, 125.0, 130.0, 135.0, 140.0, 145.0, 150.0,
    // Sudden spike (anomaly)
    500.0, 600.0, 800.0,
    // Gradual decrease (normal)
    140.0, 130.0, 120.0, 110.0, 100.0, 95.0, 90.0, 85.0, 80.0,
    // Sudden drop (anomaly)
    10.0, 5.0, 2.0,
    // Recovery to normal
    75.0, 80.0, 85.0, 90.0, 95.0, 100.0, 105.0, 110.0, 115.0
  ]
  
  let window_size = 10
  let mut sliding_window = []
  let mut streaming_anomalies = 0.0
  
  for rate in request_rate_stream {
    Histogram::record(request_rate_histogram, rate)
    
    // Add to sliding window
    sliding_window = sliding_window.push(rate)
    
    // Maintain window size
    if sliding_window.length() > window_size {
      sliding_window = sliding_window.slice(1, sliding_window.length())
    }
    
    // Only detect anomalies when window is full
    if sliding_window.length() == window_size {
      // Calculate window statistics
      let mut window_sum = 0.0
      for value in sliding_window {
        window_sum = window_sum + value
      }
      let window_mean = window_sum / Double::from_int(window_size)
      
      // Calculate window standard deviation
      let mut variance_sum = 0.0
      for value in sliding_window {
        let diff = value - window_mean
        variance_sum = variance_sum + diff * diff
      }
      let window_variance = variance_sum / Double::from_int(window_size)
      let window_std_dev = Double::sqrt(window_variance)
      
      // Check if current value is anomalous (beyond 2 standard deviations)
      let z_score = (rate - window_mean) / window_std_dev
      if Double::abs(z_score) > 2.0 {
        Counter::add(streaming_anomaly_counter, 1.0)
        streaming_anomalies = streaming_anomalies + 1.0
      }
    }
  }
  
  // Verify histogram and counter properties
  assert_eq(request_rate_histogram.name, "request.rate")
  assert_eq(streaming_anomaly_counter.name, "streaming.anomalies")
  
  // Verify streaming anomaly detection
  assert_true(streaming_anomalies > 0.0)
  assert_true(streaming_anomalies <= 6.0)  // Should detect some but not all anomalies due to windowing
}