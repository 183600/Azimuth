// Azimuth Time Series Data Processing Tests
// This file contains test cases for time series data processing

// Test 1: Basic Time Series Data Structure
test "basic time series data structure" {
  // Define time series point structure
  let create_time_series_point = fn(timestamp, value, tags) {
    { timestamp = timestamp, value = value, tags = tags }
  }
  
  // Define time series structure
  let create_time_series = fn(name, points) {
    { name = name, points = points, metadata = {} }
  }
  
  // Test creating time series points
  let point1 = create_time_series_point(1640995200000, 42.5, { "service": "api", "env": "prod" })
  let point2 = create_time_series_point(1640995260000, 38.2, { "service": "api", "env": "prod" })
  let point3 = create_time_series_point(1640995320000, 45.1, { "service": "api", "env": "prod" })
  
  assert_eq(point1.timestamp, 1640995200000)
  assert_eq(point1.value, 42.5)
  assert_eq(point1.tags["service"], "api")
  assert_eq(point1.tags["env"], "prod")
  
  // Test creating time series
  let time_series = create_time_series("response_time_ms", [point1, point2, point3])
  assert_eq(time_series.name, "response_time_ms")
  assert_eq(time_series.points.length(), 3)
  assert_eq(time_series.points[0].value, 42.5)
  assert_eq(time_series.points[1].value, 38.2)
  assert_eq(time_series.points[2].value, 45.1)
  
  // Test empty time series
  let empty_time_series = create_time_series("empty_metric", [])
  assert_eq(empty_time_series.name, "empty_metric")
  assert_eq(empty_time_series.points.length(), 0)
}

// Test 2: Time Series Aggregation
test "time series aggregation" {
  // Create test time series points
  let create_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = {} },
      { timestamp = 1640995260000, value = 20.0, tags = {} },
      { timestamp = 1640995320000, value = 30.0, tags = {} },
      { timestamp = 1640995380000, value = 40.0, tags = {} },
      { timestamp = 1640995440000, value = 50.0, tags = {} }
    ]
  }
  
  let points = create_points()
  
  // Test sum aggregation
  let sum_aggregation = fn(points) {
    let mut sum = 0.0
    for point in points {
      sum = sum + point.value
    }
    sum
  }
  
  let sum_result = sum_aggregation(points)
  assert_eq(sum_result, 150.0)
  
  // Test average aggregation
  let avg_aggregation = fn(points) {
    if points.length() == 0 {
      0.0
    } else {
      sum_aggregation(points) / points.length().to_float()
    }
  }
  
  let avg_result = avg_aggregation(points)
  assert_eq(avg_result, 30.0)
  
  // Test min aggregation
  let min_aggregation = fn(points) {
    if points.length() == 0 {
      0.0
    } else {
      let mut min = points[0].value
      for point in points {
        if point.value < min {
          min = point.value
        }
      }
      min
    }
  }
  
  let min_result = min_aggregation(points)
  assert_eq(min_result, 10.0)
  
  // Test max aggregation
  let max_aggregation = fn(points) {
    if points.length() == 0 {
      0.0
    } else {
      let mut max = points[0].value
      for point in points {
        if point.value > max {
          max = point.value
        }
      }
      max
    }
  }
  
  let max_result = max_aggregation(points)
  assert_eq(max_result, 50.0)
  
  // Test count aggregation
  let count_aggregation = fn(points) {
    points.length()
  }
  
  let count_result = count_aggregation(points)
  assert_eq(count_result, 5)
  
  // Test empty points
  let empty_points = []
  assert_eq(sum_aggregation(empty_points), 0.0)
  assert_eq(avg_aggregation(empty_points), 0.0)
  assert_eq(min_aggregation(empty_points), 0.0)
  assert_eq(max_aggregation(empty_points), 0.0)
  assert_eq(count_aggregation(empty_points), 0)
}

// Test 3: Time Series Windowing
test "time series windowing" {
  // Create test time series points with 1-minute intervals
  let create_time_series_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = {} },  // 2022-01-01 00:00:00
      { timestamp = 1640995260000, value = 20.0, tags = {} },  // 2022-01-01 00:01:00
      { timestamp = 1640995320000, value = 30.0, tags = {} },  // 2022-01-01 00:02:00
      { timestamp = 1640995380000, value = 40.0, tags = {} },  // 2022-01-01 00:03:00
      { timestamp = 1640995440000, value = 50.0, tags = {} },  // 2022-01-01 00:04:00
      { timestamp = 1640995500000, value = 60.0, tags = {} },  // 2022-01-01 00:05:00
      { timestamp = 1640995560000, value = 70.0, tags = {} },  // 2022-01-01 00:06:00
      { timestamp = 1640995620000, value = 80.0, tags = {} },  // 2022-01-01 00:07:00
      { timestamp = 1640995680000, value = 90.0, tags = {} },  // 2022-01-01 00:08:00
      { timestamp = 1640995740000, value = 100.0, tags = {} }  // 2022-01-01 00:09:00
    ]
  }
  
  let points = create_time_series_points()
  
  // Test time windowing
  let time_window = fn(points, start_time, end_time) {
    let mut windowed_points = []
    for point in points {
      if point.timestamp >= start_time && point.timestamp < end_time {
        windowed_points = windowed_points.push(point)
      }
    }
    windowed_points
  }
  
  // Test 5-minute window
  let window1 = time_window(points, 1640995200000, 1640995500000)  // 00:00:00 to 00:05:00
  assert_eq(window1.length(), 5)
  assert_eq(window1[0].value, 10.0)  // 00:00:00
  assert_eq(window1[4].value, 50.0)  // 00:04:00
  
  // Test another 5-minute window
  let window2 = time_window(points, 1640995500000, 1640995800000)  // 00:05:00 to 00:10:00
  assert_eq(window2.length(), 5)
  assert_eq(window2[0].value, 60.0)  // 00:05:00
  assert_eq(window2[4].value, 100.0) // 00:09:00
  
  // Test 3-minute window
  let window3 = time_window(points, 1640995320000, 1640995500000)  // 00:02:00 to 00:05:00
  assert_eq(window3.length(), 3)
  assert_eq(window3[0].value, 30.0)  // 00:02:00
  assert_eq(window3[2].value, 50.0)  // 00:04:00
  
  // Test empty window
  let empty_window = time_window(points, 1640996000000, 1640996100000)  // 00:10:00 to 00:11:00
  assert_eq(empty_window.length(), 0)
  
  // Test sliding windows
  let sliding_windows = fn(points, window_size_ms, step_ms) {
    let mut windows = []
    if points.length() == 0 {
      return windows
    }
    
    let start_time = points[0].timestamp
    let end_time = points[points.length() - 1].timestamp
    
    let mut current_start = start_time
    while current_start + window_size_ms <= end_time {
      let window = time_window(points, current_start, current_start + window_size_ms)
      if window.length() > 0 {
        windows = windows.push(window)
      }
      current_start = current_start + step_ms
    }
    
    windows
  }
  
  // Test 3-minute sliding windows with 1-minute step
  let sliding = sliding_windows(points, 180000, 60000)  // 3-minute window, 1-minute step
  assert_eq(sliding.length(), 8)  // Should have 8 windows
  
  assert_eq(sliding[0].length(), 3)  // 00:00:00 to 00:03:00
  assert_eq(sliding[0][0].value, 10.0)
  assert_eq(sliding[0][2].value, 40.0)
  
  assert_eq(sliding[1].length(), 4)  // 00:01:00 to 00:04:00
  assert_eq(sliding[1][0].value, 20.0)
  assert_eq(sliding[1][3].value, 50.0)
  
  assert_eq(sliding[7].length(), 3)  // 00:07:00 to 00:10:00
  assert_eq(sliding[7][0].value, 80.0)
  assert_eq(sliding[7][2].value, 100.0)
}

// Test 4: Time Series Downsampling
test "time series downsampling" {
  // Create high-frequency time series points (every 10 seconds)
  let create_high_frequency_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = {} },  // 00:00:00
      { timestamp = 1640995210000, value = 12.0, tags = {} },  // 00:00:10
      { timestamp = 1640995220000, value = 14.0, tags = {} },  // 00:00:20
      { timestamp = 1640995230000, value = 16.0, tags = {} },  // 00:00:30
      { timestamp = 1640995240000, value = 18.0, tags = {} },  // 00:00:40
      { timestamp = 1640995250000, value = 20.0, tags = {} },  // 00:00:50
      { timestamp = 1640995260000, value = 22.0, tags = {} },  // 00:01:00
      { timestamp = 1640995270000, value = 24.0, tags = {} },  // 00:01:10
      { timestamp = 1640995280000, value = 26.0, tags = {} },  // 00:01:20
      { timestamp = 1640995290000, value = 28.0, tags = {} },  // 00:01:30
      { timestamp = 1640995300000, value = 30.0, tags = {} },  // 00:01:40
      { timestamp = 1640995310000, value = 32.0, tags = {} }   // 00:01:50
    ]
  }
  
  let high_freq_points = create_high_frequency_points()
  
  // Test downsampling with average aggregation
  let downsample = fn(points, interval_ms, aggregation_fn) {
    if points.length() == 0 {
      return []
    }
    
    let mut downsampled_points = []
    let mut current_window_start = points[0].timestamp
    let mut current_window_end = current_window_start + interval_ms
    let mut window_points = []
    
    for point in points {
      if point.timestamp >= current_window_start && point.timestamp < current_window_end {
        window_points = window_points.push(point)
      } else {
        // Process current window
        if window_points.length() > 0 {
          let aggregated_value = aggregation_fn(window_points)
          let window_timestamp = current_window_start + (interval_ms / 2)  // Center of window
          downsampled_points = downsampled_points.push({
            timestamp = window_timestamp,
            value = aggregated_value,
            tags = {}
          })
        }
        
        // Start new window
        current_window_start = current_window_end
        current_window_end = current_window_start + interval_ms
        window_points = [point]
      }
    }
    
    // Process last window
    if window_points.length() > 0 {
      let aggregated_value = aggregation_fn(window_points)
      let window_timestamp = current_window_start + (interval_ms / 2)
      downsampled_points = downsampled_points.push({
        timestamp = window_timestamp,
        value = aggregated_value,
        tags = {}
      })
    }
    
    downsampled_points
  }
  
  // Test average downsampling to 1-minute intervals
  let avg_downsampled = downsample(high_freq_points, 60000, avg_aggregation)
  assert_eq(avg_downsampled.length(), 2)
  
  assert_eq(avg_downsampled[0].timestamp, 1640995230000)  // 00:00:30 (center of first minute)
  assert_eq(avg_downsampled[0].value, 15.0)  // Average of 10, 12, 14, 16, 18, 20
  
  assert_eq(avg_downsampled[1].timestamp, 1640995290000)  // 00:01:30 (center of second minute)
  assert_eq(avg_downsampled[1].value, 27.0)  // Average of 22, 24, 26, 28, 30, 32
  
  // Test max downsampling to 1-minute intervals
  let max_downsampled = downsample(high_freq_points, 60000, max_aggregation)
  assert_eq(max_downsampled.length(), 2)
  
  assert_eq(max_downsampled[0].timestamp, 1640995230000)
  assert_eq(max_downsampled[0].value, 20.0)  // Max of first minute
  
  assert_eq(max_downsampled[1].timestamp, 1640995290000)
  assert_eq(max_downsampled[1].value, 32.0)  // Max of second minute
  
  // Test min downsampling to 1-minute intervals
  let min_downsampled = downsample(high_freq_points, 60000, min_aggregation)
  assert_eq(min_downsampled.length(), 2)
  
  assert_eq(min_downsampled[0].timestamp, 1640995230000)
  assert_eq(min_downsampled[0].value, 10.0)  // Min of first minute
  
  assert_eq(min_downsampled[1].timestamp, 1640995290000)
  assert_eq(min_downsampled[1].value, 22.0)  // Min of second minute
}

// Test 5: Time Series Interpolation
test "time series interpolation" {
  // Create time series with missing points
  let create_sparse_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = {} },  // 00:00:00
      { timestamp = 1640995300000, value = 20.0, tags = {} },  // 00:01:40
      { timestamp = 1640995400000, value = 30.0, tags = {} }   // 00:03:20
    ]
  }
  
  let sparse_points = create_sparse_points()
  
  // Test linear interpolation
  let linear_interpolate = fn(points, target_timestamp) {
    if points.length() < 2 {
      return None
    }
    
    // Find the points surrounding the target timestamp
    let mut before_point = None
    let mut after_point = None
    
    for point in points {
      if point.timestamp <= target_timestamp {
        before_point = Some(point)
      } else {
        after_point = Some(point)
        break
      }
    }
    
    match (before_point, after_point) {
      (Some(before), Some(after)) => {
        if before.timestamp == target_timestamp {
          Some(before.value)
        } else if after.timestamp == target_timestamp {
          Some(after.value)
        } else {
          let time_diff = after.timestamp - before.timestamp
          let target_offset = target_timestamp - before.timestamp
          let ratio = target_offset.to_float() / time_diff.to_float()
          Some(before.value + (after.value - before.value) * ratio)
        }
      }
      _ => None
    }
  }
  
  // Test interpolation at different points
  match linear_interpolate(sparse_points, 1640995250000) {  // 00:00:50
    None => assert_true(false)
    Some(value) => assert_eq(value, 15.0)  // 10 + (20-10) * (50/100)
  }
  
  match linear_interpolate(sparse_points, 1640995350000) {  // 00:02:30
    None => assert_true(false)
    Some(value) => assert_eq(value, 25.0)  // 20 + (30-20) * (50/100)
  }
  
  match linear_interpolate(sparse_points, 1640995200000) {  // Exactly at first point
    None => assert_true(false)
    Some(value) => assert_eq(value, 10.0)
  }
  
  match linear_interpolate(sparse_points, 1640995400000) {  // Exactly at last point
    None => assert_true(false)
    Some(value) => assert_eq(value, 30.0)
  }
  
  match linear_interpolate(sparse_points, 1640995150000) {  // Before first point
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  match linear_interpolate(sparse_points, 1640995450000) {  // After last point
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test regular time series generation with interpolation
  let generate_regular_series = fn(sparse_points, interval_ms) {
    if sparse_points.length() < 2 {
      return sparse_points
    }
    
    let start_time = sparse_points[0].timestamp
    let end_time = sparse_points[sparse_points.length() - 1].timestamp
    
    let mut regular_points = []
    let mut current_time = start_time
    
    while current_time <= end_time {
      match linear_interpolate(sparse_points, current_time) {
        None => {}  // Skip if can't interpolate
        Some(value) => {
          regular_points = regular_points.push({
            timestamp = current_time,
            value = value,
            tags = {}
          })
        }
      }
      current_time = current_time + interval_ms
    }
    
    regular_points
  }
  
  // Generate regular series with 30-second intervals
  let regular_series = generate_regular_series(sparse_points, 30000)
  assert_eq(regular_series.length(), 7)  // Should have 7 points
  
  assert_eq(regular_series[0].timestamp, 1640995200000)  // 00:00:00
  assert_eq(regular_series[0].value, 10.0)
  
  assert_eq(regular_series[1].timestamp, 1640995230000)  // 00:00:30
  assert_eq(regular_series[1].value, 15.0)
  
  assert_eq(regular_series[2].timestamp, 1640995260000)  // 00:01:00
  assert_eq(regular_series[2].value, 17.0)  // 10 + (20-10) * (60/100)
  
  assert_eq(regular_series[3].timestamp, 1640995290000)  // 00:01:30
  assert_eq(regular_series[3].value, 19.0)  // 10 + (20-10) * (90/100)
  
  assert_eq(regular_series[4].timestamp, 1640995320000)  // 00:02:00
  assert_eq(regular_series[4].value, 22.0)  // 20 + (30-20) * (20/100)
  
  assert_eq(regular_series[5].timestamp, 1640995350000)  // 00:02:30
  assert_eq(regular_series[5].value, 25.0)
  
  assert_eq(regular_series[6].timestamp, 1640995380000)  // 00:03:00
  assert_eq(regular_series[6].value, 28.0)  // 20 + (30-20) * (80/100)
}

// Test 6: Time Series Trend Analysis
test "time series trend analysis" {
  // Create time series with different trends
  let create_trend_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = {} },  // 00:00:00
      { timestamp = 1640995260000, value = 15.0, tags = {} },  // 00:01:00
      { timestamp = 1640995320000, value = 20.0, tags = {} },  // 00:02:00
      { timestamp = 1640995380000, value = 25.0, tags = {} },  // 00:03:00
      { timestamp = 1640995440000, value = 30.0, tags = {} },  // 00:04:00
      { timestamp = 1640995500000, value = 28.0, tags = {} },  // 00:05:00
      { timestamp = 1640995560000, value = 26.0, tags = {} },  // 00:06:00
      { timestamp = 1640995620000, value = 24.0, tags = {} },  // 00:07:00
      { timestamp = 1640995680000, value = 22.0, tags = {} },  // 00:08:00
      { timestamp = 1640995740000, value = 20.0, tags = {} }   // 00:09:00
    ]
  }
  
  let trend_points = create_trend_points()
  
  // Test simple trend analysis
  let calculate_trend = fn(points) {
    if points.length() < 2 {
      return { slope = 0.0, direction = "stable" }
    }
    
    let first_point = points[0]
    let last_point = points[points.length() - 1]
    
    let time_diff = last_point.timestamp - first_point.timestamp
    let value_diff = last_point.value - first_point.value
    
    let slope = value_diff / time_diff.to_float()
    
    let direction = if slope > 0.001 {
      "increasing"
    } else if slope < -0.001 {
      "decreasing"
    } else {
      "stable"
    }
    
    { slope = slope, direction = direction }
  }
  
  // Test overall trend
  let overall_trend = calculate_trend(trend_points)
  assert_true(overall_trend.direction == "stable")  // Starts at 10, ends at 20, but with variation
  
  // Test trend on first half (increasing)
  let first_half = trend_points.slice(0, 5)
  let first_half_trend = calculate_trend(first_half)
  assert_true(first_half_trend.direction == "increasing")
  assert_true(first_half_trend.slope > 0)
  
  // Test trend on second half (decreasing)
  let second_half = trend_points.slice(5)
  let second_half_trend = calculate_trend(second_half)
  assert_true(second_half_trend.direction == "decreasing")
  assert_true(second_half_trend.slope < 0)
  
  // Test moving average
  let moving_average = fn(points, window_size) {
    if points.length() < window_size {
      return []
    }
    
    let mut ma_points = []
    for i in window_size - 1 ..< points.length() {
      let window_start = i - (window_size - 1)
      let window = points.slice(window_start, i + 1)
      let avg = avg_aggregation(window)
      
      ma_points = ma_points.push({
        timestamp = points[i].timestamp,
        value = avg,
        tags = {}
      })
    }
    
    ma_points
  }
  
  // Test 3-point moving average
  let ma3 = moving_average(trend_points, 3)
  assert_eq(ma3.length(), 8)  // 10 - 3 + 1 = 8
  
  assert_eq(ma3[0].value, 15.0)  // (10 + 15 + 20) / 3
  assert_eq(ma3[1].value, 20.0)  // (15 + 20 + 25) / 3
  assert_eq(ma3[2].value, 25.0)  // (20 + 25 + 30) / 3
  assert_eq(ma3[3].value, 27.67)  // (25 + 30 + 28) / 3
  assert_eq(ma3[7].value, 22.0)  // (24 + 22 + 20) / 3
  
  // Test rate of change
  let rate_of_change = fn(points) {
    if points.length() < 2 {
      return []
    }
    
    let mut roc_points = []
    for i in 1 ..< points.length() {
      let prev_point = points[i - 1]
      let curr_point = points[i]
      
      let time_diff = curr_point.timestamp - prev_point.timestamp
      let value_diff = curr_point.value - prev_point.value
      let rate = value_diff / time_diff.to_float()
      
      roc_points = roc_points.push({
        timestamp = curr_point.timestamp,
        value = rate,
        tags = {}
      })
    }
    
    roc_points
  }
  
  let roc = rate_of_change(trend_points)
  assert_eq(roc.length(), 9)
  
  assert_eq(roc[0].value, 0.00083)  // (15-10) / 6000
  assert_eq(roc[1].value, 0.00083)  // (20-15) / 6000
  assert_eq(roc[2].value, 0.00083)  // (25-20) / 6000
  assert_eq(roc[3].value, 0.00083)  // (30-25) / 6000
  assert_eq(roc[4].value, -0.00033) // (28-30) / 6000
  assert_eq(roc[5].value, -0.00033) // (26-28) / 6000
  assert_eq(roc[6].value, -0.00033) // (24-26) / 6000
  assert_eq(roc[7].value, -0.00033) // (22-24) / 6000
  assert_eq(roc[8].value, -0.00033) // (20-22) / 6000
}

// Test 7: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Create time series with anomalies
  let create_anomaly_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = {} },  // 00:00:00
      { timestamp = 1640995260000, value = 12.0, tags = {} },  // 00:01:00
      { timestamp = 1640995320000, value = 11.0, tags = {} },  // 00:02:00
      { timestamp = 1640995380000, value = 13.0, tags = {} },  // 00:03:00
      { timestamp = 1640995440000, value = 50.0, tags = {} },  // 00:04:00 - ANOMALY
      { timestamp = 1640995500000, value = 10.0, tags = {} },  // 00:05:00
      { timestamp = 1640995560000, value = 12.0, tags = {} },  // 00:06:00
      { timestamp = 1640995620000, value = 11.0, tags = {} },  // 00:07:00
      { timestamp = 1640995680000, value = 13.0, tags = {} },  // 00:08:00
      { timestamp = 1640995740000, value = 12.0, tags = {} }   // 00:09:00
    ]
  }
  
  let anomaly_points = create_anomaly_points()
  
  // Test statistical anomaly detection
  let detect_statistical_anomalies = fn(points, threshold) {
    if points.length() < 3 {
      return []
    }
    
    // Calculate mean and standard deviation
    let mean = avg_aggregation(points)
    
    let mut variance_sum = 0.0
    for point in points {
      let diff = point.value - mean
      variance_sum = variance_sum + (diff * diff)
    }
    let std_dev = (variance_sum / points.length().to_float()).sqrt()
    
    // Detect anomalies
    let mut anomalies = []
    for point in points {
      let z_score = (point.value - mean) / std_dev
      if z_score.abs() > threshold {
        anomalies = anomalies.push({
          point = point,
          z_score = z_score,
          reason = "Statistical anomaly: z-score = " + z_score.to_string()
        })
      }
    }
    
    anomalies
  }
  
  // Test with 2.0 threshold (2 standard deviations)
  let statistical_anomalies = detect_statistical_anomalies(anomaly_points, 2.0)
  assert_eq(statistical_anomalies.length(), 1)
  assert_eq(statistical_anomalies[0].point.timestamp, 1640995440000)
  assert_eq(statistical_anomalies[0].point.value, 50.0)
  assert_true(statistical_anomalies[0].z_score > 2.0)
  
  // Test moving average anomaly detection
  let detect_ma_anomalies = fn(points, window_size, threshold) {
    if points.length() < window_size + 1 {
      return []
    }
    
    let mut anomalies = []
    
    for i in window_size ..< points.length() {
      let window_start = i - window_size
      let window = points.slice(window_start, i)
      let current_point = points[i]
      
      let ma = avg_aggregation(window)
      let deviation = (current_point.value - ma).abs()
      let relative_deviation = deviation / ma
      
      if relative_deviation > threshold {
        anomalies = anomalies.push({
          point = current_point,
          moving_average = ma,
          relative_deviation = relative_deviation,
          reason = "Moving average anomaly: deviation = " + relative_deviation.to_string()
        })
      }
    }
    
    anomalies
  }
  
  // Test with 3-point window and 100% threshold
  let ma_anomalies = detect_ma_anomalies(anomaly_points, 3, 1.0)
  assert_eq(ma_anomalies.length(), 1)
  assert_eq(ma_anomalies[0].point.timestamp, 1640995440000)
  assert_eq(ma_anomalies[0].point.value, 50.0)
  assert_true(ma_anomalies[0].relative_deviation > 1.0)
  
  // Test neighboring points anomaly detection
  let detect_neighbor_anomalies = fn(points, threshold) {
    if points.length() < 3 {
      return []
    }
    
    let mut anomalies = []
    
    for i in 1 ..< points.length() - 1 {
      let prev_point = points[i - 1]
      let curr_point = points[i]
      let next_point = points[i + 1]
      
      let neighbor_avg = (prev_point.value + next_point.value) / 2.0
      let deviation = (curr_point.value - neighbor_avg).abs()
      let relative_deviation = deviation / neighbor_avg
      
      if relative_deviation > threshold {
        anomalies = anomalies.push({
          point = curr_point,
          neighbor_average = neighbor_avg,
          relative_deviation = relative_deviation,
          reason = "Neighbor anomaly: deviation = " + relative_deviation.to_string()
        })
      }
    }
    
    anomalies
  }
  
  // Test with 100% threshold
  let neighbor_anomalies = detect_neighbor_anomalies(anomaly_points, 1.0)
  assert_eq(neighbor_anomalies.length(), 1)
  assert_eq(neighbor_anomalies[0].point.timestamp, 1640995440000)
  assert_eq(neighbor_anomalies[0].point.value, 50.0)
  assert_true(neighbor_anomalies[0].relative_deviation > 1.0)
}

// Test 8: Time Series Forecasting
test "time series forecasting" {
  // Create time series for forecasting
  let create_forecast_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = {} },  // 00:00:00
      { timestamp = 1640995260000, value = 12.0, tags = {} },  // 00:01:00
      { timestamp = 1640995320000, value = 14.0, tags = {} },  // 00:02:00
      { timestamp = 1640995380000, value = 16.0, tags = {} },  // 00:03:00
      { timestamp = 1640995440000, value = 18.0, tags = {} },  // 00:04:00
      { timestamp = 1640995500000, value = 20.0, tags = {} }   // 00:05:00
    ]
  }
  
  let forecast_points = create_forecast_points()
  
  // Test linear forecasting
  let linear_forecast = fn(points, num_steps) {
    if points.length() < 2 {
      return []
    }
    
    let first_point = points[0]
    let last_point = points[points.length() - 1]
    
    let time_diff = last_point.timestamp - first_point.timestamp
    let value_diff = last_point.value - first_point.value
    let slope = value_diff / time_diff.to_float()
    
    let interval = if points.length() > 1 {
      points[1].timestamp - points[0].timestamp
    } else {
      60000  // Default to 1 minute
    }
    
    let mut forecast_points = []
    for i in 1 ..= num_steps {
      let future_timestamp = last_point.timestamp + (interval * i)
      let future_value = last_point.value + (slope * (interval * i).to_float())
      
      forecast_points = forecast_points.push({
        timestamp = future_timestamp,
        value = future_value,
        tags = { "forecast" = "true" }
      })
    }
    
    forecast_points
  }
  
  // Test 3-step linear forecast
  let linear_forecast_points = linear_forecast(forecast_points, 3)
  assert_eq(linear_forecast_points.length(), 3)
  
  assert_eq(linear_forecast_points[0].timestamp, 1640995560000)  // 00:06:00
  assert_eq(linear_forecast_points[0].value, 22.0)              // 20 + 2
  
  assert_eq(linear_forecast_points[1].timestamp, 1640995620000)  // 00:07:00
  assert_eq(linear_forecast_points[1].value, 24.0)              // 20 + 4
  
  assert_eq(linear_forecast_points[2].timestamp, 1640995680000)  // 00:08:00
  assert_eq(linear_forecast_points[2].value, 26.0)              // 20 + 6
  
  // Test seasonal naive forecasting
  let seasonal_naive_forecast = fn(points, season_length, num_steps) {
    if points.length() < season_length {
      return []
    }
    
    let interval = if points.length() > 1 {
      points[1].timestamp - points[0].timestamp
    } else {
      60000  // Default to 1 minute
    }
    
    let mut forecast_points = []
    for i in 1 ..= num_steps {
      let future_timestamp = points[points.length() - 1].timestamp + (interval * i)
      let seasonal_index = (points.length() - season_length + i - 1) % season_length
      let future_value = points[seasonal_index].value
      
      forecast_points = forecast_points.push({
        timestamp = future_timestamp,
        value = future_value,
        tags = { "forecast" = "true", "method" = "seasonal_naive" }
      })
    }
    
    forecast_points
  }
  
  // Create seasonal data
  let create_seasonal_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = {} },  // 00:00:00
      { timestamp = 1640995260000, value = 20.0, tags = {} },  // 00:01:00
      { timestamp = 1640995320000, value = 30.0, tags = {} },  // 00:02:00
      { timestamp = 1640995380000, value = 10.0, tags = {} },  // 00:03:00
      { timestamp = 1640995440000, value = 20.0, tags = {} },  // 00:04:00
      { timestamp = 1640995500000, value = 30.0, tags = {} }   // 00:05:00
    ]
  }
  
  let seasonal_points = create_seasonal_points()
  
  // Test seasonal naive forecast with season length of 3
  let seasonal_forecast_points = seasonal_naive_forecast(seasonal_points, 3, 3)
  assert_eq(seasonal_forecast_points.length(), 3)
  
  assert_eq(seasonal_forecast_points[0].timestamp, 1640995560000)  // 00:06:00
  assert_eq(seasonal_forecast_points[0].value, 10.0)              // Same as point 3
  
  assert_eq(seasonal_forecast_points[1].timestamp, 1640995620000)  // 00:07:00
  assert_eq(seasonal_forecast_points[1].value, 20.0)              // Same as point 4
  
  assert_eq(seasonal_forecast_points[2].timestamp, 1640995680000)  // 00:08:00
  assert_eq(seasonal_forecast_points[2].value, 30.0)              // Same as point 5
  
  // Test moving average forecasting
  let ma_forecast = fn(points, window_size, num_steps) {
    if points.length() < window_size {
      return []
    }
    
    let interval = if points.length() > 1 {
      points[1].timestamp - points[0].timestamp
    } else {
      60000  // Default to 1 minute
    }
    
    // Calculate the last moving average
    let window_start = points.length() - window_size
    let window = points.slice(window_start)
    let last_ma = avg_aggregation(window)
    
    let mut forecast_points = []
    for i in 1 ..= num_steps {
      let future_timestamp = points[points.length() - 1].timestamp + (interval * i)
      
      forecast_points = forecast_points.push({
        timestamp = future_timestamp,
        value = last_ma,
        tags = { "forecast" = "true", "method" = "moving_average" }
      })
    }
    
    forecast_points
  }
  
  // Test 3-step moving average forecast with window size of 3
  let ma_forecast_points = ma_forecast(forecast_points, 3, 3)
  assert_eq(ma_forecast_points.length(), 3)
  
  assert_eq(ma_forecast_points[0].timestamp, 1640995560000)  // 00:06:00
  assert_eq(ma_forecast_points[0].value, 18.0)              // Average of last 3 points (16, 18, 20)
  
  assert_eq(ma_forecast_points[1].timestamp, 1640995620000)  // 00:07:00
  assert_eq(ma_forecast_points[1].value, 18.0)              // Same MA
  
  assert_eq(ma_forecast_points[2].timestamp, 1640995680000)  // 00:08:00
  assert_eq(ma_forecast_points[2].value, 18.0)              // Same MA
}

// Test 9: Time Series Compression
test "time series compression" {
  // Create time series for compression
  let create_compression_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = {} },  // 00:00:00
      { timestamp = 1640995260000, value = 10.1, tags = {} },  // 00:01:00
      { timestamp = 1640995320000, value = 10.2, tags = {} },  // 00:02:00
      { timestamp = 1640995380000, value = 15.0, tags = {} },  // 00:03:00
      { timestamp = 1640995440000, value = 15.1, tags = {} },  // 00:04:00
      { timestamp = 1640995500000, value = 15.2, tags = {} },  // 00:05:00
      { timestamp = 1640995560000, value = 20.0, tags = {} },  // 00:06:00
      { timestamp = 1640995620000, value = 20.1, tags = {} },  // 00:07:00
      { timestamp = 1640995680000, value = 20.2, tags = {} },  // 00:08:00
      { timestamp = 1640995740000, value = 20.3, tags = {} }   // 00:09:00
    ]
  }
  
  let compression_points = create_compression_points()
  
  // Test delta encoding compression
  let delta_encode = fn(points) {
    if points.length() == 0 {
      return []
    }
    
    let mut encoded_points = []
    let mut prev_timestamp = points[0].timestamp
    let mut prev_value = points[0].value
    
    // First point is stored as-is
    encoded_points = encoded_points.push({
      timestamp = points[0].timestamp,
      value = points[0].value,
      tags = points[0].tags
    })
    
    // Subsequent points are stored as deltas
    for i in 1 ..< points.length() {
      let point = points[i]
      let timestamp_delta = point.timestamp - prev_timestamp
      let value_delta = point.value - prev_value
      
      encoded_points = encoded_points.push({
        timestamp = timestamp_delta,
        value = value_delta,
        tags = point.tags
      })
      
      prev_timestamp = point.timestamp
      prev_value = point.value
    }
    
    encoded_points
  }
  
  let delta_encoded = delta_encode(compression_points)
  assert_eq(delta_encoded.length(), 10)
  
  // First point should be unchanged
  assert_eq(delta_encoded[0].timestamp, 1640995200000)
  assert_eq(delta_encoded[0].value, 10.0)
  
  // Subsequent points should be deltas
  assert_eq(delta_encoded[1].timestamp, 6000)   // 1 minute delta
  assert_eq(delta_encoded[1].value, 0.1)       // 0.1 value delta
  
  assert_eq(delta_encoded[2].timestamp, 6000)   // 1 minute delta
  assert_eq(delta_encoded[2].value, 0.1)       // 0.1 value delta
  
  assert_eq(delta_encoded[3].timestamp, 6000)   // 1 minute delta
  assert_eq(delta_encoded[3].value, 4.8)       // 4.8 value delta
  
  // Test delta decoding
  let delta_decode = fn(encoded_points) {
    if encoded_points.length() == 0 {
      return []
    }
    
    let mut decoded_points = []
    let mut prev_timestamp = encoded_points[0].timestamp
    let mut prev_value = encoded_points[0].value
    
    // First point is as-is
    decoded_points = decoded_points.push({
      timestamp = encoded_points[0].timestamp,
      value = encoded_points[0].value,
      tags = encoded_points[0].tags
    })
    
    // Subsequent points are reconstructed from deltas
    for i in 1 ..< encoded_points.length() {
      let encoded_point = encoded_points[i]
      let timestamp = prev_timestamp + encoded_point.timestamp
      let value = prev_value + encoded_point.value
      
      decoded_points = decoded_points.push({
        timestamp = timestamp,
        value = value,
        tags = encoded_point.tags
      })
      
      prev_timestamp = timestamp
      prev_value = value
    }
    
    decoded_points
  }
  
  let delta_decoded = delta_decode(delta_encoded)
  assert_eq(delta_decoded.length(), 10)
  
  // Verify decoded points match original
  for i in 0 ..< compression_points.length() {
    assert_eq(delta_decoded[i].timestamp, compression_points[i].timestamp)
    assert_eq(delta_decoded[i].value, compression_points[i].value)
  }
  
  // Test swing door compression (simplified)
  let swing_door_compress = fn(points, threshold) {
    if points.length() < 3 {
      return points
    }
    
    let mut compressed = []
    compressed = compressed.push(points[0])  // Always include first point
    
    let mut start_idx = 0
    let mut end_idx = 1
    
    while end_idx < points.length() {
      let start_point = points[start_idx]
      let end_point = points[end_idx]
      
      // Check if all points between start and end are within threshold
      let mut all_within_threshold = true
      let slope = (end_point.value - start_point.value) / (end_point.timestamp - start_point.timestamp).to_float()
      
      for i in start_idx + 1 ..< end_idx {
        let point = points[i]
        let expected_value = start_point.value + slope * (point.timestamp - start_point.timestamp).to_float()
        let deviation = (point.value - expected_value).abs()
        
        if deviation > threshold {
          all_within_threshold = false
          break
        }
      }
      
      if all_within_threshold && end_idx < points.length() - 1 {
        end_idx = end_idx + 1
      } else {
        compressed = compressed.push(points[end_idx])
        start_idx = end_idx
        end_idx = end_idx + 1
      }
    }
    
    compressed
  }
  
  // Test swing door compression with threshold of 0.5
  let swing_compressed = swing_door_compress(compression_points, 0.5)
  assert_eq(swing_compressed.length(), 4)  // Should reduce from 10 to 4 points
  
  assert_eq(swing_compressed[0].timestamp, 1640995200000)  // First point
  assert_eq(swing_compressed[0].value, 10.0)
  
  assert_eq(swing_compressed[1].timestamp, 1640995380000)  // Around where value changes significantly
  assert_eq(swing_compressed[1].value, 15.0)
  
  assert_eq(swing_compressed[2].timestamp, 1640995560000)  // Around where value changes significantly
  assert_eq(swing_compressed[2].value, 20.0)
  
  assert_eq(swing_compressed[3].timestamp, 1640995740000)  // Last point
  assert_eq(swing_compressed[3].value, 20.3)
}

// Test 10: Time Series Query and Filtering
test "time series query and filtering" {
  // Create time series with different tags
  let create_tagged_points = fn() {
    [
      { timestamp = 1640995200000, value = 10.0, tags = { "service": "api", "env": "prod", "region": "us-east" } },
      { timestamp = 1640995260000, value = 12.0, tags = { "service": "api", "env": "prod", "region": "us-east" } },
      { timestamp = 1640995320000, value = 15.0, tags = { "service": "web", "env": "prod", "region": "us-east" } },
      { timestamp = 1640995380000, value = 18.0, tags = { "service": "web", "env": "prod", "region": "us-east" } },
      { timestamp = 1640995440000, value = 20.0, tags = { "service": "api", "env": "dev", "region": "us-west" } },
      { timestamp = 1640995500000, value = 22.0, tags = { "service": "api", "env": "dev", "region": "us-west" } },
      { timestamp = 1640995560000, value = 25.0, tags = { "service": "web", "env": "dev", "region": "us-west" } },
      { timestamp = 1640995620000, value = 28.0, tags = { "service": "web", "env": "dev", "region": "us-west" } }
    ]
  }
  
  let tagged_points = create_tagged_points()
  
  // Test tag-based filtering
  let filter_by_tags = fn(points, tag_filters) {
    let mut filtered_points = []
    
    for point in points {
      let mut matches_all = true
      
      for (tag_key, tag_value) in tag_filters {
        if !point.tags.contains(tag_key) || point.tags[tag_key] != tag_value {
          matches_all = false
          break
        }
      }
      
      if matches_all {
        filtered_points = filtered_points.push(point)
      }
    }
    
    filtered_points
  }
  
  // Test filtering by service
  let api_points = filter_by_tags(tagged_points, { "service": "api" })
  assert_eq(api_points.length(), 4)
  assert_eq(api_points[0].tags["service"], "api")
  assert_eq(api_points[1].tags["service"], "api")
  assert_eq(api_points[2].tags["service"], "api")
  assert_eq(api_points[3].tags["service"], "api")
  
  // Test filtering by environment
  let prod_points = filter_by_tags(tagged_points, { "env": "prod" })
  assert_eq(prod_points.length(), 4)
  assert_eq(prod_points[0].tags["env"], "prod")
  assert_eq(prod_points[1].tags["env"], "prod")
  assert_eq(prod_points[2].tags["env"], "prod")
  assert_eq(prod_points[3].tags["env"], "prod")
  
  // Test filtering by multiple tags
  let api_prod_points = filter_by_tags(tagged_points, { "service": "api", "env": "prod" })
  assert_eq(api_prod_points.length(), 2)
  assert_eq(api_prod_points[0].tags["service"], "api")
  assert_eq(api_prod_points[0].tags["env"], "prod")
  assert_eq(api_prod_points[1].tags["service"], "api")
  assert_eq(api_prod_points[1].tags["env"], "prod")
  
  // Test value-based filtering
  let filter_by_value = fn(points, min_value, max_value) {
    let mut filtered_points = []
    
    for point in points {
      if point.value >= min_value && point.value <= max_value {
        filtered_points = filtered_points.push(point)
      }
    }
    
    filtered_points
  }
  
  // Test filtering by value range
  let mid_range_points = filter_by_value(tagged_points, 15.0, 25.0)
  assert_eq(mid_range_points.length(), 4)
  assert_eq(mid_range_points[0].value, 15.0)
  assert_eq(mid_range_points[1].value, 18.0)
  assert_eq(mid_range_points[2].value, 20.0)
  assert_eq(mid_range_points[3].value, 22.0)
  
  // Test time-based filtering
  let filter_by_time = fn(points, start_time, end_time) {
    let mut filtered_points = []
    
    for point in points {
      if point.timestamp >= start_time && point.timestamp < end_time {
        filtered_points = filtered_points.push(point)
      }
    }
    
    filtered_points
  }
  
  // Test filtering by time range
  let time_filtered_points = filter_by_time(tagged_points, 1640995300000, 1640995550000)
  assert_eq(time_filtered_points.length(), 3)
  assert_eq(time_filtered_points[0].timestamp, 1640995320000)
  assert_eq(time_filtered_points[1].timestamp, 1640995380000)
  assert_eq(time_filtered_points[2].timestamp, 1640995440000)
  
  // Test complex query with multiple filters
  let complex_query = fn(points, tag_filters, value_range, time_range) {
    let mut filtered_points = points
    
    // Apply tag filters
    if tag_filters.length() > 0 {
      filtered_points = filter_by_tags(filtered_points, tag_filters)
    }
    
    // Apply value filter
    if value_range.length() == 2 {
      filtered_points = filter_by_value(filtered_points, value_range[0], value_range[1])
    }
    
    // Apply time filter
    if time_range.length() == 2 {
      filtered_points = filter_by_time(filtered_points, time_range[0], time_range[1])
    }
    
    filtered_points
  }
  
  // Test complex query
  let complex_result = complex_query(
    tagged_points,
    { "service": "web" },
    [20.0, 30.0],
    [1640995500000, 1640995700000]
  )
  
  assert_eq(complex_result.length(), 2)
  assert_eq(complex_result[0].tags["service"], "web")
  assert_eq(complex_result[0].value, 25.0)
  assert_eq(complex_result[1].tags["service"], "web")
  assert_eq(complex_result[1].value, 28.0)
}