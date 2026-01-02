// Azimuth Time Series Operations Tests
// This file contains test cases for time series data operations

// Test 1: Time Series Data Creation
test "time series data creation and ordering" {
  let time_series = []
  let base_time = 1640995200 // 2022-01-01 00:00:00 UTC
  
  // Create ordered time series data
  for i in 0..<10 {
    let timestamp = base_time + (i * 60) // 1-minute intervals
    let value = Int::to_float(i) * 2.5
    time_series = time_series.push((timestamp, value))
  }
  
  assert_eq(time_series.length(), 10)
  
  // Verify chronological order
  for i in 1..<time_series.length() {
    assert_true(time_series[i].0 > time_series[i-1].0)
  }
  
  // Verify first and last timestamps
  assert_eq(time_series[0].0, base_time)
  assert_eq(time_series[9].0, base_time + 540)
}

// Test 2: Time Series Resampling
test "time series resampling operations" {
  let original_series = [
    (1000, 10.0),
    (1100, 15.0),
    (1200, 20.0),
    (1300, 25.0),
    (1400, 30.0),
    (1500, 35.0)
  ]
  
  // Downsample to 300-second intervals
  let downsampled = []
  for i in 0..<original_series.length() {
    if i % 2 == 0 {
      downsampled = downsampled.push(original_series[i])
    }
  }
  
  assert_eq(downsampled.length(), 3)
  assert_eq(downsampled[0].0, 1000)
  assert_eq(downsampled[2].0, 1400)
  
  // Upsample by interpolation (simplified)
  let upsampled = []
  for i in 0..<downsampled.length() - 1 {
    let current = downsampled[i]
    let next = downsampled[i + 1]
    
    // Add original point
    upsampled = upsampled.push(current)
    
    // Add interpolated point
    let interp_time = current.0 + ((next.0 - current.0) / 2)
    let interp_value = (current.1 + next.1) / 2.0
    upsampled = upsampled.push((interp_time, interp_value))
  }
  upsampled = upsampled.push(downsampled[downsampled.length() - 1])
  
  assert_eq(upsampled.length(), 5)
  assert_eq(upsampled[1].0, 1100) // Interpolated point
}

// Test 3: Time Series Moving Average
test "time series moving average calculation" {
  let series = [
    (1000, 10.0),
    (2000, 20.0),
    (3000, 30.0),
    (4000, 40.0),
    (5000, 50.0),
    (6000, 60.0)
  ]
  
  let window_size = 3
  let moving_averages = []
  
  // Calculate moving averages
  for i in window_size-1..<series.length() {
    let mut sum = 0.0
    for j in 0..<window_size {
      sum = sum + series[i - j].1
    }
    let avg = sum / Int::to_float(window_size)
    moving_averages = moving_averages.push((series[i].0, avg))
  }
  
  assert_eq(moving_averages.length(), 4)
  assert_eq(moving_averages[0].1, 20.0) // (10+20+30)/3
  assert_eq(moving_averages[1].1, 30.0) // (20+30+40)/3
  assert_eq(moving_averages[2].1, 40.0) // (30+40+50)/3
  assert_eq(moving_averages[3].1, 50.0) // (40+50+60)/3
}

// Test 4: Time Series Trend Analysis
test "time series trend analysis" {
  let increasing_series = [
    (1000, 10.0),
    (2000, 20.0),
    (3000, 30.0),
    (4000, 40.0),
    (5000, 50.0)
  ]
  
  let decreasing_series = [
    (1000, 50.0),
    (2000, 40.0),
    (3000, 30.0),
    (4000, 20.0),
    (5000, 10.0)
  ]
  
  // Calculate trend (simplified slope calculation)
  let calculate_trend = fn(series) {
    if series.length() < 2 {
      0.0
    } else {
      let first = series[0]
      let last = series[series.length() - 1]
      (last.1 - first.1) / Int::to_float(last.0 - first.0)
    }
  }
  
  let increasing_trend = calculate_trend(increasing_series)
  let decreasing_trend = calculate_trend(decreasing_series)
  
  assert_true(increasing_trend > 0.0)
  assert_true(decreasing_trend < 0.0)
  assert_eq(increasing_trend, 0.01) // (50-10)/(5000-1000)
  assert_eq(decreasing_trend, -0.01) // (10-50)/(5000-1000)
}

// Test 5: Time Series Anomaly Detection
test "time series anomaly detection" {
  let normal_series = [
    (1000, 10.0),
    (2000, 12.0),
    (3000, 11.0),
    (4000, 13.0),
    (5000, 10.0),
    (6000, 100.0), // Anomaly
    (7000, 12.0),
    (8000, 11.0)
  ]
  
  // Calculate mean and standard deviation
  let values = normal_series.map(fn(point) { point.1 })
  let mean = values.reduce(fn(acc, val) { acc + val }, 0.0) / Int::to_float(values.length())
  
  let mut variance = 0.0
  for val in values {
    let diff = val - mean
    variance = variance + (diff * diff)
  }
  variance = variance / Int::to_float(values.length())
  let std_dev = variance.sqrt()
  
  // Detect anomalies (values beyond 2 standard deviations)
  let threshold = 2.0 * std_dev
  let anomalies = normal_series.filter(fn(point) { 
    (point.1 - mean).abs() > threshold 
  })
  
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].0, 6000)
  assert_eq(anomalies[0].1, 100.0)
}

// Test 6: Time Series Seasonality Detection
test "time series seasonality detection" {
  // Create seasonal data (daily pattern)
  let seasonal_data = []
  let base_time = 1640995200 // 2022-01-01 00:00:00 UTC
  
  for day in 0..<7 {
    for hour in 0..<24 {
      let timestamp = base_time + (day * 86400) + (hour * 3600)
      // Simulate daily pattern with higher values during business hours
      let value = if hour >= 9 && hour <= 17 { 
        50.0 + Int::to_float(hour) 
      } else { 
        10.0 + Int::to_float(hour) * 0.5 
      }
      seasonal_data = seasonal_data.push((timestamp, value))
    }
  }
  
  assert_eq(seasonal_data.length(), 168) // 7 days * 24 hours
  
  // Check daily pattern by comparing same hours across different days
  let hour_9_values = seasonal_data.filter(fn(point) { 
    let time_of_day = (point.0 % 86400) / 3600
    time_of_day == 9
  })
  
  assert_eq(hour_9_values.length(), 7)
  
  // All hour 9 values should be similar (all during business hours)
  let all_business_hours = hour_9_values.all(fn(point) { point.1 > 50.0 })
  assert_true(all_business_hours)
}

// Test 7: Time Series Forecasting (Simple Linear Projection)
test "time series simple forecasting" {
  let historical_data = [
    (1000, 10.0),
    (2000, 20.0),
    (3000, 30.0),
    (4000, 40.0),
    (5000, 50.0)
  ]
  
  // Simple linear forecasting
  let calculate_slope = fn(series) {
    let n = Int::to_float(series.length())
    let sum_x = series.reduce(fn(acc, point) { acc + Int::to_float(point.0) }, 0.0)
    let sum_y = series.reduce(fn(acc, point) { acc + point.1 }, 0.0)
    let sum_xy = series.reduce(fn(acc, point) { 
      acc + (Int::to_float(point.0) * point.1) 
    }, 0.0)
    let sum_x2 = series.reduce(fn(acc, point) { 
      acc + (Int::to_float(point.0) * Int::to_float(point.0)) 
    }, 0.0)
    
    (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  }
  
  let slope = calculate_slope(historical_data)
  
  // Forecast next point
  let last_timestamp = historical_data[historical_data.length() - 1].0
  let last_value = historical_data[historical_data.length() - 1].1
  let next_timestamp = last_timestamp + 1000
  let forecast_value = last_value + (slope * 1000.0)
  
  assert_eq(slope, 0.01)
  assert_eq(forecast_value, 60.0)
}

// Test 8: Time Series Gap Handling
test "time series gap handling and interpolation" {
  let gapped_series = [
    (1000, 10.0),
    (2000, 20.0),
    // Gap: missing 3000
    (4000, 40.0),
    (5000, 50.0),
    // Gap: missing 6000, 7000
    (8000, 80.0)
  ]
  
  // Fill gaps using linear interpolation
  let filled_series = []
  let mut i = 0
  
  while i < gapped_series.length() {
    filled_series = filled_series.push(gapped_series[i])
    
    if i < gapped_series.length() - 1 {
      let current = gapped_series[i]
      let next = gapped_series[i + 1]
      let gap = next.0 - current.0
      
      // Fill gaps larger than 1000
      if gap > 1000 {
        let steps = gap / 1000 - 1
        for j in 1..=steps {
          let interp_time = current.0 + (j * 1000)
          let ratio = Int::to_float(j) / Int::to_float(steps + 1)
          let interp_value = current.1 + ((next.1 - current.1) * ratio)
          filled_series = filled_series.push((interp_time, interp_value))
        }
      }
    }
    
    i = i + 1
  }
  
  assert_eq(filled_series.length(), 8)
  
  // Check interpolated values
  assert_eq(filled_series[2].0, 3000)
  assert_eq(filled_series[2].1, 30.0) // Linear interpolation between 20.0 and 40.0
  
  assert_eq(filled_series[5].0, 6000)
  assert_eq(filled_series[5].1, 60.0) // Linear interpolation between 50.0 and 80.0
  
  assert_eq(filled_series[6].0, 7000)
  assert_eq(filled_series[6].1, 70.0) // Linear interpolation between 50.0 and 80.0
}