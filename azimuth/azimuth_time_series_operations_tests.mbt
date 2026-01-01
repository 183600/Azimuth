// Azimuth Time Series Operations Test Suite
// This file contains test cases for time series data operations

// Test 1: Basic time series data operations
test "basic time series data operations" {
  // Test time series data creation with timestamps
  let timestamp1 = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let timestamp2 = 1735689601000000000L  // 2025-01-01 00:00:01 UTC
  let timestamp3 = 1735689602000000000L  // 2025-01-01 00:00:02 UTC
  
  // Create time series data points
  let data_point1 = (timestamp1, 100.0)
  let data_point2 = (timestamp2, 150.0)
  let data_point3 = (timestamp3, 120.0)
  
  // Verify data points
  assert_eq(data_point1.0, timestamp1)
  assert_eq(data_point1.1, 100.0)
  assert_eq(data_point2.0, timestamp2)
  assert_eq(data_point2.1, 150.0)
  assert_eq(data_point3.0, timestamp3)
  assert_eq(data_point3.1, 120.0)
  
  // Create time series array
  let time_series = [data_point1, data_point2, data_point3]
  assert_eq(time_series.length(), 3)
  assert_eq(time_series[0], data_point1)
  assert_eq(time_series[1], data_point2)
  assert_eq(time_series[2], data_point3)
  
  // Test time series ordering
  assert_true(time_series[0].0 <= time_series[1].0)
  assert_true(time_series[1].0 <= time_series[2].0)
}

// Test 2: Time series aggregation operations
test "time series aggregation operations" {
  // Create time series data for aggregation
  let base_timestamp = 1735689600000000000L
  let time_series = [
    (base_timestamp, 10.0),
    (base_timestamp + 1000000000L, 20.0),
    (base_timestamp + 2000000000L, 30.0),
    (base_timestamp + 3000000000L, 40.0),
    (base_timestamp + 4000000000L, 50.0)
  ]
  
  // Calculate sum manually
  let mut sum = 0.0
  for point in time_series {
    sum = sum + point.1
  }
  assert_eq(sum, 150.0)
  
  // Calculate average manually
  let average = sum / time_series.length().to_double()
  assert_eq(average, 30.0)
  
  // Find min and max manually
  let mut min = time_series[0].1
  let mut max = time_series[0].1
  
  for point in time_series {
    if point.1 < min {
      min = point.1
    }
    if point.1 > max {
      max = point.1
    }
  }
  
  assert_eq(min, 10.0)
  assert_eq(max, 50.0)
  
  // Calculate range
  let range = max - min
  assert_eq(range, 40.0)
}

// Test 3: Time series with different data types
test "time series with different data types" {
  let base_timestamp = 1735689600000000000L
  
  // Test with integer values
  let int_time_series = [
    (base_timestamp, 10),
    (base_timestamp + 1000000000L, 20),
    (base_timestamp + 2000000000L, 30),
    (base_timestamp + 3000000000L, 40),
    (base_timestamp + 4000000000L, 50)
  ]
  
  // Verify integer time series
  assert_eq(int_time_series[0].1, 10)
  assert_eq(int_time_series[2].1, 30)
  assert_eq(int_time_series[4].1, 50)
  
  // Test with boolean values
  let bool_time_series = [
    (base_timestamp, true),
    (base_timestamp + 1000000000L, false),
    (base_timestamp + 2000000000L, true),
    (base_timestamp + 3000000000L, true),
    (base_timestamp + 4000000000L, false)
  ]
  
  // Verify boolean time series
  assert_true(bool_time_series[0].1)
  assert_false(bool_time_series[1].1)
  assert_true(bool_time_series[2].1)
  assert_true(bool_time_series[3].1)
  assert_false(bool_time_series[4].1)
  
  // Test with string values
  let string_time_series = [
    (base_timestamp, "value1"),
    (base_timestamp + 1000000000L, "value2"),
    (base_timestamp + 2000000000L, "value3"),
    (base_timestamp + 3000000000L, "value4"),
    (base_timestamp + 4000000000L, "value5")
  ]
  
  // Verify string time series
  assert_eq(string_time_series[0].1, "value1")
  assert_eq(string_time_series[2].1, "value3")
  assert_eq(string_time_series[4].1, "value5")
}

// Test 4: Time series with irregular intervals
test "time series with irregular intervals" {
  let base_timestamp = 1735689600000000000L
  
  // Create time series with irregular intervals
  let irregular_time_series = [
    (base_timestamp, 100.0),
    (base_timestamp + 500000000L, 110.0),    // 0.5 seconds
    (base_timestamp + 2000000000L, 130.0),   // 1.5 seconds gap
    (base_timestamp + 2100000000L, 125.0),   // 0.1 seconds
    (base_timestamp + 5000000000L, 140.0),   // 2.9 seconds gap
    (base_timestamp + 6000000000L, 145.0)    // 1.0 seconds
  ]
  
  // Calculate intervals between data points
  let intervals = [
    irregular_time_series[1].0 - irregular_time_series[0].0,
    irregular_time_series[2].0 - irregular_time_series[1].0,
    irregular_time_series[3].0 - irregular_time_series[2].0,
    irregular_time_series[4].0 - irregular_time_series[3].0,
    irregular_time_series[5].0 - irregular_time_series[4].0
  ]
  
  // Verify intervals
  assert_eq(intervals[0], 500000000L)    // 0.5 seconds
  assert_eq(intervals[1], 1500000000L)   // 1.5 seconds
  assert_eq(intervals[2], 100000000L)    // 0.1 seconds
  assert_eq(intervals[3], 2900000000L)   // 2.9 seconds
  assert_eq(intervals[4], 1000000000L)   // 1.0 seconds
  
  // Calculate average interval
  let mut total_interval = 0L
  for interval in intervals {
    total_interval = total_interval + interval
  }
  let average_interval = total_interval / intervals.length()
  assert_eq(average_interval, 1200000000L)  // 1.2 seconds average
  
  // Find minimum and maximum intervals
  let mut min_interval = intervals[0]
  let mut max_interval = intervals[0]
  
  for interval in intervals {
    if interval < min_interval {
      min_interval = interval
    }
    if interval > max_interval {
      max_interval = interval
    }
  }
  
  assert_eq(min_interval, 100000000L)     // 0.1 seconds minimum
  assert_eq(max_interval, 2900000000L)    // 2.9 seconds maximum
}

// Test 5: Time series with missing data points
test "time series with missing data points" {
  let base_timestamp = 1735689600000000000L
  
  // Create time series with missing data points (represented as None)
  let sparse_time_series = [
    (base_timestamp, Some(100.0)),
    (base_timestamp + 1000000000L, None),           // Missing data
    (base_timestamp + 2000000000L, Some(120.0)),
    (base_timestamp + 3000000000L, None),           // Missing data
    (base_timestamp + 4000000000L, None),           // Missing data
    (base_timestamp + 5000000000L, Some(130.0)),
    (base_timestamp + 6000000000L, Some(125.0)),
    (base_timestamp + 7000000000L, None)            // Missing data
  ]
  
  // Count present and missing data points
  let mut present_count = 0
  let mut missing_count = 0
  let mut present_sum = 0.0
  
  for point in sparse_time_series {
    match point.1 {
      Some(value) => {
        present_count = present_count + 1
        present_sum = present_sum + value
      }
      None => {
        missing_count = missing_count + 1
      }
    }
  }
  
  assert_eq(present_count, 4)
  assert_eq(missing_count, 4)
  assert_eq(present_sum, 475.0)
  
  // Calculate average of present values
  let present_average = present_sum / present_count.to_double()
  assert_eq(present_average, 118.75)
  
  // Calculate data completeness percentage
  let total_points = present_count + missing_count
  let completeness = present_count.to_double() / total_points.to_double() * 100.0
  assert_eq(completeness, 50.0)
}

// Test 6: Time series with sliding window operations
test "time series with sliding window operations" {
  let base_timestamp = 1735689600000000000L
  
  // Create time series data
  let time_series = [
    (base_timestamp, 10.0),
    (base_timestamp + 1000000000L, 20.0),
    (base_timestamp + 2000000000L, 30.0),
    (base_timestamp + 3000000000L, 40.0),
    (base_timestamp + 4000000000L, 50.0),
    (base_timestamp + 5000000000L, 60.0),
    (base_timestamp + 6000000000L, 70.0),
    (base_timestamp + 7000000000L, 80.0)
  ]
  
  // Test sliding window with size 3
  let window_size = 3
  
  // Calculate moving averages manually
  let moving_averages = []
  
  for i in 0..(time_series.length() - window_size + 1) {
    let mut window_sum = 0.0
    for j in i..(i + window_size) {
      window_sum = window_sum + time_series[j].1
    }
    let window_avg = window_sum / window_size.to_double()
    moving_averages.push(window_avg)
  }
  
  // Verify moving averages
  assert_eq(moving_averages.length(), 6)
  assert_eq(moving_averages[0], 20.0)  // (10 + 20 + 30) / 3
  assert_eq(moving_averages[1], 30.0)  // (20 + 30 + 40) / 3
  assert_eq(moving_averages[2], 40.0)  // (30 + 40 + 50) / 3
  assert_eq(moving_averages[3], 50.0)  // (40 + 50 + 60) / 3
  assert_eq(moving_averages[4], 60.0)  // (50 + 60 + 70) / 3
  assert_eq(moving_averages[5], 70.0)  // (60 + 70 + 80) / 3
  
  // Test sliding window with time-based window (3 seconds)
  let window_duration = 3000000000L  // 3 seconds
  
  // For each point, calculate average of points within the time window
  for i in 0..time_series.length() {
    let current_timestamp = time_series[i].0
    let window_start = current_timestamp - window_duration
    
    let mut window_sum = 0.0
    let mut window_count = 0
    
    for j in 0..time_series.length() {
      if time_series[j].0 >= window_start && time_series[j].0 <= current_timestamp {
        window_sum = window_sum + time_series[j].1
        window_count = window_count + 1
      }
    }
    
    if window_count > 0 {
      let window_avg = window_sum / window_count.to_double()
      // Verify some specific windows
      if i == 2 {  // Third point (30.0)
        assert_eq(window_avg, 20.0)  // (10 + 20 + 30) / 3
      }
      if i == 4 {  // Fifth point (50.0)
        assert_eq(window_avg, 40.0)  // (30 + 40 + 50) / 3
      }
      if i == 7 {  // Last point (80.0)
        assert_eq(window_avg, 60.0)  // (60 + 70 + 80) / 3
      }
    }
  }
}

// Test 7: Time series with trend analysis
test "time series with trend analysis" {
  let base_timestamp = 1735689600000000000L
  
  // Create time series with different trends
  let increasing_trend = [
    (base_timestamp, 10.0),
    (base_timestamp + 1000000000L, 20.0),
    (base_timestamp + 2000000000L, 30.0),
    (base_timestamp + 3000000000L, 40.0),
    (base_timestamp + 4000000000L, 50.0)
  ]
  
  let decreasing_trend = [
    (base_timestamp, 50.0),
    (base_timestamp + 1000000000L, 40.0),
    (base_timestamp + 2000000000L, 30.0),
    (base_timestamp + 3000000000L, 20.0),
    (base_timestamp + 4000000000L, 10.0)
  ]
  
  let volatile_trend = [
    (base_timestamp, 25.0),
    (base_timestamp + 1000000000L, 50.0),
    (base_timestamp + 2000000000L, 10.0),
    (base_timestamp + 3000000000L, 45.0),
    (base_timestamp + 4000000000L, 15.0)
  ]
  
  // Calculate trend for increasing series
  let increasing_first = increasing_trend[0].1
  let increasing_last = increasing_trend[increasing_trend.length() - 1].1
  let increasing_change = increasing_last - increasing_first
  assert_eq(increasing_change, 40.0)  // Positive trend
  
  // Calculate trend for decreasing series
  let decreasing_first = decreasing_trend[0].1
  let decreasing_last = decreasing_trend[decreasing_trend.length() - 1].1
  let decreasing_change = decreasing_last - decreasing_first
  assert_eq(decreasing_change, -40.0)  // Negative trend
  
  // Calculate volatility (standard deviation approximation)
  let volatile_values = [25.0, 50.0, 10.0, 45.0, 15.0]
  
  // Calculate mean
  let mut volatile_sum = 0.0
  for value in volatile_values {
    volatile_sum = volatile_sum + value
  }
  let volatile_mean = volatile_sum / volatile_values.length().to_double()
  assert_eq(volatile_mean, 29.0)
  
  // Calculate variance (simplified)
  let mut variance_sum = 0.0
  for value in volatile_values {
    let diff = value - volatile_mean
    variance_sum = variance_sum + diff * diff
  }
  let variance = variance_sum / volatile_values.length().to_double()
  assert_eq(variance, 256.0)
  
  // Calculate standard deviation
  let std_dev = @sqrt(variance)
  assert_eq(std_dev, 16.0)
  
  // Higher standard deviation indicates higher volatility
  assert_true(std_dev > 10.0)
}

// Test 8: Time series with seasonal patterns
test "time series with seasonal patterns" {
  let base_timestamp = 1735689600000000000L
  
  // Create time series with daily seasonal pattern (24 hours = 86400000000000 nanoseconds)
  let daily_pattern = [
    (base_timestamp, 50.0),                    // 00:00
    (base_timestamp + 21600000000000L, 30.0),  // 06:00
    (base_timestamp + 43200000000000L, 70.0),  // 12:00
    (base_timestamp + 64800000000000L, 40.0),  // 18:00
    (base_timestamp + 86400000000000L, 55.0),  // Next day 00:00
    (base_timestamp + 108000000000000L, 35.0), // Next day 06:00
    (base_timestamp + 129600000000000L, 75.0), // Next day 12:00
    (base_timestamp + 151200000000000L, 45.0)  // Next day 18:00
  ]
  
  // Calculate hourly pattern (values at the same time of day)
  let midnight_values = [daily_pattern[0].1, daily_pattern[4].1]      // 00:00
  let morning_values = [daily_pattern[1].1, daily_pattern[5].1]       // 06:00
  let noon_values = [daily_pattern[2].1, daily_pattern[6].1]         // 12:00
  let evening_values = [daily_pattern[3].1, daily_pattern[7].1]      // 18:00
  
  // Calculate average for each time of day
  let midnight_avg = (midnight_values[0] + midnight_values[1]) / 2.0
  let morning_avg = (morning_values[0] + morning_values[1]) / 2.0
  let noon_avg = (noon_values[0] + noon_values[1]) / 2.0
  let evening_avg = (evening_values[0] + evening_values[1]) / 2.0
  
  assert_eq(midnight_avg, 52.5)
  assert_eq(morning_avg, 32.5)
  assert_eq(noon_avg, 72.5)
  assert_eq(evening_avg, 42.5)
  
  // Verify seasonal pattern exists (noon > midnight > evening > morning)
  assert_true(noon_avg > midnight_avg)
  assert_true(midnight_avg > evening_avg)
  assert_true(evening_avg > morning_avg)
  
  // Calculate seasonal range
  let seasonal_max = noon_avg
  let seasonal_min = morning_avg
  let seasonal_range = seasonal_max - seasonal_min
  assert_eq(seasonal_range, 40.0)
  
  // Calculate seasonal amplitude (half the range)
  let seasonal_amplitude = seasonal_range / 2.0
  assert_eq(seasonal_amplitude, 20.0)
  
  // Calculate seasonal variation coefficient (amplitude / mean)
  let overall_mean = (midnight_avg + morning_avg + noon_avg + evening_avg) / 4.0
  let variation_coefficient = seasonal_amplitude / overall_mean * 100.0
  assert_eq(variation_coefficient, 40.0)  // 40% variation
}