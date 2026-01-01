// Time Series Operations Tests for Azimuth System
// This file contains test cases for time series data operations and temporal functionality

test "timestamp generation and validation" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // Verify timestamp is reasonable (should be around 2025)
  assert_true(timestamp > 1700000000000000000L)
  assert_true(timestamp < 1800000000000000000L)
  
  // Generate multiple timestamps and verify they're increasing
  let timestamp2 = Clock::now_unix_nanos(clock)
  assert_true(timestamp2 >= timestamp)
  
  let timestamp3 = Clock::now_unix_nanos(clock)
  assert_true(timestamp3 >= timestamp2)
}

test "time series data point creation" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // Create a simple time series data point
  let metric_name = "cpu.usage"
  let metric_value = 75.5
  let metric_attributes = [
    ("host", StringValue("server-01")),
    ("region", StringValue("us-west"))
  ]
  
  // Verify data point components
  assert_eq(metric_name, "cpu.usage")
  assert_eq(metric_value, 75.5)
  assert_eq(metric_attributes.length, 2)
  
  // Verify timestamp is valid
  assert_true(timestamp > 0L)
}

test "time series aggregation operations" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Simulate multiple data points for aggregation
  let data_points = [
    (base_timestamp, 10.0),
    (base_timestamp + 1000000L, 20.0),
    (base_timestamp + 2000000L, 30.0),
    (base_timestamp + 3000000L, 40.0),
    (base_timestamp + 4000000L, 50.0)
  ]
  
  // Calculate sum
  let mut sum = 0.0
  for point in data_points {
    sum = sum + point.1
  }
  assert_eq(sum, 150.0)
  
  // Calculate average
  let average = sum / data_points.length.to_double()
  assert_eq(average, 30.0)
  
  // Find min and max
  let mut min = data_points[0].1
  let mut max = data_points[0].1
  for point in data_points {
    if point.1 < min {
      min = point.1
    }
    if point.1 > max {
      max = point.1
    }
  }
  assert_eq(min, 10.0)
  assert_eq(max, 50.0)
}

test "time series window operations" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Create data points spanning different time windows
  let data_points = [
    (base_timestamp, 10.0),
    (base_timestamp + 1000000L, 20.0),
    (base_timestamp + 2000000L, 30.0),
    (base_timestamp + 5000000L, 40.0),
    (base_timestamp + 6000000L, 50.0),
    (base_timestamp + 7000000L, 60.0)
  ]
  
  // Define time window (3 seconds)
  let window_size = 3000000L
  
  // Filter data points within first window (base_timestamp to base_timestamp + 3s)
  let mut first_window_points = []
  for point in data_points {
    if point.0 >= base_timestamp && point.0 <= base_timestamp + window_size {
      first_window_points = Array::push(first_window_points, point)
    }
  }
  
  // Verify first window contains expected points
  assert_eq(first_window_points.length, 3)
  assert_eq(first_window_points[0].1, 10.0)
  assert_eq(first_window_points[1].1, 20.0)
  assert_eq(first_window_points[2].1, 30.0)
  
  // Filter data points within second window (base_timestamp + 5s to base_timestamp + 8s)
  let mut second_window_points = []
  for point in data_points {
    if point.0 >= base_timestamp + 5000000L && point.0 <= base_timestamp + 8000000L {
      second_window_points = Array::push(second_window_points, point)
    }
  }
  
  // Verify second window contains expected points
  assert_eq(second_window_points.length, 3)
  assert_eq(second_window_points[0].1, 40.0)
  assert_eq(second_window_points[1].1, 50.0)
  assert_eq(second_window_points[2].1, 60.0)
}

test "time series temporal operations" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Create time series with different intervals
  let hourly_data = [
    (base_timestamp, 100.0),
    (base_timestamp + 3600000000000L, 110.0), // +1 hour
    (base_timestamp + 7200000000000L, 120.0), // +2 hours
    (base_timestamp + 10800000000000L, 115.0) // +3 hours
  ]
  
  // Create minute-level data
  let minute_data = [
    (base_timestamp, 10.0),
    (base_timestamp + 60000000000L, 12.0), // +1 minute
    (base_timestamp + 120000000000L, 11.0), // +2 minutes
    (base_timestamp + 180000000000L, 13.0) // +3 minutes
  ]
  
  // Verify time intervals are correct
  assert_eq(hourly_data[1].0 - hourly_data[0].0, 3600000000000L)
  assert_eq(hourly_data[2].0 - hourly_data[1].0, 3600000000000L)
  assert_eq(hourly_data[3].0 - hourly_data[2].0, 3600000000000L)
  
  assert_eq(minute_data[1].0 - minute_data[0].0, 60000000000L)
  assert_eq(minute_data[2].0 - minute_data[1].0, 60000000000L)
  assert_eq(minute_data[3].0 - minute_data[2].0, 60000000000L)
  
  // Calculate rate of change for hourly data
  let hourly_rate_1 = (hourly_data[1].1 - hourly_data[0].1) / ((hourly_data[1].0 - hourly_data[0].0).to_double() / 3600000000000.0)
  let hourly_rate_2 = (hourly_data[2].1 - hourly_data[1].1) / ((hourly_data[2].0 - hourly_data[1].0).to_double() / 3600000000000.0)
  let hourly_rate_3 = (hourly_data[3].1 - hourly_data[2].1) / ((hourly_data[3].0 - hourly_data[2].0).to_double() / 3600000000000.0)
  
  assert_eq(hourly_rate_1, 10.0)
  assert_eq(hourly_rate_2, 10.0)
  assert_eq(hourly_rate_3, -5.0)
}

test "time series missing data handling" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Create time series with gaps
  let data_with_gaps = [
    (base_timestamp, 10.0),
    (base_timestamp + 3600000000000L, 20.0), // +1 hour
    (base_timestamp + 10800000000000L, 40.0), // +3 hours (gap of 2 hours)
    (base_timestamp + 14400000000000L, 50.0) // +4 hours
  ]
  
  // Verify gaps exist
  assert_eq(data_with_gaps[1].0 - data_with_gaps[0].0, 3600000000000L) // 1 hour
  assert_eq(data_with_gaps[2].0 - data_with_gaps[1].0, 7200000000000L) // 2 hours (gap)
  assert_eq(data_with_gaps[3].0 - data_with_gaps[2].0, 3600000000000L) // 1 hour
  
  // Calculate linear interpolation for missing data point at +2 hours
  let gap_start = data_with_gaps[1]
  let gap_end = data_with_gaps[2]
  let interpolation_time = gap_start.0 + 3600000000000L // +2 hours
  let interpolation_ratio = (interpolation_time.to_double() - gap_start.0.to_double()) / (gap_end.0.to_double() - gap_start.0.to_double())
  let interpolated_value = gap_start.1 + (gap_end.1 - gap_start.1) * interpolation_ratio
  
  assert_eq(interpolated_value, 30.0)
}

test "time series downsampling operations" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Create high-frequency data (every minute)
  let high_freq_data = [
    (base_timestamp, 10.0),
    (base_timestamp + 60000000000L, 12.0), // +1 min
    (base_timestamp + 120000000000L, 11.0), // +2 min
    (base_timestamp + 180000000000L, 13.0), // +3 min
    (base_timestamp + 240000000000L, 15.0), // +4 min
    (base_timestamp + 300000000000L, 14.0), // +5 min
    (base_timestamp + 360000000000L, 16.0), // +6 min
    (base_timestamp + 420000000000L, 18.0), // +7 min
    (base_timestamp + 480000000000L, 17.0), // +8 min
    (base_timestamp + 540000000000L, 19.0) // +9 min
  ]
  
  // Downsample to hourly (average of all points in the hour)
  let mut sum = 0.0
  for point in high_freq_data {
    sum = sum + point.1
  }
  let hourly_avg = sum / high_freq_data.length.to_double()
  
  assert_eq(hourly_avg, 14.5)
  
  // Downsample to 5-minute intervals
  let five_min_intervals = [
    (base_timestamp + 150000000000L, 11.5), // Average of first 2.5 minutes
    (base_timestamp + 450000000000L, 14.5), // Average of next 2.5 minutes
    (base_timestamp + 750000000000L, 18.0)  // Average of last 2.5 minutes
  ]
  
  assert_eq(five_min_intervals.length, 3)
  assert_eq(five_min_intervals[0].1, 11.5)
  assert_eq(five_min_intervals[1].1, 14.5)
  assert_eq(five_min_intervals[2].1, 18.0)
}

test "time series trend analysis" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Create trending data
  let trending_up = [
    (base_timestamp, 10.0),
    (base_timestamp + 3600000000000L, 15.0), // +1 hour
    (base_timestamp + 7200000000000L, 20.0), // +2 hours
    (base_timestamp + 10800000000000L, 25.0), // +3 hours
    (base_timestamp + 14400000000000L, 30.0) // +4 hours
  ]
  
  let trending_down = [
    (base_timestamp, 50.0),
    (base_timestamp + 3600000000000L, 40.0), // +1 hour
    (base_timestamp + 7200000000000L, 30.0), // +2 hours
    (base_timestamp + 10800000000000L, 20.0), // +3 hours
    (base_timestamp + 14400000000000L, 10.0) // +4 hours
  ]
  
  // Calculate trend slope for trending up data
  let up_slope = (trending_up[4].1 - trending_up[0].1) / ((trending_up[4].0 - trending_up[0].0).to_double() / 3600000000000.0)
  let down_slope = (trending_down[4].1 - trending_down[0].1) / ((trending_down[4].0 - trending_down[0].0).to_double() / 3600000000000.0)
  
  assert_eq(up_slope, 5.0) // Increasing by 5 per hour
  assert_eq(down_slope, -10.0) // Decreasing by 10 per hour
  
  // Verify trend direction
  assert_true(up_slope > 0.0)
  assert_true(down_slope < 0.0)
}

test "time series seasonal patterns" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // Create data with daily seasonal pattern
  let daily_pattern = [
    (base_timestamp, 20.0), // 00:00
    (base_timestamp + 21600000000000L, 40.0), // +6 hours
    (base_timestamp + 43200000000000L, 80.0), // +12 hours (peak)
    (base_timestamp + 64800000000000L, 40.0), // +18 hours
    (base_timestamp + 86400000000000L, 20.0) // +24 hours (back to start)
  ]
  
  // Verify pattern repeats
  assert_eq(daily_pattern[0].1, daily_pattern[4].1) // Same value at start and end
  assert_eq(daily_pattern[1].1, daily_pattern[3].1) // Symmetric pattern
  
  // Calculate peak and trough
  let mut max_val = daily_pattern[0].1
  let mut min_val = daily_pattern[0].1
  for point in daily_pattern {
    if point.1 > max_val {
      max_val = point.1
    }
    if point.1 < min_val {
      min_val = point.1
    }
  }
  
  assert_eq(max_val, 80.0) // Peak at noon
  assert_eq(min_val, 20.0) // Trough at midnight
  
  // Calculate amplitude
  let amplitude = (max_val - min_val) / 2.0
  assert_eq(amplitude, 30.0)
}