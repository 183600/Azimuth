// Azimuth Time Series Operations Comprehensive Test Suite
// This file contains comprehensive test cases for time series operations

// Test 1: Timestamp operations and consistency
pub test "timestamp operations and consistency" {
  // Test clock operations
  let system_clock = azimuth::Clock::system()
  let current_time = azimuth::Clock::now_unix_nanos(system_clock)
  
  // Verify timestamp is in reasonable range (2025-01-01 is around 1735689600000000000 nanoseconds from epoch)
  assert_true(current_time >= 1735689600000000000L)
  assert_true(current_time <= 4102444800000000000L)  // Around year 2100
  
  // Test timestamp consistency across multiple operations
  let time1 = azimuth::Clock::now_unix_nanos(system_clock)
  let time2 = azimuth::Clock::now_unix_nanos(system_clock)
  let time3 = azimuth::Clock::now_unix_nanos(system_clock)
  
  // Timestamps should be monotonically increasing (or at least not decreasing)
  assert_true(time1 <= time2)
  assert_true(time2 <= time3)
  
  // Test timestamp arithmetic operations
  let base_time = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let one_second_nanos = 1000000000L
  let one_minute_nanos = 60 * one_second_nanos
  let one_hour_nanos = 60 * one_minute_nanos
  let one_day_nanos = 24 * one_hour_nanos
  
  let time_plus_1s = base_time + one_second_nanos
  let time_plus_1m = base_time + one_minute_nanos
  let time_plus_1h = base_time + one_hour_nanos
  let time_plus_1d = base_time + one_day_nanos
  
  // Verify timestamp arithmetic
  assert_eq(time_plus_1s - base_time, one_second_nanos)
  assert_eq(time_plus_1m - base_time, one_minute_nanos)
  assert_eq(time_plus_1h - base_time, one_hour_nanos)
  assert_eq(time_plus_1d - base_time, one_day_nanos)
  
  // Test timestamp conversion operations
  let timestamp_seconds = base_time / 1000000000L
  let timestamp_millis = base_time / 1000000L
  let timestamp_micros = base_time / 1000L
  
  // Verify timestamp conversions
  assert_eq(timestamp_seconds * 1000000000L, base_time)
  assert_eq(timestamp_millis * 1000000L, base_time)
  assert_eq(timestamp_micros * 1000L, base_time)
  
  // Test timestamp with extreme values
  let min_timestamp = 0L  // Epoch start
  let max_timestamp = 9223372036854775807L  // Max Int64
  
  // Verify extreme timestamps
  assert_true(min_timestamp >= 0L)
  assert_true(max_timestamp > min_timestamp)
}

// Test 2: Time window operations
pub test "time window operations" {
  // Test time window calculations
  let base_time = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let one_second_nanos = 1000000000L
  let one_minute_nanos = 60 * one_second_nanos
  let one_hour_nanos = 60 * one_minute_nanos
  let one_day_nanos = 24 * one_hour_nanos
  
  // Test 1-second time windows
  let window_1s_start = base_time
  let window_1s_end = base_time + one_second_nanos
  
  // Test 1-minute time windows
  let window_1m_start = base_time
  let window_1m_end = base_time + one_minute_nanos
  
  // Test 1-hour time windows
  let window_1h_start = base_time
  let window_1h_end = base_time + one_hour_nanos
  
  // Test 1-day time windows
  let window_1d_start = base_time
  let window_1d_end = base_time + one_day_nanos
  
  // Verify time window boundaries
  assert_eq(window_1s_end - window_1s_start, one_second_nanos)
  assert_eq(window_1m_end - window_1m_start, one_minute_nanos)
  assert_eq(window_1h_end - window_1h_start, one_hour_nanos)
  assert_eq(window_1d_end - window_1d_start, one_day_nanos)
  
  // Test time window alignment
  let unaligned_time = base_time + 1234567890L  // Some arbitrary offset
  let aligned_1s = (unaligned_time / one_second_nanos) * one_second_nanos
  let aligned_1m = (unaligned_time / one_minute_nanos) * one_minute_nanos
  let aligned_1h = (unaligned_time / one_hour_nanos) * one_hour_nanos
  let aligned_1d = (unaligned_time / one_day_nanos) * one_day_nanos
  
  // Verify time window alignment
  assert_true(aligned_1s <= unaligned_time)
  assert_true(aligned_1s + one_second_nanos > unaligned_time)
  
  assert_true(aligned_1m <= unaligned_time)
  assert_true(aligned_1m + one_minute_nanos > unaligned_time)
  
  assert_true(aligned_1h <= unaligned_time)
  assert_true(aligned_1h + one_hour_nanos > unaligned_time)
  
  assert_true(aligned_1d <= unaligned_time)
  assert_true(aligned_1d + one_day_nanos > unaligned_time)
  
  // Test time window containment
  let test_time = base_time + 30 * one_second_nanos  // 30 seconds after base time
  
  // Verify time window containment
  assert_true(test_time >= window_1s_start && test_time < window_1s_end)
  assert_true(test_time >= window_1m_start && test_time < window_1m_end)
  assert_true(test_time >= window_1h_start && test_time < window_1h_end)
  assert_true(test_time >= window_1d_start && test_time < window_1d_end)
  
  // Test time window overlap
  let window_a_start = base_time
  let window_a_end = base_time + one_hour_nanos
  
  let window_b_start = base_time + 30 * one_minute_nanos  // 30 minutes after base time
  let window_b_end = base_time + 90 * one_minute_nanos  // 90 minutes after base time
  
  // Verify time window overlap
  assert_true(window_b_start < window_a_end)  // Overlap exists
  assert_true(window_a_start < window_b_end)  // Overlap exists
  
  let overlap_start = window_b_start
  let overlap_end = window_a_end
  
  assert_eq(overlap_end - overlap_start, 30 * one_minute_nanos)  // 30 minutes overlap
}

// Test 3: Time series aggregation operations
pub test "time series aggregation operations" {
  // Create time series data points
  let base_time = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let one_second_nanos = 1000000000L
  
  // Create a series of data points with timestamps and values
  let data_points = [
    (base_time + 0 * one_second_nanos, 10.0),
    (base_time + 1 * one_second_nanos, 20.0),
    (base_time + 2 * one_second_nanos, 30.0),
    (base_time + 3 * one_second_nanos, 40.0),
    (base_time + 4 * one_second_nanos, 50.0),
    (base_time + 5 * one_second_nanos, 60.0),
    (base_time + 6 * one_second_nanos, 70.0),
    (base_time + 7 * one_second_nanos, 80.0),
    (base_time + 8 * one_second_nanos, 90.0),
    (base_time + 9 * one_second_nanos, 100.0)
  ]
  
  // Test sum aggregation
  let mut sum = 0.0
  for (_, value) in data_points {
    sum = sum + value
  }
  assert_eq(sum, 550.0)
  
  // Test average aggregation
  let average = sum / data_points.length.to_double()
  assert_eq(average, 55.0)
  
  // Test min aggregation
  let mut min = data_points[0].1
  for (_, value) in data_points {
    if value < min {
      min = value
    }
  }
  assert_eq(min, 10.0)
  
  // Test max aggregation
  let mut max = data_points[0].1
  for (_, value) in data_points {
    if value > max {
      max = value
    }
  }
  assert_eq(max, 100.0)
  
  // Test count aggregation
  let count = data_points.length.to_double()
  assert_eq(count, 10.0)
  
  // Test time-weighted average aggregation
  let mut weighted_sum = 0.0
  let mut total_weight = 0.0
  
  for i = 0; i < data_points.length - 1; i++ {
    let (time1, value1) = data_points[i]
    let (time2, value2) = data_points[i + 1]
    
    let duration = (time2 - time1).to_double()
    let avg_value = (value1 + value2) / 2.0
    
    weighted_sum = weighted_sum + avg_value * duration
    total_weight = total_weight + duration
  }
  
  let weighted_average = weighted_sum / total_weight
  assert_eq(weighted_average, 55.0)  // Should be the same as simple average for linear data
  
  // Test percentile aggregation (approximate)
  let sorted_values = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  
  // 50th percentile (median)
  let p50_index = (sorted_values.length * 50) / 100
  let p50_value = if p50_index < sorted_values.length { sorted_values[p50_index] } else { sorted_values[sorted_values.length - 1] }
  assert_eq(p50_value, 50.0)
  
  // 95th percentile
  let p95_index = (sorted_values.length * 95) / 100
  let p95_value = if p95_index < sorted_values.length { sorted_values[p95_index] } else { sorted_values[sorted_values.length - 1] }
  assert_eq(p95_value, 100.0)
  
  // 99th percentile
  let p99_index = (sorted_values.length * 99) / 100
  let p99_value = if p99_index < sorted_values.length { sorted_values[p99_index] } else { sorted_values[sorted_values.length - 1] }
  assert_eq(p99_value, 100.0)
}

// Test 4: Time series downsampling operations
pub test "time series downsampling operations" {
  // Create high-frequency time series data
  let base_time = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let one_second_nanos = 1000000000L
  
  // Create data points every second for 10 minutes
  let mut high_freq_data = []: Array[(Int64, Double)]
  
  for i = 0; i < 600; i++ {  // 10 minutes = 600 seconds
    let timestamp = base_time + i.to_int64() * one_second_nanos
    let value = 50.0 + 10.0 * ((2.0 * 3.141592653589793 * i.to_double() / 60.0).sin())  // Sine wave with period 1 minute
    high_freq_data = high_freq_data.concat([(timestamp, value)])
  }
  
  // Test downsampling to 1-minute intervals
  let one_minute_nanos = 60 * one_second_nanos
  let mut downsampled_1m = []: Array[(Int64, Double)]
  
  // Group data by 1-minute windows
  let mut window_start = base_time
  while window_start < base_time + 600 * one_second_nanos {
    let window_end = window_start + one_minute_nanos
    
    // Find data points in this window
    let mut window_values = []: Array[Double]
    for (timestamp, value) in high_freq_data {
      if timestamp >= window_start && timestamp < window_end {
        window_values = window_values.concat([value])
      }
    }
    
    // Calculate average for this window
    if window_values.length > 0 {
      let mut sum = 0.0
      for value in window_values {
        sum = sum + value
      }
      let average = sum / window_values.length.to_double()
      downsampled_1m = downsampled_1m.concat([(window_start, average)])
    }
    
    window_start = window_end
  }
  
  // Verify downsampling results
  assert_eq(downsampled_1m.length, 10)  // Should have 10 1-minute windows
  
  // First window should have average close to 50.0 (sine wave starts at sin(0) = 0)
  assert_true((downsampled_1m[0].1 - 50.0).abs() < 1.0)
  
  // Test downsampling to 5-minute intervals
  let five_minute_nanos = 5 * one_minute_nanos
  let mut downsampled_5m = []: Array[(Int64, Double)]
  
  // Group data by 5-minute windows
  window_start = base_time
  while window_start < base_time + 600 * one_second_nanos {
    let window_end = window_start + five_minute_nanos
    
    // Find data points in this window
    let mut window_values = []: Array[Double]
    for (timestamp, value) in high_freq_data {
      if timestamp >= window_start && timestamp < window_end {
        window_values = window_values.concat([value])
      }
    }
    
    // Calculate average for this window
    if window_values.length > 0 {
      let mut sum = 0.0
      for value in window_values {
        sum = sum + value
      }
      let average = sum / window_values.length.to_double()
      downsampled_5m = downsampled_5m.concat([(window_start, average)])
    }
    
    window_start = window_end
  }
  
  // Verify downsampling results
  assert_eq(downsampled_5m.length, 2)  // Should have 2 5-minute windows
  
  // Test downsampling with different aggregation functions
  // Max downsampling
  let mut downsampled_max = []: Array[(Int64, Double)]
  
  window_start = base_time
  while window_start < base_time + 600 * one_second_nanos {
    let window_end = window_start + one_minute_nanos
    
    // Find data points in this window
    let mut window_values = []: Array[Double]
    for (timestamp, value) in high_freq_data {
      if timestamp >= window_start && timestamp < window_end {
        window_values = window_values.concat([value])
      }
    }
    
    // Calculate max for this window
    if window_values.length > 0 {
      let mut max = window_values[0]
      for value in window_values {
        if value > max {
          max = value
        }
      }
      downsampled_max = downsampled_max.concat([(window_start, max)])
    }
    
    window_start = window_end
  }
  
  // Verify max downsampling results
  assert_eq(downsampled_max.length, 10)
  
  // Min downsampling
  let mut downsampled_min = []: Array[(Int64, Double)]
  
  window_start = base_time
  while window_start < base_time + 600 * one_second_nanos {
    let window_end = window_start + one_minute_nanos
    
    // Find data points in this window
    let mut window_values = []: Array[Double]
    for (timestamp, value) in high_freq_data {
      if timestamp >= window_start && timestamp < window_end {
        window_values = window_values.concat([value])
      }
    }
    
    // Calculate min for this window
    if window_values.length > 0 {
      let mut min = window_values[0]
      for value in window_values {
        if value < min {
          min = value
        }
      }
      downsampled_min = downsampled_min.concat([(window_start, min)])
    }
    
    window_start = window_end
  }
  
  // Verify min downsampling results
  assert_eq(downsampled_min.length, 10)
}

// Test 5: Time series rate calculations
pub test "time series rate calculations" {
  // Create time series data with counters
  let base_time = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let one_second_nanos = 1000000000L
  
  // Create counter data points
  let counter_data = [
    (base_time + 0 * one_second_nanos, 100.0),
    (base_time + 10 * one_second_nanos, 200.0),
    (base_time + 20 * one_second_nanos, 350.0),
    (base_time + 30 * one_second_nanos, 500.0),
    (base_time + 40 * one_second_nanos, 700.0),
    (base_time + 50 * one_second_nanos, 950.0),
    (base_time + 60 * one_second_nanos, 1200.0)
  ]
  
  // Calculate rate between consecutive points
  let mut rates = []: Array[(Int64, Double)]
  
  for i = 0; i < counter_data.length - 1; i++ {
    let (time1, value1) = counter_data[i]
    let (time2, value2) = counter_data[i + 1]
    
    let time_diff = (time2 - time1).to_double() / 1000000000.0  // Convert to seconds
    let value_diff = value2 - value1
    let rate = value_diff / time_diff
    
    rates = rates.concat([(time2, rate)])
  }
  
  // Verify rate calculations
  assert_eq(rates.length, 6)
  
  // First rate: (200 - 100) / 10 = 10.0
  assert_eq(rates[0].1, 10.0)
  
  // Second rate: (350 - 200) / 10 = 15.0
  assert_eq(rates[1].1, 15.0)
  
  // Third rate: (500 - 350) / 10 = 15.0
  assert_eq(rates[2].1, 15.0)
  
  // Fourth rate: (700 - 500) / 10 = 20.0
  assert_eq(rates[3].1, 20.0)
  
  // Fifth rate: (950 - 700) / 10 = 25.0
  assert_eq(rates[4].1, 25.0)
  
  // Sixth rate: (1200 - 950) / 10 = 25.0
  assert_eq(rates[5].1, 25.0)
  
  // Test moving average of rates
  let window_size = 3
  let mut moving_avg_rates = []: Array[Double]
  
  for i = window_size - 1; i < rates.length; i++ {
    let mut sum = 0.0
    for j = i - window_size + 1; j <= i; j++ {
      sum = sum + rates[j].1
    }
    let avg = sum / window_size.to_double()
    moving_avg_rates = moving_avg_rates.concat([avg])
  }
  
  // Verify moving average calculations
  assert_eq(moving_avg_rates.length, 4)
  
  // First moving average: (10.0 + 15.0 + 15.0) / 3 = 13.33
  assert_true((moving_avg_rates[0] - 13.33).abs() < 0.01)
  
  // Second moving average: (15.0 + 15.0 + 20.0) / 3 = 16.67
  assert_true((moving_avg_rates[1] - 16.67).abs() < 0.01)
  
  // Test exponential moving average of rates
  let alpha = 0.2  // Smoothing factor
  let mut ema_rates = []: Array[Double]
  
  if rates.length > 0 {
    let mut ema = rates[0].1
    ema_rates = ema_rates.concat([ema])
    
    for i = 1; i < rates.length; i++ {
      ema = alpha * rates[i].1 + (1.0 - alpha) * ema
      ema_rates = ema_rates.concat([ema])
    }
  }
  
  // Verify exponential moving average calculations
  assert_eq(ema_rates.length, 6)
  assert_eq(ema_rates[0], 10.0)  // First EMA is the first value
  
  // Second EMA: 0.2 * 15.0 + 0.8 * 10.0 = 11.0
  assert_eq(ema_rates[1], 11.0)
  
  // Third EMA: 0.2 * 15.0 + 0.8 * 11.0 = 11.8
  assert_eq(ema_rates[2], 11.8)
}

// Test 6: Time series interpolation operations
pub test "time series interpolation operations" {
  // Create sparse time series data
  let base_time = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let one_minute_nanos = 60 * 1000000000L
  
  // Create sparse data points
  let sparse_data = [
    (base_time + 0 * one_minute_nanos, 10.0),
    (base_time + 5 * one_minute_nanos, 30.0),
    (base_time + 10 * one_minute_nanos, 20.0),
    (base_time + 15 * one_minute_nanos, 40.0),
    (base_time + 20 * one_minute_nanos, 25.0)
  ]
  
  // Test linear interpolation
  let interpolation_interval = one_minute_nanos
  let mut interpolated_data = []: Array[(Int64, Double)]
  
  for i = 0; i < sparse_data.length - 1; i++ {
    let (time1, value1) = sparse_data[i]
    let (time2, value2) = sparse_data[i + 1]
    
    // Add the starting point
    interpolated_data = interpolated_data.concat([(time1, value1)])
    
    // Interpolate between points
    let mut current_time = time1 + interpolation_interval
    while current_time < time2 {
      let ratio = (current_time - time1).to_double() / (time2 - time1).to_double()
      let interpolated_value = value1 + ratio * (value2 - value1)
      interpolated_data = interpolated_data.concat([(current_time, interpolated_value)])
      current_time = current_time + interpolation_interval
    }
  }
  
  // Add the last point
  interpolated_data = interpolated_data.concat([sparse_data[sparse_data.length - 1]])
  
  // Verify interpolation results
  assert_eq(interpolated_data.length, 21)  // 5 original points + 16 interpolated points
  
  // First interpolated point (at 1 minute): 10 + (1/5) * (30 - 10) = 14.0
  assert_eq(interpolated_data[1].1, 14.0)
  
  // Second interpolated point (at 2 minutes): 10 + (2/5) * (30 - 10) = 18.0
  assert_eq(interpolated_data[2].1, 18.0)
  
  // Test forward fill interpolation
  let mut forward_fill_data = []: Array[(Int64, Double)]
  
  // Create regular time grid
  let mut current_time = base_time
  let end_time = base_time + 20 * one_minute_nanos
  
  let mut sparse_index = 0
  let mut last_value = 0.0
  
  while current_time <= end_time {
    // Check if we have a sparse data point at this time
    if sparse_index < sparse_data.length && sparse_data[sparse_index].0 == current_time {
      last_value = sparse_data[sparse_index].1
      forward_fill_data = forward_fill_data.concat([(current_time, last_value)])
      sparse_index = sparse_index + 1
    } else {
      // Use the last known value
      if sparse_index > 0 {
        forward_fill_data = forward_fill_data.concat([(current_time, last_value)])
      }
    }
    
    current_time = current_time + interpolation_interval
  }
  
  // Verify forward fill results
  assert_eq(forward_fill_data.length, 21)
  
  // Values should be constant between sparse points
  assert_eq(forward_fill_data[1].1, 10.0)  // Forward fill from first point
  assert_eq(forward_fill_data[4].1, 10.0)  // Still forward fill from first point
  assert_eq(forward_fill_data[5].1, 30.0)  // New sparse point
  assert_eq(forward_fill_data[6].1, 30.0)  // Forward fill from new point
  
  // Test backward fill interpolation
  let mut backward_fill_data = []: Array[(Int64, Double)]
  
  // Create regular time grid in reverse
  current_time = end_time
  sparse_index = sparse_data.length - 1
  let mut next_value = 0.0
  
  while current_time >= base_time {
    // Check if we have a sparse data point at this time
    if sparse_index >= 0 && sparse_data[sparse_index].0 == current_time {
      next_value = sparse_data[sparse_index].1
      backward_fill_data = [(current_time, next_value)] + backward_fill_data
      sparse_index = sparse_index - 1
    } else {
      // Use the next known value
      if sparse_index < sparse_data.length - 1 {
        backward_fill_data = [(current_time, next_value)] + backward_fill_data
      }
    }
    
    current_time = current_time - interpolation_interval
  }
  
  // Verify backward fill results
  assert_eq(backward_fill_data.length, 21)
  
  // Values should be constant between sparse points (in reverse)
  assert_eq(backward_fill_data[19].1, 25.0)  // Backward fill from last point
  assert_eq(backward_fill_data[16].1, 25.0)  // Still backward fill from last point
  assert_eq(backward_fill_data[15].1, 40.0)  // New sparse point
  assert_eq(backward_fill_data[14].1, 40.0)  // Backward fill from new point
}

// Test 7: Time series anomaly detection
pub test "time series anomaly detection" {
  // Create time series data with anomalies
  let base_time = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let one_minute_nanos = 60 * 1000000000L
  
  // Create data with normal pattern and some anomalies
  let anomaly_data = [
    (base_time + 0 * one_minute_nanos, 50.0),   // Normal
    (base_time + 1 * one_minute_nanos, 52.0),   // Normal
    (base_time + 2 * one_minute_nanos, 48.0),   // Normal
    (base_time + 3 * one_minute_nanos, 150.0),  // Anomaly (high)
    (base_time + 4 * one_minute_nanos, 51.0),   // Normal
    (base_time + 5 * one_minute_nanos, 49.0),   // Normal
    (base_time + 6 * one_minute_nanos, -50.0),  // Anomaly (low)
    (base_time + 7 * one_minute_nanos, 53.0),   // Normal
    (base_time + 8 * one_minute_nanos, 47.0),   // Normal
    (base_time + 9 * one_minute_nanos, 52.0)    // Normal
  ]
  
  // Test statistical anomaly detection using z-score
  let mut values = []: Array[Double]
  for (_, value) in anomaly_data {
    values = values.concat([value])
  }
  
  // Calculate mean and standard deviation
  let mut sum = 0.0
  for value in values {
    sum = sum + value
  }
  let mean = sum / values.length.to_double()
  
  let mut variance_sum = 0.0
  for value in values {
    let diff = value - mean
    variance_sum = variance_sum + diff * diff
  }
  let variance = variance_sum / values.length.to_double()
  let std_dev = variance.sqrt()
  
  // Calculate z-scores
  let mut z_scores = []: Array[Double]
  for value in values {
    let z_score = (value - mean) / std_dev
    z_scores = z_scores.concat([z_score])
  }
  
  // Detect anomalies using z-score threshold
  let threshold = 2.0  // Common threshold for anomaly detection
  let mut detected_anomalies = []: Array[Int]
  
  for i = 0; i < z_scores.length; i++ {
    if z_scores[i].abs() > threshold {
      detected_anomalies = detected_anomalies.concat([i])
    }
  }
  
  // Verify anomaly detection
  assert_eq(detected_anomalies.length, 2)
  assert_true(detected_anomalies.contains(3))  // High anomaly at index 3
  assert_true(detected_anomalies.contains(6))  // Low anomaly at index 6
  
  // Test moving average anomaly detection
  let window_size = 3
  let mut anomalies = []: Array[Int]
  
  for i = window_size; i < anomaly_data.length; i++ {
    // Calculate moving average of previous window
    let mut window_sum = 0.0
    for j = i - window_size; j < i; j++ {
      window_sum = window_sum + anomaly_data[j].1
    }
    let moving_avg = window_sum / window_size.to_double()
    
    // Calculate deviation from moving average
    let current_value = anomaly_data[i].1
    let deviation = (current_value - moving_avg).abs()
    
    // Detect anomaly if deviation is large relative to moving average
    let relative_deviation = deviation / moving_avg.abs()
    if relative_deviation > 1.0 {  // 100% deviation
      anomalies = anomalies.concat([i])
    }
  }
  
  // Verify moving average anomaly detection
  assert_true(anomalies.length >= 2)
  assert_true(anomalies.contains(3))  // High anomaly at index 3
  assert_true(anomalies.contains(6))  // Low anomaly at index 6
  
  // Test seasonal anomaly detection
  // Create seasonal data with anomaly
  let seasonal_data = [
    (base_time + 0 * one_minute_nanos, 10.0),   // Base of seasonal pattern
    (base_time + 1 * one_minute_nanos, 20.0),   // Rising
    (base_time + 2 * one_minute_nanos, 30.0),   // Peak
    (base_time + 3 * one_minute_nanos, 20.0),   // Falling
    (base_time + 4 * one_minute_nanos, 10.0),   // Base
    (base_time + 5 * one_minute_nanos, 20.0),   // Rising
    (base_time + 6 * one_minute_nanos, 100.0),  // Anomaly (should be 30)
    (base_time + 7 * one_minute_nanos, 20.0),   // Falling
    (base_time + 8 * one_minute_nanos, 10.0),   // Base
    (base_time + 9 * one_minute_nanos, 20.0),   // Rising
    (base_time + 10 * one_minute_nanos, 30.0),  // Peak
    (base_time + 11 * one_minute_nanos, 20.0)   // Falling
  ]
  
  // Detect seasonal anomalies by comparing with same position in previous season
  let season_length = 5
  let mut seasonal_anomalies = []: Array[Int]
  
  for i = season_length; i < seasonal_data.length; i++ {
    let current_value = seasonal_data[i].1
    let seasonal_position = i % season_length
    let previous_season_value = seasonal_data[i - season_length].1
    
    let deviation = (current_value - previous_season_value).abs()
    let relative_deviation = deviation / previous_season_value.abs()
    
    if relative_deviation > 1.0 {  // 100% deviation from seasonal pattern
      seasonal_anomalies = seasonal_anomalies.concat([i])
    }
  }
  
  // Verify seasonal anomaly detection
  assert_eq(seasonal_anomalies.length, 1)
  assert_eq(seasonal_anomalies[0], 6)  // Anomaly at index 6
}

// Test 8: Time series forecasting
pub test "time series forecasting" {
  // Create time series data for forecasting
  let base_time = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let one_hour_nanos = 60 * 60 * 1000000000L
  
  // Create linear trend data
  let linear_data = [
    (base_time + 0 * one_hour_nanos, 10.0),
    (base_time + 1 * one_hour_nanos, 20.0),
    (base_time + 2 * one_hour_nanos, 30.0),
    (base_time + 3 * one_hour_nanos, 40.0),
    (base_time + 4 * one_hour_nanos, 50.0),
    (base_time + 5 * one_hour_nanos, 60.0),
    (base_time + 6 * one_hour_nanos, 70.0),
    (base_time + 7 * one_hour_nanos, 80.0)
  ]
  
  // Test linear trend forecasting
  // Calculate slope (trend)
  let n = linear_data.length.to_double()
  let mut sum_x = 0.0
  let mut sum_y = 0.0
  let mut sum_xy = 0.0
  let mut sum_x2 = 0.0
  
  for i = 0; i < linear_data.length; i++ {
    let x = i.to_double()
    let y = linear_data[i].1
    
    sum_x = sum_x + x
    sum_y = sum_y + y
    sum_xy = sum_xy + x * y
    sum_x2 = sum_x2 + x * x
  }
  
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  let intercept = (sum_y - slope * sum_x) / n
  
  // Verify linear trend parameters
  assert_eq(slope, 10.0)  // Should increase by 10 each hour
  assert_eq(intercept, 10.0)  // Should start at 10
  
  // Forecast next 3 points
  let mut linear_forecasts = []: Array[Double]
  
  for i = 8; i < 11; i++ {
    let forecast = slope * i.to_double() + intercept
    linear_forecasts = linear_forecasts.concat([forecast])
  }
  
  // Verify linear forecasts
  assert_eq(linear_forecasts[0], 90.0)  // Hour 8
  assert_eq(linear_forecasts[1], 100.0) // Hour 9
  assert_eq(linear_forecasts[2], 110.0) // Hour 10
  
  // Test seasonal pattern forecasting
  // Create seasonal data
  let seasonal_data = [
    (base_time + 0 * one_hour_nanos, 10.0),   // Base
    (base_time + 1 * one_hour_nanos, 20.0),   // Rising
    (base_time + 2 * one_hour_nanos, 30.0),   // Peak
    (base_time + 3 * one_hour_nanos, 20.0),   // Falling
    (base_time + 4 * one_hour_nanos, 10.0),   // Base
    (base_time + 5 * one_hour_nanos, 20.0),   // Rising
    (base_time + 6 * one_hour_nanos, 30.0),   // Peak
    (base_time + 7 * one_hour_nanos, 20.0)    // Falling
  ]
  
  // Calculate seasonal pattern (average for each position in season)
  let season_length = 4
  let mut seasonal_pattern = []: Array[Double]
  
  for i = 0; i < season_length; i++ {
    let mut sum = 0.0
    let mut count = 0
    
    for j = i; j < seasonal_data.length; j = j + season_length {
      sum = sum + seasonal_data[j].1
      count = count + 1
    }
    
    let avg = sum / count.to_double()
    seasonal_pattern = seasonal_pattern.concat([avg])
  }
  
  // Verify seasonal pattern
  assert_eq(seasonal_pattern[0], 10.0)  // Base
  assert_eq(seasonal_pattern[1], 20.0)  // Rising
  assert_eq(seasonal_pattern[2], 30.0)  // Peak
  assert_eq(seasonal_pattern[3], 20.0)  // Falling
  
  // Forecast next season using seasonal pattern
  let mut seasonal_forecasts = []: Array[Double]
  
  for i = 0; i < season_length; i++ {
    seasonal_forecasts = seasonal_forecasts.concat([seasonal_pattern[i]])
  }
  
  // Verify seasonal forecasts
  assert_eq(seasonal_forecasts[0], 10.0)  // Base
  assert_eq(seasonal_forecasts[1], 20.0)  // Rising
  assert_eq(seasonal_forecasts[2], 30.0)  // Peak
  assert_eq(seasonal_forecasts[3], 20.0)  // Falling
  
  // Test moving average forecasting
  // Create data with some noise
  let noisy_data = [
    (base_time + 0 * one_hour_nanos, 50.0),
    (base_time + 1 * one_hour_nanos, 52.0),
    (base_time + 2 * one_hour_nanos, 48.0),
    (base_time + 3 * one_hour_nanos, 55.0),
    (base_time + 4 * one_hour_nanos, 45.0),
    (base_time + 5 * one_hour_nanos, 53.0),
    (base_time + 6 * one_hour_nanos, 47.0),
    (base_time + 7 * one_hour_nanos, 51.0)
  ]
  
  // Calculate moving average forecast
  let window_size = 3
  let mut ma_forecast = 0.0
  
  // Use last window_size points for forecast
  for i = noisy_data.length - window_size; i < noisy_data.length; i++ {
    ma_forecast = ma_forecast + noisy_data[i].1
  }
  ma_forecast = ma_forecast / window_size.to_double()
  
  // Verify moving average forecast
  assert_true((ma_forecast - 50.33).abs() < 0.01)  // Average of last 3 points
}