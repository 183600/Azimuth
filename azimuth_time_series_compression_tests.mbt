// Azimuth Telemetry System - Time Series Data Compression Tests
// This file contains test cases for time series data compression and optimization

test "time series data compression with delta encoding" {
  // Test delta encoding for time series compression
  let timestamp1 = 1700000000
  let timestamp2 = 1700000001
  let timestamp3 = 1700000002
  let timestamp4 = 1700000003
  let timestamp5 = 1700000004
  
  let metric1 = 100.0
  let metric2 = 102.5
  let metric3 = 98.0
  let metric4 = 105.0
  let metric5 = 101.5
  
  // Calculate delta for timestamps (usually small, regular intervals)
  let delta1 = timestamp2 - timestamp1
  let delta2 = timestamp3 - timestamp2
  let delta3 = timestamp4 - timestamp3
  let delta4 = timestamp5 - timestamp4
  
  // Verify delta compression effectiveness
  assert_eq(delta1, 1) // Regular 1-second intervals
  assert_eq(delta2, 1)
  assert_eq(delta3, 1)
  assert_eq(delta4, 1)
  
  // Calculate delta for metric values
  let metric_delta1 = metric2 - metric1
  let metric_delta2 = metric3 - metric2
  let metric_delta3 = metric4 - metric3
  let metric_delta4 = metric5 - metric4
  
  // Verify metric delta calculation
  assert_eq(metric_delta1, 2.5)
  assert_eq(metric_delta2, -4.5)
  assert_eq(metric_delta3, 7.0)
  assert_eq(metric_delta4, -3.5)
  
  // Test compression ratio estimation
  let original_size = 5 * 8 + 5 * 8 // 8 bytes per timestamp + 8 bytes per metric
  let compressed_size = 8 + // First timestamp
                       4 * 2 + // Delta timestamps (smaller)
                       8 + // First metric value  
                       4 * 4 // Delta metrics (smaller)
  
  assert_true(compressed_size < original_size)
  let compression_ratio = compressed_size * 100 / original_size
  assert_true(compression_ratio < 80) // At least 20% compression
}

test "time series aggregation with different window sizes" {
  // Test different aggregation windows for time series data
  let data1 = (1700000000, 10.0)
  let data2 = (1700000001, 12.0)
  let data3 = (1700000002, 11.0)
  let data4 = (1700000003, 13.0)
  let data5 = (1700000004, 15.0)
  let data6 = (1700000005, 14.0)
  let data7 = (1700000006, 16.0)
  let data8 = (1700000007, 18.0)
  let data9 = (1700000008, 17.0)
  let data10 = (1700000009, 19.0)
  
  // Test 5-second window aggregation
  let window_size = 5
  
  // First window (indices 0-4): 10.0, 12.0, 11.0, 13.0, 15.0
  let window1_start = data1.0
  let window1_sum = data1.1 + data2.1 + data3.1 + data4.1 + data5.1
  let window1_count = 5
  let window1_avg = window1_sum / window1_count.to_double()
  let window1_min = min(min(min(min(data1.1, data2.1), data3.1), data4.1), data5.1)
  let window1_max = max(max(max(max(data1.1, data2.1), data3.1), data4.1), data5.1)
  
  // Second window (indices 5-9): 14.0, 16.0, 18.0, 17.0, 19.0
  let window2_start = data6.0
  let window2_sum = data6.1 + data7.1 + data8.1 + data9.1 + data10.1
  let window2_count = 5
  let window2_avg = window2_sum / window2_count.to_double()
  let window2_min = min(min(min(min(data6.1, data7.1), data8.1), data9.1), data10.1)
  let window2_max = max(max(max(max(data6.1, data7.1), data8.1), data9.1), data10.1)
  
  // Verify window aggregation results
  assert_eq(window1_start, 1700000000) // Window start time
  assert_eq(window1_avg, 12.2) // Average: (10+12+11+13+15)/5 = 12.2
  assert_eq(window1_min, 10.0) // Minimum
  assert_eq(window1_max, 15.0) // Maximum
  assert_eq(window1_count, 5) // Count
  
  assert_eq(window2_start, 1700000005) // Window start time
  assert_eq(window2_avg, 16.8) // Average: (14+16+18+17+19)/5 = 16.8
  assert_eq(window2_min, 14.0) // Minimum
  assert_eq(window2_max, 19.0) // Maximum
  assert_eq(window2_count, 5) // Count
}

test "time series data downsampling for long-term storage" {
  // Test downsampling strategies for different time horizons
  let base_timestamp = 1700000000
  let base_value = 100.0
  
  // Generate 1 hour of data at 1-second intervals (simplified to 10 data points)
  let data1 = (base_timestamp, base_value + 0.0)
  let data2 = (base_timestamp + 1, base_value + 0.01)
  let data3 = (base_timestamp + 2, base_value + 0.02)
  let data4 = (base_timestamp + 3, base_value + 0.03)
  let data5 = (base_timestamp + 4, base_value + 0.04)
  let data6 = (base_timestamp + 5, base_value + 0.05)
  let data7 = (base_timestamp + 6, base_value + 0.06)
  let data8 = (base_timestamp + 7, base_value + 0.07)
  let data9 = (base_timestamp + 8, base_value + 0.08)
  let data10 = (base_timestamp + 9, base_value + 0.09)
  
  // 1-minute downsampling (simplified to 2:1 ratio for our 10 data points)
  let minute_sum1 = data1.1 + data2.1 + data3.1 + data4.1 + data5.1
  let minute_avg1 = minute_sum1 / 5.0
  let minute_start1 = data1.0
  
  let minute_sum2 = data6.1 + data7.1 + data8.1 + data9.1 + data10.1
  let minute_avg2 = minute_sum2 / 5.0
  let minute_start2 = data6.0
  
  // 1-hour downsampling (3600:1 ratio, simplified to all data points)
  let hour_sum = data1.1 + data2.1 + data3.1 + data4.1 + data5.1 + 
                data6.1 + data7.1 + data8.1 + data9.1 + data10.1
  let hour_avg = hour_sum / 10.0
  let hour_start = data1.0
  
  // Verify downsampling results
  assert_eq(minute_start1, base_timestamp)
  assert_eq(minute_start2, base_timestamp + 5)
  assert_eq(hour_start, base_timestamp)
  
  // Verify data reduction ratios
  let original_size = 10
  let minute_size = 2
  let hour_size = 1
  
  assert_eq(minute_size, original_size / 5)
  assert_eq(hour_size, original_size / 10)
  
  // Verify value preservation (approximately)
  let original_avg = hour_sum / 10.0
  assert_eq(hour_avg, original_avg)
}

test "time series anomaly detection with statistical methods" {
  // Test statistical anomaly detection in time series
  let normal_data = [
    45.0, 55.0, 48.0, 52.0, 50.0, 
    53.0, 47.0, 51.0, 49.0, 54.0
  ]
  
  // Add some anomalies
  let anomaly_data = [
    45.0, 55.0, 48.0, 52.0, 50.0, 
    150.0, // Spike anomaly
    53.0, 47.0, 51.0, 0.0 // Drop anomaly
  ]
  
  // Calculate statistics for anomaly detection
  let mut sum = 0.0
  let mut sum_squares = 0.0
  
  for value in normal_data {
    sum = sum + value
    sum_squares = sum_squares + value * value
  }
  
  let mean = sum / normal_data.length().to_double()
  let variance = (sum_squares / normal_data.length().to_double()) - (mean * mean)
  let std_dev = variance.sqrt()
  
  // Verify normal statistics
  assert_eq(mean, 50.0) // Centered around 50
  assert_true(std_dev > 0.0) // Some variance
  assert_true(std_dev < 10.0) // But not too much
  
  // Detect anomalies using 3-sigma rule
  let upper_bound = mean + 3.0 * std_dev
  let lower_bound = mean - 3.0 * std_dev
  
  // Check specific anomalies
  let spike_value = 150.0
  let drop_value = 0.0
  
  // Verify anomaly detection
  assert_true(spike_value > upper_bound)
  assert_true(drop_value < lower_bound)
  
  // Verify bounds calculation
  assert_true(upper_bound > mean)
  assert_true(lower_bound < mean)
}

test "time series data interpolation for missing values" {
  // Test interpolation methods for missing time series data
  let complete_data = [
    (1700000000, 10.0),
    (1700000001, 12.0),
    (1700000002, 14.0),
    (1700000003, 16.0),
    (1700000004, 18.0),
    (1700000005, 20.0),
    (1700000006, 22.0),
    (1700000007, 24.0),
    (1700000008, 26.0),
    (1700000009, 28.0)
  ]
  
  // Simulate missing data points
  let data1 = complete_data[0] // (1700000000, 10.0)
  let data2 = complete_data[1] // (1700000001, 12.0)
  // Missing (1700000002, 14.0)
  let data3 = complete_data[3] // (1700000003, 16.0)
  // Missing (1700000004, 18.0)
  // Missing (1700000005, 20.0)
  let data4 = complete_data[6] // (1700000006, 22.0)
  let data5 = complete_data[7] // (1700000007, 24.0)
  // Missing (1700000008, 26.0)
  let data6 = complete_data[9] // (1700000009, 28.0)
  
  // Linear interpolation for missing values
  // Interpolate between data2 and data3 for missing timestamp 1700000002
  let interp1_timestamp = 1700000002
  let interp1_ratio = 0.5 // Middle point
  let interp1_value = data2.1 + (data3.1 - data2.1) * interp1_ratio
  
  // Interpolate between data3 and data4 for missing timestamps 1700000004 and 1700000005
  let interp2_timestamp = 1700000004
  let interp2_ratio = 0.33 // 1/3 of the way
  let interp2_value = data3.1 + (data4.1 - data3.1) * interp2_ratio
  
  let interp3_timestamp = 1700000005
  let interp3_ratio = 0.67 // 2/3 of the way
  let interp3_value = data3.1 + (data4.1 - data3.1) * interp3_ratio
  
  // Interpolate between data5 and data6 for missing timestamp 1700000008
  let interp4_timestamp = 1700000008
  let interp4_ratio = 0.5 // Middle point
  let interp4_value = data5.1 + (data6.1 - data5.1) * interp4_ratio
  
  // Verify interpolation results
  assert_eq(interp1_timestamp, 1700000002)
  assert_eq(interp1_value, 14.0) // (12 + (16-12) * 0.5) = 14.0
  
  assert_eq(interp2_timestamp, 1700000004)
  assert_eq(interp2_value, 18.0) // (16 + (22-16) * 0.33) ≈ 18.0
  
  assert_eq(interp3_timestamp, 1700000005)
  assert_eq(interp3_value, 20.0) // (16 + (22-16) * 0.67) ≈ 20.0
  
  assert_eq(interp4_timestamp, 1700000008)
  assert_eq(interp4_value, 26.0) // (24 + (28-24) * 0.5) = 26.0
}