// Azimuth Time Series Data Analysis Comprehensive Test Suite
// This file contains comprehensive tests for time series data analysis in the Azimuth telemetry system

// Test 1: Time Series Data Generation
test "time series data generation" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  // Generate time series data
  let generate_time_series = fn(name: String, start_time: Int, interval: Int, count: Int, value_fn: (Int) -> Float, tags: Array[(String, String)]) {
    let mut points = []
    
    for i in 0..count {
      let timestamp = start_time + (i * interval)
      let value = value_fn(i)
      
      points = points.push({
        timestamp,
        value,
        tags
      })
    }
    
    {
      name,
      points,
      aggregation: None
    }
  }
  
  // Test with linear function
  let linear_fn = fn(i: Int) { i.to_float() * 2.5 + 10.0 }
  let linear_series = generate_time_series(
    "cpu_usage",
    1640995200,  // Start time
    60,          // 1 minute interval
    24,          // 24 data points
    linear_fn,
    [("host", "server-1"), ("region", "us-west-1")]
  )
  
  assert_eq(linear_series.name, "cpu_usage")
  assert_eq(linear_series.points.length(), 24)
  assert_eq(linear_series.points[0].timestamp, 1640995200)
  assert_eq(linear_series.points[0].value, 10.0)
  assert_eq(linear_series.points[23].value, 10.0 + 23.0 * 2.5)
  assert_eq(linear_series.points[0].tags.length(), 2)
  assert_true(linear_series.points[0].tags.contains(("host", "server-1")))
  assert_true(linear_series.points[0].tags.contains(("region", "us-west-1")))
  
  // Test with sinusoidal function
  let sinusoidal_fn = fn(i: Int) { 
    50.0 + 20.0 * (i.to_float() * 0.5).sin() 
  }
  let sinusoidal_series = generate_time_series(
    "memory_usage",
    1640995200,
    3600,  // 1 hour interval
    24,    // 24 hours
    sinusoidal_fn,
    [("host", "server-2"), ("metric_type", "memory")]
  )
  
  assert_eq(sinusoidal_series.name, "memory_usage")
  assert_eq(sinusoidal_series.points.length(), 24)
  assert_eq(sinusoidal_series.points[0].value, 50.0 + 20.0 * (0.0).sin())
  assert_eq(sinusoidal_series.points[12].value, 50.0 + 20.0 * (6.0).sin())
  
  // Test with random function
  let random_fn = fn(i: Int) { 
    // Simple pseudo-random function based on index
    let seed = i * 1103515245 + 12345
    (seed % 1000).to_float() / 10.0  // Values between 0 and 100
  }
  let random_series = generate_time_series(
    "request_rate",
    1640995200,
    300,  // 5 minute interval
    48,   // 48 data points
    random_fn,
    [("service", "api-gateway")]
  )
  
  assert_eq(random_series.name, "request_rate")
  assert_eq(random_series.points.length(), 48)
  assert_true(random_series.points[0].value >= 0.0 && random_series.points[0].value <= 100.0)
  assert_true(random_series.points[47].value >= 0.0 && random_series.points[47].value <= 100.0)
}

// Test 2: Time Series Aggregation
test "time series aggregation" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  // Create test time series
  let create_test_series = fn() {
    let mut points = []
    
    for i in 0..24 {
      points = points.push({
        timestamp: 1640995200 + i * 3600,  // Hourly data
        value: 10.0 + i.to_float() * 2.5 + (i % 3).to_float() * 5.0,  // Linear with some variation
        tags: [("host", "server-1"), ("region", "us-west-1")]
      })
    }
    
    {
      name: "cpu_usage",
      points,
      aggregation: None
    }
  }
  
  let test_series = create_test_series()
  assert_eq(test_series.points.length(), 24)
  
  // Aggregation functions
  let aggregate_by_average = fn(series: TimeSeries, window_size: Int) {
    let mut aggregated_points = []
    
    for i in 0..(series.points.length() / window_size) {
      let mut sum = 0.0
      let mut count = 0
      
      for j in 0..window_size {
        let index = i * window_size + j
        if index < series.points.length() {
          sum = sum + series.points[index].value
          count = count + 1
        }
      }
      
      if count > 0 {
        aggregated_points = aggregated_points.push({
          timestamp: series.points[i * window_size].timestamp,
          value: sum / count.to_float(),
          tags: series.points[i * window_size].tags
        })
      }
    }
    
    {
      name: series.name + "_avg",
      points: aggregated_points,
      aggregation: Some("average")
    }
  }
  
  let aggregate_by_sum = fn(series: TimeSeries, window_size: Int) {
    let mut aggregated_points = []
    
    for i in 0..(series.points.length() / window_size) {
      let mut sum = 0.0
      
      for j in 0..window_size {
        let index = i * window_size + j
        if index < series.points.length() {
          sum = sum + series.points[index].value
        }
      }
      
      aggregated_points = aggregated_points.push({
        timestamp: series.points[i * window_size].timestamp,
        value: sum,
        tags: series.points[i * window_size].tags
      })
    }
    
    {
      name: series.name + "_sum",
      points: aggregated_points,
      aggregation: Some("sum")
    }
  }
  
  let aggregate_by_max = fn(series: TimeSeries, window_size: Int) {
    let mut aggregated_points = []
    
    for i in 0..(series.points.length() / window_size) {
      let mut max_value = -Float::infinity()
      
      for j in 0..window_size {
        let index = i * window_size + j
        if index < series.points.length() {
          let value = series.points[index].value
          if value > max_value {
            max_value = value
          }
        }
      }
      
      aggregated_points = aggregated_points.push({
        timestamp: series.points[i * window_size].timestamp,
        value: max_value,
        tags: series.points[i * window_size].tags
      })
    }
    
    {
      name: series.name + "_max",
      points: aggregated_points,
      aggregation: Some("max")
    }
  }
  
  let aggregate_by_min = fn(series: TimeSeries, window_size: Int) {
    let mut aggregated_points = []
    
    for i in 0..(series.points.length() / window_size) {
      let mut min_value = Float::infinity()
      
      for j in 0..window_size {
        let index = i * window_size + j
        if index < series.points.length() {
          let value = series.points[index].value
          if value < min_value {
            min_value = value
          }
        }
      }
      
      aggregated_points = aggregated_points.push({
        timestamp: series.points[i * window_size].timestamp,
        value: min_value,
        tags: series.points[i * window_size].tags
      })
    }
    
    {
      name: series.name + "_min",
      points: aggregated_points,
      aggregation: Some("min")
    }
  }
  
  // Test average aggregation
  let avg_aggregated = aggregate_by_average(test_series, 4)  // 4-hour windows
  assert_eq(avg_aggregated.name, "cpu_usage_avg")
  assert_eq(avg_aggregated.points.length(), 6)  // 24 / 4 = 6
  assert_eq(avg_aggregated.aggregation, Some("average"))
  
  // Verify first aggregated point (average of first 4 points)
  let expected_first_avg = (test_series.points[0].value + 
                           test_series.points[1].value + 
                           test_series.points[2].value + 
                           test_series.points[3].value) / 4.0
  assert_eq(avg_aggregated.points[0].value, expected_first_avg)
  
  // Test sum aggregation
  let sum_aggregated = aggregate_by_sum(test_series, 6)  // 6-hour windows
  assert_eq(sum_aggregated.name, "cpu_usage_sum")
  assert_eq(sum_aggregated.points.length(), 4)  // 24 / 6 = 4
  assert_eq(sum_aggregated.aggregation, Some("sum"))
  
  // Verify first aggregated point (sum of first 6 points)
  let expected_first_sum = test_series.points[0].value + 
                          test_series.points[1].value + 
                          test_series.points[2].value + 
                          test_series.points[3].value + 
                          test_series.points[4].value + 
                          test_series.points[5].value
  assert_eq(sum_aggregated.points[0].value, expected_first_sum)
  
  // Test max aggregation
  let max_aggregated = aggregate_by_max(test_series, 3)  // 3-hour windows
  assert_eq(max_aggregated.name, "cpu_usage_max")
  assert_eq(max_aggregated.points.length(), 8)  // 24 / 3 = 8
  assert_eq(max_aggregated.aggregation, Some("max"))
  
  // Verify first aggregated point (max of first 3 points)
  let expected_first_max = test_series.points[0].value.max(
    test_series.points[1].value.max(test_series.points[2].value)
  )
  assert_eq(max_aggregated.points[0].value, expected_first_max)
  
  // Test min aggregation
  let min_aggregated = aggregate_by_min(test_series, 3)  // 3-hour windows
  assert_eq(min_aggregated.name, "cpu_usage_min")
  assert_eq(min_aggregated.points.length(), 8)  // 24 / 3 = 8
  assert_eq(min_aggregated.aggregation, Some("min"))
  
  // Verify first aggregated point (min of first 3 points)
  let expected_first_min = test_series.points[0].value.min(
    test_series.points[1].value.min(test_series.points[2].value)
  )
  assert_eq(min_aggregated.points[0].value, expected_first_min)
  
  // Verify max >= average >= min for each window
  for i in 0..max_aggregated.points.length() {
    assert_true(max_aggregated.points[i].value >= avg_aggregated.points[i].value)
    assert_true(avg_aggregated.points[i].value >= min_aggregated.points[i].value)
  }
}

// Test 3: Time Series Resampling
test "time series resampling" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  // Create test time series with 5-minute intervals
  let create_high_frequency_series = fn() {
    let mut points = []
    
    for i in 0..288 {  // 24 hours * 12 points per hour (5-minute intervals)
      points = points.push({
        timestamp: 1640995200 + i * 300,  // 5-minute intervals
        value: 50.0 + 10.0 * (i.to_float() * 0.1).sin() + (i % 6).to_float() * 2.0,
        tags: [("host", "server-1"), ("metric", "cpu")]
      })
    }
    
    {
      name: "cpu_usage_high_freq",
      points,
      aggregation: None
    }
  }
  
  let high_freq_series = create_high_frequency_series()
  assert_eq(high_freq_series.points.length(), 288)
  
  // Resampling functions
  let upsample = fn(series: TimeSeries, new_interval: Int, interpolation: String) {
    let original_interval = series.points[1].timestamp - series.points[0].timestamp
    let upsample_factor = new_interval / original_interval
    
    let mut upsampled_points = []
    
    for i in 0..series.points.length() - 1 {
      let current_point = series.points[i]
      let next_point = series.points[i + 1]
      
      // Add the original point
      upsampled_points = upsampled_points.push(current_point)
      
      // Add interpolated points
      for j in 1..upsample_factor {
        let fraction = j.to_float() / upsample_factor.to_float()
        let interpolated_value = match interpolation {
          "linear" => current_point.value + (next_point.value - current_point.value) * fraction,
          "constant" => current_point.value,
          _ => current_point.value
        }
        
        upsampled_points = upsampled_points.push({
          timestamp: current_point.timestamp + j * new_interval,
          value: interpolated_value,
          tags: current_point.tags
        })
      }
    }
    
    // Add the last point
    upsampled_points = upsampled_points.push(series.points[series.points.length() - 1])
    
    {
      name: series.name + "_upsampled",
      points: upsampled_points,
      aggregation: series.aggregation
    }
  }
  
  let downsample = fn(series: TimeSeries, new_interval: Int, aggregation: String) {
    let original_interval = series.points[1].timestamp - series.points[0].timestamp
    let downsample_factor = new_interval / original_interval
    
    let mut downsampled_points = []
    
    for i in 0..(series.points.length() / downsample_factor) {
      let mut values = []
      let base_timestamp = series.points[i * downsample_factor].timestamp
      
      // Collect values for the window
      for j in 0..downsample_factor {
        let index = i * downsample_factor + j
        if index < series.points.length() {
          values = values.push(series.points[index].value)
        }
      }
      
      // Aggregate values
      let aggregated_value = match aggregation {
        "average" => {
          let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
          sum / values.length().to_float()
        }
        "sum" => values.reduce(fn(acc, val) { acc + val }, 0.0),
        "max" => values.reduce(fn(acc, val) { if val > acc { val } else { acc } , values[0]),
        "min" => values.reduce(fn(acc, val) { if val < acc { val } else { acc } , values[0]),
        "first" => values[0],
        "last" => values[values.length() - 1],
        _ => values[0]
      }
      
      downsampled_points = downsampled_points.push({
        timestamp: base_timestamp,
        value: aggregated_value,
        tags: series.points[i * downsample_factor].tags
      })
    }
    
    {
      name: series.name + "_downsampled",
      points: downsampled_points,
      aggregation: Some(aggregation)
    }
  }
  
  // Test upsampling
  let upsampled_linear = upsample(high_freq_series, 150, "linear")  // From 5 min to 2.5 min
  assert_eq(upsampled_linear.name, "cpu_usage_high_freq_upsampled")
  assert_eq(upsampled_linear.points.length(), 575)  // Approximately 2x - 1
  
  // Verify first few points
  assert_eq(upsampled_linear.points[0].timestamp, high_freq_series.points[0].timestamp)
  assert_eq(upsampled_linear.points[0].value, high_freq_series.points[0].value)
  assert_eq(upsampled_linear.points[1].timestamp, high_freq_series.points[0].timestamp + 150)
  
  // Test upsampling with constant interpolation
  let upsampled_constant = upsample(high_freq_series, 150, "constant")
  assert_eq(upsampled_constant.name, "cpu_usage_high_freq_upsampled")
  assert_eq(upsampled_constant.points.length(), 575)
  
  // With constant interpolation, the interpolated point should equal the original point
  assert_eq(upsampled_constant.points[1].value, upsampled_constant.points[0].value)
  
  // Test downsampling
  let downsampled_avg = downsample(high_freq_series, 1800, "average")  // From 5 min to 30 min
  assert_eq(downsampled_avg.name, "cpu_usage_high_freq_downsampled")
  assert_eq(downsampled_avg.points.length(), 48)  // 288 / 6 = 48
  assert_eq(downsampled_avg.aggregation, Some("average"))
  
  // Verify first downsampled point is the average of first 6 original points
  let expected_first_avg = (high_freq_series.points[0].value + 
                           high_freq_series.points[1].value + 
                           high_freq_series.points[2].value + 
                           high_freq_series.points[3].value + 
                           high_freq_series.points[4].value + 
                           high_freq_series.points[5].value) / 6.0
  assert_eq(downsampled_avg.points[0].value, expected_first_avg)
  
  // Test downsampling with different aggregation methods
  let downsampled_max = downsample(high_freq_series, 3600, "max")  // From 5 min to 1 hour
  assert_eq(downsampled_max.name, "cpu_usage_high_freq_downsampled")
  assert_eq(downsampled_max.points.length(), 24)  // 288 / 12 = 24
  assert_eq(downsampled_max.aggregation, Some("max"))
  
  // Verify first downsampled point is the max of first 12 original points
  let mut expected_first_max = high_freq_series.points[0].value
  for i in 1..12 {
    if high_freq_series.points[i].value > expected_first_max {
      expected_first_max = high_freq_series.points[i].value
    }
  }
  assert_eq(downsampled_max.points[0].value, expected_first_max)
  
  // Test downsampling with sum aggregation
  let downsampled_sum = downsample(high_freq_series, 3600, "sum")  // From 5 min to 1 hour
  assert_eq(downsampled_sum.name, "cpu_usage_high_freq_downsampled")
  assert_eq(downsampled_sum.points.length(), 24)  // 288 / 12 = 24
  assert_eq(downsampled_sum.aggregation, Some("sum"))
  
  // Verify first downsampled point is the sum of first 12 original points
  let mut expected_first_sum = 0.0
  for i in 0..12 {
    expected_first_sum = expected_first_sum + high_freq_series.points[i].value
  }
  assert_eq(downsampled_sum.points[0].value, expected_first_sum)
}

// Test 4: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  type Anomaly = {
    timestamp: Int,
    value: Float,
    expected_value: Float,
    deviation_score: Float,
    anomaly_type: String
  }
  
  // Create test time series with some anomalies
  let create_series_with_anomalies = fn() {
    let mut points = []
    
    for i in 0..48 {
      let base_value = 50.0 + 10.0 * (i.to_float() * 0.2).sin()
      let value = match i {
        10 => 90.0,  // Spike anomaly
        25 => 10.0,  // Dip anomaly
        37 => 75.0,  // Another spike
        _ => base_value + (i % 5).to_float() * 2.0  // Normal variation
      }
      
      points = points.push({
        timestamp: 1640995200 + i * 1800,  // 30-minute intervals
        value,
        tags: [("host", "server-1"), ("metric", "cpu")]
      })
    }
    
    {
      name: "cpu_with_anomalies",
      points,
      aggregation: None
    }
  }
  
  let anomaly_series = create_series_with_anomalies()
  assert_eq(anomaly_series.points.length(), 48)
  
  // Verify anomalies are present
  assert_eq(anomaly_series.points[10].value, 90.0)
  assert_eq(anomaly_series.points[25].value, 10.0)
  assert_eq(anomaly_series.points[37].value, 75.0)
  
  // Anomaly detection functions
  let detect_anomalies_with_threshold = fn(series: TimeSeries, threshold: Float) {
    let mut anomalies = []
    
    // Calculate mean and standard deviation
    let sum = series.points.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let mean = sum / series.points.length().to_float()
    
    let variance = series.points.reduce(fn(acc, point) { 
      acc + (point.value - mean) * (point.value - mean) 
    }, 0.0) / series.points.length().to_float()
    
    let std_dev = variance.sqrt()
    
    for point in series.points {
      let z_score = (point.value - mean) / std_dev
      
      if z_score.abs() > threshold {
        anomalies = anomalies.push({
          timestamp: point.timestamp,
          value: point.value,
          expected_value: mean,
          deviation_score: z_score.abs(),
          anomaly_type: if z_score > 0.0 { "spike" } else { "dip" }
        })
      }
    }
    
    anomalies
  }
  
  let detect_anomalies_with_moving_average = fn(series: TimeSeries, window_size: Int, threshold: Float) {
    let mut anomalies = []
    
    for i in window_size..series.points.length() {
      let mut sum = 0.0
      let mut count = 0
      
      // Calculate moving average
      for j in (i - window_size)..i {
        sum = sum + series.points[j].value
        count = count + 1
      }
      
      let moving_avg = sum / count.to_float()
      let current_value = series.points[i].value
      let deviation = (current_value - moving_avg).abs() / moving_avg
      
      if deviation > threshold {
        anomalies = anomalies.push({
          timestamp: series.points[i].timestamp,
          value: current_value,
          expected_value: moving_avg,
          deviation_score: deviation,
          anomaly_type: if current_value > moving_avg { "spike" } else { "dip" }
        })
      }
    }
    
    anomalies
  }
  
  let detect_anomalies_with_exponential_smoothing = fn(series: TimeSeries, alpha: Float, threshold: Float) {
    let mut anomalies = []
    let mut smoothed_values = []
    
    // Initialize with first value
    smoothed_values = smoothed_values.push(series.points[0].value)
    
    // Apply exponential smoothing
    for i in 1..series.points.length() {
      let smoothed = alpha * series.points[i].value + (1.0 - alpha) * smoothed_values[i - 1]
      smoothed_values = smoothed_values.push(smoothed)
    }
    
    // Detect anomalies
    for i in 1..series.points.length() {
      let current_value = series.points[i].value
      let expected_value = smoothed_values[i - 1]
      let deviation = (current_value - expected_value).abs() / expected_value
      
      if deviation > threshold {
        anomalies = anomalies.push({
          timestamp: series.points[i].timestamp,
          value: current_value,
          expected_value,
          deviation_score: deviation,
          anomaly_type: if current_value > expected_value { "spike" } else { "dip" }
        })
      }
    }
    
    anomalies
  }
  
  // Test threshold-based anomaly detection
  let threshold_anomalies = detect_anomalies_with_threshold(anomaly_series, 2.0)
  assert_true(threshold_anomalies.length() >= 3)  // At least our 3 planted anomalies
  
  // Check if our planted anomalies are detected
  let spike_anomalies = threshold_anomalies.filter(fn(a) { a.anomaly_type == "spike" })
  let dip_anomalies = threshold_anomalies.filter(fn(a) { a.anomaly_type == "dip" })
  
  assert_true(spike_anomalies.length() >= 2)  // At least 2 spikes
  assert_true(dip_anomalies.length() >= 1)   // At least 1 dip
  
  // Test moving average anomaly detection
  let ma_anomalies = detect_anomalies_with_moving_average(anomaly_series, 6, 0.3)
  assert_true(ma_anomalies.length() >= 3)  // At least our 3 planted anomalies
  
  // Test exponential smoothing anomaly detection
  let es_anomalies = detect_anomalies_with_exponential_smoothing(anomaly_series, 0.3, 0.3)
  assert_true(es_anomalies.length() >= 3)  // At least our 3 planted anomalies
  
  // Compare detection methods
  let threshold_count = threshold_anomalies.length()
  let ma_count = ma_anomalies.length()
  let es_count = es_anomalies.length()
  
  assert_true(threshold_count > 0)
  assert_true(ma_count > 0)
  assert_true(es_count > 0)
  
  // Test anomaly severity scoring
  let calculate_anomaly_severity = fn(anomalies: Array[Anomaly]) {
    if anomalies.length() == 0 {
      "none"
    } else {
      let max_score = anomalies.reduce(fn(acc, anomaly) { 
        if anomaly.deviation_score > acc { anomaly.deviation_score } else { acc } 
      }, 0.0)
      
      if max_score > 4.0 {
        "critical"
      } else if max_score > 3.0 {
        "high"
      } else if max_score > 2.0 {
        "medium"
      } else {
        "low"
      }
    }
  }
  
  let threshold_severity = calculate_anomaly_severity(threshold_anomalies)
  let ma_severity = calculate_anomaly_severity(ma_anomalies)
  let es_severity = calculate_anomaly_severity(es_anomalies)
  
  assert_true(threshold_severity != "none")
  assert_true(ma_severity != "none")
  assert_true(es_severity != "none")
}

// Test 5: Time Series Forecasting
test "time series forecasting" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  type Forecast = {
    timestamp: Int,
    predicted_value: Float,
    confidence_interval: (Float, Float)
  }
  
  // Create test time series
  let create_test_series = fn() {
    let mut points = []
    
    for i in 0..48 {
      let value = 50.0 + 20.0 * (i.to_float() * 0.1).sin() + i.to_float() * 0.5  // Trend + seasonal
      points = points.push({
        timestamp: 1640995200 + i * 3600,  // Hourly data
        value,
        tags: [("host", "server-1"), ("metric", "cpu")]
      })
    }
    
    {
      name: "cpu_usage",
      points,
      aggregation: None
    }
  }
  
  let test_series = create_test_series()
  assert_eq(test_series.points.length(), 48)
  
  // Forecasting functions
  let forecast_with_moving_average = fn(series: TimeSeries, window_size: Int, steps: Int) {
    let mut forecasts = []
    let last_timestamp = series.points[series.points.length() - 1].timestamp
    
    // Calculate moving average of last window_size points
    let mut sum = 0.0
    for i in (series.points.length() - window_size)..series.points.length() {
      sum = sum + series.points[i].value
    }
    let moving_avg = sum / window_size.to_float()
    
    // Simple forecast: use moving average for all future points
    for i in 1..=steps {
      let forecast_timestamp = last_timestamp + i * 3600  // Hourly steps
      let confidence_width = 5.0 + i.to_float() * 0.5  // Wider confidence for further predictions
      
      forecasts = forecasts.push({
        timestamp: forecast_timestamp,
        predicted_value: moving_avg,
        confidence_interval: (moving_avg - confidence_width, moving_avg + confidence_width)
      })
    }
    
    forecasts
  }
  
  let forecast_with_linear_regression = fn(series: TimeSeries, steps: Int) {
    let mut forecasts = []
    let last_timestamp = series.points[series.points.length() - 1].timestamp
    
    // Calculate linear regression
    let n = series.points.length().to_float()
    let sum_x = (0..series.points.length()).reduce(fn(acc, i) { acc + i.to_float() }, 0.0)
    let sum_y = series.points.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let sum_xy = (0..series.points.length()).reduce(fn(acc, i) { 
      acc + i.to_float() * series.points[i].value 
    }, 0.0)
    let sum_x2 = (0..series.points.length()).reduce(fn(acc, i) { 
      acc + i.to_float() * i.to_float() 
    }, 0.0)
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    // Calculate residuals for confidence interval
    let mut residual_sum = 0.0
    for i in 0..series.points.length() {
      let predicted = slope * i.to_float() + intercept
      let residual = series.points[i].value - predicted
      residual_sum = residual_sum + residual * residual
    }
    
    let mse = residual_sum / n
    let std_error = mse.sqrt()
    
    // Generate forecasts
    for i in 1..=steps {
      let forecast_timestamp = last_timestamp + i * 3600  // Hourly steps
      let x_value = series.points.length().to_float() + i.to_float()
      let predicted_value = slope * x_value + intercept
      
      // Confidence interval widens for further predictions
      let confidence_width = std_error * 2.0 * (1.0 + i.to_float() * 0.1)
      
      forecasts = forecasts.push({
        timestamp: forecast_timestamp,
        predicted_value,
        confidence_interval: (predicted_value - confidence_width, predicted_value + confidence_width)
      })
    }
    
    forecasts
  }
  
  let forecast_with_exponential_smoothing = fn(series: TimeSeries, alpha: Float, steps: Int) {
    let mut forecasts = []
    let last_timestamp = series.points[series.points.length() - 1].timestamp
    
    // Apply exponential smoothing
    let mut smoothed = series.points[0].value
    for i in 1..series.points.length() {
      smoothed = alpha * series.points[i].value + (1.0 - alpha) * smoothed
    }
    
    // Calculate residuals for confidence interval
    let mut residual_sum = 0.0
    let mut temp_smoothed = series.points[0].value
    
    for i in 1..series.points.length() {
      let predicted = temp_smoothed
      let residual = series.points[i].value - predicted
      residual_sum = residual_sum + residual * residual
      
      temp_smoothed = alpha * series.points[i].value + (1.0 - alpha) * temp_smoothed
    }
    
    let mse = residual_sum / (series.points.length() - 1).to_float()
    let std_error = mse.sqrt()
    
    // Generate forecasts (flat forecast with exponential smoothing)
    for i in 1..=steps {
      let forecast_timestamp = last_timestamp + i * 3600  // Hourly steps
      let confidence_width = std_error * 2.0 * (1.0 + i.to_float() * 0.1)
      
      forecasts = forecasts.push({
        timestamp: forecast_timestamp,
        predicted_value: smoothed,
        confidence_interval: (smoothed - confidence_width, smoothed + confidence_width)
      })
    }
    
    forecasts
  }
  
  // Test moving average forecast
  let ma_forecasts = forecast_with_moving_average(test_series, 12, 6)  // 12-hour window, 6 steps ahead
  assert_eq(ma_forecasts.length(), 6)
  
  // Verify first forecast
  let first_forecast = ma_forecasts[0]
  assert_eq(first_forecast.timestamp, test_series.points[test_series.points.length() - 1].timestamp + 3600)
  assert_true(first_forecast.confidence_interval.0 < first_forecast.predicted_value)
  assert_true(first_forecast.confidence_interval.1 > first_forecast.predicted_value)
  
  // Verify confidence intervals widen for further predictions
  for i in 1..ma_forecasts.length() {
    let prev_width = ma_forecasts[i-1].confidence_interval.1 - ma_forecasts[i-1].confidence_interval.0
    let curr_width = ma_forecasts[i].confidence_interval.1 - ma_forecasts[i].confidence_interval.0
    assert_true(curr_width >= prev_width)
  }
  
  // Test linear regression forecast
  let lr_forecasts = forecast_with_linear_regression(test_series, 6)  // 6 steps ahead
  assert_eq(lr_forecasts.length(), 6)
  
  // Verify first forecast
  let lr_first = lr_forecasts[0]
  assert_eq(lr_first.timestamp, test_series.points[test_series.points.length() - 1].timestamp + 3600)
  assert_true(lr_first.confidence_interval.0 < lr_first.predicted_value)
  assert_true(lr_first.confidence_interval.1 > lr_first.predicted_value)
  
  // Test exponential smoothing forecast
  let es_forecasts = forecast_with_exponential_smoothing(test_series, 0.3, 6)  // 6 steps ahead
  assert_eq(es_forecasts.length(), 6)
  
  // Verify first forecast
  let es_first = es_forecasts[0]
  assert_eq(es_first.timestamp, test_series.points[test_series.points.length() - 1].timestamp + 3600)
  assert_true(es_first.confidence_interval.0 < es_first.predicted_value)
  assert_true(es_first.confidence_interval.1 > es_first.predicted_value)
  
  // Compare forecast methods
  let ma_value = ma_forecasts[0].predicted_value
  let lr_value = lr_forecasts[0].predicted_value
  let es_value = es_forecasts[0].predicted_value
  
  // Values should be different
  assert_not_eq(ma_value, lr_value)
  assert_not_eq(lr_value, es_value)
  assert_not_eq(ma_value, es_value)
  
  // Test forecast accuracy evaluation
  let evaluate_forecast_accuracy = fn(actual: Array[Float], predicted: Array[Float]) {
    if actual.length() != predicted.length() || actual.length() == 0 {
      {
        mae: Float::infinity(),
        mse: Float::infinity(),
        rmse: Float::infinity(),
        mape: Float::infinity()
      }
    } else {
      let mut mae_sum = 0.0
      let mut mse_sum = 0.0
      let mut mape_sum = 0.0
      
      for i in 0..actual.length() {
        let error = actual[i] - predicted[i]
        mae_sum = mae_sum + error.abs()
        mse_sum = mse_sum + error * error
        mape_sum = mape_sum + (error.abs() / actual[i].abs())
      }
      
      let n = actual.length().to_float()
      let mae = mae_sum / n
      let mse = mse_sum / n
      let rmse = mse.sqrt()
      let mape = mape_sum / n * 100.0
      
      {
        mae,
        mse,
        rmse,
        mape
      }
    }
  }
  
  // Create a test scenario where we use first 36 points to forecast next 12
  let training_series = {
    name: test_series.name,
    points: test_series.points.slice(0, 36),
    aggregation: test_series.aggregation
  }
  
  let actual_values = test_series.points.slice(36, 12).map(fn(p) { p.value })
  
  let ma_test_forecasts = forecast_with_moving_average(training_series, 12, 12)
  let ma_predicted_values = ma_test_forecasts.map(fn(f) { f.predicted_value })
  
  let lr_test_forecasts = forecast_with_linear_regression(training_series, 12)
  let lr_predicted_values = lr_test_forecasts.map(fn(f) { f.predicted_value })
  
  let ma_accuracy = evaluate_forecast_accuracy(actual_values, ma_predicted_values)
  let lr_accuracy = evaluate_forecast_accuracy(actual_values, lr_predicted_values)
  
  // Verify accuracy metrics are calculated
  assert_true(ma_accuracy.mae < Float::infinity())
  assert_true(ma_accuracy.mse < Float::infinity())
  assert_true(ma_accuracy.rmse < Float::infinity())
  assert_true(ma_accuracy.mape < Float::infinity())
  
  assert_true(lr_accuracy.mae < Float::infinity())
  assert_true(lr_accuracy.mse < Float::infinity())
  assert_true(lr_accuracy.rmse < Float::infinity())
  assert_true(lr_accuracy.mape < Float::infinity())
}

// Test 6: Time Series Pattern Recognition
test "time series pattern recognition" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  type Pattern = {
    pattern_type: String,
    start_index: Int,
    end_index: Int,
    confidence: Float,
    parameters: Array[(String, Float)]
  }
  
  // Create test time series with different patterns
  let create_series_with_patterns = fn() {
    let mut points = []
    
    for i in 0..96 {  // 4 days of hourly data
      let value = match i {
        // Daily pattern (24-hour cycle)
        0..23 => 30.0 + 20.0 * (i.to_float() * 0.26).sin(),  // First day
        24..47 => 30.0 + 20.0 * ((i - 24).to_float() * 0.26).sin(),  // Second day
        // Weekly pattern (7-day cycle, simplified)
        48..71 => 40.0 + 15.0 * ((i - 48).to_float() * 0.09).sin(),  // Weekend pattern
        // Trend
        72..95 => 20.0 + i.to_float() * 0.5 + 10.0 * ((i - 72).to_float() * 0.2).sin(),  // Trend with noise
        _ => 50.0
      }
      
      points = points.push({
        timestamp: 1640995200 + i * 3600,  // Hourly data
        value,
        tags: [("host", "server-1"), ("metric", "cpu")]
      })
    }
    
    {
      name: "cpu_with_patterns",
      points,
      aggregation: None
    }
  }
  
  let pattern_series = create_series_with_patterns()
  assert_eq(pattern_series.points.length(), 96)
  
  // Pattern recognition functions
  let detect_seasonal_pattern = fn(series: TimeSeries, min_period: Int, max_period: Int) {
    let mut best_period = 0
    let mut best_correlation = 0.0
    
    // Try different periods
    for period in min_period..max_period {
      let mut correlation_sum = 0.0
      let mut count = 0
      
      // Calculate autocorrelation for this period
      for i in period..series.points.length() {
        let correlation = series.points[i].value * series.points[i - period].value
        correlation_sum = correlation_sum + correlation
        count = count + 1
      }
      
      if count > 0 {
        let avg_correlation = correlation_sum / count.to_float()
        if avg_correlation > best_correlation {
          best_correlation = avg_correlation
          best_period = period
        }
      }
    }
    
    if best_period > 0 {
      let confidence = best_correlation / 1000.0  // Normalize to 0-1 range
      if confidence > 0.5 {
        Some({
          pattern_type: "seasonal",
          start_index: 0,
          end_index: series.points.length() - 1,
          confidence,
          parameters: [("period", best_period.to_float())]
        })
      } else {
        None
      }
    } else {
      None
    }
  }
  
  let detect_trend_pattern = fn(series: TimeSeries) {
    // Calculate linear regression
    let n = series.points.length().to_float()
    let sum_x = (0..series.points.length()).reduce(fn(acc, i) { acc + i.to_float() }, 0.0)
    let sum_y = series.points.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let sum_xy = (0..series.points.length()).reduce(fn(acc, i) { 
      acc + i.to_float() * series.points[i].value 
    }, 0.0)
    let sum_x2 = (0..series.points.length()).reduce(fn(acc, i) { 
      acc + i.to_float() * i.to_float() 
    }, 0.0)
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    
    // Calculate R-squared
    let y_mean = sum_y / n
    let mut ss_tot = 0.0
    let mut ss_res = 0.0
    
    for i in 0..series.points.length() {
      let y_pred = slope * i.to_float() + (sum_y - slope * sum_x) / n
      ss_tot = ss_tot + (series.points[i].value - y_mean) * (series.points[i].value - y_mean)
      ss_res = ss_res + (series.points[i].value - y_pred) * (series.points[i].value - y_pred)
    }
    
    let r_squared = if ss_tot > 0.0 { 1.0 - (ss_res / ss_tot) } else { 0.0 }
    
    if r_squared > 0.3 && slope.abs() > 0.1 {
      Some({
        pattern_type: "trend",
        start_index: 0,
        end_index: series.points.length() - 1,
        confidence: r_squared,
        parameters: [("slope", slope)]
      })
    } else {
      None
    }
  }
  
  let detect_outlier_pattern = fn(series: TimeSeries, threshold: Float) {
    let mut outliers = []
    
    // Calculate mean and standard deviation
    let sum = series.points.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let mean = sum / series.points.length().to_float()
    
    let variance = series.points.reduce(fn(acc, point) { 
      acc + (point.value - mean) * (point.value - mean) 
    }, 0.0) / series.points.length().to_float()
    
    let std_dev = variance.sqrt()
    
    for i in 0..series.points.length() {
      let z_score = (series.points[i].value - mean) / std_dev
      
      if z_score.abs() > threshold {
        outliers = outliers.push(i)
      }
    }
    
    if outliers.length() > 0 {
      let confidence = outliers.length().to_float() / series.points.length().to_float()
      Some({
        pattern_type: "outlier",
        start_index: outliers[0],
        end_index: outliers[outliers.length() - 1],
        confidence,
        parameters: [("count", outliers.length().to_float())]
      })
    } else {
      None
    }
  }
  
  // Test seasonal pattern detection
  let seasonal_pattern = detect_seasonal_pattern(pattern_series, 12, 48)  // Look for periods between 12 and 48 hours
  match seasonal_pattern {
    Some(pattern) => {
      assert_eq(pattern.pattern_type, "seasonal")
      assert_true(pattern.confidence > 0.5)
      assert_true(pattern.parameters.length() > 0)
      assert_eq(pattern.parameters[0].0, "period")
      assert_true(pattern.parameters[0].1 >= 12.0 && pattern.parameters[0].1 <= 48.0)
    }
    None => assert_true(false)  // Should detect a pattern
  }
  
  // Test trend pattern detection
  let trend_pattern = detect_trend_pattern(pattern_series)
  match trend_pattern {
    Some(pattern) => {
      assert_eq(pattern.pattern_type, "trend")
      assert_true(pattern.confidence > 0.3)
      assert_true(pattern.parameters.length() > 0)
      assert_eq(pattern.parameters[0].0, "slope")
      assert_true(pattern.parameters[0].1.abs() > 0.1)
    }
    None => assert_true(false)  // Should detect a pattern
  }
  
  // Test outlier pattern detection
  let outlier_pattern = detect_outlier_pattern(pattern_series, 2.0)
  match outlier_pattern {
    Some(pattern) => {
      assert_eq(pattern.pattern_type, "outlier")
      assert_true(pattern.confidence > 0.0)
      assert_true(pattern.parameters.length() > 0)
      assert_eq(pattern.parameters[0].0, "count")
      assert_true(pattern.parameters[0].1 >= 1.0)
    }
    None => {}  // May or may not detect outliers
  }
  
  // Test pattern combination
  let detect_all_patterns = fn(series: TimeSeries) {
    let mut patterns = []
    
    match detect_seasonal_pattern(series, 12, 48) {
      Some(pattern) => patterns = patterns.push(pattern)
      None => {}
    }
    
    match detect_trend_pattern(series) {
      Some(pattern) => patterns = patterns.push(pattern)
      None => {}
    }
    
    match detect_outlier_pattern(series, 2.0) {
      Some(pattern) => patterns = patterns.push(pattern)
      None => {}
    }
    
    patterns
  }
  
  let all_patterns = detect_all_patterns(pattern_series)
  assert_true(all_patterns.length() >= 2)  // Should detect at least seasonal and trend
  
  // Test pattern-based forecasting
  let forecast_with_pattern = fn(series: TimeSeries, pattern: Pattern, steps: Int) {
    let mut forecasts = []
    let last_timestamp = series.points[series.points.length() - 1].timestamp
    
    match pattern.pattern_type {
      "seasonal" => {
        // Get the period from pattern parameters
        let period = match pattern.parameters.find(fn(p) { p.0 == "period" }) {
          Some(param) => param.1.to_int()
          None => 24  // Default to 24 hours
        }
        
        // Use seasonal pattern for forecasting
        for i in 1..=steps {
          let forecast_timestamp = last_timestamp + i * 3600
          let historical_index = (series.points.length() - period + i) % period
          let predicted_value = series.points[historical_index].value
          
          forecasts = forecasts.push({
            timestamp: forecast_timestamp,
            predicted_value,
            confidence_interval: (predicted_value * 0.9, predicted_value * 1.1)
          })
        }
      }
      "trend" => {
        // Get the slope from pattern parameters
        let slope = match pattern.parameters.find(fn(p) { p.0 == "slope" }) {
          Some(param) => param.1
          None => 0.0
        }
        
        // Use trend for forecasting
        let last_value = series.points[series.points.length() - 1].value
        
        for i in 1..=steps {
          let forecast_timestamp = last_timestamp + i * 3600
          let predicted_value = last_value + slope * i.to_float()
          
          forecasts = forecasts.push({
            timestamp: forecast_timestamp,
            predicted_value,
            confidence_interval: (predicted_value - 5.0, predicted_value + 5.0)
          })
        }
      }
      _ => {
        // Default to simple average
        let sum = series.points.reduce(fn(acc, point) { acc + point.value }, 0.0)
        let avg = sum / series.points.length().to_float()
        
        for i in 1..=steps {
          let forecast_timestamp = last_timestamp + i * 3600
          
          forecasts = forecasts.push({
            timestamp: forecast_timestamp,
            predicted_value: avg,
            confidence_interval: (avg * 0.9, avg * 1.1)
          })
        }
      }
    }
    
    forecasts
  }
  
  // Test pattern-based forecasting
  match seasonal_pattern {
    Some(pattern) => {
      let seasonal_forecasts = forecast_with_pattern(pattern_series, pattern, 6)
      assert_eq(seasonal_forecasts.length(), 6)
      
      // Verify first forecast
      let first_forecast = seasonal_forecasts[0]
      assert_eq(first_forecast.timestamp, pattern_series.points[pattern_series.points.length() - 1].timestamp + 3600)
      assert_true(first_forecast.confidence_interval.0 < first_forecast.predicted_value)
      assert_true(first_forecast.confidence_interval.1 > first_forecast.predicted_value)
    }
    None => assert_true(false)
  }
}

// Test 7: Time Series Correlation Analysis
test "time series correlation analysis" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  type CorrelationResult = {
    correlation: Float,
    p_value: Float,
    lag: Int
  }
  
  // Create correlated test time series
  let create_correlated_series = fn() {
    let mut cpu_points = []
    let mut memory_points = []
    
    for i in 0..48 {
      let base_value = 50.0 + 20.0 * (i.to_float() * 0.1).sin()
      
      // CPU usage
      let cpu_value = base_value + (i % 5).to_float() * 2.0
      cpu_points = cpu_points.push({
        timestamp: 1640995200 + i * 3600,
        value: cpu_value,
        tags: [("host", "server-1"), ("metric", "cpu")]
      })
      
      // Memory usage (correlated with CPU, but with some lag and noise)
      let memory_value = base_value * 0.8 + 10.0 + (i % 7).to_float() * 1.5
      memory_points = memory_points.push({
        timestamp: 1640995200 + i * 3600,
        value: memory_value,
        tags: [("host", "server-1"), ("metric", "memory")]
      })
    }
    
    // Create uncorrelated series
    let mut network_points = []
    for i in 0..48 {
      let network_value = 25.0 + 15.0 * (i.to_float() * 0.3).sin() + (i % 11).to_float() * 3.0
      network_points = network_points.push({
        timestamp: 1640995200 + i * 3600,
        value: network_value,
        tags: [("host", "server-1"), ("metric", "network")]
      })
    }
    
    [
      {
        name: "cpu_usage",
        points: cpu_points,
        aggregation: None
      },
      {
        name: "memory_usage",
        points: memory_points,
        aggregation: None
      },
      {
        name: "network_usage",
        points: network_points,
        aggregation: None
      }
    ]
  }
  
  let correlated_series = create_correlated_series()
  assert_eq(correlated_series.length(), 3)
  
  // Correlation analysis functions
  let calculate_correlation = fn(series1: TimeSeries, series2: TimeSeries) {
    if series1.points.length() != series2.points.length() {
      return {
        correlation: 0.0,
        p_value: 1.0,
        lag: 0
      }
    }
    
    let n = series1.points.length().to_float()
    
    // Calculate means
    let sum1 = series1.points.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let sum2 = series2.points.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let mean1 = sum1 / n
    let mean2 = sum2 / n
    
    // Calculate correlation
    let mut numerator = 0.0
    let mut sum_sq1 = 0.0
    let mut sum_sq2 = 0.0
    
    for i in 0..series1.points.length() {
      let diff1 = series1.points[i].value - mean1
      let diff2 = series2.points[i].value - mean2
      
      numerator = numerator + diff1 * diff2
      sum_sq1 = sum_sq1 + diff1 * diff1
      sum_sq2 = sum_sq2 + diff2 * diff2
    }
    
    let denominator = (sum_sq1 * sum_sq2).sqrt()
    let correlation = if denominator > 0.0 { numerator / denominator } else { 0.0 }
    
    // Simplified p-value calculation (not statistically accurate, just for demonstration)
    let t_statistic = correlation * ((n - 2.0) / (1.0 - correlation * correlation)).sqrt()
    let p_value = if t_statistic.abs() > 2.0 { 0.05 } else { 0.5 }  // Simplified
    
    {
      correlation,
      p_value,
      lag: 0
    }
  }
  
  let calculate_cross_correlation = fn(series1: TimeSeries, series2: TimeSeries, max_lag: Int) {
    let mut best_correlation = 0.0
    let mut best_lag = 0
    
    for lag in -max_lag..max_lag {
      let mut sum1 = 0.0
      let mut sum2 = 0.0
      let mut count = 0
      
      // Calculate correlation with lag
      for i in 0..series1.points.length() {
        let index1 = i
        let index2 = i + lag
        
        if index2 >= 0 && index2 < series2.points.length() {
          sum1 = sum1 + series1.points[index1].value
          sum2 = sum2 + series2.points[index2].value
          count = count + 1
        }
      }
      
      if count > 0 {
        let mean1 = sum1 / count.to_float()
        let mean2 = sum2 / count.to_float()
        
        let mut numerator = 0.0
        let mut sum_sq1 = 0.0
        let mut sum_sq2 = 0.0
        
        for i in 0..series1.points.length() {
          let index1 = i
          let index2 = i + lag
          
          if index2 >= 0 && index2 < series2.points.length() {
            let diff1 = series1.points[index1].value - mean1
            let diff2 = series2.points[index2].value - mean2
            
            numerator = numerator + diff1 * diff2
            sum_sq1 = sum_sq1 + diff1 * diff1
            sum_sq2 = sum_sq2 + diff2 * diff2
          }
        }
        
        let denominator = (sum_sq1 * sum_sq2).sqrt()
        let correlation = if denominator > 0.0 { numerator / denominator } else { 0.0 }
        
        if correlation.abs() > best_correlation.abs() {
          best_correlation = correlation
          best_lag = lag
        }
      }
    }
    
    // Simplified p-value calculation
    let p_value = if best_correlation.abs() > 0.5 { 0.05 } else { 0.5 }
    
    {
      correlation: best_correlation,
      p_value,
      lag: best_lag
    }
  }
  
  // Test correlation between CPU and memory (should be positively correlated)
  let cpu_memory_correlation = calculate_correlation(correlated_series[0], correlated_series[1])
  assert_true(cpu_memory_correlation.correlation > 0.5)  // Positive correlation
  assert_true(cpu_memory_correlation.p_value < 0.5)      // Significant
  
  // Test correlation between CPU and network (should be less correlated)
  let cpu_network_correlation = calculate_correlation(correlated_series[0], correlated_series[2])
  assert_true(cpu_network_correlation.correlation.abs() < 0.5)  // Less correlation
  
  // Test correlation between memory and network (should be less correlated)
  let memory_network_correlation = calculate_correlation(correlated_series[1], correlated_series[2])
  assert_true(memory_network_correlation.correlation.abs() < 0.5)  // Less correlation
  
  // Test cross-correlation
  let cpu_memory_cross_correlation = calculate_cross_correlation(correlated_series[0], correlated_series[1], 5)
  assert_true(cpu_memory_cross_correlation.correlation.abs() > 0.5)
  assert_true(cpu_memory_cross_correlation.lag >= -5 && cpu_memory_cross_correlation.lag <= 5)
  
  // Create correlation matrix
  let create_correlation_matrix = fn(series_list: Array[TimeSeries]) {
    let mut matrix = {}
    
    for i in 0..series_list.length() {
      let mut row = {}
      for j in 0..series_list.length() {
        let correlation = calculate_correlation(series_list[i], series_list[j])
        row = row.insert(j, correlation.correlation)
      }
      matrix = matrix.insert(i, row)
    }
    
    matrix
  }
  
  let correlation_matrix = create_correlation_matrix(correlated_series)
  
  // Verify diagonal elements are 1.0 (self-correlation)
  for i in 0..correlated_series.length() {
    assert_eq(correlation_matrix[i][i], 1.0)
  }
  
  // Verify symmetry
  for i in 0..correlated_series.length() {
    for j in 0..correlated_series.length() {
      assert_eq(correlation_matrix[i][j], correlation_matrix[j][i])
    }
  }
  
  // Verify CPU-memory correlation is high
  assert_true(correlation_matrix[0][1] > 0.5)
  
  // Test correlation significance
  let test_correlation_significance = fn(correlation: Float, sample_size: Int, alpha: Float) {
    // Simplified significance test
    let critical_value = 2.0 / (sample_size.to_float().sqrt())
    correlation.abs() > critical_value
  }
  
  let cpu_memory_significant = test_correlation_significance(
    cpu_memory_correlation.correlation, 
    correlated_series[0].points.length(), 
    0.05
  )
  assert_true(cpu_memory_significant)
  
  let cpu_network_significant = test_correlation_significance(
    cpu_network_correlation.correlation, 
    correlated_series[0].points.length(), 
    0.05
  )
  
  // Test correlation heatmap generation
  let generate_correlation_heatmap_data = fn(matrix: {}, series_names: Array[String>) {
    let mut heatmap_data = []
    
    for i in 0..series_names.length() {
      for j in 0..series_names.length() {
        heatmap_data = heatmap_data.push({
          series1: series_names[i],
          series2: series_names[j],
          correlation: matrix[i][j]
        })
      }
    }
    
    heatmap_data
  }
  
  let series_names = ["cpu_usage", "memory_usage", "network_usage"]
  let heatmap_data = generate_correlation_heatmap_data(correlation_matrix, series_names)
  
  assert_eq(heatmap_data.length(), 9)  // 3x3 matrix
  
  // Verify diagonal elements in heatmap
  let diagonal_elements = heatmap_data.filter(fn(d) { d.series1 == d.series2 })
  assert_eq(diagonal_elements.length(), 3)
  
  for element in diagonal_elements {
    assert_eq(element.correlation, 1.0)
  }
  
  // Test lag correlation
  let calculate_lag_correlation = fn(series1: TimeSeries, series2: TimeSeries, max_lag: Int) {
    let mut lag_correlations = []
    
    for lag in -max_lag..max_lag {
      let correlation = calculate_cross_correlation(series1, series2, lag)
      lag_correlations = lag_correlations.push({
        lag,
        correlation: correlation.correlation
      })
    }
    
    lag_correlations
  }
  
  let lag_correlations = calculate_lag_correlation(correlated_series[0], correlated_series[1], 5)
  assert_eq(lag_correlations.length(), 11)  // -5 to 5 inclusive
  
  // Find the lag with maximum correlation
  let max_lag_correlation = lag_correlations.reduce(fn(acc, lc) { 
    if lc.correlation.abs() > acc.correlation.abs() { lc } else { acc } 
  }, lag_correlations[0])
  
  assert_true(max_lag_correlation.lag >= -5 && max_lag_correlation.lag <= 5)
}

// Test 8: Time Series Similarity Analysis
test "time series similarity analysis" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  type SimilarityResult = {
    series1: String,
    series2: String,
    similarity_score: Float,
    distance: Float,
    method: String
  }
  
  // Create test time series with different similarities
  let create_similarity_test_series = fn() {
    let mut identical_points = []
    let mut similar_points = []
    let mut different_points = []
    let mut shifted_points = []
    
    for i in 0..24 {
      let base_value = 50.0 + 20.0 * (i.to_float() * 0.1).sin()
      
      // Identical series
      identical_points = identical_points.push({
        timestamp: 1640995200 + i * 3600,
        value: base_value,
        tags: [("host", "server-1"), ("metric", "cpu")]
      })
      
      // Similar series (with some noise)
      let similar_value = base_value + (i % 3).to_float() * 2.0
      similar_points = similar_points.push({
        timestamp: 1640995200 + i * 3600,
        value: similar_value,
        tags: [("host", "server-1"), ("metric", "cpu_with_noise")]
      })
      
      // Different series
      let different_value = 25.0 + 15.0 * (i.to_float() * 0.2).cos()
      different_points = different_points.push({
        timestamp: 1640995200 + i * 3600,
        value: different_value,
        tags: [("host", "server-1"), ("metric", "memory")]
      })
      
      // Shifted series (same pattern but offset)
      let shifted_value = base_value + 10.0
      shifted_points = shifted_points.push({
        timestamp: 1640995200 + i * 3600,
        value: shifted_value,
        tags: [("host", "server-1"), ("metric", "cpu_offset")]
      })
    }
    
    [
      {
        name: "identical_1",
        points: identical_points,
        aggregation: None
      },
      {
        name: "identical_2",
        points: identical_points,
        aggregation: None
      },
      {
        name: "similar",
        points: similar_points,
        aggregation: None
      },
      {
        name: "different",
        points: different_points,
        aggregation: None
      },
      {
        name: "shifted",
        points: shifted_points,
        aggregation: None
      }
    ]
  }
  
  let similarity_series = create_similarity_test_series()
  assert_eq(similarity_series.length(), 5)
  
  // Similarity analysis functions
  let euclidean_distance = fn(series1: TimeSeries, series2: TimeSeries) {
    let min_length = if series1.points.length() < series2.points.length() {
                      series1.points.length() 
                    } else {
                      series2.points.length()
                    }
    
    let mut sum_sq = 0.0
    
    for i in 0..min_length {
      let diff = series1.points[i].value - series2.points[i].value
      sum_sq = sum_sq + diff * diff
    }
    
    sum_sq.sqrt()
  }
  
  let manhattan_distance = fn(series1: TimeSeries, series2: TimeSeries) {
    let min_length = if series1.points.length() < series2.points.length() {
                      series1.points.length() 
                    } else {
                      series2.points.length()
                    }
    
    let mut sum_abs = 0.0
    
    for i in 0..min_length {
      sum_abs = sum_abs + (series1.points[i].value - series2.points[i].value).abs()
    }
    
    sum_abs
  }
  
  let cosine_similarity = fn(series1: TimeSeries, series2: TimeSeries) {
    let min_length = if series1.points.length() < series2.points.length() {
                      series1.points.length() 
                    } else {
                      series2.points.length()
                    }
    
    let mut dot_product = 0.0
    let mut norm1 = 0.0
    let mut norm2 = 0.0
    
    for i in 0..min_length {
      let val1 = series1.points[i].value
      let val2 = series2.points[i].value
      
      dot_product = dot_product + val1 * val2
      norm1 = norm1 + val1 * val1
      norm2 = norm2 + val2 * val2
    }
    
    if norm1 > 0.0 && norm2 > 0.0 {
      dot_product / (norm1.sqrt() * norm2.sqrt())
    } else {
      0.0
    }
  }
  
  let pearson_correlation = fn(series1: TimeSeries, series2: TimeSeries) {
    let min_length = if series1.points.length() < series2.points.length() {
                      series1.points.length() 
                    } else {
                      series2.points.length()
                    }
    
    let n = min_length.to_float()
    
    // Calculate means
    let mut sum1 = 0.0
    let mut sum2 = 0.0
    
    for i in 0..min_length {
      sum1 = sum1 + series1.points[i].value
      sum2 = sum2 + series2.points[i].value
    }
    
    let mean1 = sum1 / n
    let mean2 = sum2 / n
    
    // Calculate correlation
    let mut numerator = 0.0
    let mut sum_sq1 = 0.0
    let mut sum_sq2 = 0.0
    
    for i in 0..min_length {
      let diff1 = series1.points[i].value - mean1
      let diff2 = series2.points[i].value - mean2
      
      numerator = numerator + diff1 * diff2
      sum_sq1 = sum_sq1 + diff1 * diff1
      sum_sq2 = sum_sq2 + diff2 * diff2
    }
    
    let denominator = (sum_sq1 * sum_sq2).sqrt()
    if denominator > 0.0 {
      numerator / denominator
    } else {
      0.0
    }
  }
  
  let calculate_similarity = fn(series1: TimeSeries, series2: TimeSeries, method: String) {
    let similarity = match method {
      "euclidean" => {
        let distance = euclidean_distance(series1, series2)
        // Convert distance to similarity (higher is more similar)
        1.0 / (1.0 + distance)
      }
      "manhattan" => {
        let distance = manhattan_distance(series1, series2)
        1.0 / (1.0 + distance)
      }
      "cosine" => cosine_similarity(series1, series2),
      "pearson" => {
        let correlation = pearson_correlation(series1, series2)
        // Convert correlation (-1 to 1) to similarity (0 to 1)
        (correlation + 1.0) / 2.0
      }
      _ => 0.0
    }
    
    let distance = match method {
      "euclidean" => euclidean_distance(series1, series2),
      "manhattan" => manhattan_distance(series1, series2),
      "cosine" => 1.0 - cosine_similarity(series1, series2),
      "pearson" => 1.0 - pearson_correlation(series1, series2).abs(),
      _ => Float::infinity()
    }
    
    {
      series1: series1.name,
      series2: series2.name,
      similarity_score: similarity,
      distance,
      method
    }
  }
  
  // Test identical series similarity
  let identical_euclidean = calculate_similarity(similarity_series[0], similarity_series[1], "euclidean")
  assert_eq(identical_euclidean.series1, "identical_1")
  assert_eq(identical_euclidean.series2, "identical_2")
  assert_eq(identical_euclidean.method, "euclidean")
  assert_true(identical_euclidean.similarity_score > 0.9)  // Very similar
  assert_true(identical_euclidean.distance < 1.0)         // Very close
  
  let identical_cosine = calculate_similarity(similarity_series[0], similarity_series[1], "cosine")
  assert_eq(identical_cosine.series1, "identical_1")
  assert_eq(identical_cosine.series2, "identical_2")
  assert_eq(identical_cosine.method, "cosine")
  assert_true(identical_cosine.similarity_score > 0.9)  // Very similar
  
  // Test similar series similarity
  let similar_pearson = calculate_similarity(similarity_series[0], similarity_series[2], "pearson")
  assert_eq(similar_pearson.series1, "identical_1")
  assert_eq(similar_pearson.series2, "similar")
  assert_eq(similar_pearson.method, "pearson")
  assert_true(similar_pearson.similarity_score > 0.7)  // Similar
  
  // Test different series similarity
  let different_manhattan = calculate_similarity(similarity_series[0], similarity_series[3], "manhattan")
  assert_eq(different_manhattan.series1, "identical_1")
  assert_eq(different_manhattan.series2, "different")
  assert_eq(different_manhattan.method, "manhattan")
  assert_true(different_manhattan.similarity_score < 0.7)  // Less similar
  
  // Test shifted series similarity
  let shifted_euclidean = calculate_similarity(similarity_series[0], similarity_series[4], "euclidean")
  assert_eq(shifted_euclidean.series1, "identical_1")
  assert_eq(shifted_euclidean.series2, "shifted")
  assert_eq(shifted_euclidean.method, "euclidean")
  assert_true(shifted_euclidean.similarity_score < 0.8)  // Less similar due to offset
  
  // Compare similarity methods
  let compare_similarity_methods = fn(series1: TimeSeries, series2: TimeSeries) {
    let euclidean = calculate_similarity(series1, series2, "euclidean")
    let manhattan = calculate_similarity(series1, series2, "manhattan")
    let cosine = calculate_similarity(series1, series2, "cosine")
    let pearson = calculate_similarity(series1, series2, "pearson")
    
    [euclidean, manhattan, cosine, pearson]
  }
  
  let method_comparisons = compare_similarity_methods(similarity_series[0], similarity_series[2])
  assert_eq(method_comparisons.length(), 4)
  
  // All methods should indicate some similarity
  for comparison in method_comparisons {
    assert_true(comparison.similarity_score > 0.0)
  }
  
  // Test similarity matrix
  let create_similarity_matrix = fn(series_list: Array[TimeSeries], method: String) {
    let mut matrix = {}
    
    for i in 0..series_list.length() {
      let mut row = {}
      for j in 0..series_list.length() {
        let similarity = calculate_similarity(series_list[i], series_list[j], method)
        row = row.insert(j, similarity.similarity_score)
      }
      matrix = matrix.insert(i, row)
    }
    
    matrix
  }
  
  let euclidean_similarity_matrix = create_similarity_matrix(similarity_series, "euclidean")
  
  // Verify diagonal elements are 1.0 (self-similarity)
  for i in 0..similarity_series.length() {
    assert_true(euclidean_similarity_matrix[i][i] > 0.9)
  }
  
  // Verify symmetry
  for i in 0..similarity_series.length() {
    for j in 0..similarity_series.length() {
      assert_eq(euclidean_similarity_matrix[i][j], euclidean_similarity_matrix[j][i])
    }
  }
  
  // Test finding most similar series
  let find_most_similar = fn(target_series: TimeSeries, candidate_series: Array[TimeSeries], method: String) {
    let mut best_match = None
    let mut best_similarity = 0.0
    
    for candidate in candidate_series {
      if candidate.name != target_series.name {
        let similarity = calculate_similarity(target_series, candidate, method)
        if similarity.similarity_score > best_similarity {
          best_similarity = similarity.similarity_score
          best_match = Some(candidate.name)
        }
      }
    }
    
    best_match
  }
  
  let most_similar_to_identical = find_most_similar(similarity_series[0], similarity_series, "euclidean")
  match most_similar_to_identical {
    Some(name) => assert_eq(name, "identical_2")  // Should find the identical series
    None => assert_true(false)
  }
  
  // Test similarity thresholding
  let find_similar_series = fn(target_series: TimeSeries, candidate_series: Array[TimeSeries], threshold: Float, method: String) {
    let mut similar_series = []
    
    for candidate in candidate_series {
      if candidate.name != target_series.name {
        let similarity = calculate_similarity(target_series, candidate, method)
        if similarity.similarity_score >= threshold {
          similar_series = similar_series.push(candidate.name)
        }
      }
    }
    
    similar_series
  }
  
  let similar_to_identical = find_similar_series(similarity_series[0], similarity_series, 0.7, "euclidean")
  assert_true(similar_to_identical.length() >= 1)  // Should find at least the identical series
  assert_true(similar_to_identical.contains("identical_2"))
  
  // Test dynamic time warping (simplified version)
  let dynamic_time_warping_distance = fn(series1: TimeSeries, series2: TimeSeries) {
    let n = series1.points.length()
    let m = series2.points.length()
    
    // Create DTW matrix
    let mut dtw = {}
    
    // Initialize first cell
    dtw = dtw.insert((0, 0), (series1.points[0].value - series2.points[0].value).abs())
    
    // Initialize first row
    for j in 1..m {
      let prev_cost = dtw[(0, j-1)]
      dtw = dtw.insert((0, j), prev_cost + (series1.points[0].value - series2.points[j].value).abs())
    }
    
    // Initialize first column
    for i in 1..n {
      let prev_cost = dtw[(i-1, 0)]
      dtw = dtw.insert((i, 0), prev_cost + (series1.points[i].value - series2.points[0].value).abs())
    }
    
    // Fill the rest of the matrix
    for i in 1..n {
      for j in 1..m {
        let cost = (series1.points[i].value - series2.points[j].value).abs()
        let min_prev = dtw[(i-1, j-1)].min(dtw[(i-1, j)]).min(dtw[(i, j-1)])
        dtw = dtw.insert((i, j), cost + min_prev)
      }
    }
    
    dtw[(n-1, m-1)]
  }
  
  let dtw_distance = dynamic_time_warping_distance(similarity_series[0], similarity_series[2])
  assert_true(dtw_distance > 0.0)
  
  // Convert DTW distance to similarity
  let dtw_similarity = 1.0 / (1.0 + dtw_distance)
  assert_true(dtw_similarity > 0.0 && dtw_similarity <= 1.0)
}

// Test 9: Time Series Decomposition
test "time series decomposition" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  type DecomposedSeries = {
    trend: Array[Float],
    seasonal: Array[Float],
    residual: Array[Float]
  }
  
  // Create test time series with trend, seasonal, and residual components
  let create_decomposition_test_series = fn() {
    let mut points = []
    
    for i in 0..48 {
      // Trend component (increasing)
      let trend = 30.0 + i.to_float() * 0.5
      
      // Seasonal component (daily pattern, 24-hour cycle)
      let seasonal = 10.0 * (i.to_float() * 0.26).sin()
      
      // Residual component (random noise)
      let residual = (i % 7).to_float() * 2.0 - 6.0  // Range -6 to 6
      
      let value = trend + seasonal + residual
      
      points = points.push({
        timestamp: 1640995200 + i * 3600,  // Hourly data
        value,
        tags: [("host", "server-1"), ("metric", "cpu")]
      })
    }
    
    {
      name: "cpu_with_components",
      points,
      aggregation: None
    }
  }
  
  let decomposition_series = create_decomposition_test_series()
  assert_eq(decomposition_series.points.length(), 48)
  
  // Decomposition functions
  let extract_trend = fn(series: TimeSeries, window_size: Int) {
    let mut trend = []
    
    for i in 0..series.points.length() {
      let mut sum = 0.0
      let mut count = 0
      
      // Calculate moving average centered at i
      for j in (i - window_size / 2)..(i + window_size / 2) {
        if j >= 0 && j < series.points.length() {
          sum = sum + series.points[j].value
          count = count + 1
        }
      }
      
      if count > 0 {
        trend = trend.push(sum / count.to_float())
      } else {
        trend = trend.push(series.points[i].value)
      }
    }
    
    trend
  }
  
  let extract_seasonal = fn(series: TimeSeries, trend: Array[Float], period: Int) {
    let mut seasonal = []
    let mut seasonal_patterns = {}
    
    // Calculate seasonal indices
    for i in 0..series.points.length() {
      if trend[i] > 0.0 {
        let detrended = series.points[i].value - trend[i]
        let seasonal_index = i % period
        
        let current_sum = match seasonal_patterns[seasonal_index] {
          Some(sum) => sum,
          None => 0.0
        }
        
        let current_count = match seasonal_patterns[(seasonal_index, "count")] {
          Some(count) => count,
          None => 0.0
        }
        
        seasonal_patterns = seasonal_patterns.insert(seasonal_index, current_sum + detrended)
        seasonal_patterns = seasonal_patterns.insert((seasonal_index, "count"), current_count + 1.0)
      }
    }
    
    // Calculate average seasonal pattern
    let mut avg_seasonal = {}
    for i in 0..period {
      let sum = seasonal_patterns[i]
      let count = seasonal_patterns[(i, "count")]
      
      if count > 0.0 {
        avg_seasonal = avg_seasonal.insert(i, sum / count)
      }
    }
    
    // Generate seasonal component
    for i in 0..series.points.length() {
      let seasonal_index = i % period
      let seasonal_value = match avg_seasonal[seasonal_index] {
        Some(value) => value,
        None => 0.0
      }
      
      seasonal = seasonal.push(seasonal_value)
    }
    
    seasonal
  }
  
  let extract_residual = fn(series: TimeSeries, trend: Array[Float], seasonal: Array[Float]) {
    let mut residual = []
    
    for i in 0..series.points.length() {
      let residual_value = series.points[i].value - trend[i] - seasonal[i]
      residual = residual.push(residual_value)
    }
    
    residual
  }
  
  let decompose_time_series = fn(series: TimeSeries, trend_window: Int, seasonal_period: Int) {
    let trend = extract_trend(series, trend_window)
    let seasonal = extract_seasonal(series, trend, seasonal_period)
    let residual = extract_residual(series, trend, seasonal)
    
    {
      trend,
      seasonal,
      residual
    }
  }
  
  // Test decomposition
  let decomposed = decompose_time_series(decomposition_series, 12, 24)
  assert_eq(decomposed.trend.length(), 48)
  assert_eq(decomposed.seasonal.length(), 48)
  assert_eq(decomposed.residual.length(), 48)
  
  // Verify reconstruction
  for i in 0..decomposition_series.points.length() {
    let reconstructed = decomposed.trend[i] + decomposed.seasonal[i] + decomposed.residual[i]
    let original = decomposition_series.points[i].value
    assert_true((reconstructed - original).abs() < 0.001)  // Should be very close
  }
  
  // Test trend component
  // Trend should be generally increasing
  let mut increasing_count = 0
  for i in 1..decomposed.trend.length() {
    if decomposed.trend[i] > decomposed.trend[i-1] {
      increasing_count = increasing_count + 1
    }
  }
  
  assert_true(increasing_count > decomposed.trend.length() / 2)  // At least half should be increasing
  
  // Test seasonal component
  // Seasonal component should have a period of 24
  let period_correlation = 0.26  // Corresponds to 24-hour period
  let mut seasonal_correlation = 0.0
  let mut count = 0
  
  for i in 24..decomposed.seasonal.length() {
    seasonal_correlation = seasonal_correlation + decomposed.seasonal[i] * decomposed.seasonal[i-24]
    count = count + 1
  }
  
  if count > 0 {
    seasonal_correlation = seasonal_correlation / count.to_float()
    assert_true(seasonal_correlation > 0.0)  // Should be positively correlated with lagged values
  }
  
  // Test residual component
  // Residual should have zero mean (approximately)
  let residual_sum = decomposed.residual.reduce(fn(acc, r) { acc + r }, 0.0)
  let residual_mean = residual_sum / decomposed.residual.length().to_float()
  assert_true(residual_mean.abs() < 1.0)  // Should be close to zero
  
  // Test STL decomposition (Seasonal and Trend decomposition using Loess)
  let stl_decompose = fn(series: TimeSeries, seasonal_period: Int) {
    // Simplified STL decomposition
    
    // Step 1: Estimate trend with loess (simplified as moving average)
    let trend = extract_trend(series, seasonal_period)
    
    // Step 2: Detrend the series
    let mut detrended = []
    for i in 0..series.points.length() {
      detrended = detrended.push(series.points[i].value - trend[i])
    }
    
    // Step 3: Estimate seasonal component
    let mut seasonal_subseries = {}
    
    for i in 0..detrended.length() {
      let seasonal_index = i % seasonal_period
      
      let current_values = match seasonal_subseries[seasonal_index] {
        Some(values) => values,
        None => []
      }
      
      seasonal_subseries = seasonal_subseries.insert(seasonal_index, current_values.push(detrended[i]))
    }
    
    // Calculate seasonal component by averaging subseries
    let mut seasonal = []
    for i in 0..series.points.length() {
      let seasonal_index = i % seasonal_period
      
      let values = seasonal_subseries[seasonal_index]
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / values.length().to_float()
      
      seasonal = seasonal.push(avg)
    }
    
    // Step 4: Calculate residual
    let mut residual = []
    for i in 0..series.points.length() {
      residual = residual.push(series.points[i].value - trend[i] - seasonal[i])
    }
    
    {
      trend,
      seasonal,
      residual
    }
  }
  
  let stl_decomposed = stl_decompose(decomposition_series, 24)
  assert_eq(stl_decomposed.trend.length(), 48)
  assert_eq(stl_decomposed.seasonal.length(), 48)
  assert_eq(stl_decomposed.residual.length(), 48)
  
  // Verify STL reconstruction
  for i in 0..decomposition_series.points.length() {
    let reconstructed = stl_decomposed.trend[i] + stl_decomposed.seasonal[i] + stl_decomposed.residual[i]
    let original = decomposition_series.points[i].value
    assert_true((reconstructed - original).abs() < 0.001)  // Should be very close
  }
  
  // Test component strength
  let calculate_component_strength = fn(component: Array[Float], original: Array[Float]) {
    let component_variance = component.reduce(fn(acc, c) { 
      let mean = component.reduce(fn(sum, x) { sum + x }, 0.0) / component.length().to_float()
      acc + (c - mean) * (c - mean)
    }, 0.0) / component.length().to_float()
    
    let original_variance = original.reduce(fn(acc, o) { 
      let mean = original.reduce(fn(sum, x) { sum + x }, 0.0) / original.length().to_float()
      acc + (o - mean) * (o - mean)
    }, 0.0) / original.length().to_float()
    
    if original_variance > 0.0 {
      component_variance / original_variance
    } else {
      0.0
    }
  }
  
  let original_values = decomposition_series.points.map(fn(p) { p.value })
  
  let trend_strength = calculate_component_strength(decomposed.trend, original_values)
  let seasonal_strength = calculate_component_strength(decomposed.seasonal, original_values)
  let residual_strength = calculate_component_strength(decomposed.residual, original_values)
  
  assert_true(trend_strength > 0.0)
  assert_true(seasonal_strength > 0.0)
  assert_true(residual_strength > 0.0)
  
  // The sum of component strengths should be approximately 1.0
  let total_strength = trend_strength + seasonal_strength + residual_strength
  assert_true(total_strength > 0.8 && total_strength < 1.2)  // Allow some tolerance
  
  // Test component analysis
  let analyze_components = fn(decomposed: DecomposedSeries) {
    // Trend analysis
    let trend_slope = if decomposed.trend.length() > 1 {
      (decomposed.trend[decomposed.trend.length() - 1] - decomposed.trend[0]) / (decomposed.trend.length() - 1).to_float()
    } else {
      0.0
    }
    
    // Seasonal analysis
    let seasonal_amplitude = if decomposed.seasonal.length() > 0 {
      let max = decomposed.seasonal.reduce(fn(acc, s) { if s > acc { s } else { acc } , decomposed.seasonal[0])
      let min = decomposed.seasonal.reduce(fn(acc, s) { if s < acc { s } else { acc } , decomposed.seasonal[0])
      (max - min) / 2.0
    } else {
      0.0
    }
    
    // Residual analysis
    let residual_mean = decomposed.residual.reduce(fn(acc, r) { acc + r }, 0.0) / decomposed.residual.length().to_float()
    let residual_variance = decomposed.residual.reduce(fn(acc, r) { 
      acc + (r - residual_mean) * (r - residual_mean)
    }, 0.0) / decomposed.residual.length().to_float()
    
    {
      trend_slope,
      seasonal_amplitude,
      residual_mean,
      residual_variance
    }
  }
  
  let component_analysis = analyze_components(decomposed)
  
  assert_true(component_analysis.trend_slope > 0.0)  // Should be increasing
  assert_true(component_analysis.seasonal_amplitude > 0.0)  // Should have seasonal variation
  assert_true(component_analysis.residual_mean.abs() < 1.0)  // Should be close to zero
  assert_true(component_analysis.residual_variance > 0.0)  // Should have some variance
}

// Test 10: Time Series Indexing and Querying
test "time series indexing and querying" {
  // Time series data point structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: Option[String]
  }
  
  type QueryFilter = {
    start_time: Option[Int],
    end_time: Option[Int],
    tags: Array[(String, String)],
    value_min: Option<Float>,
    value_max: Option<Float]
  }
  
  type IndexEntry = {
    timestamp: Int,
    series_name: String,
    point_index: Int,
    tags: Array[(String, String)]
  }
  
  // Create test time series
  let create_indexed_series = fn() {
    let mut cpu_points = []
    let mut memory_points = []
    let mut network_points = []
    
    for i in 0..72 {  // 3 days of hourly data
      let timestamp = 1640995200 + i * 3600
      
      // CPU usage
      let cpu_value = 30.0 + 20.0 * (i.to_float() * 0.1).sin() + (i % 5).to_float() * 2.0
      cpu_points = cpu_points.push({
        timestamp,
        value: cpu_value,
        tags: [
          ("host", "server-1"),
          ("metric", "cpu"),
          ("region", "us-west-1"),
          ("environment", "production")
        ]
      })
      
      // Memory usage
      let memory_value = 40.0 + 15.0 * (i.to_float() * 0.15).sin() + (i % 7).to_float() * 1.5
      memory_points = memory_points.push({
        timestamp,
        value: memory_value,
        tags: [
          ("host", "server-1"),
          ("metric", "memory"),
          ("region", "us-west-1"),
          ("environment", "production")
        ]
      })
      
      // Network usage
      let network_value = 25.0 + 10.0 * (i.to_float() * 0.2).sin() + (i % 3).to_float() * 3.0
      network_points = network_points.push({
        timestamp,
        value: network_value,
        tags: [
          ("host", "server-2"),
          ("metric", "network"),
          ("region", "us-east-1"),
          ("environment", "staging")
        ]
      })
    }
    
    [
      {
        name: "cpu_usage",
        points: cpu_points,
        aggregation: None
      },
      {
        name: "memory_usage",
        points: memory_points,
        aggregation: None
      },
      {
        name: "network_usage",
        points: network_points,
        aggregation: None
      }
    ]
  }
  
  let indexed_series = create_indexed_series()
  assert_eq(indexed_series.length(), 3)
  
  // Create time series index
  let create_time_series_index = fn(series_list: Array[TimeSeries]) {
    let mut index = []
    
    for series in series_list {
      for i in 0..series.points.length() {
        index = index.push({
          timestamp: series.points[i].timestamp,
          series_name: series.name,
          point_index: i,
          tags: series.points[i].tags
        })
      }
    }
    
    // Sort by timestamp for efficient range queries
    index.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
    index
  }
  
  let time_series_index = create_time_series_index(indexed_series)
  assert_eq(time_series_index.length(), 216)  // 3 series * 72 points each
  
  // Verify index is sorted by timestamp
  for i in 1..time_series_index.length() {
    assert_true(time_series_index[i].timestamp >= time_series_index[i-1].timestamp)
  }
  
  // Query functions
  let query_by_time_range = fn(index: Array[IndexEntry], series_list: Array[TimeSeries], start_time: Int, end_time: Int) {
    let mut results = []
    
    // Binary search for start position
    let mut left = 0
    let mut right = index.length()
    
    while left < right {
      let mid = left + (right - left) / 2
      if index[mid].timestamp < start_time {
        left = mid + 1
      } else {
        right = mid
      }
    }
    
    let start_pos = left
    
    // Collect all entries within time range
    for i in start_pos..index.length() {
      if index[i].timestamp >= start_time && index[i].timestamp <= end_time {
        let entry = index[i]
        
        // Find the series
        let mut target_series = None
        for series in series_list {
          if series.name == entry.series_name {
            target_series = Some(series)
            break
          }
        }
        
        match target_series {
          Some(series) => {
            results = results.push(series.points[entry.point_index])
          }
          None => {}
        }
      } else if index[i].timestamp > end_time {
        break
      }
    }
    
    results
  }
  
  let query_by_tags = fn(index: Array[IndexEntry], series_list: Array[TimeSeries], tags: Array[(String, String)]) {
    let mut results = []
    
    for entry in index {
      let mut all_tags_match = true
      
      for tag in tags {
        if not(entry.tags.contains(tag)) {
          all_tags_match = false
          break
        }
      }
      
      if all_tags_match {
        // Find the series
        let mut target_series = None
        for series in series_list {
          if series.name == entry.series_name {
            target_series = Some(series)
            break
          }
        }
        
        match target_series {
          Some(series) => {
            results = results.push(series.points[entry.point_index])
          }
          None => {}
        }
      }
    }
    
    results
  }
  
  let query_by_value_range = fn(index: Array[IndexEntry], series_list: Array[TimeSeries], min_value: Float, max_value: Float) {
    let mut results = []
    
    for entry in index {
      // Find the series
      let mut target_series = None
      for series in series_list {
        if series.name == entry.series_name {
          target_series = Some(series)
          break
        }
      }
      
      match target_series {
        Some(series) => {
          let point = series.points[entry.point_index]
          if point.value >= min_value && point.value <= max_value {
            results = results.push(point)
          }
        }
        None => {}
      }
    }
    
    results
  }
  
  let complex_query = fn(index: Array[IndexEntry], series_list: Array[TimeSeries], filter: QueryFilter) {
    let mut results = []
    
    for entry in index {
      // Check time range
      let time_match = match filter.start_time {
        Some(start) => match filter.end_time {
          Some(end) => entry.timestamp >= start && entry.timestamp <= end
          None => entry.timestamp >= start
        }
        None => match filter.end_time {
          Some(end) => entry.timestamp <= end
          None => true
        }
      }
      
      if not(time_match) {
        continue
      }
      
      // Check tags
      let mut tags_match = true
      for tag in filter.tags {
        if not(entry.tags.contains(tag)) {
          tags_match = false
          break
        }
      }
      
      if not(tags_match) {
        continue
      }
      
      // Check value range
      let value_match = {
        // Find the series
        let mut target_series = None
        for series in series_list {
          if series.name == entry.series_name {
            target_series = Some(series)
            break
          }
        }
        
        match target_series {
          Some(series) => {
            let point = series.points[entry.point_index]
            let min_match = match filter.value_min {
              Some(min) => point.value >= min
              None => true
            }
            
            let max_match = match filter.value_max {
              Some(max) => point.value <= max
              None => true
            }
            
            min_match && max_match
          }
          None => false
        }
      }
      
      if value_match {
        // Find the series and add the point
        let mut target_series = None
        for series in series_list {
          if series.name == entry.series_name {
            target_series = Some(series)
            break
          }
        }
        
        match target_series {
          Some(series) => {
            results = results.push(series.points[entry.point_index])
          }
          None => {}
        }
      }
    }
    
    results
  }
  
  // Test time range query
  let start_time = 1640995200 + 24 * 3600  // Start at day 2
  let end_time = 1640995200 + 48 * 3600    // End at day 3
  
  let time_range_results = query_by_time_range(time_series_index, indexed_series, start_time, end_time)
  assert_eq(time_range_results.length(), 72)  // 3 series * 24 hours each
  
  // Verify all results are within time range
  for point in time_range_results {
    assert_true(point.timestamp >= start_time && point.timestamp <= end_time)
  }
  
  // Test tag query
  let tag_results = query_by_tags(time_series_index, indexed_series, [("host", "server-1")])
  assert_eq(tag_results.length(), 144)  // 2 series * 72 points each
  
  // Verify all results have the matching tag
  for point in tag_results {
    assert_true(point.tags.contains(("host", "server-1")))
  }
  
  // Test value range query
  let value_results = query_by_value_range(time_series_index, indexed_series, 40.0, 60.0)
  assert_true(value_results.length() > 0)
  
  // Verify all results are within value range
  for point in value_results {
    assert_true(point.value >= 40.0 && point.value <= 60.0)
  }
  
  // Test complex query
  let complex_filter = {
    start_time: Some(1640995200 + 12 * 3600),  // Start at 12 hours in
    end_time: Some(1640995200 + 36 * 3600),    // End at 36 hours in
    tags: [("metric", "cpu")],
    value_min: Some(35.0),
    value_max: Some(45.0)
  }
  
  let complex_results = complex_query(time_series_index, indexed_series, complex_filter)
  assert_true(complex_results.length() > 0)
  
  // Verify all results match all criteria
  for point in complex_results {
    assert_true(point.timestamp >= complex_filter.start_time.unwrap())
    assert_true(point.timestamp <= complex_filter.end_time.unwrap())
    assert_true(point.tags.contains(("metric", "cpu")))
    assert_true(point.value >= complex_filter.value_min.unwrap())
    assert_true(point.value <= complex_filter.value_max.unwrap())
  }
  
  // Test aggregation query
  let aggregate_query_results = fn(results: Array[TimeSeriesPoint], aggregation: String) {
    match aggregation {
      "average" => {
        let sum = results.reduce(fn(acc, point) { acc + point.value }, 0.0)
        if results.length() > 0 {
          sum / results.length().to_float()
        } else {
          0.0
        }
      }
      "sum" => {
        results.reduce(fn(acc, point) { acc + point.value }, 0.0)
      }
      "min" => {
        results.reduce(fn(acc, point) { if point.value < acc { point.value } else { acc } , Float::infinity())
      }
      "max" => {
        results.reduce(fn(acc, point) { if point.value > acc { point.value } else { acc } , -Float::infinity())
      }
      "count" => {
        results.length().to_float()
      }
      _ => 0.0
    }
  }
  
  // Test aggregation
  let avg_value = aggregate_query_results(time_range_results, "average")
  assert_true(avg_value > 0.0)
  
  let sum_value = aggregate_query_results(time_range_results, "sum")
  assert_true(sum_value > 0.0)
  
  let min_value = aggregate_query_results(time_range_results, "min")
  assert_true(min_value > 0.0)
  
  let max_value = aggregate_query_results(time_range_results, "max")
  assert_true(max_value > 0.0)
  
  let count_value = aggregate_query_results(time_range_results, "count")
  assert_eq(count_value, 72.0)
  
  // Verify min <= avg <= max
  assert_true(min_value <= avg_value)
  assert_true(avg_value <= max_value)
  
  // Test group by query
  let group_by_query = fn(results: Array[TimeSeriesPoint], group_by_tag: String) {
    let mut groups = {}
    
    for point in results {
      let group_value = match point.tags.find(fn(t) { t.0 == group_by_tag }) {
        Some(tag) => tag.1
        None => "unknown"
      }
      
      let current_group = match groups[group_value] {
        Some(group) => group,
        None => []
      }
      
      groups = groups.insert(group_value, current_group.push(point))
    }
    
    groups
  }
  
  let grouped_results = group_by_query(tag_results, "metric")
  assert_true(grouped_results.size() > 0)
  
  // Should have groups for "cpu" and "memory"
  assert_true(grouped_results.contains_key("cpu"))
  assert_true(grouped_results.contains_key("memory"))
  
  let cpu_group = grouped_results["cpu"]
  let memory_group = grouped_results["memory"]
  
  assert_eq(cpu_group.length(), 72)
  assert_eq(memory_group.length(), 72)
  
  // Verify all points in each group have the correct tag
  for point in cpu_group {
    assert_true(point.tags.contains(("metric", "cpu")))
  }
  
  for point in memory_group {
    assert_true(point.tags.contains(("metric", "memory")))
  }
}