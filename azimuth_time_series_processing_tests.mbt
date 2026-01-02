// Azimuth Time Series Data Processing Test Suite
// This file contains comprehensive test cases for time series data processing

// Test 1: Basic Time Series Operations
test "basic time series operations" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let get_value_at = fn(series, timestamp) {
    for point in series.points {
      if point.timestamp == timestamp {
        return Some(point.value)
      }
    }
    None
  }
  
  let get_values_in_range = fn(series, start_time, end_time) {
    let mut result = []
    
    for point in series.points {
      if point.timestamp >= start_time and point.timestamp <= end_time {
        result = result @ [point]
      }
    }
    
    result
  }
  
  let calculate_average = fn(points) {
    if points.length() == 0 {
      0.0
    } else {
      let mut sum = 0.0
      for point in points {
        sum = sum + point.value
      }
      sum / points.length().to_float()
    }
  }
  
  let calculate_max = fn(points) {
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
  
  let calculate_min = fn(points) {
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
  
  // Test time series creation and operations
  let series = create_time_series("temperature")
  
  // Add points
  let series1 = add_point(add_point(add_point(series, 1000, 20.5), 2000, 21.0), 3000, 20.8)
  let series2 = add_point(add_point(add_point(series1, 4000, 22.1), 5000, 21.9), 6000, 21.5)
  
  assert_eq(series2.points.length(), 6)
  
  // Test value retrieval
  match get_value_at(series2, 2000) {
    Some(value) => assert_eq(value, 21.0)
    None => assert_true(false)
  }
  
  match get_value_at(series2, 2500) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test range queries
  let range_points = get_values_in_range(series2, 2000, 5000)
  assert_eq(range_points.length(), 4)
  
  // Test statistical calculations
  let average = calculate_average(series2.points)
  let max = calculate_max(series2.points)
  let min = calculate_min(series2.points)
  
  assert_true(average > 21.0 and average < 22.0)
  assert_eq(max, 22.1)
  assert_eq(min, 20.5)
  
  let range_average = calculate_average(range_points)
  assert_true(range_average > 21.0 and range_average < 22.0)
}

// Test 2: Time Series Aggregation
test "time series aggregation" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let aggregate_by_interval = fn(series, interval) {
    if series.points.length() == 0 {
      series
    } else {
      let mut aggregated_points = []
      let mut current_interval_start = (series.points[0].timestamp / interval) * interval
      let mut interval_values = []
      
      for point in series.points {
        let point_interval = (point.timestamp / interval) * interval
        
        if point_interval == current_interval_start {
          interval_values = interval_values @ [point.value]
        } else {
          // Process previous interval
          if interval_values.length() > 0 {
            let mut sum = 0.0
            for value in interval_values {
              sum = sum + value
            }
            let average = sum / interval_values.length().to_float()
            
            aggregated_points = aggregated_points @ [{
              timestamp: current_interval_start,
              value: average
            }]
          }
          
          // Start new interval
          current_interval_start = point_interval
          interval_values = [point.value]
        }
      }
      
      // Process last interval
      if interval_values.length() > 0 {
        let mut sum = 0.0
        for value in interval_values {
          sum = sum + value
        }
        let average = sum / interval_values.length().to_float()
        
        aggregated_points = aggregated_points @ [{
          timestamp: current_interval_start,
          value: average
        }]
      }
      
      { name: series.name, points: aggregated_points }
    }
  }
  
  let downsample = fn(series, factor) {
    if series.points.length() == 0 {
      series
    } else {
      let mut downsampled_points = []
      
      for i in 0..series.points.length() {
        if i % factor == 0 {
          downsampled_points = downsampled_points @ [series.points[i]]
        }
      }
      
      { name: series.name, points: downsampled_points }
    }
  }
  
  let calculate_moving_average = fn(series, window_size) {
    if series.points.length() < window_size {
      series
    } else {
      let mut moving_averages = []
      
      for i in window_size - 1..series.points.length() {
        let mut sum = 0.0
        for j in i - window_size + 1..i {
          sum = sum + series.points[j].value
        }
        let average = sum / window_size.to_float()
        
        moving_averages = moving_averages @ [{
          timestamp: series.points[i].timestamp,
          value: average
        }]
      }
      
      { name: series.name + "_ma", points: moving_averages }
    }
  }
  
  // Create test time series with 1-minute intervals
  let series = create_time_series("cpu_usage")
  
  let mut filled_series = series
  for i in 0..60 {
    filled_series = add_point(filled_series, i * 60, 50.0 + (i % 10).to_float())  // 50-59 pattern
  }
  
  assert_eq(filled_series.points.length(), 60)
  
  // Test aggregation by 5-minute intervals
  let aggregated = aggregate_by_interval(filled_series, 300)  // 5 minutes = 300 seconds
  assert_eq(aggregated.points.length(), 12)  // 60 points / 5-minute intervals
  
  // Verify first aggregated point (average of first 5 points: 50, 51, 52, 53, 54)
  assert_true(aggregated.points[0].value > 51.0 and aggregated.points[0].value < 53.0)
  
  // Test downsampling
  let downsampled = downsample(filled_series, 10)  // Keep every 10th point
  assert_eq(downsampled.points.length(), 6)
  
  // Verify downsampled points
  assert_eq(downsampled.points[0].timestamp, 0)
  assert_eq(downsampled.points[0].value, 50.0)
  assert_eq(downsampled.points[1].timestamp, 600)
  assert_eq(downsampled.points[1].value, 60.0)
  
  // Test moving average
  let ma_series = calculate_moving_average(filled_series, 5)
  assert_eq(ma_series.points.length(), 56)  // 60 - 5 + 1
  
  // Verify first moving average point
  let expected_first_ma = (50.0 + 51.0 + 52.0 + 53.0 + 54.0) / 5.0
  assert_eq(ma_series.points[0].value, expected_first_ma)
}

// Test 3: Time Series Resampling
test "time series resampling" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let linear_interpolate = fn(series, target_timestamp) {
    // Find the two points to interpolate between
    let mut before_point = None
    let mut after_point = None
    
    for point in series.points {
      if point.timestamp <= target_timestamp {
        before_point = Some(point)
      } else {
        after_point = Some(point)
        break
      }
    }
    
    match (before_point, after_point) {
      (Some(before), Some(after)) => {
        // Linear interpolation
        let ratio = (target_timestamp - before.timestamp).to_float() / (after.timestamp - before.timestamp).to_float()
        let interpolated_value = before.value + ratio * (after.value - before.value)
        Some(interpolated_value)
      }
      (Some(point), None) => Some(point.value),  // Exact match or after last point
      (None, Some(point)) => Some(point.value),  // Before first point
      (None, None) => None
    }
  }
  
  let resample = fn(series, new_interval) {
    if series.points.length() < 2 {
      series
    } else {
      let start_time = series.points[0].timestamp
      let end_time = series.points[series.points.length() - 1].timestamp
      
      let mut resampled_points = []
      let mut current_time = start_time
      
      while current_time <= end_time {
        match linear_interpolate(series, current_time) {
          Some(value) => {
            resampled_points = resampled_points @ [{
              timestamp: current_time,
              value: value
            }]
          }
          None => ()
        }
        
        current_time = current_time + new_interval
      }
      
      { name: series.name + "_resampled", points: resampled_points }
    }
  }
  
  let fill_missing = fn(series, method) {
    if series.points.length() == 0 {
      series
    } else {
      let mut filled_points = []
      
      for i in 0..series.points.length() {
        filled_points = filled_points @ [series.points[i]]
        
        // Check if there's a gap to the next point
        if i < series.points.length() - 1 {
          let current_time = series.points[i].timestamp
          let next_time = series.points[i + 1].timestamp
          let expected_interval = if i > 0 {
            series.points[i].timestamp - series.points[i - 1].timestamp
          } else {
            next_time - current_time
          }
          
          if next_time - current_time > expected_interval {
            // Fill missing points
            let mut fill_time = current_time + expected_interval
            
            while fill_time < next_time {
              let fill_value = match method {
                "forward" => series.points[i].value,
                "backward" => series.points[i + 1].value,
                "linear" => {
                  match linear_interpolate(series, fill_time) {
                    Some(value) => value,
                    None => series.points[i].value
                  }
                }
                _ => series.points[i].value
              }
              
              filled_points = filled_points @ [{
                timestamp: fill_time,
                value: fill_value
              }]
              
              fill_time = fill_time + expected_interval
            }
          }
        }
      }
      
      { name: series.name + "_filled", points: filled_points }
    }
  }
  
  // Create test time series with irregular intervals
  let series = create_time_series("irregular")
  let series1 = add_point(add_point(add_point(series, 1000, 10.0), 1500, 20.0), 2500, 30.0)
  let series2 = add_point(add_point(add_point(series1, 3000, 40.0), 4500, 50.0), 5500, 60.0)
  
  assert_eq(series2.points.length(), 6)
  
  // Test linear interpolation
  match linear_interpolate(series2, 1250) {
    Some(value) => {
      // Should be halfway between 10.0 and 20.0
      assert_true(value > 14.0 and value < 16.0)
    }
    None => assert_true(false)
  }
  
  match linear_interpolate(series2, 2000) {
    Some(value) => {
      // Should be between 20.0 and 30.0
      assert_true(value > 24.0 and value < 26.0)
    }
    None => assert_true(false)
  }
  
  // Test resampling to regular intervals
  let resampled = resample(series2, 1000)  // 1000-second intervals
  assert_eq(resampled.points.length(), 5)  // From 1000 to 5000
  
  // Verify resampled points
  assert_eq(resampled.points[0].timestamp, 1000)
  assert_eq(resampled.points[0].value, 10.0)
  
  assert_eq(resampled.points[1].timestamp, 2000)
  assert_true(resampled.points[1].value > 24.0 and resampled.points[1].value < 26.0)
  
  // Test filling missing values
  let series_with_gaps = create_time_series("gaps")
  let series_with_gaps1 = add_point(add_point(add_point(series_with_gaps, 1000, 10.0), 2000, 20.0), 5000, 30.0)
  
  let filled_forward = fill_missing(series_with_gaps1, "forward")
  assert_eq(filled_forward.points.length(), 4)  // Original 3 + 1 filled
  
  // Verify filled point
  assert_eq(filled_forward.points[2].timestamp, 3000)
  assert_eq(filled_forward.points[2].value, 20.0)  // Forward fill
  
  let filled_backward = fill_missing(series_with_gaps1, "backward")
  assert_eq(filled_backward.points[2].timestamp, 3000)
  assert_eq(filled_backward.points[2].value, 30.0)  // Backward fill
  
  let filled_linear = fill_missing(series_with_gaps1, "linear")
  assert_eq(filled_linear.points[2].timestamp, 3000)
  assert_true(filled_linear.points[2].value > 20.0 and filled_linear.points[2].value < 30.0)  // Linear interpolation
}

// Test 4: Time Series Anomaly Detection
test "time series anomaly detection" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  type Anomaly {
    timestamp : Int
    value : Float
    expected_value : Float
    score : Float
    type : String  // "spike", "drop", "trend"
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let calculate_moving_average = fn(points, index, window_size) {
    if index < window_size - 1 {
      0.0
    } else {
      let mut sum = 0.0
      for i in index - window_size + 1..index {
        sum = sum + points[i].value
      }
      sum / window_size.to_float()
    }
  }
  
  let calculate_moving_std = fn(points, index, window_size) {
    if index < window_size - 1 {
      0.0
    } else {
      let average = calculate_moving_average(points, index, window_size)
      let mut sum_squares = 0.0
      
      for i in index - window_size + 1..index {
        let diff = points[i].value - average
        sum_squares = sum_squares + diff * diff
      }
      
      (sum_squares / window_size.to_float()).sqrt()
    }
  }
  
  let detect_anomalies = fn(series, window_size, threshold) {
    let mut anomalies = []
    
    for i in 0..series.points.length() {
      if i >= window_size {
        let average = calculate_moving_average(series.points, i - 1, window_size)
        let std = calculate_moving_std(series.points, i - 1, window_size)
        let current_value = series.points[i].value
        
        if std > 0.0 {
          let z_score = (current_value - average) / std
          
          if z_score.abs() > threshold {
            let anomaly_type = if z_score > 0 {
              "spike"
            } else {
              "drop"
            }
            
            anomalies = anomalies @ [{
              timestamp: series.points[i].timestamp,
              value: current_value,
              expected_value: average,
              score: z_score.abs(),
              type: anomaly_type
            }]
          }
        }
      }
    }
    
    anomalies
  }
  
  let detect_trend_anomalies = fn(series, window_size, trend_threshold) {
    let mut anomalies = []
    
    for i in window_size..series.points.length() {
      // Calculate trend over window
      let first_value = series.points[i - window_size].value
      let last_value = series.points[i].value
      let trend = (last_value - first_value) / window_size.to_float()
      
      if trend.abs() > trend_threshold {
        anomalies = anomalies @ [{
          timestamp: series.points[i].timestamp,
          value: last_value,
          expected_value: first_value,
          score: trend.abs(),
          type: "trend"
        }]
      }
    }
    
    anomalies
  }
  
  // Create test time series with normal data and anomalies
  let series = create_time_series("test")
  
  // Add normal data (values around 50)
  let mut normal_series = series
  for i in 0..20 {
    normal_series = add_point(normal_series, i * 100, 50.0 + (i % 5).to_float() - 2.0)
  }
  
  // Add a spike anomaly
  let series_with_spike = add_point(normal_series, 2000, 80.0)
  
  // Add more normal data
  let mut more_normal_series = series_with_spike
  for i in 21..40 {
    more_normal_series = add_point(more_normal_series, i * 100, 50.0 + (i % 5).to_float() - 2.0)
  }
  
  // Add a drop anomaly
  let series_with_drop = add_point(more_normal_series, 4000, 20.0)
  
  // Add more normal data
  let mut final_normal_series = series_with_drop
  for i in 41..60 {
    final_normal_series = add_point(final_normal_series, i * 100, 50.0 + (i % 5).to_float() - 2.0)
  }
  
  // Add a trend anomaly
  let series_with_trend = add_point(final_normal_series, 6000, 70.0)
  
  assert_eq(series_with_trend.points.length(), 61)
  
  // Test anomaly detection
  let anomalies = detect_anomalies(series_with_trend, 10, 2.0)
  
  assert_eq(anomalies.length(), 2)  // Should detect spike and drop
  
  // Verify spike anomaly
  assert_eq(anomalies[0].type, "spike")
  assert_eq(anomalies[0].timestamp, 2000)
  assert_eq(anomalies[0].value, 80.0)
  assert_true(anomalies[0].score > 2.0)
  
  // Verify drop anomaly
  assert_eq(anomalies[1].type, "drop")
  assert_eq(anomalies[1].timestamp, 4000)
  assert_eq(anomalies[1].value, 20.0)
  assert_true(anomalies[1].score > 2.0)
  
  // Test trend anomaly detection
  let trend_anomalies = detect_trend_anomalies(series_with_trend, 10, 1.0)
  
  assert_eq(trend_anomalies.length(), 1)  // Should detect trend anomaly
  
  // Verify trend anomaly
  assert_eq(trend_anomalies[0].type, "trend")
  assert_eq(trend_anomalies[0].timestamp, 6000)
  assert_eq(trend_anomalies[0].value, 70.0)
  assert_true(trend_anomalies[0].score > 1.0)
}

// Test 5: Time Series Forecasting
test "time series forecasting" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  type Forecast {
    timestamp : Int
    value : Float
    confidence_lower : Float
    confidence_upper : Float
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let simple_moving_average_forecast = fn(series, periods) {
    if series.points.length() == 0 {
      []
    } else {
      let mut forecasts = []
      let last_timestamp = series.points[series.points.length() - 1].timestamp
      
      // Calculate SMA from last 10 points
      let window_size = if series.points.length() < 10 {
        series.points.length()
      } else {
        10
      }
      
      let mut sum = 0.0
      for i in series.points.length() - window_size..series.points.length() {
        sum = sum + series.points[i].value
      }
      let sma = sum / window_size.to_float()
      
      // Generate forecasts
      for i in 1..periods + 1 {
        forecasts = forecasts @ [{
          timestamp: last_timestamp + i * 100,
          value: sma,
          confidence_lower: sma * 0.9,
          confidence_upper: sma * 1.1
        }]
      }
      
      forecasts
    }
  }
  
  let linear_regression_forecast = fn(series, periods) {
    if series.points.length() < 2 {
      []
    } else {
      let mut forecasts = []
      let last_timestamp = series.points[series.points.length() - 1].timestamp
      
      // Calculate linear regression
      let n = series.points.length().to_float()
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      
      for i in 0..series.points.length() {
        let x = i.to_float()
        let y = series.points[i].value
        
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = sum_xy + x * y
        sum_x2 = sum_x2 + x * x
      }
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      // Calculate standard error for confidence intervals
      let mut sum_squared_errors = 0.0
      for i in 0..series.points.length() {
        let x = i.to_float()
        let predicted = slope * x + intercept
        let error = series.points[i].value - predicted
        sum_squared_errors = sum_squared_errors + error * error
      }
      
      let mse = sum_squared_errors / n
      let std_error = mse.sqrt()
      
      // Generate forecasts
      for i in 1..periods + 1 {
        let x = (series.points.length() + i - 1).to_float()
        let predicted_value = slope * x + intercept
        
        forecasts = forecasts @ [{
          timestamp: last_timestamp + i * 100,
          value: predicted_value,
          confidence_lower: predicted_value - 1.96 * std_error,
          confidence_upper: predicted_value + 1.96 * std_error
        }]
      }
      
      forecasts
    }
  }
  
  let seasonal_naive_forecast = fn(series, periods, season_length) {
    if series.points.length() < season_length {
      []
    } else {
      let mut forecasts = []
      let last_timestamp = series.points[series.points.length() - 1].timestamp
      
      // Generate forecasts using seasonal pattern
      for i in 1..periods + 1 {
        let seasonal_index = (series.points.length() - season_length + i - 1) % season_length
        let seasonal_value = series.points[seasonal_index].value
        
        forecasts = forecasts @ [{
          timestamp: last_timestamp + i * 100,
          value: seasonal_value,
          confidence_lower: seasonal_value * 0.8,
          confidence_upper: seasonal_value * 1.2
        }]
      }
      
      forecasts
    }
  }
  
  // Create test time series with trend and seasonality
  let series = create_time_series("test")
  
  let mut trend_seasonal_series = series
  for i in 0..50 {
    // Trend: 0.5 per period
    // Seasonality: sine wave with period 10
    let trend = 0.5 * i.to_float()
    let seasonality = 5.0 * ((2.0 * 3.14159 * i.to_float() / 10.0).sin())
    let value = 100.0 + trend + seasonality
    
    trend_seasonal_series = add_point(trend_seasonal_series, i * 100, value)
  }
  
  assert_eq(trend_seasonal_series.points.length(), 50)
  
  // Test SMA forecast
  let sma_forecasts = simple_moving_average_forecast(trend_seasonal_series, 5)
  assert_eq(sma_forecasts.length(), 5)
  
  // Verify SMA forecast structure
  for forecast in sma_forecasts {
    assert_true(forecast.confidence_lower < forecast.value)
    assert_true(forecast.confidence_upper > forecast.value)
  }
  
  // Test linear regression forecast
  let lr_forecasts = linear_regression_forecast(trend_seasonal_series, 5)
  assert_eq(lr_forecasts.length(), 5)
  
  // Verify LR forecast shows upward trend
  assert_true(lr_forecasts[4].value > lr_forecasts[0].value)
  
  // Test seasonal naive forecast
  let seasonal_forecasts = seasonal_naive_forecast(trend_seasonal_series, 5, 10)
  assert_eq(seasonal_forecasts.length(), 5)
  
  // Verify seasonal forecast pattern
  // The first forecast should be similar to the point 10 periods ago
  let expected_seasonal_value = trend_seasonal_series.points[40].value
  assert_true((seasonal_forecasts[0].value - expected_seasonal_value).abs() < 0.1)
}

// Test 6: Time Series Compression
test "time series compression" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let delta_encode = fn(series) {
    if series.points.length() == 0 {
      series
    } else {
      let mut encoded_points = [series.points[0]]
      
      for i in 1..series.points.length() {
        let prev_point = series.points[i - 1]
        let current_point = series.points[i]
        
        let delta_timestamp = current_point.timestamp - prev_point.timestamp
        let delta_value = current_point.value - prev_point.value
        
        encoded_points = encoded_points @ [{
          timestamp: delta_timestamp,
          value: delta_value
        }]
      }
      
      { name: series.name + "_delta", points: encoded_points }
    }
  }
  
  let delta_decode = fn(encoded_series) {
    if encoded_series.points.length() == 0 {
      encoded_series
    } else {
      let mut decoded_points = [encoded_series.points[0]]
      
      for i in 1..encoded_series.points.length() {
        let prev_point = decoded_points[i - 1]
        let delta_point = encoded_series.points[i]
        
        decoded_points = decoded_points @ [{
          timestamp: prev_point.timestamp + delta_point.timestamp,
          value: prev_point.value + delta_point.value
        }]
      }
      
      { name: encoded_series.name + "_decoded", points: decoded_points }
    }
  }
  
  let swing_compression = fn(series, threshold) {
    if series.points.length() < 3 {
      series
    } else {
      let mut compressed_points = [series.points[0]]
      
      for i in 1..series.points.length() - 1 {
        let prev_point = compressed_points[compressed_points.length() - 1]
        let current_point = series.points[i]
        let next_point = series.points[i + 1]
        
        // Calculate deviation from straight line between prev and next
        let line_value = prev_point.value + (next_point.value - prev_point.value) * 
                        ((current_point.timestamp - prev_point.timestamp).to_float() / 
                         (next_point.timestamp - prev_point.timestamp).to_float())
        
        let deviation = (current_point.value - line_value).abs()
        
        if deviation > threshold {
          compressed_points = compressed_points @ [current_point]
        }
      }
      
      // Always include the last point
      compressed_points = compressed_points @ [series.points[series.points.length() - 1]]
      
      { name: series.name + "_compressed", points: compressed_points }
    }
  }
  
  let calculate_compression_ratio = fn(original, compressed) {
    original.points.length().to_float() / compressed.points.length().to_float()
  }
  
  let calculate_max_error = fn(original, compressed) {
    let mut max_error = 0.0
    
    for orig_point in original.points {
      // Find closest point in compressed series
      let mut closest_point = compressed.points[0]
      let mut min_time_diff = (orig_point.timestamp - closest_point.timestamp).abs()
      
      for comp_point in compressed.points {
        let time_diff = (orig_point.timestamp - comp_point.timestamp).abs()
        if time_diff < min_time_diff {
          min_time_diff = time_diff
          closest_point = comp_point
        }
      }
      
      let error = (orig_point.value - closest_point.value).abs()
      if error > max_error {
        max_error = error
      }
    }
    
    max_error
  }
  
  // Create test time series
  let series = create_time_series("test")
  
  let mut filled_series = series
  for i in 0..100 {
    // Create a relatively smooth time series with small variations
    let base_value = 50.0 + 0.1 * i.to_float()
    let noise = (i % 7).to_float() - 3.0  // Small noise
    filled_series = add_point(filled_series, i * 10, base_value + noise)
  }
  
  assert_eq(filled_series.points.length(), 100)
  
  // Test delta encoding
  let encoded = delta_encode(filled_series)
  assert_eq(encoded.points.length(), 100)
  
  // First point should be the same
  assert_eq(encoded.points[0].timestamp, filled_series.points[0].timestamp)
  assert_eq(encoded.points[0].value, filled_series.points[0].value)
  
  // Subsequent points should be deltas
  assert_eq(encoded.points[1].timestamp, 10)  // Time delta
  assert_true(encoded.points[1].value < 1.0)   // Small value delta
  
  // Test delta decoding
  let decoded = delta_decode(encoded)
  assert_eq(decoded.points.length(), 100)
  
  // Verify decoded points match original
  for i in 0..filled_series.points.length() {
    assert_eq(decoded.points[i].timestamp, filled_series.points[i].timestamp)
    assert_true((decoded.points[i].value - filled_series.points[i].value).abs() < 0.001)
  }
  
  // Test swing compression
  let compressed = swing_compression(filled_series, 1.0)
  assert_true(compressed.points.length() < filled_series.points.length())
  
  // Calculate compression ratio
  let compression_ratio = calculate_compression_ratio(filled_series, compressed)
  assert_true(compression_ratio > 1.0)
  
  // Calculate maximum error
  let max_error = calculate_max_error(filled_series, compressed)
  assert_true(max_error <= 1.0)  // Should be within our threshold
  
  // Test with different threshold
  let highly_compressed = swing_compression(filled_series, 5.0)
  let high_compression_ratio = calculate_compression_ratio(filled_series, highly_compressed)
  let high_max_error = calculate_max_error(filled_series, highly_compressed)
  
  assert_true(high_compression_ratio > compression_ratio)
  assert_true(high_max_error > max_error)
}

// Test 7: Time Series Pattern Matching
test "time series pattern matching" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  type PatternMatch {
    start_index : Int
    end_index : Int
    score : Float
    pattern_name : String
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let normalize_series = fn(series) {
    if series.points.length() == 0 {
      series
    } else {
      // Find min and max values
      let mut min_value = series.points[0].value
      let mut max_value = series.points[0].value
      
      for point in series.points {
        if point.value < min_value {
          min_value = point.value
        }
        if point.value > max_value {
          max_value = point.value
        }
      }
      
      let range = max_value - min_value
      
      if range == 0.0 {
        series
      } else {
        let mut normalized_points = []
        
        for point in series.points {
          let normalized_value = (point.value - min_value) / range
          normalized_points = normalized_points @ [{
            timestamp: point.timestamp,
            value: normalized_value
          }]
        }
        
        { name: series.name + "_normalized", points: normalized_points }
      }
    }
  }
  
  let calculate_similarity = fn(series1, series2, start1, start2, length) {
    if start1 + length > series1.points.length() or start2 + length > series2.points.length() {
      0.0
    } else {
      let mut sum_squared_diff = 0.0
      
      for i in 0..length {
        let diff = series1.points[start1 + i].value - series2.points[start2 + i].value
        sum_squared_diff = sum_squared_diff + diff * diff
      }
      
      let mse = sum_squared_diff / length.to_float()
      1.0 / (1.0 + mse)  // Convert to similarity score (0 to 1)
    }
  }
  
  let find_pattern_matches = fn(series, pattern, min_similarity) {
    let matches = []
    
    // Normalize both series
    let normalized_series = normalize_series(series)
    let normalized_pattern = normalize_series(pattern)
    
    // Slide pattern across series
    for i in 0..normalized_series.points.length() - normalized_pattern.points.length() + 1 {
      let similarity = calculate_similarity(
        normalized_series, 
        normalized_pattern, 
        i, 
        0, 
        normalized_pattern.points.length()
      )
      
      if similarity >= min_similarity {
        matches = matches @ [{
          start_index: i,
          end_index: i + normalized_pattern.points.length() - 1,
          score: similarity,
          pattern_name: pattern.name
        }]
      }
    }
    
    matches
  }
  
  let detect_peaks = fn(series, threshold, window_size) {
    let peaks = []
    
    for i in window_size..series.points.length() - window_size {
      let current_value = series.points[i].value
      
      // Check if current point is a peak
      let mut is_peak = true
      
      for j in i - window_size..i + window_size + 1 {
        if j != i and series.points[j].value >= current_value {
          is_peak = false
          break
        }
      }
      
      if is_peak and current_value >= threshold {
        peaks = peaks @ [i]
      }
    }
    
    peaks
  }
  
  // Create test time series
  let series = create_time_series("test")
  
  // Create a series with various patterns
  let mut pattern_series = series
  
  // Add baseline
  for i in 0..30 {
    pattern_series = add_point(pattern_series, i * 10, 10.0)
  }
  
  // Add a peak pattern
  for i in 30..40 {
    let peak_value = 10.0 + 10.0 * ((i - 30).to_float() / 5.0).sin()
    pattern_series = add_point(pattern_series, i * 10, peak_value)
  }
  
  // Add baseline again
  for i in 40..60 {
    pattern_series = add_point(pattern_series, i * 10, 10.0)
  }
  
  // Add another peak pattern
  for i in 60..70 {
    let peak_value = 10.0 + 10.0 * ((i - 60).to_float() / 5.0).sin()
    pattern_series = add_point(pattern_series, i * 10, peak_value)
  }
  
  // Add baseline again
  for i in 70..100 {
    pattern_series = add_point(pattern_series, i * 10, 10.0)
  }
  
  assert_eq(pattern_series.points.length(), 100)
  
  // Create a pattern to match (the peak pattern)
  let peak_pattern = create_time_series("peak")
  for i in 0..10 {
    let peak_value = 10.0 + 10.0 * (i.to_float() / 5.0).sin()
    peak_pattern = add_point(peak_pattern, i * 10, peak_value)
  }
  
  // Test pattern matching
  let matches = find_pattern_matches(pattern_series, peak_pattern, 0.8)
  
  assert_eq(matches.length(), 2)  // Should find two peak patterns
  
  // Verify first match
  assert_eq(matches[0].start_index, 30)
  assert_eq(matches[0].end_index, 39)
  assert_eq(matches[0].pattern_name, "peak")
  assert_true(matches[0].score >= 0.8)
  
  // Verify second match
  assert_eq(matches[1].start_index, 60)
  assert_eq(matches[1].end_index, 69)
  assert_eq(matches[1].pattern_name, "peak")
  assert_true(matches[1].score >= 0.8)
  
  // Test peak detection
  let peaks = detect_peaks(pattern_series, 15.0, 2)
  
  assert_eq(peaks.length(), 2)  // Should detect two peaks
  
  // Verify peak positions
  assert_eq(peaks[0], 35)  // Middle of first peak
  assert_eq(peaks[1], 65)  // Middle of second peak
}

// Test 8: Time Series Correlation Analysis
test "time series correlation analysis" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let align_series = fn(series1, series2) {
    // Find common timestamps and align values
    let mut aligned1 = []
    let mut aligned2 = []
    
    for point1 in series1.points {
      for point2 in series2.points {
        if point1.timestamp == point2.timestamp {
          aligned1 = aligned1 @ [point1.value]
          aligned2 = aligned2 @ [point2.value]
          break
        }
      }
    }
    
    (aligned1, aligned2)
  }
  
  let calculate_correlation = fn(values1, values2) {
    if values1.length() != values2.length() or values1.length() == 0 {
      0.0
    } else {
      let n = values1.length().to_float()
      
      // Calculate means
      let mut sum1 = 0.0
      let mut sum2 = 0.0
      
      for i in 0..values1.length() {
        sum1 = sum1 + values1[i]
        sum2 = sum2 + values2[i]
      }
      
      let mean1 = sum1 / n
      let mean2 = sum2 / n
      
      // Calculate correlation coefficient
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      let mut sum_y2 = 0.0
      
      for i in 0..values1.length() {
        let x = values1[i] - mean1
        let y = values2[i] - mean2
        
        sum_xy = sum_xy + x * y
        sum_x2 = sum_x2 + x * x
        sum_y2 = sum_y2 + y * y
      }
      
      if sum_x2 == 0.0 or sum_y2 == 0.0 {
        0.0
      } else {
        sum_xy / (sum_x2 * sum_y2).sqrt()
      }
    }
  }
  
  let calculate_cross_correlation = fn(series1, series2, max_lag) {
    let (values1, values2) = align_series(series1, series2)
    let correlations = []
    
    for lag in -max_lag..max_lag + 1 {
      let mut shifted_values2 = []
      
      if lag >= 0 {
        // Positive lag: shift series2 forward
        for i in 0..values2.length() {
          if i + lag < values1.length() {
            shifted_values2 = shifted_values2 @ [values2[i]]
          }
        }
        
        // Trim values1 to match
        let mut trimmed_values1 = []
        for i in lag..values1.length() {
          trimmed_values1 = trimmed_values1 @ [values1[i]]
        }
        
        let correlation = calculate_correlation(trimmed_values1, shifted_values2)
        correlations = correlations @ [(lag, correlation)]
      } else {
        // Negative lag: shift series2 backward
        for i in (-lag)..values2.length() {
          shifted_values2 = shifted_values2 @ [values2[i]]
        }
        
        // Trim values1 to match
        let mut trimmed_values1 = []
        for i in 0..values1.length() + lag {
          trimmed_values1 = trimmed_values1 @ [values1[i]]
        }
        
        let correlation = calculate_correlation(trimmed_values1, shifted_values2)
        correlations = correlations @ [(lag, correlation)]
      }
    }
    
    correlations
  }
  
  let find_best_lag = fn(cross_correlations) {
    let mut best_lag = 0
    let mut best_correlation = 0.0
    
    for (lag, correlation) in cross_correlations {
      if correlation.abs() > best_correlation.abs() {
        best_lag = lag
        best_correlation = correlation
      }
    }
    
    (best_lag, best_correlation)
  }
  
  // Create test time series
  let series1 = create_time_series("series1")
  let series2 = create_time_series("series2")
  let series3 = create_time_series("series3")
  
  // Create correlated series (series2 follows series1 with a lag)
  let mut correlated_series1 = series1
  let mut correlated_series2 = series2
  
  for i in 0..50 {
    let value1 = 10.0 + 5.0 * ((2.0 * 3.14159 * i.to_float() / 20.0).sin())
    correlated_series1 = add_point(correlated_series1, i * 10, value1)
    
    // Series2 follows series1 with a lag of 3
    if i >= 3 {
      let lagged_value1 = 10.0 + 5.0 * ((2.0 * 3.14159 * (i - 3).to_float() / 20.0).sin())
      correlated_series2 = add_point(correlated_series2, i * 10, lagged_value1)
    }
  }
  
  // Create uncorrelated series (series3)
  let mut uncorrelated_series = series3
  for i in 0..50 {
    let random_value = 10.0 + (i % 7).to_float() - 3.0
    uncorrelated_series = add_point(uncorrelated_series, i * 10, random_value)
  }
  
  // Test correlation between series1 and series2
  let (values1, values2) = align_series(correlated_series1, correlated_series2)
  let correlation = calculate_correlation(values1, values2)
  
  assert_true(correlation > 0.8)  // Should be highly correlated
  
  // Test correlation between series1 and series3
  let (values1_3, values3) = align_series(correlated_series1, uncorrelated_series)
  let correlation_1_3 = calculate_correlation(values1_3, values3)
  
  assert_true(correlation_1_3.abs() < 0.5)  // Should be weakly correlated
  
  // Test cross-correlation
  let cross_correlations = calculate_cross_correlation(correlated_series1, correlated_series2, 10)
  
  assert_eq(cross_correlations.length(), 21)  // From -10 to 10
  
  // Find best lag
  let (best_lag, best_correlation) = find_best_lag(cross_correlations)
  
  assert_eq(best_lag, 3)  // Should detect the lag of 3
  assert_true(best_correlation > 0.8)
}

// Test 9: Time Series Decomposition
test "time series decomposition" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  type Decomposition {
    trend : Array[Float]
    seasonal : Array[Float]
    residual : Array[Float]
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let calculate_moving_average = fn(values, window) {
    if values.length() < window {
      values
    } else {
      let mut result = []
      
      for i in 0..values.length() {
        if i < window / 2 or i >= values.length() - window / 2 {
          // Use smaller window at edges
          let edge_window = if i < window / 2 {
            i + window / 2 + 1
          } else {
            values.length() - i + window / 2
          }
          
          let mut sum = 0.0
          let start = if i < window / 2 {
            0
          } else {
            i - window / 2
          }
          
          let end = if i >= values.length() - window / 2 {
            values.length()
          } else {
            i + window / 2 + 1
          }
          
          for j in start..end {
            sum = sum + values[j]
          }
          
          result = result @ [sum / (end - start).to_float()]
        } else {
          // Use full window
          let mut sum = 0.0
          for j in i - window / 2..i + window / 2 + 1 {
            sum = sum + values[j]
          }
          
          result = result @ [sum / window.to_float()]
        }
      }
      
      result
    }
  }
  
  let classical_decomposition = fn(series, period) {
    if series.points.length() < period * 2 {
      // Not enough data for decomposition
      let values = []
      for point in series.points {
        values = values @ [point.value]
      }
      
      {
        trend: values,
        seasonal: [],
        residual: []
      }
    } else {
      // Extract values
      let mut values = []
      for point in series.points {
        values = values @ [point.value]
      }
      
      // Calculate trend using moving average
      let trend = calculate_moving_average(values, period)
      
      // Calculate detrended series
      let mut detrended = []
      for i in 0..values.length() {
        if i < trend.length() {
          detrended = detrended @ [values[i] - trend[i]]
        } else {
          detrended = detrended @ [0.0]
        }
      }
      
      // Calculate seasonal components
      let mut seasonal = []
      for i in 0..period {
        let mut seasonal_sum = 0.0
        let mut seasonal_count = 0
        
        for j in i..detrended.length() {
          if j % period == i {
            seasonal_sum = seasonal_sum + detrended[j]
            seasonal_count = seasonal_count + 1
          }
        }
        
        if seasonal_count > 0 {
          seasonal = seasonal @ [seasonal_sum / seasonal_count]
        } else {
          seasonal = seasonal @ [0.0]
        }
      }
      
      // Normalize seasonal components
      let mut seasonal_sum = 0.0
      for s in seasonal {
        seasonal_sum = seasonal_sum + s
      }
      let seasonal_mean = seasonal_sum / period.to_float()
      
      let mut normalized_seasonal = []
      for s in seasonal {
        normalized_seasonal = normalized_seasonal @ [s - seasonal_mean]
      }
      
      // Calculate residual
      let mut residual = []
      for i in 0..values.length() {
        if i < trend.length() {
          let seasonal_component = normalized_seasonal[i % period]
          residual = residual @ [values[i] - trend[i] - seasonal_component]
        } else {
          residual = residual @ [0.0]
        }
      }
      
      {
        trend: trend,
        seasonal: normalized_seasonal,
        residual: residual
      }
    }
  }
  
  let reconstruct_from_decomposition = fn(decomposition, period) {
    let mut reconstructed = []
    
    for i in 0..decomposition.trend.length() {
      let seasonal_component = if i < decomposition.seasonal.length() {
        decomposition.seasonal[i % period]
      } else {
        0.0
      }
      
      let residual_component = if i < decomposition.residual.length() {
        decomposition.residual[i]
      } else {
        0.0
      }
      
      reconstructed = reconstructed @ [decomposition.trend[i] + seasonal_component + residual_component]
    }
    
    reconstructed
  }
  
  // Create test time series with trend and seasonality
  let series = create_time_series("test")
  
  let mut trend_seasonal_series = series
  for i in 0..100 {
    // Trend: 0.5 per period
    // Seasonality: sine wave with period 12
    let trend = 0.5 * i.to_float()
    let seasonality = 10.0 * ((2.0 * 3.14159 * i.to_float() / 12.0).sin())
    let noise = (i % 7).to_float() - 3.0  // Random noise
    
    let value = 100.0 + trend + seasonality + noise
    trend_seasonal_series = add_point(trend_seasonal_series, i * 10, value)
  }
  
  assert_eq(trend_seasonal_series.points.length(), 100)
  
  // Test decomposition
  let decomposition = classical_decomposition(trend_seasonal_series, 12)
  
  assert_eq(decomposition.trend.length(), 100)
  assert_eq(decomposition.seasonal.length(), 12)
  assert_eq(decomposition.residual.length(), 100)
  
  // Verify trend is increasing
  assert_true(decomposition.trend[50] > decomposition.trend[0])
  
  // Verify seasonal components sum to approximately zero
  let mut seasonal_sum = 0.0
  for s in decomposition.seasonal {
    seasonal_sum = seasonal_sum + s
  }
  assert_true(seasonal_sum.abs() < 0.1)
  
  // Test reconstruction
  let reconstructed = reconstruct_from_decomposition(decomposition, 12)
  
  assert_eq(reconstructed.length(), 100)
  
  // Verify reconstruction is close to original
  let mut max_error = 0.0
  for i in 0..trend_seasonal_series.points.length() {
    let error = (reconstructed[i] - trend_seasonal_series.points[i].value).abs()
    if error > max_error {
      max_error = error
    }
  }
  
  assert_true(max_error < 1.0)  // Reconstruction should be close to original
}

// Test 10: Time Series Window Operations
test "time series window operations" {
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
  }
  
  type WindowResult {
    timestamp : Int
    value : Float
    window_start : Int
    window_end : Int
  }
  
  let create_time_series = fn(name) {
    { name: name, points: [] }
  }
  
  let add_point = fn(series, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    { name: series.name, points: series.points @ [point] }
  }
  
  let apply_window_function = fn(series, window_size, operation) {
    let mut results = []
    
    for i in window_size - 1..series.points.length() {
      let window_start = i - window_size + 1
      let window_end = i
      
      let mut window_values = []
      for j in window_start..window_end + 1 {
        window_values = window_values @ [series.points[j].value]
      }
      
      let result_value = match operation {
        "sum" => {
          let mut sum = 0.0
          for value in window_values {
            sum = sum + value
          }
          sum
        }
        "mean" => {
          let mut sum = 0.0
          for value in window_values {
            sum = sum + value
          }
          sum / window_values.length().to_float()
        }
        "min" => {
          let mut min = window_values[0]
          for value in window_values {
            if value < min {
              min = value
            }
          }
          min
        }
        "max" => {
          let mut max = window_values[0]
          for value in window_values {
            if value > max {
              max = value
            }
          }
          max
        }
        "std" => {
          let mut sum = 0.0
          for value in window_values {
            sum = sum + value
          }
          let mean = sum / window_values.length().to_float()
          
          let mut sum_squares = 0.0
          for value in window_values {
            let diff = value - mean
            sum_squares = sum_squares + diff * diff
          }
          
          (sum_squares / window_values.length().to_float()).sqrt()
        }
        _ => 0.0
      }
      
      results = results @ [{
        timestamp: series.points[i].timestamp,
        value: result_value,
        window_start: window_start,
        window_end: window_end
      }]
    }
    
    results
  }
  
  let rolling_aggregate = fn(series, window_size, aggregation_fn) {
    let mut results = []
    let mut window_sum = 0.0
    let mut window_count = 0
    
    for i in 0..series.points.length() {
      // Add current value to window
      window_sum = window_sum + series.points[i].value
      window_count = window_count + 1
      
      // Remove values outside the window
      if window_count > window_size {
        let remove_index = i - window_size
        window_sum = window_sum - series.points[remove_index].value
        window_count = window_count - 1
      }
      
      // Apply aggregation if window is full
      if window_count == window_size {
        let aggregated_value = aggregation_fn(window_sum, window_count)
        results = results @ [{
          timestamp: series.points[i].timestamp,
          value: aggregated_value,
          window_start: i - window_size + 1,
          window_end: i
        }]
      }
    }
    
    results
  }
  
  let expanding_window = fn(series, operation) {
    let mut results = []
    
    for i in 0..series.points.length() {
      let mut window_values = []
      for j in 0..i + 1 {
        window_values = window_values @ [series.points[j].value]
      }
      
      let result_value = match operation {
        "sum" => {
          let mut sum = 0.0
          for value in window_values {
            sum = sum + value
          }
          sum
        }
        "mean" => {
          let mut sum = 0.0
          for value in window_values {
            sum = sum + value
          }
          sum / window_values.length().to_float()
        }
        "cummax" => {
          let mut max = window_values[0]
          for value in window_values {
            if value > max {
              max = value
            }
          }
          max
        }
        "cummin" => {
          let mut min = window_values[0]
          for value in window_values {
            if value < min {
              min = value
            }
          }
          min
        }
        _ => 0.0
      }
      
      results = results @ [{
        timestamp: series.points[i].timestamp,
        value: result_value,
        window_start: 0,
        window_end: i
      }]
    }
    
    results
  }
  
  // Create test time series
  let series = create_time_series("test")
  
  let mut filled_series = series
  for i in 0..20 {
    filled_series = add_point(filled_series, i * 10, (i * i).to_float())  // Squares: 0, 1, 4, 9, 16, ...
  }
  
  assert_eq(filled_series.points.length(), 20)
  
  // Test window function with mean
  let mean_results = apply_window_function(filled_series, 5, "mean")
  assert_eq(mean_results.length(), 16)  // 20 - 5 + 1
  
  // Verify first mean result (average of first 5 values: 0, 1, 4, 9, 16)
  let expected_first_mean = (0.0 + 1.0 + 4.0 + 9.0 + 16.0) / 5.0
  assert_eq(mean_results[0].value, expected_first_mean)
  
  // Test window function with max
  let max_results = apply_window_function(filled_series, 3, "max")
  assert_eq(max_results.length(), 18)  // 20 - 3 + 1
  
  // Verify first max result (max of 0, 1, 4)
  assert_eq(max_results[0].value, 4.0)
  
  // Test rolling aggregate
  let sum_aggregate = fn(sum, count) { sum }
  let rolling_sum_results = rolling_aggregate(filled_series, 4, sum_aggregate)
  assert_eq(rolling_sum_results.length(), 17)  // 20 - 4 + 1
  
  // Verify first rolling sum (sum of first 4 values: 0, 1, 4, 9)
  assert_eq(rolling_sum_results[0].value, 14.0)
  
  // Test expanding window
  let expanding_sum_results = expanding_window(filled_series, "sum")
  assert_eq(expanding_sum_results.length(), 20)
  
  // Verify first few expanding sums
  assert_eq(expanding_sum_results[0].value, 0.0)  // Sum of [0]
  assert_eq(expanding_sum_results[1].value, 1.0)  // Sum of [0, 1]
  assert_eq(expanding_sum_results[2].value, 5.0)  // Sum of [0, 1, 4]
  assert_eq(expanding_sum_results[3].value, 14.0) // Sum of [0, 1, 4, 9]
  
  // Test expanding window with cummax
  let expanding_cummax_results = expanding_window(filled_series, "cummax")
  assert_eq(expanding_cummax_results[0].value, 0.0)  // Max of [0]
  assert_eq(expanding_cummax_results[1].value, 1.0)  // Max of [0, 1]
  assert_eq(expanding_cummax_results[2].value, 4.0)  // Max of [0, 1, 4]
  assert_eq(expanding_cummax_results[3].value, 9.0)  // Max of [0, 1, 4, 9]
}