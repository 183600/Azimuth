// Azimuth Premium Time Series Analytics Tests
// This file contains comprehensive test cases for time series data analysis and analytics

// Test 1: Time Series Data Point Operations
test "time series data point operations" {
  // Define time series data point structure
  type DataPoint = {
    timestamp: Int,
    value: Float,
    attributes: Array[(String, String)]
  }
  
  // Create data points
  let point1 = { timestamp: 1640995200, value: 100.5, attributes: [("source", "sensor1")] }
  let point2 = { timestamp: 1640995260, value: 102.3, attributes: [("source", "sensor1")] }
  let point3 = { timestamp: 1640995320, value: 98.7, attributes: [("source", "sensor1")] }
  
  // Test data point creation
  assert_eq(point1.timestamp, 1640995200)
  assert_eq(point1.value, 100.5)
  assert_eq(point1.attributes.length(), 1)
  assert_eq(point1.attributes[0], ("source", "sensor1"))
  
  // Test time difference calculation
  let time_diff = point2.timestamp - point1.timestamp
  assert_eq(time_diff, 60)
  
  // Test value difference calculation
  let value_diff = point2.value - point1.value
  assert_eq(value_diff, 1.8)
  
  // Test rate of change calculation
  let rate_of_change = value_diff / time_diff.to_float()
  assert_true(rate_of_change > 0.02 && rate_of_change < 0.04)
  
  // Test attribute filtering
  let has_source_attribute = fn(point: DataPoint) {
    point.attributes.any(fn(attr) { attr.0 == "source" })
  }
  assert_true(has_source_attribute(point1))
  assert_true(has_source_attribute(point2))
  assert_true(has_source_attribute(point3))
}

// Test 2: Time Series Aggregation Operations
test "time series aggregation operations" {
  // Define time series structure
  type TimeSeries = {
    name: String,
    data_points: Array[DataPoint],
    unit: String
  }
  
  // Create a time series with multiple data points
  let data_points = [
    { timestamp: 1640995200, value: 100.5, attributes: [] },
    { timestamp: 1640995260, value: 102.3, attributes: [] },
    { timestamp: 1640995320, value: 98.7, attributes: [] },
    { timestamp: 1640995380, value: 105.2, attributes: [] },
    { timestamp: 1640995440, value: 103.8, attributes: [] }
  ]
  
  let time_series = {
    name: "cpu_usage",
    data_points,
    unit: "percent"
  }
  
  // Test time series properties
  assert_eq(time_series.name, "cpu_usage")
  assert_eq(time_series.unit, "percent")
  assert_eq(time_series.data_points.length(), 5)
  
  // Calculate average value
  let sum = time_series.data_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
  let average = sum / time_series.data_points.length().to_float()
  assert_true(average > 102.0 && average < 103.0)
  
  // Find minimum and maximum values
  let min_value = time_series.data_points.reduce(fn(acc, point) { 
    if point.value < acc { point.value } else { acc } 
  }, 999999.0)
  let max_value = time_series.data_points.reduce(fn(acc, point) { 
    if point.value > acc { point.value } else { acc } 
  }, 0.0)
  
  assert_eq(min_value, 98.7)
  assert_eq(max_value, 105.2)
  
  // Calculate range
  let range = max_value - min_value
  assert_eq(range, 6.5)
  
  // Calculate variance (simplified)
  let variance = time_series.data_points.reduce(fn(acc, point) { 
    acc + (point.value - average) * (point.value - average) 
  }, 0.0) / time_series.data_points.length().to_float()
  
  assert_true(variance > 5.0 && variance < 10.0)
  
  // Calculate standard deviation
  let std_deviation = variance.sqrt()
  assert_true(std_deviation > 2.0 && std_deviation < 4.0)
}

// Test 3: Time Series Resampling and Interpolation
test "time series resampling and interpolation" {
  // Define resampling strategy
  enum ResamplingStrategy {
    Average
    Min
    Max
    First
    Last
  }
  
  // Resample time series to different intervals
  let resample = fn(series: TimeSeries, interval_seconds: Int, strategy: ResamplingStrategy) {
    let start_time = series.data_points[0].timestamp
    let end_time = series.data_points[series.data_points.length() - 1].timestamp
    
    let mut resampled_points = []
    let mut current_time = start_time
    
    while current_time <= end_time {
      let window_end = current_time + interval_seconds
      let window_points = series.data_points.filter(fn(point) { 
        point.timestamp >= current_time && point.timestamp < window_end 
      })
      
      if window_points.length() > 0 {
        let resampled_value = match strategy {
          ResamplingStrategy::Average => {
            let sum = window_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
            sum / window_points.length().to_float()
          }
          ResamplingStrategy::Min => {
            window_points.reduce(fn(acc, point) { 
              if point.value < acc { point.value } else { acc } 
            }, 999999.0)
          }
          ResamplingStrategy::Max => {
            window_points.reduce(fn(acc, point) { 
              if point.value > acc { point.value } else { acc } 
            }, 0.0)
          }
          ResamplingStrategy::First => window_points[0].value
          ResamplingStrategy::Last => window_points[window_points.length() - 1].value
        }
        
        resampled_points = resampled_points.push({
          timestamp: current_time,
          value: resampled_value,
          attributes: []
        })
      }
      
      current_time = current_time + interval_seconds
    }
    
    { series | data_points: resampled_points }
  }
  
  // Create test data with 30-second intervals
  let original_points = [
    { timestamp: 1640995200, value: 100.0, attributes: [] },
    { timestamp: 1640995230, value: 102.0, attributes: [] },
    { timestamp: 1640995260, value: 104.0, attributes: [] },
    { timestamp: 1640995290, value: 103.0, attributes: [] },
    { timestamp: 1640995320, value: 105.0, attributes: [] },
    { timestamp: 1640995350, value: 107.0, attributes: [] }
  ]
  
  let original_series = {
    name: "test_metric",
    data_points: original_points,
    unit: "value"
  }
  
  // Resample to 60-second intervals using average strategy
  let resampled_series = resample(original_series, 60, ResamplingStrategy::Average)
  
  assert_eq(resampled_series.data_points.length(), 3)
  assert_eq(resampled_series.data_points[0].timestamp, 1640995200)
  assert_eq(resampled_series.data_points[0].value, 101.0)  // Average of 100.0 and 102.0
  assert_eq(resampled_series.data_points[1].timestamp, 1640995260)
  assert_eq(resampled_series.data_points[1].value, 103.5)  // Average of 104.0 and 103.0
  assert_eq(resampled_series.data_points[2].timestamp, 1640995320)
  assert_eq(resampled_series.data_points[2].value, 106.0)  // Average of 105.0 and 107.0
  
  // Resample using max strategy
  let max_resampled = resample(original_series, 60, ResamplingStrategy::Max)
  assert_eq(max_resampled.data_points[0].value, 102.0)
  assert_eq(max_resampled.data_points[1].value, 104.0)
  assert_eq(max_resampled.data_points[2].value, 107.0)
  
  // Resample using min strategy
  let min_resampled = resample(original_series, 60, ResamplingStrategy::Min)
  assert_eq(min_resampled.data_points[0].value, 100.0)
  assert_eq(min_resampled.data_points[1].value, 103.0)
  assert_eq(min_resampled.data_points[2].value, 105.0)
}

// Test 4: Time Series Trend Analysis
test "time series trend analysis" {
  // Define trend types
  enum Trend {
    Increasing
    Decreasing
    Stable
    Volatile
  }
  
  // Analyze trend in time series
  let analyze_trend = fn(series: TimeSeries, threshold: Float) {
    if series.data_points.length() < 3 {
      Trend::Stable
    } else {
      let first_half = series.data_points.slice(0, series.data_points.length() / 2)
      let second_half = series.data_points.slice(series.data_points.length() / 2, series.data_points.length())
      
      let first_avg = first_half.reduce(fn(acc, point) { acc + point.value }, 0.0) / first_half.length().to_float()
      let second_avg = second_half.reduce(fn(acc, point) { acc + point.value }, 0.0) / second_half.length().to_float()
      
      let change_percent = (second_avg - first_avg) / first_avg * 100.0
      
      if change_percent > threshold {
        Trend::Increasing
      } else if change_percent < -threshold {
        Trend::Decreasing
      } else {
        // Check for volatility
        let variance = series.data_points.reduce(fn(acc, point) { 
          acc + (point.value - (first_avg + second_avg) / 2.0) * (point.value - (first_avg + second_avg) / 2.0) 
        }, 0.0) / series.data_points.length().to_float()
        
        let std_deviation = variance.sqrt()
        let avg_value = (first_avg + second_avg) / 2.0
        let coefficient_of_variation = std_deviation / avg_value * 100.0
        
        if coefficient_of_variation > threshold {
          Trend::Volatile
        } else {
          Trend::Stable
        }
      }
    }
  }
  
  // Test increasing trend
  let increasing_points = [
    { timestamp: 1640995200, value: 100.0, attributes: [] },
    { timestamp: 1640995260, value: 110.0, attributes: [] },
    { timestamp: 1640995320, value: 120.0, attributes: [] },
    { timestamp: 1640995380, value: 130.0, attributes: [] },
    { timestamp: 1640995440, value: 140.0, attributes: [] },
    { timestamp: 1640995500, value: 150.0, attributes: [] }
  ]
  
  let increasing_series = {
    name: "increasing_metric",
    data_points: increasing_points,
    unit: "value"
  }
  
  let increasing_trend = analyze_trend(increasing_series, 10.0)
  match increasing_trend {
    Trend::Increasing => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test decreasing trend
  let decreasing_points = [
    { timestamp: 1640995200, value: 150.0, attributes: [] },
    { timestamp: 1640995260, value: 140.0, attributes: [] },
    { timestamp: 1640995320, value: 130.0, attributes: [] },
    { timestamp: 1640995380, value: 120.0, attributes: [] },
    { timestamp: 1640995440, value: 110.0, attributes: [] },
    { timestamp: 1640995500, value: 100.0, attributes: [] }
  ]
  
  let decreasing_series = {
    name: "decreasing_metric",
    data_points: decreasing_points,
    unit: "value"
  }
  
  let decreasing_trend = analyze_trend(decreasing_series, 10.0)
  match decreasing_trend {
    Trend::Decreasing => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test stable trend
  let stable_points = [
    { timestamp: 1640995200, value: 100.0, attributes: [] },
    { timestamp: 1640995260, value: 102.0, attributes: [] },
    { timestamp: 1640995320, value: 98.0, attributes: [] },
    { timestamp: 1640995380, value: 101.0, attributes: [] },
    { timestamp: 1640995440, value: 99.0, attributes: [] },
    { timestamp: 1640995500, value: 100.0, attributes: [] }
  ]
  
  let stable_series = {
    name: "stable_metric",
    data_points: stable_points,
    unit: "value"
  }
  
  let stable_trend = analyze_trend(stable_series, 10.0)
  match stable_trend {
    Trend::Stable => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test volatile trend
  let volatile_points = [
    { timestamp: 1640995200, value: 50.0, attributes: [] },
    { timestamp: 1640995260, value: 150.0, attributes: [] },
    { timestamp: 1640995320, value: 75.0, attributes: [] },
    { timestamp: 1640995380, value: 125.0, attributes: [] },
    { timestamp: 1640995440, value: 60.0, attributes: [] },
    { timestamp: 1640995500, value: 140.0, attributes: [] }
  ]
  
  let volatile_series = {
    name: "volatile_metric",
    data_points: volatile_points,
    unit: "value"
  }
  
  let volatile_trend = analyze_trend(volatile_series, 10.0)
  match volatile_trend {
    Trend::Volatile => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 5: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Define anomaly types
  enum AnomalyType {
    Spike
    Drop
    Outlier
    PatternDeviation
  }
  
  // Define anomaly detection result
  type Anomaly = {
    timestamp: Int,
    value: Float,
    anomaly_type: AnomalyType,
    severity: Float,
    description: String
  }
  
  // Detect anomalies using statistical methods
  let detect_anomalies = fn(series: TimeSeries, z_threshold: Float) {
    if series.data_points.length() < 3 {
      []
    } else {
      let values = series.data_points.map(fn(point) { point.value })
      let mean = values.reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_float()
      let variance = values.reduce(fn(acc, val) { acc + (val - mean) * (val - mean) }, 0.0) / values.length().to_float()
      let std_dev = variance.sqrt()
      
      let mut anomalies = []
      
      for i in 1..series.data_points.length() - 1 {
        let current_point = series.data_points[i]
        let prev_point = series.data_points[i - 1]
        let next_point = series.data_points[i + 1]
        
        let z_score = (current_point.value - mean) / std_dev
        
        if z_score.abs() > z_threshold {
          let anomaly_type = if current_point.value > mean {
            AnomalyType::Spike
          } else {
            AnomalyType::Drop
          }
          
          let severity = z_score.abs()
          
          anomalies = anomalies.push({
            timestamp: current_point.timestamp,
            value: current_point.value,
            anomaly_type,
            severity,
            description: "Value " + z_score.abs().to_string() + " standard deviations from mean"
          })
        }
      }
      
      anomalies
    }
  }
  
  // Create test data with anomalies
  let points_with_anomalies = [
    { timestamp: 1640995200, value: 100.0, attributes: [] },
    { timestamp: 1640995260, value: 102.0, attributes: [] },
    { timestamp: 1640995320, value: 98.0, attributes: [] },
    { timestamp: 1640995380, value: 150.0, attributes: [] },  // Spike anomaly
    { timestamp: 1640995440, value: 101.0, attributes: [] },
    { timestamp: 1640995500, value: 99.0, attributes: [] },
    { timestamp: 1640995560, value: 50.0, attributes: [] },   // Drop anomaly
    { timestamp: 1640995620, value: 103.0, attributes: [] },
    { timestamp: 1640995680, value: 97.0, attributes: [] }
  ]
  
  let series_with_anomalies = {
    name: "metric_with_anomalies",
    data_points: points_with_anomalies,
    unit: "value"
  }
  
  // Detect anomalies with z-threshold of 2.0
  let detected_anomalies = detect_anomalies(series_with_anomalies, 2.0)
  
  // Should detect at least 2 anomalies (spike and drop)
  assert_true(detected_anomalies.length() >= 2)
  
  // Check spike anomaly
  let spike_anomalies = detected_anomalies.filter(fn(anomaly) { 
    match anomaly.anomaly_type {
      AnomalyType::Spike => true
      _ => false
    }
  })
  assert_true(spike_anomalies.length() >= 1)
  
  // Check drop anomaly
  let drop_anomalies = detected_anomalies.filter(fn(anomaly) { 
    match anomaly.anomaly_type {
      AnomalyType::Drop => true
      _ => false
    }
  })
  assert_true(drop_anomalies.length() >= 1)
  
  // Verify anomaly details
  if detected_anomalies.length() > 0 {
    let first_anomaly = detected_anomalies[0]
    assert_true(first_anomaly.severity > 2.0)
    assert_true(first_anomaly.timestamp > 0)
    assert_true(first_anomaly.description.length() > 0)
  }
}

// Test 6: Time Series Forecasting
test "time series forecasting" {
  // Simple moving average forecasting
  let moving_average_forecast = fn(series: TimeSeries, window_size: Int, forecast_periods: Int) {
    if series.data_points.length() < window_size {
      []
    } else {
      let mut forecasts = []
      let last_timestamp = series.data_points[series.data_points.length() - 1].timestamp
      let interval = if series.data_points.length() > 1 {
        series.data_points[1].timestamp - series.data_points[0].timestamp
      } else {
        60
      }
      
      for i in 1..=forecast_periods {
        let window_start = series.data_points.length() - window_size
        let window_end = series.data_points.length()
        let window_values = series.data_points.slice(window_start, window_end)
        let forecast_value = window_values.reduce(fn(acc, point) { acc + point.value }, 0.0) / window_values.length().to_float()
        let forecast_timestamp = last_timestamp + (i * interval)
        
        forecasts = forecasts.push({
          timestamp: forecast_timestamp,
          value: forecast_value,
          attributes: [("forecast", "true")]
        })
      }
      
      forecasts
    }
  }
  
  // Linear trend forecasting
  let linear_trend_forecast = fn(series: TimeSeries, forecast_periods: Int) {
    if series.data_points.length() < 2 {
      []
    } else {
      let n = series.data_points.length().to_float()
      let sum_x = (0..series.data_points.length()).reduce(fn(acc, i) { acc + i.to_float() }, 0.0)
      let sum_y = series.data_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
      let sum_xy = series.data_points.map_with_index(fn(point, i) { 
        i.to_float() * point.value 
      }).reduce(fn(acc, val) { acc + val }, 0.0)
      let sum_x2 = (0..series.data_points.length()).map(fn(i) { 
        i.to_float() * i.to_float() 
      }).reduce(fn(acc, val) { acc + val }, 0.0)
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      let mut forecasts = []
      let last_timestamp = series.data_points[series.data_points.length() - 1].timestamp
      let interval = if series.data_points.length() > 1 {
        series.data_points[1].timestamp - series.data_points[0].timestamp
      } else {
        60
      }
      
      for i in 1..=forecast_periods {
        let x = series.data_points.length().to_float() + i.to_float()
        let forecast_value = slope * x + intercept
        let forecast_timestamp = last_timestamp + (i * interval)
        
        forecasts = forecasts.push({
          timestamp: forecast_timestamp,
          value: forecast_value,
          attributes: [("forecast", "true"), ("method", "linear")]
        })
      }
      
      forecasts
    }
  }
  
  // Create test data with clear trend
  let trend_points = [
    { timestamp: 1640995200, value: 100.0, attributes: [] },
    { timestamp: 1640995260, value: 105.0, attributes: [] },
    { timestamp: 1640995320, value: 110.0, attributes: [] },
    { timestamp: 1640995380, value: 115.0, attributes: [] },
    { timestamp: 1640995440, value: 120.0, attributes: [] },
    { timestamp: 1640995500, value: 125.0, attributes: [] }
  ]
  
  let trend_series = {
    name: "trend_metric",
    data_points: trend_points,
    unit: "value"
  }
  
  // Test moving average forecast
  let ma_forecasts = moving_average_forecast(trend_series, 3, 3)
  assert_eq(ma_forecasts.length(), 3)
  
  // Check forecast timestamps
  assert_eq(ma_forecasts[0].timestamp, 1640995560)
  assert_eq(ma_forecasts[1].timestamp, 1640995620)
  assert_eq(ma_forecasts[2].timestamp, 1640995680)
  
  // Check forecast values (should be around the average of last 3 points)
  let expected_ma_value = (115.0 + 120.0 + 125.0) / 3.0
  assert_true(ma_forecasts[0].value > expected_ma_value - 0.1 && ma_forecasts[0].value < expected_ma_value + 0.1)
  
  // Test linear trend forecast
  let linear_forecasts = linear_trend_forecast(trend_series, 3)
  assert_eq(linear_forecasts.length(), 3)
  
  // Check forecast timestamps
  assert_eq(linear_forecasts[0].timestamp, 1640995560)
  assert_eq(linear_forecasts[1].timestamp, 1640995620)
  assert_eq(linear_forecasts[2].timestamp, 1640995680)
  
  // Check forecast values (should follow the linear trend)
  assert_true(linear_forecasts[0].value > 125.0)  // Should continue increasing
  assert_true(linear_forecasts[1].value > linear_forecasts[0].value)
  assert_true(linear_forecasts[2].value > linear_forecasts[1].value)
  
  // Check forecast attributes
  assert_true(linear_forecasts[0].attributes.contains(("forecast", "true")))
  assert_true(linear_forecasts[0].attributes.contains(("method", "linear")))
}