// Time Series Data Processing Tests for Azimuth Telemetry System
// This file contains tests for time series data processing and analysis

// Test 1: Basic Time Series Data Structure
test "basic time series data structure" {
  // Define time series point
  type TimeSeriesPoint = {
    timestamp : Int
    value : Float
    metadata : {String: String}
  }
  
  // Define time series
  type TimeSeries = {
    name : String
    unit : String
    points : [TimeSeriesPoint]
    tags : {String: String}
  }
  
  // Create time series point
  let create_point = fn(timestamp : Int, value : Float, metadata : {String: String}) {
    {
      timestamp: timestamp,
      value: value,
      metadata: metadata
    }
  }
  
  // Create time series
  let create_time_series = fn(name : String, unit : String, tags : {String: String}) {
    {
      name: name,
      unit: unit,
      points: [],
      tags: tags
    }
  }
  
  // Add point to time series
  let add_point = fn(series : TimeSeries, point : TimeSeriesPoint) {
    { series | points: series.points.push(point) }
  }
  
  // Test time series creation
  let cpu_series = create_time_series("cpu_usage", "percent", {
    "host": "server1",
    "region": "us-west"
  })
  
  assert_eq(cpu_series.name, "cpu_usage")
  assert_eq(cpu_series.unit, "percent")
  assert_eq(cpu_series.points.length(), 0)
  assert_eq(cpu_series.tags.get("host").or_else(""), "server1")
  assert_eq(cpu_series.tags.get("region").or_else(""), "us-west")
  
  // Test adding points
  let point1 = create_point(1609459200, 45.5, {"source": "system_monitor"})
  let point2 = create_point(1609459260, 52.3, {"source": "system_monitor"})
  let point3 = create_point(1609459320, 38.7, {"source": "system_monitor"})
  
  let series_with_points = add_point(add_point(add_point(cpu_series, point1), point2), point3)
  
  assert_eq(series_with_points.points.length(), 3)
  assert_eq(series_with_points.points[0].timestamp, 1609459200)
  assert_eq(series_with_points.points[0].value, 45.5)
  assert_eq(series_with_points.points[2].timestamp, 1609459320)
  assert_eq(series_with_points.points[2].value, 38.7)
}

// Test 2: Time Series Aggregation
test "time series aggregation" {
  // Define aggregation types
  enum AggregationType {
    Sum
    Average
    Min
    Max
    Count
  }
  
  // Aggregate time series points
  let aggregate_points = fn(points : [TimeSeriesPoint], aggregation_type : AggregationType) {
    if points.length() == 0 {
      return 0.0
    }
    
    match aggregation_type {
      AggregationType::Sum => {
        points.reduce(fn(acc, point) { acc + point.value }, 0.0)
      }
      AggregationType::Average => {
        let sum = points.reduce(fn(acc, point) { acc + point.value }, 0.0)
        sum / points.length().to_float()
      }
      AggregationType::Min => {
        points.reduce(fn(acc, point) { if point.value < acc { point.value } else { acc } }, points[0].value)
      }
      AggregationType::Max => {
        points.reduce(fn(acc, point) { if point.value > acc { point.value } else { acc } }, points[0].value)
      }
      AggregationType::Count => {
        points.length().to_float()
      }
    }
  }
  
  // Create test points
  let points = [
    { timestamp: 1609459200, value: 10.5, metadata: {} },
    { timestamp: 1609459260, value: 20.3, metadata: {} },
    { timestamp: 1609459320, value: 15.7, metadata: {} },
    { timestamp: 1609459380, value: 25.2, metadata: {} },
    { timestamp: 1609459440, value: 18.9, metadata: {} }
  ]
  
  // Test aggregations
  let sum_result = aggregate_points(points, AggregationType::Sum)
  assert_eq(sum_result, 90.6)
  
  let avg_result = aggregate_points(points, AggregationType::Average)
  assert_eq(avg_result, 18.12)
  
  let min_result = aggregate_points(points, AggregationType::Min)
  assert_eq(min_result, 10.5)
  
  let max_result = aggregate_points(points, AggregationType::Max)
  assert_eq(max_result, 25.2)
  
  let count_result = aggregate_points(points, AggregationType::Count)
  assert_eq(count_result, 5.0)
  
  // Test empty points
  let empty_points = []
  let empty_sum = aggregate_points(empty_points, AggregationType::Sum)
  assert_eq(empty_sum, 0.0)
}

// Test 3: Time Window Operations
test "time window operations" {
  // Define time window
  type TimeWindow = {
    start_time : Int
    end_time : Int
  }
  
  // Filter points by time window
  let filter_by_time_window = fn(points : [TimeSeriesPoint], window : TimeWindow) {
    points.filter(fn(point) {
      point.timestamp >= window.start_time && point.timestamp <= window.end_time
    })
  }
  
  // Create time window
  let create_time_window = fn(start_time : Int, duration_seconds : Int) {
    {
      start_time: start_time,
      end_time: start_time + duration_seconds
    }
  }
  
  // Create test points
  let points = [
    { timestamp: 1609459200, value: 10.5, metadata: {} },  // 2021-01-01 00:00:00
    { timestamp: 1609459260, value: 20.3, metadata: {} },  // 2021-01-01 00:01:00
    { timestamp: 1609459320, value: 15.7, metadata: {} },  // 2021-01-01 00:02:00
    { timestamp: 1609459380, value: 25.2, metadata: {} },  // 2021-01-01 00:03:00
    { timestamp: 1609459440, value: 18.9, metadata: {} },  // 2021-01-01 00:04:00
    { timestamp: 1609459500, value: 22.1, metadata: {} }   // 2021-01-01 00:05:00
  ]
  
  // Test time window filtering
  let window1 = create_time_window(1609459260, 180)  // 3 minutes starting from 00:01:00
  let filtered_points1 = filter_by_time_window(points, window1)
  
  assert_eq(filtered_points1.length(), 3)
  assert_eq(filtered_points1[0].timestamp, 1609459260)
  assert_eq(filtered_points1[2].timestamp, 1609459380)
  
  // Test time window at the beginning
  let window2 = create_time_window(1609459200, 60)  // 1 minute starting from 00:00:00
  let filtered_points2 = filter_by_time_window(points, window2)
  
  assert_eq(filtered_points2.length(), 1)
  assert_eq(filtered_points2[0].timestamp, 1609459200)
  
  // Test time window at the end
  let window3 = create_time_window(1609459440, 120)  // 2 minutes starting from 00:04:00
  let filtered_points3 = filter_by_time_window(points, window3)
  
  assert_eq(filtered_points3.length(), 2)
  assert_eq(filtered_points3[0].timestamp, 1609459440)
  assert_eq(filtered_points3[1].timestamp, 1609459500)
  
  // Test time window with no matches
  let window4 = create_time_window(1609459000, 60)  // Before any points
  let filtered_points4 = filter_by_time_window(points, window4)
  
  assert_eq(filtered_points4.length(), 0)
}

// Test 4: Time Series Resampling
test "time series resampling" {
  // Define resampling methods
  enum ResamplingMethod {
    LinearInterpolation
    PreviousValue
    NextValue
    Average
  }
  
  // Resample time series to different interval
  let resample_time_series = fn(points : [TimeSeriesPoint], target_interval_seconds : Int, method : ResamplingMethod) {
    if points.length() < 2 {
      return points
    }
    
    let start_time = points[0].timestamp
    let end_time = points[points.length() - 1].timestamp
    
    let mut resampled_points = []
    let mut current_time = start_time
    
    while current_time <= end_time {
      let resampled_value = match method {
        ResamplingMethod::LinearInterpolation => {
          // Find surrounding points for linear interpolation
          let mut lower_point = None
          let mut upper_point = None
          
          for point in points {
            if point.timestamp <= current_time {
              lower_point = Some(point)
            } else if point.timestamp > current_time && upper_point == None {
              upper_point = Some(point)
            }
          }
          
          match (lower_point, upper_point) {
            (Some(lower), Some(upper)) => {
              let ratio = (current_time - lower.timestamp).to_float() / (upper.timestamp - lower.timestamp).to_float()
              lower.value + (upper.value - lower.value) * ratio
            }
            (Some(point), None) => point.value,
            (None, Some(point)) => point.value,
            (None, None) => 0.0
          }
        }
        ResamplingMethod::PreviousValue => {
          let mut previous_value = 0.0
          for point in points {
            if point.timestamp <= current_time {
              previous_value = point.value
            } else {
              break
            }
          }
          previous_value
        }
        ResamplingMethod::NextValue => {
          for point in points {
            if point.timestamp >= current_time {
              return point.value
            }
          }
          points[points.length() - 1].value
        }
        ResamplingMethod::Average => {
          let window_start = current_time - target_interval_seconds / 2
          let window_end = current_time + target_interval_seconds / 2
          let window_points = points.filter(fn(point) {
            point.timestamp >= window_start && point.timestamp <= window_end
          })
          
          if window_points.length() > 0 {
            window_points.reduce(fn(acc, point) { acc + point.value }, 0.0) / window_points.length().to_float()
          } else {
            0.0
          }
        }
      }
      
      resampled_points = resampled_points.push({
        timestamp: current_time,
        value: resampled_value,
        metadata: {}
      })
      
      current_time = current_time + target_interval_seconds
    }
    
    resampled_points
  }
  
  // Create test points (irregular intervals)
  let points = [
    { timestamp: 1609459200, value: 10.0, metadata: {} },  // 00:00:00
    { timestamp: 1609459230, value: 15.0, metadata: {} },  // 00:00:30
    { timestamp: 1609459285, value: 12.0, metadata: {} },  // 00:01:25
    { timestamp: 1609459340, value: 20.0, metadata: {} },  // 00:02:20
    { timestamp: 1609459390, value: 18.0, metadata: {} }   // 00:03:10
  ]
  
  // Test resampling with linear interpolation
  let resampled_linear = resample_time_series(points, 60, ResamplingMethod::LinearInterpolation)
  
  assert_eq(resampled_linear.length(), 4)  // 00:00, 00:01, 00:02, 00:03
  assert_eq(resampled_linear[0].timestamp, 1609459200)
  assert_eq(resampled_linear[0].value, 10.0)
  assert_eq(resampled_linear[1].timestamp, 1609459260)  // 00:01:00
  assert_eq(resampled_linear[2].timestamp, 1609459320)  // 00:02:00
  assert_eq(resampled_linear[3].timestamp, 1609459380)  // 00:03:00
  
  // Test resampling with previous value
  let resampled_previous = resample_time_series(points, 60, ResamplingMethod::PreviousValue)
  
  assert_eq(resampled_previous.length(), 4)
  assert_eq(resampled_previous[0].value, 10.0)
  assert_eq(resampled_previous[1].value, 15.0)  // Last value before or at 00:01:00
  assert_eq(resampled_previous[2].value, 12.0)  // Last value before or at 00:02:00
  assert_eq(resampled_previous[3].value, 18.0)  // Last value before or at 00:03:00
}

// Test 5: Time Series Trend Analysis
test "time series trend analysis" {
  // Define trend types
  enum TrendType {
    Increasing
    Decreasing
    Stable
    Volatile
  }
  
  // Calculate trend
  let calculate_trend = fn(points : [TimeSeriesPoint], threshold : Float) {
    if points.length() < 2 {
      return TrendType::Stable
    }
    
    // Calculate linear regression slope
    let n = points.length().to_float()
    let sum_x = points.reduce(fn(acc, point) { acc + point.timestamp.to_float() }, 0.0)
    let sum_y = points.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let sum_xy = points.reduce(fn(acc, point) { acc + point.timestamp.to_float() * point.value }, 0.0)
    let sum_x2 = points.reduce(fn(acc, point) { acc + point.timestamp.to_float() * point.timestamp.to_float() }, 0.0)
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    
    // Calculate volatility (standard deviation)
    let mean = sum_y / n
    let variance = points.reduce(fn(acc, point) { acc + (point.value - mean) * (point.value - mean) }, 0.0) / n
    let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
    
    // Determine trend based on slope and volatility
    if std_dev > threshold {
      TrendType::Volatile
    } else if slope > threshold {
      TrendType::Increasing
    } else if slope < -threshold {
      TrendType::Decreasing
    } else {
      TrendType::Stable
    }
  }
  
  // Test increasing trend
  let increasing_points = [
    { timestamp: 1609459200, value: 10.0, metadata: {} },
    { timestamp: 1609459260, value: 12.0, metadata: {} },
    { timestamp: 1609459320, value: 14.0, metadata: {} },
    { timestamp: 1609459380, value: 16.0, metadata: {} },
    { timestamp: 1609459440, value: 18.0, metadata: {} }
  ]
  
  let increasing_trend = calculate_trend(increasing_points, 0.1)
  assert_eq(increasing_trend, TrendType::Increasing)
  
  // Test decreasing trend
  let decreasing_points = [
    { timestamp: 1609459200, value: 18.0, metadata: {} },
    { timestamp: 1609459260, value: 16.0, metadata: {} },
    { timestamp: 1609459320, value: 14.0, metadata: {} },
    { timestamp: 1609459380, value: 12.0, metadata: {} },
    { timestamp: 1609459440, value: 10.0, metadata: {} }
  ]
  
  let decreasing_trend = calculate_trend(decreasing_points, 0.1)
  assert_eq(decreasing_trend, TrendType::Decreasing)
  
  // Test stable trend
  let stable_points = [
    { timestamp: 1609459200, value: 15.0, metadata: {} },
    { timestamp: 1609459260, value: 15.1, metadata: {} },
    { timestamp: 1609459320, value: 14.9, metadata: {} },
    { timestamp: 1609459380, value: 15.0, metadata: {} },
    { timestamp: 1609459440, value: 15.1, metadata: {} }
  ]
  
  let stable_trend = calculate_trend(stable_points, 0.1)
  assert_eq(stable_trend, TrendType::Stable)
  
  // Test volatile trend
  let volatile_points = [
    { timestamp: 1609459200, value: 10.0, metadata: {} },
    { timestamp: 1609459260, value: 25.0, metadata: {} },
    { timestamp: 1609459320, value: 5.0, metadata: {} },
    { timestamp: 1609459380, value: 30.0, metadata: {} },
    { timestamp: 1609459440, value: 8.0, metadata: {} }
  ]
  
  let volatile_trend = calculate_trend(volatile_points, 5.0)
  assert_eq(volatile_trend, TrendType::Volatile)
}

// Test 6: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Define anomaly types
  enum AnomalyType {
    Spike
    Drop
    PatternChange
    Outlier
  }
  
  // Define anomaly detection result
  type AnomalyDetectionResult = {
    is_anomaly : Bool
    anomaly_type : Option[AnomalyType]
    confidence : Float
    description : String
  }
  
  // Detect anomalies using statistical methods
  let detect_anomalies = fn(points : [TimeSeriesPoint], z_threshold : Float) {
    if points.length() < 3 {
      return AnomalyDetectionResult {
        is_anomaly: false,
        anomaly_type: None,
        confidence: 0.0,
        description: "Insufficient data for anomaly detection"
      }
    }
    
    let latest_point = points[points.length() - 1]
    let historical_points = points.slice(0, points.length() - 1)
    
    // Calculate mean and standard deviation of historical points
    let n = historical_points.length().to_float()
    let sum = historical_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let mean = sum / n
    
    let variance = historical_points.reduce(fn(acc, point) {
      acc + (point.value - mean) * (point.value - mean)
    }, 0.0) / n
    
    let std_dev = if variance > 0.0 { variance.sqrt() } else { 1.0 }
    
    // Calculate Z-score for the latest point
    let z_score = if std_dev > 0.0 { (latest_point.value - mean) / std_dev } else { 0.0 }
    
    // Determine if it's an anomaly
    if z_score.abs() > z_threshold {
      let anomaly_type = if z_score > 0.0 {
        if z_score > z_threshold * 2 {
          AnomalyType::Spike
        } else {
          AnomalyType::Outlier
        }
      } else {
        if z_score < -z_threshold * 2 {
          AnomalyType::Drop
        } else {
          AnomalyType::Outlier
        }
      }
      
      let confidence = if z_score.abs() > z_threshold * 3 {
        0.9
      } else if z_score.abs() > z_threshold * 2 {
        0.7
      } else {
        0.5
      }
      
      AnomalyDetectionResult {
        is_anomaly: true,
        anomaly_type: Some(anomaly_type),
        confidence: confidence,
        description: "Anomaly detected with Z-score: " + z_score.to_string()
      }
    } else {
      AnomalyDetectionResult {
        is_anomaly: false,
        anomaly_type: None,
        confidence: 0.0,
        description: "No anomaly detected"
      }
    }
  }
  
  // Test normal data
  let normal_points = [
    { timestamp: 1609459200, value: 10.0, metadata: {} },
    { timestamp: 1609459260, value: 10.5, metadata: {} },
    { timestamp: 1609459320, value: 9.8, metadata: {} },
    { timestamp: 1609459380, value: 10.2, metadata: {} },
    { timestamp: 1609459440, value: 10.1, metadata: {} }
  ]
  
  let normal_result = detect_anomalies(normal_points, 2.0)
  assert_false(normal_result.is_anomaly)
  assert_eq(normal_result.anomaly_type, None)
  
  // Test spike anomaly
  let spike_points = [
    { timestamp: 1609459200, value: 10.0, metadata: {} },
    { timestamp: 1609459260, value: 10.5, metadata: {} },
    { timestamp: 1609459320, value: 9.8, metadata: {} },
    { timestamp: 1609459380, value: 10.2, metadata: {} },
    { timestamp: 1609459440, value: 25.0, metadata: {} }  // Spike
  ]
  
  let spike_result = detect_anomalies(spike_points, 2.0)
  assert_true(spike_result.is_anomaly)
  match spike_result.anomaly_type {
    Some(AnomalyType::Spike) => assert_true(true),
    _ => assert_true(false)
  }
  assert_true(spike_result.confidence > 0.5)
  
  // Test drop anomaly
  let drop_points = [
    { timestamp: 1609459200, value: 20.0, metadata: {} },
    { timestamp: 1609459260, value: 19.5, metadata: {} },
    { timestamp: 1609459320, value: 20.2, metadata: {} },
    { timestamp: 1609459380, value: 19.8, metadata: {} },
    { timestamp: 1609459440, value: 5.0, metadata: {} }   // Drop
  ]
  
  let drop_result = detect_anomalies(drop_points, 2.0)
  assert_true(drop_result.is_anomaly)
  match drop_result.anomaly_type {
    Some(AnomalyType::Drop) => assert_true(true),
    _ => assert_true(false)
  }
  assert_true(drop_result.confidence > 0.5)
}

// Test 7: Time Series Forecasting
test "time series forecasting" {
  // Define forecasting methods
  enum ForecastingMethod {
    SimpleAverage
    LinearRegression
    ExponentialSmoothing
    MovingAverage
  }
  
  // Forecast future values
  let forecast_values = fn(points : [TimeSeriesPoint], forecast_count : Int, method : ForecastingMethod) {
    if points.length() < 2 {
      return []
    }
    
    let last_timestamp = points[points.length() - 1].timestamp
    let interval = if points.length() > 1 {
      points[points.length() - 1].timestamp - points[points.length() - 2].timestamp
    } else {
      60  // Default 1 minute interval
    }
    
    match method {
      ForecastingMethod::SimpleAverage => {
        let avg = points.reduce(fn(acc, point) { acc + point.value }, 0.0) / points.length().to_float()
        
        let forecast_values = []
        for i in 0..<forecast_count {
          forecast_values = forecast_values.push({
            timestamp: last_timestamp + (i + 1) * interval,
            value: avg,
            metadata: {"forecast_method": "simple_average"}
          })
        }
        forecast_values
      }
      ForecastingMethod::LinearRegression => {
        // Calculate linear regression
        let n = points.length().to_float()
        let sum_x = points.reduce(fn(acc, point) { acc + point.timestamp.to_float() }, 0.0)
        let sum_y = points.reduce(fn(acc, point) { acc + point.value }, 0.0)
        let sum_xy = points.reduce(fn(acc, point) { acc + point.timestamp.to_float() * point.value }, 0.0)
        let sum_x2 = points.reduce(fn(acc, point) { acc + point.timestamp.to_float() * point.timestamp.to_float() }, 0.0)
        
        let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        let intercept = (sum_y - slope * sum_x) / n
        
        let forecast_values = []
        for i in 0..<forecast_count {
          let future_timestamp = (last_timestamp + (i + 1) * interval).to_float()
          let forecast_value = slope * future_timestamp + intercept
          
          forecast_values = forecast_values.push({
            timestamp: last_timestamp + (i + 1) * interval,
            value: forecast_value,
            metadata: {"forecast_method": "linear_regression"}
          })
        }
        forecast_values
      }
      ForecastingMethod::ExponentialSmoothing => {
        let alpha = 0.3  // Smoothing factor
        
        let mut smoothed_values = []
        let mut smoothed = points[0].value
        smoothed_values = smoothed_values.push(smoothed)
        
        for i in 1..<points.length() {
          smoothed = alpha * points[i].value + (1.0 - alpha) * smoothed
          smoothed_values = smoothed_values.push(smoothed)
        }
        
        // Use the last smoothed value for all forecasts
        let last_smoothed = smoothed_values[smoothed_values.length() - 1]
        
        let forecast_values = []
        for i in 0..<forecast_count {
          forecast_values = forecast_values.push({
            timestamp: last_timestamp + (i + 1) * interval,
            value: last_smoothed,
            metadata: {"forecast_method": "exponential_smoothing"}
          })
        }
        forecast_values
      }
      ForecastingMethod::MovingAverage => {
        let window_size = 3
        let window_size_f = window_size.to_float()
        
        // Calculate moving average of the last window
        let recent_points = if points.length() >= window_size {
          points.slice(points.length() - window_size, points.length())
        } else {
          points
        }
        
        let moving_avg = recent_points.reduce(fn(acc, point) { acc + point.value }, 0.0) / recent_points.length().to_float()
        
        let forecast_values = []
        for i in 0..<forecast_count {
          forecast_values = forecast_values.push({
            timestamp: last_timestamp + (i + 1) * interval,
            value: moving_avg,
            metadata: {"forecast_method": "moving_average"}
          })
        }
        forecast_values
      }
    }
  }
  
  // Create test points
  let points = [
    { timestamp: 1609459200, value: 10.0, metadata: {} },
    { timestamp: 1609459260, value: 12.0, metadata: {} },
    { timestamp: 1609459320, value: 14.0, metadata: {} },
    { timestamp: 1609459380, value: 16.0, metadata: {} },
    { timestamp: 1609459440, value: 18.0, metadata: {} }
  ]
  
  // Test simple average forecasting
  let avg_forecast = forecast_values(points, 3, ForecastingMethod::SimpleAverage)
  assert_eq(avg_forecast.length(), 3)
  assert_eq(avg_forecast[0].value, 14.0)  // Average of 10, 12, 14, 16, 18
  assert_eq(avg_forecast[1].value, 14.0)
  assert_eq(avg_forecast[2].value, 14.0)
  
  // Test linear regression forecasting
  let regression_forecast = forecast_values(points, 3, ForecastingMethod::LinearRegression)
  assert_eq(regression_forecast.length(), 3)
  assert_true(regression_forecast[0].value > 18.0)  // Should continue increasing trend
  assert_true(regression_forecast[1].value > regression_forecast[0].value)
  assert_true(regression_forecast[2].value > regression_forecast[1].value)
  
  // Test exponential smoothing forecasting
  let smoothing_forecast = forecast_values(points, 3, ForecastingMethod::ExponentialSmoothing)
  assert_eq(smoothing_forecast.length(), 3)
  assert_eq(smoothing_forecast[0].metadata.get("forecast_method").or_else(""), "exponential_smoothing")
  
  // Test moving average forecasting
  let moving_avg_forecast = forecast_values(points, 3, ForecastingMethod::MovingAverage)
  assert_eq(moving_avg_forecast.length(), 3)
  assert_eq(moving_avg_forecast[0].value, 16.0)  // Average of last 3 values: 14, 16, 18
}

// Test 8: Time Series Seasonality Detection
test "time series seasonality detection" {
  // Define seasonality types
  enum SeasonalityType {
    Daily
    Weekly
    Monthly
    Yearly
    None
  }
  
  // Detect seasonality using autocorrelation
  let detect_seasonality = fn(points : [TimeSeriesPoint]) {
    if points.length() < 24 {  // Need at least 24 points for daily pattern detection
      return SeasonalityType::None
    }
    
    // Extract values
    let values = points.map(fn(point) { point.value })
    
    // Calculate autocorrelation for different lags
    let calculate_autocorrelation = fn(values : [Float], lag : Int) {
      if values.length() <= lag {
        return 0.0
      }
      
      let n = (values.length() - lag).to_float()
      let mean = values.reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_float()
      
      let mut numerator = 0.0
      let mut denominator = 0.0
      
      for i in 0..<values.length() - lag {
        let deviation1 = values[i] - mean
        let deviation2 = values[i + lag] - mean
        numerator = numerator + deviation1 * deviation2
      }
      
      for i in 0..<values.length() {
        let deviation = values[i] - mean
        denominator = denominator + deviation * deviation
      }
      
      if denominator > 0.0 {
        numerator / denominator
      } else {
        0.0
      }
    }
    
    // Check for different seasonal patterns
    // Daily pattern (24-hour cycle)
    let daily_autocorr = calculate_autocorrelation(values, 24)
    
    // Weekly pattern (7-day cycle, assuming hourly data)
    let weekly_autocorr = calculate_autocorrelation(values, 24 * 7)
    
    // Monthly pattern (30-day cycle, assuming hourly data)
    let monthly_autocorr = calculate_autocorrelation(values, 24 * 30)
    
    // Determine seasonality based on highest autocorrelation
    if monthly_autocorr > 0.7 {
      SeasonalityType::Monthly
    } else if weekly_autocorr > 0.7 {
      SeasonalityType::Weekly
    } else if daily_autocorr > 0.7 {
      SeasonalityType::Daily
    } else {
      SeasonalityType::None
    }
  }
  
  // Test daily seasonality
  let daily_points = []
  for day in 0..<3 {
    for hour in 0..<24 {
      let timestamp = 1609459200 + day * 24 * 3600 + hour * 3600
      let value = 10.0 + 5.0 * (hour.to_float() / 24.0 * 6.283185307).sin()  // Sine wave with daily period
      daily_points = daily_points.push({ timestamp: timestamp, value: value, metadata: {} })
    }
  }
  
  let daily_seasonality = detect_seasonality(daily_points)
  assert_eq(daily_seasonality, SeasonalityType::Daily)
  
  // Test weekly seasonality
  let weekly_points = []
  for week in 0..<3 {
    for day in 0..<7 {
      let timestamp = 1609459200 + week * 7 * 24 * 3600 + day * 24 * 3600
      let value = 10.0 + 5.0 * (day.to_float() / 7.0 * 6.283185307).sin()  // Sine wave with weekly period
      weekly_points = weekly_points.push({ timestamp: timestamp, value: value, metadata: {} })
    }
  }
  
  let weekly_seasonality = detect_seasonality(weekly_points)
  assert_eq(weekly_seasonality, SeasonalityType::Weekly)
  
  // Test no seasonality
  let no_seasonality_points = []
  for i in 0..<50 {
    let timestamp = 1609459200 + i * 3600
    let value = 10.0 + i.to_float() * 0.1  // Linear trend with no seasonality
    no_seasonality_points = no_seasonality_points.push({ timestamp: timestamp, value: value, metadata: {} })
  }
  
  let no_seasonality = detect_seasonality(no_seasonality_points)
  assert_eq(no_seasonality, SeasonalityType::None)
}

// Test 9: Time Series Compression
test "time series compression" {
  // Define compression methods
  enum CompressionMethod {
    DeltaEncoding
    RunLengthEncoding
    StraightLineOptimization
  }
  
  // Compress time series data
  let compress_time_series = fn(points : [TimeSeriesPoint], method : CompressionMethod, tolerance : Float) {
    match method {
      CompressionMethod::DeltaEncoding => {
        // Store deltas between consecutive points
        if points.length() == 0 {
          return []
        }
        
        let mut compressed = []
        compressed = compressed.push(points[0])  // First point as reference
        
        for i in 1..<points.length() {
          let prev_point = points[i - 1]
          let current_point = points[i]
          
          let delta_point = {
            timestamp: current_point.timestamp - prev_point.timestamp,
            value: current_point.value - prev_point.value,
            metadata: current_point.metadata
          }
          
          compressed = compressed.push(delta_point)
        }
        
        compressed
      }
      CompressionMethod::RunLengthEncoding => {
        // Run-length encoding for repeated values
        if points.length() == 0 {
          return []
        }
        
        let mut compressed = []
        let mut current_run = {
          value: points[0].value,
          start_timestamp: points[0].timestamp,
          end_timestamp: points[0].timestamp,
          count: 1
        }
        
        for i in 1..<points.length() {
          let point = points[i]
          
          if (point.value - current_run.value).abs() < tolerance {
            // Continue current run
            current_run = {
              value: current_run.value,
              start_timestamp: current_run.start_timestamp,
              end_timestamp: point.timestamp,
              count: current_run.count + 1
            }
          } else {
            // End current run and start new one
            compressed = compressed.push(current_run)
            current_run = {
              value: point.value,
              start_timestamp: point.timestamp,
              end_timestamp: point.timestamp,
              count: 1
            }
          }
        }
        
        // Add the last run
        compressed = compressed.push(current_run)
        compressed
      }
      CompressionMethod::StraightLineOptimization => {
        // Keep only points that deviate significantly from straight line
        if points.length() < 3 {
          return points
        }
        
        let mut compressed = []
        compressed = compressed.push(points[0])
        
        let mut start_index = 0
        
        for i in 2..<points.length() {
          let start_point = points[start_index]
          let end_point = points[i]
          
          // Check if all points between start and end are within tolerance of the straight line
          let mut all_within_tolerance = true
          
          for j in (start_index + 1)..<i {
            let middle_point = points[j]
            
            // Calculate expected value at middle point based on straight line
            let ratio = (middle_point.timestamp - start_point.timestamp).to_float() / (end_point.timestamp - start_point.timestamp).to_float()
            let expected_value = start_point.value + ratio * (end_point.value - start_point.value)
            
            if (middle_point.value - expected_value).abs() > tolerance {
              all_within_tolerance = false
              break
            }
          }
          
          if not all_within_tolerance {
            // Add the previous point as a key point
            compressed = compressed.push(points[i - 1])
            start_index = i - 1
          }
        }
        
        // Always add the last point
        compressed = compressed.push(points[points.length() - 1])
        compressed
      }
    }
  }
  
  // Create test points
  let points = [
    { timestamp: 1609459200, value: 10.0, metadata: {} },
    { timestamp: 1609459260, value: 10.5, metadata: {} },
    { timestamp: 1609459320, value: 11.0, metadata: {} },
    { timestamp: 1609459380, value: 11.5, metadata: {} },
    { timestamp: 1609459440, value: 12.0, metadata: {} }
  ]
  
  // Test delta encoding
  let delta_compressed = compress_time_series(points, CompressionMethod::DeltaEncoding, 0.0)
  assert_eq(delta_compressed.length(), 5)
  assert_eq(delta_compressed[0].timestamp, 1609459200)  // First point unchanged
  assert_eq(delta_compressed[1].timestamp, 60)         // Delta timestamp
  assert_eq(delta_compressed[1].value, 0.5)            // Delta value
  
  // Test run-length encoding
  let rle_points = [
    { timestamp: 1609459200, value: 10.0, metadata: {} },
    { timestamp: 1609459260, value: 10.1, metadata: {} },
    { timestamp: 1609459320, value: 10.05, metadata: {} },
    { timestamp: 1609459380, value: 15.0, metadata: {} },
    { timestamp: 1609459440, value: 15.1, metadata: {} }
  ]
  
  let rle_compressed = compress_time_series(rle_points, CompressionMethod::RunLengthEncoding, 0.2)
  assert_eq(rle_compressed.length(), 2)  // Two runs: one around 10.0, one around 15.0
  
  // Test straight line optimization
  let linear_points = [
    { timestamp: 1609459200, value: 10.0, metadata: {} },
    { timestamp: 1609459260, value: 10.5, metadata: {} },
    { timestamp: 1609459320, value: 11.0, metadata: {} },
    { timestamp: 1609459380, value: 11.5, metadata: {} },
    { timestamp: 1609459440, value: 12.0, metadata: {} }
  ]
  
  let slo_compressed = compress_time_series(linear_points, CompressionMethod::StraightLineOptimization, 0.1)
  assert_eq(slo_compressed.length(), 2)  // Only first and last points needed
}

// Test 10: Time Series Performance Operations
test "time series performance operations" {
  // Create large time series for performance testing
  let create_large_time_series = fn(point_count : Int, start_timestamp : Int, interval_seconds : Int) {
    let mut points = []
    
    for i in 0..<point_count {
      let timestamp = start_timestamp + i * interval_seconds
      let value = 10.0 + 5.0 * (i.to_float() / 100.0 * 6.283185307).sin() + i.to_float() * 0.01
      points = points.push({ timestamp: timestamp, value: value, metadata: {} })
    }
    
    points
  }
  
  // Measure performance of time window filtering
  let measure_time_window_performance = fn(points : [TimeSeriesPoint], window_size_minutes : Int) {
    let start_time = get_current_time_millis()
    
    let window_duration = window_size_minutes * 60
    let windows_processed = 0
    
    for i in 0..<points.length() - 100 {
      let window_start = points[i].timestamp
      let window_end = window_start + window_duration
      
      let window_points = points.filter(fn(point) {
        point.timestamp >= window_start && point.timestamp <= window_end
      })
      
      if window_points.length() > 0 {
        windows_processed = windows_processed + 1
      }
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "duration_ms": duration,
      "windows_processed": windows_processed,
      "points_per_second": if duration > 0 { (points.length().to_float() / duration.to_float()) * 1000.0 } else { 0.0 }
    }
  }
  
  // Measure performance of aggregation
  let measure_aggregation_performance = fn(points : [TimeSeriesPoint], aggregation_window_size : Int) {
    let start_time = get_current_time_millis()
    
    let aggregations_performed = 0
    
    for i in 0..<points.length() step aggregation_window_size {
      let end_index = if i + aggregation_window_size < points.length() {
        i + aggregation_window_size
      } else {
        points.length()
      }
      
      let window_points = points.slice(i, end_index)
      let sum = window_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
      let avg = sum / window_points.length().to_float()
      
      // Simulate using the result
      let _ = avg
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "duration_ms": duration,
      "aggregations_performed": points.length() / aggregation_window_size,
      "points_per_second": if duration > 0 { (points.length().to_float() / duration.to_float()) * 1000.0 } else { 0.0 }
    }
  }
  
  // Create large time series
  let large_series = create_large_time_series(10000, 1609459200, 60)  // 10000 points, 1 minute interval
  
  // Test time window performance
  let window_performance = measure_time_window_performance(large_series, 60)  // 60-minute windows
  
  assert_true(window_performance["duration_ms"] < 5000)  // Should complete in less than 5 seconds
  assert_true(window_performance["windows_processed"] > 0)
  assert_true(window_performance["points_per_second"] > 1000)  // Should process at least 1000 points/second
  
  // Test aggregation performance
  let aggregation_performance = measure_aggregation_performance(large_series, 100)  // 100-point windows
  
  assert_true(aggregation_performance["duration_ms"] < 2000)  // Should complete in less than 2 seconds
  assert_true(aggregation_performance["aggregations_performed"] > 0)
  assert_true(aggregation_performance["points_per_second"] > 5000)  // Should process at least 5000 points/second
}

// Helper function to get current time in milliseconds
fn get_current_time_millis() -> Int {
  // Implementation would depend on the available time functions in MoonBit
  // This is a placeholder for the actual implementation
  0
}