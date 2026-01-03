// Azimuth Comprehensive Time Series Analysis Tests
// This file contains comprehensive test cases for time series data analysis

// Test 1: Time Series Data Creation and Validation
test "time series data creation and validation" {
  // Create time series data points
  let data_points = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 10.5 }),  // 2022-01-01 00:00:00
    TimeSeriesPoint({ timestamp: 1641081600, value: 12.3 }),  // 2022-01-02 00:00:00
    TimeSeriesPoint({ timestamp: 1641168000, value: 11.8 }),  // 2022-01-03 00:00:00
    TimeSeriesPoint({ timestamp: 1641254400, value: 14.2 }),  // 2022-01-04 00:00:00
    TimeSeriesPoint({ timestamp: 1641340800, value: 13.7 })   // 2022-01-05 00:00:00
  ]
  
  // Create time series
  let time_series = TimeSeries({
    name: "temperature",
    unit: "celsius",
    data_points: data_points
  })
  
  // Validate time series
  assert_eq(time_series.name, "temperature")
  assert_eq(time_series.unit, "celsius")
  assert_eq(time_series.data_points.length(), 5)
  
  // Validate data points are sorted by timestamp
  for i = 1; i < time_series.data_points.length(); i = i + 1 {
    assert_true(time_series.data_points[i].timestamp >= time_series.data_points[i-1].timestamp)
  }
  
  // Validate first and last data points
  assert_eq(time_series.data_points[0].timestamp, 1640995200)
  assert_eq(time_series.data_points[0].value, 10.5)
  assert_eq(time_series.data_points[4].timestamp, 1641340800)
  assert_eq(time_series.data_points[4].value, 13.7)
}

// Test 2: Time Series Statistical Analysis
test "time series statistical analysis" {
  // Create time series with known values for statistical testing
  let test_data = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 10.0 }),
    TimeSeriesPoint({ timestamp: 1641081600, value: 20.0 }),
    TimeSeriesPoint({ timestamp: 1641168000, value: 30.0 }),
    TimeSeriesPoint({ timestamp: 1641254400, value: 40.0 }),
    TimeSeriesPoint({ timestamp: 1641340800, value: 50.0 })
  ]
  
  let time_series = TimeSeries({
    name: "test_series",
    unit: "units",
    data_points: test_data
  })
  
  // Calculate basic statistics
  let stats = time_series.calculate_statistics()
  
  // Validate statistics
  assert_eq(stats.count, 5)
  assert_eq(stats.min, 10.0)
  assert_eq(stats.max, 50.0)
  assert_eq(stats.mean, 30.0)
  assert_eq(stats.sum, 150.0)
  
  // Calculate variance and standard deviation
  let expected_variance = 200.0  // ((20^2 + 10^2 + 0^2 + 10^2 + 20^2) / 5)
  assert_eq(stats.variance, expected_variance)
  assert_eq(stats.std_dev, expected_variance.sqrt())
  
  // Calculate median
  assert_eq(stats.median, 30.0)
  
  // Calculate percentiles
  assert_eq(stats.percentile_25, 20.0)
  assert_eq(stats.percentile_75, 40.0)
}

// Test 3: Time Series Resampling
test "time series resampling" {
  // Create hourly data
  let hourly_data = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 10.0 }),  // 2022-01-01 00:00:00
    TimeSeriesPoint({ timestamp: 1640998800, value: 12.0 }),  // 2022-01-01 01:00:00
    TimeSeriesPoint({ timestamp: 1641002400, value: 14.0 }),  // 2022-01-01 02:00:00
    TimeSeriesPoint({ timestamp: 1641006000, value: 16.0 }),  // 2022-01-01 03:00:00
    TimeSeriesPoint({ timestamp: 1641009600, value: 18.0 }),  // 2022-01-01 04:00:00
    TimeSeriesPoint({ timestamp: 1641013200, value: 20.0 }),  // 2022-01-01 05:00:00
    TimeSeriesPoint({ timestamp: 1641016800, value: 22.0 }),  // 2022-01-01 06:00:00
    TimeSeriesPoint({ timestamp: 1641020400, value: 24.0 })   // 2022-01-01 07:00:00
  ]
  
  let hourly_series = TimeSeries({
    name: "hourly_data",
    unit: "units",
    data_points: hourly_data
  })
  
  // Resample to 2-hour intervals using mean
  let resampled_mean = hourly_series.resample(7200, "mean")  // 2 hours = 7200 seconds
  assert_eq(resampled_mean.data_points.length(), 4)
  assert_eq(resampled_mean.data_points[0].value, 11.0)  // (10 + 12) / 2
  assert_eq(resampled_mean.data_points[1].value, 15.0)  // (14 + 16) / 2
  assert_eq(resampled_mean.data_points[2].value, 19.0)  // (18 + 20) / 2
  assert_eq(resampled_mean.data_points[3].value, 23.0)  // (22 + 24) / 2
  
  // Resample to 4-hour intervals using max
  let resampled_max = hourly_series.resample(14400, "max")  // 4 hours = 14400 seconds
  assert_eq(resampled_max.data_points.length(), 2)
  assert_eq(resampled_max.data_points[0].value, 16.0)  // max(10, 12, 14, 16)
  assert_eq(resampled_max.data_points[1].value, 24.0)  // max(18, 20, 22, 24)
  
  // Resample to 8-hour intervals using min
  let resampled_min = hourly_series.resample(28800, "min")  // 8 hours = 28800 seconds
  assert_eq(resampled_min.data_points.length(), 1)
  assert_eq(resampled_min.data_points[0].value, 10.0)  // min(all values)
}

// Test 4: Time Series Moving Average
test "time series moving average" {
  // Create test data
  let test_data = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 10.0 }),
    TimeSeriesPoint({ timestamp: 1641081600, value: 20.0 }),
    TimeSeriesPoint({ timestamp: 1641168000, value: 30.0 }),
    TimeSeriesPoint({ timestamp: 1641254400, value: 40.0 }),
    TimeSeriesPoint({ timestamp: 1641340800, value: 50.0 })
  ]
  
  let time_series = TimeSeries({
    name: "test_series",
    unit: "units",
    data_points: test_data
  })
  
  // Calculate 3-period moving average
  let ma3 = time_series.moving_average(3)
  assert_eq(ma3.data_points.length(), 3)  // First 2 points don't have enough data
  
  assert_eq(ma3.data_points[0].value, 20.0)  // (10 + 20 + 30) / 3
  assert_eq(ma3.data_points[1].value, 30.0)  // (20 + 30 + 40) / 3
  assert_eq(ma3.data_points[2].value, 40.0)  // (30 + 40 + 50) / 3
  
  // Calculate 5-period moving average
  let ma5 = time_series.moving_average(5)
  assert_eq(ma5.data_points.length(), 1)  // Only last point has enough data
  assert_eq(ma5.data_points[0].value, 30.0)  // (10 + 20 + 30 + 40 + 50) / 5
}

// Test 5: Time Series Trend Analysis
test "time series trend analysis" {
  // Create increasing trend data
  let increasing_data = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 10.0 }),
    TimeSeriesPoint({ timestamp: 1641081600, value: 15.0 }),
    TimeSeriesPoint({ timestamp: 1641168000, value: 20.0 }),
    TimeSeriesPoint({ timestamp: 1641254400, value: 25.0 }),
    TimeSeriesPoint({ timestamp: 1641340800, value: 30.0 })
  ]
  
  let increasing_series = TimeSeries({
    name: "increasing_series",
    unit: "units",
    data_points: increasing_data
  })
  
  // Calculate trend
  let increasing_trend = increasing_series.calculate_trend()
  assert_true(increasing_trend.slope > 0.0)
  assert_eq(increasing_trend.direction, "increasing")
  
  // Create decreasing trend data
  let decreasing_data = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 50.0 }),
    TimeSeriesPoint({ timestamp: 1641081600, value: 40.0 }),
    TimeSeriesPoint({ timestamp: 1641168000, value: 30.0 }),
    TimeSeriesPoint({ timestamp: 1641254400, value: 20.0 }),
    TimeSeriesPoint({ timestamp: 1641340800, value: 10.0 })
  ]
  
  let decreasing_series = TimeSeries({
    name: "decreasing_series",
    unit: "units",
    data_points: decreasing_data
  })
  
  // Calculate trend
  let decreasing_trend = decreasing_series.calculate_trend()
  assert_true(decreasing_trend.slope < 0.0)
  assert_eq(decreasing_trend.direction, "decreasing")
  
  // Create flat trend data
  let flat_data = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 25.0 }),
    TimeSeriesPoint({ timestamp: 1641081600, value: 25.0 }),
    TimeSeriesPoint({ timestamp: 1641168000, value: 25.0 }),
    TimeSeriesPoint({ timestamp: 1641254400, value: 25.0 }),
    TimeSeriesPoint({ timestamp: 1641340800, value: 25.0 })
  ]
  
  let flat_series = TimeSeries({
    name: "flat_series",
    unit: "units",
    data_points: flat_data
  })
  
  // Calculate trend
  let flat_trend = flat_series.calculate_trend()
  assert_eq(flat_trend.slope, 0.0)
  assert_eq(flat_trend.direction, "stable")
}

// Test 6: Time Series Seasonality Detection
test "time series seasonality detection" {
  // Create seasonal data (weekly pattern)
  let seasonal_data = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 10.0 }),  // Sunday
    TimeSeriesPoint({ timestamp: 1641081600, value: 20.0 }),  // Monday
    TimeSeriesPoint({ timestamp: 1641168000, value: 30.0 }),  // Tuesday
    TimeSeriesPoint({ timestamp: 1641254400, value: 40.0 }),  // Wednesday
    TimeSeriesPoint({ timestamp: 1641340800, value: 30.0 }),  // Thursday
    TimeSeriesPoint({ timestamp: 1641427200, value: 20.0 }),  // Friday
    TimeSeriesPoint({ timestamp: 1641513600, value: 10.0 }),  // Saturday
    TimeSeriesPoint({ timestamp: 1641600000, value: 10.0 }),  // Sunday (next week)
    TimeSeriesPoint({ timestamp: 1641686400, value: 20.0 }),  // Monday (next week)
    TimeSeriesPoint({ timestamp: 1641772800, value: 30.0 }),  // Tuesday (next week)
    TimeSeriesPoint({ timestamp: 1641859200, value: 40.0 }),  // Wednesday (next week)
    TimeSeriesPoint({ timestamp: 1641945600, value: 30.0 }),  // Thursday (next week)
    TimeSeriesPoint({ timestamp: 1642032000, value: 20.0 }),  // Friday (next week)
    TimeSeriesPoint({ timestamp: 1642118400, value: 10.0 })   // Saturday (next week)
  ]
  
  let seasonal_series = TimeSeries({
    name: "seasonal_series",
    unit: "units",
    data_points: seasonal_data
  })
  
  // Detect seasonality
  let seasonality = seasonal_series.detect_seasonality()
  assert_true(seasonality.is_seasonal)
  assert_eq(seasonality.period, 7)  // Weekly pattern
  
  // Validate seasonal pattern
  assert_eq(seasonality.pattern[0], 10.0)  // Sunday
  assert_eq(seasonality.pattern[1], 20.0)  // Monday
  assert_eq(seasonality.pattern[2], 30.0)  // Tuesday
  assert_eq(seasonality.pattern[3], 40.0)  // Wednesday
  assert_eq(seasonality.pattern[4], 30.0)  // Thursday
  assert_eq(seasonality.pattern[5], 20.0)  // Friday
  assert_eq(seasonality.pattern[6], 10.0)  // Saturday
}

// Test 7: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Create data with anomalies
  let data_with_anomalies = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 10.0 }),
    TimeSeriesPoint({ timestamp: 1641081600, value: 12.0 }),
    TimeSeriesPoint({ timestamp: 1641168000, value: 11.0 }),
    TimeSeriesPoint({ timestamp: 1641254400, value: 50.0 }),  // Anomaly
    TimeSeriesPoint({ timestamp: 1641340800, value: 13.0 }),
    TimeSeriesPoint({ timestamp: 1641427200, value: 14.0 }),
    TimeSeriesPoint({ timestamp: 1641513600, value: 12.0 }),
    TimeSeriesPoint({ timestamp: 1641600000, value: 5.0 }),   // Anomaly
    TimeSeriesPoint({ timestamp: 1641686400, value: 15.0 }),
    TimeSeriesPoint({ timestamp: 1641772800, value: 16.0 })
  ]
  
  let anomaly_series = TimeSeries({
    name: "anomaly_series",
    unit: "units",
    data_points: data_with_anomalies
  })
  
  // Detect anomalies using z-score method
  let anomalies = anomaly_series.detect_anomalies("zscore", 2.0)
  assert_eq(anomalies.length(), 2)
  
  // Validate anomaly positions
  assert_eq(anomalies[0].timestamp, 1641254400)
  assert_eq(anomalies[0].value, 50.0)
  assert_eq(anomalies[1].timestamp, 1641600000)
  assert_eq(anomalies[1].value, 5.0)
  
  // Detect anomalies using IQR method
  let anomalies_iqr = anomaly_series.detect_anomalies("iqr", 1.5)
  assert_eq(anomalies_iqr.length(), 2)
}

// Test 8: Time Series Forecasting
test "time series forecasting" {
  // Create historical data
  let historical_data = [
    TimeSeriesPoint({ timestamp: 1640995200, value: 10.0 }),
    TimeSeriesPoint({ timestamp: 1641081600, value: 12.0 }),
    TimeSeriesPoint({ timestamp: 1641168000, value: 14.0 }),
    TimeSeriesPoint({ timestamp: 1641254400, value: 16.0 }),
    TimeSeriesPoint({ timestamp: 1641340800, value: 18.0 })
  ]
  
  let historical_series = TimeSeries({
    name: "historical_series",
    unit: "units",
    data_points: historical_data
  })
  
  // Simple linear extrapolation forecast
  let forecast = historical_series.forecast("linear", 3)
  assert_eq(forecast.data_points.length(), 3)
  
  // Validate forecast values (continuing the trend)
  assert_eq(forecast.data_points[0].value, 20.0)  // Next point
  assert_eq(forecast.data_points[1].value, 22.0)  // Second next point
  assert_eq(forecast.data_points[2].value, 24.0)  // Third next point
  
  // Validate forecast timestamps
  assert_eq(forecast.data_points[0].timestamp, 1641427200)  // 2022-01-06 00:00:00
  assert_eq(forecast.data_points[1].timestamp, 1641513600)  // 2022-01-07 00:00:00
  assert_eq(forecast.data_points[2].timestamp, 1641600000)  // 2022-01-08 00:00:00
  
  // Moving average forecast
  let ma_forecast = historical_series.forecast("moving_average", 2)
  assert_eq(ma_forecast.data_points.length(), 2)
  
  // Validate moving average forecast
  assert_eq(ma_forecast.data_points[0].value, 17.0)  // Average of last 3 points: (14+16+18)/3
  assert_eq(ma_forecast.data_points[1].value, 17.0)  // Same for next point
}