// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Creation
test "time series data point creation" {
  // Create a basic time series data point
  let timestamp = 1234567890L
  let value = 42.5
  let data_point = TimeSeriesDataPoint::new(timestamp, value)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
  assert_eq(TimeSeriesDataPoint::attributes(data_point).length(), 0)
  
  // Create a data point with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "metric.name", StringValue("cpu_usage"))
  Attributes::set(attrs, "host.name", StringValue("server-01"))
  
  let data_point_with_attrs = TimeSeriesDataPoint::with_attributes(timestamp, value, attrs)
  assert_eq(TimeSeriesDataPoint::timestamp(data_point_with_attrs), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point_with_attrs), value)
  assert_eq(TimeSeriesDataPoint::attributes(data_point_with_attrs).length(), 2)
  
  // Verify attributes
  let metric_name = Attributes::get(TimeSeriesDataPoint::attributes(data_point_with_attrs), "metric.name")
  match metric_name {
    Some(StringValue(name)) => assert_eq(name, "cpu_usage")
    _ => assert_true(false)
  }
  
  let host_name = Attributes::get(TimeSeriesDataPoint::attributes(data_point_with_attrs), "host.name")
  match host_name {
    Some(StringValue(name)) => assert_eq(name, "server-01")
    _ => assert_true(false)
  }
}

// Test 2: Time Series Creation and Management
test "time series creation and management" {
  // Create a new time series
  let series_name = "cpu_usage_percentage"
  let time_series = TimeSeries::new(series_name)
  
  assert_eq(TimeSeries::name(time_series), series_name)
  assert_eq(TimeSeries::data_points(time_series).length(), 0)
  
  // Add data points to the time series
  let data_point1 = TimeSeriesDataPoint::new(1234567890L, 25.5)
  let data_point2 = TimeSeriesDataPoint::new(1234567891L, 30.2)
  let data_point3 = TimeSeriesDataPoint::new(1234567892L, 28.7)
  
  TimeSeries::add_data_point(time_series, data_point1)
  TimeSeries::add_data_point(time_series, data_point2)
  TimeSeries::add_data_point(time_series, data_point3)
  
  // Verify data points were added
  assert_eq(TimeSeries::data_points(time_series).length(), 3)
  
  // Verify data points are in chronological order
  let data_points = TimeSeries::data_points(time_series)
  assert_eq(TimeSeriesDataPoint::timestamp(data_points[0]), 1234567890L)
  assert_eq(TimeSeriesDataPoint::timestamp(data_points[1]), 1234567891L)
  assert_eq(TimeSeriesDataPoint::timestamp(data_points[2]), 1234567892L)
}

// Test 3: Time Series Aggregation
test "time series aggregation" {
  // Create a time series with multiple data points
  let time_series = TimeSeries::new("test_series")
  
  // Add data points with known values for aggregation testing
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(1000L, 10.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(2000L, 20.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(3000L, 30.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(4000L, 40.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(5000L, 50.0))
  
  // Test sum aggregation
  let sum_result = TimeSeries::aggregate(time_series, Sum)
  assert_eq(sum_result, 150.0)
  
  // Test average aggregation
  let avg_result = TimeSeries::aggregate(time_series, Average)
  assert_eq(avg_result, 30.0)
  
  // Test min aggregation
  let min_result = TimeSeries::aggregate(time_series, Min)
  assert_eq(min_result, 10.0)
  
  // Test max aggregation
  let max_result = TimeSeries::aggregate(time_series, Max)
  assert_eq(max_result, 50.0)
  
  // Test count aggregation
  let count_result = TimeSeries::aggregate(time_series, Count)
  assert_eq(count_result, 5.0)
}

// Test 4: Time Series Windowing
test "time series windowing" {
  // Create a time series with data points spanning multiple time periods
  let time_series = TimeSeries::new("window_test_series")
  
  // Add data points every second for 10 seconds
  for i in 0..=9 {
    let timestamp = 1000L + (i as Int64)
    let value = (i as Float) * 10.0
    TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Test time-based windowing (3-second windows)
  let windows = TimeSeries::window_by_time(time_series, 3000L)
  assert_eq(windows.length(), 4) // 10 seconds / 3-second windows = 3.33, rounded up to 4
  
  // Verify first window (seconds 0-2)
  let first_window = windows[0]
  assert_eq(TimeSeries::data_points(first_window).length(), 3)
  
  // Verify last window (seconds 9)
  let last_window = windows[3]
  assert_eq(TimeSeries::data_points(last_window).length(), 1)
  
  // Test count-based windowing (3 data points per window)
  let count_windows = TimeSeries::window_by_count(time_series, 3)
  assert_eq(count_windows.length(), 4) // 10 data points / 3 per window = 3.33, rounded up to 4
  
  // Verify first window has 3 data points
  assert_eq(TimeSeries::data_points(count_windows[0]).length(), 3)
  
  // Verify last window has 1 data point
  assert_eq(TimeSeries::data_points(count_windows[3]).length(), 1)
}

// Test 5: Time Series Resampling
test "time series resampling" {
  // Create a time series with irregular data points
  let time_series = TimeSeries::new("resample_test_series")
  
  // Add data points at irregular intervals
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(1000L, 10.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(1500L, 15.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(2200L, 22.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(2800L, 28.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(3500L, 35.0))
  
  // Resample to regular 1-second intervals using linear interpolation
  let resampled_series = TimeSeries::resample(time_series, 1000L, LinearInterpolation)
  
  // Verify resampled data points
  let resampled_points = TimeSeries::data_points(resampled_series)
  assert_true(resampled_points.length() > 0)
  
  // First point should be at 1000L with value 10.0
  assert_eq(TimeSeriesDataPoint::timestamp(resampled_points[0]), 1000L)
  assert_eq(TimeSeriesDataPoint::value(resampled_points[0]), 10.0)
  
  // Last point should be at 3000L with interpolated value
  let last_point = resampled_points[resampled_points.length() - 1]
  assert_eq(TimeSeriesDataPoint::timestamp(last_point), 3000L)
  assert_true(TimeSeriesDataPoint::value(last_point) > 28.0 && TimeSeriesDataPoint::value(last_point) < 35.0)
  
  // Test resampling with forward fill
  let forward_fill_series = TimeSeries::resample(time_series, 1000L, ForwardFill)
  let forward_fill_points = TimeSeries::data_points(forward_fill_series)
  
  // Values should be carried forward
  assert_eq(TimeSeriesDataPoint::value(forward_fill_points[1]), 15.0) // At 2000L
  assert_eq(TimeSeriesDataPoint::value(forward_fill_points[2]), 22.0) // At 3000L
}

// Test 6: Time Series Filtering
test "time series filtering" {
  // Create a time series with various data points
  let time_series = TimeSeries::new("filter_test_series")
  
  // Add data points with different values
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(1000L, 10.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(2000L, 25.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(3000L, 15.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(4000L, 30.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(5000L, 5.0))
  
  // Filter by value range (greater than 15)
  let filtered_by_value = TimeSeries::filter_by_value(time_series, fn(v) { v > 15.0 })
  let filtered_points = TimeSeries::data_points(filtered_by_value)
  assert_eq(filtered_points.length(), 2)
  assert_eq(TimeSeriesDataPoint::value(filtered_points[0]), 25.0)
  assert_eq(TimeSeriesDataPoint::value(filtered_points[1]), 30.0)
  
  // Filter by time range (between 2000L and 4000L)
  let filtered_by_time = TimeSeries::filter_by_time(time_series, 2000L, 4000L)
  let time_filtered_points = TimeSeries::data_points(filtered_by_time)
  assert_eq(time_filtered_points.length(), 3)
  assert_eq(TimeSeriesDataPoint::timestamp(time_filtered_points[0]), 2000L)
  assert_eq(TimeSeriesDataPoint::timestamp(time_filtered_points[2]), 4000L)
  
  // Filter by attributes
  let attrs_series = TimeSeries::new("attrs_test_series")
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "host", StringValue("server-01"))
  TimeSeries::add_data_point(attrs_series, TimeSeriesDataPoint::with_attributes(1000L, 10.0, attrs1))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "host", StringValue("server-02"))
  TimeSeries::add_data_point(attrs_series, TimeSeriesDataPoint::with_attributes(2000L, 20.0, attrs2))
  
  let attrs3 = Attributes::new()
  Attributes::set(attrs3, "host", StringValue("server-01"))
  TimeSeries::add_data_point(attrs_series, TimeSeriesDataPoint::with_attributes(3000L, 30.0, attrs3))
  
  let filtered_by_attrs = TimeSeries::filter_by_attributes(attrs_series, fn(attrs) {
    match Attributes::get(attrs, "host") {
      Some(StringValue(host)) => host == "server-01"
      _ => false
    }
  })
  let attrs_filtered_points = TimeSeries::data_points(filtered_by_attrs)
  assert_eq(attrs_filtered_points.length(), 2)
}

// Test 7: Time Series Statistics
test "time series statistics" {
  // Create a time series with known data points
  let time_series = TimeSeries::new("stats_test_series")
  
  // Add data points with known statistical properties
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(1000L, 10.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(2000L, 20.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(3000L, 30.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(4000L, 40.0))
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(5000L, 50.0))
  
  // Calculate basic statistics
  let stats = TimeSeries::calculate_statistics(time_series)
  
  assert_eq(TimeSeriesStats::count(stats), 5)
  assert_eq(TimeSeriesStats::min(stats), 10.0)
  assert_eq(TimeSeriesStats::max(stats), 50.0)
  assert_eq(TimeSeriesStats::sum(stats), 150.0)
  assert_eq(TimeSeriesStats::mean(stats), 30.0)
  
  // Calculate variance and standard deviation
  // For values [10, 20, 30, 40, 50], variance = 200, std_dev = sqrt(200) ≈ 14.142
  assert_eq(TimeSeriesStats::variance(stats), 200.0)
  assert_true(abs(TimeSeriesStats::std_dev(stats) - 14.142) < 0.01)
  
  // Calculate percentiles
  assert_eq(TimeSeriesStats::percentile(stats, 0.0), 10.0) // 0th percentile (min)
  assert_eq(TimeSeriesStats::percentile(stats, 1.0), 50.0) // 100th percentile (max)
  assert_eq(TimeSeriesStats::percentile(stats, 0.5), 30.0) // 50th percentile (median)
}

// Test 8: Time Series Trend Analysis
test "time series trend analysis" {
  // Create a time series with an increasing trend
  let increasing_series = TimeSeries::new("increasing_series")
  
  for i in 0..=9 {
    let timestamp = 1000L + (i as Int64)
    let value = 10.0 + (i as Float) * 2.0 // Linear increase
    TimeSeries::add_data_point(increasing_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Analyze trend
  let increasing_trend = TimeSeries::analyze_trend(increasing_series)
  assert_eq(TimeSeriesTrend::direction(increasing_trend), Increasing)
  assert_true(TimeSeriesTrend::slope(increasing_trend) > 0.0)
  
  // Create a time series with a decreasing trend
  let decreasing_series = TimeSeries::new("decreasing_series")
  
  for i in 0..=9 {
    let timestamp = 1000L + (i as Int64)
    let value = 50.0 - (i as Float) * 2.0 // Linear decrease
    TimeSeries::add_data_point(decreasing_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Analyze trend
  let decreasing_trend = TimeSeries::analyze_trend(decreasing_series)
  assert_eq(TimeSeriesTrend::direction(decreasing_trend), Decreasing)
  assert_true(TimeSeriesTrend::slope(decreasing_trend) < 0.0)
  
  // Create a time series with no clear trend
  let flat_series = TimeSeries::new("flat_series")
  
  for i in 0..=9 {
    let timestamp = 1000L + (i as Int64)
    let value = 25.0 + (Random::next() * 10.0 - 5.0) // Random variation around 25
    TimeSeries::add_data_point(flat_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Analyze trend
  let flat_trend = TimeSeries::analyze_trend(flat_series)
  assert_eq(TimeSeriesTrend::direction(flat_trend), Stationary)
  assert_true(abs(TimeSeriesTrend::slope(flat_trend)) < 1.0)
}

// Test 9: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Create a time series with normal data and one anomaly
  let time_series = TimeSeries::new("anomaly_test_series")
  
  // Add normal data points (values around 25)
  for i in 0..=9 {
    let timestamp = 1000L + (i as Int64)
    let value = 25.0 + (Random::next() * 4.0 - 2.0) // Random variation around 25
    TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Add an anomaly (value far from normal range)
  TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(11000L, 100.0))
  
  // Add more normal data points
  for i in 11..=19 {
    let timestamp = 1000L + (i as Int64)
    let value = 25.0 + (Random::next() * 4.0 - 2.0) // Random variation around 25
    TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Detect anomalies using statistical method (z-score)
  let anomalies = TimeSeries::detect_anomalies(time_series, ZScore, 3.0)
  assert_true(anomalies.length() >= 1)
  
  // Verify the anomaly was detected
  let mut found_anomaly = false
  for anomaly in anomalies {
    if TimeSeriesDataPoint::timestamp(anomaly) == 11000L {
      found_anomaly = true
      assert_eq(TimeSeriesDataPoint::value(anomaly), 100.0)
    }
  }
  assert_true(found_anomaly)
  
  // Test with different threshold
  let anomalies_strict = TimeSeries::detect_anomalies(time_series, ZScore, 2.0)
  assert_true(anomalies_strict.length() >= anomalies.length())
  
  // Test with different method (IQR)
  let anomalies_iqr = TimeSeries::detect_anomalies(time_series, IQR, 1.5)
  assert_true(anomalies_iqr.length() >= 1)
}

// Test 10: Time Series Forecasting
test "time series forecasting" {
  // Create a time series with a clear trend
  let time_series = TimeSeries::new("forecast_test_series")
  
  // Add data points with a linear trend
  for i in 0..=19 {
    let timestamp = 1000L + (i as Int64)
    let value = 10.0 + (i as Float) * 2.0 // Linear increase
    TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Forecast next 5 data points using linear regression
  let forecast = TimeSeries::forecast(time_series, LinearRegression, 5)
  assert_eq(forecast.length(), 5)
  
  // Verify forecast values continue the trend
  // Last actual value should be at timestamp 11000L with value 48.0
  // First forecast value should be at timestamp 12000L with value around 50.0
  assert_eq(TimeSeriesDataPoint::timestamp(forecast[0]), 12000L)
  assert_true(abs(TimeSeriesDataPoint::value(forecast[0]) - 50.0) < 1.0)
  
  // Last forecast value should be at timestamp 16000L with value around 60.0
  let last_forecast = forecast[4]
  assert_eq(TimeSeriesDataPoint::timestamp(last_forecast), 16000L)
  assert_true(abs(TimeSeriesDataPoint::value(last_forecast) - 60.0) < 1.0)
  
  // Test with different forecasting method (moving average)
  let ma_forecast = TimeSeries::forecast(time_series, MovingAverage(5), 3)
  assert_eq(ma_forecast.length(), 3)
  
  // Moving average forecast should be close to the last few values
  for forecast_point in ma_forecast {
    assert_true(TimeSeriesDataPoint::value(forecast_point) > 40.0 && TimeSeriesDataPoint::value(forecast_point) < 50.0)
  }
  
  // Test forecast accuracy with known data
  // Split data into training and testing sets
  let training_series = TimeSeries::new("training_series")
  let testing_series = TimeSeries::new("testing_series")
  
  // First 15 points for training
  for i in 0..=14 {
    let timestamp = 1000L + (i as Int64)
    let value = 10.0 + (i as Float) * 2.0
    TimeSeries::add_data_point(training_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Last 5 points for testing
  for i in 15..=19 {
    let timestamp = 1000L + (i as Int64)
    let value = 10.0 + (i as Float) * 2.0
    TimeSeries::add_data_point(testing_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  // Forecast 5 points ahead
  let accuracy_forecast = TimeSeries::forecast(training_series, LinearRegression, 5)
  
  // Calculate forecast accuracy (mean absolute percentage error)
  let mape = TimeSeries::calculate_forecast_accuracy(accuracy_forecast, testing_series)
  assert_true(mape < 10.0) // Should be less than 10% error for this simple linear trend
}