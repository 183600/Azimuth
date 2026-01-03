// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Creation
test "time series data point creation" {
  // Create a time series data point with timestamp and value
  let timestamp = 1640995200000L  // 2022-01-01 00:00:00 UTC
  let value = 42.5
  let data_point = TimeSeriesDataPoint::new(timestamp, value, Some("temperature"))
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
  match TimeSeriesDataPoint::metric_name(data_point) {
    Some(name) => assert_eq(name, "temperature")
    None => assert_true(false)
  }
  
  // Test data point with attributes
  let attributes = Attributes::new()
  Attributes::set(attributes, "unit", StringValue("celsius"))
  Attributes::set(attributes, "location", StringValue("server-room-1"))
  
  let data_point_with_attrs = TimeSeriesDataPoint::with_attributes(
    timestamp, 
    value, 
    Some("temperature"), 
    attributes
  )
  
  let retrieved_attrs = TimeSeriesDataPoint::attributes(data_point_with_attrs)
  let unit_attr = Attributes::get(retrieved_attrs, "unit")
  match unit_attr {
    Some(StringValue(unit)) => assert_eq(unit, "celsius")
    _ => assert_true(false)
  }
}

// Test 2: Time Series Collection Operations
test "time series collection operations" {
  let collection = TimeSeriesCollection::new("cpu_usage")
  
  // Add data points to collection
  let base_timestamp = 1640995200000L
  for i in 0..=9 {
    let timestamp = base_timestamp + (i * 60000L)  // 1 minute intervals
    let value = 50.0 + (i * 2.5).to_float()
    let data_point = TimeSeriesDataPoint::new(timestamp, value, None)
    TimeSeriesCollection::add_point(collection, data_point)
  }
  
  // Verify collection size
  assert_eq(TimeSeriesCollection::size(collection), 10)
  
  // Test getting data points within time range
  let start_time = base_timestamp + (3 * 60000L)
  let end_time = base_timestamp + (7 * 60000L)
  let range_points = TimeSeriesCollection::get_points_in_range(collection, start_time, end_time)
  
  assert_eq(range_points.length(), 5)  // Points at indices 3, 4, 5, 6, 7
  
  // Test aggregation - average
  let avg_value = TimeSeriesCollection::average(collection, None, None)
  match avg_value {
    Some(value) => {
      // Expected average: (50.0 + 52.5 + 55.0 + 57.5 + 60.0 + 62.5 + 65.0 + 67.5 + 70.0 + 72.5) / 10 = 61.25
      assert_eq(value, 61.25)
    }
    None => assert_true(false)
  }
  
  // Test aggregation - max
  let max_value = TimeSeriesCollection::max(collection, None, None)
  match max_value {
    Some(value) => assert_eq(value, 72.5)
    None => assert_true(false)
  }
  
  // Test aggregation - min
  let min_value = TimeSeriesCollection::min(collection, None, None)
  match min_value {
    Some(value) => assert_eq(value, 50.0)
    None => assert_true(false)
  }
}

// Test 3: Time Series Downsampling
test "time series downsampling" {
  let collection = TimeSeriesCollection::new("memory_usage")
  
  // Add data points with 10-second intervals
  let base_timestamp = 1640995200000L
  for i in 0..=59 {  // 60 points = 10 minutes of data
    let timestamp = base_timestamp + (i * 10000L)  // 10 second intervals
    let value = 1000.0 + (i * 10.0).to_float()
    let data_point = TimeSeriesDataPoint::new(timestamp, value, None)
    TimeSeriesCollection::add_point(collection, data_point)
  }
  
  // Downsample to 1-minute intervals (averaging)
  let downsampled = TimeSeriesCollection::downsample(
    collection, 
    60000L,  // 1 minute
    AggregationMethod::Average
  )
  
  // Should have 10 points after downsampling (60 points / 6 points per minute)
  assert_eq(TimeSeriesCollection::size(downsampled), 10)
  
  // Verify first downsampled point (average of first 6 points)
  let first_point = TimeSeriesCollection::get_point_at(downsampled, 0)
  assert_eq(TimeSeriesDataPoint::timestamp(first_point), base_timestamp + 30000L)  // Middle of first minute
  assert_eq(TimeSeriesDataPoint::value(first_point), 1025.0)  // Average of 1000, 1010, 1020, 1030, 1040, 1050
}

// Test 4: Time Series Anomaly Detection
test "time series anomaly detection" {
  let collection = TimeSeriesCollection::new("response_time")
  
  // Add normal data points
  let base_timestamp = 1640995200000L
  for i in 0..=19 {
    let timestamp = base_timestamp + (i * 60000L)
    let value = 100.0 + (i % 5).to_float() * 2.0  // Values between 100-108
    let data_point = TimeSeriesDataPoint::new(timestamp, value, None)
    TimeSeriesCollection::add_point(collection, data_point)
  }
  
  // Add an anomaly
  let anomaly_timestamp = base_timestamp + (20 * 60000L)
  let anomaly_point = TimeSeriesDataPoint::new(anomaly_timestamp, 500.0, None)
  TimeSeriesCollection::add_point(collection, anomaly_point)
  
  // Add more normal points
  for i in 21..=29 {
    let timestamp = base_timestamp + (i * 60000L)
    let value = 100.0 + (i % 5).to_float() * 2.0  // Values between 100-108
    let data_point = TimeSeriesDataPoint::new(timestamp, value, None)
    TimeSeriesCollection::add_point(collection, data_point)
  }
  
  // Detect anomalies using standard deviation method
  let anomalies = TimeSeriesCollection::detect_anomalies(
    collection, 
    AnomalyDetectionMethod::StandardDeviation,
    2.0  // 2 standard deviations threshold
  )
  
  // Should detect at least the one anomaly we added
  assert_true(anomalies.length() >= 1)
  
  // Verify the anomaly is detected
  let found_anomaly = false
  for anomaly in anomalies {
    if TimeSeriesDataPoint::timestamp(anomaly) == anomaly_timestamp {
      assert_eq(TimeSeriesDataPoint::value(anomaly), 500.0)
      found_anomaly = true
    }
  }
  assert_true(found_anomaly)
}

// Test 5: Time Series Forecasting
test "time series forecasting" {
  let collection = TimeSeriesCollection::new("network_traffic")
  
  // Add data points with a clear trend (increasing)
  let base_timestamp = 1640995200000L
  for i in 0..=23 {  // 24 points = 24 hours
    let timestamp = base_timestamp + (i * 3600000L)  // 1 hour intervals
    let value = 1000.0 + (i * 50.0).to_float()  // Clear increasing trend
    let data_point = TimeSeriesDataPoint::new(timestamp, value, None)
    TimeSeriesCollection::add_point(collection, data_point)
  }
  
  // Forecast next 6 points using linear regression
  let forecast = TimeSeriesCollection::forecast(
    collection, 
    6,  // Forecast 6 points ahead
    ForecastingMethod::LinearRegression
  )
  
  assert_eq(forecast.length(), 6)
  
  // Verify forecast values are continuing the trend
  // Last actual value: 1000 + 23 * 50 = 2150
  // Next forecasted value should be around 2200 (2150 + 50)
  let first_forecast = forecast[0]
  assert_true(TimeSeriesDataPoint::value(first_forecast) > 2150.0)
  
  // Verify timestamps are correct
  let last_actual_timestamp = base_timestamp + (23 * 3600000L)
  let first_forecast_timestamp = TimeSeriesDataPoint::timestamp(first_forecast)
  assert_eq(first_forecast_timestamp, last_actual_timestamp + 3600000L)
}

// Test 6: Time Series Compression
test "time series compression" {
  let collection = TimeSeriesCollection::new("sensor_data")
  
  // Add data points
  let base_timestamp = 1640995200000L
  for i in 0..=99 {  // 100 points
    let timestamp = base_timestamp + (i * 60000L)
    let value = 20.0 + (i % 10).to_float() * 0.5  // Repeating pattern
    let data_point = TimeSeriesDataPoint::new(timestamp, value, None)
    TimeSeriesCollection::add_point(collection, data_point)
  }
  
  // Compress the time series using delta encoding
  let compressed = TimeSeriesCollection::compress(
    collection, 
    CompressionMethod::DeltaEncoding
  )
  
  // Verify compression reduces size
  assert_true(compressed.length() < collection.size())
  
  // Decompress and verify data integrity
  let decompressed = TimeSeriesCollection::decompress(compressed)
  assert_eq(TimeSeriesCollection::size(decompressed), TimeSeriesCollection::size(collection))
  
  // Verify values are the same
  for i in 0..=99 {
    let original_point = TimeSeriesCollection::get_point_at(collection, i)
    let decompressed_point = TimeSeriesCollection::get_point_at(decompressed, i)
    
    assert_eq(
      TimeSeriesDataPoint::timestamp(original_point),
      TimeSeriesDataPoint::timestamp(decompressed_point)
    )
    assert_eq(
      TimeSeriesDataPoint::value(original_point),
      TimeSeriesDataPoint::value(decompressed_point)
    )
  }
}

// Test 7: Time Series Window Operations
test "time series window operations" {
  let collection = TimeSeriesCollection::new("stock_price")
  
  // Add data points
  let base_timestamp = 1640995200000L
  for i in 0..=29 {  // 30 points
    let timestamp = base_timestamp + (i * 86400000L)  // 1 day intervals
    let value = 100.0 + (i % 7).to_float() * 2.0  // Weekly pattern
    let data_point = TimeSeriesDataPoint::new(timestamp, value, None)
    TimeSeriesCollection::add_point(collection, data_point)
  }
  
  // Calculate moving average with window size of 7
  let moving_avg = TimeSeriesCollection::moving_average(collection, 7)
  
  // Moving average should have fewer points (window_size - 1 fewer)
  assert_eq(moving_avg.length(), collection.size() - 6)
  
  // Verify first moving average value
  let first_ma = moving_avg[0]
  // Average of first 7 values: (100 + 102 + 104 + 106 + 108 + 100 + 102) / 7 = 103.14
  assert_true(TimeSeriesDataPoint::value(first_ma) > 103.0)
  assert_true(TimeSeriesDataPoint::value(first_ma) < 103.2)
  
  // Calculate moving standard deviation
  let moving_std = TimeSeriesCollection::moving_standard_deviation(collection, 7)
  assert_eq(moving_std.length(), collection.size() - 6)
}

// Test 8: Time Series Correlation Analysis
test "time series correlation analysis" {
  let collection1 = TimeSeriesCollection::new("temperature")
  let collection2 = TimeSeriesCollection::new("ice_cream_sales")
  
  // Add correlated data points
  let base_timestamp = 1640995200000L
  for i in 0..=29 {  // 30 points
    let timestamp = base_timestamp + (i * 86400000L)  // 1 day intervals
    
    // Temperature: higher in summer, lower in winter
    let temp_value = 20.0 + 15.0 * @sin(i * 0.2)  // Sinusoidal pattern
    let temp_point = TimeSeriesDataPoint::new(timestamp, temp_value, None)
    TimeSeriesCollection::add_point(collection1, temp_point)
    
    // Ice cream sales: correlated with temperature
    let sales_value = 100.0 + 50.0 * @sin(i * 0.2)  // Same pattern, different scale
    let sales_point = TimeSeriesDataPoint::new(timestamp, sales_value, None)
    TimeSeriesCollection::add_point(collection2, sales_point)
  }
  
  // Calculate correlation coefficient
  let correlation = TimeSeriesCollection::correlation(collection1, collection2)
  
  // Should be highly correlated (close to 1.0)
  assert_true(correlation > 0.9)
  
  // Test with uncorrelated data
  let collection3 = TimeSeriesCollection::new("random_data")
  for i in 0..=29 {
    let timestamp = base_timestamp + (i * 86400000L)
    let random_value = 100.0 + (i * 13).to_float() % 50.0  // Random pattern
    let random_point = TimeSeriesDataPoint::new(timestamp, random_value, None)
    TimeSeriesCollection::add_point(collection3, random_point)
  }
  
  let no_correlation = TimeSeriesCollection::correlation(collection1, collection3)
  // Should be low correlation (close to 0.0)
  assert_true(@abs(no_correlation) < 0.3)
}

// Test 9: Time Series Seasonality Detection
test "time series seasonality detection" {
  let collection = TimeSeriesCollection::new("monthly_sales")
  
  // Add data points with yearly seasonality
  let base_timestamp = 1640995200000L  // 2022-01-01
  for i in 0..=47 {  // 48 months = 4 years
    let timestamp = base_timestamp + (i * 2592000000L)  // 30 days intervals (approximate month)
    let month = i % 12
    
    // Higher sales in summer (months 5-7) and winter holidays (month 11)
    let seasonal_factor = 
      if month >= 5 && month <= 7 { 1.5 }  // Summer
      else if month == 11 { 1.8 }  // Holiday season
      else { 1.0 }
    
    let base_sales = 10000.0
    let trend_factor = 1.0 + (i / 12).to_float() * 0.1  // 10% growth per year
    let sales_value = base_sales * seasonal_factor * trend_factor
    
    let data_point = TimeSeriesDataPoint::new(timestamp, sales_value, None)
    TimeSeriesCollection::add_point(collection, data_point)
  }
  
  // Detect seasonality
  let seasonality = TimeSeriesCollection::detect_seasonality(collection, 12)  // Check for 12-month seasonality
  
  // Should detect seasonality
  assert_true(seasonality.detected)
  assert_eq(seasonality.period, 12)
  assert_true(seasonality.strength > 0.5)  // Strong seasonality
}

// Test 10: Time Series Gap Filling
test "time series gap filling" {
  let collection = TimeSeriesCollection::new("metric_with_gaps")
  
  // Add data points with gaps
  let base_timestamp = 1640995200000L
  for i in 0..=19 {
    // Skip some points to create gaps
    if i != 5 && i != 6 && i != 12 && i != 13 && i != 14 {
      let timestamp = base_timestamp + (i * 60000L)
      let value = 100.0 + (i * 2.0).to_float()
      let data_point = TimeSeriesDataPoint::new(timestamp, value, None)
      TimeSeriesCollection::add_point(collection, data_point)
    }
  }
  
  // Fill gaps using linear interpolation
  let filled = TimeSeriesCollection::fill_gaps(
    collection, 
    GapFillMethod::LinearInterpolation,
    60000L  // Expected interval
  )
  
  // Should have all 20 points after filling
  assert_eq(TimeSeriesCollection::size(filled), 20)
  
  // Verify filled points
  // Check point at index 5 (gap filled)
  let point_5 = TimeSeriesCollection::get_point_at(filled, 5)
  assert_eq(TimeSeriesDataPoint::timestamp(point_5), base_timestamp + (5 * 60000L))
  // Value should be interpolated between points 4 (108) and 7 (114)
  // Expected: 108 + (114 - 108) * (1/3) = 110.0
  assert_eq(TimeSeriesDataPoint::value(point_5), 110.0)
  
  // Check point at index 6 (gap filled)
  let point_6 = TimeSeriesCollection::get_point_at(filled, 6)
  assert_eq(TimeSeriesDataPoint::timestamp(point_6), base_timestamp + (6 * 60000L))
  // Expected: 108 + (114 - 108) * (2/3) = 112.0
  assert_eq(TimeSeriesDataPoint::value(point_6), 112.0)
  
  // Check point at index 12-14 (gap filled)
  let point_13 = TimeSeriesCollection::get_point_at(filled, 13)
  assert_eq(TimeSeriesDataPoint::timestamp(point_13), base_timestamp + (13 * 60000L))
  // Value should be interpolated between points 11 (122) and 15 (130)
  // Expected: 122 + (130 - 122) * (2/4) = 126.0
  assert_eq(TimeSeriesDataPoint::value(point_13), 126.0)
}