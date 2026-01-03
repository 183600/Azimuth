// Azimuth Time Series Processing Test Suite
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Operations
test "time series data point operations" {
  // Test creating data points
  let timestamp = current_timestamp()
  let data_point = TimeSeriesPoint::new(timestamp, 42.5)
  
  assert_eq(TimeSeriesPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesPoint::value(data_point), 42.5)
  
  // Test data point with metadata
  let metadata = [("source", "sensor1"), ("unit", "celsius")]
  let data_point_with_meta = TimeSeriesPoint::with_metadata(timestamp, 36.7, metadata)
  
  assert_eq(TimeSeriesPoint::value(data_point_with_meta), 36.7)
  match TimeSeriesPoint::get_metadata(data_point_with_meta, "source") {
    Some(value) => assert_eq(value, "sensor1")
    None => assert_true(false)
  }
  
  // Test data point comparison
  let earlier_point = TimeSeriesPoint::new(timestamp - 1000L, 40.0)
  let later_point = TimeSeriesPoint::new(timestamp + 1000L, 45.0)
  
  assert_true(TimeSeriesPoint::is_before(earlier_point, later_point))
  assert_false(TimeSeriesPoint::is_before(later_point, earlier_point))
}

// Test 2: Time Series Collection Operations
test "time series collection operations" {
  // Test creating time series
  let series = TimeSeries::new("temperature", "celsius")
  
  assert_eq(TimeSeries::name(series), "temperature")
  assert_eq(TimeSeries::unit(series), "celsius")
  assert_eq(TimeSeries::length(series), 0)
  
  // Test adding data points
  let base_timestamp = current_timestamp()
  let point1 = TimeSeriesPoint::new(base_timestamp, 20.0)
  let point2 = TimeSeriesPoint::new(base_timestamp + 1000L, 21.5)
  let point3 = TimeSeriesPoint::new(base_timestamp + 2000L, 22.0)
  
  let series_with_points = TimeSeries::add_point(
    TimeSeries::add_point(
      TimeSeries::add_point(series, point1), point2
    ), point3
  )
  
  assert_eq(TimeSeries::length(series_with_points), 3)
  
  // Test getting data points by index
  match TimeSeries::get_point(series_with_points, 0) {
    Some(point) => assert_eq(TimeSeriesPoint::value(point), 20.0)
    None => assert_true(false)
  }
  
  // Test getting data points by time range
  let range_points = TimeSeries::get_points_in_range(
    series_with_points, 
    base_timestamp + 500L, 
    base_timestamp + 2500L
  )
  assert_eq(range_points.length(), 2)
}

// Test 3: Time Series Aggregation Operations
test "time series aggregation operations" {
  // Create sample time series data
  let base_timestamp = current_timestamp()
  let points = [
    TimeSeriesPoint::new(base_timestamp, 10.0),
    TimeSeriesPoint::new(base_timestamp + 1000L, 20.0),
    TimeSeriesPoint::new(base_timestamp + 2000L, 30.0),
    TimeSeriesPoint::new(base_timestamp + 3000L, 40.0),
    TimeSeriesPoint::new(base_timestamp + 4000L, 50.0)
  ]
  
  let series = TimeSeries::from_points("test_series", "units", points)
  
  // Test average calculation
  let avg = TimeSeries::average(series)
  assert_eq(avg, 30.0)
  
  // Test min/max calculation
  let min = TimeSeries::minimum(series)
  let max = TimeSeries::maximum(series)
  assert_eq(min, 10.0)
  assert_eq(max, 50.0)
  
  // Test sum calculation
  let sum = TimeSeries::sum(series)
  assert_eq(sum, 150.0)
  
  // Test count calculation
  let count = TimeSeries::count(series)
  assert_eq(count, 5)
}

// Test 4: Time Series Resampling Operations
test "time series resampling operations" {
  // Create high-frequency time series data
  let base_timestamp = current_timestamp()
  let high_freq_points = []
  for i in 0..=60 {
    high_freq_points = high_freq_points.push(
      TimeSeriesPoint::new(base_timestamp + i * 1000L, (i as Float) * 0.5)
    )
  }
  
  let high_freq_series = TimeSeries::from_points("high_freq", "value", high_freq_points)
  
  // Test downsampling (e.g., every 10 seconds)
  let downsampled = TimeSeries::downsample(high_freq_series, 10000L, Aggregation::Average)
  assert_eq(TimeSeries::length(downsampled), 7) // 61 points / 10 second intervals
  
  // Test upsampling (e.g., every 0.5 seconds)
  let upsampled = TimeSeries::upsample(high_freq_series, 500L, Interpolation::Linear)
  assert_true(TimeSeries::length(upsampled) > TimeSeries::length(high_freq_series))
  
  // Test different aggregation methods
  let max_resampled = TimeSeries::downsample(high_freq_series, 10000L, Aggregation::Maximum)
  let min_resampled = TimeSeries::downsample(high_freq_series, 10000L, Aggregation::Minimum)
  
  // Verify aggregation results
  match TimeSeries::get_point(max_resampled, 0) {
    Some(point) => assert_eq(TimeSeriesPoint::value(point), 4.5) // Max of first 10 points
    None => assert_true(false)
  }
}

// Test 5: Time Series Window Operations
test "time series window operations" {
  // Create time series data
  let base_timestamp = current_timestamp()
  let points = [
    TimeSeriesPoint::new(base_timestamp, 100.0),
    TimeSeriesPoint::new(base_timestamp + 1000L, 110.0),
    TimeSeriesPoint::new(base_timestamp + 2000L, 105.0),
    TimeSeriesPoint::new(base_timestamp + 3000L, 120.0),
    TimeSeriesPoint::new(base_timestamp + 4000L, 115.0),
    TimeSeriesPoint::new(base_timestamp + 5000L, 130.0)
  ]
  
  let series = TimeSeries::from_points("window_test", "value", points)
  
  // Test moving average
  let moving_avg = TimeSeries::moving_average(series, 3)
  assert_eq(TimeSeries::length(moving_avg), 4) // 6 - window_size + 1
  
  // Test moving sum
  let moving_sum = TimeSeries::moving_sum(series, 2)
  assert_eq(TimeSeries::length(moving_sum), 5)
  
  // Verify moving average calculation
  match TimeSeries::get_point(moving_avg, 0) {
    Some(point) => assert_eq(TimeSeriesPoint::value(point), 105.0) // (100+110+105)/3
    None => assert_true(false)
  }
  
  // Test time-based window
  let time_window = TimeSeries::time_window(series, 2500L)
  assert_true(TimeSeries::length(time_window) > 0)
}

// Test 6: Time Series Trend Analysis
test "time series trend analysis" {
  // Create trend data
  let base_timestamp = current_timestamp()
  let trend_points = [
    TimeSeriesPoint::new(base_timestamp, 10.0),
    TimeSeriesPoint::new(base_timestamp + 1000L, 15.0),
    TimeSeriesPoint::new(base_timestamp + 2000L, 20.0),
    TimeSeriesPoint::new(base_timestamp + 3000L, 25.0),
    TimeSeriesPoint::new(base_timestamp + 4000L, 30.0)
  ]
  
  let trend_series = TimeSeries::from_points("trend_test", "value", trend_points)
  
  // Test linear regression
  let regression = TimeSeries::linear_regression(trend_series)
  assert_true(regression.slope > 0.0) // Positive trend
  assert_true(regression.intercept > 0.0)
  
  // Test trend detection
  let trend = TimeSeries::detect_trend(trend_series)
  assert_eq(trend, Trend::Increasing)
  
  // Test with decreasing data
  let decreasing_points = [
    TimeSeriesPoint::new(base_timestamp, 50.0),
    TimeSeriesPoint::new(base_timestamp + 1000L, 40.0),
    TimeSeriesPoint::new(base_timestamp + 2000L, 30.0),
    TimeSeriesPoint::new(base_timestamp + 3000L, 20.0),
    TimeSeriesPoint::new(base_timestamp + 4000L, 10.0)
  ]
  
  let decreasing_series = TimeSeries::from_points("decreasing_test", "value", decreasing_points)
  let decreasing_trend = TimeSeries::detect_trend(decreasing_series)
  assert_eq(decreasing_trend, Trend::Decreasing)
  
  // Test with flat data
  let flat_points = [
    TimeSeriesPoint::new(base_timestamp, 25.0),
    TimeSeriesPoint::new(base_timestamp + 1000L, 25.0),
    TimeSeriesPoint::new(base_timestamp + 2000L, 25.0),
    TimeSeriesPoint::new(base_timestamp + 3000L, 25.0),
    TimeSeriesPoint::new(base_timestamp + 4000L, 25.0)
  ]
  
  let flat_series = TimeSeries::from_points("flat_test", "value", flat_points)
  let flat_trend = TimeSeries::detect_trend(flat_series)
  assert_eq(flat_trend, Trend::Stable)
}

// Test 7: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Create data with anomalies
  let base_timestamp = current_timestamp()
  let anomaly_points = [
    TimeSeriesPoint::new(base_timestamp, 20.0),
    TimeSeriesPoint::new(base_timestamp + 1000L, 21.0),
    TimeSeriesPoint::new(base_timestamp + 2000L, 19.5),
    TimeSeriesPoint::new(base_timestamp + 3000L, 100.0), // Anomaly
    TimeSeriesPoint::new(base_timestamp + 4000L, 20.5),
    TimeSeriesPoint::new(base_timestamp + 5000L, 21.5),
    TimeSeriesPoint::new(base_timestamp + 6000L, -50.0), // Anomaly
    TimeSeriesPoint::new(base_timestamp + 7000L, 20.0)
  ]
  
  let anomaly_series = TimeSeries::from_points("anomaly_test", "value", anomaly_points)
  
  // Test statistical anomaly detection
  let anomalies = TimeSeries::detect_anomalies(anomaly_series, AnomalyMethod::Statistical, 2.0)
  assert_eq(anomalies.length(), 2)
  
  // Verify anomaly timestamps
  let anomaly_timestamps = anomalies.map(fn(point) { TimeSeriesPoint::timestamp(point) })
  assert_true(anomaly_timestamps.contains(base_timestamp + 3000L))
  assert_true(anomaly_timestamps.contains(base_timestamp + 6000L))
  
  // Test with no anomalies
  let normal_points = [
    TimeSeriesPoint::new(base_timestamp, 20.0),
    TimeSeriesPoint::new(base_timestamp + 1000L, 21.0),
    TimeSeriesPoint::new(base_timestamp + 2000L, 19.5),
    TimeSeriesPoint::new(base_timestamp + 3000L, 20.5),
    TimeSeriesPoint::new(base_timestamp + 4000L, 21.5)
  ]
  
  let normal_series = TimeSeries::from_points("normal_test", "value", normal_points)
  let no_anomalies = TimeSeries::detect_anomalies(normal_series, AnomalyMethod::Statistical, 2.0)
  assert_eq(no_anomalies.length(), 0)
}

// Test 8: Time Series Forecasting
test "time series forecasting" {
  // Create historical data
  let base_timestamp = current_timestamp()
  let historical_points = []
  for i in 0..=24 { // 25 data points
    historical_points = historical_points.push(
      TimeSeriesPoint::new(
        base_timestamp + (i * 3600000L), // Hourly data
        20.0 + (i as Float) * 0.5 + (i as Float) * 0.1 * ((i % 4) as Float - 2.0) // Some pattern
      )
    )
  }
  
  let historical_series = TimeSeries::from_points("historical", "value", historical_points)
  
  // Test simple linear forecasting
  let forecast = TimeSeries::forecast(historical_series, ForecastMethod::Linear, 5)
  assert_eq(TimeSeries::length(forecast), 5)
  
  // Verify forecast timestamps are in the future
  match TimeSeries::get_point(forecast, 0) {
    Some(point) => {
      let forecast_timestamp = TimeSeriesPoint::timestamp(point)
      assert_true(forecast_timestamp > base_timestamp + (24 * 3600000L))
    }
    None => assert_true(false)
  }
  
  // Test moving average forecasting
  let ma_forecast = TimeSeries::forecast(historical_series, ForecastMethod::MovingAverage, 3)
  assert_eq(TimeSeries::length(ma_forecast), 3)
  
  // Test seasonal forecasting (if available)
  let seasonal_forecast = TimeSeries::forecast(historical_series, ForecastMethod::Seasonal, 4)
  assert_eq(TimeSeries::length(seasonal_forecast), 4)
}

// Test 9: Time Series Compression
test "time series compression" {
  // Create high-resolution data
  let base_timestamp = current_timestamp()
  let high_res_points = []
  for i in 0..=1000 {
    high_res_points = high_res_points.push(
      TimeSeriesPoint::new(
        base_timestamp + (i * 1000L),
        100.0 + 10.0 * ((i % 10) as Float - 5.0) + 0.1 * (i as Float)
      )
    )
  }
  
  let high_res_series = TimeSeries::from_points("high_res", "value", high_res_points)
  
  // Test delta compression
  let compressed = TimeSeries::compress(high_res_series, CompressionMethod::Delta)
  assert_true(TimeSeries::size(compressed) < TimeSeries::size(high_res_series))
  
  // Test decompression
  let decompressed = TimeSeries::decompress(compressed)
  assert_eq(TimeSeries::length(decompressed), TimeSeries::length(high_res_series))
  
  // Verify data integrity after compression/decompression
  for i in 0..=10 {
    match (TimeSeries::get_point(high_res_series, i), TimeSeries::get_point(decompressed, i)) {
      (Some(original), Some(decomp)) => {
        assert_eq(TimeSeriesPoint::value(original), TimeSeriesPoint::value(decomp))
        assert_eq(TimeSeriesPoint::timestamp(original), TimeSeriesPoint::timestamp(decomp))
      }
      _ => assert_true(false)
    }
  }
  
  // Test swing door compression
  let swing_compressed = TimeSeries::compress(high_res_series, CompressionMethod::SwingDoor, 0.1)
  assert_true(TimeSeries::size(swing_compressed) < TimeSeries::size(high_res_series))
}

// Test 10: Time Series Query Operations
test "time series query operations" {
  // Create sample data with different patterns
  let base_timestamp = current_timestamp()
  let query_points = [
    TimeSeriesPoint::with_metadata(base_timestamp, 10.0, [("region", "north"), ("type", "temperature")]),
    TimeSeriesPoint::with_metadata(base_timestamp + 1000L, 20.0, [("region", "south"), ("type", "temperature")]),
    TimeSeriesPoint::with_metadata(base_timestamp + 2000L, 100.0, [("region", "north"), ("type", "humidity")]),
    TimeSeriesPoint::with_metadata(base_timestamp + 3000L, 200.0, [("region", "south"), ("type", "humidity")]),
    TimeSeriesPoint::with_metadata(base_timestamp + 4000L, 15.0, [("region", "north"), ("type", "temperature")])
  ]
  
  let query_series = TimeSeries::from_points("query_test", "value", query_points)
  
  // Test metadata filtering
  let north_data = TimeSeries::filter_by_metadata(query_series, "region", "north")
  assert_eq(TimeSeries::length(north_data), 3)
  
  let temperature_data = TimeSeries::filter_by_metadata(query_series, "type", "temperature")
  assert_eq(TimeSeries::length(temperature_data), 3)
  
  // Test complex queries
  let north_temp_data = TimeSeries::filter_by_multiple_metadata(query_series, [
    ("region", "north"),
    ("type", "temperature")
  ])
  assert_eq(TimeSeries::length(north_temp_data), 2)
  
  // Test value range queries
  let high_values = TimeSeries::filter_by_value_range(query_series, 50.0, 250.0)
  assert_eq(TimeSeries::length(high_values), 2)
  
  // Test time-based queries
  let recent_data = TimeSeries::filter_by_time_range(
    query_series, 
    base_timestamp + 1500L, 
    base_timestamp + 3500L
  )
  assert_eq(TimeSeries::length(recent_data), 2)
  
  // Test aggregation with grouping
  let grouped_avg = TimeSeries::aggregate_by_metadata(query_series, "region", Aggregation::Average)
  assert_true(grouped_avg.length() > 0)
}