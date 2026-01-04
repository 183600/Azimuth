// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Creation and Validation
test "time series data point creation and validation" {
  // Create a basic time series data point
  let timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC in milliseconds
  let value = 42.5
  let metric_name = "cpu_usage"
  let tags = [("host", "server-01"), ("region", "us-west-2")]
  
  let data_point = TimeSeriesDataPoint::new(timestamp, value, metric_name, tags)
  
  // Validate data point properties
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
  assert_eq(TimeSeriesDataPoint::metric_name(data_point), metric_name)
  assert_eq(TimeSeriesDataPoint::tags(data_point).length(), 2)
  assert_true(TimeSeriesDataPoint::tags(data_point).contains(("host", "server-01")))
  assert_true(TimeSeriesDataPoint::tags(data_point).contains(("region", "us-west-2")))
  
  // Test data point with additional metadata
  let metadata = [("unit", "percent"), ("source", "system_monitor")]
  let enriched_data_point = TimeSeriesDataPoint::with_metadata(timestamp, value, metric_name, tags, metadata)
  
  assert_eq(TimeSeriesDataPoint::metadata(enriched_data_point).length(), 2)
  assert_true(TimeSeriesDataPoint::metadata(enriched_data_point).contains(("unit", "percent")))
  assert_true(TimeSeriesDataPoint::metadata(enriched_data_point).contains(("source", "system_monitor")))
  
  // Test data point validation
  let valid_point = TimeSeriesDataPoint::new(1640995200000, 50.0, "memory_usage", [])
  assert_true(TimeSeriesDataPoint::is_valid(valid_point))
  
  // Test invalid timestamp (too old)
  let old_timestamp = 946684800000 // 2000-01-01 00:00:00 UTC
  let invalid_point_old = TimeSeriesDataPoint::new(old_timestamp, 50.0, "memory_usage", [])
  assert_false(TimeSeriesDataPoint::is_valid(invalid_point_old))
  
  // Test invalid metric name (empty)
  let invalid_point_name = TimeSeriesDataPoint::new(1640995200000, 50.0, "", [])
  assert_false(TimeSeriesDataPoint::is_valid(invalid_point_name))
  
  // Test invalid value (NaN)
  let invalid_point_nan = TimeSeriesDataPoint::new(1640995200000, NaN, "memory_usage", [])
  assert_false(TimeSeriesDataPoint::is_valid(invalid_point_nan))
}

// Test 2: Time Series Data Aggregation
test "time series data aggregation" {
  // Create time series data points for aggregation
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  let data_points = []
  
  // Add data points at 1-minute intervals
  for i in 0..=59 {
    let timestamp = base_timestamp + i * 60000 // 60 seconds = 1 minute
    let value = 50.0 + (i % 10).to_float() // Values between 50.0 and 59.0
    let data_point = TimeSeriesDataPoint::new(timestamp, value, "cpu_usage", [("host", "server-01")])
    data_points = data_points.push(data_point)
  }
  
  // Test aggregation by time window (5-minute windows)
  let aggregated_5min = TimeSeriesAggregator::aggregate_by_time_window(data_points, 300000) // 5 minutes in ms
  
  assert_eq(aggregated_5min.length(), 12) // 60 minutes / 5 minutes = 12 windows
  
  // Verify first window (0-5 minutes)
  let first_window = aggregated_5min[0]
  assert_eq(TimeSeriesDataPoint::timestamp(first_window), base_timestamp)
  assert_eq(TimeSeriesDataPoint::metric_name(first_window), "cpu_usage.cpu_avg") // Aggregated metric name
  
  // Calculate expected average for first window (50.0, 51.0, 52.0, 53.0, 54.0)
  let expected_avg = (50.0 + 51.0 + 52.0 + 53.0 + 54.0) / 5.0
  assert_eq(TimeSeriesDataPoint::value(first_window), expected_avg)
  
  // Test different aggregation functions
  let sum_aggregated = TimeSeriesAggregator::aggregate_by_time_window(data_points, 300000, Sum)
  let max_aggregated = TimeSeriesAggregator::aggregate_by_time_window(data_points, 300000, Max)
  let min_aggregated = TimeSeriesAggregator::aggregate_by_time_window(data_points, 300000, Min)
  
  // Verify sum for first window
  let expected_sum = 50.0 + 51.0 + 52.0 + 53.0 + 54.0
  assert_eq(TimeSeriesDataPoint::value(sum_aggregated[0]), expected_sum)
  
  // Verify max for first window
  assert_eq(TimeSeriesDataPoint::value(max_aggregated[0]), 54.0)
  
  // Verify min for first window
  assert_eq(TimeSeriesDataPoint::value(min_aggregated[0]), 50.0)
  
  // Test aggregation by tags
  let multi_host_data = []
  
  // Add data points for different hosts
  for host_idx in 0..=2 {
    let host_name = "server-0" + (host_idx + 1).to_string()
    for i in 0..=9 {
      let timestamp = base_timestamp + i * 60000
      let value = 40.0 + host_idx.to_float() * 10.0 + i.to_float()
      let data_point = TimeSeriesDataPoint::new(timestamp, value, "cpu_usage", [("host", host_name)])
      multi_host_data = multi_host_data.push(data_point)
    }
  }
  
  // Aggregate by host tag
  let aggregated_by_host = TimeSeriesAggregator::aggregate_by_tags(multi_host_data, ["host"])
  
  assert_eq(aggregated_by_host.length(), 3) // 3 different hosts
  
  // Verify aggregation for server-01
  let server01_data = aggregated_by_host.find(fn(dp) {
    TimeSeriesDataPoint::tags(dp).contains(("host", "server-01"))
  })
  match server01_data {
    Some(dp) => {
      // Expected average: (40.0 + 41.0 + ... + 49.0) / 10 = 44.5
      assert_eq(TimeSeriesDataPoint::value(dp), 44.5)
      assert_eq(TimeSeriesDataPoint::metric_name(dp), "cpu_usage.cpu_avg")
    }
    None => assert_true(false)
  }
}

// Test 3: Time Series Data Downsampling and Upsampling
test "time series data downsampling and upsampling" {
  // Create high-frequency time series data (1-second intervals)
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  let high_freq_data = []
  
  for i in 0..=3599 { // 1 hour of data at 1-second intervals
    let timestamp = base_timestamp + i * 1000 // 1 second in ms
    let value = 50.0 + (i % 60).to_float() // Pattern repeats every minute
    let data_point = TimeSeriesDataPoint::new(timestamp, value, "cpu_usage", [("host", "server-01")])
    high_freq_data = high_freq_data.push(data_point)
  }
  
  // Test downsampling to 1-minute intervals
  let downsampled_1min = TimeSeriesResampler::downsample(high_freq_data, 60000) // 1 minute in ms
  
  assert_eq(downsampled_1min.length(), 60) // 3600 seconds / 60 seconds = 60 minutes
  
  // Verify first downsampled point (average of first minute)
  let first_minute_avg = downsampled_1min[0]
  assert_eq(TimeSeriesDataPoint::timestamp(first_minute_avg), base_timestamp)
  
  // Calculate expected average for first minute (50.0, 51.0, ..., 59.0)
  let mut expected_sum = 0.0
  for i in 0..=59 {
    expected_sum = expected_sum + (50.0 + i.to_float())
  }
  let expected_avg = expected_sum / 60.0
  assert_eq(TimeSeriesDataPoint::value(first_minute_avg), expected_avg)
  
  // Test downsampling with different strategies
  let downsampled_max = TimeSeriesResampler::downsample(high_freq_data, 60000, Max)
  let downsampled_min = TimeSeriesResampler::downsample(high_freq_data, 60000, Min)
  
  // Verify max for first minute
  assert_eq(TimeSeriesDataPoint::value(downsampled_max[0]), 59.0)
  
  // Verify min for first minute
  assert_eq(TimeSeriesDataPoint::value(downsampled_min[0]), 50.0)
  
  // Test upsampling from 1-minute to 30-second intervals
  let upsampled_30sec = TimeSeriesResampler::upsample(downsampled_1min, 30000, Linear)
  
  assert_eq(upsampled_30sec.length(), 119) // 60 minutes * 2 - 1 (interpolated points)
  
  // Verify first upsampled point (same as original)
  assert_eq(TimeSeriesDataPoint::timestamp(upsampled_30sec[0]), base_timestamp)
  assert_eq(TimeSeriesDataPoint::value(upsampled_30sec[0]), expected_avg)
  
  // Verify interpolated point at 30 seconds
  let interpolated_point = upsampled_30sec[1]
  assert_eq(TimeSeriesDataPoint::timestamp(interpolated_point), base_timestamp + 30000)
  
  // Linear interpolation between first and second minute averages
  let first_avg = TimeSeriesDataPoint::value(downsampled_1min[0])
  let second_avg = TimeSeriesDataPoint::value(downsampled_1min[1])
  let expected_interpolated = (first_avg + second_avg) / 2.0
  assert_eq(TimeSeriesDataPoint::value(interpolated_point), expected_interpolated)
  
  // Test upsampling with different strategies
  let upsampled_step = TimeSeriesResampler::upsample(downsampled_1min, 30000, Step)
  
  // Step interpolation should carry forward the previous value
  assert_eq(TimeSeriesDataPoint::value(upsampled_step[1]), expected_avg) // Same as first minute
}

// Test 4: Time Series Data Filtering and Querying
test "time series data filtering and querying" {
  // Create time series data for filtering
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  let data_points = []
  
  // Add data points with different metrics and tags
  for i in 0..=23 { // 24 hours of data
    let timestamp = base_timestamp + i * 3600000 // 1 hour in ms
    
    // CPU usage data
    let cpu_value = 30.0 + (i % 12).to_float() * 5.0
    let cpu_point = TimeSeriesDataPoint::new(timestamp, cpu_value, "cpu_usage", [
      ("host", "server-01"),
      ("datacenter", "us-west")
    ])
    data_points = data_points.push(cpu_point)
    
    // Memory usage data
    let mem_value = 60.0 + (i % 8).to_float() * 5.0
    let mem_point = TimeSeriesDataPoint::new(timestamp, mem_value, "memory_usage", [
      ("host", "server-01"),
      ("datacenter", "us-west")
    ])
    data_points = data_points.push(mem_point)
    
    // Network traffic data for different host
    if i % 2 == 0 {
      let net_value = 100.0 + i.to_float() * 10.0
      let net_point = TimeSeriesDataPoint::new(timestamp, net_value, "network_traffic", [
        ("host", "server-02"),
        ("datacenter", "us-east")
      ])
      data_points = data_points.push(net_point)
    }
  }
  
  // Test filtering by metric name
  let cpu_data = TimeSeriesFilter::filter_by_metric(data_points, "cpu_usage")
  assert_eq(cpu_data.length(), 24)
  
  for point in cpu_data {
    assert_eq(TimeSeriesDataPoint::metric_name(point), "cpu_usage")
  }
  
  // Test filtering by tags
  let server01_data = TimeSeriesFilter::filter_by_tag(data_points, "host", "server-01")
  assert_eq(server01_data.length(), 48) // 24 CPU + 24 memory points
  
  for point in server01_data {
    assert_true(TimeSeriesDataPoint::tags(point).contains(("host", "server-01")))
  }
  
  // Test filtering by time range
  let start_time = base_timestamp + 6 * 3600000 // 6 hours after start
  let end_time = base_timestamp + 18 * 3600000 // 18 hours after start
  let time_filtered = TimeSeriesFilter::filter_by_time_range(data_points, start_time, end_time)
  
  assert_eq(time_filtered.length(), 26) // 13 hours * 2 metrics (CPU + memory)
  
  for point in time_filtered {
    let timestamp = TimeSeriesDataPoint::timestamp(point)
    assert_true(timestamp >= start_time && timestamp <= end_time)
  }
  
  // Test filtering by value range
  let high_cpu_data = TimeSeriesFilter::filter_by_value_range(cpu_data, 60.0, 100.0)
  
  for point in high_cpu_data {
    let value = TimeSeriesDataPoint::value(point)
    assert_true(value >= 60.0 && value <= 100.0)
  }
  
  // Test complex filtering with multiple criteria
  let complex_filter = TimeSeriesFilter::builder()
    .metric("memory_usage")
    .tag("host", "server-01")
    .time_range(base_timestamp + 12 * 3600000, base_timestamp + 24 * 3600000)
    .value_range(70.0, 100.0)
    .build()
  
  let complex_filtered = TimeSeriesFilter::apply(data_points, complex_filter)
  
  for point in complex_filtered {
    assert_eq(TimeSeriesDataPoint::metric_name(point), "memory_usage")
    assert_true(TimeSeriesDataPoint::tags(point).contains(("host", "server-01")))
    let timestamp = TimeSeriesDataPoint::timestamp(point)
    assert_true(timestamp >= base_timestamp + 12 * 3600000 && timestamp <= base_timestamp + 24 * 3600000)
    let value = TimeSeriesDataPoint::value(point)
    assert_true(value >= 70.0 && value <= 100.0)
  }
  
  // Test querying with regular expressions
  let regex_filtered = TimeSeriesFilter::filter_by_metric_regex(data_points, ".*_usage")
  assert_eq(regex_filtered.length(), 48) // CPU + memory usage
  
  for point in regex_filtered {
    let metric_name = TimeSeriesDataPoint::metric_name(point)
    assert_true(metric_name.contains("_usage"))
  }
}

// Test 5: Time Series Data Anomaly Detection
test "time series data anomaly detection" {
  // Create time series data with anomalies
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  let normal_data = []
  
  // Create normal data points (CPU usage around 50% with small variations)
  for i in 0..=99 {
    let timestamp = base_timestamp + i * 60000 // 1-minute intervals
    let value = 50.0 + (Random::float() - 0.5) * 10.0 // Random variation ±5%
    let data_point = TimeSeriesDataPoint::new(timestamp, value, "cpu_usage", [("host", "server-01")])
    normal_data = normal_data.push(data_point)
  }
  
  // Inject anomalies
  // Spike anomaly at index 25
  let spike_point = TimeSeriesDataPoint::new(
    base_timestamp + 25 * 60000,
    95.0, // Much higher than normal
    "cpu_usage",
    [("host", "server-01")]
  )
  normal_data = normal_data.update(25, spike_point)
  
  // Dip anomaly at index 50
  let dip_point = TimeSeriesDataPoint::new(
    base_timestamp + 50 * 60000,
    5.0, // Much lower than normal
    "cpu_usage",
    [("host", "server-01")]
  )
  normal_data = normal_data.update(50, dip_point)
  
  // Trend change anomaly at index 75
  for i in 75..=99 {
    let timestamp = base_timestamp + i * 60000
    let value = 80.0 + (i - 75).to_float() * 0.5 // Gradual increase
    let trend_point = TimeSeriesDataPoint::new(timestamp, value, "cpu_usage", [("host", "server-01")])
    normal_data = normal_data.update(i, trend_point)
  }
  
  // Test statistical anomaly detection (z-score method)
  let zscore_detector = AnomalyDetector::zscore(2.5) // 2.5 standard deviations
  let zscore_anomalies = zscore_detector.detect(normal_data)
  
  assert_eq(zscore_anomalies.length(), 3) // Should detect spike, dip, and trend change
  
  // Verify spike anomaly detection
  let spike_anomaly = zscore_anomalies.find(fn(a) { a.timestamp == base_timestamp + 25 * 60000 })
  match spike_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.timestamp, base_timestamp + 25 * 60000)
      assert_eq(anomaly.value, 95.0)
      assert_eq(anomaly.anomaly_type, Spike)
      assert_true(anomaly.score > 2.5)
    }
    None => assert_true(false)
  }
  
  // Verify dip anomaly detection
  let dip_anomaly = zscore_anomalies.find(fn(a) { a.timestamp == base_timestamp + 50 * 60000 })
  match dip_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.timestamp, base_timestamp + 50 * 60000)
      assert_eq(anomaly.value, 5.0)
      assert_eq(anomaly.anomaly_type, Dip)
      assert_true(anomaly.score > 2.5)
    }
    None => assert_true(false)
  }
  
  // Test moving average anomaly detection
  let ma_detector = AnomalyDetector::moving_average(10, 3.0) // 10-point window, 3.0 threshold
  let ma_anomalies = ma_detector.detect(normal_data)
  
  assert_true(ma_anomalies.length() >= 2) // Should detect spike and dip
  
  // Test seasonal anomaly detection
  let seasonal_data = []
  
  // Create data with daily pattern
  for day in 0..=6 { // 7 days
    for hour in 0..=23 { // 24 hours
      let timestamp = base_timestamp + day * 86400000 + hour * 3600000
      // Pattern: lower at night (0-6), higher during day (9-17), peak at noon
      let base_value = match hour {
        h if h < 6 => 20.0
        h if h < 9 => 40.0
        h if h < 12 => 60.0 + (h - 9).to_float() * 10.0
        h if h < 17 => 90.0 - (hour - 12).to_float() * 10.0
        h if h < 21 => 40.0
        _ => 20.0
      }
      let value = base_value + (Random::float() - 0.5) * 5.0
      let data_point = TimeSeriesDataPoint::new(timestamp, value, "cpu_usage", [("host", "server-01")])
      seasonal_data = seasonal_data.push(data_point)
    }
  }
  
  // Inject anomaly: unusually high CPU at 3 AM on day 3
  let anomaly_timestamp = base_timestamp + 3 * 86400000 + 3 * 3600000
  let anomaly_point = TimeSeriesDataPoint::new(anomaly_timestamp, 85.0, "cpu_usage", [("host", "server-01")])
  let anomaly_index = 3 * 24 + 3
  seasonal_data = seasonal_data.update(anomaly_index, anomaly_point)
  
  // Test seasonal anomaly detection
  let seasonal_detector = AnomalyDetector::seasonal(24, 2.5) // 24-hour seasonality, 2.5 threshold
  let seasonal_anomalies = seasonal_detector.detect(seasonal_data)
  
  assert_true(seasonal_anomalies.length() >= 1)
  
  // Verify seasonal anomaly detection
  let seasonal_anomaly = seasonal_anomalies.find(fn(a) { a.timestamp == anomaly_timestamp })
  match seasonal_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.timestamp, anomaly_timestamp)
      assert_eq(anomaly.value, 85.0)
      assert_eq(anomaly.anomaly_type, Seasonal)
      assert_true(anomaly.score > 2.5)
    }
    None => assert_true(false)
  }
}

// Test 6: Time Series Data Forecasting
test "time series data forecasting" {
  // Create time series data for forecasting
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  let historical_data = []
  
  // Create data with trend and seasonality
  for day in 0..=29 { // 30 days of historical data
    for hour in 0..=23 { // 24 hours per day
      let timestamp = base_timestamp + day * 86400000 + hour * 3600000
      
      // Trend: increasing by 0.1 each day
      let trend = day.to_float() * 0.1
      
      // Seasonality: daily pattern
      let seasonal = match hour {
        h if h < 6 => 20.0
        h if h < 9 => 40.0
        h if h < 12 => 60.0 + (h - 9).to_float() * 10.0
        h if h < 17 => 90.0 - (hour - 12).to_float() * 10.0
        h if h < 21 => 40.0
        _ => 20.0
      }
      
      // Random noise
      let noise = (Random::float() - 0.5) * 5.0
      
      let value = 50.0 + trend + seasonal + noise
      let data_point = TimeSeriesDataPoint::new(timestamp, value, "cpu_usage", [("host", "server-01")])
      historical_data = historical_data.push(data_point)
    }
  }
  
  // Test linear trend forecasting
  let linear_forecaster = TimeSeriesForecaster::linear_trend(24) // Forecast next 24 hours
  let linear_forecast = linear_forecaster.forecast(historical_data)
  
  assert_eq(linear_forecast.length(), 24) // 24 hours forecast
  
  // Verify forecast timestamps
  let last_historical_timestamp = base_timestamp + 30 * 86400000 - 3600000
  for i in 0..=23 {
    let expected_timestamp = last_historical_timestamp + (i + 1) * 3600000
    assert_eq(TimeSeriesDataPoint::timestamp(linear_forecast[i]), expected_timestamp)
  }
  
  // Test moving average forecasting
  let ma_forecaster = TimeSeriesForecaster::moving_average(168, 24) // Use 7 days (168 hours) of history
  let ma_forecast = ma_forecaster.forecast(historical_data)
  
  assert_eq(ma_forecast.length(), 24)
  
  // Test seasonal forecasting
  let seasonal_forecaster = TimeSeriesForecaster::seasonal_naive(24, 24) // 24-hour seasonality
  let seasonal_forecast = seasonal_forecaster.forecast(historical_data)
  
  assert_eq(seasonal_forecast.length(), 24)
  
  // Verify seasonal forecast (should repeat pattern from 24 hours ago)
  for i in 0..=23 {
    let forecast_value = TimeSeriesDataPoint::value(seasonal_forecast[i])
    let historical_index = historical_data.length() - 24 + i
    let historical_value = TimeSeriesDataPoint::value(historical_data[historical_index])
    assert_eq(forecast_value, historical_value)
  }
  
  // Test ARIMA forecasting
  let arima_forecaster = TimeSeriesForecaster::arima(1, 1, 1, 24) // ARIMA(1,1,1) model
  let arima_forecast = arima_forecaster.forecast(historical_data)
  
  assert_eq(arima_forecast.length(), 24)
  
  // Test forecast evaluation
  let train_data = historical_data.slice(0, historical_data.length() - 24) // All but last 24 hours
  let test_data = historical_data.slice(historical_data.length() - 24, 24) // Last 24 hours
  
  // Generate forecast for test period
  let evaluation_forecast = arima_forecaster.forecast(train_data)
  
  // Calculate forecast accuracy metrics
  let mae = TimeSeriesForecaster::calculate_mae(evaluation_forecast, test_data)
  let mse = TimeSeriesForecaster::calculate_mse(evaluation_forecast, test_data)
  let mape = TimeSeriesForecaster::calculate_mape(evaluation_forecast, test_data)
  
  assert_true(mae > 0.0)
  assert_true(mse > 0.0)
  assert_true(mape > 0.0)
  
  // Verify MAPE is reasonable (should be less than 50% for this example)
  assert_true(mape < 50.0)
}

// Test 7: Time Series Data Compression
test "time series data compression" {
  // Create high-frequency time series data for compression
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  let detailed_data = []
  
  // Create data with slowly changing values (good for compression)
  for i in 0..=1439 { // 24 hours at 1-minute intervals
    let timestamp = base_timestamp + i * 60000 // 1 minute in ms
    
    // Create a slowly changing sine wave with small variations
    let base_value = 50.0 + 10.0 * Float::sin(i.to_float() * 0.1)
    let value = base_value + (Random::float() - 0.5) * 2.0 // Small random variations
    let data_point = TimeSeriesDataPoint::new(timestamp, value, "cpu_usage", [("host", "server-01")])
    detailed_data = detailed_data.push(data_point)
  }
  
  // Test delta compression
  let delta_compressor = TimeSeriesCompressor::delta()
  let delta_compressed = delta_compressor.compress(detailed_data)
  let delta_decompressed = delta_compressor.decompress(delta_compressed)
  
  // Verify compression ratio
  let original_size = TimeSeriesCompressor::calculate_size(detailed_data)
  let compressed_size = TimeSeriesCompressor::calculate_compressed_size(delta_compressed)
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  assert_true(compression_ratio < 1.0) // Should be smaller
  assert_eq(detailed_data.length(), delta_decompressed.length()) // Should have same number of points
  
  // Verify data integrity after decompression
  for i in 0..=detailed_data.length() - 1 {
    let original = detailed_data[i]
    let decompressed = delta_decompressed[i]
    
    assert_eq(TimeSeriesDataPoint::timestamp(original), TimeSeriesDataPoint::timestamp(decompressed))
    assert_eq(TimeSeriesDataPoint::metric_name(original), TimeSeriesDataPoint::metric_name(decompressed))
    assert_eq(TimeSeriesDataPoint::tags(original), TimeSeriesDataPoint::tags(decompressed))
    assert_eq(TimeSeriesDataPoint::value(original), TimeSeriesDataPoint::value(decompressed))
  }
  
  // Test swing compression (for slowly changing values)
  let swing_compressor = TimeSeriesCompressor::swing(0.5) // 0.5 threshold
  let swing_compressed = swing_compressor.compress(detailed_data)
  let swing_decompressed = swing_compressor.decompress(swing_compressed)
  
  // Swing compression should reduce number of points
  assert_true(swing_compressed.points.length() < detailed_data.length())
  
  // Verify decompressed values are within threshold of original
  for i in 0..=detailed_data.length() - 1 {
    let original_value = TimeSeriesDataPoint::value(detailed_data[i])
    let decompressed_value = TimeSeriesDataPoint::value(swing_decompressed[i])
    let difference = Float::abs(original_value - decompressed_value)
    assert_true(difference <= 0.5) // Should be within threshold
  }
  
  // Test time bucket compression (aggregating into time buckets)
  let bucket_compressor = TimeSeriesCompressor::time_bucket(300000, Average) // 5-minute buckets
  let bucket_compressed = bucket_compressor.compress(detailed_data)
  let bucket_decompressed = bucket_compressor.decompress(bucket_compressed)
  
  // Should have 1/5 the number of points (5-minute buckets from 1-minute data)
  assert_eq(bucket_compressed.points.length(), 288) // 1440 / 5 = 288
  
  // Test Gorilla compression (for float values)
  let gorilla_compressor = TimeSeriesCompressor::gorilla()
  let gorilla_compressed = gorilla_compressor.compress(detailed_data)
  let gorilla_decompressed = gorilla_compressor.decompress(gorilla_compressed)
  
  // Gorilla compression should have good compression ratio for float data
  let gorilla_compressed_size = TimeSeriesCompressor::calculate_compressed_size(gorilla_compressed)
  let gorilla_compression_ratio = gorilla_compressed_size.to_float() / original_size.to_float()
  
  assert_true(gorilla_compression_ratio < 0.5) // Should be at least 50% compression
  
  // Test adaptive compression (automatically chooses best method)
  let adaptive_compressor = TimeSeriesCompressor::adaptive()
  let adaptive_compressed = adaptive_compressor.compress(detailed_data)
  let adaptive_decompressed = adaptive_compressor.decompress(adaptive_compressed)
  
  // Verify adaptive compression chose a method
  assert_true(adaptive_compressed.method != "")
  
  // Test compression statistics
  let compression_stats = TimeSeriesCompressor::analyze_compression(detailed_data)
  
  assert_true(compression_stats.original_size > 0)
  assert_true(compression_stats.delta_compression_ratio > 0.0)
  assert_true(compression_stats.swing_compression_ratio > 0.0)
  assert_true(compression_stats.gorilla_compression_ratio > 0.0)
  assert_true(compression_stats.best_method != "")
}