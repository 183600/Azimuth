// Azimuth Telemetry System - Time Series Data Processing Tests
// This file contains comprehensive test cases for time series data processing

// Test 1: Basic Time Series Operations
test "basic time series operations" {
  // Test time series creation
  let ts = TimeSeries::new("cpu.usage")
  
  // Test adding data points
  TimeSeries::add_point(ts, 1609459200000, 25.5)  // 2021-01-01 00:00:00 UTC
  TimeSeries::add_point(ts, 1609459260000, 30.2)  // 2021-01-01 00:01:00 UTC
  TimeSeries::add_point(ts, 1609459320000, 27.8)  // 2021-01-01 00:02:00 UTC
  TimeSeries::add_point(ts, 1609459380000, 32.1)  // 2021-01-01 00:03:00 UTC
  TimeSeries::add_point(ts, 1609459440000, 29.6)  // 2021-01-01 00:04:00 UTC
  
  // Test time series properties
  assert_eq(TimeSeries::name(ts), "cpu.usage")
  assert_eq(TimeSeries::size(ts), 5)
  assert_eq(TimeSeries::start_time(ts), 1609459200000)
  assert_eq(TimeSeries::end_time(ts), 1609459440000)
  
  // Test getting data points
  let points = TimeSeries::get_points(ts)
  assert_eq(points.length(), 5)
  assert_eq(points[0].timestamp, 1609459200000)
  assert_eq(points[0].value, 25.5)
  assert_eq(points[4].timestamp, 1609459440000)
  assert_eq(points[4].value, 29.6)
  
  // Test getting value at specific time
  let value = TimeSeries::get_value_at(ts, 1609459320000)
  match value {
    Some(v) => assert_eq(v, 27.8),
    None => assert_true(false)
  }
  
  // Test getting value at non-existent time
  let non_existent = TimeSeries::get_value_at(ts, 1609459500000)
  match non_existent {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
  
  // Test getting values in time range
  let range_values = TimeSeries::get_values_in_range(ts, 1609459260000, 1609459380000)
  assert_eq(range_values.length(), 3)
  assert_eq(range_values[0].value, 30.2)
  assert_eq(range_values[1].value, 27.8)
  assert_eq(range_values[2].value, 32.1)
  
  // Test clearing time series
  TimeSeries::clear(ts)
  assert_eq(TimeSeries::size(ts), 0)
}

// Test 2: Time Series Aggregation
test "time series aggregation" {
  // Create time series with minute-level data
  let ts = TimeSeries::new("response.time")
  
  // Add data points for each minute of an hour
  for i in 0..60 {
    let timestamp = 1609459200000 + (i * 60000)  // Each minute
    let value = 100.0 + (i % 10).to_float() * 10.0  // Values from 100 to 190
    TimeSeries::add_point(ts, timestamp, value)
  }
  
  // Test sum aggregation
  let sum_5min = TimeSeries::aggregate(ts, Sum, 5 * 60000)  // 5-minute windows
  assert_eq(sum_5min.size(), 12)  // 60 minutes / 5 minutes = 12 windows
  
  // Test average aggregation
  let avg_5min = TimeSeries::aggregate(ts, Average, 5 * 60000)  // 5-minute windows
  assert_eq(avg_5min.size(), 12)
  
  // Verify first window average
  let first_window = avg_5min.get_points()[0]
  assert_eq(first_window.value, 145.0)  // Average of 100, 110, 120, 130, 140
  
  // Test min aggregation
  let min_5min = TimeSeries::aggregate(ts, Min, 5 * 60000)  // 5-minute windows
  assert_eq(min_5min.size(), 12)
  
  // Verify first window min
  let first_min = min_5min.get_points()[0]
  assert_eq(first_min.value, 100.0)  // Min of 100, 110, 120, 130, 140
  
  // Test max aggregation
  let max_5min = TimeSeries::aggregate(ts, Max, 5 * 60000)  // 5-minute windows
  assert_eq(max_5min.size(), 12)
  
  // Verify first window max
  let first_max = max_5min.get_points()[0]
  assert_eq(first_max.value, 140.0)  // Max of 100, 110, 120, 130, 140
  
  // Test count aggregation
  let count_5min = TimeSeries::aggregate(ts, Count, 5 * 60000)  // 5-minute windows
  assert_eq(count_5min.size(), 12)
  
  // Verify first window count
  let first_count = count_5min.get_points()[0]
  assert_eq(first_count.value, 5.0)  // Count of 5 points
  
  // Test percentile aggregation
  let p95_5min = TimeSeries::aggregate(ts, Percentile(95), 5 * 60000)  // 5-minute windows
  assert_eq(p95_5min.size(), 12)
}

// Test 3: Time Series Resampling
test "time series resampling" {
  // Create time series with second-level data
  let ts = TimeSeries::new("network.throughput")
  
  // Add data points for each second of a minute
  for i in 0..60 {
    let timestamp = 1609459200000 + (i * 1000)  // Each second
    let value = 1000.0 + (i % 10).to_float() * 100.0  // Values from 1000 to 1900
    TimeSeries::add_point(ts, timestamp, value)
  }
  
  // Test upsampling (from seconds to milliseconds)
  let upsampled = TimeSeries::resample(ts, 100)  // 100ms intervals
  assert_eq(upsampled.size(), 600)  // 60 seconds * 10 points per second
  
  // Test downsampling (from seconds to minutes)
  let downsampled = TimeSeries::resample(ts, 60000)  // 1-minute intervals
  assert_eq(downsampled.size(), 1)  // 60 seconds / 60 seconds = 1 minute
  
  // Verify downsampled value (average)
  let downsampled_point = downsampled.get_points()[0]
  assert_eq(downsampled_point.value, 1450.0)  // Average of all values
  
  // Test resampling with different interpolation methods
  let linear_upsampled = TimeSeries::resample_with_interpolation(ts, 500, Linear)  // 500ms intervals
  assert_eq(linear_upsampled.size(), 120)  // 60 seconds * 2 points per second
  
  let step_upsampled = TimeSeries::resample_with_interpolation(ts, 500, Step)  // 500ms intervals
  assert_eq(step_upsampled.size(), 120)  // 60 seconds * 2 points per second
}

// Test 4: Time Series Windowing
test "time series windowing" {
  // Create time series with hourly data
  let ts = TimeSeries::new("temperature")
  
  // Add data points for each hour of a day
  for i in 0..24 {
    let timestamp = 1609459200000 + (i * 3600000)  // Each hour
    let value = 20.0 + (i - 12).to_float().abs() * 0.5  // Peak at noon
    TimeSeries::add_point(ts, timestamp, value)
  }
  
  // Test tumbling window
  let tumbling_windows = TimeSeries::tumbling_window(ts, 6 * 3600000)  // 6-hour windows
  assert_eq(tumbling_windows.length(), 4)  // 24 hours / 6 hours = 4 windows
  
  // Verify first window (midnight to 6am)
  let first_window = tumbling_windows[0]
  assert_eq(first_window.size(), 6)
  assert_eq(first_window.start_time(), 1609459200000)
  assert_eq(first_window.end_time(), 1609480800000)
  
  // Test sliding window
  let sliding_windows = TimeSeries::sliding_window(ts, 6 * 3600000, 3 * 3600000)  // 6-hour window, 3-hour slide
  assert_eq(sliding_windows.length(), 7)  // (24-6)/3 + 1 = 7 windows
  
  // Verify first window (midnight to 6am)
  let first_sliding = sliding_windows[0]
  assert_eq(first_sliding.size(), 6)
  assert_eq(first_sliding.start_time(), 1609459200000)
  assert_eq(first_sliding.end_time(), 1609480800000)
  
  // Verify second window (3am to 9am)
  let second_sliding = sliding_windows[1]
  assert_eq(second_sliding.size(), 6)
  assert_eq(second_sliding.start_time(), 1609470000000)
  assert_eq(second_sliding.end_time(), 1609495200000)
  
  // Test session window (gap-based)
  let session_ts = TimeSeries::new("user.activity")
  
  // Add data points with gaps
  TimeSeries::add_point(session_ts, 1609459200000, 1.0)  // 00:00
  TimeSeries::add_point(session_ts, 1609459260000, 1.0)  // 00:01
  TimeSeries::add_point(session_ts, 1609459320000, 1.0)  // 00:02
  TimeSeries::add_point(session_ts, 1609459800000, 1.0)  // 00:10 (8-minute gap)
  TimeSeries::add_point(session_ts, 1609459860000, 1.0)  // 00:11
  TimeSeries::add_point(session_ts, 1609460400000, 1.0)  // 00:20 (9-minute gap)
  
  // Session windows with 5-minute gap threshold
  let session_windows = TimeSeries::session_window(session_ts, 5 * 60000)
  assert_eq(session_windows.length(), 3)  // 3 sessions
  
  // Verify first session (00:00 to 00:02)
  let first_session = session_windows[0]
  assert_eq(first_session.size(), 3)
  
  // Verify second session (00:10 to 00:11)
  let second_session = session_windows[1]
  assert_eq(second_session.size(), 2)
  
  // Verify third session (00:20)
  let third_session = session_windows[2]
  assert_eq(third_session.size(), 1)
}

// Test 5: Time Series Analytics
test "time series analytics" {
  // Create time series with test data
  let ts = TimeSeries::new("test.metric")
  
  // Add data points with a trend
  for i in 0..100 {
    let timestamp = 1609459200000 + (i * 60000)  // Each minute
    let value = 100.0 + i.to_float() * 0.5 + (i % 10).to_float() * 2.0  // Trend + noise
    TimeSeries::add_point(ts, timestamp, value)
  }
  
  // Test trend analysis
  let trend = TimeSeries::calculate_trend(ts)
  assert_true(trend > 0.4 && trend < 0.6)  // Should be around 0.5
  
  // Test seasonality detection
  let seasonality = TimeSeries::detect_seasonality(ts)
  assert_true(seasonality.period > 0)  // Should detect some seasonality
  
  // Test moving average
  let ma_10 = TimeSeries::moving_average(ts, 10)
  assert_eq(ma_10.size(), 91)  // 100 - 10 + 1
  
  // Verify first moving average
  let first_ma = ma_10.get_points()[0]
  let expected_ma = (100.0 + 100.5 + 101.0 + 101.5 + 102.0 + 102.5 + 103.0 + 103.5 + 104.0 + 104.5) / 10.0
  assert_true((first_ma.value - expected_ma).abs() < 0.001)
  
  // Test exponential moving average
  let ema_10 = TimeSeries::exponential_moving_average(ts, 10, 0.2)
  assert_eq(ema_10.size(), 100)
  
  // Test rate of change
  let roc = TimeSeries::rate_of_change(ts, 1)
  assert_eq(roc.size(), 99)  // 100 - 1
  
  // Test volatility
  let volatility = TimeSeries::calculate_volatility(ts, 10)
  assert_eq(volatility.size(), 91)  // 100 - 10 + 1
  
  // Test correlation with another time series
  let ts2 = TimeSeries::new("test.metric2")
  for i in 0..100 {
    let timestamp = 1609459200000 + (i * 60000)
    let value = 50.0 + i.to_float() * 0.3 + (i % 10).to_float() * 1.5  // Similar trend
    TimeSeries::add_point(ts2, timestamp, value)
  }
  
  let correlation = TimeSeries::calculate_correlation(ts, ts2)
  assert_true(correlation > 0.8)  // Should be highly correlated
}

// Test 6: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Create time series with normal data and some anomalies
  let ts = TimeSeries::new("system.metrics")
  
  // Add normal data points
  for i in 0..50 {
    let timestamp = 1609459200000 + (i * 60000)
    let value = 100.0 + (i % 10).to_float() * 2.0  // Values from 100 to 118
    TimeSeries::add_point(ts, timestamp, value)
  }
  
  // Add anomalies
  TimeSeries::add_point(ts, 1609459200000 + 50 * 60000, 200.0)  // Spike
  TimeSeries::add_point(ts, 1609459200000 + 51 * 60000, 105.0)  // Normal
  TimeSeries::add_point(ts, 1609459200000 + 52 * 60000, 5.0)    // Dip
  TimeSeries::add_point(ts, 1609459200000 + 53 * 60000, 110.0)  // Normal
  TimeSeries::add_point(ts, 1609459200000 + 54 * 60000, 0.0)    // Zero
  TimeSeries::add_point(ts, 1609459200000 + 55 * 60000, 108.0)  // Normal
  
  // Test statistical anomaly detection (z-score)
  let zscore_anomalies = TimeSeries::detect_anomalies_zscore(ts, 2.0)  // 2 sigma threshold
  assert_eq(zscore_anomalies.length(), 3)  // Should detect spike, dip, and zero
  
  // Verify spike anomaly
  let spike_anomaly = zscore_anomalies[0]
  assert_eq(spike_anomaly.timestamp, 1609459200000 + 50 * 60000)
  assert_eq(spike_anomaly.value, 200.0)
  assert_true(spike_anomaly.score > 2.0)
  
  // Test IQR-based anomaly detection
  let iqr_anomalies = TimeSeries::detect_anomalies_iqr(ts, 1.5)  // 1.5 IQR multiplier
  assert_eq(iqr_anomalies.length(), 3)  // Should detect same anomalies
  
  // Test isolation forest anomaly detection
  let isolation_anomalies = TimeSeries::detect_anomalies_isolation_forest(ts, 0.1)  // 10% contamination
  assert_eq(isolation_anomalies.length(), 3)  // Should detect same anomalies
  
  // Test moving average-based anomaly detection
  let ma_anomalies = TimeSeries::detect_anomalies_moving_average(ts, 5, 3.0)  // 5-point MA, 3 sigma
  assert_eq(ma_anomalies.length(), 3)  // Should detect same anomalies
}

// Test 7: Time Series Forecasting
test "time series forecasting" {
  // Create time series with trend and seasonality
  let ts = TimeSeries::new("sales.data")
  
  // Add data points for 30 days
  for i in 0..30 {
    let timestamp = 1609459200000 + (i * 86400000)  // Each day
    let trend = 100.0 + i.to_float() * 2.0  // Linear trend
    let seasonality = 20.0 * (2 * 3.14159 * i.to_float() / 7.0).sin()  // Weekly seasonality
    let noise = (random_int(-5, 5)).to_float()  // Random noise
    let value = trend + seasonality + noise
    TimeSeries::add_point(ts, timestamp, value)
  }
  
  // Test linear regression forecasting
  let linear_forecast = TimeSeries::forecast_linear_regression(ts, 7)  // 7 days ahead
  assert_eq(linear_forecast.size(), 7)
  
  // Verify first forecast point
  let first_forecast = linear_forecast.get_points()[0]
  assert_eq(first_forecast.timestamp, 1609459200000 + 30 * 86400000)  // Day 31
  assert_true(first_forecast.value > 150.0)  // Should be higher than last actual value
  
  // Test moving average forecasting
  let ma_forecast = TimeSeries::forecast_moving_average(ts, 7, 7)  // 7 days ahead, 7-day MA
  assert_eq(ma_forecast.size(), 7)
  
  // Test exponential smoothing forecasting
  let es_forecast = TimeSeries::forecast_exponential_smoothing(ts, 7, 0.3)  // 7 days ahead, alpha=0.3
  assert_eq(es_forecast.size(), 7)
  
  // Test ARIMA forecasting
  let arima_forecast = TimeSeries::forecast_arima(ts, 7, 1, 1, 1)  // 7 days ahead, ARIMA(1,1,1)
  assert_eq(arima_forecast.size(), 7)
  
  // Test Prophet-like forecasting (trend + seasonality)
  let prophet_forecast = TimeSeries::forecast_prophet(ts, 7)  // 7 days ahead
  assert_eq(prophet_forecast.size(), 7)
  
  // Verify Prophet forecast captures seasonality
  let prophet_point1 = prophet_forecast.get_points()[0]
  let prophet_point8 = prophet_forecast.get_points()[7 - 1]  // 7 days later
  let seasonality_diff = prophet_point8.value - prophet_point1.value
  assert_true(seasonality_diff.abs() > 10.0)  // Should show weekly seasonality
}

// Test 8: Time Series Compression
test "time series compression" {
  // Create time series with many data points
  let ts = TimeSeries::new("high.frequency.data")
  
  // Add data points for each second of an hour
  for i in 0..3600 {
    let timestamp = 1609459200000 + (i * 1000)  // Each second
    let value = 100.0 + (i % 60).to_float() * 0.5  // Minute-level pattern
    TimeSeries::add_point(ts, timestamp, value)
  }
  
  // Test delta encoding compression
  let delta_compressed = TimeSeries::compress_delta(ts)
  assert_true(delta_compressed.size() < ts.size())  // Should be smaller
  
  // Test delta decompression
  let delta_decompressed = TimeSeries::decompress_delta(delta_compressed)
  assert_eq(delta_decompressed.size(), ts.size())
  
  // Verify decompressed data matches original
  for i in 0..ts.size() {
    let original_point = ts.get_points()[i]
    let decompressed_point = delta_decompressed.get_points()[i]
    assert_eq(original_point.timestamp, decompressed_point.timestamp)
    assert_eq(original_point.value, decompressed_point.value)
  }
  
  // Test Gorilla compression (float compression)
  let gorilla_compressed = TimeSeries::compress_gorilla(ts)
  assert_true(gorilla_compressed.size() < ts.size())  // Should be smaller
  
  // Test Gorilla decompression
  let gorilla_decompressed = TimeSeries::decompress_gorilla(gorilla_compressed)
  assert_eq(gorilla_decompressed.size(), ts.size())
  
  // Verify decompressed data matches original
  for i in 0..ts.size() {
    let original_point = ts.get_points()[i]
    let decompressed_point = gorilla_decompressed.get_points()[i]
    assert_eq(original_point.timestamp, decompressed_point.timestamp)
    assert_eq(original_point.value, decompressed_point.value)
  }
  
  // Test down-sampling compression
  let downsampled_compressed = TimeSeries::compress_downsample(ts, 60000)  // Downsample to 1-minute intervals
  assert_eq(downsampled_compressed.size(), 60)  // 3600 seconds / 60 seconds = 60 points
  
  // Test swing door compression (tolerance-based)
  let swing_compressed = TimeSeries::compress_swing_door(ts, 0.1)  // 0.1 tolerance
  assert_true(swing_compressed.size() < ts.size())  // Should be smaller
  
  // Test swing door decompression
  let swing_decompressed = TimeSeries::decompress_swing_door(swing_compressed)
  assert_eq(swing_decompressed.size(), ts.size())
  
  // Verify decompressed data is within tolerance
  for i in 0..ts.size() {
    let original_point = ts.get_points()[i]
    let decompressed_point = swing_decompressed.get_points()[i]
    assert_eq(original_point.timestamp, decompressed_point.timestamp)
    assert_true((original_point.value - decompressed_point.value).abs() <= 0.1)
  }
}

// Test 9: Time Series Storage and Retrieval
test "time series storage and retrieval" {
  // Test in-memory storage
  let memory_store = TimeSeriesStore::new_memory()
  
  // Create and store time series
  let ts1 = TimeSeries::new("metric1")
  TimeSeries::add_point(ts1, 1609459200000, 25.5)
  TimeSeries::add_point(ts1, 1609459260000, 30.2)
  
  let ts2 = TimeSeries::new("metric2")
  TimeSeries::add_point(ts2, 1609459200000, 100.0)
  TimeSeries::add_point(ts2, 1609459260000, 105.0)
  
  TimeSeriesStore::store(memory_store, ts1)
  TimeSeriesStore::store(memory_store, ts2)
  
  // Test retrieval
  let retrieved_ts1 = TimeSeriesStore::retrieve(memory_store, "metric1")
  match retrieved_ts1 {
    Some(ts) => {
      assert_eq(TimeSeries::name(ts), "metric1")
      assert_eq(TimeSeries::size(ts), 2)
      assert_eq(TimeSeries::get_value_at(ts, 1609459200000), Some(25.5))
    }
    None => assert_true(false)
  }
  
  let retrieved_ts2 = TimeSeriesStore::retrieve(memory_store, "metric2")
  match retrieved_ts2 {
    Some(ts) => {
      assert_eq(TimeSeries::name(ts), "metric2")
      assert_eq(TimeSeries::size(ts), 2)
      assert_eq(TimeSeries::get_value_at(ts, 1609459260000), Some(105.0))
    }
    None => assert_true(false)
  }
  
  // Test file-based storage
  let file_store = TimeSeriesStore::new_file("/tmp/timeseries_test.db")
  
  TimeSeriesStore::store(file_store, ts1)
  TimeSeriesStore::store(file_store, ts2)
  
  let file_retrieved_ts1 = TimeSeriesStore::retrieve(file_store, "metric1")
  match file_retrieved_ts1 {
    Some(ts) => {
      assert_eq(TimeSeries::name(ts), "metric1")
      assert_eq(TimeSeries::size(ts), 2)
    }
    None => assert_true(false)
  }
  
  // Test time range query
  let range_results = TimeSeriesStore::query_range(memory_store, "metric1", 1609459200000, 1609459260000)
  assert_eq(range_results.length(), 2)
  
  // Test latest value query
  let latest_value = TimeSeriesStore::get_latest(memory_store, "metric1")
  match latest_value {
    Some(point) => assert_eq(point.value, 30.2),
    None => assert_true(false)
  }
  
  // Test aggregation query
  let aggregated = TimeSeriesStore::aggregate_query(memory_store, "metric1", Average, 60000)
  assert_eq(aggregated.size(), 2)
  
  // Test deletion
  TimeSeriesStore::delete(memory_store, "metric1")
  let deleted_ts = TimeSeriesStore::retrieve(memory_store, "metric1")
  match deleted_ts {
    Some(_) => assert_true(false),
    None => assert_true(true)
  }
}

// Test 10: Comprehensive Time Series Processing Pipeline
test "comprehensive time series processing pipeline" {
  // Create a comprehensive time series processing pipeline
  let pipeline = TimeSeriesPipeline::new()
  
  // Step 1: Data ingestion
  let ingestion_stage = TimeSeriesIngestionStage::new()
  pipeline.add_stage(ingestion_stage)
  
  // Step 2: Data validation
  let validation_stage = TimeSeriesValidationStage::new()
  validation_stage.add_rule(|point| point.value >= 0.0)  // Non-negative values
  validation_stage.add_rule(|point| point.value <= 1000.0)  // Max value
  pipeline.add_stage(validation_stage)
  
  // Step 3: Data cleaning
  let cleaning_stage = TimeSeriesCleaningStage::new()
  cleaning_stage.add_outlier_detector(|points| {
    // Simple IQR-based outlier detection
    let values = points.map(|p| p.value)
    let q1 = percentile(values, 25)
    let q3 = percentile(values, 75)
    let iqr = q3 - q1
    let lower_bound = q1 - 1.5 * iqr
    let upper_bound = q3 + 1.5 * iqr
    
    return points.filter(|p| p.value >= lower_bound && p.value <= upper_bound)
  })
  pipeline.add_stage(cleaning_stage)
  
  // Step 4: Data transformation
  let transformation_stage = TimeSeriesTransformationStage::new()
  transformation_stage.add_transform(|point| {
    // Normalize values to 0-1 range
    return TimeSeriesPoint::new(point.timestamp, point.value / 1000.0)
  })
  pipeline.add_stage(transformation_stage)
  
  // Step 5: Data aggregation
  let aggregation_stage = TimeSeriesAggregationStage::new(Average, 60000)  // 1-minute average
  pipeline.add_stage(aggregation_stage)
  
  // Step 6: Anomaly detection
  let anomaly_stage = TimeSeriesAnomalyDetectionStage::new(ZScore, 2.0)  // 2 sigma
  pipeline.add_stage(anomaly_stage)
  
  // Step 7: Data storage
  let storage_stage = TimeSeriesStorageStage::new(TimeSeriesStore::new_memory())
  pipeline.add_stage(storage_stage)
  
  // Create test data
  let test_data = []
  for i in 0..100 {
    let timestamp = 1609459200000 + (i * 60000)  // Each minute
    let value = 500.0 + (i % 20).to_float() * 10.0  // Values from 500 to 690
    test_data.push(TimeSeriesPoint::new(timestamp, value))
  }
  
  // Add some anomalies
  test_data.push(TimeSeriesPoint::new(1609459200000 + 50 * 60000, 100.0))  // Low outlier
  test_data.push(TimeSeriesPoint::new(1609459200000 + 75 * 60000, 900.0))  // High outlier
  
  // Run pipeline
  let results = pipeline.run("test.metric", test_data)
  
  // Verify results
  assert_true(results.processed_points > 0)
  assert_true(results.anomalies_detected > 0)
  assert_true(results.storage_write_count > 0)
  
  // Get processed time series
  let processed_ts = pipeline.get_output("test.metric")
  match processed_ts {
    Some(ts) => {
      assert_eq(TimeSeries::name(ts), "test.metric")
      assert_true(TimeSeries::size(ts) > 0)
      
      // Verify values are normalized (0-1 range)
      for point in TimeSeries::get_points(ts) {
        assert_true(point.value >= 0.0 && point.value <= 1.0)
      }
    }
    None => assert_true(false)
  }
  
  // Get detected anomalies
  let anomalies = pipeline.get_anomalies("test.metric")
  assert_true(anomalies.length() > 0)
  
  // Verify pipeline statistics
  let stats = pipeline.get_statistics()
  assert_eq(stats.total_processed, test_data.length())
  assert_true(stats.validation_rejected >= 0)
  assert_true(stats.outliers_removed >= 0)
  assert_true(stats.anomalies_detected > 0)
}