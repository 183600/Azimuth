// Azimuth Time Series Data Processing Tests
// This file contains test cases for time series data processing functionality

// Test 1: Time Series Data Point Creation
test "time series data point creation" {
  // Create a simple time series data point
  let timestamp = 1640995200000L  // Jan 1, 2022 00:00:00 UTC
  let value = 42.5
  let data_point = TimeSeriesDataPoint::new(timestamp, value)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point), value)
  
  // Test with metadata
  let metadata = [
    ("source", "sensor-001"),
    ("unit", "celsius"),
    ("location", "data-center-1")
  ]
  let data_point_with_metadata = TimeSeriesDataPoint::with_metadata(timestamp, value, metadata)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point_with_metadata), timestamp)
  assert_eq(TimeSeriesDataPoint::value(data_point_with_metadata), value)
  assert_eq(TimeSeriesDataPoint::get_metadata(data_point_with_metadata, "source"), Some("sensor-001"))
  assert_eq(TimeSeriesDataPoint::get_metadata(data_point_with_metadata, "unit"), Some("celsius"))
  assert_eq(TimeSeriesDataPoint::get_metadata(data_point_with_metadata, "location"), Some("data-center-1"))
}

// Test 2: Time Series Collection Operations
test "time series collection operations" {
  let collection = TimeSeriesCollection::new("cpu-usage")
  
  // Add data points
  let base_timestamp = 1640995200000L
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp, 25.5))
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + 60000L, 30.2))
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + 120000L, 28.7))
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + 180000L, 32.1))
  
  // Test collection properties
  assert_eq(TimeSeriesCollection::name(collection), "cpu-usage")
  assert_eq(TimeSeriesCollection::count(collection), 4)
  
  // Test data point retrieval
  let points = TimeSeriesCollection::get_points(collection)
  assert_eq(points.length(), 4)
  assert_eq(points[0].timestamp, base_timestamp)
  assert_eq(points[0].value, 25.5)
  assert_eq(points[3].timestamp, base_timestamp + 180000L)
  assert_eq(points[3].value, 32.1)
  
  // Test time range query
  let start_time = base_timestamp + 60000L
  let end_time = base_timestamp + 180000L
  let range_points = TimeSeriesCollection::get_points_in_range(collection, start_time, end_time)
  assert_eq(range_points.length(), 3)
}

// Test 3: Time Series Aggregation Operations
test "time series aggregation operations" {
  let collection = TimeSeriesCollection::new("memory-usage")
  
  // Add test data points
  let base_timestamp = 1640995200000L
  let values = [1024.0, 1100.0, 980.0, 1200.0, 1150.0, 1050.0, 1300.0, 1250.0]
  
  for i = 0; i < values.length(); i = i + 1 {
    TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + (i * 60000L), values[i]))
  }
  
  // Test average aggregation
  let avg_result = TimeSeriesAggregator::average(collection)
  match avg_result {
    Some(avg) => assert_eq(avg, 1106.875)
    None => assert_true(false)
  }
  
  // Test min aggregation
  let min_result = TimeSeriesAggregator::min(collection)
  match min_result {
    Some(min) => assert_eq(min, 980.0)
    None => assert_true(false)
  }
  
  // Test max aggregation
  let max_result = TimeSeriesAggregator::max(collection)
  match max_result {
    Some(max) => assert_eq(max, 1300.0)
    None => assert_true(false)
  }
  
  // Test sum aggregation
  let sum_result = TimeSeriesAggregator::sum(collection)
  match sum_result {
    Some(sum) => assert_eq(sum, 8855.0)
    None => assert_true(false)
  }
  
  // Test count aggregation
  let count_result = TimeSeriesAggregator::count(collection)
  assert_eq(count_result, 8)
}

// Test 4: Time Series Resampling Operations
test "time series resampling operations" {
  let collection = TimeSeriesCollection::new("network-throughput")
  
  // Add test data with 1-minute intervals
  let base_timestamp = 1640995200000L
  let throughput_values = [100.0, 120.0, 80.0, 150.0, 110.0, 90.0, 130.0, 140.0]
  
  for i = 0; i < throughput_values.length(); i = i + 1 {
    TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + (i * 60000L), throughput_values[i]))
  }
  
  // Test upsampling (1-minute to 30-seconds)
  let upsampled = TimeSeriesResampler::upsample(collection, 30000L, LinearInterpolation)
  assert_eq(TimeSeriesCollection::count(upsampled), 15)  // 8 points become 15 with linear interpolation
  
  // Test downsampling (1-minute to 5-minutes)
  let downsampled = TimeSeriesResampler::downsample(collection, 300000L, MeanAggregation)
  assert_eq(TimeSeriesCollection::count(downsampled), 2)  // 8 points become 2 with mean aggregation
  
  // Verify downsampled values
  let downsampled_points = TimeSeriesCollection::get_points(downsampled)
  // First 5 minutes: (100 + 120 + 80 + 150 + 110) / 5 = 112
  assert_eq(downsampled_points[0].timestamp, base_timestamp + 240000L)  // 4 minutes in
  assert_eq(downsampled_points[0].value, 112.0)
  
  // Next 3 minutes: (90 + 130 + 140) / 3 = 120
  assert_eq(downsampled_points[1].timestamp, base_timestamp + 420000L)  // 7 minutes in
  assert_eq(downsampled_points[1].value, 120.0)
}

// Test 5: Time Series Anomaly Detection
test "time series anomaly detection" {
  let collection = TimeSeriesCollection::new("response-time")
  
  // Add normal data points
  let base_timestamp = 1640995200000L
  let normal_values = [50.0, 52.0, 48.0, 51.0, 49.0, 53.0, 47.0, 50.0]
  
  for i = 0; i < normal_values.length(); i = i + 1 {
    TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + (i * 60000L), normal_values[i]))
  }
  
  // Add anomaly data points
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + 480000L, 150.0))  // Spike
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + 540000L, 5.0))    // Dip
  TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + 600000L, 51.0))   // Back to normal
  
  // Test anomaly detection with threshold method
  let threshold_detector = AnomalyDetector::threshold(1.5)  // 1.5x standard deviation
  let threshold_anomalies = AnomalyDetector::detect(threshold_detector, collection)
  assert_eq(threshold_anomalies.length(), 2)  // Should detect the spike and dip
  
  // Test anomaly detection with moving average method
  let ma_detector = AnomalyDetector::moving_average(3, 2.0)  // 3-point window, 2.0x deviation
  let ma_anomalies = AnomalyDetector::detect(ma_detector, collection)
  assert_eq(ma_anomalies.length(), 2)  // Should also detect the spike and dip
  
  // Verify anomaly timestamps
  assert_eq(threshold_anomalies[0].timestamp, base_timestamp + 480000L)
  assert_eq(threshold_anomalies[0].value, 150.0)
  assert_eq(threshold_anomalies[1].timestamp, base_timestamp + 540000L)
  assert_eq(threshold_anomalies[1].value, 5.0)
}

// Test 6: Time Series Forecasting
test "time series forecasting" {
  let collection = TimeSeriesCollection::new("user-activity")
  
  // Add historical data with daily pattern
  let base_timestamp = 1640995200000L  // Jan 1, 2022
  let daily_pattern = [100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 350.0, 300.0, 250.0, 200.0, 150.0]  // 12-hour pattern
  
  // Add 3 days of data
  for day = 0; day < 3; day = day + 1 {
    for hour = 0; hour < daily_pattern.length(); hour = hour + 1 {
      let timestamp = base_timestamp + (day * 86400000L) + (hour * 3600000L)
      let value = daily_pattern[hour] + (day * 50.0)  // Slight increase each day
      TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(timestamp, value))
    }
  }
  
  // Test simple moving average forecasting
  let ma_forecaster = TimeSeriesForecaster::moving_average(12)  // 12-hour window
  let ma_forecast = TimeSeriesForecaster::forecast(ma_forecaster, collection, 6)  // Forecast 6 hours ahead
  
  assert_eq(ma_forecast.length(), 6)
  
  // Test exponential smoothing forecasting
  let es_forecaster = TimeSeriesForecaster::exponential_smoothing(0.3)  // Alpha = 0.3
  let es_forecast = TimeSeriesForecaster::forecast(es_forecaster, collection, 6)
  
  assert_eq(es_forecast.length(), 6)
  
  // Verify forecast timestamps are sequential
  for i = 1; i < ma_forecast.length(); i = i + 1 {
    assert_eq(ma_forecast[i].timestamp, ma_forecast[i-1].timestamp + 3600000L)
    assert_eq(es_forecast[i].timestamp, es_forecast[i-1].timestamp + 3600000L)
  }
}

// Test 7: Time Series Compression
test "time series compression" {
  let collection = TimeSeriesCollection::new("sensor-readings")
  
  // Add test data
  let base_timestamp = 1640995200000L
  for i = 0; i < 100; i = i + 1 {
    let value = 20.0 + (i * 0.1)  // Linearly increasing values
    TimeSeriesCollection::add_point(collection, TimeSeriesDataPoint::new(base_timestamp + (i * 60000L), value))
  }
  
  // Test delta encoding compression
  let delta_compressor = TimeSeriesCompressor::delta_encoding()
  let delta_compressed = TimeSeriesCompressor::compress(delta_compressor, collection)
  let delta_decompressed = TimeSeriesCompressor::decompress(delta_compressor, delta_compressed)
  
  assert_eq(TimeSeriesCollection::count(delta_decompressed), 100)
  assert_eq(TimeSeriesCollection::count(collection), TimeSeriesCollection::count(delta_decompressed))
  
  // Verify data integrity after compression/decompression
  let original_points = TimeSeriesCollection::get_points(collection)
  let decompressed_points = TimeSeriesCollection::get_points(delta_decompressed)
  
  for i = 0; i < original_points.length(); i = i + 1 {
    assert_eq(original_points[i].timestamp, decompressed_points[i].timestamp)
    assert_eq(original_points[i].value, decompressed_points[i].value)
  }
  
  // Test swing door compression
  let swing_compressor = TimeSeriesCompressor::swing_door(0.5)  // 0.5 tolerance
  let swing_compressed = TimeSeriesCompressor::compress(swing_compressor, collection)
  
  // Swing door should reduce the number of points
  assert_eq(TimeSeriesCollection::count(swing_compressed) < TimeSeriesCollection::count(collection), true)
}

// Test 8: Time Series Query Operations
test "time series query operations" {
  let collection = TimeSeriesCollection::new("temperature")
  
  // Add test data with different metadata
  let base_timestamp = 1640995200000L
  let sensors = ["sensor-1", "sensor-2", "sensor-3"]
  let locations = ["room-a", "room-b", "room-c"]
  
  for i = 0; i < 30; i = i + 1 {
    let sensor_idx = i % sensors.length()
    let location_idx = i % locations.length()
    let value = 20.0 + (sensor_idx * 2.0) + (location_idx * 1.0) + (i * 0.1)
    
    let metadata = [
      ("sensor", sensors[sensor_idx]),
      ("location", locations[location_idx]),
      ("type", "temperature")
    ]
    
    TimeSeriesCollection::add_point(
      collection, 
      TimeSeriesDataPoint::with_metadata(base_timestamp + (i * 60000L), value, metadata)
    )
  }
  
  // Test query by metadata
  let sensor_query = TimeSeriesQuery::by_metadata("sensor", "sensor-1")
  let sensor_results = TimeSeriesQuery::execute(collection, sensor_query)
  assert_eq(sensor_results.length(), 10)  // Should have 10 points from sensor-1
  
  // Test query by value range
  let range_query = TimeSeriesQuery::by_value_range(22.0, 25.0)
  let range_results = TimeSeriesQuery::execute(collection, range_query)
  assert_eq(range_results.length() > 0, true)
  
  // Test query by time range
  let start_time = base_timestamp + 600000L  // 10 minutes in
  let end_time = base_timestamp + 1800000L   // 30 minutes in
  let time_query = TimeSeriesQuery::by_time_range(start_time, end_time)
  let time_results = TimeSeriesQuery::execute(collection, time_query)
  assert_eq(time_results.length(), 20)  // Should have 20 points in the time range
  
  // Test compound query (metadata + value range)
  let compound_query = TimeSeriesQuery::and([
    TimeSeriesQuery::by_metadata("location", "room-a"),
    TimeSeriesQuery::by_value_range(20.0, 23.0)
  ])
  let compound_results = TimeSeriesQuery::execute(collection, compound_query)
  assert_eq(compound_results.length() > 0, true)
}