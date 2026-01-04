// Advanced Telemetry Data Processing Tests
// This file contains comprehensive test cases for advanced telemetry data processing

// Test 1: Advanced Telemetry Data Aggregation
test "advanced telemetry data aggregation" {
  // Create a telemetry processor
  let processor = TelemetryProcessor::new()
  
  // Create multiple telemetry data points
  let data_points = [
    TelemetryData::new("metric1", 10.5, "ms", Some([("service", "auth")])),
    TelemetryData::new("metric1", 15.2, "ms", Some([("service", "auth")])),
    TelemetryData::new("metric1", 8.7, "ms", Some([("service", "auth")])),
    TelemetryData::new("metric2", 42.0, "count", Some([("service", "api")])),
    TelemetryData::new("metric2", 38.5, "count", Some([("service", "api")])),
    TelemetryData::new("metric3", 100.0, "bytes", Some([("service", "storage")]))
  ]
  
  // Process aggregation
  let aggregated = TelemetryProcessor::aggregate(processor, data_points)
  
  // Verify aggregation results
  assert_eq(TelemetryAggregation::count(aggregated, "metric1"), 3)
  assert_eq(TelemetryAggregation::average(aggregated, "metric1"), 11.47) // (10.5 + 15.2 + 8.7) / 3
  assert_eq(TelemetryAggregation::min(aggregated, "metric1"), 8.7)
  assert_eq(TelemetryAggregation::max(aggregated, "metric1"), 15.2)
  assert_eq(TelemetryAggregation::sum(aggregated, "metric2"), 80.5)
}

// Test 2: Telemetry Data Filtering and Transformation
test "telemetry data filtering and transformation" {
  let processor = TelemetryProcessor::new()
  
  // Create telemetry data with various attributes
  let data_points = [
    TelemetryData::new("latency", 120.0, "ms", Some([("service", "auth"), ("region", "us-east")])),
    TelemetryData::new("latency", 85.0, "ms", Some([("service", "api"), ("region", "us-west")])),
    TelemetryData::new("latency", 200.0, "ms", Some([("service", "db"), ("region", "us-east")])),
    TelemetryData::new("throughput", 1000.0, "req/s", Some([("service", "api"), ("region", "us-west")])),
    TelemetryData::new("error_rate", 0.05, "ratio", Some([("service", "auth"), ("region", "us-east")]))
  ]
  
  // Filter by service
  let auth_service_data = TelemetryProcessor::filter_by_attribute(processor, data_points, "service", "auth")
  assert_eq(auth_service_data.length(), 2)
  
  // Filter by region
  let us_east_data = TelemetryProcessor::filter_by_attribute(processor, data_points, "region", "us-east")
  assert_eq(us_east_data.length(), 3)
  
  // Transform data (convert ms to seconds)
  let transformed = TelemetryProcessor::transform(processor, data_points, fn(data) {
    if TelemetryData::unit(data) == "ms" {
      TelemetryData::with_value(data, TelemetryData::value(data) / 1000.0, "s")
    } else {
      data
    }
  })
  
  // Verify transformation
  let latency_data = TelemetryProcessor::filter_by_name(processor, transformed, "latency")
  for item in latency_data {
    assert_eq(TelemetryData::unit(item), "s")
    assert_true(TelemetryData::value(item) < 1.0) // All values should be less than 1 second
  }
}

// Test 3: Time Series Telemetry Data Analysis
test "time series telemetry data analysis" {
  let processor = TelemetryProcessor::new()
  
  // Create time series data
  let base_time = 1640995200L // January 1, 2022 00:00:00 UTC
  let time_series_data = [
    TelemetryData::with_timestamp("cpu_usage", 45.2, "percent", base_time, Some([("host", "server1")])),
    TelemetryData::with_timestamp("cpu_usage", 52.8, "percent", base_time + 60L, Some([("host", "server1")])),
    TelemetryData::with_timestamp("cpu_usage", 48.1, "percent", base_time + 120L, Some([("host", "server1")])),
    TelemetryData::with_timestamp("cpu_usage", 61.3, "percent", base_time + 180L, Some([("host", "server1")])),
    TelemetryData::with_timestamp("cpu_usage", 58.9, "percent", base_time + 240L, Some([("host", "server1")])),
    TelemetryData::with_timestamp("memory_usage", 1024.0, "MB", base_time, Some([("host", "server1")])),
    TelemetryData::with_timestamp("memory_usage", 1089.5, "MB", base_time + 60L, Some([("host", "server1")])),
    TelemetryData::with_timestamp("memory_usage", 1156.2, "MB", base_time + 120L, Some([("host", "server1")]))
  ]
  
  // Analyze trends
  let cpu_trend = TelemetryProcessor::analyze_trend(processor, 
    TelemetryProcessor::filter_by_name(processor, time_series_data, "cpu_usage"))
  
  match cpu_trend {
    Trend::Increasing => assert_true(true)
    _ => assert_true(false)
  }
  
  // Calculate moving average
  let cpu_moving_avg = TelemetryProcessor::moving_average(processor,
    TelemetryProcessor::filter_by_name(processor, time_series_data, "cpu_usage"), 3)
  
  assert_eq(cpu_moving_avg.length(), 3) // 5 points with window size 3 = 3 results
  assert_eq(cpu_moving_avg[0], 48.7) // (45.2 + 52.8 + 48.1) / 3
  assert_eq(cpu_moving_avg[1], 54.07) // (52.8 + 48.1 + 61.3) / 3
  assert_eq(cpu_moving_avg[2], 56.1) // (48.1 + 61.3 + 58.9) / 3
  
  // Detect anomalies
  let anomalies = TelemetryProcessor::detect_anomalies(processor, time_series_data, 2.0)
  assert_true(anomalies.length() > 0)
}

// Test 4: Telemetry Data Correlation Analysis
test "telemetry data correlation analysis" {
  let processor = TelemetryProcessor::new()
  
  // Create correlated data points
  let base_time = 1640995200L
  let correlated_data = [
    // Response time increases with request rate
    TelemetryData::with_timestamp("response_time", 100.0, "ms", base_time, Some([("endpoint", "/api/users")])),
    TelemetryData::with_timestamp("request_rate", 10.0, "req/s", base_time, Some([("endpoint", "/api/users")])),
    TelemetryData::with_timestamp("response_time", 150.0, "ms", base_time + 60L, Some([("endpoint", "/api/users")])),
    TelemetryData::with_timestamp("request_rate", 20.0, "req/s", base_time + 60L, Some([("endpoint", "/api/users")])),
    TelemetryData::with_timestamp("response_time", 250.0, "ms", base_time + 120L, Some([("endpoint", "/api/users")])),
    TelemetryData::with_timestamp("request_rate", 40.0, "req/s", base_time + 120L, Some([("endpoint", "/api/users")])),
    TelemetryData::with_timestamp("response_time", 400.0, "ms", base_time + 180L, Some([("endpoint", "/api/users")])),
    TelemetryData::with_timestamp("request_rate", 80.0, "req/s", base_time + 180L, Some([("endpoint", "/api/users")]))
  ]
  
  // Calculate correlation between response time and request rate
  let correlation = TelemetryProcessor::calculate_correlation(processor,
    TelemetryProcessor::filter_by_name(processor, correlated_data, "response_time"),
    TelemetryProcessor::filter_by_name(processor, correlated_data, "request_rate"))
  
  // Should be positive correlation (as request rate increases, response time increases)
  assert_true(correlation > 0.7)
  
  // Find correlated metrics
  let correlated_metrics = TelemetryProcessor::find_correlated_metrics(processor, correlated_data, 0.5)
  assert_true(correlated_metrics.length() >= 1)
}

// Test 5: Telemetry Data Downsampling and Upsampling
test "telemetry data downsampling and upsampling" {
  let processor = TelemetryProcessor::new()
  
  // Create high-frequency data
  let base_time = 1640995200L
  let high_freq_data = [
    TelemetryData::with_timestamp("cpu_usage", 45.2, "percent", base_time),
    TelemetryData::with_timestamp("cpu_usage", 46.1, "percent", base_time + 10L),
    TelemetryData::with_timestamp("cpu_usage", 44.8, "percent", base_time + 20L),
    TelemetryData::with_timestamp("cpu_usage", 47.3, "percent", base_time + 30L),
    TelemetryData::with_timestamp("cpu_usage", 48.9, "percent", base_time + 40L),
    TelemetryData::with_timestamp("cpu_usage", 47.6, "percent", base_time + 50L),
    TelemetryData::with_timestamp("cpu_usage", 49.2, "percent", base_time + 60L)
  ]
  
  // Downsample to 1-minute intervals
  let downsampled = TelemetryProcessor::downsample(processor, high_freq_data, 60L, "average")
  assert_eq(downsampled.length(), 2) // 7 points downsampled to 1-minute intervals
  assert_eq(downsampled[0].timestamp, base_time)
  assert_eq(downsampled[1].timestamp, base_time + 60L)
  
  // Create low-frequency data
  let low_freq_data = [
    TelemetryData::with_timestamp("memory_usage", 1024.0, "MB", base_time),
    TelemetryData::with_timestamp("memory_usage", 1156.0, "MB", base_time + 300L) // 5 minutes later
  ]
  
  // Upsample to 1-minute intervals using linear interpolation
  let upsampled = TelemetryProcessor::upsample(processor, low_freq_data, 60L, "linear")
  assert_eq(upsampled.length(), 6) // 2 points upsampled to 1-minute intervals
  assert_eq(upsampled[0].timestamp, base_time)
  assert_eq(upsampled[5].timestamp, base_time + 300L)
  
  // Verify interpolated values
  assert_eq(upsampled[0].value, 1024.0)
  assert_eq(upsampled[5].value, 1156.0)
  // Middle values should be between 1024 and 1156
  for i in 1..4 {
    assert_true(upsampled[i].value > 1024.0 && upsampled[i].value < 1156.0)
  }
}

// Test 6: Telemetry Data Compression and Decompression
test "telemetry data compression and decompression" {
  let processor = TelemetryProcessor::new()
  
  // Create a large dataset for compression
  let mut large_dataset = []
  for i in 0..1000 {
    large_dataset = large_dataset + [TelemetryData::new("metric", i.to_int() as Float, "unit", None)]
  }
  
  // Compress the data
  let compressed = TelemetryProcessor::compress(processor, large_dataset, "gzip")
  assert_true(compressed.length() < large_dataset.length()) // Compressed data should be smaller
  
  // Decompress the data
  let decompressed = TelemetryProcessor::decompress(processor, compressed, "gzip")
  assert_eq(decompressed.length(), large_dataset.length()) // Should return to original size
  
  // Verify data integrity
  for i in 0..1000 {
    assert_eq(decompressed[i].name, "metric")
    assert_eq(decompressed[i].value, i.to_int() as Float)
    assert_eq(decompressed[i].unit, "unit")
  }
}

// Test 7: Advanced Telemetry Data Export Formats
test "advanced telemetry data export formats" {
  let processor = TelemetryProcessor::new()
  
  // Create sample data
  let sample_data = [
    TelemetryData::new("response_time", 120.5, "ms", Some([("service", "api"), ("endpoint", "/users")])),
    TelemetryData::new("throughput", 850.0, "req/s", Some([("service", "api"), ("endpoint", "/users")])),
    TelemetryData::new("error_rate", 0.02, "ratio", Some([("service", "auth"), ("endpoint", "/login")]))
  ]
  
  // Export to JSON format
  let json_export = TelemetryProcessor::export_to_json(processor, sample_data)
  assert_true(json_export.length() > 0)
  assert_true(json_export.contains("\"response_time\""))
  assert_true(json_export.contains("\"throughput\""))
  assert_true(json_export.contains("\"error_rate\""))
  
  // Export to CSV format
  let csv_export = TelemetryProcessor::export_to_csv(processor, sample_data)
  assert_true(csv_export.length() > 0)
  assert_true(csv_export.contains("name,value,unit"))
  assert_true(csv_export.contains("response_time,120.5,ms"))
  
  // Export to Prometheus format
  let prometheus_export = TelemetryProcessor::export_to_prometheus(processor, sample_data)
  assert_true(prometheus_export.length() > 0)
  assert_true(prometheus_export.contains("# TYPE response_time gauge"))
  assert_true(prometheus_export.contains("response_time{service=\"api\",endpoint=\"/users\"} 120.5"))
}

// Test 8: Telemetry Data Quality Validation
test "telemetry data quality validation" {
  let processor = TelemetryProcessor::new()
  
  // Create data with quality issues
  let mixed_quality_data = [
    TelemetryData::new("valid_metric", 100.0, "ms", Some([("service", "valid")])),
    TelemetryData::new("null_value_metric", 0.0, "ms", Some([("service", "null_test")])), // Potential null value
    TelemetryData::new("negative_metric", -50.0, "ms", Some([("service", "negative_test")])), // Negative value
    TelemetryData::new("empty_name_metric", 200.0, "ms", Some([("service", "empty_name")])), // Will be modified
    TelemetryData::new("outlier_metric", 100000.0, "ms", Some([("service", "outlier_test")])) // Potential outlier
  ]
  
  // Create empty name metric by modifying the data
  let empty_name_metric = TelemetryData::with_name(mixed_quality_data[3], "")
  let test_data = [
    mixed_quality_data[0],
    mixed_quality_data[1],
    mixed_quality_data[2],
    empty_name_metric,
    mixed_quality_data[4]
  ]
  
  // Validate data quality
  let quality_report = TelemetryProcessor::validate_data_quality(processor, test_data)
  
  // Check validation results
  assert_eq(DataQualityReport::total_metrics(quality_report), 5)
  assert_eq(DataQualityReport::valid_metrics(quality_report), 1)
  assert_eq(DataQualityReport::invalid_metrics(quality_report), 4)
  
  // Check specific issues
  assert_true(DataQualityReport::has_null_values(quality_report))
  assert_true(DataQualityReport::has_negative_values(quality_report))
  assert_true(DataQualityReport::has_empty_names(quality_report))
  assert_true(DataQualityReport::has_outliers(quality_report))
  
  // Clean the data
  let cleaned_data = TelemetryProcessor::clean_data(processor, test_data, quality_report)
  
  // Verify cleaned data
  assert_true(cleaned_data.length() < test_data.length()) // Some data should be removed
  for item in cleaned_data {
    assert_true(TelemetryData::name(item).length() > 0) // No empty names
    assert_true(TelemetryData::value(item) >= 0.0) // No negative values
  }
}