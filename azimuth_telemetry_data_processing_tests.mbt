// Azimuth Telemetry System - Data Processing Tests
// This file contains test cases for telemetry data processing functionality

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation" {
  // Create sample telemetry data points
  let data_points = [
    TelemetryDataPoint::new("cpu.usage", 45.2, 1000L),
    TelemetryDataPoint::new("cpu.usage", 50.1, 2000L),
    TelemetryDataPoint::new("cpu.usage", 38.7, 3000L),
    TelemetryDataPoint::new("memory.usage", 67.3, 1000L),
    TelemetryDataPoint::new("memory.usage", 72.1, 2000L),
    TelemetryDataPoint::new("memory.usage", 65.8, 3000L)
  ]
  
  // Test aggregation by metric name
  let aggregated = TelemetryProcessor::aggregate_by_metric(data_points)
  
  // Verify CPU usage aggregation
  let cpu_data = Map::get(aggregated, "cpu.usage")
  match cpu_data {
    Some(points) => {
      assert_eq(points.length(), 3)
      assert_eq(points[0].value, 45.2)
      assert_eq(points[1].value, 50.1)
      assert_eq(points[2].value, 38.7)
    }
    None => assert_true(false)
  }
  
  // Verify memory usage aggregation
  let memory_data = Map::get(aggregated, "memory.usage")
  match memory_data {
    Some(points) => {
      assert_eq(points.length(), 3)
      assert_eq(points[0].value, 67.3)
      assert_eq(points[1].value, 72.1)
      assert_eq(points[2].value, 65.8)
    }
    None => assert_true(false)
  }
}

// Test 2: Telemetry Data Filtering
test "telemetry data filtering" {
  // Create sample telemetry data with different time ranges
  let data_points = [
    TelemetryDataPoint::new("cpu.usage", 45.2, 1000L),
    TelemetryDataPoint::new("cpu.usage", 50.1, 2000L),
    TelemetryDataPoint::new("cpu.usage", 38.7, 3000L),
    TelemetryDataPoint::new("cpu.usage", 42.3, 4000L),
    TelemetryDataPoint::new("cpu.usage", 55.8, 5000L)
  ]
  
  // Test time range filtering
  let filtered = TelemetryProcessor::filter_by_time_range(data_points, 2000L, 4000L)
  
  // Verify filtered results
  assert_eq(filtered.length(), 3)
  assert_eq(filtered[0].value, 50.1)
  assert_eq(filtered[1].value, 38.7)
  assert_eq(filtered[2].value, 42.3)
  
  // Test value threshold filtering
  let threshold_filtered = TelemetryProcessor::filter_by_threshold(data_points, 40.0, 50.0)
  
  // Verify threshold filtered results
  assert_eq(threshold_filtered.length(), 2)
  assert_eq(threshold_filtered[0].value, 45.2)
  assert_eq(threshold_filtered[1].value, 42.3)
}

// Test 3: Telemetry Data Transformation
test "telemetry data transformation" {
  // Create sample telemetry data
  let data_points = [
    TelemetryDataPoint::new("cpu.usage", 45.2, 1000L),
    TelemetryDataPoint::new("memory.usage", 1024.0, 1000L),
    TelemetryDataPoint::new("disk.io", 256.5, 1000L)
  ]
  
  // Test unit transformation
  let transformer = UnitTransformer::new()
  let transformed_data = transformer.transform(data_points, [
    ("memory.usage", "bytes", "megabytes", 1024.0),
    ("disk.io", "bytes", "kilobytes", 1024.0)
  ])
  
  // Verify transformed memory usage (in MB)
  let memory_point = find_by_metric(transformed_data, "memory.usage")
  match memory_point {
    Some(point) => assert_eq(point.value, 1.0)
    None => assert_true(false)
  }
  
  // Verify transformed disk I/O (in KB)
  let disk_point = find_by_metric(transformed_data, "disk.io")
  match disk_point {
    Some(point) => assert_eq(point.value, 0.25)
    None => assert_true(false)
  }
  
  // CPU usage should remain unchanged
  let cpu_point = find_by_metric(transformed_data, "cpu.usage")
  match cpu_point {
    Some(point) => assert_eq(point.value, 45.2)
    None => assert_true(false)
  }
}

// Test 4: Telemetry Data Downsampling
test "telemetry data downsampling" {
  // Create high-frequency telemetry data
  let data_points = [
    TelemetryDataPoint::new("cpu.usage", 45.2, 1000L),
    TelemetryDataPoint::new("cpu.usage", 46.1, 1100L),
    TelemetryDataPoint::new("cpu.usage", 44.8, 1200L),
    TelemetryDataPoint::new("cpu.usage", 47.3, 1300L),
    TelemetryDataPoint::new("cpu.usage", 45.9, 1400L),
    TelemetryDataPoint::new("cpu.usage", 48.2, 1500L),
    TelemetryDataPoint::new("cpu.usage", 43.7, 1600L),
    TelemetryDataPoint::new("cpu.usage", 46.5, 1700L),
    TelemetryDataPoint::new("cpu.usage", 47.1, 1800L),
    TelemetryDataPoint::new("cpu.usage", 44.9, 1900L)
  ]
  
  // Test average downsampling
  let downsampled_avg = TelemetryProcessor::downsample(data_points, 1000L, 2000L, "average")
  
  // Verify downsampled result
  assert_eq(downsampled_avg.length(), 1)
  assert_eq(downsampled_avg[0].value, 45.97) // Average of all values
  
  // Test max downsampling
  let downsampled_max = TelemetryProcessor::downsample(data_points, 1000L, 2000L, "max")
  
  // Verify downsampled result
  assert_eq(downsampled_max.length(), 1)
  assert_eq(downsampled_max[0].value, 48.2) // Maximum value
  
  // Test min downsampling
  let downsampled_min = TelemetryProcessor::downsample(data_points, 1000L, 2000L, "min")
  
  // Verify downsampled result
  assert_eq(downsampled_min.length(), 1)
  assert_eq(downsampled_min[0].value, 43.7) // Minimum value
}

// Test 5: Telemetry Data Anomaly Detection
test "telemetry data anomaly detection" {
  // Create telemetry data with anomalies
  let data_points = [
    TelemetryDataPoint::new("cpu.usage", 45.2, 1000L),
    TelemetryDataPoint::new("cpu.usage", 46.1, 1100L),
    TelemetryDataPoint::new("cpu.usage", 44.8, 1200L),
    TelemetryDataPoint::new("cpu.usage", 95.3, 1300L), // Anomaly
    TelemetryDataPoint::new("cpu.usage", 45.9, 1400L),
    TelemetryDataPoint::new("cpu.usage", 48.2, 1500L),
    TelemetryDataPoint::new("cpu.usage", 43.7, 1600L),
    TelemetryDataPoint::new("cpu.usage", 5.1, 1700L),  // Anomaly
    TelemetryDataPoint::new("cpu.usage", 47.1, 1800L),
    TelemetryDataPoint::new("cpu.usage", 44.9, 1900L)
  ]
  
  // Test statistical anomaly detection
  let detector = StatisticalAnomalyDetector::new(2.0) // 2 standard deviations
  let anomalies = detector.detect(data_points)
  
  // Verify anomalies detected
  assert_eq(anomalies.length(), 2)
  assert_eq(anomalies[0].value, 95.3)
  assert_eq(anomalies[1].value, 5.1)
  
  // Test custom threshold anomaly detection
  let threshold_detector = ThresholdAnomalyDetector::new(40.0, 55.0)
  let threshold_anomalies = threshold_detector.detect(data_points)
  
  // Verify threshold anomalies
  assert_eq(threshold_anomalies.length(), 2)
  assert_eq(threshold_anomalies[0].value, 95.3)
  assert_eq(threshold_anomalies[1].value, 5.1)
}

// Test 6: Telemetry Data Correlation Analysis
test "telemetry data correlation analysis" {
  // Create correlated telemetry data
  let cpu_data = [
    TelemetryDataPoint::new("cpu.usage", 45.2, 1000L),
    TelemetryDataPoint::new("cpu.usage", 50.1, 2000L),
    TelemetryDataPoint::new("cpu.usage", 65.3, 3000L),
    TelemetryDataPoint::new("cpu.usage", 70.8, 4000L),
    TelemetryDataPoint::new("cpu.usage", 85.2, 5000L)
  ]
  
  let memory_data = [
    TelemetryDataPoint::new("memory.usage", 67.3, 1000L),
    TelemetryDataPoint::new("cpu.usage", 72.1, 2000L),
    TelemetryDataPoint::new("cpu.usage", 78.5, 3000L),
    TelemetryDataPoint::new("cpu.usage", 82.3, 4000L),
    TelemetryDataPoint::new("cpu.usage", 91.7, 5000L)
  ]
  
  // Test correlation analysis
  let analyzer = CorrelationAnalyzer::new()
  let correlation = analyzer.analyze(cpu_data, memory_data)
  
  // Verify positive correlation
  assert_true(correlation > 0.8)
  
  // Test uncorrelated data
  let network_data = [
    TelemetryDataPoint::new("network.usage", 10.2, 1000L),
    TelemetryDataPoint::new("network.usage", 15.7, 2000L),
    TelemetryDataPoint::new("network.usage", 8.3, 3000L),
    TelemetryDataPoint::new("network.usage", 20.1, 4000L),
    TelemetryDataPoint::new("network.usage", 12.5, 5000L)
  ]
  
  let network_correlation = analyzer.analyze(cpu_data, network_data)
  
  // Verify low correlation
  assert_true(network_correlation < 0.5)
}

// Test 7: Telemetry Data Trend Analysis
test "telemetry data trend analysis" {
  // Create telemetry data with trend
  let data_points = [
    TelemetryDataPoint::new("cpu.usage", 45.2, 1000L),
    TelemetryDataPoint::new("cpu.usage", 47.1, 2000L),
    TelemetryDataPoint::new("cpu.usage", 49.3, 3000L),
    TelemetryDataPoint::new("cpu.usage", 51.8, 4000L),
    TelemetryDataPoint::new("cpu.usage", 54.2, 5000L),
    TelemetryDataPoint::new("cpu.usage", 56.7, 6000L),
    TelemetryDataPoint::new("cpu.usage", 59.1, 7000L),
    TelemetryDataPoint::new("cpu.usage", 61.5, 8000L)
  ]
  
  // Test trend analysis
  let analyzer = TrendAnalyzer::new()
  let trend = analyzer.analyze(data_points)
  
  // Verify upward trend
  match trend {
    Upward(slope) => assert_true(slope > 0.0)
    Downward(_) => assert_true(false)
    Stable => assert_true(false)
  }
  
  // Test trend prediction
  let prediction = analyzer.predict(data_points, 9000L)
  match prediction {
    Some(value) => assert_true(value > 60.0)
    None => assert_true(false)
  }
  
  // Test with stable data
  let stable_data = [
    TelemetryDataPoint::new("cpu.usage", 50.0, 1000L),
    TelemetryDataPoint::new("cpu.usage", 49.8, 2000L),
    TelemetryDataPoint::new("cpu.usage", 50.1, 3000L),
    TelemetryDataPoint::new("cpu.usage", 50.2, 4000L),
    TelemetryDataPoint::new("cpu.usage", 49.9, 5000L)
  ]
  
  let stable_trend = analyzer.analyze(stable_data)
  
  // Verify stable trend
  match stable_trend {
    Upward(_) => assert_true(false)
    Downward(_) => assert_true(false)
    Stable => assert_true(true)
  }
}

// Test 8: Telemetry Data Compression
test "telemetry data compression" {
  // Create telemetry data
  let data_points = [
    TelemetryDataPoint::new("cpu.usage", 45.2, 1000L),
    TelemetryDataPoint::new("cpu.usage", 46.1, 1100L),
    TelemetryDataPoint::new("cpu.usage", 44.8, 1200L),
    TelemetryDataPoint::new("cpu.usage", 47.3, 1300L),
    TelemetryDataPoint::new("cpu.usage", 45.9, 1400L)
  ]
  
  // Test data compression
  let compressor = TelemetryCompressor::new()
  let compressed = compressor.compress(data_points)
  
  // Verify compression
  assert_true(compressed.length() < data_points.length())
  
  // Test data decompression
  let decompressed = compressor.decompress(compressed)
  
  // Verify decompression accuracy
  assert_eq(decompressed.length(), data_points.length())
  for i in 0..data_points.length() {
    assert_eq(decompressed[i].metric_name, data_points[i].metric_name)
    assert_eq(decompressed[i].value, data_points[i].value)
    assert_eq(decompressed[i].timestamp, data_points[i].timestamp)
  }
}

// Test 9: Telemetry Data Export
test "telemetry data export" {
  // Create telemetry data
  let data_points = [
    TelemetryDataPoint::new("cpu.usage", 45.2, 1000L),
    TelemetryDataPoint::new("memory.usage", 67.3, 1000L),
    TelemetryDataPoint::new("cpu.usage", 50.1, 2000L),
    TelemetryDataPoint::new("memory.usage", 72.1, 2000L)
  ]
  
  // Test JSON export
  let exporter = JsonExporter::new()
  let json_data = exporter.export(data_points)
  
  // Verify JSON export
  assert_true(json_data.length() > 0)
  assert_true(String::contains(json_data, "cpu.usage"))
  assert_true(String::contains(json_data, "memory.usage"))
  
  // Test CSV export
  let csv_exporter = CsvExporter::new()
  let csv_data = csv_exporter.export(data_points)
  
  // Verify CSV export
  assert_true(csv_data.length() > 0)
  assert_true(String::contains(csv_data, "metric_name,value,timestamp"))
  assert_true(String::contains(csv_data, "cpu.usage"))
  assert_true(String::contains(csv_data, "memory.usage"))
  
  // Test protocol buffer export
  let protobuf_exporter = ProtobufExporter::new()
  let protobuf_data = protobuf_exporter.export(data_points)
  
  // Verify protobuf export
  assert_true(protobuf_data.length() > 0)
}

// Test 10: Telemetry Data Import
test "telemetry data import" {
  // Create sample JSON data
  let json_data = "{\"data_points\":[{\"metric_name\":\"cpu.usage\",\"value\":45.2,\"timestamp\":1000},{\"metric_name\":\"memory.usage\",\"value\":67.3,\"timestamp\":1000}]}"
  
  // Test JSON import
  let importer = JsonImporter::new()
  let imported_data = importer.import(json_data)
  
  // Verify imported data
  assert_eq(imported_data.length(), 2)
  assert_eq(imported_data[0].metric_name, "cpu.usage")
  assert_eq(imported_data[0].value, 45.2)
  assert_eq(imported_data[0].timestamp, 1000L)
  assert_eq(imported_data[1].metric_name, "memory.usage")
  assert_eq(imported_data[1].value, 67.3)
  assert_eq(imported_data[1].timestamp, 1000L)
  
  // Create sample CSV data
  let csv_data = "metric_name,value,timestamp\ncpu.usage,50.1,2000\nmemory.usage,72.1,2000"
  
  // Test CSV import
  let csv_importer = CsvImporter::new()
  let csv_imported_data = csv_importer.import(csv_data)
  
  // Verify CSV imported data
  assert_eq(csv_imported_data.length(), 2)
  assert_eq(csv_imported_data[0].metric_name, "cpu.usage")
  assert_eq(csv_imported_data[0].value, 50.1)
  assert_eq(csv_imported_data[0].timestamp, 2000L)
  assert_eq(csv_imported_data[1].metric_name, "memory.usage")
  assert_eq(csv_imported_data[1].value, 72.1)
  assert_eq(csv_imported_data[1].timestamp, 2000L)
}

// Helper function to find a data point by metric name
func find_by_metric(data_points : Array[TelemetryDataPoint], metric_name : String) -> Option[TelemetryDataPoint] {
  for point in data_points {
    if point.metric_name == metric_name {
      return Some(point)
    }
  }
  return None
}