// Azimuth Telemetry System - Data Processing Tests
// This file contains test cases for telemetry data processing functionality

// Test 1: Telemetry Data Collection
test "telemetry data collection" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "data_processing_tracer")
  
  // Test span creation for data collection
  let span = Tracer::start_span(tracer, "data_collection")
  Span::set_attribute(span, "data.source", StringValue("sensor"))
  Span::set_attribute(span, "data.type", StringValue("temperature"))
  Span::set_attribute(span, "data.value", FloatValue(23.5))
  
  // Test data point collection
  let data_point = DataPoint::new(1234567890L, 23.5, "celsius")
  assert_eq(DataPoint::timestamp(data_point), 1234567890L)
  assert_eq(DataPoint::value(data_point), 23.5)
  assert_eq(DataPoint::unit(data_point), "celsius")
  
  // Test batch data collection
  let data_points = [
    DataPoint::new(1234567890L, 23.5, "celsius"),
    DataPoint::new(1234567891L, 24.0, "celsius"),
    DataPoint::new(1234567892L, 23.8, "celsius")
  ]
  let batch = DataBatch::new(data_points)
  assert_eq(DataBatch::size(batch), 3)
  
  Span::end(span)
}

// Test 2: Telemetry Data Aggregation
test "telemetry data aggregation" {
  let data_points = [
    DataPoint::new(1234567890L, 10.0, "units"),
    DataPoint::new(1234567891L, 20.0, "units"),
    DataPoint::new(1234567892L, 30.0, "units"),
    DataPoint::new(1234567893L, 40.0, "units"),
    DataPoint::new(1234567894L, 50.0, "units")
  ]
  
  // Test average calculation
  let avg = DataAggregator::average(data_points)
  assert_eq(avg, 30.0)
  
  // Test min calculation
  let min = DataAggregator::min(data_points)
  assert_eq(min, 10.0)
  
  // Test max calculation
  let max = DataAggregator::max(data_points)
  assert_eq(max, 50.0)
  
  // Test sum calculation
  let sum = DataAggregator::sum(data_points)
  assert_eq(sum, 150.0)
  
  // Test percentile calculation
  let p50 = DataAggregator::percentile(data_points, 50.0)
  assert_eq(p50, 30.0)
  
  let p90 = DataAggregator::percentile(data_points, 90.0)
  assert_eq(p90, 50.0)
}

// Test 3: Telemetry Data Filtering
test "telemetry data filtering" {
  let data_points = [
    DataPoint::new(1234567890L, 10.0, "temperature"),
    DataPoint::new(1234567891L, 20.0, "humidity"),
    DataPoint::new(1234567892L, 30.0, "temperature"),
    DataPoint::new(1234567893L, 40.0, "pressure"),
    DataPoint::new(1234567894L, 50.0, "temperature")
  ]
  
  // Test filtering by unit
  let temp_points = DataFilter::by_unit(data_points, "temperature")
  assert_eq(temp_points.length(), 3)
  
  // Test filtering by value range
  let range_points = DataFilter::by_value_range(data_points, 15.0, 45.0)
  assert_eq(range_points.length(), 3)
  
  // Test filtering by time range
  let start_time = 1234567891L
  let end_time = 1234567893L
  let time_points = DataFilter::by_time_range(data_points, start_time, end_time)
  assert_eq(time_points.length(), 3)
  
  // Test chaining filters
  let filtered_points = DataFilter::by_unit(data_points, "temperature")
  let final_points = DataFilter::by_value_range(filtered_points, 20.0, 60.0)
  assert_eq(final_points.length(), 2)
}

// Test 4: Telemetry Data Transformation
test "telemetry data transformation" {
  let data_points = [
    DataPoint::new(1234567890L, 100.0, "celsius"),
    DataPoint::new(1234567891L, 200.0, "celsius"),
    DataPoint::new(1234567892L, 300.0, "celsius")
  ]
  
  // Test unit conversion
  let fahrenheit_points = DataTransformer::convert_units(data_points, "celsius", "fahrenheit")
  assert_eq(fahrenheit_points.length(), 3)
  assert_eq(DataPoint::value(fahrenheit_points[0]), 212.0) // 100°C = 212°F
  
  // Test scaling transformation
  let scaled_points = DataTransformer::scale(data_points, 0.1)
  assert_eq(DataPoint::value(scaled_points[0]), 10.0)
  
  // Test offset transformation
  let offset_points = DataTransformer::offset(data_points, 273.15)
  assert_eq(DataPoint::value(offset_points[0]), 373.15)
  
  // Test custom transformation
  let custom_transform = fn(value: Double) -> Double { value * value }
  let custom_points = DataTransformer::apply(data_points, custom_transform)
  assert_eq(DataPoint::value(custom_points[0]), 10000.0)
}

// Test 5: Telemetry Data Validation
test "telemetry data validation" {
  let valid_point = DataPoint::new(1234567890L, 25.0, "celsius")
  let invalid_point1 = DataPoint::new(-1L, 25.0, "celsius") // Invalid timestamp
  let invalid_point2 = DataPoint::new(1234567890L, Double::infinity, "celsius") // Invalid value
  let invalid_point3 = DataPoint::new(1234567890L, 25.0, "") // Invalid unit
  
  // Test individual validation
  assert_true(DataValidator::is_valid(valid_point))
  assert_false(DataValidator::is_valid(invalid_point1))
  assert_false(DataValidator::is_valid(invalid_point2))
  assert_false(DataValidator::is_valid(invalid_point3))
  
  // Test batch validation
  let mixed_points = [valid_point, invalid_point1, valid_point, invalid_point2]
  let validation_result = DataValidator::validate_batch(mixed_points)
  
  assert_eq(validation_result.total_count, 4)
  assert_eq(validation_result.valid_count, 2)
  assert_eq(validation_result.invalid_count, 2)
  
  // Test validation with custom rules
  let temp_validator = DataValidator::with_rule(fn(point: DataPoint) -> Bool {
    DataPoint::unit(point) == "celsius" && 
    DataPoint::value(point) >= -273.15 && 
    DataPoint::value(point) <= 1000.0
  })
  
  let valid_temp = DataPoint::new(1234567890L, 25.0, "celsius")
  let invalid_temp = DataPoint::new(1234567890L, -300.0, "celsius")
  
  assert_true(temp_validator(valid_temp))
  assert_false(temp_validator(invalid_temp))
}

// Test 6: Telemetry Data Compression
test "telemetry data compression" {
  let data_points = [
    DataPoint::new(1234567890L, 10.0, "units"),
    DataPoint::new(1234567891L, 10.1, "units"),
    DataPoint::new(1234567892L, 10.2, "units"),
    DataPoint::new(1234567893L, 10.3, "units"),
    DataPoint::new(1234567894L, 10.4, "units")
  ]
  
  // Test time-series compression
  let compressed_data = DataCompressor::compress_time_series(data_points, 0.1)
  assert_true(compressed_data.compressed_size < data_points.length())
  
  // Test delta compression
  let delta_compressed = DataCompressor::compress_delta(data_points)
  assert_true(delta_compressed.compressed_size < data_points.length())
  
  // Test decompression
  let decompressed_points = DataCompressor::decompress(compressed_data)
  assert_eq(decompressed_points.length(), data_points.length())
  
  // Verify data integrity after decompression
  for i in 0..data_points.length() {
    assert_eq(
      DataPoint::value(decompressed_points[i]),
      DataPoint::value(data_points[i])
    )
  }
}

// Test 7: Telemetry Data Export
test "telemetry data export" {
  let data_points = [
    DataPoint::new(1234567890L, 25.0, "celsius"),
    DataPoint::new(1234567891L, 60.0, "humidity"),
    DataPoint::new(1234567892L, 1013.25, "pressure")
  ]
  
  // Test JSON export
  let json_exporter = DataExporter::json()
  let json_data = json_exporter.export(data_points)
  assert_true(json_data.length() > 0)
  assert_true(json_data.contains("celsius"))
  assert_true(json_data.contains("humidity"))
  
  // Test CSV export
  let csv_exporter = DataExporter::csv()
  let csv_data = csv_exporter.export(data_points)
  assert_true(csv_data.length() > 0)
  assert_true(csv_data.contains("timestamp,value,unit"))
  
  // Test custom format export
  let custom_exporter = DataExporter::custom(fn(points: Array[DataPoint]) -> String {
    let mut result = "DATA_EXPORT:\n"
    for point in points {
      result = result + "T:" + DataPoint::timestamp(point).to_string() + 
                      " V:" + DataPoint::value(point).to_string() + 
                      " U:" + DataPoint::unit(point) + "\n"
    }
    result
  })
  let custom_data = custom_exporter.export(data_points)
  assert_true(custom_data.contains("DATA_EXPORT"))
  assert_true(custom_data.contains("T:1234567890"))
}