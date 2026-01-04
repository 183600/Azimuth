// Azimuth Telemetry System - Data Processing Tests
// This file contains comprehensive test cases for telemetry data processing functionality

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data
  let data_points = [
    TelemetryData::new("metric1", 10.5, "ms", 1234567890L),
    TelemetryData::new("metric1", 15.3, "ms", 1234567891L),
    TelemetryData::new("metric1", 12.7, "ms", 1234567892L),
    TelemetryData::new("metric2", 100.0, "count", 1234567890L),
    TelemetryData::new("metric2", 150.0, "count", 1234567891L)
  ]
  
  // Process and aggregate data
  let aggregated_data = TelemetryProcessor::aggregate(processor, data_points, "metric1")
  
  // Verify aggregation results
  assert_eq(aggregated_data.count, 3)
  assert_eq(aggregated_data.min_value, 10.5)
  assert_eq(aggregated_data.max_value, 15.3)
  assert_eq(aggregated_data.avg_value, 12.833333333333334)
  assert_eq(aggregated_data.sum_value, 38.5)
  
  // Test with different metric
  let aggregated_data2 = TelemetryProcessor::aggregate(processor, data_points, "metric2")
  assert_eq(aggregated_data2.count, 2)
  assert_eq(aggregated_data2.min_value, 100.0)
  assert_eq(aggregated_data2.max_value, 150.0)
  assert_eq(aggregated_data2.avg_value, 125.0)
  assert_eq(aggregated_data2.sum_value, 250.0)
}

// Test 2: Telemetry Data Filtering
test "telemetry data filtering" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data with various attributes
  let data_points = [
    TelemetryData::with_attributes("metric1", 10.5, "ms", 1234567890L, [
      ("service", "auth"),
      ("environment", "production")
    ]),
    TelemetryData::with_attributes("metric1", 15.3, "ms", 1234567891L, [
      ("service", "payment"),
      ("environment", "staging")
    ]),
    TelemetryData::with_attributes("metric1", 12.7, "ms", 1234567892L, [
      ("service", "auth"),
      ("environment", "production")
    ]),
    TelemetryData::with_attributes("metric2", 100.0, "count", 1234567890L, [
      ("service", "user"),
      ("environment", "production")
    ])
  ]
  
  // Filter by service name
  let auth_service_data = TelemetryProcessor::filter_by_attribute(processor, data_points, "service", "auth")
  assert_eq(auth_service_data.length(), 2)
  
  // Filter by environment
  let production_data = TelemetryProcessor::filter_by_attribute(processor, data_points, "environment", "production")
  assert_eq(production_data.length(), 3)
  
  // Filter by multiple attributes
  let auth_production_data = TelemetryProcessor::filter_by_multiple_attributes(processor, data_points, [
    ("service", "auth"),
    ("environment", "production")
  ])
  assert_eq(auth_production_data.length(), 2)
  
  // Filter by time range
  let start_time = 1234567891L
  let end_time = 1234567892L
  let time_filtered_data = TelemetryProcessor::filter_by_time_range(processor, data_points, start_time, end_time)
  assert_eq(time_filtered_data.length(), 2)
}

// Test 3: Telemetry Data Transformation
test "telemetry data transformation" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data
  let data_points = [
    TelemetryData::new("response_time_ms", 1500, "ms", 1234567890L),
    TelemetryData::new("response_time_ms", 2000, "ms", 1234567891L),
    TelemetryData::new("response_time_ms", 1200, "ms", 1234567892L)
  ]
  
  // Transform unit from ms to seconds
  let transformed_data = TelemetryProcessor::transform_unit(processor, data_points, "ms", "s", 0.001)
  assert_eq(transformed_data[0].value, 1.5)
  assert_eq(transformed_data[0].unit, "s")
  assert_eq(transformed_data[1].value, 2.0)
  assert_eq(transformed_data[1].unit, "s")
  assert_eq(transformed_data[2].value, 1.2)
  assert_eq(transformed_data[2].unit, "s")
  
  // Transform metric name
  let renamed_data = TelemetryProcessor::transform_metric_name(processor, data_points, "response_time_ms", "response_time_seconds")
  assert_eq(renamed_data[0].metric_name, "response_time_seconds")
  assert_eq(renamed_data[1].metric_name, "response_time_seconds")
  assert_eq(renamed_data[2].metric_name, "response_time_seconds")
  
  // Apply custom transformation function
  let normalized_data = TelemetryProcessor::apply_transformation(processor, data_points, fn(data) {
    TelemetryData::new(data.metric_name, data.value / 1000.0, "s", data.timestamp)
  })
  assert_eq(normalized_data[0].value, 1.5)
  assert_eq(normalized_data[0].unit, "s")
}

// Test 4: Telemetry Data Sampling
test "telemetry data sampling" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data
  let data_points = []
  for i in 0..=100 {
    data_points.push(TelemetryData::new("metric1", i.to_float(), "count", 1234567890L + i.to_long()))
  }
  
  // Test uniform sampling
  let uniform_sample = TelemetryProcessor::uniform_sample(processor, data_points, 10)
  assert_eq(uniform_sample.length(), 10)
  
  // Test time-based sampling
  let time_sample = TelemetryProcessor::time_based_sample(processor, data_points, 10)
  assert_eq(time_sample.length(), 11) // Should include both start and end points
  
  // Test reservoir sampling
  let reservoir_sample = TelemetryProcessor::reservoir_sample(processor, data_points, 5)
  assert_eq(reservoir_sample.length(), 5)
  
  // Test stratified sampling
  let strata = [
    ("low", 0.0..=33.0),
    ("medium", 34.0..=66.0),
    ("high", 67.0..=100.0)
  ]
  let stratified_sample = TelemetryProcessor::stratified_sample(processor, data_points, strata, 2)
  assert_eq(stratified_sample.length(), 6) // 2 from each stratum
}

// Test 5: Telemetry Data Validation
test "telemetry data validation" {
  let processor = TelemetryProcessor::new()
  
  // Create valid telemetry data
  let valid_data = TelemetryData::new("metric1", 10.5, "ms", 1234567890L)
  
  // Create invalid telemetry data
  let invalid_data_name = TelemetryData::new("", 10.5, "ms", 1234567890L)
  let invalid_data_value = TelemetryData::new("metric1", (-1.0).sqrt(), "ms", 1234567890L) // NaN
  let invalid_data_unit = TelemetryData::new("metric1", 10.5, "", 1234567890L)
  let invalid_data_timestamp = TelemetryData::new("metric1", 10.5, "ms", -1L)
  
  // Test validation
  assert_true(TelemetryProcessor::is_valid(processor, valid_data))
  assert_false(TelemetryProcessor::is_valid(processor, invalid_data_name))
  assert_false(TelemetryProcessor::is_valid(processor, invalid_data_value))
  assert_false(TelemetryProcessor::is_valid(processor, invalid_data_unit))
  assert_false(TelemetryProcessor::is_valid(processor, invalid_data_timestamp))
  
  // Test batch validation
  let mixed_data = [valid_data, invalid_data_name, invalid_data_value]
  let validation_results = TelemetryProcessor::validate_batch(processor, mixed_data)
  assert_eq(validation_results.valid_count, 1)
  assert_eq(validation_results.invalid_count, 2)
  assert_eq(validation_results.validation_errors.length(), 2)
}

// Test 6: Telemetry Data Compression
test "telemetry data compression" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data
  let data_points = []
  for i in 0..=1000 {
    data_points.push(TelemetryData::new("metric1", i.to_float(), "count", 1234567890L + i.to_long()))
  }
  
  // Test compression
  let compressed_data = TelemetryProcessor::compress(processor, data_points)
  assert_true(compressed_data.size < data_points.length() * 50) // Expect some compression
  
  // Test decompression
  let decompressed_data = TelemetryProcessor::decompress(processor, compressed_data)
  assert_eq(decompressed_data.length(), data_points.length())
  
  // Verify data integrity
  for i in 0..=data_points.length() - 1 {
    assert_eq(decompressed_data[i].metric_name, data_points[i].metric_name)
    assert_eq(decompressed_data[i].value, data_points[i].value)
    assert_eq(decompressed_data[i].unit, data_points[i].unit)
    assert_eq(decompressed_data[i].timestamp, data_points[i].timestamp)
  }
}

// Test 7: Telemetry Data Export
test "telemetry data export" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data
  let data_points = [
    TelemetryData::new("metric1", 10.5, "ms", 1234567890L),
    TelemetryData::new("metric2", 100.0, "count", 1234567891L),
    TelemetryData::new("metric3", 0.95, "ratio", 1234567892L)
  ]
  
  // Test JSON export
  let json_export = TelemetryProcessor::export_to_json(processor, data_points)
  assert_true(json_export.contains("\"metric_name\":\"metric1\""))
  assert_true(json_export.contains("\"value\":10.5"))
  assert_true(json_export.contains("\"unit\":\"ms\""))
  
  // Test CSV export
  let csv_export = TelemetryProcessor::export_to_csv(processor, data_points)
  assert_true(csv_export.contains("metric_name,value,unit,timestamp"))
  assert_true(csv_export.contains("metric1,10.5,ms,1234567890"))
  
  // Test custom format export
  let custom_export = TelemetryProcessor::export_to_custom_format(processor, data_points, "{metric}={value}{unit}@{timestamp}")
  assert_true(custom_export.contains("metric1=10.5ms@1234567890"))
  assert_true(custom_export.contains("metric2=100.0count@1234567891"))
}

// Test 8: Telemetry Data Enrichment
test "telemetry data enrichment" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data
  let data_points = [
    TelemetryData::with_attributes("response_time", 150.0, "ms", 1234567890L, [
      ("service", "auth"),
      ("endpoint", "/login")
    ]),
    TelemetryData::with_attributes("response_time", 200.0, "ms", 1234567891L, [
      ("service", "payment"),
      ("endpoint", "/process")
    ])
  ]
  
  // Enrich with computed attributes
  let enriched_data = TelemetryProcessor::enrich_with_computed_attributes(processor, data_points, [
    ("response_category", fn(data) {
      if data.value < 100.0 { "fast" } 
      else if data.value < 200.0 { "normal" } 
      else { "slow" }
    }),
    ("hour_of_day", fn(data) {
      ((data.timestamp / 3600) % 24).to_string()
    })
  ])
  
  // Verify enrichment
  assert_eq(enriched_data[0].get_attribute("response_category"), Some("normal"))
  assert_eq(enriched_data[1].get_attribute("response_category"), Some("normal"))
  
  // Enrich with external data
  let external_data = [
    ("auth", "service_owner", "team-a"),
    ("payment", "service_owner", "team-b")
  ]
  let further_enriched_data = TelemetryProcessor::enrich_with_external_data(processor, enriched_data, external_data, "service")
  
  // Verify external enrichment
  assert_eq(further_enriched_data[0].get_attribute("service_owner"), Some("team-a"))
  assert_eq(further_enriched_data[1].get_attribute("service_owner"), Some("team-b"))
}