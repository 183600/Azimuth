// Advanced Data Processing Tests for Azimuth Telemetry System
// This file contains test cases for advanced data processing capabilities

// Test 1: Batch Processing with Aggregation
test "batch processing with aggregation" {
  let processor = BatchProcessor::new(100) // Batch size of 100
  
  // Create test data
  let telemetry_data = []
  for i in 1..=150 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric.name", StringValue("response_time"))
    Attributes::set(attrs, "service", StringValue("api"))
    Attributes::set(attrs, "endpoint", StringValue("/api/data"))
    Attributes::set(attrs, "status", StringValue(if i % 10 == 0 { "error" } else { "success" }))
    
    telemetry_data.push(TelemetryData::new(
      (i * 10.0), // Value: 10, 20, 30, ..., 1500
      attrs,
      1234567890L + (i * 1000L)
    ))
  }
  
  // Process in batches
  let processed_batches = []
  for batch in BatchProcessor::process(processor, telemetry_data) {
    let aggregated = BatchAggregator::aggregate(batch)
    processed_batches.push(aggregated)
  }
  
  // Should have 2 batches: one with 100 items, one with 50 items
  assert_eq(processed_batches.length(), 2)
  
  // Check first batch aggregation
  let first_batch = processed_batches[0]
  assert_eq(AggregatedData::count(first_batch), 100)
  assert_eq(AggregatedData::sum(first_batch), 50500.0) // Sum of 10+20+...+1000
  assert_eq(AggregatedData::average(first_batch), 505.0)
  
  // Check second batch aggregation
  let second_batch = processed_batches[1]
  assert_eq(AggregatedData::count(second_batch), 50)
  assert_eq(AggregatedData::sum(second_batch), 57750.0) // Sum of 1010+1020+...+1500
  assert_eq(AggregatedData::average(second_batch), 1155.0)
}

// Test 2: Stream Processing with Windowing
test "stream processing with windowing" {
  let stream_processor = StreamProcessor::new()
  
  // Create tumbling window of 5 seconds
  let tumbling_window = TumblingWindow::new(5000L) // 5 seconds in milliseconds
  
  // Create sliding window of 3 seconds with 1 second slide
  let sliding_window = SlidingWindow::new(3000L, 1000L) // 3s window, 1s slide
  
  // Generate time-series data
  let base_time = 1234567890L
  let stream_data = []
  
  for i in 1..=20 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "sensor.id", StringValue("temp_sensor_01"))
    Attributes::set(attrs, "location", StringValue("server_room"))
    
    stream_data.push(StreamData::new(
      20.0 + (i * 0.5), // Temperature values: 20.5, 21.0, 21.5, ...
      attrs,
      base_time + (i * 500L) // Every 500ms
    ))
  }
  
  // Process with tumbling window
  let tumbling_results = StreamProcessor::process_with_window(
    stream_processor,
    stream_data,
    tumbling_window
  )
  
  // Should have 2 windows: 0-5s and 5-10s
  assert_eq(tumbling_results.length(), 2)
  
  // First window should contain 10 data points (0-5s)
  let first_window = tumbling_results[0]
  assert_eq(WindowData::count(first_window), 10)
  assert_eq(WindowData::min_value(first_window), 20.5)
  assert_eq(WindowData::max_value(first_window), 25.0)
  
  // Second window should contain 10 data points (5-10s)
  let second_window = tumbling_results[1]
  assert_eq(WindowData::count(second_window), 10)
  assert_eq(WindowData::min_value(second_window), 25.5)
  assert_eq(WindowData::max_value(second_window), 30.0)
  
  // Process with sliding window
  let sliding_results = StreamProcessor::process_with_window(
    stream_processor,
    stream_data,
    sliding_window
  )
  
  // Should have more windows due to sliding
  assert_true(sliding_results.length() > tumbling_results.length())
}

// Test 3: Data Transformation and Enrichment
test "data transformation and enrichment" {
  let transformer = DataTransformer::new()
  
  // Create raw telemetry data
  let raw_data = TelemetryData::new(
    123.45,
    Attributes::new(),
    1234567890L
  )
  
  // Define transformation rules
  let transformation_rules = [
    // Round to 2 decimal places
    TransformRule::round_value(2),
    // Add timestamp-based attributes
    TransformRule::add_timestamp_attributes(),
    // Add computed attributes
    TransformRule::compute_attribute("value_category", |value| {
      if value < 50.0 { "low" }
      else if value < 100.0 { "medium" }
      else { "high" }
    }),
    // Add environment-based attributes
    TransformRule::enrich_with_environment()
  ]
  
  // Apply transformations
  let transformed_data = DataTransformer::apply_rules(transformer, raw_data, transformation_rules)
  
  // Verify transformations
  assert_eq(TransformedData::value(transformed_data), 123.45) // Already 2 decimal places
  
  let hour_attr = TransformedData::get_attribute(transformed_data, "timestamp.hour")
  match hour_attr {
    Some(StringValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
  
  let category_attr = TransformedData::get_attribute(transformed_data, "value_category")
  match category_attr {
    Some(StringValue(category)) => assert_eq(category, "high")
    _ => assert_true(false)
  }
  
  let env_attr = TransformedData::get_attribute(transformed_data, "environment")
  match env_attr {
    Some(StringValue(_)) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 4: Anomaly Detection in Telemetry Data
test "anomaly detection in telemetry data" {
  let detector = AnomalyDetector::new()
  
  // Create normal baseline data
  let baseline_data = []
  for i in 1..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("cpu_usage"))
    Attributes::set(attrs, "host", StringValue("server-01"))
    
    // Normal CPU usage: 20-60%
    let normal_value = 20.0 + (Float::random() * 40.0)
    baseline_data.push(TelemetryData::new(normal_value, attrs, 1234567890L + i))
  }
  
  // Train the anomaly detector
  AnomalyDetector::train(detector, baseline_data)
  
  // Create test data with anomalies
  let test_data = []
  
  // Normal data points
  for i in 1..=10 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("cpu_usage"))
    Attributes::set(attrs, "host", StringValue("server-01"))
    
    let normal_value = 20.0 + (Float::random() * 40.0)
    test_data.push(TelemetryData::new(normal_value, attrs, 1234567990L + i))
  }
  
  // Anomalous data points
  let anomaly_attrs = Attributes::new()
  Attributes::set(anomaly_attrs, "metric", StringValue("cpu_usage"))
  Attributes::set(anomaly_attrs, "host", StringValue("server-01"))
  
  // CPU spike to 95%
  test_data.push(TelemetryData::new(95.0, anomaly_attrs, 1234568001L))
  
  // CPU drop to 5%
  test_data.push(TelemetryData::new(5.0, anomaly_attrs, 1234568002L))
  
  // Detect anomalies
  let anomaly_results = AnomalyDetector::detect(detector, test_data)
  
  // Should detect 2 anomalies
  assert_eq(anomaly_results.length(), 2)
  
  // Check first anomaly (CPU spike)
  let first_anomaly = anomaly_results[0]
  assert_eq(AnomalyResult::value(first_anomaly), 95.0)
  assert_eq(AnomalyResult::anomaly_score(first_anomaly) > 0.8, true) // High anomaly score
  
  // Check second anomaly (CPU drop)
  let second_anomaly = anomaly_results[1]
  assert_eq(AnomalyResult::value(second_anomaly), 5.0)
  assert_eq(AnomalyResult::anomaly_score(second_anomaly) > 0.8, true) // High anomaly score
}

// Test 5: Data Compression and Decompression
test "data compression and decompression" {
  let compressor = DataCompressor::new(Gzip)
  
  // Create large telemetry dataset
  let large_dataset = []
  for i in 1..=1000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "trace_id", StringValue("trace_" + i.to_string()))
    Attributes::set(attrs, "span_id", StringValue("span_" + i.to_string()))
    Attributes::set(attrs, "service", StringValue("microservice_" + (i % 10).to_string()))
    Attributes::set(attrs, "operation", StringValue("operation_" + (i % 5).to_string()))
    Attributes::set(attrs, "status", StringValue(if i % 20 == 0 { "error" } else { "success" }))
    
    large_dataset.push(TelemetryData::new(
      (i * 1.5),
      attrs,
      1234567890L + i
    ))
  }
  
  // Serialize to bytes
  let serialized_data = TelemetrySerializer::serialize(large_dataset)
  let original_size = serialized_data.length()
  
  // Compress the data
  let compressed_data = DataCompressor::compress(compressor, serialized_data)
  let compressed_size = compressed_data.length()
  
  // Verify compression
  assert_true(compressed_size < original_size)
  let compression_ratio = (compressed_size as Float) / (original_size as Float)
  assert_true(compression_ratio < 0.8) // At least 20% compression
  
  // Decompress the data
  let decompressed_data = DataCompressor::decompress(compressor, compressed_data)
  
  // Verify decompression
  assert_eq(decompressed_data.length(), original_size)
  
  // Deserialize and verify data integrity
  let deserialized_data = TelemetrySerializer::deserialize(decompressed_data)
  assert_eq(deserialized_data.length(), 1000)
  
  // Verify specific data points
  let first_item = deserialized_data[0]
  assert_eq(TelemetryData::value(first_item), 1.5)
  
  let item_500 = deserialized_data[499]
  assert_eq(TelemetryData::value(item_500), 750.0)
  
  let last_item = deserialized_data[999]
  assert_eq(TelemetryData::value(last_item), 1500.0)
}

// Test 6: Data Filtering with Complex Conditions
test "data filtering with complex conditions" {
  let filter = DataFilter::new()
  
  // Create diverse telemetry data
  let telemetry_data = []
  
  // Add different types of metrics
  for i in 1..=50 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric_type", StringValue(if i % 3 == 0 { "counter" } else if i % 3 == 1 { "gauge" } else { "histogram" }))
    Attributes::set(attrs, "service", StringValue(if i % 2 == 0 { "auth" } else { "api" }))
    Attributes::set(attrs, "environment", StringValue(if i % 4 == 0 { "production" } else if i % 4 == 1 { "staging" } else { "development" }))
    Attributes::set(attrs, "region", StringValue(["us-east", "us-west", "eu-west", "ap-southeast"][i % 4]))
    
    telemetry_data.push(TelemetryData::new(
      (i * 10.0),
      attrs,
      1234567890L + i
    ))
  }
  
  // Test complex filter conditions
  let complex_filter = FilterCondition::and([
    FilterCondition::equals("environment", "production"),
    FilterCondition::or([
      FilterCondition::equals("metric_type", "counter"),
      FilterCondition::equals("metric_type", "gauge")
    ]),
    FilterCondition::not_equals("region", "ap-southeast"),
    FilterCondition::greater_than("value", 100.0)
  ])
  
  let filtered_data = DataFilter::apply(filter, telemetry_data, complex_filter)
  
  // Verify filtered results
  for item in filtered_data {
    let attrs = TelemetryData::attributes(item)
    
    // Check environment is production
    let env = Attributes::get(attrs, "environment")
    match env {
      Some(StringValue(value)) => assert_eq(value, "production")
      _ => assert_true(false)
    }
    
    // Check metric type is counter or gauge
    let metric_type = Attributes::get(attrs, "metric_type")
    match metric_type {
      Some(StringValue(value)) => assert_true(value == "counter" || value == "gauge")
      _ => assert_true(false)
    }
    
    // Check region is not ap-southeast
    let region = Attributes::get(attrs, "region")
    match region {
      Some(StringValue(value)) => assert_true(value != "ap-southeast")
      _ => assert_true(false)
    }
    
    // Check value is greater than 100
    assert_true(TelemetryData::value(item) > 100.0)
  }
}

// Test 7: Data Aggregation with Multiple Dimensions
test "data aggregation with multiple dimensions" {
  let aggregator = MultiDimensionalAggregator::new()
  
  // Create multi-dimensional telemetry data
  let telemetry_data = []
  
  for i in 1..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "service", StringValue(["auth", "api", "db", "cache"][i % 4]))
    Attributes::set(attrs, "operation", StringValue(["read", "write", "delete"][i % 3]))
    Attributes::set(attrs, "status", StringValue(if i % 10 == 0 { "error" } else { "success" }))
    Attributes::set(attrs, "user_tier", StringValue(["free", "premium", "enterprise"][i % 3]))
    
    telemetry_data.push(TelemetryData::new(
      (i * 5.0), // Response time in ms
      attrs,
      1234567890L + i
    ))
  }
  
  // Define aggregation dimensions
  let dimensions = ["service", "operation", "status", "user_tier"]
  
  // Aggregate by all dimensions
  let full_aggregation = MultiDimensionalAggregator::aggregate(
    aggregator,
    telemetry_data,
    dimensions
  )
  
  // Should have 4 * 3 * 2 * 3 = 72 unique combinations
  assert_eq(full_aggregation.length(), 72)
  
  // Find aggregation for specific combination
  let auth_read_success = MultiDimensionalAggregator::find_combination(
    full_aggregation,
    [
      ("service", "auth"),
      ("operation", "read"),
      ("status", "success"),
      ("user_tier", "free")
    ]
  )
  
  match auth_read_success {
    Some(aggregation) => {
      assert_true(AggregationData::count(aggregation) > 0)
      assert_true(AggregationData::average(aggregation) > 0)
    }
    None => assert_true(false)
  }
  
  // Aggregate by subset of dimensions
  let partial_aggregation = MultiDimensionalAggregator::aggregate(
    aggregator,
    telemetry_data,
    ["service", "status"]
  )
  
  // Should have 4 * 2 = 8 unique combinations
  assert_eq(partial_aggregation.length(), 8)
  
  // Calculate rollup statistics
  let rollup_stats = MultiDimensionalAggregator::calculate_rollup(
    aggregator,
    full_aggregation,
    ["service"]
  )
  
  assert_eq(rollup_stats.length(), 4) // 4 services
  
  // Check service-level statistics
  let auth_stats = MultiDimensionalAggregator::find_by_dimension(
    rollup_stats,
    "service",
    "auth"
  )
  
  match auth_stats {
    Some(stats) => {
      assert_true(AggregationData::count(stats) > 0)
      assert_true(AggregationData::sum(stats) > 0)
    }
    None => assert_true(false)
  }
}

// Test 8: Data Export in Multiple Formats
test "data export in multiple formats" {
  let exporter = DataExporter::new()
  
  // Create sample telemetry data
  let telemetry_data = []
  
  for i in 1..=10 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "trace_id", StringValue("trace_" + i.to_string()))
    Attributes::set(attrs, "span_id", StringValue("span_" + i.to_string()))
    Attributes::set(attrs, "service", StringValue("test_service"))
    Attributes::set(attrs, "operation", StringValue("test_operation"))
    
    telemetry_data.push(TelemetryData::new(
      (i * 10.0),
      attrs,
      1234567890L + i
    ))
  }
  
  // Export to JSON
  let json_export = DataExporter::to_json(exporter, telemetry_data)
  assert_true(json_export.length() > 0)
  assert_true(json_export.contains("\"service\":\"test_service\""))
  assert_true(json_export.contains("\"operation\":\"test_operation\""))
  
  // Export to CSV
  let csv_export = DataExporter::to_csv(exporter, telemetry_data)
  assert_true(csv_export.length() > 0)
  assert_true(csv_export.contains("trace_id,span_id,service,operation,value,timestamp"))
  assert_true(csv_export.contains("trace_1,span_1,test_service,test_operation,10.0,1234567891"))
  
  // Export to Prometheus format
  let prometheus_export = DataExporter::to_prometheus(exporter, telemetry_data)
  assert_true(prometheus_export.length() > 0)
  assert_true(prometheus_export.contains("# HELP"))
  assert_true(prometheus_export.contains("# TYPE"))
  
  // Export to OpenTelemetry format
  let otlp_export = DataExporter::to_otlp(exporter, telemetry_data)
  assert_true(otlp_export.length() > 0)
  
  // Verify export integrity by re-importing
  let json_imported = DataImporter::from_json(exporter, json_export)
  assert_eq(json_imported.length(), 10)
  
  let csv_imported = DataImporter::from_csv(exporter, csv_export)
  assert_eq(csv_imported.length(), 10)
  
  // Verify data consistency across formats
  for i in 0..9 {
    let original = telemetry_data[i]
    let json_item = json_imported[i]
    let csv_item = csv_imported[i]
    
    assert_eq(TelemetryData::value(original), TelemetryData::value(json_item))
    assert_eq(TelemetryData::value(original), TelemetryData::value(csv_item))
    assert_eq(TelemetryData::timestamp(original), TelemetryData::timestamp(json_item))
    assert_eq(TelemetryData::timestamp(original), TelemetryData::timestamp(csv_item))
  }
}