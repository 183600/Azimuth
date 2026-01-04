// Azimuth High-Quality Data Processing and Validation Tests
// This file contains comprehensive test cases for data processing and validation

// Test 1: Telemetry Data Validation
test "telemetry data validation" {
  // Create validator with comprehensive rules
  let validator = TelemetryDataValidator::new()
  
  // Add validation rules
  validator.add_rule("response_time", NumericRangeRule::new(0.0, 10000.0))
  validator.add_rule("error_rate", PercentageRule::new(0.0, 1.0))
  validator.add_rule("status_code", StatusCodeRule::new([200, 201, 400, 401, 404, 500]))
  validator.add_rule("service_name", NonEmptyStringRule::new())
  validator.add_rule("timestamp", TimestampRule::new())
  
  // Test valid data
  let valid_data = TelemetryData::new()
    .with_metric("response_time", 150.5)
    .with_metric("error_rate", 0.02)
    .with_metric("status_code", 200)
    .with_metric("service_name", "auth-service")
    .with_metric("timestamp", 1704067200000) // 2024-01-01 00:00:00 UTC
  
  let validation_result = validator.validate(valid_data)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test invalid data
  let invalid_data = TelemetryData::new()
    .with_metric("response_time", -10.0) // Invalid: negative
    .with_metric("error_rate", 1.5) // Invalid: > 1.0
    .with_metric("status_code", 999) // Invalid: not in allowed list
    .with_metric("service_name", "") // Invalid: empty
    .with_metric("timestamp", -1) // Invalid: negative
  
  let invalid_result = validator.validate(invalid_data)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() >= 4) // At least 4 errors
  
  // Verify specific error messages
  let error_messages = invalid_result.errors.map(|e| e.message)
  assert_true(error_messages.any(|m| m.contains("response_time")))
  assert_true(error_messages.any(|m| m.contains("error_rate")))
  assert_true(error_messages.any(|m| m.contains("status_code")))
  assert_true(error_messages.any(|m| m.contains("service_name")))
}

// Test 2: Data Type Conversion and Coercion
test "data type conversion and coercion" {
  let converter = DataTypeConverter::new()
  
  // Test string to numeric conversion
  let string_int = "42"
  let converted_int = converter.to_int(string_int)
  assert_true(converted_int.is_ok())
  assert_eq(converted_int.unwrap(), 42)
  
  let string_float = "3.14159"
  let converted_float = converter.to_float(string_float)
  assert_true(converted_float.is_ok())
  assert_true(abs(converted_float.unwrap() - 3.14159) < 0.00001)
  
  // Test invalid conversions
  let invalid_int = "not_a_number"
  let invalid_int_result = converter.to_int(invalid_int)
  assert_true(invalid_int_result.is_error())
  
  // Test numeric to string conversion
  let number = 123
  let string_result = converter.to_string(number)
  assert_eq(string_result, "123")
  
  // Test boolean conversion
  let string_true = "true"
  let converted_true = converter.to_bool(string_true)
  assert_true(converted_true.is_ok())
  assert_true(converted_true.unwrap())
  
  let string_false = "false"
  let converted_false = converter.to_bool(string_false)
  assert_true(converted_false.is_ok())
  assert_false(converted_false.unwrap())
  
  // Test array conversion
  let string_array = "[1, 2, 3, 4, 5]"
  let converted_array = converter.to_int_array(string_array)
  assert_true(converted_array.is_ok())
  assert_eq(converted_array.unwrap(), [1, 2, 3, 4, 5])
  
  // Test type coercion with fallback values
  let fallback_result = converter.to_int_with_fallback("invalid", 0)
  assert_eq(fallback_result, 0)
}

// Test 3: Data Cleaning and Normalization
test "data cleaning and normalization" {
  let cleaner = DataCleaner::new()
  
  // Test whitespace cleaning
  let dirty_string = "  hello world  "
  let cleaned_string = cleaner.clean_whitespace(dirty_string)
  assert_eq(cleaned_string, "hello world")
  
  // Test null/empty value handling
  let null_value = None
  let cleaned_null = cleaner.handle_null_values(null_value, "default")
  assert_eq(cleaned_null, "default")
  
  let some_value = Some("actual")
  let cleaned_some = cleaner.handle_null_values(some_value, "default")
  assert_eq(cleaned_some, "actual")
  
  // Test outlier detection and removal
  let data_with_outliers = [10, 12, 15, 18, 20, 22, 25, 28, 30, 150] // 150 is an outlier
  let cleaned_data = cleaner.remove_outliers(data_with_outliers, OutlierDetectionMethod::IQR)
  
  assert_false(cleaned_data.contains(150))
  assert_eq(cleaned_data.length(), 9)
  
  // Test data normalization
  let unnormalized_data = [10, 20, 30, 40, 50]
  let normalized_data = cleaner.normalize_data(unnormalized_data, NormalizationMethod::Min_Max)
  
  assert_eq(normalized_data[0], 0.0) // Min value becomes 0
  assert_eq(normalized_data[4], 1.0) // Max value becomes 1
  
  // Verify all values are between 0 and 1
  for value in normalized_data {
    assert_true(value >= 0.0 && value <= 1.0)
  }
  
  // Test data standardization
  let standardized_data = cleaner.standardize_data(unnormalized_data)
  
  // Verify mean is approximately 0 and standard deviation is approximately 1
  let mean = standardized_data.reduce(0.0, |a, b| a + b) / standardized_data.length()
  assert_true(abs(mean) < 0.001) // Mean should be close to 0
  
  let variance = standardized_data
    .map(|x| (x - mean) * (x - mean))
    .reduce(0.0, |a, b| a + b) / standardized_data.length()
  let std_dev = sqrt(variance)
  assert_true(abs(std_dev - 1.0) < 0.001) // Std dev should be close to 1
}

// Test 4: Data Aggregation and Grouping
test "data aggregation and grouping" {
  let aggregator = DataAggregator::new()
  
  // Create sample telemetry data
  let telemetry_data = [
    TelemetryPoint::new("service-a", "response_time", 100, 1704067200),
    TelemetryPoint::new("service-a", "response_time", 120, 1704067260),
    TelemetryPoint::new("service-a", "response_time", 110, 1704067320),
    TelemetryPoint::new("service-b", "response_time", 200, 1704067200),
    TelemetryPoint::new("service-b", "response_time", 180, 1704067260),
    TelemetryPoint::new("service-a", "error_rate", 0.01, 1704067200),
    TelemetryPoint::new("service-a", "error_rate", 0.02, 1704067260),
    TelemetryPoint::new("service-b", "error_rate", 0.05, 1704067200)
  ]
  
  // Test grouping by service
  let grouped_by_service = aggregator.group_by(telemetry_data, "service_name")
  
  assert_true(grouped_by_service.contains_key("service-a"))
  assert_true(grouped_by_service.contains_key("service-b"))
  assert_eq(grouped_by_service.get("service-a").length(), 5)
  assert_eq(grouped_by_service.get("service-b").length(), 3)
  
  // Test grouping by metric
  let grouped_by_metric = aggregator.group_by(telemetry_data, "metric_name")
  
  assert_true(grouped_by_metric.contains_key("response_time"))
  assert_true(grouped_by_metric.contains_key("error_rate"))
  assert_eq(grouped_by_metric.get("response_time").length(), 5)
  assert_eq(grouped_by_metric.get("error_rate").length(), 3)
  
  // Test aggregation functions
  let service_a_response_times = grouped_by_service
    .get("service-a")
    .filter(|p| p.metric_name == "response_time")
    .map(|p| p.value)
  
  let avg_response_time = aggregator.aggregate(service_a_response_times, AggregationFunction::Average)
  let min_response_time = aggregator.aggregate(service_a_response_times, AggregationFunction::Min)
  let max_response_time = aggregator.aggregate(service_a_response_times, AggregationFunction::Max)
  let sum_response_time = aggregator.aggregate(service_a_response_times, AggregationFunction::Sum)
  
  assert_eq(avg_response_time, 110.0) // (100 + 120 + 110) / 3
  assert_eq(min_response_time, 100.0)
  assert_eq(max_response_time, 120.0)
  assert_eq(sum_response_time, 330.0)
  
  // Test time-based aggregation
  let hourly_aggregates = aggregator.aggregate_by_time_window(
    telemetry_data,
    TimeWindow::Hourly,
    AggregationFunction::Average
  )
  
  assert_true(hourly_aggregates.length() > 0)
  
  for aggregate in hourly_aggregates {
    assert_true(aggregate.timestamp > 0)
    assert_true(aggregate.value >= 0.0)
    assert_true(aggregate.service_name.length() > 0)
    assert_true(aggregate.metric_name.length() > 0)
  }
}

// Test 5: Data Transformation and Enrichment
test "data transformation and enrichment" {
  let transformer = DataTransformer::new()
  
  // Create base telemetry data
  let base_data = TelemetryData::new()
    .with_metric("cpu_usage", 75.5)
    .with_metric("memory_usage", 60.2)
    .with_metric("request_count", 1000)
    .with_metric("error_count", 50)
    .with_metric("service_name", "api-service")
    .with_metric("timestamp", 1704067200)
  
  // Test derived metrics calculation
  transformer.add_derived_metric("error_rate", |data| {
    let error_count = data.get_metric("error_count")
    let request_count = data.get_metric("request_count")
    if request_count > 0 {
      error_count / request_count
    } else {
      0.0
    }
  })
  
  transformer.add_derived_metric("resource_utilization", |data| {
    let cpu_usage = data.get_metric("cpu_usage")
    let memory_usage = data.get_metric("memory_usage")
    (cpu_usage + memory_usage) / 2.0
  })
  
  let enriched_data = transformer.enrich(base_data)
  
  assert_true(enriched_data.has_metric("error_rate"))
  assert_true(enriched_data.has_metric("resource_utilization"))
  
  let error_rate = enriched_data.get_metric("error_rate")
  let resource_utilization = enriched_data.get_metric("resource_utilization")
  
  assert_eq(error_rate, 0.05) // 50 / 1000
  assert_eq(resource_utilization, 67.85) // (75.5 + 60.2) / 2
  
  // Test data filtering
  let filter = DataFilter::new()
    .add_condition("cpu_usage", FilterOperator::GreaterThan, 50.0)
    .add_condition("error_rate", FilterOperator::LessThan, 0.1)
  
  let is_filtered = filter.matches(enriched_data)
  assert_true(is_filtered)
  
  // Test data mapping
  let mapper = DataMapper::new()
    .add_mapping("cpu_usage", "processor_utilization_percent")
    .add_mapping("memory_usage", "ram_utilization_percent")
    .add_mapping("service_name", "component_identifier")
  
  let mapped_data = mapper.map(enriched_data)
  
  assert_true(mapped_data.has_metric("processor_utilization_percent"))
  assert_true(mapped_data.has_metric("ram_utilization_percent"))
  assert_true(mapped_data.has_metric("component_identifier"))
  
  assert_eq(mapped_data.get_metric("processor_utilization_percent"), 75.5)
  assert_eq(mapped_data.get_metric("ram_utilization_percent"), 60.2)
  assert_eq(mapped_data.get_string_metric("component_identifier"), "api-service")
}

// Test 6: Batch Processing and Streaming Data
test "batch processing and streaming data" {
  let processor = DataProcessor::new()
  
  // Test batch processing
  let batch_data = generate_telemetry_batch(1000)
  let batch_config = BatchProcessingConfig::new()
    .with_batch_size(100)
    .with_max_concurrency(4)
  
  let batch_results = processor.process_batch(batch_data, batch_config)
  
  assert_eq(batch_results.processed_count, 1000)
  assert_eq(batch_results.error_count, 0)
  assert_true(batch_results.processing_time_ms > 0)
  
  // Test streaming data processing
  let stream_processor = StreamDataProcessor::new()
  let stream_config = StreamProcessingConfig::new()
    .with_window_size(Duration::from_seconds(60))
    .with_watermark(Duration::from_seconds(10))
  
  let processed_items = []
  
  // Simulate streaming data
  for i in 0..=99 {
    let stream_item = generate_streaming_telemetry_item(i)
    let processed_item = stream_processor.process_item(stream_item, stream_config)
    processed_items.push(processed_item)
  }
  
  assert_eq(processed_items.length(), 100)
  
  // Verify all items were processed
  for item in processed_items {
    assert_true(item.is_processed)
    assert_true(item.processing_timestamp > 0)
  }
  
  // Test windowed aggregation
  let windowed_results = stream_processor.get_windowed_results()
  
  assert_true(windowed_results.length() > 0)
  
  for window in windowed_results {
    assert_true(window.start_time > 0)
    assert_true(window.end_time >= window.start_time)
    assert_true(window.aggregated_metrics.length() > 0)
  }
}

// Test 7: Data Quality Assessment
test "data quality assessment" {
  let quality_assessor = DataQualityAssessor::new()
  
  // Create data with various quality issues
  let high_quality_data = generate_high_quality_telemetry(100)
  let low_quality_data = generate_low_quality_telemetry(100)
  
  // Assess high quality data
  let high_quality_score = quality_assessor.assess(high_quality_data)
  assert_true(high_quality_score.overall_score >= 0.8)
  assert_true(high_quality_score.completeness_score >= 0.9)
  assert_true(high_quality_score.accuracy_score >= 0.8)
  assert_true(high_quality_score.consistency_score >= 0.8)
  assert_true(high_quality_score.timeliness_score >= 0.7)
  
  // Assess low quality data
  let low_quality_score = quality_assessor.assess(low_quality_data)
  assert_true(low_quality_score.overall_score <= 0.6)
  assert_true(low_quality_score.completeness_score <= 0.7)
  assert_true(low_quality_score.accuracy_score <= 0.6)
  assert_true(low_quality_score.consistency_score <= 0.7)
  
  // Test quality dimensions
  let quality_dimensions = quality_assessor.get_quality_dimensions(low_quality_data)
  
  assert_true(quality_dimensions.contains_key("completeness"))
  assert_true(quality_dimensions.contains_key("accuracy"))
  assert_true(quality_dimensions.contains_key("consistency"))
  assert_true(quality_dimensions.contains_key("timeliness"))
  assert_true(quality_dimensions.contains_key("validity"))
  assert_true(quality_dimensions.contains_key("uniqueness"))
  
  // Verify dimension scores
  for (dimension, score) in quality_dimensions {
    assert_true(score >= 0.0 && score <= 1.0)
  }
  
  // Test quality improvement recommendations
  let recommendations = quality_assessor.get_improvement_recommendations(low_quality_data)
  
  assert_true(recommendations.length() > 0)
  
  for recommendation in recommendations {
    assert_true(recommendation.issue.length() > 0)
    assert_true(recommendation.recommendation.length() > 0)
    assert_true(recommendation.priority >= 1 && recommendation.priority <= 5)
    assert_true(recommendation.estimated_impact > 0.0)
  }
}

// Test 8: Data Schema Validation
test "data schema validation" {
  let schema_validator = SchemaValidator::new()
  
  // Define telemetry data schema
  let telemetry_schema = Schema::new("telemetry")
    .with_field("service_name", FieldType::String, FieldConstraint::Required)
    .with_field("metric_name", FieldType::String, FieldConstraint::Required)
    .with_field("value", FieldType::Float, FieldConstraint::Required)
    .with_field("timestamp", FieldType::Timestamp, FieldConstraint::Required)
    .with_field("tags", FieldType::Object, FieldConstraint::Optional)
    .with_field("unit", FieldType::String, FieldConstraint::Optional)
  
  schema_validator.register_schema(telemetry_schema)
  
  // Test valid data against schema
  let valid_data = DataObject::new()
    .with_field("service_name", "auth-service")
    .with_field("metric_name", "response_time")
    .with_field("value", 150.5)
    .with_field("timestamp", 1704067200000)
    .with_field("unit", "ms")
  
  let valid_result = schema_validator.validate("telemetry", valid_data)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid data against schema
  let invalid_data = DataObject::new()
    .with_field("service_name", "auth-service")
    // Missing required field: metric_name
    .with_field("value", 150.5)
    .with_field("timestamp", 1704067200000)
    .with_field("unit", 123) // Wrong type: should be string
  
  let invalid_result = schema_validator.validate("telemetry", invalid_data)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() >= 2) // At least 2 errors
  
  // Test schema evolution
  let evolved_schema = Schema::new("telemetry_v2")
    .with_field("service_name", FieldType::String, FieldConstraint::Required)
    .with_field("metric_name", FieldType::String, FieldConstraint::Required)
    .with_field("value", FieldType::Float, FieldConstraint::Required)
    .with_field("timestamp", FieldType::Timestamp, FieldConstraint::Required)
    .with_field("tags", FieldType::Object, FieldConstraint::Optional)
    .with_field("unit", FieldType::String, FieldConstraint::Optional)
    .with_field("version", FieldType::String, FieldConstraint::Required) // New required field
    .with_field("metadata", FieldType::Object, FieldConstraint::Optional) // New optional field
  
  schema_validator.register_schema(evolved_schema)
  
  // Test backward compatibility
  let compatibility_result = schema_validator.check_backward_compatibility("telemetry", "telemetry_v2")
  assert_true(compatibility_result.is_compatible)
  
  // Test forward compatibility
  let forward_compatibility_result = schema_validator.check_forward_compatibility("telemetry", "telemetry_v2")
  assert_false(forward_compatibility_result.is_compatible) // New required field breaks forward compatibility
}

// Test 9: Data Pipeline Processing
test "data pipeline processing" {
  let pipeline = DataPipeline::new("telemetry_processing_pipeline")
  
  // Add pipeline stages
  pipeline.add_stage("validation", ValidationStage::new())
  pipeline.add_stage("cleaning", CleaningStage::new())
  pipeline.add_stage("enrichment", EnrichmentStage::new())
  pipeline.add_stage("aggregation", AggregationStage::new())
  pipeline.add_stage("output", OutputStage::new())
  
  // Configure pipeline
  let pipeline_config = PipelineConfig::new()
    .with_parallelism(4)
    .with_error_handling(ErrorHandlingStrategy::Retry)
    .with_max_retries(3)
    .with_retry_delay(Duration::from_millis(100))
  
  pipeline.configure(pipeline_config)
  
  // Process data through pipeline
  let input_data = generate_pipeline_input_data(100)
  let pipeline_result = pipeline.process(input_data)
  
  assert_true(pipeline_result.is_success)
  assert_eq(pipeline_result.processed_count, 100)
  assert_eq(pipeline_result.error_count, 0)
  
  // Verify stage execution
  let stage_metrics = pipeline.get_stage_metrics()
  
  assert_true(stage_metrics.contains_key("validation"))
  assert_true(stage_metrics.contains_key("cleaning"))
  assert_true(stage_metrics.contains_key("enrichment"))
  assert_true(stage_metrics.contains_key("aggregation"))
  assert_true(stage_metrics.contains_key("output"))
  
  for (stage_name, metrics) in stage_metrics {
    assert_true(metrics.input_count >= metrics.output_count)
    assert_true(metrics.processing_time_ms >= 0)
    assert_true(metrics.error_count >= 0)
  }
  
  // Test pipeline monitoring
  let pipeline_health = pipeline.get_health_status()
  assert_eq(pipeline_health.status, "healthy")
  assert_true(pipeline_health.uptime_ms > 0)
  assert_true(pipeline_health.throughput_per_second > 0)
  
  // Test pipeline scaling
  let scaled_config = PipelineConfig::new()
    .with_parallelism(8) // Double the parallelism
    .with_error_handling(ErrorHandlingStrategy::Retry)
    .with_max_retries(3)
    .with_retry_delay(Duration::from_millis(100))
  
  pipeline.reconfigure(scaled_config)
  
  let scaled_result = pipeline.process(input_data)
  assert_true(scaled_result.is_success)
  
  // Verify improved throughput
  let scaled_metrics = pipeline.get_pipeline_metrics()
  assert_true(scaled_metrics.throughput_per_second >= pipeline_result.throughput_per_second)
}

// Test 10: Data Retention and Archival
test "data retention and archival" {
  let retention_manager = DataRetentionManager::new()
  
  // Configure retention policies
  let retention_policy = RetentionPolicy::new("telemetry_data")
    .with_retention_period(Duration::from_days(30))
    .with_archival_period(Duration::from_days(7))
    .with_compression_enabled(true)
    .with_encryption_enabled(true)
  
  retention_manager.add_policy(retention_policy)
  
  // Create test data with various ages
  let current_time = get_current_timestamp()
  let old_data = create_test_data_with_timestamp(current_time - Duration::from_days(40)) // 40 days old
  let recent_data = create_test_data_with_timestamp(current_time - Duration::from_days(5)) // 5 days old
  let archival_data = create_test_data_with_timestamp(current_time - Duration::from_days(10)) // 10 days old
  
  // Store test data
  retention_manager.store_data(old_data)
  retention_manager.store_data(recent_data)
  retention_manager.store_data(archival_data)
  
  // Run retention enforcement
  let retention_result = retention_manager.enforce_retention()
  
  assert_true(retention_result.is_success)
  assert_true(retention_result.deleted_count > 0)
  assert_true(retention_result.archived_count > 0)
  
  // Verify data states
  let old_data_status = retention_manager.get_data_status(old_data.id)
  assert_eq(old_data_status.status, "deleted")
  
  let recent_data_status = retention_manager.get_data_status(recent_data.id)
  assert_eq(recent_data_status.status, "active")
  
  let archival_data_status = retention_manager.get_data_status(archival_data.id)
  assert_eq(archival_data_status.status, "archived")
  
  // Test data retrieval from archive
  if archival_data_status.status == "archived" {
    let retrieved_data = retention_manager.retrieve_from_archive(archival_data.id)
    assert_true(retrieved_data.is_some())
    assert_eq(retrieved_data.unwrap().id, archival_data.id)
  }
  
  // Test retention metrics
  let retention_metrics = retention_manager.get_retention_metrics()
  
  assert_true(retention_metrics.total_data_size > 0)
  assert_true(retention_metrics.active_data_size > 0)
  assert_true(retention_metrics.archived_data_size > 0)
  assert_true(retention_metrics.deleted_data_size >= 0)
  assert_true(retention_metrics.compression_ratio > 1.0)
  
  // Test custom retention rules
  let custom_rule = RetentionRule::new("important_metrics")
    .with_condition("metric_name", "equals", "critical_error_rate")
    .with_retention_period(Duration::from_days(90)) // Extended retention
    .with_priority(10) // High priority
  
  retention_manager.add_rule(custom_rule)
  
  let important_data = create_test_data_with_metric("critical_error_rate")
  retention_manager.store_data(important_data)
  
  let custom_retention_result = retention_manager.enforce_retention_with_rules()
  assert_true(custom_retention_result.is_success)
  
  let important_data_status = retention_manager.get_data_status(important_data.id)
  assert_eq(important_data_status.status, "active") // Should be kept due to custom rule
}