// Azimuth Telemetry System - Data Consistency Comprehensive Tests
// This file contains comprehensive tests for data consistency across the telemetry system

// Test 1: Attribute Value Consistency Across Operations
test "attribute value consistency across operations" {
  let attributes = Attributes::new()
  
  // Test string attribute consistency
  Attributes::set(attributes, "test.string", StringValue("original_value"))
  let retrieved = Attributes::get(attributes, "test.string")
  
  match retrieved {
    Some(StringValue(value)) => {
      assert_eq(value, "original_value")
      
      // Update and verify consistency
      Attributes::set(attributes, "test.string", StringValue("updated_value"))
      let updated = Attributes::get(attributes, "test.string")
      match updated {
        Some(StringValue(updated_value)) => assert_eq(updated_value, "updated_value")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test numeric attribute consistency
  Attributes::set(attributes, "test.int", IntValue(42))
  let int_attr = Attributes::get(attributes, "test.int")
  match int_attr {
    Some(IntValue(value)) => {
      assert_eq(value, 42)
      
      // Verify type consistency
      Attributes::set(attributes, "test.int", IntValue(100))
      let updated_int = Attributes::get(attributes, "test.int")
      match updated_int {
        Some(IntValue(updated_value)) => assert_eq(updated_value, 100)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test array attribute consistency
  let original_array = ["item1", "item2", "item3"]
  Attributes::set(attributes, "test.array", ArrayStringValue(original_array))
  let array_attr = Attributes::get(attributes, "test.array")
  match array_attr {
    Some(ArrayStringValue(value)) => {
      assert_eq(value.length(), 3)
      assert_eq(value[0], "item1")
      assert_eq(value[1], "item2")
      assert_eq(value[2], "item3")
      
      // Update array and verify consistency
      let updated_array = ["new_item1", "new_item2"]
      Attributes::set(attributes, "test.array", ArrayStringValue(updated_array))
      let updated_array_attr = Attributes::get(attributes, "test.array")
      match updated_array_attr {
        Some(ArrayStringValue(updated_value)) => {
          assert_eq(updated_value.length(), 2)
          assert_eq(updated_value[0], "new_item1")
          assert_eq(updated_value[1], "new_item2")
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// Test 2: Span Context Consistency in Distributed Tracing
test "span context consistency in distributed tracing" {
  let original_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let original_span_id = "b7ad6b7169203331"
  
  // Create parent span context
  let parent_ctx = SpanContext::new(original_trace_id, original_span_id, true, "test_state")
  
  // Create child span and verify trace consistency
  let child_span_id = "c7ad6b7169203332"
  let child_ctx = SpanContext::new(original_trace_id, child_span_id, true, "test_state")
  
  // Verify trace ID consistency across parent and child
  assert_eq(SpanContext::trace_id(parent_ctx), SpanContext::trace_id(child_ctx))
  assert_eq(SpanContext::trace_id(parent_ctx), original_trace_id)
  
  // Verify span ID uniqueness
  assert_not_eq(SpanContext::span_id(parent_ctx), SpanContext::span_id(child_ctx))
  
  // Test context propagation consistency
  let propagated_ctx = SpanContext::from_parent(parent_ctx, child_span_id)
  assert_eq(SpanContext::trace_id(propagated_ctx), original_trace_id)
  assert_eq(SpanContext::span_id(propagated_ctx), child_span_id)
  assert_eq(SpanContext::is_sampled(propagated_ctx), SpanContext::is_sampled(parent_ctx))
  
  // Test serialization/deserialization consistency
  let serialized = SpanContext::to_string(parent_ctx)
  let deserialized = SpanContext::from_string(serialized)
  
  assert_eq(SpanContext::trace_id(deserialized), SpanContext::trace_id(parent_ctx))
  assert_eq(SpanContext::span_id(deserialized), SpanContext::span_id(parent_ctx))
  assert_eq(SpanContext::is_sampled(deserialized), SpanContext::is_sampled(parent_ctx))
}

// Test 3: Metric Aggregation Consistency
test "metric aggregation consistency" {
  let aggregator = TelemetryAggregator::new()
  
  // Add data points with consistent values
  let data_points = [
    DataPoint::new("test.metric", 10.0, 1000),
    DataPoint::new("test.metric", 20.0, 2000),
    DataPoint::new("test.metric", 30.0, 3000),
    DataPoint::new("test.metric", 40.0, 4000),
    DataPoint::new("test.metric", 50.0, 5000)
  ]
  
  // Add points to aggregator
  for point in data_points {
    TelemetryAggregator::add_point(aggregator, point)
  }
  
  // Verify sum consistency
  let sum_result = TelemetryAggregator::sum(aggregator, "test.metric")
  match sum_result {
    Some(sum) => assert_eq(sum, 150.0)
    None => assert_true(false)
  }
  
  // Verify average consistency
  let avg_result = TelemetryAggregator::average(aggregator, "test.metric")
  match avg_result {
    Some(avg) => assert_eq(avg, 30.0)
    None => assert_true(false)
  }
  
  // Verify min/max consistency
  let min_result = TelemetryAggregator::min(aggregator, "test.metric")
  match min_result {
    Some(min) => assert_eq(min, 10.0)
    None => assert_true(false)
  }
  
  let max_result = TelemetryAggregator::max(aggregator, "test.metric")
  match max_result {
    Some(max) => assert_eq(max, 50.0)
    None => assert_true(false)
  }
  
  // Verify count consistency
  let count_result = TelemetryAggregator::count(aggregator, "test.metric")
  match count_result {
    Some(count) => assert_eq(count, 5)
    None => assert_true(false)
  }
  
  // Test histogram consistency
  let histogram_result = TelemetryAggregator::histogram(aggregator, "test.metric", 5)
  match histogram_result {
    Some(buckets) => {
      assert_eq(buckets.length(), 5)
      // Verify bucket boundaries and counts
      let expected_counts = [1, 1, 1, 1, 1] // One value in each bucket
      for i in 0..=4 {
        assert_eq(buckets[i].count, expected_counts[i])
      }
    }
    None => assert_true(false)
  }
}

// Test 4: Log Record Consistency
test "log record consistency" {
  let logger = Logger::new("test.logger")
  
  // Create log record with consistent attributes
  let attributes = [
    ("service.name", StringValue("test.service")),
    ("operation.name", StringValue("test.operation")),
    ("trace.id", StringValue("trace_123")),
    ("span.id", StringValue("span_456"))
  ]
  
  let log_record = LogRecord::new_with_attributes(
    Info,
    "Test log message",
    attributes,
    Some(1234567890L),
    Some(1234567891L),
    Some("trace_123"),
    Some("span_456")
  )
  
  // Verify log record consistency
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false)
  }
  
  // Verify attribute consistency
  let service_name = LogRecord::get_attribute(log_record, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test.service")
    _ => assert_true(false)
  }
  
  // Verify trace context consistency
  assert_eq(LogRecord::trace_id(log_record), Some("trace_123"))
  assert_eq(LogRecord::span_id(log_record), Some("span_456"))
  
  // Test log serialization consistency
  let serialized = LogRecord::to_json(log_record)
  let deserialized = LogRecord::from_json(serialized)
  
  assert_eq(LogRecord::severity_number(deserialized), LogRecord::severity_number(log_record))
  assert_eq(LogRecord::body(deserialized), LogRecord::body(log_record))
  assert_eq(LogRecord::trace_id(deserialized), LogRecord::trace_id(log_record))
  assert_eq(LogRecord::span_id(deserialized), LogRecord::span_id(log_record))
}

// Test 5: Resource Consistency Across Operations
test "resource consistency across operations" {
  let base_resource = Resource::new()
  
  // Add attributes to resource
  let attributes = [
    ("service.name", StringValue("test.service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(base_resource, attributes)
  
  // Verify attribute consistency
  for (key, expected_value) in attributes {
    let actual_value = Resource::get_attribute(resource_with_attrs, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // Test resource merging consistency
  let override_attrs = [
    ("service.version", StringValue("2.0.0")), // Override existing
    ("environment", StringValue("production")) // Add new
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // Verify merged resource consistency
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test.service") // Should be preserved
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "2.0.0") // Should be overridden
    _ => assert_true(false)
  }
  
  let environment = Resource::get_attribute(merged_resource, "environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "production") // Should be added
    _ => assert_true(false)
  }
  
  // Test resource serialization consistency
  let serialized = Resource::to_json(merged_resource)
  let deserialized = Resource::from_json(serialized)
  
  // Verify all attributes are preserved after serialization
  let original_service_name = Resource::get_attribute(merged_resource, "service.name")
  let deserialized_service_name = Resource::get_attribute(deserialized, "service.name")
  assert_eq(original_service_name, deserialized_service_name)
  
  let original_service_version = Resource::get_attribute(merged_resource, "service.version")
  let deserialized_service_version = Resource::get_attribute(deserialized, "service.version")
  assert_eq(original_service_version, deserialized_service_version)
}

// Test 6: Baggage Consistency Across Context Propagation
test "baggage consistency across context propagation" {
  let initial_baggage = Baggage::new()
  
  // Add baggage entries
  let entries = [
    ("user.id", "user-123"),
    ("request.id", "req-456"),
    ("session.id", "session-789")
  ]
  
  let baggage_with_entries = Baggage::with_entries(initial_baggage, entries)
  
  // Verify baggage entry consistency
  for (key, expected_value) in entries {
    let actual_value = Baggage::get_entry(baggage_with_entries, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // Test baggage propagation consistency
  let propagated_baggage = Baggage::propagate(baggage_with_entries)
  
  // Verify all entries are preserved during propagation
  for (key, expected_value) in entries {
    let propagated_value = Baggage::get_entry(propagated_baggage, key)
    match propagated_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // Test baggage serialization consistency
  let serialized = Baggage::to_string(baggage_with_entries)
  let deserialized = Baggage::from_string(serialized)
  
  // Verify all entries are preserved after serialization
  for (key, expected_value) in entries {
    let deserialized_value = Baggage::get_entry(deserialized, key)
    match deserialized_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // Test baggage removal consistency
  let baggage_after_removal = Baggage::remove_entry(baggage_with_entries, "request.id")
  
  // Verify entry is removed
  let removed_entry = Baggage::get_entry(baggage_after_removal, "request.id")
  match removed_entry {
    Some(_) => assert_true(false) // Should be None
    None => assert_true(true)
  }
  
  // Verify other entries are preserved
  let user_id = Baggage::get_entry(baggage_after_removal, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user-123")
    None => assert_true(false)
  }
}

// Test 7: Telemetry Pipeline Data Consistency
test "telemetry pipeline data consistency" {
  let collector = TelemetryCollector::new()
  let processor = TelemetryProcessor::new()
  let exporter = TelemetryExporter::new()
  
  // Create test telemetry data
  let span = Span::new("test.operation", Internal, 
    SpanContext::new("trace_123", "span_456", true, ""))
  
  // Add consistent attributes and events
  Span::set_attribute(span, "service.name", "test.service")
  Span::set_attribute(span, "operation.type", "test.type")
  
  Span::add_event(span, "test.event", Some([
    ("event.data", "test.data"),
    ("event.timestamp", "1234567890")
  ]))
  
  // Add metrics
  let counter = Metric::counter("test.counter", "Test counter")
  Metric::add(counter, 1.0)
  
  let histogram = Metric::histogram("test.histogram", "Test histogram", "ms")
  Metric::record(histogram, 100.0)
  
  // Process through pipeline
  TelemetryCollector::add_span(collector, span)
  TelemetryProcessor::process_span(processor, span)
  
  // Verify data consistency through pipeline
  let collected_spans = TelemetryCollector::get_spans(collector)
  assert_eq(collected_spans.length(), 1)
  
  let collected_span = collected_spans[0]
  assert_eq(Span::name(collected_span), "test.operation")
  
  let service_name = Span::get_attribute(collected_span, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test.service")
    _ => assert_true(false)
  }
  
  // Verify metrics consistency
  let collected_metrics = TelemetryCollector::get_metrics(collector)
  assert_true(collected_metrics.length() >= 2) // At least counter and histogram
  
  // Export and verify consistency
  let exported_data = TelemetryExporter::export(exporter, collector)
  assert_true(exported_data.length() > 0)
  
  // Parse exported data and verify consistency
  let parsed_data = TelemetryData::from_json(exported_data)
  assert_true(parsed_data.spans.length() >= 1)
  assert_true(parsed_data.metrics.length() >= 2)
  
  // Verify exported span matches original
  let exported_span = parsed_data.spans[0]
  assert_eq(exported_span.name, Span::name(span))
  assert_eq(exported_span.trace_id, SpanContext::trace_id(Span::span_context(span)))
  assert_eq(exported_span.span_id, SpanContext::span_id(Span::span_context(span)))
}

// Test 8: Configuration Consistency
test "configuration consistency" {
  let config = TelemetryConfig::new()
  
  // Set configuration values
  TelemetryConfig::set(config, "service.name", "test.service")
  TelemetryConfig::set(config, "sampling.rate", "0.5")
  TelemetryConfig::set(config, "export.interval", "5000")
  TelemetryConfig::set(config, "batch.size", "100")
  
  // Verify configuration consistency
  let service_name = TelemetryConfig::get(config, "service.name")
  match service_name {
    Some(value) => assert_eq(value, "test.service")
    None => assert_true(false)
  }
  
  let sampling_rate = TelemetryConfig::get(config, "sampling.rate")
  match sampling_rate {
    Some(value) => assert_eq(value, "0.5")
    None => assert_true(false)
  }
  
  // Test configuration type consistency
  let typed_config = TelemetryConfig::with_types(config, [
    ("service.name", StringType),
    ("sampling.rate", FloatType),
    ("export.interval", IntType),
    ("batch.size", IntType)
  ])
  
  // Verify type consistency
  let service_name_type = TelemetryConfig::get_type(typed_config, "service.name")
  match service_name_type {
    Some(StringType) => assert_true(true)
    _ => assert_true(false)
  }
  
  let sampling_rate_type = TelemetryConfig::get_type(typed_config, "sampling.rate")
  match sampling_rate_type {
    Some(FloatType) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test configuration validation consistency
  let validation_rules = [
    ("service.name", NonEmptyString),
    ("sampling.rate", Range(0.0, 1.0)),
    ("export.interval", PositiveInteger),
    ("batch.size", PositiveInteger)
  ]
  
  let validated_config = TelemetryConfig::with_validation(typed_config, validation_rules)
  
  // Verify validation consistency
  assert_true(TelemetryConfig::is_valid(validated_config))
  
  // Test invalid configuration
  TelemetryConfig::set(validated_config, "sampling.rate", "1.5") // Invalid: > 1.0
  assert_false(TelemetryConfig::is_valid(validated_config))
  
  // Test configuration serialization consistency
  let serialized = TelemetryConfig::to_json(validated_config)
  let deserialized = TelemetryConfig::from_json(serialized)
  
  // Verify all configuration values are preserved
  let original_service_name = TelemetryConfig::get(validated_config, "service.name")
  let deserialized_service_name = TelemetryConfig::get(deserialized, "service.name")
  assert_eq(original_service_name, deserialized_service_name)
  
  let original_sampling_rate = TelemetryConfig::get(validated_config, "sampling.rate")
  let deserialized_sampling_rate = TelemetryConfig::get(deserialized, "sampling.rate")
  assert_eq(original_sampling_rate, deserialized_sampling_rate)
}