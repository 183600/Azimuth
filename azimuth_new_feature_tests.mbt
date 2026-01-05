// Azimuth Telemetry System - New Feature Tests
// This file contains new test cases for enhanced telemetry functionality

// Test 1: Advanced Span Operations with Parent-Child Relationships
test "advanced span operations with parent-child relationships" {
  let parent_ctx = SpanContext::new("parent_trace_id", "parent_span_id", true, "")
  let parent_span = Span::new("parent_span", Server, parent_ctx)
  
  // Create child span with parent context
  let child_span = Span::new("child_span", Client, SpanContext::new("parent_trace_id", "child_span_id", true, ""))
  Span::set_parent(child_span, parent_span)
  
  // Test parent-child relationship
  let parent_context = Span::parent_context(child_span)
  match parent_context {
    Some(ctx) => assert_eq(SpanContext::span_id(ctx), "parent_span_id")
    None => assert_true(false)
  }
  
  // Test span hierarchy
  assert_true(Span::is_parent_of(parent_span, child_span))
  assert_false(Span::is_parent_of(child_span, parent_span))
  
  Span::end(child_span)
  Span::end(parent_span)
}

// Test 2: Metric Aggregation and Statistics
test "metric aggregation and statistics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_meter")
  
  // Create counter for aggregation testing
  let counter = Meter::create_counter(meter, "request_count", Some("Total requests"), Some("count"))
  
  // Add multiple values
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.0)
  Counter::add(counter, 3.0)
  Counter::add(counter, 4.0)
  Counter::add(counter, 5.0)
  
  // Test aggregation
  let aggregation = Counter::get_aggregation(counter)
  assert_eq(Aggregation::sum(aggregation), 15.0)
  assert_eq(Aggregation::count(aggregation), 5)
  assert_eq(Aggregation::average(aggregation), 3.0)
  
  // Create histogram for percentile testing
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response times"), Some("ms"))
  
  // Record multiple values
  Histogram::record(histogram, 10.0)
  Histogram::record(histogram, 20.0)
  Histogram::record(histogram, 30.0)
  Histogram::record(histogram, 40.0)
  Histogram::record(histogram, 50.0)
  
  // Test percentiles
  let percentile_data = Histogram::get_percentiles(histogram)
  assert_eq(Percentile::value(percentile_data, 50.0), 30.0)  // 50th percentile
  assert_eq(Percentile::value(percentile_data, 95.0), 50.0)  // 95th percentile
}

// Test 3: Correlation Context Propagation
test "correlation context propagation" {
  // Create correlation context with multiple values
  let correlation_ctx = CorrelationContext::new()
  let enriched_ctx = CorrelationContext::set_value(correlation_ctx, "user_id", "user123")
  let enriched_ctx2 = CorrelationContext::set_value(enriched_ctx, "session_id", "session456")
  let enriched_ctx3 = CorrelationContext::set_value(enriched_ctx2, "request_id", "request789")
  
  // Test value retrieval
  let user_id = CorrelationContext::get_value(enriched_ctx3, "user_id")
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let session_id = CorrelationContext::get_value(enriched_ctx3, "session_id")
  match session_id {
    Some(value) => assert_eq(value, "session456")
    None => assert_true(false)
  }
  
  let request_id = CorrelationContext::get_value(enriched_ctx3, "request_id")
  match request_id {
    Some(value) => assert_eq(value, "request789")
    None => assert_true(false)
  }
  
  // Test non-existent key
  let non_existent = CorrelationContext::get_value(enriched_ctx3, "non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test context serialization and deserialization
  let serialized = CorrelationContext::serialize(enriched_ctx3)
  let deserialized = CorrelationContext::deserialize(serialized)
  
  let deserialized_user_id = CorrelationContext::get_value(deserialized, "user_id")
  match deserialized_user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
}

// Test 4: Sampling Strategy with Dynamic Configuration
test "sampling strategy with dynamic configuration" {
  // Create sampling configuration
  let config = SamplingConfig::new()
  let updated_config = SamplingConfig::set_probability(config, 0.5)  // 50% sampling
  let final_config = SamplingConfig::set_sample_rate(updated_config, 10)  // 1 in 10
  
  // Create sampler with configuration
  let sampler = Sampler::with_config(final_config)
  
  // Test sampling decision
  let trace_id = "test_trace_id_12345"
  let span_name = "test_span"
  
  // Run multiple sampling decisions to test probability
  let mut sampled_count = 0
  for i in 0..=100 {
    let decision = Sampler::should_sample(sampler, trace_id, span_name)
    if decision {
      sampled_count = sampled_count + 1
    }
  }
  
  // Should be approximately 50% (allowing for some variance)
  assert_true(sampled_count >= 40 && sampled_count <= 60)
  
  // Test deterministic sampling
  let deterministic_sampler = Sampler::deterministic()
  let decision1 = Sampler::should_sample(deterministic_sampler, trace_id, span_name)
  let decision2 = Sampler::should_sample(deterministic_sampler, trace_id, span_name)
  
  // Deterministic sampler should return the same decision for the same inputs
  assert_eq(decision1, decision2)
}

// Test 5: Batch Processing for Telemetry Data
test "batch processing for telemetry data" {
  // Create batch processor
  let batch_config = BatchConfig::new()
  let configured_batch = BatchConfig::with_max_size(batch_config, 100)
  let final_batch_config = BatchConfig::with_timeout(configured_batch, 5000)  // 5 seconds
  
  let processor = BatchProcessor::new(final_batch_config)
  
  // Create multiple spans for batching
  let spans = [
    Span::new("span1", Internal, SpanContext::new("trace1", "span1", true, "")),
    Span::new("span2", Internal, SpanContext::new("trace1", "span2", true, "")),
    Span::new("span3", Internal, SpanContext::new("trace2", "span3", true, "")),
    Span::new("span4", Internal, SpanContext::new("trace2", "span4", true, "")),
    Span::new("span5", Internal, SpanContext::new("trace3", "span5", true, ""))
  ]
  
  // Add spans to batch processor
  for span in spans {
    BatchProcessor::add_span(processor, span)
  }
  
  // Test batch size
  assert_eq(BatchProcessor::size(processor), 5)
  
  // Test batch flushing
  let flushed_spans = BatchProcessor::flush(processor)
  assert_eq(flushed_spans.length(), 5)
  
  // Batch should be empty after flush
  assert_eq(BatchProcessor::size(processor), 0)
  
  // Test automatic flush on max size
  let small_batch_config = BatchConfig::with_max_size(BatchConfig::new(), 3)
  let small_processor = BatchProcessor::new(small_batch_config)
  
  BatchProcessor::add_span(small_processor, Span::new("span1", Internal, SpanContext::new("trace1", "span1", true, "")))
  assert_eq(BatchProcessor::size(small_processor), 1)
  
  BatchProcessor::add_span(small_processor, Span::new("span2", Internal, SpanContext::new("trace1", "span2", true, "")))
  assert_eq(BatchProcessor::size(small_processor), 2)
  
  BatchProcessor::add_span(small_processor, Span::new("span3", Internal, SpanContext::new("trace1", "span3", true, "")))
  // Should automatically flush when reaching max size
  assert_eq(BatchProcessor::size(small_processor), 0)
}

// Test 6: Telemetry Data Compression
test "telemetry data compression" {
  // Create telemetry data for compression testing
  let telemetry_data = TelemetryData::new()
  
  // Add spans with large attributes
  let span1 = Span::new("large_span1", Internal, SpanContext::new("trace1", "span1", true, ""))
  Span::add_event(span1, "large_event", Some([
    ("large_attribute1", StringValue("large_value1_with_lots_of_repeated_text_to_compress")),
    ("large_attribute2", StringValue("large_value2_with_lots_of_repeated_text_to_compress")),
    ("large_attribute3", StringValue("large_value3_with_lots_of_repeated_text_to_compress"))
  ]))
  
  let span2 = Span::new("large_span2", Internal, SpanContext::new("trace1", "span2", true, ""))
  Span::add_event(span2, "large_event", Some([
    ("large_attribute1", StringValue("large_value1_with_lots_of_repeated_text_to_compress")),
    ("large_attribute2", StringValue("large_value2_with_lots_of_repeated_text_to_compress")),
    ("large_attribute3", StringValue("large_value3_with_lots_of_repeated_text_to_compress"))
  ]))
  
  TelemetryData::add_span(telemetry_data, span1)
  TelemetryData::add_span(telemetry_data, span2)
  
  // Serialize telemetry data
  let serialized_data = TelemetryData::serialize(telemetry_data)
  let original_size = serialized_data.length()
  
  // Compress serialized data
  let compressed_data = Compression::compress(serialized_data)
  let compressed_size = compressed_data.length()
  
  // Verify compression reduces size
  assert_true(compressed_size < original_size)
  
  // Decompress and verify data integrity
  let decompressed_data = Compression::decompress(compressed_data)
  assert_eq(decompressed_data.length(), original_size)
  assert_eq(decompressed_data, serialized_data)
  
  // Deserialize decompressed data
  let restored_telemetry_data = TelemetryData::deserialize(decompressed_data)
  assert_eq(TelemetryData::span_count(restored_telemetry_data), 2)
}

// Test 7: Distributed Trace Context Propagation
test "distributed trace context propagation" {
  // Create trace context for distributed systems
  let trace_context = TraceContext::new()
  let enriched_context = TraceContext::with_trace_id(trace_context, "distributed_trace_123")
  let enriched_context2 = TraceContext::with_span_id(enriched_context, "distributed_span_456")
  let final_context = TraceContext::with_baggage(enriched_context2, [("service_name", "auth_service"), ("version", "1.2.3")])
  
  // Test trace context extraction
  let trace_id = TraceContext::trace_id(final_context)
  match trace_id {
    Some(id) => assert_eq(id, "distributed_trace_123")
    None => assert_true(false)
  }
  
  let span_id = TraceContext::span_id(final_context)
  match span_id {
    Some(id) => assert_eq(id, "distributed_span_456")
    None => assert_true(false)
  }
  
  // Test baggage extraction
  let service_name = TraceContext::get_baggage_value(final_context, "service_name")
  match service_name {
    Some(value) => assert_eq(value, "auth_service")
    None => assert_true(false)
  }
  
  let version = TraceContext::get_baggage_value(final_context, "version")
  match version {
    Some(value) => assert_eq(value, "1.2.3")
    None => assert_true(false)
  }
  
  // Test context injection into headers
  let headers = TraceContext::inject_into_headers(final_context)
  let trace_header = Headers::get(headers, "traceparent")
  match trace_header {
    Some(header) => assert_true(header.contains("distributed_trace_123"))
    None => assert_true(false)
  }
  
  let baggage_header = Headers::get(headers, "baggage")
  match baggage_header {
    Some(header) => {
      assert_true(header.contains("service_name=auth_service"))
      assert_true(header.contains("version=1.2.3"))
    }
    None => assert_true(false)
  }
  
  // Test context extraction from headers
  let extracted_context = TraceContext::extract_from_headers(headers)
  let extracted_trace_id = TraceContext::trace_id(extracted_context)
  match extracted_trace_id {
    Some(id) => assert_eq(id, "distributed_trace_123")
    None => assert_true(false)
  }
}

// Test 8: Telemetry Data Filtering and Transformation
test "telemetry data filtering and transformation" {
  // Create telemetry data with various attributes
  let telemetry_data = TelemetryData::new()
  
  let span1 = Span::new("critical_span", Internal, SpanContext::new("trace1", "span1", true, ""))
  Span::add_event(span1, "critical_event", Some([
    ("severity", StringValue("critical")),
    ("component", StringValue("auth")),
    ("user_id", StringValue("user123"))
  ]))
  
  let span2 = Span::new("normal_span", Internal, SpanContext::new("trace2", "span2", true, ""))
  Span::add_event(span2, "normal_event", Some([
    ("severity", StringValue("info")),
    ("component", StringValue("ui")),
    ("user_id", StringValue("user456"))
  ]))
  
  let span3 = Span::new("debug_span", Internal, SpanContext::new("trace3", "span3", true, ""))
  Span::add_event(span3, "debug_event", Some([
    ("severity", StringValue("debug")),
    ("component", StringValue("logging")),
    ("user_id", StringValue("user789"))
  ]))
  
  TelemetryData::add_span(telemetry_data, span1)
  TelemetryData::add_span(telemetry_data, span2)
  TelemetryData::add_span(telemetry_data, span3)
  
  // Create filter for critical severity
  let critical_filter = Filter::attribute_equals("severity", "critical")
  let filtered_data = Filter::apply(telemetry_data, critical_filter)
  
  // Should only contain span1
  assert_eq(TelemetryData::span_count(filtered_data), 1)
  let filtered_span = TelemetryData::get_span(filtered_data, 0)
  assert_eq(Span::name(filtered_span), "critical_span")
  
  // Create transformer for PII redaction
  let pii_transformer = Transformer::redact_attributes(["user_id"])
  let transformed_data = Transformer::apply(telemetry_data, pii_transformer)
  
  // Verify PII is redacted
  let transformed_span1 = TelemetryData::get_span(transformed_data, 0)
  let events = Span::events(transformed_span1)
  let event1 = events[0]
  let attributes = Event::attributes(event1)
  
  let user_id_attr = Attributes::get(attributes, "user_id")
  match user_id_attr {
    Some(StringValue(value)) => assert_eq(value, "[REDACTED]")
    _ => assert_true(false)
  }
  
  // Verify other attributes are not redacted
  let severity_attr = Attributes::get(attributes, "severity")
  match severity_attr {
    Some(StringValue(value)) => assert_eq(value, "critical")
    _ => assert_true(false)
  }
}

// Test 9: Telemetry Data Retention and Expiration
test "telemetry data retention and expiration" {
  // Create retention policy
  let retention_policy = RetentionPolicy::new()
  let configured_policy = RetentionPolicy::with_max_age(retention_policy, 3600)  // 1 hour
  let final_policy = RetentionPolicy::with_max_span_count(configured_policy, 1000)
  
  // Create retention manager
  let retention_manager = RetentionManager::new(final_policy)
  
  // Create telemetry data with timestamps
  let telemetry_data = TelemetryData::new()
  
  // Add old spans (beyond retention period)
  let old_timestamp = Time::now() - 7200  // 2 hours ago
  let old_span = Span::new("old_span", Internal, SpanContext::new("trace1", "span1", true, ""))
  Span::set_timestamp(old_span, old_timestamp)
  TelemetryData::add_span(telemetry_data, old_span)
  
  // Add recent spans (within retention period)
  let recent_timestamp = Time::now() - 1800  // 30 minutes ago
  let recent_span = Span::new("recent_span", Internal, SpanContext::new("trace2", "span2", true, ""))
  Span::set_timestamp(recent_span, recent_timestamp)
  TelemetryData::add_span(telemetry_data, recent_span)
  
  // Apply retention policy
  let retained_data = RetentionManager::apply_policy(retention_manager, telemetry_data)
  
  // Should only contain recent spans
  assert_eq(TelemetryData::span_count(retained_data), 1)
  let retained_span = TelemetryData::get_span(retained_data, 0)
  assert_eq(Span::name(retained_span), "recent_span")
  
  // Test span count limit
  let large_telemetry_data = TelemetryData::new()
  for i in 0..=1500 {
    let span = Span::new("span_" + i.to_string(), Internal, SpanContext::new("trace" + i.to_string(), "span" + i.to_string(), true, ""))
    TelemetryData::add_span(large_telemetry_data, span)
  }
  
  let count_limited_data = RetentionManager::apply_policy(retention_manager, large_telemetry_data)
  assert_eq(TelemetryData::span_count(count_limited_data), 1000)
}

// Test 10: Telemetry Data Export and Import
test "telemetry data export and import" {
  // Create telemetry data for export testing
  let telemetry_data = TelemetryData::new()
  
  let span1 = Span::new("export_span1", Internal, SpanContext::new("trace1", "span1", true, ""))
  Span::add_event(span1, "export_event1", Some([("export_attr", StringValue("export_value1"))]))
  
  let span2 = Span::new("export_span2", Internal, SpanContext::new("trace2", "span2", true, ""))
  Span::add_event(span2, "export_event2", Some([("export_attr", StringValue("export_value2"))]))
  
  TelemetryData::add_span(telemetry_data, span1)
  TelemetryData::add_span(telemetry_data, span2)
  
  // Export to JSON format
  let json_export = Exporter::to_json(telemetry_data)
  assert_true(json_export.contains("export_span1"))
  assert_true(json_export.contains("export_span2"))
  assert_true(json_export.contains("export_event1"))
  assert_true(json_export.contains("export_event2"))
  assert_true(json_export.contains("export_value1"))
  assert_true(json_export.contains("export_value2"))
  
  // Export to Protocol Buffers format
  let protobuf_export = Exporter::to_protobuf(telemetry_data)
  assert_true(protobuf_export.length() > 0)
  
  // Import from JSON
  let imported_data = Importer::from_json(json_export)
  assert_eq(TelemetryData::span_count(imported_data), 2)
  
  let imported_span1 = TelemetryData::get_span(imported_data, 0)
  assert_eq(Span::name(imported_span1), "export_span1")
  
  let imported_span2 = TelemetryData::get_span(imported_data, 1)
  assert_eq(Span::name(imported_span2), "export_span2")
  
  // Import from Protocol Buffers
  let imported_protobuf_data = Importer::from_protobuf(protobuf_export)
  assert_eq(TelemetryData::span_count(imported_protobuf_data), 2)
  
  // Verify data integrity after round-trip
  let reexported_json = Exporter::to_json(imported_data)
  assert_eq(json_export, reexported_json)
}