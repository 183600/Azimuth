// Azimuth Telemetry System - Telemetry Data Integrity Tests
// This file contains comprehensive telemetry data integrity test cases

// Test 1: Span Data Integrity
test "span data integrity" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "integrity_test_tracer")
  let span = Tracer::start_span(tracer, "integrity_test_span")
  
  // Add attributes to span
  Span::set_attribute(span, "user.id", StringValue("user123"))
  Span::set_attribute(span, "operation.type", StringValue("database_query"))
  Span::set_attribute(span, "retry.count", IntValue(3))
  
  // Add events to span
  Span::add_event(span, "query_started", Some([
    ("query", StringValue("SELECT * FROM users")),
    ("timestamp", StringValue("2023-01-01T12:00:00Z"))
  ]))
  
  Span::add_event(span, "query_completed", Some([
    ("result_count", IntValue(100)),
    ("duration_ms", IntValue(250)
  ]))
  
  // Set span status
  Span::set_status(span, Ok, Some("Query completed successfully"))
  
  // Verify span data integrity before ending
  assert_eq(Span::name(span), "integrity_test_span")
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Ok)
  
  // Verify attributes
  let user_id = Span::get_attribute(span, "user.id")
  match user_id {
    Some(StringValue(v)) => assert_eq(v, "user123")
    _ => assert_true(false)
  }
  
  let operation_type = Span::get_attribute(span, "operation.type")
  match operation_type {
    Some(StringValue(v)) => assert_eq(v, "database_query")
    _ => assert_true(false)
  }
  
  let retry_count = Span::get_attribute(span, "retry.count")
  match retry_count {
    Some(IntValue(v)) => assert_eq(v, 3)
    _ => assert_true(false)
  }
  
  // Verify events
  let events = Span::get_events(span)
  assert_eq(events.length(), 2)
  
  let first_event = events[0]
  assert_eq(Event::name(first_event), "query_started")
  
  let second_event = events[1]
  assert_eq(Event::name(second_event), "query_completed")
  
  // End span and verify data integrity is maintained
  Span::end(span)
  
  // Verify span data is still accessible after ending
  assert_false(Span::is_recording(span))
  assert_eq(Span::name(span), "integrity_test_span")
  
  // Verify attributes are preserved
  let final_user_id = Span::get_attribute(span, "user.id")
  match final_user_id {
    Some(StringValue(v)) => assert_eq(v, "user123")
    _ => assert_true(false)
  }
}

// Test 2: Metric Data Integrity
test "metric data integrity" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "integrity_test_meter")
  
  // Create counter metric
  let counter = Meter::create_counter(meter, "request_count", Some("Total number of requests"), Some("count"))
  let attrs = Attributes::new()
  Attributes::set(attrs, "method", StringValue("GET"))
  Attributes::set(attrs, "status", StringValue("200"))
  
  // Add measurements to counter
  Counter::add(counter, 1.0, Some(attrs))
  Counter::add(counter, 1.0, Some(attrs))
  Counter::add(counter, 1.0, Some(attrs))
  
  // Create histogram metric
  let histogram = Meter::create_histogram(meter, "request_duration", Some("Request duration"), Some("ms"))
  let duration_attrs = Attributes::new()
  Attributes::set(duration_attrs, "endpoint", StringValue("/api/users"))
  
  // Record measurements to histogram
  Histogram::record(histogram, 100.0, Some(duration_attrs))
  Histogram::record(histogram, 150.0, Some(duration_attrs))
  Histogram::record(histogram, 200.0, Some(duration_attrs))
  Histogram::record(histogram, 250.0, Some(duration_attrs))
  Histogram::record(histogram, 300.0, Some(duration_attrs))
  
  // Verify metric data integrity
  let counter_metric = Metric::from_instrument(Histogram::as_instrument(counter))
  match counter_metric {
    Some(metric) => {
      assert_eq(Metric::name(metric), "request_count")
      assert_eq(Metric::description(metric), Some("Total number of requests"))
      assert_eq(Metric::unit(metric), Some("count"))
      
      // Verify metric data points
      let data_points = Metric::data_points(metric)
      assert_true(data_points.length() > 0)
      
      // Find data point with our attributes
      let target_data_point = DataPoint::find_with_attributes(data_points, attrs)
      match target_data_point {
        Some(dp) => {
          assert_eq(DataPoint::value(dp), 3.0)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Verify histogram data integrity
  let histogram_metric = Metric::from_instrument(Histogram::as_instrument(histogram))
  match histogram_metric {
    Some(metric) => {
      assert_eq(Metric::name(metric), "request_duration")
      assert_eq(Metric::description(metric), Some("Request duration"))
      assert_eq(Metric::unit(metric), Some("ms"))
      
      // Verify histogram data points
      let data_points = Metric::data_points(metric)
      assert_true(data_points.length() > 0)
      
      // Find data point with our attributes
      let target_data_point = DataPoint::find_with_attributes(data_points, duration_attrs)
      match target_data_point {
        Some(dp) => {
          // Verify histogram statistics
          let histogram_stats = DataPoint::histogram_stats(dp)
          match histogram_stats {
            Some(stats) => {
              assert_eq(HistogramStats::count(stats), 5)
              assert_true(HistogramStats::sum(stats) > 900.0)
              assert_true(HistogramStats::min(stats) >= 100.0)
              assert_true(HistogramStats::max(stats) <= 300.0)
            }
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: Log Data Integrity
test "log data integrity" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "integrity_test_logger")
  
  // Create log record with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user456"))
  Attributes::set(attrs, "session.id", StringValue("session789"))
  Attributes::set(attrs, "request.id", StringValue("req123"))
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(1672574400000L), // 2023-01-01 12:00:00 UTC
    Some(1672574401000L), // 2023-01-01 12:00:01 UTC
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  
  // Add additional attributes to log record
  LogRecord::add_attribute(log_record, "error.code", IntValue(500))
  LogRecord::add_attribute(log_record, "error.message", StringValue("Connection timeout"))
  LogRecord::add_attribute(log_record, "retry.count", IntValue(3))
  
  // Emit log record
  Logger::emit(logger, log_record)
  
  // Verify log record data integrity
  assert_eq(LogRecord::severity_number(log_record), Error)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Database connection failed")
    None => assert_true(false)
  }
  
  assert_eq(LogRecord::timestamp(log_record), Some(1672574400000L))
  assert_eq(LogRecord::observed_timestamp(log_record), Some(1672574401000L))
  assert_eq(LogRecord::trace_id(log_record), Some("trace123"))
  assert_eq(LogRecord::span_id(log_record), Some("span456"))
  
  // Verify log attributes
  let log_attrs = LogRecord::attributes(log_record)
  match log_attrs {
    Some(attrs) => {
      let user_id = Attributes::get(attrs, "user.id")
      match user_id {
        Some(StringValue(v)) => assert_eq(v, "user456")
        _ => assert_true(false)
      }
      
      let error_code = Attributes::get(attrs, "error.code")
      match error_code {
        Some(IntValue(v)) => assert_eq(v, 500)
        _ => assert_true(false)
      }
      
      let error_message = Attributes::get(attrs, "error.message")
      match error_message {
        Some(StringValue(v)) => assert_eq(v, "Connection timeout")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 4: Trace Context Integrity
test "trace context integrity" {
  // Create trace context with specific values
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_flags = 1 // Sampled
  let trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  let span_ctx = SpanContext::new(trace_id, span_id, true, trace_state)
  
  // Verify trace context integrity
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_eq(SpanContext::trace_state(span_ctx), trace_state)
  
  // Create span with this context
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "context_test_tracer")
  let span = Tracer::start_span_with_context(tracer, "context_test_span", span_ctx)
  
  // Verify context is preserved in span
  let span_context = Span::span_context(span)
  assert_eq(SpanContext::trace_id(span_context), trace_id)
  assert_eq(SpanContext::span_id(span_context), span_id)
  assert_true(SpanContext::is_sampled(span_context))
  
  // Add child span and verify parent-child relationship
  let child_span = Tracer::start_span(tracer, "child_span")
  let child_context = Span::span_context(child_span)
  
  // Child should have same trace ID but different span ID
  assert_eq(SpanContext::trace_id(child_context), trace_id)
  assert_not_eq(SpanContext::span_id(child_context), span_id)
  
  // Verify parent span ID is set in child context
  let parent_span_id = SpanContext::parent_span_id(child_context)
  match parent_span_id {
    Some(id) => assert_eq(id, span_id)
    None => assert_true(false)
  }
  
  // End spans
  Span::end(child_span)
  Span::end(span)
}

// Test 5: Baggage Integrity
test "baggage integrity" {
  let baggage = Baggage::new()
  
  // Add baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session456")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "req789")
  
  // Add baggage with metadata
  let baggage4 = Baggage::set_entry_with_metadata(baggage3, "server.zone", "us-west-2", "propagate=true")
  
  // Verify baggage integrity
  let user_id = Baggage::get_entry(baggage4, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(baggage4, "session.id")
  match session_id {
    Some(value) => assert_eq(value, "session456")
    None => assert_true(false)
  }
  
  let request_id = Baggage::get_entry(baggage4, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req789")
    None => assert_true(false)
  }
  
  let server_zone = Baggage::get_entry(baggage4, "server.zone")
  match server_zone {
    Some(value) => assert_eq(value, "us-west-2")
    None => assert_true(false)
  }
  
  // Verify baggage metadata
  let server_zone_metadata = Baggage::get_entry_metadata(baggage4, "server.zone")
  match server_zone_metadata {
    Some(metadata) => assert_eq(metadata, "propagate=true")
    None => assert_true(false)
  }
  
  // Verify baggage entries count
  let entries = Baggage::get_all_entries(baggage4)
  assert_eq(entries.length(), 4)
  
  // Verify baggage serialization and deserialization
  let baggage_str = Baggage::serialize(baggage4)
  assert_true(baggage_str.length() > 0)
  
  let deserialized_baggage = Baggage::deserialize(baggage_str)
  let original_user_id = Baggage::get_entry(baggage4, "user.id")
  let deserialized_user_id = Baggage::get_entry(deserialized_baggage, "user.id")
  
  match (original_user_id, deserialized_user_id) {
    (Some(orig), Some(deser)) => assert_eq(orig, deser)
    _ => assert_true(false)
  }
}

// Test 6: Resource Integrity
test "resource integrity" {
  // Create resource with attributes
  let attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("prod-server-01")),
    ("host.id", StringValue("host-67890"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Verify resource integrity
  let service_name = Resource::get_attribute(resource, "service.name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(resource, "service.version")
  match service_version {
    Some(StringValue(v)) => assert_eq(v, "1.2.3")
    _ => assert_true(false)
  }
  
  let service_instance_id = Resource::get_attribute(resource, "service.instance.id")
  match service_instance_id {
    Some(StringValue(v)) => assert_eq(v, "instance-12345")
    _ => assert_true(false)
  }
  
  let deployment_environment = Resource::get_attribute(resource, "deployment.environment")
  match deployment_environment {
    Some(StringValue(v)) => assert_eq(v, "production")
    _ => assert_true(false)
  }
  
  // Test resource merging
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("feature.flag", StringValue("new-payment-flow"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource, override_resource)
  
  // Verify merged resource integrity
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  match merged_service_name {
    Some(StringValue(v)) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  // Service version should be overridden
  let merged_service_version = Resource::get_attribute(merged_resource, "service.version")
  match merged_service_version {
    Some(StringValue(v)) => assert_eq(v, "2.0.0")
    _ => assert_true(false)
  }
  
  // New attribute should be added
  let feature_flag = Resource::get_attribute(merged_resource, "feature.flag")
  match feature_flag {
    Some(StringValue(v)) => assert_eq(v, "new-payment-flow")
    _ => assert_true(false)
  }
}

// Test 7: Data Consistency Across Operations
test "data consistency across operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "consistency_test_tracer")
  
  // Create a span with multiple operations
  let span = Tracer::start_span(tracer, "consistency_test_span")
  
  // Set initial attributes
  Span::set_attribute(span, "operation.phase", StringValue("initialization"))
  Span::set_attribute(span, "user.id", StringValue("user123"))
  
  // Add event
  Span::add_event(span, "operation_started", Some([
    ("timestamp", StringValue("2023-01-01T12:00:00Z"))
  ]))
  
  // Verify initial state
  let phase = Span::get_attribute(span, "operation.phase")
  match phase {
    Some(StringValue(v)) => assert_eq(v, "initialization")
    _ => assert_true(false)
  }
  
  // Update attributes
  Span::set_attribute(span, "operation.phase", StringValue("processing"))
  Span::set_attribute(span, "items.processed", IntValue(0))
  
  // Process items in a loop
  for i in 0..=100 {
    // Update processed count
    Span::set_attribute(span, "items.processed", IntValue(i + 1))
    
    if i % 25 == 0 {
      // Add milestone event
      Span::add_event(span, "milestone_reached", Some([
        ("items.count", IntValue(i + 1)),
        ("percentage", FloatValue((i + 1).to_float() / 101.0 * 100.0))
      ]))
    }
  }
  
  // Final update
  Span::set_attribute(span, "operation.phase", StringValue("completed"))
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Verify final state consistency
  let final_phase = Span::get_attribute(span, "operation.phase")
  match final_phase {
    Some(StringValue(v)) => assert_eq(v, "completed")
    _ => assert_true(false)
  }
  
  let final_processed = Span::get_attribute(span, "items.processed")
  match final_processed {
    Some(IntValue(v)) => assert_eq(v, 101)
    _ => assert_true(false)
  }
  
  // Verify events consistency
  let events = Span::get_events(span)
  assert_eq(events.length(), 5) // 1 start + 4 milestones
  
  // Verify milestone events
  let milestone_events = Event::filter_by_name(events, "milestone_reached")
  assert_eq(milestone_events.length(), 4)
  
  // Verify milestone event attributes
  let first_milestone = milestone_events[0]
  let first_milestone_attrs = Event::attributes(first_milestone)
  match first_milestone_attrs {
    Some(attrs) => {
      let items_count = Attributes::get(attrs, "items.count")
      match items_count {
        Some(IntValue(v)) => assert_eq(v, 25)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // End span
  Span::end(span)
  
  // Verify data is still consistent after ending
  let final_user_id = Span::get_attribute(span, "user.id")
  match final_user_id {
    Some(StringValue(v)) => assert_eq(v, "user123")
    _ => assert_true(false)
  }
}

// Test 8: Data Corruption Detection
test "data corruption detection" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "corruption_test_tracer")
  let span = Tracer::start_span(tracer, "corruption_test_span")
  
  // Set attributes
  Span::set_attribute(span, "checksum", StringValue("initial_checksum"))
  Span::set_attribute(span, "data.version", IntValue(1))
  
  // Calculate initial checksum
  let initial_checksum = DataIntegrity::calculate_checksum(span)
  
  // Simulate data corruption
  let corrupted_span = DataIntegrity::simulate_corruption(span, "checksum", StringValue("corrupted_value"))
  
  // Detect corruption
  let is_corrupted = DataIntegrity::detect_corruption(corrupted_span, initial_checksum)
  assert_true(is_corrupted)
  
  // Repair corrupted data
  let repaired_span = DataIntegrity::repair_corruption(corrupted_span, "checksum", StringValue("initial_checksum"))
  
  // Verify repair
  let repaired_checksum = DataIntegrity::calculate_checksum(repaired_span)
  assert_eq(repaired_checksum, initial_checksum)
  
  let repaired_value = Span::get_attribute(repaired_span, "checksum")
  match repaired_value {
    Some(StringValue(v)) => assert_eq(v, "initial_checksum")
    _ => assert_true(false)
  }
  
  // End span
  Span::end(repaired_span)
}

// Test 9: Data Validation
test "data validation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "validation_test_tracer")
  let span = Tracer::start_span(tracer, "validation_test_span")
  
  // Set valid attributes
  Span::set_attribute(span, "user.id", StringValue("user123"))
  Span::set_attribute(span, "request.size", IntValue(1024))
  Span::set_attribute(span, "response.time", FloatValue(250.5))
  Span::set_attribute(span, "success", BoolValue(true))
  
  // Validate attributes
  let validation_result = DataValidation::validate_span_attributes(span)
  assert_true(validation_result.is_valid)
  
  // Set invalid attributes
  Span::set_attribute(span, "user.id", StringValue("")) // Empty user ID
  Span::set_attribute(span, "request.size", IntValue(-1)) // Negative size
  
  // Validate again
  let validation_result2 = DataValidation::validate_span_attributes(span)
  assert_false(validation_result2.is_valid)
  
  // Check validation errors
  assert_true(validation_result2.errors.length() >= 2)
  
  // Fix validation errors
  Span::set_attribute(span, "user.id", StringValue("user456"))
  Span::set_attribute(span, "request.size", IntValue(2048))
  
  // Validate again
  let validation_result3 = DataValidation::validate_span_attributes(span)
  assert_true(validation_result3.is_valid)
  
  // End span
  Span::end(span)
}

// Test 10: Data Serialization Integrity
test "data serialization integrity" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "serialization_test_tracer")
  let span = Tracer::start_span(tracer, "serialization_test_span")
  
  // Set complex attributes
  Span::set_attribute(span, "string.attr", StringValue("test_string"))
  Span::set_attribute(span, "int.attr", IntValue(42))
  Span::set_attribute(span, "float.attr", FloatValue(3.14159))
  Span::set_attribute(span, "bool.attr", BoolValue(true))
  Span::set_attribute(span, "array.attr", ArrayStringValue(["a", "b", "c"]))
  
  // Add events
  Span::add_event(span, "test_event", Some([
    ("event.data", StringValue("event_payload"))
  ]))
  
  // Serialize span to JSON
  let json_str = SpanSerializer::to_json(span)
  assert_true(json_str.length() > 0)
  
  // Calculate checksum of original span
  let original_checksum = DataIntegrity::calculate_checksum(span)
  
  // Deserialize span from JSON
  let deserialized_span = SpanSerializer::from_json(json_str)
  
  // Calculate checksum of deserialized span
  let deserialized_checksum = DataIntegrity::calculate_checksum(deserialized_span)
  
  // Verify checksums match
  assert_eq(original_checksum, deserialized_checksum)
  
  // Verify specific attributes are preserved
  let original_string_attr = Span::get_attribute(span, "string.attr")
  let deserialized_string_attr = Span::get_attribute(deserialized_span, "string.attr")
  
  match (original_string_attr, deserialized_string_attr) {
    (Some(StringValue(orig)), Some(StringValue(deser))) => assert_eq(orig, deser)
    _ => assert_true(false)
  }
  
  // Verify events are preserved
  let original_events = Span::get_events(span)
  let deserialized_events = Span::get_events(deserialized_span)
  assert_eq(original_events.length(), deserialized_events.length())
  
  if original_events.length() > 0 && deserialized_events.length() > 0 {
    let original_event = original_events[0]
    let deserialized_event = deserialized_events[0]
    assert_eq(Event::name(original_event), Event::name(deserialized_event))
  }
  
  // End spans
  Span::end(span)
  Span::end(deserialized_span)
}