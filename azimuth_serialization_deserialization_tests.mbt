// Azimuth Serialization/Deserialization Tests
// This file contains high-quality test cases for serialization and deserialization

// Test 1: Span Serialization and Deserialization
test "span serialization and deserialization" {
  // Test case 1: Basic span serialization
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "test_state")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Add attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("test_service"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "operation.type", StringValue("http_request"))
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  Attributes::set(attrs, "duration_ms", IntValue(123))
  
  // Add events
  Span::add_event(span, "request_started", Some(Attributes::with([
    ("timestamp", IntValue(1640995200)),
    ("message", StringValue("HTTP request started"))
  ])))
  
  Span::add_event(span, "request_completed", Some(Attributes::with([
    ("timestamp", IntValue(1640995323)),
    ("message", StringValue("HTTP request completed"))
  ])))
  
  // Set status
  Span::set_status(span, Ok, Some("Request completed successfully"))
  
  // End span
  Span::end(span)
  
  // Serialize span
  let serialized_span = Serialization::serialize_span(span)
  assert_true(serialized_span.length() > 0)
  
  // Deserialize span
  let deserialized_span = Serialization::deserialize_span(serialized_span)
  
  // Verify deserialized span
  assert_eq(Span::name(deserialized_span), Span::name(span))
  assert_eq(Span::kind(deserialized_span), Span::kind(span))
  assert_eq(Span::status(deserialized_span), Span::status(span))
  
  let original_ctx = Span::span_context(span)
  let deserialized_ctx = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(original_ctx), SpanContext::trace_id(deserialized_ctx))
  assert_eq(SpanContext::span_id(original_ctx), SpanContext::span_id(deserialized_ctx))
  assert_eq(SpanContext::is_sampled(original_ctx), SpanContext::is_sampled(deserialized_ctx))
  
  // Verify events
  let original_events = Span::events(span)
  let deserialized_events = Span::events(deserialized_span)
  assert_eq(original_events.length(), deserialized_events.length())
  
  for i in 0..=original_events.length() - 1 {
    assert_eq(original_events[i].name, deserialized_events[i].name)
    assert_eq(original_events[i].timestamp, deserialized_events[i].timestamp)
  }
  
  // Test case 2: Span with complex attributes
  let complex_span_ctx = SpanContext::new("complex_trace", "complex_span", true, "complex_state")
  let complex_span = Span::new("complex_span", Server, complex_span_ctx)
  
  // Add complex attributes
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "string_array", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(complex_attrs, "int_array", ArrayIntValue([1, 2, 3, 4, 5]))
  Attributes::set(complex_attrs, "float_value", FloatValue(3.14159))
  Attributes::set(complex_attrs, "bool_value", BoolValue(true))
  
  Span::add_event(complex_span, "complex_event", Some(complex_attrs))
  Span::end(complex_span)
  
  // Serialize and deserialize complex span
  let serialized_complex = Serialization::serialize_span(complex_span)
  let deserialized_complex = Serialization::deserialize_span(serialized_complex)
  
  // Verify complex attributes
  let complex_events = Span::events(deserialized_complex)
  assert_eq(complex_events.length(), 1)
  
  let event_attrs = complex_events[0].attributes
  let string_array = Attributes::get(event_attrs, "string_array")
  match string_array {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => assert_true(false)
  }
  
  let int_array = Attributes::get(event_attrs, "int_array")
  match int_array {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false)
  }
}

// Test 2: Metrics Serialization and Deserialization
test "metrics serialization and deserialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization_test_meter")
  
  // Test case 1: Counter serialization
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  // Add measurements with attributes
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("service", StringValue("api")),
    ("endpoint", StringValue("/users")),
    ("method", StringValue("GET"))
  ])))
  
  Counter::add(counter, 2.0, Some(Attributes::with([
    ("service", StringValue("api")),
    ("endpoint", StringValue("/products")),
    ("method", StringValue("POST"))
  ])))
  
  // Serialize counter
  let serialized_counter = Serialization::serialize_metric(counter)
  assert_true(serialized_counter.length() > 0)
  
  // Deserialize counter
  let deserialized_counter = Serialization::deserialize_metric(serialized_counter)
  
  // Verify deserialized counter
  assert_eq(Instrument::name(deserialized_counter), "test_counter")
  assert_eq(Instrument::description(deserialized_counter), Some("Test counter"))
  assert_eq(Instrument::unit(deserialized_counter), Some("count"))
  
  // Test case 2: Histogram serialization
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  // Record measurements
  Histogram::record(histogram, 10.5, Some(Attributes::with([("service", StringValue("api"))])))
  Histogram::record(histogram, 20.3, Some(Attributes::with([("service", StringValue("database"))])))
  Histogram::record(histogram, 15.7, Some(Attributes::with([("service", StringValue("cache"))])))
  
  // Serialize histogram
  let serialized_histogram = Serialization::serialize_metric(histogram)
  assert_true(serialized_histogram.length() > 0)
  
  // Deserialize histogram
  let deserialized_histogram = Serialization::deserialize_metric(serialized_histogram)
  
  // Verify deserialized histogram
  assert_eq(Instrument::name(deserialized_histogram), "test_histogram")
  assert_eq(Instrument::description(deserialized_histogram), Some("Test histogram"))
  assert_eq(Instrument::unit(deserialized_histogram), Some("ms"))
  
  // Test case 3: Gauge serialization
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("percent"))
  
  // Set gauge values
  Gauge::set(gauge, 75.5, Some(Attributes::with([("metric", StringValue("cpu_usage"))])))
  Gauge::set(gauge, 45.2, Some(Attributes::with([("metric", StringValue("memory_usage"))])))
  
  // Serialize gauge
  let serialized_gauge = Serialization::serialize_metric(gauge)
  assert_true(serialized_gauge.length() > 0)
  
  // Deserialize gauge
  let deserialized_gauge = Serialization::deserialize_metric(serialized_gauge)
  
  // Verify deserialized gauge
  assert_eq(Instrument::name(deserialized_gauge), "test_gauge")
  assert_eq(Instrument::description(deserialized_gauge), Some("Test gauge"))
  assert_eq(Instrument::unit(deserialized_gauge), Some("percent"))
}

// Test 3: Log Record Serialization and Deserialization
test "log record serialization and deserialization" {
  // Test case 1: Basic log record
  let log_record = LogRecord::new(Info, "Test log message")
  
  // Serialize log record
  let serialized_log = Serialization::serialize_log_record(log_record)
  assert_true(serialized_log.length() > 0)
  
  // Deserialize log record
  let deserialized_log = Serialization::deserialize_log_record(serialized_log)
  
  // Verify deserialized log record
  assert_eq(LogRecord::severity_number(deserialized_log), Info)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false)
  }
  
  // Test case 2: Log record with attributes
  let attrs = Attributes::with([
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("user.id", StringValue("user123")),
    ("request.id", StringValue("req456")),
    ("duration_ms", IntValue(123))
  ])
  
  let log_with_attrs = LogRecord::new_with_attributes(
    Warning,
    Some("Operation completed with warnings"),
    attrs,
    Some(1640995200L),
    Some(1640995323L),
    Some("trace_123"),
    Some("span_456")
  )
  
  // Serialize log record with attributes
  let serialized_log_with_attrs = Serialization::serialize_log_record(log_with_attrs)
  assert_true(serialized_log_with_attrs.length() > 0)
  
  // Deserialize log record with attributes
  let deserialized_log_with_attrs = Serialization::deserialize_log_record(serialized_log_with_attrs)
  
  // Verify deserialized log record with attributes
  assert_eq(LogRecord::severity_number(deserialized_log_with_attrs), Warning)
  match LogRecord::body(deserialized_log_with_attrs) {
    Some(body) => assert_eq(body, "Operation completed with warnings")
    None => assert_true(false)
  }
  
  assert_eq(LogRecord::timestamp(deserialized_log_with_attrs), Some(1640995200L))
  assert_eq(LogRecord::observed_timestamp(deserialized_log_with_attrs), Some(1640995323L))
  assert_eq(LogRecord::trace_id(deserialized_log_with_attrs), Some("trace_123"))
  assert_eq(LogRecord::span_id(deserialized_log_with_attrs), Some("span_456"))
  
  // Verify attributes
  let deserialized_attrs = LogRecord::attributes(deserialized_log_with_attrs)
  let service_name = Attributes::get(deserialized_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  let duration = Attributes::get(deserialized_attrs, "duration_ms")
  match duration {
    Some(IntValue(value)) => assert_eq(value, 123)
    _ => assert_true(false)
  }
}

// Test 4: Resource Serialization and Deserialization
test "resource serialization and deserialization" {
  // Test case 1: Basic resource
  let resource = Resource::new()
  
  // Serialize resource
  let serialized_resource = Serialization::serialize_resource(resource)
  assert_true(serialized_resource.length() > 0)
  
  // Deserialize resource
  let deserialized_resource = Serialization::deserialize_resource(serialized_resource)
  
  // Verify deserialized resource
  assert_true(Resource::is_valid(deserialized_resource))
  
  // Test case 2: Resource with attributes
  let resource_attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.15.0")),
    ("deployment.environment", StringValue("production"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
  
  // Serialize resource with attributes
  let serialized_resource_with_attrs = Serialization::serialize_resource(resource_with_attrs)
  assert_true(serialized_resource_with_attrs.length() > 0)
  
  // Deserialize resource with attributes
  let deserialized_resource_with_attrs = Serialization::deserialize_resource(serialized_resource_with_attrs)
  
  // Verify deserialized resource with attributes
  for (key, expected_value) in resource_attrs {
    let actual_value = Resource::get_attribute(deserialized_resource_with_attrs, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // Test case 3: Resource merging and serialization
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("deployment.environment", StringValue("staging")),
    ("feature.flag", StringValue("new_feature_enabled"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // Serialize merged resource
  let serialized_merged_resource = Serialization::serialize_resource(merged_resource)
  assert_true(serialized_merged_resource.length() > 0)
  
  // Deserialize merged resource
  let deserialized_merged_resource = Serialization::deserialize_resource(serialized_merged_resource)
  
  // Verify merged attributes
  let service_version = Resource::get_attribute(deserialized_merged_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "2.0.0") // Should be overridden
    _ => assert_true(false)
  }
  
  let deployment_env = Resource::get_attribute(deserialized_merged_resource, "deployment.environment")
  match deployment_env {
    Some(StringValue(env)) => assert_eq(env, "staging") // Should be overridden
    _ => assert_true(false)
  }
  
  let service_name = Resource::get_attribute(deserialized_merged_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service") // Should be preserved
    _ => assert_true(false)
  }
  
  let feature_flag = Resource::get_attribute(deserialized_merged_resource, "feature.flag")
  match feature_flag {
    Some(StringValue(flag)) => assert_eq(flag, "new_feature_enabled") // Should be added
    _ => assert_true(false)
  }
}

// Test 5: Batch Serialization and Deserialization
test "batch serialization and deserialization" {
  // Test case 1: Batch of spans
  let mut spans = []
  
  for i in 0..=10 {
    let span_ctx = SpanContext::new("batch_trace", "batch_span_" + i.to_string(), true, "test_state")
    let span = Span::new("batch_span", Internal, span_ctx)
    
    let attrs = Attributes::with([
      ("index", IntValue(i)),
      ("batch", StringValue("test_batch"))
    ])
    
    Span::add_event(span, "batch_event", Some(attrs))
    Span::end(span)
    
    spans = Array::append(spans, span)
  }
  
  // Serialize batch of spans
  let serialized_batch = Serialization::serialize_span_batch(spans)
  assert_true(serialized_batch.length() > 0)
  
  // Deserialize batch of spans
  let deserialized_batch = Serialization::deserialize_span_batch(serialized_batch)
  
  // Verify deserialized batch
  assert_eq(deserialized_batch.length(), spans.length())
  
  for i in 0..=spans.length() - 1 {
    assert_eq(Span::name(spans[i]), Span::name(deserialized_batch[i]))
    
    let original_events = Span::events(spans[i])
    let deserialized_events = Span::events(deserialized_batch[i])
    assert_eq(original_events.length(), deserialized_events.length())
  }
  
  // Test case 2: Batch of metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "batch_test_meter")
  
  let mut metrics = []
  
  for i in 0..=5 {
    let counter = Meter::create_counter(meter, "batch_counter_" + i.to_string(), None, None)
    Counter::add(counter, i.to_float())
    metrics = Array::append(metrics, counter)
    
    let histogram = Meter::create_histogram(meter, "batch_histogram_" + i.to_string(), None, None)
    Histogram::record(histogram, i.to_float() * 10.0)
    metrics = Array::append(metrics, histogram)
  }
  
  // Serialize batch of metrics
  let serialized_metrics_batch = Serialization::serialize_metric_batch(metrics)
  assert_true(serialized_metrics_batch.length() > 0)
  
  // Deserialize batch of metrics
  let deserialized_metrics_batch = Serialization::deserialize_metric_batch(serialized_metrics_batch)
  
  // Verify deserialized batch
  assert_eq(deserialized_metrics_batch.length(), metrics.length())
  
  for i in 0..=metrics.length() - 1 {
    assert_eq(Instrument::name(metrics[i]), Instrument::name(deserialized_metrics_batch[i]))
  }
  
  // Test case 3: Batch of log records
  let mut log_records = []
  
  for i in 0..=15 {
    let log_record = LogRecord::new_with_attributes(
      if i % 3 == 0 { Info } else if i % 3 == 1 { Warning } else { Error },
      Some("Batch log message " + i.to_string()),
      Attributes::with([
        ("index", IntValue(i)),
        ("batch", StringValue("test_log_batch"))
      ]),
      Some(1640995200L + i),
      Some(1640995200L + i + 10),
      Some("batch_trace"),
      Some("batch_span_" + i.to_string())
    )
    
    log_records = Array::append(log_records, log_record)
  }
  
  // Serialize batch of log records
  let serialized_logs_batch = Serialization::serialize_log_batch(log_records)
  assert_true(serialized_logs_batch.length() > 0)
  
  // Deserialize batch of log records
  let deserialized_logs_batch = Serialization::deserialize_log_batch(serialized_logs_batch)
  
  // Verify deserialized batch
  assert_eq(deserialized_logs_batch.length(), log_records.length())
  
  for i in 0..=log_records.length() - 1 {
    assert_eq(LogRecord::severity_number(log_records[i]), LogRecord::severity_number(deserialized_logs_batch[i]))
    
    match (LogRecord::body(log_records[i]), LogRecord::body(deserialized_logs_batch[i])) {
      (Some(original_body), Some(deserialized_body)) => {
        assert_eq(original_body, deserialized_body)
      }
      (None, None) => assert_true(true)
      _ => assert_true(false)
    }
  }
}

// Test 6: Cross-Format Serialization
test "cross-format serialization" {
  // Test case 1: JSON to Protocol Buffers
  let span_ctx = SpanContext::new("cross_format_trace", "cross_format_span", true, "test_state")
  let span = Span::new("cross_format_span", Internal, span_ctx)
  
  let attrs = Attributes::with([
    ("format.test", StringValue("cross_format")),
    ("conversion.test", BoolValue(true))
  ])
  
  Span::add_event(span, "cross_format_event", Some(attrs))
  Span::end(span)
  
  // Serialize to JSON
  let json_serialized = Serialization::serialize_span_to_json(span)
  assert_true(json_serialized.length() > 0)
  assert_true(String::starts_with(json_serialized, "{"))
  assert_true(String::ends_with(json_serialized, "}"))
  
  // Convert JSON to Protocol Buffers
  let protobuf_serialized = Serialization::json_to_protobuf(json_serialized)
  assert_true(protobuf_serialized.length() > 0)
  
  // Deserialize from Protocol Buffers
  let protobuf_deserialized = Serialization::deserialize_span_from_protobuf(protobuf_serialized)
  
  // Verify deserialized span
  assert_eq(Span::name(protobuf_deserialized), Span::name(span))
  assert_eq(Span::kind(protobuf_deserialized), Span::kind(span))
  
  // Test case 2: Protocol Buffers to JSON
  let histogram_provider = MeterProvider::default()
  let histogram_meter = MeterProvider::get_meter(histogram_provider, "cross_format_meter")
  let histogram = Meter::create_histogram(histogram_meter, "cross_format_histogram", None, None)
  
  Histogram::record(histogram, 100.0, Some(Attributes::with([("format", StringValue("protobuf"))])))
  Histogram::record(histogram, 200.0, Some(Attributes::with([("format", StringValue("json"))])))
  
  // Serialize to Protocol Buffers
  let protobuf_histogram = Serialization::serialize_metric_to_protobuf(histogram)
  assert_true(protobuf_histogram.length() > 0)
  
  // Convert Protocol Buffers to JSON
  let json_histogram = Serialization::protobuf_to_json(protobuf_histogram)
  assert_true(json_histogram.length() > 0)
  assert_true(String::starts_with(json_histogram, "{"))
  
  // Deserialize from JSON
  let json_deserialized = Serialization::deserialize_metric_from_json(json_histogram)
  
  // Verify deserialized histogram
  assert_eq(Instrument::name(json_deserialized), Instrument::name(histogram))
  assert_eq(Instrument::instrument_type(json_deserialized), Instrument::instrument_type(histogram))
  
  // Test case 3: Round-trip conversion
  let log_record = LogRecord::new_with_attributes(
    Info,
    Some("Round-trip test message"),
    Attributes::with([
      ("round.trip", StringValue("test")),
      ("conversion", StringValue("json_protobuf_json"))
    ]),
    Some(1640995200L),
    None,
    Some("round_trip_trace"),
    Some("round_trip_span")
  )
  
  // JSON -> Protocol Buffers -> JSON
  let json_original = Serialization::serialize_log_record_to_json(log_record)
  let protobuf = Serialization::json_to_protobuf(json_original)
  let json_round_trip = Serialization::protobuf_to_json(protobuf)
  
  // Should be equivalent (though formatting might differ)
  assert_true(json_round_trip.length() > 0)
  
  // Deserialize both and compare
  let original_deserialized = Serialization::deserialize_log_record_from_json(json_original)
  let round_trip_deserialized = Serialization::deserialize_log_record_from_json(json_round_trip)
  
  assert_eq(LogRecord::severity_number(original_deserialized), LogRecord::severity_number(round_trip_deserialized))
  
  match (LogRecord::body(original_deserialized), LogRecord::body(round_trip_deserialized)) {
    (Some(original_body), Some(round_trip_body)) => {
      assert_eq(original_body, round_trip_body)
    }
    (None, None) => assert_true(true)
    _ => assert_true(false)
  }
}