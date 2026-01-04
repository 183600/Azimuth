// Azimuth Telemetry System - Enhanced Basic Tests
// This file contains enhanced basic test cases for fundamental telemetry operations

// Test 1: Enhanced Attribute Operations
test "enhanced attribute operations" {
  let attrs = Attributes::new()
  
  // Test batch attribute setting
  let batch_attrs = [
    ("string.attr", StringValue("test_string")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true))
  ]
  
  for (key, value) in batch_attrs {
    Attributes::set(attrs, key, value)
  }
  
  // Verify all attributes are set correctly
  for (key, expected_value) in batch_attrs {
    let retrieved_value = Attributes::get(attrs, key)
    match retrieved_value {
      Some(v) => {
        match (v, expected_value) {
          (StringValue(a), StringValue(b)) => assert_eq(a, b)
          (IntValue(a), IntValue(b)) => assert_eq(a, b)
          (FloatValue(a), FloatValue(b)) => assert_eq(a, b)
          (BoolValue(a), BoolValue(b)) => assert_eq(a, b)
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // Test attribute removal
  Attributes::remove(attrs, "string.attr")
  let removed_attr = Attributes::get(attrs, "string.attr")
  match removed_attr {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test attribute enumeration
  let all_attrs = Attributes::list(attrs)
  assert_eq(all_attrs.length(), 3) // Should have 3 attributes after removal
}

// Test 2: Enhanced Span Context Operations
test "enhanced span context operations" {
  // Test span context with trace flags
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_flags = 0x01 // Sampled flag
  let span_ctx = SpanContext::new_with_flags(trace_id, span_id, trace_flags, "")
  
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test span context with trace state
  let trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  let span_ctx_with_state = SpanContext::new_with_state(trace_id, span_id, true, trace_state)
  
  assert_eq(SpanContext::trace_state(span_ctx_with_state), trace_state)
  
  // Test span context equality
  let identical_ctx = SpanContext::new_with_flags(trace_id, span_id, trace_flags, "")
  assert_true(SpanContext::equals(span_ctx, identical_ctx))
  
  let different_ctx = SpanContext::new("different_trace", span_id, true, "")
  assert_false(SpanContext::equals(span_ctx, different_ctx))
}

// Test 3: Enhanced Span Operations
test "enhanced span operations" {
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "")
  let span = Span::new("test_span", Server, span_ctx)
  
  // Test span with start time
  let start_time = Time::now()
  let span_with_time = Span::with_start_time("timed_span", Client, span_ctx, start_time)
  
  assert_eq(Span::name(span_with_time), "timed_span")
  assert_eq(Span::kind(span_with_time), Client)
  
  // Test span with attributes
  Span::set_attributes(span, [
    ("user.id", StringValue("user123")),
    ("operation.type", StringValue("query")),
    ("operation.duration", IntValue(1000))
  ])
  
  // Test span events with timestamps
  let event_time = Time::now()
  Span::add_event_with_time(span, "query_started", event_time, Some([
    ("query.type", StringValue("SELECT")),
    ("query.table", StringValue("users"))
  ]))
  
  // Test span links
  let linked_ctx = SpanContext::new("linked_trace", "linked_span", true, "")
  Span::add_link(span, linked_ctx, Some([
    ("link.type", StringValue("parent"))
  ]))
  
  // Test span status with detailed code
  Span::set_status_with_description(span, Error, Some("Database connection failed"), 
                                    Some("Connection timeout after 30 seconds"))
  
  assert_eq(Span::status(span), Error)
  
  Span::end(span)
  Span::end(span_with_time)
}

// Test 4: Enhanced Metrics Operations
test "enhanced metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "enhanced_meter")
  
  // Test counter with attributes
  let counter = Meter::create_counter(meter, "enhanced_counter", 
                                      Some("Enhanced counter with attributes"), Some("operations"))
  
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("operation.type", StringValue("create")),
    ("service.name", StringValue("auth-service"))
  ])))
  
  Counter::add(counter, 2.0, Some(Attributes::with([
    ("operation.type", StringValue("update")),
    ("service.name", StringValue("auth-service"))
  ])))
  
  // Test histogram with explicit boundaries
  let histogram = Meter::create_histogram_with_boundaries(meter, "enhanced_histogram", 
                                                          Some("Enhanced histogram"), Some("ms"),
                                                          [10.0, 50.0, 100.0, 500.0, 1000.0])
  
  Histogram::record(histogram, 25.0)
  Histogram::record(histogram, 75.0)
  Histogram::record(histogram, 150.0)
  Histogram::record(histogram, 750.0)
  
  // Test up-down counter with negative values
  let updown_counter = Meter::create_updown_counter(meter, "enhanced_updown", 
                                                    Some("Enhanced up-down counter"), Some("connections"))
  
  UpDownCounter::add(updown_counter, 10.0) // Add 10 connections
  UpDownCounter::add(updown_counter, -3.0) // Remove 3 connections
  
  // Test gauge with callback
  let gauge = Meter::create_gauge_with_callback(meter, "enhanced_gauge", 
                                                Some("Enhanced gauge with callback"), Some("percent"),
                                                fn() { 75.5 })
  
  // Test observable counter
  let observable_counter = Meter::create_observable_counter(meter, "enhanced_observable_counter",
                                                            Some("Enhanced observable counter"), Some("items"),
                                                            fn() { 42.0 })
}

// Test 5: Enhanced Logging Operations
test "enhanced logging operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "enhanced_logger")
  
  // Test log record with all fields
  let timestamp = Time::now()
  let log_attrs = Attributes::with([
    ("log.level", StringValue("INFO")),
    ("service.name", StringValue("payment-service")),
    ("transaction.id", StringValue("txn_123456"))
  ])
  
  let log_record = LogRecord::with_all_fields(
    Warn,
    Some("Payment processing completed with warnings"),
    Some(log_attrs),
    Some(timestamp),
    Some(1234567890L),
    Some("trace_1234567890abcdef"),
    Some("span_1234567890abcdef"),
    Some(0x01) // Trace flags
  )
  
  assert_eq(LogRecord::severity_number(log_record), Warn)
  
  // Test log record with severity text
  let log_with_severity = LogRecord::with_severity_text(
    Error,
    Some("Critical system error"),
    Some("FATAL"),
    Some(Attributes::with([
      ("error.code", StringValue("SYS_500")),
      ("error.type", StringValue("database"))
    ]))
  )
  
  assert_eq(LogRecord::severity_number(log_with_severity), Error)
  
  // Test batch log emission
  let log_records = [
    log_record,
    log_with_severity,
    LogRecord::new(Info, Some("Batch log message 1")),
    LogRecord::new(Info, Some("Batch log message 2"))
  ]
  
  for log in log_records {
    Logger::emit(logger, log)
  }
  
  // Test structured logging
  let structured_log = LogRecord::structured(
    Debug,
    "Structured log message",
    [
      ("user.id", "user123"),
      ("action", "login"),
      ("timestamp", "2023-01-01T12:00:00Z"),
      ("success", "true")
    ]
  )
  
  Logger::emit(logger, structured_log)
}

// Test 6: Enhanced Context Operations
test "enhanced context operations" {
  // Test context with multiple values
  let root_ctx = Context::root()
  
  let key1 = ContextKey::new("trace_id")
  let key2 = ContextKey::new("user_id")
  let key3 = ContextKey::new("request_id")
  
  let ctx1 = Context::with_value(root_ctx, key1, "trace_12345")
  let ctx2 = Context::with_value(ctx1, key2, "user_67890")
  let ctx3 = Context::with_value(ctx2, key3, "req_abcde")
  
  // Verify all values are accessible
  match Context::get(ctx3, key1) {
    Some(value) => assert_eq(value, "trace_12345")
    None => assert_true(false)
  }
  
  match Context::get(ctx3, key2) {
    Some(value) => assert_eq(value, "user_67890")
    None => assert_true(false)
  }
  
  match Context::get(ctx3, key3) {
    Some(value) => assert_eq(value, "req_abcde")
    None => assert_true(false)
  }
  
  // Test context with complex values
  let complex_key = ContextKey::new("complex_data")
  let complex_value = (42, "test", true) // Tuple value
  let ctx_with_complex = Context::with_value(ctx3, complex_key, complex_value)
  
  // Test context iteration
  let all_values = Context::list_values(ctx_with_complex)
  assert_eq(all_values.length(), 4) // Should have 4 key-value pairs
  
  // Test context clearing
  let cleared_ctx = Context::clear(ctx_with_complex)
  match cleared_ctx.data {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

// Test 7: Enhanced Baggage Operations
test "enhanced baggage operations" {
  let baggage = Baggage::new()
  
  // Test setting multiple baggage entries
  let entries = [
    ("user.id", "user123"),
    ("session.id", "session456"),
    ("request.origin", "mobile")
  ]
  
  let mut updated_baggage = baggage
  for (key, value) in entries {
    updated_baggage = Baggage::set_entry(updated_baggage, key, value)
  }
  
  // Verify all entries are set
  for (key, expected_value) in entries {
    let retrieved_value = Baggage::get_entry(updated_baggage, key)
    match retrieved_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // Test baggage with metadata
  let baggage_with_metadata = Baggage::set_entry_with_metadata(
    updated_baggage, 
    "special.entry", 
    "special_value", 
    "metadata=value;propagate=true"
  )
  
  // Test baggage enumeration
  let all_entries = Baggage::list_entries(baggage_with_metadata)
  assert_eq(all_entries.length(), 4) // Should have 4 entries
  
  // Test baggage serialization
  let serialized = Baggage::serialize(baggage_with_metadata)
  let deserialized = Baggage::deserialize(serialized)
  
  match deserialized {
    Some(b) => {
      let user_id = Baggage::get_entry(b, "user.id")
      match user_id {
        Some(value) => assert_eq(value, "user123")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 8: Enhanced Resource Operations
test "enhanced resource operations" {
  // Test resource with comprehensive attributes
  let resource_attrs = [
    ("service.name", StringValue("telemetry-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-abc123")),
    ("host.name", StringValue("prod-server-01")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("20.04")),
    ("deployment.environment", StringValue("production"))
  ]
  
  let resource = Resource::with_attributes(Resource::new(), resource_attrs)
  
  // Test resource detection
  let detected_resource = Resource::detect()
  assert_true(Resource::is_valid(detected_resource))
  
  // Test resource merging with conflicts
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("new.attr", StringValue("new_value"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource, override_resource)
  
  // Verify override behavior
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_true(false)
  }
  
  let new_attr = Resource::get_attribute(merged_resource, "new.attr")
  match new_attr {
    Some(StringValue(value)) => assert_eq(value, "new_value")
    _ => assert_true(false)
  }
  
  // Test resource schema URL
  let resource_with_schema = Resource::with_schema_url(merged_resource, "https://opentelemetry.io/schemas/v1.20.0")
  assert_eq(Resource::schema_url(resource_with_schema), Some("https://opentelemetry.io/schemas/v1.20.0"))
}