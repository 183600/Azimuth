// Serialization and Deserialization Tests
// This file contains comprehensive test cases for serialization and deserialization

// Test 1: Span JSON Serialization and Deserialization
test "span json serialization and deserialization" {
  // Create a test span
  let span_ctx = SpanContext::new("trace123", "span456", true, "test_state")
  let span = Span::new("test_operation", Server, span_ctx)
  
  // Add attributes
  Span::add_attribute(span, "service.name", StringValue("auth-service"))
  Span::add_attribute(span, "service.version", StringValue("1.0.0"))
  Span::add_attribute(span, "http.method", StringValue("GET"))
  Span::add_attribute(span, "http.status_code", IntValue(200))
  Span::add_attribute(span, "duration_ms", FloatValue(150.5))
  
  // Add events
  Span::add_event(span, "request_received", Some([
    ("timestamp", StringValue("1640995200")),
    ("request_size", IntValue(1024))
  ]))
  Span::add_event(span, "response_sent", Some([
    ("timestamp", StringValue("1640995350")),
    ("response_size", IntValue(2048))
  ]))
  
  // Set status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Serialize to JSON
  let json_result = SpanSerializer::to_json(span)
  match json_result {
    Ok(json) => {
      // Verify JSON structure
      assert_true(json.contains("\"name\":\"test_operation\""))
      assert_true(json.contains("\"kind\":\"Server\""))
      assert_true(json.contains("\"trace_id\":\"trace123\""))
      assert_true(json.contains("\"span_id\":\"span456\""))
      assert_true(json.contains("\"service.name\":\"auth-service\""))
      assert_true(json.contains("\"http.status_code\":200"))
      assert_true(json.contains("\"duration_ms\":150.5"))
      assert_true(json.contains("\"request_received\""))
      assert_true(json.contains("\"response_sent\""))
      
      // Deserialize from JSON
      let deserialization_result = SpanSerializer::from_json(json)
      match deserialization_result {
        Ok(deserialized_span) => {
          // Verify deserialized span
          assert_eq(Span::name(deserialized_span), "test_operation")
          match Span::kind(deserialized_span) {
            Server => assert_true(true)
            _ => assert_true(false)
          }
          
          let deserialized_ctx = Span::span_context(deserialized_span)
          assert_eq(SpanContext::trace_id(deserialized_ctx), "trace123")
          assert_eq(SpanContext::span_id(deserialized_ctx), "span456")
          assert_true(SpanContext::is_sampled(deserialized_ctx))
          
          // Verify attributes
          let service_name = Span::get_attribute(deserialized_span, "service.name")
          match service_name {
            Some(StringValue(name)) => assert_eq(name, "auth-service"),
            _ => assert_true(false)
          }
          
          let status_code = Span::get_attribute(deserialized_span, "http.status_code")
          match status_code {
            Some(IntValue(code)) => assert_eq(code, 200),
            _ => assert_true(false)
          }
          
          // Verify events
          let events = Span::events(deserialized_span)
          assert_eq(events.length(), 2)
          
          // Verify status
          assert_eq(Span::status(deserialized_span), Ok)
        }
        Err(error) => assert_true(false, "Deserialization failed: " + error)
      }
    }
    Err(error) => assert_true(false, "Serialization failed: " + error)
  }
}

// Test 2: Span Binary Serialization and Deserialization
test "span binary serialization and deserialization" {
  // Create a test span
  let span_ctx = SpanContext::new("binary_trace", "binary_span", false, "binary_state")
  let span = Span::new("binary_operation", Client, span_ctx)
  
  // Add attributes
  Span::add_attribute(span, "binary.test", StringValue("binary_value"))
  Span::add_attribute(span, "binary.number", IntValue(42))
  Span::add_attribute(span, "binary.float", FloatValue(3.14159))
  Span::add_attribute(span, "binary.bool", BoolValue(true))
  
  // Serialize to binary
  let binary_result = SpanSerializer::to_binary(span)
  match binary_result {
    Ok(binary) => {
      // Verify binary data is not empty
      assert_true(binary.length() > 0)
      
      // Deserialize from binary
      let deserialization_result = SpanSerializer::from_binary(binary)
      match deserialization_result {
        Ok(deserialized_span) => {
          // Verify deserialized span
          assert_eq(Span::name(deserialized_span), "binary_operation")
          match Span::kind(deserialized_span) {
            Client => assert_true(true)
            _ => assert_true(false)
          }
          
          let deserialized_ctx = Span::span_context(deserialized_span)
          assert_eq(SpanContext::trace_id(deserialized_ctx), "binary_trace")
          assert_eq(SpanContext::span_id(deserialized_ctx), "binary_span")
          assert_false(SpanContext::is_sampled(deserialized_ctx))
          
          // Verify attributes
          let binary_test = Span::get_attribute(deserialized_span, "binary.test")
          match binary_test {
            Some(StringValue(value)) => assert_eq(value, "binary_value"),
            _ => assert_true(false)
          }
          
          let binary_number = Span::get_attribute(deserialized_span, "binary.number")
          match binary_number {
            Some(IntValue(number)) => assert_eq(number, 42),
            _ => assert_true(false)
          }
          
          let binary_float = Span::get_attribute(deserialized_span, "binary.float")
          match binary_float {
            Some(FloatValue(float)) => assert_true((float - 3.14159).abs() < 0.00001),
            _ => assert_true(false)
          }
          
          let binary_bool = Span::get_attribute(deserialized_span, "binary.bool")
          match binary_bool {
            Some(BoolValue(bool)) => assert_true(bool),
            _ => assert_true(false)
          }
        }
        Err(error) => assert_true(false, "Binary deserialization failed: " + error)
      }
    }
    Err(error) => assert_true(false, "Binary serialization failed: " + error)
  }
}

// Test 3: Metrics Serialization and Deserialization
test "metrics serialization and deserialization" {
  // Create metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 100.0)
  Counter::add(counter, 50.0)
  
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 10.0)
  Histogram::record(histogram, 20.0)
  Histogram::record(histogram, 30.0)
  
  let updown_counter = Meter::create_updown_counter(meter, "test_updown", Some("Test updown"), Some("value"))
  UpDownCounter::add(updown_counter, 25.0)
  UpDownCounter::add(updown_counter, -5.0)
  
  // Serialize metrics to JSON
  let metrics_json_result = MetricsSerializer::to_json([counter, histogram, updown_counter])
  match metrics_json_result {
    Ok(json) => {
      // Verify JSON structure
      assert_true(json.contains("\"test_counter\""))
      assert_true(json.contains("\"test_histogram\""))
      assert_true(json.contains("\"test_updown\""))
      assert_true(json.contains("\"description\":\"Test counter\""))
      assert_true(json.contains("\"unit\":\"count\""))
      assert_true(json.contains("\"value\":150.0")) // 100 + 50
      assert_true(json.contains("\"sum\":60.0")) // 10 + 20 + 30
      assert_true(json.contains("\"count\":3"))
      assert_true(json.contains("\"value\":20.0")) // 25 - 5
      
      // Deserialize metrics from JSON
      let deserialization_result = MetricsSerializer::from_json(json)
      match deserialization_result {
        Ok(deserialized_metrics) => {
          // Verify deserialized metrics
          assert_eq(deserialized_metrics.length(), 3)
          
          // Find and verify counter
          let deserialized_counter = deserialized_metrics.find(fn(m) {
            Metric::name(m) == "test_counter"
          })
          match deserialized_counter {
            Some(counter) => {
              assert_eq(Metric::description(counter), Some("Test counter"))
              assert_eq(Metric::unit(counter), Some("count"))
              match Metric::data(counter) {
                CounterData(value) => assert_eq(value, 150.0),
                _ => assert_true(false)
              }
            }
            None => assert_true(false)
          }
          
          // Find and verify histogram
          let deserialized_histogram = deserialized_metrics.find(fn(m) {
            Metric::name(m) == "test_histogram"
          })
          match deserialized_histogram {
            Some(histogram) => {
              assert_eq(Metric::description(histogram), Some("Test histogram"))
              assert_eq(Metric::unit(histogram), Some("ms"))
              match Metric::data(histogram) {
                HistogramData(sum, count, min, max) => {
                  assert_eq(sum, 60.0)
                  assert_eq(count, 3)
                  assert_eq(min, 10.0)
                  assert_eq(max, 30.0)
                }
                _ => assert_true(false)
              }
            }
            None => assert_true(false)
          }
          
          // Find and verify updown counter
          let deserialized_updown = deserialized_metrics.find(fn(m) {
            Metric::name(m) == "test_updown"
          })
          match deserialized_updown {
            Some(updown) => {
              assert_eq(Metric::description(updown), Some("Test updown"))
              assert_eq(Metric::unit(updown), Some("value"))
              match Metric::data(updown) {
                UpDownCounterData(value) => assert_eq(value, 20.0),
                _ => assert_true(false)
              }
            }
            None => assert_true(false)
          }
        }
        Err(error) => assert_true(false, "Metrics deserialization failed: " + error)
      }
    }
    Err(error) => assert_true(false, "Metrics serialization failed: " + error)
  }
}

// Test 4: Log Record Serialization and Deserialization
test "log record serialization and deserialization" {
  // Create log records
  let log1 = LogRecord::new(Info, "Info log message")
  LogRecord::add_attribute(log1, "service.name", StringValue("test-service"))
  LogRecord::add_attribute(log1, "log.level", StringValue("info"))
  
  let log2 = LogRecord::new_with_timestamp(Error, "Error log message", 1640995200L)
  LogRecord::add_attribute(log2, "service.name", StringValue("test-service"))
  LogRecord::add_attribute(log2, "error.type", StringValue("TimeoutError"))
  LogRecord::add_attribute(log2, "error.message", StringValue("Operation timed out"))
  
  let log3 = LogRecord::new_with_trace_context(Warn, "Warning log message", 
    Some("trace123"), Some("span456"))
  LogRecord::add_attribute(log3, "service.name", StringValue("test-service"))
  LogRecord::add_attribute(log3, "warning.type", StringValue("ResourceWarning"))
  
  // Serialize log records to JSON
  let logs_json_result = LogSerializer::to_json([log1, log2, log3])
  match logs_json_result {
    Ok(json) => {
      // Verify JSON structure
      assert_true(json.contains("\"severity_number\":1")) // Info
      assert_true(json.contains("\"severity_number\":3")) // Error
      assert_true(json.contains("\"severity_number\":2")) // Warn
      assert_true(json.contains("\"Info log message\""))
      assert_true(json.contains("\"Error log message\""))
      assert_true(json.contains("\"Warning log message\""))
      assert_true(json.contains("\"service.name\":\"test-service\""))
      assert_true(json.contains("\"error.type\":\"TimeoutError\""))
      assert_true(json.contains("\"trace_id\":\"trace123\""))
      assert_true(json.contains("\"span_id\":\"span456\""))
      assert_true(json.contains("\"timestamp\":1640995200"))
      
      // Deserialize log records from JSON
      let deserialization_result = LogSerializer::from_json(json)
      match deserialization_result {
        Ok(deserialized_logs) => {
          // Verify deserialized logs
          assert_eq(deserialized_logs.length(), 3)
          
          // Verify info log
          let info_log = deserialized_logs[0]
          assert_eq(LogRecord::severity_number(info_log), Info)
          match LogRecord::body(info_log) {
            Some(body) => assert_eq(body, "Info log message"),
            None => assert_true(false)
          }
          
          let service_name = LogRecord::get_attribute(info_log, "service.name")
          match service_name {
            Some(StringValue(name)) => assert_eq(name, "test-service"),
            _ => assert_true(false)
          }
          
          // Verify error log
          let error_log = deserialized_logs[1]
          assert_eq(LogRecord::severity_number(error_log), Error)
          match LogRecord::body(error_log) {
            Some(body) => assert_eq(body, "Error log message"),
            None => assert_true(false)
          }
          
          match LogRecord::timestamp(error_log) {
            Some(timestamp) => assert_eq(timestamp, 1640995200L),
            None => assert_true(false)
          }
          
          // Verify warning log
          let warn_log = deserialized_logs[2]
          assert_eq(LogRecord::severity_number(warn_log), Warn)
          match LogRecord::body(warn_log) {
            Some(body) => assert_eq(body, "Warning log message"),
            None => assert_true(false)
          }
          
          match LogRecord::trace_id(warn_log) {
            Some(trace_id) => assert_eq(trace_id, "trace123"),
            None => assert_true(false)
          }
          
          match LogRecord::span_id(warn_log) {
            Some(span_id) => assert_eq(span_id, "span456"),
            None => assert_true(false)
          }
        }
        Err(error) => assert_true(false, "Log deserialization failed: " + error)
      }
    }
    Err(error) => assert_true(false, "Log serialization failed: " + error)
  }
}

// Test 5: Resource Serialization and Deserialization
test "resource serialization and deserialization" {
  // Create resource with attributes
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("os.type", StringValue("linux")),
    ("process.id", IntValue(12345)),
    ("process.name", StringValue("test-process"))
  ]
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Serialize resource to JSON
  let resource_json_result = ResourceSerializer::to_json(resource)
  match resource_json_result {
    Ok(json) => {
      // Verify JSON structure
      assert_true(json.contains("\"service.name\":\"test-service\""))
      assert_true(json.contains("\"service.version\":\"1.0.0\""))
      assert_true(json.contains("\"service.instance.id\":\"instance-123\""))
      assert_true(json.contains("\"host.name\":\"test-host\""))
      assert_true(json.contains("\"os.type\":\"linux\""))
      assert_true(json.contains("\"process.id\":12345"))
      assert_true(json.contains("\"process.name\":\"test-process\""))
      
      // Deserialize resource from JSON
      let deserialization_result = ResourceSerializer::from_json(json)
      match deserialization_result {
        Ok(deserialized_resource) => {
          // Verify deserialized resource attributes
          let service_name = Resource::get_attribute(deserialized_resource, "service.name")
          match service_name {
            Some(StringValue(name)) => assert_eq(name, "test-service"),
            _ => assert_true(false)
          }
          
          let service_version = Resource::get_attribute(deserialized_resource, "service.version")
          match service_version {
            Some(StringValue(version)) => assert_eq(version, "1.0.0"),
            _ => assert_true(false)
          }
          
          let service_instance_id = Resource::get_attribute(deserialized_resource, "service.instance.id")
          match service_instance_id {
            Some(StringValue(id)) => assert_eq(id, "instance-123"),
            _ => assert_true(false)
          }
          
          let host_name = Resource::get_attribute(deserialized_resource, "host.name")
          match host_name {
            Some(StringValue(name)) => assert_eq(name, "test-host"),
            _ => assert_true(false)
          }
          
          let os_type = Resource::get_attribute(deserialized_resource, "os.type")
          match os_type {
            Some(StringValue(os)) => assert_eq(os, "linux"),
            _ => assert_true(false)
          }
          
          let process_id = Resource::get_attribute(deserialized_resource, "process.id")
          match process_id {
            Some(IntValue(id)) => assert_eq(id, 12345),
            _ => assert_true(false)
          }
          
          let process_name = Resource::get_attribute(deserialized_resource, "process.name")
          match process_name {
            Some(StringValue(name)) => assert_eq(name, "test-process"),
            _ => assert_true(false)
          }
        }
        Err(error) => assert_true(false, "Resource deserialization failed: " + error)
      }
    }
    Err(error) => assert_true(false, "Resource serialization failed: " + error)
  }
}

// Test 6: Context Serialization and Deserialization
test "context serialization and deserialization" {
  // Create context with baggage
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_entries = Baggage::set_entry(baggage_with_entries, "request.source", "mobile")
  let baggage_with_entries = Baggage::set_entry(baggage_with_entries, "tenant.id", "tenant-001")
  
  let span_ctx = SpanContext::with_baggage("context_trace", "context_span", true, baggage_with_entries)
  
  // Serialize context to JSON
  let context_json_result = ContextSerializer::to_json(span_ctx)
  match context_json_result {
    Ok(json) => {
      // Verify JSON structure
      assert_true(json.contains("\"trace_id\":\"context_trace\""))
      assert_true(json.contains("\"span_id\":\"context_span\""))
      assert_true(json.contains("\"sampled\":true"))
      assert_true(json.contains("\"baggage\""))
      assert_true(json.contains("\"user.id\":\"12345\""))
      assert_true(json.contains("\"request.source\":\"mobile\""))
      assert_true(json.contains("\"tenant.id\":\"tenant-001\""))
      
      // Deserialize context from JSON
      let deserialization_result = ContextSerializer::from_json(json)
      match deserialization_result {
        Ok(deserialized_ctx) => {
          // Verify deserialized context
          assert_eq(SpanContext::trace_id(deserialized_ctx), "context_trace")
          assert_eq(SpanContext::span_id(deserialized_ctx), "context_span")
          assert_true(SpanContext::is_sampled(deserialized_ctx))
          
          // Verify baggage
          let deserialized_baggage = SpanContext::baggage(deserialized_ctx)
          let user_id = Baggage::get_entry(deserialized_baggage, "user.id")
          match user_id {
            Some(id) => assert_eq(id, "12345"),
            None => assert_true(false)
          }
          
          let request_source = Baggage::get_entry(deserialized_baggage, "request.source")
          match request_source {
            Some(source) => assert_eq(source, "mobile"),
            None => assert_true(false)
          }
          
          let tenant_id = Baggage::get_entry(deserialized_baggage, "tenant.id")
          match tenant_id {
            Some(id) => assert_eq(id, "tenant-001"),
            None => assert_true(false)
          }
        }
        Err(error) => assert_true(false, "Context deserialization failed: " + error)
      }
    }
    Err(error) => assert_true(false, "Context serialization failed: " + error)
  }
}

// Test 7: Telemetry Data Batch Serialization
test "telemetry data batch serialization" {
  // Create multiple telemetry data items
  let telemetry_data = []
  
  // Add spans
  for i in 0..5 {
    let span_name = "batch_span_" + i.to_string()
    let span_ctx = SpanContext::new("batch_trace", "batch_span_" + i.to_string(), true, "")
    let span = Span::new(span_name, Internal, span_ctx)
    Span::add_attribute(span, "index", IntValue(i))
    telemetry_data = telemetry_data + [TelemetryData::Span(span)]
  }
  
  // Add metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "batch_meter")
  
  for i in 0..3 {
    let counter = Meter::create_counter(meter, "batch_counter_" + i.to_string(), None, None)
    Counter::add(counter, i.to_int() as Float * 10.0)
    telemetry_data = telemetry_data + [TelemetryData::Metric(counter)]
  }
  
  // Add logs
  for i in 0..2 {
    let log = LogRecord::new(Info, "Batch log message " + i.to_string())
    LogRecord::add_attribute(log, "index", IntValue(i))
    telemetry_data = telemetry_data + [TelemetryData::Log(log)]
  }
  
  // Serialize batch to JSON
  let batch_json_result = BatchSerializer::to_json(telemetry_data)
  match batch_json_result {
    Ok(json) => {
      // Verify JSON structure
      assert_true(json.contains("\"batch_span_0\""))
      assert_true(json.contains("\"batch_span_4\""))
      assert_true(json.contains("\"batch_counter_0\""))
      assert_true(json.contains("\"batch_counter_2\""))
      assert_true(json.contains("\"Batch log message 0\""))
      assert_true(json.contains("\"Batch log message 1\""))
      
      // Deserialize batch from JSON
      let deserialization_result = BatchSerializer::from_json(json)
      match deserialization_result {
        Ok(deserialized_data) => {
          // Verify deserialized batch
          assert_eq(deserialized_data.length(), 10) // 5 spans + 3 metrics + 2 logs
          
          // Count different types
          let mut span_count = 0
          let mut metric_count = 0
          let mut log_count = 0
          
          for item in deserialized_data {
            match item {
              TelemetryData::Span(_) => span_count = span_count + 1
              TelemetryData::Metric(_) => metric_count = metric_count + 1
              TelemetryData::Log(_) => log_count = log_count + 1
            }
          }
          
          assert_eq(span_count, 5)
          assert_eq(metric_count, 3)
          assert_eq(log_count, 2)
        }
        Err(error) => assert_true(false, "Batch deserialization failed: " + error)
      }
    }
    Err(error) => assert_true(false, "Batch serialization failed: " + error)
  }
}

// Test 8: Serialization Format Compatibility
test "serialization format compatibility" {
  // Create a span with various attribute types
  let span_ctx = SpanContext::new("compat_trace", "compat_span", true, "")
  let span = Span::new("compat_operation", Server, span_ctx)
  
  // Add attributes of different types
  Span::add_attribute(span, "string_attr", StringValue("string_value"))
  Span::add_attribute(span, "int_attr", IntValue(42))
  Span::add_attribute(span, "float_attr", FloatValue(3.14159))
  Span::add_attribute(span, "bool_attr", BoolValue(true))
  Span::add_attribute(span, "array_string_attr", ArrayStringValue(["item1", "item2", "item3"]))
  Span::add_attribute(span, "array_int_attr", ArrayIntValue([1, 2, 3]))
  
  // Serialize to JSON
  let json_result = SpanSerializer::to_json(span)
  match json_result {
    Ok(json) => {
      // Serialize to binary
      let binary_result = SpanSerializer::to_binary(span)
      match binary_result {
        Ok(binary) => {
          // Deserialize from JSON
          let json_deserialization_result = SpanSerializer::from_json(json)
          match json_deserialization_result {
            Ok(json_deserialized) => {
              // Deserialize from binary
              let binary_deserialization_result = SpanSerializer::from_binary(binary)
              match binary_deserialization_result {
                Ok(binary_deserialized) => {
                  // Compare JSON and binary deserialized spans
                  assert_eq(Span::name(json_deserialized), Span::name(binary_deserialized))
                  assert_eq(Span::kind(json_deserialized), Span::kind(binary_deserialized))
                  
                  let json_ctx = Span::span_context(json_deserialized)
                  let binary_ctx = Span::span_context(binary_deserialized)
                  assert_eq(SpanContext::trace_id(json_ctx), SpanContext::trace_id(binary_ctx))
                  assert_eq(SpanContext::span_id(json_ctx), SpanContext::span_id(binary_ctx))
                  assert_eq(SpanContext::is_sampled(json_ctx), SpanContext::is_sampled(binary_ctx))
                  
                  // Compare attributes
                  let json_attrs = Span::attributes(json_deserialized)
                  let binary_attrs = Span::attributes(binary_deserialized)
                  assert_eq(json_attrs.length(), binary_attrs.length())
                  
                  // Verify all attributes match
                  for (key, value) in json_attrs {
                    let binary_value = binary_attrs.get(key)
                    match binary_value {
                      Some(v) => assert_eq(value, v),
                      None => assert_true(false)
                    }
                  }
                }
                Err(error) => assert_true(false, "Binary deserialization failed: " + error)
              }
            }
            Err(error) => assert_true(false, "JSON deserialization failed: " + error)
          }
        }
        Err(error) => assert_true(false, "Binary serialization failed: " + error)
      }
    }
    Err(error) => assert_true(false, "JSON serialization failed: " + error)
  }
}

// Test 9: Serialization Error Handling
test "serialization error handling" {
  // Test with invalid span (empty name)
  let invalid_span_ctx = SpanContext::new("error_trace", "error_span", true, "")
  let invalid_span = Span::new("", Internal, invalid_span_ctx)
  
  // Serialize invalid span
  let invalid_json_result = SpanSerializer::to_json(invalid_span)
  match invalid_json_result {
    Ok(_) => assert_true(false, "Should fail with invalid span"),
    Err(error) => assert_true(error.length() > 0)
  }
  
  // Test with invalid JSON
  let invalid_json = "{ invalid json format }"
  let invalid_deserialization_result = SpanSerializer::from_json(invalid_json)
  match invalid_deserialization_result {
    Ok(_) => assert_true(false, "Should fail with invalid JSON"),
    Err(error) => assert_true(error.length() > 0)
  }
  
  // Test with truncated binary data
  let truncated_binary = [1, 2, 3] // Incomplete binary data
  let binary_deserialization_result = SpanSerializer::from_binary(truncated_binary)
  match binary_deserialization_result {
    Ok(_) => assert_true(false, "Should fail with truncated binary"),
    Err(error) => assert_true(error.length() > 0)
  }
  
  // Test with malformed span data
  let malformed_json = "{\"name\":\"test\",\"kind\":\"InvalidKind\"}"
  let malformed_deserialization_result = SpanSerializer::from_json(malformed_json)
  match malformed_deserialization_result {
    Ok(_) => assert_true(false, "Should fail with malformed span data"),
    Err(error) => assert_true(error.length() > 0)
  }
  
  // Test recovery from serialization errors
  let valid_span_ctx = SpanContext::new("recovery_trace", "recovery_span", true, "")
  let valid_span = Span::new("recovery_operation", Server, valid_span_ctx)
  
  // Try to serialize with error recovery
  let recovery_result = SpanSerializer::to_json_with_recovery(valid_span, fn(span) {
    // Recovery function: fix empty name
    if Span::name(span).length() == 0 {
      Span::with_name(span, "recovered_operation")
    } else {
      span
    }
  })
  
  match recovery_result {
    Ok(json) => {
      // Should succeed with valid span
      assert_true(json.contains("\"name\":\"recovered_operation\""))
    }
    Err(error) => assert_true(false, "Recovery should succeed: " + error)
  }
}

// Test 10: Serialization Performance
test "serialization performance" {
  // Create a large number of spans for performance testing
  let spans = []
  for i in 0..1000 {
    let span_name = "perf_span_" + i.to_string()
    let span_ctx = SpanContext::new("perf_trace", "perf_span_" + i.to_string(), true, "")
    let span = Span::new(span_name, Internal, span_ctx)
    
    // Add attributes
    Span::add_attribute(span, "index", IntValue(i))
    Span::add_attribute(span, "service.name", StringValue("perf-service"))
    Span::add_attribute(span, "operation.name", StringValue("perf-operation"))
    
    // Add events
    Span::add_event(span, "start_event", None)
    Span::add_event(span, "end_event", None)
    
    spans = spans + [span]
  }
  
  // Test JSON serialization performance
  let json_start = PerformanceTimer::start()
  
  let mut json_results = []
  for span in spans {
    let json_result = SpanSerializer::to_json(span)
    match json_result {
      Ok(json) => json_results = json_results + [json],
      Err(_) => assert_true(false)
    }
  }
  
  let json_end = PerformanceTimer::end()
  let json_time = PerformanceTimer::elapsed_ms(json_start, json_end)
  
  // Verify JSON serialization performance
  assert_true(json_time < 5000.0) // Less than 5 seconds for 1000 spans
  assert_eq(json_results.length(), 1000)
  
  // Test JSON deserialization performance
  let json_deserialization_start = PerformanceTimer::start()
  
  let mut deserialized_spans = []
  for json in json_results {
    let deserialization_result = SpanSerializer::from_json(json)
    match deserialization_result {
      Ok(span) => deserialized_spans = deserialized_spans + [span],
      Err(_) => assert_true(false)
    }
  }
  
  let json_deserialization_end = PerformanceTimer::end()
  let json_deserialization_time = PerformanceTimer::elapsed_ms(json_deserialization_start, json_deserialization_end)
  
  // Verify JSON deserialization performance
  assert_true(json_deserialization_time < 5000.0) // Less than 5 seconds for 1000 spans
  assert_eq(deserialized_spans.length(), 1000)
  
  // Test binary serialization performance
  let binary_start = PerformanceTimer::start()
  
  let mut binary_results = []
  for span in spans {
    let binary_result = SpanSerializer::to_binary(span)
    match binary_result {
      Ok(binary) => binary_results = binary_results + [binary],
      Err(_) => assert_true(false)
    }
  }
  
  let binary_end = PerformanceTimer::end()
  let binary_time = PerformanceTimer::elapsed_ms(binary_start, binary_end)
  
  // Verify binary serialization performance
  assert_true(binary_time < 3000.0) // Less than 3 seconds for 1000 spans
  assert_eq(binary_results.length(), 1000)
  
  // Compare JSON vs binary performance
  assert_true(binary_time < json_time) // Binary should be faster
  
  // Compare JSON vs binary size
  let json_total_size = json_results.fold_left(0, fn(acc, json) { acc + json.length() })
  let binary_total_size = binary_results.fold_left(0, fn(acc, binary) { acc + binary.length() })
  
  // Binary should be more compact
  assert_true(binary_total_size < json_total_size)
  let compression_ratio = (binary_total_size as Float) / (json_total_size as Float)
  assert_true(compression_ratio < 0.8) // Binary should be at least 20% smaller
}