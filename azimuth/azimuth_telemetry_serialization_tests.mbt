// Telemetry Data Serialization/Deserialization Tests for Azimuth
// This file contains test cases for serialization and deserialization of telemetry data

test "attribute value serialization" {
  // Test serialization of different attribute value types
  let string_attr = azimuth::StringValue("test.string.value")
  let int_attr = azimuth::IntValue(42)
  let float_attr = azimuth::FloatValue(3.14159)
  let bool_attr = azimuth::BoolValue(true)
  let array_string_attr = azimuth::ArrayStringValue(["item1", "item2", "item3"])
  let array_int_attr = azimuth::ArrayIntValue([1, 2, 3, 4, 5])
  
  // Simulate serialization to string representation
  let string_serialized = match string_attr {
    azimuth::StringValue(s) => "string:" + s
    _ => "unknown"
  }
  
  let int_serialized = match int_attr {
    azimuth::IntValue(i) => "int:" + i.to_string()
    _ => "unknown"
  }
  
  let float_serialized = match float_attr {
    azimuth::FloatValue(f) => "float:" + f.to_string()
    _ => "unknown"
  }
  
  let bool_serialized = match bool_attr {
    azimuth::BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
    _ => "unknown"
  }
  
  let array_string_serialized = match array_string_attr {
    azimuth::ArrayStringValue(arr) => "array_string:" + arr.join(",")
    _ => "unknown"
  }
  
  let array_int_serialized = match array_int_attr {
    azimuth::ArrayIntValue(arr) => {
      let str_values = []
      for i in arr {
        str_values.push(i.to_string())
      }
      "array_int:" + str_values.join(",")
    }
    _ => "unknown"
  }
  
  // Verify serialization results
  assert_eq(string_serialized, "string:test.string.value")
  assert_eq(int_serialized, "int:42")
  assert_eq(float_serialized, "float:3.14159")
  assert_eq(bool_serialized, "bool:true")
  assert_eq(array_string_serialized, "array_string:item1,item2,item3")
  assert_eq(array_int_serialized, "array_int:1,2,3,4,5")
  
  // Simulate deserialization back to attribute values
  let deserialized_string = if string_serialized.starts_with("string:") {
    let value = string_serialized.substring(7)  // Remove "string:" prefix
    azimuth::StringValue(value)
  } else {
    azimuth::StringValue("")
  }
  
  let deserialized_int = if int_serialized.starts_with("int:") {
    let value_str = int_serialized.substring(4)  // Remove "int:" prefix
    azimuth::IntValue(value_str.to_int())
  } else {
    azimuth::IntValue(0)
  }
  
  let deserialized_float = if float_serialized.starts_with("float:") {
    let value_str = float_serialized.substring(6)  // Remove "float:" prefix
    azimuth::FloatValue(value_str.to_double())
  } else {
    azimuth::FloatValue(0.0)
  }
  
  let deserialized_bool = if bool_serialized.starts_with("bool:") {
    let value_str = bool_serialized.substring(5)  // Remove "bool:" prefix
    azimuth::BoolValue(value_str == "true")
  } else {
    azimuth::BoolValue(false)
  }
  
  // Verify deserialization results
  assert_eq(deserialized_string, string_attr)
  assert_eq(deserialized_int, int_attr)
  assert_eq(deserialized_float, float_attr)
  assert_eq(deserialized_bool, bool_attr)
}

test "span context serialization" {
  // Test serialization and deserialization of span context
  let trace_id = "12345678901234567890123456789012"
  let span_id = "1234567890123456"
  let sampled = true
  let trace_state = "key1=value1,key2=value2"
  
  let span_ctx = azimuth::SpanContext::new(trace_id, span_id, sampled, trace_state)
  
  // Simulate serialization to traceparent format (W3C Trace Context)
  let traceparent_serialized = "00-" + trace_id + "-" + span_id + "-" + (if sampled { "01" } else { "00" })
  
  // Simulate serialization of trace state
  let tracestate_serialized = trace_state
  
  // Verify serialization results
  assert_eq(traceparent_serialized, "00-12345678901234567890123456789012-1234567890123456-01")
  assert_eq(tracestate_serialized, "key1=value1,key2=value2")
  
  // Simulate deserialization from traceparent format
  let deserialized_trace_id = if traceparent_serialized.length() >= 35 {
    traceparent_serialized.substring(3, 35)
  } else {
    ""
  }
  
  let deserialized_span_id = if traceparent_serialized.length() >= 52 {
    traceparent_serialized.substring(36, 52)
  } else {
    ""
  }
  
  let deserialized_sampled = if traceparent_serialized.length() >= 54 {
    traceparent_serialized.substring(53, 55) == "01"
  } else {
    false
  }
  
  // Verify deserialization results
  assert_eq(deserialized_trace_id, trace_id)
  assert_eq(deserialized_span_id, span_id)
  assert_eq(deserialized_sampled, sampled)
  
  // Create deserialized span context
  let deserialized_span_ctx = azimuth::SpanContext::new(deserialized_trace_id, deserialized_span_id, deserialized_sampled, tracestate_serialized)
  
  // Verify deserialized span context properties
  assert_eq(azimuth::SpanContext::trace_id(deserialized_span_ctx), trace_id)
  assert_eq(azimuth::SpanContext::span_id(deserialized_span_ctx), span_id)
  assert_eq(azimuth::SpanContext::is_sampled(deserialized_span_ctx), sampled)
}

test "log record serialization" {
  // Test serialization and deserialization of log records
  let clock = azimuth::Clock::system()
  let timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Test log message with serialization"),
    Some(azimuth::Attributes::new()),
    Some(timestamp),
    Some(timestamp + 1000L),
    Some("12345678901234567890123456789012"),
    Some("1234567890123456"),
    Some(azimuth::Context::root())
  )
  
  // Simulate serialization to JSON-like structure
  let severity_num = match azimuth::LogRecord::severity_number(log_record) {
    azimuth::Trace => "TRACE"
    azimuth::Debug => "DEBUG"
    azimuth::Info => "INFO"
    azimuth::Warn => "WARN"
    azimuth::Error => "ERROR"
    azimuth::Fatal => "FATAL"
  }
  
  let body = azimuth::LogRecord::body(log_record).unwrap()
  let timestamp_str = azimuth::LogRecord::timestamp(log_record).unwrap().to_string()
  let trace_id_str = azimuth::LogRecord::trace_id(log_record).unwrap()
  let span_id_str = azimuth::LogRecord::span_id(log_record).unwrap()
  
  let serialized_log = {
    "severity": severity_num,
    "body": body,
    "timestamp": timestamp_str,
    "trace_id": trace_id_str,
    "span_id": span_id_str
  }
  
  // Verify serialization results
  assert_eq(serialized_log["severity"], "WARN")
  assert_eq(serialized_log["body"], "Test log message with serialization")
  assert_eq(serialized_log["trace_id"], "12345678901234567890123456789012")
  assert_eq(serialized_log["span_id"], "1234567890123456")
  
  // Simulate deserialization back to log record
  let deserialized_severity = match serialized_log["severity"] {
    "TRACE" => azimuth::Trace
    "DEBUG" => azimuth::Debug
    "INFO" => azimuth::Info
    "WARN" => azimuth::Warn
    "ERROR" => azimuth::Error
    "FATAL" => azimuth::Fatal
    _ => azimuth::Info
  }
  
  let deserialized_body = Some(serialized_log["body"])
  let deserialized_timestamp = Some(serialized_log["timestamp"].to_int64())
  let deserialized_trace_id = Some(serialized_log["trace_id"])
  let deserialized_span_id = Some(serialized_log["span_id"])
  
  // Create deserialized log record
  let deserialized_log_record = azimuth::LogRecord::new_with_context(
    deserialized_severity,
    deserialized_body,
    Some(azimuth::Attributes::new()),
    deserialized_timestamp,
    None,
    deserialized_trace_id,
    deserialized_span_id,
    Some(azimuth::Context::root())
  )
  
  // Verify deserialization results
  assert_eq(azimuth::LogRecord::severity_number(deserialized_log_record), azimuth::LogRecord::severity_number(log_record))
  assert_eq(azimuth::LogRecord::body(deserialized_log_record), azimuth::LogRecord::body(log_record))
  assert_eq(azimuth::LogRecord::trace_id(deserialized_log_record), azimuth::LogRecord::trace_id(log_record))
  assert_eq(azimuth::LogRecord::span_id(deserialized_log_record), azimuth::LogRecord::span_id(log_record))
}

test "resource serialization" {
  // Test serialization and deserialization of resources
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.2.3")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("host-001")),
    ("process.pid", azimuth::IntValue(12345)),
    ("process.cpu.usage", azimuth::FloatValue(0.75)),
    ("service.telemetry.enabled", azimuth::BoolValue(true))
  ])
  
  // Simulate serialization to map structure
  let serialized_resource = {}
  
  for attr in resource.attributes {
    let key = attr.0
    let value = attr.1
    
    let serialized_value = match value {
      azimuth::StringValue(s) => s
      azimuth::IntValue(i) => i.to_string()
      azimuth::FloatValue(f) => f.to_string()
      azimuth::BoolValue(b) => (if b { "true" } else { "false" })
      _ => "unsupported"
    }
    
    serialized_resource[key] = serialized_value
  }
  
  // Verify serialization results
  assert_eq(serialized_resource["service.name"], "test-service")
  assert_eq(serialized_resource["service.version"], "1.2.3")
  assert_eq(serialized_resource["service.instance.id"], "instance-123")
  assert_eq(serialized_resource["deployment.environment"], "production")
  assert_eq(serialized_resource["host.name"], "host-001")
  assert_eq(serialized_resource["process.pid"], "12345")
  assert_eq(serialized_resource["process.cpu.usage"], "0.75")
  assert_eq(serialized_resource["service.telemetry.enabled"], "true")
  
  // Simulate deserialization back to resource
  let deserialized_attrs = []
  
  for (key, value) in serialized_resource {
    let deserialized_value = match key {
      "service.name" | "service.version" | "service.instance.id" | "deployment.environment" | "host.name" => 
        azimuth::StringValue(value)
      "process.pid" => azimuth::IntValue(value.to_int())
      "process.cpu.usage" => azimuth::FloatValue(value.to_double())
      "service.telemetry.enabled" => azimuth::BoolValue(value == "true")
      _ => azimuth::StringValue(value)
    }
    
    deserialized_attrs.push((key, deserialized_value))
  }
  
  let deserialized_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), deserialized_attrs)
  
  // Verify deserialization results
  let original_service_name = azimuth::Resource::get_attribute(resource, "service.name")
  let deserialized_service_name = azimuth::Resource::get_attribute(deserialized_resource, "service.name")
  assert_eq(original_service_name, deserialized_service_name)
  
  let original_pid = azimuth::Resource::get_attribute(resource, "process.pid")
  let deserialized_pid = azimuth::Resource::get_attribute(deserialized_resource, "process.pid")
  assert_eq(original_pid, deserialized_pid)
  
  let original_cpu_usage = azimuth::Resource::get_attribute(resource, "process.cpu.usage")
  let deserialized_cpu_usage = azimuth::Resource::get_attribute(deserialized_resource, "process.cpu.usage")
  assert_eq(original_cpu_usage, deserialized_cpu_usage)
}

test "baggage serialization" {
  // Test serialization and deserialization of baggage
  let baggage = azimuth::Baggage::new()
  
  // Add baggage entries
  let baggage_with_entries = azimuth::Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_entries = azimuth::Baggage::set_entry(baggage_with_entries, "request.id", "req456")
  let baggage_with_entries = azimuth::Baggage::set_entry(baggage_with_entries, "tenant.id", "tenant789")
  let baggage_with_entries = azimuth::Baggage::set_entry(baggage_with_entries, "session.id", "session001")
  
  // Simulate serialization to W3C Baggage header format
  let baggage_entries = [
    "user.id=user123",
    "request.id=req456",
    "tenant.id=tenant789",
    "session.id=session001"
  ]
  let serialized_baggage = baggage_entries.join(",")
  
  // Verify serialization result
  assert_eq(serialized_baggage, "user.id=user123,request.id=req456,tenant.id=tenant789,session.id=session001")
  
  // Simulate deserialization from baggage header format
  let deserialized_baggage = azimuth::Baggage::new()
  let entries = serialized_baggage.split(",")
  
  for entry in entries {
    let parts = entry.split("=")
    if parts.length() == 2 {
      let key = parts[0]
      let value = parts[1]
      deserialized_baggage = azimuth::Baggage::set_entry(deserialized_baggage, key, value)
    }
  }
  
  // Verify deserialization results
  let original_user_id = azimuth::Baggage::get_entry(baggage_with_entries, "user.id")
  let deserialized_user_id = azimuth::Baggage::get_entry(deserialized_baggage, "user.id")
  assert_eq(original_user_id, deserialized_user_id)
  
  let original_request_id = azimuth::Baggage::get_entry(baggage_with_entries, "request.id")
  let deserialized_request_id = azimuth::Baggage::get_entry(deserialized_baggage, "request.id")
  assert_eq(original_request_id, deserialized_request_id)
  
  let original_tenant_id = azimuth::Baggage::get_entry(baggage_with_entries, "tenant.id")
  let deserialized_tenant_id = azimuth::Baggage::get_entry(deserialized_baggage, "tenant.id")
  assert_eq(original_tenant_id, deserialized_tenant_id)
  
  let original_session_id = azimuth::Baggage::get_entry(baggage_with_entries, "session.id")
  let deserialized_session_id = azimuth::Baggage::get_entry(deserialized_baggage, "session.id")
  assert_eq(original_session_id, deserialized_session_id)
}

test "instrument serialization" {
  // Test serialization and deserialization of metric instruments
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "serialization-test-meter")
  
  // Create different types of instruments
  let counter = azimuth::Meter::create_counter(meter, "test.counter", Some("Test counter metric"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "test.histogram", Some("Test histogram metric"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "test.updown", Some("Test up-down counter"), Some("bytes"))
  let gauge = azimuth::Meter::create_gauge(meter, "test.gauge", Some("Test gauge metric"), Some("percent"))
  
  // Get instruments as enum for serialization
  let counter_instrument = azimuth::Counter::as_instrument(counter)
  let histogram_instrument = azimuth::Histogram::as_instrument(histogram)
  let updown_counter_instrument = azimuth::UpDownCounter::as_instrument(updown_counter)
  let gauge_instrument = azimuth::Gauge::as_instrument(gauge)
  
  // Simulate serialization to JSON-like structure
  let serialize_instrument = func(instrument : azimuth::Instrument) {
    let name = azimuth::Instrument::name(instrument)
    let description = azimuth::Instrument::description(instrument).unwrap_or("")
    let unit = azimuth::Instrument::unit(instrument).unwrap_or("")
    
    let type_str = match instrument {
      azimuth::Counter(_, _, _) => "counter"
      azimuth::Histogram(_, _, _) => "histogram"
      azimuth::UpDownCounter(_, _, _) => "updown_counter"
      azimuth::Gauge(_, _, _) => "gauge"
    }
    
    {
      "name": name,
      "type": type_str,
      "description": description,
      "unit": unit
    }
  }
  
  let serialized_counter = serialize_instrument(counter_instrument)
  let serialized_histogram = serialize_instrument(histogram_instrument)
  let serialized_updown_counter = serialize_instrument(updown_counter_instrument)
  let serialized_gauge = serialize_instrument(gauge_instrument)
  
  // Verify serialization results
  assert_eq(serialized_counter["name"], "test.counter")
  assert_eq(serialized_counter["type"], "counter")
  assert_eq(serialized_counter["description"], "Test counter metric")
  assert_eq(serialized_counter["unit"], "count")
  
  assert_eq(serialized_histogram["name"], "test.histogram")
  assert_eq(serialized_histogram["type"], "histogram")
  assert_eq(serialized_histogram["description"], "Test histogram metric")
  assert_eq(serialized_histogram["unit"], "ms")
  
  assert_eq(serialized_updown_counter["name"], "test.updown")
  assert_eq(serialized_updown_counter["type"], "updown_counter")
  assert_eq(serialized_updown_counter["description"], "Test up-down counter")
  assert_eq(serialized_updown_counter["unit"], "bytes")
  
  assert_eq(serialized_gauge["name"], "test.gauge")
  assert_eq(serialized_gauge["type"], "gauge")
  assert_eq(serialized_gauge["description"], "Test gauge metric")
  assert_eq(serialized_gauge["unit"], "percent")
  
  // Simulate deserialization back to instruments
  let deserialize_instrument = func(serialized : Map[String, String]) {
    let name = serialized["name"]
    let description = if serialized["description"] == "" { None } else { Some(serialized["description"]) }
    let unit = if serialized["unit"] == "" { None } else { Some(serialized["unit"]) }
    
    match serialized["type"] {
      "counter" => azimuth::Counter(name, description, unit)
      "histogram" => azimuth::Histogram(name, description, unit)
      "updown_counter" => azimuth::UpDownCounter(name, description, unit)
      "gauge" => azimuth::Gauge(name, description, unit)
      _ => azimuth::Counter("", None, None)
    }
  }
  
  let deserialized_counter = deserialize_instrument(serialized_counter)
  let deserialized_histogram = deserialize_instrument(serialized_histogram)
  let deserialized_updown_counter = deserialize_instrument(serialized_updown_counter)
  let deserialized_gauge = deserialize_instrument(serialized_gauge)
  
  // Verify deserialization results
  assert_eq(azimuth::Instrument::name(deserialized_counter), azimuth::Instrument::name(counter_instrument))
  assert_eq(azimuth::Instrument::description(deserialized_counter), azimuth::Instrument::description(counter_instrument))
  assert_eq(azimuth::Instrument::unit(deserialized_counter), azimuth::Instrument::unit(counter_instrument))
  
  assert_eq(azimuth::Instrument::name(deserialized_histogram), azimuth::Instrument::name(histogram_instrument))
  assert_eq(azimuth::Instrument::description(deserialized_histogram), azimuth::Instrument::description(histogram_instrument))
  assert_eq(azimuth::Instrument::unit(deserialized_histogram), azimuth::Instrument::unit(histogram_instrument))
  
  assert_eq(azimuth::Instrument::name(deserialized_updown_counter), azimuth::Instrument::name(updown_counter_instrument))
  assert_eq(azimuth::Instrument::description(deserialized_updown_counter), azimuth::Instrument::description(updown_counter_instrument))
  assert_eq(azimuth::Instrument::unit(deserialized_updown_counter), azimuth::Instrument::unit(updown_counter_instrument))
  
  assert_eq(azimuth::Instrument::name(deserialized_gauge), azimuth::Instrument::name(gauge_instrument))
  assert_eq(azimuth::Instrument::description(deserialized_gauge), azimuth::Instrument::description(gauge_instrument))
  assert_eq(azimuth::Instrument::unit(deserialized_gauge), azimuth::Instrument::unit(gauge_instrument))
}

test "complex telemetry data serialization" {
  // Test serialization of complex telemetry data with nested structures
  let clock = azimuth::Clock::system()
  let timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // Create complex telemetry data
  let trace_id = "12345678901234567890123456789012"
  let span_id = "1234567890123456"
  
  let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "complex-test")
  
  let span = azimuth::Tracer::start_span(tracer, "complex-operation")
  
  // Add events with complex attributes
  let complex_attrs = [
    ("operation.type", azimuth::StringValue("database.query")),
    ("operation.duration_ms", azimuth::IntValue(150)),
    ("operation.success", azimuth::BoolValue(true)),
    ("operation.result_count", azimuth::IntValue(42)),
    ("operation.tags", azimuth::ArrayStringValue(["tag1", "tag2", "tag3"]))
  ]
  azimuth::Span::add_event(span, "operation.completed", Some(complex_attrs))
  
  // Create complex log record
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "log.level", azimuth::StringValue("INFO"))
  azimuth::Attributes::set(log_attrs, "log.source", azimuth::StringValue("database.service"))
  azimuth::Attributes::set(log_attrs, "log.correlation_id", azimuth::StringValue("corr-12345"))
  
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Complex operation completed successfully"),
    Some(log_attrs),
    Some(timestamp),
    Some(timestamp + 50000L),
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Create complex resource
  let resource_attrs = [
    ("service.name", azimuth::StringValue("complex-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("service.namespace", azimuth::StringValue("production")),
    ("service.instance.id", azimuth::StringValue("instance-complex-001")),
    ("host.name", azimuth::StringValue("host-complex-001")),
    ("host.ip", azimuth::ArrayStringValue(["192.168.1.100", "10.0.0.100"])),
    ("process.pid", azimuth::IntValue(9876)),
    ("process.cpu.usage", azimuth::FloatValue(0.85)),
    ("process.memory.usage", azimuth::FloatValue(0.65)),
    ("service.telemetry.enabled", azimuth::BoolValue(true))
  ]
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_attrs)
  
  // Create complex baggage
  let baggage = azimuth::Baggage::new()
  let baggage = azimuth::Baggage::set_entry(baggage, "user.id", "user-complex-123")
  let baggage = azimuth::Baggage::set_entry(baggage, "session.id", "session-complex-456")
  let baggage = azimuth::Baggage::set_entry(baggage, "request.id", "req-complex-789")
  let baggage = azimuth::Baggage::set_entry(baggage, "tenant.id", "tenant-complex-000")
  
  // Simulate serialization of complex telemetry data
  let complex_telemetry_data = {
    "span": {
      "name": azimuth::Span::name(span),
      "kind": match azimuth::Span::kind(span) {
        azimuth::Internal => "Internal"
        azimuth::Server => "Server"
        azimuth::Client => "Client"
        azimuth::Producer => "Producer"
        azimuth::Consumer => "Consumer"
      },
      "context": {
        "trace_id": trace_id,
        "span_id": span_id,
        "sampled": "true",
        "trace_state": "key1=value1,key2=value2"
      },
      "events": [
        {
          "name": "operation.completed",
          "attributes": {
            "operation.type": "database.query",
            "operation.duration_ms": "150",
            "operation.success": "true",
            "operation.result_count": "42",
            "operation.tags": "tag1,tag2,tag3"
          }
        }
      ]
    },
    "log": {
      "severity": "INFO",
      "body": "Complex operation completed successfully",
      "timestamp": timestamp.to_string(),
      "trace_id": trace_id,
      "span_id": span_id,
      "attributes": {
        "log.level": "INFO",
        "log.source": "database.service",
        "log.correlation_id": "corr-12345"
      }
    },
    "resource": {
      "service.name": "complex-service",
      "service.version": "2.1.0",
      "service.namespace": "production",
      "service.instance.id": "instance-complex-001",
      "host.name": "host-complex-001",
      "host.ip": "192.168.1.100,10.0.0.100",
      "process.pid": "9876",
      "process.cpu.usage": "0.85",
      "process.memory.usage": "0.65",
      "service.telemetry.enabled": "true"
    },
    "baggage": "user.id=user-complex-123,session.id=session-complex-456,request.id=req-complex-789,tenant.id=tenant-complex-000"
  }
  
  // Verify serialization results
  assert_eq(complex_telemetry_data["span"]["name"], "complex-operation")
  assert_eq(complex_telemetry_data["span"]["context"]["trace_id"], trace_id)
  assert_eq(complex_telemetry_data["span"]["context"]["span_id"], span_id)
  assert_eq(complex_telemetry_data["log"]["body"], "Complex operation completed successfully")
  assert_eq(complex_telemetry_data["resource"]["service.name"], "complex-service")
  assert_eq(complex_telemetry_data["baggage"], "user.id=user-complex-123,session.id=session-complex-456,request.id=req-complex-789,tenant.id=tenant-complex-000")
  
  // Simulate deserialization and verification
  let deserialized_trace_id = complex_telemetry_data["span"]["context"]["trace_id"]
  let deserialized_span_id = complex_telemetry_data["span"]["context"]["span_id"]
  let deserialized_sampled = complex_telemetry_data["span"]["context"]["sampled"] == "true"
  
  // Verify deserialized data matches original
  assert_eq(deserialized_trace_id, trace_id)
  assert_eq(deserialized_span_id, span_id)
  assert_eq(deserialized_sampled, true)
  
  // Verify complex attributes
  let event_attrs = complex_telemetry_data["span"]["events"][0]["attributes"]
  assert_eq(event_attrs["operation.type"], "database.query")
  assert_eq(event_attrs["operation.duration_ms"], "150")
  assert_eq(event_attrs["operation.success"], "true")
  assert_eq(event_attrs["operation.result_count"], "42")
  assert_eq(event_attrs["operation.tags"], "tag1,tag2,tag3")
}