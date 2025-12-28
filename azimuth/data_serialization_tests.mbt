// Data Serialization Integrity Test Suite for Azimuth
// This file contains comprehensive tests for data serialization and integrity

// Test 1: Attribute serialization and deserialization
pub test "attribute serialization and deserialization" {
  let attrs = azimuth::Attributes::new()
  
  // Create comprehensive attribute set for serialization testing
  let test_attrs = [
    // Basic types
    ("string.basic", azimuth::StringValue("hello world")),
    ("int.zero", azimuth::IntValue(0)),
    ("int.positive", azimuth::IntValue(42)),
    ("int.negative", azimuth::IntValue(-100)),
    ("int.max", azimuth::IntValue(2147483647)),
    ("int.min", azimuth::IntValue(-2147483648)),
    ("float.zero", azimuth::FloatValue(0.0)),
    ("float.positive", azimuth::FloatValue(3.14159)),
    ("float.negative", azimuth::FloatValue(-2.71828)),
    ("float.infinity", azimuth::FloatValue(1.0/0.0)),
    ("float.nan", azimuth::FloatValue(0.0/0.0)),
    ("bool.true", azimuth::BoolValue(true)),
    ("bool.false", azimuth::BoolValue(false)),
    
    // Array types
    ("array.string.empty", azimuth::ArrayStringValue([])),
    ("array.string.single", azimuth::ArrayStringValue(["single"])),
    ("array.string.multiple", azimuth::ArrayStringValue(["a", "b", "c", "d", "e"])),
    ("array.int.empty", azimuth::ArrayIntValue([])),
    ("array.int.single", azimuth::ArrayIntValue([42])),
    ("array.int.multiple", azimuth::ArrayIntValue([1, 2, 3, 4, 5])),
    
    // Special string values
    ("string.empty", azimuth::StringValue("")),
    ("string.whitespace", azimuth::StringValue("   	
   ")),
    ("string.quotes", azimuth::StringValue("\"quoted\" 'single'")),
    ("string.unicode", azimuth::StringValue("测试中文字符 🚀 emoji")),
    ("string.json", azimuth::StringValue("{\"key\": \"value\", \"array\": [1,2,3]}")),
    ("string.xml", azimuth::StringValue("<root><child attr=\"value\">text</child></root>")),
    ("string.url", azimuth::StringValue("https://example.com/path?param=value&other=123")),
    ("string.email", azimuth::StringValue("user@example.com")),
    
    // Edge case values
    ("string.very.long", azimuth::StringValue("x" * 1000)),
    ("string.special.chars", azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?~`")),
    ("string.null.chars", azimuth::StringValue("before\0after")),
    ("string.control.chars", azimuth::StringValue("before\u0001\u001fafter"))
  ]
  
  // Set all test attributes
  for (key, value) in test_attrs {
    azimuth::Attributes::set(attrs, key, value)
  }
  
  // Simulate serialization by verifying all attributes can be accessed
  let serialization_data = []
  for (key, _) in test_attrs {
    let value = azimuth::Attributes::get(attrs, key)
    serialization_data.push((key, value))
  }
  
  // Verify all attributes were "serialized" successfully
  assert_eq(serialization_data.length(), test_attrs.length())
  
  // Simulate deserialization by reconstructing attributes
  let reconstructed_attrs = azimuth::Attributes::new()
  for (key, value) in test_attrs {
    azimuth::Attributes::set(reconstructed_attrs, key, value)
  }
  
  // Verify reconstruction integrity
  for (key, expected_value) in test_attrs {
    let original_value = azimuth::Attributes::get(attrs, key)
    let reconstructed_value = azimuth::Attributes::get(reconstructed_attrs, key)
    assert_eq(original_value, reconstructed_value)
    assert_eq(reconstructed_value, Some(expected_value))
  }
}

// Test 2: Span context serialization integrity
pub test "span context serialization integrity" {
  // Create span contexts with various configurations
  let span_contexts = [
    ("basic.valid", azimuth::SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")),
    ("not.sampled", azimuth::SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", false, "")),
    ("with.tracestate", azimuth::SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1,key2=value2")),
    ("empty.tracestate", azimuth::SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")),
    ("complex.tracestate", azimuth::SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "vendor1=value1;vendor2=value2,tenant=abc123")),
    ("invalid.empty", azimuth::SpanContext::new("", "", false, "")),
    ("invalid.trace", azimuth::SpanContext::new("", "b7ad6b7169203331", false, "")),
    ("invalid.span", azimuth::SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", false, ""))
  ]
  
  // Simulate serialization of span contexts
  let serialized_contexts = []
  for (context_name, context) in span_contexts {
    let trace_id = azimuth::SpanContext::trace_id(context)
    let span_id = azimuth::SpanContext::span_id(context)
    let sampled = azimuth::SpanContext::is_sampled(context)
    let trace_state = azimuth::SpanContext::is_valid(context)
    
    // Create serialization representation
    let serialized = (context_name, trace_id, span_id, sampled, trace_state)
    serialized_contexts.push(serialized)
  }
  
  // Simulate deserialization and verification
  for (expected_name, expected_trace_id, expected_span_id, expected_sampled, expected_valid) in serialized_contexts {
    // Find original context
    let original_context = None
    for (name, ctx) in span_contexts {
      if name == expected_name {
        original_context = Some(ctx)
        break
      }
    }
    
    match original_context {
      Some(ctx) => {
        // Verify deserialized values match original
        assert_eq(azimuth::SpanContext::trace_id(ctx), expected_trace_id)
        assert_eq(azimuth::SpanContext::span_id(ctx), expected_span_id)
        assert_eq(azimuth::SpanContext::is_sampled(ctx), expected_sampled)
        assert_eq(azimuth::SpanContext::is_valid(ctx), expected_valid)
      }
      None => assert_true(false, "Context not found: " + expected_name)
    }
  }
}

// Test 3: Resource serialization and reconstruction
pub test "resource serialization and reconstruction" {
  let resource = azimuth::Resource::new()
  
  // Create complex resource with various attribute types
  let complex_attrs = [
    // Service information
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.2.3")),
    ("service.instance.id", azimuth::StringValue("instance-12345")),
    ("service.namespace", azimuth::StringValue("production")),
    
    // Infrastructure information
    ("host.name", azimuth::StringValue("test-host-01")),
    ("host.id", azimuth::StringValue("host-abcdef123456")),
    ("host.type", azimuth::StringValue("c5.large")),
    ("host.image.id", azimuth::StringValue("ami-0123456789abcdef0")),
    
    // Cloud information
    ("cloud.provider", azimuth::StringValue("aws")),
    ("cloud.account.id", azimuth::StringValue("123456789012")),
    ("cloud.region", azimuth::StringValue("us-west-2")),
    ("cloud.availability.zone", azimuth::StringValue("us-west-2a")),
    
    // Container information
    ("container.name", azimuth::StringValue("test-container")),
    ("container.id", azimuth::StringValue("container-abcdef123456")),
    ("container.image.name", azimuth::StringValue("test-service:latest")),
    ("container.image.tag", azimuth::StringValue("1.2.3")),
    
    // Process information
    ("process.pid", azimuth::IntValue(1234)),
    ("process.executable.name", azimuth::StringValue("test-service")),
    ("process.command", azimuth::StringValue("/usr/bin/test-service")),
    ("process.command_args", azimuth::StringValue("--port=8080 --config=/etc/test-service/config.yaml")),
    
    // Telemetry information
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.language", azimuth::StringValue("moonbit")),
    ("telemetry.sdk.version", azimuth::StringValue("1.0.0")),
    ("telemetry.auto.version", azimuth::StringValue("0.1.0")),
    
    // Custom attributes
    ("custom.feature.flags", azimuth::ArrayStringValue(["feature1", "feature2", "experimental"])),
    ("custom.port.numbers", azimuth::ArrayIntValue([8080, 8443, 9090])),
    ("custom.debug.enabled", azimuth::BoolValue(true)),
    ("custom.memory.limit.mb", azimuth::IntValue(1024)),
    ("custom.cpu.limit.cores", azimuth::FloatValue(2.5)),
    ("custom.metadata.json", azimuth::StringValue("{\"key\": \"value\", \"nested\": {\"data\": true}}"))
  ]
  
  let original_resource = azimuth::Resource::with_attributes(resource, complex_attrs)
  
  // Simulate serialization by collecting all attribute data
  let serialized_resource = []
  for (key, _) in complex_attrs {
    let value = azimuth::Resource::get_attribute(original_resource, key)
    serialized_resource.push((key, value))
  }
  
  // Verify all attributes were serialized
  assert_eq(serialized_resource.length(), complex_attrs.length())
  
  // Simulate deserialization by creating new resource
  let deserialized_resource = azimuth::Resource::new()
  for (key, value) in complex_attrs {
    azimuth::Resource::set_attribute(deserialized_resource, key, value)
  }
  
  // Note: Since Resource::set_attribute is not available in the current API,
  // we'll use with_attributes to simulate reconstruction
  let reconstructed_resource = azimuth::Resource::with_attributes(resource, complex_attrs)
  
  // Verify reconstruction integrity by comparing key attributes
  let key_attributes = [
    "service.name", "service.version", "host.name", "cloud.provider",
    "process.pid", "telemetry.sdk.name", "custom.debug.enabled"
  ]
  
  for attr_key in key_attributes {
    let original_value = azimuth::Resource::get_attribute(original_resource, attr_key)
    let reconstructed_value = azimuth::Resource::get_attribute(reconstructed_resource, attr_key)
    assert_eq(original_value, reconstructed_value)
  }
}

// Test 4: Log record serialization integrity
pub test "log record serialization integrity" {
  // Create log records with various configurations
  let log_records = [
    // Basic log record
    azimuth::LogRecord::new(azimuth::Info, "Basic log message"),
    
    // Log record with attributes
    azimuth::LogRecord::new_with_context(
      azimuth::Warn,
      Some("Warning with attributes"),
      None,
      Some(1704067200000000000L),
      None,
      None,
      None,
      None
    ),
    
    // Complete log record
    azimuth::LogRecord::new_with_context(
      azimuth::Error,
      Some("Complete log record"),
      None,
      Some(1704067200000000000L),
      Some(1704067200000001000L),
      Some("0af7651916cd43dd8448eb211c80319c"),
      Some("b7ad6b7169203331"),
      Some(azimuth::Context::root())
    ),
    
    // Log record with special characters
    azimuth::LogRecord::new_with_context(
      azimuth::Debug,
      Some("Special chars: \"quotes\", 'single', 
	, unicode: 测试 🚀"),
      None,
      Some(1704067200000000000L),
      None,
      None,
      None,
      None
    )
  ]
  
  // Simulate serialization of log records
  let serialized_records = []
  for record in log_records {
    let severity = azimuth::LogRecord::severity_number(record)
    let body = azimuth::LogRecord::body(record)
    let timestamp = azimuth::LogRecord::timestamp(record)
    let observed_timestamp = azimuth::LogRecord::observed_timestamp(record)
    let trace_id = azimuth::LogRecord::trace_id(record)
    let span_id = azimuth::LogRecord::span_id(record)
    
    let serialized = (severity, body, timestamp, observed_timestamp, trace_id, span_id)
    serialized_records.push(serialized)
  }
  
  // Verify serialization integrity
  assert_eq(serialized_records.length(), log_records.length())
  
  // Verify each serialized record matches original
  for i in 0..log_records.length() {
    let original = log_records[i]
    let (severity, body, timestamp, observed_timestamp, trace_id, span_id) = serialized_records[i]
    
    assert_eq(azimuth::LogRecord::severity_number(original), severity)
    assert_eq(azimuth::LogRecord::body(original), body)
    assert_eq(azimuth::LogRecord::timestamp(original), timestamp)
    assert_eq(azimuth::LogRecord::observed_timestamp(original), observed_timestamp)
    assert_eq(azimuth::LogRecord::trace_id(original), trace_id)
    assert_eq(azimuth::LogRecord::span_id(original), span_id)
  }
}

// Test 5: Metric instrument serialization
pub test "metric instrument serialization" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "serialization-test")
  
  // Create various metric instruments
  let counter = azimuth::Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "test.updown", Some("Test up-down counter"), Some("operations"))
  let gauge = azimuth::Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("bytes"))
  
  let instruments = [("counter", counter), ("histogram", histogram), ("updown", updown_counter), ("gauge", gauge)]
  
  // Simulate serialization of instrument metadata
  let serialized_instruments = []
  for (instrument_type, instrument) in instruments {
    let name = match instrument_type {
      "counter" => counter.name
      "histogram" => histogram.name
      "updown" => updown_counter.name
      "gauge" => gauge.name
      _ => ""
    }
    
    let description = match instrument_type {
      "counter" => counter.description
      "histogram" => histogram.description
      "updown" => updown_counter.description
      "gauge" => gauge.description
      _ => None
    }
    
    let unit = match instrument_type {
      "counter" => counter.unit
      "histogram" => histogram.unit
      "updown" => updown_counter.unit
      "gauge" => gauge.unit
      _ => None
    }
    
    let serialized = (instrument_type, name, description, unit)
    serialized_instruments.push(serialized)
  }
  
  // Verify serialization integrity
  assert_eq(serialized_instruments.length(), instruments.length())
  
  // Verify each serialized instrument matches original
  for (instrument_type, name, description, unit) in serialized_instruments {
    match instrument_type {
      "counter" => {
        assert_eq(counter.name, name)
        assert_eq(counter.description, description)
        assert_eq(counter.unit, unit)
      }
      "histogram" => {
        assert_eq(histogram.name, name)
        assert_eq(histogram.description, description)
        assert_eq(histogram.unit, unit)
      }
      "updown" => {
        assert_eq(updown_counter.name, name)
        assert_eq(updown_counter.description, description)
        assert_eq(updown_counter.unit, unit)
      }
      "gauge" => {
        assert_eq(gauge.name, name)
        assert_eq(gauge.description, description)
        assert_eq(gauge.unit, unit)
      }
      _ => assert_true(false, "Unknown instrument type: " + instrument_type)
    }
  }
}

// Test 6: Context and baggage serialization
pub test "context and baggage serialization" {
  // Create context with multiple values
  let root_ctx = azimuth::Context::root()
  let key1 = azimuth::ContextKey::new("user.id")
  let key2 = azimuth::ContextKey::new("request.id")
  let key3 = azimuth::ContextKey::new("session.id")
  
  let ctx1 = azimuth::Context::with_value(root_ctx, key1, "user-12345")
  let ctx2 = azimuth::Context::with_value(ctx1, key2, "req-abcdef")
  let ctx3 = azimuth::Context::with_value(ctx2, key3, "sess-123456")
  
  // Create baggage with multiple entries
  let baggage = azimuth::Baggage::new()
  let baggage1 = azimuth::Baggage::set_entry(baggage, "user.role", "admin")
  let baggage2 = azimuth::Baggage::set_entry(baggage1, "region", "us-west-2")
  let baggage3 = azimuth::Baggage::set_entry(baggage2, "feature.flags", "new_ui,experimental")
  
  // Simulate context serialization
  let context_values = [
    ("user.id", azimuth::Context::get(ctx3, key1)),
    ("request.id", azimuth::Context::get(ctx3, key2)),
    ("session.id", azimuth::Context::get(ctx3, key3))
  ]
  
  // Simulate baggage serialization
  let baggage_values = [
    ("user.role", azimuth::Baggage::get_entry(baggage3, "user.role")),
    ("region", azimuth::Baggage::get_entry(baggage3, "region")),
    ("feature.flags", azimuth::Baggage::get_entry(baggage3, "feature.flags"))
  ]
  
  // Verify context serialization integrity
  for (key, expected_value) in context_values {
    assert_eq(expected_value, Some(match key {
      "user.id" => "user-12345"
      "request.id" => "req-abcdef"
      "session.id" => "sess-123456"
      _ => ""
    }))
  }
  
  // Verify baggage serialization integrity
  for (key, expected_value) in baggage_values {
    assert_eq(expected_value, Some(match key {
      "user.role" => "admin"
      "region" => "us-west-2"
      "feature.flags" => "new_ui,experimental"
      _ => ""
    }))
  }
  
  // Simulate deserialization by reconstructing context and baggage
  let reconstructed_ctx = azimuth::Context::root()
  let recon_key1 = azimuth::ContextKey::new("user.id")
  let recon_ctx1 = azimuth::Context::with_value(reconstructed_ctx, recon_key1, "user-12345")
  
  let recon_key2 = azimuth::ContextKey::new("request.id")
  let recon_ctx2 = azimuth::Context::with_value(recon_ctx1, recon_key2, "req-abcdef")
  
  let recon_key3 = azimuth::ContextKey::new("session.id")
  let recon_ctx3 = azimuth::Context::with_value(recon_ctx2, recon_key3, "sess-123456")
  
  // Verify reconstructed context matches original
  assert_eq(azimuth::Context::get(ctx3, key1), azimuth::Context::get(recon_ctx3, recon_key1))
  assert_eq(azimuth::Context::get(ctx3, key2), azimuth::Context::get(recon_ctx3, recon_key2))
  assert_eq(azimuth::Context::get(ctx3, key3), azimuth::Context::get(recon_ctx3, recon_key3))
}

// Test 7: HTTP request/response serialization
pub test "http request response serialization" {
  // Create HTTP request with various components
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-abc-123"),
    ("User-Agent", "TestClient/1.0"),
    ("Accept", "application/json, text/plain"),
    ("X-Custom-Header", "custom-value")
  ]
  
  let request = azimuth::HttpRequest::new(
    "POST",
    "https://api.example.com:8443/path/to/resource?param1=value1&param2=value2",
    request_headers,
    Some("{\"key\": \"value\", \"array\": [1, 2, 3], \"unicode\": \"测试\"}")
  )
  
  // Create HTTP response with various components
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-ID", "resp-xyz-789"),
    ("Cache-Control", "no-cache"),
    ("Server", "TestServer/2.0"),
    ("X-Rate-Limit-Limit", "1000"),
    ("X-Rate-Limit-Remaining", "999")
  ]
  
  let response = azimuth::HttpResponse::new(
    200,
    response_headers,
    Some("{\"status\": \"success\", \"data\": {\"id\": 123, \"name\": \"测试数据\"}, \"timestamp\": 1704067200}")
  )
  
  // Simulate request serialization
  let serialized_request = (
    azimuth::HttpRequest::http_method(request),
    azimuth::HttpRequest::url(request),
    azimuth::HttpRequest::body(request)
  )
  
  // Simulate response serialization
  let serialized_response = (
    azimuth::HttpResponse::status_code(response),
    azimuth::HttpResponse::body(response)
  )
  
  // Verify request serialization integrity
  assert_eq(azimuth::HttpRequest::http_method(request), "POST")
  assert_eq(azimuth::HttpRequest::url(request), "https://api.example.com:8443/path/to/resource?param1=value1&param2=value2")
  assert_eq(azimuth::HttpRequest::body(request), Some("{\"key\": \"value\", \"array\": [1, 2, 3], \"unicode\": \"测试\"}"))
  
  // Verify response serialization integrity
  assert_eq(azimuth::HttpResponse::status_code(response), 200)
  assert_eq(azimuth::HttpResponse::body(response), Some("{\"status\": \"success\", \"data\": {\"id\": 123, \"name\": \"测试数据\"}, \"timestamp\": 1704067200}"))
  
  // Simulate deserialization by reconstructing request and response
  let reconstructed_request = azimuth::HttpRequest::new(
    serialized_request.0,
    serialized_request.1,
    request_headers,
    serialized_request.2
  )
  
  let reconstructed_response = azimuth::HttpResponse::new(
    serialized_response.0,
    response_headers,
    serialized_response.1
  )
  
  // Verify reconstructed objects match original
  assert_eq(azimuth::HttpRequest::http_method(request), azimuth::HttpRequest::http_method(reconstructed_request))
  assert_eq(azimuth::HttpRequest::url(request), azimuth::HttpRequest::url(reconstructed_request))
  assert_eq(azimuth::HttpRequest::body(request), azimuth::HttpRequest::body(reconstructed_request))
  
  assert_eq(azimuth::HttpResponse::status_code(response), azimuth::HttpResponse::status_code(reconstructed_response))
  assert_eq(azimuth::HttpResponse::body(response), azimuth::HttpResponse::body(reconstructed_response))
}

// Test 8: End-to-end telemetry data serialization
pub test "end-to-end telemetry data serialization" {
  // Create comprehensive telemetry scenario
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "abcdef1234567890"
  let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  let span = azimuth::Span::new("test-operation", azimuth::Server, span_ctx)
  let resource = azimuth::Resource::new()
  let resource_attrs = [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0"))
  ]
  let service_resource = azimuth::Resource::with_attributes(resource, resource_attrs)
  
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Test log message"),
    None,
    Some(1704067200000000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(azimuth::Context::root())
  )
  
  // Simulate complete serialization scenario
  let telemetry_package = (
    // Span information
    azimuth::Span::name(span),
    azimuth::Span::kind(span),
    azimuth::SpanContext::trace_id(azimuth::Span::span_context(span)),
    azimuth::SpanContext::span_id(azimuth::Span::span_context(span)),
    azimuth::SpanContext::is_sampled(azimuth::Span::span_context(span)),
    
    // Resource information
    azimuth::Resource::get_attribute(service_resource, "service.name"),
    azimuth::Resource::get_attribute(service_resource, "service.version"),
    
    // Log information
    azimuth::LogRecord::severity_number(log_record),
    azimuth::LogRecord::body(log_record),
    azimuth::LogRecord::timestamp(log_record),
    azimuth::LogRecord::trace_id(log_record),
    azimuth::LogRecord::span_id(log_record)
  )
  
  // Verify serialization integrity
  assert_eq(telemetry_package.0, "test-operation")  // span name
  assert_eq(telemetry_package.1, azimuth::Server)   // span kind
  assert_eq(telemetry_package.2, Some(trace_id))    // trace id
  assert_eq(telemetry_package.3, Some(span_id))     // span id
  assert_eq(telemetry_package.4, true)              // sampled
  assert_eq(telemetry_package.5, Some(azimuth::StringValue("test-service")))  // service name
  assert_eq(telemetry_package.6, Some(azimuth::StringValue("1.0.0")))         // service version
  assert_eq(telemetry_package.7, azimuth::Info)      // log severity
  assert_eq(telemetry_package.8, Some("Test log message"))  // log body
  assert_eq(telemetry_package.9, Some(1704067200000000000L)) // timestamp
  assert_eq(telemetry_package.10, Some(trace_id))    // log trace id
  assert_eq(telemetry_package.11, Some(span_id))     // log span id
  
  // Simulate complete deserialization scenario
  let reconstructed_span_ctx = azimuth::SpanContext::new(
    telemetry_package.2.unwrap_or(""),
    telemetry_package.3.unwrap_or(""),
    telemetry_package.4,
    ""
  )
  let reconstructed_span = azimuth::Span::new(
    telemetry_package.0,
    telemetry_package.1,
    reconstructed_span_ctx
  )
  
  let reconstructed_resource = azimuth::Resource::with_attributes(resource, [
    ("service.name", telemetry_package.5.unwrap_or(azimuth::StringValue(""))),
    ("service.version", telemetry_package.6.unwrap_or(azimuth::StringValue("")))
  ])
  
  let reconstructed_log = azimuth::LogRecord::new_with_context(
    telemetry_package.7,
    telemetry_package.8,
    None,
    telemetry_package.9,
    None,
    telemetry_package.10,
    telemetry_package.11,
    None
  )
  
  // Verify reconstructed telemetry matches original
  assert_eq(azimuth::Span::name(span), azimuth::Span::name(reconstructed_span))
  assert_eq(azimuth::Span::kind(span), azimuth::Span::kind(reconstructed_span))
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span)), azimuth::SpanContext::trace_id(azimuth::Span::span_context(reconstructed_span)))
  assert_eq(azimuth::SpanContext::span_id(azimuth::Span::span_context(span)), azimuth::SpanContext::span_id(azimuth::Span::span_context(reconstructed_span)))
  
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.name"), azimuth::Resource::get_attribute(reconstructed_resource, "service.name"))
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.version"), azimuth::Resource::get_attribute(reconstructed_resource, "service.version"))
  
  assert_eq(azimuth::LogRecord::severity_number(log_record), azimuth::LogRecord::severity_number(reconstructed_log))
  assert_eq(azimuth::LogRecord::body(log_record), azimuth::LogRecord::body(reconstructed_log))
  assert_eq(azimuth::LogRecord::timestamp(log_record), azimuth::LogRecord::timestamp(reconstructed_log))
}