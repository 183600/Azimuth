// Data Serialization and Deserialization Tests
// This file contains test cases for data serialization and deserialization

// Test 1: Span serialization and deserialization
pub test "span_serialization_deserialization" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "serialization-test-service")
  
  // Create a complex span with various attributes
  let span = azimuth::Tracer::start_span(tracer, "Serialization Test Span")
  azimuth::Span::set_attribute(span, "string.attr", azimuth::StringValue("test value"))
  azimuth::Span::set_attribute(span, "int.attr", azimuth::IntValue(42))
  azimuth::Span::set_attribute(span, "float.attr", azimuth::FloatValue(3.14))
  azimuth::Span::set_attribute(span, "bool.attr", azimuth::BoolValue(true))
  azimuth::Span::set_attribute(span, "array.string.attr", azimuth::ArrayStringValue(["a", "b", "c"]))
  azimuth::Span::set_attribute(span, "array.int.attr", azimuth::ArrayIntValue([1, 2, 3]))
  azimuth::Span::set_status_code(span, azimuth::Ok)
  azimuth::Span::end(span)
  
  // Serialize span to JSON
  let serialized_span = azimuth::Span::to_json(span)
  
  // Verify serialized span contains expected data
  assert_true(serialized_span.contains("Serialization Test Span"))
  assert_true(serialized_span.contains("string.attr"))
  assert_true(serialized_span.contains("test value"))
  assert_true(serialized_span.contains("int.attr"))
  assert_true(serialized_span.contains("42"))
  
  // Deserialize span from JSON
  let deserialized_span = azimuth::Span::from_json(serialized_span)
  
  // Verify deserialized span has same attributes
  assert_eq(azimuth::Span::name(deserialized_span), azimuth::Span::name(span))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "string.attr"), Some(azimuth::StringValue("test value")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "int.attr"), Some(azimuth::IntValue(42)))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "float.attr"), Some(azimuth::FloatValue(3.14)))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "bool.attr"), Some(azimuth::BoolValue(true)))
}

// Test 2: Metric serialization and deserialization
pub test "metric_serialization_deserialization" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "serialization-metrics")
  
  // Create metric instruments and record data
  let counter = azimuth::Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  let gauge = azimuth::Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  
  // Record metric data
  azimuth::Counter::add(counter, 100, [("tag1", "value1"), ("tag2", "value2")])
  azimuth::Histogram::record(histogram, 50.0, [("tag1", "value1"), ("tag2", "value2")])
  azimuth::Gauge::set(gauge, 75.5, [("tag1", "value1"), ("tag2", "value2")])
  
  // Serialize metrics to JSON
  let serialized_metrics = azimuth::Meter::to_json(meter)
  
  // Verify serialized metrics contain expected data
  assert_true(serialized_metrics.contains("test.counter"))
  assert_true(serialized_metrics.contains("test.histogram"))
  assert_true(serialized_metrics.contains("test.gauge"))
  assert_true(serialized_metrics.contains("tag1"))
  assert_true(serialized_metrics.contains("value1"))
  
  // Deserialize metrics from JSON
  let deserialized_meter = azimuth::Meter::from_json(serialized_metrics)
  
  // Verify deserialized metrics have same data
  let deserialized_counter = azimuth::Meter::get_counter(deserialized_meter, "test.counter")
  let deserialized_histogram = azimuth::Meter::get_histogram(deserialized_meter, "test.histogram")
  let deserialized_gauge = azimuth::Meter::get_gauge(deserialized_meter, "test.gauge")
  
  assert_eq(deserialized_counter.name, "test.counter")
  assert_eq(deserialized_histogram.name, "test.histogram")
  assert_eq(deserialized_gauge.name, "test.gauge")
}

// Test 3: Log serialization and deserialization
pub test "log_serialization_deserialization" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "serialization-logger")
  
  // Create log records with various severity levels
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Information message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error message")
  
  // Add attributes to log records
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "log.source", azimuth::StringValue("test-module"))
  azimuth::Attributes::set(attrs, "log.version", azimuth::StringValue("1.0.0"))
  
  let enriched_info_log = azimuth::LogRecord::with_attributes(info_log, attrs)
  let enriched_warn_log = azimuth::LogRecord::with_attributes(warn_log, attrs)
  let enriched_error_log = azimuth::LogRecord::with_attributes(error_log, attrs)
  
  // Serialize log records to JSON
  let serialized_info = azimuth::LogRecord::to_json(enriched_info_log)
  let serialized_warn = azimuth::LogRecord::to_json(enriched_warn_log)
  let serialized_error = azimuth::LogRecord::to_json(enriched_error_log)
  
  // Verify serialized logs contain expected data
  assert_true(serialized_info.contains("Information message"))
  assert_true(serialized_info.contains("INFO"))
  assert_true(serialized_info.contains("log.source"))
  assert_true(serialized_info.contains("test-module"))
  
  assert_true(serialized_warn.contains("Warning message"))
  assert_true(serialized_warn.contains("WARN"))
  
  assert_true(serialized_error.contains("Error message"))
  assert_true(serialized_error.contains("ERROR"))
  
  // Deserialize log records from JSON
  let deserialized_info = azimuth::LogRecord::from_json(serialized_info)
  let deserialized_warn = azimuth::LogRecord::from_json(serialized_warn)
  let deserialized_error = azimuth::LogRecord::from_json(serialized_error)
  
  // Verify deserialized logs have same data
  assert_eq(azimuth::LogRecord::body(deserialized_info), Some("Information message"))
  assert_eq(azimuth::LogRecord::severity_number(deserialized_info), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(deserialized_warn), Some("Warning message"))
  assert_eq(azimuth::LogRecord::severity_number(deserialized_warn), azimuth::Warn)
  assert_eq(azimuth::LogRecord::body(deserialized_error), Some("Error message"))
  assert_eq(azimuth::LogRecord::severity_number(deserialized_error), azimuth::Error)
}

// Test 4: Resource serialization and deserialization
pub test "resource_serialization_deserialization" {
  // Create a resource with various attributes
  let resource = azimuth::Resource::new()
  let resource_with_attrs = azimuth::Resource::with_attributes(resource, [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("deployment.environment", azimuth::StringValue("test")),
    ("host.name", azimuth::StringValue("test-host")),
    ("host.arch", azimuth::StringValue("x86_64")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("process.id", azimuth::IntValue(12345)),
    ("process.name", azimuth::StringValue("test-process")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ])
  
  // Serialize resource to JSON
  let serialized_resource = azimuth::Resource::to_json(resource_with_attrs)
  
  // Verify serialized resource contains expected data
  assert_true(serialized_resource.contains("test-service"))
  assert_true(serialized_resource.contains("1.0.0"))
  assert_true(serialized_resource.contains("test-host"))
  assert_true(serialized_resource.contains("linux"))
  assert_true(serialized_resource.contains("azimuth"))
  
  // Deserialize resource from JSON
  let deserialized_resource = azimuth::Resource::from_json(serialized_resource)
  
  // Verify deserialized resource has same attributes
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "service.name"), Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "deployment.environment"), Some(azimuth::StringValue("test")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "host.name"), Some(azimuth::StringValue("test-host")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "host.arch"), Some(azimuth::StringValue("x86_64")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "os.version"), Some(azimuth::StringValue("5.15.0")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "process.id"), Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "process.name"), Some(azimuth::StringValue("test-process")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(deserialized_resource, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
}

// Test 5: Context and baggage serialization
pub test "context_baggage_serialization" {
  // Create context with various values
  let root_ctx = azimuth::Context::root()
  let string_key = azimuth::ContextKey::new("string.key")
  let int_key = azimuth::ContextKey::new("int.key")
  let bool_key = azimuth::ContextKey::new("bool.key")
  
  let ctx_with_string = azimuth::Context::with_value(root_ctx, string_key, "string.value")
  let ctx_with_int = azimuth::Context::with_value(ctx_with_string, int_key, "42")
  let ctx_with_bool = azimuth::Context::with_value(ctx_with_int, bool_key, "true")
  
  // Create baggage with multiple entries
  let baggage = azimuth::Baggage::new()
  let baggage_with_entry1 = azimuth::Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_entry2 = azimuth::Baggage::set_entry(baggage_with_entry1, "session.id", "sess-67890")
  let baggage_with_entry3 = azimuth::Baggage::set_entry(baggage_with_entry2, "request.id", "req-11111")
  
  // Add baggage to context
  let baggage_key = azimuth::ContextKey::new("baggage")
  let ctx_with_baggage = azimuth::Context::with_value(ctx_with_bool, baggage_key, baggage_with_entry3)
  
  // Serialize context to JSON
  let serialized_context = azimuth::Context::to_json(ctx_with_baggage)
  
  // Verify serialized context contains expected data
  assert_true(serialized_context.contains("string.key"))
  assert_true(serialized_context.contains("string.value"))
  assert_true(serialized_context.contains("int.key"))
  assert_true(serialized_context.contains("42"))
  assert_true(serialized_context.contains("bool.key"))
  assert_true(serialized_context.contains("true"))
  assert_true(serialized_context.contains("baggage"))
  assert_true(serialized_context.contains("user.id"))
  
  // Deserialize context from JSON
  let deserialized_context = azimuth::Context::from_json(serialized_context)
  
  // Verify deserialized context has same values
  assert_eq(azimuth::Context::get(deserialized_context, string_key), Some("string.value"))
  assert_eq(azimuth::Context::get(deserialized_context, int_key), Some("42"))
  assert_eq(azimuth::Context::get(deserialized_context, bool_key), Some("true"))
  
  let deserialized_baggage = azimuth::Context::get(deserialized_context, baggage_key)
  assert_eq(azimuth::Baggage::get_entry(deserialized_baggage, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(deserialized_baggage, "session.id"), Some("sess-67890"))
  assert_eq(azimuth::Baggage::get_entry(deserialized_baggage, "request.id"), Some("req-11111"))
}

// Test 6: Batch serialization operations
pub test "batch_serialization_operations" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "batch-test-service")
  
  // Create multiple spans
  let mut spans = []
  let mut i = 0
  while i < 10 {
    let span = azimuth::Tracer::start_span(tracer, "Batch Span " + i.to_string())
    azimuth::Span::set_attribute(span, "batch.index", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "batch.id", azimuth::StringValue("batch-123"))
    azimuth::Span::set_status_code(span, azimuth::Ok)
    azimuth::Span::end(span)
    spans = spans + [span]
    i = i + 1
  }
  
  // Serialize spans as a batch
  let serialized_batch = azimuth::Span::batch_to_json(spans)
  
  // Verify serialized batch contains expected data
  assert_true(serialized_batch.contains("batch-123"))
  assert_true(serialized_batch.contains("Batch Span 0"))
  assert_true(serialized_batch.contains("Batch Span 9"))
  
  // Deserialize spans from batch
  let deserialized_spans = azimuth::Span::batch_from_json(serialized_batch)
  
  // Verify all spans are deserialized correctly
  assert_eq(length(deserialized_spans), 10)
  
  let mut j = 0
  for span in deserialized_spans {
    assert_eq(azimuth::Span::get_attribute(span, "batch.index"), Some(azimuth::IntValue(j)))
    assert_eq(azimuth::Span::get_attribute(span, "batch.id"), Some(azimuth::StringValue("batch-123")))
    j = j + 1
  }
}

// Test 7: Serialization with complex nested data
pub test "complex_nested_data_serialization" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "complex-test-service")
  
  // Create span with complex nested attributes
  let span = azimuth::Tracer::start_span(tracer, "Complex Data Span")
  
  // Add nested object-like structure using attributes
  azimuth::Span::set_attribute(span, "user.id", azimuth::StringValue("user-123"))
  azimuth::Span::set_attribute(span, "user.name", azimuth::StringValue("John Doe"))
  azimuth::Span::set_attribute(span, "user.email", azimuth::StringValue("john@example.com"))
  azimuth::Span::set_attribute(span, "user.age", azimuth::IntValue(30))
  azimuth::Span::set_attribute(span, "user.active", azimuth::BoolValue(true))
  
  // Add address information
  azimuth::Span::set_attribute(span, "user.address.street", azimuth::StringValue("123 Main St"))
  azimuth::Span::set_attribute(span, "user.address.city", azimuth::StringValue("New York"))
  azimuth::Span::set_attribute(span, "user.address.state", azimuth::StringValue("NY"))
  azimuth::Span::set_attribute(span, "user.address.zip", azimuth::StringValue("10001"))
  azimuth::Span::set_attribute(span, "user.address.country", azimuth::StringValue("USA"))
  
  // Add order history
  azimuth::Span::set_attribute(span, "user.orders.count", azimuth::IntValue(5))
  azimuth::Span::set_attribute(span, "user.orders.total", azimuth::FloatValue(250.75))
  azimuth::Span::set_attribute(span, "user.orders.last_order", azimuth::StringValue("2023-01-15"))
  
  // Add preferences
  azimuth::Span::set_attribute(span, "user.preferences.language", azimuth::StringValue("en"))
  azimuth::Span::set_attribute(span, "user.preferences.timezone", azimuth::StringValue("UTC-5"))
  azimuth::Span::set_attribute(span, "user.preferences.notifications", azimuth::BoolValue(true))
  
  azimuth::Span::set_status_code(span, azimuth::Ok)
  azimuth::Span::end(span)
  
  // Serialize complex span
  let serialized_span = azimuth::Span::to_json(span)
  
  // Verify serialized span contains all nested data
  assert_true(serialized_span.contains("user.id"))
  assert_true(serialized_span.contains("user-123"))
  assert_true(serialized_span.contains("user.address.street"))
  assert_true(serialized_span.contains("123 Main St"))
  assert_true(serialized_span.contains("user.orders.count"))
  assert_true(serialized_span.contains("user.preferences.language"))
  
  // Deserialize complex span
  let deserialized_span = azimuth::Span::from_json(serialized_span)
  
  // Verify all nested attributes are preserved
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.id"), Some(azimuth::StringValue("user-123")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.name"), Some(azimuth::StringValue("John Doe")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.email"), Some(azimuth::StringValue("john@example.com")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.age"), Some(azimuth::IntValue(30)))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.active"), Some(azimuth::BoolValue(true)))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.address.street"), Some(azimuth::StringValue("123 Main St")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.address.city"), Some(azimuth::StringValue("New York")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.address.state"), Some(azimuth::StringValue("NY")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.address.zip"), Some(azimuth::StringValue("10001")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.address.country"), Some(azimuth::StringValue("USA")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.orders.count"), Some(azimuth::IntValue(5)))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.orders.total"), Some(azimuth::FloatValue(250.75)))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.orders.last_order"), Some(azimuth::StringValue("2023-01-15")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.preferences.language"), Some(azimuth::StringValue("en")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.preferences.timezone"), Some(azimuth::StringValue("UTC-5")))
  assert_eq(azimuth::Span::get_attribute(deserialized_span, "user.preferences.notifications"), Some(azimuth::BoolValue(true)))
}