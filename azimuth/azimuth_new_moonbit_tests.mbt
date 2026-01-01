// New MoonBit Test Cases for Azimuth Telemetry System
// This file contains additional test cases focusing on edge cases and advanced scenarios

// Test 1: Performance metrics with high-frequency operations
test "high-frequency metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance-test")
  
  // Test counter with high-frequency increments
  let request_counter = Meter::create_counter(meter, "high.freq.requests", Some("High frequency requests"), Some("count"))
  
  // Simulate high-frequency operations
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(request_counter, 1.0)
  }
  
  // Test histogram with high-frequency recordings
  let latency_histogram = Meter::create_histogram(meter, "high.freq.latency", Some("High frequency latency"), Some("ms"))
  
  // Simulate high-frequency latency recordings
  for i = 0; i < 500; i = i + 1 {
    let latency = 50.0 + (i % 100) * 0.5
    Histogram::record(latency_histogram, latency)
  }
  
  assert_eq(request_counter.name, "high.freq.requests")
  assert_eq(latency_histogram.name, "high.freq.latency")
}

// Test 2: Complex attribute nesting and manipulation
test "complex nested attributes operations" {
  let attrs = Attributes::new()
  
  // Test deeply nested string attributes
  Attributes::set(attrs, "level1.level2.level3.string", StringValue("deep.value"))
  Attributes::set(attrs, "level1.level2.level3.number", IntValue(42))
  Attributes::set(attrs, "level1.level2.level3.boolean", BoolValue(true))
  Attributes::set(attrs, "level1.level2.level3.float", FloatValue(3.14159))
  
  // Test array attributes with complex data
  Attributes::set(attrs, "complex.array", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attrs, "numeric.array", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // Test attributes with special characters in keys
  Attributes::set(attrs, "key.with.dots", StringValue("value.with.dots"))
  Attributes::set(attrs, "key-with-dashes", StringValue("value-with-dashes"))
  Attributes::set(attrs, "key_with_underscores", StringValue("value_with_underscores"))
  Attributes::set(attrs, "key.with spaces", StringValue("value with spaces"))
  
  // Verify complex attributes
  let deep_string = Attributes::get(attrs, "level1.level2.level3.string")
  let deep_number = Attributes::get(attrs, "level1.level2.level3.number")
  let deep_boolean = Attributes::get(attrs, "level1.level2.level3.boolean")
  let deep_float = Attributes::get(attrs, "level1.level2.level3.float")
  
  assert_eq(deep_string, Some(StringValue("deep.value")))
  assert_eq(deep_number, Some(IntValue(42)))
  assert_eq(deep_boolean, Some(BoolValue(true)))
  assert_eq(deep_float, Some(FloatValue(3.14159)))
}

// Test 3: Span context validation and edge cases
test "span context validation and edge cases" {
  // Test valid span contexts
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1,key2=value2")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  
  // Test invalid span contexts with empty trace ID
  let empty_trace_ctx = SpanContext::new("", "b7ad6b7169203331", true, "")
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  
  // Test invalid span contexts with empty span ID
  let empty_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
  
  // Test invalid span contexts with both empty
  let empty_both_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_both_ctx))
  assert_false(SpanContext::is_sampled(empty_both_ctx))
  
  // Test span context with complex trace state
  let complex_trace_state_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1,key2=value2,key3=value3")
  assert_eq(SpanContext::trace_state(complex_trace_state_ctx), "key1=value1,key2=value2,key3=value3")
  
  // Test span context with empty trace state
  let empty_trace_state_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_eq(SpanContext::trace_state(empty_trace_state_ctx), "")
}

// Test 4: Advanced logging with structured data
test "advanced logging with structured data" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "structured-logger")
  
  // Create structured log attributes
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "event.type", StringValue("user.action"))
  Attributes::set(log_attrs, "user.id", StringValue("user-12345"))
  Attributes::set(log_attrs, "action", StringValue("purchase"))
  Attributes::set(log_attrs, "item.count", IntValue(3))
  Attributes::set(log_attrs, "total.amount", FloatValue(99.99))
  Attributes::set(log_attrs, "currency", StringValue("USD"))
  
  // Create structured log record
  let structured_log = LogRecord::new_with_context(
    Info,
    Some("User completed purchase"),
    Some(log_attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  // Verify structured log properties
  assert_eq(LogRecord::severity_number(structured_log), Info)
  assert_eq(LogRecord::body(structured_log), Some("User completed purchase"))
  assert_eq(LogRecord::trace_id(structured_log), Some("trace-12345"))
  assert_eq(LogRecord::span_id(structured_log), Some("span-67890"))
  
  // Emit structured log
  Logger::emit(logger, structured_log)
  
  // Test error log with exception details
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "exception.type", StringValue("DatabaseException"))
  Attributes::set(error_attrs, "exception.message", StringValue("Connection timeout"))
  Attributes::set(error_attrs, "error.code", IntValue(500))
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Database operation failed"),
    Some(error_attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-error-123"),
    Some("span-error-456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::body(error_log), Some("Database operation failed"))
  
  // Emit error log
  Logger::emit(logger, error_log)
}

// Test 5: Resource operations with environment detection
test "resource operations with environment detection" {
  // Test resource with standard attributes
  let base_resource = Resource::new()
  let service_resource = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.namespace", StringValue("production")),
    ("service.instance.id", StringValue("instance-abc123"))
  ])
  
  // Test resource with host information
  let host_resource = Resource::with_attributes(base_resource, [
    ("host.name", StringValue("prod-server-01")),
    ("host.id", StringValue("host-456def")),
    ("host.type", StringValue("virtual_machine")),
    ("host.image.name", StringValue("ubuntu-20.04")),
    ("host.image.id", StringValue("ami-12345678")),
    ("host.arch", StringValue("x86_64"))
  ])
  
  // Test resource with process information
  let process_resource = Resource::with_attributes(base_resource, [
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("azimuth-service")),
    ("process.executable.path", StringValue("/usr/local/bin/azimuth-service")),
    ("process.command", StringValue("azimuth-service --config /etc/azimuth/config.yaml")),
    ("process.command_line", ArrayStringValue(["azimuth-service", "--config", "/etc/azimuth/config.yaml"])),
    ("process.owner", StringValue("azimuth-user"))
  ])
  
  // Test resource merging with multiple sources
  let merged_resource = Resource::merge(service_resource, host_resource)
  let fully_merged_resource = Resource::merge(merged_resource, process_resource)
  
  // Verify merged resource contains all attributes
  assert_eq(Resource::get_attribute(fully_merged_resource, "service.name"), Some(StringValue("azimuth-service")))
  assert_eq(Resource::get_attribute(fully_merged_resource, "service.version"), Some(StringValue("2.1.0")))
  assert_eq(Resource::get_attribute(fully_merged_resource, "host.name"), Some(StringValue("prod-server-01")))
  assert_eq(Resource::get_attribute(fully_merged_resource, "process.pid"), Some(IntValue(12345)))
}

// Test 6: Context operations with complex data types
test "context operations with complex data types" {
  // Test context with string values
  let ctx1 = Context::with_value(Context::root(), ContextKey::new("string.value"), "test.string")
  
  // Test context with numeric values
  let ctx2 = Context::with_value(ctx1, ContextKey::new("numeric.value"), "12345")
  
  // Test context with boolean values
  let ctx3 = Context::with_value(ctx2, ContextKey::new("boolean.value"), "true")
  
  // Test context with JSON-like values
  let ctx4 = Context::with_value(ctx3, ContextKey::new("json.value"), "{\"key\": \"value\", \"number\": 42}")
  
  // Test context with array-like values
  let ctx5 = Context::with_value(ctx4, ContextKey::new("array.value"), "[1, 2, 3, 4, 5]")
  
  // Test context with special characters
  let ctx6 = Context::with_value(ctx5, ContextKey::new("special.chars"), "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  
  // Test context with Unicode characters
  let ctx7 = Context::with_value(ctx6, ContextKey::new("unicode.value"), "测试中文🚀emoji")
  
  // Verify all context values
  assert_eq(Context::get(ctx7, ContextKey::new("string.value")), Some("test.string"))
  assert_eq(Context::get(ctx7, ContextKey::new("numeric.value")), Some("12345"))
  assert_eq(Context::get(ctx7, ContextKey::new("boolean.value")), Some("true"))
  assert_eq(Context::get(ctx7, ContextKey::new("json.value")), Some("{\"key\": \"value\", \"number\": 42}"))
  assert_eq(Context::get(ctx7, ContextKey::new("array.value")), Some("[1, 2, 3, 4, 5]"))
  assert_eq(Context::get(ctx7, ContextKey::new("special.chars")), Some("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"))
  assert_eq(Context::get(ctx7, ContextKey::new("unicode.value")), Some("测试中文🚀emoji"))
  
  // Test missing keys
  assert_eq(Context::get(ctx7, ContextKey::new("missing.key")), None)
}

// Test 7: Baggage operations with complex scenarios
test "baggage operations with complex scenarios" {
  // Test baggage with multiple entries
  let baggage = Baggage::new()
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session-abcdef")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "req-67890")
  let baggage4 = Baggage::set_entry(baggage3, "trace.id", "trace-12345")
  let baggage5 = Baggage::set_entry(baggage4, "correlation.id", "corr-abcde")
  
  // Test baggage with special characters
  let baggage6 = Baggage::set_entry(baggage5, "special.chars", "!@#$%^&*()")
  let baggage7 = Baggage::set_entry(baggage6, "unicode.value", "测试中文🚀")
  let baggage8 = Baggage::set_entry(baggage7, "url.encoded", "https://example.com/path?param=value&other=test")
  
  // Verify baggage entries
  assert_eq(Baggage::get_entry(baggage8, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(baggage8, "session.id"), Some("session-abcdef"))
  assert_eq(Baggage::get_entry(baggage8, "request.id"), Some("req-67890"))
  assert_eq(Baggage::get_entry(baggage8, "trace.id"), Some("trace-12345"))
  assert_eq(Baggage::get_entry(baggage8, "correlation.id"), Some("corr-abcde"))
  assert_eq(Baggage::get_entry(baggage8, "special.chars"), Some("!@#$%^&*()"))
  assert_eq(Baggage::get_entry(baggage8, "unicode.value"), Some("测试中文🚀"))
  assert_eq(Baggage::get_entry(baggage8, "url.encoded"), Some("https://example.com/path?param=value&other=test"))
  
  // Test baggage removal
  let baggage9 = Baggage::remove_entry(baggage8, "user.id")
  let baggage10 = Baggage::remove_entry(baggage9, "session.id")
  
  assert_eq(Baggage::get_entry(baggage10, "user.id"), None)
  assert_eq(Baggage::get_entry(baggage10, "session.id"), None)
  assert_eq(Baggage::get_entry(baggage10, "request.id"), Some("req-67890"))  // Should still exist
}

// Test 8: Instrument operations with edge cases
test "instrument operations with edge cases" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "edge-case-test")
  
  // Test instrument with empty name
  let empty_name_counter = Meter::create_counter(meter, "", Some("Empty name counter"), Some("count"))
  assert_eq(empty_name_counter.name, "")
  
  // Test instrument with very long name
  let long_name = "this.is.a.very.long.instrument.name.that.tests.boundary.conditions.for.maximum.allowed.length"
  let long_name_counter = Meter::create_counter(meter, long_name, Some("Long name counter"), Some("count"))
  assert_eq(long_name_counter.name, long_name)
  
  // Test instrument with special characters in name
  let special_name_counter = Meter::create_counter(meter, "metric.with-special_chars_123.test", Some("Special chars counter"), Some("count"))
  assert_eq(special_name_counter.name, "metric.with-special_chars_123.test")
  
  // Test instrument with None description
  let no_desc_counter = Meter::create_counter(meter, "no.description", None, None)
  assert_eq(no_desc_counter.description, None)
  
  // Test instrument with None unit
  let no_unit_counter = Meter::create_counter(meter, "no.unit", Some("No unit counter"), None)
  assert_eq(no_unit_counter.unit, None)
  
  // Test instrument with empty description
  let empty_desc_counter = Meter::create_counter(meter, "empty.description", Some(""), Some("count"))
  assert_eq(empty_desc_counter.description, Some(""))
  
  // Test instrument with empty unit
  let empty_unit_counter = Meter::create_counter(meter, "empty.unit", Some("Empty unit counter"), Some(""))
  assert_eq(empty_unit_counter.unit, Some(""))
}

// Test 9: Span operations with complex scenarios
test "span operations with complex scenarios" {
  // Test span with all possible kinds
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "key1=value1")
  
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("server-operation", Server, span_ctx)
  let client_span = Span::new("client-operation", Client, span_ctx)
  let producer_span = Span::new("producer-operation", Producer, span_ctx)
  let consumer_span = Span::new("consumer-operation", Consumer, span_ctx)
  
  // Test span with complex events
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "event.type", StringValue("database.query"))
  Attributes::set(complex_attrs, "db.statement", StringValue("SELECT * FROM users WHERE id = ?"))
  Attributes::set(complex_attrs, "db.type", StringValue("postgresql"))
  Attributes::set(complex_attrs, "db.instance", StringValue("prod-db-01"))
  Attributes::set(complex_attrs, "db.user", StringValue("azimuth_service"))
  Attributes::set(complex_attrs, "execution.time", IntValue(150))
  
  Span::add_event(internal_span, "database.query.started", Some(complex_attrs))
  Span::add_event(internal_span, "database.query.completed", Some([
    ("result.count", IntValue(42)),
    ("execution.time", IntValue(150))
  ]))
  
  // Test span with complex status
  Span::set_status(internal_span, Ok, Some("Operation completed successfully"))
  Span::set_status(server_span, Error, Some("Request validation failed"))
  
  // Test span with multiple status changes
  Span::set_status(client_span, Ok, Some("Initial success"))
  Span::set_status(client_span, Error, Some("Retrying due to network error"))
  Span::set_status(client_span, Ok, Some("Final success after retry"))
  
  // Verify span properties
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::name(server_span), "server-operation")
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::name(client_span), "client-operation")
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::name(producer_span), "producer-operation")
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::name(consumer_span), "consumer-operation")
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // End all spans
  Span::end(internal_span)
  Span::end(server_span)
  Span::end(client_span)
  Span::end(producer_span)
  Span::end(consumer_span)
}

// Test 10: Comprehensive telemetry workflow
test "comprehensive telemetry workflow" {
  // Create providers
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // Create instruments
  let tracer = TracerProvider::get_tracer(tracer_provider, "workflow-test")
  let meter = MeterProvider::get_meter(meter_provider, "workflow-test")
  let logger = LoggerProvider::get_logger(logger_provider, "workflow-test")
  
  // Create metrics instruments
  let request_counter = Meter::create_counter(meter, "workflow.requests", Some("Workflow requests"), Some("count"))
  let duration_histogram = Meter::create_histogram(meter, "workflow.duration", Some("Workflow duration"), Some("ms"))
  let active_gauge = Meter::create_gauge(meter, "workflow.active", Some("Active workflows"), Some("count"))
  
  // Start workflow span
  let span = Tracer::start_span(tracer, "workflow.execution", Some([
    ("workflow.type", StringValue("data.processing")),
    ("workflow.id", StringValue("workflow-12345"))
  ]))
  
  // Record metrics
  Counter::add(request_counter, 1.0)
  UpDownCounter::add(active_gauge, 1.0)
  
  // Add workflow events
  Span::add_event(span, "workflow.started", Some([
    ("workflow.step", StringValue("initialization")),
    ("timestamp", IntValue(1735689600))
  ]))
  
  Span::add_event(span, "data.processing.started", Some([
    ("workflow.step", StringValue("processing")),
    ("data.size", IntValue(1000))
  ]))
  
  // Log workflow progress
  let progress_log = LogRecord::new_with_context(
    Info,
    Some("Workflow processing 50% complete"),
    Some([
      ("workflow.id", StringValue("workflow-12345")),
      ("progress.percent", IntValue(50))
    ]),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, progress_log)
  
  // Record duration
  Histogram::record(duration_histogram, 250.5)
  
  // Add completion event
  Span::add_event(span, "workflow.completed", Some([
    ("workflow.step", StringValue("completion")),
    ("result.status", StringValue("success"))
  ]))
  
  // Log completion
  let completion_log = LogRecord::new_with_context(
    Info,
    Some("Workflow completed successfully"),
    Some([
      ("workflow.id", StringValue("workflow-12345")),
      ("result.status", StringValue("success")),
      ("total.duration", FloatValue(250.5))
    ]),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, completion_log)
  
  // Update metrics
  UpDownCounter::add(active_gauge, -1.0)
  
  // Set span status and end
  Span::set_status(span, Ok, Some("Workflow completed successfully"))
  Span::end(span)
  
  // Verify workflow completion
  assert_true(true)  // If we reach here, the workflow completed successfully
}