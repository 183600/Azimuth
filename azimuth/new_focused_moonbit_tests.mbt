// New Focused MoonBit Test Suite for Azimuth Telemetry System
// This file contains focused test cases covering key functionality areas

// Test 1: Span context operations with validation
test "span context trace operations" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
  assert_eq(SpanContext::trace_state(span_ctx), "key1=value1,key2=value2")
}

// Test 2: Attributes with various data types
test "attributes multi-type operations" {
  let attrs = Attributes::new()
  
  // Test string attribute
  Attributes::set(attrs, "string.key", StringValue("string_value"))
  let string_result = Attributes::get(attrs, "string.key")
  assert_eq(string_result, Some(StringValue("string_value")))
  
  // Test integer attribute
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_result = Attributes::get(attrs, "int.key")
  assert_eq(int_result, Some(IntValue(42)))
  
  // Test boolean attribute
  Attributes::set(attrs, "bool.key", BoolValue(true))
  let bool_result = Attributes::get(attrs, "bool.key")
  assert_eq(bool_result, Some(BoolValue(true)))
  
  // Test float attribute
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  let float_result = Attributes::get(attrs, "float.key")
  assert_eq(float_result, Some(FloatValue(3.14)))
}

// Test 3: Context propagation with baggage
test "context baggage propagation" {
  let ctx = Context::root()
  let baggage = Baggage::new()
  
  // Add baggage entries
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-67890")
  
  // Create context with baggage
  let key = ContextKey::new("baggage")
  let ctx_with_baggage = Context::with_value(ctx, key, baggage_with_session)
  
  // Verify baggage propagation
  let propagated_baggage = Context::get(ctx_with_baggage, key)
  assert_true(propagated_baggage.is_some())
}

// Test 4: Metrics instruments creation and operations
test "metrics instruments creation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Create counter
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  // Create histogram
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Create up-down counter
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test up-down"), Some("items"))
  assert_eq(updown_counter.name, "test.updown")
  assert_eq(updown_counter.description, Some("Test up-down"))
  assert_eq(updown_counter.unit, Some("items"))
  
  // Create gauge
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  assert_eq(gauge.name, "test.gauge")
  assert_eq(gauge.description, Some("Test gauge"))
  assert_eq(gauge.unit, Some("percent"))
}

// Test 5: Log records with severity levels
test "log records severity levels" {
  // Create log records with different severity levels
  let trace_log = LogRecord::new(Trace, "Trace message")
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  let fatal_log = LogRecord::new(Fatal, "Fatal message")
  
  // Verify severity levels
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Verify log bodies
  assert_eq(LogRecord::body(trace_log), Some("Trace message"))
  assert_eq(LogRecord::body(debug_log), Some("Debug message"))
  assert_eq(LogRecord::body(info_log), Some("Info message"))
  assert_eq(LogRecord::body(warn_log), Some("Warning message"))
  assert_eq(LogRecord::body(error_log), Some("Error message"))
  assert_eq(LogRecord::body(fatal_log), Some("Fatal message"))
}

// Test 6: Resource operations with attributes
test "resource attributes operations" {
  let empty_resource = Resource::new()
  
  // Create resource with attributes
  let resource_with_attrs = Resource::with_attributes(empty_resource, [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("test")),
    ("host.name", StringValue("test-host")),
    ("process.id", IntValue(12345))
  ])
  
  // Verify resource attributes
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.instance.id"), Some(StringValue("instance-123")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "deployment.environment"), Some(StringValue("test")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "host.name"), Some(StringValue("test-host")))
  assert_eq(Resource::get_attribute(resource_with_attrs, "process.id"), Some(IntValue(12345)))
  
  // Test resource merging
  let override_resource = Resource::with_attributes(empty_resource, [
    ("service.version", StringValue("2.0.0")),  // Override existing
    ("new.attribute", StringValue("new-value"))  // New attribute
  ])
  
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // Verify merged resource (override should win)
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("test-service")))  // Unchanged
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("2.0.0")))  // Overridden
  assert_eq(Resource::get_attribute(merged_resource, "new.attribute"), Some(StringValue("new-value")))  // New
}

// Test 7: Span lifecycle with different kinds
test "span lifecycle with kinds" {
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "")
  
  // Create spans with different kinds
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("server-operation", Server, span_ctx)
  let client_span = Span::new("client-operation", Client, span_ctx)
  let producer_span = Span::new("producer-operation", Producer, span_ctx)
  let consumer_span = Span::new("consumer-operation", Consumer, span_ctx)
  
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
}

// Test 8: Propagator operations with composite
test "composite propagator operations" {
  // Test individual propagators
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Test composite propagator with multiple propagators
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Test carrier operations
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Test inject operations
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Test extract operations
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction results
  let key = ContextKey::new("extracted")
  let extracted = Context::get(extracted_ctx, key)
  assert_eq(extracted, Some("true"))
}

// Test 9: Array attributes operations
test "array attributes operations" {
  let attrs = Attributes::new()
  
  // Test string array attributes
  Attributes::set(attrs, "string.array", ArrayStringValue(["a", "b", "c"]))
  let string_array_result = Attributes::get(attrs, "string.array")
  assert_eq(string_array_result, Some(ArrayStringValue(["a", "b", "c"])))
  
  // Test int array attributes
  Attributes::set(attrs, "int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  let int_array_result = Attributes::get(attrs, "int.array")
  assert_eq(int_array_result, Some(ArrayIntValue([1, 2, 3, 4, 5])))
  
  // Test empty array attributes
  Attributes::set(attrs, "empty.array", ArrayStringValue([]))
  let empty_array_result = Attributes::get(attrs, "empty.array")
  assert_eq(empty_array_result, Some(ArrayStringValue([])))
  
  // Test single element array
  Attributes::set(attrs, "single.array", ArrayStringValue(["single"]))
  let single_array_result = Attributes::get(attrs, "single.array")
  assert_eq(single_array_result, Some(ArrayStringValue(["single"])))
}

// Test 10: Logger operations with different providers
test "logger provider operations" {
  // Test logger provider creation
  let logger_provider = LoggerProvider::default()
  
  // Test logger creation
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  assert_eq(logger.scope.name, "test-logger")
  
  // Test logger with different scopes
  let telemetry_logger = LoggerProvider::get_logger(logger_provider, "telemetry.logger")
  assert_eq(telemetry_logger.scope.name, "telemetry.logger")
  
  let metrics_logger = LoggerProvider::get_logger(logger_provider, "metrics.logger")
  assert_eq(metrics_logger.scope.name, "metrics.logger")
  
  let tracing_logger = LoggerProvider::get_logger(logger_provider, "tracing.logger")
  assert_eq(tracing_logger.scope.name, "tracing.logger")
  
  // Test log record emission
  let info_log = LogRecord::new(Info, "Test info message")
  let error_log = LogRecord::new(Error, "Test error message")
  
  Logger::emit(logger, info_log)
  Logger::emit(logger, error_log)
  Logger::emit(telemetry_logger, info_log)
  Logger::emit(metrics_logger, error_log)
  Logger::emit(tracing_logger, info_log)
}