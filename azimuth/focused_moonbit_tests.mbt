// Focused MoonBit Tests for Azimuth Telemetry System
// Core functionality tests with comprehensive coverage

// Test 1: Trace context creation and validation
pub test "trace context creation and validation" {
  let trace_id = "550e8400-e29b-41d4-a716-446655440000"
  let span_id = "550e8400-e29b-41d4-a716"
  let sampled = true
  let trace_state = "key1=value1,key2=value2"
  
  let span_ctx = azimuth::SpanContext::new(trace_id, span_id, sampled, trace_state)
  
  assert_eq(span_ctx.trace_id(), trace_id)
  assert_eq(span_ctx.span_id(), span_id)
  assert_eq(span_ctx.is_sampled(), sampled)
  assert_eq(span_ctx.trace_state(), trace_state)
  
  // Test trace context validation
  assert_eq(span_ctx.is_valid(), true)
  
  // Test invalid trace context
  let invalid_ctx = azimuth::SpanContext::new("", "", false, "")
  assert_eq(invalid_ctx.is_valid(), false)
}

// Test 2: Attributes operations with type safety
pub test "attributes type safety operations" {
  let attrs = azimuth::Attributes::new()
  
  // Test string attributes
  azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("test_value"))
  let string_result = azimuth::Attributes::get(attrs, "string.key")
  assert_eq(string_result, Some(azimuth::StringValue("test_value")))
  
  // Test integer attributes
  azimuth::Attributes::set(attrs, "int.key", azimuth::IntValue(123))
  let int_result = azimuth::Attributes::get(attrs, "int.key")
  assert_eq(int_result, Some(azimuth::IntValue(123)))
  
  // Test float attributes
  azimuth::Attributes::set(attrs, "float.key", azimuth::FloatValue(3.14))
  let float_result = azimuth::Attributes::get(attrs, "float.key")
  assert_eq(float_result, Some(azimuth::FloatValue(3.14)))
  
  // Test boolean attributes
  azimuth::Attributes::set(attrs, "bool.key", azimuth::BoolValue(true))
  let bool_result = azimuth::Attributes::get(attrs, "bool.key")
  assert_eq(bool_result, Some(azimuth::BoolValue(true)))
  
  // Test array attributes
  azimuth::Attributes::set(attrs, "array.key", azimuth::ArrayStringValue(["a", "b", "c"]))
  let array_result = azimuth::Attributes::get(attrs, "array.key")
  assert_eq(array_result, Some(azimuth::ArrayStringValue(["a", "b", "c"])))
  
  // Test non-existent key
  let missing = azimuth::Attributes::get(attrs, "nonexistent")
  assert_eq(missing, None)
}

// Test 3: Span lifecycle management
pub test "span lifecycle management" {
  let span_ctx = azimuth::SpanContext::new("trace-123", "span-456", true, "")
  let span = azimuth::Span::new("test-operation", azimuth::Internal, span_ctx)
  
  // Test span properties
  assert_eq(span.name(), "test-operation")
  assert_eq(span.kind(), azimuth::Internal)
  assert_eq(span.context().trace_id(), "trace-123")
  assert_eq(span.context().span_id(), "span-456")
  
  // Test span events
  azimuth::Span::add_event(span, "event1", azimuth::Attributes::new())
  azimuth::Span::add_event_with_timestamp(span, "event2", 1234567890, azimuth::Attributes::new())
  
  // Test span status
  azimuth::Span::set_status(span, azimuth::Ok, "operation completed")
  
  // Test span ending
  azimuth::Span::end(span)
  assert_eq(span.is_ended(), true)
}

// Test 4: Logger functionality
pub test "logger functionality" {
  let logger = azimuth::Logger::new("test.logger")
  
  // Test logger properties
  assert_eq(logger.name(), "test.logger")
  
  // Test log emission
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "user.id", azimuth::StringValue("user123"))
  azimuth::Attributes::set(log_attrs, "action", azimuth::StringValue("login"))
  
  // Emit different log levels
  azimuth::Logger::emit_log(logger, azimuth::Info, "User logged in successfully", log_attrs)
  azimuth::Logger::emit_log(logger, azimuth::Error, "Login failed", log_attrs)
  azimuth::Logger::emit_log(logger, azimuth::Debug, "Debug information", log_attrs)
  azimuth::Logger::emit_log(logger, azimuth::Warn, "Warning message", log_attrs)
}

// Test 5: Metrics operations
pub test "metrics operations" {
  let metrics = azimuth::Metrics::new("test.metrics")
  
  // Test counter metric
  let counter = azimuth::Metrics::create_counter(metrics, "request.count", "Total number of requests")
  azimuth::Counter::add(counter, 1, azimuth::Attributes::new())
  azimuth::Counter::add(counter, 5, azimuth::Attributes::new())
  
  // Test histogram metric
  let histogram = azimuth::Metrics::create_histogram(metrics, "request.duration", "Request duration in seconds")
  azimuth::Histogram::record(histogram, 0.1, azimuth::Attributes::new())
  azimuth::Histogram::record(histogram, 0.5, azimuth::Attributes::new())
  azimuth::Histogram::record(histogram, 1.0, azimuth::Attributes::new())
  
  // Test gauge metric
  let gauge = azimuth::Metrics::create_gauge(metrics, "memory.usage", "Memory usage in bytes")
  azimuth::Gauge::set(gauge, 1024, azimuth::Attributes::new())
  azimuth::Gauge::set(gauge, 2048, azimuth::Attributes::new())
}

// Test 6: Context propagation
pub test "context propagation" {
  let ctx = azimuth::Context::root()
  
  // Test context with span
  let span_ctx = azimuth::SpanContext::new("trace-789", "span-012", true, "")
  let span = azimuth::Span::new("propagation-test", azimuth::Client, span_ctx)
  let ctx_with_span = azimuth::Context::with_span(ctx, span)
  
  // Test context retrieval
  let retrieved_span = azimuth::Context::span(ctx_with_span)
  assert_eq(retrieved_span.context().trace_id(), "trace-789")
  assert_eq(retrieved_span.context().span_id(), "span-012")
  
  // Test context with baggage
  let baggage = azimuth::Baggage::new()
  azimuth::Baggage::set(baggage, "user.id", "user456")
  let ctx_with_baggage = azimuth::Context::with_baggage(ctx_with_span, baggage)
  
  let retrieved_baggage = azimuth::Context::baggage(ctx_with_baggage)
  assert_eq(azimuth::Baggage::get(retrieved_baggage, "user.id"), Some("user456"))
}

// Test 7: Propagator injection and extraction
pub test "propagator injection extraction" {
  let propagator = azimuth::W3CTraceContextPropagator::new()
  let carrier = azimuth::TextMapCarrier::new()
  
  // Create context with span
  let span_ctx = azimuth::SpanContext::new("trace-inject", "span-inject", true, "key1=value1")
  let span = azimuth::Span::new("inject-test", azimuth::Server, span_ctx)
  let ctx = azimuth::Context::with_span(azimuth::Context::root(), span)
  
  // Test injection
  azimuth::Propagator::inject(propagator, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = azimuth::Propagator::extract(propagator, carrier)
  let extracted_span = azimuth::Context::span(extracted_ctx)
  
  assert_eq(extracted_span.context().trace_id(), "trace-inject")
  assert_eq(extracted_span.context().span_id(), "span-inject")
  assert_eq(extracted_span.context().is_sampled(), true)
}

// Test 8: Resource management
pub test "resource management" {
  let resource = azimuth::Resource::new()
  
  // Test resource attributes
  azimuth::Resource::set_attribute(resource, "service.name", azimuth::StringValue("test-service"))
  azimuth::Resource::set_attribute(resource, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Resource::set_attribute(resource, "service.instance.id", azimuth::StringValue("instance-123"))
  
  // Test resource merging
  let additional_resource = azimuth::Resource::new()
  azimuth::Resource::set_attribute(additional_resource, "host.name", azimuth::StringValue("localhost"))
  azimuth::Resource::set_attribute(additional_resource, "service.name", azimuth::StringValue("updated-service"))
  
  let merged = azimuth::Resource::merge(resource, additional_resource)
  
  // service.name should be from additional_resource (takes precedence)
  assert_eq(azimuth::Resource::get_attribute(merged, "service.name"), 
    Some(azimuth::StringValue("updated-service")))
  assert_eq(azimuth::Resource::get_attribute(merged, "service.version"), 
    Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged, "host.name"), 
    Some(azimuth::StringValue("localhost")))
}

// Test 9: Error handling and resilience
pub test "error handling resilience" {
  // Test invalid attribute handling
  let attrs = azimuth::Attributes::new()
  
  // Test with null/empty values
  azimuth::Attributes::set(attrs, "empty.string", azimuth::StringValue(""))
  azimuth::Attributes::set(attrs, "zero.int", azimuth::IntValue(0))
  azimuth::Attributes::set(attrs, "zero.float", azimuth::FloatValue(0.0))
  
  // Test retrieval
  assert_eq(azimuth::Attributes::get(attrs, "empty.string"), Some(azimuth::StringValue("")))
  assert_eq(azimuth::Attributes::get(attrs, "zero.int"), Some(azimuth::IntValue(0)))
  assert_eq(azimuth::Attributes::get(attrs, "zero.float"), Some(azimuth::FloatValue(0.0)))
  
  // Test span with invalid context
  let invalid_span_ctx = azimuth::SpanContext::new("", "", false, "")
  let invalid_span = azimuth::Span::new("invalid-test", azimuth::Internal, invalid_span_ctx)
  
  // Should handle gracefully without crashing
  azimuth::Span::add_event(invalid_span, "test-event", azimuth::Attributes::new())
  azimuth::Span::set_status(invalid_span, azimuth::Error, "invalid context")
  azimuth::Span::end(invalid_span)
}

// Test 10: Internationalization support
pub test "internationalization support" {
  let attrs = azimuth::Attributes::new()
  
  // Test Unicode and multilingual content
  azimuth::Attributes::set(attrs, "chinese.text", azimuth::StringValue("测试中文内容"))
  azimuth::Attributes::set(attrs, "japanese.text", azimuth::StringValue("日本語テスト"))
  azimuth::Attributes::set(attrs, "emoji.test", azimuth::StringValue("🚀🌟💻"))
  azimuth::Attributes::set(attrs, "mixed.content", azimuth::StringValue("中文 English 日本語 🌍"))
  
  // Test retrieval
  assert_eq(azimuth::Attributes::get(attrs, "chinese.text"), 
    Some(azimuth::StringValue("测试中文内容")))
  assert_eq(azimuth::Attributes::get(attrs, "japanese.text"), 
    Some(azimuth::StringValue("日本語テスト")))
  assert_eq(azimuth::Attributes::get(attrs, "emoji.test"), 
    Some(azimuth::StringValue("🚀🌟💻")))
  assert_eq(azimuth::Attributes::get(attrs, "mixed.content"), 
    Some(azimuth::StringValue("中文 English 日本語 🌍")))
  
  // Test logging with internationalized content
  let logger = azimuth::Logger::new("国际化测试")
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "用户名", azimuth::StringValue("张三"))
  
  azimuth::Logger::emit_log(logger, azimuth::Info, "用户登录成功", log_attrs)
}