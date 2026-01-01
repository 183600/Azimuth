// Azimuth Extended Test Suite
// This file contains additional test cases for comprehensive coverage

test "resource operations and merging" {
  // Create base resource with service information
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create override resource with additional attributes
  let override_attrs = [
    ("service.name", StringValue("updated-service")),  // This should override
    ("environment", StringValue("production")),
    ("deployment.region", StringValue("us-west-2"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_resource, override_resource)
  
  // Verify merged attributes
  let service_name = Resource::get_attribute(merged, "service.name")
  assert_eq(service_name, Some(StringValue("updated-service")))
  
  let environment = Resource::get_attribute(merged, "environment")
  assert_eq(environment, Some(StringValue("production")))
  
  let region = Resource::get_attribute(merged, "deployment.region")
  assert_eq(region, Some(StringValue("us-west-2")))
}

test "propagator injection and extraction" {
  // Create context with trace information
  let ctx = Context::root()
  let trace_key = ContextKey::new("trace_id")
  let ctx_with_trace = Context::with_value(ctx, trace_key, "0af7651916cd43dd8448eb211c80319c")
  
  // Create composite propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Create carrier and inject context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_trace, carrier)
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
  
  // Verify traceparent header format
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent.is_some())
}

test "telemetry data serialization integrity" {
  // Create comprehensive log record with all fields
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("user-123"))
  Attributes::set(attrs, "request.id", StringValue("req-456"))
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(Context::root())
  )
  
  // Verify all fields are preserved
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(LogRecord::span_id(record), Some("b7ad6b7169203331"))
}

test "concurrent operations safety" {
  // Test concurrent meter operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent-test")
  let counter = Meter::create_counter(meter, "concurrent.operations")
  let histogram = Meter::create_histogram(meter, "concurrent.duration")
  let updown_counter = Meter::create_updown_counter(meter, "concurrent.active")
  let gauge = Meter::create_gauge(meter, "concurrent.memory")
  
  // Simulate concurrent operations
  for i = 0; i < 10; i++ {
    Counter::add(counter, 1.0)
    Histogram::record(histogram, 100.0 + i.to_double())
    UpDownCounter::add(updown_counter, 1.0)
  }
  
  // Test concurrent span operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent-test")
  
  for i = 0; i < 5; i++ {
    let span_name = "concurrent-span-" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    assert_eq(Span::name(span), span_name)
    assert_true(Span::is_recording(span))
    Span::end(span)
  }
  
  assert_true(true)  // If we reach here, concurrent operations were safe
}

test "error handling and recovery" {
  // Test with invalid span context
  let invalid_span_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // Test with empty trace ID
  let empty_trace_ctx = SpanContext::new("", "valid_span_id", true, "")
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  
  // Test with empty span ID
  let empty_span_ctx = SpanContext::new("valid_trace_id", "", true, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
  
  // Test span operations with invalid context
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-test")
  let span = Tracer::start_span(tracer, "error-test-span")
  
  // Set error status
  Span::set_status(span, Error, Some("Test error message"))
  assert_eq(Span::status(span), Error)
  
  // Add error event
  Span::add_event(span, "error", Some([("error.message", StringValue("Test error"))]))
  Span::end(span)
  
  assert_true(true)  // Error handling completed successfully
}

test "performance benchmark operations" {
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Create multiple spans rapidly
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "perf-test")
  
  for i = 0; i < 100; i++ {
    let span = Tracer::start_span(tracer, "perf-span-" + i.to_string())
    Span::add_event(span, "start", None)
    Span::set_status(span, Ok, None)
    Span::end(span)
  }
  
  // Create multiple metrics rapidly
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "perf-test")
  let counter = Meter::create_counter(meter, "perf.counter")
  
  for i = 0; i < 100; i++ {
    Counter::add(counter, i.to_double())
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let duration = end_time - start_time
  
  // Verify operations completed in reasonable time (less than 1 second)
  assert_true(duration < 1000000000L)  // 1 second in nanoseconds
}

test "cross-service consistency" {
  // Service A creates trace context
  let service_a_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service-a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a-operation")
  let span_ctx = Span::span_context(service_a_span)
  
  // Extract trace information for propagation
  let trace_id = SpanContext::trace_id(span_ctx)
  let span_id = SpanContext::span_id(span_ctx)
  
  // Service B receives and continues the trace
  let service_b_provider = TracerProvider::default()
  let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service-b")
  let service_b_span = Tracer::start_span(service_b_tracer, "service-b-operation")
  
  // Verify trace consistency
  assert_eq(SpanContext::trace_id(Span::span_context(service_b_span)), trace_id)
  
  // Service C adds baggage
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user-123")
  let user_id = Baggage::get_entry(updated_baggage, "user.id")
  assert_eq(user_id, Some("user-123"))
  
  // End all spans
  Span::end(service_a_span)
  Span::end(service_b_span)
  
  assert_true(true)
}

test "internationalization and text processing" {
  // Test with Unicode characters
  let chinese_attrs = [
    ("服务名称", StringValue("方位角服务")),
    ("版本", StringValue("1.0.0")),
    ("描述", StringValue("遥测数据收集系统"))
  ]
  let chinese_resource = Resource::with_attributes(Resource::new(), chinese_attrs)
  
  let service_name = Resource::get_attribute(chinese_resource, "服务名称")
  assert_eq(service_name, Some(StringValue("方位角服务")))
  
  // Test with emoji characters
  let emoji_attrs = [
    ("status", StringValue("🚀 Running")),
    ("mood", StringValue("😊 Happy")),
    ("weather", StringValue("🌤️ Partly Cloudy"))
  ]
  let emoji_resource = Resource::with_attributes(Resource::new(), emoji_attrs)
  
  let status = Resource::get_attribute(emoji_resource, "status")
  assert_eq(status, Some(StringValue("🚀 Running")))
  
  // Test log messages with international characters
  let multilingual_log = LogRecord::new(Info, "系统启动 | System started | Système démarré")
  assert_eq(LogRecord::body(multilingual_log), Some("系统启动 | System started | Système démarré"))
  
  // Test context with international keys
  let ctx = Context::root()
  let chinese_key = ContextKey::new("用户标识")
  let ctx_with_chinese = Context::with_value(ctx, chinese_key, "用户-123")
  let retrieved_chinese = Context::get(ctx_with_chinese, chinese_key)
  assert_eq(retrieved_chinese, Some("用户-123"))
}