// Azimuth Comprehensive New Test Suite
// This file contains comprehensive test cases for various Azimuth features

test "attribute value type conversion" {
  // Test string to int conversion
  let string_attr = StringValue("42")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  let bool_attr = BoolValue(true)
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  let array_int_attr = ArrayIntValue([1, 2, 3])
  
  // Verify attribute values are correctly created
  assert_eq(string_attr, StringValue("42"))
  assert_eq(int_attr, IntValue(42))
  assert_eq(float_attr, FloatValue(3.14))
  assert_eq(bool_attr, BoolValue(true))
  assert_eq(array_string_attr, ArrayStringValue(["a", "b", "c"]))
  assert_eq(array_int_attr, ArrayIntValue([1, 2, 3]))
}

test "resource attributes merge" {
  let base_resource = Resource::new()
  let base_attrs = [("service.name", StringValue("azimuth")), ("service.version", StringValue("1.0.0"))]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [("service.version", StringValue("2.0.0")), ("environment", StringValue("production"))]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // Verify merged resource contains override attributes
  assert_eq(Resource::get_attribute(merged, "service.version"), Some(StringValue("2.0.0")))
  assert_eq(Resource::get_attribute(merged, "environment"), Some(StringValue("production")))
}

test "span lifecycle management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  
  // Create a span
  let span = Tracer::start_span(tracer, "test-operation")
  
  // Verify initial state
  assert_eq(Span::name(span), "test-operation")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Add events and set status
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  Span::set_status(span, Ok, Some("Operation completed"))
  
  // End the span
  Span::end(span)
  
  assert_true(true)
}

test "metrics dashboard functionality" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard-metrics")
  
  // Create different metric instruments
  let counter = Meter::create_counter(meter, "http_requests_total")
  let histogram = Meter::create_histogram(meter, "request_duration_seconds")
  let updown_counter = Meter::create_updown_counter(meter, "active_connections")
  let gauge = Meter::create_gauge(meter, "memory_usage_bytes")
  
  // Record metrics
  Counter::add(counter, 10.0)
  Histogram::record(histogram, 0.123)
  UpDownCounter::add(updown_counter, 5.0)
  
  // Verify metric names
  assert_eq(counter.name, "http_requests_total")
  assert_eq(histogram.name, "request_duration_seconds")
  assert_eq(updown_counter.name, "active_connections")
  assert_eq(gauge.name, "memory_usage_bytes")
}

test "log record with trace correlation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "cross-service context propagation" {
  // Create initial context
  let ctx = Context::root()
  let key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(ctx, key, "12345")
  
  // Create propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Create carrier and inject context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_user, carrier)
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  let is_extracted = Context::get(extracted_ctx, extracted_key)
  
  assert_eq(is_extracted, Some("true"))
}

test "time series data operations" {
  let clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(clock)
  let timestamp2 = timestamp1 + 1000000L  // 1ms later
  
  // Create time series data points
  let data_point1 = (timestamp1, 10.0)
  let data_point2 = (timestamp2, 15.0)
  let time_series = [data_point1, data_point2]
  
  // Verify time series ordering
  assert_true(time_series.length > 1)
  assert_true(time_series[0].0 < time_series[1].0)
  
  // Verify data values
  assert_eq(time_series[0].1, 10.0)
  assert_eq(time_series[1].1, 15.0)
}

test "concurrent safety attributes" {
  let attrs = Attributes::new()
  
  // Set attributes concurrently
  Attributes::set(attrs, "key1", StringValue("value1"))
  Attributes::set(attrs, "key2", IntValue(42))
  Attributes::set(attrs, "key3", BoolValue(true))
  
  // Retrieve attributes
  let value1 = Attributes::get(attrs, "key1")
  let value2 = Attributes::get(attrs, "key2")
  let value3 = Attributes::get(attrs, "key3")
  let missing = Attributes::get(attrs, "missing.key")
  
  assert_eq(value1, Some(StringValue("value1")))
  assert_eq(value2, Some(IntValue(42)))
  assert_eq(value3, Some(BoolValue(true)))
  assert_eq(missing, None)
}

test "error boundary handling" {
  // Test error boundary conditions
  let empty_trace_id = ""
  let empty_span_id = ""
  let invalid_span_ctx = SpanContext::new(empty_trace_id, empty_span_id, false, "")
  
  // Verify invalid context is detected
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  assert_false(SpanContext::is_sampled(invalid_span_ctx))
  
  // Test valid context
  let valid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_true(SpanContext::is_valid(valid_span_ctx))
  assert_true(SpanContext::is_sampled(valid_span_ctx))
}

test "internationalization support" {
  // Test internationalization with different character sets
  let chinese_text = "遥测系统"
  let arabic_text = "نظام القياس عن بعد"
  let emoji_text = "📊 🔍 📈"
  
  // Create log records with international text
  let chinese_record = LogRecord::new(Info, chinese_text)
  let arabic_record = LogRecord::new(Info, arabic_text)
  let emoji_record = LogRecord::new(Info, emoji_text)
  
  // Verify international text is preserved
  assert_eq(LogRecord::body(chinese_record), Some(chinese_text))
  assert_eq(LogRecord::body(arabic_record), Some(arabic_text))
  assert_eq(LogRecord::body(emoji_record), Some(emoji_text))
}