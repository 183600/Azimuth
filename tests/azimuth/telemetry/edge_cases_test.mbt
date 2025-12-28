test "span context with very long IDs" {
  // Test span context with very long trace and span IDs
  let long_trace_id = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  let long_span_id = "0123456789abcdef01234567"
  let span_ctx = @azimuth.SpanContext::new(long_trace_id, long_span_id, true, "state=value")
  
  assert_eq(@azimuth.SpanContext::trace_id(span_ctx), long_trace_id)
  assert_eq(@azimuth.SpanContext::span_id(span_ctx), long_span_id)
  assert_true(@azimuth.SpanContext::is_sampled(span_ctx))
}

test "context with empty key" {
  // Test context operations with empty key
  let root_ctx = @azimuth.Context::root()
  let empty_key = @azimuth.ContextKey::new("")
  
  let ctx_with_value = @azimuth.Context::with_value(root_ctx, empty_key, "value.with.empty.key")
  let retrieved_value = @azimuth.Context::get(ctx_with_value, empty_key)
  
  assert_eq(retrieved_value, Some("value.with.empty.key"))
}

test "attributes with all value types" {
  // Test attributes with all possible value types
  let attrs = @azimuth.Attributes::new()
  
  // String value
  @azimuth.Attributes::set(attrs, "string.value", @azimuth.StringValue("test string"))
  
  // Int value
  @azimuth.Attributes::set(attrs, "int.value", @azimuth.IntValue(42))
  
  // Float value
  @azimuth.Attributes::set(attrs, "float.value", @azimuth.FloatValue(3.14159))
  
  // Bool value
  @azimuth.Attributes::set(attrs, "bool.value", @azimuth.BoolValue(true))
  
  // Array string value
  @azimuth.Attributes::set(attrs, "array.string", @azimuth.ArrayStringValue(["a", "b", "c"]))
  
  // Array int value
  @azimuth.Attributes::set(attrs, "array.int", @azimuth.ArrayIntValue([1, 2, 3]))
  
  // Test that all operations complete without exceptions
  assert_true(true)
}

test "span with all possible kinds" {
  // Test creating spans with all possible kinds
  let span_ctx = @azimuth.SpanContext::new("trace123", "span456", true, "")
  
  let internal_span = @azimuth.Span::new("internal", @azimuth.Internal, span_ctx)
  let server_span = @azimuth.Span::new("server", @azimuth.Server, span_ctx)
  let client_span = @azimuth.Span::new("client", @azimuth.Client, span_ctx)
  let producer_span = @azimuth.Span::new("producer", @azimuth.Producer, span_ctx)
  let consumer_span = @azimuth.Span::new("consumer", @azimuth.Consumer, span_ctx)
  
  assert_eq(@azimuth.Span::kind(internal_span), @azimuth.Internal)
  assert_eq(@azimuth.Span::kind(server_span), @azimuth.Server)
  assert_eq(@azimuth.Span::kind(client_span), @azimuth.Client)
  assert_eq(@azimuth.Span::kind(producer_span), @azimuth.Producer)
  assert_eq(@azimuth.Span::kind(consumer_span), @azimuth.Consumer)
}

test "log record with all severity levels" {
  // Test log records with all severity levels
  let trace_record = @azimuth.LogRecord::new(@azimuth.Trace, "Trace message")
  let debug_record = @azimuth.LogRecord::new(@azimuth.Debug, "Debug message")
  let info_record = @azimuth.LogRecord::new(@azimuth.Info, "Info message")
  let warn_record = @azimuth.LogRecord::new(@azimuth.Warn, "Warning message")
  let error_record = @azimuth.LogRecord::new(@azimuth.Error, "Error message")
  let fatal_record = @azimuth.LogRecord::new(@azimuth.Fatal, "Fatal message")
  
  assert_eq(@azimuth.LogRecord::severity_number(trace_record), @azimuth.Trace)
  assert_eq(@azimuth.LogRecord::severity_number(debug_record), @azimuth.Debug)
  assert_eq(@azimuth.LogRecord::severity_number(info_record), @azimuth.Info)
  assert_eq(@azimuth.LogRecord::severity_number(warn_record), @azimuth.Warn)
  assert_eq(@azimuth.LogRecord::severity_number(error_record), @azimuth.Error)
  assert_eq(@azimuth.LogRecord::severity_number(fatal_record), @azimuth.Fatal)
}

test "text map carrier with special header values" {
  // Test text map carrier with special header values
  let carrier = @azimuth.TextMapCarrier::new()
  
  // Set headers with special characters
  @azimuth.TextMapCarrier::set(carrier, "special-chars", "value with spaces and symbols!@#$%^&*()")
  @azimuth.TextMapCarrier::set(carrier, "unicode", "中文测试 🚀 emoji")
  @azimuth.TextMapCarrier::set(carrier, "empty-value", "")
  @azimuth.TextMapCarrier::set(carrier, "very-long-value", "This is a very long header value that might be used in real-world scenarios for passing complex information through HTTP headers or other text-based carriers")
  
  // Test that setting doesn't throw exceptions
  assert_true(true)
}

test "baggage with many entries" {
  // Test baggage with many entries
  let baggage = @azimuth.Baggage::new()
  
  // Add many entries
  let baggage_with_entries = @azimuth.Baggage::set_entry(
    @azimuth.Baggage::set_entry(
      @azimuth.Baggage::set_entry(baggage, "key1", "value1"),
      "key2", "value2"
    ),
    "key3", "value3"
  )
  
  // Test that operations complete
  assert_true(true)
}

test "resource with many attributes" {
  // Test resource with many attributes
  let resource = @azimuth.Resource::new()
  
  // Create resource with many attributes
  let many_attributes = [
    ("attr1", @azimuth.StringValue("value1")),
    ("attr2", @azimuth.StringValue("value2")),
    ("attr3", @azimuth.StringValue("value3")),
    ("attr4", @azimuth.StringValue("value4")),
    ("attr5", @azimuth.StringValue("value5"))
  ]
  
  let resource_with_many = @azimuth.Resource::with_attributes(resource, many_attributes)
  
  // Test that creation succeeds
  assert_true(true)
}

test "meter with all instrument types" {
  // Test meter with all instrument types
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "all.instruments.test")
  
  // Create all instrument types
  let counter = @azimuth.Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = @azimuth.Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  // Test that all instruments are created successfully
  assert_eq(counter.name, "test.counter")
  assert_eq(histogram.name, "test.histogram")
}

test "propagation with multiple headers" {
  // Test propagation with multiple headers in carrier
  let carrier = @azimuth.TextMapCarrier::new()
  
  // Set multiple headers
  @azimuth.TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  @azimuth.TextMapCarrier::set(carrier, "baggage", "key1=value1,key2=value2")
  @azimuth.TextMapCarrier::set(carrier, "x-custom-header", "custom-value")
  @azimuth.TextMapCarrier::set(carrier, "another-header", "another-value")
  
  let trace_propagator = @azimuth.W3CTraceContextPropagator::new()
  let composite = @azimuth.CompositePropagator::new([trace_propagator])
  
  // Extract context
  let ctx = @azimuth.CompositePropagator::extract(composite, carrier)
  
  // Test that extraction doesn't fail
  assert_true(true)
}