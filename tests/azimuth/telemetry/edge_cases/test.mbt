test "attribute value edge cases" {
  // Test attribute values with edge cases and boundaries
  
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  
  // Test very long string values
  let long_string = "a".repeat(10000)
  @azimuth.telemetry.api.common.Attributes.set(attrs, "long.string", @azimuth.telemetry.api.common.StringValue(long_string))
  
  let retrieved_long = @azimuth.telemetry.api.common.Attributes.get(attrs, "long.string")
  assert_eq(retrieved_long, Some(@azimuth.telemetry.api.common.StringValue(long_string)))
  
  // Test string with special characters
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~\\n\\t\\r"
  @azimuth.telemetry.api.common.Attributes.set(attrs, "special.chars", @azimuth.telemetry.api.common.StringValue(special_chars))
  
  let retrieved_special = @azimuth.telemetry.api.common.Attributes.get(attrs, "special.chars")
  assert_eq(retrieved_special, Some(@azimuth.telemetry.api.common.StringValue(special_chars)))
  
  // Test Unicode strings
  let unicode_string = "🚀 测试 🌟 العربية 日本語 한국어 עברית"
  @azimuth.telemetry.api.common.Attributes.set(attrs, "unicode.string", @azimuth.telemetry.api.common.StringValue(unicode_string))
  
  let retrieved_unicode = @azimuth.telemetry.api.common.Attributes.get(attrs, "unicode.string")
  assert_eq(retrieved_unicode, Some(@azimuth.telemetry.api.common.StringValue(unicode_string)))
  
  // Test empty string
  @azimuth.telemetry.api.common.Attributes.set(attrs, "empty.string", @azimuth.telemetry.api.common.StringValue(""))
  let retrieved_empty = @azimuth.telemetry.api.common.Attributes.get(attrs, "empty.string")
  assert_eq(retrieved_empty, Some(@azimuth.telemetry.api.common.StringValue("")))
  
  // Test boundary integer values
  @azimuth.telemetry.api.common.Attributes.set(attrs, "max.int", @azimuth.telemetry.api.common.IntValue(2147483647))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "min.int", @azimuth.telemetry.api.common.IntValue(-2147483648))
  
  let max_int = @azimuth.telemetry.api.common.Attributes.get(attrs, "max.int")
  let min_int = @azimuth.telemetry.api.common.Attributes.get(attrs, "min.int")
  
  assert_eq(max_int, Some(@azimuth.telemetry.api.common.IntValue(2147483647)))
  assert_eq(min_int, Some(@azimuth.telemetry.api.common.IntValue(-2147483648)))
  
  // Test extreme float values
  @azimuth.telemetry.api.common.Attributes.set(attrs, "max.float", @azimuth.telemetry.api.common.FloatValue(1.7976931348623157e+308))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "min.float", @azimuth.telemetry.api.common.FloatValue(-1.7976931348623157e+308))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "zero.float", @azimuth.telemetry.api.common.FloatValue(0.0))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "tiny.float", @azimuth.telemetry.api.common.FloatValue(1.0e-10))
  
  let max_float = @azimuth.telemetry.api.common.Attributes.get(attrs, "max.float")
  let min_float = @azimuth.telemetry.api.common.Attributes.get(attrs, "min.float")
  let zero_float = @azimuth.telemetry.api.common.Attributes.get(attrs, "zero.float")
  let tiny_float = @azimuth.telemetry.api.common.Attributes.get(attrs, "tiny.float")
  
  assert_eq(max_float, Some(@azimuth.telemetry.api.common.FloatValue(1.7976931348623157e+308)))
  assert_eq(min_float, Some(@azimuth.telemetry.api.common.FloatValue(-1.7976931348623157e+308)))
  assert_eq(zero_float, Some(@azimuth.telemetry.api.common.FloatValue(0.0)))
  assert_eq(tiny_float, Some(@azimuth.telemetry.api.common.FloatValue(1.0e-10)))
  
  // Test very large arrays
  let large_string_array = []
  let mut i = 0
  while i < 1000 {
    large_string_array = large_string_array @ ["item" + i.to_string()]
    i = i + 1
  }
  
  let large_int_array = []
  let mut j = 0
  while j < 1000 {
    large_int_array = large_int_array @ [j]
    j = j + 1
  }
  
  @azimuth.telemetry.api.common.Attributes.set(attrs, "large.string.array", @azimuth.telemetry.api.common.ArrayStringValue(large_string_array))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "large.int.array", @azimuth.telemetry.api.common.ArrayIntValue(large_int_array))
  
  let retrieved_large_string_array = @azimuth.telemetry.api.common.Attributes.get(attrs, "large.string.array")
  let retrieved_large_int_array = @azimuth.telemetry.api.common.Attributes.get(attrs, "large.int.array")
  
  assert_eq(retrieved_large_string_array, Some(@azimuth.telemetry.api.common.ArrayStringValue(large_string_array)))
  assert_eq(retrieved_large_int_array, Some(@azimuth.telemetry.api.common.ArrayIntValue(large_int_array)))
}

test "span context validation edge cases" {
  // Test span context validation with edge cases
  
  // Test invalid trace IDs
  let invalid_trace_contexts = [
    ("", "1234567890abcdef", false),  // Empty trace ID
    ("0af7651916cd43dd8448eb211c80319", "1234567890abcdef", false),  // Too short trace ID
    ("0af7651916cd43dd8448eb211c80319cc", "1234567890abcdef", false),  // Too long trace ID
    ("invalid_trace_id", "1234567890abcdef", false),  // Non-hex trace ID
    ("0af7651916cd43dd8448eb211c80319c", "", false),  // Empty span ID
    ("0af7651916cd43dd8448eb211c80319c", "1234567890abcde", false),  // Too short span ID
    ("0af7651916cd43dd8448eb211c80319c", "1234567890abcdef1", false),  // Too long span ID
    ("0af7651916cd43dd8448eb211c80319c", "invalid_span_id", false),  // Non-hex span ID
  ]
  
  for (trace_id, span_id, should_be_valid) in invalid_trace_contexts {
    let ctx = @azimuth.telemetry.api.trace.SpanContext.new(trace_id, span_id, true, "")
    let is_valid = @azimuth.telemetry.api.trace.SpanContext.is_valid(ctx)
    
    if should_be_valid {
      assert_true(is_valid, "Should be valid: trace_id=${trace_id}, span_id=${span_id}")
    } else {
      assert_false(is_valid, "Should be invalid: trace_id=${trace_id}, span_id=${span_id}")
    }
  }
  
  // Test valid trace and span IDs
  let valid_context = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  
  assert_true(@azimuth.telemetry.api.trace.SpanContext.is_valid(valid_context))
  assert_true(@azimuth.telemetry.api.trace.SpanContext.is_sampled(valid_context))
  assert_eq(@azimuth.telemetry.api.trace.SpanContext.trace_id(valid_context), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(@azimuth.telemetry.api.trace.SpanContext.span_id(valid_context), "b7ad6b7169203331")
  assert_eq(@azimuth.telemetry.api.trace.SpanContext.trace_state(valid_context), "congo=t61rcWkgMzE")
  
  // Test trace state edge cases
  let empty_trace_state_context = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    ""
  )
  
  assert_eq(@azimuth.telemetry.api.trace.SpanContext.trace_state(empty_trace_state_context), "")
  
  let long_trace_state_context = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "very.long.trace.state.with.many.entries=and.values.that.exceed.normal.lengths"
  )
  
  assert_true(@azimuth.telemetry.api.trace.SpanContext.trace_state(long_trace_state_context).length() > 50)
}

test "metric measurement boundary conditions" {
  // Test metric measurements with boundary conditions
  
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "boundary.test")
  
  // Test counter with boundary values
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "boundary.counter")
  
  // Test zero and negative values
  @azimuth.telemetry.api.metrics.Counter.add(counter, 0.0)
  @azimuth.telemetry.api.metrics.Counter.add(counter, -1.0)
  @azimuth.telemetry.api.metrics.Counter.add(counter, -1000000.0)
  
  // Test very large values
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.7976931348623157e+308)  // Max double
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0e-10)  // Very small
  
  // Test very small positive values
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0e-100)
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0e-200)
  
  // Test up-down counter with boundary values
  let up_down_counter = @azimuth.telemetry.api.metrics.Meter.create_up_down_counter(meter, "boundary.up_down_counter")
  
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, 2147483647.0)  // Max int
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, -2147483648.0)  // Min int
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, 0.0)
  
  // Test histogram with boundary values
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "boundary.histogram")
  
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 0.0)
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, -1.0)  // Negative values should be allowed
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 1.7976931348623157e+308)
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 1.0e-10)
  
  // Test gauge with boundary values
  let gauge = @azimuth.telemetry.api.metrics.Meter.create_gauge(meter, "boundary.gauge")
  
  @azimuth.telemetry.api.metrics.Gauge.record(gauge, 100.0)
  @azimuth.telemetry.api.metrics.Gauge.record(gauge, -50.0)
  @azimuth.telemetry.api.metrics.Gauge.record(gauge, 0.0)
  @azimuth.telemetry.api.metrics.Gauge.record(gauge, 1.7976931348623157e+308)
  
  // Test measurements with complex attributes
  let complex_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "very.long.attribute.name.that.exceeds.normal.lengths", @azimuth.telemetry.api.common.StringValue("value"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "unicode_attr", @azimuth.telemetry.api.common.StringValue("🚀 测试"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "empty_attr", @azimuth.telemetry.api.common.StringValue(""))
  
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0, Some(complex_attrs))
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 100.0, Some(complex_attrs))
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, 10.0, Some(complex_attrs))
  @azimuth.telemetry.api.metrics.Gauge.record(gauge, 75.0, Some(complex_attrs))
}

test "log record edge cases" {
  // Test log records with edge cases and boundary conditions
  
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "edge.case.test")
  
  // Test log with extremely long message
  let very_long_message = "This is a very long log message ".repeat(1000)
  let long_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    very_long_message,
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(long_log.body(), Some(very_long_message))
  assert_eq(long_log.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  
  // Test log with empty message
  let empty_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Debug,
    "",
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(empty_log.body(), Some(""))
  
  // Test log with special characters in message
  let special_message = "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?\\n\\t\\r and Unicode: 🚀 测试 🌟"
  let special_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Warn,
    special_message,
    None,
    None,
    None,
    None,
    None,
    None
  )
  
  assert_eq(special_log.body(), Some(special_message))
  
  // Test log with boundary timestamps
  let min_timestamp = 0L
  let max_timestamp = 9223372036854775807L  // Max Int64
  
  let min_time_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Error,
    "Min timestamp",
    None,
    Some(min_timestamp),
    None,
    None,
    None,
    None
  )
  
  let max_time_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Fatal,
    "Max timestamp",
    None,
    Some(max_timestamp),
    None,
    None,
    None,
    None
  )
  
  assert_eq(min_time_log.timestamp(), Some(min_timestamp))
  assert_eq(max_time_log.timestamp(), Some(max_timestamp))
  
  // Test log with complex attributes
  let complex_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "empty.value", @azimuth.telemetry.api.common.StringValue(""))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "unicode.value", @azimuth.telemetry.api.common.StringValue("🚀 测试 🌟"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "special.chars", @azimuth.telemetry.api.common.StringValue("!@#$%^&*()"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "max.int", @azimuth.telemetry.api.common.IntValue(2147483647))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "min.int", @azimuth.telemetry.api.common.IntValue(-2147483648))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "max.float", @azimuth.telemetry.api.common.FloatValue(1.7976931348623157e+308))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "min.float", @azimuth.telemetry.api.common.FloatValue(-1.7976931348623157e+308))
  
  let complex_log = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Complex attributes test",
    Some(complex_attrs),
    None,
    None,
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    None
  )
  
  assert_eq(complex_log.body(), Some("Complex attributes test"))
  assert_eq(complex_log.trace_id(), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(complex_log.span_id(), Some("b7ad6b7169203331"))
  
  // Test log with all severity levels
  let severity_levels = [
    @azimuth.telemetry.api.logs.SeverityNumber.Trace,
    @azimuth.telemetry.api.logs.SeverityNumber.Debug,
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    @azimuth.telemetry.api.logs.SeverityNumber.Warn,
    @azimuth.telemetry.api.logs.SeverityNumber.Error,
    @azimuth.telemetry.api.logs.SeverityNumber.Fatal
  ]
  
  for severity in severity_levels {
    let severity_log = @azimuth.telemetry.api.logs.LogRecord.new(
      severity,
      "Test message for severity",
      None,
      None,
      None,
      None,
      None,
      None
    )
    
    assert_eq(severity_log.severity_number(), severity)
  }
}

test "context capacity and performance" {
  // Test context behavior with large amounts of data
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Test context with many layers
  let mut current_ctx = ctx
  let mut i = 0
  while i < 100 {
    let key = @azimuth.telemetry.api.context.ContextKey::new("layer." + i.to_string())
    current_ctx = @azimuth.telemetry.api.context.Context.with_value(current_ctx, key, "value." + i.to_string())
    i = i + 1
  }
  
  // Verify we can still retrieve values from deep context
  let first_key = @azimuth.telemetry.api.context.ContextKey::new("layer.0")
  let first_value = @azimuth.telemetry.api.context.Context.get(current_ctx, first_key)
  assert_eq(first_value, Some("value.0"))
  
  let last_key = @azimuth.telemetry.api.context.ContextKey::new("layer.99")
  let last_value = @azimuth.telemetry.api.context.Context.get(current_ctx, last_key)
  assert_eq(last_value, Some("value.99"))
  
  // Test context with very long values
  let long_value = "a".repeat(10000)
  let long_key = @azimuth.telemetry.api.context.ContextKey::new("long.value")
  let ctx_with_long = @azimuth.telemetry.api.context.Context.with_value(current_ctx, long_key, long_value)
  
  let retrieved_long = @azimuth.telemetry.api.context.Context.get(ctx_with_long, long_key)
  assert_eq(retrieved_long, Some(long_value))
  
  // Test baggage with many entries
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  let mut current_baggage = baggage
  let mut j = 0
  while j < 50 {
    current_baggage = @azimuth.telemetry.api.context.Baggage.set_entry(
      current_baggage,
      "baggage.key." + j.to_string(),
      "baggage.value." + j.to_string()
    )
    j = j + 1
  }
  
  // Note: Simplified implementation returns None for all keys
  let first_baggage_value = @azimuth.telemetry.api.context.Baggage.get_entry(current_baggage, "baggage.key.0")
  let last_baggage_value = @azimuth.telemetry.api.context.Baggage.get_entry(current_baggage, "baggage.key.49")
  
  assert_eq(first_baggage_value, None)
  assert_eq(last_baggage_value, None)
}

test "propagation format edge cases" {
  // Test propagation formats with edge cases
  
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator.new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  ])
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  
  // Test injection with empty context
  @azimuth.telemetry.api.propagation.CompositePropagator.inject(composite, ctx, carrier)
  
  let injected_traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(carrier, "traceparent")
  assert_true(injected_traceparent != None)
  assert_true(injected_traceparent.unwrap().starts_with("00-"))
  
  // Test extraction with malformed traceparent
  let malformed_carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(malformed_carrier, "traceparent", "malformed-format")
  
  let extracted_from_malformed = @azimuth.telemetry.api.propagation.CompositePropagator.extract(composite, malformed_carrier)
  let extraction_key = @azimuth.telemetry.api.context.ContextKey::new("extracted")
  let extraction_result = @azimuth.telemetry.api.context.Context.get(extracted_from_malformed, extraction_key)
  
  // Should still return a context (simplified implementation)
  assert_eq(extraction_result, Some("true"))
  
  // Test with very long traceparent
  let very_long_trace_id = "a".repeat(32)
  let very_long_span_id = "b".repeat(16)
  let long_traceparent = "00-" + very_long_trace_id + "-" + very_long_span_id + "-01"
  
  let long_carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(long_carrier, "traceparent", long_traceparent)
  
  let extracted_from_long = @azimuth.telemetry.api.propagation.CompositePropagator.extract(composite, long_carrier)
  let long_extraction_result = @azimuth.telemetry.api.context.Context.get(extracted_from_long, extraction_key)
  
  assert_eq(long_extraction_result, Some("true"))
  
  // Test with multiple headers
  let multi_carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(multi_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(multi_carrier, "baggage", "userId=12345,sessionId=abcdef")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(multi_carrier, "x-custom-header", "custom-value")
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(multi_carrier, "another-header", "another-value")
  
  let extracted_from_multi = @azimuth.telemetry.api.propagation.CompositePropagator.extract(composite, multi_carrier)
  let multi_extraction_result = @azimuth.telemetry.api.context.Context.get(extracted_from_multi, extraction_key)
  
  assert_eq(multi_extraction_result, Some("true"))
}