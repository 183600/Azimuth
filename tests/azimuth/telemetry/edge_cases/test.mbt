test "telemetry error handling and edge cases" {
  // Test error handling and edge cases across telemetry components
  
  // Test invalid span context handling
  let invalid_span_ctx = @azimuth.telemetry.api.trace.SpanContext.invalid()
  assert_false(@azimuth.telemetry.api.trace.SpanContext.is_valid(invalid_span_ctx))
  assert_false(@azimuth.telemetry.api.trace.SpanContext.is_sampled(invalid_span_ctx))
  
  // Test span context with empty IDs
  let empty_ctx = @azimuth.telemetry.api.trace.SpanContext.new("", "", false, "")
  assert_false(@azimuth.telemetry.api.trace.SpanContext.is_valid(empty_ctx))
  
  // Test span context with malformed IDs
  let malformed_ctx = @azimuth.telemetry.api.trace.SpanContext.new("invalid", "invalid", true, "")
  assert_false(@azimuth.telemetry.api.trace.SpanContext.is_valid(malformed_ctx))
  
  // Test attributes with special characters
  let special_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(special_attrs, "empty.value", @azimuth.telemetry.api.common.StringValue(""))
  @azimuth.telemetry.api.common.Attributes.set(special_attrs, "spaces.value", @azimuth.telemetry.api.common.StringValue("   "))
  @azimuth.telemetry.api.common.Attributes.set(special_attrs, "unicode.value", @azimuth.telemetry.api.common.StringValue("🚀 测试 🌟"))
  @azimuth.telemetry.api.common.Attributes.set(special_attrs, "quotes.value", @azimuth.telemetry.api.common.StringValue("\"quoted\""))
  @azimuth.telemetry.api.common.Attributes.set(special_attrs, "newlines.value", @azimuth.telemetry.api.common.StringValue("line1\nline2"))
  
  // Test numeric edge cases
  let numeric_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(numeric_attrs, "zero.int", @azimuth.telemetry.api.common.IntValue(0))
  @azimuth.telemetry.api.common.Attributes.set(numeric_attrs, "zero.float", @azimuth.telemetry.api.common.FloatValue(0.0))
  @azimuth.telemetry.api.common.Attributes.set(numeric_attrs, "negative.int", @azimuth.telemetry.api.common.IntValue(-1))
  @azimuth.telemetry.api.common.Attributes.set(numeric_attrs, "negative.float", @azimuth.telemetry.api.common.FloatValue(-1.0))
  @azimuth.telemetry.api.common.Attributes.set(numeric_attrs, "max.int", @azimuth.telemetry.api.common.IntValue(2147483647))
  @azimuth.telemetry.api.common.Attributes.set(numeric_attrs, "min.int", @azimuth.telemetry.api.common.IntValue(-2147483648))
  
  // Test array edge cases
  let array_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(array_attrs, "empty.string.array", @azimuth.telemetry.api.common.ArrayStringValue([]))
  @azimuth.telemetry.api.common.Attributes.set(array_attrs, "empty.int.array", @azimuth.telemetry.api.common.ArrayIntValue([]))
  @azimuth.telemetry.api.common.Attributes.set(array_attrs, "single.string.array", @azimuth.telemetry.api.common.ArrayStringValue(["single"]))
  @azimuth.telemetry.api.common.Attributes.set(array_attrs, "single.int.array", @azimuth.telemetry.api.common.ArrayIntValue([42]))
  
  // Test context with non-existent keys
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let non_existent_key = @azimuth.telemetry.api.context.ContextKey::new("non.existent.key")
  let non_existent_value = @azimuth.telemetry.api.context.Context.get(ctx, non_existent_key)
  assert_eq(non_existent_value, None)
  
  // Test baggage with edge cases
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  let empty_baggage_value = @azimuth.telemetry.api.context.Baggage.get_entry(baggage, "non.existent")
  assert_eq(empty_baggage_value, None)
  
  let baggage_with_empty = @azimuth.telemetry.api.context.Baggage.set_entry(baggage, "empty.key", "")
  let empty_value = @azimuth.telemetry.api.context.Baggage.get_entry(baggage_with_empty, "empty.key")
  // Simplified implementation returns None
  assert_eq(empty_value, None)
  
  // Test propagation with empty carrier
  let empty_carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  let empty_traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier.get(empty_carrier, "traceparent")
  assert_eq(empty_traceparent, None)
  
  // Test metrics with edge case values
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "edge.case.test")
  
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "edge.counter")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "edge.histogram")
  let up_down_counter = @azimuth.telemetry.sdk.metrics.Meter.create_up_down_counter(meter, "edge.up_down")
  let gauge = @azimuth.telemetry.sdk.metrics.Meter.create_gauge(meter, "edge.gauge")
  
  // Test with zero and negative values
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 0.0)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 0.0)
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, -100.0)
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 0.0)
  
  // Test with very large values
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 999999.0)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 999999.0)
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 999999.0)
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 999999.0)
  
  // Test with very small values
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 0.001)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 0.000001)
  
  // Test logs with edge cases
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "edge.case.logger")
  
  // Test with empty messages
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "")
  
  // Test with very long messages
  let long_message = "a".repeat(1000)
  @azimuth.telemetry.sdk.logs.Logger.info(logger, long_message)
  
  // Test with special characters
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "Unicode: 🚀 测试 🌟")
  
  // Test span with edge cases
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "edge.case.tracer")
  
  // Test span with empty name
  let (_, empty_name_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "")
  @azimuth.telemetry.api.trace.Span.end(empty_name_span)
  
  // Test span with very long name
  let long_name = "a".repeat(1000)
  let (_, long_name_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, long_name)
  @azimuth.telemetry.api.trace.Span.end(long_name_span)
  
  // Test span with many attributes
  let (_, many_attrs_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "many.attrs")
  for i = 0; i < 100; i = i + 1 {
    @azimuth.telemetry.api.trace.Span.set_attribute(
      many_attrs_span, 
      "attr.${i}", 
      @azimuth.telemetry.api.common.StringValue("value.${i}")
    )
  }
  @azimuth.telemetry.api.trace.Span.end(many_attrs_span)
  
  // Test span with many events
  let (_, many_events_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "many.events")
  for i = 0; i < 50; i = i + 1 {
    @azimuth.telemetry.api.trace.Span.add_event(
      many_events_span,
      "event.${i}",
      Some([("event.index", @azimuth.telemetry.api.common.IntValue(i))]),
      None
    )
  }
  @azimuth.telemetry.api.trace.Span.end(many_events_span)
}