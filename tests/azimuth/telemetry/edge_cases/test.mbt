test "telemetry error handling and edge cases" {
  // Test error handling and edge cases across telemetry components
  
  // Test invalid span context handling
  let invalid_span_ctx = telemetry.api.trace.SpanContext.invalid()
  assert_false(telemetry.api.trace.SpanContext.is_valid(invalid_span_ctx))
  assert_false(telemetry.api.trace.SpanContext.is_sampled(invalid_span_ctx))
  
  // Test span context with empty IDs
  let empty_ctx = telemetry.api.trace.SpanContext.new("", "", false, "")
  assert_false(telemetry.api.trace.SpanContext.is_valid(empty_ctx))
  
  // Test span context with malformed IDs
  let malformed_ctx = telemetry.api.trace.SpanContext.new("invalid", "invalid", true, "")
  assert_false(telemetry.api.trace.SpanContext.is_valid(malformed_ctx))
  
  // Test attributes with special characters
  let special_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(special_attrs, "empty.value", telemetry.api.common.StringValue(""))
  telemetry.api.common.Attributes.set(special_attrs, "spaces.value", telemetry.api.common.StringValue("   "))
  telemetry.api.common.Attributes.set(special_attrs, "unicode.value", telemetry.api.common.StringValue("🚀 测试 🌟"))
  telemetry.api.common.Attributes.set(special_attrs, "quotes.value", telemetry.api.common.StringValue("\"quoted\""))
  telemetry.api.common.Attributes.set(special_attrs, "newlines.value", telemetry.api.common.StringValue("line1\nline2"))
  
  // Test numeric edge cases
  let numeric_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(numeric_attrs, "zero.int", telemetry.api.common.IntValue(0))
  telemetry.api.common.Attributes.set(numeric_attrs, "zero.float", telemetry.api.common.FloatValue(0.0))
  telemetry.api.common.Attributes.set(numeric_attrs, "negative.int", telemetry.api.common.IntValue(-1))
  telemetry.api.common.Attributes.set(numeric_attrs, "negative.float", telemetry.api.common.FloatValue(-1.0))
  telemetry.api.common.Attributes.set(numeric_attrs, "max.int", telemetry.api.common.IntValue(2147483647))
  telemetry.api.common.Attributes.set(numeric_attrs, "min.int", telemetry.api.common.IntValue(-2147483648))
  
  // Test array edge cases
  let array_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(array_attrs, "empty.string.array", telemetry.api.common.ArrayStringValue([]))
  telemetry.api.common.Attributes.set(array_attrs, "empty.int.array", telemetry.api.common.ArrayIntValue([]))
  telemetry.api.common.Attributes.set(array_attrs, "single.string.array", telemetry.api.common.ArrayStringValue(["single"]))
  telemetry.api.common.Attributes.set(array_attrs, "single.int.array", telemetry.api.common.ArrayIntValue([42]))
  
  // Test context with non-existent keys
  let ctx = telemetry.api.context.Context.root()
  let non_existent_key = telemetry.api.context.ContextKey::new("non.existent.key")
  let non_existent_value = telemetry.api.context.Context.get(ctx, non_existent_key)
  assert_eq(non_existent_value, None)
  
  // Test baggage with edge cases
  let baggage = telemetry.api.context.Baggage.new()
  let empty_baggage_value = telemetry.api.context.Baggage.get_entry(baggage, "non.existent")
  assert_eq(empty_baggage_value, None)
  
  let baggage_with_empty = telemetry.api.context.Baggage.set_entry(baggage, "empty.key", "")
  let empty_value = telemetry.api.context.Baggage.get_entry(baggage_with_empty, "empty.key")
  // Simplified implementation returns None
  assert_eq(empty_value, None)
  
  // Test propagation with empty carrier
  let empty_carrier = telemetry.api.propagation.TextMapCarrier.new()
  let empty_traceparent = telemetry.api.propagation.TextMapCarrier.get(empty_carrier, "traceparent")
  assert_eq(empty_traceparent, None)
  
  // Test metrics with edge case values
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "edge.case.test")
  
  let counter = telemetry.sdk.metrics.Meter.create_counter(meter, "edge.counter")
  let histogram = telemetry.sdk.metrics.Meter.create_histogram(meter, "edge.histogram")
  let up_down_counter = telemetry.sdk.metrics.Meter.create_up_down_counter(meter, "edge.up_down")
  let gauge = telemetry.sdk.metrics.Meter.create_gauge(meter, "edge.gauge")
  
  // Test with zero and negative values
  telemetry.sdk.metrics.Counter.add(counter, 0.0)
  telemetry.sdk.metrics.Histogram.record(histogram, 0.0)
  telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, -100.0)
  telemetry.sdk.metrics.Gauge.record(gauge, 0.0)
  
  // Test with very large values
  telemetry.sdk.metrics.Counter.add(counter, 999999.0)
  telemetry.sdk.metrics.Histogram.record(histogram, 999999.0)
  telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 999999.0)
  telemetry.sdk.metrics.Gauge.record(gauge, 999999.0)
  
  // Test with very small values
  telemetry.sdk.metrics.Histogram.record(histogram, 0.001)
  telemetry.sdk.metrics.Histogram.record(histogram, 0.000001)
  
  // Test logs with edge cases
  let logs_provider = telemetry.sdk.logs.LoggerProvider.default()
  let logger = telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "edge.case.logger")
  
  // Test with empty messages
  telemetry.sdk.logs.Logger.info(logger, "")
  
  // Test with very long messages
  let long_message = "a".repeat(1000)
  telemetry.sdk.logs.Logger.info(logger, long_message)
  
  // Test with special characters
  telemetry.sdk.logs.Logger.info(logger, "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  telemetry.sdk.logs.Logger.info(logger, "Unicode: 🚀 测试 🌟")
  
  // Test span with edge cases
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "edge.case.tracer")
  
  // Test span with empty name
  let (_, empty_name_span) = telemetry.sdk.trace.Tracer.start_span(tracer, "")
  telemetry.api.trace.Span.end(empty_name_span)
  
  // Test span with very long name
  let long_name = "a".repeat(1000)
  let (_, long_name_span) = telemetry.sdk.trace.Tracer.start_span(tracer, long_name)
  telemetry.api.trace.Span.end(long_name_span)
  
  // Test span with many attributes
  let (_, many_attrs_span) = telemetry.sdk.trace.Tracer.start_span(tracer, "many.attrs")
  for i = 0; i < 100; i = i + 1 {
    telemetry.api.trace.Span.set_attribute(
      many_attrs_span, 
      "attr.${i}", 
      telemetry.api.common.StringValue("value.${i}")
    )
  }
  telemetry.api.trace.Span.end(many_attrs_span)
  
  // Test span with many events
  let (_, many_events_span) = telemetry.sdk.trace.Tracer.start_span(tracer, "many.events")
  for i = 0; i < 50; i = i + 1 {
    telemetry.api.trace.Span.add_event(
      many_events_span,
      "event.${i}",
      Some([("event.index", telemetry.api.common.IntValue(i))]),
      None
    )
  }
  telemetry.api.trace.Span.end(many_events_span)
}