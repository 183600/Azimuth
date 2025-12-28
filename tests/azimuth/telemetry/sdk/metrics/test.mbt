test "sdk metrics provider functionality" {
  // Test SDK metrics provider functionality and integration
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "sdk.test.meter", Some("1.0.0"))
  
  // Test meter creation and properties
  assert_true(meter != @azimuth.telemetry.sdk.metrics.Meter::{})
  
  let scope = @azimuth.telemetry.sdk.metrics.Meter.instrumentation_scope(meter)
  assert_eq(scope.name(), "sdk.test.meter")
  assert_eq(scope.version(), Some("1.0.0"))
  
  // Test counter instrument creation
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(
    meter,
    "sdk.test.counter",
    Some("SDK test counter"),
    Some("operations")
  )
  
  // Test counter operations
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0)
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 2.5)
  
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "method", @azimuth.telemetry.api.common.StringValue("GET"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "status", @azimuth.telemetry.api.common.StringValue("200"))
  
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 5.0, Some(attrs))
  
  // Test histogram instrument creation
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(
    meter,
    "sdk.test.histogram",
    Some("SDK test histogram"),
    Some("ms")
  )
  
  // Test histogram operations
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 100.0)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 200.5)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 150.75)
  
  let hist_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(hist_attrs, "endpoint", @azimuth.telemetry.api.common.StringValue("/api/users"))
  
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 75.25, Some(hist_attrs))
}

test "sdk metrics with different instrument types" {
  // Test SDK metrics with different instrument types
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "instruments.test.meter")
  
  // Test up-down counter
  let up_down_counter = @azimuth.telemetry.sdk.metrics.Meter.create_up_down_counter(
    meter,
    "sdk.test.up_down_counter",
    Some("SDK test up-down counter"),
    Some("items")
  )
  
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 10.0)
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, -3.5)
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 5.0)
  
  // Test gauge
  let gauge = @azimuth.telemetry.sdk.metrics.Meter.create_gauge(
    meter,
    "sdk.test.gauge",
    Some("SDK test gauge"),
    Some("percent")
  )
  
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 75.5)
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 80.0)
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 65.25)
  
  // Test instrument properties
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(counter), "sdk.test.counter")
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.description(counter), Some("SDK test counter"))
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.unit(counter), Some("operations"))
}

test "sdk metrics with context awareness" {
  // Test SDK metrics with context awareness
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "context.test.meter")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "context.test.histogram")
  
  // Create context with trace information
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    ""
  )
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  
  // Record measurement with context
  @azimuth.telemetry.sdk.metrics.Histogram.record_with_context(
    histogram,
    ctx_with_span,
    150.0,
    None
  )
  
  // Record measurement with context and attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "operation", @azimuth.telemetry.api.common.StringValue("database_query"))
  
  @azimuth.telemetry.sdk.metrics.Histogram.record_with_context(
    histogram,
    ctx_with_span,
    75.5,
    Some(attrs)
  )
}

test "sdk metrics batch operations" {
  // Test SDK metrics batch operations
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "batch.test.meter")
  
  // Create multiple instruments
  let counter1 = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "batch.counter1")
  let counter2 = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "batch.counter2")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "batch.histogram")
  
  // Batch measurements
  let attrs1 = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs1, "batch", @azimuth.telemetry.api.common.StringValue("1"))
  
  let attrs2 = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs2, "batch", @azimuth.telemetry.api.common.StringValue("2"))
  
  // Perform batch operations
  for i in 0..10 {
    @azimuth.telemetry.sdk.metrics.Counter.add(counter1, i.to_double(), Some(attrs1))
    @azimuth.telemetry.sdk.metrics.Counter.add(counter2, (i * 2).to_double(), Some(attrs2))
    @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, (i * 10).to_double(), None)
  }
}

test "sdk metrics edge cases" {
  // Test SDK metrics edge cases and error scenarios
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "edge.test.meter")
  
  // Test with zero and negative values
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "edge.test.counter")
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 0.0)
  
  let up_down_counter = @azimuth.telemetry.sdk.metrics.Meter.create_up_down_counter(meter, "edge.test.up_down_counter")
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, -10.0)
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 0.0)
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 5.0)
  
  // Test with very small and large values
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "edge.test.histogram")
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 0.001)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 999999.999)
  
  // Test gauge with negative values
  let gauge = @azimuth.telemetry.sdk.metrics.Meter.create_gauge(meter, "edge.test.gauge")
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, -25.5)
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 0.0)
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 100.0)
}