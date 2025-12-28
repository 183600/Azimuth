test "counter instrument operations" {
  // Test counter instrument creation and operations
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "test.meter", Some("1.0.0"))
  
  // Create counter
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(
    meter,
    "test.counter",
    Some("Test counter description"),
    Some("unit")
  )
  
  // Test counter operations
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0)
  @azimuth.telemetry.api.metrics.Counter.add(counter, 2.5)
  @azimuth.telemetry.api.metrics.Counter.add(counter, 3.0)
  
  // Test counter with attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "method", @azimuth.telemetry.api.common.StringValue("GET"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "status", @azimuth.telemetry.api.common.StringValue("200"))
  
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0, Some(attrs))
  
  // Test counter properties
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.name(counter), "test.counter")
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.description(counter), Some("Test counter description"))
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.unit(counter), Some("unit"))
}

test "histogram instrument operations" {
  // Test histogram instrument creation and operations
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "test.meter")
  
  // Create histogram
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(
    meter,
    "test.histogram",
    Some("Test histogram description"),
    Some("ms")
  )
  
  // Test histogram recordings
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 100.0)
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 200.5)
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 150.75)
  
  // Test histogram with attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "endpoint", @azimuth.telemetry.api.common.StringValue("/api/users"))
  
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 75.25, Some(attrs))
  
  // Test histogram properties
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.name(histogram), "test.histogram")
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.description(histogram), Some("Test histogram description"))
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.unit(histogram), Some("ms"))
}

test "up down counter instrument operations" {
  // Test up down counter instrument creation and operations
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "test.meter")
  
  // Create up down counter
  let up_down_counter = @azimuth.telemetry.api.metrics.Meter.create_up_down_counter(
    meter,
    "test.up_down_counter",
    Some("Test up down counter description"),
    Some("items")
  )
  
  // Test up down counter operations
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, 10.0)
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, -3.5)
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, 5.0)
  
  // Test up down counter with attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "region", @azimuth.telemetry.api.common.StringValue("us-west"))
  
  @azimuth.telemetry.api.metrics.UpDownCounter.add(up_down_counter, -2.0, Some(attrs))
  
  // Test up down counter properties
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.name(up_down_counter), "test.up_down_counter")
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.description(up_down_counter), Some("Test up down counter description"))
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.unit(up_down_counter), Some("items"))
}

test "gauge instrument operations" {
  // Test gauge instrument creation and operations
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "test.meter")
  
  // Create gauge
  let gauge = @azimuth.telemetry.api.metrics.Meter.create_gauge(
    meter,
    "test.gauge",
    Some("Test gauge description"),
    Some("percent")
  )
  
  // Test gauge recordings
  @azimuth.telemetry.api.metrics.Gauge.record(gauge, 75.5)
  @azimuth.telemetry.api.metrics.Gauge.record(gauge, 80.0)
  @azimuth.telemetry.api.metrics.Gauge.record(gauge, 65.25)
  
  // Test gauge with attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "instance", @azimuth.telemetry.api.common.StringValue("server-1"))
  
  @azimuth.telemetry.api.metrics.Gauge.record(gauge, 90.0, Some(attrs))
  
  // Test gauge properties
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.name(gauge), "test.gauge")
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.description(gauge), Some("Test gauge description"))
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.unit(gauge), Some("percent"))
}

test "meter provider and meter properties" {
  // Test meter provider and meter properties
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter1 = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "test.meter")
  let meter2 = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(
    meter_provider, 
    "test.meter", 
    Some("1.0.0"),
    Some("https://example.com/schema")
  )
  
  // Same name should return same meter instance (for noop provider)
  assert_true(meter1 == meter2)
  
  // Different names should return different meters
  let meter3 = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "other.meter")
  assert_true(meter1 != meter3)
  
  // Test meter instrumentation scope
  let scope = @azimuth.telemetry.api.metrics.Meter.instrumentation_scope(meter1)
  assert_eq(scope.name(), "test.meter")
}

test "instrument attributes and measurements" {
  // Test measuring with different attribute combinations
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "test.meter")
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "attr.test.counter")
  
  // Create different attribute sets
  let attrs1 = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs1, "method", @azimuth.telemetry.api.common.StringValue("GET"))
  @azimuth.telemetry.api.common.Attributes.set(attrs1, "status", @azimuth.telemetry.api.common.StringValue("200"))
  
  let attrs2 = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs2, "method", @azimuth.telemetry.api.common.StringValue("POST"))
  @azimuth.telemetry.api.common.Attributes.set(attrs2, "status", @azimuth.telemetry.api.common.StringValue("400"))
  
  let attrs3 = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs3, "method", @azimuth.telemetry.api.common.StringValue("GET"))
  @azimuth.telemetry.api.common.Attributes.set(attrs3, "status", @azimuth.telemetry.api.common.StringValue("404"))
  
  // Record measurements with different attributes
  @azimuth.telemetry.api.metrics.Counter.add(counter, 10.0, Some(attrs1))
  @azimuth.telemetry.api.metrics.Counter.add(counter, 5.0, Some(attrs2))
  @azimuth.telemetry.api.metrics.Counter.add(counter, 3.0, Some(attrs3))
  
  // Record measurement without attributes
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0, None)
}

test "measurement context awareness" {
  // Test that measurements can be context-aware
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "test.meter")
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "context.test.histogram")
  
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
  @azimuth.telemetry.api.metrics.Histogram.record_with_context(
    histogram,
    ctx_with_span,
    150.0,
    None
  )
  
  // Record measurement with context and attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "operation", @azimuth.telemetry.api.common.StringValue("database_query"))
  
  @azimuth.telemetry.api.metrics.Histogram.record_with_context(
    histogram,
    ctx_with_span,
    75.5,
    Some(attrs)
  )
}