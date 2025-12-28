test "sdk metrics provider functionality" {
  // Test SDK metrics provider functionality
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "sdk.test.meter", Some("1.0.0"))
  
  // Test meter creation and properties
  assert_true(meter != @azimuth.telemetry.sdk.metrics.Meter::{})
  
  let scope = @azimuth.telemetry.sdk.metrics.Meter.instrumentation_scope(meter)
  assert_eq(scope.name(), "sdk.test.meter")
  assert_eq(scope.version(), Some("1.0.0"))
  
  // Test instrument creation with SDK meter
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(
    meter,
    "sdk.test.counter",
    Some("SDK test counter"),
    Some("operations")
  )
  
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(
    meter,
    "sdk.test.histogram",
    Some("SDK test histogram"),
    Some("ms")
  )
  
  // Test measurements
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0)
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 2.5)
  
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 100.0)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 200.5)
  
  // Test instrument properties
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(counter), "sdk.test.counter")
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.description(counter), Some("SDK test counter"))
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.unit(counter), Some("operations"))
}

test "sdk metrics with attributes" {
  // Test SDK metrics with attributes and context
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "attributes.test.meter")
  
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "attr.test.counter")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "attr.test.histogram")
  
  // Create attributes
  let attrs1 = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs1, "method", @azimuth.telemetry.api.common.StringValue("GET"))
  @azimuth.telemetry.api.common.Attributes.set(attrs1, "status", @azimuth.telemetry.api.common.StringValue("200"))
  
  let attrs2 = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs2, "method", @azimuth.telemetry.api.common.StringValue("POST"))
  @azimuth.telemetry.api.common.Attributes.set(attrs2, "status", @azimuth.telemetry.api.common.StringValue("400"))
  
  // Record measurements with different attributes
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 10.0, Some(attrs1))
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 5.0, Some(attrs2))
  
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 150.0, Some(attrs1))
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 75.5, Some(attrs2))
  
  // Test with complex attributes
  let complex_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "user.id", @azimuth.telemetry.api.common.StringValue("12345"))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "request.size", @azimuth.telemetry.api.common.IntValue(1024))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "cache.hit", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.common.Attributes.set(complex_attrs, "servers", @azimuth.telemetry.api.common.ArrayStringValue(["server1", "server2"]))
  
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0, Some(complex_attrs))
}

test "sdk metrics different instrument types" {
  // Test SDK metrics with all instrument types
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "instruments.test.meter")
  
  // Create all instrument types
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "test.counter")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "test.histogram")
  let up_down_counter = @azimuth.telemetry.sdk.metrics.Meter.create_up_down_counter(meter, "test.up_down_counter")
  let gauge = @azimuth.telemetry.sdk.metrics.Meter.create_gauge(meter, "test.gauge")
  
  // Test counter operations
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0)
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 2.0)
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 3.0)
  
  // Test histogram operations
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 10.0)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 20.0)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 30.0)
  
  // Test up down counter operations
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 10.0)
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, -5.0)
  @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 3.0)
  
  // Test gauge operations
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 50.0)
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 75.0)
  @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, 25.0)
  
  // Verify instrument properties
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(counter), "test.counter")
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(histogram), "test.histogram")
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(up_down_counter), "test.up_down_counter")
  assert_eq(@azimuth.telemetry.sdk.metrics.Instrument.name(gauge), "test.gauge")
}