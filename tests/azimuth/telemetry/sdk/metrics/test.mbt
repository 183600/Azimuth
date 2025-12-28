test "sdk metrics provider functionality" {
  // Test SDK metrics provider functionality
  
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "sdk.test.meter", Some("1.0.0"))
  
  // Test meter creation and properties
  assert_true(meter != telemetry.sdk.metrics.Meter::{})
  
  let scope = telemetry.sdk.metrics.Meter.instrumentation_scope(meter)
  assert_eq(scope.name(), "sdk.test.meter")
  assert_eq(scope.version(), Some("1.0.0"))
  
  // Test instrument creation with SDK meter
  let counter = telemetry.sdk.metrics.Meter.create_counter(
    meter,
    "sdk.test.counter",
    Some("SDK test counter"),
    Some("operations")
  )
  
  let histogram = telemetry.sdk.metrics.Meter.create_histogram(
    meter,
    "sdk.test.histogram",
    Some("SDK test histogram"),
    Some("ms")
  )
  
  // Test measurements
  telemetry.sdk.metrics.Counter.add(counter, 1.0)
  telemetry.sdk.metrics.Counter.add(counter, 2.5)
  
  telemetry.sdk.metrics.Histogram.record(histogram, 100.0)
  telemetry.sdk.metrics.Histogram.record(histogram, 200.5)
  
  // Test instrument properties
  assert_eq(telemetry.sdk.metrics.Instrument.name(counter), "sdk.test.counter")
  assert_eq(telemetry.sdk.metrics.Instrument.description(counter), Some("SDK test counter"))
  assert_eq(telemetry.sdk.metrics.Instrument.unit(counter), Some("operations"))
}

test "sdk metrics with attributes" {
  // Test SDK metrics with attributes and context
  
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "attributes.test.meter")
  
  let counter = telemetry.sdk.metrics.Meter.create_counter(meter, "attr.test.counter")
  let histogram = telemetry.sdk.metrics.Meter.create_histogram(meter, "attr.test.histogram")
  
  // Create attributes
  let attrs1 = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs1, "method", telemetry.api.common.StringValue("GET"))
  telemetry.api.common.Attributes.set(attrs1, "status", telemetry.api.common.StringValue("200"))
  
  let attrs2 = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(attrs2, "method", telemetry.api.common.StringValue("POST"))
  telemetry.api.common.Attributes.set(attrs2, "status", telemetry.api.common.StringValue("400"))
  
  // Record measurements with different attributes
  telemetry.sdk.metrics.Counter.add(counter, 10.0, Some(attrs1))
  telemetry.sdk.metrics.Counter.add(counter, 5.0, Some(attrs2))
  
  telemetry.sdk.metrics.Histogram.record(histogram, 150.0, Some(attrs1))
  telemetry.sdk.metrics.Histogram.record(histogram, 75.5, Some(attrs2))
  
  // Test with complex attributes
  let complex_attrs = telemetry.api.common.Attributes.new()
  telemetry.api.common.Attributes.set(complex_attrs, "user.id", telemetry.api.common.StringValue("12345"))
  telemetry.api.common.Attributes.set(complex_attrs, "request.size", telemetry.api.common.IntValue(1024))
  telemetry.api.common.Attributes.set(complex_attrs, "cache.hit", telemetry.api.common.BoolValue(true))
  telemetry.api.common.Attributes.set(complex_attrs, "servers", telemetry.api.common.ArrayStringValue(["server1", "server2"]))
  
  telemetry.sdk.metrics.Counter.add(counter, 1.0, Some(complex_attrs))
}

test "sdk metrics different instrument types" {
  // Test SDK metrics with all instrument types
  
  let metrics_provider = telemetry.sdk.metrics.MeterProvider.default()
  let meter = telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "instruments.test.meter")
  
  // Create all instrument types
  let counter = telemetry.sdk.metrics.Meter.create_counter(meter, "test.counter")
  let histogram = telemetry.sdk.metrics.Meter.create_histogram(meter, "test.histogram")
  let up_down_counter = telemetry.sdk.metrics.Meter.create_up_down_counter(meter, "test.up_down_counter")
  let gauge = telemetry.sdk.metrics.Meter.create_gauge(meter, "test.gauge")
  
  // Test counter operations
  telemetry.sdk.metrics.Counter.add(counter, 1.0)
  telemetry.sdk.metrics.Counter.add(counter, 2.0)
  telemetry.sdk.metrics.Counter.add(counter, 3.0)
  
  // Test histogram operations
  telemetry.sdk.metrics.Histogram.record(histogram, 10.0)
  telemetry.sdk.metrics.Histogram.record(histogram, 20.0)
  telemetry.sdk.metrics.Histogram.record(histogram, 30.0)
  
  // Test up down counter operations
  telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 10.0)
  telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, -5.0)
  telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, 3.0)
  
  // Test gauge operations
  telemetry.sdk.metrics.Gauge.record(gauge, 50.0)
  telemetry.sdk.metrics.Gauge.record(gauge, 75.0)
  telemetry.sdk.metrics.Gauge.record(gauge, 25.0)
  
  // Verify instrument properties
  assert_eq(telemetry.sdk.metrics.Instrument.name(counter), "test.counter")
  assert_eq(telemetry.sdk.metrics.Instrument.name(histogram), "test.histogram")
  assert_eq(telemetry.sdk.metrics.Instrument.name(up_down_counter), "test.up_down_counter")
  assert_eq(telemetry.sdk.metrics.Instrument.name(gauge), "test.gauge")
}