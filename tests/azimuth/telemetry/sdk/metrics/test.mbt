test "counter instrument creation and operations" {
  // Test counter instrument creation and basic operations
  
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "counter.test")
  
  // Create counter with all parameters
  let full_counter = @azimuth.telemetry.api.metrics.Meter.create_counter(
    meter,
    "operations.total",
    Some("Total number of operations performed"),
    Some("operations")
  )
  
  assert_eq(full_counter.name, "operations.total")
  assert_eq(full_counter.description, Some("Total number of operations performed"))
  assert_eq(full_counter.unit, Some("operations"))
  
  // Test counter operations
  @azimuth.telemetry.api.metrics.Counter.add(full_counter, 1.0, None)
  @azimuth.telemetry.api.metrics.Counter.add(full_counter, 5.0, None)
  @azimuth.telemetry.api.metrics.Counter.add(full_counter, 10.0, None)
  
  // Create counter with minimal parameters
  let minimal_counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "minimal.counter")
  
  assert_eq(minimal_counter.name, "minimal.counter")
  assert_eq(minimal_counter.description, None)
  assert_eq(minimal_counter.unit, None)
  
  // Test minimal counter operations
  @azimuth.telemetry.api.metrics.Counter.add(minimal_counter, 42.0, None)
}

test "histogram instrument functionality" {
  // Test histogram instrument creation and measurements
  
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "histogram.test")
  
  // Create histogram with all parameters
  let response_time_histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(
    meter,
    "http.request.duration",
    Some("HTTP request duration in milliseconds"),
    Some("ms")
  )
  
  assert_eq(response_time_histogram.name, "http.request.duration")
  assert_eq(response_time_histogram.description, Some("HTTP request duration in milliseconds"))
  assert_eq(response_time_histogram.unit, Some("ms"))
  
  // Record various measurements
  @azimuth.telemetry.api.metrics.Histogram.record(response_time_histogram, 50.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(response_time_histogram, 100.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(response_time_histogram, 200.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(response_time_histogram, 500.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(response_time_histogram, 1000.0, None)
  
  // Create histogram for different metrics
  let size_histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(
    meter,
    "message.size",
    Some("Message size in bytes"),
    Some("bytes")
  )
  
  // Record size measurements
  @azimuth.telemetry.api.metrics.Histogram.record(size_histogram, 1024.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(size_histogram, 2048.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(size_histogram, 4096.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(size_histogram, 8192.0, None)
}

test "instrument type identification and properties" {
  // Test instrument type identification and property access
  
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "instrument.test")
  
  // Create different instrument types
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(
    meter,
    "test.counter",
    Some("Test counter"),
    Some("count")
  )
  
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(
    meter,
    "test.histogram",
    Some("Test histogram"),
    Some("ms")
  )
  
  // Convert to instrument type and test properties
  let counter_instrument = @azimuth.telemetry.api.metrics.Histogram.as_instrument(histogram)
  
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.name(counter_instrument), "test.histogram")
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.description(counter_instrument), Some("Test histogram"))
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.unit(counter_instrument), Some("ms"))
  
  // Test with minimal instrument
  let minimal_histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "minimal.histogram")
  let minimal_instrument = @azimuth.telemetry.api.metrics.Histogram.as_instrument(minimal_histogram)
  
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.name(minimal_instrument), "minimal.histogram")
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.description(minimal_instrument), None)
  assert_eq(@azimuth.telemetry.api.metrics.Instrument.unit(minimal_instrument), None)
}

test "meter scope and instrumentation management" {
  // Test meter scope creation and instrumentation management
  
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  
  // Create meter with different scopes
  let service_meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "service.metrics")
  let component_meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "component.metrics")
  
  // Create instruments from different meters
  let service_counter = @azimuth.telemetry.api.metrics.Meter.create_counter(
    service_meter,
    "service.operations",
    Some("Service-level operation counter"),
    Some("operations")
  )
  
  let component_histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(
    component_meter,
    "component.latency",
    Some("Component latency histogram"),
    Some("ms")
  )
  
  // Verify instrument properties
  assert_eq(service_counter.name, "service.operations")
  assert_eq(service_counter.description, Some("Service-level operation counter"))
  assert_eq(service_counter.unit, Some("operations"))
  
  assert_eq(component_histogram.name, "component.latency")
  assert_eq(component_histogram.description, Some("Component latency histogram"))
  assert_eq(component_histogram.unit, Some("ms"))
  
  // Test instrument operations
  @azimuth.telemetry.api.metrics.Counter.add(service_counter, 10.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(component_histogram, 25.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(component_histogram, 50.0, None)
  @azimuth.telemetry.api.metrics.Histogram.record(component_histogram, 100.0, None)
}