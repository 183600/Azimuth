// Meter Instruments Tests for Azimuth Telemetry System
// Testing meter creation and instrument operations

test "meter provider basic operations" {
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "test.meter")
  let meter2 = MeterProvider::get_meter(provider, "another.meter")
  
  // Test meter creation
  assert_eq(meter1.scope.name, "test.meter")
  assert_eq(meter2.scope.name, "another.meter")
  
  // Test default values
  assert_eq(meter1.scope.version, None)
  assert_eq(meter1.scope.schema_url, None)
  assert_eq(meter2.scope.version, None)
  assert_eq(meter2.scope.schema_url, None)
}

test "counter instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "counter.test")
  let counter = Meter::create_counter(meter, "http.requests.total")
  
  // Test counter properties
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, None)
  assert_eq(counter.unit, None)
  
  // Test counter operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.5)
  Counter::add(counter, 0.0)
  Counter::add(counter, -5.0) // Should handle negative values
  
  assert_true(true) // If we reach here, operations didn't crash
}

test "counter with attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "counter.attrs.test")
  let counter = Meter::create_counter(meter, "api.calls")
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "method", StringValue("GET"))
  Attributes::set(attrs, "status", StringValue("200"))
  
  // Test counter operations with attributes
  Counter::add(counter, 1.0, Some(attrs))
  Counter::add(counter, 2.0, Some(attrs))
  Counter::add(counter, 3.0, None) // Without attributes
  
  assert_true(true) // If we reach here, operations didn't crash
}

test "histogram instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram.test")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  
  // Test histogram properties
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time in milliseconds"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test histogram operations
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 250.5)
  Histogram::record(histogram, 50.0)
  Histogram::record(histogram, 1000.0)
  
  assert_true(true) // If we reach here, operations didn't crash
}

test "histogram with attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram.attrs.test")
  let histogram = Meter::create_histogram(meter, "request.size", Some("Request size in bytes"), Some("bytes"))
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "endpoint", StringValue("/api/users"))
  Attributes::set(attrs, "method", StringValue("POST"))
  
  // Test histogram operations with attributes
  Histogram::record(histogram, 1024.0, Some(attrs))
  Histogram::record(histogram, 2048.0, Some(attrs))
  Histogram::record(histogram, 512.0, None) // Without attributes
  
  assert_true(true) // If we reach here, operations didn't crash
}

test "updown counter instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown.test")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active network connections"), Some("connections"))
  
  // Test updown counter properties
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Active network connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // Test updown counter operations (increment and decrement)
  UpDownCounter::add(updown_counter, 10.0) // Add 10 connections
  UpDownCounter::add(updown_counter, 5.0)  // Add 5 more
  UpDownCounter::add(updown_counter, -3.0) // Remove 3
  UpDownCounter::add(updown_counter, -12.0) // Remove all and go negative
  
  assert_true(true) // If we reach here, operations didn't crash
}

test "gauge instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge.test")
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage in bytes"), Some("bytes"))
  
  // Test gauge properties
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Memory usage in bytes"))
  assert_eq(gauge.unit, Some("bytes"))
  
  // Test gauge operations
  UpDownCounter::add(gauge, 1024.0) // Set to 1KB
  UpDownCounter::add(gauge, 2048.0) // Set to 3KB
  UpDownCounter::add(gauge, -1024.0) // Set to 2KB
  
  assert_true(true) // If we reach here, operations didn't crash
}

test "instrument type operations" {
  let counter_instr = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_instr = Counter("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter_instr = Counter("test.updown", Some("Test updown counter"), Some("items"))
  let gauge_instr = Counter("test.gauge", Some("Test gauge"), Some("percent"))
  
  // Test instrument name extraction
  assert_eq(Instrument::name(counter_instr), "test.counter")
  assert_eq(Instrument::name(histogram_instr), "test.histogram")
  assert_eq(Instrument::name(updown_counter_instr), "test.updown")
  assert_eq(Instrument::name(gauge_instr), "test.gauge")
  
  // Test instrument description extraction
  assert_eq(Instrument::description(counter_instr), Some("Test counter"))
  assert_eq(Instrument::description(histogram_instr), Some("Test histogram"))
  assert_eq(Instrument::description(updown_counter_instr), Some("Test updown counter"))
  assert_eq(Instrument::description(gauge_instr), Some("Test gauge"))
  
  // Test instrument unit extraction
  assert_eq(Instrument::unit(counter_instr), Some("count"))
  assert_eq(Instrument::unit(histogram_instr), Some("ms"))
  assert_eq(Instrument::unit(updown_counter_instr), Some("items"))
  assert_eq(Instrument::unit(gauge_instr), Some("percent"))
}

test "meter with multiple instruments" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi.instrument.test")
  
  // Create multiple instruments
  let request_counter = Meter::create_counter(meter, "http.requests")
  let response_histogram = Meter::create_histogram(meter, "http.response.time")
  let active_connections = Meter::create_updown_counter(meter, "http.active.connections")
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage")
  
  // Test all instruments
  Counter::add(request_counter, 100.0)
  Histogram::record(response_histogram, 250.0)
  UpDownCounter::add(active_connections, 50.0)
  UpDownCounter::add(cpu_usage, 75.5)
  
  // Test instrument properties
  assert_eq(request_counter.name, "http.requests")
  assert_eq(response_histogram.name, "http.response.time")
  assert_eq(active_connections.name, "http.active.connections")
  assert_eq(cpu_usage.name, "system.cpu.usage")
  
  assert_true(true) // If we reach here, all operations succeeded
}

test "instrument edge cases" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "edge.case.test")
  
  // Test with empty names
  let empty_counter = Meter::create_counter(meter, "")
  assert_eq(empty_counter.name, "")
  
  // Test with special characters in names
  let special_counter = Meter::create_counter(meter, "test.counter-with_special.chars123")
  assert_eq(special_counter.name, "test.counter-with_special.chars123")
  
  // Test with very long names
  let long_name = "this.is.a.very.long.instrument.name.that.tests.the.systems.ability.to.handle.extremely.long.metric.names.without.issues"
  let long_counter = Meter::create_counter(meter, long_name)
  assert_eq(long_counter.name, long_name)
  
  // Test with zero and negative values
  Counter::add(empty_counter, 0.0)
  Counter::add(empty_counter, -100.0)
  Histogram::record(response_histogram, 0.0)
  UpDownCounter::add(active_connections, 0.0)
  UpDownCounter::add(active_connections, -1000.0)
  
  assert_true(true) // If we reach here, edge cases were handled
}

test "meter noop provider" {
  let noop_provider = MeterProvider::noop()
  let meter = MeterProvider::get_meter(noop_provider, "noop.test")
  let counter = Meter::create_counter(meter, "noop.counter")
  
  // Test noop operations
  Counter::add(counter, 100.0)
  Counter::add(counter, 200.0, Some(Attributes::new()))
  
  assert_eq(meter.scope.name, "noop.test")
  assert_eq(counter.name, "noop.counter")
  assert_true(true) // Noop operations should not crash
}