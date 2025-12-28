// Histogram and advanced metrics tests
// Focus on testing histogram operations and instrument type handling

test "histogram creation and properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-test-meter")
  
  // Test histogram creation with different parameter combinations
  let simple_histogram = Meter::create_histogram(meter, "simple.histogram")
  let described_histogram = Meter::create_histogram(meter, "described.histogram", "A histogram with description")
  let full_histogram = Meter::create_histogram(meter, "full.histogram", "Complete histogram", "milliseconds")
  
  // Test histogram properties
  assert_eq(simple_histogram.name, "simple.histogram")
  assert_eq(described_histogram.name, "described.histogram")
  assert_eq(full_histogram.name, "full.histogram")
  
  // Test descriptions
  assert_eq(simple_histogram.description, None)
  match described_histogram.description {
    Some(desc) => assert_eq(desc, "A histogram with description")
    None => assert_true(false, "Expected description")
  }
  match full_histogram.description {
    Some(desc) => assert_eq(desc, "Complete histogram")
    None => assert_true(false, "Expected description")
  }
  
  // Test units
  assert_eq(simple_histogram.unit, None)
  assert_eq(described_histogram.unit, None)
  match full_histogram.unit {
    Some(unit) => assert_eq(unit, "milliseconds")
    None => assert_true(false, "Expected unit")
  }
}

test "histogram recording operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "recording-test-meter")
  let histogram = Meter::create_histogram(meter, "test.histogram", "Test histogram", "operations")
  
  // Test recording different value types
  Histogram::record(histogram, 1.0)
  Histogram::record(histogram, 2.5)
  Histogram::record(histogram, 10.0)
  Histogram::record(histogram, 100.5)
  
  // Test recording with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "operation.type", StringValue("test"))
  Histogram::record(histogram, 5.5, Some(attrs))
  
  // Test recording edge cases
  Histogram::record(histogram, 0.0)
  Histogram::record(histogram, -1.0)  // Negative values
  Histogram::record(histogram, 999999.999)  // Large values
  
  // Test that operations don't crash
  assert_true(true)
}

test "instrument type conversions" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "instrument-test-meter")
  
  // Test different instrument types
  let counter = Meter::create_counter(meter, "test.counter", "Test counter", "count")
  let histogram = Meter::create_histogram(meter, "test.histogram", "Test histogram", "ms")
  
  // Test converting to instrument type
  let counter_instrument = Instrument::name(Counter("test.counter", Some("Test counter"), Some("count")))
  let histogram_instrument = Histogram::as_instrument(histogram)
  
  // Test instrument name retrieval
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  
  // Test instrument description retrieval
  match Instrument::description(counter_instrument) {
    Some(desc) => assert_eq(desc, "Test counter")
    None => assert_true(false, "Expected counter description")
  }
  match Instrument::description(histogram_instrument) {
    Some(desc) => assert_eq(desc, "Test histogram")
    None => assert_true(false, "Expected histogram description")
  }
  
  // Test instrument unit retrieval
  match Instrument::unit(counter_instrument) {
    Some(unit) => assert_eq(unit, "count")
    None => assert_true(false, "Expected counter unit")
  }
  match Instrument::unit(histogram_instrument) {
    Some(unit) => assert_eq(unit, "ms")
    None => assert_true(false, "Expected histogram unit")
  }
}

test "up down counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown-test-meter")
  
  // Test creating up-down counter
  let updown_counter = UpDownCounter("test.updown", Some("Test up-down counter"), Some("items"))
  
  // Test instrument properties
  assert_eq(Instrument::name(updown_counter), "test.updown")
  match Instrument::description(updown_counter) {
    Some(desc) => assert_eq(desc, "Test up-down counter")
    None => assert_true(false, "Expected description")
  }
  match Instrument::unit(updown_counter) {
    Some(unit) => assert_eq(unit, "items")
    None => assert_true(false, "Expected unit")
  }
}

test "gauge instrument operations" {
  // Test creating gauge instrument
  let gauge = Gauge("test.gauge", Some("Test gauge"), Some("percent"))
  
  // Test gauge properties through instrument interface
  assert_eq(Instrument::name(gauge), "test.gauge")
  match Instrument::description(gauge) {
    Some(desc) => assert_eq(desc, "Test gauge")
    None => assert_true(false, "Expected gauge description")
  }
  match Instrument::unit(gauge) {
    Some(unit) => assert_eq(unit, "percent")
    None => assert_true(false, "Expected gauge unit")
  }
  
  // Test that gauge has no description
  let gauge_no_desc = Gauge("simple.gauge", None, None)
  assert_eq(Instrument::name(gauge_no_desc), "simple.gauge")
  assert_eq(Instrument::description(gauge_no_desc), None)
  assert_eq(Instrument::unit(gauge_no_desc), None)
}

test "complex metrics scenarios" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "complex-scenario-meter")
  
  // Create multiple instruments
  let counter = Meter::create_counter(meter, "request.count", "Total requests", "requests")
  let histogram = Meter::create_histogram(meter, "request.duration", "Request duration", "ms")
  
  // Test complex attribute scenarios
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("test-service"))
  Attributes::set(attrs, "endpoint", StringValue("/api/test"))
  
  // Record metrics with complex attributes
  Counter::add(counter, 1.0, Some(attrs))
  Histogram::record(histogram, 150.5, Some(attrs))
  
  // Test recording with different attribute combinations
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "service.name", StringValue("test-service"))
  Attributes::set(error_attrs, "error.type", StringValue("timeout"))
  
  Counter::add(counter, 1.0, Some(error_attrs))
  Histogram::record(histogram, 5000.0, Some(error_attrs))
  
  // Verify instrument properties remain consistent
  assert_eq(counter.name, "request.count")
  assert_eq(histogram.name, "request.duration")
  assert_true(true)  // Test passes if no crashes occur
}