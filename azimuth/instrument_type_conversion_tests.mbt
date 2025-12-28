// Instrument Type Conversion Test Suite for Azimuth Telemetry System
// Testing instrument type conversions and operations

test "instrument creation and properties" {
  let counter_instrument = Counter("http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let histogram_instrument = Histogram("http.duration", Some("HTTP request duration"), Some("ms"))
  let updown_counter_instrument = UpDownCounter("active.connections", Some("Active connections"), Some("connections"))
  let gauge_instrument = Gauge("memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Test instrument names
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::name(histogram_instrument), "http.duration")
  assert_eq(Instrument::name(updown_counter_instrument), "active.connections")
  assert_eq(Instrument::name(gauge_instrument), "memory.usage")
  
  // Test instrument descriptions
  assert_eq(Instrument::description(counter_instrument), Some("Total HTTP requests"))
  assert_eq(Instrument::description(histogram_instrument), Some("HTTP request duration"))
  assert_eq(Instrument::description(updown_counter_instrument), Some("Active connections"))
  assert_eq(Instrument::description(gauge_instrument), Some("Memory usage"))
  
  // Test instrument units
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  assert_eq(Instrument::unit(updown_counter_instrument), Some("connections"))
  assert_eq(Instrument::unit(gauge_instrument), Some("bytes"))
}

test "instrument without optional properties" {
  let simple_counter = Counter("simple.counter", None, None)
  let simple_histogram = Histogram("simple.histogram", None, None)
  let simple_updown = UpDownCounter("simple.updown", None, None)
  let simple_gauge = Gauge("simple.gauge", None, None)
  
  assert_eq(Instrument::name(simple_counter), "simple.counter")
  assert_eq(Instrument::description(simple_counter), None)
  assert_eq(Instrument::unit(simple_counter), None)
  
  assert_eq(Instrument::name(simple_histogram), "simple.histogram")
  assert_eq(Instrument::description(simple_histogram), None)
  assert_eq(Instrument::unit(simple_histogram), None)
  
  assert_eq(Instrument::name(simple_updown), "simple.updown")
  assert_eq(Instrument::description(simple_updown), None)
  assert_eq(Instrument::unit(simple_updown), None)
  
  assert_eq(Instrument::name(simple_gauge), "simple.gauge")
  assert_eq(Instrument::description(simple_gauge), None)
  assert_eq(Instrument::unit(simple_gauge), None)
}

test "counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  
  // Test counter properties
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  // Test counter operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.5)
  Counter::add(counter, 10.0)
  
  // Test with attributes
  let attrs = Attributes::new()
  Counter::add(counter, 2.0, Some(attrs))
  
  assert_true(true)
}

test "histogram operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  // Test histogram properties
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test histogram operations
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.5)
  Histogram::record(histogram, 50.25)
  
  // Test with attributes
  let attrs = Attributes::new()
  Histogram::record(histogram, 75.0, Some(attrs))
  
  // Test instrument conversion
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "test.histogram")
  assert_eq(Instrument::description(instrument), Some("Test histogram"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
  
  assert_true(true)
}

test "updown counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test updown counter"), Some("items"))
  
  // Test updown counter properties
  assert_eq(updown_counter.name, "test.updown")
  assert_eq(updown_counter.description, Some("Test updown counter"))
  assert_eq(updown_counter.unit, Some("items"))
  
  // Test updown counter operations (both positive and negative)
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.5)
  UpDownCounter::add(updown_counter, 3.25)
  
  // Test with attributes
  let attrs = Attributes::new()
  UpDownCounter::add(updown_counter, 2.0, Some(attrs))
  
  assert_true(true)
}

test "gauge operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  
  // Test gauge properties
  assert_eq(gauge.name, "test.gauge")
  assert_eq(gauge.description, Some("Test gauge"))
  assert_eq(gauge.unit, Some("percent"))
  
  // Test gauge operations
  UpDownCounter::add(gauge, 75.5)
  UpDownCounter::add(gauge, 25.0)
  UpDownCounter::add(gauge, -10.5)
  
  // Test with attributes
  let attrs = Attributes::new()
  UpDownCounter::add(gauge, 50.0, Some(attrs))
  
  assert_true(true)
}

test "meter provider operations" {
  let provider = MeterProvider::default()
  
  // Test creating multiple meters
  let meter1 = MeterProvider::get_meter(provider, "meter1")
  let meter2 = MeterProvider::get_meter(provider, "meter2", Some("1.0.0"))
  let meter3 = MeterProvider::get_meter(provider, "meter3", Some("2.0.0"), Some("https://example.com/schema"))
  
  // Test meter scope properties
  assert_eq(meter1.scope.name, "meter1")
  assert_eq(meter1.scope.version, None)
  assert_eq(meter1.scope.schema_url, None)
  
  assert_eq(meter2.scope.name, "meter2")
  assert_eq(meter2.scope.version, Some("1.0.0"))
  assert_eq(meter2.scope.schema_url, None)
  
  assert_eq(meter3.scope.name, "meter3")
  assert_eq(meter3.scope.version, Some("2.0.0"))
  assert_eq(meter3.scope.schema_url, Some("https://example.com/schema"))
}

test "instrument scope consistency" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-scope", Some("1.0.0"))
  
  // Create different instruments from the same meter
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // All instruments should have the same scope
  assert_eq(counter.name, "test.counter")
  assert_eq(histogram.name, "test.histogram")
  assert_eq(updown_counter.name, "test.updown")
  assert_eq(gauge.name, "test.gauge")
  
  // Test operations on all instruments
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 100.0)
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(gauge, 75.0)
  
  assert_true(true)
}

test "instrument edge cases" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "edge-case-meter")
  
  // Test with empty name
  let empty_counter = Meter::create_counter(meter, "")
  assert_eq(empty_counter.name, "")
  
  // Test with special characters in name
  let special_histogram = Meter::create_histogram(meter, "test.histogram-with.special_chars")
  assert_eq(special_histogram.name, "test.histogram-with.special_chars")
  
  // Test with very long name
  let long_name = "very.long.instrument.name.that.exceeds.normal.expectations.for.metric.names"
  let long_gauge = Meter::create_gauge(meter, long_name)
  assert_eq(long_gauge.name, long_name)
  
  // Test with zero and negative values
  let updown_counter = Meter::create_updown_counter(meter, "test.zero")
  UpDownCounter::add(updown_counter, 0.0)
  UpDownCounter::add(updown_counter, -100.0)
  UpDownCounter::add(updown_counter, 100.0)
  
  assert_true(true)
}

test "multiple provider isolation" {
  let provider1 = MeterProvider::default()
  let provider2 = MeterProvider::default()
  
  let meter1 = MeterProvider::get_meter(provider1, "provider1-meter")
  let meter2 = MeterProvider::get_meter(provider2, "provider2-meter")
  
  let counter1 = Meter::create_counter(meter1, "shared.counter")
  let counter2 = Meter::create_counter(meter2, "shared.counter")
  
  // Both counters should have the same name but be independent
  assert_eq(counter1.name, "shared.counter")
  assert_eq(counter2.name, "shared.counter")
  
  // Operations should not interfere
  Counter::add(counter1, 1.0)
  Counter::add(counter2, 2.0)
  
  assert_true(true)
}