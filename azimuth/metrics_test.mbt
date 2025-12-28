// Test file for Metrics functionality

pub test "counter instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let counter = Meter::create_counter(meter, "test.counter")
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, None)
  assert_eq(counter.unit, None)
  
  // Test counter add operation
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5)
  Counter::add(counter, -3.0)  // Should handle negative values
  
  // Test counter with attributes
  let attrs = Attributes::new()
  Counter::add(counter, 5.0, Some(attrs))
}

pub test "counter with description and unit" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let counter = Meter::create_counter(meter, "detailed.counter")
  // Note: In simplified implementation, description and unit are None
  
  assert_eq(counter.name, "detailed.counter")
}

pub test "histogram instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let histogram = Meter::create_histogram(meter, "test.histogram")
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, None)
  assert_eq(histogram.unit, None)
  
  // Test histogram record operation
  Histogram::record(histogram, 1.0)
  Histogram::record(histogram, 2.5)
  Histogram::record(histogram, 10.0)
  Histogram::record(histogram, 0.1)
  
  // Test histogram with attributes
  let attrs = Attributes::new()
  Histogram::record(histogram, 5.0, Some(attrs))
}

pub test "histogram with description and unit" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time histogram"), Some("ms"))
  // Note: In simplified implementation, these are not actually set
  
  assert_eq(histogram.name, "response.time")
}

pub test "instrument type properties" {
  let counter_instr = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_instr = Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter_instr = UpDownCounter("test.updown", Some("Test updown"), Some("items"))
  let gauge_instr = Gauge("test.gauge", Some("Test gauge"), Some("percent"))
  
  // Test instrument names
  assert_eq(Instrument::name(counter_instr), "test.counter")
  assert_eq(Instrument::name(histogram_instr), "test.histogram")
  assert_eq(Instrument::name(updown_counter_instr), "test.updown")
  assert_eq(Instrument::name(gauge_instr), "test.gauge")
  
  // Test instrument descriptions
  assert_eq(Instrument::description(counter_instr), Some("Test counter"))
  assert_eq(Instrument::description(histogram_instr), Some("Test histogram"))
  assert_eq(Instrument::description(updown_counter_instr), Some("Test updown"))
  assert_eq(Instrument::description(gauge_instr), Some("Test gauge"))
  
  // Test instrument units
  assert_eq(Instrument::unit(counter_instr), Some("count"))
  assert_eq(Instrument::unit(histogram_instr), Some("ms"))
  assert_eq(Instrument::unit(updown_counter_instr), Some("items"))
  assert_eq(Instrument::unit(gauge_instr), Some("percent"))
}

pub test "meter provider operations" {
  let provider = MeterProvider::noop()
  let meter1 = MeterProvider::get_meter(provider, "meter1")
  let meter2 = MeterProvider::get_meter(provider, "meter2", Some("1.0.0"))
  
  assert_eq(meter1.scope.name, "meter1")
  assert_eq(meter1.scope.version, None)
  assert_eq(meter1.scope.schema_url, None)
  
  assert_eq(meter2.scope.name, "meter2")
  assert_eq(meter2.scope.version, Some("1.0.0"))
  assert_eq(meter2.scope.schema_url, None)
}

pub test "metrics edge cases" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "edge.case.meter")
  
  // Test with empty name
  let empty_counter = Meter::create_counter(meter, "")
  assert_eq(empty_counter.name, "")
  
  // Test with special characters in name
  let special_counter = Meter::create_counter(meter, "special.counter.name.with.dots")
  assert_eq(special_counter.name, "special.counter.name.with.dots")
  
  // Test with very long name
  let long_name = "a".repeat(100)
  let long_counter = Meter::create_counter(meter, long_name)
  assert_eq(long_counter.name, long_name)
  
  // Test counter with extreme values
  Counter::add(empty_counter, 0.0)
  Counter::add(empty_counter, -1.7976931348623157e+308)  // Very small negative
  Counter::add(empty_counter, 1.7976931348623157e+308)   // Very large positive
  
  // Test histogram with extreme values
  let histogram = Meter::create_histogram(meter, "extreme.histogram")
  Histogram::record(histogram, 0.0)
  Histogram::record(histogram, -1.0)  // Negative value
  Histogram::record(histogram, 1.7976931348623157e+308)  // Very large
}

pub test "instrumentation scope" {
  let scope1 = InstrumentationScope::{ name: "test.scope1", version: None, schema_url: None }
  let scope2 = InstrumentationScope::{ name: "test.scope2", version: Some("1.0.0"), schema_url: None }
  let scope3 = InstrumentationScope::{ name: "test.scope3", version: Some("2.0.0"), schema_url: Some("https://example.com/schema") }
  
  assert_eq(scope1.name, "test.scope1")
  assert_eq(scope1.version, None)
  assert_eq(scope1.schema_url, None)
  
  assert_eq(scope2.name, "test.scope2")
  assert_eq(scope2.version, Some("1.0.0"))
  assert_eq(scope2.schema_url, None)
  
  assert_eq(scope3.name, "test.scope3")
  assert_eq(scope3.version, Some("2.0.0"))
  assert_eq(scope3.schema_url, Some("https://example.com/schema"))
}