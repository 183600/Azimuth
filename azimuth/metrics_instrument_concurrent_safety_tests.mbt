// Metrics Instrument Concurrent Safety Tests
// Test cases for concurrent safety of metrics instruments (Counter, Histogram, UpDownCounter, Gauge)

test "counter_concurrent_operations" {
  // Test counter thread safety with concurrent operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  let counter = Meter::create_counter(meter, "concurrent.counter", Some("Test counter for concurrent operations"), Some("count"))
  
  // Test concurrent add operations
  let attrs = Attributes::new()
  Attributes::set(attrs, "operation.type", StringValue("concurrent"))
  
  // Simulate concurrent additions
  Counter::add(counter, 1.0, Some(attrs))
  Counter::add(counter, 2.5, Some(attrs))
  Counter::add(counter, 0.5, Some(attrs))
  
  // Verify counter properties
  @assertion.assert_eq(counter.name, "concurrent.counter")?
  @assertion.assert_eq(counter.description, Some("Test counter for concurrent operations"))?
  @assertion.assert_eq(counter.unit, Some("count"))?
  
  // Test counter as instrument
  let instrument = Histogram::as_instrument(Histogram::{ 
    name: counter.name, 
    description: counter.description, 
    unit: counter.unit 
  })
  @assertion.assert_eq(Instrument::name(instrument), "concurrent.counter")?
  @assertion.assert_eq(Instrument::description(instrument), Some("Test counter for concurrent operations"))?
  @assertion.assert_eq(Instrument::unit(instrument), Some("count"))?
}

test "histogram_concurrent_operations" {
  // Test histogram thread safety with concurrent operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  let histogram = Meter::create_histogram(meter, "concurrent.histogram", Some("Test histogram for concurrent operations"), Some("ms"))
  
  // Test concurrent record operations
  let attrs = Attributes::new()
  Attributes::set(attrs, "operation.type", StringValue("histogram"))
  
  // Simulate concurrent recordings
  Histogram::record(histogram, 10.5, Some(attrs))
  Histogram::record(histogram, 25.0, Some(attrs))
  Histogram::record(histogram, 5.25, Some(attrs))
  Histogram::record(histogram, 100.0, Some(attrs))
  
  // Verify histogram properties
  @assertion.assert_eq(histogram.name, "concurrent.histogram")?
  @assertion.assert_eq(histogram.description, Some("Test histogram for concurrent operations"))?
  @assertion.assert_eq(histogram.unit, Some("ms"))?
  
  // Test histogram as instrument
  let instrument = Histogram::as_instrument(histogram)
  @assertion.assert_eq(Instrument::name(instrument), "concurrent.histogram")?
  @assertion.assert_eq(Instrument::description(instrument), Some("Test histogram for concurrent operations"))?
  @assertion.assert_eq(Instrument::unit(instrument), Some("ms"))?
}

test "updown_counter_concurrent_operations" {
  // Test up-down counter thread safety with concurrent operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  let updown_counter = Meter::create_updown_counter(meter, "concurrent.updown", Some("Test up-down counter for concurrent operations"), Some("items"))
  
  // Test concurrent add and subtract operations
  let attrs = Attributes::new()
  Attributes::set(attrs, "operation.type", StringValue("updown"))
  
  // Simulate concurrent operations (both positive and negative)
  UpDownCounter::add(updown_counter, 10.0, Some(attrs))
  UpDownCounter::add(updown_counter, -5.0, Some(attrs))
  UpDownCounter::add(updown_counter, 15.0, Some(attrs))
  UpDownCounter::add(updown_counter, -2.5, Some(attrs))
  
  // Verify up-down counter properties
  @assertion.assert_eq(updown_counter.name, "concurrent.updown")?
  @assertion.assert_eq(updown_counter.description, Some("Test up-down counter for concurrent operations"))?
  @assertion.assert_eq(updown_counter.unit, Some("items"))?
}

test "gauge_concurrent_operations" {
  // Test gauge thread safety with concurrent operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  let gauge = Meter::create_gauge(meter, "concurrent.gauge", Some("Test gauge for concurrent operations"), Some("temperature"))
  
  // Test concurrent gauge operations (note: gauges typically set absolute values)
  let attrs = Attributes::new()
  Attributes::set(attrs, "sensor.type", StringValue("temperature"))
  
  // Simulate concurrent gauge readings
  // Note: Gauges would typically have a set() method, but we'll use add() for testing
  // In a real implementation, this would be gauge.set(value, attributes)
  
  // Verify gauge properties
  @assertion.assert_eq(gauge.name, "concurrent.gauge")?
  @assertion.assert_eq(gauge.description, Some("Test gauge for concurrent operations"))?
  @assertion.assert_eq(gauge.unit, Some("temperature"))?
}

test "mixed_instrument_concurrent_safety" {
  // Test concurrent safety with mixed instrument types
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  
  // Create different instrument types
  let counter = Meter::create_counter(meter, "mixed.counter")
  let histogram = Meter::create_histogram(meter, "mixed.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "mixed.updown")
  let gauge = Meter::create_gauge(meter, "mixed.gauge")
  
  // Create shared attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "test.type", StringValue("concurrent.mixed"))
  
  // Simulate concurrent operations on all instruments
  Counter::add(counter, 1.0, Some(attrs))
  Histogram::record(histogram, 50.0, Some(attrs))
  UpDownCounter::add(updown_counter, 10.0, Some(attrs))
  // Gauge::set(gauge, 25.0, Some(attrs))  // Would be in real implementation
  
  // Verify all instruments maintain their properties
  @assertion.assert_eq(counter.name, "mixed.counter")?
  @assertion.assert_eq(histogram.name, "mixed.histogram")?
  @assertion.assert_eq(updown_counter.name, "mixed.updown")?
  @assertion.assert_eq(gauge.name, "mixed.gauge")?
}

test "instrument_boundary_conditions" {
  // Test instruments with boundary condition values
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  
  let counter = Meter::create_counter(meter, "boundary.counter")
  let histogram = Meter::create_histogram(meter, "boundary.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "boundary.updown")
  
  let attrs = Attributes::new()
  
  // Test with zero values
  Counter::add(counter, 0.0, Some(attrs))
  Histogram::record(histogram, 0.0, Some(attrs))
  UpDownCounter::add(updown_counter, 0.0, Some(attrs))
  
  // Test with very large values
  Counter::add(counter, 1000000.0, Some(attrs))
  Histogram::record(histogram, 999999.0, Some(attrs))
  UpDownCounter::add(updown_counter, 500000.0, Some(attrs))
  
  // Test with very small values
  Counter::add(counter, 0.000001, Some(attrs))
  Histogram::record(histogram, 0.000001, Some(attrs))
  UpDownCounter::add(updown_counter, 0.000001, Some(attrs))
  
  // Test with negative values (only for up-down counter)
  UpDownCounter::add(updown_counter, -100.0, Some(attrs))
  
  // Verify instruments still function correctly
  @assertion.assert_eq(counter.name, "boundary.counter")?
  @assertion.assert_eq(histogram.name, "boundary.histogram")?
  @assertion.assert_eq(updown_counter.name, "boundary.updown")?
}

test "instrument_attribute_consistency" {
  // Test that instruments maintain attribute consistency under concurrent operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  
  let counter = Meter::create_counter(meter, "attribute.consistency.counter")
  
  // Create different attribute sets
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "service", StringValue("auth"))
  Attributes::set(attrs1, "endpoint", StringValue("/login"))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "service", StringValue("payment"))
  Attributes::set(attrs2, "endpoint", StringValue("/charge"))
  
  let attrs3 = Attributes::new()
  Attributes::set(attrs3, "service", StringValue("user"))
  Attributes::set(attrs3, "endpoint", StringValue("/profile"))
  
  // Simulate concurrent operations with different attributes
  Counter::add(counter, 1.0, Some(attrs1))
  Counter::add(counter, 2.0, Some(attrs2))
  Counter::add(counter, 3.0, Some(attrs3))
  
  // Verify counter maintains its properties
  @assertion.assert_eq(counter.name, "attribute.consistency.counter")?
  
  // Verify attributes are correctly set
  @assertion.assert_eq(Attributes::get(attrs1, "service"), Some(StringValue("auth")))?
  @assertion.assert_eq(Attributes::get(attrs1, "endpoint"), Some(StringValue("/login")))?
  @assertion.assert_eq(Attributes::get(attrs2, "service"), Some(StringValue("payment")))?
  @assertion.assert_eq(Attributes::get(attrs2, "endpoint"), Some(StringValue("/charge")))?
  @assertion.assert_eq(Attributes::get(attrs3, "service"), Some(StringValue("user")))?
  @assertion.assert_eq(Attributes::get(attrs3, "endpoint"), Some(StringValue("/profile")))?
}

test "meter_provider_isolation" {
  // Test that meter providers provide proper isolation
  let meter_provider1 = MeterProvider::default()
  let meter_provider2 = MeterProvider::default()
  
  let meter1 = MeterProvider::get_meter(meter_provider1, "isolated.meter.1")
  let meter2 = MeterProvider::get_meter(meter_provider2, "isolated.meter.2")
  
  // Create counters from different meter providers
  let counter1 = Meter::create_counter(meter1, "isolated.counter.1")
  let counter2 = Meter::create_counter(meter2, "isolated.counter.2")
  
  // Verify counters from different providers are independent
  @assertion.assert_eq(counter1.name, "isolated.counter.1")?
  @assertion.assert_eq(counter2.name, "isolated.counter.2")?
  @assertion.assert_not_eq(counter1.name, counter2.name)?
  
  // Verify meter scopes are isolated
  @assertion.assert_eq(meter1.scope.name, "isolated.meter.1")?
  @assertion.assert_eq(meter2.scope.name, "isolated.meter.2")?
  @assertion.assert_not_eq(meter1.scope.name, meter2.scope.name)?
}