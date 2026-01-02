// Azimuth Telemetry System - Metric Instrument Type Conversion Tests
// This file contains comprehensive test cases for metric instrument type conversions

// Test 1: Counter instrument creation and properties
test "counter instrument creation and properties" {
  // Create a meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Create a counter with full properties
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter description"), Some("count"))
  
  // Verify counter properties
  assert_eq(counter.name, "test.counter")
  match counter.description {
    Some(desc) => assert_eq(desc, "Test counter description")
    None => assert_true(false)
  }
  match counter.unit {
    Some(unit) => assert_eq(unit, "count")
    None => assert_true(false)
  }
  
  // Convert counter to instrument and verify properties
  let instrument = Histogram::as_instrument(Histogram::{ name: counter.name, description: counter.description, unit: counter.unit })
  assert_eq(Instrument::name(instrument), "test.counter")
  match Instrument::description(instrument) {
    Some(desc) => assert_eq(desc, "Test counter description")
    None => assert_true(false)
  }
  match Instrument::unit(instrument) {
    Some(unit) => assert_eq(unit, "count")
    None => assert_true(false)
  }
}

// Test 2: Histogram instrument creation and properties
test "histogram instrument creation and properties" {
  // Create a meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Create a histogram with full properties
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram description"), Some("ms"))
  
  // Verify histogram properties
  assert_eq(histogram.name, "test.histogram")
  match histogram.description {
    Some(desc) => assert_eq(desc, "Test histogram description")
    None => assert_true(false)
  }
  match histogram.unit {
    Some(unit) => assert_eq(unit, "ms")
    None => assert_true(false)
  }
  
  // Convert histogram to instrument and verify properties
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "test.histogram")
  match Instrument::description(instrument) {
    Some(desc) => assert_eq(desc, "Test histogram description")
    None => assert_true(false)
  }
  match Instrument::unit(instrument) {
    Some(unit) => assert_eq(unit, "ms")
    None => assert_true(false)
  }
}

// Test 3: UpDownCounter instrument creation and properties
test "updown_counter instrument creation and properties" {
  // Create a meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Create an updown counter with full properties
  let updown_counter = Meter::create_updown_counter(meter, "test.updown_counter", Some("Test updown counter description"), Some("bytes"))
  
  // Verify updown counter properties
  assert_eq(updown_counter.name, "test.updown_counter")
  match updown_counter.description {
    Some(desc) => assert_eq(desc, "Test updown counter description")
    None => assert_true(false)
  }
  match updown_counter.unit {
    Some(unit) => assert_eq(unit, "bytes")
    None => assert_true(false)
  }
}

// Test 4: Gauge instrument creation and properties
test "gauge instrument creation and properties" {
  // Create a meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Create a gauge with full properties
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge description"), Some("percent"))
  
  // Verify gauge properties
  assert_eq(gauge.name, "test.gauge")
  match gauge.description {
    Some(desc) => assert_eq(desc, "Test gauge description")
    None => assert_true(false)
  }
  match gauge.unit {
    Some(unit) => assert_eq(unit, "percent")
    None => assert_true(false)
  }
}

// Test 5: Instrument type identification and conversion
test "instrument type identification and conversion" {
  // Create different instrument types
  let counter_instrument = Counter("test.counter", Some("Counter description"), Some("count"))
  let histogram_instrument = Histogram("test.histogram", Some("Histogram description"), Some("ms"))
  let updown_counter_instrument = UpDownCounter("test.updown_counter", Some("UpDownCounter description"), Some("bytes"))
  let gauge_instrument = Gauge("test.gauge", Some("Gauge description"), Some("percent"))
  
  // Verify instrument names
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  assert_eq(Instrument::name(updown_counter_instrument), "test.updown_counter")
  assert_eq(Instrument::name(gauge_instrument), "test.gauge")
  
  // Verify instrument descriptions
  match Instrument::description(counter_instrument) {
    Some(desc) => assert_eq(desc, "Counter description")
    None => assert_true(false)
  }
  match Instrument::description(histogram_instrument) {
    Some(desc) => assert_eq(desc, "Histogram description")
    None => assert_true(false)
  }
  match Instrument::description(updown_counter_instrument) {
    Some(desc) => assert_eq(desc, "UpDownCounter description")
    None => assert_true(false)
  }
  match Instrument::description(gauge_instrument) {
    Some(desc) => assert_eq(desc, "Gauge description")
    None => assert_true(false)
  }
  
  // Verify instrument units
  match Instrument::unit(counter_instrument) {
    Some(unit) => assert_eq(unit, "count")
    None => assert_true(false)
  }
  match Instrument::unit(histogram_instrument) {
    Some(unit) => assert_eq(unit, "ms")
    None => assert_true(false)
  }
  match Instrument::unit(updown_counter_instrument) {
    Some(unit) => assert_eq(unit, "bytes")
    None => assert_true(false)
  }
  match Instrument::unit(gauge_instrument) {
    Some(unit) => assert_eq(unit, "percent")
    None => assert_true(false)
  }
}

// Test 6: Instrument operations with attributes
test "instrument operations with attributes" {
  // Create a meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Create instruments
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown_counter")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // Create attributes
  let attributes = Attributes {
    values: [
      ("service.name", AttributeValue::StringValue("test-service")),
      ("operation.type", AttributeValue::StringValue("database")),
      ("operation.status", AttributeValue::StringValue("success"))
    ]
  }
  
  // Perform operations with attributes
  Counter::add(counter, 1.0, Some(attributes))
  Histogram::record(histogram, 100.5, Some(attributes))
  UpDownCounter::add(updown_counter, 5.0, Some(attributes))
  // Note: Gauge doesn't have a record operation in the simplified implementation
}

// Test 7: Instrument operations without attributes
test "instrument operations without attributes" {
  // Create a meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Create instruments without descriptions and units
  let counter = Meter::create_counter(meter, "simple.counter")
  let histogram = Meter::create_histogram(meter, "simple.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "simple.updown_counter")
  let gauge = Meter::create_gauge(meter, "simple.gauge")
  
  // Verify properties are None
  match counter.description {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match counter.unit {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Perform operations without attributes
  Counter::add(counter, 10.0, None)
  Histogram::record(histogram, 50.25, None)
  UpDownCounter::add(updown_counter, -2.5, None)
}

// Test 8: Instrument with complex attribute values
test "instrument with complex attribute values" {
  // Create a meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Create instruments
  let counter = Meter::create_counter(meter, "complex.counter")
  let histogram = Meter::create_histogram(meter, "complex.histogram")
  
  // Create attributes with complex values
  let complex_attributes = Attributes {
    values: [
      ("string.array", AttributeValue::ArrayStringValue(["value1", "value2", "value3"])),
      ("int.array", AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])),
      ("float.value", AttributeValue::FloatValue(3.14159)),
      ("bool.value", AttributeValue::BoolValue(true)),
      ("int.value", AttributeValue::IntValue(42))
    ]
  }
  
  // Perform operations with complex attributes
  Counter::add(counter, 1.0, Some(complex_attributes))
  Histogram::record(histogram, 200.75, Some(complex_attributes))
}

// Test 9: Instrument edge cases and error handling
test "instrument edge cases and error handling" {
  // Create a meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  
  // Test instruments with empty names
  let empty_counter = Meter::create_counter(meter, "")
  let empty_histogram = Meter::create_histogram(meter, "")
  let empty_updown_counter = Meter::create_updown_counter(meter, "")
  let empty_gauge = Meter::create_gauge(meter, "")
  
  assert_eq(empty_counter.name, "")
  assert_eq(empty_histogram.name, "")
  assert_eq(empty_updown_counter.name, "")
  assert_eq(empty_gauge.name, "")
  
  // Test instruments with very long names
  let long_name = "this-is-a-very-long-instrument-name-that-might-be-used-in-real-world-scenarios-where-descriptive-names-are-important-for-monitoring-and-debugging-purposes"
  let long_counter = Meter::create_counter(meter, long_name)
  let long_histogram = Meter::create_histogram(meter, long_name)
  
  assert_eq(long_counter.name, long_name)
  assert_eq(long_histogram.name, long_name)
  
  // Test instruments with special characters in names
  let special_name = "metric/with/special/chars?and=symbols"
  let special_counter = Meter::create_counter(meter, special_name)
  let special_histogram = Meter::create_histogram(meter, special_name)
  
  assert_eq(special_counter.name, special_name)
  assert_eq(special_histogram.name, special_name)
  
  // Test operations with zero and negative values
  Counter::add(empty_counter, 0.0, None)
  Counter::add(empty_counter, -10.0, None)
  Histogram::record(empty_histogram, 0.0, None)
  Histogram::record(empty_histogram, -50.5, None)
  UpDownCounter::add(empty_updown_counter, 0.0, None)
  UpDownCounter::add(empty_updown_counter, -100.0, None)
}

// Test 10: Instrument type conversion consistency
test "instrument type conversion consistency" {
  // Create instruments with consistent properties
  let counter_instrument = Counter("consistent.metric", Some("Consistent description"), Some("unit"))
  let histogram_instrument = Histogram("consistent.metric", Some("Consistent description"), Some("unit"))
  let updown_counter_instrument = UpDownCounter("consistent.metric", Some("Consistent description"), Some("unit"))
  let gauge_instrument = Gauge("consistent.metric", Some("Consistent description"), Some("unit"))
  
  // Verify all instruments have the same properties
  assert_eq(Instrument::name(counter_instrument), Instrument::name(histogram_instrument))
  assert_eq(Instrument::name(histogram_instrument), Instrument::name(updown_counter_instrument))
  assert_eq(Instrument::name(updown_counter_instrument), Instrument::name(gauge_instrument))
  
  assert_eq(Instrument::description(counter_instrument), Instrument::description(histogram_instrument))
  assert_eq(Instrument::description(histogram_instrument), Instrument::description(updown_counter_instrument))
  assert_eq(Instrument::description(updown_counter_instrument), Instrument::description(gauge_instrument))
  
  assert_eq(Instrument::unit(counter_instrument), Instrument::unit(histogram_instrument))
  assert_eq(Instrument::unit(histogram_instrument), Instrument::unit(updown_counter_instrument))
  assert_eq(Instrument::unit(updown_counter_instrument), Instrument::unit(gauge_instrument))
  
  // Test with None values
  let counter_none = Counter("none.metric", None, None)
  let histogram_none = Histogram("none.metric", None, None)
  let updown_counter_none = UpDownCounter("none.metric", None, None)
  let gauge_none = Gauge("none.metric", None, None)
  
  assert_eq(Instrument::name(counter_none), Instrument::name(histogram_none))
  assert_eq(Instrument::name(histogram_none), Instrument::name(updown_counter_none))
  assert_eq(Instrument::name(updown_counter_none), Instrument::name(gauge_none))
  
  match Instrument::description(counter_none) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match Instrument::unit(counter_none) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}