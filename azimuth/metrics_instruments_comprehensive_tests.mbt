// Metrics Instruments Comprehensive Test Suite
// Tests for Counter, Histogram, UpDownCounter, Gauge, and Meter operations

test "meter_provider_and_meter_creation" {
  // Test MeterProvider and Meter creation
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "test.meter")
  let meter2 = MeterProvider::get_meter(provider, "test.meter.v2", Some("1.0.0"))
  
  // Test meter scope properties
  let scope1 = meter1.scope
  let scope2 = meter2.scope
  
  assert_eq(scope1.name, "test.meter")
  assert_eq(scope1.version, None)
  
  assert_eq(scope2.name, "test.meter.v2")
  match scope2.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false, "Expected version")
  }
}

test "counter_creation_and_properties" {
  // Test Counter creation and property access
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let counter1 = Meter::create_counter(meter, "request.count")
  let counter2 = Meter::create_counter(meter, "response.size", Some("Total response size"), Some("bytes"))
  
  assert_eq(counter1.name, "request.count")
  assert_eq(counter1.description, None)
  assert_eq(counter1.unit, None)
  
  assert_eq(counter2.name, "response.size")
  match counter2.description {
    Some(desc) => assert_eq(desc, "Total response size")
    None => assert_true(false, "Expected description")
  }
  match counter2.unit {
    Some(unit) => assert_eq(unit, "bytes")
    None => assert_true(false, "Expected unit")
  }
}

test "counter_add_operations" {
  // Test Counter add operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let counter = Meter::create_counter(meter, "test.counter")
  
  // Test basic add operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.5)
  Counter::add(counter, 10.0)
  
  // Test add with attributes
  let attrs = Attributes::new()
  Counter::add(counter, 2.5, Some(attrs))
  
  assert_true(true) // Simplified implementation just tests no crashes
}

test "histogram_creation_and_properties" {
  // Test Histogram creation and property access
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let histogram1 = Meter::create_histogram(meter, "request.duration")
  let histogram2 = Meter::create_histogram(meter, "response.latency", Some("Request latency distribution"), Some("ms"))
  
  assert_eq(histogram1.name, "request.duration")
  assert_eq(histogram1.description, None)
  assert_eq(histogram1.unit, None)
  
  assert_eq(histogram2.name, "response.latency")
  match histogram2.description {
    Some(desc) => assert_eq(desc, "Request latency distribution")
    None => assert_true(false, "Expected description")
  }
  match histogram2.unit {
    Some(unit) => assert_eq(unit, "ms")
    None => assert_true(false, "Expected unit")
  }
}

test "histogram_record_operations" {
  // Test Histogram record operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  
  // Test basic record operations
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 250.5)
  Histogram::record(histogram, 500.0)
  Histogram::record(histogram, 1000.0)
  
  // Test record with attributes
  let attrs = Attributes::new()
  Histogram::record(histogram, 750.0, Some(attrs))
  
  assert_true(true) // Simplified implementation just tests no crashes
}

test "updown_counter_creation_and_operations" {
  // Test UpDownCounter creation and operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let updown_counter1 = Meter::create_updown_counter(meter, "active.connections")
  let updown_counter2 = Meter::create_updown_counter(meter, "memory.usage", Some("Current memory usage"), Some("MB"))
  
  assert_eq(updown_counter1.name, "active.connections")
  assert_eq(updown_counter1.description, None)
  assert_eq(updown_counter1.unit, None)
  
  assert_eq(updown_counter2.name, "memory.usage")
  match updown_counter2.description {
    Some(desc) => assert_eq(desc, "Current memory usage")
    None => assert_true(false, "Expected description")
  }
  
  // Test add operations (positive and negative values)
  UpDownCounter::add(updown_counter1, 10.0)
  UpDownCounter::add(updown_counter1, -3.0)
  UpDownCounter::add(updown_counter1, 5.0)
  
  // Test with attributes
  let attrs = Attributes::new()
  UpDownCounter::add(updown_counter2, 512.0, Some(attrs))
  UpDownCounter::add(updown_counter2, -128.0, Some(attrs))
  
  assert_true(true) // Simplified implementation just tests no crashes
}

test "gauge_creation_and_properties" {
  // Test Gauge creation and property access
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let gauge1 = Meter::create_gauge(meter, "cpu.usage")
  let gauge2 = Meter::create_gauge(meter, "disk.space", Some("Available disk space"), Some("GB"))
  
  assert_eq(gauge1.name, "cpu.usage")
  assert_eq(gauge1.description, None)
  assert_eq(gauge1.unit, None)
  
  assert_eq(gauge2.name, "disk.space")
  match gauge2.description {
    Some(desc) => assert_eq(desc, "Available disk space")
    None => assert_true(false, "Expected description")
  }
  match gauge2.unit {
    Some(unit) => assert_eq(unit, "GB")
    None => assert_true(false, "Expected unit")
  }
}

test "instrument_type_conversion_and_properties" {
  // Test instrument type conversions and property access
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  // Convert to Instrument enum and test properties
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram::as_instrument(histogram)
  
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  
  match Instrument::description(counter_instrument) {
    Some(desc) => assert_eq(desc, "Test counter")
    None => assert_true(false, "Expected description")
  }
  
  match Instrument::unit(counter_instrument) {
    Some(unit) => assert_eq(unit, "count")
    None => assert_true(false, "Expected unit")
  }
}

test "multiple_meter_independence" {
  // Test that different meters maintain independence
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "meter1")
  let meter2 = MeterProvider::get_meter(provider, "meter2")
  
  let counter1 = Meter::create_counter(meter1, "counter")
  let counter2 = Meter::create_counter(meter2, "counter")
  
  assert_eq(counter1.name, "counter")
  assert_eq(counter2.name, "counter")
  
  // But they belong to different meters (scopes)
  assert_ne(meter1.scope.name, meter2.scope.name)
}

test "instrument_name_validation" {
  // Test instrument creation with various name formats
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  
  // Test different naming patterns
  let simple_counter = Meter::create_counter(meter, "simple")
  let dotted_counter = Meter::create_counter(meter, "request.count")
  let underscore_counter = Meter::create_counter(meter, "error_rate_total")
  
  assert_eq(simple_counter.name, "simple")
  assert_eq(dotted_counter.name, "request.count")
  assert_eq(underscore_counter.name, "error_rate_total")
  
  // All should be valid instrument names
  assert_true(simple_counter.name.length() > 0)
  assert_true(dotted_counter.name.length() > 0)
  assert_true(underscore_counter.name.length() > 0)
}

test "meter_noop_provider_behavior" {
  // Test noop meter provider behavior
  let noop_provider = MeterProvider::noop()
  let noop_meter = MeterProvider::get_meter(noop_provider, "noop.meter")
  let noop_counter = Meter::create_counter(noop_meter, "noop.counter")
  
  // Should still have proper metadata
  assert_eq(noop_counter.name, "noop.counter")
  assert_eq(noop_meter.scope.name, "noop.meter")
  
  // Operations should not crash
  Counter::add(noop_counter, 1.0)
  
  assert_true(true) // Should complete without errors
}