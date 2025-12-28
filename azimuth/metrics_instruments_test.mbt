test "meter provider and meter creation" {
  // Test meter provider creation
  let noop_provider = @azimuth.MeterProvider::noop()
  let default_provider = @azimuth.MeterProvider::default()
  
  // Test getting meters from providers
  let noop_meter = @azimuth.MeterProvider::get_meter(noop_provider, "noop_meter")
  let default_meter = @azimuth.MeterProvider::get_meter(default_provider, "default_meter")
  
  // Test meter scope properties
  let noop_scope = noop_meter.scope
  let default_scope = default_meter.scope
  
  assert_eq(noop_scope.name, "noop_meter")
  assert_eq(default_scope.name, "default_meter")
  assert_eq(noop_scope.version, None)
  assert_eq(default_scope.version, None)
}

test "counter instrument creation and operations" {
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "test_meter")
  
  // Test counter creation
  let simple_counter = @azimuth.Meter::create_counter(meter, "simple_counter")
  let counter_with_desc = @azimuth.Meter::create_counter(meter, "described_counter", "A counter with description")
  let full_counter = @azimuth.Meter::create_counter(meter, "full_counter", "Full counter", "operations")
  
  // Test counter properties
  assert_eq(simple_counter.name, "simple_counter")
  assert_eq(counter_with_desc.name, "described_counter")
  assert_eq(full_counter.name, "full_counter")
  
  assert_eq(simple_counter.description, None)
  assert_eq(counter_with_desc.description, Some("A counter with description"))
  assert_eq(full_counter.description, Some("Full counter"))
  
  assert_eq(simple_counter.unit, None)
  assert_eq(counter_with_desc.unit, None)
  assert_eq(full_counter.unit, Some("operations"))
  
  // Test counter operations
  @azimuth.Counter::add(simple_counter, 1.0)
  @azimuth.Counter::add(counter_with_desc, 5.5)
  @azimuth.Counter::add(full_counter, 10.0)
  
  // Test with attributes
  let attrs = @azimuth.Attributes::new()
  @azimuth.Counter::add(simple_counter, 2.0, Some(attrs))
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "histogram instrument creation and operations" {
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "test_meter")
  
  // Test histogram creation with different configurations
  let simple_histogram = @azimuth.Meter::create_histogram(meter, "response_time")
  let histogram_with_desc = @azimuth.Meter::create_histogram(meter, "latency", "Request latency histogram")
  let full_histogram = @azimuth.Meter::create_histogram(meter, "duration", "Operation duration", "milliseconds")
  
  // Test histogram properties
  assert_eq(simple_histogram.name, "response_time")
  assert_eq(histogram_with_desc.name, "latency")
  assert_eq(full_histogram.name, "duration")
  
  assert_eq(simple_histogram.description, None)
  assert_eq(histogram_with_desc.description, Some("Request latency histogram"))
  assert_eq(full_histogram.description, Some("Operation duration"))
  
  assert_eq(simple_histogram.unit, None)
  assert_eq(histogram_with_desc.unit, None)
  assert_eq(full_histogram.unit, Some("milliseconds"))
  
  // Test histogram recording operations
  @azimuth.Histogram::record(simple_histogram, 100.0)
  @azimuth.Histogram::record(histogram_with_desc, 250.5)
  @azimuth.Histogram::record(full_histogram, 500.0)
  
  // Test with attributes
  let attrs = @azimuth.Attributes::new()
  @azimuth.Histogram::record(simple_histogram, 150.0, Some(attrs))
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "instrument type enumeration and properties" {
  // Test different instrument types
  let counter_instrument = @azimuth.Counter("test_counter", Some("Test counter"), Some("count"))
  let histogram_instrument = @azimuth.Histogram("test_histogram", Some("Test histogram"), Some("ms"))
  let updown_counter_instrument = @azimuth.UpDownCounter("test_updown", Some("Test updown"), Some("items"))
  let gauge_instrument = @azimuth.Gauge("test_gauge", Some("Test gauge"), Some("value"))
  
  // Test instrument names
  assert_eq(@azimuth.Instrument::name(counter_instrument), "test_counter")
  assert_eq(@azimuth.Instrument::name(histogram_instrument), "test_histogram")
  assert_eq(@azimuth.Instrument::name(updown_counter_instrument), "test_updown")
  assert_eq(@azimuth.Instrument::name(gauge_instrument), "test_gauge")
  
  // Test instrument descriptions
  assert_eq(@azimuth.Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(@azimuth.Instrument::description(histogram_instrument), Some("Test histogram"))
  assert_eq(@azimuth.Instrument::description(updown_counter_instrument), Some("Test updown"))
  assert_eq(@azimuth.Instrument::description(gauge_instrument), Some("Test gauge"))
  
  // Test instrument units
  assert_eq(@azimuth.Instrument::unit(counter_instrument), Some("count"))
  assert_eq(@azimuth.Instrument::unit(histogram_instrument), Some("ms"))
  assert_eq(@azimuth.Instrument::unit(updown_counter_instrument), Some("items"))
  assert_eq(@azimuth.Instrument::unit(gauge_instrument), Some("value"))
}

test "instrument with None properties" {
  // Test instruments with optional properties set to None
  let counter_instrument = @azimuth.Counter("simple_counter", None, None)
  let histogram_instrument = @azimuth.Histogram("simple_histogram", None, None)
  let updown_counter_instrument = @azimuth.UpDownCounter("simple_updown", None, None)
  let gauge_instrument = @azimuth.Gauge("simple_gauge", None, None)
  
  // Test that None properties are handled correctly
  assert_eq(@azimuth.Instrument::description(counter_instrument), None)
  assert_eq(@azimuth.Instrument::description(histogram_instrument), None)
  assert_eq(@azimuth.Instrument::description(updown_counter_instrument), None)
  assert_eq(@azimuth.Instrument::description(gauge_instrument), None)
  
  assert_eq(@azimuth.Instrument::unit(counter_instrument), None)
  assert_eq(@azimuth.Instrument::unit(histogram_instrument), None)
  assert_eq(@azimuth.Instrument::unit(updown_counter_instrument), None)
  assert_eq(@azimuth.Instrument::unit(gauge_instrument), None)
}

test "histogram as instrument conversion" {
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "test_meter")
  let histogram = @azimuth.Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  // Test converting histogram to instrument
  let instrument = @azimuth.Histogram::as_instrument(histogram)
  
  // Test instrument properties
  assert_eq(@azimuth.Instrument::name(instrument), "test_histogram")
  assert_eq(@azimuth.Instrument::description(instrument), Some("Test histogram"))
  assert_eq(@azimuth.Instrument::unit(instrument), Some("ms"))
}

test "meter with different scope configurations" {
  let provider = @azimuth.MeterProvider::default()
  
  // Test meters with different scope configurations
  let basic_meter = @azimuth.MeterProvider::get_meter(provider, "basic_meter")
  let versioned_meter = @azimuth.MeterProvider::get_meter(provider, "versioned_meter", Some("2.1.0"))
  
  // Test scope properties
  let basic_scope = basic_meter.scope
  let versioned_scope = versioned_meter.scope
  
  assert_eq(basic_scope.name, "basic_meter")
  assert_eq(basic_scope.version, None)
  assert_eq(basic_scope.schema_url, None)
  
  assert_eq(versioned_scope.name, "versioned_meter")
  assert_eq(versioned_scope.version, Some("2.1.0"))
  assert_eq(versioned_scope.schema_url, None)
}

test "metrics with edge case values" {
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "edge_case_meter")
  
  let counter = @azimuth.Meter::create_counter(meter, "edge_counter")
  let histogram = @azimuth.Meter::create_histogram(meter, "edge_histogram")
  
  // Test with edge case numeric values
  @azimuth.Counter::add(counter, 0.0)  // Zero value
  @azimuth.Counter::add(counter, -1.0)  // Negative value
  @azimuth.Counter::add(counter, 999999.999999)  // Large decimal value
  
  @azimuth.Histogram::record(histogram, 0.001)  // Very small value
  @azimuth.Histogram::record(histogram, -100.0)  // Negative histogram value
  @azimuth.Histogram::record(histogram, 1.7976931348623157e+308)  // Max double value
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}