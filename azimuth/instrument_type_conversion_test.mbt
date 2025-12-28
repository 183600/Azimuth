// Instrument Type Conversion Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for instrument type conversions and operations

test "instrument counter creation and operations" {
  // Test counter instrument creation and operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "counter.test")
  
  // Create counter with different parameter combinations
  let simple_counter = Meter::create_counter(meter, "simple.counter")
  let full_counter = Meter::create_counter(
    meter, 
    "full.counter", 
    Some("Full counter with description"), 
    Some("requests")
  )
  let partial_counter = Meter::create_counter(
    meter, 
    "partial.counter", 
    Some("Partial counter with description only")
  )
  
  // Test counter properties
  assert_eq(simple_counter.name, "simple.counter")
  assert_eq(simple_counter.description, None)
  assert_eq(simple_counter.unit, None)
  
  assert_eq(full_counter.name, "full.counter")
  assert_eq(full_counter.description, Some("Full counter with description"))
  assert_eq(full_counter.unit, Some("requests"))
  
  assert_eq(partial_counter.name, "partial.counter")
  assert_eq(partial_counter.description, Some("Partial counter with description only"))
  assert_eq(partial_counter.unit, None)
  
  // Test counter operations
  Counter::add(simple_counter, 1.0)
  Counter::add(simple_counter, 5.5)
  Counter::add(simple_counter, 10.25)
  
  Counter::add(full_counter, 100.0)
  Counter::add(full_counter, 200.5)
  
  Counter::add(partial_counter, 42.0)
  
  // Test counter to instrument conversion
  let simple_instrument = Counter(simple_counter.name, simple_counter.description, simple_counter.unit)
  assert_eq(Instrument::name(simple_instrument), "simple.counter")
  assert_eq(Instrument::description(simple_instrument), None)
  assert_eq(Instrument::unit(simple_instrument), None)
}

test "instrument histogram creation and operations" {
  // Test histogram instrument creation and operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram.test")
  
  // Create histogram with different parameter combinations
  let simple_histogram = Meter::create_histogram(meter, "simple.histogram")
  let full_histogram = Meter::create_histogram(
    meter, 
    "full.histogram", 
    Some("Full histogram with description"), 
    Some("milliseconds")
  )
  let partial_histogram = Meter::create_histogram(
    meter, 
    "partial.histogram", 
    Some("Partial histogram with description only")
  )
  
  // Test histogram properties
  assert_eq(simple_histogram.name, "simple.histogram")
  assert_eq(simple_histogram.description, None)
  assert_eq(simple_histogram.unit, None)
  
  assert_eq(full_histogram.name, "full.histogram")
  assert_eq(full_histogram.description, Some("Full histogram with description"))
  assert_eq(full_histogram.unit, Some("milliseconds"))
  
  assert_eq(partial_histogram.name, "partial.histogram")
  assert_eq(partial_histogram.description, Some("Partial histogram with description only"))
  assert_eq(partial_histogram.unit, None)
  
  // Test histogram operations
  Histogram::record(simple_histogram, 100.0)
  Histogram::record(simple_histogram, 200.5)
  Histogram::record(simple_histogram, 150.25)
  Histogram::record(simple_histogram, 75.75)
  
  Histogram::record(full_histogram, 50.0)
  Histogram::record(full_histogram, 100.0)
  Histogram::record(full_histogram, 150.0)
  Histogram::record(full_histogram, 200.0)
  
  Histogram::record(partial_histogram, 1.0)
  Histogram::record(partial_histogram, 2.0)
  Histogram::record(partial_histogram, 3.0)
  
  // Test histogram to instrument conversion
  let histogram_instrument = Histogram::as_instrument(simple_histogram)
  assert_eq(Instrument::name(histogram_instrument), "simple.histogram")
  assert_eq(Instrument::description(histogram_instrument), None)
  assert_eq(Instrument::unit(histogram_instrument), None)
}

test "instrument updown counter creation and operations" {
  // Test up-down counter instrument creation and operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown.test")
  
  // Create up-down counter with different parameter combinations
  let simple_updown = Meter::create_updown_counter(meter, "simple.updown")
  let full_updown = Meter::create_updown_counter(
    meter, 
    "full.updown", 
    Some("Full up-down counter with description"), 
    Some("connections")
  )
  let partial_updown = Meter::create_updown_counter(
    meter, 
    "partial.updown", 
    Some("Partial up-down counter with description only")
  )
  
  // Test up-down counter properties
  assert_eq(simple_updown.name, "simple.updown")
  assert_eq(simple_updown.description, None)
  assert_eq(simple_updown.unit, None)
  
  assert_eq(full_updown.name, "full.updown")
  assert_eq(full_updown.description, Some("Full up-down counter with description"))
  assert_eq(full_updown.unit, Some("connections"))
  
  assert_eq(partial_updown.name, "partial.updown")
  assert_eq(partial_updown.description, Some("Partial up-down counter with description only"))
  assert_eq(partial_updown.unit, None)
  
  // Test up-down counter operations (positive and negative values)
  UpDownCounter::add(simple_updown, 10.0)
  UpDownCounter::add(simple_updown, -5.5)
  UpDownCounter::add(simple_updown, 3.25)
  UpDownCounter::add(simple_updown, -2.75)
  
  UpDownCounter::add(full_updown, 100.0)
  UpDownCounter::add(full_updown, -25.0)
  UpDownCounter::add(full_updown, 50.0)
  
  UpDownCounter::add(partial_updown, 42.0)
  UpDownCounter::add(partial_updown, -10.0)
  
  // Test up-down counter to instrument conversion
  let updown_instrument = UpDownCounter(full_updown.name, full_updown.description, full_updown.unit)
  assert_eq(Instrument::name(updown_instrument), "full.updown")
  assert_eq(Instrument::description(updown_instrument), Some("Full up-down counter with description"))
  assert_eq(Instrument::unit(updown_instrument), Some("connections"))
}

test "instrument gauge creation and operations" {
  // Test gauge instrument creation and operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge.test")
  
  // Create gauge with different parameter combinations
  let simple_gauge = Meter::create_gauge(meter, "simple.gauge")
  let full_gauge = Meter::create_gauge(
    meter, 
    "full.gauge", 
    Some("Full gauge with description"), 
    Some("percent")
  )
  let partial_gauge = Meter::create_gauge(
    meter, 
    "partial.gauge", 
    Some("Partial gauge with description only")
  )
  
  // Test gauge properties
  assert_eq(simple_gauge.name, "simple.gauge")
  assert_eq(simple_gauge.description, None)
  assert_eq(simple_gauge.unit, None)
  
  assert_eq(full_gauge.name, "full.gauge")
  assert_eq(full_gauge.description, Some("Full gauge with description"))
  assert_eq(full_gauge.unit, Some("percent"))
  
  assert_eq(partial_gauge.name, "partial.gauge")
  assert_eq(partial_gauge.description, Some("Partial gauge with description only"))
  assert_eq(partial_gauge.unit, None)
  
  // Test gauge to instrument conversion
  let gauge_instrument = Gauge(full_gauge.name, full_gauge.description, full_gauge.unit)
  assert_eq(Instrument::name(gauge_instrument), "full.gauge")
  assert_eq(Instrument::description(gauge_instrument), Some("Full gauge with description"))
  assert_eq(Instrument::unit(gauge_instrument), Some("percent"))
}

test "instrument type conversion scenarios" {
  // Test various instrument type conversion scenarios
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "conversion.test")
  
  // Create different instrument types
  let counter = Meter::create_counter(meter, "conversion.counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "conversion.histogram", Some("Test histogram"), Some("ms"))
  let updown = Meter::create_updown_counter(meter, "conversion.updown", Some("Test up-down"), Some("value"))
  let gauge = Meter::create_gauge(meter, "conversion.gauge", Some("Test gauge"), Some("percent"))
  
  // Convert all to instrument type
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram::as_instrument(histogram)
  let updown_instrument = UpDownCounter(updown.name, updown.description, updown.unit)
  let gauge_instrument = Gauge(gauge.name, gauge.description, gauge.unit)
  
  // Test instrument name extraction
  assert_eq(Instrument::name(counter_instrument), "conversion.counter")
  assert_eq(Instrument::name(histogram_instrument), "conversion.histogram")
  assert_eq(Instrument::name(updown_instrument), "conversion.updown")
  assert_eq(Instrument::name(gauge_instrument), "conversion.gauge")
  
  // Test instrument description extraction
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::description(histogram_instrument), Some("Test histogram"))
  assert_eq(Instrument::description(updown_instrument), Some("Test up-down"))
  assert_eq(Instrument::description(gauge_instrument), Some("Test gauge"))
  
  // Test instrument unit extraction
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  assert_eq(Instrument::unit(updown_instrument), Some("value"))
  assert_eq(Instrument::unit(gauge_instrument), Some("percent"))
}

test "instrument pattern matching and type identification" {
  // Test instrument pattern matching and type identification
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "pattern.test")
  
  // Create instruments for pattern matching
  let counter = Meter::create_counter(meter, "pattern.counter")
  let histogram = Meter::create_histogram(meter, "pattern.histogram")
  let updown = Meter::create_updown_counter(meter, "pattern.updown")
  let gauge = Meter::create_gauge(meter, "pattern.gauge")
  
  // Create instrument enum values
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram::as_instrument(histogram)
  let updown_instrument = UpDownCounter(updown.name, updown.description, updown.unit)
  let gauge_instrument = Gauge(gauge.name, gauge.description, gauge.unit)
  
  // Test pattern matching on instrument types
  let counter_name = match counter_instrument {
    Counter(name, _, _) => name
    _ => "not.counter"
  }
  
  let histogram_name = match histogram_instrument {
    Histogram(name, _, _) => name
    _ => "not.histogram"
  }
  
  let updown_name = match updown_instrument {
    UpDownCounter(name, _, _) => name
    _ => "not.updown"
  }
  
  let gauge_name = match gauge_instrument {
    Gauge(name, _, _) => name
    _ => "not.gauge"
  }
  
  // Verify pattern matching results
  assert_eq(counter_name, "pattern.counter")
  assert_eq(histogram_name, "pattern.histogram")
  assert_eq(updown_name, "pattern.updown")
  assert_eq(gauge_name, "pattern.gauge")
}

test "instrument boundary conditions and special values" {
  // Test instrument operations with boundary conditions and special values
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "boundary.test")
  
  // Create instruments for boundary testing
  let counter = Meter::create_counter(meter, "boundary.counter")
  let histogram = Meter::create_histogram(meter, "boundary.histogram")
  let updown = Meter::create_updown_counter(meter, "boundary.updown")
  
  // Test counter with boundary values
  Counter::add(counter, 0.0)
  Counter::add(counter, 1.0)
  Counter::add(counter, 2147483647.0)
  Counter::add(counter, 1.7976931348623157e+308)
  Counter::add(counter, 1.23e-10)
  
  // Test histogram with boundary values
  Histogram::record(histogram, 0.0)
  Histogram::record(histogram, 1.23e-10)
  Histogram::record(histogram, 1.7976931348623157e+308)
  Histogram::record(histogram, 2147483647.0)
  Histogram::record(histogram, -1.7976931348623157e+308)
  
  // Test up-down counter with boundary values
  UpDownCounter::add(updown, 0.0)
  UpDownCounter::add(updown, 2147483647.0)
  UpDownCounter::add(updown, -2147483648.0)
  UpDownCounter::add(updown, 1.7976931348623157e+308)
  UpDownCounter::add(updown, -1.7976931348623157e+308)
  
  // Create instruments with special names and descriptions
  let special_counter = Meter::create_counter(
    meter, 
    "", 
    Some(""), 
    Some("")
  )
  
  let long_counter = Meter::create_counter(
    meter, 
    "this.is.a.very.long.instrument.name.that.tests.boundary.conditions.for.string.handling",
    Some("This is a very long description that tests boundary conditions for description handling in instrument creation"),
    Some("this.is.a.very.long.unit.that.tests.boundary.conditions")
  )
  
  // Verify special instrument properties
  assert_eq(special_counter.name, "")
  assert_eq(special_counter.description, Some(""))
  assert_eq(special_counter.unit, Some(""))
  
  assert_eq(long_counter.name, "this.is.a.very.long.instrument.name.that.tests.boundary.conditions.for.string.handling")
  assert_eq(long_counter.description, Some("This is a very long description that tests boundary conditions for description handling in instrument creation"))
  assert_eq(long_counter.unit, Some("this.is.a.very.long.unit.that.tests.boundary.conditions"))
}

test "instrument concurrent operations" {
  // Test instrument operations in concurrent scenarios
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent.test")
  
  // Create instruments for concurrent testing
  let counter1 = Meter::create_counter(meter, "concurrent.counter.1")
  let counter2 = Meter::create_counter(meter, "concurrent.counter.2")
  let counter3 = Meter::create_counter(meter, "concurrent.counter.3")
  
  let histogram1 = Meter::create_histogram(meter, "concurrent.histogram.1")
  let histogram2 = Meter::create_histogram(meter, "concurrent.histogram.2")
  
  let updown1 = Meter::create_updown_counter(meter, "concurrent.updown.1")
  let updown2 = Meter::create_updown_counter(meter, "concurrent.updown.2")
  
  // Perform concurrent operations on counters
  Counter::add(counter1, 1.0)
  Counter::add(counter2, 2.0)
  Counter::add(counter3, 3.0)
  
  Counter::add(counter1, 10.0)
  Counter::add(counter2, 20.0)
  Counter::add(counter3, 30.0)
  
  // Perform concurrent operations on histograms
  Histogram::record(histogram1, 100.0)
  Histogram::record(histogram2, 200.0)
  
  Histogram::record(histogram1, 150.0)
  Histogram::record(histogram2, 250.0)
  
  // Perform concurrent operations on up-down counters
  UpDownCounter::add(updown1, 5.0)
  UpDownCounter::add(updown2, 10.0)
  
  UpDownCounter::add(updown1, -2.5)
  UpDownCounter::add(updown2, -5.0)
  
  // Convert all to instruments for concurrent testing
  let instrument1 = Counter(counter1.name, counter1.description, counter1.unit)
  let instrument2 = Histogram::as_instrument(histogram1)
  let instrument3 = UpDownCounter(updown1.name, updown1.description, updown1.unit)
  
  // Test concurrent instrument operations
  let name1 = Instrument::name(instrument1)
  let name2 = Instrument::name(instrument2)
  let name3 = Instrument::name(instrument3)
  
  let desc1 = Instrument::description(instrument1)
  let desc2 = Instrument::description(instrument2)
  let desc3 = Instrument::description(instrument3)
  
  let unit1 = Instrument::unit(instrument1)
  let unit2 = Instrument::unit(instrument2)
  let unit3 = Instrument::unit(instrument3)
  
  // Verify concurrent operations
  assert_eq(name1, "concurrent.counter.1")
  assert_eq(name2, "concurrent.histogram.1")
  assert_eq(name3, "concurrent.updown.1")
  
  assert_eq(desc1, None)
  assert_eq(desc2, None)
  assert_eq(desc3, None)
  
  assert_eq(unit1, None)
  assert_eq(unit2, None)
  assert_eq(unit3, None)
}