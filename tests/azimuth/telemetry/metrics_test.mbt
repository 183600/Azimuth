test "meter provider and meter creation" {
  // Test meter provider and meter creation
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "test.meter", Some("1.0.0"))
  
  let scope = meter.scope
  assert_eq(scope.name, "test.meter")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
}

test "counter instrument creation and usage" {
  // Test counter creation and basic operations
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "counter.test")
  let counter = @azimuth.Meter::create_counter(meter, "test_counter", Some("Test counter description"), Some("count"))
  
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, Some("Test counter description"))
  assert_eq(counter.unit, Some("count"))
  
  // Test adding values to counter
  @azimuth.Counter::add(counter, 1.0)
  @azimuth.Counter::add(counter, 5.0, Some(@azimuth.Attributes::new()))
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "histogram instrument creation and usage" {
  // Test histogram creation and basic operations
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "histogram.test")
  let histogram = @azimuth.Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  assert_eq(histogram.name, "test_histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test recording values in histogram
  @azimuth.Histogram::record(histogram, 100.0)
  @azimuth.Histogram::record(histogram, 250.5, Some(@azimuth.Attributes::new()))
  
  // Test converting to instrument
  let instrument = @azimuth.Histogram::as_instrument(histogram)
  assert_eq(@azimuth.Instrument::name(instrument), "test_histogram")
  assert_eq(@azimuth.Instrument::description(instrument), Some("Test histogram"))
  assert_eq(@azimuth.Instrument::unit(instrument), Some("ms"))
}

test "instrument type properties" {
  // Test different instrument types and their properties
  let counter_instrument = @azimuth.Counter("counter_name", Some("Counter desc"), Some("unit1"))
  let histogram_instrument = @azimuth.Histogram("histogram_name", Some("Histogram desc"), Some("unit2"))
  let updown_counter_instrument = @azimuth.UpDownCounter("updown_name", Some("UpDown desc"), Some("unit3"))
  let gauge_instrument = @azimuth.Gauge("gauge_name", Some("Gauge desc"), Some("unit4"))
  
  // Test name property
  assert_eq(@azimuth.Instrument::name(counter_instrument), "counter_name")
  assert_eq(@azimuth.Instrument::name(histogram_instrument), "histogram_name")
  assert_eq(@azimuth.Instrument::name(updown_counter_instrument), "updown_name")
  assert_eq(@azimuth.Instrument::name(gauge_instrument), "gauge_name")
  
  // Test description property
  assert_eq(@azimuth.Instrument::description(counter_instrument), Some("Counter desc"))
  assert_eq(@azimuth.Instrument::description(histogram_instrument), Some("Histogram desc"))
  assert_eq(@azimuth.Instrument::description(updown_counter_instrument), Some("UpDown desc"))
  assert_eq(@azimuth.Instrument::description(gauge_instrument), Some("Gauge desc"))
  
  // Test unit property
  assert_eq(@azimuth.Instrument::unit(counter_instrument), Some("unit1"))
  assert_eq(@azimuth.Instrument::unit(histogram_instrument), Some("unit2"))
  assert_eq(@azimuth.Instrument::unit(updown_counter_instrument), Some("unit3"))
  assert_eq(@azimuth.Instrument::unit(gauge_instrument), Some("unit4"))
}

test "meter with noop provider" {
  // Test meter creation with noop provider
  let noop_provider = @azimuth.MeterProvider::noop()
  let meter = @azimuth.MeterProvider::get_meter(noop_provider, "noop.test")
  
  let counter = @azimuth.Meter::create_counter(meter, "noop_counter")
  let histogram = @azimuth.Meter::create_histogram(meter, "noop_histogram")
  
  // Operations should not throw exceptions even with noop provider
  @azimuth.Counter::add(counter, 42.0)
  @azimuth.Histogram::record(histogram, 123.45)
  
  assert_eq(counter.name, "noop_counter")
  assert_eq(histogram.name, "noop_histogram")
}

test "instrument optional properties" {
  // Test instruments with optional properties
  let provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(provider, "optional.test")
  
  // Create instruments without optional properties
  let counter_no_opts = @azimuth.Meter::create_counter(meter, "no_opts_counter")
  let histogram_no_opts = @azimuth.Meter::create_histogram(meter, "no_opts_histogram")
  
  assert_eq(counter_no_opts.name, "no_opts_counter")
  assert_eq(counter_no_opts.description, None)
  assert_eq(counter_no_opts.unit, None)
  
  assert_eq(histogram_no_opts.name, "no_opts_histogram")
  assert_eq(histogram_no_opts.description, None)
  assert_eq(histogram_no_opts.unit, None)
}