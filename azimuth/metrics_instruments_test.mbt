// Azimuth Telemetry System - Metrics Instruments Test
// Comprehensive test suite for different types of metric instruments

test "counter_instrument_creation_and_properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test basic counter creation
  let counter = Meter::create_counter(meter, "request.count")
  @assertion.assert_eq(counter.name, "request.count")?
  @assertion.assert_eq(counter.description, None)?
  @assertion.assert_eq(counter.unit, None)?
  
  // Test counter with description and unit
  let detailed_counter = Meter::create_counter(meter, "response.bytes", Some("Total response bytes"), Some("bytes"))
  @assertion.assert_eq(detailed_counter.name, "response.bytes")?
  @assertion.assert_eq(detailed_counter.description, Some("Total response bytes"))?
  @assertion.assert_eq(detailed_counter.unit, Some("bytes"))?
  
  // Test counter as instrument
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  @assertion.assert_eq(Instrument::name(counter_instrument), "request.count")?
  @assertion.assert_eq(Instrument::description(counter_instrument), None)?
  @assertion.assert_eq(Instrument::unit(counter_instrument), None)?
}

test "counter_measurement_operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "test.counter")
  
  // Test basic counter increment
  Counter::add(counter, 1.0)
  
  // Test counter increment with attributes
  let attrs = Attributes::new()
  Counter::add(counter, 5.0, Some(attrs))
  
  // Test counter increment with zero value
  Counter::add(counter, 0.0)
  
  // Test counter increment with negative value (should be allowed for counters)
  Counter::add(counter, -2.0)
  
  // Test counter increment with fractional value
  Counter::add(counter, 3.14159)
  
  // Test counter increment with large value
  Counter::add(counter, 999999.999)
  
  // All operations should complete without errors
  @assertion.assert_true(true)?
}

test "histogram_instrument_creation_and_properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test basic histogram creation
  let histogram = Meter::create_histogram(meter, "request.duration")
  @assertion.assert_eq(histogram.name, "request.duration")?
  @assertion.assert_eq(histogram.description, None)?
  @assertion.assert_eq(histogram.unit, None)?
  
  // Test histogram with description and unit
  let detailed_histogram = Meter::create_histogram(
    meter, 
    "response.size", 
    Some("Response size distribution"), 
    Some("bytes")
  )
  @assertion.assert_eq(detailed_histogram.name, "response.size")?
  @assertion.assert_eq(detailed_histogram.description, Some("Response size distribution"))?
  @assertion.assert_eq(detailed_histogram.unit, Some("bytes"))?
  
  // Test histogram as instrument
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  @assertion.assert_eq(Instrument::name(histogram_instrument), "request.duration")?
  @assertion.assert_eq(Instrument::description(histogram_instrument), None)?
  @assertion.assert_eq(Instrument::unit(histogram_instrument), None)?
}

test "histogram_measurement_operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  
  // Test basic histogram recording
  Histogram::record(histogram, 100.0)
  
  // Test histogram recording with attributes
  let attrs = Attributes::new()
  Histogram::record(histogram, 250.5, Some(attrs))
  
  // Test histogram recording with zero value
  Histogram::record(histogram, 0.0)
  
  // Test histogram recording with negative value (should be allowed)
  Histogram::record(histogram, -50.0)
  
  // Test histogram recording with fractional values
  Histogram::record(histogram, 3.14159)
  Histogram::record(histogram, 2.71828)
  
  // Test histogram recording with large values
  Histogram::record(histogram, 1000000.0)
  Histogram::record(histogram, 0.000001)
  
  // All operations should complete without errors
  @assertion.assert_true(true)?
}

test "up_down_counter_instrument_creation_and_properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test basic up-down counter creation
  let up_down_counter = Meter::create_up_down_counter(meter, "active.connections")
  @assertion.assert_eq(up_down_counter.name, "active.connections")?
  @assertion.assert_eq(up_down_counter.description, None)?
  @assertion.assert_eq(up_down_counter.unit, None)?
  
  // Test up-down counter with description and unit
  let detailed_up_down_counter = Meter::create_up_down_counter(
    meter, 
    "memory.usage", 
    Some("Current memory usage"), 
    Some("bytes")
  )
  @assertion.assert_eq(detailed_up_down_counter.name, "memory.usage")?
  @assertion.assert_eq(detailed_up_down_counter.description, Some("Current memory usage"))?
  @assertion.assert_eq(detailed_up_down_counter.unit, Some("bytes"))?
  
  // Test up-down counter as instrument
  let up_down_counter_instrument = UpDownCounter(up_down_counter.name, up_down_counter.description, up_down_counter.unit)
  @assertion.assert_eq(Instrument::name(up_down_counter_instrument), "active.connections")?
  @assertion.assert_eq(Instrument::description(up_down_counter_instrument), None)?
  @assertion.assert_eq(Instrument::unit(up_down_counter_instrument), None)?
}

test "up_down_counter_measurement_operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let up_down_counter = Meter::create_up_down_counter(meter, "test.up_down_counter")
  
  // Test basic up-down counter increment
  UpDownCounter::add(up_down_counter, 10.0)
  
  // Test up-down counter decrement (negative value)
  UpDownCounter::add(up_down_counter, -5.0)
  
  // Test up-down counter with attributes
  let attrs = Attributes::new()
  UpDownCounter::add(up_down_counter, 3.0, Some(attrs))
  
  // Test up-down counter with zero value
  UpDownCounter::add(up_down_counter, 0.0)
  
  // Test up-down counter with large positive value
  UpDownCounter::add(up_down_counter, 1000000.0)
  
  // Test up-down counter with large negative value
  UpDownCounter::add(up_down_counter, -1000000.0)
  
  // All operations should complete without errors
  @assertion.assert_true(true)?
}

test "gauge_instrument_creation_and_properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test basic gauge creation
  let gauge = Meter::create_gauge(meter, "cpu.temperature")
  @assertion.assert_eq(gauge.name, "cpu.temperature")?
  @assertion.assert_eq(gauge.description, None)?
  @assertion.assert_eq(gauge.unit, None)?
  
  // Test gauge with description and unit
  let detailed_gauge = Meter::create_gauge(
    meter, 
    "disk.usage", 
    Some("Current disk usage percentage"), 
    Some("percent")
  )
  @assertion.assert_eq(detailed_gauge.name, "disk.usage")?
  @assertion.assert_eq(detailed_gauge.description, Some("Current disk usage percentage"))?
  @assertion.assert_eq(detailed_gauge.unit, Some("percent"))?
  
  // Test gauge as instrument
  let gauge_instrument = Gauge(gauge.name, gauge.description, gauge.unit)
  @assertion.assert_eq(Instrument::name(gauge_instrument), "cpu.temperature")?
  @assertion.assert_eq(Instrument::description(gauge_instrument), None)?
  @assertion.assert_eq(Instrument::unit(gauge_instrument), None)?
}

test "gauge_measurement_operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // Test basic gauge recording
  Gauge::record(gauge, 75.5)
  
  // Test gauge recording with attributes
  let attrs = Attributes::new()
  Gauge::record(gauge, 25.0, Some(attrs))
  
  // Test gauge recording with zero value
  Gauge::record(gauge, 0.0)
  
  // Test gauge recording with negative value (should be allowed for gauges)
  Gauge::record(gauge, -10.0)
  
  // Test gauge recording with fractional values
  Gauge::record(gauge, 98.6)
  Gauge::record(gauge, -273.15)
  
  // Test gauge recording with extreme values
  Gauge::record(gauge, 1000000.0)
  Gauge::record(gauge, -1000000.0)
  
  // All operations should complete without errors
  @assertion.assert_true(true)?
}

test "instrument_type_identification" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Create different instrument types
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let up_down_counter = Meter::create_up_down_counter(meter, "test.up_down_counter")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // Create instrument enum values
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  let up_down_counter_instrument = UpDownCounter(up_down_counter.name, up_down_counter.description, up_down_counter.unit)
  let gauge_instrument = Gauge(gauge.name, gauge.description, gauge.unit)
  
  // Test instrument name extraction
  @assertion.assert_eq(Instrument::name(counter_instrument), "test.counter")?
  @assertion.assert_eq(Instrument::name(histogram_instrument), "test.histogram")?
  @assertion.assert_eq(Instrument::name(up_down_counter_instrument), "test.up_down_counter")?
  @assertion.assert_eq(Instrument::name(gauge_instrument), "test.gauge")?
  
  // Test instrument description extraction
  @assertion.assert_eq(Instrument::description(counter_instrument), None)?
  @assertion.assert_eq(Instrument::description(histogram_instrument), None)?
  @assertion.assert_eq(Instrument::description(up_down_counter_instrument), None)?
  @assertion.assert_eq(Instrument::description(gauge_instrument), None)?
  
  // Test instrument unit extraction
  @assertion.assert_eq(Instrument::unit(counter_instrument), None)?
  @assertion.assert_eq(Instrument::unit(histogram_instrument), None)?
  @assertion.assert_eq(Instrument::unit(up_down_counter_instrument), None)?
  @assertion.assert_eq(Instrument::unit(gauge_instrument), None)?
}

test "meter_provider_and_meter_operations" {
  // Test default meter provider
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "meter1")
  let meter2 = MeterProvider::get_meter(provider, "meter2")
  
  // Test creating instruments from different meters
  let counter1 = Meter::create_counter(meter1, "counter1")
  let counter2 = Meter::create_counter(meter2, "counter2")
  
  @assertion.assert_eq(counter1.name, "counter1")?
  @assertion.assert_eq(counter2.name, "counter2")?
  
  // Test noop meter provider
  let noop_provider = MeterProvider::noop()
  let noop_meter = MeterProvider::get_meter(noop_provider, "noop.meter")
  let noop_counter = Meter::create_counter(noop_meter, "noop.counter")
  
  @assertion.assert_eq(noop_counter.name, "noop.counter")?
  
  // All operations should complete without errors
  @assertion.assert_true(true)?
}

test "instrument_with_attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let up_down_counter = Meter::create_up_down_counter(meter, "test.up_down_counter")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // Create attributes for measurements
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("test-service"))
  Attributes::set(attrs, "operation.type", StringValue("http.request"))
  Attributes::set(attrs, "status.code", IntValue(200))
  
  // Test measurements with attributes
  Counter::add(counter, 1.0, Some(attrs))
  Histogram::record(histogram, 100.0, Some(attrs))
  UpDownCounter::add(up_down_counter, 5.0, Some(attrs))
  Gauge::record(gauge, 75.0, Some(attrs))
  
  // Test measurements with None attributes
  Counter::add(counter, 2.0, None)
  Histogram::record(histogram, 200.0, None)
  UpDownCounter::add(up_down_counter, -3.0, None)
  Gauge::record(gauge, 50.0, None)
  
  // All operations should complete without errors
  @assertion.assert_true(true)?
}

test "instrument_edge_cases" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  let counter = Meter::create_counter(meter, "edge.case.counter")
  let histogram = Meter::create_histogram(meter, "edge.case.histogram")
  let up_down_counter = Meter::create_up_down_counter(meter, "edge.case.up_down_counter")
  let gauge = Meter::create_gauge(meter, "edge.case.gauge")
  
  // Test with special floating point values
  Counter::add(counter, 0.0)
  Counter::add(counter, -0.0)
  
  Histogram::record(histogram, 0.0)
  Histogram::record(histogram, -0.0)
  
  UpDownCounter::add(up_down_counter, 0.0)
  UpDownCounter::add(up_down_counter, -0.0)
  
  Gauge::record(gauge, 0.0)
  Gauge::record(gauge, -0.0)
  
  // Test with very small values
  Counter::add(counter, 0.000001)
  Histogram::record(histogram, 0.000001)
  UpDownCounter::add(up_down_counter, 0.000001)
  Gauge::record(gauge, 0.000001)
  
  // Test with very large values
  Counter::add(counter, 999999999.999)
  Histogram::record(histogram, 999999999.999)
  UpDownCounter::add(up_down_counter, 999999999.999)
  Gauge::record(gauge, 999999999.999)
  
  // All operations should complete without errors
  @assertion.assert_true(true)?
}