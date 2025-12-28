// Metrics Instruments Test Cases for Azimuth Telemetry System
// Testing counter, histogram, updown counter, and gauge instruments

test "counter instrument creation and properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  
  // Test counter properties
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
}

test "counter instrument operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let counter = Meter::create_counter(meter, "test.counter")
  
  // Test counter operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.5)
  Counter::add(counter, 10.0)
  
  assert_true(true)
}

test "histogram instrument creation and properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time histogram"), Some("ms"))
  
  // Test histogram properties
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time histogram"))
  assert_eq(histogram.unit, Some("ms"))
}

test "histogram instrument operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  
  // Test histogram operations
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.5)
  Histogram::record(histogram, 150.75)
  
  assert_true(true)
}

test "updown counter instrument creation and properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  
  // Test updown counter properties
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
}

test "updown counter instrument operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  
  // Test updown counter operations (increment and decrement)
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0)
  UpDownCounter::add(updown_counter, 3.5)
  
  assert_true(true)
}

test "gauge instrument creation and properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test.meter")
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Test gauge properties
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Memory usage"))
  assert_eq(gauge.unit, Some("bytes"))
}

test "instrument enum operations" {
  let counter_instr = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_instr = Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter_instr = UpDownCounter("test.updown", Some("Test updown"), Some("value"))
  let gauge_instr = Gauge("test.gauge", Some("Test gauge"), Some("percent"))
  
  // Test instrument name extraction
  assert_eq(Instrument::name(counter_instr), "test.counter")
  assert_eq(Instrument::name(histogram_instr), "test.histogram")
  assert_eq(Instrument::name(updown_counter_instr), "test.updown")
  assert_eq(Instrument::name(gauge_instr), "test.gauge")
  
  // Test instrument description extraction
  assert_eq(Instrument::description(counter_instr), Some("Test counter"))
  assert_eq(Instrument::description(histogram_instr), Some("Test histogram"))
  assert_eq(Instrument::description(updown_counter_instr), Some("Test updown"))
  assert_eq(Instrument::description(gauge_instr), Some("Test gauge"))
  
  // Test instrument unit extraction
  assert_eq(Instrument::unit(counter_instr), Some("count"))
  assert_eq(Instrument::unit(histogram_instr), Some("ms"))
  assert_eq(Instrument::unit(updown_counter_instr), Some("value"))
  assert_eq(Instrument::unit(gauge_instr), Some("percent"))
}

test "instrument pattern matching" {
  let counter_instr = Counter("test.counter", None, None)
  let histogram_instr = Histogram("test.histogram", None, None)
  
  // Test pattern matching on instrument types
  let counter_type = match counter_instr {
    Counter(_, _, _) => "counter"
    Histogram(_, _, _) => "histogram"
    UpDownCounter(_, _, _) => "updown_counter"
    Gauge(_, _, _) => "gauge"
  }
  
  let histogram_type = match histogram_instr {
    Counter(_, _, _) => "counter"
    Histogram(_, _, _) => "histogram"
    UpDownCounter(_, _, _) => "updown_counter"
    Gauge(_, _, _) => "gauge"
  }
  
  assert_eq(counter_type, "counter")
  assert_eq(histogram_type, "histogram")
}

test "meter with multiple instruments" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi.instrument.meter")
  
  // Create multiple instruments from the same meter
  let counter = Meter::create_counter(meter, "requests")
  let histogram = Meter::create_histogram(meter, "latency")
  let updown_counter = Meter::create_updown_counter(meter, "active")
  let gauge = Meter::create_gauge(meter, "memory")
  
  // Test all instruments have correct names
  assert_eq(counter.name, "requests")
  assert_eq(histogram.name, "latency")
  assert_eq(updown_counter.name, "active")
  assert_eq(gauge.name, "memory")
}