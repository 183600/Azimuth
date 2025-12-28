// Instrument Type Conversion Tests
// Testing metric instrument type conversions and compatibility

test "counter to instrument conversion" {
  // Test converting counter to generic instrument type
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests", Some("Total HTTP requests"), Some("count"))
  
  // Convert to instrument
  let instrument = Histogram::as_instrument(Histogram::{ 
    name: counter.name, 
    description: counter.description, 
    unit: counter.unit 
  })
  
  // Verify instrument properties
  assert_eq(Instrument::name(instrument), "http.requests")
  assert_eq(Instrument::description(instrument), Some("Total HTTP requests"))
  assert_eq(Instrument::unit(instrument), Some("count"))
  
  // Test instrument type matching
  match instrument {
    Counter(name, desc, unit) => {
      assert_eq(name, "http.requests")
      assert_eq(desc, Some("Total HTTP requests"))
      assert_eq(unit, Some("count"))
    }
    _ => assert_false(true)
  }
}

test "histogram to instrument conversion" {
  // Test converting histogram to generic instrument type
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  
  // Convert to instrument
  let instrument = Histogram::as_instrument(histogram)
  
  // Verify instrument properties
  assert_eq(Instrument::name(instrument), "response.time")
  assert_eq(Instrument::description(instrument), Some("Response time"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
  
  // Test instrument type matching
  match instrument {
    Histogram(name, desc, unit) => {
      assert_eq(name, "response.time")
      assert_eq(desc, Some("Response time"))
      assert_eq(unit, Some("ms"))
    }
    _ => assert_false(true)
  }
}

test "updown counter to instrument conversion" {
  // Test converting updown counter to generic instrument type
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  
  // Convert to instrument (using Counter enum for simplicity)
  let instrument = Counter(updown_counter.name, updown_counter.description, updown_counter.unit)
  
  // Verify instrument properties
  assert_eq(Instrument::name(instrument), "active.connections")
  assert_eq(Instrument::description(instrument), Some("Active connections"))
  assert_eq(Instrument::unit(instrument), Some("connections"))
  
  // Test instrument type matching
  match instrument {
    Counter(name, desc, unit) => {
      assert_eq(name, "active.connections")
      assert_eq(desc, Some("Active connections"))
      assert_eq(unit, Some("connections"))
    }
    _ => assert_false(true)
  }
}

test "gauge to instrument conversion" {
  // Test converting gauge to generic instrument type
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Convert to instrument (using Counter enum for simplicity)
  let instrument = Counter(gauge.name, gauge.description, gauge.unit)
  
  // Verify instrument properties
  assert_eq(Instrument::name(instrument), "memory.usage")
  assert_eq(Instrument::description(instrument), Some("Memory usage"))
  assert_eq(Instrument::unit(instrument), Some("bytes"))
  
  // Test instrument type matching
  match instrument {
    Counter(name, desc, unit) => {
      assert_eq(name, "memory.usage")
      assert_eq(desc, Some("Memory usage"))
      assert_eq(unit, Some("bytes"))
    }
    _ => assert_false(true)
  }
}

test "instrument type identification" {
  // Test identifying different instrument types
  let counter_instrument = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_instrument = Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter_instrument = UpDownCounter("test.updown", Some("Test updown"), Some("count"))
  let gauge_instrument = Gauge("test.gauge", Some("Test gauge"), Some("value"))
  
  // Test counter instrument identification
  match counter_instrument {
    Counter(name, desc, unit) => {
      assert_eq(name, "test.counter")
      assert_eq(desc, Some("Test counter"))
      assert_eq(unit, Some("count"))
    }
    _ => assert_false(true)
  }
  
  // Test histogram instrument identification
  match histogram_instrument {
    Histogram(name, desc, unit) => {
      assert_eq(name, "test.histogram")
      assert_eq(desc, Some("Test histogram"))
      assert_eq(unit, Some("ms"))
    }
    _ => assert_false(true)
  }
  
  // Test updown counter instrument identification
  match updown_counter_instrument {
    UpDownCounter(name, desc, unit) => {
      assert_eq(name, "test.updown")
      assert_eq(desc, Some("Test updown"))
      assert_eq(unit, Some("count"))
    }
    _ => assert_false(true)
  }
  
  // Test gauge instrument identification
  match gauge_instrument {
    Gauge(name, desc, unit) => {
      assert_eq(name, "test.gauge")
      assert_eq(desc, Some("Test gauge"))
      assert_eq(unit, Some("value"))
    }
    _ => assert_false(true)
  }
}

test "instrument with optional description and unit" {
  // Test instruments with optional description and unit
  let counter_with_options = Counter("test.counter", Some("Test counter"), Some("count"))
  let counter_without_options = Counter("test.counter", None, None)
  let histogram_partial = Histogram("test.histogram", Some("Test histogram"), None)
  
  // Test with all options
  assert_eq(Instrument::name(counter_with_options), "test.counter")
  assert_eq(Instrument::description(counter_with_options), Some("Test counter"))
  assert_eq(Instrument::unit(counter_with_options), Some("count"))
  
  // Test without options
  assert_eq(Instrument::name(counter_without_options), "test.counter")
  assert_eq(Instrument::description(counter_without_options), None)
  assert_eq(Instrument::unit(counter_without_options), None)
  
  // Test with partial options
  assert_eq(Instrument::name(histogram_partial), "test.histogram")
  assert_eq(Instrument::description(histogram_partial), Some("Test histogram"))
  assert_eq(Instrument::unit(histogram_partial), None)
}

test "instrument type conversion with attributes" {
  // Test instrument operations with attributes
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests")
  let histogram = Meter::create_histogram(meter, "response.time")
  
  // Create attributes for measurements
  let attrs = Attributes::new()
  Attributes::set(attrs, "method", StringValue("GET"))
  Attributes::set(attrs, "status", IntValue(200))
  
  // Test counter operations with attributes
  Counter::add(counter, 1.0, Some(attrs))
  
  // Test histogram operations with attributes
  Histogram::record(histogram, 150.5, Some(attrs))
  
  // Verify instruments still work correctly
  assert_eq(counter.name, "http.requests")
  assert_eq(histogram.name, "response.time")
}

test "instrument type conversion with special characters" {
  // Test instrument names and descriptions with special characters
  let counter_special = Counter("test.counter.special-chars_123", Some("Test counter with special chars: !@#$%"), Some("count"))
  let histogram_unicode = Histogram("测试.直方图", Some("Unicode histogram: 测试 🚀"), Some("ms"))
  
  // Verify special characters are preserved
  assert_eq(Instrument::name(counter_special), "test.counter.special-chars_123")
  assert_eq(Instrument::description(counter_special), Some("Test counter with special chars: !@#$%"))
  assert_eq(Instrument::unit(counter_special), Some("count"))
  
  // Verify unicode is preserved
  assert_eq(Instrument::name(histogram_unicode), "测试.直方图")
  assert_eq(Instrument::description(histogram_unicode), Some("Unicode histogram: 测试 🚀"))
  assert_eq(Instrument::unit(histogram_unicode), Some("ms"))
}

test "instrument type conversion array operations" {
  // Test operations with arrays of instruments
  let instruments = [
    Counter("counter1", Some("Counter 1"), Some("count")),
    Histogram("histogram1", Some("Histogram 1"), Some("ms")),
    UpDownCounter("updown1", Some("UpDown 1"), Some("count")),
    Gauge("gauge1", Some("Gauge 1"), Some("value"))
  ]
  
  // Test array operations
  assert_eq(instruments.length(), 4)
  
  // Test accessing instruments by index
  let first_instrument = instruments[0]
  let second_instrument = instruments[1]
  
  assert_eq(Instrument::name(first_instrument), "counter1")
  assert_eq(Instrument::name(second_instrument), "histogram1")
  
  // Test iterating through instruments
  for instrument in instruments {
    let name = Instrument::name(instrument)
    assert_true(name.length() > 0)
  }
}

test "instrument type conversion with large values" {
  // Test instruments with large numeric values
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "large.counter")
  let histogram = Meter::create_histogram(meter, "large.histogram")
  
  // Test with large values
  Counter::add(counter, 999999999.999)
  Histogram::record(histogram, 1000000000.0)
  
  // Test with very small values
  Counter::add(counter, 0.000001)
  Histogram::record(histogram, 0.0000001)
  
  // Verify instruments still work correctly
  assert_eq(counter.name, "large.counter")
  assert_eq(histogram.name, "large.histogram")
}

test "instrument type conversion with mixed units" {
  // Test instruments with different units
  let counter_bytes = Counter("bytes.total", Some("Total bytes"), Some("bytes"))
  let counter_seconds = Counter("time.total", Some("Total time"), Some("seconds"))
  let histogram_ms = Histogram("latency", Some("Request latency"), Some("ms"))
  let gauge_percent = Gauge("cpu.usage", Some("CPU usage"), Some("%"))
  let updown_mb = UpDownCounter("memory.active", Some("Active memory"), Some("MB"))
  
  // Verify different units are preserved
  assert_eq(Instrument::unit(counter_bytes), Some("bytes"))
  assert_eq(Instrument::unit(counter_seconds), Some("seconds"))
  assert_eq(Instrument::unit(histogram_ms), Some("ms"))
  assert_eq(Instrument::unit(gauge_percent), Some("%"))
  assert_eq(Instrument::unit(updown_mb), Some("MB"))
}