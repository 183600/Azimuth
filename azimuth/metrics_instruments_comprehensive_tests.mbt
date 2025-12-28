// Metrics Instruments Comprehensive Tests
// Tests for metrics instruments creation, operations, and management

test "counter instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  
  // Test counter properties
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  // Test counter operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.5)
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "method", StringValue("GET"))
  Counter::add(counter, 2.0, Some(attrs))
}

test "histogram instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  
  // Test histogram properties
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, Some("HTTP request duration"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test histogram operations
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 250.5)
  Histogram::record(histogram, 75.25)
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "status_code", IntValue(200))
  Histogram::record(histogram, 150.0, Some(attrs))
}

test "updown counter instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active network connections"), Some("connections"))
  
  // Test updown counter properties
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Active network connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // Test updown counter operations (increment and decrement)
  UpDownCounter::add(updown_counter, 5.0)  // Increment
  UpDownCounter::add(updown_counter, -2.0) // Decrement
  UpDownCounter::add(updown_counter, 10.0) // Increment again
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "connection.type", StringValue("websocket"))
  UpDownCounter::add(updown_counter, 3.0, Some(attrs))
}

test "gauge instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage in bytes"), Some("bytes"))
  
  // Test gauge properties
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Memory usage in bytes"))
  assert_eq(gauge.unit, Some("bytes"))
  
  // Test gauge operations (setting current values)
  UpDownCounter::add(gauge, 1024.0)  // Using same add operation for simplicity
  UpDownCounter::add(gauge, 512.0)   // Update to new value
  UpDownCounter::add(gauge, -256.0)  // Decrease value
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "memory.type", StringValue("heap"))
  UpDownCounter::add(gauge, 2048.0, Some(attrs))
}

test "instrument type enumeration and properties" {
  let counter_instrument = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_instrument = Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter_instrument = UpDownCounter("test.updown", Some("Test updown"), Some("items"))
  let gauge_instrument = Gauge("test.gauge", Some("Test gauge"), Some("percent"))
  
  // Test instrument name extraction
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  assert_eq(Instrument::name(updown_counter_instrument), "test.updown")
  assert_eq(Instrument::name(gauge_instrument), "test.gauge")
  
  // Test instrument description extraction
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::description(histogram_instrument), Some("Test histogram"))
  assert_eq(Instrument::description(updown_counter_instrument), Some("Test updown"))
  assert_eq(Instrument::description(gauge_instrument), Some("Test gauge"))
  
  // Test instrument unit extraction
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  assert_eq(Instrument::unit(updown_counter_instrument), Some("items"))
  assert_eq(Instrument::unit(gauge_instrument), Some("percent"))
}

test "meter provider and meter lifecycle" {
  let provider = MeterProvider::noop()
  let meter1 = MeterProvider::get_meter(provider, "service1")
  let meter2 = MeterProvider::get_meter(provider, "service2")
  
  // Test meter scope properties
  assert_eq(meter1.scope.name, "service1")
  assert_eq(meter2.scope.name, "service2")
  assert_eq(meter1.scope.version, None)
  assert_eq(meter2.scope.version, None)
  
  // Test creating multiple instruments from same meter
  let counter = Meter::create_counter(meter1, "events.total")
  let histogram = Meter::create_histogram(meter1, "event.duration")
  let updown_counter = Meter::create_updown_counter(meter1, "active.processes")
  let gauge = Meter::create_gauge(meter1, "cpu.usage")
  
  assert_eq(counter.name, "events.total")
  assert_eq(histogram.name, "event.duration")
  assert_eq(updown_counter.name, "active.processes")
  assert_eq(gauge.name, "cpu.usage")
}

test "metrics with complex attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "complex-test")
  let counter = Meter::create_counter(meter, "api.requests")
  
  // Test metrics with multiple attributes
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "method", StringValue("GET"))
  Attributes::set(attrs1, "endpoint", StringValue("/api/users"))
  Attributes::set(attrs1, "status_code", IntValue(200))
  Counter::add(counter, 10.0, Some(attrs1))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "method", StringValue("POST"))
  Attributes::set(attrs2, "endpoint", StringValue("/api/orders"))
  Attributes::set(attrs2, "status_code", IntValue(201))
  Attributes::set(attrs2, "user.type", StringValue("premium"))
  Counter::add(counter, 5.0, Some(attrs2))
  
  let attrs3 = Attributes::new()
  Attributes::set(attrs3, "method", StringValue("DELETE"))
  Attributes::set(attrs3, "endpoint", StringValue("/api/sessions"))
  Attributes::set(attrs3, "status_code", IntValue(404))
  Attributes::set(attrs3, "error.type", StringValue("not_found"))
  Counter::add(counter, 2.0, Some(attrs3))
}

test "histogram as instrument conversion" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "conversion-test")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("seconds"))
  
  // Test conversion to instrument type
  let instrument = Histogram::as_instrument(histogram)
  
  // Test that instrument properties are preserved
  assert_eq(Instrument::name(instrument), "response.time")
  assert_eq(Instrument::description(instrument), Some("Response time"))
  assert_eq(Instrument::unit(instrument), Some("seconds"))
  
  // Test that histogram operations still work
  Histogram::record(histogram, 0.1)   // 100ms
  Histogram::record(histogram, 0.25)  // 250ms
  Histogram::record(histogram, 0.5)   // 500ms
  Histogram::record(histogram, 1.0)   // 1s
  Histogram::record(histogram, 2.5)   // 2.5s
}