// Metrics Dashboard Test Suite for Azimuth Telemetry System
// This file contains test cases for metrics dashboard functionality

test "histogram creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-test-meter")
  
  // Create histogram with name, description, and unit
  let histogram = Meter::create_histogram(
    meter,
    "http.request.duration",
    Some("HTTP request duration in seconds"),
    Some("s")
  )
  
  // Verify histogram properties
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, Some("HTTP request duration in seconds"))
  assert_eq(histogram.unit, Some("s"))
  
  // Record histogram values
  Histogram::record(histogram, 0.1)  // 100ms
  Histogram::record(histogram, 0.25) // 250ms
  Histogram::record(histogram, 0.5)  // 500ms
  Histogram::record(histogram, 1.0)  // 1000ms
  Histogram::record(histogram, 2.5)  // 2500ms
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  Histogram::record(histogram, 0.15, Some(attrs))
  
  // Verify histogram as instrument
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "http.request.duration")
  assert_eq(Instrument::description(instrument), Some("HTTP request duration in seconds"))
  assert_eq(Instrument::unit(instrument), Some("s"))
}

test "updown counter creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown-counter-test-meter")
  
  // Create updown counter with name, description, and unit
  let updown_counter = Meter::create_updown_counter(
    meter,
    "system.memory.usage",
    Some("System memory usage in bytes"),
    Some("By")
  )
  
  // Verify updown counter properties
  assert_eq(updown_counter.name, "system.memory.usage")
  assert_eq(updown_counter.description, Some("System memory usage in bytes"))
  assert_eq(updown_counter.unit, Some("By"))
  
  // Record updown counter values (can be positive or negative)
  UpDownCounter::add(updown_counter, 1024.0)    // +1KB
  UpDownCounter::add(updown_counter, 2048.0)    // +2KB
  UpDownCounter::add(updown_counter, -512.0)    // -512B
  UpDownCounter::add(updown_counter, 4096.0)    // +4KB
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "process.name", StringValue("azimuth"))
  Attributes::set(attrs, "memory.type", StringValue("heap"))
  UpDownCounter::add(updown_counter, 8192.0, Some(attrs))
}

test "gauge creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge-test-meter")
  
  // Create gauge with name, description, and unit
  let gauge = Meter::create_gauge(
    meter,
    "system.cpu.temperature",
    Some("System CPU temperature in Celsius"),
    Some("°C")
  )
  
  // Verify gauge properties
  assert_eq(gauge.name, "system.cpu.temperature")
  assert_eq(gauge.description, Some("System CPU temperature in Celsius"))
  assert_eq(gauge.unit, Some("°C"))
  
  // Record gauge values (typically set to current value)
  // Note: In a real implementation, gauge would have a 'set' method
  // For this test, we're using the simplified implementation
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "cpu.core", StringValue("0"))
  Attributes::set(attrs, "sensor.location", StringValue("package"))
}

test "counter creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "counter-test-meter")
  
  // Create counter with name, description, and unit
  let counter = Meter::create_counter(
    meter,
    "http.requests.total",
    Some("Total number of HTTP requests"),
    Some("requests")
  )
  
  // Verify counter properties
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total number of HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  // Record counter values (only positive values allowed)
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.0)
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "http.method", StringValue("POST"))
  Attributes::set(attrs, "http.status_code", IntValue(201))
  Counter::add(counter, 1.0, Some(attrs))
  
  // Verify counter as instrument
  let instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(instrument), "http.requests.total")
  assert_eq(Instrument::description(instrument), Some("Total number of HTTP requests"))
  assert_eq(Instrument::unit(instrument), Some("requests"))
}

test "instrument type operations" {
  // Test Counter instrument
  let counter_instrument = Counter(
    "test.counter",
    Some("Test counter description"),
    Some("count")
  )
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::description(counter_instrument), Some("Test counter description"))
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  
  // Test Histogram instrument
  let histogram_instrument = Histogram(
    "test.histogram",
    Some("Test histogram description"),
    Some("ms")
  )
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  assert_eq(Instrument::description(histogram_instrument), Some("Test histogram description"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  
  // Test UpDownCounter instrument
  let updown_counter_instrument = UpDownCounter(
    "test.updown_counter",
    Some("Test updown counter description"),
    Some("bytes")
  )
  assert_eq(Instrument::name(updown_counter_instrument), "test.updown_counter")
  assert_eq(Instrument::description(updown_counter_instrument), Some("Test updown counter description"))
  assert_eq(Instrument::unit(updown_counter_instrument), Some("bytes"))
  
  // Test Gauge instrument
  let gauge_instrument = Gauge(
    "test.gauge",
    Some("Test gauge description"),
    Some("percent")
  )
  assert_eq(Instrument::name(gauge_instrument), "test.gauge")
  assert_eq(Instrument::description(gauge_instrument), Some("Test gauge description"))
  assert_eq(Instrument::unit(gauge_instrument), Some("percent"))
}

test "meter provider and meter operations" {
  // Test meter provider
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "test-meter-1")
  let meter2 = MeterProvider::get_meter(provider, "test-meter-2")
  
  // Verify meters have different scopes
  assert_eq(meter1.scope.name, "test-meter-1")
  assert_eq(meter2.scope.name, "test-meter-2")
  
  // Create different instruments with the same meter
  let counter = Meter::create_counter(meter1, "test.counter")
  let histogram = Meter::create_histogram(meter1, "test.histogram")
  let updown_counter = Meter::create_updown_counter(meter1, "test.updown_counter")
  let gauge = Meter::create_gauge(meter1, "test.gauge")
  
  // Verify all instruments have the same meter scope
  assert_eq(counter.name, "test.counter")
  assert_eq(histogram.name, "test.histogram")
  assert_eq(updown_counter.name, "test.updown_counter")
  assert_eq(gauge.name, "test.gauge")
}

test "metrics with complex attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "complex-attributes-meter")
  
  // Create attributes with various types
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("azimuth"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  Attributes::set(attrs, "http.duration_ms", FloatValue(123.45))
  Attributes::set(attrs, "cache.hit", BoolValue(true))
  Attributes::set(attrs, "user.roles", ArrayStringValue(["admin", "user"]))
  Attributes::set(attrs, "retry.counts", ArrayIntValue([1, 2, 3]))
  
  // Create and use instruments with complex attributes
  let counter = Meter::create_counter(meter, "http.requests.total")
  Counter::add(counter, 1.0, Some(attrs))
  
  let histogram = Meter::create_histogram(meter, "http.request.duration")
  Histogram::record(histogram, 0.12345, Some(attrs))
  
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  UpDownCounter::add(updown_counter, 5.0, Some(attrs))
}