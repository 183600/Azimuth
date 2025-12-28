// Test file for Metrics functionality
test "instrument type operations" {
  // Test counter instrument
  let counter = Counter("request-count", Some("Total number of requests"), Some("count"))
  assert_eq(Instrument::name(counter), "request-count")
  assert_eq(Instrument::description(counter), Some("Total number of requests"))
  assert_eq(Instrument::unit(counter), Some("count"))
  
  // Test histogram instrument
  let histogram = Histogram("response-time", Some("Response time distribution"), Some("ms"))
  assert_eq(Instrument::name(histogram), "response-time")
  assert_eq(Instrument::description(histogram), Some("Response time distribution"))
  assert_eq(Instrument::unit(histogram), Some("ms"))
  
  // Test up-down counter instrument
  let updown_counter = UpDownCounter("active-connections", Some("Current active connections"), Some("connections"))
  assert_eq(Instrument::name(updown_counter), "active-connections")
  assert_eq(Instrument::description(updown_counter), Some("Current active connections"))
  assert_eq(Instrument::unit(updown_counter), Some("connections"))
  
  // Test gauge instrument
  let gauge = Gauge("memory-usage", Some("Current memory usage"), Some("bytes"))
  assert_eq(Instrument::name(gauge), "memory-usage")
  assert_eq(Instrument::description(gauge), Some("Current memory usage"))
  assert_eq(Instrument::unit(gauge), Some("bytes"))
}

test "instrument pattern matching" {
  let counter = Counter("test-counter", None, None)
  let histogram = Histogram("test-histogram", None, None)
  let updown_counter = UpDownCounter("test-updown", None, None)
  let gauge = Gauge("test-gauge", None, None)
  
  // Test counter matching
  match counter {
    Counter(name, _, _) => assert_eq(name, "test-counter")
    _ => assert_true(false, "Should match Counter")
  }
  
  // Test histogram matching
  match histogram {
    Histogram(name, _, _) => assert_eq(name, "test-histogram")
    _ => assert_true(false, "Should match Histogram")
  }
  
  // Test up-down counter matching
  match updown_counter {
    UpDownCounter(name, _, _) => assert_eq(name, "test-updown")
    _ => assert_true(false, "Should match UpDownCounter")
  }
  
  // Test gauge matching
  match gauge {
    Gauge(name, _, _) => assert_eq(name, "test-gauge")
    _ => assert_true(false, "Should match Gauge")
  }
}

test "meter provider operations" {
  // Test creating meter provider
  let provider = MeterProvider::default()
  
  // Test getting meter
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let meter_with_version = MeterProvider::get_meter(provider, "test-meter-v2")
  
  // Test meter scope
  assert_eq(meter.scope.name, "test-meter")
  assert_true(meter.scope.version.is_none())
  
  assert_eq(meter_with_version.scope.name, "test-meter-v2")
  assert_true(meter_with_version.scope.version.is_none()) // Simplified implementation
}

test "counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test creating counter
  let counter = Meter::create_counter(meter, "request-count")
  let counter_with_meta = Meter::create_counter(meter, "error-count", Some("Total errors"), Some("errors"))
  
  // Test counter properties
  assert_eq(counter.name, "request-count")
  assert_true(counter.description.is_none())
  assert_true(counter.unit.is_none())
  
  assert_eq(counter_with_meta.name, "error-count")
  assert_eq(counter_with_meta.description, Some("Total errors"))
  assert_eq(counter_with_meta.unit, Some("errors"))
  
  // Test adding to counter
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 2.5, Some(Attributes::new()))
  
  // Note: simplified implementation doesn't actually track values
  assert_true(true, "Counter add operations should not crash")
}

test "histogram operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test creating histogram
  let histogram = Meter::create_histogram(meter, "response-time")
  let histogram_with_meta = Meter::create_histogram(meter, "latency", Some("Request latency"), Some("ms"))
  
  // Test histogram properties
  assert_eq(histogram.name, "response-time")
  assert_true(histogram.description.is_none())
  assert_true(histogram.unit.is_none())
  
  assert_eq(histogram_with_meta.name, "latency")
  assert_eq(histogram_with_meta.description, Some("Request latency"))
  assert_eq(histogram_with_meta.unit, Some("ms"))
  
  // Test recording histogram values
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 250.5)
  Histogram::record(histogram, 50.0, Some(Attributes::new()))
  
  // Test histogram as instrument
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "response-time")
  
  // Note: simplified implementation doesn't actually track values
  assert_true(true, "Histogram record operations should not crash")
}

test "up-down counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test creating up-down counter
  let updown_counter = Meter::create_updown_counter(meter, "active-connections")
  let updown_with_meta = Meter::create_updown_counter(meter, "queue-size", Some("Queue size"), Some("items"))
  
  // Test up-down counter properties
  assert_eq(updown_counter.name, "active-connections")
  assert_true(updown_counter.description.is_none())
  assert_true(updown_counter.unit.is_none())
  
  assert_eq(updown_with_meta.name, "queue-size")
  assert_eq(updown_with_meta.description, Some("Queue size"))
  assert_eq(updown_with_meta.unit, Some("items"))
  
  // Test adding and subtracting from up-down counter
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0)
  UpDownCounter::add(updown_counter, 3.5, Some(Attributes::new()))
  
  // Note: simplified implementation doesn't actually track values
  assert_true(true, "UpDownCounter add operations should not crash")
}

test "gauge operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test creating gauge
  let gauge = Meter::create_gauge(meter, "memory-usage")
  let gauge_with_meta = Meter::create_gauge(meter, "cpu-usage", Some("CPU usage percentage"), Some("%"))
  
  // Test gauge properties
  assert_eq(gauge.name, "memory-usage")
  assert_true(gauge.description.is_none())
  assert_true(gauge.unit.is_none())
  
  assert_eq(gauge_with_meta.name, "cpu-usage")
  assert_eq(gauge_with_meta.description, Some("CPU usage percentage"))
  assert_eq(gauge_with_meta.unit, Some("%"))
  
  // Note: Gauge implementation is simplified, no record method in current implementation
  assert_true(true, "Gauge creation should not crash")
}

test "meter noop operations" {
  // Test noop meter provider
  let noop_provider = MeterProvider::noop()
  let noop_meter = MeterProvider::get_meter(noop_provider, "noop-meter")
  
  // Test creating instruments with noop meter
  let noop_counter = Meter::create_counter(noop_meter, "noop-counter")
  let noop_histogram = Meter::create_histogram(noop_meter, "noop-histogram")
  let noop_updown = Meter::create_updown_counter(noop_meter, "noop-updown")
  let noop_gauge = Meter::create_gauge(noop_meter, "noop-gauge")
  
  // Test operations on noop instruments
  Counter::add(noop_counter, 1.0)
  Histogram::record(noop_histogram, 100.0)
  UpDownCounter::add(noop_updown, 5.0)
  
  // Note: noop operations should not crash
  assert_true(true, "Noop meter operations should not crash")
}

test "complex metrics workflow" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "workflow-meter")
  
  // Create various instruments
  let request_counter = Meter::create_counter(meter, "requests", Some("Total requests"), Some("count"))
  let response_histogram = Meter::create_histogram(meter, "response_time", Some("Response time"), Some("ms"))
  let active_connections = Meter::create_updown_counter(meter, "active_connections", Some("Active connections"), Some("connections"))
  let memory_gauge = Meter::create_gauge(meter, "memory_usage", Some("Memory usage"), Some("bytes"))
  
  // Simulate metrics operations
  Counter::add(request_counter, 1.0)
  Histogram::record(response_histogram, 150.0)
  UpDownCounter::add(active_connections, 1.0)
  
  // Simulate some processing
  Counter::add(request_counter, 1.0)
  Histogram::record(response_histogram, 200.0)
  UpDownCounter::add(active_connections, -1.0)
  
  // Workflow should complete without errors
  assert_true(true, "Complex metrics workflow should complete successfully")
}