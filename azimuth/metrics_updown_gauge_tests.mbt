// Metrics UpDownCounter and Gauge Tests
// Tests for up-down counter and gauge instrument operations

test "updown counter creation and properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "active.users", Some("Current active users"), Some("users"))
  
  // Test updown counter properties
  assert_eq(updown_counter.name, "active.users")
  assert_eq(updown_counter.description, Some("Current active users"))
  assert_eq(updown_counter.unit, Some("users"))
}

test "updown counter add operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Test positive additions
  UpDownCounter::add(updown_counter, 1024.0)
  UpDownCounter::add(updown_counter, 2048.0)
  
  // Test negative additions (decrements)
  UpDownCounter::add(updown_counter, -512.0)
  UpDownCounter::add(updown_counter, -256.0)
  
  // Test zero addition
  UpDownCounter::add(updown_counter, 0.0)
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "region", StringValue("us-west"))
  UpDownCounter::add(updown_counter, 100.0, Some(attrs))
}

test "updown counter with attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "queue.size", Some("Queue size"), Some("items"))
  
  // Create multiple attribute sets
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "queue.name", StringValue("high-priority"))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "queue.name", StringValue("low-priority"))
  
  let attrs3 = Attributes::new()
  Attributes::set(attrs3, "queue.name", StringValue("batch"))
  Attributes::set(attrs3, "region", StringValue("us-east"))
  
  // Test operations with different attribute sets
  UpDownCounter::add(updown_counter, 5.0, Some(attrs1))
  UpDownCounter::add(updown_counter, 3.0, Some(attrs2))
  UpDownCounter::add(updown_counter, 10.0, Some(attrs3))
  
  // Test negative operations with attributes
  UpDownCounter::add(updown_counter, -2.0, Some(attrs1))
  UpDownCounter::add(updown_counter, -1.0, Some(attrs2))
}

test "gauge creation and properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "cpu.temperature", Some("CPU temperature"), Some("celsius"))
  
  // Test gauge properties
  assert_eq(gauge.name, "cpu.temperature")
  assert_eq(gauge.description, Some("CPU temperature"))
  assert_eq(gauge.unit, Some("celsius"))
}

test "gauge recording operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "memory.pressure", Some("Memory pressure"), Some("percentage"))
  
  // Test recording various values
  UpDownCounter::add(gauge, 25.5)  // Using same implementation as UpDownCounter
  UpDownCounter::add(gauge, 75.2)
  UpDownCounter::add(gauge, 50.0)
  UpDownCounter::add(gauge, 90.8)
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "host", StringValue("server-01"))
  UpDownCounter::add(gauge, 60.3, Some(attrs))
}

test "gauge with multiple attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "disk.usage", Some("Disk usage"), Some("percentage"))
  
  // Test with different attribute combinations
  let server1_attrs = Attributes::new()
  Attributes::set(server1_attrs, "server", StringValue("web-01"))
  Attributes::set(server1_attrs, "mount", StringValue("/"))
  
  let server2_attrs = Attributes::new()
  Attributes::set(server2_attrs, "server", StringValue("web-02"))
  Attributes::set(server2_attrs, "mount", StringValue("/"))
  
  let data_attrs = Attributes::new()
  Attributes::set(data_attrs, "server", StringValue("data-01"))
  Attributes::set(data_attrs, "mount", StringValue("/data"))
  
  // Record values for different servers and mounts
  UpDownCounter::add(gauge, 45.0, Some(server1_attrs))
  UpDownCounter::add(gauge, 67.0, Some(server2_attrs))
  UpDownCounter::add(gauge, 82.0, Some(data_attrs))
}

test "updown counter and gauge comparison" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "comparison-meter")
  
  // Create both instruments
  let counter = Meter::create_updown_counter(meter, "connections.active", Some("Active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "connections.total", Some("Total connections"), Some("connections"))
  
  // Test both have similar properties
  assert_eq(counter.name, "connections.active")
  assert_eq(gauge.name, "connections.total")
  assert_eq(counter.description, Some("Active connections"))
  assert_eq(gauge.description, Some("Total connections"))
  assert_eq(counter.unit, Some("connections"))
  assert_eq(gauge.unit, Some("connections"))
  
  // Test both support add operations
  UpDownCounter::add(counter, 10.0)
  UpDownCounter::add(gauge, 100.0)
  
  // Test both support attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "service", StringValue("api"))
  UpDownCounter::add(counter, 5.0, Some(attrs))
  UpDownCounter::add(gauge, 50.0, Some(attrs))
}

test "instrument edge cases and special values" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "edge-case-meter")
  
  let updown_counter = Meter::create_updown_counter(meter, "edge.case.counter", Some("Edge case counter"), None)
  let gauge = Meter::create_gauge(meter, "edge.case.gauge", Some("Edge case gauge"), None)
  
  // Test with special numeric values
  UpDownCounter::add(updown_counter, 0.0)
  UpDownCounter::add(updown_counter, -0.0)
  UpDownCounter::add(gauge, 0.0)
  
  // Test with very large values
  UpDownCounter::add(updown_counter, 999999.0)
  UpDownCounter::add(gauge, 1000000.0)
  
  // Test with very small values
  UpDownCounter::add(updown_counter, 0.001)
  UpDownCounter::add(gauge, 0.0001)
  
  // Test with negative values for both
  UpDownCounter::add(updown_counter, -100.0)
  UpDownCounter::add(gauge, -50.0)
}

test "instrument with no description or unit" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "minimal-meter")
  
  // Create instruments without description and unit
  let minimal_counter = Meter::create_updown_counter(meter, "minimal.counter", None, None)
  let minimal_gauge = Meter::create_gauge(meter, "minimal.gauge", None, None)
  
  // Test properties
  assert_eq(minimal_counter.name, "minimal.counter")
  assert_eq(minimal_gauge.name, "minimal.gauge")
  assert_eq(minimal_counter.description, None)
  assert_eq(minimal_gauge.description, None)
  assert_eq(minimal_counter.unit, None)
  assert_eq(minimal_gauge.unit, None)
  
  // Test operations still work
  UpDownCounter::add(minimal_counter, 42.0)
  UpDownCounter::add(minimal_gauge, 24.0)
}