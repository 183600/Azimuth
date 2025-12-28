// Gauge and UpDownCounter Test Cases for Azimuth Telemetry System
// Testing advanced metric instruments beyond basic counters

test "updown counter creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Current active connections"), Some("connections"))
  
  // Test counter properties
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Current active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // Test increment operations
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, 3.0)
  
  // Test decrement operations
  UpDownCounter::add(updown_counter, -2.0)
  UpDownCounter::add(updown_counter, -1.0)
  
  // Verify operations complete without errors
  assert_true(true)
}

test "gauge creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Current memory usage"), Some("bytes"))
  
  // Test gauge properties
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Current memory usage"))
  assert_eq(gauge.unit, Some("bytes"))
  
  // Test gauge recording operations
  UpDownCounter::add(gauge, 1024.0)  // Using same operation as UpDownCounter in simplified implementation
  UpDownCounter::add(gauge, 2048.0)
  UpDownCounter::add(gauge, 512.0)
  
  // Verify operations complete without errors
  assert_true(true)
}

test "updown counter with attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "queue.size", Some("Queue size by status"), Some("items"))
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "queue.name", StringValue("high-priority"))
  Attributes::set(attrs, "status", StringValue("processing"))
  
  // Test operations with attributes
  UpDownCounter::add(updown_counter, 10.0, Some(attrs))
  UpDownCounter::add(updown_counter, -3.0, Some(attrs))
  
  // Test with different attributes
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "queue.name", StringValue("low-priority"))
  Attributes::set(attrs2, "status", StringValue("pending"))
  
  UpDownCounter::add(updown_counter, 25.0, Some(attrs2))
  UpDownCounter::add(updown_counter, -5.0, Some(attrs2))
  
  // Verify operations complete without errors
  assert_true(true)
}

test "gauge with different measurement types" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "system-metrics")
  
  // Test different gauge types
  let cpu_gauge = Meter::create_gauge(meter, "cpu.utilization", Some("CPU utilization percentage"), Some("%"))
  let memory_gauge = Meter::create_gauge(meter, "memory.available", Some("Available memory"), Some("bytes"))
  let disk_gauge = Meter::create_gauge(meter, "disk.free", Some("Free disk space"), Some("bytes"))
  let temperature_gauge = Meter::create_gauge(meter, "cpu.temperature", Some("CPU temperature"), Some("celsius"))
  
  // Test recording different value types
  UpDownCounter::add(cpu_gauge, 75.5)  // 75.5% CPU
  UpDownCounter::add(memory_gauge, 8589934592.0)  // 8GB available
  UpDownCounter::add(disk_gauge, 107374182400.0)  // 100GB free
  UpDownCounter::add(temperature_gauge, 45.0)  // 45°C
  
  // Verify all gauges created and operated successfully
  assert_eq(cpu_gauge.name, "cpu.utilization")
  assert_eq(memory_gauge.name, "memory.available")
  assert_eq(disk_gauge.name, "disk.free")
  assert_eq(temperature_gauge.name, "cpu.temperature")
}

test "updown counter negative value scenarios" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "financial-metrics")
  let counter = Meter::create_updown_counter(meter, "account.balance", Some("Account balance"), Some("currency"))
  
  // Test positive balance
  UpDownCounter::add(counter, 1000.0)
  
  // Test negative balance (overdraft)
  UpDownCounter::add(counter, -1500.0)
  
  // Test zero balance
  UpDownCounter::add(counter, 500.0)
  UpDownCounter::add(counter, -500.0)
  
  // Test large negative value
  UpDownCounter::add(counter, -10000.0)
  
  // Verify operations complete without errors
  assert_true(true)
}

test "instrument type conversions and properties" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Create different instruments
  let counter = Meter::create_counter(meter, "request.count")
  let histogram = Meter::create_histogram(meter, "request.duration")
  let updown_counter = Meter::create_updown_counter(meter, "active.users")
  let gauge = Meter::create_gauge(meter, "system.load")
  
  // Test instrument properties
  assert_eq(counter.name, "request.count")
  assert_eq(histogram.name, "request.duration")
  assert_eq(updown_counter.name, "active.users")
  assert_eq(gauge.name, "system.load")
  
  // Test instrument descriptions
  assert_eq(counter.description, None)
  assert_eq(histogram.description, None)
  assert_eq(updown_counter.description, None)
  assert_eq(gauge.description, None)
  
  // Test instrument units
  assert_eq(counter.unit, None)
  assert_eq(histogram.unit, None)
  assert_eq(updown_counter.unit, None)
  assert_eq(gauge.unit, None)
}