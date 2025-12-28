// Advanced Metrics Tests for UpDownCounter and Gauge Instruments
// Comprehensive testing of advanced metric scenarios and edge cases

test "updown_counter_comprehensive_operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown_counter.test")
  let updown_counter = Meter::create_updown_counter(
    meter, 
    "active.connections", 
    Some("Currently active connections"), 
    Some("connections")
  )
  
  // Test basic properties
  assert_eq(updown_counter.name, "active.connections")
  match updown_counter.description {
    Some(desc) => assert_eq(desc, "Currently active connections")
    _ => assert_true(false)
  }
  match updown_counter.unit {
    Some(unit) => assert_eq(unit, "connections")
    _ => assert_true(false)
  }
  
  // Test positive increments
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, 5.0)
  
  // Test negative decrements
  UpDownCounter::add(updown_counter, -3.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Test zero value
  UpDownCounter::add(updown_counter, 0.0)
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "connection.type", StringValue("websocket"))
  UpDownCounter::add(updown_counter, 7.0, Some(attrs))
  
  // Test large values
  UpDownCounter::add(updown_counter, 1000000.0)
  UpDownCounter::add(updown_counter, -1000000.0)
  
  // Test fractional values
  UpDownCounter::add(updown_counter, 3.14159)
  UpDownCounter::add(updown_counter, -2.71828)
  
  // Test as instrument
  let instrument = UpDownCounter(updown_counter.name, updown_counter.description, updown_counter.unit)
  assert_eq(Instrument::name(instrument), "active.connections")
  match Instrument::description(instrument) {
    Some(desc) => assert_eq(desc, "Currently active connections")
    _ => assert_true(false)
  }
  match Instrument::unit(instrument) {
    Some(unit) => assert_eq(unit, "connections")
    _ => assert_true(false)
  }
}

test "gauge_comprehensive_operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge.test")
  let gauge = Meter::create_gauge(
    meter, 
    "memory.usage", 
    Some("Current memory usage percentage"), 
    Some("percent")
  )
  
  // Test basic properties
  assert_eq(gauge.name, "memory.usage")
  match gauge.description {
    Some(desc) => assert_eq(desc, "Current memory usage percentage")
    _ => assert_true(false)
  }
  match gauge.unit {
    Some(unit) => assert_eq(unit, "percent")
    _ => assert_true(false)
  }
  
  // Test setting different values
  // Note: Gauges typically observe current values rather than accumulating
  // In a real implementation, this would set the current value
  UpDownCounter::add(gauge, 45.5)  // Using add method for test compatibility
  UpDownCounter::add(gauge, 78.2)
  UpDownCounter::add(gauge, 12.8)
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "memory.type", StringValue("heap"))
  UpDownCounter::add(gauge, 67.3, Some(attrs))
  
  let cache_attrs = Attributes::new()
  Attributes::set(cache_attrs, "memory.type", StringValue("cache"))
  UpDownCounter::add(gauge, 23.7, Some(cache_attrs))
  
  // Test extreme values
  UpDownCounter::add(gauge, 0.0)
  UpDownCounter::add(gauge, 100.0)
  UpDownCounter::add(gauge, 0.1)
  UpDownCounter::add(gauge, 99.9)
  
  // Test as instrument
  let instrument = Gauge(gauge.name, gauge.description, gauge.unit)
  assert_eq(Instrument::name(instrument), "memory.usage")
  match Instrument::description(instrument) {
    Some(desc) => assert_eq(desc, "Current memory usage percentage")
    _ => assert_true(false)
  }
  match Instrument::unit(instrument) {
    Some(unit) => assert_eq(unit, "percent")
    _ => assert_true(false)
  }
}

test "multiple_updown_counters_with_different_attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi_counter.test")
  
  // Create multiple UpDownCounters for different scenarios
  let active_connections = Meter::create_updown_counter(
    meter, 
    "active.connections", 
    Some("Active network connections"), 
    Some("connections")
  )
  
  let queue_size = Meter::create_updown_counter(
    meter, 
    "queue.size", 
    Some("Current queue size"), 
    Some("items")
  )
  
  let buffer_usage = Meter::create_updown_counter(
    meter, 
    "buffer.usage", 
    Some("Buffer usage in bytes"), 
    Some("bytes")
  )
  
  // Simulate connection lifecycle
  UpDownCounter::add(active_connections, 5.0)  // 5 new connections
  UpDownCounter::add(active_connections, 3.0)  // 3 more connections
  UpDownCounter::add(active_connections, -2.0) // 2 connections closed
  
  // Simulate queue operations
  UpDownCounter::add(queue_size, 10.0)  // 10 items queued
  UpDownCounter::add(queue_size, -3.0)  // 3 items processed
  UpDownCounter::add(queue_size, 7.0)   // 7 more items queued
  
  // Simulate buffer usage
  UpDownCounter::add(buffer_usage, 1024.0)     // 1KB allocated
  UpDownCounter::add(buffer_usage, 2048.0)     // 2KB more allocated
  UpDownCounter::add(buffer_usage, -512.0)     // 512 bytes freed
  
  // Test with different attributes for each counter
  let connection_attrs = Attributes::new()
  Attributes::set(connection_attrs, "protocol", StringValue("http"))
  UpDownCounter::add(active_connections, 2.0, Some(connection_attrs))
  
  let queue_attrs = Attributes::new()
  Attributes::set(queue_attrs, "priority", StringValue("high"))
  UpDownCounter::add(queue_size, 5.0, Some(queue_attrs))
  
  let buffer_attrs = Attributes::new()
  Attributes::set(buffer_attrs, "type", StringValue("memory"))
  UpDownCounter::add(buffer_usage, 256.0, Some(buffer_attrs))
}

test "multiple_gauges_monitoring_different_metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi_gauge.test")
  
  // Create multiple Gauges for different system metrics
  let cpu_temperature = Meter::create_gauge(
    meter, 
    "cpu.temperature", 
    Some("CPU temperature in Celsius"), 
    Some("celsius")
  )
  
  let disk_usage = Meter::create_gauge(
    meter, 
    "disk.usage", 
    Some("Disk usage percentage"), 
    Some("percent")
  )
  
  let network_latency = Meter::create_gauge(
    meter, 
    "network.latency", 
    Some("Network latency in milliseconds"), 
    Some("ms")
  )
  
  // Simulate metric observations
  UpDownCounter::add(cpu_temperature, 45.0)   // 45°C
  UpDownCounter::add(cpu_temperature, 52.0)   // Increased to 52°C
  UpDownCounter::add(cpu_temperature, 48.0)   // Cooled to 48°C
  
  UpDownCounter::add(disk_usage, 67.5)        // 67.5% used
  UpDownCounter::add(disk_usage, 68.2)        // Increased to 68.2%
  UpDownCounter::add(disk_usage, 66.8)        // Decreased to 66.8%
  
  UpDownCounter::add(network_latency, 12.5)   // 12.5ms
  UpDownCounter::add(network_latency, 25.0)   // Increased to 25ms
  UpDownCounter::add(network_latency, 8.3)    // Decreased to 8.3ms
  
  // Test with attributes for each gauge
  let cpu_attrs = Attributes::new()
  Attributes::set(cpu_attrs, "core", StringValue("0"))
  UpDownCounter::add(cpu_temperature, 50.0, Some(cpu_attrs))
  
  let disk_attrs = Attributes::new()
  Attributes::set(disk_attrs, "mount_point", StringValue("/"))
  UpDownCounter::add(disk_usage, 70.0, Some(disk_attrs))
  
  let network_attrs = Attributes::new()
  Attributes::set(network_attrs, "destination", StringValue("api.example.com"))
  UpDownCounter::add(network_latency, 15.0, Some(network_attrs))
}

test "metric_instrument_type_validation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "type_validation.test")
  
  // Create all instrument types
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test updown"), Some("items"))
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  
  // Create instrument representations
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  let updown_instrument = UpDownCounter(updown_counter.name, updown_counter.description, updown_counter.unit)
  let gauge_instrument = Gauge(gauge.name, gauge.description, gauge.unit)
  
  // Test instrument names
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  assert_eq(Instrument::name(updown_instrument), "test.updown")
  assert_eq(Instrument::name(gauge_instrument), "test.gauge")
  
  // Test instrument descriptions
  match Instrument::description(counter_instrument) {
    Some(desc) => assert_eq(desc, "Test counter")
    _ => assert_true(false)
  }
  
  match Instrument::description(histogram_instrument) {
    Some(desc) => assert_eq(desc, "Test histogram")
    _ => assert_true(false)
  }
  
  match Instrument::description(updown_instrument) {
    Some(desc) => assert_eq(desc, "Test updown")
    _ => assert_true(false)
  }
  
  match Instrument::description(gauge_instrument) {
    Some(desc) => assert_eq(desc, "Test gauge")
    _ => assert_true(false)
  }
  
  // Test instrument units
  match Instrument::unit(counter_instrument) {
    Some(unit) => assert_eq(unit, "count")
    _ => assert_true(false)
  }
  
  match Instrument::unit(histogram_instrument) {
    Some(unit) => assert_eq(unit, "ms")
    _ => assert_true(false)
  }
  
  match Instrument::unit(updown_instrument) {
    Some(unit) => assert_eq(unit, "items")
    _ => assert_true(false)
  }
  
  match Instrument::unit(gauge_instrument) {
    Some(unit) => assert_eq(unit, "percent")
    _ => assert_true(false)
  }
}