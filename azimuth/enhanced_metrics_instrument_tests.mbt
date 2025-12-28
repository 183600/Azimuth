// Enhanced Metrics Instrument Combination Tests for Azimuth
// Tests various metric instruments and their combined operations

test "multiple counters with different attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi-counter-test")
  
  // Create multiple counters
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let error_counter = Meter::create_counter(meter, "http.errors.total")
  let success_counter = Meter::create_counter(meter, "http.success.total")
  
  // Verify counter names
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(error_counter.name, "http.errors.total")
  assert_eq(success_counter.name, "http.success.total")
  
  // Add values to counters
  Counter::add(request_counter, 100.0)
  Counter::add(error_counter, 5.0)
  Counter::add(success_counter, 95.0)
  
  // Add more values to test accumulation
  Counter::add(request_counter, 50.0)
  Counter::add(error_counter, 2.0)
  Counter::add(success_counter, 48.0)
  
  // Test with negative values (should be allowed for testing)
  Counter::add(error_counter, -1.0)
  Counter::add(success_counter, -3.0)
  
  // Test with zero values
  Counter::add(request_counter, 0.0)
  Counter::add(error_counter, 0.0)
  Counter::add(success_counter, 0.0)
  
  // Test with very large values
  Counter::add(request_counter, 1.7976931348623157e+308)
  Counter::add(error_counter, 1.7976931348623157e+308)
  Counter::add(success_counter, 1.7976931348623157e+308)
  
  // Verify counters are still functional
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(error_counter.name, "http.errors.total")
  assert_eq(success_counter.name, "http.success.total")
}

test "histogram operations with various value ranges" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-test")
  
  // Create histogram with description and unit
  let response_time_histogram = Meter::create_histogram(
    meter, 
    "http.response.time", 
    Some("HTTP response time in milliseconds"), 
    Some("ms")
  )
  let latency_histogram = Meter::create_histogram(
    meter, 
    "network.latency", 
    Some("Network latency in microseconds"), 
    Some("μs")
  )
  
  // Verify histogram properties
  assert_eq(response_time_histogram.name, "http.response.time")
  assert_eq(response_time_histogram.description, Some("HTTP response time in milliseconds"))
  assert_eq(response_time_histogram.unit, Some("ms"))
  
  assert_eq(latency_histogram.name, "network.latency")
  assert_eq(latency_histogram.description, Some("Network latency in microseconds"))
  assert_eq(latency_histogram.unit, Some("μs"))
  
  // Record various response times
  Histogram::record(response_time_histogram, 10.5)    // Fast response
  Histogram::record(response_time_histogram, 50.0)    // Medium response
  Histogram::record(response_time_histogram, 150.0)   // Slow response
  Histogram::record(response_time_histogram, 500.0)   // Very slow response
  Histogram::record(response_time_histogram, 1000.0)  # Timeout threshold
  
  // Record edge case values
  Histogram::record(response_time_histogram, 0.0)      # Minimum
  Histogram::record(response_time_histogram, -1.0)     # Negative (edge case)
  Histogram::record(response_time_histogram, 1.7976931348623157e+308)  # Max double
  
  // Record latency values
  Histogram::record(latency_histogram, 100.0)   # 100 microseconds
  Histogram::record(latency_histogram, 500.0)   # 500 microseconds
  Histogram::record(latency_histogram, 1000.0)  # 1 millisecond
  Histogram::record(latency_histogram, 5000.0)  # 5 milliseconds
  
  // Test histogram as instrument
  let response_time_instrument = Histogram::as_instrument(response_time_histogram)
  let latency_instrument = Histogram::as_instrument(latency_histogram)
  
  assert_eq(Instrument::name(response_time_instrument), "http.response.time")
  assert_eq(Instrument::description(response_time_instrument), Some("HTTP response time in milliseconds"))
  assert_eq(Instrument::unit(response_time_instrument), Some("ms"))
  
  assert_eq(Instrument::name(latency_instrument), "network.latency")
  assert_eq(Instrument::description(latency_instrument), Some("Network latency in microseconds"))
  assert_eq(Instrument::unit(latency_instrument), Some("μs"))
}

test "updown counter operations with positive and negative values" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown-test")
  
  // Create UpDownCounters for different metrics
  let active_connections = Meter::create_updown_counter(meter, "active.connections")
  let memory_usage = Meter::create_updown_counter(meter, "memory.usage")
  let queue_size = Meter::create_updown_counter(meter, "queue.size")
  
  // Verify UpDownCounter properties
  assert_eq(active_connections.name, "active.connections")
  assert_eq(memory_usage.name, "memory.usage")
  assert_eq(queue_size.name, "queue.size")
  
  // Simulate active connections: start at 0, add connections, remove connections
  UpDownCounter::add(active_connections, 10.0)  # 10 active connections
  UpDownCounter::add(active_connections, 5.0)   # 5 more connections
  UpDownCounter::add(active_connections, -3.0)  # 3 connections closed
  UpDownCounter::add(active_connections, -12.0) # 12 connections closed (should go negative)
  
  // Simulate memory usage: increase and decrease
  UpDownCounter::add(memory_usage, 1024.0)      # 1KB allocated
  UpDownCounter::add(memory_usage, 2048.0)      # 2KB more allocated
  UpDownCounter::add(memory_usage, -512.0)      # 512B freed
  UpDownCounter::add(memory_usage, -4096.0)     # 4KB freed (should go negative)
  
  // Simulate queue size: enqueue and dequeue operations
  UpDownCounter::add(queue_size, 100.0)         # 100 items enqueued
  UpDownCounter::add(queue_size, 50.0)          # 50 more items enqueued
  UpDownCounter::add(queue_size, -75.0)         # 75 items dequeued
  UpDownCounter::add(queue_size, -100.0)        # 100 more items dequeued (should go negative)
  
  // Test edge cases
  UpDownCounter::add(active_connections, 0.0)   # Zero change
  UpDownCounter::add(memory_usage, 0.0)         # Zero change
  UpDownCounter::add(queue_size, 0.0)           # Zero change
  
  # Test with very large values
  UpDownCounter::add(active_connections, 1.7976931348623157e+308)
  UpDownCounter::add(memory_usage, -1.7976931348623157e+308)
  UpDownCounter::add(queue_size, 1.7976931348623157e+308)
  
  # Verify UpDownCounters are still functional
  assert_eq(active_connections.name, "active.connections")
  assert_eq(memory_usage.name, "memory.usage")
  assert_eq(queue_size.name, "queue.size")
}

test "gauge operations with various scenarios" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge-test")
  
  # Create gauges for different metrics
  let cpu_usage = Meter::create_gauge(meter, "cpu.usage")
  let memory_usage = Meter::create_gauge(meter, "memory.usage")
  let disk_usage = Meter::create_gauge(meter, "disk.usage")
  
  # Create gauges with descriptions and units
  let temperature = Meter::create_gauge(
    meter, 
    "system.temperature", 
    Some("System temperature in Celsius"), 
    Some("°C")
  )
  let throughput = Meter::create_gauge(
    meter, 
    "network.throughput", 
    Some("Network throughput in Mbps"), 
    Some("Mbps")
  )
  
  # Verify gauge properties
  assert_eq(cpu_usage.name, "cpu.usage")
  assert_eq(memory_usage.name, "memory.usage")
  assert_eq(disk_usage.name, "disk.usage")
  assert_eq(temperature.name, "system.temperature")
  assert_eq(temperature.description, Some("System temperature in Celsius"))
  assert_eq(temperature.unit, Some("°C"))
  assert_eq(throughput.name, "network.throughput")
  assert_eq(throughput.description, Some("Network throughput in Mbps"))
  assert_eq(throughput.unit, Some("Mbps"))
  
  # Note: Gauge implementation in simplified version doesn't have explicit set operations
  # This test focuses on gauge creation and property verification
  
  # Test gauge as instrument (would need conversion in full implementation)
  # For simplified implementation, we focus on creation and properties
}

test "mixed instrument operations workflow" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "mixed-instruments-test")
  
  # Create different types of instruments for a web service scenario
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_histogram = Meter::create_histogram(meter, "http.response.duration")
  let active_connections = Meter::create_updown_counter(meter, "http.active.connections")
  let cpu_gauge = Meter::create_gauge(meter, "process.cpu.usage")
  
  # Simulate web service workflow
  # 1. Process incoming requests
  for i in range(0, 10) {
    Counter::add(request_counter, 1.0)
    UpDownCounter::add(active_connections, 1.0)  # Connection established
    
    # Simulate request processing time
    let processing_time = 50.0 + (i.to_double() * 10.0)  # Increasing processing times
    Histogram::record(response_histogram, processing_time)
    
    UpDownCounter::add(active_connections, -1.0)  # Connection closed
  }
  
  # 2. Handle error scenario
  Counter::add(request_counter, 1.0)
  UpDownCounter::add(active_connections, 1.0)
  Histogram::record(response_histogram, 5000.0)  # Slow response due to error
  UpDownCounter::add(active_connections, -1.0)
  
  # 3. Simulate connection pool management
  for i in range(0, 5) {
    UpDownCounter::add(active_connections, 1.0)  # Add connection to pool
  }
  
  for i in range(0, 3) {
    UpDownCounter::add(active_connections, -1.0)  # Remove connection from pool
  }
  
  # 4. Simulate burst traffic
  for i in range(0, 20) {
    Counter::add(request_counter, 1.0)
    let burst_processing_time = 25.0 + (i.to_double() * 2.5)
    Histogram::record(response_histogram, burst_processing_time)
  }
  
  # 5. Simulate resource cleanup
  UpDownCounter::add(active_connections, -2.0)  # Final cleanup
  
  # Verify all instruments are still accessible and functional
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(active_connections.name, "http.active.connections")
  assert_eq(cpu_gauge.name, "process.cpu.usage")
}

test "instrument creation with special names and attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "special-names-test")
  
  # Test instruments with very long names
  let long_counter_name = "very.long.instrument.name.".repeat(20) + "counter"
  let long_histogram_name = "very.long.instrument.name.".repeat(20) + "histogram"
  let long_updown_name = "very.long.instrument.name.".repeat(20) + "updown"
  let long_gauge_name = "very.long.instrument.name.".repeat(20) + "gauge"
  
  let long_counter = Meter::create_counter(meter, long_counter_name)
  let long_histogram = Meter::create_histogram(meter, long_histogram_name)
  let long_updown = Meter::create_updown_counter(meter, long_updown_name)
  let long_gauge = Meter::create_gauge(meter, long_gauge_name)
  
  assert_eq(long_counter.name, long_counter_name)
  assert_eq(long_histogram.name, long_histogram_name)
  assert_eq(long_updown.name, long_updown_name)
  assert_eq(long_gauge.name, long_gauge_name)
  
  # Test instruments with special characters
  let special_counter_name = "special@counter#name!@#$%^&*()"
  let special_histogram_name = "special@histogram#name!@#$%^&*()"
  let special_updown_name = "special@updown#name!@#$%^&*()"
  let special_gauge_name = "special@gauge#name!@#$%^&*()"
  
  let special_counter = Meter::create_counter(meter, special_counter_name)
  let special_histogram = Meter::create_histogram(meter, special_histogram_name)
  let special_updown = Meter::create_updown_counter(meter, special_updown_name)
  let special_gauge = Meter::create_gauge(meter, special_gauge_name)
  
  assert_eq(special_counter.name, special_counter_name)
  assert_eq(special_histogram.name, special_histogram_name)
  assert_eq(special_updown.name, special_updown_name)
  assert_eq(special_gauge.name, special_gauge_name)
  
  # Test instruments with unicode names
  let unicode_counter_name = "unicode-counter-名称-🚀"
  let unicode_histogram_name = "unicode-histogram-名称-🚀"
  let unicode_updown_name = "unicode-updown-名称-🚀"
  let unicode_gauge_name = "unicode-gauge-名称-🚀"
  
  let unicode_counter = Meter::create_counter(meter, unicode_counter_name)
  let unicode_histogram = Meter::create_histogram(meter, unicode_histogram_name)
  let unicode_updown = Meter::create_updown_counter(meter, unicode_updown_name)
  let unicode_gauge = Meter::create_gauge(meter, unicode_gauge_name)
  
  assert_eq(unicode_counter.name, unicode_counter_name)
  assert_eq(unicode_histogram.name, unicode_histogram_name)
  assert_eq(unicode_updown.name, unicode_updown_name)
  assert_eq(unicode_gauge.name, unicode_gauge_name)
  
  # Test operations with special named instruments
  Counter::add(special_counter, 1.0)
  Histogram::record(special_histogram, 100.0)
  UpDownCounter::add(special_updown, 5.0)
  UpDownCounter::add(special_updown, -2.0)
  
  Counter::add(unicode_counter, 10.0)
  Histogram::record(unicode_histogram, 50.5)
  UpDownCounter::add(unicode_updown, -3.0)
  UpDownCounter::add(unicode_updown, 7.0)
}

test "instrument type conversion and polymorphism" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "type-conversion-test")
  
  # Create instruments
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  # Convert to Instrument type
  let counter_instrument = Histogram::as_instrument(histogram)  # Reuse for counter
  let histogram_instrument = Histogram::as_instrument(histogram)
  
  # Test instrument properties
  assert_eq(Instrument::name(counter_instrument), "test.histogram")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  
  # Test instrument with description and unit
  let detailed_histogram = Meter::create_histogram(
    meter, 
    "detailed.histogram", 
    Some("Detailed histogram with unit"), 
    Some("units")
  )
  let detailed_instrument = Histogram::as_instrument(detailed_histogram)
  
  assert_eq(Instrument::name(detailed_instrument), "detailed.histogram")
  assert_eq(Instrument::description(detailed_instrument), Some("Detailed histogram with unit"))
  assert_eq(Instrument::unit(detailed_instrument), Some("units"))
  
  # Test instrument pattern matching (if supported)
  # This would be more comprehensive in a full implementation
  match detailed_instrument {
    Counter(name, desc, unit) => {
      assert_eq(name, "detailed.histogram")
      assert_eq(desc, Some("Detailed histogram with unit"))
      assert_eq(unit, Some("units"))
    }
    Histogram(name, desc, unit) => {
      assert_eq(name, "detailed.histogram")
      assert_eq(desc, Some("Detailed histogram with unit"))
      assert_eq(unit, Some("units"))
    }
    _ => assert_false(true)  # Should not reach here for histogram
  }
}