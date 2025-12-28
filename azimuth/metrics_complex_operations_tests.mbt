// Metrics Async Operations Test Suite for Azimuth Telemetry System
// Tests various metric types with complex scenarios and attributes

test "multiple counter operations with attributes" {
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
  Counter::add(request_counter, 10.0)
  Counter::add(error_counter, 2.0)
  Counter::add(success_counter, 8.0)
  
  // Add more values
  Counter::add(request_counter, 5.0)
  Counter::add(error_counter, 1.0)
  Counter::add(success_counter, 4.0)
  
  // Verify counters are still accessible
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(error_counter.name, "http.errors.total")
  assert_eq(success_counter.name, "http.success.total")
}

test "histogram with various measurement ranges" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-range-test")
  
  let response_time = Meter::create_histogram(
    meter, 
    "response.time.ms", 
    Some("Response time in milliseconds"), 
    Some("ms")
  )
  
  // Verify histogram properties
  assert_eq(response_time.name, "response.time.ms")
  assert_eq(response_time.description, Some("Response time in milliseconds"))
  assert_eq(response_time.unit, Some("ms"))
  
  // Record various response times
  Histogram::record(response_time, 10.5)    // Fast response
  Histogram::record(response_time, 150.2)   // Medium response
  Histogram::record(response_time, 500.8)   // Slow response
  Histogram::record(response_time, 1000.1)  // Very slow response
  Histogram::record(response_time, 0.1)     # Very fast response
  
  // Record edge cases
  Histogram::record(response_time, 0.0)     # Zero response time
  Histogram::record(response_time, 9999.9)  # Very large response time
  
  // Verify histogram properties after recordings
  assert_eq(response_time.name, "response.time.ms")
  assert_eq(response_time.description, Some("Response time in milliseconds"))
  assert_eq(response_time.unit, Some("ms"))
}

test "updown counter with increment and decrement" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown-test")
  
  let active_connections = Meter::create_updown_counter(
    meter, 
    "active.connections", 
    Some("Current number of active connections"), 
    Some("connections")
  )
  
  // Verify updown counter properties
  assert_eq(active_connections.name, "active.connections")
  assert_eq(active_connections.description, Some("Current number of active connections"))
  assert_eq(active_connections.unit, Some("connections"))
  
  // Simulate connection lifecycle
  UpDownCounter::add(active_connections, 5.0)  // 5 connections opened
  UpDownCounter::add(active_connections, 3.0)  // 3 more connections opened
  UpDownCounter::add(active_connections, -2.0) // 2 connections closed
  UpDownCounter::add(active_connections, 1.0)  // 1 more connection opened
  UpDownCounter::add(active_connections, -4.0) // 4 connections closed
  
  // Verify updown counter properties after operations
  assert_eq(active_connections.name, "active.connections")
  assert_eq(active_connections.description, Some("Current number of active connections"))
  assert_eq(active_connections.unit, Some("connections"))
}

test "gauge operations with different value types" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge-test")
  
  let memory_usage = Meter::create_gauge(
    meter, 
    "memory.usage.bytes", 
    Some("Current memory usage in bytes"), 
    Some("bytes")
  )
  
  let cpu_usage = Meter::create_gauge(
    meter, 
    "cpu.usage.percentage", 
    Some("Current CPU usage percentage"), 
    Some("%")
  )
  
  let disk_usage = Meter::create_gauge(
    meter, 
    "disk.usage.gigabytes", 
    Some("Current disk usage in GB"), 
    Some("GB")
  )
  
  // Verify gauge properties
  assert_eq(memory_usage.name, "memory.usage.bytes")
  assert_eq(memory_usage.description, Some("Current memory usage in bytes"))
  assert_eq(memory_usage.unit, Some("bytes"))
  
  assert_eq(cpu_usage.name, "cpu.usage.percentage")
  assert_eq(cpu_usage.description, Some("Current CPU usage percentage"))
  assert_eq(cpu_usage.unit, Some("%"))
  
  assert_eq(disk_usage.name, "disk.usage.gigabytes")
  assert_eq(disk_usage.description, Some("Current disk usage in GB"))
  assert_eq(disk_usage.unit, Some("GB"))
}

test "instrument conversion and type checking" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "instrument-conversion-test")
  
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // Convert to instrument type
  let counter_instrument = Counter::as_instrument(counter)
  let histogram_instrument = Histogram::as_instrument(histogram)
  
  // Test instrument properties
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  
  // Test instrument descriptions and units
  let counter_with_desc = Meter::create_counter(
    meter, 
    "detailed.counter", 
    Some("Detailed counter description"), 
    Some("count")
  )
  let histogram_with_desc = Meter::create_histogram(
    meter, 
    "detailed.histogram", 
    Some("Detailed histogram description"), 
    Some("ms")
  )
  
  let counter_instrument = Counter::as_instrument(counter_with_desc)
  let histogram_instrument = Histogram::as_instrument(histogram_with_desc)
  
  assert_eq(Instrument::name(counter_instrument), "detailed.counter")
  assert_eq(Instrument::description(counter_instrument), Some("Detailed counter description"))
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  
  assert_eq(Instrument::name(histogram_instrument), "detailed.histogram")
  assert_eq(Instrument::description(histogram_instrument), Some("Detailed histogram description"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
}

test "multiple meters with different scopes" {
  let provider = MeterProvider::default()
  
  // Create multiple meters with different scopes
  let http_meter = MeterProvider::get_meter(provider, "http-meter", Some("1.0.0"))
  let db_meter = MeterProvider::get_meter(provider, "db-meter", Some("2.1.0"))
  let cache_meter = MeterProvider::get_meter(provider, "cache-meter")
  
  // Create instruments in different meters
  let http_requests = Meter::create_counter(http_meter, "http.requests")
  let db_queries = Meter::create_counter(db_meter, "db.queries")
  let cache_hits = Meter::create_counter(cache_meter, "cache.hits")
  
  // Verify instrument names
  assert_eq(http_requests.name, "http.requests")
  assert_eq(db_queries.name, "db.queries")
  assert_eq(cache_hits.name, "cache.hits")
  
  // Record measurements
  Counter::add(http_requests, 100.0)
  Counter::add(db_queries, 50.0)
  Counter::add(cache_hits, 75.0)
  
  // Verify meter scopes
  assert_eq(http_meter.scope.name, "http-meter")
  assert_eq(http_meter.scope.version, Some("1.0.0"))
  
  assert_eq(db_meter.scope.name, "db-meter")
  assert_eq(db_meter.scope.version, Some("2.1.0"))
  
  assert_eq(cache_meter.scope.name, "cache-meter")
  assert_eq(cache_meter.scope.version, None)
}

test "complex metric scenarios with mixed operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "complex-metrics-test")
  
  // Create various instruments
  let request_counter = Meter::create_counter(meter, "requests.total")
  let response_histogram = Meter::create_histogram(meter, "response.time")
  let active_gauge = Meter::create_updown_counter(meter, "active.requests")
  let memory_gauge = Meter::create_gauge(meter, "memory.usage")
  
  // Simulate complex scenario
  Counter::add(request_counter, 1.0)      // New request
  UpDownCounter::add(active_gauge, 1.0)   // Request becomes active
  
  Histogram::record(response_histogram, 50.2)  // Response time recorded
  
  UpDownCounter::add(active_gauge, -1.0)  // Request completed
  Counter::add(request_counter, 1.0)      // Another request
  UpDownCounter::add(active_gauge, 1.0)   // Another active request
  
  Histogram::record(response_histogram, 75.8)  // Another response time
  UpDownCounter::add(active_gauge, -1.0)  // Second request completed
  
  // Verify all instruments are still functional
  assert_eq(request_counter.name, "requests.total")
  assert_eq(response_histogram.name, "response.time")
  assert_eq(active_gauge.name, "active.requests")
  assert_eq(memory_gauge.name, "memory.usage")
}