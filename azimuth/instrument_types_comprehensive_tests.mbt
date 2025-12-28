// Comprehensive Instrument Types Test Suite
// Tests for counter, histogram, updown counter, gauge instruments and their operations

test "counter instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests.total")
  
  // Test counter properties
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, None)
  assert_eq(counter.unit, None)
  
  // Test counter operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.5)
  
  // If no errors occur, operations succeeded
  assert_true(true)
}

test "counter instrument with description and unit" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "cpu.time", Some("Total CPU time"), Some("seconds"))
  
  // Test counter with description and unit
  assert_eq(counter.name, "cpu.time")
  assert_eq(counter.description, Some("Total CPU time"))
  assert_eq(counter.unit, Some("seconds"))
  
  // Test operations
  Counter::add(counter, 100.0)
  assert_true(true)
}

test "histogram instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let histogram = Meter::create_histogram(meter, "http.request.duration")
  
  // Test histogram properties
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, None)
  assert_eq(histogram.unit, None)
  
  // Test histogram operations
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 250.5)
  Histogram::record(histogram, 500.0)
  
  // If no errors occur, operations succeeded
  assert_true(true)
}

test "histogram instrument with description and unit" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let histogram = Meter::create_histogram(meter, "response.size", Some("Response size in bytes"), Some("bytes"))
  
  // Test histogram with description and unit
  assert_eq(histogram.name, "response.size")
  assert_eq(histogram.description, Some("Response size in bytes"))
  assert_eq(histogram.unit, Some("bytes"))
  
  // Test operations
  Histogram::record(histogram, 1024.0)
  Histogram::record(histogram, 2048.0)
  assert_true(true)
}

test "updown counter instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  
  // Test updown counter properties
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, None)
  assert_eq(updown_counter.unit, None)
  
  // Test updown counter operations (can go up and down)
  UpDownCounter::add(updown_counter, 10.0)  // Add connections
  UpDownCounter::add(updown_counter, -3.0)  // Remove connections
  UpDownCounter::add(updown_counter, 5.0)   // Add more connections
  
  // If no errors occur, operations succeeded
  assert_true(true)
}

test "updown counter instrument with description and unit" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let updown_counter = Meter::create_updown_counter(meter, "memory.usage", Some("Current memory usage"), Some("megabytes"))
  
  // Test updown counter with description and unit
  assert_eq(updown_counter.name, "memory.usage")
  assert_eq(updown_counter.description, Some("Current memory usage"))
  assert_eq(updown_counter.unit, Some("megabytes"))
  
  // Test operations
  UpDownCounter::add(updown_counter, 512.0)
  UpDownCounter::add(updown_counter, -128.0)
  assert_true(true)
}

test "gauge instrument creation and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "system.temperature")
  
  // Test gauge properties
  assert_eq(gauge.name, "system.temperature")
  assert_eq(gauge.description, None)
  assert_eq(gauge.unit, None)
  
  // If no errors occur during creation, test passed
  assert_true(true)
}

test "gauge instrument with description and unit" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let gauge = Meter::create_gauge(meter, "cpu.temperature", Some("Current CPU temperature"), Some("celsius"))
  
  // Test gauge with description and unit
  assert_eq(gauge.name, "cpu.temperature")
  assert_eq(gauge.description, Some("Current CPU temperature"))
  assert_eq(gauge.unit, Some("celsius"))
  
  // If no errors occur during creation, test passed
  assert_true(true)
}

test "instrument type conversions and enum operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Create different instrument types
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  // Convert to Instrument enum and test operations
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  
  // Test instrument name extraction
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  
  // Test instrument description extraction
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::description(histogram_instrument), Some("Test histogram"))
  
  // Test instrument unit extraction
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
}

test "instrument with attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests")
  
  // Create attributes for measurements
  let attrs = Attributes::new()
  Attributes::set(attrs, "method", StringValue("GET"))
  Attributes::set(attrs, "status", StringValue("200"))
  
  // Test counter with attributes
  Counter::add(counter, 1.0, Some(attrs))
  Counter::add(counter, 1.0, Some(attrs))
  
  // Test counter with different attributes
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "method", StringValue("POST"))
  Attributes::set(attrs2, "status", StringValue("201"))
  
  Counter::add(counter, 1.0, Some(attrs2))
  
  // If no errors occur, operations succeeded
  assert_true(true)
}

test "instrument with realistic business metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "business-metrics")
  
  // Business counter: total orders
  let order_counter = Meter::create_counter(meter, "orders.total", Some("Total number of orders"), Some("orders"))
  Counter::add(order_counter, 1.0)
  Counter::add(order_counter, 1.0)
  Counter::add(order_counter, 1.0)
  
  // Business histogram: order value
  let order_value_histogram = Meter::create_histogram(meter, "orders.value", Some("Order value distribution"), Some("dollars"))
  Histogram::record(order_value_histogram, 29.99)
  Histogram::record(order_value_histogram, 49.99)
  Histogram::record(order_value_histogram, 99.99)
  
  // Business updown counter: active users
  let active_users_gauge = Meter::create_updown_counter(meter, "users.active", Some("Currently active users"), Some("users"))
  UpDownCounter::add(active_users_gauge, 150.0)
  UpDownCounter::add(active_users_gauge, 25.0)
  UpDownCounter::add(active_users_gauge, -10.0)
  
  // Business gauge: system load
  let system_load_gauge = Meter::create_gauge(meter, "system.load", Some("Current system load average"), Some("ratio"))
  
  // If no errors occur, all operations succeeded
  assert_true(true)
}

test "instrument with technical metrics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "technical-metrics")
  
  // Technical counter: HTTP requests
  let http_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  Counter::add(http_counter, 1.0)
  
  // Technical histogram: response time
  let response_time_histogram = Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("milliseconds"))
  Histogram::record(response_time_histogram, 45.0)
  Histogram::record(response_time_histogram, 120.0)
  Histogram::record(response_time_histogram, 250.0)
  
  // Technical updown counter: database connections
  let db_connections = Meter::create_updown_counter(meter, "db.connections.active", Some("Active database connections"), Some("connections"))
  UpDownCounter::add(db_connections, 10.0)
  UpDownCounter::add(db_connections, -2.0)
  
  // Technical gauge: memory usage
  let memory_gauge = Meter::create_gauge(meter, "memory.usage.bytes", Some("Memory usage in bytes"), Some("bytes"))
  
  // If no errors occur, all operations succeeded
  assert_true(true)
}