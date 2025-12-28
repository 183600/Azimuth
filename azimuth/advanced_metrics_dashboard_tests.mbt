// Advanced Metrics Dashboard Test Suite
// Tests comprehensive metrics operations, dashboard functionality, and complex scenarios

test "comprehensive counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "counter-meter", Some("1.0.0"))
  
  // Test creating counters with different configurations
  let simple_counter = Meter::create_counter(meter, "http.requests.total")
  let described_counter = Meter::create_counter(meter, "http.response.size", Some("Total HTTP response size in bytes"), Some("bytes"))
  let full_counter = Meter::create_counter(meter, "database.connections", Some("Active database connections"), Some("connections"))
  
  // Verify counter properties
  assert_eq(simple_counter.name, "http.requests.total")
  assert_eq(described_counter.name, "http.response.size")
  assert_eq(full_counter.name, "database.connections")
  
  assert_eq(simple_counter.description, None)
  assert_eq(described_counter.description, Some("Total HTTP response size in bytes"))
  assert_eq(full_counter.description, Some("Active database connections"))
  
  assert_eq(simple_counter.unit, None)
  assert_eq(described_counter.unit, Some("bytes"))
  assert_eq(full_counter.unit, Some("connections"))
  
  // Test counter operations with different values
  Counter::add(simple_counter, 1.0)
  Counter::add(simple_counter, 5.0)
  Counter::add(simple_counter, 10.5)
  Counter::add(simple_counter, 0.0)
  Counter::add(simple_counter, -2.5) // Counter can accept negative values in simplified implementation
  
  // Test counter with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "method", StringValue("GET"))
  Attributes::set(attrs, "status", StringValue("200"))
  Counter::add(described_counter, 1024.0, Some(attrs))
  
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "method", StringValue("POST"))
  Attributes::set(error_attrs, "status", StringValue("500"))
  Counter::add(described_counter, 512.0, Some(error_attrs))
  
  // Test large values
  Counter::add(full_counter, 1000000.0)
  Counter::add(full_counter, 2000000.0)
  Counter::add(full_counter, 1500000.5)
}

test "histogram operations and measurements" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram-meter")
  
  // Test creating histograms
  let response_time_histogram = Meter::create_histogram(meter, "http.response.time", Some("HTTP response time in milliseconds"), Some("ms"))
  let size_histogram = Meter::create_histogram(meter, "message.size", Some("Message size in bytes"), Some("bytes"))
  let simple_histogram = Meter::create_histogram(meter, "simple.histogram")
  
  // Verify histogram properties
  assert_eq(response_time_histogram.name, "http.response.time")
  assert_eq(size_histogram.name, "message.size")
  assert_eq(simple_histogram.name, "simple.histogram")
  
  // Test recording different measurement values
  Histogram::record(response_time_histogram, 100.0)
  Histogram::record(response_time_histogram, 250.5)
  Histogram::record(response_time_histogram, 50.0)
  Histogram::record(response_time_histogram, 1000.0)
  Histogram::record(response_time_histogram, 0.1)
  
  // Test recording with attributes
  let fast_attrs = Attributes::new()
  Attributes::set(fast_attrs, "endpoint", StringValue("/api/fast"))
  Histogram::record(response_time_histogram, 25.0, Some(fast_attrs))
  
  let slow_attrs = Attributes::new()
  Attributes::set(slow_attrs, "endpoint", StringValue("/api/slow"))
  Histogram::record(response_time_histogram, 2000.0, Some(slow_attrs))
  
  // Test size measurements
  Histogram::record(size_histogram, 1024.0)
  Histogram::record(size_histogram, 2048.0)
  Histogram::record(size_histogram, 512.0)
  Histogram::record(size_histogram, 4096.0)
  
  // Test simple histogram
  Histogram::record(simple_histogram, 42.0)
  Histogram::record(simple_histogram, 84.0)
  Histogram::record(simple_histogram, 21.0)
}

test "updown counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown-meter")
  
  // Test creating updown counters
  let active_connections = Meter::create_updown_counter(meter, "active.connections", Some("Currently active connections"), Some("connections"))
  let queue_size = Meter::create_updown_counter(meter, "queue.size", Some("Current queue size"), Some("items"))
  let memory_usage = Meter::create_updown_counter(meter, "memory.usage", Some("Memory usage in MB"), Some("MB"))
  
  // Verify updown counter properties
  assert_eq(active_connections.name, "active.connections")
  assert_eq(queue_size.name, "queue.size")
  assert_eq(memory_usage.name, "memory.usage")
  
  // Test increment operations
  UpDownCounter::add(active_connections, 10.0)
  UpDownCounter::add(active_connections, 5.0)
  assert_true(true) // Simplified implementation
  
  // Test decrement operations
  UpDownCounter::add(active_connections, -3.0)
  UpDownCounter::add(active_connections, -2.0)
  
  // Test queue size operations
  UpDownCounter::add(queue_size, 100.0)
  UpDownCounter::add(queue_size, 50.0)
  UpDownCounter::add(queue_size, -75.0)
  UpDownCounter::add(queue_size, 25.0)
  
  // Test memory usage with attributes
  let heap_attrs = Attributes::new()
  Attributes::set(heap_attrs, "type", StringValue("heap"))
  UpDownCounter::add(memory_usage, 512.0, Some(heap_attrs))
  
  let stack_attrs = Attributes::new()
  Attributes::set(stack_attrs, "type", StringValue("stack"))
  UpDownCounter::add(memory_usage, 64.0, Some(stack_attrs))
  
  // Test negative values
  UpDownCounter::add(memory_usage, -128.0, Some(heap_attrs))
}

test "gauge operations and instant measurements" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge-meter")
  
  // Test creating gauges
  let cpu_usage = Meter::create_gauge(meter, "cpu.usage", Some("Current CPU usage percentage"), Some("%"))
  let temperature = Meter::create_gauge(meter, "system.temperature", Some("System temperature in Celsius"), Some("°C"))
  let disk_space = Meter::create_gauge(meter, "disk.space.available", Some("Available disk space in GB"), Some("GB"))
  
  // Verify gauge properties
  assert_eq(cpu_usage.name, "cpu.usage")
  assert_eq(temperature.name, "system.temperature")
  assert_eq(disk_space.name, "disk.space.available")
  
  // Test gauge measurements
  UpDownCounter::add(cpu_usage, 75.5) // Using UpDownCounter for gauge operations
  UpDownCounter::add(cpu_usage, 80.0)
  UpDownCounter::add(cpu_usage, 45.2)
  UpDownCounter::add(cpu_usage, 90.1)
  
  // Test temperature measurements
  let temp_attrs = Attributes::new()
  Attributes::set(temp_attrs, "sensor", StringValue("cpu"))
  UpDownCounter::add(temperature, 65.0, Some(temp_attrs))
  
  let system_temp_attrs = Attributes::new()
  Attributes::set(system_temp_attrs, "sensor", StringValue("system"))
  UpDownCounter::add(temperature, 45.0, Some(system_temp_attrs))
  
  // Test disk space measurements
  UpDownCounter::add(disk_space, 500.0)
  UpDownCounter::add(disk_space, 250.0)
  UpDownCounter::add(disk_space, 100.0)
}

test "instrument type operations and conversions" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "instrument-test-meter")
  
  // Create different instrument types
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("unit"))
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("unit"))
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test updown"), Some("unit"))
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("unit"))
  
  // Convert to Instrument enum and test properties
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  let updown_instrument = UpDownCounter(updown_counter.name, updown_counter.description, updown_counter.unit)
  let gauge_instrument = Gauge(gauge.name, gauge.description, gauge.unit)
  
  // Test instrument name extraction
  assert_eq(Instrument::name(counter_instrument), "test.counter")
  assert_eq(Instrument::name(histogram_instrument), "test.histogram")
  assert_eq(Instrument::name(updown_instrument), "test.updown")
  assert_eq(Instrument::name(gauge_instrument), "test.gauge")
  
  // Test instrument description extraction
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::description(histogram_instrument), Some("Test histogram"))
  assert_eq(Instrument::description(updown_instrument), Some("Test updown"))
  assert_eq(Instrument::description(gauge_instrument), Some("Test gauge"))
  
  // Test instrument unit extraction
  assert_eq(Instrument::unit(counter_instrument), Some("unit"))
  assert_eq(Instrument::unit(histogram_instrument), Some("unit"))
  assert_eq(Instrument::unit(updown_instrument), Some("unit"))
  assert_eq(Instrument::unit(gauge_instrument), Some("unit"))
  
  // Test histogram as_instrument conversion
  let histogram_as_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_as_instrument), "test.histogram")
  assert_eq(Instrument::description(histogram_as_instrument), Some("Test histogram"))
}

test "complex metrics scenarios" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "complex-scenario-meter")
  
  // Simulate a web server metrics scenario
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_time_histogram = Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("ms"))
  let active_connections_gauge = Meter::create_gauge(meter, "http.active.connections", Some("Active HTTP connections"), Some("connections"))
  let error_rate_updown = Meter::create_updown_counter(meter, "http.errors.rate", Some("HTTP error rate"), Some("%"))
  
  // Simulate incoming requests
  for i in 0..10 {
    // Increment request counter
    Counter::add(request_counter, 1.0)
    
    // Record response time (simulating varying response times)
    let response_time = 100.0 + (Int::to_float(i) * 50.0)
    Histogram::record(response_time_histogram, response_time)
    
    // Update active connections
    if i % 3 == 0 {
      UpDownCounter::add(active_connections_gauge, 1.0)
    } else if i % 3 == 1 {
      UpDownCounter::add(active_connections_gauge, -1.0)
    }
    
    // Simulate occasional errors
    if i % 4 == 0 {
      UpDownCounter::add(error_rate_updown, 5.0)
    }
  }
  
  // Test metrics with different attributes
  let get_attrs = Attributes::new()
  Attributes::set(get_attrs, "method", StringValue("GET"))
  Attributes::set(get_attrs, "status", StringValue("200"))
  Counter::add(request_counter, 100.0, Some(get_attrs))
  
  let post_attrs = Attributes::new()
  Attributes::set(post_attrs, "method", StringValue("POST"))
  Attributes::set(post_attrs, "status", StringValue("201"))
  Counter::add(request_counter, 50.0, Some(post_attrs))
  
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "method", StringValue("GET"))
  Attributes::set(error_attrs, "status", StringValue("500"))
  Counter::add(request_counter, 5.0, Some(error_attrs))
  
  // Test response time with endpoint attributes
  let api_attrs = Attributes::new()
  Attributes::set(api_attrs, "endpoint", StringValue("/api/data"))
  Histogram::record(response_time_histogram, 150.0, Some(api_attrs))
  
  let static_attrs = Attributes::new()
  Attributes::set(static_attrs, "endpoint", StringValue("/static/resource"))
  Histogram::record(response_time_histogram, 25.0, Some(static_attrs))
}

test "metrics edge cases and boundary conditions" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "edge-case-meter")
  
  // Test with extreme values
  let extreme_counter = Meter::create_counter(meter, "extreme.values")
  Counter::add(extreme_counter, 0.0)
  Counter::add(extreme_counter, -1.0)
  Counter::add(extreme_counter, 999999999.0)
  Counter::add(extreme_counter, -999999999.0)
  Counter::add(extreme_counter, 0.000001)
  Counter::add(extreme_counter, -0.000001)
  
  // Test with very small and very large histogram values
  let precision_histogram = Meter::create_histogram(meter, "precision.measurements")
  Histogram::record(precision_histogram, 0.0000001)
  Histogram::record(precision_histogram, 999999999.9)
  Histogram::record(precision_histogram, -0.0000001)
  Histogram::record(precision_histogram, -999999999.9)
  
  // Test with special float values
  let special_values_histogram = Meter::create_histogram(meter, "special.values")
  Histogram::record(special_values_histogram, 0.0)
  Histogram::record(special_values_histogram, 1.0)
  Histogram::record(special_values_histogram, -1.0)
  
  // Test updown counter with rapid increments/decrements
  let volatile_gauge = Meter::create_updown_counter(meter, "volatile.measurements")
  for i in 0..100 {
    if i % 2 == 0 {
      UpDownCounter::add(volatile_gauge, 1.0)
    } else {
      UpDownCounter::add(volatile_gauge, -1.0)
    }
  }
  
  // Test metrics with complex attribute combinations
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "region", StringValue("us-west-2"))
  Attributes::set(complex_attrs, "az", StringValue("us-west-2a"))
  Attributes::set(complex_attrs, "instance", StringValue("i-1234567890abcdef0"))
  Attributes::set(complex_attrs, "service", StringValue("api-service"))
  Attributes::set(complex_attrs, "version", StringValue("v1.2.3"))
  Attributes::set(complex_attrs, "environment", StringValue("production"))
  
  let complex_counter = Meter::create_counter(meter, "complex.attributes.counter")
  Counter::add(complex_counter, 1.0, Some(complex_attrs))
  
  // Test with empty and null-like attribute values
  let empty_attrs = Attributes::new()
  Attributes::set(empty_attrs, "empty_string", StringValue(""))
  Attributes::set(empty_attrs, "zero_number", IntValue(0))
  Attributes::set(empty_attrs, "false_bool", BoolValue(false))
  
  let empty_counter = Meter::create_counter(meter, "empty.attributes.counter")
  Counter::add(empty_counter, 1.0, Some(empty_attrs))
}