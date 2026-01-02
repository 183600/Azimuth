// Azimuth Telemetry System - Metrics Instrument Tests
// This file contains test cases for metrics instruments

// Test 1: Counter instrument operations
test "counter instrument operations" {
  // Create meter provider and meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Create counter
  let counter = Meter::create_counter(meter, "request_count")
  
  // Test counter properties
  assert_eq(counter.name, "request_count")
  match counter.description {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match counter.unit {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Add counter values
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.5)
  Counter::add(counter, 10.0)
  
  // Add counter with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "method", AttributeValue::StringValue("GET"))
  Attributes::set(attrs, "status", AttributeValue::StringValue("200"))
  Counter::add(counter, 2.0, Some(attrs))
  
  // Test counter with metadata
  let counter_with_meta = Meter::create_counter(meter, "response_count")
  let detailed_counter = Counter {
    name: counter_with_meta.name,
    description: Some("Total number of responses"),
    unit: Some("count")
  }
  
  match detailed_counter.description {
    Some(desc) => assert_eq(desc, "Total number of responses")
    None => assert_true(false)
  }
  
  match detailed_counter.unit {
    Some(unit) => assert_eq(unit, "count")
    None => assert_true(false)
  }
}

// Test 2: Histogram instrument operations
test "histogram instrument operations" {
  // Create meter provider and meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "histogram_meter")
  
  // Create histogram
  let histogram = Meter::create_histogram(meter, "request_duration")
  
  // Test histogram properties
  assert_eq(histogram.name, "request_duration")
  match histogram.description {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match histogram.unit {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Record histogram values
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 250.5)
  Histogram::record(histogram, 500.0)
  
  // Record histogram with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "endpoint", AttributeValue::StringValue("/api/users"))
  Histogram::record(histogram, 150.0, Some(attrs))
  
  // Test histogram with metadata
  let histogram_with_meta = Meter::create_histogram(
    meter, 
    "response_size", 
    Some("Size of response in bytes"), 
    Some("bytes")
  )
  
  assert_eq(histogram_with_meta.name, "response_size")
  match histogram_with_meta.description {
    Some(desc) => assert_eq(desc, "Size of response in bytes")
    None => assert_true(false)
  }
  
  match histogram_with_meta.unit {
    Some(unit) => assert_eq(unit, "bytes")
    None => assert_true(false)
  }
  
  // Test histogram as instrument
  let instrument = Histogram::as_instrument(histogram_with_meta)
  match instrument {
    Counter(name, desc, unit) => {
      assert_eq(name, "response_size")
      match desc {
        Some(d) => assert_eq(d, "Size of response in bytes")
        None => assert_true(false)
      }
      match unit {
        Some(u) => assert_eq(u, "bytes")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// Test 3: UpDownCounter instrument operations
test "updown counter instrument operations" {
  // Create meter provider and meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "updown_meter")
  
  // Create updown counter
  let updown_counter = Meter::create_updown_counter(meter, "active_connections")
  
  // Test updown counter properties
  assert_eq(updown_counter.name, "active_connections")
  match updown_counter.description {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match updown_counter.unit {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Add updown counter values (both positive and negative)
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -3.0)
  UpDownCounter::add(updown_counter, 5.0)
  
  // Add updown counter with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "connection_type", AttributeValue::StringValue("websocket"))
  UpDownCounter::add(updown_counter, -2.0, Some(attrs))
  
  // Test updown counter with metadata
  let updown_with_meta = Meter::create_updown_counter(
    meter, 
    "queue_size", 
    Some("Current queue size"), 
    Some("items")
  )
  
  assert_eq(updown_with_meta.name, "queue_size")
  match updown_with_meta.description {
    Some(desc) => assert_eq(desc, "Current queue size")
    None => assert_true(false)
  }
  
  match updown_with_meta.unit {
    Some(unit) => assert_eq(unit, "items")
    None => assert_true(false)
  }
}

// Test 4: Gauge instrument operations
test "gauge instrument operations" {
  // Create meter provider and meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "gauge_meter")
  
  // Create gauge
  let gauge = Meter::create_gauge(meter, "cpu_usage")
  
  // Test gauge properties
  assert_eq(gauge.name, "cpu_usage")
  match gauge.description {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match gauge.unit {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test gauge with metadata
  let gauge_with_meta = Meter::create_gauge(
    meter, 
    "memory_usage", 
    Some("Current memory usage"), 
    Some("percent")
  )
  
  assert_eq(gauge_with_meta.name, "memory_usage")
  match gauge_with_meta.description {
    Some(desc) => assert_eq(desc, "Current memory usage")
    None => assert_true(false)
  }
  
  match gauge_with_meta.unit {
    Some(unit) => assert_eq(unit, "percent")
    None => assert_true(false)
  }
}

// Test 5: Meter with different instrumentation scopes
test "meter with different instrumentation scopes" {
  // Create meter provider
  let provider = MeterProvider::default()
  
  // Create meters with different scopes
  let meter1 = MeterProvider::get_meter(provider, "service1")
  let meter2 = MeterProvider::get_meter(provider, "service2", Some("2.0.0"))
  let meter3 = MeterProvider::get_meter(provider, "service3", Some("3.0.0"), Some("https://example.com/schema"))
  
  // Test meter1 scope
  let scope1 = meter1.scope
  assert_eq(scope1.name, "service1")
  match scope1.version {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match scope1.schema_url {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test meter2 scope
  let scope2 = meter2.scope
  assert_eq(scope2.name, "service2")
  match scope2.version {
    Some(v) => assert_eq(v, "2.0.0")
    None => assert_true(false)
  }
  match scope2.schema_url {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test meter3 scope
  let scope3 = meter3.scope
  assert_eq(scope3.name, "service3")
  match scope3.version {
    Some(v) => assert_eq(v, "3.0.0")
    None => assert_true(false)
  }
  match scope3.schema_url {
    Some(s) => assert_eq(s, "https://example.com/schema")
    None => assert_true(false)
  }
}

// Test 6: Multiple instruments with same meter
test "multiple instruments with same meter" {
  // Create meter provider and meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi_instrument_meter")
  
  // Create multiple instruments
  let counter = Meter::create_counter(meter, "request_count")
  let histogram = Meter::create_histogram(meter, "request_duration")
  let updown_counter = Meter::create_updown_counter(meter, "active_requests")
  let gauge = Meter::create_gauge(meter, "system_load")
  
  // Test all instruments have the same meter scope
  assert_eq(counter.name, "request_count")
  assert_eq(histogram.name, "request_duration")
  assert_eq(updown_counter.name, "active_requests")
  assert_eq(gauge.name, "system_load")
  
  // Use all instruments
  Counter::add(counter, 5.0)
  Histogram::record(histogram, 100.0)
  UpDownCounter::add(updown_counter, 3.0)
  
  // Test with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "service", AttributeValue::StringValue("api"))
  
  Counter::add(counter, 2.0, Some(attrs))
  Histogram::record(histogram, 200.0, Some(attrs))
  UpDownCounter::add(updown_counter, -1.0, Some(attrs))
}

// Test 7: Instrument type conversions and operations
test "instrument type conversions and operations" {
  // Create different instrument types
  let counter_instrument = Instrument::Counter("counter", Some("Counter desc"), Some("count"))
  let histogram_instrument = Instrument::Histogram("histogram", Some("Histogram desc"), Some("ms"))
  let updown_counter_instrument = Instrument::UpDownCounter("updown", Some("UpDown desc"), Some("items"))
  let gauge_instrument = Instrument::Gauge("gauge", Some("Gauge desc"), Some("percent"))
  
  // Test instrument name extraction
  assert_eq(Instrument::name(counter_instrument), "counter")
  assert_eq(Instrument::name(histogram_instrument), "histogram")
  assert_eq(Instrument::name(updown_counter_instrument), "updown")
  assert_eq(Instrument::name(gauge_instrument), "gauge")
  
  // Test instrument description extraction
  match Instrument::description(counter_instrument) {
    Some(desc) => assert_eq(desc, "Counter desc")
    None => assert_true(false)
  }
  
  match Instrument::description(histogram_instrument) {
    Some(desc) => assert_eq(desc, "Histogram desc")
    None => assert_true(false)
  }
  
  match Instrument::description(updown_counter_instrument) {
    Some(desc) => assert_eq(desc, "UpDown desc")
    None => assert_true(false)
  }
  
  match Instrument::description(gauge_instrument) {
    Some(desc) => assert_eq(desc, "Gauge desc")
    None => assert_true(false)
  }
  
  // Test instrument unit extraction
  match Instrument::unit(counter_instrument) {
    Some(unit) => assert_eq(unit, "count")
    None => assert_true(false)
  }
  
  match Instrument::unit(histogram_instrument) {
    Some(unit) => assert_eq(unit, "ms")
    None => assert_true(false)
  }
  
  match Instrument::unit(updown_counter_instrument) {
    Some(unit) => assert_eq(unit, "items")
    None => assert_true(false)
  }
  
  match Instrument::unit(gauge_instrument) {
    Some(unit) => assert_eq(unit, "percent")
    None => assert_true(false)
  }
}

// Test 8: Instruments with complex attributes
test "instruments with complex attributes" {
  // Create meter provider and meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "complex_attrs_meter")
  
  // Create counter
  let counter = Meter::create_counter(meter, "complex_counter")
  
  // Create complex attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "string_attr", AttributeValue::StringValue("test_value"))
  Attributes::set(attrs, "int_attr", AttributeValue::IntValue(42))
  Attributes::set(attrs, "float_attr", AttributeValue::FloatValue(3.14))
  Attributes::set(attrs, "bool_attr", AttributeValue::BoolValue(true))
  Attributes::set(attrs, "string_array_attr", AttributeValue::ArrayStringValue(["value1", "value2"]))
  Attributes::set(attrs, "int_array_attr", AttributeValue::ArrayIntValue([1, 2, 3]))
  
  // Use counter with complex attributes
  Counter::add(counter, 1.0, Some(attrs))
  Counter::add(counter, 5.0, Some(attrs))
  
  // Create histogram with different complex attributes
  let histogram = Meter::create_histogram(meter, "complex_histogram")
  
  let histogram_attrs = Attributes::new()
  Attributes::set(histogram_attrs, "service.name", AttributeValue::StringValue("auth-service"))
  Attributes::set(histogram_attrs, "service.version", AttributeValue::StringValue("1.2.3"))
  Attributes::set(histogram_attrs, "deployment.region", AttributeValue::StringValue("us-west-2"))
  Attributes::set(histogram_attrs, "instance.id", AttributeValue::StringValue("i-1234567890abcdef0"))
  
  Histogram::record(histogram, 150.0, Some(histogram_attrs))
  Histogram::record(histogram, 200.0, Some(histogram_attrs))
}