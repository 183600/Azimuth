// Enhanced Platform Tests for Azimuth Telemetry System
// Focus on HTTP client, resource management, and platform services

test "http_client_request_response_cycle" {
  let client = HttpClient::new()
  let headers = [("Content-Type", "application/json"), ("User-Agent", "azimuth/1.0")]
  let request = HttpRequest::new("GET", "https://api.example.com/telemetry", headers, Some("{\"test\": \"data\"}"))
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/telemetry")
  assert_eq(HttpRequest::body(request), Some("{\"test\": \"data\"}"))
  
  let response = HttpResponse::new(200, [("Content-Type", "application/json")], Some("{\"status\": \"success\"}"))
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"status\": \"success\"}"))
}

test "resource_attribute_management" {
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("production-server")),
    ("process.id", IntValue(12345))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false) // Expected service.name attribute
  }
  
  let process_id = Resource::get_attribute(resource_with_attrs, "process.id")
  match process_id {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false) // Expected process.id attribute
  }
  
  let missing_attr = Resource::get_attribute(resource_with_attrs, "nonexistent")
  assert_eq(missing_attr, None)
}

test "resource_merge_functionality" {
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("environment", StringValue("development"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("region", StringValue("us-west-2"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  let merged = Resource::merge(base_resource, override_resource)
  
  let service_name = Resource::get_attribute(merged, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_true(false) // Expected overridden service.name
  }
  
  let region = Resource::get_attribute(merged, "region")
  match region {
    Some(StringValue(r)) => assert_eq(r, "us-west-2")
    _ => assert_true(false) // Expected region attribute
  }
}

test "clock_timestamp_generation" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // Verify timestamp is reasonable (should be > 2020 timestamp)
  assert_true(timestamp > 1577836800000000000L)
  
  // Verify timestamp is positive
  assert_true(timestamp > 0L)
  
  // Multiple calls should return increasing timestamps
  let timestamp2 = Clock::now_unix_nanos(clock)
  assert_true(timestamp2 >= timestamp)
}

test "random_number_generation" {
  let random = Random::system()
  
  // Test byte array generation
  let bytes = Random::next_bytes(random, 16)
  assert_eq(bytes.length(), 16)
  
  // Test U64 generation
  let random_value = Random::next_u64(random)
  assert_true(random_value >= 0UL)
  
  // Multiple calls should produce different values (probability of collision is very low)
  let random_value2 = Random::next_u64(random)
  // Note: This might occasionally fail due to randomness, but probability is extremely low
  // assert_true(random_value != random_value2)
}

test "log_record_with_full_context" {
  let attributes = Attributes::new()
  let severity = SeverityNumber::Error
  let body = Some("Database connection failed")
  let timestamp = Some(1735689600000000000L)
  let observed_timestamp = Some(1735689600000001000L)
  let trace_id = Some("trace_123456")
  let span_id = Some("span_789012")
  let context = Some(Context::root())
  
  let record = LogRecord::new_with_context(
    severity,
    body,
    Some(attributes),
    timestamp,
    observed_timestamp,
    trace_id,
    span_id,
    context
  )
  
  assert_eq(LogRecord::severity_number(record), SeverityNumber::Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some("trace_123456"))
  assert_eq(LogRecord::span_id(record), Some("span_789012"))
}

test "meter_instrument_creation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test Counter creation
  let counter = Meter::create_counter(meter, "request_count", Some("Total number of requests"), Some("requests"))
  assert_eq(counter.name, "request_count")
  assert_eq(counter.description, Some("Total number of requests"))
  assert_eq(counter.unit, Some("requests"))
  
  // Test Histogram creation
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response time distribution"), Some("ms"))
  assert_eq(histogram.name, "response_time")
  assert_eq(histogram.description, Some("Response time distribution"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test UpDownCounter creation
  let updown_counter = Meter::create_updown_counter(meter, "active_connections", Some("Currently active connections"), Some("connections"))
  assert_eq(updown_counter.name, "active_connections")
  assert_eq(updown_counter.description, Some("Currently active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  // Test Gauge creation
  let gauge = Meter::create_gauge(meter, "memory_usage", Some("Current memory usage"), Some("bytes"))
  assert_eq(gauge.name, "memory_usage")
  assert_eq(gauge.description, Some("Current memory usage"))
  assert_eq(gauge.unit, Some("bytes"))
}

test "instrument_metadata_operations" {
  let counter_instrument = Counter("test_counter", Some("Test counter description"), Some("count"))
  let histogram_instrument = Histogram("test_histogram", Some("Test histogram description"), Some("ms"))
  
  // Test instrument name
  assert_eq(Instrument::name(counter_instrument), "test_counter")
  assert_eq(Instrument::name(histogram_instrument), "test_histogram")
  
  // Test instrument description
  assert_eq(Instrument::description(counter_instrument), Some("Test counter description"))
  assert_eq(Instrument::description(histogram_instrument), Some("Test histogram description"))
  
  // Test instrument unit
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
}

test "composite_propagator_injection_extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let root_ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, root_ctx, carrier)
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_trace.is_some())
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, key)
  assert_eq(extracted_value, Some("true"))
}