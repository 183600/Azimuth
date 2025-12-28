// Top-level module for Azimuth telemetry package
pub test "basic project setup test" {
  // Basic test to ensure project setup is working
  assert_eq(1 + 1, 2)
  assert_true(true)
  assert_false(false)
}

// Test 1: Attributes operations
pub test "attributes creation and operations" {
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("test_value"))
  azimuth::Attributes::set(attrs, "int.key", azimuth::IntValue(42))
  
  let string_val = azimuth::Attributes::get(attrs, "string.key")
  let int_val = azimuth::Attributes::get(attrs, "int.key")
  let missing_val = azimuth::Attributes::get(attrs, "missing.key")
  
  assert_eq(string_val, Some(azimuth::StringValue("test_value")))
  assert_eq(int_val, Some(azimuth::IntValue(42)))
  assert_eq(missing_val, None)
}

// Test 2: Context and Baggage operations
pub test "context and baggage propagation" {
  let root_ctx = azimuth::Context::root()
  let key = azimuth::ContextKey::new("user.id")
  let ctx_with_value = azimuth::Context::with_value(root_ctx, key, "12345")
  
  let retrieved_value = azimuth::Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("12345"))
  
  let missing_key = azimuth::ContextKey::new("missing.key")
  let missing_value = azimuth::Context::get(root_ctx, missing_key)
  assert_eq(missing_value, None)
  
  // Test baggage operations
  let baggage = azimuth::Baggage::new()
  let updated_baggage = azimuth::Baggage::set_entry(baggage, "request.id", "req-123")
  let baggage_value = azimuth::Baggage::get_entry(updated_baggage, "request.id")
  assert_eq(baggage_value, Some("req-123"))
}

// Test 3: Span and Tracer functionality
pub test "span and tracer operations" {
  let span_ctx = azimuth::SpanContext::new("trace-123", "span-456", true, "")
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), "trace-123")
  assert_eq(azimuth::SpanContext::span_id(span_ctx), "span-456")
  assert_true(azimuth::SpanContext::is_valid(span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  
  let span = azimuth::Span::new("test-span", azimuth::Internal, span_ctx)
  assert_eq(azimuth::Span::name(span), "test-span")
  assert_eq(azimuth::Span::kind(span), azimuth::Internal)
  assert_true(azimuth::Span::is_recording(span))
  
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(provider, "test-tracer")
  let scope = azimuth::Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  
  let new_span = azimuth::Tracer::start_span(tracer, "operation")
  assert_eq(azimuth::Span::name(new_span), "operation")
}

// Test 4: Metrics operations
pub test "metrics counter and histogram operations" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "test-meter")
  
  let counter = azimuth::Meter::create_counter(meter, "request.count")
  assert_eq(counter.name, "request.count")
  azimuth::Counter::add(counter, 1.0)
  
  let histogram = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time in milliseconds"))
  assert_eq(histogram.unit, Some("ms"))
  azimuth::Histogram::record(histogram, 150.5)
  
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "active.connections")
  assert_eq(updown_counter.name, "active.connections")
  azimuth::UpDownCounter::add(updown_counter, 5.0)
  
  let gauge = azimuth::Meter::create_gauge(meter, "memory.usage")
  assert_eq(gauge.name, "memory.usage")
  
  let instrument = azimuth::Histogram::as_instrument(histogram)
  assert_eq(azimuth::Instrument::name(instrument), "response.time")
  assert_eq(azimuth::Instrument::description(instrument), Some("Response time in milliseconds"))
  assert_eq(azimuth::Instrument::unit(instrument), Some("ms"))
}

// Test 5: LogRecord and Logger operations
pub test "log record and logger operations" {
  let log_record = azimuth::LogRecord::new(azimuth::Info, "Test log message")
  assert_eq(azimuth::LogRecord::severity_number(log_record), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(log_record), Some("Test log message"))
  
  let detailed_record = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Error occurred"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-123"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  assert_eq(azimuth::LogRecord::severity_number(detailed_record), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(detailed_record), Some("Error occurred"))
  assert_eq(azimuth::LogRecord::trace_id(detailed_record), Some("trace-123"))
  assert_eq(azimuth::LogRecord::span_id(detailed_record), Some("span-456"))
  
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "test-logger")
  assert_eq(logger.scope.name, "test-logger")
  
  azimuth::Logger::emit(logger, log_record)
}

// Test 6: Resource management
pub test "resource operations" {
  let resource = azimuth::Resource::new()
  let attributes = [("service.name", azimuth::StringValue("test-service")), 
                    ("service.version", azimuth::StringValue("1.0.0"))]
  let resource_with_attrs = azimuth::Resource::with_attributes(resource, attributes)
  
  let service_name = azimuth::Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = azimuth::Resource::get_attribute(resource_with_attrs, "service.version")
  let missing_attr = azimuth::Resource::get_attribute(resource_with_attrs, "missing.attr")
  
  assert_eq(service_name, Some(azimuth::StringValue("test-service")))
  assert_eq(service_version, Some(azimuth::StringValue("1.0.0")))
  assert_eq(missing_attr, None)
  
  let override_resource = azimuth::Resource::with_attributes(resource, 
    [("environment", azimuth::StringValue("production"))])
  let merged_resource = azimuth::Resource::merge(resource_with_attrs, override_resource)
  let env_attr = azimuth::Resource::get_attribute(merged_resource, "environment")
  assert_eq(env_attr, Some(azimuth::StringValue("production")))
}

// Test 7: Propagator functionality
pub test "propagator operations" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  let key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, key)
  assert_eq(extracted_value, Some("true"))
}

// Test 8: HTTP Client operations
pub test "http client operations" {
  let client = azimuth::HttpClient::new()
  
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = azimuth::HttpRequest::new("GET", "https://api.example.com/data", headers, Some("request body"))
  
  assert_eq(azimuth::HttpRequest::http_method(request), "GET")
  assert_eq(azimuth::HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(azimuth::HttpRequest::body(request), Some("request body"))
  
  let response_headers = [("Content-Type", "application/json")]
  let response = azimuth::HttpResponse::new(200, response_headers, Some("{\"status\": \"success\"}"))
  
  assert_eq(azimuth::HttpResponse::status_code(response), 200)
  assert_eq(azimuth::HttpResponse::body(response), Some("{\"status\": \"success\"}"))
}

// Test 9: Clock and Random operations
pub test "clock and random operations" {
  let clock = azimuth::Clock::system()
  let timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // Verify timestamp is reasonable (should be around 2025)
  assert_true(timestamp > 1700000000000000000L)
  assert_true(timestamp < 1800000000000000000L)
  
  let random = azimuth::Random::system()
  let random_bytes = azimuth::Random::next_bytes(random, 8)
  assert_eq(random_bytes.length(), 8)
  
  let random_u64 = azimuth::Random::next_u64(random)
  assert_true(random_u64.to_int() >= 0)
  assert_true(random_u64.to_int() < 1000000)  // Reasonable range for testing
}

// Test 10: Integration test - Complete telemetry workflow
pub test "complete telemetry workflow integration" {
  // Setup resource
  let resource = azimuth::Resource::new()
  let resource_attrs = [("service.name", azimuth::StringValue("integration-test"))]
  let service_resource = azimuth::Resource::with_attributes(resource, resource_attrs)
  
  // Setup context with baggage
  let ctx = azimuth::Context::root()
  let baggage_key = azimuth::ContextKey::new("user.id")
  let ctx_with_baggage = azimuth::Context::with_value(ctx, baggage_key, "user-123")
  
  // Create tracer and span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "integration-tracer")
  let span = azimuth::Tracer::start_span(tracer, "integration-operation")
  
  // Create metrics
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "integration-meter")
  let counter = azimuth::Meter::create_counter(meter, "operations.count")
  azimuth::Counter::add(counter, 1.0)
  
  // Create logger and log record
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "integration-logger")
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Integration test completed successfully"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
    Some(ctx_with_baggage)
  )
  
  // Emit log
  azimuth::Logger::emit(logger, log_record)
  
  // Verify all components were created correctly
  assert_eq(azimuth::Span::name(span), "integration-operation")
  assert_eq(counter.name, "operations.count")
  assert_eq(logger.scope.name, "integration-logger")
  assert_eq(azimuth::LogRecord::body(log_record), Some("Integration test completed successfully"))
  
  // Verify resource attributes
  let service_name = azimuth::Resource::get_attribute(service_resource, "service.name")
  assert_eq(service_name, Some(azimuth::StringValue("integration-test")))
}