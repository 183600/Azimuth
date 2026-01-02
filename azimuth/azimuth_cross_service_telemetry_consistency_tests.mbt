// Cross-Service Telemetry Data Consistency Tests for Azimuth
// This file contains test cases for ensuring telemetry data consistency across multiple services

test "cross service trace context consistency" {
  // Test that trace context remains consistent across service boundaries
  let trace_id = "12345678901234567890123456789012"
  let span_id = "1234567890123456"
  
  // Create initial span context in service A
  let service_a_span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  // Verify span context properties
  assert_eq(azimuth::SpanContext::trace_id(service_a_span_ctx), trace_id)
  assert_eq(azimuth::SpanContext::span_id(service_a_span_ctx), span_id)
  assert_true(azimuth::SpanContext::is_sampled(service_a_span_ctx))
  assert_true(azimuth::SpanContext::is_valid(service_a_span_ctx))
  
  // Simulate propagation to service B
  let service_b_span_ctx = azimuth::SpanContext::new(trace_id, "2345678901234567", true, "key1=value1,key2=value2,key3=value3")
  
  // Verify trace ID consistency across services
  assert_eq(azimuth::SpanContext::trace_id(service_a_span_ctx), azimuth::SpanContext::trace_id(service_b_span_ctx))
  
  // Verify span IDs are different (unique per service)
  assert_true(azimuth::SpanContext::span_id(service_a_span_ctx) != azimuth::SpanContext::span_id(service_b_span_ctx))
  
  // Verify sampling decision is consistent
  assert_eq(azimuth::SpanContext::is_sampled(service_a_span_ctx), azimuth::SpanContext::is_sampled(service_b_span_ctx))
}

test "cross service baggage consistency" {
  // Test that baggage items remain consistent across service boundaries
  let service_a_baggage = azimuth::Baggage::new()
  
  // Add baggage items in service A
  let service_a_baggage_updated = azimuth::Baggage::set_entry(service_a_baggage, "user.id", "user123")
  let service_a_baggage_updated = azimuth::Baggage::set_entry(service_a_baggage_updated, "request.id", "req456")
  let service_a_baggage_updated = azimuth::Baggage::set_entry(service_a_baggage_updated, "tenant.id", "tenant789")
  
  // Simulate baggage propagation to service B
  let service_b_baggage = azimuth::Baggage::new()
  let service_b_baggage_updated = azimuth::Baggage::set_entry(service_b_baggage, "user.id", "user123")
  let service_b_baggage_updated = azimuth::Baggage::set_entry(service_b_baggage_updated, "request.id", "req456")
  let service_b_baggage_updated = azimuth::Baggage::set_entry(service_b_baggage_updated, "tenant.id", "tenant789")
  
  // Verify baggage consistency across services
  let user_id_a = azimuth::Baggage::get_entry(service_a_baggage_updated, "user.id")
  let user_id_b = azimuth::Baggage::get_entry(service_b_baggage_updated, "user.id")
  assert_eq(user_id_a, user_id_b)
  
  let request_id_a = azimuth::Baggage::get_entry(service_a_baggage_updated, "request.id")
  let request_id_b = azimuth::Baggage::get_entry(service_b_baggage_updated, "request.id")
  assert_eq(request_id_a, request_id_b)
  
  let tenant_id_a = azimuth::Baggage::get_entry(service_a_baggage_updated, "tenant.id")
  let tenant_id_b = azimuth::Baggage::get_entry(service_b_baggage_updated, "tenant.id")
  assert_eq(tenant_id_a, tenant_id_b)
}

test "cross service resource attribute consistency" {
  // Test that resource attributes maintain consistency across services
  let global_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.namespace", azimuth::StringValue("production")),
    ("service.version", azimuth::StringValue("1.2.3")),
    ("deployment.environment", azimuth::StringValue("prod"))
  ])
  
  // Service A specific resource
  let service_a_resource = azimuth::Resource::with_attributes(global_resource, [
    ("service.name", azimuth::StringValue("service-a")),
    ("service.instance.id", azimuth::StringValue("instance-a-123"))
  ])
  
  // Service B specific resource
  let service_b_resource = azimuth::Resource::with_attributes(global_resource, [
    ("service.name", azimuth::StringValue("service-b")),
    ("service.instance.id", azimuth::StringValue("instance-b-456"))
  ])
  
  // Verify global attributes are consistent
  let namespace_a = azimuth::Resource::get_attribute(service_a_resource, "service.namespace")
  let namespace_b = azimuth::Resource::get_attribute(service_b_resource, "service.namespace")
  assert_eq(namespace_a, namespace_b)
  
  let version_a = azimuth::Resource::get_attribute(service_a_resource, "service.version")
  let version_b = azimuth::Resource::get_attribute(service_b_resource, "service.version")
  assert_eq(version_a, version_b)
  
  let env_a = azimuth::Resource::get_attribute(service_a_resource, "deployment.environment")
  let env_b = azimuth::Resource::get_attribute(service_b_resource, "deployment.environment")
  assert_eq(env_a, env_b)
  
  // Verify service-specific attributes are different
  let service_name_a = azimuth::Resource::get_attribute(service_a_resource, "service.name")
  let service_name_b = azimuth::Resource::get_attribute(service_b_resource, "service.name")
  assert_true(service_name_a != service_name_b)
}

test "cross service metric consistency" {
  // Test that metrics maintain consistency across services
  let meter_provider = azimuth::MeterProvider::default()
  
  // Service A metrics
  let service_a_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-a")
  let service_a_counter = azimuth::Meter::create_counter(service_a_meter, "http.requests.total")
  let service_a_histogram = azimuth::Meter::create_histogram(service_a_meter, "http.request.duration")
  
  // Service B metrics
  let service_b_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-b")
  let service_b_counter = azimuth::Meter::create_counter(service_b_meter, "http.requests.total")
  let service_b_histogram = azimuth::Meter::create_histogram(service_b_meter, "http.request.duration")
  
  // Verify metric names are consistent
  assert_eq(service_a_counter.name, service_b_counter.name)
  assert_eq(service_a_histogram.name, service_b_histogram.name)
  
  // Add consistent attributes to metrics
  let common_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(common_attrs, "http.method", azimuth::StringValue("GET"))
  azimuth::Attributes::set(common_attrs, "http.status_code", azimuth::IntValue(200))
  
  // Record metrics in both services
  azimuth::Counter::add(service_a_counter, 1.0, Some(common_attrs))
  azimuth::Histogram::record(service_a_histogram, 150.5, Some(common_attrs))
  
  azimuth::Counter::add(service_b_counter, 1.0, Some(common_attrs))
  azimuth::Histogram::record(service_b_histogram, 120.3, Some(common_attrs))
  
  // Verify metric descriptions are consistent
  let service_a_counter_instrument = azimuth::Counter::as_instrument(service_a_counter)
  let service_b_counter_instrument = azimuth::Counter::as_instrument(service_b_counter)
  assert_eq(azimuth::Instrument::name(service_a_counter_instrument), azimuth::Instrument::name(service_b_counter_instrument))
}

test "cross service log correlation consistency" {
  // Test that log records maintain correlation consistency across services
  let logger_provider = azimuth::LoggerProvider::default()
  
  // Service A logger
  let service_a_logger = azimuth::LoggerProvider::get_logger(logger_provider, "service-a")
  
  // Service B logger
  let service_b_logger = azimuth::LoggerProvider::get_logger(logger_provider, "service-b")
  
  // Common trace and span IDs for correlation
  let trace_id = "98765432109876543210987654321098"
  let service_a_span_id = "9876543210987654"
  let service_b_span_id = "8765432109876543"
  
  // Create correlated log records in service A
  let service_a_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Processing request in service A"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(service_a_span_id),
    Some(azimuth::Context::root())
  )
  
  // Create correlated log records in service B
  let service_b_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Processing request in service B"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(service_b_span_id),
    Some(azimuth::Context::root())
  )
  
  // Verify trace ID consistency across services
  assert_eq(azimuth::LogRecord::trace_id(service_a_log), azimuth::LogRecord::trace_id(service_b_log))
  
  // Verify span IDs are different (unique per service)
  assert_true(azimuth::LogRecord::span_id(service_a_log) != azimuth::LogRecord::span_id(service_b_log))
  
  // Emit log records
  azimuth::Logger::emit(service_a_logger, service_a_log)
  azimuth::Logger::emit(service_b_logger, service_b_log)
}

test "cross service context propagation consistency" {
  // Test that context propagation maintains consistency across services
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let composite_propagator = azimuth::CompositePropagator::new([trace_propagator])
  
  // Service A creates and injects context
  let service_a_ctx = azimuth::Context::root()
  let correlation_key = azimuth::ContextKey::new("correlation.id")
  let service_a_ctx_with_correlation = azimuth::Context::with_value(service_a_ctx, correlation_key, "corr-12345")
  
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, service_a_ctx_with_correlation, carrier)
  
  // Service B extracts context
  let service_b_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extracted context has correlation data
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(service_b_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
  
  // Multiple hops: Service B injects context for Service C
  let service_b_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, service_b_ctx, service_b_carrier)
  
  // Service C extracts context
  let service_c_ctx = azimuth::CompositePropagator::extract(composite_propagator, service_b_carrier)
  let service_c_extracted_value = azimuth::Context::get(service_c_ctx, extracted_key)
  assert_eq(service_c_extracted_value, Some("true"))
}

test "cross service attribute type consistency" {
  // Test that attribute types remain consistent across services
  let service_a_attrs = azimuth::Attributes::new()
  let service_b_attrs = azimuth::Attributes::new()
  
  // Set attributes of various types in service A
  azimuth::Attributes::set(service_a_attrs, "string.key", azimuth::StringValue("consistent.value"))
  azimuth::Attributes::set(service_a_attrs, "int.key", azimuth::IntValue(42))
  azimuth::Attributes::set(service_a_attrs, "float.key", azimuth::FloatValue(3.14159))
  azimuth::Attributes::set(service_a_attrs, "bool.key", azimuth::BoolValue(true))
  
  // Set same attributes with same types in service B
  azimuth::Attributes::set(service_b_attrs, "string.key", azimuth::StringValue("consistent.value"))
  azimuth::Attributes::set(service_b_attrs, "int.key", azimuth::IntValue(42))
  azimuth::Attributes::set(service_b_attrs, "float.key", azimuth::FloatValue(3.14159))
  azimuth::Attributes::set(service_b_attrs, "bool.key", azimuth::BoolValue(true))
  
  // Verify attribute types and values are consistent
  let string_a = azimuth::Attributes::get(service_a_attrs, "string.key")
  let string_b = azimuth::Attributes::get(service_b_attrs, "string.key")
  assert_eq(string_a, string_b)
  
  let int_a = azimuth::Attributes::get(service_a_attrs, "int.key")
  let int_b = azimuth::Attributes::get(service_b_attrs, "int.key")
  assert_eq(int_a, int_b)
  
  let float_a = azimuth::Attributes::get(service_a_attrs, "float.key")
  let float_b = azimuth::Attributes::get(service_b_attrs, "float.key")
  assert_eq(float_a, float_b)
  
  let bool_a = azimuth::Attributes::get(service_a_attrs, "bool.key")
  let bool_b = azimuth::Attributes::get(service_b_attrs, "bool.key")
  assert_eq(bool_a, bool_b)
}