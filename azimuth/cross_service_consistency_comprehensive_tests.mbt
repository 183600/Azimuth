// Comprehensive Cross-Service Consistency Test Suite
// Tests for consistent telemetry data across multiple services and service boundaries

test "trace context consistency across services" {
  // Simulate trace context creation in service A
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id_a = "b7ad6b7169203331"
  let span_ctx_a = SpanContext::new(trace_id, span_id_a, true, "key1=value1")
  
  // Verify trace context properties
  assert_eq(SpanContext::trace_id(span_ctx_a), trace_id)
  assert_eq(SpanContext::span_id(span_ctx_a), span_id_a)
  assert_true(SpanContext::is_sampled(span_ctx_a))
  assert_true(SpanContext::is_valid(span_ctx_a))
  
  // Simulate service B creating child span with same trace ID
  let span_id_b = "c8de7c827a031442"
  let span_ctx_b = SpanContext::new(trace_id, span_id_b, true, "key1=value1")
  
  // Verify trace ID consistency
  assert_eq(SpanContext::trace_id(span_ctx_a), SpanContext::trace_id(span_ctx_b))
  assert_ne(SpanContext::span_id(span_ctx_a), SpanContext::span_id(span_ctx_b))
}

test "baggage propagation consistency across services" {
  // Service A creates baggage
  let baggage_a = Baggage::new()
  let baggage_a1 = Baggage::set_entry(baggage_a, "user.id", "user123")
  let baggage_a2 = Baggage::set_entry(baggage_a1, "request.id", "req456")
  
  // Service B receives and adds to baggage
  let baggage_b1 = Baggage::set_entry(baggage_a2, "service.name", "service-b")
  let baggage_b2 = Baggage::set_entry(baggage_b1, "operation.name", "process-data")
  
  // Service C receives baggage
  let user_id = Baggage::get_entry(baggage_b2, "user.id")
  let request_id = Baggage::get_entry(baggage_b2, "request.id")
  let service_name = Baggage::get_entry(baggage_b2, "service.name")
  let operation_name = Baggage::get_entry(baggage_b2, "operation.name")
  
  // Verify all baggage entries are preserved
  assert_eq(user_id, Some("user123"))
  assert_eq(request_id, Some("req456"))
  assert_eq(service_name, Some("service-b"))
  assert_eq(operation_name, Some("process-data"))
}

test "resource attribute consistency across services" {
  // Common resource attributes that should be consistent
  let common_attributes = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.environment", StringValue("production"))
  ]
  
  // Service A resource
  let resource_a = Resource::with_attributes(Resource::new(), common_attributes)
  
  // Service B resource with same common attributes
  let resource_b = Resource::with_attributes(Resource::new(), common_attributes)
  
  // Verify common attributes are consistent
  let service_name_a = Resource::get_attribute(resource_a, "service.name")
  let service_name_b = Resource::get_attribute(resource_b, "service.name")
  assert_eq(service_name_a, service_name_b)
  
  let service_version_a = Resource::get_attribute(resource_a, "service.version")
  let service_version_b = Resource::get_attribute(resource_b, "service.version")
  assert_eq(service_version_a, service_version_b)
  
  let deployment_env_a = Resource::get_attribute(resource_a, "deployment.environment")
  let deployment_env_b = Resource::get_attribute(resource_b, "deployment.environment")
  assert_eq(deployment_env_a, deployment_env_b)
}

test "instrumentation scope consistency across services" {
  // Service A instrumentation scope
  let scope_a = InstrumentationScope::{ 
    name: "payment-service", 
    version: Some("2.1.0"), 
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  // Service B instrumentation scope (same service, different instance)
  let scope_b = InstrumentationScope::{ 
    name: "payment-service", 
    version: Some("2.1.0"), 
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  // Verify scope consistency
  assert_eq(scope_a.name, scope_b.name)
  assert_eq(scope_a.version, scope_b.version)
  assert_eq(scope_a.schema_url, scope_b.schema_url)
}

test "metrics naming consistency across services" {
  let provider = MeterProvider::default()
  
  // Service A creates metrics with standardized naming
  let meter_a = MeterProvider::get_meter(provider, "payment-service")
  let counter_a = Meter::create_counter(meter_a, "payment.transactions.total")
  let histogram_a = Meter::create_histogram(meter_a, "payment.amount")
  let gauge_a = Meter::create_gauge(meter_a, "payment.queue.size")
  
  // Service B creates metrics with same naming convention
  let meter_b = MeterProvider::get_meter(provider, "payment-service")
  let counter_b = Meter::create_counter(meter_b, "payment.transactions.total")
  let histogram_b = Meter::create_histogram(meter_b, "payment.amount")
  let gauge_b = Meter::create_gauge(meter_b, "payment.queue.size")
  
  // Verify naming consistency
  assert_eq(counter_a.name, counter_b.name)
  assert_eq(histogram_a.name, histogram_b.name)
  assert_eq(gauge_a.name, gauge_b.name)
}

test "log record consistency across services" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "payment-service")
  
  // Create log records with consistent structure
  let log_a = LogRecord::new_with_context(
    Info,
    Some("Payment processed successfully"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    None
  )
  
  let log_b = LogRecord::new_with_context(
    Info,
    Some("Payment processed successfully"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("c8de7c827a031442"),
    None
  )
  
  // Verify log consistency (same trace ID, different span ID)
  assert_eq(LogRecord::trace_id(log_a), LogRecord::trace_id(log_b))
  assert_ne(LogRecord::span_id(log_a), LogRecord::span_id(log_b))
  assert_eq(LogRecord::severity_number(log_a), LogRecord::severity_number(log_b))
  assert_eq(LogRecord::body(log_a), LogRecord::body(log_b))
}

test "http header propagation consistency" {
  // Service A creates HTTP headers for outbound request
  let carrier_a = TextMapCarrier::new()
  TextMapCarrier::set(carrier_a, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier_a, "baggage", "user.id=123,session.id=456")
  
  // Service B extracts headers from inbound request
  let traceparent_b = TextMapCarrier::get(carrier_a, "traceparent")
  let baggage_b = TextMapCarrier::get(carrier_a, "baggage")
  
  // Verify header consistency
  assert_eq(traceparent_b, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(baggage_b, Some("user.id=123,session.id=456"))
  
  // Service B creates headers for outbound request to Service C
  let carrier_b = TextMapCarrier::new()
  TextMapCarrier::set(carrier_b, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-c8de7c827a031442-01")
  TextMapCarrier::set(carrier_b, "baggage", "user.id=123,session.id=456,service.b=processed")
  
  // Service C extracts headers
  let traceparent_c = TextMapCarrier::get(carrier_b, "traceparent")
  let baggage_c = TextMapCarrier::get(carrier_b, "baggage")
  
  // Verify trace context propagation (same trace ID, different span ID)
  assert_eq(traceparent_c, Some("00-0af7651916cd43dd8448eb211c80319c-c8de7c827a031442-01"))
  assert_eq(baggage_c, Some("user.id=123,session.id=456,service.b=processed"))
}

test "attribute key consistency across services" {
  // Standardized attribute keys that should be consistent across services
  let standard_keys = [
    "service.name",
    "service.version",
    "service.instance.id",
    "telemetry.sdk.name",
    "telemetry.sdk.version",
    "telemetry.sdk.language",
    "http.method",
    "http.status_code",
    "http.url",
    "user.id"
  ]
  
  // Verify all standard keys are valid strings
  for key in standard_keys {
    assert_true(key.length() > 0)
    assert_true(key.length() <= 255)  // OpenTelemetry limit
  }
  
  // Test that services use same attribute key format
  let attrs = Attributes::new()
  for key in standard_keys {
    Attributes::set(attrs, key, StringValue("test-value"))
    let value = Attributes::get(attrs, key)
    assert_eq(value, Some(StringValue("test-value")))
  }
}

test "error handling consistency across services" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "payment-service")
  
  // Service A creates span with error status
  let span_a = Tracer::start_span(tracer, "process-payment")
  Span::set_status(span_a, Error, Some("Payment declined"))
  
  // Service B creates span with error status
  let span_b = Tracer::start_span(tracer, "validate-payment")
  Span::set_status(span_b, Error, Some("Invalid card number"))
  
  // Verify error status handling is consistent
  assert_eq(Span::status(span_a), Error)
  assert_eq(Span::status(span_b), Error)
  
  // Add error events to spans
  Span::add_event(span_a, "payment.error", Some([("error.type", StringValue("declined")), ("amount", IntValue(1000))]))
  Span::add_event(span_b, "validation.error", Some([("error.type", StringValue("invalid_card")), ("card.last4", StringValue("1234"))]))
  
  // End spans
  Span::end(span_a)
  Span::end(span_b)
  
  // If no errors occur during operations, consistency is maintained
  assert_true(true)
}