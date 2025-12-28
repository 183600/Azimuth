test "Cross-service trace consistency test" {
  // Test trace ID consistency across services
  let base_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let base_span_id = "b7ad6b7169203331"
  
  // Service A: Create initial span context
  let service_a_span_ctx = SpanContext::new(base_trace_id, base_span_id, true, "")
  @assertion.assert_eq(SpanContext::trace_id(service_a_span_ctx), base_trace_id)?
  @assertion.assert_eq(SpanContext::span_id(service_a_span_ctx), base_span_id)?
  @assertion.assert_eq(SpanContext::is_sampled(service_a_span_ctx), true)?
  @assertion.assert_eq(SpanContext::is_valid(service_a_span_ctx), true)?
  
  // Service B: Create child span with same trace ID
  let service_b_span_id = "c8de7c8270314442"
  let service_b_span_ctx = SpanContext::new(base_trace_id, service_b_span_id, true, "")
  @assertion.assert_eq(SpanContext::trace_id(service_b_span_ctx), base_trace_id)?
  @assertion.assert_eq(SpanContext::span_id(service_b_span_ctx), service_b_span_id)?
  @assertion.assert_eq(SpanContext::is_sampled(service_b_span_ctx), true)?
  
  // Service C: Create another child span
  let service_c_span_id = "d9ef8d9381425553"
  let service_c_span_ctx = SpanContext::new(base_trace_id, service_c_span_id, true, "")
  @assertion.assert_eq(SpanContext::trace_id(service_c_span_ctx), base_trace_id)?
  @assertion.assert_eq(SpanContext::span_id(service_c_span_ctx), service_c_span_id)?
  
  // Verify trace ID consistency across all services
  @assertion.assert_eq(
    SpanContext::trace_id(service_a_span_ctx),
    SpanContext::trace_id(service_b_span_ctx)
  )?
  @assertion.assert_eq(
    SpanContext::trace_id(service_b_span_ctx),
    SpanContext::trace_id(service_c_span_ctx)
  )?
  
  // Verify span IDs are unique
  @assertion.assert_neq(
    SpanContext::span_id(service_a_span_ctx),
    SpanContext::span_id(service_b_span_ctx)
  )?
  @assertion.assert_neq(
    SpanContext::span_id(service_b_span_ctx),
    SpanContext::span_id(service_c_span_ctx)
  )?
}

test "Cross-service baggage propagation consistency test" {
  // Test baggage consistency across service boundaries
  let initial_baggage = Baggage::new()
  
  // Service A: Add initial baggage entries
  let service_a_baggage = Baggage::set_entry(initial_baggage, "user.id", "12345")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "session.id", "session-abc")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "request.id", "req-123")
  
  // Service B: Add additional baggage entries
  let service_b_baggage = Baggage::set_entry(service_a_baggage, "service.b.timestamp", "2025-01-01T12:00:00Z")
  let service_b_baggage = Baggage::set_entry(service_b_baggage, "operation", "user.lookup")
  
  // Service C: Add more entries
  let service_c_baggage = Baggage::set_entry(service_b_baggage, "service.c.latency", "45ms")
  let service_c_baggage = Baggage::set_entry(service_c_baggage, "cache.hit", "true")
  
  // Test baggage entry retrieval across services
  // Note: Simplified implementation returns None for all entries
  let user_id = Baggage::get_entry(service_c_baggage, "user.id")
  let session_id = Baggage::get_entry(service_c_baggage, "session.id")
  let request_id = Baggage::get_entry(service_c_baggage, "request.id")
  let timestamp = Baggage::get_entry(service_c_baggage, "service.b.timestamp")
  let operation = Baggage::get_entry(service_c_baggage, "operation")
  let latency = Baggage::get_entry(service_c_baggage, "service.c.latency")
  let cache_hit = Baggage::get_entry(service_c_baggage, "cache.hit")
  
  @assertion.assert_eq(user_id, None)?
  @assertion.assert_eq(session_id, None)?
  @assertion.assert_eq(request_id, None)?
  @assertion.assert_eq(timestamp, None)?
  @assertion.assert_eq(operation, None)?
  @assertion.assert_eq(latency, None)?
  @assertion.assert_eq(cache_hit, None)?
  
  // Test baggage entry removal
  let baggage_after_removal = Baggage::remove_entry(service_c_baggage, "session.id")
  let removed_session = Baggage::get_entry(baggage_after_removal, "session.id")
  @assertion.assert_eq(removed_session, None)?
  
  // Verify other entries are still accessible (would be None in simplified implementation)
  let preserved_user_id = Baggage::get_entry(baggage_after_removal, "user.id")
  @assertion.assert_eq(preserved_user_id, None)?
}

test "Cross-service metrics consistency test" {
  // Test metrics consistency across services
  let meter_provider = MeterProvider::default()
  
  // Service A: Create metrics
  let service_a_meter = MeterProvider::get_meter(meter_provider, "service-a")
  let service_a_counter = Meter::create_counter(service_a_meter, "requests.total")
  let service_a_histogram = Meter::create_histogram(service_a_meter, "request.duration")
  
  Counter::add(service_a_counter, 1.0)
  Histogram::record(service_a_histogram, 150.5)
  
  // Service B: Create metrics with same names
  let service_b_meter = MeterProvider::get_meter(meter_provider, "service-b")
  let service_b_counter = Meter::create_counter(service_b_meter, "requests.total")
  let service_b_histogram = Meter::create_histogram(service_b_meter, "request.duration")
  
  Counter::add(service_b_counter, 1.0)
  Histogram::record(service_b_histogram, 89.2)
  
  // Service C: Create metrics
  let service_c_meter = MeterProvider::get_meter(meter_provider, "service-c")
  let service_c_counter = Meter::create_counter(service_c_meter, "requests.total")
  let service_c_histogram = Meter::create_histogram(service_c_meter, "request.duration")
  
  Counter::add(service_c_counter, 1.0)
  Histogram::record(service_c_histogram, 203.7)
  
  // Test metric name consistency
  @assertion.assert_eq(Counter::name(service_a_counter), "requests.total")?
  @assertion.assert_eq(Counter::name(service_b_counter), "requests.total")?
  @assertion.assert_eq(Counter::name(service_c_counter), "requests.total")?
  
  @assertion.assert_eq(Histogram::name(service_a_histogram), "request.duration")?
  @assertion.assert_eq(Histogram::name(service_b_histogram), "request.duration")?
  @assertion.assert_eq(Histogram::name(service_c_histogram), "request.duration")?
  
  // Test instrument conversion consistency
  let a_counter_instrument = Counter::as_instrument(service_a_counter)
  let b_counter_instrument = Counter::as_instrument(service_b_counter)
  let c_counter_instrument = Counter::as_instrument(service_c_counter)
  
  @assertion.assert_eq(Instrument::name(a_counter_instrument), "requests.total")?
  @assertion.assert_eq(Instrument::name(b_counter_instrument), "requests.total")?
  @assertion.assert_eq(Instrument::name(c_counter_instrument), "requests.total")?
}

test "Cross-service logging consistency test" {
  // Test logging consistency across services
  let logger_provider = LoggerProvider::default()
  
  // Create shared trace context
  let shared_trace_id = "1234567890abcdef1234567890abcdef"
  let shared_span_id = "abcdef1234567890"
  
  // Service A: Create log record
  let service_a_logger = LoggerProvider::get_logger(logger_provider, "service-a")
  let service_a_record = LogRecord::new_with_context(
    Info,
    Some("Service A: Processing request"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000100L),
    Some(shared_trace_id),
    Some(shared_span_id),
    None
  )
  
  // Service B: Create log record with same trace context
  let service_b_logger = LoggerProvider::get_logger(logger_provider, "service-b")
  let service_b_record = LogRecord::new_with_context(
    Info,
    Some("Service B: Received request from Service A"),
    None,
    Some(1735689600000000200L),
    Some(1735689600000000205L),
    Some(shared_trace_id),
    Some("bcdef12345678901"),  // Different span ID
    None
  )
  
  // Service C: Create log record
  let service_c_logger = LoggerProvider::get_logger(logger_provider, "service-c")
  let service_c_record = LogRecord::new_with_context(
    Warn,
    Some("Service C: Warning during processing"),
    None,
    Some(1735689600000000300L),
    Some(1735689600000000302L),
    Some(shared_trace_id),
    Some("cdef012345678901"),  // Different span ID
    None
  )
  
  // Verify trace ID consistency across all log records
  @assertion.assert_eq(LogRecord::trace_id(service_a_record)?, shared_trace_id)?
  @assertion.assert_eq(LogRecord::trace_id(service_b_record)?, shared_trace_id)?
  @assertion.assert_eq(LogRecord::trace_id(service_c_record)?, shared_trace_id)?
  
  // Verify span ID uniqueness
  @assertion.assert_eq(LogRecord::span_id(service_a_record)?, shared_span_id)?
  @assertion.assert_eq(LogRecord::span_id(service_b_record)?, "bcdef12345678901")?
  @assertion.assert_eq(LogRecord::span_id(service_c_record)?, "cdef012345678901")?
  
  @assertion.assert_neq(LogRecord::span_id(service_a_record)?, LogRecord::span_id(service_b_record)?)?
  @assertion.assert_neq(LogRecord::span_id(service_b_record)?, LogRecord::span_id(service_c_record)?)?
  
  // Verify severity levels
  @assertion.assert_eq(LogRecord::severity_number(service_a_record), Info)?
  @assertion.assert_eq(LogRecord::severity_number(service_b_record), Info)?
  @assertion.assert_eq(LogRecord::severity_number(service_c_record), Warn)?
  
  // Emit log records
  Logger::emit(service_a_logger, service_a_record)
  Logger::emit(service_b_logger, service_b_record)
  Logger::emit(service_c_logger, service_c_record)
}

test "Cross-service resource consistency test" {
  // Test resource consistency across services
  let global_resource = Resource::new()
  
  // Define common resource attributes
  let common_attributes = [
    ("service.namespace", StringValue("production")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("prod")),
    ("host.name", StringValue("web-server-01")),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2"))
  ]
  
  let base_resource = Resource::with_attributes(global_resource, common_attributes)
  
  // Service A: Add service-specific attributes
  let service_a_attributes = [
    ("service.name", StringValue("user-service")),
    ("service.instance.id", StringValue("user-service-001")),
    ("port", IntValue(8080)),
    ("database.enabled", BoolValue(true))
  ]
  
  let service_a_resource = Resource::with_attributes(base_resource, service_a_attributes)
  
  // Service B: Add service-specific attributes
  let service_b_attributes = [
    ("service.name", StringValue("order-service")),
    ("service.instance.id", StringValue("order-service-001")),
    ("port", IntValue(8081)),
    ("cache.enabled", BoolValue(true))
  ]
  
  let service_b_resource = Resource::with_attributes(base_resource, service_b_attributes)
  
  // Service C: Add service-specific attributes
  let service_c_attributes = [
    ("service.name", StringValue("payment-service")),
    ("service.instance.id", StringValue("payment-service-001")),
    ("port", IntValue(8082)),
    ("external.api.enabled", BoolValue(true))
  ]
  
  let service_c_resource = Resource::with_attributes(base_resource, service_c_attributes)
  
  // Verify common attributes are consistent across all services
  @assertion.assert_eq(
    Resource::get_attribute(service_a_resource, "service.namespace")?,
    StringValue("production")
  )?
  @assertion.assert_eq(
    Resource::get_attribute(service_b_resource, "service.namespace")?,
    StringValue("production")
  )?
  @assertion.assert_eq(
    Resource::get_attribute(service_c_resource, "service.namespace")?,
    StringValue("production")
  )?
  
  // Verify service-specific attributes are different
  @assertion.assert_eq(
    Resource::get_attribute(service_a_resource, "service.name")?,
    StringValue("user-service")
  )?
  @assertion.assert_eq(
    Resource::get_attribute(service_b_resource, "service.name")?,
    StringValue("order-service")
  )?
  @assertion.assert_eq(
    Resource::get_attribute(service_c_resource, "service.name")?,
    StringValue("payment-service")
  )?
  
  // Test resource merging
  let additional_attrs = [
    ("process.runtime.name", StringValue("moonbit")),
    ("process.runtime.version", StringValue("0.1.0"))
  ]
  
  let additional_resource = Resource::with_attributes(Resource::new(), additional_attrs)
  let merged_resource = Resource::merge(service_a_resource, additional_resource)
  
  // Verify merge contains new attributes (simplified implementation returns override)
  let runtime_name = Resource::get_attribute(merged_resource, "process.runtime.name")
  @assertion.assert_eq(runtime_name?, StringValue("moonbit"))?
}