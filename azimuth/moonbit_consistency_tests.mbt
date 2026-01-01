// MoonBit cross-service consistency tests for Azimuth telemetry system
// These tests focus on ensuring consistency across different services and components

// Test 1: Distributed trace consistency
pub test "分布式追踪一致性测试" {
  // Simulate a distributed trace across multiple services
  
  // Service A - Entry point
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let root_span_ctx = azimuth::SpanContext::new("trace-12345", "span-a-root", true, "key1=value1")
  let root_span = azimuth::Span::new("service-a-operation", azimuth::Server, root_span_ctx)
  
  // Service B - Called by Service A
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  let span_b_ctx = azimuth::SpanContext::new("trace-12345", "span-b-child", true, "key1=value1,key2=value2")
  let span_b = azimuth::Span::new("service-b-operation", azimuth::Server, span_b_ctx)
  
  // Service C - Called by Service B
  let service_c_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-c")
  let span_c_ctx = azimuth::SpanContext::new("trace-12345", "span-c-grandchild", true, "key1=value1,key2=value2,key3=value3")
  let span_c = azimuth::Span::new("service-c-operation", azimuth::Internal, span_c_ctx)
  
  // Service D - Parallel call from Service B
  let service_d_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-d")
  let span_d_ctx = azimuth::SpanContext::new("trace-12345", "span-d-parallel", true, "key1=value1,key2=value2,key4=value4")
  let span_d = azimuth::Span::new("service-d-operation", azimuth::Client, span_d_ctx)
  
  // Verify trace ID consistency across all services
  assert_eq(azimuth::SpanContext::trace_id(root_span_ctx), "trace-12345")
  assert_eq(azimuth::SpanContext::trace_id(span_b_ctx), "trace-12345")
  assert_eq(azimuth::SpanContext::trace_id(span_c_ctx), "trace-12345")
  assert_eq(azimuth::SpanContext::trace_id(span_d_ctx), "trace-12345")
  
  // Verify span ID uniqueness
  assert_true(azimuth::SpanContext::span_id(root_span_ctx) != azimuth::SpanContext::span_id(span_b_ctx))
  assert_true(azimuth::SpanContext::span_id(span_b_ctx) != azimuth::SpanContext::span_id(span_c_ctx))
  assert_true(azimuth::SpanContext::span_id(root_span_ctx) != azimuth::SpanContext::span_id(span_c_ctx))
  assert_true(azimuth::SpanContext::span_id(span_b_ctx) != azimuth::SpanContext::span_id(span_d_ctx))
  assert_true(azimuth::SpanContext::span_id(span_c_ctx) != azimuth::SpanContext::span_id(span_d_ctx))
  assert_true(azimuth::SpanContext::span_id(root_span_ctx) != azimuth::SpanContext::span_id(span_d_ctx))
  
  // Verify sampling consistency
  assert_true(azimuth::SpanContext::is_sampled(root_span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_b_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_c_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_d_ctx))
  
  // Verify trace state propagation
  assert_eq(azimuth::SpanContext::trace_state(root_span_ctx), "key1=value1")
  assert_eq(azimuth::SpanContext::trace_state(span_b_ctx), "key1=value1,key2=value2")
  assert_eq(azimuth::SpanContext::trace_state(span_c_ctx), "key1=value1,key2=value2,key3=value3")
  assert_eq(azimuth::SpanContext::trace_state(span_d_ctx), "key1=value1,key2=value2,key4=value4")
  
  // Verify span names
  assert_eq(azimuth::Span::name(root_span), "service-a-operation")
  assert_eq(azimuth::Span::name(span_b), "service-b-operation")
  assert_eq(azimuth::Span::name(span_c), "service-c-operation")
  assert_eq(azimuth::Span::name(span_d), "service-d-operation")
  
  // Verify span kinds
  assert_eq(azimuth::Span::kind(root_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_b), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_c), azimuth::Internal)
  assert_eq(azimuth::Span::kind(span_d), azimuth::Client)
  
  // Add events to all spans
  azimuth::Span::add_event(root_span, "service-a-event", Some([("service", azimuth::StringValue("service-a"))]))
  azimuth::Span::add_event(span_b, "service-b-event", Some([("service", azimuth::StringValue("service-b"))]))
  azimuth::Span::add_event(span_c, "service-c-event", Some([("service", azimuth::StringValue("service-c"))]))
  azimuth::Span::add_event(span_d, "service-d-event", Some([("service", azimuth::StringValue("service-d"))]))
  
  // Set status for all spans
  azimuth::Span::set_status(root_span, azimuth::Ok)
  azimuth::Span::set_status(span_b, azimuth::Ok)
  azimuth::Span::set_status(span_c, azimuth::Ok)
  azimuth::Span::set_status(span_d, azimuth::Ok)
  
  // End all spans
  azimuth::Span::end(root_span)
  azimuth::Span::end(span_b)
  azimuth::Span::end(span_c)
  azimuth::Span::end(span_d)
}

// Test 2: Cross-service baggage propagation
pub test "跨服务Baggage传播测试" {
  // Simulate baggage propagation across multiple services
  
  // Service A - Initial baggage
  let baggage_a = azimuth::Baggage::new()
  let baggage_a_with_user = azimuth::Baggage::set_entry(baggage_a, "user.id", "12345")
  let baggage_a_with_session = azimuth::Baggage::set_entry(baggage_a_with_user, "session.id", "session-67890")
  let baggage_a_final = azimuth::Baggage::set_entry(baggage_a_with_session, "request.id", "req-11111")
  
  // Service B - Receives baggage from A and adds more
  let baggage_b = baggage_a_final
  let baggage_b_with_tenant = azimuth::Baggage::set_entry(baggage_b, "tenant.id", "tenant-22222")
  let baggage_b_final = azimuth::Baggage::set_entry(baggage_b_with_tenant, "operation.type", "read")
  
  // Service C - Receives baggage from B and adds more
  let baggage_c = baggage_b_final
  let baggage_c_with_region = azimuth::Baggage::set_entry(baggage_c, "region", "us-west-2")
  let baggage_c_final = azimuth::Baggage::set_entry(baggage_c_with_region, "data.center", "dc-1")
  
  // Service D - Receives baggage from C and adds more
  let baggage_d = baggage_c_final
  let baggage_d_with_version = azimuth::Baggage::set_entry(baggage_d, "api.version", "v1.2.3")
  let baggage_d_final = azimuth::Baggage::set_entry(baggage_d_with_version, "client.type", "mobile")
  
  // Verify baggage propagation consistency
  // Original entries should be preserved throughout the chain
  assert_eq(azimuth::Baggage::get_entry(baggage_a_final, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b_final, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c_final, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d_final, "user.id"), Some("12345"))
  
  assert_eq(azimuth::Baggage::get_entry(baggage_a_final, "session.id"), Some("session-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b_final, "session.id"), Some("session-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c_final, "session.id"), Some("session-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d_final, "session.id"), Some("session-67890"))
  
  assert_eq(azimuth::Baggage::get_entry(baggage_a_final, "request.id"), Some("req-11111"))
  assert_eq(azimuth::Baggage::get_entry(baggage_b_final, "request.id"), Some("req-11111"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c_final, "request.id"), Some("req-11111"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d_final, "request.id"), Some("req-11111"))
  
  // Service B additions should be preserved in C and D
  assert_eq(azimuth::Baggage::get_entry(baggage_b_final, "tenant.id"), Some("tenant-22222"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c_final, "tenant.id"), Some("tenant-22222"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d_final, "tenant.id"), Some("tenant-22222"))
  
  assert_eq(azimuth::Baggage::get_entry(baggage_b_final, "operation.type"), Some("read"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c_final, "operation.type"), Some("read"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d_final, "operation.type"), Some("read"))
  
  // Service C additions should be preserved in D
  assert_eq(azimuth::Baggage::get_entry(baggage_c_final, "region"), Some("us-west-2"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d_final, "region"), Some("us-west-2"))
  
  assert_eq(azimuth::Baggage::get_entry(baggage_c_final, "data.center"), Some("dc-1"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d_final, "data.center"), Some("dc-1"))
  
  // Service D additions should only be in D
  assert_eq(azimuth::Baggage::get_entry(baggage_d_final, "api.version"), Some("v1.2.3"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d_final, "client.type"), Some("mobile"))
  
  // Verify non-existent entries
  assert_eq(azimuth::Baggage::get_entry(baggage_a_final, "tenant.id"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_a_final, "region"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_a_final, "api.version"), None)
}

// Test 3: Cross-service context propagation
pub test "跨服务上下文传播测试" {
  // Simulate context propagation across multiple services
  
  // Service A - Initial context
  let ctx_a = azimuth::Context::root()
  let user_key = azimuth::ContextKey::new("user.id")
  let ctx_a_with_user = azimuth::Context::with_value(ctx_a, user_key, "12345")
  let session_key = azimuth::ContextKey::new("session.id")
  let ctx_a_final = azimuth::Context::with_value(ctx_a_with_user, session_key, "session-67890")
  
  // Service B - Receives context from A and adds more
  let ctx_b = ctx_a_final
  let tenant_key = azimuth::ContextKey::new("tenant.id")
  let ctx_b_with_tenant = azimuth::Context::with_value(ctx_b, tenant_key, "tenant-22222")
  let operation_key = azimuth::ContextKey::new("operation.type")
  let ctx_b_final = azimuth::Context::with_value(ctx_b_with_tenant, operation_key, "read")
  
  // Service C - Receives context from B and adds more
  let ctx_c = ctx_b_final
  let region_key = azimuth::ContextKey::new("region")
  let ctx_c_with_region = azimuth::Context::with_value(ctx_c, region_key, "us-west-2")
  let datacenter_key = azimuth::ContextKey::new("data.center")
  let ctx_c_final = azimuth::Context::with_value(ctx_c_with_region, datacenter_key, "dc-1")
  
  // Service D - Receives context from C and adds more
  let ctx_d = ctx_c_final
  let version_key = azimuth::ContextKey::new("api.version")
  let ctx_d_with_version = azimuth::Context::with_value(ctx_d, version_key, "v1.2.3")
  let client_key = azimuth::ContextKey::new("client.type")
  let ctx_d_final = azimuth::Context::with_value(ctx_d_with_version, client_key, "mobile")
  
  // Verify context propagation consistency
  // Original entries should be preserved throughout the chain
  assert_eq(azimuth::Context::get(ctx_a_final, user_key), Some("12345"))
  assert_eq(azimuth::Context::get(ctx_b_final, user_key), Some("12345"))
  assert_eq(azimuth::Context::get(ctx_c_final, user_key), Some("12345"))
  assert_eq(azimuth::Context::get(ctx_d_final, user_key), Some("12345"))
  
  assert_eq(azimuth::Context::get(ctx_a_final, session_key), Some("session-67890"))
  assert_eq(azimuth::Context::get(ctx_b_final, session_key), Some("session-67890"))
  assert_eq(azimuth::Context::get(ctx_c_final, session_key), Some("session-67890"))
  assert_eq(azimuth::Context::get(ctx_d_final, session_key), Some("session-67890"))
  
  // Service B additions should be preserved in C and D
  assert_eq(azimuth::Context::get(ctx_b_final, tenant_key), Some("tenant-22222"))
  assert_eq(azimuth::Context::get(ctx_c_final, tenant_key), Some("tenant-22222"))
  assert_eq(azimuth::Context::get(ctx_d_final, tenant_key), Some("tenant-22222"))
  
  assert_eq(azimuth::Context::get(ctx_b_final, operation_key), Some("read"))
  assert_eq(azimuth::Context::get(ctx_c_final, operation_key), Some("read"))
  assert_eq(azimuth::Context::get(ctx_d_final, operation_key), Some("read"))
  
  // Service C additions should be preserved in D
  assert_eq(azimuth::Context::get(ctx_c_final, region_key), Some("us-west-2"))
  assert_eq(azimuth::Context::get(ctx_d_final, region_key), Some("us-west-2"))
  
  assert_eq(azimuth::Context::get(ctx_c_final, datacenter_key), Some("dc-1"))
  assert_eq(azimuth::Context::get(ctx_d_final, datacenter_key), Some("dc-1"))
  
  // Service D additions should only be in D
  assert_eq(azimuth::Context::get(ctx_d_final, version_key), Some("v1.2.3"))
  assert_eq(azimuth::Context::get(ctx_d_final, client_key), Some("mobile"))
  
  // Verify non-existent entries
  let nonexistent_key = azimuth::ContextKey::new("nonexistent.key")
  assert_eq(azimuth::Context::get(ctx_a_final, nonexistent_key), None)
  assert_eq(azimuth::Context::get(ctx_b_final, nonexistent_key), None)
  assert_eq(azimuth::Context::get(ctx_c_final, nonexistent_key), None)
  assert_eq(azimuth::Context::get(ctx_d_final, nonexistent_key), None)
}

// Test 4: Cross-service metrics consistency
pub test "跨服务度量一致性测试" {
  // Simulate metrics collection across multiple services
  
  // Service A - Create metrics
  let meter_a = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "service-a")
  let counter_a = azimuth::Meter::create_counter(meter_a, "request.count", Some("Total requests"), Some("count"))
  let histogram_a = azimuth::Meter::create_histogram(meter_a, "response.time", Some("Response time"), Some("ms"))
  
  // Service B - Create metrics with same names
  let meter_b = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "service-b")
  let counter_b = azimuth::Meter::create_counter(meter_b, "request.count", Some("Total requests"), Some("count"))
  let histogram_b = azimuth::Meter::create_histogram(meter_b, "response.time", Some("Response time"), Some("ms"))
  
  // Service C - Create metrics with same names
  let meter_c = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "service-c")
  let counter_c = azimuth::Meter::create_counter(meter_c, "request.count", Some("Total requests"), Some("count"))
  let histogram_c = azimuth::Meter::create_histogram(meter_c, "response.time", Some("Response time"), Some("ms"))
  
  // Service D - Create metrics with same names
  let meter_d = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "service-d")
  let counter_d = azimuth::Meter::create_counter(meter_d, "request.count", Some("Total requests"), Some("count"))
  let histogram_d = azimuth::Meter::create_histogram(meter_d, "response.time", Some("Response time"), Some("ms"))
  
  // Verify metric name consistency across services
  assert_eq(counter_a.name, "request.count")
  assert_eq(counter_b.name, "request.count")
  assert_eq(counter_c.name, "request.count")
  assert_eq(counter_d.name, "request.count")
  
  assert_eq(histogram_a.name, "response.time")
  assert_eq(histogram_b.name, "response.time")
  assert_eq(histogram_c.name, "response.time")
  assert_eq(histogram_d.name, "response.time")
  
  // Verify metric description consistency across services
  assert_eq(counter_a.description, Some("Total requests"))
  assert_eq(counter_b.description, Some("Total requests"))
  assert_eq(counter_c.description, Some("Total requests"))
  assert_eq(counter_d.description, Some("Total requests"))
  
  assert_eq(histogram_a.description, Some("Response time"))
  assert_eq(histogram_b.description, Some("Response time"))
  assert_eq(histogram_c.description, Some("Response time"))
  assert_eq(histogram_d.description, Some("Response time"))
  
  // Verify metric unit consistency across services
  assert_eq(counter_a.unit, Some("count"))
  assert_eq(counter_b.unit, Some("count"))
  assert_eq(counter_c.unit, Some("count"))
  assert_eq(counter_d.unit, Some("count"))
  
  assert_eq(histogram_a.unit, Some("ms"))
  assert_eq(histogram_b.unit, Some("ms"))
  assert_eq(histogram_c.unit, Some("ms"))
  assert_eq(histogram_d.unit, Some("ms"))
  
  // Perform metric operations
  azimuth::Counter::add(counter_a, 10.0)
  azimuth::Counter::add(counter_b, 20.0)
  azimuth::Counter::add(counter_c, 30.0)
  azimuth::Counter::add(counter_d, 40.0)
  
  azimuth::Histogram::record(histogram_a, 100.0)
  azimuth::Histogram::record(histogram_b, 200.0)
  azimuth::Histogram::record(histogram_c, 150.0)
  azimuth::Histogram::record(histogram_d, 250.0)
  
  // Verify instrument conversion consistency
  let instrument_a = azimuth::Histogram::as_instrument(histogram_a)
  let instrument_b = azimuth::Histogram::as_instrument(histogram_b)
  let instrument_c = azimuth::Histogram::as_instrument(histogram_c)
  let instrument_d = azimuth::Histogram::as_instrument(histogram_d)
  
  assert_eq(azimuth::Instrument::name(instrument_a), "response.time")
  assert_eq(azimuth::Instrument::name(instrument_b), "response.time")
  assert_eq(azimuth::Instrument::name(instrument_c), "response.time")
  assert_eq(azimuth::Instrument::name(instrument_d), "response.time")
  
  assert_eq(azimuth::Instrument::description(instrument_a), Some("Response time"))
  assert_eq(azimuth::Instrument::description(instrument_b), Some("Response time"))
  assert_eq(azimuth::Instrument::description(instrument_c), Some("Response time"))
  assert_eq(azimuth::Instrument::description(instrument_d), Some("Response time"))
  
  assert_eq(azimuth::Instrument::unit(instrument_a), Some("ms"))
  assert_eq(azimuth::Instrument::unit(instrument_b), Some("ms"))
  assert_eq(azimuth::Instrument::unit(instrument_c), Some("ms"))
  assert_eq(azimuth::Instrument::unit(instrument_d), Some("ms"))
}

// Test 5: Cross-service logging consistency
pub test "跨服务日志一致性测试" {
  // Simulate logging across multiple services
  
  // Service A - Create logger and log records
  let logger_a = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-a")
  let log_record_a1 = azimuth::LogRecord::new(azimuth::Info, "Service A operation started")
  let log_record_a2 = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Service A operation completed with warnings"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-12345"),
    Some("span-a-123"),
    Some(azimuth::Context::root())
  )
  
  // Service B - Create logger and log records
  let logger_b = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-b")
  let log_record_b1 = azimuth::LogRecord::new(azimuth::Info, "Service B operation started")
  let log_record_b2 = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Service B operation failed"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some("trace-12345"),
    Some("span-b-456"),
    Some(azimuth::Context::root())
  )
  
  // Service C - Create logger and log records
  let logger_c = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-c")
  let log_record_c1 = azimuth::LogRecord::new(azimuth::Info, "Service C operation started")
  let log_record_c2 = azimuth::LogRecord::new_with_context(
    azimuth::Debug,
    Some("Service C operation completed"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000004L),
    Some(1735689600000000005L),
    Some("trace-12345"),
    Some("span-c-789"),
    Some(azimuth::Context::root())
  )
  
  // Service D - Create logger and log records
  let logger_d = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-d")
  let log_record_d1 = azimuth::LogRecord::new(azimuth::Info, "Service D operation started")
  let log_record_d2 = azimuth::LogRecord::new_with_context(
    azimuth::Fatal,
    Some("Service D operation failed fatally"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000006L),
    Some(1735689600000000007L),
    Some("trace-12345"),
    Some("span-d-012"),
    Some(azimuth::Context::root())
  )
  
  // Verify logger scope consistency
  assert_eq(logger_a.scope.name, "service-a")
  assert_eq(logger_b.scope.name, "service-b")
  assert_eq(logger_c.scope.name, "service-c")
  assert_eq(logger_d.scope.name, "service-d")
  
  // Verify log record severity consistency
  assert_eq(azimuth::LogRecord::severity_number(log_record_a1), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_record_a2), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(log_record_b1), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_record_b2), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(log_record_c1), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_record_c2), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(log_record_d1), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(log_record_d2), azimuth::Fatal)
  
  // Verify log record body consistency
  assert_eq(azimuth::LogRecord::body(log_record_a1), Some("Service A operation started"))
  assert_eq(azimuth::LogRecord::body(log_record_a2), Some("Service A operation completed with warnings"))
  assert_eq(azimuth::LogRecord::body(log_record_b1), Some("Service B operation started"))
  assert_eq(azimuth::LogRecord::body(log_record_b2), Some("Service B operation failed"))
  assert_eq(azimuth::LogRecord::body(log_record_c1), Some("Service C operation started"))
  assert_eq(azimuth::LogRecord::body(log_record_c2), Some("Service C operation completed"))
  assert_eq(azimuth::LogRecord::body(log_record_d1), Some("Service D operation started"))
  assert_eq(azimuth::LogRecord::body(log_record_d2), Some("Service D operation failed fatally"))
  
  // Verify trace ID consistency across services
  assert_eq(azimuth::LogRecord::trace_id(log_record_a2), Some("trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(log_record_b2), Some("trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(log_record_c2), Some("trace-12345"))
  assert_eq(azimuth::LogRecord::trace_id(log_record_d2), Some("trace-12345"))
  
  // Verify span ID uniqueness across services
  assert_eq(azimuth::LogRecord::span_id(log_record_a2), Some("span-a-123"))
  assert_eq(azimuth::LogRecord::span_id(log_record_b2), Some("span-b-456"))
  assert_eq(azimuth::LogRecord::span_id(log_record_c2), Some("span-c-789"))
  assert_eq(azimuth::LogRecord::span_id(log_record_d2), Some("span-d-012"))
  
  assert_true(azimuth::LogRecord::span_id(log_record_a2) != azimuth::LogRecord::span_id(log_record_b2))
  assert_true(azimuth::LogRecord::span_id(log_record_b2) != azimuth::LogRecord::span_id(log_record_c2))
  assert_true(azimuth::LogRecord::span_id(log_record_c2) != azimuth::LogRecord::span_id(log_record_d2))
  assert_true(azimuth::LogRecord::span_id(log_record_a2) != azimuth::LogRecord::span_id(log_record_c2))
  assert_true(azimuth::LogRecord::span_id(log_record_a2) != azimuth::LogRecord::span_id(log_record_d2))
  assert_true(azimuth::LogRecord::span_id(log_record_b2) != azimuth::LogRecord::span_id(log_record_d2))
  
  // Emit all log records
  azimuth::Logger::emit(logger_a, log_record_a1)
  azimuth::Logger::emit(logger_a, log_record_a2)
  azimuth::Logger::emit(logger_b, log_record_b1)
  azimuth::Logger::emit(logger_b, log_record_b2)
  azimuth::Logger::emit(logger_c, log_record_c1)
  azimuth::Logger::emit(logger_c, log_record_c2)
  azimuth::Logger::emit(logger_d, log_record_d1)
  azimuth::Logger::emit(logger_d, log_record_d2)
}

// Test 6: Cross-service resource consistency
pub test "跨服务资源一致性测试" {
  // Simulate resource configuration across multiple services
  
  // Service A - Resource configuration
  let resource_a_attrs = [
    ("service.name", azimuth::StringValue("service-a")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-a-123")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("host-a.example.com"))
  ]
  let resource_a = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_a_attrs)
  
  // Service B - Resource configuration
  let resource_b_attrs = [
    ("service.name", azimuth::StringValue("service-b")),
    ("service.version", azimuth::StringValue("1.2.3")),
    ("service.instance.id", azimuth::StringValue("instance-b-456")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("host-b.example.com"))
  ]
  let resource_b = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_b_attrs)
  
  // Service C - Resource configuration
  let resource_c_attrs = [
    ("service.name", azimuth::StringValue("service-c")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-c-789")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("host-c.example.com"))
  ]
  let resource_c = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_c_attrs)
  
  // Service D - Resource configuration
  let resource_d_attrs = [
    ("service.name", azimuth::StringValue("service-d")),
    ("service.version", azimuth::StringValue("0.9.5")),
    ("service.instance.id", azimuth::StringValue("instance-d-012")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("host-d.example.com"))
  ]
  let resource_d = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_d_attrs)
  
  // Verify service name consistency pattern
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.name"), Some(azimuth::StringValue("service-a")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.name"), Some(azimuth::StringValue("service-b")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.name"), Some(azimuth::StringValue("service-c")))
  assert_eq(azimuth::Resource::get_attribute(resource_d, "service.name"), Some(azimuth::StringValue("service-d")))
  
  // Verify service version pattern
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.version"), Some(azimuth::StringValue("1.2.3")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_d, "service.version"), Some(azimuth::StringValue("0.9.5")))
  
  // Verify service instance ID pattern
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.instance.id"), Some(azimuth::StringValue("instance-a-123")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.instance.id"), Some(azimuth::StringValue("instance-b-456")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.instance.id"), Some(azimuth::StringValue("instance-c-789")))
  assert_eq(azimuth::Resource::get_attribute(resource_d, "service.instance.id"), Some(azimuth::StringValue("instance-d-012")))
  
  // Verify deployment environment consistency across services
  assert_eq(azimuth::Resource::get_attribute(resource_a, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(resource_d, "deployment.environment"), Some(azimuth::StringValue("production")))
  
  // Verify host name pattern
  assert_eq(azimuth::Resource::get_attribute(resource_a, "host.name"), Some(azimuth::StringValue("host-a.example.com")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "host.name"), Some(azimuth::StringValue("host-b.example.com")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "host.name"), Some(azimuth::StringValue("host-c.example.com")))
  assert_eq(azimuth::Resource::get_attribute(resource_d, "host.name"), Some(azimuth::StringValue("host-d.example.com")))
  
  // Verify missing attributes
  assert_eq(azimuth::Resource::get_attribute(resource_a, "missing.attr"), None)
  assert_eq(azimuth::Resource::get_attribute(resource_b, "missing.attr"), None)
  assert_eq(azimuth::Resource::get_attribute(resource_c, "missing.attr"), None)
  assert_eq(azimuth::Resource::get_attribute(resource_d, "missing.attr"), None)
  
  // Create common resource attributes for all services
  let common_attrs = [
    ("deployment.environment", azimuth::StringValue("production")),
    ("data.center", azimuth::StringValue("dc-west-1")),
    ("region", azimuth::StringValue("us-west-2")),
    ("team", azimuth::StringValue("platform"))
  ]
  let common_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), common_attrs)
  
  // Merge common resource with each service resource
  let merged_resource_a = azimuth::Resource::merge(resource_a, common_resource)
  let merged_resource_b = azimuth::Resource::merge(resource_b, common_resource)
  let merged_resource_c = azimuth::Resource::merge(resource_c, common_resource)
  let merged_resource_d = azimuth::Resource::merge(resource_d, common_resource)
  
  // Verify common attributes are present in all merged resources
  assert_eq(azimuth::Resource::get_attribute(merged_resource_a, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_b, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_c, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_d, "deployment.environment"), Some(azimuth::StringValue("production")))
  
  assert_eq(azimuth::Resource::get_attribute(merged_resource_a, "data.center"), Some(azimuth::StringValue("dc-west-1")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_b, "data.center"), Some(azimuth::StringValue("dc-west-1")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_c, "data.center"), Some(azimuth::StringValue("dc-west-1")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_d, "data.center"), Some(azimuth::StringValue("dc-west-1")))
  
  assert_eq(azimuth::Resource::get_attribute(merged_resource_a, "region"), Some(azimuth::StringValue("us-west-2")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_b, "region"), Some(azimuth::StringValue("us-west-2")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_c, "region"), Some(azimuth::StringValue("us-west-2")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_d, "region"), Some(azimuth::StringValue("us-west-2")))
  
  assert_eq(azimuth::Resource::get_attribute(merged_resource_a, "team"), Some(azimuth::StringValue("platform")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_b, "team"), Some(azimuth::StringValue("platform")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_c, "team"), Some(azimuth::StringValue("platform")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource_d, "team"), Some(azimuth::StringValue("platform")))
}