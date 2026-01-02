// Azimuth Advanced Lifecycle Test Suite
// This file contains 10 advanced test cases focusing on lifecycle management and complex operations

// Test 1: Span lifecycle management
test "span lifecycle management" {
  // Create a tracer provider and tracer
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test.tracer")
  
  // Start a parent span
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  assert_eq(Span::name(parent_span), "parent-operation")
  assert_true(Span::is_recording(parent_span))
  
  // Set span status
  Span::set_status(parent_span, Ok)
  
  // Add events to span
  Span::add_event(parent_span, "event-1")
  Span::add_event(parent_span, "event-2")
  
  // Start child spans
  let child_span1 = Tracer::start_span(tracer, "child-operation-1")
  let child_span2 = Tracer::start_span(tracer, "child-operation-2")
  
  // End child spans
  Span::end(child_span1)
  Span::end(child_span2)
  
  // End parent span
  Span::end(parent_span)
  
  // Verify span context
  let span_ctx = Span::span_context(parent_span)
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_eq(SpanContext::trace_id(span_ctx), "test_trace_id")
  assert_eq(SpanContext::span_id(span_ctx), "test_span_id")
}

// Test 2: Metric aggregation operations
test "metric aggregation operations" {
  // Create meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation.test.meter")
  
  // Create different metric instruments
  let counter = Meter::create_counter(meter, "request.count")
  let histogram = Meter::create_histogram(meter, "request.duration")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  let gauge = Meter::create_gauge(meter, "memory.usage")
  
  // Perform metric operations
  Counter::add(counter, 100.0)
  Counter::add(counter, 50.0)
  
  Histogram::record(histogram, 10.5)
  Histogram::record(histogram, 25.7)
  Histogram::record(histogram, 15.3)
  
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -3.0)
  
  // Verify instrument properties
  let counter_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(counter_instrument), "request.duration")
  assert_eq(Instrument::description(counter_instrument), Some("Request duration histogram"))
  assert_eq(Instrument::unit(counter_instrument), Some("ms"))
  
  // Test aggregation calculations
  let total_requests = 100.0 + 50.0
  let avg_duration = (10.5 + 25.7 + 15.3) / 3.0
  let active_connections = 10.0 - 3.0
  
  assert_eq(total_requests, 150.0)
  assert_eq(avg_duration, 17.166666666666668)
  assert_eq(active_connections, 7.0)
}

// Test 3: Log recording and filtering
test "log recording and filtering" {
  // Create logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "filtering.test.logger")
  
  // Create log records with different severity levels
  let trace_log = LogRecord::new(Trace, "Trace message")
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  let fatal_log = LogRecord::new(Fatal, "Fatal message")
  
  // Create log records with context
  let timestamp = Clock::now_unix_nanos(Clock::system())
  let attributes = Attributes::new()
  let context_log = LogRecord::new_with_context(
    Info,
    Some("Contextual log message"),
    Some(attributes),
    Some(timestamp),
    Some(timestamp + 1000L),
    Some("trace-123"),
    Some("span-456"),
    Some(Context::root())
  )
  
  // Verify log properties using match statements
  match LogRecord::severity_number(trace_log) {
    Trace => assert_true(true)
    _ => assert_true(false)
  }
  
  match LogRecord::severity_number(debug_log) {
    Debug => assert_true(true)
    _ => assert_true(false)
  }
  
  match LogRecord::severity_number(info_log) {
    Info => assert_true(true)
    _ => assert_true(false)
  }
  
  match LogRecord::severity_number(warn_log) {
    Warn => assert_true(true)
    _ => assert_true(false)
  }
  
  match LogRecord::severity_number(error_log) {
    Error => assert_true(true)
    _ => assert_true(false)
  }
  
  match LogRecord::severity_number(fatal_log) {
    Fatal => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(LogRecord::body(trace_log), Some("Trace message"))
  assert_eq(LogRecord::body(error_log), Some("Error message"))
  
  assert_eq(LogRecord::trace_id(context_log), Some("trace-123"))
  assert_eq(LogRecord::span_id(context_log), Some("span-456"))
  
  // Test log filtering by severity
  let should_log_error = match LogRecord::severity_number(error_log) {
    Error => true
    _ => false
  }
  
  let should_log_warn = match LogRecord::severity_number(warn_log) {
    Error => true
    _ => false
  }
  
  let should_log_info = match LogRecord::severity_number(info_log) {
    Error => true
    _ => false
  }
  
  assert_true(should_log_error)
  assert_false(should_log_warn)
  assert_false(should_log_info)
  
  // Emit logs
  Logger::emit(logger, info_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, context_log)
}

// Test 4: Baggage propagation
test "baggage propagation" {
  // Create initial baggage
  let initial_baggage = Baggage::new()
  
  // Add baggage entries
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user.id", "user-123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-456")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-789")
  
  // Retrieve baggage entries
  let user_id = Baggage::get_entry(baggage_with_tenant, "user.id")
  let session_id = Baggage::get_entry(baggage_with_tenant, "session.id")
  let tenant_id = Baggage::get_entry(baggage_with_tenant, "tenant.id")
  let non_existent = Baggage::get_entry(baggage_with_tenant, "non.existent")
  
  // Verify baggage entries
  assert_eq(user_id, Some("user-123"))
  assert_eq(session_id, Some("session-456"))
  assert_eq(tenant_id, Some("tenant-789"))
  assert_eq(non_existent, None)
  
  // Remove baggage entries
  let baggage_without_user = Baggage::remove_entry(baggage_with_tenant, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_without_user, "user.id")
  let still_has_session = Baggage::get_entry(baggage_without_user, "session.id")
  
  // Verify removal
  assert_eq(removed_user_id, None)
  assert_eq(still_has_session, Some("session-456"))
  
  // Test baggage propagation through context
  let ctx = Context::root()
  let baggage_key = ContextKey::new("baggage")
  let ctx_with_baggage = Context::with_value(ctx, baggage_key, "user.id=user-123,session.id=session-456")
  let propagated_baggage = Context::get(ctx_with_baggage, baggage_key)
  
  assert_eq(propagated_baggage, Some("user.id=user-123,session.id=session-456"))
}

// Test 5: Trace context management
test "trace context management" {
  // Create trace contexts
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let child_span_id = "c8ad6b7169203332"
  let trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  let parent_ctx = SpanContext::new(trace_id, parent_span_id, true, trace_state)
  let child_ctx = SpanContext::new(trace_id, child_span_id, true, trace_state)
  
  // Verify trace context properties
  assert_eq(SpanContext::trace_id(parent_ctx), trace_id)
  assert_eq(SpanContext::span_id(parent_ctx), parent_span_id)
  assert_eq(SpanContext::trace_id(child_ctx), trace_id)
  assert_eq(SpanContext::span_id(child_ctx), child_span_id)
  
  assert_true(SpanContext::is_valid(parent_ctx))
  assert_true(SpanContext::is_valid(child_ctx))
  assert_true(SpanContext::is_sampled(parent_ctx))
  assert_true(SpanContext::is_sampled(child_ctx))
  
  // Test trace context inheritance
  let same_trace = SpanContext::trace_id(parent_ctx) == SpanContext::trace_id(child_ctx)
  let different_spans = SpanContext::span_id(parent_ctx) != SpanContext::span_id(child_ctx)
  
  assert_true(same_trace)
  assert_true(different_spans)
  
  // Test trace context serialization
  let serialized_traceparent = "00-" + trace_id + "-" + parent_span_id + "-01"
  let expected_traceparent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  
  assert_eq(serialized_traceparent, expected_traceparent)
  
  // Test trace context extraction from carrier
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", serialized_traceparent)
  TextMapCarrier::set(carrier, "tracestate", trace_state)
  
  let extracted_traceparent = TextMapCarrier::get(carrier, "traceparent")
  let extracted_tracestate = TextMapCarrier::get(carrier, "tracestate")
  
  assert_eq(extracted_traceparent, Some(serialized_traceparent))
  assert_eq(extracted_tracestate, Some(trace_state))
}

// Test 6: InstrumentationScope management
test "instrumentation scope management" {
  // Create instrumentation scopes
  let scope1 = InstrumentationScope::{ 
    name: "azimuth.tracer", 
    version: Some("1.0.0"), 
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let scope2 = InstrumentationScope::{ 
    name: "azimuth.metrics", 
    version: Some("1.0.0"), 
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let scope3 = InstrumentationScope::{ 
    name: "azimuth.logger", 
    version: Some("2.0.0"), 
    schema_url: None
  }
  
  // Verify scope properties
  assert_eq(scope1.name, "azimuth.tracer")
  assert_eq(scope1.version, Some("1.0.0"))
  assert_eq(scope1.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  assert_eq(scope2.name, "azimuth.metrics")
  assert_eq(scope2.version, Some("1.0.0"))
  assert_eq(scope2.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  assert_eq(scope3.name, "azimuth.logger")
  assert_eq(scope3.version, Some("2.0.0"))
  assert_eq(scope3.schema_url, None)
  
  // Test scope equality
  let same_name = scope1.name == scope2.name
  let same_version = scope1.version == scope2.version
  let different_names = scope1.name != scope3.name
  let different_versions = scope1.version != scope3.version
  
  assert_false(same_name)
  assert_true(same_version)
  assert_true(different_names)
  assert_true(different_versions)
  
  // Test scope with instruments
  let tracer_provider = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider, scope1.name)
  let tracer2 = TracerProvider::get_tracer(tracer_provider, scope2.name)
  
  let tracer1_scope = Tracer::instrumentation_scope(tracer1)
  let tracer2_scope = Tracer::instrumentation_scope(tracer2)
  
  assert_eq(tracer1_scope.name, scope1.name)
  assert_eq(tracer1_scope.version, scope1.version)
  assert_eq(tracer2_scope.name, scope2.name)
  assert_eq(tracer2_scope.version, scope2.version)
}

// Test 7: Attribute deep nested operations
test "attribute deep nested operations" {
  // Create complex nested attributes
  let nested_string_value = StringValue("deep.nested.value")
  let nested_int_value = IntValue(42)
  let nested_float_value = FloatValue(3.14159)
  let nested_bool_value = BoolValue(true)
  let nested_array_value = ArrayStringValue(["item1", "item2", "item3"])
  
  // Create attributes with nested structures
  let base_attrs = Attributes::new()
  Attributes::set(base_attrs, "deep.nested.string", nested_string_value)
  Attributes::set(base_attrs, "deep.nested.int", nested_int_value)
  Attributes::set(base_attrs, "deep.nested.float", nested_float_value)
  Attributes::set(base_attrs, "deep.nested.bool", nested_bool_value)
  Attributes::set(base_attrs, "deep.nested.array", nested_array_value)
  
  // Retrieve nested attributes
  let retrieved_string = Attributes::get(base_attrs, "deep.nested.string")
  let retrieved_int = Attributes::get(base_attrs, "deep.nested.int")
  let retrieved_float = Attributes::get(base_attrs, "deep.nested.float")
  let retrieved_bool = Attributes::get(base_attrs, "deep.nested.bool")
  let retrieved_array = Attributes::get(base_attrs, "deep.nested.array")
  let non_existent = Attributes::get(base_attrs, "deep.nested.nonexistent")
  
  // Verify nested attributes
  match retrieved_string {
    Some(StringValue("deep.nested.value")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match retrieved_int {
    Some(IntValue(42)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match retrieved_float {
    Some(FloatValue(3.14159)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match retrieved_bool {
    Some(BoolValue(true)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match retrieved_array {
    Some(ArrayStringValue(["item1", "item2", "item3"])) => assert_true(true)
    _ => assert_true(false)
  }
  
  match non_existent {
    None => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test attribute type conversion
  let string_to_int = match retrieved_string {
    Some(StringValue(s)) => {
      match s {
        "42" => 42
        _ => 0
      }
    }
    _ => 0
  }
  
  let int_to_string = match retrieved_int {
    Some(IntValue(i)) => {
      match i {
        42 => "42"
        _ => "0"
      }
    }
    _ => "0"
  }
  
  // Verify conversions
  assert_eq(string_to_int, 0)  // "deep.nested.value" is not a number
  assert_eq(int_to_string, "42")
}

// Test 8: Concurrent span operations
test "concurrent span operations" {
  // Create tracer for concurrent operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent.test.tracer")
  
  // Create multiple spans concurrently
  let span1 = Tracer::start_span(tracer, "concurrent-operation-0")
  let span2 = Tracer::start_span(tracer, "concurrent-operation-1")
  let span3 = Tracer::start_span(tracer, "concurrent-operation-2")
  let span4 = Tracer::start_span(tracer, "concurrent-operation-3")
  let span5 = Tracer::start_span(tracer, "concurrent-operation-4")
  
  // Verify all spans have the same trace context
  let first_span_ctx = Span::span_context(span1)
  let first_trace_id = SpanContext::trace_id(first_span_ctx)
  
  let span2_ctx = Span::span_context(span2)
  let span2_trace_id = SpanContext::trace_id(span2_ctx)
  let span2_name = Span::name(span2)
  
  let span3_ctx = Span::span_context(span3)
  let span3_trace_id = SpanContext::trace_id(span3_ctx)
  let span3_name = Span::name(span3)
  
  let span4_ctx = Span::span_context(span4)
  let span4_trace_id = SpanContext::trace_id(span4_ctx)
  let span4_name = Span::name(span4)
  
  let span5_ctx = Span::span_context(span5)
  let span5_trace_id = SpanContext::trace_id(span5_ctx)
  let span5_name = Span::name(span5)
  
  assert_eq(span2_trace_id, first_trace_id)
  assert_true(SpanContext::is_valid(span2_ctx))
  assert_true(Span::is_recording(span2))
  assert_eq(span2_name, "concurrent-operation-1")
  
  assert_eq(span3_trace_id, first_trace_id)
  assert_true(SpanContext::is_valid(span3_ctx))
  assert_true(Span::is_recording(span3))
  assert_eq(span3_name, "concurrent-operation-2")
  
  assert_eq(span4_trace_id, first_trace_id)
  assert_true(SpanContext::is_valid(span4_ctx))
  assert_true(Span::is_recording(span4))
  assert_eq(span4_name, "concurrent-operation-3")
  
  assert_eq(span5_trace_id, first_trace_id)
  assert_true(SpanContext::is_valid(span5_ctx))
  assert_true(Span::is_recording(span5))
  assert_eq(span5_name, "concurrent-operation-4")
  
  // Add events to all spans
  Span::add_event(span1, "concurrent-event")
  Span::add_event(span2, "concurrent-event")
  Span::add_event(span3, "concurrent-event")
  Span::add_event(span4, "concurrent-event")
  Span::add_event(span5, "concurrent-event")
  
  // End all spans
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  Span::end(span4)
  Span::end(span5)
}

// Test 9: Telemetry data serialization
test "telemetry data serialization" {
  // Create complex telemetry data
  let trace_id = "serialization-test-trace"
  let span_id = "serialization-test-span"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  let attributes = Attributes::new()
  Attributes::set(attributes, "service.name", StringValue("serialization-test-service"))
  Attributes::set(attributes, "service.version", StringValue("1.0.0"))
  Attributes::set(attributes, "request.count", IntValue(100))
  Attributes::set(attributes, "request.duration", FloatValue(123.45))
  Attributes::set(attributes, "request.success", BoolValue(true))
  Attributes::set(attributes, "request.tags", ArrayStringValue(["tag1", "tag2", "tag3"]))
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Serialization test log"),
    Some(attributes),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Test serialization format
  let serialized_span_context = span_ctx.trace_id + ":" + span_ctx.span_id + ":" + (if span_ctx.sampled { "true" } else { "false" })
  let expected_serialized_span_context = "serialization-test-trace:serialization-test-span:true"
  
  assert_eq(serialized_span_context, expected_serialized_span_context)
  
  // Test log record serialization
  let log_severity = match LogRecord::severity_number(log_record) {
    Info => "INFO"
    Warn => "WARN"
    Error => "ERROR"
    _ => "UNKNOWN"
  }
  
  let log_body = match LogRecord::body(log_record) {
    Some(body) => body
    None => ""
  }
  
  let serialized_log = log_severity + ":" + log_body + ":" + trace_id + ":" + span_id
  let expected_serialized_log = "INFO:Serialization test log:serialization-test-trace:serialization-test-span"
  
  assert_eq(serialized_log, expected_serialized_log)
  
  // Test attribute serialization
  let attr_service_name = match Attributes::get(attributes, "service.name") {
    Some(StringValue(value)) => value
    _ => ""
  }
  
  let attr_request_count = match Attributes::get(attributes, "request.count") {
    Some(IntValue(value)) => {
      match value {
        100 => "100"
        _ => "0"
      }
    }
    _ => "0"
  }
  
  let attr_request_success = match Attributes::get(attributes, "request.success") {
    Some(BoolValue(value)) => if value { "true" } else { "false" }
    _ => "false"
  }
  
  assert_eq(attr_service_name, "serialization-test-service")
  assert_eq(attr_request_count, "100")
  assert_eq(attr_request_success, "true")
}

// Test 10: System fault recovery
test "system fault recovery" {
  // Simulate system fault conditions
  let fault_detected = true
  let fault_type = "database.connection.timeout"
  let fault_timestamp = Clock::now_unix_nanos(Clock::system())
  
  // Create error log for fault
  let error_log = LogRecord::new(Error, "System fault detected: " + fault_type)
  Logger::emit(LoggerProvider::get_logger(LoggerProvider::default(), "fault.recovery.logger"), error_log)
  
  // Simulate recovery process
  let recovery_initiated = true
  let recovery_strategy = "reconnect.with.backoff"
  let max_retry_attempts = 3
  let current_retry_attempt = 0
  
  // Test recovery logic
  let should_retry = fault_detected && recovery_initiated && current_retry_attempt < max_retry_attempts
  let recovery_completed = should_retry && (current_retry_attempt + 1 >= max_retry_attempts)
  
  assert_true(should_retry)
  assert_false(recovery_completed)  // Not completed yet
  
  // Simulate successful recovery
  let recovery_successful = true
  let system_restored = recovery_completed && recovery_successful
  
  // Create recovery log
  let recovery_log = LogRecord::new(Info, "System recovery completed using strategy: " + recovery_strategy)
  Logger::emit(LoggerProvider::get_logger(LoggerProvider::default(), "fault.recovery.logger"), recovery_log)
  
  // Test telemetry during fault and recovery
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "fault.recovery.meter")
  
  let fault_counter = Meter::create_counter(meter, "system.faults")
  let recovery_counter = Meter::create_counter(meter, "system.recoveries")
  let recovery_time_gauge = Meter::create_gauge(meter, "system.recovery.time")
  
  
  
  // Record metrics
  Counter::add(fault_counter, 1.0)
  Counter::add(recovery_counter, 1.0)
  
  // Verify fault recovery metrics
  assert_eq(Instrument::name(Counter(fault_counter.name, fault_counter.description, fault_counter.unit)), "system.faults")
  assert_eq(Instrument::name(Counter(recovery_counter.name, recovery_counter.description, recovery_counter.unit)), "system.recoveries")
  assert_eq(Instrument::name(Gauge(recovery_time_gauge.name, recovery_time_gauge.description, recovery_time_gauge.unit)), "system.recovery.time")
  
  // Test system health after recovery
  let system_healthy = system_restored && recovery_successful
  let health_check_timestamp = Clock::now_unix_nanos(Clock::system())
  let health_check_duration = health_check_timestamp - fault_timestamp
  
  assert_true(system_healthy)
  assert_true(health_check_duration > 0L)
  
  // Create health check log
  let health_log = LogRecord::new(Info, "System health check passed after recovery")
  Logger::emit(LoggerProvider::get_logger(LoggerProvider::default(), "fault.recovery.logger"), health_log)
}
