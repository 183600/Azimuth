// Azimuth New Additional Test Suite
// This file contains 10 new test cases for the Azimuth telemetry system

// Test 1: Context propagation with complex nested scenarios
pub test "context propagation complex nested scenarios" {
  let root_ctx = azimuth::Context::root()
  let user_key = azimuth::ContextKey::new("user.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let trace_key = azimuth::ContextKey::new("trace.id")
  
  // Create nested context with multiple values
  let ctx_with_user = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_user, request_key, "req-67890")
  let ctx_with_trace = azimuth::Context::with_value(ctx_with_request, trace_key, "trace-abcdef")
  
  // Verify all values are accessible
  assert_eq(azimuth::Context::get(ctx_with_trace, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_trace, request_key), Some("req-67890"))
  assert_eq(azimuth::Context::get(ctx_with_trace, trace_key), Some("trace-abcdef"))
  
  // Test that earlier contexts are unaffected
  assert_eq(azimuth::Context::get(ctx_with_user, request_key), None)
  assert_eq(azimuth::Context::get(ctx_with_request, trace_key), None)
}

// Test 2: Advanced metrics with multiple instrument types
pub test "advanced metrics multiple instrument types" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "advanced-test-meter")
  
  // Create multiple instruments of different types
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration")
  let active_connections = azimuth::Meter::create_updown_counter(meter, "http.active_connections")
  let memory_gauge = azimuth::Meter::create_gauge(meter, "process.memory.usage")
  
  // Perform operations on all instruments
  azimuth::Counter::add(request_counter, 10.0)
  azimuth::Histogram::record(response_histogram, 150.5)
  azimuth::Histogram::record(response_histogram, 200.3)
  azimuth::UpDownCounter::add(active_connections, 5.0)
  azimuth::UpDownCounter::add(active_connections, -2.0)
  
  // Verify instrument properties
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(active_connections.name, "http.active_connections")
  assert_eq(memory_gauge.name, "process.memory.usage")
}

// Test 3: Span events and status management
pub test "span events and status management" {
  let span_ctx = azimuth::SpanContext::new("trace-123", "span-456", true, "")
  let span = azimuth::Span::new("test-operation", azimuth::Internal, span_ctx)
  
  // Add events to the span
  azimuth::Span::add_event(span, "operation.started", azimuth::Attributes::new())
  
  let event_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(event_attrs, "component", "database")
  azimuth::Attributes::set(event_attrs, "query", "SELECT * FROM users")
  azimuth::Span::add_event(span, "db.query.executed", event_attrs)
  
  // Update span status
  azimuth::Span::set_status(span, azimuth::Ok, "Operation completed successfully")
  
  // Verify span properties
  assert_eq(azimuth::Span::name(span), "test-operation")
  assert_eq(azimuth::Span::kind(span), azimuth::Internal)
  assert_true(azimuth::Span::is_recording(span))
}

// Test 4: Baggage propagation with metadata
pub test "baggage propagation with metadata" {
  let baggage = azimuth::Baggage::new()
  
  // Add baggage entries with metadata
  let updated_baggage = azimuth::Baggage::set_entry(baggage, "user.id", "12345")
  let updated_baggage = azimuth::Baggage::set_entry(updated_baggage, "session.id", "abcdef-12345")
  let updated_baggage = azimuth::Baggage::set_entry(updated_baggage, "request.source", "mobile")
  
  // Retrieve baggage values
  let user_id = azimuth::Baggage::get_entry(updated_baggage, "user.id")
  let session_id = azimuth::Baggage::get_entry(updated_baggage, "session.id")
  let request_source = azimuth::Baggage::get_entry(updated_baggage, "request.source")
  let missing_entry = azimuth::Baggage::get_entry(updated_baggage, "nonexistent.key")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(session_id, Some("abcdef-12345"))
  assert_eq(request_source, Some("mobile"))
  assert_eq(missing_entry, None)
}

// Test 5: Logger with structured logging
pub test "logger with structured logging" {
  let logger = azimuth::Logger::new("test-logger")
  
  // Create structured log attributes
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "user.id", azimuth::StringValue("12345"))
  azimuth::Attributes::set(log_attrs, "action", azimuth::StringValue("login"))
  azimuth::Attributes::set(log_attrs, "success", azimuth::BoolValue(true))
  azimuth::Attributes::set(log_attrs, "response_time", azimuth::IntValue(150))
  
  // Create log record with structured data
  let log_record = azimuth::Logger::new_log_record(
    logger,
    azimuth::Info,
    "User authentication completed",
    log_attrs
  )
  
  // Verify log record properties
  assert_eq(azimuth::LogRecord::severity_number(log_record), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(log_record), Some("User authentication completed"))
}

// Test 6: Resource management with hierarchical attributes
pub test "resource management with hierarchical attributes" {
  let resource = azimuth::Resource::new()
  
  // Set hierarchical resource attributes
  let updated_resource = azimuth::Resource::set_attribute(
    resource, 
    "service.name", 
    azimuth::StringValue("payment-service")
  )
  let updated_resource = azimuth::Resource::set_attribute(
    updated_resource, 
    "service.version", 
    azimuth::StringValue("1.2.3")
  )
  let updated_resource = azimuth::Resource::set_attribute(
    updated_resource, 
    "service.namespace", 
    azimuth::StringValue("production")
  )
  let updated_resource = azimuth::Resource::set_attribute(
    updated_resource, 
    "deployment.environment", 
    azimuth::StringValue("prod")
  )
  
  // Verify resource attributes
  let service_name = azimuth::Resource::get_attribute(updated_resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(updated_resource, "service.version")
  let service_namespace = azimuth::Resource::get_attribute(updated_resource, "service.namespace")
  let deployment_env = azimuth::Resource::get_attribute(updated_resource, "deployment.environment")
  
  assert_eq(service_name, Some(azimuth::StringValue("payment-service")))
  assert_eq(service_version, Some(azimuth::StringValue("1.2.3")))
  assert_eq(service_namespace, Some(azimuth::StringValue("production")))
  assert_eq(deployment_env, Some(azimuth::StringValue("prod")))
}

// Test 7: Composite propagator with multiple formats
pub test "composite propagator with multiple formats" {
  let trace_context_propagator = azimuth::TraceContextPropagator::new()
  let baggage_propagator = azimuth::BaggagePropagator::new()
  
  // Create composite propagator
  let composite_propagator = azimuth::CompositePropagator::new([
    trace_context_propagator,
    baggage_propagator
  ])
  
  // Test injection and extraction
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  let span_ctx = azimuth::SpanContext::new("trace-123", "span-456", true, "")
  let ctx_with_span = azimuth::Context::with_span(ctx, span_ctx)
  
  // Inject context into carrier
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_span, carrier)
  
  // Extract context from carrier
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked
  let extracted_span = azimuth::Context::span(extracted_ctx)
  assert_true(azimuth::Option::is_some(extracted_span))
}

// Test 8: Advanced attribute operations with type conversions
pub test "advanced attribute operations with type conversions" {
  let attrs = azimuth::Attributes::new()
  
  // Set attributes of different types
  azimuth::Attributes::set(attrs, "string.value", azimuth::StringValue("test"))
  azimuth::Attributes::set(attrs, "int.value", azimuth::IntValue(42))
  azimuth::Attributes::set(attrs, "float.value", azimuth::FloatValue(3.14))
  azimuth::Attributes::set(attrs, "bool.value", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "string.array", azimuth::ArrayStringValue(["a", "b", "c"]))
  azimuth::Attributes::set(attrs, "int.array", azimuth::ArrayIntValue([1, 2, 3]))
  
  // Test type-safe retrieval
  let string_val = azimuth::Attributes::get(attrs, "string.value")
  let int_val = azimuth::Attributes::get(attrs, "int.value")
  let float_val = azimuth::Attributes::get(attrs, "float.value")
  let bool_val = azimuth::Attributes::get(attrs, "bool.value")
  let string_array = azimuth::Attributes::get(attrs, "string.array")
  let int_array = azimuth::Attributes::get(attrs, "int.array")
  
  assert_eq(string_val, Some(azimuth::StringValue("test")))
  assert_eq(int_val, Some(azimuth::IntValue(42)))
  assert_eq(float_val, Some(azimuth::FloatValue(3.14)))
  assert_eq(bool_val, Some(azimuth::BoolValue(true)))
  assert_eq(string_array, Some(azimuth::ArrayStringValue(["a", "b", "c"])))
  assert_eq(int_array, Some(azimuth::ArrayIntValue([1, 2, 3])))
}

// Test 9: Span lifecycle with parent-child relationships
pub test "span lifecycle with parent-child relationships" {
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(provider, "lifecycle-test")
  
  // Create parent span
  let parent_span = azimuth::Tracer::start_span(tracer, "parent-operation")
  
  // Create child spans
  let child_span1 = azimuth::Tracer::start_span(tracer, "child-operation-1", parent_span)
  let child_span2 = azimuth::Tracer::start_span(tracer, "child-operation-2", parent_span)
  
  // Create grandchild span
  let grandchild_span = azimuth::Tracer::start_span(tracer, "grandchild-operation", child_span1)
  
  // Verify span relationships
  let parent_ctx = azimuth::Span::context(parent_span)
  let child1_ctx = azimuth::Span::context(child_span1)
  let child2_ctx = azimuth::Span::context(child_span2)
  let grandchild_ctx = azimuth::Span::context(grandchild_span)
  
  assert_eq(azimuth::SpanContext::trace_id(parent_ctx), azimuth::SpanContext::trace_id(child1_ctx))
  assert_eq(azimuth::SpanContext::trace_id(parent_ctx), azimuth::SpanContext::trace_id(child2_ctx))
  assert_eq(azimuth::SpanContext::trace_id(parent_ctx), azimuth::SpanContext::trace_id(grandchild_ctx))
  
  assert_eq(azimuth::SpanContext::span_id(parent_ctx), azimuth::SpanContext::parent_span_id(child1_ctx))
  assert_eq(azimuth::SpanContext::span_id(parent_ctx), azimuth::SpanContext::parent_span_id(child2_ctx))
  assert_eq(azimuth::SpanContext::span_id(child1_ctx), azimuth::SpanContext::parent_span_id(grandchild_ctx))
}

// Test 10: Telemetry data integrity across operations
pub test "telemetry data integrity across operations" {
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(provider, "integrity-test")
  
  // Create comprehensive span with all features
  let span = azimuth::Tracer::start_span(tracer, "comprehensive-operation")
  
  // Add attributes
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "operation.type", azimuth::StringValue("data-processing"))
  azimuth::Attributes::set(attrs, "batch.size", azimuth::IntValue(1000))
  azimuth::Attributes::set(attrs, "processing.time", azimuth::FloatValue(250.5))
  azimuth::Span::set_attributes(span, attrs)
  
  // Add events
  azimuth::Span::add_event(span, "processing.started", azimuth::Attributes::new())
  
  let progress_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(progress_attrs, "progress", azimuth::IntValue(50))
  azimuth::Span::add_event(span, "processing.progress", progress_attrs)
  
  // Set status
  azimuth::Span::set_status(span, azimuth::Ok, "Processing completed successfully")
  
  // Verify all data is preserved
  assert_eq(azimuth::Span::name(span), "comprehensive-operation")
  assert_true(azimuth::Span::is_recording(span))
  
  let span_attrs = azimuth::Span::attributes(span)
  let op_type = azimuth::Attributes::get(span_attrs, "operation.type")
  let batch_size = azimuth::Attributes::get(span_attrs, "batch.size")
  let proc_time = azimuth::Attributes::get(span_attrs, "processing.time")
  
  assert_eq(op_type, Some(azimuth::StringValue("data-processing")))
  assert_eq(batch_size, Some(azimuth::IntValue(1000)))
  assert_eq(proc_time, Some(azimuth::FloatValue(250.5)))
}